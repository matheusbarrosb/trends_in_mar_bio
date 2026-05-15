rm(list = ls())
options(error = NULL)
library(here)
library(rstan)
library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)
library(ggnewscale)
library(patchwork)

# data wrangling --------------------------------------
data_path = here("data", "abundance", "Data Gathering - Data Format (1).csv")
raw_data = read.csv(data_path)
main_d = raw_data %>%
  filter(Region %in% c("Marine Mammals", "marine birds", "elasmobranch", "turtle")) %>%
  mutate(
    Group = Region,
    ID = as.character(ID),
    Taxa = as.character(Taxa)
  ) %>%
  select(Group, ID, Taxa, Year, Value)

seagrass_d = read.csv(here("data", "abundance", "clean_area_ts_for_analysis.csv")) %>% 
  filter(year > 1900) %>%
  mutate(
    Group = "seagrass",
    ID = make.names(study_site),
    Taxa = as.character(dom_species),
    Year = year,
    Value = area
  ) %>%
  select(Group, ID, Taxa, Year, Value)

kelp_genus_family = read.csv(here("data", "abundance", "kelp_family_genus.csv"))

kelp_d = readRDS(here("data", "abundance", "krumhansl_kelp_timeseries_raw.RDS")) %>%
  mutate(Genus = case_when(
    str_detect(tolower(Taxon), "agarum") ~ "Agarum",
    str_detect(tolower(Taxon), "saccharina") ~ "Saccharina",
    str_detect(tolower(Taxon), "alaria") | str_detect(tolower(Taxon), "ala") ~ "Alaria",
    str_detect(tolower(Taxon), "cos") ~ "Costaria",
    str_detect(tolower(Taxon), "cym") ~ "Cymathaere",
    str_detect(tolower(Taxon), "egr") ~ "Egregia",
    str_detect(tolower(Taxon), "eis") ~ "Eisenia",
    str_detect(tolower(Taxon), "lam") ~ "Laminaria",
    str_detect(tolower(Taxon), "mac") ~ "Macrocystis",
    str_detect(tolower(Taxon), "ner") ~ "Nereocystis",
    str_detect(tolower(Taxon), "ple") ~ "Pleurophycus",
    str_detect(tolower(Taxon), "pte") ~ "Pterygophora",
    str_detect(tolower(Taxon), "les") ~ "Lessonia",
    .default = "Other")) %>%
  left_join(kelp_genus_family, by = "Genus") %>%
  filter(Study != "Channel_Islands_National_Park") %>%
  mutate(Density = case_when(
    is.na(Individual.Density.num.per.sq.m) ~ Stipe.Density.num.per.sq.m,
    .default = Individual.Density.num.per.sq.m)) %>%
  drop_na(Density) %>%
  filter(Sample.Year > 1900) %>%
  mutate(
    Group = "kelp",
    ID = as.character(Site),
    Taxa = as.character(Family),
    Year = Sample.Year,
    Value = Density
  ) %>%
  select(Group, ID, Taxa, Year, Value)

ram_d = read.csv(here("data", "abundance", "final_dataset.csv")) %>%
  group_by(stockid) %>%
  filter(unit == min(as.character(unit), na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(
    Group = case_when(
      taxGroup == "forage fish" ~ "forage fish",
      taxa == "Fish" & taxGroup != "forage fish" ~ "other exploited fish",
      taxa == "Crustaceans" ~ "Crustaceans",
      .default = NA_character_
    ),
    ID = as.character(stockid),
    Taxa = as.character(taxGroup),
    Year = year,
    Value = ts_raw
  ) %>%
  filter(!is.na(Group)) %>%
  select(Group, ID, Taxa, Year, Value)

mycto_d = read.csv(here("data", "abundance", "myctobase_clean_unexploitedpelagics.csv")) %>%
  drop_na(year) %>%
  mutate(
    Group = "deep sea pelagics",
    ID = as.character(scientificName),
    Taxa = as.character(scientificName),
    Year = as.numeric(year),
    Value = n_m3
  ) %>%
  select(Group, ID, Taxa, Year, Value)

noCfish_d = readRDS(here("data", "abundance", "LPI_data_CorNC.rds")) %>%
  filter(custom_class == "Bony fish - NC") %>%
  mutate(
    Group = "non-exploited fish",
    ID = as.character(ID),
    Taxa = as.character(Binomial),
    Year = year,
    Value = popvalue
  ) %>%
  select(Group, ID, Taxa, Year, Value)

taxa_cutoffs = data.frame(
  Group = c("Marine Mammals", "marine birds", "turtle", "elasmobranch", 
            "exploited fish", "non-exploited fish", "Crustaceans", "seagrass", "kelp"),
  Cutoff_Year = c(1950, 1950, 1950, 1950, 1950, 1950, 1960, 1950, 1980)
)

unified_d = bind_rows(main_d, seagrass_d, kelp_d, ram_d, mycto_d, noCfish_d) %>%
  group_by(Group, ID) %>%
  mutate(
    zero_replacement = case_when(
      Group == "kelp" ~ 0.01 * mean(Value[Value > 0], na.rm = TRUE),
      Group == "other exploited fish" ~ 0.05 * mean(Value[Value > 0], na.rm = TRUE),
      .default = 0.1 * mean(Value[Value > 0], na.rm = TRUE)
    ),
    Value = ifelse(Value == 0, zero_replacement, Value),
    scale_denominator = ifelse(Group %in% c("forage fish", "other exploited fish"), median(Value, na.rm = TRUE), mean(Value, na.rm = TRUE)),
    Value_scale = Value / scale_denominator,
    Value_log = log(Value_scale)
  ) %>%
  ungroup() %>%
  select(-zero_replacement, -scale_denominator) %>%
  mutate(
    Group = case_when(
      Group == "deep sea pelagics" ~ "non-exploited fish",
      Group %in% c("other exploited fish", "forage fish") ~ "exploited fish",
      .default = as.character(Group)
    ),
    ID = droplevels(as.factor(ID)),
    Taxa = droplevels(as.factor(Taxa))
  ) %>%
  left_join(taxa_cutoffs, by = "Group") %>%
  filter(Year >= Cutoff_Year) %>%
  select(-Cutoff_Year) %>%
  mutate(Group = droplevels(as.factor(Group)))

# extraction and plotting ----------------------------------
taxa_groups = levels(unified_d$Group)
taxa_cutoffs_vec = setNames(taxa_cutoffs$Cutoff_Year, taxa_cutoffs$Group)

all_trends_list = list()
all_raw_list = list()

for (taxa_name in taxa_groups) {
  
  fit_path = here("res", "model_fits", taxa_name, "fit.rds")
  
  if (!file.exists(fit_path)) {
    cat("Skipping:", taxa_name, "- No fit.rds file found.\n")
    next
  }
  
  cat("Extracting trend for:", taxa_name, "\n")
  
  fits_list = tryCatch({ readRDS(fit_path) }, error = function(e) NULL)
  if (is.null(fits_list) || length(fits_list) == 0) next
  
  fit = NULL
  for (res in fits_list) {
    if ("stan_fit" %in% names(res) && !is.null(res$stan_fit)) {
      fit = res$stan_fit
      break
    }
  }
  
  if (is.null(fit)) next
  
  x_draws = tryCatch({ rstan::extract(fit, pars = "x")$x }, error = function(e) NULL)
  if (is.null(x_draws)) next
  
  x_mean = colMeans(x_draws)
  x_lower = apply(x_draws, 2, quantile, probs = 0.025)
  x_upper = apply(x_draws, 2, quantile, probs = 0.975)
  
  t_d = unified_d %>% filter(Group == taxa_name)
  cutoff_yr = taxa_cutoffs_vec[[taxa_name]]
  
  total_stocks = n_distinct(t_d$ID)
  coverage_info = t_d %>%
    group_by(Year) %>%
    summarise(n_stocks = n_distinct(ID), .groups = "drop") %>%
    mutate(fraction = n_stocks / total_stocks)
  
  coverage_threshold = case_when(
    taxa_name == "non-exploited fish" ~ 0.10,
    taxa_name == "exploited fish" ~ 0.15,
    .default = 0.20
  )
  high_cov_years = coverage_info$Year[coverage_info$fraction > coverage_threshold]
  
  trend_table = data.frame(
    Group = taxa_name,
    Year = cutoff_yr + (1:length(x_mean)) - 1,
    estimate = x_mean,
    conf.low = x_lower,
    conf.high = x_upper
  ) %>% left_join(coverage_info, by = "Year")
  
  target_level = median(t_d$Value_scale[t_d$Year %in% high_cov_years], na.rm = TRUE)
  model_level_log = trend_table$estimate[trend_table$Year %in% high_cov_years]
  model_level_geom = mean(exp(model_level_log), na.rm = TRUE)
  scalar = abs(target_level / model_level_geom)
  
  trend_table = trend_table %>%
    mutate(
      Abundance_Index = exp(estimate) * scalar,
      Upper_Bound = exp(conf.high) * scalar,
      Lower_Bound = exp(conf.low) * scalar
    )
  
  write.csv(trend_table, here("res", "model_fits", taxa_name, "global_trend.csv"), row.names = FALSE)
  
  all_trends_list[[taxa_name]] = trend_table %>% select(Group, Year, Abundance_Index, Lower_Bound, Upper_Bound)
  all_raw_list[[taxa_name]] = t_d %>% select(Group, Year, ID, Value_scale) %>% left_join(coverage_info, by = "Year")
  
  rm(fits_list, fit, x_draws)
  gc()
}

stan_trends_df = bind_rows(all_trends_list)
stan_raw_df = bind_rows(all_raw_list)

coral_d = read.csv(here("data", "abundance", "souter-et-al_data-models.csv")) %>%
  filter(region == "Global", category == "Hard coral") %>%
  mutate(Group = "Hard coral", Year = year, Abundance_Index = (mean)/mean(mean), Lower_Bound = (lower_ci_95)/mean(mean), Upper_Bound = (higher_ci_95)/mean(mean)) %>%
  filter(Year >= 1980) %>%
  select(Group, Year, Abundance_Index, Lower_Bound, Upper_Bound)

mangrove_d = readxl::read_xlsx(here("data", "abundance", "gmw_v3_country_statistics_ha.xlsx")) %>%
  pivot_longer(cols = matches("^\\d{4}$"), names_to = "Year", values_to = "Area") %>%
  filter(Name == "Global (km2)") %>% drop_na(Area) %>%
  mutate(Group = "Mangrove", Year = as.numeric(Year), Abundance_Index = (Area)/mean(Area), Lower_Bound = NA, Upper_Bound = NA) %>%
  filter(Year >= 1995) %>%
  select(Group, Year, Abundance_Index, Lower_Bound, Upper_Bound)

saltmarsh_d = read.csv(here("data", "abundance", "SaltMarshExtent.csv")) %>%
  summarise(`2000-2004` = sum(`Area_2000-2004.Ha`, na.rm = TRUE), `2005-2009` = sum(`Area_ 2005-2009.Ha`, na.rm = TRUE), `2010-2014` = sum(`Area_ 2010-2014.Ha`, na.rm = TRUE), `2015-2019` = sum(`Area_ 2015-2019.Ha`, na.rm = TRUE)) %>%
  pivot_longer(cols = everything(), names_to = "Timeline", values_to = "Global_Area") %>%
  mutate(
    Group = "Salt marsh",
    Year = case_when(
      Timeline == "2000-2004" ~ 2002,
      Timeline == "2005-2009" ~ 2007,
      Timeline == "2010-2014" ~ 2012,
      Timeline == "2015-2019" ~ 2017
    ),
    Abundance_Index = (Global_Area)/mean(Global_Area),
    Lower_Bound = NA,
    Upper_Bound = NA
  ) %>%
  select(Group, Year, Abundance_Index, Lower_Bound, Upper_Bound)

combined_trends = bind_rows(stan_trends_df, coral_d, mangrove_d)
combined_trends$Group = factor(combined_trends$Group, levels = c(taxa_groups, "Hard coral", "Mangrove", "Salt marsh"))
stan_raw_df$Group = factor(stan_raw_df$Group, levels = c(taxa_groups, "Hard coral", "Mangrove", "Salt marsh"))

final_figure = ggplot() +
  geom_boxplot(data = stan_raw_df, aes(x = Year, y = Value_scale, group = Year, fill = fraction), outliers = FALSE, color = NA, linewidth = 0.3, width = 1) +
  scale_fill_gradient(low = "white", high = "forestgreen", name = "Coverage", limits = c(0, 1)) +
  ggnewscale::new_scale_fill() +
  stat_summary(data = stan_raw_df, aes(x = Year, y = Value_scale, group = Year), fun = median, geom = "point", color = "darkslategrey", size = 1, alpha = 0.8, shape = 19) +
  geom_ribbon(data = combined_trends, aes(x = Year, ymin = Lower_Bound, ymax = Upper_Bound), fill = "cornflowerblue", alpha = 0.2) +
  geom_line(data = combined_trends, aes(x = Year, y = Abundance_Index), color = "darkblue", linewidth = 1) +
  facet_wrap(~ Group, scales = "free_y", ncol = 3) +
  theme_classic() +
  theme(legend.position = "bottom", strip.text = element_text(size = 12, face = "bold"), strip.background = element_blank()) +
  labs(y = "Relative abundance / area", x = "Year")

print(final_figure)
