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
library(ggh4x)

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
  Group = c("Marine Mammals","marine birds", "turtle", "elasmobranch", 
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
    Value_scale = Value/scale_denominator,
    Value_log =log(Value_scale)
  ) %>%
  ungroup() %>%
  select(-zero_replacement, -scale_denominator) %>%
  mutate(
    Group = case_when(
      Group == "deep sea pelagics" ~ "non-exploited fish",
      Group %in% c("other exploited fish","forage fish") ~ "exploited fish",
      .default = as.character(Group)
    ),
    ID = droplevels(as.factor(ID)),
    Taxa = droplevels(as.factor(Taxa))
  ) %>%
  left_join(taxa_cutoffs, by = "Group") %>%
  filter(Year >= Cutoff_Year) %>%
  select(-Cutoff_Year)%>%
  mutate(Group = droplevels(as.factor(Group)))


# extraction and plotting ----------------------------------
taxa_groups = levels(unified_d$Group)

all_trends_list = list()
all_raw_list = list()
sensitivity_trends_list = list()

prior_map = c(
  "1" = "plain(exponential)(1)", 
  "2" = "plain(half-normal)(0,1)", 
  "3" = "plain(half-Cauchy)(0,1)"
)

for (taxa_name in taxa_groups) {
  
  global_csv_path = here("res", "model_fits", taxa_name, "global_trend.csv")
  sens_csv_path = here("res", "model_fits", taxa_name, "sensitivity_trends.csv")
  
  t_d = unified_d %>% filter(Group == taxa_name)
  
  total_stocks = n_distinct(t_d$ID)
  coverage_info = t_d %>%
    group_by(Year) %>%
    summarise(n_stocks = n_distinct(ID), .groups = "drop") %>%
    mutate(fraction = n_stocks / total_stocks)
  
  all_raw_list[[taxa_name]] = t_d %>% 
    select(Group, Year, ID, Value_scale) %>%
    left_join(coverage_info, by = "Year")
  
  if (file.exists(global_csv_path) && file.exists(sens_csv_path)) {
    cat("Loading existing CSVs for:", taxa_name,"\n")
    all_trends_list[[taxa_name]] = read.csv(global_csv_path) %>% select(Group, Year, Abundance_Index, Lower_Bound, Upper_Bound)
    sensitivity_trends_list[[length(sensitivity_trends_list) + 1]] = read.csv(sens_csv_path)
    next
  }
  
  fit_path = here("res", "model_fits",taxa_name, "fit.rds")
  
  if (!file.exists(fit_path)) {
    cat("Skipping:", taxa_name, "- No CSVs and no fit.rds found.\n")
    next
  }
  
  cat("Extracting trend for:", taxa_name,"\n")
  
  fits_list = tryCatch({ readRDS(fit_path) }, error = function(e) NULL)
  if (is.null(fits_list) || length(fits_list) == 0) next
  
  fit = NULL
  if (taxa_name == "Marine Mammals") {
    if (length(fits_list) >= 5 && !is.null(fits_list[[5]]$stan_fit)) {
      fit = fits_list[[5]]$stan_fit
    } else {
      fit =fits_list[[length(fits_list)]]$stan_fit
    }
  } else {
    for (res in fits_list) {
      if ("stan_fit" %in% names(res) && !is.null(res$stan_fit)) {
        fit = res$stan_fit
        break
      }
    }
  }
  
  if (is.null(fit)) next
  x_draws = tryCatch({ rstan::extract(fit, pars = "x")$x }, error = function(e) NULL)
  if (is.null(x_draws)) next
  
  x_mean = colMeans(x_draws)
  x_lower = apply(x_draws, 2, quantile, probs = 0.025)
  x_upper = apply(x_draws, 2, quantile, probs = 0.975)
  
  model_end_year = max(t_d$Year)
  model_start_year = model_end_year - length(x_mean) + 1
  
  trend_table_full = data.frame(
    Group = taxa_name,
    Year = model_start_year:model_end_year,
    estimate = x_mean,
    conf.low = x_lower,
    conf.high =  x_upper
  )
  
  trend_table = trend_table_full %>% 
    filter(Year >= min(t_d$Year)) %>%
    left_join(coverage_info, by = "Year")
  
  if (taxa_name == "marine birds") {
    annual_dots = t_d %>%
      group_by(Year) %>%
      summarise(dot_y = median(Value_scale, na.rm = TRUE), .groups = "drop")
    
    data_center = median(annual_dots$dot_y, na.rm = TRUE)
    model_center = median(exp(trend_table$estimate), na.rm = TRUE)
    scalar = data_center/model_center
    
    trend_table = trend_table %>%
      mutate(
        Abundance_Index = exp(estimate)*scalar,
        Upper_Bound = exp(conf.high)*scalar,
        Lower_Bound = exp(conf.low)*scalar
      )
  } else {
    coverage_threshold = case_when(
      taxa_name == "Marine Mammals" ~ 0.05,
      taxa_name == "non-exploited fish" ~ 0.10,
      taxa_name == "exploited fish" ~ 0.15,
      .default = 0.20
    )
    
    high_cov_years = coverage_info$Year[coverage_info$fraction > coverage_threshold]
    
    if (length(high_cov_years) == 0) {
      high_cov_years = seq(max(t_d$Year) - 9, max(t_d$Year))
    }
    
    annual_dots = t_d %>%
      filter(Year %in% high_cov_years) %>%
      group_by(Year) %>%
      summarise(annual_median = median(Value_scale, na.rm = TRUE), .groups = "drop")
    
    target_level = mean(annual_dots$annual_median, na.rm = TRUE)
    model_level_log = trend_table$estimate[trend_table$Year %in% high_cov_years]
    model_level_geom = mean(exp(model_level_log), na.rm = TRUE)
    scalar = abs(target_level/model_level_geom)
    
    trend_table = trend_table %>%
      mutate(
        Abundance_Index = exp(estimate)*scalar,
        Upper_Bound = exp(conf.high)*scalar,
        Lower_Bound = exp(conf.low)*scalar
      )
  }
  
  write.csv(trend_table, global_csv_path, row.names = FALSE)
  all_trends_list[[taxa_name]] = trend_table %>% select(Group, Year, Abundance_Index, Lower_Bound, Upper_Bound)
  
  taxa_sens_list = list()
  for (i in seq_along(fits_list)) {
    res = fits_list[[i]]
    if (is.null(res$stan_fit)) next
    sens_fit = res$stan_fit
    obs_t = res$obs_type
    proc_t = res$proc_type
    parsed_label = paste0(
      "omega[j*\",\"*t] %~% ", prior_map[as.character(obs_t)], 
      " * ',  ' * w[t] %~% ",prior_map[as.character(proc_t)]
    )
    
    sens_x_draws = tryCatch({ rstan::extract(sens_fit, pars = "x")$x }, error = function(e) NULL)
    if (is.null(sens_x_draws)) next
    sens_x_mean = colMeans(sens_x_draws)
    
    if (taxa_name == "marine birds") {
      sens_trend = data.frame(
        Group = taxa_name,
        Year = min(t_d$Year):max(t_d$Year),
        estimate = sens_x_mean,
        Prior_Label = parsed_label
      )
      
      sens_model_center = median(exp(sens_trend$estimate), na.rm = TRUE)
      sens_scalar = data_center / sens_model_center
      
      sens_trend = sens_trend %>%
        mutate(Abundance_Index = exp(estimate) * sens_scalar)
      
    } else {
      sens_trend_full = data.frame(
        Group = taxa_name,
        Year =  model_start_year:model_end_year,
        estimate = sens_x_mean,
        Prior_Label = parsed_label
      )
      
      sens_trend = sens_trend_full %>% filter(Year >= min(t_d$Year))
      
      sens_model_level_log = sens_trend$estimate[sens_trend$Year %in% high_cov_years]
      sens_model_level_geom = mean(exp(sens_model_level_log), na.rm = TRUE)
      sens_scalar = abs(target_level/ sens_model_level_geom)
      
      sens_trend = sens_trend %>%
        mutate(Abundance_Index = exp(estimate) * sens_scalar)
    }
    
    taxa_sens_list[[length(taxa_sens_list) + 1]] = sens_trend
  }
  
  if (length(taxa_sens_list) > 0) {
    taxa_sens_df = bind_rows(taxa_sens_list)
    write.csv(taxa_sens_df, sens_csv_path, row.names = FALSE)
    sensitivity_trends_list[[length(sensitivity_trends_list) + 1]] = taxa_sens_df
  }
  
  rm(fits_list,fit,x_draws)
  gc()
}

stan_trends_df = bind_rows(all_trends_list)
stan_raw_df = bind_rows(all_raw_list)

#gather rest of data for plotting - non modelled taxa ------
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
  summarise(`2000-2004` = sum(`Area_2000.2004.Ha`, na.rm = TRUE),
            `2005-2009` = sum(`Area_.2005.2009.Ha`, na.rm = TRUE),
            `2010-2014` = sum(`Area_.2010.2014.Ha`, na.rm = TRUE),
            `2015-2019` = sum(`Area_.2015.2019.Ha`, na.rm = TRUE)) %>%
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

combined_trends = bind_rows(stan_trends_df, coral_d, mangrove_d, saltmarsh_d)
combined_trends$Group = factor(combined_trends$Group, levels = c(taxa_groups, "Hard coral", "Mangrove", "Salt marsh"))
stan_raw_df$Group = factor(stan_raw_df$Group, levels = c(taxa_groups, "Hard coral", "Mangrove", "Salt marsh"))

# plottting ---------
final_figure = ggplot() +
  geom_boxplot(data = stan_raw_df, aes(x = Year, y = Value_scale, group = Year, fill = fraction), outliers = FALSE, color = NA, linewidth = 0.3, width = 1) +
  scale_fill_gradient(low = "white", high = "forestgreen", name = "Coverage", limits = c(0, 1)) +
  ggnewscale::new_scale_fill() +
  stat_summary(data = stan_raw_df, aes(x = Year, y = Value_scale, group = Year), fun = median, geom = "point", color = "darkslategrey", size = 1, alpha = 0.8, shape = 19) +
  geom_ribbon(data = combined_trends, aes(x = Year, ymin = Lower_Bound, ymax = Upper_Bound), fill = "cornflowerblue", alpha = 0.3) +
  geom_line(data = combined_trends, aes(x = Year, y = Abundance_Index), color = "darkblue", linewidth = 1) +
  facet_wrap(~ Group, ncol = 3, scales = "free_y") +
  facetted_pos_scales(
    y = list(
      Group == "Marine Mammals" ~ scale_y_continuous(limits = c(0, 3)),
      Group == "marine birds" ~ scale_y_continuous(limits = c(0, 3)),
      Group == "elasmobranch" ~ scale_y_continuous(limits = c(0, 3)),
      Group == "Crustaceans" ~ scale_y_continuous(limits = c(0, 7)),
      Group == "exploited fish" ~ scale_y_continuous(limits = c(0, 3)),
      Group == "non-exploited fish" ~ scale_y_continuous(limits = c(0, 3)),
      #     Group == "kelp" ~ scale_y_continuous(limits = c(0, 4)),
      Group == "seagrass" ~ scale_y_continuous(limits = c(0, 3)),
      Group == "turtle" ~ scale_y_continuous(limits = c(0, 7)),
      Group == "Hard coral" ~ scale_y_continuous(limits = c(0, 1.5)),
      Group == "Mangrove" ~ scale_y_continuous(limits = c(0, 1.5)),
      Group == "Salt marsh" ~ scale_y_continuous(limits = c(0, 1.5))
    )
  ) +
  theme_classic() +
  theme(legend.position = "bottom", strip.text = element_text(size = 12, face = "bold"), strip.background = element_blank()) +
  labs(y = "Relative abundance / area", x = "Year");final_figure

sensitivity_df = bind_rows(sensitivity_trends_list)

sensitivity_plot = ggplot(sensitivity_df, aes(x = Year, y = Abundance_Index, color = Prior_Label)) +
  geom_line(linewidth = 0.8, alpha = 0.8) +
  facet_wrap(~ Group, scales = "free_y", ncol = 3) +
  theme_classic() +
  scale_color_viridis_d(labels = scales::parse_format()) +
  labs(y = "Relative abundance / density", x = "Year", color = "Prior Configuration") +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 9),
    strip.text = element_text(size = 12, face = "bold"),
    strip.background = element_blank()
  ) +
  guides(color = guide_legend(nrow = 3, byrow = TRUE));sensitivity_plot