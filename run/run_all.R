rm(list = ls())
options(error = NULL)
library(here)
library(rstan)
library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)
library(loo)
library(ggnewscale)
library(patchwork)

# data wrangling --------------------------------------

#### 1. main grouped data ####
data_path = here("data/abundance/Data Gathering - Data Format (1).csv")
raw_data = read.csv(data_path)
main_d = raw_data %>%
  filter(Region %in% c("Marine Mammals", "marine birds", "elasmobranch", "turtle")) %>%
  mutate(
    Group = Region,
    ID = as.character(ID),
    Taxa = as.character(Taxa)
  ) %>%
  select(Group, ID, Taxa, Year, Value)

#### 2. seagrasses ####
seagrass_d = read.csv(here("data/abundance/clean_area_ts_for_analysis.csv")) %>% 
  filter(year > 1900) %>%
  mutate(
    Group = "seagrass",
    ID = make.names(study_site),
    Taxa = as.character(dom_species),
    Year = year,
    Value = area
  ) %>%
  select(Group, ID, Taxa, Year, Value)

#### 3. kelp ####
kelp_genus_family = read.csv(here("data/abundance/kelp_family_genus.csv"))

kelp_d = readRDS(here("data/abundance/krumhansl_kelp_timeseries_raw.RDS")) %>%
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

#### 4. RAM dataset - exploited fishes ####
ram_d = read.csv(here("data/abundance/final_dataset.csv")) %>%
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

#### 5. deep sea pelagics - unexploited ####
mycto_d = read.csv(here("data/abundance/myctobase_clean_unexploitedpelagics.csv")) %>%
  drop_na(year) %>%
  mutate(
    Group = "deep sea pelagics",
    ID = as.character(scientificName),
    Taxa = as.character(scientificName),
    Year = as.numeric(year),
    Value = n_m3
  ) %>%
  select(Group, ID, Taxa, Year, Value)

#### 6. non-exploited bony fishes ####
noCfish_d = readRDS(here("data/abundance/LPI_data_CorNC.rds")) %>%
  filter(custom_class == "Bony fish - NC") %>%
  mutate(
    Group = "non-exploited fish",
    ID = as.character(ID),
    Taxa = as.character(Binomial),
    Year = year,
    Value = popvalue
  ) %>%
  select(Group, ID, Taxa, Year, Value)

#### 7. unified data ####
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
    Taxa = droplevels(as.factor(Taxa)),
    Group = droplevels(as.factor(Group))
  ) %>%
  group_by(Group) %>%
  mutate(Year_scaled = Year - min(Year) + 1) %>%
  ungroup()

# fits ------------------------------------------------
if (!exists("stan_fits")) {
  stan_fits = list()
}

taxa_groups = unified_d$Group %>% levels() %>% as.character()

rerun = c(
  FALSE, FALSE, FALSE, # Crustaceans  |  elasmobranch | exploited fish
  FALSE, TRUE, FALSE, # kelp | marine birds | mammals 
  FALSE, FALSE, FALSE  # non-exploited fish | seagrass | turtle
)
names(rerun) = taxa_groups

# prior grid configuration:
# 1 = exponential(1) restrictive, shrinks variance toward zero
# 2 = half-normal(0,1) weakly informative
# 3 = half-Cauchy(0,1) long right tail, allows for large jumps
prior_grid = expand.grid(
  obs_type = c(1, 2, 3),
  proc_type = c(1, 2, 3)
)

run_sensitivity_model = function(obs_t, proc_t, data_list, comp_model) {
  if (obs_t == 1) obs_p = c(1, 0)
  if (obs_t == 2) obs_p = c(0, 1)
  if (obs_t == 3) obs_p = c(0, 1)
  
  if (proc_t == 1) proc_p = c(1, 0)
  if (proc_t == 2) proc_p = c(0, 1)
  if (proc_t == 3) proc_p = c(0, 1)
  
  data_list$prior_type_obs = obs_t
  data_list$prior_prm_obs = obs_p
  data_list$prior_type_proc = proc_t
  data_list$prior_prm_proc = proc_p
  
  fit = sampling(
    object = comp_model,
    data = data_list,
    iter = 100,
    chains = 1,
    cores = 1,
    refresh = 10,
    control = list(adapt_delta = 0.95, max_treedepth = 12)
  )
  
  return(fit)
}

compiled_marss = stan_model(file = here("model", "MARSS.stan")) 

for (taxa_name in taxa_groups) {
  
  if (!rerun[taxa_name]) next
  cat("Running models for taxa group:", taxa_name, "\n")
  
  taxa_d = unified_d %>%
    filter(Group == taxa_name) %>%
    drop_na(Taxa, ID, Value_log) %>%
    mutate(
      ID = droplevels(as.factor(ID)),
      Taxa = droplevels(as.factor(Taxa))
    )
  
  stan_data = list(
    N = nrow(taxa_d),
    T = max(taxa_d$Year_scaled),
    N_id = n_distinct(taxa_d$ID),
    N_taxa = n_distinct(taxa_d$Taxa),
    t_idx = taxa_d$Year_scaled,
    id_idx = as.numeric(taxa_d$ID),
    taxa_idx = as.numeric(taxa_d$Taxa),
    y = taxa_d$Value_log
  )
  
  results_list = list()
  
  for (i in 1:nrow(prior_grid)) {
    obs_t = prior_grid$obs_type[i]
    proc_t = prior_grid$proc_type[i]
    
    cat(" Running model with obs_type =", obs_t, "and proc_type =", proc_t, "\n")
    fit = run_sensitivity_model(obs_t, proc_t, stan_data, compiled_marss)
    
    log_lik = extract_log_lik(fit)
    loo_res = loo(log_lik)
    
    results_list[[i]] = list(
      obs_type = obs_t,
      proc_type = proc_t,
      stan_fit = fit,
      loo_metrics = loo_res
    )
  }
  
  stan_fits[[taxa_name]] = results_list
}

# model comparison ------------------------------------
model_comparison_list = list()
sensitivity_trends_list = list()

prior_map = c(
  "1" = "plain(exponential)(1)", 
  "2" = "plain(half-normal)(0,1)", 
  "3" = "plain(half-Cauchy)(0,1)"
)

for (taxa_name in names(stan_fits)) {
  
  taxa_d = unified_d %>% filter(Group == taxa_name)
  total_stocks = n_distinct(taxa_d$ID)
  
  coverage_info = taxa_d %>%
    group_by(Year) %>%
    summarise(n_stocks = n_distinct(ID), .groups = "drop") %>%
    mutate(fraction = n_stocks / total_stocks)
  
  coverage_threshold = case_when(
    taxa_name == "non-exploited fish" ~ 0.10,
    taxa_name == "exploited fish" ~ 0.15,
    .default = 0.20
  )
  high_cov_years = coverage_info$Year[coverage_info$fraction > coverage_threshold]
  
  for (i in 1:nrow(prior_grid)) {
    obs_t = prior_grid$obs_type[i]
    proc_t = prior_grid$proc_type[i]
    
    fit_obj = stan_fits[[taxa_name]][[i]]
    fit = fit_obj$stan_fit
    loo_res = fit_obj$loo_metrics
    
    log_lik_matrix = extract_log_lik(fit)
    waic_res = waic(log_lik_matrix)
    
    mean_ll = mean(rowSums(log_lik_matrix))
    k_eff = loo_res$estimates["p_loo", "Estimate"]
    n_obs = nrow(taxa_d)
    
    pseudo_aic = -2 * mean_ll + 2 * k_eff
    pseudo_bic = -2 * mean_ll + log(n_obs) * k_eff
    
    parsed_label = paste0(
      "omega[j*\",\"*t] %~% ", prior_map[as.character(obs_t)], 
      " * ',  ' * w[t] %~% ", prior_map[as.character(proc_t)]
    )
    
    model_comparison_list[[length(model_comparison_list) + 1]] = data.frame(
      Group = taxa_name,
      Obs_Prior = prior_map[as.character(obs_t)],
      Proc_Prior = prior_map[as.character(proc_t)],
      LOOIC = loo_res$estimates["looic", "Estimate"],
      WAIC = waic_res$estimates["waic", "Estimate"],
      Pseudo_AIC = pseudo_aic,
      Pseudo_BIC = pseudo_bic
    )
    
    x_summary = as.data.frame(summary(fit, pars = "x")$summary)
    
    trend_table = data.frame(
      Group = taxa_name,
      Year = min(taxa_d$Year) + (1:nrow(x_summary)) - 1,
      estimate = x_summary$mean,
      Prior_Label = parsed_label
    )
    
    target_level = median(taxa_d$Value_scale[taxa_d$Year %in% high_cov_years], na.rm = TRUE)
    model_level_log = trend_table$estimate[trend_table$Year %in% (min(taxa_d$Year) + which(coverage_info$Year %in% high_cov_years) - 1)]
    model_level_geom = mean(exp(model_level_log), na.rm = TRUE)
    scalar = abs(target_level / model_level_geom)
    
    trend_table = trend_table %>%
      mutate(Abundance_Index = exp(estimate) * scalar)
    
    sensitivity_trends_list[[length(sensitivity_trends_list) + 1]] = trend_table
  }
}

model_comparison_df = bind_rows(model_comparison_list)
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
  guides(color = guide_legend(nrow = 3, byrow = TRUE))

print(sensitivity_plot)

# final unified plot ----------------------------------
all_trends_list = list()
all_raw_list = list()

for (taxa_name in taxa_groups) {
  
  if (is.null(stan_fits[[taxa_name]])) next
  
  t_d = unified_d %>% filter(Group == taxa_name)
  fit = stan_fits[[taxa_name]][[5]]$stan_fit
  
  x_summary = as.data.frame(summary(fit, pars = "x")$summary)
  
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
    Year = min(t_d$Year) + (1:nrow(x_summary)) - 1,
    estimate = x_summary$mean,
    conf.low = x_summary[["2.5%"]],
    conf.high = x_summary[["97.5%"]]
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
  
  all_trends_list[[taxa_name]] = trend_table %>% select(Group, Year, Abundance_Index, Lower_Bound, Upper_Bound)
  all_raw_list[[taxa_name]] = t_d %>% select(Group, Year, ID, Value_scale) %>% left_join(coverage_info, by = "Year")
}

stan_trends_df = bind_rows(all_trends_list)
stan_raw_df = bind_rows(all_raw_list)

coral_d = read.csv(here("data/abundance/souter-et-al_data-models.csv")) %>%
  filter(region == "Global", category == "Hard coral") %>%
  mutate(Group = "Hard coral", Year = year, Abundance_Index = (mean)/mean(mean), Lower_Bound = (lower_ci_95)/mean(mean), Upper_Bound = (higher_ci_95)/mean(mean)) %>%
  select(Group, Year, Abundance_Index, Lower_Bound, Upper_Bound)

mangrove_d = readxl::read_xlsx(here("data/abundance/gmw_v3_country_statistics_ha.xlsx")) %>%
  pivot_longer(cols = matches("^\\d{4}$"), names_to = "Year", values_to = "Area") %>%
  filter(Name == "Global (km2)") %>% drop_na(Area) %>%
  mutate(Group = "Mangrove", Year = as.numeric(Year), Abundance_Index = (Area)/mean(Area), Lower_Bound = NA, Upper_Bound = NA) %>%
  select(Group, Year, Abundance_Index, Lower_Bound, Upper_Bound)

saltmarsh_d = read.csv(here("data/abundance/SaltMarshExtent.csv")) %>%
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

combined_trends = bind_rows(stan_trends_df, coral_d, mangrove_d, saltmarsh_d)
combined_trends$Group = factor(combined_trends$Group, levels = c(taxa_groups, "Hard coral", "Mangrove", "Salt marsh"))
stan_raw_df$Group = factor(stan_raw_df$Group, levels = c(taxa_groups, "Hard coral", "Mangrove", "Salt marsh"))

final_figure = ggplot() +
  geom_boxplot(data = stan_raw_df, aes(x = Year, y = Value_scale, group = Year, fill = fraction), outliers = FALSE, color = NA, linewidth = 0.3, width = 1) +
  scale_fill_gradient(low = "white", high = "forestgreen", name = "Coverage", limits = c(0, 1)) +
  ggnewscale::new_scale_fill() +
  stat_summary(data = stan_raw_df, aes(x = Year, y = Value_scale, group = Year), fun = median, geom = "point", color = "darkslategrey", size = 1, alpha = 0.8, shape = 19) +
  geom_ribbon(data = combined_trends, aes(x = Year, ymin = Lower_Bound, ymax = Upper_Bound), fill = "cornflowerblue", alpha = 0.5) +
  geom_line(data = combined_trends, aes(x = Year, y = Abundance_Index), color = "darkblue", linewidth = 1) +
  facet_wrap(~ Group, scales = "free_y", ncol = 3) +
  theme_classic() +
  theme(legend.position = "bottom", strip.text = element_text(size = 12, face = "bold"), strip.background = element_blank()) +
  labs(y = "Relative abundance / area", x = "Year")

print(final_figure)