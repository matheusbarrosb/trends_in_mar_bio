library(sdmTMB)
library(sf)
library(tidyverse)
library(ggplot2)
library(rnaturalearth) 
library(viridis)      

TheD = read.csv('data/abundance/Data Gathering - Data Format (1).csv')

#### Mammals ####
mammal_d = TheD %>%
  filter(Region == "Marine Mammals") %>%
  # filter(Year > 1969) %>%
  group_by(ID) %>%
  mutate(Value = ifelse(Value == 0, 0.1*mean(Value[Value > 0], na.rm = TRUE), Value),
         Value_scale = Value/mean(Value),
         Value_log = log(Value_scale)) %>%
  ungroup() %>%
  mutate(ID = droplevels(as.factor(ID)),
         Taxa = droplevels(as.factor(Taxa))) 

mammal_d$Year_scaled <- mammal_d$Year - min(mammal_d$Year) + 1

mammal_fit <- sdmTMB(
  data = mammal_d,
  formula = Value_scale ~ 0 + (1 | ID) + (1 | Taxa), 
  time_varying = ~ 1,  
  time = "Year_scaled",
  family = tweedie(link = "log"),
  spatial = "off",      
  spatiotemporal = "off")

mammal_year_intercepts = tidy(mammal_fit, "ran_vals") %>%
  filter(is.na(group_name)) %>%
  mutate(
    time_index = as.numeric(str_extract(term, "\\d+")),
    Year = min(mammal_d$Year) + time_index - 1)

mammal_coverage_info <- mammal_d %>%
  group_by(Year) %>%
  summarise(n_stocks = n_distinct(ID)) %>%
  mutate(fraction = n_stocks / n_distinct(mammal_d$ID))

mammal_high_coverage_years <- mammal_coverage_info %>%
  filter(fraction > 0.2) %>%
  pull(Year)

# Rescale
mammal_target_level <- median(mammal_d$Value_scale[mammal_d$Year %in% mammal_high_coverage_years], na.rm = TRUE)
mammal_model_level_log <- mammal_year_intercepts$estimate[mammal_year_intercepts$Year %in% mammal_high_coverage_years]
mammal_model_level_geom <- mean(exp(mammal_model_level_log), na.rm = TRUE)
mammal_scalar <- mammal_target_level / mammal_model_level_geom

mammal_global_trend_table <- mammal_year_intercepts %>%
  left_join(mammal_coverage_info, by = "Year") %>%
  mutate(
    Abundance_Index = exp(estimate) * mammal_scalar,
    Upper_Bound = exp(conf.high) * mammal_scalar,
    Lower_Bound = exp(conf.low) * mammal_scalar)

mammal_plot = mammal_global_trend_table %>% 
  left_join(
    mammal_d %>% select(Year, ID, Value_scale),
    by = "Year") %>%
  ggplot() +
  geom_boxplot(
    aes(x = Year, y = Value_scale, group = Year, fill = fraction),
    outliers = F, color = NA, linewidth = 0.3, width = 1) +
  scale_fill_gradient(low = "white", high = "forestgreen", name = "Coverage",
                      limits = c(0, 1)) +
  ggnewscale::new_scale_fill() +
  stat_summary(
    aes(x = Year, y = Value_scale, group = Year),
    fun = median,
    geom = "point",
    color = "darkslategrey",
    size = 1,
    alpha=0.8,
    shape = 19) +
  geom_ribbon(
    aes(x = Year, ymin = Lower_Bound, ymax = Upper_Bound),
    fill = "cornflowerblue",
    alpha = 0.5) +
  geom_line(
    aes(x = Year, y = Abundance_Index),
    color = "darkblue",
    linewidth = 1) +
  theme_classic() + theme(legend.position =  "none") +
  labs(title = "Marine mammal", y = "Relative abundance", x = "Year") +
  coord_cartesian(xlim = c(1950, 2025), ylim = c(0, 1.7)) 


#### Marine Birds ####
bird_d = TheD %>%
  filter(Region == "marine birds") %>%
  filter(Year > 1950) %>%
  group_by(ID) %>%
  mutate(Value = ifelse(Value == 0, 0.1*mean(Value[Value > 0], na.rm = TRUE), Value),
         Value_scale = Value/mean(Value),
         Value_log = log(Value_scale)) %>%
  ungroup() %>%
  mutate(ID = droplevels(as.factor(ID)),
         Taxa = droplevels(as.factor(Taxa))) #%>%
  drop_na(Lat, Lon)

bird_d$Year_scaled <- bird_d$Year - min(bird_d$Year) + 1

bird_fit <- sdmTMB(
  data = bird_d,
  formula = Value_log ~ 0 + (1|ID) + (1  | Taxa), #0 + (1|ID) + (1| Taxa), # 0 + (1 + Year_scaled | ID) + (1 + Year_scaled | Taxa),
  time_varying = ~ 1,               
  time = "Year_scaled",
  family = gaussian(),
  extra_time = c(3, 4, 11, 13, 14, 18, 21, 40),
  spatial = "off",      
  spatiotemporal = "off")

bird_year_intercepts = tidy(bird_fit, "ran_vals") %>%
  filter(is.na(group_name)) %>%
  mutate(
    time_index = as.numeric(str_extract(term, "\\d+")),
    Year = min(bird_d$Year) + time_index - 1)

bird_coverage_info <- bird_d %>%
  group_by(Year) %>%
  summarise(n_stocks = n_distinct(ID)) %>%
  mutate(fraction = n_stocks / n_distinct(bird_d$ID))

bird_high_coverage_years <- bird_coverage_info %>%
  filter(fraction > 0.2) %>%
  pull(Year)

bird_target_level <- median(bird_d$Value_scale[bird_d$Year %in% bird_high_coverage_years], na.rm = TRUE)
bird_model_level_log <- bird_year_intercepts$estimate[bird_year_intercepts$Year %in% bird_high_coverage_years]
bird_model_level_geom <- mean(exp(bird_model_level_log), na.rm = TRUE)
bird_scalar <- bird_target_level / bird_model_level_geom

bird_global_trend_table <- bird_year_intercepts %>%
  left_join(bird_coverage_info, by = "Year") %>%
  mutate(
    Abundance_Index = exp(estimate) * bird_scalar,
    Upper_Bound = exp(conf.high) * bird_scalar,
    Lower_Bound = exp(conf.low) * bird_scalar)

bird_plot = bird_global_trend_table %>% 
  left_join(
    bird_d %>% select(Year, ID,Taxa, Value_scale),
    by = "Year") %>%
  ggplot() +
  geom_boxplot(
    aes(x = Year, y = Value_scale, group = Year, fill = fraction),
    outliers = F, color = NA, linewidth = 0.3, width = 1) +
  scale_fill_gradient(low = "white", high = "forestgreen", name = "Coverage",
                      limits = c(0, 1)) +
  ggnewscale::new_scale_fill() +
  stat_summary(
    aes(x = Year, y = Value_scale, group = Year),
    fun = median,
    geom = "point",
    color = "darkslategrey",
    size = 1,
    alpha=0.8,
    shape = 19) +
  geom_ribbon(
    aes(x = Year, ymin = Lower_Bound, ymax = Upper_Bound),
    fill = "cornflowerblue",
    alpha = 0.5) +
  geom_line(
    aes(x = Year, y = Abundance_Index),
    color = "darkblue",
    linewidth = 1) +
  theme_classic() + 
  theme(legend.position = "none") +
  labs(title = "Marine bird", y = "Relative abundance", x = "Year") +
  coord_cartesian(xlim = c(1950, 2020), ylim = c(0, 1.5)) 


#### Sharks and Rays ####
shark_d = TheD %>%
  filter(Region == "elasmobranch") %>%
  # filter(Year > 1969) %>%
  group_by(ID) %>%
  mutate(Value = ifelse(Value == 0, 0.1*mean(Value[Value > 0], na.rm = TRUE), Value),
         Value_scale = Value/mean(Value),
         Value_log = log(Value_scale)) %>%
  ungroup() %>%
  mutate(ID = droplevels(as.factor(ID)),
         Taxa = droplevels(as.factor(Taxa))) 

shark_d$Year_scaled <- shark_d$Year - min(shark_d$Year) + 1

shark_fit <- sdmTMB(
  data = shark_d,
  formula = Value_log ~ 0 + (1 | ID), 
  time_varying = ~ 1,  
  time = "Year_scaled",
  family = gaussian(),
  spatial = "off",      
  spatiotemporal = "off")

shark_year_intercepts = tidy(shark_fit, "ran_vals") %>%
  filter(is.na(group_name)) %>%
  mutate(
    time_index = as.numeric(str_extract(term, "\\d+")),
    Year = min(shark_d$Year) + time_index - 1)

shark_coverage_info <- shark_d %>%
  group_by(Year) %>%
  summarise(n_stocks = n_distinct(ID)) %>%
  mutate(fraction = n_stocks / n_distinct(shark_d$ID))

shark_high_coverage_years <- shark_coverage_info %>%
  filter(fraction > 0.2) %>%
  pull(Year)

shark_target_level <- median(shark_d$Value_scale[shark_d$Year %in% shark_high_coverage_years], na.rm = TRUE)
shark_model_level_log <- shark_year_intercepts$estimate[shark_year_intercepts$Year %in% shark_high_coverage_years]
shark_model_level_geom <- mean(exp(shark_model_level_log), na.rm = TRUE)
shark_scalar <- shark_target_level / shark_model_level_geom

shark_global_trend_table <- shark_year_intercepts %>%
  left_join(shark_coverage_info, by = "Year") %>%
  mutate(
    Abundance_Index = exp(estimate) * shark_scalar,
    Upper_Bound = exp(conf.high) * shark_scalar,
    Lower_Bound = exp(conf.low) * shark_scalar)

shark_plot = shark_global_trend_table %>% 
  left_join(
    shark_d %>% select(Year, ID, Value_scale),
    by = "Year") %>%
  ggplot() +
  geom_boxplot(
    aes(x = Year, y = Value_scale, group = Year, fill = fraction),
    outliers = F, color = NA, linewidth = 0.3, width = 1) +
  scale_fill_gradient(low = "white", high = "forestgreen", name = "Coverage",
                      limits = c(0, 1)) +
  ggnewscale::new_scale_fill() +
  stat_summary(
    aes(x = Year, y = Value_scale, group = Year),
    fun = median,
    geom = "point",
    color = "darkslategrey",
    size = 1,
    alpha=0.8,
    shape = 19) +
  geom_ribbon(
    aes(x = Year, ymin = Lower_Bound, ymax = Upper_Bound),
    fill = "cornflowerblue",
    alpha = 0.5) +
  geom_line(
    aes(x = Year, y = Abundance_Index),
    color = "darkblue",
    linewidth = 1) +
  theme_classic() +
  labs(title = "Cartilaginous fish", y = "Relative abundance", x = "Year") +
  theme(legend.position = "none") +
  coord_cartesian(xlim = c(1950, 2020), ylim = c(0, 2.5)) 


#### Seagrass ####
seagrass_d = read.csv("data/abundance/clean_area_ts_for_analysis.csv") %>% 
  select(study_site, site_groupings, studyid, site, year, area, log_area, 
         max_area, max_plus_0.1, depth_cat, first_year, last_year, duration, decade_time_points,
         num_unique_years, lat, lon, dom_species, all_species, lh, att_method, lat_zone2, 
         apriori_cat, tidied_drivers, measurement_cat, waycott_study, Lat_Zone, 
         ECOREGION, PROVINCE, bioregion, decade, site_duplication, 
         tidied_drivers) %>% 
  mutate_if(is.character, as.factor) %>%
  mutate(study_site = make.names(study_site)) %>% 
  mutate(ID = study_site, Year = year,Lat=lat,Lon=lon) %>%
  filter(Year > 1900) %>%
  group_by(ID) %>%
  mutate(Value = ifelse(area == 0, 0.1*mean(area[area > 0], na.rm = TRUE), area),
         Value_scale = Value/mean(Value),
         Value_log = log(Value_scale)) %>%
  ungroup() %>%
  mutate(ID = droplevels(as.factor(ID)),
         Taxa = droplevels(as.factor(dom_species)),
         bioregion_taxa = interaction(Taxa, bioregion))

seagrass_d$Year_scaled <- seagrass_d$Year - min(seagrass_d$Year) + 1

seagrass_fit <- sdmTMB(
  data = seagrass_d,
  formula = Value_log ~ 0 + (1 | ID) + (1 + Year_scaled | bioregion),
  time_varying = ~ 1,  
  extra_time = c(2, 4, 5, 6, 7, 8, 10, 12, 13),
  time = "Year_scaled",
  family = gaussian(),
  spatial = "off",      
  spatiotemporal = "off")

seagrass_year_intercepts = tidy(seagrass_fit, "ran_vals") %>%
  filter(is.na(group_name)) %>%
  mutate(
    time_index = as.numeric(str_extract(term, "\\d+")),
    Year = min(seagrass_d$Year) + time_index - 1)

seagrass_coverage_info <- seagrass_d %>%
  group_by(Year) %>%
  summarise(n_stocks = n_distinct(ID)) %>%
  mutate(fraction = n_stocks / n_distinct(seagrass_d$ID))

seagrass_high_coverage_years <- seagrass_coverage_info %>%
  filter(fraction > 0.2) %>%
  pull(Year)

seagrass_target_level <- median(seagrass_d$Value_scale[seagrass_d$Year %in% seagrass_high_coverage_years], na.rm = TRUE)
seagrass_model_level_log <- seagrass_year_intercepts$estimate[seagrass_year_intercepts$Year %in% seagrass_high_coverage_years]
seagrass_model_level_geom <- mean(exp(seagrass_model_level_log), na.rm = TRUE)
seagrass_scalar <- seagrass_target_level / seagrass_model_level_geom

seagrass_global_trend_table <- seagrass_year_intercepts %>%
  left_join(seagrass_coverage_info, by = "Year") %>%
  mutate(
    Abundance_Index = exp(estimate) * seagrass_scalar,
    Upper_Bound = exp(conf.high) * seagrass_scalar,
    Lower_Bound = exp(conf.low) * seagrass_scalar)

seagrass_plot = seagrass_global_trend_table %>% 
  left_join(
    seagrass_d %>% select(Year, ID, Value_scale),
    by = "Year") %>%
  ggplot() +
  geom_boxplot(
    aes(x = Year, y = Value_scale, group = Year, fill = fraction),
    outliers = F, color = NA, linewidth = 0.3, width = 1) +
  scale_fill_gradient(low = "white", high = "forestgreen", name = "Coverage",
                      limits = c(0, 1)) +
  ggnewscale::new_scale_fill() +
  stat_summary(
    aes(x = Year, y = Value_scale, group = Year),
    fun = median,
    geom = "point",
    color = "darkslategrey",
    size = 1,
    alpha=0.8,
    shape = 19) +
  geom_ribbon(
    aes(x = Year, ymin = Lower_Bound, ymax = Upper_Bound),
    fill = "cornflowerblue",
    alpha = 0.5) +
  geom_line(
    aes(x = Year, y = Abundance_Index),
    color = "darkblue",
    linewidth = 1) +
  theme_classic() +
  labs(title = "Seagrass", y = "Relative area", x = "Year") +
  theme(legend.position = "none") +
  coord_cartesian(xlim = c(1950, 2020), ylim = c(0, 2)) 


#### Kelp ####
kelp_genus_family = read.csv("data/abundance/kelp_family_genus.csv")

kelp_d = read.csv("data/abundance/krumhansl_kelp_timeseries_raw (1).csv") %>%
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
  drop_na(Density)   %>%
  select(Site, Sample.Year, Sample.Month, Taxon, Genus, Family, Density, Realm,
         Latitude, Longitude) %>% 
  mutate_if(is.character, as.factor) %>%
  mutate(ID = Site, Year = Sample.Year, Lat = Latitude, Lon = Longitude) %>%
  filter(Year > 1900) %>%
  group_by(ID) %>%
  mutate(Value = ifelse(Density == 0, 0.01*mean(Density[Density > 0], na.rm = TRUE), Density),
         Value_scale = Value/mean(Value),
         Value_log = log(Value_scale)) %>%
  ungroup() %>%
  mutate(ID = droplevels(as.factor(ID)),
         Family = droplevels(as.factor(Family)),
         Realm = droplevels(as.factor(Realm)),
         Realm_Month = interaction(Sample.Month, Realm)) 

kelp_d$Year_scaled <- kelp_d$Year - min(kelp_d$Year) + 1

kelp_fit <- sdmTMB(
  data = kelp_d,
  formula = Value_scale ~ 0 + (1 | ID) + (1 | Realm_Month)  + (1 + Year_scaled | Family) +  (1 + Year_scaled | Realm),
  time_varying = ~ 1,  
  extra_time = c(6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 21),
  time = "Year_scaled",
  family = gaussian(), #tweedie(link = "log"),
  spatial = "off",      
  spatiotemporal = "off")

kelp_year_intercepts = tidy(kelp_fit, "ran_vals") %>%
  filter(is.na(group_name)) %>%
  mutate(
    time_index = as.numeric(str_extract(term, "\\d+")),
    Year = min(kelp_d$Year) + time_index - 1)

kelp_coverage_info <- kelp_d %>%
  group_by(Year) %>%
  summarise(n_stocks = n_distinct(ID)) %>%
  mutate(fraction = n_stocks / n_distinct(kelp_d$ID))

kelp_high_coverage_years <- kelp_coverage_info %>%
  filter(fraction > 0.2) %>%
  pull(Year)

kelp_target_level <- median(kelp_d$Value_scale[kelp_d$Year %in% kelp_high_coverage_years], na.rm = TRUE)
kelp_model_level_log <- kelp_year_intercepts$estimate[kelp_year_intercepts$Year %in% kelp_high_coverage_years]
kelp_model_level_geom <- mean(exp(kelp_model_level_log), na.rm = TRUE)
kelp_scalar <- kelp_target_level / kelp_model_level_geom

kelp_global_trend_table <- kelp_year_intercepts %>%
  left_join(kelp_coverage_info, by = "Year") %>%
  mutate(
    Abundance_Index = exp(estimate) * kelp_scalar,
    Upper_Bound = exp(conf.high) * kelp_scalar,
    Lower_Bound = exp(conf.low) * kelp_scalar)

kelp_plot = kelp_global_trend_table %>% 
  left_join(
    kelp_d %>% select(Year, ID, Value_scale),
    by = "Year") %>%
  ggplot() +
  geom_boxplot(
    aes(x = Year, y = Value_scale, group = Year, fill = fraction),
    outliers = F, color =NA, linewidth = 0.3, width = 1) +
  scale_fill_gradient(low = "white", high = "forestgreen", name = "Coverage",
                      limits = c(0, 1)) +
  ggnewscale::new_scale_fill() +
  stat_summary(
    aes(x = Year, y = Value_scale, group = Year),
    fun = median,
    geom = "point",
    color = "darkslategrey",
    size = 1,
    alpha=0.8,
    shape = 19) +
  geom_ribbon(
    aes(x = Year, ymin = Lower_Bound, ymax = Upper_Bound),
    fill = "cornflowerblue",
    alpha = 0.5) +
  geom_line(
    aes(x = Year, y = Abundance_Index),
    color = "darkblue",
    linewidth = 1) +
  theme_classic() +
  labs(title = "Kelp", y = "Relative density", x = "Year") +
  theme(legend.position = "none") +
  coord_cartesian(xlim = c(1980, 2020),ylim=c(0,0.1)) 
kelp_plot

#### Turtles ####
turtle_d = TheD %>%
  filter(Region == "turtle") %>%
  group_by(ID) %>%
  mutate(Value = ifelse(Value == 0, 0.1*mean(Value[Value > 0], na.rm = TRUE), Value),
         Value_scale = Value/mean(Value),
         Value_log = log(Value_scale)) %>%
  ungroup() %>%
  mutate(ID = droplevels(as.factor(ID)),
         Taxa = droplevels(as.factor(Taxa))) 

turtle_d$Year_scaled <- turtle_d$Year - min(turtle_d$Year) + 1

turtle_fit <- sdmTMB(
  data = turtle_d,
  formula = Value_log ~ 0 + (1 | ID) + (1 + Year_scaled | Taxa), 
  time_varying = ~ 1,               
  time = "Year_scaled",
  family = gaussian(),
  spatial = "off",      
  spatiotemporal = "off")

turtle_year_intercepts = tidy(turtle_fit, "ran_vals") %>%
  filter(is.na(group_name)) %>%
  mutate(
    time_index = as.numeric(str_extract(term, "\\d+")),
    Year = min(turtle_d$Year) + time_index - 1)

turtle_coverage_info <- turtle_d %>%
  group_by(Year) %>%
  summarise(n_stocks = n_distinct(ID)) %>%
  mutate(fraction = n_stocks / n_distinct(turtle_d$ID))

turtle_high_coverage_years <- turtle_coverage_info %>%
  filter(fraction > 0.2) %>%
  pull(Year)

turtle_target_level <- median(turtle_d$Value_scale[turtle_d$Year %in% turtle_high_coverage_years], na.rm = TRUE)
turtle_model_level_log <- turtle_year_intercepts$estimate[turtle_year_intercepts$Year %in% turtle_high_coverage_years]
turtle_model_level_geom <- mean(exp(turtle_model_level_log), na.rm = TRUE)
turtle_scalar <- turtle_target_level / turtle_model_level_geom

turtle_global_trend_table <- turtle_year_intercepts %>%
  left_join(turtle_coverage_info, by = "Year") %>%
  mutate(
    Abundance_Index = exp(estimate) * turtle_scalar,
    Upper_Bound = exp(conf.high) * turtle_scalar,
    Lower_Bound = exp(conf.low) * turtle_scalar)

turtle_plot = turtle_global_trend_table %>% 
  left_join(
    turtle_d %>% select(Year, ID, Value_scale),
    by = "Year") %>%
  ggplot() +
  geom_boxplot(
    aes(x = Year, y = Value_scale, group = Year, fill = fraction),
    outliers = F, color = NA, linewidth = 0.3, width = 1) +
  scale_fill_gradient(low = "white", high = "forestgreen", name = "Coverage",
                      limits = c(0, 1)) +
  ggnewscale::new_scale_fill() +
  stat_summary(
    aes(x = Year, y = Value_scale, group = Year),
    fun = median,
    geom = "point",
    color = "darkslategrey",
    size = 1,
    alpha=0.8,
    shape = 19) +
  geom_ribbon(
    aes(x = Year, ymin = Lower_Bound, ymax = Upper_Bound),
    fill = "cornflowerblue",
    alpha = 0.5) +
  geom_line(
    aes(x = Year, y = Abundance_Index),
    color = "darkblue",
    linewidth = 1) +
  theme_classic()+ theme(legend.position = "none")+
  labs(title = "Sea turtle", y = "Relative abundance", x = "Year") +
  coord_cartesian(xlim = c(1950, 2025), ylim = c(0, 3)) 


#### Coral ####
coral_d = read.csv("data/abundance/souter-et-al_data-models.csv") %>%
  filter(region == "Global", category == "Hard coral") %>%
  mutate(Year = year, 
         Abundance_Index = (mean)/mean(mean), 
         Lower_Bound = (lower_ci_95)/mean(mean), 
         Upper_Bound = (higher_ci_95)/mean(mean))

coral_plot = coral_d %>% 
  ggplot() + 
  geom_ribbon(aes(x = Year, ymin = Lower_Bound, ymax = Upper_Bound), 
              fill = "cornflowerblue", alpha = 0.5) +
  geom_line(aes(x = Year, y = Abundance_Index), color = "blue", size = 1) +
  theme_classic() + 
  labs(title = "Hard coral",
        x = "Year",
        y = "Relative % cover") 


#### Mangroves ####
mangrove_d = readxl::read_xlsx("data/abundance/gmw_v3_country_statistics_ha.xlsx") %>%
  pivot_longer(cols = matches("^\\d{4}$"), names_to = "Year", values_to = "Area") %>%
  filter(Name == "Global (km2)") %>%
  drop_na(Area) %>%
  mutate(Year = as.numeric(Year),
         Abundance_Index = (Area)/mean(Area), 
         Lower_Bound = (Area)/mean(Area), 
         Upper_Bound = (Area)/mean(Area)) %>% 
  ungroup()

mangrove_plot = mangrove_d %>% 
  ggplot() + 
  geom_line(aes(x = Year, y = Abundance_Index), color = "blue", size = 1) +
  theme_classic() + 
  labs(title = "Mangrove",
        x = "Year",
        y = "Relative area") + 
  ylim(c(0, 1.1))


#### Salt Marshes ####
saltmarsh_d <- read_csv("data/abundance/SaltMarshExtent.csv") %>%
  summarise(
    `2000-2004` = sum(`Area_2000-2004.Ha`, na.rm = TRUE),
    `2005-2009` = sum(`Area_ 2005-2009.Ha`, na.rm = TRUE),
    `2010-2014` = sum(`Area_ 2010-2014.Ha`, na.rm = TRUE),
    `2015-2019` = sum(`Area_ 2015-2019.Ha`, na.rm = TRUE)
  ) %>%
  pivot_longer(cols = everything(), names_to = "Timeline", values_to = "Global_Area") %>%
  mutate(Timeline = factor(Timeline, levels = c("2000-2004", "2005-2009", "2010-2014", "2015-2019"))) %>%
  mutate(Abundance_Index = (Global_Area)/mean(Global_Area), 
         Lower_Bound = (Global_Area)/mean(Global_Area), 
         Upper_Bound = (Global_Area)/mean(Global_Area)) %>% 
  ungroup()

saltmarsh_plot = ggplot(saltmarsh_d, aes(x = Timeline, y = Abundance_Index, group = 1)) + 
  geom_line(size = 1, color = "blue") +
  theme_classic()+theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))+
  scale_y_continuous(expand = c(0, 0),
                     limits = c(0, 1.2)) +
  labs(title = "Salt marsh",
       x = "Year",
       y = "Relative area")

#### Forage fish ####
forage_RAM_d = read.csv("data/abundance/final_dataset.csv") %>%
  filter(taxGroup == "forage fish") %>%
  mutate(ID = stockid) %>%
  group_by(ID) %>%
  # Filter to keep all years for only the alphabetically first unit per ID
  filter(unit == min(as.character(unit), na.rm = TRUE)) %>%
  mutate(
    Value = ts_raw,         
    Value_scale = Value / median(Value, na.rm = TRUE),
    Value_log = log(Value_scale),
    Year = year
  ) %>%
  ungroup() %>%
  mutate(ID = droplevels(as.factor(ID)))

forage_RAM_d$Year_scaled <- forage_RAM_d$Year - min(forage_RAM_d$Year) + 1

forage_fit <- sdmTMB(
  data = forage_RAM_d,
  formula = Value_log ~ 0 + (1 | ID), 
  time_varying = ~ 1,               
  time = "Year_scaled",
  family = gaussian(),
  spatial = "off",      
  spatiotemporal = "off")

forage_year_intercepts = tidy(forage_fit, "ran_vals") %>%
  filter(is.na(group_name)) %>%
  mutate(
    time_index = as.numeric(str_extract(term, "\\d+")),
    Year = min(forage_RAM_d$Year) + time_index - 1)

forage_coverage_info <- forage_RAM_d %>%
  group_by(Year) %>%
  summarise(n_stocks = n_distinct(ID)) %>%
  mutate(fraction = n_stocks / n_distinct(forage_RAM_d$ID))

forage_high_coverage_years <- forage_coverage_info %>%
  filter(fraction > 0.15) %>%
  pull(Year)

forage_target_level <- median(forage_RAM_d$Value_scale[forage_RAM_d$Year %in% forage_high_coverage_years], na.rm = TRUE)
forage_model_level_log <- forage_year_intercepts$estimate[forage_year_intercepts$Year %in% forage_high_coverage_years]
forage_model_level_geom <- mean(exp(forage_model_level_log), na.rm = TRUE)
forage_scalar <- abs(forage_target_level / forage_model_level_geom)

forage_global_trend_table <- forage_year_intercepts %>%
  left_join(forage_coverage_info, by = "Year") %>%
  mutate(
    Abundance_Index = exp(estimate) * forage_scalar,
    Upper_Bound = exp(conf.high) * forage_scalar,
    Lower_Bound = exp(conf.low) * forage_scalar)

forage_plot = forage_global_trend_table %>% 
  left_join(
    forage_RAM_d %>% select(Year, ID, Value_scale),
    by = "Year") %>%
  ggplot() +
  geom_boxplot(
    aes(x = Year, y = Value_scale, group = Year, fill = fraction),
    outliers = F, color = NA, linewidth = 0.3, width = 1) +
  scale_fill_gradient(low = "white", high = "forestgreen", name = "Coverage",
                      limits = c(0, 1)) +
  ggnewscale::new_scale_fill() +
  stat_summary(
    aes(x = Year, y = Value_scale, group = Year),
    fun = median,
    geom = "point",
    color = "darkslategrey",
    size = 1,
    alpha=0.8,
    shape = 19) +
  geom_ribbon(
    aes(x = Year, ymin = Lower_Bound, ymax = Upper_Bound),
    fill = "cornflowerblue",
    alpha = 0.5) +
  geom_line(
    aes(x = Year, y = Abundance_Index),
    color = "darkblue",
    linewidth = 1) +
  theme_classic()+ theme(legend.position = "none")+
  labs(title = "Forage fish", y = "Relative abundance", x = "Year") +
  coord_cartesian(xlim = c(1950, 2025), ylim = c(0,2)) 
forage_plot

#### Other exploited fish ####
other_RAM_d = read.csv("data/abundance/final_dataset.csv") %>%
  filter(taxa == "Fish" & taxGroup != "forage fish") %>%
  mutate(ID = stockid) %>%
  group_by(ID) %>%
  # Filter to keep all years for only the alphabetically first unit per ID
  filter(unit == min(as.character(unit), na.rm = TRUE)) %>%
  mutate(
    Value = ts_raw, 
    Value = ifelse(Value == 0, 0.05*mean(Value[Value > 0], na.rm = TRUE), Value),
    Value_scale = Value / median(Value, na.rm = TRUE),
    Value_log = log(Value_scale),
    Year = year
  ) %>%
  ungroup() %>%
  mutate(ID = droplevels(as.factor(ID)))

other_RAM_d$Year_scaled <- other_RAM_d$Year - min(other_RAM_d$Year) + 1

other_fit <- sdmTMB(
  data = other_RAM_d,
  formula = Value_log ~ 0 + (1 | ID) + (1 | taxGroup), 
  time_varying = ~ 1,               
  time = "Year_scaled",
  family = gaussian(),
  spatial = "off",      
  spatiotemporal = "off")

other_year_intercepts = tidy(other_fit, "ran_vals") %>%
  filter(is.na(group_name)) %>%
  mutate(
    time_index = as.numeric(str_extract(term, "\\d+")),
    Year = min(other_RAM_d$Year) + time_index - 1)

other_coverage_info <- other_RAM_d %>%
  group_by(Year) %>%
  summarise(n_stocks = n_distinct(ID)) %>%
  mutate(fraction = n_stocks / n_distinct(other_RAM_d$ID))

other_high_coverage_years <- other_coverage_info %>%
  filter(fraction > 0.2) %>%
  pull(Year)

other_target_level <- median(other_RAM_d$Value_scale[other_RAM_d$Year %in% other_high_coverage_years], na.rm = TRUE)
other_model_level_log <- other_year_intercepts$estimate[other_year_intercepts$Year %in% other_high_coverage_years]
other_model_level_geom <- mean(exp(other_model_level_log), na.rm = TRUE)
other_scalar <- abs(other_target_level / other_model_level_geom)

other_global_trend_table <- other_year_intercepts %>%
  left_join(other_coverage_info, by = "Year") %>%
  mutate(
    Abundance_Index = exp(estimate) * other_scalar,
    Upper_Bound = exp(conf.high) * other_scalar,
    Lower_Bound = exp(conf.low) * other_scalar)

other_plot = other_global_trend_table %>% 
  left_join(
    other_RAM_d %>% select(Year, ID, Value_scale),
    by = "Year") %>%
  ggplot() +
  geom_boxplot(
    aes(x = Year, y = Value_scale, group = Year, fill = fraction),
    outliers = F, color = NA, linewidth = 0.3, width = 1) +
  scale_fill_gradient(low = "white", high = "forestgreen", name = "Coverage",
                      limits = c(0, 1)) +
  ggnewscale::new_scale_fill() +
  stat_summary(
    aes(x = Year, y = Value_scale, group = Year),
    fun = median,
    geom = "point",
    color = "darkslategrey",
    size = 1,
    alpha=0.8,
    shape = 19) +
  geom_ribbon(
    aes(x = Year, ymin = Lower_Bound, ymax = Upper_Bound),
    fill = "cornflowerblue",
    alpha = 0.5) +
  geom_line(
    aes(x = Year, y = Abundance_Index),
    color = "darkblue",
    linewidth = 1) +
  theme_classic()+ theme(legend.position = "none")+
  labs(title = "Other exploited fish", y = "Relative abundance", x = "Year") +
  coord_cartesian(xlim = c(1950, 2025), ylim = c(0, 2)) 
other_plot

#### Crustaceans ####
crust_RAM_d = read.csv("data/abundance/final_dataset.csv") %>%
  filter(taxa == "Crustaceans") %>%
  mutate(ID = stockid) %>%
  group_by(ID) %>%
  # Filter to keep all years for only the alphabetically first unit per ID
  filter(unit == min(as.character(unit), na.rm = TRUE)) %>%
  mutate(
    Value = ts_raw,         
    Value_scale = Value / mean(Value, na.rm = TRUE),
    Value_log = log(Value_scale),
    Year = year
  ) %>%
  ungroup() %>%
  mutate(ID = droplevels(as.factor(ID)))

crust_RAM_d$Year_scaled <- crust_RAM_d$Year - min(crust_RAM_d$Year) + 1

crust_fit <- sdmTMB(
  data = crust_RAM_d,
  formula = Value_log ~ 0 + (1 | ID), 
  time_varying = ~ 1,               
  time = "Year_scaled",
  family = gaussian(),
  spatial = "off",      
  spatiotemporal = "off")

crust_year_intercepts = tidy(crust_fit, "ran_vals") %>%
  filter(is.na(group_name)) %>%
  mutate(
    time_index = as.numeric(str_extract(term, "\\d+")),
    Year = min(crust_RAM_d$Year) + time_index - 1)

crust_coverage_info <- crust_RAM_d %>%
  group_by(Year) %>%
  summarise(n_stocks = n_distinct(ID)) %>%
  mutate(fraction = n_stocks / n_distinct(crust_RAM_d$ID))

crust_high_coverage_years <- crust_coverage_info %>%
  filter(fraction > 0.2) %>%
  pull(Year)

crust_target_level <- mean(crust_RAM_d$Value_scale[crust_RAM_d$Year %in% crust_high_coverage_years], na.rm = TRUE)
crust_model_level_log <- crust_year_intercepts$estimate[crust_year_intercepts$Year %in% crust_high_coverage_years]
crust_model_level_geom <- mean(exp(crust_model_level_log), na.rm = TRUE)
crust_scalar <- abs(crust_target_level / crust_model_level_geom)

crust_global_trend_table <- crust_year_intercepts %>%
  left_join(crust_coverage_info, by = "Year") %>%
  mutate(
    Abundance_Index = exp(estimate) * crust_scalar,
    Upper_Bound = exp(conf.high) * crust_scalar,
    Lower_Bound = exp(conf.low) * crust_scalar)

crust_plot = crust_global_trend_table %>% 
  left_join(
    crust_RAM_d %>% select(Year, ID, Value_scale),
    by = "Year") %>%
  ggplot() +
  geom_boxplot(
    aes(x = Year, y = Value_scale, group = Year, fill = fraction),
    outliers = F, color = NA, linewidth = 0.3, width = 1) +
  scale_fill_gradient(low = "white", high = "forestgreen", name = "Coverage",
                      limits = c(0, 1)) +
  ggnewscale::new_scale_fill() +
  stat_summary(
    aes(x = Year, y = Value_scale, group = Year),
    fun = median,
    geom = "point",
    color = "darkslategrey",
    size = 1,
    alpha=0.8,
    shape = 19) +
  geom_ribbon(
    aes(x = Year, ymin = Lower_Bound, ymax = Upper_Bound),
    fill = "cornflowerblue",
    alpha = 0.5) +
  geom_line(
    aes(x = Year, y = Abundance_Index),
    color = "darkblue",
    linewidth = 1) +
  theme_classic()+ theme(legend.position = "none")+
  labs(title = "Exploited crustaceans", y = "Relative abundance", x = "Year") +
  coord_cartesian(xlim = c(1960, 2025), ylim = c(0, 3)) 
crust_plot

#### Deep sea fish ####
mycto_d = read.csv("data/abundance/myctobase_clean_unexploitedpelagics.csv") %>%
  mutate(ID = scientificName) %>%
  group_by(ID) %>%
  mutate(Value = n_m3,
         Value_scale = Value/mean(Value,na.rm=T),
         Value_log = log(Value_scale),
         Year=as.numeric(year)) %>%
  ungroup() %>%
  mutate(ID = droplevels(as.factor(ID))) %>%
  drop_na(Year)

mycto_d$Year_scaled <- mycto_d$Year - min(mycto_d$Year) + 1

mycto_fit <- sdmTMB(
  data = mycto_d,
  formula = Value_log ~ 0 + (1 | ID), 
  time_varying = ~ 1,               
  time = "Year_scaled",
  family = gaussian(),
  spatial = "off",      
  extra_time = c(27, 28),
  spatiotemporal = "off")

mycto_year_intercepts = tidy(mycto_fit, "ran_vals") %>%
  filter(is.na(group_name)) %>%
  mutate(
    time_index = as.numeric(str_extract(term, "\\d+")),
    Year = min(mycto_d$Year) + time_index - 1)

mycto_coverage_info <- mycto_d %>%
  group_by(Year) %>%
  summarise(n_stocks = n_distinct(ID)) %>%
  mutate(fraction = n_stocks / n_distinct(mycto_d$ID))

mycto_high_coverage_years <- mycto_coverage_info %>%
  filter(fraction > 0.05) %>%
  pull(Year)

mycto_target_level <- median(mycto_d$Value[mycto_d$Year %in% mycto_high_coverage_years], na.rm = TRUE)
mycto_model_level_log <- mycto_year_intercepts$estimate[mycto_year_intercepts$Year %in% mycto_high_coverage_years]
mycto_model_level_geom <- mean(exp(mycto_model_level_log), na.rm = TRUE)
mycto_scalar <- abs(mycto_target_level / mycto_model_level_geom)

mycto_global_trend_table <- mycto_year_intercepts %>%
  left_join(mycto_coverage_info, by = "Year") %>%
  mutate(
    Abundance_Index = exp(estimate) * mycto_scalar,
    Upper_Bound = exp(conf.high) * mycto_scalar,
    Lower_Bound = exp(conf.low) * mycto_scalar)

mycto_plot = mycto_global_trend_table %>% 
  left_join(
    mycto_d %>% select(Year, ID, Value),
    by = "Year") %>%
  ggplot() +
  geom_boxplot(
    aes(x = Year, y = Value, group = Year, fill = fraction),
    outliers = F, color = NA, linewidth = 0.3, width = 1) +
  scale_fill_gradient(low = "white", high = "forestgreen", name = "Coverage",
                      limits = c(0, 1)) +
  ggnewscale::new_scale_fill() +
  stat_summary(
    aes(x = Year, y = Value, group = Year),
    fun = median,
    geom = "point",
    color = "darkslategrey",
    size = 1,
    alpha=0.8,
    shape = 19) +
  geom_ribbon(
    aes(x = Year, ymin = Lower_Bound, ymax = Upper_Bound),
    fill = "cornflowerblue",
    alpha = 0.5) +
  geom_line(
    aes(x = Year, y = Abundance_Index),
    color = "darkblue",
    linewidth = 1) +
  theme_classic()+ theme(legend.position = "none")+
  labs(title = "Deep sea pelagics", y = "Relative abundance", x = "Year") +
  coord_cartesian(xlim = c(1990, 2020), ylim = c(0, 0.001)) 

#### Nonexploited fish ####
noCfish_d = read.csv("data/abundance/LPI_data_CorNC.csv") %>%
  filter(custom_class == "Bony fish - NC") %>%
  mutate(Year = year, Value = popvalue, Taxa = Binomial) %>%
  group_by(ID) %>%
  mutate(Value = ifelse(Value == 0, 0.1*mean(Value[Value > 0], na.rm = TRUE), Value),
         Value_scale = Value/mean(Value),
         Value_log = log(Value_scale)) %>%
  ungroup() %>%
  mutate(ID = droplevels(as.factor(ID)),
         Taxa = droplevels(as.factor(Taxa))) 

noCfish_d$Year_scaled <- noCfish_d$Year - min(noCfish_d$Year) + 1

noCfish_fit <- sdmTMB(
  data = noCfish_d,
  formula = Value_log ~ 0 + (1 | ID) + (1 | Taxa), 
  time_varying = ~ 1,  
  time = "Year_scaled",
  family = gaussian(),
  spatial = "off",      
  spatiotemporal = "off")

noCfish_year_intercepts = tidy(noCfish_fit, "ran_vals") %>%
  filter(is.na(group_name)) %>%
  mutate(
    time_index = as.numeric(str_extract(term, "\\d+")),
    Year = min(noCfish_d$Year) + time_index - 1)

noCfish_coverage_info <- noCfish_d %>%
  group_by(Year) %>%
  summarise(n_stocks = n_distinct(ID)) %>%
  mutate(fraction = n_stocks / n_distinct(noCfish_d$ID))

noCfish_high_coverage_years <- noCfish_coverage_info %>%
  filter(fraction > 0.1) %>%
  pull(Year)

# Rescale
noCfish_target_level <- median(noCfish_d$Value_scale[noCfish_d$Year %in% noCfish_high_coverage_years], na.rm = TRUE)
noCfish_model_level_log <- noCfish_year_intercepts$estimate[noCfish_year_intercepts$Year %in% noCfish_high_coverage_years]
noCfish_model_level_geom <- mean(exp(noCfish_model_level_log), na.rm = TRUE)
noCfish_scalar <- noCfish_target_level / noCfish_model_level_geom

noCfish_global_trend_table <- noCfish_year_intercepts %>%
  left_join(noCfish_coverage_info, by = "Year") %>%
  mutate(
    Abundance_Index = exp(estimate) * noCfish_scalar,
    Upper_Bound = exp(conf.high) * noCfish_scalar,
    Lower_Bound = exp(conf.low) * noCfish_scalar)

noCfish_plot = noCfish_global_trend_table %>% 
  left_join(
    noCfish_d %>% select(Year, ID, Value_scale),
    by = "Year") %>%
  ggplot() +
  geom_boxplot(
    aes(x = Year, y = Value_scale, group = Year, fill = fraction),
    outliers = F, color = NA, linewidth = 0.3, width = 1) +
  scale_fill_gradient(low = "white", high = "forestgreen", name = "Coverage",
                      limits = c(0, 1)) +
  ggnewscale::new_scale_fill() +
  stat_summary(
    aes(x = Year, y = Value_scale, group = Year),
    fun = median,
    geom = "point",
    color = "darkslategrey",
    size = 1,
    alpha=0.8,
    shape = 19) +
  geom_ribbon(
    aes(x = Year, ymin = Lower_Bound, ymax = Upper_Bound),
    fill = "cornflowerblue",
    alpha = 0.5) +
  geom_line(
    aes(x = Year, y = Abundance_Index),
    color = "darkblue",
    linewidth = 1) +
  theme_classic() + theme(legend.position =  "none") +
  labs(title = "Non-exploited fish", y = "Relative abundance", x = "Year") +
  coord_cartesian(xlim = c(1950, 2025), ylim = c(0, 1.7))

#### Combined Plot ####

library(patchwork)

# 1. Put all your plots into a list or a combined object
# Note: Ensure your individual plots (mammal_plot, bird_plot, etc.) 
# have 'legend.position = "none"' or just let patchwork handle it.

combined_plots <- (
  mammal_plot + bird_plot + turtle_plot + 
    shark_plot + forage_plot + other_plot + noCfish_plot +
    mycto_plot + crust_plot + seagrass_plot +
    kelp_plot + coral_plot + mangrove_plot + 
    saltmarsh_plot
) + 
  plot_layout(ncol = 3, guides = "collect") & 
  theme(legend.position = "bottom")

# 2. To ensure the legend specifically uses your "Coverage" styling:
final_figure <- combined_plots & 
  scale_fill_gradient(low = "white", high = "forestgreen", 
                      name = "Coverage", limits = c(0, 1))

print(final_figure)
