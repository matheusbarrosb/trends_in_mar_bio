rm(list = ls())

# Load packages ----------------------------------------------------------------
package_list = c("dplyr", "ggplot2", "here", "tidyr", "ggpubr", "readr")
lapply(package_list, function(pkg) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
    library(pkg, character.only = TRUE)
  }
})

# Load functions ---------------------------------------------------------------
function_directory = file.path(here::here(), "R", "Functions/")
function_files     = list.files(function_directory)
for (i in seq_along(function_files)) {
  source(paste0(function_directory, function_files[i]))
}

# Load data --------------------------------------------------------------------
data_directory = file.path(here::here(), "data", "RAM")
data_files     = list.files(data_directory)
print(data_files)

for (i in seq_along(data_files)) {
  assign(
    gsub(".csv", "", data_files[i]), 
    read_csv(paste0(data_directory, "/", data_files[i]))
  )
}

# Format RAM data --------------------------------------------------------------
ram_processed = ts %>%
  left_join(stock, by = "stockid") %>%
  left_join(area, by = "areaid") %>%
  left_join(taxonomy, by = "scientificname") %>%
  filter(
    !classname %in% c("Elasmobranchii", "Chondrichthyes"),
    taxGroup != "elasmobranchs"
  ) %>%
  filter(
    classname %in% c("Actinopterygii", "Teleostei", "Malacostraca") |
      taxGroup %in% c("crabs-lobsters", "shrimps", "forage fish", 
                      "other marine percoidids", "other marine fish", 
                      "pleuronectids", "gadids", "carangids-mackerels", 
                      "salmonids", "sebastids", "other scorpaenids", 
                      "tuna-billfish", "eels")
  ) %>%
  mutate(
    broad_taxa = case_when(
      classname == "Malacostraca" | taxGroup %in% c("crabs-lobsters", "shrimps") ~ "Crustaceans",
      TRUE ~ "Fish" 
    ),
    exploitation = "exploited" 
  ) %>%
  group_by(stockid, tsid) %>%
  mutate(
    z_score = (tsvalue - mean(tsvalue, na.rm = TRUE)) / sd(tsvalue, na.rm = TRUE),
    tsvalue = tsvalue
  ) %>%
  ungroup()

ram_base = ram_processed %>%
  select(
    year         = tsyear,
    country      = country,
    stockid      = stockid,
    taxa         = broad_taxa,
    taxGroup     = taxGroup,
    exploitation = exploitation,
    unit         = tsid,
    z_score      = z_score,
    ts_raw       = tsvalue
  )

ram_multinational_dupes = ram_processed %>%
  filter(country == "multinational", !is.na(primary_country)) %>%
  select(
    year         = tsyear,
    country      = primary_country,
    stockid      = stockid,
    taxa         = broad_taxa,
    taxGroup     = taxGroup,
    exploitation = exploitation,
    unit         = tsid,
    z_score      = z_score,
    ts_raw       = tsvalue
  )

final_dataset_ram = bind_rows(ram_base, ram_multinational_dupes) %>%
  filter(!is.na(z_score)) %>%
  distinct()

# format small pelagics data ---------------------------------------------------
small_pelagics_formatted = small_pelagics_data %>%
  filter(
    Group == "Unexploited small pelagics"
  ) %>%
  group_by(Group, unit) %>%
  mutate(
    z_score = (value - mean(value, na.rm = TRUE)) / sd(value, na.rm = TRUE),
    tsvalue = value
  ) %>%
  ungroup() %>%
  select(
    year    = Year,
    country = Region,       
    stockid = Group,        
    ID      = ID,
    unit    = unit,
    z_score = z_score,
    ts_raw  = tsvalue
  ) %>%
  mutate(
    taxa = "Fish",
    taxGroup = "forage fish", 
    exploitation = ifelse(ID == "RAM", "exploited", "unexploited") 
  ) %>%
  select(year, country, stockid, taxa, taxGroup, exploitation, unit, z_score, ts_raw) %>%
  filter(!is.na(z_score)) %>%
  distinct()

final_dataset = bind_rows(final_dataset_ram, small_pelagics_formatted);final_dataset

# Summary statistics per taxGroup ---------------------------------------------------------------
final_dataset %>%
  mutate(
    taxGroup = case_when(
      taxGroup == "forage fish" ~ "forage fish",
      taxa == "Fish" & taxGroup != "forage fish" ~ "other fish",
      TRUE ~ taxGroup
    )
  ) %>%
  group_by(year, taxGroup) %>%
  summarise(
    median_val = median(z_score, na.rm = TRUE),
    lower = quantile(z_score, probs = 0.025, na.rm = TRUE),
    upper = quantile(z_score, probs = 0.975, na.rm = TRUE),
    n_stocks = n(),
    .groups = "drop"
  ) %>%
  group_by(taxGroup) %>%
  summarise(
    overall_median = median(median_val, na.rm = TRUE),
    overall_lower = quantile(median_val, probs = 0.025, na.rm = TRUE),
    overall_upper = quantile(median_val, probs = 0.975, na.rm = TRUE),
    total_stocks = sum(n_stocks, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(overall_median))


# Plot -------------------------------------------------------------------------
forage_trend_plot = 
  final_dataset %>%
  group_by(country, year, taxa) %>%
  filter(taxa == "Fish") %>%
  filter(taxGroup == "forage fish") %>%
  summarise(
    median_val = median(z_score, na.rm = TRUE),
    lower = quantile(z_score, probs = 0.025, na.rm = TRUE),
    upper = quantile(z_score, probs = 0.975, na.rm = TRUE),
    n_stocks = n(),
    .groups = "drop"
  ) %>% 
  ggplot(aes(x = year)) +
  geom_tile(aes(y = -4.5, fill = n_stocks, height = 0.8)) +
  scale_fill_viridis_c(option = "plasma", name = "Stocks (n)") +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
  geom_line(aes(y = median_val)) +
  facet_wrap(~ country, ncol = 6) +
  coord_cartesian(ylim = c(-5, 5)) + 
  custom_theme() +
  ylab("Z score") +
  xlab("Year") +
  ggtitle("Forage fish") +
  xlim(1950, max(final_dataset$year, na.rm = TRUE)) +
  scale_x_continuous(
    breaks = seq(1950, 2026, by = 17), 
    limits = c(1950, 2026, na.rm = TRUE)
  ); print(forage_trend_plot) 

other_fish_trend_plot = 
  final_dataset %>%
  group_by(country, year, taxa) %>%
  filter(taxa == "Fish") %>%
  filter(taxGroup != "forage fish") %>%
  summarise(
    median_val = median(z_score, na.rm = TRUE),
    lower = quantile(z_score, probs = 0.025, na.rm = TRUE),
    upper = quantile(z_score, probs = 0.975, na.rm = TRUE),
    n_stocks = n(),
    .groups = "drop"
  ) %>% 
  ggplot(aes(x = year)) +
  geom_tile(aes(y = -4.5, fill = n_stocks, height = 0.8)) +
  scale_fill_viridis_c(option = "plasma", name = "Stocks (n)") +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
  geom_line(aes(y = median_val)) +
  facet_wrap(~ country, ncol = 6) +
  coord_cartesian(ylim = c(-5, 5)) + 
  custom_theme() +
  ylab("Z score") +
  xlab("Year") +
  ggtitle("Other fish") +
  xlim(1950, max(final_dataset$year, na.rm = TRUE)) +
  scale_x_continuous(
    breaks = seq(1950, 2026, by = 17), 
    limits = c(1950, 2026, na.rm = TRUE)
  );print(other_fish_trend_plot)

crust_trend_plot = 
  final_dataset %>%
  group_by(country, year, taxa) %>%
  filter(taxa == "Crustaceans") %>%
  summarise(
    median_val = median(z_score, na.rm = TRUE),
    lower = quantile(z_score, probs = 0.025, na.rm = TRUE),
    upper = quantile(z_score, probs = 0.975, na.rm = TRUE),
    n_stocks = n(),
    .groups = "drop"
  ) %>% 
  ggplot(aes(x = year)) +
  geom_tile(aes(y = -4.5, fill = n_stocks, height = 0.8)) +
  scale_fill_viridis_c(option = "plasma", name = "Stocks (n)") +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
  geom_line(aes(y = median_val)) +
  facet_wrap(~ country, ncol = 3) +
  coord_cartesian(ylim = c(-5, 5)) + 
  custom_theme() +
  ylab("Z score") +
  xlab("Year") +
  ggtitle("Crustaceans") +
  xlim(1950, max(final_dataset$year, na.rm = TRUE)) +
  scale_x_continuous(
    breaks = seq(1950, 2026, by = 17), 
    limits = c(1950, 2026, na.rm = TRUE)
  );print(crust_trend_plot)


long_dat = 
  final_dataset %>%
    mutate(
      taxGroup = case_when(
        taxGroup == "forage fish" ~ "forage fish",
        taxa == "Fish" & taxGroup != "forage fish" ~ "other fish",
        TRUE ~ taxGroup
      )
    ) %>%
  group_by(year, taxGroup) %>%
  mutate(
    # create a new variable that counts the number of stocks in each year and taxGroup combination
    n_stocks = n()
  )

overall_trend_plot = 
  final_dataset %>%
  mutate(
    taxGroup = case_when(
      taxGroup == "forage fish" ~ "forage fish",
      taxa == "Fish" & taxGroup != "forage fish" ~ "other fish",
      TRUE ~ taxGroup
    )
  ) %>%
  group_by(year, taxGroup) %>%
  summarise(
    median_val = median(z_score, na.rm = TRUE),
    lower = quantile(z_score, probs = 0.025, na.rm = TRUE),
    upper = quantile(z_score, probs = 0.975, na.rm = TRUE),
    n_stocks = n(),
    .groups = "drop"
  ) %>% 
  ggplot(aes(x = year)) +
  geom_boxplot(
    data = long_dat, 
    aes(x = year, y = z_score, group = interaction(year, taxGroup), fill = n_stocks), 
    outlier.shape = NA, 
    alpha = 0.5,
    color = NA,
    width = 1
  ) +
  scale_fill_viridis_c(option = "plasma", name = "Stocks (n)") +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.06, color = "black", linewidth = 0.05) +
  geom_line(aes(y = median_val)) +
  facet_wrap(~ taxGroup) +
  coord_cartesian(ylim = c(-5, 5)) + 
  custom_theme() +
  ylab("Z score") +
  xlab("Year") +
  xlim(1950, max(final_dataset$year, na.rm = TRUE)) +
  scale_x_continuous(
    breaks = seq(1950, 2026, by = 17), 
    limits = c(1950, 2026, na.rm = TRUE)
  );print(overall_trend_plot)



# save all figures
fig_dir = here::here("res")
ggsave(filename = file.path(fig_dir, "forage_fish_trend_plot.png"), plot = forage_trend_plot, width = 7, height = 8)
ggsave(filename = file.path(fig_dir, "other_fish_trend_plot.png"), plot = other_fish_trend_plot, width = 9, height = 9)
ggsave(filename = file.path(fig_dir, "crust_trend_plot.png"), plot = crust_trend_plot, width = 7, height = 6)
# also pdf
ggsave(filename = file.path(fig_dir, "forage_fish_trend_plot.pdf"), plot = forage_trend_plot, width = 7, height = 8)
ggsave(filename = file.path(fig_dir, "other_fish_trend_plot.pdf"), plot = other_fish_trend_plot, width = 9, height = 9)
ggsave(filename = file.path(fig_dir, "crust_trend_plot.pdf"), plot = crust_trend_plot, width = 7, height = 6)

# store the final dataset as .csv
write_csv(final_dataset, file.path(here::here(), "data", "final_dataset.csv"))


























