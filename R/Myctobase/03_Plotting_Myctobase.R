# read-in data ---------------------------------------------------------------
clean_data = read.csv(here::here("Data/Myctobase/myctobase_clean.csv"))

# plotting -------------------------------------------------------------------
split_data = split(clean_data, f = clean_data$scientificName)

filtered_split_data <- lapply(split_data, function(df) if(nrow(df) >= 10) df else NULL)
filtered_split_data <- Filter(Negate(is.null), filtered_split_data)

require(ggplot2)

source(here::here("R/Functions/plot_myctobase_time_series.R"))
plot_list = list()
for (i in 1:length(filtered_split_data)) {
  plot_list[[i]] = plot_myctobase_time_series(df = filtered_split_data[[i]])
}

names(plot_list) = names(filtered_split_data)
for(i in 1:length(filtered_split_data)) {
  ggsave(here::here("Plots/Myctobase/", paste0(names(filtered_split_data)[i], ".pdf")), plot_list[[i]], width = 10, height = 5, units = "in", dpi = 300)
}


# make a multipanel plot, it's only 23 species...!
kept_species = names(filtered_split_data)
clean_data %>%
  filter(scientificName %in% kept_species) %>%
  ggplot(aes(x = year, y = n_m3)) +
  geom_line() +
  geom_point() +
  theme_minimal() +
  facet_wrap(~scientificName, scales = "free_y") +
  ylab(bquote("CPUE"~(count/m^3))) +
  xlab("Year") +
  theme(strip.text = element_text(face = "italic"))

ggsave(here::here("Plots/Myctobase/myctobase_multipanel.pdf"), width = 12, height = 6, units = "in", dpi = 300)