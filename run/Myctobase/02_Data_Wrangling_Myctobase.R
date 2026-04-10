# read-in data -----------------------------------------------------------------
merged_data = read.csv(here::here("Data/Myctobase/myctobase_merged.csv"))

# data wrangling ---------------------------------------------------------------
require(dplyr)

clean_data = merged_data %>%
  group_by(
    year, 
    scientificName
    ) %>%
  summarise(
    n_m3 = mean(organismQuantity, na.rm = TRUE),
    sd   = sd(organismQuantity, na.rm = TRUE),
    se   = sd / sqrt(n())
    )

# export data ------------------------------------------------------------------
write.csv(clean_data, here::here("Data/Myctobase/myctobase_clean.csv"), row.names = FALSE)
