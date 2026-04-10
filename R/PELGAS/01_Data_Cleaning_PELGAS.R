require(dplyr)

# read-in data -----------------------------------------------------------------

raw_data = read.csv(here::here("Data/PELGAS/raw_data.csv"),sep = ";", dec = ",") 

# extract years ----------------------------------------------------------------

raw_data$year = as.numeric(substr(raw_data$cruise,7,10))

# keep species with 10 + data points only ---------------------------------------

raw_data = raw_data %>% 
  group_by(spname) %>% 
  filter(n() >= 10)

raw_data$wCV = as.numeric(sub(",", ".", sub(".", "", raw_data$wCV, fixed=TRUE), fixed=TRUE))

# export data ------------------------------------------------------------------
write.csv(raw_data, here::here("Data/PELGAS/pelgas_clean.csv"), row.names = FALSE)
