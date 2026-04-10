# read-in data -----------------------------------------------------------------

raw_data   = read.csv(here::here("Data/Myctobase/groupOccurrence.csv")) # individual occurrence records
event_data = read.csv(here::here("Data/Myctobase/event.csv")) # sampling metadata

# merging datasets -------------------------------------------------------------
# extract date from sampling metadata and add to individual occurrence records

require(dplyr)

merged_data      = left_join(raw_data, event_data, by = "eventID")
merged_data$date = as.Date(strsplit(merged_data$start_eventTime, " ")  %>% sapply("[[", 1), format = "%d/%m/%Y")
merged_data$year = as.numeric(format(merged_data$date, "%Y"))

# keep only species records
merged_data$is_species = ifelse(grepl('^\\w+\\s\\w+$', as.factor(merged_data$scientificName)), TRUE, FALSE)
merged_data            = merged_data[merged_data$is_species,]

# export data ------------------------------------------------------------------
write.csv(merged_data, here::here("Data/Myctobase/myctobase_merged.csv"), row.names = FALSE)
