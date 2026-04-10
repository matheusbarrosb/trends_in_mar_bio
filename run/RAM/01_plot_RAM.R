require(dplyr)

# Read-in files ----------------------------------------------------------------
taxonomy = read.csv(here::here("Data/RAM/taxonomy.csv"))
ts       = read.csv(here::here("Data/RAM/ts.csv"))
stock    = read.csv(here::here("Data/RAM/stock.csv"))

stock2 = data.frame(stock$stockid, stock$scientificname, stock$region)
names(stock2) = c("stockid", "scientificname", "region")

# Get list of forage fish from taxonomy database -------------------------------
taxonomy = taxonomy %>% filter(FisheryType == "Forage Fish") # 44 forage fish stocks
# taxonomy = data.frame(taxonomy$scientificname); names(taxonomy) = c("scientificname")

# Filter time series data ------------------------------------------------------
ts = ts %>%
  left_join(stock2, by = "stockid") %>%
  filter(scientificname %in% taxonomy$scientificname)
  
path = file.path(here::here(), "Data", "RAM", "RAM_processed.csv")
write.csv(ts, file = path)

# Plot -------------------------------------------------------------------------

ts %>%
  group_by(scientificname, tsyear, region) %>%
  summarise(mean = mean(tsvalue, na.rm = TRUE),
            sd   = sd(tsvalue, na.rm = TRUE),
            se   = sd/sqrt(n())) %>%
  ggplot(aes(x = tsyear, y = mean)) +
  geom_line(aes(color = region)) +
  geom_ribbon(aes(x = tsyear, ymin = mean - se, ymax = mean + se), alpha = 0.2) +
  facet_wrap(~scientificname, scales = "free_y") +
  theme_minimal() +
  theme(legend.position = "top",
        legend.title = element_blank(),
        strip.text.x = element_text(face = "italic")) +
  xlab("Year") + ylab("Index")


region_df = ts %>%
  group_by(region, scientificname) %>%
  summarise(n = n())

region_df$frac = region_df$n/sum(region_df$n)

region_df %>%
  ggplot(aes(x = region, y = n)) +
  geom_bar(stat = "identity", color = "black", fill = "black") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  xlab("Region") + ylab("# of observations") +
  coord_flip()
  



