# read-in data ---------------------------------------------------------------
clean_data = read.csv(here::here("Data/PELGAS/PELGAS_clean.csv"))

# plotting -------------------------------------------------------------------
require(ggplot2)
clean_data %>%
  ggplot(aes(x = as.numeric(year), y = as.numeric(wbiom))) +
  geom_point() +
  geom_line() +
  facet_wrap(~spname, scales = "free_y") +
  theme_minimal() +
  xlab("Year") +
  ylab("Biomass (mt)")

# export plot ----------------------------------------------------------------
ggsave(here::here("Plots/PELGAS/pelgas_multipanel.pdf"), width = 8, height = 5)
