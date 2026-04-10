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
data = ts %>%
  left_join(stock2, by = "stockid") %>%
  filter(scientificname %in% taxonomy$scientificname)

path = file.path(here::here(), "Data", "RAM", "RAM_processed.csv")
write.csv(path, file = path)

data = data %>%
  group_by(tsyear, region) %>%
  summarise(obs = sum(tsvalue, na.rm = TRUE),
            sd   = sd(tsvalue, na.rm = TRUE),
            se   = sd/sqrt(n())) 

data[data==0] = NA
data = na.exclude(data)

data %>%
  ggplot(aes(x = tsyear, y = obs)) +
  geom_line() +
  geom_errorbar(aes(ymin = obs - se, ymax = obs + se), width = 0.2) +
  custom_theme() +
  facet_wrap(~region, scales = "free_y")

names(data) = c("year", "region", "y", "sd", "se")

# Fit model --------------------------------------------------------------------
data_list = list(
  N            = length(unique(data$year)),
  S            = length(unique(data$region)),
  M            = length(unique(data$region)),
  y            = data$y,
  states       = 1:length(unique(data$region)),
  n_obsvar     = 1,
  proVariances = c(rep(1, length(unique(data$region))),0),
  obsVariances = rep(1, length(unique(data$region))),
  trends       = c(rep(1, length(unique(data$region))),0),
  est_trend    = 0,
  est_nu       = 1,
  family       = 1,
  n_provar     = 1,
  n_trends     = length(unique(data$region)),
  n_pos        = dim(data)[1],
  row_indx_pos = as.numeric(as.factor(data$region)),
  col_indx_pos = as.numeric(as.factor(data$year)),
  est_A        = c(0,0),
  n_A          = 0
)

# standardize y ---------------------------------------------------------------

Ny = data %>%
  group_by(region) %>%
  summarise(N = n_distinct(year))
Ny = as.vector(Ny[,2])

split_y = split_at_intervals(log(data_list$y + 0.000001), intervals = Ny$N)

means = rep(NA, data_list$S)
for (s in 1:data_list$S) means[s] = mean(split_y[[s]], na.rm = TRUE)
for (s in 1:data_list$S) split_y[[s]] = as.numeric(split_y[[s]])/means[s]

data_list$y = unlist(split_y)

# run model --------------------------------------------------------------------
mcmc_list = list(n_mcmc      = 3000,
                 n_burn      = 1000,
                 n_chain     = 3,
                 n_thin      = 1,
                 step_size   = 0.4,
                 adapt_delta = 0.9)

model_directory = file.path(here::here(), "MARSS", "MARSS.stan")
model_file      = cmdstan_model(model_directory)

fit = model_file$sample(
  data            = data_list,
  seed            = 2025,
  chains          = mcmc_list$n_chain,
  parallel_chains = mcmc_list$n_chain,
  iter_warmup     = mcmc_list$n_burn,
  iter_sampling   = mcmc_list$n_mcmc,
  adapt_delta     = 0.97,
  step_size       = 0.05,
  refresh         = 100,
  max_treedepth   = 20
)

# Output processing ----------------------------------------------------------
species_list = as.vector(unique(data$region))

preds = fit$summary(variables = "pred")

# Keep only observed years ---------------------------------------------------
year_index = t(sapply(preds$variable, extract_indices))[,1]
spp_index  = t(sapply(preds$variable, extract_indices))[,2]

preds$year_index = as.numeric(year_index)
preds$spp_index = as.numeric(spp_index)

years_to_keep = as.numeric(data_list$col_indx_pos)
spps_to_keep = as.numeric(data_list$row_indx_pos)

keep_df = data.frame(
  spp_index = spps_to_keep,
  year_index = years_to_keep
)

filtered_data = preds %>%
  inner_join(keep_df, by = c("spp_index", "year_index"))

preds = filtered_data

#-----------------------------------------------------------------------------
Ny = data %>%
  group_by(region) %>%
  summarise(N = n_distinct(year))

Ny = as.vector(Ny[,2])

spps = list()
for (i in 1:data_list$S) spps[[i]] = rep(species_list[i], Ny$N[i])
spps = unlist(spps)

preds$spps = spps
preds$spps = sapply(preds$spps, capitalize_first_word)
preds$year = preds$year_index + 1929
preds$obs  = data_list$y

# Plot results ----------------------------------------------------------------
preds %>%
  ggplot() +
  geom_line(aes(x = year, y = median, color = "Estimated trend"),
            linetype = 1,
            linewidth = 1) +
  #  geom_line(aes(x = year, y = obs), color = "black") +
  geom_ribbon(aes(ymin = q5, ymax = q95, x = year), alpha = 0.2) +
  xlab("Year") +
  ylab("Scaled biomass index") +
  facet_wrap(~spps, scales = "free_y") +
  custom_theme() +
  theme(legend.title = element_blank(),
        legend.position = "top",
        strip.text.x = element_text(face = "italic"))

ggsave(here::here("Plots/Trends/RAM_trend.pdf"), width = 9, height = 4.5, dpi = 300)


