require(cmdstanr)
require(dplyr)

function_directory = file.path(here::here(), "R", "Functions/")
function_files     = list.files(function_directory)
for (i in 1:length(function_files)) source(paste0(function_directory, function_files[i]))

clean_data = read.csv(here::here("Data/Myctobase/myctobase_clean.csv"))

data = clean_data %>%
  group_by(year) %>%
  summarise(obs = sum(n_m3, na.rm = TRUE)) %>%
  na.exclude()

data_list = list(
  N            = length(unique(data$year)),
  S            = 1,
  M            = 1,
  y            = data$obs,
  states       = 1,
  n_obsvar     = 1,
  proVariances = c(1, 0),
  obsVariances = c(1),
  trends       = c(1, 0),
  est_trend    = 0,
  est_nu       = 1,
  family       = 1,
  n_provar     = 1,
  n_trends     = 1,
  n_pos        = dim(data)[1],
  row_indx_pos = rep(1, dim(data)[1]),
  col_indx_pos = as.numeric(as.factor(data$year)),
  est_A        = c(0,0),
  n_A          = 0
)


# run model
mcmc_list = list(n_mcmc      = 500,
                 n_burn      = 20,
                 n_chain     = 2,
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


preds = fit$summary(variables = "pred",  ~ quantile(.x, probs = c(0.5, 0.2, 0.8)))

preds$year = data$year
preds$obs = data$obs

names(preds) = c("par", "mean", "low", "upp", "year")

preds %>%
  ggplot() +
  geom_line(aes(year, y = mean, color = "Estimated trend"), size = 1) +
  geom_ribbon(aes(x = year, ymin = low, ymax = upp), alpha = 0.15) +
  custom_theme() +
  theme(strip.text.x = element_text(face = "italic"),
        legend.position = "top",
        legend.title = element_blank()) +
  ylab("Count/m3") +
  xlab("Year") +
  ylim(0,0.12) +
  ggtitle("Myctophid fish abundance in Antartica")
  

ggsave(here::here("Plots/Trends/Myctobase_trend.pdf"), width = 6, height = 4.5, dpi = 300)











