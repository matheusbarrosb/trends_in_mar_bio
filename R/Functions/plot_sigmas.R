plot_sigmas = function(fit, legend_coord) {
  
  sigma_process = fit$draws(variables =  "sigma_process") %>%
    as.data.frame() %>%
  pivot_longer(cols = c(1:3))
  
  median_sigma_process = median(sigma_process$value, na.rm = TRUE)
  
  sigma_process_plot = sigma_process %>%
    ggplot() +
    geom_histogram(aes(x = value), color = "black", fill = "grey90", linewidth = 0.8) +
    geom_vline(aes(xintercept = median_sigma_process, linetype = "Median", color = "Median"), linewidth = 0.8) +
    scale_color_manual(name = "", values = c("Median" = "red")) +
    scale_linetype_manual(name = "", values = c("Median" = "dashed")) +
    custom_theme() +
    theme(
      legend.position = legend_coord,  
      legend.background = element_rect(fill = alpha('white', 0))
    ) +
    ylab("") +
    ggtitle(expression(sigma[process]))
  
  sigma_obs = fit$draws(variables =  "sigma_obs") %>%
    as.data.frame() %>%
    pivot_longer(cols = c(1:3))
  
  median_sigma_obs = median(sigma_obs$value, na.rm = TRUE)
  
  sigma_obs_plot = sigma_obs %>%
    ggplot() +
    geom_histogram(aes(x = value), color = "black", fill = "grey90", linewidth = 0.8) +
    geom_vline(xintercept = median_sigma_obs, linetype = "dashed", color = "red", linewidth = 0.8) +
    custom_theme()  +
    theme(legend.position = "NA") +
    ylab("Samples") +
    ggtitle(expression(sigma[obs]))

  ggpubr::ggarrange(sigma_obs_plot, sigma_process_plot)
     
}
