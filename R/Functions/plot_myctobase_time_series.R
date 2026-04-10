plot_myctobase_time_series = function(df) {

  plot = df %>%
    ggplot(aes(x = year, y = n_m3)) +
    geom_line() +
    geom_point() +
    geom_ribbon(aes(ymin = n_m3 - se, ymax = n_m3 + se), alpha = 0.2) +
    theme_minimal() +
    xlab("Year") +
    ylab("CPUE")
  
  return(plot)  
}


