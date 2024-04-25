
#timeseries
timeseries_plot_1 <- data_join %>%
  ggplot() +
  aes(time_period, obs_value, group = country, color = Continent )+
  geom_line()

ggplotly(timeseries_plot_1)
