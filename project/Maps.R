install.packages("tidyverse")
install.packages("plotly")
install.packages("dplyr")
install.packages("knitr")
library(tidyverse)
library(plotly)
library(dplyr)
library(knitr)
#Joining the tables:
unicef_indicator_1 <- read_csv("unicef_indicator_1.csv")
unicef_metadata_all <- read_csv("unicef_metadata.csv")
countries_continents <- read_csv("Countries-Continents.csv")#Just a data set that matches countries and continents.


#data_join_function_continents <- full_join(unicef_indicator_1 , countries_continents,  by = c("country" = "Country"))
#data_join <- full_join(data_join_function_continents , unicef_metadata)


unicef_metadata <-  filter(unicef_metadata_all, year >= 2000)

colnames(unicef_metadata)[10] <- "life_expectancy" #Changing the name to something a little easier to use
colnames(unicef_metadata)[6] <- "population"
colnames(unicef_indicator_1)[6] <- "year" #make the names in both tables the same



data_join <- full_join(unicef_indicator_1, unicef_metadata)


#Map of access to basic sanitation, 2000 and 2021

#map_data_join <- full_join(unicef_indicator_1, map_world, by = c("country" = "region"))
#
#ggplot(map_data_join) +
#  aes(x = long, y = lat, group = group, fill = obs_value) +
#  geom_polygon()

unicef_indicator_1_2000 <-  unicef_indicator_1 %>%
  filter(year == 2000)

map_data_join_2000 <- full_join(unicef_indicator_1_2000 , map_world, by = c("country" = "region"))

ggplot(map_data_join_2000) +
  aes(x = long, y = lat, group = group, fill = obs_value) +
  geom_polygon()+
  labs(fill = "%",
       title = "Global Access to Basic Sanitation in 2000")

unicef_indicator_1_2021 <-  unicef_indicator_1 %>%
  filter(year == 2021)

map_data_join_2021 <- full_join(unicef_indicator_1_2021 , map_world, by = c("country" = "region"))

ggplot(map_data_join_2021) +
  aes(x = long, y = lat, group = group, fill = obs_value) +
  geom_polygon()+
  labs(fill = "%",
     title = "Global Access to Basic Sanitation in 2021")


data_join_continents <- full_join(data_join, countries_continents, by = c("country" = "Country"))

DJ_Africa <- filter(data_join_continents , Continent == "Africa")


DJ_Africa_2021 <-  DJ_Africa %>%
  filter(year == 2021)

map_africa_data_join_2021 <- full_join(DJ_Africa_2021 , map_world, by = c("country" = "region"))

ggplot(map_africa_data_join_2021) +
  aes(x = long, y = lat, group = group, fill = obs_value) +
  geom_polygon()+
  scale_fill_gradient(high="green",low = "red", na.value = "grey")+
  labs(fill = "%",
       title = "Access to Basic Sanitation in Africa 2021",
       x = "Longitude",
       y = "Latitude"
  ) +
  theme_classic()+
  coord_cartesian(xlim = c(-20, 60), ylim = c(-40, 40))  # Zoom in on Africa



ggplot(map_africa_data_join_2021) +
  aes(x = long, y = lat, group = group, fill = life_expectancy) +
  geom_polygon()+
  scale_fill_gradient(high="green",low = "red", na.value = "grey")+
  labs(fill = "%",
       title = "Life Expectancy in Africa 2021",
       x = "Longitude",
       y = "Latitude"
  ) +
  theme_classic()+
  coord_cartesian(xlim = c(-20, 60), ylim = c(-40, 40))  # Zoom in on Africa

#timeseries

timeseries_plot_1 <- data_join_continents %>%
  ggplot() +
  aes(year, obs_value, group = country, color = Continent )+
  geom_line(size=1.3, alpha = .6)+
  labs(
    x = "Year",
    y = "Access to Basic Sanitation",
    title = "Access to Basic Sanitation in the World"
  )+
  theme_bw()


ggplotly(timeseries_plot_1)


timeseries_plot_2 <- DJ_Africa %>%
  ggplot() +
  aes(year, obs_value, group = country, color = country )+
  geom_line()+
  labs(
  x = "Year",
  y = "Access to Basic Sanitation",
  title = "Access to Basic Sanitation in Africa"
)+
  theme_light()

ggplotly(timeseries_plot_2)



DJ_ethiopia <- filter(data_join, country == "Ethiopia")

timeseries_plot_2 <- DJ_ethiopia %>%
  ggplot() +
  aes(year, obs_value, color = country )+
  geom_line()+
  labs(
    x = "Year",
    y = "Access to Basic Sanitation",
    title = "Access to Basic Sanitation in Ethiopia"
  )+
  guides(color = "none", country = "none")+
  theme_light()


ggplotly(timeseries_plot_2)


timeseries_plot_3 <- DJ_ethiopia %>%
  ggplot() +
  aes(year, life_expectancy, color = country )+
  geom_line()+
  labs(
    x = "Year",
    y = "Life Expectancy",
    title = "Life Expectancy in Ethiopia"
  )+
  guides(color = "none", country = "none")+
  theme_light()

ggplotly(timeseries_plot_3)




#scatterplot




average_pop <- DJ_Africa %>%  #I want to only have the largest countries in africa,
  group_by(country) %>%
  mutate(avg_population = mean(population)) #so I make a new data frame with average population appended. 
                                            #average population is meaningless for data analatics, Im just using it temporarily

largest_countries <- average_pop %>%  
  filter(avg_population > 10000000) #I filter out the small countries


ggplot(largest_countries ) +
  aes(obs_value, life_expectancy, color = country, group = country, year = 2000) +
  geom_point()+
  labs(
    x = "Access to Basic Sanitation",
    y = "Life Expectancy",
    title = "Access to Basic Sanitation and Life Expectancy Scatter Plot"
  )+
  guides(color = "none", country = "none")+
  theme_light()


#bar chart

data_join_continents %>%
  group_by(Continent, year) %>%
  summarise(m_lifeexp = mean(life_expectancy, na.rm = TRUE)) %>%
    
    ggplot()+
    aes(Continent, m_lifeexp, fill = Continent) +
    geom_col()
