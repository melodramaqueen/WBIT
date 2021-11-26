library(tidyverse)
library(lubridate)
library(ggplot2)
library(magrittr)
library(dplyr)

trends <- read_csv("multiTimeline.csv", skip = 3, col_names = c("yyyy_mm", "good", "bad")) %>% 
  separate(yyyy_mm, into = c("year", "month"), sep = "-")

trends %<>% 
  group_by(year) %>% 
  summarise(good = mean(good),
         bad = mean(bad)) %>% 
  pivot_longer(!year, names_to = "social_media_is", values_to = "trend_value") 

trends %>%  
  ggplot() +
  geom_line(aes(x = year, y = trend_value, group = social_media_is, color = social_media_is), show.legend = FALSE) +
  labs(title = "Comparing Search Volumes", subtitle = "Social Media is Good vs Social Media is Bad", x = "Search Volume (averaged over months)", y = "Year") +
  ggrepel::geom_label_repel(data = filter(trends, year == "2018"), mapping = aes(year, trend_value, color = social_media_is, label = c("social media is good", "social media is bad")), show.legend = FALSE) +
  theme(plot.title = element_text(size = 12, colour = "black", hjust = 0.5), plot.subtitle = element_text(size = 9, colour = "black", hjust = 0.5), axis.title.x = element_text(size = 9, colour = "black", hjust = 0.5), axis.title.y = element_text(size = 9, colour = "black", hjust = 0.5))