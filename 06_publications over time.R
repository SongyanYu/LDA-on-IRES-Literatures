#---
# script to display the number of publications over time
# author: Songyan Yu
# Date created: 29/07/2022
#---

library(dplyr)
library(ggplot2)
library(viridis)   

read.csv('../../R output/05_TopicDocsGamma.csv') %>%
  rowwise() %>%
  mutate(max.gamma = max(c_across(X1:X13)),
         topic = as.character(match(max.gamma, c_across(X1:X13)))) %>%
  group_by(Year, topic) %>%
  summarise(n = n()) %>%
  filter(Year <= 2021) %>%  # remove publications in 2022
  mutate(topic = factor(topic, levels = c('1', '2', '3', '4', '5', '6', '7', '8', '9', '10', '11', '12', '13'))) %>%
  ggplot(aes(x = Year, y = n, fill = topic)) +
  geom_bar(stat = 'identity', position = 'stack') +
  theme_bw() +
  ylab('Number of publications') +
  labs(fill = 'Topic') +
  scale_fill_viridis(discrete = TRUE) +
  theme(legend.position = c(0.1, 0.55))
ggsave(filename = '../../Fig/06_publication over time.png',
       width = 8, height = 5)


