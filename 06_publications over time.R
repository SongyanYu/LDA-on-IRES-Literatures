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



read.csv('../../R output/05_TopicDocsGamma.csv') %>%
  group_by(Year) %>%
  summarise(n = n()) %>%
  filter(Year <= 2021) %>%  # remove publications in 2022
  mutate(year_group = ifelse(Year <= 1975, '1966-1975', 
                           ifelse(Year <= 1980, '1976-1980', 
                                  ifelse(Year <= 1985, '1981-1985',
                                         ifelse(Year <= 1990, '1986-1990',
                                                ifelse(Year <= 1995, '1991-1995',
                                                       ifelse(Year <= 2000, '1996-2000',
                                                              ifelse(Year <= 2005, '2001-2005',
                                                                     ifelse(Year <= 2010, '2006-2010',
                                                                            ifelse(Year <= 2015, '2011-2015', '2016-2021')))))))))) %>%
  group_by(year_group) %>%
  summarise(n = sum(n)) %>%
  ggplot() +
  geom_bar(aes(x = year_group, y = n), stat = 'identity', position = 'stack', fill = '#ED7D31') +
  theme_bw() +
  ylab('Number of publications') +
  xlab('')
ggsave(filename = '../../Fig/06_publication_aggregate.png',
       width = 7.5, height = 4) 



read.csv('../../R output/05_TopicDocsGamma.csv') %>%
  rowwise() %>%
  mutate(max.gamma = max(c_across(X1:X13)),
         topic = as.character(match(max.gamma, c_across(X1:X13)))) %>%
  filter(Year <= 2021) %>%  # remove publications in 2022
  mutate(year_group = ifelse(Year <= 1975, '1966-1975', 
                             ifelse(Year <= 1980, '1976-1980', 
                                    ifelse(Year <= 1985, '1981-1985',
                                           ifelse(Year <= 1990, '1986-1990',
                                                  ifelse(Year <= 1995, '1991-1995',
                                                         ifelse(Year <= 2000, '1996-2000',
                                                                ifelse(Year <= 2005, '2001-2005',
                                                                       ifelse(Year <= 2010, '2006-2010',
                                                                              ifelse(Year <= 2015, '2011-2015', '2016-2021')))))))))) %>%
  group_by(year_group, topic) %>%
  summarise(n = n()) %>%
  mutate(topic = factor(topic, levels = c('1', '2', '3', '4', '5', '6', '7', '8', '9', '10', '11', '12', '13'))) %>%
  ggplot(aes(x = year_group, y = n, fill = topic)) +
  geom_bar(stat = 'identity', position = 'stack') +
  theme_bw() +
  ylab('Number of publications') +
  xlab('') +
  labs(fill = 'Topic') +
  scale_fill_viridis(discrete = TRUE) +
  theme(legend.position = c(0.1, 0.55))
ggsave(filename = '../../Fig/06_publication_aggregate_topic.png',
       width = 7.5, height = 5) 
