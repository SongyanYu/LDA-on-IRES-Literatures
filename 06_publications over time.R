#---
# script to display the number of publications over time
# author: Songyan Yu
# Date created: 29/07/2022
#---

#---
# 1. read in the second corpus
#---
papers<-readxl::read_xlsx("../../data/scopus_intermittent-streams_filtered.xlsx",
                          sheet = "Sheet1")

library(dplyr)
library(ggplot2)
papers %>%
  group_by(Year) %>%
  summarise(n = n()) %>%
  filter(Year <= 2021) %>%  # remove publications in 2022
  ggplot() +
  geom_line(aes(x = Year, y = n)) +
  theme_classic() +
  ylab('Number of publications')
