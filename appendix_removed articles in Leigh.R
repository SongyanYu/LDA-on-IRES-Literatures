library(dplyr)
library(tidyr)
library(ggplot2)

papers<-read.csv("../../data/Literatures in Leigh et al 2016.csv",skip = 1)

papers.abs<-papers[!(papers$Abstract==""),]

table(papers.abs$Associated.search.es.)

df <- data.frame(Assessment = (329+1+65+45+2+1+66+7+12)/1174,
                 Biogeochemistry = (1+200+65+3+2+32+7+1)/1174,
                 Fish = (174+45+3+2+21+12+1)/1174,
                 Invertebrate = (1+215+66+32+7+21+12+1)/1174) %>%
  pivot_longer(cols = c(Assessment, Biogeochemistry, Fish, Invertebrate), values_to = 'retained')

#df <- data.frame(Assessment = 11/62,
#                 Biogeochemistry = (9+3)/62,
#                 Fish = (16+1)/62,
#                 Invertebrate = (22+3+1)/62) %>%
#  pivot_longer(cols = c(Assessment, Biogeochemistry, Fish, Invertebrate), values_to = 'removed')

table(papers$Associated.search.es.)
df.total <- data.frame(Assessment = (340+1+65+45+2+1+66+7+12)/1236,
                       Biogeochemistry = (1+209+65+3+2+35+7+1)/1236,
                       Fish = (190+45+3+2+22+12+1)/1236,
                       Invertebrate = (1+237+66+35+7+22+12+1)/1236) %>%
  pivot_longer(cols = c(Assessment, Biogeochemistry, Fish, Invertebrate), values_to = 'all')

df %>%
  left_join(., df.total, by = 'name') %>%
  pivot_longer(cols = -name, names_to = 'group', values_to = 'prop') %>%
  ggplot() +
  geom_bar(aes(x = name, y = prop, fill = group), stat = 'identity', position = 'dodge') +
  theme_bw() +
  xlab('Topics') +
  ylab('Proportion') +
  theme(legend.position = 'top')
ggsave(filename = '../../Fig/appendix_retained artiles in Leigh.png',
       width = 5, height = 3)
