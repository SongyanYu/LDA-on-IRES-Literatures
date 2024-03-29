#---
# Develop LDA modelling for IRES literature
# literatures are updated to Sep 2021.
# Author: Songyan Yu
# Date created: 3/01/2022
#---

remove.packages("rlang")
install.packages("rlang")

#---
# 1. read in the second corpus
#---
papers<-readxl::read_xlsx("../../data/scopus_intermittent-streams_filtered.xlsx",
                          sheet = "Sheet1")

# remove articles without abstract
papers.abs<-papers[!(papers$Abstract==""),]

doc.info<-papers.abs[,c(1,2,3,4)]
doc.info$document<-c(1:nrow(doc.info))
doc.info$document<-as.character(doc.info$document)

doc.info$Abstract<-gsub(pattern = "\uA9.*",x=doc.info$Abstract,replacement = "") # remove "copyright" information
doc.info$Abstract<-gsub(pattern = "(Crown Copyright|Copyright).*",x=doc.info$Abstract,replacement = "") # same as above
doc.info$Abstract<-gsub(pattern = "\\(C\\) \\d.*",x=doc.info$Abstract,replacement = "") # same as above
doc.info$Abstract<-gsub(pattern = "\\[No abstract available\\]",x=doc.info$Abstract, replacement = "") # remove "no abstract"
doc.info$Abstract<-gsub(pattern = "from Authors",x=doc.info$Abstract, replacement = "") # remove duplicate abstract

text<-paste(papers.abs$Title,papers.abs$Abstract)
library(dplyr)
text.df<-tibble(line=1:length(text),text)

#---
# 2. tokenisation, remove stop words, numbers, punctuation, words with length < 3, and stem word.
#---
publisher.word<-c("wiley","elsevier","john","springer","blackwell","ltd","authors","author","taylor","francis",
                  "copyright","press","mdpi","licensee","basel","switzerland","sons") # to be removed

library(tidytext)
library(SnowballC) # stem word

#---
# 3. n-gram
#---
#unigram
text.1.gram <-
  text.df%>%
  unnest_tokens(word,text)%>%
  anti_join(stop_words)%>%                # remove stop words (e.g. and, with, etc.)
  filter(is.na(as.numeric(word)))%>%      # remove numbers
  filter(!word %in% publisher.word)%>%    # remove publisher words
  mutate(word=wordStem(word))%>%          # stem words
  filter(nchar(word)>2)%>%                # remove words with less than 3 letters (e.g. in)
  filter(!grepl("[[:digit:]]",word))%>%   # remove words with number in it (e.g. NH4)
  count(line,word)%>%
  filter(n>1)          # remove words that only be mentioned once in a paper's title and abstract.

# remove words occurring in > 400 documents
common.word <-
  text.1.gram%>%
  group_by(word)%>%
  summarise(n = n())%>%
  filter(n > 400)

text.1.gram.short <-
  text.1.gram %>%
  anti_join(common.word, by="word")

# remove words occurring in < 8 documents
rare.word <-
  text.1.gram.short %>%
  group_by(word) %>%
  summarise(n=n()) %>%
  filter(n < 8)

text.1.gram.short <-
  text.1.gram.short %>%
  anti_join(rare.word, by = "word")

# number of unique terms (n = 1271)
text.1.gram.short %>%
  group_by(word) %>%
  summarise(n = n())

length.text <-
  text.1.gram %>%
  group_by(line) %>%
  summarise(n = sum(n))

#bi-gram
library(tidyr)
text.2.gram <- 
  text.df %>%
  unnest_tokens(word, text, token = "ngrams", n=2) %>%
  separate(word, c("word1","word2"), sep=" ") %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  filter(is.na(as.numeric(word1))) %>%
  filter(is.na(as.numeric(word2))) %>%
  filter(!word1 %in% publisher.word) %>%
  filter(!word2 %in% publisher.word) %>%
  mutate(word1=wordStem(word1),word2=wordStem(word2)) %>%
  #  filter(!word1 %in% common.word$word)%>%
  #  filter(!word2 %in% common.word$word)%>%
  filter(nchar(word1)>2) %>%
  filter(nchar(word2)>2) %>%
  filter(!grepl("[[:digit:]]",word1)) %>%
  filter(!grepl("[[:digit:]]",word2)) %>%
  unite(bigram,word1,word2,sep="_") %>%
  count(line, bigram,sort=TRUE)

# retain only those bigrams that occur more than expected by chance under a significant value of p<.05
# Student's t test (degree of freedom?)
text.2.gram.short <-
  text.2.gram %>%
  separate(bigram, c("word1","word2"), sep="_") %>%
  left_join(., text.1.gram, by=c("line", "word1" = "word")) %>%
  left_join(., text.1.gram, by=c("line", "word2"="word")) %>%
  left_join(., length.text, by="line") %>%
  mutate(p_bi=n.x/n.y.y, 
         p1=n.y/n.y.y,
         p2=n.x.x/n.y.y,
         t=(p_bi-p1*p2)/sqrt(p_bi*(1-p_bi)/n.y.y)) %>%
  filter(t > 1.6) %>%
  unite(bigram, word1, word2, sep = "_") %>%
  dplyr::select(line, bigram, n=n.x)

# tri-gram
text.3.gram <- 
  text.df %>%
  unnest_tokens(word, text, token = "ngrams", n=3) %>%
  separate(word, c("word1", "word2", "word3"), sep=" ") %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  filter(!word3 %in% stop_words$word) %>%
  filter(is.na(as.numeric(word1))) %>%
  filter(is.na(as.numeric(word2))) %>%
  filter(is.na(as.numeric(word3))) %>%
  filter(!word1 %in% publisher.word) %>%
  filter(!word2 %in% publisher.word) %>%
  filter(!word3 %in% publisher.word) %>%
  mutate(word1=wordStem(word1), word2=wordStem(word2), word3=wordStem(word3)) %>%
  #  filter(!word1 %in% common.word$word)%>%
  #  filter(!word2 %in% common.word$word)%>%
  #  filter(!word3 %in% common.word$word)%>%
  filter(nchar(word1)>2) %>%
  filter(nchar(word2)>2) %>%
  filter(nchar(word3)>2) %>%
  filter(!grepl("[[:digit:]]", word1)) %>%
  filter(!grepl("[[:digit:]]", word2)) %>%
  filter(!grepl("[[:digit:]]", word3)) %>%
  unite(trigram, word1, word2, word3, sep="_") %>%
  count(line, trigram)

# remain only those trigrams that occur more than expected by chance under a significant value of p<.05
# Student's t test (degree of freedom?)
text.3.gram.short <-
  text.3.gram %>%
  separate(trigram, c("word1", "word2", "word3"), sep="_") %>%
  left_join(., text.1.gram, by = c("line", "word1" = "word")) %>%
  left_join(., text.1.gram, by = c("line", "word2" = "word")) %>%
  left_join(., text.1.gram, by = c("line", "word3" = "word")) %>%
  left_join(., length.text, by = "line") %>%
  mutate(p_tri=n.x/n,
         p1=n.y/n,
         p2=n.x.x/n,
         p3=n.y.y/n,
         t=(p_tri-p1*p2*p3)/sqrt(p_tri*(1-p_tri)/n))%>%
  filter(t > 1.4) %>%
  unite(trigram, word1, word2, word3, sep = "_") %>%
  dplyr::select(line, trigram, n=n.x)

#---
# 4. dtm and LDA modelling
#---
# combine uni-, bi- and tri-grams together to form a dtm
colnames(text.1.gram.short)[2] <- "term"
colnames(text.2.gram.short)[2] <- "term"
colnames(text.3.gram.short)[2] <- "term"

all.term<-rbind(text.1.gram.short,
                text.2.gram.short,
                text.3.gram.short)

length(unique(all.term$term))
sum(all.term$n)

#library(tm)
all.dtm <- 
  all.term %>%
  cast_dtm(document = line, term = term, value = n)

tm::inspect(all.dtm)

# select the number of topics for LDA (n=2~50)
library(ldatuning)
seed.findtopicnumber <- 1
result <- 
  FindTopicsNumber(all.dtm,
                   topics = seq(from=2,to=30,by=1),
                   metrics = c("Deveaud2014"),
                   method = "Gibbs",
                   mc.cores = 3,
                   verbose = TRUE,
                   control = list(seed = seed.findtopicnumber))

png(filename = "../../Fig/05_OptimalTopicNumber.png")
FindTopicsNumber_plot(result)  # n=13 topics looks  optimal.
dev.off()

# LDA modelling
library(topicmodels)

n.topic <-13
ires.lda <- LDA(all.dtm,
             k=n.topic,
             method = "Gibbs",
             control = list(seed = 13))

# word-topic probabilities
ires.topics <- tidy(ires.lda, matrix="beta")

library(ggplot2)
ires.top.terms <-
  ires.topics %>%
  group_by(topic) %>%
  top_n(20, beta) %>%  # can be changed to 20
  ungroup() %>%
  arrange(topic, -beta)

top_topic_words <-
  ires.top.terms %>%
  group_by(topic) %>%
  summarise(Top_topic_words = paste(term, collapse = ", "))
write.csv(top_topic_words, paste0("../../R output/05_TopTopicWords_n",n.topic,".csv"), row.names = FALSE)


ires.top.terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~topic, scales = "free", ncol = 3) +
  coord_flip() +
  scale_x_reordered() +
  labs(y = "probability")
ggsave(paste0("../../Fig/05_topic-term probabilities_n",n.topic,".png"), width=8, height = 10)

#---
# topic similarity
#---
library(reshape2)
weight.matrix <- as.data.frame(acast(ires.topics, topic~term))

# Bray-Curtis distance (dissimilarity)
library(vegan)
topic.dis <- vegdist(weight.matrix, method = "bray")

# non-metric multidimentional scaling. Note final value is better <20
library(MASS)
fits <- isoMDS(topic.dis, k=3)

library(ggplot2)
similarity.df <- as.data.frame(fits$points)
similarity.df$topic <- c(1:n.topic)

# document-topic probabilities
ires.documents <- tidy(ires.lda, matrix="gamma")

ires.documents.wide <-
  ires.documents %>%
  pivot_wider(id_cols = document, names_from = topic, values_from = gamma) %>%
  left_join(., doc.info, by="document")

write.csv(ires.documents.wide,
          file = "../../R output/05_TopicDocsGamma.csv",
          row.names = FALSE)

relevant.docs <- 
  ires.documents %>%
  group_by(topic) %>%
  top_n(20,gamma) %>%
  ungroup() %>%
  arrange(topic, -gamma)

relevant.docs <-
  relevant.docs %>%
  left_join(doc.info, by="document")

write.csv(relevant.docs,
          file = paste0("../../R output/05_TopicDoc_Top20.csv"),
          row.names = FALSE)

topic.size <- 
  ires.documents %>%
  group_by(document) %>%
  summarise(dom.topic = match(max(gamma), gamma)) %>%
  group_by(dom.topic) %>%
  summarise(n = n())

similarity.df %>%
  left_join(topic.size, by = c("topic" = "dom.topic")) %>%
  ggplot(aes(x=V2, y=V3, size=n)) +
  geom_point(alpha = 0.5, color = "black", shape = 21) +
  geom_text(aes(label = topic), size=4) +
  scale_size(range = c(5, 20), name = paste0("Frequency of dominance","\n","              (articles)"))+
  xlab("NMDS1") +
  ylab("NMDS2") +
  theme_classic() +
  theme(legend.position = 'bottom')
ggsave(filename = paste0("../../Fig/05_TopicSimilarity.png"),width = 6,height = 5)

#---
# topic-term-beta
#---
ires.top.terms <-
  ires.topics %>%
  group_by(topic) %>%
  top_n(100,beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

write.csv(ires.top.terms,file = "../../R output/05_TopicTermBeta-Top100.csv",row.names = FALSE)

#---
# topic popularity
#---
pub.by.year <- 
  doc.info %>%
  group_by(Year) %>%
  summarise(n = n())

# document-topic probabilities
ires.documents <- tidy(ires.lda, matrix="gamma")

text.df$year = doc.info$Year
text.df$line <- as.character(text.df$line)

topic.pop <- 
  ires.documents %>%
  left_join(., text.df, by=c("document" = "line")) %>%
  group_by(year, topic) %>%
  summarise(total.gamma = sum(gamma), n.doc=n())

library(tibble)

# aggregate 1966-1976
topic.pop.1966_1976 <- 
#  topic.pop[c(1:117),] %>%
  topic.pop[c(1:130),] %>%
  group_by(topic) %>%
  summarise(gamma = sum(total.gamma), n=sum(n.doc), prop = gamma/n) %>%
  add_column(year = "1966-1976") %>%
  dplyr::select(year, topic, prop)

topic.pop.1977_1981 <-
  topic.pop[c(131:208),]%>%
  group_by(topic)%>%
  summarise(gamma=sum(total.gamma),n=sum(n.doc),prop=gamma/n)%>%
  add_column(year="1977-1981")%>%
  dplyr::select(year,topic,prop)

topic.pop.1982_1986 <-
  topic.pop[c(209:260),]%>%
  group_by(topic)%>%
  summarise(gamma=sum(total.gamma),n=sum(n.doc),prop=gamma/n)%>%
  add_column(year="1982-1986")%>%
  dplyr::select(year,topic,prop)

topic.pop.1987_1991 <-
  topic.pop[c(261:325),]%>%
  group_by(topic)%>%
  summarise(gamma=sum(total.gamma),n=sum(n.doc),prop=gamma/n)%>%
  add_column(year="1987-1991")%>%
  dplyr::select(year,topic,prop)

topic.pop.1992_1996 <-
  topic.pop[c(326:390),]%>%
  group_by(topic)%>%
  summarise(gamma=sum(total.gamma),n=sum(n.doc),prop=gamma/n)%>%
  add_column(year="1992-1996")%>%
  dplyr::select(year,topic,prop)

topic.pop.1997_2001 <-
  topic.pop[c(391:455),]%>%
  group_by(topic)%>%
  summarise(gamma=sum(total.gamma),n=sum(n.doc),prop=gamma/n)%>%
  add_column(year="1997-2001")%>%
  dplyr::select(year,topic,prop)

topic.pop.2002_2006 <-
  topic.pop[c(456:520),]%>%
  group_by(topic)%>%
  summarise(gamma=sum(total.gamma),n=sum(n.doc),prop=gamma/n)%>%
  add_column(year="2002-2006")%>%
  dplyr::select(year,topic,prop)

topic.pop.2007_2011 <-
  topic.pop[c(521:585),]%>%
  group_by(topic)%>%
  summarise(gamma=sum(total.gamma),n=sum(n.doc),prop=gamma/n)%>%
  add_column(year="2007-2011")%>%
  dplyr::select(year,topic,prop)

topic.pop.2012_2016 <-
  topic.pop[c(586:650),]%>%
  group_by(topic)%>%
  summarise(gamma=sum(total.gamma),n=sum(n.doc),prop=gamma/n)%>%
  add_column(year="2012-2016")%>%
  dplyr::select(year,topic,prop)

topic.pop.2017_2021 <-
  topic.pop[c(651:728),]%>%
  group_by(topic)%>%
  summarise(gamma=sum(total.gamma),n=sum(n.doc),prop=gamma/n)%>%
  add_column(year="2017-2021")%>%
  dplyr::select(year,topic,prop)

topic.pop.all<-bind_rows(topic.pop.1966_1976,
                         topic.pop.1977_1981,
                         topic.pop.1982_1986,
                         topic.pop.1987_1991,
                         topic.pop.1992_1996,
                         topic.pop.1997_2001,
                         topic.pop.2002_2006,
                         topic.pop.2007_2011,
                         topic.pop.2012_2016,
                         topic.pop.2017_2021)

topic.name <- readxl::read_xlsx("../../data/Topic names_all literature.xlsx",
                              sheet = "Sheet1")
topic.pop.all <- left_join(topic.pop.all, topic.name,by = c("topic" = "Topic"))

topic.avg.prop <-
  topic.pop.all %>%
  group_by(topic,Topic.name) %>%
  summarise(avg.prop.change = mean(diff(prop))*100,
            se = sd(prop)/sqrt(n())) %>%
  arrange(avg.prop.change) %>%
  mutate(group = ifelse(avg.prop.change > 0.1, "Hot", ifelse(avg.prop.change < (-0.1), "Cold", "Neutral")))

# average change in prevalence (figure)
topic.avg.prop %>%
  ggplot(aes(x = factor(topic, levels = dput(as.character(topic))),
             y = avg.prop.change,
             color = avg.prop.change,
             label = Topic.name)) +
  geom_hline(yintercept = 0,
             linetype = "dotted",
             size = 1.5) +
  geom_point(show.legend = FALSE) +
  geom_pointrange(aes(ymin = avg.prop.change - se * 100,
                      ymax = avg.prop.change + se * 100),
                  fatten = 5, size = 1.2,
                  show.legend = FALSE) +
  scale_color_gradient2(low = "blue", high = "red", mid = "grey",
                        name = "Average change in popularity") +
  geom_text(aes(y = avg.prop.change-2),
            size = 5,
            hjust = 0.,
            show.legend = FALSE) +
  coord_flip() +
  ylab("Average change in popularity (%)") +
  theme_classic() +
  theme(axis.line.y = element_blank(), axis.ticks.y = element_blank(),
        axis.text.y = element_blank(), axis.title.y = element_blank()) +
  scale_y_continuous(position = "right")
ggsave(filename = paste0("../../Fig/05_TopicPopularity_n",n.topic,".png"), width = 10, height = 5.73)


# topic trend over time (figure)
topic.pop.all %>%
  left_join(topic.avg.prop[,c(1,3,5)], by = "topic") %>%
  ggplot(aes(x = year,y = prop * 100, group = Topic.name)) +
  geom_line(aes(color = avg.prop.change),size = 1.2) +
  scale_color_gradient2(low = "blue", high = "red", mid = "grey",
                        name = "Average change in prevalence") +
  facet_grid(factor(group,levels = c("Hot", "Neutral", "Cold"))~.) +
  theme_classic() +
  theme(panel.grid.major.y = element_line(linetype = "dotted", color = "grey")) +
  xlab("") +
  ylab("Topic prevalence (%)")
ggsave(filename = paste0("../../Fig/05_TopicTrend_n", n.topic, ".png"), width = 10, height = 5.73)

#---
# topic generality/specificity
#---

topic.weight <- 
  ires.documents.wide %>%
  rename(tp01 = "1", tp02 = "2",tp03 = "3", tp04 = "4", tp05 = "5", 
         tp06 = "6", tp07 = "7", tp08 = "8", tp09 = "9", tp10 = "10",
         tp11 = "11", tp12 = "12", tp13 = "13") %>%
  mutate(max = pmax(tp01, tp02, tp03, tp04, tp05, tp06, tp07, tp08, tp09, tp10, tp11, tp12, tp13),
         slec01 = ifelse(tp01 < max, 0, 1), slec02 = ifelse(tp02 < max, 0, 1),
         slec03 = ifelse(tp03 < max, 0, 1), slec04 = ifelse(tp04 < max, 0, 1),
         slec05 = ifelse(tp05 < max, 0, 1), slec06 = ifelse(tp06 < max, 0, 1),
         slec07 = ifelse(tp07 < max, 0, 1), slec08 = ifelse(tp08 < max, 0, 1),
         slec09 = ifelse(tp09 < max, 0, 1), slec10 = ifelse(tp10 < max, 0, 1),
         slec11 = ifelse(tp11 < max, 0, 1), slec12 = ifelse(tp12 < max, 0, 1),
         slec13 = ifelse(tp13 < max, 0, 1)) %>%
  transmute(tp01_slec_weight = mean(tp01[slec01 ==1]),
            tp02_slec_weight = mean(tp02[slec02 ==1]),
            tp03_slec_weight = mean(tp03[slec03 ==1]),
            tp04_slec_weight = mean(tp04[slec04 ==1]),
            tp05_slec_weight = mean(tp05[slec05 ==1]),
            tp06_slec_weight = mean(tp06[slec06 ==1]),
            tp07_slec_weight = mean(tp07[slec07 ==1]),
            tp08_slec_weight = mean(tp08[slec08 ==1]),
            tp09_slec_weight = mean(tp09[slec09 ==1]),
            tp10_slec_weight = mean(tp10[slec10 ==1]),
            tp11_slec_weight = mean(tp11[slec11 ==1]),
            tp12_slec_weight = mean(tp12[slec12 ==1]),
            tp13_slec_weight = mean(tp13[slec13 ==1]),
            tp01_unslec_weight = mean(tp01[slec01 ==0]),
            tp02_unslec_weight = mean(tp02[slec02 ==0]),
            tp03_unslec_weight = mean(tp03[slec03 ==0]),
            tp04_unslec_weight = mean(tp04[slec04 ==0]),
            tp05_unslec_weight = mean(tp05[slec05 ==0]),
            tp06_unslec_weight = mean(tp06[slec06 ==0]),
            tp07_unslec_weight = mean(tp07[slec07 ==0]),
            tp08_unslec_weight = mean(tp08[slec08 ==0]),
            tp09_unslec_weight = mean(tp09[slec09 ==0]),
            tp10_unslec_weight = mean(tp10[slec10 ==0]),
            tp11_unslec_weight = mean(tp11[slec11 ==0]),
            tp12_unslec_weight = mean(tp12[slec12 ==0]),
            tp13_unslec_weight = mean(tp13[slec13 ==0]),)

library(ggplot2)
library(stringr)


topic.name.v <- sapply(topic.name$Topic.name, FUN = function(x){
  strsplit(x, split = "\\. ")[[1]][2]
})

data.frame(slec.weight = colMeans(topic.weight)[1:13],
           unslec.weight = colMeans(topic.weight[14:26])) %>%
  mutate(tp_name = topic.name.v) %>%
  mutate(tp_no = as.character(c(1:13))) %>%
  ggplot(aes(x = unslec.weight, y = slec.weight)) +
  geom_point(alpha = 2, color = 'black', shape = 21, size = 10) +
  geom_text(aes(label = str_wrap(tp_no, 20)),position = position_jitter()) +
  annotate(geom = "text", x = 0.056, y = 0.28, label = "Specific topics", size = 5, fontface = "italic") +
  annotate(geom = "text", x = 0.065, y = 0.25, label = "General topics", size = 5, fontface = "italic") +
  xlab("Mean topic weight in non-dominant articles") +
  ylab("Mean topic weight in dominant articles") +
  theme_classic() +
  xlim(c(0.0545, 0.0685)) +
  ylim(c(0.235, 0.305)) +
  theme(legend.position = 'none')
ggsave(filename = "../../Fig/05_TopicGenerality.png",width = 6,height = 4)


#---
# research gap analysis
#---
library(vegan)
article.weight.matrix <-
  ires.documents %>%
  pivot_wider(id_cols = topic, names_from = document, values_from = gamma) %>%
  dplyr::select(-topic)

article.topic.dis<-vegdist(article.weight.matrix,method = "bray")

max(article.topic.dis)
min(article.topic.dis)

article.topic.dis.scaled <- (article.topic.dis - min(article.topic.dis))/(max(article.topic.dis) - min(article.topic.dis))
topic.dis.scaled <- (topic.dis - min(topic.dis))/(max(topic.dis) - min(topic.dis))

gap.dist<-topic.dis.scaled * article.topic.dis.scaled
gap.dist
# then copy the gap.dist to an excel file for visualisation
