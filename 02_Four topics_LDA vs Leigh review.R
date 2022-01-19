#---
# Compare the identified topics from LDA modelling with Leigh review paper.
# Four topics: Biogeochemistry, Invertebrate, Fish, and Hydrological and ecological assessment.
#---

# NEED TO RUN "01_dtm preparation.R" to prepare dtm.

# LDA modelling
library(topicmodels)

n.topic<-4
leigh.lda<-LDA(leigh.dtm,k=n.topic,method = "Gibbs",control = list(seed=1)) 

# word-topic probabilities
leigh.topics<-tidy(leigh.lda,matrix="beta")

library(ggplot2)
leigh.top.terms <-
  leigh.topics %>%
  group_by(topic) %>%
  top_n(20, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_topic_words <-
  leigh.top.terms %>%
  group_by(topic) %>%
  summarise(Top_topic_words = paste(term, collapse = ", "))

write.csv(top_topic_words, "../../R output/02_TopTopicWords_n4.csv", row.names = FALSE)

leigh.top.terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term,beta,fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~topic, scales = "free") +
  coord_flip() +
  scale_x_reordered() +
  labs(y = "probability")
ggsave(paste0("../../Fig/02_TopicTermProbabilities_n",n.topic,".png"),width=8,height = 7)


#---
# article compositon between the two group of 4 topics (LDA vs. Leigh)
#---

dom.topic.doc <-
  tidy(leigh.lda,matrix="gamma") %>%
  group_by(document) %>%
  summarise(dom.topic = match(max(gamma), gamma)) %>%
  mutate(dom.topic = factor(dom.topic, levels = c("1","2","3","4"),
                            labels = c("Invertebrate", "Biogeochemistry", "Fish", "Assessment"))) %>%
  left_join(., doc.info, by = "document") %>%
  select(1,2,3) %>%
  rowwise() %>%
  mutate(consist = ifelse(dom.topic %in% (strsplit(Associated.search.es.,split = ", ")[[1]]), 1, 0)) %>%
  group_by(dom.topic) %>%
  summarise(n = n(),
            consist = sum(consist),
            prop = consist/n)





