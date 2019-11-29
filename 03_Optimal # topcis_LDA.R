#---
# Since LDA modelling has been validated against Leigh review paper,
# continue to develop another LDA model with the optimal number of topics.
#---

# NEED TO RUN "01_dtm preparation.R" to prepare dtm.

# select the number of topics for LDA
library(ldatuning)
result<-FindTopicsNumber(
  leigh.dtm,
  topics = seq(from=2,to=50,by=1),
  metrics = c("Griffiths2004","CaoJuan2009","Arun2010","Deveaud2014"),
  method = "Gibbs",
  mc.cores = 3,
  verbose = TRUE
)

FindTopicsNumber_plot(result)

# Ten is selected as the optimal topic number
# LDA modelling
library(topicmodels)
n.topic<-10
sunny.lda<-LDA(leigh.dtm,k=n.topic,method = "Gibbs",control = list(seed=1))

# word-topic probabilities
sunny.topics<-tidy(sunny.lda,matrix="beta")

library(ggplot2)
sunny.top.terms<-sunny.topics%>%
  group_by(topic)%>%
  top_n(20,beta)%>%
  ungroup()%>%
  arrange(topic,-beta)

sunny.top.terms%>%
  mutate(term=reorder_within(term,beta,topic))%>%
  ggplot(aes(term,beta,fill=factor(topic)))+
  geom_col(show.legend = FALSE)+
  facet_wrap(~topic,scales = "free")+
  coord_flip()+
  scale_x_reordered()+
  labs(y="probability")+
  ggsave(paste0("Fig/topic-term probabilities_n",n.topic,".png"),width=18,height = 11)

#---
# topic similarity
#---
library(reshape2)
weight.matrix<-as.data.frame(acast(sunny.topics,topic~term))

# Bray-Curtis distance (dissimilarity)
library(vegan)
topic.dis<-vegdist(weight.matrix,method = "bray")

# non-metric multidimentional scaling
library(MASS)
fits<-isoMDS(topic.dis,k=2)

library(ggplot2)
similarity.df<-as.data.frame(fits$points)
similarity.df$topic<-c(1:n.topic)

# document-topic probabilities
sunny.documents<-tidy(sunny.lda,matrix="gamma")

relevant.docs<-sunny.documents%>%
  group_by(topic)%>%
  top_n(10,gamma)%>%
  ungroup()%>%
  arrange(topic,-gamma)

#relevant.docs<-relevant.docs%>%
#  left_join(doc.info,by="document")

#write.csv(relevant.docs,
#          file = paste0("R output/topic-documents_n",n.topic,".csv"),
#          row.names = FALSE)

topic.size<-sunny.documents%>%
  group_by(document)%>%
  summarise(dom.topic=match(max(gamma),gamma))%>%
  group_by(dom.topic)%>%
  summarise(n=n())

similarity.df%>%
  left_join(topic.size,by=c("topic"="dom.topic"))%>%
  ggplot(aes(x=V1,y=V2,size=n))+
  geom_point(alpha=0.5,color="black",shape=21)+
  geom_text(aes(label=topic),size=4)+
  scale_size(range = c(5,15),name=paste0("Frequency of dominance","\n","              (articles)"))+
  xlab("NMDS1")+ylab("NMDS2")+
  theme_classic()+
  ggsave(filename = paste0("Fig/Topic similarity_n",n.topic,".png"),width = 8,height = 4)


