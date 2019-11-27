#---
# LDA modelling based on retained literatures from Leigh et al. 2016 Freshwater Ecology.
#---
setwd("../../")

#---
# 1. read in Leigh literatures
#---
papers<-read.csv("data/Literatures in Leigh et al 2016.csv",skip = 1)

# remove articles without abstract
papers.abs<-papers[!(papers$Abstract==""),]

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
text.1.gram<-text.df%>%
  unnest_tokens(word,text)%>%
  anti_join(stop_words)%>%
  filter(is.na(as.numeric(word)))%>%
  filter(!word %in% publisher.word)%>%
  mutate(word=wordStem(word))%>%
  filter(nchar(word)>2)%>%
  filter(!grepl("[[:digit:]]",word))%>%
  count(line,word)%>%
  filter(n>1)

# remove words occurring in >300 documents
common.word<-text.1.gram%>%
  group_by(word)%>%
  summarise(n=n())%>%
  filter(n>203)

text.1.gram.short<-text.1.gram%>%
  anti_join(common.word,by="word")

# remove words occurring in <6 documents
rare.word<-text.1.gram.short%>%
  group_by(word)%>%
  summarise(n=n())%>%
  filter(n<6)

text.1.gram.short<-text.1.gram.short%>%
  anti_join(rare.word,by="word")

# number of unique terms (n=839)
text.1.gram.short%>%
  group_by(word)%>%
  summarise(n=n())

length.text<-text.1.gram%>%
  group_by(line)%>%
  summarise(n=n())

#bi-gram
library(tidyr)
text.2.gram<-text.df%>%
  unnest_tokens(word,text,token = "ngrams",n=2)%>%
  separate(word,c("word1","word2"),sep=" ")%>%
  filter(!word1 %in% stop_words$word)%>%
  filter(!word2 %in% stop_words$word)%>%
  filter(!word1 %in% common.word$word)%>%
  filter(!word2 %in% common.word$word)%>%
  filter(is.na(as.numeric(word1)))%>%
  filter(is.na(as.numeric(word2)))%>%
  filter(!word1 %in% publisher.word)%>%
  filter(!word2 %in% publisher.word)%>%
  mutate(word1=wordStem(word1),word2=wordStem(word2))%>%
  filter(nchar(word1)>2)%>%
  filter(nchar(word2)>2)%>%
  filter(!grepl("[[:digit:]]",word1))%>%
  filter(!grepl("[[:digit:]]",word2))%>%
  unite(bigram,word1,word2,sep="_")%>%
  count(line, bigram,sort=TRUE)

# remain only those bigrams that occur more than expected by chance under a significant value of p<.05
# Student's t test (degree of freedom?)
text.2.gram.short<-text.2.gram%>%
  separate(bigram,c("word1","word2"),sep="_")%>%
  left_join(.,text.1.gram,by=c("line","word1"="word"))%>%
  left_join(.,text.1.gram,by=c("line","word2"="word"))%>%
  left_join(.,length.text,by="line")%>%
  mutate(p_bi=n.x/n.y.y,p1=n.y/n.y.y,p2=n.x.x/n.y.y,t=(p_bi-p1*p2)/sqrt(p_bi*(1-p_bi)/n.y.y))%>%
  filter(t>1.6)%>%
  unite(bigram,word1,word2,sep = "_")%>%
  select(line,bigram,n=n.x)

# tri-gram
text.3.gram<-text.df%>%
  unnest_tokens(word,text,token = "ngrams",n=3)%>%
  separate(word,c("word1","word2","word3"),sep=" ")%>%
  filter(!word1 %in% stop_words$word)%>%
  filter(!word2 %in% stop_words$word)%>%
  filter(!word3 %in% stop_words$word)%>%
  filter(is.na(as.numeric(word1)))%>%
  filter(is.na(as.numeric(word2)))%>%
  filter(is.na(as.numeric(word3)))%>%
  filter(!word1 %in% publisher.word)%>%
  filter(!word2 %in% publisher.word)%>%
  filter(!word3 %in% publisher.word)%>%
  filter(!word1 %in% common.word$word)%>%
  filter(!word2 %in% common.word$word)%>%
  filter(!word3 %in% common.word$word)%>%
  mutate(word1=wordStem(word1),word2=wordStem(word2),word3=wordStem(word3))%>%
  filter(nchar(word1)>2)%>%
  filter(nchar(word2)>2)%>%
  filter(nchar(word3)>2)%>%
  filter(!grepl("[[:digit:]]",word1))%>%
  filter(!grepl("[[:digit:]]",word2))%>%
  filter(!grepl("[[:digit:]]",word3))%>%
  unite(trigram,word1,word2,word3,sep="_")%>%
  count(line,trigram)

# remain only those trigrams that occur more than expected by chance under a significant value of p<.05
# Student's t test (degree of freedom?)
text.3.gram.short<-text.3.gram%>%
  separate(trigram,c("word1","word2","word3"),sep="_")%>%
  left_join(.,text.1.gram,by=c("line","word1"="word"))%>%
  left_join(.,text.1.gram,by=c("line","word2"="word"))%>%
  left_join(.,text.1.gram,by=c("line","word3"="word"))%>%
  left_join(.,length.text,by="line")%>%
  mutate(p_tri=n.x/n,p1=n.y/n,p2=n.x.x/n,p3=n.y.y/n,t=(p_tri-p1*p2*p3)/sqrt(p_tri*(1-p_tri)/n))%>%
  filter(t>1.4)%>%
  unite(trigram,word1,word2,word3,sep = "_")%>%
  select(line,trigram,n=n.x)

#---
# 4. dtm and LDA modelling
#---
# combine uni-, bi- and tri-grams together to form a dtm
colnames(text.1.gram.short)[2]<-"term"
colnames(text.2.gram.short)[2]<-"term"
colnames(text.3.gram.short)[2]<-"term"

leigh.term<-rbind(text.1.gram.short,
                text.2.gram.short,
                text.3.gram.short)

length(unique(leigh.term$term))
sum(leigh.term$n)

leigh.dtm<-leigh.term%>%
  cast_dtm(document = line,term = term,value=n)

library(tm)
inspect(leigh.dtm)

# LDA modelling
library(topicmodels)

n.topic<-4

leigh.lda<-LDA(leigh.dtm,k=n.topic,method = "Gibbs")

# word-topic probabilities
leigh.topics<-tidy(leigh.lda,matrix="beta")

library(ggplot2)
leigh.top.terms<-leigh.topics%>%
  group_by(topic)%>%
  top_n(20,beta)%>%
  ungroup()%>%
  arrange(topic,-beta)

leigh.top.terms%>%
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
weight.matrix<-as.data.frame(acast(leigh.topics,topic~term))

# Bray-Curtis distance (dissimilarity)
library(vegan)
topic.dis<-vegdist(weight.matrix,method = "bray")

# non-metric multidimentional scaling
library(MASS)
fits<-isoMDS(topic.dis,k=3)

library(ggplot2)
similarity.df<-as.data.frame(fits$points)
similarity.df$topic<-c(1:n.topic)

# document-topic probabilities
leigh.documents<-tidy(leigh.lda,matrix="gamma")

relevant.docs<-leigh.documents%>%
  group_by(topic)%>%
  top_n(10,gamma)%>%
  ungroup()%>%
  arrange(topic,-gamma)

relevant.docs<-relevant.docs%>%
  left_join(doc.info,by="document")

#write.csv(relevant.docs,
#          file = paste0("R output/topic-documents_n",n.topic,".csv"),
#          row.names = FALSE)

topic.size<-leigh.documents%>%
  group_by(document)%>%
  summarise(dom.topic=match(max(gamma),gamma))%>%
  group_by(dom.topic)%>%
  summarise(n=n())

similarity.df%>%
  left_join(topic.size,by=c("topic"="dom.topic"))%>%
  ggplot(aes(x=V1,y=V3,size=n))+
  geom_point(alpha=0.5,color="black",shape=21)+
  geom_text(aes(label=topic),size=4)+
  scale_size(range = c(5,15),name=paste0("Frequency of dominance","\n","              (articles)"))+
  xlab("NMDS1")+ylab("NMDS3")+
  theme_classic()+
  ggsave(filename = paste0("Fig/Topic similarity_n",n.topic,".png"),width = 8,height = 4)






