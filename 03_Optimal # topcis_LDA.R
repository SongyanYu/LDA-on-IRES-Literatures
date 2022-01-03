#---
# Since LDA modelling has been validated against Leigh review paper,
# continue to develop another LDA model with the optimal number of topics.
#---

# NEED TO RUN "01_dtm preparation.R" to prepare dtm.

#---
# 1. select the number of topics for LDA
#---
library(ldatuning)
result<-FindTopicsNumber(
  leigh.dtm,
  topics = seq(from=2,to=50,by=1),
  metrics = c("Deveaud2014"),
  method = "Gibbs",
  mc.cores = 3,
  verbose = TRUE)

FindTopicsNumber_plot(result)

#---
# Ten is selected as the optimal topic number
# 2. LDA modelling
#---
library(topicmodels)
n.topic<-10
sunny.lda<-LDA(leigh.dtm,k=n.topic,method = "Gibbs",control = list(seed=1))

#---
# 3. identify topic names
#---
#word-topic probabilities
sunny.topics<-tidy(sunny.lda,matrix="beta")

library(ggplot2)
sunny.top.terms<-sunny.topics%>%
  group_by(topic)%>%
  top_n(20,beta)%>%
  ungroup()%>%
  arrange(topic,-beta)

top_topic_words<-sunny.top.terms%>%
  group_by(topic)%>%
  summarise(Top_topic_words=paste(term,collapse = ", "))

write.csv(top_topic_words,paste0("../../R output/Top topic words_n",n.topic,".csv"),row.names = FALSE)

#sunny.top.terms%>%
#  mutate(term=reorder_within(term,beta,topic))%>%
#  ggplot(aes(term,beta,fill=factor(topic)))+
#  geom_col(show.legend = FALSE)+
#  facet_wrap(~topic,scales = "free")+
#  coord_flip()+
#  scale_x_reordered()+
#  labs(y="probability")+
#  ggsave(paste0("../../Fig/topic-term probabilities_n",n.topic,".png"),width=18,height = 11)

# document-topic probabilities
sunny.documents<-tidy(sunny.lda,matrix="gamma")

relevant.docs<-sunny.documents%>%
  group_by(topic)%>%
  top_n(10,gamma)%>%
  ungroup()%>%
  arrange(topic,-gamma)

relevant.docs<-relevant.docs%>%
  left_join(doc.info,by="document")

write.csv(relevant.docs,
          file = paste0("../../R output/topic-documents_n",n.topic,".csv"),
          row.names = FALSE)

#---
# 4. document composition between the four and ten topics
#---

new.topic.name<-read.csv("../../data/Topic name.csv")

# document-topic probabilities
sunny.documents<-tidy(sunny.lda,matrix="gamma")

sunny.doc.membership<-sunny.documents%>%
  group_by(document)%>%
  summarise(dom.topic=match(max(gamma),gamma))

leigh.doc.membership<-strsplit(papers.abs$Associated.search.es.,", ")
fish.doc<-grep("Fish",leigh.doc.membership)
assessment.doc<-grep("Assessment",leigh.doc.membership)
biogeochemistry.doc<-grep("Biogeochemistry",leigh.doc.membership)
invertebrate.doc<-grep("Invertebrate",leigh.doc.membership)

# membership interaction between 4 and 10 topics.
topics_in_fish<-sunny.doc.membership$dom.topic[as.numeric(sunny.doc.membership$document) %in% fish.doc]
topics_in_assessment<-sunny.doc.membership$dom.topic[as.numeric(sunny.doc.membership$document) %in% assessment.doc]
topics_in_invertebrate<-sunny.doc.membership$dom.topic[as.numeric(sunny.doc.membership$document) %in% invertebrate.doc]
topics_in_biogeochemistry<-sunny.doc.membership$dom.topic[as.numeric(sunny.doc.membership$document) %in% biogeochemistry.doc]

topics_in_fish.df<-data.frame(table(topics_in_fish),topic = "Fish")%>%
  rename(New_topic = topics_in_fish)
topics_in_assessment.df<-data.frame(table(topics_in_assessment),topic = "Assessment")%>%
  rename(New_topic = topics_in_assessment)
topics_in_invertebrate.df<-data.frame(table(topics_in_invertebrate),topic = "Invertebrate")%>%
  rename(New_topic = topics_in_invertebrate)
topics_in_biogeochemistry.df<-data.frame(table(topics_in_biogeochemistry), topic = "Biogeochemistry")%>%
  rename(New_topic = topics_in_biogeochemistry)

rbind(topics_in_fish.df,
      topics_in_assessment.df,
      topics_in_biogeochemistry.df,
      topics_in_invertebrate.df)%>%
  mutate(New_topic = factor(New_topic, labels = new.topic.name$Topic.name))%>%
  ggplot()+
  geom_bar(aes(fill = New_topic, x = topic, y = Freq),
           position = "stack",stat = "identity")+
  ylab("Number of articles")+
  xlab("Topics in Leigh et al. 2016")+
  labs(fill = "New topics for LDA modelling")+
  theme_bw()+
  theme(legend.position = "top")+
  guides(fill=guide_legend(nrow=5, byrow=TRUE))+
  scale_fill_brewer(palette = "Set3")+
  ggsave(filename = "../../Fig/topic composition_4 vs 10 topics.png",
         width = 9.32, height = 7)


#---
# 3. topic similarity
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

#---
# 4. topic popularity
#---

text.df$year=papers.abs$Publication.year
text.df$line<-as.character(text.df$line)

topic.pop<-
  sunny.documents%>%
  left_join(.,text.df,by=c("document"="line"))%>%
  group_by(year,topic)%>%
  summarise(total.gamma=sum(gamma),n.doc=n())

library(tibble)

doc.year<-text.df%>%
  group_by(year)%>%
  summarise(n=n())

# aggregate 1981-1994
topic.pop.1981_1994<-topic.pop[c(1:80),]%>%
  group_by(topic)%>%
  summarise(gamma=sum(total.gamma),n=sum(n.doc),prop=gamma/n)%>%
  add_column(year="1981-1994")%>%
  dplyr::select(year,topic,prop)

topic.pop.1995_2014<-topic.pop[-c(1:80),]%>%
  group_by(year,topic)%>%
  summarise(prop=total.gamma/n.doc)

topic.pop.1995_2014$year<-as.character(topic.pop.1995_2014$year)
topic.pop.all<-bind_rows(topic.pop.1981_1994,topic.pop.1995_2014)

# plotting
topic.name<-read.csv("data/Topic name.csv")
topic.pop.all<-left_join(topic.pop.all,topic.name,by=c("topic"="Topic"))

topic.avg.prop<-topic.pop.all%>%
  group_by(topic,Topic.name)%>%
  summarise(avg.prop.change=mean(diff(prop))*100,
            se=sd(prop)/sqrt(n()))%>%
  arrange(avg.prop.change)%>%
  mutate(group=ifelse(avg.prop.change>0.1,"Hot",ifelse(avg.prop.change<(-0.1),"Cold","Neutral")))

# average change in prevalence (figure)
topic.avg.prop%>%
  ggplot(aes(x=factor(topic,levels=dput(as.character(topic))),
             y=avg.prop.change,
             color=avg.prop.change,
             label=Topic.name))+
  geom_hline(yintercept = 0,
             linetype="dotted",
             size=1.5)+
  geom_point(show.legend = FALSE)+
  geom_pointrange(aes(ymin=avg.prop.change-se*100,
                      ymax=avg.prop.change+se*100),
                  fatten=5,size=1.2,
                  show.legend = FALSE)+
  scale_color_gradient2(low="blue",high = "red",mid="grey",
                        name="Average change in prevalence")+
  geom_text(aes(y=avg.prop.change-1.6),
            size=5,
            hjust=0.,
            show.legend = FALSE)+
  coord_flip()+
  ylab("Average change in prevalence (%)")+
  theme_classic()+
  theme(axis.line.y = element_blank(),axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),axis.title.y = element_blank())+
  scale_y_continuous(position = "right")+
  ggsave(filename = paste0("Fig/Avg topic prevalence_n",n.topic,".png"))


# topic trend over time (figure)
topic.pop.all%>%
  left_join(topic.avg.prop[,c(1,3,5)],by="topic")%>%
  ggplot(aes(x=year,y=prop*100,group=Topic.name))+
  geom_line(aes(color=avg.prop.change),size=1.2)+
  scale_color_gradient2(low="blue",high = "red",mid = "grey",
                        name="Average change in prevalence")+
  facet_grid(factor(group,levels =c("Hot","Neutral","Cold"))~.)+
  theme_classic()+
  theme(panel.grid.major.y=element_line(linetype = "dotted",color="grey"))+
  xlab("")+ylab("Topic prevalence (%)")+
  ggsave(filename = paste0("Fig/Topic trend over time_n",n.topic,".png"))


