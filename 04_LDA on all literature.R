#---
# LDA modelling on updated literature list (Leigh's + latest)
# Author: Songyan Yu
# Date created: 09/04/2020
# Date updated: 10/03/2021
#---

#---
# 1. read in all literatures (Leigh's + latest)
#---

# 1.1 Leigh's lit (1964 - Aug 2014)
papers<-read.csv("../../data/Literatures in Leigh et al 2016.csv",skip = 1)

# remove articles without abstract
library(dplyr)
papers.abs<-papers[!(papers$Abstract==""),]
doc.info<-papers.abs[,c("Title","Abstract","Publication.year")]%>%
  rename(Year = Publication.year)

# 1.2 Latest lit (Sep 2014-Nov 2019)
#new.lit<-list.files(path = "../../data/",pattern = "_Filtered.xlsx",full.names = TRUE)
#library(readxl)
#new.lit.lst<-lapply(new.lit,FUN = function(x) read_xlsx(x))
#new.lit.lst<-lapply(new.lit.lst,"[",c("Title","Abstract"))
#library(dplyr)
#new.lit<-do.call("rbind",new.lit.lst)%>%
#  distinct(.,Title)
#write.csv(new.lit,file = "../../R output/New literature.csv",row.names = FALSE)
# check for further duplicates, manually remove duplicates.

files<-list.files(path = "../../data/",pattern = "2014-2019.csv",full.names = TRUE)
abstract.lst<-lapply(files,FUN = function(x) read.csv(x,encoding = "UTF-8"))
abstract.lst<-lapply(abstract.lst,"[",c("Title","Abstract","Year"))
abstract<-do.call("rbind",abstract.lst)

lit.title<-read.csv("../../R output/New literature-NO DUPLICATES.csv")
new.lit<-abstract[match(lit.title$Title,abstract$Title),]
new.lit<-new.lit[complete.cases(new.lit),]

# 1.3 combine the two lits together
all.doc<-rbind(doc.info,new.lit)%>%
  distinct(Title,.keep_all = TRUE)

all.doc$Abstract<-gsub(pattern = "\uA9.*",x=all.doc$Abstract,replacement = "") # remove "copyright" information
all.doc$Abstract<-gsub(pattern = "(Crown Copyright|Copyright).*",x=all.doc$Abstract,replacement = "") # same as above
all.doc$Abstract<-gsub(pattern = "\\(C\\) \\d.*",x=all.doc$Abstract,replacement = "") # same as above
all.doc$Abstract<-gsub(pattern = "\\(c\\) \\d.*",x=all.doc$Abstract,replacement = "") # same as above
all.doc$Abstract<-gsub(pattern = "\\[No abstract available\\]",x=all.doc$Abstract, replacement = "") # remove "no abstract"
all.doc$Abstract<-gsub(pattern = "from Authors",x=all.doc$Abstract, replacement = "") # remove duplicate abstract

#write.csv(all.doc,file = "../../R output/All doc.csv",row.names = FALSE)

all.doc$document<-c(1:nrow(all.doc))
all.doc$document<-as.character(all.doc$document)

text<-paste(all.doc$Title,all.doc$Abstract)
library(dplyr)
text.df<-tibble(line=1:length(text),text)

#---
# 2. tokenisation, remove stop words, numbers, punctuation, words with length < 3, and stem word.
#---
publisher.word<-c("wiley","elsevier","john","springer","blackwell","ltd","authors","author","taylor","francis",
                  "copyright","press","mdpi","licensee","basel","switzerland","sons","australia") # to be removed

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
  filter(n>300) #was 300

text.1.gram.short<-text.1.gram%>%              # If there is an error, rerun this two lines again.
  anti_join(common.word,by="word")

# remove words occurring in <6 documents
rare.word<-text.1.gram.short%>%
  group_by(word)%>%
  summarise(n=n())%>%
  filter(n<6)

text.1.gram.short<-text.1.gram.short%>%
  anti_join(rare.word,by="word")

# number of unique terms (n=1041)
text.1.gram.short%>%
  group_by(word)%>%
  summarise(n=n())

length.text<-text.1.gram%>%
  group_by(line)%>%
  summarise(n = sum(n))

#bi-gram
library(tidyr)
text.2.gram<-text.df%>%
  unnest_tokens(word,text,token = "ngrams",n=2)%>%
  separate(word,c("word1","word2"),sep=" ")%>%
  filter(!word1 %in% stop_words$word)%>%
  filter(!word2 %in% stop_words$word)%>%
  filter(is.na(as.numeric(word1)))%>%
  filter(is.na(as.numeric(word2)))%>%
  filter(!word1 %in% publisher.word)%>%
  filter(!word2 %in% publisher.word)%>%
  mutate(word1=wordStem(word1),word2=wordStem(word2))%>%
#  filter(!word1 %in% common.word$word)%>%              # a common unigram word does not mean a common 2-gram word.
#  filter(!word2 %in% common.word$word)%>%
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
  dplyr::select(line,bigram,n = n.x)


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
  mutate(word1=wordStem(word1),word2=wordStem(word2),word3=wordStem(word3))%>%
#  filter(!word1 %in% common.word$word)%>%
#  filter(!word2 %in% common.word$word)%>%
#  filter(!word3 %in% common.word$word)%>%
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
  dplyr::select(line,trigram,n=n.x)

#---
# 4. dtm and LDA modelling
#---
# combine uni-, bi- and tri-grams together to form a dtm
colnames(text.1.gram.short)[2]<-"term"
colnames(text.2.gram.short)[2]<-"term"
colnames(text.3.gram.short)[2]<-"term"

all.term<-rbind(text.1.gram.short,
                  text.2.gram.short,
                  text.3.gram.short)

length(unique(all.term$term))
sum(all.term$n)

all.dtm<-all.term%>%
  cast_dtm(document = line,term = term,value=n)

library(tm)
inspect(all.dtm)

# select the number of topics for LDA (n=2~50)
library(ldatuning)
seed.findtopicnumber <- c(2:30)
result<-FindTopicsNumber(
  all.dtm,
  topics = seq(from=2,to=30,by=1),
  metrics = c("Deveaud2014"),
  method = "Gibbs",
  mc.cores = 3,
  verbose = TRUE)

#png(filename = "Fig/ARI authors/Find topic number_v4.png")
FindTopicsNumber_plot(result)  # n=10 topics looks  optimal.
#dev.off()


# LDA modelling
library(topicmodels)
seed.lda<-seq(from=1, to=8, by=1)  # for reproducibility.

# either try this number of topics (n=15)
n.topic<-10

# or run a loop to try multiple numbers of topics (n=15-22)
#for(n.topic in 15:22){
  ari.lda<-LDA(all.dtm,k=n.topic,method = "Gibbs",control = list(seed=seed.lda[n.topic-9]))
  
  # word-topic probabilities
  ari.topics<-tidy(ari.lda,matrix="beta")
  
  library(ggplot2)
  ari.top.terms<-ari.topics%>%
    group_by(topic)%>%
    top_n(20,beta)%>%
    ungroup()%>%
    arrange(topic,-beta)
  
  ari.top.terms%>%
    mutate(term=reorder_within(term,beta,topic))%>%
    ggplot(aes(term,beta,fill=factor(topic)))+
    geom_col(show.legend = FALSE)+
    facet_wrap(~topic,scales = "free")+
    coord_flip()+
    scale_x_reordered()+
    labs(y="probability")+
    ggsave(paste0("../../Fig/topic-term probabilities_n",n.topic,"_v1.png"),width=18,height = 11)
  
  #---
  # topic similarity
  #---
  library(reshape2)
  weight.matrix<-as.data.frame(acast(ari.topics,topic~term))
  
  # Bray-Curtis distance (dissimilarity)
  library(vegan)
  topic.dis<-vegdist(weight.matrix,method = "bray")
  
  # non-metric multidimentional scaling. Note final value is better <20
  library(MASS)
  fits<-isoMDS(topic.dis,k=3)
  
  library(ggplot2)
  similarity.df<-as.data.frame(fits$points)
  similarity.df$topic<-c(1:n.topic)
  
  # document-topic probabilities
  ari.documents<-tidy(ari.lda,matrix="gamma")
  
  ari.documents.wide<-ari.documents%>%
    pivot_wider(id_cols = document,names_from = topic,values_from = gamma)%>%
    left_join(.,all.doc,by="document")
  
  #write.csv(ari.documents.wide,file = "ARI_Topic_Docs.csv",row.names = FALSE)
  
  relevant.docs<-ari.documents%>%
        group_by(topic)%>%
        top_n(10,gamma)%>%
        ungroup()%>%
    arrange(topic,-gamma)
  
  relevant.docs<-relevant.docs%>%
    left_join(all.doc,by="document")
  
  
  #write.csv(relevant.docs,
  #          file = paste0("../../R output/topic-documents_n",n.topic,"_all-literature.csv"),
  #          row.names = FALSE)
  
  topic.size<-ari.documents%>%
    group_by(document)%>%
    summarise(dom.topic=match(max(gamma),gamma))%>%
    group_by(dom.topic)%>%
    summarise(n=n())
  
  similarity.df%>%
    left_join(topic.size,by=c("topic"="dom.topic"))%>%
    ggplot(aes(x=V2,y=V3,size=n))+
    geom_point(alpha=0.5,color="black",shape=21)+
    geom_text(aes(label=topic),size=4)+
    scale_size(range = c(5,15),name=paste0("Frequency of dominance","\n","              (articles)"))+
    xlab("NMDS1")+ylab("NMDS2")+
    theme_classic()+
    ggsave(filename = paste0("../../Fig/Topic similarity_n",n.topic,".png"),width = 8,height = 4)
  
#}

#---
# topic-document-gamma
#---
ari.documents.cast<-dcast(ari.documents,document~topic)
topic.document.gamma<-left_join(ari.documents.cast,all.doc,by=c("document"))
#write.csv(topic.document.gamma,file = "R output/topic-document-gamma_n15.csv",row.names = FALSE)

#---
# topic-term-beta
#---
ari.top.terms<-ari.topics%>%
  group_by(topic)%>%
  top_n(100,beta)%>%
  ungroup()%>%
  arrange(topic,-beta)

#write.csv(ari.top.terms,file = "R output/topic-term-beta-top100_n15.csv",row.names = FALSE)

#---
# topic popularity
#---
pub.by.year<-all.doc%>%
  group_by(Year)%>%
  summarise(n=n())

# document-topic probabilities
ari.documents<-tidy(ari.lda,matrix="gamma")

text.df$year=all.doc$Year
text.df$line<-as.character(text.df$line)

topic.pop<-ari.documents%>%
  left_join(.,text.df,by=c("document"="line"))%>%
  group_by(year,topic)%>%
  summarise(total.gamma=sum(gamma),n.doc=n())

library(tibble)

# aggregate 1981-1994

topic.pop.1981_1994<-topic.pop[c(1:80),]%>%
  group_by(topic)%>%
  summarise(gamma=sum(total.gamma),n=sum(n.doc),prop=gamma/n)%>%
  add_column(year="1981-1994")%>%
  dplyr::select(year,topic,prop)

topic.pop.1995_1999<-topic.pop[c(81:130),]%>%
  group_by(topic)%>%
  summarise(gamma=sum(total.gamma),n=sum(n.doc),prop=gamma/n)%>%
  add_column(year="1995-1999")%>%
  dplyr::select(year,topic,prop)

topic.pop.2000_2004<-topic.pop[c(131:180),]%>%
  group_by(topic)%>%
  summarise(gamma=sum(total.gamma),n=sum(n.doc),prop=gamma/n)%>%
  add_column(year="2000-2004")%>%
  dplyr::select(year,topic,prop)

topic.pop.2005_2009<-topic.pop[c(181:230),]%>%
  group_by(topic)%>%
  summarise(gamma=sum(total.gamma),n=sum(n.doc),prop=gamma/n)%>%
  add_column(year="2005-2009")%>%
  dplyr::select(year,topic,prop)

topic.pop.2010_2014<-topic.pop[c(231:280),]%>%
  group_by(topic)%>%
  summarise(gamma=sum(total.gamma),n=sum(n.doc),prop=gamma/n)%>%
  add_column(year="2010-2014")%>%
  dplyr::select(year,topic,prop)

topic.pop.2015_2019<-topic.pop[c(281:330),]%>%
  group_by(topic)%>%
  summarise(gamma=sum(total.gamma),n=sum(n.doc),prop=gamma/n)%>%
  add_column(year="2014-2019")%>%
  dplyr::select(year,topic,prop)

topic.pop.all<-bind_rows(topic.pop.1981_1994,
                         topic.pop.1995_1999,
                         topic.pop.2000_2004,
                         topic.pop.2005_2009,
                         topic.pop.2010_2014,
                         topic.pop.2015_2019)

topic.name<-read.csv("../../data/Topic names_all literature.csv")
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
  geom_text(aes(y=avg.prop.change-5),
            size=5,
            hjust=0.,
            show.legend = FALSE)+
  coord_flip()+
  ylab("Average change in prevalence (%)")+
  theme_classic()+
  theme(axis.line.y = element_blank(),axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),axis.title.y = element_blank())+
  scale_y_continuous(position = "right")+
  ggsave(filename = paste0("../../Fig/Avg topic prevalence_n",n.topic,".png"),width = 9.32,height = 5.73)


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
  ggsave(filename = paste0("../../Fig/Topic trend over time_n",n.topic,".png"),width = 9.32,height = 5.73)

#---
# topic generality/specificity
#---

topic.weight<-ari.documents.cast%>%
  rename(tp01 = "1", tp02 = "2",tp03 = "3", tp04 = "4", tp05 = "5", 
         tp06 = "6", tp07 = "7", tp08 = "8", tp09 = "9", tp10 = "10")%>%
  mutate(max = pmax(tp01, tp02, tp03, tp04, tp05, tp06, tp07, tp08, tp09, tp10),
         slec01 = ifelse(tp01 < max, 0, 1), slec02 = ifelse(tp02 < max, 0, 1),
         slec03 = ifelse(tp03 < max, 0, 1), slec04 = ifelse(tp04 < max, 0, 1),
         slec05 = ifelse(tp05 < max, 0, 1), slec06 = ifelse(tp06 < max, 0, 1),
         slec07 = ifelse(tp07 < max, 0, 1), slec08 = ifelse(tp08 < max, 0, 1),
         slec09 = ifelse(tp09 < max, 0, 1), slec10 = ifelse(tp10 < max, 0, 1))%>%
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
            tp01_unslec_weight = mean(tp01[slec01 ==0]),
            tp02_unslec_weight = mean(tp02[slec02 ==0]),
            tp03_unslec_weight = mean(tp03[slec03 ==0]),
            tp04_unslec_weight = mean(tp04[slec04 ==0]),
            tp05_unslec_weight = mean(tp05[slec05 ==0]),
            tp06_unslec_weight = mean(tp06[slec06 ==0]),
            tp07_unslec_weight = mean(tp07[slec07 ==0]),
            tp08_unslec_weight = mean(tp08[slec08 ==0]),
            tp09_unslec_weight = mean(tp09[slec09 ==0]),
            tp10_unslec_weight = mean(tp10[slec10 ==0]))

library(ggplot2)
library(stringr)
data.frame(slec.weight = colMeans(topic.weight)[1:10],
           unslec.weight = colMeans(topic.weight[11:20]))%>%
  mutate(tp_name = topic.name$Topic)%>%
  ggplot(aes(x = unslec.weight, y = slec.weight))+
  geom_point(shape = 10,size = 10)+
  geom_text(aes(label = str_wrap(tp_name,15)),position = position_jitter(),vjust = 2.5,size = 4)+
  annotate(geom = "text", x = 0.07, y = 0.327, label = "Specific topics",size = 5,fontface = "italic")+
  annotate(geom = "text", x = 0.0825, y = 0.28, label = "General topics",size = 5,fontface = "italic")+
  xlab("Mean weight (unselected articles)")+
  ylab("Mean weight (selected articles)")+
  theme_bw()+
  ylim(c(0.265, 0.35))+
  xlim(c(0.068, 0.086))+
  ggsave(filename = "../../Fig/Topic generality.png",width = 7,height = 5)



