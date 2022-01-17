#---
# Prepare dtm for LDA modelling
# Author: Songyan Yu
# Date updated: 23/03/2021
#---

#---
# 1. read in Leigh literature
#---
papers<-read.csv("../../data/Literatures in Leigh et al 2016.csv",skip = 1)

# remove articles without abstract
papers.abs<-papers[!(papers$Abstract==""),]

doc.info<-papers.abs[,c(1,3,4,5)]
doc.info$document<-c(1:nrow(doc.info))
doc.info$document<-as.character(doc.info$document)

#doc.info$Abstract<-gsub(pattern = "\uA9.*",x=doc.info$Abstract,replacement = "") # remove "copyright" information
#doc.info$Abstract<-gsub(pattern = "(Crown Copyright|Copyright).*",x=doc.info$Abstract,replacement = "") # same as above
#doc.info$Abstract<-gsub(pattern = "\\(C\\) \\d.*",x=doc.info$Abstract,replacement = "") # same as above
#doc.info$Abstract<-gsub(pattern = "\\[No abstract available\\]",x=doc.info$Abstract, replacement = "") # remove "no abstract"
#doc.info$Abstract<-gsub(pattern = "from Authors",x=doc.info$Abstract, replacement = "") # remove duplicate abstract

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
  anti_join(stop_words)%>%                # remove stop words (e.g. and, with, etc.)
  filter(is.na(as.numeric(word)))%>%      # remove numbers
  filter(!word %in% publisher.word)%>%    # remove publisher words
  mutate(word=wordStem(word))%>%          # stem words
  filter(nchar(word)>2)%>%                # remove words with less than 3 letters (e.g. in)
  filter(!grepl("[[:digit:]]",word))%>%   # remove words with number in it (e.g. NH4)
  count(line,word)%>%
  filter(n>1)          # remove words that only be mentioned once in a paper's title and abstract.

# remove words occurring in >203 documents
common.word<-text.1.gram%>%
  group_by(word)%>%
  summarise(n=n())%>%
  filter(n>203)

text.1.gram.short<-text.1.gram%>%
  anti_join(common.word,by="word")

# remove words occurring in < 8 documents
rare.word<-text.1.gram.short%>%
  group_by(word)%>%
  summarise(n=n())%>%
  filter(n<8)

text.1.gram.short<-text.1.gram.short%>%
  anti_join(rare.word,by="word")

# number of unique terms (n=837)
text.1.gram.short%>%
  group_by(word)%>%
  summarise(n=n())

length.text<-text.1.gram%>%
  group_by(line)%>%
  summarise(n=sum(n))

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
#  filter(!word1 %in% common.word$word)%>%
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

leigh.dtm <-
  leigh.term%>%
  cast_dtm(document = line,term = term,value=n)

tm::inspect(leigh.dtm)
