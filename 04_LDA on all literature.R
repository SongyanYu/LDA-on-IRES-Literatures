#---
# LDA modelling on updated literature list (Leigh's + latest)
# Author: Songyan Yu
# Date created: 09/04/2020
#---

#---
# 1. read in all literatures (Leigh's + latest)
#---

# 1.1 Leigh's lit ( - Aug 2014)
papers<-read.csv("../../data/Literatures in Leigh et al 2016.csv",skip = 1)

# remove articles without abstract
papers.abs<-papers[!(papers$Abstract==""),]
doc.info<-papers.abs[,c("Title","Abstract")]

# 1.2 Latest lit (Sep 2014-Nov 2019)
new.lit<-list.files(path = "../../data/",pattern = "_Filtered.xlsx",full.names = TRUE)
library(readxl)
new.lit.lst<-lapply(new.lit,FUN = function(x) read_xlsx(x))
new.lit.lst<-lapply(new.lit.lst,"[",c("Title","Abstract"))

#library(dplyr)
#new.lit<-do.call("rbind",new.lit.lst)%>%
#  distinct(.,Title)

#write.csv(new.lit,file = "../../R output/New literature.csv",row.names = FALSE)
# check for further duplicates, manually remove duplicates.

files<-list.files(path = "../../data/",pattern = "2014-2019.csv",full.names = TRUE)
abstract.lst<-lapply(files,FUN = function(x) read.csv(x,encoding = "UTF-8"))
abstract.lst<-lapply(abstract.lst,"[",c("Title","Abstract"))
abstract<-do.call("rbind",abstract.lst)

lit.title<-read.csv("../../R output/New literature-NO DUPLICATES.csv")
new.lit<-abstract[match(lit.title$Title,abstract$Title),]
new.lit<-new.lit[complete.cases(new.lit),]

all.doc<-rbind(doc.info,new.lit)
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
