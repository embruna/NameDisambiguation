
# R CODE FOR DISAMBIGUATING NAMES FOR SCIENTOMETRIC ANALYSES:

# stringdist = 0.9.4.4
# RecordLinkage = 0.4.10
# tidyverse = 1.0
# R version = 3.3.1

library(tidyverse)
library(RecordLinkage)
library(stringdist)




  
  ######################################################
  ######################################################
  #
  # NAME CORRECTION AND DISAMIGUATION
  #
  ######################################################
  ######################################################
 
  # The fiel should have first name, middle name, last name, FirstMiddleLast....
 
 
  #############################################################
  # NAME COMPARISON AND SPELL CHECK
  ##############################################################
  # This function will compare all names  to each other to help ID 
  # spelling mistakes, cases where names are similar enough to warrant
  # 2x, middle initials, etc. This will makeit easier to assign a ID 
  # number to each editor for disambiguation
  
  source("Name.check.R")
  NameSimilarityDF<-Name.check(ALLDATA,ALLDATA$FirstMiddleLast)
  write.csv(NameSimilarityDF, file="------/NameCheck_ALLDATA_ALLYRS.csv", row.names = T) #export it as a csv file

  # AFER YOU HAVE CHECKED THE NAMES FOR CONSISTENCY, NEED TO DISAMBIGUATE
  # The best way to disambiguate is as follows: 
  # 1. assign a different index to entries with different First Initial+Last Name (there aren't too many of there)
  # 2. Search for all that have same index BUT different first name
  
  
  #############################################################
  # NAME DISAMBIGUATION & ASSIGNING UNIQUE ID NUMBER TO EACH EDITOR 
  ##############################################################
  
  source("Name.disambig.R")
  DisambigFile<-Name.disambig(ALLDATA)
  DisambigFile<-select(DisambigFile,-VOLUME,-ISSUE,-NOTES)
  write.csv(DisambigFile, file=" ------/DisambigList.csv", row.names = T) #export it as a csv file

  # Look over the DisambigFile and identify those that should have different editor_id numbers.  
  # Delete the editor_id from the one that needs a new one (ie Ånurag Agrawal and Aneil Agrawal have
  # editor_id "2".  Keep to for Anurage and leave a blank cell for Aneil's editor_id). Renumber the first column
  # from 1:nrows. call that column index then Save that as a csv file called FixList.csv
  # all columns must have a name

 FixList<-read.csv(file="-----/FixList.csv", dec=".", header = TRUE, sep = ",", check.names=FALSE )
 FixList<-select(FixList,FirstMiddleLast)
 FixList$editor_id<-seq(max(ALLDATA$editor_id+1), length.out=nrow(FixList), by = 1)
 FixList$FirstMiddleLast<-as.character(FixList$FirstMiddleLast)
 
# join the two together, if there is a real number (i.e., not NA) in the 2nd editor id column just replace it)
# HT: http://stackoverflow.com/questions/36433757/conditionally-replace-values-in-data-frame-from-separate-data-frame-in-r
# This saved me from the gnarly nested loop below
ALLDATA <- full_join(ALLDATA, FixList, by = "FirstMiddleLast", all = T) %>% mutate(editor_id = ifelse(is.na(editor_id.y), editor_id.x, editor_id.y)) %>% select(-editor_id.x,-editor_id.y)
rm(FixList)


