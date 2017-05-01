
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
  # FIRST MAKE ANY NEEDED CORRECTIONS TO YOUR RAW DATAFRAME
  #
  ######################################################
  ######################################################
  
str(DATASET)
summary(DATASET)
DATASET$JOURNAL<-as.factor(DATASET$JOURNAL)
#write.csv(DATASET, file="/Users/emiliobruna/Dropbox/EMB - ACTIVE/MANUSCRIPTS/Editorial Board Geography/DATASET.csv", row.names = T) #export it as a csv file

# THIS REMOVEAS A FEW WITH BLANKS IN THE NAMES
DATASET <-filter(DATASET, DATASET$FIRST_NAME!="" & DATASET$LAST_NAME!="")
# Error Correction

DATASET$FIRST_NAME<-as.character(DATASET$FIRST_NAME)
#####
str(DATASET)

# Make the data types consistent with ANyother dataset you might be joining this up with  
# DATASET$VOLUME<-as.integer(DATASET$VOLUME)
# DATASET$ISSUE<-as.integer(DATASET$ISSUE)


#Remove (trim) the leading and trailing white spaces (note can do with one command as per: http://stackoverflow.com/questions/2261079/how-to-trim-leading-and-trailing-whitespace-in-r)
trim.trailing <- function (x) sub("\\s+$", "", x)
DATASET$FIRST_NAME<-trim.trailing(DATASET$FIRST_NAME)
DATASET$MIDDLE_NAME<-trim.trailing(DATASET$MIDDLE_NAME)
DATASET$LAST_NAME<-trim.trailing(DATASET$LAST_NAME)
DATASET$TITLE<-trim.trailing(DATASET$TITLE)
DATASET$INSTITUTION<-trim.trailing(DATASET$INSTITUTION)

trim.leading <- function (x)  sub("^\\s+", "", x)
DATASET$FIRST_NAME<-trim.leading(DATASET$FIRST_NAME)
DATASET$MIDDLE_NAME<-trim.leading(DATASET$MIDDLE_NAME)
DATASET$LAST_NAME<-trim.leading(DATASET$LAST_NAME)
DATASET$TITLE<-trim.leading(DATASET$TITLE)
DATASET$INSTITUTION<-trim.leading(DATASET$INSTITUTION)

# remove any double spaces
DATASET$FIRST_NAME<-gsub("  ", " ", DATASET$FIRST_NAME)
DATASET$LAST_NAME<-gsub("  ", " ", DATASET$LAST_NAME, fixed=TRUE)
DATASET$MIDDLE_NAME<-gsub("  ", " ", DATASET$MIDDLE_NAME, fixed=TRUE)

# Remove the periods from peoples names to make consistent accross all files
DATASET$FIRST_NAME<-gsub("[.]","", DATASET$FIRST_NAME) # . is a wildcard, so [.] removes only the period.
DATASET$MIDDLE_NAME<-gsub(".", "", DATASET$MIDDLE_NAME, fixed=TRUE)
DATASET$LAST_NAME<-gsub(".", "", DATASET$LAST_NAME, fixed=TRUE)


# Add any missing years and volumes (THESE ARE EXAMPLES OF HOW
DATASET$VOLUME[DATASET$JOURNAL == "AMNAT" & DATASET$YEAR == "2007"] <- "169"
DATASET$VOLUME[DATASET$JOURNAL == "AMNAT" & DATASET$YEAR == "2008"] <- "171"
DATASET$ISSUE[DATASET$JOURNAL == "AMNAT" & is.na(DATASET$ISSUE)] <- 1
DATASET$VOLUME[DATASET$JOURNAL == "PLANTECOL" & DATASET$YEAR == "1993"] <- "104/105"
DATASET$YEAR[DATASET$JOURNAL == "LECO" & DATASET$YEAR == "1996" & DATASET$VOLUME == "10"] <- "1995"

# MIDDLE NAMES TO BE CORRECTED
DATASET$MIDDLE_NAME[DATASET$MIDDLE_NAME == "J A"] <- "JA"
DATASET$MIDDLE_NAME[DATASET$MIDDLE_NAME == "Richiard"] <- "Richard"
DATASET$MIDDLE_NAME[DATASET$FIRST_NAME == "H" & DATASET$LAST_NAME == "Godfray"] <- "CharlesJ" #WORKING?
DATASET$MIDDLE_NAME[DATASET$MIDDLE_NAME == "Paolo"] <- ""

# LAST NAMES TO BE CORRECTED
DATASET$LAST_NAME[DATASET$LAST_NAME == "Mueller" & DATASET$FIRST_NAME == "Caroline"] <- "Muller" #WORKING?
DATASET$LAST_NAME[DATASET$LAST_NAME == "Fairburn"] <- "Fairbairn"
DATASET$LAST_NAME[DATASET$LAST_NAME == "Abrecht"] <- "Albrecht"

# Cleaning up the titles 
# FIrst standardize them
# #remove extra spaces, converts to chr
# DATASET$TITLE<-gsub(" ", "", DATASET$TITLE, fixed=TRUE) 

DATASET$TITLE<-gsub("\\ ", ".", DATASET$TITLE) #Replace spaces with period
DATASET$TITLE<-gsub(":", "", DATASET$TITLE) #Replace : with period
DATASET$TITLE[DATASET$JOURNAL == "AMNAT" & DATASET$TITLE == "AE"] <- "Editorial.Board"
DATASET$TITLE[DATASET$TITLE == "Editor-in-Chief"] <- "EIC"
DATASET$TITLE[DATASET$TITLE == "Editor-In-Chief"] <- "EIC"
DATASET$TITLE[DATASET$TITLE == "Natural.HistoryEditor"] <- "Natural.History.Editor"
DATASET$TITLE[DATASET$TITLE == "Deputy.Editor-In-Chief"] <- "Deputy.EIC"

#Now correct the Categories that were incorrectly assigned

DATASET$CATEGORY<-as.character(DATASET$CATEGORY)
DATASET$CATEGORY[DATASET$JOURNAL == "AMNAT" & DATASET$TITLE == "Natural.History.Editor"] <- "SPECIAL"
DATASET$CATEGORY[DATASET$JOURNAL == "AMNAT" & DATASET$TITLE == "Editor" & (DATASET$YEAR >= 2005 & DATASET$YEAR < 2016)  ] <- "AE"
DATASET$TITLE[DATASET$JOURNAL == "AMNAT" & DATASET$LAST_NAME == "Whitlock" & DATASET$YEAR == 2005] <- "Editor"
DATASET$CATEGORY[DATASET$JOURNAL == "AMNAT" & DATASET$LAST_NAME == "Whitlock" & DATASET$YEAR == 2005] <- "AE"
DATASET$TITLE[DATASET$JOURNAL == "AMNAT" & DATASET$LAST_NAME == "Winn" & DATASET$YEAR == 2015] <- "Editorial.Board"
DATASET$CATEGORY[DATASET$JOURNAL == "AMNAT" & DATASET$LAST_NAME == "Winn" & DATASET$YEAR == 2015] <- "SE"
DATASET$CATEGORY[DATASET$TITLE == "Secretary"] <- "Production"
DATASET$CATEGORY[DATASET$TITLE == "Editorial.Office.Manager"] <- "Production"
DATASET$CATEGORY[DATASET$TITLE == "Production.Editor"] <- "Production"
DATASET$CATEGORY[DATASET$TITLE == "Technical.Editor" & DATASET$JOURNAL == "AGRONOMY"] <- "AE"
DATASET$CATEGORY[DATASET$TITLE == "Editorial.Board" & DATASET$JOURNAL == "AMNAT" ] <- "SE"
DATASET$CATEGORY[DATASET$TITLE == "Special.Editor" & DATASET$JOURNAL=="EVOL"] <- "SPECIAL" # NEED to 2x
DATASET$CATEGORY[DATASET$CATEGORY == "" & DATASET$JOURNAL=="EVOL"] <- "2xCheck" # NEED to 2x
DATASET$CATEGORY[DATASET$TITLE == "Guest.Editor"] <- "SPECIAL"

#Clean Up Categories
DATASET$CATEGORY<-as.character(DATASET$CATEGORY)
DATASET$CATEGORY[DATASET$CATEGORY == "Ae"] <- "AE"
DATASET$CATEGORY[DATASET$CATEGORY == "OTHER"] <- "other"
DATASET$CATEGORY[DATASET$CATEGORY == "Other"] <- "other"
DATASET$CATEGORY[DATASET$CATEGORY == "Other "] <- "other"
DATASET$CATEGORY[DATASET$CATEGORY == "EDITOR-IN-CHIEF"] <- "EIC"
DATASET$CATEGORY[DATASET$CATEGORY == "special"] <- "SPECIAL"

# Convert to factor and drop levels
DATASET$TITLE<-as.factor(DATASET$TITLE)
DATASET$TITLE<-droplevels(DATASET$TITLE)
DATASET$CATEGORY<-as.factor(DATASET$CATEGORY)
DATASET$CATEGORY<-droplevels(DATASET$CATEGORY)
# levels(DATASET$CATEGORY)
# levels(DATASET$TITLE) 


# Make Gender Consistent
DATASET$GENDER[DATASET$GENDER == "female"] <- "F"
DATASET$GENDER[DATASET$GENDER == "male"] <- "M"
DATASET$GENDER[DATASET$GENDER == "U"] <- NA
DATASET$GENDER[DATASET$GENDER == "Unkown"] <- NA
DATASET$GENDER[DATASET$GENDER == "Unknown"] <- NA
DATASET$GENDER[DATASET$GENDER == ""] <- NA
DATASET$GENDER<-droplevels(DATASET$GENDER)


# Correct some of the countries
DATASET$COUNTRY[DATASET$COUNTRY == "Austrailia"]  <- "Australia" #removing old names
DATASET$COUNTRY[DATASET$COUNTRY == "US"]  <- "USA" #removing old names


# Adding combinations of names to the database
# First Name, Last Name
DATASET$FirstLast<-paste(DATASET$FIRST_NAME,DATASET$LAST_NAME, sep=" ") 
# First Name, Middle Name, Last Name
DATASET$FirstMiddleLast<-paste(DATASET$FIRST_NAME,DATASET$MIDDLE_NAME,DATASET$LAST_NAME, sep=" ")
# First initial 1st name + last name": 
DATASET$FIRST_INIT<-as.character(DATASET$FIRST_NAME)
DATASET$FIRST_INIT<-substring(DATASET$FIRST_INIT,1,1)
DATASET$FirstInitialLast<-paste(DATASET$FIRST_INIT,DATASET$LAST_NAME, sep=" ")
DATASET$FIRST_INIT<-NULL #delete it out now that we don't need it
#Delete column with suffix
DATASET$SUFFIX<-NULL #delete it out now that we don't need it


# Remove the periods from peoples names to make consistent accross all files
DATASET$FirstLast<-gsub("  ", " ", DATASET$FirstLast)
DATASET$FirstMiddleLast<-gsub("  ", " ", DATASET$FirstMiddleLast)
DATASET$FirstInitialLast<-gsub("  ", " ", DATASET$FirstInitialLast)

# 
DATASET_clean<-DATASET



  ######################################################
  ######################################################

  
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
  NameSimilarityDF<-Name.check(DATASET_clean,DATASET_clean$FirstMiddleLast)
  write.csv(NameSimilarityDF, file="------/NameCheck_ALLDATA_ALLYRS.csv", row.names = T) #export it as a csv file

  # AFER YOU HAVE CHECKED THE NAMES FOR CONSISTENCY, NEED TO DISAMBIGUATE
  # The best way to disambiguate is as follows: 
  # 1. assign a different index to entries with different First Initial+Last Name (there aren't too many of there)
  # 2. Search for all that have same index BUT different first name
  
  
  #############################################################
  # NAME DISAMBIGUATION & ASSIGNING UNIQUE ID NUMBER TO EACH EDITOR 
  ##############################################################
  
  source("Name.disambig.R")
  DisambigFile<-Name.disambig(DATASET_clean)
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
DATASET_clean <- full_join(DATASET_clean, FixList, by = "FirstMiddleLast", all = T) %>% mutate(editor_id = ifelse(is.na(editor_id.y), editor_id.x, editor_id.y)) %>% select(-editor_id.x,-editor_id.y)
rm(FixList)


