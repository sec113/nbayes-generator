# Copyright ? 2016 by Sergio Castro
# All rights reserved. This document or any portion thereof
# may not be reproduced or used in any manner whatsoever
# without the express written permission of the author.
#
# Title   : BI-RADS Naive Bayes Database Generator
# Author  : Sergio Castro
# Created : 25/05/2016
# Comment : input:  (1) CSV document with bag of words from the whole testing set but not line_numbers or birads
#           input:  (2) CSV document with the rest of the features
#           output: (1) database ready to go to WEKA

# List of packages for session
options( java.parameters = "-Xmx4g" )
.packages = c('foreign','RWeka','data.table','base')
# Install CRAN packages (if not already installed)
.inst = .packages %in% installed.packages()
if(length(.packages[!.inst]) > 0) install.packages(.packages[!.inst])
# Load packages into session 
invisible(lapply(.packages, library, character.only=TRUE))

# Load database with the Bag of Words
train_db_BagOfWord = read.csv("C:/Users/sec113/Downloads/birads/db_folder/training_file.txt", sep = "\t")

#Load database with the BI-RADS and other features
train_db_Birads = read.csv("C:/Users/sec113/Downloads/birads/db_folder/training_birads.txt", sep = "\t")

#Copy columns from one table to another 1:6 in the birads counter. 1:5 in the normal version
train_db_BagOfWord[,1:6] = train_db_Birads[,1:6]

##Correcting the difference between 4a and 4A
db_Bline_only = train_db_BagOfWord
db_Bline_only$BiradsNumber = as.character(db_Bline_only$BiradsNumber)
db_Bline_only$BiradsNumber[db_Bline_only$BiradsNumber == "4a"] = "4A"
db_Bline_only$BiradsNumber = as.factor(db_Bline_only$BiradsNumber)

train_db_BagOfWord[,3] = db_Bline_only[,3]

#Check everything worked as planned
table(train_db_BagOfWord[,3])
table(db_Bline_only[,3])
rm(db_Bline_only)

# Filter by NoBirads
dataf=train_db_BagOfWord[train_db_BagOfWord$BiradsCategory!="NoBirads",]

###Only for the Birads Counter Version
dataf$BiradsCounter = as.numeric(dataf$BiradsCounter)

dataf=within(dataf,{
  BiradsCounter <-ave (BiradsCounter, DocumentID, FUN = cumsum)
})

####BIRADS RANKER
temp=train_db_BagOfWord[train_db_BagOfWord$BiradsCategory!="NoBirads",]
temp=temp[,1:6]
temp[,4]=as.numeric(temp[,4])
temp=within(temp,{
  BiradsCounter <-ave (BiradsCounter, DocumentID, FUN = cumsum)
})

temp[,7]=temp[,4]
ids=unique(temp$DocumentID)


for (id in ids){
  indices = which(temp$DocumentID == id)
  biradsRanking = temp[,4]
  degree = length(indices)
  if (degree==1){
    next
  } 
  else{
    if (degree ==2){
      temp[indices[degree],7] = "Last"
    }
    else{
      for (i in 2:(degree-1)){
        temp[indices[i],7] = "Middle"
      }
      temp[indices[degree],7] = "Last"
    }
  }
}

temp[temp$BiradsCounter==1,7]="First"
dataf$BiradsRank=temp[,7]
rm(temp)
rm(biradsRanking)
rm(degree)
rm(ids)
rm(id)

#####
#Filter features
dataf=as.data.frame(dataf)
dataf=dataf[sapply(dataf, function(x) length(unique(x))>1)]


#Change all 0's and 1's to FALSE and TRUE 4 in normal version 5 in this version
dataf[,5:length(dataf)][dataf[,5:length(dataf)] == 0] <- "F"
dataf[,5:length(dataf)][dataf[,5:length(dataf)] == 1] <- "T"

#Export file
write.table(dataf, file = "C:/Users/sec113/Downloads/birads/bayes_ftr/dataf.txt", sep = "\t",row.names=FALSE,quote = FALSE)
dataf = read.csv("C:/Users/sec113/Downloads/birads/bayes_ftr/dataf.txt", sep = "\t")

#columns to drop
coltodrop = c('DocumentID','bagofTokens','Line','X0','X1','X2','X3','X4','X4a','X4b','X4c','X5','X6')
dataf = dataf[,-which(names(dataf) %in% coltodrop)]



write.arff(dataf,file = "C:/Users/sec113/Downloads/birads/bayes_ftr/bayes_train.arff")



--------------------------------------------------------------------------------------------------------------------------------
  
  
  # Import the dev_reports file 
  dev_files_db = read.csv("C:/Users/sec113/Downloads/birads/db_folder/test_xmi.txt", sep = "\t")
dev_birads = read.csv("C:/Users/sec113/Downloads/birads/db_folder/dev_set_birads.txt", sep = "\t")

#Copy columns from one table to another
#dev_files_db[,1:4] = dev_birads[,1:4]
dev_files_db[,1:5] = dev_birads[,1:5] #1:5 in the counter version

##Correct the difference between 4a and 4A
temp_dv = dev_files_db
temp_dv$BiradsNumber = as.character(temp_dv$BiradsNumber)
temp_dv$BiradsNumber[temp_dv$BiradsNumber == "4a"] = "4A"
temp_dv$BiradsNumber = as.factor(temp_dv$BiradsNumber)

dev_files_db$BiradsNumber = temp_dv$BiradsNumber

rm(temp_dv)

# Filter by NoBirads
dev_only = dev_files_db[dev_files_db$BiradsCategory!= "NoBirads",]

###Only for the Birads Counter Version
dev_only$BiradsCounter = as.numeric(dev_only$BiradsCounter)

dev_only=within(dev_only,{
  BiradsCounter <-ave (BiradsCounter, DocumentID, FUN = cumsum)
})

####BIRADS RANKER

temp=dev_only[dev_only$BiradsCategory!="NoBirads",]
temp=temp[,1:6]
temp[,4]=as.numeric(temp[,4])
temp=within(temp,{
  BiradsCounter <-ave (BiradsCounter, DocumentID, FUN = cumsum)
})

temp[,7]=temp[,4]
ids=unique(temp$DocumentID)


for (id in ids){
  indices = which(temp$DocumentID == id)
  biradsRanking = temp[,4]
  degree = length(indices)
  if (degree==1){
    next
  } 
  else{
    if (degree ==2){
      temp[indices[degree],7] = "Last"
    }
    else{
      for (i in 2:(degree-1)){
        temp[indices[i],7] = "Middle"
      }
      temp[indices[degree],7] = "Last"
    }
  }
}

temp[temp$BiradsCounter==1,7]="First"
dev_only$BiradsRank=temp[,7]
rm(temp)
rm(biradsRanking)
rm(degree)
rm(ids)
rm(id)

######

# Add the columns from the training dataset to a list to then create the rest of the columns
# get the column headers from the training dataset
col_headers = c(colnames(dataf))
col_headers = as.factor(col_headers)
#replace mis-spelled headers by correct ones in a list to be used for the reg.exp
header_list = gsub('bi.rads','bi-rads',col_headers)
header_list= gsub('bi.rad','bi-rad',col_headers)


#create new dataframe with the headers
header_table = data.frame(x=1:length(dev_only[,1]))

#fill the dataframe with the bag of words
for (header in header_list){
  a = paste(cbind("\\b",header,'\\b'),collapse='')
  x = as.numeric(grepl(a,dev_only[,6]))
  header_table = cbind(header_table,data.frame(x=x))
}
colnames(header_table) = c('nada',header_list)
header_table = header_table[,-1]
colnames(header_table) = col_headers

#merge the 2 dataframes
final_dev= cbind(dev_only,header_table[!names(header_table) %in% names(dev_only)])
testing_set = final_dev[,-which(names(final_dev) %in% coltodrop)]
rm(header_table)

#Change all 0's and 1's to FALSE and TRUE
testing_set[,4:length(testing_set)][testing_set[,4:length(testing_set)] == 0] <- "F"
testing_set[,4:length(testing_set)][testing_set[,4:length(testing_set)] == 1] <- "T"

#Export files
write.table(testing_set, file = "C:/Users/sec113/Downloads/birads/bayes_ftr/testing_set.txt", sep = "\t",
            row.names=FALSE,quote = FALSE)
testing_set = read.csv("C:/Users/sec113/Downloads/birads/bayes_ftr/testing_set.txt", sep = "\t")

write.arff(testing_set,file = "C:/Users/sec113/Downloads/birads/bayes_ftr/bayes_test_set.arff")

