require(corrplot)
require(Rtsne)
require(xgboost)
require(stats)
require(knitr)
require(bit64)
require(ggplot2)
require(data.table)
require(graphics)
library(RCurl) # download https data
library(data.table)

rm(list=ls())
setwd("/Users/apple1/Github_Repo/Kaggle/SpringLeaf_Marketing")

# Read in training data
train = fread("./data/train.csv", stringsAsFactors = F)
test = fread("./data/test.csv", stringsAsFactors = F)

# show training and testing content
dim(train); dim(test)
train[1:6,1:5, with =F]
test[1:6,1:5, with =F]



#####################  Phase 1: Exploratory Data Analysis and Data Cleaning  ##############################
# delete ID column for both training and testing data set
train[, ID := NULL]; test[, ID := NULL]

# Feature names (ID removed already)
feature.names <- names(train)[(1:(ncol(train)-1))]

# Extract names of non-numeric features
feature.types<-lapply(train,class)
nonnumeric.types<-feature.types[(feature.types!="integer")&(feature.types!="numeric")]
nonnumeric.featurenames<-names(nonnumeric.types)
interger64.featurename <-nonnumeric.featurenames[nonnumeric.types=="integer64"]
logical.featurenames<-nonnumeric.featurenames[nonnumeric.types=="logical"]
# logical.data<-train[,logical.featurenames,with=F]
# sum(!is.na(logical.data)) 
# - return 0, which means that the logical typed features are useless

# delete the logical typed features
train[, (logical.featurenames) := NULL]
test[, (logical.featurenames) := NULL]

# delete the integer64 typed features (temporarily)
train[, (interger64.featurename) := NULL]
test[, (interger64.featurename) := NULL]

# Now, after removal of logical/64bit integer variables, again extract/update names of non-numeric features
feature.types<-lapply(train,class)
nonnumeric.types<-feature.types[(feature.types!="integer")&(feature.types!="numeric")]
nonnumeric.featurenames<-names(nonnumeric.types)
# double check and make sure all nonnumeric type variables are characters
sum(nonnumeric.types!="character") # should return 0


# a matrix that stores number of level for each "character" type variables
factornum_df <- data.frame(matrix(NA,nrow=1,ncol=length(nonnumeric.featurenames)))
names(factornum_df) <-nonnumeric.featurenames
for (feature in nonnumeric.featurenames) {
     factornum_df[1,(feature)] <- length(table(train[,(feature),with=F]))
}

# for feature with more 15 levels: see what they are actually (I suspect they may be date)
diverse.features <- nonnumeric.featurenames[factornum_df[1,]>15]
factornum_df[,(diverse.features)]
# show these factor features
print (train[,(diverse.features),with=F])

table(train[, VAR_0274])
length(table(train[, VAR_0274])) # level -1 + level "" + level: 

VAR_0200Freq <- table(train[, VAR_0200])
VAR_0200GreatFreq<-VAR_0200Freq[VAR_0200Freq>300]
length(VAR_0200GreatFreq)
length(table(train[, VAR_0200])) # individual cities