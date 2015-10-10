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
train.size=nrow(train)
train[1:6,1:5, with =F]
test[1:6,1:5, with =F]



#####################  Phase 1: Exploratory Data Analysis and Data Cleaning  ##############################
# delete ID column for both training and testing data set
train[, ID := NULL]; test[, ID := NULL]

# Feature names (ID removed already)
feature.names <- setdiff(names(train),"target")

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

print(factornum_df)
# for feature with more 15 levels: see what they are actually (I suspect they may be date)
diverse.features <- nonnumeric.featurenames[factornum_df[1,]>15]
print (diverse.features)
# print number of levels for each variable
print (factornum_df[,(diverse.features)])
# show these factor features
print (train[,(diverse.features),with=F])

# Store these features description
diverse.feature.description <- rep(NA,length(diverse.features))
names(diverse.feature.description) <-diverse.features
date.features <-c("VAR_0073","VAR_0075","VAR_0156","VAR_0157","VAR_0158","VAR_0159",
                  "VAR_0166","VAR_0167","VAR_0168","VAR_0169","VAR_0176","VAR_0177",
                  "VAR_0178","VAR_0179","VAR_0204","VAR_0204","VAR_0217")

for (f in date.features) {
     #     as.Date(train[[f]],"%d%b%Y")
     # Currently convert date time
     train[[f]] <- as.double(strptime(train[[f]], format='%d%b%y:%H:%M:%S', tz="UTC"))
}

print (train[,(date.features),with =F])
# Extract Hours for the columns with non-zero


print("remaining non-date variables:")
print(setdiff(diverse.features, date.features))
# [1] "VAR_0200" "VAR_0237" "VAR_0274" "VAR_0342" "VAR_0404" "VAR_0493"

# FEATURE: VAR_0200 individual cities
print(paste('number of different cities:',length(table(train[, VAR_0200]))))
# decide what levels/cities to keep for VAR_0200
VAR_0200.freq <- table(train[, VAR_0200])
print (VAR_0200.freq[VAR_0200.freq>train.size*0.005]) # returns CHICAGO
VAR_0200.freqlevels <- names(VAR_0200.freq[VAR_0200.freq>1000])
print (train[,VAR_0200]) # [1] "CHICAGO"      "HOUSTON"      "JACKSONVILLE" "SAN ANTONIO" 
replaced.VAR_0200col <- ifelse(!train[,VAR_0200]%in%VAR_0200.freqlevels,NA,train[,VAR_0200])
replaced.VAR_0200col_test <- ifelse(!test[,VAR_0200]%in%VAR_0200.freqlevels,NA,test[,VAR_0200])
print (sum(!is.na(replaced.VAR_0200col))/train.size) # only 3.76% are not NA, so may be we won't keep this feature
###!!!!!!!!!!!!!!!!! MAY EXCLUDE THIS FEATURE/and compare performance !!!!!!!!!!!!!!!!!!!###
######################## MAY map the city to its state ########################
length(train[,VAR_0200])
length(replaced.VAR_0200col)
train[ , VAR_0200 := replaced.VAR_0200col]
test[ , VAR_0200 := replaced.VAR_0200col_test]

# REMAINING: [1] "VAR_0237" "VAR_0274" "VAR_0342" "VAR_0404" "VAR_0493"
# FEATURE: VAR_0237 - states
# FEATURE: VAR_0274 - states
# FEATURE: VAR_0342 - states
table(train[,VAR_0237])
table(train[,VAR_0274])
table(train[,VAR_0342])
length(table(train[,VAR_0237])) # level "" + level of states 48
length(table(train[,VAR_0274])) # level -1 + level "" + level of states 56
length(table(train[,VAR_0342])) # level -1 + level "" + level of states 49
# Create 
state.features <- c("VAR_0237","VAR_0274","VAR_0342")
train[, (state.features), with = F]

# REMAINING: [1] "VAR_0404" "VAR_0493"
# two types: with job title and without job title
VAR_0404col<-train[,VAR_0404]
table(VAR_0404col)
sum(VAR_0404col==-1)+sum(VAR_0404col=="") # without job titles
replaced.VAR_0404col<-ifelse( ((VAR_0404col==-1)|(VAR_0404col=="")), "NoJobDesc", "JobDesc")
replaced.VAR_0404col_test<-ifelse( ((test[,VAR_0404]==-1)|(test[,VAR_0404]=="")), "NoJobDesc", "JobDesc")
train[ , VAR_0404 := replaced.VAR_0200col]
test[ , VAR_0404 := replaced.VAR_0404col_test]

VAR_0493col<-train[,VAR_0493]
table(train[,VAR_0493col])
sum(VAR_0493col==-1)+sum(VAR_0493col=="") # without job titles
replaced.VAR_0493col<-ifelse( ((VAR_0493col==-1)|(VAR_0493col=="")), "NoJobDesc", "JobDesc")
replaced.VAR_0493col_test <-ifelse( ((test[,VAR_0493]==-1)|(test[,VAR_0493]=="")), "NoJobDesc", "JobDesc")
train[ , VAR_0493 := replaced.VAR_0493col]
test[ , VAR_0493 := replaced.VAR_0404col_test]


jobtitle.features<-c("VAR_0404","VAR_0493")
train[, (jobtitle.features), with = F]

# VAR_0044 should be removed
train[, VAR_0044 := NULL]
test[, VAR_0044 := NULL]

# AFTER CLEANING THE DATA, CHECK DATA AGAIN
feature.types<-lapply(train,class)
nonnumeric.types<-feature.types[(feature.types!="integer")&(feature.types!="numeric")]
nonnumeric.featurenames<-names(nonnumeric.types)
train[,(nonnumeric.featurenames),with=F]

factornum_df <- data.frame(matrix(NA,nrow=1,ncol=length(nonnumeric.featurenames)))
names(factornum_df) <-nonnumeric.featurenames
for (feature in nonnumeric.featurenames) {
     factornum_df[1,(feature)] <- length(table(train[,(feature),with=F]))
}
