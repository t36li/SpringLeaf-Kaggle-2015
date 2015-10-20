# require(corrplot)
# require(Rtsne)
# require(xgboost)
# require(stats)
# require(knitr)
# require(bit64)
# require(ggplot2)
require(data.table)
# require(graphics)
# library(RCurl) # download https data
library(data.table)
library(lubridate)

## NOTE: the part where we may try something different will be marked with comment as follows:
## ** TODO: try option 1: @#$#@!@# (done)
##          try option 2: @#$#@!@# (to do)
##          try option 3: @#$#@!@# (to do)

## Just just search for the key word: TODO to jump to the place where we would like to try out different options

rm(list=ls())
setwd("C:/Users/bli13/Documents/Kaggle")

# Read in training data
train = fread("train.csv", stringsAsFactors = F)
test = fread("test.csv", stringsAsFactors = F)

# show training and testing content
dim(train); dim(test)
train.size=nrow(train)
train[1:6,1:5, with =F]
test[1:6,1:5, with =F]


testID = test[,ID]
#####################  Phase 1: Exploratory Data Analysis and Data Cleaning  ##############################
# delete ID column for both training and testing data set
train[, ID := NULL]; test[, ID := NULL]

# Feature names (ID removed already)
feature.names <- setdiff(names(train),"target")
# Extract names of non-numeric features
feature.types <- lapply(train,class)
nonnumeric.types <- feature.types[(feature.types!="integer")&(feature.types!="numeric")]
nonnumeric.featurenames <- names(nonnumeric.types)
interger64.featurename <- nonnumeric.featurenames[nonnumeric.types=="integer64"]
logical.featurenames <- nonnumeric.featurenames[nonnumeric.types=="logical"]
# logical.data<-train[,logical.featurenames,with=F]
# sum(!is.na(logical.data)) 
# - return 0, which means that the logical typed features are useless

# Logical features: "VAR_0207" "VAR_0213" "VAR_0840"
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
# Why 15 levels here???
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
                  "VAR_0178","VAR_0179","VAR_0204","VAR_0217")
print (train[,(date.features),with=F])

## ======================== PROCESSING DATE FEATURES ============================
## ** TODO: try option 1: convert date time into continuous numeric value (done)
##          try option 2: convert date with 00:00 into mday, month, year 3 columns,
##                        and date with specific time (i.e., non-midnight time)
##                        mday, month, year, hour 4 different columns     (to do)
##          try option 3: based on either option 1 or option 2, add weekday boolean 
##                        as additional feature                           (to do)

DateFeatureConvertOption=2
if (DateFeatureConvertOption==1) {
  ## IMPlEMENTED OPTION 1 (begin)
  for (f in date.features) {
    # Currently convert date time
    train[[f]] <- as.double(strptime(train[[f]], format='%d%b%y:%H:%M:%S', tz="UTC"))
    test[[f]] <- as.double(strptime(test[[f]], format='%d%b%y:%H:%M:%S', tz="UTC"))
  }
  print (train[,(date.features), with = F])
  ## IMPlEMENTED OPTION 1 (end)
}

if (DateFeatureConvertOption==2) {
  ##  OPTION 2 (begin)
  ############ Option 2: Extract Hours for the columns with non-00:00:00 time #############
  print (paste('Column count before date feature:',ncol(train)))
  new.date.features <- c()
  for (f in date.features) {
    current.time.train <- as.Date(strptime(train[[f]], format='%d%b%y:%H:%M:%S', tz="UTC"))
    train[[paste(f,'year',sep="_")]]<-year(current.time.train)
    train[[paste(f,'month',sep="_")]]<-month(current.time.train)
    train[[paste(f,'day',sep="_")]]<-day(current.time.train)
    
    current.time.test <- as.Date(strptime(test[[f]], format='%d%b%y:%H:%M:%S', tz="UTC"))
    test[[paste(f,'year',sep="_")]]<-year(current.time.test)
    test[[paste(f,'month',sep="_")]]<-month(current.time.test)
    test[[paste(f,'day',sep="_")]]<-day(current.time.test)
    new.date.features<-c(new.date.features,paste(f,'year',sep="_"),paste(f,'month',sep="_"),paste(f,'day',sep="_"))
  }
  print (train[,(new.date.features),with=F])
  #### Remove old date feature columns ###
  train<-train[,setdiff(names(train),date.features),with=F]
  test<-test[,setdiff(names(test),date.features),with=F]
  print (paste('Column count after date feature:',ncol(train)))
  ##  OPTION 2 (end)
}

##Call garbage collection to clean memory ###
gc()

print("remaining non-date variables:")
print(setdiff(diverse.features, date.features))
# [1] "VAR_0200" "VAR_0237" "VAR_0274" "VAR_0342" "VAR_0404" "VAR_0493"

## ======================== PROCESSING CITIES FEATURES ============================
## ** TODO: try option 1: only keep levels with frequency > 1000 (done)
##              option 2: only keep levels with frequency > 800 (to do)
##              option 2: only keep levels with frequency > 500 (to do)
## Removed all other cities... only kept cities with freq > 1000
## Big cities more likely to respond?

# FEATURE: VAR_0200 individual cities
print(paste('number of different cities:',length(table(train[, VAR_0200]))))
# decide what levels/cities to keep for VAR_0200
VAR_0200.freq <- table(train[, VAR_0200])
print (VAR_0200.freq[VAR_0200.freq>train.size*0.005]) # returns CHICAGO

################ Parameter here: city cut off criteria 1000, 800, 500 ################
## IMPlEMENTED OPTION 1 (begin)
VAR_0200.freqlevels <- names(VAR_0200.freq[VAR_0200.freq>1000])
## IMPlEMENTED OPTION 1 (end)

print (train[,VAR_0200.freqlevels]) # [1] "CHICAGO"      "HOUSTON"      "JACKSONVILLE" "SAN ANTONIO" 
replaced.VAR_0200col <- ifelse(!train[,VAR_0200]%in%VAR_0200.freqlevels,-1,train[,VAR_0200])
replaced.VAR_0200col_test <- ifelse(!test[,VAR_0200]%in%VAR_0200.freqlevels,-1,test[,VAR_0200])
#print (sum(!(replaced.VAR_0200col=="-1"))/train.size) # only 3.76% are not NA, so may be we won't keep this feature, will see
train[ , VAR_0200 := replaced.VAR_0200col]
test[ , VAR_0200 := replaced.VAR_0200col_test]

# REMAINING: [1] "VAR_0237" "VAR_0274" "VAR_0342" "VAR_0404" "VAR_0493"
# FEATURE: VAR_0237 - states
# FEATURE: VAR_0274 - states
# FEATURE: VAR_0342 - states
state.features <- c("VAR_0237","VAR_0274","VAR_0342")
# table(train[,VAR_0237])
# table(train[,VAR_0274])
# table(train[,VAR_0342])
# length(table(train[,VAR_0237])) # level "" + level of states 48
# length(table(train[,VAR_0274])) # level -1 + level "" + level of states 56
# length(table(train[,VAR_0342])) # level -1 + level "" + level of states 49

# replace "" in state with -1
train[,VAR_0237 := (ifelse(train[,VAR_0237]=="",-1,train[,VAR_0237]))]
train[,VAR_0274 := (ifelse(train[,VAR_0274]=="",-1,train[,VAR_0274]))]
train[,VAR_0342 := (ifelse(train[,VAR_0342]=="",-1,train[,VAR_0342]))]
test[,VAR_0237 := (ifelse(test[,VAR_0237]=="",-1,test[,VAR_0237]))]
test[,VAR_0274 := (ifelse(test[,VAR_0274]=="",-1,test[,VAR_0274]))]
test[,VAR_0342 := (ifelse(test[,VAR_0342]=="",-1,test[,VAR_0342]))]
train[, (state.features), with = F]

# REMAINING: [1] "VAR_0404" "VAR_0493"
# two types: with job title and without job title
# !!! Replaced occupations with just two levels
jobtitle.features<-c("VAR_0404","VAR_0493")
VAR_0404col<-train[,VAR_0404]
sum(VAR_0404col==-1)+sum(VAR_0404col=="") # without job titles
replaced.VAR_0404col<-ifelse( ((VAR_0404col==-1)|(VAR_0404col=="")), "NoJobDesc", "JobDesc")
replaced.VAR_0404col_test<-ifelse( ((test[,VAR_0404]==-1)|(test[,VAR_0404]=="")), "NoJobDesc", "JobDesc")
train[ , VAR_0404 := replaced.VAR_0404col]
test[ , VAR_0404 := replaced.VAR_0404col_test]
VAR_0493col<-train[,VAR_0493]
table(train[,VAR_0493col])
sum(VAR_0493col==-1)+sum(VAR_0493col=="") # without job titles
replaced.VAR_0493col<-ifelse( ((VAR_0493col==-1)|(VAR_0493col=="")), "NoJobDesc", "JobDesc")
replaced.VAR_0493col_test <-ifelse( ((test[,VAR_0493]==-1)|(test[,VAR_0493]=="")), "NoJobDesc", "JobDesc")
train[ , VAR_0493 := replaced.VAR_0493col]
test[ , VAR_0493 := replaced.VAR_0493col_test]
train[, (jobtitle.features), with = F]

# VAR_0044 should be removed - all empty
train[, VAR_0044 := NULL]
test[, VAR_0044 := NULL]

gc()

## ===================== REPLACING MISSING NUMERIC DATA  =========================
## ** TODO: try option 1: replace with -1
##          try option 2: replace with median (Using this)
##          try option 3: replace with nearest neighbor or K nearest neighbor

replaceMissingValueOption=2

if (replaceMissingValueOption==1) {
  ## IMPlEMENTED OPTION 1 (begin)
  cat("replacing missing values with -1\n")
  for (f in setdiff(names(train),"target")) {
    if (class(train[[f]])!="character") {
      temp_train_col = ifelse(is.na(train[[f]]),-1,train[[f]])
      temp_test_col = ifelse(is.na(test[[f]]),-1,test[[f]])
      train[, (f):= temp_train_col]
      test[, (f):= temp_test_col]
    }
  }
  ## IMPlEMENTED OPTION 1 (end)
}

if (replaceMissingValueOption==2) {
  ## IMPlEMENTED OPTION 2 (begin)
  cat("replacing missing values with median\n")
  for (f in setdiff(names(train),"target")) {
    if (class(train[[f]])!="character") {
      train_median_col<-as.numeric(train[[f]])
      train_median_col[is.na(train_median_col)]=median(train_median_col, na.rm=TRUE)
      test_median_col<-as.numeric(test[[f]])
      test_median_col[is.na(test_median_col)]=median(test_median_col, na.rm=TRUE)
      train[, (f):= train_median_col]
      test[, (f):= test_median_col]
    }
  }
  ## IMPlEMENTED OPTION 2 (end)
}

# convert character variable to factor/categorical
for (f in setdiff(names(train),"target")) {
  if (class(train[[f]])=="character") {
    train[[f]] = as.factor(train[[f]])
    test[[f]] = as.factor(test[[f]])
  }
}

# Now missing data are all filled.
sum(is.na(train)) # 0
sum(is.na(test)) # 0

# ======= CHECK DATA AGAIN (for checking purpose only) ============
feature.types<-lapply(train,class)
nonnumeric.types<-feature.types[(feature.types!="integer")&(feature.types!="numeric")]
nonnumeric.featurenames<-names(nonnumeric.types)
train[,(nonnumeric.featurenames),with=F]

factornum_df <- data.frame(matrix(NA,nrow=1,ncol=length(nonnumeric.featurenames)))
names(factornum_df) <-nonnumeric.featurenames
for (feature in nonnumeric.featurenames) {
  factornum_df[1,(feature)] <- length(table(train[,(feature),with=F]))
}
# make sure only three type of classes "integer", "factor", "numeric" exist
feature.types<-lapply(train,class)
length(feature.types)
sum(feature.types=="integer")+sum(feature.types=="factor")+sum(feature.types=="numeric")

# ==== convert data table to data frame, simply because I want to get something done quickly, 
# ==== and I'm a lot more comfortable with data.frame.

### remove unused variables ###
rm(list=setdiff(ls(),c("test","train","testID")))
train<-data.frame(train)
test<-data.frame(test)

# ============= replace "-1" and "" in categorical data with label "other"  ==================
for (i in names(train)) {
  if (class(train[,i])=="factor") {
    temp = as.character(train[,i])
    temp_test = as.character(test[,i])
    temp = ifelse(((temp=="")|(temp=="-1")),"other",temp)
    temp_test = ifelse(((temp_test=="")|(temp_test=="-1")),"other",temp_test)
    train[,i]=as.factor(temp)
    test[,i]=as.factor(temp_test)
  }
}

gc()

# ================== for categorical variables with only two levels, =========================
# === remove it if the single dominant level makes up over 99.5% (close to zero variance) ====

### count number of factor columns ###
feature.types<-lapply(train,class)
sum(feature.types=="factor")

nearZeroVarFeatures<-c()
print (paste('Column count before cutoff:',ncol(train)))
for (i in names(train)) {
  if (class(train[,i])=="factor") {
    #print(paste("========= factor:",i,"============")) 
    temp = as.character(train[,i])
    #print (unique(temp))
    #print (table(temp))
    # if only 2 variables, look at ratio minus label count/total label count
    if (length(unique(temp))==2) {
      if (min(table(temp))/nrow(train)*100<0.5)
        nearZeroVarFeatures<-c(nearZeroVarFeatures,i)
    }
  }
}
# remove these near zero variance categorical features
train<-train[,setdiff(names(train),nearZeroVarFeatures)]
test<-test[,setdiff(names(test),nearZeroVarFeatures)]
print (paste('Column count after cutoff:',ncol(train)))

######### There is also a way to remove zero variance numeric features #########
### a function called nearZeroVar - x must be all numeric ###
# library(caret)
# print (paste('Column count before cutoff:',ncol(train)))
# nzv <- nearZeroVar(train, saveMetrics = TRUE)
# head(nzv,100) 
# #dim(nzv[nzv$percentUnique>0.1,])
# percentUniqueThres=0.05
# selectedFeatures<-rownames(nzv[nzv$percentUnique>percentUniqueThres,])
# train<-train[,selectedFeatures]
# print (paste('Column count after cutoff:',ncol(train)))

########============= The most important step!!! creating dummy variables =======########
#Create dummy variable (with base level removed)/One-hot encoding
print (paste('Column count before One-Hot Encoding:',ncol(train)))
for (f in names(train)) {
  if (class(train[,f])=="factor") {
    alllevels= as.character(unique(train[,f]))
    levelsWithoutBase = alllevels[(1:(length(alllevels)-1))]
    #           print (paste("====== ",f,"======"))
    #           print (alllevels)
    #           print (levelsWithoutBase)
    for (level in levelsWithoutBase) {
      train[paste(f,level,sep="_")]<-ifelse(train[,f]==level,1,0)
      test[paste(f,level,sep="_")]<-ifelse(test[,f]==level,1,0)
    }
  }
}
# Remove categorical data now since dummy variable are all created 
factorFeatures<-names(train)[unlist(lapply(train,class))=="factor"]
train<-train[,setdiff(names(train),factorFeatures)]
test<-test[,setdiff(names(test),factorFeatures)]
print (paste('Column count after One-Hot Encoding:',ncol(train)))

# Double Check
dim(train)
dim(test)
unique(unlist(lapply(train,class)))
unique(unlist(lapply(test,class)))
# all variable names are the same
all(setdiff(names(train),"target")==names(test))
# all variable types match
all(unlist(lapply(train[,setdiff(names(train),"target")],class))==unlist(lapply(test,class)))

##Obtain column name for dummy (0 or 1 value) columns


rm(list=setdiff(ls(),c("test","train","testID")))
#save(train,test,testID,file="Processed_Data.RData")
write.csv(train,file="train_clean.csv")
write.csv(test,file="test_clean.csv")
write.csv(testID,file="test_ID.csv")
