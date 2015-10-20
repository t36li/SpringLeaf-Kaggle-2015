require(xgboost)
require(data.table)
rm(list=ls())
setwd("C:/Users/bli13/Documents/Kaggle")

# Read in training data
train = fread("train_final.csv", stringsAsFactors = F)
test = fread("test_final.csv", stringsAsFactors = F)
#stacked_col = fread("stacked_model_pred.csv", stringsAsFactors = F)

#remove first column (it is null)
dim(train); dim(test)
train[, V1 := NULL]; test[, V1 := NULL]
dim(train); dim(test)

## Cross Validation xgboost ###
### Randomly split data into 5 folds ###
### Fit xgboost into it ###
### perform testing results ###
for (optdep in seq(8,12,1)) {
  for (optcldwgt in seq(6,12,1)) {
    for (eta_i in seq(0.005,0.03,0.005)) {
      for (nrounds_i in seq(5000,15000,2500)) {
        res.cv <- xgb.cv(data=data.matrix(train[,setdiff(names(train),'target'), with =F]),
                         eta = eta_i,
                         gamma = 0,
                         label = train$target,
                         nrounds = nround_i,
                         min_child_weight = 8,
                         max_depth = 10,
                         colsample_bytree = 0.7,
                         subsample = 0.5,
                         scale_pos_weight=3.3,
                         objective = "binary:logistic",
                         metrics = "auc",
                         verbose = TRUE
        )
      }
    }
  }
}

eta_i=0.005
nround_i = 18000

model = xgboost(  data=data.matrix(train[,setdiff(names(train),'target'), with =F]),
                  eta = eta_i,
                  gamma = 0,
                  label = train$target,
                  nrounds = nround_i,
                  min_child_weight = 8,
                  max_depth = 10,
                  colsample_bytree = 0.7,
                  subsample = 0.5,
                  scale_pos_weight=3.3,
                  objective = "binary:logistic",
                  metrics = "auc",
                  verbose = TRUE
)

# create submission file
submission <- data.frame(ID=testID)
submission$target<-predict(model, newdata=data.matrix(test))
write.csv(submission, "xgboost_submission_final_Bob.csv",row.names = F)
