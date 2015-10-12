require(xgboost)
setwd("/Users/apple1/Github_Repo/Kaggle/SpringLeaf_Marketing")
rm(list=ls())
#source("data_prep.R")
load("Processed_Data.RData")

# For cross-validation only (this does not train the model):
# cv.res = xgb.cv(data= data.matrix(train[,setdiff(names(train),'target')]),
#                 nfold = 5,
#                 eta = 0.01,
#                 label = train$target,
#                 nround = 1000,
#                 min_child_weight = 10,
#                 max_depth = 8,
#                 colsample_bytree = 0.8,
#                 subsample = 0.7,
#                 objective = "binary:logistic",
#                 eval_metric = "auc"
# )

model = xgboost(data= data.matrix(train[,setdiff(names(train),'target')]),
                eta = 0.1,
                label = train$target,
                nround = 10,
                #           min_child_weight = 10,
                max_depth = 8,
                colsample_bytree = 0.8,
                subsample = 0.7,
                objective = "binary:logistic",
                eval_metric = "auc"
)

# create submission file
submission <- data.frame(ID=testID)
submission$target<-predict(model, newdata=data.matrix(test))
write.csv(submission, "xgboost_submission_crap.csv",row.names = F)
