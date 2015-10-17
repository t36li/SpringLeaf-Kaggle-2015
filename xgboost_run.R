require(xgboost)
library(caret)
setwd("C:/Users/bli13/Documents/Kaggle")
rm(list=ls())
#source("data_prep.R")
load("Processed_Data.RData")

# Perform LASSO to select features to reduce dimensionality and resolve collinearity
control <- trainControl(method="repeatedcv", number=5, repeats=5)
# train the model
model <- train(x=data.matrix(train[,setdiff(names(train),'target')]), 
               y=data.matrix(train[,'target']), method="lasso", preProcess="scale", trControl=control)
# estimate variable importance
importance <- varImp(model, scale=FALSE)
# summarize importance
print(importance)

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
