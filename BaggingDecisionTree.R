base::source("DatabaseManager.R")

base::set.seed(1L)
#train <- base::sample(1L:base::nrow(boston), .75 * base::nrow(boston))


# mtry = 13 means that all 13 predictors should be used (ie: bagging)
# replace = TRUE means sampling with replacement
#   Other args to define sample are: strata, sampsize
# importance = TRUE means that predictor importance should be assessed
# ntree = 25 means that 25 trees will be grown
# nPerm = 1 (default) is the number of times OOB obs are permuted per tree for assessing variable importance
baggingDecisionTreeFrameCombo <- randomForest::randomForest(
  close ~ .,
  data = databaseFrameCombEstimatorsTrain,
  mtry = 1023L,
  replace = TRUE,
  importance = TRUE,
  ntree = 100L,
  nPerm = 1L
)

# 
# base::class(bag_boston)
# base::names(bag_boston)
# 
# # A randomForest object of type regression
# bag_boston[["type"]]
# 
# # Predicted values for the OOB samples
# bag_boston[["predicted"]]
# 
# # Varaible importance matrix
# bag_boston[["importance"]]
# 
# 
# # %IncMSE: mean decrease of accuracy in predictions on the OOB sample when a given variable is excluded from the model
# # IncNodePurity: total decrease in node impurity that results from splits over that variable, averaged over all trees
# #   for regression: mean impurity is measured by training RSS
# #   for classification: mean impurity is measured by deviance
# randomForest::importance(bag_boston)
# 
# 
# randomForest::varImpPlot(bag_boston)
# 
# 
# # For a regression problem (see classification below) the MSE 
# bag_boston[["mse"]]
# graphics::hist(bag_boston[["mse"]])
# 
# randomForest:::plot.randomForest(bag_boston)
# 

# Test error rate
yhatBaggingDecisionTreeFrameCombo <- stats::predict(baggingDecisionTreeFrameCombo, 
                                                    newdata = databaseFrameCombEstimatorsTest)

# base::plot(yhat_bag, boston[-train, ][["medv"]])
# graphics::abline(0, 1)

# RMSE 91.17014
baggingDecisionTreeFrameComboRMSE <- base::sqrt(base::mean((yhatBaggingDecisionTreeFrameCombo - 
                                                            databaseFrameCombEstimatorsTest$close)^2))
