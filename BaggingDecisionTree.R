base::source("DatabaseManager.R")

base::set.seed(1L)

baggingDecisionTreeFrameCombo <- randomForest::randomForest(
  close ~ .,
  data = databaseFrameCombEstimatorsTrain,
  mtry = 1023L,
  replace = TRUE,
  importance = TRUE,
  ntree = 100L,
  nPerm = 1L
)

# Valores predichos para la muestra OOB
baggingDecisionTreeFrameCombo[["predicted"]]
 
# Importancia de variables
baggingDecisionTreeFrameCombo[["importance"]]

randomForest::varImpPlot(baggingDecisionTreeFrameCombo)

# Histograma MSE 
baggingDecisionTreeFrameCombo[["mse"]]
graphics::hist(baggingDecisionTreeFrameCombo[["mse"]])

# Error disminuye a medida que aumenta la cantidad de arboles
randomForest:::plot.randomForest(baggingDecisionTreeFrameCombo)

# Error de test
yhatBaggingDecisionTreeFrameCombo <- stats::predict(baggingDecisionTreeFrameCombo, 
                                                    newdata = databaseFrameCombEstimatorsTest)

base::plot(yhatBaggingDecisionTreeFrameCombo, databaseFrameCombEstimatorsTest$close)

# RMSE 78.87432
baggingDecisionTreeFrameComboRMSE <- base::sqrt(base::mean((yhatBaggingDecisionTreeFrameCombo - 
                                                            databaseFrameCombEstimatorsTest$close)^2))
