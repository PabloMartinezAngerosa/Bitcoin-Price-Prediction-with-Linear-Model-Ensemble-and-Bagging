base::library(magrittr)
base::source("DatabaseManager.R")
base::source("LinearModelGeneralFit.R")


# Se genera para la base con 70% train y 30% test
# Ajustes con Modelos Lineales.
linearModelGeneralFit <- GetLinearModelGeneralFit(datapriceTrain, datapriceTest)
# RMSE 231.1647 linearModelFited = lm(close~ closeLag1 +  closeLag3 , data=databaseTrain)
bestRMSELinearModel   <- linearModelGeneralFit$RMSE[base::which.min(linearModelGeneralFit$RMSE)]

# Buscamos el porcentaje de aciertos de los combo de estimadores
# Carga base datos generada con 2^n predicciones por frame
# Esta base fue generada con DatabaseFrameComboEstimatorsLinearModels.R
databaseFrameCombEstimators <-  utils::read.csv("frameCombEstimators.csv")
totalEstimadoresEnBandas    <- 0
cota <- 5

for (i in base::c(1:base::nrow(databaseFrameCombEstimators))) {
  for (j in base::c(1:(base::ncol(databaseFrameCombEstimators)-1))) {
    close <- databaseFrameCombEstimators[i,ncol(databaseFrameCombEstimators)]
    closeEstimated <- databaseFrameCombEstimators[i, j]
    if (closeEstimated > (close - cota) && closeEstimated < (close + cota)) {
      totalEstimadoresEnBandas <- totalEstimadoresEnBandas + 1
      break
    }
  }
}

# Porcentaje de estimadores por frame que entre 2^n predicciones al menos 1 se encuentra
# entre las cotas
# cota = 5 -> 76.15005%
totalEstimadoresEnBandas/nrow(databaseFrameCombEstimators)

# Calcula RMSE de la media como estimador
acumMeanComboEstimadores<- 0
for (i in base::c(1:base::nrow(databaseFrameCombEstimators))) {
  close <- databaseFrameCombEstimators[i,ncol(databaseFrameCombEstimators)]
  acumCloseEstimated  <- 0
  for (j in base::c(1:(base::ncol(databaseFrameCombEstimators)-1))) {
    closeEstimated <- databaseFrameCombEstimators[i, j]
    acumCloseEstimated <- acumCloseEstimated + closeEstimated
  }
  meanEstimator <- acumCloseEstimated/(base::ncol(databaseFrameCombEstimators)-1)
  acumMeanComboEstimadores <- acumMeanComboEstimadores + ( meanEstimator - close )^2
}
RMSEMeanComboEstimadores <- base::sqrt(
                              acumMeanComboEstimadores / 
                              base::nrow(databaseFrameCombEstimators) 
                            )
RMSEMeanComboEstimadores









