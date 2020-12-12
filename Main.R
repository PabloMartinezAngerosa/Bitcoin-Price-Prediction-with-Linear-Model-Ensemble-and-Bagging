base::library(magrittr)
base::source("DatabaseManager.R")
base::source("LinearModelGeneralFit.R")
base::source("Bagging.R")


# Se genera para la base con 70% train y 30% test
# Ajustes con Modelos Lineales.
linearModelGeneralFit <- GetLinearModelGeneralFit(datapriceTrain, datapriceTest)
# RMSE 231.1647 linearModelFited = lm(close~ closeLag1 +  closeLag3 , data=databaseTrain)
bestRMSELinearModel   <- linearModelGeneralFit$RMSE[base::which.min(linearModelGeneralFit$RMSE)]

# Buscamos el porcentaje de aciertos de los combo de estimadores
# Carga base datos generada con 2^n predicciones por frame
# Esta base fue generada con DatabaseFrameComboEstimatorsLinearModels.R
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
totalEstimadoresEnBandas/base::nrow(databaseFrameCombEstimators)

# Calcula RMSE de la media experta de opiniones como estimador
acumMeanComboEstimadores<- 0
for (i in base::c(1:base::nrow(databaseFrameCombEstimatorsTest))) {
  close <- databaseFrameCombEstimatorsTest[i,ncol(databaseFrameCombEstimatorsTest)]
  acumCloseEstimated  <- 0
  for (j in base::c(1:(base::ncol(databaseFrameCombEstimatorsTest)-1))) {
    closeEstimated <- databaseFrameCombEstimatorsTest[i, j]
    acumCloseEstimated <- acumCloseEstimated + closeEstimated
  }
  meanEstimator <- acumCloseEstimated/(base::ncol(databaseFrameCombEstimatorsTest)-1)
  acumMeanComboEstimadores <- acumMeanComboEstimadores + ( meanEstimator - close )^2
}
RMSEMeanComboEstimadores <- base::sqrt(
                              acumMeanComboEstimadores / 
                              base::nrow(databaseFrameCombEstimatorsTest) 
                            )
RMSEMeanComboEstimadores

# Utilizamos Bagging para media experta de opinones como estimador
# Se recomienda un 60% de las observaciones por bolsa
nInBag = (ncol(databaseFrameCombEstimatorsTest)  - 1 )  * 0.6
# Generamos 1000 bolsas
nBag <- 1000
acumRMSEBagging <- 0

for (i in base::c(1:base::nrow(databaseFrameCombEstimatorsTest))) {
  close <- databaseFrameCombEstimatorsTest[i,ncol(databaseFrameCombEstimatorsTest)]
  data  <- databaseFrameCombEstimatorsTest[i, 1:(ncol(databaseFrameCombEstimatorsTest)-1)]   
  baggingPrediction <- DoBagging(data, nBag, nInBag)
  acumRMSEBagging   <- acumRMSEBagging  + ( baggingPrediction - close )^2
  print(i)
}
RMSEBaggingComboEstimadores <- base::sqrt(
  acumRMSEBagging / base::nrow(databaseFrameCombEstimatorsTest) 
)
RMSEBaggingComboEstimadores









