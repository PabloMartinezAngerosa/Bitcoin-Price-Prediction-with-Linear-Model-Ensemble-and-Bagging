base::library(magrittr)
base::source("DatabaseManager.R")
base::source("LinearModelGeneralFit.R")


# Se genera para la base con 70% train y 30% test
# Ajustes con Modelos Lineales.
linearModelGeneralFit <- GetLinearModelGeneralFit(datapriceTrain, datapriceTrain)
# RMSE 235.2711 linearModelFited = lm(close ~ closeLag1 +  closeLag3 + volLag2 + volLag3, data=databaseTrain)
bestRMSELinearModel   <- linearModelGeneralFit$RMSE[base::which.min(linearModelGeneralFit$RMSE)]

linearModelFited = lm(close ~ closeLag1 +  closeLag3 + volLag2 + volLag3, data = datapriceTrain)
predDataprice <- stats::predict(linearModelFited, datapriceTest, interval = "prediction")
# RMSE con la muestra de testeo 231.4834
linearModelRMSEDataprice <- base::sqrt(base::sum((predDataprice -
                             datapriceTest$close )^2)/base::length(datapriceTest$close))

# Buscamos el porcentaje de aciertos de los combo de estimadores
# Carga base datos generada con 2^n predicciones por frame
# Esta base fue generada con DatabaseFrameComboEstimatorsLinearModels.R
totalEstimadoresEnBandas <- 0
cota <- 5
colData <- base::ncol(databaseFrameCombEstimators)-1
closeIndex <- base::ncol(databaseFrameCombEstimators)
rowData <- base::nrow(databaseFrameCombEstimators)

for (i in base::c(1:rowData)) {
  close <- databaseFrameCombEstimators[i,closeIndex]
  for (j in base::c(1:colData)) {
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
totalEstimadoresEnBandas/rowData

# Realizamos histogramas basado en la cantidad de aciertos para cada experto en una
# cota +-5 . 
aciertosExpertos = base::rep(0,colData)

for (i in base::c(1:rowData)) {
  close <-  databaseFrameCombEstimators[i,closeIndex]
  for (j in base::c(1:colData)) {
    closeEstimated <- databaseFrameCombEstimators[i, j]
    if (closeEstimated > (close - cota) && closeEstimated < (close + cota)) {
      aciertosExpertos[j] = aciertosExpertos[j] + 1
    }
  }
}
graphics::hist(aciertosExpertos)
max(aciertosExpertos)  #1061
min(aciertosExpertos)  #351

# Calcula RMSE de la media experta de opiniones como estimador 91.33031
acumMeanComboEstimadores <- 0
colData <- base::ncol(databaseFrameCombEstimatorsTest)-1
closeIndex <- base::ncol(databaseFrameCombEstimatorsTest)
rowData <- base::nrow(databaseFrameCombEstimatorsTest)

for (i in base::c(1:rowData)) {
  close <- databaseFrameCombEstimatorsTest[i,closeIndex]
  # se obtienen todas las estimaciones de close como vector
  data  <- as.vector(t(databaseFrameCombEstimatorsTest[i, 1:colData]))
  # se hace la media
  meanEstimator <- mean(data)
  acumMeanComboEstimadores <- acumMeanComboEstimadores + ( meanEstimator - close )^2
}
RMSEMeanComboEstimadores <- base::sqrt(
                              acumMeanComboEstimadores / 
                              base::nrow(databaseFrameCombEstimatorsTest) 
                            )
RMSEMeanComboEstimadores











