base::library(magrittr)
base::source("DatabaseManager.R")
base::source("LinearModelGeneralFit.R")


# Se genera para la base con 70% train y 30% test
# Ajustes con Modelos Lineales.
linearModelGeneralFit <- GetLinearModelGeneralFit(datapriceTrain, datapriceTest)
# RMSE 231.4537 linearModelFited = lm(close ~ closeLag1 + closeLag3, data=databaseTrain)
bestRMSELinearModel   <- linearModelGeneralFit$RMSE[base::which.min(linearModelGeneralFit$RMSE)]


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
cota <- 5
colData <- base::ncol(databaseFrameCombEstimators)-1
closeIndex <- base::ncol(databaseFrameCombEstimators)
rowData <- base::nrow(databaseFrameCombEstimators)

for (i in base::c(1:rowData)) {
  close <-  databaseFrameCombEstimators[i,closeIndex]
  for (j in base::c(1:colData)) {
    closeEstimated <- databaseFrameCombEstimators[i, j]
    if (closeEstimated > (close - cota) && closeEstimated < (close + cota)) {
      aciertosExpertos[j] = aciertosExpertos[j] + 1
    }
  }
}
base::plot(aciertosExpertos, type = "h", xlim=c(1,1023), xlab="Modelos", ylab="Cantidad de aciertos")

# Gráfico de acierto de los modelos y las variables más importante segun Bagging
base::plot(aciertosExpertos, type = "l", xlim=c(1,1023), xlab="Modelos", ylab="Cantidad de aciertos")
abline(v=c(728,16,512,714,704,673,110,513,596,804,777,907))

# Promedio de acierto de umbrales absolutos y relativos
base::mean(aciertosExpertos[1:250]) #545.904
(base::mean(aciertosExpertos[1:250])/rowData)*100 #6.262522%
base::mean(aciertosExpertos[251:550]) #713.1167
(base::mean(aciertosExpertos[251:550])/rowData)*100 #8.180758%
base::mean(aciertosExpertos[551:1023]) #887.797
(base::mean(aciertosExpertos[551:1023])/rowData)*100 #10.18466%

base::max(aciertosExpertos)  #1061
(base::max(aciertosExpertos)/rowData)*100  #12.17162%
base::min(aciertosExpertos)  #351
(base::min(aciertosExpertos)/rowData)*100 #4.026615%
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

base::set.seed(134)
colData <- base::ncol(databaseFrameCombEstimators)-1
closeIndex <- base::ncol(databaseFrameCombEstimators)
muestra <- sample(1:base::nrow(databaseFrameCombEstimators), size = 4)
muestra1 <- as.vector(t(databaseFrameCombEstimators[muestra[1], 1:colData]))
close1 <- databaseFrameCombEstimators[muestra[1], closeIndex]
plot(muestra1, seq(from = 1, to = 1, length.out = length(muestra1)), 
     ylim=c(0.5,1.5), xlim = c(min(muestra1)-30,max(muestra1)+30),
     xlab="", ylab = "")
abline(v=close1, col=1)

muestra2 <- as.vector(t(databaseFrameCombEstimators[muestra[2], 1:colData]))
close2 <- databaseFrameCombEstimators[muestra[2], closeIndex]
plot(muestra2, seq(from = 1, to = 1, length.out = length(muestra1)), 
     ylim=c(0.5,1.5), xlim = c(min(muestra2)-30,max(muestra2)+30),
     xlab="", ylab = "")
abline(v=close2, col=1)

muestra3 <- as.vector(t(databaseFrameCombEstimators[muestra[3], 1:colData]))
close3 <- databaseFrameCombEstimators[muestra[3], closeIndex]
plot(muestra3, seq(from = 1, to = 1, length.out = length(muestra1)), 
     ylim=c(0.5,1.5), xlim = c(min(muestra3)-30,max(muestra3)+30),
     xlab="", ylab = "")
abline(v=close3, col=1)

muestra4 <- as.vector(t(databaseFrameCombEstimators[muestra[4], 1:colData]))
close4 <- databaseFrameCombEstimators[muestra[4], closeIndex]
plot(muestra4, seq(from = 1, to = 1, length.out = length(muestra4)), 
     ylim=c(0.5,1.5), xlim = c(min(muestra4)-30,max(muestra4)+30),
     xlab="", ylab = "")
abline(v=close4, col=1)
