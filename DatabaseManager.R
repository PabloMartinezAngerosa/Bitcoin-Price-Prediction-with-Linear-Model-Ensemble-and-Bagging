base::library(magrittr)
# Bitcoin precio por hora  desde 2019-12-10 23:00:00 hasta 2020-12-10 01:00:00 #
.btcPrice <-  utils::read.csv("BTCUSDT-1h.csv")

# Se toma los "precios del close" en el momento n.
# La "cantidad de volumen" en el momento n-1.
# La variable explicada "precio del close de bitcoin" esta en el momento n.
# Se agregan los lags de 1 a 5 del precio del bitcion y volumen de bitcoin. 
.limite = base::length(.btcPrice$open) - 6

databaseCoins <- base::data.frame(
  "date"       =  base::as.POSIXct(.btcPrice$timestamp[1:(.limite-1)]),
  "close"      = .btcPrice$close[1:(.limite-1)],
  "closeLag1"  = .btcPrice$close[2:.limite],
  "closeLag2"  = .btcPrice$close[3:(.limite +1)],
  "closeLag3"  = .btcPrice$close[4:(.limite +2)],
  "closeLag4"  = .btcPrice$close[5:(.limite +3)],
  "closeLag5"  = .btcPrice$close[6:(.limite +4)],
  "closeLag6"  = .btcPrice$close[7:(.limite +5)],
  "volLag1"    = .btcPrice$volume[2:.limite],
  "volLag2"    = .btcPrice$volume[3:(.limite +1)],
  "volLag3"    = .btcPrice$volume[4:(.limite +2)],
  "volLag4"    = .btcPrice$volume[5:(.limite +3)],
  "volLag5"    = .btcPrice$volume[6:(.limite +4)],
  "volLag6"    = .btcPrice$volume[7:(.limite +5)]
)

databaseFrameCombEstimators <-  utils::read.csv("frameCombEstimators.csv")
dummyIndex <- data.frame("index" = base::c(1:base::nrow(databaseFrameCombEstimators)))
databaseFrameCombEstimators <- cbind(dummyIndex, databaseFrameCombEstimators)

# Invertimos el orden para que el primero sea la fecha mas reciente
databaseFrameCombEstimators <- databaseFrameCombEstimators %>% 
  dplyr::arrange(dplyr::desc(index))

databaseFrameCombEstimators <- databaseFrameCombEstimators[,-1]
databaseFrameCombEstimators <- base::cbind(dummyIndex, databaseFrameCombEstimators)

dummyIndex    <- data.frame("index" = base::c(1:base::nrow(databaseCoins)))
databaseCoins <- base::cbind(dummyIndex, databaseCoins)

databaseCoins <- databaseCoins[databaseFrameCombEstimators$index,]

# Seleccionamos aleatoriamente 70% para train y 30% para test. #
# cortamos la base para que tenga las mismas predicciones que la
# base con las ventanas moviles.
base::set.seed(1234)
datapriceTest  <- dplyr::slice_sample(databaseCoins, prop = 0.3)
datapriceTrain <- dplyr::anti_join(databaseCoins, datapriceTest)

databaseFrameCombEstimatorsTest  <- databaseFrameCombEstimators[datapriceTest$index,]
databaseFrameCombEstimatorsTrain <- databaseFrameCombEstimators[datapriceTrain$index,]

# Certificamos que los indices son todos los mismos para ambas bases de train y test
base::sum(datapriceTest$index == databaseFrameCombEstimatorsTest$index)
base::dim(datapriceTest)
base::dim(databaseFrameCombEstimatorsTest)

base::sum(datapriceTrain$index == databaseFrameCombEstimatorsTrain$index)
base::dim(datapriceTrain)
base::dim(databaseFrameCombEstimatorsTrain)

# Sacamos la variable Index de las Basees de Datos generadas
databaseFrameCombEstimators <- databaseFrameCombEstimators[,-1]
databaseFrameCombEstimatorsTest  <- databaseFrameCombEstimatorsTest[,-1]
databaseFrameCombEstimatorsTrain <- databaseFrameCombEstimatorsTrain[,-1]

databaseCoins  <- databaseCoins[,-1]
datapriceTest  <- datapriceTest[,-1]
datapriceTrain <- datapriceTrain[,-1]

