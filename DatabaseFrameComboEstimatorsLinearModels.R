# Este archivo se llama una sola vez para crear el CSV #
# Genera base de datos con cada uno de las 2^n - 1 estimaciones
# generado por cada modelo correspondiente de toda la base en un frame dado.
# Realiza una base de entrenamiento y de muestra con los mismos indices que la general.
base::source("DatabaseManager.R")
base::source("ComboEstimatorsLinearModels.R")


frameSize <- 25
inicio    <- base::nrow(databaseCoins) - 1 - frameSize  # 8717

databaseFrame <- base::data.frame() 

# Recorremos toda la base 
for (i in c(0:(inicio-1))) {
  
  frameIndex <- i + 1
  print(frameIndex)
  # Genera dataset de test y train para la ventana
  datapriceTest  <- databaseCoins[(inicio - i):(inicio - i),]
  datapriceTrain <- databaseCoins[(inicio - i + 1):(inicio - i + frameSize + 1),]
  

  frameComboEstimators <- GetComboEstimatorsLinearModels(datapriceTrain,
                                                        datapriceTest,
                                                        getPircePrediction = TRUE)
  
  rowDummy <- frameComboEstimators$price
  rowDummy[base::length(rowDummy) + 1 ] <- datapriceTest$close
  databaseFrame <- rbind(databaseFrame, rowDummy)  
  colnames(databaseFrame) <- c(1:1023,"close")

  
}

# Creamos .csv con la base por frame
utils::write.csv(databaseFrame,"frameCombEstimators.csv", row.names = FALSE)



