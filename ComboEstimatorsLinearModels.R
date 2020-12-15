# Este algoritmo devuelve las (2^n) - 1  modelos lineales 
# de todas las combinaciones posibles de un conjunto de variables
# para una Base de entrenamiento dada.
# Por default devuelve el RMSE de cada uno de los modelos generados
# utilizando datasetTest recibido como parametro.
# Si se marca getPricePrediction = TRUE, retorna el precio estimado para
# cada modelo generado. 
GetComboEstimatorsLinearModels <- function(databaseTrain, databaseTest, getPircePrediction=FALSE) {
  
  # Algoritmo de fuerza bruta utilizado para encontrar el modelo con el  
  # minimo RMSE.
  # Buscamos todas las combinaciones posibles entre n variables.  
  .combinacionesPosibles <- tidyr::crossing(
    var1  = 0:1, 
    var2  = 0:1,
    var3  = 0:1,
    var4  = 0:1,
    var5  = 0:1,
    var6  = 0:1,
    var7  = 0:1,
    var8  = 0:1,
    var9  = 0:1,
    var10 = 0:1
  )
  
  .variablesLag <- base::c(
    "closeLag1",
    "closeLag2",
    "closeLag3",
    "closeLag4",
    "closeLag5",
    "volLag1",
    "volLag2",
    "volLag3",
    "volLag4",
    "volLag5"
  )
  
  .linearModelEvalFirst <- "linearModelFited = lm(close~  "
  .linearModelEvalEnd   <- ", data=databaseTrain)"
  
  .linearModelRMSEDataprice   <- base::c()
  .linearModelModelsFit       <- base::c()
  .linearModelPricePrediction <- base::c()
  
  .totalCombinaciones <- base::nrow(.combinacionesPosibles)
  
  # Recorremos todas las combinaciones posibles.
  for (i in base::c(1:.totalCombinaciones)) {
    
    eval <- ""
    totalVariables <- 0
    
    for ( variable in .variablesLag[.combinacionesPosibles[i,]==1]) {
      if (eval == "") {
        variable <- base::paste(" ", variable)
      } else {
        variable <- base::paste("+ ", variable)  
      }
      eval <- base::paste(eval, variable)
      totalVariables <- totalVariables + 1
    }
    
    if (eval!="") {
      
      linerModelEval <- base::paste(.linearModelEvalFirst, eval, .linearModelEvalEnd)
      
      # Se ejecuta una instancia de ajuste por eval.
      base::eval(base::parse(text=linerModelEval))
      predDataprice <- stats::predict(linearModelFited, databaseTest, interval = "prediction")
      
      # RMSE Linear Model
      .linearModelRMSEDataprice[base::length(.linearModelRMSEDataprice) + 1] <- base::sqrt(
        base::sum((predDataprice -
                     databaseTest$close )^2)/
          base::length(databaseTest$close))
      
      # Guardamos el Modelo 
      .linearModelModelsFit[base::length(.linearModelModelsFit) + 1] <- linerModelEval
      
      # si se pide la prediccion del modelo correspondiente 
      if (getPircePrediction == TRUE) {
        .linearModelPricePrediction[base::length(.linearModelPricePrediction) + 1] <- predDataprice
      }
      # Estado actual del algoritmo. 
      base::print((i/.totalCombinaciones))
    }
  }
  
  out <- base::data.frame(
    "model" = .linearModelModelsFit,
    "RMSE"  = .linearModelRMSEDataprice
  )
  
  if (getPircePrediction == TRUE) {
    out <- base::cbind(out, "price" = .linearModelPricePrediction)
  } 
  
  return (out)
}

