base::library(magrittr)
base::source("ComboEstimatorsLinearModels.R")

GetLinearModelGeneralFit <- function(train, test) {

  # generamos los posibles modelos #
  # para la muestra aleatoria general.
  comboFittedLinearModels = GetComboEstimatorsLinearModels(train, test)
  print(dim(comboFittedLinearModels))
  # Dataframe con el resultado RMSE y los modelos de todas las posibles 
  # combinaciones ejecutadas.
  # El que tiene menor RMSE en cada dataframe es el algoritmo optimo #
  # para cada tipo de modelado.
  out <- base::data.frame(
    "model" =  comboFittedLinearModels$model,
    "RMSE"  = comboFittedLinearModels$RMSE
  )

  return(out)
}

