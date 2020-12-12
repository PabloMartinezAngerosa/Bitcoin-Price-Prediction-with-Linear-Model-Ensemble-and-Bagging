# Esta funcion realiza Bagging de una muestra.
# Se especifica numero de bolsas, numero dentro de las bolsas.
# Retorna el Resultado de Bagging 
# El modelo de ajuste que utilza es Promedio por opinion experta.
DoBagging <- function (data, nBag, nInBag) {
  
  trainedBag <- c()
  for ( i in 1:nBag) {
    # Obtenemos con remplazo la primer bolsa
    # La distribucion de probabildad es una Uniforme 0,1
    sample <- base::sample(data, replace=TRUE, size=nInBag)
    #  Ejecutamos el Promedio por opinion experta en la bolsa
    trainedBag[i] <- mean(as.vector(sample))
  }
  # Hacemos la media de las distintas estimaciones y retorna prediccion baggin
  meanBag <- mean(trainedBag)
  return(meanBag)
  
}
