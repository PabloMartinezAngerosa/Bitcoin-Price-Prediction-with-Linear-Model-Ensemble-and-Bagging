base::library(magrittr)
base::library(keras)
base::library(tfdatasets)
base::source("DatabaseManager.R")

datapriceTestNeuralNetwork  <- databaseFrameCombEstimatorsTest
datapriceTrainNeuralNetwork <- databaseFrameCombEstimatorsTrain

# Estandarizamos todas las variables para que tengan media 0 y desvio 1.
spec <- tfdatasets::feature_spec(
  datapriceTrainNeuralNetwork,
  close ~ . 
) %>% 
  tfdatasets::step_numeric_column(
    tfdatasets::all_numeric(),
    normalizer_fn = tfdatasets::scaler_standard()
  ) %>% 
  keras::fit()

layer <- keras::layer_dense_features(
  feature_columns = tfdatasets::dense_features(spec), 
  dtype = tfdatasets::tf[["float32"]]
)
layer(datapriceTrainNeuralNetwork)

# Ejecutar Modelo.
model_runs <- tfruns::tuning_run(
  file = "modelBlueprint.R", 
  flags = base::list(
    nodes_layer_2 = base::c(16, 8),
    activation_layer_2 = base::c("relu", "linear")
  ),
  confirm = FALSE,
  runs_dir = "runs"
)

# Resultados.
# utils::View(model_runs)
# tfruns::ls_runs(order = metric_val_accuracy)


# RMSE 136.8908
# flag_nodes_layer_2 16
# flag_nodes_layer_3 8
# flag_activation_layer_2 relu
# flag_activation_layer_3 relu
tfruns::view_run(run_dir = "2020-12-12T17-30-29Z")

# RMSE 10640.92
# flag_nodes_layer_2 8
# flag_nodes_layer_3 8
# flag_activation_layer_2 relu
# flag_activation_layer_3 relu
tfruns::view_run(run_dir = "2020-12-12T23-26-30Z")