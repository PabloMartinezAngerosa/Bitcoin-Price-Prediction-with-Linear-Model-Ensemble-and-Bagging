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
