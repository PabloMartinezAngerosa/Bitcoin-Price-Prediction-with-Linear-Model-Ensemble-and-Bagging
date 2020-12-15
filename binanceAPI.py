# TEST KEY
# API Key: **********
# Secret Key: **********

import pandas as pd
import math
import os.path
import time
from binance.client import Client
from binance.websockets import BinanceSocketManager
from twisted.internet import reactor
from datetime import timedelta, datetime
from dateutil import parser
from tqdm import tqdm_notebook #(Optional, used for progress-bars)
from datetime import datetime, timedelta

# init
api_key = "******"
api_secret = "*********"
client = Client(api_key, api_secret)
print("Conectado")

KLINE_INTERVAL_1MINUTE= '1m'
KLINE_INTERVAL_3MINUTE= '3m'
KLINE_INTERVAL_5MINUTE= '5m' 
KLINE_INTERVAL_15MINUTE= '15m' 
KLINE_INTERVAL_30MINUTE= '30m'
KLINE_INTERVAL_1HOUR= '1h' 
KLINE_INTERVAL_2HOUR= '2h' 
KLINE_INTERVAL_4HOUR= '4h' 
KLINE_INTERVAL_6HOUR= '6h'
KLINE_INTERVAL_8HOUR= '8h'
KLINE_INTERVAL_12HOUR= '12h'
KLINE_INTERVAL_1DAY= '1d'
KLINE_INTERVAL_3DAY= '3d'
KLINE_INTERVAL_1WEEK= '1w'
KLINE_INTERVAL_1MONTH= '1M'

interval = KLINE_INTERVAL_1HOUR

N = 365
date_N_days_ago = datetime.now() - timedelta(days=N)

filename = "BTCUSDT-" + interval + ".csv"
print("Empieza a descargar cada " + interval )
klines = client.get_historical_klines('BTCUSDT', interval, date_N_days_ago.strftime("%d %b %Y %H:%M:%S"))
data = pd.DataFrame(klines, columns = ['timestamp', 'open', 'high', 'low', 'close', 'volume', 'close_time', 'quote_av', 'trades', 'tb_base_av', 'tb_quote_av', 'ignore' ])
data['timestamp'] = pd.to_datetime(data['timestamp'], unit='ms')
data.set_index('timestamp', inplace=True)
data.sort_values(by=['timestamp'], inplace=True, ascending=False)
data.to_csv(filename)
