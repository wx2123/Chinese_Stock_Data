# -*- coding: utf-8 -*-
"""
Created on Wed May 18 22:40:14 2022

@author: xuewu
"""

import pandas as pd
from yahoo_fin import stock_info as si


df1 = pd.DataFrame( si.tickers_sp500() )
df2 = pd.DataFrame( si.tickers_nasdaq() )
df3 = pd.DataFrame( si.tickers_dow() )
df4 = pd.DataFrame( si.tickers_other() )