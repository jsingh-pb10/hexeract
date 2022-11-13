from typing import Optional
import uvicorn
from fastapi import FastAPI
from pydmd import OptDMD
from pydmd import DMD
import mysql.connector
import numpy as np
import pandas as pd
opt_dmd = OptDMD()
dmd = DMD()
app = FastAPI()


@app.get("/")
def read_root():
    return {"Test": "Success"}


@app.get("/items/{item_id}")
def read_item(item_id: int, q: Optional[str] = None):
    return {"item_id": item_id, "q": q}

@app.post("/fit_dmd/{tickerlist}")
def fit(tickerlist: str, rank: int):
    final_df = pd.DataFrame()
    tickerlist = list(tickerlist.split(" "))
    for df in tickerlist:
        if df in agrilist:
            df_temp = pd.read_sql(df, con = engine_cloud_agri)
            final_df[df] = df_temp.CLOSE
        if df in energylist:
            df_temp = pd.read_sql(df, con = engine_cloud_energy)
            final_df[df] = df_temp.CLOSE

        if df in metalslist:
            df_temp = pd.read_sql(df, con = engine_cloud_metals)
            final_df[df] = df_temp.CLOSE

        if df in forexlist:
            df_temp = pd.read_sql(df, con = engine_cloud_forex)
            final_df[df] = df_temp.CLOSE

        if df in indiceslist:
            df_temp = pd.read_sql(df, con = engine_cloud_indices)
            final_df[df] = df_temp.CLOSE

        if df in equitylist:
            df_temp = pd.read_sql(df, con = engine_cloud_equity)
            final_df[df] = df_temp.CLOSE

        if df in shippinglist:
            df_temp = pd.read_sql(df, con = engine_cloud_shipping)
            final_df[df] = df_temp.CLOSE

        if df in tickdatalist:
            df_temp = pd.read_sql(df, con = engine_cloud_tickdata)
            final_df[df] = df_temp.CLOSE
        if df in indialist:
            df_temp = pd.read_sql(df, con = engine_cloud_india_market)
            try:
                print(df_temp.head())
                final_df[df] = df_temp.close
               
            except:
                final_df[df] = df_temp.CLOSE
          

    dmd_closelist = []
    final_df = final_df.fillna(method = 'ffill')


  
    for i in range(len(final_df.columns.values)):
        dmd_closelist.append(final_df.iloc[:,i].values)
    opt_dmd.svd_rank = rank
    opt_dmd.fit(X = np.asarray(dmd_closelist))
    return np.asarray(dmd_closelist).shape
   
@app.post("/predict_dmd/{latest_state}")
def predict(latest_state: str):
    latest_state = list(latest_state.split(" "))

    statelist = []
    for i in range(len(latest_state)):
        latest_state[i] = float(latest_state[i])
        statelist.append(latest_state[i])

    statelist = np.array(statelist)
    statelist = statelist.reshape(len(statelist),1)

 
    opt_pr = opt_dmd.predict(statelist)
    flat = opt_pr.flatten()
    flt = np.real(flat)
    pr = np.array2string(flt,separator=',')
 
    return {"prediction": pr}

    
    
 
    

############################# Connection to Schema ############################

mydb = mysql.connector.connect(
  host="data-main.ccy5tshcdpvf.ap-south-1.rds.amazonaws.com",
  user="admin",
  password="Brava2021!!",
    database = 'energy'


)

mycursor = mydb.cursor()
from sqlalchemy import create_engine
engine_cloud_energy = create_engine('database address')
engine_cloud_metals = create_engine('database address')
engine_cloud_forex = create_engine('database address')
engine_cloud_agri = create_engine('database address'')
engine_cloud_equity = create_engine('database address')
engine_cloud_indices = create_engine('database address')
engine_cloud_news = create_engine('database address')
engine_cloud_tickdata = create_engine('database address')
engine_cloud_india_market = create_engine('database address')





########################################### Read Tickerlist ###################################################

ticker_df = pd.read_csv('~/trading/Trade/tickerlist.csv')
agri_df = ticker_df[ticker_df.TABLE_SCHEMA == 'agri'] 
forex_df = ticker_df[ticker_df.TABLE_SCHEMA == 'forex']
energy_df = ticker_df[ticker_df.TABLE_SCHEMA == 'energy'] 
equity_df = ticker_df[ticker_df.TABLE_SCHEMA == 'equity'] 
indices_df = ticker_df[ticker_df.TABLE_SCHEMA == 'indices'] 
metals_df = ticker_df[ticker_df.TABLE_SCHEMA == 'metals'] 
shipping_df = ticker_df[ticker_df.TABLE_SCHEMA == 'shippping'] 
tickdata_df = ticker_df[ticker_df.TABLE_SCHEMA == 'tickdata'] 
india_market_df = ticker_df[ticker_df.TABLE_SCHEMA == 'india_market'] 

agrilist = list(agri_df.TABLE_NAME.values)
forexlist = list(forex_df.TABLE_NAME.values)
metalslist = list(metals_df.TABLE_NAME.values)
energylist = list(energy_df.TABLE_NAME.values)
equitylist = list(equity_df.TABLE_NAME.values)
indiceslist = list(indices_df.TABLE_NAME.values)
shippinglist = list(shipping_df.TABLE_NAME.values)
tickdatalist = list(tickdata_df.TABLE_NAME.values)
indialist = list(india_market_df.TABLE_NAME.values)


#if __name__ == '__main__':
 #   uvicorn.run("trading_endpoint:app", port=8080, host='0.0.0.0')

