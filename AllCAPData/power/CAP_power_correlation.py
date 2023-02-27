#finding correlation between CAP's power prices and market prices
import datetime
import pandas as pd
import numpy as np
import matplotlib as mpl
from matplotlib import pyplot as plt
import seaborn as sns
import os

#read in CAP data
#cap net power transactions
years = [2021, 2022]
months = ["JANUARY", "FEBRUARY", "MARCH", "APRIL", "MAY", "JUNE", "JULY", "AUGUST", "SEPTEMBER", "OCTOBER", "NOVEMBER", "DECEMBER"]
cap = pd.DataFrame()
for year in years:
    m = 1
    for month in months:
        if m < 10:
            month_float = "0" + str(m)
        else:
            month_float = str(m)
        try:
            cap = cap.append(pd.read_excel(str(year)+"." + month_float + " " + month + " Daily Load-Resources-Cost.xlsx", sheet_name = "Hourly Totals", header = 1, usecols = ['Date','MWh','$/MW','TOTAL $']))
        except ValueError:
            cap = cap.append(pd.read_excel(str(year) + "." + month_float + " " + month + " Daily Load-Resources-Cost.xlsx", sheet_name="Hourly Totals", header=2, usecols=['Date', 'MWh', '$/MW', 'TOTAL $']))
        m = m + 1

#take out rows that aren't associated with a date
idx = []
for i, row in cap.iterrows():
    if pd.notna(row.Date) and type(row.Date) != str:
        idx.append(True)
    else:
        idx.append(False)
cap = cap.iloc[idx]
cap = cap.reset_index(drop = True)
for i,row in cap.iterrows():
    if isinstance(row.Date,datetime.datetime):
        cap.at[i,'Date'] = pd.Timestamp(row.Date)

#read in daily palo verde, CAISO peak prices, natural gas from EIA
eia_peak = pd.DataFrame()
for year in years:
    eia_peak = eia_peak.append(pd.read_excel("ice_electric-" + str(year) + "final.xlsx"))
    eia_peak = eia_peak[eia_peak['Price hub'] == 'Palo Verde Peak']
    eia_peak['start'] = pd.to_datetime(eia_peak['Delivery start date'])
    eia_peak['end'] = pd.to_datetime(eia_peak['Delivery \nend date'])

#compare
#if cap.Date within market delivery dates -- compare
market = []
market_hi = []
market_lo = []
for i,row in cap.iterrows():
    eia = eia_peak[np.logical_and(eia_peak.start <= row.Date,eia_peak.end >= row.Date)]
    if eia['Wtd avg price $/MWh'].size == 0:
        if i == 0:
            market.append(0)
            market_hi.append(0)
            market_lo.append(0)
        else:
            market.append(market[i-1])
            market_hi.append(market_hi[i-1])
            market_lo.append(market_lo[i-1])
    else:
        market.append(eia['Wtd avg price $/MWh'].values[0])
        market_hi.append(eia['High price $/MWh'].values[0])
        market_lo.append(eia['Low price $/MWh'].values[0])
cap['market'] = market
cap['market_high'] = market_hi
cap['market_low'] = market_lo

#plot scatter
fig, ax = plt.subplots()
#mi = 100
ma =200
idx = np.logical_and(cap.market<ma, cap['$/MW']<ma)
#idx = np.logical_and(cap['$/MW']>-300,np.logical_and(cap.market>mi, cap.market < 600))
#ax.errorbar(cap.market[idx], cap['$/MW'][idx],xerr=[cap.market_low,cap.market_high],fmt="o")
ax.scatter(cap.market[idx],cap['$/MW'][idx])
plt.xlabel('Market price: wtd av')
plt.ylabel('Cap price: net')
plt.axhline(y=0, linestyle=':', color='black')
plt.show()
fig.savefig('scatter_power_zoom.png')


#plot timeseries
fig, ax = plt.subplots()
cap.market.plot()
cap['$/MW'].plot()
dum = cap.iloc[ax.get_xticks().astype(int)[1:-1]].Date
l=[]
for i in dum:
    l.append("Q" + str(i.quarter))
plt.xticks(ticks=ax.get_xticks()[1:-1], labels=l)
plt.legend(['PV Market: peak','CAP price: net'])

plt.ylabel('Price: $/mw')
plt.xlabel('Time (2021-2022)')
plt.show()
fig.savefig('ts_power.png')
