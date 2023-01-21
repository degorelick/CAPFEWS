import numpy as np
import pandas as pd


power_data = pd.read_excel('CAP_power_purchase_data_2022.xlsx')
idx = np.logical_and(power_data.Month == 'TOTAL',power_data.variable == 'MWH')
power_data_total = power_data[idx].copy()
power_data_total.loc[power_data_total.Use=='Sell','Product']="Day Ahead (sell)"

remaining_power = 0
all_power = 0
power = {}

for s in power_data_total.Product.unique():
    if s == 'SRP FLEET OPTION' or s == 'SOLAR' or s == 'Day Ahead (sell)':
        power[s] = power_data_total.loc[power_data_total.Product == s, 'value'].values
    #elif s == 'Day Ahead (sell)':
        #power[s] = power_data_total.loc[power_data_total.Product == s, 'value'].values
    else:
        power[s] = power_data_total.loc[power_data_total.Product == s, 'value'].values
        remaining_power += power[s]
    all_power += power[s]
power_frac = {}
for s in power_data_total.Product.unique():
    if s == 'SRP FLEET OPTION' or s == 'SOLAR':
        pass
    elif s == 'Day Ahead (sell)':
        power_frac[s] = power[s]/all_power
    else:
        power_frac[s] = power[s]/remaining_power