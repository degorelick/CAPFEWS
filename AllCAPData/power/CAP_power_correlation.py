#finding correlation between CAP's power prices and market prices

import pandas as pd
import numpy as np
import matplotlib as mpl
from matplotlib import pyplot as plt
import seaborn as sns
import os

#read in CAP data #TODO for loop for all data
year = 2022
month = "JANUARY"
month_float = "01"
#summary data is net power transactions
cap = pd.read_excel("AllCAPData/power/2021." + month_float + " " + month + " Daily Load-Resources-Cost.xlsx", sheet_name = "Hourly Totals", header = 1, usecols = ['Date','MWh','$/MW','TOTAL $'])

#read in daily palo verde, CAISO peak prices, natural gas from EIA
eia_peak = pd.read_excel("ice_electric-" + str(year) + "final.xlsx")
eia_peak = eia_peak[eia_peak['Price hub'] == 'Palo Verde Peak']

#compare
#TODO convert date columns to datetime
#if cap.Date within market delivery dates -- compare
