# By Rachel Kleiman
# Figure for Central Arizona Project models


import pandas as pd
import numpy as np
import matplotlib as mpl
from matplotlib import pyplot as plt

from datetime import date


############# CRSS ####################
def crss_fig(runs, ti=""):
    fig, ax = plt.subplots()
    for run in runs:
        crss = pd.read_csv('calfews_src\data\input\cap-data-crss-' + str(run) + '.csv')

        ax.plot(crss.MDE_ele, label=str(run))

        plt.ylabel('Projected Lake Mead Elevation (ft)')

        d = pd.DatetimeIndex(crss.datetime)
        idx = np.logical_and(d.month == 1, d.year % 5 == 3)
        xl = d[idx].year

        plt.xticks(ticks=crss[idx].index, labels=xl)
        plt.xlabel('Year')
        plt.ylim([900, 1150])
        plt.title('CRSS runs')
    ax.legend()
    if ti == "":
        fig.savefig('results\MDE_elevation_crss_' + str(runs[0]) + '_' + str(runs[-1]) + '.png')
    else:
        fig.savefig('results\MDE_elevation_crss_' + str(ti))


# looking at all crss runs
for y in range(1, 27, 5):
    if y < 26:
        r = range(y, y + 5)
    else:
        r = range(y, y + 7)
    crss_fig(r)

# select crss run
crss_fig([1, 6, 13, 16], ti='select')

############# CAP PPA ####################

cmap = mpl.colormaps['Set2'].colors


power_data = pd.read_csv('ALLCAPData/CAP_power_purchase_data_2022.csv')

power_yr = power_data[power_data.Month == "TOTAL"][power_data.variable=="MWH"]
power_yr.value.plot.pie(labels=(power_yr['value']/(365*24)).round(1),colors=cmap)
plt.title("Total 2022 Power Traded (MW)")
#plt.legend(power_yr.Product)
plt.show()

power_yr = power_data[power_data.Month == "TOTAL"][power_data.variable=="MWH"]
power_yr.value.plot.pie(labels=power_yr['Product'],colors=cmap)
plt.title("Total 2022 Power Traded (MW)")
plt.show()

power_yr = power_data[power_data.Month == "TOTAL"][power_data.variable=="Total Dollars"]
power_yr.value.plot.pie(labels=(power_yr['value']/10**6).round(2),colors=cmap)
plt.title("Total 2022 Power Traded ($M)")
plt.show()


#####################################################################
#stacked bar chart for monthly power purchase (MWH and $)
#####################################################################

cmap = mpl.colormaps['Set2'].colors
power_data = pd.read_csv('ALLCAPData/CAP_power_purchase_data_2022.csv')

########MWH#############
power_mo=power_data[power_data.variable=="MWH"].copy()
power_mo=power_mo[power_mo.Month!="TOTAL"].copy()
#negate sell values
power_mo.loc[power_mo.Use=='Sell','value']=-power_mo.loc[power_mo.Use=='Sell','value']
power_mo.loc[power_mo.Use=='Sell','Product']="Day Ahead (sell)"

fig, ax = plt.subplots(figsize=(10,9))
b=np.zeros(len(power_mo[power_mo.Product==power_mo.Product.unique()[0]]))

for p in power_mo.Product.unique():

    val=power_mo[power_mo.Product==p].value/(365*24)
    if all(val<0):
        ax.bar(power_mo[power_mo.Product == p].Month, val, 0.35, label=p)
        print(p)
    else:
        ax.bar(power_mo[power_mo.Product == p].Month, val, 0.35, label=p, bottom=b)
        b=b+val.values

    #ax.bar(power_mo[power_mo.Product=='SOLAR'].Month, power_mo[power_mo.Product=='SOLAR'].value/(365*24), 0.35, label='Solar', bottom=(power_mo[power_mo.Product=='Monthly'].value.values)/(365*24))
plt.legend()
plt.ylabel("2022 Power Purchased (MW)")
plt.show()
fig.savefig('results\PPA_MWH.png')

############ $$$$$$$$ ###########
power_mo=power_data[power_data.variable=="Total Dollars"].copy()
power_mo=power_mo[power_mo.Month!="TOTAL"].copy()
#negate sell values
power_mo.loc[power_mo.Use=='Sell','value']=-power_mo.loc[power_mo.Use=='Sell','value']
power_mo.loc[power_mo.Use=='Sell','Product']="Day Ahead (sell)"

fig, ax = plt.subplots(figsize=(10,9))
b=np.zeros(len(power_mo[power_mo.Product==power_mo.Product.unique()[0]]))

for p in power_mo.Product.unique():

    val=power_mo[power_mo.Product==p].value/(1e6)
    if all(val<0):
        ax.bar(power_mo[power_mo.Product == p].Month, val, 0.35, label=p)
        print(p)
    else:
        ax.bar(power_mo[power_mo.Product == p].Month, val, 0.35, label=p, bottom=b)
        b=b+val.values

    #ax.bar(power_mo[power_mo.Product=='SOLAR'].Month, power_mo[power_mo.Product=='SOLAR'].value/(365*24), 0.35, label='Solar', bottom=(power_mo[power_mo.Product=='Monthly'].value.values)/(365*24))
plt.legend()
plt.ylabel("2022 Power Purchased ($M)")
plt.show()
fig.savefig('results\PPA_Total_Dollars.png')


#looking at prices
fig, ax = plt.subplots()
power_price= power_data[power_data.variable=="Unit Price ($/MWH)"]
power_price=power_price[power_price.Month!='TOTAL']
power_price.loc[power_price.Use=='Sell','Product']="Day Ahead (sell)"
for p in power_price.Product.unique():
    df=pd.DataFrame(power_price[power_price.Product==p].value.values,index=power_price.Month.unique())[0]
    df.plot()
plt.legend(power_price.Product.unique())
plt.ylabel("Unit Price ($/MWH)")
plt.xlabel("Month")
plt.title("2022 CAP Power Prices")
plt.show()



#####################################################################
#output of model-- power
#####################################################################
years = 8 #until 2030
time = years * 12
crss_runs = (1, 6, 13, 16)
#timeseries comparing crss runs
scenarios = ['10.2MW_solar_PPA', 'full_PPAS','52.5MW_solar_PPA']

for scenario in scenarios:
    fig, ax = plt.subplots()

    for crss in crss_runs:
        output = pd.read_csv('results/crss-' + str(crss) + "/" + scenario + "/results.csv")
        finances = output[output.filter(like='CAP').columns]
        del output
        finances = finances[0:time]
        (finances.CAP_total_pumping_cost/1e6).plot()

    plt.xlabel('Month')
    plt.xticks(ticks = range(0,time-1,12), labels = range(2023,2031))
    plt.ylabel('Total Pumping Cost ($M)')
    plt.ylim([-2,30])
    plt.xlim([-5,time+5])
    plt.legend((1, 6, 13, 16))
    plt.show()
    fig.savefig('results/' + scenario + '.png')

#timeseires comparing PPA scenarios

for crss in crss_runs:
    fig, ax = plt.subplots()
    for scenario in scenarios:
        output = pd.read_csv('results/crss-' + str(crss) + "/" + scenario + "/results.csv")
        finances = output[output.filter(like='CAP').columns]
        del output
        finances = finances[0:time]
        (finances.CAP_total_pumping_cost / 1e6).plot()

    plt.xlabel('Month')
    plt.xticks(ticks=range(0, time - 1, 12), labels=range(2023, 2031))
    plt.ylabel('Total Pumping Cost ($M)')
    plt.ylim([-2, 30])
    plt.xlim([-5, time + 5])
    plt.legend(scenarios)
    plt.show()
    fig.savefig('results/' + str(crss) + '_scenarios.png')