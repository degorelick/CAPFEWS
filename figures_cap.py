# By Rachel Kleiman
# Figure for Central Arizona Project models


import pandas as pd
import numpy as np
import matplotlib as mpl
from matplotlib import pyplot as plt

from datetime import date

set = mpl.colormaps['Set1'].colors
cmap = [set[2], set[0], set[3], set[5], set[1], set[4]]
cmap_power = pd.DataFrame()
cmap_power['Product'] = ['Monthly', 'Day Ahead', 'SRP FLEET OPTION', 'SOLAR', 'HOOVER', 'Day Ahead (sell)']
cmap_power['cmap'] = cmap

crss_plot = 0
power_pie = 0
stacked_bar_power = 1
historic_prices = 0
results_power = 0
############# CRSS ####################

if crss_plot == 1:
    def crss_fig(runs, ti="", end_year="", legend=True, tiers=""):
        fig, ax = plt.subplots()
        for run in runs:
            crss = pd.read_csv('calfews_src/data/input/cap-data-crss-' + str(run) + '.csv')

            if end_year != "":
                idx = crss[pd.DatetimeIndex(crss.datetime).year == end_year].index[0]
                crss = crss[0:idx]

            ax.plot(crss.MDE_ele, label=str(run))

            plt.ylabel('Projected Lake Mead Elevation (ft)')

            d = pd.DatetimeIndex(crss.datetime)
            if end_year == "":
                idx = np.logical_and(d.month == 1, d.year % 5 == 3)
                xl = d[idx].year
                plt.xticks(ticks=crss[idx].index, labels=xl)
            else:
                idx = (d.month == 1)
                xl = d[idx].year
                plt.xticks(ticks=crss[idx].index, labels=xl)
            plt.xlabel('Year')
            plt.ylim([900, 1150])
            plt.title('CRSS runs')
        if legend != False:
            ax.legend()
        if tiers != "":
            for t in tiers:
                plt.axhline(y=t, linestyle=':', color='black')
        if ti == "":
            fig.savefig('results/MDE_elevation_crss_' + str(runs[0]) + '_' + str(runs[-1]) + '.png')
        else:
            fig.savefig('results/MDE_elevation_crss_' + str(ti))


    # looking at all crss runs in chunks
    # for y in range(1, 27, 5):
    #     if y < 26:
    #         r = range(y, y + 5)
    #     else:
    #         r = range(y, y + 7)
    #     crss_fig(r)

    crss_fig(range(1, 33), ti='all_2030', end_year=2030, legend=False, )

    # select crss run

    tiers = [1090, 1075, 1050, 1045, 1025, 895]
    t = []
    for tier in tiers:
        t.append(tier)
        crss_fig([1, 6, 13, 16], ti='2030_' + str(t), end_year=2030, tiers=t, legend=False)

############# CAP PPA ####################
if power_pie == 1:
    power_data = pd.read_excel('CAP_power_purchase_data_2022.xlsx')

    fig, ax = plt.subplots()
    power_yr = power_data[power_data.Month == "TOTAL"][power_data.variable == "MWH"]
    power_yr = power_yr[power_yr.Use == 'Buy']
    power_yr.value.plot.pie(labels=(power_yr['value'] / (365 * 24)).round(1), colors=cmap)
    plt.title("Total 2022 Power Traded (MW)")
    # plt.legend(power_yr.Product)
    plt.show()
    fig.savefig('results/pie_power_MWH.png')

    fig, ax = plt.subplots()
    power_yr = power_data[power_data.Month == "TOTAL"][power_data.variable == "MWH"]
    power_yr = power_yr[power_yr.Use == 'Buy']
    power_yr.value.plot.pie(labels=power_yr['Product'], colors=cmap)
    plt.title("Total 2022 Power Traded (MW)")
    plt.show()

    fig, ax = plt.subplots()
    power_yr = power_data[power_data.Month == "TOTAL"][power_data.variable == "Total Dollars"]
    power_yr = power_yr[power_yr.Use == 'Buy']
    power_yr.value.plot.pie(labels=(power_yr['value'] / 10 ** 6).round(2), colors=cmap)
    plt.title("Total 2022 Power Traded ($M)")
    plt.show()
    fig.savefig('results/pie_power_$.png')

    set_gray = mpl.colormaps['tab20c'].colors
    cmap_gray = [set[1], set[0], '0.90', '0.95', '0.88', set[7]]
    fig, ax = plt.subplots()
    power_yr = power_data[power_data.Month == "TOTAL"][power_data.variable == "Total Dollars"]
    power_yr = power_yr[power_yr.Use == 'Buy']
    power_yr.value.plot.pie(labels=(power_yr['value'] / 10 ** 6).round(2), colors=cmap_gray)
    plt.title("Total 2022 Power Traded ($M)")
    plt.show()
    fig.savefig('results/pie_power_$_spot.png')

#####################################################################
# stacked bar chart for monthly power purchase (MWH and $)
#####################################################################
if stacked_bar_power == 1:

    power_data = pd.read_excel('CAP_power_purchase_data_2022.xlsx')
    power_data['Month_idx'] = pd.to_datetime(power_data.Month, format='%b', errors='coerce').dt.month
    power_data = power_data.sort_values(by="Month_idx")

    ########MWH#############
    power_mo = power_data[power_data.variable == "MWH"].copy()
    power_mo = power_mo[power_mo.Month != "TOTAL"].copy()
    # negate buy values
    power_mo.loc[power_mo.Use != 'Sell', 'value'] = -power_mo.loc[power_mo.Use != 'Sell', 'value']
    power_mo.loc[power_mo.Use == 'Sell', 'Product'] = "Day Ahead (sell)"

    fig, ax = plt.subplots(figsize=(10, 9))
    b = np.zeros(len(power_mo[power_mo.Product == power_mo.Product.unique()[0]]))

    for p in power_mo.Product.unique():
        val = power_mo[power_mo.Product == p].value / (365 * 24)
        if all(val < 0):
            ax.bar(power_mo[power_mo.Product == p].Month, val, 0.35, label=p, bottom=b,
                   color=cmap_power[cmap_power.Product == p].cmap)
            b = b + val.values
            print(p)
        else:
            ax.bar(power_mo[power_mo.Product == p].Month, val, 0.35, label=p,
                   color=cmap_power[cmap_power.Product == p].cmap)


    plt.legend()
    plt.ylabel("2022 Power Transactions (MW)")
    plt.show()
    fig.savefig('results/PPA_MWH.png')

    ############ $$$$$$$$ ###########
    power_mo = power_data[power_data.variable == "Total Dollars"].copy()
    power_mo = power_mo[power_mo.Month != "TOTAL"].copy()
    # negate buy values
    power_mo.loc[power_mo.Use != 'Sell', 'value'] = -power_mo.loc[power_mo.Use != 'Sell', 'value']
    power_mo.loc[power_mo.Use == 'Sell', 'Product'] = "Day Ahead (sell)"

    fig, ax = plt.subplots(figsize=(10, 9))
    b = np.zeros(len(power_mo[power_mo.Product == power_mo.Product.unique()[0]]))

    for p in power_mo.Product.unique():
        val = power_mo[power_mo.Product == p].value/1e6
        if all(val < 0):
            ax.bar(power_mo[power_mo.Product == p].Month, val, 0.35, label=p, bottom=b,
                   color=cmap_power[cmap_power.Product == p].cmap)
            b = b + val.values
            print(p)
        else:
            ax.bar(power_mo[power_mo.Product == p].Month, val, 0.35, label=p,
                   color=cmap_power[cmap_power.Product == p].cmap)


    plt.legend()
    plt.ylabel("2022 Power Transactions ($M)")
    plt.show()
    fig.savefig('results/PPA_Total_Dollars.png')

if historic_prices == 1:
    # looking at prices
    fig, ax = plt.subplots()
    power_price = power_data[power_data.variable == "Unit Price ($/MWH)"]
    power_price = power_price[power_price.Month != 'TOTAL']
    power_price.loc[power_price.Use == 'Sell', 'Product'] = "Day Ahead (sell)"
    for p in power_price.Product.unique():
        df = pd.DataFrame(power_price[power_price.Product == p].value.values, index=power_price.Month.unique())[0]
        df.plot()
    plt.legend(power_price.Product.unique())
    plt.ylabel("Unit Price ($/MWH)")
    plt.xlabel("Month")
    plt.title("2022 CAP Power Prices")
    plt.show()

#####################################################################
# output of model-- power
#####################################################################
if results_power == 1:
    start_year, end_year = 2023, 2030
    years = end_year - start_year
    time = years * 12
    crss_runs = (1, 6, 13, 16)
    # timeseries comparing crss runs
    scenarios = ['10.2MW_solar_PPA', 'full_PPAS', '52.5MW_solar_PPA']

    for scenario in scenarios:
        fig, ax = plt.subplots()

        for crss in crss_runs:
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
        plt.legend((1, 6, 13, 16))
        plt.show()
        fig.savefig('results/' + scenario + '.png')

    # timeseires comparing PPA scenarios

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

    # average per year for madison
    cost_yr = {}
    sale_yr = {}
    for crss in crss_runs:
        for scenario in scenarios:
            output = pd.read_csv('results/crss-' + str(crss) + "/" + scenario + "/results.csv")
            finances = output[output.filter(like='CAP').columns]
            del output
            finances = finances[0:time]
            sale = np.zeros(years)
            cost = np.zeros(years)
            idx = 0
            for i in range(0, len(finances), 12):
                sale[idx] = finances.iloc[i:i + 12].CAP_day_ahead_sales.sum()
                cost[idx] = finances.iloc[i:i + 12].CAP_total_pumping_cost.sum()
                idx += 1
            sale_yr['crss_' + str(crss), scenario] = sale
            cost_yr['crss_' + str(crss), scenario] = cost

    pd.DataFrame(cost_yr, index=range(start_year, end_year)).to_csv('results/yearly_pumping costs.csv')
    pd.DataFrame(sale_yr, index=range(start_year, end_year)).to_csv('results/yearly_day_ahead_sales.csv')
