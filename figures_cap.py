# By Rachel Kleiman
# Figure for Central Arizona Project models


import pandas as pd
import numpy as np
import matplotlib as mpl
from matplotlib import pyplot as plt
import seaborn as sns
import os

#colors
set = mpl.colormaps['Set1'].colors
cmap = [set[2], set[0], set[3], set[5], set[1], set[4]]
cmap_power = pd.DataFrame()
cmap_power['Product'] = ['Monthly', 'Day Ahead', 'SRP FLEET OPTION', 'SOLAR', 'HOOVER', 'Day Ahead (sell)']
cmap_power['cmap'] = cmap

#time
start_year, end_year = 2023, 2030
years = end_year - start_year
time = years * 12

#scenarios
crss_runs = (6, 13, 16)
scenarios = ['10.2MW_solar_PPA', '+100MW_PPA']

#figure toggle
crss_plot = 0
explore_hoover = 1
mead_allocation = 0
power_pie = 0
stacked_bar_power = 0
historic_prices = 0
results_timeseries_power = 0
results_boxplot_power = 0

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
            plt.ylim([890, 1150])
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

    crss_fig(range(1, 33), ti='all_2030', end_year=2030, legend=False)

    # select crss run
    crss_fig(crss_runs, ti='select_2030', end_year=2030, legend=True)
    tiers = [1090, 1075, 1050, 1045, 1025, 895]
    t = []
    for tier in tiers:
        t.append(tier)
        crss_fig(crss_runs, ti='2030_' + str(t), end_year=2030, tiers=t, legend=False)

if explore_hoover == 1:

    #clean hoover data if not already done
    if not os.path.exists('ALLCAPData/APA_Hoover_cleaned.csv'):
        hoover_raw = pd.read_excel('ALLCAPData/APA Hoover Monthly Capacity and Energy.xlsx', sheet_name = '2018-2023', header = 2, usecols = range(0,10))
        #clean
        idx = hoover_raw.Capacity.apply(type) == int
        hoover = hoover_raw[idx].reset_index(drop = True)

        df = hoover_raw[hoover_raw.Month.str.contains('APA') == True][['Month', 'Unnamed: 0']]
        df['Unnamed: 0'][15] = 2018
        df['Year'] = df['Unnamed: 0'].astype(int)
        df = df.drop('Unnamed: 0', axis=1)
        df = df.append(pd.Series(data = {'Month':'APA--Hoover Capacity and Energy','Year': 2018}),ignore_index = True)
        df = df.sort_values('Year')
        #switch the first two
        df.index = [1, 0, 2, 3, 4, 5, 6]
        df = df.sort_index()
        #repeat each row 13 ties and add to hoover
        hoover['Year'] = df.Year.repeat(13).reset_index(drop = True)
        hoover['Label'] = df.Month.repeat(13).reset_index(drop = True)
        hoover = hoover.drop(['Unnamed: 0'], axis=1)
        #reorder
        col = hoover.columns.tolist()
        col = [col[-1]] + [col[-2]] + col[:-2]
        hoover = hoover[col]
        hoover = hoover[hoover.Label != 'APA--Hoover Capacity and Energy Daily Average']
        del hoover_raw
        hoover.to_csv('ALLCAPData/APA_Hoover_cleaned.csv')

    #read in cleaned hoover data
    hoover = pd.read_csv('ALLCAPData/APA_Hoover_cleaned.csv')



    #totals
    hoover_total = hoover[hoover.Month == 'Total'].reset_index(drop=True)
    fig, ax = plt.subplots()
    ax.plot(hoover_total[['Capacity', '$ Amount', 'Energy', '$ Amount.1', 'Total', '$/MWh']])
    plt.xticks(ticks = hoover_total.index, labels=hoover_total.Year)
    plt.legend(['Capacity (af)', 'Capacity ($)', 'Energy (mwh)', 'Energy ($)', 'Total ($)','Rate ($/MWh)'])
    plt.show()

    #correlation between capacity and rate
    fig, ax = plt.subplots()
    ax.scatter(hoover_total.Capacity, hoover_total.Energy)
    plt.xlabel('Capacity (af)')
    plt.ylabel('Energy(mwh)')
    plt.show()

    fig, ax = plt.subplots()
    ax.scatter(hoover_total.Capacity, hoover_total['$/MWh'])
    plt.xlabel('Capacity (af)')
    plt.ylabel('Rate ($/MWh)')
    plt.show()



    #monthlies
    hoover = hoover[hoover.Month != 'Total'].reset_index(drop=True)
    #plot rates
    fig, ax = plt.subplots()
    ax.plot(hoover[['Rate','Rate.1','$/MWh']])
    plt.xticks(ticks = range(0,len(hoover),12),labels = hoover.Year.unique())
    plt.xlabel('Year')
    plt.ylabel('Rate ($/mwh)')
    plt.legend(['Capacity rate','Energy Rate','Total Rate'])
    plt.show()
    fig.savefig('results/hoover/hoover_rates.png')
    #energy/water
    fig, ax = plt.subplots()
    ax.plot(hoover[['Capacity', 'Energy']])
    plt.xticks(ticks=range(0, len(hoover), 12), labels=hoover.Year.unique())
    plt.xlabel('Year')
    plt.legend(['Capacity (AF)', 'Energy (mwh)'])
    plt.show()
    fig.savefig('results/hoover/hoover_energy.png')
    #$
    fig, ax = plt.subplots()
    ax.plot(hoover[['$ Amount', '$ Amount.1', 'Total']]/1e6)
    plt.xticks(ticks=range(0, len(hoover), 12), labels=hoover.Year.unique())
    plt.xlabel('Year')
    plt.ylabel('Cost ($M)')
    plt.legend(['Capacity', 'Energy', 'Total'])
    plt.show()
    fig.savefig('results/hoover/hoover_$.png')
    #correlation between capacity and rate
    fig, ax = plt.subplots()
    ax.scatter(hoover.Capacity,hoover.Energy)
    plt.xlabel('Capacity (af)')
    plt.ylabel('Energy(mwh)')
    plt.show()

    # wait, what is this capacity? correlation between capactiy and mead elevation
    mde = pd.read_csv('AllCAPData/LakeMead_HistoricalMonthlyElevation_2013_to_2022.csv',index_col = 'Year')
    mde = mde[mde.index > 2017]
    mde_hist = mde.stack().reset_index()
    #timeseries mead
    fig, ax = plt.subplots()
    ax.plot(mde_hist[0])
    plt.xlabel('date')
    plt.xticks(ticks=range(0, len(mde_hist), 12), labels=[2018,2019,2020,2021,2022])

    plt.ylabel('MDE')
    plt.show()

    fig, ax = plt.subplots()
    ax.scatter(hoover.iloc[:len(mde_hist)].Capacity,mde_hist[0])
    plt.xlabel('Capacity APA Data')
    plt.ylabel('MDE')
    plt.show()

    #just october
    mde_mo = mde.OCT
    hoover_mo = hoover[hoover.Month =='Oct']
    fig, ax = plt.subplots()
    ax.scatter(hoover_mo[:len(mde_mo)].Capacity,mde_mo)
    plt.xlabel('Capacity APA Data')
    plt.ylabel('MDE')
    plt.title('OCT')
    plt.show()

    #jan
    mde_mo = mde.JAN
    hoover_mo = hoover[hoover.Month =='Jan']
    fig, ax = plt.subplots()
    ax.scatter(hoover_mo[:len(mde_mo)].Capacity,mde_mo)
    plt.xlabel('Capacity APA Data')
    plt.ylabel('MDE')
    plt.title('JAN')
    plt.show()

    #sum
    x = hoover_total.Capacity
    y = mde.sum(axis=1)
    fig, ax = plt.subplots()
    ax.scatter(x[:len(y)-1],y[:-1])
    plt.xlabel('Capacity APA Data')
    plt.ylabel('MDE')
    plt.title('sum')
    plt.show()



if mead_allocation == 1:
    def mead_allocation_fig(runs, ti="", end_year="", legend=True, tiers=""):
        fig, ax = plt.subplots()
        for run in runs:
            mead = pd.read_csv('results/crss-' + str(run) + "/" + '10.2MW_solar_PPA' + "/results.csv")

            if end_year != "":
                idx = mead[mead.month_of_simulation == time].index[0]
                mead = mead[0:idx]

            ax.plot(mead.mead_cap_allocation, label=str(run))

            plt.ylabel('Projected Lake Mead CAP Allocation (KAF)')

            idx = mead.month_of_simulation % 12 == 1
            plt.xticks(ticks=mead[idx].index, labels=range(start_year, end_year))

            plt.xlabel('Year')
            #plt.ylim([900, 1150])
            plt.title('CRSS runs')
        if legend != False:
            ax.legend()
        if tiers != "":
            for t in tiers:
                plt.axhline(y=t, linestyle=':', color='black')
        if ti == "":
            fig.savefig('results/MDE_cap_allocation_crss_' + str(runs[0]) + '_' + str(runs[-1]) + '.png')
        else:
            fig.savefig('results/MDE_cap_allocation_' + str(ti))

    mead_allocation_fig(crss_runs, ti='all_2030', end_year=2030, legend=False)

    #agregating years
    def mead_allocation_yr_fig(runs, ti="", end_year="", start_year=2023, legend=True, tiers=""):
        fig, ax = plt.subplots()
        for run in runs:
            mead = pd.read_csv('results/crss-' + str(run) + "/" + '10.2MW_solar_PPA' + "/results.csv")

            if end_year != "":
                idx = mead[mead.month_of_simulation == time].index[0]
                mead = mead[0:idx]
            mead_yr=pd.DataFrame(columns = ['years','mead_cap_allocation'])
            mead_yr['year'] = range(start_year, end_year+1)
            idx = 0
            for y in range(0,int(time/12)):
                mead_yr.mead_cap_allocation[y] = mead.mead_cap_allocation.iloc[idx:idx+11].max()
                idx += 12

            ax.plot(mead_yr.mead_cap_allocation, label=str(run))

            plt.ylabel('Projected Lake Mead CAP Allocation (KAF)')

            #idx = mead.month_of_simulation % 12 == 1
            plt.xticks(ticks=mead_yr.index.values, labels=mead_yr.year)

            plt.xlabel('Year')
            #plt.ylim([900, 1150])
            plt.title('CAP Allocation from Model')
        if legend != False:
            ax.legend()
        if tiers != "":
            for t in tiers:
                plt.axhline(y=t, linestyle=':', color='black')
        if ti == "":
            fig.savefig('results/MDE_cap_allocation_crss_' + str(runs[0]) + '_' + str(runs[-1]) + '.png')
        else:
            fig.savefig('results/MDE_cap_allocation_' + str(ti))

    mead_allocation_yr_fig(crss_runs, ti='model_2030', end_year=2030, legend=True)

    #using adjusted cap allocation from madison
    def mead_allocation_fig2(runs: object, ti: object = "", end_year: object = "", legend: object = True, tiers: object = "") -> object:
        mead = pd.read_excel("CRSS_CAP_Processed_01302023.xlsx")
        fig, ax = plt.subplots()
        for run in runs:
            col = "Run" + str(run-1)
            if end_year != "":
                idx = mead[mead['CAPDiversionTotal Diversion Requested'] == end_year].index.values[0]
                mead = mead[0:idx + 1]

            ax.plot(mead[col]/1e3, label=str(run))

            plt.ylabel('Projected Lake Mead CAP Allocation (KAF)')

            #idx = mead.month_of_simulation % 12 == 1
            plt.xticks(ticks = mead.index.values, labels=mead['CAPDiversionTotal Diversion Requested'])

            plt.xlabel('Year')
            plt.ylim([600, 1900])
            plt.title('CAP Allocation from Madison')
        if legend != False:
            ax.legend()
        if tiers != "":
            for t in tiers:
                plt.axhline(y=t, linestyle=':', color='black')
        if ti == "":
            fig.savefig('results/MDE_cap_allocation_crss_' + str(runs[0]) + '_' + str(runs[-1]) + '.png')
        else:
            fig.savefig('results/MDE_cap_allocation_' + str(ti))


    mead_allocation_fig2([crss_runs[0]], ti='1madison_2030', end_year=2030, legend=False)
    mead_allocation_fig2(crss_runs[0:2], ti='2madison_2030', end_year=2030, legend=False)
    mead_allocation_fig2(crss_runs, ti='3madison_2030', end_year=2030, legend=False)


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
    cmap_gray = [cmap[0], cmap[1], '0.90', '0.95', '0.88', cmap[5]]
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
            #ax.bar(power_mo[power_mo.Product == p].Month, val, 0.35, label=p, bottom=b,
                   #color=cmap_power[cmap_power.Product == p].cmap)
            ax.bar(power_mo[power_mo.Product == p].Month, val, 0.35, label=p, bottom=b,
                   color='gray')
            b = b + val.values
            print(p)
        else:
            ax.bar(power_mo[power_mo.Product == p].Month, val, 0.35, label=p,
                   color=cmap_power[cmap_power.Product == p].cmap)


    #plt.legend()
    plt.ylabel("2022 Power Transactions ($M)")
    plt.show()
    fig.savefig('results/PPA_Total_Dollars_gray.png')

if historic_prices == 1:
    # looking at prices
    fig, ax = plt.subplots()
    power_price = power_data[power_data.variable == "Unit Price ($/MWH)"]
    power_price = power_price[power_price.Month != 'TOTAL']
    power_price.loc[power_price.Use == 'Sell', 'Product'] = "Day Ahead (sell)"
    i = 0
    for p in power_price.Product.unique():
        df = pd.DataFrame(power_price[power_price.Product == p].value.values, index=power_price.Month.unique())[0]
        df.plot(color = cmap_power[cmap_power.Product == p].cmap.values)
        i = i + 1
    plt.legend(power_price.Product.unique())
    plt.ylabel("Unit Price ($/MWH)")
    plt.xlabel("Month")
    plt.title("2022 CAP Power Prices")
    plt.show()

#####################################################################
# output of model-- power
#####################################################################
if results_timeseries_power == 1:
    cmap = set = mpl.colormaps['tab10'].colors
    for scenario in scenarios:
        fig, ax = plt.subplots()

        for crss in crss_runs:
            output = pd.read_csv('results/crss-' + str(crss) + "/" + scenario + "/results.csv")
            finances = output[output.filter(like='CAP').columns]
            del output
            finances = finances[0:time]
            (finances.CAP_total_pumping_cost / 1e6).plot()

        plt.xlabel('Month')
        plt.xticks(ticks=range(0, time+11, 12), labels=range(2023, 2031))
        plt.ylabel('Total Pumping Cost ($M)')
        plt.ylim([-2, 30])
        plt.xlim([-5, time + 5])
        plt.legend((1, 6, 13, 16))
        plt.show()
        fig.savefig('results/' + scenario + '.png')

    # timeseires comparing PPA scenarios

    for crss in crss_runs:
        fig, ax = plt.subplots()
        i = 0
        for scenario in scenarios:
            output = pd.read_csv('results/crss-' + str(crss) + "/" + scenario + "/results.csv")
            finances = output[output.filter(like='CAP').columns]
            del output
            finances = finances[0:time]
            (finances.CAP_total_pumping_cost / 1e6).plot()
            #if i == 0:
            #plt.axhline(y=finances.CAP_total_pumping_cost.mean()/1e6,linestyle ="--", color = cmap[i])
            i += 1

        plt.xlabel('Month')
        plt.xticks(ticks=range(0, time+11, 12), labels=range(2023, 2031))
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

if results_boxplot_power == 1:
    df = pd.DataFrame(columns=['CAP_total_pumping_cost', 'CAP_day_ahead_sales', 'scenario', 'crss'])
    for scenario in scenarios:
        for crss in crss_runs:
            output = pd.read_csv('results/crss-' + str(crss) + "/" + scenario + "/results.csv")
            finances = output[output.filter(like='CAP').columns]
            del output
            finances = finances[0:time]
            finances['CAP_total_pumping_cost'] = finances['CAP_total_pumping_cost']/1e6
            finances['scenario'] = scenario
            finances['crss'] = str(crss)
            df = df.append(finances[df.columns])

    fig, ax = plt.subplots(figsize=[10, 8])
    sns.boxplot(df, x='CAP_total_pumping_cost', y='crss', hue='scenario')
    plt.xlabel('Total Pumping Cost ($M)')
    plt.show()
    fig.savefig('results/boxplot_' + scenario + '.png')

    #histograms
    for crss in crss_runs:
        fig, ax = plt.subplots(figsize=[10, 8])
        sns.kdeplot(df[df.crss == str(crss)], x='CAP_total_pumping_cost', hue='scenario')
        i = 0
        for scenario in scenarios:
            dum = df[df.scenario == scenario]
            dum2 = dum[(dum.crss) == str(crss)]
            #if i == 0:
            ax.axvline(np.percentile(dum2.CAP_total_pumping_cost, 99),linestyle = "--", color = cmap[i])
            print(np.percentile(dum2.CAP_total_pumping_cost, 99))
            i = i + 1
        plt.ylabel('Frequency')
        plt.xlabel('Total Pumping Cost ($M)')
        plt.show()
        fig.savefig('results/hist_VAR2' + str(crss) + '_' + scenario + '.png')