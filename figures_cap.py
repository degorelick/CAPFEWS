# By Rachel Kleiman
# Figure for Central Arizona Project models

import pandas as pd
import numpy as np
from matplotlib import pyplot as plt
from datetime import date



def crss_fig(runs,ti=""):
    fig, ax = plt.subplots()
    for run in runs:
        crss = pd.read_csv('calfews_src\data\input\cap-data-crss-' + str(run) + '.csv')

        ax.plot(crss.MDE_ele,label=str(run))

        plt.ylabel('Projected Lake Mead Elevation (ft)')

        d = pd.DatetimeIndex(crss.datetime)
        idx = np.logical_and(d.month == 1, d.year % 5 == 3)
        xl = d[idx].year

        plt.xticks(ticks=crss[idx].index, labels=xl)
        plt.xlabel('Year')
        plt.ylim([900,1150])
        plt.title('CRSS runs')
    ax.legend()
    if ti=="":
        fig.savefig('results\MDE_elevation_crss_'+str(runs[0])+'_'+str(runs[-1])+'.png')
    else:
        fig.savefig('results\MDE_elevation_crss_'+str(ti))

# looking at all crss runs
for y in range (1,27,5):
    if y<26:
        r=range(y,y+5)
    else:
        r=range(y,y+7)
    crss_fig(r)

#select crss run
crss_fig([1,6,13,16],ti='select')
