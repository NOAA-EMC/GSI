import osense
import pandas as pd


( convdata, satdata )= osense.read_osense('osense_2017091000.dat.out')

# creates a DataFrame with the mean of each satellite instrument
#meanbyobtype = satdata.groupby('obtype').mean()

meanbyobtype=satdata[['obtype','osense_kin','osense_dry','osense_moist']].groupby('obtype').mean()
#cmeanbyobtype=convdata[['stattype','osense_kin','osense_dry','osense_moist']].groupby('stattype').mean()

convcodes = pd.read_csv('convdata_codes.csv')

# associate each data point with its source, by code
# it would be more efficient to take the mean of the observation sensitivities by
# code/stattype, but this way the mean is by the message column in the codes
# (ADPUPA, AIRCRAFT, etc) to consolidate for simpler graphing. Taking the mean 
# by code/stattype would break it down by data source more
convbycodes=pd.merge(convdata,convcodes,how='left',left_on='stattype', right_on='code')

convmean=convbycodes[['message','osense_kin','osense_dry','osense_moist']].groupby('message').mean()
alltheobtypes=pd.concat([meanbyobtype,convmean])

figuresize = (10,6)

o_dry=alltheobtypes['osense_dry'].sort_values(ascending=False)
dry_plot=o_dry.plot.barh(title = '2017091000 mean obs sensitivity, dry', figsize=figuresize);
fig=dry_plot.get_figure()
fig.savefig('obsense_dry.png',bbox_inches='tight')

fig.clear()

o_moist=alltheobtypes['osense_moist'].sort_values(ascending=False)
moist_plot=o_moist.plot.barh(title = '2017091000 mean obs sensitivity, moist', figsize=figuresize);
fig=moist_plot.get_figure()
fig.savefig('obsense_moist.png',bbox_inches='tight')

fig.clear()

o_kin=alltheobtypes['osense_kin'].sort_values(ascending=False)
kin_plot=o_kin.plot.barh(title = '2017091000 mean obs sensitivity, kinetic', figsize=figuresize);
fig=kin_plot.get_figure()
fig.savefig('obsense_kin.png',bbox_inches='tight')

fig.clear()



