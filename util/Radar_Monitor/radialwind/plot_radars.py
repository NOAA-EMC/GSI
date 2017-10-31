import pynemsio         # The nemsio reading library
import matplotlib       # Matplotlib to make graphics
matplotlib.use('Agg')   # Need this to generate figs when not running an Xserver (e.g. via PBS/LSF)
import ncepy, sys       # Need NCEPy for some handy little utilities.  Need sys to read cmd line args.
import numpy as np      # Need numpy for simple math and array manipulation
import matplotlib.pyplot as plt # More matplotlib stuff for plotting
from mpl_toolkits.basemap import Basemap, cm # Import basemap for plotting geographic data and cm

print('Starting plot_radars.py')
anal_time=sys.argv[1]
f1=str(sys.argv[2])
nio=pynemsio.nemsfile(f1)
numradars=int(sys.argv[3])
total_radars=int(sys.argv[4])
print('      number of radars in CONUS used = ',numradars)
print('Total number of radars in CONUS used = ',total_radars)
sids=np.chararray(numradars, itemsize=4); fsids=np.chararray(total_radars, itemsize=4) 
sids[:]='AAAA'; fsids[:]='AAAA'
lats=np.zeros(numradars); flats=np.zeros(total_radars)
lons=np.zeros(numradars); flons=np.zeros(total_radars)
hgts=np.zeros(numradars); fhgts=np.zeros(total_radars)
nobs=np.zeros(numradars)

for i in range(numradars):
    sids[i]=sys.argv[i+5+numradars*0]        
    lats[i]=sys.argv[i+5+numradars*1]
    lons[i]=sys.argv[i+5+numradars*2]
    hgts[i]=sys.argv[i+5+numradars*3]
    nobs[i]=sys.argv[i+5+numradars*4]
for j in range(total_radars):
    fsids[j]=sys.argv[j+5+numradars*5+total_radars*0]
    flats[j]=sys.argv[j+5+numradars*5+total_radars*1]
    flons[j]=sys.argv[j+5+numradars*5+total_radars*2]
    fhgts[j]=sys.argv[j+5+numradars*5+total_radars*3]
    print(j,fsids[j],flats[j],flons[j],fhgts[j])
    
fig = plt.figure(figsize=(12,12))
ax = fig.add_axes([0.1,0.1,0.8,0.8])
dom='CONUS'
proj='lcc' # Set map projection to lcc
llcrnrlon,llcrnrlat,urcrnrlon,urcrnrlat,res=ncepy.corners_res(dom,proj=proj)
# CREATE THE MAP BACKGROUND (LCC PROJECTION, CORNERS, REFERENCE LONGITUDE, etc.)
lat_1=25.0  # True latitude for the LCC projection
lon_0=-95.0 # Reference longitude for the LCC projection
m = Basemap(llcrnrlon=llcrnrlon,llcrnrlat=llcrnrlat,urcrnrlon=urcrnrlon,urcrnrlat=urcrnrlat,\
            rsphere=(6378137.00,6356752.3142),\
            resolution=res,projection=proj,\
            lat_1=lat_1,lon_0=lon_0,ax=ax)
# ADD SOME NICE GEOGRAPHIC FEATURES TO THE MAP.
m.drawcoastlines(linewidth=1.25)
m.drawstates(linewidth=1.25)
m.drawcountries(linewidth=1.25)
m.drawcounties(linewidth=0.2)
numRed=0; numGreen=0; numBlack=0

for i in range(numradars):
    tmpsid=sids[i]
    tmplat=lats[i]
    tmplon=lons[i]
    tmphgt=hgts[i]
    tmpnob=nobs[i]
    if(tmpnob == 0):
        numRed=numRed+1
        tmpcolor='red' # no obs
    if(tmpnob > 0):
        numGreen=numGreen+1
        tmpcolor='green'
    tmpi,tmpj=nio.ll_to_ij(tmplat,tmplon)
    m.scatter(nio.lons[tmpi,tmpj],nio.lats[tmpi,tmpj],s=175,color=tmpcolor,marker='*',latlon=True)

#plot a green and red star outside the domain for label. Probably not the best way to do this.
m.scatter(0,0,s=175,color='red',marker='*',latlon=True,label='0 observations')
m.scatter(0,0,s=175,color='green',marker='*',latlon=True,label='1+ observations')
m.scatter(0,0,s=175,color='black',marker='*',latlon=True,label='missing')


# PLOT BLACK FOR RADARS THAT ARE COMPLETELY MISSING
print('\n PLOT BLACK FOR RADARS THAT ARE COMPLETELY MISSING')
count=0
for j in range(total_radars):
    print(str(j+1)+'/'+str(total_radars))
    ltmp=False
    for i in range(numradars):
        if(fsids[j] == sids[i]):
            ltmp=True
            count=count+1
            print(fsids[j]+' '+sids[i]+' '+str(count))
            break
    if(not ltmp): #we want the ones that give false.
        tmplat=flats[j]
        tmplon=flons[j]
        numBlack=numBlack+1
        tmpcolor='black'
        tmpi,tmpj=nio.ll_to_ij(tmplat,tmplon)
        m.scatter(nio.lons[tmpi,tmpj],nio.lats[tmpi,tmpj],s=175,color=tmpcolor,marker='*',latlon=True)
        ltmp=False

plt.legend(loc='lower left',numpoints=1, ncol=3, fontsize=8, bbox_to_anchor=(0, 0))    
plt.show()
plt.title('Radars (not) used in assimilation for '+str(anal_time)+'\n' \
         +'Green = '+str(numGreen)+', Red = '+str(numRed)+', Black = '+str(numBlack)\
         +', Total = '+str(numGreen+numRed+numBlack))
plt.savefig(str(anal_time)+'_rwMon'+'.png',bbox_inches='tight')
