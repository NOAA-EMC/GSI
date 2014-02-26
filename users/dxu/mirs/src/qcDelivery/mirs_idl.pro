;***************************************************************************************************
; To check consistency of all MIRS inter-mediate files and final data files.
;
; History:
; 05/18/2011    Wanchun Chen        Original Coder
;
;
;***************************************************************************************************


@/net/orbit273l/disk1/pub/wchen/mirs_gfortran_linux_x64/setup/paths_idl.pro

Pro mirs_idl
Consts,Consts

;*******************************
; defined input files
;*******************************
file_tdr = 'TDR_SATMS_npp_d20100906_t0500127_e0500441_b00003_c20110321195442292903_noaa_ops.h5'
file_sdr = 'SDR_SATMS_npp_d20100906_t0500127_e0500441_b00003_c20110321195442292903_noaa_ops.h5'
file_fmsdr = 'FMSDR_TMS_npp_d20100906_t0500127_e0500441_b00003_c20110321195442292903_noaa_ops.h5.LR'
file_edr = 'EDR_TMS_npp_d20100906_t0500127_e0500441_b00003_c20110321195442292903_noaa_ops.h5.LR.ORB'
file_dep = 'DEP_TMS_npp_d20100906_t0500127_e0500441_b00003_c20110321195442292903_noaa_ops.h5.LR.ORB'
file_img = 'NPR-MIRS-IMG_v7_NPP_s20100906050012_e20100906050044_c20110518130548.nc'
file_snd = 'NPR-MIRS-SND_v7_NPP_s20100906050012_e20100906050044_c20110518130548.nc'


;*******************************
; define switches
;*******************************
stat_edr=1
stat_dep=1
stat_tdr=1
stat_sdr=1
stat_fmsdr=1
stat_img=1
stat_snd=1






;###################################################################################################

if stat_tdr eq 1 then begin
print, '####################  STATS FROM TDR ####################' 

nscan=0L
nfov=0L
nqc=0L
nchan=0L

openr,iu,file_tdr,/get_lun
readf,iu,format='(4i4)',nscan,nfov,nqc,nchan

;print,nscan,nfov,nqc,nchan

freqs=fltarr(nchan)
polars=lonarr(nchan)

readf,iu,format='(10f10.5)',freqs
readf,iu,format='(20I3)',polars

;print, freqs
;print, polars


node	 = 0L
scanDay  = 0L
scanYear = 0L
scanUTC  = 0L

lat	    = fltarr(nfov)
lon	    = fltarr(nfov)
angle	    = fltarr(nfov)
RelAziangle = fltarr(nfov)
SolZenangle = fltarr(nfov)
tb	    = fltarr(nfov,nchan)
qc	    = lonarr(nqc)

TBS = fltarr(nfov,nchan,nscan)
    
for iscan = 0, nscan-1 do begin

    readf,iu,format='(i4,3i10)', node,scanDAY,scanYear,scanUTC
    readf,iu,format='(10f8.2)', lat
    readf,iu,format='(10f8.2)', lon
    readf,iu,format='(10f8.2)', angle
    readf,iu,format='(10f8.2)', RelAziangle
    readf,iu,format='(10f8.2)', SolZenangle
    readf,iu,format='(10f8.2)', tb
    readf,iu,format='(10i4)',   qc
    
    TBS(*,*,iscan) = tb(*,*)
    
endfor

free_lun,iu

ss = where(TBS ge 0.0 and TBS le 300, NN)
;print,'-------- TDR --------'
print, 'NPOINT=', NN
print, 'mean(TB)=', MEAN(TBS(ss))
print, 'stdv(TB)=', STDDEV(TBS(ss))  
print, 'min(TB)=',  MIN(TBS(ss))  
print, 'max(TB)=',  MAX(TBS(ss))  

endif





;###################################################################################################

if stat_sdr eq 1 then begin
print, '####################  STATS FROM SDR ####################' 

nscan=0L
nfov=0L
nqc=0L
nchan=0L

openr,iu,file_sdr,/get_lun,/f77_unformatted,/swap_if_little_endian

readu,iu,nscan,nfov,nqc,nchan
;print,nscan,nfov,nqc,nchan 

freqs=fltarr(nchan)
polars=lonarr(nchan)
readu,iu,freqs
readu,iu,polars

;print, freqs
;print, polars

node	    = 0L
scanDay     = 0L
scanYear    = 0L
scanUTC     = 0L

lat	    = fltarr(nfov)
lon	    = fltarr(nfov)
angle	    = fltarr(nfov)
RelAziAngle = fltarr(nfov)
SolZenAngle = fltarr(nfov)
tb	    = fltarr(nfov,nchan)
qc	    = lonarr(nqc)

TBS = fltarr(nfov,nchan,nscan)
    
for iscan = 0, nscan-1 do begin
    Readu,iu, node
    Readu,iu, scanDAY,scanYear
    Readu,iu, scanUTC
    Readu,iu, lat
    Readu,iu, lon
    Readu,iu, angle
    Readu,iu, RelAziAngle
    Readu,iu, SolZenAngle
    Readu,iu, tb
    Readu,iu, qc

    TBS(*,*,iscan) = tb(*,*)

endfor
free_lun,iu

ss = where(TBS ge 0.0 and TBS le 300, NN)
;print,'-------- SDR --------'
print, 'NPOINT=', NN
print, 'mean(TB)=', MEAN(TBS(ss))
print, 'stdv(TB)=', STDDEV(TBS(ss))  
print, 'min(TB)=',  MIN(TBS(ss))  
print, 'max(TB)=',  MAX(TBS(ss))  

endif





;###################################################################################################

if stat_fmsdr eq 1 then begin
print, '####################  STATS FROM FMSDR ####################' 

openr,iu,file_fmsdr,/get_lun,/f77_unformatted,/swap_if_little_endian

nchan=0L
nprf=0L
nfov=0L
nscan=0L
nqc=0L
readu,iu,nprf 
readu,iu,nchan
readu,iu,nfov,nscan
readu,iu,nqc
freqs=fltarr(nchan)
polars=lonarr(nchan)
readu,iu,freqs
readu,iu,polars

lat=0.0
lon=0.0
RelAziAngle=0.0
SolZenAngle=0.0

node=0L
ifov=0L
iscan=0L
scanyear=0L
scanday=0L
utc=0.0

tmp=fltarr(nchan)

TBS = fltarr(nprf,nchan)

qc = lonarr(nqc)

for iprf = 0L, nprf - 1L do begin
  
  readu, iu, lat,lon,RelAziAngle,SolZenAngle
  readu,iu,node,ifov,iscan,scanyear,scanday,utc
  readu,iu,tmp
  
  readu,iu,tmp
  TBS(iprf,*) = tmp(*)
  readu,iu,qc
  
endfor

free_lun,iu

ss = where(TBS ge 0.0 and TBS le 300, NN)
;print,'-------- FMSDR --------'
print, 'NPOINT=', NN
print, 'mean(TB)=', MEAN(TBS(ss))
print, 'stdv(TB)=', STDDEV(TBS(ss))  
print, 'min(TB)=',  MIN(TBS(ss))  
print, 'max(TB)=',  MAX(TBS(ss))  

endif





;###################################################################################################

if stat_edr eq 1 then begin
print, '####################  STATS FROM EDR ####################' 

nprof2read=10000000L
LoadSceneFile,file_edr,topId,Scene,nprof2read
;help,Scene
  
print,''
print,'-------- TSKIN --------'
tskins = Scene.TSKINVEC
ss = where( tskins gt 0.0 and tskins le 350, NN )
tskin = tskins(ss)
print, 'NPOINT=', NN
print, 'MEAN(tskin)=', MEAN(tskin)
print, 'STDV(tskin)=', STDDEV(tskin)
print, 'MIN(tskin)=',  MIN(tskin)
print, 'MAX(tskin)=',  MAX(tskin)
print,''


print,'-------- EM1 --------'
ems = Scene.EMISSVEC(*,0)
ss = where( ems gt 0.0 and ems le 1.0, NN )
em = ems(ss)
print, 'NPOINT=', NN
print, 'MEAN(em1)=', MEAN(em)
print, 'STDV(em1)=', STDDEV(em)
print, 'MIN(em1)=',  MIN(em)
print, 'MAX(em1)=',  MAX(em)
print,''


print,'-------- TEMP @ 950mb --------'
TEMP10 = Scene.TEMPLAYVEC(*,94)
ss = where(TEMP10 gt 0.0 and TEMP10 le 350, NN )
TEMP = TEMP10(ss)
print, 'NPOINT=', NN
print, 'MEAN(t(950mb))=', MEAN(TEMP)
print, 'STDV(t(950mb))=', STDDEV(TEMP)
print, 'MIN(t(950mb))=',  MIN(TEMP)
print, 'MAX(t(950mb))=',  MAX(TEMP)
print,''


print,'-------- WV @ 950mb --------'
WV10 = Scene.ABSORBLAYVEC(*,94,0)
ss = where(WV10 gt 0.0 and WV10 le 30, NN )
WV = WV10(ss)
print, 'NPOINT=', NN
print, 'MEAN(wv(950mb))=', MEAN(WV)
print, 'STDV(wv(950mb))=', STDDEV(WV)
print, 'MIN(wv(950mb))=',  MIN(WV)
print, 'MAX(wv(950mb))=',  MAX(WV)
print,''

endif





;###################################################################################################

if stat_dep eq 1 then begin
print, '####################  STATS FROM DEP ####################' 

ReadDEP, file_dep, nprf, Dep
;help, Dep

print,''
print,'-------- CLW --------'
clws = Dep.CLW
ss = where(clws gt 0.0 and clws le 2,NN )
clw = clws(ss)
print, 'NPOINT=', NN
print, 'MEAN(clw)=', MEAN(clw)
print, 'STDV(clw)=', STDDEV(clw)
print, 'MIN(clw)=',  MIN(clw)
print, 'MAX(clw)=',  MAX(clw)
print,''


print,'-------- TPW --------'
tpws = Dep.TPW
sss = where(tpws gt 0.0 and tpws le 100,NN )
tpw = tpws(ss)
print, 'NPOINT=', NN
print, 'MEAN(tpw)=', MEAN(tpw)
print, 'STDV(tpw)=', STDDEV(tpw)
print, 'MIN(tpw)=',  MIN(tpw)
print, 'MAX(tpw)=',  MAX(tpw)
print,''


print,'-------- RWP --------'
rwps = Dep.RWP
ss = where(rwps gt 0.0 and rwps le 2,NN )
rwp = rwps(ss)
print, 'NPOINT=', NN
print, 'MEAN(rwp)=', MEAN(rwp)
print, 'STDV(rwp)=', STDDEV(rwp)
print, 'MIN(rwp)=',  MIN(rwp)
print, 'MAX(rwp)=',  MAX(rwp)
print,''


print,'-------- GWP --------'
gwps = Dep.GWP
ss = where(gwps gt 0.0 and gwps le 2,NN )
gwp = gwps(ss)
print, 'NPOINT=', NN
print, 'MEAN(gwp)=', MEAN(gwp)
print, 'STDV(gwp)=', STDDEV(gwp)
print, 'MIN(gwp)=',  MIN(gwp)
print, 'MAX(gwp)=',  MAX(gwp)
print,''

endif





;###################################################################################################

if stat_img eq 1 then begin
print, '####################  STATS FROM IMG ####################' 

Cdfid = NCDF_OPEN( file_img, /NOWRITE )


varid = NCDF_VARID(Cdfid, 'Qc')
ncdf_varget,Cdfid,varid,qc_dep

varid = NCDF_VARID(Cdfid, 'BT')
ncdf_varget,Cdfid,varid,bt

varid = NCDF_VARID(Cdfid, 'YM')
ncdf_varget,Cdfid,varid,ym

varid = NCDF_VARID(Cdfid, 'YFWD')
ncdf_varget,Cdfid,varid,yfwd

varid = NCDF_VARID(Cdfid, 'ChanSel')
ncdf_varget,Cdfid,varid,chansel

varid = NCDF_VARID(Cdfid, 'SurfP')
ncdf_varget,Cdfid,varid,psfc


varid = NCDF_VARID(Cdfid, 'TSkin')
ncdf_varget,Cdfid,varid,tskins
;print,tskins
print,'-------- TSKIN --------'
tskin = tskins * 0.01
ss = where(tskin gt 0.0 and tskin le 350,NN )
TSK = tskin(ss)
print, 'NPOINT=', NN
print, 'MEAN(tskin)=', MEAN(TSK)
print, 'STDV(tskin)=', STDDEV(TSK)
print, 'MIN(tskin)=',  MIN(TSK)
print, 'MAX(tskin)=',  MAX(TSK)
print,''


print,'-------- EM1 --------'
varid = NCDF_VARID(Cdfid, 'Emis')
ncdf_varget,Cdfid,varid,emis
emi= emis * 0.01
EM1=emi(0,*,*)
ss = where(EM1 gt 0.0 and EM1 le 1,NN )
EM = EM1(ss)
print, 'NPOINT=', NN
print, 'MEAN(em1)=', MEAN(EM)
print, 'STDV(em1)=', STDDEV(EM)
print, 'MIN(em1)=',  MIN(EM)
print, 'MAX(em1)=',  MAX(EM)
print,''


print,'-------- CLW --------'
varid = NCDF_VARID(Cdfid, 'CLW')
ncdf_varget,Cdfid,varid,clws
clw = clws * 0.01
ss = where(clw gt 0.0 and clw le 2,NN )
clw = clw(ss)
print, 'NPOINT=', NN
print, 'MEAN(clw)=', MEAN(clw)
print, 'STDV(clw)=', STDDEV(clw)
print, 'MIN(clw)=',  MIN(clw)
print, 'MAX(clw)=',  MAX(clw)
print,''


print,'-------- TPW --------'
varid = NCDF_VARID(Cdfid, 'TPW')
ncdf_varget,Cdfid,varid,tpws
tpw = tpws * 0.1
ss = where(tpw gt 0.0 and tpw le 100,NN )
tpw = tpw(ss)
print, 'NPOINT=', NN
print, 'MEAN(tpw)=', MEAN(tpw)
print, 'STDV(tpw)=', STDDEV(tpw)
print, 'MIN(tpw)=',  MIN(tpw)
print, 'MAX(tpw)=',  MAX(tpw)
print,''


print,'-------- RWP --------'
varid = NCDF_VARID(Cdfid, 'RWP')
ncdf_varget,Cdfid,varid,rwps
rwp = rwps * 0.01
ss = where(rwp gt 0.0 and rwp le 2,NN )
rwp = rwp(ss)
print, 'NPOINT=', NN
print, 'MEAN(rwp)=', MEAN(rwp)
print, 'STDV(rwp)=', STDDEV(rwp)
print, 'MIN(rwp)=',  MIN(rwp)
print, 'MAX(rwp)=',  MAX(rwp)
print,''


print,'-------- GWP --------'
varid = NCDF_VARID(Cdfid, 'GWP')
ncdf_varget,Cdfid,varid,gwps
gwp = gwps * 0.01
ss = where(gwp gt 0.00001 and gwp le 2,NN )
gwp = gwp(ss)
print, 'NPOINT=', NN
print, 'MEAN(gwp)=', MEAN(gwp)
print, 'STDV(gwp)=', STDDEV(gwp)
print, 'MIN(gwp)=',  MIN(gwp)
print, 'MAX(gwp)=',  MAX(gwp)
print,''


varid = NCDF_VARID(Cdfid, 'LWP')
ncdf_varget,Cdfid,varid,lwp

varid = NCDF_VARID(Cdfid, 'SWP')
ncdf_varget,Cdfid,varid,swp

varid = NCDF_VARID(Cdfid, 'IWP')
ncdf_varget,Cdfid,varid,iwp

varid = NCDF_VARID(Cdfid, 'RR')
ncdf_varget,Cdfid,varid,rr

varid = NCDF_VARID(Cdfid, 'SFR')
ncdf_varget,Cdfid,varid,sfr

varid = NCDF_VARID(Cdfid, 'RFlag')
ncdf_varget,Cdfid,varid,rflag

varid = NCDF_VARID(Cdfid, 'Snow')
ncdf_varget,Cdfid,varid,snow

varid = NCDF_VARID(Cdfid, 'SnowGS')
ncdf_varget,Cdfid,varid,snowgs

varid = NCDF_VARID(Cdfid, 'SurfM')
ncdf_varget,Cdfid,varid,sfcm

varid = NCDF_VARID(Cdfid, 'SIce')
ncdf_varget,Cdfid,varid,sice

varid = NCDF_VARID(Cdfid, 'SIce_MY')
ncdf_varget,Cdfid,varid,sicemy

varid = NCDF_VARID(Cdfid, 'SIce_FY')
ncdf_varget,Cdfid,varid,sicefy

NCDF_CLOSE, Cdfid

endif





;###################################################################################################

if stat_snd eq 1 then begin
print, '####################  STATS FROM SND ####################' 


Cdfid = NCDF_OPEN( file_snd, /NOWRITE )
;print, Cdfid
info = NCDF_INQUIRE(Cdfid)

Ndims = info.Ndims
Nvars = info.Nvars
Ngatts = info.Ngatts
RecDim = info.RecDim

;print, info.Ndims
;print, info.Nvars
;print, info.Ngatts
;print, info.RecDim

varid = NCDF_VARID(Cdfid, 'Freq')
ncdf_varget,Cdfid,varid,freq
;print, freq

varid = NCDF_VARID(Cdfid, 'Polo')
ncdf_varget,Cdfid,varid,polo


varid = NCDF_VARID(Cdfid, 'ScanTime_year')
ncdf_varget,Cdfid,varid,year

varid = NCDF_VARID(Cdfid, 'ScanTime_doy')
ncdf_varget,Cdfid,varid,doy

varid = NCDF_VARID(Cdfid, 'ScanTime_month')
ncdf_varget,Cdfid,varid,month

varid = NCDF_VARID(Cdfid, 'ScanTime_dom')
ncdf_varget,Cdfid,varid,dom

varid = NCDF_VARID(Cdfid, 'ScanTime_hour')
ncdf_varget,Cdfid,varid,hour

varid = NCDF_VARID(Cdfid, 'ScanTime_minute')
ncdf_varget,Cdfid,varid,minute

varid = NCDF_VARID(Cdfid, 'ScanTime_second')
ncdf_varget,Cdfid,varid,second

varid = NCDF_VARID(Cdfid, 'ScanTime_UTC')
ncdf_varget,Cdfid,varid,utc


varid = NCDF_VARID(Cdfid, 'Orb_mode')
ncdf_varget,Cdfid,varid,mode

varid = NCDF_VARID(Cdfid, 'Latitude')
ncdf_varget,Cdfid,varid,latitude
;print,latitude

varid = NCDF_VARID(Cdfid, 'Longitude')
ncdf_varget,Cdfid,varid,longitude

varid = NCDF_VARID(Cdfid, 'Sfc_type')
ncdf_varget,Cdfid,varid,sfctype

varid = NCDF_VARID(Cdfid, 'Atm_type')
ncdf_varget,Cdfid,varid,atmtype


varid = NCDF_VARID(Cdfid, 'Qc')
ncdf_varget,Cdfid,varid,qc


varid = NCDF_VARID(Cdfid, 'NAttempt')
ncdf_varget,Cdfid,varid,nattempt

varid = NCDF_VARID(Cdfid, 'NIter')
ncdf_varget,Cdfid,varid,niter

varid = NCDF_VARID(Cdfid, 'ChiSqr')
ncdf_varget,Cdfid,varid,chisq

varid = NCDF_VARID(Cdfid, 'LZ_angle')
ncdf_varget,Cdfid,varid,lz_angle

varid = NCDF_VARID(Cdfid, 'RAzi_angle')
ncdf_varget,Cdfid,varid,razi_angle

varid = NCDF_VARID(Cdfid, 'SZ_angle')
ncdf_varget,Cdfid,varid,sz_angle

varid = NCDF_VARID(Cdfid, 'Player')
ncdf_varget,Cdfid,varid,player
;help, player

varid = NCDF_VARID(Cdfid, 'Plevel')
ncdf_varget,Cdfid,varid,plevel
;help, plevel


varid = NCDF_VARID(Cdfid, 'PTemp')
ncdf_varget,Cdfid,varid,ptemp
;print, ptemp
;help, ptemp
print,'-------- TEMP @ 950mb --------'
TEMP10 = ptemp(94,*,*)
ss = where(TEMP10 gt 0.0 and TEMP10 le 350, NN )
TEMP = TEMP10(ss)
print, 'NPOINT=', NN
print, 'MEAN(t(950mb))=', MEAN(TEMP)
print, 'STDV(t(950mb))=', STDDEV(TEMP)
print, 'MIN(t(950mb))=',  MIN(TEMP)
print, 'MAX(t(950mb))=',  MAX(TEMP)
print,''


print,'-------- WV @ 950mb --------'
varid = NCDF_VARID(Cdfid, 'PVapor')
ncdf_varget,Cdfid,varid,pvapor
WV10 = pvapor(94,*,*)
ss = where(WV10 gt 0.0 and WV10 le 30, NN )
WV = WV10(ss)
print, 'NPOINT=', NN
print, 'MEAN(wv(950mb))=', MEAN(WV)
print, 'STDV(wv(950mb))=', STDDEV(WV)
print, 'MIN(wv(950mb))=',  MIN(WV)
print, 'MAX(wv(950mb))=',  MAX(WV)
print,''


varid = NCDF_VARID(Cdfid, 'POzone')
ncdf_varget,Cdfid,varid,pozone

varid = NCDF_VARID(Cdfid, 'PClw')
ncdf_varget,Cdfid,varid,pclw

varid = NCDF_VARID(Cdfid, 'PRain')
ncdf_varget,Cdfid,varid,prain

varid = NCDF_VARID(Cdfid, 'PGraupel')
ncdf_varget,Cdfid,varid,pgraupel

varid = NCDF_VARID(Cdfid, 'PSnow')
ncdf_varget,Cdfid,varid,psnow

varid = NCDF_VARID(Cdfid, 'PIce')
ncdf_varget,Cdfid,varid,pice

varid = NCDF_VARID(Cdfid, 'SurfP')
ncdf_varget,Cdfid,varid,psfc


attname = NCDF_ATTNAME( Cdfid, varid, 0)
ncdf_attget,Cdfid,varid,attname,value0

attname = NCDF_ATTNAME( Cdfid, varid, 1)
ncdf_attget,Cdfid,varid,attname,value1

NCDF_CLOSE, Cdfid

endif




End
