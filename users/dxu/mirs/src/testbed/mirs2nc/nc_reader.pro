;===============================================================================
;
; Name:  nc_reader.pro
;
; Type:  IDL Program
;
;
; Description:
;   Simple stand-alone IDL code to read converted IMG/SND netcdf4 files.
;   You can just use hdfview to view all content of nc files.
;
; Requirements:
;   -none
;
; History:
;   Wanchun Chen  2012-03-01  Original coder
;
;===============================================================================

file_snd='NPR-MIRS-SND_v7_NPP_s201204152359123_e201204152359439_c20120416162912.nc'


Cdfid = NCDF_OPEN( file_snd, /NOWRITE )
print, Cdfid
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

varid = NCDF_VARID(Cdfid, 'Longitude')
ncdf_varget,Cdfid,varid,longitude

varid = NCDF_VARID(Cdfid, 'Sfc_type')
ncdf_varget,Cdfid,varid,sfctype

varid = NCDF_VARID(Cdfid, 'Atm_type')
ncdf_varget,Cdfid,varid,atmtype


varid = NCDF_VARID(Cdfid, 'Qc')
ncdf_varget,Cdfid,varid,qc


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
help, player

varid = NCDF_VARID(Cdfid, 'Plevel')
ncdf_varget,Cdfid,varid,plevel
help, plevel

varid = NCDF_VARID(Cdfid, 'PTemp')
ncdf_varget,Cdfid,varid,ptemp

varid = NCDF_VARID(Cdfid, 'PVapor')
ncdf_varget,Cdfid,varid,pvapor

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



file_img='NPR-MIRS-IMG_v7_NPP_s201204152359123_e201204152359439_c20120416162912.nc'


Cdfid = NCDF_OPEN( file_img, /NOWRITE )


varid = NCDF_VARID(Cdfid, 'Qc')
ncdf_varget,Cdfid,varid,qc_dep

varid = NCDF_VARID(Cdfid, 'BT')
ncdf_varget,Cdfid,varid,bt

varid = NCDF_VARID(Cdfid, 'YM')
ncdf_varget,Cdfid,varid,ym

varid = NCDF_VARID(Cdfid, 'ChanSel')
ncdf_varget,Cdfid,varid,chansel



varid = NCDF_VARID(Cdfid, 'TPW')
ncdf_varget,Cdfid,varid,tpw

varid = NCDF_VARID(Cdfid, 'CLW')
ncdf_varget,Cdfid,varid,clw

varid = NCDF_VARID(Cdfid, 'RWP')
ncdf_varget,Cdfid,varid,rwp

varid = NCDF_VARID(Cdfid, 'LWP')
ncdf_varget,Cdfid,varid,lwp

varid = NCDF_VARID(Cdfid, 'SWP')
ncdf_varget,Cdfid,varid,swp

varid = NCDF_VARID(Cdfid, 'IWP')
ncdf_varget,Cdfid,varid,iwp

varid = NCDF_VARID(Cdfid, 'GWP')
ncdf_varget,Cdfid,varid,gwp

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

varid = NCDF_VARID(Cdfid, 'TSkin')
ncdf_varget,Cdfid,varid,tskin

help, tskin

varid = NCDF_VARID(Cdfid, 'SurfP')
ncdf_varget,Cdfid,varid,psfc


varid = NCDF_VARID(Cdfid, 'Emis')
ncdf_varget,Cdfid,varid,emis


NCDF_CLOSE, Cdfid


End
