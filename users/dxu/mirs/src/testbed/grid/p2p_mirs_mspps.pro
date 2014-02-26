@../../../setup/paths_idl.pro
;===================================================================================================
;
;  Purpose:
;    To Plot gridded data set into png images.
;
;  Dependence:
;    plot_grid.pro
;
;  Record of revisions:
;        Date          Programmer     	Description of change
;    ============    ==============    ======================
;     09/01/07        Wanchun Chen     Original Code
;     03/16/10        Wanchun Chen     Added LST
;
;===================================================================================================

PRO P2P_MIRS_MSPPS, nameList=namelist


;***************************************************************
;    some constants definitions
;***************************************************************
satId='metopA'
gridfactor=4
gridDir='/disk1/pub/mirs_operational/data/TestbedData/Outputs/grid/metopA_amsua_mhs/2011-07-31/'
figsDir='./'
date='2011-07-31'
processMode=1
version='2147'
edr_file_list='/disk1/pub/mirs_operational/data/InputsData/metopA_edrFiles_4Bias_2011-07-31.list'
dep_file_list='/disk1/pub/mirs_operational/data/InputsData/metopA_depFiles_4Bias_2011-07-31.list'
nwp_file_list='/disk1/pub/mirs_operational/data/InputsData/metopA_NWPanalysFiles_4Bias_2011-07-31.list_gdas'
biasPath='./'
latmin=-90
latmax=90
lonmin=-180
lonmax=180

fromNameList=1

if ( fromNameList eq 1 ) then begin
  openr,iu,namelist,/get_lun 
  readf,iu,format='(a)', satId		       	;Satellite ID
  readf,iu,format='(i)', gridfactor	       	;gridfactor
  readf,iu,format='(a)', gridDir  	       	;gridded data path
  readf,iu,format='(a)', figsDir  	       	;path where to put image files
  readf,iu,format='(a)', date		       	;file extension of image
  readf,iu,format='(a)', version    		;version number
  readf,iu,format='(a)', edr_file_list	       	;EDR file list
  readf,iu,format='(a)', dep_file_list	       	;DEP file list
  readf,iu,format='(a)', nwp_file_list	       	;NWP file list
  readf,iu,format='(a)', biasPath    		;biasPath
  readf,iu,format='(f)', latmin              	;min lat
  readf,iu,format='(f)', latmax              	;max lat
  readf,iu,format='(f)', lonmin              	;min lon
  readf,iu,format='(f)', lonmax              	;max lon
  close,iu
  free_lun,iu,/force
endif

sensor_id = 1
if ( satId eq 'n18'    ) then sensor_id = 1
if ( satId eq 'n19'    ) then sensor_id = 4
if ( satId eq 'metopA' ) then sensor_id = 2
if ( satId eq 'metopB' ) then sensor_id = 14


if ( processMode eq 0 ) then yyyymmdd=date
if ( processMode eq 1 ) then yyyymmdd=strmid(date,0,4)+strmid(date,5,2)+strmid(date,8,2)

;***************************************************************
;    some flgs to turn on/off to plot or not
;***************************************************************
PLOT_CLW_MSPPS=1
PLOT_CLW_MIRS=0
PLOT_LWP_MIRS=0

PLOT_SWE_MSPPS=1
PLOT_SWE_MIRS=0

PLOT_SIC_MSPPS=1
PLOT_SIC_MIRS=0

PLOT_EM1_MSPPS=1
PLOT_EM1_MIRS=0

PLOT_EM2_MSPPS=1
PLOT_EM2_MIRS=0

PLOT_EM3_MSPPS=1
PLOT_EM3_MIRS=0

PLOT_TSKIN_MIRS = 0
PLOT_TSKIN_MSPPS = 1

PLOT_IWP_MIRS = 0
PLOT_IWP_MSPPS = 1

PLOT_RR_MIRS = 0
PLOT_RR_MSPPS = 1

PLOT_POLAR_SIC_MIRS = 0
PLOT_POLAR_SIC_MSPPS = 0

PLOT_POLAR_SWE_MIRS = 0
PLOT_POLAR_SWE_MSPPS = 0

PLOT_POLAR_SFC_MIRS = 0
PLOT_POLAR_SFC_MSPPS = 0


;***************************************************************
;    P2P identifiers
;***************************************************************
prefix='mirs_adv_poes_'+satId+'_amsuamhs_glb_'
prefix_mspps='mirs_adv_poes_'+satId+'_mspps_glb_'
prefix_diff='mirs_adv_poes_'+satId+'_mspps_diff_glb_'

readlist,dep_file_list,filesDep,nfile
if ( nfile eq 0 ) then begin
    print, 'Error: No files found in :', dep_file_list
    exit
endif
FILEMAX=nfile

PROFMAX=0L

FOR ifile=0L,nfile-1L DO BEGIN
  iTyp       = 0L
  AlgSN      = 0L
  nPrf       = 0L
  iu	     = 0L
  OPENR,iu,filesDep(ifile),/get_lun,error=err,/f77_unformatted,/swap_if_little_endian
  IF (err ne 0) THEN BEGIN
      print ,!ERROR_STATE.MSG
      RETURN
  ENDIF
  readu,iu,iTyp,AlgSN
  readu,iu,nPrf
  close,iu
  free_lun,iu
  if nPrf gt PROFMAX then PROFMAX=nPrf
ENDFOR

print, 'FILEMAX=',FILEMAX
print, 'PROFMAX=',PROFMAX


MIRS_CLW_AS=fltarr(PROFMAX,FILEMAX)   & MSPPS_CLW_AS=fltarr(PROFMAX,FILEMAX)
MIRS_SIC_AS=fltarr(PROFMAX,FILEMAX)   & MSPPS_SIC_AS=fltarr(PROFMAX,FILEMAX)
MIRS_SWE_AS=fltarr(PROFMAX,FILEMAX)   & MSPPS_SWE_AS=fltarr(PROFMAX,FILEMAX)
MIRS_EM1_AS=fltarr(PROFMAX,FILEMAX)   & MSPPS_EM1_AS=fltarr(PROFMAX,FILEMAX)
MIRS_EM2_AS=fltarr(PROFMAX,FILEMAX)   & MSPPS_EM2_AS=fltarr(PROFMAX,FILEMAX)
MIRS_EM3_AS=fltarr(PROFMAX,FILEMAX)   & MSPPS_EM3_AS=fltarr(PROFMAX,FILEMAX)
MIRS_TSKIN_AS=fltarr(PROFMAX,FILEMAX) & MSPPS_TSKIN_AS=fltarr(PROFMAX,FILEMAX)
MIRS_LWP_AS=fltarr(PROFMAX,FILEMAX)

MIRS_CLW_DS=fltarr(PROFMAX,FILEMAX)   & MSPPS_CLW_DS=fltarr(PROFMAX,FILEMAX)
MIRS_SIC_DS=fltarr(PROFMAX,FILEMAX)   & MSPPS_SIC_DS=fltarr(PROFMAX,FILEMAX)
MIRS_SWE_DS=fltarr(PROFMAX,FILEMAX)   & MSPPS_SWE_DS=fltarr(PROFMAX,FILEMAX)
MIRS_EM1_DS=fltarr(PROFMAX,FILEMAX)   & MSPPS_EM1_DS=fltarr(PROFMAX,FILEMAX)
MIRS_EM2_DS=fltarr(PROFMAX,FILEMAX)   & MSPPS_EM2_DS=fltarr(PROFMAX,FILEMAX)
MIRS_EM3_DS=fltarr(PROFMAX,FILEMAX)   & MSPPS_EM3_DS=fltarr(PROFMAX,FILEMAX)
MIRS_TSKIN_DS=fltarr(PROFMAX,FILEMAX) & MSPPS_TSKIN_DS=fltarr(PROFMAX,FILEMAX)
MIRS_LWP_DS=fltarr(PROFMAX,FILEMAX)

MIRS_SFC_AS=intarr(PROFMAX,FILEMAX)
MIRS_SFC_DS=intarr(PROFMAX,FILEMAX)

MIRS_SFC2_AS=intarr(PROFMAX,FILEMAX)
MIRS_SFC2_DS=intarr(PROFMAX,FILEMAX)

MIRS_IWP_AS=fltarr(PROFMAX,FILEMAX)  & MSPPS_IWP_AS=fltarr(PROFMAX,FILEMAX)
MIRS_IWP_DS=fltarr(PROFMAX,FILEMAX)  & MSPPS_IWP_DS=fltarr(PROFMAX,FILEMAX)
MIRS_RR_AS=fltarr(PROFMAX,FILEMAX)   & MSPPS_RR_AS=fltarr(PROFMAX,FILEMAX)
MIRS_RR_DS=fltarr(PROFMAX,FILEMAX)   & MSPPS_RR_DS=fltarr(PROFMAX,FILEMAX)

LAT_AS=fltarr(PROFMAX,FILEMAX)
LAT_DS=fltarr(PROFMAX,FILEMAX)
LON_AS=fltarr(PROFMAX,FILEMAX)
LON_DS=fltarr(PROFMAX,FILEMAX)

MIRS_QC_AS=intarr(PROFMAX,FILEMAX)
MIRS_QC_DS=intarr(PROFMAX,FILEMAX)

MIRS_CLW_AS(*,*) 	= -999.0
MIRS_LWP_AS(*,*) 	= -999.0
MIRS_SIC_AS(*,*) 	= -999.0
MIRS_SWE_AS(*,*) 	= -999.0
MIRS_EM1_AS(*,*) 	= -999.0
MIRS_EM2_AS(*,*) 	= -999.0
MIRS_EM3_AS(*,*) 	= -999.0
MIRS_TSKIN_AS(*,*) 	= -999.0

MIRS_CLW_DS(*,*) 	= -999.0
MIRS_LWP_DS(*,*) 	= -999.0
MIRS_SIC_DS(*,*) 	= -999.0
MIRS_SWE_DS(*,*) 	= -999.0
MIRS_EM1_DS(*,*) 	= -999.0
MIRS_EM2_DS(*,*) 	= -999.0
MIRS_EM3_DS(*,*) 	= -999.0
MIRS_TSKIN_DS(*,*) 	= -999.0


MSPPS_CLW_AS(*,*) 	= -999.0
MSPPS_SIC_AS(*,*) 	= -999.0
MSPPS_SWE_AS(*,*) 	= -999.0
MSPPS_EM1_AS(*,*) 	= -999.0
MSPPS_EM2_AS(*,*) 	= -999.0
MSPPS_EM3_AS(*,*) 	= -999.0
MSPPS_TSKIN_AS(*,*) 	= -999.0

MSPPS_CLW_DS(*,*) 	= -999.0
MSPPS_SIC_DS(*,*) 	= -999.0
MSPPS_SWE_DS(*,*) 	= -999.0
MSPPS_EM1_DS(*,*) 	= -999.0
MSPPS_EM2_DS(*,*) 	= -999.0
MSPPS_EM3_DS(*,*) 	= -999.0
MSPPS_TSKIN_DS(*,*) 	= -999.0

MIRS_SFC_AS(*,*) 	= -999.0
MIRS_SFC_DS(*,*) 	= -999.0

MIRS_SFC2_AS(*,*) 	= -999.0
MIRS_SFC2_DS(*,*) 	= -999.0

MIRS_IWP_AS(*,*)        = -999.0
MIRS_IWP_DS(*,*)        = -999.0
MIRS_RR_AS(*,*)         = -999.0
MIRS_RR_DS(*,*)         = -999.0

MSPPS_IWP_AS(*,*) 	= -999.0
MSPPS_IWP_DS(*,*) 	= -999.0
MSPPS_RR_AS(*,*) 	= -999.0
MSPPS_RR_DS(*,*) 	= -999.0


LAT_AS(*,*) = -999.0
LAT_DS(*,*) = -999.0
LON_AS(*,*) = -999.0
LON_DS(*,*) = -999.0

MIRS_QC_AS(*,*) = -999.0
MIRS_QC_DS(*,*) = -999.0


;***************************************************************
;    grid identifiers
;***************************************************************
NCOL=360*gridfactor
NROW=180*gridfactor

grid_angle_mirs_as = fltarr(NCOL,NROW) 
grid_angle_mirs_ds = fltarr(NCOL,NROW)

grid_sfc2_mirs_as = fltarr(NCOL,NROW) 
grid_sfc2_mirs_ds = fltarr(NCOL,NROW)

grid_clw_mirs_as = fltarr(NCOL,NROW) 
grid_clw_mirs_ds = fltarr(NCOL,NROW)

grid_lwp_mirs_as = fltarr(NCOL,NROW) 
grid_lwp_mirs_ds = fltarr(NCOL,NROW)

grid_sic_mirs_as = fltarr(NCOL,NROW)
grid_sic_mirs_ds = fltarr(NCOL,NROW)

grid_swe_mirs_as = fltarr(NCOL,NROW)
grid_swe_mirs_ds = fltarr(NCOL,NROW)

grid_em1_mirs_as = fltarr(NCOL,NROW)
grid_em1_mirs_ds = fltarr(NCOL,NROW)

grid_em2_mirs_as = fltarr(NCOL,NROW)
grid_em2_mirs_ds = fltarr(NCOL,NROW)

grid_em3_mirs_as = fltarr(NCOL,NROW)
grid_em3_mirs_ds = fltarr(NCOL,NROW)

grid_tskin_mirs_as = fltarr(NCOL,NROW)
grid_tskin_mirs_ds = fltarr(NCOL,NROW)

grid_iwp_mirs_as = fltarr(NCOL,NROW)
grid_iwp_mirs_ds = fltarr(NCOL,NROW)

grid_rr_mirs_as = fltarr(NCOL,NROW)
grid_rr_mirs_ds = fltarr(NCOL,NROW)

grid_clw_mspps_as = fltarr(NCOL,NROW)
grid_clw_mspps_ds = fltarr(NCOL,NROW)

grid_sic_mspps_as = fltarr(NCOL,NROW)
grid_sic_mspps_ds = fltarr(NCOL,NROW)

grid_swe_mspps_as = fltarr(NCOL,NROW)
grid_swe_mspps_ds = fltarr(NCOL,NROW)

grid_em1_mspps_as = fltarr(NCOL,NROW)
grid_em1_mspps_ds = fltarr(NCOL,NROW)

grid_em2_mspps_as = fltarr(NCOL,NROW)
grid_em2_mspps_ds = fltarr(NCOL,NROW)

grid_em3_mspps_as = fltarr(NCOL,NROW)
grid_em3_mspps_ds = fltarr(NCOL,NROW)

grid_tskin_mspps_as = fltarr(NCOL,NROW)
grid_tskin_mspps_ds = fltarr(NCOL,NROW)

grid_iwp_mspps_as = fltarr(NCOL,NROW)
grid_iwp_mspps_ds = fltarr(NCOL,NROW)

grid_rr_mspps_as = fltarr(NCOL,NROW)
grid_rr_mspps_ds = fltarr(NCOL,NROW)


grid_angle_mirs_as(*,*) = -999.0
grid_angle_mirs_ds(*,*) = -999.0

grid_sfc2_mirs_as(*,*) = -999.0
grid_sfc2_mirs_ds(*,*) = -999.0

grid_clw_mirs_as(*,*) = -999.0
grid_clw_mirs_ds(*,*) = -999.0

grid_lwp_mirs_as(*,*) = -999.0
grid_lwp_mirs_ds(*,*) = -999.0

grid_sic_mirs_as(*,*) = -999.0 
grid_sic_mirs_ds(*,*) = -999.0 

grid_swe_mirs_as(*,*) = -999.0 
grid_swe_mirs_ds(*,*) = -999.0

grid_em1_mirs_as(*,*) = -999.0 
grid_em1_mirs_ds(*,*) = -999.0

grid_em2_mirs_as(*,*) = -999.0 
grid_em2_mirs_ds(*,*) = -999.0

grid_em3_mirs_as(*,*) = -999.0 
grid_em3_mirs_ds(*,*) = -999.0

grid_tskin_mirs_as(*,*) = -999.0 
grid_tskin_mirs_ds(*,*) = -999.0

grid_iwp_mirs_as(*,*) = -999.0 
grid_iwp_mirs_ds(*,*) = -999.0

grid_rr_mirs_as(*,*) = -999.0 
grid_rr_mirs_ds(*,*) = -999.0


grid_clw_mspps_as(*,*) = -999.0 
grid_clw_mspps_ds(*,*) = -999.0 

grid_sic_mspps_as(*,*) = -999.0 
grid_sic_mspps_ds(*,*) = -999.0

grid_swe_mspps_as(*,*) = -999.0 
grid_swe_mspps_ds(*,*) = -999.0

grid_em1_mspps_as(*,*) = -999.0 
grid_em1_mspps_ds(*,*) = -999.0

grid_em2_mspps_as(*,*) = -999.0 
grid_em2_mspps_ds(*,*) = -999.0

grid_em3_mspps_as(*,*) = -999.0 
grid_em3_mspps_ds(*,*) = -999.0

grid_tskin_mspps_as(*,*) = -999.0 
grid_tskin_mspps_ds(*,*) = -999.0

grid_iwp_mspps_as(*,*) = -999.0 
grid_iwp_mspps_ds(*,*) = -999.0

grid_rr_mspps_as(*,*) = -999.0 
grid_rr_mspps_ds(*,*) = -999.0

nadir_as = fltarr(NCOL,NROW) & nadir_as(*,*) = 30
nadir_ds = fltarr(NCOL,NROW) & nadir_ds(*,*) = 30



;**************************
; filling in stuff
;**************************
NUMSPOT_A=30
fov_size_A = fltarr(NUMSPOT_A)
FOV_A, fov_size_A

PI=3.141593
adjustment=0.5
LATLIM_A=12


;***************************************************************
;    read file from file list
;***************************************************************
close,/all
openr, 1, dep_file_list
openr, 2, edr_file_list
openr, 3, nwp_file_list

MM=0L
file_dep=''
file_edr=''
file_nwp=''


WHILE ( NOT EOF(1) ) DO BEGIN

  readf, 1, file_dep, format='(a)'
  print, file_dep
  
  readf, 2, file_edr, format='(a)'
  print, file_edr
  
  readf, 3, file_nwp, format='(a)'
  print, file_nwp

  ;***************************************************************************************************
  ; read out mirs product values from DEP
  ;***************************************************************************************************
  ReadDEP, file_dep, nprofiles, Dep

  ;***************************************************************************************************
  ; read out mirs TB from EDR files
  ;***************************************************************************************************
  nProfs2read = 300000L
  nProfs2readNwp = 300000L
  LoadSceneFile,file_edr,topId,Scene,nProfs2read
  LoadSceneFile,file_nwp,topIdNwp,SceneNwp,nProfs2readNwp

  ;***************************************************************************************************
  ; point to point matching
  ;***************************************************************************************************
  iprf=0
  while ( iprf lt Dep.nProfsProcessed ) do begin
    
    ;---- compute MSPPS product
    tb23 = Scene.Ym(iprf,0)
    tb31 = Scene.Ym(iprf,1)
    tb50 = Scene.Ym(iprf,2)
    TB4  = Scene.Ym(iprf,3)
    TB5  = Scene.Ym(iprf,4)
    TB15 = Scene.Ym(iprf,14)
    TB16 = Scene.Ym(iprf,15)
    TB17 = Scene.Ym(iprf,16)
    TB18 = Scene.Ym(iprf,17)
    TB19 = Scene.Ym(iprf,18)
    TB20 = Scene.Ym(iprf,19)
    
    lat  = Scene.lat(iprf)
    lon  = Scene.lon(iprf)
    angle= Scene.AnglVec(iprf)
    iScanPos = Scene.ScanPos(iprf)
    node = Scene.direc(iprf)
    tb89 = Scene.Ym(iprf,15)
    
    tskin = SceneNwp.TskinVec(iprf)
    e23   = SceneNwp.EmissVec(iprf,0)
    e31   = SceneNwp.EmissVec(iprf,1)
    e50   = SceneNwp.EmissVec(iprf,2)
    e89   = SceneNwp.EmissVec(iprf,15)
    e157  = SceneNwp.EmissVec(iprf,16)
    
    sic=-999.0
    swe=-999.0
    snow=-999.0
    clw=-999.0
    tpw=-999.0
    em1=-999.0
    em2=-999.0
    em3=-999.0
    lst=-999.0
    iwp = -999.0
    de  = -999.0
    rr  = -999.0
    
    sfc2 = Dep.iTypSfc(iprf)
    
    if ( Scene.SfcTypVec(iprf) eq 0 or Scene.SfcTypVec(iprf) eq 1 ) then $
    	mspps_ocean,sensor_id,tb23,tb31,tb50,tskin,e23,e31,lat,node,angle, tpw,clw,sic

    if ( Scene.SfcTypVec(iprf) eq 2 or Scene.SfcTypVec(iprf) eq 3 ) then $
    	mspps_land,tb23,tb31,tb50,TB4,TB5,TB15,TB17,TB19,lat,lon,angle, lst,em1,em2,em3,snow,swe

    mspps_rain, Scene.SfcTypVec(iprf),lat,lon,tb23,tb31,tb89,TB17,TB18,TB19,TB20,angle,tskin,e89,e157,clw,tpw, iwp,de,rr  
    if ( (rr-1.52523) lt 0.0001) then rr = 0.
    if ( sic gt 0 ) then rr = -999.
    
    ;---- P2P goes here -----
    if ( Dep.node(iprf) eq 0 ) then begin

        LAT_AS(iprf,MM)       = lat
        LON_AS(iprf,MM)       = lon

    	MSPPS_SIC_AS(iprf,MM) = sic
    	MSPPS_SWE_AS(iprf,MM) = swe
    	MSPPS_CLW_AS(iprf,MM) = clw
    	MSPPS_EM1_AS(iprf,MM) = em1
    	MSPPS_EM2_AS(iprf,MM) = em2
    	MSPPS_EM3_AS(iprf,MM) = em3
	MSPPS_TSKIN_AS(iprf,MM) = lst
        MSPPS_IWP_AS(iprf,MM)   = iwp
        MSPPS_RR_AS(iprf,MM)    =  rr
	
    	MIRS_SIC_AS(iprf,MM)  = Dep.SIC(iprf)
    	MIRS_SWE_AS(iprf,MM)  = Dep.SWE(iprf)
    	MIRS_CLW_AS(iprf,MM)  = Dep.CLW(iprf)
        MIRS_LWP_AS(iprf,MM)  = Dep.LWP(iprf)
	MIRS_SFC_AS(iprf,MM)  = Scene.SfcTypVec(iprf)
	MIRS_SFC2_AS(iprf,MM) = Dep.iTypSfc(iprf)
    	MIRS_EM1_AS(iprf,MM)  = e23
    	MIRS_EM2_AS(iprf,MM)  = e31
    	MIRS_EM3_AS(iprf,MM)  = e50
	MIRS_TSKIN_AS(iprf,MM)= tskin
        MIRS_IWP_AS(iprf,MM)  = DEP.GWP(iprf)
        MIRS_RR_AS(iprf,MM)   = DEP.RR(iprf)
	MIRS_QC_AS(iprf,MM)   = DEP.qc(0,iprf)

        if ( Dep.qc(0,iprf) eq 2) then begin
            MIRS_SIC_AS(iprf,MM)  = -99.
            MIRS_SWE_AS(iprf,MM)  = -99.
            MIRS_CLW_AS(iprf,MM)  = -99.
            MIRS_LWP_AS(iprf,MM)  = -99.
            MIRS_SFC_AS(iprf,MM)  = -99.
            MIRS_SFC2_AS(iprf,MM) = -99.
            MIRS_EM1_AS(iprf,MM)  = -99.
            MIRS_EM2_AS(iprf,MM)  = -99.
            MIRS_EM3_AS(iprf,MM)  = -99.
            MIRS_TSKIN_AS(iprf,MM)= -99.
            MIRS_IWP_AS(iprf,MM)  = -99.
            MIRS_RR_AS(iprf,MM)   = -99.
        endif
	
    endif
    
    if ( Dep.node(iprf) eq 1 ) then begin

	LAT_DS(iprf,MM)       = lat
        LON_DS(iprf,MM)       = lon

    	MSPPS_SIC_DS(iprf,MM) = sic
    	MSPPS_SWE_DS(iprf,MM) = swe
    	MSPPS_CLW_DS(iprf,MM) = clw
    	MSPPS_EM1_DS(iprf,MM) = em1
    	MSPPS_EM2_DS(iprf,MM) = em2
    	MSPPS_EM3_DS(iprf,MM) = em3
	MSPPS_TSKIN_DS(iprf,MM) = lst
        MSPPS_IWP_DS(iprf,MM)   = iwp
        MSPPS_RR_DS(iprf,MM)    =  rr
    	
	MIRS_SIC_DS(iprf,MM)  = Dep.SIC(iprf)
    	MIRS_SWE_DS(iprf,MM)  = Dep.SWE(iprf)
    	MIRS_CLW_DS(iprf,MM)  = Dep.CLW(iprf)
        MIRS_LWP_DS(iprf,MM)  = Dep.LWP(iprf)
	MIRS_SFC_DS(iprf,MM)  = Scene.SfcTypVec(iprf)
	MIRS_SFC2_DS(iprf,MM) = Dep.iTypSfc(iprf)
    	MIRS_EM1_DS(iprf,MM)  = e23
    	MIRS_EM2_DS(iprf,MM)  = e31
    	MIRS_EM3_DS(iprf,MM)  = e50
	MIRS_TSKIN_DS(iprf,MM)= tskin
        MIRS_IWP_DS(iprf,MM)  = DEP.GWP(iprf)
        MIRS_RR_DS(iprf,MM)   = DEP.RR(iprf)
	MIRS_QC_DS(iprf,MM)   = DEP.qc(0,iprf)

        if ( Dep.qc(0,iprf) eq 2) then begin
            MIRS_SIC_DS(iprf,MM)  = -99.
            MIRS_SWE_DS(iprf,MM)  = -99.
            MIRS_CLW_DS(iprf,MM)  = -99.
            MIRS_LWP_DS(iprf,MM)  = -99.
            MIRS_SFC_DS(iprf,MM)  = -99.
            MIRS_SFC2_DS(iprf,MM) = -99.
            MIRS_EM1_DS(iprf,MM)  = -99.
            MIRS_EM2_DS(iprf,MM)  = -99.
            MIRS_EM3_DS(iprf,MM)  = -99.
            MIRS_TSKIN_DS(iprf,MM)= -99.
            MIRS_IWP_DS(iprf,MM)  = -99.
            MIRS_RR_DS(iprf,MM)   = -99.
        endif

    endif
    
    
    ;---- grid stuff goese here -----
    loncorr=abs(1/cos(PI*Dep.lat(iprf)/180.0))
    if ( loncorr gt 200 ) then loncorr=200
    ifov = iprf MOD NUMSPOT_A

    lonleft  = Dep.lon(iprf) - 0.5 * fov_size_A(ifov) * loncorr
    lonright = Dep.lon(iprf) + 0.5 * fov_size_A(ifov) * loncorr
    gridlon_left  = FIX( (lonleft  + 180.0) * gridfactor + adjustment )
    gridlon_right = FIX( (lonright + 180.0) * gridfactor + adjustment )

    if ( gridlon_left	lt 0	 )  then gridlon_left=0
    if ( gridlon_left	ge NCOL  )  then gridlon_left=NCOL-1
    if ( gridlon_right  lt 0	 )  then gridlon_right=0
    if ( gridlon_right  ge NCOL  )  then gridlon_right=NCOL-1

    if ( abs(ifov-(NUMSPOT_A-1.)/2.) lt (LATLIM_A - 0.4 )) then begin
    	gridlat_bot = FIX( (Dep.lat(iprf)+90) * gridfactor )
    	gridlat_top = gridlat_bot + 1 
    endif else begin 
    	gridlat_bot = FIX( (Dep.lat(iprf)+90.0) * gridfactor - 1 + adjustment )
    	gridlat_top = FIX( (Dep.lat(iprf)+90.0) * gridfactor + 1 + adjustment )
    endelse

    if ( gridlat_bot lt 0 ) then gridlat_bot=0
    if ( gridlat_top lt 0 ) then gridlat_top=0

    if ( gridlat_top ge NROW ) then gridlat_top=NROW-1
    if ( gridlat_bot ge NROW ) then gridlat_bot=NROW-1

    near_nadir = abs(ifov - (NUMSPOT_A-1.)/2.) + 0.6

    for lonbox=gridlon_left, gridlon_right do begin
    for latbox=gridlat_bot,  gridlat_top   do begin
    
        ;---- ascending
    	if ( Dep.node(iprf) eq 0  and  near_nadir le nadir_as(lonbox,latbox) ) then begin
	
	    nadir_as(lonbox,latbox) = near_nadir
	    grid_angle_mirs_as(lonbox,latbox)=Dep.angle(iprf)
	    if ( sic ge 0 ) then grid_sic_mspps_as(lonbox,latbox)=sic
	    if ( swe ge 0 ) then grid_swe_mspps_as(lonbox,latbox)=swe
	    if ( clw ge 0 ) then grid_clw_mspps_as(lonbox,latbox)=clw
	    if ( em1 ge 0 ) then grid_em1_mspps_as(lonbox,latbox)=em1
	    if ( em2 ge 0 ) then grid_em2_mspps_as(lonbox,latbox)=em2
	    if ( em3 ge 0 ) then grid_em3_mspps_as(lonbox,latbox)=em3
	    if ( iwp ge 0 ) then grid_iwp_mspps_as(lonbox,latbox)=iwp
	    if ( rr  ge 0 ) then grid_rr_mspps_as(lonbox,latbox) =rr
	    if ( lst gt 0 ) then grid_tskin_mspps_as(lonbox,latbox) = lst
	
	    if( Dep.qc(0,iprf) ne 2 ) then begin
	        if ( Dep.sic(iprf) ge 0 ) then grid_sic_mirs_as(lonbox,latbox)=Dep.sic(iprf)
	        if ( Dep.swe(iprf) ge 0 ) then grid_swe_mirs_as(lonbox,latbox)=Dep.swe(iprf)
	        if ( Dep.clw(iprf) ge 0 ) then grid_clw_mirs_as(lonbox,latbox)=Dep.clw(iprf)
                if ( Dep.lwp(iprf) ge 0 ) then grid_lwp_mirs_as(lonbox,latbox)=Dep.lwp(iprf)
	        if ( Dep.gwp(iprf) ge 0 ) then grid_iwp_mirs_as(lonbox,latbox)=Dep.gwp(iprf)
	        if ( Dep.rr(iprf)  ge 0 ) then grid_rr_mirs_as(lonbox,latbox) =Dep.rr(iprf)
	        if ( e23 ge 0 	        ) then grid_em1_mirs_as(lonbox,latbox)=e23
	        if ( e31 ge 0 	        ) then grid_em2_mirs_as(lonbox,latbox)=e31
	        if ( e50 ge 0 	        ) then grid_em3_mirs_as(lonbox,latbox)=e50
		if ( tskin gt 0         ) then grid_tskin_mirs_as(lonbox,latbox)=tskin
		if ( sfc2 ge 0          ) then grid_sfc2_mirs_as(lonbox,latbox)=sfc2
	    endif
	    if( Dep.qc(0,iprf) eq 2 ) then begin
	        if ( Dep.sic(iprf) ge 0 ) then grid_sic_mirs_as(lonbox,latbox)=-99.0
	        if ( Dep.swe(iprf) ge 0 ) then grid_swe_mirs_as(lonbox,latbox)=-99.0
	        if ( Dep.clw(iprf) ge 0 ) then grid_clw_mirs_as(lonbox,latbox)=-99.0
                if ( Dep.lwp(iprf) ge 0 ) then grid_lwp_mirs_as(lonbox,latbox)=-99.0
	        if ( Dep.gwp(iprf) ge 0 ) then grid_iwp_mirs_as(lonbox,latbox)=-99.0
	        if ( Dep.rr(iprf)  ge 0 ) then grid_rr_mirs_as(lonbox,latbox) =-99.0
	        if ( e23 ge 0 	        ) then grid_em1_mirs_as(lonbox,latbox)=-99.0
	        if ( e31 ge 0 	        ) then grid_em2_mirs_as(lonbox,latbox)=-99.0
	        if ( e50 ge 0 	        ) then grid_em3_mirs_as(lonbox,latbox)=-99.0
		if ( tskin gt 0         ) then grid_tskin_mirs_as(lonbox,latbox)=-99.0
		if ( sfc2 ge 0          ) then grid_sfc2_mirs_as(lonbox,latbox)=-99.0
	    endif
	    
    	endif
     	
        ;---- descending
    	if ( Dep.node(iprf) eq 1  and  near_nadir le nadir_ds(lonbox,latbox) ) then begin
	
	    nadir_ds(lonbox,latbox) = near_nadir
	    grid_angle_mirs_ds(lonbox,latbox)=Dep.angle(iprf)
	    if ( sic ge 0 ) then grid_sic_mspps_ds(lonbox,latbox)=sic
	    if ( swe ge 0 ) then grid_swe_mspps_ds(lonbox,latbox)=swe
	    if ( clw ge 0 ) then grid_clw_mspps_ds(lonbox,latbox)=clw
	    if ( em1 ge 0 ) then grid_em1_mspps_ds(lonbox,latbox)=em1
	    if ( em2 ge 0 ) then grid_em2_mspps_ds(lonbox,latbox)=em2
	    if ( em3 ge 0 ) then grid_em3_mspps_ds(lonbox,latbox)=em3
	    if ( iwp ge 0 ) then grid_iwp_mspps_ds(lonbox,latbox)=iwp
	    if ( rr  ge 0 ) then grid_rr_mspps_ds(lonbox,latbox) =rr
	    if ( lst gt 0 ) then grid_tskin_mspps_ds(lonbox,latbox) = lst
	
	    if( Dep.qc(0,iprf) ne 2 ) then begin
	        if ( Dep.sic(iprf) ge 0 ) then grid_sic_mirs_ds(lonbox,latbox)=Dep.sic(iprf)
	        if ( Dep.swe(iprf) ge 0 ) then grid_swe_mirs_ds(lonbox,latbox)=Dep.swe(iprf)
	        if ( Dep.clw(iprf) ge 0 ) then grid_clw_mirs_ds(lonbox,latbox)=Dep.clw(iprf)
	        if ( Dep.lwp(iprf) ge 0 ) then grid_lwp_mirs_ds(lonbox,latbox)=Dep.lwp(iprf)
	        if ( Dep.gwp(iprf) ge 0 ) then grid_iwp_mirs_ds(lonbox,latbox)=Dep.gwp(iprf)
	        if ( Dep.rr(iprf)  ge 0 ) then grid_rr_mirs_ds(lonbox,latbox) =Dep.rr(iprf)
	        if ( e23 ge 0 	        ) then grid_em1_mirs_ds(lonbox,latbox)=e23
	        if ( e31 ge 0 	        ) then grid_em2_mirs_ds(lonbox,latbox)=e31
	        if ( e50 ge 0 	        ) then grid_em3_mirs_ds(lonbox,latbox)=e50
		if ( tskin gt 0         ) then grid_tskin_mirs_ds(lonbox,latbox)=tskin
		if ( sfc2 ge 0          ) then grid_sfc2_mirs_ds(lonbox,latbox)=sfc2
	    endif
	    if( Dep.qc(0,iprf) eq 2 ) then begin
	        if ( Dep.sic(iprf) ge 0 ) then grid_sic_mirs_ds(lonbox,latbox)=-99.0
	        if ( Dep.swe(iprf) ge 0 ) then grid_swe_mirs_ds(lonbox,latbox)=-99.0
	        if ( Dep.clw(iprf) ge 0 ) then grid_clw_mirs_ds(lonbox,latbox)=-99.0
	        if ( Dep.lwp(iprf) ge 0 ) then grid_lwp_mirs_ds(lonbox,latbox)=-99.0
	        if ( Dep.gwp(iprf) ge 0 ) then grid_iwp_mirs_ds(lonbox,latbox)=-99.0
	        if ( Dep.rr(iprf)  ge 0 ) then grid_rr_mirs_ds(lonbox,latbox) =-99.0
	        if ( e23 ge 0 	        ) then grid_em1_mirs_ds(lonbox,latbox)=-99.0
	        if ( e31 ge 0 	        ) then grid_em2_mirs_ds(lonbox,latbox)=-99.0
	        if ( e50 ge 0 	        ) then grid_em3_mirs_ds(lonbox,latbox)=-99.0
		if ( tskin gt 0         ) then grid_tskin_mirs_ds(lonbox,latbox)=-99.0
		if ( sfc2 ge 0          ) then grid_sfc2_mirs_ds(lonbox,latbox)=-99.0
	    endif
	    
    	endif
     	
    endfor 
    endfor 
    
    iprf=iprf+1

  endwhile 
  
  nextLoop:
  
  MM=MM+1
  
ENDWHILE
CLOSE, /all


;***************************************************************
;    read in sfc types from static land/sea marks
;***************************************************************
sfcMask=BytArr(nCol,nRow)
dum1=BytArr(nCol,nRow)
dum2=BytArr(nCol,nRow)
IF ( nCol EQ 1440 ) THEN OpenR,Lun,'lndsea25.tag',/Get_Lun, /Swap_If_Big_Endian
IF ( nCol EQ 1080 ) THEN OpenR,Lun,'lndsea30.tag',/Get_Lun, /Swap_If_Big_Endian
IF ( nCol EQ 720 )  THEN OpenR,Lun,'lndsea50.tag',/Get_Lun, /Swap_If_Big_Endian
ReadU,Lun,dum1
Free_Lun,Lun

;---- reverse index of land/sea mask for our map
for jrow=0, nrow-1 do begin  
  dum2[*,jrow] = dum1[*,nrow-1-jrow]
endfor

for icol=0, ncol-1 do begin
  if icol ge ncol/2 then begin
    sfcMask(icol,*) = dum2(icol-ncol/2,*)
  endif
  if icol lt ncol/2 then begin
    sfcMask(icol,*) = dum2(icol+ncol/2,*)
  endif
endfor


;***************************************************************
;  Plot gridded MSPPS CLW
;***************************************************************
IF ( PLOT_CLW_MSPPS EQ 1 ) THEN BEGIN

  minvalue=0.0
  maxvalue=0.70
  div=7
  format='(f5.2)'
  color_table_index = 41

  sfcPick=0
  map_name = figsDir + prefix_mspps + yyyymmdd +'_clw_sea_as.png'
  title = 'MSPPS ' + strupcase(satId) + ' EDR CLW (mm) ' + date +  ' Asc' + ' (V' + version +')'
  plot_grid, grid_clw_mspps_as, sfcMask, map_name, minvalue, maxvalue, latmin,latmax,lonmin,lonmax, title, sfcPick, div, format, color_table_index=color_table_index	

  map_name = figsDir + prefix_mspps + yyyymmdd +'_clw_sea_ds.png'
  title = 'MSPPS ' + strupcase(satId) + ' EDR CLW (mm) ' + date +  ' Des' + ' (V' + version +')'
  plot_grid, grid_clw_mspps_ds, sfcMask, map_name, minvalue, maxvalue, latmin,latmax,lonmin,lonmax, title, sfcPick, div, format, color_table_index=color_table_index	

ENDIF


;***************************************************************
;  Plot gridded MIRS CLW
;***************************************************************
IF ( PLOT_CLW_MIRS EQ 1 ) THEN BEGIN

  minvalue=0.0
  maxvalue=0.7
  div=7
  format='(f5.2)'

  sfcPick=2
  map_name = figsDir + prefix + yyyymmdd +'_clw_all_as.png'
  title = 'MIRS ' + strupcase(satId) + ' EDR CLW (mm) ' + date +  ' Asc' + ' (V' + version +')'
  plot_grid, grid_clw_mirs_as, sfcMask, map_name, minvalue, maxvalue, latmin,latmax,lonmin,lonmax, title, sfcPick, div, format   

  map_name = figsDir + prefix + yyyymmdd +'_clw_all_ds.png'
  title = 'MIRS ' + strupcase(satId) + ' EDR CLW (mm) ' + date +  ' Des' + ' (V' + version +')'
  plot_grid, grid_clw_mirs_ds, sfcMask, map_name, minvalue, maxvalue, latmin,latmax,lonmin,lonmax, title, sfcPick, div, format   

  sfcPick=1
  map_name = figsDir + prefix + yyyymmdd +'_clw_lnd_as.png'
  title = 'MIRS ' + strupcase(satId) + ' EDR CLW (mm) ' + date +  ' Asc' + ' (V' + version +')'
  plot_grid, grid_clw_mirs_as, sfcMask, map_name, minvalue, maxvalue, latmin,latmax,lonmin,lonmax, title, sfcPick, div, format   

  map_name = figsDir + prefix + yyyymmdd +'_clw_lnd_ds.png'
  title = 'MIRS ' + strupcase(satId) + ' EDR CLW (mm) ' + date +  ' Des' + ' (V' + version +')'
  plot_grid, grid_clw_mirs_ds, sfcMask, map_name, minvalue, maxvalue, latmin,latmax,lonmin,lonmax, title, sfcPick, div, format   

  sfcPick=0
  map_name = figsDir + prefix + yyyymmdd +'_clw_sea_as.png'
  title = 'MIRS ' + strupcase(satId) + ' EDR CLW (mm) ' + date +  ' Asc' + ' (V' + version +')'
  plot_grid, grid_clw_mirs_as, sfcMask, map_name, minvalue, maxvalue, latmin,latmax,lonmin,lonmax, title, sfcPick, div, format   

  map_name = figsDir + prefix + yyyymmdd +'_clw_sea_ds.png'
  title = 'MIRS ' + strupcase(satId) + ' EDR CLW (mm) ' + date +  ' Des' + ' (V' + version +')'
  plot_grid, grid_clw_mirs_ds, sfcMask, map_name, minvalue, maxvalue, latmin,latmax,lonmin,lonmax, title, sfcPick, div, format   

ENDIF


;***************************************************************
;    plot CLW difference
;***************************************************************
diff_grid_clw_as = fltarr(NCOL,NROW)
diff_grid_clw_ds = fltarr(NCOL,NROW)

diff_grid_clw_as(*,*) = -999.0
diff_grid_clw_ds(*,*) = -999.0

for j = 0, NROW-1 do begin
for i = 0, NCOL-1 do begin
    if ( grid_clw_mirs_as(i,j) ge 0 and grid_clw_mspps_as(i,j) ge 0 ) then $
    	diff_grid_clw_as(i,j) =  grid_clw_mirs_as(i,j) - grid_clw_mspps_as(i,j)
	
    if ( grid_clw_mirs_ds(i,j) ge 0 and grid_clw_mspps_ds(i,j) ge 0 ) then $
    	diff_grid_clw_ds(i,j) =  grid_clw_mirs_ds(i,j) - grid_clw_mspps_ds(i,j)
endfor
endfor

minvalue=-0.2
maxvalue=0.2
div=10
format='(f5.2)'

sfcPick=0
map_name = figsDir + prefix_diff + yyyymmdd +'_clw_sea_as.png'
title = 'MIRS ' + strupcase(satId) + ' - MSPPS CLW (mm) ' + date +  ' Asc' + ' (V' + version +')'
plot_grid, diff_grid_clw_as, sfcMask, map_name, minvalue, maxvalue, latmin,latmax,lonmin,lonmax, title, sfcPick, div, format   

map_name = figsDir + prefix_diff + yyyymmdd +'_clw_sea_ds.png'
title = 'MIRS ' + strupcase(satId) + ' - MSPPS CLW (mm) ' + date +  ' Des' + ' (V' + version +')'
plot_grid, diff_grid_clw_ds, sfcMask, map_name, minvalue, maxvalue, latmin,latmax,lonmin,lonmax, title, sfcPick, div, format   


;***************************************************************
;  Plot gridded MIRS LWP
;***************************************************************
IF ( PLOT_LWP_MIRS EQ 1 ) THEN BEGIN

  minvalue=0.0
  maxvalue=0.7
  div=7
  format='(f4.2)'

  sfcPick=2
  map_name = figsDir + prefix + yyyymmdd +'_lwp_all_as.png'
  title = 'MIRS ' + strupcase(satId) + ' EDR LWP (mm) ' + date +  ' Asc' + ' (V' + version +')'
  plot_grid, grid_clw_mirs_as, sfcMask, map_name, minvalue, maxvalue, latmin,latmax,lonmin,lonmax, title, sfcPick, div, format   

  map_name = figsDir + prefix + yyyymmdd +'_lwp_all_ds.png'
  title = 'MIRS ' + strupcase(satId) + ' EDR LWP (mm) ' + date +  ' Des' + ' (V' + version +')'
  plot_grid, grid_clw_mirs_ds, sfcMask, map_name, minvalue, maxvalue, latmin,latmax,lonmin,lonmax, title, sfcPick, div, format   

  sfcPick=1
  map_name = figsDir + prefix + yyyymmdd +'_lwp_lnd_as.png'
  title = 'MIRS ' + strupcase(satId) + ' EDR LWP (mm) ' + date +  ' Asc' + ' (V' + version +')'
  plot_grid, grid_clw_mirs_as, sfcMask, map_name, minvalue, maxvalue, latmin,latmax,lonmin,lonmax, title, sfcPick, div, format   

  map_name = figsDir + prefix + yyyymmdd +'_lwp_lnd_ds.png'
  title = 'MIRS ' + strupcase(satId) + ' EDR LWP (mm) ' + date +  ' Des' + ' (V' + version +')'
  plot_grid, grid_clw_mirs_ds, sfcMask, map_name, minvalue, maxvalue, latmin,latmax,lonmin,lonmax, title, sfcPick, div, format   

  sfcPick=0
  map_name = figsDir + prefix + yyyymmdd +'_lwp_sea_as.png'
  title = 'MIRS ' + strupcase(satId) + ' EDR LWP (mm) ' + date +  ' Asc' + ' (V' + version +')'
  plot_grid, grid_clw_mirs_as, sfcMask, map_name, minvalue, maxvalue, latmin,latmax,lonmin,lonmax, title, sfcPick, div, format   

  map_name = figsDir + prefix + yyyymmdd +'_lwp_sea_ds.png'
  title = 'MIRS ' + strupcase(satId) + ' EDR LWP (mm) ' + date +  ' Des' + ' (V' + version +')'
  plot_grid, grid_clw_mirs_ds, sfcMask, map_name, minvalue, maxvalue, latmin,latmax,lonmin,lonmax, title, sfcPick, div, format   

ENDIF


;***************************************************************
;    plot LWP difference from MSPPS CLW
;***************************************************************
diff_grid_lwp_as = fltarr(NCOL,NROW)
diff_grid_lwp_ds = fltarr(NCOL,NROW)

diff_grid_lwp_as(*,*) = -999.0
diff_grid_lwp_ds(*,*) = -999.0

for j = 0, NROW-1 do begin
for i = 0, NCOL-1 do begin
    if ( grid_lwp_mirs_as(i,j) ge 0 and grid_clw_mspps_as(i,j) ge 0 ) then $
    	diff_grid_lwp_as(i,j) =  grid_lwp_mirs_as(i,j) - grid_clw_mspps_as(i,j)
	
    if ( grid_lwp_mirs_ds(i,j) ge 0 and grid_clw_mspps_ds(i,j) ge 0 ) then $
    	diff_grid_lwp_ds(i,j) =  grid_lwp_mirs_ds(i,j) - grid_clw_mspps_ds(i,j)
endfor
endfor

minvalue=-0.2
maxvalue=0.2
div=10
format='(f5.2)'

sfcPick=0
map_name = figsDir + prefix_diff + yyyymmdd +'_lwp_sea_as.png'
title = 'MIRS LWP ' + strupcase(satId) + ' - MSPPS CLW (mm) ' + date +  ' Asc' + ' (V' + version +')'
plot_grid, diff_grid_lwp_as, sfcMask, map_name, minvalue, maxvalue, latmin,latmax,lonmin,lonmax, title, sfcPick, div, format   

map_name = figsDir + prefix_diff + yyyymmdd +'_lwp_sea_ds.png'
title = 'MIRS LWP ' + strupcase(satId) + ' - MSPPS CLW (mm) ' + date +  ' Des' + ' (V' + version +')'
plot_grid, diff_grid_lwp_ds, sfcMask, map_name, minvalue, maxvalue, latmin,latmax,lonmin,lonmax, title, sfcPick, div, format   


;***************************************************************
;    plot MIRS swe 
;***************************************************************
IF ( PLOT_SWE_MIRS EQ 1 ) THEN BEGIN

  minvalue=0
  maxvalue=8
  div=8
  format='(I2)'

  sfcPick=1
  map_name = figsDir + prefix + yyyymmdd +'_swe_lnd_as.png'
  title = 'MIRS ' + strupcase(satId) + ' EDR Snow Water Equivalent (cm) ' + date +  ' Asc' + ' (V' + version +')'
  plot_grid, grid_swe_mirs_as, sfcMask, map_name, minvalue, maxvalue, latmin,latmax,lonmin,lonmax, title, sfcPick, div, format   

  map_name = figsDir + prefix + yyyymmdd +'_swe_lnd_ds.png'
  title = 'MIRS ' + strupcase(satId) + ' EDR Snow Water Equivalent (cm) ' + date +  ' Des' + ' (V' + version +')'
  plot_grid, grid_swe_mirs_ds, sfcMask, map_name, minvalue, maxvalue, latmin,latmax,lonmin,lonmax, title, sfcPick, div, format   

ENDIF

;***************************************************************
;    plot MSPPS swe 
;***************************************************************
minvalue=0
maxvalue=8
div=8
format='(I2)'

sfcPick=1
map_name = figsDir + prefix_mspps + yyyymmdd +'_swe_lnd_as.png'
title = 'MSPPS ' + strupcase(satId) + ' EDR Snow Water Equivalent (cm) ' + date +  ' Asc' + ' (V' + version +')'
plot_grid, grid_swe_mspps_as, sfcMask, map_name, minvalue, maxvalue, latmin,latmax,lonmin,lonmax, title, sfcPick, div, format   

map_name = figsDir + prefix_mspps + yyyymmdd +'_swe_lnd_ds.png'
title = 'MSPPS ' + strupcase(satId) + ' EDR Snow Water Equivalent (cm) ' + date +  ' Des' + ' (V' + version +')'
plot_grid, grid_swe_mspps_ds, sfcMask, map_name, minvalue, maxvalue, latmin,latmax,lonmin,lonmax, title, sfcPick, div, format   



;***************************************************************
;    plot swe difference 
;***************************************************************
diff_grid_swe_as = fltarr(NCOL,NROW)
diff_grid_swe_ds = fltarr(NCOL,NROW)

diff_grid_swe_as(*,*) = -999.0
diff_grid_swe_ds(*,*) = -999.0

for j = 0, NROW-1 do begin
for i = 0, NCOL-1 do begin
    if ( grid_swe_mirs_as(i,j) ge 0 and grid_swe_mspps_as(i,j) ge 0 ) then $
    	diff_grid_swe_as(i,j) =  grid_swe_mirs_as(i,j) - grid_swe_mspps_as(i,j)
	
    if ( grid_swe_mirs_ds(i,j) ge 0 and grid_swe_mspps_ds(i,j) ge 0 ) then $
    	diff_grid_swe_ds(i,j) =  grid_swe_mirs_ds(i,j) - grid_swe_mspps_ds(i,j)
endfor
endfor

minvalue=-4
maxvalue=4
div=8
format='(I2)'

sfcPick=1
map_name = figsDir + prefix_diff + yyyymmdd +'_swe_lnd_as.png'
title = 'MIRS ' + strupcase(satId) + ' - MSPPS SWE (cm) ' + date +  ' Asc' + ' (V' + version +')'
plot_grid, diff_grid_swe_as, sfcMask, map_name, minvalue, maxvalue, latmin,latmax,lonmin,lonmax, title, sfcPick, div, format   

map_name = figsDir + prefix_diff + yyyymmdd +'_swe_lnd_ds.png'
title = 'MIRS ' + strupcase(satId) + ' - MSPPS SWE (cm) ' + date +  ' Des' + ' (V' + version +')'
plot_grid, diff_grid_swe_ds, sfcMask, map_name, minvalue, maxvalue, latmin,latmax,lonmin,lonmax, title, sfcPick, div, format   



;***************************************************************
;    plot MIRS sic
;***************************************************************
IF ( PLOT_SIC_MIRS EQ 1 ) THEN BEGIN

  minvalue=0
  maxvalue=100
  div=10
  format='(I3)'

  sfcPick=0
  map_name = figsDir + prefix + yyyymmdd +'_sice_sea_as.png'
  title = 'MIRS ' + strupcase(satId) + ' EDR Sea Ice Concentration (%) ' + date +  ' Asc' + ' (V' + version +')'
  plot_grid, grid_sic_mirs_as, sfcMask, map_name, minvalue, maxvalue, latmin,latmax,lonmin,lonmax, title, sfcPick, div, format   

  map_name = figsDir + prefix + yyyymmdd +'_sice_sea_ds.png'
  title = 'MIRS ' + strupcase(satId) + ' EDR Sea Ice Concentration (%) ' + date +  ' Des' + ' (V' + version +')'
  plot_grid, grid_sic_mirs_ds, sfcMask, map_name, minvalue, maxvalue, latmin,latmax,lonmin,lonmax, title, sfcPick, div, format   

ENDIF

;***************************************************************
;    plot MSPPS sic
;***************************************************************
minvalue=0
maxvalue=100
div=10
format='(I3)'

sfcPick=0
map_name = figsDir + prefix_mspps + yyyymmdd +'_sice_sea_as.png'
title = 'MSPPS ' + strupcase(satId) + ' EDR Sea Ice Concentration (%) ' + date +  ' Asc' + ' (V' + version +')'
plot_grid, grid_sic_mspps_as, sfcMask, map_name, minvalue, maxvalue, latmin,latmax,lonmin,lonmax, title, sfcPick, div, format   

map_name = figsDir + prefix_mspps + yyyymmdd +'_sice_sea_ds.png'
title = 'MSPPS ' + strupcase(satId) + ' EDR Sea Ice Concentration (%) ' + date +  ' Des' + ' (V' + version +')'
plot_grid, grid_sic_mspps_ds, sfcMask, map_name, minvalue, maxvalue, latmin,latmax,lonmin,lonmax, title, sfcPick, div, format   



;***************************************************************
;    plot sic difference 
;***************************************************************
diff_grid_sic_as = fltarr(NCOL,NROW)
diff_grid_sic_ds = fltarr(NCOL,NROW)

diff_grid_sic_as(*,*) = -999.0
diff_grid_sic_ds(*,*) = -999.0

for j = 0, NROW-1 do begin
for i = 0, NCOL-1 do begin
    if ( grid_sic_mirs_as(i,j) ge 0 and grid_sic_mspps_as(i,j) ge 0 ) then $
    	diff_grid_sic_as(i,j) =  grid_sic_mirs_as(i,j) - grid_sic_mspps_as(i,j)
	
    if ( grid_sic_mirs_ds(i,j) ge 0 and grid_sic_mspps_ds(i,j) ge 0 ) then $
    	diff_grid_sic_ds(i,j) =  grid_sic_mirs_ds(i,j) - grid_sic_mspps_ds(i,j)
endfor
endfor

minvalue=-10
maxvalue=10
div=10
format='(I3)'

sfcPick=0
map_name = figsDir + prefix_diff + yyyymmdd +'_sice_sea_as.png'
title = 'MIRS ' + strupcase(satId) + ' - MSPPS Sea Ice Concentration (%) ' + date +  ' Asc' + ' (V' + version +')'
plot_grid, diff_grid_sic_as, sfcMask, map_name, minvalue, maxvalue, latmin,latmax,lonmin,lonmax, title, sfcPick, div, format   

map_name = figsDir + prefix_diff + yyyymmdd +'_sice_sea_ds.png'
title = 'MIRS ' + strupcase(satId) + ' - MSPPS Sea Ice Concentration (%) ' + date +  ' Des' + ' (V' + version +')'
plot_grid, diff_grid_sic_ds, sfcMask, map_name, minvalue, maxvalue, latmin,latmax,lonmin,lonmax, title, sfcPick, div, format   



;***************************************************************
;  Plot gridded MIRS EM1(23V)
;***************************************************************
IF ( PLOT_EM1_MIRS EQ 1 ) THEN BEGIN

  minvalue=0.65
  maxvalue=1.0
  div=7
  format='(f4.2)'

  sfcPick=2
  map_name = figsDir + prefix + yyyymmdd +'_em_23v_all_as.png'
  title = 'MIRS ' + strupcase(satId) + ' EDR Emissivity at 23V ' + date +  ' Asc' + ' (V' + version +')'
  plot_grid, grid_em1_mirs_as, sfcMask, map_name, minvalue, maxvalue, latmin,latmax,lonmin,lonmax, title, sfcPick, div, format   

  map_name = figsDir + prefix + yyyymmdd +'_em_23v_all_ds.png'
  title = 'MIRS ' + strupcase(satId) + ' EDR Emissivity at 23V ' + date +  ' Des' + ' (V' + version +')'
  plot_grid, grid_em1_mirs_ds, sfcMask, map_name, minvalue, maxvalue, latmin,latmax,lonmin,lonmax, title, sfcPick, div, format   

  sfcPick=1
  map_name = figsDir + prefix + yyyymmdd +'_em_23v_lnd_as.png'
  title = 'MIRS ' + strupcase(satId) + ' EDR Emissivity at 23V ' + date +  ' Asc' + ' (V' + version +')'
  plot_grid, grid_em1_mirs_as, sfcMask, map_name, minvalue, maxvalue, latmin,latmax,lonmin,lonmax, title, sfcPick, div, format   

  map_name = figsDir + prefix + yyyymmdd +'_em_23v_lnd_ds.png'
  title = 'MIRS ' + strupcase(satId) + ' EDR Emissivity at 23V ' + date +  ' Des' + ' (V' + version +')'
  plot_grid, grid_em1_mirs_ds, sfcMask, map_name, minvalue, maxvalue, latmin,latmax,lonmin,lonmax, title, sfcPick, div, format   

  sfcPick=0
  map_name = figsDir + prefix + yyyymmdd +'_em_23v_sea_as.png'
  title = 'MIRS ' + strupcase(satId) + ' EDR Emissivity at 23V ' + date +  ' Asc' + ' (V' + version +')'
  plot_grid, grid_em1_mirs_as, sfcMask, map_name, minvalue, maxvalue, latmin,latmax,lonmin,lonmax, title, sfcPick, div, format   

  map_name = figsDir + prefix + yyyymmdd +'_em_23v_sea_ds.png'
  title = 'MIRS ' + strupcase(satId) + ' EDR Emissivity at 23V ' + date +  ' Des' + ' (V' + version +')'
  plot_grid, grid_em1_mirs_ds, sfcMask, map_name, minvalue, maxvalue, latmin,latmax,lonmin,lonmax, title, sfcPick, div, format   

ENDIF

;***************************************************************
;  Plot gridded MSPPS EM1(23V)
;***************************************************************
minvalue=0.65
maxvalue=1.0
div=7
format='(f4.2)'

sfcPick=1
map_name = figsDir + prefix_mspps + yyyymmdd +'_em_23v_lnd_as.png'
title = 'MSPPS ' + strupcase(satId) + ' EDR Emissivity at 23V ' + date +  ' Asc' + ' (V' + version +')'
plot_grid, grid_em1_mspps_as, sfcMask, map_name, minvalue, maxvalue, latmin,latmax,lonmin,lonmax, title, sfcPick, div, format  

map_name = figsDir + prefix_mspps + yyyymmdd +'_em_23v_lnd_ds.png'
title = 'MSPPS ' + strupcase(satId) + ' EDR Emissivity at 23V ' + date +  ' Des' + ' (V' + version +')'
plot_grid, grid_em1_mspps_ds, sfcMask, map_name, minvalue, maxvalue, latmin,latmax,lonmin,lonmax, title, sfcPick, div, format  


;***************************************************************
;    plot EM1(23V) difference
;***************************************************************
diff_grid_em1_as = fltarr(NCOL,NROW)
diff_grid_em1_ds = fltarr(NCOL,NROW)

diff_grid_em1_as(*,*) = -999.0
diff_grid_em1_ds(*,*) = -999.0

for j = 0, NROW-1 do begin
for i = 0, NCOL-1 do begin
    if ( grid_em1_mirs_as(i,j) ge 0 and grid_em1_mspps_as(i,j) ge 0 ) then $
    	diff_grid_em1_as(i,j) =  grid_em1_mirs_as(i,j) - grid_em1_mspps_as(i,j)

    if ( grid_em1_mirs_ds(i,j) ge 0 and grid_em1_mspps_ds(i,j) ge 0 ) then $
    	diff_grid_em1_ds(i,j) =  grid_em1_mirs_ds(i,j) - grid_em1_mspps_ds(i,j)
endfor
endfor

minvalue=-0.2
maxvalue=0.2
div=10
format='(f5.2)'

sfcPick=1
map_name = figsDir + prefix_diff + yyyymmdd +'_em_23v_lnd_as.png'
title = 'MIRS ' + strupcase(satId) + ' - MSPPS Emissivity at 23V  ' + date +  ' Asc' + ' (V' + version +')'
plot_grid, diff_grid_em1_as, sfcMask, map_name, minvalue, maxvalue, latmin,latmax,lonmin,lonmax, title, sfcPick, div, format   

map_name = figsDir + prefix_diff + yyyymmdd +'_em_23v_lnd_ds.png'
title = 'MIRS ' + strupcase(satId) + ' - MSPPS Emissivity at 23V  ' + date +  ' Des' + ' (V' + version +')'
plot_grid, diff_grid_em1_ds, sfcMask, map_name, minvalue, maxvalue, latmin,latmax,lonmin,lonmax, title, sfcPick, div, format   


;***************************************************************
;  Plot gridded MIRS EM2(31V)
;***************************************************************
IF ( PLOT_EM2_MIRS EQ 1 ) THEN BEGIN

  minvalue=0.65
  maxvalue=1.0
  div=7
  format='(f4.2)'

  sfcPick=2
  map_name = figsDir + prefix + yyyymmdd +'_em_31v_all_as.png'
  title = 'MIRS ' + strupcase(satId) + ' EDR Emissivity at 31V ' + date +  ' Asc' + ' (V' + version +')'
  plot_grid, grid_em2_mirs_as, sfcMask, map_name, minvalue, maxvalue, latmin,latmax,lonmin,lonmax, title, sfcPick, div, format   

  map_name = figsDir + prefix + yyyymmdd +'_em_31v_all_ds.png'
  title = 'MIRS ' + strupcase(satId) + ' EDR Emissivity at 31V ' + date +  ' Des' + ' (V' + version +')'
  plot_grid, grid_em2_mirs_ds, sfcMask, map_name, minvalue, maxvalue, latmin,latmax,lonmin,lonmax, title, sfcPick, div, format   

  sfcPick=1
  map_name = figsDir + prefix + yyyymmdd +'_em_31v_lnd_as.png'
  title = 'MIRS ' + strupcase(satId) + ' EDR Emissivity at 31V ' + date +  ' Asc' + ' (V' + version +')'
  plot_grid, grid_em2_mirs_as, sfcMask, map_name, minvalue, maxvalue, latmin,latmax,lonmin,lonmax, title, sfcPick, div, format   

  map_name = figsDir + prefix + yyyymmdd +'_em_31v_lnd_ds.png'
  title = 'MIRS ' + strupcase(satId) + ' EDR Emissivity at 31V ' + date +  ' Des' + ' (V' + version +')'
  plot_grid, grid_em2_mirs_ds, sfcMask, map_name, minvalue, maxvalue, latmin,latmax,lonmin,lonmax, title, sfcPick, div, format   

  sfcPick=0
  map_name = figsDir + prefix + yyyymmdd +'_em_31v_sea_as.png'
  title = 'MIRS ' + strupcase(satId) + ' EDR Emissivity at 31V ' + date +  ' Asc' + ' (V' + version +')'
  plot_grid, grid_em2_mirs_as, sfcMask, map_name, minvalue, maxvalue, latmin,latmax,lonmin,lonmax, title, sfcPick, div, format   

  map_name = figsDir + prefix + yyyymmdd +'_em_31v_sea_ds.png'
  title = 'MIRS ' + strupcase(satId) + ' EDR Emissivity at 31V ' + date +  ' Des' + ' (V' + version +')'
  plot_grid, grid_em2_mirs_ds, sfcMask, map_name, minvalue, maxvalue, latmin,latmax,lonmin,lonmax, title, sfcPick, div, format   

ENDIF


;***************************************************************
;  Plot gridded MSPPS EM2(31V)
;***************************************************************
minvalue=0.65
maxvalue=1.0
div=7
format='(f4.2)'

sfcPick=1
map_name = figsDir + prefix_mspps + yyyymmdd +'_em_31v_lnd_as.png'
title = 'MSPPS ' + strupcase(satId) + ' EDR Emissivity at 31V ' + date +  ' Asc' + ' (V' + version +')'
plot_grid, grid_em2_mspps_as, sfcMask, map_name, minvalue, maxvalue, latmin,latmax,lonmin,lonmax, title, sfcPick, div, format  

map_name = figsDir + prefix_mspps + yyyymmdd +'_em_31v_lnd_ds.png'
title = 'MSPPS ' + strupcase(satId) + ' EDR Emissivity at 31V ' + date +  ' Des' + ' (V' + version +')'
plot_grid, grid_em2_mspps_ds, sfcMask, map_name, minvalue, maxvalue, latmin,latmax,lonmin,lonmax, title, sfcPick, div, format  


;***************************************************************
;    plot EM2(31V) difference
;***************************************************************
diff_grid_em2_as = fltarr(NCOL,NROW)
diff_grid_em2_ds = fltarr(NCOL,NROW)

diff_grid_em2_as(*,*) = -999.0
diff_grid_em2_ds(*,*) = -999.0

for j = 0, NROW-1 do begin
for i = 0, NCOL-1 do begin
    if ( grid_em2_mirs_as(i,j) ge 0 and grid_em2_mspps_as(i,j) ge 0 ) then $
    	diff_grid_em2_as(i,j) =  grid_em2_mirs_as(i,j) - grid_em2_mspps_as(i,j)

    if ( grid_em2_mirs_ds(i,j) ge 0 and grid_em2_mspps_ds(i,j) ge 0 ) then $
    	diff_grid_em2_ds(i,j) =  grid_em2_mirs_ds(i,j) - grid_em2_mspps_ds(i,j)
endfor
endfor

minvalue=-0.2
maxvalue=0.2
div=10
format='(f5.2)'

sfcPick=1
map_name = figsDir + prefix_diff + yyyymmdd +'_em_31v_lnd_as.png'
title = 'MIRS ' + strupcase(satId) + ' - MSPPS Emissivity at 31V ' + date +  ' Asc' + ' (V' + version +')'
plot_grid, diff_grid_em2_as, sfcMask, map_name, minvalue, maxvalue, latmin,latmax,lonmin,lonmax, title, sfcPick, div, format   

map_name = figsDir + prefix_diff + yyyymmdd +'_em_31v_lnd_ds.png'
title = 'MIRS ' + strupcase(satId) + ' - MSPPS Emissivity at 31V ' + date +  ' Des' + ' (V' + version +')'
plot_grid, diff_grid_em2_ds, sfcMask, map_name, minvalue, maxvalue, latmin,latmax,lonmin,lonmax, title, sfcPick, div, format   


;***************************************************************
;  Plot gridded MIRS EM3(50V)
;***************************************************************
IF ( PLOT_EM3_MIRS EQ 1 ) THEN BEGIN

  minvalue=0.65
  maxvalue=1.0
  div=7
  format='(f4.2)'

  sfcPick=2
  map_name = figsDir + prefix + yyyymmdd +'_em_50v_all_as.png'
  title = 'MIRS ' + strupcase(satId) + ' EDR Emissivity at 50V ' + date +  ' Asc' + ' (V' + version +')'
  plot_grid, grid_em3_mirs_as, sfcMask, map_name, minvalue, maxvalue, latmin,latmax,lonmin,lonmax, title, sfcPick, div, format   

  map_name = figsDir + prefix + yyyymmdd +'_em_50v_all_ds.png'
  title = 'MIRS ' + strupcase(satId) + ' EDR Emissivity at 50V ' + date +  ' Des' + ' (V' + version +')'
  plot_grid, grid_em3_mirs_ds, sfcMask, map_name, minvalue, maxvalue, latmin,latmax,lonmin,lonmax, title, sfcPick, div, format   

  sfcPick=1
  map_name = figsDir + prefix + yyyymmdd +'_em_50v_lnd_as.png'
  title = 'MIRS ' + strupcase(satId) + ' EDR Emissivity at 50V ' + date +  ' Asc' + ' (V' + version +')'
  plot_grid, grid_em3_mirs_as, sfcMask, map_name, minvalue, maxvalue, latmin,latmax,lonmin,lonmax, title, sfcPick, div, format   

  map_name = figsDir + prefix + yyyymmdd +'_em_50v_lnd_ds.png'
  title = 'MIRS ' + strupcase(satId) + ' EDR Emissivity at 50V ' + date +  ' Des' + ' (V' + version +')'
  plot_grid, grid_em3_mirs_ds, sfcMask, map_name, minvalue, maxvalue, latmin,latmax,lonmin,lonmax, title, sfcPick, div, format   

  sfcPick=0
  map_name = figsDir + prefix + yyyymmdd +'_em_50v_sea_as.png'
  title = 'MIRS ' + strupcase(satId) + ' EDR Emissivity at 50V ' + date +  ' Asc' + ' (V' + version +')'
  plot_grid, grid_em3_mirs_as, sfcMask, map_name, minvalue, maxvalue, latmin,latmax,lonmin,lonmax, title, sfcPick, div, format   

  map_name = figsDir + prefix + yyyymmdd +'_em_50v_sea_ds.png'
  title = 'MIRS ' + strupcase(satId) + ' EDR Emissivity at 50V ' + date +  ' Des' + ' (V' + version +')'
  plot_grid, grid_em3_mirs_ds, sfcMask, map_name, minvalue, maxvalue, latmin,latmax,lonmin,lonmax, title, sfcPick, div, format   

ENDIF

;***************************************************************
;  Plot gridded MSPPS EM3(50V)
;***************************************************************
minvalue=0.65
maxvalue=1.0
div=7
format='(f4.2)'

sfcPick=1
map_name = figsDir + prefix_mspps + yyyymmdd +'_em_50v_lnd_as.png'
title = 'MSPPS ' + strupcase(satId) + ' EDR Emissivity at 50V ' + date +  ' Asc' + ' (V' + version +')'
plot_grid, grid_em3_mspps_as, sfcMask, map_name, minvalue, maxvalue, latmin,latmax,lonmin,lonmax, title, sfcPick, div, format  

map_name = figsDir + prefix_mspps + yyyymmdd +'_em_50v_lnd_ds.png'
title = 'MSPPS ' + strupcase(satId) + ' EDR Emissivity at 50V ' + date +  ' Des' + ' (V' + version +')'
plot_grid, grid_em3_mspps_ds, sfcMask, map_name, minvalue, maxvalue, latmin,latmax,lonmin,lonmax, title, sfcPick, div, format  


;***************************************************************
;    plot EM3(50V) difference
;***************************************************************
 diff_grid_em3_as = fltarr(NCOL,NROW)
diff_grid_em3_ds = fltarr(NCOL,NROW)

diff_grid_em3_as(*,*) = -999.0
diff_grid_em3_ds(*,*) = -999.0

for j = 0, NROW-1 do begin
for i = 0, NCOL-1 do begin
    if ( grid_em3_mirs_as(i,j) ge 0 and grid_em3_mspps_as(i,j) ge 0 ) then $
    	diff_grid_em3_as(i,j) =  grid_em3_mirs_as(i,j) - grid_em3_mspps_as(i,j)

    if ( grid_em3_mirs_ds(i,j) ge 0 and grid_em3_mspps_ds(i,j) ge 0 ) then $
    	diff_grid_em3_ds(i,j) =  grid_em3_mirs_ds(i,j) - grid_em3_mspps_ds(i,j)
endfor
endfor

minvalue=-0.2
maxvalue=0.2
div=10
format='(f5.2)'

sfcPick=1
map_name = figsDir + prefix_diff + yyyymmdd +'_em_50v_lnd_as.png'
title = 'MIRS ' + strupcase(satId) + ' - MSPPS Emissivity at 50V ' + date +  ' Asc' + ' (V' + version +')'
plot_grid, diff_grid_em3_as, sfcMask, map_name, minvalue, maxvalue, latmin,latmax,lonmin,lonmax, title, sfcPick, div, format   

map_name = figsDir + prefix_diff + yyyymmdd +'_em_50v_lnd_ds.png'
title = 'MIRS ' + strupcase(satId) + ' - MSPPS Emissivity at 50V ' + date +  ' Des' + ' (V' + version +')'
plot_grid, diff_grid_em3_ds, sfcMask, map_name, minvalue, maxvalue, latmin,latmax,lonmin,lonmax, title, sfcPick, div, format   


;***************************************************************
;  Plot gridded MSPPS IWP
;***************************************************************
minvalue=0.00
maxvalue=0.20
div=4
format='(f4.2)'

color_table_index = 41

sfcPick=2
map_name = figsDir + prefix_mspps + yyyymmdd +'_iwp_all_as.png'
title = 'MSPPS ' + strupcase(satId) + ' EDR Ice Water Path ' + date +  ' Asc' + ' (V' + version +')'
plot_grid, grid_iwp_mspps_as, sfcMask, map_name, minvalue, maxvalue, latmin,latmax,lonmin,lonmax, title, sfcPick, div, format, color_table_index=color_table_index

map_name = figsDir + prefix_mspps + yyyymmdd +'_iwp_all_ds.png'
title = 'MSPPS ' + strupcase(satId) + ' EDR Ice Water Path ' + date +  ' Des' + ' (V' + version +')'
plot_grid, grid_iwp_mspps_ds, sfcMask, map_name, minvalue, maxvalue, latmin,latmax,lonmin,lonmax, title, sfcPick, div, format, color_table_index=color_table_index

sfcPick=1
map_name = figsDir + prefix_mspps + yyyymmdd +'_iwp_lnd_as.png'
title = 'MSPPS ' + strupcase(satId) + ' EDR Ice Water Path ' + date +  ' Asc' + ' (V' + version +')'
plot_grid, grid_iwp_mspps_as, sfcMask, map_name, minvalue, maxvalue, latmin,latmax,lonmin,lonmax, title, sfcPick, div, format, color_table_index=color_table_index

map_name = figsDir + prefix_mspps + yyyymmdd +'_iwp_lnd_ds.png'
title = 'MSPPS ' + strupcase(satId) + ' EDR Ice Water Path ' + date +  ' Des' + ' (V' + version +')'
plot_grid, grid_iwp_mspps_ds, sfcMask, map_name, minvalue, maxvalue, latmin,latmax,lonmin,lonmax, title, sfcPick, div, format, color_table_index=color_table_index

sfcPick=0
map_name = figsDir + prefix_mspps + yyyymmdd +'_iwp_sea_as.png'
title = 'MSPPS ' + strupcase(satId) + ' EDR Ice Water Path ' + date +  ' Asc' + ' (V' + version +')'
plot_grid, grid_iwp_mspps_as, sfcMask, map_name, minvalue, maxvalue, latmin,latmax,lonmin,lonmax, title, sfcPick, div, format, color_table_index=color_table_index

map_name = figsDir + prefix_mspps + yyyymmdd +'_iwp_sea_ds.png'
title = 'MSPPS ' + strupcase(satId) + ' EDR Ice Water Path ' + date +  ' Des' + ' (V' + version +')'
plot_grid, grid_iwp_mspps_ds, sfcMask, map_name, minvalue, maxvalue, latmin,latmax,lonmin,lonmax, title, sfcPick, div, format, color_table_index=color_table_index

;***************************************************************
;    plot IWP  difference
;***************************************************************
diff_grid_iwp_as = fltarr(NCOL,NROW)
diff_grid_iwp_ds = fltarr(NCOL,NROW)

diff_grid_iwp_as(*,*) = -999.0
diff_grid_iwp_ds(*,*) = -999.0

for j = 0, NROW-1 do begin
for i = 0, NCOL-1 do begin
    if ( grid_iwp_mirs_as(i,j) ge 0 and grid_iwp_mspps_as(i,j) ge 0 ) then $
        diff_grid_iwp_as(i,j) =  grid_iwp_mirs_as(i,j) - grid_iwp_mspps_as(i,j)

    if ( grid_iwp_mirs_ds(i,j) ge 0 and grid_iwp_mspps_ds(i,j) ge 0 ) then $
        diff_grid_iwp_ds(i,j) =  grid_iwp_mirs_ds(i,j) - grid_iwp_mspps_ds(i,j)
endfor
endfor

minvalue=-0.2
maxvalue=0.2
div=10
format='(f5.2)'

sfcPick=2
map_name = figsDir + prefix_diff + yyyymmdd +'_iwp_all_as.png'
title = 'MIRS GWP ' + strupcase(satId) + ' - MSPPS IWP ' + date +  ' Asc' + ' (V' + version +')'
plot_grid, diff_grid_iwp_as, sfcMask, map_name, minvalue, maxvalue, latmin,latmax,lonmin,lonmax, title, sfcPick, div, format

map_name = figsDir + prefix_diff + yyyymmdd +'_iwp_all_ds.png'
title = 'MIRS GWP ' + strupcase(satId) + ' - MSPPS IWP ' + date +  ' Des' + ' (V' + version +')'
plot_grid, diff_grid_iwp_ds, sfcMask, map_name, minvalue, maxvalue, latmin,latmax,lonmin,lonmax,title, sfcPick, div, format



sfcPick=1
map_name = figsDir + prefix_diff + yyyymmdd +'_iwp_lnd_as.png'
title = 'MIRS GWP ' + strupcase(satId) + ' - MSPPS IWP ' + date +  ' Asc' + ' (V' + version +')'
plot_grid, diff_grid_iwp_as, sfcMask, map_name, minvalue, maxvalue, latmin,latmax,lonmin,lonmax, title, sfcPick, div, format

map_name = figsDir + prefix_diff + yyyymmdd +'_iwp_lnd_ds.png'
title = 'MIRS GWP ' + strupcase(satId) + ' - MSPPS IWP ' + date +  ' Des' + ' (V' + version +')'
plot_grid, diff_grid_iwp_ds, sfcMask, map_name, minvalue, maxvalue, latmin,latmax,lonmin,lonmax,title, sfcPick, div, format


sfcPick=0
map_name = figsDir + prefix_diff + yyyymmdd +'_iwp_sea_as.png'
title = 'MIRS GWP ' + strupcase(satId) + ' - MSPPS IWP ' + date +  ' Asc' + ' (V' + version +')'
plot_grid, diff_grid_iwp_as, sfcMask, map_name, minvalue, maxvalue, latmin,latmax,lonmin,lonmax, title, sfcPick, div, format

map_name = figsDir + prefix_diff + yyyymmdd +'_iwp_sea_ds.png'
title = 'MIRS GWP ' + strupcase(satId) + ' - MSPPS IWP ' + date +  ' Des' + ' (V' + version +')'
plot_grid, diff_grid_iwp_ds, sfcMask, map_name, minvalue, maxvalue, latmin,latmax,lonmin,lonmax,title, sfcPick, div, format


IF ( PLOT_RR_MIRS EQ 1 ) THEN BEGIN

  minvalue=0.0
  maxvalue=10.0
  div=10
  format='(f4.1)'

  sfcPick=2
  map_name = figsDir + prefix + yyyymmdd +'_rr_all_as.png'
  title = 'MIRS ' + strupcase(satId) + ' EDR Rain Rate ' + date +  ' Asc' + ' (V' + version +')'
  plot_grid, grid_rr_mirs_as, sfcMask, map_name, minvalue, maxvalue, latmin,latmax,lonmin,lonmax, title, sfcPick, div, format

  map_name = figsDir + prefix + yyyymmdd +'_rr_all_ds.png'
  title = 'MIRS ' + strupcase(satId) + ' EDR Rain Rate ' + date +  ' Des' + ' (V' + version +')'
  plot_grid, grid_rr_mirs_ds, sfcMask, map_name, minvalue, maxvalue, latmin,latmax,lonmin,lonmax, title, sfcPick, div, format

  sfcPick=1
  map_name = figsDir + prefix + yyyymmdd +'_rr_lnd_as.png'
  title = 'MIRS ' + strupcase(satId) + ' EDR Rain Rate ' + date +  ' Asc' + ' (V' + version +')'
  plot_grid, grid_rr_mirs_as, sfcMask, map_name, minvalue, maxvalue, latmin,latmax,lonmin,lonmax, title, sfcPick, div, format

  map_name = figsDir + prefix + yyyymmdd +'_rr_lnd_ds.png'
  title = 'MIRS ' + strupcase(satId) + ' IDR Ice Water Path ' + date +  ' Des' + ' (V' + version +')'
  plot_grid, grid_rr_mirs_ds, sfcMask, map_name, minvalue, maxvalue, latmin,latmax,lonmin,lonmax, title, sfcPick, div, format

  sfcPick=0
  map_name = figsDir + prefix + yyyymmdd +'_rr_sea_as.png'
  title = 'MIRS ' + strupcase(satId) + ' EDR Rain Rate ' + date +  ' Asc' + ' (V' + version +')'
  plot_grid, grid_rr_mirs_as, sfcMask, map_name, minvalue, maxvalue, latmin,latmax,lonmin,lonmax, title, sfcPick, div, format

  map_name = figsDir + prefix + yyyymmdd +'_rr_sea_ds.png'
  title = 'MIRS ' + strupcase(satId) + ' EDR Rain Rate ' + date +  ' Des' + ' (V' + version +')'
  plot_grid, grid_rr_mirs_ds, sfcMask, map_name, minvalue, maxvalue, latmin,latmax,lonmin,lonmax, title, sfcPick, div, format

ENDIF


;***************************************************************
;  Plot gridded MSPPS RR
;***************************************************************
minvalue=0.0
maxvalue=10.0
div=5
format='(f4.1)'

color_table_index = 41

sfcPick=2
map_name = figsDir + prefix_mspps + yyyymmdd +'_rr_all_as.png'
title = 'MSPPS ' + strupcase(satId) + ' EDR Rain Rate ' + date +  ' Asc' + ' (V' + version +')'
plot_grid, grid_rr_mspps_as, sfcMask, map_name, minvalue, maxvalue, latmin,latmax,lonmin,lonmax, title, sfcPick, div, format, color_table_index=color_table_index

map_name = figsDir + prefix_mspps + yyyymmdd +'_rr_all_ds.png'
title = 'MSPPS ' + strupcase(satId) + ' EDR Rain Rate ' + date +  ' Des' + ' (V' + version +')'
plot_grid, grid_rr_mspps_ds, sfcMask, map_name, minvalue, maxvalue, latmin,latmax,lonmin,lonmax, title, sfcPick, div, format, color_table_index=color_table_index

sfcPick=1
map_name = figsDir + prefix_mspps + yyyymmdd +'_rr_lnd_as.png'
title = 'MSPPS ' + strupcase(satId) + ' EDR Rain Rate ' + date +  ' Asc' + ' (V' + version +')'
plot_grid, grid_rr_mspps_as, sfcMask, map_name, minvalue, maxvalue, latmin,latmax,lonmin,lonmax, title, sfcPick, div, format, color_table_index=color_table_index

map_name = figsDir + prefix_mspps + yyyymmdd +'_rr_lnd_ds.png'
title = 'MSPPS ' + strupcase(satId) + ' EDR Rain Rate ' + date +  ' Des' + ' (V' + version +')'
plot_grid, grid_rr_mspps_ds, sfcMask, map_name, minvalue, maxvalue, latmin,latmax,lonmin,lonmax, title, sfcPick, div, format, color_table_index=color_table_index

sfcPick=0
map_name = figsDir + prefix_mspps + yyyymmdd +'_rr_sea_as.png'
title = 'MSPPS ' + strupcase(satId) + ' EDR Rain Rate ' + date +  ' Asc' + ' (V' + version +')'
plot_grid, grid_rr_mspps_as, sfcMask, map_name, minvalue, maxvalue, latmin,latmax,lonmin,lonmax, title, sfcPick, div, format, color_table_index=color_table_index

map_name = figsDir + prefix_mspps + yyyymmdd +'_rr_sea_ds.png'
title = 'MSPPS ' + strupcase(satId) + ' EDR Rain Rate ' + date +  ' Des' + ' (V' + version +')'
plot_grid, grid_rr_mspps_ds, sfcMask, map_name, minvalue, maxvalue, latmin,latmax,lonmin,lonmax, title, sfcPick, div, format, color_table_index=color_table_index


;***************************************************************
;    plot RR  difference
;***************************************************************
diff_grid_rr_as = fltarr(NCOL,NROW)
diff_grid_rr_ds = fltarr(NCOL,NROW)

diff_grid_rr_as(*,*) = -999.0
diff_grid_rr_ds(*,*) = -999.0

for j = 0, NROW-1 do begin
for i = 0, NCOL-1 do begin
    if ( grid_rr_mirs_as(i,j) ge 0 and grid_rr_mspps_as(i,j) ge 0 ) then $
        diff_grid_rr_as(i,j) =  grid_rr_mirs_as(i,j) - grid_rr_mspps_as(i,j)

    if ( grid_rr_mirs_ds(i,j) ge 0 and grid_rr_mspps_ds(i,j) ge 0 ) then $
        diff_grid_rr_ds(i,j) =  grid_rr_mirs_ds(i,j) - grid_rr_mspps_ds(i,j)
endfor
endfor

minvalue=-5.0
maxvalue=5.0
div=10
format='(f5.2)'

sfcPick=2
map_name = figsDir + prefix_diff + yyyymmdd +'_rr_all_as.png'
title = 'MIRS ' + strupcase(satId) + ' - MSPPS RR ' + date +  ' Asc' + ' (V' + version +')'
plot_grid, diff_grid_rr_as, sfcMask, map_name, minvalue, maxvalue, latmin,latmax,lonmin,lonmax, title, sfcPick, div, format

map_name = figsDir + prefix_diff + yyyymmdd +'_rr_all_ds.png'
title = 'MIRS ' + strupcase(satId) + ' - MSPPS RR ' + date +  ' Des' + ' (V' + version +')'
plot_grid, diff_grid_rr_ds, sfcMask, map_name, minvalue, maxvalue, latmin,latmax,lonmin,lonmax,title, sfcPick, div, format


sfcPick=1
map_name = figsDir + prefix_diff + yyyymmdd +'_rr_lnd_as.png'
title = 'MIRS ' + strupcase(satId) + ' - MSPPS RR ' + date +  ' Asc' + ' (V' + version +')'
plot_grid, diff_grid_rr_as, sfcMask, map_name, minvalue, maxvalue, latmin,latmax,lonmin,lonmax, title, sfcPick, div, format

map_name = figsDir + prefix_diff + yyyymmdd +'_rr_lnd_ds.png'
title = 'MIRS ' + strupcase(satId) + ' - MSPPS RR ' + date +  ' Des' + ' (V' + version +')'
plot_grid, diff_grid_rr_ds, sfcMask, map_name, minvalue, maxvalue, latmin,latmax,lonmin,lonmax,title, sfcPick, div, format


sfcPick=0
map_name = figsDir + prefix_diff + yyyymmdd +'_rr_sea_as.png'
title = 'MIRS ' + strupcase(satId) + ' - MSPPS RR ' + date +  ' Asc' + ' (V' + version +')'
plot_grid, diff_grid_rr_as, sfcMask, map_name, minvalue, maxvalue, latmin,latmax,lonmin,lonmax, title, sfcPick, div, format

map_name = figsDir + prefix_diff + yyyymmdd +'_rr_sea_ds.png'
title = 'MIRS ' + strupcase(satId) + ' - MSPPS RR ' + date +  ' Des' + ' (V' + version +')'
plot_grid, diff_grid_rr_ds, sfcMask, map_name, minvalue, maxvalue, latmin,latmax,lonmin,lonmax,title, sfcPick, div, format



;***************************************************************
;  Plot gridded MSPPS LST
;***************************************************************
minvalue=200
maxvalue=325
div=5
format='(I3)'

sfcPick=1
map_name = figsDir + prefix_mspps + yyyymmdd +'_tskin_lnd_as.png'
title = 'MSPPS ' + strupcase(satId) + ' Surface Temperature ' + date +  ' Asc' + ' (V' + version +')'
plot_grid, grid_tskin_mspps_as, sfcMask, map_name, minvalue, maxvalue, latmin,latmax,lonmin,lonmax, title, sfcPick, div, format

map_name = figsDir + prefix_mspps + yyyymmdd +'_tskin_lnd_ds.png'
title = 'MSPPS ' + strupcase(satId) + ' Surface Temperature ' + date +  ' Des' + ' (V' + version +')'
plot_grid, grid_tskin_mspps_ds, sfcMask, map_name, minvalue, maxvalue, latmin,latmax,lonmin,lonmax, title, sfcPick, div, format


;***************************************************************
;    plot LST difference of MIRS and MSPPS
;***************************************************************
diff_grid_tskin_as = fltarr(NCOL,NROW)
diff_grid_tskin_ds = fltarr(NCOL,NROW)

diff_grid_tskin_as(*,*) = -999.0
diff_grid_tskin_ds(*,*) = -999.0

for j = 0, NROW-1 do begin
for i = 0, NCOL-1 do begin
    if ( grid_tskin_mirs_as(i,j) gt 0 and grid_tskin_mspps_as(i,j) gt 0 ) then $
        diff_grid_tskin_as(i,j) =  grid_tskin_mirs_as(i,j) - grid_tskin_mspps_as(i,j)

    if ( grid_tskin_mirs_ds(i,j) gt 0 and grid_tskin_mspps_ds(i,j) gt 0 ) then $
        diff_grid_tskin_ds(i,j) =  grid_tskin_mirs_ds(i,j) - grid_tskin_mspps_ds(i,j)
endfor
endfor

minvalue=-4
maxvalue=4
div=8
format='(I2)'

sfcPick=1
map_name = figsDir + prefix_diff + yyyymmdd +'_tskin_lnd_as.png'
title = 'MIRS Skin Temp. ' + strupcase(satId) + ' - MSPPS Sfc Temp. ' + date +  ' Asc' + ' (V' + version +')'
plot_grid, diff_grid_tskin_as, sfcMask, map_name, minvalue, maxvalue, latmin,latmax,lonmin,lonmax, title, sfcPick, div, format

map_name = figsDir + prefix_diff + yyyymmdd +'_tskin_lnd_ds.png'
title = 'MIRS Skin Temp. ' + strupcase(satId) + ' - MSPPS Sfc Temp. ' + date +  ' Des' + ' (V' + version +')'
plot_grid, diff_grid_tskin_ds, sfcMask, map_name, minvalue, maxvalue, latmin,latmax,lonmin,lonmax,title, sfcPick, div, format





;******************************************************************************************************************************
;
;    P2P Section
;
;******************************************************************************************************************************
;---- CLW
minval=0
maxval=0.7
  
TVLCT, r, g, b, /get
r(0)=255 & g(0)=255 & b(0)=255
r(255)=0 & g(255)=0 & b(255)=0

r(7)=0 & g(7)=0 & b(7)=255
r(8)=255 & g(8)=0 & b(8)=0

clw_ranges=[0.0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7]

tt=where( mirs_clw_as ge 0 and mspps_clw_as ge 0 and mirs_sfc_as eq 0 and mirs_qc_as ne 2, cnt)
if ( cnt gt 1 ) then begin
  img_name=figsDir+prefix_mspps+'p2p_'+yyyymmdd+'_clw_sea_as.png'
  plot,  	clw_ranges,clw_ranges,$
	  /nodata, Xticks=7, Yticks=7, charsize=0.75 ,xrange=[minval,maxval], yrange=[minval,maxval], $
	  xTitle='MIRS Retrieval', yTitle='MSPPS', title='Asc Ocean CLW (mm) ' +date
  oplot, 	mirs_clw_as(tt),mspps_clw_as(tt), psym=2, symsize=0.25, color=7
  oplot, [minval,maxval], [minval,maxval], linestyle=0, color=8
  corr = correlate(mirs_clw_as(tt), mspps_clw_as(tt))
  xyouts,0.175,0.85, 'corr='+STRTRIM(String(corr),2),/normal
  xyouts,0.175,0.80, 'nPts='+STRTRIM(String(cnt),2),/normal
  write_png, img_name, TVRD(), r, g, b									     
endif

tt=where( mirs_clw_ds ge 0 and mspps_clw_ds ge 0 and mirs_sfc_ds eq 0 and mirs_qc_ds ne 2, cnt)
if ( cnt gt 1 ) then begin
  img_name=figsDir+prefix_mspps+'p2p_'+yyyymmdd+'_clw_sea_ds.png'
  plot,  	clw_ranges,clw_ranges,$
	  /nodata, Xticks=7, Yticks=7, charsize=0.75 ,xrange=[minval,maxval], yrange=[minval,maxval], $
	  xTitle='MIRS Retrieval', yTitle='MSPPS', title='Des Ocean CLW (mm) ' +date
  oplot, 	mirs_clw_ds(tt),mspps_clw_ds(tt), psym=2, symsize=0.25, color=7
  oplot, [minval,maxval], [minval,maxval], linestyle=0, color=8
  corr = correlate(mirs_clw_ds(tt), mspps_clw_ds(tt))
  xyouts,0.175,0.85, 'corr='+STRTRIM(String(corr),2),/normal
  xyouts,0.175,0.80, 'nPts='+STRTRIM(String(cnt),2),/normal
  write_png, img_name, TVRD(), r, g, b									     
endif

;---CLW gt 0
tt=where( mirs_clw_as gt 0 and mspps_clw_as gt 0 and mirs_sfc_as eq 0 and mirs_qc_as ne 2, cnt)
if ( cnt gt 1 ) then begin
  img_name=figsDir+prefix_mspps+'p2p_'+yyyymmdd+'_clw2_sea_as.png'
  plot,  	clw_ranges,clw_ranges,$
	  /nodata, Xticks=7, Yticks=7, charsize=0.75 ,xrange=[minval,maxval], yrange=[minval,maxval], $
	  xTitle='MIRS Retrieval', yTitle='MSPPS', title='Asc Ocean CLW > 0 (mm) ' +date
  oplot, 	mirs_clw_as(tt),mspps_clw_as(tt), psym=2, symsize=0.25, color=7
  oplot, [minval,maxval], [minval,maxval], linestyle=0, color=8
  corr = correlate(mirs_clw_as(tt), mspps_clw_as(tt))
  xyouts,0.175,0.85, 'corr='+STRTRIM(String(corr),2),/normal
  xyouts,0.175,0.80, 'nPts='+STRTRIM(String(cnt),2),/normal
  write_png, img_name, TVRD(), r, g, b									     
endif

tt=where( mirs_clw_ds gt 0 and mspps_clw_ds gt 0 and mirs_sfc_ds eq 0 and mirs_qc_ds ne 2, cnt)
if ( cnt gt 1 ) then begin
  img_name=figsDir+prefix_mspps+'p2p_'+yyyymmdd+'_clw2_sea_ds.png'
  plot,  	clw_ranges,clw_ranges,$
	  /nodata, Xticks=7, Yticks=7, charsize=0.75 ,xrange=[minval,maxval], yrange=[minval,maxval], $
	  xTitle='MIRS Retrieval', yTitle='MSPPS', title='Des Ocean CLW > 0 (mm) ' +date
  oplot, 	mirs_clw_ds(tt),mspps_clw_ds(tt), psym=2, symsize=0.25, color=7
  oplot, [minval,maxval], [minval,maxval], linestyle=0, color=8
  corr = correlate(mirs_clw_ds(tt), mspps_clw_ds(tt))
  xyouts,0.175,0.85, 'corr='+STRTRIM(String(corr),2),/normal
  xyouts,0.175,0.80, 'nPts='+STRTRIM(String(cnt),2),/normal
  write_png, img_name, TVRD(), r, g, b									     
endif

;---- MIRS LWP v MSPPS CLW
minval=0
maxval=0.7
  
TVLCT, r, g, b, /get
r(0)=255 & g(0)=255 & b(0)=255
r(255)=0 & g(255)=0 & b(255)=0

r(7)=0 & g(7)=0 & b(7)=255
r(8)=255 & g(8)=0 & b(8)=0

lwp_ranges=[0.0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7]

tt=where( mirs_lwp_as ge 0 and mspps_clw_as ge 0 and mirs_sfc_as eq 0 and mirs_qc_as ne 2, cnt)
if ( cnt gt 1 ) then begin
  img_name=figsDir+prefix_mspps+'p2p_'+yyyymmdd+'_lwp_sea_as.png'
  plot,  	lwp_ranges,lwp_ranges,$
	  /nodata, Xticks=7, Yticks=7, charsize=0.75 ,xrange=[minval,maxval], yrange=[minval,maxval], $
	  xTitle='MIRS Retrieval LWP', yTitle='MSPPS CLW', title='Asc Ocean LWP (mm) ' +date
  oplot, 	mirs_lwp_as(tt),mspps_clw_as(tt), psym=2, symsize=0.25, color=7
  oplot, [minval,maxval], [minval,maxval], linestyle=0, color=8
  corr = correlate(mirs_lwp_as(tt), mspps_clw_as(tt))
  xyouts,0.175,0.85, 'corr='+STRTRIM(String(corr),2),/normal
  xyouts,0.175,0.80, 'nPts='+STRTRIM(String(cnt),2),/normal
  write_png, img_name, TVRD(), r, g, b									     
endif

tt=where( mirs_lwp_ds ge 0 and mspps_clw_ds ge 0 and mirs_sfc_ds eq 0 and mirs_qc_ds ne 2, cnt)
if ( cnt gt 1 ) then begin
  img_name=figsDir+prefix_mspps+'p2p_'+yyyymmdd+'_lwp_sea_ds.png'
  plot,  	lwp_ranges,lwp_ranges,$
	  /nodata, Xticks=7, Yticks=7, charsize=0.75 ,xrange=[minval,maxval], yrange=[minval,maxval], $
	  xTitle='MIRS Retrieval LWP', yTitle='MSPPS CLW', title='Des Ocean LWP (mm) ' +date
  oplot, 	mirs_lwp_ds(tt),mspps_clw_ds(tt), psym=2, symsize=0.25, color=7
  oplot, [minval,maxval], [minval,maxval], linestyle=0, color=8
  corr = correlate(mirs_lwp_ds(tt), mspps_clw_ds(tt))
  xyouts,0.175,0.85, 'corr='+STRTRIM(String(corr),2),/normal
  xyouts,0.175,0.80, 'nPts='+STRTRIM(String(cnt),2),/normal
  write_png, img_name, TVRD(), r, g, b									     
endif

;---MIRS LWP and MSPPS CLW gt 0
tt=where( mirs_lwp_as gt 0 and mspps_clw_as gt 0 and mirs_sfc_as eq 0 and mirs_qc_as ne 2, cnt)
if ( cnt gt 1 ) then begin
  img_name=figsDir+prefix_mspps+'p2p_'+yyyymmdd+'_lwp2_sea_as.png'
  plot,  	lwp_ranges,lwp_ranges,$
	  /nodata, Xticks=7, Yticks=7, charsize=0.75 ,xrange=[minval,maxval], yrange=[minval,maxval], $
	  xTitle='MIRS Retrieval LWP', yTitle='MSPPS CLW', title='Asc Ocean LWP > 0 (mm) ' +date
  oplot, 	mirs_lwp_as(tt),mspps_clw_as(tt), psym=2, symsize=0.25, color=7
  oplot, [minval,maxval], [minval,maxval], linestyle=0, color=8
  corr = correlate(mirs_lwp_as(tt), mspps_clw_as(tt))
  xyouts,0.175,0.85, 'corr='+STRTRIM(String(corr),2),/normal
  xyouts,0.175,0.80, 'nPts='+STRTRIM(String(cnt),2),/normal
  write_png, img_name, TVRD(), r, g, b									     
endif

tt=where( mirs_lwp_ds ge 0 and mspps_clw_ds ge 0 and mirs_sfc_ds eq 0 and mirs_qc_ds ne 2, cnt)
if ( cnt gt 1 ) then begin
  img_name=figsDir+prefix_mspps+'p2p_'+yyyymmdd+'_lwp2_sea_ds.png'
  plot,  	lwp_ranges,lwp_ranges,$
	  /nodata, Xticks=7, Yticks=7, charsize=0.75 ,xrange=[minval,maxval], yrange=[minval,maxval], $
	  xTitle='MIRS Retrieval LWP', yTitle='MSPPS CLW', title='Des Ocean LWP > 0 (mm) ' +date
  oplot, 	mirs_lwp_ds(tt),mspps_clw_ds(tt), psym=2, symsize=0.25, color=7
  oplot, [minval,maxval], [minval,maxval], linestyle=0, color=8
  corr = correlate(mirs_lwp_ds(tt), mspps_clw_ds(tt))
  xyouts,0.175,0.85, 'corr='+STRTRIM(String(corr),2),/normal
  xyouts,0.175,0.80, 'nPts='+STRTRIM(String(cnt),2),/normal
  write_png, img_name, TVRD(), r, g, b									     
endif

;---- RR scattered plot
minval=0
maxval=10
  
TVLCT, r, g, b, /get
r(0)=255 & g(0)=255 & b(0)=255
r(255)=0 & g(255)=0 & b(255)=0

r(7)=0 & g(7)=0 & b(7)=255
r(8)=255 & g(8)=0 & b(8)=0

rr_ranges_mirs=[0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
rr_ranges_mspps=[0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10]

tt=where( mirs_rr_as ge 0 and mspps_rr_as ge 0 and mirs_qc_as ne 2, cnt)
if ( cnt gt 1 ) then begin
  img_name=figsDir+prefix_mspps+'p2p_'+yyyymmdd+'_rr_all_as.png'
  plot,  	rr_ranges_mirs,rr_ranges_mspps,$
	  /nodata, Xticks=10, Yticks=10, charsize=0.75 ,xrange=[0,10], yrange=[minval,maxval], $
	  xTitle='MIRS Retrieval', yTitle='MSPPS', title='Asc RR (mm) ' +date
  oplot, 	mirs_rr_as(tt),mspps_rr_as(tt), psym=2, symsize=0.25, color=7
  oplot, [0,10], [minval,maxval], linestyle=0, color=8
  corr = correlate(mirs_rr_as(tt), mspps_rr_as(tt))
  xyouts,0.175,0.85, 'corr='+STRTRIM(String(corr),2),/normal
  xyouts,0.175,0.80, 'nPts='+STRTRIM(String(cnt),2),/normal
  write_png, img_name, TVRD(), r, g, b									     
endif

tt=where( mirs_rr_ds ge 0 and mspps_rr_ds ge 0 and mirs_qc_ds ne 2, cnt)
if ( cnt gt 1 ) then begin
  img_name=figsDir+prefix_mspps+'p2p_'+yyyymmdd+'_rr_all_ds.png'
  plot,  	rr_ranges_mirs,rr_ranges_mspps,$
	  /nodata, Xticks=10, Yticks=10, charsize=0.75 ,xrange=[0,10], yrange=[minval,maxval], $
	  xTitle='MIRS Retrieval', yTitle='MSPPS', title='Des RR (mm) ' +date
  oplot, 	mirs_rr_ds(tt),mspps_rr_ds(tt), psym=2, symsize=0.25, color=7
  oplot, [0,10], [minval,maxval], linestyle=0, color=8
  corr = correlate(mirs_rr_ds(tt), mspps_rr_ds(tt))
  xyouts,0.175,0.85, 'corr='+STRTRIM(String(corr),2),/normal
  xyouts,0.175,0.80, 'nPts='+STRTRIM(String(cnt),2),/normal
  write_png, img_name, TVRD(), r, g, b									     
endif


tt=where( mirs_rr_as ge 0 and mspps_rr_as ge 0 and mirs_sfc_as eq 0 and mirs_qc_as ne 2,cnt)
if ( cnt gt 1 ) then begin
  img_name=figsDir+prefix_mspps+'p2p_'+yyyymmdd+'_rr_sea_as.png'
  plot,  	rr_ranges_mirs,rr_ranges_mspps,$
	  /nodata, Xticks=10, Yticks=10, charsize=0.75 ,xrange=[0,10], yrange=[minval,maxval], $
	  xTitle='MIRS Retrieval', yTitle='MSPPS', title='Asc Ocean RR (mm) ' +date
  oplot, 	mirs_rr_as(tt),mspps_rr_as(tt), psym=2, symsize=0.25, color=7
  oplot, [0,10], [minval,maxval], linestyle=0, color=8
  corr = correlate(mirs_rr_as(tt), mspps_rr_as(tt))
  xyouts,0.175,0.85, 'corr='+STRTRIM(String(corr),2),/normal
  xyouts,0.175,0.80, 'nPts='+STRTRIM(String(cnt),2),/normal
  write_png, img_name, TVRD(), r, g, b									     
endif

tt=where( mirs_rr_ds ge 0 and mspps_rr_ds ge 0 and mirs_sfc_ds eq 0 and mirs_qc_ds ne 2,cnt)
if ( cnt gt 1 ) then begin
  img_name=figsDir+prefix_mspps+'p2p_'+yyyymmdd+'_rr_sea_ds.png'
  plot,  	rr_ranges_mirs,rr_ranges_mspps,$
	  /nodata, Xticks=10, Yticks=10, charsize=0.75 ,xrange=[0,10], yrange=[minval,maxval], $
	  xTitle='MIRS Retrieval', yTitle='MSPPS', title='Des Ocean RR (mm) ' +date
  oplot, 	mirs_rr_ds(tt),mspps_rr_ds(tt), psym=2, symsize=0.25, color=7
  oplot, [0,10], [minval,maxval], linestyle=0, color=8
  corr = correlate(mirs_rr_ds(tt), mspps_rr_ds(tt))
  xyouts,0.175,0.85, 'corr='+STRTRIM(String(corr),2),/normal
  xyouts,0.175,0.80, 'nPts='+STRTRIM(String(cnt),2),/normal
  write_png, img_name, TVRD(), r, g, b									     
endif


tt=where( mirs_rr_as ge 0 and mspps_rr_as ge 0 and mirs_sfc_as eq 2 and mirs_qc_as ne 2,cnt)
if ( cnt gt 1 ) then begin
  img_name=figsDir+prefix_mspps+'p2p_'+yyyymmdd+'_rr_lnd_as.png'
  plot,  	rr_ranges_mirs,rr_ranges_mspps,$
	/nodata, Xticks=10, Yticks=10, charsize=0.75 ,xrange=[0,10], yrange=[minval,maxval], $
	xTitle='MIRS Retrieval', yTitle='MSPPS', title='Asc Land RR (mm) ' +date
  oplot, 	mirs_rr_as(tt),mspps_rr_as(tt), psym=2, symsize=0.25, color=7
  oplot, [0,10], [minval,maxval], linestyle=0, color=8
  corr = correlate(mirs_rr_as(tt), mspps_rr_as(tt))
  xyouts,0.175,0.85, 'corr='+STRTRIM(String(corr),2),/normal
  xyouts,0.175,0.80, 'nPts='+STRTRIM(String(cnt),2),/normal
  write_png, img_name, TVRD(), r, g, b									     
endif

tt=where( mirs_rr_ds ge 0 and mspps_rr_ds ge 0 and mirs_sfc_ds eq 2 and mirs_qc_ds ne 2,cnt)
if ( cnt gt 1 ) then begin
  img_name=figsDir+prefix_mspps+'p2p_'+yyyymmdd+'_rr_lnd_ds.png'
  plot,  	rr_ranges_mirs,rr_ranges_mspps,$
	/nodata, Xticks=10, Yticks=10, charsize=0.75 ,xrange=[0,10], yrange=[minval,maxval], $
	xTitle='MIRS Retrieval', yTitle='MSPPS', title='Des Land RR (mm) ' +date
  oplot, 	mirs_rr_ds(tt),mspps_rr_ds(tt), psym=2, symsize=0.25, color=7
  oplot, [0,10], [minval,maxval], linestyle=0, color=8
  corr = correlate(mirs_rr_ds(tt), mspps_rr_ds(tt))
  xyouts,0.175,0.85, 'corr='+STRTRIM(String(corr),2),/normal
  xyouts,0.175,0.80, 'nPts='+STRTRIM(String(cnt),2),/normal
  write_png, img_name, TVRD(), r, g, b									     
endif

;---RR where both MIRS and MSPPS are gt 0
tt=where( mirs_rr_as gt 0 and mspps_rr_as gt 0 and mirs_qc_as ne 2, cnt)
if ( cnt gt 1 ) then begin
  img_name=figsDir+prefix_mspps+'p2p_'+yyyymmdd+'_rr2_all_as.png'
  plot,  	rr_ranges_mirs,rr_ranges_mspps,$
	  /nodata, Xticks=10, Yticks=10, charsize=0.75 ,xrange=[0,10], yrange=[minval,maxval], $
	  xTitle='MIRS Retrieval', yTitle='MSPPS', title='Asc RR > 0 (mm) ' +date
  oplot, 	mirs_rr_as(tt),mspps_rr_as(tt), psym=2, symsize=0.25, color=7
  oplot, [0,10], [minval,maxval], linestyle=0, color=8
  corr = correlate(mirs_rr_as(tt), mspps_rr_as(tt))
  xyouts,0.175,0.85, 'corr='+STRTRIM(String(corr),2),/normal
  xyouts,0.175,0.80, 'nPts='+STRTRIM(String(cnt),2),/normal
  write_png, img_name, TVRD(), r, g, b									     
endif

tt=where( mirs_rr_ds gt 0 and mspps_rr_ds gt 0 and mirs_qc_ds ne 2, cnt)
if ( cnt gt 1 ) then begin
  img_name=figsDir+prefix_mspps+'p2p_'+yyyymmdd+'_rr2_all_ds.png'
  plot,  	rr_ranges_mirs,rr_ranges_mspps,$
	  /nodata, Xticks=10, Yticks=10, charsize=0.75 ,xrange=[0,10], yrange=[minval,maxval], $
	  xTitle='MIRS Retrieval', yTitle='MSPPS', title='Des RR > 0 (mm) ' +date
  oplot, 	mirs_rr_ds(tt),mspps_rr_ds(tt), psym=2, symsize=0.25, color=7
  oplot, [0,10], [minval,maxval], linestyle=0, color=8
  corr = correlate(mirs_rr_ds(tt), mspps_rr_ds(tt))
  xyouts,0.175,0.85, 'corr='+STRTRIM(String(corr),2),/normal
  xyouts,0.175,0.80, 'nPts='+STRTRIM(String(cnt),2),/normal
  write_png, img_name, TVRD(), r, g, b									     
endif


tt=where( mirs_rr_as gt 0 and mspps_rr_as gt 0 and mirs_sfc_as eq 0 and mirs_qc_as ne 2,cnt)
if ( cnt gt 1 ) then begin
  img_name=figsDir+prefix_mspps+'p2p_'+yyyymmdd+'_rr2_sea_as.png'
  plot,  	rr_ranges_mirs,rr_ranges_mspps,$
	  /nodata, Xticks=10, Yticks=10, charsize=0.75 ,xrange=[0,10], yrange=[minval,maxval], $
	  xTitle='MIRS Retrieval', yTitle='MSPPS', title='Asc Ocean RR > 0 (mm) ' +date
  oplot, 	mirs_rr_as(tt),mspps_rr_as(tt), psym=2, symsize=0.25, color=7
  oplot, [0,10], [minval,maxval], linestyle=0, color=8
  corr = correlate(mirs_rr_as(tt), mspps_rr_as(tt))
  xyouts,0.175,0.85, 'corr='+STRTRIM(String(corr),2),/normal
  xyouts,0.175,0.80, 'nPts='+STRTRIM(String(cnt),2),/normal
  write_png, img_name, TVRD(), r, g, b									     
endif

tt=where( mirs_rr_ds gt 0 and mspps_rr_ds gt 0 and mirs_sfc_ds eq 0 and mirs_qc_ds ne 2,cnt)
if ( cnt gt 1 ) then begin
  img_name=figsDir+prefix_mspps+'p2p_'+yyyymmdd+'_rr2_sea_ds.png'
  plot,  	rr_ranges_mirs,rr_ranges_mspps,$
	  /nodata, Xticks=10, Yticks=10, charsize=0.75 ,xrange=[0,10], yrange=[minval,maxval], $
	  xTitle='MIRS Retrieval', yTitle='MSPPS', title='Des Ocean RR > 0 (mm) ' +date
  oplot, 	mirs_rr_ds(tt),mspps_rr_ds(tt), psym=2, symsize=0.25, color=7
  oplot, [0,10], [minval,maxval], linestyle=0, color=8
  corr = correlate(mirs_rr_ds(tt), mspps_rr_ds(tt))
  xyouts,0.175,0.85, 'corr='+STRTRIM(String(corr),2),/normal
  xyouts,0.175,0.80, 'nPts='+STRTRIM(String(cnt),2),/normal
  write_png, img_name, TVRD(), r, g, b									     
endif


tt=where( mirs_rr_as gt 0 and mspps_rr_as gt 0 and mirs_sfc_as eq 2 and mirs_qc_as ne 2,cnt)
if ( cnt gt 1 ) then begin
  img_name=figsDir+prefix_mspps+'p2p_'+yyyymmdd+'_rr2_lnd_as.png'
  plot,  	rr_ranges_mirs,rr_ranges_mspps,$
	/nodata, Xticks=10, Yticks=10, charsize=0.75 ,xrange=[0,10], yrange=[minval,maxval], $
	xTitle='MIRS Retrieval', yTitle='MSPPS', title='Asc Land RR > 0 (mm) ' +date
  oplot, 	mirs_rr_as(tt),mspps_rr_as(tt), psym=2, symsize=0.25, color=7
  oplot, [0,10], [minval,maxval], linestyle=0, color=8
  corr = correlate(mirs_rr_as(tt), mspps_rr_as(tt))
  xyouts,0.175,0.85, 'corr='+STRTRIM(String(corr),2),/normal
  xyouts,0.175,0.80, 'nPts='+STRTRIM(String(cnt),2),/normal
  write_png, img_name, TVRD(), r, g, b									     
endif

tt=where( mirs_rr_ds gt 0 and mspps_rr_ds gt 0 and mirs_sfc_ds eq 2 and mirs_qc_ds ne 2,cnt)
if ( cnt gt 1 ) then begin
  img_name=figsDir+prefix_mspps+'p2p_'+yyyymmdd+'_rr2_lnd_ds.png'
  plot,  	rr_ranges_mirs,rr_ranges_mspps,$
	/nodata, Xticks=10, Yticks=10, charsize=0.75 ,xrange=[0,10], yrange=[minval,maxval], $
	xTitle='MIRS Retrieval', yTitle='MSPPS', title='Des Land RR > 0 (mm) ' +date
  oplot, 	mirs_rr_ds(tt),mspps_rr_ds(tt), psym=2, symsize=0.25, color=7
  oplot, [0,10], [minval,maxval], linestyle=0, color=8
  corr = correlate(mirs_rr_ds(tt), mspps_rr_ds(tt))
  xyouts,0.175,0.85, 'corr='+STRTRIM(String(corr),2),/normal
  xyouts,0.175,0.80, 'nPts='+STRTRIM(String(cnt),2),/normal
  write_png, img_name, TVRD(), r, g, b									     
endif


;---- IWP scattered plot
minval=0
maxval=1.0
  
TVLCT, r, g, b, /get
r(0)=255 & g(0)=255 & b(0)=255
r(255)=0 & g(255)=0 & b(255)=0

r(7)=0 & g(7)=0 & b(7)=255
r(8)=255 & g(8)=0 & b(8)=0

iwp_ranges=[0.0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0]

tt=where( mirs_iwp_as ge 0 and mspps_iwp_as ge 0 and mirs_qc_as ne 2,cnt)
if ( cnt gt 1 ) then begin
  img_name=figsDir+prefix_mspps+'p2p_'+yyyymmdd+'_iwp_all_as.png'
  plot,  	iwp_ranges,iwp_ranges,$
	  /nodata, Xticks=10, Yticks=10, charsize=0.75 ,xrange=[minval,maxval], yrange=[minval,maxval], $
	  xTitle='MIRS Retrieval', yTitle='MSPPS', title='Asc iwp(kg/m^2) ' +date
  oplot, 	mirs_iwp_as(tt),mspps_iwp_as(tt), psym=2, symsize=0.25, color=7
  oplot, [minval,maxval], [minval,maxval], linestyle=0, color=8
  corr = correlate(mirs_iwp_as(tt), mspps_iwp_as(tt))
  xyouts,0.175,0.85, 'corr='+STRTRIM(String(corr),2),/normal
  xyouts,0.175,0.80, 'nPts='+STRTRIM(String(cnt),2),/normal
  write_png, img_name, TVRD(), r, g, b									     
endif

tt=where( mirs_iwp_ds ge 0 and mspps_iwp_ds ge 0 and mirs_qc_ds ne 2,cnt)
if ( cnt gt 1 ) then begin
  img_name=figsDir+prefix_mspps+'p2p_'+yyyymmdd+'_iwp_all_ds.png'
  plot,  	iwp_ranges,iwp_ranges,$
	  /nodata, Xticks=10, Yticks=10, charsize=0.75 ,xrange=[minval,maxval], yrange=[minval,maxval], $
	  xTitle='MIRS Retrieval', yTitle='MSPPS', title='Des iwp(kg/m^2) ' +date
  oplot, 	mirs_iwp_ds(tt),mspps_iwp_ds(tt), psym=2, symsize=0.25, color=7
  oplot, [minval,maxval], [minval,maxval], linestyle=0, color=8
  corr = correlate(mirs_iwp_ds(tt), mspps_iwp_ds(tt))
  xyouts,0.175,0.85, 'corr='+STRTRIM(String(corr),2),/normal
  xyouts,0.175,0.80, 'nPts='+STRTRIM(String(cnt),2),/normal
  write_png, img_name, TVRD(), r, g, b									     
endif


tt=where( mirs_iwp_as ge 0 and mspps_iwp_as ge 0 and mirs_sfc_as eq 0 and mirs_qc_as ne 2,cnt)
if ( cnt gt 1 ) then begin
  img_name=figsDir+prefix_mspps+'p2p_'+yyyymmdd+'_iwp_sea_as.png'
  plot,  	iwp_ranges,iwp_ranges,$
	  /nodata, Xticks=10, Yticks=10, charsize=0.75 ,xrange=[minval,maxval], yrange=[minval,maxval], $
	  xTitle='MIRS Retrieval', yTitle='MSPPS', title='Asc Ocean iwp(kg/m^2) ' +date
  oplot, 	mirs_iwp_as(tt),mspps_iwp_as(tt), psym=2, symsize=0.25, color=7
  oplot, [minval,maxval], [minval,maxval], linestyle=0, color=8
  corr = correlate(mirs_iwp_as(tt), mspps_iwp_as(tt))
  xyouts,0.175,0.85, 'corr='+STRTRIM(String(corr),2),/normal
  xyouts,0.175,0.80, 'nPts='+STRTRIM(String(cnt),2),/normal
  write_png, img_name, TVRD(), r, g, b									     
endif

tt=where( mirs_iwp_ds ge 0 and mspps_iwp_ds ge 0 and mirs_sfc_ds eq 0 and mirs_qc_ds ne 2,cnt)
if ( cnt gt 1 ) then begin
  img_name=figsDir+prefix_mspps+'p2p_'+yyyymmdd+'_iwp_sea_ds.png'
  plot,  	iwp_ranges,iwp_ranges,$
	  /nodata, Xticks=10, Yticks=10, charsize=0.75 ,xrange=[minval,maxval], yrange=[minval,maxval], $
	  xTitle='MIRS Retrieval', yTitle='MSPPS', title='Des Ocean iwp(kg/m^2) ' +date
  oplot, 	mirs_iwp_ds(tt),mspps_iwp_ds(tt), psym=2, symsize=0.25, color=7
  oplot, [minval,maxval], [minval,maxval], linestyle=0, color=8
  corr = correlate(mirs_iwp_ds(tt), mspps_iwp_ds(tt))
  xyouts,0.175,0.85, 'corr='+STRTRIM(String(corr),2),/normal
  xyouts,0.175,0.80, 'nPts='+STRTRIM(String(cnt),2),/normal
  write_png, img_name, TVRD(), r, g, b									     
endif


tt=where( mirs_iwp_as ge 0 and mspps_iwp_as ge 0 and mirs_sfc_as eq 2 and mirs_qc_as ne 2,cnt)
if ( cnt gt 1 ) then begin
  img_name=figsDir+prefix_mspps+'p2p_'+yyyymmdd+'_iwp_lnd_as.png'
  plot,  	iwp_ranges,iwp_ranges,$
	  /nodata, Xticks=10, Yticks=10, charsize=0.75 ,xrange=[minval,maxval], yrange=[minval,maxval], $
	  xTitle='MIRS Retrieval', yTitle='MSPPS', title='Asc Land iwp(kg/m^2) ' +date
  oplot, 	mirs_iwp_as(tt),mspps_iwp_as(tt), psym=2, symsize=0.25, color=7
  oplot, [minval,maxval], [minval,maxval], linestyle=0, color=8
  corr = correlate(mirs_iwp_as(tt), mspps_iwp_as(tt))
  xyouts,0.175,0.85, 'corr='+STRTRIM(String(corr),2),/normal
  xyouts,0.175,0.80, 'nPts='+STRTRIM(String(cnt),2),/normal
  write_png, img_name, TVRD(), r, g, b									     
endif

tt=where( mirs_iwp_ds ge 0 and mspps_iwp_ds ge 0 and mirs_sfc_ds eq 2 and mirs_qc_ds ne 2,cnt)
if ( cnt gt 1 ) then begin
  img_name=figsDir+prefix_mspps+'p2p_'+yyyymmdd+'_iwp_lnd_ds.png'
  plot,  	iwp_ranges,iwp_ranges,$
	  /nodata, Xticks=10, Yticks=10, charsize=0.75 ,xrange=[minval,maxval], yrange=[minval,maxval], $
	  xTitle='MIRS Retrieval', yTitle='MSPPS', title='Des Land iwp(kg/m^2) ' +date
  oplot, 	mirs_iwp_ds(tt),mspps_iwp_ds(tt), psym=2, symsize=0.25, color=7
  oplot, [minval,maxval], [minval,maxval], linestyle=0, color=8
  corr = correlate(mirs_iwp_ds(tt), mspps_iwp_ds(tt))
  xyouts,0.175,0.85, 'corr='+STRTRIM(String(corr),2),/normal
  xyouts,0.175,0.80, 'nPts='+STRTRIM(String(cnt),2),/normal
  write_png, img_name, TVRD(), r, g, b									     
endif

;---- SWE
minval=0
maxval=16

tt=where( mirs_swe_as ge 0 and mspps_swe_as ge 0 and mirs_sfc_as eq 2 and mirs_qc_as eq 0, cnt )    
if ( cnt gt 1 ) then begin
  img_name=figsDir+prefix_mspps+'p2p_'+yyyymmdd+'_swe_lnd_as.png'
  ChoppVec_png,minval,maxval,minval,maxval,mirs_swe_as(tt),mspps_swe_as(tt),10,10,'MIRS Retrieval','MSPPS','Asc Snow Water Equivalent (cm) Over Land ' +date + ' (V' + version +')',1,1,stats, img_name
endif
if ( cnt le 1 ) then begin
  img_name=figsDir+prefix_mspps+'p2p_'+yyyymmdd+'_swe_lnd_as.png'
  ChoppVec_png_dummy,minval,maxval,minval,maxval,'MIRS Retrieval','MSPPS','Asc Snow Water Equivalent (cm) Over Land ' +date + ' (V' + version +')',1,img_name
endif
  
tt=where( mirs_swe_ds ge 0 and mspps_swe_ds ge 0 and mirs_sfc_ds eq 2 and mirs_qc_ds eq 0, cnt )
if ( cnt gt 1 ) then begin
  img_name=figsDir+prefix_mspps+'p2p_'+yyyymmdd+'_swe_lnd_ds.png'
  ChoppVec_png,minval,maxval,minval,maxval,mirs_swe_ds(tt),mspps_swe_ds(tt),10,10,'MIRS Retrieval','MSPPS','Des Snow Water Equivalent (cm) Over Land ' +date + ' (V' + version +')',1,1,stats, img_name
endif
if ( cnt le 1 ) then begin
  img_name=figsDir+prefix_mspps+'p2p_'+yyyymmdd+'_swe_lnd_ds.png'
  ChoppVec_png_dummy,minval,maxval,minval,maxval,'MIRS Retrieval','MSPPS','Des Snow Water Equivalent (cm) Over Land ' +date + ' (V' + version +')',1,img_name
endif

tt=where( mirs_swe_as ge 0 and mspps_swe_as ge 0 and mirs_sfc_as eq 3 and mirs_qc_as eq 0, cnt )
if ( cnt gt 1 ) then begin
  img_name=figsDir+prefix_mspps+'p2p_'+yyyymmdd+'_swe_snw_as.png'
  ChoppVec_png,minval,maxval,minval,maxval,mirs_swe_as(tt),mspps_swe_as(tt),10,10,'MIRS Retrieval','MSPPS','Asc Snow Water Equivalent (cm) Over Snow ' +date + ' (V' + version +')',1,1,stats, img_name
endif
if ( cnt le 1 ) then begin
  img_name=figsDir+prefix_mspps+'p2p_'+yyyymmdd+'_swe_snw_as.png'
  ChoppVec_png_dummy,minval,maxval,minval,maxval,'MIRS Retrieval','MSPPS','Asc Snow Water Equivalent (cm) Over Snow ' +date + ' (V' + version +')',1,img_name
endif
  
tt=where( mirs_swe_ds ge 0 and mspps_swe_ds ge 0 and mirs_sfc_ds eq 3 and mirs_qc_ds eq 0, cnt )
if ( cnt gt 1 ) then begin
  img_name=figsDir+prefix_mspps+'p2p_'+yyyymmdd+'_swe_snw_ds.png'
  ChoppVec_png,minval,maxval,minval,maxval,mirs_swe_ds(tt),mspps_swe_ds(tt),10,10,'MIRS Retrieval','MSPPS','Des Snow Water Equivalent (cm) Over Snow ' +date + ' (V' + version +')',1,1,stats, img_name
endif
if ( cnt le 1 ) then begin
  img_name=figsDir+prefix_mspps+'p2p_'+yyyymmdd+'_swe_snw_ds.png'
  ChoppVec_png_dummy,minval,maxval,minval,maxval,'MIRS Retrieval','MSPPS','Des Snow Water Equivalent (cm) Over Snow ' +date + ' (V' + version +')',1,img_name
endif
   
;---- SIC
minval=0
maxval=100

tt=where( mirs_sic_as ge 0 and mspps_sic_as ge 0 and mirs_sfc_as eq 0 and mirs_qc_as eq 0, cnt )
if ( cnt gt 1 ) then begin
  img_name=figsDir+prefix_mspps+'p2p_'+yyyymmdd+'_sice_sea_as.png'
  ChoppVec_png,minval,maxval,minval,maxval,mirs_sic_as(tt),mspps_sic_as(tt),10,10,'MIRS Retrieval','MSPPS','Asc Sea Ice Concentration (%) Over Ocean ' +date + ' (V' + version +')',1,1,stats, img_name
endif
   
tt=where( mirs_sic_ds ge 0 and mspps_sic_ds ge 0 and mirs_sfc_ds eq 0 and mirs_qc_ds eq 0, cnt )
if ( cnt gt 1 ) then begin
  img_name=figsDir+prefix_mspps+'p2p_'+yyyymmdd+'_sice_sea_ds.png'
  ChoppVec_png,minval,maxval,minval,maxval,mirs_sic_ds(tt),mspps_sic_ds(tt),10,10,'MIRS Retrieval','MSPPS','Des Sea Ice Concentration (%) Over Ocean ' +date + ' (V' + version +')',1,1,stats, img_name
endif


tt=where( mirs_sic_as ge 0 and mspps_sic_as ge 0 and mirs_sfc_as eq 1 and mirs_qc_as eq 0, cnt )
if ( cnt gt 1 ) then begin
  img_name=figsDir+prefix_mspps+'p2p_'+yyyymmdd+'_sice_ice_as.png'
  ChoppVec_png,minval,maxval,minval,maxval,mirs_sic_as(tt),mspps_sic_as(tt),10,10,'MIRS Retrieval','MSPPS','Asc Sea Ice Concentration (%) Over Ice ' +date + ' (V' + version +')',1,1,stats, img_name
endif
   
tt=where( mirs_sic_ds ge 0 and mspps_sic_ds ge 0 and mirs_sfc_ds eq 1 and mirs_qc_ds eq 0, cnt )
if ( cnt gt 1 ) then begin
  img_name=figsDir+prefix_mspps+'p2p_'+yyyymmdd+'_sice_ice_ds.png'
  ChoppVec_png,minval,maxval,minval,maxval,mirs_sic_ds(tt),mspps_sic_ds(tt),10,10,'MIRS Retrieval','MSPPS','Des Sea Ice Concentration (%) Over Ice ' +date + ' (V' + version +')',1,1,stats, img_name
endif


;---- EM1
minval=0.65
maxval=1.00

tt=where( mirs_em1_as ge 0 and mspps_em1_as ge 0 and mirs_sfc_as eq 2 and mirs_qc_as eq 0, cnt )
if ( cnt gt 1 ) then begin
  img_name=figsDir+prefix_mspps+'p2p_'+yyyymmdd+'_em_23v_lnd_as.png'
  ChoppVec_png,minval,maxval,minval,maxval,mirs_em1_as(tt),mspps_em1_as(tt),10,10,'MIRS Retrieval','MSPPS','Asc Emissivity at 23V Over Land ' +date + ' (V' + version +')',1,1,stats, img_name
endif

tt=where( mirs_em1_ds ge 0 and mspps_em1_ds ge 0 and mirs_sfc_ds eq 2 and mirs_qc_ds eq 0, cnt )
if ( cnt gt 1 ) then begin
  img_name=figsDir+prefix_mspps+'p2p_'+yyyymmdd+'_em_23v_lnd_ds.png'
  ChoppVec_png,minval,maxval,minval,maxval,mirs_em1_ds(tt),mspps_em1_ds(tt),10,10,'MIRS Retrieval','MSPPS','Des Emissivity at 23V Over Land ' +date + ' (V' + version +')',1,1,stats, img_name
endif


tt=where( mirs_em1_as ge 0 and mspps_em1_as ge 0 and mirs_sfc_as eq 3 and mirs_qc_as eq 0, cnt )
if ( cnt gt 1 ) then begin
  img_name=figsDir+prefix_mspps+'p2p_'+yyyymmdd+'_em_23v_snw_as.png'
  ChoppVec_png,minval,maxval,minval,maxval,mirs_em1_as(tt),mspps_em1_as(tt),10,10,'MIRS Retrieval','MSPPS','Asc Emissivity at 23V Over Snow ' +date + ' (V' + version +')',1,1,stats, img_name
endif

if ( cnt gt 1 ) then begin
tt=where( mirs_em1_ds ge 0 and mspps_em1_ds ge 0 and mirs_sfc_ds eq 3 and mirs_qc_ds eq 0, cnt )
  img_name=figsDir+prefix_mspps+'p2p_'+yyyymmdd+'_em_23v_snw_ds.png'
  ChoppVec_png,minval,maxval,minval,maxval,mirs_em1_ds(tt),mspps_em1_ds(tt),10,10,'MIRS Retrieval','MSPPS','Des Emissivity at 23V Over Snow ' +date + ' (V' + version +')',1,1,stats, img_name
endif


;---- EM2
minval=0.65
maxval=1.00

tt=where( mirs_em2_as ge 0 and mspps_em2_as ge 0 and mirs_sfc_as eq 2 and mirs_qc_as eq 0, cnt )
if ( cnt gt 1 ) then begin
  img_name=figsDir+prefix_mspps+'p2p_'+yyyymmdd+'_em_31v_lnd_as.png'
  ChoppVec_png,minval,maxval,minval,maxval,mirs_em2_as(tt),mspps_em2_as(tt),10,10,'MIRS Retrieval','MSPPS','Asc Emissivity at 31V Over Land ' +date + ' (V' + version +')',1,1,stats, img_name
endif

tt=where( mirs_em2_ds ge 0 and mspps_em2_ds ge 0 and mirs_sfc_ds eq 2 and mirs_qc_ds eq 0, cnt )
if ( cnt gt 1 ) then begin
  img_name=figsDir+prefix_mspps+'p2p_'+yyyymmdd+'_em_31v_lnd_ds.png'
  ChoppVec_png,minval,maxval,minval,maxval,mirs_em2_ds(tt),mspps_em2_ds(tt),10,10,'MIRS Retrieval','MSPPS','Des Emissivity at 31V Over Land ' +date + ' (V' + version +')',1,1,stats, img_name
endif


tt=where( mirs_em2_as ge 0 and mspps_em2_as ge 0 and mirs_sfc_as eq 3 and mirs_qc_as eq 0, cnt )
if ( cnt gt 1 ) then begin
  img_name=figsDir+prefix_mspps+'p2p_'+yyyymmdd+'_em_31v_snw_as.png'
  ChoppVec_png,minval,maxval,minval,maxval,mirs_em2_as(tt),mspps_em2_as(tt),10,10,'MIRS Retrieval','MSPPS','Asc Emissivity at 31V Over Snow ' +date + ' (V' + version +')',1,1,stats, img_name
endif

tt=where( mirs_em2_ds ge 0 and mspps_em2_ds ge 0 and mirs_sfc_ds eq 3 and mirs_qc_ds eq 0, cnt )
if ( cnt gt 1 ) then begin
  img_name=figsDir+prefix_mspps+'p2p_'+yyyymmdd+'_em_31v_snw_ds.png'
  ChoppVec_png,minval,maxval,minval,maxval,mirs_em2_ds(tt),mspps_em2_ds(tt),10,10,'MIRS Retrieval','MSPPS','Des Emissivity at 31V Over Snow ' +date + ' (V' + version +')',1,1,stats, img_name
endif


;---- EM3
minval=0.65
maxval=1.00

tt=where( mirs_em3_as ge 0 and mspps_em3_as ge 0 and mirs_sfc_as eq 2 and mirs_qc_as eq 0, cnt )
if ( cnt gt 1 ) then begin
  img_name=figsDir+prefix_mspps+'p2p_'+yyyymmdd+'_em_50v_lnd_as.png'
  ChoppVec_png,minval,maxval,minval,maxval,mirs_em3_as(tt),mspps_em3_as(tt),10,10,'MIRS Retrieval','MSPPS','Asc Emissivity at 50V Over Land ' +date + ' (V' + version +')',1,1,stats, img_name
endif

tt=where( mirs_em3_ds ge 0 and mspps_em3_ds ge 0 and mirs_sfc_ds eq 2 and mirs_qc_ds eq 0, cnt )
if ( cnt gt 1 ) then begin
  img_name=figsDir+prefix_mspps+'p2p_'+yyyymmdd+'_em_50v_lnd_ds.png'
  ChoppVec_png,minval,maxval,minval,maxval,mirs_em3_ds(tt),mspps_em3_ds(tt),10,10,'MIRS Retrieval','MSPPS','Des Emissivity at 50V Over Land ' +date + ' (V' + version +')',1,1,stats, img_name
endif


tt=where( mirs_em3_as ge 0 and mspps_em3_as ge 0 and mirs_sfc_as eq 3 and mirs_qc_as eq 0, cnt )
if ( cnt gt 1 ) then begin
  img_name=figsDir+prefix_mspps+'p2p_'+yyyymmdd+'_em_50v_snw_as.png'
  ChoppVec_png,minval,maxval,minval,maxval,mirs_em3_as(tt),mspps_em3_as(tt),10,10,'MIRS Retrieval','MSPPS','Asc Emissivity at 50V Over Snow ' +date + ' (V' + version +')',1,1,stats, img_name
endif

tt=where( mirs_em3_ds ge 0 and mspps_em3_ds ge 0 and mirs_sfc_ds eq 3 and mirs_qc_ds eq 0, cnt )
if ( cnt gt 1 ) then begin
  img_name=figsDir+prefix_mspps+'p2p_'+yyyymmdd+'_em_50v_snw_ds.png'
  ChoppVec_png,minval,maxval,minval,maxval,mirs_em3_ds(tt),mspps_em3_ds(tt),10,10,'MIRS Retrieval','MSPPS','Des Emissivity at 50V Over Snow ' +date + ' (V' + version +')',1,1,stats, img_name
endif


;---- LST
minval=200
maxval=325

tt=where( mirs_tskin_as ge 0 and mspps_tskin_as ge 0 and mirs_sfc_as eq 2 and mirs_qc_as eq 0, cnt )
if ( cnt gt 1 ) then begin
  img_name=figsDir+prefix_mspps+'p2p_'+yyyymmdd+'_tskin_lnd_as.png'
  ChoppVec_png,minval,maxval,minval,maxval,mirs_tskin_as(tt),mspps_tskin_as(tt),10,10,'MIRS Skin Temperature','MSPPS Surface Temperature','Asc Skin/Sfc Temperature Over Land ' +date + ' (V' + version +')',1,1,stats, img_name
endif

tt=where( mirs_tskin_ds ge 0 and mspps_tskin_ds ge 0 and mirs_sfc_ds eq 2 and mirs_qc_ds eq 0, cnt )
if ( cnt gt 1 ) then begin
  img_name=figsDir+prefix_mspps+'p2p_'+yyyymmdd+'_tskin_lnd_ds.png'
  ChoppVec_png,minval,maxval,minval,maxval,mirs_tskin_ds(tt),mspps_tskin_ds(tt),10,10,'MIRS Skin Temperature','MSPPS Surface Temperature','Des Skin/Sfc Temperature Over Land ' +date + ' (V' + version +')',1,1,stats, img_name
endif


tt=where( mirs_tskin_as ge 0 and mspps_tskin_as ge 0 and mirs_sfc_as eq 3 and mirs_qc_as eq 0, cnt )
if ( cnt gt 1 ) then begin
  img_name=figsDir+prefix_mspps+'p2p_'+yyyymmdd+'_tskin_snw_as.png'
  ChoppVec_png,minval,maxval,minval,maxval,mirs_tskin_as(tt),mspps_tskin_as(tt),10,10,'MIRS Skin Temperature','MSPPS Surface Temperature','Asc Skin/Sfc Temperature Over Snow ' +date + ' (V' + version +')',1,1,stats, img_name
endif

tt=where( mirs_tskin_ds ge 0 and mspps_tskin_ds ge 0 and mirs_sfc_ds eq 3 and mirs_qc_ds eq 0, cnt )
if ( cnt gt 1 ) then begin
  img_name=figsDir+prefix_mspps+'p2p_'+yyyymmdd+'_tskin_snw_ds.png'
  ChoppVec_png,minval,maxval,minval,maxval,mirs_tskin_ds(tt),mspps_tskin_ds(tt),10,10,'MIRS Skin Temperature','MSPPS Surface Temperature','Des Skin/Sfc Temperature Over Snow ' +date + ' (V' + version +')',1,1,stats, img_name
endif




;******************************************************************************************************************************
;
;    Bias asymmetry Section
;
;******************************************************************************************************************************
NUM_BIN=30
BIN_BOX=4*findgen(NUM_BIN+1)-60


angle_mirs_CLW_as_sea=fltarr(NUM_BIN)
angle_num_CLW_as_sea=fltarr(NUM_BIN)  & angle_num_CLW_as_sea(*)=0.0

angle_mirs_CLW_ds_sea=fltarr(NUM_BIN)
angle_num_CLW_ds_sea=fltarr(NUM_BIN)  & angle_num_CLW_ds_sea(*)=0.0

angle_mirs_LWP_as_sea=fltarr(NUM_BIN)
angle_num_LWP_as_sea=fltarr(NUM_BIN)  & angle_num_LWP_as_sea(*)=0.0

angle_mirs_LWP_ds_sea=fltarr(NUM_BIN)
angle_num_LWP_ds_sea=fltarr(NUM_BIN)  & angle_num_LWP_ds_sea(*)=0.0


angle_mirs_SIC_as_sea=fltarr(NUM_BIN)
angle_num_SIC_as_sea=fltarr(NUM_BIN)  & angle_num_SIC_as_sea(*)=0.0

angle_mirs_SIC_ds_sea=fltarr(NUM_BIN)
angle_num_SIC_ds_sea=fltarr(NUM_BIN)  & angle_num_SIC_ds_sea(*)=0.0


angle_mirs_SWE_as_lnd=fltarr(NUM_BIN)
angle_num_SWE_as_lnd=fltarr(NUM_BIN)  & angle_num_SWE_as_lnd(*)=0.0

angle_mirs_SWE_ds_lnd=fltarr(NUM_BIN)
angle_num_SWE_ds_lnd=fltarr(NUM_BIN)  & angle_num_SWE_ds_lnd(*)=0.0


angle_mirs_EM1_as_lnd=fltarr(NUM_BIN)
angle_num_EM1_as_lnd=fltarr(NUM_BIN)  & angle_num_EM1_as_lnd(*)=0.0

angle_mirs_EM1_ds_lnd=fltarr(NUM_BIN)
angle_num_EM1_ds_lnd=fltarr(NUM_BIN)  & angle_num_EM1_ds_lnd(*)=0.0


angle_mirs_EM2_as_lnd=fltarr(NUM_BIN)
angle_num_EM2_as_lnd=fltarr(NUM_BIN)  & angle_num_EM2_as_lnd(*)=0.0

angle_mirs_EM2_ds_lnd=fltarr(NUM_BIN)
angle_num_EM2_ds_lnd=fltarr(NUM_BIN)  & angle_num_EM2_ds_lnd(*)=0.0


angle_mirs_EM3_as_lnd=fltarr(NUM_BIN)
angle_num_EM3_as_lnd=fltarr(NUM_BIN)  & angle_num_EM3_as_lnd(*)=0.0

angle_mirs_EM3_ds_lnd=fltarr(NUM_BIN)
angle_num_EM3_ds_lnd=fltarr(NUM_BIN)  & angle_num_EM3_ds_lnd(*)=0.0


angle_mirs_TSKIN_as_lnd=fltarr(NUM_BIN)
angle_num_TSKIN_as_lnd=fltarr(NUM_BIN)  & angle_num_TSKIN_as_lnd(*)=0.0

angle_mirs_TSKIN_ds_lnd=fltarr(NUM_BIN)
angle_num_TSKIN_ds_lnd=fltarr(NUM_BIN)  & angle_num_TSKIN_ds_lnd(*)=0.0


angle_mirs_IWP_as_lnd=fltarr(NUM_BIN)
angle_num_IWP_as_lnd=fltarr(NUM_BIN)  & angle_num_IWP_as_lnd(*)=0.0

angle_mirs_IWP_ds_lnd=fltarr(NUM_BIN)
angle_num_IWP_ds_lnd=fltarr(NUM_BIN)  & angle_num_IWP_ds_lnd(*)=0.0

angle_mirs_IWP_as_sea=fltarr(NUM_BIN)
angle_num_IWP_as_sea=fltarr(NUM_BIN)  & angle_num_IWP_as_sea(*)=0.0

angle_mirs_IWP_ds_sea=fltarr(NUM_BIN)
angle_num_IWP_ds_sea=fltarr(NUM_BIN)  & angle_num_IWP_ds_sea(*)=0.0


angle_mirs_RR_as_lnd=fltarr(NUM_BIN)
angle_num_RR_as_lnd=fltarr(NUM_BIN)  & angle_num_RR_as_lnd(*)=0.0

angle_mirs_RR_ds_lnd=fltarr(NUM_BIN)
angle_num_RR_ds_lnd=fltarr(NUM_BIN)  & angle_num_RR_ds_lnd(*)=0.0

angle_mirs_RR_as_sea=fltarr(NUM_BIN)
angle_num_RR_as_sea=fltarr(NUM_BIN)  & angle_num_RR_as_sea(*)=0.0

angle_mirs_RR_ds_sea=fltarr(NUM_BIN)
angle_num_RR_ds_sea=fltarr(NUM_BIN)  & angle_num_RR_ds_sea(*)=0.0

angle_mirs_RR_as_all=fltarr(NUM_BIN)
angle_num_RR_as_all=fltarr(NUM_BIN)  & angle_num_RR_as_all(*)=0.0

angle_mirs_RR_ds_all=fltarr(NUM_BIN)
angle_num_RR_ds_all=fltarr(NUM_BIN)  & angle_num_RR_ds_all(*)=0.0


for irow=0, nROW-1 do begin
for icol=0, nCOL-1 do begin

   i=grid_angle_mirs_as(icol,irow)
   j=grid_angle_mirs_ds(icol,irow)
   
   for ibin=0, NUM_BIN-1 do begin
   
      if ( i ge BIN_BOX(ibin) and  i lt BIN_BOX(ibin+1) and diff_grid_clw_as(icol,irow)  ne -999.0  and  sfcMask(icol,irow) eq 0 ) then begin
	angle_mirs_CLW_as_sea(ibin) = angle_mirs_CLW_as_sea(ibin) + diff_grid_clw_as(icol,irow)
	angle_num_CLW_as_sea(ibin)  = angle_num_CLW_as_sea(ibin)  + 1.0
      endif 
      if ( j ge BIN_BOX(ibin) and  j lt BIN_BOX(ibin+1) and diff_grid_clw_ds(icol,irow)  ne -999.0  and  sfcMask(icol,irow) eq 0 ) then begin
	angle_mirs_CLW_ds_sea(ibin) = angle_mirs_CLW_ds_sea(ibin) + diff_grid_clw_ds(icol,irow)
	angle_num_CLW_ds_sea(ibin)  = angle_num_CLW_ds_sea(ibin)  + 1.0
    endif

      if ( i ge BIN_BOX(ibin) and  i lt BIN_BOX(ibin+1) and diff_grid_lwp_as(icol,irow)  ne -999.0  and  sfcMask(icol,irow) eq 0 ) then begin
	angle_mirs_LWP_as_sea(ibin) = angle_mirs_LWP_as_sea(ibin) + diff_grid_lwp_as(icol,irow)
	angle_num_LWP_as_sea(ibin)  = angle_num_LWP_as_sea(ibin)  + 1.0
      endif 
      if ( j ge BIN_BOX(ibin) and  j lt BIN_BOX(ibin+1) and diff_grid_lwp_ds(icol,irow)  ne -999.0  and  sfcMask(icol,irow) eq 0 ) then begin
	angle_mirs_LWP_ds_sea(ibin) = angle_mirs_LWP_ds_sea(ibin) + diff_grid_lwp_ds(icol,irow)
	angle_num_LWP_ds_sea(ibin)  = angle_num_LWP_ds_sea(ibin)  + 1.0
      endif
      
      if ( i ge BIN_BOX(ibin) and  i lt BIN_BOX(ibin+1) and diff_grid_sic_as(icol,irow)  ne -999.0  and  sfcMask(icol,irow) eq 0 ) then begin
	angle_mirs_sic_as_sea(ibin) = angle_mirs_sic_as_sea(ibin) + diff_grid_sic_as(icol,irow)
	angle_num_sic_as_sea(ibin)  = angle_num_sic_as_sea(ibin)  + 1.0
      endif 
      if ( j ge BIN_BOX(ibin) and  j lt BIN_BOX(ibin+1) and diff_grid_sic_ds(icol,irow)  ne -999.0  and  sfcMask(icol,irow) eq 0 ) then begin
	angle_mirs_sic_ds_sea(ibin) = angle_mirs_sic_ds_sea(ibin) + diff_grid_sic_ds(icol,irow)
	angle_num_sic_ds_sea(ibin)  = angle_num_sic_ds_sea(ibin)  + 1.0
      endif
      
      if ( i ge BIN_BOX(ibin) and  i lt BIN_BOX(ibin+1) and diff_grid_swe_as(icol,irow)  ne -999.0  and  sfcMask(icol,irow) eq 1 ) then begin
	angle_mirs_swe_as_lnd(ibin) = angle_mirs_swe_as_lnd(ibin) + diff_grid_swe_as(icol,irow)
	angle_num_swe_as_lnd(ibin)  = angle_num_swe_as_lnd(ibin)  + 1.0
      endif 
      if ( j ge BIN_BOX(ibin) and  j lt BIN_BOX(ibin+1) and diff_grid_swe_ds(icol,irow)  ne -999.0  and  sfcMask(icol,irow) eq 1 ) then begin
	angle_mirs_swe_ds_lnd(ibin) = angle_mirs_swe_ds_lnd(ibin) + diff_grid_swe_ds(icol,irow)
	angle_num_swe_ds_lnd(ibin)  = angle_num_swe_ds_lnd(ibin)  + 1.0
      endif
      
      if ( i ge BIN_BOX(ibin) and  i lt BIN_BOX(ibin+1) and diff_grid_em1_as(icol,irow)  ne -999.0  and  sfcMask(icol,irow) eq 1 ) then begin
	angle_mirs_em1_as_lnd(ibin) = angle_mirs_em1_as_lnd(ibin) + diff_grid_em1_as(icol,irow)
	angle_num_em1_as_lnd(ibin)  = angle_num_em1_as_lnd(ibin)  + 1.0
      endif 
      if ( j ge BIN_BOX(ibin) and  j lt BIN_BOX(ibin+1) and diff_grid_em1_ds(icol,irow)  ne -999.0  and  sfcMask(icol,irow) eq 1 ) then begin
	angle_mirs_em1_ds_lnd(ibin) = angle_mirs_em1_ds_lnd(ibin) + diff_grid_em1_ds(icol,irow)
	angle_num_em1_ds_lnd(ibin)  = angle_num_em1_ds_lnd(ibin)  + 1.0
      endif

      if ( i ge BIN_BOX(ibin) and  i lt BIN_BOX(ibin+1) and diff_grid_em2_as(icol,irow)  ne -999.0  and  sfcMask(icol,irow) eq 1 ) then begin
	angle_mirs_em2_as_lnd(ibin) = angle_mirs_em2_as_lnd(ibin) + diff_grid_em2_as(icol,irow)
	angle_num_em2_as_lnd(ibin)  = angle_num_em2_as_lnd(ibin)  + 1.0
      endif 
      if ( j ge BIN_BOX(ibin) and  j lt BIN_BOX(ibin+1) and diff_grid_em2_ds(icol,irow)  ne -999.0  and  sfcMask(icol,irow) eq 1 ) then begin
	angle_mirs_em2_ds_lnd(ibin) = angle_mirs_em2_ds_lnd(ibin) + diff_grid_em2_ds(icol,irow)
	angle_num_em2_ds_lnd(ibin)  = angle_num_em2_ds_lnd(ibin)  + 1.0
      endif

      if ( i ge BIN_BOX(ibin) and  i lt BIN_BOX(ibin+1) and diff_grid_em3_as(icol,irow)  ne -999.0  and  sfcMask(icol,irow) eq 1 ) then begin
	angle_mirs_em3_as_lnd(ibin) = angle_mirs_em3_as_lnd(ibin) + diff_grid_em3_as(icol,irow)
	angle_num_em3_as_lnd(ibin)  = angle_num_em3_as_lnd(ibin)  + 1.0
      endif 
      if ( j ge BIN_BOX(ibin) and  j lt BIN_BOX(ibin+1) and diff_grid_em3_ds(icol,irow)  ne -999.0  and  sfcMask(icol,irow) eq 1 ) then begin
	angle_mirs_em3_ds_lnd(ibin) = angle_mirs_em3_ds_lnd(ibin) + diff_grid_em3_ds(icol,irow)
	angle_num_em3_ds_lnd(ibin)  = angle_num_em3_ds_lnd(ibin)  + 1.0
      endif

      ;---- MSPPS LST only valid over land ---- 
      if ( i ge BIN_BOX(ibin) and  i lt BIN_BOX(ibin+1) and FIX(diff_grid_tskin_as(icol,irow))  ne -999  and  grid_sfc2_mirs_as(icol,irow) eq 2 ) then begin
	angle_mirs_tskin_as_lnd(ibin) = angle_mirs_tskin_as_lnd(ibin) + diff_grid_tskin_as(icol,irow)
	angle_num_tskin_as_lnd(ibin)  = angle_num_tskin_as_lnd(ibin)  + 1.0
      endif 
      if ( j ge BIN_BOX(ibin) and  j lt BIN_BOX(ibin+1) and FIX(diff_grid_tskin_ds(icol,irow))  ne -999  and  grid_sfc2_mirs_ds(icol,irow) eq 2 ) then begin
	angle_mirs_tskin_ds_lnd(ibin) = angle_mirs_tskin_ds_lnd(ibin) + diff_grid_tskin_ds(icol,irow)
	angle_num_tskin_ds_lnd(ibin)  = angle_num_tskin_ds_lnd(ibin)  + 1.0
      endif

      
      if ( i ge BIN_BOX(ibin) and  i lt BIN_BOX(ibin+1) and diff_grid_iwp_as(icol,irow)  ne -999.0  and  sfcMask(icol,irow) eq 1 ) then begin
        angle_mirs_iwp_as_lnd(ibin) = angle_mirs_iwp_as_lnd(ibin) + diff_grid_iwp_as(icol,irow)
        angle_num_iwp_as_lnd(ibin)  = angle_num_iwp_as_lnd(ibin)  + 1.0
      endif
      if ( j ge BIN_BOX(ibin) and  j lt BIN_BOX(ibin+1) and diff_grid_iwp_ds(icol,irow)  ne -999.0  and  sfcMask(icol,irow) eq 1 ) then begin
        angle_mirs_iwp_ds_lnd(ibin) = angle_mirs_iwp_ds_lnd(ibin) + diff_grid_iwp_ds(icol,irow)
        angle_num_iwp_ds_lnd(ibin)  = angle_num_iwp_ds_lnd(ibin)  + 1.0
      endif
      
      if ( i ge BIN_BOX(ibin) and  i lt BIN_BOX(ibin+1) and diff_grid_iwp_as(icol,irow)  ne -999.0  and  sfcMask(icol,irow) eq 1 ) then begin
        angle_mirs_iwp_as_sea(ibin) = angle_mirs_iwp_as_sea(ibin) + diff_grid_iwp_as(icol,irow)
        angle_num_iwp_as_sea(ibin)  = angle_num_iwp_as_sea(ibin)  + 1.0
      endif
      if ( j ge BIN_BOX(ibin) and  j lt BIN_BOX(ibin+1) and diff_grid_iwp_ds(icol,irow)  ne -999.0  and  sfcMask(icol,irow) eq 1 ) then begin
        angle_mirs_iwp_ds_sea(ibin) = angle_mirs_iwp_ds_sea(ibin) + diff_grid_iwp_ds(icol,irow)
        angle_num_iwp_ds_sea(ibin)  = angle_num_iwp_ds_sea(ibin)  + 1.0
      endif
 
      if ( i ge BIN_BOX(ibin) and  i lt BIN_BOX(ibin+1) and diff_grid_rr_as(icol,irow)  ne -999.0  and  sfcMask(icol,irow) eq 1 ) then begin
        angle_mirs_rr_as_lnd(ibin) = angle_mirs_rr_as_lnd(ibin) + diff_grid_rr_as(icol,irow)
        angle_num_rr_as_lnd(ibin)  = angle_num_rr_as_lnd(ibin)  + 1.0
      endif
      if ( j ge BIN_BOX(ibin) and  j lt BIN_BOX(ibin+1) and diff_grid_rr_ds(icol,irow)  ne -999.0  and  sfcMask(icol,irow) eq 1 ) then begin
        angle_mirs_rr_ds_lnd(ibin) = angle_mirs_rr_ds_lnd(ibin) + diff_grid_rr_ds(icol,irow)
        angle_num_rr_ds_lnd(ibin)  = angle_num_rr_ds_lnd(ibin)  + 1.0
      endif
 
      if ( i ge BIN_BOX(ibin) and  i lt BIN_BOX(ibin+1) and diff_grid_rr_as(icol,irow)  ne -999.0  and  sfcMask(icol,irow) eq 1 ) then begin
        angle_mirs_rr_as_sea(ibin) = angle_mirs_rr_as_sea(ibin) + diff_grid_rr_as(icol,irow)
        angle_num_rr_as_sea(ibin)  = angle_num_rr_as_sea(ibin)  + 1.0
      endif
      if ( j ge BIN_BOX(ibin) and  j lt BIN_BOX(ibin+1) and diff_grid_rr_ds(icol,irow)  ne -999.0  and  sfcMask(icol,irow) eq 1 ) then begin
        angle_mirs_rr_ds_sea(ibin) = angle_mirs_rr_ds_sea(ibin) + diff_grid_rr_ds(icol,irow)
        angle_num_rr_ds_sea(ibin)  = angle_num_rr_ds_sea(ibin)  + 1.0
      endif
 
      if ( i ge BIN_BOX(ibin) and  i lt BIN_BOX(ibin+1) and diff_grid_rr_as(icol,irow)  ne -999.0  and  sfcMask(icol,irow) ge 0 ) then begin
        angle_mirs_rr_as_all(ibin) = angle_mirs_rr_as_all(ibin) + diff_grid_rr_as(icol,irow)
        angle_num_rr_as_all(ibin)  = angle_num_rr_as_all(ibin)  + 1.0
      endif
      if ( j ge BIN_BOX(ibin) and  j lt BIN_BOX(ibin+1) and diff_grid_rr_ds(icol,irow)  ne -999.0  and  sfcMask(icol,irow) ge 0 ) then begin
        angle_mirs_rr_ds_all(ibin) = angle_mirs_rr_ds_all(ibin) + diff_grid_rr_ds(icol,irow)
        angle_num_rr_ds_all(ibin)  = angle_num_rr_ds_all(ibin)  + 1.0
      endif
 
  endfor
    
endfor
endfor


for i=0, NUM_BIN-1 do begin

  if angle_num_CLW_as_sea(i) gt 0 then angle_mirs_CLW_as_sea(i) = angle_mirs_CLW_as_sea(i) / angle_num_CLW_as_sea(i)
  if angle_num_CLW_ds_sea(i) gt 0 then angle_mirs_CLW_ds_sea(i) = angle_mirs_CLW_ds_sea(i) / angle_num_CLW_ds_sea(i)

  if angle_num_LWP_as_sea(i) gt 0 then angle_mirs_LWP_as_sea(i) = angle_mirs_LWP_as_sea(i) / angle_num_LWP_as_sea(i)
  if angle_num_LWP_ds_sea(i) gt 0 then angle_mirs_LWP_ds_sea(i) = angle_mirs_LWP_ds_sea(i) / angle_num_LWP_ds_sea(i)
  
  if angle_num_SIC_as_sea(i) gt 0 then angle_mirs_SIC_as_sea(i) = angle_mirs_SIC_as_sea(i) / angle_num_SIC_as_sea(i)
  if angle_num_SIC_ds_sea(i) gt 0 then angle_mirs_SIC_ds_sea(i) = angle_mirs_SIC_ds_sea(i) / angle_num_SIC_ds_sea(i)
  
  if angle_num_SWE_as_lnd(i) gt 0 then angle_mirs_SWE_as_lnd(i) = angle_mirs_SWE_as_lnd(i) / angle_num_SWE_as_lnd(i)
  if angle_num_SWE_ds_lnd(i) gt 0 then angle_mirs_SWE_ds_lnd(i) = angle_mirs_SWE_ds_lnd(i) / angle_num_SWE_ds_lnd(i)
  
  if angle_num_EM1_as_lnd(i) gt 0 then angle_mirs_EM1_as_lnd(i) = angle_mirs_EM1_as_lnd(i) / angle_num_EM1_as_lnd(i)
  if angle_num_EM1_ds_lnd(i) gt 0 then angle_mirs_EM1_ds_lnd(i) = angle_mirs_EM1_ds_lnd(i) / angle_num_EM1_ds_lnd(i)
  
  if angle_num_EM2_as_lnd(i) gt 0 then angle_mirs_EM2_as_lnd(i) = angle_mirs_EM2_as_lnd(i) / angle_num_EM2_as_lnd(i)
  if angle_num_EM2_ds_lnd(i) gt 0 then angle_mirs_EM2_ds_lnd(i) = angle_mirs_EM2_ds_lnd(i) / angle_num_EM2_ds_lnd(i)
  
  if angle_num_EM3_as_lnd(i) gt 0 then angle_mirs_EM3_as_lnd(i) = angle_mirs_EM3_as_lnd(i) / angle_num_EM3_as_lnd(i)
  if angle_num_EM3_ds_lnd(i) gt 0 then angle_mirs_EM3_ds_lnd(i) = angle_mirs_EM3_ds_lnd(i) / angle_num_EM3_ds_lnd(i)
  
  if angle_num_TSKIN_as_lnd(i) gt 0 then angle_mirs_TSKIN_as_lnd(i) = angle_mirs_TSKIN_as_lnd(i) / angle_num_TSKIN_as_lnd(i)
  if angle_num_TSKIN_ds_lnd(i) gt 0 then angle_mirs_TSKIN_ds_lnd(i) = angle_mirs_TSKIN_ds_lnd(i) / angle_num_TSKIN_ds_lnd(i)
  
  if angle_num_iwp_as_lnd(i) gt 0 then angle_mirs_iwp_as_lnd(i) = angle_mirs_iwp_as_lnd(i) / angle_num_iwp_as_lnd(i)
  if angle_num_iwp_ds_lnd(i) gt 0 then angle_mirs_iwp_ds_lnd(i) = angle_mirs_iwp_ds_lnd(i) / angle_num_iwp_ds_lnd(i)

  if angle_num_iwp_as_sea(i) gt 0 then angle_mirs_iwp_as_sea(i) = angle_mirs_iwp_as_sea(i) / angle_num_iwp_as_sea(i)
  if angle_num_iwp_ds_sea(i) gt 0 then angle_mirs_iwp_ds_sea(i) = angle_mirs_iwp_ds_sea(i) / angle_num_iwp_ds_sea(i)

  if angle_num_rr_as_lnd(i) gt 0 then angle_mirs_rr_as_lnd(i) = angle_mirs_rr_as_lnd(i) / angle_num_rr_as_lnd(i)
  if angle_num_rr_ds_lnd(i) gt 0 then angle_mirs_rr_ds_lnd(i) = angle_mirs_rr_ds_lnd(i) / angle_num_rr_ds_lnd(i)

  if angle_num_rr_as_sea(i) gt 0 then angle_mirs_rr_as_sea(i) = angle_mirs_rr_as_sea(i) / angle_num_rr_as_sea(i)
  if angle_num_rr_ds_sea(i) gt 0 then angle_mirs_rr_ds_sea(i) = angle_mirs_rr_ds_sea(i) / angle_num_rr_ds_sea(i)

  if angle_num_rr_as_all(i) gt 0 then angle_mirs_rr_as_all(i) = angle_mirs_rr_as_all(i) / angle_num_rr_as_all(i)
  if angle_num_rr_ds_all(i) gt 0 then angle_mirs_rr_ds_all(i) = angle_mirs_rr_ds_all(i) / angle_num_rr_ds_all(i)

endfor


prefix_asym="mirs_adv_poes_"+satId+"_mspps_diffasym_glb_"

;----	 plot clw angle asymmetry
xrange=[-60,60]
yrange=[-0.1,0.1]
xtitle='Local Zenith Angle (degree)'

lndsea='sea'
ytitle='CLW Bias (mm) Over Ocean'

title='MIRS Retrieval - MSPPS ' + date + ' Asc' + ' (V' + version +')'
map_name=figsDir+prefix_asym+yyyymmdd+'_clw_'+lndsea+'_as.png'
plot_line, bin_box, angle_mirs_CLW_as_sea, xtitle, ytitle, title, xrange, yrange, map_name

title='MIRS Retrieval - MSPPS ' + date + ' Des' + ' (V' + version +')'
map_name=figsDir+prefix_asym+yyyymmdd+'_clw_'+lndsea+'_ds.png'
plot_line, bin_box, angle_mirs_CLW_ds_sea, xtitle, ytitle, title, xrange, yrange, map_name


;----	 plot lwp angle asymmetry
xrange=[-60,60]
yrange=[-0.1,0.1]
xtitle='Local Zenith Angle (degree)'

lndsea='sea'
ytitle='LWP Bias (mm) Over Ocean'

title='MIRS Retrieval LWP - MSPPS CLW ' + date + ' Asc' + ' (V' + version +')'
map_name=figsDir+prefix_asym+yyyymmdd+'_lwp_'+lndsea+'_as.png'
plot_line, bin_box, angle_mirs_LWP_as_sea, xtitle, ytitle, title, xrange, yrange, map_name

title='MIRS Retrieval LWP - MSPPS CLW ' + date + ' Des' + ' (V' + version +')'
map_name=figsDir+prefix_asym+yyyymmdd+'_lwp_'+lndsea+'_ds.png'
plot_line, bin_box, angle_mirs_LWP_ds_sea, xtitle, ytitle, title, xrange, yrange, map_name

;----	 plot sic angle asymmetry
xrange=[-60,60]
yrange=[-10,10]
xtitle='Local Zenith Angle (degree)'

lndsea='sea'
ytitle='Sea Ice Concentration Bias (%)'

title='MIRS Retrieval - MSPPS ' + date + ' Asc' + ' (V' + version +')'
map_name=figsDir+prefix_asym+yyyymmdd+'_sice_'+lndsea+'_as.png'
plot_line, bin_box, angle_mirs_SIC_as_sea, xtitle, ytitle, title, xrange, yrange, map_name

title='MIRS Retrieval - MSPPS ' + date + ' Des' + ' (V' + version +')'
map_name=figsDir+prefix_asym+yyyymmdd+'_sice_'+lndsea+'_ds.png'
plot_line, bin_box, angle_mirs_SIC_ds_sea, xtitle, ytitle, title, xrange, yrange, map_name


;----	 plot swe angle asymmetry
xrange=[-60,60]
yrange=[-4,4]
xtitle='Local Zenith Angle (degree)'

lndsea='lnd'
ytitle='Snow Water Equivalent Bias (cm)'

title='MIRS Retrieval - MSPPS ' + date + ' Asc' + ' (V' + version +')'
map_name=figsDir+prefix_asym+yyyymmdd+'_swe_'+lndsea+'_as.png'
plot_line, bin_box, angle_mirs_SWE_as_lnd, xtitle, ytitle, title, xrange, yrange, map_name

title='MIRS Retrieval - MSPPS ' + date + ' Des' + ' (V' + version +')'
map_name=figsDir+prefix_asym+yyyymmdd+'_swe_'+lndsea+'_ds.png'
plot_line, bin_box, angle_mirs_SWE_ds_lnd, xtitle, ytitle, title, xrange, yrange, map_name


;----  plot em1 angle asymmetry
xrange=[-60,60]
yrange=[-0.1,0.1]
xtitle='Local Zenith Angle (degree)'

lndsea='lnd'
ytitle='Emissivity at 23V Over Land'

title='MIRS Retrieval - MSPPS ' + date + ' Asc' + ' (V' + version +')'
map_name=figsDir+prefix_asym+yyyymmdd+'_em_23v_'+lndsea+'_as.png'
plot_line, bin_box, angle_mirs_EM1_as_lnd, xtitle, ytitle, title, xrange, yrange, map_name

title='MIRS Retrieval - MSPPS ' + date + ' Des' + ' (V' + version +')'
map_name=figsDir+prefix_asym+yyyymmdd+'_em_23v_'+lndsea+'_ds.png'
plot_line, bin_box, angle_mirs_EM1_ds_lnd, xtitle, ytitle, title, xrange, yrange, map_name


;----  plot em2 angle asymmetry
xrange=[-60,60]
yrange=[-0.1,0.1]
xtitle='Local Zenith Angle (degree)'

lndsea='lnd'
ytitle='Emissivity at 31V Over Land'

title='MIRS Retrieval - MSPPS ' + date + ' Asc' + ' (V' + version +')'
map_name=figsDir+prefix_asym+yyyymmdd+'_em_31v_'+lndsea+'_as.png'
plot_line, bin_box, angle_mirs_EM2_as_lnd, xtitle, ytitle, title, xrange, yrange, map_name

title='MIRS Retrieval - MSPPS ' + date + ' Des' + ' (V' + version +')'
map_name=figsDir+prefix_asym+yyyymmdd+'_em_31v_'+lndsea+'_ds.png'
plot_line, bin_box, angle_mirs_EM2_ds_lnd, xtitle, ytitle, title, xrange, yrange, map_name


;----  plot em3 angle asymmetry
xrange=[-60,60]
yrange=[-0.1,0.1]
xtitle='Local Zenith Angle (degree)'

lndsea='lnd'
ytitle='Emissivity at 50V Over Land'

title='MIRS Retrieval - MSPPS ' + date + ' Asc' + ' (V' + version +')'
map_name=figsDir+prefix_asym+yyyymmdd+'_em_50v_'+lndsea+'_as.png'
plot_line, bin_box, angle_mirs_EM3_as_lnd, xtitle, ytitle, title, xrange, yrange, map_name

title='MIRS Retrieval - MSPPS ' + date + ' Des' + ' (V' + version +')'
map_name=figsDir+prefix_asym+yyyymmdd+'_em_50v_'+lndsea+'_ds.png'
plot_line, bin_box, angle_mirs_EM3_ds_lnd, xtitle, ytitle, title, xrange, yrange, map_name


;----  plot LST angle asymmetry
xrange=[-60,60]
yrange=[-10,10]
xtitle='Local Zenith Angle (degree)'

lndsea='lnd'
ytitle='Skin Temperature Over Land'

title='MIRS Retrieval - MSPPS ' + date + ' Asc' + ' (V' + version +')'
map_name=figsDir+prefix_asym+yyyymmdd+'_tskin_'+lndsea+'_as.png'
plot_line, bin_box, angle_mirs_TSKIN_as_lnd, xtitle, ytitle, title, xrange, yrange, map_name

title='MIRS Retrieval - MSPPS ' + date + ' Des' + ' (V' + version +')'
map_name=figsDir+prefix_asym+yyyymmdd+'_tskin_'+lndsea+'_ds.png'
plot_line, bin_box, angle_mirs_TSKIN_ds_lnd, xtitle, ytitle, title, xrange, yrange, map_name


;----  plot IWP angle asymmetry
xrange=[-60,60]
yrange=[-0.1,0.1]
xtitle='Local Zenith Angle (degree)'

lndsea='lnd'
ytitle='IWP over Land'

title='MIRS GWP - MSPPS IWP' + date + ' Asc' + ' (V' + version +')'
map_name=figsDir+prefix_asym+yyyymmdd+'_iwp_'+lndsea+'_as.png'
plot_line, bin_box, angle_mirs_iwp_as_lnd, xtitle, ytitle, title, xrange, yrange, map_name

title='MIRS GWP - MSPPS IWP' + date + ' Des' + ' (V' + version +')'
map_name=figsDir+prefix_asym+yyyymmdd+'_iwp_'+lndsea+'_ds.png'
plot_line, bin_box, angle_mirs_iwp_ds_lnd, xtitle, ytitle, title, xrange, yrange, map_name

lndsea='sea'
ytitle='IWP over Ocean'

title='MIRS GWP - MSPPS IWP' + date + ' Asc' + ' (V' + version +')'
map_name=figsDir+prefix_asym+yyyymmdd+'_iwp_'+lndsea+'_as.png'
plot_line, bin_box, angle_mirs_iwp_as_sea, xtitle, ytitle, title, xrange, yrange, map_name

title='MIRS GWP - MSPPS IWP' + date + ' Des' + ' (V' + version +')'
map_name=figsDir+prefix_asym+yyyymmdd+'_iwp_'+lndsea+'_ds.png'
plot_line, bin_box, angle_mirs_iwp_ds_sea, xtitle, ytitle, title, xrange, yrange, map_name


;----  plot RR angle asymmetry
xrange=[-60,60]
yrange=[-2.5,2.5]
xtitle='Local Zenith Angle (degree)'

lndsea='lnd'
ytitle='RR over Land'

title='MIRS - MSPPS RR ' + date + ' Asc' + ' (V' + version +')'
map_name=figsDir+prefix_asym+yyyymmdd+'_rr_'+lndsea+'_as.png'
plot_line, bin_box, angle_mirs_rr_as_lnd, xtitle, ytitle, title, xrange, yrange, map_name

title='MIRS - MSPPS RR ' + date + ' Des' + ' (V' + version +')'
map_name=figsDir+prefix_asym+yyyymmdd+'_rr_'+lndsea+'_ds.png'
plot_line, bin_box, angle_mirs_rr_ds_lnd, xtitle, ytitle, title, xrange, yrange, map_name

lndsea='sea'
ytitle='RR over Ocean'

title='MIRS - MSPPS RR ' + date + ' Asc' + ' (V' + version +')'
map_name=figsDir+prefix_asym+yyyymmdd+'_rr_'+lndsea+'_as.png'
plot_line, bin_box, angle_mirs_rr_as_sea, xtitle, ytitle, title, xrange, yrange, map_name

title='MIRS - MSPPS RR ' + date + ' Des' + ' (V' + version +')'
map_name=figsDir+prefix_asym+yyyymmdd+'_rr_'+lndsea+'_ds.png'
plot_line, bin_box, angle_mirs_rr_ds_sea, xtitle, ytitle, title, xrange, yrange, map_name

lndsea='all'
ytitle='RR over Globe'

title='MIRS - MSPPS RR ' + date + ' Asc' + ' (V' + version +')'
map_name=figsDir+prefix_asym+yyyymmdd+'_rr_'+lndsea+'_as.png'
plot_line, bin_box, angle_mirs_rr_as_all, xtitle, ytitle, title, xrange, yrange, map_name

title='MIRS - MSPPS RR ' + date + ' Des' + ' (V' + version +')'
map_name=figsDir+prefix_asym+yyyymmdd+'_rr_'+lndsea+'_ds.png'
plot_line, bin_box, angle_mirs_rr_ds_all, xtitle, ytitle, title, xrange, yrange, map_name



IF ( PLOT_POLAR_SIC_MIRS EQ 1 ) THEN BEGIN

  sic_ps  =  reform(MIRS_SIC_AS(*,*), PROFMAX*FILEMAX)
  sfc_ps =  reform(MIRS_SFC_AS(*,*), PROFMAX*FILEMAX)
  xlat  =  reform(LAT_AS(*,*), PROFMAX*FILEMAX)
  xlon  =  reform(LON_AS(*,*), PROFMAX*FILEMAX)
  minvalue=0
  maxvalue=100
  div=10
  format='(I3)'
  sfcPick=0
  centrlat = 90.0
  orientlon = -80.0
  latmin_ps = 45.0
  test = xlat ge latmin_ps and xlat le latmax and xlon ge lonmin and xlon le lonmax
  ind = where(test)
  map_name = figsDir + prefix + yyyymmdd +'_sice_pn_sea_as.png'
  title = 'MIRS ' + strupcase(satId) + ' EDR Sea Ice Concentration (%) ' + date +  ' Asc' + ' (V' + version +')'
  if ( ind[0] ne -1 ) then begin
    plot_polar,sic_ps(ind),sfc_ps(ind),map_name,minvalue,maxvalue,latmin_ps,latmax,lonmin,lonmax,centrlat,orientlon,xlat(ind),xlon(ind),title, $
    sfcPick,div,format
  endif
  
  centrlat = -90.0
  orientlon = 0.0
  latmax_ps = -50.0
  test = xlat ge latmin and xlat le latmax_ps and xlon ge lonmin and xlon le lonmax
  ind = where(test)
  map_name = figsDir + prefix + yyyymmdd +'_sice_ps_sea_as.png'
  title = 'MIRS ' + strupcase(satId) + ' EDR Sea Ice Concentration (%) ' + date +  ' Asc' + ' (V' + version +')'
  if ( ind[0] ne -1 ) then begin
    plot_polar,sic_ps(ind),sfc_ps(ind),map_name,minvalue,maxvalue,latmin,latmax_ps,lonmin,lonmax,centrlat,orientlon,xlat(ind),xlon(ind),title, $
    sfcPick,div,format
  endif
  
  sic_ps  =  reform(MIRS_SIC_DS(*,*), PROFMAX*FILEMAX)
  sfc_ps =  reform(MIRS_SFC_DS(*,*), PROFMAX*FILEMAX)
  xlat  =  reform(LAT_DS(*,*), PROFMAX*FILEMAX)
  xlon  =  reform(LON_DS(*,*), PROFMAX*FILEMAX)
  minvalue=0
  maxvalue=100
  div=10
  format='(I3)'
  sfcPick=0
  centrlat = 90.0
  orientlon = -80.0
  latmin_ps = 50.0
  test = xlat ge latmin_ps and xlat le latmax and xlon ge lonmin and xlon le lonmax
  ind = where(test)
  map_name = figsDir + prefix + yyyymmdd +'_sice_pn_sea_ds.png'
  title = 'MIRS ' + strupcase(satId) + ' EDR Sea Ice Concentration (%) ' + date +  ' Des' + ' (V' + version +')'
  if ( ind[0] ne -1 ) then begin
    plot_polar,sic_ps(ind),sfc_ps(ind),map_name,minvalue,maxvalue,latmin_ps,latmax,lonmin,lonmax,centrlat,orientlon,xlat(ind),xlon(ind),title, $
    sfcPick,div,format
  endif

  centrlat = -90.0
  orientlon = 0.0
  latmax_ps = -50.0
  test = xlat ge latmin and xlat le latmax_ps and xlon ge lonmin and xlon le lonmax
  ind = where(test)
  map_name = figsDir + prefix + yyyymmdd +'_sice_ps_sea_ds.png'
  title = 'MIRS ' + strupcase(satId) + ' EDR Sea Ice Concentration (%) ' + date +  ' Des' + ' (V' + version +')'
  if ( ind[0] ne -1 ) then begin
    plot_polar,sic_ps(ind),sfc_ps(ind),map_name,minvalue,maxvalue,latmin,latmax_ps,lonmin,lonmax,centrlat,orientlon,xlat(ind),xlon(ind),title, $
    sfcPick,div,format
  endif

ENDIF

IF ( PLOT_POLAR_SWE_MIRS EQ 1 ) THEN BEGIN

  swe_ps  =  reform(MIRS_SWE_AS(*,*), PROFMAX*FILEMAX)
  sfc_ps =  reform(MIRS_SFC_AS(*,*), PROFMAX*FILEMAX)
  xlat  =  reform(LAT_AS(*,*), PROFMAX*FILEMAX)
  xlon  =  reform(LON_AS(*,*), PROFMAX*FILEMAX)

  minvalue=0
  maxvalue=8
  div=8
  format='(I2)'
  sfcPick=1
  centrlat = 90.0
  orientlon = -80.0
  latmin_ps = 0.0
  latmax_ps = 90.0
  test = xlat ge latmin_ps and xlat le latmax_ps and xlon ge lonmin and xlon le lonmax
  ind = where(test)
  map_name = figsDir + prefix + yyyymmdd +'_swe_pn_lnd_as.png'
  title = 'MIRS ' + strupcase(satId) + ' EDR Snow Water Equivalent (cm) ' + date +  ' Asc' + ' (V' + version +')'
  if ( ind[0] ne -1 ) then begin
    plot_polar,swe_ps(ind),sfc_ps(ind),map_name,minvalue,maxvalue,latmin_ps,latmax_ps,lonmin,lonmax,centrlat,orientlon,xlat(ind),xlon(ind),title, $
    sfcPick,div,format
  endif

  swe_ps  =  reform(MIRS_SWE_DS(*,*), PROFMAX*FILEMAX)
  sfc_ps =  reform(MIRS_SFC_DS(*,*), PROFMAX*FILEMAX)
  xlat  =  reform(LAT_DS(*,*), PROFMAX*FILEMAX)
  xlon  =  reform(LON_DS(*,*), PROFMAX*FILEMAX)
  minvalue=0
  maxvalue=8
  div=8
  format='(I2)'
  sfcPick=1
  centrlat = 90.0
  orientlon = -80.0
  latmin_ps = 0.0
  latmax_ps = 90.0
  test = xlat ge latmin_ps and xlat le latmax_ps and xlon ge lonmin and xlon le lonmax
  ind = where(test)
  map_name = figsDir + prefix + yyyymmdd +'_swe_pn_lnd_ds.png'
  title = 'MIRS ' + strupcase(satId) + ' EDR Snow Water Equivalent (cm) ' + date +  ' Des' + ' (V' + version +')'
  if ( ind[0] ne -1 ) then begin
    plot_polar,swe_ps(ind),sfc_ps(ind),map_name,minvalue,maxvalue,latmin_ps,latmax_ps,lonmin,lonmax,centrlat,orientlon,xlat(ind),xlon(ind),title, $
    sfcPick,div,format
  endif
ENDIF


IF ( PLOT_POLAR_SFC_MIRS EQ 1 ) THEN BEGIN

  sfc_ps =  reform(MIRS_SFC_AS(*,*), PROFMAX*FILEMAX)
  xlat  =  reform(LAT_AS(*,*), PROFMAX*FILEMAX)
  xlon  =  reform(LON_AS(*,*), PROFMAX*FILEMAX)
  minvalue=0
  maxvalue=3
  div=3
  format='(I2)'
  sfcPick=2
  centrlat = 90.0
  orientlon = -80.0
  latmin_ps = 0.0
  latmax_ps = 90.0
  test = xlat ge latmin_ps and xlat le latmax_ps and xlon ge lonmin and xlon le lonmax
  ind = where(test)
  map_name = figsDir + prefix + yyyymmdd +'_sfcTyp_pn_all_as.png'
  title = 'MIRS ' + strupcase(satId) + ' Pre-Process Surface Type ' + date +  ' Asc' + ' (V' + version +')'
  plot_polar,sfc_ps(ind),sfc_ps(ind),map_name,minvalue,maxvalue,latmin_ps,latmax_ps,lonmin,lonmax,centrlat,orientlon,xlat(ind),xlon(ind),title, $
    sfcPick,div,format

  centrlat = -90.0
  orientlon = 0.0
  latmin_ps = -90.0
  latmax_ps = 0.0
  test = xlat ge latmin_ps and xlat le latmax_ps and xlon ge lonmin and xlon le lonmax
  ind = where(test)
  map_name = figsDir + prefix + yyyymmdd +'_sfcTyp_ps_all_as.png'
  title = 'MIRS ' + strupcase(satId) + ' Pre-Process Surface Type ' + date +  ' Asc' + ' (V' + version +')'
  plot_polar,sfc_ps(ind),sfc_ps(ind),map_name,minvalue,maxvalue,latmin_ps,latmax_ps,lonmin,lonmax,centrlat,orientlon,xlat(ind),xlon(ind),title, $
    sfcPick,div,format


  sfc_ps =  reform(MIRS_SFC_DS(*,*), PROFMAX*FILEMAX)
  xlat  =  reform(LAT_DS(*,*), PROFMAX*FILEMAX)
  xlon  =  reform(LON_DS(*,*), PROFMAX*FILEMAX)
  minvalue=0
  maxvalue=3
  div=3
  format='(I2)'
  sfcPick=2
  centrlat = 90.0
  orientlon = -80.0
  latmin_ps = 0.0
  latmax_ps = 90.0
  test = xlat ge latmin_ps and xlat le latmax_ps and xlon ge lonmin and xlon le lonmax
  ind = where(test)
  map_name = figsDir + prefix + yyyymmdd +'_sfcTyp_pn_all_ds.png'
  title = 'MIRS ' + strupcase(satId) + ' Pre-Process Surface Type ' + date +  ' Des' + ' (V' + version +')'
  plot_polar,sfc_ps(ind),sfc_ps(ind),map_name,minvalue,maxvalue,latmin_ps,latmax_ps,lonmin,lonmax,centrlat,orientlon,xlat(ind),xlon(ind),title, $
    sfcPick,div,format


  centrlat = -90.0
  orientlon = 0.0
  latmin_ps = -90.0
  latmax_ps = 0.0
  test = xlat ge latmin_ps and xlat le latmax_ps and xlon ge lonmin and xlon le lonmax
  ind = where(test)
  map_name = figsDir + prefix + yyyymmdd +'_sfcTyp_ps_all_ds.png'
  title = 'MIRS ' + strupcase(satId) + ' Pre-Process Surface Type ' + date +  ' Des' + ' (V' + version +')'
  plot_polar,sfc_ps(ind),sfc_ps(ind),map_name,minvalue,maxvalue,latmin_ps,latmax_ps,lonmin,lonmax,centrlat,orientlon,xlat(ind),xlon(ind),title, $
    sfcPick,div,format


  sfc_ps =  reform(MIRS_SFC2_AS(*,*), PROFMAX*FILEMAX)
  xlat  =  reform(LAT_AS(*,*), PROFMAX*FILEMAX)
  xlon  =  reform(LON_AS(*,*), PROFMAX*FILEMAX)
  minvalue=0
  maxvalue=3
  div=3
  format='(I2)'
  sfcPick=2
  centrlat = 90.0
  orientlon = -80.0
  latmin_ps = 0.0
  latmax_ps = 90.0
  test = xlat ge latmin_ps and xlat le latmax_ps and xlon ge lonmin and xlon le lonmax
  ind = where(test)

  map_name = figsDir + prefix + yyyymmdd +'_sfcTyp2_pn_all_as.png'
  title = 'MIRS ' + strupcase(satId) + ' Post-Process Surface Type ' + date +  ' Asc' + ' (V' + version +')'
  plot_polar,sfc_ps(ind),sfc_ps(ind),map_name,minvalue,maxvalue,latmin_ps,latmax_ps,lonmin,lonmax,centrlat,orientlon,xlat(ind),xlon(ind),title, $
    sfcPick,div,format


  centrlat = -90.0
  orientlon = 0.0
  latmin_ps = -90.0
  latmax_ps = 0.0
  test = xlat ge latmin_ps and xlat le latmax_ps and xlon ge lonmin and xlon le lonmax
  ind = where(test)
  map_name = figsDir + prefix + yyyymmdd +'_sfcTyp2_ps_all_as.png'
  title = 'MIRS ' + strupcase(satId) + ' Post-Process Surface Type ' + date +  ' Asc' + ' (V' + version +')'
  plot_polar,sfc_ps(ind),sfc_ps(ind),map_name,minvalue,maxvalue,latmin_ps,latmax_ps,lonmin,lonmax,centrlat,orientlon,xlat(ind),xlon(ind),title, $
    sfcPick,div,format


  sfc_ps =  reform(MIRS_SFC2_DS(*,*), PROFMAX*FILEMAX)
  xlat  =  reform(LAT_DS(*,*), PROFMAX*FILEMAX)
  xlon  =  reform(LON_DS(*,*), PROFMAX*FILEMAX)
  minvalue=0
  maxvalue=3
  div=3
  format='(I2)'
  sfcPick=2
  centrlat = 90.0
  orientlon = -80.0
  latmin_ps = 0.0
  latmax_ps = 90.0
  test = xlat ge latmin_ps and xlat le latmax_ps and xlon ge lonmin and xlon le lonmax
  ind = where(test)
  map_name = figsDir + prefix + yyyymmdd +'_sfcTyp2_pn_all_ds.png'
  title = 'MIRS ' + strupcase(satId) + ' Post-Process Surface Type ' + date +  ' Des' + ' (V' + version +')'
  plot_polar,sfc_ps(ind),sfc_ps(ind),map_name,minvalue,maxvalue,latmin_ps,latmax_ps,lonmin,lonmax,centrlat,orientlon,xlat(ind),xlon(ind),title, $
    sfcPick,div,format

  centrlat = -90.0
  orientlon = 0.0
  latmin_ps = -90.0
  latmax_ps = 0.0
  test = xlat ge latmin_ps and xlat le latmax_ps and xlon ge lonmin and xlon le lonmax
  ind = where(test)
  map_name = figsDir + prefix + yyyymmdd +'_sfcTyp2_ps_all_ds.png'
  title = 'MIRS ' + strupcase(satId) + ' Post-Process Surface Type ' + date +  ' Des' + ' (V' + version +')'
  plot_polar,sfc_ps(ind),sfc_ps(ind),map_name,minvalue,maxvalue,latmin_ps,latmax_ps,lonmin,lonmax,centrlat,orientlon,xlat(ind),xlon(ind),title, $
    sfcPick,div,format

ENDIF


End
