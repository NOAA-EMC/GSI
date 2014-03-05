;---------------------------------------------------------------------------------
; Name:  plotSSMIS.pro
;
; Type:  IDL Program
;
; Description: 
;   To read SSMIS observed radiance data and simulated radiance data, 
;   which is generated from GFS 6-h forecast, then do following steps:
;   to access SSMIS data quality. 
;     1. Plot observed radiance
;     2. Plot simulated radiance
;     3. Plot Bias ( observed radiance - simulated radiance )
;
; Author: Deyong Xu (RTI) @ JCSDA, 
;         Deyong.Xu@noaa.gov
; Version: Feb 27, 2014, DXu, Initial coding
;         
;
;---------------------------------------------------------------------------------

;--------------------------------
; Specify IDL code to use.
;--------------------------------
@paths_idl.pro

chooseSensor:
PRINT, 'Choose instrument: '
PRINT,' 1 : NOAA-18/AMSUA&MHS'
PRINT,' 2 : NOAA-19/AMSUA&MHS'
PRINT,' 3 : MetOp-A/AMSUA&MHS'
PRINT,' 4 : MetOp-B/AMSUA/MHS'
PRINT,' 5 : F16/SSMIS'
PRINT,' 6 : F17/SSMIS'
PRINT,' 7 : F18/SSMIS'
PRINT,' 8 : NPP/ATMS'
PRINT,' 9 : AQUA/AMSRE'
PRINT,'10 : GCOMW1/AMSR2'
PRINT,'11 : FY3/MWRI'
PRINT,'12 : FY3/MWHS/MWTS'
PRINT,'13 : TRMM/TMI'
PRINT,'14 : GPM/GMI'
PRINT,'15 : MT/MADRAS'
PRINT,'16 : MT/SAPHIR'
PRINT,'17 : WindSat'


sensorOption = 0S
READ, sensorOption

; Set flag to fill value: 999
optionFlag = 999
; Check to see if a right option is chosen.
FOR i = 0, 16 DO BEGIN 
   IF sensorOption eq (i + 1) THEN BEGIN
      optionFlag = sensorOption 
      BREAK
   ENDIF
ENDFOR

; None of options is chosen
IF ( optionFlag eq 999 ) THEN BEGIN
   PRINT, "Wrong option, choose again !!!" 
   PRINT, ""
   GOTO, chooseSensor
ENDIF
                        
;-------------------------------------------
; Files containing a list of radiance files 
;-------------------------------------------
CASE sensorOption OF
    1: BEGIN
	 radListFile1 = '/data/home001/dxu/graphic/meas.list' 
	 radListFile2 = '/data/home001/dxu/graphic/fwd_fix_em.list'
       END
    2: BEGIN
	 radListFile1 = '/data/home001/dxu/graphic/meas.list' 
	 radListFile2 = '/data/home001/dxu/graphic/fwd_fix_em.list'
       END
    3: BEGIN
	 radListFile1 = '/data/home001/dxu/graphic/meas.list' 
	 radListFile2 = '/data/home001/dxu/graphic/fwd_fix_em.list'
       END
    4: BEGIN
	 radListFile1 = '/data/home001/dxu/graphic/meas.list' 
	 radListFile2 = '/data/home001/dxu/graphic/fwd_fix_em.list'
       END
    5: BEGIN
	 radListFile1 = '/data/home001/dxu/graphic/meas.list' 
	 radListFile2 = '/data/home001/dxu/graphic/fwd_fix_em.list'
       END
    6: BEGIN
	 radListFile1 = '/data/home001/dxu/graphic/meas.list' 
	 radListFile2 = '/data/home001/dxu/graphic/fwd_fix_em.list'
       END
    7: BEGIN
	 radListFile1 = '/data/home001/dxu/graphic/meas.list' 
	 radListFile2 = '/data/home001/dxu/graphic/fwd_fix_em.list'
       END
    8: BEGIN
	 radListFile1 = '/data/home001/dxu/graphic/meas.list' 
	 radListFile2 = '/data/home001/dxu/graphic/fwd_fix_em.list'
       END
    9: BEGIN
       radListFile1 = '/data/home001/dxu/graphic/meas.list' 
       radListFile2 = '/data/home001/dxu/graphic/fwd_fix_em.list'
       END
    10: BEGIN
	  radListFile1 = '/data/home001/dxu/graphic/meas.list' 
	  radListFile2 = '/data/home001/dxu/graphic/fwd_fix_em.list'
        END
    11: BEGIN
	  radListFile1 = '/data/home001/dxu/graphic/meas.list' 
	  radListFile2 = '/data/home001/dxu/graphic/fwd_fix_em.list'
        END
    12: BEGIN
	  radListFile1 = '/data/home001/dxu/graphic/meas.list' 
	  radListFile2 = '/data/home001/dxu/graphic/fwd_fix_em.list'
        END
    13: BEGIN
	  radListFile1 = '/data/home001/dxu/graphic/meas.list' 
	  radListFile2 = '/data/home001/dxu/graphic/fwd_fix_em.list'
        END
    14: BEGIN
	  radListFile1 = '/data/home001/dxu/graphic/meas.list' 
	  radListFile2 = '/data/home001/dxu/graphic/fwd_fix_em.list'
        END
    15: BEGIN
	  radListFile1 = '/data/home001/dxu/graphic/meas.list' 
	  radListFile2 = '/data/home001/dxu/graphic/fwd_fix_em.list'
        END
    16: BEGIN
	  radListFile1 = '/data/home001/dxu/graphic/meas.list' 
	  radListFile2 = '/data/home001/dxu/graphic/fwd_fix_em.list'
        END
    ELSE: BEGIN 
	    radListFile1 = '/data/home001/dxu/graphic/meas.list' 
	    radListFile2 = '/data/home001/dxu/graphic/fwd_fix_em.list'
          END
ENDCASE

; dxu: not in use yet. Once I'm done with reading 
; rad and create bias, then this is the thing I need to deal with. 
;--------------------------------
; Scene files for surface type
;--------------------------------
; dxu: this need to be changed so I can 
;      read scene files such as gdas, gfs, ecmwf or mirs 1d-var  
sceneListFile = '/net/orbit232l/home/pub/kgarrett/mirs_utilities/data/InputsData/edr.list'

;----------------------------------------
;  Allocation of memory for variables
;----------------------------------------
MAX_FOV = 30000L   ; max number of FOVs in a file (one orbit)
MAX_CHAN = 24L     ; max number of channels

PRINT, 'Read data again?'
PRINT, '0 - NO'
PRINT, '1 - YES'

READ, readAgain
IF (readAgain eq 0) THEN GOTO, mark_plotting
IF (readAgain eq 1) THEN GOTO, mark_readMeas

mark_readMeas:
;----------------------------------
; step 1: 
; Read two lists of radiance files
;----------------------------------
;  readlist,I, O, O 
readlist, radListFile1, radFileList1, nfilesRad1
readlist, radListFile2, radFileList2, nfilesRad2

; Get number of radiance files in each list/array.
nRadFiles1 = n_elements(radFileList1)
nRadFiles2 = n_elements(radFileList2)

; Make sure that number of radiance files are equal and not 0.
IF ( (nRadFiles1 ne nRadFiles2) || nRadFiles1 eq 0 ) THEN BEGIN
   PRINT,'Error: Number of Rad files in two list files NOT match'
   stop
ENDIF

; Save number of rad files (orbits)
nList=nRadFiles1

;-------------------------------------------
; step 2:
; Read radiances (measurements) from List1
; Read radiances (simulated) from List2
;-------------------------------------------
;-------------------------------
; sub-step 2.1:
;   Declare variables for rad1
;-------------------------------
;
; 1-D array (1) : File
nFOV_Rad1 = lonarr(nList)          ; total number of FOVs in a file
; 2-D array (6) : Fov x File 
scanPosRad1  = intarr(MAX_FOV,nList)  ; pos per file
scanLineRad1 = intarr(MAX_FOV,nList)  ; line per file
latRad1      = fltarr(MAX_FOV,nList)  ; lat per file
lonRad1   = fltarr(MAX_FOV,nList)  ; lon per file
dirRad1   = fltarr(MAX_FOV,nList)  ; dir per file
angleRad1   = fltarr(MAX_FOV,nList)  ; ang per file
QC_Rad1   = fltarr(MAX_FOV,nList)  ; QC  per file
; 3-D array (1) : Fov X File X Channel 
tbRad1    = fltarr(MAX_FOV,nList,MAX_CHAN) ; tb per file per channel
;
; Declare variables for rad2
;
; 1-D array (1) : File
nFOV_Rad2 = lonarr(nList)          ; total number of FOVs in a file
; 2-D array (6) : Fov x File 
scanPosRad2  = intarr(MAX_FOV,nList)  ; pos per file
scanLineRad2 = intarr(MAX_FOV,nList)  ; line per file
latRad2      = fltarr(MAX_FOV,nList)  ; lat per file
lonRad2      = fltarr(MAX_FOV,nList)  ; lon per file
dirRad2      = fltarr(MAX_FOV,nList)  ; dir per file
angleRad2    = fltarr(MAX_FOV,nList)  ; ang per file
QC_Rad2       = fltarr(MAX_FOV,nList)  ; QC  per file
; 3-D array (1) : Fov X File X Channel 
tbRad2    = fltarr(MAX_FOV,nList,MAX_CHAN) ; tb per file per channel

;---------------------------------------
; sub-step 2.2:
;   Read FMSRC file (observed radiance)
;---------------------------------------
;
; Loop thru. files/orbits
FOR iFile=0L,nRadFiles1-1 DO BEGIN
   PRINT,'------------------------------------------------'
   PRINT,'Orbit ',iFile, " is being processed : "
   PRINT,'   observed radiance orbit file :',radFileList1(iFile)

   ;
   ; Read radiance from a pair of files to rad1 and rad2
   ;
   ; LoadRadFile I, I , O , X
   LoadRadFile,1,radFileList1(iFile),rad1,0 

   PRINT, "Number of files                   : ", rad1.nFilesRad
   PRINT, "Number of profiles in file        : ", rad1.nProf
   PRINT, "Number of channels in file        : ", rad1.nChan
   PRINT, "Number of scan positions per line : ", rad1.nPosScan
   PRINT, "Number of scan lines              : ", rad1.nScanLines
   PRINT,'------------------------------------------------'

   ; Save total number of FOVs in a file
   nFOV_Rad1(iFile) = rad1.nprof

   ; Loop thru. FOVs within orbit
   FOR iProf=0L,rad1.nprof-1 DO BEGIN
      scanPosRad1(iProf, iFile) = rad1.ScanPos(0, iProf)
      scanLineRad1(iProf, iFile) = rad1.ScanLine(0, iProf)
      latRad1(iProf, iFile)  = rad1.Lat(0, iProf)
      lonRad1(iProf, iFile)  = rad1.Lon(0, iProf)
      dirRad1(iProf, iFile)  = rad1.Direc(0, iProf)
      ; Average angles over all channels to compute mean angle
      angleRad1(iProf, iFile)  = mean(rad1.Angle(0, iProf, 0 : rad1.nChan - 1))
      ; Get the 1st QC in the 1st orbit
      QC_Rad1(iProf, iFile)  = rad1.QC(0,iProf,0)
      ; Get tb for each channel
      tbRad1(iProf, iFile, 0 : rad1.nChan - 1)   $
          = rad1.tb(0, iProf, 0 : rad1.nChan - 1)
   ENDFOR
ENDFOR

;
;---------------------------------------
; sub-step 2.3:
;   Read FWD file   (simulated radiance)
;---------------------------------------
;
; Loop thru. files/orbits
FOR iFile=0L,nRadFiles1-1 DO BEGIN
   PRINT,'------------------------------------------------'
   PRINT,'Orbit ',iFile, " is being processed : "
   PRINT,'   simulated orbit file :',radFileList2(iFile)

   ;
   ; Read radiance from rad2
   ;
   ; LoadRadFile I, I , O , X
   LoadRadFile,1,radFileList2(iFile),rad2,0 

   PRINT, "Number of files                   : ", rad2.nFilesRad
   PRINT, "Number of profiles in file        : ", rad2.nProf
   PRINT, "Number of channels in file        : ", rad2.nChan
   PRINT, "Number of scan positions per line : ", rad2.nPosScan
   PRINT, "Number of scan lines              : ", rad2.nScanLines
   PRINT,'------------------------------------------------'

   ; Loop thru. FOVs within orbit
   FOR iProf=0L,rad2.nprof-1 DO BEGIN
      scanPosRad2(iProf, iFile) = rad2.ScanPos(0, iProf)
      scanLineRad2(iProf, iFile) = rad2.ScanLine(0, iProf)
      latRad2(iProf, iFile)  = rad2.Lat(0, iProf)
      lonRad2(iProf, iFile)  = rad2.Lon(0, iProf)
      dirRad2(iProf, iFile)  = rad2.Direc(0, iProf)
      ; Average angles over all channels to compute mean angle
      angleRad2(iProf, iFile)  = mean(rad2.Angle(0, iProf, 0 : rad2.nChan - 1))
      ; Get the 1st QC in the 1st orbit
      QC_Rad2(iProf, iFile)  = rad2.QC(0,iProf,0)
      ; Get tb for each channel
      tbRad2(iProf, iFile, 0 : rad2.nChan - 1)   $
          = rad2.tb(0, iProf, 0 : rad2.nChan - 1)
   ENDFOR
ENDFOR

;-------------------------------------------
; step 3:
; Reform data
;-------------------------------------------
reform:
;-----------------
; sub-step 3.1:
;   constants
;-----------------
; rad1 (observed)
;
nChan = rad1.nChan      ; number of channels
  
;-----------------
; sub-step 3.2:
;   2-d arrays
;-----------------
; Convert 2-D (Fov X File) into 1-D array ( Fov * File )
; Result:
;     file 1        file 2             file n
;   [ Fov * File ][ Fov * File ] ... [ Fov * File ]
;
; for observed data, ref_ stands for reform
ref_scanPos1  = reform(scanPosRad1(*, *), nList * MAX_FOV)
ref_scanLine1 = reform(scanLineRad1(*, *), nList * MAX_FOV)
ref_Lat1      = reform(latRad1(*, *), nList * MAX_FOV)
ref_Lon1      = reform(lonRad1(*, *), nList * MAX_FOV)
ref_ModeFlag1 = reform(dirRad1(*, *), nList * MAX_FOV)
ref_Angle1    = reform(angleRad1(*, *), nList * MAX_FOV)
ref_QC1       = reform(QC_Rad1(*, *), nList * MAX_FOV)
ref_Tb1       = fltarr(nList * MAX_FOV, rad1.nChan)
; for simulated data
ref_scanPos2  = reform(scanPosRad2(*, *), nList * MAX_FOV)
ref_scanLine2 = reform(scanLineRad2(*, *), nList * MAX_FOV)
ref_Lat2      = reform(latRad2(*, *), nList * MAX_FOV)
ref_Lon2      = reform(lonRad2(*, *), nList * MAX_FOV)
ref_ModeFlag2 = reform(dirRad2(*, *), nList * MAX_FOV)
ref_Angle2    = reform(angleRad2(*, *), nList * MAX_FOV)
ref_QC2       = reform(QC_Rad2(*, *), nList * MAX_FOV)
ref_Tb2       = fltarr(nList * MAX_FOV, rad2.nChan)
 
;-----------------
; sub-step 3.3:
;   3-d arrays
;-----------------
; Convert 3-D (Fov X File X Channel ) 
; into 2-D array ( (Fov * File) X Channel )
;   file 1        file 2             file n
; [ Fov * File ][ Fov * File ] ... [ Fov * File ]  <= chan 1
; [ Fov * File ][ Fov * File ] ... [ Fov * File ]  <= chan 2
;   ...
; [ Fov * File ][ Fov * File ] ... [ Fov * File ]  <= chan n
;
FOR iChan = 0L, nChan - 1 DO BEGIN
   ref_Tb1(*, iChan) = reform(tbRad1(*, *, iChan), nList * MAX_FOV)
   ref_Tb2(*, iChan) = reform(tbRad2(*, *, iChan), nList * MAX_FOV)
ENDFOR

PRINT,"size of ref_scanPos1  =  ", n_elements(ref_scanPos1)
PRINT,"size of ref_scanLine1 =  ", n_elements(ref_scanLine1)
PRINT,"size of ref_Lat1      =  ", n_elements(ref_Lat1)
PRINT,"size of ref_Lon1      =  ", n_elements(ref_Lon1)
PRINT,"size of ref_ModeFlag1 =  ", n_elements(ref_ModeFlag1)
PRINT,"size of ref_Angle1    =  ", n_elements(ref_Angle1)
PRINT,"size of ref_QC1       =  ", n_elements(ref_QC1)
PRINT,"size of ref_Tb1       =  ", n_elements(ref_Tb1)

;---------------------------------------
; step 4: 
; Plot radiances (observed + simulated)
;---------------------------------------
mark_plotting:
impr = 0

; global
MIN_LAT = -90
MAX_LAT = 90
MIN_LON = -180
MAX_LON = 180

; SSMIS channel defs
ssmisChanNum = ['1','2','3','4','5','6','7','8','9','10',          $
                '11','12','13','14','15','16','17','18','19','20', $ 
                '21','22', '23', '24']
ssmisChanInfo  = ['50.3', '52.8', '53.596', '54.4', '55.5', '57.29', '59.4',  $
                  '150', '183.31', '183.31' , '183.31' , '19.35', '19.35',    $
                  '22.235', '37', '37', '91.655', '91.655', '63.283242',      $
                  '60.792668', '60.792668', '60.792668', '60.792668', '60.792668' ]
;; These values need updated !!! 
minValues_SSMIS = [ 160, 140, 170, 170, 200, 200, 200, 210, 200, 190,  $
                    190, 200, 200, 230, 240, 150, 150, 170, 200, 170,  $
                    170, 170, 170, 170 ]
maxValues_SSMIS = [ 280, 300, 290, 290, 290, 280, 250, 230, 230, 220,  $
                    250, 240, 250, 260, 280, 300, 320, 310, 300, 300,  $
                    300, 280, 280, 280 ]

; Specify the channels to plot
chs2Plot = INDGEN(24)
;chs2Plot = INDGEN(2)
;chs2Plot = [22]

nChs2Plot = n_elements(chs2Plot)

prefix1 = 'SSMIS_Obs_plotting_'
prefix2 = 'SSMIS_Sim_plotting_'

;----------------------------------
; sub-step 4.1: 
; Plot observed radiances for chosen channels.
;----------------------------------
;
; Loop thru. channels to plot
FOR i=0, nChs2Plot - 1 DO BEGIN
   ; Remove all the whitespaces to make a new string
   image_name=strcompress(prefix1 + ssmisChanNum[chs2Plot(i)] + '.ps',/remove_all)

   ERASE
   !P.MULTI=1
   !P.FONT=-1

   xz=20
   yz=18
   LOADCT, 39
   SET_PLOT, 'PS'
   DEVICE, filename=image_name, /color, bits_per_pixel=8, $ 
           xsize=xz, ysize=yz, xoffset=2, yoffset=2,      $
           /portrait, font_size=16, /bold, /courier

   channel = ssmisChanInfo[chs2Plot(i)] + ' GHz'
   title = 'SSMIS observed TB ' + channel + ' 2012-01-20'


   ; Select out profiles 
   ;    radiance: ref_Tb1 > 0.
   ;    Orbit mode flag: ref_ModeFlag1 = 0
   filter1 = WHERE(ref_Lat1 ge MIN_LAT       $
	     and ref_Lat1 le MAX_LAT         $
	     and ref_Tb1(*,chs2Plot(i)) gt 0 $
	     and ref_ModeFlag1 eq 0)

   mapPlot, MIN_LAT,MAX_LAT,MIN_LON,MAX_LON,    $
            ref_Lat1,ref_Lon1,                  $
            filter1,   $
            title,     $
            minValues_SSMIS(chs2Plot(i)),   $
            maxValues_SSMIS(chs2Plot(i)),   $
            ref_Tb1(*,chs2Plot(i)),         $   
            'K', $   ;unit
            0.8, $   ;scale
            8,   $   ;symb
            1,   $   ;thick
            0,   $   ;overlap
            '(f5.1)' ;fmt

   ; Draws continental boundaries, etc, over an existing map projection 
   ; established by MAP_SET.
   ;
   MAP_CONTINENTS,/continents,/noborder,/hires,/usa,fill_continents=0,color=18

ENDFOR

;----------------------------------
; sub-step 4.2: 
; Plot simulated radiances for chosen channels.
;----------------------------------
;
; Loop thru. channels to plot
FOR i=0, nChs2Plot - 1 DO BEGIN
   ; Remove all the whitespaces to make a new string
   image_name=strcompress(prefix2 + ssmisChanNum[chs2Plot(i)] + '.ps',/remove_all)

   ERASE
   !P.MULTI=1
   !P.FONT=-1

   xz=20
   yz=18
   LOADCT, 39
   SET_PLOT, 'PS'
   DEVICE, filename=image_name, /color, bits_per_pixel=8, $ 
           xsize=xz, ysize=yz, xoffset=2, yoffset=2,      $
           /portrait, font_size=16, /bold, /courier

   channel = ssmisChanInfo[chs2Plot(i)] + ' GHz'
   ;title = 'SSMIS TB ' + channel + ' 2012-04-15'
   title = 'SSMIS simulated TB ' + channel + ' 2012-01-20'


   ; Select out profiles 
   ;    radiance: ref_Tb2 > 0.
   ;    Orbit mode flag: ref_ModeFlag2 = 0
   filter2 = WHERE(ref_Lat2 ge MIN_LAT       $
	     and ref_Lat2 le MAX_LAT         $
	     and ref_Tb2(*,chs2Plot(i)) gt 0 $
	     and ref_ModeFlag2 eq 0)

   mapPlot, MIN_LAT,MAX_LAT,MIN_LON,MAX_LON,    $
            ref_Lat2,ref_Lon2,                  $
            filter2,   $
            title,     $
            minValues_SSMIS(chs2Plot(i)),   $
            maxValues_SSMIS(chs2Plot(i)),   $
            ref_Tb2(*,chs2Plot(i)),         $   
            'K', $   ;unit
            0.8, $   ;scale
            8,   $   ;symb
            1,   $   ;thick
            0,   $   ;overlap
            '(f5.1)' ;fmt

   ; Draws continental boundaries, etc, over an existing map projection 
   ; established by MAP_SET.
   ;
   MAP_CONTINENTS,/continents,/noborder,/hires,/usa,fill_continents=0,color=18

ENDFOR

PRINT,'End of processing...'
END
