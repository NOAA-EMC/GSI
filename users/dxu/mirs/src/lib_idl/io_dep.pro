;=========================================================================================
; Name:		ReadDEP
;
;
; Type:		IDL Subroutine
;
;
; Description:  Read DEP file
;
;
; Arguments:
;
;      Name		    Type	    Description
;      ------------------------------------------------------------------
;	- InputFile          I              Name of the input file
;	- nprofiles          O              number of profiles contained
;	- Dep                O              Structure contain all data
;
; Modules needed:
;       - None
;
;  Record of revisions:
;        Date          Programmer                    Description of change
;      ------------    -------------      	    ----------------------
; 	05/25/2007     Wanchun Chen		        Original code
;
;=========================================================================================

PRO ReadDEP, inputFile, nprofiles, Dep

    AlgVersion = 0L
    iTyp       = 0L
    nPrf       = 0L
    nPosScan   = 0L
    nScanLines = 0L
    iu	       = 0L
    
    OPENR,iu,InputFile,/get_lun,error=err,/f77_unformatted,/swap_if_little_endian
    IF (err ne 0) THEN BEGIN
        print ,!ERROR_STATE.MSG
        RETURN
    ENDIF
    
    readu,iu,iTyp,AlgSN
    readu,iu,nPrf
    readu,iu,nPosScan 
    readu,iu,nScanLines 
    nprofiles = nPrf
    
    ;*********************************************
    ;  Define DEP structure if not defined yet
    ;*********************************************
    typeDep=size(dep,/type)
    DepIsNotDefined = ( typeDep ne 8)
    NprfBiggerDeclaredSize=0
    
    IF (not(DepIsNotDefined)) THEN NprfBiggerDeclaredSize=(nPrf gt Dep.DeclarNprf)
    
    if ( DepIsNotDefined or NprfBiggerDeclaredSize) then begin

    	Dep = { 				 	$
 		AlgSN:AlgSN,	                        $ ;MIRS Algorithm serial number (svn) 
 		iTyp:iTyp,				$ ;0->DEP from Scene, 1->DEP from retrieved scene 
		nProf:nPrf,				$ ;number of profiles
		nProfsProcessed:0L,                     $ ;effective number of processed files
		ProfIndx:lonArr(nPrf),			$ ;profile index 
		;--Positioning Data
		Angle:fltarr(nPrf),		        $ ;Angle
		RelAziAngle:fltarr(nPrf),		$ ;Relative Azimuth Angle
		SolZenAngle:fltarr(nPrf),		$ ;Solar Zenith Angle
		lat:fltarr(nPrf),		        $ ;Latitude
		lon:fltarr(nPrf),		        $ ;Longitude
		node:lonarr(nPrf),			$ ;=0->ASC, =1->DESC
		scanDAY:lonarr(nPrf),			$ ;Day 
		scanYear:lonarr(nPrf),			$ ;Year
		scanUTC:fltarr(nPrf),		 	$ ;UTC time
		iscanPos:lonarr(nPrf),			$ ;Scan position 
		iscanLine:lonarr(nPrf),			$ ;Scan line index
		nPosScan:nPosScan,			$ ;Number of scan positions within scanline
		nScanLines:nScanLines,			$ ;Number of scan lines within orbit
		DeclarNprf:nPrf,                        $
		;--Atmospheric/Hydrometeors/Cloud-related information
		iTypAtm:lonarr(nPrf),		   	$ ;Atmospheric type ID
		;DescTypAtm:strarr(nPrf),		$ ;Label of the atmospheric class
		TPW:fltarr(nPrf),			$ ;Total precipitable Water 
		Clw:fltarr(nPrf),			$ ;Integrated Cloud amount 
		RWP:fltarr(nPrf),			$ ;Integrated Liquid Rain water path
		SWP:fltarr(nPrf),			$ ;Integrated Snow water path
		IWP:fltarr(nPrf),			$ ;Integrated Ice water path
		GWP:fltarr(nPrf),			$ ;Integrated Graupel water path
		RR:fltarr(nPrf),			$ ;Surface rain rate
		SFR:fltarr(nPrf),			$ ;Snow falling rate
		CldTop:fltarr(nPrf),		     	$ ;Cloud Top Pressure
		CldBase:fltarr(nPrf),		    	$ ;Cloud Base Pressure
		CldThick:fltarr(nPrf),		   	$ ;Cloud thickness
		PrecipType:fltarr(nPrf),		$ ;Precipitation type (frozen/liquid)
		RainFlag :fltarr(nPrf),		 	$ ;Rain flag
		LWP:fltarr(nPrf),			$ ;Integrated Liquid water path
		;--Surface -related information
		iTypSfc:lonarr(nPrf),		  	$ ;Surface type ID
		;DescTypSfc:strarr(nPrf),		$ ;Label of the surface class
		SWE:fltarr(nPrf),			$ ;Snow water equivalent
		SnowGS:fltarr(nPrf),			$ ;Snow effective grain size
		SnowCover:fltarr(nPrf),		  	$ ;Snow cover extent
		SM:fltarr(nPrf),			$ ;Soil Moisture
		SIC:fltarr(nPrf),			$ ;Sea-ice concentration
		SIC_FY:fltarr(nPrf),			$ ;First-year sea-ice concentration
		SIC_MY:fltarr(nPrf),			$ ;Multi-yeat sea-ice concentration
		WindSp:fltarr(nPrf),		    	$ ;Wind speed
		WindDir:fltarr(nPrf),		    	$ ;Wind vector
		WindU:fltarr(nPrf),			$ ;U-direction wind speed
 		WindV:fltarr(nPrf),			$ ;V-direction wind speed			     
		;--QC info 
		qc:lonarr(4,nPrf),			$ ;QC vector
		;--Convergence items (when the DEP is coming from a retrieved scene )
		nIter:lonarr(nPrf),			$ ;Number of iterations 			   
		ChiSq:fltarr(nPrf) 			$ ;Convergence metric	 
	}
    
    endif 

    
    ;*************************
    ; initialize variables
    ;*************************
   
    ProfIndx = 0L

    iTypAtm	=0L
    TPW		=0.0
    Clw		=0.0
    RWP		=0.0
    SWP		=0.0
    IWP		=0.0
    GWP		=0.0
    LWP		=0.0
    RR		=0.0
    SFR		=0.0
    CldTop 	=0.0
    CldBase 	=0.0
    CldThick 	=0.0
    PrecipType 	=0.0
    RainFlag  	=0.0

  
    iTypSfc    	=0L
    ;DescTypSfc =''
    SWE        	=0.0
    SnowGS      =0.0
    SnowCover  	=0.0
    SM         	=0.0
    SIC        	=0.0
    SIC_FY    	=0.0
    SIC_MY    	=0.0
    WindSp     	=0.0
    WindDir    	=0.0
    WindU      	=0.0
    WindV      	=0.0 


    lat		=0.0 
    lon		=0.0 
    node	=0L 
    scanDAY	=0L
    scanYear	=0L
    scanUTC	=0.0
    iscanPos	=0L
    iscanLine	=0L
    angle	=0.0
    RelAziAngle	=0.0
    SolZenAngle	=0.0

    qc   	=lonarr(4) 
     
    nIter 	=0L
    ChiSq 	=0.0
   
   
    ;*******************************************************************
    ; read data profile by profile and assign them to Dep structure
    ;*******************************************************************
    nprofiles = 0L
    for i=0L, nprf-1 do begin

   	ON_IOerror,endRead
	
	readu, iu, ProfIndx
	Dep.ProfIndx(i) = ProfIndx
    
	;---Atmospheric, cloud and hydrometeors constituents
	READU, iu, iTypAtm, TPW ,CLW, RWP, SWP, IWP, GWP, RR, $
	  	SFR, CldTop, CldBase, CldThick, PrecipType, RainFlag, LWP  
    	Dep.iTypAtm(i)   = iTypAtm
	Dep.TPW(i) 	 = TPW
	Dep.CLW(i) 	 = CLW
	Dep.RWP(i) 	 = RWP
	Dep.SWP(i) 	 = SWP
	Dep.IWP(i) 	 = IWP
	Dep.GWP(i) 	 = GWP
	Dep.RR(i)  	 = RR
	Dep.SFR(i) 	 = SFR
	Dep.CldTop(i)    = CldTop
	Dep.CldBase(i)   = CldBase 
	Dep.CldThick(i)  = CldThick
	Dep.PrecipType(i)= PrecipType
	Dep.RainFlag(i)  = RainFlag
	Dep.LWP(i) 	 = LWP


    	;---Surface parameters
	READU, iu, iTypSfc, SWE, SnowCover, SM, SIC,WindSp, WindDir, WindU, WindV, SnowGS,SIC_FY,SIC_MY	
	Dep.iTypSfc(i) 	= iTypSfc
	Dep.SWE(i) 	= SWE 
        Dep.SnowGS(i)   = SnowGS
	Dep.SnowCover(i)= SnowCover
	Dep.SM(i) 	= SM
	Dep.SIC(i) 	= SIC
	Dep.SIC_FY(i) 	= SIC_FY
	Dep.SIC_MY(i) 	= SIC_MY
	Dep.WindSp(i) 	= WindSp
	Dep.WindDir(i) 	= WindDir
	Dep.WindU(i) 	= WindU
	Dep.WindV(i) 	= WindV
	
   
    	;---QC variables 
	READU, iu, qc  
 	Dep.qc(0,i) 	= qc(0) 
	Dep.qc(1,i) 	= qc(1) 
 	Dep.qc(2,i) 	= qc(2) 
	Dep.qc(3,i) 	= qc(3) 
 	
    
    	;---Positioning variables
    	READU, iu, lat, lon, node, scanUTC, scanYear, scanDay, iscanPos, iscanLine, angle, RelAziangle, SolZenangle
	Dep.lat(i) 	   = lat
	Dep.lon(i) 	   = lon
 	Dep.node(i) 	   = node
	Dep.scanUTC(i) 	   = scanUTC
	Dep.scanYear(i)    = scanYear
	Dep.scanDay(i) 	   = scanDay
 	Dep.iscanPos(i)    = iscanPos
 	Dep.iscanLine(i)   = iscanLine
	Dep.angle(i) 	   = angle
    	Dep.relAziAngle(i) = RelAziAngle
	Dep.SolZenAngle(i) = SolZenAngle

    	;---In case the DEP is from a retrieved scene 
    	IF (iTyp eq 1) THEN BEGIN
    	   READU, iu, nIter,ChiSq
	   Dep.nIter(i) = nIter
	   Dep.ChiSq(i) = ChiSq
    	ENDIF
    	
	nprofiles = nprofiles + 1
	Dep.nProfsProcessed = nprofiles 
    
    endfor
    close,iu
    free_lun,iu
    print, 'Effective DEP nprofiles=', nprofiles
   
    endRead: return
    
END
