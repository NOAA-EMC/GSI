      PROGRAM grid2grid

C********************************************************************
C
C  GRID2GRID   - CREATE VERIFICATION STATS BETWEEN GRIDED FORECAST and 
C                GRIDED OBSERVATION and write the SL1L2, VL1L2, and FHO partial sum
C                into a vsdb file. This program needs a user defined control file
C                which defines the verification period, verification valid time,
C                grid ID, subregions, statistical types, variables with their
C                levels and verification types (anomly, tendency, wave/filter, etc).
C                This program also needs both well-arranged, a thinned forecast
C                GRIB file and a observation(analysis) GRIB file. If anomly verification
C                is requested, a climatology GRIB file is needed
C
C                Please pay attention to the maximum value settings for this
C                program. If your verification are performed on very large model
C                domain or too many variables or on too many levels, or too many
C                FHO thresholds, you must modify the parm.inc file 
C  
C                THIS PROGRAM IS  MODIFIED FROM Geoff DiMego's GRIDTOBS
C
C  AUTHOR: Binbin Zhou, EMC/NCEP, NWS, NOAA, United States, Feb, 2005
C
C    MODIFCATION LOG:
C
C    03/20/2005: B. Zhou, Add tedency verification ability
C    01/20/2006: B. ZHOU, add climatological anomly SAL1L2, VAL1L2    
C    02/15/2006: B. ZHOU, add waved filters
C    03/01/2006: B. ZHOU, add Anomly FHO - AFHO
C    08/07/2006: B. ZhOU, Modify 2D wave (FITWAV_2D) and add 1D wave(FITWAV_1D)   
C    10/01/2006: B. Zhou, consider cloud/rain/aerosol/smoke verification by using continue_mrk
C    01/30/2007: B. Zhou, add weighting of grid area for different latitude
C    02/21/2007: B. Zhou, add ensemble ESL1L2, RHNT, RHET stats
C    04/13/2007: B. Zhou, add soil moisture and soil temperature mask for continue_mrk
C    06/08/2007: B. Zhou, change all partial sum from single to double precision as requesed by Fanglin 
c
C   Variables:
C    - maxmod, mxfcst, mxdate, maxobs, mxarea, mxstat, mxvrbl, maxlvl
C      integer: maximum numbers of models, forecast time, verified time, obs type, sub-region,
C               statistic type, variables, and vertical level/levels
C               defined in parm.inc file
C    - sumdata,sumgrid,sumprod,ssqdata,ssqgrid 
C      real: the 5 results written into vsdb file for SL1L2
C    - count 
C      real: counting number when computing SL1L2, VL1L2 and FHO
C    - namodel, namfcst, namvfdate, namvfyobs, namarea, nchrstat, namvarbl, namlevel
C      character: the string name of models, forecast time, verified time, obs type, sub-region,
C                 statistic type, variables, and vertical level/levels
C    - nchrmodel, nchrfcst, nchrvfdate, nchrvfyobs, nchrarea, nchrstat, nchrvarbl, nchrlevel    
C      integer: number of characters for the string name of models, forecast time, verified time, 
C               obs type, sub-region, statistic type, variables, and vertical level/levels
C    - vtflg, nmbflg, concon, cenlon: not used in grid2grid, only used in call setarea( ) 
C    - regions (100):             
C      characters: string name for 31 sub-regions
C                  read from regions file
C    - mode, imax, imin, jmax, jmin, ngrid
C      real: region mode, min and max numbers for lon and lat, total number of points
C            ngrid = imax*jmax
C    - alat1, elon1, dxx, dyy, elonv, alatan, latlong, lambert, polarstereo: not used
C    - numreg    
C      integer: requested region id 
C    - ig104(147,110)
C      integer: 30 region locations matrix read from  grid#104 file
C    - iplevel, modelvl
C      integer: pressure of each level, level mode (multiple or single),  
C    - namversion
C      character: program version name ('V01')
C    - vdbhdr132, input, substr (3)
C      characters: working variable in reading control files
C
C    Folowing are added for grid2grid 
C
C    - cyyyyfcst,cmmfcst,cddfcst,chhfcst,cfffcst,cyyyyobsv,cmmobsv,cddobsv,chhobsv,cffobsv
C      characters: string names for year, month, day, cycle time and lead time for forecast and observation  
C                  read from control file
C    - yyyyfcst,mmfcst,ddfcst,hhfcst,fffcst,yyyyobsv,mmobsv,ddobsv,hhobsv,ffobsv
C      integer: numbers for year, month, day, cycle time and lead time for forecast and observation
C               converted from cmmfcst,cddfcst,chhfcst,cfffcst,cyyyyobsv,cmmobsv,cddobsv,chhobsv,cffobsv
C   -  k5,k6,k7,ck7
C      integer: kpds5, kpds6, kpds7 for all variables (ck7: kpds7 in string) 
C   -  vectormrk
C      integer: vector masks for vectors (1: vector, 0:non-vector)
C   -  namlvl,nchrlvl 
C      characters, integer: name of levels for different variables and its string length
C   -  plevel
C      integer: levels(in pressure)  for different variables 
C   -  fho, nchrfho
C      characters, integer: statistic type name 'FHO>' or 'FHO<' read from control file and its string length
C   -  fhothr, nchrfhothr
C      characters, integer: FHO threshold name and its lenth for various variables
C   -  rfhothr
C      real: FHO threshold in real 
C   -  fhomrk
C      integer: FHO mark for various variables(0:no FHO, number: has FHO and represent number of thresholds)   
C   -  region_id
C      integer: the sub-region id (1-30 defined by regions file) for all the grid points (total ngrid) 
C               retrieved from getregionid subroutine 
C   -  region_latlon   
C      integer: latitude and longitude of all grid proints (total ngrid) 
C               retrieved from getregionid subroutine
C   -  fcstdata, obsvdata
C      real, allocatable: forecast and observation data read from forecast and observation GRIB files
C   -  ufcst,vfcst, uobsv, vobsv
C      real, allocatable: fcst and obsv u and v profile data one all pressure levels, read from GRIB files
C      real, allocatable: fcst and obsv u and v at 10m, read from GRIB files
C   -  jpds,jgds,kpds,kgds,jjpds, kens, kprob,xprob,kclust,kmembr
C      integer: pds data to read GRIB file 
C   -  yyfcst, yyobsv,   
C      integer: year in 2 digits    
C   -  fgrbunit,ogrbunit,finxunit, oinxunit, nvsdb
C      integer: file units of grib file, grib index file for fsct and obsv data  
C   -  fgrbfile, ogrbfile,finxfile, oinxfile
C      characters, grib and index file names for fsct and obsv 
C   -  nvsdb
C      integer: vsdb file unit
C   -  all variables with 2: the fcst or obsv data for previous times to compute the  
C      tendency. 
C      dt, cdt: The tendency period hours(3hr change, 6 hr change, 12 hr change and 24 hr change)
C   -  tendencymrk: to check if the a variable is requested to compute tendency (ie, variable has has _TND at end)      
C   -  updown: Character*1, mark FHO for tendency variable, if '+', to see increase, '-' to see decrease
C   -  hasdata: inetger, in case of compute tendency, need to get data from previous 3, or 6, or 12, or 24 hours. 
C      if such data is out of range specified in forecast/observation times in control file, then,
C      such data are not exist. Then set it = 0   
C   -  continue_mrk: for those non-continuing variables such as cloud, rain, slow, aerosol, smoke,etc
C
C   -  climdata:  climatologic data for computing anomly
C   -  uclim, vclim: climatologic u, v at different pressure levels
C   -  anomly_mrk: anomly markers for different variables
C   -  anomlylev:  anomly markers for has-anomly variables at different levels
C
C   -  wavemrk: Wave marker array for varaibles that are required for wave filter (0 for no wave, 1 for 1D wave, 2 for 2D wave)
C      fcst2d, obsv2d, clim2d: temp array 
C
C   -  afhomrk: fho marks for anomly 
C
C   -  cloud_sfc: sfc height indicator  for compute cloud base
C
C   -  lat_weight: latitude weight indicator for grid area
C   
C   -  fcstmdl: stores forecast data from each ensemble member 
C   -  ufcstmdl: stores forecast U from each ensemble member
C   -  vfcstmdl: stores forecast V from each ensemble member
  

C The grid2grid main program has three steps:
C
C     STEP 1: read parameters from control files, they are
C         model name, forecast/observed times, observation types, statistic types, variables,
C         variable's kpds5,6,7, vectors, FHO thresholds, pressure levels, etc
C
C     STEP 2: according to the kpds5,6,7, read data from fcst and obsv GRIB files
C
C     STEP 3: according to the statistic types (SL1L2, VL1L2, FHO, etc), do statistic 
C             computation and written the results into the vsdb file
C
C     OUTPUT: file name: "grid2grid.vsdb" 
C
C  Language: FORTRAN 90
C  System:   IBM CCS/Blue/White/Dew/Mist
C              
C
      INCLUDE 'parm.inc'

c      DIMENSION sumdata(mxfcst,mxvrbl,maxlvl,mxarea,maxobs), 
c     +            sumgrid(mxfcst,mxvrbl,maxlvl,mxarea,maxobs), 
c     +            sumprod(mxfcst,mxvrbl,maxlvl,mxarea,maxobs), 
c     +            ssqdata(mxfcst,mxvrbl,maxlvl,mxarea,maxobs), 
c     +            ssqgrid(mxfcst,mxvrbl,maxlvl,mxarea,maxobs), 
c     +            count(mxfcst,mxvrbl,maxlvl,mxarea,maxobs)

      DIMENSION nchrmodel(maxmod), nchrfcst(mxfcst), nchrvfdate(mxdate),
     +            nchrvfyobs(maxobs), nchrarea(mxarea), 
     +            nchrstat(mxstat), nchrvarbl(mxvrbl), 
     +            nchrlevel(maxlvl)
      CHARACTER*24 namodel(maxmod), namfcst(mxfcst), namvfdate(mxdate),
     +            namvfyobs(maxobs), namarea(mxarea), namstat(mxstat), 
     +            namvarbl(mxvrbl), namlevel(maxlvl)

      COMMON /names/namodel, namfcst, namvfdate, namvfyobs, namarea, 
     +            namstat, namvarbl, namlevel
      COMMON /nchrs/nchrmodel, nchrfcst, nchrvfdate, nchrvfyobs, 
     +            nchrarea, nchrstat, nchrvarbl, nchrlevel
      LOGICAL vtflg, nmbflg
      COMMON /cnvrsns/vtflg, nmbflg (maxmod), concon (maxmod),
     +		       cenlon (maxmod)
      CHARACTER*3 regions (100)

      LOGICAL*1 latlong, lambert, polarstereo  !Add  by Binbin, otherwise, they are can not be retrieved
      COMMON /grdef/mode(mxarea), imax(mxarea), imin(mxarea),
     +            jmax(mxarea), jmin(mxarea), alat1(mxarea),
     +            elon1(mxarea), dxx(mxarea), dyy(mxarea),
     +            elonv(mxarea), alatan(mxarea), latlong(mxarea),
     +            lambert(mxarea), polarstereo(mxarea)
      COMMON /grdef1/numreg(mxarea), ig104(147,110),regions


      CHARACTER*3 namversion
      CHARACTER*132 vdbhdr132, input, substr (4)

C
C

C  Binbin Zhou: For grid2grid
      CHARACTER*80 str
      CHARACTER*24 fcst_ymdhf(mxfcst), obsv_ymdhf(maxobs)      !store YYYYMMDDHHFF string for fcst and obsv read from control
      CHARACTER*4 cyyyyfcst(mxfcst),cyyyyobsv(maxobs)
      CHARACTER*2 cmmfcst(mxfcst),cmmobsv(maxobs)
      CHARACTER*2 cddfcst(mxfcst),cddobsv(maxobs)
      CHARACTER*2 chhfcst(mxfcst),chhobsv(maxobs)
      CHARACTER*3 cfffcst(mxfcst),cffobsv(maxobs)
      integer yyyyfcst(mxfcst), mmfcst(mxfcst),
     +        ddfcst(mxfcst), hhfcst(mxfcst), fffcst(mxfcst),
     +        yyyyobsv(maxobs), mmobsv(maxobs),
     +        ddobsv(maxobs), hhobsv(maxobs), ffobsv(maxobs)
      integer k5(mxvrbl),k6(mxvrbl),k7(mxvrbl),vectormrk(mxvrbl)
      CHARACTER*6 ck7(mxvrbl)

      CHARACTER*24 namlvl(mxvrbl,maxlvl)
      integer nchrlvl(mxvrbl,maxlvl)

      CHARACTER*24 fho(mxvrbl),fhothr(mxvrbl,20)
      CHARACTER*24 afho(mxvrbl),afhothr(mxvrbl,20)
      integer  nchrfho(mxvrbl),nchrfhothr(mxvrbl,20),fhomrk(mxvrbl)
      integer  nchrafho(mxvrbl),nchrafhothr(mxvrbl,20),afhomrk(mxvrbl)
      real rfhothr(mxvrbl,20), rafhothr(mxvrbl,20)
      integer  continue_mrk(mxvrbl)

      integer anomlylev(mxvrbl,maxlvl), anomly_mrk(mxvrbl)

      COMMON /g2g/cyyyyfcst,cmmfcst,cddfcst,chhfcst,cfffcst,
     +            cyyyyobsv,cmmobsv,cddobsv,chhobsv,cffobsv,
     +             yyyyfcst, mmfcst, ddfcst, hhfcst, fffcst,
     +             yyyyobsv, mmobsv, ddobsv, hhobsv, ffobsv,
     +             k5,k6,k7,ck7,vectormrk,namlvl,nchrlvl,
     +        fhomrk,fho,nchrfho,fhothr,nchrfhothr,rfhothr,
     +             continue_mrk,anomly_mrk,anomlylev,
     +       afhomrk,afho,nchrafho,afhothr,nchrafhothr,rafhothr

      integer plevel(maxlvl)
      integer region_id(maxpts)
      real region_latlon(2,maxpts), ptr1(2,mxarea),ptr2(mxarea)
      integer levels(mxvrbl)          !levels for different variables
      integer hasdata(mxfcst,mxvrbl,maxlvl)
      character*5 cloud_sfc, lat_weight

      COMMON /reg/region_id, region_latlon, ptr1,ptr2
      COMMON /layer/modelvl(maxlvl), iplevel(maxlvl,2)

C     for tendency:-------------------------------------------------
      DIMENSION  nchrfcst2(4,mxfcst), nchrvfdate2(4,mxdate)
      CHARACTER*24  namfcst2(4,mxfcst),namvfdate2(4,mxdate)
      CHARACTER*24 fcst_ymdhf2(4,mxfcst), obsv_ymdhf2(4,maxobs)      !store YYYYMMDDHHFF string for fcst and obsv read from control
      CHARACTER*4 cyyyyfcst2(4,mxfcst),cyyyyobsv2(4,maxobs)
      CHARACTER*2 cmmfcst2(4,mxfcst),cmmobsv2(4,maxobs)
      CHARACTER*2 cddfcst2(4,mxfcst),cddobsv2(4,maxobs)
      CHARACTER*2 chhfcst2(4,mxfcst),chhobsv2(4,maxobs)
      CHARACTER*3 cfffcst2(4,mxfcst),cffobsv2(4,maxobs)
      CHARACTER*2 cdt(mxvrbl)
      integer      dt(mxvrbl),tnd_has3,tnd_has6,tnd_has12,tnd_has24
      integer yyyyfcst2(4,mxfcst), mmfcst2(4,mxfcst),
     +        ddfcst2(4,mxfcst), hhfcst2(4,mxfcst), fffcst2(4,mxfcst),
     +        yyyyobsv2(4,maxobs), mmobsv2(4,maxobs),
     +        ddobsv2(4,maxobs), hhobsv2(4,maxobs), ffobsv2(4,maxobs)
      integer tendencymrk(mxvrbl)
      CHARACTER*1 updown(mxvrbl,20)
      COMMON /tnd/cyyyyfcst2,cmmfcst2,cddfcst2,chhfcst2,cfffcst2,
     +            cyyyyobsv2,cmmobsv2,cddobsv2,chhobsv2,cffobsv2,
     +            yyyyfcst2, mmfcst2, ddfcst2, hhfcst2, fffcst2,
     +            yyyyobsv2, mmobsv2, ddobsv2, hhobsv2, ffobsv2, 
     +            namfcst2,namvfdate2,nchrfcst2,nchrvfdate2,
     +            cdt, dt, tendencymrk, updown





      real, allocatable, dimension(:,:,:,:) :: fcstdata2,obsvdata2
      real, allocatable, dimension(:,:,:,:) :: ufcst2,vfcst2
      real, allocatable, dimension(:,:,:,:) :: uobsv2,vobsv2
      real, allocatable, dimension(:)       :: HGTsfc


      integer yyfcst2(4,mxfcst),yyobsv2(4,maxobs)

c--------------------------------------------------------------------

      real, allocatable, dimension(:,:,:,:) :: fcstdata,obsvdata
      real, allocatable, dimension(:,:,:,:) :: ufcst,vfcst
      real, allocatable, dimension(:,:,:,:) :: uobsv,vobsv

      real, allocatable, dimension(:,:,:,:) :: climdata
      real, allocatable, dimension(:,:,:,:) :: uclim,vclim


      dimension jpds(25),jgds(25),kpds(25),kgds(25),jjpds(25)          !grib
      dimension kens(5),kprob(2),xprob(2),kclust(16),kmembr(80) !grib extension

      integer yyfcst(mxfcst),yyobsv(maxobs), ngrid

      integer fgrbunit,ogrbunit,finxunit, oinxunit, nvsdb
      CHARACTER*80 fgrbfile, ogrbfile,finxfile, oinxfile     
      CHARACTER*80 fgrbfile2, ogrbfile2,finxfile2, oinxfile2     

      CHARACTER*80 cgrbfile, cinxfile
      integer getClimData_called

                                                                                                                  
      integer wavemrk(mxvrbl), wv1(mxvrbl), wv2(mxvrbl)
      CHARACTER*2 cwv1(mxvrbl),cwv2(mxvrbl)
      COMMON  /wave/wavemrk,cwv1,cwv2,wv1,wv2
      real, allocatable, dimension(:,:) :: fcst2d,obsv2d,clim2d

      integer nodata(mxfcst,mxvrbl,maxlvl)
      COMMON /nofile/nodata 

      integer igribid
      COMMON /grb/igribid

      real grid_area(maxpts), area_factor(maxpts), area_ref
      COMMON /weight/area_factor

 
      !just store fcst data of each models for ensemble stats  
      real, allocatable, dimension(:,:,:,:,:) :: fcstmdl        !Nmodel,numfcst,numvarbl,numlevel,ngrid
      real, allocatable, dimension(:,:,:,:,:) :: ufcstmdl
      real, allocatable, dimension(:,:,:,:,:) :: vfcstmdl

      CHARACTER*20 ensname                                      !SREF/NNN 
      Integer      Nmodel                                       !Total number of ensemble members, note: numodel always =1
  

C
C...   STRING FOR HEADER PARAMETERS
C
C
C...   STRING FOR THE OB, GUESS, ANALYSIS ....
C
C
C...   STRING FOR THE QUALITY MARKS ....
C
C----------------------------------------------------
      DATA namversion /'V01'/
C
      DATA bmiss /10E10/
      DATA rmiss /99999./
C     
C     STEP 1. READ CONTROL PARAMETERS from Control file
C
C     Also read in logical flag (T or F) to convert virtual temperature
C     observed into actual temperature.
C     

      continue_mrk = 0
      nodata = 0
      imodel = 1        ! number of models count.  initial = 1
      NMODEL = 1        ! Total number of models, default = 1

2000  READ (5,'(A)',END=3000) input
      CALL ST_CLST ( input, ' ', ' ', 4, substr, num, ier )
      namversion = substr (1)
      CALL ST_NUMB ( substr (2), lunin, ier )
      vtflg = ( substr (3) .eq. 't' .or. substr (3) .eq. 'T' )
      write(*,*) namversion, lunin 

c
c      obtain:
c      ensname       !e.g. SREF/21, SREF/WRF, SREF/ETA, SREF/RSM, SREF/CTL
c      Nmodel        !total # of models, numodel returned from readcntl is always = 1
c

      write(*,*) 'imodel=',imodel

      if(num.gt.3.and.imodel.eq.1) then    !if user set additional ensemble info in user control  
       ensname = substr (3)
       CALL ST_NUMB ( substr (4), Nmodel, ier)
       write(*,*) ensname, Nmodel
      end if
C     
C     READ REST OF THIS CONTROL-FILE GROUP
C     TO GET NUMBER OF THINGS TO BE VERIFIED
C     
      CALL readcntl(numodel,numfcst,numvfdate,numvfyobs,numarea,numstat,
     +            numvarbl,numlevel,numvector)
 
      write(*,*) 'CALL readcntl done'
   
      READ (5, '(A)',END=3000) cloud_sfc
      READ (5, '(A)',END=3000) lat_weight

      write(*,*) 'cloud_sfc=',cloud_sfc,' lat_weight=',lat_weight

      if(imodel.eq.1) then

         do n=1,numfcst
          do nt=1,4
           yyfcst2(nt,n)=yyyyfcst2(nt,n)-(yyyyfcst2(nt,n)/100)*100
           yyobsv2(nt,n)=yyyyobsv2(nt,n)-(yyyyobsv2(nt,n)/100)*100
          end do
         end do

         do n=1,numfcst
          yyfcst(n)=yyyyfcst(n)-(yyyyfcst(n)/100)*100
          yyobsv(n)=yyyyobsv(n)-(yyyyobsv(n)/100)*100

!!        write(*,*)'yyfcst(n)=',yyfcst(n),
!!   +            ' yyobsv(n)=',yyobsv(n)
         end do

       

         do n=1, numarea
          write(*,*) 'imax=',imax(n),' jmax=',jmax(n)
         end do

         ngrid = imax(1)*jmax(1)
         write(*,*) 'ngrid=',ngrid
        
         do n=1,numarea
          write(*,*) namarea(n), 'region ID:', numreg(n)
         end do

!!       do n=1,numvarbl
c          write(*,*) trim(namvarbl(n)),
c     +   ' k5,k6,k7,ck7=', k5(n),k6(n),k7(n),ck7(n),
c     +   ' tendencymrk=',tendencymrk(n),' fhomrk=',fhomrk(n),
c     +   ' vectormrk=',vectormrk(n), 
c     +   ' tendencymrk=', tendencymrk(n),
c     +   ' updown=', (updown(n,k),k=1,fhomrk(n))                                                           

!!        write(*,*) trim(namvarbl(n)),
!!   +     ' k5,k6,k7,ck7=', k5(n),k6(n),k7(n),
!!   +     'wavemrk=', wavemrk(n),wv1(n),wv2(n)
!!       end do


        !only work on p level mode 1
        do n=1, numlevel
         if(modelvl(n).eq.2) then
           write(*,*) 'P level mode=2 (layer) is not done yet'
           stop
         else
          if(numlevel.ge.1) then
            plevel(n)=iplevel(n,1)
            write(*,*) 'plevel(n)=',plevel(n)
          else
            plevel(1)=0
          end if
         end if
        end do


      !Determine if need weighting grid area for different latitude
      !Modified by Fanglin Yang 
       grid_area = 0.0
        if (trim(lat_weight) .eq. "yes") then
          do i = 1,ngrid
c           grid_area(i) = cos (region_latlon(1,i)/180.0)
           grid_area(i)=cos(datan(1.0d0)*region_latlon(1,i)/45.0)
          end do
          area_ref = maxval(grid_area)
          write(*,*) 'area_ref =', area_ref 
          if (area_ref.eq.0.0) stop 2222
           do i = 1,ngrid
c            area_factor(i) = cos(region_latlon(1,i)/180.0)/area_ref
            area_factor(i) = grid_area(i)
           end do
        else
           area_factor = 1.0
        end if

        write(*,*) 'STEP 1 done'

C    STEP 2. READ DATA FROM Thined GRIB Forecast file and Thined GRIB Observation file

        if (Nmodel.gt.1) then    !if is ensemble verifiy
         allocate (fcstmdl(Nmodel,numfcst,numvarbl,numlevel,ngrid))
         allocate (ufcstmdl(Nmodel,numfcst,numvarbl,numlevel,ngrid))
         allocate (vfcstmdl(Nmodel,numfcst,numvarbl,numlevel,ngrid))
         write(*,*) 'allocate fcstmdl done'
        end if


          allocate(fcstdata(numfcst,numvarbl,numlevel,ngrid))
          allocate(obsvdata(numfcst,numvarbl,numlevel,ngrid))

          allocate(climdata(numfcst,numvarbl,numlevel,ngrid))
          allocate(uclim(numfcst,numvarbl,numlevel,ngrid))
          allocate(vclim(numfcst,numvarbl,numlevel,ngrid))


          allocate(ufcst(numfcst,numvarbl,numlevel,ngrid))
          allocate(vfcst(numfcst,numvarbl,numlevel,ngrid))
          allocate(uobsv(numfcst,numvarbl,numlevel,ngrid))
          allocate(vobsv(numfcst,numvarbl,numlevel,ngrid))
 
          

c          fgrbfile='fcst.grib'      
c          finxfile='fcst.indx' 

           ogrbfile='obsv.grib.'//trim(namodel(1))   !add model name to deal with ensemble system
           oinxfile='obsv.indx.'//trim(namodel(1))   !add model name to deal with ensemble system

           allocate(fcstdata2(numfcst,numvarbl,numlevel,ngrid))
           allocate(obsvdata2(numfcst,numvarbl,numlevel,ngrid))
                                                                                                                                                      
           allocate(ufcst2(numfcst,numvarbl,numlevel,ngrid))
           allocate(vfcst2(numfcst,numvarbl,numlevel,ngrid))
           allocate(uobsv2(numfcst,numvarbl,numlevel,ngrid))
           allocate(vobsv2(numfcst,numvarbl,numlevel,ngrid))
 
           allocate(HGTsfc(ngrid))

        End if  !end of (if imodel=1)

          hasdata=1
 
          fgrbfile='fcst.grib.'//trim(namodel(1))
          finxfile='fcst.indx.'//trim(namodel(1))
          ifgrb=10+imodel
          ifinx=100+imodel

        
    !1: Read forecast data
                                                                                                                                            
        call getGRIBdata (ifgrb,ifinx,
     +       fgrbfile,finxfile,
     +       fcstdata,ufcst,vfcst,levels,
     +       numfcst,numvarbl,numlevel,ngrid,
     +       yyfcst,mmfcst,ddfcst,hhfcst,fffcst,k5,k6,k7,
     +       plevel,namvarbl)


        !get Sfc height for cloud base/top computation
        HGTsfc=0.0
        if(trim(cloud_sfc).eq.'no') then    !cloud base/top from sea level
         do n=1,numvarbl
          if(k5(n).eq.7.and.(k6(n).eq.2.or.k6(n).eq.3)) then
           call getHGTsfc(HGTsfc, ngrid)
           goto 111   
          end if
         end do
111      continue
         !compute the cloud base/top
         do n=1,numvarbl
           if(k5(n).eq.7.and.(k6(n).eq.2.or.k6(n).eq.3)) then
            do ng=1,ngrid
             fcstdata(:,n,:,ng)=fcstdata(:,n,:,ng)-HGTsfc(ng)
            end do
           end if
         end do
        end if


        

        write(*,*) 'read fcst grib done'

    !2:read observation data
        
       if(imodel.eq.1 ) then
         call getGRIBdata (42,43,
     +       ogrbfile,oinxfile,
     +       obsvdata,uobsv,vobsv,levels,
     +       numfcst,numvarbl,numlevel,ngrid,
     +       yyobsv,mmobsv,ddobsv,hhobsv,ffobsv,k5,k6,k7,
     +       plevel,namvarbl)

             write(*,*) 'read obsv grib done'
       end if

        !Addition: read fcast and obsv GRIB files for tendency 
        ! get tendency GRIB fcst and obsv data
        tnd_has3=0
        tnd_has6=0
        tnd_has12=0
        tnd_has24=0
        do n=1,numvarbl
         if(dt(n).eq.3) tnd_has3=1
         if(dt(n).eq.6) tnd_has6=1
         if(dt(n).eq.12) tnd_has12=1
         if(dt(n).eq.24) tnd_has24=1
        end do

!!     write(*,*) 'tnd_has=',tnd_has3,tnd_has6,tnd_has12,tnd_has24 

        if(tnd_has3.eq.1) then
          fgrbfile2='fcst03.grib'
          finxfile2='fcst03.indx'
          ogrbfile2='obsv03.grib'
          oinxfile2='obsv03.indx'
          call gettndGRIBdata (44,45,fgrbfile2,finxfile2,
     +       fcstdata2,ufcst2,vfcst2,levels,
     +       numfcst,numvarbl,numlevel,ngrid,
     +       yyfcst2(1,:),mmfcst2(1,:),ddfcst2(1,:),
     +       hhfcst2(1,:),fffcst2(1,:),k5,k6,k7,plevel,namvarbl,
     +       tendencymrk)
     
          call get_hasdata(fcstdata2,numfcst,numvarbl,numlevel,
     +               ngrid,levels,tendencymrk,3,hasdata)

          do n=1,numvarbl                    !for cloud base/top from sea level
           if(k5(n).eq.7.and.(k6(n).eq.2.or.k6(n).eq.3)) then
            do ng=1,ngrid
             fcstdata2(:,n,:,ng)=fcstdata2(:,n,:,ng)-HGTsfc(ng)
            end do
           end if
          end do


          call getTndGRIBdata (46,47,ogrbfile2,oinxfile2,
     +       obsvdata2,uobsv2,vobsv2,levels,
     +       numfcst,numvarbl,numlevel,ngrid,
     +       yyobsv2(1,:),mmobsv2(1,:),ddobsv2(1,:),
     +       hhobsv2(1,:),ffobsv2(1,:),k5,k6,k7,plevel,namvarbl,
     +       tendencymrk)
          call get_hasdata(obsvdata2,numfcst,numvarbl,numlevel,
     +               ngrid,levels,tendencymrk,3,hasdata)
 
          do i=1,numfcst
           do j=1,numvarbl
            if(tendencymrk(j).eq.3) then
             do k=1,levels(j)
              fcstdata(i,j,k,:)=fcstdata(i,j,k,:)-
     +                         fcstdata2(i,j,k,:)
              obsvdata(i,j,k,:)=obsvdata(i,j,k,:)-
     +                         obsvdata2(i,j,k,:)
             end do
            end if
           end do
         end do

           write(*,*) 'read grib03 done'
         end if

         if (tnd_has6.eq.1) then
          fgrbfile2='fcst06.grib'
          finxfile2='fcst06.indx'
          ogrbfile2='obsv06.grib'
          oinxfile2='obsv06.indx'
          call getTndGRIBdata (48,49,fgrbfile2,finxfile2,
     +       fcstdata2,ufcst2,vfcst2,levels,
     +       numfcst,numvarbl,numlevel,ngrid,
     +       yyfcst2(2,:),mmfcst2(2,:),ddfcst2(2,:),
     +       hhfcst2(2,:),fffcst2(2,:),k5,k6,k7,plevel,namvarbl,
     +       tendencymrk)


          call get_hasdata(fcstdata2,numfcst,numvarbl,numlevel,
     +               ngrid,levels, tendencymrk,6, hasdata)


          do n=1,numvarbl                    !for cloud base/top from sea level
           if(k5(n).eq.7.and.(k6(n).eq.2.or.k6(n).eq.3)) then
            do ng=1,ngrid
             fcstdata2(:,n,:,ng)=fcstdata2(:,n,:,ng)-HGTsfc(ng)
            end do
           end if
          end do


          call getTndGRIBdata (50,51,ogrbfile2,oinxfile2,
     +       obsvdata2,uobsv2,vobsv2,levels,
     +       numfcst,numvarbl,numlevel,ngrid,
     +       yyobsv2(2,:),mmobsv2(2,:),ddobsv2(2,:),
     +       hhobsv2(2,:),ffobsv2(2,:),k5,k6,k7,plevel,namvarbl,
     +       tendencymrk)
    
          call get_hasdata(obsvdata2,numfcst,numvarbl,numlevel,
     +               ngrid,levels, tendencymrk,6, hasdata)


          do i=1,numfcst
           do j=1,numvarbl
            if(tendencymrk(j).eq.6) then
             do k=1,levels(j)
              fcstdata(i,j,k,:)=fcstdata(i,j,k,:)-
     +                         fcstdata2(i,j,k,:)
              obsvdata(i,j,k,:)=obsvdata(i,j,k,:)-
     +                         obsvdata2(i,j,k,:)
             end do
            end if
           end do
         end do
           write(*,*) 'read grib06 done'
        end if

        if (tnd_has12.eq.1) then
          fgrbfile2='fcst12.grib'
          finxfile2='fcst12.indx'
          ogrbfile2='obsv12.grib'
          oinxfile2='obsv12.indx'
          call getTndGRIBdata (52,53,fgrbfile2,finxfile2,
     +       fcstdata2,ufcst2,vfcst2,levels,
     +       numfcst,numvarbl,numlevel,ngrid,
     +       yyfcst2(3,:),mmfcst2(3,:),ddfcst2(3,:),
     +       hhfcst2(3,:),fffcst2(3,:),k5,k6,k7,plevel,namvarbl,
     +       tendencymrk)

          call get_hasdata(fcstdata2,numfcst,numvarbl,numlevel,
     +               ngrid,levels,tendencymrk,12, hasdata)


          do n=1,numvarbl                    !for cloud base/top from sea level
           if(k5(n).eq.7.and.(k6(n).eq.2.or.k6(n).eq.3)) then
            do ng=1,ngrid
             fcstdata2(:,n,:,ng)=fcstdata2(:,n,:,ng)-HGTsfc(ng)
            end do
           end if
          end do


          call getTndGRIBdata (54,55,ogrbfile2,oinxfile2,
     +       obsvdata2,uobsv2,vobsv2,levels,
     +       numfcst,numvarbl,numlevel,ngrid,
     +       yyobsv2(3,:),mmobsv2(3,:),ddobsv2(3,:),
     +       hhobsv2(3,:),ffobsv2(3,:),k5,k6,k7,plevel,namvarbl,
     +       tendencymrk)

          call get_hasdata(obsvdata2,numfcst,numvarbl,numlevel,
     +               ngrid,levels,tendencymrk,12, hasdata)



          do i=1,numfcst
           do j=1,numvarbl
            if(tendencymrk(j).eq.12) then
 
             do k=1,levels(j)
              fcstdata(i,j,k,:)=fcstdata(i,j,k,:)-
     +                         fcstdata2(i,j,k,:)
              obsvdata(i,j,k,:)=obsvdata(i,j,k,:)-
     +                         obsvdata2(i,j,k,:)
             end do

            end if
           end do
          end do

          write(*,*) 'read grib12 done'
               
         end if
 
       
        if (tnd_has24.eq.1) then
          fgrbfile2='fcst24.grib'
          finxfile2='fcst24.indx'
          ogrbfile2='obsv24.grib'
          oinxfile2='obsv24.indx'
          call getTndGRIBdata (56,57,fgrbfile2,finxfile2,
     +       fcstdata2,ufcst2,vfcst2,levels,
     +       numfcst,numvarbl,numlevel,ngrid,
     +       yyfcst2(4,:),mmfcst2(4,:),ddfcst2(4,:),
     +       hhfcst2(4,:),fffcst2(4,:),k5,k6,k7,plevel,namvarbl,
     +       tendencymrk)

          call get_hasdata(fcstdata2,numfcst,numvarbl,numlevel,
     +               ngrid,levels, tendencymrk,24, hasdata)


          do n=1,numvarbl                    !for cloud base/top from sea level
           if(k5(n).eq.7.and.(k6(n).eq.2.or.k6(n).eq.3)) then
            do ng=1,ngrid
             fcstdata2(:,n,:,ng)=fcstdata2(:,n,:,ng)-HGTsfc(ng)
            end do
           end if
          end do


          call getTndGRIBdata (58,59,ogrbfile2,oinxfile2,
     +       obsvdata2,uobsv2,vobsv2,levels,
     +       numfcst,numvarbl,numlevel,ngrid,
     +       yyobsv2(4,:),mmobsv2(4,:),ddobsv2(4,:),
     +       hhobsv2(4,:),ffobsv2(4,:),k5,k6,k7,plevel,namvarbl,
     +       tendencymrk)
       
          call get_hasdata(obsvdata2,numfcst,numvarbl,numlevel,
     +               ngrid,levels, tendencymrk,24, hasdata)
 
          do i=1,numfcst
           do j=1,numvarbl
            if(tendencymrk(j).eq.24) then
             do k=1,levels(j)
              fcstdata(i,j,k,:)=fcstdata(i,j,k,:)-
     +                         fcstdata2(i,j,k,:)
              obsvdata(i,j,k,:)=obsvdata(i,j,k,:)-
     +                         obsvdata2(i,j,k,:)
             end do
            end if
           end do
          end do

         write(*,*) 'read grib24 done'
        end if

        do i=1,numfcst
         do j=1,numvarbl
           do k=1,levels(j)
            if(hasdata(i,j,k).eq.0) then
             fcstdata(i,j,k,:)=-1.0e3
             obsvdata(i,j,k,:)=-1.0e3
            end if
           end do    
          end do    
         end do    



cccccccccccccccccccccccccccccccccccccccccccccccccccccc
c     Filting with wave, in this case, only global
c     grib got filtered.
 
      allocate( fcst2d(imax(1),jmax(1)) )
      allocate( obsv2d(imax(1),jmax(1)) )

      write(*,*) 'Wave filting data ....'
      do nfcst = 1, numfcst
       do nvr=1,numvarbl

        if(wavemrk(nvr).eq.2 .or. wavemrk(nvr).eq.1 ) then

         do nlvl = 1, levels(nvr)
  
!!        write(*,*)'FCST:', nfcst,', wave var ',nvr,' at nlvl ', nlvl   

          do j = 1, jmax(1)
          do i = 1, imax(1)
           ij=(j-1)*imax(1) + i
           fcst2d(i,j)=fcstdata(nfcst,nvr,nlvl,ij)
           obsv2d(i,j)=obsvdata(nfcst,nvr,nlvl,ij)
          end do
          end do

 
!!        write(*,*) 'Before wave:nlvl=',nlvl, wv1(nvr),'-',wv2(nvr)
!!        write(*,'(10f10.2)')(fcstdata(nfcst,nvr,nlvl,ij),ij=1,10)      
!!        write(*,'(10f10.2)')(obsvdata(nfcst,nvr,nlvl,ij),ij=1,10)      
 
          if(wavemrk(nvr).eq.2) then 
            call FITWAV_2D(fcst2d,imax(1),jmax(1),wv1(nvr),wv2(nvr),0)
            call FITWAV_2D(obsv2d,imax(1),jmax(1),wv1(nvr),wv2(nvr),0)
          else if (wavemrk(nvr).eq.1) then
            call FITWAV_1D(fcst2d,imax(1),jmax(1),wv1(nvr),wv2(nvr))
            call FITWAV_1D(obsv2d,imax(1),jmax(1),wv1(nvr),wv2(nvr))
          end if

          do j = 1, jmax(1)
          do i = 1, imax(1)
           ij=(j-1)*imax(1) + i
           fcstdata(nfcst,nvr,nlvl,ij)=fcst2d(i,j)        
           obsvdata(nfcst,nvr,nlvl,ij)=obsv2d(i,j)        
          end do
          end do

!!        write(*,*) 'After wave'
!!        write(*,'(10f10.2)')(fcstdata(nfcst,nvr,nlvl,ij),ij=1,10)
!!        write(*,'(10f10.2)')(obsvdata(nfcst,nvr,nlvl,ij),ij=1,10)


          !if wind speed, U and V components also needs to filtered
          if(k5(nvr).eq.32) then
           !U-component
           do j = 1, jmax(1)
           do i = 1, imax(1)
            ij=(j-1)*imax(1) + i
            fcst2d(i,j)=ufcst(nfcst,nvr,nlvl,ij)
            obsv2d(i,j)=uobsv(nfcst,nvr,nlvl,ij)
           end do
           end do

!!          write(*,*) 'U Before wave:nlvl=',nlvl
!!          write(*,'(10f10.2)')(ufcst(nfcst,nvr,nlvl,ij),ij=1,10)
!!          write(*,'(10f10.2)')(uobsv(nfcst,nvr,nlvl,ij),ij=1,10)

          if(wavemrk(nvr).eq.2) then
            call FITWAV_2D(fcst2d,imax(1),jmax(1),wv1(nvr),wv2(nvr),0)
            call FITWAV_2D(obsv2d,imax(1),jmax(1),wv1(nvr),wv2(nvr),0)
          else if (wavemrk(nvr).eq.1) then
            call FITWAV_1D(fcst2d,imax(1),jmax(1),wv1(nvr),wv2(nvr))
            call FITWAV_1D(obsv2d,imax(1),jmax(1),wv1(nvr),wv2(nvr))
          end if

                                                                                                                   
           do j = 1, jmax(1)
           do i = 1, imax(1)
            ij=(j-1)*imax(1) + i
            ufcst(nfcst,nvr,nlvl,ij)=fcst2d(i,j)
            uobsv(nfcst,nvr,nlvl,ij)=obsv2d(i,j)
           end do
           end do

!!          write(*,*) 'U After wave'
!!          write(*,'(10f10.2)')(ufcst(nfcst,nvr,nlvl,ij),ij=1,10)
!!          write(*,'(10f10.2)')(uobsv(nfcst,nvr,nlvl,ij),ij=1,10)

           !V-component
           do j = 1, jmax(1)
           do i = 1, imax(1)
            ij=(j-1)*imax(1) + i
            fcst2d(i,j)=vfcst(nfcst,nvr,nlvl,ij)
            obsv2d(i,j)=vobsv(nfcst,nvr,nlvl,ij)
           end do
           end do
 
!!          write(*,*) 'V Before wave:'
!!          write(*,'(10f10.2)')(vfcst(nfcst,nvr,nlvl,ij),ij=1,10)
!!          write(*,'(10f10.2)')(vobsv(nfcst,nvr,nlvl,ij),ij=1,10)
                                                                                                                  
           
          if(wavemrk(nvr).eq.2) then
            call FITWAV_2D(fcst2d,imax(1),jmax(1),wv1(nvr),wv2(nvr),0)
            call FITWAV_2D(obsv2d,imax(1),jmax(1),wv1(nvr),wv2(nvr),0)
          else if (wavemrk(nvr).eq.1) then
            call FITWAV_1D(fcst2d,imax(1),jmax(1),wv1(nvr),wv2(nvr))
            call FITWAV_1D(obsv2d,imax(1),jmax(1),wv1(nvr),wv2(nvr))
          end if
                                                                                                        
           do j = 1, jmax(1)
           do i = 1, imax(1)
            ij=(j-1)*imax(1) + i
            vfcst(nfcst,nvr,nlvl,ij)=fcst2d(i,j)
            vobsv(nfcst,nvr,nlvl,ij)=obsv2d(i,j)
           end do
           end do

!!          write(*,*) 'V After wave'
!!          write(*,'(10f10.2)')(vfcst(nfcst,nvr,nlvl,ij),ij=1,10)
!!          write(*,'(10f10.2)')(vobsv(nfcst,nvr,nlvl,ij),ij=1,10)

           end if  !end of wind
          end do   !end of nlvl
         end if    !end of wavemrk

       end do      !end of nvr
      end do       !end of nfcst


cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc


        write(*,*) 'STEP 2 done'


C    STEP 3. DO FVS Computation
   
 
       nvsdb = 500
       open (nvsdb, file='grid2grid.vsdb', status='unknown')

       getClimData_called = 0      

       if (Nmodel.le.1) then !non-ensemble  >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

        do 1001 ist =1, numstat

         write(*,*) namstat(ist)

c      ----- stats without anomly --------------------------------------
         if(trim(namstat(ist)).eq.'SL1L2') then
           call SL1L2(nvsdb, imodel,ist,numfcst,numvfyobs,numarea,
     +          numvarbl,numlevel,ngrid,levels,hasdata,
     +          fcstdata, obsvdata)
           write(*,*) 'SL1L2 done'
         end if

         if(trim(namstat(ist)).eq.'VL1L2') then
           call VL1L2(nvsdb,imodel, ist,numfcst,numvfyobs,numarea,
     +          numvarbl,numlevel,ngrid,levels,plevel,hasdata,
     +          ufcst,vfcst, uobsv,vobsv)
            write(*,*) 'VL1L2 done'
         end if

         if(trim(namstat(ist)).eq.'FHO') then
           call gtFHO(nvsdb, imodel,ist,numfcst,numvfyobs,numarea,
     +          numvarbl,numlevel,ngrid,levels,tendencymrk,updown,
     +          hasdata,fcstdata, obsvdata)
           write(*,*) 'FHO done'
        end if

c -------------- end of stats withou anomly -------------------------------


c        ------ stats with anomly ---------------------------------------

         if(trim(namstat(ist)).eq.'AFHO') then
           call gtAFHO(nvsdb, imodel,ist,numfcst,numvfyobs,numarea,
     +          numvarbl,numlevel,ngrid,levels,tendencymrk,updown,
     +          hasdata,fcstdata, obsvdata)
           write(*,*) 'AFHO done'
         end if

 
        if(trim(namstat(ist)).eq.'SAL1L2'.or.
     +     trim(namstat(ist)).eq.'VAL1L2') then

          !Step 1: set anomlylev marks for all levels
          call getAnomlyLevMrk(k5,k6,anomly_mrk,plevel,
     +       numvarbl,numlevel,anomlylev)

          !step 2: read climatologic data

          if(getClimData_called .eq. 0) then
           call getAnomlyLevMrk(k5,k6,anomly_mrk,plevel,
     +       numvarbl,numlevel,anomlylev)
           
!!         write(*,*) 'AnomlyLevMrk:'
!!         do ivr = 1,numvarbl
!!          write(*,*)'ivr=',ivr,':',
!!   +          (anomlylev(ivr,k),k=1,numlevel)
!!         end do 
           write(*,*) ' getAnomlyLevMrk done'

           call getMeanClimData(climdata,uclim,vclim,
     +       levels,numfcst,numvfyobs,numvarbl,numlevel,ngrid,
     +       yyobsv,mmobsv,ddobsv,hhobsv,ffobsv,k5,k6,k7,
     +       plevel,namvarbl,anomly_mrk,anomlylev,
     +       cmmobsv,cddobsv)
           write(*,*) ' getMeanClimData done'

           !if any variable is WAVED, its climatological data also needs waved

           allocate( clim2d(imax(1),jmax(1)) )

           write(*,*) 'Wave filter climatologic data ....'

           do 1002 nfcst = 1, numfcst 

            do 1003  nvr=1,numvarbl

             if(wavemrk(nvr).eq.2 .or. wavemrk(nvr).eq.1) then

              do nlvl = 1, levels(nvr)

              if(anomlylev(nvr,nlvl).eq.1) then      

!!         write(*,*)'FCST:', nfcst,' wave clim var ',nvr,' at nlvl ',nlvl 

               do j = 1, jmax(1)
               do i = 1, imax(1)
                ij=(j-1)*imax(1) + i
                clim2d(i,j)=climdata(nfcst,nvr,nlvl,ij)
               end do
               end do

               
!!            write(*,*) 'Before wave anomly:nlvl=',nlvl, wv1(nvr),'-',wv2(nvr)
!!            write(*,'(10f10.2)')(climdata(nfcst,nvr,nlvl,ij),ij=1,10)
              

             if(wavemrk(nvr).eq.2) then
              call FITWAV_2D(clim2d,imax(1),jmax(1),wv1(nvr),wv2(nvr),0)
             else if (wavemrk(nvr).eq.1) then
              call FITWAV_1D(clim2d,imax(1),jmax(1),wv1(nvr),wv2(nvr))
             end if

               do j = 1, jmax(1)
               do i = 1, imax(1)
               ij=(j-1)*imax(1) + i
                climdata(nfcst,nvr,nlvl,ij)=clim2d(i,j)
               end do
               end do

!!            write(*,*) 'After wave'
!!            write(*,'(10f10.2)')(climdata(nfcst,nvr,nlvl,ij),ij=1,10)

              !if wind speed, U and V components also needs to filtered
              if(k5(nvr).eq.32) then

               !U-component
               do j = 1, jmax(1)
               do i = 1, imax(1)
                ij=(j-1)*imax(1) + i
                clim2d(i,j)=uclim(nfcst,nvr,nlvl,ij)
               end do
               end do

!!             write(*,*) 'Before anomly U wave:nlvl=',nlvl
!!             write(*,'(10f10.2)')(uclim(nfcst,nvr,nlvl,ij),ij=1,10)

               if(wavemrk(nvr).eq.2) then
             call FITWAV_2D(clim2d,imax(1),jmax(1),wv1(nvr),wv2(nvr),0)
               else if (wavemrk(nvr).eq.1) then
             call FITWAV_1D(clim2d,imax(1),jmax(1),wv1(nvr),wv2(nvr))
               end if

               do j = 1, jmax(1)
               do i = 1, imax(1)
                ij=(j-1)*imax(1) + i
                uclim(nfcst,nvr,nlvl,ij)=clim2d(i,j)
               end do
               end do

!!             write(*,*) 'After anomly U' 
!!             write(*,'(10f10.2)')(uclim(nfcst,nvr,nlvl,ij),ij=1,10)

               !V-component
               do j = 1, jmax(1)
               do i = 1, imax(1)
                ij=(j-1)*imax(1) + i
                clim2d(i,j)=vclim(nfcst,nvr,nlvl,ij)
               end do
               end do

!!             write(*,*) 'Before anomly V wave:nlvl=',nlvl
!!             write(*,'(10f10.2)')(uclim(nfcst,nvr,nlvl,ij),ij=1,10)

               if(wavemrk(nvr).eq.2) then
            call FITWAV_2D(clim2d,imax(1),jmax(1),wv1(nvr),wv2(nvr),0)
               else if (wavemrk(nvr).eq.1) then
            call FITWAV_1D(clim2d,imax(1),jmax(1),wv1(nvr),wv2(nvr))
               end if

               do j = 1, jmax(1)
               do i = 1, imax(1)
                ij=(j-1)*imax(1) + i
                vclim(nfcst,nvr,nlvl,ij)=clim2d(i,j)
               end do
               end do

!!             write(*,*) 'After anomly V'
!!             write(*,'(10f10.2)')(uclim(nfcst,nvr,nlvl,ij),ij=1,10)

              end if      !end of wind speed
 
             end if    ! end of anomlylev
             end do    ! end of nlvl
             end if    ! end of if (wavemrk(nvr)=1 or wavemrk(nvr)=2   

!!            write(*,*) 'Climdata waved for var ', nvr

1003       continue !end of nvr

1002      continue  !end of nfcst

           getClimData_called = 1
           deallocate(clim2d)

          end if  !if getClimData_called = 0  
 
           !step 3: 
            if(trim(namstat(ist)).eq.'SAL1L2') then
             call SAL1L2(nvsdb, imodel,ist,numfcst,numvfyobs,numarea,
     +          numvarbl,numlevel,ngrid,levels,hasdata,
     +          fcstdata, obsvdata,climdata,tendencymrk)
             write(*,*) 'SAL1L2 done'
            end if

            if(trim(namstat(ist)).eq.'VAL1L2') then
              call VAL1L2(nvsdb,imodel,ist,numfcst,numvfyobs,numarea,
     +          numvarbl,numlevel,ngrid,levels,plevel,hasdata,
     +          ufcst,vfcst, uobsv,vobsv,uclim,vclim,tendencymrk)
             write(*,*) 'VAL1L2 done'
            end if

         end if  !end of if is SAL1L2/VAL1L2

c ----------------------------- end of stats with anomly --------------------

1001    continue  !end of ist    

       else     !Ensemble verification >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

        write(*,*) 'Ensemble member',imodel, '.....'
        write(*,*) '                                         '
        write(*,*) '                                         '

        fcstmdl(imodel,:,:,:,:)=fcstdata(:,:,:,:)     !first save forecast data of each models 

        if(k5(numvarbl).eq.32) then
         ufcstmdl(imodel,:,:,:,:)=ufcst(:,:,:,:)
         vfcstmdl(imodel,:,:,:,:)=vfcst(:,:,:,:)

        end if

        if(imodel .eq. Nmodel) then             ! After all of model's data saved, 
                                                ! then begin to compute partial sum
          do 2001 ist = 1, numstat 

            if(trim(namstat(ist)).eq.'ESL1L2') then
   
              call ESL1L2(nvsdb,Nmodel,ensname,ist,numfcst,numvfyobs,
     +        numarea,numvarbl,numlevel,ngrid,levels,
     +        hasdata,fcstmdl,obsvdata)

              write(*,*) 'call ESL1L2 done!'

            end if ! end if 'ESL1L2'

            if(trim(namstat(ist)).eq.'EVL1L2') then
 
              call EVL1L2(nvsdb,Nmodel,ensname,ist,numfcst,
     +        numvfyobs,numarea, numvarbl,numlevel,ngrid,levels,
     +        plevel,hasdata,ufcstmdl,vfcstmdl,uobsv,vobsv)

              write(*,*) 'call EVL1L2 done!'

            end if

            if(trim(namstat(ist)).eq.'RHNT') then
             
             call RHNT(nvsdb,Nmodel,ensname,ist,numfcst,
     +       numvfyobs,numarea,numvarbl,numlevel,ngrid,levels,
     +       hasdata,fcstmdl,obsvdata,ufcstmdl,vfcstmdl,
     +       uobsv,vobsv)

             write(*,*) 'call RHNT done!'

            end if

            if(trim(namstat(ist)).eq.'RHET') then
 
             call RHET(nvsdb,Nmodel,ensname,ist,numfcst,
     +       numvfyobs,numarea,numvarbl,numlevel,ngrid,levels,
     +       hasdata,fcstmdl,obsvdata,ufcstmdl,vfcstmdl,
     +       uobsv,vobsv)
 
             write(*,*) 'call RHET done!'
 
            end if


2001      continue

        end if              

           imodel = imodel + 1

       end if  ! end if non-ensemble | ensemble
       
          if (imodel.le.Nmodel) goto 2000

          if(allocated(fcstdata)) deallocate(fcstdata)
          if(allocated(obsvdata)) deallocate(obsvdata)
          if(allocated(ufcst)) deallocate(ufcst)
          if(allocated(vfcst)) deallocate(vfcst)
          if(allocated(uobsv)) deallocate(uobsv)
          if(allocated(vobsv)) deallocate(vobsv)
 

          if(allocated(fcstdata2)) deallocate(fcstdata2)
          if(allocated(obsvdata2)) deallocate(obsvdata2)
          if(allocated(ufcst2)) deallocate(ufcst2)
          if(allocated(vfcst2)) deallocate(vfcst2)
          if(allocated(uobsv2)) deallocate(uobsv2)
          if(allocated(vobsv2)) deallocate(vobsv2)

          if(allocated(climdata)) deallocate(climdata)
          if(allocated(uclim)) deallocate(uclim)
          if(allocated(vclim)) deallocate(vclim)
        
          if(allocated(fcst2d)) deallocate(fcst2d)
          if(allocated(obsv2d)) deallocate(obsv2d)
 
          if(allocated(fcstmdl)) deallocate(fcstmdl)
          if(allocated(ufcstmdl)) deallocate(ufcstmdl)
          if(allocated(vfcstmdl)) deallocate(vfcstmdl)

          write(*,*) 'STEP 3 done'
          goto 2000

3000      CONTINUE
          STOP
          END


