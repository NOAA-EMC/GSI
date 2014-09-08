C******************************************************************
C
C  BRIEF DESCRIPTION OF PROGRAM MODULES:
C
C   backspaceFile - Repositions the specified file back "numRcrds" records.
C   buildDvorak - Builds a long or short term trend dvorak code.
C   convToAiddata - Create an AID_DATA record using data passed in.
C   convToARecord - Create an A_RECORD record using data passed in.
C   convWriteAidData - Convert the passed data to an AID_DATA record and 
C              write it to the specified output file.
C   doReadBT - Read one record from the best track file.  Same as readBT 
C              except it can read a multi-line best track record, reads
C              10 digit dtg and reads the storm-type.
C   doWriteAidRcd - Write the aid record to the output data file.
C   doWriteErrRadRcd - Write the error radius record to the output data file.
C   getAidDTG - Gets the first aid data for the specified DTG from
C              the input file.
C   getAidTAU - Gets the first A_RECORD record for the specified tau
C              from the supplied AID_DATA.  
C   getARecord - Reads one record of specified type from the input file.
C   getBigAidDTG - Gets all the aid data for the specified DTG from
C                  the input file
C   getSingleTAU - Gets the aid data for the specified tau from the
C              supplied AID_DATA.
C   getTech - Gets data for a specified aid technique from the supplied
C             BIG_AID_DATA structure and returns an AID_DATA structure
C   newWriteAidRcd - Write the aid record to the output data file.
C                    NHC version
C   newWriteErrRadRcd - Write the error radius record to the output data file.
C                    NHC version
C   outputAircraft - Build the aircraft section of the fix record
C   outputAnalysis - Build the analysis section of the fix record
C   outputArcd - write the A_RECORD to the specified file stream.
C   outputDropsonde - Build the dropsonde section of the fix record
C   outputDVTO - Build the objective dvorak section of the fix record
C   outputDVTS - Build the subjective dvorak section of the fix record
C   outputFrcd - Write the F_RECORD to the spectified file.
C   outputMicrowave - Build the microwave section of the fix record
C   outputRadar - Build the radar section of the fix record.
C   processAircraft - Assigns the data for an aircraft fix record.
C   processAnalysis - Assigns the data for an analysis fix record.
C   processArcd - Assigns the data for a A_RECORD structure.
C   processDropsonde - Assigns the data for a dropsonde fix record.
C   processDvorak - Assigns the data for a DVORAK fix structure.
C   processDVTO - Assigns the data for an objective dvorak fix record
C   processDVTS - Assigns the data for a subjective dvorak fix record.
C   processFrcd - Assigns the data for an F_RECORD structure.
C   processMicrowave - Assigns the data for a microwave fix record.
C   processRadar - Assigns the data for a radar fix record.
C   readARecord - Reads one AID_DATA data info from the input file.
C   readBest - Read one record from the best track file,
C              including the basin and cyclone number.
C   readBestTrk - Read one record from the best track file.
C              Same as readBest except it can read multi-line best track files.
C   readBT   - Read one record from the best track file.
C   readBTrk - Read one record from the best track file.
C              Same as readBT except it can read multi-line best track files.
C   readFRecord - Reads one FIX_DATA data info from the input file.
C   readNext - Reads the next ATCF_RECORD record from the input file.
C   readNextFix - Reads the next FIX_RECORD record from the input file.
C   writeAid - Write the aid record to the output data file.
C   writeAidData - Write an AID_DATA record to the specified file stream.
C   writeAidRcd - Write the aid record to the output data file.
C                JTWC, NPMOC version
C   writeFix - Write a FIX_DATA record to the specified file.
C   
C******************************************************************

C........................START PROLOGUE.................................
C
C  SUBPROGRAM NAME:  readBT
C
C  DESCRIPTION:  Read one record from the best track file.
C
C  PROGRAMMER, DATE:  Ann Schrader   (SAIC)  June 1998
C
C  USAGE:  call readBT (datFile,cent,dtg,flat,ns,flon,ew,iwind,ios)
C
C  INPUT PARAMETERS:  
C     datFile - unit number of best track file
C
C  OUTPUT PARAMETERS:
C     cent - century of posit (2 characters, e.g. 19)
C     dtg - YYMMDDHH, date-time-group read from the best track file
C     flat - latitude read from the best track file
C     ns - one character north/south indicator for latitude
C     flon - longitude read from the best track file
C     ew - one character east/west indicator for longitude
C     iwind - max wind read from the best track file
C     ios - input/output status upon completion or the read operation
C           0 - successful read
C           negative - end-of-file
C           positive - error
C
C  ERROR CONDITIONS:
C
C........................MAINTENANCE SECTION............................
C
C  PRINCIPAL VARIABLES AND ARRAYS:
C
C  METHOD:
C
C  RECORD OF CHANGES:
C   sampson, nrl    nov 9 98   added cent
C
C........................END PROLOGUE..................................
C
      subroutine readBT (datFile,cent,dtg,flat,ns,flon,ew,iwind,ios)   
c
c         formal parameters
      integer           datFile
      character*8       dtg
      character*2       cent
      real              flat, flon
      character*1       ns, ew
      integer           iwind
      integer           ios

      read( datFile, '(8x,a2,a8,17x,f3.1,a1,2x,f4.1,a1,2x,i3)', 
     1     iostat=ios ) cent, dtg, flat, ns, flon, ew, iwind
C
      END


C
C........................START PROLOGUE.................................
C
C  SUBPROGRAM NAME:  readBest
C
C  DESCRIPTION:  Read one record from the best track file.
C                Same as readBT except also reads the basin and 
C                cyclone number.
C
C  PROGRAMMER, DATE:  Ann Schrader   (SAIC)  June 1998
C
C  USAGE:  call readBest (datFile, basin, cycnum, cent, dtg, flat, ns, 
C                           flon, ew, iwind, ios )
C
C  INPUT PARAMETERS:  
C     datFile - unit number of best track file
C
C  OUTPUT PARAMETERS:
C     basin - basin read from the best track file
C     cycnum - cyclone num read from the best track file
C     cent - century (2 characters, e.g. 19)
C     dtg - YYMMDDHH, date-time-group read from the best track file
C     flat - latitude read from the best track file
C     ns - one character north/south indicator for latitude
C     flon - longitude read from the best track file
C     ew - one character east/west indicator for longitude
C     iwind - max wind read from the best track file
C     ios - input/output status upon completion or the read operation
C           0 - successful read
C           negative - end-of-file
C           positive - error
C
C  ERROR CONDITIONS:
C
C........................MAINTENANCE SECTION............................
C
C  PRINCIPAL VARIABLES AND ARRAYS:
C
C  METHOD:
C
C  RECORD OF CHANGES:
C   sampson, nrl    nov 9 98   added cent
C
C........................END PROLOGUE..................................
C
      subroutine readBest (datFile, basin, cycnum, cent, dtg, flat, ns, 
     &     flon, ew, iwind, ios )
c
c         formal parameters
      integer           datFile
      character*2       basin
      character*2       cycnum
      character*8       dtg
      character*2       cent
      real              flat, flon
      character*1       ns, ew
      integer           iwind
      integer           ios

      read( datFile, '(a2,2x,a2,2x,a2,a8,17x,f3.1,a1,2x,f4.1,a1,2x,i3)', 
     1     iostat=ios )basin,cycnum,cent,dtg,flat,ns,flon,ew,iwind
C
      RETURN
      END


C........................START PROLOGUE.................................
C
C  SUBPROGRAM NAME:  doReadBT
C
C  DESCRIPTION:  Read one record from the best track file.
C                Same as readBT except it can read a multi-line 
C                best track record and it reads a 10 digit dtg
C                and it reads the storm-type.
C
C  PROGRAMMER, DATE:  Ann Schrader   (SAIC)  June 1998
C
C  USAGE:  call doReadBT (datFile,dtg,flat,ns,flon,ew,iwind,bttype,ios)
C
C  INPUT PARAMETERS:  
C     datFile - unit number of best track file
C
C  OUTPUT PARAMETERS:
C     dtg - YYYYMMDDHH, date-time-group read from the best track file
C     flat - latitude read from the best track file
C     ns - one character north/south indicator for latitude
C     flon - longitude read from the best track file
C     ew - one character east/west indicator for longitude
C     iwind - max wind read from the best track file
C     bttype - storm development (TD,TS,TY,ST,TC,HU,SD,SS,EX,LO,WV,ET,XX)
C     ios - input/output status upon completion or the read operation
C           0 - successful read
C           negative - end-of-file
C           positive - error
C
C  ERROR CONDITIONS:
C
C........................MAINTENANCE SECTION............................
C
C  PRINCIPAL VARIABLES AND ARRAYS:
C
C  METHOD:
C
C  RECORD OF CHANGES:
C
C........................END PROLOGUE..................................
C
      subroutine doReadBT (datFile,dtg,flat,ns,flon,ew,iwind,bttype,
     &     ios) 

      include 'dataformats.inc'
c
c         formal parameters
      integer           datFile
      character*10       dtg
      character*2       cent
      real              flat, flon
      character*1       ns, ew
      integer           iwind
      character*2       bttype
      integer           ios
c
c         local variables
      type (AID_DATA) btRcd
      integer         readStat

c     Read the next record in the data file.
      call readARecord( datFile, btRcd, readStat )
      if( readStat .eq. 1 ) then
         dtg = btRcd%aRecord(1)%DTG
         flat = btRcd%aRecord(1)%lat
         ns = btRcd%aRecord(1)%NS
         flon = btRcd%aRecord(1)%lon
         ew = btRcd%aRecord(1)%EW
         iwind = btRcd%aRecord(1)%vmax
         bttype = btRcd%aRecord(1)%ty
      endif
C     Switch the returned results from readARecord to results as defined by 
C     FORTRAN read
      if( readStat .lt. 0 ) then
         ios = readStat
      elseif( readStat .eq. 1 ) then
         ios = 0
      elseif( readStat .eq. 0 ) then
         ios = 1
      endif
      
C
      END


C........................START PROLOGUE.................................
C
C  SUBPROGRAM NAME:  readBTrk
C
C  DESCRIPTION:  Read one record from the best track file.
C                Same as readBT except it can read multi-line 
C                best track files.
C
C  PROGRAMMER, DATE:  Ann Schrader   (SAIC)  June 1998
C
C  USAGE:  call readBTrk (datFile,cent,dtg,flat,ns,flon,ew,iwind,ios)
C
C  INPUT PARAMETERS:  
C     datFile - unit number of best track file
C
C  OUTPUT PARAMETERS:
C     cent - century of posit (2 characters, e.g. 19)
C     dtg - YYMMDDHH, date-time-group read from the best track file
C     flat - latitude read from the best track file
C     ns - one character north/south indicator for latitude
C     flon - longitude read from the best track file
C     ew - one character east/west indicator for longitude
C     iwind - max wind read from the best track file
C     ios - input/output status upon completion or the read operation
C           0 - successful read
C           negative - end-of-file
C           positive - error
C
C  ERROR CONDITIONS:
C
C........................MAINTENANCE SECTION............................
C
C  PRINCIPAL VARIABLES AND ARRAYS:
C
C  METHOD:
C
C  RECORD OF CHANGES:
C
C........................END PROLOGUE..................................
C
      subroutine readBTrk (datFile,cent,dtg,flat,ns,flon,ew,iwind,ios)   

      include 'dataformats.inc'
c
c         formal parameters
      integer           datFile
      character*8       dtg
      character*2       cent
      real              flat, flon
      character*1       ns, ew
      integer           iwind
      integer           ios
c
c         local variables
      character*10      dtgnew
      character*2       bttype

      call doReadBT( datFile,dtgnew,flat,ns,flon,ew,iwind,bttype,
     &     ios )
      cent = dtgnew(1:2)
      dtg = dtgnew(3:10)
C
      END


C
C........................START PROLOGUE.................................
C
C  SUBPROGRAM NAME:  readBestTrk
C
C  DESCRIPTION:  Read one record from the best track file.
C                Same as readBT except also reads the basin and 
C                cyclone number.
C                Same as readBest except it can read multi-line 
C                best track files.
C
C  PROGRAMMER, DATE:  Ann Schrader   (SAIC)  June 1998
C
C  USAGE:  call readBestTrk (datFile, basin, cycnum, cent, dtg, flat, ns, 
C                           flon, ew, iwind, ios )
C
C  INPUT PARAMETERS:  
C     datFile - unit number of best track file
C
C  OUTPUT PARAMETERS:
C     basin - basin read from the best track file
C     cycnum - cyclone num read from the best track file
C     cent - century (2 characters, e.g. 19)
C     dtg - YYMMDDHH, date-time-group read from the best track file
C     flat - latitude read from the best track file
C     ns - one character north/south indicator for latitude
C     flon - longitude read from the best track file
C     ew - one character east/west indicator for longitude
C     iwind - max wind read from the best track file
C     ios - input/output status upon completion or the read operation
C           0 - successful read
C           negative - end-of-file
C           positive - error
C
C  ERROR CONDITIONS:
C
C........................MAINTENANCE SECTION............................
C
C  PRINCIPAL VARIABLES AND ARRAYS:
C
C  METHOD:
C
C  RECORD OF CHANGES:
C
C........................END PROLOGUE..................................
C
      subroutine readBestTrk (datFile, basin, cycnum, cent, dtg, flat, 
     &     ns, flon, ew, iwind, ios )

      include 'dataformats.inc'
c
c         formal parameters
      integer           datFile
      character*2       basin
      character*2       cycnum
      character*8       dtg
      character*2       cent
      real              flat, flon
      character*1       ns, ew
      integer           iwind
      integer           ios
c
c         local variables
      type (AID_DATA) btRcd
      integer         readStat

c     Read the next record in the data file.
      call readARecord( datFile, btRcd, readStat )
      if( readStat .eq. 1 ) then
         basin = btRcd%aRecord(1)%basin
         write( cycnum, '(i2.2)') btRcd%aRecord(1)%cyNum
         cent = btRcd%aRecord(1)%DTG(1:2)
         dtg = btRcd%aRecord(1)%DTG(3:10)
         flat = btRcd%aRecord(1)%lat
         ns = btRcd%aRecord(1)%NS
         flon = btRcd%aRecord(1)%lon
         ew = btRcd%aRecord(1)%EW
         iwind = btRcd%aRecord(1)%vmax
      endif
C     Switch the returned results from readARecord to results as defined by 
C     FORTRAN read
      if( readStat .lt. 0 ) then
         ios = readStat
      elseif( readStat .eq. 1 ) then
         ios = 0
      elseif( readStat .eq. 0 ) then
         ios = 1
      endif
      
C
      RETURN
      END


C
C........................START PROLOGUE.................................
C
C  SUBPROGRAM NAME:  doWriteAidRcd
C
C  DESCRIPTION:  Write the aid record to the output data file.
C
C  PROGRAMMER, DATE:  Ann Schrader   (SAIC)  June 1998
C
C  USAGE:  call doWriteAidRcd (datFile,stormID,cdtg,techname,itau,llwnd)
C
C  INPUT PARAMETERS:  
C     datFile - unit number of output data file
C     stormID - storm id, eg. cp021997
C     cdtg - current dtg, eg. 1998060912
C     techname - objective aid name, eg. CLIM
C     itau - forecast period
C     llwnd - array of integers dimensioned (3) where 
C             and the three components are lat, lon and wind
C
C  OUTPUT PARAMETERS: NONE
C
C  ERROR CONDITIONS:
C
C........................MAINTENANCE SECTION............................
C
C  PRINCIPAL VARIABLES AND ARRAYS:
C
C  METHOD:
C
C  RECORD OF CHANGES:
C
C........................END PROLOGUE..................................
C
      subroutine doWriteAidRcd (datFile, stormID, cdtg, techname, 
     1     itau, llwnd )

      include 'dataioparms.inc'
c
c         formal parameters
      integer           datFile
      character*8       stormID
      character*10      cdtg
      character*4       techname
      integer           itau
      integer           llwnd(llw)
c
c         local variables
      character*2       basin
      character*2       stormnum
      character*1       ns, ew
      character*2       technum
      integer           ilat, ilon
c . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
c
      basin = stormID(1:2)
      call upcase( basin, 2 )
      stormnum = stormID(3:4)

c     Assign the objective aid technique number
      if( techname .eq. 'CARQ' ) then
         technum = '01'
      else if( techname .eq. 'WRNG' ) then
         technum = '02'
      else
         technum = '03'
      endif

cx    Check for model runs where lat/lon out of range - convert to 0's
cx    sampson nrl oct 26, 1998
cx    sampson nrl aug 19, 2000  changed lon check for less than zero
cx    Handle cases of forecasts crossing 0 longitude,  ajs 1/17/01
      if( llwnd(2) .lt. 0 ) then
          llwnd(2) = llwnd(2) + 3600
      endif

      if( llwnd(1) .lt. -900  .or. 
     1    llwnd(1) .gt.  900  .or.
     2    llwnd(2) .lt.    0  .or. 
     3    llwnd(2) .gt.  3600 ) then
         llwnd(1) = 0
         llwnd(2) = 0
      endif
cx    Check for model runs where wind out of range - convert to 0's
cx    sampson nrl oct 26, 1998
      if( llwnd(3) .lt. 0 .or. llwnd(3) .gt. 300) 
     1     llwnd(3) = 0
      
      if( llwnd(1) .ne. 0 .or. llwnd(2) .ne. 0 
     1     .or. llwnd(3) .ne. 0) then
c     Convert from -900 thru 900 to 900S thru 900N
         ns = 'N'
         ilat = llwnd(1)
         if( ilat .lt. 0 ) then
            ilat = -ilat
            ns = 'S'
         endif
c     Convert from 0 thru 3600 (WH < 1800 < EH) to 1800W thru 1800E
         ew = 'W'
         ilon = llwnd(2)
         if( ilon .gt. 1800 ) then
            ilon = 3600 - ilon
            ew = 'E'
         endif
c     Write the aid record...
cx    only if the lat and lon are meaningful
         if ( ilon .le. 1800 .and. ilat .lt. 900 ) then
            write(datFile,9080) basin, stormnum, cdtg, 
     1           technum, techname, itau, ilat, ns, ilon, ew, 
     1           llwnd(3)
 9080       format( A2,", ",A2,", ",A10,", ",A2,", ",A4,", ",
     1           I3,", ",I3,A1,", ",I4,A1,", ",I3 )
         endif
      endif
      
C
      END


C
C........................START PROLOGUE.................................
C
C  SUBPROGRAM NAME:  newWriteAidRcd
C
C  DESCRIPTION:  Write the aid record to the output data file,
C                NHC version
C
C  PROGRAMMER, DATE:  Ann Schrader   (SAIC)  June 1998
C
C  USAGE:  call newWriteAidRcd (datFile,stormID,cdtg,techname,ltlnwnd)
C
C  INPUT PARAMETERS:  
C     datFile - unit number of output data file
C     stormID - storm id, eg. cp021997
C     cdtg - current dtg, eg. 1998060912
C     techname - objective aid name, eg. CLIM
C     ltlnwnd - array of integers dimensioned (newnumtau,llw) where 
C               the first dimension is the TAUs 0, 12, 24, 36, 48, 60, 72...
C               and the second dimension is lat, lon and wind
C
C  OUTPUT PARAMETERS: NONE
C
C  ERROR CONDITIONS:
C
C........................MAINTENANCE SECTION............................
C
C  PRINCIPAL VARIABLES AND ARRAYS:
C
C  METHOD:
C
C  RECORD OF CHANGES:
C
C........................END PROLOGUE..................................
C
      subroutine newWriteAidRcd (datFile, stormID, cdtg, techname, 
     1     ltlnwnd )

      include 'dataioparms.inc'
c
c         formal parameters
      integer           datFile
      character*8       stormID
      character*10      cdtg
      character*4       techname
      integer           ltlnwnd(newnumtau,llw)
c
c         local variables
      integer           ii, jj, itau
      integer           llwnd(llw)
c . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
c

      loopend = newnumtau
c     For CARQ and WRNG loop on the taus: -24, -18, -12, -6 and 0
      if (techname.eq.'CARQ' .or. techname.eq.'WRNG') loopend = 5

c     For all other aids loop on the taus: 
c        0, 12, 24, 36, 48, 60, 72, 84, 96, 108 and 120
      do ii = 1, loopend
         if( techname .eq. 'CARQ' .or. techname .eq. 'WRNG' ) then
            itau = -( (5-ii) * 6)
cx       do all taus in 12 hr increments
         else
	    itau = (ii-1) * 12
         endif
         do jj = 1, llw
            llwnd(jj) = ltlnwnd(ii,jj)
         enddo
         call doWriteAidRcd(datFile, stormID, cdtg, techname, itau, 
     &                      llwnd )
      enddo
C
      END


C
C........................START PROLOGUE.................................
C
C  SUBPROGRAM NAME:  writeAidRcd
C
C  DESCRIPTION:  Write the aid record to the output data file,
C                JTWC, NPMOC version
C
C  PROGRAMMER, DATE:  Ann Schrader   (SAIC)  June 1998
C
C  USAGE:  call writeAidRcd (datFile,stormID,cdtg,techname,ltlnwnd)
C
C  INPUT PARAMETERS:  
C     datFile - unit number of output data file
C     stormID - storm id, eg. cp021997
C     cdtg - current dtg, eg. 1998060912
C     techname - objective aid name, eg. CLIM
C     ltlnwnd - array of integers dimensioned (10,3) where 
C               the first dimension is the TAUs 12, 24, 36, 48, 72, ...
C               and the second dimension is lat, lon and wind
C
C  OUTPUT PARAMETERS: NONE
C
C  ERROR CONDITIONS:
C
C........................MAINTENANCE SECTION............................
C
C  PRINCIPAL VARIABLES AND ARRAYS:
C
C  METHOD:
C
C  RECORD OF CHANGES:
C
C........................END PROLOGUE..................................
C
      subroutine writeAidRcd (datFile, stormID, cdtg, techname, 
     1     ltlnwnd )

      include 'dataioparms.inc'
c
c         formal parameters
      integer           datFile
      character*8       stormID
      character*10      cdtg
      character*4       techname
      integer           ltlnwnd(numtau,llw)
c
c         local variables
      character*6       oldstormID;
c . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
c

      oldstormID = stormID(1:4)//stormID(7:8)
      call writeAid( datFile, oldstormID, stormID(5:6), cdtg(3:10), techname,
     &     ltlnwnd )
C
      END


C
C........................START PROLOGUE.................................
C
C  SUBPROGRAM NAME:  writeAid
C
C  DESCRIPTION:  Write the aid record to the output data file.
C
C  PROGRAMMER, DATE:  Ann Schrader   (SAIC)  June 1998
C
C  USAGE:  call writeAid (datFile,stormID,century,cdtg,techname,ltlnwnd)
C
C  INPUT PARAMETERS:  
C     datFile - unit number of output data file
C     stormID - storm id, eg. cp0297
C     century - 1st two digits of the storm year, eg. 19
C     cdtg - current dtg, eg. 98060912
C     techname - objective aid name, eg. CLIM
C     ltlnwnd - array of integers dimensioned (numtau,llw) where 
C               the first dimension is the TAUs 12, 24, 36, 48, 60, 72 ...
C               and the second dimension is lat, lon and wind
C
C  OUTPUT PARAMETERS: NONE
C
C  ERROR CONDITIONS:
C
C........................MAINTENANCE SECTION............................
C
C  PRINCIPAL VARIABLES AND ARRAYS:
C
C  METHOD:
C
C  RECORD OF CHANGES:
C
C  sampson, nrl  Apr 99     R120 - has only 72, 96 and 120 hr fcsts
C  sampson, nrl  Jan 00     C120 - 120 hour forecast, 12 hr interval
C  sampson, nrl  Apr 00     XTRP - 120 hour forecast, 12 hr interval
C  sampson, nrl  May 01     ST5D - 120 hour forecast, 12 hr interval
C  sampson, nrl  May 01     STIP, STID - 120 hour forecast, 12 hr interval
C  sampson, nrl  Oct 02     R120 - is 120 hour forecast, 12 hr interval
C  sampson, nrl  May 04     STWP, STWD - 120 hour forecast, 12 hr interval
C  sampson, nrl  Jun 04     xxSP, xxSD - 120 hour forecast, 12 hr interval
C  sampson, nrl  Jun 04     xxS1, xxS1 - 120 hour forecast, 12 hr interval
C........................END PROLOGUE..................................
C
      subroutine writeAid (datFile, stormID, century, cdtg, techname,
     1     ltlnwnd )

      include 'dataioparms.inc'
c
c         formal parameters
      integer           datFile
      character*6       stormID
      character*2       century
      character*8       cdtg
      character*4       techname
      integer           ltlnwnd(numtau,llw)
c
c         local variables
      character*2       basin
      character*2       stormnum
      character*1       ns, ew
      character*2       technum
      integer           ii, itau
      integer           ilat, ilon
c . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
c
      basin = stormID(1:2)
      call upcase( basin, 2 )
      stormnum = stormID(3:4)

c     Assign the objective aid technique number
      if( techname .eq. 'CARQ' ) then
         technum = '01'
      else if( techname .eq. 'WRNG' ) then
         technum = '02'
      else
         technum = '03'
      endif
cx  
      loopend = 5
      if (techname.eq.'C120' .or. techname.eq.'XTRP' .or.
     &    techname.eq.'C121' .or. techname.eq.'C12X' .or.
     &    techname.eq.'C12C' .or. techname.eq.'C12X' .or.
     &    techname.eq.'STIP' .or. techname.eq.'STID' .or.
     &    techname.eq.'STWP' .or. techname.eq.'STWD' .or.
     &    techname(3:4).eq.'SD' .or. techname(3:4).eq.'SP' .or.
     &    techname(3:4).eq.'S1' .or. techname(3:4).eq.'S1' .or.
     &    techname.eq.'ST5D' .or. techname.eq.'R120' ) loopend = numtau

c     Loop on the taus: 12, 24, 36, 48 and 72
      do ii = 1, loopend
         if( techname .eq. 'CARQ' .or. techname .eq. 'WRNG' ) then
            itau = -( (5-ii) * 6)
cx       long-range forecasts (e.g., C120) ... sampson NRL Jan 00
cx       does all taus in 12 hr increments
         else if ( techname.eq.'C120' .or. techname.eq.'XTRP' .or.
     &             techname.eq.'C121' .or. techname.eq.'C12X' .or.
     &             techname.eq.'C12C' .or. techname.eq.'C12X' .or.
     &             techname.eq.'STIP' .or. techname.eq.'STID' .or.
     &             techname.eq.'STWP' .or. techname.eq.'STWD' .or.
     &             techname(3:4).eq.'SD' .or. techname(3:4).eq.'SP' .or.
     &             techname(3:4).eq.'S1' .or. techname(3:4).eq.'S1' .or.
     &             techname.eq.'ST5D' .or. techname.eq.'R120' ) then
	    itau = ii * 12
cx       special case for extended forecasts (R120) ... sampson NRL Apr 99
cx       else if ( techname .eq. 'R120' ) then
cx          itau = ii * 12 + 60
cx       old case for 72 hour forecasts ... 
         else
            itau = ii * 12
            if( itau .eq. 60 ) itau = 72
         endif
cx       Check for model runs where lat/lon out of range - convert to 0's
cx       sampson nrl oct 26, 1998
	 if( ltlnwnd(ii,1) .lt. -900  .or. 
     1	     ltlnwnd(ii,1) .gt.  900  .or.
     2       ltlnwnd(ii,2) .lt. -1800 .or. 
     3       ltlnwnd(ii,2) .gt.  3600 ) then
      	       ltlnwnd(ii,1) = 0
      	       ltlnwnd(ii,2) = 0
         endif
cx       Check for model runs where wind out of range - convert to 0's
cx       sampson nrl oct 26, 1998
	 if( ltlnwnd(ii,3) .lt. 0 .or. ltlnwnd(ii,3) .gt. 300) 
     1	       ltlnwnd(ii,3) = 0

         if( ltlnwnd(ii,1) .ne. 0 .or. ltlnwnd(ii,2) .ne. 0 
     1        .or. ltlnwnd(ii,3) .ne. 0) then
c           Convert from -900 thru 900 to 900S thru 900N
            ns = 'N'
            ilat = ltlnwnd(ii,1)
            if( ilat .lt. 0 ) then
               ilat = -ilat
               ns = 'S'
            endif
c           Convert from 0 thru 3600 (WH < 1800 < EH) to 1800W thru 1800E
            ew = 'W'
            ilon = ltlnwnd(ii,2)
            if( ilon .gt. 1800 ) then
               ilon = 3600 - ilon
               ew = 'E'
            endif
c           Write the aid record...
cx          only if the lat and lon are meaningful
	    if ( ilon .lt. 1800 .and. ilat .lt. 900 ) then
               write(datFile,9080) basin, stormnum, century, cdtg, 
     1              technum, techname, itau, ilat, ns, ilon, ew, 
     1              ltlnwnd(ii,3)
 9080         format( A2,", ",A2,", ",A2,A8,", ",A2,", ",A4,", ",
     1              I3,", ",I3,A1,", ",I4,A1,", ",I3 )
            endif
         endif
      enddo
C
      END



C
C........................START PROLOGUE.................................
C
C  SUBPROGRAM NAME:  readNext
C
C  DESCRIPTION:  Reads the next ATCF_RECORD record from the input file
C
C  PROGRAMMER, DATE:  Ann Schrader   (SAIC)  June 1998
C
C  USAGE:  call readNext (datFile,record,ios)
C
C  INPUT PARAMETERS:  
C     datFile - unit number of input data file
C
C  OUTPUT PARAMETERS:
C     rcd - structure to read record into
C     ios - return 0 success, neg for end of file, pos for error
C
C  ERROR CONDITIONS:
C
C........................MAINTENANCE SECTION............................
C
C  PRINCIPAL VARIABLES AND ARRAYS:
C
C  METHOD:
C
C  RECORD OF CHANGES:
C
C........................END PROLOGUE..................................
C
      subroutine readNext (datFile, rcd, ios )
c
      implicit none
c
      INCLUDE  'dataformats.inc'
c
c         formal parameters
      integer            datFile
      type (ATCF_RECORD) rcd
      integer            ios
c
c         local variables
      character line*200
c . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
c
c
c     Zero out the dtg, tech, lat, lon and vmax just in case
      rcd%DTG = '          '
      rcd%tech = '    '
      rcd%latns = '    '
      rcd%lonew = '     '
      rcd%vmax = '   '
c
c     Read one data record.
      read( datFile, '(a200)', iostat=ios ) line
c     Skip over blank lines.
      do while( ios .eq. 0 .and. len_trim(line) .eq. 0 )
         read( datFile, '(a200)', iostat=ios ) line
      enddo
      if( ios .eq. 0 ) then
c
c     Get the individual fields from the data record.
         read( line, '(a2,2x,a2,2x,a10,2x,a2,2x,a4,2x,a3)' )
     &        rcd%basin, rcd%cyNum, rcd%DTG, rcd%technum, rcd%tech, 
     &        rcd%tau
         read( line, '(35x,a4,2x,a5,2x,a3)' )
     &        rcd%latns, rcd%lonew, rcd%vmax
         read( line, '(53x,a4,2x,a2,2x,a3,2x,a3,4(2x,a4))' )
     &        rcd%mslp, rcd%ty, rcd%rad, rcd%windcode, rcd%radii(1), 
     &        rcd%radii(2), rcd%radii(3), rcd%radii(4)
         read( line, '(97x,a4,2x,a4,5(2x,a3),2x,a3,2x,a3,2x,a3)' )
     &        rcd%radp, rcd%rrp, rcd%mrd, rcd%gusts, rcd%eye, 
     &        rcd%subregion, rcd%maxseas, rcd%initials, rcd%dir, 
     &        rcd%speed
         read( line, '(149x,a10,2x,a1)' )  
     &        rcd%stormname, rcd%depth
         read( line, '(164x,a2,2x,a3,4(2x,a3))' )  
     &        rcd%seas, rcd%seascode, rcd%seasrad(1), rcd%seasrad(2),
     &        rcd%seasrad(3), rcd%seasrad(4)
      endif
C     
      END



C
C........................START PROLOGUE.................................
C
C  SUBPROGRAM NAME:  readNextFix
C
C  DESCRIPTION:  Reads the next FIX_RECORD record from the input file.
C
C  PROGRAMMER, DATE:  Ann Schrader   (SAIC)  June 2004
C
C  USAGE:  call readNextFix (datFile,record,ios)
C
C  INPUT PARAMETERS:  
C     datFile - unit number of input data file
C
C  OUTPUT PARAMETERS:
C     rcd - structure to read record into
C     ios - return 0 success, neg for end of file, pos for error
C
C  ERROR CONDITIONS:
C
C........................MAINTENANCE SECTION............................
C
C  PRINCIPAL VARIABLES AND ARRAYS:
C
C  METHOD:
C
C  RECORD OF CHANGES:
C
C........................END PROLOGUE..................................
C
      subroutine readNextFix (datFile, rcd, ios )
c
      implicit none
c
      INCLUDE  'dataformats.inc'
c
c         formal parameters
      integer            datFile
      type (FIX_RECORD)  rcd
      integer            ios
c
c         local variables
      character line*400
c . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
c
c
c     Zero out the dtg, fixtype, lat, lon and vmax 
      rcd%DTG = '            '
      rcd%fixtype = '    '
      rcd%latns = '     '
      rcd%lonew = '      '
      rcd%v = '   '
c
c     Read one data record.
      read( datFile, '(a400)', iostat=ios ) line
c     Skip over blank lines.
      do while( ios .eq. 0 .and. len_trim(line) .eq. 0 )
         read( datFile, '(a400)', iostat=ios ) line
      enddo
      if( ios .eq. 0 ) then
c
c     Get the individual fields from the data record.
         read( line, '(a2,2x,a2,2x,a12,2x,a3,2x,a4,2x,a10,2x,a1)' )
     &        rcd%basin, rcd%cyNum, rcd%DTG, rcd%fixformat, rcd%fixtype, 
     &        rcd%cicode, rcd%flagged
         read( line, '(48x,a5,2x,a6,2x,a5,2x,a1,2x,a3)' )
     &        rcd%latns, rcd%lonew, rcd%height, rcd%positConf, rcd%v
         read( line, '(78x,a1,2x,a4,2x,a1,2x,a4)' )
     &        rcd%vConf, rcd%pressure, rcd%presConf, rcd%presDeriv
         read( line, '(96x,a3,2x,a4,4(2x,a4),4(2x,a1))' )
     &        rcd%rad, rcd%windcode, rcd%radii(1), rcd%radii(2),
     &        rcd%radii(3), rcd%radii(4), rcd%radMod(1), 
     &        rcd%radMod(2), rcd%radMod(3), rcd%radMod(4)
         read( line, '(143x,a1,2x,a3,2x,a3,2x,a1,2x,a5,2x,a3)' )
     &        rcd%radConf, rcd%mrd, rcd%eye, rcd%subregion, 
     &        rcd%fixsite, rcd%initials
         read( line, '(171x,a200)' ) rcd%remainder
      endif
C     
      END


C
C........................START PROLOGUE.................................
C
C  SUBPROGRAM NAME:  getARecord
C
C  DESCRIPTION:  Reads one record of specified type from the input file
C
C  PROGRAMMER, DATE:  Ann Schrader   (SAIC)  June 1998
C
C  USAGE:  call getARecord (datFile,"CARQ",aidRcd,result)
C
C  INPUT PARAMETERS:  
C     datFile - unit number of output data file
C     technique - "CARQ", "WRNG", "JTWC" ...
C
C  OUTPUT PARAMETERS:
C     aidRcd - structure to read data into
C     result - return 0 for fail, neg for end-of-file, 1 for success
C
C  ERROR CONDITIONS:
C
C........................MAINTENANCE SECTION............................
C
C  PRINCIPAL VARIABLES AND ARRAYS:
C
C  METHOD:
C
C  RECORD OF CHANGES:
C
C........................END PROLOGUE..................................
C
      subroutine getARecord (datFile, technique, aidRcd, result )
c
      implicit none
c
      INCLUDE  'dataformats.inc'
c
c         formal parameters
      integer         datFile
      character*4     technique
      type (AID_DATA) aidRcd
      integer         result
c
c         local variables
      logical*2   found
c . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
c
      result = 1
      found = .false.
c
c     Loop on reading records until a record is found
c     of the type specified
      do while( result.eq.1 .and. .not.found )
c
c        Read the next record in the data file.
         call readARecord( datFile, aidRcd, result )
c
c        If tech name matches specified record type then process.
         if( technique .eq. aidRcd%aRecord(1)%tech ) found = .true.
c
      enddo
c
      if( result.ne.0 .and. .not.found ) result = 0
C
      END


C
C........................START PROLOGUE.................................
C
C  SUBPROGRAM NAME:  readARecord
C
C  DESCRIPTION:  Reads one AID_DATA data info from the input file.
C
C  PROGRAMMER, DATE:  Ann Schrader   (SAIC)  June 1998
C
C  USAGE:  call readARecord (datFile,aidRcd,result)
C
C  INPUT PARAMETERS:  
C     datFile - unit number of output data file
C
C  OUTPUT PARAMETERS:
C     aidRcd - structure to read data into
C     result - return 0 for fail, neg for unexpected end-of-file, 
C              1 for success
C
C  ERROR CONDITIONS:
C
C........................MAINTENANCE SECTION............................
C
C  PRINCIPAL VARIABLES AND ARRAYS:
C
C  METHOD:
C
C  RECORD OF CHANGES:
C
C........................END PROLOGUE..................................
C
      subroutine readARecord (datFile, aidRcd, result )
c
      implicit none
c
      INCLUDE  'dataformats.inc'
c
c         formal parameters
      integer         datFile
      type (AID_DATA) aidRcd
      integer         result
c
c         local variables
      integer            ii
      integer            readStat
      type (ATCF_RECORD) recrd
      character          savDTG*10
      character          savtech*4
      logical*2          done
      integer            savtau
      integer            newtau
c . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
c
      result = 0
c
c     Read the next record in the data file
      call readNext( datFile, recrd, readStat )
c
c     Save the date-time-group and the technique
      if( readStat .eq. 0 ) then
         result = 1
         savDTG = recrd%DTG
         savtech = recrd%tech
      endif
c
c     Read all of the tau's for this DTG and tech
      ii=0
      aidRcd%numrcrds = 0
      done = .false.
      do while (result.eq.1 .and. readStat.eq.0
     &          .and. ii.lt.AidTauMax*AidRadMax .and. .not.done)
         ii = ii + 1
         read( recrd%tau, '(i3)' ) savtau
c
c        Process the A_RECORD
         call processArcd( aidRcd%aRecord(ii), recrd, result )
c        Copy the ATCF_RECORD
         aidRcd%atcfRcd(ii) = recrd
         aidRcd%numrcrds = ii
c
c        Read the next record in the data file
         call readNext( datFile, recrd, readStat )
         read( recrd%tau, '(i3)' ) newtau
c
c        If new dtg or tech or tau is less than previous tau then 
c        flag done and backup one record.
         if( readStat.eq.0 .and. (savDTG.ne.recrd%DTG .or. 
     &        savtech.ne.recrd%tech .or. newtau.lt.savtau) ) then
            done = .true.
            backspace( datFile )
         endif
      enddo

      if( readStat .gt. 0 ) result = 0
      if( readStat .lt. 0 .and. aidRcd%numrcrds .eq. 0 ) 
     &     result = readStat
C
      END


C
C........................START PROLOGUE.................................
C
C  SUBPROGRAM NAME:  getSingleTAU
C
C  DESCRIPTION:  Gets the aid data for the specified tau from the
C                supplied AID_DATA.  Returns the data for the tau 
C                in tauData.
C
C  PROGRAMMER, DATE:  Ann Schrader   (SAIC)  June 1998
C
C  USAGE:  call getSingleTAU (aidRcd, 72, tauData, result)
C
C  INPUT PARAMETERS:  
C     aidRcd - supplied AID_DATA structure
C     tau    - requested tau
C
C  OUTPUT PARAMETERS:
C     tauData - struct to read data into
C     result - return 0 for fail, 1 for success
C
C  ERROR CONDITIONS:
C
C........................MAINTENANCE SECTION............................
C
C  PRINCIPAL VARIABLES AND ARRAYS:
C
C  METHOD:
C
C  RECORD OF CHANGES:
C
C........................END PROLOGUE..................................
C
      subroutine getSingleTAU ( aidRcd, tau, tauData, result )
c
      implicit none
c
      INCLUDE  'dataformats.inc'
c
c         formal parameters
      type (AID_DATA) aidRcd, tauData
      integer         tau
      integer         result
c
c         local variables
      integer   ii, jj
      logical*2 found
c . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
c
      result = 1
c
c     Find the requested tau in the AID_DATA structure.
      found = .false.
      ii = 1
      jj = 1
      tauData%numrcrds = 0
      do while( ii.le.aidRcd%numrcrds )
         if( aidRcd%aRecord(ii)%tau .eq. tau ) then
            found = .true.
            tauData%aRecord(jj) = aidRcd%aRecord(ii)
            tauData%atcfRcd(jj) = aidRcd%atcfRcd(ii)
            jj = jj + 1
            tauData%numrcrds = tauData%numrcrds + 1
         endif
         ii = ii + 1
      enddo

      if( .not.found ) result = 0
C
      END


C
C........................START PROLOGUE.................................
C
C  SUBPROGRAM NAME:  getAidTAU
C
C  DESCRIPTION:  Gets the first A_RECORD record for the specified tau from
C                the supplied AID_DATA.
C                Note: this only get the first RAD (34, 50, 64, 100 kt),
C                for the requested tau.  If all the records for the tau
C                are needed then use getSingleTAU().
C
C  PROGRAMMER, DATE:  Ann Schrader   (SAIC)  June 1998
C
C  USAGE:  call getAidTAU (aidRcd, 72, aRecord, result)
C
C  INPUT PARAMETERS:  
C     aidRcd - supplied AID_DATA structure
C     tau    - requested tau
C
C  OUTPUT PARAMETERS:
C     aRecord - struct to read data into
C     result - return 0 for fail, 1 for success
C
C  ERROR CONDITIONS:
C
C........................MAINTENANCE SECTION............................
C
C  PRINCIPAL VARIABLES AND ARRAYS:
C
C  METHOD:
C
C  RECORD OF CHANGES:
C
C........................END PROLOGUE..................................
C
      subroutine getAidTAU ( aidRcd, tau, aRecord, result )
c
      implicit none
c
      INCLUDE  'dataformats.inc'
c
c         formal parameters
      type (AID_DATA) aidRcd
      integer         tau
      type (A_RECORD) aRecord
      integer         result
c
c         local variables
      integer   ii
      logical*2 found
c . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
c
      result = 1
c
c     Find the requested tau in the AID_DATA structure.
      found = .false.
      ii = 1
      do while( ii.le.aidRcd%numrcrds .and. .not.found )
         if( aidRcd%aRecord(ii)%tau .eq. tau ) then
            found = .true.
            aRecord = aidRcd%aRecord(ii)
         endif
         ii = ii + 1
      enddo

      if( .not.found ) result = 0
C
      END


C
C........................START PROLOGUE.................................
C
C  SUBPROGRAM NAME:  getAidDTG
C
C  DESCRIPTION:  Gets the first aid data for the specified DTG 
C                from the input file
C
C  PROGRAMMER, DATE:  Ann Schrader   (SAIC)  June 1998
C
C  USAGE:  call getAidDTG (datFile, dtg, aidRcd, result)
C
C  INPUT PARAMETERS:  
C     datFile - unit number of output data file
C     dtg    - requested dtg (date-time-group)
C
C  OUTPUT PARAMETERS:
C     aidRcd - structure to read data into
C     result - return 0 for fail, neg for end-of-file, 1 for success
C
C  ERROR CONDITIONS:
C
C........................MAINTENANCE SECTION............................
C
C  PRINCIPAL VARIABLES AND ARRAYS:
C
C  METHOD:
C
C  RECORD OF CHANGES:
C
C........................END PROLOGUE..................................
C
      subroutine getAidDTG ( datFile, dtg, aidRcd, result )
c
      implicit none
c
      INCLUDE  'dataformats.inc'
c
c         formal parameters
      integer           datFile
      character*8       dtg
      type (AID_DATA)   aidRcd
      integer           result
c
c         local variables
      logical*2 found
c . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
c
      result = 1
      found = .false.
c
c     Loop on reading records until a record is found
c     with the dtg specified
      do while( result.eq.1 .and. .not.found )
c
c        Read the next record in the data file.
         call readARecord( datFile, aidRcd, result )
c
c        If dtg matches specified dtg then process.
         if( dtg .eq. aidRcd%aRecord(1)%DTG(3:10) ) found = .true.
c
      enddo
c
      if( result.ne.0 .and. .not.found ) result = 0
C
      END


C
C........................START PROLOGUE.................................
C
C  SUBPROGRAM NAME:  getTech
C
C  DESCRIPTION:  Gets the data for a specified aid technique
C                from the supplied BIG_AID_DATA structure and
C                returns an AID_DATA structure
C
C  PROGRAMMER, DATE:  Ann Schrader   (SAIC)  Sept 2000
C
C  USAGE:  call getTech (bigAidRcd, tech, aidRcd, result)
C
C  INPUT PARAMETERS:  
C     bigAidRcd - BIG_AID_DATA structure containing all records for a dtg
C     tech - requested obj aid technique
C
C  OUTPUT PARAMETERS:
C     aidRcd - structure to read data into
C     result - return 0 for fail, 1 for success
C
C  ERROR CONDITIONS:
C
C........................MAINTENANCE SECTION............................
C
C  PRINCIPAL VARIABLES AND ARRAYS:
C
C  METHOD:
C
C  RECORD OF CHANGES:
C
C........................END PROLOGUE..................................
C
      subroutine getTech ( bigAidRcd, tech, aidRcd, result )
c
      implicit none
c
      INCLUDE  'dataformats.inc'
c
c         formal parameters
      type (BIG_AID_DATA) bigAidRcd
      character*4     tech
      type (AID_DATA) aidRcd
      integer         result
c
c         local variables
      integer   ii, jj
      logical*2 found
c . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
c
      result = 1
c
c     Find the requested tech in the BIG_AID_DATA structure.
      found = .false.
      ii = 1
      jj = 1
      aidRcd%numrcrds = 0
      do while( ii .le. bigAidRcd%numrcrds )
         if( bigAidRcd%aRecord(ii)%tech .eq. tech ) then
            found = .true.
            aidRcd%aRecord(jj) = bigAidRcd%aRecord(ii)
            aidRcd%atcfRcd(jj) = bigAidRcd%atcfRcd(ii)
            jj = jj + 1
            aidRcd%numrcrds = aidRcd%numrcrds + 1
         endif
         ii = ii + 1
      enddo

      if( .not.found ) result = 0
C
      END


C
C........................START PROLOGUE.................................
C
C  SUBPROGRAM NAME:  getBigAidDTG
C
C  DESCRIPTION:  Gets all the aid data for the specified DTG 
C                from the input file 
C
C  PROGRAMMER, DATE:  Ann Schrader   (SAIC)  Sept 2000
C
C  USAGE:  call getBigAidDTG (datFile, dtg, bigAidRcd, result)
C
C  INPUT PARAMETERS:  
C     datFile - unit number of output data file
C     dtg    - requested dtg (date-time-group)
C
C  OUTPUT PARAMETERS:
C     bigAidRcd - structure to read data into
C     result - return 0 for fail, neg for end-of-file, 1 for success
C
C  ERROR CONDITIONS:
C
C........................MAINTENANCE SECTION............................
C
C  PRINCIPAL VARIABLES AND ARRAYS:
C
C  METHOD:
C
C  RECORD OF CHANGES:
C
C  Sampson, NRL 8/01 If dtg of file is larger than searched dtg, quit search
C
C........................END PROLOGUE..................................
C
      subroutine getBigAidDTG ( datFile, dtg, bigAidRcd, result )
c
      implicit none
c
      INCLUDE  'dataformats.inc'
c
c         formal parameters
      integer           datFile
c      character*8       dtg
      character*10       dtg
      type (BIG_AID_DATA)   bigAidRcd
      type (AID_DATA)   aidRcd
      integer           readStat
      integer           result
      integer           ii, jj
c
c         local variables
      logical*2 found
      logical*2 dtgmatch
c . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
c
      result = 1
      readStat = 1
      found = .false.
      dtgmatch = .false.
c
c     Loop on reading records until a record is found
c     with the dtg specified
      do while( result.eq.1 .and. .not.found )
c
c        Read the next record in the data file.
         call readARecord( datFile, aidRcd, readStat )
         result = readStat
c
c        If dtg matches specified dtg then process.
         if( dtg .eq. aidRcd%aRecord(1)%DTG ) found = .true.
cx
cx       If dtg is greater than specified dtg then stop and backspace.
         if( result.eq.1 .and. dtg .lt. aidRcd%aRecord(1)%DTG ) then
	     result = 0
             call backspaceFile( datFile, aidRcd%numrcrds )
	     return
         endif
c
      enddo

      if( found ) dtgmatch = .true.
c     If found assign the aidRcd read into bigAidRcd 
      if( dtgmatch ) then
         do ii=1, aidRcd%numrcrds
            bigAidRcd%aRecord(ii) = aidRcd%aRecord(ii)
            bigAidRcd%atcfRcd(ii) = aidRcd%atcfRcd(ii)
         enddo
         bigAidRcd%numrcrds = aidRcd%numrcrds
      endif

c     Loop on reading records as long as dtg matches specified dtg
      do while( readStat.eq.1 .and. dtgmatch )
c
c        Read the next record in the data file.
         call readARecord( datFile, aidRcd, readStat )
         result = readStat
c
c        If dtg matches specified dtg then process.
         if( dtg .ne. aidRcd%aRecord(1)%DTG ) dtgmatch = .false.
c        If matching dtg then assign aidRcd into bigAidRcd
         if( readStat.ne.0 .and. dtgmatch ) then
            jj = bigAidRcd%numrcrds + 1
            do ii=1, aidRcd%numrcrds
               bigAidRcd%aRecord(jj) = aidRcd%aRecord(ii)
               bigAidRcd%atcfRcd(jj) = aidRcd%atcfRcd(ii)
               jj = jj + 1
            enddo
            bigAidRcd%numrcrds = bigAidRcd%numrcrds + aidRcd%numrcrds
         endif
c
      enddo
      
c     Backup the file to just after the last matching dtg.
      if( found .and. .not. dtgmatch .and. aidRcd%numrcrds .gt. 0 )
     &   call backspaceFile( datFile, aidRcd%numrcrds )
c
      if( result.ne.0 .and. .not.found ) result = 0
C
      END


C
C........................START PROLOGUE.................................
C
C  SUBPROGRAM NAME:  backspaceFile
C
C  DESCRIPTION:  Repositions the specified file back "numRcrds" records.
C
C  PROGRAMMER, DATE:  Ann Schrader   (SAIC)  Sept 2000
C
C  USAGE:  call backspaceFile ( datFile, numRcrds )
C
C  INPUT PARAMETERS:  
C     datFile - unit number of the objective aids file
C     numRcrds - number of records to back up
C
C  OUTPUT PARAMETERS:
C
C  ERROR CONDITIONS:
C
C........................MAINTENANCE SECTION............................
C
C  PRINCIPAL VARIABLES AND ARRAYS:
C
C  METHOD:
C
C  RECORD OF CHANGES:
C
C........................END PROLOGUE..................................
C
      subroutine backspaceFile (datFile, numRcrds)
c
      implicit none
c
      INCLUDE  'dataformats.inc'
c
c         formal parameters
      integer            datFile
      integer            numRcrds
      integer            ii
c
c         local variables
c . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
c
      do ii=1, numRcrds
         backspace datFile
      enddo
C
      END


C
C........................START PROLOGUE.................................
C
C  SUBPROGRAM NAME:  processArcd
C
C  DESCRIPTION:  Assigns the data for a A_RECORD structure.
C
C  PROGRAMMER, DATE:  Ann Schrader   (SAIC)  June 1998
C
C  USAGE:  call processArcd ( aidRcd%aRecord(ii), recrd, result )
C
C  INPUT PARAMETERS:  
C     atcfRcd - ATCF_RECORD struct containing data
C
C  OUTPUT PARAMETERS:
C     aidRcd - A_RECORD struct to receive data
C     result - return 0 for fail, 1 for success
C
C  ERROR CONDITIONS:
C
C........................MAINTENANCE SECTION............................
C
C  PRINCIPAL VARIABLES AND ARRAYS:
C
C  METHOD:
C
C  RECORD OF CHANGES:
C
C........................END PROLOGUE..................................
C
      subroutine processArcd (aidRcd, atcfRcd, result )
c
      implicit none
c
      INCLUDE  'dataformats.inc'
c
c         formal parameters
      type (A_RECORD)    aidRcd
      type (ATCF_RECORD) atcfRcd
      integer            result
c
c         local variables
c . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
c
      result = 1

      aidRcd%basin = atcfRcd%basin
      read( atcfRcd%cyNum, '(i2)' ) aidRcd%cyNum
      aidRcd%DTG = atcfRcd%DTG
      read( atcfRcd%technum, '(i2)' ) aidRcd%technum
      aidRcd%tech = atcfRcd%tech
      read( atcfRcd%tau, '(i3)' ) aidRcd%tau
      read( atcfRcd%latns, '(f3.1,a1)' ) aidRcd%lat, aidRcd%NS
      read( atcfRcd%lonew, '(f4.1,a1)' ) aidRcd%lon, aidRcd%EW
      read( atcfRcd%vmax, '(i3)' ) aidRcd%vmax
      read( atcfRcd%mslp, '(i4)' ) aidRcd%mslp
      aidRcd%ty = atcfRcd%ty
      read( atcfRcd%rad, '(i3)' ) aidRcd%rad
      aidRcd%windcode = atcfRcd%windcode
      read( atcfRcd%radii(1), '(i4)' ) aidRcd%radii(1)
      read( atcfRcd%radii(2), '(i4)' ) aidRcd%radii(2)
      read( atcfRcd%radii(3), '(i4)' ) aidRcd%radii(3)
      read( atcfRcd%radii(4), '(i4)' ) aidRcd%radii(4)
      read( atcfRcd%radp, '(i4)' ) aidRcd%radp
      read( atcfRcd%rrp, '(i4)' ) aidRcd%rrp
      read( atcfRcd%mrd, '(i3)' ) aidRcd%mrd
      read( atcfRcd%gusts, '(i3)' ) aidRcd%gusts
      read( atcfRcd%eye, '(i3)' ) aidRcd%eye
      read( atcfRcd%subregion, '(a3)' ) aidRcd%subregion
      read( atcfRcd%maxseas, '(i3)' ) aidRcd%maxseas
      aidRcd%initials = atcfRcd%initials
      read( atcfRcd%dir, '(i3)' ) aidRcd%dir
      read( atcfRcd%speed, '(i3)' ) aidRcd%speed
      aidRcd%stormname = atcfRcd%stormname
      aidRcd%depth = atcfRcd%depth
      read( atcfRcd%seas, '(i2)' ) aidRcd%seas
      aidRcd%seascode = atcfRcd%seascode
      read( atcfRcd%seasrad(1), '(i3)' ) aidRcd%seasrad(1)
      read( atcfRcd%seasrad(2), '(i3)' ) aidRcd%seasrad(2)
      read( atcfRcd%seasrad(3), '(i3)' ) aidRcd%seasrad(3)
      read( atcfRcd%seasrad(4), '(i3)' ) aidRcd%seasrad(4)
C
      END


C
C........................START PROLOGUE.................................
C
C  SUBPROGRAM NAME:  convWriteAidData
C
C  DESCRIPTION:  Convert the passed data to an AID_DATA record and write 
C                the AID_DATA record to the output data file, NHC version
C
C  PROGRAMMER, DATE:  Ann Schrader   (SAIC)  August 2003
C
C  USAGE:  call convWriteAidData (datFile,stormID,cdtg,techname,ltlnwnd,
C                                 windcode,radii,result)
C
C  INPUT PARAMETERS:  
C     datFile - unit number of output data file
C     stormID - storm id, eg. cp021997
C     cdtg - current dtg, eg. 1998060912
C     techname - objective aid name, eg. CLIM
C     ltlnwnd - array of integers dimensioned (newnumtau,llw) where 
C               the first dimension is the TAUs 0, 12, 24, 36, 48, 60, 72...
C               and the second dimension is lat, lon and wind
C     windcode - array of strings, dimensioned (newnumtau,4) where
C               the first dimension is the TAUs 0, 12, 24, 36, 48, 60, 72...
C               and the 2nd dimension is the wind intensities: 34, 50, 64 and 100.  
C                Values for the windcode are:
C                     AAA - all quadrants or 
C                     NEQ-northeast quadrant.
C     radii - array of wind radii values, dimension (newnumtau,4,4) where
C               the first dimension is the TAUs 0, 12, 24, 36, 48, 60, 72...
C               the second dimension is the wind intensity: 34,50,64,100
C               and the third dimension is the 4 possible radii quadrants.
C
C  OUTPUT PARAMETERS:
C     result - return 0 for fail, 1 for success
C
C  ERROR CONDITIONS:
C
C........................MAINTENANCE SECTION............................
C
C  PRINCIPAL VARIABLES AND ARRAYS:
C
C  METHOD:
C
C  RECORD OF CHANGES:
C
C........................END PROLOGUE..................................
C
      subroutine convWriteAidData (datFile, stormID, cdtg, techname, 
     1     ltlnwnd, windcode, radii, result )

      include 'dataioparms.inc'
      INCLUDE  'dataformats.inc'
c
c         formal parameters
      integer           datFile
      character*8       stormID
      character*10      cdtg
      character*4       techname
      integer           ltlnwnd(newnumtau,llw)
      character*3       windcode(newnumtau,4)
      integer           radii(newnumtau,4,4)
      integer           result
c
c         local variables
      integer           ii
      type (AID_DATA)   aiddata
c . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
c
C     Create the AID_DATA record from the individual variables and arrays.
      call convToAiddata( stormID, cdtg, techname, ltlnwnd, windcode,
     &     radii, aiddata )

C     Write the AID_DATA record out to the specified file (datFile).
      call writeAidData( datFile, aiddata, result )
C
      END



C
C........................START PROLOGUE.................................
C
C  SUBPROGRAM NAME:  convToAiddata
C
C  DESCRIPTION:  Creates an AID_DATA record using data passed in.
C
C  PROGRAMMER, DATE:  Ann Schrader   (SAIC)  August 2003
C
C  USAGE:  call convToAiddata ( stormID, cdtg, techname, ltlnwnd, windcode,
C                               radii, aiddata )
C
C  INPUT PARAMETERS:  
C     stormID - storm id, eg. cp021997
C     cdtg - current dtg, eg. 1998060912
C     techname - objective aid name, eg. CLIM
C     ltlnwnd - array of integers dimensioned (newnumtau,llw) where 
C               the first dimension is the TAUs 0, 12, 24, 36, 48, 60, 72...
C               and the second dimension is lat, lon and wind
C     windcode - array of strings, dimensioned (newnumtau,4) where
C               the first dimension is the TAUs 0, 12, 24, 36, 48, 60, 72...
C               and the 2nd dimension is the wind intensities: 34, 50, 64 and 100.  
C                Values for the windcode are:
C                     AAA - all quadrants or 
C                     NEQ-northeast quadrant.
C     radii - array of wind radii values, dimension (newnumtau,4,4) where
C               the first dimension is the TAUs 0, 12, 24, 36, 48, 60, 72...
C               the second dimension is the wind intensity: 34,50,64,100
C               and the third dimension is the 4 possible radii quadrants.
C                
C
C  OUTPUT PARAMETERS:
C     aiddata - resulting AID_DATA struct
C
C  ERROR CONDITIONS:
C
C........................MAINTENANCE SECTION............................
C
C  PRINCIPAL VARIABLES AND ARRAYS:
C
C  LANGUAGE:  FORTRAN
C
C  RECORD OF CHANGES:
C  Minor change to records written - don't include 360 longitudes
C  Added due to extra lines written for wind radii cliper  ... sampson 3/24/04
C
C........................END PROLOGUE..................................
C
      subroutine convToAiddata (stormID, cdtg, techname, ltlnwnd, 
     &     windcode, radii, aiddata )
c
      implicit none
c
      include 'dataioparms.inc'
      INCLUDE  'dataformats.inc'
c
c         formal parameters
      character*8       stormID
      character*10      cdtg
      character*4       techname
      integer           ltlnwnd(newnumtau,llw)
      character*3       windcode(newnumtau,4)
      integer           radii(newnumtau,4,4)
      type (AID_DATA)   aiddata
c
c         local variables
      type (A_RECORD)   rcrd
      type (A_RECORD)   aRcrd
      integer           ii, jj, kk, mm, itau
      integer           loopend
      integer           numrcrd
      integer           llwnd(llw)
      integer           wndrad(4)
      integer           rad(4)
      data rad/34,50,64,100/
c . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
c
      numrcrd = 1
      loopend = newnumtau

c     For CARQ and WRNG loop on the taus: -24, -18, -12, -6 and 0
      if (techname.eq.'CARQ' .or. techname.eq.'WRNG') loopend = 5

c     For all other aids loop on the taus: 
c        0, 12, 24, 36, 48, 60, 72, 84, 96, 108 and 120

      do ii = 1, loopend

c        Get the tau.
         if( techname .eq. 'CARQ' .or. techname .eq. 'WRNG' ) then
            itau = -( (5-ii) * 6)
c        else all taus in 12 hr increments
         else
            itau = (ii-1) * 12
         endif

c        Get the lat/lon/wind for this tau
         do jj = 1, llw
            llwnd(jj) = ltlnwnd(ii,jj)
         enddo
      
c        Check for model runs where lat/lon out of range - convert to 0's
c        Handle cases of forecasts crossing 0 longitude,  ajs 1/17/01
         if( llwnd(2) .lt. 0 ) then
            llwnd(2) = llwnd(2) + 3600
         endif
         
         if( llwnd(1) .lt. -900  .or. llwnd(1) .gt.  900  .or.
     &        llwnd(2) .lt. 0  .or. llwnd(2) .ge.  3600 ) then
            llwnd(1) = 0
            llwnd(2) = 0
         endif
c        Check for model runs where wind out of range - convert to 0's
         if( llwnd(3) .lt. 0 .or. llwnd(3) .gt. 300) 
     &        llwnd(3) = 0
         
         if( llwnd(1) .ne. 0 .or. llwnd(2) .ne. 0 
     1        .or. llwnd(3) .ne. 0) then

            do jj = 1, 4
c              Loop thru the four radii, 34,50,64,100 kt.  Always write
c              out the 34 kt radii.  Write out the others if radii have
c              been defined for them.
               if( jj .eq. 1 .or. 
     &              radii(ii,jj,1).gt.0 .or. radii(ii,jj,2).gt.0 .or.
     &              radii(ii,jj,3).gt.0 .or. radii(ii,jj,4).gt.0 ) then
c                 Get the wind radii for this wind intensity.
                  do kk = 1, 4
                     wndrad(kk) = radii(ii,jj,kk)
                  enddo
                  
                  call convToARecord( stormID, cdtg, itau, techname,
     &                 llwnd, rad(jj), windcode(ii,jj), wndrad, aRcrd )
                  
                  aiddata%aRecord(numrcrd) = aRcrd
                  aiddata%numrcrds = numrcrd
                  numrcrd = numrcrd + 1
                  
               endif            ! if 34 kt radii .or. radii .gt. 0
            enddo               ! do jj = 1, 4
         endif                  ! if llwnd(1) .or. llwnd(20 .or. llwnd(3)
      enddo                     ! do ii = 1, loopend
C     
      END



C
C........................START PROLOGUE.................................
C
C  SUBPROGRAM NAME:  convToARecord
C
C  DESCRIPTION:  Creates an A_RECORD record using data passed in.
C
C  PROGRAMMER, DATE:  Ann Schrader   (SAIC)  August 2003
C
C  USAGE:  call convToARecord ( stormID, cdtg, techname, llwnd, rad,
C                               windcode, wndrad, aRcrd )
C
C  INPUT PARAMETERS:  
C     stormID - storm id, eg. cp021997
C     cdtg - current dtg, eg. 1998060912
C     techname - objective aid name, eg. CLIM
C     llwnd - array of integers dimensioned (llw) where 
C               the llw dimension is lat, lon and wind
C     rad - specifies which wind radii, 34, 50, 64 or 100
C     windcode - wind radii code string
C                Values for the windcode are:
C                     AAA - all quadrants or 
C                     NEQ-northeast quadrant.
C     wndrad - array of wind radii values, dimension (4) 
C               representing the 4 possible radii quadrants.
C                
C
C  OUTPUT PARAMETERS:
C     aRcrd - resulting A_RECORD struct
C
C  ERROR CONDITIONS:
C
C........................MAINTENANCE SECTION............................
C
C  PRINCIPAL VARIABLES AND ARRAYS:
C
C  LANGUAGE:  FORTRAN
C
C  RECORD OF CHANGES:
C
C  Added initialization as per Colin McCadie, NHC ... sampson Apr 04
C........................END PROLOGUE..................................
C
      subroutine convToARecord (stormID, cdtg, itau, techname, llwnd, 
     &     rad, windcode, wndrad, aRcrd )
c
      implicit none
c
      include 'dataioparms.inc'
      INCLUDE  'dataformats.inc'
c
c         formal parameters
      character*8       stormID
      character*10      cdtg
      integer           itau
      character*4       techname
      integer           llwnd(llw)
      integer           rad
      character*3       windcode
      integer           wndrad(4)
      type (A_RECORD)   aRcrd
c
c         local variables
      integer           ii
c . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
c
      
c     Assign the data to the A_RECORD
      aRcrd%basin = stormID(1:2)
      call upcase( aRcrd%basin, 2 )
      read( stormID(3:4), '(i2)' ) aRcrd%cyNum
      aRcrd%DTG = cdtg
      if( techname .eq. 'CARQ' ) then
         aRcrd%technum = 1
      else if( techname .eq. 'WRNG' ) then
         aRcrd%technum = 2
      else
         aRcrd%technum = 3
      endif
      aRcrd%tech = techname
      aRcrd%tau = itau
      
c     Convert from -900 thru 900 to 90.0S thru 90.0N
      aRcrd%NS = 'N'
      aRcrd%lat = llwnd(1) / 10.
      if( aRcrd%lat .lt. 0. ) then
         aRcrd%lat = -aRcrd%lat
         aRcrd%NS = 'S'
      endif
c     Convert from 0 thru 3600 (WH < 1800 < EH) to 180.0W thru 180.0E
      aRcrd%EW = 'W'
      aRcrd%lon = llwnd(2) / 10.
      if( aRcrd%lon .gt. 180.0 ) then
         aRcrd%lon = 360.0 - aRcrd%lon
         aRcrd%EW = 'E'
      endif
      aRcrd%vmax = llwnd(3)

      aRcrd%mslp = 0
      aRcrd%ty = " "

c     Assign the wind radii values.
      aRcrd%rad = rad;
      aRcrd%windcode = windcode
      do ii = 1, 4
         aRcrd%radii(ii) = wndrad(ii)
      enddo

      aRcrd%subregion = " "
      aRcrd%initials = " "
      aRcrd%stormname = " "
      aRcrd%depth = " "
      aRcrd%seascode = " "
C
cx
cx from Colin McCadie = had to explicitly set these variables to zero (hp compiler)
      aRcrd%radp = 0
      aRcrd%rrp  = 0
      aRcrd%mrd  = 0
      aRcrd%gusts= 0
      aRcrd%eye  = 0

      aRcrd%maxseas = 0
      aRcrd%dir     = 0
      aRcrd%speed   = 0
      aRcrd%seas    = 0
      aRcrd%seasrad = 0

      END



C
C........................START PROLOGUE.................................
C
C  SUBPROGRAM NAME:  writeAidData
C
C  DESCRIPTION:  Writes an AID_DATA record to the specified file.
C
C  PROGRAMMER, DATE:  Ann Schrader   (SAIC)  August 2003
C
C  USAGE:  call writeAidData ( datFile, aiddata, result )
C
C  INPUT PARAMETERS:  
C     datFile - unit number of the objective aids file
C     aiddata - AID_DATA struct containing data to write out
C
C  OUTPUT PARAMETERS:
C     result - return 0 for fail, 1 for success
C
C  ERROR CONDITIONS:
C
C........................MAINTENANCE SECTION............................
C
C  PRINCIPAL VARIABLES AND ARRAYS:
C
C  LANGUAGE:  FORTRAN
C
C  RECORD OF CHANGES:
C
C........................END PROLOGUE..................................
C
      subroutine writeAidData (datFile, aiddata, result)
c
      implicit none
c
      INCLUDE  'dataformats.inc'
c
c         formal parameters
      integer           datFile
      type (AID_DATA)   aiddata
      integer           result
c
c         local variables
      integer           ii
c . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
c
      result = 1
C
      do ii = 1, aiddata%numrcrds
         call outputArcd( datFile, aiddata%aRecord(ii), result )
      enddo
C
      END



C......................................................................
c
c     This function was added because the NCEP IBM mainframe compiler
c     wouldn't allow testing integers as logicals.   03/2004  A. Schrader
c

      logical function nonzero (ii)
c
      integer ii
c
      if( ii .ne. 0 ) then
         nonzero = .TRUE.
      else
         nonzero = .FALSE.
      endif

      end
C......................................................................


C
C........................START PROLOGUE.................................
C
C  SUBPROGRAM NAME:  outputArcd
C
C  DESCRIPTION:  Writes the A_RECORD to the spectified file.
C
C  PROGRAMMER, DATE:  Ann Schrader   (SAIC)  August 2003
C
C  USAGE:  call outputArcd ( datFile, aidRcd%aRecord(ii), result )
C
C  INPUT PARAMETERS:  
C     datFile - unit number of the objective aids file
C     aidRcd - A_RECORD struct containing record to write out
C
C  OUTPUT PARAMETERS:
C     result - return 0 for fail, 1 for success
C
C  ERROR CONDITIONS:
C
C........................MAINTENANCE SECTION............................
C
C  PRINCIPAL VARIABLES AND ARRAYS:
C
C  LANGUAGE:  FORTRAN
C
C  RECORD OF CHANGES:
C
C........................END PROLOGUE..................................
C
      subroutine outputArcd (datFile, aidRcd, result)
c
      implicit none
c
      INCLUDE  'dataformats.inc'
c
c         formal parameters
      integer           datFile
      type (A_RECORD)   aidRcd
      integer           result
c
c         local variables
      integer           ii
      character*200     line
      character*50      temp
      integer           writeline
      logical           nonzero
c . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
c
      result = 1
c
c     Build the output objective aid line. */
c
c     Start with the basin, cyclone number and date-time-group.
      write( line,8000 ) aidRcd%basin, aidRcd%cyNum, aidRcd%DTG
 8000 format( a2,", ",i2.2,", ",a10,',')

c     Add on the tech number, tech name and tau.
      if( aidRcd%tech .eq. "BEST" ) then
         write( temp, 8010 ) aidRcd%tech, aidRcd%tau
 8010    format( ' ','  , ',a4,", ",i3,',')
      else
         write( temp,8020 ) aidRcd%technum, aidRcd%tech, aidRcd%tau
 8020    format( ' ',i2.2,", ",a4,", ",i3,',')
      endif
      line = line(1:LEN_TRIM(line))//temp(1:LEN_TRIM(temp))

c     Add on the lat, lon and vmax.
      write( temp,8030 ) NINT(aidRcd%lat*10.), aidRcd%NS, 
     &     NINT(aidRcd%lon*10.), aidRcd%EW, aidRcd%vmax
 8030 format( ' ',i3,a1,", ",i4,a1,", ",i3,',')
      line = line(1:LEN_TRIM(line))//temp(1:LEN_TRIM(temp))

c     Add on the mslp and ty.
      write( temp,8040 ) aidRcd%mslp, aidRcd%ty
 8040 format( ' ',i4,", ",a2,',')
      line = line(1:LEN_TRIM(line))//temp(1:LEN_TRIM(temp))

c     Add on the rad, windcode and radii data.
      write( temp,8050 ) aidRcd%rad, aidRcd%windcode, 
     &     (aidRcd%radii(ii),ii=1,4)
 8050 format( ' ',i3,", ",a3,4(", ",i4),',')
      line = line(1:LEN_TRIM(line))//temp(1:LEN_TRIM(temp))

      writeline = 1
c     Add on the last closed isobar(radp), it's radius(rrp),
c     radius of max winds(mrd), gusts and eye diameter.
      if( nonzero(aidRcd%radp) .or. nonzero(aidRcd%rrp) .or. 
     &     nonzero(aidRcd%mrd) .or. nonzero(aidRcd%gusts) .or. 
     &     nonzero(aidRcd%eye) .or. nonzero(aidRcd%maxseas) .or.
     &     nonzero(aidRcd%dir) .or. nonzero(aidRcd%speed) .or. 
     &     nonzero(LEN_TRIM(aidRcd%subregion)) .or.
     &     nonzero(LEN_TRIM(aidRcd%initials)) .or. 
     &     nonzero(LEN_TRIM(aidRcd%stormname)) .or. 
     &     nonzero(LEN(aidRcd%depth)) .or.
     &     nonzero(aidRcd%seasrad(1)) .or.
     &     nonzero(aidRcd%seasrad(2)) .or.
     &     nonzero(aidRcd%seasrad(3)) .or. 
     &     nonzero(aidRcd%seasrad(4)) ) then
         write( temp,8060 ) aidRcd%radp, aidRcd%rrp, aidRcd%mrd, 
     &        aidRcd%gusts, aidRcd%eye
 8060    format( ' ',i4,", ",i4,", ",i3,", ",i3,", ",i3,',')
         line = line(1:LEN_TRIM(line))//temp(1:LEN_TRIM(temp))
         writeline = writeline + 1
      endif

      if( nonzero(aidRcd%maxseas) .or. nonzero(aidRcd%dir) .or. 
     &     nonzero(aidRcd%speed) .or. 
     &     nonzero(LEN_TRIM(aidRcd%initials)) .or. 
     &     nonzero(LEN_TRIM(aidRcd%subregion)) .or.
     &     nonzero(LEN_TRIM(aidRcd%stormname)) .or. 
     &     nonzero(LEN(aidRcd%depth)) .or.
     &     nonzero(aidRcd%seasrad(1)) .or. 
     &     nonzero(aidRcd%seasrad(2)) .or.
     &     nonzero(aidRcd%seasrad(3)) .or. 
     &     nonzero(aidRcd%seasrad(4)) ) then
c        Add the subregion.
         write( temp,8070 ) aidRcd%subregion
 8070    format( ' ',a3,',')
         line = line(1:LEN_TRIM(line))//temp(1:LEN_TRIM(temp))

c        Add the max seas.
         write( temp,8080 ) aidRcd%maxseas
 8080    format( ' ',i3,',')
         line = line(1:LEN_TRIM(line))//temp(1:LEN_TRIM(temp))

c        Add the initials.
         write( temp,8090 ) aidRcd%initials
 8090    format( ' ',a3,',')
         line = line(1:LEN_TRIM(line))//temp(1:LEN_TRIM(temp))

c        Add the dir and speed if available.
         write( temp,9000 ) aidRcd%dir, aidRcd%speed
 9000    format( ' ',i3,", ",i3,',')
         line = line(1:LEN_TRIM(line))//temp(1:LEN_TRIM(temp))
         writeline = writeline + 1
      endif

c     Add the stormname and system depth.
      if( nonzero(LEN_TRIM(aidRcd%stormname)) .or. 
     &    nonzero(LEN(aidRcd%depth)) .or.
     &    nonzero(aidRcd%seasrad(1)) .or. 
     &    nonzero(aidRcd%seasrad(2)) .or.
     &    nonzero(aidRcd%seasrad(3)) .or. 
     &    nonzero(aidRcd%seasrad(4)) ) then
         write( temp,9010 ) aidRcd%stormname, aidRcd%depth
 9010    format( ' ',a10,", ",a1,',')
         line = line(1:LEN_TRIM(line))//temp(1:LEN_TRIM(temp))
         writeline = writeline + 1
      endif

c     Add on the seas, seascode and seas radii data.
      if( nonzero(aidRcd%seasrad(1)) .or. 
     &    nonzero(aidRcd%seasrad(2)) .or.
     &    nonzero(aidRcd%seasrad(3)) .or. 
     &    nonzero(aidRcd%seasrad(4)) ) then
         write( temp,9020 ) aidRcd%seas, aidRcd%seascode, 
     &        (aidRcd%seasrad(ii),ii=1,4)
 9020    format( ' ',i2,", ",a3,", ",i3,", ",i3,", ",i3,", ",i3)
         line = line(1:LEN_TRIM(line))//temp(1:LEN_TRIM(temp))
         writeline = writeline + 1
      endif

c     Write the line out to the data file.
c     Write with different format statements because I didn't want to
c     have to add a compile switch to get variable-format expressions.
c     And I didn't want every line to be 200 characters in length. ajs
      if( writeline .eq. 1 ) then
         write( datFile,'( a100 )') line
      elseif (writeline .eq. 2 ) then
         write( datFile,'( a125 )') line
      elseif (writeline .eq. 3 ) then
         write( datFile,'( a150 )') line
      elseif (writeline .eq. 4 ) then
         write( datFile,'( a165 )') line
      elseif (writeline .eq. 5 ) then
         write( datFile,'( a195 )') line
      endif

C
      END


C
C........................START PROLOGUE.................................
C
C  SUBPROGRAM NAME:  doWriteErrRadRcd
C
C  DESCRIPTION:  Write the error radius record to the output data file.
C
C  PROGRAMMER, DATE:  Jim Goerss   (NRL)  March 2004
C
C  USAGE:  call doWriteErrRadRcd (datFile,stormID,cdtg,techname,itau,llrad)
C
C  INPUT PARAMETERS:  
C     datFile - unit number of output data file
C     stormID - storm id, eg. cp021997
C     cdtg - current dtg, eg. 1998060912
C     techname - objective aid name, eg. CLIM
C     itau - forecast period
C     llrad - array of integers dimensioned (3) where 
C             and the three components are lat, lon and error radius
C
C  OUTPUT PARAMETERS: NONE
C
C  ERROR CONDITIONS:
C
C........................MAINTENANCE SECTION............................
C
C  PRINCIPAL VARIABLES AND ARRAYS:
C
C  METHOD:
C
C  RECORD OF CHANGES:
C
C........................END PROLOGUE..................................
C
      subroutine doWriteErrRadRcd (datFile, stormID, cdtg, techname, 
     1     itau, llrad )

      include 'dataioparms.inc'
c
c         formal parameters
      integer           datFile
      character*8       stormID
      character*10      cdtg
      character*4       techname
      integer           itau
      integer           llrad(llw)
c
c         local variables
      character*2       basin
      character*2       stormnum
      character*1       ns, ew
      character*2       technum
      integer           ilat, ilon
c . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
c
      basin = stormID(1:2)
      call upcase( basin, 2 )
      stormnum = stormID(3:4)

c     Assign the objective aid technique number
      if( techname .eq. 'CARQ' ) then
         technum = '01'
      else if( techname .eq. 'WRNG' ) then
         technum = '02'
      else
         technum = '03'
      endif

cx    Check for model runs where lat/lon out of range - convert to 0's
      if( llrad(2) .lt. 0 ) then
          llrad(2) = llrad(2) + 3600
      endif

      if( llrad(1) .lt. -900  .or. 
     1    llrad(1) .gt.  900  .or.
     2    llrad(2) .lt.    0  .or. 
     3    llrad(2) .gt.  3600 ) then
         llrad(1) = 0
         llrad(2) = 0
      endif
cx    Check for model runs where error radius is out of range - convert to 0's
      if( llrad(3) .lt. 0 .or. llrad(3) .gt. 9999) 
     1     llrad(3) = 0
      
      if( llrad(1) .ne. 0 .or. llrad(2) .ne. 0 
     1     .or. llrad(3) .ne. 0) then
c     Convert from -900 thru 900 to 900S thru 900N
         ns = 'N'
         ilat = llrad(1)
         if( ilat .lt. 0 ) then
            ilat = -ilat
            ns = 'S'
         endif
c     Convert from 0 thru 3600 (WH < 1800 < EH) to 1800W thru 1800E
         ew = 'W'
         ilon = llrad(2)
         if( ilon .gt. 1800 ) then
            ilon = 3600 - ilon
            ew = 'E'
         endif
c     Write the aid record...
cx    only if the lat and lon are meaningful
         if ( ilon .le. 1800 .and. ilat .lt. 900 ) then
            write(datFile,9080) basin, stormnum, cdtg, 
     1           technum, techname, itau, ilat, ns, ilon, ew, 
     1           llrad(3)
 9080       format( A2,", ",A2,", ",A10,", ",A2,", ",A4,", ",
     1           I3,", ",I3,A1,", ",I4,A1,',  , ',I6 )
         endif
      endif
      
C
      END

C
C........................START PROLOGUE.................................
C
C  SUBPROGRAM NAME:  newWriteErrRadRcd
C
C  DESCRIPTION:  Write the error radius record to the output data file,
C                NHC version
C
C  PROGRAMMER, DATE:  Jim Goerss   (NRL)  March 2004
C
C  USAGE:  call newWriteErrRadRcd (datFile,stormID,cdtg,techname,ltlnrad)
C
C  INPUT PARAMETERS:  
C     datFile - unit number of output data file
C     stormID - storm id, eg. cp021997
C     cdtg - current dtg, eg. 1998060912
C     techname - objective aid name, eg. CLIM
C     ltlnrad - array of integers dimensioned (newnumtau,llw) where 
C               the first dimension is the TAUs 0, 12, 24, 36, 48, 60, 72...
C               and the second dimension is lat, lon and error radius
C
C  OUTPUT PARAMETERS: NONE
C
C  ERROR CONDITIONS:
C
C........................MAINTENANCE SECTION............................
C
C  PRINCIPAL VARIABLES AND ARRAYS:
C
C  METHOD:
C
C  RECORD OF CHANGES:
C
C........................END PROLOGUE..................................
C
      subroutine newWriteErrRadRcd (datFile, stormID, cdtg, techname, 
     1     ltlnrad )

      include 'dataioparms.inc'
c
c         formal parameters
      integer           datFile
      character*8       stormID
      character*10      cdtg
      character*4       techname
      integer           ltlnrad(newnumtau,llw)
c
c         local variables
      integer           ii, jj, itau
      integer           llrad(llw)
c . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
c

      loopend = newnumtau

c     For all aids loop on the taus: 
c        0, 12, 24, 36, 48, 60, 72, 84, 96, 108 and 120
      do ii = 1, loopend
	 itau = (ii-1) * 12
         do jj = 1, llw
            llrad(jj) = ltlnrad(ii,jj)
         enddo
         call doWriteErrRadRcd(datFile, stormID, cdtg, techname, itau, 
     &                      llrad )
      enddo
C
      END
C


C
C........................START PROLOGUE.................................
C
C  SUBPROGRAM NAME:  readFRecord
C
C  DESCRIPTION:  Reads one FIX_DATA data info from the input file.
C
C  PROGRAMMER, DATE:  Ann Schrader   (SAIC)  2004
C
C  USAGE:  call readFRecord ( datFile, fixData, result )
C
C  INPUT PARAMETERS:  
C     datFile - unit number of the fixes file
C
C  OUTPUT PARAMETERS:
C     fixData - structure to read data into
C     result - return 0 for fail, neg for unexpected end-of-file
C              1 for success
C
C  ERROR CONDITIONS:
C
C........................MAINTENANCE SECTION............................
C
C  PRINCIPAL VARIABLES AND ARRAYS:
C
C  LANGUAGE:  FORTRAN
C
C  RECORD OF CHANGES:
C
C........................END PROLOGUE..................................
C
      subroutine readFRecord (datFile, fixData, result)
c
      implicit none
c
      INCLUDE  'dataformats.inc'
c
c         formal parameters
      integer         datFile
      type (FIX_DATA) fixData
      integer         result
c
c         local variables
      integer            ii
      integer            readStat
      type (FIX_RECORD)  recrd
      character          savDTG*12
      character          savfixtype*4
      logical*2          done
c . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
c
      result = 0
c
c     Read the next record in the data file
      call readNextFix( datFile, recrd, readStat )
c
c     Save the date-time-group and the fixtype
      if( readStat .eq. 0 ) then
         result = 1
         savDTG = recrd%DTG
         savfixtype = recrd%fixtype
      endif
c
c     Read all of the radii for this DTG and fixtype
      ii=0
      fixData%numrcrds = 0
      done = .false.
      do while (result.eq.1 .and. readStat.eq.0
     &          .and. ii.lt.AidRadMax .and. .not.done)
         ii = ii + 1
c
c        Process the F_RECORD
         call processFrcd( fixData%fRecord(ii), recrd, result )
         fixData%numrcrds = ii
c
c        Read the next record in the data file
         call readNextFix( datFile, recrd, readStat )
c
c        If new dtg or fixtype or rad is not 50, 64 or 100 then 
c        flag done and backup one record.
         if( readStat.eq.0 .and. (savDTG.ne.recrd%DTG .or. 
     &        savfixtype.ne.recrd%fixtype .or. recrd%rad.eq.'   '
     &        .or. recrd%rad.eq.' 34') ) then
            done = .true.
            backspace( datFile )
         endif
      enddo

      if( readStat .gt. 0 ) result = 0
      if( readStat .lt. 0 .and. fixData%numrcrds .eq. 0 ) 
     &     result = readStat
C
      END


C
C........................START PROLOGUE.................................
C
C  SUBPROGRAM NAME:  processFrcd
C
C  DESCRIPTION:  Assigns the data for an F_RECORD structure.
C
C  PROGRAMMER, DATE:  Ann Schrader   (SAIC)  2004
C
C  USAGE:  call processFrcd ( fixData%fRecord(ii), recrd, result )
C
C  INPUT PARAMETERS:  
C     recrd - FIX_RECORD struct containing fix data
C
C  OUTPUT PARAMETERS:
C     fixRcd - F_RECORD struct to receive data
C     result - return 0 for fail, 1 for success
C
C  ERROR CONDITIONS:
C
C........................MAINTENANCE SECTION............................
C
C  PRINCIPAL VARIABLES AND ARRAYS:
C
C  METHOD:
C
C  RECORD OF CHANGES:
C
C........................END PROLOGUE..................................
C
      subroutine processFrcd (fixRcd, recrd, result )
c
      implicit none
c
      INCLUDE  'dataformats.inc'
c
c         formal parameters
      type (F_RECORD)    fixRcd
      type (FIX_RECORD)  recrd
      integer            result
c
c         local variables
      integer   ii
c . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
c
      result = 1

      fixRcd%basin = recrd%basin
      read( recrd%cyNum, '(i2)' ) fixRcd%cyNum
      fixRcd%DTG = recrd%DTG
      read( recrd%fixformat, '(i3)' ) fixRcd%fixformat
      fixRcd%fixtype = recrd%fixtype
      fixRcd%centerFix = .false.
      fixRcd%intensityFix = .false.
      fixRcd%radiiFix = .false.
      fixRcd%pressureFix = .false.
      do ii = 1, LEN( recrd%cicode )
         if( recrd%cicode(ii:ii) .eq. 'C' ) then
            fixRcd%centerFix = .true.
         else if( recrd%cicode(ii:ii) .eq. 'I' ) then
            fixRcd%intensityFix = .true.
         else if( recrd%cicode(ii:ii) .eq. 'R' ) then
            fixRcd%radiiFix = .true.
         else if( recrd%cicode(ii:ii) .eq. 'P' ) then
            fixRcd%pressureFix = .true.
         endif
      enddo
      fixRcd%flagged = recrd%flagged
      read( recrd%latns, '(f4.2,a1)' ) fixRcd%lat, fixRcd%NS
      read( recrd%lonew, '(f5.2,a1)' ) fixRcd%lon, fixRcd%EW
      read( recrd%height, '(i5)' ) fixRcd%height
      read( recrd%positConf, '(i1)' ) fixRcd%positConf
      read( recrd%v, '(i3)' ) fixRcd%v
      read( recrd%vConf, '(i1)' ) fixRcd%vConf
      read( recrd%pressure, '(i4)' ) fixRcd%pressure
      read( recrd%presConf, '(i1)' ) fixRcd%presConf
      fixRcd%presDeriv = recrd%presDeriv
      read( recrd%rad, '(i3)' ) fixRcd%rad
      fixRcd%windcode = recrd%windcode
      read( recrd%radii(1), '(i4)' ) fixRcd%radii(1)
      read( recrd%radii(2), '(i4)' ) fixRcd%radii(2)
      read( recrd%radii(3), '(i4)' ) fixRcd%radii(3)
      read( recrd%radii(4), '(i4)' ) fixRcd%radii(4)
      do ii = 1, 4
         fixRcd%edge(ii) = .false.
         fixRcd%cut(ii) = .false.
      enddo
      do ii = 1, 4
         if( recrd%radMod(ii) .eq. 'E' ) then
            fixRcd%edge(ii) = .true.
         else if( recrd%radMod(ii) .eq. 'C' ) then
            fixRcd%cut(ii) = .true.
         else if( recrd%radMod(ii) .eq. 'B' ) then
            fixRcd%edge(ii) = .true.
            fixRcd%cut(ii) = .true.
         endif
      enddo
      read( recrd%radConf, '(i1)' ) fixRcd%radConf
      read( recrd%mrd, '(i3)' ) fixRcd%mrd
      read( recrd%eye, '(i3)' ) fixRcd%eye
      fixRcd%subregion = recrd%subregion
      fixRcd%fixsite = recrd%fixsite
      fixRcd%initials = recrd%initials
c
c      print*, 'basin = ',fixRcd%basin
c      print*, 'DTG = ',fixRcd%DTG
c      print*, 'fixformat = ',fixRcd%fixformat
c      print*, 'fixtype = ',fixRcd%fixtype
c      print*, 'centerFix = ',fixRcd%centerFix
c      print*, 'intensityFix = ',fixRcd%intensityFix
c      print*, 'radiiFix = ',fixRcd%radiiFix
c      print*, 'pressureFix = ',fixRcd%pressureFix
c      print*, 'flagged = ',fixRcd%flagged
c      print*, 'lat NS = ',fixRcd%lat, fixRcd%NS
c      print*, 'lon EW = ',fixRcd%lon, fixRcd%EW
c      print*, 'height = ',fixRcd%height
c      print*, 'positConf = ',fixRcd%positConf
c      print*, 'v, vConf = ',fixRcd%v, fixRcd%vConf
c      print*, 'pressure = ',fixRcd%pressure
c      print*, 'presConf = ',fixRcd%presConf
c      print*, 'presDeriv = ',fixRcd%presDeriv
c      print*, 'rad = ',fixRcd%rad
c      print*, 'radii = ',(fixRcd%radii(ii),ii=1,4)
c      print*, 'edge = ',(fixRcd%edge(ii),ii=1,4)
c      print*, 'cut = ',(fixRcd%cut(ii),ii=1,4)
c      print*, 'radConf = ',fixRcd%radConf
c      print*, 'mrd = ',fixRcd%mrd
c      print*, 'eye = ',fixRcd%eye
c      print*, 'subregion = ',fixRcd%subregion
c      print*, 'fixsite = ',fixRcd%fixsite
c      print*, 'initials = ',fixRcd%initials
c
c        Process the rest of the record
      if( fixRcd%fixformat .eq. DVTStype ) then
         call processDVTS( fixRcd, recrd )
      else if( fixRcd%fixformat .eq. DVTOtype ) then
         call processDVTO( fixRcd, recrd )
      else if( fixRcd%fixformat .eq. MICRtype .or.
     &        fixRcd%fixformat .eq. SCATtype ) then
         call processMicrowave( fixRcd, recrd )
      else if( fixRcd%fixformat .eq. RDRtype ) then
         call processRadar( fixRcd, recrd )
      else if( fixRcd%fixformat .eq. AIRCtype ) then
         call processAircraft( fixRcd, recrd )
      else if( fixRcd%fixformat .eq. DROPtype ) then
         call processDropsonde( fixRcd, recrd )
      else if( fixRcd%fixformat .eq. ANALtype ) then
         call processAnalysis( fixRcd, recrd )
      endif
         
C
      END


C
C........................START PROLOGUE.................................
C
C  SUBPROGRAM NAME:  processDvorak
C
C  DESCRIPTION:  Assigns the data for a DVORAK fix structure.
C
C  PROGRAMMER, DATE:  Ann Schrader   (SAIC)  2004
C
C  USAGE:  call processDvorak( dPtr, fixPtr, column,  option )
C
C  INPUT PARAMETERS:  
C     fixRcd - FIX_RECORD struct containing fix data
C     col - column position of fix type specific portion of
C           fix record
C     option - 'short-term' or 'long-term'
C
C  OUTPUT PARAMETERS:
C     dPtr - DVORAK structure to receive data
C
C  ERROR CONDITIONS:
C
C........................MAINTENANCE SECTION............................
C
C  PRINCIPAL VARIABLES AND ARRAYS:
C
C  METHOD:
C
C  RECORD OF CHANGES:
C
C........................END PROLOGUE..................................
C
      subroutine processDvorak (dvrk, fixRcd, col, option )
c
      implicit none
c
      INCLUDE  'dataformats.inc'
c
c         formal parameters
      type (DVORAK)     dvrk
      type (FIX_RECORD) fixRcd
      integer           col
      character         option*10
c
c         local variables
c
c . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
c
c
c     Don't process the t-number, ci number or intensity change for
c     the short term trend dvorak.

      if( option(1:9) .eq. 'long-term' ) then
         dvrk%tnum = 0
         dvrk%cinum = 0
         if( fixRcd%remainder(col:col+1) .ne. '//' .and.
     &        fixRcd%remainder(col:col+1) .ne. '  ' ) then
            read( fixRcd%remainder(col:col+1), '(i2)' ) dvrk%tnum
         endif
         col = col + 2
         if( fixRcd%remainder(col:col+1) .ne. '//' .and.
     &        fixRcd%remainder(col:col+1) .ne. '  ' ) then
            read( fixRcd%remainder(col:col+1), '(i2)' ) dvrk%cinum
         endif
         col = col + 2
         dvrk%intChg = fixRcd%remainder(col:col)
         col = col + 1
      endif
      dvrk%tnumChg = 0
      dvrk%lastEvalHrsAgo = 0
      dvrk%pastChg = fixRcd%remainder(col:col)
      col = col + 1
      if( fixRcd%remainder(col:col+1) .ne. '//' .and.
     &     fixRcd%remainder(col:col+1) .ne. '  ' ) then
         read( fixRcd%remainder(col:col+1), '(i2)' ) dvrk%tnumChg
      endif
      col = col + 2
      if( fixRcd%remainder(col:col+1) .ne. '//' .and.
     &     fixRcd%remainder(col:col+1) .ne. '  ' ) then
         read( fixRcd%remainder(col:col+1), '(i2)' ) dvrk%lastEvalHrsAgo
      endif
      col = col + 2
c     add 2 for the comma and space
      col = col + 2
         
C
      END


C
C........................START PROLOGUE.................................
C
C  SUBPROGRAM NAME:  processDVTS
C
C  DESCRIPTION:  Assigns the data for a subjective dvorak fix record
C
C  PROGRAMMER, DATE:  Ann Schrader   (SAIC)  2004
C
C  USAGE:  call processDVTS( f_rcrd, fixPtr )
C
C  INPUT PARAMETERS:  
C     fixRcd - FIX_RECORD struct containing fix data
C
C  OUTPUT PARAMETERS:
C     f_rcrd - F_RECORD structure to receive data
C
C  ERROR CONDITIONS:
C
C........................MAINTENANCE SECTION............................
C
C  PRINCIPAL VARIABLES AND ARRAYS:
C
C  METHOD:
C
C  RECORD OF CHANGES:
C
C........................END PROLOGUE..................................
C
      subroutine processDVTS (f_rcrd, fixRcd )
c
      implicit none
c
      INCLUDE  'dataformats.inc'
c
c         formal parameters
      type (F_RECORD)   f_rcrd
      type (FIX_RECORD) fixRcd
c
c         local variables
      integer           col
c
c . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
c
      col = 1
      f_rcrd%dvts%sensor = fixRcd%remainder(col:col+3)
      col = col + 6
      read( fixRcd%remainder(col:col), '(i1)' ) f_rcrd%dvts%pcn
      col = col + 3
      call processDvorak( f_rcrd%dvts%longTerm, fixRcd, col, 
     &     'long-term' )
      call processDvorak( f_rcrd%dvts%shortTerm, fixRcd, col, 
     &     'short-term' )
      read( fixRcd%remainder(col:col+1), '(i2)' ) f_rcrd%dvts%ci24hr
      col = col + 4
      f_rcrd%dvts%satType = fixRcd%remainder(col:col+5)
      col = col + 8
      f_rcrd%dvts%centertype = fixRcd%remainder(col:col+3)
      col = col + 6
      f_rcrd%dvts%tropical = fixRcd%remainder(col:col)
      col = col + 3
      f_rcrd%dvts%comments = fixRcd%remainder(col:col+51)

c      print*,'sensor = ',f_rcrd%dvts%sensor
c      print*,'pcn = ',f_rcrd%dvts%pcn
c      print*,'longterm dvk = ',f_rcrd%dvts%longTerm
c      print*,'shortterm dvk = ',f_rcrd%dvts%shortTerm
c      print*,'ci24hr = ',f_rcrd%dvts%ci24hr
c      print*,'satType = ',f_rcrd%dvts%satType
c      print*,'centertype = ',f_rcrd%dvts%centertype
c      print*,'tropical = ',f_rcrd%dvts%tropical
c      print*,'comments = ',f_rcrd%dvts%comments
C
      END


C
C........................START PROLOGUE.................................
C
C  SUBPROGRAM NAME:  processDVTO
C
C  DESCRIPTION:  Assigns the data for an objective dvorak fix record
C
C  PROGRAMMER, DATE:  Ann Schrader   (SAIC)  2004
C
C  USAGE:  call processDVTO( f_rcrd, fixPtr )
C
C  INPUT PARAMETERS:  
C     fixRcd - FIX_RECORD struct containing fix data
C
C  OUTPUT PARAMETERS:
C     f_rcrd - F_RECORD structure to receive data
C
C  ERROR CONDITIONS:
C
C........................MAINTENANCE SECTION............................
C
C  PRINCIPAL VARIABLES AND ARRAYS:
C
C  METHOD:
C
C  RECORD OF CHANGES:
C
C........................END PROLOGUE..................................
C
      subroutine processDVTO (f_rcrd, fixRcd )
c
      implicit none
c
      INCLUDE  'dataformats.inc'
c
c         formal parameters
      type (F_RECORD)   f_rcrd
      type (FIX_RECORD) fixRcd
c
c         local variables
      integer           col
c
c . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
c
      col = 1
      f_rcrd%dvto%sensor = fixRcd%remainder(col:col+3)
      col = col + 6
      read( fixRcd%remainder(col:col+1), '(i2)' ) f_rcrd%dvto%cinum
      col = col + 4
      read( fixRcd%remainder(col:col), '(i1)' ) f_rcrd%dvto%ciConf
      col = col + 3
      read( fixRcd%remainder(col:col+1), '(i2)' ) f_rcrd%dvto%tnumAvg
      col = col + 4
      read( fixRcd%remainder(col:col+2), '(i3)' ) 
     &     f_rcrd%dvto%tnumAvgTime
      col = col + 5
      f_rcrd%dvto%tnumAvgDeriv = fixRcd%remainder(col:col)
      col = col + 3
      read( fixRcd%remainder(col:col+1), '(i2)' ) f_rcrd%dvto%tnumRaw
      col = col + 4
c     If no eyeTemp assign -999 to signify none
      if( fixRcd%remainder(col:col+3) .eq. '    ' ) then
         f_rcrd%dvto%eyeTemp = -999
      else
         read( fixRcd%remainder(col:col+3), '(i4)' ) f_rcrd%dvto%eyeTemp
      endif
      col = col + 6
c     If no cloudTemp assign -999 to signify none
      if( fixRcd%remainder(col:col+3) .eq. '    ' ) then
         f_rcrd%dvto%cloudTemp = -999
      else
         read( fixRcd%remainder(col:col+3), '(i4)' ) 
     &        f_rcrd%dvto%cloudTemp
      endif
      col = col + 6
      f_rcrd%dvto%sceneType = fixRcd%remainder(col:col+3)
      col = col + 6
      f_rcrd%dvto%algorithm = fixRcd%remainder(col:col+1)
      col = col + 4
      f_rcrd%dvto%satType = fixRcd%remainder(col:col+5)
      col = col + 8
      f_rcrd%dvto%tropical = fixRcd%remainder(col:col)
      col = col + 3
      f_rcrd%dvto%comments = fixRcd%remainder(col:col+51)
         
C
      END


C
C........................START PROLOGUE.................................
C
C  SUBPROGRAM NAME:  processMicrowave
C
C  DESCRIPTION:  Assigns the data for a microwave fix record
C
C  PROGRAMMER, DATE:  Ann Schrader   (SAIC)  2004
C
C  USAGE:  call processMicrowave( f_rcrd, fixPtr )
C
C  INPUT PARAMETERS:  
C     fixRcd - FIX_RECORD struct containing fix data
C
C  OUTPUT PARAMETERS:
C     f_rcrd - F_RECORD structure to receive data
C
C  ERROR CONDITIONS:
C
C........................MAINTENANCE SECTION............................
C
C  PRINCIPAL VARIABLES AND ARRAYS:
C
C  METHOD:
C
C  RECORD OF CHANGES:
C
C........................END PROLOGUE..................................
C
      subroutine processMicrowave (f_rcrd, fixRcd )
c
      implicit none
c
      INCLUDE  'dataformats.inc'
c
c         formal parameters
      type (F_RECORD)   f_rcrd
      type (FIX_RECORD) fixRcd
c
c         local variables
      integer           col
      integer           ii
c
c . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
c
      col = 1
      f_rcrd%micro%rain = .false.
      if( fixRcd%remainder(col:col) .eq. 'R' ) 
     &     f_rcrd%micro%rain = .true.
      col = col + 3
      read( fixRcd%remainder(col:col+2), '(i3)' ) 
     &     f_rcrd%micro%rainrate
      col = col + 5
      f_rcrd%micro%algorithm = fixRcd%remainder(col:col+5)
      col = col + 8
      read( fixRcd%remainder(col:col+1), '(i2)' ) f_rcrd%micro%wave
      col = col + 4
      read( fixRcd%remainder(col:col+3), '(i4)' ) f_rcrd%micro%temp
      col = col + 6
      read( fixRcd%remainder(col:col+3), '(i4)' ) f_rcrd%micro%slpraw
      col = col + 6
      read( fixRcd%remainder(col:col+3), '(i4)' ) 
     &     f_rcrd%micro%slpretr
      col = col + 6
      read( fixRcd%remainder(col:col+2), '(i3)' ) f_rcrd%micro%seas
      col = col + 5
      f_rcrd%micro%satType = fixRcd%remainder(col:col+5)
      col = col + 8
      read( fixRcd%remainder(col:col+2), '(i3)' ) f_rcrd%micro%rad
      col = col + 5
      f_rcrd%micro%windcode = fixRcd%remainder(col:col+3)
      col = col + 6
c     call RemoveSpaces() ?????   ajs
      do ii=1, 8
         read( fixRcd%remainder(col:col+3), '(i4)' ) 
     &        f_rcrd%micro%radii(ii)
         col = col + 6
      enddo
      do ii=1, 8
         f_rcrd%micro%edge = .false.
         f_rcrd%micro%cut = .false.
      enddo
      do ii=1, 8
         if( fixRcd%remainder(col:col) .eq. 'E' ) then
            f_rcrd%micro%edge(ii) = .true.
         else if( fixRcd%remainder(col:col) .eq. 'C' ) then
            f_rcrd%micro%cut(ii) = .true.
         else if( fixRcd%remainder(col:col) .eq. 'B' ) then
            f_rcrd%micro%edge(ii) = .true.
            f_rcrd%micro%cut(ii) = .true.
         endif
         col = col + 3
      enddo
      read( fixRcd%remainder(col:col), '(i1)' ) 
     &     f_rcrd%micro%radConf
      col = col + 3
      f_rcrd%micro%comments = fixRcd%remainder(col:col+51)
         
C
      END


C
C........................START PROLOGUE.................................
C
C  SUBPROGRAM NAME:  processRadar
C
C  DESCRIPTION:  Assigns the data for a radar fix record
C
C  PROGRAMMER, DATE:  Ann Schrader   (SAIC)  2004
C
C  USAGE:  call processRadar( f_rcrd, fixPtr )
C
C  INPUT PARAMETERS:  
C     fixRcd - FIX_RECORD struct containing fix data
C
C  OUTPUT PARAMETERS:
C     f_rcrd - F_RECORD structure to receive data
C
C  ERROR CONDITIONS:
C
C........................MAINTENANCE SECTION............................
C
C  PRINCIPAL VARIABLES AND ARRAYS:
C
C  METHOD:
C
C  RECORD OF CHANGES:
C
C........................END PROLOGUE..................................
C
      subroutine processRadar (f_rcrd, fixRcd )
c
      implicit none
c
      INCLUDE  'dataformats.inc'
c
c         formal parameters
      type (F_RECORD)   f_rcrd
      type (FIX_RECORD) fixRcd
c
c         local variables
      integer           col
      integer           ii
c
c . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
c
      col = 1
      f_rcrd%radar%rdrtype = fixRcd%remainder(col:col)
      col = col + 3
      f_rcrd%radar%plainlanguage = .false.
      f_rcrd%radar%radobcode = .false.
      f_rcrd%radar%doppler = .false.
      if( fixRcd%remainder(col:col) .eq. 'P' ) then
         f_rcrd%radar%plainlanguage = .true.
      elseif( fixRcd%remainder(col:col) .eq. 'R' ) then
         f_rcrd%radar%radobcode = .true.
      elseif( fixRcd%remainder(col:col) .eq. 'D' ) then
         f_rcrd%radar%doppler = .true.
      endif
      col = col + 3
      f_rcrd%radar%radob = fixRcd%remainder(col:col+9)
      col = col + 12
      f_rcrd%radar%eyeShape = fixRcd%remainder(col:col+1)
      col = col + 4
      read( fixRcd%remainder(col:col+1), '(i2)' ) 
     &     f_rcrd%radar%eyeWallOb
      col = col + 4
      read( fixRcd%remainder(col:col+1), '(i2)' ) 
     &     f_rcrd%radar%spiralOv
      col = col + 4
      read( fixRcd%remainder(col:col+4), '(f4.2,a1)' ) 
     &     f_rcrd%radar%lat, f_rcrd%radar%NS
      col = col + 7
      read( fixRcd%remainder(col:col+5), '(f5.2,a1)' ) 
     &     f_rcrd%radar%lon, f_rcrd%radar%EW
      col = col + 8
      read( fixRcd%remainder(col:col+2), '(i3)' ) f_rcrd%radar%vmaxin
      col = col + 5
      read( fixRcd%remainder(col:col+2), '(i3)' ) 
     &     f_rcrd%radar%azimuthin
      col = col + 5
      read( fixRcd%remainder(col:col+2), '(i3)' ) 
     &     f_rcrd%radar%rangein
      col = col + 5
      read( fixRcd%remainder(col:col+4), '(i5)' ) f_rcrd%radar%elevin
      col = col + 7
      read( fixRcd%remainder(col:col+2), '(i3)' ) 
     &     f_rcrd%radar%vmaxout
      col = col + 5
      read( fixRcd%remainder(col:col+2), '(i3)' ) 
     &     f_rcrd%radar%azimuthout
      col = col + 5
      read( fixRcd%remainder(col:col+2), '(i3)' ) 
     &     f_rcrd%radar%rangeout
      col = col + 5
      read( fixRcd%remainder(col:col+4), '(i5)' ) 
     &     f_rcrd%radar%elevout
      col = col + 7
      read( fixRcd%remainder(col:col+4), '(i5)' ) 
     &     f_rcrd%radar%cloudHeight
      col = col + 7
      read( fixRcd%remainder(col:col+4), '(f5.2)' )
     &     f_rcrd%radar%rainAccum
      col = col + 7
      read( fixRcd%remainder(col:col+2), '(i3)' ) 
     &     f_rcrd%radar%rainAcTimeInt
      col = col + 5
      read( fixRcd%remainder(col:col+4), '(f4.2,a1)' ) 
     &     f_rcrd%radar%rainlat, f_rcrd%radar%rainNS
      col = col + 7
      read( fixRcd%remainder(col:col+5), '(f5.2,a1)' ) 
     &     f_rcrd%radar%rainlon, f_rcrd%radar%rainEW
      col = col + 8
      f_rcrd%radar%comments = fixRcd%remainder(col:col+51)
         
C
      END


C
C........................START PROLOGUE.................................
C
C  SUBPROGRAM NAME:  processAircraft
C
C  DESCRIPTION:  Assigns the data for an aircraft fix record
C
C  PROGRAMMER, DATE:  Ann Schrader   (SAIC)  2004
C
C  USAGE:  call processAircraft( f_rcrd, fixPtr )
C
C  INPUT PARAMETERS:  
C     fixRcd - FIX_RECORD struct containing fix data
C
C  OUTPUT PARAMETERS:
C     f_rcrd - F_RECORD structure to receive data
C
C  ERROR CONDITIONS:
C
C........................MAINTENANCE SECTION............................
C
C  PRINCIPAL VARIABLES AND ARRAYS:
C
C  METHOD:
C
C  RECORD OF CHANGES:
C
C........................END PROLOGUE..................................
C
      subroutine processAircraft (f_rcrd, fixRcd )
c
      implicit none
c
      INCLUDE  'dataformats.inc'
c
c         formal parameters
      type (F_RECORD)   f_rcrd
      type (FIX_RECORD) fixRcd
c
c         local variables
      integer           col
      integer           ii
c
c . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
c
      col = 1
      read( fixRcd%remainder(col:col+1), '(i2)' ) 
     &     f_rcrd%air%FlightLevelFt
      col = col + 4
      read( fixRcd%remainder(col:col+2), '(i3)' ) 
     &     f_rcrd%air%FlightLevelMB
      col = col + 5
      read( fixRcd%remainder(col:col+3), '(i4)' ) 
     &     f_rcrd%air%MinHeight
      col = col + 6
      read( fixRcd%remainder(col:col+2), '(i3)' ) 
     &     f_rcrd%air%MaxSurfaceWind
      col = col + 5
      read( fixRcd%remainder(col:col+2), '(i3)' ) 
     &     f_rcrd%air%MaxSurfaceWindBearing
      col = col + 5
      read( fixRcd%remainder(col:col+2), '(i3)' ) 
     &     f_rcrd%air%MaxSurfaceWindRange
      col = col + 5
      read( fixRcd%remainder(col:col+2), '(i3)' ) 
     &     f_rcrd%air%MaxFlightLevelWindDir
      col = col + 5
      read( fixRcd%remainder(col:col+2), '(i3)' ) 
     &     f_rcrd%air%MaxFlightLevelWindInt
      col = col + 5
      read( fixRcd%remainder(col:col+2), '(i3)' ) 
     &     f_rcrd%air%MaxFlightLevelWindBearing
      col = col + 5
      read( fixRcd%remainder(col:col+2), '(i3)' ) 
     &     f_rcrd%air%MaxFlightLevelWindRange
      col = col + 5
      read( fixRcd%remainder(col:col+3), '(i4)' ) 
     &     f_rcrd%air%MinSeaLevelPressure
      col = col + 6
      read( fixRcd%remainder(col:col+2), '(i3)' ) 
     &     f_rcrd%air%EyeTempOutside
      col = col + 5
      read( fixRcd%remainder(col:col+2), '(i3)' ) 
     &     f_rcrd%air%EyeTempInside
      col = col + 5
      read( fixRcd%remainder(col:col+2), '(i3)' ) 
     &     f_rcrd%air%DewPoint
      col = col + 5
      read( fixRcd%remainder(col:col+1), '(i2)' ) 
     &     f_rcrd%air%SeaSurface
      col = col + 4
      read( fixRcd%remainder(col:col+1), '(i2)' ) 
     &     f_rcrd%air%WallCloudThickness
      col = col + 4
      f_rcrd%air%EyeShape = 0
      if( fixRcd%remainder(col:col+1) .eq. 'CI' ) then
         f_rcrd%air%EyeShape = 1
      elseif( fixRcd%remainder(col:col+1) .eq. 'EL' ) then
         f_rcrd%air%EyeShape = 2
      elseif( fixRcd%remainder(col:col+1) .eq. 'CO' ) then
         f_rcrd%air%EyeShape = 3
      endif
      col = col + 4
      read( fixRcd%remainder(col:col+2), '(i3)' ) 
     &     f_rcrd%air%EyeOrientation
      col = col + 5
      read( fixRcd%remainder(col:col+1), '(i2)' ) 
     &     f_rcrd%air%DiameterLongAxis
      col = col + 4
      read( fixRcd%remainder(col:col+1), '(i2)' ) 
     &     f_rcrd%air%DiameterShortAxis
      col = col + 4
      read( fixRcd%remainder(col:col+2), '(f3.1)' )
     &     f_rcrd%air%NavigationalAccuracy
      col = col + 5
      read( fixRcd%remainder(col:col+2), '(f3.1)' )
     &     f_rcrd%air%NavigationalMeteorological
      col = col + 5
      read( fixRcd%remainder(col:col+1), '(i2)' ) 
     &     f_rcrd%air%MissionNumber
      col = col + 4
      f_rcrd%air%comments = fixRcd%remainder(col:col+51)
         
C
      END


C
C........................START PROLOGUE.................................
C
C  SUBPROGRAM NAME:  processDropsonde
C
C  DESCRIPTION:  Assigns the data for a dropsonde fix record
C
C  PROGRAMMER, DATE:  Ann Schrader   (SAIC)  2004
C
C  USAGE:  call processDropsonde( f_rcrd, fixPtr )
C
C  INPUT PARAMETERS:  
C     fixRcd - FIX_RECORD struct containing fix data
C
C  OUTPUT PARAMETERS:
C     f_rcrd - F_RECORD structure to receive data
C
C  ERROR CONDITIONS:
C
C........................MAINTENANCE SECTION............................
C
C  PRINCIPAL VARIABLES AND ARRAYS:
C
C  METHOD:
C
C  RECORD OF CHANGES:
C
C........................END PROLOGUE..................................
C
      subroutine processDropsonde (f_rcrd, fixRcd )
c
      implicit none
c
      INCLUDE  'dataformats.inc'
c
c         formal parameters
      type (F_RECORD)   f_rcrd
      type (FIX_RECORD) fixRcd
c
c         local variables
      integer           col
      integer           ii
c
c . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
c
      col = 1
      f_rcrd%drop%sondeEnv = fixRcd%remainder(col:col+9)
      col = col + 12
      read( fixRcd%remainder(col:col+2), '(i3)' ) 
     &     f_rcrd%drop%height150
      col = col + 5
      read( fixRcd%remainder(col:col+2), '(i3)' ) 
     &     f_rcrd%drop%vspd150
      col = col + 5
      read( fixRcd%remainder(col:col+2), '(i3)' ) 
     &     f_rcrd%drop%vspd500
      col = col + 5
      f_rcrd%drop%comments = fixRcd%remainder(col:col+51)
         
C
      END


C
C........................START PROLOGUE.................................
C
C  SUBPROGRAM NAME:  processAnalysis
C
C  DESCRIPTION:  Assigns the data for an analysis fix record
C
C  PROGRAMMER, DATE:  Ann Schrader   (SAIC)  2004
C
C  USAGE:  call processAnalysis( f_rcrd, fixPtr )
C
C  INPUT PARAMETERS:  
C     fixRcd - FIX_RECORD struct containing fix data
C
C  OUTPUT PARAMETERS:
C     f_rcrd - F_RECORD structure to receive data
C
C  ERROR CONDITIONS:
C
C........................MAINTENANCE SECTION............................
C
C  PRINCIPAL VARIABLES AND ARRAYS:
C
C  METHOD:
C
C  RECORD OF CHANGES:
C
C........................END PROLOGUE..................................
C
      subroutine processAnalysis (f_rcrd, fixRcd )
c
      implicit none
c
      INCLUDE  'dataformats.inc'
c
c         formal parameters
      type (F_RECORD)   f_rcrd
      type (FIX_RECORD) fixRcd
c
c         local variables
      integer           col
      integer           ii
c
c . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
c
      col = 1
      f_rcrd%anal%initials = fixRcd%remainder(col:col+2)
      col = col + 5
      f_rcrd%anal%startDTG = fixRcd%remainder(col:col+11)
      col = col + 14
      f_rcrd%anal%endDTG = fixRcd%remainder(col:col+11)
      col = col + 14
      read( fixRcd%remainder(col:col+2), '(i3)' ) 
     &     f_rcrd%anal%DistanceToNearestData
      col = col + 5
      read( fixRcd%remainder(col:col+3), '(i4)' ) f_rcrd%anal%SST
      col = col + 6
      f_rcrd%anal%obSources = fixRcd%remainder(col:col+23)
      col = col + 26
      f_rcrd%anal%comments = fixRcd%remainder(col:col+51)
         
C
      END


C
C........................START PROLOGUE.................................
C
C  SUBPROGRAM NAME:  buildDvorak
C
C  DESCRIPTION:  Builds a long or short term trend dvorak code.
C
C  PROGRAMMER, DATE:  Ann Schrader   (SAIC)  April 2005
C
C  USAGE:  call buildDvorak ( fixRcd%dvts, 'long', dvorakStr )
C
C  INPUT PARAMETERS:  
C     fix - DVTS_RECORD containing the dvts data 
C     option - 'long' or 'short'
C
C  OUTPUT PARAMETERS:
C     dvorakStr - returned dvorak code
C
C  ERROR CONDITIONS:
C
C........................MAINTENANCE SECTION............................
C
C  PRINCIPAL VARIABLES AND ARRAYS:
C
C  LANGUAGE:  FORTRAN
C
C  RECORD OF CHANGES:
C
C........................END PROLOGUE..................................
C
      subroutine buildDvorak (fix, option, dvorakStr)
c
      implicit none
c
      INCLUDE  'dataformats.inc'
c
c         formal parameters
      type (DVTS_RECORD) fix
      character*5        option
      character*10       dvorakStr
c
c         local variables
      integer           ii
      character*100     temp
c . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
c

c     Initialize dvorak
      dvorakStr = ' '

      if( option(1:4) .eq. 'long' ) then
         if( fix%longTerm%tnum .ne. 0 .or. 
     &        fix%longTerm%cinum .ne. 0 .or.
     &        fix%longTerm%intChg .eq. '+' .or. 
     &        fix%longTerm%intChg .eq. '-' ) then
            if( fix%longTerm%tnumChg .ne. 0 .or. 
     &           fix%longTerm%lastEvalHrsAgo .ne. 0 ) then
               write( dvorakStr, '(i2.2,i2.2,a1,a1,i2.2,i2.2)' )
     &              fix%longTerm%tnum, fix%longTerm%cinum, 
     &              fix%longTerm%intChg, fix%longTerm%pastChg,
     &              fix%longTerm%tnumChg, fix%longTerm%lastEvalHrsAgo
            else
               write( dvorakStr, '(i2.2,i2.2,a1,"/////")' )
     &              fix%longTerm%tnum, fix%longTerm%cinum,
     &              fix%longTerm%intChg
            endif
         else
            if( fix%longTerm%tnumChg .ne. 0 .or. 
     &           fix%longTerm%lastEvalHrsAgo .ne. 0 ) then
               write( dvorakStr, '("     ",a1,i2.2,i2.2)' )
     &              fix%longTerm%pastChg, fix%longTerm%tnumChg,
     &              fix%longTerm%lastEvalHrsAgo
            endif
         endif
      elseif( option(1:5) .eq. 'short' ) then
         if( fix%longTerm%tnum .ne. 0 .or. 
     &        fix%longTerm%cinum .ne. 0 .or.
     &        fix%longTerm%intChg .eq. '+' .or. 
     &        fix%longTerm%intChg .eq. '-' ) then
            if( fix%shortTerm%tnumChg .ne. 0 .or. 
     &           fix%shortTerm%lastEvalHrsAgo .ne. 0) then
               write( dvorakStr, '(i2.2,i2.2,a1,a1,i2.2,i2.2)' )
     &              fix%shortTerm%tnum, fix%shortTerm%cinum,
     &              fix%shortTerm%intChg, fix%shortTerm%pastChg,
     &              fix%shortTerm%tnumChg, fix%shortTerm%lastEvalHrsAgo
            else
               write( dvorakStr, '(i2.2,i2.2,a1,"/////")' )
     &              fix%shortTerm%tnum, fix%shortTerm%cinum,
     &              fix%shortTerm%intChg
            endif
         else
            if( fix%shortTerm%tnumChg .ne. 0 .or. 
     &           fix%shortTerm%lastEvalHrsAgo .ne. 0 ) then
               write( dvorakStr, '("     ",a1,i2.2,i2.2)' )
     &              fix%shortTerm%pastChg, fix%shortTerm%tnumChg,
     &              fix%shortTerm%lastEvalHrsAgo
            endif
         endif
      endif

C
      END


C
C........................START PROLOGUE.................................
C
C  SUBPROGRAM NAME:  outputDVTO
C
C  DESCRIPTION:  Build the objective dvorak section of the fix record
C
C  PROGRAMMER, DATE:  Ann Schrader   (SAIC)  April 2005
C
C  USAGE:  call outputDVTO ( lineEnd, fixRcd%dvto )
C
C  INPUT PARAMETERS:  
C     fixRcd - DVTO_RECORD containing the dvto data being written out
C
C  OUTPUT PARAMETERS:
C     lineEnd - the DVTO specific portion of the output fix record
C
C  ERROR CONDITIONS:
C
C........................MAINTENANCE SECTION............................
C
C  PRINCIPAL VARIABLES AND ARRAYS:
C
C  LANGUAGE:  FORTRAN
C
C  RECORD OF CHANGES:
C
C........................END PROLOGUE..................................
C
      subroutine outputDVTO (lineEnd, fix)
c
      implicit none
c
      INCLUDE  'dataformats.inc'
c
c         formal parameters
      character*200      lineEnd
      type (DVTO_RECORD) fix
c
c         local variables
      integer           ii
      character*100     temp
c . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
c
c
c     Build the objective dvorak section of the output fix line.
c
c     Start with the sensor.
      write( lineEnd, '(" ",a4,",")' )  ADJUSTR(fix%sensor)

c     Add the CI number and confidence.
      if( fix%cinum .ne. 0 ) then
         write( temp, '(" ",i2,", ",i1,",")' )  fix%cinum, fix%ciConf
      else
         write( temp, '("   ,  ,")' )
      endif
      lineEnd = lineEnd(1:LEN_TRIM(lineEnd))//temp(1:LEN_TRIM(temp))

c     Add the t-number average, t-number averaging time period and
c     t-number averaging derivation
      if( fix%tnumAvg .ne. 0 ) then
         write( temp, '(" ",i2,",")' ) fix%tnumAvg
      else
         write( temp, '("   ,")' )
      endif
      lineEnd = lineEnd(1:LEN_TRIM(lineEnd))//temp(1:LEN_TRIM(temp))

      if( fix%tnumAvgTime .ne. 0 ) then
         write( temp, '(" ",i3,",")' )  fix%tnumAvgTime
      else
         write( temp, '("    ,")' )
      endif
      lineEnd = lineEnd(1:LEN_TRIM(lineEnd))//temp(1:LEN_TRIM(temp))

      write( temp, '(" ",a1,",")' )  fix%tnumAvgDeriv
      lineEnd = lineEnd(1:LEN_TRIM(lineEnd))//temp(1:LEN_TRIM(temp))

c     Add the t-number (raw)
      if( fix%tnumRaw .ne. 0 ) then
         write( temp, '(" ",i2,",")' )  fix%tnumRaw
      else
         write( temp, '("   ,")' )
      endif
      lineEnd = lineEnd(1:LEN_TRIM(lineEnd))//temp(1:LEN_TRIM(temp))

c     Add the eye temperature
      if( fix%eyeTemp .ne. -999 ) then
         write( temp, '(" ",i4,",")' )  fix%eyeTemp
      else
         write( temp, '("     ,")' )
      endif
      lineEnd = lineEnd(1:LEN_TRIM(lineEnd))//temp(1:LEN_TRIM(temp))

c     Add the cloud temp,
      if( fix%cloudTemp .ne. -999 ) then
         write( temp, '(" ",i4,",")' )  fix%cloudTemp
      else
         write( temp, '("     ,")' )
      endif
      lineEnd = lineEnd(1:LEN_TRIM(lineEnd))//temp(1:LEN_TRIM(temp))

c     Add the scene type and algorithm
      write( temp, '(" ",a4,", ",a2,",")' )  
     &     ADJUSTR(fix%sceneType), ADJUSTR(fix%algorithm)
      lineEnd = lineEnd(1:LEN_TRIM(lineEnd))//temp(1:LEN_TRIM(temp))

c     Add on the satellite type, tropical indicator and comments.
      write( temp, '(" ",a6,", ",a1,", ",a)' )
     &     ADJUSTR(fix%satType), fix%tropical, fix%comments
      lineEnd = lineEnd(1:LEN_TRIM(lineEnd))//temp(1:LEN_TRIM(temp))
         
C
      END


C
C........................START PROLOGUE.................................
C
C  SUBPROGRAM NAME:  outputDVTS
C
C  DESCRIPTION:  Build the subjective dvorak section of the fix record
C
C  PROGRAMMER, DATE:  Ann Schrader   (SAIC)  April 2005
C
C  USAGE:  call outputDVTS ( lineEnd, fixRcd%dvts )
C
C  INPUT PARAMETERS:  
C     fixRcd - DVTS_RECORD containing the dvts data being written out
C
C  OUTPUT PARAMETERS:
C     lineEnd - the DVTS specific portion of the output fix record
C
C  ERROR CONDITIONS:
C
C........................MAINTENANCE SECTION............................
C
C  PRINCIPAL VARIABLES AND ARRAYS:
C
C  LANGUAGE:  FORTRAN
C
C  RECORD OF CHANGES:
C
C........................END PROLOGUE..................................
C
      subroutine outputDVTS (lineEnd, fix)
c
      implicit none
c
      INCLUDE  'dataformats.inc'
c
c         formal parameters
      character*200      lineEnd
      type (DVTS_RECORD) fix
c
c         local variables
      integer           ii
      character*100     temp
      character*10      dvorakStr
c . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
c
c
c     Build the subjective dvorak section of the output fix line.
c
c     Start with the sensor.
      write( lineEnd, '(" ",a4,",")' ) ADJUSTR(fix%sensor)

c     Add the pcn.
      if( fix%pcn .ne. 0 .and. fix%pcn .ne. -1 ) then
         write( temp, '(" ",i1,",")' ) fix%pcn
      else
         write( temp, '("  ,")' )
      endif
      lineEnd = lineEnd(1:LEN_TRIM(lineEnd))//temp(1:LEN_TRIM(temp))

c     Build the long term trend dvorak.
      call buildDvorak( fix, 'long', dvorakStr )
      lineEnd = lineEnd(1:LEN_TRIM(lineEnd))//" "//dvorakStr//","

c     Short term dvorak code
      if( fix%shortTerm%tnumChg .ne. 0 .or. 
     &     fix%shortTerm%lastEvalHrsAgo .ne. 0) then
         write( dvorakStr, '(a1,i2.2,i2.2)' )
     &        fix%shortTerm%pastChg, fix%shortTerm%tnumChg,
     &        fix%shortTerm%lastEvalHrsAgo
      else
         dvorakStr = '     '
      endif
      lineEnd = lineEnd(1:LEN_TRIM(lineEnd))//" "//dvorakStr(1:5)//","

c     Add on the ci 24 hour forecast.
      if( fix%ci24hr .ne. 0 ) then
         write( temp, '(" ",i2,",")' )  fix%ci24hr
      else
         write( temp, '("   ,")' )
      endif
      lineEnd = lineEnd(1:LEN_TRIM(lineEnd))//temp(1:LEN_TRIM(temp))

c     Add on the satellite type, center type, tropical indicator and 
c     comments.
      write( temp, '(" ",a6,", ",a4,", ",a1,", ",a)' )
     &     ADJUSTR(fix%satType), ADJUSTR(fix%centerType), 
     &     fix%tropical, fix%comments
      lineEnd = lineEnd(1:LEN_TRIM(lineEnd))//temp(1:LEN_TRIM(temp))
         
C
      END


C
C........................START PROLOGUE.................................
C
C  SUBPROGRAM NAME:  outputMicrowave
C
C  DESCRIPTION:  Build the microwave section of the fix record
C
C  PROGRAMMER, DATE:  Ann Schrader   (SAIC)  April 2005
C
C  USAGE:  call outputMicrowave ( lineEnd, fixRcd%micro )
C
C  INPUT PARAMETERS:  
C     fixRcd - MICROWAVE_RECORD containing the microwave data
C
C  OUTPUT PARAMETERS:
C     lineEnd - the microwave specific portion of the output fix record
C
C  ERROR CONDITIONS:
C
C........................MAINTENANCE SECTION............................
C
C  PRINCIPAL VARIABLES AND ARRAYS:
C
C  LANGUAGE:  FORTRAN
C
C  RECORD OF CHANGES:
C
C........................END PROLOGUE..................................
C
      subroutine outputMicrowave (lineEnd, fix)
c
      implicit none
c
      INCLUDE  'dataformats.inc'
c
c         formal parameters
      character*200      lineEnd
      type (MICROWAVE_RECORD) fix
c
c         local variables
      integer           ii
      character*100     temp
      character*1       rainchar
      character*1       edgecut(8)
c . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
c
c
c     Build the microwave section of the output fix line.
c
      if( fix%rain ) then
         rainchar = 'R'
      else
         rainchar = ' '
      endif
c     Start with the rain flag
      write( lineEnd, '(" ",a1,",")' )  rainchar

c     Add the rainrate and algorithm
      if( fix%rainrate .ne. 0 ) then
         write( temp, '(" ",i3,", ",a6,",")' ) 
     &        fix%rainrate, ADJUSTR(fix%algorithm)
      else
         write( temp, '("    , ",a6,",")' )  ADJUSTR(fix%algorithm)
      endif
      lineEnd = lineEnd(1:LEN_TRIM(lineEnd))//temp(1:LEN_TRIM(temp))

c     Add the wave height
      if( fix%wave .ne. 0 ) then
         write( temp, '(" ",i2,",")' )  fix%wave
      else
         write( temp, '("   ,")' )
      endif
      lineEnd = lineEnd(1:LEN_TRIM(lineEnd))//temp(1:LEN_TRIM(temp))

c     Add the temp
      if( fix%temp .ne. 0 ) then
         write( temp, '(" ",i4,",")' )  fix%temp
      else
         write( temp, '("     ,")' )
      endif
      lineEnd = lineEnd(1:LEN_TRIM(lineEnd))//temp(1:LEN_TRIM(temp))

c     Add the slp raw
      if( fix%slpraw .ne. 0 ) then
         write( temp, '(" ",i4,",")' ) fix%slpraw
      else
         write( temp, '("     ,")' )
      endif
      lineEnd = lineEnd(1:LEN_TRIM(lineEnd))//temp(1:LEN_TRIM(temp))

c     Add the slp retrieved
      if( fix%slpretr .ne. 0 ) then
         write( temp, '(" ",i4,",")' ) fix%slpretr
      else
         write( temp, '("     ,")' )
      endif
      lineEnd = lineEnd(1:LEN_TRIM(lineEnd))//temp(1:LEN_TRIM(temp))

c     Add the max seas
      if( fix%seas .ne. 0 ) then
         write( temp, '(" ",i3,",")' )  fix%seas
      else
         write( temp, '("    ,")' )
      endif
      lineEnd = lineEnd(1:LEN_TRIM(lineEnd))//temp(1:LEN_TRIM(temp))

c     Add the satellite type
      write( temp, '(" ",a6,",")' )  ADJUSTR(fix%satType)
      lineEnd = lineEnd(1:LEN_TRIM(lineEnd))//temp(1:LEN_TRIM(temp))

c     Add the rad, windcode and radii.
      if( fix%radii(1) .ne. 0 .or. fix%radii(2) .ne. 0 .or.
     &     fix%radii(3) .ne. 0 .or. fix%radii(4) .ne. 0 ) then
         write( temp, '(" ",i3,", ",a4,8(", ",i4),",")' )
     &        fix%rad, ADJUSTR(fix%windcode), (fix%radii(ii),ii=1,8)
      else
         write( temp, '("    ,     ,     ,     ,     ,     ,     , ",
     &        "    ,     ,     ,")' )
      endif
      lineEnd = lineEnd(1:LEN_TRIM(lineEnd))//temp(1:LEN_TRIM(temp))

c     Add the radii modifiers
      do ii = 1, 8
         edgecut(ii) = ' '
         if( fix%edge(ii) .and. fix%cut(ii) ) then
            edgecut(ii) = 'B'
         elseif( fix%edge(ii) ) then
            edgecut(ii) = 'E'
         elseif( fix%cut(ii) ) then
            edgecut(ii) = 'C'
         endif
      enddo
      write( temp, '( 8(" ",a1,",") )' )
     &     (edgecut(ii),ii=1,8)
      lineEnd = lineEnd(1:LEN_TRIM(lineEnd))//temp(1:LEN_TRIM(temp))

c     Add the radii confidence
      if( fix%radConf .ne. 0 ) then
         write( temp, '(" ",i1,"," )' ) fix%radConf
      else
         write( temp, '("  ,")' )
      endif
      lineEnd = lineEnd(1:LEN_TRIM(lineEnd))//temp(1:LEN_TRIM(temp))

c     Add the comments
      write( temp, '(" ",a )' )  fix%comments
      lineEnd = lineEnd(1:LEN_TRIM(lineEnd))//temp(1:LEN_TRIM(temp))

C
      END


C
C........................START PROLOGUE.................................
C
C  SUBPROGRAM NAME:  outputRadar
C
C  DESCRIPTION:  Build the radar section of the fix record
C
C  PROGRAMMER, DATE:  Ann Schrader   (SAIC)  April 2005
C
C  USAGE:  call outputRadar ( lineEnd, fixRcd%radar )
C
C  INPUT PARAMETERS:  
C     fixRcd - RADAR_RECORD containing the radar data
C
C  OUTPUT PARAMETERS:
C     lineEnd - the radar specific portion of the output fix record
C
C  ERROR CONDITIONS:
C
C........................MAINTENANCE SECTION............................
C
C  PRINCIPAL VARIABLES AND ARRAYS:
C
C  LANGUAGE:  FORTRAN
C
C  RECORD OF CHANGES:
C
C........................END PROLOGUE..................................
C
      subroutine outputRadar (lineEnd, fix)
c
      implicit none
c
      INCLUDE  'dataformats.inc'
c
c         formal parameters
      character*200      lineEnd
      type (RADAR_RECORD) fix
c
c         local variables
      integer           ii
      character*100     temp
      character*1       radarformat
c . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
c
c
c     Build the radar section of the output fix line.
c
c     Start with the radar type and radar format
      if( fix%radobcode ) then
         radarformat = 'R'
      elseif( fix%plainlanguage ) then
         radarformat = 'P'
      elseif( fix%doppler ) then
         radarformat = 'D'
      endif
      write( lineEnd, '(" ",a1,", ",a1,",")' )
     &     fix%rdrtype, radarformat

c     Add the radob code, eye shape
      write( temp, '(" ",a10,", ",a2,",")' ) 
     &     fix%radob, ADJUSTR(fix%eyeShape)
      lineEnd = lineEnd(1:LEN_TRIM(lineEnd))//temp(1:LEN_TRIM(temp))

c     Add the % of eye wall observed
      if( fix%eyeWallOb .ne. 0 ) then
         write( temp, '(" ",i2,",")' )  fix%eyeWallOb
      else
         write( temp, '("   ,")' )
      endif
      lineEnd = lineEnd(1:LEN_TRIM(lineEnd))//temp(1:LEN_TRIM(temp))

c     Add the spiral overlay
      if( fix%spiralOV .ne. 0 ) then
         write( temp, '(" ",i2,",")' )  fix%spiralOV
      else
         write( temp, '("   ,")' )
      endif
      lineEnd = lineEnd(1:LEN_TRIM(lineEnd))//temp(1:LEN_TRIM(temp))

c     Add the radar site lat/lon.
      if( fix%lat .ne. 0. .or. fix%lon .ne. 0. ) then
         write( temp, '(" ",i4,a1,", ",i5,a1,",")' )
     &        NINT(fix%lat*100.), fix%NS, NINT(fix%lon*100.), fix%EW
      else
         write( temp, '("      ,       ,")' )
      endif
      lineEnd = lineEnd(1:LEN_TRIM(lineEnd))//temp(1:LEN_TRIM(temp))

c     Add the inbound wind
      if( fix%vmaxin .ne. 0 ) then
         write( temp, '(" ",i3,",")' ) fix%vmaxin
      else
         write( temp, '("    ,")' )
      endif
      lineEnd = lineEnd(1:LEN_TRIM(lineEnd))//temp(1:LEN_TRIM(temp))

c     Add the azimuth
      if( fix%azimuthin .ne. 0 ) then
         write( temp, '(" ",i3,",")' ) fix%azimuthin
      else
         write( temp, '("    ,")' )
      endif
      lineEnd = lineEnd(1:LEN_TRIM(lineEnd))//temp(1:LEN_TRIM(temp))

c     Add the range
      if( fix%rangein .ne. 0 ) then
         write( temp, '(" ",i3,",")' ) fix%rangein
      else
         write( temp, '("    ,")' )
      endif
      lineEnd = lineEnd(1:LEN_TRIM(lineEnd))//temp(1:LEN_TRIM(temp))

c     Add the elevation
      if( fix%elevin .ne. 0 ) then
         write( temp, '(" ",i5,",")' ) fix%elevin
      else
         write( temp, '("      ,")' )
      endif
      lineEnd = lineEnd(1:LEN_TRIM(lineEnd))//temp(1:LEN_TRIM(temp))

c     Add the outbound wind
      if( fix%vmaxout .ne. 0 ) then
         write( temp, '(" ",i3,",")' ) fix%vmaxout
      else
         write( temp, '("    ,")' )
      endif
      lineEnd = lineEnd(1:LEN_TRIM(lineEnd))//temp(1:LEN_TRIM(temp))

c     Add the azimuth
      if( fix%azimuthout .ne. 0 ) then
         write( temp, '(" ",i3,",")' ) fix%azimuthout
      else
         write( temp, '("    ,")' )
      endif
      lineEnd = lineEnd(1:LEN_TRIM(lineEnd))//temp(1:LEN_TRIM(temp))

c     Add the range
      if( fix%rangeout .ne. 0 ) then
         write( temp, '(" ",i3,",")' ) fix%rangeout
      else
         write( temp, '("    ,")' )
      endif
      lineEnd = lineEnd(1:LEN_TRIM(lineEnd))//temp(1:LEN_TRIM(temp))

c     Add the elevation
      if( fix%elevout .ne. 0 ) then
         write( temp, '(" ",i5,",")' ) fix%elevout
      else
         write( temp, '("      ,")' )
      endif
      lineEnd = lineEnd(1:LEN_TRIM(lineEnd))//temp(1:LEN_TRIM(temp))

c     Add the cloud height.
      if( fix%cloudHeight .ne. 0 ) then
         write( temp, '(" ",i5,",")' )  fix%cloudHeight
      else
         write( temp, '("      ,")' )
      endif
      lineEnd = lineEnd(1:LEN_TRIM(lineEnd))//temp(1:LEN_TRIM(temp))

c     Add the rain info  accumulation
      if( fix%rainAccum .ne. 0. ) then
         write( temp, '(" ",i5,",")' ) NINT(fix%rainAccum*100.)
      else
         write( temp, '("      ,")' )
      endif
      lineEnd = lineEnd(1:LEN_TRIM(lineEnd))//temp(1:LEN_TRIM(temp))

c     Add the rain info time interval
      if( fix%rainAcTimeInt .ne. 0 ) then
         write( temp, '(" ",i3,",")' ) fix%rainAcTimeInt
      else
         write( temp, '("    ,")' )
      endif
      lineEnd = lineEnd(1:LEN_TRIM(lineEnd))//temp(1:LEN_TRIM(temp))

c     Add the rani info lat/lon
      if( fix%rainlat .ne. 0. .or. fix%rainlon .ne. 0 ) then
         write( temp, '(" ",i4,a1,", ",i5,a1,",")' )
     &        NINT(fix%rainlat*100.), fix%rainNS, 
     &        NINT(fix%rainlon*100.), fix%rainEW
      else
         write( temp, '("      ,       ,")' )
      endif
      lineEnd = lineEnd(1:LEN_TRIM(lineEnd))//temp(1:LEN_TRIM(temp))

c     Add the comments.
      write( temp, '(" ",a)' ) fix%comments
      lineEnd = lineEnd(1:LEN_TRIM(lineEnd))//temp(1:LEN_TRIM(temp))

C
      END


C
C........................START PROLOGUE.................................
C
C  SUBPROGRAM NAME:  outputAircraft
C
C  DESCRIPTION:  Build the aircraft section of the fix record
C
C  PROGRAMMER, DATE:  Ann Schrader   (SAIC)  April 2005
C
C  USAGE:  call outputAircraft ( lineEnd, fixRcd%air )
C
C  INPUT PARAMETERS:  
C     fixRcd - AIRCRAFT_RECORD containing the aircraft data
C
C  OUTPUT PARAMETERS:
C     lineEnd - the aircraft specific portion of the output fix record
C
C  ERROR CONDITIONS:
C
C........................MAINTENANCE SECTION............................
C
C  PRINCIPAL VARIABLES AND ARRAYS:
C
C  LANGUAGE:  FORTRAN
C
C  RECORD OF CHANGES:
C
C........................END PROLOGUE..................................
C
      subroutine outputAircraft (lineEnd, fix)
c
      implicit none
c
      INCLUDE  'dataformats.inc'
c
c         formal parameters
      character*200      lineEnd
      type (AIRCRAFT_RECORD) fix
c
c         local variables
      integer           ii
      character*100     temp
      character*2       eyeshapeStr
c . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
c
c
c     Build the aircraft section of the output fix line.
c
c     Start with flight level in 100's of feet
      if( fix%FlightLevelFt .ne. 0 ) then
         write( lineEnd, '(" ",i2,",")' )  fix%FlightLevelFt
      else
         write( lineEnd, '("   ,")' )
      endif

c     Add the flight level in mb
      if( fix%FlightLevelMB .ne. 0 ) then
         write( temp, '(" ",i3,",")' ) fix%FlightLevelMB
      else
         write( temp, '("    ,")' )
      endif
      lineEnd = lineEnd(1:LEN_TRIM(lineEnd))//temp(1:LEN_TRIM(temp))

c     Add the min height
      if( fix%MinHeight .ne. 0 ) then
         write( temp, '(" ",i4,",")' )  fix%MinHeight
      else
         write( temp, '("     ,")' )
      endif
      lineEnd = lineEnd(1:LEN_TRIM(lineEnd))//temp(1:LEN_TRIM(temp))

c     Add inbound info for max sfc wind intensity
      if( fix%MaxSurfaceWind .ne. 0 ) then
         write( temp, '(" ",i3,",")' ) fix%MaxSurfaceWind
      else
         write( temp, '("    ,")' )
      endif
      lineEnd = lineEnd(1:LEN_TRIM(lineEnd))//temp(1:LEN_TRIM(temp))

c     Add inbound bearing
      if( fix%MaxSurfaceWindBearing .ne. 0 ) then
         write( temp, '(" ",i3,",")' ) fix%MaxSurfaceWindBearing 
      else
         write( temp, '("    ,")' )
      endif
      lineEnd = lineEnd(1:LEN_TRIM(lineEnd))//temp(1:LEN_TRIM(temp))

c     Add inbound range
      if( fix%MaxSurfaceWindRange .ne. 0 ) then
         write( temp, '(" ",i3,",")' ) fix%MaxSurfaceWindRange
      else
         write( temp, '("    ,")' )
      endif
      lineEnd = lineEnd(1:LEN_TRIM(lineEnd))//temp(1:LEN_TRIM(temp))

c     Add inbound info for max flight level wind dir
      if( fix%MaxFlightLevelWindDir .ne. 0 ) then
         write( temp, '(" ",i3,",")' ) fix%MaxFlightLevelWindDir
      else
         write( temp, '("    ,")' )
      endif
      lineEnd = lineEnd(1:LEN_TRIM(lineEnd))//temp(1:LEN_TRIM(temp))

c     Add inbound info for max flight level wind intensity
      if( fix%MaxFlightLevelWindInt .ne. 0 ) then
         write( temp, '(" ",i3,",")' ) fix%MaxFlightLevelWindInt
      else
         write( temp, '("    ,")' )
      endif
      lineEnd = lineEnd(1:LEN_TRIM(lineEnd))//temp(1:LEN_TRIM(temp))

c     Add inbound info for max flight level wind bearing
      if( fix%MaxFlightLevelWindBearing .ne. 0 ) then
         write( temp, '(" ",i3,",")' ) fix%MaxFlightLevelWindBearing
      else
         write( temp, '("    ,")' )
      endif
      lineEnd = lineEnd(1:LEN_TRIM(lineEnd))//temp(1:LEN_TRIM(temp))

c     Add inbound info for max flight level range
      if( fix%MaxFlightLevelWindRange .ne. 0 ) then
         write( temp, '(" ",i3,",")' ) fix%MaxFlightLevelWindRange
      else
         write( temp, '("    ,")' )
      endif
      lineEnd = lineEnd(1:LEN_TRIM(lineEnd))//temp(1:LEN_TRIM(temp))

c     Add MSLP
      if( fix%MinSeaLevelPressure .ne. 0 ) then
         write( temp, '(" ",i4,",")') fix%MinSeaLevelPressure
      else
         write( temp, '("     ,")' )
      endif
      lineEnd = lineEnd(1:LEN_TRIM(lineEnd))//temp(1:LEN_TRIM(temp))

c     Add temp outside eye
      if( fix%EyeTempOutside .ne. 0 ) then
         write( temp, '(" ",i3,",")') fix%EyeTempOutside
      else
         write( temp, '("    ,")' )
      endif
      lineEnd = lineEnd(1:LEN_TRIM(lineEnd))//temp(1:LEN_TRIM(temp))

c     Add temp inside eye
      if( fix%EyeTempInside .ne. 0 ) then
         write( temp, '(" ",i3,",")') fix%EyeTempInside
      else
         write( temp, '("    ,")' )
      endif
      lineEnd = lineEnd(1:LEN_TRIM(lineEnd))//temp(1:LEN_TRIM(temp))

c     Add dew pt temp
      if( fix%DewPoint .ne. 0 ) then
         write( temp, '(" ",i3,",")') fix%DewPoint
      else
         write( temp, '("    ,")' )
      endif
      lineEnd = lineEnd(1:LEN_TRIM(lineEnd))//temp(1:LEN_TRIM(temp))

c     Add ss temp
      if( fix%SeaSurface .ne. 0 ) then
         write( temp, '(" ",i2,",")') fix%SeaSurface
      else
         write( temp, '("   ,")' )
      endif
      lineEnd = lineEnd(1:LEN_TRIM(lineEnd))//temp(1:LEN_TRIM(temp))

c     Add wall cloud thickness
      if( fix%WallCloudThickness .ne. 0 ) then
         write( temp, '(" ",i2,",")') fix%WallCloudThickness
      else
         write( temp, '("   ,")' )
      endif
      lineEnd = lineEnd(1:LEN_TRIM(lineEnd))//temp(1:LEN_TRIM(temp))

      if( fix%EyeShape .eq. 0 ) then
         eyeshapeStr = '  '
      elseif( fix%EyeShape .eq. 1 ) then
         eyeshapeStr = 'CI'
      elseif( fix%EyeShape .eq. 2 ) then
         eyeshapeStr = 'EL'
      elseif( fix%EyeShape .eq. 3 ) then
         eyeshapeStr = 'CO'
      endif
c     Add eye characteristics, shape
      write( temp, '(" ",a2,",")' ) ADJUSTR(eyeshapeStr)
      lineEnd = lineEnd(1:LEN_TRIM(lineEnd))//temp(1:LEN_TRIM(temp))

c     Add eye characteristics, orientation
      if( fix%EyeOrientation .ne. 0 ) then
         write( temp, '(" ",i3,",")' ) fix%EyeOrientation
      else
         write( temp, '("    ,")' )
      endif
      lineEnd = lineEnd(1:LEN_TRIM(lineEnd))//temp(1:LEN_TRIM(temp))

c     Add eye characteristics, diameter
      if( fix%DiameterLongAxis .ne. 0 ) then
         write( temp, '(" ",i2,",")' ) fix%DiameterLongAxis
      else
         write( temp, '("   ,")' )
      endif
      lineEnd = lineEnd(1:LEN_TRIM(lineEnd))//temp(1:LEN_TRIM(temp))

c     Add eye characteristics, short axis
      if( fix%DiameterShortAxis .ne. 0 ) then
         write( temp, '(" ",i2,",")' ) fix%DiameterShortAxis
      else
         write( temp, '("   ,")' )
      endif
      lineEnd = lineEnd(1:LEN_TRIM(lineEnd))//temp(1:LEN_TRIM(temp))

c     Add the accuracy, navigational
      if( fix%NavigationalAccuracy .ne. 0 ) then
         write( temp, '(" ",i3,",")' ) 
     &        NINT(fix%NavigationalAccuracy*10.)
      else
         write( temp, '("    ,")' )
      endif
      lineEnd = lineEnd(1:LEN_TRIM(lineEnd))//temp(1:LEN_TRIM(temp))

c     Add the accuracy, meteorological
      if( fix%NavigationalMeteorological .ne. 0. ) then
         write( temp, '(" ",i3,",")' ) 
     &        NINT(fix%NavigationalMeteorological*10.)
      else
         write( temp, '("    ,")' )
      endif
      lineEnd = lineEnd(1:LEN_TRIM(lineEnd))//temp(1:LEN_TRIM(temp))

c     Add the mission #
      if( fix%MissionNumber .ne. 0 ) then
         write( temp, '(" ",i2,",")' ) fix%MissionNumber
      else
         write( temp, '("   ,")' )
      endif
      lineEnd = lineEnd(1:LEN_TRIM(lineEnd))//temp(1:LEN_TRIM(temp))

c     Add the comments.
      write( temp, '(" ",a)' ) fix%comments
      lineEnd = lineEnd(1:LEN_TRIM(lineEnd))//temp(1:LEN_TRIM(temp))

C
      END


C
C........................START PROLOGUE.................................
C
C  SUBPROGRAM NAME:  outputDropsonde
C
C  DESCRIPTION:  Build the dropsonde section of the fix record
C
C  PROGRAMMER, DATE:  Ann Schrader   (SAIC)  April 2005
C
C  USAGE:  call outputDropsonde ( lineEnd, fixRcd%drop )
C
C  INPUT PARAMETERS:  
C     fixRcd - DROPSONDE_RECORD containing the dropsonde data
C
C  OUTPUT PARAMETERS:
C     lineEnd - the dropsonde specific portion of the output fix record
C
C  ERROR CONDITIONS:
C
C........................MAINTENANCE SECTION............................
C
C  PRINCIPAL VARIABLES AND ARRAYS:
C
C  LANGUAGE:  FORTRAN
C
C  RECORD OF CHANGES:
C
C........................END PROLOGUE..................................
C
      subroutine outputDropsonde (lineEnd, fix)
c
      implicit none
c
      INCLUDE  'dataformats.inc'
c
c         formal parameters
      character*200      lineEnd
      type (DROPSONDE_RECORD) fix
c
c         local variables
      integer           ii
      character*100     temp
c . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
c
c
c     Build the dropsonde section of the output fix line.
c
c     Start with the sonde environment and midpoint height
      write( lineEnd, '(" ",a10,", ",i3,",")' )
     &     ADJUSTR(fix%sondeEnv), fix%height150

c     Add spd of mean wind lowest 150 m and spd of mean wind 0-500 m
      write( temp, '(" ",i3,", ",i3,",")' )  fix%vspd150, fix%vspd500
      lineEnd = lineEnd(1:LEN_TRIM(lineEnd))//temp(1:LEN_TRIM(temp))

c     Add the comments.
      write( temp, '(" ",a)' ) fix%comments
      lineEnd = lineEnd(1:LEN_TRIM(lineEnd))//temp(1:LEN_TRIM(temp))

C
      END


C
C........................START PROLOGUE.................................
C
C  SUBPROGRAM NAME:  outputAnalysis
C
C  DESCRIPTION:  Build the analysis section of the fix record
C
C  PROGRAMMER, DATE:  Ann Schrader   (SAIC)  April 2005
C
C  USAGE:  call outputAnalysis ( lineEnd, fixRcd%anal )
C
C  INPUT PARAMETERS:  
C     fixRcd - ANALYSIS_RECORD containing the analysis data
C
C  OUTPUT PARAMETERS:
C     lineEnd - the analysis specific portion of the output fix record
C
C  ERROR CONDITIONS:
C
C........................MAINTENANCE SECTION............................
C
C  PRINCIPAL VARIABLES AND ARRAYS:
C
C  LANGUAGE:  FORTRAN
C
C  RECORD OF CHANGES:
C
C........................END PROLOGUE..................................
C
      subroutine outputAnalysis (lineEnd, fix)
c
      implicit none
c
      INCLUDE  'dataformats.inc'
c
c         formal parameters
      character*200      lineEnd
      type (ANALYSIS_RECORD) fix
c
c         local variables
      character*100     temp
c . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
c
c
c     Build the analysis section of the output fix line.
c
c     Start with the analyst initials, start time, end time and
c     distance to nearest data
      write( lineEnd, '(" ",a3,", ",a12,", ",a12,", ",i3,",")' )
     &     fix%initials, fix%startDTG, fix%endDTG, 
     &     fix%DistanceToNearestData

c     Start with the analyst initials, start time and end time 
      write( lineEnd, '(" ",a3,", ",a12,", ",a12,",")' )
     &     ADJUSTR(fix%initials), 
     &     ADJUSTR(fix%startDTG), ADJUSTR(fix%endDTG)

c     Add the distance to nearest data
      if( fix%DistanceToNearestData .ne. 0 ) then
         write( temp, '(" ",i3,",")' ) fix%DistanceToNearestData
      else
         write( temp, '("    ,")' )
      endif
      lineEnd = lineEnd(1:LEN_TRIM(lineEnd))//temp(1:LEN_TRIM(temp))

c     Add the sst
      if( fix%SST .ne. 0 ) then
         write( temp, '(" ",i4,",")' ) fix%SST
      else
         write( temp, '("     ,")' )
      endif
      lineEnd = lineEnd(1:LEN_TRIM(lineEnd))//temp(1:LEN_TRIM(temp))

c     Add the observation sources
      write( temp, '(" ",a24,",")' ) ADJUSTR(fix%obSources)
      lineEnd = lineEnd(1:LEN_TRIM(lineEnd))//temp(1:LEN_TRIM(temp))

c     Add the comments.
      write( temp, '(" ",a)' ) fix%comments
      lineEnd = lineEnd(1:LEN_TRIM(lineEnd))//temp(1:LEN_TRIM(temp))

C
      END


C
C........................START PROLOGUE.................................
C
C  SUBPROGRAM NAME:  outputFrcd
C
C  DESCRIPTION:  Writes the F_RECORD to the spectified file.
C
C  PROGRAMMER, DATE:  Ann Schrader   (SAIC)  April 2005
C
C  USAGE:  call outputFrcd ( datFile, fixdata%fRecord(ii), result )
C
C  INPUT PARAMETERS:  
C     datFile - unit number of the objective aids file
C     fixRcd - F_RECORD struct containing record to write out
C
C  OUTPUT PARAMETERS:
C     result - return 0 for fail, 1 for success
C
C  ERROR CONDITIONS:
C
C........................MAINTENANCE SECTION............................
C
C  PRINCIPAL VARIABLES AND ARRAYS:
C
C  LANGUAGE:  FORTRAN
C
C  RECORD OF CHANGES:
C
C........................END PROLOGUE..................................
C
      subroutine outputFrcd (datFile, fixRcd, result)
c
      implicit none
c
      INCLUDE  'dataformats.inc'
c
c         formal parameters
      integer           datFile
      type (F_RECORD)   fixRcd
      integer           result
c
c         local variables
      integer           ii
      character*400     line
      character*100     temp
      character*200     lineEnd
      character*10      cicode
      character*1       edgecut(4)
c . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
c
      result = 1
c
c     Build the output fix line. */
c
c     Start with the basin, cyclone number and date-time-group.
      write( line, '(a2,", ",i2.2,", ",a12,",")' ) 
     &     fixRcd%basin, fixRcd%cyNum, fixRcd%DTG

      cicode = ' '
      if( fixRcd%centerFix ) cicode = 'C'
      if( fixRcd%intensityFix ) 
     &     cicode = cicode(1:LEN_TRIM(cicode))//"I"
      if( fixRcd%radiiFix ) cicode = cicode(1:LEN_TRIM(cicode))//"R"
      if( fixRcd%pressureFix ) cicode = cicode(1:LEN_TRIM(cicode))//"P"
      
c     Add on the fix format, fix code, center/intens code and flagged.
      write( temp, '(" ",i3,", ",a4,", ",a10,", ",a1,",")' ) 
     &     fixRcd%fixformat, ADJUSTR(fixRcd%fixtype), ADJUSTR(cicode),
     &     fixRcd%flagged
      line = line(1:LEN_TRIM(line))//temp(1:LEN_TRIM(temp))

c     Add on the lat and lon.
      if( fixRcd%lat .ne. 0. .or. fixRcd%lon .ne. 0. ) then
         write( temp, '(" ",i4,a1,", ",i5,a1,",")' ) 
     &        NINT(fixRcd%lat*100.), fixRcd%NS, 
     &        NINT(fixRcd%lon*100.), fixRcd%EW
      else
         write( temp, '("      ,       ," )' )
      endif
      line = line(1:LEN_TRIM(line))//temp(1:LEN_TRIM(temp))

c     Add on the height 
      if( fixRcd%height .ne. 0 ) then
         write( temp, '(" ",i5,",")' )  fixRcd%height
      else
         write( temp, '("      ,")' )
      endif
      line = line(1:LEN_TRIM(line))//temp(1:LEN_TRIM(temp))

c     Add on the position confidence 
      if( fixRcd%positConf .ne. 0 ) then
         write( temp, '(" ",i1,",")' )  fixRcd%positConf
      else
         write( temp, '("  ,")' )
      endif
      line = line(1:LEN_TRIM(line))//temp(1:LEN_TRIM(temp))

c     Add on the vmax and v confidence
      if( fixRcd%v .ne. 0 ) then
         write( temp, '(" ",i3,",")' ) fixRcd%v
      else
         write( temp, '("    ,")' )
      endif
      line = line(1:LEN_TRIM(line))//temp(1:LEN_TRIM(temp))

c     Add on the v confidence
      if( fixRcd%vConf .ne. 0 ) then
         write( temp, '(" ",i1,",")' ) fixRcd%vConf
      else
         write( temp, '("  ,")' )
      endif
      line = line(1:LEN_TRIM(line))//temp(1:LEN_TRIM(temp))

c     Add on the pressure
      if( fixRcd%pressure .ne. 0 ) then
         write( temp, '(" ",i4,",")' )  fixRcd%pressure
      else
         write( temp, '("     ,")' )
      endif
      line = line(1:LEN_TRIM(line))//temp(1:LEN_TRIM(temp))

c     Add on the pressure confidence
      if( fixRcd%presConf .ne. 0 ) then
         write( temp, '(" ",i1,",")' )  fixRcd%presConf
      else
         write( temp, '("  ,")' )
      endif
      line = line(1:LEN_TRIM(line))//temp(1:LEN_TRIM(temp))

c     Add on the pressure derivation
      write( temp, '(" ",a4,",")' )  ADJUSTR(fixRcd%presDeriv)
      line = line(1:LEN_TRIM(line))//temp(1:LEN_TRIM(temp))

      do ii = 1, 4
         edgecut(ii) = " "
         if( fixRcd%edge(ii) .and. fixRcd%cut(ii) ) then
            edgecut(ii) = "B"
         elseif( fixRcd%edge(ii) ) then
            edgecut(ii) = "E"
         elseif( fixRcd%cut(ii) ) then
            edgecut(ii) = "C"
         endif
      enddo
            
c     Add on the rad, windcode and radii data
      if( fixRcd%radii(1) .ne. 0 .or. fixRcd%radii(2) .ne. 0 .or.
     &     fixRcd%radii(3) .ne. 0 .or. fixRcd%radii(4) .ne. 0 ) then
         write( temp, '(" ",i3,", ",a4,4(", ",i4),",")' )
     &        fixRcd%rad, ADJUSTR(fixRcd%windcode),
     &        (fixRcd%radii(ii),ii=1,4)
      else
         write( temp, '("    ,     ,     ,     ,     ,     ,")' )
      endif
      line = line(1:LEN_TRIM(line))//temp(1:LEN_TRIM(temp))
            
c     Add on the radii modifier
      write( temp, '(" ",a1,", ",a1,", ",a1,", ",a1,",")' )
     &     (edgecut(ii),ii=1,4)
      line = line(1:LEN_TRIM(line))//temp(1:LEN_TRIM(temp))
            
c     Add on the radii confidence.
      if( fixRcd%radConf .ne. 0 ) then
         write( temp, '(" ",i1,",")' )  fixRcd%radConf
      else
         write( temp, '("  ,")' )
      endif
      line = line(1:LEN_TRIM(line))//temp(1:LEN_TRIM(temp))

c     Add on the radius of max winds
      if( fixRcd%mrd .ne. 0 ) then
         write( temp, '(" ",i3,",")' )  fixRcd%mrd
      else
         write( temp, '("    ,")' )
      endif
      line = line(1:LEN_TRIM(line))//temp(1:LEN_TRIM(temp))

c     Add on the eye
      if( fixRcd%eye .ne. 0 ) then
         write( temp, '(" ",i3,",")' )  fixRcd%eye
      else
         write( temp, '("    ,")' )
      endif
      line = line(1:LEN_TRIM(line))//temp(1:LEN_TRIM(temp))

c     Add on the subregion, fix site and initials.
      write( temp, '(" ",a1,", ",a5,", ",a3,",")' )
     &     fixRcd%subregion, ADJUSTR(fixRcd%fixsite), 
     &     ADJUSTR(fixRcd%initials)
      line = line(1:LEN_TRIM(line))//temp(1:LEN_TRIM(temp))

c     Process the rest of the record.
      if( fixRcd%fixformat .eq. DVTStype ) then
         call outputDVTS( lineEnd, fixRcd%dvts )
         line = line(1:LEN_TRIM(line))//lineEnd(1:LEN_TRIM(lineEnd))
         write( datFile,'( a )') line(1:LEN_TRIM(line))
      elseif( fixRcd%fixformat .eq. DVTOtype ) then
         call outputDVTO( lineEnd, fixRcd%dvto )
         line = line(1:LEN_TRIM(line))//lineEnd(1:LEN_TRIM(lineEnd))
         write( datFile,'( a )') line(1:LEN_TRIM(line))
      elseif( fixRcd%fixformat .eq. MICRtype .or. 
     &        fixRcd%fixformat .eq. SCATtype ) then
         call outputMicrowave( lineEnd, fixRcd%micro )
         line = line(1:LEN_TRIM(line))//lineEnd(1:LEN_TRIM(lineEnd))
         write( datFile,'( a )') line(1:LEN_TRIM(line))
      elseif( fixRcd%fixformat .eq. RDRtype ) then
         call outputRadar( lineEnd, fixRcd%radar )
         line = line(1:LEN_TRIM(line))//lineEnd(1:LEN_TRIM(lineEnd))
         write( datFile,'( a )') line(1:LEN_TRIM(line))
      elseif( fixRcd%fixformat .eq. AIRCtype ) then
         call outputAircraft( lineEnd, fixRcd%air )
         line = line(1:LEN_TRIM(line))//lineEnd(1:LEN_TRIM(lineEnd))
         write( datFile,'( a )') line(1:LEN_TRIM(line))
      elseif( fixRcd%fixformat .eq. DROPtype ) then
         call outputDropsonde( lineEnd, fixRcd%drop )
         line = line(1:LEN_TRIM(line))//lineEnd(1:LEN_TRIM(lineEnd))
         write( datFile,'( a )') line(1:LEN_TRIM(line))
      elseif( fixRcd%fixformat .eq. ANALtype ) then
         call outputAnalysis( lineEnd, fixRcd%anal )
         line = line(1:LEN_TRIM(line))//lineEnd(1:LEN_TRIM(lineEnd))
         write( datFile,'( a )') line(1:LEN_TRIM(line))
      endif

C
      END




C
C........................START PROLOGUE.................................
C
C  SUBPROGRAM NAME:  writeFix
C
C  DESCRIPTION:  Writes a FIX_DATA record to the specified file.
C
C  PROGRAMMER, DATE:  Ann Schrader   (SAIC)  April 2005
C
C  USAGE:  call writeFix ( datFile, fixdata, result )
C
C  INPUT PARAMETERS:  
C     datFile - unit number of the objective aids file
C     fixdata - FIX_DATA struct containing data to write out
C
C  OUTPUT PARAMETERS:
C     result - return 0 for fail, 1 for success
C
C  ERROR CONDITIONS:
C
C........................MAINTENANCE SECTION............................
C
C  PRINCIPAL VARIABLES AND ARRAYS:
C
C  LANGUAGE:  FORTRAN
C
C  RECORD OF CHANGES:
C
C........................END PROLOGUE..................................
C
      subroutine writeFix (datFile, fixdata, result)
c
      implicit none
c
      INCLUDE  'dataformats.inc'
c
c         formal parameters
      integer           datFile
      type (FIX_DATA)   fixdata
      integer           result
c
c         local variables
      integer           ii
c . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
c
      result = 1
C
      do ii = 1, fixdata%numrcrds
         call outputFrcd( datFile, fixdata%fRecord(ii), result )
      enddo
C
      END

         
