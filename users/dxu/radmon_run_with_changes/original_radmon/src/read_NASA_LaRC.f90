subroutine read_NASA_LaRC(nread,ndata,infile,obstype,lunout,twind,sis)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:  read_NASA_LaRC          Reading in NASA LaRC cloud   
!
!   PRGMMR: Ming Hu          ORG: GSD/AMB        DATE: 2009-09-21
!
! ABSTRACT: 
!     This routine reads in NASA LaRC cloud data. The data has already  
!          been interpolated into analysis grid and in form of BUFR.
!
! PROGRAM HISTORY LOG:
!    2009-09-21  Hu  initial
!    2010-04-09  Hu  make changes based on current trunk style
!    2013-03-27  Hu  add code to map obs from WRF mass H grid to analysis grid
!
!
!   input argument list:
!     infile   - unit from which to read NASA LaRC file
!     obstype  - observation type to process
!     lunout   - unit to which to write data for further processing
!     twind    - input group time window (hours)
!     sis      - observation variable name
!
!   output argument list:
!     nread    - number of type "obstype" observations read
!     ndata    - number of type "obstype" observations retained for further processing
!
! USAGE:
!   INPUT FILES:  NASALaRCCloudInGSI.bufr
!
!   OUTPUT FILES:
!
! REMARKS:
!
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 90 
!   MACHINE:  Linux cluster (WJET)
!
!$$$
!
!_____________________________________________________________________
!
  use kinds, only: r_kind,r_double,i_kind
  use constants, only: zero,one
  use convinfo, only: nconvtype,ctwind,cgross,cermax,cermin,cvar_b,cvar_pg, &
        ncmiter,ncgroup,ncnumgrp,icuse,ictype,icsubtype,ioctype
  use gsi_4dvar, only: l4dvar,winlen
  use gridmod, only: nlon,nlat,nlon_regional,nlat_regional
  use mod_wrfmass_to_a, only: wrfmass_obs_to_a8

  implicit none
!
  
  character(10),    intent(in)   :: infile,obstype
  integer(i_kind),  intent(in)   :: lunout
  integer(i_kind),  intent(inout):: nread,ndata
  real(r_kind),     intent(in   ):: twind
  character(20),    intent(in)   :: sis
!
!  For LaRC
!
  integer(i_kind) nreal,nchanl,ilat,ilon

  integer(i_kind) ifn,i,j
 
  logical :: LaRCobs

!
!  for read in bufr
!
    real(r_kind) :: hdr(5),obs(1,5)
    character(80):: hdrstr='SID XOB YOB DHR TYP'
    character(80):: obsstr='POB'

    INTEGER(i_kind),PARAMETER ::  MXBF = 160000
    INTEGER(i_kind) :: ibfmsg = MXBF/4

    character(8) subset,sid
    integer(i_kind) :: lunin,idate
    integer(i_kind)  :: ireadmg,ireadsb

    INTEGER(i_kind)  ::  maxlvl
    INTEGER(i_kind)  ::  numlvl,numLaRC,numobsa
    INTEGER(i_kind)  ::  n,k,iret
    INTEGER(i_kind),PARAMETER  ::  nmsgmax=100000
    INTEGER(i_kind)  ::  nmsg,ntb
    INTEGER(i_kind)  ::  nrep(nmsgmax)
    INTEGER(i_kind),PARAMETER  ::  maxobs=4500000 

    REAL(r_kind),allocatable :: LaRCcld_in(:,:)   ! 3D reflectivity in column

    integer(i_kind)  :: ikx
    real(r_kind)     :: timeo,t4dv

    REAL(r_double)  :: rid
    EQUIVALENCE (sid,rid)

!**********************************************************************
!
!            END OF DECLARATIONS....start of program
!
   LaRCobs = .false.
   ikx=0
   do i=1,nconvtype
       if(trim(obstype) == trim(ioctype(i)) .and. abs(icuse(i))== 1) then
           LaRCobs =.true.
           ikx=i
       endif
   end do

   nchanl= 0
   nread = 0
   ndata = 0
   ifn = 15
!
   if(LaRCobs) then
      lunin = 10            
      maxlvl= 5
      allocate(LaRCcld_in(maxlvl+2,maxobs))

      OPEN  ( UNIT = lunin, FILE = trim(infile),form='unformatted',err=200)
      CALL OPENBF  ( lunin, 'IN', lunin )
      CALL DATELEN  ( 10 )

      nmsg=0
      nrep=0
      ntb = 0
      msg_report: do while (ireadmg(lunin,subset,idate) == 0)
         nmsg=nmsg+1
         if (nmsg>nmsgmax) then
            write(6,*)'read_NASA_LaRC: messages exceed maximum ',nmsgmax
            call stop2(50)
         endif
         loop_report: do while (ireadsb(lunin) == 0)
            ntb = ntb+1
            nrep(nmsg)=nrep(nmsg)+1
            if (ntb>maxobs) then
                write(6,*)'read_NASA_LaRC: reports exceed maximum ',maxobs
                call stop2(50)
            endif

!    Extract type, date, and location information
            call ufbint(lunin,hdr,5,1,iret,hdrstr)
! check time window in subset
            if (l4dvar) then
               t4dv=hdr(4)
               if (t4dv<zero .OR. t4dv>winlen) then
                  write(6,*)'read_NASALaRC:      time outside window ',&
                       t4dv,' skip this report'
                  cycle loop_report
               endif
            else
               timeo=hdr(4)
               if (abs(timeo)>ctwind(ikx) .or. abs(timeo) > twind) then
                  write(6,*)'read_NASALaRC:  time outside window ',&
                       timeo,' skip this report'
                  cycle loop_report
               endif
            endif

! read in observations
            call ufbint(lunin,obs,1,maxlvl,iret,obsstr)
            numlvl=iret

            LaRCcld_in(1,ntb)=hdr(2)*10.0_r_kind       ! observation location, grid index i
            LaRCcld_in(2,ntb)=hdr(3)*10.0_r_kind       ! observation location, grid index j

            do k=1,numlvl
              LaRCcld_in(2+k,ntb)=obs(1,k)             ! NASA LaRC cloud products: k=1 cloud top pressure
            enddo                                      ! k=2 cloud top temperature, k=3 cloud fraction     
                                                       ! k=4 lwp,  k=5, cloud levels
         enddo loop_report
      enddo msg_report

      write(6,*)'read_NASALaRC: messages/reports = ',nmsg,'/',ntb
      numLaRC=ntb
!
      ilon=1
      ilat=2
      nread=numLaRC
      ndata=numLaRC
      nreal=maxlvl+2
      if(numLaRC > 0 ) then
          if(nlon==nlon_regional .and. nlat==nlat_regional) then
             write(lunout) obstype,sis,nreal,nchanl,ilat,ilon
             write(lunout) ((LaRCcld_in(k,i),k=1,maxlvl+2),i=1,numLaRC)
          else
             call wrfmass_obs_to_a8(LaRCcld_in,nreal,numLaRC,ilat,ilon,numobsa)
             nread=numobsa
             ndata=numobsa
             write(lunout) obstype,sis,nreal,nchanl,ilat,ilon
             write(lunout) ((LaRCcld_in(k,i),k=1,maxlvl+2),i=1,numobsa)
          endif
          deallocate(LaRCcld_in)
      endif
    endif
!
    call closbf(lunin)
    return
200 continue
    write(6,*) 'read_NASA_LaRC, Warning : cannot find LaRC data file'

end subroutine  read_NASA_LaRC
