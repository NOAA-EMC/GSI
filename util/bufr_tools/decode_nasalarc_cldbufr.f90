program decode_nasalarc_cldbufr
!
!   PRGMMR: Ming Hu          ORG: GSD        DATE: 2010-07-09
!
! ABSTRACT: 
!     This routine read in NASA LaRC cloud products 
!     from a bufr file                      
!
! PROGRAM HISTORY LOG:
!
!   variable list
!
! USAGE:
!   INPUT FILES:  
!
!   OUTPUT FILES:
!
! REMARKS:
!
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 90 + EXTENSIONS
!   MACHINE:  wJET
!
!$$$
!
!_____________________________________________________________________
!
  use kinds, only: r_kind,i_kind

  implicit none
!
!
!
  character(80):: hdstr='YEAR  MNTH  DAYS HOUR  MINU  SECO'
  character(80):: obstr='CLATH  CLONH CLDP HOCT CDTP EBBTH VILWC'
! CLDP     |  CLOUD PHASE
! HOCB     | HEIGHT OF BASE OF CLOUD
! HOCT     | HEIGHT OF TOP OF CLOUD             (METERS)
! CDBP     | PRESSURE AT BASE OF CLOUD
! CDTP     | PRESSURE AT TOP OF CLOUD           (PA)
! EBBTH    | EQUIVALENT BLACK BODY TEMPERATURE  (KELVIN)
! VILWC    | VERTICALLY-INTEGRATED LIQUID WATER CONTENT

  real(8) :: hdr(6),obs(7,1)

  INTEGER :: ireadmg,ireadsb

  character(8) subset
  integer :: unit_in=10,idate,iret,nmsg,ntb

!
!  For NASA LaRC 
!
  CHARACTER*40   satfile
  integer(i_kind) :: east_time, west_time

  INTEGER ::   maxobs, numobs  ! dimension
  INTEGER(i_kind) ::  obs_time
  REAL*8      time_offset
!
!
!  ** misc

!  integer i,j,k
!  Integer nf_status,nf_fid,nx,ny,nf_vid
!
!  integer :: status
  character*10  atime
!
!**********************************************************************
!
 satfile='NASA_LaRC_cloud.bufr'
 open(24,file='NASA.bufrtable')
 open(unit_in,file=trim(satfile),form='unformatted')
 call openbf(unit_in,'IN',unit_in)
 call dxdump(unit_in,24)
 call datelen(10)
   nmsg=0
   ntb = 0
   msg_report: do while (ireadmg(unit_in,subset,idate) == 0)
     nmsg=nmsg+1
     sb_report: do while (ireadsb(unit_in) == 0)
       call ufbint(unit_in,hdr,6,1,iret,hdstr)
       obs_time=int((hdr(1)-2000.0)*100000000+hdr(2)*1000000+hdr(3)*10000+hdr(4)*100+hdr(5))
       call ufbint(unit_in,obs,7,1,iret,obstr)
       write(50,'(5f15.2)') obs(2,1)+360.0,obs(1,1),obs(5,1),obs(6,1),obs(7,1)
         ntb = ntb+1
     enddo sb_report
   enddo msg_report
   write(*,*) 'message/reports num=',nmsg,ntb
 call closbf(unit_in)

end program decode_nasalarc_cldbufr
