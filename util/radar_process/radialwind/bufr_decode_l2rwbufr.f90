program bufr_decode_l2rwbufr
!
! 10/17/2013             DTC
!
! Read all level 2 radar radial velocity obs out from NCEP Level II radial
! velocity bufr. Also, read and write bufr table for radar from the file.
!
! MNEMONIC used in this code
!    SSTN CLAT CLON HSMSL HSALG ANEL ANAZ QCRW
!    YEAR MNTH DAYS HOUR MINU SECO
!    SCID HNQV VOCP VOID
!    DIST125M DMVR DVSW
! Please refer BUFR table for explanations of MNEMONIC
!
 implicit none

 integer, parameter :: mxmn=35, mxlv=1000
 character(80):: hdstr= 'SSTN CLAT CLON HSMSL HSALG ANEL ANAZ QCRW'
 character(80):: hdstr2='YEAR MNTH DAYS HOUR MINU SECO' 
 character(80):: hdstr3='SCID HNQV VOCP VOID'
 character(80):: obstr='DIST125M DMVR DVSW'
 real(8) :: hdr(mxmn),hdr2(mxmn),hdr3(mxmn),obs(3,mxlv)

 INTEGER        :: ireadmg,ireadsb

 character(8)   :: subset
 integer        :: unit_in=10,unit_out=20
 integer        :: idate,nmsg,ntb
 integer        :: i,k,iret

 character(8)   :: c_sid
 real(8)        :: rstation_id
 equivalence(rstation_id,c_sid)

 integer        :: numrwbin
!
 open(unit_out,file='l2rwbufr.bin',form='unformatted',status='new')

 open(24,file='bufr_radar.table')
 open(unit_in,file='l2rwbufr',form='unformatted',status='old')
 call openbf(unit_in,'IN',unit_in)
 call dxdump(unit_in,24)
 call datelen(10)
   nmsg=0
   ntb = 0
   msg_report: do while (ireadmg(unit_in,subset,idate) == 0)
     nmsg=nmsg+1
!     write(*,*)
!     write(*,'(3a,i10)') 'subset=',subset,' cycle time =',idate
     sb_report: do while (ireadsb(unit_in) == 0)
!       ntb = ntb+1
       call ufbint(unit_in,hdr ,mxmn,1  ,iret,hdstr)
       call ufbint(unit_in,hdr2,mxmn,1  ,iret,hdstr2)
       call ufbint(unit_in,hdr3,mxmn,1  ,iret,hdstr3)
       rstation_id=hdr(1)
!       write(*,*)
       write(*,'(2I10,a14,10f8.1)') ntb,iret,c_sid,(hdr(i),i=2,8)
       write(*,'(10f8.1)') (hdr2(i),i=1,6)
       call ufbint(unit_in,obs ,3,  mxlv,iret,obstr)
       if(iret >= 1) then
          numrwbin=0
          do i=1,iret
             if(obs(2,i) < 10000.0) numrwbin=numrwbin+1
          enddo
          if(numrwbin>=1) then
             ntb = ntb+1
             write(*,'(2I10,a14,10f8.1)') ntb,numrwbin,c_sid,(hdr(i),i=2,8)
             write(*,'(34x,10f8.1)') (hdr2(i),i=1,6)
             write(unit_out) ntb,numrwbin,(hdr(i),i=1,8)
             write(unit_out) (hdr2(i),i=1,6)
             write(unit_out) (hdr3(i),i=1,4)
          endif
          do i=1,iret
             if(obs(2,i) < 10000.0) & 
                write(*,'(a10,i10,5f16.2)') 'rw obs=',i,obs(:,i)
                write(unit_out) obs(1:3,i)
          enddo
       endif
     enddo sb_report
   enddo msg_report
 call closbf(unit_in)

 close(unit_out)

 write(*,*) 'message=',nmsg,'  subset=',ntb

end program
