program prepbufr_decode_all
!
! read all observations out from prepbufr. 
! read bufr table from prepbufr file
!
 implicit none

 integer, parameter :: mxmn=35, mxlv=250
 character(80):: hdstr='SID XOB YOB DHR TYP ELV SAID T29'
! cloud top pressure, total cloud amount, cloud top temperature,
! cloud top temp. qc mark
 character(80):: obstr='CDTP TOCC GCDTT CDTP_QM'

 real(8) :: hdr(mxmn),obs(mxmn,mxlv)

 INTEGER        :: ireadmg,ireadsb

 character(8)   :: subset
 integer        :: unit_in=10,idate,nmsg,ntb

 character(8)   :: c_sid
 real(8)        :: rstation_id
 equivalence(rstation_id,c_sid)

 integer        :: i,k,iret
!
!
 open(24,file='prepbufr.table')
 open(unit_in,file='prepbufr',form='unformatted',status='old')
 call openbf(unit_in,'IN',unit_in)
 call dxdump(unit_in,24)
 call datelen(10)
   nmsg=0
   msg_report: do while (ireadmg(unit_in,subset,idate) == 0)
     nmsg=nmsg+1
     ntb = 0
!     write(*,*)
!     write(*,'(3a,i10)') 'subset=',subset,' cycle time =',idate
     sb_report: do while (ireadsb(unit_in) == 0)
       ntb = ntb+1
       call ufbint(unit_in,hdr,mxmn,1   ,iret,hdstr)
       call ufbint(unit_in,obs,4,1,iret,obstr)
       rstation_id=hdr(1)
       if(int(hdr(5)) == 151) then
!         write(*,'(a,2I10,a14,8f14.1)') subset,ntb,iret,c_sid,(hdr(i),i=2,8)
          DO k=1,iret
            write(*,'(7f12.3)') hdr(2),hdr(3),obs(1,k)/100.0,(obs(i,k),i=2,3)
          ENDDO
       endif
     enddo sb_report
   enddo msg_report
 call closbf(unit_in)

end program
