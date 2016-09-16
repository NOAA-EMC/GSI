program prepbufr_decode_all
!
! read all observations out from prepbufr. 
! read bufr table from prepbufr file
!
 implicit none

 integer, parameter :: mxmn=35, mxlv=250, mxev=10
 character(80):: hdstr='SID XOB YOB DHR TYP ELV SAID T29'
 character(80):: obstr='POB QOB TOB ZOB UOB VOB PWO CAT PRSS'
 character(80):: qcstr='PQM QQM TQM ZQM WQM QPC PWQ TPC '
 character(80):: oestr='POE QOE TOE NUL WOE NUL PWE     '
 real(8) :: hdr(mxmn),obs(mxmn,mxlv,mxev),qcf(mxmn,mxlv,mxev),oer(mxmn,mxlv,mxev)

 INTEGER        :: ireadmg,ireadsb

 character(8)   :: subset
 integer        :: unit_in=10,idate,nmsg,ntb

 character(8)   :: c_sid
 real(8)        :: rstation_id
 equivalence(rstation_id,c_sid)

 integer        :: itype
 integer        :: i,k,iret, numobs,n
!
!
 open(24,file='prepbufr.table')
 open(unit_in,file='prepbufr',form='unformatted',status='old')
 call openbf(unit_in,'IN',unit_in)
 call dxdump(unit_in,24)
 call datelen(10)
   nmsg=0
   numobs=0
   msg_report: do while (ireadmg(unit_in,subset,idate) == 0)
     nmsg=nmsg+1
     ntb = 0
!     write(*,*)
!     write(*,'(3a,i10)') 'subset=',subset,' cycle time =',idate
     sb_report: do while (ireadsb(unit_in) == 0)
       ntb = ntb+1
       call ufbint(unit_in,hdr,mxmn,1   ,iret,hdstr)
!       call ufbint(unit_in,obs,mxmn,mxlv,iret,obstr)
       call UFBEVN(unit_in,obs,mxmn,mxlv,mxev,iret,obstr)
!       call ufbint(unit_in,oer,mxmn,mxlv,iret,oestr)
       call UFBEVN(unit_in,oer,mxmn,mxlv,mxev,iret,oestr)
!       call ufbint(unit_in,qcf,mxmn,mxlv,iret,qcstr)
       call UFBEVN(unit_in,qcf,mxmn,mxlv,mxev,iret,qcstr)
       rstation_id=hdr(1)
       itype=hdr(5)
!       write(*,*)
       if(itype >= 180 .and. itype < 188) then
       if(qcf(3,1,1) < 16.0 .and. abs(qcf(3,1,1) - 2.0 ) > 0.1) then
       write(*,'(2I10,a14,8f15.1)') ntb,iret,c_sid,(hdr(i),i=2,8)
       numobs=numobs+1
       DO k=1,iret
       DO n=1,5
         write(*,'(i3,a10,9f15.1)') n,'obs=',(obs(i,k,n),i=1,6)
!         write(*,'(i3,a10,9f15.1)') k,'oer=',(oer(i,k,n),i=1,7)
         write(*,'(i3,a10,9f15.1)') n,'qcf=',(qcf(i,k,n),i=1,8)
       ENDDO
       ENDDO
       endif
       endif
     enddo sb_report
   enddo msg_report
 call closbf(unit_in)
  write(*,*) 'numobs=',numobs

end program
