program lghtngbufr_decode
!
! read all observations out from prepbufr. 
! read bufr table from prepbufr file
!
 implicit none

 integer, parameter :: mxmn=35, mxlv=1
 character(80):: hdstr='YEAR  MNTH  DAYS  HOUR  MINU CLATH  CLONH'
 character(80):: obstr='AMPLS  PLRTS  OWEP  NOFL'
 real(8) :: hdr(mxmn),obs(mxmn,mxlv)

 INTEGER        :: ireadmg,ireadsb

 character(8)   :: subset
 integer        :: unit_in=10,idate,nmsg,ntb

 integer        :: i,k,iret
!
!
 open(24,file='lghtngbufr.table')
 open(unit_in,file='lghtngbufr',form='unformatted',status='old')
 call openbf(unit_in,'IN',unit_in)
 call dxdump(unit_in,24)
 call datelen(10)
   nmsg=0
   ntb = 0
   msg_report: do while (ireadmg(unit_in,subset,idate) == 0)
     nmsg=nmsg+1
     write(*,*)
     write(*,'(3a,i10)') 'subset=',subset,' cycle time =',idate
     sb_report: do while (ireadsb(unit_in) == 0)
        ntb = ntb+1
        call ufbint(unit_in,hdr,mxmn,1,iret,hdstr)
        call ufbint(unit_in,obs,mxmn,1,iret,obstr)
!        write(*,*)
!        write(*,'(I10,8f14.1)') ntb,(hdr(i),i=1,7)
!        write(*,'(9f14.1)') (obs(i,1),i=1,4)
     enddo sb_report
   enddo msg_report
 call closbf(unit_in)

end program
