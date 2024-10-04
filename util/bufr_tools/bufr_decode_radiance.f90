program bufr_decode_radiance
!
! read all radaince observations out from bufr. 
! read bufr table from prepbufr file
!
 implicit none

 integer, parameter :: mxmn=35, mxlv=250
 character(80):: hdstr= &
   'SAID FOVN YEAR MNTH DAYS HOUR MINU SECO CLAT CLON CLATH CLONH HOLS'
 character(80):: hdr2b='SAZA SOZA BEARAZ SOLAZI'
 character(80):: obstr='TMBR'
! character(80):: obstr='TMBRST'
 real(8) :: hdr(mxmn),hdr2(mxmn),obs(mxlv)

 INTEGER        :: ireadmg,ireadsb

 character(8)   :: subset
 integer        :: unit_in=10,idate,nmsg,ntb
 integer,parameter:: max_sat_type=20
 integer        :: nsat_type(max_sat_type),nsat_num(max_sat_type)
 integer        :: i,k,iret,ksatid,nchanl,num_sat_type,ii
!
 nchanl=15
 nsat_num=0
 nsat_type=0
!
 open(24,file='bufr.table')
 open(unit_in,file='1bamua',form='unformatted',status='old')
 call openbf(unit_in,'IN',unit_in)
 call dxdump(unit_in,24)
 call datelen(10)
   nmsg=0
   ntb = 0
   num_sat_type = 0
   msg_report: do while (ireadmg(unit_in,subset,idate) == 0)
     nmsg=nmsg+1
     write(*,*)
     write(*,'(3a,i10)') 'subset=',subset,' cycle time =',idate
     sb_report: do while (ireadsb(unit_in) == 0)
       ntb = ntb+1
       call ufbint(unit_in,hdr ,mxmn,1  ,iret,hdstr)
       call ufbint(unit_in,hdr2,mxmn,1  ,iret,hdr2b)

       call ufbrep(unit_in,obs ,1   ,nchanl,iret,obstr)
       ksatid=nint(hdr(1))
!       write(*,*)
!       write(*,'(2I10,I14,13f8.1,)') ntb,iret,ksatid,(hdr(i),i=3,10),hdr(13), &
!                                     (hdr2(i),i=1,4)
!       write(*,'(a10,15f7.1)') 'obs=',(obs(i),i=1,iret)

! satellite type inventory
       if(num_sat_type == 0 ) then
          num_sat_type=1
          nsat_type(num_sat_type)=ksatid
          nsat_num(num_sat_type)= 1
       else
          ii=0
          DO i=1,num_sat_type
             if(nsat_type(i) == ksatid) ii=i
          ENDDO
          if( ii > 0 .and. ii <=num_sat_type) then
             nsat_num(ii)=nsat_num(ii) + 1
          else
             num_sat_type=num_sat_type+1
             if(num_sat_type > max_sat_type) then
                write(*,*) 'Error: too many satellite types'
                write(*,*) 'Need to increase max_sat_type'
                stop 1234
             endif
             nsat_type(num_sat_type)=ksatid
             nsat_num(num_sat_type)=1
          endif
       endif

     enddo sb_report
   enddo msg_report
 call closbf(unit_in)

 write(*,*) 'message=',nmsg,'  subset=',ntb
 DO i=1,num_sat_type
   write(*,'(i4,2i10)') i,nsat_type(i),nsat_num(i)
 ENDDO


end program
