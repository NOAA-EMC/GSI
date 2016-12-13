program bufr_decode_sample
!
! example of reading observations from bufr
!
 implicit none

 character(80):: hdstr='XOB YOB DHR'
 character(80):: obstr='TOB'
 real(8) :: hdr(3),obs(1,10)

 integer :: ireadmg,ireadsb
 character(8) subset
 integer :: unit_in=10
 integer :: idate,iret,num_message,num_subset

! decode
 open(unit_in,file='sample.bufr',action='read',form='unformatted')
 call openbf(unit_in,'IN',unit_in)
 call datelen(10)
   num_message=0
   msg_report: do while (ireadmg(unit_in,subset,idate) == 0)
     num_message=num_message+1
     num_subset = 0
     write(*,'(I10,I4,a10)') idate,num_message,subset
     sb_report: do while (ireadsb(unit_in) == 0)
       num_subset = num_subset+1
       call ufbint(unit_in,hdr,3,1 ,iret,hdstr)
       call ufbint(unit_in,obs,1,10,iret,obstr)
       write(*,'(2I5,4f8.1)') num_subset,iret,hdr,obs(1,1)
     enddo sb_report
   enddo msg_report
 call closbf(unit_in)

end program
