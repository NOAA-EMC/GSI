program bufr_encode_sample
!
!  example of writing one value into a bufr file
!
 implicit none
 
 character(80):: hdstr='XOB YOB DHR'
 character(80):: obstr='TOB'
 real(8) :: hdr(3),obs(1,1)
 
 character(8) subset
 integer :: unit_out=10,unit_table=20
 integer :: idate,iret
 
! set data values
 hdr(1)=75.;hdr(2)=30.;hdr(3)=-0.1  
 obs(1,1)=287.15
 idate=2008120100  ! YYYYMMDDHH
 subset='ADPUPA'   ! upper-air reports
 
! encode
 open(unit_table,file='table_prepbufr.txt')
 open(unit_out,file='sample.bufr',action='write' &
               ,form='unformatted')
 call datelen(10)
 call openbf(unit_out,'OUT',unit_table)
   call openmb(unit_out,subset,idate)
      call ufbint(unit_out,hdr,3,1,iret,hdstr)
      call ufbint(unit_out,obs,1,1,iret,obstr)
      call writsb(unit_out)
   call closmg(unit_out)
 call closbf(unit_out)

end program
