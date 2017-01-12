program prepbufr_decode_inventory
!
! read all observation head out from prepbufr and count data types. 
! read bufr table from prepbufr file
!
 implicit none

 integer, parameter :: mxmn=35
 character(80):: hdstr='SID XOB YOB DHR TYP ELV SAID T29'
 real(8) :: hdr(mxmn)

 INTEGER        :: ireadmg,ireadsb

 character(8)   :: subset
 integer        :: unit_in=10,unit_table=24,idate,nmsg,ntb

 character(8)   :: c_sid
 real(8)        :: rstation_id
 equivalence(rstation_id,c_sid)
!
!
 integer, parameter :: max_data_type=1000
 character(8) :: message_type_all(max_data_type)
 integer :: num_data_all(max_data_type)
 integer :: num_data_time(max_data_type,13)
 integer :: num_data_type
 real    :: max_lat(max_data_type)
 real    :: max_lon(max_data_type)
 real    :: min_lat(max_data_type)
 real    :: min_lon(max_data_type)
 real    :: max_time(max_data_type)
 real    :: min_time(max_data_type)
!
 integer        :: i,k,iret,itype,j,inum
 real :: rlat,rlon,rtime
!
 message_type_all(:)=''
 num_data_all(:)=0
 num_data_type=0
 max_lat=-999.0
 min_lat=999.0
 max_lon=-999.0
 min_lon=999.0
 max_time=-999.0
 min_time=999.0
 num_data_time=0
!
 open(unit_table,file='out_bufr.table')
 open(unit_in,file='prepbufr',form='unformatted',status='old')
 call openbf(unit_in,'IN',unit_in)
 call dxdump(unit_in,unit_table)
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
       rstation_id=hdr(1)
       if( hdr(5) > 1.0e8) then
          write(*,*) 'This obs includes wrong data type:', hdr(5)
          cycle
       endif
       if( hdr(5) > max_data_type) then
          write(*,*) 'This obs type is out max_data_type:', hdr(5)
          stop 123
       endif

       rlat=hdr(3)
       rlon=hdr(2)
       if(abs(rlat)>90.0 .or. abs(rlon)>360.0 ) then
          write(*,*) 'This obs includes wrong location(x,y):', rlon,rlat
          cycle 
       endif
!       if(abs(rlon-360.0) < 0.0001 ) rlon=rlon-360.0
!       if(rlon < 0.0) rlon=rlon+360.0

       rtime=hdr(4)
       itype=int(hdr(5))
       message_type_all(itype)=subset
       num_data_all(itype)= num_data_all(itype) + 1
       max_lat(itype)=max(max_lat(itype),rlat)
       min_lat(itype)=min(min_lat(itype),rlat)
       max_lon(itype)=max(max_lon(itype),rlon)
       min_lon(itype)=min(min_lon(itype),rlon)
       max_time(itype)=max(max_time(itype),rtime)
       min_time(itype)=min(min_time(itype),rtime)
       inum=min(max(1,(int((rtime+2.00)*4.0) + 1)),13)
       num_data_time(itype,inum)=num_data_time(itype,inum)+1
     enddo sb_report
   enddo msg_report
 call closbf(unit_in)
!
 DO i=1,max_data_type
   if(num_data_all(i) > 0 ) num_data_type=num_data_type+1
 ENDDO
 write(*,*) 'number of data type =',num_data_type
 write(*,1000) 'data','obs',   'data','obs',   'message','Lon','Lon','Lat','Lat','Time','Time'
 write(*,1000) 'type','number','type','number','type',   'min','max','min','max','min','max'
1000 format(2(a5,a10),a8,2x,6a8)
 DO i=100,199
   if(num_data_all(i) > 0 .or. num_data_all(i+100) > 0 ) then
      if(num_data_all(i) > 0) then
         subset=message_type_all(i)
      else
         subset=message_type_all(i+100)
      endif
      write(*,'(2(i5,i10),a10,6f8.2)') i,num_data_all(i),i+100,num_data_all(100+i),subset, &
                          min(min_lon(i),min_lon(i+100)),max(max_lon(i),max_lon(i+100)),   &
                          min(min_lat(i),min_lat(i+100)),max(max_lat(i),max_lat(i+100)),   &
                          min(min_time(i),min_time(i+100)),max(max_time(i),max_time(i+100))
   endif
 ENDDO
1100 format(2(a5,a10),a8,2x,13a8)
 write(*,1100) 'data','obs',   'data','obs',   'message','Time','Time','Time','Time','Time','Time','Time','Time','Time','Time','Time','Time','Time'
 write(*,1100) 'type','number','type','number','type',   '-2','-1.75','-1.50','-1.25','-1.00','-0.75','-0.50','-0.25','0.00','0.25','0.50','0.75','1.00'
 DO i=100,199
   if(num_data_all(i) > 0 .or. num_data_all(i+100) > 0 ) then
      if(num_data_all(i) > 0) then
         subset=message_type_all(i)
      else
         subset=message_type_all(i+100)
      endif
      write(*,'(2(i5,i10),a10,13i8)') i,num_data_all(i),i+100,num_data_all(100+i),subset, &
                          (num_data_time(i,j)+num_data_time(100+i,j),j=1,13)
   endif
 ENDDO
 DO i=1,99
   if(num_data_all(i) > 0 ) then
      num_data_type=num_data_type+1
      write(*,'(i10,i15,a15)') i,num_data_all(i),message_type_all(i)
   endif
 ENDDO
 DO i=300,max_data_type
   if(num_data_all(i) > 0 ) then
      num_data_type=num_data_type+1
      write(*,'(i10,i15,a15)') i,num_data_all(i),message_type_all(i)
   endif
 ENDDO

end program
