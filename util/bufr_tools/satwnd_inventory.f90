program prepbufr_decode_inventory
!
! read all observation head out from prepbufr and count data types. 
! read bufr table from prepbufr file
!
 use kinds, only: i_kind, r_kind, r_double
 implicit none

 integer, parameter :: mxmn=35
 character(70) obstr_v1, obstr_v2,hdrtr_v1,hdrtr_v2
 character(50) qcstr

 data hdrtr_v1 /'SAID CLAT CLON YEAR MNTH DAYS HOUR MINU SWCM SAZA OGCE SCCF SWQM'/
 data hdrtr_v2 /'SAID CLATH CLONH YEAR MNTH DAYS HOUR MINU SWCM SAZA OGCE SCCF SWQM'/

 data obstr_v1 /'HAMD PRLC WDIR WSPD'/
 data obstr_v2 /'EHAM PRLC WDIR WSPD'/
 data qcstr /' OGCE GNAP PCCF'/

 real(8) :: hdrdat(13)

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
 integer :: typeid(max_data_type)
 integer :: typelist(max_data_type)
 integer :: subtypelist(max_data_type)
 integer :: thistype,satid
 real    :: max_lat(max_data_type)
 real    :: max_lon(max_data_type)
 real    :: min_lat(max_data_type)
 real    :: min_lon(max_data_type)
 real    :: max_time(max_data_type)
 real    :: min_time(max_data_type)
!
 integer        :: i,k,iret,itype,j,inum
 real :: rlat,rlon,rtime
 character(len=12) :: cdate
 integer :: cycYear,cycMon,cycDay,cycHr,cycMin
 integer :: refDay,refHr,refMin
 integer :: cycRefMin, obsRefMin
!
 cdate='201811130000'
 read(cdate,'(I4,4I2)') cycYear,cycMon,cycDay,cycHr,cycMin
 write(*,*) cycYear,cycMon,cycDay,cycHr,cycMin
 cycRefMin=24*60
 refDay=cycDay-1
 refHr=cycHr
 refMin=cycMin
!
 message_type_all(:)=''
 num_data_all(:)=0

 num_data_type=0
 typelist=-99
 subtypelist=-99

 max_lat=-999.0
 min_lat=999.0
 max_lon=-999.0
 min_lon=999.0
 max_time=-999.0
 min_time=999.0
 num_data_time=0
!
 open(unit_table,file='out_bufr.table')
 open(unit_in,file='satwndbufr',form='unformatted',status='old')
 call openbf(unit_in,'IN',unit_in)
 call dxdump(unit_in,unit_table)
 call datelen(10)
   nmsg=0
   msg_report: do while (ireadmg(unit_in,subset,idate) == 0)
     nmsg=nmsg+1
     ntb = 0
!     write(*,*)
     write(*,'(3a,i10)') 'subset=',subset,' cycle time =',idate
     sb_report: do while (ireadsb(unit_in) == 0)
       ntb = ntb+1
       if(subset=='NC005030' .or. subset=='NC005031' .or. subset=='NC005032' .or.&
          subset=='NC005034' .or. subset=='NC005039' ) then
          call ufbint(unit_in,hdrdat,13,1,iret,hdrtr_v2)
       else
          call ufbint(unit_in,hdrdat,13,1,iret,hdrtr_v1)
       endif
       call get_satwnd_itype(subset,hdrdat,itype)

       satid=int(hdrdat(1))
       call find_typeid(max_data_type,typelist,subtypelist,num_data_type,itype,satid,thistype)
!
!       write(*,'(6F10.1)') hdrdat(1),(hdrdat(i),i=4,8)
       rlat=hdrdat(2)
       rlon=hdrdat(3)
       if(rlon < -80) rlon=360+rlon
       if(abs(rlat)>90.0 .or. abs(rlon)>360.0 ) then
          write(*,*) 'This obs includes wrong location(x,y):', rlon,rlat
          cycle 
       endif

       obsRefMin=(hdrdat(6)-refDay)*24*60+(hdrdat(7)-refHr)*60+(hdrdat(8)-refMin)
       rtime=obsRefMin-cycRefMin
       rtime=rtime/60.0
       message_type_all(thistype)=subset
       num_data_all(thistype)= num_data_all(thistype) + 1
       max_lat(thistype)=max(max_lat(thistype),rlat)
       min_lat(thistype)=min(min_lat(thistype),rlat)
       max_lon(thistype)=max(max_lon(thistype),rlon)
       min_lon(thistype)=min(min_lon(thistype),rlon)
       max_time(thistype)=max(max_time(thistype),rtime)
       min_time(thistype)=min(min_time(thistype),rtime)
       inum=min(max(1,(int((rtime+2.00)*4.0) + 1)),13)
       num_data_time(thistype,inum)=num_data_time(thistype,inum)+1
     enddo sb_report
   enddo msg_report
 call closbf(unit_in)
!
 write(*,*) 'number of data type =',num_data_type
 write(*,1000) 'data','data',   'obs',   'message','Lon','Lon','Lat','Lat','Time','Time'
 write(*,1000) 'type','subtype',' number','type',   'min','max','min','max','min','max'
1000 format(10x,(a5,a10),2x,a8,2x,7a8)
 DO i=1,num_data_type
      write(*,'(5x,3i10,a10,6f8.2)') typelist(i),subtypelist(i),num_data_all(i),message_type_all(i), &
                          min_lon(i),max_lon(i),min_lat(i),max_lat(i),min_time(i),max_time(i)
 ENDDO
1100 format(10x,(a5,a10),2x,a8,2x,14a8)
 write(*,1100) 'data',  'data','obs',   'message','Time','Time','Time','Time','Time','Time','Time','Time','Time','Time','Time','Time','Time'
 write(*,1100) 'type', 'subtype',' number','type',   '-2','-1.75','-1.50','-1.25','-1.00','-0.75','-0.50','-0.25','0.00','0.25','0.50','0.75','1.00'
 DO i=1,num_data_type
      write(*,'(5x,3i10,a10,14i8)')  typelist(i),subtypelist(i),num_data_all(i),message_type_all(i), &
                          (num_data_time(i,j),j=1,13)
 ENDDO

end program

subroutine find_typeid(max_data_type,typelist,subtypelist,num_data_type,itype,satid,thistype)
!
 implicit none
!
 integer,intent(in) :: max_data_type
 integer,intent(inout) :: num_data_type
 integer,intent(inout) :: typelist(max_data_type)
 integer,intent(inout) :: subtypelist(max_data_type)
 integer,intent(in) :: itype,satid
 integer,intent(inout) :: thistype
!
 integer :: i,j
!
 thistype=0
 if(num_data_type > 0) then
   thistype=0
   do i=1,num_data_type
     if((typelist(i)==itype) .and. (subtypelist(i)==satid)) thistype=i
   enddo
   if(thistype==0) then
      num_data_type=num_data_type+1
      typelist(num_data_type)=itype
      subtypelist(num_data_type)=satid
      thistype=num_data_type
   endif
 else
   num_data_type=1
   typelist(num_data_type)=itype
   subtypelist(num_data_type)=satid
   thistype=num_data_type
 endif 

end subroutine

subroutine get_satwnd_itype(subset,hdrdat,itype)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    get_satwnd_itype
!   prgmmr: Ming Hu      org: NOAA/GSD                date: 2018-09-11
!
! abstract:  This routine decide satellite wind  type 
!$$$
  use constants, only: one,two,three,four,five
  use kinds, only: i_kind, r_kind, r_double

  implicit none
  integer(i_kind), intent(inout) :: itype
  real(r_double),dimension(13), intent(in) :: hdrdat
  character(8), intent(in) :: subset

  real(r_kind),parameter:: r50= 50.0_r_kind
  real(r_kind),parameter:: r80= 80.0_r_kind
  real(r_kind),parameter:: r100= 100.0_r_kind
  real(r_kind),parameter:: r199= 199.0_r_kind
  real(r_kind),parameter:: r200= 200.0_r_kind
  real(r_kind),parameter:: r250= 250.0_r_kind
  real(r_kind),parameter:: r299= 299.0_r_kind
  real(r_kind),parameter:: r799= 799.0_r_kind
  real(r_kind),parameter:: r700= 700.0_r_kind

!

        if(trim(subset) == 'NC005064' .or. trim(subset) == 'NC005065' .or. &
           trim(subset) == 'NC005066') then
           if( hdrdat(1) <r80 .and. hdrdat(1) >= r50) then   !the range of EUMETSAT satellite IDS
              if(hdrdat(9) == one)  then                  ! IR winds
                 itype=253
              else if(hdrdat(9) == two) then              ! visible winds
                 itype=243
              else if(hdrdat(9) == three) then            ! WV cloud top
                 itype=254
              else if(hdrdat(9) >= four) then             ! WV deep layer, monitored
                itype=254
              endif
           endif
        else if(trim(subset) == 'NC005044' .or. trim(subset) == 'NC005045' .or. &
           trim(subset) == 'NC005046') then
           if( hdrdat(1) >=r100 .and. hdrdat(1) <=r199 ) then   ! the range of JMA satellite IDS
              if(hdrdat(9) == one)  then                            ! IR winds
                 itype=252
              else if(hdrdat(9) == two) then                        ! visible winds
                 itype=242
              else if(hdrdat(9) == three) then                      ! WV cloud top
                 itype=250
              else if(hdrdat(9) >= four) then                       ! WV deep layer,monitored
                 itype=250
              endif
           endif
        else if(trim(subset) == 'NC005010' .or. trim(subset) == 'NC005011' .or. &
           trim(subset) == 'NC005012' ) then
           if( hdrdat(1) >=r250 .and. hdrdat(1) <=r299 ) then  ! the range of NESDIS satellite IDS  
              if(hdrdat(9) == one)  then                            ! IR winds
                 if(hdrdat(12) <50000000000000.0_r_kind) then
                    itype=245
                 else
                    itype=240                                       ! short wave IR winds
                 endif
              else if(hdrdat(9) == two  ) then    ! visible winds
                 itype=251
              else if(hdrdat(9) == three ) then   ! WV cloud top
                 itype=246
              else if(hdrdat(9) >= four ) then    ! WV deep layer,monitored
                 itype=247
              endif
           endif
        else if(trim(subset) == 'NC005070' .or. trim(subset) == 'NC005071'  ) then
           if( hdrdat(1) >=r700 .and. hdrdat(1) <= r799 ) then    ! the range of NASA Terra and Aqua satellite IDs
              if(hdrdat(9) == one)  then                            ! IR winds
                 itype=257
              else if(hdrdat(9) == three) then                      ! WV cloud top
                 itype=258
              else if(hdrdat(9) >= four) then                       ! WV deep layer
                 itype=259
              endif
           endif
        else if( trim(subset) == 'NC005080') then                    
           if( hdrdat(1) <10.0_r_kind .or. (hdrdat(1) >= 200.0_r_kind .and. &
               hdrdat(1) <=223.0_r_kind) ) then      ! the range of EUMETSAT and NOAA polar orbit satellite IDs  
              if(hdrdat(9) == one)  then                            ! IR winds
                 itype=244
              else
                 write(6,*) 'READ_SATWND: wrong derived method value'
              endif
           endif
        else if( trim(subset) == 'NC005019') then                   ! GOES shortwave winds
           if(hdrdat(1) >=r250 .and. hdrdat(1) <=r299 ) then   ! The range of NESDIS satellite IDS
              if(hdrdat(9) == one)  then                            ! short wave IR winds
                 itype=240
              endif
           endif
        else if( trim(subset) == 'NC005090') then                   ! VIIRS winds 
           if(hdrdat(1) >=r200 .and. hdrdat(1) <=r250 ) then   ! The range of satellite IDS
              if(hdrdat(9) == one)  then                            ! VIIRS IR winds
                 itype=260
              endif
           endif
        !GOES-R section of the 'if' statement over 'subsets' 
        else if(trim(subset) == 'NC005030' .or. trim(subset) == 'NC005031' .or. trim(subset) == 'NC005032' .or. &
                trim(subset) == 'NC005034' .or. trim(subset) == 'NC005039') then
! Commented out, because we need clarification for SWCM/hdrdat(9) from Yi Song
! NOTE: Once it is confirmed that SWCM values are sensible, apply this logic and replace lines 685-702
!                 if(hdrdat(9) == one)  then
!                    if(hdrdat(12) <50000000000000.0_r_kind) then
!                     itype=245                                      ! GOES-R IR(LW) winds
!                    else
!                     itype=240                                      ! GOES-R IR(SW) winds
!                    endif
!                 else if(hdrdat(9) == two  ) then
!                    itype=251                                       !  GOES-R VIS    winds
!                 else if(hdrdat(9) == three ) then
!                    itype=246                                       !  GOES-R CT WV  winds
!                 else if(hdrdat(9) >= four ) then 
!                    itype=247                                       !  GOES-R CS WV  winds
!                 endif

!Temporary solution replacing the commented code above
                 if(trim(subset) == 'NC005030')  then                 ! IR LW winds
                    itype=245
                 else if(trim(subset) == 'NC005039')  then            ! IR SW winds
                    itype=240                                      
                 else if(trim(subset) == 'NC005032')  then            ! VIS winds
                    itype=251
                 else if(trim(subset) == 'NC005034')  then            ! WV cloud top
                    itype=246
                 else if(trim(subset) == 'NC005031')  then            ! WV clear sky/deep layer
                    itype=247
                 endif

!| NC005030 | A62116 | MSG TYPE 005-030 NESDIS SATWIND, GOES-16 IR(LW)   (BUFR  |
!| NC005031 | A62117 | MSG TYPE 005-031 NESDIS SATWIND, GOES-16 WV-IMG/DL(BUFR  |
!| NC005032 | A62118 | MSG TYPE 005-032 NESDIS SATWIND, GOES-16 VIS      (BUFR  |
!| NC005034 | A62119 | MSG TYPE 005-034 NESDIS SATWIND, GOES-16 WV-IMG/CT(BUFR  |
!| NC005039 | A62120 | MSG TYPE 005-039 NESDIS SATWIND, GOES-16 IR(SW)   (BUFR  |

         endif

end subroutine get_satwnd_itype
