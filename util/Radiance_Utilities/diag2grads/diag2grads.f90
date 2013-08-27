!$$$  program documentation block
!                .      .    .                                       .
!  program: diag2grad                               create GrADS files
!   prgmmr: tahara           org: np20                date: 2003-01-01
!
! abstract:  This program create a GrADS station data file and its 
!            control file a GSI radiance diagnostic file.
!
! program history log:
!   2010-12-22 treadon - add this doc block
!
! contains
!   adjust_lon - adjust longitude to degrees west
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$

program diag2grads

! Include modules
  use Utilities_Time
  use read_diag
  use grads_hdr
  use write_station

  implicit none


! Declare and set parameters
  integer,parameter :: ft_namelist  = 1         ! (in)  namelist
  integer,parameter :: max_n_chan  = 900
! real(4),parameter :: Criterion_SolarZenAng = 96._8    ! 96 degrees is better
  real(4),parameter :: Criterion_SolarZenAng = 90._8


! Declare namelist with defaults
  logical           :: retrieval        = .false.

  integer           :: idsat            = 14   ! HIRS14
  integer           :: n_chan           = 19

  integer           :: ft_diag_top      = 10   ! diagnostic files
  integer           :: ft_diag_end      = 10

  integer           :: datetime(5)      = (/ 2000, 1, 1, 0, 0 /)
  integer           :: tint             = 6

  real(4)           :: North            =  90. ! Statistics area
  real(4)           :: South            = -90. ! Statistics area
  real(4)           :: West             =   0. ! Statistics area
  real(4)           :: East             = 360. ! Statistics area

  integer           :: qc               =   0  ! 0:all 1:only qc pass -1:only rejected
  integer           :: solar            =   0  ! 0:all 1:night 2:day
  character(len=5)  :: cqc(-1:1)        = (/ 'ALL  ', 'PASS ', 'RJCT ' /)
  character(len=5)  :: clandsea(0:2)    = (/ 'ALL  ', 'LAND ', 'SEA  ' /)
  character(len=5)  :: csolar(0:2)      = (/ 'ALL  ', 'NIGHT', 'DAY  ' /)
  
  character(len=GRADS_MAXLEN_COMMENT)  :: comment  = ''
  character(len=GRADS_MAXLEN_FILENAME) :: filename = ''
  character(len=GRADS_MAXLEN_FILENAME+4) :: dsetname = ''
  
  namelist / PARM / comment, ft_diag_top, ft_diag_end, filename, &
       idsat, n_chan, datetime, tint, South, North, West, East, &
       qc, solar, retrieval
  


! Declare variables for reading / processing satellite data
  type(diag_header_fix_list )             :: header_fix
  type(diag_header_chan_list),allocatable :: header_chan(:)
  type(diag_data_name_list  )             :: data_name
  type(diag_data_fix_list   )             :: data_fix
  type(diag_data_chan_list  ),allocatable :: data_chan(:)
  type(diag_data_extra_list ),allocatable :: data_extra(:,:)
  
  real(4),allocatable :: nuchan(:)
  real(4),allocatable :: data_sfc(:)
  real(4),allocatable :: data_lvl(:,:)

  character(len=GRADS_LEN_ID) :: id


! Other variables
  character(8):: stid
  integer :: ft_diag
  
  integer :: lsflag, iceflag, scan, mype
  integer :: iflag, iexist, nelem_sfc,nelem_lvl
  
  integer :: ttlmin_file, ttlmin_assign, ttlmin_assign0
  integer :: time_work(5)
  
  integer :: iobs_total
  integer :: i, k, n
  integer :: npred_radiag, n_bcor
  integer :: lunstn,lunctl
  integer :: nlev,nflag,itime_terminator

  real(4):: rlat,rlon,rtim

  npred_radiag = 5
  lunstn = 21
  lunctl = 22


! Read namelist
  read(ft_namelist,PARM)



! Adjust longitude
  call adjust_lon( East )
  call adjust_lon( West )
  if( East == West )then
     West =   0.
     East = 360.
  endif
  

! Print namelist
  write(6,*)' '
  write(6,*)'*** NAMELIST ***************************************************'
  write(6,*) 'Title                : ', trim(comment)
  write(6,*) 'FT # of diag files   : ', ft_diag_top, ft_diag_end
  write(6,*) 'GrADS data file name : ', trim(filename)
  write(6,*) 'SAT ID               : ', idsat
  write(6,*) '# of channels        : ', n_chan
  write(6,*) 'Time of the 1st file : ', datetime
  write(6,*) 'Interval of files(hr): ', tint
  write(6,*) 'Area(S,N,W,E)        : ', real( (/South, North, West, East/), 4 )
  write(6,*) '0:All 1:used -1:rjct -2:All w/o any qc : ', qc
  write(6,*) '0:All 1:night 2:day  : ', solar


! Check number of channels
  if (n_chan > max_n_chan) then
     write(6,*)'### ERROR: n_chan=',n_chan,' > max_n_chan=',max_n_chan
     stop 99
  endif
  
! Open file to GrADS station data file
  dsetname = trim(filename) // '.dat'
  open(lunstn,file=dsetname,form='unformatted')
  write(6,*)'Open unit lunstn=',lunstn,' to GrADS station data file ',trim(dsetname)
  


! Allocate channel list array
  allocate( nuchan(n_chan) )


! Set GrADS data id with sat id
  write(id,'(i8)') idsat


! Compute total minutes for the first diagnostic file
  call Convert_DateTime_TotalMin( ttlmin_assign0, datetime )
  

! Big loop over diagnostic files
  iobs_total = 0
  do ft_diag = ft_diag_top, ft_diag_end
     itime_terminator=0

!    Read header
     call read_radiag_header( ft_diag, npred_radiag, retrieval, header_fix, header_chan, data_name, iflag )
     if (iflag/=0) then
        write(6,*)'***ERROR*** problem reading header, iflag=',iflag
        stop 99
     endif


!    Check for satellite and sensor
!    if( header_fix%isat /= idsat )cycle
!    if( header_fix%isat /= mod(idsat,100) )cycle	!### TEMPORAL ###
    

!    Check number of channels    
     if( header_fix%nchan /= n_chan )then
        write(6,*) '### ERROR: UNEXPECTED NUMBER OF CHANNELS IS READ'
        write(6,*) 'THE NUMBER IS', header_fix%nchan
        stop 99
     endif

!    Check time
     time_work(1) =      header_fix%idate            / 1000000
     time_work(2) = mod( header_fix%idate, 1000000 ) /   10000
     time_work(3) = mod( header_fix%idate,   10000 ) /     100
     time_work(4) = mod( header_fix%idate,     100 ) 
     time_work(5) = 0
     call Convert_DateTime_TotalMin( ttlmin_file, time_work )

     ttlmin_assign = ttlmin_assign0 + (ft_diag - ft_diag_top) * tint * 60
     
     if( ttlmin_file /= ttlmin_assign )then
        write(6,*) '### ERROR: TIMES ARE INCONSISTENT'
        write(6,*) 'FILE    :', header_fix%idate
        write(6,*) 'ASSIGNED:', datetime, '+', (ft_diag - ft_diag_top) * tint, 'hours'
        stop 99
     endif
     

!    Extract channel list
     do n=1, n_chan
        nuchan(n) = real( header_chan(n)%nuchan, 4 )
     enddo
    
!    Set number of level (channel) and surface (fix) data
     nelem_lvl=header_fix%idiag
     nelem_sfc=ireal_radiag-2
     if (allocated(data_sfc)) deallocate(data_sfc)
     allocate(data_sfc(nelem_sfc))
     write(6,*)'Allocate rank-1 data_sfc as nelem_sfc=',nelem_sfc
     
     if (allocated(data_lvl)) deallocate(data_lvl)
     allocate(data_lvl(nelem_lvl,n_chan))
     write(6,*)'Allocate rank-2 data_lvl as nelem_lvl=',nelem_lvl,' by n_chan=',n_chan
     

!    Print header
     write(6,*)' '
     write(6,*) '*** HEADER OF DIAGNOSTIC FILE ***'
     write(6,*) 'FIX PART:', header_fix
     write(6,*) 'n_chan=',n_chan,' n_bcor=',n_bcor
     do n=1, n_chan
        write(6,*) 'CH', n, ':', header_chan(n)
     enddo
     

!    Loop over data in current diagnostic file
     write(6,*)' '
     write(6,*) '*** DATA OF DIAGNOSTIC FILE ***'

     do

!       Read data record
        call read_radiag_data( ft_diag, header_fix, retrieval, data_fix, data_chan, data_extra, iflag )
        if( iflag /= 0 )then
           write(6,*)'***ERROR***  problem reading data iflag=',iflag,' iexist=',iexist
           exit      ! EOF or error
        endif
        

!       Check area
        if( data_fix%lat > North .or. data_fix%lat < South )cycle
        
        call adjust_lon( data_fix%lon )
        
        if( West < East )then
           if( data_fix%lon < West .or.  East < data_fix%lon )cycle
        else if( West > East )then
           if( East < data_fix%lon .and. data_fix%lon < West )cycle
        endif
        

!       Check solar
        if( ( solar == 1 .and. data_fix%solzen_ang <  Criterion_SolarZenAng ) .or. &
             ( solar == 2 .and. data_fix%solzen_ang >= Criterion_SolarZenAng ) )then
           cycle
        endif


!     Initialize surface data to missing
        do i=1,nelem_sfc
           data_sfc(i)=GRADS_MISSING
        end do
        
!     Load surface data 
        rlat         = data_fix%lat
        rlon         = data_fix%lon
        rtim         = 0.
        data_sfc(1)  = data_fix%zsges
        data_sfc(2)  = data_fix%obstime
        data_sfc(3)  = data_fix%senscn_pos
        data_sfc(4)  = data_fix%satzen_ang
        data_sfc(5)  = data_fix%satazm_ang
        data_sfc(6)  = data_fix%solzen_ang
        data_sfc(7)  = data_fix%solazm_ang
        data_sfc(8)  = data_fix%sungln_ang
        data_sfc(9)  = data_fix%water_frac
        data_sfc(10) = data_fix%land_frac
        data_sfc(11) = data_fix%ice_frac
        data_sfc(12) = data_fix%snow_frac
        data_sfc(13) = data_fix%water_temp
        data_sfc(14) = data_fix%land_temp
        data_sfc(15) = data_fix%ice_temp
        data_sfc(16) = data_fix%snow_temp
        data_sfc(17) = data_fix%soil_temp
        data_sfc(18) = data_fix%soil_mois
        data_sfc(19) = data_fix%land_type
        data_sfc(20) = data_fix%veg_frac
        data_sfc(21) = data_fix%snow_depth
        data_sfc(22) = data_fix%sfc_wndspd
        data_sfc(23) = data_fix%qcdiag1
        data_sfc(24) = data_fix%qcdiag2
        
!     Initialize the channel (level) data
        do n=1,n_chan
           do i=1,nelem_lvl
              data_lvl(i,n) = GRADS_MISSING
           end do
        end do
        
!     Loop over channels
        iexist = 0
        do n = 1, n_chan
      
!          Check Tb
           if( qc /= -2 .and. ( abs(data_chan(n)%omgnbc) > 200. .or. &
                data_chan(n)%tbobs < 50. .or. data_chan(n)%tbobs > 500. ) )cycle

!          Check uss flag
           if( ( qc == -1 .and. data_chan(n)%errinv >= 1.e-6 ) .or. &
                ( qc ==  1 .and. data_chan(n)%errinv <  1.e-6 ) )cycle
           
!          There is data to be written
           iexist = 1
        
!          Load arrays with level (channel) dependent data
           data_lvl(1,n)  = data_chan(n)%tbobs
           data_lvl(2,n)  = data_chan(n)%omgbc
           data_lvl(3,n)  = data_chan(n)%omgnbc
           data_lvl(4,n)  = data_chan(n)%errinv
           data_lvl(5,n)  = data_chan(n)%qcmark
           data_lvl(6,n)  = data_chan(n)%emiss
           data_lvl(7,n)  = data_chan(n)%tlap
           if (header_fix%iversion < iversion_radiag) then
              data_lvl( 8,n)=data_chan(n)%bifix(1)
              data_lvl( 9,n)=data_chan(n)%bilap
              data_lvl(10,n)=data_chan(n)%bilap2
              data_lvl(11,n)=data_chan(n)%bicons
              data_lvl(12,n)=data_chan(n)%biang
              data_lvl(13,n)=data_chan(n)%biclw
              if (retrieval) data_lvl(13,n)=data_chan(n)%bisst
           else
              data_lvl(8,n)  = data_chan(n)%bicons
              data_lvl(9,n)  = data_chan(n)%biang
              data_lvl(10,n) = data_chan(n)%biclw
              data_lvl(11,n) = data_chan(n)%bilap2
              data_lvl(12,n) = data_chan(n)%bilap
              do i=1,header_fix%angord+1
                 data_lvl(12+i,n) = data_chan(n)%bifix(i)
              end do
              data_lvl(12+header_fix%angord+2,n)=data_chan(n)%bisst
           endif
           
!       End loop over channels
        enddo


!       Check if data should be written
        if( iexist == 0 )cycle
        
        iobs_total = iobs_total + 1

      
!       Write station data
        call write_station_data(lunstn,&
             id, &                         ! id
             rlat, &                       ! lat
             rlon, &                       ! lon
             rtim, &                       ! t
             n_chan, &                     ! nlev(# of chan)
             nelem_lvl, &
             nelem_sfc, &
             nuchan, &                     ! ch # assigned as hight
             data_lvl, &
             data_sfc )
        

!       Periodically print data
        if( mod(iobs_total,2000) == 0 )then    
           write(6,*) 'DATA #:', iobs_total
           write(6,*) 'FIX PART:', data_fix
           do n=1, n_chan, n_chan/2
              write(6,*) 'CH SEQ #:', n, ':', data_lvl(:,n)
           enddo
        endif
        

!    End loop over data records        
     enddo
     
    
!    Write a record to indicate the end of time group
     if (itime_terminator==0) then
        write(6,*)'Write GrADS station data terminator for time ft_diag=',ft_diag
        stid='time_end'
        rlat=0.0
        rlon=0.0
        rtim=0.0
        nlev=0
        nflag=0
        write(lunstn) stid,rlat,rlon,rtim,nlev,nflag
        itime_terminator=1
     endif

! End loop over times     
  enddo
    
    
! Close GrADS station file
  if (itime_terminator==0) then
     write(6,*)'Write GrADS station data terminator'
     stid='end'
     rlat=0.0
     rlon=0.0
     rtim=0.0
     nlev=0
     nflag=0
     write(lunstn) stid,rlat,rlon,rtim,nlev,nflag
  endif
  write(6,*)'Close GrADS station data file attached to lunstn=',lunstn
  close(lunstn)


! Create GrADS control file
  dsetname = trim(filename) // '.ctl'
  open(lunctl,file=dsetname,form='formatted')
  write(6,*)'Open unit lunctl=',lunctl,' to GrADS control file ',dsetname
  call write_station_ctl(lunctl,comment, &
       filename, &
       datetime, &
       tint, &
       ft_diag_end - ft_diag_top + 1, &
       n_chan, &
       nelem_lvl, &
       nelem_sfc, &
       data_name)
  close(lunctl)

  

! Check total number of data
  if( iobs_total <= 0 )then
     write(6,*) '### NO DATA ###'
     stop
  endif
  

! End of programk  !--- end of program
  write(6,*)' '
  write(6,*) '=== NORMAL END ==='
  
contains
    
  !-------------------------------------------------------------
  ! Adjust longitude between [0:360)
  !-------------------------------------------------------------

  subroutine adjust_lon( lon )
    
    real(4),intent(inout) :: lon
    
    if( lon < 0. )then
       lon = lon + 360.
    else if( lon >= 360. )then
       lon = lon - 360.
    endif
    
  end subroutine adjust_lon
  
  
end program diag2grads
