!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    oznmon_read_diag       read ozone diag file
!
! abstract:  This module contains code to process ozone
!            diagnostic files.  The module defines structures
!            to contain information from the ozone
!            diagnostic files and then provides two routines
!            to access contents of the file.  Both binary 
!            and NetCDF formats are supported.
!
!------------------------------------------------------------


module oznmon_read_diag

  !--- use ---!

  use kinds, only: r_single,r_double,i_kind
  use nc_diag_read_mod, only: nc_diag_read_init, nc_diag_read_close, & 
                           nc_diag_read_get_dim, nc_diag_read_get_global_attr, &
                           nc_diag_read_get_var_names, &
                           nc_diag_read_get_global_attr_names, &
                           nc_diag_read_get_var
 
  use ncdr_vars, only:     nc_diag_read_check_var


  !--- implicit ---!

  implicit none


  !--- public & private ---!

  private

  public :: diag_header_fix_list
  public :: diag_header_nlev_list
  public :: diag_data_fix_list
  public :: diag_data_nlev_list
  public :: diag_data_extra_list

  public :: open_ozndiag
  public :: close_ozndiag
  public :: read_ozndiag_header
  public :: read_ozndiag_data

  public :: set_netcdf_read


  !--- diagnostic file format - header ---!
  
  type diag_header_fix_list
    sequence
    character(len=20) :: isis           ! sat and sensor type
    character(len=10) :: id             ! sat type
    character(len=10) :: obstype	! observation type
    integer(i_kind)   :: jiter          ! outer loop counter (1 = ges, 3 = anl)
    integer(i_kind)   :: nlevs		! number of levels (layer amounts + total column) per obs
    integer(i_kind)   :: ianldate	! analysis date in YYYYMMDDHH 
    integer(i_kind)   :: iint		! mpi task number
    integer(i_kind)   :: ireal		! # of real elements in the fix part of a data record
    integer(i_kind)   :: iextra		! # of extra elements for each level
  end type diag_header_fix_list

  type diag_header_nlev_list
    sequence
    real(r_single) :: pob		! SBUV/2,omi and gome-2 obs pressure level
    real(r_single) :: grs		! gross error
    real(r_single) :: err		! observation error
    integer(i_kind):: iouse		! use flag
  end type diag_header_nlev_list

  !--- diagnostic file format - data ---!

  integer,parameter :: IREAL_RESERVE  = 3
  
  type diag_data_fix_list
    sequence
    real(r_single) :: lat            ! latitude (deg)
    real(r_single) :: lon            ! longitude (deg)
    real(r_single) :: obstime        ! observation time relative to analysis
  end type diag_data_fix_list
  integer(i_kind), parameter    :: fix_list_size = 3

  type diag_data_nlev_list
    sequence
    real(r_single) :: ozobs              ! ozone (obs)
    real(r_single) :: ozone_inv          ! obs-ges
    real(r_single) :: varinv             ! inverse obs error **2
    real(r_single) :: sza                ! solar zenith angle
    real(r_single) :: fovn               ! scan position (field of view)
    real(r_single) :: toqf               ! omi row anomaly index or MLS o3mr precision
  end type diag_data_nlev_list
  integer(i_kind), parameter    :: nlev_list_size = 6

  type diag_data_extra_list
  sequence
    real(r_single) :: extra              ! extra information
  end type diag_data_extra_list

  logical,save                  :: netcdf           = .false.
  integer(i_kind),save          :: num_global_attrs = 0
  integer(i_kind),save          :: attr_name_mlen   = 0
  integer(i_kind),save          :: num_vars         = 0
  integer(i_kind),save          :: var_name_mlen    = 0

  character(len=:),dimension(:), allocatable,save :: var_names,attr_names

  type ncdiag_status
     logical :: nc_read
     integer(i_kind) :: cur_ob_idx
     integer(i_kind) :: num_records
     type(diag_data_fix_list), allocatable   :: all_data_fix(:)
     type(diag_data_nlev_list), allocatable  :: all_data_nlev(:,:) 
     type(diag_data_extra_list), allocatable :: all_data_extra(:,:,:)
  end type ncdiag_status

  integer(i_kind), parameter                            :: MAX_OPEN_NCDIAG = 2
  integer(i_kind), save                                 :: nopen_ncdiag = 0
  integer(i_kind), dimension(MAX_OPEN_NCDIAG), save     :: ncdiag_open_id = (/-1, -1/)
  type(ncdiag_status), dimension(MAX_OPEN_NCDIAG), save :: ncdiag_open_status


 contains

  !------------------------------------------------------------
  ! subroutine open_ozndiag
  !------------------------------------------------------------
  subroutine open_ozndiag(filename, ftin, istatus)
     character*500,   intent(in) :: filename

     !---------------------------------------------------------------------------- 
     !  Note:  This use of ftin here as inout is pretty sloppy.  Internally this
     !  module should translate ftin (file id from time.f90) into the proper index
     !  for use w/in this module.  Encapsulation is cleaner than modification of 
     !  external variables.  I got this directly from the src/gsi read_diag.f90
     !  but that doesn't make it right.  I'll clean this up when all is working.
     !
     integer(i_kind), intent(inout) :: ftin             
     integer(i_kind), intent(out)   :: istatus
     integer(i_kind)                :: i  !,ncd_nobs


     istatus = -999


     if (netcdf) then

        if (nopen_ncdiag >= MAX_OPEN_NCDIAG) then
           write(6,*) 'OPEN_RADIAG:  ***ERROR*** Cannot open more than ', &
                    MAX_OPEN_NCDIAG, ' netcdf diag files.'
           istatus = -1
        endif

        if ( istatus /= 0 ) then
           call nc_diag_read_init(filename,ftin)
           istatus=0

           do i = 1, MAX_OPEN_NCDIAG

              if (ncdiag_open_id(i) < 0) then
                 
                 ncdiag_open_id(i) = ftin
                 ncdiag_open_status(i)%nc_read = .false.
                 ncdiag_open_status(i)%cur_ob_idx = 1 

                 if (allocated(ncdiag_open_status(i)%all_data_fix)) then
                    deallocate(ncdiag_open_status(i)%all_data_fix)
                 endif
                 if (allocated(ncdiag_open_status(i)%all_data_nlev)) then
                    deallocate(ncdiag_open_status(i)%all_data_nlev)
                 endif
                 if (allocated(ncdiag_open_status(i)%all_data_extra)) then
                    deallocate(ncdiag_open_status(i)%all_data_extra)
                 endif

                 ncdiag_open_status(i)%num_records = nc_diag_read_get_dim(ftin,'nobs')
                 nopen_ncdiag = nopen_ncdiag + 1

                 write(6,*) ''
                 write(6,*) 'ncdiag_open_status(i) dump, i     = ', i
                 write(6,*) '                      %nc_read    = ', ncdiag_open_status(i)%nc_read
                 write(6,*) '                      %cur_ob_idx = ', ncdiag_open_status(i)%cur_ob_idx
                 write(6,*) '                      %num_records= ', ncdiag_open_status(i)%num_records
                 write(6,*) 'nopen_ncdiag = ', nopen_ncdiag
                 write(6,*) 'ncdiag_open_id(i) = ', ncdiag_open_id(i)
                 write(6,*) ''
                 exit

              endif

           enddo
        endif

        call load_file_vars_nc( ftin )

     else
       open(ftin,form="unformatted",file=filename,iostat=istatus)
       rewind(ftin)
     endif

  end subroutine open_ozndiag


  !------------------------------------------------------------
  ! subroutine close_ozndiag
  !------------------------------------------------------------
  subroutine close_ozndiag(filename, ftin)
     character*500,   intent(in) :: filename
     integer(i_kind), intent(inout) :: ftin

     integer(i_kind) :: id

     if (netcdf) then
        id = find_ncdiag_id(ftin)
        if (id < 0) then
           write(6,*) 'CLOSE_RADIAG:  ***ERROR*** ncdiag file ', filename,   &
                      ' was not opened'
        endif
        call nc_diag_read_close(filename)
        ncdiag_open_id(id) = -1
        ncdiag_open_status(id)%nc_read = .false.
        ncdiag_open_status(id)%cur_ob_idx = -9999
        ncdiag_open_status(id)%num_records = -9999
        if (allocated(ncdiag_open_status(id)%all_data_fix)) then
           deallocate(ncdiag_open_status(id)%all_data_fix)
        endif
        if (allocated(ncdiag_open_status(id)%all_data_nlev)) then
           deallocate(ncdiag_open_status(id)%all_data_nlev)
        endif
        if (allocated(ncdiag_open_status(id)%all_data_extra)) then
           deallocate(ncdiag_open_status(id)%all_data_extra)
        endif
        nopen_ncdiag = nopen_ncdiag - 1

        num_global_attrs = 0
        attr_name_mlen   = 0
        attr_names       = ''

        num_vars         = 0
        var_name_mlen    = 0
        var_names        = ''

     else
        close(ftin)
     endif

  end subroutine close_ozndiag


  !------------------------------------------------------------
  ! subroutine set_netcdf_read
  !
  ! set the use_netcdf flag to read either binary (default) or
  !    netcdf formatted diagnostic files.
  !------------------------------------------------------------
  subroutine set_netcdf_read( use_netcdf )
     logical,intent(in)                     :: use_netcdf


     netcdf = use_netcdf

  end subroutine set_netcdf_read


  !------------------------------------------------------------
  !  read_ozndiag_header
  !
  !  Read a header record of a diagnostic file in either 
  !  NetCDF for binary format.
  !------------------------------------------------------------
  subroutine read_ozndiag_header( ftin, header_fix, header_nlev, new_hdr, istatus )

    !--- interface

    integer                    ,intent(in)  :: ftin
    type(diag_header_fix_list ),intent(out) :: header_fix
    type(diag_header_nlev_list),pointer     :: header_nlev(:)
    logical                                 :: new_hdr
    integer(i_kind),intent(out)             :: istatus

  
    istatus = 0
 
    if ( netcdf ) then
       call read_ozndiag_header_nc( ftin, header_fix, header_nlev, new_hdr, istatus )
    else
       call read_ozndiag_header_bin( ftin, header_fix, header_nlev, new_hdr, istatus )
    endif

    write(6,*) 'ftin                = ',        ftin
    write(6,*) 'header_fix%isis     = ',        header_fix%isis
    write(6,*) 'header_fix%id       = ',        header_fix%id
    write(6,*) 'header_fix%obstype  = ',        header_fix%obstype
    write(6,*) 'header_fix%jiter    = ',        header_fix%jiter  
    write(6,*) 'header_fix%nlevs    = ',        header_fix%nlevs  
    write(6,*) 'header_fix%ianldate = ',        header_fix%ianldate
    write(6,*) 'header_fix%iint     = ',        header_fix%iint    
    write(6,*) 'header_fix%ireal    = ',        header_fix%ireal    
    write(6,*) 'header_fix%iextra   = ',        header_fix%iextra   

    write(6,*) 'istatus             = ',        istatus
    write(6,*) ''
 
  end subroutine read_ozndiag_header



  !------------------------------------------------------------
  !  subroutine read_ozndiag_header_nc
  !------------------------------------------------------------
  subroutine read_ozndiag_header_nc( ftin, header_fix, header_nlev, new_hdr, istatus )

    !--- interface

    integer                    ,intent(in)  :: ftin
    type(diag_header_fix_list ),intent(out) :: header_fix
    type(diag_header_nlev_list),pointer     :: header_nlev(:)
    logical                                 :: new_hdr
    integer(i_kind),intent(out)             :: istatus

    !--- variables
    
    integer,save :: nlevs_last = -1

    character(len=10):: sat,obstype
    character(len=20):: isis
    integer(i_kind):: jiter,nlevs
    integer(i_kind),dimension(:),allocatable :: iouse
    real(r_double),dimension(:),allocatable  :: pobs,gross,tnoise
   
    integer(i_kind)                          :: nsdim,k,idate,idx
    integer(i_kind),dimension(:),allocatable :: iuse_flag

 
    istatus = 0

    !--- get global attr
    !
    !    This may look like overkill with a check on each variable
    !    name, but a failure on these nc library read operations 
    !    is fatal.  Thus, this abundance of caution verifying the 
    !    variable exists before attempting to retreive it.
    !
    if( verify_var_name_nc( "date_time" ) ) then
       call nc_diag_read_get_global_attr(ftin, "date_time", idate)    
    else
       write(6,*) 'WARNING:  unable to read global var data_time from file '
    end if

    if( verify_var_name_nc( "Satellite_Sensor" ) ) then
       call nc_diag_read_get_global_attr(ftin, "Satellite_Sensor", isis)      
    else
       write(6,*) 'WARNING:  unable to read global var Satellite_Sensor from file '
    end if

    if( verify_var_name_nc( "Satellite" ) ) then
       call nc_diag_read_get_global_attr(ftin, "Satellite", sat) 
    else
       write(6,*) 'WARNING:  unable to read global var Satellite from file '
    end if

    if( verify_var_name_nc( "Observation_type" ) ) then
       call nc_diag_read_get_global_attr(ftin, "Observation_type", obstype)   ;
    else
       write(6,*) 'WARNING:  unable to read global var Observation_type from file '
    end if

    if( verify_var_name_nc( "pobs" ) ) then
       call nc_diag_read_get_global_attr(ftin, "pobs", pobs )
    else
       write(6,*) 'WARNING:  unable to read global var pobs from file '
    end if

    if( verify_var_name_nc( "gross" ) ) then
       call nc_diag_read_get_global_attr(ftin, "gross", gross )
    else
       write(6,*) 'WARNING:  unable to read global var gross from file '
    end if

    if( verify_var_name_nc( "tnoise" ) ) then
       call nc_diag_read_get_global_attr(ftin, "tnoise", tnoise )
    else
       write(6,*) 'WARNING:  unable to read global var tnoise from file '
    end if

    !-------------------------------------------------------------------
    !  The Anaysis_Use_Flag in the netcdf file resides in the 
    !  obs data rather than global (equivalent of binary file header 
    !  location. Assign the first nlevs number of those values to
    !  the iuse_flag array.
    !
    idx = find_ncdiag_id(ftin)

    if( verify_var_name_nc( "Analysis_Use_Flag" ) ) then
       if( ncdiag_open_status(idx)%num_records > 0 ) then 
          allocate( iuse_flag( ncdiag_open_status(idx)%num_records ))

          call nc_diag_read_get_var( ftin, 'Analysis_Use_Flag', iuse_flag )

       else
          do k=1,ncdiag_open_status(idx)%num_records
             iuse_flag(k) = -1
          end do

       end if 
    else
       write(6,*) 'WARNING:  unable to read global var Analysis_Use_Flag from file '
    end if

    nlevs = SIZE( pobs )

    header_fix%isis      = isis
    header_fix%id        = sat
    header_fix%obstype   = obstype
!    header_fix%jiter     = jiter   ! This is not in the NetCDF file.  It's not
				    ! used by the OznMon so, fortunately, no loss.
    header_fix%nlevs     = nlevs
    header_fix%ianldate  = idate

    !--- allocate if necessary

    write(6,*) 'header_fix%nlevs, nlevs_last = ', header_fix%nlevs, nlevs_last

    if( header_fix%nlevs /= nlevs_last )then
      if( nlevs_last > 0 )then
        deallocate( header_nlev )
      endif
      allocate( header_nlev( header_fix%nlevs ) )
      nlevs_last = header_fix%nlevs
    endif

    !--- read header (level part)
    
    do k=1,header_fix%nlevs
       header_nlev(k)%pob = pobs(k)
       header_nlev(k)%grs = gross(k)
       header_nlev(k)%err = tnoise(k)
       header_nlev(k)%iouse = iuse_flag(k)
    end do

    if( allocated( pobs )) deallocate( pobs )
    if( allocated( gross )) deallocate( gross )
    if( allocated( tnoise )) deallocate( tnoise )
    if( allocated( iuse_flag )) deallocate( iuse_flag )

  end subroutine read_ozndiag_header_nc



  !------------------------------------------------------------
  !  subroutine read_ozndiag_header_bin
  !------------------------------------------------------------
  subroutine read_ozndiag_header_bin( ftin, header_fix, header_nlev, new_hdr, istatus )

    !--- interface

    integer                    ,intent(in)  :: ftin
    type(diag_header_fix_list ),intent(out) :: header_fix
    type(diag_header_nlev_list),pointer     :: header_nlev(:)
    logical                                 :: new_hdr
    integer(i_kind),intent(out)             :: istatus


    !--- variables
    
    integer,save :: nlevs_last = -1
    integer :: ilev,k,ioff0
    character(len=10):: id,obstype
    character(len=20):: isis
    integer(i_kind):: jiter,nlevs,ianldate,iint,ireal,iextra
    integer(i_kind),dimension(:),allocatable:: iouse
    real(r_single),dimension(:),allocatable:: pob,grs,err
    
    istatus = 0

    !--- read header (fix part)
    !--- the new header format contains one additional integer value 
    !
    if ( new_hdr ) then
       read(ftin) isis,id,obstype,jiter,nlevs,ianldate,iint,ireal,iextra,ioff0
    else
       read(ftin) isis,id,obstype,jiter,nlevs,ianldate,iint,ireal,iextra
    endif

    header_fix%isis      = isis
    header_fix%id        = id
    header_fix%obstype   = obstype
    header_fix%jiter     = jiter
    header_fix%nlevs     = nlevs
    header_fix%ianldate  = ianldate
    header_fix%iint      = iint
    header_fix%ireal     = ireal
    header_fix%iextra    = iextra

    !--- check header
    
    if( header_fix%ireal  /= IREAL_RESERVE  ) then

      write(6,*) '### ERROR: UNEXPECTED DATA RECORD FORMAT'
      write(6,*) 'ireal  =', header_fix%ireal  
      stop 99

    endif

    if (header_fix%iextra /= 0) then
       write(6,*)'READ_DIAG_HEADER:  extra diagnostic information available, ',&
            'iextra=',header_fix%iextra
    endif

    !--- allocate if necessary

    write(6,*) 'header_fix%nlevs, nlevs_last = ', header_fix%nlevs, nlevs_last
    if( header_fix%nlevs /= nlevs_last )then
      if( nlevs_last > 0 )then
        deallocate( header_nlev )
      endif
      allocate( header_nlev( header_fix%nlevs ) )
      nlevs_last = header_fix%nlevs
    endif

    !--- read header (level part)
    
    allocate (pob(header_fix%nlevs))
    allocate (grs(header_fix%nlevs))
    allocate (err(header_fix%nlevs))
    allocate (iouse(header_fix%nlevs))
    read(ftin)  pob,grs,err,iouse
    do k=1,header_fix%nlevs
       header_nlev(k)%pob = pob(k)
       header_nlev(k)%grs = grs(k)
       header_nlev(k)%err = err(k)
       header_nlev(k)%iouse = iouse(k)
    end do

    if( allocated( pob )) deallocate( pob )
    if( allocated( grs )) deallocate( grs )
    if( allocated( err )) deallocate( err )
    if( allocated( iouse )) deallocate( iouse )

  end subroutine read_ozndiag_header_bin



  !------------------------------------------------------------
  !  subroutine read_ozndiag_data
  !
  !  Read a data record of the diagnostic file in either
  !  NetCDF or binary format.
  !------------------------------------------------------------

  subroutine read_ozndiag_data( ftin, header_fix, data_fix, data_nlev, data_extra, ntobs, iflag )
  
    !--- interface

    integer                    ,intent(in)  :: ftin
    type(diag_header_fix_list ),intent(in)  :: header_fix

    !--- NOTE:  These pointers are used to build an array numbering
    !           iobs.  They should be allocated every time this
    !           routine is called and should not be deallocated
    !           here.  
    type(diag_data_fix_list),   pointer     :: data_fix(:)
    type(diag_data_nlev_list)  ,pointer     :: data_nlev(:,:)
    type(diag_data_extra_list) ,pointer     :: data_extra(:,:)
    integer                    ,intent(out) :: iflag
    integer(i_kind)            ,intent(out) :: ntobs
   

    if ( netcdf ) then
       call read_ozndiag_data_nc( ftin, header_fix, data_fix, data_nlev, data_extra, ntobs, iflag )
    else
       call read_ozndiag_data_bin( ftin, header_fix, data_fix, data_nlev, data_extra, ntobs, iflag )
    end if 

  end subroutine read_ozndiag_data



  !------------------------------------------------  
  !  subroutine read_ozndiag_data_nc
  !------------------------------------------------  
  subroutine read_ozndiag_data_nc( ftin, header_fix, data_fix, data_nlev, data_extra, ntobs, iflag )

    !--- interface

    integer                    ,intent(in)  :: ftin
    type(diag_header_fix_list ),intent(in)  :: header_fix

    !--- NOTE:  These pointers are used to build an array numbering
    !           iobs.  They should be allocated every time this
    !           routine is called and should not be deallocated
    !           here.  
    !
    type(diag_data_fix_list),   pointer     :: data_fix(:)
    type(diag_data_nlev_list)  ,pointer     :: data_nlev(:,:)
    type(diag_data_extra_list) ,pointer     :: data_extra(:,:)

    integer                    ,intent(out) :: iflag
    integer(i_kind)            ,intent(out) :: ntobs
    integer(i_kind)                         :: id,ii,jj,cur_idx
    integer(i_kind),allocatable             :: Use_Flag(:)
    integer(i_kind)                         :: nlevs             ! number of levels
    integer(i_kind)                         :: nrecords          ! number of file records, which 
                                                                 ! is number of levels * number of obs


    real(r_single),allocatable              :: lat(:)            ! latitude (deg)
    real(r_single),allocatable              :: lon(:)            ! longitude (deg)
    real(r_single),allocatable              :: obstime(:)        ! observation time relative to analysis

    real(r_single),allocatable              :: ozobs(:)          ! observation
    real(r_single),allocatable              :: ozone_inv(:)      ! obs-forecast adjusted
    real(r_single),allocatable              :: varinv(:)         ! inverse obs error
    real(r_single),allocatable              :: sza(:)            ! solar zenith angle   
    real(r_single),allocatable              :: fovn(:)           ! scan position (fielf of view)
    real(r_single),allocatable              :: toqf(:)           ! row anomaly index
   
    logical                                 :: test

    cur_idx = ncdiag_open_id( nopen_ncdiag )
  
    !----------------------------------------------------------
    !  The binary file read (the original version of the file
    !  read) is designed to be called in a loop, as it reads
    !  each obs from the file.  
    !
    !  The newer netcdf read processes each field for all obs 
    !  so it can grab all the obs data in a single call.  The
    !  calling routine uses the iflag value to process the,
    !  and non-zero value to indicate everything has been read.  
    !  In order to use this same iflag mechanism we'll use the 
    !  ncdiag_open_status%nc_read field, setting it to true and 
    !  iflag to 0 after reading, and if nc_read is already true 
    !  then set iflag to -1.
    !
    !  It's not as clear or clean as it should be, so I'll 
    !  leave this comment in as a note-to-self to redesign this
    !  when able.
  
    nlevs = header_fix%nlevs 

    if( ncdiag_open_status(cur_idx)%nc_read .eqv. .true. ) then 
       iflag = -1  
    else  
       iflag = 0
       ntobs = 0

       id = find_ncdiag_id(ftin)
       nrecords = ncdiag_open_status(id)%num_records
       if( header_fix%nlevs > 1 ) then
          ntobs = nrecords / header_fix%nlevs
       else
          ntobs = nrecords
       end if

       write(6,*) 'header_fix%nlevs, ncdiag_open_status(id)%num_records, ntobs = ', header_fix%nlevs, ncdiag_open_status(id)%num_records, ntobs

       !------------------------------------
       !  allocate the returned structures
       !
       allocate( data_fix( ntobs ) )
       allocate( data_nlev( header_fix%nlevs,ntobs ) )

      
       !---------------------------------
       ! load data_fix structure
       !
       allocate( lat(nrecords) ) 
       allocate( lon(nrecords) )
       allocate( obstime(nrecords) )

       !--- get obs data
       !
       !    This may look like overkill with a check on each variable
       !    name, but due to the genius of the ncdiag library, a
       !    failure on these read operations is fatal, because, reasons
       !    I guess.  Thus, this abundance of caution.
       !

       if( verify_var_name_nc( "Latitude" ) ) then
          call nc_diag_read_get_var( ftin, 'Latitude', lat )
       else
          write(6,*) 'WARNING:  unable to read global var Latitude from file '
       end if

       if( verify_var_name_nc( "Longitude" ) ) then
          call nc_diag_read_get_var( ftin, 'Longitude', lon )
       else
          write(6,*) 'WARNING:  unable to read global var Longitude from file '
       end if

       if( verify_var_name_nc( "Time" ) ) then
          call nc_diag_read_get_var( ftin, 'Time', obstime )
       else
          write(6,*) 'WARNING:  unable to read global var Time from file '
       end if

       !-------------------------------------------------------
       ! lat, lon, obstime are dimensioned to nrecords
       ! read those as nobs * nlevs with all lat/lon values 
       ! for a given obs being the same.  So a source with 22
       ! levels has that value repeated 22 times for each 
       ! obs.  The data_fix structure is dimentioned to nobs,
       ! not nrecords to match the binary file structure.
       !
       ! Note that only sbuv2_n19 and ompsnp_npp (at present)
       ! have nlevs > 1.
       !
       do ii=1,ntobs
          if( nlevs > 1 ) then
             data_fix(ii)%lat     =     lat(1 + ((ii-1)*nlevs) )
             data_fix(ii)%lon     =     lon(1 + ((ii-1)*nlevs) )
             data_fix(ii)%obstime = obstime(1 + ((ii-1)*nlevs) )
          else
             data_fix(ii)%lat     =     lat(ii)
             data_fix(ii)%lon     =     lon(ii)
             data_fix(ii)%obstime = obstime(ii)
          end if
          
       end do
 
       if( allocated( lat     )) deallocate( lat )
       if( allocated( lon     )) deallocate( lon )
       if( allocated( obstime )) deallocate( obstime )

       !---------------------------------
       ! load data_nlev structure
       !
       allocate( ozobs(nrecords) ) 

       allocate( ozone_inv(nrecords) ) 
       allocate( varinv(nrecords) ) 
       allocate( sza(nrecords) ) 
       allocate( fovn(nrecords) )
       allocate( toqf(nrecords) )

       if( verify_var_name_nc( "Observation" ) ) then
          call nc_diag_read_get_var( ftin, 'Observation', ozobs )
       else
          write(6,*) 'WARNING:  unable to read var Observation from file '
       end if

       if( verify_var_name_nc( "Obs_Minus_Forecast_adjusted" ) ) then
          call nc_diag_read_get_var( ftin, 'Obs_Minus_Forecast_adjusted', ozone_inv )
       else
          write(6,*) 'WARNING:  unable to read var Obs_Minus_Forecast_adjusted from file '
       end if

       if( verify_var_name_nc( "Inverse_Observation_Error" ) ) then
          call nc_diag_read_get_var( ftin, 'Inverse_Observation_Error', varinv )
       else
          write(6,*) 'WARNING:  unable to read var Invers_Observation_Error from file '
       end if

       if( verify_var_name_nc( "Solar_Zenith_Angle" ) ) then
          call nc_diag_read_get_var( ftin, 'Solar_Zenith_Angle', sza )
       else
          write(6,*) 'WARNING:  unable to read var Solar_Zenith_Angle from file '
       end if

       if( verify_var_name_nc( "Scan_Position" ) ) then
          call nc_diag_read_get_var( ftin, 'Scan_Position', fovn )
       else
          write(6,*) 'WARNING:  unable to read var Scan_Position from file '
       end if

       if( verify_var_name_nc( "Row_Anomaly_Index" ) ) then
          call nc_diag_read_get_var( ftin, 'Row_Anomaly_Index', toqf )
       else
          write(6,*) 'WARNING:  unable to read var Row_Anomaly_Index from file '
       end if


       !------------------------------------------------
       ! All vars used to read the file are dimensioned
       ! to nrecord, which is nobs * nlevs
       !
       do jj=1,ntobs
          do ii=1,header_fix%nlevs
             data_nlev(ii,jj)%ozobs     =     ozobs( ii + ((jj-1) * nlevs) )
             data_nlev(ii,jj)%ozone_inv = ozone_inv( ii + ((jj-1) * nlevs) )
             data_nlev(ii,jj)%varinv    =    varinv( ii + ((jj-1) * nlevs) )
             data_nlev(ii,jj)%sza       =       sza( ii + ((jj-1) * nlevs) )
             data_nlev(ii,jj)%fovn      =      fovn( ii + ((jj-1) * nlevs) )
             data_nlev(ii,jj)%toqf      =      toqf( ii + ((jj-1) * nlevs) )
          end do
       end do

       if( allocated( ozobs     )) deallocate( ozobs )
       if( allocated( ozone_inv )) deallocate( ozone_inv )
       if( allocated( varinv    )) deallocate( varinv )
       if( allocated( sza       )) deallocate( sza )
       if( allocated( fovn      )) deallocate( fovn )
       if( allocated( toqf      )) deallocate( toqf )

       ncdiag_open_status(cur_idx)%nc_read = .true.

    end if

  end subroutine read_ozndiag_data_nc



  !------------------------------------------------  
  !  subroutine read_ozndiag_data_bin
  !------------------------------------------------  
  subroutine read_ozndiag_data_bin( ftin, header_fix, data_fix, data_nlev, data_extra, ntobs, iflag )
  
    !--- interface

    integer                    ,intent(in)  :: ftin
    type(diag_header_fix_list ),intent(in)  :: header_fix

    !--- NOTE:  These pointers are used to build an array numbering
    !           ntobs.  So they should be allocated every time this
    !           routine is called and should not be deallocated
    !           here.  

    type(diag_data_fix_list),   pointer     :: data_fix(:)
    type(diag_data_nlev_list)  ,pointer     :: data_nlev(:,:)
    type(diag_data_extra_list) ,pointer     :: data_extra(:,:)

    integer                    ,intent(out) :: iflag
    integer(i_kind)            ,intent(out) :: ntobs
    integer(i_kind)            ,pointer     :: data_mpi(:)
    
    !--- variables
    integer,save :: nlevs_last = -1
    integer,save :: iextra_last = -1
    integer :: iev,iobs,i,j
    real(r_single),allocatable,dimension(:,:)  :: tmp_fix
    real(r_single),allocatable,dimension(:,:,:):: tmp_nlev
    real(r_single),allocatable,dimension(:,:)  :: tmp_extra

    !--- allocate if necessary

    read(ftin,IOSTAT=iflag) ntobs
    write(6,*) ' READ 1, ntobs, iflag = ', ntobs, iflag

    write(6,*) 'header_fix%nlevs, nlevs_last = ', header_fix%nlevs, nlevs_last
    if( header_fix%nlevs /= nlevs_last )then
      if( nlevs_last > 0 )then
        write(6,*) ' DEALLOCATING data_nlev, data_fix, data_mpi'
        deallocate( data_nlev )
        deallocate( data_fix )
        deallocate( data_mpi )
      endif

      allocate( data_fix( ntobs ) )
      allocate( data_mpi( ntobs ) )
      allocate( data_nlev( header_fix%nlevs,ntobs ) )
      nlevs_last = header_fix%nlevs
    endif

    write(6,*) 'iextra_last = ', iextra_last
    if (iextra_last > 0) then
       deallocate ( data_extra )
    endif

    allocate( data_extra(header_fix%iextra,ntobs) )
    iextra_last = header_fix%iextra

    !--- read a record

    allocate( tmp_fix( fix_list_size, ntobs ))
    allocate( tmp_nlev( nlev_list_size, header_fix%nlevs,ntobs ))

    if (header_fix%iextra == 0) then
       read(ftin,IOSTAT=iflag) data_mpi, tmp_fix, tmp_nlev
    else
       allocate(  tmp_extra(header_fix%iextra,ntobs) )
       read(ftin,IOSTAT=iflag) data_mpi, tmp_fix, tmp_nlev, tmp_extra

       do j=1,ntobs
          do i=1,header_fix%iextra
             data_extra(i,j)%extra=tmp_extra(i,j)
          end do
       end do

       if( allocated( tmp_extra )) deallocate( tmp_extra )
    endif

    do j=1,ntobs
       data_fix(j)%lat     = tmp_fix(1,j)
       data_fix(j)%lon     = tmp_fix(2,j)
       data_fix(j)%obstime = tmp_fix(3,j)
    end do
    if( allocated( tmp_fix )) deallocate( tmp_fix )

    do j=1,ntobs
       do i=1,header_fix%nlevs
          data_nlev(i,j)%ozobs     = tmp_nlev(1,i,j)
          data_nlev(i,j)%ozone_inv = tmp_nlev(2,i,j)
          data_nlev(i,j)%varinv    = tmp_nlev(3,i,j)
          data_nlev(i,j)%sza       = tmp_nlev(4,i,j)
          data_nlev(i,j)%fovn      = tmp_nlev(5,i,j)
          data_nlev(i,j)%toqf      = tmp_nlev(6,i,j)
       end do
    end do
    if( allocated( tmp_nlev )) deallocate( tmp_nlev )

    nlevs_last = -1

  end subroutine read_ozndiag_data_bin


  !------------------------------------------------  
  !  function find_ncdiag_id
  !------------------------------------------------  
  integer( i_kind ) function find_ncdiag_id( ftin )

     integer(i_kind), intent(in) :: ftin

     integer(i_kind) :: i

     find_ncdiag_id = -1
     do i = 1, MAX_OPEN_NCDIAG
        if ( ncdiag_open_id(i) == ftin ) then
           find_ncdiag_id = i
           return
        endif
     enddo

     return
  end function find_ncdiag_id


  !------------------------------------------------  
  !  load_file_vars_nc
  !
  !  Query the netcdf file and load all the global
  !  and variable attribute names into memory.
  !------------------------------------------------  
  subroutine load_file_vars_nc( ftin )
     integer(i_kind), intent(in) :: ftin

   
     call nc_diag_read_get_global_attr_names(ftin, num_global_attrs, &
                attr_name_mlen, attr_names)

     call nc_diag_read_get_var_names(ftin, num_vars, var_name_mlen, var_names)


  end subroutine load_file_vars_nc


  !------------------------------------------------  
  !  function verify_var_name_nc
  !------------------------------------------------  
  logical function verify_var_name_nc( test_name )

     character(*),intent(in)       :: test_name
     integer(i_kind)               :: k
 
     verify_var_name_nc = .false.

     
     do k=1,num_global_attrs
        if( test_name == attr_names(k) ) then
           verify_var_name_nc = .true.
           exit
        end if 
     end do 

     if( verify_var_name_nc .eqv. .false. ) then
        do k=1,num_vars
           if( test_name == var_names(k) ) then
              verify_var_name_nc = .true.
              exit
           end if 
        end do 
     end if

  end function verify_var_name_nc


end module oznmon_read_diag

