!----------------------------------------------------------------------
!  conmon_read_diag
!
!     This subroutine reads the conventional data assimilation 
!     diagnostic files (contained in the cnvstat tar file) and returns
!     the requested obs that match the input type and subtype in a 
!     linked list.  The diagnostic files may be in either binary or 
!     NetCDF format.
!
!     Note:
!        There are problems/mismatches between the contents of binary
!        and NetCDF files for types gps and sst.
!----------------------------------------------------------------------

module conmon_read_diag

   !--- use ---!
   use generic_list
   use data
   use kinds, only:  i_kind,r_single,r_kind

   use nc_diag_read_mod, only: nc_diag_read_init, nc_diag_read_close, &
                           nc_diag_read_get_dim, nc_diag_read_get_global_attr, &
                           nc_diag_read_get_var_names, &
                           nc_diag_read_get_global_attr_names, &
                           nc_diag_read_get_var, nc_diag_read_get_dim_names

   use ncdr_vars, only:    nc_diag_read_check_var
   use ncdr_dims, only:    nc_diag_read_check_dim


   !--- implicit ---!
   implicit none
 

   !--- public & private ---!
   private
  
   public :: set_netcdf_read 
   public :: conmon_read_diag_file
   public :: conmon_return_all_obs

   interface load_nc_var
     module procedure load_nc_var_int, load_nc_var_real, load_nc_var_char
   end interface


   !--- common data structures ---!
   logical,save                                       :: netcdf          = .false.

   integer(i_kind), parameter                         :: MAX_OPEN_NCDIAG = 2
   integer(i_kind), save                              :: nopen_ncdiag    = 0
   integer(i_kind), dimension(MAX_OPEN_NCDIAG), save  :: ncdiag_open_id  = (/-1, -1/)

   type ncdiag_status
      logical :: nc_read
      integer(i_kind) :: cur_ob_idx
      integer(i_kind) :: num_records
   end type ncdiag_status

   type(ncdiag_status), dimension(MAX_OPEN_NCDIAG), save :: ncdiag_open_status



   contains


   subroutine load_nc_var_int( var_name, ftin, var_storage, ierr )
      character(len=*), intent(in)                    :: var_name
      integer, intent(in)                             :: ftin
      integer, dimension(:), allocatable, intent(out) :: var_storage
      integer, intent(out)                            :: ierr 

      if( nc_diag_read_check_var( var_name )) then
         call nc_diag_read_get_var( ftin, var_name, var_storage )
         ierr = 0
      else
         print *, 'WARNING:  unable to read ', trim( var_name )
         ierr = 1
      end if
   end subroutine


   subroutine load_nc_var_real( var_name, ftin, var_storage, ierr )
      character(len=*), intent(in)                           :: var_name
      integer, intent(in)                                    :: ftin
      real(r_single), dimension(:), allocatable, intent(out) :: var_storage
      integer, intent(out)                                   :: ierr

      if( nc_diag_read_check_var( var_name )) then
         call nc_diag_read_get_var( ftin, var_name, var_storage )
         ierr=0
      else
         print *, 'WARNING:  unable to read ', trim( var_name )
         ierr=1
      end if
   end subroutine


   subroutine load_nc_var_char( var_name, ftin, var_storage, ierr )
      character(len=*), intent(in)                             :: var_name
      integer, intent(in)                                      :: ftin
      character(len=:), dimension(:), allocatable, intent(out) :: var_storage
      integer, intent(out)                                     :: ierr

      if( nc_diag_read_check_var( var_name )) then
         call nc_diag_read_get_var( ftin, var_name, var_storage )
         ierr=0
      else
         print *, 'WARNING:  unable to read ', trim( var_name )
         ierr=1
      end if
   end subroutine



   !------------------------------------------------------------
   ! set_netcdf_read
   !
   ! set the use_netcdf flag to signal binary (default) or
   !    netcdf formatted diagnostic files.
   !------------------------------------------------------------
   subroutine set_netcdf_read( use_netcdf )
      logical,intent(in)                     :: use_netcdf

      netcdf = use_netcdf

   end subroutine set_netcdf_read



   !---------------------------------------------------------------
   ! conmon_read_diag_file
   !
   ! Public routine to read a conventional diagnostic file
   ! 
   ! Obs in the input_file are returned in the list according 
   ! to these rules:
   !
   !    1.  All obs matching intype and in_subtype are returned.
   !
   !    2.  If there are no subtype values in the input_file then
   !        all obs matching intype are returned.
   !
   !---------------------------------------------------------------
   subroutine conmon_read_diag_file( input_file, ctype, intype, expected_nreal, nobs, in_subtype, list )

      !--- interface 
      character(100), intent(in) :: input_file
      character(3), intent(in)   :: ctype

      !--- note expected_nreal has no meaning for netcdf files
      integer, intent(in)        :: intype, expected_nreal, in_subtype
      integer, intent(out)       :: nobs
      type(list_node_t), pointer :: list
      logical                    :: return_all = .false.

      write(6,*)'--> conmon_read_diag_file'

      if ( netcdf ) then
         write(6,*) ' call nc read subroutine'
         call read_diag_file_nc( input_file, return_all, ctype, intype, expected_nreal, nobs, in_subtype, list )
      else
         call read_diag_file_bin( input_file,return_all, ctype, intype, expected_nreal,nobs,in_subtype, list )
      end if

      write(6,*)"<-- conmon_read_diag_file"

   end subroutine conmon_read_diag_file



   !------------------------------------------------------------------
   !  conmon_return_all_obs
   !
   !  Retrieve all obs from a given conventional diagnostic file.
   !  Note that the NetCDF formatted diag files always contain only
   !  one conventional data type (gps, ps, q, sst, t, uv), while 
   !  binary formatted diag files contain all types for a single 
   !  ges or anl run.
   !
   !  Note:  the ctype parameter only has meaning for NetCDF 
   !  formatted diag files. 
   !------------------------------------------------------------------
   subroutine conmon_return_all_obs( input_file, ctype, nobs, list )

      !--- interface 
      character(100), intent(in) :: input_file
      character(3), intent(in)   :: ctype
      integer, intent(out)       :: nobs
      type(list_node_t), pointer :: list

      logical                    :: return_all = .true.
      integer                    :: intype, expected_nreal, in_subtype

      write(6,*)'--> conmon_return_all_obs'
      nobs = 0

      !
      ! Q:  Can I re-use the read_diag_* to do this read,
      !     adding extra param(s) to switch on _all_ obs?
      !     Those routines are private to this module so
      !     that wouldn't pose a complexity problem for
      !     accessing the routines.
      !
      if ( netcdf ) then
         write(6,*) ' call nc retrieve all routine'
         call read_diag_file_nc( input_file, return_all, ctype, intype, expected_nreal, nobs, in_subtype, list )
      else
         write(6,*) ' call bin retrieve all routine'
         call read_diag_file_bin( input_file,return_all, ctype, intype, expected_nreal,nobs,in_subtype, list )
      end if

      write(6,*)'<-- conmon_return_all_obs'

   end subroutine conmon_return_all_obs


   !-------------------------------
   !  read_diag_file_nc
   !
   !  NetCDF read routine
   !-------------------------------
   subroutine read_diag_file_nc( input_file, return_all, ctype, intype, expected_nreal, nobs, in_subtype, list )

      !--- interface 
      character(100), intent(in) :: input_file
      logical, intent(in)        :: return_all
      character(3), intent(in)   :: ctype
      integer, intent(in)        :: intype, expected_nreal, in_subtype
      integer, intent(out)       :: nobs
      type(list_node_t), pointer :: list

      !--- local vars
      type(list_node_t), pointer :: next => null()
      type(data_ptr)             :: ptr
      integer                    :: ii, ierr, istatus, ftin, total_obs, id, idx

      data ftin / 11 /


      print *, ' ' 
      print *, '   --> read_diag_file_nc'
      print *, '         ctype, intype, in_subtype = ', ctype, intype, in_subtype
      nobs = 0
      ierr = 0

      if (nopen_ncdiag >= MAX_OPEN_NCDIAG) then
         write(6,*) 'OPEN_RADIAG:  ***ERROR*** Cannot open more than ', &
                    MAX_OPEN_NCDIAG, ' netcdf diag files.'
      endif

      call nc_diag_read_init( input_file, ftin )
      istatus=0

      do ii = 1, MAX_OPEN_NCDIAG

         if( ncdiag_open_id(ii) < 0 ) then
            ncdiag_open_id(ii) = ftin
            ncdiag_open_status(ii)%nc_read = .false.

            ncdiag_open_status(ii)%cur_ob_idx = -9999
            ncdiag_open_status(ii)%num_records = -9999
            nopen_ncdiag = nopen_ncdiag + 1
            exit
         endif

      enddo

      select case ( trim( adjustl( ctype ) ) )
   
         case ( 'gps' ) 
            call read_diag_file_gps_nc( input_file, return_all, ftin, ctype, intype, expected_nreal, nobs, in_subtype, list )

         case ( 'ps' ) 
            call read_diag_file_ps_nc(  input_file, return_all, ftin, ctype, intype, expected_nreal, nobs, in_subtype, list )

         case ( 'q' ) 
            call read_diag_file_q_nc(   input_file, return_all, ftin, ctype, intype, expected_nreal, nobs, in_subtype, list )

         case ( 'sst' )
            call read_diag_file_sst_nc( input_file, return_all, ftin, ctype, intype, expected_nreal, nobs, in_subtype, list )

         case ( 't' ) 
            call read_diag_file_t_nc(   input_file, return_all, ftin, ctype, intype, expected_nreal, nobs, in_subtype, list )

         case ( 'uv' ) 
            call read_diag_file_uv_nc(  input_file, return_all, ftin, ctype, intype, expected_nreal, nobs, in_subtype, list )

         case default
            print *, 'ERROR:  unmatched ctype :', ctype

      end select


      id = find_ncdiag_id(ftin)
      if (id < 0) then
           write(6,*) 'CLOSE_RADIAG:  ***ERROR*** ncdiag file ', input_file,   &
                      ' was not opened'
      endif

      call nc_diag_read_close( input_file )
      ncdiag_open_id(id) = -1

      ncdiag_open_status(id)%nc_read = .false.
      ncdiag_open_status(id)%cur_ob_idx = -9999
      ncdiag_open_status(id)%num_records = -9999

      nopen_ncdiag = nopen_ncdiag - 1

      print *, ' ' 
      print *, '   <-- read_diag_file_nc, ierr = ', ierr


   end subroutine read_diag_file_nc
 

   !--------------------------------------------------------- 
   !  netcdf read routine for ps data types in netcdf files
   !
   subroutine read_diag_file_ps_nc( input_file, return_all, ftin, ctype, intype,expected_nreal,nobs,in_subtype, list )
  
      !--- interface 
      character(100), intent(in) :: input_file
      logical, intent(in)        :: return_all
      integer, intent(in)        :: ftin
      character(3), intent(in)   :: ctype
      integer, intent(in)        :: intype, expected_nreal, in_subtype
      integer, intent(out)       :: nobs
      type(list_node_t), pointer :: list

      !--- local vars
      type(list_node_t), pointer :: next => null()
      type(data_ptr)             :: ptr
      integer                    :: ii, ierr, istatus, total_obs, idx
      logical                    :: have_subtype = .true.
      logical                    :: add_obs

      !--- NetCDF file components                                                               dimension(s) 
      !
      character(len=:), dimension(:), allocatable  :: Station_ID                      !  (nobs, Station_ID_maxstrlen)
      character(len=:), dimension(:), allocatable  :: Observation_Class               !  (nobs, Station_Class_maxstrlen)
      integer, dimension(:), allocatable           :: Observation_Type                !  (obs)
      integer, dimension(:), allocatable           :: Observation_Subtype             !  (obs)
      real(r_single), dimension(:), allocatable    :: Latitude                        !  (obs)
      real(r_single), dimension(:), allocatable    :: Longitude                       !  (obs)
      real(r_single), dimension(:), allocatable    :: Station_Elevation               !  (obs)
      real(r_single), dimension(:), allocatable    :: Pressure                        !  (obs)
      real(r_single), dimension(:), allocatable    :: Height                          !  (obs)
      real(r_single), dimension(:), allocatable    :: Time                            !  (obs)
      real(r_single), dimension(:), allocatable    :: Prep_QC_Mark                    !  (obs)
      real(r_single), dimension(:), allocatable    :: Prep_Use_Flag                   !  (obs)
      real(r_single), dimension(:), allocatable    :: Nonlinear_QC_Var_Jb             !  (obs)
      real(r_single), dimension(:), allocatable    :: Nonlinear_QC_Rel_Wgt            !  (obs)
      real(r_single), dimension(:), allocatable    :: Analysis_Use_Flag               !  (obs)
      real(r_single), dimension(:), allocatable    :: Errinv_Input                    !  (obs)
      real(r_single), dimension(:), allocatable    :: Errinv_Adjust                   !  (obs)
      real(r_single), dimension(:), allocatable    :: Errinv_Final                    !  (obs)
      real(r_single), dimension(:), allocatable    :: Observation                     !  (obs)
      real(r_single), dimension(:), allocatable    :: Obs_Minus_Forecast_adjusted     !  (obs)
      real(r_single), dimension(:), allocatable    :: Obs_Minus_Forecast_unadjusted   !  (obs)
      integer(i_kind)                              :: idate 

      print *, ' '
      print *, '      --> read_diag_file_ps_nc'


      !--- get NetCDF file dimensions
      !
      if( nc_diag_read_check_dim( 'nobs' )) then
         total_obs = nc_diag_read_get_dim(ftin,'nobs')
         ncdiag_open_status(ii)%num_records = total_obs
         print *, '          total_obs = ', total_obs
      else
         print *, 'ERROR:  unable to read nobs'
         ierr=1
      end if


      !--- get vars
      
      call load_nc_var( 'Station_ID',                    ftin, Station_ID,                    ierr )
      call load_nc_var( 'Observation_Class',             ftin, Observation_Class,             ierr )
      call load_nc_var( 'Observation_Type',              ftin, Observation_Type,              ierr )
      call load_nc_var( 'Observation_Subtype',           ftin, Observation_Subtype,           ierr )
      if( ierr == 1 ) then
         have_subtype = .false.
      end if
      call load_nc_var( 'Latitude',                      ftin, Latitude,                      ierr )
      call load_nc_var( 'Longitude',                     ftin, Longitude,                     ierr )
      call load_nc_var( 'Station_Elevation',             ftin, Station_Elevation,             ierr )
      call load_nc_var( 'Pressure',                      ftin, Pressure,                      ierr )
      call load_nc_var( 'Height',                        ftin, Height,                        ierr )
      call load_nc_var( 'Time',                          ftin, Time,                          ierr )
      call load_nc_var( 'Prep_QC_Mark',                  ftin, Prep_QC_Mark,                  ierr )
      call load_nc_var( 'Nonlinear_QC_Var_Jb',           ftin, Nonlinear_QC_Var_Jb,           ierr )
      call load_nc_var( 'Nonlinear_QC_Rel_Wgt',          ftin, Nonlinear_QC_Rel_Wgt,          ierr )
      call load_nc_var( 'Prep_Use_Flag',                 ftin, Prep_Use_Flag,                 ierr )
      call load_nc_var( 'Analysis_Use_Flag',             ftin, Analysis_Use_Flag,             ierr )
      call load_nc_var( 'Errinv_Input',                  ftin, Errinv_Input,                  ierr )
      call load_nc_var( 'Errinv_Adjust',                 ftin, Errinv_Adjust,                 ierr )
      call load_nc_var( 'Errinv_Final',                  ftin, Errinv_Final,                  ierr )
      call load_nc_var( 'Observation',                   ftin, Observation,                   ierr )
      call load_nc_var( 'Obs_Minus_Forecast_adjusted',   ftin, Obs_Minus_Forecast_adjusted,   ierr )
      call load_nc_var( 'Obs_Minus_Forecast_unadjusted', ftin, Obs_Minus_Forecast_unadjusted, ierr )

        

      !---------------------------------------------------------------
      !  Process all obs.  If type and subtype match the input values 
      !  add this obs to the linked list (ptr%p).
      !
      nobs = 0 
      do ii = 1, total_obs
     

         add_obs = .false.

         !------------------------------------------------------------------------
         ! Check on Observation_Class to ctype is a sanity check -- 
         ! only a single class of obs are in any given NetCDF formatted diag file
         ! but, if the ctype and input file don't match, things could get 
         ! interesting (but not in a good way).
         !
         if( adjustl( trim( Observation_Class(ii) )) == adjustl( trim( ctype ))) then         

            if( return_all .eqv. .true. ) then
               add_obs = .true.

            else if( Observation_Type(ii) == intype ) then

               if( have_subtype .eqv. .false. ) then
                  add_obs = .true.
               else if( Observation_Subtype(ii) == in_subtype ) then
                  add_obs = .true.
               end if

            end if
         end if

         if( add_obs .eqv. .true. )  then 

            nobs=nobs+1

            !---------------------------------------------
            ! Allocate a new data element and load
            !
            allocate( ptr%p )
            ptr%p%stn_id = Station_ID( ii )

            do idx=1,max_rdiag_reals
               ptr%p%rdiag( idx ) = 0.00
            end do
   
            if( allocated( Observation_Type              )) ptr%p%rdiag( idx_obs_type_ps    ) = Observation_Type( ii )   
            if( allocated( Observation_Subtype           )) ptr%p%rdiag( idx_obs_subtype_ps ) = Observation_Subtype( ii )   
            if( allocated( Latitude                      )) ptr%p%rdiag( idx_obs_lat_ps     ) = Latitude( ii )   
            if( allocated( Longitude                     )) ptr%p%rdiag( idx_obs_lon_ps     ) = Longitude( ii )   
            if( allocated( Station_Elevation             )) ptr%p%rdiag( idx_stn_elev_ps    ) = Station_Elevation( ii )   
            if( allocated( Pressure                      )) ptr%p%rdiag( idx_pres_ps        ) = Pressure( ii )
            if( allocated( Height                        )) ptr%p%rdiag( idx_hgt_ps         ) = Height( ii )
            if( allocated( Time                          )) ptr%p%rdiag( idx_time_ps        ) = Time( ii )
            if( allocated( Prep_QC_Mark                  )) ptr%p%rdiag( idx_iqc_ps         ) = Prep_QC_Mark( ii )
            if( allocated( Nonlinear_QC_Var_Jb           )) ptr%p%rdiag( idx_var_jb_ps      ) = Nonlinear_QC_Var_Jb( ii )
            if( allocated( Prep_Use_Flag                 )) ptr%p%rdiag( idx_iuse_ps        ) = Prep_Use_Flag( ii )
            if( allocated( Analysis_Use_Flag             )) ptr%p%rdiag( idx_anl_use_ps     ) = Analysis_Use_Flag( ii )
            if( allocated( Nonlinear_QC_Rel_Wgt          )) ptr%p%rdiag( idx_rwgt_ps        ) = Nonlinear_QC_Rel_Wgt( ii )
            if( allocated( Errinv_Input                  )) ptr%p%rdiag( idx_err_input_ps   ) = Errinv_Input( ii )
            if( allocated( Errinv_Adjust                 )) ptr%p%rdiag( idx_errinv_ps      ) = Errinv_Adjust( ii )
            if( allocated( Errinv_Final                  )) ptr%p%rdiag( idx_errinv_fnl_ps  ) = Errinv_Final( ii )
            if( allocated( Observation                   )) ptr%p%rdiag( idx_obs_ps         ) = Observation( ii )
            if( allocated( Obs_Minus_Forecast_adjusted   )) ptr%p%rdiag( idx_obsmg_adj_ps   ) = Obs_Minus_Forecast_adjusted( ii )
            if( allocated( Obs_Minus_Forecast_unadjusted )) ptr%p%rdiag( idx_obsmg_nadj_ps  ) = Obs_Minus_Forecast_unadjusted( ii )

            if( nobs == 1 ) then
               !-------------------------------------------------
               ! Initialize the list with the first data element
               !
               call list_init(list, transfer(ptr, list_data))
               next => list

            else
               !-------------------------------------------------
               ! Insert subsequent nodes into the list
               !
               call list_insert(next, transfer(ptr, list_data))
               next => list_next(next)
            end if

         end if

      end do


      if( allocated( Station_ID                      )) deallocate( Station_ID                      )
      if( allocated( Observation_Class               )) deallocate( Observation_Class               )
      if( allocated( Observation_Type                )) deallocate( Observation_Type                )
      if( allocated( Observation_Subtype             )) deallocate( Observation_Subtype             )
      if( allocated( Latitude                        )) deallocate( Latitude                        )
      if( allocated( Longitude                       )) deallocate( Longitude                       )
      if( allocated( Station_Elevation               )) deallocate( Station_Elevation               )
      if( allocated( Time                            )) deallocate( Time                            )
      if( allocated( Prep_QC_Mark                    )) deallocate( Prep_QC_Mark                    )
      if( allocated( Prep_Use_Flag                   )) deallocate( Prep_Use_Flag                   )
      if( allocated( Nonlinear_QC_Var_Jb             )) deallocate( Nonlinear_QC_Var_Jb             )
      if( allocated( Nonlinear_QC_Rel_Wgt            )) deallocate( Nonlinear_QC_Rel_Wgt            )
      if( allocated( Analysis_Use_Flag               )) deallocate( Analysis_Use_Flag               )
      if( allocated( Errinv_Input                    )) deallocate( Errinv_Input                    )
      if( allocated( Errinv_Adjust                   )) deallocate( Errinv_Final                    )
      if( allocated( Errinv_Final                    )) deallocate( Errinv_Final                    )
      if( allocated( Observation                     )) deallocate( Observation                     )
      if( allocated( Obs_Minus_Forecast_adjusted     )) deallocate( Obs_Minus_Forecast_adjusted     )
      if( allocated( Obs_Minus_Forecast_unadjusted   )) deallocate( Obs_Minus_Forecast_unadjusted   )

      print *, ' '
      print *, '      <-- read_diag_file_ps_nc'

   end subroutine read_diag_file_ps_nc


   !--------------------------------------------------------- 
   !  netcdf read routine for q data types in netcdf files
   !
   subroutine read_diag_file_q_nc( input_file, return_all, ftin, ctype, intype,expected_nreal,nobs,in_subtype, list )
  
      !--- interface 
      character(100), intent(in) :: input_file
      logical, intent(in)        :: return_all
      integer, intent(in)        :: ftin
      character(3), intent(in)   :: ctype
      integer, intent(in)        :: intype, expected_nreal, in_subtype
      integer, intent(out)       :: nobs
      type(list_node_t), pointer :: list

      !--- local vars
      type(list_node_t), pointer :: next => null()
      type(data_ptr)             :: ptr
      integer                    :: ii, ierr, istatus, total_obs, idx
      logical                    :: have_subtype = .true.
      logical                    :: add_obs

      !--- NetCDF file components                                                               dimension(s) 
      !
      character(len=:), dimension(:), allocatable  :: Station_ID                      !  (nobs, Station_ID_maxstrlen)
      character(len=:), dimension(:), allocatable  :: Observation_Class               !  (nobs, Station_Class_maxstrlen)
      integer, dimension(:), allocatable           :: Observation_Type                !  (obs)
      integer, dimension(:), allocatable           :: Observation_Subtype             !  (obs)
      real(r_single), dimension(:), allocatable    :: Latitude                        !  (obs)
      real(r_single), dimension(:), allocatable    :: Longitude                       !  (obs)
      real(r_single), dimension(:), allocatable    :: Station_Elevation               !  (obs)
      real(r_single), dimension(:), allocatable    :: Pressure                        !  (obs)
      real(r_single), dimension(:), allocatable    :: Height                          !  (obs)
      real(r_single), dimension(:), allocatable    :: Time                            !  (obs)
      real(r_single), dimension(:), allocatable    :: Prep_QC_Mark                    !  (obs)
      real(r_single), dimension(:), allocatable    :: Prep_Use_Flag                   !  (obs)
      real(r_single), dimension(:), allocatable    :: Nonlinear_QC_Var_Jb             !  (obs)
      real(r_single), dimension(:), allocatable    :: Nonlinear_QC_Rel_Wgt            !  (obs)
      real(r_single), dimension(:), allocatable    :: Analysis_Use_Flag               !  (obs)
      real(r_single), dimension(:), allocatable    :: Errinv_Input                    !  (obs)
      real(r_single), dimension(:), allocatable    :: Errinv_Adjust                   !  (obs)
      real(r_single), dimension(:), allocatable    :: Errinv_Final                    !  (obs)
      real(r_single), dimension(:), allocatable    :: Observation                     !  (obs)
      real(r_single), dimension(:), allocatable    :: Obs_Minus_Forecast_adjusted     !  (obs)
      real(r_single), dimension(:), allocatable    :: Obs_Minus_Forecast_unadjusted   !  (obs)
      real(r_single), dimension(:), allocatable    :: Forecast_Saturation_Spec_Hum    !  (obs)
      integer(i_kind)                              :: idate 

      print *, ' '
      print *, '      --> read_diag_file_q_nc'


      !--- get NetCDF file dimensions
      !
      if( nc_diag_read_check_dim( 'nobs' )) then
         total_obs = nc_diag_read_get_dim(ftin,'nobs')
         ncdiag_open_status(ii)%num_records = total_obs
         print *, '          total_obs = ', total_obs
      else
         print *, 'ERROR:  unable to read nobs'
         ierr=1
      end if


      !--- get vars
      
      call load_nc_var( 'Station_ID',                    ftin, Station_ID,                    ierr )
      call load_nc_var( 'Observation_Class',             ftin, Observation_Class,             ierr )
      call load_nc_var( 'Observation_Type',              ftin, Observation_Type,              ierr )
      call load_nc_var( 'Observation_Subtype',           ftin, Observation_Subtype,           ierr )
      if( ierr == 1 ) then
         have_subtype = .false.
      end if
      call load_nc_var( 'Latitude',                      ftin, Latitude,                      ierr )
      call load_nc_var( 'Longitude',                     ftin, Longitude,                     ierr )
      call load_nc_var( 'Station_Elevation',             ftin, Station_Elevation,             ierr )
      call load_nc_var( 'Pressure',                      ftin, Pressure,                      ierr )
      call load_nc_var( 'Height',                        ftin, Height,                        ierr )
      call load_nc_var( 'Time',                          ftin, Time,                          ierr )
      call load_nc_var( 'Prep_QC_Mark',                  ftin, Prep_QC_Mark,                  ierr )
      call load_nc_var( 'Nonlinear_QC_Var_Jb',           ftin, Nonlinear_QC_Var_Jb,           ierr )
      call load_nc_var( 'Nonlinear_QC_Rel_Wgt',          ftin, Nonlinear_QC_Rel_Wgt,          ierr )
      call load_nc_var( 'Prep_Use_Flag',                 ftin, Prep_Use_Flag,                 ierr )
      call load_nc_var( 'Analysis_Use_Flag',             ftin, Analysis_Use_Flag,             ierr )
      call load_nc_var( 'Errinv_Input',                  ftin, Errinv_Input,                  ierr )
      call load_nc_var( 'Errinv_Adjust',                 ftin, Errinv_Adjust,                 ierr )
      call load_nc_var( 'Errinv_Final',                  ftin, Errinv_Final,                  ierr )
      call load_nc_var( 'Observation',                   ftin, Observation,                   ierr )
      call load_nc_var( 'Obs_Minus_Forecast_adjusted',   ftin, Obs_Minus_Forecast_adjusted,   ierr )
      call load_nc_var( 'Obs_Minus_Forecast_unadjusted', ftin, Obs_Minus_Forecast_unadjusted, ierr )
      call load_nc_var( 'Forecast_Saturation_Spec_Hum',  ftin, Forecast_Saturation_Spec_Hum,  ierr )

        

      !---------------------------------------------------------------
      !  Process all obs.  If type and subtype match the input values 
      !  add this obs to the linked list (ptr%p).
      !
      nobs = 0 
      do ii = 1, total_obs

         add_obs = .false.

         !------------------------------------------------------------------------
         ! Check on Observation_Class to ctype is a sanity check -- 
         ! only a single class of obs are in any given NetCDF formatted diag file
         ! but, if the ctype and input file don't match, things could get 
         ! interesting (but not in a good way).
         !
         if( adjustl( trim( Observation_Class(ii) )) == adjustl( trim( ctype ))) then         

            if( return_all .eqv. .true. ) then
               add_obs = .true.

            else if( Observation_Type(ii) == intype ) then

               if( have_subtype .eqv. .false. ) then
                  add_obs = .true.
               else if( Observation_Subtype(ii) == in_subtype ) then
                  add_obs = .true.
               end if

            end if
         end if


         if( add_obs .eqv. .true. )  then 

            nobs=nobs+1

            !---------------------------------------------
            ! Allocate a new data element and load
            !
            allocate(ptr%p)
            ptr%p%stn_id = Station_ID( ii )

            do idx=1,max_rdiag_reals
               ptr%p%rdiag( idx ) = 0.00
            end do

            if( allocated( Observation_Type              )) ptr%p%rdiag( idx_obs_type_q    ) = Observation_Type( ii )   
            if( allocated( Observation_Subtype           )) ptr%p%rdiag( idx_obs_subtype_q ) = Observation_Subtype( ii )   
            if( allocated( Latitude                      )) ptr%p%rdiag( idx_obs_lat_q     ) = Latitude( ii )   
            if( allocated( Longitude                     )) ptr%p%rdiag( idx_obs_lon_q     ) = Longitude( ii )   
            if( allocated( Station_Elevation             )) ptr%p%rdiag( idx_stn_elev_q    ) = Station_Elevation( ii )   
            if( allocated( Pressure                      )) ptr%p%rdiag( idx_pres_q        ) = Pressure( ii )
            if( allocated( Height                        )) ptr%p%rdiag( idx_hgt_q         ) = Height( ii )
            if( allocated( Time                          )) ptr%p%rdiag( idx_time_q        ) = Time( ii )
            if( allocated( Prep_QC_Mark                  )) ptr%p%rdiag( idx_iqc_q         ) = Prep_QC_Mark( ii )
            if( allocated( Nonlinear_QC_Var_Jb           )) ptr%p%rdiag( idx_var_jb_q      ) = Nonlinear_QC_Var_Jb( ii )
            if( allocated( Prep_Use_Flag                 )) ptr%p%rdiag( idx_iuse_q        ) = Prep_Use_Flag( ii )
            if( allocated( Analysis_Use_Flag             )) ptr%p%rdiag( idx_anl_use_q     ) = Analysis_Use_Flag( ii )
            if( allocated( Nonlinear_QC_Rel_Wgt          )) ptr%p%rdiag( idx_rwgt_q        ) = Nonlinear_QC_Rel_Wgt( ii )
            if( allocated( Errinv_Input                  )) ptr%p%rdiag( idx_err_input_q   ) = Errinv_Input( ii )
            if( allocated( Errinv_Adjust                 )) ptr%p%rdiag( idx_errinv_q      ) = Errinv_Adjust( ii )
            if( allocated( Errinv_Final                  )) ptr%p%rdiag( idx_errinv_fnl_q  ) = Errinv_Final( ii )
            if( allocated( Observation                   )) ptr%p%rdiag( idx_obs_q         ) = Observation( ii )
            if( allocated( Obs_Minus_Forecast_adjusted   )) ptr%p%rdiag( idx_obsmg_adj_q   ) = Obs_Minus_Forecast_adjusted( ii )
            if( allocated( Obs_Minus_Forecast_unadjusted )) ptr%p%rdiag( idx_obsmg_nadj_q  ) = Obs_Minus_Forecast_unadjusted( ii )
            if( allocated( Forecast_Saturation_Spec_Hum  )) ptr%p%rdiag( idx_ges_sat_q     ) = Forecast_Saturation_Spec_Hum( ii )


            if( nobs == 1 ) then
               !-------------------------------------------------
               ! Initialize the list with the first data element
               !
               call list_init(list, transfer(ptr, list_data))
               next => list

            else
               !-------------------------------------------------
               ! Insert subsequent nodes into the list
               !
               call list_insert(next, transfer(ptr, list_data))
               next => list_next(next)
            end if

         end if

      end do


      if( allocated( Station_ID                      )) deallocate( Station_ID                      )
      if( allocated( Observation_Class               )) deallocate( Observation_Class               )
      if( allocated( Observation_Type                )) deallocate( Observation_Type                )
      if( allocated( Observation_Subtype             )) deallocate( Observation_Subtype             )
      if( allocated( Latitude                        )) deallocate( Latitude                        )
      if( allocated( Longitude                       )) deallocate( Longitude                       )
      if( allocated( Station_Elevation               )) deallocate( Station_Elevation               )
      if( allocated( Time                            )) deallocate( Time                            )
      if( allocated( Prep_QC_Mark                    )) deallocate( Prep_QC_Mark                    )
      if( allocated( Prep_Use_Flag                   )) deallocate( Prep_Use_Flag                   )
      if( allocated( Nonlinear_QC_Var_Jb             )) deallocate( Nonlinear_QC_Var_Jb             )
      if( allocated( Nonlinear_QC_Rel_Wgt            )) deallocate( Nonlinear_QC_Rel_Wgt            )
      if( allocated( Analysis_Use_Flag               )) deallocate( Analysis_Use_Flag               )
      if( allocated( Errinv_Input                    )) deallocate( Errinv_Input                    )
      if( allocated( Errinv_Adjust                   )) deallocate( Errinv_Final                    )
      if( allocated( Errinv_Final                    )) deallocate( Errinv_Final                    )
      if( allocated( Observation                     )) deallocate( Observation                     )
      if( allocated( Obs_Minus_Forecast_adjusted     )) deallocate( Obs_Minus_Forecast_adjusted     )
      if( allocated( Obs_Minus_Forecast_unadjusted   )) deallocate( Obs_Minus_Forecast_unadjusted   )
      if( allocated( Forecast_Saturation_Spec_Hum    )) deallocate( Forecast_Saturation_Spec_Hum    )

      print *, ' '
      print *, '      <-- read_diag_file_q_nc'

   end subroutine read_diag_file_q_nc



   !------------------------------------------------------------- 
   !  netcdf read routine for ps data types in netcdf files
   !
   !  NOTE1:  This routine is untested.  The ConMon does not 
   !          currently process sst obs.
   !
   !  NOTE2:  There are known discrepencies between the contents
   !          of sst obs in binary and NetCDF files. 
   !
   subroutine read_diag_file_sst_nc( input_file, return_all, ftin, ctype, intype,expected_nreal,nobs,in_subtype, list )
  
      !--- interface 
      character(100), intent(in) :: input_file
      logical, intent(in)        :: return_all
      integer, intent(in)        :: ftin
      character(3), intent(in)   :: ctype
      integer, intent(in)        :: intype, expected_nreal, in_subtype
      integer, intent(out)       :: nobs
      type(list_node_t), pointer :: list

      !--- local vars
      type(list_node_t), pointer :: next => null()
      type(data_ptr)             :: ptr
      integer                    :: ii, ierr, istatus, total_obs, idx
      logical                    :: have_subtype = .true.
      logical                    :: add_obs

      !--- NetCDF file components                                                               dimension(s) 
      !
      character(len=:), dimension(:), allocatable  :: Station_ID                      !  (nobs, Station_ID_maxstrlen)
      character(len=:), dimension(:), allocatable  :: Observation_Class               !  (nobs, Station_Class_maxstrlen)
      integer, dimension(:), allocatable           :: Observation_Type                !  (obs)
      integer, dimension(:), allocatable           :: Observation_Subtype             !  (obs)
      real(r_single), dimension(:), allocatable    :: Latitude                        !  (obs)
      real(r_single), dimension(:), allocatable    :: Longitude                       !  (obs)
      real(r_single), dimension(:), allocatable    :: Station_Elevation               !  (obs)
      real(r_single), dimension(:), allocatable    :: Pressure                        !  (obs)
      real(r_single), dimension(:), allocatable    :: Height                          !  (obs)
      real(r_single), dimension(:), allocatable    :: Time                            !  (obs)
      real(r_single), dimension(:), allocatable    :: Prep_QC_Mark                    !  (obs)
      real(r_single), dimension(:), allocatable    :: Prep_Use_Flag                   !  (obs)
      real(r_single), dimension(:), allocatable    :: Nonlinear_QC_Var_Jb             !  (obs)
      real(r_single), dimension(:), allocatable    :: Nonlinear_QC_Rel_Wgt            !  (obs)
      real(r_single), dimension(:), allocatable    :: Analysis_Use_Flag               !  (obs)
      real(r_single), dimension(:), allocatable    :: Errinv_Input                    !  (obs)
      real(r_single), dimension(:), allocatable    :: Errinv_Adjust                   !  (obs)
      real(r_single), dimension(:), allocatable    :: Errinv_Final                    !  (obs)
      real(r_single), dimension(:), allocatable    :: Observation                     !  (obs)
      real(r_single), dimension(:), allocatable    :: Obs_Minus_Forecast_adjusted     !  (obs)
      real(r_single), dimension(:), allocatable    :: Obs_Minus_Forecast_unadjusted   !  (obs)
      real(r_single), dimension(:), allocatable    :: FoundationTempBG                !  (obs)
      real(r_single), dimension(:), allocatable    :: DiurnalWarming_at_zob           !  (obs)
      real(r_single), dimension(:), allocatable    :: SkinLayerCooling_at_zob         !  (obs)
      real(r_single), dimension(:), allocatable    :: Sensitivity_Tzob_Tr             !  (obs)
      integer(i_kind)                              :: idate 

      print *, ' '
      print *, '      --> read_diag_file_sst_nc'


      !--- get NetCDF file dimensions
      !
      if( nc_diag_read_check_dim( 'nobs' )) then
         total_obs = nc_diag_read_get_dim(ftin,'nobs')
         ncdiag_open_status(ii)%num_records = total_obs
         print *, '          total_obs = ', total_obs
      else
         print *, 'ERROR:  unable to read nobs'
         ierr=1
      end if


      !--- get vars
      
      call load_nc_var( 'Station_ID',                    ftin, Station_ID,                    ierr )
      call load_nc_var( 'Observation_Class',             ftin, Station_ID,                    ierr )
      call load_nc_var( 'Observation_Type',              ftin, Observation_Type,              ierr )
      call load_nc_var( 'Observation_Subtype',           ftin, Observation_Subtype,           ierr )
      if( ierr == 1 ) then
         have_subtype = .false.
      end if
      call load_nc_var( 'Latitude',                      ftin, Latitude,                      ierr )
      call load_nc_var( 'Longitude',                     ftin, Longitude,                     ierr )
      call load_nc_var( 'Station_Elevation',             ftin, Station_Elevation,             ierr )
      call load_nc_var( 'Pressure',                      ftin, Pressure,                      ierr )
      call load_nc_var( 'Height',                        ftin, Height,                        ierr )
      call load_nc_var( 'Time',                          ftin, Time,                          ierr )
      call load_nc_var( 'Prep_QC_Mark',                  ftin, Prep_QC_Mark,                  ierr )
      call load_nc_var( 'Nonlinear_QC_Var_Jb',           ftin, Nonlinear_QC_Var_Jb,           ierr )
      call load_nc_var( 'Nonlinear_QC_Rel_Wgt',          ftin, Nonlinear_QC_Rel_Wgt,          ierr )
      call load_nc_var( 'Prep_Use_Flag',                 ftin, Prep_Use_Flag,                 ierr )
      call load_nc_var( 'Analysis_Use_Flag',             ftin, Analysis_Use_Flag,             ierr )
      call load_nc_var( 'Errinv_Input',                  ftin, Errinv_Input,                  ierr )
      call load_nc_var( 'Errinv_Adjust',                 ftin, Errinv_Adjust,                 ierr )
      call load_nc_var( 'Errinv_Final',                  ftin, Errinv_Final,                  ierr )
      call load_nc_var( 'Observation',                   ftin, Observation,                   ierr )
      call load_nc_var( 'Obs_Minus_Forecast_adjusted',   ftin, Obs_Minus_Forecast_adjusted,   ierr )
      call load_nc_var( 'Obs_Minus_Forecast_unadjusted', ftin, Obs_Minus_Forecast_unadjusted, ierr )
      call load_nc_var( 'FoundationTempBG',              ftin, FoundationTempBG,              ierr )
      call load_nc_var( 'DiurnalWarming_at_zob',         ftin, DiurnalWarming_at_zob,         ierr )
      call load_nc_var( 'SkinLayerCooling_at_zob',       ftin, SkinLayerCooling_at_zob,       ierr )
      call load_nc_var( 'Sensitivity_Tzob_Tr',           ftin, Sensitivity_Tzob_Tr,           ierr )

        

      !---------------------------------------------------------------
      !  Process all obs.  If type and subtype match the input values 
      !  add this obs to the linked list (ptr%p).
      !
      nobs = 0 
      do ii = 1, total_obs

         add_obs = .false.

         !------------------------------------------------------------------------
         ! Check on Observation_Class to ctype is a sanity check --
         ! only a single class of obs are in any given NetCDF formatted diag
         ! file
         ! but, if the ctype and input file don't match, things could get
         ! interesting (but not in a good way).
         !
         if( adjustl( trim( Observation_Class(ii) )) == adjustl( trim( ctype ))) then

            if( return_all .eqv. .true. ) then
               add_obs = .true.

            else if( Observation_Type(ii) == intype ) then

               if( have_subtype .eqv. .false. ) then
                  add_obs = .true.
               else if( Observation_Subtype(ii) == in_subtype ) then
                  add_obs = .true.
               end if

            end if
         end if


         if( add_obs .eqv. .true. )  then

            nobs=nobs+1

            !---------------------------------------------
            ! Allocate a new data element and load
            !
            allocate( ptr%p )
            ptr%p%stn_id = Station_ID( ii )

            do idx=1,max_rdiag_reals
               ptr%p%rdiag( idx ) = 0.00
            end do

            if( allocated( Observation_Type              )) ptr%p%rdiag( idx_obs_type_sst    ) = Observation_Type( ii )   
            if( allocated( Observation_Subtype           )) ptr%p%rdiag( idx_obs_subtype_sst ) = Observation_Subtype( ii )   
            if( allocated( Latitude                      )) ptr%p%rdiag( idx_obs_lat_sst     ) = Latitude( ii )   
            if( allocated( Longitude                     )) ptr%p%rdiag( idx_obs_lon_sst     ) = Longitude( ii )   
            if( allocated( Station_Elevation             )) ptr%p%rdiag( idx_stn_elev_sst    ) = Station_Elevation( ii )   
   
            if( allocated( Pressure                      )) ptr%p%rdiag( idx_opn_wtr_tmp_sst ) = Pressure( ii )  ! identified as background open water temperature in setupsst.f90

            if( allocated( Height                        )) ptr%p%rdiag( idx_depth_sst       ) = Height( ii )    ! identified as observation depth (meters) in setupsst.f90

            if( allocated( Time                          )) ptr%p%rdiag( idx_time_sst        ) = Time( ii )

!            if( allocated(  )) ptr%p%rdiag( idx_opn_wtr_pct_sst ) = Prep_QC_Mark( ii )  ! identified as open water percentage in setupsst.f90

            if( allocated( Prep_QC_Mark                  )) ptr%p%rdiag( idx_setup_qc_sst    ) = Prep_QC_Mark( ii )  
            if( allocated( Prep_Use_Flag                 )) ptr%p%rdiag( idx_iuse_sst        ) = Prep_Use_Flag( ii )
            if( allocated( Analysis_Use_Flag             )) ptr%p%rdiag( idx_anl_use_sst     ) = Analysis_Use_Flag( ii )
            if( allocated( Nonlinear_QC_Rel_Wgt          )) ptr%p%rdiag( idx_rwgt_sst        ) = Nonlinear_QC_Rel_Wgt( ii )
            if( allocated( Errinv_Input                  )) ptr%p%rdiag( idx_err_input_sst   ) = Errinv_Input( ii )
            if( allocated( Errinv_Adjust                 )) ptr%p%rdiag( idx_errinv_sst      ) = Errinv_Adjust( ii )
            if( allocated( Errinv_Final                  )) ptr%p%rdiag( idx_errinv_fnl_sst  ) = Errinv_Final( ii )
            if( allocated( Observation                   )) ptr%p%rdiag( idx_obs_sst         ) = Observation( ii )
            if( allocated( Obs_Minus_Forecast_adjusted   )) ptr%p%rdiag( idx_omgbc_sst       ) = Obs_Minus_Forecast_adjusted( ii )
            if( allocated( Obs_Minus_Forecast_unadjusted )) ptr%p%rdiag( idx_omgnbc_sst      ) = Obs_Minus_Forecast_unadjusted( ii )
 
!            if( allocated(  )) ptr%p%rdiag( idx_type_sst        ) = type of measurement?  per setupsst.f90
   
            if( allocated( FoundationTempBG              )) ptr%p%rdiag( idx_tr_sst          ) = FoundationTempBG( ii )
            if( allocated( DiurnalWarming_at_Zob         )) ptr%p%rdiag( idx_dt_warm_sst     ) = DiurnalWarming_at_zob( ii )
            if( allocated( SkinLayerCooling_at_zob       )) ptr%p%rdiag( idx_dt_cool_sst     ) = SkinLayerCooling_at_zob( ii ) 
            if( allocated( Sensitivity_Tzob_Tr           )) ptr%p%rdiag( idx_dtz_dtr_sst     ) = Sensitivity_Tzob_Tr( ii )


            if( nobs == 1 ) then
               !-------------------------------------------------
               ! Initialize the list with the first data element
               !
               call list_init(list, transfer(ptr, list_data))
               next => list
  
            else
               !-------------------------------------------------
               ! Insert subsequent nodes into the list
               !
               call list_insert(next, transfer(ptr, list_data))
               next => list_next(next)
            end if

         end if

      end do


      if( allocated( Station_ID                      )) deallocate( Station_ID                      )
      if( allocated( Observation_Class               )) deallocate( Observation_Class               )
      if( allocated( Observation_Type                )) deallocate( Observation_Type                )
      if( allocated( Observation_Subtype             )) deallocate( Observation_Subtype             )
      if( allocated( Latitude                        )) deallocate( Latitude                        )
      if( allocated( Longitude                       )) deallocate( Longitude                       )
      if( allocated( Station_Elevation               )) deallocate( Station_Elevation               )
      if( allocated( Time                            )) deallocate( Time                            )
      if( allocated( Prep_QC_Mark                    )) deallocate( Prep_QC_Mark                    )
      if( allocated( Prep_Use_Flag                   )) deallocate( Prep_Use_Flag                   )
      if( allocated( Nonlinear_QC_Var_Jb             )) deallocate( Nonlinear_QC_Var_Jb             )
      if( allocated( Nonlinear_QC_Rel_Wgt            )) deallocate( Nonlinear_QC_Rel_Wgt            )
      if( allocated( Analysis_Use_Flag               )) deallocate( Analysis_Use_Flag               )
      if( allocated( Errinv_Input                    )) deallocate( Errinv_Input                    )
      if( allocated( Errinv_Adjust                   )) deallocate( Errinv_Final                    )
      if( allocated( Errinv_Final                    )) deallocate( Errinv_Final                    )
      if( allocated( Observation                     )) deallocate( Observation                     )
      if( allocated( Obs_Minus_Forecast_adjusted     )) deallocate( Obs_Minus_Forecast_adjusted     )
      if( allocated( Obs_Minus_Forecast_unadjusted   )) deallocate( Obs_Minus_Forecast_unadjusted   )
      if( allocated( FoundationTempBG                )) deallocate( FoundationTempBG                )
      if( allocated( DiurnalWarming_at_zob           )) deallocate( DiurnalWarming_at_zob           )
      if( allocated( SkinLayerCooling_at_zob         )) deallocate( SkinLayerCooling_at_zob         )
      if( allocated( Sensitivity_Tzob_Tr             )) deallocate( Sensitivity_Tzob_Tr             )

      print *, ' '
      print *, '      <-- read_diag_file_sst_nc'

   end subroutine read_diag_file_sst_nc



   !--------------------------------------------------------- 
   !  netcdf read routine for t data types in netcdf files
   !
   subroutine read_diag_file_t_nc( input_file, return_all, ftin, ctype, intype, expected_nreal, nobs, in_subtype, list )
  
      !--- interface 
      character(100), intent(in) :: input_file
      logical, intent(in)        :: return_all
      integer, intent(in)        :: ftin
      character(3), intent(in)   :: ctype
      integer, intent(in)        :: intype, expected_nreal, in_subtype
      integer, intent(out)       :: nobs
      type(list_node_t), pointer :: list

      !--- local vars
      type(list_node_t), pointer :: next => null()
      type(data_ptr)             :: ptr
      integer                    :: ii, ierr, istatus, total_obs, idx, bcor_terms
      logical                    :: have_subtype = .true.
      logical                    :: add_obs

      !--- NetCDF file components                                                               dimension(s) 
      !
      character(len=:), dimension(:), allocatable  :: Station_ID                      !  (nobs, Station_ID_maxstrlen)
      character(len=:), dimension(:), allocatable  :: Observation_Class               !  (nobs, Station_Class_maxstrlen)
      integer, dimension(:), allocatable           :: Observation_Type                !  (obs)
      integer, dimension(:), allocatable           :: Observation_Subtype             !  (obs)
      real(r_single), dimension(:), allocatable    :: Latitude                        !  (obs)
      real(r_single), dimension(:), allocatable    :: Longitude                       !  (obs)
      real(r_single), dimension(:), allocatable    :: Station_Elevation               !  (obs)
      real(r_single), dimension(:), allocatable    :: Pressure                        !  (obs)
      real(r_single), dimension(:), allocatable    :: Height                          !  (obs)
      real(r_single), dimension(:), allocatable    :: Time                            !  (obs)
      real(r_single), dimension(:), allocatable    :: Prep_QC_Mark                    !  (obs)
      real(r_single), dimension(:), allocatable    :: Prep_Use_Flag                   !  (obs)
      real(r_single), dimension(:), allocatable    :: Nonlinear_QC_Var_Jb             !  (obs)
      real(r_single), dimension(:), allocatable    :: Nonlinear_QC_Rel_Wgt            !  (obs)
      real(r_single), dimension(:), allocatable    :: Analysis_Use_Flag               !  (obs)
      real(r_single), dimension(:), allocatable    :: Errinv_Input                    !  (obs)
      real(r_single), dimension(:), allocatable    :: Errinv_Adjust                   !  (obs)
      real(r_single), dimension(:), allocatable    :: Errinv_Final                    !  (obs)
      real(r_single), dimension(:), allocatable    :: Observation                     !  (obs)
      real(r_single), dimension(:), allocatable    :: Obs_Minus_Forecast_adjusted     !  (obs)
      real(r_single), dimension(:), allocatable    :: Obs_Minus_Forecast_unadjusted   !  (obs)
      real(r_single), dimension(:), allocatable    :: Data_Pof                        !  (obs) 
      real(r_single), dimension(:), allocatable    :: Data_Vertical_Velocity          !  (obs)
      real(r_single), dimension(:,:), allocatable  :: Bias_Correction_Terms           !  (nobs, Bias_Correction_Terms_arr_dim)
      integer(i_kind)                              :: idate 
      integer(i_kind)                              :: jj

      print *, ' '
      print *, '      --> read_diag_file_t_nc'

      print *, '            input_file = ', input_file
      print *, '            ftin       = ', ftin
      print *, '            ctype      = ', ctype
      print *, '            intype     = ', intype  
      print *, '            expected_nreal = ', expected_nreal 
      print *, '            in_subtype = ', in_subtype


      !--- get NetCDF file dimensions
      !
      if( nc_diag_read_check_dim( 'nobs' )) then
         total_obs = nc_diag_read_get_dim(ftin,'nobs')
         ncdiag_open_status(ii)%num_records = total_obs
      else
         print *, 'ERROR:  unable to read nobs'
         ierr=1
      end if

      if( nc_diag_read_check_dim( 'Bias_Correction_Terms_arr_dim' )) then
         bcor_terms = nc_diag_read_get_dim(ftin,'Bias_Correction_Terms_arr_dim')
      else
         print *, 'ERROR:  unable to read bcor_terms'
         ierr=1
      end if

    

      !--- get vars
      
      call load_nc_var( 'Station_ID',                    ftin, Station_ID,                    ierr )
      call load_nc_var( 'Observation_Class',             ftin, Observation_Class,             ierr )
      call load_nc_var( 'Observation_Type',              ftin, Observation_Type,              ierr )
      call load_nc_var( 'Observation_Subtype',           ftin, Observation_Subtype,           ierr )
      if( ierr == 1 ) then
         have_subtype = .false.
      end if

      call load_nc_var( 'Latitude',                      ftin, Latitude,                      ierr )
      call load_nc_var( 'Longitude',                     ftin, Longitude,                     ierr )
      call load_nc_var( 'Station_Elevation',             ftin, Station_Elevation,             ierr )
      call load_nc_var( 'Pressure',                      ftin, Pressure,                      ierr )
      call load_nc_var( 'Height',                        ftin, Height,                        ierr )
      call load_nc_var( 'Time',                          ftin, Time,                          ierr )
      call load_nc_var( 'Prep_QC_Mark',                  ftin, Prep_QC_Mark,                  ierr )
      call load_nc_var( 'Nonlinear_QC_Var_Jb',           ftin, Nonlinear_QC_Var_Jb,           ierr )
      call load_nc_var( 'Nonlinear_QC_Rel_Wgt',          ftin, Nonlinear_QC_Rel_Wgt,          ierr )
      call load_nc_var( 'Prep_Use_Flag',                 ftin, Prep_Use_Flag,                 ierr )
      call load_nc_var( 'Analysis_Use_Flag',             ftin, Analysis_Use_Flag,             ierr )
      call load_nc_var( 'Errinv_Input',                  ftin, Errinv_Input,                  ierr )
      call load_nc_var( 'Errinv_Adjust',                 ftin, Errinv_Adjust,                 ierr )
      call load_nc_var( 'Errinv_Final',                  ftin, Errinv_Final,                  ierr )
      call load_nc_var( 'Observation',                   ftin, Observation,                   ierr )
      call load_nc_var( 'Obs_Minus_Forecast_adjusted',   ftin, Obs_Minus_Forecast_adjusted,   ierr )
      call load_nc_var( 'Obs_Minus_Forecast_unadjusted', ftin, Obs_Minus_Forecast_unadjusted, ierr )
      call load_nc_var( 'Data_Pof',                      ftin, Data_Pof,                      ierr )
      call load_nc_var( 'Data_Vertical_Velocity',        ftin, Data_Vertical_Velocity,        ierr )

      if( nc_diag_read_check_var( 'Bias_Correction_Terms' )) then
         call nc_diag_read_get_var( ftin, 'Bias_Correction_Terms', Bias_Correction_Terms )
         ierr = 0
      else
         print *, 'ERROR:  unable to read Bias_Correction_Terms' 
         ierr = 25
      end if

        

      !---------------------------------------------------------------
      !  Process all obs.  If type and subtype match the input values 
      !  add this obs to the linked list (ptr%p).
      !
      nobs = 0 
      do ii = 1, total_obs

         !------------------------------------------------------------------------
         ! Check on Observation_Class to ctype is a sanity check -- 
         ! only a single class of obs are in any given NetCDF formatted diag file
         ! but, if the ctype and input file don't match, things could get 
         ! interesting (but not in a good way).
         !
         add_obs = .false.

         if( adjustl( trim( Observation_Class(ii) )) == adjustl( trim( ctype ))) then         

            if( return_all .eqv. .true. ) then
               add_obs = .true.

            else if( Observation_Type(ii) == intype ) then

               if( have_subtype .eqv. .false. ) then
                  add_obs = .true.
               else if( Observation_Subtype(ii) == in_subtype ) then
                  add_obs = .true.
               end if

            end if
         end if

         if( add_obs .eqv. .true. )  then 

            nobs=nobs+1

            !---------------------------------------------
            ! Allocate a new data element and load
            !
            allocate(ptr%p)
            ptr%p%stn_id = Station_ID( ii )

            do idx=1,max_rdiag_reals
               ptr%p%rdiag( idx ) = 0.00
            end do

            if( allocated( Observation_Type              )) ptr%p%rdiag( idx_obs_type_t )    = Observation_Type( ii )   
            if( allocated( Observation_Subtype           )) ptr%p%rdiag( idx_obs_subtype_t ) = Observation_Subtype( ii )   
            if( allocated( Latitude                      )) ptr%p%rdiag( idx_obs_lat_t )     = Latitude( ii )   
            if( allocated( Longitude                     )) ptr%p%rdiag( idx_obs_lon_t )     = Longitude( ii )   
            if( allocated( Station_Elevation             )) ptr%p%rdiag( idx_stn_elev_t )    = Station_Elevation( ii )   
            if( allocated( Pressure                      )) ptr%p%rdiag( idx_pres_t )        = Pressure( ii )
            if( allocated( Height                        )) ptr%p%rdiag( idx_hgt_t )         = Height( ii )
            if( allocated( Time                          )) ptr%p%rdiag( idx_time_t )        = Time( ii )
            if( allocated( Prep_QC_Mark                  )) ptr%p%rdiag( idx_iqc_t )         = Prep_QC_Mark( ii )
            if( allocated( Nonlinear_QC_Var_Jb           )) ptr%p%rdiag( idx_setup_qc_t )    = Nonlinear_QC_Var_Jb( ii )
            if( allocated( Prep_Use_Flag                 )) ptr%p%rdiag( idx_iuse_t )        = Prep_Use_Flag( ii )
            if( allocated( Analysis_Use_Flag             )) ptr%p%rdiag( idx_anl_use_t )     = Analysis_Use_Flag( ii )
            if( allocated( Nonlinear_QC_Rel_Wgt          )) ptr%p%rdiag( idx_rwgt_t )        = Nonlinear_QC_Rel_Wgt( ii )
            if( allocated( Errinv_Input                  )) ptr%p%rdiag( idx_err_input_t )   = Errinv_Input( ii )
            if( allocated( Errinv_Adjust                 )) ptr%p%rdiag( idx_errinv_t )      = Errinv_Adjust( ii )
            if( allocated( Errinv_Final                  )) ptr%p%rdiag( idx_errinv_fnl_t )  = Errinv_Final( ii )
            if( allocated( Observation                   )) ptr%p%rdiag( idx_obs_t )         = Observation( ii )
            if( allocated( Obs_Minus_Forecast_adjusted   )) ptr%p%rdiag( idx_omgbc_t )       = Obs_Minus_Forecast_adjusted( ii )
            if( allocated( Obs_Minus_Forecast_unadjusted )) ptr%p%rdiag( idx_omgnbc_t )      = Obs_Minus_Forecast_unadjusted( ii )
            if( allocated( Data_Pof                      )) ptr%p%rdiag( idx_pof_t )         = Data_Pof( ii )
            if( allocated( Data_Vertical_Velocity        )) ptr%p%rdiag( idx_vv_t )          = Data_Vertical_Velocity( ii )
            do jj = 1, bcor_terms
               if( allocated( Bias_Correction_Terms      )) ptr%p%rdiag( idx_vv_t+jj )       = Bias_Correction_Terms( jj,ii )
            end do

            if( nobs == 1 ) then
               !-------------------------------------------------
               ! Initialize the list with the first data element
               !
               call list_init(list, transfer(ptr, list_data))
               next => list
   
            else
               !-------------------------------------------------
               ! Insert subsequent nodes into the list
               !
               call list_insert(next, transfer(ptr, list_data))
               next => list_next(next)

            end if

         end if

      end do


      if( allocated( Station_ID                      )) deallocate( Station_ID                      )
      if( allocated( Observation_Class               )) deallocate( Observation_Class               )
      if( allocated( Observation_Type                )) deallocate( Observation_Type                )
      if( allocated( Observation_Subtype             )) deallocate( Observation_Subtype             )
      if( allocated( Latitude                        )) deallocate( Latitude                        )
      if( allocated( Longitude                       )) deallocate( Longitude                       )
      if( allocated( Station_Elevation               )) deallocate( Station_Elevation               )
      if( allocated( Time                            )) deallocate( Time                            )
      if( allocated( Prep_QC_Mark                    )) deallocate( Prep_QC_Mark                    )
      if( allocated( Prep_Use_Flag                   )) deallocate( Prep_Use_Flag                   )
      if( allocated( Nonlinear_QC_Var_Jb             )) deallocate( Nonlinear_QC_Var_Jb             )
      if( allocated( Nonlinear_QC_Rel_Wgt            )) deallocate( Nonlinear_QC_Rel_Wgt            )
      if( allocated( Analysis_Use_Flag               )) deallocate( Analysis_Use_Flag               )
      if( allocated( Errinv_Input                    )) deallocate( Errinv_Input                    )
      if( allocated( Errinv_Adjust                   )) deallocate( Errinv_Final                    )
      if( allocated( Errinv_Final                    )) deallocate( Errinv_Final                    )
      if( allocated( Observation                     )) deallocate( Observation                     )
      if( allocated( Obs_Minus_Forecast_adjusted     )) deallocate( Obs_Minus_Forecast_adjusted     )
      if( allocated( Obs_Minus_Forecast_unadjusted   )) deallocate( Obs_Minus_Forecast_unadjusted   )

      print *, ' '
      print *, '      <-- read_diag_file_t_nc'

   end subroutine read_diag_file_t_nc



   !--------------------------------------------------------- 
   !  netcdf read routine for uv data types in netcdf files
   !
   subroutine read_diag_file_uv_nc( input_file, return_all, ftin, ctype, intype, expected_nreal, nobs, in_subtype, list )
  
      !--- interface 
      character(100), intent(in) :: input_file
      logical, intent(in)        :: return_all
      integer, intent(in)        :: ftin
      character(3), intent(in)   :: ctype
      integer, intent(in)        :: intype, expected_nreal, in_subtype
      integer, intent(out)       :: nobs
      type(list_node_t), pointer :: list

      !--- local vars
      type(list_node_t), pointer :: next => null()
      type(data_ptr)             :: ptr
      integer                    :: ii, ierr, istatus, total_obs, idx
      logical                    :: have_subtype = .true.
      logical                    :: add_obs

      !--- NetCDF file components                                                               dimension(s) 
      !
      character(len=:), dimension(:), allocatable  :: Station_ID                      !  (nobs, Station_ID_maxstrlen)
      character(len=:), dimension(:), allocatable  :: Observation_Class               !  (nobs, Station_Class_maxstrlen)
      integer, dimension(:), allocatable           :: Observation_Type                !  (obs)
      integer, dimension(:), allocatable           :: Observation_Subtype             !  (obs)
      real(r_single), dimension(:), allocatable    :: Latitude                        !  (obs)
      real(r_single), dimension(:), allocatable    :: Longitude                       !  (obs)
      real(r_single), dimension(:), allocatable    :: Station_Elevation               !  (obs)
      real(r_single), dimension(:), allocatable    :: Pressure                        !  (obs)
      real(r_single), dimension(:), allocatable    :: Height                          !  (obs)
      real(r_single), dimension(:), allocatable    :: Time                            !  (obs)
      real(r_single), dimension(:), allocatable    :: Prep_QC_Mark                    !  (obs)
      real(r_single), dimension(:), allocatable    :: Setup_QC_Mark                   !  (obs)
      real(r_single), dimension(:), allocatable    :: Prep_Use_Flag                   !  (obs)
      real(r_single), dimension(:), allocatable    :: Nonlinear_QC_Rel_Wgt            !  (obs)
      real(r_single), dimension(:), allocatable    :: Nonlinear_QC_Var_Jb             !  (obs)
      real(r_single), dimension(:), allocatable    :: Analysis_Use_Flag               !  (obs)
      real(r_single), dimension(:), allocatable    :: Errinv_Input                    !  (obs)
      real(r_single), dimension(:), allocatable    :: Errinv_Adjust                   !  (obs)
      real(r_single), dimension(:), allocatable    :: Errinv_Final                    !  (obs)
      real(r_single), dimension(:), allocatable    :: Wind_Reduction_Factor_at_10m    !  (obs)
      real(r_single), dimension(:), allocatable    :: u_Observation                   !  (obs)
      real(r_single), dimension(:), allocatable    :: u_Obs_Minus_Forecast_adjusted   !  (obs)
      real(r_single), dimension(:), allocatable    :: u_Obs_Minus_Forecast_unadjusted !  (obs)
      real(r_single), dimension(:), allocatable    :: v_Observation                   !  (obs)
      real(r_single), dimension(:), allocatable    :: v_Obs_Minus_Forecast_adjusted   !  (obs)
      real(r_single), dimension(:), allocatable    :: v_Obs_Minus_Forecast_unadjusted !  (obs)
      integer(i_kind)                              :: idate 

      print *, ' '
      print *, '      --> read_diag_file_uv_nc'


      !--- get NetCDF file dimensions
      !
      if( nc_diag_read_check_dim( 'nobs' )) then
         total_obs = nc_diag_read_get_dim(ftin,'nobs')
         ncdiag_open_status(ii)%num_records = total_obs
         print *, '          total_obs = ', total_obs
      else
         print *, 'ERROR:  unable to read nobs'
         ierr=1
      end if


      !--- get vars
      
      call load_nc_var( 'Station_ID',                      ftin, Station_ID,                      ierr )
      call load_nc_var( 'Observation_Class',               ftin, Observation_Class,               ierr )
      call load_nc_var( 'Observation_Type',                ftin, Observation_Type,                ierr )
      call load_nc_var( 'Observation_Subtype',             ftin, Observation_Subtype,             ierr )
      if( ierr == 1 ) then
         have_subtype = .false.
      end if
      call load_nc_var( 'Latitude',                        ftin, Latitude,                        ierr )
      call load_nc_var( 'Longitude',                       ftin, Longitude,                       ierr )
      call load_nc_var( 'Station_Elevation',               ftin, Station_Elevation,               ierr )
      call load_nc_var( 'Pressure',                        ftin, Pressure,                        ierr )
      call load_nc_var( 'Height',                          ftin, Height,                          ierr )
      call load_nc_var( 'Time',                            ftin, Time,                            ierr )
      call load_nc_var( 'Prep_QC_Mark',                    ftin, Prep_QC_Mark,                    ierr )
      call load_nc_var( 'Setup_QC_Mark',                   ftin, Setup_QC_Mark,                   ierr )
      call load_nc_var( 'Nonlinear_QC_Rel_Wgt',            ftin, Nonlinear_QC_Rel_Wgt,            ierr )
      call load_nc_var( 'Prep_Use_Flag',                   ftin, Prep_Use_Flag,                   ierr )
      call load_nc_var( 'Nonlinear_QC_Var_Jb',             ftin, Nonlinear_QC_Var_Jb,             ierr )
      call load_nc_var( 'Analysis_Use_Flag',               ftin, Analysis_Use_Flag,               ierr )
      call load_nc_var( 'Errinv_Input',                    ftin, Errinv_Input,                    ierr )
      call load_nc_var( 'Errinv_Adjust',                   ftin, Errinv_Adjust,                   ierr )
      call load_nc_var( 'Errinv_Final',                    ftin, Errinv_Final,                    ierr )
      call load_nc_var( 'Wind_Reduction_Factor_at_10m',    ftin, Wind_Reduction_Factor_at_10m,    ierr )
      call load_nc_var( 'u_Observation',                   ftin, u_Observation,                   ierr )
      call load_nc_var( 'u_Obs_Minus_Forecast_adjusted',   ftin, u_Obs_Minus_Forecast_adjusted,   ierr )
      call load_nc_var( 'u_Obs_Minus_Forecast_unadjusted', ftin, u_Obs_Minus_Forecast_unadjusted, ierr )
      call load_nc_var( 'v_Observation',                   ftin, v_Observation,                   ierr )
      call load_nc_var( 'v_Obs_Minus_Forecast_adjusted',   ftin, v_Obs_Minus_Forecast_adjusted,   ierr )
      call load_nc_var( 'v_Obs_Minus_Forecast_unadjusted', ftin, v_Obs_Minus_Forecast_unadjusted, ierr )


      !---------------------------------------------------------------
      !  Process all obs.  If type and subtype match the input values 
      !  add this obs to the linked list (ptr%p).
      !
      nobs = 0 
      do ii = 1, total_obs
         

         !------------------------------------------------------------------------
         ! Check on Observation_Class to ctype is a sanity check -- 
         ! only a single class of obs are in any given NetCDF formatted diag file
         ! but, if the ctype and input file don't match, things could get 
         ! interesting (but not in a good way).
         !
         add_obs = .false.

         if( adjustl( trim( Observation_Class(ii) )) == adjustl( trim( ctype ))) then         

            if( return_all .eqv. .true. ) then
               add_obs = .true.

            else if( Observation_Type(ii) == intype ) then

               if( have_subtype .eqv. .false. ) then
                  add_obs = .true.
               else if( Observation_Subtype(ii) == in_subtype ) then
                  add_obs = .true.
               end if

            end if
         end if

         if( add_obs .eqv. .true. )  then 

            nobs=nobs+1

            !---------------------------------------------
            ! Allocate a new data element and load
            !
            allocate(ptr%p)
            ptr%p%stn_id = Station_ID( ii )

            do idx=1,max_rdiag_reals
               ptr%p%rdiag( idx ) = 0.00
            end do

            if( allocated( Observation_Type                )) ptr%p%rdiag( idx_obs_type_uv    ) = Observation_Type( ii )   
            if( allocated( Observation_Subtype             )) ptr%p%rdiag( idx_obs_subtype_uv ) = Observation_Subtype( ii )   
            if( allocated( Latitude                        )) ptr%p%rdiag( idx_obs_lat_uv     ) = Latitude( ii )   
            if( allocated( Longitude                       )) ptr%p%rdiag( idx_obs_lon_uv     ) = Longitude( ii )   
            if( allocated( Station_Elevation               )) ptr%p%rdiag( idx_stn_elev_uv    ) = Station_Elevation( ii )   
            if( allocated( Pressure                        )) ptr%p%rdiag( idx_pres_uv        ) = Pressure( ii )
            if( allocated( Height                          )) ptr%p%rdiag( idx_hgt_uv         ) = Height( ii )
            if( allocated( Time                            )) ptr%p%rdiag( idx_time_uv        ) = Time( ii )
            if( allocated( Prep_QC_Mark                    )) ptr%p%rdiag( idx_iqc_uv         ) = Prep_QC_Mark( ii )

!            if( allocated( Nonlinear_QC_Var_JB             )) ptr%p%rdiag( idx_setup_qc_uv    ) = Nonlinear_QC_Var_Jb( ii )  ! missing from diagnostic file

            if( allocated( Prep_Use_Flag                   )) ptr%p%rdiag( idx_iuse_uv        ) = Prep_Use_Flag( ii )
            if( allocated( Analysis_Use_Flag               )) ptr%p%rdiag( idx_anl_use_uv     ) = Analysis_Use_Flag( ii )
            if( allocated( Nonlinear_QC_Rel_Wgt            )) ptr%p%rdiag( idx_rwgt_uv        ) = Nonlinear_QC_Rel_Wgt( ii )
            if( allocated( Errinv_Input                    )) ptr%p%rdiag( idx_err_input_uv   ) = Errinv_Input( ii )
            if( allocated( Errinv_Adjust                   )) ptr%p%rdiag( idx_errinv_uv      ) = Errinv_Adjust( ii )
            if( allocated( Errinv_Final                    )) ptr%p%rdiag( idx_errinv_fnl_uv  ) = Errinv_Final( ii )

            if( allocated( u_Observation                   )) ptr%p%rdiag( idx_u_obs_uv       ) = u_Observation( ii )
            if( allocated( u_Obs_Minus_Forecast_adjusted   )) ptr%p%rdiag( idx_u_omgbc_uv     ) = u_Obs_Minus_Forecast_adjusted( ii )
            if( allocated( u_Obs_Minus_Forecast_unadjusted )) ptr%p%rdiag( idx_u_omgnbc_uv    ) = u_Obs_Minus_Forecast_unadjusted( ii )
            if( allocated( v_Observation                   )) ptr%p%rdiag( idx_v_obs_uv       ) = v_Observation( ii )
            if( allocated( v_Obs_Minus_Forecast_adjusted   )) ptr%p%rdiag( idx_v_omgbc_uv     ) = v_Obs_Minus_Forecast_adjusted( ii )
            if( allocated( v_Obs_Minus_Forecast_unadjusted )) ptr%p%rdiag( idx_v_omgnbc_uv    ) = v_Obs_Minus_Forecast_unadjusted( ii )


            if( nobs == 1 ) then
               !-------------------------------------------------
               ! Initialize the list with the first data element
               !
               call list_init(list, transfer(ptr, list_data))
               next => list
   
            else
               !-------------------------------------------------
               ! Insert subsequent nodes into the list
               !
               call list_insert(next, transfer(ptr, list_data))
               next => list_next(next)
            end if

         end if

      end do


      if( allocated( Station_ID                      )) deallocate( Station_ID                      )
      if( allocated( Observation_Class               )) deallocate( Observation_Class               )
      if( allocated( Observation_Type                )) deallocate( Observation_Type                )
      if( allocated( Observation_Subtype             )) deallocate( Observation_Subtype             )
      if( allocated( Latitude                        )) deallocate( Latitude                        )
      if( allocated( Longitude                       )) deallocate( Longitude                       )
      if( allocated( Station_Elevation               )) deallocate( Station_Elevation               )
      if( allocated( Time                            )) deallocate( Time                            )
      if( allocated( Prep_QC_Mark                    )) deallocate( Prep_QC_Mark                    )
      if( allocated( Prep_Use_Flag                   )) deallocate( Prep_Use_Flag                   )
      if( allocated( Nonlinear_QC_Var_Jb             )) deallocate( Nonlinear_QC_Var_Jb             )
      if( allocated( Nonlinear_QC_Rel_Wgt            )) deallocate( Nonlinear_QC_Rel_Wgt            )
      if( allocated( Analysis_Use_Flag               )) deallocate( Analysis_Use_Flag               )
      if( allocated( Errinv_Input                    )) deallocate( Errinv_Input                    )
      if( allocated( Errinv_Adjust                   )) deallocate( Errinv_Final                    )
      if( allocated( Errinv_Final                    )) deallocate( Errinv_Final                    )
      if( allocated( Wind_Reduction_Factor_at_10m    )) deallocate( Wind_Reduction_Factor_at_10m    )
      if( allocated( u_Observation                   )) deallocate( u_Observation                   )
      if( allocated( u_Obs_Minus_Forecast_adjusted   )) deallocate( u_Obs_Minus_Forecast_adjusted   )
      if( allocated( u_Obs_Minus_Forecast_unadjusted )) deallocate( u_Obs_Minus_Forecast_unadjusted )
      if( allocated( v_Observation                   )) deallocate( v_Observation                   )
      if( allocated( v_Obs_Minus_Forecast_adjusted   )) deallocate( v_Obs_Minus_Forecast_adjusted   )
      if( allocated( v_Obs_Minus_Forecast_unadjusted )) deallocate( v_Obs_Minus_Forecast_unadjusted )

      print *, ' '
      print *, '      <-- read_diag_file_uv_nc'

   end subroutine read_diag_file_uv_nc


   !----------------------------------------------------------- 
   !  netcdf read routine for gps data types in netcdf files
   !
   !  NOTE1:  This routine is untested.  At present the ConMon
   !          does not process gps data.  There are plans to 
   !          change that though.
   !
   !  NOTE2:  There are known descrepencies between gps obs 
   !          in binary and NetCDF formatted diag files. See
   !          comments below.
   !
   subroutine read_diag_file_gps_nc( input_file, return_all, ftin, ctype, intype,expected_nreal,nobs,in_subtype, list )
 
      !--- interface 
      character(100), intent(in) :: input_file
      logical, intent(in)        :: return_all
      integer, intent(in)        :: ftin
      character(3), intent(in)   :: ctype
      integer, intent(in)        :: intype, expected_nreal, in_subtype
      integer, intent(out)       :: nobs
      type(list_node_t), pointer :: list

      !--- local vars
      type(list_node_t), pointer :: next => null()
      type(data_ptr)             :: ptr
      integer                    :: ii, ierr, istatus, total_obs, idx
      logical                    :: have_subtype = .true.
      logical                    :: add_obs

      !--- NetCDF file components                                                               dimension(s) 
      !
      character(len=:), dimension(:), allocatable  :: Station_ID                        !  (nobs, Station_ID_maxstrlen)
      character(len=:), dimension(:), allocatable  :: Observation_Class                 !  (nobs, Station_Class_maxstrlen)
      integer, dimension(:), allocatable           :: Observation_Type                  !  (obs)
      integer, dimension(:), allocatable           :: Observation_Subtype               !  (obs)
      real(r_single), dimension(:), allocatable    :: Latitude                          !  (obs)
      real(r_single), dimension(:), allocatable    :: Longitude                         !  (obs)
      real(r_single), dimension(:), allocatable    :: Incremental_Bending_Angle         !  (obs)
      real(r_single), dimension(:), allocatable    :: Station_Elevation                 !  (obs)
      real(r_single), dimension(:), allocatable    :: Pressure                          !  (obs)
      real(r_single), dimension(:), allocatable    :: Height                            !  (obs)
      real(r_single), dimension(:), allocatable    :: Time                              !  (obs)
      real(r_single), dimension(:), allocatable    :: Model_Elevation                   !  (obs)
      real(r_single), dimension(:), allocatable    :: Setup_QC_Mark                     !  (obs)
      real(r_single), dimension(:), allocatable    :: Prep_Use_Flag                     !  (obs)
      real(r_single), dimension(:), allocatable    :: Nonlinear_QC_Var_Jb               !  (obs)
      real(r_single), dimension(:), allocatable    :: Nonlinear_QC_Rel_Wgt              !  (obs)
      real(r_single), dimension(:), allocatable    :: Analysis_Use_Flag                 !  (obs)
      real(r_single), dimension(:), allocatable    :: Errinv_Input                      !  (obs)
      real(r_single), dimension(:), allocatable    :: Errinv_Adjust                     !  (obs)
      real(r_single), dimension(:), allocatable    :: Errinv_Final                      !  (obs)
      real(r_single), dimension(:), allocatable    :: Observation                       !  (obs)
      real(r_single), dimension(:), allocatable    :: Obs_Minus_Forecast_adjusted       !  (obs)
      real(r_single), dimension(:), allocatable    :: Obs_Minus_Forecast_unadjusted     !  (obs)
      real(r_single), dimension(:), allocatable    :: GPS_Type                          !  (obs)
      real(r_single), dimension(:), allocatable    :: Temperature_at_Obs_Location       !  (obs)
      real(r_single), dimension(:), allocatable    :: Specific_Humidity_at_Obs_Location !  (obs)

      integer(i_kind)                              :: idate 



      print *, ' '
      print *, '      --> read_diag_file_gps_nc'


      !--- get NetCDF file dimensions
      !
      if( nc_diag_read_check_dim( 'nobs' )) then
         total_obs = nc_diag_read_get_dim(ftin,'nobs')
         ncdiag_open_status(ii)%num_records = total_obs
         print *, '          total_obs = ', total_obs
      else
         print *, 'ERROR:  unable to read nobs'
         ierr=1
      end if


      !--- get vars
      
      call load_nc_var( 'Station_ID',                        ftin, Station_ID,                        ierr )
      call load_nc_var( 'Observation_Class',                 ftin, Observation_Class,                 ierr )
      call load_nc_var( 'Observation_Type',                  ftin, Observation_Type,                  ierr )
      call load_nc_var( 'Observation_Subtype',               ftin, Observation_Subtype,               ierr )
      if( ierr == 1 ) then
         have_subtype = .false.
      end if
      call load_nc_var( 'Latitude',                          ftin, Latitude,                          ierr )
      call load_nc_var( 'Longitude',                         ftin, Longitude,                         ierr )
      call load_nc_var( 'Incremental_Bending_Angle',         ftin, Incremental_Bending_Angle,         ierr )
      call load_nc_var( 'Pressure',                          ftin, Pressure,                          ierr )
      call load_nc_var( 'Height',                            ftin, Height,                            ierr )
      call load_nc_var( 'Time',                              ftin, Time,                              ierr )
      call load_nc_var( 'Model_Elevation',                   ftin, Model_Elevation,                   ierr )
      call load_nc_var( 'Setup_QC_Mark',                     ftin, Setup_QC_Mark,                     ierr )
      call load_nc_var( 'Prep_Use_Flag',                     ftin, Prep_Use_Flag,                     ierr )
      call load_nc_var( 'Analysis_Use_Flag',                 ftin, Analysis_Use_Flag,                 ierr )
      call load_nc_var( 'Nonlinear_QC_Rel_Wgt',              ftin, Nonlinear_QC_Rel_Wgt,              ierr )
      call load_nc_var( 'Errinv_Input',                      ftin, Errinv_Input,                      ierr )
      call load_nc_var( 'Errinv_Adjust',                     ftin, Errinv_Adjust,                     ierr )
      call load_nc_var( 'Errinv_Final',                      ftin, Errinv_Final,                      ierr )
      call load_nc_var( 'Observation',                       ftin, Observation,                       ierr )
      call load_nc_var( 'Obs_Minus_Forecast_adjusted',       ftin, Obs_Minus_Forecast_adjusted,       ierr )
      call load_nc_var( 'Obs_Minus_Forecast_unadjusted',     ftin, Obs_Minus_Forecast_unadjusted,     ierr )
      call load_nc_var( 'GPS_Type',                          ftin, GPS_Type,                          ierr )
      call load_nc_var( 'Temperature_at_Obs_Location',       ftin, Temperature_at_Obs_Location,       ierr )
      call load_nc_var( 'Specific_Humidity_at_Obs_Location', ftin, Specific_Humidity_at_Obs_Location, ierr )

!      call load_nc_var( 'Nonlinear_QC_Var_Jb',           ftin, Nonlinear_QC_Var_Jb,           13, ierr )

 

      !---------------------------------------------------------------
      !  Process all obs.  If type and subtype match the input values 
      !  add this obs to the linked list (ptr%p).
      !
      nobs = 0 
      do ii = 1, total_obs

         !------------------------------------------------------------------------
         ! Check on Observation_Class to ctype is a sanity check --
         ! only a single class of obs are in any given NetCDF formatted diag
         ! file
         ! but, if the ctype and input file don't match, things could get
         ! interesting (but not in a good way).
         !
         add_obs = .false.

         if( adjustl( trim( Observation_Class(ii) )) == adjustl( trim( ctype ))) then

            if( return_all .eqv. .true. ) then
               add_obs = .true.

            else if( Observation_Type(ii) == intype ) then

               if( have_subtype .eqv. .false. ) then
                  add_obs = .true.
               else if( Observation_Subtype(ii) == in_subtype ) then
                  add_obs = .true.
               end if

            end if
         end if

         if( add_obs .eqv. .true. )  then

            nobs=nobs+1

            !---------------------------------------------
            ! Allocate a new data element and load
            !
            allocate(ptr%p)
            ptr%p%stn_id = Station_ID( ii )

            do idx=1,max_rdiag_reals
               ptr%p%rdiag( idx ) = 0.00
            end do
   
            if( allocated ( Observation_Type                  )) ptr%p%rdiag( idx_obs_type_gps ) = Observation_Type( ii )   
            if( allocated ( Observation_Subtype               )) ptr%p%rdiag( idx_obs_subtype_gps ) = Observation_Subtype( ii )   
            if( allocated ( Latitude                          )) ptr%p%rdiag( idx_obs_lat_gps ) = Latitude( ii )   
            if( allocated ( Longitude                         )) ptr%p%rdiag( idx_obs_lon_gps ) = Longitude( ii )   
            if( allocated ( Incremental_Bending_Angle         )) ptr%p%rdiag( idx_bend_ang_gps ) = Incremental_Bending_Angle( ii )   
            if( allocated ( Pressure                          )) ptr%p%rdiag( idx_pres_gps ) = Pressure( ii )
            if( allocated ( Height                            )) ptr%p%rdiag( idx_height_gps ) = Height( ii )
            if( allocated ( Time                              )) ptr%p%rdiag( idx_time_gps ) = Time( ii )
            if( allocated ( Model_Elevation                   )) ptr%p%rdiag( idx_zsges_gps ) = Model_Elevation( ii )
            if( allocated ( Setup_QC_Mark                     )) ptr%p%rdiag( idx_setup_qc_gps ) = Setup_QC_Mark( ii )
            if( allocated ( Prep_Use_Flag                     )) ptr%p%rdiag( idx_iuse_gps ) = Prep_Use_Flag( ii )
            if( allocated ( Analysis_Use_Flag                 )) ptr%p%rdiag( idx_anl_use_gps ) = Analysis_Use_Flag( ii )
            if( allocated ( Nonlinear_QC_Rel_Wgt              )) ptr%p%rdiag( idx_rwgt_gps ) = Nonlinear_QC_Rel_Wgt( ii )
            if( allocated ( Errinv_input                      )) ptr%p%rdiag( idx_err_input_gps ) = Errinv_Input( ii )
            if( allocated ( Errinv_Adjust                     )) ptr%p%rdiag( idx_errinv_gps ) = Errinv_Adjust( ii )
            if( allocated ( Errinv_Final                      )) ptr%p%rdiag( idx_errinv_fnl_gps ) = Errinv_Final( ii )
  
            if( allocated ( Observation                       )) ptr%p%rdiag( idx_obs_gps ) = Observation( ii )
            if( allocated ( Temperature_at_Obs_Location       )) ptr%p%rdiag( idx_tref_gps ) = Temperature_at_Obs_Location( ii )
!            if( allocated ( Obs_Minus_Forecast_unadjusted    )) ptr%p%rdiag( idx_hob_gps ) = Obs_Minus_Forecast_unadjusted( ii )  
            if( allocated ( GPS_Type                          )) ptr%p%rdiag( idx_uses_gps ) = GPS_Type( ii )
            if( allocated ( Specific_Humidity_at_Obs_Location )) ptr%p%rdiag( idx_qref_gps ) = Specific_Humidity_at_Obs_Location( ii )


           !  This oddity is from genstats_gps.f90 which produces the NetCDF
           !  formatted diag file:
           !
           !  call nc_diag_metadata("Obs_Minus_Forecast_adjusted", sngl(gps_allptr%rdiag(17))*sngl(gps_allptr%rdiag(5)) )
           !  call nc_diag_metadata("Obs_Minus_Forecast_unadjusted", sngl(gps_allptr%rdiag(17))*sngl(gps_allptr%rdiag(5)) )
           !  call nc_diag_metadata("GPS_Type", sngl(gps_allptr%rdiag(20))   )
           !  call nc_diag_metadata("Temperature_at_Obs_Location", sngl(gps_allptr%rdiag(18))   )
           !
           !  It would seem from this that rdiagbuf(19) is not used?
           !  And also Obs_Minus_Forecast_[un|'']adjusted is derived and not stored.
           !
           !  This from setupbend.f90:
           !    rdiagbuf(18,i)  = trefges   ! temperature at obs location (Kelvin) if monotone grid
           !    rdiagbuf(19,i)  = hob       ! model vertical grid (interface) if monotone grid
           !    rdiagbuf(20,i)  = one       ! uses gps_ref (one = use of bending angle)
           !

 
            if( nobs == 1 ) then
               !-------------------------------------------------
               ! Initialize the list with the first data element
               !
               call list_init(list, transfer(ptr, list_data))
               next => list
   
            else
               !-------------------------------------------------
               ! Insert subsequent nodes into the list
               !
               call list_insert(next, transfer(ptr, list_data))
               next => list_next(next)
  
            end if

         end if

      end do


      if( allocated( Station_ID                        )) deallocate( Station_ID                        )
      if( allocated( Observation_Class                 )) deallocate( Observation_Class                 )
      if( allocated( Observation_Type                  )) deallocate( Observation_Type                  )
      if( allocated( Observation_Subtype               )) deallocate( Observation_Subtype               )
      if( allocated( Latitude                          )) deallocate( Latitude                          )
      if( allocated( Longitude                         )) deallocate( Longitude                         )
      if( allocated( Incremental_Bending_Angle         )) deallocate( Incremental_Bending_Angle         )
      if( allocated( Pressure                          )) deallocate( Pressure                          )
      if( allocated( Height                            )) deallocate( Height                            )
      if( allocated( Time                              )) deallocate( Time                              )
      if( allocated( Model_Elevation                   )) deallocate( Model_Elevation                   )
      if( allocated( Setup_QC_Mark                     )) deallocate( Setup_QC_Mark                     )
      if( allocated( Prep_Use_Flag                     )) deallocate( Prep_Use_Flag                     )
      if( allocated( Analysis_Use_Flag                 )) deallocate( Analysis_Use_Flag                 )
      if( allocated( Nonlinear_QC_Rel_Wgt              )) deallocate( Nonlinear_QC_Rel_Wgt              )
      if( allocated( Errinv_Input                      )) deallocate( Errinv_Input                      )
      if( allocated( Errinv_Adjust                     )) deallocate( Errinv_Final                      )
      if( allocated( Errinv_Final                      )) deallocate( Errinv_Final                      )
      if( allocated( Observation                       )) deallocate( Observation                       )
      if( allocated( Obs_Minus_Forecast_adjusted       )) deallocate( Obs_Minus_Forecast_adjusted       )
      if( allocated( Obs_Minus_Forecast_unadjusted     )) deallocate( Obs_Minus_Forecast_unadjusted     )
      if( allocated( GPS_Type                          )) deallocate( GPS_Type                          )
      if( allocated( Temperature_at_Obs_Location       )) deallocate( Temperature_at_Obs_Location       )
      if( allocated( Specific_Humidity_at_Obs_Location )) deallocate( Specific_Humidity_at_Obs_Location )

      print *, ' '
      print *, '      <-- read_diag_file_gps_nc'

   end subroutine read_diag_file_gps_nc



 
   !---  binary read routine
   !
   subroutine read_diag_file_bin( input_file, return_all, ctype, intype,expected_nreal,nobs,in_subtype, list )

      !--- interface 
      character(100), intent(in) :: input_file
      logical, intent(in)        :: return_all
      character(3), intent(in)   :: ctype
      integer, intent(in)        :: intype, expected_nreal, in_subtype
      integer, intent(out)       :: nobs
      type(list_node_t), pointer :: list


      !--- local vars
      type(list_node_t), pointer :: next => null()
      type(data_ptr)             :: ptr
 
      real(4),allocatable,dimension(:,:)     :: rdiag 
      character(8),allocatable,dimension(:)  :: cdiag 

      character(3)   :: dtype
      character(10)  :: otype
      character(15)  :: fileo,fileo_subtyp

      integer nchar,file_nreal,i,ii,mype,idate,iflag,file_itype,iscater,igrads
      integer lunin,lunot,ldtype,file_subtype
      integer idx,ioff02

      data lunin / 11 /

      nobs=0
      print *, '      --> read_diag_file_bin'
      print *, '            ctype            = ', ctype
      print *, '            intype, in_subtype = ', intype, in_subtype
      print *, '            expected_nreal     = ', expected_nreal


      !--- open diag file
      open(lunin, file=input_file, form='unformatted')  
      rewind(lunin)

      read(lunin) idate
      print *, 'idate=',idate 

      loopd: do  

         !--- read obs header
         read(lunin,IOSTAT=iflag) dtype,nchar,file_nreal,ii,mype,ioff02

         if( iflag /= 0 ) exit loopd

         !-----------------------------------------------------
         !  exit loop if the number of reals (nreal) from file 
         !  doesn't match the target number
         !
         if(( return_all .eqv. .true. ) .OR. ( trim(dtype) == trim(ctype) .and. file_nreal /= expected_nreal )) then
            print *, 'matched observation type:',dtype,' file_nreal=', file_nreal
            exit 
         endif

         !--------------------------------------------- 
         ! skip to next iteration if types don't match
         !
         if(( return_all .eqv. .false. ) .AND. ( trim( dtype ) /= trim( ctype )))  then
            cycle
         endif


         !--- read diag info
         allocate(cdiag(ii),rdiag(file_nreal,ii))
         read(lunin,IOSTAT=iflag) cdiag,rdiag

         !--- exit loop on read error
         if( iflag /= 0 ) then
            deallocate( cdiag,rdiag )
            exit loopd
         end if

         do i=1,ii
            file_itype = int(rdiag(1,i)) 
            file_subtype = int(rdiag(2,i)) 

            !------------------------------------------------
            !  if both types and subtypes match 
            !  then add a new data element 
            !
            if(( return_all .eqv. .true. ) .OR. ( file_itype == intype .AND. file_subtype == in_subtype ))  then 

               nobs=nobs+1

               !---------------------------------------------
               ! Allocate a new data element and load
               !
               allocate(ptr%p)
               ptr%p%stn_id = cdiag(i)

               do idx=1,max_rdiag_reals
                  if( idx > file_nreal ) then
                     ptr%p%rdiag( idx ) = 0.00
                  else
                     ptr%p%rdiag( idx ) = rdiag( idx,i )
                  end if
               end do 
   
               if( nobs == 1 ) then
                  !-------------------------------------------------
                  ! Initialize the list with the first data element
                  !
                  call list_init(list, transfer(ptr, list_data))
                  next => list

               else
                  !-------------------------------------------------
                  ! Insert subsequent nodes into the list
                  !
                  call list_insert(next, transfer(ptr, list_data))
                  next => list_next(next)
   
               end if

            endif

         enddo

         deallocate(cdiag,rdiag)
      enddo   loopd               !  ending read data do loop
   
      close(lunin)

      print *, 'nobs added to list = ', nobs
      print *, '      <-- read_diag_file_bin'

      return 
   end subroutine read_diag_file_bin



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


end module conmon_read_diag
