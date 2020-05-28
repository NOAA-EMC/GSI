!----------------------------------------------------------------------
!  conmon_read_diag
!
!     This subroutine reads the conventional data assimilation 
!     diagnostic files (contained in the cnvstat file) and writes it 
!     to the [stype]_[subtype].tmp file.
!
!     NOTE:  Next step in clean-up is to write directly to the 
!               GrADS output file rather than to the .tmp file,
!               which is then re-read and converted to the GrADS
!               output file.  Unnecessary I/O.
!     Note:
!        intype  : the observarion type like t for temp, uv for wind
!        stype   : the observation sub type, like t120 uv220
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


   subroutine conmon_read_diag_file( input_file,ctype,stype,intype,expected_nreal,nobs,in_subtype,subtype,list )

      !--- interface 
      character(100), intent(in) :: input_file
      character(3), intent(in)   :: ctype
      character(10), intent(in)  :: stype                   !! appears not to be used
      character(3), intent(in)   :: subtype                 !! appears not to be used

      !--- note expected_nreal has no meaning for netcdf files
      integer, intent(in)        :: intype, expected_nreal, in_subtype
      integer, intent(out)       :: nobs
      type(list_node_t), pointer :: list


      write(6,*)'--> conmon_read_diag_file'

      if ( netcdf ) then
         write(6,*) ' call nc read subroutine'
         call read_diag_file_nc( input_file, ctype, stype, intype, expected_nreal, nobs, in_subtype, subtype, list )
      else
         call read_diag_file_bin( input_file, ctype,stype,intype,expected_nreal,nobs,in_subtype,subtype,list )
      end if

      write(6,*)"<-- conmon_read_diag_file"
   end subroutine conmon_read_diag_file


   !-------------------------------
   !  NetCDF read routine
   !
   subroutine read_diag_file_nc( input_file, ctype,stype,intype,expected_nreal,nobs,in_subtype,subtype,list )

      !--- interface 
      character(100), intent(in) :: input_file
      character(3), intent(in)   :: ctype
      character(10), intent(in)  :: stype                   !! appears not to be used
      character(3), intent(in)   :: subtype                 !! appears not to be used
      integer, intent(in)        :: intype, expected_nreal, in_subtype
      integer, intent(out)       :: nobs
      type(list_node_t), pointer :: list

      !--- local vars
      type(list_node_t), pointer :: next => null()
      type(data_ptr) :: ptr
      integer                    :: ii, ierr, istatus, ftin, total_obs, id, idx

      data ftin / 11 /


      print *, ' ' 
      print *, '   --> read_diag_file_nc'

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

      call read_diag_file_all_nc( input_file, ftin, ctype,stype,intype,expected_nreal,nobs,in_subtype,subtype,list )


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
   !  netcdf read routine for all data types in netcdf files
   !
   subroutine read_diag_file_all_nc( input_file, ftin, ctype,stype,intype,expected_nreal,nobs,in_subtype,subtype,list )
  
      !--- interface 
      character(100), intent(in) :: input_file
      integer, intent(in)        :: ftin
      character(3), intent(in)   :: ctype
      character(10), intent(in)  :: stype                   !! appears not to be used
      character(3), intent(in)   :: subtype                 !! appears not to be used
      integer, intent(in)        :: intype, expected_nreal, in_subtype
      integer, intent(out)       :: nobs
      type(list_node_t), pointer :: list

      !--- local vars
      type(list_node_t), pointer :: next => null()
      type(data_ptr) :: ptr
      integer                    :: ii, ierr, istatus, total_obs, idx

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

      ! q type specific 
      real(r_single), dimension(:), allocatable    :: Forecast_Saturation_Spec_Hum    !  (obs)

      ! t type specific
      real(r_single), dimension(:), allocatable    ::  Data_Pof                       !  (obs) 
      real(r_single), dimension(:), allocatable    ::  Data_Vertical_Velocity         !  (obs)
      real(r_single), dimension(:,:), allocatable  ::  Bias_Correction_Terms          !  (nobs, Bias_Correction_Terms_arr_dim)


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
      !
      !    This may look like overkill with a check on each variable
      !    name, but due to the genius of the ncdiag library, a
      !    failure on these read operations is fatal, because, reasons
      !    I guess.  Thus, this abundance of caution.
      !
      if( nc_diag_read_check_var( 'Station_ID' )) then
         call nc_diag_read_get_var( ftin, 'Station_ID', Station_ID )
      else
         print *, 'ERROR:  unable to read Station_ID'
         ierr=2
      end if 

      if( nc_diag_read_check_var( 'Station_ID' )) then
         call nc_diag_read_get_var( ftin, 'Observation_Class', Observation_Class )
      else
         print *, 'ERROR:  unable to read Observation_Class'
         ierr=3
      end if 

      if( nc_diag_read_check_var( 'Observation_Type' )) then
         call nc_diag_read_get_var( ftin, 'Observation_Type', Observation_Type )
      else
         print *, 'ERROR:  unable to read Observation_Type'
         ierr=4
      end if 

      if( nc_diag_read_check_var( 'Observation_Subtype' )) then
         call nc_diag_read_get_var( ftin, 'Observation_Subtype', Observation_Subtype )
      else
         print *, 'ERROR:  unable to read Observation_Subtype'
         ierr=5
      end if 

      if( nc_diag_read_check_var( 'Latitude' )) then
         call nc_diag_read_get_var( ftin, 'Latitude', Latitude )
      else
         print *, 'ERROR:  unable to read Latitude'
         ierr=6
      end if 

      if( nc_diag_read_check_var( 'Longitude' )) then
         call nc_diag_read_get_var( ftin, 'Longitude', Longitude )
      else
         print *, 'ERROR:  unable to read Longitude'
         ierr=7
      end if 

      if( nc_diag_read_check_var( 'Station_Elevation' )) then
         call nc_diag_read_get_var( ftin, 'Station_Elevation', Station_Elevation )
      else
         print *, 'ERROR:  unable to read Station_Elevation'
         ierr=8
      end if 

      if( nc_diag_read_check_var( 'Pressure' )) then
         call nc_diag_read_get_var( ftin, 'Pressure', Pressure )
      else
         print *, 'ERROR:  unable to read Pressure'
         ierr=9
      end if 

      if( nc_diag_read_check_var( 'Height' )) then
         call nc_diag_read_get_var( ftin, 'Height', Height )
      else
         print *, 'ERROR:  unable to read Height'
         ierr=10
      end if 

      if( nc_diag_read_check_var( 'Time' )) then
         call nc_diag_read_get_var( ftin, 'Time', Time )
      else
         print *, 'ERROR:  unable to read Time'
         ierr=11
      end if 

      if( nc_diag_read_check_var( 'Prep_QC_Mark' )) then
         call nc_diag_read_get_var( ftin, 'Prep_QC_Mark', Prep_QC_Mark )
      else
         print *, 'ERROR:  unable to read Prep_QC_Mark'
         ierr=12
      end if 

      if( nc_diag_read_check_var( 'Nonlinear_QC_Var_Jb' )) then
         call nc_diag_read_get_var( ftin, 'Nonlinear_QC_Var_Jb', Nonlinear_QC_Var_Jb )
      else
         print *, 'ERROR:  unable to read Nonlinear_QC_Var_Jb'
         ierr=13
      end if 

      if( nc_diag_read_check_var( 'Nonlinear_QC_Rel_Wgt' )) then
         call nc_diag_read_get_var( ftin, 'Nonlinear_QC_Rel_Wgt', Nonlinear_QC_Rel_Wgt )
      else
         print *, 'ERROR:  unable to read Nonlinear_QC_Rel_Wgt'
         ierr=14
      end if 

      if( nc_diag_read_check_var( 'Prep_Use_Flag' )) then
         call nc_diag_read_get_var( ftin, 'Prep_Use_Flag', Prep_Use_Flag )
      else
         print *, 'ERROR:  unable to read Prep_Use_Flag'
         ierr=14
      end if 

      if( nc_diag_read_check_var( 'Analysis_Use_Flag' )) then
         call nc_diag_read_get_var( ftin, 'Analysis_Use_Flag', Analysis_Use_Flag )
      else
         print *, 'ERROR:  unable to read Analysis_Use_Flag'
         ierr=15
      end if 

      if( nc_diag_read_check_var( 'Errinv_Input' )) then
         call nc_diag_read_get_var( ftin, 'Errinv_Input', Errinv_Input )
      else
         print *, 'ERROR:  unable to read Errinv_Input'
         ierr=16
      end if 

      if( nc_diag_read_check_var( 'Errinv_Adjust' )) then
         call nc_diag_read_get_var( ftin, 'Errinv_Adjust', Errinv_Adjust )
      else
         print *, 'ERROR:  unable to read Errinv_Adjust'
         ierr=17
      end if 

      if( nc_diag_read_check_var( 'Errinv_Final' )) then
         call nc_diag_read_get_var( ftin, 'Errinv_Final', Errinv_Final )
      else
         print *, 'ERROR:  unable to read Errinv_Final'
         ierr=18
      end if 

      if( nc_diag_read_check_var( 'Observation' )) then
         call nc_diag_read_get_var( ftin, 'Observation', Observation )
      else
         print *, 'ERROR:  unable to read Observation'
         ierr=19
      end if 

      if( nc_diag_read_check_var( 'Obs_Minus_Forecast_adjusted' )) then
         call nc_diag_read_get_var( ftin, 'Obs_Minus_Forecast_adjusted', Obs_Minus_Forecast_adjusted )
      else
         print *, 'ERROR:  unable to read Obs_Minus_Forecast_adjusted'
         ierr=20
      end if 

      if( nc_diag_read_check_var( 'Obs_Minus_Forecast_unadjusted' )) then
         call nc_diag_read_get_var( ftin, 'Obs_Minus_Forecast_unadjusted', Obs_Minus_Forecast_unadjusted )
      else
         print *, 'ERROR:  unable to read Obs_Minus_Forecast_unadjusted'
         ierr=21
      end if 

      select case ( trim( adjustl( ctype ) ) )
   
         case ( 'ps' ) 
         case ( 'q'  )
            if( nc_diag_read_check_var( 'Forecast_Saturation_Spec_Hum' )) then
               call nc_diag_read_get_var( ftin, 'Forecast_Saturation_Spec_Hum', Forecast_Saturation_Spec_Hum )
            else
               print *, 'ERROR:  unable to read Forecast_Saturation_Spec_Hum'
               ierr=22
            end if 

         case ( 't'  )
            if( nc_diag_read_check_var( 'Data_Pof' )) then
               call nc_diag_read_get_var( ftin, 'Data_Pof', Data_Pof )
            else
               print *, 'ERROR:  unable to read Data_Pof'
               ierr=23
            end if 

            if( nc_diag_read_check_var( 'Bias_Correction_Terms' )) then
               call nc_diag_read_get_var( ftin, 'Bias_Correction_Terms', Bias_Correction_Terms )
            else
               print *, 'ERROR:  unable to read Bias_Correction_Terms'
               ierr=24
            end if 

         case ( 'uv'  )
         case default
            print *, 'ERROR:  unmatched ctype!'

      end select
      

      nobs = 0 
      do ii = 1, total_obs

         !------------------------------------------
         !  If type and subtype match add this obs 
         !  to the next list element (ptr%p).
         !
         if( Observation_Type(ii) == intype .AND. Observation_Subtype(ii) == in_subtype)  then 

            nobs=nobs+1

            !---------------------------------------------
            ! Allocate a new data element and load
            !
            print *, 'Allocating new data element'

            allocate(ptr%p)
            ptr%p%stn_id = Station_ID( ii )
            print *, 'ptr%p%stn_id = ', ptr%p%stn_id

            do idx=1,max_rdiag_reals
               ptr%p%rdiag( idx ) = 0.00
            end do

            ptr%p%rdiag(  1 ) = Observation_Type( ii )   
            ptr%p%rdiag(  2 ) = Observation_Subtype( ii )   
            ptr%p%rdiag(  3 ) = Latitude( ii )   
            ptr%p%rdiag(  4 ) = Longitude( ii )   
            ptr%p%rdiag(  5 ) = Station_Elevation( ii )   
            ptr%p%rdiag(  6 ) = Pressure( ii )
            ptr%p%rdiag(  7 ) = Height( ii )
            ptr%p%rdiag(  8 ) = Time( ii )
            ptr%p%rdiag(  9 ) = Prep_QC_Mark( ii )
            ptr%p%rdiag( 10 ) = Nonlinear_QC_Var_Jb( ii )
            ptr%p%rdiag( 11 ) = Prep_Use_Flag( ii )
            ptr%p%rdiag( 12 ) = Analysis_Use_Flag( ii )
            ptr%p%rdiag( 13 ) = Nonlinear_QC_Rel_Wgt( ii )
            ptr%p%rdiag( 14 ) = Errinv_Input( ii )
            ptr%p%rdiag( 15 ) = Errinv_Adjust( ii )
            ptr%p%rdiag( 16 ) = Errinv_Final( ii )
            ptr%p%rdiag( 17 ) = Observation( ii )
            ptr%p%rdiag( 18 ) = Obs_Minus_Forecast_adjusted( ii )
            ptr%p%rdiag( 19 ) = Obs_Minus_Forecast_unadjusted( ii )

            select case ( trim( adjustl( ctype ) ) )
   
               case ( 'ps' ) 

               case ( 'q'  )
                  ptr%p%rdiag( 20 ) = Forecast_Saturation_Spec_Hum( ii )

               case ( 't'  )
                  ptr%p%rdiag( 20 ) = Data_Pof( ii )

!  MISSING data vertical velocity term in the diag file!                 
 
!       rdiagbuf(20,ii) = data(ipof,i)       ! data pof
!       rdiagbuf(21,ii) = data(ivvlc,i)      ! data vertical velocity
!       do j=1,npredt
!          rdiagbuf(21+j,ii) = predbias(j)
!       end do

!        float Bias_Correction_Terms(nobs, Bias_Correction_Terms_arr_dim) ;

               case ( 'uv'  )
               case default
                  print *, 'ERROR:  unmatched ctype!'

            end select
     
 
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


            print *, 'Station_ID(ii)           : ', Station_ID(ii)
            print *, 'Observation_Class(ii)    : ', Observation_Class(ii)
            print *, 'Latitude(ii)             : ', Latitude(ii)
            print *, 'Longitude(ii)            : ', Longitude(ii)
            print *, 'Station_Elevation(ii)    : ', Station_Elevation(ii)
            print *, 'Time(ii)                 : ', Time(ii)
            print *, 'Prep_QC_Mark(ii)         : ', Prep_QC_Mark(ii)
            print *, 'Prep_Use_Flag(ii)        : ', Prep_Use_Flag(ii)
            print *, 'Nonlinear_QC_Var_Jb      : ', Nonlinear_QC_Var_Jb(ii)
            print *, 'Nonlinear_QC_Rel_Wgt(ii) : ', Nonlinear_QC_Rel_Wgt(ii)
            print *, 'Analysis_Use_Flag(ii)    : ', Analysis_Use_Flag(ii)
            print *, 'Errinv_Input(ii)         : ', Errinv_Input(ii)
            print *, 'Errinv_Adjust(ii)        : ', Errinv_Adjust(ii)
            print *, 'Errinv_Final(ii)         : ', Errinv_Final(ii)
            print *, 'Observation(ii)          : ', Observation(ii)
            print *, 'Obs_Minus_Forecast_adjusted(ii)          : ', Obs_Minus_Forecast_adjusted(ii)
            print *, 'Obs_Minus_Forecast_unadjusted(ii)        : ', Obs_Minus_Forecast_unadjusted(ii)

            select case ( trim( adjustl( ctype ) ) )
   
               case ( 'ps' ) 
               case ( 'q'  )
                  print *, 'Forecast_Saturation_Spec_Hum(ii)        : ', Forecast_Saturation_Spec_Hum(ii)

               case ( 't'  )
                  print *, 'Data_Pof( ii )                          : ', Data_Pof( ii )
!                 float Bias_Correction_Terms(nobs, Bias_Correction_Terms_arr_dim)

               case ( 'uv'  )
               case default
                  print *, 'ERROR:  unmatched ctype!'

            end select
         end if

      end do

      if( allocated( Station_ID                    )) deallocate( Station_ID                    )
      if( allocated( Observation_Class             )) deallocate( Observation_Class             )
      if( allocated( Observation_Type              )) deallocate( Observation_Type              )
      if( allocated( Observation_Subtype           )) deallocate( Observation_Subtype           )
      if( allocated( Latitude                      )) deallocate( Latitude                      )
      if( allocated( Longitude                     )) deallocate( Longitude                     )
      if( allocated( Station_Elevation             )) deallocate( Station_Elevation             )
      if( allocated( Time                          )) deallocate( Time                          )
      if( allocated( Prep_QC_Mark                  )) deallocate( Prep_QC_Mark                  )
      if( allocated( Prep_Use_Flag                 )) deallocate( Prep_Use_Flag                 )
      if( allocated( Nonlinear_QC_Var_Jb           )) deallocate( Nonlinear_QC_Var_Jb           )
      if( allocated( Nonlinear_QC_Rel_Wgt          )) deallocate( Nonlinear_QC_Rel_Wgt          )
      if( allocated( Analysis_Use_Flag             )) deallocate( Analysis_Use_Flag             )
      if( allocated( Errinv_Input                  )) deallocate( Errinv_Input                  )
      if( allocated( Errinv_Adjust                 )) deallocate( Errinv_Final                  )
      if( allocated( Errinv_Final                  )) deallocate( Errinv_Final                  )
      if( allocated( Observation                   )) deallocate( Observation                   )
      if( allocated( Obs_Minus_Forecast_adjusted   )) deallocate( Obs_Minus_Forecast_adjusted   )
      if( allocated( Obs_Minus_Forecast_unadjusted )) deallocate( Obs_Minus_Forecast_unadjusted )
      if( allocated( Forecast_Saturation_Spec_Hum  )) deallocate( Forecast_Saturation_Spec_Hum  )
      if( allocated( Data_Pof                      )) deallocate( Data_Pof                      )
      if( allocated( Bias_Correction_Terms         )) deallocate( Bias_Correction_Terms         )

      print *, ' '
      print *, '      <-- read_diag_file_ps_nc'

   end subroutine read_diag_file_all_nc


 
   !---  binary read routine
   !
   subroutine read_diag_file_bin( input_file, ctype,stype,intype,expected_nreal,nobs,in_subtype,subtype,list )

      !--- interface 
      character(100), intent(in) :: input_file
      character(3), intent(in)   :: ctype
      character(10), intent(in)  :: stype                   !! appears not to be used
      character(3), intent(in)   :: subtype                 !! appears not to be used
      integer, intent(in)        :: intype, expected_nreal, in_subtype
      integer, intent(out)       :: nobs
      type(list_node_t), pointer :: list


      !--- local vars
      type(list_node_t), pointer :: next => null()
      type(data_ptr) :: ptr
 
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
      print *, '            stype            = ', stype
      print *, '            subtype          = ', subtype
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
         if( trim(dtype) == trim(ctype) .and. file_nreal /= expected_nreal ) then
            print *, 'matched observation type:',dtype,' file_nreal=', file_nreal
            exit 
         endif

         !--------------------------------------------- 
         ! skip to next iteration if types don't match
         !
         if(trim(dtype) /= trim(ctype))  then
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
            if(file_itype == intype .AND. file_subtype == in_subtype)  then 

               nobs=nobs+1

               !---------------------------------------------
               ! Allocate a new data element and load
               !
!               print *, 'Allocating new data element'

               allocate(ptr%p)
               ptr%p%stn_id = cdiag(i)
               print *, 'stn_id = ', cdiag(i)

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
