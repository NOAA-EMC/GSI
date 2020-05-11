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
   use nc_diag_read_mod, only: nc_diag_read_init, nc_diag_read_close, &
                           nc_diag_read_get_dim, nc_diag_read_get_global_attr, &
                           nc_diag_read_get_var_names, &
                           nc_diag_read_get_global_attr_names, &
                           nc_diag_read_get_var

   use ncdr_vars, only:    nc_diag_read_check_var

   !--- implicit ---!
   implicit none
 

   !--- public & private ---!
   private
  
   public :: set_netcdf_read 
   public :: conmon_read_diag_file


   !--- common data structures ---!
   logical,save                  :: netcdf           = .false.



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



   !--- NetCDF read routine
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


      print *, '   --> read_diag_file_nc'
      nobs=0


      print *, '   <-- read_diag_file_nc'

   end subroutine read_diag_file_nc
   
   

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
      print *, '     ctype            = ', ctype
      print *, '     stype            = ', stype
      print *, '     subtype          = ', subtype
      print *, '     intype, in_subtype = ', intype, in_subtype
      print *, '     expected_nreal     = ', expected_nreal


      !--- open diag file
      open(lunin, file=input_file, form='unformatted')  
      rewind(lunin)

      read(lunin) idate
      print *, 'idate=',idate 

      loopd: do  

         !--- read obs header
         read(lunin,IOSTAT=iflag) dtype,nchar,file_nreal,ii,mype,ioff02
!         print *, 'iflag from header read = ', iflag
!         print *, 'dtype      = ', dtype
!         print *, 'nchar      = ', nchar
!         print *, 'file_nreal = ', file_nreal
!         print *, 'ii         = ', ii
!         print *, 'mype       = ', mype
!         print *, 'ioff02     = ', ioff02


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
!                  print *, 'Initializing list with data:', ptr%p
                  next => list

               else
                  !-------------------------------------------------
                  ! Insert subsequent nodes into the list
                  !
!                  print *, 'Inserting node with data:', ptr%p
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



end module conmon_read_diag
