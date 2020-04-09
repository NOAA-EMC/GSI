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
   public :: read_cnvstat_file


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


   subroutine read_cnvstat_file( ctype,stype,intype,target_nreal,nobs,isubtype,subtype,list )

      !--- interface 
      character(3), intent(in)   :: ctype
      character(10), intent(in)  :: stype                   !! appears not to be used
      character(3), intent(in)   :: subtype                 !! appears not to be used
      integer, intent(in)        :: intype, target_nreal, isubtype
      integer, intent(out)       :: nobs
      type(list_node_t), pointer :: list


      write(6,*)' --> read_cnvstat_file'

      if ( netcdf ) then
         write(6,*) ' call nc read subroutine'
      else
         call read_conv2grads( ctype,stype,intype,target_nreal,nobs,isubtype,subtype,list )
      end if

      write(6,*)" <-- read_cnvstat_file"
   end subroutine read_cnvstat_file



   !--binary read
   subroutine read_conv2grads( ctype,stype,intype,target_nreal,nobs,isubtype,subtype,list )

      !--- interface 
      character(3), intent(in)   :: ctype
      character(10), intent(in)  :: stype                   !! appears not to be used
      character(3), intent(in)   :: subtype                 !! appears not to be used
      integer, intent(in)        :: intype, target_nreal, isubtype
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

      integer nchar,file_nreal,i,ii,mype,idate,iflag,itype,iscater,igrads
      integer lunin,lunot,ldtype,jsubtype
      integer idx,ioff02

      data lunin / 11 /

      nobs=0
      print *, '--> read_conv2grads'
      print *, '     itype, isubtype = ', itype, isubtype

      open(lunin,file='conv_diag',form='unformatted')  
      rewind(lunin)

      read(lunin) idate
      print *, 'idate=',idate 

      loopd: do  

         read(lunin,IOSTAT=iflag) dtype,nchar,file_nreal,ii,mype,ioff02
!         print *, 'iflag from header read = ', iflag
         if( iflag /= 0 ) exit loopd

         if( trim(dtype) == trim(ctype) .and. file_nreal /= target_nreal ) then
            print *, 'observation type:',dtype,' file_nreal=', file_nreal
            exit 
         endif

         if(trim(dtype) /= trim(ctype))  then
            cycle
         endif

         allocate(cdiag(ii),rdiag(file_nreal,ii))
         read(lunin,IOSTAT=iflag) cdiag,rdiag
         if( iflag /= 0 ) then
!            print *, 'iflag from cdiag,rdiag read = ', iflag
            deallocate( cdiag,rdiag )
            exit loopd
         end if

         do i=1,ii
            itype = int(rdiag(1,i)) 
            jsubtype = int(rdiag(2,i)) 

            if( itype == intype .AND. itype == 245 ) then
               print *, 'itype == intype ', itype, intype
               print *, 'jsubtype == isubtype ', jsubtype, isubtype
            end if 
         
!            if( jsubtype == isubtype ) then
!               print *, 'and jsubtype == isubtype ', itype, intype
!            end if

            if(itype == intype .AND. jsubtype == isubtype)  then 
!            if( itype == intype )  then 
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
      print *, '<-- read_conv2grads'

      return 
   end subroutine read_conv2grads



end module conmon_read_diag
