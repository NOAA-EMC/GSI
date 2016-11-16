

module variable_types

!-------------------------------------------------------------------------- 
! Purpose: Define the objects of the new features of gen_be 
!
! History:
!
! Date     Author & Comment
! -------- ----------------
! dd/mm/yy Gael Descombes
!          Initial version (01/07/2012)
!          Variable type used in GEN_BE V2.0 are based on MPAS (NCAR Team
!          developers) and are subsequently modified  
!--------------------------------------------------------------------------

   !---------------------------------------------------------------
   ! Define the basics
   !---------------------------------------------------------------

   ! Derived type describing info for doing I/O specific to a field
   type io_info
      character (len=1024) :: fieldName
      integer, dimension(4) :: start
      integer, dimension(4) :: count
      logical :: input
      logical :: output
      integer :: ID
   end type io_info

   ! Derived type for storing fields
   type field3DReal
      real(kind=8) , allocatable, dimension(:,:,:) :: array
      !real (kind=8), dimension(:,:,:), pointer :: array
      type (io_info), pointer :: ioinfo
   end type field3DReal

   ! Derived type for storing fields
   type field2DReal
      real(kind=8) , allocatable, dimension(:,:) :: array
      !real (kind=8), dimension(:,:), pointer :: array
      type (io_info), pointer :: ioinfo
   end type field2DReal

   ! Derived type for storing fields
   type field1DReal
      !real (kind=8), dimension(:), pointer :: array
      real(kind=8) , allocatable , dimension(:) :: array
      type (io_info), pointer :: ioinfo
   end type field1DReal

   ! Derived type for storing fields
   type field0DReal
      real(kind=8) :: scalar
      type (io_info), pointer :: ioinfo
   end type field0DReal

   ! Derived type for storing fields
   type field3DInteger
!      integer, dimension(:,:,:), pointer :: array
      integer, allocatable, dimension(:,:,:) :: array
      type (io_info), pointer :: ioinfo
   end type field3DInteger

   ! Derived type for storing fields
   type field2DInteger
!      integer, dimension(:,:), pointer :: array
      integer, allocatable, dimension(:,:) :: array
      type (io_info), pointer :: ioinfo
   end type field2DInteger

   ! Derived type for storing fields
   type field1DInteger
!      integer, dimension(:), pointer :: array
      integer(kind=4), allocatable, dimension(:) :: array
!      integer, allocatable :: array(:)
      type (io_info), pointer :: ioinfo
   end type field1DInteger

   ! Derived type for storing fields
   type field1DChar
      character(len=1024), dimension(:), pointer :: array
      type (io_info), pointer :: ioinfo
   end type field1DChar

   !-----------------------------------------------------------------
   ! Derived type for storing grid meta-data
   !-----------------------------------------------------------------
   type mesh_type
      integer :: Dim1
      integer :: Dim2
      integer :: Dim3
      integer :: Dim1u, Dim2u, Dim1v, Dim2v 
      type (field0DReal), pointer :: ds
      type (field2DReal), pointer :: lat
      type (field2DReal), pointer :: lon
      type (field3DReal), pointer :: hgt
      type (field2DReal), pointer :: mapfac_u
      type (field2DReal), pointer :: mapfac_v
      type (field2DReal), pointer :: mapfac_m
      type (field1DReal), pointer :: znu
   end type mesh_type

   type bins_type
      integer :: bin_type
      integer :: num_bins, num_bins2d
      type (field1DInteger), pointer :: bin2d_pts
      type (field1DInteger), pointer :: bin_pts
      type (field2DInteger), pointer :: bin2d
      type (field3DInteger), pointer :: bin
!      type (field3DInteger), pointer :: bin3d_pts
!      type (field3DInteger), pointer :: bin3d
      type (field3DInteger), pointer :: ij_counter_rc
      type (state_multilevel_type0D), pointer :: bin_type2_param

      logical :: do_fil
      logical :: do_bin
      type (field3DInteger), pointer :: filter3d
      type (field3DInteger), pointer :: counter3d
      type (field2DInteger), pointer :: counter2d
 
   end type bins_type

   !type filter_type
   !   logical :: do_fil
   !   logical :: do_bin
   !   type (field3DInteger), pointer :: filter3d
   !   type (field3DInteger), pointer :: counter3d
   !   type (field2DInteger), pointer :: counter2d
   !end type filter_type

   !----------------------------------------------------------
   ! Define state and matrix state
   !----------------------------------------------------------
   type fieldnDReal 
      type (field0DReal), pointer :: field0d
      type (field1DReal), pointer :: field1d
      type (field2DReal), pointer :: field2d
      type (field3DReal), pointer :: field3d
      !type (io_info), pointer :: ioinfo    
   end type fieldnDReal 

   type fieldnD_type
      type (fieldnDReal), pointer :: field
      integer :: IDdim
   end type fieldnD_type
  
   type fieldnDInt
      type (field1DInteger), pointer :: field1d
      type (field2DInteger), pointer :: field2d
      type (field3DInteger), pointer :: field3d
      !type (io_info), pointer :: ioinfo    
   end type fieldnDInt

   type fieldnDInt_type
      type (fieldnDInt), pointer :: field
      integer :: IDdim
   end type fieldnDInt_type

   type state_type
      character (len=10) :: date
      character (len=10) :: name ! bins or grid 
      type (fieldnD_type), dimension(:), pointer :: num
      integer :: nvar
   end type state_type

   type state_matrix_type
      type (fieldnD_type), dimension(:,:), pointer :: num2d
      integer :: nvar
   end type state_matrix_type

   type state_type0D
      type (field0DReal), pointer :: field
   end type state_type0D

   type state_multilevel_type0D
      integer :: nvar
      type (state_type0D), dimension(:), pointer :: num
   end type state_multilevel_type0D

   !----------------------------------------------------------
   ! Define domain
   !----------------------------------------------------------
   type domain_type
     character (len=32) :: model, application
     type (state_type) , pointer :: state
     type (mesh_type) , pointer :: mesh
   end type domain_type

   !----------------------------------------------------------
   ! Eigen vector
   !----------------------------------------------------------
   type eigen_type
     type (fieldnD_type), pointer :: val
     type (fieldnD_type), pointer :: vec
   end type eigen_type

   !----------------------------------------------------------
   ! general to write or in the netcdf
   !----------------------------------------------------------
   integer :: StrLen = 32 
   integer :: DateStrLen = 10 

   !-----------------------------------------------------------------------
   ! Declaration of interface
   !-----------------------------------------------------------------------

   interface allocate_field
      module procedure allocate_field3d
      module procedure allocate_field2d
      module procedure allocate_field1d
      module procedure allocate_field0d
      module procedure allocate_field3dInteger
      module procedure allocate_field2dInteger
      module procedure allocate_field1dInteger
      module procedure allocate_field1dchar
   end interface allocate_field

   interface deallocate_field
      module procedure deallocate_field3d
      module procedure deallocate_field2d
      module procedure deallocate_field1d
      module procedure deallocate_field0d
      module procedure deallocate_field3dInteger
      module procedure deallocate_field2dInteger
      module procedure deallocate_field1dInteger
      module procedure deallocate_field1dchar
   end interface deallocate_field

   contains

!=============================================================================
! ALLOCATE SECTION
!=============================================================================

   !-----------------------------------------------------------------------
   ! Allocation all fields
   !-----------------------------------------------------------------------

   subroutine allocate_field3d(field3d, fieldName, Dim1, Dim2, Dim3)

      implicit none
      type (field3DReal) , intent(inout), pointer :: field3d
      character (len=*), intent(in) :: fieldName
      integer, intent(in) :: Dim1, Dim2, Dim3

      allocate( field3d )
      allocate( field3d % ioinfo )
      allocate( field3d % array(Dim1,Dim2,Dim3) )
      field3d % ioinfo % fieldName = trim(fieldName)
      field3d % ioinfo % ID = 1 
      field3d % array = 0.
      field3d % ioinfo % start(1) = 1
      field3d % ioinfo % start(2) = 1
      field3d % ioinfo % start(3) = 1
      field3d % ioinfo % count(1) = Dim1
      field3d % ioinfo % count(2) = Dim2
      field3d % ioinfo % count(3) = Dim3

   end subroutine allocate_field3d

   subroutine allocate_field2d(field2d, fieldName, Dim1, Dim2)

      implicit none
      type (field2DReal) , intent(inout), pointer :: field2d
      character (len=*), intent(in) :: fieldName
      integer, intent(in) :: Dim1, Dim2

      allocate( field2d )
      allocate( field2d % ioinfo )
      allocate( field2d % array(Dim1,Dim2) )
      field2d % ioinfo % fieldName = trim(fieldName)
      field2d % array = 0.
      field2d % ioinfo % start(1) = 1
      field2d % ioinfo % start(2) = 1
      field2d % ioinfo % count(1) = Dim1
      field2d % ioinfo % count(2) = Dim2

   end subroutine allocate_field2d

   subroutine allocate_field1d(field1d, fieldName, Dim1)

      implicit none
      type (field1DReal) , intent(inout), pointer :: field1d
      character (len=*), intent(in) :: fieldName
      integer, intent(in) :: Dim1

      allocate( field1d )
      allocate( field1d % ioinfo )
      allocate( field1d % array(Dim1) )
      field1d % ioinfo % fieldName = trim(fieldName)
      field1d % array = 0.
      field1d % ioinfo % start(1) = 1
      field1d % ioinfo % count(1) = Dim1

   end subroutine allocate_field1d

   subroutine allocate_field0d(field0d, fieldName)

      implicit none
      type (field0DReal) , intent(inout), pointer :: field0d
      character (len=*), intent(in) :: fieldName

      allocate( field0d )
      allocate( field0d % ioinfo )
      field0d % ioinfo % fieldName = trim(fieldName)
      field0d % scalar = 0.
      field0d % ioinfo % start(1) = 1
      field0d % ioinfo % count(1) = 1

   end subroutine allocate_field0d

   subroutine allocate_field3dInteger(field3d, fieldName, Dim1, Dim2, Dim3)

      implicit none
      type (field3DInteger) , intent(inout), pointer :: field3d
      character (len=*), intent(in) :: fieldName
      integer, intent(in) :: Dim1, Dim2, Dim3

      allocate( field3d )
      allocate( field3d % ioinfo )
      allocate( field3d % array(Dim1,Dim2,Dim3) )
      field3d % ioinfo % fieldName = trim(fieldName)
      field3d % array = 0
      field3d % ioinfo % start(1) = 1
      field3d % ioinfo % start(2) = 1
      field3d % ioinfo % start(3) = 1
      field3d % ioinfo % count(1) = Dim1
      field3d % ioinfo % count(2) = Dim2
      field3d % ioinfo % count(3) = Dim3

   end subroutine allocate_field3dInteger

   subroutine allocate_field2dInteger(field2d, fieldName, Dim1, Dim2)

      implicit none
      type (field2DInteger) , intent(inout), pointer :: field2d
      character (len=*), intent(in) :: fieldName
      integer, intent(in) :: Dim1, Dim2

      allocate( field2d )
      allocate( field2d % ioinfo )
      allocate( field2d % array(Dim1,Dim2) )
      field2d % ioinfo % fieldName = trim(fieldName)
      field2d % array = 0
      field2d % ioinfo % start(1) = 1
      field2d % ioinfo % start(2) = 1
      field2d % ioinfo % count(1) = Dim1
      field2d % ioinfo % count(2) = Dim2

   end subroutine allocate_field2dInteger

   subroutine allocate_field1dInteger(field1d, fieldName, Dim1)

      implicit none
      type (field1DInteger) , intent(inout), pointer :: field1d
      character (len=*), intent(in) :: fieldName
      integer, intent(in) :: Dim1

      allocate( field1d )
      allocate( field1d % ioinfo )
      allocate( field1d % array(Dim1) )
      field1d % ioinfo % fieldName = trim(fieldName)
      field1d % array = 0
      field1d % ioinfo % start(1) = 1
      field1d % ioinfo % count(1) = Dim1

   end subroutine allocate_field1dInteger

   subroutine allocate_field1dChar(field1d, fieldName, Dim1)

      implicit none
      type (field1DChar) , intent(inout), pointer :: field1d
      character (len=*), intent(in) :: fieldName
      integer, intent(in) :: Dim1

      allocate( field1d )
      allocate( field1d % ioinfo )
      allocate( field1d % array(Dim1) )
      field1d % ioinfo % fieldName = trim(fieldName)
      field1d % array = ""
      field1d % ioinfo % start(1) = 1
      field1d % ioinfo % count(1) = Dim1

   end subroutine allocate_field1dChar
   
   subroutine allocate_fieldnd(field, fieldName, IvarDim, Dim1, Dim2, Dim3)

    implicit none
    type (fieldnDReal) , intent(inout), pointer :: field
    character (len=*), intent(in) :: fieldName
    integer, intent(in) :: Dim1, Dim2, Dim3, IvarDim

    allocate(field)
    nullify(field % field0d)
    nullify(field % field1d)
    nullify(field % field2d)
    nullify(field % field3d)

    if ( IvarDim == 0 ) then
       call allocate_field(field % field0d, fieldName)
    else if ( IvarDim == 1 ) then
       call allocate_field(field % field1d, fieldName, Dim1)
    else if ( IvarDim == 2 ) then
       call allocate_field(field % field2d, fieldName, Dim1, Dim2)
    else if ( IvarDim == 3 ) then
       call allocate_field(field % field3d, fieldName, Dim1, Dim2, Dim3)
    end if

    end subroutine allocate_fieldnd


    subroutine allocate_fieldndInt(field, fieldName, IvarDim, Dim1, Dim2, Dim3)

    implicit none
    type (fieldnDInt) , intent(inout), pointer :: field
    character (len=*), intent(in) :: fieldName
    integer, intent(in) :: Dim1, Dim2, Dim3, IvarDim

    allocate(field)
    nullify(field % field1d)
    nullify(field % field2d)
    nullify(field % field3d)

    if ( IvarDim == 1 ) then
       call allocate_field(field % field1d, fieldName, Dim1)
    else if ( IvarDim == 2 ) then
       call allocate_field(field % field2d, fieldName, Dim1, Dim2)
    else if ( IvarDim == 3 ) then
       call allocate_field(field % field3d, fieldName, Dim1, Dim2, Dim3)
    end if

    end subroutine allocate_fieldndInt


   !-----------------------------------------------------------------------
   ! Allocation all state
   !-----------------------------------------------------------------------

   subroutine allocate_state0d(state0D, nVar0D)

      implicit none
      type (state_multilevel_type0D), intent(inout), pointer :: state0D
      integer, intent(in) :: nVar0D
      integer :: i
      character (len=64) :: fieldName

      write (*,*) 'allocate_state 0d ',nVar0D
      allocate ( state0D )
      state0D % nvar = nVar0D
      fieldName = 'no_name'

      if ( nVar0D .gt. 0  ) then
         allocate( state0D % num(nVar0D) )
         do i=1, state0D % nvar
            call allocate_field(state0D % num(i) % field,fieldName)
         end do
      end if
     
   end subroutine allocate_state0d


   subroutine allocate_state(state, Dim1, Dim2, Dim3, nvar, cv_list, vardim_list)

     implicit none
     integer, intent(in) :: nvar, Dim1, Dim2, Dim3
     type (state_type), intent(out), pointer :: state
     character (len=32), dimension(:),intent(in) :: cv_list
     integer, dimension(:), intent(in) :: vardim_list
     integer :: vv
     character (len=32) :: varname
     real(kind=8) :: value

     allocate (state)
     allocate (state % num(nvar))
     state % nvar = nvar

     do vv = 1, nvar
         call allocate_fieldnd(state % num(vv) % field, trim(cv_list(vv)), vardim_list(vv), Dim1, Dim2, Dim3)
         state % num(vv) % IDdim = vardim_list(vv)
         write(*,*)'state allocate dimension ',vardim_list(vv),trim(cv_list(vv)),Dim1, Dim2, Dim3
     end do

     value = 0.0
     call initial_state(state, value)

   end subroutine  allocate_state


   subroutine allocate_state_bin(state, num_bins, num_bins2d, Dim3, nvar, cv_list, vardim_list)

     implicit none
     integer, intent(in) :: nvar, num_bins, num_bins2d, Dim3
     type (state_type), intent(out), pointer :: state
     character (len=*), dimension(:),intent(in) :: cv_list
     integer, dimension(:), intent(in) :: vardim_list
     integer :: vv
     character (len=32) :: varname
     real(kind=8) :: value

     allocate (state)
     allocate (state % num(nvar))
     state % nvar = nvar

     do vv = 1, nvar
         if ( vv.eq.1 ) then
            if (vardim_list(vv) .eq. 1 ) then
               call allocate_fieldnd(state % num(vv) % field, trim(cv_list(vv)), vardim_list(vv), num_bins, num_bins, num_bins)
            else if (vardim_list(vv) .eq. 3 ) then
               call allocate_fieldnd(state % num(vv) % field, trim(cv_list(vv)), vardim_list(vv), Dim3, Dim3, num_bins2d)
            else
               call allocate_fieldnd(state % num(vv) % field, trim(cv_list(vv)), vardim_list(vv), Dim3, num_bins2d, num_bins2d)
            end if
         else
            if (vardim_list(vv) .eq. 2 ) then
               call allocate_fieldnd(state % num(vv) % field, trim(cv_list(vv)), vardim_list(vv), Dim3, num_bins2d, num_bins2d)
            else if (vardim_list(vv) .eq. 1 ) then
               call allocate_fieldnd(state % num(vv) % field, trim(cv_list(vv)), vardim_list(vv), num_bins2d, num_bins2d, num_bins2d) 
            else if (vardim_list(vv) .eq. 3 ) then
                call allocate_fieldnd(state % num(vv) % field, trim(cv_list(vv)), vardim_list(vv), Dim3, Dim3, num_bins2d)
            end if 
         end if
         state % num(vv) % IDdim = vardim_list(vv)
         write(*,*)'state allocate dimension ',vardim_list(vv),trim(cv_list(vv)),num_bins, num_bins2d, Dim3
     end do

     value = 0.0
     call initial_state(state, value)

   end subroutine  allocate_state_bin


   subroutine initialize_state(state, state_ref, Dim1, Dim2, Dim3)

       implicit none
       integer, intent(in) :: Dim1, Dim2, Dim3
       type (state_type), intent(inout), pointer :: state
       type (state_type), intent(in), pointer :: state_ref
       integer :: vv

       write(*,*)'initialize_state',state_ref % nvar
       allocate (state)
       allocate (state % num(state_ref % nvar))
       state % nvar = state_ref % nvar
       state % date = state_ref % date

       do vv = 1, state % nvar
         write(*,*)'allocate vv = ',vv
         allocate(state % num(vv) % field)
         state % num(vv) % IDdim = state_ref % num(vv) % IDdim
         write(*,*)'state % num(vv) % IDdim ',state % num(vv) % IDdim
         nullify(state % num(vv) % field % field0d)
         nullify(state % num(vv) % field % field1d)
         nullify(state % num(vv) % field % field2d)
         nullify(state % num(vv) % field % field3d)
         write(*,*)'state_ref % num(vv) % IDdim ',state_ref % num(vv) % IDdim

         if ( state_ref % num(vv) % IDdim == 0 ) then
            call allocate_field(state%num(vv)%field%field0d,trim(state_ref%num(vv)%field%field0d%ioinfo%fieldName))
            state%num(vv)%field%field0d%scalar= 0.0
            state%num(vv)%field%field0d%ioinfo%ID = vv
         else if ( state_ref % num(vv) % IDdim == 1 ) then
            call allocate_field(state%num(vv)%field%field1d,trim(state_ref%num(vv)%field%field1d%ioinfo%fieldName), Dim1)
            state%num(vv)%field%field1d%array = 0.0
            state%num(vv)%field%field1d%ioinfo%ID = vv
         else if (  state_ref%num(vv)%IDdim == 2 ) then
            call allocate_field(state%num(vv) % field % field2d,trim(state_ref%num(vv)%field%field2d%ioinfo%fieldName), Dim1, Dim2)
            state%num(vv)%field%field2d%array = 0.0
            state %num(vv)%field%field2d%ioinfo%ID = vv
         else if ( state_ref % num(vv) % IDdim == 3 ) then
            call allocate_field(state%num(vv)%field%field3d,trim(state_ref%num(vv)%field%field3d%ioinfo%fieldName), Dim1, Dim2, Dim3)
            state%num(vv)%field%field3d%array = 0.0
            state%num(vv)%field%field3d%ioinfo%ID = vv
         end if
      end do

   end subroutine initialize_state

!   subroutine initialize_state_bin(state_bin, state, Dim1, Dim2)
!
!     implicit none
!     integer, intent(in) :: Dim1, Dim2
!     type (state_type), intent(inout), pointer :: state_bin
!     type (state_type), intent(in), pointer :: state
!     integer :: vv
!
!     write(*,*)'initialize_state_bin'
!     allocate (state_bin)
!     allocate (state_bin % num(state%nvar))
!     state_bin%nvar = state%nvar
!
!     do vv = 1, state%nvar
!         state_bin%num(vv)%IDdim = state%num(vv)%IDdim - 1
!         write(*,*)'state_bin Dim : ',state_bin%num(vv)%IDdim
!         if (  state_bin%num(vv)%IDdim == 1 ) then
!            call allocate_field(state_bin%num(vv)%field%field1d,trim(state%num(vv)%field%field2d%ioinfo%fieldName),Dim1)
!            state_bin%num(vv)%field%field1d%ioinfo%ID = vv
!         else if ( state_bin%num(vv)%IDdim == 2 ) then
!            call allocate_field(state_bin%num(vv)%field%field2d,trim(state%num(vv)%field%field3d%ioinfo%fieldName),Dim1,Dim2)
!            state_bin%num(vv)%field%field2d%ioinfo%ID = vv
!         end if
!     end do
!     write(*,*)'end of initialize_state_bin'
!
!   end subroutine initialize_state_bin


   subroutine allocate_state_matrix(matrix0, bins, Dim3, nvar, covar_ID, cv_list, prefix)

     implicit none
     integer, intent(in) :: nvar, Dim3
     type (state_matrix_type), intent(inout), pointer :: matrix0
     type (bins_type), intent(in), pointer :: bins
     character (len=32), dimension(:),intent(in) :: cv_list
     integer, dimension(:,:), intent(in) :: covar_ID
     character (len=*),intent(in),optional :: prefix
     integer ::  ii, jj, kk, num_bins, num_bins2d
     character (len=32) :: varname
     integer, allocatable, dimension(:) :: ncovar

     num_bins = bins%num_bins
     num_bins2d = bins%num_bins2d

     allocate(ncovar(1:nvar))
     allocate(matrix0)
     matrix0%nvar = nvar
     allocate(matrix0%num2d(1:nvar,1:nvar))

     do ii = 1, nvar
        do jj = 1, nvar
            matrix0 % num2d(ii,jj) % IDdim = -1
            allocate( matrix0 % num2d(ii,jj) % field )
            nullify ( matrix0 % num2d(ii,jj) % field % field0d )
            nullify ( matrix0 % num2d(ii,jj) % field % field1d )
            nullify ( matrix0 % num2d(ii,jj) % field % field2d )
            nullify ( matrix0 % num2d(ii,jj) % field % field3d )
        end do
     end do

     ncovar = 0
     
     do jj = 1, nvar-1
         do ii = jj+1, nvar
           if ( ncovar(jj).gt.0 ) then
              varname = trim(cv_list(jj))//'_u'
           else
              varname = trim(cv_list(jj))
           end if
           varname = trim(varname)//'_'//trim(cv_list(ii))
           if (present(prefix)) then
              varname = trim(prefix)//'_'//trim(varname)
           end if 
           if ( covar_ID(ii,jj) == 3 ) then
              write(*,*)'Allocate matrix varname dimension : ',trim(varname), covar_ID(ii,jj)
              call allocate_field( matrix0 % num2d(ii,jj) % field % field3d, trim(varname), Dim3, Dim3, num_bins2d)
              ncovar(ii) = ncovar(ii) + 1
           else if ( covar_ID(ii,jj) == 1 ) then
              write(*,*)'Allocate matrix varname dimension : ',trim(varname), covar_ID(ii,jj)
              call allocate_field( matrix0 % num2d(ii,jj) % field % field1d, trim(varname), num_bins)
              ncovar(ii) = ncovar(ii) + 1
           else if ( covar_ID(ii,jj) == 2 ) then
              write(*,*)'Allocate matrix varname dimension : ',trim(varname), covar_ID(ii,jj)
              call allocate_field( matrix0 % num2d(ii,jj) % field % field2d, trim(varname), Dim3, num_bins2d)
              ncovar(ii) = ncovar(ii) + 1
           end if
           matrix0 % num2d(ii,jj) % IDdim = covar_ID(ii,jj)
        end do
     end do

     deallocate(ncovar)

   end subroutine allocate_state_matrix

  
   !-----------------------------------------------------------------------
   ! Allocate all other types
   !-----------------------------------------------------------------------

   subroutine allocate_mesh(mesh, Dim1, Dim2, Dim3)

      implicit none
      type (mesh_type), intent(inout), pointer :: mesh
      integer, intent(in) :: Dim1, Dim2, Dim3 

      ! allocate mesh and dimensions
      allocate(mesh)
      mesh % Dim1 = Dim1
      mesh % Dim2 = Dim2
      mesh % Dim3 = Dim3
      ! allocate latitude
      call allocate_field(mesh % lat, 'lat', Dim1, Dim2)
      ! allocate longitude
      call allocate_field(mesh % lon, 'lon', Dim1, Dim2)
      ! allocate mapfac_m
      call allocate_field(mesh % mapfac_m, 'mapfac_m', Dim1, Dim2)
      ! allocate height
      call allocate_field(mesh % hgt, 'height', Dim1, Dim2, Dim3)
      call allocate_field(mesh % ds,'ds')
      ! model dependent, see module io_input_model for initialisation
      nullify(mesh%mapfac_u)
      nullify(mesh%mapfac_v)
      nullify(mesh%znu)
      mesh % Dim1u = 0 
      mesh % Dim2u = 0
      mesh % Dim1v = 0
      mesh % Dim2v = 0 

  end subroutine allocate_mesh


  subroutine allocate_bins(bins, Dim1, Dim2, Dim3, num_bins, num_bins2d)

      implicit none
      type (bins_type), intent(inout), pointer :: bins
      integer, intent(in) :: Dim1, Dim2, Dim3, num_bins, num_bins2d
      integer :: vv

     ! allocate bin and dimensions
     allocate(bins)
     bins % num_bins = num_bins
     bins % num_bins2d = num_bins2d
     call allocate_field(bins % bin,'bin',Dim1,Dim2,Dim3)
     call allocate_field(bins % bin2d,'bin2d',Dim1,Dim2)
     call allocate_field(bins % ij_counter_rc,'ij_counter_rc',Dim1,Dim2,num_bins2d)
     call allocate_field(bins % bin_pts,'bin_pts', num_bins)
     call allocate_field(bins % bin2d_pts,'bin2d_pts', num_bins2d)

     ! for option filtering
     bins%do_fil = .true.
     bins%do_bin = .true.
     call allocate_field(bins % counter3d,'count_filter3d_pts',num_bins2d,Dim3,Dim3)
     call allocate_field(bins % counter2d,'count_filter2d_pts',num_bins2d,Dim3) 
     call allocate_field(bins % filter3d,'filter3d',Dim1,Dim2,Dim3) 

     allocate(bins % bin_type2_param)
     call allocate_state0d(bins % bin_type2_param, 6)
     !write(*,*)'allocate bin bin_type2_param ',bins % bin_type2_param%nvar
!    GD pb
     bins % bin_type2_param%num(1)%field%ioinfo%fieldName = 'lat_min'
     bins % bin_type2_param%num(2)%field%ioinfo%fieldName = 'lat_max'
     bins % bin_type2_param%num(3)%field%ioinfo%fieldName = 'binwidth_lat'
     bins % bin_type2_param%num(4)%field%ioinfo%fieldName = 'hgt_min'
     bins % bin_type2_param%num(5)%field%ioinfo%fieldName = 'hgt_max'
     bins % bin_type2_param%num(6)%field%ioinfo%fieldName = 'binwidth_hgt'
     !write(*,*)'allocate bin bin_type2_param end'

  end subroutine allocate_bins


  subroutine allocate_domain(domain, Dim1, Dim2, Dim3, nvar, cv_list, vardim_list)

      implicit none
      type (domain_type), intent(inout), pointer :: domain
      integer, intent(in) :: Dim1, Dim2, Dim3, nvar
      character (len=32), dimension(:),intent(in) :: cv_list
      integer, dimension(:), intent(in) :: vardim_list

      allocate(domain)
      !allocate(domain%state)
      call allocate_state(domain%state, Dim1, Dim2, Dim3, nvar, cv_list, vardim_list)
      !allocate(domain%mesh)
      call allocate_mesh(domain%mesh, Dim1, Dim2, Dim3)

  end subroutine allocate_domain


  subroutine allocate_eigenvar(eigen, use_global_eofs,varname, IvarDim, Dim3, nums_bin2d)

      implicit none
      type (eigen_type), intent(inout), pointer :: eigen
      character (len=32), intent(in) :: varname
      integer, intent(in) :: IvarDim, Dim3, nums_bin2d
      logical, intent(in) :: use_global_eofs
      character (len=32) :: varname_tmp

      allocate(eigen)

      allocate(eigen%vec)
      allocate(eigen%vec%field)
      nullify (eigen%vec%field%field0d)
      nullify (eigen%vec%field%field1d)
      nullify (eigen%vec%field%field2d)
      nullify (eigen%vec%field%field3d)
      eigen%vec%IDdim = -1  

      allocate(eigen%val)
      allocate(eigen%val%field)
      nullify (eigen%val%field%field0d)
      nullify (eigen%val%field%field1d)
      nullify (eigen%val%field%field2d)
      nullify (eigen%val%field%field3d)
      eigen%val%IDdim = -1

      if ( IvarDim .eq. 3 ) then
         eigen%vec%IDdim = 3
         eigen%val%IDdim = 2
      else if ( IvarDim .eq. 2 ) then 
         eigen%vec%IDdim = 1 
         eigen%val%IDdim = 1
      end if
      if ( use_global_eofs ) then
         eigen%vec%IDdim = eigen%vec%IDdim - 1
         eigen%val%IDdim = eigen%val%IDdim - 1
      end if
      
      if ( eigen%vec%IDdim .eq. 3 ) then
         varname_tmp='eigen_vector_'//trim(varname)
         call allocate_field(eigen%vec%field%field3d,trim(varname_tmp),Dim3,Dim3,nums_bin2d)
         varname_tmp='eigen_value_'//trim(varname)
         call allocate_field(eigen%val%field%field2d,trim(varname_tmp),Dim3,nums_bin2d)
      else if ( eigen%vec%IDdim .eq. 2 ) then
         varname_tmp='eigen_vector_'//trim(varname)
         call allocate_field(eigen%vec%field%field2d,trim(varname_tmp),Dim3,Dim3)
         varname_tmp='eigen_value_'//trim(varname)
         call allocate_field(eigen%val%field%field1d,trim(varname_tmp),Dim3)
      else if ( eigen%vec%IDdim .eq. 1 ) then
         varname_tmp='eigen_vector_'//trim(varname)
         call allocate_field(eigen%vec%field%field1d,trim(varname_tmp),nums_bin2d)
         varname_tmp='eigen_value_'//trim(varname)
         call allocate_field(eigen%val%field%field1d,trim(varname_tmp),nums_bin2d) 
      else if ( eigen%vec%IDdim .eq. 0 ) then
         varname_tmp='eigen_vector_'//trim(varname)
         call allocate_field(eigen%vec%field%field0d,trim(varname_tmp))
         varname_tmp='eigen_value_'//trim(varname)
         call allocate_field(eigen%val%field%field0d,trim(varname_tmp))
      end if 

   end subroutine allocate_eigenvar


   subroutine allocate_dynamic_mask(bin_type, mask, mesh)

     implicit none

     type (fieldnDInt) , intent(out), pointer :: mask
     type (mesh_type), intent(in), pointer :: mesh
     integer, intent(in) :: bin_type
     integer :: vardim  

     if ( bin_type .eq. 7 ) then
        vardim = 2
        call allocate_fieldndInt(mask, 'vert_rain', vardim, mesh%Dim1, mesh%Dim2, mesh%Dim3)
     end if

   end subroutine  allocate_dynamic_mask


!=============================================================================
! DEALLOCATE SECTION
!=============================================================================

   !--------------------------------------------------------------
   ! deallocate all fields
   !--------------------------------------------------------------

    subroutine deallocate_field3d(field3d)

       implicit none
       type (field3DReal), intent(inout), pointer :: field3d

       deallocate( field3d % ioinfo )
       deallocate( field3d % array )
       deallocate( field3d )

    end subroutine deallocate_field3d

    subroutine deallocate_field2d(field2d)

       implicit none
       type (field2DReal), intent(inout), pointer :: field2d

       deallocate( field2d % ioinfo )
       deallocate( field2d % array )
       deallocate( field2d )

    end subroutine deallocate_field2d

    subroutine deallocate_field1d(field1d)

       implicit none
       type (field1DReal), intent(inout), pointer :: field1d

       deallocate( field1d % ioinfo )
       deallocate( field1d % array )
       deallocate( field1d )

    end subroutine deallocate_field1d

    subroutine deallocate_field0d(field0d)

       implicit none
       type (field0DReal), intent(inout), pointer :: field0d

       deallocate( field0d % ioinfo )
       deallocate( field0d )

    end subroutine deallocate_field0d

    subroutine deallocate_field3dInteger(field3d)

       implicit none
       type (field3DInteger), intent(inout), pointer :: field3d

       deallocate( field3d % ioinfo )
       deallocate( field3d % array )
       deallocate( field3d )

    end subroutine deallocate_field3dInteger

    subroutine deallocate_field2dInteger(field2d)

       implicit none
       type (field2DInteger), intent(inout), pointer :: field2d

       deallocate( field2d % ioinfo )
       deallocate( field2d % array )
       deallocate( field2d )

    end subroutine deallocate_field2dInteger

    subroutine deallocate_field1dInteger(field1d)

       implicit none
       type (field1DInteger), intent(inout), pointer :: field1d

       deallocate( field1d % ioinfo )
       deallocate( field1d % array )
       deallocate( field1d )

    end subroutine deallocate_field1dInteger

    subroutine deallocate_field1dChar(field1d)

       implicit none
       type (field1dChar), intent(inout), pointer :: field1d

       deallocate( field1d % ioinfo )
       deallocate( field1d % array )
       deallocate( field1d )

    end subroutine deallocate_field1dChar


   !--------------------------------------------------------------
   ! deallocate all fields
   !--------------------------------------------------------------

   subroutine deallocate_fieldnd(field)

      implicit none
      type (fieldndReal), intent(inout), pointer :: field 

      if ( associated(field % field0d) ) then
         call deallocate_field(field % field0d)
      else if ( associated( field % field1d) ) then
         call deallocate_field(field % field1d)
      else if ( associated( field % field2d) ) then
         call deallocate_field(field % field2d)
      else if ( associated(field % field3d) ) then
         call deallocate_field(field % field3d)
      end if
      deallocate(field)

   end subroutine deallocate_fieldnd

   subroutine deallocate_fieldndInt(field)

      implicit none
      type (fieldndInt), intent(inout), pointer :: field

      if ( associated( field % field1d) ) then
         call deallocate_field(field % field1d)
      else if ( associated( field % field2d) ) then
         call deallocate_field(field % field2d)
      else if ( associated(field % field3d) ) then
         call deallocate_field(field % field3d)
      end if
      deallocate(field)

   end subroutine deallocate_fieldndInt


   subroutine deallocate_state0d(state0D)

     implicit none
     type (state_multilevel_type0D), intent(inout), pointer :: state0D
     integer :: i

     write (*,*) 'deallocate_state 0d '

     if ( state0D % nvar .gt. 0 ) then
        do i=1, state0D % nvar
           deallocate(state0D % num(i) % field % ioinfo )
           !deallocate(state0D % num(i) % field % scalar)
           deallocate(state0D % num(i) % field)
        end do
        deallocate ( state0D % num )
     end if
     deallocate ( state0D )

   end subroutine deallocate_state0d


   subroutine deallocate_state(state)

     implicit none
     type (state_type), intent(inout), pointer :: state
     integer :: vv

     do vv = 1, state%nvar
         call deallocate_fieldnd(state % num(vv) % field)
     end do
     deallocate(state % num)
     deallocate(state)

  end subroutine  deallocate_state


  subroutine deallocate_state_matrix(matrix0)

     implicit none
     type (state_matrix_type), intent(inout), pointer :: matrix0
     integer :: ii, jj

     do ii = 1, matrix0%nvar
        do jj = 1, matrix0%nvar
           call deallocate_fieldnd( matrix0 % num2d(ii,jj) % field )
        end do
     end do

     deallocate(matrix0%num2d)
     deallocate(matrix0)

   end subroutine deallocate_state_matrix



   !------------------------------------------------------------------------
   ! deallocate all other types
   !------------------------------------------------------------------------

   subroutine deallocate_mesh(mesh)

      implicit none
      type (mesh_type), intent(inout), pointer :: mesh

      call deallocate_field(mesh % lat)
      call deallocate_field(mesh % lon)
      call deallocate_field(mesh % hgt)
      call deallocate_field(mesh % ds)
      call deallocate_field(mesh % mapfac_m)
    
      write(*,*)'deallocate WRF specifics'
      ! WRF specifics 
      if ( associated(mesh % mapfac_u)) then
         write(*,*)'deallocate mesh % mapfac_u' 
         call deallocate_field(mesh % mapfac_u)
      end if
      if ( associated(mesh % mapfac_v)) then 
         call deallocate_field(mesh % mapfac_v)
         write(*,*)'deallocate mesh % mapfac_v' 
      end if
      if ( associated(mesh % znu)) then
         call deallocate_field(mesh % znu) 
         write(*,*)'deallocate mesh % znu' 
      end if

      deallocate(mesh)
      write(*,*)'deallocate mesh'

  end subroutine deallocate_mesh


  subroutine deallocate_bins(bins)

      implicit none
      type (bins_type), intent(inout), pointer :: bins

!      write(*,*)'deallocate bins a'
      call deallocate_field(bins % bin)
!      write(*,*)'deallocate bins b'
      call deallocate_field(bins % bin2d)
!      write(*,*)'deallocate bins c'
      call deallocate_field(bins % ij_counter_rc)
! PB GAEL

      if ( associated(bins % bin_pts)) then
!         write(*,*)'deallocate bins d associated'
         call deallocate_field(bins % bin_pts)
      end if
      if ( associated(bins % bin2d_pts)) then
!         write(*,*)'deallocate bins e'
         call deallocate_field1DInteger(bins % bin2d_pts)
      end if

! PB GAEL
!      write(*,*)'deallocate bins f'
      call deallocate_field(bins % counter2d)
!      write(*,*)'deallocate bins g'
      call deallocate_field(bins % counter3d)
!      write(*,*)'deallocate bins h'
      call deallocate_field(bins % filter3d)

!      write(*,*)'deallocate bin 2'
      if ( bins % bin_type2_param % nvar .gt. 0  ) then
         call deallocate_state0d(bins % bin_type2_param)
      end if
      deallocate(bins)
!      write(*,*)'deallocate bin 3'

  end subroutine deallocate_bins


  subroutine deallocate_domain(domain)
      
      implicit none
      type (domain_type), intent(inout), pointer :: domain

      write(*,*) 'deallocate domain state'
      call deallocate_state(domain % state)
      write(*,*) 'deallocate domain mesh'
      call deallocate_mesh(domain % mesh)
      write(*,*)'deallocate domain'
      deallocate ( domain )
      write(*,*)'end deallocate_domain'

   end subroutine deallocate_domain


   subroutine deallocate_eigenvar(eigen)

      implicit none
      type (eigen_type), intent(inout), pointer :: eigen

      if ( associated(eigen%vec%field%field3d )) then
         call deallocate_field(eigen%vec%field%field3d)
         call deallocate_field(eigen%val%field%field2d)
      else if ( associated(eigen%vec%field%field2d )) then
         call deallocate_field(eigen%vec%field%field2d)
         call deallocate_field(eigen%val%field%field1d)
      else if ( associated(eigen%vec%field%field1d )) then
         call deallocate_field(eigen%vec%field%field1d)
         call deallocate_field(eigen%val%field%field1d)
      else if ( associated(eigen%vec%field%field0d )) then      
         call deallocate_field(eigen%vec%field%field0d)
         call deallocate_field(eigen%val%field%field0d)
      else
         write(*,*)'Deallocation only for variable of dimensions 2 and 3'
         stop
      end if

      deallocate(eigen%val%field)
      deallocate(eigen%vec%field)
      deallocate(eigen%val)
      deallocate(eigen%vec)
      deallocate(eigen)

   end subroutine deallocate_eigenvar




!=============================================================================
! OPERATOR SECTION
!=============================================================================

   subroutine substract_state(state_new,state_ref)
         
         implicit none
         type (state_type), intent(inout), pointer :: state_new
         type (state_type), intent(in), pointer :: state_ref
         integer :: vv

         do vv = 1, state_ref%nvar
            if ( state_ref%num(vv)%IDdim == 0 ) then
               state_new%num(vv)%field%field0d%scalar = state_new%num(vv)%field%field0d%scalar - state_ref%num(vv)%field%field0d%scalar
            else if ( state_ref%num(vv)%IDdim == 1 ) then
               state_new%num(vv)%field%field1d%array = state_new%num(vv)%field%field1d%array - state_ref%num(vv)%field%field1d%array
            else if ( state_ref%num(vv)%IDdim == 2 ) then
               state_new%num(vv)%field%field2d%array = state_new%num(vv)%field%field2d%array - state_ref%num(vv)%field%field2d%array
            else if ( state_ref%num(vv)%IDdim == 3 ) then
               state_new%num(vv)%field%field3d%array = state_new%num(vv)%field%field3d%array - state_ref%num(vv)%field%field3d%array
            end if
         end do

   end subroutine substract_state


   subroutine mult_state(state, factor)

   implicit none
   type (state_type), intent(inout), pointer :: state
   real(kind=8), intent(in) :: factor 
   integer :: vv

   do vv = 1, state%nvar
      if ( state%num(vv)%IDdim == 0 ) then
           state%num(vv)%field%field0d%scalar = state%num(vv)%field%field0d%scalar * factor 
      else if ( state%num(vv)%IDdim == 1 ) then
           state%num(vv)%field%field1d%array = state%num(vv)%field%field1d%array * factor 
      else if ( state%num(vv)%IDdim == 2 ) then
           state%num(vv)%field%field2d%array = state%num(vv)%field%field2d%array * factor 
      else if ( state%num(vv)%IDdim == 3 ) then
           state%num(vv)%field%field3d%array = state%num(vv)%field%field3d%array * factor 
      end if
   end do
   
   end subroutine mult_state
 
   subroutine initial_state(state, value)

     implicit none
     type (state_type), intent(inout) :: state
     real(kind=8), intent(in) :: value
     integer :: vv

     do vv = 1, state%nvar
        if ( state%num(vv)%IDdim == 0 ) then
               state%num(vv)%field%field0d%scalar = value
        else if ( state%num(vv)%IDdim == 1 ) then
               state%num(vv)%field%field1d%array = value
        else if ( state%num(vv)%IDdim == 2 ) then
               state%num(vv)%field%field2d%array = value
        else if ( state%num(vv)%IDdim == 3 ) then
               state%num(vv)%field%field3d%array = value
        end if
     end do

   end subroutine initial_state


   subroutine filter_state(state, value)

     implicit none
     type (state_type), intent(inout) :: state
     real(kind=8), intent(in) :: value
     integer :: vv

     do vv = 1, state%nvar
        if ( state%num(vv)%IDdim == 0 ) then
           if (state%num(vv)%field%field0d%scalar.lt.value) then
               state%num(vv)%field%field0d%scalar = value
           end if
        else if ( state%num(vv)%IDdim == 1 ) then
           where(state%num(vv)%field%field1d%array.lt.value) 
               state%num(vv)%field%field1d%array(:) = value
           end where
        else if (state%num(vv)%IDdim == 2 ) then
           where(state%num(vv)%field%field2d%array(:,:).lt.value)
               state%num(vv)%field%field2d%array(:,:) = value
           end where
        else if ( state%num(vv)%IDdim == 3 ) then
           where(state%num(vv)%field%field3d%array(:,:,:).lt.value)
              state%num(vv)%field%field3d%array(:,:,:) = value
           end where
        end if
     end do

   end subroutine filter_state


   subroutine filter_state_min(state, Dim3, num_bins2d)

     implicit none
     type (state_type), intent(inout) :: state
     character (len=1024) :: varname
     integer, intent(in)  :: Dim3, num_bins2d     
     integer :: vv, kk, bb

     do vv = 1, state%nvar
         if ( state%num(vv)%IDdim == 2 ) then
            varname = trim(state%num(vv)%field%field2d%ioinfo%fieldName)
            do bb=1, num_bins2d
               do kk = 2, Dim3
                  if ( state%num(vv)%field%field2d%array(kk,bb) .lt. 1e-30  ) state%num(vv)%field%field2d%array(kk,bb)= state%num(vv)%field%field2d%array(kk-1,bb)
               end do 
            end do
          end if
     end do

   end subroutine filter_state_min


   subroutine rename_state(state, list_cv)

     implicit none
     type (state_type), intent(inout) :: state
     character (len=*), dimension(:), intent(in) :: list_cv     
     integer :: vv

     do vv = 1, state%nvar

        if ( state%num(vv)%IDdim == 0 ) then
               state%num(vv)%field%field0d%ioinfo%fieldName = trim(list_cv(vv))
        else if ( state%num(vv)%IDdim == 1 ) then
               state%num(vv)%field%field1d%ioinfo%fieldName = trim(list_cv(vv))
        else if ( state%num(vv)%IDdim == 2 ) then
               state%num(vv)%field%field2d%ioinfo%fieldName = trim(list_cv(vv))
        else if ( state%num(vv)%IDdim == 3 ) then
               state%num(vv)%field%field3d%ioinfo%fieldName = trim(list_cv(vv))
        end if

     end do

   end subroutine rename_state
    
 
   subroutine compute_mean_state(state_mean, state, member)

     implicit none
     type (state_type), intent(in) :: state
     type (state_type), intent(inout) ::  state_mean
     integer, intent(in) :: member
     integer :: vv
     real :: coeffa, coeffb

     coeffa = 1.0 / real(member)
     coeffb = real(member-1) * coeffa

     do vv=1, state%nvar
    
        if ( state%num(vv)%IDdim == 3 ) then
           state_mean%num(vv)%field%field3d%array =  coeffb * state_mean%num(vv)%field%field3d%array &
             + state%num(vv)%field%field3d%array  * coeffa
        else if ( state%num(vv)%IDdim == 2 ) then 
           state_mean%num(vv)%field%field2d%array =  coeffb * state_mean%num(vv)%field%field2d%array &
             + state%num(vv)%field%field2d%array  * coeffa
        else if ( state%num(vv)%IDdim == 1 ) then
           state_mean%num(vv)%field%field1d%array =  coeffb * state_mean%num(vv)%field%field1d%array &
             + state%num(vv)%field%field1d%array  * coeffa
        else if ( state%num(vv)%IDdim == 0 ) then
           state_mean%num(vv)%field%field0d%scalar =  coeffb * state_mean%num(vv)%field%field0d%scalar &
             + state%num(vv)%field%field0d%scalar  * coeffa
        end if

     end do
     write(*,*)'coeffa coeffb : ',coeffa,coeffb

   end subroutine compute_mean_state


   subroutine check_state(state,ii0,jj0,kk0)

     implicit none

     type (state_type), intent(in) :: state
     integer, intent(in) :: ii0, jj0, kk0
     integer :: vv
     real :: var0,mmax,mmin
     integer :: Dim1, Dim2, Dim3
    
     write(*,*)'------------------------------------------------------------------------------'
     write(*,*)' check state at ii0 jj0 kk0 ',ii0, jj0, kk0
     do vv=1, state%nvar

        if (state%num(vv)%IDdim == 0) then
           var0 = state%num(vv)%field%field0d%scalar
           write(*,*)'value of  ',trim(state%num(vv)%field%field0d%ioinfo%fieldName),var0        
        else if (state%num(vv)%IDdim == 1) then
           var0 = state%num(vv)%field%field1d%array(ii0)
           mmax = maxval(state%num(vv)%field%field1d%array(:))
           mmin = minval(state%num(vv)%field%field1d%array(:))
           write(*,*)'min of    ',trim(state%num(vv)%field%field1d%ioinfo%fieldName),mmin
           write(*,*)'max of    ',trim(state%num(vv)%field%field1d%ioinfo%fieldName),mmax
           write(*,*)'value of  ',trim(state%num(vv)%field%field1d%ioinfo%fieldName),var0
        else if (state%num(vv)%IDdim == 2) then
           var0 = state%num(vv)%field%field2d%array(ii0,jj0)
           mmax = maxval(state%num(vv)%field%field2d%array(:,:))
           mmin = minval(state%num(vv)%field%field2d%array(:,:))
           write(*,*)'min of    ',trim(state%num(vv)%field%field2d%ioinfo%fieldName),mmin
           write(*,*)'max of    ',trim(state%num(vv)%field%field2d%ioinfo%fieldName),mmax
           write(*,*)'value of  ',trim(state%num(vv)%field%field2d%ioinfo%fieldName),var0
        else if (state%num(vv)%IDdim == 3) then
           var0 = state%num(vv)%field%field3d%array(ii0,jj0,kk0)
           mmax = maxval(state%num(vv)%field%field3d%array(:,:,:))
           mmin = minval(state%num(vv)%field%field3d%array(:,:,:))
           write(*,*)'min of    ',trim(state%num(vv)%field%field3d%ioinfo%fieldName),mmin
           write(*,*)'max of    ',trim(state%num(vv)%field%field3d%ioinfo%fieldName),mmax
           write(*,*)'value of  ',trim(state%num(vv)%field%field3d%ioinfo%fieldName),var0
        else
           write(*,*)'variable none allocated, number ',vv
        end if

     end do
     write(*,*)'----------------------------------------------------------------------------'

   end subroutine check_state


   subroutine check_matrix(matrix0, ii0, jj0, kk0)

     implicit none

     type (state_matrix_type), intent(in) :: matrix0
     integer, intent(in) :: ii0, jj0, kk0
     real :: mmax, mmin, var0
     integer :: ii, jj

     write(*,*)'------------------------------------------------------------------------------'
     write(*,*)' check matrix at (i,j,k) ',ii0,jj0,kk0

     do ii=1, matrix0%nvar 
     do jj=1, matrix0%nvar 

     if ( matrix0%num2d(ii,jj)%IDdim == 0 ) then
        var0  = matrix0%num2d(ii,jj)%field%field0d%scalar
        write(*,*)'var0d     ',trim(matrix0%num2d(ii,jj)%field%field0d%ioinfo%fieldName)
        write(*,*)'value of  ',var0
     else if ( matrix0%num2d(ii,jj)%IDdim == 1 ) then
         var0  = matrix0%num2d(ii,jj)%field%field1d%array(ii0)
         mmax = maxval(matrix0%num2d(ii,jj)%field%field1d%array(:))
         mmin = minval(matrix0%num2d(ii,jj)%field%field1d%array(:))
         write(*,*)'var1d     ',trim(matrix0%num2d(ii,jj)%field%field1d%ioinfo%fieldName)
         write(*,*)'min of    ',mmin
         write(*,*)'max of    ',mmax
         write(*,*)'value of  ',var0
     else if ( matrix0%num2d(ii,jj)%IDdim == 2 ) then 
         var0 = matrix0%num2d(ii,jj)%field%field2d%array(ii0,jj0)
         mmax = maxval(matrix0%num2d(ii,jj)%field%field2d%array(:,:))
         mmin = minval(matrix0%num2d(ii,jj)%field%field2d%array(:,:))
         write(*,*)'var2d     ',trim(matrix0%num2d(ii,jj)%field%field2d%ioinfo%fieldName)
         write(*,*)'min of    ',mmin
         write(*,*)'max of    ',mmax
         write(*,*)'value of  ',var0
     else if ( matrix0%num2d(ii,jj)%IDdim == 3 ) then
         var0 = matrix0%num2d(ii,jj)%field%field3d%array(ii0,jj0,kk0)
         mmax = maxval(matrix0%num2d(ii,jj)%field%field3d%array(:,:,:))
         mmin = minval(matrix0%num2d(ii,jj)%field%field3d%array(:,:,:))
         write(*,*)'var3d     ',trim(matrix0%num2d(ii,jj)%field%field3d%ioinfo%fieldName)
         write(*,*)'min of    ',mmin
         write(*,*)'max of    ',mmax
         write(*,*)'value of  ',var0
     end if

     end do
     end do
   
     write(*,*)'------------------------------------------------------------------------------'

   end subroutine check_matrix

   
   subroutine read_matrix_param(matrix, varname_list, vardim_list, ncovar)

      implicit none

      type (state_matrix_type), intent(in) :: matrix
      character(len=*), dimension(:), intent(inout) :: varname_list
      integer,  dimension(:), intent(inout) :: vardim_list
      integer, intent(inout) :: ncovar
      integer :: ii, jj, vv

      vv = 0
      do ii = 1, matrix%nvar
          do jj = 1, matrix%nvar
             if ( matrix%num2d(ii,jj)%IDdim == 1 ) then
                vv = vv + 1
                vardim_list(vv) = matrix%num2d(ii,jj)%IDdim
                varname_list(vv) = trim(matrix%num2d(ii,jj)%field%field1d%ioinfo%fieldName)
             else if ( matrix%num2d(ii,jj)%IDdim == 2 ) then
                vv = vv + 1
                vardim_list(vv) = matrix%num2d(ii,jj)%IDdim
                varname_list(vv) = trim(matrix%num2d(ii,jj)%field%field2d%ioinfo%fieldName)
             else if ( matrix%num2d(ii,jj)%IDdim == 3 ) then
                vv = vv + 1
                vardim_list(vv) = matrix%num2d(ii,jj)%IDdim
                varname_list(vv) = trim(matrix%num2d(ii,jj)%field%field3d%ioinfo%fieldName)
             end if
          end do
      end do
      ncovar = vv

   end subroutine  read_matrix_param


   function get_state_indice(state, varname)

     implicit none

     integer :: get_state_indice
     type (state_type), intent(in) ,pointer :: state
     character (len=*), intent(in) :: varname
     integer :: vv

     get_state_indice = 0
     do vv = 1, state%nvar
        if ( state  % num(vv) % IDdim == 0 ) then
            if ( state  % num(vv) % field % field0d % ioinfo % fieldName == trim(varname) ) then
               get_state_indice = vv
            end if
         else if ( state  % num(vv) % IDdim == 1 ) then
            if ( state  % num(vv) % field % field1d % ioinfo % fieldName == trim(varname) ) then
               get_state_indice = vv
            end if         
         else if ( state  % num(vv) % IDdim == 2 ) then
            if ( state  % num(vv) % field % field2d % ioinfo % fieldName == trim(varname) ) then
               get_state_indice = vv
            end if
         else if ( state  % num(vv) % IDdim == 3 ) then
            if ( state  % num(vv) % field % field3d % ioinfo % fieldName == trim(varname) ) then
               get_state_indice = vv
            end if
         end if
     end do

   end function get_state_indice


   !-------------------------------------------------------------------------------------
   ! compute variance
   !-------------------------------------------------------------------------------------

   ! compute variance 1d bin dependent

   !subroutine compute_varce1dbin3d_var3d(variance, variable, bins, Dim1, Dim2, Dim3)
  ! 
  !    implicit none
  !    integer , intent(in) :: Dim1, Dim2, Dim3
  !    real , dimension(:,:,:) ,intent(in) :: variable  
  !    real , dimension(:) ,intent(inout) :: variance
  !    type (bins_type), intent(in) :: bins  
  ! 
  !    real :: coeffa, coeffb !  Accumulating mean coefficients
  !    integer :: i, j, k, b
  !    integer ,allocatable, dimension(:) :: bin_pts
!
!      allocate(bin_pts(1:bins%num_bins))
!      bin_pts = bins%bin_pts%array
!
!      do k = 1, Dim3
!         do j = 1, Dim2
!            do i = 1, Dim1
!                b = bins%bin%array(i,j,k)                
!                bin_pts(b) = bin_pts(b) + 1
!                coeffa = 1.0 / real(bin_pts(b))
!                coeffb = real(bin_pts(b)-1) * coeffa
!                variance(b) = coeffb * variance(b) + &
!                     coeffa * variable(i,j,k) * variable(i,j,k)
!                !variance(b) = variance(b) +  variable(i,j,k) * variable(i,j,k)
!            end do
!         end do
!      end do
!
!      deallocate(bin_pts)
!
!   end subroutine compute_varce1dbin3d_var3d


   subroutine compute_varce1dbin2d_var2d(variance, variable ,bins, Dim1, Dim2, Dim3)

      implicit none
      integer , intent(in) :: Dim1, Dim2, Dim3
      real(kind=8) , dimension(:,:) ,intent(in) :: variable
      real(kind=8) , dimension(:) ,intent(inout) :: variance
      type (bins_type), intent(in) :: bins

      real :: coeffa, coeffb !  Accumulating mean coefficients
      integer :: i, j, b
      integer ,allocatable, dimension(:) :: bin2d_pts0
      
      allocate(bin2d_pts0(1:bins%num_bins2d))
      bin2d_pts0 = bins%bin2d_pts%array

      do j = 1, Dim2
         do i = 1, Dim1
            b = bins%bin2d%array(i,j)
            bin2d_pts0(b) = bin2d_pts0(b) + 1
            coeffa = 1.0 / real(bin2d_pts0(b))
            coeffb = real(bin2d_pts0(b)-1) * coeffa
            variance(b) = coeffb * variance(b) + &
                  coeffa * variable(i,j) * variable(i,j)
            !variance(b) = variance(b) + variable(i,j) * variable(i,j)
         end do
      end do

      deallocate(bin2d_pts0)

  end subroutine compute_varce1dbin2d_var2d  
 
 
  subroutine compute_varce3dbin2d_var3d(variance, variable, bins, Dim1, Dim2, Dim3)

      implicit none
      integer , intent(in) :: Dim1, Dim2, Dim3
      real(kind=8) , dimension(:,:,:) ,intent(in) :: variable
      real(kind=8) , dimension(:,:,:) ,intent(inout) :: variance
      type (bins_type), intent(inout) :: bins

      real :: coeffa, coeffb !  Accumulating mean coefficients
      integer :: i, j, k, b, k2
      integer ,allocatable, dimension(:) :: bin2d_pts0

      allocate(bin2d_pts0(1:bins%num_bins2d))
      bin2d_pts0 = bins%bin2d_pts%array

      do j = 1, Dim2
         do i = 1, Dim1
            b = bins%bin2d%array(i,j)
            bin2d_pts0(b) = bin2d_pts0(b) + 1
            coeffa = 1.0 / real(bin2d_pts0(b))
            coeffb = real(bin2d_pts0(b)-1) * coeffa
            do k = 1, Dim3
               do k2 = 1, k
                  variance(k,k2,b) = coeffb * variance(k,k2,b) + &
                      coeffa * variable(i,j,k) * variable(i,j,k2)
               !    variance(k,k2,b) = variance(k,k2,b) + variable(i,j,k) * variable(i,j,k2)
               end do
            end do
         end do
       end do

      !  Fill covariance by symmetry:
      do b = 1, bins%num_bins2d
         do k = 1, Dim3
           do k2 = k+1, Dim3  ! Symmetry.
              !! warning with pgf90
              variance(k,k2,b) = variance(k2,k,b)
           end do
        end do
      end do

      !write(*,*)'**** variance  = ',variance(1,1,1),variance(10,1,1) 
     
      deallocate(bin2d_pts0)

  end subroutine compute_varce3dbin2d_var3d


!-------------------------------------------------------------
! compute covariance
!-------------------------------------------------------------

   subroutine compute_covar1d_bin3d(covar1d, var1, var2, bins, Dim1, Dim2, Dim3)

      implicit none
      type (bins_type), intent(in) :: bins
      integer, intent(in) :: Dim1, Dim2, Dim3
      real(kind=8), intent(in), dimension(:,:,:) :: var1, var2
      real(kind=8), intent(inout) :: covar1d(:)
      integer                :: ii, jj, kk
      integer                :: b
      integer ,allocatable, dimension(:) :: bin_pts0
      real :: coeffa, coeffb !  Accumulating mean coefficients

      write(*,*)'==> compute_covar1d_bin3d'   
      allocate(bin_pts0(1:bins%num_bins))
      bin_pts0 = bins%bin_pts%array
 
      do kk = 1, Dim3
         do jj = 1, Dim2
            do ii = 1, Dim1 
               b = bins%bin%array(ii,jj,kk)
               bin_pts0(b) = bin_pts0(b) + 1
               coeffa = 1.0 / real(bin_pts0(b))
               coeffb = real(bin_pts0(b)-1) * coeffa
               covar1d(b) = coeffb * covar1d(b) + coeffa * var1(ii,jj,kk) * var2(ii,jj,kk)
             end do
         end do
      end do
      write(*,*)'**** compute_covar1d_bin3d covar1d : ',covar1d
      write(*,*)
      deallocate(bin_pts0)

   end subroutine compute_covar1d_bin3d


   subroutine compute_covar2d_bin2d (covar2d, var2d, var3d, bins,  Dim1, Dim2, Dim3)

      implicit none
      type (bins_type), intent(in) :: bins
      integer, intent(in) :: Dim1, Dim2, Dim3
      real(kind=8), dimension(Dim1, Dim2), intent(in) :: var2d
      real(kind=8), dimension(Dim1,Dim2,Dim3), intent(in) :: var3d
      real(kind=8), dimension(:,:), intent(inout)  :: covar2d
      integer                :: ii, jj, kk
      integer                :: b
      integer ,allocatable, dimension(:) :: bin2d_pts0
      real :: coeffa, coeffb !  Accumulating mean coefficients
      
      write(*,*)'==> compute_covar2d_bin2d'   
      allocate(bin2d_pts0(1:bins%num_bins2d))
      bin2d_pts0 = bins%bin2d_pts%array

      do ii = 1, Dim1
         do jj = 1, Dim2
            b = bins%bin2d%array(ii,jj)
            bin2d_pts0(b) = bin2d_pts0(b) + 1
            coeffa = 1.0 / real(bin2d_pts0(b))
            coeffb = real(bin2d_pts0(b)-1) * coeffa
            do kk = 1, Dim3
               covar2d(kk,b) = coeffb * covar2d(kk,b) + coeffa * var2d(ii,jj) * var3d(ii,jj,kk)
            end do
         end do
     end do

     deallocate(bin2d_pts0)

   end subroutine compute_covar2d_bin2d


   subroutine compute_covar3d_bin2d (covar3d, var3d_1, var3d_2, bins, Dim1, Dim2, Dim3)

      implicit none
      type (bins_type), intent(in) :: bins
      integer, intent(in) :: Dim1, Dim2, Dim3
      real(kind=8), dimension(Dim1,Dim2,Dim3), intent(in) :: var3d_1, var3d_2
      real(kind=8), dimension(:,:,:), intent(inout)  :: covar3d
      integer                :: ii, jj, kk, kk2
      integer                :: b
      integer ,allocatable, dimension(:) :: bin2d_pts0
      real :: coeffa, coeffb !  Accumulating mean coefficients

      write(*,*)'==> compute_covar3d_bin2d'   
      allocate(bin2d_pts0(1:bins%num_bins2d))
      bin2d_pts0 = bins%bin2d_pts%array

      do ii = 1, Dim1
         do jj = 1, Dim2
            b = bins%bin2d%array(ii,jj)
            bin2d_pts0(b) = bin2d_pts0(b) + 1
            coeffa = 1.0 / real(bin2d_pts0(b))
            coeffb = real(bin2d_pts0(b)-1) * coeffa
            do kk = 1, Dim3
               do kk2 = 1, Dim3
                  covar3d(kk,kk2,b) = coeffb * covar3d(kk,kk2,b) + coeffa * var3d_1(ii,jj,kk) * var3d_2(ii,jj,kk2)
               end do
            end do
         end do
      end do


      deallocate(bin2d_pts0)

   end subroutine compute_covar3d_bin2d

   
   subroutine compute_var3d_unbalanced(state, ivar, regcoeff, bins, Dim1, Dim2, Dim3)
  
      type (state_type), intent(inout) :: state 
      type (state_matrix_type), intent(in) :: regcoeff 
      type (bins_type), intent(in) :: bins
      integer, intent(in) :: Dim1, Dim2, Dim3, ivar
      integer :: vv, i, j, k, nvar, b

      do vv = 1, ivar-1

         if ( regcoeff%num2d(ivar,vv)%IDdim == 3 ) then ! vv and ivar variables are 3d, bin2d
          write(*,*)'compute unbalanced part of ',trim(state%num(ivar)%field%field3d%ioinfo%fieldname)
          write(*,*)'using regcoeff : ',trim(regcoeff%num2d(ivar,vv)%field%field3d%ioinfo%fieldname)
            do j = 1, Dim2
               do i = 1, Dim1
                  b = bins%bin2d%array(i,j)
                  do k = 1, Dim3
                      state%num(ivar)%field%field3d%array(i,j,k) = state%num(ivar)%field%field3d%array(i,j,k) &
                          - SUM(regcoeff%num2d(ivar,vv)%field%field3d%array(k,1:Dim3,b) * state%num(vv)%field%field3d%array(i,j,1:Dim3) )
                  end do
               end do
            end do

         else if ( regcoeff%num2d(ivar,vv)%IDdim == 2 ) then ! vv or ivar variables are 2d, bin2d
            do j = 1, Dim2
               do i = 1, Dim1
                  b = bins%bin2d%array(i,j)
                    if ( state%num(ivar)%IDdim == 3 ) then
                      do k = 1, Dim3
                        state%num(ivar)%field%field3d%array(i,j,k) = state%num(ivar)%field%field3d%array(i,j,k) &
                      !     - SUM(regcoeff%num2d(ivar,vv)%field%field2d%array(1:Dim3,b) * state%num(vv)%field%field2d%array(i,j) )
                           - (regcoeff%num2d(ivar,vv)%field%field2d%array(k,b) * state%num(vv)%field%field2d%array(i,j) )
                      end do
                  else 
                     state%num(ivar)%field%field2d%array(i,j) = state%num(ivar)%field%field2d%array(i,j) &
                         - SUM(regcoeff%num2d(ivar,vv)%field%field2d%array(1:Dim3,b) * state%num(vv)%field%field3d%array(i,j,1:Dim3) )
                  end if
               end do
            end do

         else if ( regcoeff%num2d(ivar,vv)%IDdim == 1 ) then ! only psi3d is binning 3d --> 1d variable
            write(*,*)'compute unbalanced part of ',trim(state%num(ivar)%field%field3d%ioinfo%fieldname)
            write(*,*)'using regcoeff : ',trim(regcoeff%num2d(ivar,vv)%field%field1d%ioinfo%fieldname)
            do k = 1, Dim3
               do j = 1, Dim2
                  do i = 1, Dim1
                     b = bins%bin%array(i,j,k)
                          state%num(ivar)%field%field3d%array(i,j,k) = state%num(ivar)%field%field3d%array(i,j,k) &
                           - regcoeff%num2d(ivar,vv)%field%field1d%array(b) * state%num(vv)%field%field3d%array(i,j,k)
                  end do
               end do
            end do
 
         end if

    end do

   end subroutine compute_var3d_unbalanced

   
   subroutine compute_var2d_unbalanced(state, ivar, regcoeff, bins, Dim1, Dim2, Dim3)

      implicit none
      type (state_type), intent(inout) :: state
      type (state_matrix_type), intent(in) :: regcoeff
      type (bins_type), intent(in) :: bins
      integer, intent(in) :: Dim1, Dim2, Dim3, ivar
      integer :: vv, i, j, k, b
     
      write(*,*) 'inside compute_var2d_unbalanced ',trim(state%num(ivar)%field%field2d%ioinfo%fieldName) 

      do vv = 1, ivar-1     
         write(*,*)'ivar,vv ',ivar,vv
         write(*,*)'regcoeff%num2d(ivar,vv)%IDdim ',regcoeff%num2d(ivar,vv)%IDdim
          do j = 1, Dim2
             do i = 1, Dim1
                 b = bins%bin2d%array(i,j)
                 if ( state%num(vv)%IDdim == 3 ) then ! var 3d
                       if ( regcoeff%num2d(ivar,vv)%IDdim == 1 ) then
                          do k = 1, Dim3
                             b = bins%bin%array(i,j,k)
                             state%num(ivar)%field%field2d%array(i,j) = state%num(ivar)%field%field2d%array(i,j) &
                               - regcoeff%num2d(ivar,vv)%field%field1d%array(b) * state%num(vv)%field%field3d%array(i,j,k)
                          end do
                       else if ( regcoeff%num2d(ivar,vv)%IDdim == 2 ) then
                          state%num(ivar)%field%field2d%array(i,j) = state%num(ivar)%field%field2d%array(i,j) &
                            - SUM( regcoeff%num2d(ivar,vv)%field%field2d%array(1:Dim3,b) * state%num(vv)%field%field3d%array(i,j,1:Dim3) )
                       end if
                 else ! var 2d
                      if ( regcoeff%num2d(ivar,vv)%IDdim == 2 ) then 
                        state%num(ivar)%field%field2d%array(i,j) = state%num(ivar)%field%field2d%array(i,j) &
                           - SUM( regcoeff%num2d(ivar,vv)%field%field2d%array(1:Dim3,b) * state%num(vv)%field%field2d%array(i,j) )
                      end if
                 end if
             end do
       end do

      end do

   end subroutine compute_var2d_unbalanced


   subroutine increment_bin_pts(bins, Dim1, Dim2, Dim3)
 
      implicit none
      type (bins_type), intent(inout) :: bins
      integer, intent(in) :: Dim1, Dim2, Dim3
      integer :: i, j, k, b, b2

      !! increments for binning  
      do j = 1, Dim2
        do i = 1, Dim1
            b = bins%bin2d%array(i,j)
            bins%bin2d_pts%array(b) = bins%bin2d_pts%array(b) + 1
            do k = 1, Dim3
               b2 = bins%bin%array(i,j,k)
               bins%bin_pts%array(b2) = bins%bin_pts%array(b2) + 1
            end do
         end do
      end do

   end subroutine increment_bin_pts


!-----------------------------------------------------------------
 
  subroutine sum_int2d(var2d,summ,Dim1,Dim2)
    implicit none
    integer :: summ
    integer :: ii, jj, Dim1, Dim2
    integer, dimension(Dim1,Dim2), intent(in) :: var2d

    summ = 0
    do ii=1, Dim1
      do jj=1, Dim2
         summ = summ + var2d(ii,jj)
      end do
    end do

  end subroutine sum_int2d
   


!=========================================================================================

end module variable_types
