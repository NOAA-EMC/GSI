

module io_output

!---------------------------------------------------------------------- 
! Purpose: Define how to write objects in netcdf format 
!
! History:
!
! Date     Author & Comment
! -------- ----------------
! dd/mm/yy Gael Descombes
!          Initial version (01/07/2012)
!----------------------------------------------------------------------

   use variable_types
   use configure
!   use io_input_model
 
   type io_output_object

      integer :: wr_ncid

      character (len=1024) :: filename
      integer :: time

      integer :: wrDimIDtime
      integer :: wrDimIDlat
      integer :: wrDimIDmlat
      integer :: wrDimIDlon
      integer :: wrDimIDlev
      integer :: wrDimIDwav
      integer :: wrDimIDate
      integer :: wrDimIDStr
      integer :: wrDimIDnvar
      integer :: wrDimIDncovar
      integer :: wrDimIDmaxvar
      integer :: wrDimIDne

      integer :: wrVarIDlat
      integer :: wrVarIDlon
      integer :: wrVarIDlev

      integer :: wrVarIDvar0
      integer :: ivarnd
      integer , dimension(100) :: wrVarIDvarnd
      integer :: wrVarIDate

      !! mesh parameter
      integer :: wrDimIDlatu 
      integer :: wrDimIDlonu
      integer :: wrDimIDlatv 
      integer :: wrDimIDlonv
      integer :: wrVarIDhgt
      integer :: wrVarIDds
      !! specific wrf or not
      integer :: wrVarIDmapfu
      integer :: wrVarIDmapfv
      integer :: wrVarIDmapfm
      integer :: wrVarIDznu
      !! other model ??

      !! bins
      integer :: wrDimIDbin
      integer :: wrDimIDbin2d
      integer :: wrVarIDbin
      integer :: wrVarIDbin2dpts
      integer :: wrVarIDbinpts
      integer :: wrVarIDbin2d
      integer :: wrVarIDijcrc
      integer :: wrVarIDbinty

      !! covariance array (from namelist)
      integer :: wrVarIDcovar       
      integer , dimension(100) :: wrVarIDnaml

   end type io_output_object


   interface io_output_field
      module procedure io_output_field0dReal
      module procedure io_output_field1dReal
      module procedure io_output_field2dReal
      module procedure io_output_field3dReal
      module procedure io_output_field1dInteger
      module procedure io_output_field2dInteger
      module procedure io_output_field3dInteger
   end interface io_output_field

   interface io_output_field_time
      module procedure io_output_field0dReal_time
      module procedure io_output_field1dReal_time
      module procedure io_output_field2dReal_time
      module procedure io_output_field3dReal_time
      module procedure io_output_field1dChar_time
      module procedure io_output_field1dChar
   end interface io_output_field_time

   interface write_variable
      module procedure write_variable3dReal
      module procedure write_variable2dReal
      module procedure write_variable1dReal
      module procedure write_variable2dInteger
      module procedure write_variable3dInteger
      module procedure write_variable3dReal_bin
      module procedure write_variable2dReal_bin
      module procedure write_variable1dReal_bin
   end interface write_variable


   contains
 
!==================================================================================
! WRITE A STATE AND A BIN STATE FILE
!==================================================================================
   subroutine check_err(nferr,varname)

   implicit none
   integer, intent(in) :: nferr
   character(len=*), optional, intent(in) :: varname
   include "netcdf.inc"

   if (nferr /= NF_NOERR) then
         write(0,*) ' '
         write(0,*) 'Error nferr ', nferr
         write(0,*) 'nf_strerror ', nf_strerror(nferr)
         if (present(varname)) then
            write(0,*) ' problem ',trim(varname)
         end if
         write(0,*) ' '
         stop
   end if

   end subroutine check_err

   !---------------------------------------------------------
   ! WRITE A STATE
   !---------------------------------------------------------

   subroutine io_output_init(filename,output_obj)

      implicit none

      type (io_output_object), intent(out) :: output_obj
      character (len=*), intent(in) :: filename
      integer :: nferr

      include 'netcdf.inc'

      output_obj % filename = trim(filename)
      output_obj % ivarnd = 0
   
      nferr = nf_create(trim(output_obj % filename), ior(NF_CLOBBER,NF_64BIT_OFFSET), output_obj%wr_ncid)
   
   end subroutine io_output_init


   subroutine  output_state_init(output_obj, state, mesh, filename)

      implicit none

      include 'netcdf.inc'

      type (io_output_object), intent(inout) :: output_obj
      type (mesh_type), intent(in) :: mesh
      type (state_type), intent(inout) :: state
      character (len=*), intent(in) :: filename
      integer :: nferr
      integer, dimension(10) :: dimlist

      call io_output_init(filename,output_obj)

      ! definition of the dimensions
      nferr = nf_def_dim(output_obj%wr_ncid, 'lon', mesh%Dim1, output_obj%wrDimIDlon)
      nferr = nf_def_dim(output_obj%wr_ncid, 'lat', mesh%Dim2, output_obj%wrDimIDlat)
      nferr = nf_def_dim(output_obj%wr_ncid, 'lev', mesh%Dim3, output_obj%wrDimIDlev)
      nferr = nf_def_dim(output_obj%wr_ncid, 'DateStrLen', DateStrLen, output_obj%wrDimIDate)
      if (output_obj % time .ne. 0 ) then
         nferr = nf_def_dim(output_obj%wr_ncid, 'Time', NF_UNLIMITED, output_obj%wrDimIDTime)
      end if
      ! Define latitude
      dimlist(1) = output_obj % wrDimIDlon
      dimlist(2) = output_obj % wrDimIDlat
      nferr = nf_def_var(output_obj % wr_ncid, 'lat', NF_DOUBLE, 2, dimlist, output_obj % wrVarIDlat)
      ! Define longitude
      dimlist(1) = output_obj % wrDimIDlon
      dimlist(2) = output_obj % wrDimIDlat
      nferr = nf_def_var(output_obj % wr_ncid, 'lon', NF_DOUBLE, 2, dimlist, output_obj % wrVarIDlon)
      ! Define Times
      if (output_obj % time .ne. 0 ) then
         dimlist(1) = output_obj % wrDimIDate
         dimlist(2) = output_obj % wrDimIDtime
        nferr = nf_def_var(output_obj % wr_ncid, 'Times', NF_CHAR, 2, dimlist, output_obj % wrVarIDate)
      end if
      ! define variables time dependent
      call io_output_state_def(state, output_obj % time, output_obj)

      nferr = nf_enddef(output_obj % wr_ncid)
      nferr = nf_sync(output_obj % wr_ncid)
      write(*,*)'nf_strerror ', nf_strerror(nferr)

   end subroutine output_state_init


   subroutine io_output_state_def(state, itime, output_obj)

      implicit none

      include 'netcdf.inc'

      type (io_output_object), intent(inout) :: output_obj
      type (state_type), intent(inout) :: state
      integer, intent(in) :: itime 
      integer :: nferr
      integer, dimension(10) :: dimlist
      integer :: vv, ivarnd

      ivarnd = output_obj % ivarnd

      ! define variables
      write(*,*)'state%nvar ',state%nvar
      do vv= 1, state%nvar

         write(*,*)'Define variable ',state%num(vv)%IDdim

         if ( state%num(vv)%IDdim == 0  ) then
            ivarnd = ivarnd + 1
            dimlist(1) = output_obj % wrDimIDtime
            if ( itime == 0 ) then
               nferr = nf_def_var(output_obj % wr_ncid, trim(state%num(vv)%field%field0d%ioinfo%fieldName), &
                               NF_DOUBLE, 0, dimlist, output_obj % wrVarIDvarnd(ivarnd) )
            else
               nferr = nf_def_var(output_obj % wr_ncid, trim(state%num(vv)%field%field0d%ioinfo%fieldName), &
                               NF_DOUBLE, 1, dimlist, output_obj % wrVarIDvarnd(ivarnd) )
            end if
            state%num(vv)%field%field0d%ioinfo%ID = ivarnd

         else if ( state%num(vv)%IDdim == 1  ) then
            ivarnd = ivarnd + 1
            if (trim(state%name) == 'bins') then
               if ( vv .eq. 1 ) then
                  dimlist(1) = output_obj % wrDimIDbin
               else
                  dimlist(1) = output_obj % wrDimIDbin2d
               end if
            else if (trim(state%name) == 'bin2d') then
               dimlist(1) = output_obj % wrDimIDbin2d
            else
               dimlist(1) = output_obj % wrDimIDlev
            end if
            dimlist(2) = output_obj % wrDimIDtime
            if ( itime == 0 ) then
               nferr = nf_def_var(output_obj % wr_ncid, trim(state%num(vv)%field%field1d%ioinfo%fieldName), &
                               NF_DOUBLE, 1, dimlist, output_obj % wrVarIDvarnd(ivarnd) )
            else
               nferr = nf_def_var(output_obj % wr_ncid, trim(state%num(vv)%field%field1d%ioinfo%fieldName), &
                               NF_DOUBLE, 2, dimlist, output_obj % wrVarIDvarnd(ivarnd) )
            end if
            state%num(vv)%field%field1d%ioinfo%ID = ivarnd

         else if ( state%num(vv)%IDdim == 2  ) then
            ivarnd = ivarnd + 1
            if (trim(state%name) == 'bins') then
               dimlist(1) = output_obj % wrDimIDlev
               dimlist(2) = output_obj % wrDimIDbin2d
            else if (trim(state%name) == 'bin2d') then
               dimlist(1) = output_obj % wrDimIDlev
               dimlist(2) = output_obj % wrDimIDbin2d
            else
               dimlist(1) = output_obj % wrDimIDlon
               dimlist(2) = output_obj % wrDimIDlat
            end if
               dimlist(3) = output_obj % wrDimIDtime
            if ( itime == 0 ) then
               nferr = nf_def_var(output_obj % wr_ncid, trim(state%num(vv)%field%field2d%ioinfo%fieldName), &
                             NF_DOUBLE, 2, dimlist, output_obj % wrVarIDvarnd(ivarnd))
               call check_err(nferr,state%num(vv)%field%field2d%ioinfo%fieldName)
            else
               nferr = nf_def_var(output_obj % wr_ncid, trim(state%num(vv)%field%field2d%ioinfo%fieldName), &
                             NF_DOUBLE, 3, dimlist, output_obj % wrVarIDvarnd(ivarnd))
               call check_err(nferr,trim(state%num(vv)%field%field2d%ioinfo%fieldName))
            end if
            state%num(vv)%field%field2d%ioinfo%ID = ivarnd

         else if ( state%num(vv)%IDdim == 3  ) then
            ivarnd = ivarnd + 1
            if (trim(state%name) == 'bins') then
               dimlist(1) = output_obj % wrDimIDlev
               dimlist(2) = output_obj % wrDimIDlev
               dimlist(3) = output_obj % wrDimIDbin2d
            else if (trim(state%name) == 'bin2d') then
               dimlist(1) = output_obj % wrDimIDlev
               dimlist(2) = output_obj % wrDimIDlev
               dimlist(3) = output_obj % wrDimIDbin2d
            else
               dimlist(1) = output_obj % wrDimIDlon
               dimlist(2) = output_obj % wrDimIDlat
               dimlist(3) = output_obj % wrDimIDlev
            end if
            dimlist(4) = output_obj % wrDimIDtime
            if ( itime == 0 ) then
               nferr = nf_def_var(output_obj % wr_ncid, trim(state%num(vv)%field%field3d%ioinfo%fieldName), &
                               NF_DOUBLE, 3, dimlist, output_obj % wrVarIDvarnd(ivarnd) )
               call check_err(nferr,trim(state%num(vv)%field%field3d%ioinfo%fieldName))
            else 
               nferr = nf_def_var(output_obj % wr_ncid, trim(state%num(vv)%field%field3d%ioinfo%fieldName), &
                               NF_DOUBLE, 4, dimlist, output_obj % wrVarIDvarnd(ivarnd) )
               call check_err(nferr,trim(state%num(vv)%field%field3d%ioinfo%fieldName))
            end if
            state%num(vv)%field%field3d%ioinfo%ID = ivarnd
         end if

      end do

      output_obj % ivarnd = ivarnd

   end subroutine io_output_state_def


   subroutine io_output_state_var(state, itime, output_obj)

      implicit none

      include 'netcdf.inc'

      type (io_output_object), intent(inout) :: output_obj
      type (state_type), intent(in) :: state
      integer, intent(in) :: itime
      integer :: nferr
      integer :: vv
   
      do vv = 1,  state%nvar

         call io_output_fieldnd_var(state%num(vv), itime, output_obj)

      end do

   end subroutine io_output_state_var


   subroutine output_state_for_domain(output_obj, state, mesh, itime)
   
      implicit none
   
      type (io_output_object), intent(inout) :: output_obj
      type (mesh_type), intent(in) :: mesh
      type (state_type), intent(in) :: state
      integer, intent(in) :: itime
      integer :: vv

      output_obj % time = itime

      ! write mesh grid
      call io_output_field(output_obj, mesh%lat)
      call io_output_field(output_obj, mesh%lon)
      call io_output_state_var(state, itime, output_obj)
      ! write date
      if ( itime .ne. 0 ) then
         write (*,*)'output_state_for_domain date ',trim(state%date)
         call io_output_field_time(output_obj, state%date)
      end if

   end subroutine output_state_for_domain


   subroutine io_output_finalize(output_obj)

      implicit none

      include 'netcdf.inc'

      type (io_output_object), intent(inout) :: output_obj

      integer :: nferr

      nferr = nf_close(output_obj % wr_ncid)

   end subroutine io_output_finalize


   subroutine write_output_state(filename, state, mesh)

      implicit none

      character (len=1024), intent(in) :: filename
      type (mesh_type), intent(in) :: mesh
      type (state_type), intent(inout) :: state
      type (io_output_object) :: output_obj
      
      output_obj % time = 1
      call output_state_init(output_obj, state, mesh, filename)
      call output_state_for_domain(output_obj, state, mesh, output_obj % time)
      call io_output_finalize(output_obj)

   end subroutine write_output_state

   
   subroutine write_output_state_bin(filename, state, itime, mesh, bins)

      implicit none

      include 'netcdf.inc'

      character (len=*), intent(in) :: filename
      type (mesh_type), intent(in) :: mesh
      type (bins_type), intent(in) :: bins
      type (state_type), intent(inout) :: state
      type (io_output_object) :: output_obj
      integer :: itime, nferr 

      call io_output_init(filename,output_obj)      
      output_obj % time = 0
      nferr = nf_def_dim(output_obj%wr_ncid, 'lev', mesh%Dim3, output_obj%wrDimIDlev)
      nferr = nf_def_dim(output_obj%wr_ncid, 'num_bins', bins%num_bins, output_obj%wrDimIDbin)
      nferr = nf_def_dim(output_obj%wr_ncid, 'num_bins2d', bins%num_bins2d, output_obj%wrDimIDbin2d)

      state%name = 'bins'
      call io_output_state_def(state, output_obj % time, output_obj)   
      nferr = nf_enddef(output_obj % wr_ncid)
      nferr = nf_sync(output_obj % wr_ncid)      

      call io_output_state_var(state, itime, output_obj)

      nferr = nf_close(output_obj % wr_ncid)

   end subroutine write_output_state_bin

   !---------------------------------------------------------
   ! WRITE A MATRIX STATE
   !---------------------------------------------------------

   subroutine write_matrix_state(filename, matrix, bins, mesh)

      implicit none

      type (io_output_object) :: output_obj
      type (state_matrix_type), intent(inout) :: matrix
      character (len=*), intent(in):: filename
      type (bins_type), intent(in) :: bins
      type (mesh_type), intent(in) :: mesh
      integer :: nferr, ii, jj

      include 'netcdf.inc'

      call io_output_init(filename,output_obj)      

      ! definition of the dimensions
      nferr = nf_def_dim(output_obj%wr_ncid, 'lev', mesh%Dim3, output_obj%wrDimIDlev)
      nferr = nf_def_dim(output_obj%wr_ncid, 'num_bins', bins%num_bins, output_obj%wrDimIDbin)
      nferr = nf_def_dim(output_obj%wr_ncid, 'num_bins2d', bins%num_bins2d, output_obj%wrDimIDbin2d)

      ! definition of variables
      call io_output_matrix_def(matrix ,output_obj)
      nferr = nf_enddef(output_obj % wr_ncid)
      nferr = nf_sync(output_obj % wr_ncid)

      ! write variables
      call io_output_matrix_var(matrix, output_obj)

      nferr = nf_close(output_obj % wr_ncid)

   end subroutine write_matrix_state

   
   subroutine io_output_matrix_def(matrix, output_obj)

      implicit none

      type (io_output_object), intent(inout) :: output_obj
      type (state_matrix_type), intent(inout) :: matrix
      integer, dimension(10) :: dimlist
      integer :: nferr, ii, jj, ivarnd

      include 'netcdf.inc'

      ivarnd  = output_obj % ivarnd
   
      write(*,*)'toto nvar ',matrix%nvar
      write(*,*)'inside gael io_output_matrix_def ivarnd ',ivarnd
 
      do jj = 1, matrix%nvar
          do ii = 1, matrix%nvar

            !write(*,*)'matrix ',jj,ii

             if ( matrix%num2d(ii,jj)%IDdim == 1 ) then
                ivarnd = ivarnd + 1
                dimlist(1) = output_obj % wrDimIDbin
                nferr = nf_def_var(output_obj % wr_ncid, trim(matrix%num2d(ii,jj)%field%field1d%ioinfo%fieldName), &
                            NF_DOUBLE, 1, dimlist, output_obj % wrVarIDvarnd(ivarnd))
                call check_err(nferr,matrix%num2d(ii,jj)%field%field1d%ioinfo%fieldName)
                matrix%num2d(ii,jj)%field%field1d%ioinfo%ID = ivarnd

             else if ( matrix%num2d(ii,jj)%IDdim == 2 ) then
                ivarnd = ivarnd + 1
                dimlist(1) = output_obj % wrDimIDlev
                dimlist(2) = output_obj % wrDimIDbin2d
                write(*,*)'fieldname ',trim(matrix%num2d(ii,jj)%field%field2d%ioinfo%fieldName)
                nferr = nf_def_var(output_obj % wr_ncid, trim(matrix%num2d(ii,jj)%field%field2d%ioinfo%fieldName), &
                            NF_DOUBLE, 2, dimlist, output_obj % wrVarIDvarnd(ivarnd))
                call check_err(nferr,matrix%num2d(ii,jj)%field%field2d%ioinfo%fieldName)
                matrix%num2d(ii,jj)%field%field2d%ioinfo%ID = ivarnd

             else if ( matrix%num2d(ii,jj)%IDdim == 3 ) then
                ivarnd = ivarnd + 1
                dimlist(1) = output_obj % wrDimIDlev
                dimlist(2) = output_obj % wrDimIDlev
                dimlist(3) = output_obj % wrDimIDbin2d
                write(*,*)'fieldname ',trim(matrix%num2d(ii,jj)%field%field3d%ioinfo%fieldName)
                nferr = nf_def_var(output_obj % wr_ncid, trim(matrix%num2d(ii,jj)%field%field3d%ioinfo%fieldName), &
                            NF_DOUBLE, 3, dimlist, output_obj % wrVarIDvarnd(ivarnd))
                call check_err(nferr,matrix%num2d(ii,jj)%field%field3d%ioinfo%fieldName)
                matrix%num2d(ii,jj)%field%field3d%ioinfo%ID = ivarnd
             end if

          end do
      end do

      output_obj % ivarnd = ivarnd
      write(*,*)'leaving  io_output_matrix_def ', ivarnd

    end subroutine io_output_matrix_def


    subroutine io_output_matrix_var(matrix,output_obj)

      implicit none

      type (io_output_object), intent(inout) :: output_obj
      type (state_matrix_type), intent(inout) :: matrix
      integer :: nferr, ii, jj

      include 'netcdf.inc'

      write(*,*)'--write_matrix_state--'
      do ii = 1, matrix%nvar
          do jj = 1, matrix%nvar
             if ( matrix%num2d(ii,jj)%IDdim == 1 ) then
                call io_output_field(output_obj, matrix%num2d(ii,jj)%field%field1d)
             else if ( matrix%num2d(ii,jj)%IDdim == 2 ) then
                call io_output_field(output_obj, matrix%num2d(ii,jj)%field%field2d)
             else if ( matrix%num2d(ii,jj)%IDdim == 3 ) then
                call io_output_field(output_obj, matrix%num2d(ii,jj)%field%field3d)
             end if
          end do
      end do

    end subroutine io_output_matrix_var

   !---------------------------------------------------------
   ! WRITE A BIN STATE
   !---------------------------------------------------------

   subroutine write_output_bin(filename, bins, mesh, state)

      implicit none

      integer, dimension(10) :: dimlist
      type (io_output_object) :: output_obj
      type (state_type), intent(inout), optional :: state
      character (len=*), intent(in):: filename
      type (bins_type), intent(inout) :: bins
      type (mesh_type), intent(in) :: mesh
      integer :: vv, nferr, nparam, ivarnd

      include 'netcdf.inc' 

      integer, dimension(1) :: start1, count1
      start1(1) = 1
      count1(1) = 1

      call io_output_init(filename,output_obj)

      nferr = nf_def_dim(output_obj%wr_ncid, 'lon', mesh%Dim1, output_obj%wrDimIDlon)
      nferr = nf_def_dim(output_obj%wr_ncid, 'lat', mesh%Dim2, output_obj%wrDimIDlat)
      nferr = nf_def_dim(output_obj%wr_ncid, 'lev', mesh%Dim3, output_obj%wrDimIDlev)
      nferr = nf_def_dim(output_obj%wr_ncid, 'num_bins', bins%num_bins, output_obj%wrDimIDbin)
      nferr = nf_def_dim(output_obj%wr_ncid, 'num_bins2d', bins%num_bins2d, output_obj%wrDimIDbin2d)
       
      ! define bins parameters
      nparam = bins % bin_type2_param % nvar
      call io_output_bins_def(bins, output_obj)

      ! define state mean bin
      if (present(state)) then
         dimlist(1) = output_obj % wrDimIDbin2d
         dimlist(2) = output_obj % wrDimIDlev
         do vv = 1,  state%nvar
            ivarnd = output_obj % ivarnd + 1
            if ( state%num(vv)%IDdim == 1  ) then
               nferr = nf_def_var(output_obj % wr_ncid, state%num(vv)%field%field1d%ioinfo%fieldName, NF_DOUBLE, 1, dimlist, output_obj%wrVarIDvarnd(ivarnd))
               state%num(vv)%field%field1d%ioinfo%ID = ivarnd
            else if ( state%num(vv)%IDdim == 2  ) then
               nferr = nf_def_var(output_obj % wr_ncid, state%num(vv)%field%field2d%ioinfo%fieldName, NF_DOUBLE, 2, dimlist, output_obj%wrVarIDvarnd(ivarnd))  
               state%num(vv)%field%field2d%ioinfo%ID = ivarnd
            end if
         end do
      end if

      nferr = nf_enddef(output_obj % wr_ncid)
      nferr = nf_sync(output_obj % wr_ncid)

      ! write bins
      call io_output_bins_var(bins, output_obj)

      ! write var1d
      if (present(state)) then
         do vv = 1, state%nvar
            if ( state%num(vv)%IDdim == 1  ) then
               call io_output_field(output_obj, state%num(vv)%field%field1d)
            else if ( state%num(vv)%IDdim == 2  ) then
               call io_output_field(output_obj, state%num(vv)%field%field2d)
            end if
         end do
      end if

      nferr = nf_close(output_obj % wr_ncid)

   end subroutine write_output_bin

  
   subroutine io_output_bins_def(bins, output_obj)

      implicit none

      integer, dimension(10) :: dimlist
      type (io_output_object), intent(inout) :: output_obj
      type (bins_type), intent(inout) :: bins
      integer :: vv, nferr, ivarnd
      integer, dimension(1) :: start1, count1

      include 'netcdf.inc'

      ivarnd = output_obj % ivarnd

      ! define bins
      dimlist(1) = output_obj % wrDimIDlon
      dimlist(2) = output_obj % wrDimIDlat
      dimlist(3) = output_obj % wrDimIDlev
      ! define bin2d / bin
      nferr = nf_def_var(output_obj % wr_ncid, bins%bin2d%ioinfo%fieldName, NF_INT, 2, dimlist, output_obj%wrVarIDbin2d)
      nferr = nf_def_var(output_obj % wr_ncid, bins%bin%ioinfo%fieldName, NF_INT, 3, dimlist, output_obj%wrVarIDbin)
      write(*,*)'nf_strerror after wrVarIDbin ', nf_strerror(nferr)
      ! define bin
      dimlist(3) = output_obj % wrDimIDbin2d
      nferr = nf_def_var(output_obj % wr_ncid, bins%ij_counter_rc%ioinfo%fieldName, NF_INT, 3, dimlist, output_obj%wrVarIDijcrc)
      ! define bin_pts
      dimlist(1) = output_obj % wrDimIDbin
      nferr = nf_def_var(output_obj % wr_ncid, bins%bin_pts%ioinfo%fieldName, NF_INT, 1, dimlist, output_obj%wrVarIDbinpts)
      write(*,*)'nf_strerror binpts ', nf_strerror(nferr), bins%bin_pts%ioinfo%fieldName
      dimlist(1) = output_obj % wrDimIDbin2d
      nferr = nf_def_var(output_obj % wr_ncid, bins%bin2d_pts%ioinfo%fieldName, NF_INT, 1, dimlist, output_obj%wrVarIDbin2dpts)
      write(*,*)'write bin after define bin_pts', nferr
      write(*,*)'nf_strerror ', nf_strerror(nferr)

      dimlist(1) = 1
      do vv = 1, 6 !bins % bin_type2_param % nvar
         ivarnd = ivarnd + 1
         nferr = nf_def_var(output_obj % wr_ncid, bins%bin_type2_param%num(vv)%field%ioinfo%fieldName, NF_DOUBLE, 0, dimlist, output_obj%wrVarIDvarnd(ivarnd))
         bins%bin_type2_param%num(vv)%field%ioinfo%ID = ivarnd
      end do
      nferr = nf_def_var(output_obj % wr_ncid, 'bin_type', NF_INT, 0, dimlist, output_obj%wrVarIDbinty)
      write(*,*)' nferr ',nferr
      write(*,*)'nf_strerror ', nf_strerror(nferr)
    
      output_obj % ivarnd = ivarnd

   end subroutine io_output_bins_def


   subroutine io_output_bins_var(bins, output_obj)

     implicit none

     type (io_output_object), intent(inout) :: output_obj
     type (bins_type), intent(in) :: bins
     integer :: vv, nferr
     integer, dimension(1) :: start1, count1

     include 'netcdf.inc'

     start1(1) = 1
     count1(1) = 1

     ! write bins
     call io_output_field(output_obj, bins%bin)
     call io_output_field(output_obj, bins%ij_counter_rc)
     call io_output_field(output_obj, bins%bin2d)
     call io_output_field(output_obj, bins%bin_pts)
     call io_output_field(output_obj, bins%bin2d_pts)

     do vv = 1, bins % bin_type2_param % nvar
        call io_output_field(output_obj, bins%bin_type2_param%num(vv)%field)
     end do
     write(*,*)'bins%bin_type ',bins%bin_type
     nferr = nf_put_vara_int(output_obj % wr_ncid, output_obj%wrVarIDbinty, start1, count1, bins%bin_type)

   end subroutine io_output_bins_var


!=================================================================================
! Dynamical mask
!=================================================================================

subroutine compute_dynamic_mask(bins, mesh, state, state_mean, cem, date0)

      use configure

      implicit none
      type (bins_type), intent(inout), pointer :: bins
      type (mesh_type), intent(in), pointer :: mesh
      type (state_type) , intent(in), pointer ::  state, state_mean
      character (len=10), intent(in) :: date0
      character (len=3), intent(in) :: cem
      character (len=1024) :: filename
      real(kind=8) :: var_mean(1:mesh%Dim1, 1:mesh%Dim2)
      real(kind=8) :: var(1:mesh%Dim1, 1:mesh%Dim2)
      integer :: indice, k, sum_tot

      var_mean = 0.0
      var = 0.0
      
      if (( bins%bin_type .eq. 7 ).or.( bins%bin_type .eq. 8 )) then

         if ( bins%bin_type .eq. 7 ) then
            !! from Yann Michel meteo France : accumulate the vertical rain in var variables
            write(*,*)'Binning option = 7, dynamical mask based on vertical rain (4 classes) '

            indice =  get_state_indice(state,"qrain")
            do k = 1, mesh%dim3
               var(:,:) = var(:,:) + state%num(indice)%field%field3d%array(:,:,k)
               var_mean(:,:) = var_mean(:,:) + state_mean%num(indice)%field%field3d%array(:,:,k)
            end do

            bins%bin2d%array = 1       ! mixed case
            where ((var.ge.qrain_th_high).and.(var_mean.ge.qrain_th_high))
               bins%bin2d%array = 3 ! Heavy Rain
            end where
            where ((var.lt.qrain_th_high).and.(var_mean.lt.qrain_th_high)&
                .and.(var.ge.qrain_th_low).and.(var_mean.ge.qrain_th_low))
              bins%bin2d%array = 2 ! Light Rain
            end where
              where ((var.lt.qrain_th_low).and.(var_mean.lt.qrain_th_low))
              bins%bin2d%array = 0 ! Clear
            end where

         else if ( bins%bin_type .eq. 8 ) then
            !! from Gael DESCOMBES MMM/NCAR acaps project 2013 : accumulate vertical hydrometeors in var variables
            write(*,*)'Binning option = 8, dynamical mask based on vertical cloud mixing ratio content (6 classes) '
            !! class 0 : no qcloud, no qrain, no qice
            !! class 1 : qcloud
            !! class 2 : qrain and qcloud
            !! class 3 : no qcloud, no qrain, qice
            !! class 4 : qcloud, qice, no qrain
            !! class 5 : qrain, qice, qrain 

            !! QCLOUD class (0,1)
            indice =  get_state_indice(state,"qcloud")
            do k = 1, mesh%dim3
               var(:,:) = var(:,:) + state%num(indice)%field%field3d%array(:,:,k)
               var_mean(:,:) = var_mean(:,:) + state_mean%num(indice)%field%field3d%array(:,:,k)
            end do
            bins%bin2d%array = 0
            where ( (var.ge.qrain_th_low).and.(var_mean.ge.qrain_th_low) )
               bins%bin2d%array = 1                
            end where 

            !! QRAIN+QCLOUD class (2)
            indice =  get_state_indice(state,"qrain")
            var = 0.0
            var_mean = 0.0
            do k = 1, mesh%dim3
               var(:,:) = var(:,:) +state%num(indice)%field%field3d%array(:,:,k)
               var_mean(:,:) = var_mean(:,:) +state_mean%num(indice)%field%field3d%array(:,:,k)
            end do
            where ( (var.ge.qrain_th_low).and.(var_mean.ge.qrain_th_low).and.(bins%bin2d%array.eq.1) )
               bins%bin2d%array = 2 
            end where
            !where ((var.lt.qrain_th_low).and.(var_mean.lt.qrain_th_low).and.(bins%bin2d%array.eq.1)) then
            !   bins%bin2d%array = 3
            !end where            

            !! QICE class (3,4)
            indice =  get_state_indice(state,"qice")
            var = 0.0
            var_mean = 0.0
            do k = 1, mesh%dim3
               var(:,:) = var(:,:) +state%num(indice)%field%field3d%array(:,:,k)
               var_mean(:,:) = var_mean(:,:) + state_mean%num(indice)%field%field3d%array(:,:,k)
            end do
            where  ( (var.ge.qrain_th_low).and.(var_mean.ge.qrain_th_low) ) 
               bins%bin2d%array = bins%bin2d%array + 3
            end where          

         end if       
 
         filename = 'mask'
         filename = trim(filename)//'/'//trim(filename)//'.'//date0(1:10)//'.e'//cem
         write(*,*)'bins%bin_type ',bins%bin_type    
         call write_variable(filename, bins%bin2d , mesh, 0)
         ! diagnostics
         ! do k = 1,  bins%num_bins2d
         !   where (bins%bin2d%array==(k-1))
         !      var=1.0
         !   elsewhere
         !      var=0.0
         !   end where
         !   sum_tot = sum(var)
         !   write(*,*)'nbins ',k,sum_tot
         !end do


      end if


  end subroutine compute_dynamic_mask

!=================================================================================
! WRITE A VARIABLE IN A SINGLE FILE
!=================================================================================

   subroutine write_variable3dReal(filename, field, mesh, itime)

      implicit none

      integer, dimension(10) :: dimlist
      type (io_output_object) :: output_obj
      type (field3DReal), intent(in) :: field
      type (mesh_type), intent(in) :: mesh
      character (len=*), intent(in):: filename
      integer, intent(in) :: itime
      integer :: nferr
      integer :: varID
      integer, dimension(4) :: start4, count4
      integer, dimension(3) :: start3, count3

      include 'netcdf.inc'

      call io_output_init(filename,output_obj)

      nferr = nf_def_dim(output_obj%wr_ncid, 'lon', mesh%Dim1, output_obj%wrDimIDlon)
      nferr = nf_def_dim(output_obj%wr_ncid, 'lat', mesh%Dim2, output_obj%wrDimIDlat)
      nferr = nf_def_dim(output_obj%wr_ncid, 'lev', mesh%Dim3, output_obj%wrDimIDlev)
      dimlist(1) = output_obj % wrDimIDlon
      dimlist(2) = output_obj % wrDimIDlat
      dimlist(3) = output_obj % wrDimIDlev
      !field%ioinfo%ID = 1
      if ( itime == 0 ) then
         nferr = nf_def_var(output_obj % wr_ncid, trim(field%ioinfo%fieldName), NF_DOUBLE, 3, dimlist, output_obj%wrVarIDvar0)
      else 
         nferr = nf_def_dim(output_obj%wr_ncid, 'Time', NF_UNLIMITED, output_obj%wrDimIDTime)
         dimlist(4) = output_obj % wrDimIDtime   
         nferr = nf_def_var(output_obj % wr_ncid, trim(field%ioinfo%fieldName), NF_DOUBLE, 4, dimlist, output_obj%wrVarIDvar0)
      end if
      call check_err(nferr,field%ioinfo%fieldName)
      nferr = nf_enddef(output_obj % wr_ncid)
      nferr = nf_sync(output_obj % wr_ncid)

      start4(1) = field % ioinfo % start(1)
      start4(2) = field % ioinfo % start(2)
      start4(3) = field % ioinfo % start(3)
      start4(4) = 1
      count4(1) = field % ioinfo % count(1)
      count4(2) = field % ioinfo % count(2)
      count4(3) = field % ioinfo % count(3)
      count4(4) = 1
      start3(1:3) = start4(1:3)
      count3(1:3) = count4(1:3)
      write(*,*)'start3 ',count3, field %ioinfo%fieldname, filename

      varID = output_obj % wrVarIDvar0
      if ( itime == 0 ) then
         nferr = nf_put_vara_double(output_obj % wr_ncid, VarID, start3, count3, field % array)
      else
         nferr = nf_put_vara_double(output_obj % wr_ncid, VarID, start4, count4, field % array)
      end if
      call check_err(nferr,field%ioinfo%fieldName)
      write(*,*)'write_variable2dReal output_obj%wrVarIDvar0  nf_strerror ', nf_strerror(nferr)
      nferr = nf_sync(output_obj % wr_ncid)
      nferr = nf_close(output_obj % wr_ncid) 

   end subroutine write_variable3dReal


   subroutine write_variable2dReal(filename, field, mesh, itime)

      implicit none

      type (field2DReal), intent(in) :: field
      character (len=*), intent(in) :: filename
      integer, intent(in) :: itime
      type (mesh_type), intent(in) :: mesh
      type (io_output_object) :: output_obj
      integer :: nferr
      integer :: varID
      integer, dimension(2) :: start2, count2
      integer, dimension(3) :: start3, count3
      integer, dimension(10) :: dimlist

      include 'netcdf.inc'

      call io_output_init(filename,output_obj)

      nferr = nf_def_dim(output_obj%wr_ncid, 'lon', mesh%Dim1, output_obj%wrDimIDlon)
      nferr = nf_def_dim(output_obj%wr_ncid, 'lat', mesh%Dim2, output_obj%wrDimIDlat)
      dimlist(1) = output_obj % wrDimIDlon
      dimlist(2) = output_obj % wrDimIDlat
      field%ioinfo%ID = 1
      if ( itime == 0 ) then
         nferr = nf_def_var(output_obj % wr_ncid, trim(field%ioinfo%fieldName), NF_DOUBLE, 2, dimlist, output_obj%wrVarIDvar0)
      else
         nferr = nf_def_dim(output_obj%wr_ncid, 'Time', NF_UNLIMITED, output_obj%wrDimIDTime)
         dimlist(3) = output_obj % wrDimIDtime
         nferr = nf_def_var(output_obj % wr_ncid, trim(field%ioinfo%fieldName), NF_DOUBLE, 3, dimlist, output_obj%wrVarIDvar0)
      end if
      call check_err(nferr,field%ioinfo%fieldName)
      write(*,*)'write_variable2dReal output_obj%wrVarIDvar0  nf_strerror ', nf_strerror(nferr),output_obj%wrVarIDvar0
      nferr = nf_enddef(output_obj % wr_ncid)
      nferr = nf_sync(output_obj % wr_ncid)

      start3(1) = field % ioinfo % start(1)
      start3(2) = field % ioinfo % start(2)
      start3(3) = 1
      count3(1) = field % ioinfo % count(1)
      count3(2) = field % ioinfo % count(2)
      count3(3) = 1
      start2(1:2) = start3(1:2)
      count2(1:2) = count3(1:2)

      varID = output_obj % wrVarIDvar0
      if ( itime == 0 ) then
         nferr = nf_put_vara_double(output_obj % wr_ncid, VarID, start2, count2, field % array)
      else
         nferr = nf_put_vara_double(output_obj % wr_ncid, VarID, start3, count3, field % array)
      end if
      call check_err(nferr,field%ioinfo%fieldName)
      nferr = nf_sync(output_obj % wr_ncid)
      nferr = nf_close(output_obj % wr_ncid)

   end subroutine write_variable2dReal


   subroutine write_variable1dReal(filename, field, mesh, itime)

      implicit none

      type (io_output_object) :: output_obj
      type (field1DReal), intent(in) :: field
      character (len=*), intent(in) :: filename
      integer, intent(in) :: itime
      type (mesh_type), intent(in) :: mesh 
      integer ::  nferr
      integer :: varID
      integer, dimension(2) :: start1, count1
      integer, dimension(3) :: start2, count2
      integer, dimension(10) :: dimlist

      include 'netcdf.inc'

      call io_output_init(filename,output_obj)

      nferr = nf_def_dim(output_obj%wr_ncid, 'lev', mesh%Dim3, output_obj%wrDimIDlev)
      dimlist(1) = output_obj % wrDimIDlev
      !field%ioinfo%ID = 1
      if ( itime == 0 ) then
         nferr = nf_def_var(output_obj % wr_ncid, trim(field%ioinfo%fieldName), NF_DOUBLE, 1, dimlist, output_obj%wrVarIDvar0)
      else
         nferr = nf_def_dim(output_obj%wr_ncid, 'Time', NF_UNLIMITED, output_obj%wrDimIDTime)
         dimlist(2) = output_obj % wrDimIDtime
         nferr = nf_def_var(output_obj % wr_ncid, trim(field%ioinfo%fieldName), NF_DOUBLE, 2, dimlist, output_obj%wrVarIDvar0)
         write(*,*)'open ',nf_strerror(nferr),trim(field%ioinfo%fieldName)
      end if
      call check_err(nferr,field%ioinfo%fieldName)
      nferr = nf_enddef(output_obj % wr_ncid)
      nferr = nf_sync(output_obj % wr_ncid)

      start2(1) = field % ioinfo % start(1)
      start2(2) = 1
      count2(1) = field % ioinfo % count(1)
      count2(2) = 1
      start1(1) = start2(1)
      count1(1) = count2(1)

      varID = output_obj % wrVarIDvar0
      if ( itime == 0 ) then
         nferr = nf_put_vara_double(output_obj % wr_ncid, VarID, start1, count1, field % array)
      else
         nferr = nf_put_vara_double(output_obj % wr_ncid, VarID, start2, count2, field % array)
      end if
      call check_err(nferr,field%ioinfo%fieldName)
      nferr = nf_sync(output_obj % wr_ncid)
      nferr = nf_close(output_obj % wr_ncid)

   end subroutine write_variable1dReal


   subroutine write_variable2dInteger(filename, field, mesh, itime)

      implicit none

      type (io_output_object) :: output_obj
      type (field2DInteger), intent(in) :: field
      character (len=*), intent(in) :: filename
      integer, intent(in) :: itime
      type (mesh_type), intent(in) :: mesh
      integer :: nferr
      integer :: varID
      integer, dimension(2) :: start2, count2
      integer, dimension(3) :: start3, count3
      integer, dimension(10) :: dimlist
 
      include 'netcdf.inc'

      call io_output_init(filename,output_obj)

      nferr = nf_def_dim(output_obj%wr_ncid, 'lon', mesh%Dim1, output_obj%wrDimIDlon)
      nferr = nf_def_dim(output_obj%wr_ncid, 'lat', mesh%Dim2, output_obj%wrDimIDlat)
      dimlist(1) = output_obj % wrDimIDlon
      dimlist(2) = output_obj % wrDimIDlat
      !field%ioinfo%ID = 1
      if ( itime == 0 ) then
         nferr = nf_def_var(output_obj % wr_ncid, trim(field%ioinfo%fieldName), NF_INT, 2, dimlist, output_obj%wrVarIDvar0)
      else
         nferr = nf_def_dim(output_obj%wr_ncid, 'Time', NF_UNLIMITED, output_obj%wrDimIDTime)
         dimlist(3) = output_obj % wrDimIDtime
         nferr = nf_def_var(output_obj % wr_ncid, trim(field%ioinfo%fieldName), NF_INT, 3, dimlist, output_obj%wrVarIDvar0)
      end if
      call check_err(nferr,field%ioinfo%fieldName)
      nferr = nf_enddef(output_obj % wr_ncid)
      nferr = nf_sync(output_obj % wr_ncid)

      start3(1) = field % ioinfo % start(1)
      start3(2) = field % ioinfo % start(2)
      start3(3) = 1
      count3(1) = field % ioinfo % count(1)
      count3(2) = field % ioinfo % count(2)
      count3(3) = 1
      start2(1:2) = start3(1:2)
      count2(1:2) = count3(1:2)

      varID = output_obj % wrVarIDvar0
      if ( itime == 0 ) then
         nferr = nf_put_vara_int(output_obj % wr_ncid, VarID, start2, count2, field % array)
      else
         nferr = nf_put_vara_int(output_obj % wr_ncid, VarID, start3, count3, field % array)
      end if
      call check_err(nferr,field%ioinfo%fieldName)

      nferr = nf_sync(output_obj % wr_ncid)

      nferr = nf_close(output_obj % wr_ncid)

   end subroutine write_variable2dInteger


   subroutine write_variable3dInteger(filename, field, mesh, itime)

      implicit none

      integer, dimension(10) :: dimlist
      type (io_output_object) :: output_obj
      type (field3DInteger), intent(in) :: field
      type (mesh_type), intent(in) :: mesh
      character (len=*), intent(in):: filename
      integer, intent(in) :: itime
      integer :: nferr
      integer :: varID
      integer, dimension(4) :: start4, count4
      integer, dimension(3) :: start3, count3

      include 'netcdf.inc'

      call io_output_init(filename,output_obj)

      nferr = nf_def_dim(output_obj%wr_ncid, 'lon', mesh%Dim1, output_obj%wrDimIDlon)
      nferr = nf_def_dim(output_obj%wr_ncid, 'lat', mesh%Dim2, output_obj%wrDimIDlat)
      nferr = nf_def_dim(output_obj%wr_ncid, 'lev', mesh%Dim3, output_obj%wrDimIDlev)
      dimlist(1) = output_obj % wrDimIDlon
      dimlist(2) = output_obj % wrDimIDlat
      dimlist(3) = output_obj % wrDimIDlev
      !field%ioinfo%ID = 1
      if ( itime == 0 ) then
         nferr = nf_def_var(output_obj % wr_ncid, trim(field%ioinfo%fieldName), NF_INT, 3, dimlist, output_obj%wrVarIDvar0)
      else
         nferr = nf_def_dim(output_obj%wr_ncid, 'Time', NF_UNLIMITED, output_obj%wrDimIDTime)
         dimlist(4) = output_obj % wrDimIDtime
         nferr = nf_def_var(output_obj % wr_ncid, trim(field%ioinfo%fieldName), NF_INT, 4, dimlist, output_obj%wrVarIDvar0)
      end if
      call check_err(nferr,field%ioinfo%fieldName)
      nferr = nf_enddef(output_obj % wr_ncid)
      nferr = nf_sync(output_obj % wr_ncid)

      start4(1) = field % ioinfo % start(1)
      start4(2) = field % ioinfo % start(2)
      start4(3) = field % ioinfo % start(3)
      start4(4) = 1
      count4(1) = field % ioinfo % count(1)
      count4(2) = field % ioinfo % count(2)
      count4(3) = field % ioinfo % count(3)
      count4(4) = 1
      start3(1:3) = start4(1:3)
      count3(1:3) = count4(1:3)

      varID = output_obj % wrVarIDvar0
      if ( itime == 0 ) then
         nferr = nf_put_vara_int(output_obj % wr_ncid, VarID, start3, count3, field % array)
      else
         nferr = nf_put_vara_int(output_obj % wr_ncid, VarID, start4, count4, field % array)
      end if
      call check_err(nferr,field%ioinfo%fieldName)
      nferr = nf_sync(output_obj % wr_ncid)
      nferr = nf_close(output_obj % wr_ncid)

   end subroutine write_variable3dInteger







   !! WRITING BINNING VARIANCE !!
   
   subroutine write_variable3dReal_bin(filename, field, mesh, bins, itime)

      implicit none

      integer, dimension(10) :: dimlist
      type (io_output_object) :: output_obj
      type (field3DReal), intent(in) :: field
      type (bins_type), intent(in) :: bins
      type (mesh_type), intent(in) :: mesh
      character (len=*), intent(in):: filename
      integer, intent(in) :: itime
      integer :: nferr
      integer :: varID
      integer, dimension(4) :: start4, count4
      integer, dimension(3) :: start3, count3

      include 'netcdf.inc'

      call io_output_init(filename,output_obj)

      nferr = nf_def_dim(output_obj%wr_ncid, 'num_bins2d', bins%num_bins2d, output_obj%wrDimIDbin2d) 
      nferr = nf_def_dim(output_obj%wr_ncid, 'lev', mesh%Dim3, output_obj%wrDimIDlev)
      dimlist(1) = output_obj % wrDimIDlev
      dimlist(2) = output_obj % wrDimIDlev
      dimlist(3) = output_obj % wrDimIDbin2d
      
      if ( itime == 0 ) then
         nferr = nf_def_var(output_obj % wr_ncid, trim(field%ioinfo%fieldName), NF_DOUBLE, 3, dimlist, output_obj%wrVarIDvar0)
      else
         nferr = nf_def_dim(output_obj%wr_ncid, 'Time', NF_UNLIMITED, output_obj%wrDimIDTime)
         dimlist(4) = output_obj % wrDimIDtime
         nferr = nf_def_var(output_obj % wr_ncid, trim(field%ioinfo%fieldName), NF_DOUBLE, 4, dimlist, output_obj%wrVarIDvar0)
      end if
      call check_err(nferr,field%ioinfo%fieldName)
      nferr = nf_enddef(output_obj % wr_ncid)
      nferr = nf_sync(output_obj % wr_ncid)

      start4(1) = field % ioinfo % start(1)
      start4(2) = field % ioinfo % start(2)
      start4(3) = field % ioinfo % start(3)
      start4(4) = 1
      count4(1) = field % ioinfo % count(1)
      count4(2) = field % ioinfo % count(2)
      count4(3) = field % ioinfo % count(3)
      count4(4) = 1
      start3(1:3) = start4(1:3)
      count3(1:3) = count4(1:3)
      write(*,*)'start3 ',count3

      varID = output_obj % wrVarIDvar0
      if ( itime == 0 ) then
         nferr = nf_put_vara_double(output_obj % wr_ncid, VarID, start3, count3, field % array)
      else
         nferr = nf_put_vara_double(output_obj % wr_ncid, VarID, start4, count4, field % array)
      end if
      call check_err(nferr,field%ioinfo%fieldName)
      write(*,*)'nf_strerror ', nf_strerror(nferr)
      
      nferr = nf_sync(output_obj % wr_ncid)
      nferr = nf_close(output_obj % wr_ncid)

   end subroutine write_variable3dReal_bin

   
   subroutine write_variable2dReal_bin(filename, field, mesh, bins, itime)

      implicit none

      type (io_output_object) :: output_obj
      type (field2DReal), intent(in) :: field
      character (len=*), intent(in) :: filename
      integer, intent(in) :: itime
      type (bins_type), intent(in) :: bins
      type (mesh_type), intent(in) :: mesh 
      integer :: nferr
      integer :: varID
      integer, dimension(2) :: start2, count2
      integer, dimension(3) :: start3, count3
      integer, dimension(10) :: dimlist

      include 'netcdf.inc'

      call io_output_init(filename,output_obj)

      nferr = nf_def_dim(output_obj%wr_ncid, 'num_bins2d', bins%num_bins2d, output_obj%wrDimIDbin2d) 
      nferr = nf_def_dim(output_obj%wr_ncid, 'lev', mesh%Dim3, output_obj%wrDimIDlev)
      dimlist(1) = output_obj % wrDimIDlev
      dimlist(2) = output_obj % wrDimIDbin2d
      field%ioinfo%ID = 1
      if ( itime == 0 ) then
         nferr = nf_def_var(output_obj % wr_ncid, trim(field%ioinfo%fieldName), NF_DOUBLE, 2, dimlist, output_obj%wrVarIDvar0)
      else
         nferr = nf_def_dim(output_obj%wr_ncid, 'Time', NF_UNLIMITED, output_obj%wrDimIDTime)
         dimlist(3) = output_obj % wrDimIDtime
         nferr = nf_def_var(output_obj % wr_ncid, trim(field%ioinfo%fieldName), NF_DOUBLE, 3, dimlist, output_obj%wrVarIDvar0)
         write(*,*)'open ',nf_strerror(nferr),trim(field%ioinfo%fieldName)
      end if
      call check_err(nferr,field%ioinfo%fieldName)
      write(*,*)'write_variable2dReal output_obj%wrVarIDvar0  nf_strerror ', nf_strerror(nferr),output_obj%wrVarIDvar0
      nferr = nf_enddef(output_obj % wr_ncid)
      nferr = nf_sync(output_obj % wr_ncid)

      start3(1) = field % ioinfo % start(1)
      start3(2) = field % ioinfo % start(2)
      start3(3) = 1
      count3(1) = field % ioinfo % count(1)
      count3(2) = field % ioinfo % count(2)
      count3(3) = 1
      start2(1:2) = start3(1:2)
      count2(1:2) = count3(1:2)

      varID = output_obj % wrVarIDvar0
      if ( itime == 0 ) then
         nferr = nf_put_vara_double(output_obj % wr_ncid, VarID, start2, count2, field % array)
      else
         nferr = nf_put_vara_double(output_obj % wr_ncid, VarID, start3, count3, field % array)
      end if
      call check_err(nferr,field%ioinfo%fieldName)
      write(*,*)'write_variable2dReal nf_strerror ', nf_strerror(nferr)
      nferr = nf_sync(output_obj % wr_ncid)
      nferr = nf_close(output_obj % wr_ncid)

   end subroutine write_variable2dReal_bin


   subroutine write_variable1dReal_bin(filename, field, mesh, bins, itime)

      implicit none

      type (io_output_object) :: output_obj
      type (field1DReal), intent(in) :: field
      character (len=*), intent(in) :: filename
      integer, intent(in) :: itime
      type (mesh_type), intent(in) :: mesh
      type (bins_type), intent(in) :: bins 
      integer :: vv, nferr
      real :: var
      integer :: varID
      integer, dimension(2) :: start1, count1
      integer, dimension(3) :: start2, count2
      integer, dimension(10) :: dimlist

      include 'netcdf.inc'

      call io_output_init(filename,output_obj)

      if ( bins%num_bins .eq. field%ioinfo%count(1) ) then
         nferr = nf_def_dim(output_obj%wr_ncid, 'num_bins', bins%num_bins, output_obj%wrDimIDbin)
         dimlist(1) = output_obj % wrDimIDbin
      else
         nferr = nf_def_dim(output_obj%wr_ncid, 'num_bins2d', bins%num_bins2d, output_obj%wrDimIDbin2d)
         dimlist(1) = output_obj % wrDimIDbin2d
      end if
      call check_err(nferr,field%ioinfo%fieldName)
      
      if ( itime == 0 ) then
         nferr = nf_def_var(output_obj % wr_ncid, trim(field%ioinfo%fieldName), NF_DOUBLE, 1, dimlist, output_obj%wrVarIDvar0)
      else
         nferr = nf_def_dim(output_obj%wr_ncid, 'Time', NF_UNLIMITED, output_obj%wrDimIDTime)
         dimlist(2) = output_obj % wrDimIDtime
         nferr = nf_def_var(output_obj % wr_ncid, trim(field%ioinfo%fieldName), NF_DOUBLE, 2, dimlist, output_obj%wrVarIDvar0)
         write(*,*)'open ',nf_strerror(nferr),trim(field%ioinfo%fieldName)
      end if
      call check_err(nferr,field%ioinfo%fieldName)

      write(*,*)'write_variable1dReal output_obj%wrVarIDvar0  nf_strerror ', nf_strerror(nferr),output_obj%wrVarIDvar0
      nferr = nf_enddef(output_obj % wr_ncid)
      nferr = nf_sync(output_obj % wr_ncid)

      start2(1) = field % ioinfo % start(1)
      start2(2) = 1
      count2(1) = field % ioinfo % count(1)
      count2(2) = 1
      start1(1) = start2(1)
      count1(1) = count2(1)

      varID = output_obj % wrVarIDvar0
      if ( itime == 0 ) then
         nferr = nf_put_vara_double(output_obj % wr_ncid, VarID, start1, count1, field % array)
      else
         nferr = nf_put_vara_double(output_obj % wr_ncid, VarID, start2, count2, field % array)
      end if
      call check_err(nferr,field%ioinfo%fieldName)
      nferr = nf_sync(output_obj % wr_ncid)
      nferr = nf_close(output_obj % wr_ncid)

   end subroutine write_variable1dReal_bin


   !! WRITTING THE STATE !!

   subroutine write_state_into_variable_file(state, mesh, date, ce)

      implicit none
      type (state_type), intent(in) :: state
      type (mesh_type), intent(in) :: mesh
      character (len=StrLen), intent(in) :: date
      character (len=3),  intent(in) :: ce
      character (len=1024) :: filename
      character (len=1024) :: variable
      integer :: vv, itime

      itime = 1 
  
      do vv=1, state%nvar
         if ( state%num(vv)%IDdim == 3  ) then
            variable = trim(state%num(vv)%field%field3d%ioinfo%fieldName)
            filename = trim(variable)//'/'//trim(variable)//'.'//date(1:10)
            filename = trim(filename)//'.e'//ce
            call write_variable3dReal(filename, state%num(vv)%field%field3d, mesh, itime)
         else if ( state%num(vv)%IDdim == 2  ) then
            variable = trim(state%num(vv)%field%field2d%ioinfo%fieldName)
            filename = trim(variable)//'/'//trim(variable)//'.'//date(1:10)
            filename = trim(filename)//'.e'//ce
            call write_variable2dReal(filename, state%num(vv)%field%field2d, mesh, itime)
         else if ( state%num(vv)%IDdim == 1  ) then
            variable = state%num(vv)%field%field1d%ioinfo%fieldName
            filename = trim(variable)//'/'//date(1:10)
            filename = trim(filename)//'.'//trim(variable)//'.e'//ce
            call write_variable(filename, state%num(vv)%field%field1d, mesh, itime)
         end if
      end do      

   end subroutine write_state_into_variable_file


!===================================================================================
! WRITE THE MESH GRID
!===================================================================================

   subroutine io_output_mesh(mesh,filename)

      implicit none

      include 'netcdf.inc'

      type (io_output_object) :: output_obj
      type (mesh_type), intent(in) :: mesh
      character (len=*), intent(in), optional :: filename
      integer :: nferr
      integer, dimension(10) :: dimlist


      ! create a new file with a name defined by the user or by default
      if (present(filename)) then
            output_obj % filename = trim(filename)
      else 
            output_obj % filename = 'mesh_grid.nc'
      end if
      call io_output_init(output_obj % filename,output_obj)

      ! create a new file, erase the old one if it is present / Define dimensions
      nferr = nf_def_dim(output_obj%wr_ncid, 'lon', mesh%Dim1, output_obj%wrDimIDlon)
      nferr = nf_def_dim(output_obj%wr_ncid, 'lat', mesh%Dim2, output_obj%wrDimIDlat)
      nferr = nf_def_dim(output_obj%wr_ncid, 'lev', mesh%Dim3, output_obj%wrDimIDlev)
      if (associated(mesh%mapfac_u)) then
         nferr = nf_def_dim(output_obj%wr_ncid, 'lon_u', mesh%Dim1u, output_obj%wrDimIDlonu)
         nferr = nf_def_dim(output_obj%wr_ncid, 'lat_u', mesh%Dim2u, output_obj%wrDimIDlatu)
      end if
      if (associated(mesh%mapfac_v)) then
         nferr = nf_def_dim(output_obj%wr_ncid, 'lon_v', mesh%Dim1v, output_obj%wrDimIDlonv)
         nferr = nf_def_dim(output_obj%wr_ncid, 'lat_v', mesh%Dim2v, output_obj%wrDimIDlatv)
      end if

      call io_output_mesh_def(mesh,output_obj)

      nferr = nf_enddef(output_obj % wr_ncid)
      nferr = nf_sync(output_obj % wr_ncid)
      write(*,*)'nf_strerror ', nf_strerror(nferr)

      call io_output_mesh_var(mesh,output_obj)

      nferr = nf_close(output_obj % wr_ncid)

   end subroutine io_output_mesh


   subroutine io_output_mesh_def(mesh,output_obj)

      implicit none

      include 'netcdf.inc'

      type (io_output_object), intent(inout) :: output_obj
      type (mesh_type), intent(in) :: mesh
      integer :: nferr
      integer, dimension(10) :: dimlist

      ! define latitude longitude
      dimlist(1) = output_obj % wrDimIDlon
      dimlist(2) = output_obj % wrDimIDlat
      nferr = nf_def_var(output_obj % wr_ncid, mesh%lat%ioinfo%fieldName, NF_DOUBLE, 2, dimlist, output_obj % wrVarIDlat)
      nferr = nf_def_var(output_obj % wr_ncid, mesh%lon%ioinfo%fieldName, NF_DOUBLE, 2, dimlist, output_obj % wrVarIDlon)
      ! define mapfac_u
      if (associated(mesh%mapfac_u)) then
         dimlist(1) = output_obj % wrDimIDlonu
         dimlist(2) = output_obj % wrDimIDlatu
         nferr = nf_def_var(output_obj % wr_ncid, mesh%mapfac_u%ioinfo%fieldName, NF_DOUBLE, 2, dimlist, output_obj % wrVarIDmapfu)
      end if
      ! define mapfac_v
      if (associated(mesh%mapfac_v)) then
         dimlist(1) = output_obj % wrDimIDlonv 
         dimlist(2) = output_obj % wrDimIDlatv
         nferr = nf_def_var(output_obj % wr_ncid, mesh%mapfac_v%ioinfo%fieldName, NF_DOUBLE, 2, dimlist, output_obj % wrVarIDmapfv)
      end if
      ! define znu
      if (associated(mesh%znu)) then
         dimlist(1) = output_obj % wrDimIDlev
         nferr = nf_def_var(output_obj % wr_ncid, mesh%znu%ioinfo%fieldName, NF_DOUBLE, 1, dimlist, output_obj % wrVarIDznu)
      end if
      ! define mapfac_m
      dimlist(1) = output_obj % wrDimIDlon
      dimlist(2) = output_obj % wrDimIDlat
      nferr = nf_def_var(output_obj % wr_ncid, mesh%mapfac_m%ioinfo%fieldName, NF_DOUBLE, 2, dimlist, output_obj % wrVarIDmapfm)
      ! define height
      dimlist(1) = output_obj % wrDimIDlon
      dimlist(2) = output_obj % wrDimIDlat
      dimlist(3) = output_obj % wrDimIDlev
      nferr = nf_def_var(output_obj % wr_ncid, mesh%hgt%ioinfo%fieldName, NF_DOUBLE, 3, dimlist, output_obj % wrVarIDhgt)
      ! define ds
      dimlist(1) = 1
      nferr = nf_def_var(output_obj % wr_ncid,'ds', NF_DOUBLE, 0, dimlist, output_obj % wrVarIDds)

   end subroutine io_output_mesh_def


   subroutine io_output_mesh_var(mesh,output_obj)

      implicit none

      include 'netcdf.inc'

      type (io_output_object), intent(inout) :: output_obj
      type (mesh_type), intent(in) :: mesh
      integer :: nferr

      ! write mesh grid
      call io_output_field(output_obj, mesh%lat)
      call io_output_field(output_obj, mesh%lon)
      if (associated(mesh%mapfac_u)) then
         call io_output_field(output_obj, mesh%mapfac_u)
      end if
      if (associated(mesh%mapfac_v)) then
         call io_output_field(output_obj, mesh%mapfac_v)
      end if
      if (associated(mesh%znu)) then
         call io_output_field(output_obj, mesh%znu)
      end if
      call io_output_field(output_obj, mesh%mapfac_m)
      call io_output_field(output_obj, mesh%hgt)
      call io_output_field(output_obj, mesh%ds)

   end subroutine io_output_mesh_var


!===================================================================
! WRITE EIGEN VARIABLES 
!===================================================================

   subroutine io_output_eigen_def(eigen, output_obj)

      implicit none

      include 'netcdf.inc'

      type (io_output_object), intent(inout) :: output_obj
      type (eigen_type), intent(inout) :: eigen 
      integer :: nferr, ivarnd
      integer, dimension(10) :: dimlist

      ivarnd = output_obj % ivarnd

      if ( eigen%vec%IDdim .eq. 3  ) then
         ivarnd = ivarnd + 1
         dimlist(1) = output_obj % wrDimIDlev
         dimlist(2) = output_obj % wrDimIDlev
         dimlist(3) = output_obj % wrDimIDbin2d
         nferr = nf_def_var(output_obj % wr_ncid, trim(eigen%vec%field%field3d%ioinfo%fieldName), NF_DOUBLE, 3, dimlist, output_obj%wrVarIDvarnd(ivarnd))
         eigen%vec%field%field3d%ioinfo%ID = ivarnd
         ivarnd = ivarnd + 1 
         dimlist(1) = output_obj % wrDimIDlev
         dimlist(2) = output_obj % wrDimIDbin2d
         nferr = nf_def_var(output_obj % wr_ncid, trim(eigen%val%field%field2d%ioinfo%fieldName), NF_DOUBLE, 2, dimlist, output_obj%wrVarIDvarnd(ivarnd))
         eigen%val%field%field2d%ioinfo%ID = ivarnd

      else if ( eigen%vec%IDdim .eq. 2  ) then
         ivarnd = ivarnd + 1
         dimlist(1) = output_obj % wrDimIDlev
         dimlist(2) = output_obj % wrDimIDlev
         nferr = nf_def_var(output_obj % wr_ncid, trim(eigen%vec%field%field2d%ioinfo%fieldName), NF_DOUBLE, 2, dimlist, output_obj%wrVarIDvarnd(ivarnd))
         eigen%vec%field%field2d%ioinfo%ID = ivarnd
         ivarnd = ivarnd + 1
         dimlist(1) = output_obj % wrDimIDlev
         nferr = nf_def_var(output_obj % wr_ncid, trim(eigen%val%field%field1d%ioinfo%fieldName), NF_DOUBLE, 1, dimlist, output_obj%wrVarIDvarnd(ivarnd))
         eigen%val%field%field1d%ioinfo%ID = ivarnd

      else if ( eigen%vec%IDdim .eq. 1  ) then
         ivarnd = ivarnd + 1
         dimlist(1) = output_obj % wrDimIDbin2d
         nferr = nf_def_var(output_obj % wr_ncid, trim(eigen%vec%field%field1d%ioinfo%fieldName), NF_DOUBLE, 1, dimlist, output_obj%wrVarIDvarnd(ivarnd))
         eigen%vec%field%field1d%ioinfo%ID = ivarnd
         ivarnd = ivarnd + 1
         nferr = nf_def_var(output_obj % wr_ncid, trim(eigen%val%field%field1d%ioinfo%fieldName), NF_DOUBLE, 1, dimlist, output_obj%wrVarIDvarnd(ivarnd))
         eigen%val%field%field1d%ioinfo%ID = ivarnd

      else if ( eigen%vec%IDdim .eq. 0  ) then
         ivarnd = ivarnd + 1
         dimlist(1) = 1 
         nferr = nf_def_var(output_obj % wr_ncid, trim(eigen%vec%field%field0d%ioinfo%fieldName), NF_DOUBLE, 0, dimlist, output_obj%wrVarIDvarnd(ivarnd))
         eigen%vec%field%field0d%ioinfo%ID = ivarnd
         ivarnd = ivarnd + 1
         nferr = nf_def_var(output_obj % wr_ncid, trim(eigen%val%field%field0d%ioinfo%fieldName), NF_DOUBLE, 0, dimlist, output_obj%wrVarIDvarnd(ivarnd))
         eigen%val%field%field0d%ioinfo%ID = ivarnd

      end if
      
      output_obj % ivarnd = ivarnd

   end subroutine io_output_eigen_def

   
   subroutine io_output_eigen_var(eigen,output_obj)

      implicit none

      include 'netcdf.inc'

      type (io_output_object), intent(inout) :: output_obj
      type (eigen_type), intent(in) :: eigen
      integer :: nferr

      ! write eigen value
      if (eigen%vec%IDdim .eq. 3) then 
         call io_output_field(output_obj, eigen%vec%field%field3d)
         call io_output_field(output_obj, eigen%val%field%field2d)
      else if (eigen%vec%IDdim .eq. 2) then
         call io_output_field(output_obj, eigen%vec%field%field2d)
         call io_output_field(output_obj, eigen%val%field%field1d)
      else if (eigen%vec%IDdim .eq. 1) then
         call io_output_field(output_obj, eigen%vec%field%field1d)
         call io_output_field(output_obj, eigen%val%field%field1d)
      else if (eigen%vec%IDdim .eq. 0) then
         call io_output_field(output_obj, eigen%vec%field%field0d)
         call io_output_field(output_obj, eigen%val%field%field0d)
      end if

   end subroutine io_output_eigen_var

   
   subroutine io_output_eigen(filename, eigen, Dim3, num_bins2d)

      implicit none

      include 'netcdf.inc'

      type (io_output_object) :: output_obj
      type (eigen_type), intent(inout) :: eigen 
      character (len=*), intent(in):: filename
      integer , intent(in) ::  Dim3, num_bins2d
      integer :: nferr

      call io_output_init(filename, output_obj)

      nferr = nf_def_dim(output_obj%wr_ncid, 'num_bins2d', num_bins2d, output_obj%wrDimIDbin2d)
      nferr = nf_def_dim(output_obj%wr_ncid, 'lev', Dim3, output_obj%wrDimIDlev)

      call io_output_eigen_def(eigen,output_obj)
      nferr = nf_enddef(output_obj % wr_ncid)
      nferr = nf_sync(output_obj % wr_ncid)
      write(*,*)'nf_strerror ', nf_strerror(nferr)

      call io_output_eigen_var(eigen,output_obj)

      nferr = nf_close(output_obj % wr_ncid)

   end subroutine io_output_eigen


!===================================================================
! WRITE LENGTH SCALES VARIABLES 
!===================================================================

   subroutine io_output_lenscale_def(varname, lenscale, uh_method, output_obj)

      implicit none

      include 'netcdf.inc'
      type (io_output_object), intent(inout) :: output_obj
      type (fieldnD_type), intent(inout) :: lenscale
      character (len=*), intent(in) :: varname,  uh_method
      character (len=1024) :: varname0
      integer :: nferr
      integer, dimension(10) :: dimlist

      output_obj % ivarnd = output_obj % ivarnd + 1
      dimlist(1) = 1 

      if (trim(uh_method) == 'power') then
          varname0 = 'power_spectra_'//trim(varname)
          lenscale%field%field1d%ioinfo%fieldName = trim(varname0)
          dimlist(1) = output_obj % wrDimIDwav
          lenscale%field%field1d%ioinfo%ID = output_obj % ivarnd
          nferr = nf_def_var(output_obj % wr_ncid, trim(varname0), NF_DOUBLE, 1, dimlist, output_obj%wrVarIDvarnd(output_obj % ivarnd))
      else
          varname0 = 'lenscale_'//trim(varname)
          if ( lenscale%IDdim .eq. 1 ) then
             lenscale%field%field1d%ioinfo%fieldName = trim(varname0) 
             dimlist(1) = output_obj % wrDimIDlev
             lenscale%field%field1d%ioinfo%ID = output_obj % ivarnd
             nferr = nf_def_var(output_obj % wr_ncid, trim(varname0), NF_DOUBLE, 1, dimlist, output_obj%wrVarIDvarnd(output_obj % ivarnd))
          else if ( lenscale%IDdim .eq. 0 ) then
             lenscale%field%field0d%ioinfo%fieldName = trim(varname0) 
             lenscale%field%field0d%ioinfo%ID = output_obj % ivarnd
             nferr = nf_def_var(output_obj % wr_ncid, trim(varname0), NF_DOUBLE, 0, dimlist, output_obj%wrVarIDvarnd(output_obj % ivarnd))
          end if
      end if

   end subroutine io_output_lenscale_def


   subroutine io_output_fieldnd_var(fieldnd, itime, output_obj)
    
      implicit none

      type (io_output_object), intent(inout) :: output_obj
      type (fieldnd_type), intent(in) :: fieldnd
      integer, intent(in) :: itime

      if ( fieldnd%IDdim .eq. 0 ) then
         if ( itime == 0 ) then
            call io_output_field(output_obj,fieldnd%field%field0d)
         else
            call io_output_field_time(output_obj,fieldnd%field%field0d)
         end if 
      else if ( fieldnd%IDdim .eq. 1 ) then
         if ( itime == 0 ) then
            call io_output_field(output_obj,fieldnd%field%field1d)
         else
            call io_output_field_time(output_obj,fieldnd%field%field1d)
         end if
      else if ( fieldnd%IDdim .eq. 2 ) then
         if ( itime == 0 ) then
            call io_output_field(output_obj,fieldnd%field%field2d)
         else
            call io_output_field_time(output_obj,fieldnd%field%field2d)
         end if
      else if ( fieldnd%IDdim .eq. 3 ) then
         if ( itime == 0 ) then
            call io_output_field(output_obj,fieldnd%field%field3d)
         else
            call io_output_field_time(output_obj,fieldnd%field%field3d)
         end if
      end if

   end subroutine io_output_fieldnd_var


   subroutine io_output_lenscale(filename, varname, lenscale, uh_method, Dimm)

      implicit none

      include 'netcdf.inc'

      type (io_output_object) :: output_obj
      type (fieldnd_type),intent(inout) :: lenscale
      character (len=*),intent(in):: filename, varname, uh_method
      integer, intent(in) :: Dimm
      integer :: nferr, itime

      itime = 0   
      call io_output_init(filename, output_obj)

      if (trim(uh_method) == 'power') then
         nferr = nf_def_dim(output_obj%wr_ncid, 'wave', Dimm, output_obj%wrDimIDwav)
      else
         nferr = nf_def_dim(output_obj%wr_ncid, 'lev', Dimm, output_obj%wrDimIDlev)
      end if

      call io_output_lenscale_def(varname, lenscale, uh_method, output_obj)

      nferr = nf_enddef(output_obj % wr_ncid)
      nferr = nf_sync(output_obj % wr_ncid)
      write(*,*)'io_output_lenscale nf_strerror ', nf_strerror(nferr)

      call io_output_fieldnd_var(lenscale,itime,output_obj)

      nferr = nf_close(output_obj % wr_ncid)

   end subroutine io_output_lenscale

!===================================================================
! WRITE PARAMETERS OF THE NAMELIST
!===================================================================
   
    subroutine io_output_namelist_def(output_obj)

      implicit none

      include 'netcdf.inc'
      type (io_output_object), intent(inout) :: output_obj
      integer :: nferr, varID
      integer, dimension(10) :: dimlist


      write(*,*)'io_output_namelist '
      dimlist(1) = output_obj % wrDimIDStr
      nferr = nf_def_var(output_obj % wr_ncid, 'uh_method', NF_CHAR, 1, dimlist,  output_obj%wrVarIDnaml(1))

      dimlist(1) = output_obj % wrDimIDStr 
      dimlist(2) = output_obj % wrDimIDnvar
      nferr = nf_def_var(output_obj % wr_ncid, 'cv_list', NF_CHAR, 2, dimlist,  output_obj%wrVarIDnaml(2))

      dimlist(1) = output_obj % wrDimIDStr
      dimlist(2) = output_obj % wrDimIDnvar
      write(*,*)'output_obj % wrDimIDnvar ',output_obj % wrDimIDnvar
      nferr = nf_def_var(output_obj % wr_ncid, 'cv_listu', NF_CHAR, 2, dimlist,  output_obj%wrVarIDnaml(3))

      dimlist(1) = output_obj % wrDimIDnvar
      nferr = nf_def_var(output_obj % wr_ncid, 'vardim_list', NF_INT, 1, dimlist,  output_obj%wrVarIDnaml(4))

      dimlist(1) = output_obj % wrDimIDStr
      dimlist(2) = output_obj % wrDimIDncovar
      write(*,*)'output_obj % wrDimIDnvar ',output_obj % wrDimIDnvar
      nferr = nf_def_var(output_obj % wr_ncid, 'regcoeff_list', NF_CHAR, 2, dimlist,  output_obj%wrVarIDnaml(5))

      dimlist(1) = output_obj % wrDimIDncovar
      nferr = nf_def_var(output_obj % wr_ncid, 'regcoeffdim_list', NF_INT, 1, dimlist,  output_obj%wrVarIDnaml(6))

      ! define covariance ID table from namelist
      dimlist(1) = output_obj % wrDimIDnvar
      dimlist(2) = output_obj % wrDimIDnvar
      nferr = nf_def_var(output_obj % wr_ncid, 'covar_ID', NF_INT, 2, &
                  dimlist, output_obj%wrVarIDcovar)

       write(*,*)'nf_strerror', nf_strerror(nferr)
       write(*,*)'nferr ',nferr 
      

   end subroutine io_output_namelist_def


   subroutine io_output_namelist_var(uh_method, nvar, ncovar, cv_list, cv_listu, vardim_list, matrix, covar_ID, output_obj)

     implicit none

     include 'netcdf.inc'
     type (io_output_object), intent(inout) :: output_obj
     character (len=*), intent(in) :: uh_method
     character (len=*), dimension(1:nvar), intent(in) :: cv_list, cv_listu
     integer, dimension(1:nvar), intent(in) :: vardim_list
     integer, dimension(:,:), intent(in) ::  covar_ID 
     integer, intent(in) :: nvar     
     type (state_matrix_type), intent(in) :: matrix
     integer, intent(inout) :: ncovar

     character (len=StrLen), allocatable, dimension(:) :: regcoeff_list
     integer, allocatable, dimension(:) :: regcoeffdim_list 
     character (len=StrLen), allocatable :: tmp_list(:)
     character (len=StrLen) :: char_tmp
     type (field2DInteger), pointer :: covarID_field 
     integer, dimension(:), allocatable :: vardim_tmp
     integer :: nferr, varID, vv, ll
     integer, dimension(10) :: dimlist
     integer, dimension(1) :: start1, count1
     integer, dimension(2) :: start2, count2
     character (len=StrLen) :: fieldName

     ! write uh_method
     start1(1) = 1
     count1(1) = StrLen
     varID = output_obj%wrVarIDnaml(1) 
     write(*,*)'varID ',varID, trim(uh_method)
     char_tmp(1:StrLen) = '          '
     char_tmp(1:StrLen) = uh_method
     nferr = nf_put_vara_text(output_obj % wr_ncid, varID, start1, count1, char_tmp(1:StrLen))
     write(*,*)'nf_strerror ', nf_strerror(nferr)

     ! write the list of variables defined by the user
     start2(1) = 1
     start2(2) = 1
     count2(1) = StrLen
     count2(2) = nvar
     varID = output_obj%wrVarIDnaml(2)
     allocate(tmp_list(nvar))
     do vv = 1, nvar
        tmp_list(vv) = trim(cv_list(vv))
     end do
     nferr = nf_put_vara_text(output_obj % wr_ncid, varID, start2, count2, tmp_list)

     ! write the list of variables defined by the user
     varID = output_obj%wrVarIDnaml(3)
     do vv = 1, nvar
        tmp_list(vv) = trim(cv_listu(vv))
     end do
     nferr = nf_put_vara_text(output_obj % wr_ncid, varID, start2, count2, tmp_list)
     deallocate(tmp_list)
  
     ! write dimension of the cv variables
     start1(1) = 1
     count1(1) = nvar
     allocate(vardim_tmp(nvar))
     vardim_tmp = vardim_list(1:nvar)    
     varID = output_obj%wrVarIDnaml(4)
     nferr = nf_put_vara_int(output_obj % wr_ncid, varID, start1, count1, vardim_list)
     

     ! write the covariance table between CV
     fieldName = 'covarID'
     call allocate_field(covarID_field, fieldName, nvar, nvar)
     covarID_field%array(1:nvar,1:nvar) = covar_ID(1:nvar,1:nvar)   ! from the namelist and configure.f90 module
     start2(1) = 1
     start2(2) = 1
     count2(1) = nvar
     count2(2) = nvar
     varID = output_obj%wrVarIDcovar 
     nferr = nf_put_vara_int(output_obj % wr_ncid, varID, start2, count2, covarID_field%array)
     call deallocate_field(covarID_field)


     ! write list of dim an name of the regcoeff
     allocate(regcoeff_list(1:ncovar))
     allocate(regcoeffdim_list(1:ncovar))
     allocate(tmp_list(1:ncovar))
     call read_matrix_param(matrix, regcoeff_list, regcoeffdim_list, ncovar)

     do vv = 1, ncovar
        tmp_list(vv) = trim(regcoeff_list(vv))
     end do
     varID = output_obj%wrVarIDnaml(5)
     start2(1) = 1
     start2(2) = 1
     count2(1) = StrLen
     count2(2) = ncovar
     nferr = nf_put_vara_text(output_obj % wr_ncid, varID, start2, count2, tmp_list)

     start1(1) = 1
     count1(1) = ncovar
     varID = output_obj%wrVarIDnaml(6)
     nferr = nf_put_vara_int(output_obj % wr_ncid, varID, start1, count1, regcoeffdim_list)

     deallocate(tmp_list)
     deallocate(regcoeff_list)
     deallocate(regcoeffdim_list)

     write(*,*)'nf_strerror ', nf_strerror(nferr)
     deallocate(vardim_tmp)

   end subroutine io_output_namelist_var
   


!====================================================================
! WRITE THE BASICS
!====================================================================


   subroutine io_output_field0dReal(output_obj, field)

      implicit none

      type (io_output_object), intent(in) :: output_obj
      type (field0dReal), intent(in) :: field

      include 'netcdf.inc'

      integer :: nferr
      integer :: varID
      integer, dimension(1) :: start1, count1

      start1(1) = 1
      count1(1) = 1
      if (trim(field % ioinfo % fieldName) == 'ds') then
         varID = output_obj % wrVarIDds
      else if (trim(field % ioinfo % fieldName) == 'bin_type') then
         varID = output_obj % wrVarIDbinty
      else
         write(*,*)'filedName ',field % ioinfo % fieldName
         varID = output_obj % wrVarIDvarnd( field % ioinfo % ID  )
      end if

      nferr = nf_put_vara_double(output_obj % wr_ncid, varID, start1, count1, field % scalar)
      call check_err(nferr,field % ioinfo % fieldName)
      nferr = nf_sync(output_obj % wr_ncid)

   end subroutine io_output_field0dReal


   subroutine io_output_field1dReal(output_obj, field)

      implicit none

      type (io_output_object), intent(in) :: output_obj
      type (field1dReal), intent(in) :: field

      include 'netcdf.inc'

      integer :: nferr
      integer :: varID
      integer, dimension(1) :: start1, count1

      start1(1) = field % ioinfo % start(1)
      count1(1) = field % ioinfo % count(1)
      if (trim(field % ioinfo % fieldName) == 'znu') then
         varID = output_obj % wrVarIDznu
      else
         varID = output_obj % wrVarIDvarnd( field % ioinfo % ID  )
      end if
      nferr = nf_put_vara_double(output_obj % wr_ncid, VarID, start1, count1, field % array)
      call check_err(nferr,field % ioinfo % fieldName)
      nferr = nf_sync(output_obj % wr_ncid)
      write(*,*)'nf_strerror ', nf_strerror(nferr),varID
      nferr = nf_sync(output_obj % wr_ncid)


   end subroutine io_output_field1dReal
 
 
   subroutine io_output_field2dReal(output_obj, field)

      implicit none

      type (io_output_object), intent(in) :: output_obj
      type (field2dReal), intent(in) :: field
      real(kind=8), dimension(:,:), allocatable :: temp

      include 'netcdf.inc'

      integer :: nferr
      integer :: varID
      integer, dimension(2) :: start2, count2
      real :: var

      start2(1) = field % ioinfo % start(1)
      start2(2) = field % ioinfo % start(2)
      count2(1) = field % ioinfo % count(1)
      count2(2) = field % ioinfo % count(2)
      write(*,*)'=== inside io_output_field2dReal ==',trim(field % ioinfo % fieldName)
      if (trim(field % ioinfo % fieldName) == 'lat') then
         varID = output_obj % wrVarIDlat
      else if (trim(field % ioinfo % fieldName) == 'lon') then
         varID = output_obj % wrVarIDlon
      else if (trim(field % ioinfo % fieldName) == 'mapfac_u') then
         varID = output_obj % wrVarIDmapfu
      else if (trim(field % ioinfo % fieldName) == 'mapfac_v') then
         varID = output_obj % wrVarIDmapfv
      else if (trim(field % ioinfo % fieldName) == 'mapfac_m') then
         varID = output_obj % wrVarIDmapfm
      else if (trim(field % ioinfo % fieldName) == 'bin2d') then
         varID = output_obj % wrVarIDbin2d
      else
         varID = output_obj % wrVarIDvarnd( field % ioinfo % ID  )
         write(*,*)'field % ioinfo % ID ',field % ioinfo % ID
         write(*,*)'field % ioinfo % fieldName ',trim(field % ioinfo % fieldName)
      end if
  
      var = field % array(1,1)
      write(*,*)' fieldName ',trim(field % ioinfo % fieldName) 
      write(*,*)' varID ',varID
      write(*,*)' start2 ',start2 
      write(*,*)' count2 ',count2
      write(*,*)' var ',var
       
      nferr = nf_put_vara_double(output_obj % wr_ncid, varID, start2, count2, field%array)
      call check_err(nferr,field % ioinfo % fieldName)
      write(*,*)'nferr ', nferr
      write(*,*)'nf_strerror ', nf_strerror(nferr)
      write(*,*)'=== leaving io_output_field2dReal ==',trim(field % ioinfo % fieldName)

      nferr = nf_sync(output_obj % wr_ncid)

   end subroutine io_output_field2dReal
 
 
   subroutine io_output_field3dReal(output_obj, field)

      implicit none

      type (io_output_object), intent(in) :: output_obj
      type (field3dReal), intent(in) :: field
      real, allocatable     :: tmp(:,:,:)
      integer :: nferr
      integer :: varID
      integer, dimension(3) :: start3, count3

       include 'netcdf.inc'

      start3(1) = field % ioinfo % start(1)
      start3(2) = field % ioinfo % start(2)
      start3(3) = field % ioinfo % start(3)
      count3(1) = field % ioinfo % count(1)
      count3(2) = field % ioinfo % count(2)
      count3(3) = field % ioinfo % count(3)

      write(*,*)'=== inside io_output_field3dReal ==',trim(field % ioinfo % fieldName)
      if (trim(field % ioinfo % fieldName) == 'height') then
         varID = output_obj % wrVarIDhgt
      else
         varID = output_obj % wrVarIDvarnd( field % ioinfo % ID  )
      end if
      write(*,*)'varID : ',varID
      write(*,*)'start3 count3 : ',start3,count3
 
      nferr = nf_put_vara_double(output_obj % wr_ncid, varID, start3, count3, field % array)
      call check_err(nferr,field % ioinfo % fieldName)
      nferr = nf_sync(output_obj % wr_ncid)
     write(*,*)'nf_strerror ', nf_strerror(nferr)
     write(*,*)'=== leaving io_output_field3dReal ==',trim(field % ioinfo % fieldName)

   end subroutine io_output_field3dReal


   subroutine io_output_field0dReal_time(output_obj, field)

      implicit none

      type (io_output_object), intent(in) :: output_obj
      type (field0dReal), intent(in) :: field

      include 'netcdf.inc'

      integer :: nferr
      integer :: varID
      integer, dimension(1) :: start1, count1

      start1(1) = output_obj % time
      count1(1) = 1

    !  if (trim(field % ioinfo % fieldName) == 'xtime') then
    !     varID = output_obj % wrVarIDxtime
    !  end if

    !  nferr = nf_put_vara_double(output_obj % wr_ncid, varID, start1, count1, field % scalar)
 
    !  nferr = nf_sync(output_obj % wr_ncid)

   end subroutine io_output_field0dReal_time


   subroutine io_output_field1dReal_time(output_obj, field)

      implicit none

      type (io_output_object), intent(in) :: output_obj
      type (field1dReal), intent(in) :: field

      include 'netcdf.inc'

      integer :: nferr
      integer :: varID
      integer, dimension(2) :: start2, count2

      start2(1) = field % ioinfo % start(1)
      start2(2) = output_obj % time
      count2(1) = field % ioinfo % count(1)
      count2(2) = 1

      !varID = output_obj % wrVarIDvar2d( field % ioinfo % ID  )

      !nferr = nf_put_vara_double(output_obj % wr_ncid, varID, start2, count2, field % array)
 
      !nferr = nf_sync(output_obj % wr_ncid)

   end subroutine io_output_field1dReal_time


   subroutine io_output_field1dChar_time(output_obj, field)

      implicit none

      type (io_output_object), intent(in) :: output_obj
      !type (field1dChar), intent(inout) :: field
      character (len=10) :: field

      include 'netcdf.inc'

      integer :: nferr
      integer :: varID
      integer, dimension(2) :: start2, count2

      start2(1) = 1 ! field % ioinfo % start(1)
      start2(2) = output_obj % time
      count2(1) = 10 ! field % ioinfo % count(1)
      count2(2) = 1

      varID = output_obj % wrVarIDate

      nferr = nf_put_vara_text(output_obj % wr_ncid, varID, start2, count2, field)
      !call check_err(nferr,field % ioinfo % fieldName)

      nferr = nf_sync(output_obj % wr_ncid)

   end subroutine io_output_field1dChar_time

   subroutine io_output_field1dChar(output_obj, field)

      implicit none

      type (io_output_object), intent(in) :: output_obj
      type (field1dChar), intent(in) :: field

      include 'netcdf.inc'

      integer :: nferr
      integer :: varID
      integer, dimension(2) :: start2, count2

      start2(1) =  field % ioinfo % start(1)
      start2(2) =  1 
      count2(1) =  field % ioinfo % count(1)
      count2(2) =  20

      varID = output_obj % wrVarIDvarnd( field % ioinfo % ID  )

      nferr = nf_put_vara_text(output_obj % wr_ncid, varID, start2, count2, field)
      call check_err(nferr,field % ioinfo % fieldName)

      nferr = nf_sync(output_obj % wr_ncid)

   end subroutine io_output_field1dChar


   subroutine io_output_field2dReal_time(output_obj, field)

      implicit none

      type (io_output_object), intent(in) :: output_obj
      type (field2dReal), intent(in) :: field

      include 'netcdf.inc'

      integer :: nferr
      integer :: varID
      integer, dimension(3) :: start3, count3

      start3(1) = field % ioinfo % start(1)
      start3(2) = field % ioinfo % start(2)
      start3(3) = output_obj % time
      count3(1) = field % ioinfo % count(1)
      count3(2) = field % ioinfo % count(2)
      count3(3) = 1

      varID = output_obj % wrVarIDvarnd( field % ioinfo % ID  )

      write(*,*)'inside io_output_field2dReal_time field % ioinfo % fieldName',trim(field % ioinfo % fieldName)
      write(*,*)'inside io_output_field2dReal_time field % ioinfo % ID', field % ioinfo % ID
      write(*,*)'varID ',varID


      nferr = nf_put_vara_double(output_obj % wr_ncid, varID, start3, count3, field % array)
      call check_err(nferr,field % ioinfo % fieldName)
 
      nferr = nf_sync(output_obj % wr_ncid)

   end subroutine io_output_field2dReal_time


   subroutine io_output_field3dReal_time(output_obj, field)

      implicit none

      type (io_output_object), intent(in) :: output_obj
      type (field3dReal), intent(in) :: field
      !type (field3dReal), intent(in) :: field

      include 'netcdf.inc'

      integer :: nferr
      integer :: varID
      integer, dimension(4) :: start4, count4

      start4(1) = field % ioinfo % start(1)
      start4(2) = field % ioinfo % start(2)
      start4(3) = field % ioinfo % start(3)
      start4(4) = output_obj % time
      count4(1) = field % ioinfo % count(1)
      count4(2) = field % ioinfo % count(2)
      count4(3) = field % ioinfo % count(3)
      count4(4) = 1
 
      write(*,*)'========== inside io_output_field3dReal_time ============='

      write(*,*)'field % ioinfo % ID ',field % ioinfo % ID
      varID = output_obj % wrVarIDvarnd( field % ioinfo % ID  )      
      write(*,*)'varID ',varID 

      nferr = nf_put_vara_double(output_obj % wr_ncid, VarID, start4, count4, field % array)
      call check_err(nferr,field % ioinfo % fieldName)

      nferr = nf_sync(output_obj % wr_ncid)

      write(*,*)'nf_strerror ', nf_strerror(nferr)
      write(*,*)'field % array inside io_output_field3dReal_time'

      write(*,*)'========== outside io_output_field3dReal_time ============='

   end subroutine io_output_field3dReal_time


   subroutine io_output_field1dInteger(output_obj, field)

      implicit none

      type (io_output_object), intent(in) :: output_obj
      type (field1dInteger), intent(in) :: field

      include 'netcdf.inc'

      integer :: nferr
      integer :: varID
      integer, dimension(1) :: start1, count1

      start1(1) = field % ioinfo % start(1)
      count1(1) = field % ioinfo % count(1)

      if (trim(field % ioinfo % fieldName) == 'bin_pts') then
         varID = output_obj % wrVarIDbinpts
      else if (trim(field % ioinfo % fieldName) == 'bin2d_pts') then
         varID = output_obj % wrVarIDbin2dpts
      end if

      nferr = nf_put_vara_int(output_obj % wr_ncid, varID, start1, count1, field % array)
      call check_err(nferr,field % ioinfo % fieldName)

      nferr = nf_sync(output_obj % wr_ncid)

   end subroutine io_output_field1dInteger


   subroutine io_output_field2dInteger(output_obj, field)

      implicit none

      type (io_output_object), intent(in) :: output_obj
      type (field2dInteger), intent(in) :: field

      include 'netcdf.inc'

      integer :: nferr
      integer :: varID
      integer, dimension(2) :: start2, count2

      start2(1) = field % ioinfo % start(1)
      start2(2) = field % ioinfo % start(2)
      count2(1) = field % ioinfo % count(1)
      count2(2) = field % ioinfo % count(2)

      if (trim(field % ioinfo % fieldName) == 'bin2d') then
         varID = output_obj % wrVarIDbin2d
      else if (trim(field % ioinfo % fieldName) == 'covarID') then
         varID = output_obj % wrVarIDcovar
      end if

      nferr = nf_put_vara_int(output_obj % wr_ncid, varID, start2, count2, field % array)
      call check_err(nferr,field % ioinfo % fieldName)
      nferr = nf_sync(output_obj % wr_ncid)

   end subroutine io_output_field2dInteger


   subroutine io_output_field3dInteger(output_obj, field)

      implicit none

      type (io_output_object), intent(in) :: output_obj
      type (field3dInteger), intent(in) :: field
      real, allocatable     :: tmp(:,:,:)
      integer :: nferr
      integer :: varID
      integer, dimension(3) :: start3, count3

       include 'netcdf.inc'

      start3(1) = field % ioinfo % start(1)
      start3(2) = field % ioinfo % start(2)
      start3(3) = field % ioinfo % start(3)
      count3(1) = field % ioinfo % count(1)
      count3(2) = field % ioinfo % count(2)
      count3(3) = field % ioinfo % count(3)

     write(*,*)'=== inside io_output_field3dInteger ==',trim(field % ioinfo % fieldName)

      if (trim(field % ioinfo % fieldName) == 'bin') then
         varID = output_obj % wrVarIDbin
         write(*,*)'field ',field % array(1,1,:)
      else if (trim(field % ioinfo % fieldName) == 'ij_counter_rc') then
         varID = output_obj % wrVarIDijcrc
!      else
!         varID = output_obj % wrVarIDvar3d( field % ioinfo % ID  )
      end if

!     varID =  output_obj % wrVarIDvar3d0
     write(*,*)'varID : ',varID
     write(*,*)'start3 count3 : ',start3,count3

     nferr = nf_put_vara_int(output_obj % wr_ncid, varID, start3, count3, field % array)
     call check_err(nferr,field % ioinfo % fieldName)
     nferr = nf_sync(output_obj % wr_ncid)

!      write(*,*)'nferr ', nferr
     write(*,*)'nf_strerror ', nf_strerror(nferr)

     write(*,*)'=== leaving io_output_Integer3d ==',trim(field % ioinfo % fieldName)

   end subroutine io_output_field3dInteger


end module io_output

