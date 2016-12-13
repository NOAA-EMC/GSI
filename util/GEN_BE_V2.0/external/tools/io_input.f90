

module io_input

!---------------------------------------------------------------------- 
! Purpose: Define how to read objects from netcdf format input
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
   !use da_tools_serial, only : da_get_unit

   type io_input_object
      character (len=1024) :: filename
      integer :: rd_ncid

      integer :: time

      integer :: rdDimIDTime
      integer :: rdDimIDlat
      integer :: rdDimIDlon
      integer :: rdDimIDlatu
      integer :: rdDimIDlonu
      integer :: rdDimIDlatv
      integer :: rdDimIDlonv
      integer :: rdDimIDlev
      integer :: rdDimIDwav
      integer :: rdDimIDnvar
      integer :: rdDimIDate
      integer :: rdDimIDchar

      integer :: rdLocalTime
      integer :: rdLocallat
      integer :: rdLocallon
      integer :: rdLocallatu
      integer :: rdLocallonu
      integer :: rdLocallatv
      integer :: rdLocallonv
      integer :: rdLocallev
      integer :: rdLocalwav
      integer :: rdLocalchar
      integer :: rdLocaldate
      integer :: rdLocalnvar

      integer :: rdVarIDlat
      integer :: rdVarIDlon
      integer :: rdVarIDlev
      integer :: rdVarIDhgt
      integer :: rdVarIDmfm
      integer :: rdVarIDmfu
      integer :: rdVarIDmfv
      integer :: rdVarIDznu
      integer :: rdVarIDds
      integer :: rdVarIDchar

      integer , dimension(100) :: rdVarIDvarnd
      integer :: ivarnd
      integer :: rdVarIDvar0

      ! read bins
      integer :: rdVarIDbtyp
      integer :: rdVarIDbin
      integer :: rdVarIDbin2d
      integer :: rdVarIDbinpts
      integer :: rdVarIDbin2dpts
      integer :: rdDimIDbin
      integer :: rdDimIDbin2d
      integer :: rdLocalbin
      integer :: rdLocalbin2d

      ! namelist
      integer :: rdVarIDcovar
      integer, dimension(100) :: rdVarIDnaml

   end type io_input_object


   interface io_input_field
      module procedure io_input_field0dReal
      module procedure io_input_field1dReal
      module procedure io_input_field2dReal
      module procedure io_input_field3dReal
!      module procedure io_input_field1dChar
!      module procedure io_input_field0dInteger
      module procedure io_input_field1dInteger
      module procedure io_input_field2dInteger
      module procedure io_input_field3dInteger
   end interface io_input_field

   interface io_input_field_time
      module procedure io_input_field0dReal_time
      module procedure io_input_field1dReal_time
      module procedure io_input_field2dReal_time
      module procedure io_input_field3dReal_time
      module procedure io_input_field1dChar_time
   end interface io_input_field_time

   interface read_variable
      module procedure read_variable3dReal
      module procedure read_variable2dReal
      module procedure read_variable1dReal
      module procedure read_variable2dInteger
   end interface read_variable
 

   contains


!======================================================================================
! UTILS
!======================================================================================

  subroutine isfile_present(filename, file_here)

      implicit none

      character (len=*), intent(in) :: filename
      logical, intent(inout) :: file_here
      integer :: nferr, rd_ncid

      include 'netcdf.inc'

      nferr = nf_open(trim(filename), ior(NF_SHARE,NF_64BIT_OFFSET),  rd_ncid)
      if (nferr /= NF_NOERR) then

         inquire(file=trim(filename),exist=file_here)
         if ( file_here ) then
            write(0,*)'Open binary file ', trim(filename) 
         else
            write(0,*) ' '
            write(0,*) 'Error opening input file ''', trim(filename), ''''
            write(0,*) ' '
            file_here = .false.
         end if
      else
         file_here = .true.
         nferr = nf_close(rd_ncid)         
      end if

   end subroutine isfile_present 

!=====================================================================================
! READ A STATE
!=====================================================================================

   subroutine read_input_state(stream,state)
   
      implicit none
   
      type (state_type), intent(inout) , pointer :: state
      type (io_input_object) :: input_obj
      character (len=*), intent(in) :: stream
      integer :: nferr

      include 'netcdf.inc'

      call io_input_init(trim(stream),input_obj)
      input_obj % time = 1
  
      ! Dimensions  
      nferr = nf_inq_unlimdim(input_obj % rd_ncid, input_obj % rdDimIDTime)
      nferr = nf_inq_dimlen(input_obj % rd_ncid, input_obj % rdDimIDTime, input_obj % rdLocalTime)
      nferr = nf_inq_dimid(input_obj % rd_ncid, 'lat', input_obj % rdDimIDlat)
      nferr = nf_inq_dimlen(input_obj % rd_ncid, input_obj % rdDimIDlat, input_obj % rdLocallat)
      nferr = nf_inq_dimid(input_obj % rd_ncid, 'lon', input_obj % rdDimIDlon)
      nferr = nf_inq_dimlen(input_obj % rd_ncid, input_obj % rdDimIDlon, input_obj % rdLocallon)
      nferr = nf_inq_dimid(input_obj % rd_ncid, 'lev', input_obj % rdDimIDlev)
      nferr = nf_inq_dimlen(input_obj % rd_ncid, input_obj % rdDimIDlev, input_obj % rdLocallev)
      nferr = nf_inq_dimid(input_obj % rd_ncid, 'DateStrLen', input_obj % rdDimIDate)
      nferr = nf_inq_dimlen(input_obj % rd_ncid, input_obj % rdDimIDate, input_obj % rdLocaldate)
      nferr = nf_inq_varid(input_obj%rd_ncid, 'Times', input_obj % rdVarIDchar)

      call io_input_state(input_obj, state)
      call io_input_field_time(input_obj, state % date)
      call io_input_finalize(input_obj)

   end subroutine read_input_state

      
   subroutine io_input_init(filename,input_obj)

      implicit none

      type (io_input_object), intent(inout) :: input_obj
      character (len=*), intent(in) :: filename
      integer :: nferr

      include 'netcdf.inc'

      input_obj % ivarnd = 0
      input_obj % time = 0

      input_obj % filename = filename

      nferr = nf_open(trim(input_obj % filename), ior(NF_SHARE,NF_64BIT_OFFSET), input_obj % rd_ncid)
      if (nferr /= NF_NOERR) then
         write(0,*) ' '
         write(0,*) 'Error opening input file ''', trim(input_obj % filename), ''''
         write(0,*) ' '
         stop
      end if
      write(*,*)'open ',trim(filename)

   end subroutine io_input_init


   subroutine io_input_state(input_obj, state)
 
      implicit none

      type (io_input_object), intent(inout) :: input_obj
      type (state_type), intent(inout) :: state

      integer :: vv, ivarnd 
      integer :: nferr
      
      include 'netcdf.inc'

      ivarnd = input_obj % ivarnd
      
      do vv = 1,  state%nvar
         if (  state%num(vv)%IDdim == 1 ) then
             ivarnd = ivarnd + 1
             nferr = nf_inq_varid(input_obj%rd_ncid, state%num(vv)%field%field1d%ioinfo%fieldName, &
                                 input_obj%rdVarIDvarnd(ivarnd))
             state%num(vv)%field%field1d%ioinfo%ID = ivarnd
             if ( input_obj%time == 0 ) then
                call io_input_field(input_obj, state%num(vv)%field%field1d)
             else
                call io_input_field_time(input_obj, state%num(vv)%field%field1d)
             end if
             write(*,*)'Read ',trim(state%num(vv)%field%field1d%ioinfo%fieldName),ivarnd

         else if (  state%num(vv)%IDdim == 2 ) then
             ivarnd = ivarnd + 1
             nferr = nf_inq_varid(input_obj%rd_ncid, state%num(vv)%field%field2d%ioinfo%fieldName, & 
                                input_obj % rdVarIDvarnd(ivarnd))
             state%num(vv)%field%field2d%ioinfo%ID = ivarnd
             if ( input_obj%time == 0 ) then
                call io_input_field(input_obj, state%num(vv)%field%field2d)
             else
                call io_input_field_time(input_obj, state%num(vv)%field%field2d)
             end if

         else if (  state%num(vv)%IDdim == 3 ) then
             ivarnd = ivarnd + 1
             nferr = nf_inq_varid(input_obj%rd_ncid, state%num(vv)%field%field3d%ioinfo%fieldName, &
                                input_obj % rdVarIDvarnd(ivarnd))
             state%num(vv)%field%field3d%ioinfo%ID = ivarnd
             if ( input_obj%time == 0 ) then
                call io_input_field(input_obj, state%num(vv)%field%field3d)
             else
                call io_input_field_time(input_obj, state%num(vv)%field%field3d)
             end if
         
         end if
      end do

      input_obj % ivarnd = ivarnd

   end subroutine io_input_state


   subroutine io_input_finalize(input_obj)

      implicit none

      type (io_input_object), intent(inout) :: input_obj

      include 'netcdf.inc'

      integer :: nferr

      nferr = nf_close(input_obj % rd_ncid)

   end subroutine io_input_finalize

!================================================================================================
! READ MATRIX STATE
!================================================================================================

  subroutine read_matrix_state(filename, matrix)
  
    implicit none

    type (io_input_object) :: input_obj
    type (state_matrix_type), intent(inout) :: matrix
    character (len=*), intent(in):: filename
    integer :: vv, nferr, ii, jj, ivarnd

    include 'netcdf.inc'

    call io_input_init(filename,input_obj)

    ! Dimensions inquire
    nferr = nf_inq_dimid(input_obj % rd_ncid, 'lev', input_obj % rdDimIDlev)
    nferr = nf_inq_dimlen(input_obj % rd_ncid, input_obj % rdDimIDlev, input_obj % rdLocallev)
    nferr = nf_inq_dimid(input_obj % rd_ncid, 'num_bins', input_obj % rdDimIDbin)
    nferr = nf_inq_dimlen(input_obj % rd_ncid, input_obj % rdDimIDbin, input_obj % rdLocalbin)
    nferr = nf_inq_dimid(input_obj % rd_ncid, 'num_bins2d', input_obj % rdDimIDbin2d)
    nferr = nf_inq_dimlen(input_obj % rd_ncid, input_obj % rdDimIDbin2d, input_obj % rdLocalbin2d)

    ! matrix inquire and read
    call io_input_matrix_state(input_obj, matrix)

    nferr = nf_close(input_obj % rd_ncid)

  end subroutine read_matrix_state


  subroutine io_input_matrix_state(input_obj, matrix)

    implicit none

    type (io_input_object), intent(inout) :: input_obj
    type (state_matrix_type), intent(inout) :: matrix
    integer :: vv, nferr, ii, jj, ivarnd

    include 'netcdf.inc'

    ivarnd = input_obj%ivarnd 
    
    do jj = 1, matrix%nvar
       do ii = 1, matrix%nvar
          if ( matrix%num2d(ii,jj)%IDdim == 1 ) then
                ivarnd = ivarnd + 1
                nferr = nf_inq_varid(input_obj%rd_ncid, trim(matrix%num2d(ii,jj)%field%field1d%ioinfo%fieldName), &
                                  input_obj % rdVarIDvarnd(ivarnd))
                matrix%num2d(ii,jj)%field%field1d%ioinfo%ID = ivarnd
                write(*,*)'Read ',trim(matrix%num2d(ii,jj)%field%field1d%ioinfo%fieldName),ivarnd
                call io_input_field(input_obj,matrix%num2d(ii,jj)%field%field1d)
             else if ( matrix%num2d(ii,jj)%IDdim == 2 ) then
                ivarnd = ivarnd + 1
                nferr = nf_inq_varid(input_obj%rd_ncid, trim(matrix%num2d(ii,jj)%field%field2d%ioinfo%fieldName), &
                                  input_obj % rdVarIDvarnd(ivarnd))
                matrix%num2d(ii,jj)%field%field2d%ioinfo%ID = ivarnd
                write(*,*)'Read ',trim(matrix%num2d(ii,jj)%field%field2d%ioinfo%fieldName),ivarnd
                call io_input_field(input_obj,matrix%num2d(ii,jj)%field%field2d)
             else if ( matrix%num2d(ii,jj)%IDdim == 3 ) then
                ivarnd = ivarnd + 1
                nferr = nf_inq_varid(input_obj%rd_ncid, trim(matrix%num2d(ii,jj)%field%field3d%ioinfo%fieldName), &
                                  input_obj % rdVarIDvarnd(ivarnd))
                write(*,*)'Read ',trim(matrix%num2d(ii,jj)%field%field3d%ioinfo%fieldName),ivarnd
                matrix%num2d(ii,jj)%field%field3d%ioinfo%ID = ivarnd
                call io_input_field(input_obj,matrix%num2d(ii,jj)%field%field3d)
             end if
          end do
     end do

     input_obj%ivarnd = ivarnd

  end subroutine io_input_matrix_state


!================================================================================================
! READ A VARIABLE IN A FILE
!================================================================================================
   
   subroutine read_variable3dReal(filename, field, itime)

      implicit none 
      type (field3DReal), intent(inout) :: field
      character (len=*), intent(in) :: filename
      integer, intent(in) :: itime
      integer, dimension(3) :: start3, count3
      integer, dimension(4) :: start4, count4
      type (io_input_object) :: input_obj
      integer :: nferr, varID

      include 'netcdf.inc'
  
      call io_input_init(filename,input_obj) 
     
      nferr = nf_inq_varid(input_obj%rd_ncid, trim(field % ioinfo % fieldName), input_obj % rdVarIDvar0)

      start4(1) = field % ioinfo % start(1)
      start4(2) = field % ioinfo % start(2)
      start4(3) = field % ioinfo % start(3)
      start4(4) = 1
      count4(1) = field % ioinfo % count(1)
      count4(2) = field % ioinfo % count(2)
      count4(3) = field % ioinfo % count(3)
      count4(4) = 1
      start3(1:3)=start4(1:3)
      count3(1:3)=count4(1:3)
      !write(*,*)'count3 ',count3

      varID = input_obj % rdVarIDvar0
      if ( itime == 0 ) then
         nferr = nf_get_vara_double(input_obj % rd_ncid, varID, start3, count3, field%array(:,:,:) )
      else
         nferr = nf_get_vara_double(input_obj % rd_ncid, varID, start4, count4, field % array)
      end if
      write(*,*)' nferr = nf_inq_varid ',nferr
      write(*,*)'nf_strerror ', nf_strerror(nferr)
      nferr = nf_close(input_obj % rd_ncid)

   end subroutine read_variable3dReal

   
   subroutine read_variable2dReal(filename, field, itime)

      implicit none
      type (field2DReal), intent(inout) :: field
      character (len=*), intent(in) :: filename
      integer, intent(in) :: itime
      integer, dimension(3) :: start3, count3
      integer, dimension(2) :: start2, count2
      type (io_input_object) :: input_obj
      integer :: nferr, varID

      include 'netcdf.inc'

      call io_input_init(filename,input_obj)

      nferr = nf_inq_varid(input_obj%rd_ncid, trim(field % ioinfo % fieldName), input_obj % rdVarIDvar0)

      start3(1) = field % ioinfo % start(1)
      start3(2) = field % ioinfo % start(2)
      start3(3) = 1
      count3(1) = field % ioinfo % count(1)
      count3(2) = field % ioinfo % count(2)
      count3(3) = 1
      start2(1:2)=start3(1:2)
      count2(1:2)=count3(1:2)

      varID = input_obj % rdVarIDvar0
      if ( itime == 0 ) then
         nferr = nf_get_vara_double(input_obj % rd_ncid, varID, start2, count2, field % array )
      else
         nferr = nf_get_vara_double(input_obj % rd_ncid, varID, start3, count3, field % array)
      end if
      write(*,*)' nferr = nf_inq_varid ',nferr
      write(*,*)'nf_strerror ', nf_strerror(nferr)
      nferr = nf_close(input_obj % rd_ncid)

   end subroutine read_variable2dReal
   

   subroutine read_variable1dReal(filename, field, itime)

      implicit none
      type (field1DReal), intent(inout) :: field
      character (len=*), intent(in) :: filename
      integer, intent(in) :: itime
      integer, dimension(1) :: start1, count1
      integer, dimension(2) :: start2, count2
      type (io_input_object) :: input_obj
      integer :: nferr, varID

      include 'netcdf.inc'

      call io_input_init(filename,input_obj)

      nferr = nf_inq_varid(input_obj%rd_ncid, trim(field % ioinfo % fieldName), input_obj % rdVarIDvar0)

      start2(1) = field % ioinfo % start(1)
      start2(2) = 1
      count2(1) = field % ioinfo % count(1)
      count2(2) = 1
      start1(1:1)=start2(1:1)
      count1(1:1)=count2(1:1)

      varID = input_obj % rdVarIDvar0
      if ( itime == 0 ) then
         nferr = nf_get_vara_double(input_obj % rd_ncid, varID, start1, count1, field % array )
      else
         nferr = nf_get_vara_double(input_obj % rd_ncid, varID, start2, count2, field % array)
      end if
      write(*,*)' nferr = nf_inq_varid ',nferr
      write(*,*)'nf_strerror ', nf_strerror(nferr)
      nferr = nf_close(input_obj % rd_ncid)

   end subroutine read_variable1dReal


   subroutine read_variable2dInteger(filename, field, itime)

      implicit none
      type (field2DInteger), intent(inout) :: field
      character (len=*), intent(in) :: filename
      integer, intent(in) :: itime
      integer, dimension(3) :: start3, count3
      integer, dimension(2) :: start2, count2
      type (io_input_object) :: input_obj
      integer :: nferr, varID

      include 'netcdf.inc'

      call io_input_init(filename,input_obj)

      nferr = nf_inq_varid(input_obj%rd_ncid, trim(field % ioinfo % fieldName), input_obj % rdVarIDvar0)

      start3(1) = field % ioinfo % start(1)
      start3(2) = field % ioinfo % start(2)
      start3(3) = 1
      count3(1) = field % ioinfo % count(1)
      count3(2) = field % ioinfo % count(2)
      count3(3) = 1
      start2(1:2)=start3(1:2)
      count2(1:2)=count3(1:2)

      varID = input_obj % rdVarIDvar0
      if ( itime == 0 ) then
         nferr = nf_get_vara_int(input_obj % rd_ncid, varID, start2, count2, field % array )
      else
         nferr = nf_get_vara_int(input_obj % rd_ncid, varID, start3, count3, field % array)
      end if
      write(*,*)' nferr = nf_inq_varid ',nferr
      write(*,*)'nf_strerror ', nf_strerror(nferr)
      nferr = nf_close(input_obj % rd_ncid)

   end subroutine read_variable2dInteger

  
   subroutine read_state_from_filist(state, filename, itime)

   implicit none
   type (state_type), intent(inout) :: state
   character (len=*), dimension(:) ,intent(in) :: filename
   integer, intent(in) :: itime
   integer :: vv

   do vv=1, state%nvar

        if (state % num(vv) % IDdim == 1 ) then
            call read_variable1dReal(trim(filename(vv)), state%num(vv)%field%field1d, itime)
        else if (state % num(vv) % IDdim == 2 ) then
            call read_variable2dReal(trim(filename(vv)), state%num(vv)%field%field2d, itime)
        else if (state % num(vv) % IDdim == 3 ) then
            call read_variable3dReal(trim(filename(vv)), state%num(vv)%field%field3d, itime)
        else
            write(*,*)'filename is missing : ',filename(vv), vv
        end if
        write(*,*) 'reading ',trim(filename(vv))

   end do

   end subroutine read_state_from_filist


   subroutine read_state_from_variable_file(state, ivar, date, ce)

      implicit none
      type (state_type), intent(inout) :: state
      character (len=10), intent(in) :: date
      character (len=3),  intent(in) :: ce
      integer, intent(in)   :: ivar
      character (len=1024) :: filename
      character (len=1024) :: variable
      integer :: vv, itime

      itime = 1
      filename = "not reading variable none allocated"
   
      do vv=ivar, state%nvar
            if (state % num(vv) % IDdim == 1 ) then
               variable = trim(state % num(vv) % field % field1d % ioinfo % fieldName)
               filename = trim(variable)//'/'//trim(variable)//'.'//date(1:10)
               filename = trim(filename)//'.e'//ce
               call read_variable1dReal(filename, state%num(vv)%field%field1d, itime)
            else if (state % num(vv) % IDdim == 2 ) then
               variable = trim(state % num(vv) % field % field2d % ioinfo % fieldName)
               filename = trim(variable)//'/'//trim(variable)//'.'//date(1:10)
               filename = trim(filename)//'.e'//ce
               call read_variable2dReal(filename, state%num(vv)%field%field2d, itime)
            else if (state % num(vv) % IDdim == 3 ) then
               variable = trim(state % num(vv) % field % field3d % ioinfo % fieldName)
               filename = trim(variable)//'/'//trim(variable)//'.'//date(1:10)
               filename = trim(filename)//'.e'//ce
               call read_variable3dReal(filename, state%num(vv)%field%field3d, itime)
            else
               write(*,*)'filename : ',filename,ivar
               stop 
            end if
            write(*,*) 'reading ',trim(filename)
      end do

   end subroutine read_state_from_variable_file
      

   subroutine read_state_unbalanced_from_variable_file(state, list_cv, ivar, date, ce)

      implicit none
      type (state_type), intent(inout) :: state
      character (len=*), dimension(:), intent(in)   :: list_cv
      character (len=10), intent(in)   :: date
      character (len=3),  intent(in)   :: ce
      integer, intent(in)   :: ivar
      character (len=1024)  :: filename
      character (len=1024)  :: variable
      integer :: vv, itime
      real(kind=8) :: init0

      itime = 1
      init0 = 0.0
      filename = "not reading variable none allocated"
      call initial_state(state,init0 )

 
      if (state % num(ivar) % IDdim == 2 ) then
          ! read the unbalanced variable(ivar) to compute the covariance
          variable = trim(state % num(ivar) % field % field2d % ioinfo % fieldName)//'_u'
          state%num(ivar)%field%field2d%ioinfo%fieldName = trim(variable)
          filename = trim(variable)//'/'//trim(variable)//'.'//date(1:10)
          filename = trim(filename)//'.e'//ce          
          call read_variable2dReal(filename, state%num(ivar)%field%field2d, itime)
          ! rename with original name to remove the unbalance part later
          state%num(ivar)%field%field2d%ioinfo%fieldName = list_cv(ivar)
      else if (state % num(ivar) % IDdim == 3 ) then
          ! read the unbalanced variable(ivar) to compute the covariance
          variable = trim(state % num(ivar) % field % field3d % ioinfo % fieldName)//'_u'
          state%num(ivar)%field%field3d%ioinfo%fieldName = trim(variable)
          filename = trim(variable)//'/'//trim(variable)//'.'//date(1:10)
          filename = trim(filename)//'.e'//ce
          call read_variable3dReal(filename, state%num(ivar)%field%field3d, itime)
          ! rename with original name to remove the unbalance part later
          state%num(ivar)%field%field3d%ioinfo%fieldName = list_cv(ivar)
      else
          write(*,*)'filename : ',filename,ivar
          stop
      end if
      write(*,*)'reading unbalance variable ',filename,ivar

      !end do
       
!      call read_state_from_variable_file(state, ivar+1, date, ce)
         
   end subroutine read_state_unbalanced_from_variable_file



!======================================================================================
! READ DIMENSIONS, MESH GRID, BIN PARAMETER
!======================================================================================

   subroutine get_vardim(filin, Dim1, Dim2, Dim3)

      implicit none

      character (len=*), intent(in)  :: filin
      integer, intent(inout) ::  Dim1, Dim2, Dim3
      type (io_input_object) :: input_obj
      integer :: nferr

      include 'netcdf.inc'

      call io_input_init(filin, input_obj)

      nferr = nf_inq_dimid(input_obj % rd_ncid, 'lat', input_obj % rdDimIDlat)
      nferr = nf_inq_dimlen(input_obj % rd_ncid, input_obj % rdDimIDlat, input_obj % rdLocallat)
      nferr = nf_inq_dimid(input_obj % rd_ncid, 'lon', input_obj % rdDimIDlon)
      nferr = nf_inq_dimlen(input_obj % rd_ncid, input_obj % rdDimIDlon, input_obj % rdLocallon)
      nferr = nf_inq_dimid(input_obj % rd_ncid, 'lev', input_obj % rdDimIDlev)
      nferr = nf_inq_dimlen(input_obj % rd_ncid, input_obj % rdDimIDlev, input_obj % rdLocallev)
      Dim1 = input_obj % rdLocallon
      Dim2 = input_obj % rdLocallat
      Dim3 = input_obj % rdLocallev
      write(*,*)'get_vardim Dim1, Dim2, Dim3 : ',Dim1, Dim2, Dim3
      nferr = nf_close(input_obj % rd_ncid)

   end subroutine get_vardim


   subroutine init_mesh(filin, mesh, model)
   
       implicit none

       character (len=*), intent(in)  :: filin
       type (mesh_type), intent(inout), pointer :: mesh 
       character (len=*),intent(in), optional  :: model      
       type (io_input_object) :: input_obj
       integer :: nferr, Dim1, Dim2, Dim3

       include 'netcdf.inc'
 
       call io_input_init(filin, input_obj)

       call get_vardim(filin, Dim1, Dim2, Dim3)
       call allocate_mesh(mesh, Dim1, Dim2, Dim3)       
         
       nferr = nf_open(trim(input_obj % filename), ior(NF_SHARE,NF_64BIT_OFFSET), input_obj % rd_ncid)

       nferr = nf_inq_varid(input_obj % rd_ncid, 'lat', input_obj % rdVarIDlat)
       nferr = nf_inq_varid(input_obj % rd_ncid, 'lon', input_obj % rdVarIDlon)
       nferr = nf_inq_varid(input_obj % rd_ncid, 'height', input_obj % rdVarIDhgt)
       nferr = nf_inq_varid(input_obj % rd_ncid, 'mapfac_m', input_obj % rdVarIDmfm)
       nferr = nf_inq_varid(input_obj % rd_ncid, 'ds', input_obj % rdVarIDds)
      
       call io_input_field(input_obj, mesh % lat) 
       call io_input_field(input_obj, mesh % lon) 
       call io_input_field(input_obj, mesh % hgt) 
       call io_input_field(input_obj, mesh % ds) 
       call io_input_field(input_obj, mesh % mapfac_m)

       if (present(model)) then

          write(*,*)'Allocate for ',trim(model)

          nferr = nf_inq_dimid(input_obj % rd_ncid, 'lat_u', input_obj % rdDimIDlatu)
          nferr = nf_inq_dimlen(input_obj % rd_ncid, input_obj % rdDimIDlatu, input_obj % rdLocallatu)
          nferr = nf_inq_dimid(input_obj % rd_ncid, 'lon_u', input_obj % rdDimIDlonu)
          nferr = nf_inq_dimlen(input_obj % rd_ncid, input_obj % rdDimIDlonu, input_obj % rdLocallonu)
          nferr = nf_inq_dimid(input_obj % rd_ncid, 'lat_v', input_obj % rdDimIDlatv)
          nferr = nf_inq_dimlen(input_obj % rd_ncid, input_obj % rdDimIDlatv, input_obj % rdLocallatv)
          nferr = nf_inq_dimid(input_obj % rd_ncid, 'lon_v', input_obj % rdDimIDlonv)
          nferr = nf_inq_dimlen(input_obj % rd_ncid, input_obj % rdDimIDlonv, input_obj % rdLocallonv)
          mesh%Dim1u = input_obj % rdLocallonu
          mesh%Dim2u = input_obj % rdLocallatu
          mesh%Dim1v = input_obj % rdLocallonv
          mesh%Dim2v = input_obj % rdLocallatv

          nferr = nf_inq_varid(input_obj % rd_ncid, 'mapfac_u', input_obj % rdVarIDmfu)
          if ( nferr .eq. 0) then 
             call allocate_field(mesh % mapfac_u,'mapfac_u',input_obj % rdLocallonu, input_obj % rdLocallatu)
             call io_input_field(input_obj, mesh % mapfac_u)
          end if
          nferr = nf_inq_varid(input_obj % rd_ncid, 'mapfac_v', input_obj % rdVarIDmfv)
          if ( nferr .eq. 0) then
             call allocate_field(mesh % mapfac_v,'mapfac_v',input_obj % rdLocallonv, input_obj % rdLocallatv)
             call io_input_field(input_obj, mesh % mapfac_v)
             write(*,*)'input_obj % rdVarIDmfv ',input_obj % rdVarIDmfv
          end if
          nferr = nf_inq_varid(input_obj % rd_ncid, 'znu', input_obj % rdVarIDznu)
          if ( nferr .eq. 0) then
             call allocate_field(mesh % znu,'znu',mesh%Dim3)
             call io_input_field(input_obj, mesh % znu)
          end if

       end if

       nferr = nf_close(input_obj % rd_ncid)
   
   end subroutine init_mesh


   subroutine init_bins(filin, bins)

       implicit none

       character (len=*), intent(in)  :: filin
       type (bins_type), intent(inout), pointer :: bins
       type (io_input_object) :: input_obj
       integer :: nferr,vv
       type (state_multilevel_type0D), pointer :: bin_type2_param
       integer :: Dim1, Dim2, Dim3
       integer, dimension(1) :: start1, count1
       integer :: btyp0d, VarID       

       include 'netcdf.inc'

       call io_input_init(filin,input_obj)

       nferr = nf_inq_dimid(input_obj % rd_ncid, 'num_bins2d', input_obj % rdDimIDbin2d)
       nferr = nf_inq_dimlen(input_obj % rd_ncid, input_obj % rdDimIDbin2d, input_obj % rdLocalbin2d)
       nferr = nf_inq_dimid(input_obj % rd_ncid, 'num_bins', input_obj % rdDimIDbin)
       nferr = nf_inq_dimlen(input_obj % rd_ncid, input_obj % rdDimIDbin, input_obj % rdLocalbin)

       ! alloc bins
       call get_vardim(filin, Dim1, Dim2, Dim3)
       call allocate_bins(bins, Dim1, Dim2, Dim3, input_obj % rdLocalbin, input_obj % rdLocalbin2d)
       write(*,*)'Dim1, Dim2, Dim3, input_obj % rdLocalbin, input_obj % rdLocalbin2d',Dim1, Dim2, Dim3, input_obj % rdLocalbin, input_obj % rdLocalbin2d

       call io_input_bins(bins, input_obj)
      

       nferr = nf_close(input_obj % rd_ncid)

   end subroutine init_bins


   subroutine update_dynamic_mask(bins, mesh, date, ce, dyn_mask)

      implicit none
    
      character*3 ,    intent(in)    :: ce                         ! Member index -> character.
      character*10,    intent(in)    :: date
      type (bins_type), intent(inout), pointer :: bins
      type (mesh_type), intent(in), pointer :: mesh
      logical, intent(inout) :: dyn_mask
      character(len=1024) :: filename
      integer :: k
      type (io_input_object) :: input_obj
      integer :: nferr

      include 'netcdf.inc'

      ! read dynamical mask file netcdf format
      filename = 'mask'
      filename = trim(filename)//'/'//trim(filename)//'.'//date(1:10)//'.e'//ce

      if ( ( bins%bin_type .eq. 7 ).or.( bins%bin_type .eq. 8 )) then

         write(*,*)'open dyn update ',trim(filename)
         dyn_mask = .true.
         call io_input_init(filename,input_obj)
         nferr = nf_inq_varid(input_obj%rd_ncid, trim(bins % bin2d % ioinfo % fieldName), input_obj % rdVarIDbin2d)
         call io_input_field(input_obj, bins%bin2d)
         bins%bin2d%array =  bins%bin2d%array + 1

         do k= 1, mesh%Dim3
            if ( bins%bin_type .eq. 7 ) then
               bins%bin%array(:,:,k) =  bins%bin2d%array(:,:) + 4*(k-1)
            else if ( bins%bin_type .eq. 8 ) then
               bins%bin%array(:,:,k) =  bins%bin2d%array(:,:) + 6*(k-1)
            end if
         end do
         nferr = nf_close(input_obj % rd_ncid)
         write(*,*)'nf_strerror ', nf_strerror(nferr)

      end if

      if ( bins%bin_type .eq. 5 ) then
         bins%do_fil = .true. 
      end if

   end subroutine update_dynamic_mask


   subroutine io_input_bins(bins, input_obj)

      implicit none

      type (bins_type), intent(inout), pointer :: bins
      type (io_input_object), intent(inout) :: input_obj
      integer :: nferr,vv
      type (state_multilevel_type0D), pointer :: bin_type2_param
      integer :: Dim1, Dim2, Dim3
      integer, dimension(1) :: start1, count1
      integer :: btyp0d, VarID, ivarnd

       include 'netcdf.inc'

       ivarnd = input_obj % ivarnd

       do vv = 1,  bins%bin_type2_param%nvar
          ivarnd = ivarnd + 1
          nferr = nf_inq_varid(input_obj % rd_ncid, bins%bin_type2_param%num(vv)%field%ioinfo%fieldName, input_obj % rdVarIDvarnd(ivarnd))
          bins%bin_type2_param%num(vv)%field%ioinfo%ID = vv
          write(*,*)'nferr ',nferr
       end do
       nferr = nf_inq_varid(input_obj % rd_ncid,'bin_type', input_obj % rdVarIDbtyp)

       ! read variables
       nferr = nf_inq_varid(input_obj % rd_ncid, 'bin2d', input_obj % rdVarIDbin2d)
       nferr = nf_inq_varid(input_obj % rd_ncid, 'bin', input_obj % rdVarIDbin)
       nferr = nf_inq_varid(input_obj % rd_ncid, 'bin2d_pts', input_obj % rdVarIDbin2dpts)
       nferr = nf_inq_varid(input_obj % rd_ncid, 'bin_pts', input_obj % rdVarIDbinpts)
       call io_input_field(input_obj, bins%bin2d)
       call io_input_field(input_obj, bins%bin)
       call io_input_field(input_obj, bins%bin2d_pts)
       call io_input_field(input_obj, bins%bin_pts)

       do vv = 1,  bins%bin_type2_param%nvar
          call io_input_field(input_obj, bins%bin_type2_param%num(vv)%field)
          write(*,*)'bin_type2_param : ',trim(bins%bin_type2_param%num(vv)%field%ioinfo%fieldName),bins%bin_type2_param%num(vv)%field%scalar
       end do
       ! read bin_type
       start1(1) = 1
       count1(1) = 1
       varID = input_obj % rdVarIDbtyp
       nferr = nf_get_vara_int(input_obj % rd_ncid, varID, start1, count1, btyp0d)
       bins%bin_type=btyp0d

       input_obj % ivarnd = ivarnd


   end subroutine io_input_bins

  
   subroutine io_input_get_dimension(input_obj, dimname, dimsize)

      implicit none

      type (io_input_object), intent(in) :: input_obj
      character (len=*), intent(in) :: dimname
      integer, intent(out) :: dimsize

      if (trim(dimname) == 'lat') then
         dimsize = input_obj % rdLocallat
      else if (trim(dimname) == 'lon') then
         dimsize = input_obj % rdLocallon
      else if (trim(dimname) == 'lev') then
         dimsize = input_obj % rdLocallev
      else if (trim(dimname) == 'Time') then
         dimsize = input_obj % rdLocalTime
      end if

   end subroutine io_input_get_dimension

!======================================================================================
! Read eigen variable
!======================================================================================

    subroutine io_input_eigenvar(eigen, input_obj)
      
      type (io_input_object), intent(inout) :: input_obj
      type (eigen_type), intent(inout) :: eigen
      integer :: nferr, ivarnd

      include 'netcdf.inc'

      ivarnd = input_obj % ivarnd 

      if (eigen%vec%IDdim .eq. 3) then
          ivarnd = ivarnd + 1
          nferr = nf_inq_varid(input_obj%rd_ncid, trim(eigen%vec%field%field3d%ioinfo%fieldName), &
             input_obj % rdVarIDvarnd(ivarnd))
          eigen%vec%field%field3d%ioinfo%ID = ivarnd
          call io_input_field(input_obj, eigen%vec%field%field3d)
          ivarnd = ivarnd + 1
          nferr = nf_inq_varid(input_obj%rd_ncid, trim(eigen%val%field%field2d%ioinfo%fieldName), &
             input_obj % rdVarIDvarnd(ivarnd))
          eigen%val%field%field2d%ioinfo%ID = ivarnd
          call io_input_field(input_obj, eigen%val%field%field2d)

      else if (eigen%vec%IDdim .eq. 2) then
         write(*,*)'ivarnd : ',ivarnd
         ivarnd = ivarnd + 1
         nferr = nf_inq_varid(input_obj%rd_ncid, trim(eigen%vec%field%field2d%ioinfo%fieldName), &
             input_obj % rdVarIDvarnd(ivarnd))
         eigen%vec%field%field2d%ioinfo%ID = ivarnd
         call io_input_field(input_obj, eigen%vec%field%field2d)
         ivarnd = ivarnd + 1
         nferr = nf_inq_varid(input_obj%rd_ncid, trim(eigen%val%field%field1d%ioinfo%fieldName), &
             input_obj % rdVarIDvarnd(ivarnd))
         eigen%val%field%field1d%ioinfo%ID = ivarnd
         call io_input_field(input_obj, eigen%val%field%field1d)
      
       else if (eigen%vec%IDdim .eq. 1) then
         ivarnd = ivarnd + 1
         nferr = nf_inq_varid(input_obj%rd_ncid, trim(eigen%vec%field%field1d%ioinfo%fieldName), &
             input_obj % rdVarIDvarnd(ivarnd))
         eigen%vec%field%field1d%ioinfo%ID = ivarnd
         call io_input_field(input_obj, eigen%vec%field%field1d)
         ivarnd = ivarnd + 1
         nferr = nf_inq_varid(input_obj%rd_ncid, trim(eigen%val%field%field1d%ioinfo%fieldName), &
             input_obj % rdVarIDvarnd(ivarnd))
         eigen%val%field%field1d%ioinfo%ID = ivarnd
         call io_input_field(input_obj, eigen%val%field%field1d)
   
       else if (eigen%vec%IDdim .eq. 0) then
         ivarnd = ivarnd + 1
         nferr = nf_inq_varid(input_obj%rd_ncid, trim(eigen%vec%field%field0d%ioinfo%fieldName), &
             input_obj % rdVarIDvarnd(ivarnd))
         eigen%vec%field%field0d%ioinfo%ID = ivarnd
         call io_input_field(input_obj, eigen%vec%field%field0d)
         ivarnd = ivarnd + 1
         nferr = nf_inq_varid(input_obj%rd_ncid, trim(eigen%val%field%field0d%ioinfo%fieldName), &
             input_obj % rdVarIDvarnd(ivarnd))
         eigen%val%field%field0d%ioinfo%ID = ivarnd
         call io_input_field(input_obj, eigen%val%field%field0d)

      end if
 
      input_obj % ivarnd = ivarnd

    end subroutine io_input_eigenvar


    subroutine init_eigenvar(filin, eigen, use_global_eofs, varname, IvarDim)

      implicit none

      type (io_input_object) :: input_obj
      type (eigen_type), intent(inout), pointer :: eigen 
      character (len=*), intent(in) :: filin
      character (len=*), intent(in) :: varname
      logical, intent(in) :: use_global_eofs
      integer, intent(in) :: IvarDim
      integer :: nferr
  
      include 'netcdf.inc'

      call io_input_init(filin,input_obj)

      nferr = nf_inq_dimid(input_obj % rd_ncid, 'lev', input_obj % rdDimIDlev)
      nferr = nf_inq_dimlen(input_obj % rd_ncid, input_obj % rdDimIDlev, input_obj % rdLocallev)
      nferr = nf_inq_dimid(input_obj % rd_ncid, 'num_bins2d', input_obj % rdDimIDbin2d)
      nferr = nf_inq_dimlen(input_obj % rd_ncid, input_obj % rdDimIDbin2d, input_obj % rdLocalbin2d)

      call allocate_eigenvar(eigen, use_global_eofs, varname, IvarDim,  input_obj % rdLocallev, input_obj % rdLocalbin2d)
      
      call io_input_eigenvar(eigen, input_obj)

      nferr = nf_close(input_obj % rd_ncid)

    end subroutine init_eigenvar

!======================================================================================
! Read lenscale variable
!======================================================================================
   

   subroutine init_lenscale(filin, varname, lenscale, uh_method)

      implicit none

      type (io_input_object) :: input_obj
      type (Fieldnd_type), intent(inout), pointer :: lenscale 
      character (len=*), intent(in) :: filin
      character (len=*), intent(in) :: varname, uh_method
      character (len=1024) :: varname0
      integer :: nferr

      include 'netcdf.inc'

      call io_input_init(filin,input_obj)

      if (trim(uh_method) == 'power') then
         !nferr = nf_inq_dimid(input_obj % rd_ncid, 'wave', input_obj % rdDimIDwav)
         !nferr = nf_inq_dimlen(input_obj % rd_ncid, input_obj % rdDimIDwav, input_obj % rdLocalwav)
         varname0 = 'power_spectra_'//trim(varname)
         !call allocate_field(lenscale, trim(varname0), input_obj % rdLocalwav)
      else
         !nferr = nf_inq_dimid(input_obj % rd_ncid, 'lev', input_obj % rdDimIDlev)
         !nferr = nf_inq_dimlen(input_obj % rd_ncid, input_obj % rdDimIDlev, input_obj % rdLocallev)
         varname0 = 'lenscale_'//trim(varname)
         !call allocate_field(lenscale, trim(varname0), input_obj % rdLocallev)
      end if  

      if ( lenscale%IDdim .eq. 1 ) then
         input_obj % ivarnd = input_obj % ivarnd + 1 
         nferr = nf_inq_varid(input_obj%rd_ncid, trim(lenscale%field%field1d%ioinfo%fieldName), &
                  input_obj % rdVarIDvarnd(input_obj%ivarnd))
         lenscale%field%field1d%ioinfo%ID = input_obj % ivarnd
         call io_input_field(input_obj, lenscale%field%field1d)
      else if ( lenscale%IDdim .eq. 0 ) then
         input_obj % ivarnd = input_obj % ivarnd + 1 
         nferr = nf_inq_varid(input_obj%rd_ncid, trim(lenscale%field%field0d%ioinfo%fieldName), &
                  input_obj % rdVarIDvarnd(input_obj%ivarnd))
         lenscale%field%field0d%ioinfo%ID = input_obj%ivarnd
         call io_input_field(input_obj, lenscale%field%field0d)
      end if 

      nferr = nf_close(input_obj % rd_ncid)

    end subroutine init_lenscale
  
   
    subroutine postproc_lenscale(lenscale, dimm,  uh_method, n_smth_sl)

       implicit none

       type (Field1dReal), intent(inout), pointer :: lenscale
       character (len=*), intent(in) :: uh_method
       integer, intent(in) :: dimm, n_smth_sl


       real, parameter :: spike_tolerance = 1.5      ! Threshold for detecting spikes in data.
       real :: mean_scale
       real, allocatable :: sl_smth(:)
       integer :: k, kdum


       if (trim(uh_method) == 'scale') then
         
          allocate(sl_smth(1:dimm)) 
          ! check the data
          ! If missing value is encountered, use the value from the last
          do k = 1, dimm
             if ( lenscale%array(k) .le. 0.0) then 
               lenscale%array(k) = lenscale%array(k-1)
             end if
          end do

          ! Remove spikes in lengthscales (extrapolate if spike detected):
          do k = 2, dimm-1
             mean_scale = 0.5 * (lenscale%array(k-1) + lenscale%array(k+1))
             if ( lenscale%array(k) > spike_tolerance*mean_scale ) then
                lenscale%array(k) = mean_scale
             end if
          end do

          ! Smoothing the scale_length
          sl_smth(1:dimm) = lenscale%array(1:dimm)
          do kdum = 1, n_smth_sl
             do k = 2, dimm-1
                 sl_smth(k) = lenscale%array(k) &
                   + 0.25*(lenscale%array(k-1)+(k+1)-2.0*lenscale%array(k))
             end do
          end do
          lenscale%array(1:dimm) = sl_smth(1:dimm)

          deallocate(sl_smth)    

       end if


    end subroutine postproc_lenscale 


!======================================================================================
! READ THE NAMELIST PARAMETERS
!======================================================================================

    subroutine io_input_namelist(input_obj, uh_method, nvar, cvlist, cvlistu, vardimlist, covarID)

      implicit none

      type (io_input_object), intent(inout) :: input_obj
      integer, dimension(:,:), intent(inout) :: covarID
      character (len=StrLen), dimension(:), intent(inout) :: cvlist, cvlistu
      integer, dimension(:), intent(inout) :: vardimlist
      character (len=StrLen), intent(inout) :: uh_method
      integer, intent(in) :: nvar
      
      integer :: nferr, VarID
      integer, dimension(1) :: start1, count1
      integer, dimension(2) :: start2, count2

      include 'netcdf.inc'

      ! read covarID, matrice of covarance between variable
      nferr = nf_inq_varid(input_obj%rd_ncid, 'covarID', &
                       input_obj % rdVarIDcovar)
      start2(1) = 1
      count2(1) = nvar
      start2(2) = 1
      count2(2) = nvar
      VarID = input_obj % rdVarIDcovar
      nferr = nf_get_vara_int(input_obj % rd_ncid, varID, start2, count2, covarID)

      ! read uh_method : scale or power 
      nferr = nf_inq_varid(input_obj%rd_ncid, 'uh_method', &
                       input_obj % rdVarIDnaml(1))
      start1(1) = 1
      count1(1) = StrLen
      VarID = input_obj % rdVarIDnaml(1)
      nferr = nf_get_vara_text(input_obj % rd_ncid, varID, start1, count1, uh_method )

      ! read list of cv
      nferr = nf_inq_varid(input_obj%rd_ncid, 'cv_list', &
                       input_obj % rdVarIDnaml(2))
      start2(2) = 1
      count2(2) = nvar
      start2(1) = 1
      count2(1) = StrLen
      VarID = input_obj % rdVarIDnaml(2)
      nferr = nf_get_vara_text(input_obj % rd_ncid, varID, start2, count2, cvlist )  
      write(*,*)'nf_strerror ', nf_strerror(nferr)

      ! read list of cv
      nferr = nf_inq_varid(input_obj%rd_ncid, 'cv_listu', &
                       input_obj % rdVarIDnaml(3))
      start2(2) = 1
      count2(2) = nvar
      start2(1) = 1
      count2(1) = StrLen
      VarID = input_obj % rdVarIDnaml(3)
      nferr = nf_get_vara_text(input_obj % rd_ncid, varID, start2, count2, cvlistu )
      write(*,*)'nf_strerror ', nf_strerror(nferr)

      ! read vardimlist
      nferr = nf_inq_varid(input_obj%rd_ncid, 'vardim_list', &
                       input_obj % rdVarIDnaml(4))
      start1(1) = 1
      count1(1) = nvar
      VarID = input_obj % rdVarIDnaml(4)
      nferr = nf_get_vara_int(input_obj % rd_ncid, varID, start1, count1, covarID)
      

    end subroutine io_input_namelist
 
   
!======================================================================================
! READ THE BASICS
!======================================================================================

   subroutine io_input_field0dReal(input_obj, field)
 
      implicit none

      type (io_input_object), intent(in) :: input_obj      
      type (field0dReal), intent(inout) :: field
 
      include 'netcdf.inc'
 
      integer :: nferr
      integer :: varID
      integer, dimension(1) :: start1, count1
 
      start1(1) = 1
      count1(1) = 1

      if (trim(field % ioinfo % fieldName) == 'ds') then
         varID = input_obj % rdVarIDds
      else 
         varID = input_obj % rdVarIDvarnd(field % ioinfo % ID)
      end if

      nferr = nf_get_vara_double(input_obj % rd_ncid, varID, start1, count1, field % scalar)
 
   end subroutine io_input_field0dReal


   subroutine io_input_field1dReal(input_obj, field)
 
      implicit none

      type (io_input_object), intent(in) :: input_obj      
      type (field1dReal), intent(inout) :: field
 
      include 'netcdf.inc'
 
      integer :: nferr
      integer :: varID
      integer, dimension(1) :: start1, count1
 
      start1(1) = field % ioinfo % start(1)
      count1(1) = field % ioinfo % count(1)

      if (trim(field % ioinfo % fieldName) == 'znu') then
         varID = input_obj % rdVarIDznu
      else
         write(*,*) 'field % ioinfo % ID ',field % ioinfo % ID
         varID = input_obj % rdVarIDvarnd(field % ioinfo % ID)
      end if

      nferr = nf_get_vara_double(input_obj % rd_ncid, varID, start1, count1, field % array)
 
   end subroutine io_input_field1dReal


   subroutine io_input_field2dReal(input_obj, field)
 
      implicit none

      type (io_input_object), intent(in) :: input_obj      
      type (field2dReal), intent(inout) :: field
 
      include 'netcdf.inc'
 
      integer :: nferr
      integer :: varID
      integer, dimension(2) :: start2, count2
      real ,allocatable, dimension(:,:) :: tmp2

 
      start2(1) = field % ioinfo % start(1)
      start2(2) = field % ioinfo % start(2)
      count2(1) = field % ioinfo % count(1)
      count2(2) = field % ioinfo % count(2)

      write(*,*)'====== io_input_field2dReal ',trim(field % ioinfo % fieldName)
      if (trim(field % ioinfo % fieldName) == 'lat') then
         write(*,*) 'lat io_output_field2dReal'
         varID = input_obj % rdVarIDlat
      else if (trim(field % ioinfo % fieldName) == 'lon') then
         write(*,*) 'lon io_output_field2dReal'
         varID = input_obj % rdVarIDlon
      else if (trim(field % ioinfo % fieldName) == 'mapfac_m') then
         write(*,*) 'mapfac_m io_output_field2dReal'
         varID = input_obj % rdVarIDmfm
      else if (trim(field % ioinfo % fieldName) == 'mapfac_u') then
         varID = input_obj % rdVarIDmfu
      else if (trim(field % ioinfo % fieldName) == 'mapfac_v') then
         varID = input_obj % rdVarIDmfv
      else
         varID = input_obj % rdVarIDvarnd(field % ioinfo % ID)
      end if
     write(*,*)'varID ',varID 
      nferr = nf_get_vara_double(input_obj % rd_ncid, varID, start2, count2, field % array)

      write(*,*)'nf_strerror ', nf_strerror(nferr)

      write(*,*)'====== end of io_input_field2dReal'

   end subroutine io_input_field2dReal


   subroutine io_input_field3dReal(input_obj, field)
 
      implicit none

      type (io_input_object), intent(in) :: input_obj      
      type (field3dReal), intent(inout) :: field
 
      include 'netcdf.inc'
 
      integer :: nferr
      integer :: varID
      integer, dimension(3) :: start3, count3
 
      start3(1) = field % ioinfo % start(1)
      start3(2) = field % ioinfo % start(2)
      start3(3) = field % ioinfo % start(3)
      count3(1) = field % ioinfo % count(1)
      count3(2) = field % ioinfo % count(2)
      count3(3) = field % ioinfo % count(3)

      if (trim(field % ioinfo % fieldName) == 'height') then
         write(*,*) 'height io_output_field3dReal'
         varID = input_obj % rdVarIDhgt
      else
         varID = input_obj % rdVarIDvarnd(field % ioinfo % ID)
      end if

      nferr = nf_get_vara_double(input_obj % rd_ncid, varID, start3, count3, field % array)
      write(*,*)'io_input_field3dReal ',nferr


   end subroutine io_input_field3dReal


   subroutine io_input_field0dReal_time(input_obj, field)
 
      implicit none

      type (io_input_object), intent(in) :: input_obj      
      type (field0dReal), intent(inout) :: field
 
      include 'netcdf.inc'
 
      integer :: nferr
      integer :: varID
      integer, dimension(1) :: start1, count1
 
      start1(1) = input_obj % time
      count1(1) = 1
 
    !  if (trim(field % ioinfo % fieldName) == 'xtime') then
    !     varID = input_obj % rdVarIDxtime
    !  end if

    !  nferr = nf_get_vara_double(input_obj % rd_ncid, varID, start1, count1, field % scalar)

   end subroutine io_input_field0dReal_time


   subroutine io_input_field1dChar_time(input_obj, field)

      implicit none

      type (io_input_object), intent(in) :: input_obj
    !  type (field1dChar), intent(inout) :: field
      character (len=StrLen), intent(in) :: field

      include 'netcdf.inc'

      integer :: nferr
      integer :: varID
      integer, dimension(2) :: start2, count2

      
      start2(2) = input_obj % time
      count2(2) = StrLen
      start2(1) = 1 !field % ioinfo % count(1)
      count2(1) = 1


      nferr = nf_get_vara_text(input_obj % rd_ncid, varID, start2, count2, field)

   end subroutine io_input_field1dChar_time


   subroutine io_input_field1dReal_time(input_obj, field)
 
      implicit none

      type (io_input_object), intent(in) :: input_obj      
      type (field1dReal), intent(inout) :: field
 
      include 'netcdf.inc'
 
      integer :: nferr
      integer :: varID
      integer, dimension(2) :: start2, count2
 
      start2(1) = field % ioinfo % start(1)
      start2(2) = input_obj % time
      count2(1) = field % ioinfo % count(1)
      count2(2) = 1
 
    !  if (trim(field % ioinfo % fieldName) == 'cofrz') then
    !     varID = input_obj % rdVarIDcofrz
    !  end if

    !  nferr = nf_get_vara_double(input_obj % rd_ncid, varID, start2, count2, field % array)

   end subroutine io_input_field1dReal_time


   subroutine io_input_field2dReal_time(input_obj, field)
 
      implicit none

      type (io_input_object), intent(in) :: input_obj      
      type (field2dReal), intent(inout) :: field
 
      include 'netcdf.inc'
 
      integer :: nferr
      integer :: varID
      integer, dimension(3) :: start3, count3

      real(kind=8) ,allocatable, dimension(:,:,:) :: tmp2
 
      start3(1) = field % ioinfo % start(1)
      start3(2) = field % ioinfo % start(2)
      start3(3) = input_obj % time
      count3(1) = field % ioinfo % count(1)
      count3(2) = field % ioinfo % count(2)
      count3(3) = 1

      allocate ( tmp2(start3(1):count3(1), start3(2):count3(2), start3(3):count3(3) ) )
 
      !write(*,*)'====== io_input_field2dReal_time ',trim(field % ioinfo % fieldName)
      varID = input_obj % rdVarIDvarnd( field % ioinfo % ID )
      nferr = nf_get_vara_double(input_obj % rd_ncid, varID, start3, count3, tmp2)
      !nferr = nf_get_vara_double(input_obj % rd_ncid, varID, start3, count3, field%array)
      
      field % array = tmp2(:,:,1)
      !write(*,*)'tmp2 ',tmp2(1,1,1)
      deallocate( tmp2 )
      !write(*,*)'nf_strerror ', nf_strerror(nferr)
      !write(*,*)'====== io_input_field2dReal_time '

   end subroutine io_input_field2dReal_time


   subroutine io_input_field3dReal_time(input_obj, field)
 
      implicit none

      type (io_input_object), intent(in) :: input_obj      
      type (field3dReal), intent(inout) :: field
 
      include 'netcdf.inc'
 
      integer :: nferr
      integer :: varID
      integer, dimension(4) :: start4, count4
      integer :: vv
      real(kind=8) , allocatable, dimension(:,:,:,:) :: tmp2
 
      start4(1) = field % ioinfo % start(1)
      start4(2) = field % ioinfo % start(2)
      start4(3) = field % ioinfo % start(3)
      start4(4) = input_obj % time
      count4(1) = field % ioinfo % count(1)
      count4(2) = field % ioinfo % count(2)
      count4(3) = field % ioinfo % count(3)
      count4(4) = 1

      allocate ( tmp2(start4(1):count4(1), start4(2):count4(2), start4(3):count4(3), start4(4):count4(4) ) )

        write(*,*)'=======  io_input_field3dReal_time ',trim(field % ioinfo % fieldName), input_obj % time
        varID = input_obj%rdVarIDvarnd( field%ioinfo%ID   )
        write(*,*)'varID ',varID
        nferr = nf_get_vara_double(input_obj % rd_ncid, varID, start4, count4, tmp2)
        field % array = tmp2(:,:,:,1)
       ! write(*,*)'tmp2 ',tmp2(1,1,1,1)
       ! write(*,*)'nferr ',nferr     

      deallocate( tmp2 )

   end subroutine io_input_field3dReal_time


   subroutine io_input_field1dInteger(input_obj, field)
 
      implicit none

      type (io_input_object), intent(in) :: input_obj      
      type (field1dInteger), intent(inout) :: field
 
      include 'netcdf.inc'
 
      integer :: nferr
      integer :: varID
      integer, dimension(1) :: start1, count1
 
      start1(1) = field % ioinfo % start(1)
      count1(1) = field % ioinfo % count(1)

      if (trim(field % ioinfo % fieldName) == 'bin_pts') then
          varID = input_obj % rdVarIDbinpts
      else if(trim(field % ioinfo % fieldName) == 'bin2d_pts') then
          varID = input_obj % rdVarIDbin2dpts
      else
          varID = input_obj%rdVarIDvarnd( field%ioinfo%ID)
      end if

      nferr = nf_get_vara_int(input_obj % rd_ncid, varID, start1, count1, field % array)
 
   end subroutine io_input_field1dInteger


   subroutine io_input_field2dInteger(input_obj, field)
 
      implicit none

      type (io_input_object), intent(in) :: input_obj      
      type (field2dInteger), intent(inout) :: field
 
      include 'netcdf.inc'
 
      integer :: nferr
      integer :: varID
      integer, dimension(2) :: start2, count2
 
      start2(1) = field % ioinfo % start(1)
      start2(2) = field % ioinfo % start(2)
      count2(1) = field % ioinfo % count(1)
      count2(2) = field % ioinfo % count(2)

      if (trim(field % ioinfo % fieldName) == 'bin2d') then
          varID = input_obj % rdVarIDbin2d
      else
          varID = input_obj%rdVarIDvarnd( field%ioinfo%ID)
      end if

      nferr = nf_get_vara_int(input_obj % rd_ncid, varID, start2, count2, field % array)
      write(*,*)'nf_strerror ', nf_strerror(nferr)
      !write(*,*)'field % array',field % array 

   end subroutine io_input_field2dInteger


   subroutine io_input_field3dInteger(input_obj, field)

      implicit none

      type (io_input_object), intent(in) :: input_obj
      type (field3dInteger), intent(inout) :: field

      include 'netcdf.inc'

      integer :: nferr
      integer :: varID
      integer, dimension(3) :: start3, count3

      start3(1) = field % ioinfo % start(1)
      start3(2) = field % ioinfo % start(2)
      start3(3) = field % ioinfo % start(3)
      count3(1) = field % ioinfo % count(1)
      count3(2) = field % ioinfo % count(2)
      count3(3) = field % ioinfo % count(3)

      if (trim(field % ioinfo % fieldName) == 'bin') then
         varID = input_obj % rdVarIDbin
      else
          varID = input_obj%rdVarIDvarnd( field%ioinfo%ID)
      end if
      nferr = nf_get_vara_int(input_obj % rd_ncid, varID, start3, count3, field % array)

   end subroutine io_input_field3dInteger



 
end module io_input
