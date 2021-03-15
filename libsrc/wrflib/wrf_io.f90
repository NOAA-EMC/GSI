!*------------------------------------------------------------------------------
!* Standard Disclaimer
!*
!* Forecast Systems Laboratory
!* NOAA/OAR/ERL/FSL
!* 325 Broadway
!* Boulder, CO 80303
!*
!* AVIATION DIVISION
!* ADVANCED COMPUTING BRANCH
!* SMS/NNT Version: 2.0.0
!*
!* This software and its documentation are in the public domain and
!* are furnished "as is". The United States government, its
!* instrumentalities, officers, employees, and agents make no
!* warranty, express or implied, as to the usefulness of the software
!* and documentation for any purpose. They assume no
!* responsibility (1) for the use of the software and documentation;
!* or (2) to provide technical support to users.
!*
!* Permission to use, copy, modify, and distribute this software is
!* hereby granted, provided that this disclaimer notice appears in
!* all copies. All modifications to this software must be clearly
!* documented, and are solely the responsibility of the agent making
!* the modification. If significant modifications or enhancements
!* are made to this software, the SMS Development team
!* (sms-info@fsl.noaa.gov) should be notified.
!*
!*----------------------------------------------------------------------------
!*
!* WRF NetCDF I/O
! Author: Jacques Middlecoff jacquesm@fsl.noaa.gov
!* Date: October 6, 2000
!*
!*----------------------------------------------------------------------------

module wrf_data

  integer , parameter :: FATAL = 1
  integer , parameter :: WARN = 1
  integer , parameter :: WrfDataHandleMax = 99
  integer , parameter :: MaxDims = 2000 ! = NF_MAX_VARS



  integer , parameter :: MaxVars = 3000

  integer , parameter :: MaxTimes = 10000
  integer , parameter :: DateStrLen = 19
  integer , parameter :: VarNameLen = 31
  integer , parameter :: NO_DIM = 0
  integer , parameter :: NVarDims = 4
  integer , parameter :: NMDVarDims = 2
  character (8) , parameter :: NO_NAME = 'NULL'
  character (DateStrLen) , parameter :: ZeroDate = '0000-00-00-00:00:00'
      integer, parameter :: WRF_FILE_NOT_OPENED = 100
      integer, parameter :: WRF_FILE_OPENED_NOT_COMMITTED = 101
      integer, parameter :: WRF_FILE_OPENED_FOR_WRITE = 102
      integer, parameter :: WRF_FILE_OPENED_FOR_READ = 103
      integer, parameter :: WRF_REAL = 104
      integer, parameter :: WRF_DOUBLE = 105
      integer, parameter :: WRF_FLOAT=WRF_REAL
      integer, parameter :: WRF_INTEGER = 106
      integer, parameter :: WRF_LOGICAL = 107
      integer, parameter :: WRF_COMPLEX = 108
      integer, parameter :: WRF_DOUBLE_COMPLEX = 109
      integer, parameter :: WRF_FILE_OPENED_FOR_UPDATE = 110
! This bit is for backwards compatibility with old variants of these flags
! that are still being used in io_grib1 and io_phdf5. It should be removed!
      integer, parameter :: WRF_FILE_OPENED_AND_COMMITTED = 102
  character (256) :: msg
  logical :: WrfIOnotInitialized = .true.
  type :: wrf_data_handle
    character (255) :: FileName
    integer :: FileStatus
    integer :: Comm
    integer :: NCID
    logical :: Free
    logical :: Write
    character (5) :: TimesName
    integer :: TimeIndex
    integer :: CurrentTime !Only used for read
    integer :: NumberTimes !Only used for read
    character (DateStrLen), pointer :: Times(:)
    integer :: TimesVarID
    integer , pointer :: DimLengths(:)
    integer , pointer :: DimIDs(:)
    character (31) , pointer :: DimNames(:)
    integer :: DimUnlimID
    character (9) :: DimUnlimName
    integer , dimension(NVarDims) :: DimID
    integer , dimension(NVarDims) :: Dimension
    integer , pointer :: MDVarIDs(:)
    integer , pointer :: MDVarDimLens(:)
    character (80) , pointer :: MDVarNames(:)
    integer , pointer :: VarIDs(:)
    integer , pointer :: VarDimLens(:,:)
    character (VarNameLen), pointer :: VarNames(:)
    integer :: CurrentVariable !Only used for read
    integer :: NumVars
! first_operation is set to .TRUE. when a new handle is allocated
! or when open-for-write or open-for-read are committed. It is set
! to .FALSE. when the first field is read or written.
    logical :: first_operation
    logical :: R4OnOutput
    logical :: nofill
    logical :: use_netcdf_classic
  end type wrf_data_handle
  type(wrf_data_handle),target :: WrfDataHandles(WrfDataHandleMax)
end module wrf_data
module ext_ncd_support_routines
  implicit none
CONTAINS
subroutine allocHandle(DataHandle,DH,Comm,Status)
  use wrf_data
  include 'wrf_status_codes.h'
  integer ,intent(out) :: DataHandle
  type(wrf_data_handle),pointer :: DH
  integer ,intent(IN) :: Comm
  integer ,intent(out) :: Status
  integer :: i
  integer :: stat
  do i=1,WrfDataHandleMax
    if(WrfDataHandles(i)%Free) then
      DH => WrfDataHandles(i)
      DataHandle = i
      allocate(DH%Times(MaxTimes), STAT=stat)
      if(stat/= 0) then
        Status = WRF_ERR_FATAL_ALLOCATION_ERROR
        write(msg,*) 'Fatal ALLOCATION ERROR in ',"wrf_io.F90",', line', 124
        call wrf_debug ( FATAL , msg)
        return
      endif
      allocate(DH%DimLengths(MaxDims), STAT=stat)
      if(stat/= 0) then
        Status = WRF_ERR_FATAL_ALLOCATION_ERROR
        write(msg,*) 'Fatal ALLOCATION ERROR in ',"wrf_io.F90",', line', 131
        call wrf_debug ( FATAL , msg)
        return
      endif
      allocate(DH%DimIDs(MaxDims), STAT=stat)
      if(stat/= 0) then
        Status = WRF_ERR_FATAL_ALLOCATION_ERROR
        write(msg,*) 'Fatal ALLOCATION ERROR in ',"wrf_io.F90",', line', 138
        call wrf_debug ( FATAL , msg)
        return
      endif
      allocate(DH%DimNames(MaxDims), STAT=stat)
      if(stat/= 0) then
        Status = WRF_ERR_FATAL_ALLOCATION_ERROR
        write(msg,*) 'Fatal ALLOCATION ERROR in ',"wrf_io.F90",', line', 145
        call wrf_debug ( FATAL , msg)
        return
      endif
      allocate(DH%MDVarIDs(MaxVars), STAT=stat)
      if(stat/= 0) then
        Status = WRF_ERR_FATAL_ALLOCATION_ERROR
        write(msg,*) 'Fatal ALLOCATION ERROR in ',"wrf_io.F90",', line', 152
        call wrf_debug ( FATAL , msg)
        return
      endif
      allocate(DH%MDVarDimLens(MaxVars), STAT=stat)
      if(stat/= 0) then
        Status = WRF_ERR_FATAL_ALLOCATION_ERROR
        write(msg,*) 'Fatal ALLOCATION ERROR in ',"wrf_io.F90",', line', 159
        call wrf_debug ( FATAL , msg)
        return
      endif
      allocate(DH%MDVarNames(MaxVars), STAT=stat)
      if(stat/= 0) then
        Status = WRF_ERR_FATAL_ALLOCATION_ERROR
        write(msg,*) 'Fatal ALLOCATION ERROR in ',"wrf_io.F90",', line', 166
        call wrf_debug ( FATAL , msg)
        return
      endif
      allocate(DH%VarIDs(MaxVars), STAT=stat)
      if(stat/= 0) then
        Status = WRF_ERR_FATAL_ALLOCATION_ERROR
        write(msg,*) 'Fatal ALLOCATION ERROR in ',"wrf_io.F90",', line', 173
        call wrf_debug ( FATAL , msg)
        return
      endif
      allocate(DH%VarDimLens(NVarDims-1,MaxVars), STAT=stat)
      if(stat/= 0) then
        Status = WRF_ERR_FATAL_ALLOCATION_ERROR
        write(msg,*) 'Fatal ALLOCATION ERROR in ',"wrf_io.F90",', line', 180
        call wrf_debug ( FATAL , msg)
        return
      endif
      allocate(DH%VarNames(MaxVars), STAT=stat)
      if(stat/= 0) then
        Status = WRF_ERR_FATAL_ALLOCATION_ERROR
        write(msg,*) 'Fatal ALLOCATION ERROR in ',"wrf_io.F90",', line', 187
        call wrf_debug ( FATAL , msg)
        return
      endif
      exit
    endif
    if(i==WrfDataHandleMax) then
      Status = WRF_WARN_TOO_MANY_FILES
      write(msg,*) 'Warning TOO MANY FILES in ',"wrf_io.F90",', line', 195
      call wrf_debug ( WARN , TRIM(msg))
      write(msg,*) 'Did you call ext_ncd_ioinit?'
      call wrf_debug ( WARN , TRIM(msg))
      return
    endif
  enddo
  DH%Free =.false.
  DH%Comm = Comm
  DH%Write =.false.
  DH%first_operation = .TRUE.
  DH%R4OnOutput = .false.
  DH%nofill = .false.
  Status = WRF_NO_ERR
end subroutine allocHandle
subroutine deallocHandle(DataHandle, Status)
  use wrf_data
  include 'wrf_status_codes.h'
  integer ,intent(in) :: DataHandle
  integer ,intent(out) :: Status
  type(wrf_data_handle),pointer :: DH
  integer :: i
  integer :: stat
  IF ( DataHandle .GE. 1 .AND. DataHandle .LE. WrfDataHandleMax ) THEN
    if(.NOT. WrfDataHandles(DataHandle)%Free) then
      DH => WrfDataHandles(DataHandle)
      deallocate(DH%Times, STAT=stat)
      if(stat/= 0) then
        Status = WRF_ERR_FATAL_DEALLOCATION_ERR
        write(msg,*) 'Fatal DEALLOCATION ERROR in ',"wrf_io.F90",', line', 226
        call wrf_debug ( FATAL , msg)
        return
      endif
      deallocate(DH%DimLengths, STAT=stat)
      if(stat/= 0) then
        Status = WRF_ERR_FATAL_DEALLOCATION_ERR
        write(msg,*) 'Fatal DEALLOCATION ERROR in ',"wrf_io.F90",', line', 233
        call wrf_debug ( FATAL , msg)
        return
      endif
      deallocate(DH%DimIDs, STAT=stat)
      if(stat/= 0) then
        Status = WRF_ERR_FATAL_DEALLOCATION_ERR
        write(msg,*) 'Fatal DEALLOCATION ERROR in ',"wrf_io.F90",', line', 240
        call wrf_debug ( FATAL , msg)
        return
      endif
      deallocate(DH%DimNames, STAT=stat)
      if(stat/= 0) then
        Status = WRF_ERR_FATAL_DEALLOCATION_ERR
        write(msg,*) 'Fatal ALLOCATION ERROR in ',"wrf_io.F90",', line', 247
        call wrf_debug ( FATAL , msg)
        return
      endif
      deallocate(DH%MDVarIDs, STAT=stat)
      if(stat/= 0) then
        Status = WRF_ERR_FATAL_DEALLOCATION_ERR
        write(msg,*) 'Fatal DEALLOCATION ERROR in ',"wrf_io.F90",', line', 254
        call wrf_debug ( FATAL , msg)
        return
      endif
      deallocate(DH%MDVarDimLens, STAT=stat)
      if(stat/= 0) then
        Status = WRF_ERR_FATAL_DEALLOCATION_ERR
        write(msg,*) 'Fatal DEALLOCATION ERROR in ',"wrf_io.F90",', line', 261
        call wrf_debug ( FATAL , msg)
        return
      endif
      deallocate(DH%MDVarNames, STAT=stat)
      if(stat/= 0) then
        Status = WRF_ERR_FATAL_DEALLOCATION_ERR
        write(msg,*) 'Fatal DEALLOCATION ERROR in ',"wrf_io.F90",', line', 268
        call wrf_debug ( FATAL , msg)
        return
      endif
      deallocate(DH%VarIDs, STAT=stat)
      if(stat/= 0) then
        Status = WRF_ERR_FATAL_DEALLOCATION_ERR
        write(msg,*) 'Fatal DEALLOCATION ERROR in ',"wrf_io.F90",', line', 275
        call wrf_debug ( FATAL , msg)
        return
      endif
      deallocate(DH%VarDimLens, STAT=stat)
      if(stat/= 0) then
        Status = WRF_ERR_FATAL_DEALLOCATION_ERR
        write(msg,*) 'Fatal DEALLOCATION ERROR in ',"wrf_io.F90",', line', 282
        call wrf_debug ( FATAL , msg)
        return
      endif
      deallocate(DH%VarNames, STAT=stat)
      if(stat/= 0) then
        Status = WRF_ERR_FATAL_DEALLOCATION_ERR
        write(msg,*) 'Fatal DEALLOCATION ERROR in ',"wrf_io.F90",', line', 289
        call wrf_debug ( FATAL , msg)
        return
      endif
      DH%Free =.TRUE.
    endif
  ENDIF
  Status = WRF_NO_ERR
end subroutine deallocHandle
subroutine GetDH(DataHandle,DH,Status)
  use wrf_data
  include 'wrf_status_codes.h'
  integer ,intent(in) :: DataHandle
  type(wrf_data_handle) ,pointer :: DH
  integer ,intent(out) :: Status
  if(DataHandle < 1 .or. DataHandle > WrfDataHandleMax) then
    Status = WRF_WARN_BAD_DATA_HANDLE
    return
  endif
  DH => WrfDataHandles(DataHandle)
  if(DH%Free) then
    Status = WRF_WARN_BAD_DATA_HANDLE
    return
  endif
  Status = WRF_NO_ERR
  return
end subroutine GetDH
subroutine DateCheck(Date,Status)
  use wrf_data
  include 'wrf_status_codes.h'
  character*(*) ,intent(in) :: Date
  integer ,intent(out) :: Status
  if(len(Date) /= DateStrLen) then
    Status = WRF_WARN_DATESTR_BAD_LENGTH
  else
    Status = WRF_NO_ERR
  endif
  return
end subroutine DateCheck

subroutine GetName(Element,Var,Name,Status)
  use wrf_data
  include 'wrf_status_codes.h'
  character*(*) ,intent(in) :: Element
  character*(*) ,intent(in) :: Var
  character*(*) ,intent(out) :: Name
  integer ,intent(out) :: Status
  character (VarNameLen) :: VarName
  character (1) :: c
  integer :: i
  integer, parameter :: upper_to_lower =IACHAR('a')-IACHAR('A')

  VarName = Var
  Name = 'MD___'
  do i=1,len(Name)
    c=Name(i:i)
    if('A'<=c .and. c <='Z') Name(i:i)=achar(iachar(c)+upper_to_lower)
    if(c=='-'.or.c==':') Name(i:i)='_'
  enddo
  Status = WRF_NO_ERR
  return
end subroutine GetName

subroutine GetTimeIndex(IO,DataHandle,DateStr,TimeIndex,Status)
  use wrf_data
  include 'wrf_status_codes.h'
  include 'netcdf.inc'
  character (*) ,intent(in) :: IO
  integer ,intent(in) :: DataHandle
  character*(*) ,intent(in) :: DateStr
  integer ,intent(out) :: TimeIndex
  integer ,intent(out) :: Status
  type(wrf_data_handle) ,pointer :: DH
  integer :: VStart(2)
  integer :: VCount(2)
  integer :: stat
  integer :: i

  DH => WrfDataHandles(DataHandle)
  call DateCheck(DateStr,Status)
  if(Status /= WRF_NO_ERR) then
    Status = WRF_WARN_DATESTR_ERROR
    write(msg,*) 'Warning DATE STRING ERROR in ',"wrf_io.F90",', line', 375
    call wrf_debug ( WARN , TRIM(msg))
    return
  endif
  if(IO == 'write') then
    TimeIndex = DH%TimeIndex
    if(TimeIndex <= 0) then
      TimeIndex = 1
    elseif(DateStr == DH%Times(TimeIndex)) then
      Status = WRF_NO_ERR
      return
    else
      TimeIndex = TimeIndex +1
      if(TimeIndex > MaxTimes) then
        Status = WRF_WARN_TIME_EOF
        write(msg,*) 'Warning TIME EOF in ',"wrf_io.F90",', line', 390
        call wrf_debug ( WARN , TRIM(msg))
        return
      endif
    endif
    DH%TimeIndex = TimeIndex
    DH%Times(TimeIndex) = DateStr
    VStart(1) = 1
    VStart(2) = TimeIndex
    VCount(1) = DateStrLen
    VCount(2) = 1
    stat = NF_PUT_VARA_TEXT(DH%NCID,DH%TimesVarID,VStart,VCount,DateStr)
    call netcdf_err(stat,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) 'NetCDF error in ',"wrf_io.F90",', line', 404
      call wrf_debug ( WARN , TRIM(msg))
      return
    endif
  else
    do i=1,MaxTimes
      if(DH%Times(i)==DateStr) then
        Status = WRF_NO_ERR
        TimeIndex = i
        exit
      endif
      if(i==MaxTimes) then
        Status = WRF_WARN_TIME_NF
        write(msg,*) 'Warning TIME ',DateStr,' NOT FOUND in ',"wrf_io.F90",', line', 417
        call wrf_debug ( WARN , TRIM(msg))
        return
      endif
    enddo
  endif
  return
end subroutine GetTimeIndex

subroutine GetDim(MemoryOrder,NDim,Status)
  include 'wrf_status_codes.h'
  character*(*) ,intent(in) :: MemoryOrder
  integer ,intent(out) :: NDim
  integer ,intent(out) :: Status
  character*3 :: MemOrd

  call LowerCase(MemoryOrder,MemOrd)
  select case (MemOrd)
    case ('xyz','xzy','yxz','yzx','zxy','zyx','xsz','xez','ysz','yez')
      NDim = 3
    case ('xy','yx','xs','xe','ys','ye','cc')
      NDim = 2
    case ('z','c')
      NDim = 1
    case ('0') ! NDim=0 for scalars. TBH: 20060502
      NDim = 0
    case default
      Status = WRF_WARN_BAD_MEMORYORDER
      return
  end select
  Status = WRF_NO_ERR
  return
end subroutine GetDim
subroutine GetIndices(NDim,Start,End,i1,i2,j1,j2,k1,k2)
  integer ,intent(in) :: NDim
  integer ,dimension(*),intent(in) :: Start,End
  integer ,intent(out) :: i1,i2,j1,j2,k1,k2
  i1=1
  i2=1
  j1=1
  j2=1
  k1=1
  k2=1
  if(NDim == 0) return ! NDim=0 for scalars. TBH: 20060502
  i1 = Start(1)
  i2 = End (1)
  if(NDim == 1) return
  j1 = Start(2)
  j2 = End (2)
  if(NDim == 2) return
  k1 = Start(3)
  k2 = End (3)
  return
end subroutine GetIndices
logical function ZeroLengthHorzDim(MemoryOrder,Vector,Status)
  use wrf_data
  include 'wrf_status_codes.h'
  character*(*) ,intent(in) :: MemoryOrder
  integer,dimension(*) ,intent(in) :: Vector
  integer ,intent(out) :: Status
  integer :: NDim
  integer,dimension(NVarDims) :: temp
  character*3 :: MemOrd
  logical zero_length
  call GetDim(MemoryOrder,NDim,Status)
  temp(1:NDim) = Vector(1:NDim)
  call LowerCase(MemoryOrder,MemOrd)
  zero_length = .false.
  select case (MemOrd)
    case ('xsz','xez','ysz','yez','xs','xe','ys','ye','z','c')
      continue
    case ('0')
      continue ! NDim=0 for scalars. TBH: 20060502
    case ('xzy','yzx')
      zero_length = temp(1) .lt. 1 .or. temp(3) .lt. 1
    case ('xy','yx','xyz','yxz')
      zero_length = temp(1) .lt. 1 .or. temp(2) .lt. 1
    case ('zxy','zyx')
      zero_length = temp(2) .lt. 1 .or. temp(3) .lt. 1
    case default
      Status = WRF_WARN_BAD_MEMORYORDER
      ZeroLengthHorzDim = .true.
      return
  end select
  Status = WRF_NO_ERR
  ZeroLengthHorzDim = zero_length
  return
end function ZeroLengthHorzDim
subroutine ExtOrder(MemoryOrder,Vector,Status)
  use wrf_data
  include 'wrf_status_codes.h'
  character*(*) ,intent(in) :: MemoryOrder
  integer,dimension(*) ,intent(inout) :: Vector
  integer ,intent(out) :: Status
  integer :: NDim
  integer,dimension(NVarDims) :: temp
  character*3 :: MemOrd
  call GetDim(MemoryOrder,NDim,Status)
  temp(1:NDim) = Vector(1:NDim)
  call LowerCase(MemoryOrder,MemOrd)
  select case (MemOrd)
    case ('xyz','xsz','xez','ysz','yez','xy','xs','xe','ys','ye','z','c')
      continue
    case ('0')
      continue ! NDim=0 for scalars. TBH: 20060502
    case ('xzy')
      Vector(2) = temp(3)
      Vector(3) = temp(2)
    case ('yxz')
      Vector(1) = temp(2)
      Vector(2) = temp(1)
    case ('yzx')
      Vector(1) = temp(3)
      Vector(2) = temp(1)
      Vector(3) = temp(2)
    case ('zxy')
      Vector(1) = temp(2)
      Vector(2) = temp(3)
      Vector(3) = temp(1)
    case ('zyx')
      Vector(1) = temp(3)
      Vector(3) = temp(1)
    case ('yx')
      Vector(1) = temp(2)
      Vector(2) = temp(1)
    case default
      Status = WRF_WARN_BAD_MEMORYORDER
      return
  end select
  Status = WRF_NO_ERR
  return
end subroutine ExtOrder
subroutine ExtOrderStr(MemoryOrder,Vector,ROVector,Status)
  use wrf_data
  include 'wrf_status_codes.h'
  character*(*) ,intent(in) :: MemoryOrder
  character*(*),dimension(*) ,intent(in) :: Vector
  character(80),dimension(NVarDims),intent(out) :: ROVector
  integer ,intent(out) :: Status
  integer :: NDim
  character*3 :: MemOrd
  call GetDim(MemoryOrder,NDim,Status)
  ROVector(1:NDim) = Vector(1:NDim)
  call LowerCase(MemoryOrder,MemOrd)
  select case (MemOrd)
    case ('xyz','xsz','xez','ysz','yez','xy','xs','xe','ys','ye','z','c')
      continue
    case ('0')
      continue ! NDim=0 for scalars. TBH: 20060502
    case ('xzy')
      ROVector(2) = Vector(3)
      ROVector(3) = Vector(2)
    case ('yxz')
      ROVector(1) = Vector(2)
      ROVector(2) = Vector(1)
    case ('yzx')
      ROVector(1) = Vector(3)
      ROVector(2) = Vector(1)
      ROVector(3) = Vector(2)
    case ('zxy')
      ROVector(1) = Vector(2)
      ROVector(2) = Vector(3)
      ROVector(3) = Vector(1)
    case ('zyx')
      ROVector(1) = Vector(3)
      ROVector(3) = Vector(1)
    case ('yx')
      ROVector(1) = Vector(2)
      ROVector(2) = Vector(1)
    case default
      Status = WRF_WARN_BAD_MEMORYORDER
      return
  end select
  Status = WRF_NO_ERR
  return
end subroutine ExtOrderStr
subroutine LowerCase(MemoryOrder,MemOrd)
  character*(*) ,intent(in) :: MemoryOrder
  character*(*) ,intent(out) :: MemOrd
  character*1 :: c
  integer ,parameter :: upper_to_lower =IACHAR('a')-IACHAR('A')
  integer :: i,N
  MemOrd = ' '
  N = len(MemoryOrder)
  MemOrd(1:N) = MemoryOrder(1:N)
  do i=1,N
    c = MemoryOrder(i:i)
    if('A'<=c .and. c <='Z') MemOrd(i:i)=achar(iachar(c)+upper_to_lower)
  enddo
  return
end subroutine LowerCase
subroutine UpperCase(MemoryOrder,MemOrd)
  character*(*) ,intent(in) :: MemoryOrder
  character*(*) ,intent(out) :: MemOrd
  character*1 :: c
  integer ,parameter :: lower_to_upper =IACHAR('A')-IACHAR('a')
  integer :: i,N
  MemOrd = ' '
  N = len(MemoryOrder)
  MemOrd(1:N) = MemoryOrder(1:N)
  do i=1,N
    c = MemoryOrder(i:i)
    if('a'<=c .and. c <='z') MemOrd(i:i)=achar(iachar(c)+lower_to_upper)
  enddo
  return
end subroutine UpperCase
subroutine netcdf_err(err,Status)
  use wrf_data
  include 'wrf_status_codes.h'
  include 'netcdf.inc'
  integer ,intent(in) :: err
  integer ,intent(out) :: Status
  character(len=80) :: errmsg
  integer :: stat
  if( err==NF_NOERR )then
    Status = WRF_NO_ERR
  else
    errmsg = NF_STRERROR(err)
    write(msg,*) 'NetCDF error: ',errmsg
    call wrf_debug ( WARN , TRIM(msg))
    Status = WRF_WARN_NETCDF
  endif
  return
end subroutine netcdf_err
subroutine FieldIO(IO,DataHandle,DateStr,Length,MemoryOrder &
                     ,FieldType,NCID,VarID,XField,Status)
  use wrf_data
  include 'wrf_status_codes.h'
  include 'netcdf.inc'
  character (*) ,intent(in) :: IO
  integer ,intent(in) :: DataHandle
  character*(*) ,intent(in) :: DateStr
  integer,dimension(NVarDims),intent(in) :: Length
  character*(*) ,intent(in) :: MemoryOrder
  integer ,intent(in) :: FieldType
  integer ,intent(in) :: NCID
  integer ,intent(in) :: VarID
  integer,dimension(*) ,intent(inout) :: XField
  integer ,intent(out) :: Status
  integer :: TimeIndex
  integer :: NDim
  integer,dimension(NVarDims) :: VStart
  integer,dimension(NVarDims) :: VCount
! include 'wrf_io_flags.h'
  call GetTimeIndex(IO,DataHandle,DateStr,TimeIndex,Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) 'Warning in ',"wrf_io.F90",', line', 704
    call wrf_debug ( WARN , TRIM(msg))
    write(msg,*) '  Bad time index for DateStr = ',DateStr
    call wrf_debug ( WARN , TRIM(msg))
    return
  endif
  call GetDim(MemoryOrder,NDim,Status)
  VStart(:) = 1
  VCount(:) = 1
  VStart(1:NDim) = 1
  VCount(1:NDim) = Length(1:NDim)
  VStart(NDim+1) = TimeIndex
  VCount(NDim+1) = 1
  ! Do not use SELECT statement here as sometimes WRF_REAL=WRF_DOUBLE
  IF (FieldType == WRF_REAL) THEN
    call ext_ncd_RealFieldIO (IO,NCID,VarID,VStart,VCount,XField,Status)
  ELSE IF (FieldType == WRF_DOUBLE) THEN
    call ext_ncd_DoubleFieldIO (IO,NCID,VarID,VStart,VCount,XField,Status)
  ELSE IF (FieldType == WRF_INTEGER) THEN
    call ext_ncd_IntFieldIO (IO,NCID,VarID,VStart,VCount,XField,Status)
  ELSE IF (FieldType == WRF_LOGICAL) THEN
    call ext_ncd_LogicalFieldIO (IO,NCID,VarID,VStart,VCount,XField,Status)
    if(Status /= WRF_NO_ERR) return
  ELSE
   write(6,*) 'WARNING---- some missing calls commented out'
      Status = WRF_WARN_DATA_TYPE_NOT_FOUND
      write(msg,*) 'Warning DATA TYPE NOT FOUND in ',"wrf_io.F90",', line', 731
      call wrf_debug ( WARN , TRIM(msg))
      return
  END IF
  return
end subroutine FieldIO
subroutine Transpose(IO,MemoryOrder,di, Field,l1,l2,m1,m2,n1,n2 &
                                      ,XField,x1,x2,y1,y2,z1,z2 &
                                             ,i1,i2,j1,j2,k1,k2 )
  character*(*) ,intent(in) :: IO
  character*(*) ,intent(in) :: MemoryOrder
  integer ,intent(in) :: l1,l2,m1,m2,n1,n2
  integer ,intent(in) :: di
  integer ,intent(in) :: x1,x2,y1,y2,z1,z2
  integer ,intent(in) :: i1,i2,j1,j2,k1,k2
  integer ,intent(inout) :: Field(di,l1:l2,m1:m2,n1:n2)
!jm 010827 integer ,intent(inout) :: XField(di,x1:x2,y1:y2,z1:z2)
  integer ,intent(inout) :: XField(di,(i2-i1+1)*(j2-j1+1)*(k2-k1+1))
  character*3 :: MemOrd
  character*3 :: MemO
  integer ,parameter :: MaxUpperCase=IACHAR('Z')
  integer :: i,j,k,ix,jx,kx
  call LowerCase(MemoryOrder,MemOrd)
  select case (MemOrd)
! define(`XDEX',($1-``$1''1+1+(``$1''2-``$1''1+1)*(($2-``$2''1)+($3-``$3''1)*(``$2''2-``$2''1+1))))
    case ('xzy')
  ix=0
  jx=0
  kx=0
  call reorder(MemoryOrder,MemO)
  if(IACHAR(MemO(1:1)) > MaxUpperCase) ix=i2+i1
  if(IACHAR(MemO(2:2)) > MaxUpperCase) jx=j2+j1
  if(IACHAR(MemO(3:3)) > MaxUpperCase) kx=k2+k1
! pjj/cray
  if(IO == 'write') then
!!dir$ concurrent
!$OMP PARALLEL DO SCHEDULE(RUNTIME) PRIVATE(i,j,k)
  do k=k1,k2
    do j=j1,j2
!!dir$ prefervector
!!dir$ concurrent
!cdir select(vector)
      do i=i1,i2
          XField(1:di,i-i1+1+(i2-i1+1)*((k-k1)+(j-j1)*(k2-k1+1))) = Field(1:di,abs(ix-i),abs(jx-j),abs(kx-k))
      enddo
    enddo
  enddo
!$OMP END PARALLEL DO
else
!!dir$ concurrent
!$OMP PARALLEL DO SCHEDULE(RUNTIME) PRIVATE(i,j,k)
  do k=k1,k2
    do j=j1,j2
!!dir$ prefervector
!!dir$ concurrent
!cdir select(vector)
      do i=i1,i2
          Field(1:di,abs(ix-i),abs(jx-j),abs(kx-k)) = XField(1:di,i-i1+1+(i2-i1+1)*((k-k1)+(j-j1)*(k2-k1+1)))
      enddo
    enddo
  enddo
!$OMP END PARALLEL DO
endif
  return
    case ('xyz','xsz','xez','ysz','yez','xy','xs','xe','ys','ye','z','c','0')
  ix=0
  jx=0
  kx=0
  call reorder(MemoryOrder,MemO)
  if(IACHAR(MemO(1:1)) > MaxUpperCase) ix=i2+i1
  if(IACHAR(MemO(2:2)) > MaxUpperCase) jx=j2+j1
  if(IACHAR(MemO(3:3)) > MaxUpperCase) kx=k2+k1
! pjj/cray
  if(IO == 'write') then
!!dir$ concurrent
!$OMP PARALLEL DO SCHEDULE(RUNTIME) PRIVATE(i,j,k)
  do k=k1,k2
    do j=j1,j2
!!dir$ prefervector
!!dir$ concurrent
!cdir select(vector)
      do i=i1,i2
          XField(1:di,i-i1+1+(i2-i1+1)*((j-j1)+(k-k1)*(j2-j1+1))) = Field(1:di,abs(ix-i),abs(jx-j),abs(kx-k))
      enddo
    enddo
  enddo
!$OMP END PARALLEL DO
else
!!dir$ concurrent
!$OMP PARALLEL DO SCHEDULE(RUNTIME) PRIVATE(i,j,k)
  do k=k1,k2
    do j=j1,j2
!!dir$ prefervector
!!dir$ concurrent
!cdir select(vector)
      do i=i1,i2
          Field(1:di,abs(ix-i),abs(jx-j),abs(kx-k)) = XField(1:di,i-i1+1+(i2-i1+1)*((j-j1)+(k-k1)*(j2-j1+1)))
      enddo
    enddo
  enddo
!$OMP END PARALLEL DO
endif
  return
    case ('yxz')
  ix=0
  jx=0
  kx=0
  call reorder(MemoryOrder,MemO)
  if(IACHAR(MemO(1:1)) > MaxUpperCase) ix=i2+i1
  if(IACHAR(MemO(2:2)) > MaxUpperCase) jx=j2+j1
  if(IACHAR(MemO(3:3)) > MaxUpperCase) kx=k2+k1
! pjj/cray
  if(IO == 'write') then
!!dir$ concurrent
!$OMP PARALLEL DO SCHEDULE(RUNTIME) PRIVATE(i,j,k)
  do k=k1,k2
    do j=j1,j2
!!dir$ prefervector
!!dir$ concurrent
!cdir select(vector)
      do i=i1,i2
          XField(1:di,j-j1+1+(j2-j1+1)*((i-i1)+(k-k1)*(i2-i1+1))) = Field(1:di,abs(ix-i),abs(jx-j),abs(kx-k))
      enddo
    enddo
  enddo
!$OMP END PARALLEL DO
else
!!dir$ concurrent
!$OMP PARALLEL DO SCHEDULE(RUNTIME) PRIVATE(i,j,k)
  do k=k1,k2
    do j=j1,j2
!!dir$ prefervector
!!dir$ concurrent
!cdir select(vector)
      do i=i1,i2
          Field(1:di,abs(ix-i),abs(jx-j),abs(kx-k)) = XField(1:di,j-j1+1+(j2-j1+1)*((i-i1)+(k-k1)*(i2-i1+1)))
      enddo
    enddo
  enddo
!$OMP END PARALLEL DO
endif
  return
    case ('zxy')
  ix=0
  jx=0
  kx=0
  call reorder(MemoryOrder,MemO)
  if(IACHAR(MemO(1:1)) > MaxUpperCase) ix=i2+i1
  if(IACHAR(MemO(2:2)) > MaxUpperCase) jx=j2+j1
  if(IACHAR(MemO(3:3)) > MaxUpperCase) kx=k2+k1
! pjj/cray
  if(IO == 'write') then
!!dir$ concurrent
!$OMP PARALLEL DO SCHEDULE(RUNTIME) PRIVATE(i,j,k)
  do k=k1,k2
    do j=j1,j2
!!dir$ prefervector
!!dir$ concurrent
!cdir select(vector)
      do i=i1,i2
          XField(1:di,k-k1+1+(k2-k1+1)*((i-i1)+(j-j1)*(i2-i1+1))) = Field(1:di,abs(ix-i),abs(jx-j),abs(kx-k))
      enddo
    enddo
  enddo
!$OMP END PARALLEL DO
else
!!dir$ concurrent
!$OMP PARALLEL DO SCHEDULE(RUNTIME) PRIVATE(i,j,k)
  do k=k1,k2
    do j=j1,j2
!!dir$ prefervector
!!dir$ concurrent
!cdir select(vector)
      do i=i1,i2
          Field(1:di,abs(ix-i),abs(jx-j),abs(kx-k)) = XField(1:di,k-k1+1+(k2-k1+1)*((i-i1)+(j-j1)*(i2-i1+1)))
      enddo
    enddo
  enddo
!$OMP END PARALLEL DO
endif
  return
    case ('yzx')
  ix=0
  jx=0
  kx=0
  call reorder(MemoryOrder,MemO)
  if(IACHAR(MemO(1:1)) > MaxUpperCase) ix=i2+i1
  if(IACHAR(MemO(2:2)) > MaxUpperCase) jx=j2+j1
  if(IACHAR(MemO(3:3)) > MaxUpperCase) kx=k2+k1
! pjj/cray
  if(IO == 'write') then
!!dir$ concurrent
!$OMP PARALLEL DO SCHEDULE(RUNTIME) PRIVATE(i,j,k)
  do k=k1,k2
    do j=j1,j2
!!dir$ prefervector
!!dir$ concurrent
!cdir select(vector)
      do i=i1,i2
          XField(1:di,j-j1+1+(j2-j1+1)*((k-k1)+(i-i1)*(k2-k1+1))) = Field(1:di,abs(ix-i),abs(jx-j),abs(kx-k))
      enddo
    enddo
  enddo
!$OMP END PARALLEL DO
else
!!dir$ concurrent
!$OMP PARALLEL DO SCHEDULE(RUNTIME) PRIVATE(i,j,k)
  do k=k1,k2
    do j=j1,j2
!!dir$ prefervector
!!dir$ concurrent
!cdir select(vector)
      do i=i1,i2
          Field(1:di,abs(ix-i),abs(jx-j),abs(kx-k)) = XField(1:di,j-j1+1+(j2-j1+1)*((k-k1)+(i-i1)*(k2-k1+1)))
      enddo
    enddo
  enddo
!$OMP END PARALLEL DO
endif
  return
    case ('zyx')
  ix=0
  jx=0
  kx=0
  call reorder(MemoryOrder,MemO)
  if(IACHAR(MemO(1:1)) > MaxUpperCase) ix=i2+i1
  if(IACHAR(MemO(2:2)) > MaxUpperCase) jx=j2+j1
  if(IACHAR(MemO(3:3)) > MaxUpperCase) kx=k2+k1
! pjj/cray
  if(IO == 'write') then
!!dir$ concurrent
!$OMP PARALLEL DO SCHEDULE(RUNTIME) PRIVATE(i,j,k)
  do k=k1,k2
    do j=j1,j2
!!dir$ prefervector
!!dir$ concurrent
!cdir select(vector)
      do i=i1,i2
          XField(1:di,k-k1+1+(k2-k1+1)*((j-j1)+(i-i1)*(j2-j1+1))) = Field(1:di,abs(ix-i),abs(jx-j),abs(kx-k))
      enddo
    enddo
  enddo
!$OMP END PARALLEL DO
else
!!dir$ concurrent
!$OMP PARALLEL DO SCHEDULE(RUNTIME) PRIVATE(i,j,k)
  do k=k1,k2
    do j=j1,j2
!!dir$ prefervector
!!dir$ concurrent
!cdir select(vector)
      do i=i1,i2
          Field(1:di,abs(ix-i),abs(jx-j),abs(kx-k)) = XField(1:di,k-k1+1+(k2-k1+1)*((j-j1)+(i-i1)*(j2-j1+1)))
      enddo
    enddo
  enddo
!$OMP END PARALLEL DO
endif
  return
    case ('yx')
  ix=0
  jx=0
  kx=0
  call reorder(MemoryOrder,MemO)
  if(IACHAR(MemO(1:1)) > MaxUpperCase) ix=i2+i1
  if(IACHAR(MemO(2:2)) > MaxUpperCase) jx=j2+j1
  if(IACHAR(MemO(3:3)) > MaxUpperCase) kx=k2+k1
! pjj/cray
  if(IO == 'write') then
!!dir$ concurrent
!$OMP PARALLEL DO SCHEDULE(RUNTIME) PRIVATE(i,j,k)
  do k=k1,k2
    do j=j1,j2
!!dir$ prefervector
!!dir$ concurrent
!cdir select(vector)
      do i=i1,i2
          XField(1:di,j-j1+1+(j2-j1+1)*((i-i1)+(k-k1)*(i2-i1+1))) = Field(1:di,abs(ix-i),abs(jx-j),abs(kx-k))
      enddo
    enddo
  enddo
!$OMP END PARALLEL DO
else
!!dir$ concurrent
!$OMP PARALLEL DO SCHEDULE(RUNTIME) PRIVATE(i,j,k)
  do k=k1,k2
    do j=j1,j2
!!dir$ prefervector
!!dir$ concurrent
!cdir select(vector)
      do i=i1,i2
          Field(1:di,abs(ix-i),abs(jx-j),abs(kx-k)) = XField(1:di,j-j1+1+(j2-j1+1)*((i-i1)+(k-k1)*(i2-i1+1)))
      enddo
    enddo
  enddo
!$OMP END PARALLEL DO
endif
  return
  end select
  return
end subroutine Transpose
subroutine reorder (MemoryOrder,MemO)
  character*(*) ,intent(in) :: MemoryOrder
  character*3 ,intent(out) :: MemO
  character*3 :: MemOrd
  integer :: N,i,i1,i2,i3
  MemO = MemoryOrder
  N = len_trim(MemoryOrder)
  if(N == 1) return
  call lowercase(MemoryOrder,MemOrd)
! never invert the boundary codes
  select case ( MemOrd )
     case ( 'xsz','xez','ysz','yez' )
       return
     case default
       continue
  end select
  i1 = 1
  i3 = 1
  do i=2,N
    if(ichar(MemOrd(i:i)) < ichar(MemOrd(i1:i1))) I1 = i
    if(ichar(MemOrd(i:i)) > ichar(MemOrd(i3:i3))) I3 = i
  enddo
  if(N == 2) then
    i2=i3
  else
    i2 = 6-i1-i3
  endif
  MemO(1:1) = MemoryOrder(i1:i1)
  MemO(2:2) = MemoryOrder(i2:i2)
  if(N == 3) MemO(3:3) = MemoryOrder(i3:i3)
  if(MemOrd(i1:i1) == 's' .or. MemOrd(i1:i1) == 'e') then
    MemO(1:N-1) = MemO(2:N)
    MemO(N:N ) = MemoryOrder(i1:i1)
  endif
  return
end subroutine reorder
! Returns .TRUE. iff it is OK to write time-independent domain metadata to the
! file referenced by DataHandle. If DataHandle is invalid, .FALSE. is
! returned.
LOGICAL FUNCTION ncd_ok_to_put_dom_ti( DataHandle )
    USE wrf_data
    include 'wrf_status_codes.h'
    INTEGER, INTENT(IN) :: DataHandle
    CHARACTER*80 :: fname
    INTEGER :: filestate
    INTEGER :: Status
    LOGICAL :: dryrun, first_output, retval
    call ext_ncd_inquire_filename( DataHandle, fname, filestate, Status )
    IF ( Status /= WRF_NO_ERR ) THEN
      write(msg,*) 'Warning Status = ',Status,' in ',"wrf_io.F90", &
                   ', line', 846
      call wrf_debug ( WARN , TRIM(msg) )
      retval = .FALSE.
    ELSE
      dryrun = ( filestate .EQ. WRF_FILE_OPENED_NOT_COMMITTED )
      first_output = ncd_is_first_operation( DataHandle )
      retval = .NOT. dryrun .AND. first_output
    ENDIF
    ncd_ok_to_put_dom_ti = retval
    RETURN
END FUNCTION ncd_ok_to_put_dom_ti
! Returns .TRUE. iff it is OK to read time-independent domain metadata from the
! file referenced by DataHandle. If DataHandle is invalid, .FALSE. is
! returned.
LOGICAL FUNCTION ncd_ok_to_get_dom_ti( DataHandle )
    USE wrf_data
    include 'wrf_status_codes.h'
    INTEGER, INTENT(IN) :: DataHandle
    CHARACTER*80 :: fname
    INTEGER :: filestate
    INTEGER :: Status
    LOGICAL :: dryrun, retval
    call ext_ncd_inquire_filename( DataHandle, fname, filestate, Status )
    IF ( Status /= WRF_NO_ERR ) THEN
      write(msg,*) 'Warning Status = ',Status,' in ',"wrf_io.F90", &
                   ', line', 872
      call wrf_debug ( WARN , TRIM(msg) )
      retval = .FALSE.
    ELSE
      dryrun = ( filestate .EQ. WRF_FILE_OPENED_NOT_COMMITTED )
      retval = .NOT. dryrun
    ENDIF
    ncd_ok_to_get_dom_ti = retval
    RETURN
END FUNCTION ncd_ok_to_get_dom_ti
! Returns .TRUE. iff nothing has been read from or written to the file
! referenced by DataHandle. If DataHandle is invalid, .FALSE. is returned.
LOGICAL FUNCTION ncd_is_first_operation( DataHandle )
    USE wrf_data
    INCLUDE 'wrf_status_codes.h'
    INTEGER, INTENT(IN) :: DataHandle
    TYPE(wrf_data_handle) ,POINTER :: DH
    INTEGER :: Status
    LOGICAL :: retval
    CALL GetDH( DataHandle, DH, Status )
    IF ( Status /= WRF_NO_ERR ) THEN
      write(msg,*) 'Warning Status = ',Status,' in ',"wrf_io.F90", &
                   ', line', 895
      call wrf_debug ( WARN , TRIM(msg) )
      retval = .FALSE.
    ELSE
      retval = DH%first_operation
    ENDIF
    ncd_is_first_operation = retval
    RETURN
END FUNCTION ncd_is_first_operation
subroutine upgrade_filename(FileName)
  implicit none
  character*(*), intent(inout) :: FileName
  integer :: i
  do i = 1, len(trim(FileName))
     if(FileName(i:i) == '-') then
        FileName(i:i) = '_'
     else if(FileName(i:i) == ':') then
         FileName(i:i) = '_'
     endif
  enddo
end subroutine upgrade_filename
end module ext_ncd_support_routines
subroutine TransposeToR4(IO,MemoryOrder,di, Field,l1,l2,m1,m2,n1,n2 &
                                      ,XField,x1,x2,y1,y2,z1,z2 &
                                             ,i1,i2,j1,j2,k1,k2 )
  use ext_ncd_support_routines
  character*(*) ,intent(in) :: IO
  character*(*) ,intent(in) :: MemoryOrder
  integer ,intent(in) :: l1,l2,m1,m2,n1,n2
  integer ,intent(in) :: di
  integer ,intent(in) :: x1,x2,y1,y2,z1,z2
  integer ,intent(in) :: i1,i2,j1,j2,k1,k2
  real*8 ,intent(inout) :: Field(di,l1:l2,m1:m2,n1:n2)
  real*4 ,intent(inout) :: XField(di,(i2-i1+1)*(j2-j1+1)*(k2-k1+1))
  character*3 :: MemOrd
  character*3 :: MemO
  integer ,parameter :: MaxUpperCase=IACHAR('Z')
  integer :: i,j,k,ix,jx,kx
  call LowerCase(MemoryOrder,MemOrd)
  select case (MemOrd)
!#define A-A1+1+(A2-A1+1)*((B-B1)+(C-C1)*(B2-B1+1)) A-A ## 1+1+(A ## 2-A ## 1+1)*((B-B ## 1)+(C-C ## 1)*(B ## 2-B ## 1+1))
! define(`XDEX',($1-``$1''1+1+(``$1''2-``$1''1+1)*(($2-``$2''1)+($3-``$3''1)*(``$2''2-``$2''1+1))))
    case ('xzy')
  ix=0
  jx=0
  kx=0
  call reorder(MemoryOrder,MemO)
  if(IACHAR(MemO(1:1)) > MaxUpperCase) ix=i2+i1
  if(IACHAR(MemO(2:2)) > MaxUpperCase) jx=j2+j1
  if(IACHAR(MemO(3:3)) > MaxUpperCase) kx=k2+k1
! pjj/cray
  if(IO == 'write') then
!!dir$ concurrent
!$OMP PARALLEL DO SCHEDULE(RUNTIME) PRIVATE(i,j,k)
  do k=k1,k2
    do j=j1,j2
!!dir$ prefervector
!!dir$ concurrent
!cdir select(vector)
      do i=i1,i2
          XField(1:di,i-i1+1+(i2-i1+1)*((k-k1)+(j-j1)*(k2-k1+1))) = Field(1:di,abs(ix-i),abs(jx-j),abs(kx-k))
      enddo
    enddo
  enddo
!$OMP END PARALLEL DO
else
!!dir$ concurrent
!$OMP PARALLEL DO SCHEDULE(RUNTIME) PRIVATE(i,j,k)
  do k=k1,k2
    do j=j1,j2
!!dir$ prefervector
!!dir$ concurrent
!cdir select(vector)
      do i=i1,i2
          Field(1:di,abs(ix-i),abs(jx-j),abs(kx-k)) = XField(1:di,i-i1+1+(i2-i1+1)*((k-k1)+(j-j1)*(k2-k1+1)))
      enddo
    enddo
  enddo
!$OMP END PARALLEL DO
endif
  return
    case ('xyz','xsz','xez','ysz','yez','xy','xs','xe','ys','ye','z','c','0')
  ix=0
  jx=0
  kx=0
  call reorder(MemoryOrder,MemO)
  if(IACHAR(MemO(1:1)) > MaxUpperCase) ix=i2+i1
  if(IACHAR(MemO(2:2)) > MaxUpperCase) jx=j2+j1
  if(IACHAR(MemO(3:3)) > MaxUpperCase) kx=k2+k1
! pjj/cray
  if(IO == 'write') then
!!dir$ concurrent
!$OMP PARALLEL DO SCHEDULE(RUNTIME) PRIVATE(i,j,k)
  do k=k1,k2
    do j=j1,j2
!!dir$ prefervector
!!dir$ concurrent
!cdir select(vector)
      do i=i1,i2
          XField(1:di,i-i1+1+(i2-i1+1)*((j-j1)+(k-k1)*(j2-j1+1))) = Field(1:di,abs(ix-i),abs(jx-j),abs(kx-k))
      enddo
    enddo
  enddo
!$OMP END PARALLEL DO
else
!!dir$ concurrent
!$OMP PARALLEL DO SCHEDULE(RUNTIME) PRIVATE(i,j,k)
  do k=k1,k2
    do j=j1,j2
!!dir$ prefervector
!!dir$ concurrent
!cdir select(vector)
      do i=i1,i2
          Field(1:di,abs(ix-i),abs(jx-j),abs(kx-k)) = XField(1:di,i-i1+1+(i2-i1+1)*((j-j1)+(k-k1)*(j2-j1+1)))
      enddo
    enddo
  enddo
!$OMP END PARALLEL DO
endif
  return
    case ('yxz')
  ix=0
  jx=0
  kx=0
  call reorder(MemoryOrder,MemO)
  if(IACHAR(MemO(1:1)) > MaxUpperCase) ix=i2+i1
  if(IACHAR(MemO(2:2)) > MaxUpperCase) jx=j2+j1
  if(IACHAR(MemO(3:3)) > MaxUpperCase) kx=k2+k1
! pjj/cray
  if(IO == 'write') then
!!dir$ concurrent
!$OMP PARALLEL DO SCHEDULE(RUNTIME) PRIVATE(i,j,k)
  do k=k1,k2
    do j=j1,j2
!!dir$ prefervector
!!dir$ concurrent
!cdir select(vector)
      do i=i1,i2
          XField(1:di,j-j1+1+(j2-j1+1)*((i-i1)+(k-k1)*(i2-i1+1))) = Field(1:di,abs(ix-i),abs(jx-j),abs(kx-k))
      enddo
    enddo
  enddo
!$OMP END PARALLEL DO
else
!!dir$ concurrent
!$OMP PARALLEL DO SCHEDULE(RUNTIME) PRIVATE(i,j,k)
  do k=k1,k2
    do j=j1,j2
!!dir$ prefervector
!!dir$ concurrent
!cdir select(vector)
      do i=i1,i2
          Field(1:di,abs(ix-i),abs(jx-j),abs(kx-k)) = XField(1:di,j-j1+1+(j2-j1+1)*((i-i1)+(k-k1)*(i2-i1+1)))
      enddo
    enddo
  enddo
!$OMP END PARALLEL DO
endif
  return
    case ('zxy')
  ix=0
  jx=0
  kx=0
  call reorder(MemoryOrder,MemO)
  if(IACHAR(MemO(1:1)) > MaxUpperCase) ix=i2+i1
  if(IACHAR(MemO(2:2)) > MaxUpperCase) jx=j2+j1
  if(IACHAR(MemO(3:3)) > MaxUpperCase) kx=k2+k1
! pjj/cray
  if(IO == 'write') then
!!dir$ concurrent
!$OMP PARALLEL DO SCHEDULE(RUNTIME) PRIVATE(i,j,k)
  do k=k1,k2
    do j=j1,j2
!!dir$ prefervector
!!dir$ concurrent
!cdir select(vector)
      do i=i1,i2
          XField(1:di,k-k1+1+(k2-k1+1)*((i-i1)+(j-j1)*(i2-i1+1))) = Field(1:di,abs(ix-i),abs(jx-j),abs(kx-k))
      enddo
    enddo
  enddo
!$OMP END PARALLEL DO
else
!!dir$ concurrent
!$OMP PARALLEL DO SCHEDULE(RUNTIME) PRIVATE(i,j,k)
  do k=k1,k2
    do j=j1,j2
!!dir$ prefervector
!!dir$ concurrent
!cdir select(vector)
      do i=i1,i2
          Field(1:di,abs(ix-i),abs(jx-j),abs(kx-k)) = XField(1:di,k-k1+1+(k2-k1+1)*((i-i1)+(j-j1)*(i2-i1+1)))
      enddo
    enddo
  enddo
!$OMP END PARALLEL DO
endif
  return
    case ('yzx')
  ix=0
  jx=0
  kx=0
  call reorder(MemoryOrder,MemO)
  if(IACHAR(MemO(1:1)) > MaxUpperCase) ix=i2+i1
  if(IACHAR(MemO(2:2)) > MaxUpperCase) jx=j2+j1
  if(IACHAR(MemO(3:3)) > MaxUpperCase) kx=k2+k1
! pjj/cray
  if(IO == 'write') then
!!dir$ concurrent
!$OMP PARALLEL DO SCHEDULE(RUNTIME) PRIVATE(i,j,k)
  do k=k1,k2
    do j=j1,j2
!!dir$ prefervector
!!dir$ concurrent
!cdir select(vector)
      do i=i1,i2
          XField(1:di,j-j1+1+(j2-j1+1)*((k-k1)+(i-i1)*(k2-k1+1))) = Field(1:di,abs(ix-i),abs(jx-j),abs(kx-k))
      enddo
    enddo
  enddo
!$OMP END PARALLEL DO
else
!!dir$ concurrent
!$OMP PARALLEL DO SCHEDULE(RUNTIME) PRIVATE(i,j,k)
  do k=k1,k2
    do j=j1,j2
!!dir$ prefervector
!!dir$ concurrent
!cdir select(vector)
      do i=i1,i2
          Field(1:di,abs(ix-i),abs(jx-j),abs(kx-k)) = XField(1:di,j-j1+1+(j2-j1+1)*((k-k1)+(i-i1)*(k2-k1+1)))
      enddo
    enddo
  enddo
!$OMP END PARALLEL DO
endif
  return
    case ('zyx')
  ix=0
  jx=0
  kx=0
  call reorder(MemoryOrder,MemO)
  if(IACHAR(MemO(1:1)) > MaxUpperCase) ix=i2+i1
  if(IACHAR(MemO(2:2)) > MaxUpperCase) jx=j2+j1
  if(IACHAR(MemO(3:3)) > MaxUpperCase) kx=k2+k1
! pjj/cray
  if(IO == 'write') then
!!dir$ concurrent
!$OMP PARALLEL DO SCHEDULE(RUNTIME) PRIVATE(i,j,k)
  do k=k1,k2
    do j=j1,j2
!!dir$ prefervector
!!dir$ concurrent
!cdir select(vector)
      do i=i1,i2
          XField(1:di,k-k1+1+(k2-k1+1)*((j-j1)+(i-i1)*(j2-j1+1))) = Field(1:di,abs(ix-i),abs(jx-j),abs(kx-k))
      enddo
    enddo
  enddo
!$OMP END PARALLEL DO
else
!!dir$ concurrent
!$OMP PARALLEL DO SCHEDULE(RUNTIME) PRIVATE(i,j,k)
  do k=k1,k2
    do j=j1,j2
!!dir$ prefervector
!!dir$ concurrent
!cdir select(vector)
      do i=i1,i2
          Field(1:di,abs(ix-i),abs(jx-j),abs(kx-k)) = XField(1:di,k-k1+1+(k2-k1+1)*((j-j1)+(i-i1)*(j2-j1+1)))
      enddo
    enddo
  enddo
!$OMP END PARALLEL DO
endif
  return
    case ('yx')
  ix=0
  jx=0
  kx=0
  call reorder(MemoryOrder,MemO)
  if(IACHAR(MemO(1:1)) > MaxUpperCase) ix=i2+i1
  if(IACHAR(MemO(2:2)) > MaxUpperCase) jx=j2+j1
  if(IACHAR(MemO(3:3)) > MaxUpperCase) kx=k2+k1
! pjj/cray
  if(IO == 'write') then
!!dir$ concurrent
!$OMP PARALLEL DO SCHEDULE(RUNTIME) PRIVATE(i,j,k)
  do k=k1,k2
    do j=j1,j2
!!dir$ prefervector
!!dir$ concurrent
!cdir select(vector)
      do i=i1,i2
          XField(1:di,j-j1+1+(j2-j1+1)*((i-i1)+(k-k1)*(i2-i1+1))) = Field(1:di,abs(ix-i),abs(jx-j),abs(kx-k))
      enddo
    enddo
  enddo
!$OMP END PARALLEL DO
else
!!dir$ concurrent
!$OMP PARALLEL DO SCHEDULE(RUNTIME) PRIVATE(i,j,k)
  do k=k1,k2
    do j=j1,j2
!!dir$ prefervector
!!dir$ concurrent
!cdir select(vector)
      do i=i1,i2
          Field(1:di,abs(ix-i),abs(jx-j),abs(kx-k)) = XField(1:di,j-j1+1+(j2-j1+1)*((i-i1)+(k-k1)*(i2-i1+1)))
      enddo
    enddo
  enddo
!$OMP END PARALLEL DO
endif
  return
  end select
  return
end subroutine TransposeToR4
subroutine ext_ncd_open_for_read(DatasetName, Comm1, Comm2, SysDepInfo, DataHandle, Status)
  use wrf_data
  use ext_ncd_support_routines
  implicit none
  include 'wrf_status_codes.h'
  include 'netcdf.inc'
  character *(*), INTENT(IN) :: DatasetName
  integer , INTENT(IN) :: Comm1, Comm2
  character *(*), INTENT(IN) :: SysDepInfo
  integer , INTENT(OUT) :: DataHandle
  integer , INTENT(OUT) :: Status
  DataHandle = 0 ! dummy setting to quiet warning message
  CALL ext_ncd_open_for_read_begin( DatasetName, Comm1, Comm2, SysDepInfo, DataHandle, Status )
  IF ( Status .EQ. WRF_NO_ERR ) THEN
    CALL ext_ncd_open_for_read_commit( DataHandle, Status )
  ENDIF
  return
end subroutine ext_ncd_open_for_read
!ends training phase; switches internal flag to enable input
!must be paired with call to ext_ncd_open_for_read_begin
subroutine ext_ncd_open_for_read_commit(DataHandle, Status)
  use wrf_data
  use ext_ncd_support_routines
  implicit none
  include 'wrf_status_codes.h'
  include 'netcdf.inc'
  integer, intent(in) :: DataHandle
  integer, intent(out) :: Status
  type(wrf_data_handle) ,pointer :: DH
  if(WrfIOnotInitialized) then
    Status = WRF_IO_NOT_INITIALIZED
    write(msg,*) 'ext_ncd_ioinit was not called ',"wrf_io.F90",', line', 1013
    call wrf_debug ( FATAL , msg)
    return
  endif
  call GetDH(DataHandle,DH,Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) 'Warning Status = ',Status,' in ',"wrf_io.F90",', line', 1019
    call wrf_debug ( WARN , TRIM(msg))
    return
  endif
  DH%FileStatus = WRF_FILE_OPENED_FOR_READ
  DH%first_operation = .TRUE.
  Status = WRF_NO_ERR
  return
end subroutine ext_ncd_open_for_read_commit
subroutine ext_ncd_open_for_read_begin( FileName, Comm, IOComm, SysDepInfo, DataHandle, Status)
  use wrf_data
  use ext_ncd_support_routines
  implicit none
  include 'wrf_status_codes.h'
  include 'netcdf.inc'
  character*(*) ,intent(INOUT) :: FileName
  integer ,intent(IN) :: Comm
  integer ,intent(IN) :: IOComm
  character*(*) ,intent(in) :: SysDepInfo
  integer ,intent(out) :: DataHandle
  integer ,intent(out) :: Status
  type(wrf_data_handle) ,pointer :: DH
  integer :: XType
  integer :: stat
  integer ,allocatable :: Buffer(:)
  integer :: VarID
  integer :: StoredDim
  integer :: NAtts
  integer :: DimIDs(2)
  integer :: VStart(2)
  integer :: VLen(2)
  integer :: TotalNumVars
  integer :: NumVars
  integer :: i
  character (NF_MAX_NAME) :: Name
  !call upgrade_filename(FileName)
  if(WrfIOnotInitialized) then
    Status = WRF_IO_NOT_INITIALIZED
    write(msg,*) 'ext_ncd_ioinit was not called ',"wrf_io.F90",', line', 1064
    call wrf_debug ( FATAL , msg)
    return
  endif
  call allocHandle(DataHandle,DH,Comm,Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) 'Fatal ALLOCATION ERROR in ',"wrf_io.F90",', line', 1070
    call wrf_debug ( WARN , TRIM(msg))
    return
  endif
  stat = NF_OPEN(FileName, NF_NOWRITE, DH%NCID)
  call netcdf_err(stat,Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) 'NetCDF error in ',"wrf_io.F90",', line', 1078
    call wrf_debug ( WARN , TRIM(msg))
    return
  endif
  stat = NF_INQ_VARID(DH%NCID,DH%TimesName,VarID)
  call netcdf_err(stat,Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) 'NetCDF error in ',"wrf_io.F90",', line', 1085
    call wrf_debug ( WARN , TRIM(msg))
    return
  endif
  stat = NF_INQ_VAR(DH%NCID,VarID,DH%TimesName, XType, StoredDim, DimIDs, NAtts)
  call netcdf_err(stat,Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) 'NetCDF error in ',"wrf_io.F90",', line', 1092
    call wrf_debug ( WARN , TRIM(msg))
    return
  endif
  if(XType/=NF_CHAR) then
    Status = WRF_WARN_TYPE_MISMATCH
    write(msg,*) 'Warning TYPE MISMATCH in ',"wrf_io.F90",', line', 1098
    call wrf_debug ( WARN , TRIM(msg))
    return
  endif
  stat = NF_INQ_DIMLEN(DH%NCID,DimIDs(1),VLen(1))
  call netcdf_err(stat,Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) 'NetCDF error in ',"wrf_io.F90",', line', 1105
    call wrf_debug ( WARN , TRIM(msg))
    return
  endif
  if(VLen(1) /= DateStrLen) then
    Status = WRF_WARN_DATESTR_BAD_LENGTH
    write(msg,*) 'Warning DATESTR BAD LENGTH in ',"wrf_io.F90",', line', 1111
    call wrf_debug ( WARN , TRIM(msg))
    return
  endif
  stat = NF_INQ_DIMLEN(DH%NCID,DimIDs(2),VLen(2))
  call netcdf_err(stat,Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) 'NetCDF error in ',"wrf_io.F90",', line', 1118
    call wrf_debug ( WARN , TRIM(msg))
    return
  endif
  if(VLen(2) > MaxTimes) then
    Status = WRF_ERR_FATAL_TOO_MANY_TIMES
    write(msg,*) 'Fatal TOO MANY TIME VALUES in ',"wrf_io.F90",', line', 1124
    call wrf_debug ( FATAL , TRIM(msg))
    return
  endif
  VStart(1) = 1
  VStart(2) = 1
  stat = NF_GET_VARA_TEXT(DH%NCID,VarID,VStart,VLen,DH%Times)
  call netcdf_err(stat,Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) 'NetCDF error in ',"wrf_io.F90",', line', 1133
    call wrf_debug ( WARN , TRIM(msg))
    return
  endif
  stat = NF_INQ_NVARS(DH%NCID,TotalNumVars)
  call netcdf_err(stat,Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) 'NetCDF error in ',"wrf_io.F90",', line', 1140
    call wrf_debug ( WARN , TRIM(msg))
    return
  endif
  NumVars = 0
  do i=1,TotalNumVars
    stat = NF_INQ_VARNAME(DH%NCID,i,Name)
    call netcdf_err(stat,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) 'NetCDF error in ',"wrf_io.F90",', line', 1149
      call wrf_debug ( WARN , TRIM(msg))
      return
    elseif(Name(1:5) /= 'md___' .and. Name /= DH%TimesName) then
      NumVars = NumVars+1
      DH%VarNames(NumVars) = Name
      DH%VarIDs(NumVars) = i
    endif
  enddo
  DH%NumVars = NumVars
  DH%NumberTimes = VLen(2)
  DH%FileStatus = WRF_FILE_OPENED_NOT_COMMITTED
  DH%FileName = trim(FileName)
  DH%CurrentVariable = 0
  DH%CurrentTime = 0
  DH%TimesVarID = VarID
  DH%TimeIndex = 0
  return
end subroutine ext_ncd_open_for_read_begin
subroutine ext_ncd_open_for_update( FileName, Comm, IOComm, SysDepInfo, DataHandle, Status)
  use wrf_data
  use ext_ncd_support_routines
  implicit none
  include 'wrf_status_codes.h'
  include 'netcdf.inc'
  character*(*) ,intent(INOUT) :: FileName
  integer ,intent(IN) :: Comm
  integer ,intent(IN) :: IOComm
  character*(*) ,intent(in) :: SysDepInfo
  integer ,intent(out) :: DataHandle
  integer ,intent(out) :: Status
  type(wrf_data_handle) ,pointer :: DH
  integer :: XType
  integer :: stat
  integer ,allocatable :: Buffer(:)
  integer :: VarID
  integer :: StoredDim
  integer :: NAtts
  integer :: DimIDs(2)
  integer :: VStart(2)
  integer :: VLen(2)
  integer :: TotalNumVars
  integer :: NumVars
  integer :: i
  character (NF_MAX_NAME) :: Name
  !call upgrade_filename(FileName)
  if(WrfIOnotInitialized) then
    Status = WRF_IO_NOT_INITIALIZED
    write(msg,*) 'ext_ncd_ioinit was not called ',"wrf_io.F90",', line', 1204
    call wrf_debug ( FATAL , msg)
    return
  endif
  call allocHandle(DataHandle,DH,Comm,Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) 'Fatal ALLOCATION ERROR in ',"wrf_io.F90",', line', 1210
    call wrf_debug ( WARN , TRIM(msg))
    return
  endif
  stat = NF_OPEN(FileName, NF_WRITE, DH%NCID)
  call netcdf_err(stat,Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) 'NetCDF error in ',"wrf_io.F90",', line', 1217
    call wrf_debug ( WARN , TRIM(msg))
    return
  endif
  stat = NF_INQ_VARID(DH%NCID,DH%TimesName,VarID)
  call netcdf_err(stat,Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) 'NetCDF error in ',"wrf_io.F90",', line', 1224
    call wrf_debug ( WARN , TRIM(msg))
    return
  endif
  stat = NF_INQ_VAR(DH%NCID,VarID,DH%TimesName, XType, StoredDim, DimIDs, NAtts)
  call netcdf_err(stat,Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) 'NetCDF error in ',"wrf_io.F90",', line', 1231
    call wrf_debug ( WARN , TRIM(msg))
    return
  endif
  if(XType/=NF_CHAR) then
    Status = WRF_WARN_TYPE_MISMATCH
    write(msg,*) 'Warning TYPE MISMATCH in ',"wrf_io.F90",', line', 1237
    call wrf_debug ( WARN , TRIM(msg))
    return
  endif
  stat = NF_INQ_DIMLEN(DH%NCID,DimIDs(1),VLen(1))
  call netcdf_err(stat,Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) 'NetCDF error in ',"wrf_io.F90",', line', 1244
    call wrf_debug ( WARN , TRIM(msg))
    return
  endif
  if(VLen(1) /= DateStrLen) then
    Status = WRF_WARN_DATESTR_BAD_LENGTH
    write(msg,*) 'Warning DATESTR BAD LENGTH in ',"wrf_io.F90",', line', 1250
    call wrf_debug ( WARN , TRIM(msg))
    return
  endif
  stat = NF_INQ_DIMLEN(DH%NCID,DimIDs(2),VLen(2))
  call netcdf_err(stat,Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) 'NetCDF error in ',"wrf_io.F90",', line', 1257
    call wrf_debug ( WARN , TRIM(msg))
    return
  endif
  if(VLen(2) > MaxTimes) then
    Status = WRF_ERR_FATAL_TOO_MANY_TIMES
    write(msg,*) 'Fatal TOO MANY TIME VALUES in ',"wrf_io.F90",', line', 1263
    call wrf_debug ( FATAL , TRIM(msg))
    return
  endif
  VStart(1) = 1
  VStart(2) = 1
  stat = NF_GET_VARA_TEXT(DH%NCID,VarID,VStart,VLen,DH%Times)
  call netcdf_err(stat,Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) 'NetCDF error in ',"wrf_io.F90",', line', 1272
    call wrf_debug ( WARN , TRIM(msg))
    return
  endif
  stat = NF_INQ_NVARS(DH%NCID,TotalNumVars)
  call netcdf_err(stat,Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) 'NetCDF error in ',"wrf_io.F90",', line', 1279
    call wrf_debug ( WARN , TRIM(msg))
    return
  endif
  NumVars = 0
  do i=1,TotalNumVars
    stat = NF_INQ_VARNAME(DH%NCID,i,Name)
    call netcdf_err(stat,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) 'NetCDF error in ',"wrf_io.F90",', line', 1288
      call wrf_debug ( WARN , TRIM(msg))
      return
    elseif(Name(1:5) /= 'md___' .and. Name /= DH%TimesName) then
      NumVars = NumVars+1
      DH%VarNames(NumVars) = Name
      DH%VarIDs(NumVars) = i
    endif
  enddo
  DH%NumVars = NumVars
  DH%NumberTimes = VLen(2)
  DH%FileStatus = WRF_FILE_OPENED_FOR_UPDATE
  DH%FileName = trim(FileName)
  DH%CurrentVariable = 0
  DH%CurrentTime = 0
  DH%TimesVarID = VarID
  DH%TimeIndex = 0
  return
end subroutine ext_ncd_open_for_update
SUBROUTINE ext_ncd_open_for_write_begin(FileName,Comm,IOComm,SysDepInfo,DataHandle,Status)
  use wrf_data
  use ext_ncd_support_routines
  implicit none
  include 'wrf_status_codes.h'
  include 'netcdf.inc'
  character*(*) ,intent(inout) :: FileName
  integer ,intent(in) :: Comm
  integer ,intent(in) :: IOComm
  character*(*) ,intent(in) :: SysDepInfo
  integer ,intent(out) :: DataHandle
  integer ,intent(out) :: Status
  type(wrf_data_handle),pointer :: DH
  integer :: i
  integer :: stat
  character (7) :: Buffer
  integer :: VDimIDs(2)
  !call upgrade_filename(FileName)
  if(WrfIOnotInitialized) then
    Status = WRF_IO_NOT_INITIALIZED
    write(msg,*) 'ext_ncd_open_for_write_begin: ext_ncd_ioinit was not called ',"wrf_io.F90",', line', 1338
    call wrf_debug ( FATAL , msg)
    return
  endif
  call allocHandle(DataHandle,DH,Comm,Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) 'Fatal ALLOCATION ERROR in ext_ncd_open_for_write_begin ',"wrf_io.F90",', line', 1344
    call wrf_debug ( FATAL , TRIM(msg))
    return
  endif
  DH%TimeIndex = 0
  DH%Times = ZeroDate
  stat = NF_CREATE(FileName, IOR(NF_CLOBBER,NF_64BIT_OFFSET), DH%NCID)
  call netcdf_err(stat,Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) 'NetCDF error in ext_ncd_open_for_write_begin ',"wrf_io.F90",', line', 1374
    call wrf_debug ( WARN , TRIM(msg))
    return
  endif
  DH%FileStatus = WRF_FILE_OPENED_NOT_COMMITTED
  DH%FileName = trim(FileName)
  stat = NF_DEF_DIM(DH%NCID,DH%DimUnlimName,NF_UNLIMITED,DH%DimUnlimID)
  call netcdf_err(stat,Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) 'NetCDF error in ext_ncd_open_for_write_begin ',"wrf_io.F90",', line', 1383
    call wrf_debug ( WARN , TRIM(msg))
    return
  endif
  DH%VarNames (1:MaxVars) = NO_NAME
  DH%MDVarNames(1:MaxVars) = NO_NAME
  do i=1,MaxDims
    write(Buffer,FMT="('DIM',i4.4)") i
    DH%DimNames (i) = Buffer
    DH%DimLengths(i) = NO_DIM
  enddo
  DH%DimNames(1) = 'DateStrLen'
  stat = NF_DEF_DIM(DH%NCID,DH%DimNames(1),DateStrLen,DH%DimIDs(1))
  call netcdf_err(stat,Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) 'NetCDF error in ext_ncd_open_for_write_begin ',"wrf_io.F90",', line', 1398
    call wrf_debug ( WARN , TRIM(msg))
    return
  endif
  VDimIDs(1) = DH%DimIDs(1)
  VDimIDs(2) = DH%DimUnlimID
  stat = NF_DEF_VAR(DH%NCID,DH%TimesName,NF_CHAR,2,VDimIDs,DH%TimesVarID)
  call netcdf_err(stat,Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) 'NetCDF error in ext_ncd_open_for_write_begin ',"wrf_io.F90",', line', 1407
    call wrf_debug ( WARN , TRIM(msg))
    return
  endif
  DH%DimLengths(1) = DateStrLen
  if (index(SysDepInfo,'REAL_OUTPUT_SIZE=4') /= 0) then
     DH%R4OnOutput = .true.
  end if
!toggle on nofill mode
  if (index(SysDepInfo,'NOFILL=.TRUE.') /= 0) then
     DH%nofill = .true.
  end if
  return
end subroutine ext_ncd_open_for_write_begin
!stub
!opens a file for writing or coupler datastream for sending messages.
!no training phase for this version of the open stmt.
subroutine ext_ncd_open_for_write (DatasetName, Comm1, Comm2, &
                                   SysDepInfo, DataHandle, Status)
  use wrf_data
  use ext_ncd_support_routines
  implicit none
  include 'wrf_status_codes.h'
  include 'netcdf.inc'
  character *(*), intent(in) ::DatasetName
  integer , intent(in) ::Comm1, Comm2
  character *(*), intent(in) ::SysDepInfo
  integer , intent(out) :: DataHandle
  integer , intent(out) :: Status
  Status=WRF_WARN_NOOP
  DataHandle = 0 ! dummy setting to quiet warning message
  return
end subroutine ext_ncd_open_for_write
SUBROUTINE ext_ncd_open_for_write_commit(DataHandle, Status)
  use wrf_data
  use ext_ncd_support_routines
  implicit none
  include 'wrf_status_codes.h'
  include 'netcdf.inc'
  integer ,intent(in) :: DataHandle
  integer ,intent(out) :: Status
  type(wrf_data_handle),pointer :: DH
  integer :: i
  integer :: stat
  integer :: oldmode ! for nf_set_fill, not used
  if(WrfIOnotInitialized) then
    Status = WRF_IO_NOT_INITIALIZED
    write(msg,*) 'ext_ncd_open_for_write_commit: ext_ncd_ioinit was not called ',"wrf_io.F90",', line', 1459
    call wrf_debug ( FATAL , msg)
    return
  endif
  call GetDH(DataHandle,DH,Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) 'Warning Status = ',Status,' in ext_ncd_open_for_write_commit ',"wrf_io.F90",', line', 1465
    call wrf_debug ( WARN , TRIM(msg))
    return
  endif
  if ( DH%nofill ) then
    Status = NF_SET_FILL(DH%NCID,NF_NOFILL, oldmode )
    if(Status /= WRF_NO_ERR) then
      write(msg,*) 'Warning Status = ',Status,' from NF_SET_FILL ',"wrf_io.F90",', line', 1472
      call wrf_debug ( WARN , TRIM(msg))
      return
    endif
    write(msg,*) 'Information: NOFILL being set for writing to ',TRIM(DH%FileName)
    call wrf_debug ( WARN , TRIM(msg))
  endif
  stat = NF_ENDDEF(DH%NCID)
  call netcdf_err(stat,Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) 'NetCDF error in ext_ncd_open_for_write_commit ',"wrf_io.F90",', line', 1482
    call wrf_debug ( WARN , TRIM(msg))
    return
  endif
  DH%FileStatus = WRF_FILE_OPENED_FOR_WRITE
  DH%first_operation = .TRUE.
  return
end subroutine ext_ncd_open_for_write_commit
subroutine ext_ncd_ioclose(DataHandle, Status)
  use wrf_data
  use ext_ncd_support_routines
  implicit none
  include 'wrf_status_codes.h'
  include 'netcdf.inc'
  integer ,intent(in) :: DataHandle
  integer ,intent(out) :: Status
  type(wrf_data_handle),pointer :: DH
  integer :: stat
  call GetDH(DataHandle,DH,Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) 'Warning Status = ',Status,' in ext_ncd_ioclose ',"wrf_io.F90",', line', 1504
    call wrf_debug ( WARN , TRIM(msg))
    return
  endif
  if(DH%FileStatus == WRF_FILE_NOT_OPENED) then
    Status = WRF_WARN_FILE_NOT_OPENED
    write(msg,*) 'Warning FILE NOT OPENED in ext_ncd_ioclose ',"wrf_io.F90",', line', 1510
    call wrf_debug ( WARN , TRIM(msg))
  elseif(DH%FileStatus == WRF_FILE_OPENED_NOT_COMMITTED) then
    Status = WRF_WARN_DRYRUN_CLOSE
    write(msg,*) 'Warning TRY TO CLOSE DRYRUN in ext_ncd_ioclose ',"wrf_io.F90",', line', 1514
    call wrf_debug ( WARN , TRIM(msg))
  elseif(DH%FileStatus == WRF_FILE_OPENED_FOR_WRITE) then
    continue
  elseif(DH%FileStatus == WRF_FILE_OPENED_FOR_READ) then
    continue
  elseif(DH%FileStatus == WRF_FILE_OPENED_FOR_UPDATE) then
    continue
  else
    Status = WRF_ERR_FATAL_BAD_FILE_STATUS
    write(msg,*) 'Fatal error BAD FILE STATUS in ext_ncd_ioclose ',"wrf_io.F90",', line', 1524
    call wrf_debug ( FATAL , TRIM(msg))
    return
  endif
  stat = NF_CLOSE(DH%NCID)
  call netcdf_err(stat,Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) 'NetCDF error in ext_ncd_ioclose ',"wrf_io.F90",', line', 1532
    call wrf_debug ( WARN , TRIM(msg))
    return
  endif
  CALL deallocHandle( DataHandle, Status )
  DH%Free=.true.
  return
end subroutine ext_ncd_ioclose
subroutine ext_ncd_iosync( DataHandle, Status)
  use wrf_data
  use ext_ncd_support_routines
  implicit none
  include 'wrf_status_codes.h'
  include 'netcdf.inc'
  integer ,intent(in) :: DataHandle
  integer ,intent(out) :: Status
  type(wrf_data_handle),pointer :: DH
  integer :: stat
  call GetDH(DataHandle,DH,Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) 'Warning Status = ',Status,' in ext_ncd_iosync ',"wrf_io.F90",', line', 1554
    call wrf_debug ( WARN , TRIM(msg))
    return
  endif
  if(DH%FileStatus == WRF_FILE_NOT_OPENED) then
    Status = WRF_WARN_FILE_NOT_OPENED
    write(msg,*) 'Warning FILE NOT OPENED in ext_ncd_iosync ',"wrf_io.F90",', line', 1560
    call wrf_debug ( WARN , TRIM(msg))
  elseif(DH%FileStatus == WRF_FILE_OPENED_NOT_COMMITTED) then
    Status = WRF_WARN_FILE_NOT_COMMITTED
    write(msg,*) 'Warning FILE NOT COMMITTED in ext_ncd_iosync ',"wrf_io.F90",', line', 1564
    call wrf_debug ( WARN , TRIM(msg))
  elseif(DH%FileStatus == WRF_FILE_OPENED_FOR_WRITE) then
    continue
  elseif(DH%FileStatus == WRF_FILE_OPENED_FOR_READ) then
    continue
  else
    Status = WRF_ERR_FATAL_BAD_FILE_STATUS
    write(msg,*) 'Fatal error BAD FILE STATUS in ext_ncd_iosync ',"wrf_io.F90",', line', 1572
    call wrf_debug ( FATAL , TRIM(msg))
    return
  endif
  stat = NF_SYNC(DH%NCID)
  call netcdf_err(stat,Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) 'NetCDF error in ext_ncd_iosync ',"wrf_io.F90",', line', 1579
    call wrf_debug ( WARN , TRIM(msg))
    return
  endif
  return
end subroutine ext_ncd_iosync
subroutine ext_ncd_redef( DataHandle, Status)
  use wrf_data
  use ext_ncd_support_routines
  implicit none
  include 'wrf_status_codes.h'
  include 'netcdf.inc'
  integer ,intent(in) :: DataHandle
  integer ,intent(out) :: Status
  type(wrf_data_handle),pointer :: DH
  integer :: stat
  call GetDH(DataHandle,DH,Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) 'Warning Status = ',Status,' in ',"wrf_io.F90",', line', 1601
    call wrf_debug ( WARN , TRIM(msg))
    return
  endif
  if(DH%FileStatus == WRF_FILE_NOT_OPENED) then
    Status = WRF_WARN_FILE_NOT_OPENED
    write(msg,*) 'Warning FILE NOT OPENED in ',"wrf_io.F90",', line', 1607
    call wrf_debug ( WARN , TRIM(msg))
  elseif(DH%FileStatus == WRF_FILE_OPENED_NOT_COMMITTED) then
    Status = WRF_WARN_FILE_NOT_COMMITTED
    write(msg,*) 'Warning FILE NOT COMMITTED in ',"wrf_io.F90",', line', 1611
    call wrf_debug ( WARN , TRIM(msg))
  elseif(DH%FileStatus == WRF_FILE_OPENED_FOR_WRITE) then
    continue
  elseif(DH%FileStatus == WRF_FILE_OPENED_FOR_UPDATE) then
    continue
  elseif(DH%FileStatus == WRF_FILE_OPENED_FOR_READ) then
    Status = WRF_WARN_FILE_OPEN_FOR_READ
    write(msg,*) 'Warning FILE OPEN FOR READ in ',"wrf_io.F90",', line', 1619
    call wrf_debug ( WARN , TRIM(msg))
  else
    Status = WRF_ERR_FATAL_BAD_FILE_STATUS
    write(msg,*) 'Fatal error BAD FILE STATUS in ',"wrf_io.F90",', line', 1623
    call wrf_debug ( FATAL , TRIM(msg))
    return
  endif
  stat = NF_REDEF(DH%NCID)
  call netcdf_err(stat,Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) 'NetCDF error in ',"wrf_io.F90",', line', 1630
    call wrf_debug ( WARN , TRIM(msg))
    return
  endif
  DH%FileStatus = WRF_FILE_OPENED_NOT_COMMITTED
  return
end subroutine ext_ncd_redef
subroutine ext_ncd_enddef( DataHandle, Status)
  use wrf_data
  use ext_ncd_support_routines
  implicit none
  include 'wrf_status_codes.h'
  include 'netcdf.inc'
  integer ,intent(in) :: DataHandle
  integer ,intent(out) :: Status
  type(wrf_data_handle),pointer :: DH
  integer :: stat
  call GetDH(DataHandle,DH,Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) 'Warning Status = ',Status,' in ',"wrf_io.F90",', line', 1651
    call wrf_debug ( WARN , TRIM(msg))
    return
  endif
  if(DH%FileStatus == WRF_FILE_NOT_OPENED) then
    Status = WRF_WARN_FILE_NOT_OPENED
    write(msg,*) 'Warning FILE NOT OPENED in ',"wrf_io.F90",', line', 1657
    call wrf_debug ( WARN , TRIM(msg))
  elseif(DH%FileStatus == WRF_FILE_OPENED_NOT_COMMITTED) then
    Status = WRF_WARN_FILE_NOT_COMMITTED
    write(msg,*) 'Warning FILE NOT COMMITTED in ',"wrf_io.F90",', line', 1661
    call wrf_debug ( WARN , TRIM(msg))
  elseif(DH%FileStatus == WRF_FILE_OPENED_FOR_WRITE) then
    continue
  elseif(DH%FileStatus == WRF_FILE_OPENED_FOR_READ) then
    Status = WRF_WARN_FILE_OPEN_FOR_READ
    write(msg,*) 'Warning FILE OPEN FOR READ in ',"wrf_io.F90",', line', 1667
    call wrf_debug ( WARN , TRIM(msg))
  else
    Status = WRF_ERR_FATAL_BAD_FILE_STATUS
    write(msg,*) 'Fatal error BAD FILE STATUS in ',"wrf_io.F90",', line', 1671
    call wrf_debug ( FATAL , TRIM(msg))
    return
  endif
  stat = NF_ENDDEF(DH%NCID)
  call netcdf_err(stat,Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) 'NetCDF error in ',"wrf_io.F90",', line', 1678
    call wrf_debug ( WARN , TRIM(msg))
    return
  endif
  DH%FileStatus = WRF_FILE_OPENED_FOR_WRITE
  return
end subroutine ext_ncd_enddef
subroutine ext_ncd_ioinit(SysDepInfo, Status)
  use wrf_data
  implicit none
  include 'wrf_status_codes.h'
  CHARACTER*(*), INTENT(IN) :: SysDepInfo
  INTEGER ,INTENT(INOUT) :: Status
  WrfIOnotInitialized = .false.
  WrfDataHandles(1:WrfDataHandleMax)%Free = .true.
  WrfDataHandles(1:WrfDataHandleMax)%TimesName = 'Times'
  WrfDataHandles(1:WrfDataHandleMax)%DimUnlimName = 'Time'
  WrfDataHandles(1:WrfDataHandleMax)%FileStatus = WRF_FILE_NOT_OPENED
  if(trim(SysDepInfo) == "use_netcdf_classic" ) then
     WrfDataHandles(1:WrfDataHandleMax)%use_netcdf_classic = .true.
  else
     WrfDataHandles(1:WrfDataHandleMax)%use_netcdf_classic = .false.
  endif
  Status = WRF_NO_ERR
  return
end subroutine ext_ncd_ioinit
subroutine ext_ncd_inquiry (Inquiry, Result, Status)
  use wrf_data
  implicit none
  include 'wrf_status_codes.h'
  character *(*), INTENT(IN) :: Inquiry
  character *(*), INTENT(OUT) :: Result
  integer ,INTENT(INOUT) :: Status
  SELECT CASE (Inquiry)
  CASE ("RANDOM_WRITE","RANDOM_READ","SEQUENTIAL_WRITE","SEQUENTIAL_READ")
        Result='ALLOW'
  CASE ("OPEN_READ","OPEN_COMMIT_WRITE")
        Result='REQUIRE'
  CASE ("OPEN_WRITE","OPEN_COMMIT_READ","PARALLEL_IO")
        Result='NO'
  CASE ("SELF_DESCRIBING","SUPPORT_METADATA","SUPPORT_3D_FIELDS")
        Result='YES'
  CASE ("MEDIUM")
        Result ='FILE'
  CASE DEFAULT
      Result = 'No Result for that inquiry!'
  END SELECT
  Status=WRF_NO_ERR
  return
end subroutine ext_ncd_inquiry
subroutine ext_ncd_ioexit(Status)
  use wrf_data
  use ext_ncd_support_routines
  implicit none
  include 'wrf_status_codes.h'
  include 'netcdf.inc'
  integer , INTENT(INOUT) ::Status
  integer :: error
  type(wrf_data_handle),pointer :: DH
  integer :: i
  integer :: stat
  if(WrfIOnotInitialized) then
    Status = WRF_IO_NOT_INITIALIZED
    write(msg,*) 'ext_ncd_ioinit was not called ',"wrf_io.F90",', line', 1749
    call wrf_debug ( FATAL , msg)
    return
  endif
  do i=1,WrfDataHandleMax
    CALL deallocHandle( i , stat )
  enddo
  return
end subroutine ext_ncd_ioexit
subroutine ext_ncd_get_dom_ti_real(DataHandle,Element,Data,Count,OutCount,Status)
!*------------------------------------------------------------------------------
!* Standard Disclaimer
!*
!* Forecast Systems Laboratory
!* NOAA/OAR/ERL/FSL
!* 325 Broadway
!* Boulder, CO 80303
!*
!* AVIATION DIVISION
!* ADVANCED COMPUTING BRANCH
!* SMS/NNT Version: 2.0.0
!*
!* This software and its documentation are in the public domain and
!* are furnished "as is". The United States government, its
!* instrumentalities, officers, employees, and agents make no
!* warranty, express or implied, as to the usefulness of the software
!* and documentation for any purpose. They assume no
!* responsibility (1) for the use of the software and documentation;
!* or (2) to provide technical support to users.
!*
!* Permission to use, copy, modify, and distribute this software is
!* hereby granted, provided that this disclaimer notice appears in
!* all copies. All modifications to this software must be clearly
!* documented, and are solely the responsibility of the agent making
!* the modification. If significant modifications or enhancements
!* are made to this software, the SMS Development team
!* (sms-info@fsl.noaa.gov) should be notified.
!*
!*----------------------------------------------------------------------------
!*
!* WRF NetCDF I/O
! Author: Jacques Middlecoff jacquesm@fsl.noaa.gov
!* Date: October 6, 2000
!*
!*----------------------------------------------------------------------------
  use wrf_data
  use ext_ncd_support_routines
  implicit none
  include 'wrf_status_codes.h'
  include 'netcdf.inc'
  integer ,intent(in) :: DataHandle
  character*(*) ,intent(in) :: Element
  real,intent(out) :: Data(*)
  integer,intent(in) :: Count
  integer,intent(out) :: OutCOunt
  integer ,intent(out) :: Status
  type(wrf_data_handle) ,pointer :: DH
  integer :: XType
  integer :: Len
  integer :: stat
  real,allocatable :: Buffer(:)
  call GetDH(DataHandle,DH,Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) &
'Warning Status = ',Status,' in ',"ext_ncd_get_dom_ti.code",' ','REAL',', line', 57
    call wrf_debug ( WARN , msg)
    return
  endif
! Do nothing unless it is time to read time-independent domain metadata.
IF ( ncd_ok_to_get_dom_ti( DataHandle ) ) THEN
  if(DH%FileStatus == WRF_FILE_NOT_OPENED) then
    Status = WRF_WARN_FILE_NOT_OPENED
    write(msg,*) &
'Warning FILE NOT OPENED in ',"ext_ncd_get_dom_ti.code",' ','REAL',', line', 66
    call wrf_debug ( WARN , msg)
  elseif(DH%FileStatus == WRF_FILE_OPENED_NOT_COMMITTED) then
    Status = WRF_WARN_DRYRUN_READ
    write(msg,*) &
'Warning DRYRUN READ in ',"ext_ncd_get_dom_ti.code",' ','REAL',', line', 71
    call wrf_debug ( WARN , msg)
  elseif(DH%FileStatus == WRF_FILE_OPENED_FOR_WRITE) then
    Status = WRF_WARN_READ_WONLY_FILE
    write(msg,*) &
'Warning READ WRITE ONLY FILE in ',"ext_ncd_get_dom_ti.code",' ','REAL',', line', 76
    call wrf_debug ( WARN , msg)
  elseif(DH%FileStatus == WRF_FILE_OPENED_FOR_READ) then
    stat = NF_INQ_ATT(DH%NCID,NF_GLOBAL,Element, XType, Len)
    call netcdf_err(stat,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) &
'NetCDF error in ',"ext_ncd_get_dom_ti.code",' ','REAL',', line', 83,' Element ',Element
      call wrf_debug ( WARN , msg)
      return
    endif
    if ( NF_FLOAT == NF_DOUBLE .OR. NF_FLOAT == NF_FLOAT ) then
      if( .NOT. ( XType==NF_FLOAT .OR. XType==NF_DOUBLE) ) then
        Status = WRF_WARN_TYPE_MISMATCH
        write(msg,*) &
'Warning TYPE MISMATCH in ',"ext_ncd_get_dom_ti.code",' ','REAL',', line', 91
        call wrf_debug ( WARN , msg)
        return
      endif
    else
      if( XType/=NF_FLOAT) then
        Status = WRF_WARN_TYPE_MISMATCH
        write(msg,*) &
'Warning TYPE MISMATCH in ',"ext_ncd_get_dom_ti.code",' ','REAL',', line', 99
        call wrf_debug ( WARN , msg)
        return
      endif
    endif
    if(Len<=0) then
      Status = WRF_WARN_LENGTH_LESS_THAN_1
      write(msg,*) &
'Warning LENGTH < 1 in ',"ext_ncd_get_dom_ti.code",' ','REAL',', line', 107
      call wrf_debug ( WARN , msg)
      return
    endif
    allocate(Buffer(Len), STAT=stat)
    if(stat/= 0) then
      Status = WRF_ERR_FATAL_ALLOCATION_ERROR
      write(msg,*) &
'Fatal ALLOCATION ERROR in ',"ext_ncd_get_dom_ti.code",' ','REAL',', line', 116
      call wrf_debug ( FATAL , msg)
      return
    endif
    stat = NF_GET_ATT_REAL (DH%NCID,NF_GLOBAL,Element,Buffer)
    call netcdf_err(stat,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) &
'NetCDF error in ',"ext_ncd_get_dom_ti.code",' ','REAL',', line', 128,' Element ',Element
      call wrf_debug ( WARN , msg)
      return
    endif
    Data(1:min(Len,Count)) = Buffer(1:min(Len,Count))
    deallocate(Buffer, STAT=stat)
    if(stat/= WRF_NO_ERR) then
      Status = WRF_ERR_FATAL_DEALLOCATION_ERR
      write(msg,*) &
'Fatal DEALLOCATION ERROR in ',"ext_ncd_get_dom_ti.code",' ','REAL',', line', 138
      call wrf_debug ( FATAL , msg)
      return
    endif
    if(Len > Count) then
      OutCount = Count
      Status = WRF_WARN_MORE_DATA_IN_FILE
    else
      OutCount = Len
      Status = WRF_NO_ERR
    endif
  else
    Status = WRF_ERR_FATAL_BAD_FILE_STATUS
    write(msg,*) &
'Fatal error BAD FILE STATUS in ',"ext_ncd_get_dom_ti.code",' ','REAL',', line', 153
    call wrf_debug ( FATAL , msg)
  endif
ENDIF
  return
end subroutine ext_ncd_get_dom_ti_real
subroutine ext_ncd_get_dom_ti_integer(DataHandle,Element,Data,Count,OutCount,Status)
!*------------------------------------------------------------------------------
!* Standard Disclaimer
!*
!* Forecast Systems Laboratory
!* NOAA/OAR/ERL/FSL
!* 325 Broadway
!* Boulder, CO 80303
!*
!* AVIATION DIVISION
!* ADVANCED COMPUTING BRANCH
!* SMS/NNT Version: 2.0.0
!*
!* This software and its documentation are in the public domain and
!* are furnished "as is". The United States government, its
!* instrumentalities, officers, employees, and agents make no
!* warranty, express or implied, as to the usefulness of the software
!* and documentation for any purpose. They assume no
!* responsibility (1) for the use of the software and documentation;
!* or (2) to provide technical support to users.
!*
!* Permission to use, copy, modify, and distribute this software is
!* hereby granted, provided that this disclaimer notice appears in
!* all copies. All modifications to this software must be clearly
!* documented, and are solely the responsibility of the agent making
!* the modification. If significant modifications or enhancements
!* are made to this software, the SMS Development team
!* (sms-info@fsl.noaa.gov) should be notified.
!*
!*----------------------------------------------------------------------------
!*
!* WRF NetCDF I/O
! Author: Jacques Middlecoff jacquesm@fsl.noaa.gov
!* Date: October 6, 2000
!*
!*----------------------------------------------------------------------------
  use wrf_data
  use ext_ncd_support_routines
  implicit none
  include 'wrf_status_codes.h'
  include 'netcdf.inc'
  integer ,intent(in) :: DataHandle
  character*(*) ,intent(in) :: Element
  integer,intent(out) :: Data(*)
  integer,intent(in) :: Count
  integer,intent(out) :: OutCOunt
  integer ,intent(out) :: Status
  type(wrf_data_handle) ,pointer :: DH
  integer :: XType
  integer :: Len
  integer :: stat
  integer,allocatable :: Buffer(:)
  call GetDH(DataHandle,DH,Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) &
'Warning Status = ',Status,' in ',"ext_ncd_get_dom_ti.code",' ','INTEGER',', line', 57
    call wrf_debug ( WARN , msg)
    return
  endif
! Do nothing unless it is time to read time-independent domain metadata.
IF ( ncd_ok_to_get_dom_ti( DataHandle ) ) THEN
  if(DH%FileStatus == WRF_FILE_NOT_OPENED) then
    Status = WRF_WARN_FILE_NOT_OPENED
    write(msg,*) &
'Warning FILE NOT OPENED in ',"ext_ncd_get_dom_ti.code",' ','INTEGER',', line', 66
    call wrf_debug ( WARN , msg)
  elseif(DH%FileStatus == WRF_FILE_OPENED_NOT_COMMITTED) then
    Status = WRF_WARN_DRYRUN_READ
    write(msg,*) &
'Warning DRYRUN READ in ',"ext_ncd_get_dom_ti.code",' ','INTEGER',', line', 71
    call wrf_debug ( WARN , msg)
  elseif(DH%FileStatus == WRF_FILE_OPENED_FOR_WRITE) then
    Status = WRF_WARN_READ_WONLY_FILE
    write(msg,*) &
'Warning READ WRITE ONLY FILE in ',"ext_ncd_get_dom_ti.code",' ','INTEGER',', line', 76
    call wrf_debug ( WARN , msg)
  elseif(DH%FileStatus == WRF_FILE_OPENED_FOR_READ) then
    stat = NF_INQ_ATT(DH%NCID,NF_GLOBAL,Element, XType, Len)
    call netcdf_err(stat,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) &
'NetCDF error in ',"ext_ncd_get_dom_ti.code",' ','INTEGER',', line', 83,' Element ',Element
      call wrf_debug ( WARN , msg)
      return
    endif
    if ( NF_INT == NF_DOUBLE .OR. NF_INT == NF_FLOAT ) then
      if( .NOT. ( XType==NF_FLOAT .OR. XType==NF_DOUBLE) ) then
        Status = WRF_WARN_TYPE_MISMATCH
        write(msg,*) &
'Warning TYPE MISMATCH in ',"ext_ncd_get_dom_ti.code",' ','INTEGER',', line', 91
        call wrf_debug ( WARN , msg)
        return
      endif
    else
      if( XType/=NF_INT) then
        Status = WRF_WARN_TYPE_MISMATCH
        write(msg,*) &
'Warning TYPE MISMATCH in ',"ext_ncd_get_dom_ti.code",' ','INTEGER',', line', 99
        call wrf_debug ( WARN , msg)
        return
      endif
    endif
    if(Len<=0) then
      Status = WRF_WARN_LENGTH_LESS_THAN_1
      write(msg,*) &
'Warning LENGTH < 1 in ',"ext_ncd_get_dom_ti.code",' ','INTEGER',', line', 107
      call wrf_debug ( WARN , msg)
      return
    endif
    allocate(Buffer(Len), STAT=stat)
    if(stat/= 0) then
      Status = WRF_ERR_FATAL_ALLOCATION_ERROR
      write(msg,*) &
'Fatal ALLOCATION ERROR in ',"ext_ncd_get_dom_ti.code",' ','INTEGER',', line', 116
      call wrf_debug ( FATAL , msg)
      return
    endif
    stat = NF_GET_ATT_INT (DH%NCID,NF_GLOBAL,Element,Buffer)
    call netcdf_err(stat,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) &
'NetCDF error in ',"ext_ncd_get_dom_ti.code",' ','INTEGER',', line', 128,' Element ',Element
      call wrf_debug ( WARN , msg)
      return
    endif
    Data(1:min(Len,Count)) = Buffer(1:min(Len,Count))
    deallocate(Buffer, STAT=stat)
    if(stat/= WRF_NO_ERR) then
      Status = WRF_ERR_FATAL_DEALLOCATION_ERR
      write(msg,*) &
'Fatal DEALLOCATION ERROR in ',"ext_ncd_get_dom_ti.code",' ','INTEGER',', line', 138
      call wrf_debug ( FATAL , msg)
      return
    endif
    if(Len > Count) then
      OutCount = Count
      Status = WRF_WARN_MORE_DATA_IN_FILE
    else
      OutCount = Len
      Status = WRF_NO_ERR
    endif
  else
    Status = WRF_ERR_FATAL_BAD_FILE_STATUS
    write(msg,*) &
'Fatal error BAD FILE STATUS in ',"ext_ncd_get_dom_ti.code",' ','INTEGER',', line', 153
    call wrf_debug ( FATAL , msg)
  endif
ENDIF
  return
end subroutine ext_ncd_get_dom_ti_integer
subroutine ext_ncd_get_dom_ti_double(DataHandle,Element,Data,Count,OutCount,Status)
!*------------------------------------------------------------------------------
!* Standard Disclaimer
!*
!* Forecast Systems Laboratory
!* NOAA/OAR/ERL/FSL
!* 325 Broadway
!* Boulder, CO 80303
!*
!* AVIATION DIVISION
!* ADVANCED COMPUTING BRANCH
!* SMS/NNT Version: 2.0.0
!*
!* This software and its documentation are in the public domain and
!* are furnished "as is". The United States government, its
!* instrumentalities, officers, employees, and agents make no
!* warranty, express or implied, as to the usefulness of the software
!* and documentation for any purpose. They assume no
!* responsibility (1) for the use of the software and documentation;
!* or (2) to provide technical support to users.
!*
!* Permission to use, copy, modify, and distribute this software is
!* hereby granted, provided that this disclaimer notice appears in
!* all copies. All modifications to this software must be clearly
!* documented, and are solely the responsibility of the agent making
!* the modification. If significant modifications or enhancements
!* are made to this software, the SMS Development team
!* (sms-info@fsl.noaa.gov) should be notified.
!*
!*----------------------------------------------------------------------------
!*
!* WRF NetCDF I/O
! Author: Jacques Middlecoff jacquesm@fsl.noaa.gov
!* Date: October 6, 2000
!*
!*----------------------------------------------------------------------------
  use wrf_data
  use ext_ncd_support_routines
  implicit none
  include 'wrf_status_codes.h'
  include 'netcdf.inc'
  integer ,intent(in) :: DataHandle
  character*(*) ,intent(in) :: Element
  real*8,intent(out) :: Data(*)
  integer,intent(in) :: Count
  integer,intent(out) :: OutCOunt
  integer ,intent(out) :: Status
  type(wrf_data_handle) ,pointer :: DH
  integer :: XType
  integer :: Len
  integer :: stat
  real*8,allocatable :: Buffer(:)
  call GetDH(DataHandle,DH,Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) &
'Warning Status = ',Status,' in ',"ext_ncd_get_dom_ti.code",' ','DOUBLE',', line', 57
    call wrf_debug ( WARN , msg)
    return
  endif
! Do nothing unless it is time to read time-independent domain metadata.
IF ( ncd_ok_to_get_dom_ti( DataHandle ) ) THEN
  if(DH%FileStatus == WRF_FILE_NOT_OPENED) then
    Status = WRF_WARN_FILE_NOT_OPENED
    write(msg,*) &
'Warning FILE NOT OPENED in ',"ext_ncd_get_dom_ti.code",' ','DOUBLE',', line', 66
    call wrf_debug ( WARN , msg)
  elseif(DH%FileStatus == WRF_FILE_OPENED_NOT_COMMITTED) then
    Status = WRF_WARN_DRYRUN_READ
    write(msg,*) &
'Warning DRYRUN READ in ',"ext_ncd_get_dom_ti.code",' ','DOUBLE',', line', 71
    call wrf_debug ( WARN , msg)
  elseif(DH%FileStatus == WRF_FILE_OPENED_FOR_WRITE) then
    Status = WRF_WARN_READ_WONLY_FILE
    write(msg,*) &
'Warning READ WRITE ONLY FILE in ',"ext_ncd_get_dom_ti.code",' ','DOUBLE',', line', 76
    call wrf_debug ( WARN , msg)
  elseif(DH%FileStatus == WRF_FILE_OPENED_FOR_READ) then
    stat = NF_INQ_ATT(DH%NCID,NF_GLOBAL,Element, XType, Len)
    call netcdf_err(stat,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) &
'NetCDF error in ',"ext_ncd_get_dom_ti.code",' ','DOUBLE',', line', 83,' Element ',Element
      call wrf_debug ( WARN , msg)
      return
    endif
    if ( NF_DOUBLE == NF_DOUBLE .OR. NF_DOUBLE == NF_FLOAT ) then
      if( .NOT. ( XType==NF_FLOAT .OR. XType==NF_DOUBLE) ) then
        Status = WRF_WARN_TYPE_MISMATCH
        write(msg,*) &
'Warning TYPE MISMATCH in ',"ext_ncd_get_dom_ti.code",' ','DOUBLE',', line', 91
        call wrf_debug ( WARN , msg)
        return
      endif
    else
      if( XType/=NF_DOUBLE) then
        Status = WRF_WARN_TYPE_MISMATCH
        write(msg,*) &
'Warning TYPE MISMATCH in ',"ext_ncd_get_dom_ti.code",' ','DOUBLE',', line', 99
        call wrf_debug ( WARN , msg)
        return
      endif
    endif
    if(Len<=0) then
      Status = WRF_WARN_LENGTH_LESS_THAN_1
      write(msg,*) &
'Warning LENGTH < 1 in ',"ext_ncd_get_dom_ti.code",' ','DOUBLE',', line', 107
      call wrf_debug ( WARN , msg)
      return
    endif
    allocate(Buffer(Len), STAT=stat)
    if(stat/= 0) then
      Status = WRF_ERR_FATAL_ALLOCATION_ERROR
      write(msg,*) &
'Fatal ALLOCATION ERROR in ',"ext_ncd_get_dom_ti.code",' ','DOUBLE',', line', 116
      call wrf_debug ( FATAL , msg)
      return
    endif
    stat = NF_GET_ATT_DOUBLE (DH%NCID,NF_GLOBAL,Element,Buffer)
    call netcdf_err(stat,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) &
'NetCDF error in ',"ext_ncd_get_dom_ti.code",' ','DOUBLE',', line', 128,' Element ',Element
      call wrf_debug ( WARN , msg)
      return
    endif
    Data(1:min(Len,Count)) = Buffer(1:min(Len,Count))
    deallocate(Buffer, STAT=stat)
    if(stat/= WRF_NO_ERR) then
      Status = WRF_ERR_FATAL_DEALLOCATION_ERR
      write(msg,*) &
'Fatal DEALLOCATION ERROR in ',"ext_ncd_get_dom_ti.code",' ','DOUBLE',', line', 138
      call wrf_debug ( FATAL , msg)
      return
    endif
    if(Len > Count) then
      OutCount = Count
      Status = WRF_WARN_MORE_DATA_IN_FILE
    else
      OutCount = Len
      Status = WRF_NO_ERR
    endif
  else
    Status = WRF_ERR_FATAL_BAD_FILE_STATUS
    write(msg,*) &
'Fatal error BAD FILE STATUS in ',"ext_ncd_get_dom_ti.code",' ','DOUBLE',', line', 153
    call wrf_debug ( FATAL , msg)
  endif
ENDIF
  return
end subroutine ext_ncd_get_dom_ti_double
subroutine ext_ncd_get_dom_ti_logical(DataHandle,Element,Data,Count,OutCount,Status)
!*------------------------------------------------------------------------------
!* Standard Disclaimer
!*
!* Forecast Systems Laboratory
!* NOAA/OAR/ERL/FSL
!* 325 Broadway
!* Boulder, CO 80303
!*
!* AVIATION DIVISION
!* ADVANCED COMPUTING BRANCH
!* SMS/NNT Version: 2.0.0
!*
!* This software and its documentation are in the public domain and
!* are furnished "as is". The United States government, its
!* instrumentalities, officers, employees, and agents make no
!* warranty, express or implied, as to the usefulness of the software
!* and documentation for any purpose. They assume no
!* responsibility (1) for the use of the software and documentation;
!* or (2) to provide technical support to users.
!*
!* Permission to use, copy, modify, and distribute this software is
!* hereby granted, provided that this disclaimer notice appears in
!* all copies. All modifications to this software must be clearly
!* documented, and are solely the responsibility of the agent making
!* the modification. If significant modifications or enhancements
!* are made to this software, the SMS Development team
!* (sms-info@fsl.noaa.gov) should be notified.
!*
!*----------------------------------------------------------------------------
!*
!* WRF NetCDF I/O
! Author: Jacques Middlecoff jacquesm@fsl.noaa.gov
!* Date: October 6, 2000
!*
!*----------------------------------------------------------------------------
  use wrf_data
  use ext_ncd_support_routines
  implicit none
  include 'wrf_status_codes.h'
  include 'netcdf.inc'
  integer ,intent(in) :: DataHandle
  character*(*) ,intent(in) :: Element
  logical,intent(out) :: Data(*)
  integer,intent(in) :: Count
  integer,intent(out) :: OutCOunt
  integer ,intent(out) :: Status
  type(wrf_data_handle) ,pointer :: DH
  integer :: XType
  integer :: Len
  integer :: stat
  integer,allocatable :: Buffer(:)
  call GetDH(DataHandle,DH,Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) &
'Warning Status = ',Status,' in ',"ext_ncd_get_dom_ti.code",' ','LOGICAL',', line', 57
    call wrf_debug ( WARN , msg)
    return
  endif
! Do nothing unless it is time to read time-independent domain metadata.
IF ( ncd_ok_to_get_dom_ti( DataHandle ) ) THEN
  if(DH%FileStatus == WRF_FILE_NOT_OPENED) then
    Status = WRF_WARN_FILE_NOT_OPENED
    write(msg,*) &
'Warning FILE NOT OPENED in ',"ext_ncd_get_dom_ti.code",' ','LOGICAL',', line', 66
    call wrf_debug ( WARN , msg)
  elseif(DH%FileStatus == WRF_FILE_OPENED_NOT_COMMITTED) then
    Status = WRF_WARN_DRYRUN_READ
    write(msg,*) &
'Warning DRYRUN READ in ',"ext_ncd_get_dom_ti.code",' ','LOGICAL',', line', 71
    call wrf_debug ( WARN , msg)
  elseif(DH%FileStatus == WRF_FILE_OPENED_FOR_WRITE) then
    Status = WRF_WARN_READ_WONLY_FILE
    write(msg,*) &
'Warning READ WRITE ONLY FILE in ',"ext_ncd_get_dom_ti.code",' ','LOGICAL',', line', 76
    call wrf_debug ( WARN , msg)
  elseif(DH%FileStatus == WRF_FILE_OPENED_FOR_READ) then
    stat = NF_INQ_ATT(DH%NCID,NF_GLOBAL,Element, XType, Len)
    call netcdf_err(stat,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) &
'NetCDF error in ',"ext_ncd_get_dom_ti.code",' ','LOGICAL',', line', 83,' Element ',Element
      call wrf_debug ( WARN , msg)
      return
    endif
    if ( NF_INT == NF_DOUBLE .OR. NF_INT == NF_FLOAT ) then
      if( .NOT. ( XType==NF_FLOAT .OR. XType==NF_DOUBLE) ) then
        Status = WRF_WARN_TYPE_MISMATCH
        write(msg,*) &
'Warning TYPE MISMATCH in ',"ext_ncd_get_dom_ti.code",' ','LOGICAL',', line', 91
        call wrf_debug ( WARN , msg)
        return
      endif
    else
      if( XType/=NF_INT) then
        Status = WRF_WARN_TYPE_MISMATCH
        write(msg,*) &
'Warning TYPE MISMATCH in ',"ext_ncd_get_dom_ti.code",' ','LOGICAL',', line', 99
        call wrf_debug ( WARN , msg)
        return
      endif
    endif
    if(Len<=0) then
      Status = WRF_WARN_LENGTH_LESS_THAN_1
      write(msg,*) &
'Warning LENGTH < 1 in ',"ext_ncd_get_dom_ti.code",' ','LOGICAL',', line', 107
      call wrf_debug ( WARN , msg)
      return
    endif
    allocate(Buffer(Len), STAT=stat)
    if(stat/= 0) then
      Status = WRF_ERR_FATAL_ALLOCATION_ERROR
      write(msg,*) &
'Fatal ALLOCATION ERROR in ',"ext_ncd_get_dom_ti.code",' ','LOGICAL',', line', 116
      call wrf_debug ( FATAL , msg)
      return
    endif
    stat = NF_GET_ATT_INT (DH%NCID,NF_GLOBAL,Element,Buffer)
    call netcdf_err(stat,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) &
'NetCDF error in ',"ext_ncd_get_dom_ti.code",' ','LOGICAL',', line', 128,' Element ',Element
      call wrf_debug ( WARN , msg)
      return
    endif
    Data(1:min(Len,Count)) = Buffer(1:min(Len,Count))==1
    deallocate(Buffer, STAT=stat)
    if(stat/= WRF_NO_ERR) then
      Status = WRF_ERR_FATAL_DEALLOCATION_ERR
      write(msg,*) &
'Fatal DEALLOCATION ERROR in ',"ext_ncd_get_dom_ti.code",' ','LOGICAL',', line', 138
      call wrf_debug ( FATAL , msg)
      return
    endif
    if(Len > Count) then
      OutCount = Count
      Status = WRF_WARN_MORE_DATA_IN_FILE
    else
      OutCount = Len
      Status = WRF_NO_ERR
    endif
  else
    Status = WRF_ERR_FATAL_BAD_FILE_STATUS
    write(msg,*) &
'Fatal error BAD FILE STATUS in ',"ext_ncd_get_dom_ti.code",' ','LOGICAL',', line', 153
    call wrf_debug ( FATAL , msg)
  endif
ENDIF
  return
end subroutine ext_ncd_get_dom_ti_logical
subroutine ext_ncd_get_dom_ti_char(DataHandle,Element,Data,Status)
!*------------------------------------------------------------------------------
!* Standard Disclaimer
!*
!* Forecast Systems Laboratory
!* NOAA/OAR/ERL/FSL
!* 325 Broadway
!* Boulder, CO 80303
!*
!* AVIATION DIVISION
!* ADVANCED COMPUTING BRANCH
!* SMS/NNT Version: 2.0.0
!*
!* This software and its documentation are in the public domain and
!* are furnished "as is". The United States government, its
!* instrumentalities, officers, employees, and agents make no
!* warranty, express or implied, as to the usefulness of the software
!* and documentation for any purpose. They assume no
!* responsibility (1) for the use of the software and documentation;
!* or (2) to provide technical support to users.
!*
!* Permission to use, copy, modify, and distribute this software is
!* hereby granted, provided that this disclaimer notice appears in
!* all copies. All modifications to this software must be clearly
!* documented, and are solely the responsibility of the agent making
!* the modification. If significant modifications or enhancements
!* are made to this software, the SMS Development team
!* (sms-info@fsl.noaa.gov) should be notified.
!*
!*----------------------------------------------------------------------------
!*
!* WRF NetCDF I/O
! Author: Jacques Middlecoff jacquesm@fsl.noaa.gov
!* Date: October 6, 2000
!*
!*----------------------------------------------------------------------------
  use wrf_data
  use ext_ncd_support_routines
  implicit none
  include 'wrf_status_codes.h'
  include 'netcdf.inc'
  integer ,intent(in) :: DataHandle
  character*(*) ,intent(in) :: Element
  character*(*),intent(out) :: Data
 
 
  integer ,intent(out) :: Status
  type(wrf_data_handle) ,pointer :: DH
  integer :: XType
  integer :: Len
  integer :: stat
 
  call GetDH(DataHandle,DH,Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) &
'Warning Status = ',Status,' in ',"ext_ncd_get_dom_ti.code",' ','CHAR',', line', 57
    call wrf_debug ( WARN , msg)
    return
  endif
! Do nothing unless it is time to read time-independent domain metadata.
IF ( ncd_ok_to_get_dom_ti( DataHandle ) ) THEN
  if(DH%FileStatus == WRF_FILE_NOT_OPENED) then
    Status = WRF_WARN_FILE_NOT_OPENED
    write(msg,*) &
'Warning FILE NOT OPENED in ',"ext_ncd_get_dom_ti.code",' ','CHAR',', line', 66
    call wrf_debug ( WARN , msg)
  elseif(DH%FileStatus == WRF_FILE_OPENED_NOT_COMMITTED) then
    Status = WRF_WARN_DRYRUN_READ
    write(msg,*) &
'Warning DRYRUN READ in ',"ext_ncd_get_dom_ti.code",' ','CHAR',', line', 71
    call wrf_debug ( WARN , msg)
  elseif(DH%FileStatus == WRF_FILE_OPENED_FOR_WRITE) then
    Status = WRF_WARN_READ_WONLY_FILE
    write(msg,*) &
'Warning READ WRITE ONLY FILE in ',"ext_ncd_get_dom_ti.code",' ','CHAR',', line', 76
    call wrf_debug ( WARN , msg)
  elseif(DH%FileStatus == WRF_FILE_OPENED_FOR_READ) then
    stat = NF_INQ_ATT(DH%NCID,NF_GLOBAL,Element, XType, Len)
    call netcdf_err(stat,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) &
'NetCDF error in ',"ext_ncd_get_dom_ti.code",' ','CHAR',', line', 83,' Element ',Element
      call wrf_debug ( WARN , msg)
      return
    endif
    if ( NF_CHAR == NF_DOUBLE .OR. NF_CHAR == NF_FLOAT ) then
      if( .NOT. ( XType==NF_FLOAT .OR. XType==NF_DOUBLE) ) then
        Status = WRF_WARN_TYPE_MISMATCH
        write(msg,*) &
'Warning TYPE MISMATCH in ',"ext_ncd_get_dom_ti.code",' ','CHAR',', line', 91
        call wrf_debug ( WARN , msg)
        return
      endif
    else
      if( XType/=NF_CHAR) then
        Status = WRF_WARN_TYPE_MISMATCH
        write(msg,*) &
'Warning TYPE MISMATCH in ',"ext_ncd_get_dom_ti.code",' ','CHAR',', line', 99
        call wrf_debug ( WARN , msg)
        return
      endif
    endif
    if(Len<=0) then
      Status = WRF_WARN_LENGTH_LESS_THAN_1
      write(msg,*) &
'Warning LENGTH < 1 in ',"ext_ncd_get_dom_ti.code",' ','CHAR',', line', 107
      call wrf_debug ( WARN , msg)
      return
    endif
    Data = ''
    stat = NF_GET_ATT_TEXT(DH%NCID,NF_GLOBAL,Element,Data)
    call netcdf_err(stat,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) &
'NetCDF error in ',"ext_ncd_get_dom_ti.code",' ','CHAR',', line', 128,' Element ',Element
      call wrf_debug ( WARN , msg)
      return
    endif
  else
    Status = WRF_ERR_FATAL_BAD_FILE_STATUS
    write(msg,*) &
'Fatal error BAD FILE STATUS in ',"ext_ncd_get_dom_ti.code",' ','CHAR',', line', 153
    call wrf_debug ( FATAL , msg)
  endif
ENDIF
  return
end subroutine ext_ncd_get_dom_ti_char
subroutine ext_ncd_put_dom_ti_real(DataHandle,Element,Data,Count,Status)
!*------------------------------------------------------------------------------
!* Standard Disclaimer
!*
!* Forecast Systems Laboratory
!* NOAA/OAR/ERL/FSL
!* 325 Broadway
!* Boulder, CO 80303
!*
!* AVIATION DIVISION
!* ADVANCED COMPUTING BRANCH
!* SMS/NNT Version: 2.0.0
!*
!* This software and its documentation are in the public domain and
!* are furnished "as is". The United States government, its
!* instrumentalities, officers, employees, and agents make no
!* warranty, express or implied, as to the usefulness of the software
!* and documentation for any purpose. They assume no
!* responsibility (1) for the use of the software and documentation;
!* or (2) to provide technical support to users.
!*
!* Permission to use, copy, modify, and distribute this software is
!* hereby granted, provided that this disclaimer notice appears in
!* all copies. All modifications to this software must be clearly
!* documented, and are solely the responsibility of the agent making
!* the modification. If significant modifications or enhancements
!* are made to this software, the SMS Development team
!* (sms-info@fsl.noaa.gov) should be notified.
!*
!*----------------------------------------------------------------------------
!*
!* WRF NetCDF I/O
! Author: Jacques Middlecoff jacquesm@fsl.noaa.gov
!* Date: October 6, 2000
!*
!*----------------------------------------------------------------------------
  use wrf_data
  use ext_ncd_support_routines
  implicit none
  include 'wrf_status_codes.h'
  include 'netcdf.inc'
  integer ,intent(in) :: DataHandle
  character*(*) ,intent(in) :: Element
  real ,intent(in) :: Data(*)
  integer,intent(in) :: Count
  integer ,intent(out) :: Status
  type(wrf_data_handle) ,pointer :: DH
  integer :: stat
  integer :: stat2
  integer ,allocatable :: Buffer(:)
  integer :: i
  call GetDH(DataHandle,DH,Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) &
'Warning Status = ',Status,' in ',"ext_ncd_put_dom_ti.code",' ','REAL',', line', 56
    call wrf_debug ( WARN , msg)
    return
  endif
! Do nothing unless it is time to write time-independent domain metadata.
IF ( ncd_ok_to_put_dom_ti( DataHandle ) ) THEN
  if(DH%FileStatus == WRF_FILE_NOT_OPENED) then
    Status = WRF_WARN_FILE_NOT_OPENED
    write(msg,*) &
'Warning FILE NOT OPENED in ',"ext_ncd_put_dom_ti.code",' ','REAL',', line', 65
    call wrf_debug ( WARN , msg)
  elseif(DH%FileStatus == WRF_FILE_OPENED_FOR_READ) then
    STATUS = WRF_WARN_WRITE_RONLY_FILE
    write(msg,*) &
'Warning WRITE READ ONLY FILE in ',"ext_ncd_put_dom_ti.code",' ','REAL',', line', 70
    call wrf_debug ( WARN , msg)
  elseif(DH%FileStatus == WRF_FILE_OPENED_NOT_COMMITTED) then
      stat = NF_PUT_ATT_REAL (DH%NCID,NF_GLOBAL,Element,NF_FLOAT,Count,Data)
    call netcdf_err(stat,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) 'NetCDF error in ',"ext_ncd_put_dom_ti.code",' ','REAL',', line', 101,' Element ',Element
      call wrf_debug ( WARN , msg)
      return
    endif
  elseif (DH%FileStatus == WRF_FILE_OPENED_FOR_WRITE .OR. DH%FileStatus == WRF_FILE_OPENED_FOR_UPDATE) then
    stat = NF_REDEF(DH%NCID)
    call netcdf_err(stat,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) &
'NetCDF error in ',"ext_ncd_put_dom_ti.code",' ','REAL',', line', 110,' Element ',Element
      call wrf_debug ( WARN , msg)
      return
    endif
      stat = NF_PUT_ATT_REAL (DH%NCID,NF_GLOBAL,Element,NF_FLOAT,Count,Data)
    call netcdf_err(stat,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) &
'NetCDF error in ',"ext_ncd_put_dom_ti.code",' ','REAL',', line', 145,' Element ',Element
      call wrf_debug ( WARN , msg)
      return
    endif
    stat = NF_ENDDEF(DH%NCID)
    call netcdf_err(stat,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) &
'NetCDF error in ',"ext_ncd_put_dom_ti.code",' ','REAL',', line', 153,' Element ',Element
      call wrf_debug ( WARN , msg)
      return
    endif
  else
    Status = WRF_ERR_FATAL_BAD_FILE_STATUS
    write(msg,*) &
'Fatal error BAD FILE STATUS in ',"ext_ncd_put_dom_ti.code",' ','REAL',', line', 160
    call wrf_debug ( FATAL , msg)
  endif
ENDIF
  return
end subroutine ext_ncd_put_dom_ti_real
subroutine ext_ncd_put_dom_ti_integer(DataHandle,Element,Data,Count,Status)
!*------------------------------------------------------------------------------
!* Standard Disclaimer
!*
!* Forecast Systems Laboratory
!* NOAA/OAR/ERL/FSL
!* 325 Broadway
!* Boulder, CO 80303
!*
!* AVIATION DIVISION
!* ADVANCED COMPUTING BRANCH
!* SMS/NNT Version: 2.0.0
!*
!* This software and its documentation are in the public domain and
!* are furnished "as is". The United States government, its
!* instrumentalities, officers, employees, and agents make no
!* warranty, express or implied, as to the usefulness of the software
!* and documentation for any purpose. They assume no
!* responsibility (1) for the use of the software and documentation;
!* or (2) to provide technical support to users.
!*
!* Permission to use, copy, modify, and distribute this software is
!* hereby granted, provided that this disclaimer notice appears in
!* all copies. All modifications to this software must be clearly
!* documented, and are solely the responsibility of the agent making
!* the modification. If significant modifications or enhancements
!* are made to this software, the SMS Development team
!* (sms-info@fsl.noaa.gov) should be notified.
!*
!*----------------------------------------------------------------------------
!*
!* WRF NetCDF I/O
! Author: Jacques Middlecoff jacquesm@fsl.noaa.gov
!* Date: October 6, 2000
!*
!*----------------------------------------------------------------------------
  use wrf_data
  use ext_ncd_support_routines
  implicit none
  include 'wrf_status_codes.h'
  include 'netcdf.inc'
  integer ,intent(in) :: DataHandle
  character*(*) ,intent(in) :: Element
  integer,intent(in) :: Data(*)
  integer,intent(in) :: Count
  integer ,intent(out) :: Status
  type(wrf_data_handle) ,pointer :: DH
  integer :: stat
  integer :: stat2
  integer ,allocatable :: Buffer(:)
  integer :: i
  call GetDH(DataHandle,DH,Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) &
'Warning Status = ',Status,' in ',"ext_ncd_put_dom_ti.code",' ','INTEGER',', line', 56
    call wrf_debug ( WARN , msg)
    return
  endif
! Do nothing unless it is time to write time-independent domain metadata.
IF ( ncd_ok_to_put_dom_ti( DataHandle ) ) THEN
  if(DH%FileStatus == WRF_FILE_NOT_OPENED) then
    Status = WRF_WARN_FILE_NOT_OPENED
    write(msg,*) &
'Warning FILE NOT OPENED in ',"ext_ncd_put_dom_ti.code",' ','INTEGER',', line', 65
    call wrf_debug ( WARN , msg)
  elseif(DH%FileStatus == WRF_FILE_OPENED_FOR_READ) then
    STATUS = WRF_WARN_WRITE_RONLY_FILE
    write(msg,*) &
'Warning WRITE READ ONLY FILE in ',"ext_ncd_put_dom_ti.code",' ','INTEGER',', line', 70
    call wrf_debug ( WARN , msg)
  elseif(DH%FileStatus == WRF_FILE_OPENED_NOT_COMMITTED) then
      stat = NF_PUT_ATT_INT (DH%NCID,NF_GLOBAL,Element,NF_INT,Count,Data)
    call netcdf_err(stat,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) 'NetCDF error in ',"ext_ncd_put_dom_ti.code",' ','INTEGER',', line', 101,' Element ',Element
      call wrf_debug ( WARN , msg)
      return
    endif
  elseif (DH%FileStatus == WRF_FILE_OPENED_FOR_WRITE .OR. DH%FileStatus == WRF_FILE_OPENED_FOR_UPDATE) then
    stat = NF_REDEF(DH%NCID)
    call netcdf_err(stat,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) &
'NetCDF error in ',"ext_ncd_put_dom_ti.code",' ','INTEGER',', line', 110,' Element ',Element
      call wrf_debug ( WARN , msg)
      return
    endif
      stat = NF_PUT_ATT_INT (DH%NCID,NF_GLOBAL,Element,NF_INT,Count,Data)
    call netcdf_err(stat,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) &
'NetCDF error in ',"ext_ncd_put_dom_ti.code",' ','INTEGER',', line', 145,' Element ',Element
      call wrf_debug ( WARN , msg)
      return
    endif
    stat = NF_ENDDEF(DH%NCID)
    call netcdf_err(stat,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) &
'NetCDF error in ',"ext_ncd_put_dom_ti.code",' ','INTEGER',', line', 153,' Element ',Element
      call wrf_debug ( WARN , msg)
      return
    endif
  else
    Status = WRF_ERR_FATAL_BAD_FILE_STATUS
    write(msg,*) &
'Fatal error BAD FILE STATUS in ',"ext_ncd_put_dom_ti.code",' ','INTEGER',', line', 160
    call wrf_debug ( FATAL , msg)
  endif
ENDIF
  return
end subroutine ext_ncd_put_dom_ti_integer
subroutine ext_ncd_put_dom_ti_double(DataHandle,Element,Data,Count,Status)
!*------------------------------------------------------------------------------
!* Standard Disclaimer
!*
!* Forecast Systems Laboratory
!* NOAA/OAR/ERL/FSL
!* 325 Broadway
!* Boulder, CO 80303
!*
!* AVIATION DIVISION
!* ADVANCED COMPUTING BRANCH
!* SMS/NNT Version: 2.0.0
!*
!* This software and its documentation are in the public domain and
!* are furnished "as is". The United States government, its
!* instrumentalities, officers, employees, and agents make no
!* warranty, express or implied, as to the usefulness of the software
!* and documentation for any purpose. They assume no
!* responsibility (1) for the use of the software and documentation;
!* or (2) to provide technical support to users.
!*
!* Permission to use, copy, modify, and distribute this software is
!* hereby granted, provided that this disclaimer notice appears in
!* all copies. All modifications to this software must be clearly
!* documented, and are solely the responsibility of the agent making
!* the modification. If significant modifications or enhancements
!* are made to this software, the SMS Development team
!* (sms-info@fsl.noaa.gov) should be notified.
!*
!*----------------------------------------------------------------------------
!*
!* WRF NetCDF I/O
! Author: Jacques Middlecoff jacquesm@fsl.noaa.gov
!* Date: October 6, 2000
!*
!*----------------------------------------------------------------------------
  use wrf_data
  use ext_ncd_support_routines
  implicit none
  include 'wrf_status_codes.h'
  include 'netcdf.inc'
  integer ,intent(in) :: DataHandle
  character*(*) ,intent(in) :: Element
  real*8 ,intent(in) :: Data(*)
  integer,intent(in) :: Count
  integer ,intent(out) :: Status
  type(wrf_data_handle) ,pointer :: DH
  integer :: stat
  integer :: stat2
  integer ,allocatable :: Buffer(:)
  integer :: i
  call GetDH(DataHandle,DH,Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) &
'Warning Status = ',Status,' in ',"ext_ncd_put_dom_ti.code",' ','DOUBLE',', line', 56
    call wrf_debug ( WARN , msg)
    return
  endif
! Do nothing unless it is time to write time-independent domain metadata.
IF ( ncd_ok_to_put_dom_ti( DataHandle ) ) THEN
  if(DH%FileStatus == WRF_FILE_NOT_OPENED) then
    Status = WRF_WARN_FILE_NOT_OPENED
    write(msg,*) &
'Warning FILE NOT OPENED in ',"ext_ncd_put_dom_ti.code",' ','DOUBLE',', line', 65
    call wrf_debug ( WARN , msg)
  elseif(DH%FileStatus == WRF_FILE_OPENED_FOR_READ) then
    STATUS = WRF_WARN_WRITE_RONLY_FILE
    write(msg,*) &
'Warning WRITE READ ONLY FILE in ',"ext_ncd_put_dom_ti.code",' ','DOUBLE',', line', 70
    call wrf_debug ( WARN , msg)
  elseif(DH%FileStatus == WRF_FILE_OPENED_NOT_COMMITTED) then
      stat = NF_PUT_ATT_DOUBLE (DH%NCID,NF_GLOBAL,Element,NF_DOUBLE,Count,Data)
    call netcdf_err(stat,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) 'NetCDF error in ',"ext_ncd_put_dom_ti.code",' ','DOUBLE',', line', 101,' Element ',Element
      call wrf_debug ( WARN , msg)
      return
    endif
  elseif (DH%FileStatus == WRF_FILE_OPENED_FOR_WRITE .OR. DH%FileStatus == WRF_FILE_OPENED_FOR_UPDATE) then
    stat = NF_REDEF(DH%NCID)
    call netcdf_err(stat,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) &
'NetCDF error in ',"ext_ncd_put_dom_ti.code",' ','DOUBLE',', line', 110,' Element ',Element
      call wrf_debug ( WARN , msg)
      return
    endif
      stat = NF_PUT_ATT_DOUBLE (DH%NCID,NF_GLOBAL,Element,NF_DOUBLE,Count,Data)
    call netcdf_err(stat,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) &
'NetCDF error in ',"ext_ncd_put_dom_ti.code",' ','DOUBLE',', line', 145,' Element ',Element
      call wrf_debug ( WARN , msg)
      return
    endif
    stat = NF_ENDDEF(DH%NCID)
    call netcdf_err(stat,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) &
'NetCDF error in ',"ext_ncd_put_dom_ti.code",' ','DOUBLE',', line', 153,' Element ',Element
      call wrf_debug ( WARN , msg)
      return
    endif
  else
    Status = WRF_ERR_FATAL_BAD_FILE_STATUS
    write(msg,*) &
'Fatal error BAD FILE STATUS in ',"ext_ncd_put_dom_ti.code",' ','DOUBLE',', line', 160
    call wrf_debug ( FATAL , msg)
  endif
ENDIF
  return
end subroutine ext_ncd_put_dom_ti_double
subroutine ext_ncd_put_dom_ti_logical(DataHandle,Element,Data,Count,Status)
!*------------------------------------------------------------------------------
!* Standard Disclaimer
!*
!* Forecast Systems Laboratory
!* NOAA/OAR/ERL/FSL
!* 325 Broadway
!* Boulder, CO 80303
!*
!* AVIATION DIVISION
!* ADVANCED COMPUTING BRANCH
!* SMS/NNT Version: 2.0.0
!*
!* This software and its documentation are in the public domain and
!* are furnished "as is". The United States government, its
!* instrumentalities, officers, employees, and agents make no
!* warranty, express or implied, as to the usefulness of the software
!* and documentation for any purpose. They assume no
!* responsibility (1) for the use of the software and documentation;
!* or (2) to provide technical support to users.
!*
!* Permission to use, copy, modify, and distribute this software is
!* hereby granted, provided that this disclaimer notice appears in
!* all copies. All modifications to this software must be clearly
!* documented, and are solely the responsibility of the agent making
!* the modification. If significant modifications or enhancements
!* are made to this software, the SMS Development team
!* (sms-info@fsl.noaa.gov) should be notified.
!*
!*----------------------------------------------------------------------------
!*
!* WRF NetCDF I/O
! Author: Jacques Middlecoff jacquesm@fsl.noaa.gov
!* Date: October 6, 2000
!*
!*----------------------------------------------------------------------------
  use wrf_data
  use ext_ncd_support_routines
  implicit none
  include 'wrf_status_codes.h'
  include 'netcdf.inc'
  integer ,intent(in) :: DataHandle
  character*(*) ,intent(in) :: Element
  logical,intent(in) :: Data(*)
  integer,intent(in) :: Count
  integer ,intent(out) :: Status
  type(wrf_data_handle) ,pointer :: DH
  integer :: stat
  integer :: stat2
  integer ,allocatable :: Buffer(:)
  integer :: i
  call GetDH(DataHandle,DH,Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) &
'Warning Status = ',Status,' in ',"ext_ncd_put_dom_ti.code",' ','LOGICAL',', line', 56
    call wrf_debug ( WARN , msg)
    return
  endif
! Do nothing unless it is time to write time-independent domain metadata.
IF ( ncd_ok_to_put_dom_ti( DataHandle ) ) THEN
  if(DH%FileStatus == WRF_FILE_NOT_OPENED) then
    Status = WRF_WARN_FILE_NOT_OPENED
    write(msg,*) &
'Warning FILE NOT OPENED in ',"ext_ncd_put_dom_ti.code",' ','LOGICAL',', line', 65
    call wrf_debug ( WARN , msg)
  elseif(DH%FileStatus == WRF_FILE_OPENED_FOR_READ) then
    STATUS = WRF_WARN_WRITE_RONLY_FILE
    write(msg,*) &
'Warning WRITE READ ONLY FILE in ',"ext_ncd_put_dom_ti.code",' ','LOGICAL',', line', 70
    call wrf_debug ( WARN , msg)
  elseif(DH%FileStatus == WRF_FILE_OPENED_NOT_COMMITTED) then
      allocate(Buffer(Count), STAT=stat)
      if(stat/= 0) then
        Status = WRF_ERR_FATAL_ALLOCATION_ERROR
        write(msg,*) 'Fatal ALLOCATION ERROR in ',"ext_ncd_put_dom_ti.code",' ','LOGICAL',', line', 77
        call wrf_debug ( FATAL , msg)
        return
      endif
      do i=1,Count
        if(data(i)) then
           Buffer(i)=1
        else
           Buffer(i)=0
        endif
      enddo
      stat = NF_PUT_ATT_INT (DH%NCID,NF_GLOBAL,Element,NF_INT,Count,Buffer)
      deallocate(Buffer, STAT=stat2)
      if(stat2/= 0) then
        Status = WRF_ERR_FATAL_DEALLOCATION_ERR
        write(msg,*) 'Fatal DEALLOCATION ERROR in ',"ext_ncd_put_dom_ti.code",' ','LOGICAL',', line', 92
        call wrf_debug ( FATAL , msg)
        return
      endif
    call netcdf_err(stat,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) 'NetCDF error in ',"ext_ncd_put_dom_ti.code",' ','LOGICAL',', line', 101,' Element ',Element
      call wrf_debug ( WARN , msg)
      return
    endif
  elseif (DH%FileStatus == WRF_FILE_OPENED_FOR_WRITE .OR. DH%FileStatus == WRF_FILE_OPENED_FOR_UPDATE) then
    stat = NF_REDEF(DH%NCID)
    call netcdf_err(stat,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) &
'NetCDF error in ',"ext_ncd_put_dom_ti.code",' ','LOGICAL',', line', 110,' Element ',Element
      call wrf_debug ( WARN , msg)
      return
    endif
      allocate(Buffer(Count), STAT=stat)
      if(stat/= 0) then
        Status = WRF_ERR_FATAL_ALLOCATION_ERROR
        write(msg,*) &
'Fatal ALLOCATION ERROR in ',"ext_ncd_put_dom_ti.code",' ','LOGICAL',', line', 119
        call wrf_debug ( FATAL , msg)
        return
      endif
      do i=1,Count
        if(data(i)) then
           Buffer(i)=1
        else
           Buffer(i)=0
        endif
      enddo
      stat = NF_PUT_ATT_INT (DH%NCID,NF_GLOBAL,Element,NF_INT,Count,Buffer)
      deallocate(Buffer, STAT=stat2)
      if(stat2/= 0) then
        Status = WRF_ERR_FATAL_DEALLOCATION_ERR
        write(msg,*) &
'Fatal DEALLOCATION ERROR in ',"ext_ncd_put_dom_ti.code",' ','LOGICAL',', line', 135
        call wrf_debug ( FATAL , msg)
        return
      endif
    call netcdf_err(stat,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) &
'NetCDF error in ',"ext_ncd_put_dom_ti.code",' ','LOGICAL',', line', 145,' Element ',Element
      call wrf_debug ( WARN , msg)
      return
    endif
    stat = NF_ENDDEF(DH%NCID)
    call netcdf_err(stat,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) &
'NetCDF error in ',"ext_ncd_put_dom_ti.code",' ','LOGICAL',', line', 153,' Element ',Element
      call wrf_debug ( WARN , msg)
      return
    endif
  else
    Status = WRF_ERR_FATAL_BAD_FILE_STATUS
    write(msg,*) &
'Fatal error BAD FILE STATUS in ',"ext_ncd_put_dom_ti.code",' ','LOGICAL',', line', 160
    call wrf_debug ( FATAL , msg)
  endif
ENDIF
  return
end subroutine ext_ncd_put_dom_ti_logical
subroutine ext_ncd_put_dom_ti_char(DataHandle,Element,Data,Status)
!*------------------------------------------------------------------------------
!* Standard Disclaimer
!*
!* Forecast Systems Laboratory
!* NOAA/OAR/ERL/FSL
!* 325 Broadway
!* Boulder, CO 80303
!*
!* AVIATION DIVISION
!* ADVANCED COMPUTING BRANCH
!* SMS/NNT Version: 2.0.0
!*
!* This software and its documentation are in the public domain and
!* are furnished "as is". The United States government, its
!* instrumentalities, officers, employees, and agents make no
!* warranty, express or implied, as to the usefulness of the software
!* and documentation for any purpose. They assume no
!* responsibility (1) for the use of the software and documentation;
!* or (2) to provide technical support to users.
!*
!* Permission to use, copy, modify, and distribute this software is
!* hereby granted, provided that this disclaimer notice appears in
!* all copies. All modifications to this software must be clearly
!* documented, and are solely the responsibility of the agent making
!* the modification. If significant modifications or enhancements
!* are made to this software, the SMS Development team
!* (sms-info@fsl.noaa.gov) should be notified.
!*
!*----------------------------------------------------------------------------
!*
!* WRF NetCDF I/O
! Author: Jacques Middlecoff jacquesm@fsl.noaa.gov
!* Date: October 6, 2000
!*
!*----------------------------------------------------------------------------
  use wrf_data
  use ext_ncd_support_routines
  implicit none
  include 'wrf_status_codes.h'
  include 'netcdf.inc'
  integer ,intent(in) :: DataHandle
  character*(*) ,intent(in) :: Element
  character*(*),intent(in) :: Data
  integer,parameter :: Count=1
  integer ,intent(out) :: Status
  type(wrf_data_handle) ,pointer :: DH
  integer :: stat
  integer :: stat2
  integer ,allocatable :: Buffer(:)
  integer :: i
  call GetDH(DataHandle,DH,Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) &
'Warning Status = ',Status,' in ',"ext_ncd_put_dom_ti.code",' ','CHAR',', line', 56
    call wrf_debug ( WARN , msg)
    return
  endif
! Do nothing unless it is time to write time-independent domain metadata.
IF ( ncd_ok_to_put_dom_ti( DataHandle ) ) THEN
  if(DH%FileStatus == WRF_FILE_NOT_OPENED) then
    Status = WRF_WARN_FILE_NOT_OPENED
    write(msg,*) &
'Warning FILE NOT OPENED in ',"ext_ncd_put_dom_ti.code",' ','CHAR',', line', 65
    call wrf_debug ( WARN , msg)
  elseif(DH%FileStatus == WRF_FILE_OPENED_FOR_READ) then
    STATUS = WRF_WARN_WRITE_RONLY_FILE
    write(msg,*) &
'Warning WRITE READ ONLY FILE in ',"ext_ncd_put_dom_ti.code",' ','CHAR',', line', 70
    call wrf_debug ( WARN , msg)
  elseif(DH%FileStatus == WRF_FILE_OPENED_NOT_COMMITTED) then
      stat = NF_PUT_ATT_TEXT (DH%NCID,NF_GLOBAL,Element,len_trim(Data),Data)
    call netcdf_err(stat,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) 'NetCDF error in ',"ext_ncd_put_dom_ti.code",' ','CHAR',', line', 101,' Element ',Element
      call wrf_debug ( WARN , msg)
      return
    endif
  elseif (DH%FileStatus == WRF_FILE_OPENED_FOR_WRITE .OR. DH%FileStatus == WRF_FILE_OPENED_FOR_UPDATE) then
    stat = NF_REDEF(DH%NCID)
    call netcdf_err(stat,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) &
'NetCDF error in ',"ext_ncd_put_dom_ti.code",' ','CHAR',', line', 110,' Element ',Element
      call wrf_debug ( WARN , msg)
      return
    endif
      stat = NF_PUT_ATT_TEXT (DH%NCID,NF_GLOBAL,Element,len_trim(Data),Data)
    call netcdf_err(stat,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) &
'NetCDF error in ',"ext_ncd_put_dom_ti.code",' ','CHAR',', line', 145,' Element ',Element
      call wrf_debug ( WARN , msg)
      return
    endif
    stat = NF_ENDDEF(DH%NCID)
    call netcdf_err(stat,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) &
'NetCDF error in ',"ext_ncd_put_dom_ti.code",' ','CHAR',', line', 153,' Element ',Element
      call wrf_debug ( WARN , msg)
      return
    endif
  else
    Status = WRF_ERR_FATAL_BAD_FILE_STATUS
    write(msg,*) &
'Fatal error BAD FILE STATUS in ',"ext_ncd_put_dom_ti.code",' ','CHAR',', line', 160
    call wrf_debug ( FATAL , msg)
  endif
ENDIF
  return
end subroutine ext_ncd_put_dom_ti_char
subroutine ext_ncd_put_var_ti_real(DataHandle,Element,Var,Data,Count,Status)
!*------------------------------------------------------------------------------
!* Standard Disclaimer
!*
!* Forecast Systems Laboratory
!* NOAA/OAR/ERL/FSL
!* 325 Broadway
!* Boulder, CO 80303
!*
!* AVIATION DIVISION
!* ADVANCED COMPUTING BRANCH
!* SMS/NNT Version: 2.0.0
!*
!* This software and its documentation are in the public domain and
!* are furnished "as is". The United States government, its
!* instrumentalities, officers, employees, and agents make no
!* warranty, express or implied, as to the usefulness of the software
!* and documentation for any purpose. They assume no
!* responsibility (1) for the use of the software and documentation;
!* or (2) to provide technical support to users.
!*
!* Permission to use, copy, modify, and distribute this software is
!* hereby granted, provided that this disclaimer notice appears in
!* all copies. All modifications to this software must be clearly
!* documented, and are solely the responsibility of the agent making
!* the modification. If significant modifications or enhancements
!* are made to this software, the SMS Development team
!* (sms-info@fsl.noaa.gov) should be notified.
!*
!*----------------------------------------------------------------------------
!*
!* WRF NetCDF I/O
! Author: Jacques Middlecoff jacquesm@fsl.noaa.gov
!* Date: October 6, 2000
!*
!*----------------------------------------------------------------------------
  use wrf_data
  use ext_ncd_support_routines
  implicit none
  include 'wrf_status_codes.h'
  include 'netcdf.inc'
  integer ,intent(in) :: DataHandle
  character*(*) ,intent(in) :: Element
  character*(*) ,intent(in) :: Var
  real ,intent(in) :: Data(*)
  integer ,intent(in) :: Count
  integer ,intent(out) :: Status
  type(wrf_data_handle) ,pointer :: DH
  character (VarNameLen) :: VarName
  integer :: stat
  integer ,allocatable :: Buffer(:)
  integer :: i
  integer :: NVar
  character*1 :: null
  null=char(0)
  VarName = Var
  call GetDH(DataHandle,DH,Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) &
'Warning Status = ',Status,' in ',"ext_ncd_put_var_ti.code",' ','REAL',', line', 61
    call wrf_debug ( WARN , msg)
    return
  endif
  if(DH%FileStatus == WRF_FILE_NOT_OPENED) then
    Status = WRF_WARN_FILE_NOT_OPENED
    write(msg,*) &
'Warning FILE NOT OPENED in ',"ext_ncd_put_var_ti.code",' ','REAL',', line', 68
    call wrf_debug ( WARN , msg)
  elseif(DH%FileStatus == WRF_FILE_OPENED_FOR_READ) then
    Status = WRF_WARN_WRITE_RONLY_FILE
    write(msg,*) &
'Warning WRITE READ ONLY FILE in ',"ext_ncd_put_var_ti.code",' ','REAL',', line', 73
    call wrf_debug ( WARN , msg)
  elseif(DH%FileStatus == WRF_FILE_OPENED_FOR_WRITE) then
    Status = WRF_WARN_MD_AFTER_OPEN
    write(msg,*) &
'Warning WRITE METADATA AFTER OPEN in ',"ext_ncd_put_var_ti.code",' ','REAL',', line', 78
    call wrf_debug ( WARN , msg)
    return
  elseif(DH%FileStatus == WRF_FILE_OPENED_NOT_COMMITTED) then
    do NVar=1,MaxVars
      if(TRIM(DH%VarNames(NVar)) == TRIM(VarName)) then
        exit
      elseif(NVar == MaxVars) then
        Status = WRF_WARN_VAR_NF
        write(msg,*) &
'Warning VARIABLE NOT FOUND in ',"ext_ncd_put_var_ti.code",' ','REAL',', line', 88 &
                        ,NVar,VarName
        call wrf_debug ( WARN , msg)
        return
      endif
    enddo
    stat = NF_PUT_ATT_REAL(DH%NCID,DH%VarIDs(NVar),trim(Element), NF_FLOAT,Count,Data )
    call netcdf_err(stat,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) &
'NetCDF error for Var ',TRIM(Var),&
        ' Element ',trim(Element),' in ',"ext_ncd_put_var_ti.code",' ','REAL',', line', 124
      call wrf_debug ( WARN , msg)
    endif
  else
    Status = WRF_ERR_FATAL_BAD_FILE_STATUS
    write(msg,*) &
'Fatal error BAD FILE STATUS in ',"ext_ncd_put_var_ti.code",' ','REAL',', line', 140
    call wrf_debug ( FATAL , msg)
    return
  endif
  return
end subroutine ext_ncd_put_var_ti_real
subroutine ext_ncd_put_var_td_real(DataHandle,Element,DateStr,Var,Data,Count,Status)
!*------------------------------------------------------------------------------
!* Standard Disclaimer
!*
!* Forecast Systems Laboratory
!* NOAA/OAR/ERL/FSL
!* 325 Broadway
!* Boulder, CO 80303
!*
!* AVIATION DIVISION
!* ADVANCED COMPUTING BRANCH
!* SMS/NNT Version: 2.0.0
!*
!* This software and its documentation are in the public domain and
!* are furnished "as is". The United States government, its
!* instrumentalities, officers, employees, and agents make no
!* warranty, express or implied, as to the usefulness of the software
!* and documentation for any purpose. They assume no
!* responsibility (1) for the use of the software and documentation;
!* or (2) to provide technical support to users.
!*
!* Permission to use, copy, modify, and distribute this software is
!* hereby granted, provided that this disclaimer notice appears in
!* all copies. All modifications to this software must be clearly
!* documented, and are solely the responsibility of the agent making
!* the modification. If significant modifications or enhancements
!* are made to this software, the SMS Development team
!* (sms-info@fsl.noaa.gov) should be notified.
!*
!*----------------------------------------------------------------------------
!*
!* WRF NetCDF I/O
! Author: Jacques Middlecoff jacquesm@fsl.noaa.gov
!* Date: October 6, 2000
!*
!*----------------------------------------------------------------------------
  use wrf_data
  use ext_ncd_support_routines
  implicit none
  include 'wrf_status_codes.h'
  include 'netcdf.inc'
  integer ,intent(in) :: DataHandle
  character*(*) ,intent(in) :: Element
  character*(*) ,intent(in) :: DateStr
  character*(*) ,intent(in) :: Var
  real ,intent(in) :: Data(*)
  integer ,intent(in) :: Count
  integer ,intent(out) :: Status
  type(wrf_data_handle) ,pointer :: DH
  character (VarNameLen) :: VarName
  character (40+len(Element)) :: Name
  integer :: stat
  integer :: stat2
  integer ,allocatable :: Buffer(:)
  integer :: i
  integer :: VDims (2)
  integer :: VStart(2)
  integer :: VCount(2)
  integer :: NVar
  integer :: TimeIndex
  integer :: NCID
  VarName = Var
  call DateCheck(DateStr,Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) &
'Warning DATE STRING ERROR in ',"ext_ncd_put_var_td.code",' ','REAL',', line', 67
    call wrf_debug ( WARN , msg)
    return
  endif
  call GetDH(DataHandle,DH,Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) &
'Warning Status = ',Status,' in ',"ext_ncd_put_var_td.code",' ','REAL',', line', 74
    call wrf_debug ( WARN , msg)
    return
  endif
  NCID = DH%NCID
  call GetName(Element, VarName, Name, Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) &
'Warning Status = ',Status,' in ',"ext_ncd_put_var_td.code",' ','REAL',', line', 82
    call wrf_debug ( WARN , msg)
    return
  endif
  if(DH%FileStatus == WRF_FILE_NOT_OPENED) then
    Status = WRF_WARN_FILE_NOT_OPENED
    write(msg,*) &
'Warning FILE NOT OPENED in ',"ext_ncd_put_var_td.code",' ','REAL',', line', 89
    call wrf_debug ( WARN , msg)
  elseif(DH%FileStatus == WRF_FILE_OPENED_FOR_READ) then
    Status = WRF_WARN_WRITE_RONLY_FILE
    write(msg,*) &
'Warning WRITE READ ONLY FILE in ',"ext_ncd_put_var_td.code",' ','REAL',', line', 94
    call wrf_debug ( WARN , msg)
  elseif(DH%FileStatus == WRF_FILE_OPENED_NOT_COMMITTED) then
    if(Count < 1) then
      Status = WRF_WARN_ZERO_LENGTH_PUT
      return
    endif
    do NVar=1,MaxVars
      if(DH%MDVarNames(NVar) == Name) then
        Status = WRF_WARN_2DRYRUNS_1VARIABLE
        return
      elseif(DH%MDVarNames(NVar) == NO_NAME) then
        DH%MDVarNames(NVar) = Name
        exit
      elseif(NVar == MaxVars) then
        Status = WRF_WARN_TOO_MANY_VARIABLES
        write(msg,*) &
'Warning TOO MANY VARIABLES in ',"ext_ncd_put_var_td.code",' ','REAL',', line', 111
        call wrf_debug ( WARN , msg)
        return
      endif
    enddo
    do i=1,MaxDims
      if(DH%DimLengths(i) == Count) then
        exit
      elseif(DH%DimLengths(i) == NO_DIM) then
        stat = NF_DEF_DIM(NCID,DH%DimNames(i),Count,DH%DimIDs(i))
        call netcdf_err(stat,Status)
        if(Status /= WRF_NO_ERR) then
          write(msg,*) &
'NetCDF error in ',"ext_ncd_put_var_td.code",' ','REAL',', line', 124,' Element ',Element
          call wrf_debug ( WARN , msg)
          return
        endif
        DH%DimLengths(i) = Count
        exit
      elseif(i == MaxDims) then
        Status = WRF_WARN_TOO_MANY_DIMS
        write(msg,*) &
'Warning TOO MANY DIMENSIONS in ',"ext_ncd_put_var_td.code",' ','REAL',', line', 133
        call wrf_debug ( WARN , msg)
        return
      endif
    enddo
    DH%MDVarDimLens(NVar) = Count
    VDims(1) = DH%DimIDs(i)
    VDims(2) = DH%DimUnlimID
    stat = NF_DEF_VAR(NCID,Name,NF_FLOAT,2,VDims,DH%MDVarIDs(NVar))
    call netcdf_err(stat,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) &
'NetCDF error in ',"ext_ncd_put_var_td.code",' ','REAL',', line', 145,' Element ',Element
      call wrf_debug ( WARN , msg)
      return
    endif
  elseif(DH%FileStatus == WRF_FILE_OPENED_FOR_WRITE) then
    do NVar=1,MaxVars
      if(DH%MDVarNames(NVar) == Name) then
        exit
      elseif(DH%MDVarNames(NVar) == NO_NAME) then
        Status = WRF_WARN_MD_NF
        write(msg,*) &
'Warning METADATA NOT FOUND in ',"ext_ncd_put_var_td.code",' ','REAL',', line', 156
        call wrf_debug ( WARN , msg)
        return
      elseif(NVar == MaxVars) then
        Status = WRF_WARN_TOO_MANY_VARIABLES
        write(msg,*) &
'Warning TOO MANY VARIABLES in ',"ext_ncd_put_var_td.code",' ','REAL',', line', 162
        call wrf_debug ( WARN , msg)
        return
      endif
    enddo
    if(Count > DH%MDVarDimLens(NVar)) then
      Status = WRF_WARN_COUNT_TOO_LONG
      write(msg,*) &
'Warning COUNT TOO LONG in ',"ext_ncd_put_var_td.code",' ','REAL',', line', 170
      call wrf_debug ( WARN , msg)
      return
    elseif(Count < 1) then
      Status = WRF_WARN_ZERO_LENGTH_PUT
      write(msg,*) &
'Warning ZERO LENGTH PUT in ',"ext_ncd_put_var_td.code",' ','REAL',', line', 176
      call wrf_debug ( WARN , msg)
      return
    endif
    call GetTimeIndex('write',DataHandle,DateStr,TimeIndex,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) &
'Warning in ',"ext_ncd_put_var_td.code",' ','REAL',', line', 183
      call wrf_debug ( WARN , msg)
      return
    endif
    VStart(1) = 1
    VStart(2) = TimeIndex
    VCount(1) = Count
    VCount(2) = 1
      stat = NF_PUT_VARA_REAL (NCID,DH%MDVarIDs(NVar),VStart,VCount,Data)
    call netcdf_err(stat,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) &
'NetCDF error in ',"ext_ncd_put_var_td.code",' ','REAL',', line', 222,' Element ',Element
      call wrf_debug ( WARN , msg)
      return
    endif
  else
    Status = WRF_ERR_FATAL_BAD_FILE_STATUS
    write(msg,*) &
'Fatal error BAD FILE STATUS in ',"ext_ncd_put_var_td.code",' ','REAL',', line', 229
    call wrf_debug ( FATAL , msg)
    return
  endif
  return
end subroutine ext_ncd_put_var_td_real
subroutine ext_ncd_put_var_ti_double(DataHandle,Element,Var,Data,Count,Status)
!*------------------------------------------------------------------------------
!* Standard Disclaimer
!*
!* Forecast Systems Laboratory
!* NOAA/OAR/ERL/FSL
!* 325 Broadway
!* Boulder, CO 80303
!*
!* AVIATION DIVISION
!* ADVANCED COMPUTING BRANCH
!* SMS/NNT Version: 2.0.0
!*
!* This software and its documentation are in the public domain and
!* are furnished "as is". The United States government, its
!* instrumentalities, officers, employees, and agents make no
!* warranty, express or implied, as to the usefulness of the software
!* and documentation for any purpose. They assume no
!* responsibility (1) for the use of the software and documentation;
!* or (2) to provide technical support to users.
!*
!* Permission to use, copy, modify, and distribute this software is
!* hereby granted, provided that this disclaimer notice appears in
!* all copies. All modifications to this software must be clearly
!* documented, and are solely the responsibility of the agent making
!* the modification. If significant modifications or enhancements
!* are made to this software, the SMS Development team
!* (sms-info@fsl.noaa.gov) should be notified.
!*
!*----------------------------------------------------------------------------
!*
!* WRF NetCDF I/O
! Author: Jacques Middlecoff jacquesm@fsl.noaa.gov
!* Date: October 6, 2000
!*
!*----------------------------------------------------------------------------
  use wrf_data
  use ext_ncd_support_routines
  implicit none
  include 'wrf_status_codes.h'
  include 'netcdf.inc'
  integer ,intent(in) :: DataHandle
  character*(*) ,intent(in) :: Element
  character*(*) ,intent(in) :: Var
  real*8 ,intent(in) :: Data(*)
  integer ,intent(in) :: Count
  integer ,intent(out) :: Status
  type(wrf_data_handle) ,pointer :: DH
  character (VarNameLen) :: VarName
  integer :: stat
  integer ,allocatable :: Buffer(:)
  integer :: i
  integer :: NVar
  character*1 :: null
  null=char(0)
  VarName = Var
  call GetDH(DataHandle,DH,Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) &
'Warning Status = ',Status,' in ',"ext_ncd_put_var_ti.code",' ','DOUBLE',', line', 61
    call wrf_debug ( WARN , msg)
    return
  endif
  if(DH%FileStatus == WRF_FILE_NOT_OPENED) then
    Status = WRF_WARN_FILE_NOT_OPENED
    write(msg,*) &
'Warning FILE NOT OPENED in ',"ext_ncd_put_var_ti.code",' ','DOUBLE',', line', 68
    call wrf_debug ( WARN , msg)
  elseif(DH%FileStatus == WRF_FILE_OPENED_FOR_READ) then
    Status = WRF_WARN_WRITE_RONLY_FILE
    write(msg,*) &
'Warning WRITE READ ONLY FILE in ',"ext_ncd_put_var_ti.code",' ','DOUBLE',', line', 73
    call wrf_debug ( WARN , msg)
  elseif(DH%FileStatus == WRF_FILE_OPENED_FOR_WRITE) then
    Status = WRF_WARN_MD_AFTER_OPEN
    write(msg,*) &
'Warning WRITE METADATA AFTER OPEN in ',"ext_ncd_put_var_ti.code",' ','DOUBLE',', line', 78
    call wrf_debug ( WARN , msg)
    return
  elseif(DH%FileStatus == WRF_FILE_OPENED_NOT_COMMITTED) then
    do NVar=1,MaxVars
      if(TRIM(DH%VarNames(NVar)) == TRIM(VarName)) then
        exit
      elseif(NVar == MaxVars) then
        Status = WRF_WARN_VAR_NF
        write(msg,*) &
'Warning VARIABLE NOT FOUND in ',"ext_ncd_put_var_ti.code",' ','DOUBLE',', line', 88 &
                        ,NVar,VarName
        call wrf_debug ( WARN , msg)
        return
      endif
    enddo
    stat = NF_PUT_ATT_DOUBLE(DH%NCID,DH%VarIDs(NVar),trim(Element), NF_DOUBLE,Count,Data )
    call netcdf_err(stat,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) &
'NetCDF error for Var ',TRIM(Var),&
        ' Element ',trim(Element),' in ',"ext_ncd_put_var_ti.code",' ','DOUBLE',', line', 124
      call wrf_debug ( WARN , msg)
    endif
  else
    Status = WRF_ERR_FATAL_BAD_FILE_STATUS
    write(msg,*) &
'Fatal error BAD FILE STATUS in ',"ext_ncd_put_var_ti.code",' ','DOUBLE',', line', 140
    call wrf_debug ( FATAL , msg)
    return
  endif
  return
end subroutine ext_ncd_put_var_ti_double
subroutine ext_ncd_put_var_td_double(DataHandle,Element,DateStr,Var,Data,Count,Status)
!*------------------------------------------------------------------------------
!* Standard Disclaimer
!*
!* Forecast Systems Laboratory
!* NOAA/OAR/ERL/FSL
!* 325 Broadway
!* Boulder, CO 80303
!*
!* AVIATION DIVISION
!* ADVANCED COMPUTING BRANCH
!* SMS/NNT Version: 2.0.0
!*
!* This software and its documentation are in the public domain and
!* are furnished "as is". The United States government, its
!* instrumentalities, officers, employees, and agents make no
!* warranty, express or implied, as to the usefulness of the software
!* and documentation for any purpose. They assume no
!* responsibility (1) for the use of the software and documentation;
!* or (2) to provide technical support to users.
!*
!* Permission to use, copy, modify, and distribute this software is
!* hereby granted, provided that this disclaimer notice appears in
!* all copies. All modifications to this software must be clearly
!* documented, and are solely the responsibility of the agent making
!* the modification. If significant modifications or enhancements
!* are made to this software, the SMS Development team
!* (sms-info@fsl.noaa.gov) should be notified.
!*
!*----------------------------------------------------------------------------
!*
!* WRF NetCDF I/O
! Author: Jacques Middlecoff jacquesm@fsl.noaa.gov
!* Date: October 6, 2000
!*
!*----------------------------------------------------------------------------
  use wrf_data
  use ext_ncd_support_routines
  implicit none
  include 'wrf_status_codes.h'
  include 'netcdf.inc'
  integer ,intent(in) :: DataHandle
  character*(*) ,intent(in) :: Element
  character*(*) ,intent(in) :: DateStr
  character*(*) ,intent(in) :: Var
  real*8,intent(in) :: Data(*)
  integer ,intent(in) :: Count
  integer ,intent(out) :: Status
  type(wrf_data_handle) ,pointer :: DH
  character (VarNameLen) :: VarName
  character (40+len(Element)) :: Name
  integer :: stat
  integer :: stat2
  integer ,allocatable :: Buffer(:)
  integer :: i
  integer :: VDims (2)
  integer :: VStart(2)
  integer :: VCount(2)
  integer :: NVar
  integer :: TimeIndex
  integer :: NCID
  VarName = Var
  call DateCheck(DateStr,Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) &
'Warning DATE STRING ERROR in ',"ext_ncd_put_var_td.code",' ','DOUBLE',', line', 67
    call wrf_debug ( WARN , msg)
    return
  endif
  call GetDH(DataHandle,DH,Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) &
'Warning Status = ',Status,' in ',"ext_ncd_put_var_td.code",' ','DOUBLE',', line', 74
    call wrf_debug ( WARN , msg)
    return
  endif
  NCID = DH%NCID
  call GetName(Element, VarName, Name, Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) &
'Warning Status = ',Status,' in ',"ext_ncd_put_var_td.code",' ','DOUBLE',', line', 82
    call wrf_debug ( WARN , msg)
    return
  endif
  if(DH%FileStatus == WRF_FILE_NOT_OPENED) then
    Status = WRF_WARN_FILE_NOT_OPENED
    write(msg,*) &
'Warning FILE NOT OPENED in ',"ext_ncd_put_var_td.code",' ','DOUBLE',', line', 89
    call wrf_debug ( WARN , msg)
  elseif(DH%FileStatus == WRF_FILE_OPENED_FOR_READ) then
    Status = WRF_WARN_WRITE_RONLY_FILE
    write(msg,*) &
'Warning WRITE READ ONLY FILE in ',"ext_ncd_put_var_td.code",' ','DOUBLE',', line', 94
    call wrf_debug ( WARN , msg)
  elseif(DH%FileStatus == WRF_FILE_OPENED_NOT_COMMITTED) then
    if(Count < 1) then
      Status = WRF_WARN_ZERO_LENGTH_PUT
      return
    endif
    do NVar=1,MaxVars
      if(DH%MDVarNames(NVar) == Name) then
        Status = WRF_WARN_2DRYRUNS_1VARIABLE
        return
      elseif(DH%MDVarNames(NVar) == NO_NAME) then
        DH%MDVarNames(NVar) = Name
        exit
      elseif(NVar == MaxVars) then
        Status = WRF_WARN_TOO_MANY_VARIABLES
        write(msg,*) &
'Warning TOO MANY VARIABLES in ',"ext_ncd_put_var_td.code",' ','DOUBLE',', line', 111
        call wrf_debug ( WARN , msg)
        return
      endif
    enddo
    do i=1,MaxDims
      if(DH%DimLengths(i) == Count) then
        exit
      elseif(DH%DimLengths(i) == NO_DIM) then
        stat = NF_DEF_DIM(NCID,DH%DimNames(i),Count,DH%DimIDs(i))
        call netcdf_err(stat,Status)
        if(Status /= WRF_NO_ERR) then
          write(msg,*) &
'NetCDF error in ',"ext_ncd_put_var_td.code",' ','DOUBLE',', line', 124,' Element ',Element
          call wrf_debug ( WARN , msg)
          return
        endif
        DH%DimLengths(i) = Count
        exit
      elseif(i == MaxDims) then
        Status = WRF_WARN_TOO_MANY_DIMS
        write(msg,*) &
'Warning TOO MANY DIMENSIONS in ',"ext_ncd_put_var_td.code",' ','DOUBLE',', line', 133
        call wrf_debug ( WARN , msg)
        return
      endif
    enddo
    DH%MDVarDimLens(NVar) = Count
    VDims(1) = DH%DimIDs(i)
    VDims(2) = DH%DimUnlimID
    stat = NF_DEF_VAR(NCID,Name,NF_DOUBLE,2,VDims,DH%MDVarIDs(NVar))
    call netcdf_err(stat,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) &
'NetCDF error in ',"ext_ncd_put_var_td.code",' ','DOUBLE',', line', 145,' Element ',Element
      call wrf_debug ( WARN , msg)
      return
    endif
  elseif(DH%FileStatus == WRF_FILE_OPENED_FOR_WRITE) then
    do NVar=1,MaxVars
      if(DH%MDVarNames(NVar) == Name) then
        exit
      elseif(DH%MDVarNames(NVar) == NO_NAME) then
        Status = WRF_WARN_MD_NF
        write(msg,*) &
'Warning METADATA NOT FOUND in ',"ext_ncd_put_var_td.code",' ','DOUBLE',', line', 156
        call wrf_debug ( WARN , msg)
        return
      elseif(NVar == MaxVars) then
        Status = WRF_WARN_TOO_MANY_VARIABLES
        write(msg,*) &
'Warning TOO MANY VARIABLES in ',"ext_ncd_put_var_td.code",' ','DOUBLE',', line', 162
        call wrf_debug ( WARN , msg)
        return
      endif
    enddo
    if(Count > DH%MDVarDimLens(NVar)) then
      Status = WRF_WARN_COUNT_TOO_LONG
      write(msg,*) &
'Warning COUNT TOO LONG in ',"ext_ncd_put_var_td.code",' ','DOUBLE',', line', 170
      call wrf_debug ( WARN , msg)
      return
    elseif(Count < 1) then
      Status = WRF_WARN_ZERO_LENGTH_PUT
      write(msg,*) &
'Warning ZERO LENGTH PUT in ',"ext_ncd_put_var_td.code",' ','DOUBLE',', line', 176
      call wrf_debug ( WARN , msg)
      return
    endif
    call GetTimeIndex('write',DataHandle,DateStr,TimeIndex,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) &
'Warning in ',"ext_ncd_put_var_td.code",' ','DOUBLE',', line', 183
      call wrf_debug ( WARN , msg)
      return
    endif
    VStart(1) = 1
    VStart(2) = TimeIndex
    VCount(1) = Count
    VCount(2) = 1
      stat = NF_PUT_VARA_DOUBLE (NCID,DH%MDVarIDs(NVar),VStart,VCount,Data)
    call netcdf_err(stat,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) &
'NetCDF error in ',"ext_ncd_put_var_td.code",' ','DOUBLE',', line', 222,' Element ',Element
      call wrf_debug ( WARN , msg)
      return
    endif
  else
    Status = WRF_ERR_FATAL_BAD_FILE_STATUS
    write(msg,*) &
'Fatal error BAD FILE STATUS in ',"ext_ncd_put_var_td.code",' ','DOUBLE',', line', 229
    call wrf_debug ( FATAL , msg)
    return
  endif
  return
end subroutine ext_ncd_put_var_td_double
subroutine ext_ncd_put_var_ti_integer(DataHandle,Element,Var,Data,Count,Status)
!*------------------------------------------------------------------------------
!* Standard Disclaimer
!*
!* Forecast Systems Laboratory
!* NOAA/OAR/ERL/FSL
!* 325 Broadway
!* Boulder, CO 80303
!*
!* AVIATION DIVISION
!* ADVANCED COMPUTING BRANCH
!* SMS/NNT Version: 2.0.0
!*
!* This software and its documentation are in the public domain and
!* are furnished "as is". The United States government, its
!* instrumentalities, officers, employees, and agents make no
!* warranty, express or implied, as to the usefulness of the software
!* and documentation for any purpose. They assume no
!* responsibility (1) for the use of the software and documentation;
!* or (2) to provide technical support to users.
!*
!* Permission to use, copy, modify, and distribute this software is
!* hereby granted, provided that this disclaimer notice appears in
!* all copies. All modifications to this software must be clearly
!* documented, and are solely the responsibility of the agent making
!* the modification. If significant modifications or enhancements
!* are made to this software, the SMS Development team
!* (sms-info@fsl.noaa.gov) should be notified.
!*
!*----------------------------------------------------------------------------
!*
!* WRF NetCDF I/O
! Author: Jacques Middlecoff jacquesm@fsl.noaa.gov
!* Date: October 6, 2000
!*
!*----------------------------------------------------------------------------
  use wrf_data
  use ext_ncd_support_routines
  implicit none
  include 'wrf_status_codes.h'
  include 'netcdf.inc'
  integer ,intent(in) :: DataHandle
  character*(*) ,intent(in) :: Element
  character*(*) ,intent(in) :: Var
  integer ,intent(in) :: Data(*)
  integer ,intent(in) :: Count
  integer ,intent(out) :: Status
  type(wrf_data_handle) ,pointer :: DH
  character (VarNameLen) :: VarName
  integer :: stat
  integer ,allocatable :: Buffer(:)
  integer :: i
  integer :: NVar
  character*1 :: null
  null=char(0)
  VarName = Var
  call GetDH(DataHandle,DH,Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) &
'Warning Status = ',Status,' in ',"ext_ncd_put_var_ti.code",' ','INTEGER',', line', 61
    call wrf_debug ( WARN , msg)
    return
  endif
  if(DH%FileStatus == WRF_FILE_NOT_OPENED) then
    Status = WRF_WARN_FILE_NOT_OPENED
    write(msg,*) &
'Warning FILE NOT OPENED in ',"ext_ncd_put_var_ti.code",' ','INTEGER',', line', 68
    call wrf_debug ( WARN , msg)
  elseif(DH%FileStatus == WRF_FILE_OPENED_FOR_READ) then
    Status = WRF_WARN_WRITE_RONLY_FILE
    write(msg,*) &
'Warning WRITE READ ONLY FILE in ',"ext_ncd_put_var_ti.code",' ','INTEGER',', line', 73
    call wrf_debug ( WARN , msg)
  elseif(DH%FileStatus == WRF_FILE_OPENED_FOR_WRITE) then
    Status = WRF_WARN_MD_AFTER_OPEN
    write(msg,*) &
'Warning WRITE METADATA AFTER OPEN in ',"ext_ncd_put_var_ti.code",' ','INTEGER',', line', 78
    call wrf_debug ( WARN , msg)
    return
  elseif(DH%FileStatus == WRF_FILE_OPENED_NOT_COMMITTED) then
    do NVar=1,MaxVars
      if(TRIM(DH%VarNames(NVar)) == TRIM(VarName)) then
        exit
      elseif(NVar == MaxVars) then
        Status = WRF_WARN_VAR_NF
        write(msg,*) &
'Warning VARIABLE NOT FOUND in ',"ext_ncd_put_var_ti.code",' ','INTEGER',', line', 88 &
                        ,NVar,VarName
        call wrf_debug ( WARN , msg)
        return
      endif
    enddo
    stat = NF_PUT_ATT_INT(DH%NCID,DH%VarIDs(NVar),trim(Element), NF_INT,Count,Data )
    call netcdf_err(stat,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) &
'NetCDF error for Var ',TRIM(Var),&
        ' Element ',trim(Element),' in ',"ext_ncd_put_var_ti.code",' ','INTEGER',', line', 124
      call wrf_debug ( WARN , msg)
    endif
  else
    Status = WRF_ERR_FATAL_BAD_FILE_STATUS
    write(msg,*) &
'Fatal error BAD FILE STATUS in ',"ext_ncd_put_var_ti.code",' ','INTEGER',', line', 140
    call wrf_debug ( FATAL , msg)
    return
  endif
  return
end subroutine ext_ncd_put_var_ti_integer
subroutine ext_ncd_put_var_td_integer(DataHandle,Element,DateStr,Var,Data,Count,Status)
!*------------------------------------------------------------------------------
!* Standard Disclaimer
!*
!* Forecast Systems Laboratory
!* NOAA/OAR/ERL/FSL
!* 325 Broadway
!* Boulder, CO 80303
!*
!* AVIATION DIVISION
!* ADVANCED COMPUTING BRANCH
!* SMS/NNT Version: 2.0.0
!*
!* This software and its documentation are in the public domain and
!* are furnished "as is". The United States government, its
!* instrumentalities, officers, employees, and agents make no
!* warranty, express or implied, as to the usefulness of the software
!* and documentation for any purpose. They assume no
!* responsibility (1) for the use of the software and documentation;
!* or (2) to provide technical support to users.
!*
!* Permission to use, copy, modify, and distribute this software is
!* hereby granted, provided that this disclaimer notice appears in
!* all copies. All modifications to this software must be clearly
!* documented, and are solely the responsibility of the agent making
!* the modification. If significant modifications or enhancements
!* are made to this software, the SMS Development team
!* (sms-info@fsl.noaa.gov) should be notified.
!*
!*----------------------------------------------------------------------------
!*
!* WRF NetCDF I/O
! Author: Jacques Middlecoff jacquesm@fsl.noaa.gov
!* Date: October 6, 2000
!*
!*----------------------------------------------------------------------------
  use wrf_data
  use ext_ncd_support_routines
  implicit none
  include 'wrf_status_codes.h'
  include 'netcdf.inc'
  integer ,intent(in) :: DataHandle
  character*(*) ,intent(in) :: Element
  character*(*) ,intent(in) :: DateStr
  character*(*) ,intent(in) :: Var
  integer ,intent(in) :: Data(*)
  integer ,intent(in) :: Count
  integer ,intent(out) :: Status
  type(wrf_data_handle) ,pointer :: DH
  character (VarNameLen) :: VarName
  character (40+len(Element)) :: Name
  integer :: stat
  integer :: stat2
  integer ,allocatable :: Buffer(:)
  integer :: i
  integer :: VDims (2)
  integer :: VStart(2)
  integer :: VCount(2)
  integer :: NVar
  integer :: TimeIndex
  integer :: NCID
  VarName = Var
  call DateCheck(DateStr,Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) &
'Warning DATE STRING ERROR in ',"ext_ncd_put_var_td.code",' ','INTEGER',', line', 67
    call wrf_debug ( WARN , msg)
    return
  endif
  call GetDH(DataHandle,DH,Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) &
'Warning Status = ',Status,' in ',"ext_ncd_put_var_td.code",' ','INTEGER',', line', 74
    call wrf_debug ( WARN , msg)
    return
  endif
  NCID = DH%NCID
  call GetName(Element, VarName, Name, Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) &
'Warning Status = ',Status,' in ',"ext_ncd_put_var_td.code",' ','INTEGER',', line', 82
    call wrf_debug ( WARN , msg)
    return
  endif
  if(DH%FileStatus == WRF_FILE_NOT_OPENED) then
    Status = WRF_WARN_FILE_NOT_OPENED
    write(msg,*) &
'Warning FILE NOT OPENED in ',"ext_ncd_put_var_td.code",' ','INTEGER',', line', 89
    call wrf_debug ( WARN , msg)
  elseif(DH%FileStatus == WRF_FILE_OPENED_FOR_READ) then
    Status = WRF_WARN_WRITE_RONLY_FILE
    write(msg,*) &
'Warning WRITE READ ONLY FILE in ',"ext_ncd_put_var_td.code",' ','INTEGER',', line', 94
    call wrf_debug ( WARN , msg)
  elseif(DH%FileStatus == WRF_FILE_OPENED_NOT_COMMITTED) then
    if(Count < 1) then
      Status = WRF_WARN_ZERO_LENGTH_PUT
      return
    endif
    do NVar=1,MaxVars
      if(DH%MDVarNames(NVar) == Name) then
        Status = WRF_WARN_2DRYRUNS_1VARIABLE
        return
      elseif(DH%MDVarNames(NVar) == NO_NAME) then
        DH%MDVarNames(NVar) = Name
        exit
      elseif(NVar == MaxVars) then
        Status = WRF_WARN_TOO_MANY_VARIABLES
        write(msg,*) &
'Warning TOO MANY VARIABLES in ',"ext_ncd_put_var_td.code",' ','INTEGER',', line', 111
        call wrf_debug ( WARN , msg)
        return
      endif
    enddo
    do i=1,MaxDims
      if(DH%DimLengths(i) == Count) then
        exit
      elseif(DH%DimLengths(i) == NO_DIM) then
        stat = NF_DEF_DIM(NCID,DH%DimNames(i),Count,DH%DimIDs(i))
        call netcdf_err(stat,Status)
        if(Status /= WRF_NO_ERR) then
          write(msg,*) &
'NetCDF error in ',"ext_ncd_put_var_td.code",' ','INTEGER',', line', 124,' Element ',Element
          call wrf_debug ( WARN , msg)
          return
        endif
        DH%DimLengths(i) = Count
        exit
      elseif(i == MaxDims) then
        Status = WRF_WARN_TOO_MANY_DIMS
        write(msg,*) &
'Warning TOO MANY DIMENSIONS in ',"ext_ncd_put_var_td.code",' ','INTEGER',', line', 133
        call wrf_debug ( WARN , msg)
        return
      endif
    enddo
    DH%MDVarDimLens(NVar) = Count
    VDims(1) = DH%DimIDs(i)
    VDims(2) = DH%DimUnlimID
    stat = NF_DEF_VAR(NCID,Name,NF_INT,2,VDims,DH%MDVarIDs(NVar))
    call netcdf_err(stat,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) &
'NetCDF error in ',"ext_ncd_put_var_td.code",' ','INTEGER',', line', 145,' Element ',Element
      call wrf_debug ( WARN , msg)
      return
    endif
  elseif(DH%FileStatus == WRF_FILE_OPENED_FOR_WRITE) then
    do NVar=1,MaxVars
      if(DH%MDVarNames(NVar) == Name) then
        exit
      elseif(DH%MDVarNames(NVar) == NO_NAME) then
        Status = WRF_WARN_MD_NF
        write(msg,*) &
'Warning METADATA NOT FOUND in ',"ext_ncd_put_var_td.code",' ','INTEGER',', line', 156
        call wrf_debug ( WARN , msg)
        return
      elseif(NVar == MaxVars) then
        Status = WRF_WARN_TOO_MANY_VARIABLES
        write(msg,*) &
'Warning TOO MANY VARIABLES in ',"ext_ncd_put_var_td.code",' ','INTEGER',', line', 162
        call wrf_debug ( WARN , msg)
        return
      endif
    enddo
    if(Count > DH%MDVarDimLens(NVar)) then
      Status = WRF_WARN_COUNT_TOO_LONG
      write(msg,*) &
'Warning COUNT TOO LONG in ',"ext_ncd_put_var_td.code",' ','INTEGER',', line', 170
      call wrf_debug ( WARN , msg)
      return
    elseif(Count < 1) then
      Status = WRF_WARN_ZERO_LENGTH_PUT
      write(msg,*) &
'Warning ZERO LENGTH PUT in ',"ext_ncd_put_var_td.code",' ','INTEGER',', line', 176
      call wrf_debug ( WARN , msg)
      return
    endif
    call GetTimeIndex('write',DataHandle,DateStr,TimeIndex,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) &
'Warning in ',"ext_ncd_put_var_td.code",' ','INTEGER',', line', 183
      call wrf_debug ( WARN , msg)
      return
    endif
    VStart(1) = 1
    VStart(2) = TimeIndex
    VCount(1) = Count
    VCount(2) = 1
      stat = NF_PUT_VARA_INT (NCID,DH%MDVarIDs(NVar),VStart,VCount,Data)
    call netcdf_err(stat,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) &
'NetCDF error in ',"ext_ncd_put_var_td.code",' ','INTEGER',', line', 222,' Element ',Element
      call wrf_debug ( WARN , msg)
      return
    endif
  else
    Status = WRF_ERR_FATAL_BAD_FILE_STATUS
    write(msg,*) &
'Fatal error BAD FILE STATUS in ',"ext_ncd_put_var_td.code",' ','INTEGER',', line', 229
    call wrf_debug ( FATAL , msg)
    return
  endif
  return
end subroutine ext_ncd_put_var_td_integer
subroutine ext_ncd_put_var_ti_logical(DataHandle,Element,Var,Data,Count,Status)
!*------------------------------------------------------------------------------
!* Standard Disclaimer
!*
!* Forecast Systems Laboratory
!* NOAA/OAR/ERL/FSL
!* 325 Broadway
!* Boulder, CO 80303
!*
!* AVIATION DIVISION
!* ADVANCED COMPUTING BRANCH
!* SMS/NNT Version: 2.0.0
!*
!* This software and its documentation are in the public domain and
!* are furnished "as is". The United States government, its
!* instrumentalities, officers, employees, and agents make no
!* warranty, express or implied, as to the usefulness of the software
!* and documentation for any purpose. They assume no
!* responsibility (1) for the use of the software and documentation;
!* or (2) to provide technical support to users.
!*
!* Permission to use, copy, modify, and distribute this software is
!* hereby granted, provided that this disclaimer notice appears in
!* all copies. All modifications to this software must be clearly
!* documented, and are solely the responsibility of the agent making
!* the modification. If significant modifications or enhancements
!* are made to this software, the SMS Development team
!* (sms-info@fsl.noaa.gov) should be notified.
!*
!*----------------------------------------------------------------------------
!*
!* WRF NetCDF I/O
! Author: Jacques Middlecoff jacquesm@fsl.noaa.gov
!* Date: October 6, 2000
!*
!*----------------------------------------------------------------------------
  use wrf_data
  use ext_ncd_support_routines
  implicit none
  include 'wrf_status_codes.h'
  include 'netcdf.inc'
  integer ,intent(in) :: DataHandle
  character*(*) ,intent(in) :: Element
  character*(*) ,intent(in) :: Var
  logical ,intent(in) :: Data(*)
  integer ,intent(in) :: Count
  integer ,intent(out) :: Status
  type(wrf_data_handle) ,pointer :: DH
  character (VarNameLen) :: VarName
  integer :: stat
  integer ,allocatable :: Buffer(:)
  integer :: i
  integer :: NVar
  character*1 :: null
  null=char(0)
  VarName = Var
  call GetDH(DataHandle,DH,Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) &
'Warning Status = ',Status,' in ',"ext_ncd_put_var_ti.code",' ','LOGICAL',', line', 61
    call wrf_debug ( WARN , msg)
    return
  endif
  if(DH%FileStatus == WRF_FILE_NOT_OPENED) then
    Status = WRF_WARN_FILE_NOT_OPENED
    write(msg,*) &
'Warning FILE NOT OPENED in ',"ext_ncd_put_var_ti.code",' ','LOGICAL',', line', 68
    call wrf_debug ( WARN , msg)
  elseif(DH%FileStatus == WRF_FILE_OPENED_FOR_READ) then
    Status = WRF_WARN_WRITE_RONLY_FILE
    write(msg,*) &
'Warning WRITE READ ONLY FILE in ',"ext_ncd_put_var_ti.code",' ','LOGICAL',', line', 73
    call wrf_debug ( WARN , msg)
  elseif(DH%FileStatus == WRF_FILE_OPENED_FOR_WRITE) then
    Status = WRF_WARN_MD_AFTER_OPEN
    write(msg,*) &
'Warning WRITE METADATA AFTER OPEN in ',"ext_ncd_put_var_ti.code",' ','LOGICAL',', line', 78
    call wrf_debug ( WARN , msg)
    return
  elseif(DH%FileStatus == WRF_FILE_OPENED_NOT_COMMITTED) then
    do NVar=1,MaxVars
      if(TRIM(DH%VarNames(NVar)) == TRIM(VarName)) then
        exit
      elseif(NVar == MaxVars) then
        Status = WRF_WARN_VAR_NF
        write(msg,*) &
'Warning VARIABLE NOT FOUND in ',"ext_ncd_put_var_ti.code",' ','LOGICAL',', line', 88 &
                        ,NVar,VarName
        call wrf_debug ( WARN , msg)
        return
      endif
    enddo
    allocate(Buffer(Count), STAT=stat)
    if(stat/= 0) then
      Status = WRF_ERR_FATAL_ALLOCATION_ERROR
      write(msg,*) &
'Fatal ALLOCATION ERROR in ',"ext_ncd_put_var_ti.code",' ','LOGICAL',', line', 99
      call wrf_debug ( FATAL , msg)
      return
    endif
    do i=1,Count
      if(data(i)) then
         Buffer(i)=1
      else
         Buffer(i)=0
      endif
    enddo
    stat = NF_PUT_ATT_INT(DH%NCID,DH%VarIDs(NVar),trim(Element), NF_INT,Count,Buffer )
    call netcdf_err(stat,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) &
'NetCDF error for Var ',TRIM(Var),&
        ' Element ',trim(Element),' in ',"ext_ncd_put_var_ti.code",' ','LOGICAL',', line', 124
      call wrf_debug ( WARN , msg)
    endif
    deallocate(Buffer, STAT=stat)
    if(stat/= 0) then
      Status = WRF_ERR_FATAL_DEALLOCATION_ERR
      write(msg,*) &
'Fatal DEALLOCATION ERROR in ',"ext_ncd_put_var_ti.code",' ','LOGICAL',', line', 132
      call wrf_debug ( FATAL , msg)
      return
    endif
  else
    Status = WRF_ERR_FATAL_BAD_FILE_STATUS
    write(msg,*) &
'Fatal error BAD FILE STATUS in ',"ext_ncd_put_var_ti.code",' ','LOGICAL',', line', 140
    call wrf_debug ( FATAL , msg)
    return
  endif
  return
end subroutine ext_ncd_put_var_ti_logical
subroutine ext_ncd_put_var_td_logical(DataHandle,Element,DateStr,Var,Data,Count,Status)
!*------------------------------------------------------------------------------
!* Standard Disclaimer
!*
!* Forecast Systems Laboratory
!* NOAA/OAR/ERL/FSL
!* 325 Broadway
!* Boulder, CO 80303
!*
!* AVIATION DIVISION
!* ADVANCED COMPUTING BRANCH
!* SMS/NNT Version: 2.0.0
!*
!* This software and its documentation are in the public domain and
!* are furnished "as is". The United States government, its
!* instrumentalities, officers, employees, and agents make no
!* warranty, express or implied, as to the usefulness of the software
!* and documentation for any purpose. They assume no
!* responsibility (1) for the use of the software and documentation;
!* or (2) to provide technical support to users.
!*
!* Permission to use, copy, modify, and distribute this software is
!* hereby granted, provided that this disclaimer notice appears in
!* all copies. All modifications to this software must be clearly
!* documented, and are solely the responsibility of the agent making
!* the modification. If significant modifications or enhancements
!* are made to this software, the SMS Development team
!* (sms-info@fsl.noaa.gov) should be notified.
!*
!*----------------------------------------------------------------------------
!*
!* WRF NetCDF I/O
! Author: Jacques Middlecoff jacquesm@fsl.noaa.gov
!* Date: October 6, 2000
!*
!*----------------------------------------------------------------------------
  use wrf_data
  use ext_ncd_support_routines
  implicit none
  include 'wrf_status_codes.h'
  include 'netcdf.inc'
  integer ,intent(in) :: DataHandle
  character*(*) ,intent(in) :: Element
  character*(*) ,intent(in) :: DateStr
  character*(*) ,intent(in) :: Var
  logical ,intent(in) :: Data(*)
  integer ,intent(in) :: Count
  integer ,intent(out) :: Status
  type(wrf_data_handle) ,pointer :: DH
  character (VarNameLen) :: VarName
  character (40+len(Element)) :: Name
  integer :: stat
  integer :: stat2
  integer ,allocatable :: Buffer(:)
  integer :: i
  integer :: VDims (2)
  integer :: VStart(2)
  integer :: VCount(2)
  integer :: NVar
  integer :: TimeIndex
  integer :: NCID
  VarName = Var
  call DateCheck(DateStr,Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) &
'Warning DATE STRING ERROR in ',"ext_ncd_put_var_td.code",' ','LOGICAL',', line', 67
    call wrf_debug ( WARN , msg)
    return
  endif
  call GetDH(DataHandle,DH,Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) &
'Warning Status = ',Status,' in ',"ext_ncd_put_var_td.code",' ','LOGICAL',', line', 74
    call wrf_debug ( WARN , msg)
    return
  endif
  NCID = DH%NCID
  call GetName(Element, VarName, Name, Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) &
'Warning Status = ',Status,' in ',"ext_ncd_put_var_td.code",' ','LOGICAL',', line', 82
    call wrf_debug ( WARN , msg)
    return
  endif
  if(DH%FileStatus == WRF_FILE_NOT_OPENED) then
    Status = WRF_WARN_FILE_NOT_OPENED
    write(msg,*) &
'Warning FILE NOT OPENED in ',"ext_ncd_put_var_td.code",' ','LOGICAL',', line', 89
    call wrf_debug ( WARN , msg)
  elseif(DH%FileStatus == WRF_FILE_OPENED_FOR_READ) then
    Status = WRF_WARN_WRITE_RONLY_FILE
    write(msg,*) &
'Warning WRITE READ ONLY FILE in ',"ext_ncd_put_var_td.code",' ','LOGICAL',', line', 94
    call wrf_debug ( WARN , msg)
  elseif(DH%FileStatus == WRF_FILE_OPENED_NOT_COMMITTED) then
    if(Count < 1) then
      Status = WRF_WARN_ZERO_LENGTH_PUT
      return
    endif
    do NVar=1,MaxVars
      if(DH%MDVarNames(NVar) == Name) then
        Status = WRF_WARN_2DRYRUNS_1VARIABLE
        return
      elseif(DH%MDVarNames(NVar) == NO_NAME) then
        DH%MDVarNames(NVar) = Name
        exit
      elseif(NVar == MaxVars) then
        Status = WRF_WARN_TOO_MANY_VARIABLES
        write(msg,*) &
'Warning TOO MANY VARIABLES in ',"ext_ncd_put_var_td.code",' ','LOGICAL',', line', 111
        call wrf_debug ( WARN , msg)
        return
      endif
    enddo
    do i=1,MaxDims
      if(DH%DimLengths(i) == Count) then
        exit
      elseif(DH%DimLengths(i) == NO_DIM) then
        stat = NF_DEF_DIM(NCID,DH%DimNames(i),Count,DH%DimIDs(i))
        call netcdf_err(stat,Status)
        if(Status /= WRF_NO_ERR) then
          write(msg,*) &
'NetCDF error in ',"ext_ncd_put_var_td.code",' ','LOGICAL',', line', 124,' Element ',Element
          call wrf_debug ( WARN , msg)
          return
        endif
        DH%DimLengths(i) = Count
        exit
      elseif(i == MaxDims) then
        Status = WRF_WARN_TOO_MANY_DIMS
        write(msg,*) &
'Warning TOO MANY DIMENSIONS in ',"ext_ncd_put_var_td.code",' ','LOGICAL',', line', 133
        call wrf_debug ( WARN , msg)
        return
      endif
    enddo
    DH%MDVarDimLens(NVar) = Count
    VDims(1) = DH%DimIDs(i)
    VDims(2) = DH%DimUnlimID
    stat = NF_DEF_VAR(NCID,Name,NF_INT,2,VDims,DH%MDVarIDs(NVar))
    call netcdf_err(stat,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) &
'NetCDF error in ',"ext_ncd_put_var_td.code",' ','LOGICAL',', line', 145,' Element ',Element
      call wrf_debug ( WARN , msg)
      return
    endif
  elseif(DH%FileStatus == WRF_FILE_OPENED_FOR_WRITE) then
    do NVar=1,MaxVars
      if(DH%MDVarNames(NVar) == Name) then
        exit
      elseif(DH%MDVarNames(NVar) == NO_NAME) then
        Status = WRF_WARN_MD_NF
        write(msg,*) &
'Warning METADATA NOT FOUND in ',"ext_ncd_put_var_td.code",' ','LOGICAL',', line', 156
        call wrf_debug ( WARN , msg)
        return
      elseif(NVar == MaxVars) then
        Status = WRF_WARN_TOO_MANY_VARIABLES
        write(msg,*) &
'Warning TOO MANY VARIABLES in ',"ext_ncd_put_var_td.code",' ','LOGICAL',', line', 162
        call wrf_debug ( WARN , msg)
        return
      endif
    enddo
    if(Count > DH%MDVarDimLens(NVar)) then
      Status = WRF_WARN_COUNT_TOO_LONG
      write(msg,*) &
'Warning COUNT TOO LONG in ',"ext_ncd_put_var_td.code",' ','LOGICAL',', line', 170
      call wrf_debug ( WARN , msg)
      return
    elseif(Count < 1) then
      Status = WRF_WARN_ZERO_LENGTH_PUT
      write(msg,*) &
'Warning ZERO LENGTH PUT in ',"ext_ncd_put_var_td.code",' ','LOGICAL',', line', 176
      call wrf_debug ( WARN , msg)
      return
    endif
    call GetTimeIndex('write',DataHandle,DateStr,TimeIndex,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) &
'Warning in ',"ext_ncd_put_var_td.code",' ','LOGICAL',', line', 183
      call wrf_debug ( WARN , msg)
      return
    endif
    VStart(1) = 1
    VStart(2) = TimeIndex
    VCount(1) = Count
    VCount(2) = 1
      allocate(Buffer(Count), STAT=stat)
      if(stat/= 0) then
        Status = WRF_ERR_FATAL_ALLOCATION_ERROR
        write(msg,*) &
'Fatal ALLOCATION ERROR in ',"ext_ncd_put_var_td.code",' ','LOGICAL',', line', 196
        call wrf_debug ( FATAL , msg)
        return
      endif
      do i=1,Count
        if(data(i)) then
           Buffer(i)=1
        else
           Buffer(i)=0
        endif
      enddo
      stat = NF_PUT_VARA_INT (NCID,DH%MDVarIDs(NVar),VStart,VCount,Buffer)
      deallocate(Buffer, STAT=stat2)
      if(stat2/= 0) then
        Status = WRF_ERR_FATAL_DEALLOCATION_ERR
        write(msg,*) &
'Fatal DEALLOCATION ERROR in ',"ext_ncd_put_var_td.code",' ','LOGICAL',', line', 212
        call wrf_debug ( FATAL , msg)
        return
      endif
    call netcdf_err(stat,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) &
'NetCDF error in ',"ext_ncd_put_var_td.code",' ','LOGICAL',', line', 222,' Element ',Element
      call wrf_debug ( WARN , msg)
      return
    endif
  else
    Status = WRF_ERR_FATAL_BAD_FILE_STATUS
    write(msg,*) &
'Fatal error BAD FILE STATUS in ',"ext_ncd_put_var_td.code",' ','LOGICAL',', line', 229
    call wrf_debug ( FATAL , msg)
    return
  endif
  return
end subroutine ext_ncd_put_var_td_logical
subroutine ext_ncd_put_var_ti_char(DataHandle,Element,Var,Data,Status)
!*------------------------------------------------------------------------------
!* Standard Disclaimer
!*
!* Forecast Systems Laboratory
!* NOAA/OAR/ERL/FSL
!* 325 Broadway
!* Boulder, CO 80303
!*
!* AVIATION DIVISION
!* ADVANCED COMPUTING BRANCH
!* SMS/NNT Version: 2.0.0
!*
!* This software and its documentation are in the public domain and
!* are furnished "as is". The United States government, its
!* instrumentalities, officers, employees, and agents make no
!* warranty, express or implied, as to the usefulness of the software
!* and documentation for any purpose. They assume no
!* responsibility (1) for the use of the software and documentation;
!* or (2) to provide technical support to users.
!*
!* Permission to use, copy, modify, and distribute this software is
!* hereby granted, provided that this disclaimer notice appears in
!* all copies. All modifications to this software must be clearly
!* documented, and are solely the responsibility of the agent making
!* the modification. If significant modifications or enhancements
!* are made to this software, the SMS Development team
!* (sms-info@fsl.noaa.gov) should be notified.
!*
!*----------------------------------------------------------------------------
!*
!* WRF NetCDF I/O
! Author: Jacques Middlecoff jacquesm@fsl.noaa.gov
!* Date: October 6, 2000
!*
!*----------------------------------------------------------------------------
  use wrf_data
  use ext_ncd_support_routines
  implicit none
  include 'wrf_status_codes.h'
  include 'netcdf.inc'
  integer ,intent(in) :: DataHandle
  character*(*) ,intent(in) :: Element
  character*(*) ,intent(in) :: Var
  character*(*) ,intent(in) :: Data
 
  integer ,intent(out) :: Status
  type(wrf_data_handle) ,pointer :: DH
  character (VarNameLen) :: VarName
  integer :: stat
  integer ,allocatable :: Buffer(:)
  integer :: i
  integer :: NVar
  character*1 :: null
  null=char(0)
  VarName = Var
  call GetDH(DataHandle,DH,Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) &
'Warning Status = ',Status,' in ',"ext_ncd_put_var_ti.code",' ','CHAR',', line', 61
    call wrf_debug ( WARN , msg)
    return
  endif
  if(DH%FileStatus == WRF_FILE_NOT_OPENED) then
    Status = WRF_WARN_FILE_NOT_OPENED
    write(msg,*) &
'Warning FILE NOT OPENED in ',"ext_ncd_put_var_ti.code",' ','CHAR',', line', 68
    call wrf_debug ( WARN , msg)
  elseif(DH%FileStatus == WRF_FILE_OPENED_FOR_READ) then
    Status = WRF_WARN_WRITE_RONLY_FILE
    write(msg,*) &
'Warning WRITE READ ONLY FILE in ',"ext_ncd_put_var_ti.code",' ','CHAR',', line', 73
    call wrf_debug ( WARN , msg)
  elseif(DH%FileStatus == WRF_FILE_OPENED_FOR_WRITE) then
    Status = WRF_WARN_MD_AFTER_OPEN
    write(msg,*) &
'Warning WRITE METADATA AFTER OPEN in ',"ext_ncd_put_var_ti.code",' ','CHAR',', line', 78
    call wrf_debug ( WARN , msg)
    return
  elseif(DH%FileStatus == WRF_FILE_OPENED_NOT_COMMITTED) then
    do NVar=1,MaxVars
      if(TRIM(DH%VarNames(NVar)) == TRIM(VarName)) then
        exit
      elseif(NVar == MaxVars) then
        Status = WRF_WARN_VAR_NF
        write(msg,*) &
'Warning VARIABLE NOT FOUND in ',"ext_ncd_put_var_ti.code",' ','CHAR',', line', 88 &
                        ,NVar,VarName
        call wrf_debug ( WARN , msg)
        return
      endif
    enddo
    if(len_trim(Data).le.0) then
      stat = NF_PUT_ATT_TEXT(DH%NCID,DH%VarIDs(NVar),trim(Element),len_trim(null),null)
    else
      stat = NF_PUT_ATT_TEXT(DH%NCID,DH%VarIDs(NVar),trim(Element), len_trim(Data),trim(Data) )
    endif
    call netcdf_err(stat,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) &
'NetCDF error for Var ',TRIM(Var),&
        ' Element ',trim(Element),' in ',"ext_ncd_put_var_ti.code",' ','CHAR',', line', 124
      call wrf_debug ( WARN , msg)
    endif
  else
    Status = WRF_ERR_FATAL_BAD_FILE_STATUS
    write(msg,*) &
'Fatal error BAD FILE STATUS in ',"ext_ncd_put_var_ti.code",' ','CHAR',', line', 140
    call wrf_debug ( FATAL , msg)
    return
  endif
  return
end subroutine ext_ncd_put_var_ti_char
subroutine ext_ncd_put_var_td_char(DataHandle,Element,DateStr,Var,Data,Status)
!*------------------------------------------------------------------------------
!* Standard Disclaimer
!*
!* Forecast Systems Laboratory
!* NOAA/OAR/ERL/FSL
!* 325 Broadway
!* Boulder, CO 80303
!*
!* AVIATION DIVISION
!* ADVANCED COMPUTING BRANCH
!* SMS/NNT Version: 2.0.0
!*
!* This software and its documentation are in the public domain and
!* are furnished "as is". The United States government, its
!* instrumentalities, officers, employees, and agents make no
!* warranty, express or implied, as to the usefulness of the software
!* and documentation for any purpose. They assume no
!* responsibility (1) for the use of the software and documentation;
!* or (2) to provide technical support to users.
!*
!* Permission to use, copy, modify, and distribute this software is
!* hereby granted, provided that this disclaimer notice appears in
!* all copies. All modifications to this software must be clearly
!* documented, and are solely the responsibility of the agent making
!* the modification. If significant modifications or enhancements
!* are made to this software, the SMS Development team
!* (sms-info@fsl.noaa.gov) should be notified.
!*
!*----------------------------------------------------------------------------
!*
!* WRF NetCDF I/O
! Author: Jacques Middlecoff jacquesm@fsl.noaa.gov
!* Date: October 6, 2000
!*
!*----------------------------------------------------------------------------
  use wrf_data
  use ext_ncd_support_routines
  implicit none
  include 'wrf_status_codes.h'
  include 'netcdf.inc'
  integer ,intent(in) :: DataHandle
  character*(*) ,intent(in) :: Element
  character*(*) ,intent(in) :: DateStr
  character*(*) ,intent(in) :: Var
  character*(*) ,intent(in) :: Data
 
  integer ,intent(out) :: Status
  type(wrf_data_handle) ,pointer :: DH
  character (VarNameLen) :: VarName
  character (40+len(Element)) :: Name
  integer :: stat
  integer :: stat2
  integer ,allocatable :: Buffer(:)
  integer :: i
  integer :: VDims (2)
  integer :: VStart(2)
  integer :: VCount(2)
  integer :: NVar
  integer :: TimeIndex
  integer :: NCID
  VarName = Var
  call DateCheck(DateStr,Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) &
'Warning DATE STRING ERROR in ',"ext_ncd_put_var_td.code",' ','CHAR',', line', 67
    call wrf_debug ( WARN , msg)
    return
  endif
  call GetDH(DataHandle,DH,Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) &
'Warning Status = ',Status,' in ',"ext_ncd_put_var_td.code",' ','CHAR',', line', 74
    call wrf_debug ( WARN , msg)
    return
  endif
  NCID = DH%NCID
  call GetName(Element, VarName, Name, Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) &
'Warning Status = ',Status,' in ',"ext_ncd_put_var_td.code",' ','CHAR',', line', 82
    call wrf_debug ( WARN , msg)
    return
  endif
  if(DH%FileStatus == WRF_FILE_NOT_OPENED) then
    Status = WRF_WARN_FILE_NOT_OPENED
    write(msg,*) &
'Warning FILE NOT OPENED in ',"ext_ncd_put_var_td.code",' ','CHAR',', line', 89
    call wrf_debug ( WARN , msg)
  elseif(DH%FileStatus == WRF_FILE_OPENED_FOR_READ) then
    Status = WRF_WARN_WRITE_RONLY_FILE
    write(msg,*) &
'Warning WRITE READ ONLY FILE in ',"ext_ncd_put_var_td.code",' ','CHAR',', line', 94
    call wrf_debug ( WARN , msg)
  elseif(DH%FileStatus == WRF_FILE_OPENED_NOT_COMMITTED) then
    if(len(Data) < 1) then
      Status = WRF_WARN_ZERO_LENGTH_PUT
      return
    endif
    do NVar=1,MaxVars
      if(DH%MDVarNames(NVar) == Name) then
        Status = WRF_WARN_2DRYRUNS_1VARIABLE
        return
      elseif(DH%MDVarNames(NVar) == NO_NAME) then
        DH%MDVarNames(NVar) = Name
        exit
      elseif(NVar == MaxVars) then
        Status = WRF_WARN_TOO_MANY_VARIABLES
        write(msg,*) &
'Warning TOO MANY VARIABLES in ',"ext_ncd_put_var_td.code",' ','CHAR',', line', 111
        call wrf_debug ( WARN , msg)
        return
      endif
    enddo
    do i=1,MaxDims
      if(DH%DimLengths(i) == len(Data)) then
        exit
      elseif(DH%DimLengths(i) == NO_DIM) then
        stat = NF_DEF_DIM(NCID,DH%DimNames(i),len(Data),DH%DimIDs(i))
        call netcdf_err(stat,Status)
        if(Status /= WRF_NO_ERR) then
          write(msg,*) &
'NetCDF error in ',"ext_ncd_put_var_td.code",' ','CHAR',', line', 124,' Element ',Element
          call wrf_debug ( WARN , msg)
          return
        endif
        DH%DimLengths(i) = len(Data)
        exit
      elseif(i == MaxDims) then
        Status = WRF_WARN_TOO_MANY_DIMS
        write(msg,*) &
'Warning TOO MANY DIMENSIONS in ',"ext_ncd_put_var_td.code",' ','CHAR',', line', 133
        call wrf_debug ( WARN , msg)
        return
      endif
    enddo
    DH%MDVarDimLens(NVar) = len(Data)
    VDims(1) = DH%DimIDs(i)
    VDims(2) = DH%DimUnlimID
    stat = NF_DEF_VAR(NCID,Name,NF_CHAR,2,VDims,DH%MDVarIDs(NVar))
    call netcdf_err(stat,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) &
'NetCDF error in ',"ext_ncd_put_var_td.code",' ','CHAR',', line', 145,' Element ',Element
      call wrf_debug ( WARN , msg)
      return
    endif
  elseif(DH%FileStatus == WRF_FILE_OPENED_FOR_WRITE) then
    do NVar=1,MaxVars
      if(DH%MDVarNames(NVar) == Name) then
        exit
      elseif(DH%MDVarNames(NVar) == NO_NAME) then
        Status = WRF_WARN_MD_NF
        write(msg,*) &
'Warning METADATA NOT FOUND in ',"ext_ncd_put_var_td.code",' ','CHAR',', line', 156
        call wrf_debug ( WARN , msg)
        return
      elseif(NVar == MaxVars) then
        Status = WRF_WARN_TOO_MANY_VARIABLES
        write(msg,*) &
'Warning TOO MANY VARIABLES in ',"ext_ncd_put_var_td.code",' ','CHAR',', line', 162
        call wrf_debug ( WARN , msg)
        return
      endif
    enddo
    if(len(Data) > DH%MDVarDimLens(NVar)) then
      Status = WRF_WARN_COUNT_TOO_LONG
      write(msg,*) &
'Warning COUNT TOO LONG in ',"ext_ncd_put_var_td.code",' ','CHAR',', line', 170
      call wrf_debug ( WARN , msg)
      return
    elseif(len(Data) < 1) then
      Status = WRF_WARN_ZERO_LENGTH_PUT
      write(msg,*) &
'Warning ZERO LENGTH PUT in ',"ext_ncd_put_var_td.code",' ','CHAR',', line', 176
      call wrf_debug ( WARN , msg)
      return
    endif
    call GetTimeIndex('write',DataHandle,DateStr,TimeIndex,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) &
'Warning in ',"ext_ncd_put_var_td.code",' ','CHAR',', line', 183
      call wrf_debug ( WARN , msg)
      return
    endif
    VStart(1) = 1
    VStart(2) = TimeIndex
    VCount(1) = len(Data)
    VCount(2) = 1
      stat = NF_PUT_VARA_TEXT (NCID,DH%MDVarIDs(NVar),VStart,VCount,Data)
    call netcdf_err(stat,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) &
'NetCDF error in ',"ext_ncd_put_var_td.code",' ','CHAR',', line', 222,' Element ',Element
      call wrf_debug ( WARN , msg)
      return
    endif
  else
    Status = WRF_ERR_FATAL_BAD_FILE_STATUS
    write(msg,*) &
'Fatal error BAD FILE STATUS in ',"ext_ncd_put_var_td.code",' ','CHAR',', line', 229
    call wrf_debug ( FATAL , msg)
    return
  endif
  return
end subroutine ext_ncd_put_var_td_char
subroutine ext_ncd_get_var_ti_real(DataHandle,Element,Var,Data,Count,OutCount,Status)
!*------------------------------------------------------------------------------
!* Standard Disclaimer
!*
!* Forecast Systems Laboratory
!* NOAA/OAR/ERL/FSL
!* 325 Broadway
!* Boulder, CO 80303
!*
!* AVIATION DIVISION
!* ADVANCED COMPUTING BRANCH
!* SMS/NNT Version: 2.0.0
!*
!* This software and its documentation are in the public domain and
!* are furnished "as is". The United States government, its
!* instrumentalities, officers, employees, and agents make no
!* warranty, express or implied, as to the usefulness of the software
!* and documentation for any purpose. They assume no
!* responsibility (1) for the use of the software and documentation;
!* or (2) to provide technical support to users.
!*
!* Permission to use, copy, modify, and distribute this software is
!* hereby granted, provided that this disclaimer notice appears in
!* all copies. All modifications to this software must be clearly
!* documented, and are solely the responsibility of the agent making
!* the modification. If significant modifications or enhancements
!* are made to this software, the SMS Development team
!* (sms-info@fsl.noaa.gov) should be notified.
!*
!*----------------------------------------------------------------------------
!*
!* WRF NetCDF I/O
! Author: Jacques Middlecoff jacquesm@fsl.noaa.gov
!* Date: October 6, 2000
!*
!*----------------------------------------------------------------------------
  use wrf_data
  use ext_ncd_support_routines
  implicit none
  include 'wrf_status_codes.h'
  include 'netcdf.inc'
  integer ,intent(in) :: DataHandle
  character*(*) ,intent(in) :: Element
  character*(*) ,intent(in) :: Var
  real ,intent(out) :: Data(*)
  integer,intent(in) :: Count
  integer,intent(out) :: OutCount
  integer ,intent(out) :: Status
  type(wrf_data_handle) ,pointer :: DH
  integer :: XLen
  real ,allocatable :: Buffer(:)
  character (VarNameLen) :: VarName
  integer :: stat
  integer :: NVar
  integer :: XType
  if(Count <= 0) then
    Status = WRF_WARN_ZERO_LENGTH_GET
    write(msg,*) &
'Warning ZERO LENGTH GET in ',"ext_ncd_get_var_ti.code",' ','REAL',', line', 60
    call wrf_debug ( WARN , msg)
    return
  endif
  VarName = Var
  call GetDH(DataHandle,DH,Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) &
'Warning Status = ',Status,' in ',"ext_ncd_get_var_ti.code",' ','REAL',', line', 68
    call wrf_debug ( WARN , msg)
    return
  endif
  if(DH%FileStatus == WRF_FILE_NOT_OPENED) then
    Status = WRF_WARN_FILE_NOT_OPENED
    write(msg,*) &
'Warning FILE NOT OPENED in ',"ext_ncd_get_var_ti.code",' ','REAL',', line', 75
    call wrf_debug ( WARN , msg)
  elseif(DH%FileStatus == WRF_FILE_OPENED_NOT_COMMITTED) then
    Status = WRF_WARN_DRYRUN_READ
    write(msg,*) &
'Warning DRYRUN READ in ',"ext_ncd_get_var_ti.code",' ','REAL',', line', 80
    call wrf_debug ( WARN , msg)
  elseif(DH%FileStatus == WRF_FILE_OPENED_FOR_WRITE) then
    Status = WRF_WARN_READ_WONLY_FILE
    write(msg,*) &
'Warning READ WONLY FILE in ',"ext_ncd_get_var_ti.code",' ','REAL',', line', 85
    call wrf_debug ( WARN , msg)
  elseif(DH%FileStatus == WRF_FILE_OPENED_FOR_READ) then
    do NVar=1,DH%NumVars
      if(DH%VarNames(NVar) == VarName) then
        exit
      elseif(NVar == DH%NumVars) then
        Status = WRF_WARN_VAR_NF
        write(msg,*) &
'Warning VARIABLE NOT FOUND in ',"ext_ncd_get_var_ti.code",' ','REAL',', line', 94
        call wrf_debug ( WARN , msg)
        return
      endif
    enddo
    stat = NF_INQ_ATT(DH%NCID,DH%VarIDs(NVar),trim(Element),XType,XLen)
    call netcdf_err(stat,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) &
'NetCDF error in ',"ext_ncd_get_var_ti.code",' ','REAL',', line', 103,' Element ',Element
      call wrf_debug ( WARN , msg)
    endif
    if ( NF_FLOAT == NF_DOUBLE .OR. NF_FLOAT == NF_FLOAT ) then
      if( .NOT. ( XType==NF_FLOAT .OR. XType==NF_DOUBLE) ) then
        Status = WRF_WARN_TYPE_MISMATCH
        write(msg,*) &
'Warning TYPE MISMATCH in ',"ext_ncd_get_var_ti.code",' ','REAL',', line', 110
        call wrf_debug ( WARN , msg)
        return
      endif
    else
      if(XType /= NF_FLOAT) then
        Status = WRF_WARN_TYPE_MISMATCH
        write(msg,*) &
'Warning TYPE MISMATCH in ',"ext_ncd_get_var_ti.code",' ','REAL',', line', 118
        call wrf_debug ( WARN , msg)
        return
      endif
    endif
    allocate(Buffer(XLen), STAT=stat)
    if(stat/= 0) then
      Status = WRF_ERR_FATAL_ALLOCATION_ERROR
      write(msg,*) &
'Fatal ALLOCATION ERROR in ',"ext_ncd_get_var_ti.code",' ','REAL',', line', 128
      call wrf_debug ( FATAL , msg)
      return
    endif
    stat = NF_GET_ATT_REAL(DH%NCID,DH%VarIDs(NVar),trim(Element), Buffer )
    call netcdf_err(stat,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) &
'NetCDF error in ',"ext_ncd_get_var_ti.code",' ','REAL',', line', 146,' Element ',Element
      call wrf_debug ( WARN , msg)
    endif
    Data(1:min(XLen,Count)) = Buffer(1:min(XLen,Count))
    deallocate(Buffer, STAT=stat)
    if(stat/= 0) then
      Status = WRF_ERR_FATAL_DEALLOCATION_ERR
      write(msg,*) &
'Fatal DEALLOCATION ERROR in ',"ext_ncd_get_var_ti.code",' ','REAL',', line', 155
      call wrf_debug ( FATAL , msg)
      return
    endif
    if(XLen > Count) then
      OutCount = Count
      Status = WRF_WARN_MORE_DATA_IN_FILE
    else
      OutCount = XLen
      Status = WRF_NO_ERR
    endif
  else
    Status = WRF_ERR_FATAL_BAD_FILE_STATUS
    write(msg,*) &
'Fatal error BAD FILE STATUS in ',"ext_ncd_get_var_ti.code",' ','REAL',', line', 170
    call wrf_debug ( FATAL , msg)
    return
  endif
  return
end subroutine ext_ncd_get_var_ti_real
subroutine ext_ncd_get_var_td_real(DataHandle,Element,DateStr,Var,Data,Count,OutCount,Status)
!*------------------------------------------------------------------------------
!* Standard Disclaimer
!*
!* Forecast Systems Laboratory
!* NOAA/OAR/ERL/FSL
!* 325 Broadway
!* Boulder, CO 80303
!*
!* AVIATION DIVISION
!* ADVANCED COMPUTING BRANCH
!* SMS/NNT Version: 2.0.0
!*
!* This software and its documentation are in the public domain and
!* are furnished "as is". The United States government, its
!* instrumentalities, officers, employees, and agents make no
!* warranty, express or implied, as to the usefulness of the software
!* and documentation for any purpose. They assume no
!* responsibility (1) for the use of the software and documentation;
!* or (2) to provide technical support to users.
!*
!* Permission to use, copy, modify, and distribute this software is
!* hereby granted, provided that this disclaimer notice appears in
!* all copies. All modifications to this software must be clearly
!* documented, and are solely the responsibility of the agent making
!* the modification. If significant modifications or enhancements
!* are made to this software, the SMS Development team
!* (sms-info@fsl.noaa.gov) should be notified.
!*
!*----------------------------------------------------------------------------
!*
!* WRF NetCDF I/O
! Author: Jacques Middlecoff jacquesm@fsl.noaa.gov
!* Date: October 6, 2000
!*
!*----------------------------------------------------------------------------
  use wrf_data
  use ext_ncd_support_routines
  implicit none
  include 'netcdf.inc'
  include 'wrf_status_codes.h'
  integer ,intent(in) :: DataHandle
  character*(*) ,intent(in) :: Element
  character (DateStrLen),intent(in) :: DateStr
  character*(*) ,intent(in) :: Var
  real ,intent(out) :: Data(*)
  integer,intent(in) :: Count
  integer,intent(out) :: OutCount
  integer ,intent(out) :: Status
  type(wrf_data_handle) ,pointer :: DH
  character (VarNameLen) :: VarName
  character (40+len(Element)) :: Name
  character (40+len(Element)) :: FName
  integer :: stat
  real ,allocatable :: Buffer(:)
  integer :: i
  integer :: VDims (2)
  integer :: VStart(2)
  integer :: VCount(2)
  integer :: NVar
  integer :: TimeIndex
  integer :: NCID
  integer :: DimIDs(2)
  integer :: VarID
  integer :: XType
  integer :: NDims
  integer :: NAtts
  integer :: Len1
  if(Count <= 0) then
    Status = WRF_WARN_ZERO_LENGTH_GET
    write(msg,*) &
'Warning ZERO LENGTH GET in ',"ext_ncd_get_var_td.code",' ','REAL',', line', 73
    call wrf_debug ( WARN , msg)
    return
  endif
  VarName = Var
  call DateCheck(DateStr,Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) &
'Warning DATE STRING ERROR in ',"ext_ncd_get_var_td.code",' ','REAL',', line', 81
    call wrf_debug ( WARN , msg)
    return
  endif
  call GetDH(DataHandle,DH,Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) &
'Warning Status = ',Status,' in ',"ext_ncd_get_var_td.code",' ','REAL',', line', 88
    call wrf_debug ( WARN , msg)
    return
  endif
  NCID = DH%NCID
  call GetName(Element, VarName, Name, Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) &
'Warning Status = ',Status,' in ',"ext_ncd_get_var_td.code",' ','REAL',', line', 96
    call wrf_debug ( WARN , msg)
    return
  endif
  if(DH%FileStatus == WRF_FILE_NOT_OPENED) then
    Status = WRF_WARN_FILE_NOT_OPENED
    write(msg,*) &
'Warning FILE NOT OPENED in ',"ext_ncd_get_var_td.code",' ','REAL',', line', 103
    call wrf_debug ( WARN , msg)
  elseif(DH%FileStatus == WRF_FILE_OPENED_NOT_COMMITTED) then
    Status = WRF_WARN_DRYRUN_READ
    write(msg,*) &
'Warning DRYRUN READ in ',"ext_ncd_get_var_td.code",' ','REAL',', line', 108
    call wrf_debug ( WARN , msg)
  elseif(DH%FileStatus == WRF_FILE_OPENED_FOR_WRITE) then
    Status = WRF_WARN_READ_WONLY_FILE
    write(msg,*) &
'Warning READ WONLY FILE in ',"ext_ncd_get_var_td.code",' ','REAL',', line', 113
    call wrf_debug ( WARN , msg)
  elseif(DH%FileStatus == WRF_FILE_OPENED_FOR_READ) then
    stat = NF_INQ_VARID(NCID,Name,VarID)
    call netcdf_err(stat,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) &
'NetCDF error in ',"ext_ncd_get_var_td.code",' ','REAL',', line', 120,' Element ',Element
      call wrf_debug ( WARN , msg)
      return
    endif
    stat = NF_INQ_VAR(NCID,VarID,FName,XType,NDims,DimIDs,NAtts)
    call netcdf_err(stat,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) &
'NetCDF error in ',"ext_ncd_get_var_td.code",' ','REAL',', line', 128,' Element ',Element
      call wrf_debug ( WARN , msg)
      return
    endif
    if ( NF_FLOAT == NF_DOUBLE .OR. NF_FLOAT == NF_FLOAT ) then
      if( .NOT. ( XType==NF_FLOAT .OR. XType==NF_DOUBLE) ) then
        Status = WRF_WARN_TYPE_MISMATCH
        write(msg,*) &
'Warning TYPE MISMATCH in ',"ext_ncd_get_var_td.code",' ','REAL',', line', 136
        call wrf_debug ( WARN , msg)
        return
      endif
    else
      if(XType /= NF_FLOAT) then
        Status = WRF_WARN_TYPE_MISMATCH
        write(msg,*) &
'Warning TYPE MISMATCH in ',"ext_ncd_get_var_td.code",' ','REAL',', line', 144
        call wrf_debug ( WARN , msg)
        return
      endif
    endif
    if(NDims /= NMDVarDims) then
      Status = WRF_ERR_FATAL_MDVAR_DIM_NOT_1D
      write(msg,*) &
'Fatal MDVAR DIM NOT 1D in ',"ext_ncd_get_var_td.code",' ','REAL',', line', 152
      call wrf_debug ( FATAL , msg)
      return
    endif
    stat = NF_INQ_DIMLEN(NCID,DimIDs(1),Len1)
    call netcdf_err(stat,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) &
'NetCDF error in ',"ext_ncd_get_var_td.code",' ','REAL',', line', 160,' DimIDs(1) ',DimIDs(1)
      call wrf_debug ( WARN , msg)
      return
    endif
    call GetTimeIndex('read',DataHandle,DateStr,TimeIndex,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) &
'Warning in ',"ext_ncd_get_var_td.code",' ','REAL',', line', 167
      call wrf_debug ( WARN , msg)
      return
    endif
    VStart(1) = 1
    VStart(2) = TimeIndex
    VCount(1) = min(Count,Len1)
    VCount(2) = 1
    allocate(Buffer(VCount(1)), STAT=stat)
    if(stat/= 0) then
      Status = WRF_ERR_FATAL_ALLOCATION_ERROR
      write(msg,*) &
'Fatal ALLOCATION ERROR in ',"ext_ncd_get_var_td.code",' ','REAL',', line', 180
      call wrf_debug ( FATAL , msg)
      return
    endif
    stat = NF_GET_VARA_REAL (NCID,VarID,VStart,VCount,Buffer)
    call netcdf_err(stat,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) &
'NetCDF error in ',"ext_ncd_get_var_td.code",' ','REAL',', line', 199
      call wrf_debug ( WARN , msg)
      return
    endif
    Data(1:min(Len1,Count)) = Buffer(1:min(Len1,Count))
    deallocate(Buffer, STAT=stat)
    if(stat/= 0) then
      Status = WRF_ERR_FATAL_DEALLOCATION_ERR
      write(msg,*) &
'Fatal DEALLOCATION ERROR in ',"ext_ncd_get_var_td.code",' ','REAL',', line', 209
      call wrf_debug ( FATAL , msg)
      return
    endif
    if(Len1 > Count) then
      OutCount = Count
      Status = WRF_WARN_MORE_DATA_IN_FILE
    else
      OutCount = Len1
      Status = WRF_NO_ERR
    endif
  else
    Status = WRF_ERR_FATAL_BAD_FILE_STATUS
    write(msg,*) &
'Fatal error BAD FILE STATUS in ',"ext_ncd_get_var_td.code",' ','REAL',', line', 224
    call wrf_debug ( FATAL , msg)
  endif
  return
end subroutine ext_ncd_get_var_td_real
subroutine ext_ncd_get_var_ti_double(DataHandle,Element,Var,Data,Count,OutCount,Status)
!*------------------------------------------------------------------------------
!* Standard Disclaimer
!*
!* Forecast Systems Laboratory
!* NOAA/OAR/ERL/FSL
!* 325 Broadway
!* Boulder, CO 80303
!*
!* AVIATION DIVISION
!* ADVANCED COMPUTING BRANCH
!* SMS/NNT Version: 2.0.0
!*
!* This software and its documentation are in the public domain and
!* are furnished "as is". The United States government, its
!* instrumentalities, officers, employees, and agents make no
!* warranty, express or implied, as to the usefulness of the software
!* and documentation for any purpose. They assume no
!* responsibility (1) for the use of the software and documentation;
!* or (2) to provide technical support to users.
!*
!* Permission to use, copy, modify, and distribute this software is
!* hereby granted, provided that this disclaimer notice appears in
!* all copies. All modifications to this software must be clearly
!* documented, and are solely the responsibility of the agent making
!* the modification. If significant modifications or enhancements
!* are made to this software, the SMS Development team
!* (sms-info@fsl.noaa.gov) should be notified.
!*
!*----------------------------------------------------------------------------
!*
!* WRF NetCDF I/O
! Author: Jacques Middlecoff jacquesm@fsl.noaa.gov
!* Date: October 6, 2000
!*
!*----------------------------------------------------------------------------
  use wrf_data
  use ext_ncd_support_routines
  implicit none
  include 'wrf_status_codes.h'
  include 'netcdf.inc'
  integer ,intent(in) :: DataHandle
  character*(*) ,intent(in) :: Element
  character*(*) ,intent(in) :: Var
  real*8 ,intent(out) :: Data(*)
  integer,intent(in) :: Count
  integer,intent(out) :: OutCount
  integer ,intent(out) :: Status
  type(wrf_data_handle) ,pointer :: DH
  integer :: XLen
  real*8 ,allocatable :: Buffer(:)
  character (VarNameLen) :: VarName
  integer :: stat
  integer :: NVar
  integer :: XType
  if(Count <= 0) then
    Status = WRF_WARN_ZERO_LENGTH_GET
    write(msg,*) &
'Warning ZERO LENGTH GET in ',"ext_ncd_get_var_ti.code",' ','DOUBLE',', line', 60
    call wrf_debug ( WARN , msg)
    return
  endif
  VarName = Var
  call GetDH(DataHandle,DH,Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) &
'Warning Status = ',Status,' in ',"ext_ncd_get_var_ti.code",' ','DOUBLE',', line', 68
    call wrf_debug ( WARN , msg)
    return
  endif
  if(DH%FileStatus == WRF_FILE_NOT_OPENED) then
    Status = WRF_WARN_FILE_NOT_OPENED
    write(msg,*) &
'Warning FILE NOT OPENED in ',"ext_ncd_get_var_ti.code",' ','DOUBLE',', line', 75
    call wrf_debug ( WARN , msg)
  elseif(DH%FileStatus == WRF_FILE_OPENED_NOT_COMMITTED) then
    Status = WRF_WARN_DRYRUN_READ
    write(msg,*) &
'Warning DRYRUN READ in ',"ext_ncd_get_var_ti.code",' ','DOUBLE',', line', 80
    call wrf_debug ( WARN , msg)
  elseif(DH%FileStatus == WRF_FILE_OPENED_FOR_WRITE) then
    Status = WRF_WARN_READ_WONLY_FILE
    write(msg,*) &
'Warning READ WONLY FILE in ',"ext_ncd_get_var_ti.code",' ','DOUBLE',', line', 85
    call wrf_debug ( WARN , msg)
  elseif(DH%FileStatus == WRF_FILE_OPENED_FOR_READ) then
    do NVar=1,DH%NumVars
      if(DH%VarNames(NVar) == VarName) then
        exit
      elseif(NVar == DH%NumVars) then
        Status = WRF_WARN_VAR_NF
        write(msg,*) &
'Warning VARIABLE NOT FOUND in ',"ext_ncd_get_var_ti.code",' ','DOUBLE',', line', 94
        call wrf_debug ( WARN , msg)
        return
      endif
    enddo
    stat = NF_INQ_ATT(DH%NCID,DH%VarIDs(NVar),trim(Element),XType,XLen)
    call netcdf_err(stat,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) &
'NetCDF error in ',"ext_ncd_get_var_ti.code",' ','DOUBLE',', line', 103,' Element ',Element
      call wrf_debug ( WARN , msg)
    endif
    if ( NF_DOUBLE == NF_DOUBLE .OR. NF_DOUBLE == NF_FLOAT ) then
      if( .NOT. ( XType==NF_FLOAT .OR. XType==NF_DOUBLE) ) then
        Status = WRF_WARN_TYPE_MISMATCH
        write(msg,*) &
'Warning TYPE MISMATCH in ',"ext_ncd_get_var_ti.code",' ','DOUBLE',', line', 110
        call wrf_debug ( WARN , msg)
        return
      endif
    else
      if(XType /= NF_DOUBLE) then
        Status = WRF_WARN_TYPE_MISMATCH
        write(msg,*) &
'Warning TYPE MISMATCH in ',"ext_ncd_get_var_ti.code",' ','DOUBLE',', line', 118
        call wrf_debug ( WARN , msg)
        return
      endif
    endif
    allocate(Buffer(XLen), STAT=stat)
    if(stat/= 0) then
      Status = WRF_ERR_FATAL_ALLOCATION_ERROR
      write(msg,*) &
'Fatal ALLOCATION ERROR in ',"ext_ncd_get_var_ti.code",' ','DOUBLE',', line', 128
      call wrf_debug ( FATAL , msg)
      return
    endif
    stat = NF_GET_ATT_DOUBLE(DH%NCID,DH%VarIDs(NVar),trim(Element), Buffer )
    call netcdf_err(stat,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) &
'NetCDF error in ',"ext_ncd_get_var_ti.code",' ','DOUBLE',', line', 146,' Element ',Element
      call wrf_debug ( WARN , msg)
    endif
    Data(1:min(XLen,Count)) = Buffer(1:min(XLen,Count))
    deallocate(Buffer, STAT=stat)
    if(stat/= 0) then
      Status = WRF_ERR_FATAL_DEALLOCATION_ERR
      write(msg,*) &
'Fatal DEALLOCATION ERROR in ',"ext_ncd_get_var_ti.code",' ','DOUBLE',', line', 155
      call wrf_debug ( FATAL , msg)
      return
    endif
    if(XLen > Count) then
      OutCount = Count
      Status = WRF_WARN_MORE_DATA_IN_FILE
    else
      OutCount = XLen
      Status = WRF_NO_ERR
    endif
  else
    Status = WRF_ERR_FATAL_BAD_FILE_STATUS
    write(msg,*) &
'Fatal error BAD FILE STATUS in ',"ext_ncd_get_var_ti.code",' ','DOUBLE',', line', 170
    call wrf_debug ( FATAL , msg)
    return
  endif
  return
end subroutine ext_ncd_get_var_ti_double
subroutine ext_ncd_get_var_td_double(DataHandle,Element,DateStr,Var,Data,Count,OutCount,Status)
!*------------------------------------------------------------------------------
!* Standard Disclaimer
!*
!* Forecast Systems Laboratory
!* NOAA/OAR/ERL/FSL
!* 325 Broadway
!* Boulder, CO 80303
!*
!* AVIATION DIVISION
!* ADVANCED COMPUTING BRANCH
!* SMS/NNT Version: 2.0.0
!*
!* This software and its documentation are in the public domain and
!* are furnished "as is". The United States government, its
!* instrumentalities, officers, employees, and agents make no
!* warranty, express or implied, as to the usefulness of the software
!* and documentation for any purpose. They assume no
!* responsibility (1) for the use of the software and documentation;
!* or (2) to provide technical support to users.
!*
!* Permission to use, copy, modify, and distribute this software is
!* hereby granted, provided that this disclaimer notice appears in
!* all copies. All modifications to this software must be clearly
!* documented, and are solely the responsibility of the agent making
!* the modification. If significant modifications or enhancements
!* are made to this software, the SMS Development team
!* (sms-info@fsl.noaa.gov) should be notified.
!*
!*----------------------------------------------------------------------------
!*
!* WRF NetCDF I/O
! Author: Jacques Middlecoff jacquesm@fsl.noaa.gov
!* Date: October 6, 2000
!*
!*----------------------------------------------------------------------------
  use wrf_data
  use ext_ncd_support_routines
  implicit none
  include 'netcdf.inc'
  include 'wrf_status_codes.h'
  integer ,intent(in) :: DataHandle
  character*(*) ,intent(in) :: Element
  character (DateStrLen),intent(in) :: DateStr
  character*(*) ,intent(in) :: Var
  real*8 ,intent(out) :: Data(*)
  integer,intent(in) :: Count
  integer,intent(out) :: OutCount
  integer ,intent(out) :: Status
  type(wrf_data_handle) ,pointer :: DH
  character (VarNameLen) :: VarName
  character (40+len(Element)) :: Name
  character (40+len(Element)) :: FName
  integer :: stat
  real*8 ,allocatable :: Buffer(:)
  integer :: i
  integer :: VDims (2)
  integer :: VStart(2)
  integer :: VCount(2)
  integer :: NVar
  integer :: TimeIndex
  integer :: NCID
  integer :: DimIDs(2)
  integer :: VarID
  integer :: XType
  integer :: NDims
  integer :: NAtts
  integer :: Len1
  if(Count <= 0) then
    Status = WRF_WARN_ZERO_LENGTH_GET
    write(msg,*) &
'Warning ZERO LENGTH GET in ',"ext_ncd_get_var_td.code",' ','DOUBLE',', line', 73
    call wrf_debug ( WARN , msg)
    return
  endif
  VarName = Var
  call DateCheck(DateStr,Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) &
'Warning DATE STRING ERROR in ',"ext_ncd_get_var_td.code",' ','DOUBLE',', line', 81
    call wrf_debug ( WARN , msg)
    return
  endif
  call GetDH(DataHandle,DH,Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) &
'Warning Status = ',Status,' in ',"ext_ncd_get_var_td.code",' ','DOUBLE',', line', 88
    call wrf_debug ( WARN , msg)
    return
  endif
  NCID = DH%NCID
  call GetName(Element, VarName, Name, Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) &
'Warning Status = ',Status,' in ',"ext_ncd_get_var_td.code",' ','DOUBLE',', line', 96
    call wrf_debug ( WARN , msg)
    return
  endif
  if(DH%FileStatus == WRF_FILE_NOT_OPENED) then
    Status = WRF_WARN_FILE_NOT_OPENED
    write(msg,*) &
'Warning FILE NOT OPENED in ',"ext_ncd_get_var_td.code",' ','DOUBLE',', line', 103
    call wrf_debug ( WARN , msg)
  elseif(DH%FileStatus == WRF_FILE_OPENED_NOT_COMMITTED) then
    Status = WRF_WARN_DRYRUN_READ
    write(msg,*) &
'Warning DRYRUN READ in ',"ext_ncd_get_var_td.code",' ','DOUBLE',', line', 108
    call wrf_debug ( WARN , msg)
  elseif(DH%FileStatus == WRF_FILE_OPENED_FOR_WRITE) then
    Status = WRF_WARN_READ_WONLY_FILE
    write(msg,*) &
'Warning READ WONLY FILE in ',"ext_ncd_get_var_td.code",' ','DOUBLE',', line', 113
    call wrf_debug ( WARN , msg)
  elseif(DH%FileStatus == WRF_FILE_OPENED_FOR_READ) then
    stat = NF_INQ_VARID(NCID,Name,VarID)
    call netcdf_err(stat,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) &
'NetCDF error in ',"ext_ncd_get_var_td.code",' ','DOUBLE',', line', 120,' Element ',Element
      call wrf_debug ( WARN , msg)
      return
    endif
    stat = NF_INQ_VAR(NCID,VarID,FName,XType,NDims,DimIDs,NAtts)
    call netcdf_err(stat,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) &
'NetCDF error in ',"ext_ncd_get_var_td.code",' ','DOUBLE',', line', 128,' Element ',Element
      call wrf_debug ( WARN , msg)
      return
    endif
    if ( NF_DOUBLE == NF_DOUBLE .OR. NF_DOUBLE == NF_FLOAT ) then
      if( .NOT. ( XType==NF_FLOAT .OR. XType==NF_DOUBLE) ) then
        Status = WRF_WARN_TYPE_MISMATCH
        write(msg,*) &
'Warning TYPE MISMATCH in ',"ext_ncd_get_var_td.code",' ','DOUBLE',', line', 136
        call wrf_debug ( WARN , msg)
        return
      endif
    else
      if(XType /= NF_DOUBLE) then
        Status = WRF_WARN_TYPE_MISMATCH
        write(msg,*) &
'Warning TYPE MISMATCH in ',"ext_ncd_get_var_td.code",' ','DOUBLE',', line', 144
        call wrf_debug ( WARN , msg)
        return
      endif
    endif
    if(NDims /= NMDVarDims) then
      Status = WRF_ERR_FATAL_MDVAR_DIM_NOT_1D
      write(msg,*) &
'Fatal MDVAR DIM NOT 1D in ',"ext_ncd_get_var_td.code",' ','DOUBLE',', line', 152
      call wrf_debug ( FATAL , msg)
      return
    endif
    stat = NF_INQ_DIMLEN(NCID,DimIDs(1),Len1)
    call netcdf_err(stat,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) &
'NetCDF error in ',"ext_ncd_get_var_td.code",' ','DOUBLE',', line', 160,' DimIDs(1) ',DimIDs(1)
      call wrf_debug ( WARN , msg)
      return
    endif
    call GetTimeIndex('read',DataHandle,DateStr,TimeIndex,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) &
'Warning in ',"ext_ncd_get_var_td.code",' ','DOUBLE',', line', 167
      call wrf_debug ( WARN , msg)
      return
    endif
    VStart(1) = 1
    VStart(2) = TimeIndex
    VCount(1) = min(Count,Len1)
    VCount(2) = 1
    allocate(Buffer(VCount(1)), STAT=stat)
    if(stat/= 0) then
      Status = WRF_ERR_FATAL_ALLOCATION_ERROR
      write(msg,*) &
'Fatal ALLOCATION ERROR in ',"ext_ncd_get_var_td.code",' ','DOUBLE',', line', 180
      call wrf_debug ( FATAL , msg)
      return
    endif
    stat = NF_GET_VARA_DOUBLE (NCID,VarID,VStart,VCount,Buffer)
    call netcdf_err(stat,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) &
'NetCDF error in ',"ext_ncd_get_var_td.code",' ','DOUBLE',', line', 199
      call wrf_debug ( WARN , msg)
      return
    endif
    Data(1:min(Len1,Count)) = Buffer(1:min(Len1,Count))
    deallocate(Buffer, STAT=stat)
    if(stat/= 0) then
      Status = WRF_ERR_FATAL_DEALLOCATION_ERR
      write(msg,*) &
'Fatal DEALLOCATION ERROR in ',"ext_ncd_get_var_td.code",' ','DOUBLE',', line', 209
      call wrf_debug ( FATAL , msg)
      return
    endif
    if(Len1 > Count) then
      OutCount = Count
      Status = WRF_WARN_MORE_DATA_IN_FILE
    else
      OutCount = Len1
      Status = WRF_NO_ERR
    endif
  else
    Status = WRF_ERR_FATAL_BAD_FILE_STATUS
    write(msg,*) &
'Fatal error BAD FILE STATUS in ',"ext_ncd_get_var_td.code",' ','DOUBLE',', line', 224
    call wrf_debug ( FATAL , msg)
  endif
  return
end subroutine ext_ncd_get_var_td_double
subroutine ext_ncd_get_var_ti_integer(DataHandle,Element,Var,Data,Count,OutCount,Status)
!*------------------------------------------------------------------------------
!* Standard Disclaimer
!*
!* Forecast Systems Laboratory
!* NOAA/OAR/ERL/FSL
!* 325 Broadway
!* Boulder, CO 80303
!*
!* AVIATION DIVISION
!* ADVANCED COMPUTING BRANCH
!* SMS/NNT Version: 2.0.0
!*
!* This software and its documentation are in the public domain and
!* are furnished "as is". The United States government, its
!* instrumentalities, officers, employees, and agents make no
!* warranty, express or implied, as to the usefulness of the software
!* and documentation for any purpose. They assume no
!* responsibility (1) for the use of the software and documentation;
!* or (2) to provide technical support to users.
!*
!* Permission to use, copy, modify, and distribute this software is
!* hereby granted, provided that this disclaimer notice appears in
!* all copies. All modifications to this software must be clearly
!* documented, and are solely the responsibility of the agent making
!* the modification. If significant modifications or enhancements
!* are made to this software, the SMS Development team
!* (sms-info@fsl.noaa.gov) should be notified.
!*
!*----------------------------------------------------------------------------
!*
!* WRF NetCDF I/O
! Author: Jacques Middlecoff jacquesm@fsl.noaa.gov
!* Date: October 6, 2000
!*
!*----------------------------------------------------------------------------
  use wrf_data
  use ext_ncd_support_routines
  implicit none
  include 'wrf_status_codes.h'
  include 'netcdf.inc'
  integer ,intent(in) :: DataHandle
  character*(*) ,intent(in) :: Element
  character*(*) ,intent(in) :: Var
  integer,intent(out) :: Data(*)
  integer,intent(in) :: Count
  integer,intent(out) :: OutCount
  integer ,intent(out) :: Status
  type(wrf_data_handle) ,pointer :: DH
  integer :: XLen
  integer,allocatable :: Buffer(:)
  character (VarNameLen) :: VarName
  integer :: stat
  integer :: NVar
  integer :: XType
  if(Count <= 0) then
    Status = WRF_WARN_ZERO_LENGTH_GET
    write(msg,*) &
'Warning ZERO LENGTH GET in ',"ext_ncd_get_var_ti.code",' ','INTEGER',', line', 60
    call wrf_debug ( WARN , msg)
    return
  endif
  VarName = Var
  call GetDH(DataHandle,DH,Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) &
'Warning Status = ',Status,' in ',"ext_ncd_get_var_ti.code",' ','INTEGER',', line', 68
    call wrf_debug ( WARN , msg)
    return
  endif
  if(DH%FileStatus == WRF_FILE_NOT_OPENED) then
    Status = WRF_WARN_FILE_NOT_OPENED
    write(msg,*) &
'Warning FILE NOT OPENED in ',"ext_ncd_get_var_ti.code",' ','INTEGER',', line', 75
    call wrf_debug ( WARN , msg)
  elseif(DH%FileStatus == WRF_FILE_OPENED_NOT_COMMITTED) then
    Status = WRF_WARN_DRYRUN_READ
    write(msg,*) &
'Warning DRYRUN READ in ',"ext_ncd_get_var_ti.code",' ','INTEGER',', line', 80
    call wrf_debug ( WARN , msg)
  elseif(DH%FileStatus == WRF_FILE_OPENED_FOR_WRITE) then
    Status = WRF_WARN_READ_WONLY_FILE
    write(msg,*) &
'Warning READ WONLY FILE in ',"ext_ncd_get_var_ti.code",' ','INTEGER',', line', 85
    call wrf_debug ( WARN , msg)
  elseif(DH%FileStatus == WRF_FILE_OPENED_FOR_READ) then
    do NVar=1,DH%NumVars
      if(DH%VarNames(NVar) == VarName) then
        exit
      elseif(NVar == DH%NumVars) then
        Status = WRF_WARN_VAR_NF
        write(msg,*) &
'Warning VARIABLE NOT FOUND in ',"ext_ncd_get_var_ti.code",' ','INTEGER',', line', 94
        call wrf_debug ( WARN , msg)
        return
      endif
    enddo
    stat = NF_INQ_ATT(DH%NCID,DH%VarIDs(NVar),trim(Element),XType,XLen)
    call netcdf_err(stat,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) &
'NetCDF error in ',"ext_ncd_get_var_ti.code",' ','INTEGER',', line', 103,' Element ',Element
      call wrf_debug ( WARN , msg)
    endif
    if ( NF_INT == NF_DOUBLE .OR. NF_INT == NF_FLOAT ) then
      if( .NOT. ( XType==NF_FLOAT .OR. XType==NF_DOUBLE) ) then
        Status = WRF_WARN_TYPE_MISMATCH
        write(msg,*) &
'Warning TYPE MISMATCH in ',"ext_ncd_get_var_ti.code",' ','INTEGER',', line', 110
        call wrf_debug ( WARN , msg)
        return
      endif
    else
      if(XType /= NF_INT) then
        Status = WRF_WARN_TYPE_MISMATCH
        write(msg,*) &
'Warning TYPE MISMATCH in ',"ext_ncd_get_var_ti.code",' ','INTEGER',', line', 118
        call wrf_debug ( WARN , msg)
        return
      endif
    endif
    allocate(Buffer(XLen), STAT=stat)
    if(stat/= 0) then
      Status = WRF_ERR_FATAL_ALLOCATION_ERROR
      write(msg,*) &
'Fatal ALLOCATION ERROR in ',"ext_ncd_get_var_ti.code",' ','INTEGER',', line', 128
      call wrf_debug ( FATAL , msg)
      return
    endif
    stat = NF_GET_ATT_INT(DH%NCID,DH%VarIDs(NVar),trim(Element), Buffer )
    call netcdf_err(stat,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) &
'NetCDF error in ',"ext_ncd_get_var_ti.code",' ','INTEGER',', line', 146,' Element ',Element
      call wrf_debug ( WARN , msg)
    endif
    Data(1:min(XLen,Count)) = Buffer(1:min(XLen,Count))
    deallocate(Buffer, STAT=stat)
    if(stat/= 0) then
      Status = WRF_ERR_FATAL_DEALLOCATION_ERR
      write(msg,*) &
'Fatal DEALLOCATION ERROR in ',"ext_ncd_get_var_ti.code",' ','INTEGER',', line', 155
      call wrf_debug ( FATAL , msg)
      return
    endif
    if(XLen > Count) then
      OutCount = Count
      Status = WRF_WARN_MORE_DATA_IN_FILE
    else
      OutCount = XLen
      Status = WRF_NO_ERR
    endif
  else
    Status = WRF_ERR_FATAL_BAD_FILE_STATUS
    write(msg,*) &
'Fatal error BAD FILE STATUS in ',"ext_ncd_get_var_ti.code",' ','INTEGER',', line', 170
    call wrf_debug ( FATAL , msg)
    return
  endif
  return
end subroutine ext_ncd_get_var_ti_integer
subroutine ext_ncd_get_var_td_integer(DataHandle,Element,DateStr,Var,Data,Count,OutCount,Status)
!*------------------------------------------------------------------------------
!* Standard Disclaimer
!*
!* Forecast Systems Laboratory
!* NOAA/OAR/ERL/FSL
!* 325 Broadway
!* Boulder, CO 80303
!*
!* AVIATION DIVISION
!* ADVANCED COMPUTING BRANCH
!* SMS/NNT Version: 2.0.0
!*
!* This software and its documentation are in the public domain and
!* are furnished "as is". The United States government, its
!* instrumentalities, officers, employees, and agents make no
!* warranty, express or implied, as to the usefulness of the software
!* and documentation for any purpose. They assume no
!* responsibility (1) for the use of the software and documentation;
!* or (2) to provide technical support to users.
!*
!* Permission to use, copy, modify, and distribute this software is
!* hereby granted, provided that this disclaimer notice appears in
!* all copies. All modifications to this software must be clearly
!* documented, and are solely the responsibility of the agent making
!* the modification. If significant modifications or enhancements
!* are made to this software, the SMS Development team
!* (sms-info@fsl.noaa.gov) should be notified.
!*
!*----------------------------------------------------------------------------
!*
!* WRF NetCDF I/O
! Author: Jacques Middlecoff jacquesm@fsl.noaa.gov
!* Date: October 6, 2000
!*
!*----------------------------------------------------------------------------
  use wrf_data
  use ext_ncd_support_routines
  implicit none
  include 'netcdf.inc'
  include 'wrf_status_codes.h'
  integer ,intent(in) :: DataHandle
  character*(*) ,intent(in) :: Element
  character (DateStrLen),intent(in) :: DateStr
  character*(*) ,intent(in) :: Var
  integer,intent(out) :: Data(*)
  integer,intent(in) :: Count
  integer,intent(out) :: OutCount
  integer ,intent(out) :: Status
  type(wrf_data_handle) ,pointer :: DH
  character (VarNameLen) :: VarName
  character (40+len(Element)) :: Name
  character (40+len(Element)) :: FName
  integer :: stat
  integer ,allocatable :: Buffer(:)
  integer :: i
  integer :: VDims (2)
  integer :: VStart(2)
  integer :: VCount(2)
  integer :: NVar
  integer :: TimeIndex
  integer :: NCID
  integer :: DimIDs(2)
  integer :: VarID
  integer :: XType
  integer :: NDims
  integer :: NAtts
  integer :: Len1
  if(Count <= 0) then
    Status = WRF_WARN_ZERO_LENGTH_GET
    write(msg,*) &
'Warning ZERO LENGTH GET in ',"ext_ncd_get_var_td.code",' ','INTEGER',', line', 73
    call wrf_debug ( WARN , msg)
    return
  endif
  VarName = Var
  call DateCheck(DateStr,Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) &
'Warning DATE STRING ERROR in ',"ext_ncd_get_var_td.code",' ','INTEGER',', line', 81
    call wrf_debug ( WARN , msg)
    return
  endif
  call GetDH(DataHandle,DH,Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) &
'Warning Status = ',Status,' in ',"ext_ncd_get_var_td.code",' ','INTEGER',', line', 88
    call wrf_debug ( WARN , msg)
    return
  endif
  NCID = DH%NCID
  call GetName(Element, VarName, Name, Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) &
'Warning Status = ',Status,' in ',"ext_ncd_get_var_td.code",' ','INTEGER',', line', 96
    call wrf_debug ( WARN , msg)
    return
  endif
  if(DH%FileStatus == WRF_FILE_NOT_OPENED) then
    Status = WRF_WARN_FILE_NOT_OPENED
    write(msg,*) &
'Warning FILE NOT OPENED in ',"ext_ncd_get_var_td.code",' ','INTEGER',', line', 103
    call wrf_debug ( WARN , msg)
  elseif(DH%FileStatus == WRF_FILE_OPENED_NOT_COMMITTED) then
    Status = WRF_WARN_DRYRUN_READ
    write(msg,*) &
'Warning DRYRUN READ in ',"ext_ncd_get_var_td.code",' ','INTEGER',', line', 108
    call wrf_debug ( WARN , msg)
  elseif(DH%FileStatus == WRF_FILE_OPENED_FOR_WRITE) then
    Status = WRF_WARN_READ_WONLY_FILE
    write(msg,*) &
'Warning READ WONLY FILE in ',"ext_ncd_get_var_td.code",' ','INTEGER',', line', 113
    call wrf_debug ( WARN , msg)
  elseif(DH%FileStatus == WRF_FILE_OPENED_FOR_READ) then
    stat = NF_INQ_VARID(NCID,Name,VarID)
    call netcdf_err(stat,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) &
'NetCDF error in ',"ext_ncd_get_var_td.code",' ','INTEGER',', line', 120,' Element ',Element
      call wrf_debug ( WARN , msg)
      return
    endif
    stat = NF_INQ_VAR(NCID,VarID,FName,XType,NDims,DimIDs,NAtts)
    call netcdf_err(stat,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) &
'NetCDF error in ',"ext_ncd_get_var_td.code",' ','INTEGER',', line', 128,' Element ',Element
      call wrf_debug ( WARN , msg)
      return
    endif
    if ( NF_INT == NF_DOUBLE .OR. NF_INT == NF_FLOAT ) then
      if( .NOT. ( XType==NF_FLOAT .OR. XType==NF_DOUBLE) ) then
        Status = WRF_WARN_TYPE_MISMATCH
        write(msg,*) &
'Warning TYPE MISMATCH in ',"ext_ncd_get_var_td.code",' ','INTEGER',', line', 136
        call wrf_debug ( WARN , msg)
        return
      endif
    else
      if(XType /= NF_INT) then
        Status = WRF_WARN_TYPE_MISMATCH
        write(msg,*) &
'Warning TYPE MISMATCH in ',"ext_ncd_get_var_td.code",' ','INTEGER',', line', 144
        call wrf_debug ( WARN , msg)
        return
      endif
    endif
    if(NDims /= NMDVarDims) then
      Status = WRF_ERR_FATAL_MDVAR_DIM_NOT_1D
      write(msg,*) &
'Fatal MDVAR DIM NOT 1D in ',"ext_ncd_get_var_td.code",' ','INTEGER',', line', 152
      call wrf_debug ( FATAL , msg)
      return
    endif
    stat = NF_INQ_DIMLEN(NCID,DimIDs(1),Len1)
    call netcdf_err(stat,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) &
'NetCDF error in ',"ext_ncd_get_var_td.code",' ','INTEGER',', line', 160,' DimIDs(1) ',DimIDs(1)
      call wrf_debug ( WARN , msg)
      return
    endif
    call GetTimeIndex('read',DataHandle,DateStr,TimeIndex,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) &
'Warning in ',"ext_ncd_get_var_td.code",' ','INTEGER',', line', 167
      call wrf_debug ( WARN , msg)
      return
    endif
    VStart(1) = 1
    VStart(2) = TimeIndex
    VCount(1) = min(Count,Len1)
    VCount(2) = 1
    allocate(Buffer(VCount(1)), STAT=stat)
    if(stat/= 0) then
      Status = WRF_ERR_FATAL_ALLOCATION_ERROR
      write(msg,*) &
'Fatal ALLOCATION ERROR in ',"ext_ncd_get_var_td.code",' ','INTEGER',', line', 180
      call wrf_debug ( FATAL , msg)
      return
    endif
    stat = NF_GET_VARA_INT (NCID,VarID,VStart,VCount,Buffer)
    call netcdf_err(stat,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) &
'NetCDF error in ',"ext_ncd_get_var_td.code",' ','INTEGER',', line', 199
      call wrf_debug ( WARN , msg)
      return
    endif
    Data(1:min(Len1,Count)) = Buffer(1:min(Len1,Count))
    deallocate(Buffer, STAT=stat)
    if(stat/= 0) then
      Status = WRF_ERR_FATAL_DEALLOCATION_ERR
      write(msg,*) &
'Fatal DEALLOCATION ERROR in ',"ext_ncd_get_var_td.code",' ','INTEGER',', line', 209
      call wrf_debug ( FATAL , msg)
      return
    endif
    if(Len1 > Count) then
      OutCount = Count
      Status = WRF_WARN_MORE_DATA_IN_FILE
    else
      OutCount = Len1
      Status = WRF_NO_ERR
    endif
  else
    Status = WRF_ERR_FATAL_BAD_FILE_STATUS
    write(msg,*) &
'Fatal error BAD FILE STATUS in ',"ext_ncd_get_var_td.code",' ','INTEGER',', line', 224
    call wrf_debug ( FATAL , msg)
  endif
  return
end subroutine ext_ncd_get_var_td_integer
subroutine ext_ncd_get_var_ti_logical(DataHandle,Element,Var,Data,Count,OutCount,Status)
!*------------------------------------------------------------------------------
!* Standard Disclaimer
!*
!* Forecast Systems Laboratory
!* NOAA/OAR/ERL/FSL
!* 325 Broadway
!* Boulder, CO 80303
!*
!* AVIATION DIVISION
!* ADVANCED COMPUTING BRANCH
!* SMS/NNT Version: 2.0.0
!*
!* This software and its documentation are in the public domain and
!* are furnished "as is". The United States government, its
!* instrumentalities, officers, employees, and agents make no
!* warranty, express or implied, as to the usefulness of the software
!* and documentation for any purpose. They assume no
!* responsibility (1) for the use of the software and documentation;
!* or (2) to provide technical support to users.
!*
!* Permission to use, copy, modify, and distribute this software is
!* hereby granted, provided that this disclaimer notice appears in
!* all copies. All modifications to this software must be clearly
!* documented, and are solely the responsibility of the agent making
!* the modification. If significant modifications or enhancements
!* are made to this software, the SMS Development team
!* (sms-info@fsl.noaa.gov) should be notified.
!*
!*----------------------------------------------------------------------------
!*
!* WRF NetCDF I/O
! Author: Jacques Middlecoff jacquesm@fsl.noaa.gov
!* Date: October 6, 2000
!*
!*----------------------------------------------------------------------------
  use wrf_data
  use ext_ncd_support_routines
  implicit none
  include 'wrf_status_codes.h'
  include 'netcdf.inc'
  integer ,intent(in) :: DataHandle
  character*(*) ,intent(in) :: Element
  character*(*) ,intent(in) :: Var
  logical,intent(out) :: Data(*)
  integer,intent(in) :: Count
  integer,intent(out) :: OutCount
  integer ,intent(out) :: Status
  type(wrf_data_handle) ,pointer :: DH
  integer :: XLen
  integer,allocatable :: Buffer(:)
  character (VarNameLen) :: VarName
  integer :: stat
  integer :: NVar
  integer :: XType
  if(Count <= 0) then
    Status = WRF_WARN_ZERO_LENGTH_GET
    write(msg,*) &
'Warning ZERO LENGTH GET in ',"ext_ncd_get_var_ti.code",' ','LOGICAL',', line', 60
    call wrf_debug ( WARN , msg)
    return
  endif
  VarName = Var
  call GetDH(DataHandle,DH,Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) &
'Warning Status = ',Status,' in ',"ext_ncd_get_var_ti.code",' ','LOGICAL',', line', 68
    call wrf_debug ( WARN , msg)
    return
  endif
  if(DH%FileStatus == WRF_FILE_NOT_OPENED) then
    Status = WRF_WARN_FILE_NOT_OPENED
    write(msg,*) &
'Warning FILE NOT OPENED in ',"ext_ncd_get_var_ti.code",' ','LOGICAL',', line', 75
    call wrf_debug ( WARN , msg)
  elseif(DH%FileStatus == WRF_FILE_OPENED_NOT_COMMITTED) then
    Status = WRF_WARN_DRYRUN_READ
    write(msg,*) &
'Warning DRYRUN READ in ',"ext_ncd_get_var_ti.code",' ','LOGICAL',', line', 80
    call wrf_debug ( WARN , msg)
  elseif(DH%FileStatus == WRF_FILE_OPENED_FOR_WRITE) then
    Status = WRF_WARN_READ_WONLY_FILE
    write(msg,*) &
'Warning READ WONLY FILE in ',"ext_ncd_get_var_ti.code",' ','LOGICAL',', line', 85
    call wrf_debug ( WARN , msg)
  elseif(DH%FileStatus == WRF_FILE_OPENED_FOR_READ) then
    do NVar=1,DH%NumVars
      if(DH%VarNames(NVar) == VarName) then
        exit
      elseif(NVar == DH%NumVars) then
        Status = WRF_WARN_VAR_NF
        write(msg,*) &
'Warning VARIABLE NOT FOUND in ',"ext_ncd_get_var_ti.code",' ','LOGICAL',', line', 94
        call wrf_debug ( WARN , msg)
        return
      endif
    enddo
    stat = NF_INQ_ATT(DH%NCID,DH%VarIDs(NVar),trim(Element),XType,XLen)
    call netcdf_err(stat,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) &
'NetCDF error in ',"ext_ncd_get_var_ti.code",' ','LOGICAL',', line', 103,' Element ',Element
      call wrf_debug ( WARN , msg)
    endif
    if ( NF_INT == NF_DOUBLE .OR. NF_INT == NF_FLOAT ) then
      if( .NOT. ( XType==NF_FLOAT .OR. XType==NF_DOUBLE) ) then
        Status = WRF_WARN_TYPE_MISMATCH
        write(msg,*) &
'Warning TYPE MISMATCH in ',"ext_ncd_get_var_ti.code",' ','LOGICAL',', line', 110
        call wrf_debug ( WARN , msg)
        return
      endif
    else
      if(XType /= NF_INT) then
        Status = WRF_WARN_TYPE_MISMATCH
        write(msg,*) &
'Warning TYPE MISMATCH in ',"ext_ncd_get_var_ti.code",' ','LOGICAL',', line', 118
        call wrf_debug ( WARN , msg)
        return
      endif
    endif
    allocate(Buffer(XLen), STAT=stat)
    if(stat/= 0) then
      Status = WRF_ERR_FATAL_ALLOCATION_ERROR
      write(msg,*) &
'Fatal ALLOCATION ERROR in ',"ext_ncd_get_var_ti.code",' ','LOGICAL',', line', 128
      call wrf_debug ( FATAL , msg)
      return
    endif
    stat = NF_GET_ATT_INT(DH%NCID,DH%VarIDs(NVar),trim(Element), Buffer )
    call netcdf_err(stat,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) &
'NetCDF error in ',"ext_ncd_get_var_ti.code",' ','LOGICAL',', line', 146,' Element ',Element
      call wrf_debug ( WARN , msg)
    endif
    Data(1:min(XLen,Count)) = Buffer(1:min(XLen,Count))==1
    deallocate(Buffer, STAT=stat)
    if(stat/= 0) then
      Status = WRF_ERR_FATAL_DEALLOCATION_ERR
      write(msg,*) &
'Fatal DEALLOCATION ERROR in ',"ext_ncd_get_var_ti.code",' ','LOGICAL',', line', 155
      call wrf_debug ( FATAL , msg)
      return
    endif
    if(XLen > Count) then
      OutCount = Count
      Status = WRF_WARN_MORE_DATA_IN_FILE
    else
      OutCount = XLen
      Status = WRF_NO_ERR
    endif
  else
    Status = WRF_ERR_FATAL_BAD_FILE_STATUS
    write(msg,*) &
'Fatal error BAD FILE STATUS in ',"ext_ncd_get_var_ti.code",' ','LOGICAL',', line', 170
    call wrf_debug ( FATAL , msg)
    return
  endif
  return
end subroutine ext_ncd_get_var_ti_logical
subroutine ext_ncd_get_var_td_logical(DataHandle,Element,DateStr,Var,Data,Count,OutCount,Status)
!*------------------------------------------------------------------------------
!* Standard Disclaimer
!*
!* Forecast Systems Laboratory
!* NOAA/OAR/ERL/FSL
!* 325 Broadway
!* Boulder, CO 80303
!*
!* AVIATION DIVISION
!* ADVANCED COMPUTING BRANCH
!* SMS/NNT Version: 2.0.0
!*
!* This software and its documentation are in the public domain and
!* are furnished "as is". The United States government, its
!* instrumentalities, officers, employees, and agents make no
!* warranty, express or implied, as to the usefulness of the software
!* and documentation for any purpose. They assume no
!* responsibility (1) for the use of the software and documentation;
!* or (2) to provide technical support to users.
!*
!* Permission to use, copy, modify, and distribute this software is
!* hereby granted, provided that this disclaimer notice appears in
!* all copies. All modifications to this software must be clearly
!* documented, and are solely the responsibility of the agent making
!* the modification. If significant modifications or enhancements
!* are made to this software, the SMS Development team
!* (sms-info@fsl.noaa.gov) should be notified.
!*
!*----------------------------------------------------------------------------
!*
!* WRF NetCDF I/O
! Author: Jacques Middlecoff jacquesm@fsl.noaa.gov
!* Date: October 6, 2000
!*
!*----------------------------------------------------------------------------
  use wrf_data
  use ext_ncd_support_routines
  implicit none
  include 'netcdf.inc'
  include 'wrf_status_codes.h'
  integer ,intent(in) :: DataHandle
  character*(*) ,intent(in) :: Element
  character (DateStrLen),intent(in) :: DateStr
  character*(*) ,intent(in) :: Var
  logical,intent(out) :: Data(*)
  integer,intent(in) :: Count
  integer,intent(out) :: OutCount
  integer ,intent(out) :: Status
  type(wrf_data_handle) ,pointer :: DH
  character (VarNameLen) :: VarName
  character (40+len(Element)) :: Name
  character (40+len(Element)) :: FName
  integer :: stat
  integer ,allocatable :: Buffer(:)
  integer :: i
  integer :: VDims (2)
  integer :: VStart(2)
  integer :: VCount(2)
  integer :: NVar
  integer :: TimeIndex
  integer :: NCID
  integer :: DimIDs(2)
  integer :: VarID
  integer :: XType
  integer :: NDims
  integer :: NAtts
  integer :: Len1
  if(Count <= 0) then
    Status = WRF_WARN_ZERO_LENGTH_GET
    write(msg,*) &
'Warning ZERO LENGTH GET in ',"ext_ncd_get_var_td.code",' ','LOGICAL',', line', 73
    call wrf_debug ( WARN , msg)
    return
  endif
  VarName = Var
  call DateCheck(DateStr,Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) &
'Warning DATE STRING ERROR in ',"ext_ncd_get_var_td.code",' ','LOGICAL',', line', 81
    call wrf_debug ( WARN , msg)
    return
  endif
  call GetDH(DataHandle,DH,Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) &
'Warning Status = ',Status,' in ',"ext_ncd_get_var_td.code",' ','LOGICAL',', line', 88
    call wrf_debug ( WARN , msg)
    return
  endif
  NCID = DH%NCID
  call GetName(Element, VarName, Name, Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) &
'Warning Status = ',Status,' in ',"ext_ncd_get_var_td.code",' ','LOGICAL',', line', 96
    call wrf_debug ( WARN , msg)
    return
  endif
  if(DH%FileStatus == WRF_FILE_NOT_OPENED) then
    Status = WRF_WARN_FILE_NOT_OPENED
    write(msg,*) &
'Warning FILE NOT OPENED in ',"ext_ncd_get_var_td.code",' ','LOGICAL',', line', 103
    call wrf_debug ( WARN , msg)
  elseif(DH%FileStatus == WRF_FILE_OPENED_NOT_COMMITTED) then
    Status = WRF_WARN_DRYRUN_READ
    write(msg,*) &
'Warning DRYRUN READ in ',"ext_ncd_get_var_td.code",' ','LOGICAL',', line', 108
    call wrf_debug ( WARN , msg)
  elseif(DH%FileStatus == WRF_FILE_OPENED_FOR_WRITE) then
    Status = WRF_WARN_READ_WONLY_FILE
    write(msg,*) &
'Warning READ WONLY FILE in ',"ext_ncd_get_var_td.code",' ','LOGICAL',', line', 113
    call wrf_debug ( WARN , msg)
  elseif(DH%FileStatus == WRF_FILE_OPENED_FOR_READ) then
    stat = NF_INQ_VARID(NCID,Name,VarID)
    call netcdf_err(stat,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) &
'NetCDF error in ',"ext_ncd_get_var_td.code",' ','LOGICAL',', line', 120,' Element ',Element
      call wrf_debug ( WARN , msg)
      return
    endif
    stat = NF_INQ_VAR(NCID,VarID,FName,XType,NDims,DimIDs,NAtts)
    call netcdf_err(stat,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) &
'NetCDF error in ',"ext_ncd_get_var_td.code",' ','LOGICAL',', line', 128,' Element ',Element
      call wrf_debug ( WARN , msg)
      return
    endif
    if ( NF_INT == NF_DOUBLE .OR. NF_INT == NF_FLOAT ) then
      if( .NOT. ( XType==NF_FLOAT .OR. XType==NF_DOUBLE) ) then
        Status = WRF_WARN_TYPE_MISMATCH
        write(msg,*) &
'Warning TYPE MISMATCH in ',"ext_ncd_get_var_td.code",' ','LOGICAL',', line', 136
        call wrf_debug ( WARN , msg)
        return
      endif
    else
      if(XType /= NF_INT) then
        Status = WRF_WARN_TYPE_MISMATCH
        write(msg,*) &
'Warning TYPE MISMATCH in ',"ext_ncd_get_var_td.code",' ','LOGICAL',', line', 144
        call wrf_debug ( WARN , msg)
        return
      endif
    endif
    if(NDims /= NMDVarDims) then
      Status = WRF_ERR_FATAL_MDVAR_DIM_NOT_1D
      write(msg,*) &
'Fatal MDVAR DIM NOT 1D in ',"ext_ncd_get_var_td.code",' ','LOGICAL',', line', 152
      call wrf_debug ( FATAL , msg)
      return
    endif
    stat = NF_INQ_DIMLEN(NCID,DimIDs(1),Len1)
    call netcdf_err(stat,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) &
'NetCDF error in ',"ext_ncd_get_var_td.code",' ','LOGICAL',', line', 160,' DimIDs(1) ',DimIDs(1)
      call wrf_debug ( WARN , msg)
      return
    endif
    call GetTimeIndex('read',DataHandle,DateStr,TimeIndex,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) &
'Warning in ',"ext_ncd_get_var_td.code",' ','LOGICAL',', line', 167
      call wrf_debug ( WARN , msg)
      return
    endif
    VStart(1) = 1
    VStart(2) = TimeIndex
    VCount(1) = min(Count,Len1)
    VCount(2) = 1
    allocate(Buffer(VCount(1)), STAT=stat)
    if(stat/= 0) then
      Status = WRF_ERR_FATAL_ALLOCATION_ERROR
      write(msg,*) &
'Fatal ALLOCATION ERROR in ',"ext_ncd_get_var_td.code",' ','LOGICAL',', line', 180
      call wrf_debug ( FATAL , msg)
      return
    endif
    stat = NF_GET_VARA_INT (NCID,VarID,VStart,VCount,Buffer)
    call netcdf_err(stat,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) &
'NetCDF error in ',"ext_ncd_get_var_td.code",' ','LOGICAL',', line', 199
      call wrf_debug ( WARN , msg)
      return
    endif
    Data(1:min(Len1,Count)) = Buffer(1:min(Len1,Count))==1
    deallocate(Buffer, STAT=stat)
    if(stat/= 0) then
      Status = WRF_ERR_FATAL_DEALLOCATION_ERR
      write(msg,*) &
'Fatal DEALLOCATION ERROR in ',"ext_ncd_get_var_td.code",' ','LOGICAL',', line', 209
      call wrf_debug ( FATAL , msg)
      return
    endif
    if(Len1 > Count) then
      OutCount = Count
      Status = WRF_WARN_MORE_DATA_IN_FILE
    else
      OutCount = Len1
      Status = WRF_NO_ERR
    endif
  else
    Status = WRF_ERR_FATAL_BAD_FILE_STATUS
    write(msg,*) &
'Fatal error BAD FILE STATUS in ',"ext_ncd_get_var_td.code",' ','LOGICAL',', line', 224
    call wrf_debug ( FATAL , msg)
  endif
  return
end subroutine ext_ncd_get_var_td_logical
subroutine ext_ncd_get_var_ti_char(DataHandle,Element,Var,Data,Status)
!*------------------------------------------------------------------------------
!* Standard Disclaimer
!*
!* Forecast Systems Laboratory
!* NOAA/OAR/ERL/FSL
!* 325 Broadway
!* Boulder, CO 80303
!*
!* AVIATION DIVISION
!* ADVANCED COMPUTING BRANCH
!* SMS/NNT Version: 2.0.0
!*
!* This software and its documentation are in the public domain and
!* are furnished "as is". The United States government, its
!* instrumentalities, officers, employees, and agents make no
!* warranty, express or implied, as to the usefulness of the software
!* and documentation for any purpose. They assume no
!* responsibility (1) for the use of the software and documentation;
!* or (2) to provide technical support to users.
!*
!* Permission to use, copy, modify, and distribute this software is
!* hereby granted, provided that this disclaimer notice appears in
!* all copies. All modifications to this software must be clearly
!* documented, and are solely the responsibility of the agent making
!* the modification. If significant modifications or enhancements
!* are made to this software, the SMS Development team
!* (sms-info@fsl.noaa.gov) should be notified.
!*
!*----------------------------------------------------------------------------
!*
!* WRF NetCDF I/O
! Author: Jacques Middlecoff jacquesm@fsl.noaa.gov
!* Date: October 6, 2000
!*
!*----------------------------------------------------------------------------
  use wrf_data
  use ext_ncd_support_routines
  implicit none
  include 'wrf_status_codes.h'
  include 'netcdf.inc'
  integer ,intent(in) :: DataHandle
  character*(*) ,intent(in) :: Element
  character*(*) ,intent(in) :: Var
  character*(*) ,intent(out) :: Data
  integer :: Count = 1
 
  integer ,intent(out) :: Status
  type(wrf_data_handle) ,pointer :: DH
  integer :: XLen
 
  character (VarNameLen) :: VarName
  integer :: stat
  integer :: NVar
  integer :: XType
  if(Count <= 0) then
    Status = WRF_WARN_ZERO_LENGTH_GET
    write(msg,*) &
'Warning ZERO LENGTH GET in ',"ext_ncd_get_var_ti.code",' ','CHAR',', line', 60
    call wrf_debug ( WARN , msg)
    return
  endif
  VarName = Var
  call GetDH(DataHandle,DH,Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) &
'Warning Status = ',Status,' in ',"ext_ncd_get_var_ti.code",' ','CHAR',', line', 68
    call wrf_debug ( WARN , msg)
    return
  endif
  if(DH%FileStatus == WRF_FILE_NOT_OPENED) then
    Status = WRF_WARN_FILE_NOT_OPENED
    write(msg,*) &
'Warning FILE NOT OPENED in ',"ext_ncd_get_var_ti.code",' ','CHAR',', line', 75
    call wrf_debug ( WARN , msg)
  elseif(DH%FileStatus == WRF_FILE_OPENED_NOT_COMMITTED) then
    Status = WRF_WARN_DRYRUN_READ
    write(msg,*) &
'Warning DRYRUN READ in ',"ext_ncd_get_var_ti.code",' ','CHAR',', line', 80
    call wrf_debug ( WARN , msg)
  elseif(DH%FileStatus == WRF_FILE_OPENED_FOR_WRITE) then
    Status = WRF_WARN_READ_WONLY_FILE
    write(msg,*) &
'Warning READ WONLY FILE in ',"ext_ncd_get_var_ti.code",' ','CHAR',', line', 85
    call wrf_debug ( WARN , msg)
  elseif(DH%FileStatus == WRF_FILE_OPENED_FOR_READ) then
    do NVar=1,DH%NumVars
      if(DH%VarNames(NVar) == VarName) then
        exit
      elseif(NVar == DH%NumVars) then
        Status = WRF_WARN_VAR_NF
        write(msg,*) &
'Warning VARIABLE NOT FOUND in ',"ext_ncd_get_var_ti.code",' ','CHAR',', line', 94
        call wrf_debug ( WARN , msg)
        return
      endif
    enddo
    stat = NF_INQ_ATT(DH%NCID,DH%VarIDs(NVar),trim(Element),XType,XLen)
    call netcdf_err(stat,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) &
'NetCDF error in ',"ext_ncd_get_var_ti.code",' ','CHAR',', line', 103,' Element ',Element
      call wrf_debug ( WARN , msg)
    endif
    if ( NF_CHAR == NF_DOUBLE .OR. NF_CHAR == NF_FLOAT ) then
      if( .NOT. ( XType==NF_FLOAT .OR. XType==NF_DOUBLE) ) then
        Status = WRF_WARN_TYPE_MISMATCH
        write(msg,*) &
'Warning TYPE MISMATCH in ',"ext_ncd_get_var_ti.code",' ','CHAR',', line', 110
        call wrf_debug ( WARN , msg)
        return
      endif
    else
      if(XType /= NF_CHAR) then
        Status = WRF_WARN_TYPE_MISMATCH
        write(msg,*) &
'Warning TYPE MISMATCH in ',"ext_ncd_get_var_ti.code",' ','CHAR',', line', 118
        call wrf_debug ( WARN , msg)
        return
      endif
    endif
    if(XLen > len(Data)) then
      Status = WRF_WARN_CHARSTR_GT_LENDATA
      write(msg,*) &
'Warning LEN CHAR STRING > LEN DATA in ',"ext_ncd_get_var_ti.code",' ','CHAR',', line', 137
      call wrf_debug ( WARN , msg)
      return
    endif
    stat = NF_GET_ATT_TEXT(DH%NCID,DH%VarIDs(NVar),trim(Element), Data )
    call netcdf_err(stat,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) &
'NetCDF error in ',"ext_ncd_get_var_ti.code",' ','CHAR',', line', 146,' Element ',Element
      call wrf_debug ( WARN , msg)
    endif
   
  else
    Status = WRF_ERR_FATAL_BAD_FILE_STATUS
    write(msg,*) &
'Fatal error BAD FILE STATUS in ',"ext_ncd_get_var_ti.code",' ','CHAR',', line', 170
    call wrf_debug ( FATAL , msg)
    return
  endif
  return
end subroutine ext_ncd_get_var_ti_char
subroutine ext_ncd_get_var_td_char(DataHandle,Element,DateStr,Var,Data,Status)
!*------------------------------------------------------------------------------
!* Standard Disclaimer
!*
!* Forecast Systems Laboratory
!* NOAA/OAR/ERL/FSL
!* 325 Broadway
!* Boulder, CO 80303
!*
!* AVIATION DIVISION
!* ADVANCED COMPUTING BRANCH
!* SMS/NNT Version: 2.0.0
!*
!* This software and its documentation are in the public domain and
!* are furnished "as is". The United States government, its
!* instrumentalities, officers, employees, and agents make no
!* warranty, express or implied, as to the usefulness of the software
!* and documentation for any purpose. They assume no
!* responsibility (1) for the use of the software and documentation;
!* or (2) to provide technical support to users.
!*
!* Permission to use, copy, modify, and distribute this software is
!* hereby granted, provided that this disclaimer notice appears in
!* all copies. All modifications to this software must be clearly
!* documented, and are solely the responsibility of the agent making
!* the modification. If significant modifications or enhancements
!* are made to this software, the SMS Development team
!* (sms-info@fsl.noaa.gov) should be notified.
!*
!*----------------------------------------------------------------------------
!*
!* WRF NetCDF I/O
! Author: Jacques Middlecoff jacquesm@fsl.noaa.gov
!* Date: October 6, 2000
!*
!*----------------------------------------------------------------------------
  use wrf_data
  use ext_ncd_support_routines
  implicit none
  include 'netcdf.inc'
  include 'wrf_status_codes.h'
  integer ,intent(in) :: DataHandle
  character*(*) ,intent(in) :: Element
  character (DateStrLen),intent(in) :: DateStr
  character*(*) ,intent(in) :: Var
  character*(*) ,intent(out) :: Data
  integer :: Count = 1
 
  integer ,intent(out) :: Status
  type(wrf_data_handle) ,pointer :: DH
  character (VarNameLen) :: VarName
  character (40+len(Element)) :: Name
  character (40+len(Element)) :: FName
  integer :: stat
  character (80) ,allocatable :: Buffer(:)
  integer :: i
  integer :: VDims (2)
  integer :: VStart(2)
  integer :: VCount(2)
  integer :: NVar
  integer :: TimeIndex
  integer :: NCID
  integer :: DimIDs(2)
  integer :: VarID
  integer :: XType
  integer :: NDims
  integer :: NAtts
  integer :: Len1
  if(Count <= 0) then
    Status = WRF_WARN_ZERO_LENGTH_GET
    write(msg,*) &
'Warning ZERO LENGTH GET in ',"ext_ncd_get_var_td.code",' ','CHAR',', line', 73
    call wrf_debug ( WARN , msg)
    return
  endif
  VarName = Var
  call DateCheck(DateStr,Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) &
'Warning DATE STRING ERROR in ',"ext_ncd_get_var_td.code",' ','CHAR',', line', 81
    call wrf_debug ( WARN , msg)
    return
  endif
  call GetDH(DataHandle,DH,Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) &
'Warning Status = ',Status,' in ',"ext_ncd_get_var_td.code",' ','CHAR',', line', 88
    call wrf_debug ( WARN , msg)
    return
  endif
  NCID = DH%NCID
  call GetName(Element, VarName, Name, Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) &
'Warning Status = ',Status,' in ',"ext_ncd_get_var_td.code",' ','CHAR',', line', 96
    call wrf_debug ( WARN , msg)
    return
  endif
  if(DH%FileStatus == WRF_FILE_NOT_OPENED) then
    Status = WRF_WARN_FILE_NOT_OPENED
    write(msg,*) &
'Warning FILE NOT OPENED in ',"ext_ncd_get_var_td.code",' ','CHAR',', line', 103
    call wrf_debug ( WARN , msg)
  elseif(DH%FileStatus == WRF_FILE_OPENED_NOT_COMMITTED) then
    Status = WRF_WARN_DRYRUN_READ
    write(msg,*) &
'Warning DRYRUN READ in ',"ext_ncd_get_var_td.code",' ','CHAR',', line', 108
    call wrf_debug ( WARN , msg)
  elseif(DH%FileStatus == WRF_FILE_OPENED_FOR_WRITE) then
    Status = WRF_WARN_READ_WONLY_FILE
    write(msg,*) &
'Warning READ WONLY FILE in ',"ext_ncd_get_var_td.code",' ','CHAR',', line', 113
    call wrf_debug ( WARN , msg)
  elseif(DH%FileStatus == WRF_FILE_OPENED_FOR_READ) then
    stat = NF_INQ_VARID(NCID,Name,VarID)
    call netcdf_err(stat,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) &
'NetCDF error in ',"ext_ncd_get_var_td.code",' ','CHAR',', line', 120,' Element ',Element
      call wrf_debug ( WARN , msg)
      return
    endif
    stat = NF_INQ_VAR(NCID,VarID,FName,XType,NDims,DimIDs,NAtts)
    call netcdf_err(stat,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) &
'NetCDF error in ',"ext_ncd_get_var_td.code",' ','CHAR',', line', 128,' Element ',Element
      call wrf_debug ( WARN , msg)
      return
    endif
    if ( NF_CHAR == NF_DOUBLE .OR. NF_CHAR == NF_FLOAT ) then
      if( .NOT. ( XType==NF_FLOAT .OR. XType==NF_DOUBLE) ) then
        Status = WRF_WARN_TYPE_MISMATCH
        write(msg,*) &
'Warning TYPE MISMATCH in ',"ext_ncd_get_var_td.code",' ','CHAR',', line', 136
        call wrf_debug ( WARN , msg)
        return
      endif
    else
      if(XType /= NF_CHAR) then
        Status = WRF_WARN_TYPE_MISMATCH
        write(msg,*) &
'Warning TYPE MISMATCH in ',"ext_ncd_get_var_td.code",' ','CHAR',', line', 144
        call wrf_debug ( WARN , msg)
        return
      endif
    endif
    if(NDims /= NMDVarDims) then
      Status = WRF_ERR_FATAL_MDVAR_DIM_NOT_1D
      write(msg,*) &
'Fatal MDVAR DIM NOT 1D in ',"ext_ncd_get_var_td.code",' ','CHAR',', line', 152
      call wrf_debug ( FATAL , msg)
      return
    endif
    stat = NF_INQ_DIMLEN(NCID,DimIDs(1),Len1)
    call netcdf_err(stat,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) &
'NetCDF error in ',"ext_ncd_get_var_td.code",' ','CHAR',', line', 160,' DimIDs(1) ',DimIDs(1)
      call wrf_debug ( WARN , msg)
      return
    endif
    call GetTimeIndex('read',DataHandle,DateStr,TimeIndex,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) &
'Warning in ',"ext_ncd_get_var_td.code",' ','CHAR',', line', 167
      call wrf_debug ( WARN , msg)
      return
    endif
    VStart(1) = 1
    VStart(2) = TimeIndex
    VCount(1) = Len1
    VCount(2) = 1
    if(Len1 > len(Data)) then
      Status = WRF_WARN_CHARSTR_GT_LENDATA
      write(msg,*) &
'Warning LEN CHAR STRING > LEN DATA in ',"ext_ncd_get_var_td.code",' ','CHAR',', line', 189
      call wrf_debug ( WARN , msg)
      return
    endif
    Data = ''
    stat = NF_GET_VARA_TEXT (NCID,VarID,VStart,VCount,Data)
    call netcdf_err(stat,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) &
'NetCDF error in ',"ext_ncd_get_var_td.code",' ','CHAR',', line', 199
      call wrf_debug ( WARN , msg)
      return
    endif
  else
    Status = WRF_ERR_FATAL_BAD_FILE_STATUS
    write(msg,*) &
'Fatal error BAD FILE STATUS in ',"ext_ncd_get_var_td.code",' ','CHAR',', line', 224
    call wrf_debug ( FATAL , msg)
  endif
  return
end subroutine ext_ncd_get_var_td_char
subroutine ext_ncd_put_dom_td_real(DataHandle,Element,DateStr,Data,Count,Status)
  integer ,intent(in) :: DataHandle
  character*(*) ,intent(in) :: Element
  character*(*) ,intent(in) :: DateStr
  real ,intent(in) :: Data(*)
  integer ,intent(in) :: Count
  integer ,intent(out) :: Status
  call ext_ncd_put_var_td_real(DataHandle,Element,DateStr, &
       'E_X_T_D_O_M_A_I_N_M_E_T_A_DATA_' ,Data,Count,Status)
  return
end subroutine ext_ncd_put_dom_td_real
subroutine ext_ncd_put_dom_td_integer(DataHandle,Element,DateStr,Data,Count,Status)
  integer ,intent(in) :: DataHandle
  character*(*) ,intent(in) :: Element
  character*(*) ,intent(in) :: DateStr
  integer ,intent(in) :: Data(*)
  integer ,intent(in) :: Count
  integer ,intent(out) :: Status
  call ext_ncd_put_var_td_integer(DataHandle,Element,DateStr, &
       'E_X_T_D_O_M_A_I_N_M_E_T_A_DATA_' ,Data,Count,Status)
  return
end subroutine ext_ncd_put_dom_td_integer
subroutine ext_ncd_put_dom_td_double(DataHandle,Element,DateStr,Data,Count,Status)
  integer ,intent(in) :: DataHandle
  character*(*) ,intent(in) :: Element
  character*(*) ,intent(in) :: DateStr
  real*8 ,intent(in) :: Data(*)
  integer ,intent(in) :: Count
  integer ,intent(out) :: Status
  call ext_ncd_put_var_td_double(DataHandle,Element,DateStr, &
       'E_X_T_D_O_M_A_I_N_M_E_T_A_DATA_' ,Data,Count,Status)
  return
end subroutine ext_ncd_put_dom_td_double
subroutine ext_ncd_put_dom_td_logical(DataHandle,Element,DateStr,Data,Count,Status)
  integer ,intent(in) :: DataHandle
  character*(*) ,intent(in) :: Element
  character*(*) ,intent(in) :: DateStr
  logical ,intent(in) :: Data(*)
  integer ,intent(in) :: Count
  integer ,intent(out) :: Status
  call ext_ncd_put_var_td_logical(DataHandle,Element,DateStr, &
       'E_X_T_D_O_M_A_I_N_M_E_T_A_DATA_' ,Data,Count,Status)
  return
end subroutine ext_ncd_put_dom_td_logical
subroutine ext_ncd_put_dom_td_char(DataHandle,Element,DateStr,Data,Status)
  integer ,intent(in) :: DataHandle
  character*(*) ,intent(in) :: Element
  character*(*) ,intent(in) :: DateStr
  character*(*) ,intent(in) :: Data
  integer ,intent(out) :: Status
  call ext_ncd_put_var_td_char(DataHandle,Element,DateStr, &
       'E_X_T_D_O_M_A_I_N_M_E_T_A_DATA_' ,Data,Status)
  return
end subroutine ext_ncd_put_dom_td_char
subroutine ext_ncd_get_dom_td_real(DataHandle,Element,DateStr,Data,Count,OutCount,Status)
  integer ,intent(in) :: DataHandle
  character*(*) ,intent(in) :: Element
  character*(*) ,intent(in) :: DateStr
  real ,intent(out) :: Data(*)
  integer ,intent(in) :: Count
  integer ,intent(out) :: OutCount
  integer ,intent(out) :: Status
  call ext_ncd_get_var_td_real(DataHandle,Element,DateStr, &
       'E_X_T_D_O_M_A_I_N_M_E_T_A_DATA_' ,Data,Count,OutCount,Status)
  return
end subroutine ext_ncd_get_dom_td_real
subroutine ext_ncd_get_dom_td_integer(DataHandle,Element,DateStr,Data,Count,OutCount,Status)
  integer ,intent(in) :: DataHandle
  character*(*) ,intent(in) :: Element
  character*(*) ,intent(in) :: DateStr
  integer ,intent(out) :: Data(*)
  integer ,intent(in) :: Count
  integer ,intent(out) :: OutCount
  integer ,intent(out) :: Status
  call ext_ncd_get_var_td_integer(DataHandle,Element,DateStr, &
       'E_X_T_D_O_M_A_I_N_M_E_T_A_DATA_' ,Data,Count,OutCount,Status)
  return
end subroutine ext_ncd_get_dom_td_integer
subroutine ext_ncd_get_dom_td_double(DataHandle,Element,DateStr,Data,Count,OutCount,Status)
  integer ,intent(in) :: DataHandle
  character*(*) ,intent(in) :: Element
  character*(*) ,intent(in) :: DateStr
  real*8 ,intent(out) :: Data(*)
  integer ,intent(in) :: Count
  integer ,intent(out) :: OutCount
  integer ,intent(out) :: Status
  call ext_ncd_get_var_td_double(DataHandle,Element,DateStr, &
       'E_X_T_D_O_M_A_I_N_M_E_T_A_DATA_' ,Data,Count,OutCount,Status)
  return
end subroutine ext_ncd_get_dom_td_double
subroutine ext_ncd_get_dom_td_logical(DataHandle,Element,DateStr,Data,Count,OutCount,Status)
  integer ,intent(in) :: DataHandle
  character*(*) ,intent(in) :: Element
  character*(*) ,intent(in) :: DateStr
  logical ,intent(out) :: Data(*)
  integer ,intent(in) :: Count
  integer ,intent(out) :: OutCount
  integer ,intent(out) :: Status
  call ext_ncd_get_var_td_logical(DataHandle,Element,DateStr, &
       'E_X_T_D_O_M_A_I_N_M_E_T_A_DATA_' ,Data,Count,OutCount,Status)
  return
end subroutine ext_ncd_get_dom_td_logical
subroutine ext_ncd_get_dom_td_char(DataHandle,Element,DateStr,Data,Status)
  integer ,intent(in) :: DataHandle
  character*(*) ,intent(in) :: Element
  character*(*) ,intent(in) :: DateStr
  character*(*) ,intent(out) :: Data
  integer ,intent(out) :: Status
  call ext_ncd_get_var_td_char(DataHandle,Element,DateStr, &
       'E_X_T_D_O_M_A_I_N_M_E_T_A_DATA_' ,Data,Status)
  return
end subroutine ext_ncd_get_dom_td_char
subroutine ext_ncd_write_field(DataHandle,DateStr,Var,Field,FieldTypeIn, &
  Comm, IOComm, DomainDesc, MemoryOrdIn, Stagger, DimNames, &
  DomainStart,DomainEnd,MemoryStart,MemoryEnd,PatchStart,PatchEnd,Status)
  use wrf_data
  use ext_ncd_support_routines
  implicit none
  include 'wrf_status_codes.h'
  include 'netcdf.inc'
  integer ,intent(in) :: DataHandle
  character*(*) ,intent(in) :: DateStr
  character*(*) ,intent(in) :: Var
  integer ,intent(inout) :: Field(*)
  integer ,intent(in) :: FieldTypeIn
  integer ,intent(inout) :: Comm
  integer ,intent(inout) :: IOComm
  integer ,intent(in) :: DomainDesc
  character*(*) ,intent(in) :: MemoryOrdIn
  character*(*) ,intent(in) :: Stagger ! Dummy for now
  character*(*) ,dimension(*) ,intent(in) :: DimNames
  integer ,dimension(*) ,intent(in) :: DomainStart, DomainEnd
  integer ,dimension(*) ,intent(in) :: MemoryStart, MemoryEnd
  integer ,dimension(*) ,intent(in) :: PatchStart, PatchEnd
  integer ,intent(out) :: Status
  integer :: FieldType
  character (3) :: MemoryOrder
  type(wrf_data_handle) ,pointer :: DH
  integer :: NCID
  integer :: NDim
  character (VarNameLen) :: VarName
  character (3) :: MemO
  character (3) :: UCMemO
  integer :: VarID
  integer ,dimension(NVarDims) :: Length
  integer ,dimension(NVarDims) :: VDimIDs
  character(80),dimension(NVarDims) :: RODimNames
  integer ,dimension(NVarDims) :: StoredStart
  integer ,dimension(:,:,:,:),allocatable :: XField
  integer :: stat
  integer :: NVar
  integer :: i,j
  integer :: i1,i2,j1,j2,k1,k2
  integer :: x1,x2,y1,y2,z1,z2
  integer :: l1,l2,m1,m2,n1,n2
  integer :: XType
  integer :: di
  character (80) :: NullName
  logical :: NotFound
  MemoryOrder = trim(adjustl(MemoryOrdIn))
  NullName=char(0)
  call GetDim(MemoryOrder,NDim,Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) 'Warning BAD MEMORY ORDER |',MemoryOrder,'| in ',"wrf_io.F90",', line', 2482
    call wrf_debug ( WARN , TRIM(msg))
    return
  endif
  call DateCheck(DateStr,Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) 'Warning DATE STRING ERROR |',DateStr,'| in ',"wrf_io.F90",', line', 2489
    call wrf_debug ( WARN , TRIM(msg))
    return
  endif
  VarName = Var
  call GetDH(DataHandle,DH,Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) 'Warning Status = ',Status,' in ',"wrf_io.F90",', line', 2496
    call wrf_debug ( WARN , TRIM(msg))
    return
  endif
  NCID = DH%NCID
  if ( DH%R4OnOutput .and. FieldTypeIn == WRF_DOUBLE ) then
     FieldType = WRF_REAL
  else
     FieldType = FieldTypeIn
  end if
  write(msg,*)'ext_ncd_write_field: called for ',TRIM(Var)
!jm 010827 Length(1:NDim) = DomainEnd(1:NDim)-DomainStart(1:NDim)+1
  Length(1:NDim) = PatchEnd(1:NDim)-PatchStart(1:NDim)+1
  IF ( ZeroLengthHorzDim(MemoryOrder,Length,Status) ) THEN
     write(msg,*)'ext_ncd_write_field: zero length dimension in ',TRIM(Var),'. Ignoring'
     call wrf_debug ( WARN , TRIM(msg))
     return
  ENDIF
  call ExtOrder(MemoryOrder,Length,Status)
  call ExtOrderStr(MemoryOrder,DimNames,RODimNames,Status)
  if(DH%FileStatus == WRF_FILE_NOT_OPENED) then
    Status = WRF_WARN_FILE_NOT_OPENED
    write(msg,*) 'Warning FILE NOT OPENED in ',"wrf_io.F90",', line', 2533
    call wrf_debug ( WARN , TRIM(msg))
  elseif(DH%FileStatus == WRF_FILE_OPENED_FOR_READ) then
    Status = WRF_WARN_WRITE_RONLY_FILE
    write(msg,*) 'Warning WRITE READ ONLY FILE in ',"wrf_io.F90",', line', 2537
    call wrf_debug ( WARN , TRIM(msg))
  elseif(DH%FileStatus == WRF_FILE_OPENED_NOT_COMMITTED) then
    do NVar=1,MaxVars
      if(DH%VarNames(NVar) == VarName ) then
        Status = WRF_WARN_2DRYRUNS_1VARIABLE
        write(msg,*) 'Warning 2 DRYRUNS 1 VARIABLE in ',"wrf_io.F90",', line', 2543
        call wrf_debug ( WARN , TRIM(msg))
        return
      elseif(DH%VarNames(NVar) == NO_NAME) then
        DH%VarNames(NVar) = VarName
        DH%NumVars = NVar
        exit
      elseif(NVar == MaxVars) then
        Status = WRF_WARN_TOO_MANY_VARIABLES
        write(msg,*) 'Warning TOO MANY VARIABLES in ',"wrf_io.F90",', line', 2552
        call wrf_debug ( WARN , TRIM(msg))
        return
      endif
    enddo
    do j = 1,NDim
      if(RODimNames(j) == NullName .or. RODimNames(j) == '') then
        do i=1,MaxDims
          if(DH%DimLengths(i) == Length(j)) then
            exit
          elseif(DH%DimLengths(i) == NO_DIM) then
            stat = NF_DEF_DIM(NCID,DH%DimNames(i),Length(j),DH%DimIDs(i))
            call netcdf_err(stat,Status)
            if(Status /= WRF_NO_ERR) then
              write(msg,*) 'NetCDF error in ',"wrf_io.F90",', line', 2566
              call wrf_debug ( WARN , TRIM(msg))
              return
            endif
            DH%DimLengths(i) = Length(j)
            exit
          elseif(i == MaxDims) then
            Status = WRF_WARN_TOO_MANY_DIMS
            write(msg,*) 'Warning TOO MANY DIMENSIONS in ',"wrf_io.F90",', line', 2574
            call wrf_debug ( WARN , TRIM(msg))
            return
          endif
        enddo
      else !look for input name and check if already defined
        NotFound = .true.
        do i=1,MaxDims
          if (DH%DimNames(i) == RODimNames(j)) then
            if (DH%DimLengths(i) == Length(j)) then
              NotFound = .false.
              exit
            else
              Status = WRF_WARN_DIMNAME_REDEFINED
              write(msg,*) 'Warning DIM ',i,', NAME ',TRIM(DH%DimNames(i)),' REDEFINED  by var ', &
                           TRIM(Var),' ',DH%DimLengths(i),Length(j) ,' in ', "wrf_io.F90" ,' line', 2589
              call wrf_debug ( WARN , TRIM(msg))
              return
            endif
          endif
        enddo
        if (NotFound) then
          do i=1,MaxDims
            if (DH%DimLengths(i) == NO_DIM) then
              DH%DimNames(i) = RODimNames(j)
              stat = NF_DEF_DIM(NCID,DH%DimNames(i),Length(j),DH%DimIDs(i))
              call netcdf_err(stat,Status)
              if(Status /= WRF_NO_ERR) then
                write(msg,*) 'NetCDF error in ',"wrf_io.F90",', line', 2602
                call wrf_debug ( WARN , TRIM(msg))
                return
              endif
              DH%DimLengths(i) = Length(j)
              exit
            elseif(i == MaxDims) then
              Status = WRF_WARN_TOO_MANY_DIMS
              write(msg,*) 'Warning TOO MANY DIMENSIONS in ',"wrf_io.F90",', line', 2610
              call wrf_debug ( WARN , TRIM(msg))
              return
            endif
          enddo
        endif
      endif
      VDimIDs(j) = DH%DimIDs(i)
      DH%VarDimLens(j,NVar) = Length(j)
    enddo
    VDimIDs(NDim+1) = DH%DimUnlimID
    ! Do not use SELECT statement here as sometimes WRF_REAL=WRF_DOUBLE
    IF (FieldType == WRF_REAL) THEN
      XType = NF_FLOAT
    ELSE IF (FieldType == WRF_DOUBLE) THEN
      Xtype = NF_DOUBLE
    ELSE IF (FieldType == WRF_INTEGER) THEN
      XType = NF_INT
    ELSE IF (FieldType == WRF_LOGICAL) THEN
      XType = NF_INT
    ELSE
        Status = WRF_WARN_DATA_TYPE_NOT_FOUND
        write(msg,*) 'Warning DATA TYPE NOT FOUND in ',"wrf_io.F90",', line', 2633
        call wrf_debug ( WARN , TRIM(msg))
        return
    END IF
    stat = NF_DEF_VAR(NCID,VarName,XType,NDim+1,VDimIDs,VarID)
    call netcdf_err(stat,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) 'ext_ncd_write_field: NetCDF error for ',TRIM(VarName),' in ',"wrf_io.F90",', line', 2641
      call wrf_debug ( WARN , TRIM(msg))
      return
    endif
    DH%VarIDs(NVar) = VarID
    stat = NF_PUT_ATT_INT(NCID,VarID,'FieldType',NF_INT,1,FieldType)
    call netcdf_err(stat,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) 'ext_ncd_write_field: NetCDF error in ',"wrf_io.F90",', line', 2697
      call wrf_debug ( WARN , TRIM(msg))
      return
    endif
    call reorder(MemoryOrder,MemO)
    call uppercase(MemO,UCMemO)
    stat = NF_PUT_ATT_TEXT(NCID,VarID,'MemoryOrder',3,UCMemO)
    call netcdf_err(stat,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) 'ext_ncd_write_field: NetCDF error in ',"wrf_io.F90",', line', 2706
      call wrf_debug ( WARN , TRIM(msg))
      return
    endif
  elseif(DH%FileStatus == WRF_FILE_OPENED_FOR_WRITE .OR. DH%FileStatus == WRF_FILE_OPENED_FOR_UPDATE) then
    do NVar=1,DH%NumVars
      if(DH%VarNames(NVar) == VarName) then
        exit
      elseif(NVar == DH%NumVars) then
        Status = WRF_WARN_VAR_NF
        write(msg,*) 'Warning VARIABLE NOT FOUND in ',"wrf_io.F90",', line', 2716
        call wrf_debug ( WARN , TRIM(msg))
        return
      endif
    enddo
    VarID = DH%VarIDs(NVar)
    do j=1,NDim
      if(Length(j) /= DH%VarDimLens(j,NVar) .AND. DH%FileStatus /= WRF_FILE_OPENED_FOR_UPDATE ) then
        Status = WRF_WARN_WRTLEN_NE_DRRUNLEN
        write(msg,*) 'Warning LENGTH != DRY RUN LENGTH for |', &
                     VarName,'| dim ',j,' in ',"wrf_io.F90",', line', 2726
        call wrf_debug ( WARN , TRIM(msg))
        write(msg,*) '   LENGTH ',Length(j),' DRY RUN LENGTH ',DH%VarDimLens(j,NVar)
        call wrf_debug ( WARN , TRIM(msg))
        return
!jm 010825 elseif(DomainStart(j) < MemoryStart(j)) then
      elseif(PatchStart(j) < MemoryStart(j)) then
        Status = WRF_WARN_DIMENSION_ERROR
        write(msg,*) 'Warning DIMENSION ERROR for |',VarName, &
                     '| in ',"wrf_io.F90",', line', 2735
        call wrf_debug ( WARN , TRIM(msg))
        return
      endif
    enddo
    StoredStart = 1
    call GetIndices(NDim,MemoryStart,MemoryEnd,l1,l2,m1,m2,n1,n2)
    call GetIndices(NDim,StoredStart,Length ,x1,x2,y1,y2,z1,z2)
    call GetIndices(NDim,PatchStart, PatchEnd ,i1,i2,j1,j2,k1,k2)
    di=1
    if(FieldType == WRF_DOUBLE) di=2
    allocate(XField(di,x1:x2,y1:y2,z1:z2), STAT=stat)
    if(stat/= 0) then
      Status = WRF_ERR_FATAL_ALLOCATION_ERROR
      write(msg,*) 'Fatal ALLOCATION ERROR in ',"wrf_io.F90",', line', 2749
      call wrf_debug ( FATAL , TRIM(msg))
      return
    endif
    if (DH%R4OnOutput .and. FieldTypeIn == WRF_DOUBLE) then
       call TransposeToR4('write',MemoryOrder,di, Field,l1,l2,m1,m2,n1,n2 &
                                                ,XField,x1,x2,y1,y2,z1,z2 &
                                                   ,i1,i2,j1,j2,k1,k2 )
    else
       call Transpose('write',MemoryOrder,di, Field,l1,l2,m1,m2,n1,n2 &
                                            ,XField,x1,x2,y1,y2,z1,z2 &
                                                   ,i1,i2,j1,j2,k1,k2 )
    end if
    call FieldIO('write',DataHandle,DateStr,Length,MemoryOrder, &
                  FieldType,NCID,VarID,XField,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) 'Warning Status = ',Status,' in ',"wrf_io.F90",', line', 2765
      call wrf_debug ( WARN , TRIM(msg))
      return
    endif
    deallocate(XField, STAT=stat)
    if(stat/= 0) then
      Status = WRF_ERR_FATAL_DEALLOCATION_ERR
      write(msg,*) 'Fatal DEALLOCATION ERROR in ',"wrf_io.F90",', line', 2772
      call wrf_debug ( FATAL , TRIM(msg))
      return
    endif
  else
    Status = WRF_ERR_FATAL_BAD_FILE_STATUS
    write(msg,*) 'Fatal error BAD FILE STATUS in ',"wrf_io.F90",', line', 2778
    call wrf_debug ( FATAL , TRIM(msg))
  endif
  DH%first_operation = .FALSE.
  return
end subroutine ext_ncd_write_field
subroutine ext_ncd_read_field(DataHandle,DateStr,Var,Field,FieldType,Comm, &
  IOComm, DomainDesc, MemoryOrdIn, Stagger, DimNames, &
  DomainStart,DomainEnd,MemoryStart,MemoryEnd,PatchStart,PatchEnd,Status)
  use wrf_data
  use ext_ncd_support_routines
  implicit none
  include 'wrf_status_codes.h'
  include 'netcdf.inc'
  integer ,intent(in) :: DataHandle
  character*(*) ,intent(in) :: DateStr
  character*(*) ,intent(in) :: Var
  integer ,intent(out) :: Field(*)
  integer ,intent(in) :: FieldType
  integer ,intent(inout) :: Comm
  integer ,intent(inout) :: IOComm
  integer ,intent(in) :: DomainDesc
  character*(*) ,intent(in) :: MemoryOrdIn
  character*(*) ,intent(in) :: Stagger ! Dummy for now
  character*(*) , dimension (*) ,intent(in) :: DimNames
  integer ,dimension(*) ,intent(in) :: DomainStart, DomainEnd
  integer ,dimension(*) ,intent(in) :: MemoryStart, MemoryEnd
  integer ,dimension(*) ,intent(in) :: PatchStart, PatchEnd
  integer ,intent(out) :: Status
  character (3) :: MemoryOrder
  character (NF_MAX_NAME) :: dimname
  type(wrf_data_handle) ,pointer :: DH
  integer :: NDim
  integer :: NCID
  character (VarNameLen) :: VarName
  integer :: VarID
  integer ,dimension(NVarDims) :: VCount
  integer ,dimension(NVarDims) :: VStart
  integer ,dimension(NVarDims) :: Length
  integer ,dimension(NVarDims) :: VDimIDs
  integer ,dimension(NVarDims) :: MemS
  integer ,dimension(NVarDims) :: MemE
  integer ,dimension(NVarDims) :: StoredStart
  integer ,dimension(NVarDims) :: StoredLen
  integer ,dimension(:,:,:,:) ,allocatable :: XField
  integer :: NVar
  integer :: j
  integer :: i1,i2,j1,j2,k1,k2
  integer :: x1,x2,y1,y2,z1,z2
  integer :: l1,l2,m1,m2,n1,n2
  character (VarNameLen) :: Name
  integer :: XType
  integer :: StoredDim
  integer :: NAtts
  integer :: Len
  integer :: stat
  integer :: di
  integer :: FType
  MemoryOrder = trim(adjustl(MemoryOrdIn))
  call GetDim(MemoryOrder,NDim,Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) 'Warning BAD MEMORY ORDER |',TRIM(MemoryOrder),'| for |', &
                 TRIM(Var),'| in ext_ncd_read_field ',"wrf_io.F90",', line', 2842
    call wrf_debug ( WARN , TRIM(msg))
    return
  endif
  call DateCheck(DateStr,Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) 'Warning DATE STRING ERROR |',TRIM(DateStr),'| for |',TRIM(Var), &
                 '| in ext_ncd_read_field ',"wrf_io.F90",', line', 2849
    call wrf_debug ( WARN , TRIM(msg))
    return
  endif
  VarName = Var
  call GetDH(DataHandle,DH,Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) 'Warning Status = ',Status,' in ext_ncd_read_field ',"wrf_io.F90",', line', 2856
    call wrf_debug ( WARN , TRIM(msg))
    return
  endif
  if(DH%FileStatus == WRF_FILE_NOT_OPENED) then
    Status = WRF_WARN_FILE_NOT_OPENED
    write(msg,*) 'Warning FILE NOT OPENED in ',"wrf_io.F90",', line', 2862
    call wrf_debug ( WARN , TRIM(msg))
  elseif(DH%FileStatus == WRF_FILE_OPENED_NOT_COMMITTED) then
! jm it is okay to have a dry run read. means read is called between ofrb and ofrc. Just return.
! Status = WRF_WARN_DRYRUN_READ
! write(msg,*) 'Warning DRYRUN READ in ',"wrf_io.F90",', line', 2867
! call wrf_debug ( WARN , TRIM(msg))
    Status = WRF_NO_ERR
    RETURN
  elseif(DH%FileStatus == WRF_FILE_OPENED_FOR_WRITE) then
    Status = WRF_WARN_READ_WONLY_FILE
    write(msg,*) 'Warning READ WRITE ONLY FILE in ',"wrf_io.F90",', line', 2873
    call wrf_debug ( WARN , TRIM(msg))
  elseif(DH%FileStatus == WRF_FILE_OPENED_FOR_READ .OR. DH%FileStatus == WRF_FILE_OPENED_FOR_UPDATE ) then
    NCID = DH%NCID
!jm Length(1:NDim) = DomainEnd(1:NDim)-DomainStart(1:NDim)+1
    Length(1:NDim) = PatchEnd(1:NDim)-PatchStart(1:NDim)+1
    call ExtOrder(MemoryOrder,Length,Status)
    stat = NF_INQ_VARID(NCID,VarName,VarID)
    call netcdf_err(stat,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) 'NetCDF error in ',"wrf_io.F90",', line', 2884,' Varname ',Varname
      call wrf_debug ( WARN , TRIM(msg))
      return
    endif
    stat = NF_INQ_VAR(NCID,VarID,Name,XType,StoredDim,VDimIDs,NAtts)
    call netcdf_err(stat,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) 'NetCDF error in ',"wrf_io.F90",', line', 2891
      call wrf_debug ( WARN , TRIM(msg))
      return
    endif
    stat = NF_GET_ATT_INT(NCID,VarID,'FieldType',FType)
    call netcdf_err(stat,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) 'NetCDF error in ',"wrf_io.F90",', line', 2898
      call wrf_debug ( WARN , TRIM(msg))
      return
    endif
! allow coercion between double and single prec real
!jm if(FieldType /= Ftype) then
    if( (FieldType == WRF_REAL .OR. FieldType == WRF_DOUBLE) ) then
      if ( .NOT. (Ftype == WRF_REAL .OR. Ftype == WRF_DOUBLE )) then
        Status = WRF_WARN_TYPE_MISMATCH
        write(msg,*) 'Warning TYPE MISMATCH in ',"wrf_io.F90",', line', 2907
        call wrf_debug ( WARN , TRIM(msg))
        return
      endif
    else if(FieldType /= Ftype) then
      Status = WRF_WARN_TYPE_MISMATCH
      write(msg,*) 'Warning TYPE MISMATCH in ',"wrf_io.F90",', line', 2913
      call wrf_debug ( WARN , TRIM(msg))
      return
    endif
    ! Do not use SELECT statement here as sometimes WRF_REAL=WRF_DOUBLE
    IF (FieldType == WRF_REAL) THEN
! allow coercion between double and single prec real
        if(.NOT. (XType == NF_FLOAT .OR. XType == NF_DOUBLE) ) then
          Status = WRF_WARN_TYPE_MISMATCH
          write(msg,*) 'Warning REAL TYPE MISMATCH in ',"wrf_io.F90",', line', 2923
        endif
    ELSE IF (FieldType == WRF_DOUBLE) THEN
! allow coercion between double and single prec real
        if(.NOT. (XType == NF_FLOAT .OR. XType == NF_DOUBLE) ) then
          Status = WRF_WARN_TYPE_MISMATCH
          write(msg,*) 'Warning DOUBLE TYPE MISMATCH in ',"wrf_io.F90",', line', 2929
        endif
    ELSE IF (FieldType == WRF_INTEGER) THEN
        if(XType /= NF_INT) then
          Status = WRF_WARN_TYPE_MISMATCH
          write(msg,*) 'Warning INTEGER TYPE MISMATCH in ',"wrf_io.F90",', line', 2934
        endif
    ELSE IF (FieldType == WRF_LOGICAL) THEN
        if(XType /= NF_INT) then
          Status = WRF_WARN_TYPE_MISMATCH
          write(msg,*) 'Warning LOGICAL TYPE MISMATCH in ',"wrf_io.F90",', line', 2939
        endif
    ELSE
        Status = WRF_WARN_DATA_TYPE_NOT_FOUND
        write(msg,*) 'Warning DATA TYPE NOT FOUND in ',"wrf_io.F90",', line', 2943
    END IF
    if(Status /= WRF_NO_ERR) then
      call wrf_debug ( WARN , TRIM(msg))
      return
    endif
    ! NDim=0 for scalars. Handle read of old NDim=1 files. TBH: 20060502
    IF ( ( NDim == 0 ) .AND. ( StoredDim == 2 ) ) THEN
      stat = NF_INQ_DIMNAME(NCID,VDimIDs(1),dimname)
      call netcdf_err(stat,Status)
      if(Status /= WRF_NO_ERR) then
        write(msg,*) 'NetCDF error in ',"wrf_io.F90",', line', 2955
        call wrf_debug ( WARN , TRIM(msg))
        return
      endif
      IF ( dimname(1:10) == 'ext_scalar' ) THEN
        NDim = 1
        Length(1) = 1
      ENDIF
    ENDIF
    if(StoredDim /= NDim+1) then
      Status = WRF_ERR_FATAL_BAD_VARIABLE_DIM
      write(msg,*) 'Fatal error BAD VARIABLE DIMENSION in ext_ncd_read_field ',TRIM(Var),TRIM(DateStr)
      call wrf_debug ( FATAL , msg)
      write(msg,*) '  StoredDim ', StoredDim, ' .NE. NDim+1 ', NDim+1
      call wrf_debug ( FATAL , msg)
      return
    endif
    do j=1,NDim
      stat = NF_INQ_DIMLEN(NCID,VDimIDs(j),StoredLen(j))
      call netcdf_err(stat,Status)
      if(Status /= WRF_NO_ERR) then
        write(msg,*) 'NetCDF error in ',"wrf_io.F90",', line', 2976
        call wrf_debug ( WARN , TRIM(msg))
        return
      endif
      if(Length(j) > StoredLen(j)) then
        Status = WRF_WARN_READ_PAST_EOF
        write(msg,*) 'Warning READ PAST EOF in ext_ncd_read_field of ',TRIM(Var),Length(j),'>',StoredLen(j)
        call wrf_debug ( WARN , TRIM(msg))
        return
      elseif(Length(j) <= 0) then
        Status = WRF_WARN_ZERO_LENGTH_READ
        write(msg,*) 'Warning ZERO LENGTH READ in ',"wrf_io.F90",', line', 2987
        call wrf_debug ( WARN , TRIM(msg))
        return
      elseif(DomainStart(j) < MemoryStart(j)) then
        Status = WRF_WARN_DIMENSION_ERROR
        write(msg,*) 'Warning dim ',j,' DomainStart (',DomainStart(j), &
                     ') < MemoryStart (',MemoryStart(j),') in ',"wrf_io.F90",', line', 2993
        call wrf_debug ( WARN , TRIM(msg))
! return
      endif
    enddo
    StoredStart = 1
    call GetIndices(NDim,MemoryStart,MemoryEnd,l1,l2,m1,m2,n1,n2)
    call GetIndices(NDim,StoredStart,StoredLen,x1,x2,y1,y2,z1,z2)
!jm call GetIndices(NDim,DomainStart,DomainEnd,i1,i2,j1,j2,k1,k2)
    call GetIndices(NDim,PatchStart,PatchEnd,i1,i2,j1,j2,k1,k2)
    di=1
    if(FieldType == WRF_DOUBLE) di=2
    allocate(XField(di,x1:x2,y1:y2,z1:z2), STAT=stat)
    if(stat/= 0) then
      Status = WRF_ERR_FATAL_ALLOCATION_ERROR
      write(msg,*) 'Fatal ALLOCATION ERROR in ',"wrf_io.F90",', line', 3010
      call wrf_debug ( FATAL , msg)
      return
    endif
    call FieldIO('read',DataHandle,DateStr,Length,MemoryOrder, &
                  FieldType,NCID,VarID,XField,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) 'Warning Status = ',Status,' in ',"wrf_io.F90",', line', 3017
      call wrf_debug ( WARN , TRIM(msg))
      return
    endif
    call Transpose('read',MemoryOrder,di, Field,l1,l2,m1,m2,n1,n2 &
                                        ,XField,x1,x2,y1,y2,z1,z2 &
                                               ,i1,i2,j1,j2,k1,k2 )
    deallocate(XField, STAT=stat)
    if(stat/= 0) then
      Status = WRF_ERR_FATAL_DEALLOCATION_ERR
      write(msg,*) 'Fatal DEALLOCATION ERROR in ',"wrf_io.F90",', line', 3027
      call wrf_debug ( FATAL , msg)
      return
    endif
  else
    Status = WRF_ERR_FATAL_BAD_FILE_STATUS
    write(msg,*) 'Fatal error BAD FILE STATUS in ',"wrf_io.F90",', line', 3033
    call wrf_debug ( FATAL , msg)
  endif
  DH%first_operation = .FALSE.
  return
end subroutine ext_ncd_read_field
subroutine ext_ncd_inquire_opened( DataHandle, FileName , FileStatus, Status )
  use wrf_data
  use ext_ncd_support_routines
  implicit none
  include 'wrf_status_codes.h'
  integer ,intent(in) :: DataHandle
  character*(*) ,intent(inout) :: FileName
  integer ,intent(out) :: FileStatus
  integer ,intent(out) :: Status
  type(wrf_data_handle) ,pointer :: DH
  !call upgrade_filename(FileName)
  call GetDH(DataHandle,DH,Status)
  if(Status /= WRF_NO_ERR) then
    FileStatus = WRF_FILE_NOT_OPENED
    return
  endif
  if(trim(FileName) /= trim(DH%FileName)) then
    FileStatus = WRF_FILE_NOT_OPENED
  else
    FileStatus = DH%FileStatus
  endif
  Status = WRF_NO_ERR
  return
end subroutine ext_ncd_inquire_opened
subroutine ext_ncd_inquire_filename( Datahandle, FileName, FileStatus, Status )
  use wrf_data
  use ext_ncd_support_routines
  implicit none
  include 'wrf_status_codes.h'
  integer ,intent(in) :: DataHandle
  character*(*) ,intent(out) :: FileName
  integer ,intent(out) :: FileStatus
  integer ,intent(out) :: Status
  type(wrf_data_handle) ,pointer :: DH
  FileStatus = WRF_FILE_NOT_OPENED
  call GetDH(DataHandle,DH,Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) 'Warning Status = ',Status,' in ',"wrf_io.F90",', line', 3080
    call wrf_debug ( WARN , TRIM(msg))
    return
  endif
  FileName = trim(DH%FileName)
  FileStatus = DH%FileStatus
  Status = WRF_NO_ERR
  return
end subroutine ext_ncd_inquire_filename
subroutine ext_ncd_set_time(DataHandle, DateStr, Status)
  use wrf_data
  use ext_ncd_support_routines
  implicit none
  include 'wrf_status_codes.h'
  integer ,intent(in) :: DataHandle
  character*(*) ,intent(in) :: DateStr
  integer ,intent(out) :: Status
  type(wrf_data_handle) ,pointer :: DH
  integer :: i
  call DateCheck(DateStr,Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) 'Warning DATE STRING ERROR in ',"wrf_io.F90",', line', 3103
    call wrf_debug ( WARN , TRIM(msg))
    return
  endif
  call GetDH(DataHandle,DH,Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) 'Warning Status = ',Status,' in ',"wrf_io.F90",', line', 3109
    call wrf_debug ( WARN , TRIM(msg))
    return
  endif
  if(DH%FileStatus == WRF_FILE_NOT_OPENED) then
    Status = WRF_WARN_FILE_NOT_OPENED
    write(msg,*) 'Warning FILE NOT OPENED in ',"wrf_io.F90",', line', 3115
    call wrf_debug ( WARN , TRIM(msg))
  elseif(DH%FileStatus == WRF_FILE_OPENED_NOT_COMMITTED) then
    Status = WRF_WARN_FILE_NOT_COMMITTED
    write(msg,*) 'Warning FILE NOT COMMITTED in ',"wrf_io.F90",', line', 3119
    call wrf_debug ( WARN , TRIM(msg))
  elseif(DH%FileStatus == WRF_FILE_OPENED_FOR_WRITE) then
    Status = WRF_WARN_READ_WONLY_FILE
    write(msg,*) 'Warning READ WRITE ONLY FILE in ',"wrf_io.F90",', line', 3123
    call wrf_debug ( WARN , TRIM(msg))
  elseif(DH%FileStatus == WRF_FILE_OPENED_FOR_READ) then
    do i=1,MaxTimes
      if(DH%Times(i)==DateStr) then
        DH%CurrentTime = i
        exit
      endif
      if(i==MaxTimes) then
        Status = WRF_WARN_TIME_NF
        return
      endif
    enddo
    DH%CurrentVariable = 0
    Status = WRF_NO_ERR
  else
    Status = WRF_ERR_FATAL_BAD_FILE_STATUS
    write(msg,*) 'Fatal error BAD FILE STATUS in ',"wrf_io.F90",', line', 3140
    call wrf_debug ( FATAL , msg)
  endif
  return
end subroutine ext_ncd_set_time
subroutine ext_ncd_get_next_time(DataHandle, DateStr, Status)
  use wrf_data
  use ext_ncd_support_routines
  implicit none
  include 'wrf_status_codes.h'
  integer ,intent(in) :: DataHandle
  character*(*) ,intent(out) :: DateStr
  integer ,intent(out) :: Status
  type(wrf_data_handle) ,pointer :: DH
  call GetDH(DataHandle,DH,Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) 'Warning Status = ',Status,' in ',"wrf_io.F90",', line', 3158
    call wrf_debug ( WARN , TRIM(msg))
    return
  endif
  if(DH%FileStatus == WRF_FILE_NOT_OPENED) then
    Status = WRF_WARN_FILE_NOT_OPENED
    write(msg,*) 'Warning FILE NOT OPENED in ',"wrf_io.F90",', line', 3164
    call wrf_debug ( WARN , TRIM(msg))
  elseif(DH%FileStatus == WRF_FILE_OPENED_NOT_COMMITTED) then
    Status = WRF_WARN_DRYRUN_READ
    write(msg,*) 'Warning DRYRUN READ in ',"wrf_io.F90",', line', 3168
    call wrf_debug ( WARN , TRIM(msg))
  elseif(DH%FileStatus == WRF_FILE_OPENED_FOR_WRITE) then
    Status = WRF_WARN_READ_WONLY_FILE
    write(msg,*) 'Warning READ WRITE ONLY FILE in ',"wrf_io.F90",', line', 3172
    call wrf_debug ( WARN , TRIM(msg))
  elseif(DH%FileStatus == WRF_FILE_OPENED_FOR_READ .OR. DH%FileStatus == WRF_FILE_OPENED_FOR_UPDATE ) then
    if(DH%CurrentTime >= DH%NumberTimes) then
      Status = WRF_WARN_TIME_EOF
      return
    endif
    DH%CurrentTime = DH%CurrentTime +1
    DateStr = DH%Times(DH%CurrentTime)
    DH%CurrentVariable = 0
    Status = WRF_NO_ERR
  else
    Status = WRF_ERR_FATAL_BAD_FILE_STATUS
    write(msg,*) 'DH%FileStatus ',DH%FileStatus
    call wrf_debug ( FATAL , msg)
    write(msg,*) 'Fatal error BAD FILE STATUS in ',"wrf_io.F90",', line', 3187
    call wrf_debug ( FATAL , msg)
  endif
  return
end subroutine ext_ncd_get_next_time
subroutine ext_ncd_get_previous_time(DataHandle, DateStr, Status)
  use wrf_data
  use ext_ncd_support_routines
  implicit none
  include 'wrf_status_codes.h'
  integer ,intent(in) :: DataHandle
  character*(*) ,intent(out) :: DateStr
  integer ,intent(out) :: Status
  type(wrf_data_handle) ,pointer :: DH
  call GetDH(DataHandle,DH,Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) 'Warning Status = ',Status,' in ',"wrf_io.F90",', line', 3205
    call wrf_debug ( WARN , TRIM(msg))
    return
  endif
  if(DH%FileStatus == WRF_FILE_NOT_OPENED) then
    Status = WRF_WARN_FILE_NOT_OPENED
    write(msg,*) 'Warning FILE NOT OPENED in ',"wrf_io.F90",', line', 3211
    call wrf_debug ( WARN , TRIM(msg))
  elseif(DH%FileStatus == WRF_FILE_OPENED_NOT_COMMITTED) then
    Status = WRF_WARN_DRYRUN_READ
    write(msg,*) 'Warning DRYRUN READ in ',"wrf_io.F90",', line', 3215
    call wrf_debug ( WARN , TRIM(msg))
  elseif(DH%FileStatus == WRF_FILE_OPENED_FOR_WRITE) then
    Status = WRF_WARN_READ_WONLY_FILE
    write(msg,*) 'Warning READ WRITE ONLY FILE in ',"wrf_io.F90",', line', 3219
    call wrf_debug ( WARN , TRIM(msg))
  elseif(DH%FileStatus == WRF_FILE_OPENED_FOR_READ) then
    if(DH%CurrentTime.GT.0) then
      DH%CurrentTime = DH%CurrentTime -1
    endif
    DateStr = DH%Times(DH%CurrentTime)
    DH%CurrentVariable = 0
    Status = WRF_NO_ERR
  else
    Status = WRF_ERR_FATAL_BAD_FILE_STATUS
    write(msg,*) 'Fatal error BAD FILE STATUS in ',"wrf_io.F90",', line', 3230
    call wrf_debug ( FATAL , msg)
  endif
  return
end subroutine ext_ncd_get_previous_time
subroutine ext_ncd_get_next_var(DataHandle, VarName, Status)
  use wrf_data
  use ext_ncd_support_routines
  implicit none
  include 'wrf_status_codes.h'
  include 'netcdf.inc'
  integer ,intent(in) :: DataHandle
  character*(*) ,intent(out) :: VarName
  integer ,intent(out) :: Status
  type(wrf_data_handle) ,pointer :: DH
  integer :: stat
  character (80) :: Name
  call GetDH(DataHandle,DH,Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) 'Warning Status = ',Status,' in ',"wrf_io.F90",', line', 3251
    call wrf_debug ( WARN , TRIM(msg))
    return
  endif
  if(DH%FileStatus == WRF_FILE_NOT_OPENED) then
    Status = WRF_WARN_FILE_NOT_OPENED
    write(msg,*) 'Warning FILE NOT OPENED in ',"wrf_io.F90",', line', 3257
    call wrf_debug ( WARN , TRIM(msg))
  elseif(DH%FileStatus == WRF_FILE_OPENED_NOT_COMMITTED) then
    Status = WRF_WARN_DRYRUN_READ
    write(msg,*) 'Warning DRYRUN READ in ',"wrf_io.F90",', line', 3261
    call wrf_debug ( WARN , TRIM(msg))
  elseif(DH%FileStatus == WRF_FILE_OPENED_FOR_WRITE) then
    Status = WRF_WARN_READ_WONLY_FILE
    write(msg,*) 'Warning READ WRITE ONLY FILE in ',"wrf_io.F90",', line', 3265
    call wrf_debug ( WARN , TRIM(msg))
  elseif(DH%FileStatus == WRF_FILE_OPENED_FOR_READ .OR. DH%FileStatus == WRF_FILE_OPENED_FOR_UPDATE) then
    DH%CurrentVariable = DH%CurrentVariable +1
    if(DH%CurrentVariable > DH%NumVars) then
      Status = WRF_WARN_VAR_EOF
      return
    endif
    VarName = DH%VarNames(DH%CurrentVariable)
    Status = WRF_NO_ERR
  else
    Status = WRF_ERR_FATAL_BAD_FILE_STATUS
    write(msg,*) 'Fatal error BAD FILE STATUS in ',"wrf_io.F90",', line', 3278
    call wrf_debug ( FATAL , msg)
  endif
  return
end subroutine ext_ncd_get_next_var
subroutine ext_ncd_end_of_frame(DataHandle, Status)
  use wrf_data
  use ext_ncd_support_routines
  implicit none
  include 'netcdf.inc'
  include 'wrf_status_codes.h'
  integer ,intent(in) :: DataHandle
  integer ,intent(out) :: Status
  type(wrf_data_handle) ,pointer :: DH
  call GetDH(DataHandle,DH,Status)
  return
end subroutine ext_ncd_end_of_frame
! NOTE: For scalar variables NDim is set to zero and DomainStart and
! NOTE: DomainEnd are left unmodified.
subroutine ext_ncd_get_var_info(DataHandle,Name,NDim,MemoryOrder,Stagger,DomainStart,DomainEnd,WrfType,Status)
  use wrf_data
  use ext_ncd_support_routines
  implicit none
  include 'netcdf.inc'
  include 'wrf_status_codes.h'
  integer ,intent(in) :: DataHandle
  character*(*) ,intent(in) :: Name
  integer ,intent(out) :: NDim
  character*(*) ,intent(out) :: MemoryOrder
  character*(*) :: Stagger ! Dummy for now
  integer ,dimension(*) ,intent(out) :: DomainStart, DomainEnd
  integer ,intent(out) :: WrfType
  integer ,intent(out) :: Status
  type(wrf_data_handle) ,pointer :: DH
  integer :: VarID
  integer ,dimension(NVarDims) :: VDimIDs
  integer :: j
  integer :: stat
  integer :: XType
  call GetDH(DataHandle,DH,Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) 'Warning Status = ',Status,' in ',"wrf_io.F90",', line', 3323
    call wrf_debug ( WARN , TRIM(msg))
    return
  endif
  if(DH%FileStatus == WRF_FILE_NOT_OPENED) then
    Status = WRF_WARN_FILE_NOT_OPENED
    write(msg,*) 'Warning FILE NOT OPENED in ',"wrf_io.F90",', line', 3329
    call wrf_debug ( WARN , TRIM(msg))
    return
  elseif(DH%FileStatus == WRF_FILE_OPENED_NOT_COMMITTED) then
    Status = WRF_WARN_DRYRUN_READ
    write(msg,*) 'Warning DRYRUN READ in ',"wrf_io.F90",', line', 3334
    call wrf_debug ( WARN , TRIM(msg))
    return
  elseif(DH%FileStatus == WRF_FILE_OPENED_FOR_WRITE) then
    Status = WRF_WARN_READ_WONLY_FILE
    write(msg,*) 'Warning READ WRITE ONLY FILE in ',"wrf_io.F90",', line', 3339
    call wrf_debug ( WARN , TRIM(msg))
    return
  elseif(DH%FileStatus == WRF_FILE_OPENED_FOR_READ .OR. DH%FileStatus == WRF_FILE_OPENED_FOR_UPDATE) then
    stat = NF_INQ_VARID(DH%NCID,Name,VarID)
    call netcdf_err(stat,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) 'NetCDF error in ',"wrf_io.F90",', line', 3346
      call wrf_debug ( WARN , TRIM(msg))
      return
    endif
    stat = NF_INQ_VARTYPE(DH%NCID,VarID,XType)
    call netcdf_err(stat,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) 'NetCDF error in ',"wrf_io.F90",', line', 3353
      call wrf_debug ( WARN , TRIM(msg))
      return
    endif
    stat = NF_GET_ATT_INT(DH%NCID,VarID,'FieldType',WrfType)
    call netcdf_err(stat,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) 'NetCDF error in ',"wrf_io.F90",', line', 3360
      call wrf_debug ( WARN , TRIM(msg))
      return
    endif
    select case (XType)
      case (NF_BYTE)
        Status = WRF_WARN_BAD_DATA_TYPE
        write(msg,*) 'Warning BYTE IS BAD DATA TYPE in ',"wrf_io.F90",', line', 3367
        call wrf_debug ( WARN , TRIM(msg))
        return
      case (NF_CHAR)
        Status = WRF_WARN_BAD_DATA_TYPE
        write(msg,*) 'Warning CHAR IS BAD DATA TYPE in ',"wrf_io.F90",', line', 3372
        call wrf_debug ( WARN , TRIM(msg))
        return
      case (NF_SHORT)
        Status = WRF_WARN_BAD_DATA_TYPE
        write(msg,*) 'Warning SHORT IS BAD DATA TYPE in ',"wrf_io.F90",', line', 3377
        call wrf_debug ( WARN , TRIM(msg))
        return
      case (NF_INT)
        if(WrfType /= WRF_INTEGER .and. WrfType /= WRF_LOGICAL) then
          Status = WRF_WARN_BAD_DATA_TYPE
          write(msg,*) 'Warning BAD DATA TYPE in ',"wrf_io.F90",', line', 3383
          call wrf_debug ( WARN , TRIM(msg))
          return
        endif
      case (NF_FLOAT)
        if(WrfType /= WRF_REAL) then
          Status = WRF_WARN_BAD_DATA_TYPE
          write(msg,*) 'Warning BAD DATA TYPE in ',"wrf_io.F90",', line', 3390
          call wrf_debug ( WARN , TRIM(msg))
          return
        endif
      case (NF_DOUBLE)
        if(WrfType /= WRF_DOUBLE) then
          Status = WRF_WARN_BAD_DATA_TYPE
          write(msg,*) 'Warning BAD DATA TYPE in ',"wrf_io.F90",', line', 3397
          call wrf_debug ( WARN , TRIM(msg))
          return
        endif
      case default
        Status = WRF_WARN_DATA_TYPE_NOT_FOUND
        write(msg,*) 'Warning DATA TYPE NOT FOUND in ',"wrf_io.F90",', line', 3403
        call wrf_debug ( WARN , TRIM(msg))
        return
    end select
    stat = NF_GET_ATT_TEXT(DH%NCID,VarID,'MemoryOrder',MemoryOrder)
    call netcdf_err(stat,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) 'NetCDF error in ',"wrf_io.F90",', line', 3411
      call wrf_debug ( WARN , TRIM(msg))
      return
    endif
    call GetDim(MemoryOrder,NDim,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) 'Warning BAD MEMORY ORDER ',TRIM(MemoryOrder),' in ',"wrf_io.F90",', line', 3417
      call wrf_debug ( WARN , TRIM(msg))
      return
    endif
    stat = NF_INQ_VARDIMID(DH%NCID,VarID,VDimIDs)
    call netcdf_err(stat,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) 'NetCDF error in ',"wrf_io.F90",', line', 3424
      call wrf_debug ( WARN , TRIM(msg))
      return
    endif
    do j = 1, NDim
      DomainStart(j) = 1
      stat = NF_INQ_DIMLEN(DH%NCID,VDimIDs(j),DomainEnd(j))
      call netcdf_err(stat,Status)
      if(Status /= WRF_NO_ERR) then
        write(msg,*) 'NetCDF error in ',"wrf_io.F90",', line', 3433
        call wrf_debug ( WARN , TRIM(msg))
        return
      endif
    enddo
  else
    Status = WRF_ERR_FATAL_BAD_FILE_STATUS
    write(msg,*) 'Fatal error BAD FILE STATUS in ',"wrf_io.F90",', line', 3440
    call wrf_debug ( FATAL , msg)
  endif
  return
end subroutine ext_ncd_get_var_info
subroutine ext_ncd_warning_str( Code, ReturnString, Status)
  use wrf_data
  use ext_ncd_support_routines
  implicit none
  include 'netcdf.inc'
  include 'wrf_status_codes.h'
  integer , intent(in) ::Code
  character *(*), intent(out) :: ReturnString
  integer, intent(out) ::Status
  SELECT CASE (Code)
  CASE (0)
      ReturnString='No error'
      Status=WRF_NO_ERR
      return
  CASE (-1)
      ReturnString= 'File not found (or file is incomplete)'
      Status=WRF_NO_ERR
      return
  CASE (-2)
      ReturnString='Metadata not found'
      Status=WRF_NO_ERR
      return
  CASE (-3)
      ReturnString= 'Timestamp not found'
      Status=WRF_NO_ERR
      return
  CASE (-4)
      ReturnString= 'No more timestamps'
      Status=WRF_NO_ERR
      return
  CASE (-5)
      ReturnString= 'Variable not found'
      Status=WRF_NO_ERR
      return
  CASE (-6)
      ReturnString= 'No more variables for the current time'
      Status=WRF_NO_ERR
      return
  CASE (-7)
      ReturnString= 'Too many open files'
      Status=WRF_NO_ERR
      return
  CASE (-8)
      ReturnString= 'Data type mismatch'
      Status=WRF_NO_ERR
      return
  CASE (-9)
      ReturnString= 'Attempt to write read-only file'
      Status=WRF_NO_ERR
      return
  CASE (-10)
      ReturnString= 'Attempt to read write-only file'
      Status=WRF_NO_ERR
      return
  CASE (-11)
      ReturnString= 'Attempt to access unopened file'
      Status=WRF_NO_ERR
      return
  CASE (-12)
      ReturnString= 'Attempt to do 2 trainings for 1 variable'
      Status=WRF_NO_ERR
      return
  CASE (-13)
      ReturnString= 'Attempt to read past EOF'
      Status=WRF_NO_ERR
      return
  CASE (-14)
      ReturnString= 'Bad data handle'
      Status=WRF_NO_ERR
      return
  CASE (-15)
      ReturnString= 'Write length not equal to training length'
      Status=WRF_NO_ERR
      return
  CASE (-16)
      ReturnString= 'More dimensions requested than training'
      Status=WRF_NO_ERR
      return
  CASE (-17)
      ReturnString= 'Attempt to read more data than exists'
      Status=WRF_NO_ERR
      return
  CASE (-18)
      ReturnString= 'Input dimensions inconsistent'
      Status=WRF_NO_ERR
      return
  CASE (-19)
      ReturnString= 'Input MemoryOrder not recognized'
      Status=WRF_NO_ERR
      return
  CASE (-20)
      ReturnString= 'A dimension name with 2 different lengths'
      Status=WRF_NO_ERR
      return
  CASE (-21)
      ReturnString= 'String longer than provided storage'
      Status=WRF_NO_ERR
      return
  CASE (-22)
      ReturnString= 'Function not supportable'
      Status=WRF_NO_ERR
      return
  CASE (-23)
      ReturnString= 'Package implements this routine as NOOP'
      Status=WRF_NO_ERR
      return
!netcdf-specific warning messages
  CASE (-1007)
      ReturnString= 'Bad data type'
      Status=WRF_NO_ERR
      return
  CASE (-1008)
      ReturnString= 'File not committed'
      Status=WRF_NO_ERR
      return
  CASE (-1009)
      ReturnString= 'File is opened for reading'
      Status=WRF_NO_ERR
      return
  CASE (-1011)
      ReturnString= 'Attempt to write metadata after open commit'
      Status=WRF_NO_ERR
      return
  CASE (-1010)
      ReturnString= 'I/O not initialized'
      Status=WRF_NO_ERR
      return
  CASE (-1012)
     ReturnString= 'Too many variables requested'
      Status=WRF_NO_ERR
      return
  CASE (-1013)
     ReturnString= 'Attempt to close file during a dry run'
      Status=WRF_NO_ERR
      return
  CASE (-1014)
      ReturnString= 'Date string not 19 characters in length'
      Status=WRF_NO_ERR
      return
  CASE (-1015)
      ReturnString= 'Attempt to read zero length words'
      Status=WRF_NO_ERR
      return
  CASE (-1016)
      ReturnString= 'Data type not found'
      Status=WRF_NO_ERR
      return
  CASE (-1017)
      ReturnString= 'Badly formatted date string'
      Status=WRF_NO_ERR
      return
  CASE (-1018)
      ReturnString= 'Attempt at read during a dry run'
      Status=WRF_NO_ERR
      return
  CASE (-1019)
      ReturnString= 'Attempt to get zero words'
      Status=WRF_NO_ERR
      return
  CASE (-1020)
      ReturnString= 'Attempt to put zero length words'
      Status=WRF_NO_ERR
      return
  CASE (-1021)
      ReturnString= 'NetCDF error'
      Status=WRF_NO_ERR
      return
  CASE (-1022)
      ReturnString= 'Requested length <= 1'
      Status=WRF_NO_ERR
      return
  CASE (-1023)
      ReturnString= 'More data available than requested'
      Status=WRF_NO_ERR
      return
  CASE (-1024)
      ReturnString= 'New date less than previous date'
      Status=WRF_NO_ERR
      return
  CASE DEFAULT
      ReturnString= 'This warning code is not supported or handled directly by WRF and NetCDF. &
      & Might be an erroneous number, or specific to an i/o package other than NetCDF; you may need &
      & to be calling a package-specific routine to return a message for this warning code.'
      Status=WRF_NO_ERR
  END SELECT
  return
end subroutine ext_ncd_warning_str
!returns message string for all WRF and netCDF warning/error status codes
!Other i/o packages must provide their own routines to return their own status messages
subroutine ext_ncd_error_str( Code, ReturnString, Status)
  use wrf_data
  use ext_ncd_support_routines
  implicit none
  include 'netcdf.inc'
  include 'wrf_status_codes.h'
  integer , intent(in) ::Code
  character *(*), intent(out) :: ReturnString
  integer, intent(out) ::Status
  SELECT CASE (Code)
  CASE (-100)
      ReturnString= 'Allocation Error'
      Status=WRF_NO_ERR
      return
  CASE (-101)
      ReturnString= 'Deallocation Error'
      Status=WRF_NO_ERR
      return
  CASE (-102)
      ReturnString= 'Bad File Status'
      Status=WRF_NO_ERR
      return
  CASE (-1004)
      ReturnString= 'Variable on disk is not 3D'
      Status=WRF_NO_ERR
      return
  CASE (-1005)
      ReturnString= 'Metadata on disk is not 1D'
      Status=WRF_NO_ERR
      return
  CASE (-1006)
      ReturnString= 'Time dimension too small'
      Status=WRF_NO_ERR
      return
  CASE DEFAULT
      ReturnString= 'This error code is not supported or handled directly by WRF and NetCDF. &
      & Might be an erroneous number, or specific to an i/o package other than NetCDF; you may need &
      & to be calling a package-specific routine to return a message for this error code.'
      Status=WRF_NO_ERR
  END SELECT
  return
end subroutine ext_ncd_error_str
