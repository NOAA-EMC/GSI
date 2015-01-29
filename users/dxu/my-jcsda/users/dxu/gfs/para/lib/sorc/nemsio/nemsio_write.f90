!----------------------------------------------------------------------------
module nemsio_write
!
!$$$ documentation clock
!
! module: nemsio_read      read data fields from a nemsio file
!  Programmer: J. Wang          date: 2011-01-13
!
! abstract: this module provides subroutines to write data fields into of a
!           nemsio file. The data file could be 'bin4','bin8', or 'grib'.
!           The dat field could be written by the record number
!           or by given the data field name, level type and level. Overload
!           interfaces are provided to handle different data type (real(4)
!           or real(8)) of the array that holds the data.
!
! Possible return code
!          0   Successful call
!         -61  get dimension from gfile
!         -62  write data field by record number using w3d
!         -63  write data field by given data field name,level type and level using w3d
!         -64  write data field by record number using w34
!         -65  write data field by given data field name,level type and level using w34
!         -71  write data field by record number from 4 byte real binary file
!         -72  write data field by record number from 8 byte real binary file
!         -73  write data field by field name,levtyp and lev from 4 byte real binary file
!         -74  write data field by field name,levtyp and lev from 8 byte real binary file
!         -75  write data field by record number using w34 from grib data
!         -76  write data field by field name,level type and level using w34 from grib data
!         -77  write data field by record number using w3d from grib data
!         -78  write data field by field name,level type and level using w3d from grib data
!------------------------------------------------------------------------------
!
  use nemsio_openclose
!
  implicit none
!
  private
!------------------------------------------------------------------------------
!----- interface
!
  interface nemsio_writerec
    module procedure nemsio_writerec4
    module procedure nemsio_writerec8
  end interface nemsio_writerec
!
  interface nemsio_writerecv
    module procedure nemsio_writerecv4
    module procedure nemsio_writerecv8
  end interface nemsio_writerecv
!
  interface nemsio_writerecw34
    module procedure nemsio_writerec4w34
    module procedure nemsio_writerec8w34
  end interface nemsio_writerecw34
!
  interface nemsio_writerecvw34
    module procedure nemsio_writerecv4w34
    module procedure nemsio_writerecv8w34
  end interface nemsio_writerecvw34
!
!public mehtods
  public nemsio_writerec,nemsio_writerecv,nemsio_writerecw34,nemsio_writerecvw34
!
!---------------------------------------------------------
! local data
!
  character(8) :: mygdatatype
  integer mydimx,mydimy,mydimz,mynframe,myfieldsize,mytlmeta,myflunit
!
contains
!
!------------------------------------------------------------------------------
  subroutine nemsio_getgfile(gfile,iret)
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
! abstract: read nemsio data by record number into a 2D 32 bits array
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
    implicit none
    type(nemsio_gfile),intent(in)                 :: gfile
    integer(nemsio_intkind),optional,intent(out)  :: iret
!
    if(present(iret)) iret=-61
    call nemsio_getfilehead(gfile,iret=iret,gdatatype=mygdatatype,dimx=mydimx,   &
           dimy=mydimy,dimz=mydimz,nframe=mynframe,tlmeta=mytlmeta,              &
           flunit=myflunit )
    myfieldsize=(mydimx+2*mynframe)*(mydimy+2*mynframe)
    if(present(iret)) iret=0
!
!    print *,'in nemsio_getgfile,dimx=',mydimx,mydimy,mydimz
!
  end subroutine nemsio_getgfile
!
!------------------------------------------------------------------------------
  subroutine nemsio_writerec4(gfile,jrec,data,iret,itr,zhour)
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
! abstract: write nemsio a 2D 32 bits array data into bin file using record number
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
    implicit none
    type(nemsio_gfile),intent(in)                 :: gfile
    integer(nemsio_intkind),intent(in)            :: jrec
    real(nemsio_realkind),intent(in)              :: data(:)
    integer(nemsio_intkind),optional,intent(out)  :: iret
    integer(nemsio_intkind),optional,intent(in)   :: itr 
    real(nemsio_realkind),optional,intent(in)     :: zhour
    integer :: ios
!
!------------------------------------------------------------
! write 4 byte rec
!------------------------------------------------------------
!
   if(present(iret)) iret=-62
   call nemsio_getgfile(gfile,iret)
!
   if ( mygdatatype .eq. 'bin4') then
     call nemsio_writerecbin4d4(gfile,jrec,data,ios)
   else if ( mygdatatype .eq. 'bin8') then
     call nemsio_writerecbin8d4(gfile,jrec,data,ios)
   else
     call nemsio_writerecgrb4(gfile,jrec,data,ios,itr=itr,zhour=zhour)
   endif
   if ( ios .ne.0 ) then
     if(present(iret)) then
       iret=ios
       return
     else
       call nemsio_stop
     endif
   endif
   if(present(iret)) iret=0
!
   return
  end subroutine nemsio_writerec4
!------------------------------------------------------------------------------
  subroutine nemsio_writerec8(gfile,jrec,data,iret,itr,zhour)
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
! abstract: write nemsio a 2D 64 bits array data into bin file using record number
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
    implicit none
    type(nemsio_gfile),intent(in)                 :: gfile
    integer(nemsio_intkind),intent(in)            :: jrec
    real(nemsio_dblekind),intent(in)              :: data(:)
    integer(nemsio_intkind),optional,intent(out)  :: iret
    integer(nemsio_intkind),optional,intent(in)   :: itr 
    real(nemsio_realkind),optional,intent(in)     :: zhour
    integer ios
!------------------------------------------------------------
! write 4 byte rec
!------------------------------------------------------------
!
   if(present(iret)) iret=-62
   call nemsio_getgfile(gfile,iret)
!
   if ( mygdatatype .eq. 'bin4') then
     call nemsio_writerecbin4d8(gfile,jrec,data,ios)
   else if ( mygdatatype .eq. 'bin8') then
     call nemsio_writerecbin8d8(gfile,jrec,data,ios)
   else
     call nemsio_writerecgrb8(gfile,jrec,data,ios,itr=itr,zhour=zhour)
   endif
   if ( ios .ne.0 ) then
     if(present(iret)) then
       iret=ios
       return
     else
       call nemsio_stop
     endif
   endif
   if(present(iret)) iret=0
!
   return
  end subroutine nemsio_writerec8
!------------------------------------------------------------------------------
  subroutine nemsio_writerecv4(gfile,name,levtyp,lev,data,iret, &
             itr,zhour)
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
! abstract: write nemsio a 2D 32 bits array data into bin file using record number
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
    implicit none
    type(nemsio_gfile),intent(in)                 :: gfile
    character(*),intent(in)                       :: name
    character(*),optional,intent(in)              :: levtyp
    integer(nemsio_intkind),optional,intent(in)   :: lev
    real(nemsio_realkind),intent(in)              :: data(:)
    integer(nemsio_intkind),optional,intent(out)  :: iret
    integer(nemsio_intkind),optional,intent(in)   :: itr 
    real(nemsio_realkind),optional,intent(in)     :: zhour
    integer ios
!------------------------------------------------------------
! read 4 byte rec
!------------------------------------------------------------
!
   if(present(iret))iret=-63
!
   call nemsio_getgfile(gfile,iret)
!
   if ( mygdatatype .eq. 'bin4') then
     call nemsio_writerecvbin4d4(gfile,name,levtyp,lev,data,ios)
   else if ( mygdatatype .eq. 'bin8') then
     call nemsio_writerecvbin8d4(gfile,name,levtyp,lev,data,ios)
   else
     call nemsio_writerecvgrb4(gfile,name,levtyp,lev,data,ios,itr=itr,        &
          zhour=zhour)
   endif
   if ( ios .ne.0 ) then
     if(present(iret)) then
       iret=ios
       return
     else
       call nemsio_stop
     endif
   endif
   if(present(iret)) iret=0
!
   return
  end subroutine nemsio_writerecv4
!------------------------------------------------------------------------------
  subroutine nemsio_writerecv8(gfile,name,levtyp,lev,data,iret, &
             itr,zhour)
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
! abstract: write nemsio a 2D 32 bits array data into bin file using record number
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
    implicit none
    type(nemsio_gfile),intent(in)                 :: gfile
    character(*),intent(in)                       :: name
    character(*),optional,intent(in)              :: levtyp
    integer(nemsio_intkind),optional,intent(in)   :: lev
    real(nemsio_dblekind),intent(in)              :: data(:)
    integer(nemsio_intkind),optional,intent(out)  :: iret
    integer(nemsio_intkind),optional,intent(in)   :: itr 
    real(nemsio_realkind),optional,intent(in)     :: zhour
    integer ios
!------------------------------------------------------------
! read 4 byte rec
!------------------------------------------------------------
!
    if(present(iret)) iret=-63
!
   call nemsio_getgfile(gfile,iret)
!
   if ( mygdatatype .eq. 'bin4') then
     call nemsio_writerecvbin4d8(gfile,name,levtyp,lev,data,ios)
   else if ( mygdatatype .eq. 'bin8') then
     call nemsio_writerecvbin8d8(gfile,name,levtyp,lev,data,ios)
   else
     call nemsio_writerecvgrb8(gfile,name,levtyp,lev,data,ios,itr=itr,    &
          zhour=zhour)
   endif
   if ( ios .ne.0 ) then
     if(present(iret)) then
       iret=ios
       return
     else
       call nemsio_stop
     endif
   endif
   if(present(iret)) iret=0
!
   return
  end subroutine nemsio_writerecv8
!
!------------------------------------------------------------------------------
  subroutine nemsio_writerec4w34(gfile,jrec,data,iret,itr,zhour)
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
! abstract: write nemsio a 2D 32 bits array data into bin file using record number
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
    implicit none
    type(nemsio_gfile),intent(in)                 :: gfile
    integer(nemsio_intkind),intent(in)            :: jrec
    real(nemsio_realkind),intent(in)              :: data(:)
    integer(nemsio_intkind),optional,intent(out)  :: iret
    integer(nemsio_intkind),optional,intent(in)   :: itr
    real(nemsio_realkind),optional,intent(in)     :: zhour
    integer ios
!
!------------------------------------------------------------
! write 4 byte rec
!------------------------------------------------------------
!
    if(present(iret)) iret=-64
!
   call nemsio_getgfile(gfile,iret)
!
   if ( mygdatatype .eq. 'bin4') then
     call nemsio_writerecbin4d4(gfile,jrec,data,ios)
   else if ( mygdatatype .eq. 'bin8') then
     call nemsio_writerecbin8d4(gfile,jrec,data,ios)
   else
     call nemsio_writerecgrb4w34(gfile,jrec,data,ios,itr=itr,zhour=zhour)
   endif
   if ( ios .ne.0 ) then
     if(present(iret)) then
       iret=ios
       return
     else
       call nemsio_stop
     endif
   endif
   if(present(iret)) iret=0
!
   return
  end subroutine nemsio_writerec4w34
!------------------------------------------------------------------------------
  subroutine nemsio_writerec8w34(gfile,jrec,data,iret,itr,zhour)
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
! abstract: write nemsio a 2D 64 bits array data into bin file using record number
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
    implicit none
    type(nemsio_gfile),intent(in)                 :: gfile
    integer(nemsio_intkind),intent(in)            :: jrec
    real(nemsio_dblekind),intent(in)              :: data(:)
    integer(nemsio_intkind),optional,intent(out)  :: iret
    integer(nemsio_intkind),optional,intent(in)   :: itr
    real(nemsio_realkind),optional,intent(in)     :: zhour
    real(nemsio_realkind),allocatable             :: datatmp(:)
    integer ios
!------------------------------------------------------------
! write 4 byte rec
!------------------------------------------------------------
!
   if(present(iret)) iret=-64
!
   call nemsio_getgfile(gfile,iret)
!
   if ( mygdatatype .eq. 'bin4') then
     call nemsio_writerecbin4d8(gfile,jrec,data,ios)
   else if ( mygdatatype .eq. 'bin8') then
     call nemsio_writerecbin8d8(gfile,jrec,data,ios)
   else
     allocate(datatmp(myfieldsize))
     datatmp(1:myfieldsize)=data(1:myfieldsize)
     call nemsio_writerecgrb4w34(gfile,jrec,datatmp,ios,itr=itr,zhour=zhour)
   endif
   if ( ios .ne.0 ) then
     if(present(iret)) then
       iret=ios
       return
     else
       call nemsio_stop
     endif
   endif
   if(mygdatatype .eq. 'grib')  deallocate(datatmp)
   if(present(iret)) iret=0
!
   return
  end subroutine nemsio_writerec8w34
!------------------------------------------------------------------------------
  subroutine nemsio_writerecv4w34(gfile,name,levtyp,lev,data,iret, &
             itr,zhour)
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
! abstract: write nemsio a 2D 32 bits array data into bin file using record number
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
    implicit none
    type(nemsio_gfile),intent(in)                 :: gfile
    character(*),intent(in)                       :: name
    character(*),optional,intent(in)              :: levtyp
    integer(nemsio_intkind),optional,intent(in)   :: lev
    real(nemsio_realkind),intent(in)              :: data(:)
    integer(nemsio_intkind),optional,intent(out)  :: iret
    integer(nemsio_intkind),optional,intent(in)   :: itr
    real(nemsio_realkind),optional,intent(in)     :: zhour
    integer ios
!------------------------------------------------------------
! read 4 byte rec
!------------------------------------------------------------
!
   if(present(iret)) iret=-65
!
   call nemsio_getgfile(gfile,iret)
!
   if ( mygdatatype .eq. 'bin4') then
     call nemsio_writerecvbin4d4(gfile,name,levtyp,lev,data,ios)
   else if ( mygdatatype .eq. 'bin8') then
     call nemsio_writerecvbin8d4(gfile,name,levtyp,lev,data,ios)
   else
     call nemsio_writerecvgrb4w34(gfile,name,levtyp,lev,data,ios,itr=itr,        &
          zhour=zhour)
   endif
   if ( ios .ne.0 ) then
     if(present(iret)) then
       iret=ios
       return
     else
       call nemsio_stop
     endif
   endif
   if(present(iret)) iret=0
!
   return
  end subroutine nemsio_writerecv4w34
!------------------------------------------------------------------------------
  subroutine nemsio_writerecv8w34(gfile,name,levtyp,lev,data,iret, &
             itr,zhour)
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
! abstract: write nemsio a 2D 32 bits array data into bin file using record number
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
    implicit none
    type(nemsio_gfile),intent(in)                 :: gfile
    character(*),intent(in)                       :: name
    character(*),optional,intent(in)              :: levtyp
    integer(nemsio_intkind),optional,intent(in)   :: lev
    real(nemsio_dblekind),intent(in)              :: data(:)
    integer(nemsio_intkind),optional,intent(out)  :: iret
    integer(nemsio_intkind),optional,intent(in)   :: itr
    real(nemsio_realkind),optional,intent(in)     :: zhour
    real(nemsio_realkind),allocatable             :: datatmp(:)
    integer ios
!------------------------------------------------------------
! read 4 byte rec
!------------------------------------------------------------
!
    if(present(iret)) iret=-65
!
   call nemsio_getgfile(gfile,iret)
!
   if ( mygdatatype .eq. 'bin4') then
     call nemsio_writerecvbin4d8(gfile,name,levtyp,lev,data,ios)
   else if ( mygdatatype .eq. 'bin8') then
     call nemsio_writerecvbin8d8(gfile,name,levtyp,lev,data,ios)
   else
     allocate(datatmp(myfieldsize))
     datatmp(1:myfieldsize)=data(1:myfieldsize)
     call nemsio_writerecvgrb4w34(gfile,name,levtyp,lev,datatmp,ios,itr=itr,    &
         zhour=zhour)
   endif
   if ( ios .ne.0 ) then
     if(present(iret)) then
       iret=ios
       return
     else
       call nemsio_stop
     endif
   endif
   if(mygdatatype .eq. 'grib')   deallocate(datatmp)
   if(present(iret)) iret=0
!
   return
   end subroutine nemsio_writerecv8w34
!------------------------------------------------------------------------------

!*****************   write out bin data set :  ********************************

!------------------------------------------------------------------------------
  subroutine nemsio_writerecbin4d4(gfile,jrec,data,iret)
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
! abstract: read nemsio data (bin) by record number into a 2D 32 bits array
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
    implicit none
    type(nemsio_gfile),intent(in)                 :: gfile
    integer(nemsio_intkind),intent(in)            :: jrec
    real(nemsio_realkind),intent(in)              :: data(:)
    integer(nemsio_intkind),intent(out)           :: iret
    integer(nemsio_intkind8) :: iskip,iwrite,nwrite
!
    iret=-71
    if(size(data)/=myfieldsize) then
      print *,'ERROR: input data size ',size(data),' is not match the data domain ', &
        myfieldsize,'please check dimension and nframe'
      return
    endif
    iskip=mytlmeta+int(jrec-1,8)*int(nemsio_realkind*myfieldsize+8,8)
    iwrite=int(nemsio_realkind,8)*int(size(data),8)
    call bafrwritel(myflunit,iskip,iwrite,nwrite,data)
    if(nwrite.lt.iwrite) return
    iret=0

    return
  end subroutine nemsio_writerecbin4d4
!------------------------------------------------------------------------------
  subroutine nemsio_writerecbin4d8(gfile,jrec,data,iret)
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
! abstract: read nemsio data (bin) by record number into a 2D 32 bits array
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
    implicit none
    type(nemsio_gfile),intent(in)                 :: gfile
    integer(nemsio_intkind),intent(in)            :: jrec
    real(nemsio_dblekind),intent(in)              :: data(:)
    integer(nemsio_intkind),intent(out)           :: iret
    real(nemsio_realkind),allocatable             :: data4(:)
    integer(nemsio_intkind8) :: iskip,iwrite,nwrite

    iret=-71
    if(size(data)/=myfieldsize) then
      print *,'ERROR: input data size ',size(data),' is not match the data domain ', &
        myfieldsize,'please check dimension and nframe'
      return
    endif
    allocate(data4(size(data)) )
    data4=data
    iskip=mytlmeta+int(jrec-1,8)*int(nemsio_realkind*myfieldsize+8,8)
    iwrite=int(nemsio_realkind,8)*int(size(data4),8)
    call bafrwritel(myflunit,iskip,iwrite,nwrite,data4)
    if(nwrite.lt.iwrite) return
    iret=0

    return
  end subroutine nemsio_writerecbin4d8
!------------------------------------------------------------------------------
 subroutine nemsio_writerecvbin4d4(gfile,name,levtyp,lev,data,iret)
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
! abstract: read nemsio data (bin) by record number into a 2D 32 bits array
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
    implicit none
    type(nemsio_gfile),intent(in)                 :: gfile
    character(*),intent(in)                       :: name
    character(*),optional,intent(in)              :: levtyp
    integer(nemsio_intkind),optional,intent(in)   :: lev
    real(nemsio_realkind),intent(in)              :: data(:)
    integer(nemsio_intkind),intent(out)           :: iret
    integer :: jrec, ierr
    integer(nemsio_intkind8) :: iskip,iwrite,nwrite

    iret=-73
    call nemsio_searchrecv(gfile,jrec,name,levtyp,lev,ierr)
    if ( ierr .ne. 0) return
    if(size(data)/=myfieldsize) then
      print *,'ERROR: input data size ',size(data),' is not match the data domain ', &
        myfieldsize,'please check dimension and nframe'
      return
    endif
    iskip=mytlmeta+int(jrec-1,8)*int(nemsio_realkind*myfieldsize+8,8)
    iwrite=int(nemsio_realkind,8)*int(size(data),8)
    call bafrwritel(myflunit,iskip,iwrite,nwrite,data)
    if(nwrite.lt.iwrite) return
    iret=0

    return
  end subroutine nemsio_writerecvbin4d4
!------------------------------------------------------------------------------
 subroutine nemsio_writerecvbin4d8(gfile,name,levtyp,lev,data,iret)
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
! abstract: read nemsio data (bin) by record number into a 2D 32 bits array
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
    implicit none
    type(nemsio_gfile),intent(in)                 :: gfile
    character(*),intent(in)                       :: name
    character(*),optional,intent(in)              :: levtyp
    integer(nemsio_intkind),optional,intent(in)   :: lev
    real(nemsio_dblekind),intent(in)              :: data(:)
    integer(nemsio_intkind),intent(out)           :: iret
    real(nemsio_realkind),allocatable        :: data4(:)
    integer :: jrec, ierr
    integer(nemsio_intkind8) :: iskip,iwrite,nwrite

    iret=-73
    allocate(data4(size(data)) )
    data4=data
    call nemsio_searchrecv(gfile,jrec,name,levtyp,lev,ierr)
    if ( ierr .ne. 0) return
    if(size(data)/=myfieldsize) then
      print *,'ERROR: input data size ',size(data),' is not match the data domain ', &
        myfieldsize,'please check dimension and nframe'
      return
    endif
    iskip=mytlmeta+int(jrec-1,8)*int(nemsio_realkind*myfieldsize+8,8)
    iwrite=int(nemsio_realkind,8)*int(size(data4),8)
    call bafrwritel(myflunit,iskip,iwrite,nwrite,data4)
    if(nwrite.lt.iwrite) return
    iret=0

    return
  end subroutine nemsio_writerecvbin4d8
!------------------------------------------------------------------------------
  subroutine nemsio_writerecbin8d4(gfile,jrec,data,iret)
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
! abstract: read nemsio data (bin) by record number into a 2D 32 bits array
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
    implicit none
    type(nemsio_gfile),intent(in)                 :: gfile
    integer(nemsio_intkind),intent(in)            :: jrec
    real(nemsio_realkind),intent(in)              :: data(:)
    integer(nemsio_intkind),intent(out)           :: iret
    real(nemsio_dblekind),allocatable        :: data8(:)
    integer(nemsio_intkind8) :: iskip,iwrite,nwrite

    iret=-72
    if(size(data)/=myfieldsize) then
      print *,'ERROR: input data size ',size(data),' is not match the data domain ', &
        myfieldsize,'please check dimension and nframe'
      return
    endif
    allocate(data8(size(data)) )
    data8=data
    iskip=mytlmeta+int(jrec-1,8)*int(nemsio_dblekind*myfieldsize+8,8)
    iwrite=int(nemsio_dblekind,8)*int(size(data8),8)
    call bafrwritel(myflunit,iskip,iwrite,nwrite,data8)
    if(nwrite.lt.iwrite) return
    iret=0

    return
  end subroutine nemsio_writerecbin8d4
!------------------------------------------------------------------------------
  subroutine nemsio_writerecbin8d8(gfile,jrec,data,iret)
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
! abstract: read nemsio data (bin) by record number into a 2D 32 bits array
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
    implicit none
    type(nemsio_gfile),intent(in)                 :: gfile
    integer(nemsio_intkind),intent(in)            :: jrec
    real(nemsio_dblekind),intent(in)              :: data(:)
    integer(nemsio_intkind),intent(out)           :: iret
    integer(nemsio_intkind8) :: iskip,iwrite,nwrite

    iret=-72
    if(size(data)/=myfieldsize) then
      print *,'ERROR: input data size ',size(data),' is not match the data domain ', &
        myfieldsize,'please check dimension and nframe'
      return
    endif
    iskip=mytlmeta+int(jrec-1,8)*int(nemsio_dblekind*myfieldsize+8,8)
    iwrite=int(nemsio_dblekind,8)*int(size(data),8)
    call bafrwritel(myflunit,iskip,iwrite,nwrite,data)
    if(nwrite.lt.iwrite) return
    iret=0

    return
  end subroutine nemsio_writerecbin8d8
!------------------------------------------------------------------------------
  subroutine nemsio_writerecvbin8d4(gfile,name,levtyp,lev,data,iret)
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
! abstract: read nemsio data (bin) by record number into a 2D 32 bits array
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
    implicit none
    type(nemsio_gfile),intent(in)                 :: gfile
    character(*),intent(in)                       :: name
    character(*),optional,intent(in)              :: levtyp
    integer(nemsio_intkind),optional,intent(in)   :: lev
    real(nemsio_realkind),intent(in)              :: data(:)
    integer(nemsio_intkind),intent(out)           :: iret
    real(nemsio_dblekind),allocatable        :: data8(:)
    integer :: jrec, ierr
    integer(nemsio_intkind8) :: iskip,iwrite,nwrite

    iret=-74
    if(size(data)/=myfieldsize) then
      print *,'ERROR: input data size ',size(data),' is not match the data domain ', &
        myfieldsize,'please check dimension and nframe'
      return
    endif
    allocate(data8(size(data)) )
    data8=data
    call nemsio_searchrecv(gfile,jrec,name,levtyp,lev,ierr)
    if ( ierr .ne. 0) return
    iskip=mytlmeta+int(jrec-1,8)*int(nemsio_dblekind*myfieldsize+8,8)
    iwrite=int(nemsio_dblekind,8)*int(size(data8),8)
    call bafrwritel(myflunit,iskip,iwrite,nwrite,data8)
    if(nwrite.lt.iwrite) return
    iret=0

    return
  end subroutine nemsio_writerecvbin8d4

!------------------------------------------------------------------------------
  subroutine nemsio_writerecvbin8d8(gfile,name,levtyp,lev,data,iret)
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
! abstract: read nemsio data (bin) by record number into a 2D 32 bits array
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
    implicit none
    type(nemsio_gfile),intent(in)                 :: gfile
    character(*),intent(in)                       :: name
    character(*),optional,intent(in)              :: levtyp
    integer(nemsio_intkind),optional,intent(in)   :: lev
    real(nemsio_dblekind),intent(in)              :: data(:)
    integer(nemsio_intkind),intent(out)           :: iret
    integer :: jrec, ierr
    integer(nemsio_intkind8) :: iskip,iwrite,nwrite

    iret=-74
    if(size(data)/=myfieldsize) then
      print *,'ERROR: input data size ',size(data),' is not match the data domain ', &
        myfieldsize,'please check dimension and nframe'
      return
    endif
    call nemsio_searchrecv(gfile,jrec,name,levtyp,lev,ierr)
    if ( ierr .ne. 0) return
    iskip=mytlmeta+int(jrec-1,8)*int(nemsio_dblekind*myfieldsize+8,8)
    iwrite=int(nemsio_dblekind,8)*int(size(data),8)
    call bafrwritel(myflunit,iskip,iwrite,nwrite,data)
    if(nwrite.lt.iwrite) return
    iret=0

    return
  end subroutine nemsio_writerecvbin8d8
!------------------------------------------------------------------------------
!
!*****************   write out grb data set :  ********************************
!
!------------------------------------------------------------------------------
  subroutine nemsio_writerecgrb4w34(gfile,jrec,data,iret,idrt,itr,zhour)
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
! abstract: read nemsio data by record number into a 2D 32bits array,
!           using w3_4 library to compile
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
    implicit none
    type(nemsio_gfile),intent(in)               :: gfile
    integer(nemsio_intkind),intent(in)          :: jrec
    real(nemsio_realkind),intent(in)            :: data(:)
    integer(nemsio_intkind),optional,intent(out):: iret
    integer(nemsio_intkind),optional,intent(in) :: idrt
    integer(nemsio_intkind),optional,intent(in) :: itr
    real(nemsio_realkind),optional,intent(in)   :: zhour
    type(nemsio_grbmeta)         :: grbmeta
    integer(nemsio_intkind)      :: N=nemsio_kpds_intfill
    integer(nemsio_intkind)      :: nc,i
    integer(nemsio_intkind)      :: ios,w34,ibms
!---
    real(nemsio_realkind)      :: mymax
!------------------------------------------------------------
! set up grib meta
!------------------------------------------------------------
    if(present(iret)) iret=-75
    w34=1
!------------------------------------------------------------
! set up grib meta lbms
!------------------------------------------------------------
    ibms=0
    if(any(abs(data)>=nemsio_undef_grb)) ibms=1
!
    if(present(idrt)) then
      call nemsio_setrqst(gfile,grbmeta,ios,jrec=jrec,w34=w34, &
           idrt=idrt,itr=itr,zhour=zhour,ibms=ibms)
    else
      call nemsio_setrqst(gfile,grbmeta,ios,jrec=jrec,w34=w34, &
           itr=itr,zhour=zhour,ibms=ibms)
    endif
    if (ios.ne.0) then
       if ( present(iret))  then
         iret=ios
         return
       else
         call nemsio_stop
       endif
    endif
!
    grbmeta%lbms=.true.
    where(abs(data)>=nemsio_undef_grb) grbmeta%lbms=.false.
    mymax=minval(data)
    do i=1,myfieldsize
     if(abs(data(i))<nemsio_undef_grb) then
       if(data(i) .gt.mymax) mymax=data(i)
     endif
    enddo
!
!------------------------------------------------------------
! check precision -- for pressure now
!------------------------------------------------------------
    if ( grbmeta%jpds(5).eq.1 .and. grbmeta%jpds(6).eq.109 ) then
     grbmeta%jpds(22)=min(int(6-log10(mymax)),4)
    endif
!------------------------------------------------------------
! get data from putgb _w34
!------------------------------------------------------------
    call putgb(myflunit,grbmeta%jf,grbmeta%jpds,grbmeta%jgds, &
      grbmeta%lbms,data,ios)
    deallocate(grbmeta%lbms)
    if(ios.ne.0) then
       if ( present(iret))  then
          print *,'putgb_ios=',ios
         iret=ios
         return
       else
         call nemsio_stop
       endif
    endif
    if(present(iret)) iret=0
  end subroutine nemsio_writerecgrb4w34
!------------------------------------------------------------------------------
  subroutine nemsio_writerecgrb4(gfile,jrec,data,iret,idrt,itr,zhour)
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
! abstract: read nemsio data by record number into a 2D 32bits array, 
!           using w3_d library to compile
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
    implicit none
    type(nemsio_gfile),intent(in)               :: gfile
    integer(nemsio_intkind),intent(in)          :: jrec
    real(nemsio_realkind),intent(in)            :: data(:)
    integer(nemsio_intkind),optional,intent(out):: iret
    integer(nemsio_intkind),optional,intent(in) :: idrt
    integer(nemsio_intkind),optional,intent(in) :: itr
    real(nemsio_realkind),optional,intent(in)   :: zhour
    real(nemsio_dblekind),allocatable        :: data8(:)
    type(nemsio_grbmeta)         :: grbmeta
    integer(nemsio_intkind)      :: N=nemsio_kpds_intfill
    integer(nemsio_intkind)      :: nc,i,nc1
    integer(nemsio_intkind)      :: ios,ibms
    real(nemsio_dblekind)         :: mymax
!------------------------------------------------------------
! set up grib meta 
!------------------------------------------------------------
    if(present(iret)) iret=-77
!------------------------------------------------------------
! set up grib meta ibms
!------------------------------------------------------------
    ibms=0
!
    allocate(data8(size(data)) )
    data8=data
    if(any(abs(data8)>=nemsio_undef_grb))  ibms=1
!
!------------------------------------------------------------
! set up grib meta data
!------------------------------------------------------------
    if(present(idrt)) then
      call nemsio_setrqst(gfile,grbmeta,ios,jrec=jrec,idrt=idrt, &
           itr=itr,zhour=zhour,ibms=ibms)
    else
      call nemsio_setrqst(gfile,grbmeta,ios,jrec=jrec, &
           itr=itr,zhour=zhour,ibms=ibms)
    endif
    if (ios.ne.0) then
       if ( present(iret))  then
         iret=ios
         return
       else
         call nemsio_stop
       endif
    endif
!
!------------------------------------------------------------
! set up lbms 
!------------------------------------------------------------
    grbmeta%lbms=.true.
    where(abs(data8)>=nemsio_undef_grb) grbmeta%lbms=.false.
    mymax=minval(data8)
    do i=1,myfieldsize
     if(abs(data8(i))<nemsio_undef_grb) then
        if(data8(i) .gt.mymax) mymax=data8(i)
     endif
    enddo
!     write(0,*)'in writerecgrb4,max=',mymax,'imb=',ibms, &
!      'size(data)=',size(data),'size(lbms)=',size(grbmeta%lbms), &
!      grbmeta%lbms(1:15),data8(1:15)
!
!------------------------------------------------------------
! check precision -- for pressure now
!------------------------------------------------------------
    if ( grbmeta%jpds(5).eq.1 .and. grbmeta%jpds(6).eq.109 ) then
     grbmeta%jpds(22)=min(int(6-log10(mymax)),4)
    endif
!------------------------------------------------------------
! get data from putgb _w3d
!------------------------------------------------------------
!    allocate(data8(size(data)) )
!    data8=data
!    write(0,*)'in writerecgrb4,before putgb=',grbmeta%lbms(1:15)
    call putgb(myflunit,grbmeta%jf,grbmeta%jpds,grbmeta%jgds, &
      grbmeta%lbms,data8,ios)
!    write(0,*)'in writerecgrb4,after putgb,iret=',ios,'jpds=',grbmeta%jpds(1:25), &
!      'gds=',grbmeta%jgds(1:25),'data=',maxval(data8),minval(data8)
    deallocate(grbmeta%lbms)
    if(ios.ne.0) then
       if ( present(iret))  then
          print *,'putgb_ios=',ios
         iret=ios
         return
       else
         call nemsio_stop
       endif
    endif
    if(present(iret)) iret=0
  end subroutine nemsio_writerecgrb4
!------------------------------------------------------------------------------
  subroutine nemsio_writerecgrb8(gfile,jrec,data8,iret,idrt,itr,zhour)
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
! abstract: read nemsio data by record number into a 2D 64bits array, 
!           using w3_d library to compile
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
    implicit none
    type(nemsio_gfile),intent(in)               :: gfile
    integer(nemsio_intkind),intent(in)          :: jrec
    real(nemsio_dblekind),intent(in)            :: data8(:)
    integer(nemsio_intkind),optional,intent(out):: iret
    integer(nemsio_intkind),optional,intent(in) :: idrt
    integer(nemsio_intkind),optional,intent(in) :: itr
    real(nemsio_realkind),optional,intent(in)   :: zhour
    type(nemsio_grbmeta)         :: grbmeta
    integer(nemsio_intkind)      :: N=nemsio_kpds_intfill
    integer(nemsio_intkind)      :: nc,i
    integer(nemsio_intkind)      :: ios,ibms
!---
    real(nemsio_dblekind)      :: mymax
!------------------------------------------------------------
! set up grib meta 
!------------------------------------------------------------
    if(present(iret)) iret=-77
!------------------------------------------------------------
! set up grib meta lbms
!------------------------------------------------------------
    ibms=0
    if(any(abs(data8)>=nemsio_undef_grb))  ibms=1
!
    if(present(idrt)) then
      call nemsio_setrqst(gfile,grbmeta,ios,jrec=jrec,idrt=idrt, &
           itr=itr,zhour=zhour,ibms=ibms)
    else
      call nemsio_setrqst(gfile,grbmeta,ios,jrec=jrec, &
           itr=itr,zhour=zhour,ibms=ibms)
    endif
    if (ios.ne.0) then
       if ( present(iret))  then
         iret=ios
         return
       else
         call nemsio_stop
       endif
    endif
!
    grbmeta%lbms=.true.
    where(abs(data8)>=nemsio_undef_grb) grbmeta%lbms=.false.
    mymax=minval(data8)
    do i=1,myfieldsize
     if(abs(data8(i))<nemsio_undef_grb) then
        if(data8(i) .gt.mymax) mymax=data8(i)
     endif
    enddo
!
!------------------------------------------------------------
! check precision -- for pressure now
!------------------------------------------------------------
    if ( grbmeta%jpds(5).eq.1 .and. grbmeta%jpds(6).eq.109 ) then
     grbmeta%jpds(22)=min(int(6-log10(mymax)),4)
    endif
!------------------------------------------------------------
! get data from putgb _w3d
!------------------------------------------------------------
    call putgb(myflunit,grbmeta%jf,grbmeta%jpds,grbmeta%jgds, &
      grbmeta%lbms,data8,ios)
    deallocate(grbmeta%lbms)
    if(ios.ne.0) then
       if ( present(iret))  then
          print *,'putgb_ios=',ios
         iret=ios
         return
       else
         call nemsio_stop
       endif
    endif
    if(present(iret)) iret=0
  end subroutine nemsio_writerecgrb8
!------------------------------------------------------------------------------
  subroutine nemsio_writerecvgrb4w34(gfile,vname,vlevtyp,vlev,data,iret,idrt, &
             itr,zhour)
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
! abstract: read nemsio data by field name into a 2D 32bits array,
!           using w3_4 library to compile
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
    implicit none
    type(nemsio_gfile),intent(in)               :: gfile
    character*(*),intent(in)                    :: vname,vlevtyp
    integer(nemsio_intkind),intent(in)          :: vlev
    real(nemsio_realkind),intent(in)            :: data(:)
    integer(nemsio_intkind),optional,intent(out):: iret
    integer(nemsio_intkind),optional,intent(in) :: idrt
    integer(nemsio_intkind),optional,intent(in) :: itr
    real(nemsio_realkind),optional,intent(in)   :: zhour
    type(nemsio_grbmeta)         :: grbmeta
    integer(nemsio_intkind)      :: N=nemsio_kpds_intfill
    integer(nemsio_intkind)      :: nc,i
    integer(nemsio_intkind)      :: ios,w34,ibms
    real(nemsio_realkind)        :: mymax
!------------------------------------------------------------
! set up grib meta
!------------------------------------------------------------
    if(present(iret)) iret=-76
!------------------------------------------------------------
! set up grib meta lbms
!------------------------------------------------------------
    ibms=0
    if(any(abs(data)>=nemsio_undef_grb))  ibms=1
!
    w34=1
    if(present(idrt)) then
      call nemsio_setrqst(gfile,grbmeta,ios,vname=vname, &
        vlevtyp=vlevtyp, vlev=vlev, w34=w34, idrt=idrt,  &
        itr=itr,zhour=zhour,ibms=ibms)
    else
      call nemsio_setrqst(gfile,grbmeta,ios,vname=vname, &
        vlevtyp=vlevtyp, vlev=vlev, w34=w34,itr=itr,     &
        zhour=zhour,ibms=ibms)
    endif
    if (ios.ne.0) then
       if ( present(iret))  then
         iret=ios
         return
       else
         call nemsio_stop
       endif
    endif
!
    grbmeta%lbms=.true.
    where(abs(data)>=nemsio_undef_grb) grbmeta%lbms=.false.
    mymax=minval(data)
    do i=1,myfieldsize
     if(abs(data(i))<nemsio_undef_grb) then
        if(data(i) .gt.mymax) mymax=data(i)
     endif
    enddo
!
!------------------------------------------------------------
! check precision -- for pressure now
!------------------------------------------------------------
    if ( grbmeta%jpds(5).eq.1 .and. grbmeta%jpds(6).eq.109 ) then
     grbmeta%jpds(22)=min(int(6-log10(mymax)),4)
    endif
!------------------------------------------------------------
! get data from putgb _w34
!------------------------------------------------------------
    call putgb(myflunit,grbmeta%jf,grbmeta%jpds,grbmeta%jgds, &
      grbmeta%lbms,data,ios)
    deallocate(grbmeta%lbms)
    if(ios.ne.0) then
       if ( present(iret))  then
          print *,'putgb_ios=',ios
         iret=ios
         return
       else
         call nemsio_stop
       endif
    endif
    if(present(iret)) iret=0
  end subroutine nemsio_writerecvgrb4w34
!------------------------------------------------------------------------------
  Subroutine nemsio_writerecvgrb4(gfile,vname,vlevtyp,vlev,data,iret,idrt, &
             itr,zhour)
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
! abstract: read nemsio data by field name into a 2D 32bits array, 
!           using w3_d library to compile
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
    implicit none
    type(nemsio_gfile),intent(in)               :: gfile
    character*(*),intent(in)                   :: vname,vlevtyp
    integer(nemsio_intkind),intent(in)          :: vlev
    real(nemsio_realkind),intent(in)            :: data(:)
    integer(nemsio_intkind),optional,intent(out):: iret
    integer(nemsio_intkind),optional,intent(in) :: idrt
    integer(nemsio_intkind),optional,intent(in) :: itr
    real(nemsio_realkind),optional,intent(in)   :: zhour
    real(nemsio_dblekind),allocatable        :: data8(:)
    type(nemsio_grbmeta)         :: grbmeta
    integer(nemsio_intkind)      :: N=nemsio_kpds_intfill
    integer(nemsio_intkind)      :: nc,i
    integer(nemsio_intkind)      :: ios,ibms
    real(nemsio_dblekind)        :: mymax
!------------------------------------------------------------
! set up grib meta 
!------------------------------------------------------------
    if(present(iret)) iret=-78
!------------------------------------------------------------
! set up grib meta lbms
!------------------------------------------------------------
    ibms=0
    if(any(abs(data)>=nemsio_undef_grb))  ibms=1
!
    if(present(idrt)) then
      call nemsio_setrqst(gfile,grbmeta,ios,vname=vname, &
        vlevtyp=vlevtyp, vlev=vlev, idrt=idrt,itr=itr,   &
        zhour=zhour,ibms=ibms)
    else
      call nemsio_setrqst(gfile,grbmeta,ios,vname=vname, &
        vlevtyp=vlevtyp, vlev=vlev,itr=itr,zhour=zhour,ibms=ibms)
    endif
    if (ios.ne.0) then
       if ( present(iret))  then
         iret=ios
         return
       else
         call nemsio_stop
       endif
    endif
!
    allocate(data8(size(data)) )
    data8=data
!
    grbmeta%lbms=.true.
    where(abs(data8)>=nemsio_undef_grb) grbmeta%lbms=.false.
    mymax=minval(data8)
    do i=1,myfieldsize
     if(abs(data8(i))<nemsio_undef_grb) then
       if(data8(i) .gt.mymax) mymax=data8(i)
     endif
    enddo
!
!------------------------------------------------------------
! check precision -- for pressure now
!------------------------------------------------------------
    if ( grbmeta%jpds(5).eq.1 .and. grbmeta%jpds(6).eq.109 ) then
     grbmeta%jpds(22)=min(int(6-log10(mymax)),4)
    endif
!------------------------------------------------------------
! get data from putgb _w3d
!------------------------------------------------------------
    call putgb(myflunit,grbmeta%jf,grbmeta%jpds,grbmeta%jgds, &
      grbmeta%lbms,data8,ios)
    deallocate(grbmeta%lbms)
    if(ios.ne.0) then
       if ( present(iret))  then
          print *,'putgb_ios=',ios
         iret=ios
         return
       else
         call nemsio_stop
       endif
    endif
    if(present(iret)) iret=0
  end subroutine nemsio_writerecvgrb4
!------------------------------------------------------------------------------
  subroutine nemsio_writerecvgrb8(gfile,vname,vlevtyp,vlev,data8,iret,idrt,itr, &
       zhour)
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
! abstract: read nemsio data by field name into a 2D 64bits array, 
!           using w3_d library to compile
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
    implicit none
    type(nemsio_gfile),intent(in)               :: gfile
    character*(*),intent(in)                   :: vname,vlevtyp
    integer(nemsio_intkind),intent(in)          :: vlev
    real(nemsio_dblekind),intent(in)            :: data8(:)
    integer(nemsio_intkind),optional,intent(out):: iret
    integer(nemsio_intkind),optional,intent(in) :: idrt
    integer(nemsio_intkind),optional,intent(in) :: itr
    real(nemsio_realkind),optional,intent(in)   :: zhour
    type(nemsio_grbmeta)         :: grbmeta
    integer(nemsio_intkind)      :: N=nemsio_kpds_intfill
    integer(nemsio_intkind)      :: nc,i
    integer(nemsio_intkind)      :: ios,ibms
    real(nemsio_dblekind)        :: mymax
!------------------------------------------------------------
! set up grib meta 
!------------------------------------------------------------
    if(present(iret)) iret=-78
!------------------------------------------------------------
! set up grib meta lbms
!------------------------------------------------------------
    ibms=0
    if(any(abs(data8)>=nemsio_undef_grb))  ibms=1
!
    if(present(idrt)) then
      call nemsio_setrqst(gfile,grbmeta,ios,vname=vname, &
        vlevtyp=vlevtyp, vlev=vlev, idrt=idrt,itr=itr,   &
        zhour=zhour,ibms=ibms)
    else
      call nemsio_setrqst(gfile,grbmeta,ios,vname=vname, &
        vlevtyp=vlevtyp, vlev=vlev,itr=itr,zhour=zhour,ibms=ibms)
    endif
    if (ios.ne.0) then
       if ( present(iret))  then
         iret=ios
         return
       else
         call nemsio_stop
       endif
    endif
!
    grbmeta%lbms=.true.
    where(abs(data8)>=nemsio_undef_grb) grbmeta%lbms=.false.
    mymax=minval(data8)
    do i=1,myfieldsize
     if(abs(data8(i))<nemsio_undef_grb) then
        if(data8(i) .gt.mymax) mymax=data8(i)
     endif
    enddo
!
!------------------------------------------------------------
! check precision -- for pressure now
!------------------------------------------------------------
    if ( grbmeta%jpds(5).eq.1 .and. grbmeta%jpds(6).eq.109 ) then
     grbmeta%jpds(22)=min(int(6-log10(mymax)),4)
    endif
!------------------------------------------------------------
! get data from putgb _w3d
!------------------------------------------------------------
    call putgb(myflunit,grbmeta%jf,grbmeta%jpds,grbmeta%jgds, &
      grbmeta%lbms,data8,ios)
    deallocate(grbmeta%lbms)
    if(ios.ne.0) then
       if ( present(iret))  then
          print *,'putgb_ios=',ios
         iret=ios
         return
       else
         call nemsio_stop
       endif
    endif
    if(present(iret)) iret=0
  end subroutine nemsio_writerecvgrb8
!----------------------------------------------------------------------------
end module nemsio_write
