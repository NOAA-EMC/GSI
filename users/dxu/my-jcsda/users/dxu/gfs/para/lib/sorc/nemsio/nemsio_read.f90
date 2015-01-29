!----------------------------------------------------------------------------
module nemsio_read
!
!$$$ documentation clock
!
! module: nemsio_read      read data fields from a nemsio file
!  Programmer: J. Wang          date: 2011-01-13
!
! abstract: this module provides subroutines to read data fields out of a
!           nemsio file. The data file could be 'bin4','bin8', or 'grib',
!           The dat field could be read out by the record number
!           or by given the data field name, level type and level. Overload 
!           interfaces are provided to handle different data type (real(4)
!           or real(8) ) of the array that holds the data.
!
! Possible return code
!          0   Successful call
!         -31  get dimension ffrom gfile
!         -32  read data field by record number using w3d
!         -33  read data field by given data field name,level type and level using w3d
!         -34  read data field by record number using w34
!         -35  read data field by given data field name,level type and level using w34
!         -41  read data field by record number from 4 byte real binary file 
!         -42  read data field by record number from 8 byte real binary file 
!         -43  read data field by field name,levtyp and lev from 4 byte real binary file 
!         -44  read data field by field name,levtyp and lev from 8 byte real binary file 
!         -45  read data field by record number using w34 from grib data
!         -46  read data field by field name,level type and level using w34 from grib data
!         -47  read data field by record number using w3d from grib data
!         -48  read data field by field name,level type and level using w3d from grib data
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
  interface nemsio_readrec
    module procedure nemsio_readrec4
    module procedure nemsio_readrec8
  end interface nemsio_readrec
!
  interface nemsio_readrecv
    module procedure nemsio_readrecv4
    module procedure nemsio_readrecv8
  end interface nemsio_readrecv
!
  interface nemsio_readrecw34
    module procedure nemsio_readrec4w34
    module procedure nemsio_readrec8w34
  end interface nemsio_readrecw34
!
  interface nemsio_readrecvw34
    module procedure nemsio_readrecv4w34
    module procedure nemsio_readrecv8w34
  end interface nemsio_readrecvw34
!
!public mehtods
  public nemsio_readrec,nemsio_readrecv,nemsio_readrecw34,nemsio_readrecvw34
!
!---------------------------------------------------------
! local data
!
  character(8) :: mygdatatype
  character(255) :: mygfname
  integer mydimx,mydimy,mydimz,mynframe,myfieldsize,mytlmeta,myflunit
  character(255),save :: mygfnamep=''
  integer,save :: mymbuf,mynnum,mynlen,mymnum
  character,allocatable,save  :: mycbuf(:)
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
    integer ios
! 
    if(present(iret)) iret= -31
!
    call nemsio_getfilehead(gfile,iret=ios,gdatatype=mygdatatype,dimx=mydimx,   &
           dimy=mydimy,dimz=mydimz,nframe=mynframe,tlmeta=mytlmeta,              &
           flunit=myflunit,gfname=mygfname )
    if(ios/=0) then
       if(present(iret)) then
         iret=ios
         return
       else
         print *,'ERROR: NEMSIO readrec in getting file head'
         stop
       endif
    endif
  
    myfieldsize=(mydimx+2*mynframe)*(mydimy+2*mynframe)
    if(trim(mygfnamep)/=trim(mygfname)) then 
       mygfnamep=mygfname
       if(trim(mygdatatype)=='grib') then
         mymbuf=256*1024
         mynnum=0
         mynlen=0
         mymnum=-1
         if(allocated(mycbuf)) deallocate(mycbuf)
         allocate(mycbuf(mymbuf))
       endif
     endif
!
     if(present(iret)) iret=0
!
  end subroutine nemsio_getgfile
!------------------------------------------------------------------------------
!
!------------------------------------------------------------------------------
  subroutine nemsio_readrec4(gfile,jrec,data,nframe,iret)
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
! abstract: read nemsio data by record number into a 2D 32 bits array
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
    implicit none
    type(nemsio_gfile),intent(in)                 :: gfile
    integer(nemsio_intkind),intent(in)            :: jrec
    real(nemsio_realkind),intent(inout)           :: data(:)
    integer(nemsio_intkind),optional,intent(out)  :: iret
    integer(nemsio_intkind),optional,intent(in)  :: nframe
    real(nemsio_realkind),allocatable             :: datatmp(:)
    real(nemsio_dblekind),allocatable            :: datatmp8(:)
    integer :: i,j,ios
!------------------------------------------------------------
! read 4 byte rec
!------------------------------------------------------------
   if(present(iret)) iret=-32
!---
   call nemsio_getgfile(gfile,iret)
!---
   if ( mygdatatype .eq. 'bin4') then 
     if(.not.present(nframe) ) then
       call nemsio_readrecbin4d4(gfile,jrec,data,ios)
     else
      allocate(datatmp(myfieldsize) )
      call nemsio_readrecbin4d4(gfile,jrec,datatmp,ios)
     endif
   else if ( mygdatatype .eq. 'bin8') then
     allocate(datatmp8(myfieldsize) )
     call nemsio_readrecbin8d8(gfile,jrec,datatmp8,ios)
   else
     allocate(datatmp8(myfieldsize) )
     call nemsio_readrecgrb8(gfile,jrec,datatmp8,ios)
   endif
   if ( ios .ne.0 ) then
     if(present(iret)) then
       iret=ios
       return
     else
       call nemsio_stop
     endif
   endif
!---
   if ( present(nframe) ) then
     if(mygdatatype .eq. 'bin4') then
       do j=1,mydimy+2*mynframe-2*nframe
        do i=1,mydimx+2*mynframe -2*nframe
         data(i+(j-1)*(mydimx+2*mynframe-2*nframe))=datatmp(i+nframe        &
           +(j-1+nframe)*(mydimx+2*mynframe))
        enddo
       enddo
       deallocate(datatmp)
     elseif(mygdatatype=='bin8'.or.mygdatatype=='grib') then
       do j=1,mydimy+2*mynframe-2*nframe
        do i=1,mydimx+2*mynframe -2*nframe
         data(i+(j-1)*(mydimx+2*mynframe-2*nframe))=datatmp8(i+nframe        &
           +(j-1+nframe)*(mydimx+2*mynframe))
        enddo
       enddo
       deallocate(datatmp8)
     endif
   else
     if(mygdatatype=='bin8'.or.mygdatatype=='grib') then
       data=datatmp8
       deallocate(datatmp8)
     endif
   endif
!---
   if(present(iret)) iret=0
   return
  end subroutine nemsio_readrec4
!------------------------------------------------------------------------------
  subroutine nemsio_readrec8(gfile,jrec,data,nframe,iret)
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
! abstract: read nemsio data (bin) by record number into a 2D 32 bits array
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
    implicit none
    type(nemsio_gfile),intent(in)                 :: gfile
    integer(nemsio_intkind),intent(in)            :: jrec
    real(nemsio_dblekind),intent(inout)           :: data(:)
    integer(nemsio_intkind),optional,intent(out)  :: iret
    integer(nemsio_intkind),optional,intent(in)   :: nframe
    real(nemsio_realkind),allocatable             :: datatmp4(:)
    real(nemsio_dblekind),allocatable             :: datatmp(:)
    integer :: i,j,ios
!------------------------------------------------------------
! read 4 byte rec
!------------------------------------------------------------
   if(present(iret)) iret=-32
!---
   call nemsio_getgfile(gfile,iret)
!
   if ( mygdatatype .eq. 'bin4') then
     allocate(datatmp4(myfieldsize))
     call nemsio_readrecbin4d4(gfile,jrec,datatmp4,ios)
   else if ( mygdatatype .eq. 'bin8') then
     if(.not.present(nframe)) then
       call nemsio_readrecbin8d8(gfile,jrec,data,ios)
     else
       allocate(datatmp(myfieldsize))
       call nemsio_readrecbin8d8(gfile,jrec,datatmp,ios)
     endif
   else
     if(.not.present(nframe)) then
       call nemsio_readrecgrb8(gfile,jrec,data,ios)
     else
       allocate(datatmp(myfieldsize))
       call nemsio_readrecgrb8(gfile,jrec,datatmp,ios)
     endif
   endif
   if ( ios .ne.0 ) then
     if(present(iret)) then
       iret=ios
       return
     else
       call nemsio_stop
     endif
   endif
!---
   if ( present(nframe) ) then
     if(mygdatatype=='bin4') then
        do j=1,mydimy+2*mynframe-2*nframe
         do i=1,mydimx+2*mynframe -2*nframe
          data(i+(j-1)*(mydimx+2*mynframe-2*nframe))=datatmp4(i+nframe        &
            +(j-1+nframe)*(mydimx+2*mynframe))
         enddo
        enddo
        deallocate(datatmp4)
     elseif(mygdatatype=='bin8'.or.mygdatatype=='grib') then
        do j=1,mydimy+2*mynframe-2*nframe
         do i=1,mydimx+2*mynframe -2*nframe
          data(i+(j-1)*(mydimx+2*mynframe-2*nframe))=datatmp(i+nframe        &
            +(j-1+nframe)*(mydimx+2*mynframe))
         enddo
        enddo
        deallocate(datatmp)
     endif
   else
     if(mygdatatype=='bin4') then
       data=datatmp4
       deallocate(datatmp4)
     endif
   endif
!
   if(present(iret)) iret=0
   return
  end subroutine nemsio_readrec8
!------------------------------------------------------------------------------
  subroutine nemsio_readrecv4(gfile,name,levtyp,lev,data,nframe,iret)
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
! abstract: read nemsio data by record number into a 2D 32 bits array
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
    implicit none
    type(nemsio_gfile),intent(in)                 :: gfile
    character(*),intent(in)                       :: name
    character(*),intent(in),optional              :: levtyp
    integer(nemsio_intkind),optional,intent(in)   :: lev
    real(nemsio_realkind),intent(inout)           :: data(:)
    integer(nemsio_intkind),optional,intent(out)  :: iret
    integer(nemsio_intkind),optional,intent(in)   :: nframe
    real(nemsio_realkind),allocatable             :: datatmp(:)
    real(nemsio_dblekind),allocatable             :: datatmp8(:)
    integer :: i,j,ios
!------------------------------------------------------------
! read 4 byte rec
!------------------------------------------------------------
   if(present(iret)) iret=-33
!---
   call nemsio_getgfile(gfile,iret)
!---
   if ( mygdatatype .eq. 'bin4') then
     if(.not.present(nframe) ) then
       call nemsio_readrecvbin4d4(gfile,name,levtyp,lev,data,ios)
     else
       allocate(datatmp(myfieldsize) )
       call nemsio_readrecvbin4d4(gfile,name,levtyp,lev,datatmp,ios)
     endif
   else if ( mygdatatype .eq. 'bin8') then
     allocate(datatmp8(myfieldsize) )
     call nemsio_readrecvbin8d8(gfile,name,levtyp,lev,datatmp8,ios)
   else
     allocate(datatmp8(myfieldsize) )
     call nemsio_readrecvgrb8(gfile,name,levtyp,lev,datatmp8,ios)
   endif
   if ( ios .ne.0 ) then
     if(present(iret)) then
       iret=ios
       return
     else
       call nemsio_stop
     endif
   endif
!---
   if ( present(nframe) ) then
     if(mygdatatype=='bin4') then
        do j=1,mydimy+2*mynframe-2*nframe
         do i=1,mydimx+2*mynframe -2*nframe
          data(i+(j-1)*(mydimx+2*mynframe-2*nframe))=datatmp(i+nframe        &
            +(j-1+nframe)*(mydimx+2*mynframe))
         enddo
        enddo
        deallocate(datatmp)
     elseif(mygdatatype=='bin8'.or.mygdatatype=='grib' ) then
        do j=1,mydimy+2*mynframe-2*nframe
         do i=1,mydimx+2*mynframe -2*nframe
          data(i+(j-1)*(mydimx+2*mynframe-2*nframe))=datatmp8(i+nframe        &
            +(j-1+nframe)*(mydimx+2*mynframe))
         enddo
        enddo
        deallocate(datatmp8)
     endif
   else
     if(mygdatatype=='bin8'.or.mygdatatype=='grib' ) then
       data=datatmp8
       deallocate(datatmp8)
     endif
   endif
!---
   if(present(iret)) iret=0
   return
  end subroutine nemsio_readrecv4
!------------------------------------------------------------------------------
  subroutine nemsio_readrecv8(gfile,name,levtyp,lev,data,nframe,iret)
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
! abstract: read nemsio data by record number into a 2D 32 bits array
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
    implicit none
    type(nemsio_gfile),intent(in)                 :: gfile
    character(*),intent(in)                       :: name
    character(*),intent(in),optional              :: levtyp
    integer(nemsio_intkind),optional,intent(in)   :: lev
    real(nemsio_dblekind),intent(inout)           :: data(:)
    integer(nemsio_intkind),optional,intent(out)  :: iret
    integer(nemsio_intkind),optional,intent(in)   :: nframe
    real(nemsio_realkind),allocatable             :: datatmp4(:)
    real(nemsio_dblekind),allocatable             :: datatmp(:)
    integer :: i,j,ios
!------------------------------------------------------------
! read 8 byte rec
!------------------------------------------------------------
   if(present(iret)) iret=-33
!---
   call nemsio_getgfile(gfile,iret)
!---
   if ( mygdatatype .eq. 'bin4') then
     allocate(datatmp4(myfieldsize) )
     call nemsio_readrecvbin4d4(gfile,name,levtyp,lev,datatmp4,ios)
   else if ( mygdatatype .eq. 'bin8') then
     if(.not.present(nframe) ) then
       call nemsio_readrecvbin8d8(gfile,name,levtyp,lev,data,ios)
     else
       allocate(datatmp(myfieldsize) )
       call nemsio_readrecvbin8d8(gfile,name,levtyp,lev,datatmp,ios)
     endif
   else
     if(.not.present(nframe) ) then
       call nemsio_readrecvgrb8(gfile,name,levtyp,lev,data,ios)
     else
       allocate(datatmp(myfieldsize) )
       call nemsio_readrecvgrb8(gfile,name,levtyp,lev,datatmp,ios)
     endif
   endif
   if ( ios .ne.0 ) then
     if(present(iret)) then
       iret=ios
       return
     else
       call nemsio_stop
     endif
   endif
!---
   if ( present(nframe) ) then
      if(mygdatatype=='bin4') then
        do j=1,mydimy+2*mynframe-2*nframe
         do i=1,mydimx+2*mynframe -2*nframe
          data(i+(j-1)*(mydimx+2*mynframe-2*nframe))=datatmp4(i+nframe        &
           +(j-1+nframe)*(mydimx+2*mynframe))
         enddo
        enddo
        deallocate(datatmp4)
      elseif(mygdatatype=='bin8'.or.mygdatatype=='grib') then
        do j=1,mydimy+2*mynframe-2*nframe
         do i=1,mydimx+2*mynframe -2*nframe
          data(i+(j-1)*(mydimx+2*mynframe-2*nframe))=datatmp(i+nframe        &
           +(j-1+nframe)*(mydimx+2*mynframe))
         enddo
        enddo
        deallocate(datatmp)
      endif
   else
     if(mygdatatype=='bin4') then
       data=datatmp4
       deallocate(datatmp4)
     endif
   endif
!
   if(present(iret)) iret=0
   return
  end subroutine nemsio_readrecv8
!------------------------------------------------------------------------------
  subroutine nemsio_readrec4w34(gfile,jrec,data,nframe,iret)
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
! abstract: read nemsio data by record number into a 2D 32 bits array,
!           using w3_4 library to compile
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
    implicit none
    type(nemsio_gfile),intent(in)                 :: gfile
    integer(nemsio_intkind),intent(in)            :: jrec
    real(nemsio_realkind),intent(inout)           :: data(:)
    integer(nemsio_intkind),optional,intent(out)  :: iret
    integer(nemsio_intkind),optional,intent(in)   :: nframe
    real(nemsio_realkind),allocatable             :: datatmp(:)
    real(nemsio_dblekind),allocatable             :: datatmp8(:)
    integer :: i,j,ios
!------------------------------------------------------------
! read 4 byte rec
!------------------------------------------------------------
   if(present(iret)) iret=-34
!---
   call nemsio_getgfile(gfile,iret)
!---
   if ( mygdatatype .eq. 'bin4') then
     if(.not.present(nframe)) then
       call nemsio_readrecbin4d4(gfile,jrec,data,ios)
     else
       allocate(datatmp(myfieldsize) )
       call nemsio_readrecbin4d4(gfile,jrec,datatmp,ios)
     endif
   else if ( mygdatatype .eq. 'bin8') then
      allocate(datatmp8(myfieldsize) )
      call nemsio_readrecbin8d8(gfile,jrec,datatmp8,ios)
   else
     if(.not.present(nframe)) then
       call nemsio_readrecgrb4w34(gfile,jrec,data,ios)
     else
       allocate(datatmp(myfieldsize) )
       call nemsio_readrecgrb4w34(gfile,jrec,datatmp,ios)
     endif
   endif
   if ( ios .ne.0 ) then
     if(present(iret)) then
       iret=ios
       return
     else
       call nemsio_stop
     endif
   endif
!---
   if ( present(nframe) ) then
      if(mygdatatype=='bin4'.or.mygdatatype=='grib') then
        do j=1,mydimy+2*mynframe-2*nframe
         do i=1,mydimx+2*mynframe -2*nframe
          data(i+(j-1)*(mydimx+2*mynframe-2*nframe))=datatmp(i+nframe        &
            +(j-1+nframe)*(mydimx+2*mynframe))
         enddo
        enddo
        deallocate(datatmp)
      elseif(mygdatatype=='bin8') then
        do j=1,mydimy+2*mynframe-2*nframe
         do i=1,mydimx+2*mynframe -2*nframe
          data(i+(j-1)*(mydimx+2*mynframe-2*nframe))=datatmp8(i+nframe        &
            +(j-1+nframe)*(mydimx+2*mynframe))
         enddo
        enddo
        deallocate(datatmp8)
      endif
   else
     if(mygdatatype=='bin8') then
       data=datatmp8
       deallocate(datatmp8)
     endif
   endif
!---
   if(present(iret)) iret=0
   return
  end subroutine nemsio_readrec4w34
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
  subroutine nemsio_readrec8w34(gfile,jrec,data,nframe,iret)
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
! abstract: read nemsio data by record number into a 2D 32 bits array,
!           using w3_4 library to compile
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
    implicit none
    type(nemsio_gfile),intent(in)                 :: gfile
    integer(nemsio_intkind),intent(in)            :: jrec
    real(nemsio_dblekind),intent(inout)           :: data(:)
    integer(nemsio_intkind),optional,intent(out)  :: iret
    integer(nemsio_intkind),optional,intent(in)   :: nframe
    real(nemsio_realkind),allocatable             :: datatmp4(:)
    real(nemsio_dblekind),allocatable             :: datatmp(:)
    integer :: i,j,ios
!------------------------------------------------------------
! read 4 byte rec
!------------------------------------------------------------
   if(present(iret)) iret=-34
!---
   call nemsio_getgfile(gfile,iret)
!---
   if ( mygdatatype .eq. 'bin4') then
     allocate(datatmp4(myfieldsize) )
     call nemsio_readrecbin4d4(gfile,jrec,datatmp4,ios)
   else if ( mygdatatype .eq. 'bin8') then
     if(.not.present(nframe) ) then
      call nemsio_readrecbin8d8(gfile,jrec,data,ios)
     else
      allocate(datatmp(myfieldsize) )
      call nemsio_readrecbin8d8(gfile,jrec,datatmp,ios)
     endif
   else
     allocate(datatmp4(myfieldsize) )
     call nemsio_readrecgrb4w34(gfile,jrec,datatmp4,ios)
   endif
   if ( ios .ne.0 ) then
     if(present(iret)) then
       iret=ios
       return
     else
       call nemsio_stop
     endif
   endif
!---
   if ( present(nframe) ) then
     if(mygdatatype .eq. 'bin4'.or.mygdatatype .eq. 'grib' ) then
       do j=1,mydimy+2*mynframe-2*nframe
       do i=1,mydimx+2*mynframe -2*nframe
        data(i+(j-1)*(mydimx+2*mynframe-2*nframe))=datatmp4(i+nframe        &
          +(j-1+nframe)*(mydimx+2*mynframe))
       enddo
       enddo
       deallocate(datatmp4)
     else if(mygdatatype .eq. 'bin8') then
       do j=1,mydimy+2*mynframe-2*nframe
       do i=1,mydimx+2*mynframe -2*nframe
        data(i+(j-1)*(mydimx+2*mynframe-2*nframe))=datatmp(i+nframe       &
          +(j-1+nframe)*(mydimx+2*mynframe))
       enddo
       enddo
       deallocate(datatmp)
     endif
   else
     if(mygdatatype .eq. 'bin4'.or.mygdatatype .eq. 'grib' ) then
       data=datatmp4
       deallocate(datatmp4)
     endif
   endif
!---
   if(present(iret)) iret=0
   return
  end subroutine nemsio_readrec8w34
!------------------------------------------------------------------------------
  subroutine nemsio_readrecv4w34(gfile,name,levtyp,lev,data,nframe,iret)
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
! abstract: read nemsio data by record number into a 2D 32 bits array
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
    implicit none
    type(nemsio_gfile),intent(in)                 :: gfile
    character(*),intent(in)                       :: name
    character(*),intent(in),optional              :: levtyp
    integer(nemsio_intkind),optional,intent(in)   :: lev
    real(nemsio_realkind),intent(inout)           :: data(:)
    integer(nemsio_intkind),optional,intent(out)  :: iret
    integer(nemsio_intkind),optional,intent(in)   :: nframe
    real(nemsio_realkind),allocatable             :: datatmp(:)
    real(nemsio_dblekind),allocatable             :: datatmp8(:)
    integer :: i,j,ios
!------------------------------------------------------------
! read 4 byte rec
!------------------------------------------------------------
   if(present(iret)) iret=-35
!---
   call nemsio_getgfile(gfile,iret)
!---
   if ( mygdatatype .eq. 'bin4') then
     if(.not.present(nframe)) then
       call nemsio_readrecvbin4d4(gfile,name,levtyp,lev,data,ios)
     else
       allocate(datatmp(myfieldsize) )
       call nemsio_readrecvbin4d4(gfile,name,levtyp,lev,datatmp,ios)
     endif
   else if ( mygdatatype .eq. 'bin8') then
     allocate(datatmp8(myfieldsize) )
     call nemsio_readrecvbin8d8(gfile,name,levtyp,lev,datatmp8,ios)
   else
     if(.not.present(nframe)) then
       call nemsio_readrecvgrb4w34(gfile,name,levtyp,lev,data,ios)
     else
       allocate(datatmp(myfieldsize) )
       call nemsio_readrecvgrb4w34(gfile,name,levtyp,lev,datatmp,ios)
     endif
   endif
   if ( ios .ne.0 ) then
     if(present(iret)) then
       iret=ios
       return
     else
       call nemsio_stop
     endif
   endif
!---
   if ( present(nframe) ) then
      if(mygdatatype=='bin4'.or.mygdatatype=='grib') then
        do j=1,mydimy+2*mynframe-2*nframe
         do i=1,mydimx+2*mynframe -2*nframe
           data(i+(j-1)*(mydimx+2*mynframe-2*nframe))=datatmp(i+nframe        &
            +(j-1+nframe)*(mydimx+2*mynframe))
         enddo
        enddo
        deallocate(datatmp)
      elseif(mygdatatype=='grib8') then
        do j=1,mydimy+2*mynframe-2*nframe
         do i=1,mydimx+2*mynframe -2*nframe
           data(i+(j-1)*(mydimx+2*mynframe-2*nframe))=datatmp8(i+nframe        &
            +(j-1+nframe)*(mydimx+2*mynframe))
         enddo
        enddo
        deallocate(datatmp8)
      endif
   else
      if(mygdatatype=='grib8') then
        data=datatmp8
        deallocate(datatmp8)
      endif
   endif
!---
   if(present(iret)) iret=0
   return
  end subroutine nemsio_readrecv4w34
!
!------------------------------------------------------------------------------
  subroutine nemsio_readrecv8w34(gfile,name,levtyp,lev,data,nframe,iret)
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
! abstract: read nemsio data by record number into a 2D 32 bits array
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
    implicit none
    type(nemsio_gfile),intent(in)                 :: gfile
    character(*),intent(in)                       :: name
    character(*),intent(in),optional              :: levtyp
    integer(nemsio_intkind),optional,intent(in)   :: lev
    real(nemsio_dblekind),intent(inout)           :: data(:)
    integer(nemsio_intkind),optional,intent(out)  :: iret
    integer(nemsio_intkind),optional,intent(in)   :: nframe
    real(nemsio_dblekind),allocatable             :: datatmp(:)
    real(nemsio_realkind),allocatable             :: datatmp4(:)
    integer :: i,j,ios
!------------------------------------------------------------
! read 8 byte rec
!------------------------------------------------------------
   if(present(iret)) iret=-35
!---
   call nemsio_getgfile(gfile,iret)
!---
   if ( mygdatatype .eq. 'bin4') then
     allocate(datatmp4(myfieldsize) )
     call nemsio_readrecvbin4d4(gfile,name,levtyp,lev,datatmp4,ios)
   else if ( mygdatatype .eq. 'bin8') then
     if(.not.present(nframe)) then
       call nemsio_readrecvbin8d8(gfile,name,levtyp,lev,data,ios)
     else
       allocate(datatmp(myfieldsize) )
       call nemsio_readrecvbin8d8(gfile,name,levtyp,lev,datatmp,ios)
     endif
   else
     allocate(datatmp4(myfieldsize) )
     call nemsio_readrecvgrb4w34(gfile,name,levtyp,lev,datatmp4,ios)
   endif
   if ( ios .ne.0 ) then
     if(present(iret)) then
       iret=ios
       return
     else
       call nemsio_stop
     endif
   endif
!---
   if ( present(nframe) ) then
      if(mygdatatype .eq. 'bin4'.or.mygdatatype .eq. 'grib') then
       do j=1,mydimy+2*mynframe-2*nframe
       do i=1,mydimx+2*mynframe -2*nframe
        data(i+(j-1)*(mydimx+2*mynframe-2*nframe))=datatmp4(i+nframe        &
          +(j-1+nframe)*(mydimx+2*mynframe))
       enddo
       enddo
       deallocate(datatmp4)
      elseif(mygdatatype .eq. 'bin8') then
       do j=1,mydimy+2*mynframe-2*nframe
       do i=1,mydimx+2*mynframe -2*nframe
        data(i+(j-1)*(mydimx+2*mynframe-2*nframe))=datatmp(i+nframe        &
          +(j-1+nframe)*(mydimx+2*mynframe))
       enddo
       enddo
       deallocate(datatmp)
     endif
   else
     if(mygdatatype .eq. 'bin4'.or.mygdatatype .eq. 'grib') then
       data=datatmp4
       deallocate(datatmp4)
     endif
   endif
!
   if(present(iret)) iret=0
   return
  end subroutine nemsio_readrecv8w34
!------------------------------------------------------------------------------
!
!*****************   read bin data set :  ********************************
!
!------------------------------------------------------------------------------
  subroutine nemsio_readrecbin4d4(gfile,jrec,data,iret)
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
! abstract: read nemsio data (bin) by record number into a 2D 32 bits array
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
    implicit none
    type(nemsio_gfile),intent(in)                 :: gfile
    integer(nemsio_intkind),intent(in)            :: jrec
    real(nemsio_realkind),intent(inout)           :: data(:)
    integer(nemsio_intkind),optional,intent(out)  :: iret
    integer(nemsio_intkind8) :: iskip,iread,nread

    if(present(iret)) iret=-41
    iskip=mytlmeta+int(jrec-1,8)*int(kind(data)*myfieldsize+8,8)
    iread=int(nemsio_realkind,8)*int(size(data),8)
    call bafrreadl(myflunit,iskip,iread,nread,data)
    if(nread.lt.iread) return
    if(present(iret)) iret=0

    return
  end subroutine nemsio_readrecbin4d4
!------------------------------------------------------------------------------
  subroutine nemsio_readrecvbin4d4(gfile,name,levtyp,lev,data,iret)
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
! abstract: read nemsio data (bin) by record number into a 2D 32 bits array
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
    implicit none
    type(nemsio_gfile),intent(in)                 :: gfile
    character(*),intent(in)                      :: name
    character(*),intent(in),optional             :: levtyp
    integer(nemsio_intkind),optional,intent(in)  :: lev
    real(nemsio_realkind),intent(out)             :: data(:)
    integer(nemsio_intkind),optional,intent(out)  :: iret
    integer(nemsio_intkind8) :: iskip,iread,nread
    integer :: jrec, ierr

    if(present(iret)) iret=-42
    call nemsio_searchrecv(gfile,jrec,name,levtyp,lev,ierr)
    if ( ierr .ne. 0)  return
    iskip=mytlmeta+int(jrec-1,8)*int(nemsio_realkind*myfieldsize+8,8)
    iread=int(kind(data),8)*int(size(data),8)
    call bafrreadl(myflunit,iskip,iread,nread,data)
    if(nread.lt.iread) return
    if(present(iret)) iret=0

    return
  end subroutine nemsio_readrecvbin4d4
!------------------------------------------------------------------------------
  subroutine nemsio_readrecbin8d8(gfile,jrec,data,iret)
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
! abstract: read nemsio data (bin) by record number into a 2D 32 bits array
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
    implicit none
    type(nemsio_gfile),intent(in)                 :: gfile
    integer(nemsio_intkind),intent(in)            :: jrec
    real(nemsio_dblekind),intent(out)             :: data(:)
    integer(nemsio_intkind),optional,intent(out)  :: iret
    integer(nemsio_intkind8) :: iskip,iread,nread

    if(present(iret)) iret=-42
    iskip=mytlmeta+int(jrec-1,8)*int(nemsio_dblekind*myfieldsize+8,8)
    iread=int(nemsio_dblekind,8)*int(size(data),8)
    call bafrreadl(myflunit,iskip,iread,nread,data)
    if(nread.lt.iread) return
    if(present(iret)) iret=0

    return
  end subroutine nemsio_readrecbin8d8
!------------------------------------------------------------------------------
  subroutine nemsio_readrecvbin8d8(gfile,name,levtyp,lev,data,iret)
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
! abstract: read nemsio data (bin) by record number into a 2D 32 bits array
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
    implicit none
    type(nemsio_gfile),intent(in)                 :: gfile
    character(*),intent(in)                      :: name
    character(*),intent(in),optional             :: levtyp
    integer(nemsio_intkind),optional,intent(in)  :: lev
    real(nemsio_dblekind),intent(out)             :: data(:)
    integer(nemsio_intkind),optional,intent(out)  :: iret
    integer(nemsio_intkind8) :: iskip,iread,nread
    integer :: jrec, ierr

    if(present(iret)) iret=-44
    call nemsio_searchrecv(gfile,jrec,name,levtyp,lev,ierr)
    if ( ierr .ne. 0) return
    iskip=mytlmeta+int(jrec-1,8)*int(nemsio_dblekind*myfieldsize+8,8)
    iread=int(nemsio_dblekind,8)*int(size(data),8)
    call bafrreadl(myflunit,iskip,iread,nread,data)
    if(nread.lt.iread) return
    if(present(iret)) iret=0

    return
  end subroutine nemsio_readrecvbin8d8
!------------------------------------------------------------------------------
!
!*****************   read w34 data set :  *************************************
!
!------------------------------------------------------------------------------
  subroutine  nemsio_readrecgrb4w34(gfile,jrec,data,iret)
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
! abstract: read nemsio data by record number into a 2D 32 bits array,
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
    implicit none
    type(nemsio_gfile),intent(in)                 :: gfile
    integer(nemsio_intkind),intent(in)            :: jrec
    real(nemsio_realkind),intent(out)             :: data(:)
    integer(nemsio_intkind),optional,intent(out)  :: iret
    type(nemsio_grbmeta)         :: grbmeta
    integer(nemsio_intkind)      :: luidx
    integer(nemsio_intkind)      :: kf,k,kpds(200),kgds(200)
    logical*1,allocatable       :: lbms(:)
    integer(nemsio_intkind)      :: N=nemsio_kpds_intfill
    integer(nemsio_intkind)      :: ios,i,w34
!
!------------------------------------------------------------
! set up grib meta
!------------------------------------------------------------
    luidx=0
    if ( present(iret)) iret=-45
    w34=1
    call nemsio_setrqst(gfile,grbmeta,ios,jrec=jrec,w34=w34)
    if (ios.ne.0) then
       if ( present(iret))  then
         iret=ios
         return
       else
         call nemsio_stop
       endif
    endif
    allocate(lbms(grbmeta%jf))
    N=0
!------------------------------------------------------------
! get data from getgb
!------------------------------------------------------------
    call getgbm(myflunit,luidx,grbmeta%jf,N,grbmeta%jpds,grbmeta%jgds,&
      mymbuf,mycbuf,mynlen,mynnum,mymnum, &
      kf,k,kpds,kgds,lbms,data,ios)
    deallocate(lbms,grbmeta%lbms)
    if(ios.ne.0) then
       if ( present(iret))  then
         print *,'getgb_ios=',ios
         return
       else
         call nemsio_stop
       endif
    endif
    if (present(iret)) iret=0
  end subroutine nemsio_readrecgrb4w34
!------------------------------------------------------------------------------
  subroutine nemsio_readrecvgrb4w34(gfile,vname,vlevtyp,vlev,data,iret)
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
! abstract: read nemsio data by field name into 32 bits array,
!           using w3_4 library to compile
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
    implicit none
    type(nemsio_gfile),intent(in)                 :: gfile
    character*(*),intent(in)                      :: vname,vlevtyp
    integer(nemsio_intkind),intent(in)            :: vlev
    real(nemsio_realkind),intent(out)             :: data(:)
    integer(nemsio_intkind),optional,intent(out)  :: iret
    type(nemsio_grbmeta)         :: grbmeta
    integer(nemsio_intkind)      :: luidx
    integer(nemsio_intkind)      :: kf,k,kpds(200),kgds(200)
    logical*1,allocatable       :: lbms(:)
    integer(nemsio_intkind)      :: N=nemsio_kpds_intfill
    integer(nemsio_intkind)      :: ios,i,w34
!
!------------------------------------------------------------
! set up grib meta
!------------------------------------------------------------
    luidx=0
    if ( present(iret)) iret=-45
    w34=1
    call nemsio_setrqst(gfile,grbmeta,ios,vname=vname, &
      vlevtyp=vlevtyp, vlev=vlev ,w34=w34)
    if (ios.ne.0) then
       if ( present(iret))  then
         iret=ios
         return
       else
         call nemsio_stop
       endif
    endif
!------------------------------------------------------------
! get data from getgb _w34
!------------------------------------------------------------
    allocate(lbms(grbmeta%jf))
    N=0
    call getgbm(myflunit,luidx,grbmeta%jf,N,grbmeta%jpds,grbmeta%jgds,&
      mymbuf,mycbuf,mynlen,mynnum,mymnum, &
      kf,k,kpds,kgds,lbms,data,ios)
    deallocate(lbms,grbmeta%lbms)
    if(ios.ne.0) then
       if ( present(iret))  then
          print *,'getgb_ios=',ios
         return
       else
         call nemsio_stop
       endif
    endif
    if ( present(iret)) iret=0
  end subroutine nemsio_readrecvgrb4w34
!------------------------------------------------------------------------------
!
!*****************   read grb data set w3d:  *************************************
!
!------------------------------------------------------------------------------
  subroutine nemsio_readrecgrb8(gfile,jrec,data,iret)
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
! abstract: read nemsio data by record number into a 2D 64 bits array, 
!           using w3_d library to compile
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
    implicit none
    type(nemsio_gfile),intent(in)                 :: gfile
    integer(nemsio_intkind),intent(in)            :: jrec
    real(nemsio_dblekind),intent(out)             :: data(:)
    integer(nemsio_intkind),optional,intent(out)  :: iret
    type(nemsio_grbmeta)         :: grbmeta
    integer(nemsio_intkind)      :: luidx
    integer(nemsio_intkind)      :: kf,k,kpds(200),kgds(200)
    logical*1,allocatable       :: lbms(:)
    integer(nemsio_intkind)      :: N=nemsio_kpds_intfill
    integer(nemsio_intkind)      :: ios,i
!
!------------------------------------------------------------
! set up grib meta 
!------------------------------------------------------------
    luidx=0
    if ( present(iret)) iret=-46
    call nemsio_setrqst(gfile,grbmeta,ios,jrec=jrec)
    if (ios.ne.0) then
       if ( present(iret))  then
         iret=ios
         return
       else
         call nemsio_stop
       endif
    endif
!------------------------------------------------------------
! get data from getgb _w3d
!------------------------------------------------------------
    allocate(lbms(grbmeta%jf))
    N=0
    call getgbm(myflunit,luidx,grbmeta%jf,N,grbmeta%jpds,grbmeta%jgds,&
      mymbuf,mycbuf,mynlen,mynnum,mymnum, &
      kf,k,kpds,kgds,lbms,data,ios)
    deallocate(lbms,grbmeta%lbms)
    if(ios.ne.0) then
       if ( present(iret))  then
         print *,'getgb_ios=',ios
         return
       else
         call nemsio_stop
       endif
    endif
    if (present(iret)) iret=0
  end subroutine nemsio_readrecgrb8
!------------------------------------------------------------------------------
  subroutine nemsio_readrecvgrb8(gfile,vname,vlevtyp,vlev,data,iret)
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
! abstract: read nemsio data by field name into a 2D 64bits array, 
!           using w3_d library to compile
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
    implicit none
    type(nemsio_gfile),intent(in)                 :: gfile
    character*(*),intent(in)                     :: vname,vlevtyp
    integer(nemsio_intkind),intent(in)            :: vlev
    real(nemsio_dblekind),intent(out)             :: data(:)
    integer(nemsio_intkind),optional,intent(out)  :: iret
    type(nemsio_grbmeta)         :: grbmeta
    integer(nemsio_intkind)      :: luidx
    integer(nemsio_intkind)      :: kf,k,kpds(200),kgds(200)
    logical*1,allocatable       :: lbms(:)
    integer(nemsio_intkind)      :: N=nemsio_kpds_intfill
    integer(nemsio_intkind)      :: ios,i
!
!------------------------------------------------------------
! set up grib meta 
!------------------------------------------------------------
    luidx=0
    if ( present(iret)) iret=-47
    call nemsio_setrqst(gfile,grbmeta,ios,vname=vname, &
      vlevtyp=vlevtyp, vlev=vlev )
    if (ios.ne.0) then
       if ( present(iret))  then
         iret=ios
         return
       else
         call nemsio_stop
       endif
    endif
!------------------------------------------------------------
! get data from getgb _w3d
!------------------------------------------------------------
    allocate(lbms(grbmeta%jf))
    N=0
    call getgbm(myflunit,luidx,grbmeta%jf,N,grbmeta%jpds,grbmeta%jgds,&
      mymbuf,mycbuf,mynlen,mynnum,mymnum, &
      kf,k,kpds,kgds,lbms,data,ios)
    deallocate(lbms,grbmeta%lbms)
    if(ios.ne.0) then
       if ( present(iret))  then
          print *,'getgb_ios=',ios
         return
       else
         call nemsio_stop
       endif
    endif
    if ( present(iret)) iret=0
  end subroutine nemsio_readrecvgrb8
!------------------------------------------------------------------------------
end module nemsio_read
