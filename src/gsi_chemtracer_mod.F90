module gsi_chemtracer_mod
!-------------------------------------------------------------------------
!  NASA/GSFC, Global Modeling and Assimilation Office, Code 610.1, GMAO  !
!-------------------------------------------------------------------------
!BOP

! !MODULE: ChemTracerMod -- Implements ESMF wrapper to invoke GSI
!
! !DESCRIPTION: Very preliminary module to handle chemistry tracers.
!      This still uses wired-in type arrays to hold the guess. Soon
!      we'll generalize this.
!
! !REMARKS:
!   1. VERY IMPORTANT: No object from this file is to be make
!                      explicitly available to the outside world.
!                      Each object must be opaque with a get and
!                      a put method associated with it.
!   2. This is still functioning as a common-block when it comes
!      to the chem type itself - needs some work to make it into 
!      a self-contained type
! 
! !USES:

use kinds, only: i_kind,r_kind
use mpimod, only : mype
use mpeu_util,only: die
use file_utility, only : get_lun
use gsi_bundlemod, only : GSI_BundleCreate
use gsi_bundlemod, only : GSI_BundleGetPointer
use gsi_bundlemod, only : GSI_Bundle
use gsi_bundlemod, only : GSI_BundlePrint
use gsi_bundlemod, only : GSI_BundleDestroy

use gsi_bundlemod, only : GSI_Grid
use gsi_bundlemod, only : GSI_GridCreate

use mpeu_util, only: gettablesize
use mpeu_util, only: gettable
use mpeu_util, only: getindex

implicit none
private
save

! !PUBLIC ROUTINES:

public :: gsi_chemtracer_create_grids
public :: gsi_chemtracer_destroy_grids
public :: gsi_chemtracer_init
public :: gsi_chemtracer_get
public :: gsi_chemtracer_final
public :: nbundles

public :: GSI_chem_bundle   ! still a common for now, ultimately should 
                            ! be a dynamic "type", passed around in arg list

! !INTERFACE:

interface gsi_chemtracer_init
          module procedure init_
end interface
interface gsi_chemtracer_final
          module procedure final_ 
end interface
interface gsi_chemtracer_create_grids
          module procedure create_
end interface
interface gsi_chemtracer_destroy_grids
          module procedure destroy_
end interface
interface gsi_chemtracer_get
          module procedure get_int0d_
          module procedure get_char0d_
          module procedure get_char1d_
end interface

type(GSI_Bundle),pointer :: GSI_chem_bundle(:)   ! still a common for now


! !REVISION HISTORY:
!
!   20Apr2010 Todling  Initial code.
!   03May2010 Treadon - add iostat error check to ibm_sp read(lu,chemtracers) in init_
!   19May2010 Todling - porter Hou's igfsco2 flag from setup namelist to this namelist
!   30May2010 Todling - remove namelist; revamp the way fields/info read in (i90-style)
!   25Jun2010 Treadon - consistently intialize ivar; check/use length of desc (gsi_chemtracter_get)
!
!EOP
!-------------------------------------------------------------------------

! !PRIVATE ROUTINES:

integer(i_kind),parameter::MAXSTR=256
logical:: tracer_grid_initialized=.false.
logical:: chem_initialized_=.false.
character(len=*), parameter :: myname = 'chemtracermod'

integer :: nbundles=-1
integer :: ntgases=0
integer :: naero=0
integer :: n2daero=0
integer :: n3daero=0
integer :: ng3d=-1
integer :: ng2d=-1
character(len=MAXSTR),allocatable :: tgases(:)  ! same as list above, but each var as element of array
character(len=MAXSTR),allocatable :: tgases3d(:)! same as list above, but each var as element of array
character(len=MAXSTR),allocatable :: tgases2d(:)! same as list above, but each var as element of array
character(len=MAXSTR),allocatable :: chemtype(:)! indicate type of chem (used for aerosols for now)
integer(i_kind),allocatable,dimension(:) :: i4crtm ! controls use of gas in CRTM:
                                                   ! < 0 don't use in CRTM
                                                   ! = 0 use predefined global mean co2 mixing ration
                                                   ! = 1 use gfs yearly global annual mean historical co2 value
                                                   ! = 2 use gfs monthly horizontal 2-d historical co2 value
                                                   ! >=10 indicates aerosol

logical:: verbose_=.true.

contains

subroutine init_
implicit none
!character(len=*),parameter:: rcname='anavinfo.txt'
character(len=*),parameter:: rcname='anavinfo'  ! filename should have extension
character(len=*),parameter:: tbname='chem_guess::'
integer(i_kind) luin,i,ii,ntot,icrtmuse
character(len=256),allocatable,dimension(:):: utable
character(len=20) var,ctype
character(len=*),parameter::myname_=myname//'*init_'
integer(i_kind) ilev, itracer

! load file
luin=get_lun()
open(luin,file=rcname,form='formatted')

! Scan file for desired table first
! and get size of table
call gettablesize(tbname,luin,ntot,ntgases)
if(ntgases==0) then
   close(luin)
   return
endif

! Get contents of table
allocate(utable(ntgases))
call gettable(tbname,luin,ntot,ntgases,utable)

! release file unit
close(luin)

! Retrieve each token of interest from table and define
! variables participating in state vector

! Count variables first
ng3d=0; ng2d=0
do ii=1,ntgases
   read(utable(ii),*) var, ilev, itracer, icrtmuse
   if(ilev>1) then
       ng3d=ng3d+1
   else if(ilev==1) then
       ng2d=ng2d+1
   else
       write(6,*) myname_,': error, unknown number of levels'
       call stop2(999)
   endif
enddo

allocate(tgases3d(ng3d),tgases2d(ng2d),i4crtm(ntgases),chemtype(ntgases))

! Now load information from table
ng3d=0;ng2d=0
do ii=1,ntgases
   read(utable(ii),*) var, ilev, itracer, icrtmuse, ctype
   if(ilev>1) then
      ng3d=ng3d+1
      tgases3d(ng3d)=trim(adjustl(var))
      if(icrtmuse>=10) n3daero=n3daero+1 ! convention, for now
   else
      ng2d=ng2d+1
      tgases2d(ng2d)=trim(adjustl(var))
      if(icrtmuse>=10) n2daero=n2daero+1 ! convention, for now
   endif
   i4crtm  (ii)=icrtmuse
   chemtype(ii)=trim(ctype)
   if(icrtmuse>=10) naero=naero+1 ! convention, for now
enddo

deallocate(utable)

allocate(tgases(ntgases))

! Fill in array w/ all var names (must be 3d first, then 2d)
ii=0
do i=1,ng3d
   ii=ii+1
   tgases(ii)=tgases3d(i)
enddo
do i=1,ng2d
   ii=ii+1
   tgases(ii)=tgases2d(i)
enddo

if (mype==0) then
    write(6,*) myname_,':  2D-CHEM STATE VARIABLES: '
    do i=1,ng2d
       write(6,*) trim(tgases2d(i))
    enddo
    write(6,*) myname_,':  3D-CHEM STATE VARIABLES:'
    do i=1,ng3d
       write(6,*) trim(tgases3d(i))
    enddo
    write(6,*) myname_,': ALL CHEM STATE VARIABLES:'
    do i=1,ntgases
       write(6,*) trim(tgases(i))
    enddo
end if
chem_initialized_=.true.

end subroutine init_
subroutine final_
implicit none
if(.not.chem_initialized_) return
deallocate(tgases)
deallocate(tgases3d,tgases2d,i4crtm,chemtype)
chem_initialized_=.false.
end subroutine final_

!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  create_ --- Alloc grid for trace gases
!
! !INTERFACE:
!
! subroutine create_(GSI_Chem_Bundle,im,jm,km,lm,istatus) ! ultimately
  subroutine create_(im,jm,km,lm,istatus)

! !USES:

    use constants,only: zero
    implicit none

! !INPUT PARAMETERS:
    integer(i_kind),intent(in)::im,jm,km,lm

! !OUTPUT PARAMETERS:
    integer(i_kind),intent(out)::istatus

! !INPUT/OUTPUT PARAMETERS:
!   type(GSI_Bundle) :: GSI_chem_bundle

! !DESCRIPTION: allocate grids to hold guess cloud fields
!
! !REVISION HISTORY:
!   2010-04-20  todling  initial code
!   2010-05-17  todling  update create interface to pass a grid
!
! !AUTHOR:
!   Ricardo Todling  org: gmao      date: 2010-04-10
!
!EOP
!-------------------------------------------------------------------------

    character(len=*), parameter :: myname_ = myname//'*create_'
    integer(i_kind) i,j,k,n,nt,ic
    character(len=MAXSTR) :: var
    type(GSI_Grid):: grid

    istatus=0
    if(ntgases<=0) return

    if(tracer_grid_initialized) return

!   Create simple regular grid
    call gsi_gridcreate ( grid, im, jm, km )

    nbundles = lm
    allocate(GSI_chem_bundle(nbundles))
    do nt=1,nbundles
       call GSI_BundleCreate ( GSI_chem_bundle(nt), grid, 'Trace Gases', istatus, &
                               names3d=tgases3d ) ! only 3d for now
    enddo

    if (istatus/=0) then
        if(mype==0) write(6,*)trim(myname_),':  allocate error1, istatus=',&
                              istatus,im,jm,km,lm
        return
    endif

    if (verbose_) then
       if(mype==0) write(6,*) trim(myname_),': alloc() for chem-tracer done'
    endif
    tracer_grid_initialized=.true.

    return
  end subroutine create_

!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  destroy_tgases_grids --- Dealloc grid for trace gases
!
! !INTERFACE:
!
! subroutine destroy_ (GSI_Chem_Bundle, istatus) ! ultimately
  subroutine destroy_ (istatus)

! !USES:
    implicit none

! !INPUT PARAMETERS:

! !OUTPUT PARAMETERS:
  integer(i_kind), intent(out) :: istatus

! !INPPUT/OUTPUT PARAMETERS:
!   type(GSI_Bundle) :: GSI_chem_bundle

! !DESCRIPTION: Dealloc grids holding trace gases
!
! !REVISION HISTORY:
!   2010-04-10  todling  initial code
!
! !REMARKS:
!   language: f90
!   machine:  ijet
!
! !AUTHOR:
!   Ricardo Todling  org: gmao      date: 2010-04-10
!
!EOP
!-------------------------------------------------------------------------

    character(len=*), parameter :: myname_ = myname//'*destroy_'
    integer(i_kind) :: nt
    character(len=MAXSTR) :: var
    istatus=0

    if(.not.tracer_grid_initialized) return

     do nt=1,nbundles
        call GSI_BundleDestroy ( GSI_chem_bundle(nt), istatus )
     enddo
     deallocate(GSI_chem_bundle,stat=istatus)

    if (istatus/=0) then
        if(mype==0) write(6,*)trim(myname_),':  deallocate error1, istatus=',istatus
        return
    endif

    if (verbose_) then
       if(mype==0) write(6,*) trim(myname_),': dealloc() for chem-tracer done'
    endif
    tracer_grid_initialized=.false.

    return
  end subroutine destroy_


! From here down, inquiry function to make all object opaque
! ----------------------------------------------------------
  subroutine get_int0d_ ( desc, ivar, istatus )
  implicit none
  character(len=*),intent(in):: desc
  integer(i_kind),intent(out):: ivar
  integer(i_kind),intent(out):: istatus
  character(len=MAXSTR):: work
  integer(i_kind) id,ln
  istatus=1
  ivar=0
  if(trim(desc)=='dim') then
     ivar = ntgases
     istatus=0
  endif
  if(trim(desc)=='aerosols') then
     ivar = naero
     istatus=0
  endif
  if(trim(desc)=='aerosols::3d') then
     ivar = n3daero
     istatus=0
  endif
  if(trim(desc)=='aerosols::2d') then
     ivar = n2daero
     istatus=0
  endif
  if(index(trim(desc),'i4crtm::')/=0) then
     ln=len_trim(desc)
     work=desc(9:ln)
     if(allocated(tgases)) then
        id=getindex(tgases,trim(work))
        ivar=i4crtm(id)
     else
        ivar=0
     endif
     istatus=0
  endif
  if(desc(1:5)=='var::') then
     if(allocated(tgases)) then
        id=len_trim(desc)
        if(id>=6) ivar=getindex(tgases,desc(6:id))
     endif
  endif
  end subroutine get_int0d_

  subroutine get_char0d_ ( desc, ivar, istatus )
  implicit none
  character(len=*),intent(in):: desc
  character(len=*),intent(out):: ivar
  integer(i_kind),intent(out):: istatus
  character(len=MAXSTR):: gaslist,work
  integer(i_kind) is,ie,i,i0
  istatus=1
  ivar=''
  if(trim(desc)=='list') then
     if(allocated(tgases)) then
        gaslist=trim(tgases(1))
        do i=2,ntgases
           i0=len_trim(gaslist)
           is=i0+1
           ie=is+len_trim(tgases(i))+1
           gaslist(is:ie)=','//tgases(i)
        enddo
        ivar = trim(gaslist)
        istatus=0
     endif
  endif
  end subroutine get_char0d_

  subroutine get_char1d_ ( desc, ivar, istatus )
  implicit none
  character(len=*),intent(in):: desc
  character(len=*),intent(out):: ivar(:)
  integer(i_kind),intent(out):: istatus
  integer(i_kind) i,ii
  istatus=1
  ivar=''
  if(trim(desc)=='shortnames') then
     if(size(ivar)>=size(tgases)) then
        if(allocated(tgases))then
          ivar = tgases
          istatus=0
        endif
     endif
  endif
  if(trim(desc)=='aerosols') then
     if(size(ivar)>=naero) then
        ii=0
        do i=1,ntgases
           if(i4crtm(i)>=10) then
              ii=ii+1
              ivar(ii)=tgases(ii) 
           endif
        enddo
     endif
  endif
  if(trim(desc)=='aerosols::3d') then
     if(size(ivar)>=naero) then
        ii=0
        do i=1,ng3d
           if(i4crtm(i)>=10) then
              ii=ii+1
              ivar(ii)=tgases(ii) 
           endif
        enddo
     endif
  endif
  if(trim(desc)=='aerosols::2d') then
     if(size(ivar)>=naero) then
        ii=0
        do i=ng3d+1,ng2d
           if(i4crtm(i)>=10) then
              ii=ii+1
              ivar(ii)=tgases(ii) 
           endif
        enddo
     endif
  endif
  if(trim(desc)=='aerosol_types::3d') then
     if(size(ivar)>=naero) then
        ii=0
        do i=1,ng3d
           if(i4crtm(i)>=10) then
              ii=ii+1
              ivar(ii)=chemtype(ii) 
           endif
        enddo
     endif
  endif
  end subroutine get_char1d_
end module gsi_chemtracer_mod
