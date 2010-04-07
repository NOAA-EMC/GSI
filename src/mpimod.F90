!-------------------------------------------------------------------------
!    NOAA/NCEP, National Centers for Environmental Prediction GSI        !
!-------------------------------------------------------------------------
!BOP
!
! !MODULE:  mpimod --- GSI Module containing mpi related variables
!
! !INTERFACE:
!

module mpimod

! !USES:

  use kinds, only: i_kind
  use constants, only: izero,ione

#ifdef ibm_sp
! Include standard mpi includes file.
  use mpi
#else
  use mpeu_mpif, only : mpi_rtype4 => mpi_real4
  use mpeu_mpif, only : mpi_rtype => mpi_real8
  use mpeu_mpif, only : mpi_itype => mpi_integer4
  use mpeu_mpif, only : mpi_real8
  use mpeu_mpif, only : mpi_real16
  use mpeu_mpif, only : mpi_status_size
  use mpeu_mpif, only : mpi_sum
  use mpeu_mpif, only : mpi_integer
  use mpeu_mpif, only : mpi_integer1
  use mpeu_mpif, only : mpi_integer2
  use mpeu_mpif, only : mpi_integer4
  use mpeu_mpif, only : mpi_integer8
  use mpeu_mpif, only : mpi_real4
  use mpeu_mpif, only : mpi_max
  use mpeu_mpif, only : mpi_min
  use mpeu_mpif, only : mpi_offset_kind
  use mpeu_mpif, only : mpi_info_null
  use mpeu_mpif, only : mpi_mode_rdonly
  use mpeu_mpif, only : mpi_mode_rdwr
  use mpeu_mpif, only : mpi_byte
#ifndef HAVE_ESMF
  use mpeu_mpif, only : mpi_comm_world
#endif /* HAVE_ESMF */
#endif

  implicit none

!
! !DESCRIPTION: module containing mpi related variables
!
! !REVISION HISTORY:
!
!   2003-09-30  kleist
!   2004-05-18  kleist, new variables and documentation
!   2004-06-10  todling, explicitly declated var from m_mpif
!   2004-07-15  todling, protex-compliant prologue
!   2004-07-23  treadon - add routine strip_periodic
!   2005-01-24  kleist - fix bug in array initialization
!   2005-02-15  todling - add use m_mpif, only for mpi_integer4,
!                         mpi_offset_kind, ... (only applies
!                         to non IBM SP machines)    
!   2005-07-25  todling - add a couple more exports from m_mpif
!                         (only applies to non IBM SP machines)
!   2006-04-06  middlecoff - remove mpi_request_null since not used
!   2006-06-20  treadon - add mpi_itype
!   2006-06-28  da Silva - Added 2 integers represing a layout: nxPE and nyPE.
!   2009-02-19  jing guo - replaced m_mpif of GMAO_mpeu with gmaogsi_mpif.
!   2009-04-21  derber - add communications for strong balance constraint (bal)
!                        and unified uv (vec) transformation
!   2010-04-01  treadon - remove routines reorder, reorder2, strip_single, strip,
!                         vectosub, reload, and strip_periodic from mpimod - these
!                         routines are now found in gridmod
!
! !REMARKS:
!
!   language: f90
!   machine:  ibm RS/6000 SP, SGI Origin 2000; Compaq HP
!
! !AUTHOR: 
!    kleist           org: np20                date: 2003-09-30 
!
!EOP
!-------------------------------------------------------------------------

! set default to private
  private
! set subroutines to public
  public :: init_mpi_vars
  public :: destroy_mpi_vars
  public :: setcomm
! set passed variables to public
  public :: ierror,mpi_comm_world,npe,mpi_rtype,mpi_sum,mype,mpi_max,mpi_itype
  public :: mpi_real4,mpi_integer4,levs_id,mpi_min,mpi_real8,mpi_real16,mpi_integer8
  public :: mpi_integer,mpi_integer1,mpi_integer2,nvar_id,nnnuvlevs,iscuv_g
  public :: nuvlevs,ircuv_g,irduv_g,irduv_s,iscuv_s,ircuv_s,isduv_g,isduv_s
  public :: mpi_status_size,mpi_rtype4,nvar_pe,nype,nxpe,nnnvsbal,nlevsbal
  public :: nvarbal_id,lu_gs,nlevsuv,nnnvsuv,irdvec_g,ircvec_g,lv_gs,ircvec_s
  public :: irdvec_s,iscvec_s,iscvec_g,isdvec_g,isdvec_s,iscnt_s,isdsp_s
  public :: irdsp_s,ircnt_s,kv_gs,ku_gs,kp_gs,kt_gs,ircbal_s,irdbal_s,isdbal_s
  public :: iscbal_s,isdbal_g,irdbal_g,iscbal_g,ircbal_g,isdsp_g,irdsp_g
  public :: iscnt_g,ircnt_g,mpi_mode_rdonly,mpi_info_null,mpi_offset_kind
  public :: mpi_mode_rdwr,mpi_byte

#ifdef HAVE_ESMF
  integer(i_kind) :: mpi_comm_world
#endif

#ifdef ibm_sp
! Define size for mpi_real
  integer(i_kind), parameter :: mpi_rtype=mpi_real8
  integer(i_kind), parameter :: mpi_rtype4=mpi_real4
! integer(i_kind), parameter :: mpi_rtype=mpi_real4
  integer(i_kind), parameter :: mpi_itype=mpi_integer4
#endif

  integer(i_kind) ierror
  integer(i_kind) :: npe         ! total num of MPI tasks
  integer(i_kind) :: mype        ! number of MPI task
  integer(i_kind)    nuvlevs     ! max num levs per task, for dist. of uv/stvp         
  integer(i_kind)    nnnuvlevs   ! num levs current task, for dist. of uv/stvp         
  integer(i_kind)    nlevsbal    ! max num levs per task, for dist. of balance         
  integer(i_kind)    nnnvsbal    ! num levs current task, for dist. of balance         
  integer(i_kind)    nlevsuv     ! max num levs per task, for dist. of balance         
  integer(i_kind)    nnnvsuv     ! num levs current task, for dist. of balance         

! Optional ESMF-like layout information: nxPE is the number of
! processors used to decompose the longitudinal dimensional, while nyPE 
! the number of processors used to decompose the latitudinal dimension.
! By construction, nPE = nxPE * nyPE.
! 
  integer(i_kind) :: nxpe=-ione     ! optional layout information
  integer(i_kind) :: nype=-ione     ! optional layout information


! communication arrays...set up in init_mpi_vars

  integer(i_kind),allocatable,dimension(:):: levs_id ! vert lev id for each level 
                                             !  of the nsig1o slabs (zero if
                                             !  empty, else can vary between 1-->nsig)


  integer(i_kind),allocatable,dimension(:):: nvar_id ! variable id for each level 
                                             !   of the nsig1o slabs:
                                             !    1: streamfunction
                                             !    2: velocity potential
                                             !    3: surface pressure
                                             !    4: temperature
                                             !    5: q
                                             !    6: ozone
                                             !    7: sea surface temperature
                                             !    8: cloud water
                                             !    9: land skin temperature
                                             !   10: sfc ice temperature
  integer(i_kind),allocatable,dimension(:):: nvarbal_id ! variable id for each level 
                                             !   of the nsig1o slabs:
                                             !    1: streamfunction
                                             !    2: velocity potential
                                             !    3: pressure
                                             !    4: temperature
  integer(i_kind),allocatable,dimension(:,:):: nvar_pe ! pe where each var is kept
  integer(i_kind),allocatable,dimension(:):: ku_gs,kv_gs,kp_gs,kt_gs  ! pointers for balanced level reordering
  integer(i_kind),allocatable,dimension(:):: lu_gs,lv_gs              ! pointers for balanced level reordering
!

! Allocated in init_mpi_vars, defined by init_comm_vars

                                             ! comm. array, displacement ...
  integer(i_kind),allocatable,dimension(:):: isdsp_g !  for send to nsig1o slabs
  integer(i_kind),allocatable,dimension(:):: irdsp_g !  for receive from nsig1o slabs
  integer(i_kind),allocatable,dimension(:):: isdsp_s !  for send from nsig1o slabs
  integer(i_kind),allocatable,dimension(:):: irdsp_s !  for receive from nsig1o slabs

                                             ! comm. array, count ...
  integer(i_kind),allocatable,dimension(:):: iscnt_g !  for send to nsig1o slabs
  integer(i_kind),allocatable,dimension(:):: ircnt_g !  for receive from nsig1o slabs
  integer(i_kind),allocatable,dimension(:):: iscnt_s !  for send from nsig1o slabs
  integer(i_kind),allocatable,dimension(:):: ircnt_s !  for receive from nsig1o slabs
                                             ! comm. array, displacement ...
  integer(i_kind),allocatable,dimension(:):: isdbal_g !  for send to nsig1o slabs
  integer(i_kind),allocatable,dimension(:):: irdbal_g !  for receive from nsig1o slabs
  integer(i_kind),allocatable,dimension(:):: isdbal_s !  for send from nsig1o slabs
  integer(i_kind),allocatable,dimension(:):: irdbal_s !  for receive from nsig1o slabs

                                             ! comm. array, count ...
  integer(i_kind),allocatable,dimension(:):: iscbal_g !  for send to nsig1o slabs
  integer(i_kind),allocatable,dimension(:):: ircbal_g !  for receive from nsig1o slabs
  integer(i_kind),allocatable,dimension(:):: iscbal_s !  for send from nsig1o slabs
  integer(i_kind),allocatable,dimension(:):: ircbal_s !  for receive from nsig1o slabs

                                             ! comm. array, displacement ...
  integer(i_kind),allocatable,dimension(:):: isdvec_g !  for send to nsig1o slabs
  integer(i_kind),allocatable,dimension(:):: irdvec_g !  for receive from nsig1o slabs
  integer(i_kind),allocatable,dimension(:):: isdvec_s !  for send from nsig1o slabs
  integer(i_kind),allocatable,dimension(:):: irdvec_s !  for receive from nsig1o slabs

                                             ! comm. array, count ...
  integer(i_kind),allocatable,dimension(:):: iscvec_g !  for send to nsig1o slabs
  integer(i_kind),allocatable,dimension(:):: ircvec_g !  for receive from nsig1o slabs
  integer(i_kind),allocatable,dimension(:):: iscvec_s !  for send from nsig1o slabs
  integer(i_kind),allocatable,dimension(:):: ircvec_s !  for receive from nsig1o slabs

                                             ! comm. array, displacement ...
  integer(i_kind),allocatable,dimension(:):: isduv_g !  for send to nuvlevs slabs
  integer(i_kind),allocatable,dimension(:):: irduv_g !  for receive from nuvlevs slabs
  integer(i_kind),allocatable,dimension(:):: isduv_s !  for send from nuvlevs slabs
  integer(i_kind),allocatable,dimension(:):: irduv_s !  for receive from nuvlevs slabs

                                             ! comm. array, count ...
  integer(i_kind),allocatable,dimension(:):: iscuv_g !  for send to nuvlevs slabs
  integer(i_kind),allocatable,dimension(:):: ircuv_g !  for receive from nuvlevs slabs
  integer(i_kind),allocatable,dimension(:):: iscuv_s !  for send from nuvlevs slabs
  integer(i_kind),allocatable,dimension(:):: ircuv_s !  for receive from nuvlevs slabs

contains

!-------------------------------------------------------------------------
!    NOAA/NCEP, National Centers for Environmental Prediction GSI        !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  init_mpi_vars --- Initialize variables used in mpi communications
!
! !INTERFACE:
!
  subroutine init_mpi_vars(nsig,mype,nsig1o,nnnn1o)

! !USES:

    implicit none

! !INPUT PARAMETERS:

    integer(i_kind),intent(in   ) :: nsig    ! number of levels
    integer(i_kind),intent(in   ) :: mype    ! task identifier
    integer(i_kind),intent(in   ) :: nsig1o  ! no. of levels distributed on each processor
    integer(i_kind),intent(out  ) :: nnnn1o  ! actual of levels distributed on current processor

! !OUTPUT PARAMETERS:

! !DESCRIPTION: initialize variables used in mpi communications.
!
!     Much of this routine is leftover MPI bits from the SSI code.
!
! !REVISION HISTORY:
!
!   2003-09-30  kleist
!   2004-05-18  kleist, new variables and documentation
!   2004-07-15  todling, protex-compliant prologue
!   2010-04-01  treadon - add nnnn1o to subroutine argument list
!
! !REMARKS:
!
!   language: f90
!   machine:  ibm rs/6000 sp; sgi origin 2000; compaq/hp
!
! !AUTHOR: 
!    kleist           org: np20                date: 2003-09-30
!
!EOP
!-------------------------------------------------------------------------

    integer(i_kind) k,kk,n,mm1
    integer(i_kind) vps,pss,ts,qs,ozs,tss,tls,tis,cwms,varcnt,kchk
    integer(i_kind) levscnt

    allocate(levs_id(nsig1o),nvar_id(nsig1o))
    allocate(nvar_pe(6*nsig+4_i_kind,2))
    allocate(iscnt_g(npe),isdsp_g(npe),ircnt_g(npe),&
       irdsp_g(npe),iscnt_s(npe),isdsp_s(npe),ircnt_s(npe),&
       irdsp_s(npe))
    allocate(iscbal_g(npe),isdbal_g(npe),ircbal_g(npe),&
       irdbal_g(npe),iscbal_s(npe),isdbal_s(npe),ircbal_s(npe),&
       irdbal_s(npe))
    allocate(iscvec_g(npe),isdvec_g(npe),ircvec_g(npe),&
       irdvec_g(npe),iscvec_s(npe),isdvec_s(npe),ircvec_s(npe),&
       irdvec_s(npe))
    allocate(iscuv_g(npe),isduv_g(npe),ircuv_g(npe),&
       irduv_g(npe),iscuv_s(npe),isduv_s(npe),ircuv_s(npe),&
       irduv_s(npe))

    mm1=mype+ione
    nuvlevs=nsig/npe
    if(mod(nsig,npe)/=izero) nuvlevs=nuvlevs+ione


! redefine kchk for uv/stvp distribution
    if (mod(nsig,npe)==izero) then
       kchk=npe
    else
       kchk=mod(nsig,npe)
    end if

    levscnt=izero
    do n=1,npe
       if(n<=kchk) then
          kk=nuvlevs
       else
          kk=nuvlevs-ione
       end if

       do k=1,kk
          levscnt=levscnt+ione
          if ( n==mm1 .and. levscnt<=nsig ) then
             nnnuvlevs=kk
          end if
       end do
    end do

! Initialize slab/subdomain communicators, redefined in
! init_commvars
    do n=1,npe
       iscnt_g(n)   = izero
       isdsp_g(n)   = izero
       ircnt_g(n)   = izero
       irdsp_g(n)   = izero
       iscnt_s(n)   = izero
       isdsp_s(n)   = izero
       ircnt_s(n)   = izero
       irdsp_s(n)   = izero

       iscbal_g(n)  = izero
       isdbal_g(n)  = izero
       ircbal_g(n)  = izero
       irdbal_g(n)  = izero
       iscbal_s(n)  = izero
       isdbal_s(n)  = izero
       ircbal_s(n)  = izero
       irdbal_s(n)  = izero
 
       iscvec_g(n)  = izero
       isdvec_g(n)  = izero
       ircvec_g(n)  = izero
       irdvec_g(n)  = izero
       iscvec_s(n)  = izero
       isdvec_s(n)  = izero
       ircvec_s(n)  = izero
       irdvec_s(n)  = izero

       iscuv_g(n)   = izero
       isduv_g(n)   = izero
       ircuv_g(n)   = izero
       irduv_g(n)   = izero
       iscuv_s(n)   = izero
       isduv_s(n)   = izero
       ircuv_s(n)   = izero
       irduv_s(n)   = izero

    end do
    allocate(lu_gs(nsig),lv_gs(nsig),ku_gs(nsig),kv_gs(nsig),kt_gs(nsig),kp_gs(nsig+1))

! Distribute variables as evenly as possible over the tasks
! start by defining starting points for each variable
    vps =nsig+ione
    pss =vps +nsig
    ts  =pss +ione
    qs  =ts  +nsig
    ozs =qs  +nsig
    tss =ozs +nsig
    tls =tss +ione
    tis =tls +ione
    cwms=tis +ione

! Need to use a variable to know which tasks have a full nsig1o 
! array, and which one have the last level irrelevant
    if (mod((6*nsig)+4_i_kind,npe)==izero) then
       kchk=npe
    else
       kchk=mod((nsig*6)+4_i_kind,npe)
    end if

    nvar_id=izero
    levs_id=izero
    nvar_pe=-999_i_kind

! Define which variable/level each task has for the
! global slabs (levs_id,nvar_id)
    varcnt=izero
    do n=1,npe
       if(n<=kchk) then
          kk=nsig1o
       else
          kk=nsig1o-ione
       end if
       do k=1,kk
          varcnt=varcnt+ione
          nvar_pe(varcnt,1)=n-ione
          nvar_pe(varcnt,2)=k
          if (n==mm1) then
             if (varcnt<vps) then
                nvar_id(k)=ione
                levs_id(k)=varcnt
             else if (varcnt>=vps .and. varcnt<pss) then
                nvar_id(k)=2_i_kind
                levs_id(k)=varcnt-vps+ione
             else if (varcnt==pss) then
                nvar_id(k)=3_i_kind
                levs_id(k)=1_i_kind
             else if (varcnt>=ts .and. varcnt<qs) then
                nvar_id(k)=4_i_kind
                levs_id(k)=varcnt-ts+ione
             else if (varcnt>=qs .and. varcnt<ozs) then
                nvar_id(k)=5_i_kind
                levs_id(k)=varcnt-qs+ione
             else if (varcnt>=ozs .and. varcnt<tss) then
                nvar_id(k)=6_i_kind
                levs_id(k)=varcnt-ozs+ione
             else if (varcnt==tss) then
                nvar_id(k)=7_i_kind
                levs_id(k)=ione
             else if (varcnt==tls) then
                nvar_id(k)=9_i_kind
                levs_id(k)=ione
             else if (varcnt==tis) then
                nvar_id(k)=10_i_kind
                levs_id(k)=ione
             else
                nvar_id(k)=8_i_kind
                levs_id(k)=varcnt-cwms+ione
             end if ! end if for varcnt
          end if ! end if for task id
       end do ! enddo over levs
    end do ! enddo over npe


    nnnn1o=izero
    do k=1,nsig1o
       if (levs_id(k)/=izero) nnnn1o=nnnn1o+ione
    end do


    return
  end subroutine init_mpi_vars

!-------------------------------------------------------------------------
!    NOAA/NCEP, National Centers for Environmental Prediction GSI        !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  destroy_mpi_vars --- deallocate variables used in mpi communications
!
! !INTERFACE:
!

  subroutine destroy_mpi_vars

! !USES:

   implicit none

! !INPUT PARAMETERS:

! !OUTPUT PARAMETERS:

! !DESCRIPTION: deallocate variables used in mpi communications
!
! !REVISION HISTORY:
!
!   2003-09-30  kleist
!   2004-05-13  kleist, documentation
!   2004-07-15  todling, protex-compliant prologue
!
! !REMAKRS:
!
!   language: f90
!   machine:  ibm rs/6000 sp; sgi orgin 2000; compaq/hp
!
! !AUTHOR: 
!    kleist           org: np20                date: 2003-09-30
!
!EOP
!-------------------------------------------------------------------------

    deallocate(levs_id)
    deallocate(nvar_id)
    deallocate(nvar_pe)
    deallocate(iscnt_g,isdsp_g,ircnt_g,&
       irdsp_g,iscnt_s,isdsp_s,ircnt_s,&
       irdsp_s)
    deallocate(iscuv_g,isduv_g,ircuv_g,&
       irduv_g,iscuv_s,isduv_s,ircuv_s,&
       irduv_s)
    deallocate(iscbal_g,isdbal_g,ircbal_g,&
       irdbal_g,iscbal_s,isdbal_s,ircbal_s,&
       irdbal_s)
    deallocate(iscvec_g,isdvec_g,ircvec_g,&
       irdvec_g,iscvec_s,isdvec_s,ircvec_s,&
       irdvec_s)
    deallocate(lu_gs,lv_gs,ku_gs,kv_gs,kt_gs,kp_gs)
    return
  end subroutine destroy_mpi_vars

!-------------------------------------------------------------------------
!    NOAA/NCEP, National Centers for Environmental Prediction GSI        !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  setcomm ---  set mpi communicator
!
! !INTERFACE:
!
  subroutine setcomm(iworld,iworld_group,nsize,members,ncomma,ierr)

! !USES:
    implicit none

! ! INPUT PARAMETERS:

    integer(i_kind)                 ,intent(inout) :: iworld_group
    integer(i_kind)                 ,intent(in   ) :: nsize
    integer(i_kind),dimension(nsize),intent(in   ) :: members

! ! OUTPUT PARAMETERS:

! ! INPUT/OUTPUT PARAMETERS:

    integer(i_kind)                 ,intent(inout) :: iworld,ncomma
    integer(i_kind)                 ,intent(inout) :: ierr

! !DESCRIPTION: set mpi communicator
!
! !REVISION HISTORY:
!
!   2004-07-23  treadon
!   2004-08-04  treadon - protex-compliant prologue
!
! !REMARKS:
!
!   language: f90
!   machine:  ibm rs/6000 sp; sgi origin 2000; compaq/hp
!
! !AUTHOR:
!    treadon           org: np20                date: 2004-07-23
!
!EOP
!-------------------------------------------------------------------------

    integer(i_kind) :: ncommva_group

    ncomma=mpi_comm_world
    iworld=mpi_comm_world
    call mpi_comm_group(iworld,iworld_group,ierr)

    call mpi_group_incl(iworld_group,nsize,members,ncommva_group,ierr)
    call mpi_comm_create(iworld,ncommva_group,ncomma,ierr)
    call mpi_group_free(ncommva_group,ierr)
    return
  end subroutine setcomm

end module mpimod


