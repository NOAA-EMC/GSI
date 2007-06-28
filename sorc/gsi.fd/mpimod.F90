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

#ifdef ibm_sp
! Include standard mpi includes file.
  use mpi
#else
  use m_mpif, only : mpi_rtype => mpi_real8
  use m_mpif, only : mpi_itype => mpi_integer4
  use m_mpif, only : mpi_real8
  use m_mpif, only : mpi_real16
  use m_mpif, only : mpi_comm_world
  use m_mpif, only : mpi_status_size
  use m_mpif, only : mpi_sum
  use m_mpif, only : mpi_integer
  use m_mpif, only : mpi_integer2
  use m_mpif, only : mpi_integer4
  use m_mpif, only : mpi_integer8
  use m_mpif, only : mpi_real4
  use m_mpif, only : mpi_max
  use m_mpif, only : mpi_min
  use m_mpif, only : mpi_offset_kind
  use m_mpif, only : mpi_info_null
  use m_mpif, only : mpi_mode_rdonly
  use m_mpif, only : mpi_mode_rdwr
  use m_mpif, only : mpi_byte
  use m_mpif, only : mpi_seek_set
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

#ifdef ibm_sp
! Define size for mpi_real
  integer(i_kind), parameter :: mpi_rtype=mpi_real8
! integer(i_kind), parameter :: mpi_rtype=mpi_real4
  integer(i_kind), parameter :: mpi_itype=mpi_integer4
#endif

  integer(i_kind) ierror
  integer(i_kind) :: npe         ! total num of MPI tasks
  integer(i_kind)    nuvlevs     ! num levs per task, for dist. of uv/stvp         

! Optional ESMF-like layout information: nxPE is the number of
! processors used to decompose the longitudinal dimensional, while nyPE 
! the number of processors used to decompose the latitudinal dimension.
! By construction, nPE = nxPE * nyPE.
! 
  integer(i_kind) :: nxpe=-1     ! optional layout information
  integer(i_kind) :: nype=-1     ! optional layout information


! communication arrays...set up in init_mpi_vars

  integer(i_kind),allocatable,dimension(:):: levs_id ! vert lev id for each level 
                                             !  of the nsig1o slabs (zero if
                                             !  empty, else can vary between 1-->nsig)

  integer(i_kind),allocatable,dimension(:):: levsuv_id

  integer(i_kind),allocatable,dimension(:):: nvar_id ! variable id for each level 
                                             !   of the nsig1o slabs:
                                             !    1: streamfunction
                                             !    2: velocity potential
                                             !    3: log surface pressure
                                             !    4: temperature
                                             !    5: q
                                             !    6: ozone
                                             !    7: sea surface temperature
                                             !    8: cloud water
                                             !    9: land skin temperature
                                             !   10: sfc ice temperature
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
  subroutine init_mpi_vars(nsig,mype,nsig1o)

! !USES:

    use kinds, only: i_kind
    use constants, only: izero,ione
    implicit none

! !INPUT PARAMETERS:

    integer(i_kind),intent(in):: nsig    ! number of levels
    integer(i_kind),intent(in):: mype    ! task identifier
    integer(i_kind),intent(in):: nsig1o  ! no. of levels distributed on each processor

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

    integer(i_kind) k,kk,n,j,mm1
    integer(i_kind) vps,pss,ts,qs,ozs,tss,tls,tis,cwms,varcnt,kchk
    integer(i_kind) levscnt

    allocate(levs_id(nsig1o),nvar_id(nsig1o))
    allocate(iscnt_g(npe),isdsp_g(npe),ircnt_g(npe),&
       irdsp_g(npe),iscnt_s(npe),isdsp_s(npe),ircnt_s(npe),&
       irdsp_s(npe))
    allocate(iscuv_g(npe),isduv_g(npe),ircuv_g(npe),&
       irduv_g(npe),iscuv_s(npe),isduv_s(npe),ircuv_s(npe),&
       irduv_s(npe))

    mm1=mype+1
    nuvlevs=nsig/npe
    if(mod(nsig,npe)/=izero) nuvlevs=nuvlevs+1

    allocate(levsuv_id(nuvlevs))
    levsuv_id=izero

! redefine kchk for uv/stvp distribution
    if (mod(nsig,npe)==izero) then
      kchk=npe
    else
      kchk=mod(nsig,npe)
    end if

    levscnt=izero
    do n=1,npe
      if(n.le.kchk) then
        kk=nuvlevs
      else
        kk=nuvlevs-1
      end if

      do k=1,kk
        levscnt=levscnt+ione
	if ( n==mm1 .and. levscnt.le.nsig ) then
	  levsuv_id(k)=levscnt
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

      iscuv_g(n)   = izero
      isduv_g(n)   = izero
      ircuv_g(n)   = izero
      irduv_g(n)   = izero
      iscuv_s(n)   = izero
      isduv_s(n)   = izero
      ircuv_s(n)   = izero
      irduv_s(n)   = izero

    end do

! Distribute variables as evenly as possible over the tasks
! start by defining starting points for each variable
    vps=nsig+1
    pss=vps+nsig
    ts=pss+1
    qs=ts+nsig
    ozs=qs+nsig
    tss=ozs+nsig
    tls=tss+1
    tis=tls+1
    cwms=tis+1

! Need to use a variable to know which tasks have a full nsig1o 
! array, and which one have the last level irrelevant
    if (mod((6*nsig)+4,npe)==izero) then
      kchk=npe
    else
      kchk=mod((nsig*6)+4,npe)
    end if

    nvar_id=izero
    levs_id=izero

! Define which variable/level each task has for the
! global slabs (levs_id,nvar_id)
    varcnt=izero
    do n=1,npe
      if(n.le.kchk) then
        kk=nsig1o
      else
        kk=nsig1o-1
      end if
      do k=1,kk
        varcnt=varcnt+1
        if (n==mm1) then
          if (varcnt.lt.vps) then
            nvar_id(k)=1
            levs_id(k)=varcnt
          else if (varcnt.ge.vps .and. varcnt.lt.pss) then
            nvar_id(k)=2
            levs_id(k)=varcnt-vps+1
          else if (varcnt.eq.pss) then
            nvar_id(k)=3
            levs_id(k)=1
          else if (varcnt.ge.ts .and. varcnt.lt.qs) then
            nvar_id(k)=4
            levs_id(k)=varcnt-ts+1
          else if (varcnt.ge.qs .and. varcnt.lt.ozs) then
            nvar_id(k)=5
            levs_id(k)=varcnt-qs+1
          else if (varcnt.ge.ozs .and. varcnt.lt.tss) then
            nvar_id(k)=6
            levs_id(k)=varcnt-ozs+1
          else if (varcnt.eq.tss) then
            nvar_id(k)=7
            levs_id(k)=1
          else if (varcnt.eq.tls) then
            nvar_id(k)=9
            levs_id(k)=1
          else if (varcnt.eq.tis) then
            nvar_id(k)=10
            levs_id(k)=1
          else
            nvar_id(k)=8
            levs_id(k)=varcnt-cwms+1
          end if ! end if for varcnt
        end if ! end if for task id
      end do ! enddo over levs
    end do ! enddo over npe

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
    deallocate(levsuv_id)
    deallocate(iscnt_g,isdsp_g,ircnt_g,&
       irdsp_g,iscnt_s,isdsp_s,ircnt_s,&
       irdsp_s)
    deallocate(iscuv_g,isduv_g,ircuv_g,&
       irduv_g,iscuv_s,isduv_s,ircuv_s,&
       irduv_s)
    return
  end subroutine destroy_mpi_vars

!-------------------------------------------------------------------------
!    NOAA/NCEP, National Centers for Environmental Prediction GSI        !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  reorder --- reorder work array post mpi communication
!
! !INTERFACE:
!
  subroutine reorder(work,k_in)

! !USES:

    use kinds, only: r_kind,i_kind
    use constants, only: zero
    use gridmod, only: ijn,itotsub,iglobal
    implicit none

! !INPUT PARAMETERS:

   integer(i_kind), intent(in) ::  k_in    ! number of levs in work array

! !INPUT/OUTPUT PARAMETERS:

    real(r_kind),dimension(max(iglobal,itotsub)*k_in),intent(inout):: work ! array to reorder

! !OUTPUT PARAMETERS:

! !DESCRIPTION: reorder work array post mpi communication
!
! !REVISION HISTORY:
!
!   2004-01-25  kleist
!   2004-05-14  kleist, documentation
!   2004-07-15  todling, protex-compliant prologue
!   2004-03-30  treadon - replace itotsub with max(iglobal,itotsub) in work dimension
!
! !REMAKRS:
!
!   language: f90
!   machine:  ibm rs/6000 sp; sgi origin 2000; compaq/hp
!
! !AUTHOR: 
!    kleist           org: np20                date: 2004-01-25
!
!EOP
!-------------------------------------------------------------------------

    integer(i_kind) iloc,iskip,i,j,k,n
    real(r_kind),dimension(max(iglobal,itotsub),k_in):: temp

! Zero out temp array
    do k=1,k_in
       do i=1,itotsub
          temp(i,k)=zero
       end do
    end do
	
! Load temp array in desired order
    do k=1,k_in
      iskip=0
      iloc=0
      do n=1,npe
        if (n/=1) then
          iskip=iskip+ijn(n-1)*k_in
        end if
        do i=1,ijn(n)
          iloc=iloc+1
          temp(iloc,k)=work(i + iskip + &
                   (k-1)*ijn(n))
        end do
      end do
    end do

! Load the temp array back into work
    iloc=0
    do k=1,k_in
      do i=1,itotsub
        iloc=iloc+1
        work(iloc)=temp(i,k)
      end do
    end do

    return
  end subroutine reorder

!-------------------------------------------------------------------------
!    NOAA/NCEP, National Centers for Environmental Prediction GSI        !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  reorder2 --- reorder work array post mpi communication
!
! !INTERFACE:
!

  subroutine reorder2(work,k_in)

! !USES:

    use kinds, only: r_kind,i_kind
    use constants, only: zero
    use gridmod, only: ijn_s,itotsub
    implicit none


! !INPUT PARAMETERS:

   integer(i_kind), intent(in) ::  k_in    ! number of levs in work array

! !INPUT/OUTPUT PARAMETERS:

    real(r_kind),dimension(itotsub,k_in),intent(inout):: work

! !OUTPUT PARAMETERS:

! !DESCRIPTION: reorder work array pre mpi communication
!
! !REVISION HISTORY:
!
!   2004-01-25  kleist
!   2004-05-14  kleist, documentation
!   2004-07-15  todling, protex-compliant prologue
!
! !REMARKS:
!   language: f90
!   machine:  ibm rs/6000 sp; sgi origin 2000; compaq/hp
!
! !AUTHOR: 
!    kleist           org: np20                date: 2004-01-25
!
!EOP
!-------------------------------------------------------------------------

    integer(i_kind) iloc,iskip,i,k,n
    real(r_kind),dimension(itotsub*k_in):: temp

! Zero out temp array
    do k=1,itotsub*k_in
       temp(k)=zero
    end do

! Load temp array in order of subdomains
    iloc=0
    iskip=0
    do n=1,npe
      if (n/=1) then
        iskip=iskip+ijn_s(n-1)
      end if

      do k=1,k_in
        do i=1,ijn_s(n)
          iloc=iloc+1
          temp(iloc)=work(iskip+i,k)
        end do
      end do
    end do

! Now load the tmp array back into work
    iloc=0
    do k=1,k_in
      do i=1,itotsub
        iloc=iloc+1
        work(i,k)=temp(iloc)
      end do
    end do

    return
  end subroutine reorder2

!-------------------------------------------------------------------------
!    NOAA/NCEP, National Centers for Environmental Prediction GSI        !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  strip_single --- strip off buffer points froms subdomains for 
!                       mpi comm purposes (works with 4 byte reals)
!
! !INTERFACE:
!
  subroutine strip_single(field_in,field_out,nz)

! !USES:

    use kinds, only: r_single,i_kind
    use gridmod, only: lat1,lon1,lat2,lon2
    implicit none

! !INPUT PARAMETERS:

    integer(i_kind), intent(in)::  nz        !  number of levs in subdomain array
    real(r_single),dimension(lat2,lon2,nz),intent(in):: field_in   ! full subdomain 
                                                                 !    array containing 
                                                                 !    buffer points
! !OUTPUT PARAMETERS:

    real(r_single),dimension(lat1,lon1,nz),intent(out):: field_out ! subdomain array
                                                                 !   with buffer points
                                                                 !   stripped off

! !DESCRIPTION: strip off buffer points froms subdomains for mpi comm
!               purposes
!
! !REVISION HISTORY:
!
!   2004-01-25  kleist
!   2004-05-14  kleist, documentation
!   2004-07-15  todling, protex-compliant prologue
!
! !REMARKS:
!
!   language: f90
!   machine:  ibm rs/6000 sp; sgi origin 2000; compaq/hp
!
! !AUTHOR: 
!    kleist           org: np20                date: 2004-01-25
!
!EOP
!-------------------------------------------------------------------------

    integer(i_kind) i,j,k,jp1

    do k=1,nz
      do j=1,lon1
        jp1 = j+1
        do i=1,lat1
          field_out(i,j,k)=field_in(i+1,jp1,k)
        end do
      end do
    end do

    return
  end subroutine strip_single

!-------------------------------------------------------------------------
!    NOAA/NCEP, National Centers for Environmental Prediction GSI        !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  strip --- strip off buffer points froms subdomains for 
!                       mpi comm purposes
!
! !INTERFACE:
!
  subroutine strip(field_in,field_out,nz)

! !USES:

    use kinds, only: r_kind,i_kind
    use gridmod, only: lat1,lon1,lat2,lon2
    implicit none

! !INPUT PARAMETERS:

    integer(i_kind), intent(in)::  nz        !  number of levs in subdomain array
    real(r_kind),dimension(lat2,lon2,nz),intent(in):: field_in   ! full subdomain 
                                                                  !    array containing 
                                                                  !    buffer points
! !OUTPUT PARAMETERS:

    real(r_kind),dimension(lat1,lon1,nz),intent(out):: field_out ! subdomain array
                                                                  !   with buffer points
                                                                  !   stripped off

! !DESCRIPTION: strip off buffer points froms subdomains for mpi comm
!               purposes
!
! !REVISION HISTORY:
!
!   2004-01-25  kleist
!   2004-05-14  kleist, documentation
!   2004-07-15  todling, protex-compliant prologue
!
! !REMARKS:
!
!   language: f90
!   machine:  ibm rs/6000 sp; sgi origin 2000; compaq/hp
!
! !AUTHOR: 
!    kleist           org: np20                date: 2004-01-25
!
!EOP
!-------------------------------------------------------------------------

    integer(i_kind) i,j,k,jp1

    do k=1,nz
      do j=1,lon1
        jp1 = j+1
        do i=1,lat1
          field_out(i,j,k)=field_in(i+1,jp1,k)
        end do
      end do
    end do

    return
  end subroutine strip


!-------------------------------------------------------------------------
!    NOAA/NCEP, National Centers for Environmental Prediction GSI        !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  vectosub --- transform vector array into three dimensional 
!                          subdomain array
!
! !INTERFACE:
!
  subroutine vectosub(fld_in,nz,fld_out)

    use kinds, only: r_kind,i_kind
    use gridmod, only: lat2,lon2
    implicit none

! !INPUT PARAMETERS:

    integer(i_kind), intent(in) ::  nz    ! number of levs in subdomain array
    real(r_kind),dimension(lat2*lon2*nz),intent(in):: fld_in ! subdomain array 
                                                              !   in vector form

! !OUTPUT PARAMETERS:

    real(r_kind),dimension(lat2,lon2,nz),intent(out):: fld_out ! three dimensional 
                                                                !  subdomain variable array

! !DESCRIPTION: Transform vector array into three dimensional subdomain
!               array
!
! !REVISION HISTORY:
!
!   2004-01-25  kleist
!   2004-05-14  kleist, documentation
!   2004-07-15  todling, protex-compliant prologue
!
! !REMARKS:
!   language: f90
!   machine:  ibm rs/6000 sp; sgi origin 2000; compaq/hp
!
! !AUTHOR: 
!   kleist           org: np20                date: 2004-01-25
!
!EOP
!-------------------------------------------------------------------------

    integer(i_kind) i,j,k,iloc

    iloc=0
    do k=1,nz
      do j=1,lon2
        do i=1,lat2
          iloc=iloc+1
          fld_out(i,j,k)=fld_in(iloc)
        end do 
      end do
    end do

    return
  end subroutine vectosub

!-------------------------------------------------------------------------
!    NOAA/NCEP, National Centers for Environmental Prediction GSI        !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  reload --- Transfer contents of 2-d array to 3-d array
!
! !INTERFACE:
!
subroutine reload(work_in,work_out)

! !USES:

  use kinds, only: r_kind,i_kind
  use gridmod, only: lat2,lon2,nsig
  implicit none

! !INPUT PARAMETERS:

  real(r_kind),dimension(lat2*lon2,nsig),intent(in):: work_in   ! 2-d array

! !OUTPUT PARAMETERS:

  real(r_kind),dimension(lat2,lon2,nsig),intent(out) :: work_out  ! 3-d array

! !DESCRIPTION: Transfer contents of 2-d array to 3-d array
!
! !REVISION HISTORY:
!   2004-05-14  treadon
!   2004-07-15  todling, protex-compliant prologue
!
! !REMARKS:
!
!   language: f90
!   machine:  ibm rs/6000 sp; sgi origin 2000; compaq/hp
!
! !AUTHOR: 
!   treadon          org: np23                date: 2004-05-14
!
!EOP
!-------------------------------------------------------------------------

  integer(i_kind) i,j,k,ij

  do k=1,nsig
     ij=0
     do j=1,lon2
        do i=1,lat2
           ij=ij+1
           work_out(i,j,k)=work_in(ij,k)
        end do
     end do
  end do
  return
end subroutine reload

!-------------------------------------------------------------------------
!    NOAA/NCEP, National Centers for Environmental Prediction GSI        !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  strip_periodic --- strip off buffer points from periodic
!                       subdomains for mpi comm purposes
!
! !INTERFACE:
!
  subroutine strip_periodic(field_in,field_out,nz)

! !USES:

    use kinds, only: r_kind,i_kind
    use gridmod, only: lat1,lon1,lat2,lon2
    implicit none

! !INPUT PARAMETERS:

    integer(i_kind), intent(in)::  nz        !  number of levs in subdomain array
    real(r_kind),dimension(lat2,lon2,nz),intent(in):: field_in   ! full subdomain
                                                                  !    array containing
                                                                  !    buffer points
! !OUTPUT PARAMETERS:

    real(r_kind),dimension(lat1,lon1,nz),intent(out):: field_out ! subdomain array
                                                                  !   with buffer points
                                                                  !   stripped off

! !DESCRIPTION: strip off buffer points froms subdomains for mpi comm
!               purposes
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

    integer(i_kind) i,j,k,jp1

    do k=1,nz
      do j=1,lon1
        jp1 = j+1
        do i=1,lat1
          field_out(i,j,k)=field_in(i+1,jp1,k)
        end do
      end do
    end do
    do k=1,nz
       do i=1,lat1
          field_out(i,1,k)    = field_out(i,1,k)    + field_in(i+1,lon2,k)
          field_out(i,lon1,k) = field_out(i,lon1,k) + field_in(i+1,1,k)
       end do
    end do

    return
  end subroutine strip_periodic

  subroutine setcomm(iworld,iworld_group,nsize,members,ncomma,ierr)
    use kinds, only: i_kind
    implicit none
    integer(i_kind):: n_end,iworld,iworld_group,n_beg,ncomma,nstep
    integer(i_kind):: k,ncommva_group,ierr,nsize
    integer(i_kind),dimension(nsize):: members
    integer(i_kind),dimension(nsize)::ncomm_array

    call mpi_group_incl(iworld_group,nsize,members,ncommva_group,ierr)
    call mpi_comm_create(iworld,ncommva_group,ncomma,ierr)
    call mpi_group_free(ncommva_group,ierr)
    return
  end subroutine setcomm

end module mpimod


