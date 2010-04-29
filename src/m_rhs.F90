module m_rhs
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:	 module m_rhs
!   prgmmr:	 j guo <jguo@nasa.gov>
!      org:	 NASA/GSFC, Global Modeling and Assimilation Office, 900.3
!     date:	 2010-03-22
!
! abstract: defines persistant workspace for multiple-pass setuprhsall()
!
! program history log:
!   2010-03-22  j guo   - added this document block
!
!   input argument list: see Fortran 90 style document below
!
!   output argument list: see Fortran 90 style document below
!
! attributes:
!   language: Fortran 90 and/or above
!   machine:
!
!$$$  end subprogram documentation block

!#define VERBOSE
!#define DEBUG_TRACE
#include "mytrace.H"

! module interface:

  use kinds, only: r_kind, i_kind, r_single
  use mpeu_util, only: die,perr,tell
  implicit none
  private
  public:: rhs_alloc		! interface for allocation
  public:: rhs_dealloc		! interface for deallocation
  public:: rhs_allocated	! state of all moduel variables

  public:: rhs_awork		! variables ...
  public:: rhs_bwork
  public:: rhs_aivals
  public:: rhs_stats
  public:: rhs_stats_oz
  public:: rhs_toss_gps

  !! GPS processing specific interfaces and variables

  public:: rhs_GPSalloc
  public:: rhs_GPSaliases
  public:: rhs_GPSunaliases
  public:: rhs_GPSdealloc

  public:: muse
  public:: termq
  public:: termpk
  public:: termt
  public:: termtk
  public:: termtl
  public:: termpl1
  public:: termpl2
  public:: pressure
  public:: dpresl
  public:: dpressure
  public:: dpres
  public:: dbend
  public:: dbend_loc
  public:: xj
  public:: n_t
  public:: n_q
  public:: n_p
  public:: nrefges
  public:: rges
  public:: gp2gm
  public:: prsltmp_o
  public:: tges_o
  public:: gps2work
  public:: error
  public:: error_adjst
  public:: ratio_errors
  public:: cutoff
  public:: rdiagbuf
  public:: cdiagbuf

  public:: qcfail
  public:: qcfail_loc
  public:: qcfail_high
  public:: qcfail_gross
  public:: qcfail_stats_1
  public:: qcfail_stats_2

  public:: data_ier
  public:: data_igps
  public:: data_ihgt

! Revision history:
!   2009-08-19  guo     - created to support multi-pass setuprhsall().
!			  This module contains all statistics variables
!			  defined for any single pass but all passes.

  !! usage:
  !!	use xyz_mod, only: npres_print,nconvtype,nsig
  !!	use m_rhs, only: rhs_alloc
  !!	use m_rhs, only: rhs_dealloc
  !!	use m_rhs, only: rhs_allocated
  !!	use m_rhs, only: awork => rhs_awork
  !!	use m_rhs, only: bwork => rhs_bwork
  !!
  !!	if(.not.rhs_allocated) &
  !!		call rhs_alloc()
  !!	call xxxx(awork,bwork,...)
  !!    call rhs_dealloc()

  logical,save:: rhs_allocated=.false.
  real(r_kind),allocatable,dimension(:,:    ),save:: rhs_awork
  real(r_kind),allocatable,dimension(:,:,:,:),save:: rhs_bwork
  real(r_kind),allocatable,dimension(:,:    ),save:: rhs_aivals
  real(r_kind),allocatable,dimension(:,:    ),save:: rhs_stats
  real(r_kind),allocatable,dimension(:,:    ),save:: rhs_stats_oz
  real(r_kind),allocatable,dimension(:      ),save:: rhs_toss_gps

  type rhs_GPSbuffer
    private
    logical:: alloced =.false.
    character(len=max(len('ref'),len('bend'))):: class =""

    logical         , pointer, dimension(  :):: muse => null()

    real(r_kind    ), pointer, dimension(:,:):: gps2work => null()
    real(r_kind    ), pointer, dimension(  :):: error    => null()
    real(r_kind    ), pointer, dimension(  :):: error_adjst => null()
    real(r_kind    ), pointer, dimension(  :):: ratio_errors=> null()
    real(r_kind    ), pointer, dimension(  :):: cutoff => null()

    	! case: class=="ref"
    real(r_kind    ), pointer, dimension(  :):: termq  => null()
    real(r_kind    ), pointer, dimension(  :):: termpk => null()
    real(r_kind    ), pointer, dimension(  :):: termt  => null()
    real(r_kind    ), pointer, dimension(  :):: termtk => null()
    real(r_kind    ), pointer, dimension(:,:):: termtl => null()
    real(r_kind    ), pointer, dimension(:,:):: termpl1=> null()
    real(r_kind    ), pointer, dimension(:,:):: termpl2=> null()
    real(r_kind    ), pointer, dimension(  :):: pressure => null()
    real(r_kind    ), pointer, dimension(  :):: dpresl => null()

    	! case: class=="bend"
    real(r_kind    ), pointer, dimension(  :):: dpres	=> null()
    real(r_kind    ), pointer, dimension(  :):: dpressure => null()
    real(r_kind    ), pointer, dimension(  :):: dbend	=> null()
    real(r_kind    ), pointer, dimension(:,:):: dbend_loc => null()
    real(r_kind    ), pointer, dimension(:,:):: xj	=> null()
    real(r_kind    ), pointer, dimension(:,:):: n_t	=> null()
    real(r_kind    ), pointer, dimension(:,:):: n_q	=> null()
    real(r_kind    ), pointer, dimension(:,:):: n_p	=> null()
    real(r_kind    ), pointer, dimension(:,:):: nrefges	=> null()
    real(r_kind    ), pointer, dimension(:,:):: rges	=> null()
    real(r_kind    ), pointer, dimension(:,:):: gp2gm	=> null()
    real(r_kind    ), pointer, dimension(:,:):: prsltmp_o => null()
    real(r_kind    ), pointer, dimension(:,:):: tges_o	=> null()

    real(r_kind    ), pointer, dimension(:,:):: rdiagbuf => null()
    character(len=8), pointer, dimension(  :):: cdiagbuf => null()

    logical         , pointer, dimension(  :):: qcfail => null()
    real(r_single  ), pointer, dimension(  :):: qcfail_loc  => null()
    real(r_single  ), pointer, dimension(  :):: qcfail_high => null()
    real(r_single  ), pointer, dimension(  :):: qcfail_gross=> null()
    real(r_single  ), pointer, dimension(  :):: qcfail_stats_1 => null()
    real(r_single  ), pointer, dimension(  :):: qcfail_stats_2 => null()

    real(r_kind    ), pointer, dimension(  :):: data_ier  => null()
    real(r_kind    ), pointer, dimension(  :):: data_igps => null()
    real(r_kind    ), pointer, dimension(  :):: data_ihgt => null()
  end type rhs_GPSbuffer

  type(rhs_GPSbuffer),dimension(:),allocatable,target,save:: aGPSbuffer

!! Aliases to all private components of the is-th element of aGPSbuffer(1:ndat).
!! These aliases will let the uses of these variables unchanged in setupref()
!! and setupbend() routines.

  logical         , pointer, dimension(  :), save:: muse

  real(r_kind    ), pointer, dimension(  :), save:: error,error_adjst
  real(r_kind    ), pointer, dimension(:,:), save:: gps2work
  real(r_kind    ), pointer, dimension(  :), save:: ratio_errors,cutoff

  	! case: class=='ref'
  real(r_kind    ), pointer, dimension(  :), save:: termq,termpk,termt,termtk
  real(r_kind    ), pointer, dimension(:,:), save:: termtl,termpl1,termpl2
  real(r_kind    ), pointer, dimension(  :), save:: dpresl, pressure

  	! case: class=='bend'
  real(r_kind    ), pointer, dimension(  :), save:: dpres ,dpressure,dbend
  real(r_kind    ), pointer, dimension(:,:), save:: dbend_loc,xj
  real(r_kind    ), pointer, dimension(:,:), save:: n_t,n_q,n_p,nrefges
  real(r_kind    ), pointer, dimension(:,:), save:: rges,gp2gm,prsltmp_o,tges_o

  real(r_kind    ), pointer, dimension(:,:), save:: rdiagbuf
  character(len=8), pointer, dimension(  :), save:: cdiagbuf

  logical         , pointer, dimension(  :), save:: qcfail
  real(r_single  ), pointer, dimension(  :), save:: qcfail_loc,qcfail_high,qcfail_gross
  real(r_single  ), pointer, dimension(  :), save:: qcfail_stats_1,qcfail_stats_2

  real(r_kind    ), pointer, dimension(  :), save:: data_ier
  real(r_kind    ), pointer, dimension(  :), save:: data_igps
  real(r_kind    ), pointer, dimension(  :), save:: data_ihgt

  character(len=*),parameter:: myname="m_rhs"

contains

!#ifdef _TRACE_
!#define _ENTRY_	call tell(myname_,'entered ..')
!#define _EXIT_	call tell(myname_,'exiting ..')
!#define _TELL_(M)  call tell(myname_,#M//" =",M)
!#else
!#define _ENTRY_
!#define _EXIT_
!#define _TELL_(M)
!#endif

subroutine rhs_alloc()
  	! supporting information
  use kinds, only: i_kind
  use constants, only: zero,izero

  	! run-time dimensional information
  use obsmod  , only: ndat
  use obsmod  , only: nprof_gps
  use radinfo , only: jpch_rad
  use ozinfo  , only: jpch_oz
  use qcmod   , only: npres_print
  use gridmod , only: nsig
  use convinfo, only: nconvtype

  	! indirectly used counter
  use obsmod  , only: nchan_total
  implicit none
  character(len=*),parameter:: myname_=myname//'.alloc'
_ENTRY_(myname_)
  if(rhs_allocated) call die(myname_,'already allocated')

#ifdef VERBOSE
  call tell(myname_,'nsig ='       ,nsig)
  call tell(myname_,'npres_print =',npres_print)
  call tell(myname_,'nconvtype ='  ,nconvtype)
  call tell(myname_,'ndat ='       ,ndat)
  call tell(myname_,'jpch_rad ='   ,jpch_rad)
  call tell(myname_,'jpch_oz ='    ,jpch_oz)
  call tell(myname_,'nprof_gps ='  ,nprof_gps)
#endif

  rhs_allocated=.true.
  allocate(rhs_awork(7*nsig+100,13))
  allocate(rhs_bwork(npres_print,nconvtype,5,3))
  allocate(rhs_aivals(40,ndat))
  allocate(rhs_stats(7,jpch_rad))
  allocate(rhs_stats_oz(9,jpch_oz))

  allocate(rhs_toss_gps(max(1,nprof_gps)))
  allocate(aGPSbuffer(ndat))
  aGPSbuffer(1:ndat)%alloced=.false.

  rhs_awork    =zero
  rhs_bwork    =zero
  rhs_aivals   =zero
  rhs_stats    =zero
  rhs_stats_oz =zero
  rhs_toss_gps =zero

  nchan_total      =izero
_EXIT_(myname_)
end subroutine rhs_alloc

subroutine rhs_GPSalloc(is,class,nobs,nsig,nreal,grids_dim)
  use constants, only: zero
  implicit none
  integer(i_kind),intent(in) :: is
  character(len=*),intent(in) :: class
  integer(i_kind),intent(in) :: nobs
  integer(i_kind),intent(in) :: nsig
  integer(i_kind),intent(in) :: nreal
  integer(i_kind),intent(in) :: grids_dim

  type(rhs_GPSbuffer),pointer:: b
  character(len=*),parameter:: myname_=myname//'.GPSalloc'
_ENTRY_(myname_)
  b=>aGPSbuffer(is)	! b is aliased to an entry in rhs_GPSbuffer

  b%alloced =.true.
  b%class   = class

  allocate(b%muse(nobs))

  b%muse(:)=.false.

  select case(b%class)
  case('ref')

    allocate(b%termq  (     nobs))
    allocate(b%termpk (     nobs))
    allocate(b%termpl1(nsig,nobs))
    allocate(b%termpl2(nsig,nobs))

    b%termq  (  :)=zero
    b%termpk (  :)=zero
    b%termpl1(:,:)=zero
    b%termpl2(:,:)=zero

    allocate(b%termt  (     nobs))
    allocate(b%termtk (     nobs))
    allocate(b%termtl (nsig,nobs))

    b%termt (  :)=zero
    b%termtk(  :)=zero
    b%termtl(:,:)=zero

    allocate(b%pressure    (nobs))
    allocate(b%dpresl(nobs))

    b%pressure(:)=huge(b%pressure)
    b%dpresl  (:)=huge(b%dpresl  )

  case('bend')

    allocate(b%dpressure(nobs))
    allocate(b%dpres    (nobs))

    b%dpressure(:)=huge(b%dpressure)
    b%dpres    (:)=huge(b%dpres    )

    allocate(b%dbend    (          nobs))
    allocate(b%dbend_loc(grids_dim,nobs))
    allocate(b%xj       (grids_dim,nobs))

    b%dbend(:)=HUGE(b%dbend)
    b%dbend_loc(:,:) = HUGE(b%dbend_loc)
    b%xj(:,:)=HUGE(b%xj)

    allocate(b%n_t(nsig,nobs))
    allocate(b%n_q(nsig,nobs))
    allocate(b%n_p(nsig,nobs))
    allocate(b%nrefges(nsig+10,nobs))

    b%n_t(:,:)=HUGE(b%n_t)
    b%n_q(:,:)=HUGE(b%n_q)
    b%n_p(:,:)=HUGE(b%n_p)
    b%nrefges(:,:)=HUGE(b%nrefges)

    allocate(b%rges     (nsig,nobs))
    allocate(b%gp2gm    (nsig,nobs))
    allocate(b%prsltmp_o(nsig,nobs))
    allocate(b%tges_o   (nsig,nobs))

    b%rges(:,:) = HUGE(b%rges)
    b%gp2gm(:,:) = HUGE(b%gp2gm)
    b%prsltmp_o(:,:) = HUGE(b%prsltmp_o)
    b%tges_o(:,:) = HUGE(b%tges_o)

  end select

  allocate(b%gps2work(3,nobs))

  b%gps2work(:,:)=zero

  allocate(b%error       (nobs))
  allocate(b%error_adjst (nobs))
  allocate(b%ratio_errors(nobs))

  b%error       (:)=huge(b%error)
  b%error_adjst (:)=huge(b%error_adjst)
  b%ratio_errors(:)=huge(b%ratio_errors)

  allocate(b%cutoff(nobs))

  b%cutoff(:)=huge(b%cutoff)

  allocate(b%rdiagbuf(nreal,nobs))
  allocate(b%cdiagbuf(      nobs))

  b%rdiagbuf(:,:)=huge(b%rdiagbuf)
  b%cdiagbuf(  :)=""

  allocate(b%qcfail        (nobs))
  allocate(b%qcfail_loc    (nobs))
  allocate(b%qcfail_high   (nobs))
  allocate(b%qcfail_gross  (nobs))
  allocate(b%qcfail_stats_1(nobs))
  allocate(b%qcfail_stats_2(nobs))

  b%qcfail=.false.
  b%qcfail_loc    =zero
  b%qcfail_high   =zero
  b%qcfail_gross  =zero
  b%qcfail_stats_1=zero
  b%qcfail_stats_2=zero

  allocate(b%data_ier (nobs))
  allocate(b%data_igps(nobs))
  allocate(b%data_ihgt(nobs))
  
  b%data_ier (:)=huge(b%data_ier)
  b%data_igps(:)=huge(b%data_igps)
  b%data_ihgt(:)=huge(b%data_ihgt)
_EXIT_(myname_)
end subroutine rhs_GPSalloc

subroutine rhs_GPSdealloc(is)
  implicit none
  integer(i_kind),intent(in) :: is

  type(rhs_GPSbuffer),pointer:: b
  character(len=*),parameter:: myname_=myname//'.GPSdealloc'

_ENTRY_(myname_)
  b=>aGPSbuffer(is)	! b is aliased to an entry in rhs_GPSbuffer

  deallocate(b%muse)

  select case(b%class)
  case('ref')
    deallocate(b%termq  )
    deallocate(b%termpk )
    deallocate(b%termpl1)
    deallocate(b%termpl2)

    deallocate(b%termt  )
    deallocate(b%termtk )
    deallocate(b%termtl )

    deallocate(b%pressure)
    deallocate(b%dpresl)

  case('bend')
    deallocate(b%dpressure)
    deallocate(b%dpres)

    deallocate(dbend,dbend_loc,xj)
    deallocate(n_t,n_q,n_p,nrefges)
    deallocate(rges,gp2gm,prsltmp_o,tges_o)
  end select

  deallocate(b%gps2work)

  deallocate(b%error       )
  deallocate(b%error_adjst )
  deallocate(b%ratio_errors)

  deallocate(b%cutoff)

  deallocate(b%rdiagbuf)
  deallocate(b%cdiagbuf)

  deallocate(b%qcfail        )
  deallocate(b%qcfail_loc    )
  deallocate(b%qcfail_high   )
  deallocate(b%qcfail_gross  )
  deallocate(b%qcfail_stats_1)
  deallocate(b%qcfail_stats_2)

  deallocate(b%data_ier )
  deallocate(b%data_igps)
  deallocate(b%data_ihgt)

  b%class=""
  b%alloced=.false.
_EXIT_(myname_)
end subroutine rhs_GPSdealloc

subroutine rhs_GPSaliases(is)
  implicit none
  integer(i_kind),intent(in) :: is

  type(rhs_GPSbuffer),pointer:: b
  character(len=*),parameter:: myname_=myname//'.GPSaliases'
_ENTRY_(myname_)
_PRINT_(myname_,is)
  b=>aGPSbuffer(is)	! b is aliased to an entry in rhs_GPSbuffer

_PRINT_(myname_,count(b%muse))
  muse		=> b%muse

  select case(b%class)
  case('ref')
    termq		=> b%termq
    termpk	=> b%termpk
    termt		=> b%termt
    termtk	=> b%termtk
    termtl	=> b%termtl
    termpl1	=> b%termpl1
    termpl2	=> b%termpl2
    pressure	=> b%pressure
    dpresl	=> b%dpresl

  case('bend')
    dpressure	=> b%dpressure
    dpres	=> b%dpres
    dbend	=> b%dbend
    dbend_loc	=> b%dbend_loc
    xj		=> b%xj
    n_t		=> b%n_t
    n_q		=> b%n_q
    n_p		=> b%n_p
    nrefges	=> b%nrefges
    rges	=> b%rges
    gp2gm	=> b%gp2gm
    prsltmp_o	=> b%prsltmp_o
    tges_o	=> b%tges_o
  end select

  gps2work	=> b%gps2work

  error		=> b%error
  error_adjst	=> b%error_adjst
  ratio_errors	=> b%ratio_errors

  cutoff	=> b%cutoff

  rdiagbuf	=> b%rdiagbuf
  cdiagbuf	=> b%cdiagbuf

_PRINT_(myname_,count(b%qcfail))
  qcfail	=> b%qcfail
  qcfail_loc	=> b%qcfail_loc
  qcfail_high	=> b%qcfail_high
  qcfail_gross	=> b%qcfail_gross
  qcfail_stats_1=> b%qcfail_stats_1
  qcfail_stats_2=> b%qcfail_stats_2

  data_ier	=> b%data_ier
  data_igps	=> b%data_igps
  data_ihgt	=> b%data_ihgt
_EXIT_(myname_)
end subroutine rhs_GPSaliases

subroutine rhs_GPSunaliases()
  implicit none
  character(len=*),parameter:: myname_=myname//'.GPSunaliases'
#ifdef DEBUG_TRACE
  type(rhs_GPSbuffer),pointer:: b
  integer:: is,idat

_ENTRY_(myname_)

  idat=0
  do is=1,size(aGPSbuffer)
    b=>aGPSbuffer(is)
    idat=is
    if(associated(muse,b%muse)) exit
  enddo
_PRINT_(myname_,idat)
_PRINT_(myname_,count(muse))

#endif
  nullify(muse)
  nullify(termq)
  nullify(termpk,termpl1,termpl2)
  nullify(termt,termtk,termtl)
  nullify(pressure)
  nullify(dpresl)
  nullify(dpressure)
  nullify(dpres)
  nullify(dbend,dbend_loc,xj)
  nullify(n_t,n_q,n_p,nrefges)
  nullify(rges,gp2gm,prsltmp_o,tges_o)
  nullify(gps2work)
  nullify(error,error_adjst,ratio_errors)
  nullify(cutoff)
  nullify(rdiagbuf,cdiagbuf)
  nullify(qcfail,qcfail_loc,qcfail_gross)
  nullify(qcfail_stats_1,qcfail_stats_2,qcfail_high)
  nullify(data_ier,data_igps,data_ihgt)
_EXIT_(myname_)
end subroutine rhs_GPSunaliases

subroutine rhs_dealloc()
  use kinds, only: i_kind
  implicit none
  character(len=*),parameter:: myname_=myname//'.dealloc'
  integer(i_kind):: is
_ENTRY_(myname_)
  if(.not.rhs_allocated) call die(myname_,'can not be deallocted')

  rhs_allocated=.false.
  deallocate(rhs_awork)
  deallocate(rhs_bwork)
  deallocate(rhs_aivals)
  deallocate(rhs_stats)
  deallocate(rhs_stats_oz)

  deallocate(rhs_toss_gps)

  if(allocated(aGPSbuffer)) then
    do is=1,size(aGPSbuffer)
      if(aGPSbuffer(is)%alloced) call rhs_GPSdealloc(is)
    enddo
    deallocate(aGPSbuffer)
  endif
_EXIT_(myname_)
end subroutine rhs_dealloc

end module m_rhs
