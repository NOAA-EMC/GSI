!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! NASA/GSFC, Global Modeling and Assimilation Office, 900.3, GEOS/DAS  !
!BOP -------------------------------------------------------------------
!
! !MODULE: m_berror_stats - a module of berror_stats input
!
! !DESCRIPTION:
!
! !INTERFACE:

    module m_berror_stats
      use kinds,only : i_kind
      use constants, only: izero,ione

      implicit none

      private	! except

        ! reconfigurable parameters, via NAMELIST/setup/
      public :: berror_stats	! reconfigurable filename
      public :: berror_nvars	! reconfigurable number of variables

        ! interfaces to file berror_stats.
      public :: berror_get_dims	! get dimensions, jfunc::createj_func()
      public :: berror_read_bal	! get cross-cov.stats., balmod::prebal()
      public :: berror_read_wgt	! get auto-cov.stats., prewgt()

      	! external interfaces relating to internal procedures.
      interface berror_get_dims; module procedure get_dims; end interface
      interface berror_read_bal; module procedure read_bal; end interface
      interface berror_read_wgt; module procedure read_wgt; end interface

! !REVISION HISTORY:
! 	30Jul08	- Jing Guo <guo@gmao.gsfc.nasa.gov>
!		- initial prototype/prolog/code to wrap up all file
!		  "berror_stats" related operations.
!EOP ___________________________________________________________________

  character(len=*),parameter :: myname='m_berror_stats'

  	! Reconfigurable parameters, vai NAMELISt/setup/
  character(len=256),save :: berror_stats = "berror_stats"	! filename
  integer(i_kind)   ,save :: berror_nvars = 6_i_kind            ! variable count.

  integer(i_kind),parameter :: default_unit_ = 22_i_kind
  integer(i_kind),parameter :: ERRCODE=2_i_kind
contains
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! NASA/GSFC, Global Modeling and Assimilation Office, 900.3, GEOS/DAS  !
!BOP -------------------------------------------------------------------
!
! !IROUTINE: get_dims - get dimensions
!
! !DESCRIPTION:
!
! !INTERFACE:

    subroutine get_dims(msig,mlat,unit)
      implicit none
      integer(i_kind),intent(out) :: msig  ! dimension of levels
      integer(i_kind),intent(out) :: mlat  ! dimension of latitudes
      integer(i_kind),optional,intent(in) :: unit  ! logical unit [22]

! !REVISION HISTORY:
! 	30Jul08	- Jing Guo <guo@gmao.gsfc.nasa.gov>
!		- the main body of the code is extracted from jfunc.f90
!EOP ___________________________________________________________________

  character(len=*),parameter :: myname_=myname//'::get_dims'

  integer(i_kind) :: inerr

! Read dimension of stats file
  inerr=default_unit_
  if(present(unit)) inerr = unit
  open(inerr,file=berror_stats,form='unformatted',status='old')
  rewind inerr
  read(inerr) msig,mlat
  close(inerr)
end subroutine get_dims
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! NASA/GSFC, Global Modeling and Assimilation Office, 900.3, GEOS/DAS  !
!BOP -------------------------------------------------------------------
!
! !IROUTINE: read_bal - get cross-corr. coefficients
!
! !DESCRIPTION:
!
! !INTERFACE:

    subroutine read_bal(agvin,bvin,wgvin,mype,unit)
      use kinds,only : r_single
      use gridmod,only : nlat,nlon,nsig
      implicit none
      real(r_single),dimension(nlat,nsig,nsig),intent(out):: agvin
      real(r_single),dimension(nlat,nsig),intent(out):: bvin,wgvin
      integer(i_kind),intent(in) :: mype  ! "my" processor ID
      integer(i_kind),optional,intent(in) :: unit ! an alternative unit

! !REVISION HISTORY:
! 	30Jul08	- Jing Guo <guo@gmao.gsfc.nasa.gov>
!		- the main body of code for input is extracted from
!		  prebal() in balmod.f90.
!EOP ___________________________________________________________________

  character(len=*),parameter :: myname_=myname//'::read_bal'

!   workspaces/variables for data not returned

  real(r_single),dimension(nlat,nsig):: corz,cord,corh,corq,corq2
  real(r_single),dimension(nlat,nsig):: corc,coroz
  real(r_single),dimension(nlat):: corp
  real(r_single),dimension(nlat,nlon):: corsst
  real(r_single),dimension(nlat,nsig*berror_nvars+ione):: hwllin
  real(r_single),dimension(nlat,nlon):: hsst
  real(r_single),dimension(nlat,nsig*berror_nvars):: vscalesin

  integer(i_kind):: nsigstat,nlatstat
  integer(i_kind):: inerr


!   Open background error statistics file
    inerr=default_unit_
    if(present(unit)) inerr=unit
    open(inerr,file=berror_stats,form='unformatted',status='old')

!   Read header.  Ensure that vertical resolution is consistent
!   with that specified via the user namelist

    rewind inerr
    read(inerr) nsigstat,nlatstat
    if(mype==izero) then
      write(6,*) myname_,'(PREBAL):  read error amplitudes ', &
        '"',trim(berror_stats),'".  ', &
        'mype,nsigstat,nlatstat =', &
         mype,nsigstat,nlatstat
      write(6,'(1x,2a,6i5)') myname_, &
        '(PREBAL):  berror_nvars, etc. = ', &
        berror_nvars,nsig,berror_nvars*nsig, &
	size(vscalesin,2),size(hwllin,2)
    end if

    if (nsig/=nsigstat .or. nlat/=nlatstat) then
       write(6,*) myname_,'(PREBAL):  ***ERROR*** resolution of ', &
         '"',trim(berror_stats),'"', &
            'incompatiable with guess'
       write(6,*) myname_,'(PREBAL):  ***ERROR*** nsigstat,nlatstat=', &
         nsigstat,nlatstat
       write(6,*) myname_,'(PREBAL):  ***ERROR*** expects nsig,nlat=', &
         nsig,nlat
       call stop2(ERRCODE)
    end if

!   Read background error file to get balance variables
    rewind inerr
    select case(berror_nvars)
    case(6)
      read(inerr)nsigstat,nlatstat,&
         corz,cord,corh,corq,corq2,coroz,corc,corp,&
         hwllin,vscalesin,&
         agvin,bvin,wgvin,&
         corsst,hsst

    case(5)
      read(inerr)nsigstat,nlatstat,&
         corz,cord,corh,corq,corq2,corc,corp,&
         hwllin,vscalesin,&
         agvin,bvin,wgvin,&
         corsst,hsst

    case default
      write(6,*) myname_,'(PREBAL):  unknown format, []_nvars = ',berror_nvars
      call stop2(ERRCODE)
    endselect
    close(inerr)
    return
end subroutine read_bal
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! NASA/GSFC, Global Modeling and Assimilation Office, 900.3, GEOS/DAS  !
!BOP -------------------------------------------------------------------
!
! !IROUTINE: read_wgt - read auto-corr. coeffs.
!
! !DESCRIPTION:
!
! !INTERFACE:

    subroutine read_wgt(corz,cord,corh,corq,corq2,coroz,corc,corp,&
       hwllin,vscalesin,corsst,hsst,mype,unit)

      use kinds,only : r_single
      use gridmod,only : nlat,nlon,nsig
      implicit none
      real(r_single),dimension(nlat,nsig),intent(out):: corz  ! #1
      real(r_single),dimension(nlat,nsig),intent(out):: cord  ! #2
      real(r_single),dimension(nlat,nsig),intent(out):: corh  ! #3
      real(r_single),dimension(nlat,nsig),intent(out):: corq  ! #4
      real(r_single),dimension(nlat,nsig),intent(out):: corq2 ! #4a
      real(r_single),dimension(nlat,nsig),intent(out):: coroz ! #5
      real(r_single),dimension(nlat,nsig),intent(out):: corc  ! #6
      real(r_single),dimension(nlat     ),intent(out):: corp  ! #7

      real(r_single),dimension(:,:),intent(out):: hwllin
      real(r_single),dimension(:,:),intent(out):: vscalesin

      real(r_single),dimension(nlat,nlon),intent(out):: corsst
      real(r_single),dimension(nlat,nlon),intent(out):: hsst

      integer(i_kind),intent(in) :: mype  ! "my" processor ID
      integer(i_kind),optional,intent(in) :: unit ! an alternative unit

! !REVISION HISTORY:
! 	30Jul08	- Jing Guo <guo@gmao.gsfc.nasa.gov>
!		- the main body of the code for input is extracted from
!		  prewgt() in prewgt.f90.
!EOP ___________________________________________________________________

  character(len=*),parameter :: myname_=myname//'::read_wgt'

!  workspace variables not returned
  real(r_single),dimension(nlat,nsig,nsig):: agvin
  real(r_single),dimension(nlat,nsig) :: wgvin,bvin
 
  integer(i_kind) :: inerr
  integer(i_kind) :: nsigstat,nlatstat
  integer(i_kind) :: msigvars,msigvarsp1

  if(nlat /= size(hwllin,1) .or. nlat /= size(vscalesin,1)) then
    if(nlat /= size(hwllin,1)) then
      write(6,*) myname_, &
        '(PREWGT):  ***ERROR*** size(hwllin,1) = ',size(hwllin,1)
      write(6,*) myname_, &
        '(PREWGT):  ***ERROR*** while expecting ',nlat
    endif
    if(nlat /= size(vscalesin,1)) then
      write(6,*) myname_, &
        '(PREWGT):  ***ERROR*** size(vscalesin,1) = ',size(vscalesin,1)
      write(6,*) myname_, &
        '(PREWGT):  ***ERROR*** while expecting ',nlat
    endif
    call stop2(ERRCODE)
  endif

  msigvarsp1=size(hwllin,2)
  msigvars  =size(vscalesin,2)
  if(msigvarsp1 /= nsig*6+ione .or. msigvars /= nsig*6) then
    if(msigvarsp1/=nsig*6+ione) then
      write(6,*) myname_, &
        '(PREWGT):  ***ERROR*** size(hwllin,2) = ',msigvarsp1
      write(6,*) myname_, &
        '(PREWGT):  ***ERROR*** while expecting ',nsig*6+ione
    endif
    if(msigvars/=nsig*6) then
      write(6,*) myname_, &
        '(PREWGT):  ***ERROR*** size(vscalesin,2) = ',msigvars
      write(6,*) myname_, &
        '(PREWGT):  ***ERROR*** while expecting ',nsig*6
    endif
    call stop2(ERRCODE)
  endif

  inerr=default_unit_
  if(present(unit)) inerr=unit

! Open background error statistics file
  open(inerr,file=berror_stats,form='unformatted',status='old')

! Read header.  Ensure that vertical resolution is consistent
! with that specified via the user namelist

  rewind inerr
  read(inerr)nsigstat,nlatstat
! write(6,*) 'nsigstat,nlatstat',nsigstat,nlatstat
  if(mype==izero) then
    write(6,*) myname_,'(PREWGT):  read error amplitudes ', &
      '"',trim(berror_stats),'".  ', &
      'mype,nsigstat,nlatstat =', &
       mype,nsigstat,nlatstat
    write(6,'(1x,2a,6i5)') myname_, &
      '(PREWGT):  berror_nvars,etc. = ', &
      berror_nvars,nsig,berror_nvars*nsig, &
      size(vscalesin,2),size(hwllin,2)
  end if

  if (nsig/=nsigstat .or. nlat/=nlatstat) then
     write(6,*) myname_,'(PREWGT):  ***ERROR*** resolution of ', &
       '"',trim(berror_stats),'"', &
          'incompatiable with guess'
     write(6,*) myname_,'(PREWGT):  ***ERROR*** nsigstat,nlatstat=', &
       nsigstat,nlatstat
     write(6,*) myname_,'(PREWGT):  ***ERROR*** expects nsig,nlat=', &
       nsig,nlat
     call stop2(ERRCODE)
  end if

! Read amplitudes
  rewind inerr
  select case(berror_nvars)

  case(6)	! stats. for 6 variables including ozone
    read(inerr)nsigstat,nlatstat,&
       corz,cord,corh,corq,corq2,coroz,corc,corp,&
       hwllin,vscalesin,&
       agvin,bvin,wgvin,&
       corsst,hsst

  case(5)	! stats. for 5 variabls not including ozone
!       corz,cord,corh,corq,corq2,coroz,corc,corp,&
!       hwlz,hwld,hwlh,hwlq,      hwloz,hwlc,hwlp,&
!       vscz,vscd,vsch,vscq,      vscoz,vscc,     &

    read(inerr)nsigstat,nlatstat,&
       corz,cord,corh,corq,corq2,corc,corp,&
       hwllin   (:,0*nsig+ione:4*nsig),&
       hwllin   (:,5*nsig+ione:6*nsig),&
       hwllin   (:,6*nsig+ione       ),&
       vscalesin(:,0*nsig+ione:4*nsig),&
       vscalesin(:,5*nsig+ione:6*nsig),&
       agvin,bvin,wgvin,&
       corsst,hsst

	! set ozone related data.
    call setcoroz_(coroz,mype)
    call sethwlloz_   (hwllin   (:,4*nsig+ione:5*nsig),mype)
    call setvscalesoz_(vscalesin(:,4*nsig+ione:5*nsig))

  case default
    write(6,*) myname_,'(PREBAL):  unknown format, []_nvars = ',berror_nvars
    call stop2(ERRCODE)
  endselect
  close(inerr)
  return
end subroutine read_wgt

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! NASA/GSFC, Global Modeling and Assimilation Office, 900.3, GEOS/DAS  !
!BOP -------------------------------------------------------------------
!
! !IROUTINE: setcoroz_ - a modeled corr.coeffs. of ozone
!
! !DESCRIPTION:
!
! !INTERFACE:

    subroutine setcoroz_(coroz,mype)
      use kinds,    only: r_single,r_kind
      use constants,only: zero,rozcon,one
      use mpimod,   only: npe,mpi_rtype,mpi_sum,mpi_comm_world

      use gridmod,  only: nlat,nsig
      use gridmod,  only: lon1,lat1

      use guess_grids,only: ntguessig
      use guess_grids,only: ges_oz   ! ozone fields
      use guess_grids,only: ges_prsi ! interface pressures (kPa)

      implicit none
      real(r_single),dimension(:,:),intent(out) :: coroz ! of ozone
      integer(i_kind),intent(in) :: mype     ! ID of this processor

! !REVISION HISTORY:
! 	31Jul08	- Jing Guo <guo@gmao.gsfc.nasa.gov>
!		- adopted from PREWGT of previous version
!EOP ___________________________________________________________________

  character(len=*),parameter :: myname_=myname//'::setcoroz_'
  real(r_kind),parameter:: r25 = one/25.0_r_kind

!! -- workspace and working variables

    real(r_kind),dimension(nsig+ione,npe) :: work_oz,work_oz1
    real(r_kind),dimension(nsig) :: ozmz
    real(r_kind) :: asum,bsum

    integer(i_kind) :: mlat,msig
    integer(i_kind) :: i,j,k,n,mm1
    integer(i_kind) :: ierror

!! -- synity check
    if(mype==izero) then
      write(6,*) myname_,'(PREWGT): mype = ',mype
    endif

    mlat=size(coroz,1)
    msig=size(coroz,2)
    if(mlat/=nlat .or. msig/=nsig) then
      write(6,*) myname_,'(PREWGT): shape mismatching on PE ',mype
      write(6,*) myname_,'(PREWGT): shape(coroz) = ',shape(coroz)
      write(6,*) myname_,'(PREWGT): while expecting nlat = ',nlat
      write(6,*) myname_,'(PREWGT): while expecting nsig = ',nsig
      call stop2(ERRCODE)
    endif

!! -- The first part is taken from read_guess().

! Calculate global means for ozone
! Calculate sums for ozone to estimate variance.
  mm1=mype+ione
  work_oz = zero
  do k = 1,nsig
     do j = 2,lon1+ione
        do i = 2,lat1+ione
           work_oz(k,mm1) = work_oz(k,mm1) + ges_oz(i,j,k,ntguessig)* &
                rozcon*(ges_prsi(i,j,k,ntguessig)-ges_prsi(i,j,k+ione,ntguessig))
        end do
     end do
  end do
  work_oz(nsig+ione,mm1)=float(lon1*lat1)

  call mpi_allreduce(work_oz,work_oz1,(nsig+ione)*npe,mpi_rtype,mpi_sum,&
       mpi_comm_world,ierror)
    if(ierror/=izero) then
      write(6,*) myname_,'(PREWGT): MPI_allreduce() error on PE ',mype
      call stop2(ierror)
    endif

!! -- All it does above, through mm1 plus mpi_allreduce() to work_oz1[],
!! seems no more than a mpi_allgatherv() to me.  The "reduce" part is
!! actually done below ...

  bsum=zero
  do n=1,npe
    bsum=bsum+work_oz1(nsig+ione,n)
  end do
  do k=1,nsig
     ozmz(k)=zero
     asum=zero
     do n=1,npe
       asum=asum+work_oz1(k,n)
     end do
     if (bsum>zero) ozmz(k)=asum/bsum
  enddo

!! -- now this part is taken from prewgt().

!   load variances onto subdomains
  do k=1,nsig
    coroz(:,k) = max(ozmz(k),0.0002_r_kind)*r25
  enddo

end subroutine setcoroz_
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! NASA/GSFC, Global Modeling and Assimilation Office, 900.3, GEOS/DAS  !
!BOP -------------------------------------------------------------------
!
! !IROUTINE: sethwlloz_ - a modeled hwll of ozone
!
! !DESCRIPTION:
!
! !INTERFACE:

    subroutine sethwlloz_(hwlloz,mype)
      use kinds,   only: r_single,r_kind
      use mpimod,  only: levs_id
      use gridmod, only: nnnn1o,nsig,nlon
      use constants,only: two,three,pi,rearth_equator
      implicit none
      real(r_single),dimension(:,:),intent(out) :: hwlloz
      integer(i_kind),intent(in) :: mype ! ID of this processor

! !REVISION HISTORY:
! 	31Jul08	- Jing Guo <guo@gmao.gsfc.nasa.gov>
!		- initial prototype/prolog/code
!EOP ___________________________________________________________________

  character(len=*),parameter :: myname_=myname//'::sethwlloz_'

  real(r_kind),parameter :: r400=400._r_kind
  real(r_kind),parameter :: r800=800._r_kind
  real(r_kind),parameter :: r40000=40000._r_kind

  integer(i_kind) :: k,k1
  real(r_kind) :: fact
  real(r_kind) :: s2u
    
    if(mype==izero) then
      write(6,*) myname_,'(PREWGT): mype = ',mype
    endif

    s2u=(two*pi*rearth_equator)/nlon
    do k=1,nnnn1o
      k1=levs_id(k)
      if(k1>izero) then
        if(k1<=nsig*3/4)then
        ! fact=1./hwl
          fact=r40000/(r400*nlon)
        else
          fact=r40000/(nlon*(r800-r400*(nsig-k1)/(nsig-nsig*3/4)))
        endif
        fact=fact*three
        hwlloz(:,k1)=s2u/fact
      endif
    enddo

end subroutine sethwlloz_
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! NASA/GSFC, Global Modeling and Assimilation Office, 900.3, GEOS/DAS  !
!BOP -------------------------------------------------------------------
!
! !IROUTINE: setvscalesoz_ - a modeled vscales for ozone
!
! !DESCRIPTION:
!
! !INTERFACE:

    subroutine setvscalesoz_(vscalesoz)
      use kinds,only: r_single,r_kind
      implicit none
      real(r_single),dimension(:,:),intent(out) :: vscalesoz

! !REVISION HISTORY:
! 	31Jul08	- Jing Guo <guo@gmao.gsfc.nasa.gov>
!		- initial prototype/prolog/code
!EOP ___________________________________________________________________

  character(len=*),parameter :: myname_=myname//'::setvscalesoz_'
  real(r_kind),parameter:: eight_tenths = 0.8_r_kind

  	! a fixed value is used.
  vscalesoz(:,:)=eight_tenths

end subroutine setvscalesoz_
end module m_berror_stats
