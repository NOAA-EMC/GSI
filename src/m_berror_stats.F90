module m_berror_stats
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:	 module m_berror_stats
!   prgmmr:	 j guo <jguo@nasa.gov>
!      org:	 NASA/GSFC, Global Modeling and Assimilation Office, 900.3
!     date:	 2010-03-24
!
! abstract:  a module of berror_stats input
!
! program history log:
!   2010-03-24  j guo   - added this document block
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

! module interface:

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! NASA/GSFC, Global Modeling and Assimilation Office, 900.3, GEOS/DAS  !
!BOP -------------------------------------------------------------------
!
! !MODULE: m_berror_stats - a module of berror_stats input
!
! !DESCRIPTION:
!
! !INTERFACE:

      use kinds,only : i_kind
      use constants, only: one

      implicit none

      private	! except

        ! reconfigurable parameters, via NAMELIST/setup/
      public :: berror_stats	! reconfigurable filename

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
!       25Feb10 - Zhu
!               - made changes for generalizing control variables
!               - remove berror_nvars
!EOP ___________________________________________________________________

  character(len=*),parameter :: myname='m_berror_stats'

  	! Reconfigurable parameters, vai NAMELISt/setup/
  character(len=256),save :: berror_stats = "berror_stats"	! filename

  integer(i_kind),parameter :: default_unit_ = 22
  integer(i_kind),parameter :: ERRCODE=2
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

      integer(i_kind)         ,intent(  out) :: msig  ! dimension of levels
      integer(i_kind)         ,intent(  out) :: mlat  ! dimension of latitudes
      integer(i_kind),optional,intent(in   ) :: unit  ! logical unit [22]

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

      real(r_single),dimension(nlat,nsig,nsig),intent(  out) :: agvin
      real(r_single),dimension(nlat,nsig)     ,intent(  out) :: bvin,wgvin
      integer(i_kind)                         ,intent(in   ) :: mype  ! "my" processor ID
      integer(i_kind),optional                ,intent(in   ) :: unit ! an alternative unit

! !REVISION HISTORY:
! 	30Jul08	- Jing Guo <guo@gmao.gsfc.nasa.gov>
!		- the main body of code for input is extracted from
!		  prebal() in balmod.f90.
!       25Feb10 - Zhu 
!               - change the structure of background error file
!               - read in agvin,wgvin,bvin only
!EOP ___________________________________________________________________

  character(len=*),parameter :: myname_=myname//'::read_bal'

!   workspaces/variables for data not returned

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

    if(mype==0) then
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

       write(6,*) myname_,'(PREBAL):  get balance variables', &
         '"',trim(berror_stats),'".  ', &
         'mype,nsigstat,nlatstat =', &
          mype,nsigstat,nlatstat
    end if

!   Read background error file to get balance variables
    read(inerr) agvin,bvin,wgvin
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

    subroutine read_wgt(corz,corp,hwll,hwllp,vz,corsst,hsst,mype,unit)

      use kinds,only : r_single,r_kind
      use gridmod,only : nlat,nlon,nsig
      use control_vectors,only: nrf,nrf2,nrf3,nrf_var,nrf2_loc,nrf3_loc,nrf3_oz
      use jfunc,only: varq,qoption

      implicit none

      real(r_single),dimension(nlat,nsig,nrf3),intent(out) :: corz 
      real(r_single),dimension(nlat,nrf2),intent(out) :: corp  

      real(r_single),dimension(nlat,nsig,nrf3),intent(out) :: hwll
      real(r_single),dimension(nlat,nrf2)     ,intent(out) :: hwllp
      real(r_single),dimension(nsig,nlat,nrf3),intent(out) :: vz

      real(r_single),dimension(nlat,nlon),intent(out) :: corsst
      real(r_single),dimension(nlat,nlon),intent(out) :: hsst

      integer(i_kind)                    ,intent(in   ) :: mype  ! "my" processor ID
      integer(i_kind),optional           ,intent(in   ) :: unit ! an alternative unit

! !REVISION HISTORY:
! 	30Jul08	- Jing Guo <guo@gmao.gsfc.nasa.gov>
!		- the main body of the code for input is extracted from
!		  prewgt() in prewgt.f90.
!       25Feb10 - Zhu 
!               - change the structure of background error file
!               - make changes for generalizing control variables
!               - move varq here from prewgt
!EOP ___________________________________________________________________

  character(len=*),parameter :: myname_=myname//'::read_wgt'

!  workspace variables not returned
  real(r_single),dimension(nlat,nsig,nsig):: agvin
  real(r_single),dimension(nlat,nsig) :: wgvin,bvin
 
  integer(i_kind) :: i,n,k
  integer(i_kind) :: inerr,istat
  integer(i_kind) :: nsigstat,nlatstat
  integer(i_kind) :: loc,nn,isig
  real(r_kind) :: corq2x
  character*5 var
  logical,dimension(nrf):: nrf_err

  real(r_single),allocatable,dimension(:,:):: hwllin
  real(r_single),allocatable,dimension(:,:):: corzin
  real(r_single),allocatable,dimension(:,:):: corq2
  real(r_single),allocatable,dimension(:,:):: vscalesin

! Open background error statistics file
  inerr=default_unit_
  if(present(unit)) inerr=unit
  open(inerr,file=berror_stats,form='unformatted',status='old')

! Read header.  Ensure that vertical resolution is consistent
! with that specified via the user namelist

  rewind inerr
  read(inerr)nsigstat,nlatstat
  if(mype==0) then
     if(nsigstat/=nsig .or. nlatstat/=nlat) then
        write(6,*)'PREBAL: **ERROR** resolution of berror_stats incompatiable with GSI'
        write(6,*)'PREBAL:  berror nsigstat,nlatstat=', nsigstat,nlatstat, &
             ' -vs- GSI nsig,nlat=',nsig,nlat
        call stop2(101)
     end if

     write(6,*) myname_,'(PREWGT):  read error amplitudes ', &
       '"',trim(berror_stats),'".  ', &
       'mype,nsigstat,nlatstat =', &
        mype,nsigstat,nlatstat
  end if
  read(inerr) agvin,bvin,wgvin

! Read amplitudes
  nrf_err=.false.
  read: do
     read(inerr,iostat=istat) var, isig
     if (istat/=0) exit

     allocate ( corzin(nlat,isig) )
     if (var=='q') allocate ( corq2(nlat,isig) )
     allocate ( hwllin(nlat,isig) )
     if (isig>1) allocate ( vscalesin(nlat,isig) )

     if (var/='sst') then
        if (var=='q' .or. var=='Q') then
           read(inerr) corzin,corq2
        else
           read(inerr) corzin
        end if
        read(inerr) hwllin
        if (isig>1) read(inerr) vscalesin
     else
        read(inerr) corsst
        read(inerr) hsst
     end if

!    load the variances
     do n=1,nrf
        if (var==nrf_var(n)) then
           nrf_err(n)=.true.
           loc=n
           exit
        end if
     end do

     if (isig>1) then
        do n=1,nrf3
           if (nrf3_loc(n)==loc) then
              do k=1,isig
                 do i=1,nlat
                    corz(i,k,n)=corzin(i,k)
                    vz(k,i,n)=vscalesin(i,k)
                 end do
              end do
              if (var=='q' .and. qoption==2)then
                 do k=1,isig
                    do i=1,nlat
                       corq2x=corq2(i,k)
                       varq(i,k)=min(max(corq2x,0.0015_r_kind),one)
                    enddo
                 enddo
                 do k=1,isig
                    do i=1,nlat
                       corz(i,k,n)=one
                    end do
                 end do
              end if
              do k=1,isig
                 do i=1,nlat
                    hwll(i,k,n)=hwllin(i,k)
                 end do
              end do
              exit
           end if ! end of nrf3_loc
        end do ! end of nrf3
     end if ! end of isig

     if (isig==1) then
       do n=1,nrf2
          if (nrf2_loc(n)==loc .and. var/='sst') then
             do i=1,nlat
                corp(i,n)=corzin(i,1)
                hwllp(i,n)=hwllin(i,1)
             end do
             exit
          end if
       end do
     end if

     deallocate(corzin,hwllin)
     if (isig>1) deallocate(vscalesin)
     if (var=='q') deallocate(corq2)
  enddo read 
  close(inerr)

! corz, hwll & vz for undefined variable
  do n=1,nrf3
     loc=nrf3_loc(n)
     if (nrf_err(loc)) cycle
     if (n==nrf3_oz) then
        call setcoroz_(corz(1,1,n),mype)
        call sethwlloz_(hwll(1,1,n),mype)
        call setvscalesoz_(vz(1,1,n))
     end if   
  end do

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

      real(r_single),dimension(nlat,nsig),intent(  out) :: coroz ! of ozone
      integer(i_kind)              ,intent(in   ) :: mype     ! ID of this processor

! !REVISION HISTORY:
! 	31Jul08	- Jing Guo <guo@gmao.gsfc.nasa.gov>
!		- adopted from PREWGT of previous version
!EOP ___________________________________________________________________

  character(len=*),parameter :: myname_=myname//'::setcoroz_'
  real(r_kind),parameter:: r25 = one/25.0_r_kind

!! -- workspace and working variables

    real(r_kind),dimension(nsig+1,npe) :: work_oz,work_oz1
    real(r_kind),dimension(nsig) :: ozmz
    real(r_kind) :: asum,bsum

    integer(i_kind) :: mlat,msig
    integer(i_kind) :: i,j,k,n,mm1
    integer(i_kind) :: ierror

!! -- synity check
    if(mype==0) then
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
  mm1=mype+1
  work_oz = zero
  do k = 1,nsig
     do j = 2,lon1+1
        do i = 2,lat1+1
           work_oz(k,mm1) = work_oz(k,mm1) + ges_oz(i,j,k,ntguessig)* &
                rozcon*(ges_prsi(i,j,k,ntguessig)-ges_prsi(i,j,k+1,ntguessig))
        end do
     end do
  end do
  work_oz(nsig+1,mm1)=float(lon1*lat1)

  call mpi_allreduce(work_oz,work_oz1,(nsig+1)*npe,mpi_rtype,mpi_sum,&
       mpi_comm_world,ierror)
  if(ierror/=0) then
     write(6,*) myname_,'(PREWGT): MPI_allreduce() error on PE ',mype
     call stop2(ierror)
  endif

!! -- All it does above, through mm1 plus mpi_allreduce() to work_oz1[],
!! seems no more than a mpi_allgatherv() to me.  The "reduce" part is
!! actually done below ...

  bsum=zero
  do n=1,npe
     bsum=bsum+work_oz1(nsig+1,n)
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
      use gridmod, only: nnnn1o,nsig,nlon,nlat
      use constants,only: two,three,pi,rearth_equator
      implicit none

      real(r_single),dimension(nlat,nsig),intent(  out) :: hwlloz
      integer(i_kind)              ,intent(in   ) :: mype ! ID of this processor

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
    
  if(mype==0) then
     write(6,*) myname_,'(PREWGT): mype = ',mype
  endif

  s2u=(two*pi*rearth_equator)/nlon
  do k=1,nnnn1o
     k1=levs_id(k)
     if(k1>0) then
     write(6,*) myname_,'(PREWGT): mype = ',mype, k1
        if(k1<=nsig*3/4)then
        !  fact=1./hwl
           fact=r40000/(r400*nlon)
        else
           fact=r40000/(nlon*(r800-r400*(nsig-k1)/(nsig-nsig*3/4)))
        endif
        fact=fact*three
        hwlloz(:,k1)=s2u/fact
     endif
  enddo


  if(mype==0) then
     write(6,*) myname_,'(PREWGT): mype = ',mype, 'finish sethwlloz_'
  endif


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
      use gridmod,only : nlat,nlon,nsig
      use kinds,only: r_single,r_kind
      implicit none

      real(r_single),dimension(nsig,nlat),intent(  out) :: vscalesoz

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
