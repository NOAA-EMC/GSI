
module anisofilter
!$$$   module documentation block
!                .      .    .                                       .
! module:    anisofilter 
! prgmmr: pondeca          org: np23                date: 2006-08-01
!
! abstract:  computes the anisotropic aspect tensor of the background error
!            auto-correlation model. for now, works only for the gsi-regional
!
! program history log:
!   2006-08-01  pondeca
!   2008-06-05  safford - rm unused uses
!
! subroutines included:
!
!   init_anisofilter_reg          - initialize anisotropic background error
!                                   related variables
!   anprewgt_reg                  - main subroutine for computation of the 
!                                   anisotropic aspect tensor of auto-correlation 
!                                   model
!   get2berr_reg                  - compute the anisotropic aspect tensor for the 
!                                   2dvar case of gsi-regional
!   get3berr_reg                  - compute the anisotropic aspect tensor for the 
!                                   3dvar case of gsi-regional
!   read_bckgstats                - read in background error statistics
!   get_background                - compute smoothed versions of the background fields 
!                                   and respective spatial derivatives on filter grid
!                                   for use in auto-correlation model
!   isotropic_scales              - compute isotropic length-scales of 
!                                   auto-correlation model
!   get_theta_corrl_lenghts       - compute function correlation length-scales of   
!                                   Riishojgaard-type anisotropic auto-correlation 
!                                   model based on the background potential temperature
!   smther_one                    - apply hanning smoother to 2-dimensional field
!   
! variable definitions:
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$ end documentation block

! Uses:
  use kinds, only: r_kind,i_kind,r_single,r_double,i_long
  use anberror, only: ids,ide,jds,jde,kds,kde, &
                      ims,ime,jms,jme,kms,kme, &
                      ips,ipe,jps,jpe,kps,kpe, &
                      create_anberror_vars_reg, &
                      nvars,idvar,jdvar,kvar_start,kvar_end, &
                      var_names,clenmax,clenmaxi,smooth_len,filter_all, &
                      triad4,ifilt_ord,npass,normal,binom,grid_ratio, &
                      ngauss,rgauss,an_amp,an_vs,an_flen_u,an_flen_t,an_flen_z

  use fgrid2agrid_mod, only: nlatf,nlonf,fgrid2agrid,agrid2fgrid, & 
                      grid_ratio_lat,grid_ratio_lon

  use gridmod, only: nsig,nsig1o,region_dx,region_dy,nlon,nlat, & 
                     istart,jstart,lat2,lon2,twodvar_regional, & 
                     iglobal,itotsub,lon1,lat1,region_lat,&
                     ltosi,ltosj,ltosi_s,ltosj_s

  use constants, only: izero,zero,half,one,two,rd_over_cp
                       
  use balmod, only: rllat
  use raflib, only: init_raf4,raf_sm4,raf_sm4_ad
  use jfunc, only: varq,qoption
  use guess_grids, only: ges_u,ges_v,ges_prsl,ges_tv,ges_z,ntguessig,& 
                         ges_prslavg,ges_psfcavg,ges_ps,ges_q,geop_hgtl
  use mpimod, only: npe,levs_id,nvar_id,ierror,mpi_real8,mpi_sum, & 
                    mpi_comm_world,mpi_integer4

  implicit none

! Declare passed variables

! Declare local parameters
  logical,parameter:: latdepend=.true.
  logical,parameter:: lreadnorm         = .false.

  real(r_kind),parameter:: zero_25      = 0.25_r_kind
  real(r_kind),parameter:: zero_3       = 0.3_r_kind
  real(r_kind),parameter:: r400000      = 400000.0_r_kind
  real(r_kind),parameter:: r800000      = 800000.0_r_kind
  real(r_kind),parameter:: r25          = 1.0_r_kind/25.0_r_kind
  real(r_kind),parameter:: r015         = 0.15_r_kind

  real(r_kind),parameter:: rvsmall      = 1.e-10_r_kind
  real(r_kind),parameter:: qlth_temp0   = 0.5_r_kind
  real(r_kind),parameter:: qltv_temp0   = 5.0_r_kind
  real(r_kind),parameter:: qlth_wind0   = 3.0_r_kind
  real(r_kind),parameter:: qltv_wind0   = 7.5_r_kind
  real(r_kind),parameter:: qls_rh       = 5.e+05_r_kind

  real(r_kind),parameter:: scalex1      = 1.2_r_kind
  real(r_kind),parameter:: scalex2      = 1.2_r_kind
  real(r_kind),parameter:: scalex3      = 1.2_r_kind
 
  integer(i_kind) igauss
  integer(i_kind) msig,mlat
  integer(i_kind),allocatable,dimension(:):: ks
  integer(i_kind) it
  real(r_kind),allocatable::rfact0h(:),rfact0v(:)
  real(r_kind),allocatable::corz(:,:,:),corp(:),hwll(:,:,:),hwllp(:),vz(:,:,:)
  real(r_kind),allocatable::agvi(:,:,:),bvi(:,:),wgvi(:,:)
  real(r_kind),allocatable,dimension(:,:)::dxf,dyf,rllatf,hfilter
  real(r_single),allocatable,dimension(:,:,:,:)::aspect
  real(r_single),allocatable,dimension(:,:,:)::theta0f,theta0zf,u0f,u0zf,v0f,v0zf,z0f
  real(r_kind),allocatable,dimension(:,:)::asp10f,asp20f,asp30f

  real(r_kind),allocatable,dimension(:,:,:)::psg
  real(r_kind),parameter:: r100=100.0_r_kind

  real(r_kind) f1m,f1p,psfc015
  integer(i_long) ngauss_smooth,npass_smooth,normal_smooth,ifilt_ord_smooth
  real(r_double) rgauss_smooth(1)
  real(r_kind),allocatable:: vzimax(:,:),vzimin(:,:),vziavg(:,:)
  real(r_kind),allocatable:: corzavg(:,:),hwllavg(:,:)
  real(r_kind) corpavg,hwllpavg

  real(r_kind) rltop,rltop_wind,rltop_temp,rltop_q,rltop_psfc,afact
  real(r_kind) qlth,qltv,gmax,r
  real(r_kind) qls,ucomp,vcomp,bfact,dxf0,dyf0
  real(r_single),allocatable,dimension(:,:,:)::tx1_slab,tx2_slab,tx3_slab
  real(r_kind)rk1,fblend
  real(r_kind),allocatable::asp1_max(:,:),asp2_max(:,:),asp3_max(:,:)
  real(r_kind),allocatable::qlth_temp(:)
  real(r_kind),allocatable::qltv_temp(:)
  real(r_kind),allocatable::qlth_wind(:)
  real(r_kind),allocatable::qltv_wind(:)
  real(r_kind),allocatable,dimension(:)::work
  real(r_single),allocatable,dimension(:)::scratch

!-------------------------------------------------------------------------
contains
!-------------------------------------------------------------------------
!    NOAA/NCEP, National Centers for Environmental Prediction GSI        !
!-------------------------------------------------------------------------
subroutine anprewgt_reg(mype)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    anprewgt_reg
! prgmmr: parrish          org: np22                date: 2005-02-08
!
! abstract: setup everything for anisotropic regional background error     
!
! program history log:
!   2005-02-08  parrish      
!   2006-08-01  pondeca - add subroutines get3berr_reg and get2berr_reg 
!                         to treat the 3dvar and 2dvar cases separately.
!                         for now, anisotropic component of the covariance
!                         model expressed only in terms of the background 
!                         potential temperature and wind for the 3dvar case 
!                         and terrain for the 2dvar case. 
!
!   input argument list:
!     mype     - mpi task id
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$ end documentation block

! Declare passed variables
  integer(i_kind),intent(in):: mype

  if (twodvar_regional) then 
     call get2berr_reg(mype)
   else
     call get3berr_reg(mype)
  endif

 end subroutine anprewgt_reg

!=======================================================================
!=======================================================================

subroutine get2berr_reg(mype)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    get2berr_reg
! prgmmr: parrish          org: np22                date: 2005-02-08
!
! abstract: setup everything for anisotropic regional background 
!           error (2dvar case).     
!
! program history log:
!   2005-02-08  parrish      
!   2005-03-28  wu - replace mlath with mlat and setup qoption=2
!   2005-05-24  pondeca - add reconstruction of aspect tensor via
!                         eigenvector decomposition
!   2005-11-29  derber - unify ozone variance calculations
!   2006-02-02  treadon - rename prsl as ges_prsl
!   2006-04-17  treadon - use rlsig from call rdgstat_reg; replace sigl
!                         with ges_prslavg/ges_psfcavg
!   2006-07-01  pondeca - add terrain following covariances.
!                         remove eigenvector decomposition
!   2007-05-30  h.liu - remove ozmz, use factoz
!   2008-06-05  safford - rm unused vars
!
!   input argument list:
!     mype     - mpi task id
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$ end documentation block
  implicit none

! Declare passed variables
  integer(i_kind),intent(in):: mype


! Declare local parameters
  real(r_kind),parameter:: r25          = 1.0_r_kind/25.0_r_kind

! Declare local variables 
  integer(i_kind) i,j,k,l,lp,k1,kvar,ivar,im,ip,jm,jp

  real(r_kind) dl1,dl2,factk,factor,hwll_loc
  real(r_kind) a1,a2,a3,a4,a5,a6,detai
  real(r_kind) biga1,biga2,biga3,biga4,biga5,biga6
  real(r_kind) fx2,fx1,fx3,dxi,dyi
  real(r_kind) asp1,asp2,asp3,factoz

  real(r_kind),allocatable,dimension(:,:)::bckgvar,bckgvar0f
  real(r_single),allocatable,dimension(:,:)::bckgvar4
  real(r_single),allocatable,dimension(:,:)::region_dx4,region_dy4,psg4
  character*10 chvarname(10)

  data chvarname/'psi', 'chi', 'lnps', 't', 'pseudorh', 'oz', 'sst', &
                 'lst', 'ist', 'qw'/

  call read_bckgstats(mype)
  call get_background(mype)

!-----optionally, remove latitudinal dependence from statistics
 l=int(rllatf(nlatf/2,nlonf/2))
  do j=0,mlat+1
   if (j>=1 .and. j<= mlat) corz(j,:,:)=corz(l,:,:)
   if (j>=1 .and. j<= mlat) corp(j)=corp(l)
   hwll(j,:,:)=hwll(l,:,:)
   hwllp(j)=hwllp(l)
  end do

!-----define the anisotropic aspect tensor-----------------------------
!-------------------------------------------------------------------------------------
! Set up scales

! This first loop for nsig1o will be if we aren't dealing with
! surface pressure, skin temperature, or ozone

  rltop_wind=300._r_kind
  rltop_temp=100._r_kind
  rltop_q=100._r_kind
  rltop_psfc=150._r_kind

  aspect(:,:,:,:)=zero

  do k=1,nsig1o

    k1=levs_id(k)
    if (k1==izero) cycle
    call isotropic_scales(asp10f,asp20f,asp30f,k,mype)

    do j=1,nlonf
       do i=1,nlatf

          asp1=asp10f(i,j)
          asp2=asp20f(i,j)
          asp3=asp30f(i,j)
           
          if(i.eq.nlatf/2.and.j.eq.nlonf/2) then
             write(6,'("at domain center, var,k1,asp1,asp2,asp3 =",2i4,3f11.3)') &
                    nvar_id(k),k1,asp1,asp2,asp3
             write(6,'("at domain center, var,k1,hwll_loc,dxf,dyf =",2i4,3f11.3)') &
                    nvar_id(k),k1,hwll_loc,dxf(i,j),dyf(i,j)
          end if

          jp=min(nlonf,j+1) ; jm=max(1,j-1)
          dxi=one/(jp-jm)
           
          ip=min(nlatf,i+1) ; im=max(1,i-1)
          dyi=one/(ip-im)
           
          fx1= dyi*(z0f(ip,j,k)-z0f(im,j,k))
          fx2= dxi*(z0f(i,jp,k)-z0f(i,jm,k))
          fx3= zero

          rltop=rltop_wind 
          afact=zero

          if (nvar_id(k)==1) rltop=rltop_wind 
          if (nvar_id(k)==2) rltop=rltop_wind  
          if (nvar_id(k)==3) rltop=rltop_psfc
          if (nvar_id(k)==4) rltop=rltop_temp
          if (nvar_id(k)==5) rltop=rltop_q
          if (nvar_id(k) <= 5) afact=one  !(use "zero" for isotropic computations) 

          if (afact==one) then
             asp1=scalex1*asp1
             asp2=scalex2*asp2
          endif


          aspect(1,i,j,k) =   1./asp1**2 + afact*fx1*fx1/rltop**2   ! 1st (y) direction    x1*x1
          aspect(2,i,j,k) =   1./asp2**2 + afact*fx2*fx2/rltop**2   ! 2nd (x) direction    x2*x2
          aspect(3,i,j,k) =   1./asp3**2                            ! 3rd (z) direction    x3*x3
          aspect(4,i,j,k) =   afact*fx3*fx2/rltop**2                !  x3*x2
          aspect(5,i,j,k) =   afact*fx3*fx1/rltop**2                !  x3*x1
          aspect(6,i,j,k) =   afact*fx2*fx1/rltop**2                !  x2*x1
          aspect(7,i,j,k)=    zero

       end do
    end do
  end do

!  Invert to get true aspect tensor

  do k=1,nsig1o
    if(levs_id(k).eq.0 ) cycle
    do j=1,nlonf
      do i=1,nlatf
        a1=aspect(1,i,j,k)
        a2=aspect(2,i,j,k)
        a3=aspect(3,i,j,k)
        a4=aspect(4,i,j,k)
        a5=aspect(5,i,j,k)
        a6=aspect(6,i,j,k)
        biga1=a2*a3-a4*a4
        biga2=a1*a3-a5*a5
        biga3=a1*a2-a6*a6
        biga4=a5*a6-a1*a4
        biga5=a4*a6-a2*a5
        biga6=a4*a5-a3*a6
        detai=one/(a1*biga1+a6*biga6+a5*biga5)
        aspect(1,i,j,k)=biga1*detai
        aspect(2,i,j,k)=biga2*detai
        aspect(3,i,j,k)=biga3*detai
        aspect(4,i,j,k)=biga4*detai
        aspect(5,i,j,k)=biga5*detai
        aspect(6,i,j,k)=biga6*detai
       end do
     end do
   end do


   if(mype.eq.0) write(6,*)'rltop_wind,rltop_temp,rltop_q,rltop_psfc=',&
                                     rltop_wind,rltop_temp,rltop_q,rltop_psfc

   if(mype.eq.0) write(6,*)' in get2berr_reg, nlat,nlon,nlatf,nlonf=',nlat,nlon,nlatf,nlonf

   if(mype.eq.0) write(6,*)' in get2berr_reg, ids,ide=',ids,ide
   if(mype.eq.0) write(6,*)' in get2berr_reg, jds,jde=',jds,jde
   write(6,*)'in get2berr_reg, mype,ips,ipe,jps,jpe,kps,kpe=',mype,ips,ipe,jps,jpe,kps,kpe

  if(lreadnorm) normal=0

  call init_raf4(aspect,triad4,ngauss,rgauss,npass,normal,binom,ifilt_ord,filter_all, &
              nvars,idvar,kvar_start,kvar_end,var_names, &
              ids, ide, jds, jde, kds, kde, &         ! domain indices
              ips, ipe, jps, jpe, kps, kpe, &         ! patch indices
              ims, ime, jms, jme, kms, kme, &         ! memory indices
              mype, npe)

  do k=kps,kpe
    ivar=idvar(k)
    open (94,file='fltnorm.dat_'//trim(chvarname(ivar)),form='unformatted')
    do j=jps,jpe
      do i=ips,ipe
        do igauss=1,ngauss
           if(lreadnorm) then
             read(94)  filter_all(1)%amp(igauss,i,j,k)
            else
             write(94) filter_all(1)%amp(igauss,i,j,k)
           endif
        end do
      end do
    end do
  end do
  close(94)

  allocate(bckgvar0f(ips:ipe,jps:jpe))
  bckgvar0f=zero

!  filter normalized to unit amplitude.  now adjust amplitude to correspond
!  to desired error amplitude.
!
!                    (corz, corp contain sqrt(error variance) from background error file)
!                    (an_amp are input tuneable error amplitude parameters)

  factoz = 0.0002_r_kind*r25
  do k=kps,kpe
    ivar=idvar(k)
    kvar=k-kvar_start(ivar)+1
    do j=jps,jpe
      do i=ips,ipe
             l=int(rllatf(i,j))
             lp=l+1
             dl2=rllatf(i,j)-float(l)
             dl1=one-dl2
             if(ivar.eq. 1) factk=dl1*corz(l,kvar,1)+dl2*corz(lp,kvar,1)  ! streamfunction
             if(ivar.eq. 2) factk=dl1*corz(l,kvar,2)+dl2*corz(lp,kvar,2)  ! velocity potential
             if(ivar.eq. 3) factk=dl1*corp(l)+dl2*corp(lp)                ! log(ps)
             if(ivar.eq. 4) factk=dl1*corz(l,kvar,3)+dl2*corz(lp,kvar,3)  ! temperature
             if(ivar.eq. 5) factk=dl1*corz(l,kvar,4)+dl2*corz(lp,kvar,4)  ! specific humidity
             if(ivar.eq. 6) factk=factoz                                  ! ozone
             if(ivar.eq. 7) factk=zero_3                                  ! sea surface temperature
             if(ivar.eq. 8) factk=one                                     ! land surface temperature
             if(ivar.eq. 9) factk=one                                     ! ice surface temperature
             if(ivar.eq.10) factk=dl1*corz(l,kvar,4)+dl2*corz(lp,kvar,4)  ! cloud condensate mixing ratio

!               Note: ideally, correct an_amp should come from
!               namelist anbkgerr

        if (ivar == 1)  an_amp(:,ivar)=0.35_r_double
        if (ivar == 2)  an_amp(:,ivar)=0.35_r_double
        if (ivar == 3)  an_amp(:,ivar)=0.70_r_double
        if (ivar == 4)  an_amp(:,ivar)=1.00_r_double
        if (ivar == 5)  an_amp(:,ivar)=1.00_r_double
        if (ivar == 6)  an_amp(:,ivar)=0.80_r_double
        if (ivar == 7)  an_amp(:,ivar)=0.60_r_double
        if (ivar == 8)  an_amp(:,ivar)=0.60_r_double
        if (ivar == 9)  an_amp(:,ivar)=0.60_r_double
        if (ivar == 10) an_amp(:,ivar)=0.60_r_double

        do igauss=1,ngauss
          factor=factk*an_amp(igauss,ivar)
          filter_all(1)%amp(igauss,i,j,k)=factor*filter_all(1)%amp(igauss,i,j,k)
          bckgvar0f(i,j)=factor
        end do
      end do
    end do
  end do


! write out a few bckg fields and the error variances. For layer constant 
! statistics, these are good for post-processing purposes on the real 
! model grid. Interface between filter grid and model domain needed 
! for non-constant statistics!

  allocate(bckgvar(nlat,nlon))
  allocate(bckgvar4(nlat,nlon))

  call fgrid2agrid(bckgvar0f,bckgvar)
  bckgvar4=bckgvar

  do k=kps,kpe
    ivar=idvar(k)
    open (94,file='bckgvar.dat_'//trim(chvarname(ivar)),form='unformatted')
    write(94) bckgvar4
    close(94)
  enddo

  allocate(region_dy4(nlat,nlon),region_dx4(nlat,nlon))
  allocate(psg4(nlat,nlon))

  if (mype.eq.0) then   
   region_dx4=region_dx
   region_dy4=region_dy
   open (94,file='bckg_dxdy.dat',form='unformatted')
   write(94) region_dx4,region_dy4
   close(94)

    psg4(:,:)=psg(:,:,1)
    open (94,file='bckg_psfc.dat',form='unformatted')
    write(94) psg4
    close(94)
  end if


  deallocate(bckgvar0f)
  deallocate(bckgvar4)
  deallocate(bckgvar)
  deallocate(region_dy4,region_dx4)
  deallocate(corz,corp,hwll,hwllp,vz,aspect)
  deallocate(dxf,dyf,rllatf,theta0f,theta0zf)
  deallocate(u0f,u0zf,v0f,v0zf,z0f)

end subroutine get2berr_reg

!=======================================================================
!=======================================================================

subroutine get3berr_reg(mype)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    get3berr_reg
! prgmmr: parrish          org: np22                date: 2005-02-08
!
! abstract: setup everything for anisotropic regional background 
!           error (3dvar case)     
!
! program history log:
!   2005-02-08  parrish      
!   2005-03-28  wu - replace mlath with mlat and setup qoption=2
!   2005-05-24  pondeca - add reconstruction of aspect tensor via
!                         eigenvector decomposition
!   2005-11-29  derber - unify ozone variance calculations
!   2006-02-02  treadon - rename prsl as ges_prsl
!   2006-04-17  treadon - use rlsig from call rdgstat_reg; replace sigl
!                         with ges_prslavg/ges_psfcavg
!   2006-08-01  pondeca - build background potential temperature and 
!                         wind following covariances.
!                         remove eigenvector decomposition
!   2007-05-30  h.liu - remove ozmz, use factoz
!   2008-06-05  safford - rm unused vars
!
!   input argument list:
!     mype     - mpi task id
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$ end documentation block
  implicit none

! Declare passed variables
  integer(i_kind),intent(in):: mype

! Declare local parameters
  real(r_kind),parameter:: r25          = 1.0_r_kind/25.0_r_kind

! Declare local variables
  integer(i_kind) i,j,k,l,lp,k1,kvar,ivar

  real(r_kind) dl1,dl2,factk,factor,hwll_loc
  real(r_kind) a1,a2,a3,a4,a5,a6,detai
  real(r_kind) biga1,biga2,biga3,biga4,biga5,biga6
  real(r_kind) fx2,fx1,fx3
  real(r_kind) asp1,asp2,asp3,factoz
 
  character*10 chvarname(10)
  character*4  clun

  data chvarname/'psi', 'chi', 'lnps', 't', 'pseudorh', 'oz', 'sst', &
                 'lst', 'ist', 'qw'/

  call init_anisofilter_reg
  call read_bckgstats(mype)
  call get_background(mype)

!-----------------------------------------------------------
!-----define the anisotropic aspect tensor------------------
!-----------------------------------------------------------
! Set up scales

! This first loop for nsig1o will be if we aren't dealing with
! surface pressure, skin temperature, or ozone

  call get_theta_corrl_lenghts(mype)

  aspect(:,:,:,:)=zero

  do k=1,nsig1o

    k1=levs_id(k)
    if (k1==izero) cycle
    call isotropic_scales(asp10f,asp20f,asp30f,k,mype)

    do j=1,nlonf
       do i=1,nlatf

          asp1=asp10f(i,j)
          asp2=asp20f(i,j)
          asp3=asp30f(i,j)

          if(i.eq.nlatf/2.and.j.eq.nlonf/2) then
             write(6,*)'("at domain center, var,k1,asp1,asp2,asp3 =",2i4,3f11.3)', & 
                    nvar_id(k),k1,asp1,asp2,asp3
             write(6,*)'("at domain center, var,k1,hwll_loc,dxf,dyf =",2i4,3f11.3)', & 
                    nvar_id(k),k1,hwll_loc,dxf(i,j),dyf(i,j)
             write(6,*)'("at domain center, var,k1,asp1_max,asp2_max,asp3_max =",2i4,3f11.3)', & 
                    nvar_id(k),k1,asp1_max(nvar_id(k),k1),asp2_max(nvar_id(k),k1), & 
                    asp3_max(nvar_id(k),k1)
          end if

          afact=zero
          fx1=one
          fx2=one
          fx3=one

          qlth=one
          qltv=one
          if (nvar_id(k)==1) then ; qlth=qlth_wind(k1) ; qltv=qltv_wind(k1) ; endif
          if (nvar_id(k)==2) then ; qlth=qlth_wind(k1) ; qltv=qltv_wind(k1) ; endif
          if (nvar_id(k)==4) then ; qlth=qlth_temp(k1) ; qltv=qltv_temp(k1) ; endif

          if (nvar_id(k)==1 .or. nvar_id(k)==2 .or. nvar_id(k)==4) then
             afact=one  
             asp1=scalex1*asp1
             asp2=scalex2*asp2       
             asp3=scalex3*asp3       
             fx1=tx1_slab(i,j,k)
             fx2=tx2_slab(i,j,k)
             fx3=tx3_slab(i,j,k)
          endif

          qls=one
          bfact=zero
          ucomp=u0f(i,j,k)
          vcomp=v0f(i,j,k)
          dyf0=dyf(nlatf/2,nlonf/2)
          dxf0=dxf(nlatf/2,nlonf/2)

          if (nvar_id(k)==5)  then
!           bfact=one
            qls=qls_rh
            asp1=scalex1*asp1
            asp2=scalex2*asp2
            asp3=scalex3*asp3
           endif

          rk1=float(k1-44_i_kind)
          fblend=half*(one-tanh(rk1))! one

        if (nvar_id(k) /= 5) then
          aspect(1,i,j,k) = 1./asp1**2 + afact*fblend*fx1*fx1/qlth**2  ! 1st (y) direction    x1*x1
          aspect(2,i,j,k) = 1./asp2**2 + afact*fblend*fx2*fx2/qlth**2  ! 2nd (x) direction    x2*x2
          aspect(3,i,j,k) = 1./asp3**2 + afact*fblend*fx3*fx3/qltv**2  ! 3rd (z) direction    x3*x3
          aspect(4,i,j,k) = afact*fblend*fx3*fx2/qlth/qltv             !  x3*x2
          aspect(5,i,j,k) = afact*fblend*fx3*fx1/qlth/qltv             !  x3*x1
          aspect(6,i,j,k) = afact*fblend*fx2*fx1/qlth**2               !  x2*x1
          aspect(7,i,j,k)=  zero
        elseif (nvar_id(k)==5) then
          aspect(1,i,j,k) = 1./asp1**2 +  bfact*fblend*ucomp*ucomp*dyf0*dyf0/qls**2
          aspect(2,i,j,k) = 1./asp2**2 +  bfact*fblend*vcomp*vcomp*dxf0*dxf0/qls**2
          aspect(3,i,j,k) = 1./asp3**2
          aspect(4,i,j,k) = zero
          aspect(5,i,j,k) = zero
          aspect(6,i,j,k) = - bfact*fblend*ucomp*vcomp*dyf0*dxf0/qls**2
          aspect(7,i,j,k)=  zero
         endif

       end do
    end do
  end do

!  Invert to get true aspect tensor

  do k=1,nsig1o
    if(levs_id(k).eq.0 ) cycle
    do j=1,nlonf
      do i=1,nlatf
        a1=aspect(1,i,j,k)
        a2=aspect(2,i,j,k)
        a3=aspect(3,i,j,k)
        a4=aspect(4,i,j,k)
        a5=aspect(5,i,j,k)
        a6=aspect(6,i,j,k)
        biga1=a2*a3-a4*a4
        biga2=a1*a3-a5*a5
        biga3=a1*a2-a6*a6
        biga4=a5*a6-a1*a4
        biga5=a4*a6-a2*a5
        biga6=a4*a5-a3*a6
        detai=one/(a1*biga1+a6*biga6+a5*biga5)
        aspect(1,i,j,k)=biga1*detai
        aspect(2,i,j,k)=biga2*detai
        aspect(3,i,j,k)=biga3*detai
        aspect(4,i,j,k)=biga4*detai
        aspect(5,i,j,k)=biga5*detai
        aspect(6,i,j,k)=biga6*detai
       end do
     end do
   end do

   if(mype.eq.0) write(6,*)' in get3berr_reg, nlat,nlon,nlatf,nlonf=', & 
                                              nlat,nlon,nlatf,nlonf

   if(mype.eq.0) write(6,*)' ids,ide=',ids,ide
   if(mype.eq.0) write(6,*)' jds,jde=',jds,jde

  if(lreadnorm) normal=0

  call init_raf4(aspect,triad4,ngauss,rgauss,npass,normal,binom,ifilt_ord,filter_all, &
                 nvars,idvar,kvar_start,kvar_end,var_names, &
                 ids, ide, jds, jde, kds, kde, &         ! domain indices
                 ips, ipe, jps, jpe, kps, kpe, &         ! patch indices
                 ims, ime, jms, jme, kms, kme, &         ! memory indices
                 mype, npe)

  write(clun(1:4),'(i4.4)') mype
  do k=kps,kpe
    ivar=idvar(k)
    open (94,file='fltnorm.dat_'//trim(chvarname(ivar))//'_'//clun,form='unformatted')
    do j=jps,jpe
      do i=ips,ipe
        do igauss=1,ngauss
           if(lreadnorm) then
             read(94)  filter_all(1)%amp(igauss,i,j,k)
            else
             write(94) filter_all(1)%amp(igauss,i,j,k)
           endif
        end do
      end do
    end do
  end do
  close(94)

!  filter normalized to unit amplitude.  now adjust amplitude to correspond
!  to desired error amplitude.
!
!                    (corz, corp contain sqrt(error variance) from background error file)
!                    (an_amp are input tuneable error amplitude parameters)

  factoz = 0.0002_r_kind*r25
  do k=kps,kpe
    ivar=idvar(k)
    kvar=k-kvar_start(ivar)+1
    do j=jps,jpe
      do i=ips,ipe
             l=int(rllatf(i,j))
             lp=l+1
             dl2=rllatf(i,j)-float(l)
             dl1=one-dl2
             if(ivar.eq. 1) factk=dl1*corz(l,kvar,1)+dl2*corz(lp,kvar,1)  ! streamfunction
             if(ivar.eq. 2) factk=dl1*corz(l,kvar,2)+dl2*corz(lp,kvar,2)  ! velocity potential
             if(ivar.eq. 3) factk=dl1*corp(l)+dl2*corp(lp)                ! log(ps)
             if(ivar.eq. 4) factk=dl1*corz(l,kvar,3)+dl2*corz(lp,kvar,3)  ! temperature
             if(ivar.eq. 5) factk=dl1*corz(l,kvar,4)+dl2*corz(lp,kvar,4)  ! specific humidity
             if(ivar.eq. 6) factk=factoz                                  ! ozone
             if(ivar.eq. 7) factk=zero_3                                  ! sea surface temperature
             if(ivar.eq. 8) factk=one                                     ! land surface temperature
             if(ivar.eq. 9) factk=one                                     ! ice surface temperature
             if(ivar.eq.10) factk=dl1*corz(l,kvar,4)+dl2*corz(lp,kvar,4)  ! cloud condensate mixing ratio


!               Note: ideally, correct an_amp should come from
!               namelist anbkgerr

        if (ivar == 1)  an_amp(:,ivar)=0.50_r_double
        if (ivar == 2)  an_amp(:,ivar)=0.50_r_double
        if (ivar == 3)  an_amp(:,ivar)=0.50_r_double
        if (ivar == 4)  an_amp(:,ivar)=1.10_r_double
        if (ivar == 5)  an_amp(:,ivar)=0.70_r_double
        if (ivar == 6)  an_amp(:,ivar)=0.80_r_double
        if (ivar == 7)  an_amp(:,ivar)=0.60_r_double
        if (ivar == 8)  an_amp(:,ivar)=0.60_r_double
        if (ivar == 9)  an_amp(:,ivar)=0.60_r_double
        if (ivar == 10) an_amp(:,ivar)=0.60_r_double

        do igauss=1,ngauss
          factor=factk*an_amp(igauss,ivar)
          filter_all(1)%amp(igauss,i,j,k)=factor*filter_all(1)%amp(igauss,i,j,k)
        end do
      end do
    end do
  end do

  deallocate(corz,corp,hwll,hwllp,vz,aspect)
  deallocate(dxf,dyf,rllatf,theta0f,theta0zf)
  deallocate(u0f,u0zf,v0f,v0zf,z0f)

end subroutine get3berr_reg

!=======================================================================
!=======================================================================

subroutine init_anisofilter_reg
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:   init_anisofilter_reg 
! prgmmr: pondeca          org: np22                date: 2006-08-01
!
! abstract: initialize anisotropic background error related variables
!
! program history log:
!   2006-08-01  pondeca
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$ end documentation block

  implicit none

  allocate (rfact0h(10))
  allocate (rfact0v(10))
  
  rfact0h(1)=1.0_r_kind    ;  rfact0v(1)=1.50_r_kind
  rfact0h(2)=1.0_r_kind    ;  rfact0v(2)=1.50_r_kind
  rfact0h(3)=1.2_r_kind    ;  rfact0v(3)=1.50_r_kind
  rfact0h(4)=1.5_r_kind    ;  rfact0v(4)=1.50_r_kind
  rfact0h(5)=1.5_r_kind    ;  rfact0v(5)=1.25_r_kind
  rfact0h(6)=1.0_r_kind    ;  rfact0v(6)=1.25_r_kind
  rfact0h(7)=1.0_r_kind    ;  rfact0v(7)=1.25_r_kind
  rfact0h(8)=1.0_r_kind    ;  rfact0v(8)=1.25_r_kind
  rfact0h(9)=1.0_r_kind    ;  rfact0v(9)=1.25_r_kind
  rfact0h(10)=1.0_r_kind   ;  rfact0v(10)=1.25_r_kind
  
end subroutine init_anisofilter_reg

!=======================================================================
!=======================================================================

subroutine read_bckgstats(mype)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:   read_bckgstats
! prgmmr: pondeca          org: np22                date: 2006-08-01
!
! abstract: read in background error statistics. built from parrish's 
!           old anprewgt_reg
!
!
! program history log:
!   2006-08-01  pondeca
!
!   input argument list:
!    mype     - mpi task id
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$ end documentation block
  implicit none

! Declare passed variables
  integer(i_kind),intent(in):: mype

! Declare local variables
  integer(i_kind) j,k,l,n,inerr

  real(r_kind),allocatable::rlsig(:),dlsig(:)

! Read dimension of stats file
  inerr=22
  open(inerr,file='berror_stats',form='unformatted')
  rewind inerr
  read(inerr)msig,mlat

! Allocate arrays in stats file
  allocate ( corz(1:mlat,1:nsig,1:4) )
  allocate ( corp(1:mlat) )
  allocate ( hwll(0:mlat+1,1:nsig,1:4),hwllp(0:mlat+1) )
  allocate ( vz(1:nsig,0:mlat+1,1:6) )
  allocate ( agvi(0:mlat+1,1:nsig,1:nsig) )
  allocate ( bvi(0:mlat+1,1:nsig),wgvi(0:mlat+1,1:nsig) )

! Read in background error stats and interpolate in vertical 
! to that specified in namelist

  allocate(rlsig(nsig))
  allocate(dlsig(nsig))

  call rdgstat_reg(msig,mlat,inerr,&
       hwll,hwllp,vz,agvi,bvi,wgvi,corz,corp,rlsig)
  deallocate(agvi,bvi,wgvi)
  close(inerr)

  write(6,*)'in read_bckgstats,mlat=',mlat

  allocate(vzimax(1:nsig,1:6))
  allocate(vzimin(1:nsig,1:6))
  allocate(vziavg(1:nsig,1:6))
  allocate(corzavg(1:nsig,1:4))
  allocate(hwllavg(1:nsig,1:4))


  if(qoption==2)then
    varq(1:mlat,1:nsig)=corz(1:mlat,1:nsig,4)
    corz(1:mlat,1:nsig,4)=1.
  endif

! Normalize vz with del sigma and convert to vertical grid units!
  if (.not.twodvar_regional) then
     dlsig(1)=rlsig(1)-rlsig(2)
     do k=2,nsig-1
     dlsig(k)=half*(rlsig(k-1)-rlsig(k+1))
     enddo
     dlsig(nsig)=rlsig(nsig-1)-rlsig(nsig)
   else
     dlsig=one  ! Really no meaning for 2dvar.  Set to 1.0 to avoid
               ! division by zero below.  Array vz is reset for 2dvar
               ! case, so vz calculation below is not truly need.
  end if
  
  do k=1,nsig
    vz(k,0:mlat+1,1:6)=vz(k,0:mlat+1,1:6)*dlsig(k)
  end do

  call create_anberror_vars_reg(mype)

!----- apply scaling to vertical length scales.
!      note:  parameter vs needs to be inverted

  write(6,*)'in read_bckgstats,an_vs=',an_vs
  an_vs=one/an_vs
  vz=vz/an_vs
  if (twodvar_regional) vz(:,:,:)=sqrt(one)


!-----compute and print out diagnostics for 
!     vertical length scales

  do n=1,6
    do k=1,nsig
      vzimax(k,n)=maxval(one/vz(k,0:mlat+1,n))
      vzimin(k,n)=minval(one/vz(k,0:mlat+1,n))
      vziavg(k,n)=sum((one/vz(k,0:mlat+1,n)))/float(mlat+2)
   end do
     
   if(mype.eq.0) then
      do k=1,nsig
        write(6,'(" var,k,max,min,avg vert corlen =",2i4,3f11.3)') & 
               n,k,vzimax(k,n),vzimin(k,n),vziavg(k,n)
       end do
    end if
  end do

!-----optionally, remove latitudinal dependence from statistics
  if (.not.latdepend) then

    do n=1,4
      do k=1,nsig
        corzavg(k,n)=sum(corz(1:mlat,k,n))/float(mlat)
        hwllavg(k,n)=sum(hwll(0:mlat+1,k,n))/float(mlat+2)
      end do
    end do
    corpavg=sum(corp(1:mlat))/float(mlat)
    hwllpavg=sum(hwllp(0:mlat+1))/float(mlat+2)

    do j=1,mlat
       corz(j,1:nsig,1:4)=corzavg(1:nsig,1:4)
       corp(j)=corpavg
    end do
    do j=0,mlat+1
       hwll(j,1:nsig,1:4)=hwllavg(1:nsig,1:4)
       hwllp(j)=hwllpavg
       vz(1:nsig,j,1:6)=one/vziavg(1:nsig,1:6)
    end do

  end if

! hybrid sigma level structure calculated in rdgstat_reg
! ks used to load constant horizontal scales for SF/VP
! above sigma level 0.15
! loop l for diff variable of each PE.
                                                                                                                
 psfc015=r015*ges_psfcavg
 allocate (ks(nsig1o))
  do l=1,nsig1o
    ks(l)=nsig+1
    if(nvar_id(l)<3)then
      k_loop: do k=1,nsig
        if (ges_prslavg(k)< psfc015) then
          ks(l)=k
          exit k_loop
        end if
      enddo k_loop
    endif
  end do

end subroutine read_bckgstats

!=======================================================================
!=======================================================================

subroutine get_background(mype)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:   get_background
! prgmmr: pondeca          org: np22                date: 2006-08-01
!
! abstract: compute background fields and their spatial derivatives
!           on filter grid for use in anisotropic covariance model. 
!           built from parrish's old anprewgt_reg
!
!
! program history log:
!   2006-08-01  pondeca
!   2008-06-05  safford - rm unused vars
!
!   input argument list:
!    mype     - mpi task id
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$ end documentation block
  implicit none

! Declare passed variables
  integer(i_kind),intent(in):: mype

! Declare local variables
  integer(i_kind) i,j,k,l,kp,km,mm1,iflg,k1,ivar
  integer(i_kind) n

  real(r_kind) hwll_loc
  real(r_kind) dzi
  real(r_kind) asp1,asp2,asp3

  real(r_kind),allocatable,dimension(:,:,:)::work_st,work_vp,work_t,work_q, & 
                                             work_oz,work_cwmr,hfine
  real(r_kind),allocatable,dimension(:,:)::work_p,work_sst,work_slndt,work_sicet
! real(r_kind),allocatable,dimension(:,:)::hfilter
  real(r_kind),allocatable,dimension(:,:,:)::field,fieldz
  real(r_single),allocatable,dimension(:,:,:)::field0f,field0zf

!   get dxf,dyf,rllatf
!       note: if filter grid coarser than analysis grid, then normalized 
!       adjoint of filter to analysis interpolation used to transfer fields 
!       from analysis grid to filter grid. otherwise, normal interpolation 
!       is done. this is transparent at this level. it appears in the 
!       definition of the interpolation and adjoint of interpolation
!       weights. check for accuracy.(done).

  allocate(dxf(nlatf,nlonf),dyf(nlatf,nlonf),rllatf(nlatf,nlonf))

  call agrid2fgrid(region_dx,dxf)
  dxf=grid_ratio_lon*dxf             !  note that dxf = grid_ratio_lon*dx
  call agrid2fgrid(region_dy,dyf)    
  dyf=grid_ratio_lat*dyf             !  note that dyf = grid_ratio_lat*dy
  call agrid2fgrid(rllat,rllatf)
   if(mype.eq.0) then
     write(6,*)' at 11.28 in anprewgt_reg, nlatf,nlonf=',nlatf,nlonf
     write(6,*)' at 11.29 in anprewgt_reg, min,max(rllat)=',minval(rllat),maxval(rllat)
     write(6,*)' at 11.3 in anprewgt_reg, min,max(rllatf)=',minval(rllatf),maxval(rllatf)
     write(6,*)' at 11.31 in anprewgt_reg, min,max(grid_ratio_lon*dx)=', &
                    minval(grid_ratio_lon*region_dx),maxval(grid_ratio_lon*region_dx)
     write(6,*)' at 11.32 in anprewgt_reg, min,max(dxf)=',minval(dxf),maxval(dxf)
     write(6,*)' at 11.33 in anprewgt_reg, min,max(grid_ratio_lat*dy)=', &
                    minval(grid_ratio_lat*region_dy),maxval(grid_ratio_lat*region_dy)
     write(6,*)' at 11.34 in anprewgt_reg, min,max(dyf)=',minval(dyf),maxval(dyf)
   end if

  mm1=mype+1

!  convert all basic variables from subdomain to slab mode and interpolate to filter grid
!  then repeat this process for all vertical derivatives of subdomain variables

  allocate(hfilter(nlatf,nlonf),hfine(nlat,nlon,nsig1o))
  allocate(work_st(lat2,lon2,nsig),work_vp(lat2,lon2,nsig),work_t(lat2,lon2,nsig))
  allocate(work_q(lat2,lon2,nsig),work_oz(lat2,lon2,nsig),work_cwmr(lat2,lon2,nsig))
  allocate(work_p(lat2,lon2),work_sst(lat2,lon2))
  allocate(work_slndt(lat2,lon2),work_sicet(lat2,lon2))


  allocate(field(lat2,lon2,nsig))
  allocate(fieldz(lat2,lon2,nsig))

  allocate(theta0f(nlatf,nlonf,nsig1o))
  allocate(theta0zf(nlatf,nlonf,nsig1o))

  allocate(u0f(nlatf,nlonf,nsig1o))
  allocate(u0zf(nlatf,nlonf,nsig1o))

  allocate(v0f(nlatf,nlonf,nsig1o))
  allocate(v0zf(nlatf,nlonf,nsig1o))

  allocate(z0f(nlatf,nlonf,nsig1o))


  it=ntguessig

  do n=1,4

   do k=1,nsig
    do j=1,lon2
     do i=1,lat2
     if (n==1) field(i,j,k)=ges_tv(i,j,k,it)/(ges_prsl(i,j,k,it)/r100)**rd_over_cp
     if (n==2) field(i,j,k)=ges_u(i,j,k,it)
     if (n==3) field(i,j,k)=ges_v(i,j,k,it)
     if (n==4) field(i,j,k)=ges_z(i,j,it)
     end do
    end do
   end do

   
   work_t=field
   work_st=field
   work_vp=field
   work_q=field
   work_oz=field
   work_cwmr=field

   work_p(:,:)=field(:,:,1)
   work_sst(:,:)=field(:,:,1)
   work_slndt(:,:)=field(:,:,1)
   work_sicet(:,:)=field(:,:,1)

   iflg=1
   call sub2grid(hfine,work_t,work_p,work_q,work_oz,work_sst, &
                work_slndt,work_sicet,work_cwmr,work_st,work_vp,iflg)

   do k=1,nsig1o
     call agrid2fgrid(hfine(1,1,k),hfilter) !analysis to filter grid
     do j=1,nlonf
       do i=1,nlatf
        if (n==1) theta0f(i,j,k)=hfilter(i,j)
        if (n==2) u0f(i,j,k)=hfilter(i,j)
        if (n==3) v0f(i,j,k)=hfilter(i,j)
        if (n==4) z0f(i,j,k)=hfilter(i,j)
       end do
     end do
   end do

   do k=1,nsig
    kp=min(nsig,k+1)
    km=max(1,k-1)
    dzi=zero !formal fix for twodvar_regional
    if (.not.twodvar_regional) dzi=one/(kp-km)
    do j=1,lon2
     do i=1,lat2
       f1p=field(i,j,kp)
       f1m=field(i,j,km)
       fieldz(i,j,k)=dzi*(f1p-f1m)
      end do
     end do
   end do

   work_t=fieldz
   work_st=fieldz
   work_vp=fieldz
   work_q=fieldz
   work_oz=fieldz
   work_cwmr=fieldz

   work_p=zero! fieldz(:,:,1)
   work_sst=zero! fieldz(:,:,1)
   work_slndt=zero! fieldz(:,:,1)
   work_sicet=zero! fieldz(:,:,1)

   call sub2grid(hfine,work_t,work_p,work_q,work_oz,work_sst, &
                 work_slndt,work_sicet,work_cwmr,work_st,work_vp,iflg)

   do k=1,nsig1o
    call agrid2fgrid(hfine(1,1,k),hfilter) !analysis to filter grid
    do j=1,nlonf
      do i=1,nlatf
        if (n==1) theta0zf(i,j,k)=hfilter(i,j)
        if (n==2) u0zf(i,j,k)=hfilter(i,j)
        if (n==3) v0zf(i,j,k)=hfilter(i,j)
        end do
    end do
   end do
  end do
!-----------------------------------------------------------
!-------end of getting background variables on filter grid--
!-----------------------------------------------------------
  allocate(psg(nlat,nlon,nsig1o))
  it=ntguessig
  do k=1,nsig
   do j=1,lon2
    do i=1,lat2
     work_t(i,j,k)=1000._r_single*ges_ps(i,j,it)
     work_st(i,j,k)=work_t(i,j,k)
     work_vp(i,j,k)=work_t(i,j,k)
     work_q(i,j,k)=work_t(i,j,k)
     work_oz(i,j,k)=work_t(i,j,k)
     work_cwmr(i,j,k)=work_t(i,j,k)
     if(k.eq.1) then
      work_p(i,j)=work_t(i,j,k)
      work_sst(i,j)=work_t(i,j,k)
      work_slndt(i,j)=work_t(i,j,k)
      work_sicet(i,j)=work_t(i,j,k)
     end if
    end do
   end do
  end do
  iflg=1
  call sub2grid(psg,work_t,work_p,work_q,work_oz,work_sst, &
                work_slndt,work_sicet,work_cwmr,work_st,work_vp,iflg)
!-----------------------------------------------------------
!-----------------------------------------------------------
!-----------------------------------------------------------
  deallocate(hfilter,hfine,work_st,work_vp,work_t,work_q,work_oz,work_cwmr)
  deallocate(work_p,work_sst,work_slndt,work_sicet)

! ------------------------------------------------------------
! ------------ in this section, set up isotropic filter for 
! ------------ generating smoothed guess
! ------------------------------------------------------------

  allocate(field0f(nlatf,nlonf,nsig1o))
  allocate(field0zf(nlatf,nlonf,nsig1o))

  allocate(aspect(7,nlatf,nlonf,nsig1o))

  do k=1,nsig1o
   do j=1,nlonf
    do i=1,nlatf
       k1=levs_id(k)
       if (k1==izero) cycle
 
       if (twodvar_regional) then 
         aspect(1,i,j,k)=10._r_kind**2/grid_ratio_lat
         aspect(2,i,j,k)=10._r_kind**2/grid_ratio_lat
         aspect(3,i,j,k)=1._r_kind**2
         aspect(4:7,i,j,k)=zero
        else
         ivar=3
         l=int(rllatf(nlatf/2,nlonf/2))
         hwll_loc=hwll(l,k1,ivar) 
         asp1=hwll_loc/min(dyf(nlatf/2,nlonf/2),dxf(nlatf/2,nlonf/2))*rfact0h(4)
         asp2=asp1
         asp3=one/vz(k1,l,ivar)*rfact0v(4) 
         aspect(1,i,j,k)=asp1**2
         aspect(2,i,j,k)=asp2**2
         aspect(3,i,j,k)=asp3**2
         aspect(4:7,i,j,k)=zero
       endif

    end do
   end do
  end do

  ngauss_smooth=1
  rgauss_smooth=one
  npass_smooth=1
  normal_smooth=0
  ifilt_ord_smooth=4
  call init_raf4(aspect,triad4,ngauss_smooth,rgauss_smooth, & 
                 npass_smooth,normal_smooth,binom, &
                 ifilt_ord_smooth,filter_all, &
                 nvars,idvar,kvar_start,kvar_end,var_names, &
                 ids, ide, jds, jde, kds, kde, &         ! domain indices
                 ips, ipe, jps, jpe, kps, kpe, &         ! patch indices
                 ims, ime, jms, jme, kms, kme, &         ! memory indices
                 mype, npe)

  do n=1,4
    if (n==1) then 
      field0f=theta0f
      field0zf=theta0zf
     else if (n==2) then
      field0f=u0f
      field0zf=u0zf
     else if (n==3) then
      field0f=v0f
      field0zf=v0zf
     else if (n==4) then
      field0f=z0f
      field0zf=z0f
    endif  
  
    call raf_sm4(field0f,filter_all,ngauss_smooth, &
                   ids, ide, jds, jde, kds, kde, &         ! domain indices
                   ips, ipe, jps, jpe, kps, kpe, &         ! patch indices
                   ims, ime, jms, jme, kms, kme, &         ! memory indices
                   mype, npe)
    call raf_sm4_ad(field0f,filter_all,ngauss_smooth, &
                   ids, ide, jds, jde, kds, kde, &         ! domain indices
                   ips, ipe, jps, jpe, kps, kpe, &         ! patch indices
                   ims, ime, jms, jme, kms, kme, &         ! memory indices
                   mype, npe)
    call raf_sm4(field0zf,filter_all,ngauss_smooth, &
                   ids, ide, jds, jde, kds, kde, &         ! domain indices
                   ips, ipe, jps, jpe, kps, kpe, &         ! patch indices
                   ims, ime, jms, jme, kms, kme, &         ! memory indices
                   mype, npe)
    call raf_sm4_ad(field0zf,filter_all,ngauss_smooth, &
                   ids, ide, jds, jde, kds, kde, &         ! domain indices
                   ips, ipe, jps, jpe, kps, kpe, &         ! patch indices
                   ims, ime, jms, jme, kms, kme, &         ! memory indices
                   mype, npe)

    if (n==1) then 
      theta0f=field0f
      theta0zf=field0zf
     else if (n==2) then
      u0f=field0f
      u0zf=field0zf
     else if (n==3) then
      v0f=field0f
      v0zf=field0zf
     else if (n==4) then
      z0f=field0f
  endif
  end do

  call raf_sm4(z0f,filter_all,ngauss_smooth, &
                 ids, ide, jds, jde, kds, kde, &         ! domain indices
                 ips, ipe, jps, jpe, kps, kpe, &         ! patch indices
                 ims, ime, jms, jme, kms, kme, &         ! memory indices
                 mype, npe)
  call raf_sm4_ad(z0f,filter_all,ngauss_smooth, &
                 ids, ide, jds, jde, kds, kde, &         ! domain indices
                 ips, ipe, jps, jpe, kps, kpe, &         ! patch indices
                 ims, ime, jms, jme, kms, kme, &         ! memory indices
                 mype, npe)


 allocate(asp10f(nlatf,nlonf))
 allocate(asp20f(nlatf,nlonf))
 allocate(asp30f(nlatf,nlonf))

 deallocate(field)
 deallocate(fieldz)
 deallocate(field0f)
 deallocate(field0zf)

end subroutine get_background

!=======================================================================
!=======================================================================

subroutine isotropic_scales(scale1,scale2,scale3,k,mype)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:   isotropic_scales
! prgmmr: pondeca          org: np22                date: 2006-08-01
!
! abstract: compute isotropic length scales of auto-correlation model.
!           built from parrish's old anprewgt_reg
!
! program history log:
!   2006-08-01  pondeca
!   2008-06-05  safford - rm unused var
!
!   input argument list:
!    mype     - mpi task id
!    k        - level number of field in slab mode
!
!   output argument list:
!    scale1     - 2d field of correlations lengths in the x-direction
!    scale2     - 2d field of correlations lengths in the y-direction
!    scale3     - 2d field of correlations lengths in the vertical direction
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$ end documentation block
  implicit none

!Declare passed variables
 integer(i_kind) k, mype

!Declare local variables
 integer(i_kind) i,j,k1,ivar,l,lp

 real(r_kind) scale1(nlatf,nlonf)
 real(r_kind) scale2(nlatf,nlonf)
 real(r_kind) scale3(nlatf,nlonf)
 real(r_kind) dl1,dl2,hwll_loc


    k1=levs_id(k)

    if (nvar_id(k)==1)  ivar=1  ! streamfunction
    if (nvar_id(k)==2)  ivar=2  ! velocity potential
    if (nvar_id(k)==4)  ivar=3  ! temperature
    if (nvar_id(k)==5)  ivar=4  ! specific humidity
    if (nvar_id(k)==6)  ivar=5  ! Ozone
    if (nvar_id(k)==7)  ivar=1  ! SST
    if (nvar_id(k)==8)  ivar=4  ! cloud water
    if (nvar_id(k)==9)  ivar=1  ! surface temp (land)
    if (nvar_id(k)==10) ivar=1  ! surface temp (ice)

    do j=1,nlonf
       do i=1,nlatf

          if (nvar_id(k)==1 .or. nvar_id(k)==2 .or. nvar_id(k)==4 .or. &
              nvar_id(k)==5 .or. nvar_id(k)==8 )then
               if(k1 >= ks(k))then
                 l=int(rllatf(nlatf/2,nlonf/2))
                 hwll_loc=hwll(l,k1,ivar) 
               else
                 l=int(rllatf(i,j))
                 lp=l+1
                 dl2=rllatf(i,j)-float(l)
                 dl1=one-dl2
                 hwll_loc=dl1*hwll(l,k1,ivar)+dl2*hwll(lp,k1,ivar)
               end if
               scale3(i,j)=one/vz(k1,l,ivar) 

            else if (nvar_id(k)==3) then        !surface pressure  
               l=int(rllatf(i,j))
               lp=l+1
               dl2=rllatf(i,j)-float(l)
               dl1=one-dl2
               hwll_loc=dl1*hwllp(l)+dl2*hwllp(lp)
               scale3(i,j)=one 

            else if (nvar_id(k)==6) then
               if(k1 <= nsig*3/4)then
                  hwll_loc=r400000
               else
                 hwll_loc=(r800000-r400000*(nsig-k1)/(nsig-nsig*3/4))
               end if
               l=int(rllatf(nlatf/2,nlonf/2))
               scale3(i,j)=one/vz(k1,l,ivar) 

            else if (nvar_id(k)==7) then ! SST
               l=int(rllatf(i,j))
               lp=l+1
               dl2=rllatf(i,j)-float(l)
               dl1=one-dl2
               hwll_loc=half*(dl1*hwll(l,1,ivar)+dl2*hwll(lp,1,ivar))
               scale3(i,j)=one 

            else if (nvar_id(k)==9) then ! surface temp (land)
               l=int(rllatf(i,j))
               lp=l+1
               dl2=rllatf(i,j)-float(l)
               dl1=one-dl2
               hwll_loc=zero_25*(dl1*hwll(l,1,ivar)+dl2*hwll(lp,1,ivar))
               scale3(i,j)=one 

            else if (nvar_id(k)==10) then ! surface temp (ice)
               l=int(rllatf(i,j))
               lp=l+1
               dl2=rllatf(i,j)-float(l)
               dl1=one-dl2
               hwll_loc=zero_25*(dl1*hwll(l,1,ivar)+dl2*hwll(lp,1,ivar))
               scale3(i,j)=one 
          end if 
           
          scale1(i,j)=hwll_loc/dyf(i,j)
          scale2(i,j)=hwll_loc/dxf(i,j)


          if (twodvar_regional) then

            !Rescaling

            if (nvar_id(k)==4 .or. nvar_id(k)==5) then
               scale1(i,j)=0.75_r_kind*scale1(i,j)
               scale2(i,j)=0.75_r_kind*scale2(i,j)
             else
               scale1(i,j)=0.5_r_kind*scale1(i,j)
               scale2(i,j)=0.5_r_kind*scale2(i,j)
            endif

          else 

            if (.not.latdepend) then
              scale1(i,j)=max(scale1(i,j),scale2(i,j))
              scale2(i,j)=scale1(i,j)
            endif

            !rescaling to roughly match original analysis from purely isotropic  
            !option, ie.. when anisotropic=.false. in namelist "anbkgerr".

            scale1(i,j)=rfact0h(nvar_id(k))*scale1(i,j)
            scale2(i,j)=rfact0h(nvar_id(k))*scale2(i,j)
            if (nvar_id(k) /=3 .and. nvar_id(k) /=7 .and. & 
                nvar_id(k) /=9 .and. nvar_id(k) /=10 ) & 
            scale3(i,j)=rfact0v(nvar_id(k))*scale3(i,j)
          endif
    enddo
       enddo
  return
end subroutine isotropic_scales

!=======================================================================
!=======================================================================

subroutine get_theta_corrl_lenghts(mype)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:   get_theta_corrl_lenghts
! prgmmr: pondeca          org: np22                date: 2006-08-01
!
! abstract: compute function correlations lengths of Riishojgaard-type 
!           anisotropic auto-correlation model
!
! program history log:
!   2006-08-01  pondeca
!   2008-06-05  safford - rm unused vars
!
!   input argument list:
!    mype     - mpi task id
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$ end documentation block

  implicit none

! Declare passed variables
  integer(i_kind),intent(in):: mype

! Declare local variables
  integer(i_kind) i,j,k,ip,im,jp,jm,kp,km,k1
  integer(i_kind) mcount0,mcount

  real(r_kind) fx2,fx1,fx3,dxi,dyi,dzi
  real(r_kind) asp1,asp2,asp3
  real(r_kind) rho

  real(r_kind) pbar4a,pbar4(nsig),hgt4(nsig),tbar4(nsig), &
              thetabar4(nsig),dthetabarz(nsig),dthetabarzmax


!-----compute max value of correlation length at each model level
  allocate(qlth_temp(nsig))
  allocate(qltv_temp(nsig))
  allocate(qlth_wind(nsig))
  allocate(qltv_wind(nsig))

!compute scaling factors for the function correlation length  
  it=ntguessig
  do k=1,nsig
   pbar4a=zero
   do j=1,lon2
    do i=1,lat2
     pbar4a=pbar4a+ges_prsl(i,j,k ,it)*10._r_kind
    end do
   end do
   mcount0=lon2*lat2! It's OK to count buffer points 
   call mpi_allreduce(pbar4a,pbar4(k),1,mpi_real8,mpi_sum,mpi_comm_world,ierror)
   call mpi_allreduce(mcount0,mcount,1,mpi_integer4,mpi_sum,mpi_comm_world,ierror)
   pbar4(k)=pbar4(k)/float(mcount)
   if(mype.eq.0) write(6,*)'in get3berr_reg,k,pbar4=',k,pbar4(k)
   call w3fa03(pbar4(k),hgt4(k),tbar4(k),thetabar4(k))
  end do

  dthetabarzmax=zero
  do k=1,nsig
   kp=min(nsig,k+1)
   km=max(1,k-1)
   dzi=one/(kp-km)
   dthetabarz(k)=dzi*(thetabar4(kp)-thetabar4(km))
   dthetabarzmax=max(dthetabarz(k),dthetabarzmax)
   if(mype.eq.0) then
      write(6,*)'("in get3berr_reg,k,pbar4,hgt4,tbar4=",i4,3f11.3)',k,pbar4(k),hgt4(k),tbar4(k)
      write(6,*)'("in get3berr_reg,k,thetabar4,dthetabarz=",i4,2f11.3)',k,thetabar4(k),dthetabarz(k)
   endif
  end do
  if(mype.eq.0) write(6,*)'in get3berr_reg,dthetabarzmax=',dthetabarzmax

  do k=1,nsig
   dthetabarz(k)=dthetabarz(k)/dthetabarzmax
   if(mype.eq.0) then
      write(6,*)'in get3berr_reg,k,normalized dthetabarz=',k,dthetabarz(k)
   endif
  end do

  do k=1,nsig
   qlth_temp(k)=qlth_temp0
   qlth_wind(k)=qlth_wind0
   if (k.le.44) then 
     qltv_temp(k)=qltv_temp0
     qltv_wind(k)=qltv_wind0
    else
     qltv_temp(k)=qltv_temp0*dthetabarz(k)/dthetabarz(44)*4._r_kind
     qltv_wind(k)=qltv_wind0*dthetabarz(k)/dthetabarz(44)*4._r_kind
   endif
  end do

  call hanning_smther(qltv_temp, nsig, 5)
  call hanning_smther(qltv_wind, nsig, 5)

  if (mype.eq.0) then
   do k=1,nsig
     write(6,*)'in get3berr_reg,k,qltv_temp,qltv_wind=',k,qltv_temp(k),qltv_wind(k)
   enddo
  endif


  if (mype.eq.0) then
   open (94,file='std_atm.dat',form='unformatted')
   write(94)pbar4
   write(94)hgt4
   write(94)tbar4
   write(94)thetabar4
   write(94)dthetabarz
   close(94)
  endif
!-----------------------------------------------------------
!-----define the anisotropic aspect tensor------------------
!-----------------------------------------------------------
! Set up scales
  allocate(asp1_max(10,nsig))
  allocate(asp2_max(10,nsig))
  allocate(asp3_max(10,nsig))
  asp1_max(:,:)=zero
  asp2_max(:,:)=zero
  asp3_max(:,:)=zero
  do k=1,nsig1o
    k1=levs_id(k)
    if (k1==izero)  cycle      !  skip to next k value
    call isotropic_scales(asp10f,asp20f,asp30f,k,mype)


    do j=1,nlonf
       do i=1,nlatf

          asp1=asp10f(i,j)
          asp2=asp20f(i,j)
          asp3=asp30f(i,j)
 
          asp1_max(nvar_id(k),k1)=max(asp1,asp1_max(nvar_id(k),k1))
          asp2_max(nvar_id(k),k1)=max(asp2,asp2_max(nvar_id(k),k1))
          asp3_max(nvar_id(k),k1)=max(asp3,asp3_max(nvar_id(k),k1))

       end do
    end do
  end do

!-----use smoothed guess to compute fields of bounded horizontal
!     and vertical derivatives. Then smooth the resulting fields 
  allocate(tx1_slab(nlatf,nlonf,nsig1o))
  allocate(tx2_slab(nlatf,nlonf,nsig1o))
  allocate(tx3_slab(nlatf,nlonf,nsig1o))

  tx1_slab(:,:,:)=zero 
  tx2_slab(:,:,:)=zero 
  tx3_slab(:,:,:)=zero 
  do k=1,nsig1o
    k1=levs_id(k)
    if (k1==izero)  cycle      !  skip to next k value
    call isotropic_scales(asp10f,asp20f,asp30f,k,mype)

    do j=1,nlonf
       do i=1,nlatf

          asp1=asp10f(i,j)
          asp2=asp20f(i,j)
          asp3=asp30f(i,j)
 
          jp=min(nlonf,j+1) ; jm=max(1,j-1)
          dxi=one/(jp-jm)
           
          ip=min(nlatf,i+1) ; im=max(1,i-1)
          dyi=one/(ip-im)
           
          fx1= dyi*(theta0f(ip,j,k)-theta0f(im,j,k))
          fx2= dxi*(theta0f(i,jp,k)-theta0f(i,jm,k))
          fx3= theta0zf(i,j,k)

          qlth=one
          qltv=one
          if (nvar_id(k)==1) then ; qlth=qlth_wind(k1) ; qltv=qltv_wind(k1) ; endif
          if (nvar_id(k)==2) then ; qlth=qlth_wind(k1) ; qltv=qltv_wind(k1) ; endif
          if (nvar_id(k)==4) then ; qlth=qlth_temp(k1) ; qltv=qltv_temp(k1) ; endif

          if (nvar_id(k)==1 .or. nvar_id(k)==2 .or. nvar_id(k)==4 ) then
            if (abs(fx1).gt.rvsmall) then
              gmax=two*qlth/asp1_max(nvar_id(k),k1)
              r=abs(fx1)/gmax
              rho=tanh(r)/r
!             fx1=rho*fx1
              tx1_slab(i,j,k)=fx1
             endif

            if (abs(fx2).gt.rvsmall) then
              gmax=two*qlth/asp2_max(nvar_id(k),k1)
              r=abs(fx2)/gmax
              rho=tanh(r)/r
!             fx2=rho*fx2
              tx2_slab(i,j,k)=fx2
             endif

            if (abs(fx3).gt.rvsmall) then
              gmax=two*qltv/asp3_max(nvar_id(k),k1)
              r=abs(fx3)/gmax
              rho=tanh(r)/r
!             fx3=rho*fx3
              tx3_slab(i,j,k)=fx3
             endif
          endif 

       end do
    end do
  call smther_one(tx1_slab,1,nlatf,1,nlonf,2)
  call smther_one(tx2_slab,1,nlatf,1,nlonf,2)
  call smther_one(tx3_slab,1,nlatf,1,nlonf,2)
  end do


end subroutine get_theta_corrl_lenghts

!=======================================================================
!=======================================================================

subroutine hanning_smther(g1, npts, ns)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:   hanning_smther
! prgmmr: pondeca          org: np22                date: 2006-08-01
!
! abstract: apply one pass of hanning smoother to 1d data array
!
! program history log:
!   2006-08-01  pondeca
!
!   input argument list:
!    g1        - 1d array of field to be smoothed
!    npts      - number of data values in array data
!    ns        - number of passes
!
!   output argument list:
!    g1        - 1d array of smoothed field
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$ end documentation block

   use kinds, only: r_kind,i_kind
   implicit none


   integer(i_kind) npts,ns,it,itp,itm,l
   real(r_kind) g1(npts)
   real(r_kind), allocatable:: g2(:)

   allocate(g2(npts))

   do l=1,ns
    g2(:)=g1(:)
    do it = 1,npts
       itp=min(it+1,npts) ; itm=max(1,it-1)
       g1(it) = .25*g2(itm) + .5*g2(it) + .25*g2(itp)
    enddo
   enddo

   deallocate(g2)
   return
end subroutine hanning_smther

!=======================================================================
!=======================================================================

subroutine smther_one(g1,is,ie,js,je,ns)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:   smther_one
! prgmmr: pondeca          org: np22                date: 2006-08-01
!
! abstract: apply 1-2-1 smoother in each direction of data slab
!
! program history log:
!   2006-08-01  pondeca
!
!   input argument list:
!    g1        - 2d array of field to be smoothed
!    is,ie     - first and last i values of data slab
!    js,je     - first and last j values of data slab
!    ns        - number of passes
!
!   output argument list:
!    g1        - smoothed 2d field
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$ end documentation block

  use kinds,only: r_single,i_long
  implicit none

  integer(i_long)  is, ie, js, je
  integer(i_long)  i,j,l,ip,im,jp,jm
  integer(i_long), intent(in) :: ns

  real(r_single), dimension(is:ie, js:je), intent(inout) :: g1
                                   !  on input: original data slab
                                   !  on ouput: filtered data slab
 

  real(r_single), allocatable:: g2(:,:)

   allocate(g2(is:ie,js:je))
   do l=1,ns

     do j=js,je
      do i=is,ie
       ip=min(i+1,ie) ; im=max(is,i-1)
         g2(i,j)=.25*(g1(ip,j)+g1(im,j))+.5*g1(i,j)
      end do
     end do

     do i=is,ie
      do j=js,je
       jp=min(j+1,je) ; jm=max(js,j-1)
       g1(i,j)=.25*(g2(i,jp)+g2(i,jm))+.5*g2(i,j)
      end do
     end do

   end do
   deallocate(g2)

   return
end subroutine smther_one

!=======================================================================
!=======================================================================
end module anisofilter
