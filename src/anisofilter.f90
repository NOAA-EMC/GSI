!-------------------------------------------------------------------------
!    NOAA/NCEP, National Centers for Environmental Prediction GSI        !
!-------------------------------------------------------------------------
module anisofilter
!$$$   module documentation block
!                .      .    .                                       .
! module:    anisofilter
! prgmmr: pondeca          org: np23                date: 2006-08-01
!
! abstract:  computes the anisotropic aspect tensor of the background error
!            auto-correlation model.
!
! program history log:
!   2006-08-01  pondeca
!   2007-09-04  sato - update for fgrid2agrid_mod
!   2007-10-30  sato - some changes for global mode
!   2007-12-20  sato - replace get2berr with get_aspect_reg_2d, and
!                              get3berr with get_aspect_reg_pt.
!                      These new subroutines only make aspect array.
!                      The pre & post processes to initialize the filter
!                      are almost same, so they are moved back to anprewgt.
!                      Some processes were moved to new subroutine.
!   2008-02-27  sato - change iso-aniso composition in get_aspect_reg_ens.
!                      enable to use iensamp. delete some test code.
!   2008-09-22  sato - unified with 2dvar suboption case
!   2010-03-03  zhu  - make some changes for generalizing control variables,
!                      add one more dimension to corp and hwllp
!   2010-04-01  treadon - move strip_single to gridmod
!   2010-05-28  todling - obtain variable id's on the fly (add getindex)
!   2010-06-05  todling - an_amp0 coming from control_vectors
!
!
! subroutines included:
!
!   anprewgt_reg                      - main subroutine for computation of the
!                                       anisotropic aspect tensor of auto-correlation
!                                       model
!   get_aspect_reg_2d                 - compute the anisotropic aspect tensor for the
!                                       2dvar case of gsi-regional
!   get_aspect_reg_pt                 - compute the anisotropic aspect tensor for the
!                                       3dvar case of gsi-regional based on theta
!   fact_qopt2                        - correct q variance for qoption=2
!
!   init_anisofilter_reg              - initialize anisotropic background error
!                                       related variables
!   read_bckgstats                    - read in background error statistics
!
!   get_background                    - compute smoothed versions of the background fields
!                                       and respective spatial derivatives on filter grid
!                                       for use in auto-correlation model
!   raf_sm_reg                        -
!
!   isotropic_scales                  - compute isotropic length-scales of
!                                       auto-correlation model
!   get_theta_corrl_lenghts           - compute function correlation length-scales of
!                                       Riishojgaard-type anisotropic auto-correlation
!                                       model based on the background potential temperature
!   mk_gradpt_slab                    - compute horizontal differential for theta
!   hanning_smther                    - apply hanning smoother to 2-dimensional field
!
!   smther_one                        - apply 1-2-1 smoother to 2-dimensional field
!
!   smther_one_8                      -
!
!   invert_aspect_tensor              - invert ascpect tensor
!
!   get_aspect_det                    -
!
!   get_aspect_reg_ens                - compute the anisotropic aspect tensor for the
!                                       3dvar case of gsi-regional based on ensemble info
!   get_ensmber                       -
!
!   set_range_aniall                  -
!
!   mode_val                          -
!
!   writeout_isoscaleinfo             -
!
!   get2berr_reg_subdomain_option     -
!
!   get_background_subdomain_option   -
!
!   isotropic_scales_subdomain_option -
!
! variable definitions:
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$ end documentation block

  use kinds, only: r_kind,i_kind,r_single,r_double,i_long

  use anberror, only: indices,&
                      idvar,jdvar,kvar_start,kvar_end,levs_jdvar,&
                      var_names,smooth_len, &
                      filter_all,pf2aP1, &
                      triad4,ifilt_ord,npass,normal,binom, &
                      ngauss,rgauss,anhswgt,an_amp,an_vs, &
                      ancovmdl, covmap, lreadnorm, &
                      rtma_subdomain_option,nsmooth,nsmooth_shapiro

  use fgrid2agrid_mod, only: fgrid2agrid,agrid2fgrid

  use gridmod, only: nsig,nsig1o,region_dx,region_dy,nlon,nlat, &
                     lat2,lon2,twodvar_regional, &
                     itotsub,lon1,lat1,&
                     ltosi_s,ltosj_s, &
                     displs_s,displs_g,ijn_s,ijn,strip_single

  use constants, only: izero,                         ione, & ! for integer
                       zero_single, tiny_single,            & ! for real(4)
                       zero,        tiny_r_kind, quarter, half, one, two, three, four, five, & ! for real(8)
                       rd_over_cp, pi, r100

  use balmod,only: llmin,llmax,rllat,fstat

  use raflib,only: init_raf4_wrap,raf_sm4_wrap,raf_sm4_ad_wrap

  use jfunc, only: varq,qoption

  use control_vectors, only: cvars2d,cvars3d,cvarsmd
  use control_vectors, only: nvars,nrf,nrf3_loc,nrf2_loc,nrf_3d,nrf_var
  use control_vectors, only: nrf3 => nc3d
  use control_vectors, only: nrf2 => nc2d
  use control_vectors, only: an_amp0

  use guess_grids, only: ges_u,ges_v,ges_prsl,ges_tv,ges_z,ntguessig,&
                         ges_prslavg,ges_psfcavg,ges_ps,ges_q,ges_tsen

  use mpimod, only: npe,levs_id,nvar_id,ierror,&
                    mpi_real8,mpi_real4,mpi_integer4,mpi_rtype,&
                    mpi_sum,mpi_comm_world

  use aniso_ens_util, only: ens_intpcoeffs_reg,fillanlgrd,ens_uv_to_psichi, &
                            pges_minmax,intp_spl
  use mpeu_util, only: getindex


  implicit none

! set default to private
  private
! set subroutines to public
  public :: anprewgt_reg
  public :: get_aspect_reg_2d
  public :: get_aspect_reg_pt
  public :: fact_qopt2
  public :: init_anisofilter_reg
  public :: read_bckgstats
  public :: get_background
  public :: raf_sm_reg
  public :: isotropic_scales
  public :: get_theta_corrl_lenghts
  public :: mk_gradpt_slab
  public :: hanning_smther
  public :: smther_one
  public :: smther_one_8
  public :: invert_aspect_tensor
  public :: get_aspect_det
  public :: get_aspect_reg_ens
  public :: get_ensmber
  public :: set_range_aniall
  public :: mode_val
  public :: writeout_isoscaleinfo
  public :: get2berr_reg_subdomain_option
  public :: get_background_subdomain_option
  public :: isotropic_scales_subdomain_option
! set passed variables to public
  public :: theta0zf,theta0f,asp3_max,u0f,v0zf,v0f,u0zf,tx1_slab,hfilter,hfine,tx2_slab,asp2_max,asp1_max,tx3_slab
  public :: qltv_wind,qlth_wind,qltv_temp,eampmax,pgesmax,pgesmin,eampmin,asp10f,rh0f,z0f,asp20f,qlth_temp,psg,asp30f
  public :: qlth_wind0,qltv_temp0,qlth_temp0,qltv_wind0,scalex3,scalex2,scalex1,lreadnorm
  public :: r015,corp,corz,rfact0v,hwll,aspect,vz,hwllp,stpcode_ensdata,stpcode_namelist,stpcode_alloc
  public :: stpcode_statdata,rfact0h,ks,mlat,rllatf,ensamp

! Declare passed variables

! Declare local parameters
  integer(i_kind),parameter:: stpcode_alloc    = 101_i_kind ! stop code (memory allocation error)
  integer(i_kind),parameter:: stpcode_namelist = 102_i_kind ! stop code (namelist has error)
  integer(i_kind),parameter:: stpcode_ensdata  = 103_i_kind ! stop code (ensemble data error)
  integer(i_kind),parameter:: stpcode_statdata = 104_i_kind ! stop code (stat data error)

  logical:: latdepend

  real(r_kind),parameter:: zero_3       = 0.3_r_kind
  real(r_kind),parameter:: r400000      = 400000.0_r_kind
  real(r_kind),parameter:: r800000      = 800000.0_r_kind
  real(r_kind),parameter:: r25          =   one/25.0_r_kind
  real(r_kind),parameter:: r015         =   0.15_r_kind

  real(r_kind),parameter:: qlth_temp0   = half
  real(r_kind),parameter:: qltv_temp0   = five
  real(r_kind),parameter:: qlth_wind0   = three
  real(r_kind),parameter:: qltv_wind0   = 7.5_r_kind
  real(r_kind),parameter:: qls_rh       = 5.e+05_r_kind

  real(r_kind) scalex1,scalex2,scalex3

  real(r_single),parameter:: EAMPMAX=2.0_r_single
  real(r_single),parameter:: EAMPMIN=0.5_r_single

  integer(i_kind),parameter:: opt_sclclb=izero ! iso scale calibration option
                                             ! 0: isoscale=isoscale *rfact0(ikind)
                                             ! 1: isoscale=isoscale**rfact0(1)+rfact0(2)
                                             ! 2: H:0 / V:1

  integer(i_kind):: mlat
  integer(i_kind),allocatable:: ks(:)
  real(r_kind)  ,allocatable::rfact0h(:),rfact0v(:)
! real(r_kind)  ,allocatable::corz(:,:,:),corp(:),hwll(:,:,:),hwllp(:),vz(:,:,:)
  real(r_kind)  ,allocatable::corz(:,:,:),corp(:,:),hwll(:,:,:),hwllp(:,:),vz(:,:,:)

  real(r_kind)  ,allocatable,dimension(:,:)    :: dxf,dyf,rllatf,hfilter
  real(r_kind)  ,allocatable,dimension(:,:,:)  :: hfine
  real(r_single),allocatable,dimension(:,:,:,:):: aspect
  real(r_single),allocatable,dimension(:,:,:)  :: theta0f,theta0zf,u0f,u0zf,v0f,v0zf,z0f,rh0f ! for regional / zonal patch
  real(r_kind)  ,allocatable,dimension(:,:)    :: asp10f,asp20f,asp30f ! for regional / zonal patch
  real(r_kind)  ,allocatable,dimension(:,:,:)  :: psg

  real(r_single),allocatable,dimension(:,:,:)::tx1_slab,tx2_slab,tx3_slab
  real(r_kind),allocatable::asp1_max(:,:),asp2_max(:,:),asp3_max(:,:)
  real(r_kind),allocatable::qlth_temp(:),qltv_temp(:)
  real(r_kind),allocatable::qlth_wind(:),qltv_wind(:)

!--- For Ensemble Aspect
  integer(i_kind),parameter::ngrds=3_i_kind        !# of supported grids
  integer(i_kind),parameter::nensmax=96_i_kind
  real(r_single),allocatable,dimension(:,:,:,:,:):: aniasp
  real(r_single),allocatable,dimension(:,:,:,:)  :: ensv
  real(r_single),allocatable,dimension(:,:,:)    :: ens0f,ens0zf,ensamp
  real(r_kind)  ,allocatable,dimension(:,:,:,:)  :: enscoeff
  real(r_kind)  ,allocatable,dimension(:,:,:)    :: ensmask
  real(r_kind)  ,allocatable,dimension(:,:,:)    :: gblend
  real(r_kind)  ,allocatable,dimension(:)        :: pgesmin,pgesmax

  real(r_kind):: scalex1ens, scalex2ens, scalex3ens
  real(r_single):: ensamp_mod

  real(r_kind),allocatable :: field_st(:,:,:), field_t(:,:,:)
  integer(i_kind) :: kens_p

!--- Low level amplitude adjustment parameters
! llamp_adjust : flag to use this adjustment or not
! llamp_levtop : model level to start this adjustment
! llamp_coeff  : the lowest level amplitude multiplying factor
  logical         :: llamp_adjust
  integer(i_kind) :: llamp_levtop
  real   (r_kind) :: llamp_coeff

  real(r_kind) hsteep
  logical volpreserve
  logical lsmoothterrain
  logical lwater_scaleinfl
  integer(i_kind) nhscale_pass
  real(r_kind) water_scalefact(10),hsmooth_len
  real(r_kind),allocatable,dimension(:,:,:)::rsliglb      !sea-land-ice mask on analysis grid. type real
  real(r_kind):: rltop,rltop_wind,rltop_temp,rltop_q,rltop_psfc

  integer(i_kind):: nrf3_oz,nrf3_t,nrf3_sf,nrf3_vp,nrf3_q,nrf3_cw
  integer(i_kind):: nrf2_ps,nrf2_sst,nrf2_stl,nrf2_sti

!_RT  integer(i_kind),allocatable,dimension(:) :: nrf2_loc,nrf3_loc  ! should !become local

!-------------------------------------------------------------------------
contains
!-------------------------------------------------------------------------
!    NOAA/NCEP, National Centers for Environmental Prediction GSI        !
!-------------------------------------------------------------------------
!=======================================================================
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
!   2007-05-30  h.liu - remove ozmz, use factoz
!   2007-12-20  sato - replace get2berr with get_aspect_reg_2d, and
!                              get3berr with get_aspect_reg_pt.
!                      These new subroutines only make aspect array.
!                      The pre & post processes to initialize the filter
!                      are almost same, so they are moved back to anprewgt.
!                      Some processes were moved to new subroutine.
!   2010-03-10  zhu  - use nrf* for generalized control variable
!
!   input argument list:
!     mype     - mpi task id
!
!   output argument list:
!
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$ end documentation block
  implicit none

! Declare passed variables
  integer(i_kind),intent(in   ) :: mype

  integer(i_kind):: n,i,j,k,ivar,kvar,k1,l,lp,igauss,nlatf,nlonf,ierr
  real(r_kind):: dl1,dl2,factor,factoz,factk,anhswgtsum
  real(r_kind)  ,allocatable,dimension(:,:):: bckgvar,bckgvar0f,zaux,zsmooth
  real(r_single),allocatable,dimension(:,:):: bckgvar4,zsmooth4
  real(r_single),allocatable,dimension(:,:):: region_dx4,region_dy4,psg4

  character(len=10):: chvarname(10)
  character(len= 4):: clun

!---
! Initialize low level amplitude adjustment parameters
! These parameters might be changed in get_aspect_*
  llamp_adjust = .false.
  llamp_coeff  = 0.6_r_kind
  llamp_levtop = 40_i_kind
!---

  nlatf=pf2aP1%nlatf
  nlonf=pf2aP1%nlonf

! Get indexes to required CV variables
  nrf3_oz   = getindex(cvars3d,'oz')
  nrf3_t    = getindex(cvars3d,'t')
  nrf3_sf   = getindex(cvars3d,'sf')
  nrf3_vp   = getindex(cvars3d,'vp')
  nrf3_q    = getindex(cvars3d,'q')
  nrf3_cw   = getindex(cvars3d,'cw')
  nrf2_ps   = getindex(cvars2d,'ps')
  nrf2_sst  = getindex(cvars2d,'sst')
  nrf2_stl  = getindex(cvarsmd,'stl')
  nrf2_sti  = getindex(cvarsmd,'sti')

  call init_anisofilter_reg(mype)
  call read_bckgstats(mype)

!--- 2dvar subodomain mode -> bypassed to the subroutine for subdomain mode
  if (twodvar_regional.and.rtma_subdomain_option) then
     if(mype==izero) write(6,*) '### 2d auto correlation model - subdomain mode ###'
     call get2berr_reg_subdomain_option(mype)
     return
  end if

!-----------------------------------------------------------------------
!==>allocate variables used to define bckg spatial correlation lengths
!-----------------------------------------------------------------------
  allocate(asp10f(nlatf,nlonf))
  allocate(asp20f(nlatf,nlonf))
  allocate(asp30f(nlatf,nlonf))
  allocate(aspect(7,nlatf,nlonf,nsig1o))
  allocate(hfilter(nlatf,nlonf))
  allocate(hfine(nlat,nlon,nsig1o))
  aspect=zero

!---

  call get_background(mype)

!----------------------------------------------
! Makes anisotropic aspect array
!----------------------------------------------
  if (twodvar_regional) then
     if(mype==izero) write(6,*) '### 2d auto correlation model ###'
     call get_aspect_reg_2d
  else if (ancovmdl==ione) then
     if(mype==izero) write(6,*) '### ens-based auto correlation model ###'
     call get_aspect_reg_ens(mype)
  else
     if(mype==izero) write(6,*) '### pt-based auto correlation model ###'
     call get_aspect_reg_pt(mype)
  endif
  deallocate(asp10f,asp20f,asp30f,ks)

!----------------------------------------------
! Invert aspect array to get true aspect tensor
!----------------------------------------------
  call invert_aspect_tensor(aspect,nlatf,nlonf,nsig1o)

  if(mype==izero) write(6,*)' in anprewgt_reg, nlat,nlon,nlatf,nlonf=', &
                                               nlat,nlon,nlatf,nlonf

  if(mype==izero) write(6,*)' in anprewgt_reg, ids,ide=',indices%ids,indices%ide
  if(mype==izero) write(6,*)' in anprewgt_reg, jds,jde=',indices%jds,indices%jde

  if(lreadnorm) normal=izero
  call init_raf4_wrap(aspect,triad4,ngauss,rgauss,npass,normal,binom,ifilt_ord,filter_all, &
                      nsmooth,nsmooth_shapiro, &
                      nvars,idvar,kvar_start,kvar_end,var_names, &
                      indices,mype, npe)

!----------------------------------------------
! Write/Read filter values
!----------------------------------------------
  if (twodvar_regional) then
     do k=indices%kps,indices%kpe
        ivar=idvar(k)
        chvarname(ivar)=fvarname(ivar)
        open (94,file='fltnorm.dat_'//trim(chvarname(ivar)),form='unformatted')
        if(lreadnorm) then 
           read(94) filter_all(1)%amp(1:ngauss,indices%ips:indices%ipe, & 
                                               indices%jps:indices%jpe,k)
        else               
           write(94) filter_all(1)%amp(1:ngauss,indices%ips:indices%ipe, & 
                                                indices%jps:indices%jpe,k)
        end if
        close(94)
     end do
  else
     write(clun(1:4),'(i4.4)') mype
     open (94,file='fltnorm.dat_'//clun,form='unformatted')
     if(lreadnorm) then; read(94)  filter_all(1)%amp
     else;               write(94) filter_all(1)%amp
     end if
     close(94)
  endif

!----------------------------------------------
! keep original amplitude factor for qoption2 (used in compute_derived)
!----------------------------------------------
  if(qoption==2_i_kind) then
     deallocate(filter_all(2)%amp,stat=ierr)
     allocate(filter_all(2)%amp(ngauss,indices%ips:indices%ipe, &
                               &       indices%jps:indices%jpe, &
                               &       indices%kps:indices%kpe))
     filter_all(2)%amp=filter_all(1)%amp
  end if

!----------------------------------------------
! make covariance map GrADS file
!----------------------------------------------
  if(covmap) call antest_maps0(mype,theta0f,z0f)

  if (twodvar_regional) then
     allocate(bckgvar0f(indices%ips:indices%ipe,indices%jps:indices%jpe))
     bckgvar0f=zero
  end if

!----------------------------------------------
!  filter normalized to unit amplitude.
!  Now adjust amplitude to correspond to desired error amplitude.
! (an_amp are input tuneable error amplitude parameters)
! (corz, corp contain sqrt(error variance) from background error file)
!----------------------------------------------
  factoz = 0.0002_r_kind*r25
  anhswgtsum=sum(anhswgt(1:ngauss))
  do k=indices%kps,indices%kpe
     ivar=idvar(k)
     kvar=k-kvar_start(ivar)+ione
     do k1=1,nsig1o
        if(levs_id(k1)==kvar) exit
     end do

!----------------------------------------------
!   Note: an_amp0 come from namelist anbkgerr
!----------------------------------------------

     if (.not.twodvar_regional) then
        an_amp(:,ivar)=an_amp0(ivar)

!----------------------------------------------
!  low level amplitude adjustment
!----------------------------------------------
        if( llamp_adjust ) then
           if ( llamp_coeff < zero .or. llamp_coeff >= one) then
              print*, 'anprewgt_reg: llamp_coeff (',llamp_coeff,')must be >= 0.0 and < 1.0'
              call stop2(stpcode_namelist)
           end if
           if ( (ivar==nrf3_loc(nrf3_sf).or.ivar==nrf3_loc(nrf3_vp) .or.&
                 ivar==nrf3_loc(nrf3_t) .or.ivar==nrf3_loc(nrf3_q)) .and. (kvar<llamp_levtop) ) then
              an_amp(:,ivar)= an_amp0(ivar) &
                           *(one - ((cos(real(kvar,r_kind)/real(llamp_levtop,r_kind)*pi)+one)*half)*(one-llamp_coeff))
           end if
        end if
     end if

!----------------------------------------------
!  get variance and multiply it to the filter amp
!----------------------------------------------
     do j=indices%jps,indices%jpe
        do i=indices%ips,indices%ipe
           l =max(min(int(rllatf(i,j)),mlat),ione)
           lp=min((l+ione),mlat)
           dl2=rllatf(i,j)-float(l)
           dl1=one-dl2
           if (ivar>nrf) then
              factk=one
           else
              if (nrf_3d(ivar)) then
                 do n=1,nrf3
                    if (nrf3_loc(n)==ivar) then
                       if (n==nrf3_oz) then
                          factk=factoz
                       else
                          factk=dl1*corz(l,kvar,n)+dl2*corz(lp,kvar,n)
                          if ((nrf_var(ivar)=='q' .or. nrf_var(ivar)=='Q') .and. qoption==2_i_kind) & 
                          call fact_qopt2(factk,rh0f(i,j,k1),kvar)
                       end if
                       exit
                    end if
                 end do
              else
                 do n=1,nrf2
                    if (nrf2_loc(n)==ivar) then
                       if (n==nrf2_sst) then
                          factk=zero_3
                       else
                          factk=dl1*corp(l,n)+dl2*corp(lp,n)
                       end if
                       exit
                    end if
                 end do
              end if ! end of nrf_3d
           end if    ! end of ivar       

           do igauss=1,ngauss
              factor=anhswgt(igauss)*factk*an_amp(igauss,ivar)/sqrt(anhswgtsum)
              filter_all(1)%amp(igauss,i,j,k)=factor*filter_all(1)%amp(igauss,i,j,k)
              if (twodvar_regional) bckgvar0f(i,j)=factor            ! for output
              if (allocated(ensamp)) then
                 filter_all(1)%amp(igauss,i,j,k)=filter_all(1)%amp(igauss,i,j,k)*ensamp(i,j,k1)
              end if
           end do

        end do
     end do
  end do

! if (allocated(ensamp)) deallocate(ensamp)

  if (twodvar_regional) then
!----------------------------------------------
! write out a few bckg fields and the error variances. For layer constant
! statistics, these are good for post-processing purposes on the real
! model grid. Interface between filter grid and model domain needed
! for non-constant statistics!
!----------------------------------------------

     allocate(bckgvar (nlat,nlon))
     allocate(bckgvar4(nlat,nlon))

     call fgrid2agrid(pf2aP1,bckgvar0f,bckgvar)
     bckgvar4=bckgvar

     do k=indices%kps,indices%kpe
        ivar=idvar(k)
        open (94,file='bckgvar.dat_'//trim(chvarname(ivar)),form='unformatted')
        write(94) bckgvar4
        close(94)
     enddo

     allocate(zaux(pf2aP1%nlatf,pf2aP1%nlonf))
     allocate(zsmooth (nlat,nlon))
     allocate(zsmooth4(nlat,nlon))

     zaux(:,:)=z0f(:,:,1)
     call fgrid2agrid(pf2aP1,zaux,zsmooth)

     allocate(region_dy4(nlat,nlon),region_dx4(nlat,nlon))
     allocate(psg4(nlat,nlon))
 
     if (mype==izero) then
        region_dx4=region_dx
        region_dy4=region_dy
        open (94,file='bckg_dxdy.dat',form='unformatted')
        write(94) region_dx4,region_dy4
        close(94)

        psg4(:,:)=psg(:,:,1)
        open (94,file='bckg_psfc.dat',form='unformatted')
        write(94) psg4
        close(94)

        zsmooth4(:,:)=zsmooth(:,:)
        open (94,file='bckg_z.dat',form='unformatted')
        write(94) zsmooth4
        close(94)
     end if

     deallocate(bckgvar0f)
     deallocate(bckgvar4)
     deallocate(bckgvar)
     deallocate(region_dy4,region_dx4,psg4)
     deallocate(zaux,zsmooth,zsmooth4)
  end if

! deallocate(corz,corp,hwll,hwllp,vz,aspect)
! deallocate(dxf,dyf,rllatf,theta0f,theta0zf)
  deallocate(corp,hwll,hwllp,vz,aspect)
  deallocate(dxf,dyf,theta0f,theta0zf)
  deallocate(u0f,u0zf,v0f,v0zf,z0f,rh0f,psg,rsliglb)

  deallocate(rfact0h,rfact0v)
  deallocate(hfine,hfilter)

end subroutine anprewgt_reg
!=======================================================================
!=======================================================================
subroutine get_aspect_reg_2d
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    get_aspect_reg_2d
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
!   2007-12-20  sato    - replace get2berr with get_aspect_reg_2d,
!                         in which some procedures are moved
!                         to the parent subroutine anprewgt_reg().
!   2008-11-26  zhu  - use nrf* for generalized control variable
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
  use anberror, only: afact0
  implicit none

! Declare passed variables

! Declare local variables
  integer(i_kind):: i,j,k,k1,ip,im,jp,jm,ivar

  real(r_kind):: fx2,fx1,fx3,dxi,dyi
  real(r_kind):: asp1,asp2,asp3
  real(r_kind):: afact,deta0,deta1

  integer(i_kind):: nlatf,nlonf
  character(len=5) cvar

  nlatf=pf2aP1%nlatf
  nlonf=pf2aP1%nlonf

!-----define the anisotropic aspect tensor-----------------------------
!-------------------------------------------------------------------------------------
! Set up scales

! This first loop for nsig1o will be if we aren't dealing with
! surface pressure, skin temperature, or ozone

  do k=1,nsig1o

     ivar=nvar_id(k)
     k1=levs_id(k)

     if (k1==izero) cycle
     call isotropic_scales(asp10f,asp20f,asp30f,k)

     do j=1,nlonf
        do i=1,nlatf
 
           asp1=asp10f(i,j)
           asp2=asp20f(i,j)
           asp3=asp30f(i,j)

           jp=min(nlonf,j+ione) ; jm=max(ione,j-ione); dxi=one/(jp-jm)
           ip=min(nlatf,i+ione) ; im=max(ione,i-ione); dyi=one/(ip-im)
 
           fx1= dyi*real(z0f(ip,j,k)-z0f(im,j,k),r_kind)
           fx2= dxi*real(z0f(i,jp,k)-z0f(i,jm,k),r_kind)
           fx3= zero

           rltop=rltop_wind
           afact=zero
 
           cvar=nrf_var(ivar)
           select case(cvar)
              case('sf','SF'); rltop=rltop_wind
              case('vp','VP'); rltop=rltop_wind
              case('ps','PS'); rltop=rltop_psfc
              case('t','T'); rltop=rltop_temp
              case('q','Q'); rltop=rltop_q
           end select
           if (jdvar(k) <= 5_i_kind) afact=afact0(ivar)
 
           if (afact>zero) then
              asp1=scalex1*asp1
              asp2=scalex2*asp2
           end if

           aspect(1,i,j,k) = real(one/asp1**2+afact*fx1*fx1/rltop**2,r_single)  ! 1st (y) direction    x1*x1
           aspect(2,i,j,k) = real(one/asp2**2+afact*fx2*fx2/rltop**2,r_single)  ! 2nd (x) direction    x2*x2
           aspect(3,i,j,k) = real(one/asp3**2                       ,r_single)  ! 3rd (z) direction    x3*x3
           aspect(4,i,j,k) = real(            afact*fx3*fx2/rltop**2,r_single)  !  x3*x2
           aspect(5,i,j,k) = real(            afact*fx3*fx1/rltop**2,r_single)  !  x3*x1
           aspect(6,i,j,k) = real(            afact*fx2*fx1/rltop**2,r_single)  !  x2*x1
           aspect(7,i,j,k)=  zero_single
 
           if (volpreserve) then
              deta0=one/(asp1*asp2)**2
              deta1=aspect(1,i,j,k)*aspect(2,i,j,k)-aspect(6,i,j,k)*aspect(6,i,j,k)
              aspect(1,i,j,k) = aspect(1,i,j,k)/sqrt(deta1)*sqrt(deta0)
              aspect(2,i,j,k) = aspect(2,i,j,k)/sqrt(deta1)*sqrt(deta0)
              aspect(6,i,j,k) = aspect(6,i,j,k)/sqrt(deta1)*sqrt(deta0)
           endif

        end do
     end do

     i=nlatf/2
     j=nlonf/2
     asp1=asp10f(i,j)
     asp2=asp20f(i,j)
     asp3=asp30f(i,j)
     asp1=scalex1*asp1
     asp2=scalex2*asp2
     call writeout_isoscaleinfo(ivar,k1,asp1,asp2,asp3,dxf(i,j),dyf(i,j))

  end do

end subroutine get_aspect_reg_2d
!=======================================================================
!=======================================================================
subroutine get_aspect_reg_pt(mype)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    get_aspect_reg_pt
! prgmmr: parrish          org: np22                date: 2005-02-08
!
! abstract: setup everything for anisotropic regional background
!           error (3dvar case, pt-based)
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
!   2007-12-20  sato    - replace get3berr with get_aspect_reg_pt,
!                         in which some procedures are moved
!                         to the parent subroutine anprewgt_reg().
!   2010-03-10  zhu     - use nrf* for generalizing control variable
!   2010-05-28  todling - obtain variable id's on the fly (add getcvi)
!
!   input argument list:
!     mype     - mpi task id
!
!   output argument list:
!
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$ end documentation block
  use anberror, only: afact0
  implicit none

! Declare passed variables
  integer(i_kind),intent(in   ) :: mype

! Declare local variables
  integer(i_kind):: i,j,k,k1
  real(r_kind):: rk1,fblend
  real(r_kind):: fx2,fx1,fx3
  real(r_kind):: asp1,asp2,asp3
  real(r_kind):: qls,ucomp,vcomp,bfact,dxf0,dyf0
  real(r_kind):: afact,qltv,qlth

  integer(i_kind):: nlatf,nlonf

  nlatf=pf2aP1%nlatf
  nlonf=pf2aP1%nlonf

!-----------------------------------------------------------
!-----define the anisotropic aspect tensor------------------
!-----------------------------------------------------------
! Set up scales

! This first loop for nsig1o will be if we aren't dealing with
! surface pressure, skin temperature, or ozone

!----- allocate memories to be used in get_theta_corrl_lenghts()

  allocate(qlth_temp(nsig))
  allocate(qltv_temp(nsig))
  allocate(qlth_wind(nsig))
  allocate(qltv_wind(nsig))

  allocate(asp1_max(nvars,nsig))
  allocate(asp2_max(nvars,nsig))
  allocate(asp3_max(nvars,nsig))

  allocate(tx1_slab(nlatf,nlonf,nsig1o))
  allocate(tx2_slab(nlatf,nlonf,nsig1o))
  allocate(tx3_slab(nlatf,nlonf,nsig1o))

  call get_theta_corrl_lenghts(mype)

  do k=1,nsig1o

     k1=levs_id(k)
     if (k1==izero) cycle
     call isotropic_scales(asp10f,asp20f,asp30f,k)

     do j=1,nlonf
        do i=1,nlatf

           asp1=asp10f(i,j)
           asp2=asp20f(i,j)
           asp3=asp30f(i,j)

           afact=zero
           fx1=one
           fx2=one
           fx3=one

           qlth=one
           qltv=one

           if (nrf3_loc(nrf3_sf)==nvar_id(k) .or. nrf3_loc(nrf3_vp)==nvar_id(k)) then
              qlth=qlth_wind(k1) ; qltv=qltv_wind(k1)
           end if
           if (nrf3_loc(nrf3_t)==nvar_id(k)) then
              qlth=qlth_temp(k1) ; qltv=qltv_temp(k1)
           end if

           if ( (nvar_id(k)==nrf3_loc(nrf3_sf) .or. nvar_id(k)==nrf3_loc(nrf3_vp) &
              .or. nvar_id(k)==nrf3_loc(nrf3_t)) .and. afact0(nvar_id(k))>zero ) then
              afact=real(afact0(nvar_id(k)),r_single)
              asp1=scalex1*asp1
              asp2=scalex2*asp2
              asp3=scalex3*asp3
              fx1=real(tx1_slab(i,j,k),r_kind)
              fx2=real(tx2_slab(i,j,k),r_kind)
              fx3=real(tx3_slab(i,j,k),r_kind)
           endif

           qls=one
           bfact=zero
           ucomp=real(u0f(i,j,k),r_kind)
           vcomp=real(v0f(i,j,k),r_kind)
           dyf0=dyf(nlatf/2,nlonf/2)
           dxf0=dxf(nlatf/2,nlonf/2)

!---
! Note: not effective in the current settings...
           if (nvar_id(k)==nrf3_loc(nrf3_q))  then
!             bfact=one
              qls=qls_rh
              asp1=scalex1*asp1
              asp2=scalex2*asp2
              asp3=scalex3*asp3
           endif

           rk1=float(k1-44_i_kind)
           fblend=half*(one-tanh(rk1))! one

           if (nvar_id(k) /= nrf3_loc(nrf3_q)) then
              aspect(1,i,j,k) = real(one/asp1**2+afact*fblend*fx1*fx1/(qlth**2)  ,r_single) ! 1st (y) direction    x1*x1
              aspect(2,i,j,k) = real(one/asp2**2+afact*fblend*fx2*fx2/(qlth**2)  ,r_single) ! 2nd (x) direction    x2*x2
              aspect(3,i,j,k) = real(one/asp3**2+afact*fblend*fx3*fx3/(qltv**2)  ,r_single) ! 3rd (z) direction    x3*x3
              aspect(4,i,j,k) = real(            afact*fblend*fx3*fx2/(qlth*qltv),r_single) !  x3*x2
              aspect(5,i,j,k) = real(            afact*fblend*fx3*fx1/(qlth*qltv),r_single) !  x3*x1
              aspect(6,i,j,k) = real(            afact*fblend*fx2*fx1/(qlth**2)  ,r_single) !  x2*x1
              aspect(7,i,j,k)=  zero_single
           else if (nvar_id(k)==nrf3_loc(nrf3_q)) then
              aspect(1,i,j,k) = real(one/asp1**2+bfact*fblend*ucomp*ucomp*dyf0*dyf0/qls**2,r_single)
              aspect(2,i,j,k) = real(one/asp2**2+bfact*fblend*vcomp*vcomp*dxf0*dxf0/qls**2,r_single)
              aspect(3,i,j,k) = real(one/asp3**2                                          ,r_single)
              aspect(4,i,j,k) = zero_single
              aspect(5,i,j,k) = zero_single
              aspect(6,i,j,k) = real(           -bfact*fblend*ucomp*vcomp*dyf0*dxf0/qls**2,r_single)
              aspect(7,i,j,k)=  zero_single
           end if

        end do
     end do
  end do


  deallocate(qlth_temp,qltv_temp)
  deallocate(qlth_wind,qltv_wind)

  deallocate(asp1_max,asp2_max,asp3_max)
  deallocate(tx1_slab,tx2_slab,tx3_slab)

end subroutine get_aspect_reg_pt
!=======================================================================
!=======================================================================
subroutine fact_qopt2(factk,rh,kvar)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:   fact_qopt2
! prgmmr: sato             org: np22                date: 2007-12-10
!
! abstract: multiply the RH dependent coefficients for qoption=2
!
! program history log:
!   2007-12-10  sato
!
!   input argument list:
!     factk - base variance for q
!     rh    - guess RH data
!     kvar  -
!   output argument list:
!     factk - corrected variance for q
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$ end documentation block
  implicit none

  real(r_kind)   ,intent(inout) :: factk
  real(r_single) ,intent(in   ) :: rh
  integer(i_kind),intent(in   ) :: kvar

  integer(i_kind):: n,np
  real(r_kind)   :: d,dn1,dn2

  d  =20.0_r_kind * rh + one
  n  =int(d)
  np =n+ione
  dn2=d-float(n)
  dn1=one-dn2
  n =min0(max(ione,n) ,25_i_kind)
  np=min0(max(ione,np),25_i_kind)
  factk=factk*(varq(n,kvar)*dn1+varq(np,kvar)*dn2)    ! qoption=2

  return
end subroutine fact_qopt2
!=======================================================================
!=======================================================================
  character*10 function fvarname(ivar)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    fvarname
!   prgmmr: zhu          org: np23                date: 2008-06-28
!
! abstract: set variable name for fltnorm.dat
!
! program history log:
!   2008-11-28  zhu
!
!   input argument list:
!
!   output argument list:
!
! remarks:
!   - this routine is a bottle neck. Must vars really be renamed? (Todling)
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp
!
!$$$

  implicit none

! Declar passed variables
  integer(i_kind) ivar

  if (ivar==nrf3_loc(nrf3_sf)) fvarname='psi'
  if (ivar==nrf3_loc(nrf3_vp)) fvarname='chi'
  if (ivar==nrf3_loc(nrf3_t))  fvarname='t'
  if (ivar==nrf3_loc(nrf3_q))  fvarname='pseudorh'
  if (nrf3_oz>0.and.ivar==nrf3_loc(nrf3_oz)) fvarname='oz'
  if (nrf3_cw>0.and.ivar==nrf3_loc(nrf3_cw)) fvarname='qw'
  if (ivar==nrf2_loc(nrf2_ps)) fvarname='lnps'
  if (nrf2_sst>0.and.ivar==nrf2_loc(nrf2_sst)) fvarname='sst'
  if (nrf2_sst>0.and.ivar==nrf+1) fvarname='lst'   ! _RTod this is a disaster!
  if (nrf2_sst>0.and.ivar==nrf+2) fvarname='ist'   ! _RTod this is a disaster!

  return
end function fvarname

!=======================================================================
subroutine init_anisofilter_reg(mype)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:   init_anisofilter_reg
! prgmmr: pondeca          org: np22                date: 2006-08-01
!
! abstract: initialize anisotropic background error related variables
!
! program history log:
!   2006-08-01  pondeca
!   2010-03-31  zhu - make changes using nvars,nrf* for generaling control variable
!
!   input argument list:
!    mype
!
!   output argument list:
!
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$ end documentation block
  use anberror, only: afact0
  implicit none

! Declare passed variables
  integer(i_kind),intent(in   ) :: mype

! Declare local variables
  integer(i_kind):: i,n
  logical:: fexist
  integer(i_kind)           :: nlatf,nlonf

  real(r_double) svpsi,svchi,svpsfc,svtemp,svshum
  real(r_kind) sclpsi,sclchi,sclpsfc,scltemp,sclhum
  real(r_kind) water_scalefactpsi,water_scalefactchi,water_scalefacttemp, &
               water_scalefactq,water_scalefactpsfc

  namelist/parmcardanisof/latdepend,scalex1,scalex2,scalex3,afact0,hsteep, &
          lsmoothterrain,hsmooth_len,volpreserve, &
          lwater_scaleinfl,water_scalefactpsi,water_scalefactchi, &
          water_scalefacttemp,water_scalefactq,water_scalefactpsfc, &
          nhscale_pass, &
          rltop_wind,rltop_temp,rltop_q,rltop_psfc, &
          svpsi,svchi,svpsfc,svtemp,svshum, &
          sclpsi,sclchi,sclpsfc,scltemp,sclhum
!*******************************************************************

  nlatf=pf2aP1%nlatf
  nlonf=pf2aP1%nlonf

  latdepend=.true.
  if(twodvar_regional) then
     scalex1=one
     scalex2=one
     scalex3=one
  else
     scalex1=1.2_r_kind
     scalex2=1.2_r_kind
     scalex3=1.2_r_kind
  end if
  anhswgt(:)=one

!-----------------------------------------------------------------------
!==>prescribe scaling factors for spatial correlation lengths.
!   used in the 3dvar case only
!-----------------------------------------------------------------------

  allocate (rfact0h(nvars))
  allocate (rfact0v(nvars))

!--- fine tuning with rgauss tuning
  if(.not.twodvar_regional) then
     if(opt_sclclb==izero) then
        do n=1,nrf
           select case(nrf_var(n))
              case('sf','SF'); rfact0h(n)=0.90_r_kind;  rfact0v(n)=1.60_r_kind
              case('vp','VP'); rfact0h(n)=one        ;  rfact0v(n)=1.60_r_kind
              case('t','T')  ; rfact0h(n)=1.40_r_kind;  rfact0v(n)=1.40_r_kind
              case('q','Q')  ; rfact0h(n)=1.40_r_kind;  rfact0v(n)=one
              case('oz','OZ'); rfact0h(n)=1.00_r_kind;  rfact0v(n)=1.20_r_kind
              case('cw','CW'); rfact0h(n)=1.40_r_kind;  rfact0v(n)=1.20_r_kind
              case('ps','PS'); rfact0h(n)=1.40_r_kind;  rfact0v(n)=1.40_r_kind
              case('sst','SST'); rfact0h(n)=one  ;  rfact0v(n)=1.20_r_kind;
                                 rfact0h(nrf+1)=one    ;  rfact0v(nrf+1)=1.20_r_kind;
                                 rfact0h(nrf+2)=one    ;  rfact0v(nrf+2)=1.20_r_kind
           end select
        end do
     else if(opt_sclclb==ione) then
        rfact0h(1)=1.20_r_kind   ;  rfact0v(1)=1.20_r_kind
        rfact0h(2)=0.20_r_kind   ;  rfact0v(2)=0.20_r_kind
     end if

!---
  else

     do n=1,nrf
        select case(nrf_var(n))
           case('sf','SF'); rfact0h(n)=one;  rfact0v(n)=1.50_r_kind
           case('vp','VP'); rfact0h(n)=one;  rfact0v(n)=1.50_r_kind
           case('t','T')  ; rfact0h(n)=1.5_r_kind;  rfact0v(n)=1.50_r_kind
           case('q','Q')  ; rfact0h(n)=1.5_r_kind;  rfact0v(n)=1.25_r_kind
           case('oz','OZ'); rfact0h(n)=one;  rfact0v(n)=1.25_r_kind
           case('cw','CW'); rfact0h(n)=one;  rfact0v(n)=1.25_r_kind
           case('ps','PS'); rfact0h(n)=1.2_r_kind;  rfact0v(n)=1.50_r_kind 
           case('sst','SST'); rfact0h(n)=one    ;  rfact0v(n)=1.25_r_kind;
                              rfact0h(nrf+1)=one;  rfact0v(nrf+1)=1.25_r_kind;
                              rfact0h(nrf+2)=one;  rfact0v(nrf+2)=1.25_r_kind
        end select
     end do

     afact0=one     !(use "zero" for isotropic computations)
     hsteep=zero
     hsmooth_len=10._r_kind
     lsmoothterrain=.false.
     volpreserve=.false.
     lwater_scaleinfl=.false.
     water_scalefactpsi=one
     water_scalefactchi=one
     water_scalefacttemp=one
     water_scalefactq=one
     water_scalefactpsfc=one
     nhscale_pass=izero

     rltop_wind=huge(rltop_wind)
     rltop_temp=huge(rltop_temp)
     rltop_q=huge(rltop_q)
     rltop_psfc=huge(rltop_psfc)

     svpsi =0.35_r_double
     svchi =0.35_r_double*2.063_r_double
     svpsfc=0.70_r_double*two
     svtemp=one*two
     svshum=half*1.5_r_double*two

     sclpsi =0.3_r_kind*75._r_kind/100._r_kind*1.2_r_kind
     sclchi =0.3_r_kind*75._r_kind/100._r_kind*1.2_r_kind
     sclpsfc=0.3_r_kind*75._r_kind/100._r_kind*1.2_r_kind
     scltemp=0.3_r_kind*1.2_r_kind
     sclhum =0.3_r_kind*1.2_r_kind

     water_scalefact(:)=one

     inquire(file='parmcard_input',exist=fexist)
     if (fexist) then
        open(55,file='parmcard_input',form='formatted')
        read(55,parmcardanisof)
        close(55)
     endif

          !Rescaling of bckg error variances
          !Rescaling of correlation lengths
          !Rescaling of correlation lengths over water only
     an_amp=0.60_r_double
     rfact0h=one
     do n=1,nrf
        select case(nrf_var(n))
           case('sf','SF') 
              an_amp(:,n) =svpsi
              rfact0h(n)=sclpsi
              water_scalefact(n)=water_scalefactpsi
           case('vp','VP')
              an_amp(:,n) =svchi
              rfact0h(n)=sclchi
              water_scalefact(n)=water_scalefactchi
           case('t','T')  
              an_amp(:,n) =svtemp
              rfact0h(n)=scltemp
              water_scalefact(n)=water_scalefacttemp
           case('q','Q')  
              an_amp(:,n) =svshum
              rfact0h(n)=sclhum
              water_scalefact(n)=water_scalefactq
           case('oz','OZ'); an_amp(:,n) =0.80_r_double
           case('ps','PS')
              an_amp(:,n) =svpsfc
              rfact0h(n)=sclpsfc
              water_scalefact(n)=water_scalefactpsfc
        end select
     end do

     if (mype==izero) then
        print*,'in init_anisofilter_reg: hsteep=',hsteep
        print*,'in init_anisofilter_reg: hsmooth_len=',hsmooth_len
        print*,'in init_anisofilter_reg: lsmoothterrain=',lsmoothterrain
        print*,'in init_anisofilter_reg: volpreserve=',volpreserve
        print*,'in init_anisofilter_reg: lwater_scaleinfl=',lwater_scaleinfl
        print*,'in init_anisofilter_reg: water_scalefactpsi=',water_scalefactpsi
        print*,'in init_anisofilter_reg: water_scalefactchi=',water_scalefactchi
        print*,'in init_anisofilter_reg: water_scalefactpsfc=',water_scalefactpsfc
        print*,'in init_anisofilter_reg: water_scalefacttemp=',water_scalefacttemp
        print*,'in init_anisofilter_reg: water_scalefactq=',water_scalefactq

        print*,'in init_anisofilter_reg: latdepend=',latdepend
        print*,'in init_anisofilter_reg: scalex1=',scalex1
        print*,'in init_anisofilter_reg: scalex2=',scalex2
        print*,'in init_anisofilter_reg: scalex3=',scalex3

        print*,'in init_anisofilter_reg: rltop_wind=',rltop_wind
        print*,'in init_anisofilter_reg: rltop_temp=',rltop_temp
        print*,'in init_anisofilter_reg: rltop_q=',rltop_q
        print*,'in init_anisofilter_reg: rltop_psfc=',rltop_psfc

        print*,'in init_anisofilter_reg: svpsi=',svpsi
        print*,'in init_anisofilter_reg: svchi=',svchi
        print*,'in init_anisofilter_reg: svpsfc=',svpsfc
        print*,'in init_anisofilter_reg: svtemp=',svtemp
        print*,'in init_anisofilter_reg: svshum=',svshum

        print*,'in init_anisofilter_reg: sclpsi=',sclpsi
        print*,'in init_anisofilter_reg: sclchi=',sclchi
        print*,'in init_anisofilter_reg: sclpsfc=',sclpsfc
        print*,'in init_anisofilter_reg: scltemp=',scltemp
        print*,'in init_anisofilter_reg: sclhum=',sclhum

        print*,'in init_anisofilter_reg: nhscale_pass=',nhscale_pass
     endif
  endif

  if (mype==izero) then
     do i=1,nvars
        print*,'in init_anisofilter_reg: i,rfact0h,rfact0v,afact0=',i,rfact0h(i),rfact0v(i),afact0(i)
     enddo
  endif

!
!-----------------------------------------------------------------------
!==>get dxf,dyf,rllatf
!-----------------------------------------------------------------------
! note: if filter grid coarser than analysis grid, then normalized
! adjoint of filter to analysis interpolation used to transfer fields
! from analysis grid to filter grid. otherwise, normal interpolation
! is done. this is transparent at this level. it appears in the
! definition of the interpolation and adjoint of interpolation
! weights. check for accuracy.(done).

  allocate(dxf(nlatf,nlonf),dyf(nlatf,nlonf),rllatf(nlatf,nlonf))

  call agrid2fgrid(pf2aP1,region_dx,dxf)
  dxf=pf2aP1%grid_ratio_lon*dxf             !  note that dxf = grid_ratio_lon*dx
  call agrid2fgrid(pf2aP1,region_dy,dyf)
  dyf=pf2aP1%grid_ratio_lat*dyf             !  note that dyf = grid_ratio_lat*dy
  call agrid2fgrid(pf2aP1,rllat,rllatf)

  if(mype==izero) then
     write(6,*)'in anisofilter_reg, nlatf,nlonf=',nlatf,nlonf
     write(6,*)'in anisofilter_reg, min,max(rllat)=',minval(rllat),maxval(rllat)
     write(6,*)'in anisofilter_reg, min,max(rllatf)=',minval(rllatf),maxval(rllatf)
     write(6,*)'in anisofilter_reg, min,max(grid_ratio_lon*dx)=', &
                   minval(pf2aP1%grid_ratio_lon*region_dx), &
                   maxval(pf2aP1%grid_ratio_lon*region_dx)
     write(6,*)'in anisofilter_reg, min,max(dxf)=',minval(dxf),maxval(dxf)
     write(6,*)'in anisofilter_reg, min,max(grid_ratio_lat*dy)=', &
                   minval(pf2aP1%grid_ratio_lat*region_dy), &
                   maxval(pf2aP1%grid_ratio_lat*region_dy)
     write(6,*)'in anisofilter_reg, min,max(dyf)=',minval(dyf),maxval(dyf)
 
  end if

!_RT soon
! allocate(nrf3_loc(nrf3),nrf2_loc(nrf2))
! do n=1,nrf3
!    nrf3_loc(n)=getindex(nrf_var,cvars3d(n))
! enddo
! do n=1,nrf2
!    nrf2_loc(n)=getindex(nrf_var,cvars2d(n))
! enddo

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
!   2008-15-25  zhu  - make changes for generalized control variables
!                    - change structure of background error file
!                    - varq was moved to berror_read_wgt_reg
!
!   input argument list:
!    mype     - mpi task id
!
!   output argument list:
!
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$ end documentation block

  use m_berror_stats_reg, only: berror_get_dims_reg,berror_read_wgt_reg
  implicit none

! Declare passed variables
  integer(i_kind),intent(in   ) :: mype

! Declare local variables
  integer(i_kind):: j,k,l,n,inerr
  integer(i_kind):: msig,kds,kde

  real(r_kind),allocatable:: rlsig(:),dlsig(:)
  real(r_kind),allocatable:: vzimax(:,:),vzimin(:,:),vziavg(:,:)
  real(r_kind):: psfc015
  real(r_kind),allocatable:: corzavg(:,:),hwllavg(:,:)
  real(r_kind),allocatable:: corpavg(:),hwllpavg(:)

! Read dimension of stats file
  inerr=22_i_kind
  call berror_get_dims_reg(msig,mlat,inerr)

! Allocate arrays in stats file
  allocate ( corz(1:mlat,1:nsig,1:nrf3) )
  allocate ( corp(1:mlat,nrf2) )
  allocate ( hwll(0:mlat+ione,1:nsig,1:nrf3),hwllp(0:mlat+ione,nvars-nrf3) )
  allocate ( vz(1:nsig,0:mlat+ione,1:nrf3) )

  allocate(rlsig(nsig))
  allocate(dlsig(nsig))

! Read in background error stats and interpolate in vertical
! to that specified in namelist
  call berror_read_wgt_reg(msig,mlat,corz,corp,hwll,hwllp,vz,rlsig,mype,inerr)

  if(mype==izero) write(6,*)'in read_bckgstats,mlat=',mlat

! Normalize vz with del sigma and convert to vertical grid units!
  if(.not.twodvar_regional) then
     dlsig(1)=rlsig(1)-rlsig(2)
     do k=2,nsig-ione
        dlsig(k)=half*(rlsig(k-ione)-rlsig(k+ione))
     end do
     dlsig(nsig)=rlsig(nsig-ione)-rlsig(nsig)
  else
     dlsig=one ! Really no meaning for 2dvar.  Set to 1.0 to avoid
               ! division by zero below.  Array vz is reset for 2dvar
               ! case, so vz calculation below is not truly need.
  end if

  do k=1,nsig
     vz(k,0:mlat+ione,1:nrf3)=vz(k,0:mlat+ione,1:nrf3)*dlsig(k)
  end do

  deallocate(rlsig)
  deallocate(dlsig)

!----- apply scaling to vertical length scales.
!      note:  parameter vs needs to be inverted

  if(mype==izero) write(6,*)'in read_bckgstats,an_vs=',an_vs
  an_vs=one/an_vs
  vz=vz/an_vs
  if (twodvar_regional) vz(1:nsig,0:mlat+ione,1:nrf3)=sqrt(one)


!-----compute and print out diagnostics for
!     vertical length scales
  allocate(vzimax(1:nsig,1:nrf3))
  allocate(vzimin(1:nsig,1:nrf3))
  allocate(vziavg(1:nsig,1:nrf3))
  do n=1,nrf3
     do k=1,nsig
        vzimax(k,n)=maxval(one/vz(k,0:mlat+ione,n))
        vzimin(k,n)=minval(one/vz(k,0:mlat+ione,n))
        vziavg(k,n)=sum((one/vz(k,0:mlat+ione,n)))/float(mlat+2_i_kind)
     end do
     if(mype==izero) then
        do k=1,nsig
           write(6,'(" var,k,max,min,avg vert corlen =",2i4,3f11.3)') &
                       n,k,vzimax(k,n),vzimin(k,n),vziavg(k,n)
        end do
     end if
  end do

!-----optionally, remove latitudinal dependence from statistics
  if (.not.latdepend) then

     allocate(corzavg(1:nsig,1:nrf3))
     allocate(hwllavg(1:nsig,1:nrf3))
     allocate(corpavg(1:nrf2))
     allocate(hwllpavg(1:nrf2))

     do n=1,nrf3
        do k=1,nsig
           corzavg(k,n)=sum(corz(1:mlat,k,n))/float(mlat)
           hwllavg(k,n)=sum(hwll(0:mlat+ione,k,n))/float(mlat+2_i_kind)
        end do
     end do
     do n=1,nrf2
        corpavg(n)=sum(corp(1:mlat,n))/float(mlat)
        hwllpavg(n)=sum(hwllp(0:mlat+ione,n))/float(mlat+2_i_kind)
     end do

     do j=1,mlat
        corz(j,1:nsig,1:nrf3)=corzavg(1:nsig,1:nrf3)
        corp(j,1:nrf2)=corpavg(1:nrf2)
     end do
     do j=0,mlat+ione
        hwll(j,1:nsig,1:nrf3)=hwllavg(1:nsig,1:nrf3)
        hwllp(j,1:nrf2)=hwllpavg(1:nrf2)
        vz(1:nsig,j,1:nrf3)=one/vziavg(1:nsig,1:nrf3)
     end do

     deallocate(corzavg)
     deallocate(hwllavg)
     deallocate(corpavg)
     deallocate(hwllpavg)

  end if

  deallocate(vzimax)
  deallocate(vzimin)
  deallocate(vziavg)

! hybrid sigma level structure calculated in rdgstat_reg
! ks used to load constant horizontal scales for SF/VP
! above sigma level 0.15
! loop l for diff variable of each PE.

  psfc015=r015*ges_psfcavg
  if (twodvar_regional.and.rtma_subdomain_option) then
     kds=indices%kds; kde=indices%kde
     allocate (ks(kds:kde))
     do l=kds,kde
        ks(l)=nsig+ione
        if(jdvar(l)<3_i_kind)then
           k_loop0: do k=1,nsig
              if (ges_prslavg(k)< psfc015) then
                 ks(l)=k
                 exit k_loop0
              end if
           end do k_loop0
        end if
     end do
  else
     allocate (ks(nsig1o))
     do l=1,nsig1o
        ks(l)=nsig+ione
        if(nvar_id(l)<3_i_kind)then
           k_loop: do k=1,nsig
              if (ges_prslavg(k)< psfc015) then
                 ks(l)=k
                 exit k_loop
              end if
           end do k_loop
        end if
     end do
  end if

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
!   2007-12-20  sato - add rh field, which will be used when qoption=2
!
!   input argument list:
!    mype     - mpi task id
!
!   output argument list:
!
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$ end documentation block
  use sub2fslab_mod, only: setup_sub2fslab, destroy_sub2fslab, &
                           sub2fslab, sub2fslabdz, sub2fslab2d, sub2slab2d
  use guess_grids, only: isli2
  implicit none

! Declare passed variables
  integer(i_kind),intent(in   ) :: mype

! Declare local variables
  integer(i_kind) i,j,k,l,mm1,k1,ivar

  real(r_kind) hwll_loc
  real(r_kind) asp1,asp2,asp3

  real(r_kind),allocatable,dimension(:,:,:)::field
  logical:: ice

  integer(i_long):: ngauss_smooth,npass_smooth,normal_smooth,ifilt_ord_smooth
  integer(i_long):: nsmooth_smooth,nsmooth_shapiro_smooth
  real(r_double) :: rgauss_smooth(1)

  integer(i_kind):: nlatf,nlonf,it,iderivative

  nlatf=pf2aP1%nlatf
  nlonf=pf2aP1%nlonf
  mm1=mype+ione

!=========================================================================

!  convert all basic variables from subdomain to slab mode and interpolate to filter grid
!  then repeat this process for all vertical derivatives of subdomain variables

  allocate(field(lat2,lon2,nsig))

  allocate(theta0f (nlatf,nlonf,nsig1o),theta0zf(nlatf,nlonf,nsig1o))
  allocate(u0f(nlatf,nlonf,nsig1o),u0zf(nlatf,nlonf,nsig1o))
  allocate(v0f(nlatf,nlonf,nsig1o),v0zf(nlatf,nlonf,nsig1o))
  allocate(z0f(nlatf,nlonf,nsig1o),rh0f(nlatf,nlonf,nsig1o))
  allocate(psg(nlat ,nlon ,nsig1o),rsliglb(nlat,nlon,nsig1o))

  it=ntguessig

!-----------------------------------------------
! background values for tv,u,v,z, and rh
!-----------------------------------------------
  call setup_sub2fslab

  !-------------
  ! T
  field(:,:,:)=ges_tv(:,:,:,it)/(ges_prsl(:,:,:,it)/r100)**rd_over_cp

  call sub2fslab  (field,theta0f )
  call sub2fslabdz(field,theta0zf)

  !-------------
  ! U
  call sub2fslab  (ges_u(1,1,1,it),u0f )
  call sub2fslabdz(ges_u(1,1,1,it),u0zf)

  !-------------
  ! V
  call sub2fslab  (ges_v(1,1,1,it),v0f )
  call sub2fslabdz(ges_v(1,1,1,it),v0zf)

  !-------------
  ! Z
  do j=1,lon2
     do i=1,lat2
        if (min(max(isli2(i,j),izero),ione)==izero) then
           field(i,j,1)=ges_z(i,j,it)-hsteep
        else
           field(i,j,1)=ges_z(i,j,it)
        end if
     end do
  end do
  call sub2fslab2d(field(1,1,1),z0f )

  if(nsig1o>ione) then
     do j=1,nlonf
        do i=1,nlatf
           z0f(i,j,2:nsig1o)=z0f(i,j,1)
        end do
     end do
  endif

  !-------------
  ! RH
  ice=.true.
  iderivative=0
  call genqsat(field,ges_tsen(1,1,1,it),ges_prsl(1,1,1,it),lat2,lon2,nsig,ice,iderivative)
  field(:,:,:)=ges_q(:,:,:,it)/field(:,:,:)

  call sub2fslab(field,rh0f)

  !-------------
  ! PS (2d full grid)
  field(:,:,1)=1000.0_r_single*ges_ps(:,:,it)
  call sub2slab2d(field(1,1,1),psg)

  !-------------
  ! LandSea Mask (2d full grid)
  field(:,:,1)=real(isli2(:,:),r_kind)
  call sub2slab2d(field(1,1,1),rsliglb)

  call destroy_sub2fslab
  deallocate(field)

!-----------------------------------------------------------
!-------end of getting background variables on filter grid--
!-----------------------------------------------------------

! ------------------------------------------------------------
! ------------ in this section, set up isotropic filter for
! ------------ generating smoothed guess
! ------------------------------------------------------------

  do k=1,nsig1o
     k1=levs_id(k)
     if (k1==izero) cycle

     do j=1,nlonf
        do i=1,nlatf
           if (twodvar_regional) then
              aspect(1,i,j,k)=real(hsmooth_len**2/pf2aP1%grid_ratio_lat,r_single)
              aspect(2,i,j,k)=real(hsmooth_len**2/pf2aP1%grid_ratio_lon,r_single)
              aspect(3,i,j,k)=real( one**2                             ,r_single)
              aspect(4:7,i,j,k)=zero_single
           else
              ivar=nrf3_loc(nrf3_t)
              l=int(rllatf(nlatf/2,nlonf/2))
              hwll_loc=hwll(l,k1,nrf3_t)
              asp1=hwll_loc/min(dyf(nlatf/2,nlonf/2),dxf(nlatf/2,nlonf/2))*rfact0h(4)
              asp2=asp1
              asp3=one/vz(k1,l,nrf3_t)*rfact0v(ivar)
              aspect(1,i,j,k)=real(asp1**2,r_single)
              aspect(2,i,j,k)=real(asp2**2,r_single)
              aspect(3,i,j,k)=real(asp3**2,r_single)
              aspect(4:7,i,j,k)=zero_single
           end if
        end do
     end do
  end do

  ngauss_smooth=ione
  rgauss_smooth=one
  npass_smooth =ione
  normal_smooth=izero
  ifilt_ord_smooth=4_i_kind
  nsmooth_smooth=izero
  nsmooth_shapiro_smooth=izero

  call init_raf4_wrap(aspect,triad4,ngauss_smooth,rgauss_smooth, &
                     npass_smooth,normal_smooth,binom, &
                     ifilt_ord_smooth,filter_all, &
                     nsmooth_smooth,nsmooth_shapiro_smooth, &
                     nvars,idvar,kvar_start,kvar_end,var_names, &
                     indices, mype, npe)

  call raf_sm_reg(theta0f ,ngauss_smooth)
  call raf_sm_reg(theta0zf,ngauss_smooth)
  call raf_sm_reg(u0f ,ngauss_smooth)
  call raf_sm_reg(u0zf,ngauss_smooth)
  call raf_sm_reg(v0f ,ngauss_smooth)
  call raf_sm_reg(v0zf,ngauss_smooth)

  if( (twodvar_regional.and.lsmoothterrain) .or. (.not.twodvar_regional)) &
     call raf_sm_reg(z0f ,ngauss_smooth)

  call raf_sm_reg(rh0f,ngauss_smooth)

end subroutine get_background
!=======================================================================
!=======================================================================
subroutine raf_sm_reg(fslb0,ngauss_smooth)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    raf_sm_reg
!   prgmmr:
!
! abstract:
!
! program history log:
!   2009-09-16  lueken - added subprogram doc block
!
!   input argument list:
!    ngauss_smooth
!    fslb0
!
!   output argument list:
!    fslb0
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block
  implicit none

  integer(i_long),intent(in   ) :: ngauss_smooth

  real(r_single) ,intent(inout) :: fslb0(pf2aP1%nlatf,pf2aP1%nlonf)

  !--- Smoother for regional
  call raf_sm4_wrap   (fslb0,filter_all,ngauss_smooth,indices,npe)
  call raf_sm4_ad_wrap(fslb0,filter_all,ngauss_smooth,indices,npe)

  return
end subroutine
!=======================================================================
!=======================================================================
subroutine isotropic_scales(scale1,scale2,scale3,k)
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
!   2010-03-10  zhu    - add changes for generalized control variables
!
!   input argument list:
!    k        - level number of field in slab mode
!
!   output argument list:
!    scale1     - 2d field of correlations lengths in the x-direction
!    scale2     - 2d field of correlations lengths in the y-direction
!    scale3     - 2d field of correlations lengths in the vertical direction
!
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$ end documentation block
  implicit none

! Declare passed variables
  integer(i_kind),intent(in   ) :: k

  real(r_kind)   ,intent(  out) :: scale1(pf2aP1%nlatf,pf2aP1%nlonf)
  real(r_kind)   ,intent(  out) :: scale2(pf2aP1%nlatf,pf2aP1%nlonf)
  real(r_kind)   ,intent(  out) :: scale3(pf2aP1%nlatf,pf2aP1%nlonf)

! Declare local variables
  integer(i_kind) i,j,k1,ivar,l,lp,n,nn

  real(r_kind) dl1,dl2,hwll_loc,cc
  real(r_kind) scaleaux1(pf2aP1%nlata,pf2aP1%nlona)
  real(r_kind) scaleaux2(pf2aP1%nlata,pf2aP1%nlona)
  integer(i_kind) nlatf,nlonf

  nlatf=pf2aP1%nlatf
  nlonf=pf2aP1%nlonf

  k1=levs_id(k)
  ivar=nvar_id(k)

  do j=1,nlonf
     do i=1,nlatf

!       3d analysis variables
        nn=-ione
        do n=1,nrf3
           if (nrf3_loc(n)==ivar) then
              nn=n
              if (nn==nrf3_oz) then  
                 if(k1 <= nsig*3/4)then
                    hwll_loc=r400000
                 else
                    hwll_loc=(r800000-r400000*(nsig-k1)/(nsig-nsig*3/4))
                 end if
                 l=int(rllatf(nlatf/2,nlonf/2))
                 scale3(i,j)=one/vz(k1,l,nn)
              else
                 if(k1 >= ks(k))then
                    l=int(rllatf(nlatf/2,nlonf/2))
                    hwll_loc=hwll(l,k1,nn)
                 else
                    l =max(min(int(rllatf(i,j)),mlat),ione)
                    lp=min((l+ione),mlat)
                    dl2=rllatf(i,j)-float(l)
                    dl1=one-dl2
                    hwll_loc=dl1*hwll(l,k1,nn)+dl2*hwll(lp,k1,nn)
                 end if
                 scale3(i,j)=one/vz(k1,l,nn)
              end if
              exit
           end if
        end do

        if (nn==-ione) then
           do n=1,nrf2
              if (nrf2_loc(n)==ivar .or. ivar>nrf) then
                 nn=n
                 if (ivar>nrf) nn=ivar-nrf3

                 cc=one
                 if (nn==nrf2_sst) cc=half
                 if (ivar>nrf) cc=quarter 
                 l =max(min(int(rllatf(i,j)),mlat),ione)
                 lp=min((l+ione),mlat)
                 dl2=rllatf(i,j)-float(l)
                 dl1=one-dl2
                 hwll_loc=cc*(dl1*hwllp(l,nn)+dl2*hwllp(lp,nn))
                 scale3(i,j)=one
                 exit
              end if
           end do
        end if

        scale1(i,j)=hwll_loc/dyf(i,j)
        scale2(i,j)=hwll_loc/dxf(i,j)
 
        if (.not.latdepend) then
           scale1(i,j)=max(scale1(i,j),scale2(i,j))
           scale2(i,j)=scale1(i,j)
        endif
 
        !rescaling to roughly match original analysis from purely isotropic
        !option, ie.. when anisotropic=.false. in namelist "anbkgerr".
 
        if(opt_sclclb==izero) then
           scale1(i,j)=rfact0h(nvar_id(k))*scale1(i,j)
           scale2(i,j)=rfact0h(nvar_id(k))*scale2(i,j)
        else if(opt_sclclb==ione) then
           scale1(i,j)=scale1(i,j)**rfact0h(1)+rfact0h(2)
           scale2(i,j)=scale2(i,j)**rfact0h(1)+rfact0h(2)
        end if
 
        if (.not.twodvar_regional) then
           if (nrf_3d(ivar)) then
              if(opt_sclclb==izero) then
                 scale3(i,j)=rfact0v(nvar_id(k))*scale3(i,j)
              else if(opt_sclclb==ione) then
                 scale3(i,j)=scale3(i,j)**rfact0v(1)+rfact0v(2)
              end if
           end if
        end if
     enddo
  enddo

  if (lwater_scaleinfl .and. water_scalefact(nvar_id(k))/=one) then
     call fgrid2agrid(pf2aP1,scale1,scaleaux1)
     call fgrid2agrid(pf2aP1,scale2,scaleaux2)
     do j=1,nlon
        do i=1,nlat
           if (min(max(anint(rsliglb(i,j,k)),zero),one)==zero) then
              scaleaux1(i,j)=water_scalefact(nvar_id(k))*scaleaux1(i,j)
              scaleaux2(i,j)=water_scalefact(nvar_id(k))*scaleaux2(i,j)
           end if
        end do
     end do
     if (nhscale_pass>izero) then
        call smther_one_8(scaleaux1,ione,nlat,ione,nlon,nhscale_pass)
        call smther_one_8(scaleaux2,ione,nlat,ione,nlon,nhscale_pass)
     end if
     call agrid2fgrid(pf2aP1,scaleaux1,scale1)
     call agrid2fgrid(pf2aP1,scaleaux2,scale2)
  end if

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
!   2007-12-20  sato - move the grad(pt) part to the other subroutine
!   2010-03-10  zhu  - make changes using nrf* for generalizing cv
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
!$$$  end documentation block
  implicit none

! Declare passed variables
  integer(i_kind),intent(in   ) :: mype

! Declare local variables
  integer(i_kind) i,j,k,kp,km,k1
  integer(i_kind) mcount0,mcount

  real(r_kind) dzi
  real(r_kind) asp1,asp2,asp3

  real(r_kind) pbar4a,pbar4(nsig),hgt4(nsig),tbar4(nsig), &
               thetabar4(nsig),dthetabarz(nsig),dthetabarzmax, &
               qltv,qlth

  integer(i_kind) nlatf,nlonf,it

  nlatf=pf2aP1%nlatf
  nlonf=pf2aP1%nlonf

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
     call mpi_allreduce(pbar4a,pbar4(k),ione,mpi_real8,mpi_sum,mpi_comm_world,ierror)
     call mpi_allreduce(mcount0,mcount,ione,mpi_integer4,mpi_sum,mpi_comm_world,ierror)
     pbar4(k)=pbar4(k)/float(mcount)
     if(mype==izero) write(6,*)'in get_theta_corrl_lenghts,k,pbar4=',k,pbar4(k)
     call w3fa03(pbar4(k),hgt4(k),tbar4(k),thetabar4(k))
  end do

  dthetabarzmax=zero
  do k=1,nsig
     kp=min(nsig,k+ione)
     km=max(ione,k-ione)
     dzi=one/(kp-km)
     dthetabarz(k)=dzi*(thetabar4(kp)-thetabar4(km))
     dthetabarzmax=max(dthetabarz(k),dthetabarzmax)
     if(mype==izero) then
        write(6,'("in get_theta_corrl_lenghts,k,pbar4,hgt4,tbar4=",i4,3f11.3)') k,pbar4(k),hgt4(k),tbar4(k)
        write(6,'("in get_theta_corrl_lenghts,k,thetabar4,dthetabarz=",i4,2f11.3)') k,thetabar4(k),dthetabarz(k)
     endif
  end do
  if(mype==izero) write(6,*)'in get_theta_corrl_lenghts,dthetabarzmax=',dthetabarzmax

  do k=1,nsig
     dthetabarz(k)=dthetabarz(k)/dthetabarzmax
     if(mype==izero) then
        write(6,*)'in get_theta_corrl_lenghts,k,normalized dthetabarz=',k,dthetabarz(k)
     endif
  end do

  do k=1,nsig
     qlth_temp(k)=qlth_temp0
     qlth_wind(k)=qlth_wind0
     if (k<=44_i_kind) then
        qltv_temp(k)=qltv_temp0
        qltv_wind(k)=qltv_wind0
     else
        qltv_temp(k)=qltv_temp0*dthetabarz(k)/dthetabarz(44)*four
        qltv_wind(k)=qltv_wind0*dthetabarz(k)/dthetabarz(44)*four
     endif
  end do

  call hanning_smther(qltv_temp, nsig, 5_i_kind)
  call hanning_smther(qltv_wind, nsig, 5_i_kind)

  if (mype==izero) then
     do k=1,nsig
        write(6,*)'in get3berr_reg,k,qltv_temp,qltv_wind=',k,qltv_temp(k),qltv_wind(k)
     enddo
  endif

  if (mype==izero) then
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
  asp1_max(:,:)=zero
  asp2_max(:,:)=zero
  asp3_max(:,:)=zero
  do k=1,nsig1o
     k1=levs_id(k)
     if(k1==izero) cycle      !  skip to next k value
     call isotropic_scales(asp10f,asp20f,asp30f,k)

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

  do k=1,nsig1o
     k1=levs_id(k)
     if (k1==izero)  cycle      !  skip to next k value

     qlth=one
     qltv=one
     select case(nrf_var(nvar_id(k)))
        case('sf','SF'); qlth=qlth_wind(k1) ; qltv=qltv_wind(k1)
        case('vp','VP'); qlth=qlth_wind(k1) ; qltv=qltv_wind(k1)
        case('t','T'); qlth=qlth_temp(k1) ; qltv=qltv_temp(k1)
     end select

     call mk_gradpt_slab(pf2aP1%nlatf,pf2aP1%nlonf, &
                 tx1_slab(1,1,k), &
                 tx2_slab(1,1,k), &
                 tx3_slab(1,1,k), &
                 theta0f (1,1,k), &
                 theta0zf(1,1,k), &
                 asp1_max(nvar_id(k),k1), &
                 asp2_max(nvar_id(k),k1), &
                 asp3_max(nvar_id(k),k1), &
                 qlth,qltv,nvar_id(k))

  end do

end subroutine get_theta_corrl_lenghts
!=======================================================================
!=======================================================================
subroutine mk_gradpt_slab(nlatf,nlonf, &
                  tx1,tx2,tx3, &
                  thetaf,thetazf, &
                  asp1max,asp2max,asp3max, &
                  qlth,qltv,nvarid, &
                  ilatf)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:   mk_gradpt_slab
! prgmmr: sato             org: np22                date: 2007-12-20
!
! abstract: compute grad(pt) parameters to be used for aspect calc.
!           ilatf is used for polar patches in global mode.
!
! program history log:
!   2007-12-20  sato
!   2010-03-10  zhu  - use nrf*
!
!   input argument list:
!     nlatf    - # of lat for filtered space
!     nlonf    - # of lon for filtered space
!     thetaf   - background    theta      in filtered space
!     thetazf  - background (d theta /dz) in filtered space
!     asp1max  - max isotropic aspect value for x
!     asp2max  - max isotropic aspect value for y
!     asp3max  - max isotropic aspect value for z
!     qlth     - horizontal scaling factor
!     qltv     - vertical   scaling factor
!     nvarid   - element id
!   optional input argument list:
!     ilatf    - lat index for patch space,
!                ilatf=zero means no valid data is set for the point
!
!   output argument list:
!     tx1 - anisotropic aspect parameter for x
!     tx2 - anisotropic aspect parameter for y
!     tx3 - anisotropic aspect parameter for z
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$  end documentation block
  implicit none

! Declare passed variables
  integer(i_kind)                               ,intent(in   ) :: nlatf,nlonf
  integer(i_kind)                               ,intent(in   ) :: nvarid
  real(r_kind)                                  ,intent(in   ) :: asp1max,asp2max,asp3max
  real(r_kind)                                  ,intent(in   ) :: qlth,qltv

  real(r_single),dimension(nlatf,nlonf)         ,intent(in   ) :: thetaf,thetazf
  real(r_kind)  ,dimension(nlatf,nlonf),optional,intent(in   ) :: ilatf

  real(r_single),dimension(nlatf,nlonf)         ,intent(  out) :: tx1,tx2,tx3

!--- set limcor .true.
!        to set maximum for fx[123]
!              =minimum for correlation length
  logical     ,parameter:: limcor=.true.
! logical     ,parameter:: limcor=.false.
!---
  real(r_kind),parameter:: rvsmall = 1.e-10_r_kind

! Declare local variables
  integer(i_kind):: i,im,ip,j,jm,jp,inodat
  real(r_kind)::dxi,dyi,fx1,fx2,fx3,gmax,r,rho

  do j=1,nlonf
     do i=1,nlatf

        inodat=izero
 
        if( present(ilatf) ) then
           if( ilatf(i,j)==zero ) then
              inodat=ione
           else
              jp=min(nlonf,j+ione) ; jm=max(ione,j-ione)
              ip=min(nlatf,i+ione) ; im=max(ione,i-ione)
              if( ilatf(ip,j)==zero .or. &
                & ilatf(im,j)==zero .or. &
                & ilatf(i,jp)==zero .or. &
                & ilatf(i,jm)==zero ) inodat=ione
           end if
        end if

        tx1(i,j)=zero_single
        tx2(i,j)=zero_single
        tx3(i,j)=zero_single

        if( inodat /= ione ) then

           jp=min(nlonf,j+ione) ; jm=max(ione,j-ione) ; dxi=one/(jp-jm)
           ip=min(nlatf,i+ione) ; im=max(ione,i-ione) ; dyi=one/(ip-im)

           fx1= dyi*real(thetaf(ip,j)-thetaf(im,j),r_kind)
           fx2= dxi*real(thetaf(i,jp)-thetaf(i,jm),r_kind)
           fx3=     real(thetazf(i,j)             ,r_kind)

           if ( nvarid==nrf3_loc(nrf3_sf) .or. nvarid==nrf3_loc(nrf3_vp) &
               .or. nvarid==nrf3_loc(nrf3_t) ) then
              if (abs(fx1)>rvsmall) then
                 if(limcor) then
                    gmax=two*qlth/asp1max
                    r=abs(fx1)/gmax
                    rho=tanh(r)/r
                    fx1=rho*fx1
                 end if
                 tx1(i,j)=real(fx1,r_single)
              end if
              if (abs(fx2)>rvsmall) then
                 if(limcor) then
                    gmax=two*qlth/asp2max
                    r=abs(fx2)/gmax
                    rho=tanh(r)/r
                    fx2=rho*fx2
                 end if
                 tx2(i,j)=real(fx2,r_single)
              end if
              if (abs(fx3)>rvsmall) then
                 if(limcor) then
                    gmax=two*qltv/asp3max
                    r=abs(fx3)/gmax
                    rho=tanh(r)/r
                    fx3=rho*fx3
                 end if
                 tx3(i,j)=real(fx3,r_single)
              end if
           end if
        end if

     end do
  end do

  call smther_one(tx1(1,1),ione,nlatf,ione,nlonf,2_i_kind)
  call smther_one(tx2(1,1),ione,nlatf,ione,nlonf,2_i_kind)
  call smther_one(tx3(1,1),ione,nlatf,ione,nlonf,2_i_kind)

  return
end subroutine mk_gradpt_slab
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
   implicit none

   integer(i_kind), intent(in   ) :: npts,ns
   real(r_kind)   , intent(inout) :: g1(npts)

   integer(i_kind) it,itp,itm,l
   real(r_kind), allocatable:: g2(:)

   allocate(g2(npts))

   do l=1,ns
      g2(:)=g1(:)
      do it = 1,npts
         itp=min(it+ione,npts) ; itm=max(ione,it-ione)
         g1(it) = quarter*g2(itm) + half*g2(it) + quarter*g2(itp)
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
  implicit none

  integer(i_long)                        , intent(in   ) :: is, ie, js, je
  integer(i_long)                        , intent(in   ) :: ns

  real(r_single), dimension(is:ie, js:je), intent(inout) :: g1
                                   !  on input: original data slab
                                   !  on ouput: filtered data slab


  integer(i_long)  i,j,l,ip,im,jp,jm
  real(r_single), allocatable:: g2(:,:)

  allocate(g2(is:ie,js:je))
  do l=1,ns

     do j=js,je
        do i=is,ie
           ip=min(i+ione,ie) ; im=max(is,i-ione)
           g2(i,j)=quarter*(g1(ip,j)+g1(im,j))+half*g1(i,j)
        end do
     end do

     do i=is,ie
        do j=js,je
           jp=min(j+ione,je) ; jm=max(js,j-ione)
           g1(i,j)=quarter*(g2(i,jp)+g2(i,jm))+half*g2(i,j)
        end do
     end do

  end do
  deallocate(g2)

  return
end subroutine smther_one
!=======================================================================
!=======================================================================
subroutine smther_one_8(g1,is,ie,js,je,ns)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:   smther_one_8
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
  implicit none

  integer(i_long)                      , intent(in   ) :: is, ie, js, je
  integer(i_long)                      , intent(in   ) :: ns

  real(r_kind), dimension(is:ie, js:je), intent(inout) :: g1
                                   !  on input: original data slab
                                   !  on ouput: filtered data slab


  integer(i_long)  i,j,l,ip,im,jp,jm
  real(r_kind), allocatable:: g2(:,:)

  allocate(g2(is:ie,js:je))
  do l=1,ns

     do j=js,je
        do i=is,ie
           ip=min(i+ione,ie) ; im=max(is,i-ione)
           g2(i,j)=quarter*(g1(ip,j)+g1(im,j))+half*g1(i,j)
        end do
     end do

     do i=is,ie
        do j=js,je
           jp=min(j+ione,je) ; jm=max(js,j-ione)
           g1(i,j)=quarter*(g2(i,jp)+g2(i,jm))+half*g2(i,j)
        end do
     end do

  end do
  deallocate(g2)

  return
end subroutine smther_one_8
!=======================================================================
!=======================================================================
subroutine invert_aspect_tensor(asp,ni,nj,nk)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:   invert_aspect_tensor
! prgmmr: sato             org: np23                date: 2007-10-30
!
! abstract: invert aspect tensor, just extracted from get2berr_reg().
!
! program history log:
!   2007-10-30   sato
!
!   input argument list:
!    asp - aspect tensor to be inverted
!    ni,nj,nk - dimensions for filter space
!
!   output argument list:
!    asp - inverted aspect tensor
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
  implicit none

  integer(i_kind),intent(in   ) :: ni,nj,nk
  real(r_single) ,intent(inout) :: asp(7,ni,nj,nk)

  real(r_kind):: a1,a2,a3,a4,a5,a6,detai
  real(r_kind):: biga1,biga2,biga3,biga4,biga5,biga6
  integer(i_kind):: i,j,k

  do k=1,nk
     if( levs_id(k)==izero ) cycle
     do j=1,nj
        do i=1,ni
           a1=real(asp(1,i,j,k),r_kind)
           a2=real(asp(2,i,j,k),r_kind)
           a3=real(asp(3,i,j,k),r_kind)
           a4=real(asp(4,i,j,k),r_kind)
           a5=real(asp(5,i,j,k),r_kind)
           a6=real(asp(6,i,j,k),r_kind)
           biga1=a2*a3-a4*a4
           biga2=a1*a3-a5*a5
           biga3=a1*a2-a6*a6
           biga4=a5*a6-a1*a4
           biga5=a4*a6-a2*a5
           biga6=a4*a5-a3*a6
           detai=one/(a1*biga1+a6*biga6+a5*biga5)
           asp(1,i,j,k)=real(biga1*detai,r_single)
           asp(2,i,j,k)=real(biga2*detai,r_single)
           asp(3,i,j,k)=real(biga3*detai,r_single)
           asp(4,i,j,k)=real(biga4*detai,r_single)
           asp(5,i,j,k)=real(biga5*detai,r_single)
           asp(6,i,j,k)=real(biga6*detai,r_single)
        end do
     end do
  end do
end subroutine invert_aspect_tensor
!=======================================================================
!=======================================================================
subroutine get_aspect_det(asp,det)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:   get_aspect_det
! prgmmr: sato             org: np23                date: 2007-10-30
!
! abstract: get detaminant, just extracted from get2berr_reg().
!
! program history log:
!   2008-03-03   sato
!
!   input argument list:
!    asp - aspect tensor to be inverted
!    det - detaminant
!
!   output argument list:
!    det
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$ end documentation block
  implicit none

  real(r_single),intent(in   ) :: asp(6)
  real(r_kind)  ,intent(inout) :: det

  real(r_kind):: a1,a2,a3,a4,a5,a6
  real(r_kind):: biga1,biga2,biga3,biga4,biga5,biga6

  a1=real(asp(1),r_kind)
  a2=real(asp(2),r_kind)
  a3=real(asp(3),r_kind)
  a4=real(asp(4),r_kind)
  a5=real(asp(5),r_kind)
  a6=real(asp(6),r_kind)
  biga1=a2*a3-a4*a4
  biga2=a1*a3-a5*a5
  biga3=a1*a2-a6*a6
  biga4=a5*a6-a1*a4
  biga5=a4*a6-a2*a5
  biga6=a4*a5-a3*a6
  det=(a1*biga1+a6*biga6+a5*biga5)

end subroutine get_aspect_det
!=======================================================================
!=======================================================================
subroutine get_aspect_reg_ens(mype)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    get_aspect_reg_ens
! prgmmr: pondeca          org: np22                date: 2007-03-05
!
! abstract: compute ensemble based anisotropic aspect tensor for the
!           3dvar case. supports input ensemble grids in awips 212, 221,
!           and global lat-lon formats.
!
! program history log:
!   2007-03-05  pondeca
!   2007-12-20  sato    - replace get3berr_ens_reg with get_aspect_reg_ens
!                         in which some procedures are moved
!                         to the parent subroutine anprewgt_reg().
!   2008-01-15  sato - add more accurate blending of iso & aniso tensor
!   2010-03-10  zhu  - use nvars
!
!   input argument list:
!     mype     - mpi task id
!
!   output argument list:
!
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$ end documentation block
  use anberror, only: afact0
  implicit none

! Declare passed variables
  integer(i_kind),intent(in   ) :: mype

! Declare local variables
  integer(i_kind),parameter::ntensmax=200_i_kind   !max # of ens members
  integer(i_kind) igrdtype(ntensmax)               !ens grid number (212, 221 or 3)
  integer(i_kind) igrdtype_in(50)                  !aux field to igrdtype
  integer(i_kind) ngrps                            !# of distinct igrdtype's present
  integer(i_kind) nflds(ntensmax)                  !# of distinct physical flds in each ens member
  integer(i_kind) nflds_in(nensmax)                !auxiliar field to nflds
  integer(i_kind) ifldlevs(ntensmax,nensmax)       !# of vert levels in each physical fld of each ens member
  integer(i_kind) ifldlevs_in(20,nensmax)          !auxiliar to ifldlevs
  integer(i_kind) nrepeat(nensmax)                 !ens of igrdtype(i) appears nrepeat(i) times
!
  integer(i_kind) i,j,k,k1,ivar,im,ip,jm,jp,m,m1,m2,n,nlatlonf
  integer(i_kind) igbox(4,ngrds),igbox0f(4,ngrds),igrid,igd
  integer(i_kind) kens,ntens
  integer(i_kind) nens(nsig1o,ngrds),nt1,nt2,nt3
  integer(i_kind) ifld
  integer(i_kind) nflag(nvars),nkflag(nsig1o) !dimension is # of anl. variables
  integer(i_kind) kflag(nsig)
  integer(i_kind) iref(nlat,nlon,ngrds)
  integer(i_kind) jref(nlat,nlon,ngrds)

  real(r_single):: aniall(6),ensvin
  real(r_kind) fx2,fx1,fx3,dxi,dyi
  real(r_kind) asp1,asp2,asp3,coeff_asplim
  real(r_kind) qlx,qly,qlz
  real(r_single):: s1,s2,s3,smax,max_grad,min_grad
  real(r_single):: c(6,3)
  real(r_single):: afact,deta0,deta1,alpha,alphaz,mag
  real(r_single):: qlxmin(nvars,nsig) !10 is # of analysis variables
  real(r_single):: qlymin(nvars,nsig) !10 is # of analysis variables
  real(r_single):: qlzmin(nvars,nsig) !10 is # of analysis variables
  real(r_single):: rescvar(10) ! -> change into namelist parameter
  real(r_single):: rescvarzadj(10) ! -> change into namelist parameter
  real(r_kind),parameter:: rperc=0.0001_r_kind
  real(r_kind),parameter:: detmin=one, detmax=10.0_r_kind

  logical:: lgrd1,lgrd2,lgrd3
  logical:: lres1,lres2
  real(r_kind):: aensv(6,nsig1o)
  integer(i_kind):: nsmp

  integer(i_kind):: icovmap  ! flag for output covariance map
  integer(i_kind):: idiagens ! if = 1 then print out diganostic info
  integer(i_kind):: icorlim  ! if = 1 then vertical correlation length is restricted
  integer(i_kind):: ibldani  ! 0: use simple formulation to blend iso-aniso aspect
                     ! 1: use Jim's formulation
                     ! 2: use Sato's formulation
                     ! 3: use anisotropic components only

  integer(i_kind):: iensamp  ! if = 1 then uses amplitude adjustment by ensemble varience
                     ! only when ibldani=0
                     ! NOTE: this option could now work correctlly now (20080122) !

  integer(i_kind):: isatytest ! for my temporary test

  logical:: truewind ! if true, covariance model is based on u,v
                     ! if false,covariance model is based on psi,chi
  logical:: unbalens ! if true, covariance model is based on unbalanced part for chi,t,psfc
                     ! if false,covariance model is based on full values for chi,t,psfc

  namelist/ensparam/ntens,ngrps,igrdtype_in,nflds_in,ifldlevs_in,nrepeat, &
                    rescvar,rescvarzadj,idiagens,icovmap,icorlim, &
                    truewind, unbalens, ibldani, iensamp, isatytest, scalex1ens, scalex2ens, scalex3ens

  data idiagens  / izero /
  data icovmap   / izero /
  data icorlim   / izero /
  data isatytest / izero /
  data ibldani   / izero /
  data iensamp   / izero /
  data truewind / .false. /
  data unbalens / .false. /
  data rescvar / one,one,one,one,one, one,one,one,one,one /
  data rescvarzadj / one,one,one,one,one, one,one,one,one,one /

  allocate(aniasp(6,pf2aP1%nlatf,pf2aP1%nlonf,nsig1o,ngrds))
  allocate(ens0f (  pf2aP1%nlatf,pf2aP1%nlonf,nsig1o))
  allocate(ens0zf(  pf2aP1%nlatf,pf2aP1%nlonf,nsig1o))
  allocate(ensv  (  pf2aP1%nlatf,pf2aP1%nlonf,nsig1o,ngrds))
  allocate(enscoeff(4,nlat,nlon,ngrds))
  allocate(ensmask (  nlat,nlon,ngrds))
  allocate(pgesmin(nsig))           !vert. profile of bckg layer minimum pressure
  allocate(pgesmax(nsig))           !vert. profile of bckg layer maximum pressure
  allocate(gblend(pf2aP1%nlatf,pf2aP1%nlonf,2))   !blending fcts. 2nd dim is for grids 212 and 221

!-----------------------------------------------------------
!-----define the aspect tensor------------------------------
!-----------------------------------------------------------

!==> ensemble parameter:

  igrdtype_in(:)=izero     ; igrdtype(:)=izero
  nflds_in(:)=izero        ; nflds(:)=izero
  ifldlevs_in(:,:)=izero   ; ifldlevs(:,:)=izero
  nrepeat(:)=izero
  ntens=izero

!==> isotropic contribution: -> will be changed by namelist parameters
  scalex1ens   = 1.2_r_kind
  scalex2ens   = 1.2_r_kind
  scalex3ens   = 1.2_r_kind

  open (55,file='ensparam_input',form='formatted')
  read (55,ensparam)
  close(55)

  if( unbalens ) truewind = .false. ! to use unbalance part, it needs to use phi,chi
  if(icovmap==ione) covmap=.true. ! passed to anprewgt_reg

!-----------------------------------------------------------

  if (mype==izero) then
     write (6, ensparam)

     write(6,200)
     200 format('input parameters for ensemble based bckg error covariances:',//)
     print*,'in get3berr_ens_reg: ntens,ngrps=',ntens,ngrps

     do n=1,ngrps
        print*,'in get3berr_ens_reg: n,igrdtype_in(n),nflds_in(n)=',&
                n,igrdtype_in(n),nflds_in(n)
     end do

     do n=1,ngrps
        do m=1,nflds_in(n)
           print*,'in get3berr_ens_reg:, n,m,ifldlevs_in(n,m)=',n,m,ifldlevs_in(n,m)
        end do
     end do
  end if

  do k=1,nsig1o

     ivar=nvar_id(k)
     k1=levs_id(k)
     if (k1==izero) cycle

     call isotropic_scales(asp10f,asp20f,asp30f,k)
 
     do j=1,pf2aP1%nlonf
        do i=1,pf2aP1%nlatf

           if( afact0(ivar) <= zero ) then
              asp1=asp10f(i,j)
              asp2=asp20f(i,j)
              asp3=asp30f(i,j)
           else
              asp1=scalex1ens*asp10f(i,j)
              asp2=scalex2ens*asp20f(i,j)
              asp3=scalex3ens*asp30f(i,j)
           end if

           aspect(1,i,j,k) = real(one/asp1**2,r_single) ! 1st (y) direction    x1*x1
           aspect(2,i,j,k) = real(one/asp2**2,r_single) ! 2nd (x) direction    x2*x2
           aspect(3,i,j,k) = real(one/asp3**2,r_single) ! 3rd (z) direction    x3*x3
           aspect(4,i,j,k) = zero_single                !                      x3*x2
           aspect(5,i,j,k) = zero_single                !                      x3*x1
           aspect(6,i,j,k) = zero_single                !                      x2*x1
           aspect(7,i,j,k) = zero_single
        end do
     end do

  end do

!==> ensemble contribution:

  if (ntens>izero) then !see mark-0

     m1=ione
     do n=1,ngrps
        if (n > ione) m1=m1+nrepeat(n-ione)
        do m=m1,m1+nrepeat(n)-ione
           igrdtype(m)=igrdtype_in(n)
           nflds(m)=nflds_in(n)
           do m2=1,nflds_in(n)
              ifldlevs(m,m2)=ifldlevs_in(n,m2)
           end do
        end do
     end do

     if (mype==izero) then
        do n=1,nensmax
           print*,'in get3berr_ens_reg: n,igrdtype(n),nflds(n)=',n,igrdtype(n),nflds(n)
        end do

        do n=1,nensmax
           do m=1,nflds(n)
              print*,'in get3berr_ens_reg:, n,m,ifldlevs(n,m)=',n,m,ifldlevs(n,m)
           enddo
        enddo
     endif

     call pges_minmax(mype,pgesmin,pgesmax)

     call ens_intpcoeffs_reg(ngrds,igbox,iref,jref,igbox0f,ensmask,enscoeff,gblend,mype)

     if (mype==izero) then
        do n=1,3
           print*,'in get3berr_ens_reg: nlat,nlon=',nlat,nlon
           print*,'n,igbox(1,n),igbox0f(1,n)=',n,igbox(1,n),igbox0f(1,n)
           print*,'n,igbox(2,n),igbox0f(2,n)=',n,igbox(2,n),igbox0f(2,n)
           print*,'n,igbox(3,n),igbox0f(3,n)=',n,igbox(3,n),igbox0f(3,n)
           print*,'n,igbox(4,n),igbox0f(4,n)=',n,igbox(4,n),igbox0f(4,n)
        end do
     end if

     aniasp(:,:,:,:,:)=zero_single
     ensv(:,:,:,:)=zero_single
     nens(:,:)=izero
     lgrd1=.false.
     lgrd2=.false.
     lgrd3=.false.
     nkflag(:)=izero

     do kens=1,ntens
        do ifld=1,nflds(kens)

           if (ifldlevs(kens,ifld)==izero) cycle

           igrid=igrdtype(kens)
           nflag(:)=izero
           kflag(:)=izero
 
           call get_ensmber(kens,ifld,igrid,ntensmax,ifldlevs,truewind,unbalens, &
                igbox,iref,jref,nflag,kflag,idiagens,mype)

           if      (igrid == 212_i_kind) then; igd=ione;        lgrd1=.true.
           else if (igrid == 221_i_kind) then; igd=2_i_kind;    lgrd2=.true.
           else if (igrid ==   3_i_kind) then; igd=3_i_kind;    lgrd3=.true.
           else
              print*,'in get3berr_ens_reg: igrid=',igrid
              print*,'in get3berr_ens_reg: unknown ensemble grid. aborting ...'
              call stop2(stpcode_ensdata)
           endif

           lres1=any(nflag(:)==ione)
           lres2=any(kflag(:)==ione)
           if ( .not.lres1 .or. .not.lres2) cycle
 
           do k=1,nsig1o

              ivar=nvar_id(k)
              k1  =levs_id(k)

              if ( ivar==izero .or. k1==izero .or. &
                   nflag(ivar) /=ione .or. kflag(k1) /=ione ) cycle

              nkflag(k)=ione

              nens(k,igd)=nens(k,igd)+ione

              do j=1,pf2aP1%nlonf
                 do i=1,pf2aP1%nlatf

                    ensv(i,j,k,igd)=ensv(i,j,k,igd)+ens0f(i,j,k)*ens0f(i,j,k)

                    jp=min(pf2aP1%nlonf,j+ione) ; jm=max(ione,j-ione); dxi=one/(jp-jm)
                    ip=min(pf2aP1%nlatf,i+ione) ; im=max(ione,i-ione); dyi=one/(ip-im)

                    fx1= dyi*real(ens0f(ip,j,k)-ens0f(im,j,k),r_kind)
                    fx2= dxi*real(ens0f(i,jp,k)-ens0f(i,jm,k),r_kind)
                    fx3=     real(ens0zf(i,j,k)              ,r_kind)

                    aniasp(1,i,j,k,igd)=aniasp(1,i,j,k,igd) + real(fx1*fx1,r_single) ! 1st (y) direction    x1*x1
                    aniasp(2,i,j,k,igd)=aniasp(2,i,j,k,igd) + real(fx2*fx2,r_single) ! 2nd (x) direction    x2*x2
                    aniasp(3,i,j,k,igd)=aniasp(3,i,j,k,igd) + real(fx3*fx3,r_single) ! 3rd (z) direction    x3*x3
                    aniasp(4,i,j,k,igd)=aniasp(4,i,j,k,igd) + real(fx3*fx2,r_single) !                      x3*x2
                    aniasp(5,i,j,k,igd)=aniasp(5,i,j,k,igd) + real(fx3*fx1,r_single) !                      x3*x1
                    aniasp(6,i,j,k,igd)=aniasp(6,i,j,k,igd) + real(fx2*fx1,r_single) !                      x2*x1
                 end do
              end do
           end do

        end do !end of ifld do-loop
     end do   !end of kens do-loop

!==> rescale variances
     do igd=1,3
        do k=1,nsig1o
           ivar=nvar_id(k)
           k1=levs_id(k)
           if (ivar==izero .or. k1==izero) cycle
           ensv(:,:,k,igd)=rescvar(ivar)*sqrt(ensv(:,:,k,igd))
        end do
     end do

!==> compute reasonable lower bounds for variances
     do k=1,nsig1o

        ivar=nvar_id(k)
        k1=levs_id(k)
        if (ivar==izero .or. k1==izero) cycle

        nt1=max(ione,(nens(k,1)+nens(k,2)+nens(k,3)-ione))
        nt2=max(ione,          (nens(k,2)+nens(k,3)-ione))
        nt3=max(ione,                    (nens(k,3)-ione))

        smax=-huge(smax)
        do j=1,pf2aP1%nlonf
           do i=1,pf2aP1%nlatf
              s1=(ensv(i,j,k,1)+ensv(i,j,k,2)+ensv(i,j,k,3))/sqrt(real(nt1,r_single))
              s2=              (ensv(i,j,k,2)+ensv(i,j,k,3))/sqrt(real(nt2,r_single))
              s3=                             ensv(i,j,k,3) /sqrt(real(nt3,r_single))
              smax=max(smax,s1,s2,s3)
           end do
        end do
        if (nkflag(k)==ione) then
           qlxmin(ivar,k1)=rperc*smax
           qlymin(ivar,k1)=rperc*smax
           qlzmin(ivar,k1)=rperc*smax*rescvarzadj(ivar)
        else
           qlxmin(ivar,k1)=one
           qlymin(ivar,k1)=one
           qlzmin(ivar,k1)=one
        endif

     end do


!==> compute averages over each type of ens grid:
     aensv=zero
     nlatlonf=pf2aP1%nlonf*pf2aP1%nlatf

     do k=1,nsig1o

        ivar=nvar_id(k)
        k1=levs_id(k)
        if (ivar==izero .or. k1==izero) cycle

        nt1=max(ione,(nens(k,1)+nens(k,2)+nens(k,3)-ione))
        nt2=max(ione,          (nens(k,2)+nens(k,3)-ione))
        nt3=max(ione,                    (nens(k,3)-ione))
 
        do j=1,pf2aP1%nlonf
           do i=1,pf2aP1%nlatf
              ensv(i,j,k,1)=(ensv(i,j,k,1)+ensv(i,j,k,2)+ensv(i,j,k,3))/sqrt(float(nt1))
              ensv(i,j,k,2)=              (ensv(i,j,k,2)+ensv(i,j,k,3))/sqrt(float(nt2))
              ensv(i,j,k,3)=                             ensv(i,j,k,3) /sqrt(float(nt3))

              if( ibldani==izero .or. ibldani==2_i_kind .or. ibldani==3_i_kind ) then
                 do m=1,6
                    c(m,1)=(aniasp(m,i,j,k,1)+aniasp(m,i,j,k,2)+aniasp(m,i,j,k,3))/float(nt1)
                    c(m,2)=                  (aniasp(m,i,j,k,2)+aniasp(m,i,j,k,3))/float(nt2)
                    c(m,3)=                                     aniasp(m,i,j,k,3) /float(nt3)
                 end do
                 do igd=1,3
                    qlx=max(qlxmin(ivar,k1),ensv(i,j,k,igd))
                    qly=max(qlymin(ivar,k1),ensv(i,j,k,igd))
                    qlz=max(qlzmin(ivar,k1),ensv(i,j,k,igd)*rescvarzadj(ivar))
                    if ( qlx>tiny_r_kind .and. qly>tiny_r_kind .and. qlz>tiny_r_kind ) then
                       aniasp(1,i,j,k,igd)=c(1,igd)/real(qly**2,r_single)
                       aniasp(2,i,j,k,igd)=c(2,igd)/real(qlx**2,r_single)
                       aniasp(3,i,j,k,igd)=c(3,igd)/real(qlz**2,r_single)
                       aniasp(4,i,j,k,igd)=c(4,igd)/real(qlz*qlx,r_single)
                       aniasp(5,i,j,k,igd)=c(5,igd)/real(qlz*qly,r_single)
                       aniasp(6,i,j,k,igd)=c(6,igd)/real(qly*qlx,r_single)
                    end if
                 end do
              else if(ibldani==ione) then
                 do m=1,6
                    aniasp(m,i,j,k,1)=(aniasp(m,i,j,k,1)+aniasp(m,i,j,k,2)+aniasp(m,i,j,k,3))/float(nt1)
                    aniasp(m,i,j,k,2)=                  (aniasp(m,i,j,k,2)+aniasp(m,i,j,k,3))/float(nt2)
                    aniasp(m,i,j,k,3)=                                     aniasp(m,i,j,k,3) /float(nt3)
                 end do
                 smax=real(maxval(ensv(i,j,k,1:3)),r_kind)
                 aensv(1,k)=aensv(1,k)+max(smax                  ,qlxmin(ivar,k1))/nlatlonf
                 aensv(2,k)=aensv(2,k)+max(smax                  ,qlymin(ivar,k1))/nlatlonf
                 aensv(3,k)=aensv(3,k)+max(smax*rescvarzadj(ivar),qlzmin(ivar,k1))/nlatlonf
              else
                 write(6,*) 'invalid ibldani setting !'
                 call stop2(stpcode_ensdata)
              end if
           end do
        end do

        if(ibldani==ione) then
           aensv(4,k)=aensv(1,k)*aensv(3,k)
           aensv(5,k)=aensv(3,k)*aensv(2,k)
           aensv(6,k)=aensv(2,k)*aensv(1,k)
           aensv(1,k)=aensv(1,k)*aensv(1,k)
           aensv(2,k)=aensv(2,k)*aensv(2,k)
           aensv(3,k)=aensv(3,k)*aensv(3,k)
        end if

     end do

!==> perform blending of various ens grids and
!    finally add isotropic + ensemble contribution together:
!
!    NOTE: ensvin (variance) is used only for (ibldani/=1 or iensamp==1).
!    But it is always calculated since the task must not be heavy
!
     if(iensamp==ione) then
        allocate(ensamp(pf2aP1%nlatf,pf2aP1%nlonf,nsig1o))
        ensamp=1.0_r_single
     end if

     do k=1,nsig1o
 
        ivar=nvar_id(k)
        k1=levs_id(k)
        if (ivar==izero .or. k1==izero ) cycle
 
        afact=afact0(ivar)

        lres1=any(abs(aniasp(:,:,:,k,:))>tiny_single)
        if (.not.lres1) then
           afact=zero_single
           cycle
        end if

        ensamp_mod=zero

    !-------------------------------------------------------
    !  +---------------------------------------+
    !  |       BBBBBBBBBBBBBBBBBBBBBBBBB       |
    !  |       B       BBBBBBBBB       B       |
    !  |  ID3  B  ID2  B  ID1  B       B       |
    !  |       B       BBBBBBBBB       B       |
    !  |       BBBBBBBBBBBBBBBBBBBBBBBBB       |
    !  +---------------------------------------+
    !  |<-GLB->B<-221->B<-212->B<-221->B<-GLB->|
    !   ID3=GLB, ID2=GLB+221, ID1=GLB+221+212
    !   B: Blending Zone
    !-------------------------------------------------------
        do j=1,pf2aP1%nlonf
           do i=1,pf2aP1%nlatf
              if (lgrd1 .and. lgrd2 .and. lgrd3) then
                 ! use grd212, grd221 & global
                 aniall(:) = (one-gblend(i,j,1))*aniasp(:,i,j,k,2)+gblend(i,j,1) *aniasp(:,i,j,k,1)
                 aniall(:) = (one-gblend(i,j,2))*aniasp(:,i,j,k,3)+gblend(i,j,2) *aniall(:)
                 ensvin    = (one-gblend(i,j,1))*ensv(i,j,k,2)    +gblend(i,j,1) *ensv(i,j,k,1)
                 ensvin    = (one-gblend(i,j,2))*ensv(i,j,k,3)    +gblend(i,j,2) *ensvin
              else if (.not.lgrd1 .and. lgrd2 .and. lgrd3) then
                 ! use grd221 & global
                 aniall(:) = (one-gblend(i,j,2))*aniasp(:,i,j,k,3)+gblend(i,j,2) *aniasp(:,i,j,k,2)
                 ensvin    = (one-gblend(i,j,2))*ensv(i,j,k,3)    +gblend(i,j,2) *ensv(i,j,k,2)
              else if (lgrd1 .and. .not.lgrd2 .and. lgrd3) then
                 ! use grd212 & global
                 aniall(:) = (one-gblend(i,j,1))*aniasp(:,i,j,k,3)+gblend(i,j,1) *aniasp(:,i,j,k,1)
                 ensvin    = (one-gblend(i,j,1))*ensv(i,j,k,3)    +gblend(i,j,1) *ensv(i,j,k,1)
              else if (lgrd1 .and. lgrd2 .and. .not.lgrd3) then
                 ! use grd212 & grd221
                 aniall(:) = (one-gblend(i,j,1))*aniasp(:,i,j,k,2)+gblend(i,j,1) *aniasp(:,i,j,k,1)
                 aniall(:) = (one-gblend(i,j,2))*aspect(:,i,j,k)  +gblend(i,j,2) *aniall(:)
                 ensvin    = (one-gblend(i,j,1))*ensv(i,j,k,2)    +gblend(i,j,1) *ensv(i,j,k,1)
              else if (lgrd1 .and. .not.lgrd2 .and. .not.lgrd3) then
                 ! use grd212
                 aniall(:) = (one-gblend(i,j,1))*aspect(:,i,j,k)  +gblend(i,j,1) *aniasp(:,i,j,k,1)
                 ensvin    =  ensv(i,j,k,1)
              else if (.not.lgrd1 .and. lgrd2 .and. .not.lgrd3) then
                 ! use grd221
                 aniall(:) = (one-gblend(i,j,2))*aspect(:,i,j,k)  +gblend(i,j,2) *aniasp(:,i,j,k,2)
                 ensvin    =  ensv(i,j,k,2)
              else if (.not.lgrd1 .and. .not.lgrd2 .and. lgrd3) then
                 ! use global
                 aniall(:) = aniasp(:,i,j,k,3)
                 ensvin    = ensv(i,j,k,3)
              else
                 print*,'in get3berr_ens_reg: lgrd1,lgrd2,lgrd3=',lgrd1,lgrd2,lgrd3
                 print*,'in get3berr_ens_reg: unknown case. aborting ...'
                 call stop2(stpcode_ensdata)
              end if

      !-----------------------
      ! Sets limit to the aspect tensors with the isotropic values.
      !-----------------------
              if(icorlim==ione) then
                 max_grad=-1.0_r_single
                 min_grad= 1.2_r_single
                 call set_range_aniall(aniall,aspect(1:3,i,j,k),max_grad,min_grad)
              end if

              if(ibldani==izero) then
                 !==> simple blending
                 aspect(1,i,j,k) = aspect(1,i,j,k)+afact*aniall(1)
                 aspect(2,i,j,k) = aspect(2,i,j,k)+afact*aniall(2)
!--- use vertical component ?
                 aspect(3,i,j,k) = aspect(3,i,j,k)+afact*aniall(3)
                 aspect(4,i,j,k) =                 afact*aniall(4)
                 aspect(5,i,j,k) =                 afact*aniall(5)
!---
                 aspect(6,i,j,k) =                 afact*aniall(6)
                 aspect(7,i,j,k) = zero_single
              else if(ibldani==ione) then
                 !==> Jim's formulation
                 qlx=max(qlxmin(ivar,k1),ensvin)
                 qly=max(qlymin(ivar,k1),ensvin)
                 qlz=max(qlzmin(ivar,k1),ensvin*rescvarzadj(ivar))
                 aspect(1,i,j,k) = (aspect(1,i,j,k)*aensv(2,k)+afact*aniall(1))/(aensv(2,k)+afact*qly**2)
                 aspect(2,i,j,k) = (aspect(2,i,j,k)*aensv(1,k)+afact*aniall(2))/(aensv(1,k)+afact*qlx**2)
                 aspect(3,i,j,k) = (aspect(3,i,j,k)*aensv(3,k)+afact*aniall(3))/(aensv(3,k)+afact*qlz**2)
                 aspect(4,i,j,k) =                             afact*aniall(4) /(aensv(4,k)+afact*qlz*qlx)
                 aspect(5,i,j,k) =                             afact*aniall(5) /(aensv(5,k)+afact*qlz*qlx)
                 aspect(6,i,j,k) =                             afact*aniall(6) /(aensv(6,k)+afact*qly*qlx)
                 aspect(7,i,j,k) = zero_single

              else if(ibldani==2_i_kind) then
                 !==> Yoshi's formulation
                 alpha=0.5_r_single
                 alphaz=0.5_r_single
                 deta0=aspect(1,i,j,k)*aspect(2,i,j,k)
                 deta1=aniall(1)*aniall(2)-aniall(6)*aniall(6)
 
                 ! blend magnitude
                 mag=min(max((one-alpha)+alpha*sqrt(deta1/deta0),detmin),detmax)

                 ! normalize aspect shape only for horizontal direction
                 coeff_asplim=sqrt(deta0/deta1)
                 aniall(1)=aniall(1)*coeff_asplim
                 aniall(2)=aniall(2)*coeff_asplim
                 aniall(6)=aniall(6)*coeff_asplim
 
                 aniall(4)=aniall(4)*sqrt(coeff_asplim)
                 aniall(5)=aniall(5)*sqrt(coeff_asplim)
 
                 ! blend aspect shape and multiply blended magnitude
                 aspect(1,i,j,k) = ((one-alpha)*aspect(1,i,j,k)+alpha*aniall(1))*mag
                 aspect(2,i,j,k) = ((one-alpha)*aspect(2,i,j,k)+alpha*aniall(2))*mag
                 aspect(6,i,j,k) = (                            alpha*aniall(6))*mag

                 aspect(3,i,j,k) = ((one-alphaz)*aspect(3,i,j,k)+alphaz*aniall(3))
                 aspect(4,i,j,k) = (                 sqrt(alphaz*alpha)*aniall(4))*sqrt(mag)
                 aspect(5,i,j,k) = (                 sqrt(alphaz*alpha)*aniall(5))*sqrt(mag)

                 aspect(7,i,j,k) = zero_single
              else if(ibldani==3_i_kind) then
                 max_grad=10.0_r_single
                 min_grad=1.0_r_single
                 call set_range_aniall(aniall,aspect(1:3,i,j,k),max_grad,min_grad)
                 aspect(1,i,j,k) = aniall(1)
                 aspect(2,i,j,k) = aniall(2)
                 aspect(6,i,j,k) = aniall(6)
                 aspect(7,i,j,k) = zero_single
                 if(aniall(3)>zero_single) then
                    aspect(3,i,j,k) = aniall(3)
                    aspect(4,i,j,k) = aniall(4)
                    aspect(5,i,j,k) = aniall(5)
                 end if
              end if

              if(iensamp==ione) then
                 ensamp(i,j,k)=ensvin
              end if
 
           end do
        end do

        !==> normalize ensamp (0.5-2.0)
        !    sqrt(ensamp) will be multiplied to an_amp
        if(iensamp==ione.and.lres1) then
           nsmp=100_i_kind
           call mode_val(ensamp(1,1,k),nlatlonf,nsmp,ensamp_mod)
           do j=1,pf2aP1%nlonf
              do i=1,pf2aP1%nlatf
                 ensamp(i,j,k)=ensamp(i,j,k)/ensamp_mod
                 if     (ensamp(i,j,k)>EAMPMAX) then ; ensamp(i,j,k)=EAMPMAX
                 else if(ensamp(i,j,k)<EAMPMIN) then ; ensamp(i,j,k)=EAMPMIN
                 end if
              end do
           end do
        end if

     end do

  end if !mark-0

  deallocate(aniasp)
  deallocate(ens0f)
  deallocate(ens0zf)
  deallocate(ensv)
  deallocate(enscoeff)
  deallocate(ensmask)
  deallocate(pgesmin)
  deallocate(pgesmax)
  deallocate(gblend)

  if(allocated(field_st)) deallocate(field_st)
  if(allocated(field_t))  deallocate(field_t)

!-----------------------------------------------
! Amplitude correction settings to pass to anprewgt_reg
!-----------------------------------------------
  if     (isatytest==4_i_kind) then
     llamp_adjust = .true.
     llamp_coeff  = 0.6_r_kind
     llamp_levtop =40_i_kind
  else if(isatytest==6_i_kind) then
     llamp_adjust = .true.
     llamp_coeff  = 0.8_r_kind
     llamp_levtop =40_i_kind
  end if

end subroutine get_aspect_reg_ens
!=======================================================================
!=======================================================================
subroutine get_ensmber(kens,ifld,igrid,ntensmax,ifldlevs,truewind,unbalens, &
           igbox,iref,jref,nflag,kflag,idiagens,mype)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:   get_ensmber
! prgmmr: pondeca          org: np22                date: 2007-03-08
!
! abstract: obtain specific physical field of specific ens member on the
!           filter grid. also obtain the vertical derivative of that field.
!
! program history log:
!   2007-03-08  pondeca
!   2007-12-07  sato : add unbalanced part mode
!   2010-03-10  zhu  - use nvars,replace agvz,wgvz,bvz by agvk,wgvk,bvk
!
!   input argument list:
!    mype           - mpi task id
!    ntensmax       - max # of e-members supported
!    kens           - order # of this e-member
!    igrid          - grid number for this e-member
!    ifld           - order # of this physical field of the kens e-member
!    ngrds          - number of e-grids supported. currently three
!    enscoeff(4,i,j,ngrds)  - bilinear interpolation coeffs from e-grid to anl grid
!    ensmask(i,j,ngrds)   - 1. for pts of anl grid inside e-grid, 0. otherwise
!    igbox(4,ngrds) - i and j corner values of portion of anl grid that
!                     falls completely inside e-grid.
!    ifldlevs(kens,ifld) - # of p-levels in the ifld field of the kens e-member
!    truewind
!    unbalens
!
!   output argument list:
!    nflag(i) - 1 if field will be used to construct covariance of
!               ith anl variable, 0 otherwise
!    kflag(k) - 1 if field will be used to construct covariance at
!               the kth model level, 0 otherwise
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$ end documentation block

  use balmod, only : bvk,agvk,wgvk,agvk_lm,ke_vp,f1
  use sub2fslab_mod, only : setup_sub2fslab, destroy_sub2fslab, &
                            sub2fslab, sub2fslabdz

  implicit none

! Declare passed variables
  integer(i_kind),intent(in   ) :: kens,igrid,ifld,ntensmax,mype
  integer(i_kind),intent(in   ) :: igbox(4,ngrds)
  integer(i_kind),intent(in   ) :: iref(nlat,nlon,ngrds)
  integer(i_kind),intent(in   ) :: jref(nlat,nlon,ngrds)
  integer(i_kind),intent(inout) :: nflag(nvars) !dim is # of anl variables
  integer(i_kind),intent(inout) :: kflag(nsig)
  integer(i_kind),intent(in   ) :: ifldlevs(ntensmax,nensmax)
  integer(i_kind),intent(in   ) :: idiagens  !if = 1 then print out diganostic info

  logical        ,intent(in   ) :: truewind
  logical        ,intent(in   ) :: unbalens

! Declare local variables
  integer(i_kind) i,j,k,l,m,l2,ivar,it
  integer(i_kind) n,kup
  integer(i_kind) inttype !read in from each e-member. tells about
!                          desired vertical interp type for specific
!                          physical field. 0 for linear in p and
!                          1 in ln(p).
  integer(i_kind) irc_s_reg(npe),ird_s_reg(npe)

  real(r_kind) asp1,asp2,asp3
  real(r_kind) dl1,dl2

  real(r_kind),allocatable,dimension(:,:,:)::field

  integer(i_kind) lun,nx,ny,k1
  integer(i_kind) kk,num_pad,kslab,kstart,kend,kslab_prev,ier
  real(r_single),allocatable,dimension(:)::slab,aslab
  real(r_single),allocatable,dimension(:,:,:)::h_loc
  real(r_single),allocatable,dimension(:)::pres
  real(r_single) slab2(nlat,nlon),aslab2(nlat,nlon)
  real(r_single) tempa(itotsub)
  real(r_single) strp(lat1*lon1)
  real(r_single) auxa(lat2,lon2),auxb(nlon,nlat)
  real(r_single) p0,gamma
  real(r_kind) p1,p2
  logical one21
  character(3) clun,clun2

  integer(i_long):: ngauss_smooth,npass_smooth,normal_smooth,ifilt_ord_smooth
  integer(i_long):: nsmooth_smooth, nsmooth_shapiro_smooth

  real(r_double) :: rgauss_smooth(1)

  logical:: saty_spl
  real(r_kind),dimension(nsig):: xspli,yspli,xsplo,ysplo
! saty_spl=.true.
  saty_spl=.false.

!==========================================================================
!==>determine dimensions of input ensemble grid and allocate
!   slab, which is used to read in the ens fields:
!==========================================================================
  if      (igrid == 212_i_kind) then; nx=185_i_kind; ny=129_i_kind
  else if (igrid == 221_i_kind) then; nx=349_i_kind; ny=277_i_kind
  else if (igrid ==   3_i_kind) then; nx=360_i_kind; ny=181_i_kind
  else
     if (mype == izero ) then
        print*,'in get_ensmber: igrid=',igrid
        print*,'in get_ensmber: unsupported grid, aborting ...'
     end if
     call stop2(stpcode_ensdata)
  end if

  allocate(slab(nx*ny))
  allocate(aslab(nx*ny))

!==========================================================================
!==>ens input fields are written as direct access files. determine the
!   address of desired initial record. also retrieve pressure values
!   of the field's vertical levels:
!==========================================================================
  write (clun(1:3),'(i3.3)') kens

  lun=55_i_kind
  open (lun,file='ens.dat_'//clun,form='unformatted', &
        access='direct',recl=4*nx*ny)

  kstart=izero
  do k=1,ifld-ione
     kstart=kstart+(ione+ifldlevs(kens,k))
  enddo
  kstart=kstart+ione

  read(lun,rec=kstart) slab

  do i=1,10
     nflag(i)=nint(slab(i))
  enddo

  ivar=nint(slab(20))

  n=nint(slab(25))
  if (igrid  /= n) then
     if (mype == izero) then
        print*,'in get_ensmber: igrid,n=',igrid,n
        print*,'in get_ensmber: inconsistency in grid type for this field. &
                                &igrid and n must be equal. aborting ...'
     end if
     call stop2(stpcode_ensdata)
  end if

  inttype=nint(slab(26))

  n=nint(slab(27))
  if (ifldlevs(kens,ifld) /= n) then
     if (mype == izero) then
        print*,'in get_ensmber: ifldlevs(kens,ifld),n=',ifldlevs(kens,ifld),n
        print*,'in get_ensmber: inconsistency in number of levels for this field. &
                                &ifldlevs and n must be equal. aborting ...'
     end if
     call stop2(stpcode_ensdata)
  end if

  allocate(pres(ifldlevs(kens,ifld)))
  do k=1,ifldlevs(kens,ifld)
     pres(k)=slab(k+39_i_kind)
  end do

  if (idiagens==ione .and. mype==izero) then
     print*,'in get_ensmber: kens,ifld,kstart,igrid,nx,ny=', &
                             kens,ifld,kstart,igrid,nx,ny
  end if
!==========================================================================
!==>prepare for alltoallv comunications:
!==========================================================================
  n=ifldlevs(kens,ifld)
  if(mod(n,npe)==izero) then; num_pad= n
  else;                   num_pad=(n/npe+ione)*npe
  end if
  if (mype==izero) &
     print*,'in get_ensmber: kens,ifld,ivar,ifldlevs,npe,num_pad=', &
                             kens,ifld,ivar,ifldlevs(kens,ifld),npe,num_pad

  allocate(h_loc(lat2,lon2,num_pad))
  h_loc(:,:,:)=zero_single

  do i=1,npe
     irc_s_reg(i)=ijn_s(mype+ione)
  end do
  ird_s_reg(1)=izero
  do i=1,npe
     if(i /= ione) ird_s_reg(i)=ird_s_reg(i-ione)+irc_s_reg(i-ione)
  end do

!==========================================================================
!==>read in ensemble field and distribute over subdomains
!==========================================================================
  kslab_prev=ione
  kstart=kstart+ione
  kend=kstart+ifldlevs(kens,ifld)-ione

  tempa(:)=zero_single

  do 200 kslab=kstart,kend
     if (mod(kslab-kstart,npe) == mype) then
        read(lun,rec=kslab) slab
        slab2(:,:)=zero_single

        call fillanlgrd(slab,ngrds,igrid,nx,ny,slab2,iref,jref,igbox,enscoeff)

        if (ifld==ione .or. ifld==2_i_kind) then
           if (ifld==ione)     read(lun,rec=(kslab+(ifldlevs(kens,ifld)+ione))) aslab ! v-comp
           if (ifld==2_i_kind) read(lun,rec=(kslab-(ifldlevs(kens,ifld)+ione))) aslab ! u-comp
           aslab2(:,:)=zero_single
 
           call fillanlgrd(aslab,ngrds,igrid,nx,ny,aslab2,iref,jref,igbox,enscoeff)
           if (ifld==ione) then
              ! slab2=u -> st, aslab2=v -> vp
              call ens_uv_to_psichi(slab2,aslab2,truewind)
           else
              ! aslab2=u -> st, slab2=v -> vp
              call ens_uv_to_psichi(aslab2,slab2,truewind)
           end if
        end if

        do i=1,itotsub
           tempa(i)=slab2(ltosi_s(i),ltosj_s(i))
        end do

     endif

     kk=kslab-kstart+ione
     if ( mod(kk,npe)==izero .or. kk==ifldlevs(kens,ifld) ) then
        call mpi_alltoallv(tempa,ijn_s,displs_s,mpi_real4, &
                h_loc(1,1,kslab_prev),irc_s_reg,ird_s_reg, &
                            mpi_real4,mpi_comm_world,ierror)
        kslab_prev=kk+ione
    end if
200 continue
  close(lun)

!==========================================================================
!==>interpolate vertically and populate field(:,:)
!==========================================================================

  it=ntguessig

  allocate(field(lat2,lon2,nsig),stat=ier)
  if(ier/=izero) then
     write(6,*) 'could not allocate memory for field'
     call stop2(stpcode_alloc)
  end if

  field (:,:,:)=zero

  n=ifldlevs(kens,ifld)

  if (n == ione) then
     do k=1,nsig
        if(ivar==nrf2_loc(nrf2_ps)) then
           field(:,:,k)=h_loc(:,:,1)*0.001_r_kind ! Pa->cb
        else
           field(:,:,k)=h_loc(:,:,1)
        end if
     end do

  else
     if(saty_spl) then
        ! Spline interpolation
        do i=1,lat2
           do j=1,lon2
              if(inttype==izero) then
                 xspli=pres
                 do k=1,nsig
                    xsplo(k)=ges_prsl(i,j,k,it)*10.0_r_kind
                 end do
              else
                 xspli=log(pres)
                 do k=1,nsig
                    xsplo(k)=log(ges_prsl(i,j,k,it)*10.0_r_kind)
                 end do
              end if
              do k=1,n
                 yspli(k)=h_loc(i,j,k)
              end do
              call intp_spl(xspli,yspli,xsplo,ysplo,n,nsig)
!             if(mype==izero.and.i==lat2.and.j==lon2) then
!                write(6,*) 'splchk:',xspli(1:n),'|',yspli(1:n),'|',xsplo,'|',ysplo
!             end if
              do k=1,nsig
                 field(i,j,k)=ysplo(k)
              end do
           end do
        end do
     else
        ! Linear interpolation
        do k=1,nsig
           do j=1,lon2
              do i=1,lat2
                 p0=ges_prsl(i,j,k,it)*10._r_single
                 if (p0<pres(n)) then
                    field(i,j,k)=h_loc(i,j,n)
                 else if (p0>=pres(1)) then
                    field(i,j,k)=h_loc(i,j,1)
                 else
                    do kk=1,n-ione
                       if (p0<=pres(kk) .and. p0>=pres(kk+ione)) then
                          if (inttype == izero) then
                             gamma=(h_loc(i,j,kk+ione)-h_loc(i,j,kk))/(pres(kk+ione)-pres(kk))
                             field(i,j,k)=h_loc(i,j,kk)+gamma*(p0-pres(kk))
                          else
                             gamma=(h_loc(i,j,kk+ione)-h_loc(i,j,kk))/alog(pres(kk+ione)/pres(kk))
                             field(i,j,k)=h_loc(i,j,kk)+gamma*alog(p0/pres(kk))
                          end if
                       end if
                    end do
                 end if
              end do
           end do
        end do
     end if
  end if

! save the data to estimate unbalanced part
  if( unbalens .and. ivar==ione ) then
     if(.not.allocated(field_st)) allocate(field_st(lat2,lon2,nsig))
     field_st = field
     kens_p = kens
  end if

!==========================================================================
!==>Estimate unbalanced part
!==========================================================================
  if( unbalens ) then
     if( kens/=kens_p ) then
        write(6,*) 'get_ensmber(): kens must be equal to kens_p,',kens,kens_p
        call stop2(stpcode_ensdata)
     end if
!-------------------------------
! Subtract ST part
!-------------------------------
     select case(ivar)
!-------------------------------
! Chi
!-------------------------------
        case(2)
           do k=1,ke_vp
              do j=1,lon2
                 do i=1,lat2
                    field(i,j,k)=field(i,j,k)-bvk(i,j,k)*field_st(i,j,k)
                 end do
              end do
           end do
!-------------------------------
! T
!-------------------------------
        case(3)
           if(fstat) then
              do k=1,nsig
                 do m=1,nsig
                    do j=1,lon2
                       do i=1,lat2
                          field(i,j,m)=field(i,j,m)-agvk_lm(m,k)*f1(i,j)*field_st(i,j,k)
                       end do
                    end do
                 end do
              end do
           else
              do k=1,nsig
                 do m=1,nsig
                    do j=1,lon2
                       do i=1,lat2
                          field(i,j,m)=field(i,j,m)-agvk(i,j,m,k)*field_st(i,j,k)
                       end do
                    end do
                 end do
              end do
           end if
!-------------------------------
! Psfc
!-------------------------------
        case(7)
           do j=1,lon2
              do i=1,lat2
                 do k=1,nsig
                    field(i,j,1)=field(i,j,1)-wgvk(i,j,k)*field_st(i,j,k)
                 end do
                 field(i,j,2:nsig)=field(i,j,1)
              end do
           end do
     end select
  end if

!-------------------------------
! Test: replace Q info by T
!-------------------------------
! if     (ivar==3_i_kind) then
!    if(.not.allocated(field_t)) allocate(field_t(lat2,lon2,nsig))
!    field_t = field
! else if(ivar==4_i_kind) then
!    field = field_t
! end if
!
!-------------------------------
! Vertical Smoother
!-------------------------------
  nsmooth_smooth=2_i_kind
  nsmooth_shapiro_smooth=izero
  n=ifldlevs(kens,ifld)
  if (n > ione) then
     call vert_smther(field,nsmooth_smooth,nsmooth_shapiro_smooth)
  end if

!==========================================================================
!==>Output to check the field
!==========================================================================
  if (idiagens==ione) then
     write (clun(1:3),'(i3.3)') kens
     write (clun2(1:3),'(i3.3)') ifld
     open (54,file='field.dat_'//clun//'_'//clun2,form='unformatted')

     if (n == ione) then
        k=ione
        auxa(:,:)=field(:,:,k)
        call strip_single(auxa,strp,ione)
        call mpi_gatherv(strp,ijn(mype+ione),mpi_real4, &
             tempa,ijn,displs_g,mpi_real4,izero,mpi_comm_world,ierror)
        if (mype==izero) then
           auxb(:,:)=zero_single
           call unfill_mass_grid2t(tempa,nlon,nlat,auxb)
           write(54) auxb
        end if
     else
        do k=1,nsig
           auxa(:,:)=field(:,:,k)
           call strip_single(auxa,strp,ione)
           call mpi_gatherv(strp,ijn(mype+ione),mpi_real4, &
                tempa,ijn,displs_g,mpi_real4,izero,mpi_comm_world,ierror)
           if (mype==izero) then
              auxb(:,:)=zero_single
              call unfill_mass_grid2t(tempa,nlon,nlat,auxb)
              write(54) auxb
           end if
        end do
     end if
     close(54)
  end if
!==========================================================================
!==>populate kflag
!  code foresees possible use of ensemble grids that might only be given
!  on a limitted number of pressure levels whose range does not fully
!  contain that of the background pressure field. In that case, the
!  particular ensemble field can only be used to define the covariances
!  on a portion of the model's vertical levels. kflag determines what
!  those vertical levels are. kflag(k)=1(0) means that the current ensemble
!  field will(not) be used for the covariance on the the kth model level.
!==========================================================================
  kflag(1:nsig)=ione

  n=ifldlevs(kens,ifld)
  if (n == ione) kflag(2:nsig)=-ione ! in the current code, n==1 is assumed to occur
                                     ! only when ensemble field is a surface field

  kup=izero
  if (n > ione) then
     p1=pres(1)*one
     p2=pres(n)*one
     do k=nsig,1,-1
        if (pgesmax(k)>p1 .or. pgesmin(k)<p2 ) then
           kflag(k)=-ione
           if (kup==izero .and. pgesmax(k)>p1) kup=k
        endif
     enddo
     if (pres(1)>=999.5_r_single) then
        do k=1,kup
           kflag(k)=ione
        enddo
     endif
  endif
!==========================================================================


!==========================================================================
!==>convert field(:,:,:) from subdomain to slab mode and interpolate to   |
!   filter grid. repeat for the vertical derivative of field(:,:,:)       !
!==========================================================================
  ens0f (:,:,:)=zero_single
  ens0zf(:,:,:)=zero_single
  call setup_sub2fslab
  call sub2fslab  (field,ens0f )
  if(n>ione) call sub2fslabdz(field,ens0zf)
  call destroy_sub2fslab

!-----------------------------------------------------------
!-------end of getting background variables on filter grid--
!-----------------------------------------------------------

  one21=.true.

  if (one21) then
     do k=1,nsig1o
        call smther_one(ens0f (1,1,k),ione,pf2aP1%nlatf,ione,pf2aP1%nlonf,nsmooth_smooth)
        call smther_one(ens0zf(1,1,k),ione,pf2aP1%nlatf,ione,pf2aP1%nlonf,nsmooth_smooth)
     end do
  else
     aspect(:,:,:,:)=zero
     do k=1,nsig1o
        k1=levs_id(k)
        if (k1==izero) cycle
        do j=1,pf2aP1%nlonf
           do i=1,pf2aP1%nlatf
              asp1=smooth_len/pf2aP1%grid_ratio_lat
              asp2=smooth_len/pf2aP1%grid_ratio_lon
              asp3=smooth_len
              aspect(1,i,j,k)=real(asp1**2,r_single)
              aspect(2,i,j,k)=real(asp2**2,r_single)
              aspect(3,i,j,k)=real(asp3**2,r_single)
              aspect(4:7,i,j,k)=zero_single
           end do
        end do
     end do


     ngauss_smooth=ione
     rgauss_smooth=one
     npass_smooth=ione
     normal_smooth=izero
     ifilt_ord_smooth=4_i_kind
     nsmooth_smooth=izero
     nsmooth_shapiro_smooth=izero
     call init_raf4_wrap(aspect,triad4,ngauss_smooth,rgauss_smooth, &
                        npass_smooth,normal_smooth,binom, &
                        ifilt_ord_smooth,filter_all, &
                        nsmooth_smooth,nsmooth_shapiro_smooth, &
                        nvars,idvar,kvar_start,kvar_end,var_names, &
                        indices, mype, npe)

     call raf_sm_reg(ens0f ,ngauss_smooth)
     call raf_sm_reg(ens0zf,ngauss_smooth)

  endif

  do k=1,nsig1o
     k1=levs_id(k)
     if (k1==izero) cycle
     if (kflag(k1) == izero) then
        ens0f (:,:,k)=zero_single
        ens0zf(:,:,k)=zero_single
     endif
  end do

  deallocate(field)
  deallocate(slab)
  deallocate(aslab)
  deallocate(pres)
  deallocate(h_loc)

end subroutine get_ensmber
!=======================================================================
!=======================================================================
subroutine set_range_aniall(aniall,isoasp,max_grad,min_grad)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:   set_range_aniall
! prgmmr: sato             org: np22                date: 2008-04-14
!
! abstract: set aniall range based on isoasp as:
!           isoasp(1:3)*min_grad < aniall(1:3) < isoasp(1:3)*max_grad
!
! program history log:
!   2008-04-14  sato
!
!   input argument list:
!    aniall         - anisotropic aspect tensor
!    isoasp         - isotropic aspect tensor
!    max_grad       - maximum gradient parameter ( <=0 -> no limit)
!    min_grad       - minimum gradient parameter ( <=0 -> no limit)
!   output argument list:
!    aniall         - anisotropic aspect tensor
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$ end documentation block
  implicit none
  real(r_single),intent(in   ) :: max_grad, min_grad
  real(r_single),intent(inout) :: aniall(6)
  real(r_single),intent(in   ) :: isoasp(3)

  real(r_single) :: asplim,coeff_asplim

  !--- for Z
  if(aniall(3)>zero_single) then
     if(min_grad > zero_single) then
        asplim=isoasp(3)*min_grad
        if(aniall(3)<asplim) then
           coeff_asplim=sqrt(aniall(3)/asplim)
           aniall(3)=asplim
           aniall(4)=aniall(4)/coeff_asplim
           aniall(5)=aniall(5)/coeff_asplim
        end if
     end if
     if(max_grad > zero_single) then
        asplim=isoasp(3)*max_grad
        if(aniall(3)>asplim) then
           coeff_asplim=sqrt(aniall(3)/asplim)
           aniall(3)=asplim
           aniall(4)=aniall(4)/coeff_asplim
           aniall(5)=aniall(5)/coeff_asplim
        end if
     end if
  end if

  !--- for Y
  if(min_grad > zero_single) then
     asplim=isoasp(1)*min_grad
     if( aniall(1)<asplim ) then
        coeff_asplim=sqrt(aniall(1)/asplim)
        aniall(1)=asplim
        aniall(5)=aniall(5)/coeff_asplim
        aniall(6)=aniall(6)/coeff_asplim
     end if
  end if
  if(max_grad > zero_single) then
     asplim=isoasp(1)*max_grad
     if( aniall(1)>asplim ) then
        coeff_asplim=sqrt(aniall(1)/asplim)
        aniall(1)=asplim
        aniall(5)=aniall(5)/coeff_asplim
        aniall(6)=aniall(6)/coeff_asplim
     end if
  end if

  !--- for X
  if(min_grad > zero_single) then
     asplim=isoasp(2)*min_grad
     if( aniall(2)<asplim ) then
        coeff_asplim=sqrt(aniall(2)/asplim)
        aniall(2)=asplim
        aniall(4)=aniall(4)/coeff_asplim
        aniall(6)=aniall(6)/coeff_asplim
     end if
  end if
  if(max_grad > zero_single) then
     asplim=isoasp(2)*max_grad
     if( aniall(2)>asplim ) then
        coeff_asplim=sqrt(aniall(2)/asplim)
        aniall(2)=asplim
        aniall(4)=aniall(4)/coeff_asplim
        aniall(6)=aniall(6)/coeff_asplim
     end if
  end if

  return
end subroutine set_range_aniall
!=======================================================================
!=======================================================================
subroutine mode_val(gx,nx,nsmp,gmod)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:   mode_val
! prgmmr: sato             org: np22                date: 2008-03-06
!
! abstract: obtain mode-like value
!
! program history log:
!   2008-03-06  sato
!
!   input argument list:
!    gx             - input array
!    nx             - array size
!    nsmp           - number of sections to make histogram
!   output argument list:
!    gmod           - avarage value of most frequent section
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$ end documentation block
  implicit none
  integer(i_kind),intent(in   ) :: nx,nsmp
  real(r_single) ,intent(in   ) :: gx(nx)
  real(r_single) ,intent(  out) :: gmod

  real(r_single) :: gmax,gmin
  real(r_single) :: ag_sum(nsmp)
  integer(i_kind):: ig_sum(nsmp)
  integer(i_kind):: ix,ig,ig_max

  ig_sum=izero
  ag_sum=zero_single
  gmax=maxval(gx)
  gmin=minval(gx)
  do ix=1,nx
     ig=max(min(int((gx(ix)-gmin)*nsmp/(gmax-gmin)),nsmp),ione)
     ig_sum(ig)=ig_sum(ig)+ione
     ag_sum(ig)=ag_sum(ig)+gx(ix)
  end do

  ig_max=ione
  do ig=2,nsmp
     if(ig_sum(ig_max)<ig_sum(ig)) ig_max=ig
  end do

  gmod=ag_sum(ig_max)/ig_sum(ig_max)

  write(6,*) 'mode_val:avg/mod',sum(ag_sum)/sum(ig_sum),gmod,ig_max

  return
end subroutine mode_val
!=======================================================================
!=======================================================================
subroutine writeout_isoscaleinfo(nvar,k1,asp1,asp2,asp3,dxfc,dyfc,aspx)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    writeout_isoscaleinfo
!   prgmmr:
!
! abstract:
!
! program history log:
!   2009-09-16  lueken - added subprogram doc block
!
!   input argument list:
!    nvar,k1
!    asp1,asp2,asp3
!    dxfc,dyfc
!    aspx
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

  implicit none

  integer(i_kind)      ,intent(in   ) :: nvar,k1
  real(r_kind)         ,intent(in   ) :: asp1,asp2,asp3
  real(r_kind)         ,intent(in   ) :: dxfc,dyfc
  real(r_kind),optional,intent(in   ) :: aspx(3)

  write(6,'("at domain center, var,k1,asp1,asp2,asp3,dxf,dyf =",2i4,5f11.3)') &
                nvar, k1,asp1,asp2,asp3,dxfc,dyfc

  if(present(aspx)) then
     write(6,'("max_asps, var,k1,asp1_max,asp2_max,asp3_max =",2i4,3f11.3)') &
                    nvar,k1,aspx(1),aspx(2),aspx(3)
  end if

  return
end subroutine
!=======================================================================
subroutine get2berr_reg_subdomain_option(mype)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    get2berr_reg_subdomain_option
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
!   2010-03-10  zhu   - make changes for generalizing control variable
!
!   input argument list:
!     mype     - mpi task id
!
!   output argument list:
!
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$ end documentation block
  use anberror, only: afact0
  use gridmod, only: istart,jstart
  use raflib, only: init_raf4
  implicit none

! Declare passed variables
  integer(i_kind),intent(in   ) :: mype

! Declare local variables
  integer(i_kind) n,i,j,k,l,lp,k1,kvar,ivar,im,ip,jm,jp,mm1,iloc,iploc,imloc,jloc,jploc,jmloc,igauss
  integer(i_kind) iglob,jglob

  real(r_kind) dl1,dl2,factk,factor,anhswgtsum
  real(r_kind) a1,a2,a3,a4,a5,a6,detai
  real(r_kind) biga1,biga2,biga3,biga4,biga5,biga6
  real(r_kind) fx2,fx1,fx3,dxi,dyi
  real(r_kind) asp1,asp2,asp3,factoz,afact
  real(r_kind) deta0,deta1

  real(r_kind),allocatable,dimension(:,:,:):: bckgvar0f
  real(r_single),allocatable,dimension(:,:)::bckgvar4,bckgvar4a,zsmooth4,zsmooth4a
  real(r_single),allocatable,dimension(:,:)::region_dx4,region_dy4,psg4,psg4a
  real(r_single),allocatable,dimension(:,:,:):: fltvals0,fltvals
  character*10 chvarname(nvars)
  character(80) fname
  character(5) cvar

  integer(i_kind):: ids,ide,jds,jde,kds,kde,ips,ipe,jps,jpe,kps,kpe
  integer(i_kind):: nlatf,nlonf


  ids=indices%ids; ide=indices%ide
  jds=indices%jds; jde=indices%jde
  kds=indices%kds; kde=indices%kde
  ips=indices%ips; ipe=indices%ipe
  jps=indices%jps; jpe=indices%jpe
  kps=indices%kps; kpe=indices%kpe
  nlatf=pf2aP1%nlatf
  nlonf=pf2aP1%nlonf

  allocate(asp10f(lat2,lon2))
  allocate(asp20f(lat2,lon2))
  allocate(asp30f(lat2,lon2))

  call get_background_subdomain_option(mype)

!-----define the anisotropic aspect tensor-----------------------------
!-------------------------------------------------------------------------------------
! Set up scales

! This first loop for nsig1o will be if we aren't dealing with
! surface pressure, skin temperature, or ozone

  allocate(aspect(7,ips:ipe,jps:jpe,kps:kpe))
  aspect(:,:,:,:)=zero
  mm1=mype+ione

  do k=kds,kde
     ivar=jdvar(k)
     k1=levs_jdvar(k)
     call isotropic_scales_subdomain_option(asp10f,asp20f,asp30f,k,mype)

     do j=jps,jpe
        jloc=j-jstart(mm1)+2_i_kind
        do i=ips,ipe
           iloc=i-istart(mm1)+2_i_kind

           asp1=asp10f(iloc,jloc)
           asp2=asp20f(iloc,jloc)
           asp3=asp30f(iloc,jloc)

           jp=min(nlonf,j+ione) ; jm=max(ione,j-ione)
           jploc=jp-jstart(mm1)+2_i_kind
           jmloc=jm-jstart(mm1)+2_i_kind
           dxi=one/(jp-jm)
 
           ip=min(nlatf,i+ione) ; im=max(ione,i-ione)
           iploc=ip-istart(mm1)+2_i_kind
           imloc=im-istart(mm1)+2_i_kind
           dyi=one/(ip-im)

           fx1= dyi*(z0f(iploc,jloc,k1)-z0f(imloc,jloc,k1))
           fx2= dxi*(z0f(iloc,jploc,k1)-z0f(iloc,jmloc,k1))
           fx3= zero

           rltop=rltop_wind
           afact=zero

           cvar=nrf_var(ivar)
           select case(cvar)
              case('sf','SF'); rltop=rltop_wind
              case('vp','VP'); rltop=rltop_wind
              case('ps','PS'); rltop=rltop_psfc
              case('t','T'); rltop=rltop_temp
              case('q','Q'); rltop=rltop_q
           end select
           if (jdvar(k) <= 5_i_kind)  afact=afact0(ivar)  !(use "zero" for isotropic computations)

           if (afact>zero) then
              asp1=scalex1*asp1
              asp2=scalex2*asp2
           endif


           if(i==nlatf/2.and.j==nlonf/2) then
              write(6,'("at domain center, var,k1,asp1,asp2,asp3 =",2i4,3f11.3)') &
                     jdvar(k),k1,asp1,asp2,asp3
              write(6,'("at domain center, var,k1,dxf,dyf =",2i4,3f11.3)') &
                     jdvar(k),k1,dxf(i,j),dyf(i,j)
           end if

           aspect(1,i,j,k) =   one/asp1**2 + afact*fx1*fx1/rltop**2  ! 1st (y) direction    x1*x1
           aspect(2,i,j,k) =   one/asp2**2 + afact*fx2*fx2/rltop**2  ! 2nd (x) direction    x2*x2
           aspect(3,i,j,k) =   one/asp3**2                           ! 3rd (z) direction    x3*x3
           aspect(4,i,j,k) =   afact*fx3*fx2/rltop**2                !  x3*x2
           aspect(5,i,j,k) =   afact*fx3*fx1/rltop**2                !  x3*x1
           aspect(6,i,j,k) =   afact*fx2*fx1/rltop**2                !  x2*x1
           aspect(7,i,j,k)=    zero

           if (volpreserve) then
              deta0=one/(asp1*asp2)**2
              deta1=aspect(1,i,j,k)*aspect(2,i,j,k)-aspect(6,i,j,k)*aspect(6,i,j,k)
              aspect(1,i,j,k) = aspect(1,i,j,k)/sqrt(deta1)*sqrt(deta0)
              aspect(2,i,j,k) = aspect(2,i,j,k)/sqrt(deta1)*sqrt(deta0)
              aspect(6,i,j,k) = aspect(6,i,j,k)/sqrt(deta1)*sqrt(deta0)
           endif

        end do
     end do
  end do

!  Invert to get true aspect tensor
  do k=kps,kpe
     if(levs_jdvar(k)==izero ) cycle
     do j=jps,jpe
        do i=ips,ipe
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


  if(mype==izero) write(6,*)'rltop_wind,rltop_temp,rltop_q,rltop_psfc=',&
                                    rltop_wind,rltop_temp,rltop_q,rltop_psfc

  if(mype==izero) write(6,*)' in get2berr_reg, nlat,nlon,nlatf,nlonf=',nlat,nlon,nlatf,nlonf

  if(mype==izero) write(6,*)' in get2berr_reg, ids,ide=',ids,ide
  if(mype==izero) write(6,*)' in get2berr_reg, jds,jde=',jds,jde
  write(6,*)'in get2berr_reg, mype,ips,ipe,jps,jpe,kps,kpe=',mype,ips,ipe,jps,jpe,kps,kpe

  if(lreadnorm) normal=izero

  call init_raf4(aspect,triad4,ngauss,rgauss,npass,normal,binom,ifilt_ord,filter_all, &
              nsmooth,nsmooth_shapiro, &
              nvars,idvar,kvar_start,kvar_end,var_names, &
              ids, ide, jds, jde, kds, kde, &         ! domain indices
              ips, ipe, jps, jpe, kps, kpe, &         ! patch indices
              mype, npe)

  call antest_maps0_subdomain_option(mype,theta0f,z0f)

  allocate(fltvals0(ngauss,nlatf,nlonf))
  allocate(fltvals(ngauss,nlatf,nlonf))


  do k=kps,kpe!      Effectively counting number of analysis variables


     fltvals0=zero_single
     fltvals=zero_single

     if (mype==izero) then
        open (94,file='fltnorm.dat_'//trim(chvarname(k)),form='unformatted')
        if(lreadnorm)  read(94) fltvals0
     endif

     if (lreadnorm) then
        call mpi_allreduce(fltvals0,fltvals,ngauss*nlatf*nlonf,mpi_real4,mpi_sum,mpi_comm_world,ierror)
     endif

     do igauss=1,ngauss
        do j=jps,jpe
           do i=ips,ipe
              if(lreadnorm) then
                 filter_all(1)%amp(igauss,i,j,k)=fltvals(igauss,i,j)
              else
                 fltvals(igauss,i,j)=filter_all(1)%amp(igauss,i,j,k)
              endif
           enddo
        enddo
     enddo
     if(.not.lreadnorm)  then 
        fltvals0=zero_single
        call mpi_reduce(fltvals,fltvals0,ngauss*nlatf*nlonf,mpi_real4,mpi_sum,izero,mpi_comm_world,ierror)
        if (mype==izero) write(94) fltvals0
     endif
     if (mype==izero) close(94)
  enddo

  allocate(bckgvar0f(ips:ipe,jps:jpe,kps:kpe))
  bckgvar0f=zero

!  filter normalized to unit amplitude.  now adjust amplitude to correspond
!  to desired error amplitude.
!
!                    (corz, corp contain sqrt(error variance) from background error file)
!                    (an_amp are input tuneable error amplitude parameters)

  factoz = 0.0002_r_kind*r25
  anhswgtsum=sum(anhswgt(1:ngauss))
  do k=kps,kpe
     if(k==ione)        fname='fltnorm.dat_psi'
     if(k==2_i_kind)    fname='fltnorm.dat_chi'
     if(k==3_i_kind)    fname='fltnorm.dat_ps'
     if(k==4_i_kind)    fname='fltnorm.dat_t'
     if(k==5_i_kind)    fname='fltnorm.dat_pseudorh'
     ivar=jdvar(k)
     kvar=levs_jdvar(k)
     do j=jps,jpe
        do i=ips,ipe
           l=max(min(int(rllatf(i,j)),mlat),ione)
           lp=min((l+ione),mlat)
           dl2=rllatf(i,j)-float(l)
           dl1=one-dl2
           if (nrf_3d(ivar)) then
              do n=1,nrf3
                 if (nrf3_loc(n)==ivar) then
                    factk=dl1*corz(l,kvar,n)+dl2*corz(lp,kvar,n)
                    exit
                 end if
              end do
           else
              do n=1,nrf2
                 if (nrf2_loc(n)==ivar) then
                    factk=dl1*corp(l,n)+dl2*corp(lp,n)
                    exit
                 end if
              end do
           end if

           do igauss=1,ngauss
              ! if (j==(jps+2_i_kind) .and. (i==(ips+2_i_kind) .or. i==(ips+8_i_kind))) then
              !    print*,'in anisofilter,mype,k,ivar,i,j,factk=',mype,k,ivar,i,j,factk
              ! endif
              factor=anhswgt(igauss)*factk*an_amp(igauss,ivar)/sqrt(anhswgtsum)
              filter_all(1)%amp(igauss,i,j,k)=factor*filter_all(1)%amp(igauss,i,j,k)
              bckgvar0f(i,j,k)=factor
           end do
        end do
     end do
  end do


! write out a few bckg fields and the error variances. For layer constant
! statistics, these are good for post-processing purposes on the real
! model grid. Interface between filter grid and model domain needed
! for non-constant statistics!

  allocate(bckgvar4a(nlat,nlon))
  allocate(bckgvar4(nlat,nlon))


  do k=kps,kpe
     bckgvar4a=zero
     do j=jps,jpe
        do i=ips,ipe
           bckgvar4a(i,j)=bckgvar0f(i,j,k)
        end do
     end do
     call mpi_reduce(bckgvar4a,bckgvar4,nlat*nlon,mpi_real4,mpi_sum,izero,mpi_comm_world,ierror)
     if(mype==izero) then
        ivar=jdvar(k)
        open (94,file='bckgvar.dat_'//trim(chvarname(ivar)),form='unformatted')
        write(94) bckgvar4
        close(94)
     end if
  enddo

  allocate(zsmooth4a(nlat,nlon))
  allocate(zsmooth4(nlat,nlon))
  zsmooth4a=zero
  do j=2,lon2-ione
     jglob=j+jstart(mm1)-2_i_kind
     do i=2,lat2-ione
        iglob=i+istart(mm1)-2_i_kind
        zsmooth4a(iglob,jglob)=z0f(i,j,1)
     end do
  end do
  call mpi_reduce(zsmooth4a,zsmooth4,nlat*nlon,mpi_real4,mpi_sum,izero,mpi_comm_world,ierror)

  allocate(psg4a(nlat,nlon))
  allocate(psg4(nlat,nlon))
  psg4a=zero
  do j=2,lon2-ione
     jglob=j+jstart(mm1)-2_i_kind
     do i=2,lat2-ione
        iglob=i+istart(mm1)-2_i_kind
        psg4a(iglob,jglob)=psg(i,j,1)
     end do
  end do
  call mpi_reduce(psg4a,psg4,nlat*nlon,mpi_real4,mpi_sum,izero,mpi_comm_world,ierror)

  allocate(region_dy4(nlat,nlon),region_dx4(nlat,nlon))

  if (mype==izero) then
     region_dx4=region_dx
     region_dy4=region_dy
     open (94,file='bckg_dxdy.dat',form='unformatted')
     write(94) region_dx4,region_dy4
     close(94)

     open (94,file='bckg_psfc.dat',form='unformatted')
     write(94) psg4
     close(94)

     open (94,file='bckg_z.dat',form='unformatted')
     write(94) zsmooth4
     close(94)
  end if


  deallocate(bckgvar0f)
  deallocate(bckgvar4a)
  deallocate(bckgvar4)
  deallocate(region_dy4,region_dx4)
  deallocate(corz,corp,hwll,hwllp,vz,aspect)
  deallocate(dxf,dyf,rllatf,theta0f)
  deallocate(u0f,v0f,z0f)
  deallocate(zsmooth4a,psg4a)
  deallocate(zsmooth4,psg4)
  deallocate(fltvals0)
  deallocate(fltvals)

end subroutine get2berr_reg_subdomain_option
!=======================================================================
!=======================================================================
subroutine get_background_subdomain_option(mype)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:   get_background_subdomain_option
! prgmmr: pondeca          org: np22                date: 2006-08-01
!
! abstract: compute background fields and their spatial derivatives
!           on filter grid for use in anisotropic covariance model.
!           built from parrish's old anprewgt_reg
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
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$ end documentation block
  use raflib, only: init_raf4,raf_sm4,raf_sm4_ad
  use gridmod, only: istart,jstart
  use anberror, only: halo_update_reg
  use guess_grids, only: isli2
  implicit none

! Declare passed variables
  integer(i_kind),intent(in   ) :: mype

! Declare local variables
  integer(i_kind) i,j,k,mm1
  integer(i_kind) n,iloc,jloc
  integer(i_kind) kds0,kde0,kps0,kpe0
  integer(i_kind) nvars0
  integer(i_kind),allocatable::idvar0(:),kvar_start0(:),kvar_end0(:)
  character(80),allocatable::var_names0(:)

  real(r_kind),allocatable,dimension(:,:,:)::field2
  real(r_single),allocatable,dimension(:,:,:)::field

  integer(i_long):: ngauss_smooth,npass_smooth,normal_smooth,ifilt_ord_smooth
  integer(i_long):: nsmooth_smooth,nsmooth_shapiro_smooth
  real(r_double) :: rgauss_smooth(1)

  integer(i_kind):: ids,ide,jds,jde,kds,kde,ips,ipe,jps,jpe,kps,kpe
  integer(i_kind):: nlatf,nlonf,it

  ids=indices%ids; ide=indices%ide
  jds=indices%jds; jde=indices%jde
  kds=indices%kds; kde=indices%kde
  ips=indices%ips; ipe=indices%ipe
  jps=indices%jps; jpe=indices%jpe
  kps=indices%kps; kpe=indices%kpe
  nlatf=pf2aP1%nlatf
  nlonf=pf2aP1%nlonf

!   get dxf,dyf,rllatf
!       note: if filter grid coarser than analysis grid, then normalized
!       adjoint of filter to analysis interpolation used to transfer fields
!       from analysis grid to filter grid. otherwise, normal interpolation
!       is done. this is transparent at this level. it appears in the
!       definition of the interpolation and adjoint of interpolation
!       weights. check for accuracy.(done).

  if(nlat/=nlatf.or.nlon/=nlonf) then
     if(mype==izero) &
        write(6,*)' rtma_subdomain_option true, nlat ne nlatf and/or nlon ne nlonf, nlat,nlatf,nlon,nlonf=', &
                          nlat,nlatf,nlon,nlonf
     call mpi_finalize(i)
     stop
  end if

  mm1=mype+ione

! define local vertical index for smoothing single variables
  kds0=ione
  kde0=nsig
  kps0=kds0
  kpe0=kde0
  nvars0=ione
  allocate(idvar0(kds0:kde0),kvar_start0(nvars0),kvar_end0(nvars0))
  allocate(var_names0(nvars0))
  idvar0=ione
  kvar_start0=ione
  kvar_end0=nsig
  var_names0(1)='background'
! ------------------------------------------------------------
! ------------ in this section, set up isotropic filter for
! ------------ generating smoothed guess
! ------------------------------------------------------------

  allocate(aspect(7,ips:ipe,jps:jpe,kps0:kpe0))

  do k=kps0,kpe0
     do j=jps,jpe
        do i=ips,ipe

           aspect(1,i,j,k)=hsmooth_len**2
           aspect(2,i,j,k)=hsmooth_len**2
           aspect(3,i,j,k)=one**2
           aspect(4:7,i,j,k)=zero
 
        end do
     end do
  end do

  ngauss_smooth=ione
  rgauss_smooth=one
  npass_smooth=ione
  normal_smooth=izero
  ifilt_ord_smooth=4_i_kind
  nsmooth_smooth=izero
  nsmooth_shapiro_smooth=izero
  call init_raf4(aspect,triad4,ngauss_smooth,rgauss_smooth, &
                 npass_smooth,normal_smooth,binom, &
                 ifilt_ord_smooth,filter_all, &
                 nsmooth_smooth,nsmooth_shapiro_smooth, &
                 nvars0,idvar0,kvar_start0,kvar_end0,var_names0, &
                 ids, ide, jds, jde, kds0, kde0, &         ! domain indices
                 ips, ipe, jps, jpe, kps0, kpe0, &         ! patch indices
                 mype, npe)

  it=ntguessig

  allocate(field(ips:ipe,jps:jpe,kps0:kpe0),field2(lat2,lon2,nsig))
  allocate(theta0f(lat2,lon2,nsig))
  allocate(    u0f(lat2,lon2,nsig))
  allocate(    v0f(lat2,lon2,nsig))
  allocate(    z0f(lat2,lon2,nsig))
  do n=1,4
 
     do k=kps0,kpe0
        do j=jps,jpe
           jloc=j-jstart(mm1)+2_i_kind
           do i=ips,ipe
              iloc=i-istart(mm1)+2_i_kind
              if (n==ione)        field(i,j,k)=ges_tv(iloc,jloc,k,it)/(ges_prsl(iloc,jloc,k,it)/r100)**rd_over_cp
              if (n==2_i_kind)    field(i,j,k)=ges_u(iloc,jloc,k,it)
              if (n==3_i_kind)    field(i,j,k)=ges_v(iloc,jloc,k,it)
              if (n==4_i_kind)    then
                 if ( min(max(isli2(iloc,jloc),izero),ione)==izero ) then
                    field(i,j,k)=ges_z(iloc,jloc,it)-hsteep
                 else
                    field(i,j,k)=ges_z(iloc,jloc,it)
                 end if
              endif
           end do
        end do
     end do
     if (n<=3_i_kind .or. (n==4_i_kind .and. lsmoothterrain)) then
        call raf_sm4(field,filter_all,ngauss_smooth,ips,ipe,jps,jpe,kps0,kpe0,npe)
        call raf_sm4_ad(field,filter_all,ngauss_smooth,ips,ipe,jps,jpe,kps0,kpe0,npe)
     endif
     do k=kps0,kpe0
        do j=jps,jpe
           jloc=j-jstart(mm1)+2_i_kind
           do i=ips,ipe
              iloc=i-istart(mm1)+2_i_kind
              field2(iloc,jloc,k)=field(i,j,k)
           end do
        end do
     end do
     call halo_update_reg(field2,nsig)
     do k=1,nsig
        do j=1,lon2
           do i=1,lat2
              if (n==ione)    theta0f(i,j,k)=field2(i,j,k)
              if (n==2_i_kind)    u0f(i,j,k)=field2(i,j,k)
              if (n==3_i_kind)    v0f(i,j,k)=field2(i,j,k)
              if (n==4_i_kind)    z0f(i,j,k)=field2(i,j,k)
           end do
        end do
     end do
  end do
  allocate(psg(lat2,lon2,nsig))
  do k=1,nsig
     do j=1,lon2
        do i=1,lat2
           psg(i,j,k)=1000._r_single*ges_ps(i,j,it)
        end do
     end do
  end do

 deallocate(field)
 deallocate(field2)
 deallocate(aspect)
 deallocate(idvar0,kvar_start0,kvar_end0,var_names0)

end subroutine get_background_subdomain_option
!=======================================================================
!=======================================================================
subroutine isotropic_scales_subdomain_option(scale1,scale2,scale3,k,mype)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:   isotropic_scales_subdomain_option
! prgmmr: pondeca          org: np22                date: 2008-01-27
!
! abstract: compute isotropic length scales of auto-correlation model.
!           built from parrish's old anprewgt_reg
!
! program history log:
!   2006-08-01  pondeca
!   2010-03-10  zhu - add flexibility for more control variables
!
!   input argument list:
!    mype     - mpi task id
!    k        - level number of field in subdomain mode
!
!   output argument list:
!    scale1     - 2d field of correlations lengths in the x-direction
!    scale2     - 2d field of correlations lengths in the y-direction
!    scale3     - 2d field of correlations lengths in the vertical direction
!
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$ end documentation block
  use gridmod,only:istart,jstart
  use guess_grids, only: isli2
  implicit none

! Declare passed variables
  integer(i_kind),intent(in   ) :: k, mype

  real(r_kind)   ,intent(  out) :: scale1(lat2,lon2)
  real(r_kind)   ,intent(  out) :: scale2(lat2,lon2)
  real(r_kind)   ,intent(  out) :: scale3(lat2,lon2)

! Declare local variables
  integer(i_kind) n,i,j,k1,ivar,l,lp,iglob,jglob,mm1

  real(r_kind) dl1,dl2,hwll_loc
  real(r_kind),dimension(pf2aP1%nlatf,pf2aP1%nlonf):: &
               scaleaux1,scaleauxa, &
               scaleaux2,scaleauxb

  integer(i_kind):: nlatf,nlonf,it
  nlatf=pf2aP1%nlatf
  nlonf=pf2aP1%nlonf

  mm1=mype+ione

  k1=levs_jdvar(k)
  ivar=jdvar(k)

  do j=1,lon2
     jglob=j+jstart(mm1)-2_i_kind
     if(jglob<ione.or.jglob>nlonf) cycle
     do i=1,lat2
        iglob=i+istart(mm1)-2_i_kind
        if(iglob<ione.or.iglob>nlatf) cycle

        if (nrf_3d(ivar)) then
           do n=1,nrf3
              if (nrf3_loc(n)==ivar) then
                 l=int(rllatf(iglob,jglob))
                 lp=l+ione
                 dl2=rllatf(iglob,jglob)-float(l)
                 dl1=one-dl2
                 hwll_loc=dl1*hwll(l,k1,n)+dl2*hwll(lp,k1,n)
                 scale3(i,j)=one/vz(k1,l,n)
                 exit
              end if
           end do
        else
           do n=1,nrf2
              if (nrf2_loc(n)==ivar) then
                 l=int(rllatf(iglob,jglob))
                 lp=l+ione
                 dl2=rllatf(iglob,jglob)-float(l)
                 dl1=one-dl2
                 hwll_loc=dl1*hwllp(l,n)+dl2*hwllp(lp,n)
                 scale3(i,j)=one
                 exit
              end if
           end do
        end if


        scale1(i,j)=hwll_loc/dyf(iglob,jglob)
        scale2(i,j)=hwll_loc/dxf(iglob,jglob)

        if (.not.latdepend) then
           scale1(i,j)=max(scale1(i,j),scale2(i,j))
           scale2(i,j)=scale1(i,j)
        endif

        !rescaling to roughly match original analysis from purely isotropic
        !option, ie.. when anisotropic=.false. in namelist "anbkgerr".

        scale1(i,j)=rfact0h(jdvar(k))*scale1(i,j)
        scale2(i,j)=rfact0h(jdvar(k))*scale2(i,j)

        if (.not.twodvar_regional) then
           if (jdvar(k) /=3_i_kind .and. jdvar(k) /=7_i_kind .and. &
               jdvar(k) /=9_i_kind .and. jdvar(k) /=10_i_kind ) &
              scale3(i,j)=rfact0v(jdvar(k))*scale3(i,j)
        endif

     enddo
  enddo

  if (lwater_scaleinfl .and. water_scalefact(jdvar(k))/=zero) then
     it=ntguessig
     do j=1,lon2
        do i=1,lat2
           if ( min(max(isli2(i,j),izero),ione)==izero ) then
              scale1(i,j)=water_scalefact(jdvar(k))*scale1(i,j)
              scale2(i,j)=water_scalefact(jdvar(k))*scale2(i,j)
           endif
        enddo
     enddo

     if (nhscale_pass>izero) then
        scaleauxa=zero
        scaleauxb=zero
        do j=2,lon2-ione
           jglob=j+jstart(mm1)-2_i_kind
           if(jglob<ione.or.jglob>nlonf) cycle
           do i=2,lat2-ione
              iglob=i+istart(mm1)-2_i_kind
              if(iglob<ione.or.iglob>nlatf) cycle
              scaleauxa(iglob,jglob)=scale1(i,j)
              scaleauxb(iglob,jglob)=scale2(i,j)
           enddo
        enddo
        call mpi_allreduce(scaleauxa,scaleaux1,nlatf*nlonf,mpi_rtype,mpi_sum,mpi_comm_world,ierror)
        call mpi_allreduce(scaleauxb,scaleaux2,nlatf*nlonf,mpi_rtype,mpi_sum,mpi_comm_world,ierror)

        call smther_one_8(scaleaux1,ione,nlatf,ione,nlonf,nhscale_pass)
        call smther_one_8(scaleaux2,ione,nlatf,ione,nlonf,nhscale_pass)

        do j=1,lon2
           jglob=j+jstart(mm1)-2_i_kind
           if(jglob<ione.or.jglob>nlonf) cycle
           do i=1,lat2
              iglob=i+istart(mm1)-2_i_kind
              if(iglob<ione.or.iglob>nlatf) cycle
              scale1(i,j)=scaleaux1(iglob,jglob)
              scale2(i,j)=scaleaux2(iglob,jglob)
           enddo
        enddo
     endif
  endif

  return
end subroutine isotropic_scales_subdomain_option

end module anisofilter
