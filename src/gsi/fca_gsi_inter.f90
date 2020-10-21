module fca_gsi_inter_m
!$$$ module documentation block
!           .      .    .                                       .
! module:   fca_gsi_inter_m
!  prgmmr: Nehrkorn
!
! abstract: GSI-FCA interface routines: xfer of WRF model state between GSI and FCA
!
! program history log:
!
! subroutines included:
!   sub  initi_fca_from_gsi
!   sub  ges_to_fca_wrf
!   sub  sval_to_disp_grid
!   sub  sval_to_disp_grid_adj
!   sub  fca_state_to_sval
!   sub  fca_state_to_sval_adj
!   sub  print_halos
!
! functions included:
!
! variable definition:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

  use fp_types_m, only: fp
!domain and memory limits, tile and patch limits, FCA constants
  use core_disp_types_m, only: fca_gridded_disp, ids,ide,jds,jde,kds,kde, ims,ime,jms,jme,kms,kme, &
       its,ite,jts,jte,kts,kte, ips,ipe,jps,jpe,kps,kpe, &
       bilinear, bicubic
  
  use fca_wrf_grid_types_m, only: fca_wrf_grid
  use displace_wrf_m, only: base_pres, t0, kappa, calc_ph_hyd_wrf, Rd, Rv, grav, cp

  use constants, only: zero, one, two, r1000, r100, max_varname_length
  use control_vectors, only: cvars3d
  use gridmod, only: aeta1_ll, eta1_ll, aeta2_ll, eta2_ll, idpsfc5, idthrm5, pt_ll, &
       nlat,nlon,nsig,lat1,lon1,lon2,lat2,istart,jstart,ilat1,jlon1,regional, &
       wrf_mass_regional,fv3_regional
  use file_utility, only : get_lun
  use gsi_bundlemod, only : GSI_BundleGetPointer, gsi_bundle
  use gsi_chemguess_mod, only: gsi_chemguess_get,gsi_chemguess_bundle
  use gsi_4dvar, only: nsubwin
  use gsi_metguess_mod, only: gsi_metguess_get,gsi_metguess_bundle
  use kinds, only: r_kind,i_kind
  use mpeu_util, only: getindex
  use mpimod, only: mype, npe
  use general_sub2grid_mod, only: general_sub2grid,general_grid2sub,sub2grid_info,general_sub2grid_create_info
  use general_commvars_mod, only: s2g_raf
  use wrf_vars_mod, only : dbz_exist
  use obsmod,only: if_model_dbz

  implicit none

  private
  public :: initi_fca_from_gsi, ges_to_fca_wrf, sval_to_disp_grid, fca_state_to_sval, &
       sval_to_disp_grid_adj, fca_state_to_sval_adj

  integer, parameter, public :: idebug=0 ! 3 - enable full debugging prints; 2 - enable some debugging prints/halo outputs; 1 - to enable FCA disp prints

! namelist variables in /SETUP/:
  logical, public :: fca_switch=.false.  !if .true., activate displacement algorithm in pcgsoi.f90
  real, public :: uv_zlevel_par = 0.5  !fraction of vertical column where to place uv_zlevel: 0 - lowest, 1 - highest

! constants used in WRF FCA modules
! Because GSI uses a halo of one, need to restrict interpolation order to bilinear:
  integer, parameter, public :: fca_interp_order=bilinear !order of horizontal interpolation to displacement origin: 1 - bilinear, 3- bicubic
  integer, parameter, public :: th_compute_par = 1 !choice for displacement of theta (1) or temperature (0) along model surfaces
  integer, parameter, public :: p_qv=1 
! not needed:  integer :: cv_options_hum =1  !option for choice of analysis moisture variable
! TBD: not needed for now: integer :: p_qc=2, p_qr=3, p_qi=4, p_qs=5, p_qg=6

  integer, public :: haloi, haloj ! computed in initi_fca_from_gsi from gridmod variables
  integer, public :: nmoist ! computed in initi_fca_from_gsi
  character (len=max_varname_length), allocatable :: moistnames(:) ! allocated/populated in initi_fca_from_gsi
  integer, allocatable :: moistguess(:) ! 1 -metguess; 2 - chemguess
  
! constants used in this module
  real(fp) :: r622 = 0.622_fp, t300=300._fp

  type(sub2grid_info),save :: s2g_press

contains

subroutine initi_fca_from_gsi
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram: initi_fca_from_gsi
!   prgmmr: Nehrkorn
!
! abstract: Initialize FCA variables from GSI
!
! program history log:
!
!   input argument list: 
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block
  implicit none
  integer :: inner_vars, num_fields, icloudin, icloudout, ioff, istatus, n_actual_clouds, ngases
  character (len=max_varname_length), allocatable :: cloudnames(:), gasnames(:)

  if (.not. wrf_mass_regional .and. .not. fv3_regional) then
     if (mype .eq. 0) write (*,*) 'initi_fca_from_gsi: fca_switch=.TRUE. is only supported for ',&
          'wrf_mass_regional=.TRUE. or fv3_regional=.TRUE.'
     stop 'initi_fca_from_gsi: unsupported configuration for fca_switch=.TRUE.'
  end if
  
  ! domain dimensions (whole grid, no buffer)
  ! NOTE: we follow the storage convention of GSI here: (lat,lon,sig) 
  !       rather than WRF (west_east,south_north,vert) to avoid
  !       having to transpose arrays. 
  ! But note that we pad ide/jde by one gridpoint to account for staggered array usage in WRFDA and compute_xy_orig:
  ! (Note that since ide/jde is padded by one for staggered grids, we need to use ide-1 in the comparison below)
  ids=1
  jds=1
  kds=1
  jde=nlon+1
  ide=nlat+1
  kde=nsig

  ! tile/patch dimensions (no buffer)
  jts = jstart(mype+1) !lon
  jte = jts + lon1-1
  its = istart(mype+1) !lat
  ite = its + lat1-1
  kts=kds
  kte=kde

  jps=jts
  jpe=jte
  ips=its
  ipe=ite
  kps=kts
  kpe=kte

  !memory limits:
  haloi = (lat2-lat1)/2
  haloj = (lon2-lon1)/2

  ims = its-haloi
  ime = ite+haloi
  jms = jts-haloj
  jme = jte+haloj
  kms=kts
  kme=kte+1 ! to allow for vertically staggered arrays

  !debug prints:
  if (idebug .ge. 1) &
     write (*,'(a,/,a,/,20I5)') 'initi_fca_from_gsi: ',&
     'mype nsig nlon lon1 lon2 nlat lat1 lat2 its  ite  ims  ime  jts  jte  jms  jme  kms  kme  kts  kte:',&
     mype, nsig, nlon, lon1, lon2, nlat, lat1, lat2, its, ite, ims, ime, jts, jte, jms, jme, kms, kme, kts, kte

  inner_vars = 1
  num_fields = 1 + nsig
  call general_sub2grid_create_info(s2g_press,inner_vars,nlat,nlon,nsig,num_fields,regional,s_ref=s2g_raf)

  nmoist = 1 !for p_qv, which is hardwired to 1
  ! Inquire about cloud guess fields (follows similar code in control2state)
  ! (simplified/more general version of what's in wrwrfmassa_netcdf_wrf in cplr_wrwrfmassa.f90)
  call gsi_metguess_get('clouds::3d',n_actual_clouds,istatus)
  if (mype .eq. 0) write (*,*) 'initi_fca_from_gsi: n_actual_clouds=',n_actual_clouds
  if (n_actual_clouds .gt. 0 .and. istatus .eq. 0) then
     if (dbz_exist .and. if_model_dbz) n_actual_clouds=n_actual_clouds+1
     allocate(cloudnames(n_actual_clouds))
     call gsi_metguess_get('clouds::3d',cloudnames,istatus)
     if (istatus .ne. 0) then
        if (mype .eq. 0) write (*,*) 'initi_fca_from_gsi: Failure to get cloudnames with istatus=',istatus
        n_actual_clouds=0
     else
        if (dbz_exist .and. if_model_dbz) then
           ! check for presence 'dbz' in guess, add to cloudnames if needed
           call gsi_metguess_get ( 'var::dbz', icloudin, istatus )
           if (icloudin .gt. 0 .and. getindex(cloudnames,'dbz') .le. 0) then
              cloudnames(n_actual_clouds)='dbz'
           else
              ! adjust since either dbz is already in cloudnames, or missing from guess
              n_actual_clouds=n_actual_clouds-1
           end if
        end if !dbz_exist
     end if !endelse istatus (cloudnames)
  end if !n_actual_clouds > 0
  if (n_actual_clouds .gt. 0) then
     ! Look for cloudnames in control vector (this assumes cloudnames map one-to-one control to state vector)
     if (mype .eq. 0) write (*,'(a,i4,a,/,(20a10))') &
          'initi_fca_from_gsi: Found ',n_actual_clouds,' cloudnames:',cloudnames(1:n_actual_clouds)
     icloudout=0 !
     do icloudin=1,n_actual_clouds
        istatus=getindex(cvars3d,cloudnames(icloudin))
        if (istatus .gt. 0) then
           icloudout=icloudout+1
           if (icloudout .lt. icloudin) cloudnames(icloudout)=cloudnames(icloudin)
        endif
     end do
     if (icloudout .lt. n_actual_clouds) then
        n_actual_clouds=icloudout
        if (mype .eq. 0) write (*,'(a,i4,a,/,(20a10))') &
             'initi_fca_from_gsi: Only found ',n_actual_clouds,' cloudnames in control_vector (cvars3d):',&
             cloudnames(1:n_actual_clouds)
     end if
     nmoist = nmoist + n_actual_clouds
  end if
  
  ! handling of chemical tracers
  call gsi_chemguess_get('dim',ngases,istatus)
  if (mype .eq. 0) write (*,*) 'initi_fca_from_gsi: ngases=',ngases
  if (ngases .gt. 0 .and. istatus .eq. 0) then
     allocate(gasnames(ngases))
     call gsi_chemguess_get('gsinames',gasnames,istatus)
     if (istatus .ne. 0) then
        if (mype .eq. 0) write (*,*) 'initi_fca_from_gsi: Failure to get gasnames with istatus=',istatus
        ngases=0
     endif
  end if
  if (ngases .gt. 0) then
     ! Look for gasnames in control vector (this assumes gasnames map one-to-one control to state vector)
     if (mype .eq. 0) write (*,'(a,i4,a,/,(20a10))') &
          'initi_fca_from_gsi: Found ',ngases,' gasnames:',gasnames(1:ngases)
     icloudout=0 !
     do icloudin=1,ngases
        istatus=getindex(cvars3d,gasnames(icloudin))
        if (istatus .gt. 0) then
           icloudout=icloudout+1
           if (icloudout .lt. icloudin) gasnames(icloudout)=gasnames(icloudin)
        endif
     end do
     if (icloudout .lt. ngases) then
        ngases=icloudout
        if (mype .eq. 0) write (*,'(a,i4,a,/,(20a10))') &
             'initi_fca_from_gsi: Only found ',ngases,' gasnames in control_vector (cvars3d):',&
             gasnames(1:ngases)
     end if
     nmoist = nmoist + ngases
  end if
  
  ! store cloudnames and gasnames in moistnames:
  allocate(moistnames(nmoist),moistguess(nmoist))
  moistnames(1)='q'
  ioff=1
  if (n_actual_clouds .gt. 0) &
       moistnames(ioff+1:ioff+n_actual_clouds)=cloudnames(1:n_actual_clouds)
  ioff=ioff+n_actual_clouds
  moistguess(1:ioff)=1 !metguess
  if (ngases .gt. 0) then
     moistnames(ioff+1:ioff+ngases)=gasnames(1:ngases)
     moistguess(ioff+1:ioff+ngases)=2 !chemguess
  endif
     
end subroutine initi_fca_from_gsi

subroutine ges_to_fca_wrf(bg_state, ierror)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram: ges_to_fca_wrf
!   prgmmr: Nehrkorn
!
! abstract: xfer GSI guess fields to bg_state (an instance of fca_wrf_grid)
!
! program history log:
!
!   input argument list: 
!    bg_state
!   output argument list:
!    bg_state, ierror
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block
  implicit none
  type(fca_wrf_grid), intent(inout):: bg_state
  integer, intent(out) :: ierror

  real(r_kind),dimension(:,:  ),pointer::ges_ps=>NULL()
  real(r_kind),dimension(:,:  ),pointer::ges_z=>NULL()
  real(r_kind),dimension(:,:,:),pointer::ges_u=>NULL()
  real(r_kind),dimension(:,:,:),pointer::ges_v=>NULL()
  real(r_kind),dimension(:,:,:),pointer::ges_q=>NULL()
  real(r_kind),dimension(:,:,:),pointer::ges_prse=>NULL()
  real(r_kind),dimension(:,:,:),pointer::ges_tv=>NULL()
  real(r_kind),dimension(:,:,:),pointer::ges_rank3=>NULL()
  integer(i_kind) :: i,j,k, imoist, istatus

  real(fp), parameter :: WRFB_A=50., WRFB_T0=300., WRFB_P0=1.E5, cv=cp-Rd, p1000mbPa=1.E5
!!$  real(fp), allocatable :: c1f(:), c2f(:) !not needed
  real(fp), allocatable :: phm(:,:), pfu(:,:), pfd(:,:)
  
  ! 2d fields:
  ierror = 1 ! 1: sfc pressure
  call gsi_bundlegetpointer (gsi_metguess_bundle(1),'ps',ges_ps,  istatus)
  if (istatus .ne. 0) return

  bg_state%PSFC(ims:ime,jms:jme) = r1000*ges_ps(1:lat2,1:lon2)

  if (idebug .ge. 3 .and.  its .le. 30 .and. ite .ge. 30 .and. jts .le. 24 .and. jte .ge. 24) then
     print *,'Sample point/profile in ges_to_fca:'
     print *,'PSFC[30,24] ',bg_state%PSFC(30,24)
  end if
  
!OPEN(UNIT=13, FILE="ges_ps.txt", ACTION="write", STATUS="replace", &
!       FORM="unformatted")
!  WRITE(13) ges_ps
!  CLOSE(UNIT=13)

!  OPEN(UNIT=12, FILE="ges_ps.txt", ACTION="write", STATUS="replace")
!  DO i=1,71
!    WRITE(12,*) (ges_ps(i,j), j=1,61)
!  END DO


  ierror=ierror+1 ! 2: terr
  call gsi_bundlegetpointer (gsi_metguess_bundle(1),'z',ges_z,istatus)
  if (istatus .ne. 0) return
  bg_state%HGT(ims:ime,jms:jme) = ges_z(1:lat2,1:lon2)

  if (idebug .ge. 3 .and.  its .le. 30 .and. ite .ge. 30 .and. jts .le. 24 .and. jte .ge. 24) then
     print *,'HGT[30,24] ',bg_state%HGT(30,24)
  end if
  
  ! ZNU, and znw, c1h, c2h, c3h, c4h, c3f, c4f
  if (wrf_mass_regional) then
     ! first store c3 and c4 from eta1/eta2:
     ! Note: units of pt_ll, eta2_ll are in hPa, convert to Pa:
     bg_state%c3h(kts:kte) = aeta1_ll(1:nsig)
     bg_state%c3f(kts:kte+1) = eta1_ll(1:nsig+1)
     bg_state%c4h(kts:kte) = r100*aeta2_ll(1:nsig)
     bg_state%c4f(kts:kte+1) = r100*eta2_ll(1:nsig+1)
     bg_state%ptop = r100*pt_ll
  elseif (fv3_regional) then
     ! first store c3 and c4 from eta1/eta2:
     ! Note: eta1/eta2 are switched compared to wrf-arw in the GSI
     ! Note: units of eta1_ll are in kPa, convert to Pa:
     !       see: gsi_rfv3io_get_grid_specs (gsi_rfv3io_mod.f90), getprs.f90
     bg_state%c3h(kts:kte) = aeta2_ll(1:nsig)
     bg_state%c3f(kts:kte+1) = eta2_ll(1:nsig+1)
     bg_state%c4h(kts:kte) = r1000*aeta1_ll(1:nsig)
     bg_state%c4f(kts:kte+1) = r1000*eta1_ll(1:nsig+1)
     bg_state%ptop = 0.
  end if
  
  ! Then compute eta from: eta=c3+c4/(po-ptop) (formulas from module_initialize_real):
  bg_state%znu(kts:kte) = bg_state%c3h(kts:kte) + bg_state%c4h(kts:kte)/(p1000mbPa-bg_state%ptop)
  bg_state%znw(kts:kte+1) = bg_state%c3f(kts:kte+1) + bg_state%c4f(kts:kte+1)/(p1000mbPa-bg_state%ptop)
  ! Now compute c1 and c2 (formulas from module_initialize_real):
! interface values of c1,c2 not needed
!!$  allocate(c1f(kms:kme),c2f(kms:kme))
!!$  c1f(kts) = 1.
!!$  if (all(abs(bg_state%c4f(kts:kte+1)) .lt. 1.E-4)) then
!!$     ! terrain-following (old eta) coords:
!!$     c1f(kte+1) = 1.
!!$  else
!!$     ! hybrid coords:
!!$     c1f(kte+1) = 0.
!!$  end if
!!$  do k=kts+1,kte
!!$     c1f(k) = ( bg_state%c3h(k) - bg_state%c3h(k-1) ) / ( bg_state%znu(k) - bg_state%znu(k-1) )
!!$  end do
!!$  DO k=kts, kte+1
!!$     c2f(k) = ( 1. - c1f(k) ) * ( p1000mbPa - bg_state%ptop )
!!$  ENDDO
  DO k=kts, kte
     bg_state%c1h(k) = ( bg_state%c3f(k+1) - bg_state%c3f(k) ) / ( bg_state%znw(k+1) - bg_state%znw(k) )
     bg_state%c2h(k) = ( 1. - bg_state%c1h(k) ) * ( p1000mbPa - bg_state%ptop )
  ENDDO
  
  if (idebug .ge. 3 .and.  its .le. 30 .and. ite .ge. 30 .and. jts .le. 24 .and. jte .ge. 24) then
     print *,'bg_state%ZNU',bg_state%ZNU
     print *,'bg_state%ZNW',bg_state%ZNW
     print *,'bg_state%C1H',bg_state%C1H
     print *,'bg_state%C2H',bg_state%C2H
     print *,'bg_state%C3H',bg_state%C3H
     print *,'bg_state%C4H',bg_state%C4H
     print *,'bg_state%C3F',bg_state%C3F
     print *,'bg_state%C4F',bg_state%C4F
  end if
!!$  deallocate(c1f,c2f) !not needed

!!$    bg_state%MUB(:,:) = ! in WRFDA: grid%mub(ims:ime,jms:jme)
!!$    bg_state%MU(:,:) = !  in WRFDA: grid%xb%psac(ims:ime,jms:jme) - wrf_state%mub
 
  ! Compute MUB from base state formulas:
  bg_state%MUB(ims:ime,jms:jme) = WRFB_P0*exp(-WRFB_T0/WRFB_A + &
       sqrt((WRFB_T0/WRFB_A)**2-two*grav*bg_state%HGT(ims:ime,jms:jme)/(WRFB_A*Rd))) - bg_state%ptop
  ! Compute perturbation MU as the residual:
  bg_state%MU(ims:ime,jms:jme) = bg_state%PSFC(ims:ime,jms:jme) - bg_state%ptop - bg_state%MUB(ims:ime,jms:jme)
  if (idebug .ge. 3 .and.  its .le. 30 .and. ite .ge. 30 .and. jts .le. 24 .and. jte .ge. 24) then
     print *,'ptop, MUB[30,24], MU[30,24] ', bg_state%ptop, bg_state%MUB(30,24), bg_state%MU(30,24)
  end if
  ! Note: with the above assignments for c3/c4, the following formulas are consistent with getprs.f90
  !       for both wrfarw and fv3:
  do k=kts,kte
!!$     bg_state%PB(ims:ime,jms:jme,k) = bg_state%MUB(ims:ime,jms:jme)*aeta1_ll(k) + bg_state%ptop
     bg_state%PB(ims:ime,jms:jme,k) = bg_state%ptop + bg_state%MUB(ims:ime,jms:jme)*bg_state%c3h(k) + bg_state%c4h(k)
!!$     bg_state%P(ims:ime,jms:jme,k) = bg_state%MU(ims:ime,jms:jme)*aeta1_ll(k)
     bg_state%P(ims:ime,jms:jme,k) = bg_state%ptop + bg_state%MU(ims:ime,jms:jme)*bg_state%c3h(k) + bg_state%c4h(k)
  end do
  bg_state%PB(ims:ime,jms:jme,kme) = bg_state%PB(ims:ime,jms:jme,kte) 
  bg_state%P(ims:ime,jms:jme,kme) = bg_state%P(ims:ime,jms:jme,kte) 

  if (idebug .ge. 3 .and.  its .le. 30 .and. ite .ge. 30 .and. jts .le. 24 .and. jte .ge. 24) then
     print *,'PB[30,24] ',bg_state%PB(30,24,:)
     print *,'P[30,24] ',bg_state%P(30,24,:)
  end if
  

!  ierror=ierror+1 ! 3: u,v

  call gsi_bundlegetpointer (gsi_metguess_bundle(1),'u' ,ges_u,   istatus)
  if (istatus .ne. 0) return
  bg_state%U(ims:ime,jms:jme,kts:kte) = ges_u(1:lat2,1:lon2,1:nsig)
  bg_state%U(ims:ime,jms:jme,kme) = bg_state%U(ims:ime,jms:jme,kte) 


  call gsi_bundlegetpointer (gsi_metguess_bundle(1),'v' ,ges_v,   istatus)
  if (istatus .ne. 0) return
  bg_state%V(ims:ime,jms:jme,kts:kte) = ges_v(1:lat2,1:lon2,1:nsig)
  bg_state%V(ims:ime,jms:jme,kme) = bg_state%V(ims:ime,jms:jme,kte) 

  if (idebug .ge. 3 .and.  its .le. 30 .and. ite .ge. 30 .and. jts .le. 24 .and. jte .ge. 24) then
     print *,'U [30,24] ',bg_state%U(30,24,:)
     print *,'V [30,24] ',bg_state%V(30,24,:)
  end if
  
! MOIST array:
  bg_state%MOIST(ims:ime,jms:jme,kms:kme,:) = zero
  ierror=ierror+1 ! 4: qv
  call gsi_bundlegetpointer (gsi_metguess_bundle(1),'q', ges_q,   istatus)
  if (istatus .ne. 0) return

! Convert specific humidity to mixing ratio
  bg_state%MOIST(ims:ime,jms:jme,kts:kte,p_qv) = ges_q(1:lat2,1:lon2,1:nsig)/(1-ges_q(1:lat2,1:lon2,1:nsig))

  if (idebug .ge. 3 .and.  its .le. 30 .and. ite .ge. 30 .and. jts .le. 24 .and. jte .ge. 24) then
     print *,'QV[30,24] ',bg_state%MOIST(30,24,:,p_qv)
  end if
  
  ! Perturbation potential temperature

  call gsi_bundlegetpointer (gsi_metguess_bundle(1),'tv', ges_tv,   istatus)
  if (istatus .ne. 0) return

  bg_state%T(ims:ime,jms:jme,kts:kte) = ges_tv(1:lat2,1:lon2,1:nsig)
  if (idebug .ge. 3 .and.  its .le. 30 .and. ite .ge. 30 .and. jts .le. 24 .and. jte .ge. 24) then
     print *,'tv: T [30,24] ',bg_state%T(30,24,kts:kte)
  end if
  ! convert to sensible temperature:
  bg_state%T(ims:ime,jms:jme,kts:kte) = bg_state%T(ims:ime,jms:jme,kts:kte)*&
       (1+bg_state%MOIST(ims:ime,jms:jme,kts:kte,p_qv)) / &
       (1+bg_state%MOIST(ims:ime,jms:jme,kts:kte,p_qv)*Rv/Rd)
  if (idebug .ge. 3 .and.  its .le. 30 .and. ite .ge. 30 .and. jts .le. 24 .and. jte .ge. 24) then
     print *,'tsens: T [30,24] ',bg_state%T(30,24,kts:kte)
  end if
  bg_state%T(ims:ime,jms:jme,kme) = bg_state%T(ims:ime,jms:jme,kte)

! w: all zeroes
  bg_state%W(ims:ime,jms:jme,kms:kme) = zero
  
! Convert sensible temperature to potential temperature
!  based on WRFDA: grid%xb%t(i,j,k) / ((grid%xb%p(i,j,k)/base_pres)**kappa) - t0

  bg_state%T(ims:ime,jms:jme,kts:kte)=bg_state%T(ims:ime,jms:jme,kts:kte) / &
       (((bg_state%PB(ims:ime,jms:jme,kts:kte)+bg_state%P(ims:ime,jms:jme,kts:kte))/base_pres)**kappa)-t0

  if (idebug .ge. 3 .and.  its .le. 30 .and. ite .ge. 30 .and. jts .le. 24 .and. jte .ge. 24) then
     print *,'pert theta: T [30,24] ',bg_state%T(30,24,:)
  end if

! Compute PHB from formulas:
  allocate(phm(ims:ime,jms:jme),pfu(ims:ime,jms:jme),pfd(ims:ime,jms:jme))
  bg_state%PHB(ims:ime,jms:jme,1) = bg_state%HGT(ims:ime,jms:jme) * grav
  do k=kts+1,kte+1
!!$     bg_state%PHB(ims:ime,jms:jme,k) = bg_state%PHB(ims:ime,jms:jme,k-1) + &
!!$          (eta1_ll(k-1)-eta1_ll(k))*bg_state%MUB(ims:ime,jms:jme) * &
     pfu(:,:) = bg_state%PTOP+bg_state%MUB(:,:)*bg_state%C3F(k)  +bg_state%C4F(k)
     pfd(:,:) = bg_state%PTOP+bg_state%MUB(:,:)*bg_state%C3F(k-1)+bg_state%C4F(k-1)
     phm(:,:) = bg_state%PTOP+bg_state%MUB(:,:)*bg_state%C3H(k-1)+bg_state%C4H(k-1)
     bg_state%PHB(ims:ime,jms:jme,k) = bg_state%PHB(ims:ime,jms:jme,k-1) + &
          phm(:,:)*log(pfd(:,:)/pfu(:,:)) * &
          (Rd * ( &
           (WRFB_T0+WRFB_A*log(bg_state%PB(ims:ime,jms:jme,k)/WRFB_P0)) * &
           (WRFB_P0/bg_state%PB(ims:ime,jms:jme,k))**kappa)/WRFB_P0) * &
          (bg_state%PB(ims:ime,jms:jme,k)/WRFB_P0)**(-cv/cp)
  end do
  deallocate(phm,pfu,pfd)

! geopotential: use subroutine from displace_wrf module to compute total PH:
  call calc_ph_hyd_wrf(bg_state%MOIST(:,:,:,p_qv), bg_state%P, bg_state%PB, &
       bg_state%T, bg_state%HGT, bg_state%MU, bg_state%MUB, &
       bg_state%C3H, bg_state%C4H, bg_state%C3F, bg_state%C4F, bg_state%PTOP, bg_state%PH)
  bg_state%PH(ims:ime,jms:jme,kms:kme) = bg_state%PH(ims:ime,jms:jme,kms:kme) - &
       bg_state%PHB(ims:ime,jms:jme,kms:kme)
  if (idebug .ge. 3) then
     if (its .le. 30 .and. ite .ge. 30 .and. jts .le. 24 .and. jte .ge. 24) then
        print *,'PHB[30,24] ',bg_state%PHB(30,24,:)
        print *,'PH[30,24] ',bg_state%PH(30,24,:)
     endif
     print *,'debugging ges_to_fca'
     write(*,'(a,1x,3e15.6)') 'w/o halo min/max/meansq psfc:',minval(bg_state%PSFC(its:ite,jts:jte)),maxval(bg_state%PSFC(its:ite,jts:jte)),&
          sum(bg_state%PSFC(its:ite,jts:jte)**2)/((ite-its+1)*(jte-jts+1))
     write(*,'(a,1x,3e15.6)') 'w/  halo min/max/meansq psfc:',minval(bg_state%PSFC),maxval(bg_state%PSFC),&
          sum(bg_state%PSFC**2)/((ime-ims+1)*(jme-jms+1))
     write(*,'(a,1x,3e15.6)') 'w/o halo min/max/meansq hgt:',minval(bg_state%HGT(its:ite,jts:jte)),maxval(bg_state%HGT(its:ite,jts:jte)),&
          sum(bg_state%HGT(its:ite,jts:jte)**2)/((ite-its+1)*(jte-jts+1))
     write(*,'(a,1x,3e15.6)') 'w/  halo min/max/meansq hgt:',minval(bg_state%HGT),maxval(bg_state%HGT),&
          sum(bg_state%HGT**2)
     write(*,'(a,1x,3e15.6)') 'w/o halo min/max/meansq mub:',minval(bg_state%MUB(its:ite,jts:jte)),maxval(bg_state%MUB(its:ite,jts:jte)),&
          sum(bg_state%MUB(its:ite,jts:jte)**2)/((ite-its+1)*(jte-jts+1))
     write(*,'(a,1x,3e15.6)') 'w/  halo min/max/meansq mub:',minval(bg_state%MUB),maxval(bg_state%MUB),&
          sum(bg_state%MUB**2)
     write(*,'(a,1x,3e15.6)') 'w/o halo min/max/meansq mu:',minval(bg_state%MU(its:ite,jts:jte)),maxval(bg_state%MU(its:ite,jts:jte)),&
          sum(bg_state%MU(its:ite,jts:jte)**2)/((ite-its+1)*(jte-jts+1))
     write(*,'(a,1x,3e15.6)') 'w/  halo min/max/meansq mu:',minval(bg_state%MU),maxval(bg_state%MU),&
          sum(bg_state%MU**2)
     write(*,'(a,1x,3e15.6)') 'w/o halo min/max/meansq mub+mu:',minval(bg_state%MUB(its:ite,jts:jte)+bg_state%MU(its:ite,jts:jte)),&
          maxval(bg_state%MUB(its:ite,jts:jte)+bg_state%MU(its:ite,jts:jte)),&
          sum((bg_state%MUB(its:ite,jts:jte)+bg_state%MU(its:ite,jts:jte))**2)/((ite-its+1)*(jte-jts+1))
     write(*,'(a,1x,3e15.6)') 'w/  halo min/max/meansq mub+mu:',minval(bg_state%MUB+bg_state%MU),maxval(bg_state%MUB+bg_state%MU),&
          sum((bg_state%MUB+bg_state%MU)**2)/((ime-ims+1)*(jme-jms+1))
     do k=kts,kte
      write(*,'(a,1x,i3,3e15.6)') 'w/o halo min/max/meansq pb:',k,minval(bg_state%PB(its:ite,jts:jte,k)),maxval(bg_state%PB(its:ite,jts:jte,k)),&
           sum(bg_state%PB(its:ite,jts:jte,k)**2)/((ite-its+1)*(jte-jts+1))
      write(*,'(a,1x,i3,3e15.6)') 'w/o halo min/max/meansq p:',k,minval(bg_state%P(its:ite,jts:jte,k)),maxval(bg_state%P(its:ite,jts:jte,k)),&
           sum(bg_state%P(its:ite,jts:jte,k)**2)/((ite-its+1)*(jte-jts+1))
      write(*,'(a,1x,i3,3e15.6)') 'w/o halo min/max/meansq pb+p:',k,minval(bg_state%PB(its:ite,jts:jte,k)+bg_state%P(its:ite,jts:jte,k)), &
          maxval(bg_state%PB(its:ite,jts:jte,k)+bg_state%P(its:ite,jts:jte,k)),&
          sum((bg_state%PB(its:ite,jts:jte,k)+bg_state%P(its:ite,jts:jte,k))**2)/((ite-its+1)*(jte-jts+1))
      write(*,'(a,1x,i3,3e15.6)') 'w/o halo min/max/meansq u:',k,minval(bg_state%U(its:ite,jts:jte,k)),maxval(bg_state%U(its:ite,jts:jte,k)),&
           sum(bg_state%U(its:ite,jts:jte,k)**2)/((ite-its+1)*(jte-jts+1))
      write(*,'(a,1x,i3,3e15.6)') 'w/o halo min/max/meansq v:',k,minval(bg_state%V(its:ite,jts:jte,k)),maxval(bg_state%V(its:ite,jts:jte,k)),&
           sum(bg_state%V(its:ite,jts:jte,k)**2)/((ite-its+1)*(jte-jts+1))
      write(*,'(a,1x,i3,3e15.6)') 'w/o halo min/max/meansq qv:',k,minval(bg_state%MOIST(its:ite,jts:jte,k,p_qv)),maxval(bg_state%MOIST(its:ite,jts:jte,k,p_qv)),&
           sum(bg_state%MOIST(its:ite,jts:jte,k,p_qv)**2)/((ite-its+1)*(jte-jts+1))
      write(*,'(a,1x,i3,3e15.6)') 'w/o halo min/max/meansq t:',k,minval(bg_state%T(its:ite,jts:jte,k)),maxval(bg_state%T(its:ite,jts:jte,k)),&
           sum(bg_state%T(its:ite,jts:jte,k)**2)/((ite-its+1)*(jte-jts+1))
      write(*,'(a,1x,i3,3e15.6)') 'w/o halo min/max/meansq phb:',k,minval(bg_state%PHB(its:ite,jts:jte,k)),maxval(bg_state%PHB(its:ite,jts:jte,k)),&
           sum(bg_state%PHB(its:ite,jts:jte,k)**2)/((ite-its+1)*(jte-jts+1))
      write(*,'(a,1x,i3,3e15.6)') 'w/o halo min/max/meansq ph:',k,minval(bg_state%PH(its:ite,jts:jte,k)),maxval(bg_state%PH(its:ite,jts:jte,k)),&
           sum(bg_state%PH(its:ite,jts:jte,k)**2)/((ite-its+1)*(jte-jts+1))
      write(*,'(a,1x,i3,3e15.6)') 'w/o halo min/max/meansq phb+ph:',k,minval(bg_state%PHB(its:ite,jts:jte,k)+bg_state%PH(its:ite,jts:jte,k)), &
          maxval(bg_state%PHB(its:ite,jts:jte,k)+bg_state%PH(its:ite,jts:jte,k)),&
          sum((bg_state%PHB(its:ite,jts:jte,k)+bg_state%PH(its:ite,jts:jte,k))**2)/((ite-its+1)*(jte-jts+1))
   end do
  end if

  if (nmoist .gt. 1) then
     do imoist=2,nmoist
        ierror=ierror+1 ! 5-4+nmoist-1: elements of moist array
        if (moistguess(imoist) .eq. 1) then
           call gsi_bundlegetpointer (gsi_metguess_bundle(1),trim(moistnames(imoist)), ges_rank3, istatus)
        elseif (moistguess(imoist) .eq. 2) then
           call gsi_bundlegetpointer (gsi_chemguess_bundle(1),trim(moistnames(imoist)), ges_rank3, istatus)
        else
           write (*,*) 'ges_to_fca_wrf: invalid value of moistguess=',moistguess(imoist),' for imoist=',imoist
           istatus=1
        end if
        if (istatus .ne. 0) return
        bg_state%MOIST(ims:ime,jms:jme,kts:kte,imoist) = ges_rank3(1:lat2,1:lon2,1:nsig)
     end do
  end if

  ierror = 0 !Finished with all metguess/chemguess_bundle calls, success

  !DM_PARALLEL debugging:

  if (idebug .ge. 2) then
     call print_halos('psfc',bg_state%psfc(ims,jms),1)
     call print_halos('p',bg_state%p(ims,jms,kms),kte)
     call print_halos('u',bg_state%u(ims,jms,kms),kte)
     call print_halos('v',bg_state%v(ims,jms,kms),kte)
     call print_halos('t',bg_state%t(ims,jms,kms),kte)
     do imoist=1,nmoist
        call print_halos(moistnames(imoist),bg_state%moist(ims,jms,kms,imoist),kte)
     end do
  end if
  
end subroutine ges_to_fca_wrf

subroutine sval_to_disp_grid(sval,disp,uv_zlevel,ierror)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram: sval_to_disp_grid
!   prgmmr: Nehrkorn
!
! abstract: extract displacement vectors from GSI increments
!
! program history log:
!
!   input argument list: 
!    sval,disp,uv_zlevel
!   output argument list:
!    disp,ierror
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block
  implicit none
  type(gsi_bundle) ,intent(in) :: sval(:) ! nsubwin (=1)
  type(fca_gridded_disp), intent(inout):: disp
  integer, intent(in) :: uv_zlevel
  integer, intent(out) :: ierror
  integer(i_kind) :: i,j,k, istatus
  real(r_kind),pointer,dimension(:,:,:) :: sv_u
  real(r_kind),pointer,dimension(:,:,:) :: sv_v
  
  ierror=3 ! 3: u,v
  call gsi_bundlegetpointer (sval(1),'u'   ,sv_u,   istatus)
  if (istatus .ne. 0) return
  call gsi_bundlegetpointer (sval(1),'v'   ,sv_v,   istatus)
  if (istatus .ne. 0) return
  ierror = 0

  ! NOTE: since the i,j indices are switched compared to their WRF usage,
  !       we associate the x_disp (in the i-direction with the meridional (v) velocity component
  !       (and y_disp, j, with u)
  disp%x_disp(1:lat1,1:lon1) = sv_v(haloi+1:haloi+lat1,haloj+1:haloj+lon1,uv_zlevel)
  disp%y_disp(1:lat1,1:lon1) = sv_u(haloi+1:haloi+lat1,haloj+1:haloj+lon1,uv_zlevel)
  if (idebug .ge. 3) then
    print *,'debugging sval_to_disp_grid: from sv_v/sv_u'
    write(*,'(a,1x,3e15.6)') 'w/o halo min/max/meansq x_disp:',minval(disp%x_disp(1:lat1,1:lon1)),maxval(disp%x_disp(1:lat1,1:lon1)),&
         sum(disp%x_disp(1:lat1,1:lon1)**2)/(lat1*lon1)
    write(*,'(a,1x,3e15.6)') 'w/o halo min/max/meansq y_disp:',minval(disp%y_disp(1:lat1,1:lon1)),maxval(disp%y_disp(1:lat1,1:lon1)),&
         sum(disp%y_disp(1:lat1,1:lon1)**2)/(lat1*lon1)
  end if

  return
end subroutine sval_to_disp_grid

subroutine sval_to_disp_grid_adj(sval,disp,uv_zlevel,ierror)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram: sval_to_disp_grid_adj
!   prgmmr: Nehrkorn
!
! abstract: extract displacement vectors from GSI increments (adjoint version)
!
! program history log:
!
!   input argument list: 
!    sval,disp,uv_zlevel
!   output argument list:
!    sval,disp,ierror
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block
  implicit none
  type(gsi_bundle) ,intent(inout) :: sval(:) ! nsubwin (=1)
  type(fca_gridded_disp), intent(inout):: disp
  integer, intent(in) :: uv_zlevel
  integer, intent(out) :: ierror
  integer(i_kind) :: i,j,k, imoist, istatus
  character (len=80) :: fname_fcauv ! for debugging output of disp_ad

  real(r_kind),pointer,dimension(:,:)   :: sv_ps
  real(r_kind),pointer,dimension(:,:,:) :: sv_u
  real(r_kind),pointer,dimension(:,:,:) :: sv_v
  real(r_kind),pointer,dimension(:,:,:) :: sv_prse
  real(r_kind),pointer,dimension(:,:,:) :: sv_q
  real(r_kind),pointer,dimension(:,:,:) :: sv_tsen
  real(r_kind),pointer,dimension(:,:,:) :: sv_tv
  real(r_kind),pointer,dimension(:,:,:) :: sv_rank3
  
  real(r_kind),dimension(nlat*nlon*s2g_raf%nlevs_alloc)      :: hwork

! zero sval (in WRFDA: da_zero_x(grid%xa)), done inside sval_to_disp_grid_adj
  ! 2d fields:
  ierror = 1 ! 1: sfc pressure (convert from Pa to kPa)
  call gsi_bundlegetpointer (sval(1),'ps'  ,sv_ps,  istatus)
  if (istatus .ne. 0) return
  sv_ps(1:lat2,1:lon2) = zero

  ierror=3 ! 3: u,v
  call gsi_bundlegetpointer (sval(1),'u'   ,sv_u,   istatus)
  if (istatus .ne. 0) return
  sv_u(1:lat2,1:lon2,1:nsig) = zero
  call gsi_bundlegetpointer (sval(1),'v'   ,sv_v,   istatus)
  if (istatus .ne. 0) return
  sv_v(1:lat2,1:lon2,1:nsig) = zero
  call gsi_bundlegetpointer (sval(1),'prse'   ,sv_prse,   istatus)
  if (istatus .ne. 0) return
  sv_prse(1:lat2,1:lon2,1:nsig) = zero
  
  ! NOTE: since the i,j indices are switched compared to their WRF usage,
  !       we associate the x_disp (in the i-direction with the meridional (v) velocity component
  !       (and y_disp, j, with u)
  sv_v(haloi+1:haloi+lat1,haloj+1:haloj+lon1,uv_zlevel) = disp%x_disp(1:lat1,1:lon1)
  sv_u(haloi+1:haloi+lat1,haloj+1:haloj+lon1,uv_zlevel) = disp%y_disp(1:lat1,1:lon1)
  if (idebug .ge. 3) then
    print *,'debugging sval_to_disp_grid_adj: grad wrt x/y_disp'
    write(*,'(a,1x,3e15.6)') 'w/o halo min/max/meansq x_disp:',minval(disp%x_disp(1:lat1,1:lon1)),maxval(disp%x_disp(1:lat1,1:lon1)),&
         sum(disp%x_disp(1:lat1,1:lon1)**2)/(lat1*lon1)
    write(*,'(a,1x,3e15.6)') 'w/o halo min/max/meansq y_disp:',minval(disp%y_disp(1:lat1,1:lon1)),maxval(disp%y_disp(1:lat1,1:lon1)),&
         sum(disp%y_disp(1:lat1,1:lon1)**2)/(lat1*lon1)
    if (idebug .ge. 3 .and. mype .eq. 0) then
       k = get_lun()
       i = 0
       j = 1
       do while (j .ne. 0)
          i=i+1
          write (fname_fcauv,'(a,i3.3,a)') 'debug_',i,'_fca_adj'
          open(unit=k,file=trim(fname_fcauv),form='formatted',status='new',iostat=j)
       end do
       print *,'debugging sval_to_disp_grid_adj: outputting grad wrt x/y_disp to ',trim(fname_fcauv)
       write (k,'(a,i4,a)') 'Displacement vectors gradients for subdomain ',mype,' with subdomain i,j (in original WRF coordinates) grid indices:'
       write (k,'(4i10)') jts, jte, its, ite
       write (k,'(6e20.10)') ((disp%y_disp(i,j),j=1,lon1),i=1,lat1)
       write (k,'(6e20.10)') ((disp%x_disp(i,j),j=1,lon1),i=1,lat1)
       close(k)
    end if
  end if
 
  ! 3D fields that require non-linear conversions
  ierror=ierror+1 ! 4: qv
  call gsi_bundlegetpointer (sval(1),'q'   ,sv_q ,  istatus)
  if (istatus .ne. 0) return
  sv_q(1:lat2,1:lon2,1:nsig) = zero
  
  ierror=ierror+1 ! 5: tsen, tv
  call gsi_bundlegetpointer (sval(1),'tsen'   ,sv_tsen ,  istatus)
  if (istatus .ne. 0) return
  sv_tsen(1:lat2,1:lon2,1:nsig) = zero
  call gsi_bundlegetpointer (sval(1),'tv'   ,sv_tv ,  istatus)
  if (istatus .ne. 0) return
  sv_tv(1:lat2,1:lon2,1:nsig) = zero

  ! MOIST array
  if (nmoist .gt. 1) then
     do imoist=2,nmoist
        call gsi_bundlegetpointer (sval(1),trim(moistnames(imoist)),sv_rank3 ,  istatus)
        if (istatus .ne. 0) then
           if (mype .eq. 0) write (*,*) 'sval_to_disp_adj: ',trim(moistnames(imoist)),' missing from sval, skipped'
        else
           sv_rank3(1:lat2,1:lon2,1:nsig) = zero
        end if
     end do
  end if

! Halo exchange of GSI gradients after transfer from FCA
! Convert from subdomain to full horizontal field distributed among processors
  call general_sub2grid(s2g_raf,sval(1)%values,hwork)
! Put back onto subdomains
  call general_grid2sub(s2g_raf,hwork,sval(1)%values)
  ierror = 0

  return
end subroutine sval_to_disp_grid_adj

subroutine fca_state_to_sval(full_state,inc_state,flag_linear,sval,ierror)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram: fca_state_to_sval
!   prgmmr: Nehrkorn
!
! abstract: Initialize FCA variables from GSI
!
! program history log:
!
!   input argument list: 
!    full_state,inc_state,flag_linear,sval
!   output argument list:
!    inc_state,sval,ierror
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block
  implicit none

  type(gsi_bundle) ,intent(inout) :: sval(:)
! For idebug 0:
!  type(fca_wrf_grid), intent(in):: full_state, inc_state
! For idebug 2 halo prints:
  type(fca_wrf_grid), intent(in):: full_state
  type(fca_wrf_grid), intent(inout):: inc_state
  integer, intent(out) :: ierror
  logical, intent(in) :: flag_linear

  integer :: i,j,k,imoist,istatus
  real(r_kind),pointer,dimension(:,:)   :: sv_ps
  real(r_kind),pointer,dimension(:,:,:) :: sv_u
  real(r_kind),pointer,dimension(:,:,:) :: sv_v
  real(r_kind),pointer,dimension(:,:,:) :: sv_prse
  real(r_kind),pointer,dimension(:,:,:) :: sv_q
  real(r_kind),pointer,dimension(:,:,:) :: sv_tsen
  real(r_kind),pointer,dimension(:,:,:) :: sv_tv
  real(r_kind),pointer,dimension(:,:,:) :: sv_rank3

  real(fp) :: work3d_1(ims:ime,jms:jme,kms:kme),work3d_2(ims:ime,jms:jme,kms:kme), &
       work3d_3(ims:ime,jms:jme,kms:kme)

  character (len=max_varname_length) :: haloname
  real(r_kind) :: hwork(nlat*nlon*s2g_raf%nlevs_alloc)
  real(r_kind),allocatable :: hwork_press(:,:,:,:),work_field(:,:,:,:)

  if (idebug .ge. 3) then
    print *,'debugging fca_state_to_sval: inc_state% variables'
    write(*,'(a,1x,3e15.6)') 'w/o halo min/max/meansq psfc:',minval(inc_state%PSFC(its:ite,jts:jte)),maxval(inc_state%PSFC(its:ite,jts:jte)),&
         sum(inc_state%PSFC(its:ite,jts:jte)**2)/((ite-its+1)*(jte-jts-1))
    write(*,'(a,1x,3e15.6)') 'w/ halo min/max/meansq psfc:',minval(inc_state%PSFC(ims:ime,jms:jme)),maxval(inc_state%PSFC(ims:ime,jms:jme)),&
         sum(inc_state%PSFC(ims:ime,jms:jme)**2)/((ime-ims+1)*(jme-jms-1))
    do k=kts,kte
       write(*,'(a,1x,i3,3e15.6)') 'w/o halo min/max/meansq p:',k,minval(inc_state%p(its:ite,jts:jte,k)),maxval(inc_state%p(its:ite,jts:jte,k)),&
            sum(inc_state%p(its:ite,jts:jte,k)**2)/((ite-its+1)*(jte-jts-1))
       write(*,'(a,1x,i3,3e15.6)') 'w/ halo min/max/meansq p:',k,minval(inc_state%p(ims:ime,jms:jme,k)),maxval(inc_state%p(ims:ime,jms:jme,k)),&
            sum(inc_state%p(ims:ime,jms:jme,k)**2)/((ime-ims+1)*(jme-jms-1))
       write(*,'(a,1x,i3,3e15.6)') 'w/o halo min/max/meansq u:',k,minval(inc_state%u(its:ite,jts:jte,k)),maxval(inc_state%u(its:ite,jts:jte,k)),&
            sum(inc_state%u(its:ite,jts:jte,k)**2)/((ite-its+1)*(jte-jts-1))
       write(*,'(a,1x,i3,3e15.6)') 'w/ halo min/max/meansq u:',k,minval(inc_state%u(ims:ime,jms:jme,k)),maxval(inc_state%u(ims:ime,jms:jme,k)),&
            sum(inc_state%u(ims:ime,jms:jme,k)**2)/((ime-ims+1)*(jme-jms-1))
       write(*,'(a,1x,i3,3e15.6)') 'w/o halo min/max/meansq v:',k,minval(inc_state%v(its:ite,jts:jte,k)),maxval(inc_state%v(its:ite,jts:jte,k)),&
            sum(inc_state%v(its:ite,jts:jte,k)**2)/((ite-its+1)*(jte-jts-1))
       write(*,'(a,1x,i3,3e15.6)') 'w/ halo min/max/meansq v:',k,minval(inc_state%v(ims:ime,jms:jme,k)),maxval(inc_state%v(ims:ime,jms:jme,k)),&
            sum(inc_state%v(ims:ime,jms:jme,k)**2)/((ime-ims+1)*(jme-jms-1))
       write(*,'(a,1x,i3,3e15.6)') 'w/o halo min/max/meansq w:',k,minval(inc_state%w(its:ite,jts:jte,k)),maxval(inc_state%w(its:ite,jts:jte,k)),&
            sum(inc_state%w(its:ite,jts:jte,k)**2)/((ite-its+1)*(jte-jts-1))
       write(*,'(a,1x,i3,3e15.6)') 'w/ halo min/max/meansq w:',k,minval(inc_state%w(ims:ime,jms:jme,k)),maxval(inc_state%w(ims:ime,jms:jme,k)),&
            sum(inc_state%w(ims:ime,jms:jme,k)**2)/((ime-ims+1)*(jme-jms-1))
       write(*,'(a,1x,i3,3e15.6)') 'w/o halo min/max/meansq th:',k,minval(inc_state%t(its:ite,jts:jte,k)),maxval(inc_state%t(its:ite,jts:jte,k)),&
            sum(inc_state%t(its:ite,jts:jte,k)**2)/((ite-its+1)*(jte-jts-1))
       write(*,'(a,1x,i3,3e15.6)') 'w/ halo min/max/meansq th:',k,minval(inc_state%t(ims:ime,jms:jme,k)),maxval(inc_state%t(ims:ime,jms:jme,k)),&
            sum(inc_state%t(ims:ime,jms:jme,k)**2)/((ime-ims+1)*(jme-jms-1))
       write(*,'(a,1x,i3,3e15.6)') 'w/o halo min/max/meansq q:',k,minval(inc_state%moist(its:ite,jts:jte,k,p_qv)),maxval(inc_state%moist(its:ite,jts:jte,k,p_qv)),&
            sum(inc_state%moist(its:ite,jts:jte,k,p_qv)**2)/((ite-its+1)*(jte-jts-1))
       write(*,'(a,1x,i3,3e15.6)') 'w/ halo min/max/meansq q:',k,minval(inc_state%moist(ims:ime,jms:jme,k,p_qv)),maxval(inc_state%moist(ims:ime,jms:jme,k,p_qv)),&
            sum(inc_state%moist(ims:ime,jms:jme,k,p_qv)**2)/((ime-ims+1)*(jme-jms-1))
    end do
 end if
 

  !DM_PARALLEL debugging:
 if (idebug .ge. 2) then
    call print_halos('full_psfc',full_state%psfc(ims,jms),1)
    call print_halos('full_p',full_state%p(ims,jms,kms),kte)
    call print_halos('full_u',full_state%u(ims,jms,kms),kte)
    call print_halos('full_v',full_state%v(ims,jms,kms),kte)
    call print_halos('full_t',full_state%t(ims,jms,kms),kte)

    call print_halos('inc_psfc',inc_state%psfc(ims,jms),1)
    call print_halos('inc_p',inc_state%p(ims,jms,kms),kte)
    call print_halos('inc_u',inc_state%u(ims,jms,kms),kte)
    call print_halos('inc_v',inc_state%v(ims,jms,kms),kte)
    call print_halos('inc_t',inc_state%t(ims,jms,kms),kte)

    do imoist=1,nmoist
       haloname='full_' // trim(moistnames(imoist))
       call print_halos(haloname,full_state%moist(ims,jms,kms,imoist),kte)
       haloname='inc_' // trim(moistnames(imoist))
       call print_halos(haloname,inc_state%moist(ims,jms,kms,imoist),kte)
    end do
    
 end if

  ! 2d fields:
  ierror = 1 ! 1: sfc pressure (convert from Pa to kPa)
  call gsi_bundlegetpointer (sval(1),'ps'  ,sv_ps,  istatus)
  if (istatus .ne. 0) return
  sv_ps(1:lat2,1:lon2) = inc_state%PSFC(ims:ime,jms:jme)/r1000

  ! 3d fields that are straight copies
  ierror=3 ! 3: u,v,prse (convert from Pa to kPa)
  call gsi_bundlegetpointer (sval(1),'u'   ,sv_u,   istatus)
  if (istatus .ne. 0) return
  sv_u(1:lat2,1:lon2,1:nsig) = inc_state%U(ims:ime,jms:jme,kts:kte)
  call gsi_bundlegetpointer (sval(1),'v'   ,sv_v,   istatus)
  if (istatus .ne. 0) return
  sv_v(1:lat2,1:lon2,1:nsig) = inc_state%V(ims:ime,jms:jme,kts:kte)
  call gsi_bundlegetpointer (sval(1),'prse'   ,sv_prse,   istatus)
  if (istatus .ne. 0) return
  sv_prse(1:lat2,1:lon2,1:nsig) = inc_state%P(ims:ime,jms:jme,kts:kte)/r1000

  ! 3D fields that require non-linear conversions
  ierror=ierror+1 ! 4: qv
  call gsi_bundlegetpointer (sval(1),'q'   ,sv_q ,  istatus)
  if (istatus .ne. 0) return
  work3d_1(ims:ime,jms:jme,kts:kte)=full_state%MOIST(ims:ime,jms:jme,kts:kte,p_qv)
  ! restore original bg_state qv (mixing ratio, r) by subtracting increment from full_state:
  if (.not. flag_linear) work3d_1(ims:ime,jms:jme,kts:kte) = work3d_1(ims:ime,jms:jme,kts:kte) - &
       inc_state%MOIST(ims:ime,jms:jme,kts:kte,p_qv)
  ! convert qv increment to specific humidity increment:
  ! Use q=r/(1+r) ==>  Dq=(dq/dr)*Dr=Dr/(1+r^2):
  sv_q(1:lat2,1:lon2,1:nsig) = inc_state%MOIST(ims:ime,jms:jme,kts:kte,p_qv) / &
       ((one+work3d_1(ims:ime,jms:jme,kts:kte))**2)
  
  ierror=ierror+1 ! 5: tsen
  call gsi_bundlegetpointer (sval(1),'tsen'   ,sv_tsen ,  istatus)
  if (istatus .ne. 0) return
  ! convert increment of potential temperature perturbation
  ! back to increment of tsen (ignore effect of pressure perturbation)
  work3d_2(ims:ime,jms:jme,kts:kte)=full_state%PB(ims:ime,jms:jme,kts:kte)+full_state%P(ims:ime,jms:jme,kts:kte)
  ! restore original pressure by subtracting perturbation pressure increment (PB is unchanged):
  if (.not. flag_linear) work3d_2(ims:ime,jms:jme,kts:kte) = work3d_2(ims:ime,jms:jme,kts:kte) - &
       inc_state%P(ims:ime,jms:jme,kts:kte)
  sv_tsen(1:lat2,1:lon2,1:nsig)=((work3d_2(ims:ime,jms:jme,kts:kte)/base_pres)**kappa) * &
       inc_state%T(ims:ime,jms:jme,kts:kte)
  ! Also need to compute and store sv_tv:
  call gsi_bundlegetpointer (sval(1),'tv'   ,sv_tv ,  istatus)
  if (istatus .ne. 0) return
  work3d_3(ims:ime,jms:jme,kts:kte)=t300+full_state%T(ims:ime,jms:jme,kts:kte)
  ! restore original pot temperature by subtracting pot temp increment:
  if (.not. flag_linear) work3d_3(ims:ime,jms:jme,kts:kte) = work3d_3(ims:ime,jms:jme,kts:kte) - &
       inc_state%T(ims:ime,jms:jme,kts:kte)
  ! Apply formula from WRFv402, physics/module_diag_functions.F: 
  !  Tv = tK [Temp,K]* A, A=( 1.0 + (w[Mixing Ratio,kg/kg]/r622) ) / ( 1.0 + w )
  !  ==> dTv = dtK*A+tk*dw*dA/dw, dA/dw=((1/r622)-1)/((1+w)**2)
  sv_tv(1:lat2,1:lon2,1:nsig)=sv_tsen(1:lat2,1:lon2,1:nsig) * ( one + (work3d_1(ims:ime,jms:jme,kts:kte)/r622) ) &
       / ( one + work3d_1(ims:ime,jms:jme,kts:kte) ) + &
       ((work3d_2(ims:ime,jms:jme,kts:kte)/base_pres)**kappa) * work3d_3(ims:ime,jms:jme,kts:kte) * &
       inc_state%MOIST(ims:ime,jms:jme,kts:kte,p_qv)*((one/r622)-one)/((1+work3d_1(ims:ime,jms:jme,kts:kte))**2)

  if (idebug .ge. 3) then
     write (*,'(a,i4,6(i4,i4,e15.6))') 'before sv_u(k=15) mype=',mype,1,1,sv_u(1,1,15),2,2,sv_u(2,2,15),haloi+lat1,haloj+lon1,sv_u(haloi+lat1,haloj+lon1,15),lat2,lon2,sv_u(lat2,lon2,15)
     write (*,'(a,i4,6(i4,i4,e15.6))') 'before sv_v(k=15) mype=',mype,1,1,sv_v(1,1,15),2,2,sv_v(2,2,15),haloi+lat1,haloj+lon1,sv_v(haloi+lat1,haloj+lon1,15),lat2,lon2,sv_v(lat2,lon2,15)
     write (*,'(a,i4,6(i4,i4,e15.6))') 'before qv(k=15) mype=',mype,1,1,sv_q(1,1,15),2,2,sv_q(2,2,15),haloi+lat1,haloj+lon1,sv_q(haloi+lat1,haloj+lon1,15),lat2,lon2,sv_q(lat2,lon2,15)
     write (*,'(a,i4,6(i4,i4,e15.6))') 'before sv_tsen(k=15) mype=',mype,1,1,sv_tsen(1,1,15),2,2,sv_tsen(2,2,15),haloi+lat1,haloj+lon1,sv_tsen(haloi+lat1,haloj+lon1,15),lat2,lon2,sv_tsen(lat2,lon2,15)
  end if
  
  ! MOIST array
  if (nmoist .gt. 1) then
     do imoist=2,nmoist
        call gsi_bundlegetpointer (sval(1),trim(moistnames(imoist)),sv_rank3 ,  istatus)
        if (istatus .ne. 0) then
           if (mype .eq. 0) write (*,*) 'fca_state_to_sval: ',trim(moistnames(imoist)),' missing from sval, skipped'
        else
           sv_rank3(1:lat2,1:lon2,1:nsig) = inc_state%MOIST(ims:ime,jms:jme,kts:kte,imoist)
        end if
     end do
  end if
  
  ierror = 0

  ! Halo exchange of GSI increments after transfer from FCA:
  ! Convert from subdomain to full horizontal field distributed among processors
  call general_sub2grid(s2g_raf,sval(1)%values,hwork)
  ! Put back onto subdomains
  call general_grid2sub(s2g_raf,hwork,sval(1)%values)
  if (.not. flag_linear) then
     ! The above does not take care of sv_ps and sv_prse, do this separately:
     ! (see initi_fca_from_gsi for creation of s2g_press)
     allocate(work_field(s2g_press%inner_vars,lat2,lon2,s2g_press%num_fields), &
          hwork_press(s2g_press%inner_vars,s2g_press%nlat,s2g_press%nlon,s2g_press%kbegin_loc:s2g_press%kend_alloc))
     work_field(1,:,:,1:nsig) = sv_prse(:,:,:)
     work_field(1,:,:,nsig+1) = sv_ps(:,:)
     call general_sub2grid(s2g_press,work_field,hwork_press)
     call general_grid2sub(s2g_press,hwork_press,work_field)
     sv_prse(:,:,:) = work_field(1,:,:,1:nsig)
     sv_ps(:,:) = work_field(1,:,:,nsig+1)
     deallocate(work_field,hwork_press)
  end if
  if (idebug .ge. 3) then
     write (*,'(a,i4,6(i4,i4,e15.6))') 'after sv_u(k=15) mype=',mype,1,1,sv_u(1,1,15),2,2,sv_u(2,2,15),haloi+lat1,haloj+lon1,sv_u(haloi+lat1,haloj+lon1,15),lat2,lon2,sv_u(lat2,lon2,15)
     write (*,'(a,i4,6(i4,i4,e15.6))') 'after sv_v(k=15) mype=',mype,1,1,sv_v(1,1,15),2,2,sv_v(2,2,15),haloi+lat1,haloj+lon1,sv_v(haloi+lat1,haloj+lon1,15),lat2,lon2,sv_v(lat2,lon2,15)
     write (*,'(a,i4,6(i4,i4,e15.6))') 'after qv(k=15) mype=',mype,1,1,sv_q(1,1,15),2,2,sv_q(2,2,15),haloi+lat1,haloj+lon1,sv_q(haloi+lat1,haloj+lon1,15),lat2,lon2,sv_q(lat2,lon2,15)
     write (*,'(a,i4,6(i4,i4,e15.6))') 'after sv_tsen(k=15) mype=',mype,1,1,sv_tsen(1,1,15),2,2,sv_tsen(2,2,15),haloi+lat1,haloj+lon1,sv_tsen(haloi+lat1,haloj+lon1,15),lat2,lon2,sv_tsen(lat2,lon2,15)
  end if
  
 if (idebug .ge. 2) then
! Copy sval back to increments and check halos (!omit nonlinear conversions):
    inc_state%PSFC(ims:ime,jms:jme) = sv_ps(1:lat2,1:lon2) * r1000
    call print_halos('inc2_psfc',inc_state%psfc(ims,jms),1)
    inc_state%P(ims:ime,jms:jme,kts:kte) = sv_prse(1:lat2,1:lon2,1:nsig) * r1000
    call print_halos('inc2_p',inc_state%p(ims,jms,kms),kte)
    inc_state%U(ims:ime,jms:jme,kts:kte) = sv_u(1:lat2,1:lon2,1:nsig)
    call print_halos('inc2_u',inc_state%u(ims,jms,kms),kte)
    inc_state%V(ims:ime,jms:jme,kts:kte) = sv_v(1:lat2,1:lon2,1:nsig)
    call print_halos('inc2_v',inc_state%v(ims,jms,kms),kte)
    inc_state%T(ims:ime,jms:jme,kts:kte) = sv_tsen(1:lat2,1:lon2,1:nsig)
    call print_halos('inc2_t',inc_state%t(ims,jms,kms),kte)
    inc_state%T(ims:ime,jms:jme,kts:kte) = sv_tv(1:lat2,1:lon2,1:nsig)
    call print_halos('inc2_tv',inc_state%t(ims,jms,kms),kte)
    do imoist=1,nmoist
       call gsi_bundlegetpointer (sval(1),trim(moistnames(imoist)),sv_rank3 ,  istatus)
       if (istatus .eq. 0) then
          inc_state%MOIST(ims:ime,jms:jme,kts:kte,imoist) = sv_rank3(1:lat2,1:lon2,1:nsig)
          haloname='inc2_' // trim(moistnames(imoist))
          call print_halos(haloname,inc_state%moist(ims,jms,kms,imoist),kte)
       end if
    end do
 end if

end subroutine fca_state_to_sval

subroutine fca_state_to_sval_adj(full_state,inc_state,flag_linear,sval,ierror)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram: fca_state_to_sval_adj
!   prgmmr: Nehrkorn
!
! abstract: Adjoint of fca_state_to_sval (only called with flag_linear==.TRUE.)
!
! program history log:
!
!   input argument list: 
!    full_state,inc_state,flag_linear,sval
!   output argument list:
!    inc_state,sval,ierror
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block
  implicit none

  type(gsi_bundle) ,intent(inout) :: sval(:)
  type(fca_wrf_grid), intent(in):: full_state
  type(fca_wrf_grid), intent(inout):: inc_state
  integer, intent(out) :: ierror
  logical, intent(in) :: flag_linear

  integer :: i,j,k, imoist, istatus
  real(r_kind),pointer,dimension(:,:)   :: sv_ps
  real(r_kind),pointer,dimension(:,:,:) :: sv_u
  real(r_kind),pointer,dimension(:,:,:) :: sv_v
  real(r_kind),pointer,dimension(:,:,:) :: sv_prse
  real(r_kind),pointer,dimension(:,:,:) :: sv_q
  real(r_kind),pointer,dimension(:,:,:) :: sv_tv
  real(r_kind),pointer,dimension(:,:,:) :: sv_tsen
  real(r_kind),pointer,dimension(:,:,:) :: sv_rank3

  character (len=max_varname_length) :: haloname
  real(fp) :: work3d_1(ims:ime,jms:jme,kms:kme),work3d_2(ims:ime,jms:jme,kms:kme),&
       work3d_3(ims:ime,jms:jme,kms:kme)

  ! MOIST array:
  if (nmoist .gt. 1) then
     do imoist=2,nmoist
        call gsi_bundlegetpointer (sval(1),trim(moistnames(imoist)),sv_rank3 ,  istatus)
        if (istatus .ne. 0) then
           if (mype .eq. 0) write (*,*) 'fca_state_to_sval_adj: ',trim(moistnames(imoist)),' missing from sval, skipped'
        else
           inc_state%MOIST(ims:ime,jms:jme,kts:kte,imoist) = sv_rank3(1:lat2,1:lon2,1:nsig)
        end if
     end do
  end if

  work3d_1(ims:ime,jms:jme,kts:kte)=full_state%MOIST(ims:ime,jms:jme,kts:kte,p_qv)
  work3d_2(ims:ime,jms:jme,kts:kte)=full_state%PB(ims:ime,jms:jme,kts:kte)+full_state%P(ims:ime,jms:jme,kts:kte)
  work3d_3(ims:ime,jms:jme,kts:kte)=t300+full_state%T(ims:ime,jms:jme,kts:kte)

  ierror=5 ! 5: tsen
  call gsi_bundlegetpointer (sval(1),'tsen'   ,sv_tsen ,  istatus)
  if (istatus .ne. 0) return
  call gsi_bundlegetpointer (sval(1),'tv'   ,sv_tv ,  istatus)
  if (istatus .ne. 0) return
  ! Apply formula from WRFv402, physics/module_diag_functions.F: 
  !  Tv = tK [Temp,K]* A, A=( 1.0 + (w[Mixing Ratio,kg/kg]/0.622) ) / ( 1.0 + w )
  !  ==> dTv = dtK*A+tk*dw*dA/dw, dA/dw=((1/0.622)-1)/((1+w)**2)
  ! Add to adjoint of sv_tsen, initialize adjoint of moist:
  sv_tsen(1:lat2,1:lon2,1:nsig)=sv_tsen(1:lat2,1:lon2,1:nsig) + &
       sv_tv(1:lat2,1:lon2,1:nsig)* ( one + (work3d_1(ims:ime,jms:jme,kts:kte)/r622) ) &
       / ( one + work3d_1(ims:ime,jms:jme,kts:kte) )
  inc_state%MOIST(ims:ime,jms:jme,kts:kte,p_qv)=  &
       ((work3d_2(ims:ime,jms:jme,kts:kte)/base_pres)**kappa) * work3d_3(ims:ime,jms:jme,kts:kte) * &
       sv_tv(1:lat2,1:lon2,1:nsig)*((one/r622)-one)/((1+work3d_1(ims:ime,jms:jme,kts:kte))**2)
  ! convert increment of potential temperature perturbation
  ! back to increment of tsen: initialize adjoint of T:
  inc_state%T(ims:ime,jms:jme,kts:kte) = ((work3d_2(ims:ime,jms:jme,kts:kte)/base_pres)**kappa) * &
       sv_tsen(1:lat2,1:lon2,1:nsig)

  ! 3D fields that require non-linear conversions
  ierror=4 ! 4: qv
  call gsi_bundlegetpointer (sval(1),'q'   ,sv_q ,  istatus)
  if (istatus .ne. 0) return
  work3d_1(ims:ime,jms:jme,kts:kte)=full_state%MOIST(ims:ime,jms:jme,kts:kte,p_qv)
  ! convert qv increment to specific humidity increment: Add to adjoint of moist
  ! Use q=r/(1+r) ==>  Dq=(dq/dr)*Dr=Dr/(1+r^2):
  inc_state%MOIST(ims:ime,jms:jme,kts:kte,p_qv) = inc_state%MOIST(ims:ime,jms:jme,kts:kte,p_qv) + &
       sv_q(1:lat2,1:lon2,1:nsig) / ((one+full_state%MOIST(ims:ime,jms:jme,kts:kte,p_qv))**2)

  ! 3d fields that are straight copies: initialize adjoints
  ierror=3 ! 3: u,v,prse (convert from Pa to kPa)
  call gsi_bundlegetpointer (sval(1),'u'   ,sv_u,   istatus)
  if (istatus .ne. 0) return
  inc_state%U(ims:ime,jms:jme,kts:kte) = sv_u(1:lat2,1:lon2,1:nsig)
  call gsi_bundlegetpointer (sval(1),'v'   ,sv_v,   istatus)
  if (istatus .ne. 0) return
  inc_state%V(ims:ime,jms:jme,kts:kte) = sv_v(1:lat2,1:lon2,1:nsig)
  call gsi_bundlegetpointer (sval(1),'prse'   ,sv_prse,   istatus)
  if (istatus .ne. 0) return
  inc_state%P(ims:ime,jms:jme,kts:kte) = sv_prse(1:lat2,1:lon2,1:nsig)/r1000

  ! 2d fields:
  ierror = 1 ! 1: sfc pressure (convert from Pa to kPa): initialize adjoint
  call gsi_bundlegetpointer (sval(1),'ps'  ,sv_ps,  istatus)
  if (istatus .ne. 0) return
  inc_state%PSFC(ims:ime,jms:jme) = sv_ps(1:lat2,1:lon2)/r1000

  if (idebug .ge. 3) then
    print *,'debugging fca_state_to_sval_adj: sval variables'
    write(*,'(a,1x,3e15.6)') 'w/o halo min/max/meansq psfc:',minval(sv_ps(haloi+1:haloi+lat1,haloj+1:haloj+lon1)),maxval(sv_ps(haloi+1:haloi+lat1,haloj+1:haloj+lon1)),&
         sum(sv_ps(haloi+1:haloi+lat1,haloj+1:haloj+lon1)**2)/(lat1*lon1)
    write(*,'(a,1x,3e15.6)') 'w/ halo min/max/meansq psfc:',minval(sv_ps(1:lat2,1:lon2)),maxval(sv_ps(1:lat2,1:lon2)),&
         sum(sv_ps(1:lat2,1:lon2)**2)/((lat2)*(lon2))
    do k=kts,kte
       write(*,'(a,1x,i3,3e15.6)') 'w/o halo min/max/meansq p:',k,minval(sv_prse(haloi+1:haloi+lat1,haloj+1:haloj+lon1,k)),maxval(sv_prse(haloi+1:haloi+lat1,haloj+1:haloj+lon1,k)),&
            sum(sv_prse(haloi+1:haloi+lat1,haloj+1:haloj+lon1,k)**2)/(lat1*lon1)
       write(*,'(a,1x,i3,3e15.6)') 'w/ halo min/max/meansq p:',k,minval(sv_prse(1:lat2,1:lon2,k)),maxval(sv_prse(1:lat2,1:lon2,k)),&
            sum(sv_prse(1:lat2,1:lon2,k)**2)/((lat2)*(lon2))
       write(*,'(a,1x,i3,3e15.6)') 'w/o halo min/max/meansq u:',k,minval(sv_u(haloi+1:haloi+lat1,haloj+1:haloj+lon1,k)),maxval(sv_u(haloi+1:haloi+lat1,haloj+1:haloj+lon1,k)),&
            sum(sv_u(haloi+1:haloi+lat1,haloj+1:haloj+lon1,k)**2)/(lat1*lon1)
       write(*,'(a,1x,i3,3e15.6)') 'w/ halo min/max/meansq u:',k,minval(sv_u(1:lat2,1:lon2,k)),maxval(sv_u(1:lat2,1:lon2,k)),&
            sum(sv_u(1:lat2,1:lon2,k)**2)/((lat2)*(lon2))
       write(*,'(a,1x,i3,3e15.6)') 'w/o halo min/max/meansq v:',k,minval(sv_v(haloi+1:haloi+lat1,haloj+1:haloj+lon1,k)),maxval(sv_v(haloi+1:haloi+lat1,haloj+1:haloj+lon1,k)),&
            sum(sv_v(haloi+1:haloi+lat1,haloj+1:haloj+lon1,k)**2)/(lat1*lon1)
       write(*,'(a,1x,i3,3e15.6)') 'w/ halo min/max/meansq v:',k,minval(sv_v(1:lat2,1:lon2,k)),maxval(sv_v(1:lat2,1:lon2,k)),&
            sum(sv_v(1:lat2,1:lon2,k)**2)/((lat2)*(lon2))
       write(*,'(a,1x,i3,3e15.6)') 'w/o halo min/max/meansq t:',k,minval(sv_tsen(haloi+1:haloi+lat1,haloj+1:haloj+lon1,k)),maxval(sv_tsen(haloi+1:haloi+lat1,haloj+1:haloj+lon1,k)),&
            sum(sv_tsen(haloi+1:haloi+lat1,haloj+1:haloj+lon1,k)**2)/(lat1*lon1)
       write(*,'(a,1x,i3,3e15.6)') 'w/ halo min/max/meansq t:',k,minval(sv_tsen(1:lat2,1:lon2,k)),maxval(sv_tsen(1:lat2,1:lon2,k)),&
            sum(sv_tsen(1:lat2,1:lon2,k)**2)/((lat2)*(lon2))
       write(*,'(a,1x,i3,3e15.6)') 'w/o halo min/max/meansq q:',k,minval(sv_q(haloi+1:haloi+lat1,haloj+1:haloj+lon1,k)),maxval(sv_q(haloi+1:haloi+lat1,haloj+1:haloj+lon1,k)),&
            sum(sv_q(haloi+1:haloi+lat1,haloj+1:haloj+lon1,k)**2)/(lat1*lon1)
       write(*,'(a,1x,i3,3e15.6)') 'w/ halo min/max/meansq q:',k,minval(sv_q(1:lat2,1:lon2,k)),maxval(sv_q(1:lat2,1:lon2,k)),&
            sum(sv_q(1:lat2,1:lon2,k)**2)/((lat2)*(lon2))
    end do
  end if
 
  if (idebug .ge. 3) then
    print *,'debugging fca_state_to_sval_adj: inc_state variables'
    write(*,'(a,1x,3e15.6)') 'w/o halo min/max/meansq psfc:',minval(inc_state%PSFC(its:ite,jts:jte)),maxval(inc_state%PSFC(its:ite,jts:jte)),&
         sum(inc_state%PSFC(its:ite,jts:jte)**2)/((ite-its+1)*(jte-jts+1))
    write(*,'(a,1x,3e15.6)') 'w/ halo min/max/meansq psfc:',minval(inc_state%PSFC(ims:ime,jms:jme)),maxval(inc_state%PSFC(ims:ime,jms:jme)),&
         sum(inc_state%PSFC(ims:ime,jms:jme)**2)/((ime-ims+1)*(jme-jms+1))
    do k=kts,kte
       write(*,'(a,1x,i3,3e15.6)') 'w/o halo min/max/meansq p:',k,minval(inc_state%p(its:ite,jts:jte,k)),maxval(inc_state%p(its:ite,jts:jte,k)),&
            sum(inc_state%p(its:ite,jts:jte,k)**2)/((ite-its+1)*(jte-jts+1))
       write(*,'(a,1x,i3,3e15.6)') 'w/ halo min/max/meansq p:',k,minval(inc_state%p(ims:ime,jms:jme,k)),maxval(inc_state%p(ims:ime,jms:jme,k)),&
            sum(inc_state%p(ims:ime,jms:jme,k)**2)/((ime-ims+1)*(jme-jms+1))
       write(*,'(a,1x,i3,3e15.6)') 'w/o halo min/max/meansq u:',k,minval(inc_state%u(its:ite,jts:jte,k)),maxval(inc_state%u(its:ite,jts:jte,k)),&
            sum(inc_state%u(its:ite,jts:jte,k)**2)/((ite-its+1)*(jte-jts+1))
       write(*,'(a,1x,i3,3e15.6)') 'w/ halo min/max/meansq u:',k,minval(inc_state%u(ims:ime,jms:jme,k)),maxval(inc_state%u(ims:ime,jms:jme,k)),&
            sum(inc_state%u(ims:ime,jms:jme,k)**2)/((ime-ims+1)*(jme-jms+1))
       write(*,'(a,1x,i3,3e15.6)') 'w/o halo min/max/meansq v:',k,minval(inc_state%v(its:ite,jts:jte,k)),maxval(inc_state%v(its:ite,jts:jte,k)),&
            sum(inc_state%v(its:ite,jts:jte,k)**2)/((ite-its+1)*(jte-jts+1))
       write(*,'(a,1x,i3,3e15.6)') 'w/ halo min/max/meansq v:',k,minval(inc_state%v(ims:ime,jms:jme,k)),maxval(inc_state%v(ims:ime,jms:jme,k)),&
            sum(inc_state%v(ims:ime,jms:jme,k)**2)/((ime-ims+1)*(jme-jms+1))
       write(*,'(a,1x,i3,3e15.6)') 'w/o halo min/max/meansq t:',k,minval(inc_state%t(its:ite,jts:jte,k)),maxval(inc_state%t(its:ite,jts:jte,k)),&
            sum(inc_state%t(its:ite,jts:jte,k)**2)/((ite-its+1)*(jte-jts+1))
       write(*,'(a,1x,i3,3e15.6)') 'w/ halo min/max/meansq t:',k,minval(inc_state%t(ims:ime,jms:jme,k)),maxval(inc_state%t(ims:ime,jms:jme,k)),&
            sum(inc_state%t(ims:ime,jms:jme,k)**2)/((ime-ims+1)*(jme-jms+1))
       write(*,'(a,1x,i3,3e15.6)') 'w/o halo min/max/meansq q:',k,minval(inc_state%MOIST(its:ite,jts:jte,k,p_qv)),maxval(inc_state%MOIST(its:ite,jts:jte,k,p_qv)),&
            sum(inc_state%MOIST(its:ite,jts:jte,k,p_qv)**2)/((ite-its+1)*(jte-jts+1))
       write(*,'(a,1x,i3,3e15.6)') 'w/ halo min/max/meansq q:',k,minval(inc_state%MOIST(ims:ime,jms:jme,k,p_qv)),maxval(inc_state%MOIST(ims:ime,jms:jme,k,p_qv)),&
            sum(inc_state%MOIST(ims:ime,jms:jme,k,p_qv)**2)/((ime-ims+1)*(jme-jms+1))
    end do
  end if

  if (idebug .ge. 2) then
     call print_halos('adj_psfc',inc_state%psfc(ims,jms),1)
     call print_halos('adj_p',inc_state%p(ims,jms,kms),kte)
     call print_halos('adj_u',inc_state%u(ims,jms,kms),kte)
     call print_halos('adj_v',inc_state%v(ims,jms,kms),kte)
     call print_halos('adj_t',inc_state%t(ims,jms,kms),kte)
     do imoist=1,nmoist
        haloname='adj_' // trim(moistnames(imoist))
        call print_halos(haloname,inc_state%moist(ims,jms,kms,imoist),kte)
     end do
  end if

  ierror=0

end subroutine fca_state_to_sval_adj
   
subroutine print_halos(varname,print_array,nlev)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram: print_halos
!   prgmmr: Nehrkorn
!
! abstract: Print variables in halos of DM subdomain
!
! program history log:
!
!   input argument list: 
!    varname,print_array,nlev
!   output argument list:
!    
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block
  use file_utility, only : get_lun
  
  implicit none
  character (len=*), intent(in) :: varname
  integer, intent(in) :: nlev
  real(fp), dimension(ims:ime,jms:jme,1:nlev), intent(in) :: print_array

  integer :: unit_print, seq_no, iostat, i, j, k
  character (len=256) :: fname_print

  unit_print = get_lun()
  seq_no=1
  iostat=1
  do while (iostat .ne. 0 .and. seq_no .lt. 999)
     write (fname_print,'(3a,i4.4,a,i3.3)') 'halo_',trim(varname),'_',mype,'_',seq_no
     open(unit=unit_print,file=trim(fname_print),status='new',iostat=iostat)
     seq_no=seq_no+1
  enddo
  if (iostat .ne. 0) then
     write (*,'(a,i4,3a,i4)') 'mype=',mype,' NO MORE HALO_PRINTS, ran out of seq_no after fname_print=',trim(fname_print)
  else
     write (*,'(a,i4,3a,i4)') 'mype=',mype,' fname_print=',trim(fname_print),' nlev=',nlev
     do k=1,nlev
        if (its .gt. ids) then
           do i=ims,its
              write (unit_print,'(a,i4,a,i4,2i4)') 'k=',k,' i=',i,jms,jme
              write (unit_print,'(6e20.10)') (print_array(i,j,k),j=jms,jme)
           end do
        end if
        ! Note: need to check here against ide-1/jde-1, since ide/jde are padded by one
        if (ite .lt. ide-1) then
           do i=ite,ime
              write (unit_print,'(a,i4,a,i4,2i4)') 'k=',k,' i=',i,jms,jme
              write (unit_print,'(6e20.10)') (print_array(i,j,k),j=jms,jme)
           end do
        end if
        if (jts .gt. jds) then
           do j=jms,jts
              write (unit_print,'(a,i4,a,i4,2i4)') 'k=',k,' j=',j,ims,ime
              write (unit_print,'(6e20.10)') (print_array(i,j,k),i=ims,ime)
           end do
        end if
        if (jte .lt. jde-1) then
           do j=jte,jme
              write (unit_print,'(a,i4,a,i4,2i4)') 'k=',k,' j=',j,ims,ime
              write (unit_print,'(6e20.10)') (print_array(i,j,k),i=ims,ime)
           end do
        end if
     end do
     close(unit_print)
  end if
end subroutine print_halos

end module fca_gsi_inter_m
