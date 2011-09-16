subroutine get_gefs_for_regional
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    get_gefs_for_regionl  read gefsozone for regional
!   prgmmr: parrish          org: np22                date: 2010-09-26
!
! abstract: read gefs and interpolate to regional ensemble grid.
!          (adaptation of get_gefs_ensperts_dualres)
!
!
! program history log:
!   2010-09-26  parrish, initial documentation
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

  use gridmod, only: idsl5,regional
                                  use gridmod, only: region_lat,region_lon  !  for debug only
  use hybrid_ensemble_isotropic, only: region_lat_ens,region_lon_ens
  use hybrid_ensemble_isotropic, only: en_perts,ps_bar,nelen
  use hybrid_ensemble_parameters, only: n_ens,grd_ens,grd_anl,grd_a1,grd_e1,p_e2a,uv_hyb_ens,dual_res
 !use hybrid_ensemble_parameters, only: add_bias_perturbation
  use control_vectors, only: cvars2d,cvars3d,nc2d,nc3d
  use gsi_bundlemod, only: gsi_bundlegetpointer
  use constants,only: zero,half,fv,rd_over_cp,one,h300
  use constants, only: rd,grav
  use mpimod, only: mpi_comm_world,ierror,mype,mpi_rtype,mpi_min,mpi_max
  use kinds, only: r_kind,i_kind,r_single
  use general_sub2grid_mod, only: sub2grid_info,general_sub2grid_create_info
  use general_sub2grid_mod, only: general_grid2sub,general_sub2grid
  use general_sub2grid_mod, only: general_suba2sube,general_sube2suba
  use general_specmod, only: spec_vars,general_init_spec_vars
  use egrid2agrid_mod, only: g_create_egrid2points_slow,egrid2agrid_parm,g_egrid2points_faster
  use sigio_module, only: sigio_intkind,sigio_head,sigio_srhead
  use guess_grids, only: ges_prsl,ges_ps,ntguessig,geop_hgti,ges_z
  use aniso_ens_util, only: intp_spl
  use obsmod, only: iadate
  implicit none

  type(sub2grid_info) grd_gfs,grd_mix
  type(spec_vars) sp_gfs
  real(r_kind),allocatable,dimension(:,:,:) :: pri,vor,div,u,v,tv,q,cwmr,oz,prsl
  real(r_kind),allocatable,dimension(:,:)   :: z,ps
  real(r_kind),allocatable,dimension(:) :: ak5,bk5,ck5,tref5
  real(r_kind),allocatable :: work_sub(:,:,:,:),work(:,:,:,:),work_reg(:,:,:,:)
  real(r_kind),allocatable,dimension(:,:,:)::stbar,vpbar,tbar,rhbar,ozbar,cwbar
  real(r_kind),allocatable,dimension(:,:)::  pbar,sstbar,zbar,pbar_nmmb
  real(r_kind),allocatable,dimension(:,:,:,:)::st_eg,vp_eg,t_eg,rh_eg,oz_eg,cw_eg
  real(r_kind),allocatable,dimension(:,:,:):: p_eg,z_eg,p_eg_nmmb
  real(r_kind),allocatable,dimension(:,:,:,:):: ges_prsl_e
  real(r_kind),allocatable,dimension(:,:,:)::tsen,qs
  real(r_kind),allocatable,dimension(:,:,:)::ut,vt,tt,rht,ozt,cwt
  real(r_single),pointer,dimension(:,:,:):: w3
  real(r_single),pointer,dimension(:,:):: w2

  real(r_kind) bar_norm,sig_norm,kapr,kap1,trk
  integer(i_kind) iret,i,ig,j,jg,k,k2,n,il,jl,mm1,iderivative
  integer(i_kind) ic2,ic3
  integer(i_kind) ku,kv,kt,kq,koz,kcw,kz,kps
  character(70) filename
  logical ice
  integer(sigio_intkind):: lunges = 11
  type(sigio_head):: sighead
  type(egrid2agrid_parm) :: p_g2r
  integer(i_kind) inner_vars,num_fields,nlat_gfs,nlon_gfs,nsig_gfs,jcap_gfs,jcap_gfs_test
  integer(i_kind) nord_g2r
  logical,allocatable :: vector(:)
  real(r_kind) ozmin,ozmax
  real(r_kind) ozmin0,ozmax0
  real(r_kind),parameter::  zero_001=0.001_r_kind
  real(r_kind),allocatable,dimension(:) :: xspli,yspli,xsplo,ysplo
  integer(i_kind) iyr,ihourg
  integer(i_kind),dimension(4):: idate4
  integer(i_kind),dimension(8) :: ida,jda 
  integer(i_kind),dimension(5) :: iadate_gfs
  real(r_kind) hourg
  real(r_kind),dimension(5):: fha
  real(r_kind),allocatable,dimension(:)::glb_umin,glb_umax,reg_umin,reg_umax
  real(r_kind),allocatable,dimension(:)::glb_vmin,glb_vmax,reg_vmin,reg_vmax
  real(r_kind),allocatable,dimension(:)::glb_tmin,glb_tmax,reg_tmin,reg_tmax
  real(r_kind),allocatable,dimension(:)::glb_rhmin,glb_rhmax,reg_rhmin,reg_rhmax
  real(r_kind),allocatable,dimension(:)::glb_ozmin,glb_ozmax,reg_ozmin,reg_ozmax
  real(r_kind),allocatable,dimension(:)::glb_cwmin,glb_cwmax,reg_cwmin,reg_cwmax
  real(r_kind),allocatable,dimension(:)::glb_umin0,glb_umax0,reg_umin0,reg_umax0
  real(r_kind),allocatable,dimension(:)::glb_vmin0,glb_vmax0,reg_vmin0,reg_vmax0
  real(r_kind),allocatable,dimension(:)::glb_tmin0,glb_tmax0,reg_tmin0,reg_tmax0
  real(r_kind),allocatable,dimension(:)::glb_rhmin0,glb_rhmax0,reg_rhmin0,reg_rhmax0
  real(r_kind),allocatable,dimension(:)::glb_ozmin0,glb_ozmax0,reg_ozmin0,reg_ozmax0
  real(r_kind),allocatable,dimension(:)::glb_cwmin0,glb_cwmax0,reg_cwmin0,reg_cwmax0
                                             character(len=50) :: fname
  integer(i_kind) k_t,kp,istatus
  real(r_kind) rdog,h,dz,this_tv
  real(r_kind),allocatable::height(:),zbarl(:,:,:)
  logical add_bias_perturbation
  integer(i_kind) n_ens_temp
logical point1,point2
integer(i_kind) kk,n_in
real(r_kind) pdiffmax,pmax,pdiffmax0,pmax0,pdiffmin,pdiffmin0
real(r_kind),allocatable::psfc_out(:,:)
integer(i_kind) ilook,jlook

  add_bias_perturbation=.false.  !  not fully activated yet--testing new adjustment of ps perturbions 1st

!     figure out what are acceptable dimensions for global grid, based on resolution of input spectral coefs
!   need to inquire from file what is spectral truncation, then setup general spectral structure variable

!  filename='sigf06_ens_mem001'
  open(10,file='filelist',form='formatted',err=30)
  rewind (10) 
  do n=1,200
  read(10,'(a)',err=20,end=40)filename 
  enddo
40  n_ens=n-1

!    set n_ens_temp depending on if we want to add bias perturbation to the ensemble

  if(add_bias_perturbation) then
     n_ens_temp=n_ens+1
  else
     n_ens_temp=n_ens
  end if

  rewind (10) 
  read(10,'(a)',err=20,end=20)filename 
  open(lunges,file=trim(filename),form='unformatted')
  call sigio_srhead(lunges,sighead,iret)
  close(lunges)
  if(mype == 0) then
     write(6,*) ' sighead%fhour,sighead%idate=',sighead%fhour,sighead%idate
     write(6,*) ' iadate(y,m,d,hr,min)=',iadate
     write(6,*) ' sighead%jcap,sighead%levs=',sighead%jcap,sighead%levs
     write(6,*) ' sighead%latf,sighead%lonf=',sighead%latf,sighead%lonf
     write(6,*) ' sighead%idvc,sighead%nvcoord=',sighead%idvc,sighead%nvcoord
     write(6,*) ' sighead%idsl=',sighead%idsl
     do k=1,sighead%levs+1
        write(6,*)' k,vcoord=',k,sighead%vcoord(k,:)
     end do
  end if

! Extract header information
  hourg    = sighead%fhour
  idate4(1)= sighead%idate(1)
  idate4(2)= sighead%idate(2)
  idate4(3)= sighead%idate(3)
  idate4(4)= sighead%idate(4)

! Compute valid time from ensemble date and forecast length and compare to iadate, the analysis time
  iyr=idate4(4)
  ihourg=hourg
  if(iyr>=0.and.iyr<=99) then
     if(iyr>51) then
        iyr=iyr+1900
     else
        iyr=iyr+2000
     end if
  end if
  fha=zero ; ida=0; jda=0
  fha(2)=ihourg    ! relative time interval in hours
  ida(1)=iyr       ! year
  ida(2)=idate4(2) ! month
  ida(3)=idate4(3) ! day
  ida(4)=0         ! time zone
  ida(5)=idate4(1) ! hour
  call w3movdat(fha,ida,jda)
  iadate_gfs(1)=jda(1) ! year
  iadate_gfs(2)=jda(2) ! mon
  iadate_gfs(3)=jda(3) ! day
  iadate_gfs(4)=jda(5) ! hour
  iadate_gfs(5)=0      ! minute
  if(mype == 0) then
     write(6,*)' in get_gefs_for_regional, iadate_gefs=',iadate_gfs
     write(6,*)' in get_gefs_for_regional, iadate    =',iadate
  end if
  if(iadate_gfs(1)/=iadate(1).or.iadate_gfs(2)/=iadate(2).or.iadate_gfs(3)/=iadate(3).or.&
                                 iadate_gfs(4)/=iadate(4).or.iadate_gfs(5)/=iadate(5) ) then
     if(mype == 0) write(6,*)' GEFS ENSEMBLE MEMBER DATE NOT EQUAL TO ANALYSIS DATE, PROGRAM STOPS'
     call stop2(85)
  end if
     

!         set up ak5,bk5,ck5 for use in computing 3d pressure field (needed for vertical interp to regional)
!                            following is code segment from gesinfo.F90
  allocate(ak5(sighead%levs+1))
  allocate(bk5(sighead%levs+1))
  allocate(ck5(sighead%levs+1))
  allocate(tref5(sighead%levs))
  do k=1,sighead%levs+1
     ak5(k)=zero
     bk5(k)=zero
     ck5(k)=zero
  end do
  if (sighead%nvcoord == 1) then
     do k=1,sighead%levs+1
        bk5(k) = sighead%vcoord(k,1)
     end do
  elseif (sighead%nvcoord == 2) then
     do k = 1,sighead%levs+1
        ak5(k) = sighead%vcoord(k,1)*zero_001
        bk5(k) = sighead%vcoord(k,2)
     end do
  elseif (sighead%nvcoord == 3) then
     do k = 1,sighead%levs+1
        ak5(k) = sighead%vcoord(k,1)*zero_001
        bk5(k) = sighead%vcoord(k,2)
        ck5(k) = sighead%vcoord(k,3)*zero_001
     end do
  else
     write(6,*)'READ_GFS_OZONE_FOR_REGIONAL:  ***ERROR*** INVALID value for nvcoord=',sighead%nvcoord
     call stop2(85)
  endif
! Load reference temperature array (used by general coordinate)
  do k=1,sighead%levs
     tref5(k)=h300
  end do


  inner_vars=1
  nlat_gfs=sighead%latf+2
  nlon_gfs=sighead%lonf
  nsig_gfs=sighead%levs
  num_fields=6*nsig_gfs+2      !  want to transfer u,v,t,q,oz,cw,ps,z from gfs subdomain to slab
                            !  later go through this code, adapting gsibundlemod, since currently 
                            !   hardwired.
  allocate(vector(num_fields))
  vector=.false.
  vector(1:2*nsig_gfs)=uv_hyb_ens
  call general_sub2grid_create_info(grd_gfs,inner_vars,nlat_gfs,nlon_gfs,nsig_gfs,num_fields, &
                                  .not.regional,vector)
  jcap_gfs=sighead%jcap
  jcap_gfs_test=jcap_gfs
  call general_init_spec_vars(sp_gfs,jcap_gfs,jcap_gfs_test,grd_gfs%nlat,grd_gfs%nlon)

!  also want to set up regional grid structure variable grd_mix, which still has number of
!   vertical levels set to nsig_gfs, but horizontal dimensions set to regional domain.

  call general_sub2grid_create_info(grd_mix,inner_vars,grd_ens%nlat,grd_ens%nlon,nsig_gfs, &
                                    num_fields,regional,vector)

!  create interpolation information for global grid to regional ensemble grid

  nord_g2r=4
! call g_create_egrid2points_slow(grd_ens%nlat*grd_ens%nlon,region_lat_ens,region_lon_ens, &
!                   grd_gfs%nlat,sp_gfs%rlats,grd_gfs%nlon,sp_gfs%rlons,nord_g2r,p_g2r)
!    following only for no dualres !!!
  call g_create_egrid2points_slow(grd_ens%nlat*grd_ens%nlon,region_lat,region_lon, &
                    grd_gfs%nlat,sp_gfs%rlats,grd_gfs%nlon,sp_gfs%rlons,nord_g2r,p_g2r)

!  allocate mix ensemble space--horizontal on regional domain, vertical still gefs 
  allocate(st_eg(grd_mix%lat2,grd_mix%lon2,grd_mix%nsig,n_ens))
  allocate(vp_eg(grd_mix%lat2,grd_mix%lon2,grd_mix%nsig,n_ens))
  allocate( t_eg(grd_mix%lat2,grd_mix%lon2,grd_mix%nsig,n_ens))
  allocate(rh_eg(grd_mix%lat2,grd_mix%lon2,grd_mix%nsig,n_ens))
  allocate(oz_eg(grd_mix%lat2,grd_mix%lon2,grd_mix%nsig,n_ens))
  allocate(cw_eg(grd_mix%lat2,grd_mix%lon2,grd_mix%nsig,n_ens))
  allocate( p_eg(grd_mix%lat2,grd_mix%lon2,n_ens))
  allocate( p_eg_nmmb(grd_mix%lat2,grd_mix%lon2,n_ens))
  allocate( z_eg(grd_mix%lat2,grd_mix%lon2,n_ens))
  st_eg=zero ; vp_eg=zero ; t_eg=zero ; rh_eg=zero ; oz_eg=zero ; cw_eg=zero ; p_eg=zero ; z_eg=zero
  p_eg_nmmb=zero

!                begin loop over ensemble members

   rewind(10)
  do n=1,n_ens
      read(10,'(a)',err=20,end=20)filename 
      filename=trim(filename)
!     write(filename,100) n
!100        format('sigf06_ens_mem',i3.3)


!!   allocate necessary space on global grid

     allocate( vor(grd_gfs%lat2,grd_gfs%lon2,grd_gfs%nsig))
     allocate( div(grd_gfs%lat2,grd_gfs%lon2,grd_gfs%nsig))
     allocate(   u(grd_gfs%lat2,grd_gfs%lon2,grd_gfs%nsig))
     allocate(   v(grd_gfs%lat2,grd_gfs%lon2,grd_gfs%nsig))
     allocate(  tv(grd_gfs%lat2,grd_gfs%lon2,grd_gfs%nsig))
     allocate(   q(grd_gfs%lat2,grd_gfs%lon2,grd_gfs%nsig))
     allocate(cwmr(grd_gfs%lat2,grd_gfs%lon2,grd_gfs%nsig))
     allocate(  oz(grd_gfs%lat2,grd_gfs%lon2,grd_gfs%nsig))
     allocate(   z(grd_gfs%lat2,grd_gfs%lon2))
     allocate(  ps(grd_gfs%lat2,grd_gfs%lon2))
     vor=zero ; div=zero ; u=zero ; v=zero ; tv=zero ; q=zero ; cwmr=zero ; oz=zero ; z=zero ; ps=zero
     call general_read_gfsatm(grd_gfs,sp_gfs,filename,mype,uv_hyb_ens,z,ps,vor,div,u,v,tv,q,cwmr,oz,iret)
     deallocate(vor,div)
     allocate(work_sub(grd_gfs%lat2,grd_gfs%lon2,num_fields,1))
     do k=1,grd_gfs%nsig
        ku=k ; kv=k+grd_gfs%nsig ; kt=k+2*grd_gfs%nsig ; kq=k+3*grd_gfs%nsig ; koz=k+4*grd_gfs%nsig
        kcw=k+5*grd_gfs%nsig
        do j=1,grd_gfs%lon2
           do i=1,grd_gfs%lat2
              work_sub(i,j,ku,1)=u(i,j,k)
              work_sub(i,j,kv,1)=v(i,j,k)
              work_sub(i,j,kt,1)=tv(i,j,k)
              work_sub(i,j,kq,1)=q(i,j,k)
              work_sub(i,j,koz,1)=oz(i,j,k)
              work_sub(i,j,kcw,1)=cwmr(i,j,k)
           end do
        end do
     end do
     deallocate(u,v,tv,q,oz,cwmr)
     kz=num_fields ; kps=kz-1
     do j=1,grd_gfs%lon2
        do i=1,grd_gfs%lat2
           work_sub(i,j,kz,1)=z(i,j)
           work_sub(i,j,kps,1)=ps(i,j)
        end do
     end do
     deallocate(z,ps)
     allocate(work(grd_gfs%nlat,grd_gfs%nlon,grd_gfs%kbegin_loc:grd_gfs%kend_alloc,1))
     call general_sub2grid(grd_gfs,work_sub,work)
     deallocate(work_sub)

!    then interpolate to regional analysis grid
     allocate(work_reg(grd_mix%nlat,grd_mix%nlon,grd_gfs%kbegin_loc:grd_gfs%kend_alloc,1))
     do k=grd_gfs%kbegin_loc,grd_gfs%kend_loc
        call g_egrid2points_faster(p_g2r,work(:,:,k,1),work_reg(:,:,k,1),vector(k))
     end do
     deallocate(work)

!    next general_grid2sub to go to regional grid subdomains.
     allocate(work_sub(grd_mix%lat2,grd_mix%lon2,num_fields,1))
     call general_grid2sub(grd_mix,work_reg,work_sub)
     deallocate(work_reg)
     allocate(pri(grd_mix%lat2,grd_mix%lon2,grd_mix%nsig+1))
     kz=num_fields ; kps=kz-1
!    compute 3d pressure on interfaces
     kap1=rd_over_cp+one
     kapr=one/rd_over_cp
     pri=zero
     k=1
     k2=grd_mix%nsig+1
     do j=1,grd_mix%lon2
        do i=1,grd_mix%lat2
           pri(i,j,k)=work_sub(i,j,kps,1)
           pri(i,j,k2)=zero
        end do
     end do
     if (sighead%idvc /= 3) then
        do k=2,grd_mix%nsig
           do j=1,grd_mix%lon2
              do i=1,grd_mix%lat2
                 pri(i,j,k)=ak5(k)+bk5(k)*work_sub(i,j,kps,1)
              end do
           end do
        end do
     else
        do k=2,grd_mix%nsig
           kt=k+2*grd_mix%nsig
           do j=1,grd_mix%lon2
              do i=1,grd_mix%lat2
                 trk=(half*(work_sub(i,j,kt-1,1)+work_sub(i,j,kt,1))/tref5(k))**kapr
                 pri(i,j,k)=ak5(k)+(bk5(k)*work_sub(i,j,kps,1))+(ck5(k)*trk)
              end do
           end do
        end do
     end if

     allocate(tsen(grd_mix%lat2,grd_mix%lon2,grd_mix%nsig))
     allocate(qs(grd_mix%lat2,grd_mix%lon2,grd_mix%nsig))
!    Compute RH and potential virtual temp
!    First step is go get sensible temperature and 3d pressure
     do k=1,grd_mix%nsig
        kt=k+2*grd_mix%nsig ; kq=k+3*grd_mix%nsig
        do j=1,grd_mix%lon2
           do i=1,grd_mix%lat2
              tsen(i,j,k)= work_sub(i,j,kt,1)/(one+fv*max(zero,work_sub(i,j,kq,1)))
           end do
        end do
     end do
!    Get 3d pressure field now on interfaces
     allocate(prsl(grd_mix%lat2,grd_mix%lon2,grd_mix%nsig))
     if (idsl5/=2) then
        do j=1,grd_mix%lon2
           do i=1,grd_mix%lat2
              do k=1,grd_mix%nsig
                 prsl(i,j,k)=((pri(i,j,k)**kap1-pri(i,j,k+1)**kap1)/&
                           (kap1*(pri(i,j,k)-pri(i,j,k+1))))**kapr
              end do
           end do
        end do
     else
        do j=1,grd_mix%lon2
           do i=1,grd_mix%lat2
              do k=1,grd_mix%nsig
                 prsl(i,j,k)=(pri(i,j,k)+pri(i,j,k+1))*half
              end do
           end do
        end do
     end if
!  !Compute geopotential height at interface between layers
     ! allocate(zbarl(grd_mix%lat2,grd_mix%lon2,grd_mix%nsig))
     ! allocate(height(grd_mix%nsig))
     ! rdog=rd/grav
     ! do j=1,grd_mix%lon2
     !    do i=1,grd_mix%lat2
     !       k  = 1
     !       kt=k+2*grd_mix%nsig
     !       h  = rdog * work_sub(i,j,kt,1)
     !       dz = h * log(pri(i,j,k)/prsl(i,j,k))
     !       height(k) = work_sub(i,j,kz,1)+dz

     !       do k=2,grd_mix%nsig
     !          kt=k+2*grd_mix%nsig
     !          h  = rdog * half * (work_sub(i,j,kt-1,1)+work_sub(i,j,kt,1))
     !          dz = h * log(prsl(i,j,k-1)/prsl(i,j,k))
     !          height(k) = height(k-1) + dz
     !       end do
     !       do k=1,grd_mix%nsig
     !          zbarl(i,j,k)=height(k)
     !       end do
     !    end do
     ! end do
             deallocate(pri)
    !deallocate(pri,height)
!! recompute pbar using routine Wan-Shu obtained from Matt Pyle:

 !allocate(tt(grd_mix%lat2,grd_mix%lon2,grd_mix%nsig))
  allocate(psfc_out(grd_mix%lat2,grd_mix%lon2))
     !  do k=1,grd_mix%nsig
     !     kt=k+2*grd_mix%nsig
     !     do j=1,grd_mix%lon2
     !        do i=1,grd_mix%lat2
     !           tt(i,j,k)=work_sub(i,j,kt,1)
     !        end do
     !     end do
     !  end do
     !  mm1=mype+1
     ! !ilook=ide/2
     ! !jlook=jde/2
     ! !ilook=29
     ! !jlook=41
     !           ilook=-1 ; jlook=-1
     !  call compute_nmm_surfacep ( ges_z(:,:,ntguessig), zbarl,1000._r_kind*prsl,tt, &
     !                              psfc_out,grd_mix%nsig,grd_mix%lat2,grd_mix%lon2, &
     !                              .true.,ilook,jlook)
     !  deallocate(tt,zbarl)
     !  psfc_out=.001_r_kind*psfc_out
        psfc_out=ges_ps(:,:,ntguessig)
        write(0,*)' min,max ges_ps-psfc_out=',&
             minval(ges_ps(:,:,ntguessig)-psfc_out),maxval(ges_ps(:,:,ntguessig)-psfc_out)
                    pdiffmax=-huge(pdiffmax)
                    pdiffmin= huge(pdiffmin)
        !  do j=2,grd_mix%lon2-1
        !     do i=2,grd_mix%lat2-1
           do j=1,grd_mix%lon2
              do i=1,grd_mix%lat2
                 pdiffmax=max(ges_ps(i,j,ntguessig)-psfc_out(i,j),pdiffmax)
                 pdiffmin=min(ges_ps(i,j,ntguessig)-psfc_out(i,j),pdiffmin)
                 if(ges_ps(i,j,ntguessig)<10._r_kind) &
                    write(0,*)' small ges_ps,i,j,lat2,lon2,ig,jg,ide,jde=',i,j,grd_mix%lat2,grd_mix%lon2,&
                        grd_mix%istart(mm1)-2+i,grd_mix%jstart(mm1)-2+j,grd_mix%nlat,grd_mix%nlon
                 if(psfc_out(i,j)<10._r_kind) &
                    write(0,*)' small ens ps,i,j,lat2,lon2,ig,jg,ide,jde=',i,j,grd_mix%lat2,grd_mix%lon2,&
                        grd_mix%istart(mm1)-2+i,grd_mix%jstart(mm1)-2+j,grd_mix%nlat,grd_mix%nlon
              end do
           end do
           call mpi_allreduce(pdiffmax,pdiffmax0,1,mpi_rtype,mpi_max,mpi_comm_world,ierror)
           call mpi_allreduce(pdiffmin,pdiffmin0,1,mpi_rtype,mpi_min,mpi_comm_world,ierror)
                  if(mype==0) write(0,*)' min,max ges_ps - matt ps =',pdiffmin0,pdiffmax0

                                                     write(fname,'("matt_pbar_corrected")')
                                                     call grads1a(psfc_out,1,mype,trim(fname))
                                                     write(fname,'("ges_ps")')
                                                     call grads1a(ges_ps(:,:,ntguessig),1,mype,trim(fname))

     ice=.true.
     iderivative=0
     call genqsat(qs,tsen,prsl,grd_mix%lat2,grd_mix%lon2,grd_mix%nsig,ice,iderivative)
     deallocate(tsen)

     do k=1,grd_mix%nsig
        kt=k+2*grd_mix%nsig ; kq=k+3*grd_mix%nsig
        do j=1,grd_mix%lon2
           do i=1,grd_mix%lat2
              work_sub(i,j,kq,1) = work_sub(i,j,kq,1)/qs(i,j,k)
              work_sub(i,j,kt,1)=work_sub(i,j,kt,1)/(0.01_r_kind*prsl(i,j,k))**rd_over_cp
           end do
        end do
     end do
     deallocate(qs)
     deallocate(prsl)

     do k=1,grd_mix%nsig
        ku=k ; kv=ku+grd_mix%nsig ; kt=kv+grd_mix%nsig ; kq=kt+grd_mix%nsig ; koz=kq+grd_mix%nsig
        kcw=koz+grd_mix%nsig
        do j=1,grd_mix%lon2
           do i=1,grd_mix%lat2
              st_eg(i,j,k,n)=work_sub(i,j,ku,1)
              vp_eg(i,j,k,n)=work_sub(i,j,kv,1)
               t_eg(i,j,k,n)=work_sub(i,j,kt,1)     !  now pot virtual temp
              rh_eg(i,j,k,n)=work_sub(i,j,kq,1)     !  now rh
              oz_eg(i,j,k,n)=work_sub(i,j,koz,1)
              cw_eg(i,j,k,n)=work_sub(i,j,kcw,1)
           end do
        end do
     end do
     kz=num_fields ; kps=kz-1
     do j=1,grd_mix%lon2
        do i=1,grd_mix%lat2
           p_eg(i,j,n)=work_sub(i,j,kps,1)
           p_eg_nmmb(i,j,n)=psfc_out(i,j)
           z_eg(i,j,n)=work_sub(i,j,kz,1)
        end do
     end do
     deallocate(work_sub,psfc_out)

                    pdiffmax=-huge(pdiffmax)
                    pdiffmin= huge(pdiffmin)
           do j=1,grd_mix%lon2
              do i=1,grd_mix%lat2
                 pdiffmax=max(ges_ps(i,j,ntguessig)-p_eg_nmmb(i,j,n),pdiffmax)
                 pdiffmin=min(ges_ps(i,j,ntguessig)-p_eg_nmmb(i,j,n),pdiffmin)
                  if(ges_ps(i,j,ntguessig)<10._r_kind) &
                     write(0,*)' small ges_ps,i,j,lat2,lon2,ig,jg,ide,jde=',i,j,grd_mix%lat2,grd_mix%lon2,&
                         grd_mix%istart(mm1)-1+i,grd_mix%jstart(mm1)-1+j,grd_mix%nlat,grd_mix%nlon
                  if(p_eg_nmmb(i,j,n)<10._r_kind) &
                     write(0,*)' small ens ps,i,j,lat2,lon2,ig,jg,ide,jde=',i,j,grd_mix%lat2,grd_mix%lon2,&
                         grd_mix%istart(mm1)-1+i,grd_mix%jstart(mm1)-1+j,grd_mix%nlat,grd_mix%nlon
              end do
           end do
           call mpi_allreduce(pdiffmax,pdiffmax0,1,mpi_rtype,mpi_max,mpi_comm_world,ierror)
           call mpi_allreduce(pdiffmin,pdiffmin0,1,mpi_rtype,mpi_min,mpi_comm_world,ierror)
                   if(mype==0) write(0,*)' with halo, n,min,max ges_ps - matt ps =',n,pdiffmin0,pdiffmax0

  end do   !  end loop over ensemble members.

!   next, compute mean of ensembles.

  allocate(stbar(grd_mix%lat2,grd_mix%lon2,grd_mix%nsig))
  allocate(vpbar(grd_mix%lat2,grd_mix%lon2,grd_mix%nsig))
  allocate( tbar(grd_mix%lat2,grd_mix%lon2,grd_mix%nsig))
  allocate(rhbar(grd_mix%lat2,grd_mix%lon2,grd_mix%nsig))
  allocate(ozbar(grd_mix%lat2,grd_mix%lon2,grd_mix%nsig))
  allocate(cwbar(grd_mix%lat2,grd_mix%lon2,grd_mix%nsig))
  allocate(pbar(grd_mix%lat2,grd_mix%lon2))
  allocate(pbar_nmmb(grd_mix%lat2,grd_mix%lon2))
  allocate( zbar(grd_mix%lat2,grd_mix%lon2))
  allocate(sstbar(grd_mix%lat2,grd_mix%lon2))

!   compute mean state
  stbar=zero ; vpbar=zero ; tbar=zero ; rhbar=zero ; ozbar=zero ; cwbar=zero ; pbar=zero
  zbar=zero ; sstbar=zero ; pbar_nmmb=zero
  do n=1,n_ens
     do k=1,grd_mix%nsig
        do j=1,grd_mix%lon2
           do i=1,grd_mix%lat2
              stbar(i,j,k)=stbar(i,j,k)+st_eg(i,j,k,n)
              vpbar(i,j,k)=vpbar(i,j,k)+vp_eg(i,j,k,n)
               tbar(i,j,k)= tbar(i,j,k)+ t_eg(i,j,k,n)
              rhbar(i,j,k)=rhbar(i,j,k)+rh_eg(i,j,k,n)
              ozbar(i,j,k)=ozbar(i,j,k)+oz_eg(i,j,k,n)
              cwbar(i,j,k)=cwbar(i,j,k)+cw_eg(i,j,k,n)
           end do
        end do
     end do
     do j=1,grd_mix%lon2
        do i=1,grd_mix%lat2
           pbar(i,j)=pbar(i,j)+p_eg(i,j,n)
           pbar_nmmb(i,j)=pbar_nmmb(i,j)+p_eg_nmmb(i,j,n)
           zbar(i,j)=zbar(i,j)+z_eg(i,j,n)
        end do
     end do
  end do

! Convert to mean
  bar_norm = one/float(n_ens)
  do k=1,grd_mix%nsig
     do j=1,grd_mix%lon2
        do i=1,grd_mix%lat2
           stbar(i,j,k)=stbar(i,j,k)*bar_norm
           vpbar(i,j,k)=vpbar(i,j,k)*bar_norm
            tbar(i,j,k)= tbar(i,j,k)*bar_norm
           rhbar(i,j,k)=rhbar(i,j,k)*bar_norm
           ozbar(i,j,k)=ozbar(i,j,k)*bar_norm
           cwbar(i,j,k)=cwbar(i,j,k)*bar_norm
        end do
     end do
  end do
  do j=1,grd_mix%lon2
     do i=1,grd_mix%lat2
        pbar(i,j)=pbar(i,j)*bar_norm
        pbar_nmmb(i,j)=pbar_nmmb(i,j)*bar_norm
!   also save pbar to module array ps_bar for possible use in vertical localization
!                                                    in terms of scale heights/normalized p/p
        ps_bar(i,j)=pbar_nmmb(i,j)
        zbar(i,j)=zbar(i,j)*bar_norm
     end do
  end do
                                                 write(fname,'("test_pbar_uncorrected")')
                                                 call grads1a(pbar,1,mype,trim(fname))
                                                 write(fname,'("test_ges_ps")')
                                                 call grads1a(ges_ps,1,mype,trim(fname))
                                                 write(fname,'("test_ges_z")')
                                                 call grads1a(ges_z,1,mype,trim(fname))

! Subtract mean from ensemble members, but save scaling by sqrt(1/(nens-1)) until after vertical interpolation
  do n=1,n_ens
     do k=1,grd_mix%nsig
        do j=1,grd_mix%lon2
           do i=1,grd_mix%lat2
              st_eg(i,j,k,n)=st_eg(i,j,k,n)-stbar(i,j,k)
              vp_eg(i,j,k,n)=vp_eg(i,j,k,n)-vpbar(i,j,k)
               t_eg(i,j,k,n)= t_eg(i,j,k,n)- tbar(i,j,k)
              rh_eg(i,j,k,n)=rh_eg(i,j,k,n)-rhbar(i,j,k)
              oz_eg(i,j,k,n)=oz_eg(i,j,k,n)-ozbar(i,j,k)
              cw_eg(i,j,k,n)=cw_eg(i,j,k,n)-cwbar(i,j,k)
           end do
        end do
     end do
     do j=1,grd_mix%lon2
        do i=1,grd_mix%lat2
           p_eg_nmmb(i,j,n)=p_eg_nmmb(i,j,n)-pbar_nmmb(i,j)
           p_eg(i,j,n)=p_eg(i,j,n)-pbar(i,j)
        end do
     end do
  end do

!   now obtain mean pressure prsl
!    compute 3d pressure on interfaces
     kap1=rd_over_cp+one
     kapr=one/rd_over_cp
     allocate(pri(grd_mix%lat2,grd_mix%lon2,grd_mix%nsig+1))
     pri=zero
     k=1
     k2=grd_mix%nsig+1
     do j=1,grd_mix%lon2
        do i=1,grd_mix%lat2
           pri(i,j,k)=pbar(i,j)
           pri(i,j,k2)=zero
        end do
     end do
     if (sighead%idvc /= 3) then
        do k=2,grd_mix%nsig
           do j=1,grd_mix%lon2
              do i=1,grd_mix%lat2
                 pri(i,j,k)=ak5(k)+bk5(k)*pbar(i,j)
              end do
           end do
        end do
     else
        do k=2,grd_mix%nsig
           do j=1,grd_mix%lon2
              do i=1,grd_mix%lat2
                 trk=(half*(tbar(i,j,k-1)+tbar(i,j,k))/tref5(k))**kapr
                 pri(i,j,k)=ak5(k)+(bk5(k)*pbar(i,j))+(ck5(k)*trk)
              end do
           end do
        end do
     end if

!    Get 3d pressure field now at layer midpoints
     allocate(prsl(grd_mix%lat2,grd_mix%lon2,grd_mix%nsig))
     if (sighead%idsl/=2) then
        do j=1,grd_mix%lon2
           do i=1,grd_mix%lat2
              do k=1,grd_mix%nsig
                 prsl(i,j,k)=((pri(i,j,k)**kap1-pri(i,j,k+1)**kap1)/&
                           (kap1*(pri(i,j,k)-pri(i,j,k+1))))**kapr
              end do
           end do
        end do
     else
        do j=1,grd_mix%lon2
           do i=1,grd_mix%lat2
              do k=1,grd_mix%nsig
                 prsl(i,j,k)=(pri(i,j,k)+pri(i,j,k+1))*half
              end do
           end do
        end do
     end if
     deallocate(pri)

! interpolate/extrapolate in vertical using yoshi's spline code.

!  first need ges_prsl_e, the 3d pressure on the ensemble grid.

  allocate(ges_prsl_e(grd_ens%lat2,grd_ens%lon2,grd_ens%nsig,1))
  if(dual_res) then
     call general_suba2sube(grd_a1,grd_e1,p_e2a,ges_prsl(:,:,:,ntguessig:ntguessig),ges_prsl_e,regional)
  else
     ges_prsl_e(:,:,:,1)=ges_prsl(:,:,:,ntguessig)
  end if
!                      write(0,*)' ntguessig,regional,min,max ges_prsl_e=',&
!                                        ntguessig,regional,minval(ges_prsl_e),maxval(ges_prsl_e)

  allocate(xspli(grd_mix%nsig),yspli(grd_mix%nsig),xsplo(grd_ens%nsig),ysplo(grd_ens%nsig))

  allocate(glb_umin(grd_mix%nsig),glb_umax(grd_mix%nsig))
  allocate(glb_umin0(grd_mix%nsig),glb_umax0(grd_mix%nsig))
  allocate(glb_vmin(grd_mix%nsig),glb_vmax(grd_mix%nsig))
  allocate(glb_vmin0(grd_mix%nsig),glb_vmax0(grd_mix%nsig))
  allocate(glb_tmin(grd_mix%nsig),glb_tmax(grd_mix%nsig))
  allocate(glb_tmin0(grd_mix%nsig),glb_tmax0(grd_mix%nsig))
  allocate(glb_rhmin(grd_mix%nsig),glb_rhmax(grd_mix%nsig))
  allocate(glb_rhmin0(grd_mix%nsig),glb_rhmax0(grd_mix%nsig))
  allocate(glb_ozmin(grd_mix%nsig),glb_ozmax(grd_mix%nsig))
  allocate(glb_ozmin0(grd_mix%nsig),glb_ozmax0(grd_mix%nsig))
  allocate(glb_cwmin(grd_mix%nsig),glb_cwmax(grd_mix%nsig))
  allocate(glb_cwmin0(grd_mix%nsig),glb_cwmax0(grd_mix%nsig))

  allocate(reg_umin(grd_ens%nsig),reg_umax(grd_ens%nsig))
  allocate(reg_umin0(grd_ens%nsig),reg_umax0(grd_ens%nsig))
  allocate(reg_vmin(grd_ens%nsig),reg_vmax(grd_ens%nsig))
  allocate(reg_vmin0(grd_ens%nsig),reg_vmax0(grd_ens%nsig))
  allocate(reg_tmin(grd_ens%nsig),reg_tmax(grd_ens%nsig))
  allocate(reg_tmin0(grd_ens%nsig),reg_tmax0(grd_ens%nsig))
  allocate(reg_rhmin(grd_ens%nsig),reg_rhmax(grd_ens%nsig))
  allocate(reg_rhmin0(grd_ens%nsig),reg_rhmax0(grd_ens%nsig))
  allocate(reg_ozmin(grd_ens%nsig),reg_ozmax(grd_ens%nsig))
  allocate(reg_ozmin0(grd_ens%nsig),reg_ozmax0(grd_ens%nsig))
  allocate(reg_cwmin(grd_ens%nsig),reg_cwmax(grd_ens%nsig))
  allocate(reg_cwmin0(grd_ens%nsig),reg_cwmax0(grd_ens%nsig))

  glb_umin= huge(glb_umin)
  glb_umax=-huge(glb_umax)
  glb_vmin= huge(glb_vmin)
  glb_vmax=-huge(glb_vmax)
  glb_tmin= huge(glb_tmin)
  glb_tmax=-huge(glb_tmax)
  glb_rhmin= huge(glb_rhmin)
  glb_rhmax=-huge(glb_rhmax)
  glb_ozmin= huge(glb_ozmin)
  glb_ozmax=-huge(glb_ozmax)
  glb_cwmin= huge(glb_cwmin)
  glb_cwmax=-huge(glb_cwmax)

  reg_umin= huge(reg_umin)
  reg_umax=-huge(reg_umax)
  reg_vmin= huge(reg_vmin)
  reg_vmax=-huge(reg_vmax)
  reg_tmin= huge(reg_tmin)
  reg_tmax=-huge(reg_tmax)
  reg_rhmin= huge(reg_rhmin)
  reg_rhmax=-huge(reg_rhmax)
  reg_ozmin= huge(reg_ozmin)
  reg_ozmax=-huge(reg_ozmax)
  reg_cwmin= huge(reg_cwmin)
  reg_cwmax=-huge(reg_cwmax)

  allocate(ut(grd_ens%lat2,grd_ens%lon2,grd_ens%nsig))
  allocate(vt(grd_ens%lat2,grd_ens%lon2,grd_ens%nsig))
  allocate(tt(grd_ens%lat2,grd_ens%lon2,grd_ens%nsig))
  allocate(rht(grd_ens%lat2,grd_ens%lon2,grd_ens%nsig))
  allocate(ozt(grd_ens%lat2,grd_ens%lon2,grd_ens%nsig))
  allocate(cwt(grd_ens%lat2,grd_ens%lon2,grd_ens%nsig))
!                           if(mype==0) write(0,*)' at 5 in get_gefs_for_regional'
  do n=1,n_ens
     do j=1,grd_ens%lon2
        do i=1,grd_ens%lat2
           do k=1,grd_mix%nsig
              xspli(k)=log(prsl(i,j,k)*10.0_r_kind)
           end do
           do k=1,grd_ens%nsig
              xsplo(k)=log(ges_prsl_e(i,j,k,1)*10._r_kind)
           end do

!    u
           do k=1,grd_mix%nsig
              yspli(k)=st_eg(i,j,k,n)
!              glb_umax(k)=max(yspli(k),glb_umax(k))
!              glb_umin(k)=min(yspli(k),glb_umin(k))
           end do
           call intp_spl(xspli,yspli,xsplo,ysplo,grd_mix%nsig,grd_ens%nsig)
!               following is to correct for bug in intp_spl
           do k=1,grd_ens%nsig
              if(xsplo(k) < xspli(grd_mix%nsig)) ysplo(k)=yspli(grd_mix%nsig)
              if(xsplo(k) > xspli(1)) ysplo(k)=yspli(1)
           end do
           do k=1,grd_ens%nsig
              ut(i,j,k)=ysplo(k)
!              reg_umax(k)=max(ysplo(k),reg_umax(k))
!              reg_umin(k)=min(ysplo(k),reg_umin(k))
           end do
!    v
           do k=1,grd_mix%nsig
              yspli(k)=vp_eg(i,j,k,n)
!              glb_vmax(k)=max(yspli(k),glb_vmax(k))
!              glb_vmin(k)=min(yspli(k),glb_vmin(k))
           end do
           call intp_spl(xspli,yspli,xsplo,ysplo,grd_mix%nsig,grd_ens%nsig)
!               following is to correct for bug in intp_spl
           do k=1,grd_ens%nsig
              if(xsplo(k) < xspli(grd_mix%nsig)) ysplo(k)=yspli(grd_mix%nsig)
              if(xsplo(k) > xspli(1)) ysplo(k)=yspli(1)
           end do
           do k=1,grd_ens%nsig
              vt(i,j,k)=ysplo(k)
!              reg_vmax(k)=max(ysplo(k),reg_vmax(k))
!              reg_vmin(k)=min(ysplo(k),reg_vmin(k))
           end do
!    t
           do k=1,grd_mix%nsig
              yspli(k)=t_eg(i,j,k,n)
!              glb_tmax(k)=max(yspli(k),glb_tmax(k))
!              glb_tmin(k)=min(yspli(k),glb_tmin(k))
           end do
           call intp_spl(xspli,yspli,xsplo,ysplo,grd_mix%nsig,grd_ens%nsig)
!               following is to correct for bug in intp_spl
           do k=1,grd_ens%nsig
              if(xsplo(k) < xspli(grd_mix%nsig)) ysplo(k)=yspli(grd_mix%nsig)
              if(xsplo(k) > xspli(1)) ysplo(k)=yspli(1)
           end do
           do k=1,grd_ens%nsig
              ysplo(k)=ysplo(k)*(0.01_r_kind*ges_prsl_e(i,j,k,1))**rd_over_cp  ! converting from pot Tv to Tv
              tt(i,j,k)=ysplo(k)
!              reg_tmax(k)=max(ysplo(k),reg_tmax(k))
!              reg_tmin(k)=min(ysplo(k),reg_tmin(k))
           end do
!    rh
           do k=1,grd_mix%nsig
              yspli(k)=rh_eg(i,j,k,n)
!              glb_rhmax(k)=max(yspli(k),glb_rhmax(k))
!              glb_rhmin(k)=min(yspli(k),glb_rhmin(k))
           end do
           call intp_spl(xspli,yspli,xsplo,ysplo,grd_mix%nsig,grd_ens%nsig)
!               following is to correct for bug in intp_spl
           do k=1,grd_ens%nsig
              if(xsplo(k) < xspli(grd_mix%nsig)) ysplo(k)=yspli(grd_mix%nsig)
              if(xsplo(k) > xspli(1)) ysplo(k)=yspli(1)
           end do
           do k=1,grd_ens%nsig
              rht(i,j,k)=ysplo(k)
!              reg_rhmax(k)=max(ysplo(k),reg_rhmax(k))
!              reg_rhmin(k)=min(ysplo(k),reg_rhmin(k))
           end do
!       oz
           do k=1,grd_mix%nsig
              yspli(k)=oz_eg(i,j,k,n)
!              glb_ozmax(k)=max(yspli(k),glb_ozmax(k))
!              glb_ozmin(k)=min(yspli(k),glb_ozmin(k))
           end do
           call intp_spl(xspli,yspli,xsplo,ysplo,grd_mix%nsig,grd_ens%nsig)
!               following is to correct for bug in intp_spl
           do k=1,grd_ens%nsig
              if(xsplo(k) < xspli(grd_mix%nsig)) ysplo(k)=yspli(grd_mix%nsig)
              if(xsplo(k) > xspli(1)) ysplo(k)=yspli(1)
           end do
           do k=1,grd_ens%nsig
              ozt(i,j,k)=ysplo(k)
!              reg_ozmax(k)=max(ysplo(k),reg_ozmax(k))
!              reg_ozmin(k)=min(ysplo(k),reg_ozmin(k))
           end do
!    cw
           do k=1,grd_mix%nsig
              yspli(k)=cw_eg(i,j,k,n)
!              glb_cwmax(k)=max(yspli(k),glb_cwmax(k))
!              glb_cwmin(k)=min(yspli(k),glb_cwmin(k))
           end do
           call intp_spl(xspli,yspli,xsplo,ysplo,grd_mix%nsig,grd_ens%nsig)
!               following is to correct for bug in intp_spl
           do k=1,grd_ens%nsig
              if(xsplo(k) < xspli(grd_mix%nsig)) ysplo(k)=yspli(grd_mix%nsig)
              if(xsplo(k) > xspli(1)) ysplo(k)=yspli(1)
           end do
           do k=1,grd_ens%nsig
              cwt(i,j,k)=ysplo(k)
!              reg_cwmax(k)=max(ysplo(k),reg_cwmax(k))
!              reg_cwmin(k)=min(ysplo(k),reg_cwmin(k))
           end do

        end do
     end do



!   transfer from temporary arrays to perturbation arrays and normalize by sig_norm
     sig_norm=sqrt(one/max(one,n_ens_temp-one))
     if(n_ens_temp==n_ens.and.n==n_ens+1) sig_norm=one
                                                  if(mod(n,20)==0) then
                                                      write(fname,'("test_up_",i2.2)')n
                                                      call grads1a(ut,grd_ens%nsig,mype,trim(fname))
                                                      write(fname,'("test_vp_",i2.2)')n
                                                      call grads1a(vt,grd_ens%nsig,mype,trim(fname))
                                                      write(fname,'("test_tp_",i2.2)')n
                                                      call grads1a(tt,grd_ens%nsig,mype,trim(fname))
                                                      write(fname,'("test_rhp_",i2.2)')n
                                                      call grads1a(rht,grd_ens%nsig,mype,trim(fname))
                                                      write(fname,'("test_ozp_",i2.2)')n
                                                      call grads1a(ozt,grd_ens%nsig,mype,trim(fname))
                                                      write(fname,'("test_cwp_",i2.2)')n
                                                      call grads1a(cwt,grd_ens%nsig,mype,trim(fname))
                                                  end if
     do ic3=1,nc3d

        call gsi_bundlegetpointer(en_perts(n),trim(cvars3d(ic3)),w3,istatus)
        if(istatus/=0) then
           write(6,*)' error retrieving pointer to ',trim(cvars3d(ic3)),' for ensemble member ',n
           call stop2(999)
        end if

        select case (trim(cvars3d(ic3)))

           case('sf','SF')

              do k=1,grd_ens%nsig
                 do j=1,grd_ens%lon2
                    do i=1,grd_ens%lat2
                       w3(i,j,k) = ut(i,j,k)*sig_norm
                    end do
                 end do
              end do

           case('vp','VP')

              do k=1,grd_ens%nsig
                 do j=1,grd_ens%lon2
                    do i=1,grd_ens%lat2
                       w3(i,j,k) = vt(i,j,k)*sig_norm
                    end do
                 end do
              end do

           case('t','T')

              do k=1,grd_ens%nsig
                 do j=1,grd_ens%lon2
                    do i=1,grd_ens%lat2
                       w3(i,j,k) = tt(i,j,k)*sig_norm
                    end do
                 end do
              end do

           case('q','Q')

              do k=1,grd_ens%nsig
                 do j=1,grd_ens%lon2
                    do i=1,grd_ens%lat2
                       w3(i,j,k) = rht(i,j,k)*sig_norm
                    end do
                 end do
              end do

           case('oz','OZ')
!          temporarily ignore ozone perturbations

              do k=1,grd_ens%nsig
                 do j=1,grd_ens%lon2
                    do i=1,grd_ens%lat2
                   !   w3(i,j,k) = ozt(i,j,k)*sig_norm
                       w3(i,j,k) = zero
                    end do
                 end do
              end do

           case('cw','CW')
!          temporarily ignore cloud water perturbations

              do k=1,grd_ens%nsig
                 do j=1,grd_ens%lon2
                    do i=1,grd_ens%lat2
                   !   w3(i,j,k) = cwt(i,j,k)*sig_norm
                       w3(i,j,k) = zero
                    end do
                 end do
              end do

        end select
     end do
     do ic2=1,nc2d

        call gsi_bundlegetpointer(en_perts(n),trim(cvars2d(ic2)),w2,istatus)
        if(istatus/=0) then
           write(6,*)' error retrieving pointer to ',trim(cvars2d(ic2)),' for ensemble member ',n
           call stop2(999)
        end if

        select case (trim(cvars2d(ic2)))

           case('ps','PS')

                 do j=1,grd_ens%lon2
                    do i=1,grd_ens%lat2
                       w2(i,j) = p_eg_nmmb(i,j,n)*sig_norm
                    end do
                 end do

           case('sst','SST')

! dtk: temporarily ignore sst perturbations in hybrid
                 do j=1,grd_ens%lon2
                    do i=1,grd_ens%lat2
                       w2(i,j) = zero
                    end do
                 end do

        end select
     end do
  end do


!  call mpi_allreduce(reg_umax,reg_umax0,grd_ens%nsig,mpi_rtype,mpi_max,mpi_comm_world,ierror)
!  call mpi_allreduce(reg_umin,reg_umin0,grd_ens%nsig,mpi_rtype,mpi_min,mpi_comm_world,ierror)
!  call mpi_allreduce(glb_umax,glb_umax0,grd_mix%nsig,mpi_rtype,mpi_max,mpi_comm_world,ierror)
!  call mpi_allreduce(glb_umin,glb_umin0,grd_mix%nsig,mpi_rtype,mpi_min,mpi_comm_world,ierror)
!  if(mype == 0) then
!     do k=1,grd_mix%nsig
!        write(0,'(" k,glb_umin,max=",i4,2e15.4)') k,glb_umin0(k),glb_umax0(k)
!     end do
!     do k=1,grd_ens%nsig
!        write(0,'(" k,reg_umin,max=",i4,2e15.4)') k,reg_umin0(k),reg_umax0(k)
!     end do
!  end if
!                           if(mype==0) write(0,*)' at 7 in get_gefs_for_regional'

!  call mpi_allreduce(reg_vmax,reg_vmax0,grd_ens%nsig,mpi_rtype,mpi_max,mpi_comm_world,ierror)
!  call mpi_allreduce(reg_vmin,reg_vmin0,grd_ens%nsig,mpi_rtype,mpi_min,mpi_comm_world,ierror)
!  call mpi_allreduce(glb_vmax,glb_vmax0,grd_mix%nsig,mpi_rtype,mpi_max,mpi_comm_world,ierror)
!  call mpi_allreduce(glb_vmin,glb_vmin0,grd_mix%nsig,mpi_rtype,mpi_min,mpi_comm_world,ierror)
!  if(mype == 0) then
!     do k=1,grd_mix%nsig
!        write(0,'(" k,glb_vmin,max=",i4,2e15.4)') k,glb_vmin0(k),glb_vmax0(k)
!     end do
!     do k=1,grd_ens%nsig
!        write(0,'(" k,reg_vmin,max=",i4,2e15.4)') k,reg_vmin0(k),reg_vmax0(k)
!     end do
!  end if
!                           if(mype==0) write(0,*)' at 8 in get_gefs_for_regional'

!  call mpi_allreduce(reg_tmax,reg_tmax0,grd_ens%nsig,mpi_rtype,mpi_max,mpi_comm_world,ierror)
!  call mpi_allreduce(reg_tmin,reg_tmin0,grd_ens%nsig,mpi_rtype,mpi_min,mpi_comm_world,ierror)
!  call mpi_allreduce(glb_tmax,glb_tmax0,grd_mix%nsig,mpi_rtype,mpi_max,mpi_comm_world,ierror)
!  call mpi_allreduce(glb_tmin,glb_tmin0,grd_mix%nsig,mpi_rtype,mpi_min,mpi_comm_world,ierror)
!  if(mype == 0) then
!     do k=1,grd_mix%nsig
!        write(0,'(" k,glb_tmin,max=",i4,2e15.4)') k,glb_tmin0(k),glb_tmax0(k)
!     end do
!     do k=1,grd_ens%nsig
!        write(0,'(" k,reg_tmin,max=",i4,2e15.4)') k,reg_tmin0(k),reg_tmax0(k)
!     end do
!  end if

!  call mpi_allreduce(reg_rhmax,reg_rhmax0,grd_ens%nsig,mpi_rtype,mpi_max,mpi_comm_world,ierror)
!  call mpi_allreduce(reg_rhmin,reg_rhmin0,grd_ens%nsig,mpi_rtype,mpi_min,mpi_comm_world,ierror)
!  call mpi_allreduce(glb_rhmax,glb_rhmax0,grd_mix%nsig,mpi_rtype,mpi_max,mpi_comm_world,ierror)
!  call mpi_allreduce(glb_rhmin,glb_rhmin0,grd_mix%nsig,mpi_rtype,mpi_min,mpi_comm_world,ierror)
!  if(mype == 0) then
!     do k=1,grd_mix%nsig
!        write(0,'(" k,glb_rhmin,max=",i4,2e15.4)') k,glb_rhmin0(k),glb_rhmax0(k)
!     end do
!     do k=1,grd_ens%nsig
!        write(0,'(" k,reg_rhmin,max=",i4,2e15.4)') k,reg_rhmin0(k),reg_rhmax0(k)
!     end do
!  end if
!                           if(mype==0) write(0,*)' at 9 in get_gefs_for_regional'

!  call mpi_allreduce(reg_ozmax,reg_ozmax0,grd_ens%nsig,mpi_rtype,mpi_max,mpi_comm_world,ierror)
!  call mpi_allreduce(reg_ozmin,reg_ozmin0,grd_ens%nsig,mpi_rtype,mpi_min,mpi_comm_world,ierror)
!  call mpi_allreduce(glb_ozmax,glb_ozmax0,grd_mix%nsig,mpi_rtype,mpi_max,mpi_comm_world,ierror)
!  call mpi_allreduce(glb_ozmin,glb_ozmin0,grd_mix%nsig,mpi_rtype,mpi_min,mpi_comm_world,ierror)
!  if(mype == 0) then
!     do k=1,grd_mix%nsig
!        write(0,'(" k,glb_ozmin,max=",i4,2e15.4)') k,glb_ozmin0(k),glb_ozmax0(k)
!     end do
!     do k=1,grd_ens%nsig
!        write(0,'(" k,reg_ozmin,max=",i4,2e15.4)') k,reg_ozmin0(k),reg_ozmax0(k)
!     end do
!  end if

!  call mpi_allreduce(reg_cwmax,reg_cwmax0,grd_ens%nsig,mpi_rtype,mpi_max,mpi_comm_world,ierror)
!  call mpi_allreduce(reg_cwmin,reg_cwmin0,grd_ens%nsig,mpi_rtype,mpi_min,mpi_comm_world,ierror)
!  call mpi_allreduce(glb_cwmax,glb_cwmax0,grd_mix%nsig,mpi_rtype,mpi_max,mpi_comm_world,ierror)
!  call mpi_allreduce(glb_cwmin,glb_cwmin0,grd_mix%nsig,mpi_rtype,mpi_min,mpi_comm_world,ierror)
!  if(mype == 0) then
!     do k=1,grd_mix%nsig
!        write(0,'(" k,glb_cwmin,max=",i4,2e15.4)') k,glb_cwmin0(k),glb_cwmax0(k)
!     end do
!     do k=1,grd_ens%nsig
!        write(0,'(" k,reg_cwmin,max=",i4,2e15.4)') k,reg_cwmin0(k),reg_cwmax0(k)
!     end do
!  end if

!                           if(mype==0) write(0,*)' at 10 in get_gefs_for_regional'
!                           if(mype==0) write(0,*)' at 11 in get_gefs_for_regional'
                               ! if(1/=0) then
                               !     call mpi_finalize(i)
                               !     stop
                               ! end if

  return

30 write(6,*) 'GET_GEFS+FOR_REGIONAL open filelist failed '
   call stop2(555)
20 write(6,*) 'GET_GEFS+FOR_REGIONAL read gfs ens failed ',n
   call stop2(555)
end subroutine get_gefs_for_regional

  SUBROUTINE compute_nmm_surfacep ( TERRAIN_HGT_T, Z3D_IN, PRESS3D_IN, T3D_IN   &
     &,                             psfc_out,generic,IME,JME,spectral, Ilook,Jlook )

	
       use kinds, only: r_kind,i_kind
       IMPLICIT NONE

       real(r_kind), allocatable:: dum2d(:,:),DUM2DB(:,:)
       
       integer(i_kind) :: IME,JME
       integer(i_kind) :: Ilook,Jlook
       integer(i_kind) :: I,J,II,generic,L,KINSERT,K,bot_lev,LL
       integer(i_kind) :: loopinc,iloopinc
	
       real(r_kind) :: TERRAIN_HGT_T(IME,JME)
       real(r_kind) :: Z3D_IN(IME,JME,generic)
       real(r_kind) :: T3D_IN(IME,JME,generic)
       real(r_kind) :: PRESS3D_IN(IME,JME,generic)
       real(r_kind) :: PSFC_IN(IME,JME),TOPO_IN(IME,JME)
       real(r_kind) :: psfc_out(IME,JME)
       real(r_kind) :: dif1,dif2,dif3,dif4,dlnpdz,BOT_INPUT_HGT,BOT_INPUT_PRESS,dpdz,rhs
       real(r_kind) :: zin(generic),pin(generic)

       character (len=132) :: message
	
       logical :: DEFINED_PSFC(IME,JME), DEFINED_PSFCB(IME,JME), spectral

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!	write(0,*) 'size(TERRAIN_HGT_T):: ', size(TERRAIN_HGT_T,dim=1),size(TERRAIN_HGT_T,dim=2)
!	write(0,*) 'what is JME here??? : ', JME
!	write(0,*) 'JmE: ', JmE

       DO J=1,JME
       DO I=1,IME
          DEFINED_PSFC(I,J)=.FALSE.
          DEFINED_PSFCB(I,J)=.FALSE.
        IF (PRESS3D_IN(I,J,1) .ne. 200100.) THEN
          PSFC_IN(I,J)=PRESS3D_IN(I,J,1)
          TOPO_IN(I,J)=Z3D_IN(I,J,1)
        ELSE
          PSFC_IN(I,J)=PRESS3D_IN(I,J,2)
          TOPO_IN(I,J)=Z3D_IN(I,J,2)
        ENDIF
       ENDDO
       ENDDO

!write(0,*) 'terrain_hgt_t in surfacep compute ', IME,JME
!do J=JME,1,min(-(JME-1)/20,-1)
!write(0,535) J,(TERRAIN_HGT_T(I,J),I=1,IME,max(1,(IME-1)/12))
!enddo

!write(0,*) 'z3d_in(3) at same points:'
!do J=JME,1,min(-(JME-1)/20,-1)
!write(0,535) J,(Z3D_IN(I,J,3),I=1,IME,max(1,(IME-1)/12))
!enddo
! 535	format(I4,' ::: ',18(f5.0,1x))

       ALLOCATE(DUM2D(IME,JME))
     
       DO J=1,JME
        DO I=1,IME
         DUM2D(I,J)=-9.
        END DO
       END DO

       DO J=1,JmE
        I_loop: DO I=1,ImE

         IF (PSFC_IN(I,J) .eq. 0.) THEN
           write(0,*) 'QUITTING BECAUSE I,J, PSFC_IN: ', I,J,PSFC_IN(I,J)

	STOP
         ENDIF

         BOT_INPUT_PRESS=PSFC_IN(I,J)
         BOT_INPUT_HGT=TOPO_IN(I,J)


        IF (I .eq. Ilook .AND. J .eq. Jlook) THEN

!	   write(0,*) ' TERRAIN_HGT_T: ', I,J, TERRAIN_HGT_T(I,J)
	   write(0,*) ' PSFC_IN, TOPO_IN: ', &
                            I, J, PSFC_IN(I,J),TOPO_IN(I,J)

           DO L=1,generic
	     write(0,*) ' L,PRESS3D_IN, Z3D_IN: ', &
                             I,J,L, PRESS3D_IN(I,J,L),Z3D_IN(I,J,L)
           END DO
         ENDIF

!	do L=2,generic
       DO L=generic,2,-1

         IF ( PRESS3D_IN(i,j,L) .gt. PSFC_IN(I,J) .AND.  &
             Z3D_IN(I,J,L) .lt. TERRAIN_HGT_T(I,J) .AND. &
             Z3D_IN(I,J,L+1) .gt. TERRAIN_HGT_T(I,J) ) THEN

           BOT_INPUT_PRESS=PRESS3D_IN(i,j,L)
           BOT_INPUT_HGT=Z3D_IN(I,J,L)

           IF (I .eq. Ilook .and. J .eq. Jlook) THEN
             write(0,*) 'BOT_INPUT_PRESS, BOT_INPUT_HGT NOW : ', &
                         Ilook,Jlook, BOT_INPUT_PRESS, BOT_INPUT_HGT
           ENDIF

          ENDIF 
       END DO	

           IF (I .eq. Ilook .and. J .eq. Jlook) THEN
	write(0,*) 'enter this section, TERRAIN_HGT_T, BOT_INPUT_HGT: ', TERRAIN_HGT_T(I,J), BOT_INPUT_HGT
           ENDIF

        IF (TERRAIN_HGT_T(I,J) .eq. BOT_INPUT_HGT ) THEN
           dum2d(I,J)=BOT_INPUT_PRESS
           DEFINED_PSFC(I,J)=.TRUE.
           IF (I .eq. Ilook .and. J .eq. Jlook) THEN
	   write(0,*) 'TERRAIN_HGT_T .eq. BOT_INPUT_HGT, set dum2d to: ', I,J, dum2d(I,J)
           ENDIF

	! IF (BOT_INPUT_HGT .ne. 0. .and. (BOT_INPUT_HGT-INT(BOT_INPUT_HGT) .ne. 0.) ) THEN
	!   write(0,*) 'with BOT_INPUT_HGT: ', BOT_INPUT_HGT, &
        !                    'set dum2d to bot_input_pres: ', I,J,dum2d(I,J)
        ! ENDIF

        ELSEIF (TERRAIN_HGT_T(I,J) .lt. BOT_INPUT_HGT ) THEN

!         target is below lowest possible input...extrapolate

          IF ( BOT_INPUT_PRESS-PRESS3D_IN(I,J,2) .gt. 500. ) THEN
            dlnpdz= (log(BOT_INPUT_PRESS)-log(PRESS3D_IN(i,j,2)) ) / &
                     (BOT_INPUT_HGT-Z3D_IN(i,j,2))
            IF (I .eq. Ilook .and. J .eq. Jlook) THEN
              write(0,*) 'I,J,dlnpdz(a): ', I,J,dlnpdz
            ENDIF

          ELSE

!! thin layer and/or just have lowest level - difference with 3rd level data
            IF ( abs(BOT_INPUT_PRESS - PRESS3D_IN(i,j,3)) .gt. 290. ) THEN

              dlnpdz= (log(BOT_INPUT_PRESS)-log(PRESS3D_IN(i,j,3)) ) / &
                      (BOT_INPUT_HGT-Z3D_IN(i,j,3))

              IF (I .eq. Ilook .and. J .eq. Jlook) then
               write(0,*) 'p diff: ', BOT_INPUT_PRESS, PRESS3D_IN(i,j,3)
               write(0,*) 'z diff: ', BOT_INPUT_HGT, Z3D_IN(i,j,3)
              ENDIF
	
            ELSE

!! Loop up to level 7 looking for a sufficiently thick layer

              FIND_THICK:  DO LL=4,7
               IF( abs(BOT_INPUT_PRESS - PRESS3D_IN(i,j,LL)) .gt. 290.) THEN
                 dlnpdz= (log(BOT_INPUT_PRESS)-log(PRESS3D_IN(i,j,LL)) ) / &
                   (BOT_INPUT_HGT-Z3D_IN(i,j,LL))
                EXIT FIND_THICK
               ENDIF 
              END DO FIND_THICK

            ENDIF
        
          ENDIF

        dum2d(I,J)= exp(log(BOT_INPUT_PRESS) + dlnpdz * &
                        (TERRAIN_HGT_T(I,J) - BOT_INPUT_HGT) )

           DEFINED_PSFC(I,J)=.TRUE.

           IF (I .eq. Ilook .and. J .eq. Jlook) THEN
	   write(0,*) 'here(b) set dum2d to: ', I,J, dum2d(I,J)
           ENDIF

        ELSE ! target level bounded by input levels

          DO L=2,generic-1
            IF (TERRAIN_HGT_T(I,J) .gt. Z3D_IN(i,j,L) .AND. &
                  TERRAIN_HGT_T(I,J) .lt. Z3D_IN(i,j,L+1) ) THEN
               dlnpdz= (log(PRESS3D_IN(i,j,l))-log(PRESS3D_IN(i,j,L+1)) ) / &
                       (Z3D_IN(i,j,l)-Z3D_IN(i,j,L+1))
               dum2d(I,J)= log(PRESS3D_IN(i,j,l)) +   &
                           dlnpdz * (TERRAIN_HGT_T(I,J) - Z3D_IN(i,j,L) )
               dum2d(i,j)=exp(dum2d(i,j))
           DEFINED_PSFC(I,J)=.TRUE.
           IF (I .eq. Ilook .and. J .eq. Jlook) THEN
	     write(0,*) 'here(c) set dum2d to: ', I,J, Dum2d(I,J)
           ENDIF
            ENDIF
          ENDDO

!!! account for situation where BOT_INPUT_HGT < TERRAIN_HGT_T < Z3D_IN(:,2,:)
          IF (dum2d(I,J) .eq. -9 .AND. BOT_INPUT_HGT .lt. TERRAIN_HGT_T(I,J) &
              .AND. TERRAIN_HGT_T(I,J) .lt. Z3D_IN(I,J,2)) then

          ! IF (mod(I,50) .eq. 0 .AND. mod(J,50) .eq. 0) THEN
          !   write(0,*) 'I,J,BOT_INPUT_HGT, bot_pres, TERRAIN_HGT_T: ',  &
          !      I,J,BOT_INPUT_HGT, BOT_INPUT_PRESS, TERRAIN_HGT_T(I,J)
          ! ENDIF

            dlnpdz= (log(PSFC_IN(i,j))-log(PRESS3D_IN(i,j,2)) ) / &
                    (TOPO_IN(i,j)-Z3D_IN(i,j,2))
            dum2d(I,J)= log(PSFC_IN(i,j)) +   &
                        dlnpdz * (TERRAIN_HGT_T(I,J) - TOPO_IN(i,j) )
            dum2d(i,j)= exp(dum2d(i,j))
           DEFINED_PSFC(I,J)=.TRUE.
           IF (I .eq. Ilook .and. J .eq. Jlook) THEN
	     write(0,*) 'here(d) set dum2d to: ', I,J, Dum2d(I,J)
           ENDIF
          ENDIF

          IF (dum2d(I,J) .eq. -9.) THEN
            write(0,*) 'must have flukey situation in new ', I,J
            write(0,*) 'I,J,BOT_INPUT_HGT, bot_pres, TERRAIN_HGT_T: ',  &
                       I,J,BOT_INPUT_HGT, BOT_INPUT_PRESS, TERRAIN_HGT_T(I,J)

            DO L=1,generic-1
              IF ( TERRAIN_HGT_T(I,J) .eq. Z3D_IN(i,j,L) ) THEN
! problematic with HGT_M substitution for "input" surface height?
                dum2d(i,j)=PRESS3D_IN(I,J,L)
                DEFINED_PSFC(I,J)=.TRUE.
           IF (I .eq. Ilook .and. J .eq. Jlook) THEN
	     write(0,*) 'here(e) set dum2d to: ', I,J, Dum2d(I,J)
           ENDIF
              ENDIF
            ENDDO

            IF ( TERRAIN_HGT_T(I,J) .eq. TOPO_IN(I,J)) THEN
              dum2d(I,J)=PSFC_IN(I,J)
              DEFINED_PSFC(I,J)=.TRUE.
           IF (I .eq. Ilook .and. J .eq. Jlook) THEN
	     write(0,*) 'here(f) set dum2d to: ', I,J, Dum2d(I,J)
           ENDIF
         !   write(0,*) 'matched input topo, psfc: ', I,J,TOPO_IN(I,J),PSFC_IN(I,J)
            ENDIF

!            IF (dum2d(I,J) .eq. -9.) THEN
!            ENDIF 

          ENDIF

	if (.not. defined_psfc(i,J)) then
!write(0,*) 'switching to true here'
          DEFINED_PSFC(I,J)=.TRUE.
        endif

	  IF (I .eq. Ilook .AND. J .eq. Jlook) THEN
	    write(0,*) 'newstyle psfc: ', I,J,dum2d(I,J)
          ENDIF

        ENDIF 

	if (.not. DEFINED_PSFC(I,J)) then
!	write(0,*) 'new style undefined at: ', I,J
	endif

        ENDDO I_loop
        ENDDO

       !write(0,*) 'psfc points (new style)'
	loopinc=max( (JmE-1)/20,1)
	iloopinc=max( (ImE-1)/10,1)

        DO J=JmE,1,-loopinc
       !  write(0,633) (dum2d(I,J)/100.,I=1,min(ImE,ImE),iloopinc)
        END DO

  633   format(35(f5.0,1x))

!        write(0,*) 'PSFC extremes (new style): ',  minval(dum2d,MASK=DEFINED_PSFC),maxval(dum2d,MASK=DEFINED_PSFC)

!         IF (minval(dum2d,MASK=DEFINED_PSFC) .lt. 40000. .or. maxval(dum2d,MASK=DEFINED_PSFC) .gt. 110000.) THEN
!        ENDIF

!! "traditional" isobaric only approach ------------------------------------------------

       ALLOCATE (DUM2DB(IME,JME))
       DO J=1,JME
        DO I=1,IME
         DUM2DB(I,J)=-9.
        END DO
       END DO

       DO J=1,JmE
       DO I=1,ImE

        IF (TERRAIN_HGT_T(I,J) .lt. Z3D_IN(i,j,2)) THEN ! targ below lowest

          IF ( abs(PRESS3D_IN(i,j,2)-PRESS3D_IN(i,j,3)) .gt. 290.) THEN
            dlnpdz= (log(PRESS3D_IN(i,j,2))-log(PRESS3D_IN(i,j,3)) ) / &
                    (Z3D_IN(i,j,2)-Z3D_IN(i,j,3))
          ELSE
            dlnpdz= (log(PRESS3D_IN(i,j,2))-log(PRESS3D_IN(i,j,4)) ) / &
                    (Z3D_IN(i,j,2)-Z3D_IN(i,j,4))
          ENDIF

          DUM2DB(I,J)= exp( log(PRESS3D_IN(i,j,2)) + dlnpdz * &
                           (TERRAIN_HGT_T(I,J) - Z3D_IN(i,j,2)) )

	  IF (I .eq. Ilook .and. J .eq. Jlook) THEN
	    write(0,*) 'I,K, trad: dlnpdz, press_in(2), terrain_t, Z3D_IN(2): ', I,J,dlnpdz, &
                             PRESS3D_IN(i,j,2), TERRAIN_HGT_T(I,J), Z3D_IN(i,j,2)
          ENDIF

          DEFINED_PSFCB(i,j)=.true.

        ELSEIF (TERRAIN_HGT_T(I,J) .gt. Z3D_IN(i,j,2)) THEN ! target level bounded by input levels

        DO L=2,generic-1
          IF (TERRAIN_HGT_T(I,J) .gt. Z3D_IN(i,j,L) .AND. &
              TERRAIN_HGT_T(I,J) .lt. Z3D_IN(i,j,L+1) ) THEN

            dlnpdz= (log(PRESS3D_IN(i,j,l))-log(PRESS3D_IN(i,j,L+1)) ) / &
                    (Z3D_IN(i,j,l)-Z3D_IN(i,j,L+1))

            DUM2DB(I,J)= log(PRESS3D_IN(i,j,l)) +   &
                         dlnpdz * (TERRAIN_HGT_T(I,J) - Z3D_IN(i,j,L) )
            DUM2DB(i,j)=exp(DUM2DB(i,j))

           IF (I .eq. Ilook .and. J .eq. Jlook) THEN
	     write(0,*) 'L, L+1, p3d_in(L), p3d_in(L+1), z3d_in(L), z3d_in(L+1): ', L, L+1, PRESS3D_IN(i,j,l), PRESS3D_IN(i,j,L+1), Z3D_IN(i,j,l), Z3D_IN(i,j,L+1)
	     write(0,*) 'TERRAIN_HGT_T(I,J) , Z3D_IN(i,j,L): ', TERRAIN_HGT_T(I,J) , Z3D_IN(i,j,L)
	     write(0,*) 'here(2b) set dum2db to: ', I,J, Dum2db(I,J)
           ENDIF

	    DEFINED_PSFCB(i,j)=.true.

            IF (DUM2DB(I,J) .lt. 13000.) THEN
           !  write(0,*) 'I,J,L,terrain,Z3d(L),z3d(L+1),p3d(L),p3d(l+1): ', I,J,L, &
           !                    TERRAIN_HGT_T(I,J),Z3D_IN(I,J,L),Z3D_IN(I,J,L+1),PRESS3D_IN(I,J,L), &
           !                    PRESS3D_IN(I,J,L+1)
            ENDIF
          ENDIF
        ENDDO

        ELSEIF (TERRAIN_HGT_T(I,J) .eq. Z3D_IN(i,j,2)) THEN
          DUM2DB(i,j)=PRESS3D_IN(I,J,2)
           IF (I .eq. Ilook .and. J .eq. Jlook) THEN
	     write(0,*) 'here(2c) set dum2db to: ', I,J, Dum2db(I,J)
           ENDIF
	  DEFINED_PSFCB(i,j)=.true.
        ENDIF

        IF (DUM2DB(I,J) .eq. -9.) THEN
         !write(0,*) 'must have flukey situation in trad ', I,J
          DO L=1,generic-1
            IF ( TERRAIN_HGT_T(I,J) .eq. Z3D_IN(i,j,L) ) THEN
              DUM2DB(i,j)=PRESS3D_IN(I,J,L)
           IF (I .eq. Ilook .and. J .eq. Jlook) THEN
	     write(0,*) 'here(2d) set dum2db to: ', I,J, Dum2db(I,J)
           ENDIF
              DEFINED_PSFCB(i,j)=.true.
            ENDIF
          ENDDO
        ENDIF

        IF (DUM2DB(I,J) .eq. -9.) THEN
          write(0,*) 'HOPELESS PSFC, I QUIT'
        ENDIF

	if (I .eq. Ilook .and. J .eq. Jlook) THEN
	  write(0,*) ' traditional psfc: ', I,J,DUM2DB(I,J)
        ENDIF

       ENDDO
       ENDDO

!       write(0,*) 'psfc points (traditional)'
!       DO J=JmE,1,-loopinc
!         write(0,633) (DUM2DB(I,J)/100.,I=1,ime,iloopinc)
!       ENDDO

!      write(0,*) 'PSFC extremes (traditional): ', minval(DUM2DB,MASK=DEFINED_PSFCB),maxval(DUM2DB,MASK=DEFINED_PSFCB)

!       IF (minval(DUM2DB,MASK=DEFINED_PSFCB) .lt. 40000. .or. maxval(DUM2DB,MASK=DEFINED_PSFCB) .gt. 108000.) THEN
!       ENDIF

!!!!! end traditional

       DO J=1,JmE
       DO I=1,ImE
         IF (DEFINED_PSFCB(I,J) .and. DEFINED_PSFC(I,J)) THEN

          IF (  abs(dum2d(I,J)-DUM2DB(I,J)) .gt. 400.) THEN
	!    write(0,*) 'BIG DIFF I,J, dum2d, DUM2DB: ', I,J,dum2d(I,J),DUM2DB(I,J)
          ENDIF

!! do we have enough confidence in new style to give it more than 50% weight?
          psfc_out(I,J)=0.5*(dum2d(I,J)+DUM2DB(I,J))
         ELSEIF (DEFINED_PSFC(I,J)) THEN
           psfc_out(I,J)=dum2d(I,J)
         ELSEIF (DEFINED_PSFCB(I,J)) THEN
           psfc_out(I,J)=DUM2DB(I,J)
         ELSE
         ! write(0,*) 'I,J,dum2d,DUM2DB: ', I,J,dum2d(I,J),DUM2DB(I,J)
	 ! write(0,*) 'I,J,DEFINED_PSFC(I,J),DEFINED_PSFCB(I,J): ', I,J,DEFINED_PSFC(I,J),DEFINED_PSFCB(I,J)
         ENDIF

	IF (I .eq. Ilook .AND. J .eq. Jlook) THEN
	  write(0,*) ' combined psfc: ', I,J,psfc_out(I,J)
        ENDIF

	IF (psfc_out(I,J) .lt. 50000. .or. psfc_out(I,J) .gt. 108000.) THEN
	 !write(0,*) 'strange combo on psfc_out, terrain_hgt_t: ', I,J, psfc_out(I,J), terrain_hgt_t(I,J)
	 !write(0,*) 'DEFINED_PSFC, dum2d: ', DEFINED_PSFC(I,J),dum2d(I,J)
	 !write(0,*) 'DEFINED_PSFCB, DUM2DB: ', DEFINED_PSFCB(I,J),DUM2DB(I,J)

!	if (terrain_hgt_t(I,J) .gt. 0 .and. terrain_hgt_t(I,J) .lt. 5000.) then
!        else
!          write(0,*) 'will let strange psfc pass because surface topo is: ', terrain_hgt_t(I,J)
!        endif

	ENDIF

       ENDDO
       ENDDO

      !write(0,*) 'psfc points (final combined)'
       DO J=JmE,1,-loopinc
      !  write(0,633) (psfc_out(I,J)/100.,I=1,ime,iloopinc)
       ENDDO
	
	deallocate(dum2d,dum2db)

	END SUBROUTINE compute_nmm_surfacep


subroutine grads1a(f,nvert,mype,fname)

  use kinds, only: r_kind,i_kind
  use gridmod, only: nlat,nlon,lon2,lat2,rlats,rlons,regional
  use constants, only: rad2deg
  implicit none

  integer(i_kind) nvert,mype
  character(*) fname
  real(r_kind)   f(lat2,lon2,nvert)

  real(r_kind),dimension(nlat,nlon)::work
  real(4) outfield(nlon,nlat)

  character(50) dsname,title,filename
! data dsname/'test.dat'/
  data title/'inmi'/
  character(112) datdes(50000)
  character(1) blank
  data blank/' '/
  data undef/-9.99e33/

  integer(i_kind) i,k,kend,kstart,next,np,ioutdes,ioutdat
  integer(i_kind) last,j,koutmax
  real(4) undef
  real(4) startp,pinc
  real(4) rlons_deg(nlon)
  real(4) rlats_deg(nlat)

  if(mype.eq.0) then
    if(regional) then
       rlons_deg=rlons
       rlats_deg=rlats
    else
       rlons_deg=rad2deg*rlons
       rlats_deg=rad2deg*rlats
    end if
    np=nvert
    startp=1.
    pinc=1.
    ioutdes=98550
    ioutdat=98551
    write(filename,'(a,".des")')trim(fname)
    write(dsname,'(a,".dat")')trim(fname)
    open(unit=ioutdes,file=trim(filename),form='formatted')
    open(unit=ioutdat,file=trim(dsname),form='unformatted')
    rewind ioutdes
    rewind ioutdat
    do i=1,50000
      write(datdes(i),'(112a1)')(blank,k=1,112)
    end do
    write(datdes(1),'("DSET ",a50)')dsname
    write(datdes(2),'("options big_endian sequential")')
    write(datdes(3),'("TITLE ",a50)')title
    write(datdes(4),'("UNDEF ",e11.2)')undef
    next=5
    write(datdes(next),'("XDEF ",i5," LEVELS")')nlon
    kend=0
    do
      kstart=kend+1
      kend=min(kstart+9,nlon)
      if(kstart.gt.nlon) exit
      next=next+1
      write(datdes(next),'(10f11.4)')(rlons_deg(k),k=kstart,kend)
    end do
    next=next+1
    write(datdes(next),'("YDEF ",i5," LEVELS")')nlat
    kend=0
    do
      kstart=kend+1
      kend=min(kstart+9,nlat)
      if(kstart.gt.nlat) exit
      next=next+1
      write(datdes(next),'(10f11.4)')(rlats_deg(k),k=kstart,kend)
    end do
    next=next+1
    write(datdes(next),'("ZDEF ",i5," LINEAR ",f7.2,f7.2)')np,startp,pinc
    next=next+1
    koutmax=1
    write(datdes(next),'("TDEF ",i5," LINEAR 0Z23may1992 24hr")')koutmax
    next=next+1
    write(datdes(next),'("VARS 1")')
    next=next+1
    write(datdes(next),'("f   ",i5," 99 f   ")')nvert
    next=next+1
    write(datdes(next),'("ENDVARS")')
    last=next
    write(ioutdes,'(a112)')(datdes(i),i=1,last)

  end if

  do k=1,nvert
    call sub2grid_1a(f(1,1,k),work,0,mype)
    if(mype.eq.0) then
      do j=1,nlon ; do i=1,nlat
          outfield(j,i)=work(i,j)
      end do ; end do
      write(ioutdat)outfield
    end if
  end do

  if(mype.eq.0) then
    close(ioutdes)
    close(ioutdat)
  end if

end subroutine grads1a

subroutine sub2grid_1a(sub,grid,gridpe,mype)

!     straightforward, but inefficient code to convert a single variable on subdomains to complete
!      slab on one processor.

  use kinds, only: r_kind,i_kind
  use constants, only: zero
  use gridmod, only: nlat,nlon,lat2,lon2,lat1,lon1,&
         ltosi,ltosj,iglobal,ijn,displs_g,itotsub,strip
  use mpimod, only: mpi_comm_world,ierror,mpi_rtype
  implicit none

  integer(i_kind), intent(in)::gridpe,mype
  real(r_kind),dimension(lat2,lon2),intent(in):: sub
  real(r_kind),dimension(nlat,nlon),intent(out)::grid

  real(r_kind),dimension(lat1*lon1):: zsm
  real(r_kind),dimension(itotsub):: work1
  integer(i_kind) mm1,i,j,k

  mm1=mype+1

  do j=1,lon1*lat1
    zsm(j)=zero
  end do
  call strip(sub,zsm,1)
  call mpi_gatherv(zsm,ijn(mm1),mpi_rtype, &
                 work1,ijn,displs_g,mpi_rtype, &
                 gridpe,mpi_comm_world,ierror)
  if(mype.eq.gridpe) then
    do k=1,iglobal
      i=ltosi(k) ; j=ltosj(k)
      grid(i,j)=work1(k)
    end do
  end if

end subroutine sub2grid_1a
