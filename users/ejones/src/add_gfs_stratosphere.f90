subroutine add_gfs_stratosphere
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    add_gfs_stratosphere
!   prgmmr: parrish          org: np22                date: 2012-02-18
!
! abstract: This was created from a copy of get_gefs_for_regional.f90.  This subroutine
!             reads in the gfs guess, interpolates in horizontal to nmmb analysis grid,
!             then interpolates in the vertical to the new mixed nmmb/gfs vertical coordinate,
!             blending with nmmb fields in the blend zone pblend0 > p > pblend1.
!             Before the blending of gfs and nmmb can take place, the nmmb fields, which have
!             just been read in to module guess_grids, must be interpolated in the vertical from
!             the original nmmb vertical coordinate to the extended mixed nmmb/gfs vertical coordinate.
!
!
! program history log:
!   2012-02-18  parrish, initial documentation
!   2012-10-11  eliu - add FGAT capability for wrf_nmm_regional (HWRF) 
!   2013-10-19  todling - metguess now holds background
!   2014-08-18  tong    - modified to allow gfs/gdas spectral coefficients to be
!                         transformed to a coarser resolution grid
!   2014-12-03  derber  - modify call to general_read_gfsatm to reduce reading
!                         of unused variables
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

  use gridmod, only: idsl5,regional,wrf_nmm_regional 
  use gridmod, only: region_lat,region_lon,eta1_ll,eta2_ll,aeta1_ll,aeta2_ll,pdtop_ll,pt_ll  
  use gridmod, only: nlon,nlat,lat2,lon2,nsig,rotate_wind_ll2xy
  use gridmod, only: use_gfs_ozone,jcap_gfs,nlat_gfs,nlon_gfs
  use constants,only: zero,one_tenth,half,one,ten,fv
  use mpimod, only: mype
  use mpimod, only: mpi_comm_world
  use kinds, only: r_kind,i_kind
  use mpeu_util, only: die
  use gsi_bundlemod, only : gsi_bundlegetpointer
  use gsi_metguess_mod, only: gsi_metguess_bundle
  use general_sub2grid_mod, only: sub2grid_info,general_sub2grid_create_info
  use general_sub2grid_mod, only: general_grid2sub,general_sub2grid
  use general_specmod, only: spec_vars,general_init_spec_vars
  use egrid2agrid_mod, only: g_create_egrid2points_slow,egrid2agrid_parm,g_egrid2points_faster
  use sigio_module, only: sigio_intkind,sigio_head,sigio_srhead
  use guess_grids, only: ntguessig,nfldsig,ifilesig 
  use guess_grids, only: ges_tsen
  use aniso_ens_util, only: intp_spl
  use obsmod, only: iadate
  use gfs_stratosphere, only: nsigg,nsig_save,ak5,bk5,aeta1_save,aeta2_save,eta1_save,eta2_save
  use gfs_stratosphere, only: blend_rm,blend_gm
  use gfs_stratosphere, only: ges_tv_r,ges_q_r,ges_u_r,ges_v_r,ges_tsen_r,ges_oz_r
  use gfs_stratosphere, only: ges_tv_r_g,ges_q_r_g,ges_u_r_g,ges_v_r_g,ges_tsen_r_g,ges_oz_r_g
  use gfs_stratosphere, only: good_o3mr
  implicit none

  type(sub2grid_info) grd_gfs,grd_mix
  type(spec_vars) sp_gfs,sp_b
  real(r_kind),allocatable,dimension(:,:,:) :: pri_g,pri_r,vor,div,u,v,tv,q,cwmr,oz,prsl_g,prsl_r,prsl_m
  real(r_kind),allocatable,dimension(:,:)   :: z,ps
  real(r_kind),allocatable :: work_sub(:,:,:,:),work(:,:,:,:),work_reg(:,:,:,:)
  real(r_kind),allocatable,dimension(:,:,:)::ut,vt,tt,qt,ozt,ttsen

  character(len=*),parameter::myname='add_gfs_stratosphere'
  integer(i_kind) it_beg,it_end 
  integer(i_kind) iret,i,j,k,k2,mm1
  integer(i_kind) ku,kv,kt,kq,koz,kcw,kz,kps
  character(255) filename
  character(255),allocatable, dimension(:)::infiles  
  integer(sigio_intkind):: lunges = 11
  type(sigio_head):: sighead
  type(egrid2agrid_parm) :: p_g2r
  integer(i_kind) inner_vars,num_fields,nsig_gfs,jcap_gfs_test
  integer(i_kind) nord_g2r,jcap_org,nlon_b
  logical,allocatable :: vector(:)
  logical hires
  real(r_kind),allocatable,dimension(:) :: xspli_r,yspliu_r,yspliv_r,xsplo,xsplo_r,ysplou_r,ysplov_r
  real(r_kind),allocatable,dimension(:) :: xspli_g,yspliu_g,yspliv_g,ysplou_g,ysplov_g
  integer(i_kind) iyr,ihourg
  integer(i_kind),dimension(4):: idate4
  integer(i_kind),dimension(8) :: ida,jda 
  integer(i_kind),dimension(5) :: iadate_gfs
  real(r_kind) hourg
  real(r_kind),dimension(5):: fha
  real(r_kind),allocatable,dimension(:):: blend_rm_oz,blend_gm_oz

  real(r_kind) dlon,dlat,uob,vob
  integer(i_kind) ii,jj,it,ier,istatus
 
  real(r_kind),dimension(:,:  ),pointer:: ges_ps =>NULL()
  real(r_kind),dimension(:,:,:),pointer:: ges_u  =>NULL()
  real(r_kind),dimension(:,:,:),pointer:: ges_v  =>NULL()
  real(r_kind),dimension(:,:,:),pointer:: ges_tv =>NULL()
  real(r_kind),dimension(:,:,:),pointer:: ges_q  =>NULL()
  real(r_kind),dimension(:,:,:),pointer:: ges_oz =>NULL()

!    allocate space for saving original regional model guess and original blended regional-global guess:

      allocate(ges_tv_r_g(lat2,lon2,nsig,nfldsig))
      allocate(ges_q_r_g (lat2,lon2,nsig,nfldsig))
      allocate(ges_u_r_g (lat2,lon2,nsig,nfldsig))
      allocate(ges_v_r_g (lat2,lon2,nsig,nfldsig))
      allocate(ges_tsen_r_g (lat2,lon2,nsig,nfldsig))
      allocate(ges_oz_r_g(lat2,lon2,nsig,nfldsig))
      allocate(ges_tv_r  (lat2,lon2,nsig_save,nfldsig))
      allocate(ges_q_r   (lat2,lon2,nsig_save,nfldsig))
      allocate(ges_u_r   (lat2,lon2,nsig_save,nfldsig))
      allocate(ges_v_r   (lat2,lon2,nsig_save,nfldsig))
      allocate(ges_tsen_r   (lat2,lon2,nsig_save,nfldsig))
      allocate(ges_oz_r  (lat2,lon2,nsig_save,nfldsig))

!  first, save current contents of ges_tv, etc  (later, consider how to save only from bottom of blend zone
!                                                  to regional model top, for computational savings)
  do it=1,nfldsig
     ier=0
     call gsi_bundlegetpointer(gsi_metguess_bundle(it),'u'  ,ges_u ,istatus) 
     ier=ier+istatus
     call gsi_bundlegetpointer(gsi_metguess_bundle(it),'v'  ,ges_v ,istatus) 
     ier=ier+istatus
     call gsi_bundlegetpointer(gsi_metguess_bundle(it),'tv' ,ges_tv,istatus) 
     ier=ier+istatus
     call gsi_bundlegetpointer(gsi_metguess_bundle(it),'q'  ,ges_q ,istatus) 
     ier=ier+istatus
     call gsi_bundlegetpointer(gsi_metguess_bundle(it),'oz' ,ges_oz,istatus) 
     ier=ier+istatus
     if(ier/=0) call die(myname,': missing guess vars, aborting ...',ier)
     do k=1,nsig_save
        do j=1,lon2
           do i=1,lat2
              ges_tv_r(i,j,k,it)=ges_tv(i,j,k)
              ges_q_r (i,j,k,it)=ges_q (i,j,k)
              ges_u_r (i,j,k,it)=ges_u (i,j,k)
              ges_v_r (i,j,k,it)=ges_v (i,j,k)
              ges_tsen_r(i,j,k,it)=ges_tsen(i,j,k,it)
              ges_oz_r(i,j,k,it)=ges_oz(i,j,k)
           end do
        end do
     end do
  end do

!     figure out what are acceptable dimensions for global grid, based on resolution of input spectral coefs
!   need to inquire from file what is spectral truncation, then setup general spectral structure variable

! Determine input GFS filenames
  it_beg=1
  it_end=nfldsig
  allocate(infiles(nfldsig))
  do it=it_beg,it_end
     write(filename,'("gfs_sigf",i2.2)')ifilesig(it)
     infiles(it)=filename
     if(mype==0) then
        write(6,*) 'add_gfs_stratosphere: gfs file required: nfldsig     = ',nfldsig              
        write(6,*) 'add_gfs_stratosphere: gfs file required: ifilesig(it)= ',ifilesig(it)
        write(6,*) 'add_gfs_stratosphere: gfs file required: infiles(it) = ',trim(infiles(it))                      
        write(6,*) 'add_gfs_stratosphere: gfs file required: ntguessig   = ',ntguessig                      
     endif
  enddo

! Loop through input GFS files
  it_loop: do it = it_beg,it_end  

  ier=0
  call gsi_bundlegetpointer(gsi_metguess_bundle(it),'ps' ,ges_ps,istatus)
  ier=ier+istatus
  call gsi_bundlegetpointer(gsi_metguess_bundle(it),'u'  ,ges_u ,istatus) 
  ier=ier+istatus
  call gsi_bundlegetpointer(gsi_metguess_bundle(it),'v'  ,ges_v ,istatus) 
  ier=ier+istatus
  call gsi_bundlegetpointer(gsi_metguess_bundle(it),'tv' ,ges_tv,istatus) 
  ier=ier+istatus
  call gsi_bundlegetpointer(gsi_metguess_bundle(it),'q'  ,ges_q ,istatus) 
  ier=ier+istatus
  call gsi_bundlegetpointer(gsi_metguess_bundle(it),'oz' ,ges_oz,istatus) 
  ier=ier+istatus
  if(ier/=0) call die(myname,': missing guess vars, aborting ...',ier)

  filename=infiles(it)    
  if (mype==0) write(6,*)'add_gfs_stratosphere: reading in gfs file: ',trim(filename)                       
  open(lunges,file=trim(filename),form='unformatted')
  call sigio_srhead(lunges,sighead,iret)
  close(lunges)
  if(mype == 0) then
     write(6,*) ' sighead%fhour,sighead%idate=',sighead%fhour,sighead%idate
     write(6,*) ' iadate(y,m,d,hr,min)=',iadate
     write(6,*) ' sighead%latf,sighead%lonf=',sighead%latf,sighead%lonf
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
     write(6,*)' in add_gfs_stratosphere, iadate_gefs=',iadate_gfs
     write(6,*)' in add_gfs_stratosphere, iadate    =',iadate
  end if
  if (.not. wrf_nmm_regional) then
  if(iadate_gfs(1)/=iadate(1).or.iadate_gfs(2)/=iadate(2).or.iadate_gfs(3)/=iadate(3).or.&
                                 iadate_gfs(4)/=iadate(4).or.iadate_gfs(5)/=iadate(5) ) then
     if(mype == 0) write(6,*)' IN ADD_GFS_STRATOSPHERE, GFS DATE NOT EQUAL TO ANALYSIS DATE, PROGRAM STOPS'
     call stop2(85)
  end if
  endif

  inner_vars=1
  jcap_org=sighead%jcap
  nsig_gfs=nsigg
  num_fields=6*nsig_gfs+2      !  want to transfer u,v,t,q,oz,cw,ps,z from gfs subdomain to slab
                            !  later go through this code, adapting gsibundlemod, since currently 
                            !   hardwired.

  nlon_b=((2*jcap_org+1)/nlon_gfs+1)*nlon_gfs
  if (nlon_b > nlon_gfs) then
     hires=.true.
  else
     hires=.false.
     jcap_gfs=sighead%jcap
     nlat_gfs=sighead%latf+2
     nlon_gfs=sighead%lonf
  end if 

  if(mype==0) write(6,*)' in add_gfs_stratosphere before general_sub2grid_create_info'                                                
  if(mype==0) write(6,*)' in add_gfs_stratosphere: num_fields = ', num_fields  
  if(mype==0) write(6,*)' in add_gfs_stratosphere: jcap_org, jcap_gfs= ', &
              jcap_org, jcap_gfs
  if(mype==0) write(6,*)' in add_gfs_stratosphere: nlon_b, nlon_gfs, hires=', &
                          nlon_b, nlon_gfs, hires

  allocate(vector(num_fields))
  vector=.false.
  vector(1:2*nsig_gfs)=.true.
  call general_sub2grid_create_info(grd_gfs,inner_vars,nlat_gfs,nlon_gfs,nsig_gfs,num_fields, &
                                  .not.regional,vector)
  jcap_gfs_test=jcap_gfs
  call general_init_spec_vars(sp_gfs,jcap_gfs,jcap_gfs_test,grd_gfs%nlat,grd_gfs%nlon)
  if (hires) then
      call general_init_spec_vars(sp_b,jcap_org,jcap_org,nlat_gfs,nlon_b)
  end if

!  also want to set up regional grid structure variable grd_mix, which still has number of
!   vertical levels set to nsig_gfs, but horizontal dimensions set to regional domain.

  call general_sub2grid_create_info(grd_mix,inner_vars,nlat,nlon,nsig_gfs, &
                                    num_fields,regional,vector)

!  create interpolation information for global grid to regional ensemble grid

  nord_g2r=4
  call g_create_egrid2points_slow(nlat*nlon,region_lat,region_lon, &
                    grd_gfs%nlat,sp_gfs%rlats,grd_gfs%nlon,sp_gfs%rlons,nord_g2r,p_g2r)

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
     if (hires) then
        call general_read_gfsatm(grd_gfs,sp_gfs,sp_b,filename,mype,.true.,.false.,.true., &
                                 z,ps,vor,div,u,v,tv,q,cwmr,oz,iret)
     else
        call general_read_gfsatm(grd_gfs,sp_gfs,sp_gfs,filename,mype,.true.,.false.,.true., &
                                 z,ps,vor,div,u,v,tv,q,cwmr,oz,iret)
     end if
        
!test
!     call grads3a(grd_gfs,u,oz,tv,q,ps,grd_gfs%nsig,mype,'gfsfields')

     deallocate(vor,div)
     allocate(work_sub(grd_gfs%inner_vars,grd_gfs%lat2,grd_gfs%lon2,num_fields))
     do k=1,grd_gfs%nsig
        ku=k ; kv=k+grd_gfs%nsig ; kt=k+2*grd_gfs%nsig ; kq=k+3*grd_gfs%nsig ; koz=k+4*grd_gfs%nsig
        kcw=k+5*grd_gfs%nsig
        do j=1,grd_gfs%lon2
           do i=1,grd_gfs%lat2
              work_sub(1,i,j,ku)=u(i,j,k)
              work_sub(1,i,j,kv)=v(i,j,k)
              work_sub(1,i,j,kt)=tv(i,j,k)
              work_sub(1,i,j,kq)=q(i,j,k)
              work_sub(1,i,j,koz)=oz(i,j,k)
              work_sub(1,i,j,kcw)=cwmr(i,j,k)
           end do
        end do
     end do
     deallocate(u,v,tv,q,oz,cwmr)
     kz=num_fields ; kps=kz-1
     do j=1,grd_gfs%lon2
        do i=1,grd_gfs%lat2
           work_sub(1,i,j,kz)=z(i,j)
           work_sub(1,i,j,kps)=ps(i,j)
        end do
     end do
     deallocate(z,ps)
     allocate(work(grd_gfs%inner_vars,grd_gfs%nlat,grd_gfs%nlon,grd_gfs%kbegin_loc:grd_gfs%kend_alloc))
     call general_sub2grid(grd_gfs,work_sub,work)
     deallocate(work_sub)

!    then interpolate to regional analysis grid
     allocate(work_reg(grd_mix%inner_vars,grd_mix%nlat,grd_mix%nlon,grd_gfs%kbegin_loc:grd_gfs%kend_alloc))
     do k=grd_gfs%kbegin_loc,grd_gfs%kend_loc
        call g_egrid2points_faster(p_g2r,work(1,1,1,k),work_reg(1,1,1,k),vector(k))
     end do
     deallocate(work)

!    next general_grid2sub to go to regional grid subdomains.
     allocate(work_sub(grd_mix%inner_vars,grd_mix%lat2,grd_mix%lon2,num_fields))
     call general_grid2sub(grd_mix,work_reg,work_sub)
     deallocate(work_reg)
     allocate(pri_g(grd_mix%lat2,grd_mix%lon2,grd_mix%nsig+1))
     kz=num_fields ; kps=kz-1

!  IN FOLLOWING SECTION GET 3D PRESSURE FOR GFS.

!    compute 3d pressure on interfaces
     pri_g=zero
     k=1
     k2=nsigg+1
!                       FOR NOW, ONLY CONSIDER pressure defined by ak5,bk5
     do j=1,lon2
        do i=1,lat2
           pri_g(i,j,k)=work_sub(1,i,j,kps)
           pri_g(i,j,k2)=zero
        end do
     end do
     do k=2,nsigg
        do j=1,lon2
           do i=1,lat2
              pri_g(i,j,k)=one_tenth*ak5(k)+bk5(k)*work_sub(1,i,j,kps)
     if(mype==0.and.i==10.and.j==10) &
           write(6,'(" k, pri_g,ak5,bk5,ps=",i3,4f18.3)') k,pri_g(i,j,k),ak5(k),bk5(k),work_sub(1,i,j,kps)
           end do
        end do
     end do

!    Get 3d pressure field now on layers
!       FOR NOW, ASSUME IDSL==2, so layer value is just average of interface values (also apparently the
!                                                                          definition used by nmmb)

     allocate(prsl_g(lat2,lon2,nsigg))
     do k=1,nsigg
        do j=1,lon2
           do i=1,lat2
              prsl_g(i,j,k)=(pri_g(i,j,k)+pri_g(i,j,k+1))*half
     if(mype==0.and.i==10.and.j==10) &
           write(6,'(" k, prsl_g=",i3,f18.3)') k,prsl_g(i,j,k)
           end do
        end do
     end do
     deallocate(pri_g)

!  IN THIS SECTION GET 3D PRESSURE FOR INPUT NMMB.

     allocate(pri_r(lat2,lon2,nsig_save+1))
     do k=1,nsig_save+1
        do j=1,lon2
           do i=1,lat2
              pri_r(i,j,k)=one_tenth*(eta1_save(k)*pdtop_ll + &
                              eta2_save(k)*(ten*ges_ps(i,j)-pdtop_ll-pt_ll) + & !                         
                              pt_ll)
           end do
        end do
     end do
     deallocate(pri_r)
     allocate(prsl_r(lat2,lon2,nsig_save))
     do k=1,nsig_save
        do j=1,lon2
           do i=1,lat2
              prsl_r(i,j,k)=one_tenth*(aeta1_save(k)*pdtop_ll + &
                              aeta2_save(k)*(ten*ges_ps(i,j)-pdtop_ll-pt_ll) + & 
                              pt_ll)
           end do
        end do
     end do

!  IF THERE IS NO REGIONAL GES OZONE (good_o3mr=F), THEN INTERPOLATE GLOBAL OZONE TO NMMB VERT COORD:

   if(.not.good_o3mr) then                            
     allocate(xsplo_r(nsig_save),ysplou_r(nsig_save))
     allocate(xspli_g(nsigg),yspliu_g(nsigg))
     do j=1,lon2
        do i=1,lat2
           do k=1,nsigg
              xspli_g(k)=log(prsl_g(i,j,k)*ten)
           end do
           do k=1,nsig_save
              xsplo_r(k)=log(prsl_r(i,j,k)*ten)
           end do

!   global ozone:
           do k=1,nsigg
              koz=k+4*grd_gfs%nsig
              yspliu_g(k)=work_sub(1,i,j,koz)
           end do
           call intp_spl(xspli_g,yspliu_g,xsplo_r,ysplou_r,nsigg,nsig_save)
!               following is to correct for bug in intp_spl
           do k=1,nsig_save
              if(xsplo_r(k) < xspli_g(nsigg)) ysplou_r(k)=yspliu_g(nsigg)
              if(xsplo_r(k) > xspli_g(1)) ysplou_r(k)=yspliu_g(1)
           end do
           do k=1,nsig_save
              ges_oz_r(i,j,k,it)=ysplou_r(k)         
           end do
        end do
     end do
     deallocate(xsplo_r,ysplou_r,xspli_g,yspliu_g)
  end if

!  IN THIS SECTION GET 3D PRESSURE FOR MERGED (TARGET) PRESSURE

! allocate(pri_m(lat2,lon2,nsig+1))
! do k=1,nsig+1
!    do j=1,lon2
!       do i=1,lat2
!          pri_m(i,j,k)=one_tenth*(eta1_ll(k)*pdtop_ll + &
!                          eta2_ll(k)*(ten*ges_ps(i,j)-pdtop_ll-pt_ll) + &        
!                          pt_ll)
!       end do
!    end do
! end do
  allocate(prsl_m(lat2,lon2,nsig))
  do k=1,nsig
     do j=1,lon2
        do i=1,lat2
           prsl_m(i,j,k)=one_tenth*(aeta1_ll(k)*pdtop_ll + &
                           aeta2_ll(k)*(ten*ges_ps(i,j)-pdtop_ll-pt_ll) + &       
                           pt_ll)
        end do
     end do
  end do

  allocate(xspli_r(nsig_save),yspliu_r(nsig_save),yspliv_r(nsig_save),xsplo(nsig))
  allocate(ysplou_r(nsig),ysplov_r(nsig),ysplou_g(nsig),ysplov_g(nsig))
  allocate(xspli_g(nsigg),yspliu_g(nsigg),yspliv_g(nsigg))

  allocate(ut(lat2,lon2,nsig))
  allocate(vt(lat2,lon2,nsig))
  allocate(tt(lat2,lon2,nsig))
  allocate(ttsen(lat2,lon2,nsig))
  allocate(qt(lat2,lon2,nsig))
  allocate(ozt(lat2,lon2,nsig))
  mm1=mype+1
  allocate(blend_rm_oz(nsig))
  allocate(blend_gm_oz(nsig))
  blend_rm_oz=zero
  blend_gm_oz=zero
  if(use_gfs_ozone) then
     if(good_o3mr) then
        blend_rm_oz=blend_rm
        blend_gm_oz=blend_gm
     else
        blend_rm_oz=zero
        blend_gm_oz=one
     end if
  end if

  do j=1,lon2
     do i=1,lat2
        ii=i+grd_mix%istart(mm1)-2
        jj=j+grd_mix%jstart(mm1)-2
        ii=min(grd_mix%nlat,max(1,ii))
        jj=min(grd_mix%nlon,max(1,jj))
        dlon=float(jj)
        dlat=float(ii)
        do k=1,nsig_save
           xspli_r(k)=log(prsl_r(i,j,k)*ten)
        end do
        do k=1,nsigg
           xspli_g(k)=log(prsl_g(i,j,k)*ten)
        end do
        do k=1,nsig
           xsplo(k)=log(prsl_m(i,j,k)*ten)
        end do

!    u,v -- regional contribution
        do k=1,nsig_save
           yspliu_r(k)=ges_u(i,j,k) 
           yspliv_r(k)=ges_v(i,j,k)  
        end do
        call intp_spl(xspli_r,yspliu_r,xsplo,ysplou_r,nsig_save,nsig)
        call intp_spl(xspli_r,yspliv_r,xsplo,ysplov_r,nsig_save,nsig)
!               following is to correct for bug in intp_spl
        do k=1,nsig
           if(xsplo(k) < xspli_r(nsig_save)) ysplou_r(k)=yspliu_r(nsig_save)
           if(xsplo(k) > xspli_r(1)) ysplou_r(k)=yspliu_r(1)
           if(xsplo(k) < xspli_r(nsig_save)) ysplov_r(k)=yspliv_r(nsig_save)
           if(xsplo(k) > xspli_r(1)) ysplov_r(k)=yspliv_r(1)
        end do
!    u,v -- global contribution
        do k=1,nsigg
           ku=k ; kv=k+grd_gfs%nsig
           yspliu_g(k)=work_sub(1,i,j,ku)
           yspliv_g(k)=work_sub(1,i,j,kv)
        end do
        call intp_spl(xspli_g,yspliu_g,xsplo,ysplou_g,nsigg,nsig)
        call intp_spl(xspli_g,yspliv_g,xsplo,ysplov_g,nsigg,nsig)
!               following is to correct for bug in intp_spl
        do k=1,nsig
           if(xsplo(k) < xspli_g(nsigg)) ysplou_g(k)=yspliu_g(nsigg)
           if(xsplo(k) > xspli_g(1)) ysplou_g(k)=yspliu_g(1)
           if(xsplo(k) < xspli_g(nsigg)) ysplov_g(k)=yspliv_g(nsigg)
           if(xsplo(k) > xspli_g(1)) ysplov_g(k)=yspliv_g(1)
        end do
!                blend contributions from regional and global:
        do k=1,nsig
!            rotate gfs wind to nmmb coordinate:
           call rotate_wind_ll2xy(ysplou_g(k),ysplov_g(k), &
                                  uob,vob,region_lon(ii,jj),dlon,dlat)
           ut(i,j,k)=blend_rm(k)*ysplou_r(k)+blend_gm(k)*uob
           vt(i,j,k)=blend_rm(k)*ysplov_r(k)+blend_gm(k)*vob
        end do
!    t   -- regional contribution
        do k=1,nsig_save
           yspliu_r(k)=ges_tv(i,j,k) 
        end do
        call intp_spl(xspli_r,yspliu_r,xsplo,ysplou_r,nsig_save,nsig) ! try replacing this with
                                                                   !  linear interpolation and compare result
     !  do k=1,nsig
     !     pthis=xsplo(k)
     !     do kk=1,nsig_save-1
     !        if(pthis < xspli_r(kk) .and. pthis >= xspli_r(kk+1)) then
     !           delta=(yspliu_r(kk+1)-yspliu_r(kk))/(xspli_r(kk+1)-xspli_r(kk))
     !           ysplou_r(k)=yspliu_r(kk)+delta*(pthis-xspli_r(kk))
     !           exit
     !        end if
     !     end do
     !  end do
!               following is to correct for bug in intp_spl
           do k=1,nsig
              if(xsplo(k) < xspli_r(nsig_save)) ysplou_r(k)=yspliu_r(nsig_save)
              if(xsplo(k) > xspli_r(1)) ysplou_r(k)=yspliu_r(1)
           end do
!    t   -- global contribution
        do k=1,nsigg
           kt=k+2*grd_gfs%nsig
           yspliu_g(k)=work_sub(1,i,j,kt)
        end do
        call intp_spl(xspli_g,yspliu_g,xsplo,ysplou_g,nsigg,nsig)
         
!               following is to correct for bug in intp_spl
        do k=1,nsig
           if(xsplo(k) < xspli_g(nsigg)) ysplou_g(k)=yspliu_g(nsigg)
           if(xsplo(k) > xspli_g(1)) ysplou_g(k)=yspliu_g(1)
        end do
!                blend contributions from regional and global:
        do k=1,nsig
           tt(i,j,k)=blend_rm(k)*ysplou_r(k)+blend_gm(k)*ysplou_g(k)
        end do
!    q   -- regional contribution
        do k=1,nsig_save
           yspliu_r(k)=ges_q(i,j,k)
        end do
        call intp_spl(xspli_r,yspliu_r,xsplo,ysplou_r,nsig_save,nsig)
!               following is to correct for bug in intp_spl
        do k=1,nsig
           if(xsplo(k) < xspli_r(nsig_save)) ysplou_r(k)=yspliu_r(nsig_save)
           if(xsplo(k) > xspli_r(1)) ysplou_r(k)=yspliu_r(1)
        end do
!    q   -- global contribution
        do k=1,nsigg
           kq=k+3*grd_gfs%nsig
           yspliu_g(k)=work_sub(1,i,j,kq)
        end do
        call intp_spl(xspli_g,yspliu_g,xsplo,ysplou_g,nsigg,nsig)
!               following is to correct for bug in intp_spl
        do k=1,nsig
           if(xsplo(k) < xspli_g(nsigg)) ysplou_g(k)=yspliu_g(nsigg)
           if(xsplo(k) > xspli_g(1)) ysplou_g(k)=yspliu_g(1)
        end do
!                blend contributions from regional and global:
        do k=1,nsig
           qt(i,j,k)=blend_rm(k)*ysplou_r(k)+blend_gm(k)*ysplou_g(k)
           ttsen(i,j,k)=tt(i,j,k)/(one+fv*qt(i,j,k))
        end do
!   oz   -- regional contribution
        do k=1,nsig_save
           yspliu_r(k)=ges_oz(i,j,k) 
        end do
        call intp_spl(xspli_r,yspliu_r,xsplo,ysplou_r,nsig_save,nsig)
!               following is to correct for bug in intp_spl
        do k=1,nsig
           if(xsplo(k) < xspli_r(nsig_save)) ysplou_r(k)=yspliu_r(nsig_save)
           if(xsplo(k) > xspli_r(1)) ysplou_r(k)=yspliu_r(1)
        end do
!   oz   -- global contribution
        do k=1,nsigg
           koz=k+4*grd_gfs%nsig
           yspliu_g(k)=work_sub(1,i,j,koz)
        end do
        call intp_spl(xspli_g,yspliu_g,xsplo,ysplou_g,nsigg,nsig)
!               following is to correct for bug in intp_spl
        do k=1,nsig
           if(xsplo(k) < xspli_g(nsigg)) ysplou_g(k)=yspliu_g(nsigg)
           if(xsplo(k) > xspli_g(1)) ysplou_g(k)=yspliu_g(1)
        end do
!                blend contributions from regional and global:
        do k=1,nsig
           ozt(i,j,k)=blend_rm_oz(k)*ysplou_r(k)+blend_gm_oz(k)*ysplou_g(k)
        end do

     end do
  end do
  deallocate(blend_rm_oz,blend_gm_oz)
                                                   !  call grads1a(ut,nsig,mype,'u')
                                                   !  call grads1a(vt,nsig,mype,'v')
                                                   !  call grads1a(tt,nsig,mype,'t')
                                                   !  call grads1a(ttsen,nsig,mype,'tsen')
                                                   !  call grads1a(qt,nsig,mype,'q')
                                                   !  call grads1a(ozt,nsig,mype,'oz')
  do k=1,nsig
     do j=1,lon2
        do i=1,lat2
           ges_tv(i,j,k)=tt(i,j,k)      
           ges_tsen(i,j,k,it)=ttsen(i,j,k)
           ges_u(i,j,k)=ut(i,j,k)       
           ges_v(i,j,k)=vt(i,j,k)       
           ges_q(i,j,k)=qt(i,j,k)       
           ges_oz(i,j,k)=ozt(i,j,k)    
        end do
     end do
  end do
!   save blended fields for use at end of analysis
  do k=1,nsig
     do j=1,lon2
        do i=1,lat2
           ges_tv_r_g(i,j,k,it)=ges_tv(i,j,k)     
           ges_tsen_r_g(i,j,k,it)=ges_tsen(i,j,k,it) 
           ges_u_r_g(i,j,k,it)=ges_u(i,j,k)       
           ges_v_r_g(i,j,k,it)=ges_v(i,j,k)       
           ges_q_r_g(i,j,k,it)=ges_q(i,j,k)      
           ges_oz_r_g(i,j,k,it)=ges_oz(i,j,k)    
        end do
     end do
  end do

  deallocate(ut,vt,tt,ttsen,qt,ozt)
  deallocate(xspli_r,yspliu_r,yspliv_r,xsplo)
  deallocate(ysplou_r,ysplov_r,ysplou_g,ysplov_g)
  deallocate(xspli_g,yspliu_g,yspliv_g)
  deallocate(prsl_m,prsl_r,prsl_g,work_sub)
! deallocate(pri_m)  
  deallocate(vector) 

  enddo it_loop       
  deallocate(infiles) 

end subroutine add_gfs_stratosphere

subroutine revert_to_nmmb
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    revert_to_nmmb
!   prgmmr: parrish          org: np22                date: 2012-02-18
!
! abstract: Convert variables from mixed nmmb-gfs variables back to original nmmb in vertical.
!             To do this, subtract mixed nmmb-gfs guess to get analysis increment in mixed nmmb-gfs
!             vertical coordinate.  Then interpolate to original nmmb coordinate.  Finally add original
!             nmmb coordinate guess on to end up with nmmb analysis.
!
! program history log:
!   2012-09-06  parrish, initial documentation
!   2013-10-19  todling - metguess now holds background
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

  use gridmod, only: aeta1_ll,aeta2_ll,pdtop_ll,pt_ll
  use gridmod, only: lat2,lon2,nsig
  use constants,only: zero,one_tenth,one,ten,fv
  use mpimod, only: mype
             use mpimod, only: mpi_comm_world
  use kinds, only: r_kind,i_kind
  use guess_grids, only: ntguessig,nfldsig
  use guess_grids, only: ges_tsen
  use aniso_ens_util, only: intp_spl
  use obsmod, only: iadate
  use gfs_stratosphere, only: nsigg,nsig_save,ak5,bk5,aeta1_save,aeta2_save,eta1_save,eta2_save
  use gfs_stratosphere, only: blend_rm,blend_gm
  use gfs_stratosphere, only: ges_tv_r,ges_q_r,ges_u_r,ges_v_r,ges_tsen_r,ges_oz_r
  use gfs_stratosphere, only: ges_tv_r_g,ges_q_r_g,ges_u_r_g,ges_v_r_g,ges_tsen_r_g,ges_oz_r_g
  use gsi_bundlemod, only : gsi_bundlegetpointer
  use gsi_metguess_mod, only: gsi_metguess_bundle
  use mpeu_util, only: die
  implicit none

  character(len=*),parameter::myname='revert_to_nmmb'
  integer(i_kind) i,j,k,num_i,num_o,ier,istatus
  real(r_kind) xspli(nsig),xsplo(nsig_save)
  real(r_kind) yspli(nsig),ysplo(nsig_save)
  real(r_kind) prsl_r(lat2,lon2,nsig_save),prsl_m(lat2,lon2,nsig)

  real(r_kind),dimension(:,:  ),pointer:: ges_ps =>NULL()
  real(r_kind),dimension(:,:,:),pointer:: ges_u  =>NULL()
  real(r_kind),dimension(:,:,:),pointer:: ges_v  =>NULL()
  real(r_kind),dimension(:,:,:),pointer:: ges_tv =>NULL()
  real(r_kind),dimension(:,:,:),pointer:: ges_q  =>NULL()
  real(r_kind),dimension(:,:,:),pointer:: ges_oz =>NULL()

!  GET 3D PRESSURE FOR ORIGINAL NMMB COORDINATE:

  ier=0
  call gsi_bundlegetpointer(gsi_metguess_bundle(ntguessig),'ps' ,ges_ps,istatus)
  ier=ier+istatus
  call gsi_bundlegetpointer(gsi_metguess_bundle(ntguessig),'u'  ,ges_u ,istatus) 
  ier=ier+istatus
  call gsi_bundlegetpointer(gsi_metguess_bundle(ntguessig),'v'  ,ges_v ,istatus) 
  ier=ier+istatus
  call gsi_bundlegetpointer(gsi_metguess_bundle(ntguessig),'tv' ,ges_tv,istatus) 
  ier=ier+istatus
  call gsi_bundlegetpointer(gsi_metguess_bundle(ntguessig),'q'  ,ges_q ,istatus) 
  ier=ier+istatus
  call gsi_bundlegetpointer(gsi_metguess_bundle(ntguessig),'oz' ,ges_oz,istatus) 
  ier=ier+istatus
  if(ier/=0) call die(myname,': missing guess vars, aborting ...',ier)

  num_i=nsig
  num_o=nsig_save
  do k=1,nsig_save
     do j=1,lon2
        do i=1,lat2
           prsl_r(i,j,k)=one_tenth*(aeta1_save(k)*pdtop_ll + &
                           aeta2_save(k)*(ten*ges_ps(i,j)-pdtop_ll-pt_ll) + &
                           pt_ll)

        end do
     end do
  end do

!  REPEAT FOR NMMB-GFS COORDINATE

  do k=1,nsig
     do j=1,lon2
        do i=1,lat2
           prsl_m(i,j,k)=one_tenth*(aeta1_ll(k)*pdtop_ll + &
                           aeta2_ll(k)*(ten*ges_ps(i,j)-pdtop_ll-pt_ll) + &
                           pt_ll)
        end do
     end do
  end do

!  In following, for nmmb-gfs, replace saved guess with current analysis.  Then fields in guess_grids
!    will be updated to nmmb analysis.

!   note:  only need to interpolate from k0m+1 to nsig_save

  do j=1,lon2
     do i=1,lat2
        do k=1,nsig
           xspli(k)=log(prsl_m(i,j,k)*ten)
        end do
        do k=1,nsig_save
           xsplo(k)=log(prsl_r(i,j,k)*ten)
        end do
!  u:
        do k=1,nsig
           yspli(k)=ges_u(i,j,k)-ges_u_r_g(i,j,k,ntguessig)
           ges_u_r_g(i,j,k,ntguessig)=ges_u(i,j,k) !  keep original for after write analysis
        end do
        call intp_spl(xspli,yspli,xsplo,ysplo,num_i,num_o)
!               following is to correct for bug in intp_spl
        do k=1,nsig_save
           if(xsplo(k) < xspli(nsig)) ysplo(k)=yspli(nsig)
           if(xsplo(k) > xspli(1)) ysplo(k)=yspli(1)
        end do
        do k=1,nsig_save
           ges_u(i,j,k)=ysplo(k)+ges_u_r(i,j,k,ntguessig)
        end do
!  v:
        do k=1,nsig
           yspli(k)=ges_v(i,j,k)-ges_v_r_g(i,j,k,ntguessig)
           ges_v_r_g(i,j,k,ntguessig)=ges_v(i,j,k) !  keep original for after write analysis
        end do
        call intp_spl(xspli,yspli,xsplo,ysplo,num_i,num_o)
!               following is to correct for bug in intp_spl
        do k=1,nsig_save
           if(xsplo(k) < xspli(nsig)) ysplo(k)=yspli(nsig)
           if(xsplo(k) > xspli(1)   ) ysplo(k)=yspli(1)
        end do
        do k=1,nsig_save
           ges_v(i,j,k)=ysplo(k)+ges_v_r(i,j,k,ntguessig)
        end do
!  q:
        do k=1,nsig
           yspli(k)=ges_q(i,j,k)-ges_q_r_g(i,j,k,ntguessig)
           ges_q_r_g(i,j,k,ntguessig)=ges_q(i,j,k) !  keep original for after write analysis
        end do
        call intp_spl(xspli,yspli,xsplo,ysplo,num_i,num_o)
!               following is to correct for bug in intp_spl
        do k=1,nsig_save
           if(xsplo(k) < xspli(nsig)) ysplo(k)=yspli(nsig)
           if(xsplo(k) > xspli(1)   ) ysplo(k)=yspli(1)
        end do
        do k=1,nsig_save
           ges_q(i,j,k)=ysplo(k)+ges_q_r(i,j,k,ntguessig)
        end do
! tsen:
        do k=1,nsig
           yspli(k)=ges_tsen(i,j,k,ntguessig)-ges_tsen_r_g(i,j,k,ntguessig)
           ges_tsen_r_g(i,j,k,ntguessig)=ges_tsen(i,j,k,ntguessig) !  keep original for after write analysis
        end do
        call intp_spl(xspli,yspli,xsplo,ysplo,num_i,num_o)
!               following is to correct for bug in intp_spl
        do k=1,nsig_save
           if(xsplo(k) < xspli(nsig)) ysplo(k)=yspli(nsig)
           if(xsplo(k) > xspli(1)   ) ysplo(k)=yspli(1)
        end do
        do k=1,nsig_save
           ges_tsen(i,j,k,ntguessig)=ysplo(k)+ges_tsen_r(i,j,k,ntguessig)
        end do
! oz:  
        do k=1,nsig
           yspli(k)=ges_oz(i,j,k)-ges_oz_r_g(i,j,k,ntguessig)
           ges_oz_r_g(i,j,k,ntguessig)=ges_oz(i,j,k) !  keep original for after write analysis
        end do
        call intp_spl(xspli,yspli,xsplo,ysplo,num_i,num_o)
!               following is to correct for bug in intp_spl
        do k=1,nsig_save
           if(xsplo(k) < xspli(nsig)) ysplo(k)=yspli(nsig)
           if(xsplo(k) > xspli(1)   ) ysplo(k)=yspli(1)
        end do
        do k=1,nsig_save
           ges_oz(i,j,k)=ysplo(k)+ges_oz_r(i,j,k,ntguessig)
        end do

     end do
  end do

end subroutine revert_to_nmmb

subroutine restore_nmmb_gfs
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    restore_nmmb_gfs
!   prgmmr: parrish          org: np22                date: 2012-02-18
!
! abstract: recreate mixed nmmb-gfs variables for use in getting final fit of data to analysis.
!
! program history log:
!   2012-09-06  parrish, initial documentation
!   2013-10-19  todling - metguess now holds background
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

  use gridmod, only: lat2,lon2,nsig
  use mpimod, only: mype
             use mpimod, only: mpi_comm_world
  use kinds, only: i_kind,r_kind
  use guess_grids, only: ntguessig
  use guess_grids, only: ges_tsen
  use gfs_stratosphere, only: ges_tv_r_g,ges_q_r_g,ges_u_r_g,ges_v_r_g,ges_tsen_r_g,ges_oz_r_g
  use gsi_bundlemod, only : gsi_bundlegetpointer
  use gsi_metguess_mod, only: gsi_metguess_bundle
  use mpeu_util, only: die
  implicit none

  character(len=*),parameter::myname='restore_nmmb_gfs'
  integer(i_kind) i,j,k,ier,istatus
  real(r_kind),dimension(:,:,:),pointer:: ges_u  =>NULL()
  real(r_kind),dimension(:,:,:),pointer:: ges_v  =>NULL()
  real(r_kind),dimension(:,:,:),pointer:: ges_q  =>NULL()
  real(r_kind),dimension(:,:,:),pointer:: ges_oz =>NULL()

  ier=0
  call gsi_bundlegetpointer(gsi_metguess_bundle(ntguessig),'u'  ,ges_u ,istatus) 
  ier=ier+istatus
  call gsi_bundlegetpointer(gsi_metguess_bundle(ntguessig),'v'  ,ges_v ,istatus) 
  ier=ier+istatus
  call gsi_bundlegetpointer(gsi_metguess_bundle(ntguessig),'q'  ,ges_q ,istatus) 
  ier=ier+istatus
  call gsi_bundlegetpointer(gsi_metguess_bundle(ntguessig),'oz' ,ges_oz,istatus) 
  ier=ier+istatus
  if(ier/=0) call die(myname,': missing guess vars, aborting ...',ier)

!  restore nmmb-gfs analysis variable
  do k=1,nsig
     do j=1,lon2
        do i=1,lat2
           ges_u(i,j,k)=ges_u_r_g(i,j,k,ntguessig)
           ges_v(i,j,k)=ges_v_r_g(i,j,k,ntguessig)
           ges_q(i,j,k)=ges_q_r_g(i,j,k,ntguessig)
           ges_tsen(i,j,k,ntguessig)=ges_tsen_r_g(i,j,k,ntguessig)
           ges_oz(i,j,k)=ges_oz_r_g(i,j,k,ntguessig)
        end do
     end do
  end do

end subroutine restore_nmmb_gfs
