subroutine read_gfs_ozone_for_regional
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    read_gfs_ozone_for_regionl  read gfs ozone for regional
!   prgmmr: parrish          org: np22                date: 2010-03-06
!
! abstract: read gfs ozone and interpolate to regional analysis grid.
!
!
! program history log:
!   2010-03-06  parrish, initial documentation
!   2010-03-19  parrish - correct for extrapolation error in subroutine intp_spl
!   2010-03-19  parrish - correct diagnostic output of reg_ozmin, reg_ozmax
!   2010-04-15  wu - set ges_oz to a small number if the gfs input negative
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

  use gridmod, only: nlat,nlon,lat2,lon2,nsig,region_lat,region_lon,check_gfs_ozone_date
  use constants,only: zero,ione,izero,half,fv,rd_over_cp,one,h300
                       use constants, only: rad2deg  !  debug
  use mpimod, only: mpi_comm_world,ierror,mype,mpi_rtype,mpi_min,mpi_max
  use kinds, only: r_kind,i_kind
  use general_sub2grid_mod, only: sub2grid_info,general_sub2grid_create_info
  use general_sub2grid_mod, only: general_grid2sub_r_double,general_sub2grid_r_double
  use general_specmod, only: spec_vars,general_init_spec_vars
  use egrid2agrid_mod, only: g_create_egrid2points_slow,egrid2agrid_parm,g_egrid2points_faster
  use sigio_module, only: sigio_intkind,sigio_head,sigio_srhead
  use guess_grids, only: ges_prsl,ges_oz,ntguessig
  use aniso_ens_util, only: intp_spl
  use obsmod, only: iadate
  implicit none

  type(sub2grid_info) grd_gfs,grd_mix
  type(spec_vars) sp_gfs
  real(r_kind),allocatable,dimension(:,:,:) :: pri,vor,div,u,v,tv,q,cwmr,oz,prsl
  real(r_kind),allocatable,dimension(:,:)   :: z,ps
  real(r_kind),allocatable,dimension(:) :: ak5,bk5,ck5,tref5
  real(r_kind),allocatable :: work_sub(:,:,:,:),work(:,:,:),work_reg(:,:,:)

  real(r_kind) bar_norm,sig_norm,kapr,kap1,trk
  integer(i_kind) iret,i,j,k,k2,m,n,il,jl,mm1
  character(24) filename
  logical ice
  integer(sigio_intkind):: lunges = 11
  type(sigio_head):: sighead
  type(egrid2agrid_parm) :: p_g2r
  integer(i_kind) inner_vars,num_fields,nlat_gfs,nlon_gfs,nsig_gfs,jcap_gfs,jcap_gfs_test
  integer(i_kind) nord_g2r
  logical,allocatable :: vector(:)
  logical vector0
  real(r_kind) ozmin,ozmax
  real(r_kind) ozmin0,ozmax0
  real(r_kind),parameter::  zero_001=0.001_r_kind
  logical regional
  real(r_kind),allocatable,dimension(:) :: xspli,yspli,xsplo,ysplo
  integer(i_kind) iyr,ihourg
  integer(i_kind),dimension(4):: idate4
  integer(i_kind),dimension(8) :: ida,jda 
  integer(i_kind),dimension(5) :: iadate_gfs
  real(r_kind) hourg
  real(r_kind),dimension(5):: fha
  real(r_kind),allocatable,dimension(:)::glb_ozmin,glb_ozmax,reg_ozmin,reg_ozmax
  real(r_kind),allocatable,dimension(:)::glb_ozmin0,glb_ozmax0,reg_ozmin0,reg_ozmax0

!     figure out what are acceptable dimensions for global grid, based on resolution of input spectral coefs
!   need to inquire from file what is spectral truncation, then setup general spectral structure variable

  regional=.true.
  filename='gfs_sigf03'
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

! Compute valid time from guess date and forecast length and compare to iadate, the analysis time
  iyr=idate4(4)
  ihourg=hourg
  if(iyr>=izero.and.iyr<=99_i_kind) then
     if(iyr>51_i_kind) then
        iyr=iyr+1900_i_kind
     else
        iyr=iyr+2000_i_kind
     end if
  end if
  fha=zero ; ida=izero; jda=izero
  fha(2)=ihourg    ! relative time interval in hours
  ida(1)=iyr       ! year
  ida(2)=idate4(2) ! month
  ida(3)=idate4(3) ! day
  ida(4)=izero     ! time zone
  ida(5)=idate4(1) ! hour
  call w3movdat(fha,ida,jda)
  iadate_gfs(1)=jda(1) ! year
  iadate_gfs(2)=jda(2) ! mon
  iadate_gfs(3)=jda(3) ! day
  iadate_gfs(4)=jda(5) ! hour
  iadate_gfs(5)=izero  ! minute
  if(mype == 0) then
     write(6,*)' in read_gfs_ozone_for_regional, iadate_gfs=',iadate_gfs
     write(6,*)' in read_gfs_ozone_for_regional, iadate    =',iadate
  end if
  if(iadate_gfs(1)/=iadate(1).or.iadate_gfs(2)/=iadate(2).or.iadate_gfs(3)/=iadate(3).or.&
                                 iadate_gfs(4)/=iadate(4).or.iadate_gfs(5)/=iadate(5) ) then
     if(mype == 0) write(6,*)' WARNING: GFS OZONE FIELD DATE NOT EQUAL TO ANALYSIS DATE'
     if(check_gfs_ozone_date) then
        if(mype == 0) write(6,*)' CHECK_GFS_OZONE_DATE = .true., PROGRAM STOPS DUE TO OZONE DATE MISMATCH'
        call stop2(85)
     end if
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
  if (sighead%nvcoord == ione) then
     do k=1,sighead%levs+ione
        bk5(k) = sighead%vcoord(k,1)
     end do
  elseif (sighead%nvcoord == 2_i_kind) then
     do k = 1,sighead%levs+ione
        ak5(k) = sighead%vcoord(k,1)*zero_001
        bk5(k) = sighead%vcoord(k,2)
     end do
  elseif (sighead%nvcoord == 3_i_kind) then
     do k = 1,sighead%levs+ione
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
  num_fields=2*nsig_gfs           !  want to transfer ozone and 3d pressure from gfs subdomain to slab
  allocate(vector(num_fields))
  vector=.false.
  call general_sub2grid_create_info(grd_gfs,inner_vars,nlat_gfs,nlon_gfs,nsig_gfs,num_fields, &
                                  .not.regional,vector)
  jcap_gfs=sighead%jcap
  jcap_gfs_test=jcap_gfs
  call general_init_spec_vars(sp_gfs,jcap_gfs,jcap_gfs_test,grd_gfs%nlat,grd_gfs%nlon)

!  also want to set up regional grid structure variable grd_mix, which still has number of
!   vertical levels set to nsig_gfs, but horizontal dimensions set to regional domain.

  call general_sub2grid_create_info(grd_mix,inner_vars,nlat,nlon,nsig_gfs,num_fields,regional,vector)
  deallocate(vector)


!!   allocate necessary space on global grid

  allocate( pri(grd_gfs%lat2,grd_gfs%lon2,grd_gfs%nsig+1))
  allocate( vor(grd_gfs%lat2,grd_gfs%lon2,grd_gfs%nsig))
  allocate( div(grd_gfs%lat2,grd_gfs%lon2,grd_gfs%nsig))
  allocate(   u(grd_gfs%lat2,grd_gfs%lon2,grd_gfs%nsig))
  allocate(   v(grd_gfs%lat2,grd_gfs%lon2,grd_gfs%nsig))
  allocate(  tv(grd_gfs%lat2,grd_gfs%lon2,grd_gfs%nsig))
  allocate(   q(grd_gfs%lat2,grd_gfs%lon2,grd_gfs%nsig))
  allocate(cwmr(grd_gfs%lat2,grd_gfs%lon2,grd_gfs%nsig))
  allocate(  oz(grd_gfs%lat2,grd_gfs%lon2,grd_gfs%nsig))
  allocate(prsl(grd_gfs%lat2,grd_gfs%lon2,grd_gfs%nsig))
  allocate(   z(grd_gfs%lat2,grd_gfs%lon2))
  allocate(  ps(grd_gfs%lat2,grd_gfs%lon2))

  call general_read_gfsatm(grd_gfs,sp_gfs,filename,mype,z,ps,vor,div,u,v,tv,q,cwmr,oz,iret)
  deallocate(vor,div,u,v,q,cwmr,z)
  do k=1,grd_gfs%nsig
     ozmin=minval(oz(:,:,k))
     ozmax=maxval(oz(:,:,k))
     call mpi_allreduce(ozmin,ozmin0,1,mpi_rtype,mpi_min,mpi_comm_world,ierror)
     call mpi_allreduce(ozmax,ozmax0,1,mpi_rtype,mpi_max,mpi_comm_world,ierror)
     if(mype == 0) write(6,'(" k,min,max gfs oz = ",i3,2e15.4)')k,ozmin0,ozmax0
  end do

! compute 3d pressure on interfaces
  kap1=rd_over_cp+one
  kapr=one/rd_over_cp
  pri=zero
  k=ione
  k2=grd_gfs%nsig+ione
  do j=1,grd_gfs%lon2
     do i=1,grd_gfs%lat2
        pri(i,j,k)=ps(i,j)
        pri(i,j,k2)=zero
     end do
  end do
  if (sighead%idvc /= 3_i_kind) then
     do k=2,grd_gfs%nsig
        do j=1,grd_gfs%lon2
           do i=1,grd_gfs%lat2
              pri(i,j,k)=ak5(k)+bk5(k)*ps(i,j)
           end do
        end do
     end do
  else
     do k=2,grd_gfs%nsig
        do j=1,grd_gfs%lon2
           do i=1,grd_gfs%lat2
              trk=(half*(tv(i,j,k-ione)+tv(i,j,k))/tref5(k))**kapr
              pri(i,j,k)=ak5(k)+(bk5(k)*ps(i,j))+(ck5(k)*trk)
           end do
        end do
     end do
  end if
  deallocate(tv,ps,ak5,bk5,ck5,tref5)
  do k=1,grd_gfs%nsig+1
     ozmin=minval(pri(:,:,k))
     ozmax=maxval(pri(:,:,k))
     call mpi_allreduce(ozmin,ozmin0,1,mpi_rtype,mpi_min,mpi_comm_world,ierror)
     call mpi_allreduce(ozmax,ozmax0,1,mpi_rtype,mpi_max,mpi_comm_world,ierror)
     if(mype == 0) write(6,'(" k,min,max gfs pri = ",i3,2e15.4)')k,ozmin0,ozmax0
  end do

! next get pressure at layer midpoints.
  if (sighead%idsl/=2_i_kind) then
     do j=1,grd_gfs%lon2
        do i=1,grd_gfs%lat2
           do k=1,grd_gfs%nsig
              prsl(i,j,k)=((pri(i,j,k)**kap1-pri(i,j,k+ione)**kap1)/&
                           (kap1*(pri(i,j,k)-pri(i,j,k+ione))))**kapr
           end do
        end do
     end do
  else
     do j=1,grd_gfs%lon2
        do i=1,grd_gfs%lat2
           do k=1,grd_gfs%nsig
              prsl(i,j,k)=(pri(i,j,k)+pri(i,j,k+ione))*half
           end do
        end do
     end do
  end if
  deallocate(pri)
  do k=1,grd_gfs%nsig
     ozmin=minval(prsl(:,:,k))
     ozmax=maxval(prsl(:,:,k))
     call mpi_allreduce(ozmin,ozmin0,1,mpi_rtype,mpi_min,mpi_comm_world,ierror)
     call mpi_allreduce(ozmax,ozmax0,1,mpi_rtype,mpi_max,mpi_comm_world,ierror)
     if(mype == 0) write(6,'(" k,min,max gfs prsl = ",i3,2e15.4)')k,ozmin0,ozmax0
  end do

! use general_sub2grid to go to horizontal slabs 

  allocate(work_sub(grd_gfs%lat2,grd_gfs%lon2,grd_gfs%nsig,2))
  do k=1,grd_gfs%nsig
     do j=1,grd_gfs%lon2
        do i=1,grd_gfs%lat2
           work_sub(i,j,k,1)=prsl(i,j,k)
           work_sub(i,j,k,2)=oz(i,j,k)
        end do
     end do
  end do
  deallocate(oz,prsl)
  allocate(work(grd_gfs%nlat,grd_gfs%nlon,grd_gfs%kbegin_loc:grd_gfs%kend_alloc))
  call general_sub2grid_r_double(grd_gfs,work_sub,work)
  deallocate(work_sub)

! then interpolate to regional analysis grid
  nord_g2r=4
  call g_create_egrid2points_slow(nlat*nlon,region_lat,region_lon, &
                    grd_gfs%nlat,sp_gfs%rlats,grd_gfs%nlon,sp_gfs%rlons,nord_g2r,p_g2r)
  vector0=.false.
  allocate(work_reg(grd_mix%nlat,grd_mix%nlon,grd_gfs%kbegin_loc:grd_gfs%kend_alloc))
  do k=grd_gfs%kbegin_loc,grd_gfs%kend_loc
     call g_egrid2points_faster(p_g2r,work(:,:,k),work_reg(:,:,k),vector0)
  end do
  deallocate(work)

! next general_grid2sub to go to regional grid subdomains.
  allocate(work_sub(grd_mix%lat2,grd_mix%lon2,grd_gfs%nsig,2))
  call general_grid2sub_r_double(grd_mix,work_reg,work_sub)
  deallocate(work_reg)
  do k=1,grd_gfs%nsig
     ozmin=minval(work_sub(:,:,k,1))
     ozmax=maxval(work_sub(:,:,k,1))
     call mpi_allreduce(ozmin,ozmin0,1,mpi_rtype,mpi_min,mpi_comm_world,ierror)
     call mpi_allreduce(ozmax,ozmax0,1,mpi_rtype,mpi_max,mpi_comm_world,ierror)
  end do
  do k=1,grd_gfs%nsig
     ozmin=minval(work_sub(:,:,k,2))
     ozmax=maxval(work_sub(:,:,k,2))
     call mpi_allreduce(ozmin,ozmin0,1,mpi_rtype,mpi_min,mpi_comm_world,ierror)
     call mpi_allreduce(ozmax,ozmax0,1,mpi_rtype,mpi_max,mpi_comm_world,ierror)
  end do

! finally, interpolate/extrapolate in vertical using yoshi's spline code.  deposit result in ges_oz.
  allocate(xspli(grd_mix%nsig),yspli(grd_mix%nsig),xsplo(nsig),ysplo(nsig))
  allocate(glb_ozmin(grd_mix%nsig),glb_ozmax(grd_mix%nsig))
  allocate(reg_ozmin(        nsig),reg_ozmax(        nsig))
  allocate(glb_ozmin0(grd_mix%nsig),glb_ozmax0(grd_mix%nsig))
  allocate(reg_ozmin0(        nsig),reg_ozmax0(        nsig))
  glb_ozmin= huge(glb_ozmin)
  glb_ozmax=-huge(glb_ozmax)
  reg_ozmin= huge(reg_ozmin)
  reg_ozmax=-huge(reg_ozmax)
  do j=1,lon2
     do i=1,lat2
        do k=1,grd_mix%nsig
           xspli(k)=log(work_sub(i,j,k,1)*10.0_r_kind)
           yspli(k)=work_sub(i,j,k,2)
           glb_ozmax(k)=max(yspli(k),glb_ozmax(k))
           glb_ozmin(k)=min(yspli(k),glb_ozmin(k))
                 
        end do
        do k=1,nsig
           xsplo(k)=log(ges_prsl(i,j,k,ntguessig)*10._r_kind)
        end do
        call intp_spl(xspli,yspli,xsplo,ysplo,grd_mix%nsig,nsig)
!               following is to correct for bug in intp_spl
        do k=1,nsig
           if(xsplo(k) < xspli(grd_mix%nsig)) ysplo(k)=yspli(grd_mix%nsig)
           if(xsplo(k) > xspli(1)) ysplo(k)=yspli(1)
        end do
        do k=1,nsig
           if(ysplo(k) < zero)ysplo(k)=1.e-10_r_kind
           ges_oz(i,j,k,:)=ysplo(k)   !  for now, only read in ges at analysis time and copy to time levels
           reg_ozmax(k)=max(ysplo(k),reg_ozmax(k))
           reg_ozmin(k)=min(ysplo(k),reg_ozmin(k))
        end do
     end do
  end do
  deallocate(work_sub)
  call mpi_allreduce(reg_ozmax,reg_ozmax0,nsig,mpi_rtype,mpi_max,mpi_comm_world,ierror)
  call mpi_allreduce(reg_ozmin,reg_ozmin0,nsig,mpi_rtype,mpi_min,mpi_comm_world,ierror)
  call mpi_allreduce(glb_ozmax,glb_ozmax0,grd_mix%nsig,mpi_rtype,mpi_max,mpi_comm_world,ierror)
  call mpi_allreduce(glb_ozmin,glb_ozmin0,grd_mix%nsig,mpi_rtype,mpi_min,mpi_comm_world,ierror)
  if(mype == 0) then
     do k=1,grd_mix%nsig
        write(6,'(" k,glb_ozmin,max=",i4,2e15.4)') k,glb_ozmin0(k),glb_ozmax0(k)
     end do
     do k=1,nsig
        write(6,'(" k,reg_ozmin,max=",i4,2e15.4)') k,reg_ozmin0(k),reg_ozmax0(k)
     end do
  end if

  return
end subroutine read_gfs_ozone_for_regional
