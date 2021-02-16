module mod_fv3_lolgrid
!$$$ module documentation block
!           .      .    .                                       .
! module:   mod_fv3_lola
!   prgmmr: parrish
!
! abstract:  This module contains routines to interpolate from a single
!             fv3 D grid tile to a rotated lat-lon analysis grid which completely
!             covers the fv3 tile.  Points beyond the fv3 tile are
!             filled with nearest fv3 edge values, but have no actual
!             impact on the analysis.
!
! program history log:
!   2017-02-24  parrish--initial documentation (patterned after
!   mod_fv3_to_a.f90)
!   2017-10-10  wu w - setup interpolation and trnsform coeff in generate_anl_grid
!                      add routines earthuv2fv3, fv3uv2earth, fv3_h_to_ll
!                        fv3_ll_to_h
!   2019-11-01  wu   - add checks in generate_anl_grid to present the mean
 !                      longitude correctly to fix problem near lon=0
!
!
! subroutines included:
!   sub generate_anl_grid
!   sub earthuv2fv3
!   sub fv3uv2earth
!   sub fv3_h_to_ll
!   sub fv3_ll_to_h
!   sub rotate2deg 
!   sub unrotate2deg 
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

!       DIAGRAM:  D-Grid layout:
!
!   1                  nx
!   .                   .   (U,H)
!
! 1                     nx +1
! .                       .    (V)

!   U   U   U   U   U   U        + ny +1 (for U)
! V H V H V H V H V H V H V      + ny    (for V,H)
!   U   U   U   U   U   U                            xh(i) = i            dx=1
! V H V H V H V H V H V H V                          xu(i) = i
!   U   U   U   U   U   U                            xv(i) = i-0.5
! V H V H V H V H V H V H V
!   U   U   U   U   U   U                            yh(j) = j            dy=1
! V H V H V H V H V H V H V                          yu(j) = j-0.5
!   U   U   U   U   U   U                            yv(j) = j
! V H V H V H V H V H V H V
!   U   U   U   U   U   U
! V H V H V H V H V H V H V      + 1     (for V,H)
!   U   U   U   U   U   U        + 1     (for U)

! U(nx ,ny +1),V(nx +1,ny ),H(nx ,ny )

  use kinds, only: r_kind,i_kind
  implicit none
!
  private
  public :: generate_regular_grids
  public :: definecoef_regular_grids
  public :: fv3_h_to_ll_regular_grids,fv3_ll_to_h_regular_grids
  public :: fv3uv2earth_regular_grids,earthuv2fv3_regular_grids
  public :: fv3sar2grid_parm
  public :: p_fv3sar2anlgrid
  public :: p_fv3sar2ensgrid
  public :: nxa,nya
  public :: nxa_ens,nya_ens
  type fv3sar2grid_parm
  logical bilinear
  integer(i_kind) nxout,nyout,nx,ny
  real(r_kind) ,pointer,dimension(:,:):: fv3dx=>null(),fv3dx1=>null(),fv3dy=>null(),fv3dy1=>null()
  integer(i_kind),pointer,dimension(:,:)::  fv3ix=>null(),fv3ixp=>null(),fv3jy=>null(),fv3jyp=>null()
  real(r_kind) ,pointer,dimension(:,:):: a3dx=>null(),a3dx1=>null(),a3dy=>null(),a3dy1=>null()
  real(r_kind) ,pointer,dimension(:,:):: cangu=>null(),sangu=>null(),cangv=>null(),sangv=>null()
  integer(i_kind),pointer,dimension(:,:)::  a3ix=>null(),a3ixp=>null(),a3jy=>null(),a3jyp=>null()
  end type
  type (fv3sar2grid_parm)::p_fv3sar2anlgrid,p_fv3sar2ensgrid
  integer(i_kind) nxa,nya
  integer(i_kind) nxa_ens,nya_ens
  

contains

subroutine generate_regular_grids(nx,ny,grid_lon,grid_lont,grid_lat,grid_latt,p_fv3sar2grid,&
                             nlatout,nlonout,region_lat_out,region_lon_out, &
                             region_dx_out,region_dy_out, &
                             region_dxi_out,region_dyi_out, &
                             coeffx_out,coeffy_out)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    generate_ens_grid
!clt modified from generate_anl_grid
!   prgmmr: parrish
!
! abstract:  define rotated lat-lon analysis grid which is centered on fv3 tile 
!             and oriented to completely cover the tile.
!
! program history log:
!   2017-05-02  parrish
!   2017-10-10  wu   - 1. setup analysis A-grid, 
!                      2. compute/setup FV3 to A grid interpolation parameters
!                      3. compute/setup A to FV3 grid interpolation parameters         
!                      4. setup weightings for wind conversion from FV3 to earth
!
!     2019-11-01  wu   - add checks to present the mean longitude correctly to fix
!   input argument list:
!    nx, ny               - number of cells = nx*ny 
!    grid_lon ,grid_lat   - longitudes and latitudes of fv3 grid cell corners
!    grid_lont,grid_latt  - longitudes and latitudes of fv3 grid cell centers
 !                       problem near lon=0

!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

  use kinds, only: r_kind,i_kind
  use constants, only: quarter,one,two,half,zero,deg2rad,rearth,rad2deg
  use gridmod,  only:grid_ratio_fv3_regional
  use mpimod, only: mype
  use gridmod,  only:init_general_transform 
  use gridmod,  only: fv3_regional_dd_reduce,nlon_fv3_regional_reduce,nlat_fv3_regional_reduce

  implicit none
  type (fv3sar2grid_parm),intent(inout):: p_fv3sar2grid
  real(r_kind),allocatable,intent(out):: region_lat_out(:,:),region_lon_out(:,:)
  real(r_kind),allocatable,intent(out):: region_dx_out(:,:),region_dy_out(:,:)
  real(r_kind),allocatable,intent(out):: region_dxi_out(:,:),region_dyi_out(:,:)
  real(r_kind),allocatable,optional, intent(out):: coeffx_out(:,:),coeffy_out(:,:)
  integer(i_kind), intent(out):: nlatout,nlonout


  real(r_kind) ,pointer,dimension(:,:):: fv3dx,fv3dx1,fv3dy,fv3dy1
  integer(i_kind),pointer,dimension(:,:)::  fv3ix,fv3ixp,fv3jy,fv3jyp
  real(r_kind) ,pointer,dimension(:,:):: a3dx,a3dx1,a3dy,a3dy1
  real(r_kind) ,pointer,dimension(:,:):: cangu,sangu,cangv,sangv
  integer(i_kind),pointer,dimension(:,:)::  a3ix,a3ixp,a3jy,a3jyp

  real(r_kind),allocatable,dimension(:)::xbh_a,xa_a,xa_b
  real(r_kind),allocatable,dimension(:)::ybh_a,ya_a,ya_b,yy
  real(r_kind),allocatable,dimension(:,:)::xbh_b,ybh_b
  real(r_kind) dlat,dlon,dyy,dxx,dyyi,dxxi
  real(r_kind) dyyh,dxxh


  integer(i_kind), intent(in   ) :: nx,ny                 ! fv3 tile x- and y-dimensions
  real(r_kind)   , intent(inout) :: grid_lon(nx+1,ny+1)   ! fv3 cell corner longitudes
  real(r_kind)   , intent(inout) :: grid_lont(nx,ny)      ! fv3 cell center longitudes
  real(r_kind)   , intent(inout) :: grid_lat(nx+1,ny+1)   ! fv3 cell corner latitudes
  real(r_kind)   , intent(inout) :: grid_latt(nx,ny)      ! fv3 cell center latitudes
  logical :: bilinear
  integer(i_kind) i,j,ir,jr,n
  real(r_kind),allocatable,dimension(:,:) :: xc,yc,zc,gclat,gclon,gcrlat,gcrlon,rlon_in,rlat_in
  real(r_kind),allocatable,dimension(:,:) :: glon_an,glat_an
  real(r_kind) xcent,ycent,zcent,rnorm,centlat,centlon
  real(r_kind) adlon,adlat,alon,clat,clon
  integer(i_kind) nxout,nyout
  integer(i_kind) nlonh,nlath,nxh,nyh
  integer(i_kind) ib1,ib2,jb1,jb2,jj

  integer(i_kind) nord_e2a
  real(r_kind)gxa,gya

  real(r_kind) x(nx+1,ny+1),y(nx+1,ny+1),z(nx+1,ny+1), xr,yr,zr,xu,yu,zu,rlat,rlon
  real(r_kind) xv,yv,zv,vval
  real(r_kind) cx,cy
  real(r_kind) uval,ewval,nsval
  real(r_kind) diff,sq180

  real(r_kind) d(4),ds
  integer(i_kind) kk,k
  real(r_kind),allocatable:: rtem1(:,:)


  nord_e2a=4
  bilinear=.false.
  p_fv3sar2grid%bilinear=bilinear


!   create xc,yc,zc for the cell centers.
  allocate(xc(nx,ny))
  allocate(yc(nx,ny))
  allocate(zc(nx,ny))
  allocate(gclat(nx,ny))
  allocate(gclon(nx,ny))
  allocate(gcrlat(nx,ny))
  allocate(gcrlon(nx,ny))
  do j=1,ny
     do i=1,nx
        xc(i,j)=cos(grid_latt(i,j)*deg2rad)*cos(grid_lont(i,j)*deg2rad)
        yc(i,j)=cos(grid_latt(i,j)*deg2rad)*sin(grid_lont(i,j)*deg2rad)
        zc(i,j)=sin(grid_latt(i,j)*deg2rad)
     enddo
  enddo

!  compute center as average x,y,z coordinates of corners of domain --

  xcent=quarter*(xc(1,1)+xc(1,ny)+xc(nx,1)+xc(nx,ny))
  ycent=quarter*(yc(1,1)+yc(1,ny)+yc(nx,1)+yc(nx,ny))
  zcent=quarter*(zc(1,1)+zc(1,ny)+zc(nx,1)+zc(nx,ny))

  rnorm=one/sqrt(xcent**2+ycent**2+zcent**2)
  xcent=rnorm*xcent
  ycent=rnorm*ycent
  zcent=rnorm*zcent
  centlat=asin(zcent)*rad2deg
  centlon=atan2(ycent,xcent)*rad2deg

!!  compute new lats, lons
  call rotate2deg(grid_lont,grid_latt,gcrlon,gcrlat, &
                  centlon,centlat,nx,ny)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!  compute analysis A-grid  lats, lons
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!--------------------------obtain analysis grid dimensions nxout,nyout
  p_fv3sar2grid%nx=nx
  p_fv3sar2grid%ny=ny
  nxout=1+nint((nx-one)/grid_ratio_fv3_regional)
  p_fv3sar2grid%nxout=nxout
  nyout=1+nint((ny-one)/grid_ratio_fv3_regional)
  p_fv3sar2grid%nyout=nyout
  nlatout=p_fv3sar2grid%nyout
  nlonout=p_fv3sar2grid%nxout
  nxa=nxout
  nya=nyout  ! for compatiability 
  if(mype==0) print *,'nlatout,nlonout = ',nlatout,nlonout

!--------------------------obtain analysis grid spacing
  dlat=(maxval(gcrlat)-minval(gcrlat))/(ny-1)
  dlon=(maxval(gcrlon)-minval(gcrlon))/(nx-1)
  adlat=dlat*grid_ratio_fv3_regional
  adlon=dlon*grid_ratio_fv3_regional

!-------setup analysis A-grid; find center of the domain
  nlonh=nlonout/2
  nlath=nlatout/2

  if(nlonh*2==nlonout)then
     clon=adlon/two
     cx=half
  else
     clon=adlon
     cx=one
  endif

  if(nlath*2==nlatout)then
     clat=adlat/two
     cy=half
  else
     clat=adlat
     cy=one
  endif

!
!-----setup analysis A-grid from center of the domain
!
  allocate(rlat_in(nlatout,nlonout),rlon_in(nlatout,nlonout))
  do j=1,nlonout
     alon=(j-nlonh)*adlon-clon
     do i=1,nlatout
        rlon_in(i,j)=alon
     enddo
  enddo


  do j=1,nlonout
     do i=1,nlatout
        rlat_in(i,j)=(i-nlath)*adlat-clat
     enddo
  enddo

  if (allocated(region_dx_out )) deallocate(region_dx_out ) !clt will correspond
                                                            !to region_dx_ens and so on
  if (allocated(region_dy_out )) deallocate(region_dy_out )
  allocate(region_dx_out(nlatout,nlonout),region_dy_out(nlatout,nlonout))
  allocate(region_dxi_out(nlatout,nlonout),region_dyi_out(nlatout,nlonout))
  if (present(coeffx_out))  allocate(coeffx_out(nlatout,nlonout))
  if (present(coeffy_out))  allocate(coeffy_out(nlatout,nlonout))
  allocate(rtem1(nlatout,nlonout))
  dyy=rearth*adlat*deg2rad
  dyyi=one/dyy
  dyyh=half/dyy
  do j=1,nlonout
     do i=1,nlatout
        region_dy_out(i,j)=dyy
        region_dyi_out(i,j)=dyyi
        rtem1(i,j)=dyyh
     enddo
  enddo
  if (present(coeffy_out))  coeffy_out=rtem1

  do i=1,nlatout
     dxx=rearth*cos(rlat_in(i,1)*deg2rad)*adlon*deg2rad
     dxxi=one/dxx
     dxxh=half/dxx
     do j=1,nlonout
        region_dx_out(i,j)=dxx
        region_dxi_out(i,j)=dxxi
        rtem1(i,j)=dxxh
     enddo
  enddo
  if (present(coeffx_out))  coeffx_out=rtem1
  

!
!----------  setup  region_lat,region_lon in earth coord
!
  if (allocated(region_lat_out)) deallocate(region_lat_out)
  if (allocated(region_lon_out)) deallocate(region_lon_out)
  allocate(region_lat_out(nlatout,nlonout),region_lon_out(nlatout,nlonout))

  call unrotate2deg(region_lon_out,region_lat_out,rlon_in,rlat_in, &
                    centlon,centlat,nlatout,nlonout)

  region_lat_out=region_lat_out*deg2rad
  region_lon_out=region_lon_out*deg2rad
  if (present(coeffx_out)) then ! if coeffx_out is present, this is for analysis grid 
  allocate(glat_an(nlonout,nlatout),glon_an(nlonout,nlatout))

  do j=1,nlatout
     do i=1,nlonout
        glat_an(i,j)=region_lat_out(j,i)
        glon_an(i,j)=region_lon_out(j,i)
     enddo
  enddo

  call init_general_transform(glat_an,glon_an)
 
  deallocate(glat_an,glon_an)

  endif
!--------------------compute all combinations of relative coordinates

  allocate(xbh_a(nx),xbh_b(nx,ny),xa_a(nxout),xa_b(nxout))
  allocate(ybh_a(ny),ybh_b(nx,ny),ya_a(nyout),ya_b(nyout))

  nxh=nx/2
  nyh=ny/2

!!!!!! fv3 rotated grid; not equal spacing, non_orthogonal !!!!!!
  do j=1,ny
     jr=ny+1-j
     do i=1,nx
        ir=nx+1-i
        xbh_b(ir,jr)=gcrlon(i,j)/dlon
     end do
  end do
  do j=1,ny
     jr=ny+1-j
     do i=1,nx
       ir=nx+1-i
       ybh_b(ir,jr)=gcrlat(i,j)/dlat
     end do
  end do

!!!!  define analysis A grid  !!!!!!!!!!!!!
  do j=1,nxout
     xa_a(j)=(float(j-nlonh)-cx)*grid_ratio_fv3_regional
  end do
  do i=1,nyout
     ya_a(i)=(float(i-nlath)-cy)*grid_ratio_fv3_regional
  end do

!!!!!compute fv3 to A grid interpolation parameters !!!!!!!!!
  allocate  (   p_fv3sar2grid%fv3dx(nxout,nyout),p_fv3sar2grid%fv3dx1(nxout,nyout),p_fv3sar2grid%fv3dy(nxout,nyout),p_fv3sar2grid%fv3dy1(nxout,nyout) )
  allocate  (   p_fv3sar2grid%fv3ix(nxout,nyout),p_fv3sar2grid%fv3ixp(nxout,nyout),p_fv3sar2grid%fv3jy(nxout,nyout),p_fv3sar2grid%fv3jyp(nxout,nyout) )
  fv3dx=>p_fv3sar2grid%fv3dx
  fv3dx1=>p_fv3sar2grid%fv3dx1
  fv3dy=>p_fv3sar2grid%fv3dy
  fv3dy1=>p_fv3sar2grid%fv3dy1
  fv3ix=>p_fv3sar2grid%fv3ix
  fv3ixp=>p_fv3sar2grid%fv3ixp
  fv3jy=>p_fv3sar2grid%fv3jy
  fv3jyp=>p_fv3sar2grid%fv3jyp
  
  allocate(yy(ny))

! iteration to find the fv3 grid cell
  jb1=1
  ib1=1
  do j=1,nyout
     do i=1,nxout
      do n=1,3 
         gxa=xa_a(i)
         if(gxa < xbh_b(1,jb1))then
            gxa= 1
         else if(gxa > xbh_b(nx,jb1))then
            gxa= nx
         else
            call grdcrd1(gxa,xbh_b(1,jb1),nx,1)
         endif
         ib2=ib1
         ib1=gxa
         do jj=1,ny
            yy(jj)=ybh_b(ib1,jj)
         enddo
         gya=ya_a(j)
         if(gya < yy(1))then
            gya= 1
         else if(gya > yy(ny))then
            gya= ny
         else
            call grdcrd1(gya,yy,ny,1)
         endif
         jb2=jb1
         jb1=gya

         if((ib1 == ib2) .and. (jb1 == jb2)) exit
         if(n==3 ) then     
!!!!!!!   if not converge, find the nearest corner point
            d(1)=(xa_a(i)-xbh_b(ib1,jb1))**2+(ya_a(j)-ybh_b(ib1,jb1))**2
            d(2)=(xa_a(i)-xbh_b(ib1+1,jb1))**2+(ya_a(j)-ybh_b(ib1+1,jb1))**2
            d(3)=(xa_a(i)-xbh_b(ib1,jb1+1))**2+(ya_a(j)-ybh_b(ib1,jb1+1))**2
            d(4)=(xa_a(i)-xbh_b(ib1+1,jb1+1))**2+(ya_a(j)-ybh_b(ib1+1,jb1+1))**2
            kk=1 
            do k=2,4
               if(d(k)<d(kk))kk=k
            enddo
!!!!!!!!!!! Find the cell for interpolation
            gxa=xa_a(i)
            gya=ya_a(j)
            if(kk==1)then
               call grdcrd1(gxa,xbh_b(1,jb1),nx,1)
               do jj=1,ny
                  yy(jj)=ybh_b(ib1,jj)
               enddo
               call grdcrd1(gya,yy,ny,1)
            else if(kk==2)then
               call grdcrd1(gxa,xbh_b(1,jb1),nx,1)
               do jj=1,ny
                  yy(jj)=ybh_b(ib1+1,jj)
               enddo
               call grdcrd1(gya,yy,ny,1)
            else if(kk==3)then
               call grdcrd1(gxa,xbh_b(1,jb1+1),nx,1)
               do jj=1,ny
                  yy(jj)=ybh_b(ib1,jj)
               enddo
               call grdcrd1(gya,yy,ny,1)
            else if(kk==4)then
               call grdcrd1(gxa,xbh_b(1,jb1+1),nx,1)
               do jj=1,ny
                  yy(jj)=ybh_b(ib1+1,jj)
               enddo
               call grdcrd1(gya,yy,ny,1)
            endif
            exit
         endif  !n=3   
      enddo  ! n

      fv3ix(i,j)=int(gxa)
      fv3ix(i,j)=min(max(1,fv3ix(i,j)),nx)
      fv3ixp(i,j)=min(nx,fv3ix(i,j)+1)
      fv3jy(i,j)=int(gya)
      fv3jy(i,j)=min(max(1,fv3jy(i,j)),ny)
      fv3jyp(i,j)=min(ny,fv3jy(i,j)+1)

      if(bilinear)then
         fv3dy(i,j)=max(zero,min(one,gya-fv3jy(i,j)))
         fv3dy1(i,j)=one-fv3dy(i,j)
         fv3dx(i,j)=max(zero,min(one,gxa-fv3ix(i,j)))
         fv3dx1(i,j)=one-fv3dx(i,j)
      else ! inverse-distance weighting average 
         ib1=fv3ix(i,j)
         ib2=fv3ixp(i,j)
         jb1=fv3jy(i,j)
         jb2=fv3jyp(i,j)
         if(xa_a(i)==xbh_b(ib1,jb1) .and. ya_a(j)==ybh_b(ib1,jb1))then
            fv3dy(i,j)=zero
            fv3dy1(i,j)=zero
            fv3dx(i,j)=one
            fv3dx1(i,j)=zero
         else if(xa_a(i)==xbh_b(ib2,jb1) .and. ya_a(j)==ybh_b(ib2,jb1))then
            fv3dy(i,j)=zero
            fv3dy1(i,j)=zero
            fv3dx(i,j)=zero
            fv3dx1(i,j)=one
         else if(xa_a(i)==xbh_b(ib1,jb2) .and. ya_a(j)==ybh_b(ib1,jb2))then
            fv3dy(i,j)=one 
            fv3dy1(i,j)=zero
            fv3dx(i,j)=zero
            fv3dx1(i,j)=zero
         else if(xa_a(i)==xbh_b(ib2,jb2) .and. ya_a(j)==ybh_b(ib2,jb2))then
            fv3dy(i,j)=zero
            fv3dy1(i,j)=one 
            fv3dx(i,j)=zero
            fv3dx1(i,j)=zero
         else 
            d(1)=one/((xa_a(i)-xbh_b(ib1,jb1))**2+(ya_a(j)-ybh_b(ib1,jb1))**2)
            d(2)=one/((xa_a(i)-xbh_b(ib2,jb1))**2+(ya_a(j)-ybh_b(ib2,jb1))**2)
            d(3)=one/((xa_a(i)-xbh_b(ib1,jb2))**2+(ya_a(j)-ybh_b(ib1,jb2))**2)
            d(4)=one/((xa_a(i)-xbh_b(ib2,jb2))**2+(ya_a(j)-ybh_b(ib2,jb2))**2)
            ds=one/(d(1)+d(2)+d(3)+d(4))
            fv3dy(i,j)=d(3)*ds
            fv3dy1(i,j)=d(4)*ds
            fv3dx(i,j)=d(1)*ds
            fv3dx1(i,j)=d(2)*ds
         endif
      endif !bilinear

     end do ! i
  end do ! j

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!1111
!!!!!compute A to fv3 grid interpolation parameters !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!1111
  allocate  (   p_fv3sar2grid%a3dx(ny,nx),p_fv3sar2grid%a3dx1(ny,nx),p_fv3sar2grid%a3dy(ny,nx),p_fv3sar2grid%a3dy1(ny,nx) )
  allocate  (   p_fv3sar2grid%a3ix(ny,nx),p_fv3sar2grid%a3ixp(ny,nx),p_fv3sar2grid%a3jy(ny,nx),p_fv3sar2grid%a3jyp(ny,nx) )
  a3dx =>p_fv3sar2grid%a3dx
  a3dx1=>p_fv3sar2grid%a3dx1
  a3dy =>p_fv3sar2grid%a3dy
  a3dy1=>p_fv3sar2grid%a3dy1

  a3ix =>p_fv3sar2grid%a3ix
  a3ixp =>p_fv3sar2grid%a3ixp
  a3jy =>p_fv3sar2grid%a3jy
  a3jyp=>p_fv3sar2grid%a3jyp
  
  do i=1,nx
     do j=1,ny
        gxa=xbh_b(i,j)
        if(gxa < xa_a(1))then
           gxa= 1
        else if(gxa > xa_a(nxout))then
           gxa= nxout
        else
           call grdcrd1(gxa,xa_a,nxout,1)
        endif
        a3ix(j,i)=int(gxa)
        a3ix(j,i)=min(max(1,a3ix(j,i)),nxout)
        a3dx(j,i)=max(zero,min(one,gxa-a3ix(j,i)))
        a3dx1(j,i)=one-a3dx(j,i)
        a3ixp(j,i)=min(nxout,a3ix(j,i)+1)
     end do
  end do

  do i=1,nx
    do j=1,ny
        gya=ybh_b(i,j)
        if(gya < ya_a(1))then
           gya= 1
        else if(gya > ya_a(nyout))then
           gya= nyout
        else
           call grdcrd1(gya,ya_a,nyout,1)
        endif
        a3jy(j,i)=int(gya)
        a3jy(j,i)=min(max(1,a3jy(j,i)),nyout)
        a3dy(j,i)=max(zero,min(one,gya-a3jy(j,i)))
        a3dy1(j,i)=one-a3dy(j,i)
        a3jyp(j,i)=min(ny,a3jy(j,i)+1)
     end do
  end do

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!! find coefficients for wind conversion btw FV3 & earth
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  allocate  (  p_fv3sar2grid%cangu(nx,ny+1),p_fv3sar2grid%sangu(nx,ny+1),p_fv3sar2grid%cangv(nx+1,ny),p_fv3sar2grid%sangv(nx+1,ny) )
   cangu=>p_fv3sar2grid%cangu
   sangu=>p_fv3sar2grid%sangu
   cangv=>p_fv3sar2grid%cangv
   sangv=>p_fv3sar2grid%sangv

!   1.  compute x,y,z at cell cornor from grid_lon, grid_lat

  do j=1,ny+1
     do i=1,nx+1
        x(i,j)=cos(grid_lat(i,j)*deg2rad)*cos(grid_lon(i,j)*deg2rad)
        y(i,j)=cos(grid_lat(i,j)*deg2rad)*sin(grid_lon(i,j)*deg2rad)
        z(i,j)=sin(grid_lat(i,j)*deg2rad)
     enddo
  enddo

!  2   find angles to E-W and N-S for U edges
  sq180=180._r_kind**2 
  do j=1,ny+1
     do i=1,nx
!      center lat/lon of the edge 
        rlat=half*(grid_lat(i,j)+grid_lat(i+1,j))
        diff=(grid_lon(i,j)-grid_lon(i+1,j))**2
        if(diff < sq180)then
           rlon=half*(grid_lon(i,j)+grid_lon(i+1,j))
        else
           rlon=half*(grid_lon(i,j)+grid_lon(i+1,j)-360._r_kind)
        endif
!    vector to center of the edge
        xr=cos(rlat*deg2rad)*cos(rlon*deg2rad)
        yr=cos(rlat*deg2rad)*sin(rlon*deg2rad)
        zr=sin(rlat*deg2rad)
!     vector of the edge
        xu= x(i+1,j)-x(i,j)
        yu= y(i+1,j)-y(i,j)
        zu= z(i+1,j)-z(i,j)
!    find angle with cross product
        uval=sqrt((xu**2+yu**2+zu**2))
        ewval=sqrt((xr**2+yr**2))
        nsval=sqrt((xr*zr)**2+(zr*yr)**2+(xr*xr+yr*yr)**2)
        cangu(i,j)=(-yr*xu+xr*yu)/ewval/uval
        sangu(i,j)=(-xr*zr*xu-zr*yr*yu+(xr*xr+yr*yr)*zu) / nsval/uval
     enddo
  enddo
 
!  3   find angles to E-W and N-S for V edges
  do j=1,ny
     do i=1,nx+1
        rlat=half*(grid_lat(i,j)+grid_lat(i,j+1))
        diff=(grid_lon(i,j)-grid_lon(i,j+1))**2
        if(diff < sq180)then
           rlon=half*(grid_lon(i,j)+grid_lon(i,j+1))
        else
           rlon=half*(grid_lon(i,j)+grid_lon(i,j+1)-360._r_kind)
        endif
        xr=cos(rlat*deg2rad)*cos(rlon*deg2rad)
        yr=cos(rlat*deg2rad)*sin(rlon*deg2rad)
        zr=sin(rlat*deg2rad)
        xv= x(i,j+1)-x(i,j)
        yv= y(i,j+1)-y(i,j)
        zv= z(i,j+1)-z(i,j)
        vval=sqrt((xv**2+yv**2+zv**2))
        ewval=sqrt((xr**2+yr**2))
        nsval=sqrt((xr*zr)**2+(zr*yr)**2+(xr*xr+yr*yr)**2)
        cangv(i,j)=(-yr*xv+xr*yv)/ewval/vval
        sangv(i,j)=(-xr*zr*xv-zr*yr*yv+(xr*xr+yr*yr)*zv) / nsval/vval
     enddo
  enddo
!clt following Hongli Wang's treatment
  if(fv3_regional_dd_reduce) then 
     !write(*,*)'testing Hongli code'
     if(nlon_fv3_regional_reduce.le.0.and.nlat_fv3_regional_reduce.le.0) then 
        nlon_fv3_regional_reduce=nint((maxval(gcrlon)-maxval(gcrlon(:,ny/2)))/adlon)
        j=nlon_fv3_regional_reduce
        nlat_fv3_regional_reduce=nint((gcrlat(j,ny)-minval(gcrlat))/adlat)
        if(nlon_fv3_regional_reduce.lt.0) nlon_fv3_regional_reduce=0
        if(nlat_fv3_regional_reduce.lt.0) nlat_fv3_regional_reduce=0
     endif
   endif
  deallocate( xc,yc,zc,gclat,gclon,gcrlat,gcrlon)
  deallocate(rlat_in,rlon_in)
end subroutine generate_regular_grids
subroutine definecoef_regular_grids(nx,ny,nxa_inout,nya_inout,grid_lon,grid_lont,grid_lat,grid_latt,p_fv3sar2grid,&
                             nlatin,nlonin,region_lat_in,region_lon_in)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    generate_??ens_grid
!clt modified from generate_regular_grid
!   prgmmr: parrish
!
! abstract:  define rotated lat-lon analysis grid which is centered on fv3 tile 
!             and oriented to completely cover the tile.
!
! program history log:
!   2017-05-02  parrish
!   2017-10-10  wu   - 1. setup analysis A-grid, 
!                      2. compute/setup FV3 to A grid interpolation parameters
!                      3. compute/setup A to FV3 grid interpolation parameters         
!                      4. setup weightings for wind conversion from FV3 to earth
!   2021-02-01 Lu & Wang - modify variable intent for HAFS dual ens. POC: xuguang.wang@ou.edu
!
!   input argument list:
!    nx, ny               - number of cells = nx*ny 
!    grid_lon ,grid_lat   - longitudes and latitudes of fv3 grid cell corners
!    grid_lont,grid_latt  - longitudes and latitudes of fv3 grid cell centers
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

  use kinds, only: r_kind,i_kind
  use constants, only: quarter,one,two,half,zero,deg2rad,rearth,rad2deg
  use gridmod,  only:grid_ratio_fv3_regional
  use mpimod, only: mype
  use gridmod,  only:init_general_transform 
  implicit none
  type (fv3sar2grid_parm),intent(inout):: p_fv3sar2grid
  real(r_kind),allocatable,intent(inout):: region_lat_in(:,:),region_lon_in(:,:)
  integer(i_kind), intent(inout):: nlatin,nlonin


  real(r_kind) ,pointer,dimension(:,:):: fv3dx,fv3dx1,fv3dy,fv3dy1
  integer(i_kind),pointer,dimension(:,:)::  fv3ix,fv3ixp,fv3jy,fv3jyp
  real(r_kind) ,pointer,dimension(:,:):: a3dx,a3dx1,a3dy,a3dy1
  real(r_kind) ,pointer,dimension(:,:):: cangu,sangu,cangv,sangv
  integer(i_kind),pointer,dimension(:,:)::  a3ix,a3ixp,a3jy,a3jyp

  real(r_kind),allocatable,dimension(:)::xbh_a,xa_a,xa_b
  real(r_kind),allocatable,dimension(:)::ybh_a,ya_a,ya_b,yy
  real(r_kind),allocatable,dimension(:,:)::xbh_b,ybh_b
  real(r_kind) dlat,dlon,dyy,dxx,dyyi,dxxi
  real(r_kind) dyyh,dxxh


  integer(i_kind), intent(in   ) :: nx,ny                 ! fv3 tile x- and y-dimensions
  integer(i_kind), intent(inout   ) :: nxa_inout,nya_inout 
  real(r_kind)   , intent(inout) :: grid_lon(nx+1,ny+1)   ! fv3 cell corner longitudes
  real(r_kind)   , intent(inout) :: grid_lont(nx,ny)      ! fv3 cell center longitudes
  real(r_kind)   , intent(inout) :: grid_lat(nx+1,ny+1)   ! fv3 cell corner latitudes
  real(r_kind)   , intent(inout) :: grid_latt(nx,ny)      ! fv3 cell center latitudes
  logical :: bilinear
  integer(i_kind) i,j,ir,jr,n
  real(r_kind),allocatable,dimension(:,:) :: xc,yc,zc,gclat,gclon,gcrlat,gcrlon,rlon_in,rlat_in
  real(r_kind),allocatable,dimension(:,:) :: glon_an,glat_an
  real(r_kind) xcent,ycent,zcent,rnorm,centlat,centlon
  real(r_kind) adlon,adlat,alon,clat,clon
  integer(i_kind) nxout,nyout
  integer(i_kind) nlonh,nlath,nxh,nyh
  integer(i_kind) ib1,ib2,jb1,jb2,jj
  integer (i_kind):: index0

  integer(i_kind) nord_e2a
  real(r_kind)gxa,gya

  real(r_kind) x(nx+1,ny+1),y(nx+1,ny+1),z(nx+1,ny+1), xr,yr,zr,xu,yu,zu,rlat,rlon
  real(r_kind) xv,yv,zv,vval
  real(r_kind) cx,cy
  real(r_kind) uval,ewval,nsval

  real(r_kind) d(4),ds
  integer(i_kind) kk,k


  nord_e2a=4
  bilinear=.false.
  p_fv3sar2grid%bilinear=bilinear


!   create xc,yc,zc for the cell centers.
  allocate(xc(nx,ny))
  allocate(yc(nx,ny))
  allocate(zc(nx,ny))
  allocate(gclat(nx,ny))
  allocate(gclon(nx,ny))
  allocate(gcrlat(nx,ny))
  allocate(gcrlon(nx,ny))
  do j=1,ny
     do i=1,nx
        xc(i,j)=cos(grid_latt(i,j)*deg2rad)*cos(grid_lont(i,j)*deg2rad)
        yc(i,j)=cos(grid_latt(i,j)*deg2rad)*sin(grid_lont(i,j)*deg2rad)
        zc(i,j)=sin(grid_latt(i,j)*deg2rad)
     enddo
  enddo

!  compute center as average x,y,z coordinates of corners of domain --

  xcent=quarter*(xc(1,1)+xc(1,ny)+xc(nx,1)+xc(nx,ny))
  ycent=quarter*(yc(1,1)+yc(1,ny)+yc(nx,1)+yc(nx,ny))
  zcent=quarter*(zc(1,1)+zc(1,ny)+zc(nx,1)+zc(nx,ny))

  rnorm=one/sqrt(xcent**2+ycent**2+zcent**2)
  xcent=rnorm*xcent
  ycent=rnorm*ycent
  zcent=rnorm*zcent
  centlat=asin(zcent)*rad2deg
  centlon=atan2(ycent,xcent)*rad2deg

!!  compute new lats, lons
  call rotate2deg(grid_lont,grid_latt,gcrlon,gcrlat, &
                  centlon,centlat,nx,ny)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!  compute analysis A-grid  lats, lons
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!--------------------------obtain analysis grid dimensions nxout,nyout
  p_fv3sar2grid%nx=nx
  p_fv3sar2grid%ny=ny
  nxout=nlonin
  p_fv3sar2grid%nxout=nxout
  nyout=nlatin
  p_fv3sar2grid%nyout=nyout
  nxa_inout=nxout
  nya_inout=nyout  ! for compatiability 
  if(mype==0) print *,'nlatin,nlonin = ',nlatin,nlonin

!--------------------------obtain analysis grid spacing
  dlat=(maxval(gcrlat)-minval(gcrlat))/(ny-1)
  dlon=(maxval(gcrlon)-minval(gcrlon))/(nx-1)
  adlat=dlat*grid_ratio_fv3_regional
  adlon=dlon*grid_ratio_fv3_regional

!-------setup analysis A-grid; find center of the domain
  nlonh=nlonin/2
  nlath=nlatin/2

  if(nlonh*2==nlonin)then
     clon=adlon/two
     cx=half
  else
     clon=adlon
     cx=one
  endif

  if(nlath*2==nlatin)then
     clat=adlat/two
     cy=half
  else
     clat=adlat
     cy=one
  endif
  allocate(rlat_in(nlatin,nlonin),rlon_in(nlatin,nlonin))
  call rotate2deg(region_lon_in,region_lat_in,rlon_in,rlat_in, &
                    clon,clat,nlatin,nlonin)

!
!-----setup analysis A-grid from center of the domain
!
!--------------------compute all combinations of relative coordinates

  allocate(xbh_a(nx),xbh_b(nx,ny),xa_a(nxout),xa_b(nxout))
  allocate(ybh_a(ny),ybh_b(nx,ny),ya_a(nyout),ya_b(nyout))

  nxh=nx/2
  nyh=ny/2

!!!!!! fv3 rotated grid; not equal spacing, non_orthogonal !!!!!!
  do j=1,ny
     jr=ny+1-j
     do i=1,nx
        ir=nx+1-i
        xbh_b(ir,jr)=gcrlon(i,j)/dlon
     end do
  end do
  do j=1,ny
     jr=ny+1-j
     do i=1,nx
       ir=nx+1-i
       ybh_b(ir,jr)=gcrlat(i,j)/dlat
     end do
  end do

!!!!  define analysis A grid  !!!!!!!!!!!!!



  do j=1,nxout
     xa_a(j)=(float(j-nlonh)-cx)*grid_ratio_fv3_regional
  end do
  do i=1,nyout
     ya_a(i)=(float(i-nlath)-cy)*grid_ratio_fv3_regional
  end do

  index0=1
!cltthinkdeb should region_lon_in be in degree or not?
  do j=1,nxa_inout
     xa_a(j)= (rlon_in(index0,j)-clon)/dlon
  end do
  do i=1,nya_inout
     ya_a(i)= (rlat_in(i,index0)-clat)/dlon
  end do

!!!!!compute fv3 to A grid interpolation parameters !!!!!!!!!
  allocate  (   p_fv3sar2grid%fv3dx(nxout,nyout),p_fv3sar2grid%fv3dx1(nxout,nyout),p_fv3sar2grid%fv3dy(nxout,nyout),p_fv3sar2grid%fv3dy1(nxout,nyout) )
  allocate  (   p_fv3sar2grid%fv3ix(nxout,nyout),p_fv3sar2grid%fv3ixp(nxout,nyout),p_fv3sar2grid%fv3jy(nxout,nyout),p_fv3sar2grid%fv3jyp(nxout,nyout) )
  fv3dx=>p_fv3sar2grid%fv3dx
  fv3dx1=>p_fv3sar2grid%fv3dx1
  fv3dy=>p_fv3sar2grid%fv3dy
  fv3dy1=>p_fv3sar2grid%fv3dy1
  fv3ix=>p_fv3sar2grid%fv3ix
  fv3ixp=>p_fv3sar2grid%fv3ixp
  fv3jy=>p_fv3sar2grid%fv3jy
  fv3jyp=>p_fv3sar2grid%fv3jyp
  
  allocate(yy(ny))

! iteration to find the fv3 grid cell
  jb1=1
  ib1=1
  do j=1,nyout
     do i=1,nxout
      do n=1,3 
         gxa=xa_a(i)
         if(gxa < xbh_b(1,jb1))then
            gxa= 1
         else if(gxa > xbh_b(nx,jb1))then
            gxa= nx
         else
            call grdcrd1(gxa,xbh_b(1,jb1),nx,1)
         endif
         ib2=ib1
         ib1=gxa
         do jj=1,ny
            yy(jj)=ybh_b(ib1,jj)
         enddo
         gya=ya_a(j)
         if(gya < yy(1))then
            gya= 1
         else if(gya > yy(ny))then
            gya= ny
         else
            call grdcrd1(gya,yy,ny,1)
         endif
         jb2=jb1
         jb1=gya

         if((ib1 == ib2) .and. (jb1 == jb2)) exit
         if(n==3 ) then     
!!!!!!!   if not converge, find the nearest corner point
            d(1)=(xa_a(i)-xbh_b(ib1,jb1))**2+(ya_a(j)-ybh_b(ib1,jb1))**2
            d(2)=(xa_a(i)-xbh_b(ib1+1,jb1))**2+(ya_a(j)-ybh_b(ib1+1,jb1))**2
            d(3)=(xa_a(i)-xbh_b(ib1,jb1+1))**2+(ya_a(j)-ybh_b(ib1,jb1+1))**2
            d(4)=(xa_a(i)-xbh_b(ib1+1,jb1+1))**2+(ya_a(j)-ybh_b(ib1+1,jb1+1))**2
            kk=1 
            do k=2,4
               if(d(k)<d(kk))kk=k
            enddo
!!!!!!!!!!! Find the cell for interpolation
            gxa=xa_a(i)
            gya=ya_a(j)
            if(kk==1)then
               call grdcrd1(gxa,xbh_b(1,jb1),nx,1)
               do jj=1,ny
                  yy(jj)=ybh_b(ib1,jj)
               enddo
               call grdcrd1(gya,yy,ny,1)
            else if(kk==2)then
               call grdcrd1(gxa,xbh_b(1,jb1),nx,1)
               do jj=1,ny
                  yy(jj)=ybh_b(ib1+1,jj)
               enddo
               call grdcrd1(gya,yy,ny,1)
            else if(kk==3)then
               call grdcrd1(gxa,xbh_b(1,jb1+1),nx,1)
               do jj=1,ny
                  yy(jj)=ybh_b(ib1,jj)
               enddo
               call grdcrd1(gya,yy,ny,1)
            else if(kk==4)then
               call grdcrd1(gxa,xbh_b(1,jb1+1),nx,1)
               do jj=1,ny
                  yy(jj)=ybh_b(ib1+1,jj)
               enddo
               call grdcrd1(gya,yy,ny,1)
            endif
            exit
         endif  !n=3   
      enddo  ! n

      fv3ix(i,j)=int(gxa)
      fv3ix(i,j)=min(max(1,fv3ix(i,j)),nx)
      fv3ixp(i,j)=min(nx,fv3ix(i,j)+1)
      fv3jy(i,j)=int(gya)
      fv3jy(i,j)=min(max(1,fv3jy(i,j)),ny)
      fv3jyp(i,j)=min(ny,fv3jy(i,j)+1)

      if(bilinear)then
         fv3dy(i,j)=max(zero,min(one,gya-fv3jy(i,j)))
         fv3dy1(i,j)=one-fv3dy(i,j)
         fv3dx(i,j)=max(zero,min(one,gxa-fv3ix(i,j)))
         fv3dx1(i,j)=one-fv3dx(i,j)
      else ! inverse-distance weighting average 
         ib1=fv3ix(i,j)
         ib2=fv3ixp(i,j)
         jb1=fv3jy(i,j)
         jb2=fv3jyp(i,j)
         if(xa_a(i)==xbh_b(ib1,jb1) .and. ya_a(j)==ybh_b(ib1,jb1))then
            fv3dy(i,j)=zero
            fv3dy1(i,j)=zero
            fv3dx(i,j)=one
            fv3dx1(i,j)=zero
         else if(xa_a(i)==xbh_b(ib2,jb1) .and. ya_a(j)==ybh_b(ib2,jb1))then
            fv3dy(i,j)=zero
            fv3dy1(i,j)=zero
            fv3dx(i,j)=zero
            fv3dx1(i,j)=one
         else if(xa_a(i)==xbh_b(ib1,jb2) .and. ya_a(j)==ybh_b(ib1,jb2))then
            fv3dy(i,j)=one 
            fv3dy1(i,j)=zero
            fv3dx(i,j)=zero
            fv3dx1(i,j)=zero
         else if(xa_a(i)==xbh_b(ib2,jb2) .and. ya_a(j)==ybh_b(ib2,jb2))then
            fv3dy(i,j)=zero
            fv3dy1(i,j)=one 
            fv3dx(i,j)=zero
            fv3dx1(i,j)=zero
         else 
            d(1)=one/((xa_a(i)-xbh_b(ib1,jb1))**2+(ya_a(j)-ybh_b(ib1,jb1))**2)
            d(2)=one/((xa_a(i)-xbh_b(ib2,jb1))**2+(ya_a(j)-ybh_b(ib2,jb1))**2)
            d(3)=one/((xa_a(i)-xbh_b(ib1,jb2))**2+(ya_a(j)-ybh_b(ib1,jb2))**2)
            d(4)=one/((xa_a(i)-xbh_b(ib2,jb2))**2+(ya_a(j)-ybh_b(ib2,jb2))**2)
            ds=one/(d(1)+d(2)+d(3)+d(4))
            fv3dy(i,j)=d(3)*ds
            fv3dy1(i,j)=d(4)*ds
            fv3dx(i,j)=d(1)*ds
            fv3dx1(i,j)=d(2)*ds
         endif
      endif !bilinear

     end do ! i
  end do ! j

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!1111
!!!!!compute A to fv3 grid interpolation parameters !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!1111
  allocate  (   p_fv3sar2grid%a3dx(ny,nx),p_fv3sar2grid%a3dx1(ny,nx),p_fv3sar2grid%a3dy(ny,nx),p_fv3sar2grid%a3dy1(ny,nx) )
  allocate  (   p_fv3sar2grid%a3ix(ny,nx),p_fv3sar2grid%a3ixp(ny,nx),p_fv3sar2grid%a3jy(ny,nx),p_fv3sar2grid%a3jyp(ny,nx) )
  a3dx =>p_fv3sar2grid%a3dx
  a3dx1=>p_fv3sar2grid%a3dx1
  a3dy =>p_fv3sar2grid%a3dy
  a3dy1=>p_fv3sar2grid%a3dy1

  a3ix =>p_fv3sar2grid%a3ix
  a3ixp =>p_fv3sar2grid%a3ixp
  a3jy =>p_fv3sar2grid%a3jy
  a3jyp=>p_fv3sar2grid%a3jyp
  
  do i=1,nx
     do j=1,ny
        gxa=xbh_b(i,j)
        if(gxa < xa_a(1))then
           gxa= 1
        else if(gxa > xa_a(nxout))then
           gxa= nxout
        else
           call grdcrd1(gxa,xa_a,nxout,1)
        endif
        a3ix(j,i)=int(gxa)
        a3ix(j,i)=min(max(1,a3ix(j,i)),nxout)
        a3dx(j,i)=max(zero,min(one,gxa-a3ix(j,i)))
        a3dx1(j,i)=one-a3dx(j,i)
        a3ixp(j,i)=min(nxout,a3ix(j,i)+1)
     end do
  end do

  do i=1,nx
    do j=1,ny
        gya=ybh_b(i,j)
        if(gya < ya_a(1))then
           gya= 1
        else if(gya > ya_a(nyout))then
           gya= nyout
        else
           call grdcrd1(gya,ya_a,nyout,1)
        endif
        a3jy(j,i)=int(gya)
        a3jy(j,i)=min(max(1,a3jy(j,i)),nyout)
        a3dy(j,i)=max(zero,min(one,gya-a3jy(j,i)))
        a3dy1(j,i)=one-a3dy(j,i)
        a3jyp(j,i)=min(ny,a3jy(j,i)+1)
     end do
  end do

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!! find coefficients for wind conversion btw FV3 & earth
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  allocate  (  p_fv3sar2grid%cangu(nx,ny+1),p_fv3sar2grid%sangu(nx,ny+1),p_fv3sar2grid%cangv(nx+1,ny),p_fv3sar2grid%sangv(nx+1,ny) )
   cangu=>p_fv3sar2grid%cangu
   sangu=>p_fv3sar2grid%sangu
   cangv=>p_fv3sar2grid%cangv
   sangv=>p_fv3sar2grid%sangv

!   1.  compute x,y,z at cell cornor from grid_lon, grid_lat

  do j=1,ny+1
     do i=1,nx+1
        x(i,j)=cos(grid_lat(i,j)*deg2rad)*cos(grid_lon(i,j)*deg2rad)
        y(i,j)=cos(grid_lat(i,j)*deg2rad)*sin(grid_lon(i,j)*deg2rad)
        z(i,j)=sin(grid_lat(i,j)*deg2rad)
     enddo
  enddo

!  2   find angles to E-W and N-S for U edges
 
  do j=1,ny+1
     do i=1,nx
!      center lat/lon of the edge 
        rlat=half*(grid_lat(i,j)+grid_lat(i+1,j))
        rlon=half*(grid_lon(i,j)+grid_lon(i+1,j))
!    vector to center of the edge
        xr=cos(rlat*deg2rad)*cos(rlon*deg2rad)
        yr=cos(rlat*deg2rad)*sin(rlon*deg2rad)
        zr=sin(rlat*deg2rad)
!     vector of the edge
        xu= x(i+1,j)-x(i,j)
        yu= y(i+1,j)-y(i,j)
        zu= z(i+1,j)-z(i,j)
!    find angle with cross product
        uval=sqrt((xu**2+yu**2+zu**2))
        ewval=sqrt((xr**2+yr**2))
        nsval=sqrt((xr*zr)**2+(zr*yr)**2+(xr*xr+yr*yr)**2)
        cangu(i,j)=(-yr*xu+xr*yu)/ewval/uval
        sangu(i,j)=(-xr*zr*xu-zr*yr*yu+(xr*xr+yr*yr)*zu) / nsval/uval
     enddo
  enddo
 
!  3   find angles to E-W and N-S for V edges
  do j=1,ny
     do i=1,nx+1
        rlat=half*(grid_lat(i,j)+grid_lat(i,j+1))
        rlon=half*(grid_lon(i,j)+grid_lon(i,j+1))
        xr=cos(rlat*deg2rad)*cos(rlon*deg2rad)
        yr=cos(rlat*deg2rad)*sin(rlon*deg2rad)
        zr=sin(rlat*deg2rad)
        xv= x(i,j+1)-x(i,j)
        yv= y(i,j+1)-y(i,j)
        zv= z(i,j+1)-z(i,j)
        vval=sqrt((xv**2+yv**2+zv**2))
        ewval=sqrt((xr**2+yr**2))
        nsval=sqrt((xr*zr)**2+(zr*yr)**2+(xr*xr+yr*yr)**2)
        cangv(i,j)=(-yr*xv+xr*yv)/ewval/vval
        sangv(i,j)=(-xr*zr*xv-zr*yr*yv+(xr*xr+yr*yr)*zv) / nsval/vval
     enddo
  enddo
  deallocate( xc,yc,zc,gclat,gclon,gcrlat,gcrlon)
  deallocate(rlat_in,rlon_in)
end subroutine definecoef_regular_grids
subroutine earthuv2fv3_regular_grids(u,v,nx,ny,u_out,v_out,p_fv3sar2grid)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    earthuv2fv3
!   prgmmr: wu                      2017-06-15
!
! abstract: project earth UV to fv3 UV and interpolate to edge of the cell
!
! program history log:
!   
!
!   input argument list:
!    u,v -  earth wind components at center of the cell
!    nx,ny - dimensions
!
!   output argument list:
!    u_out,v_out - output fv3 winds on the cell boundaries
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block  

  use kinds, only: r_kind,i_kind
  use constants, only: half
  implicit none

  integer(i_kind), intent(in   ) :: nx,ny                 ! fv3 tile x- and y-dimensions
  type (fv3sar2grid_parm),intent(inout):: p_fv3sar2grid
  real(r_kind),intent(in   ) :: u(nx,ny),v(nx,ny)
  real(r_kind),intent(  out) :: u_out(nx,ny+1),v_out(nx+1,ny)
  integer(i_kind) i,j
  real(r_kind) ,pointer,dimension(:,:):: cangu,sangu,cangv,sangv
   cangu=>p_fv3sar2grid%cangu
   sangu=>p_fv3sar2grid%sangu
   cangv=>p_fv3sar2grid%cangv
   sangv=>p_fv3sar2grid%sangv
  

!!!!!!! earth u/v to covariant u/v
  j=1
  do i=1,nx
     u_out(i,j)= u(i,j)*cangu(i,j)+v(i,j)*sangu(i,j)
  end do

  do j=2,ny
     do i=1,nx
        u_out(i,j)=half   *( (u(i,j)+u(i,j-1))*cangu(i,j)+(v(i,j)+v(i,j-1))*sangu(i,j) )
     end do
  end do
  j=ny
  do i=1,nx
     u_out(i,j+1)= u(i,j)*cangu(i,j+1)+v(i,j)*sangu(i,j+1)
  end do

  do j=1,ny
     v_out(1,j)=u(1,j)*cangv(1,j)+v(1,j)*sangv(1,j)
     do i=2,nx
        v_out(i,j)=half   *( (u(i,j)+u(i-1,j))*cangv(i,j)+(v(i,j)+v(i-1,j))*sangv(i,j) )
     end do
     v_out(nx+1,j)=u(nx,j)*cangv(nx+1,j)+v(nx,j)*sangv(nx+1,j)
  end do
end subroutine earthuv2fv3_regular_grids

subroutine fv3uv2earth_regular_grids(u,v,nx,ny,u_out,v_out,p_fv3sar2grid)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    fv3uv2earth
!   prgmmr: wu                      2017-06-15
!
! abstract: project fv3 UV to earth UV and interpolate to the center of the cells
!
! program history log:
!   
!
!   input argument list:
!    u,v - fv3 winds on the cell boundaries
!    nx,ny - dimensions
!
!   output argument list:
!    u_out,v_out - output earth wind components at center of the cell
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block  

  use kinds, only: r_kind,i_kind
  use constants, only: half
  implicit none
   
  type (fv3sar2grid_parm),intent(inout):: p_fv3sar2grid
  integer(i_kind), intent(in   ) :: nx,ny                 ! fv3 tile x- and y-dimensions
  real(r_kind),intent(in   ) :: u(nx,ny+1),v(nx+1,ny)
  real(r_kind),intent(  out) :: u_out(nx,ny),v_out(nx,ny)
  integer(i_kind) i,j
  real(r_kind) ,pointer,dimension(:,:):: cangu,sangu,cangv,sangv
   cangu=>p_fv3sar2grid%cangu
   sangu=>p_fv3sar2grid%sangu
   cangv=>p_fv3sar2grid%cangv
   sangv=>p_fv3sar2grid%sangv

  do j=1,ny
     do i=1,nx
        u_out(i,j)=half *( (u(i,j)*sangv(i,j)-v(i,j)*sangu(i,j))/(cangu(i,j)*sangv(i,j)-sangu(i,j)*cangv(i,j)) &
                       +(u(i,j+1)*sangv(i+1,j)-v(i+1,j)*sangu(i,j+1))/(cangu(i,j+1)*sangv(i+1,j)-sangu(i,j+1)*cangv(i+1,j)))
        v_out(i,j)=half *( (u(i,j)*cangv(i,j)-v(i,j)*cangu(i,j))/(sangu(i,j)*cangv(i,j)-cangu(i,j)*sangv(i,j)) &
                       +(u(i,j+1)*cangv(i+1,j)-v(i+1,j)*cangu(i,j+1))/(sangu(i,j+1)*cangv(i+1,j)-cangu(i,j+1)*sangv(i+1,j)))
     end do
  end do
  return
end subroutine fv3uv2earth_regular_grids
subroutine fv3_h_to_ll_regular_grids(b_in,a,nb,mb,na,ma,p_fv3sar2grid)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    fv3_h_to_ll
!   prgmmr: wu                      2017-05-30
!
! abstract: interpolate from rotated fv3 grid to A grid. 
!     Interpolation choices 1)bilinear both ways 
!                           2)inverse-distance weighting average
!     reverse E-W and N-S directions & reverse i,j for output array a(nlat,nlon)
!
! program history log:
!   
!
!   input argument list:
!    mb,nb - fv3 dimensions
!    ma,na - a dimensions
!    b     - input variable b
!    xb,yb - b array x and y coordinates
!    xa,ya - a array coordinates (xa in xb units, ya in yb units)
!
!   output argument list:
!    a     - output interpolated array
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block  

  use constants, only: zero,one
  implicit none
 
  type (fv3sar2grid_parm),intent(inout):: p_fv3sar2grid
  integer(i_kind),intent(in   ) :: mb,nb,ma,na
  real(r_kind)   ,intent(in   ) :: b_in(nb,mb)
  real(r_kind)   ,intent(  out) :: a(ma,na)

  real(r_kind) ,pointer,dimension(:,:):: fv3dx,fv3dx1,fv3dy,fv3dy1
  integer(i_kind),pointer,dimension(:,:)::  fv3ix,fv3ixp,fv3jy,fv3jyp
  logical :: bilinear
  integer(i_kind) i,j,ir,jr,mbp,nbp
  real(r_kind)    b(nb,mb)
  
  bilinear= p_fv3sar2grid%bilinear
  fv3dx=>p_fv3sar2grid%fv3dx
  fv3dx1=>p_fv3sar2grid%fv3dx1
  fv3dy=>p_fv3sar2grid%fv3dy
  fv3dy1=>p_fv3sar2grid%fv3dy1
  fv3ix=>p_fv3sar2grid%fv3ix
  fv3ixp=>p_fv3sar2grid%fv3ixp
  fv3jy=>p_fv3sar2grid%fv3jy
  fv3jyp=>p_fv3sar2grid%fv3jyp
  
!!!!!!!!! reverse E-W and N-S
  mbp=mb+1
  nbp=nb+1
  do j=1,mb
     jr=mbp-j
     do i=1,nb
        ir=nbp-i
        b(ir,jr)=b_in(i,j)
     end do
  end do
!!!!!!!!! interpolate to A grid & reverse ij for array a(lat,lon)
  if(bilinear)then ! bilinear interpolation
     do j=1,ma
        do i=1,na
           a(j,i)=fv3dx1(i,j)*(fv3dy1(i,j)*b(fv3ix (i,j),fv3jy(i,j))+fv3dy(i,j)*b(fv3ix (i,j),fv3jyp(i,j))) &
              +fv3dx (i,j)*(fv3dy1(i,j)*b(fv3ixp(i,j),fv3jy(i,j))+fv3dy(i,j)*b(fv3ixp(i,j),fv3jyp(i,j)))
        end do
     end do
  else  ! inverse-distance weighting average 
     do j=1,ma
        do i=1,na
           a(j,i)=fv3dx(i,j)*b(fv3ix (i,j),fv3jy(i,j))+fv3dy(i,j)*b(fv3ix (i,j),fv3jyp(i,j)) &
              +fv3dx1(i,j)*b(fv3ixp(i,j),fv3jy(i,j))+fv3dy1(i,j)*b(fv3ixp(i,j),fv3jyp(i,j))
        end do
     end do
  endif
  return
end subroutine fv3_h_to_ll_regular_grids

subroutine fv3_ll_to_h_regular_grids(a,b,nxa,nya,nxb,nyb,rev_flg,p_fv3sar2grid)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    fv3_ll_to_h
!   prgmmr: wu                      2017-05-30
!
! abstract: interpolate from analysis A grid to rotated fv3 grid. 
!    Interpolation is bilinear both ways.  Reverse E-W and N-S and
!     reverse i,j for output array b(nxb,nyb)
!
! program history log:
!   
!
!   input argument list:
!    nxa,nya - a array dimensions
!    nxb,nyb - b array dimensions
!
!    b     - input variable b
!    rev_flg - flag for reverse i,j order
!   output argument list:
!    a     - output interpolated array
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block  

  use kinds, only: r_kind,i_kind
  use constants, only: zero,one
  implicit none

  integer(i_kind),intent(in   ) :: nyb,nxb,nya,nxa
  real(r_kind)   ,intent(in   ) :: a(nya,nxa)
  logical        ,intent(in   ) :: rev_flg 
  real(r_kind)   ,intent(  out) :: b(nxb*nyb)
  type (fv3sar2grid_parm),intent(inout):: p_fv3sar2grid
  real(r_kind) ,pointer,dimension(:,:):: a3dx,a3dx1,a3dy,a3dy1
  integer(i_kind),pointer,dimension(:,:)::  a3ix,a3ixp,a3jy,a3jyp
  logical:: linear

  integer(i_kind) i,j,ir,jr,nybp,nxbp,ijr

  a3dx =>p_fv3sar2grid%a3dx
  a3dx1=>p_fv3sar2grid%a3dx1
  a3dy =>p_fv3sar2grid%a3dy
  a3dy1=>p_fv3sar2grid%a3dy1

  a3ix =>p_fv3sar2grid%a3ix
  a3ixp =>p_fv3sar2grid%a3ixp
  a3jy =>p_fv3sar2grid%a3jy
  a3jyp=>p_fv3sar2grid%a3jyp

  if(rev_flg)then
!!!!!!!!!! output in reverse E-W, N-S and reversed i,j !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
     nybp=nyb+1
     nxbp=nxb+1
     do i=1,nyb
        ir=nybp-i
        ijr=(ir-1)*nxb
        do j=1,nxb
           jr=nxbp-j
           b(jr+ijr)=a3dy1(i,j)*(a3dx1(i,j)*a(a3jy (i,j),a3ix(i,j))+a3dx(i,j)*a(a3jy (i,j),a3ixp(i,j))) &
             +a3dy (i,j)*(a3dx1(i,j)*a(a3jyp(i,j),a3ix(i,j))+a3dx(i,j)*a(a3jyp(i,j),a3ixp(i,j)))
        end do
     end do
  else
!!!!!!!!!! output order as input W-E S-N and (i:lat,j:lon) !!!!!!!!!!!
     do j=1,nxb
        ijr=(j-1)*nyb
        do i=1,nyb
           b(i+ijr)=a3dy1(i,j)*(a3dx1(i,j)*a(a3jy (i,j),a3ix(i,j))+a3dx(i,j)*a(a3jy (i,j),a3ixp(i,j))) &
             +a3dy (i,j)*(a3dx1(i,j)*a(a3jyp(i,j),a3ix(i,j))+a3dx(i,j)*a(a3jyp(i,j),a3ixp(i,j)))
        end do
     end do
  endif
end subroutine fv3_ll_to_h_regular_grids
  
  
end module mod_fv3_lolgrid
