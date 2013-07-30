subroutine grdsphdp(a,dadx,dady)
  use kinds, only: r_double
  use variables, only: zero,nlon,nlat,coef,noq
  implicit none

  integer nx,ny,nxh,nbp,nya,nxa,lacox1,lbcox1,lacox2,lbcox2,lacoy1,lbcoy1
  integer lbcoy2,lacoy2,lcy,iy,ix,ni1,ni2,kk,i,j
  real(r_double) cy
  real(r_double),dimension(nlat,nlon),intent(in):: a
  real(r_double),dimension(nlat,nlon),intent(out):: dadx,dady
  real(r_double),dimension(nlat-2,nlon):: work1,grid1,grid2

! Set parameters for calls to subsequent routines
  ny=nlat-2
  nxh=nlon/2
  nbp=2*noq+1
  nya=ny*nbp
  nxa=nxh*nbp

  lacox1=1
  lbcox1=lacox1+nxa
  lacox2=lbcox1+nxa
  lbcox2=lacox2+nxa
  lacoy1=lbcox2+nxa
  lbcoy1=lacoy1+nya
  lacoy2=lbcoy1+nya
  lbcoy2=lacoy2+nya
  lcy   =lbcoy2+nya-1


! Initialize output arrays to zero
  do j=1,nlon
     do i=1,nlat
        dadx(i,j)=zero
        dady(i,j)=zero
     end do
  end do

! Transfer scalar input field to work array.
! Zero other work arrays.
  do j=1,nlon
     do i=1,ny
        work1(i,j) = a(i+1,j)
        grid1(i,j)=zero
        grid2(i,j)=zero
     end do
  end do


! Compute x (east-west) derivatives on sphere
  call xdcirdp(work1,grid1, &
       coef(lacox1),coef(lbcox1),coef(lacox2),coef(lbcox2),&
       nlon,ny,noq,nxh)

! Compute y (south-north) derivatives on sphere
  call ydsphdp(work1,grid2, &
       coef(lacoy1),coef(lbcoy1),coef(lacoy2),coef(lbcoy2),&
       nlon,ny,noq)

! Make corrections for convergence of meridians:
  do ix=1,nlon
     do iy=1,ny
        grid1(iy,ix)=grid1(iy,ix)*coef(lcy+iy)
     end do
  end do


! Load results into ouptut arrays
  do j=1,nlon
     do i=1,ny
        dadx(i+1,j) = grid1(i,j)
        dady(i+1,j) = grid2(i,j)
     end do
  end do

  return
  end subroutine grdsphdp

subroutine divsphdp(uin,vin,div)

  use kinds, only: r_double
  use variables, only: nlon,nlat,zero,noq,coef
  implicit none

  real(r_double),dimension(nlat,nlon),intent(in):: uin,vin
  real(r_double),dimension(nlat,nlon),intent(out):: div
  integer nx,ny,nxh,nbp,nya,nxa,lacox1,lbcox1,lacox2,lbcox2,lacoy1,lbcoy1
  integer lbcoy2,lacoy2,lcy,iy,ix,ni1,ni2,kk,i,j
  real(r_double),dimension(nlat-2,nlon):: work1,work2,grid1,grid2

! Set parameters for calls to subsequent routines
  ny=nlat-2
  nxh=nlon/2
  nbp=2*noq+1
  nya=ny*nbp
  nxa=nxh*nbp

  lacox1=1
  lbcox1=lacox1+nxa
  lacox2=lbcox1+nxa
  lbcox2=lacox2+nxa
  lacoy1=lbcox2+nxa
  lbcoy1=lacoy1+nya
  lacoy2=lbcoy1+nya
  lbcoy2=lacoy2+nya
  lcy   =lbcoy2+nya-1

! Initialize output arrays to zero
  do j=1,nlon
     do i=1,nlat
        div(i,j)=zero
     end do
  end do

! Transfer scalar input field to work array.
! Zero other work arrays.
  do j=1,nlon
     do i=1,ny
        work1(i,j) = uin(i+1,j)
        work2(i,j) = vin(i+1,j)
        grid1(i,j)=zero
        grid2(i,j)=zero
     end do
  end do

! Compute x (east-west) derivatives on sphere of u-wind
  call xdcirdp(work1,grid1, &
       coef(lacox1),coef(lbcox1),coef(lacox2),coef(lbcox2),&
       nlon,ny,noq,nxh)

! Make corrections for convergence of meridians:
  do ix=1,nlon
     do iy=1,ny
        grid1(iy,ix)=grid1(iy,ix)*coef(lcy+iy)
     end do
  end do

! Compute y (south-north) derivatives on sphere of v-wnd

!  first multiply by cos(lat)
  do ix=1,nlon
     do iy=1,ny
        work2(iy,ix)=work2(iy,ix)/coef(lcy+iy)
     end do
  end do

  call ydsphdp(work2,grid2, &
       coef(lacoy1),coef(lbcoy1),coef(lacoy2),coef(lbcoy2),&
       nlon,ny,noq)

! Make corrections for convergence of meridians:
  do ix=1,nlon
     do iy=1,ny
        grid2(iy,ix)=grid2(iy,ix)*coef(lcy+iy)
     end do
  end do

! Load results into ouptut arrays
  do j=1,nlon
     do i=1,ny
        div(i+1,j)=grid1(i,j)+grid2(i,j)
     end do
  end do

  return
end subroutine divsphdp

  subroutine xdcirdp(p,q,aco1,bco1,aco2,bco2,nx,ny,noq,nxh)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    xdcirdp               compute x derivatives on sphere 
!   prgmmr: purser           org: np20               date:  1994-01-01
!
! abstract:  compute the x-derivatives of data with circle topology 
!            for rows using compact-differencing and add to existing
!            an field.		       
!									       
! program history log:
!   1994-05-12  parrish,d. elimanate memory bank conflicts
!   2004-07-27  treadon - add intent in/out
!
!   input argument list:
!     p      - array of input data					       
!     aco1   - array containing the "a-coefficients", in the format of    
!              a banded l-d-u factorization, for the antisymmetric portion of   
!              the field to be differenced (initialized in cdcoef) 	    
!     bco1   - corresponding band-matrix of "b-coefficients"	   
!     aco2   - like aco1, but for the symmetric portion of the data	  
!     bco2   - like bco1, but for the symmetric portion of the data	 
!     nx     - number of points in a cyclic-row (x-direction) 	
!     ny     - number of parallel rows				
!     noq    - quarter of the order of differencing (1 implies 4th-order)
!     nxh    - one half of nx				       
!
!   output argument list:
!     q      - array of derivatives are added		   	       
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp
!$$$
  use kinds, only: r_double
  implicit none

! Declare passed variables
  integer,intent(in):: nx,ny,noq,nxh
  real(r_double),dimension(ny,nx),intent(in):: p
  real(r_double),dimension(nxh,-noq:noq),intent(in):: aco1,bco1,aco2,bco2
  real(r_double),dimension(ny,nx),intent(out):: q

! Declare local variables
  integer nxhp,ix,iy,nxp,ix1,ix2
  real(r_double),dimension(ny,nx):: v1,v2

  nxhp=nxh+1
  nxp=nx+1

!  treat odd-symmetry component of input:
  do ix=1,nxh
     ix1=nxh+ix
     ix2=nxp-ix
     do iy=1,ny
        v1(iy,ix1)=p(iy,ix)-p(iy,ix2)
        v2(iy,ix)=p(iy,ix)+p(iy,nxp-ix)
     enddo
  enddo
  call xmulbv(bco1,v1(1,nxhp),v1,nxh,nxh,noq,noq,ny,nxh,nx,nx)
  call xbacbv(aco1,v1,nxh,noq,noq,ny,nxh,nx)

!  treat even-symmetry component of input:
  call xmulbv(bco2,v2,v2(1,nxhp),nxh,nxh,noq,noq,ny,nxh,nx,nx)
  call xbacbv(aco2,v2(1,nxhp),nxh,noq,noq,ny,nxh,nx)
  do ix=1,nxh
     ix1=nxp-ix
     ix2=nxh+ix
     do iy=1,ny
        q(iy,ix) =v1(iy,ix)+v2(iy,ix2)
        q(iy,ix1)=v1(iy,ix)-v2(iy,ix2)
     enddo
  enddo
  return
  end subroutine xdcirdp

  subroutine xmulbv(a,v1,v2,n1x,n2x,nbh1,nbh2,ny, na,nv1,nv2)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    xmulbv multiplication of a banded matrix times x vectors
!   prgmmr: purser           org: np20                date: 1994-01-01
!
! abstract:  multiplication of a banded matrix times parallel x vectors     
!									       
!
! program history log:
!   1994-05-12  parrish,d. elimanate memory bank conflicts
!   2004-07-27  treadon - add intent in/out
!
!   input argument list:
!     a      - matrix
!     v1     - array of input vectors
!     n1x    - number of rows assumed for a and for v2
!     n2x    - number of columns assumed for a and rows for v1
!     nbh1   - left half-bandwidth of fortran array a
!     nbh2   - right half-bandwidth of fortran array a
!     ny     - number of parallel x-vectors
!     na     - first fortran dimension of a
!     nv1    - first fortran dimension of v1
!     nv2    - first fortran dimension of v2
!
!   output argument list:
!     v2     - array of output vectors
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp
!$$$
  use kinds, only: r_double
  use variables, only: zero
  implicit none

! Declare passed variables
  integer,intent(in):: n1x,n2x,nbh1,nbh2,ny,na,nv1,nv2
  real(r_double),dimension(na,-nbh1:nbh2),intent(in):: a
  real(r_double),dimension(ny,nv1),intent(in):: v1
  real(r_double),dimension(ny,nv2),intent(out):: v2

! Declare local variables
  logical odd
  integer ix,iy,jix,ix1,iy1

  odd=mod(ny,2)==1
  do ix=1,n1x
     do iy=1,ny
        v2(iy,ix)=zero
     enddo
  enddo
  do jix=-nbh1,nbh2
     do ix=max(1,1-jix),min(n1x,n2x-jix)
        ix1=jix+ix
        do iy=1,ny-1,2
           iy1=iy+1
           v2(iy,ix)=v2(iy,ix)+a(ix,jix)*v1(iy,ix1)
           v2(iy1,ix)=v2(iy1,ix)+a(ix,jix)*v1(iy1,ix1)
        enddo
        if (odd) v2(ny,ix)=v2(ny,ix)+a(ix,jix)*v1(ny,ix1)
     enddo
  enddo
  return
  end subroutine xmulbv

  subroutine xbacbv(a,v,nx,nbh1,nbh2,ny,na,nv)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    xbacbv back-substitution step of parallel linear inversion
!   prgmmr: purser           org: np20                date:  1994-01-01
!
! abstract:  back-substitution step of parallel linear 
!            inversion involving banded matrix and x-vectors.
!
! program history log:
!   1994-05-12  parrish,d. elimanate memory bank conflicts
!   2004-07-27  treadon - add intent in/out
!
!   input argument list:
!     a      - encodes the (l)*(d**-1)*(u) factorization of the linear-system
!              matrix, as supplied by subroutine aldub or, if n=na, by ldub
!     v      - right-hand-side vectors
!     nx     - number of rows assumed for a and length of
!              x-vectors stored in v
!     nbh1   - left half-bandwidth of fortran array a
!     nbh2   - right half-bandwidth of fortran array a
!     ny     - number of parallel x-vectors inverted
!     na     - first fortran dimension of a
!     nv     - first (x-direction) fortran dimension of v
!
!   output argument list:
!     v      - solution vectors
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp
!$$$
  use kinds, only: r_double
  implicit none

! Declare passed variables
  integer,intent(in):: nx,nbh1,nbh2,ny,na,nv
  real(r_double),dimension(na,-nbh1:nbh2),intent(in):: a
  real(r_double),dimension(ny,nv),intent(inout):: v

! Declare local variables
  integer jx,ix,iy,ix1
  real(r_double) aij

  do jx=1,nx
     do ix=jx+1,min(nx,jx+nbh1)
        ix1=jx-ix
        do iy=1,ny
           v(iy,ix)=v(iy,ix)-a(ix,ix1)*v(iy,jx)
        enddo
     end do
     do iy=1,ny
        v(iy,jx)=a(jx,0)*v(iy,jx)
     end do
  end do
  do jx=nx,2,-1
     do ix=max(1,jx-nbh2),jx-1
        ix1=jx-ix
        do iy=1,ny
           v(iy,ix)=v(iy,ix)-a(ix,ix1)*v(iy,jx)
        enddo
     enddo
  end do

  return
  end subroutine xbacbv

  subroutine ydsphdp(p,q,aco1,bco1,aco2,bco2,nx,ny,noq)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    ydsphdp               compute y derivatives on sphere
!   prgmmr: purser           org: np20               date:  1994-01-01
!
! abstract:  compute the y-derivatives of data with spherical topology    
!  using compact-differencing and add to an existing field		    
!
! program history log:
!   1994-05-12  parrish,d. elimanate memory bank conflicts
!   2004-07-27  treadon - add only on use declarations; add intent in/out
!
!   input argument list:
!     p      - array of input data					  
!     q      - array to which derivatives are added			   
!     aco1   - array containing the "a-coefficients", in the format of 
!              a banded l-d-u factorization, for the antisymmetric portion of 
!              the field to be differenced (initialized in cdcoef) 	   
!     bco1   - corresponding band-matrix of "b-coefficients"	  
!     aco2   - like aco1, but for the symmetric portion of the data	 
!     bco2   - like bco1, but for the symmetric portion of the data	
!     nx     - number of longitudes (x-direction), an even number.	
!     ny     - number of latitude points (y-direction)	
!     noq    - quarter of the order of differencing (1 implies 4th-order) 
!
!   output argument list:
!     q      - array to which derivatives are added			   
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp
!$$$
  use kinds, only: r_double
  implicit none

! Declare passed variables
  integer,intent(in):: nx,ny,noq
  real(r_double),dimension(ny,-noq:noq),intent(in):: aco1,bco1,aco2,bco2
  real(r_double),dimension(ny,nx),intent(in):: p
  real(r_double),dimension(ny,nx),intent(out):: q

! Declare local variables
  integer nxh,nxhp,ix,iy,ix1
  real(r_double),dimension(ny,nx):: v1,v2

  nxh=nx/2
  nxhp=nxh+1

!  treat odd-symmetry component of input:
  do ix=1,nxh
     ix1=nxh+ix
     do iy=1,ny
        v1(iy,ix1)=p(iy,ix)-p(iy,ix1)
        v2(iy,ix)= p(iy,ix)+p(iy,ix1)
     enddo
  enddo
  call ymulbv(bco1,v1(1,nxhp),v1,ny,ny,noq,noq,nxh,ny,nx,nx)
  call ybacbv(aco1,v1,ny,noq,noq,nxh,ny,nx)

!  treat even-symmetry component of input:
  call ymulbv(bco2,v2,v2(1,nxhp),ny,ny,noq,noq,nxh,ny,nx,nx)
  call ybacbv(aco2,v2(1,nxhp),ny,noq,noq,nxh,ny,nx)
  do ix=1,nxh
     ix1=nxh+ix
     do iy=1,ny
        q(iy,ix)=v2(iy,ix1)+v1(iy,ix)
        q(iy,ix1)=v2(iy,ix1)-v1(iy,ix)
     enddo
  enddo
  return
  end subroutine ydsphdp

  subroutine ymulbv(a,v1,v2, n1y,n2y,nbh1,nbh2,nx, na,nv1,nv2)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    ymulbv multiplication of a banded matrix times parallel y vects
!   prgmmr: purser           org: np20               date:  1994-01-01
!
! abstract:  multiplication of a banded matrix times parallel y-vectors.
!
! program history log:
!   1994-05-12  parrish,d. elimanate memory bank conflicts
!   2004-07-27  treadon - add only on use declarations; add intent in/out
!
!   input argument list:
!     a      - matrix
!     v1     - array of input vectors
!     n1y    - number of rows assumed for a and for v2
!     n2y    - number of columns assumed for a and rows for v1
!     nbh1   - left half-bandwidth of fortran array a
!     nbh2   - right half-bandwidth of fortran array a
!     nx     - length of each of the parallel y-vectors
!     na     - first fortran dimension of a
!     nv1    - first fortran dimension of v1
!     nv2    - first fortran dimension of v2
!
!   output argument list:
!     v2     - array of output vectors
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp
!$$$

  use kinds, only: r_double
  use variables, only: zero
  implicit none

! Declare passed variables
  integer,intent(in):: n1y,n2y,nbh1,nbh2,nx,na,nv1,nv2
  real(r_double),dimension(na,-nbh1:nbh2),intent(in):: a
  real(r_double),dimension(n2y,nv1),intent(in):: v1
  real(r_double),dimension(n1y,nv2),intent(out):: v2

! Declare local variables
  integer ix,iy,jy,jiy
  real(r_double) aij

  do ix=1,nx
     do iy=1,n1y
        v2(iy,ix)=zero
     enddo
  enddo
  do ix=1,nx
     do jiy=-nbh1,nbh2
        do iy=max(1,1-jiy),min(n1y,n2y-jiy)
           v2(iy,ix)=v2(iy,ix)+a(iy,jiy)*v1(jiy+iy,ix)
        enddo
     end do
  end do
  return
  end subroutine ymulbv

  subroutine ybacbv(a,v,ny,nbh1,nbh2,nx,na,nv)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    ybacbv back-substitution step of parallel linear inversion
!   prgmmr: purser           org: np20               date:  1994-01-01
!
! abstract:  back-substitution step of parallel linear inversion involving
!  banded matrix and y-vectors.
!
! program history log:
!   1994-05-12  parrish,d. elimanate memory bank conflicts
!   2004-07-27  treadon - add intent in/out
!
!   input argument list:
!     v      - right-hand-side vectors
!     a      - encodes the (l)*(d**-1)*(u) factorization of the linear-system
!              matrix, as supplied by subroutine aldub or, if n=na, by ldub
!     ny     - number of rows assumed for a and length of
!              y-vectors stored in v
!     nbh1   - left half-bandwidth of fortran array a
!     nbh2   - right half-bandwidth of fortran array a
!     nx     - number of parallel y-vectors inverted
!     na     - first fortran dimension of a
!     nv     - first (x-direction) fortran dimension of v
!
!   output argument list:
!     v      - solution vectors
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp
!$$$

  use kinds, only: r_double
  implicit none

! Declare passed variables
  integer,intent(in):: ny,nbh1,nbh2,nx,na,nv
  real(r_double),dimension(na,-nbh1:nbh2),intent(in):: a
  real(r_double),dimension(ny,nv),intent(inout):: v

! Declare local variables
  logical odd
  integer jy,iy,ix,ix1
  real(r_double) aij

  odd = mod(nx,2)==1
  do ix=1,nx-1,2
     ix1=ix+1
     do jy=1,ny
        do iy=jy+1,min(ny,jy+nbh1)
           v(iy,ix) =v(iy,ix) -a(iy,jy-iy)*v(jy,ix)
           v(iy,ix1)=v(iy,ix1)-a(iy,jy-iy)*v(jy,ix1)
        enddo
        v(jy,ix) =a(jy,0)*v(jy,ix)
        v(jy,ix1)=a(jy,0)*v(jy,ix1)
     end do
     do jy=ny,2,-1
        do iy=max(1,jy-nbh2),jy-1
           v(iy,ix) =v(iy,ix) -a(iy,jy-iy)*v(jy,ix)
           v(iy,ix1)=v(iy,ix1)-a(iy,jy-iy)*v(jy,ix1)
        enddo
     enddo
  end do
  if (odd) then
     ix=nx
     do jy=1,ny
        do iy=jy+1,min(ny,jy+nbh1)
           v(iy,ix)=v(iy,ix)-a(iy,jy-iy)*v(jy,ix)
        enddo
        v(jy,ix)=a(jy,0)*v(jy,ix)
     end do
     do jy=ny,2,-1
        do iy=max(1,jy-nbh2),jy-1
           v(iy,ix)=v(iy,ix)-a(iy,jy-iy)*v(jy,ix)
        enddo
     enddo
  endif
     
  return
  end subroutine ybacbv

  subroutine tydsphdp(p,q,aco1,bco1,aco2,bco2,nx,ny,noq)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    tydsphdp                           adjoint of ydsphdp
!   prgmmr: purser           org: np20               date:  1994-01-01
!
! abstract:  adjoint of ydsphdp which computes the y-derivatives of
!            data with spherical topology using compact-differencing
!            and add to an existing field		    
!
! program history log:
!   1994-05-12  parrish,d. elimanate memory bank conflicts
!   2004-07-27  treadon - add intent in/out
!
!   input argument list:
!     q      - array of input adjoint data  
!     aco1   - array containing the "a-coefficients", in the format of 
!              a banded l-d-u factorization, for the antisymmetric portion of 
!              the field to be differenced (initialized in cdcoef) 	   
!     bco1   - corresponding band-matrix of "b-coefficients"	  
!     aco2   - like aco1, but for the symmetric portion of the data	 
!     bco2   - like bco1, but for the symmetric portion of the data	
!     nx     - number of longitudes (x-direction), an even number.	
!     ny     - number of latitude points (y-direction)	
!     noq    - quarter of the order of differencing (1 implies 4th-order) 
!
!   output argument list:
!     p      - array to which adjoint derivatives are added  
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp
!$$$

  use kinds, only: r_double
  implicit none

! Declare passed variables
  integer,intent(in):: nx,ny,noq
  real(r_double),dimension(ny,-noq:noq),intent(in):: aco1,bco1,aco2,bco2
  real(r_double),dimension(ny,nx),intent(in):: q
  real(r_double),dimension(ny,nx),intent(out):: p

! Declare local variables
  integer nxh,nxhp,ix,iy,ix1
  real(r_double),dimension(ny,nx):: v1,v2

  nxh=nx/2
  nxhp=nxh+1
  do ix=1,nxh
     ix1=nxh+ix
     do iy=1,ny
        v1(iy,ix1)=q(iy,ix)+q(iy,ix1)
        v2(iy,ix)=q(iy,ix)-q(iy,ix1)
     enddo
  enddo
  
  call ybacvb(v1(1,nxhp),aco2,ny,noq,noq,nxh,nx,ny)
  call ymulvb(v1(1,nxhp),bco2,v1,ny,ny,noq,noq,nxh,nx,ny,nx)
  
  call ybacvb(v2,aco1,ny,noq,noq,nxh,nx,ny)
  call ymulvb(v2,bco1,v2(1,nxhp),ny,ny,noq,noq,nxh,nx,ny,nx)
  do ix=1,nxh
     ix1=nxh+ix
     do iy=1,ny
        p(iy,ix)=p(iy,ix)-v1(iy,ix)-v2(iy,ix1)
        p(iy,ix1)=p(iy,ix1)-v1(iy,ix)+v2(iy,ix1)
     enddo
  enddo
  
  return
  end subroutine tydsphdp

  subroutine ybacvb(v,a,ny,nbh1,nbh2,nx,nv,na)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    ybacvb back-substitution step of parallel linear inversion
!   prgmmr: purser           org: np20               date:  1994-01-01
!
! abstract:  back-substitution step of parallel linear inversion involving
!            banded matrix and row-y-vectors.
!
! program history log:
!   1994-05-12  parrish,d. elimanate memory bank conflicts
!   2004-07-27  treadon - add intent in/out
!
!   input argument list:
!     v      - right-hand-side vectors
!     a      - encodes the (l)*(d**-1)*(u) factorization of the linear-system
!              matrix, as supplied by subroutine aldub or, if n=na, by ldub
!     ny     - number of rows assumed for a and length of
!              y-vectors stored in v
!     nbh1   - left half-bandwidth of fortran array a
!     nbh2   - right half-bandwidth of fortran array a
!     nx     - number of parallel y-vectors inverted
!     na     - first fortran dimension of a
!     nv     - first (x-direction) fortran dimension of v
!
!   output argument list:
!     v      - solution vectors
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp
!$$$

  use kinds, only: r_double
  implicit none

! Declare passed variables
  integer,intent(in):: ny,nbh1,nbh2,nx,nv,na
  real(r_double),dimension(na,-nbh1:nbh2),intent(in):: a
  real(r_double),dimension(ny,nv),intent(inout):: v

! Declare local variables  
  logical odd
  integer iy,jy,ix,ix1
  real(r_double) aij

  odd = mod(nx,2)==1

  do ix=1,nx-1,2
     ix1=ix+1
     do iy=1,ny
        do jy=iy+1,min(ny,iy+nbh2)
           v(jy,ix) =v(jy,ix) -v(iy,ix) *a(iy,jy-iy)
           v(jy,ix1)=v(jy,ix1)-v(iy,ix1)*a(iy,jy-iy)
        enddo
        v(iy,ix) =v(iy,ix) *a(iy,0)
        v(iy,ix1)=v(iy,ix1)*a(iy,0)
     enddo

     do iy=ny,2,-1
        do jy=max(1,iy-nbh1),iy-1
           v(jy,ix) =v(jy,ix) -v(iy,ix) *a(iy,jy-iy)
           v(jy,ix1)=v(jy,ix1)-v(iy,ix1)*a(iy,jy-iy)
        enddo
     enddo
  end do

  if (odd) then
     ix=nx
     do iy=1,ny
        do jy=iy+1,min(ny,iy+nbh2)
           v(jy,ix) =v(jy,ix) -v(iy,ix) *a(iy,jy-iy)
        enddo
        v(iy,ix) =v(iy,ix) *a(iy,0)
     end do
     do iy=ny,2,-1
        do jy=max(1,iy-nbh1),iy-1
           v(jy,ix) =v(jy,ix) -v(iy,ix) *a(iy,jy-iy)
        enddo
     end do
  endif
  return
  end subroutine ybacvb

  subroutine ymulvb(v1,a,v2,n1y,n2y,nbh1,nbh2,nx,nv1,na,nv2)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    ymulvb multiplication of y-vectors times banded matrix 
!   prgmmr: purser           org: np20               date:  1994-01-01
!
! abstract:  multiplication of y-vectors times banded matrix 
!
! program history log:
!   1994-05-12  parrish,d. elimanate memory bank conflicts
!   2004-07-27  treadon - add intent in/out
!
!   input argument list:
!     a      - matrix
!     v1     - array of input row-vectors
!     n1y    - number of rows assumed for a and for v1
!     n2y    - number of columns assumed for a and columns for v2
!     nbh1   - left half-bandwidth of fortran array a
!     nbh2   - right half-bandwidth of fortran array a
!     nx     - length of each of the parallel y-vectors
!     na     - first fortran dimension of a
!     nv1    - first fortran dimension of v1
!     nv2    - first fortran dimension of v2
!
!   output argument list:
!     v2     - array of output vectors
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp
!$$$

  use kinds, only: r_double
  use variables, only: zero
  implicit none

! Delcare passed variables
  integer,intent(in):: n1y,n2y,nbh1,nbh2,nx,na,nv1,nv2
  real(r_double),dimension(na,-nbh1:nbh2),intent(in):: a
  real(r_double),dimension(n1y,nv2),intent(in):: v1
  real(r_double),dimension(n2y,nv2),intent(out):: v2

! Declare local variables
  integer ix,iy,jiy,jy
  real(r_double) aij

  do ix=1,nx
     do iy=1,n2y
        v2(iy,ix)=zero
     enddo
  enddo

  do ix=1,nx
     do jiy=-nbh1,nbh2
        do iy=max(1,1-jiy),min(n1y,n2y-jiy)
           jy=jiy+iy
           aij=a(iy,jiy)
           v2(jy,ix)=v2(jy,ix)+v1(iy,ix)*aij
        enddo
     enddo
  end do
  return
  end subroutine ymulvb

  subroutine inisph(r,yor,tau,nx,ny)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    inisph          init coefs for compact diff on sphere
!   prgmmr: purser, r.j.     org: np20                date: 1994-01-01
!
! abstract:  This routine initializes coefficients for high-order 
!            compact differencing on a spherical grid.
!
! program history log:
!   1994-01-01 purser
!   2004-06-21 treadon - update documentation
!   2004-07-28  treadon - add only to module use, add intent in/out
!
!   input argument list:
!     r   - radius of the sphere
!     yor - array of the increasing grid latitudes in radians
!     tau - array of quadrature weights associated with yor
!     nx  - (even) number of grid longitudes
!     ny  - number of grid latitudes
!
!   output argument list:
!     none
!
!   Remarks:  
!     This routine initializes array coef which is in module berror.
!     coef is an array of size 3*ny+4*(2*noq+1)*(ny+nx/2) containing 
!     the coefficients for high-order compact differencing
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_double
  use variables, only: zero,half,one,two,pi,nlat,coef,noq
  implicit none

! Declare passed variables
  integer,intent(in):: nx,ny
  real(r_double),intent(in):: r
  real(r_double),dimension(nlat-1),intent(in):: tau,yor
  
! Declare local variables
  integer nxh,nbp,nya,nxa,lbcox1,lacox1,ltaui,lbcox2
  integer lacoy1,lacox2,lbcoy1,lcy,lacoy2,lbcoy2,i,ix,ltau,iy
  real(r_double) ir,pih,pi2onx,ri
  real(r_double),dimension(max(nx/2,ny+2)+16+52*(noq+5)+32*(noq+5)**2):: w

! Set parameters for calls to subsequent routines  
  nxh=nx/2
  nbp=2*noq+1
  nya=ny*nbp
  nxa=nxh*nbp
  lacox1=1
  lbcox1=lacox1+nxa
  lacox2=lbcox1+nxa
  lbcox2=lacox2+nxa
  lacoy1=lbcox2+nxa
  lbcoy1=lacoy1+nya
  lacoy2=lbcoy1+nya
  lbcoy2=lacoy2+nya
  lcy   =lbcoy2+nya-1
  ltau  =lcy   +ny
  ltaui =ltau  +ny
  coef = zero


! Check for error conditions.  If found, write message to standard
! out and stop program execution.
  if (2*nxh /= nx) then
     write(6,*)'INISPH:  ***ERROR***'
     write(6,'(" number of longitudes,'',i4,'' specified in ")') nx
     write(6,'(" passed through parameter list of subroutine inisph")')
     write(6,'(" is an odd number. Please use an EVEN number.")')

     STOP
!!     call stop2(61)
  endif
  do iy=1,ny
     if (yor(iy).le.(-pi) .or. yor(iy).ge.pi) then
        write(6,*)'INISPH:  ***ERROR***'
        write(6,'(" grid-latitude number ",i4," passed through")') iy
        write(6,'(" parameter list of subroutine inisph lies outside")')
        write(6,'(" the range of (-pi/2 , pi/2)")')
!!        call stop2(62)
        STOP
     endif
  enddo

! Load coefficient array
  ri=one/r
  pih=pi/two
  pi2onx=pi/float(nxh)
  do ix=1,nxh
     coef(lacoy1+ix-1)=(float(ix)-half)*pi2onx
  enddo

  call cdcoef(nxh,noq,zero,pi,coef(lacoy1),w&
       ,coef(lacox1),coef(lbcox1),coef(lacox2),coef(lbcox2)&
       ,nxh,nxh,nxh,nxh)
  do i=0,nxa-1
     coef(lbcox1+i)=coef(lbcox1+i)*ri
     coef(lbcox2+i)=coef(lbcox2+i)*ri
  enddo
  
  call cdcoef(ny,noq,-pih,pih,yor,w&
       ,coef(lacoy1),coef(lbcoy1),coef(lacoy2),coef(lbcoy2)&
       ,ny,ny,ny,ny)
  do i=0,nya-1
     coef(lbcoy1+i)=coef(lbcoy1+i)*ri
     coef(lbcoy2+i)=coef(lbcoy2+i)*ri
  enddo
  
  do iy=1,ny
     coef(lcy+iy)=one/cos(yor(iy))
     coef(ltau+iy)=tau(iy)
     coef(ltaui+iy)=one/tau(iy)
  enddo

  end subroutine inisph

  subroutine cdcoef(nh,noq,zlim1,zlim2,z,work,aco1,bco1,aco2,bco2,na1,nb1,na2,nb2)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    cdcoef              compute differencing coeffciients
!   prgmmr: purser, r.j.     org: np20                date: 1994-01-01
!
! abstract:  This routine computes the coefficients, in band-matrix 
!            form, of a compact differencing operator for 
!            symmetrically arranged cyclic data.
!
! program history log:
!   1994-01-01 purser
!   2004-06-21 treadon - update documentation
!   2004-07-28  treadon - add only to module use, add intent in/out
!
!   input argument list:
!     nh    - number data in one of the symmetric halves of the cycle
!     noq   - quarter of the intended order of accuracy
!     zlim1 - coordinate of first reflection-point
!     zlim2 - coordinate of the second reflection-point
!     z     - array of coordinates, strictly in (zlim1,zlim2) for one
!             symmetric half of the cycle. the coordinate of the point
!             preceeding z(1) is 2*zlim1-z(1) etc., while the coordinate
!             of the point succeeding z(nh) in the cycle is 2*zlim2-z(nh) etc.
!     work  - work-space array of size 2*(8+nh+26*noq+16*noq**2)
!     na1   - first dimension of aco1
!     nb1   - first dimension of bco1
!     na2   - first dimension of aco2
!     nb2   - first dimension of bco2
!
!   output argument list:
!     aco1 - array containing the "a-coefficients", in the format of
!            a banded l-d-u factorization, for the antisymmetric portion
!            of the field to be differenced
!     bco1 - corresponding band-matrix of "b-coefficients"
!     aco2 - like aco1, but for the symmetric portion of the data
!     bco2 - like bco1, but for the symmetric portion of the data
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_double
  use variables, only: zero,two
  implicit none

! Declare passed variables  
  integer,intent(in):: nh,noq,na1,nb1,na2,nb2
  real(r_double),intent(in):: zlim1,zlim2
  real(r_double),dimension(*),intent(in):: z
  real(r_double),dimension(1-noq:*):: work
  real(r_double),dimension(na1,-noq:noq),intent(out):: aco1
  real(r_double),dimension(nb1,-noq:noq),intent(out):: bco1
  real(r_double),dimension(na2,-noq:noq),intent(out):: aco2
  real(r_double),dimension(nb2,-noq:noq),intent(out):: bco2

! Declare local variables
  integer kw,kb,js,ir,i,n,j,ka,nohp

! Initialize output arrays to zero  
  n=nh*2
  do i=1,nh
     work(i)=z(i)
     do j=-noq,noq
        aco1(i,j)=zero
        bco1(i,j)=zero
        aco2(i,j)=zero
        bco2(i,j)=zero
     enddo
  enddo

! Load work array
  do i=1,noq
     work(1-i)=two*zlim1-work(i)
     work(nh+i)=two*zlim2-work(nh+1-i)
  enddo

! Set local parameters
  nohp=noq*2+1
  ka=nh+noq+1
  kb=ka+nohp
  kw=kb+nohp


! Compute coefficients
  do i=1,nh
     call dfcd(work(i-noq),work(i-noq),work(i),nohp,nohp&
          ,work(ka),work(kb),work(kw))
     do j=i-noq,i+noq
        if(j.lt.1)then
           ir=-1
           js=1-j-i
        elseif(j.gt.nh)then
           js=n+1-j-i
           ir=-1
        else
           js=j-i
           ir=1
        endif
        aco1(i,js)=aco1(i,js)+	 work(ka+noq+j-i)
        bco1(i,js)=bco1(i,js)+ir*work(kb+noq+j-i)/two
        aco2(i,js)=aco2(i,js)+ir*work(ka+noq+j-i)
        bco2(i,js)=bco2(i,js)+	 work(kb+noq+j-i)/two
     enddo
  enddo
  call aldub(aco1,nh,noq,noq,na1)
  call aldub(aco2,nh,noq,noq,na2)
  return
  end subroutine cdcoef

  subroutine dfcd(za,zb,z0,na,nb,a,b,work)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    dfcd        compute coefs for compact/lagrange schemes
!                            for differencing or quadrature
!   prgmmr: purser, r.j.     org: np20                date: 1994-01-01
!
! abstract:  This routine computes coefficients for compact or 
!            lagrange schemes for differencing or quadrature.  A 
!            description of the routine functionality for quadrature
!            and differencing follows in the remarks section.
!
! program history log:
!   1994-01-01 purser
!   2004-06-21 treadon - update documentation
!   2004-07-28  treadon - add only to module use, add intent in/out
!
!   input argument list:
!      The meaning of arguments varies as described above based
!      on the type of coefficients being computed.
!
!   output argument list:
!      The meaning of arguments varies as described above based
!      on the type of coefficients being computed.
!
! remarks:
!
!  Quadrature:
!   Input:
!     let Z0 be the coordinate of the nominated "target" point,
!            (e.g., the midpoint of the target interval)
!     ZA are the coordinates of the NA source template points.
!     ZB are the coordinates of the NB target template points -
!     NB=2 for a Lagrange scheme, otherwise NB>2.
!   Output:
!     A is the vector of NA A-coefficients, A(1), .. A(NA)
!     B is the vector of NB B-coefficients, B(1), ...B(NB)
!     (For a Lagrange scheme B(1) = -B(2) = 1/("delta sigma".)
!   
!   WORK is an array of work-space used for the intermediate
!   calculations - it contains nothing of interest on input or output.
!   It must be given a size of at least 2*(NA+NB)*(NA+NB+1) in the
!   routine that calls this one (it is the same as in DFCO except now
!   using r_double precision)
!
!   Differencing:
!   The only changes from the case of quadrature are that:
!     Z0 is the coordinate of the particular target point, which is
!        no longer arbitrary;
!     ZA are coordinates of TARGET template point(s);
!     ZB are coordinates of SOURCE template points;
!     A is the vector of NA A-coefficients;
!     B is the vector of NB (=ND) D-coefficients
!     (For a Lagrange scheme NA=1 and, trivially, A(1) = 1. )
!
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_double
  use variables, only: zero,one
  implicit none

! Declare passed variables
  integer,intent(in):: na,nb
  real(r_double),intent(in):: z0
  real(r_double),dimension(*),intent(in):: za,zb
  real(r_double),dimension(*),intent(out):: work,a,b

! Declare local variables  
  integer nc,ncs,ncsp,j,iw,i
  real(r_double) p,z
  
  nc=na+nb
  ncs=nc*nc
  ncsp=ncs+1
  do j=1,na
     iw=1+(j-1)*nc
     work(iw)=one
     work(iw+1)=zero
     work(iw+2)=one
  enddo
  do j=1,nb
     iw=1+(j+na-1)*nc
     work(iw)=zero
     work(iw+1)=one
  enddo
  do j=1,na
     z=za(j)-z0
     p=one
     do i=4,nc
        p=p*z
        work(i+(j-1)*nc)=p*(i-2)
     enddo
  enddo
  do j=1,nb
     z=zb(j)-z0
     p=one
     do i=3,nc
        p=p*z
        work(i+(j+na-1)*nc)=-p
     enddo
  enddo
  work(ncsp)=one
  do i=2,nc
     work(ncs+i)=zero
  enddo
  
! Find the following routine qlinvmv (a linear equation solver) amongst
! all the other basic matrix routines (here, the r_double precision
! version is used).
  call dlinvmm(work,work(ncsp),nc,1,nc,nc)
  do i=1,na
     a(i)=work(ncs+i)
  enddo
  do i=1,nb
     b(i)=work(ncs+na+i)
  enddo
  return
  end subroutine dfcd

  subroutine aldub(a,n,nbh1,nbh2,na)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    aldub                               ldu decomposition
!   prgmmr: purser, r.j.     org: np20                date: 1994-01-01
!
! abstract:  This routine computes the (L)*(D**-1)*(U) decomposition 
!            of asymmetric band-matrix compact differencing on 
!            a spherical grid.
!
! program history log:
!   1994-01-01 purser
!   2004-06-21 treadon - update documentation
!   2004-07-28  treadon - add only to module use, add intent in/out
!
!   input argument list:
!     "a"   - asymmetric band matrix
!      n    - number of rows assumed for A and for V
!      nbh1 - left half-bandwidth of fortran array A
!      nbh2 - right half-bandwidth of fortran array A
!      na   - first fortran dimension of A
!
!   output argument list:
!     "a"   - contains the (L)*(D**-1)*(U) factorization of the 
!             input matrix, where
!             (L) is lower triangular with unit main diagonal
!             (D) is a diagonal matrix
!             (U) is upper triangular with unit main diagonal
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_double
  use variables, only: zero,one
  implicit none

! Declare passed variables
  integer,intent(in):: n,nbh1,nbh2,na
  real(r_double),dimension(na,-nbh1:nbh2),intent(inout):: a

! Declare local variables
  integer j,jp,i,k,imost,jmost
  real(r_double) ajj,aij,ajji

  do j=1,n
     imost=min(n,j+nbh1)
     jmost=min(n,j+nbh2)
     jp=j+1
     ajj=a(j,0)
     if(ajj.eq.zero)then
        write(6,*)'ALDUB:  ***ERROR***'
        write(6,'("  FAILURE:  matrix requires pivoting or is singular")')
!!        call stop2(63)
        STOP
     endif
     ajji=one/ajj
     a(j,0)=ajji
     do i=jp,imost
        aij=ajji*a(i,j-i)
        a(i,j-i)=aij
        do k=jp,jmost
           a(i,k-i)=a(i,k-i)-aij*a(j,k-j)
        enddo
     enddo
     do k=jp,jmost
        a(j,k-j)=ajji*a(j,k-j)
     enddo
  enddo
  return
  end subroutine aldub

  subroutine dlinvmm(a,b,m,mm,na,nb)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    dlinvmm                         invert linear systems
!   prgmmr: purser, r.j.     org: np20                date: 1993-01-01
!
! abstract:  This routine inverts linear systems sharing the same square
!            system matrix in R_DOUBLE PRECISION.
!
! program history log:
!   1993-01-01 purser
!   2004-06-21 treadon - update documentation
!   2004-07-28  treadon - add only to module use, add intent in/out
!
!   input argument list:
!     "a"   - square system matrix
!     "b"   - right-hands-sides
!      m    - degree of (active part of) b and a
!      mm   - number of right-hand-side vectors (active columns of b)
!      na   - first fortran dimension of a
!      nb   - first fortran dimension of b
!
!   output argument list:
!     "a"   - L-D-U factorization of input matrix "a"
!     "b"   - matrix solution of vectors
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_double
  implicit none

! Declare passed variables
  integer,intent(in):: m,mm,na,nb
  real(r_double),dimension(na,*),intent(inout):: a
  real(r_double),dimension(nb,*),intent(inout):: b  

! Declare local parameters
  integer,parameter:: nn = 500

! Declare local variables
  integer,dimension(nn):: ipiv
  real(r_double) d

  call dlufm(a,ipiv,d,m,na)
  call dlubmm(a,b,ipiv,m,mm,na,nb)
  return
  end subroutine dlinvmm

  subroutine dlufm(a,ipiv,d,m,na)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    dlufm                       perform l-u decomposition
!   prgmmr: purser, r.j.     org: np20                date: 1993-01-01
!
! abstract:  This routine performs l-u decomposition of square matrix
!            "a" in place with partial pivoting in R_DOUBLE PRECISION.
!
! program history log:
!   1993-01-01 purser
!   2004-06-21 treadon - update documentation
!   2004-07-28  treadon - add only to module use, add intent in/out
!
!   input argument list:
!     "a"   - square matrix to be factorized
!      m    - degree of (active part of) a
!      na   - first fortran dimension of a
!
!   output argument list:
!     "a"   - L-U factorization of input matrix "a"
!     ipiv  - array encoding the pivoting sequence
!     d     - indicator for possible sign change of determinant
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_double
  use variables, only: zero,one
  implicit none

! Declare passed variables
  integer,intent(in):: m,na
  integer,dimension(*),intent(out):: ipiv
  real(r_double),intent(out):: d
  real(r_double),dimension(na,*),intent(inout):: a

! Declare local variables
  integer j,jp,ibig,jm,i,k
  real(r_double) ajj,aij,ajji,t,abig,aa

  d=one
  ipiv(m)=m
  do j=1,m-1
     jp=j+1
     abig=abs(a(j,j))
     ibig=j
     do i=jp,m
        aa=abs(a(i,j))
        if(aa.gt.abig)then
           ibig=i
           abig=aa
        endif
     enddo
!  swap rows, recording changed sign of determinant
     ipiv(j)=ibig
     if(ibig.ne.j)then
        d=-d
        do k=1,m
           t=a(j,k)
           a(j,k)=a(ibig,k)
           a(ibig,k)=t
        enddo
     endif
     ajj=a(j,j)
     if(ajj.eq.zero)then
        jm=j-1
        write(6,*)'DLUFM:  ***ERROR***'
        write(6,'("DLUFM:  failure due to singular matrix,r, rank=",i3)') jm
!!        call stop2(64)
        STOP
     endif
     ajji=one/ajj
     do i=jp,m
        aij=ajji*a(i,j)
        a(i,j)=aij
        do k=jp,m
           a(i,k)=a(i,k)-aij*a(j,k)
        enddo
     enddo
  enddo
  return
  end subroutine dlufm

  subroutine dlubmm(a,b,ipiv,m,mm,na,nb)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    dlubmm                                  invert matrix
!   prgmmr: purser, r.j.     org: np20                date: 1993-01-01
!
! abstract:  This routine inverts matrix "a"
!
! program history log:
!   1993-01-01 purser
!   2004-06-21 treadon - update documentation
!   2004-07-28  treadon - add only to module use, add intent in/out
!
!   input argument list:
!     "a"   - square matrix to be factorized
!      m    - degree of (active part of) a
!      mm   - number of columns (active part of) b
!      na   - first fortran dimension of a
!      nb   - first fortran dimension of b
!     ipiv  - array encoding the pivoting sequence
!
!   output argument list:
!     "b"   - matrix solution of vectors
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_double
  implicit none

! Declare passed variables
  integer,intent(in):: m,mm,na,nb
  integer,dimension(*),intent(in):: ipiv
  real(r_double),dimension(na,*),intent(in):: a
  real(r_double),dimension(nb,*),intent(out):: b

! Declare local variables  
  integer k,i,l,j
  real(r_double) s

  do k=1,mm !loop over columns of b
     do i=1,m
        l=ipiv(i)
        s=b(l,k)
        b(l,k)=b(i,k)
        do j=1,i-1
           s=s-a(i,j)*b(j,k)
        enddo
        b(i,k)=s
     enddo
     do i=m,1,-1
        s=b(i,k)
        do j=i+1,m
           s=s-a(i,j)*b(j,k)
        enddo
        b(i,k)=s/a(i,i)
     enddo
  enddo
  return
  end subroutine dlubmm
