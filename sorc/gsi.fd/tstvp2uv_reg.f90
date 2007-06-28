subroutine tstvp2uv_reg(work1,work2)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    tstvp2uv_reg                   adjoint of stvp2uv_reg
!   prgmmr: purser           org: np20               date:  1994-01-01
!
! abstract:  adjoint of stvp2uv_reg which computes regional uv from 
!            streamfunction/velocity potential by calculating gradient
!            of a scalar field using high-order compact differencing 
!            on a spherical grid.
!
! program history log:
!   1994-05-15  parrish,d. elimanate memory bank conflicts
!
!   input argument list:
!     work1  - array containing the adjoint u velocity
!     work2  - array containing the adjoint v velocity
!
!   output argument list:
!     work1  - array containing the adjoint streamfunction
!     work2  - array containing the adjoint velocity potential
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp
!$$$
  use kinds, only: r_kind,i_kind
  use gridmod, only: iglobal,itotsub,nlat,nlon,&
       ltosi,ltosj,ltosi_s,ltosj_s
  implicit none

  integer(i_kind) k,kk,ni1,ni2
  real(r_kind),dimension(max(iglobal,itotsub)):: work1,work2
  real(r_kind),dimension(nlat,nlon):: grid1,grid2,grid3,grid4
  
  do kk=1,iglobal
     ni1=ltosi(kk); ni2=ltosj(kk)
     grid1(ni1,ni2)=work1(kk)
     grid2(ni1,ni2)=work2(kk)
  enddo
  
  call psichi2uvt_reg(grid1,grid2,grid3,grid4,&
       1,nlat, 1,nlon, 1,1, &
       1,nlat, 1,nlon, 1,1, &
       1,nlat, 1,nlon, 1,1)
  
  do kk=1,itotsub
     ni1=ltosi_s(kk); ni2=ltosj_s(kk)
     work1(kk)=grid3(ni1,ni2)
     work2(kk)=grid4(ni1,ni2)
  enddo
  
  return
end subroutine tstvp2uv_reg

subroutine tstvp2uv2_reg(work1,work2)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    tstvp2uv_reg                   adjoint of stvp2uv_reg
!   prgmmr: purser           org: np20               date:  1994-01-01
!
! abstract:  adjoint of stvp2uv2_reg which computes regional uv from 
!            streamfunction/velocity potential by calculating gradient
!            of a scalar field using high-order compact differencing 
!            on a spherical grid.
!
! program history log:
!   1994-05-15  parrish,d. elimanate memory bank conflicts
!
!   input argument list:
!     work1  - array containing the adjoint u velocity
!     work2  - array containing the adjoint v velocity
!
!   output argument list:
!     work1  - array containing the adjoint streamfunction
!     work2  - array containing the adjoint velocity potential
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp
!$$$
  use kinds, only: r_kind,i_kind
  use gridmod, only: nlat,nlon
  implicit none

  integer(i_kind) i,j
  real(r_kind),dimension(nlat,nlon):: work1,work2
  real(r_kind),dimension(nlat,nlon):: grid1,grid2,grid3,grid4

  do j=1,nlon
    do i=1,nlat
      grid1(i,j)=work1(i,j)
      grid2(i,j)=work2(i,j)
    end do
  end do
  
  call psichi2uvt_reg(grid1,grid2,grid3,grid4,&
       1,nlat, 1,nlon, 1,1, &
       1,nlat, 1,nlon, 1,1, &
       1,nlat, 1,nlon, 1,1)
  
  do j=1,nlon
    do i=1,nlat
      work1(i,j)=grid3(i,j)
      work2(i,j)=grid4(i,j)
    end do
  end do

  return
end subroutine tstvp2uv2_reg

subroutine tget_dlon_reg(work1,work2)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    tstvp2uv_reg                   adjoint of stvp2uv_reg
!   prgmmr: purser           org: np20               date:  1994-01-01
!
! abstract:  adjoint of stvp2uv2_reg which computes regional uv from 
!            streamfunction/velocity potential by calculating gradient
!            of a scalar field using high-order compact differencing 
!            on a spherical grid.
!
! program history log:
!   1994-05-15  parrish,d. eliminate memory bank conflicts
!   2005-09-28  parrish - fix bug in computing derivatives
!
!   input argument list:
!     work1  - array containing the adjoint u velocity
!     work2  - array containing the adjoint v velocity
!
!   output argument list:
!     work1  - array containing the adjoint streamfunction
!     work2  - array containing the adjoint velocity potential
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp
!$$$
  use kinds, only: r_kind,i_kind
  use gridmod, only: nlat,nlon
  implicit none

  integer(i_kind) i,j
  real(r_kind),dimension(nlat,nlon):: work1,work2
  real(r_kind),dimension(nlat,nlon):: grid1,grid2,grid3,grid4

  do j=1,nlon
    do i=1,nlat
      grid1(i,j)=work1(i,j)
    end do
  end do
  
  call tdelx_reg(grid1,grid2,&
       1,nlat, 1,nlon, 1,1, &
       1,nlat, 1,nlon, 1,1, &
       1,nlat, 1,nlon, 1,1)
  
  do j=1,nlon
    do i=1,nlat
      work2(i,j)=work2(i,j)+grid2(i,j)
    end do
  end do
  
  return
end subroutine tget_dlon_reg

subroutine tget_dlat_reg(work1,work2)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    tstvp2uv_reg                   adjoint of stvp2uv_reg
!   prgmmr: purser           org: np20               date:  1994-01-01
!
! abstract:  adjoint of stvp2uv2_reg which computes regional uv from 
!            streamfunction/velocity potential by calculating gradient
!            of a scalar field using high-order compact differencing 
!            on a spherical grid.
!
! program history log:
!   1994-05-15  parrish,d. elimanate memory bank conflicts
!   2005-09-28  parrish - fix bug in computing derivatives
!
!   input argument list:
!     work1  - array containing the adjoint u velocity
!     work2  - array containing the adjoint v velocity
!
!   output argument list:
!     work1  - array containing the adjoint streamfunction
!     work2  - array containing the adjoint velocity potential
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp
!$$$
  use kinds, only: r_kind,i_kind
  use gridmod, only: nlat,nlon
  implicit none

  integer(i_kind) i,j
  real(r_kind),dimension(nlat,nlon):: work1,work2
  real(r_kind),dimension(nlat,nlon):: grid1,grid2,grid3,grid4

  do j=1,nlon
    do i=1,nlat
      grid1(i,j)=work1(i,j)
    end do
  end do
  
  call tdely_reg(grid1,grid2,&
       1,nlat, 1,nlon, 1,1, &
       1,nlat, 1,nlon, 1,1, &
       1,nlat, 1,nlon, 1,1)
  
  do j=1,nlon
    do i=1,nlat
      work2(i,j)=work2(i,j)+grid2(i,j)
    end do
  end do
  
  return
end subroutine tget_dlat_reg

subroutine tget_delsqr_reg(work2,work1)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    tget_delsqr_reg             adjoint of get_delsqr_reg
!   prgmmr: parrish          org: np23               date:  2006-02-13
!
! abstract:  adjoint of get_delsqr_reg
!
! program history log:
!   2006-02-13  parrish
!
!   input argument list:
!     work2  - array containing delsqr variable
!     work1  - previous contents to be accumulated to
!
!   output argument list:
!     work1  - array containing accumulation of adjoint delsqr
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp
!$$$
  use kinds, only: r_kind,i_kind
  use gridmod, only: nlat,nlon
  use constants, only: zero
  implicit none

  integer(i_kind) i,j
  real(r_kind),dimension(nlat,nlon):: work1,work2
  real(r_kind),dimension(nlat,nlon):: grid1,grid2,grid3,grid4

  do j=1,nlon
    do i=1,nlat
      grid2(i,j)=zero
      grid4(i,j)=zero
      grid1(i,j)=zero
      grid3(i,j)=zero
    end do
  end do
  do j=1,nlon
    do i=1,nlat
      grid2(i,j)=grid2(i,j)+work2(i,j)
      grid4(i,j)=grid4(i,j)+work2(i,j)
    end do
  end do
  call tget_dlat_reg(grid4,grid3)
  call tget_dlat_reg(grid3,work1)
  call tget_dlon_reg(grid2,grid1)
  call tget_dlon_reg(grid1,work1)
  
  return
end subroutine tget_delsqr_reg
