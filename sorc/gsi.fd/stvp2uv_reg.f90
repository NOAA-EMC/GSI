subroutine stvp2uv_reg(work1,work2)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    stvp2uv_reg compute regional uv from streamfunct./vel. pot. 
!   prgmmr: purser           org: np20               date:  1994-01-01
!
! abstract:  computes regional uv from streamfunction/velocity potential
!            by calculating gradient of a scalar field using high-order
!            compact differencing on a spherical grid.
!
! program history log:
!   1994-05-15  parrish,d. elimanate memory bank conflicts
!
!   input argument list:
!     work1  - array containing the streamfunction
!     work2  - array containing the velocity potential
!
!   output argument list:
!     work1  - array containing the u velocity
!     work2  - array containing the v velocity
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp
!$$$

  use kinds, only: r_kind,i_kind
  use gridmod, only: iglobal,itotsub,nlat,nlon,ltosi,&
       ltosj,ltosi_s,ltosj_s
  implicit none
  
  integer(i_kind) k,kk,ni1,ni2
  real(r_kind),dimension(max(iglobal,itotsub)):: work1,work2
  real(r_kind),dimension(nlat,nlon):: grid1,grid2,grid3,grid4

  do kk=1,iglobal
     ni1=ltosi(kk); ni2=ltosj(kk)
     grid1(ni1,ni2)=work1(kk)
     grid2(ni1,ni2)=work2(kk)
  enddo
  
  call psichi2uv_reg(grid1,grid2,grid3,grid4,&
       1,nlat, 1,nlon, 1,1, &
       1,nlat, 1,nlon, 1,1, &
       1,nlat, 1,nlon, 1,1)
  
  do kk=1,itotsub
     ni1=ltosi_s(kk); ni2=ltosj_s(kk)
     work1(kk)=grid3(ni1,ni2)
     work2(kk)=grid4(ni1,ni2)
  enddo
  
  return
end subroutine stvp2uv_reg

subroutine stvp2uv2_reg(work1,work2)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    stvp2uv_reg compute regional uv from streamfunct./vel. pot. 
!   prgmmr: purser           org: np20               date:  1994-01-01
!
! abstract:  computes regional uv from streamfunction/velocity potential
!            by calculating gradient of a scalar field using high-order
!            compact differencing on a spherical grid.
!
! program history log:
!   1994-05-15  parrish,d. elimanate memory bank conflicts
!
!   input argument list:
!     work1  - array containing the streamfunction
!     work2  - array containing the velocity potential
!
!   output argument list:
!     work1  - array containing the u velocity
!     work2  - array containing the v velocity
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
  
  call psichi2uv_reg(grid1,grid2,grid3,grid4,&
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
end subroutine stvp2uv2_reg

subroutine get_dlon_reg(work1,work2)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    get_dlon_reg compute x derivative  
!   prgmmr: purser           org: np20               date:  1994-01-01
!
! abstract:  compute x derivative
!
! program history log:
!   1994-05-15  parrish,d. elimanate memory bank conflicts
!
!   input argument list:
!     work1  - array to be differentiated
!
!   output argument list:
!     work2  - array containing derivative in x
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
  
  call delx_reg(grid1,grid2,&
       1,nlat, 1,nlon, 1,1, &
       1,nlat, 1,nlon, 1,1, &
       1,nlat, 1,nlon, 1,1)
  
  do j=1,nlon
    do i=1,nlat
      work2(i,j)=grid2(i,j)
    end do
  end do
  
  return
end subroutine get_dlon_reg

subroutine get_dlat_reg(work1,work2)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    get_dlat_reg compute y derivative  
!   prgmmr: purser           org: np20               date:  1994-01-01
!
! abstract:  compute y derivative
!
! program history log:
!   1994-05-15  parrish,d. elimanate memory bank conflicts
!
!   input argument list:
!     work1  - array to be differentiated
!
!   output argument list:
!     work2  - array containing derivative in x
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
  
  call dely_reg(grid1,grid2,&
       1,nlat, 1,nlon, 1,1, &
       1,nlat, 1,nlon, 1,1, &
       1,nlat, 1,nlon, 1,1)
  
  do j=1,nlon
    do i=1,nlat
      work2(i,j)=grid2(i,j)
    end do
  end do
  
  return
end subroutine get_dlat_reg

subroutine get_delsqr_reg(work1,work2)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    get_delsqr_reg compute laplacian  
!   prgmmr: parrish          org: np23               date:  2006-02-13
!
! abstract:  compute laplacian
!
! program history log:
!   2006-02-13  parrish
!
!   input argument list:
!     work1  - array to be differentiated
!
!   output argument list:
!     work2  - array containing laplacian
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

  call get_dlon_reg(work1,grid1)
  call get_dlon_reg(grid1,grid2)
  call get_dlat_reg(work1,grid3)
  call get_dlat_reg(grid3,grid4)

  do j=1,nlon
    do i=1,nlat
      work2(i,j)=grid2(i,j)+grid4(i,j)
    end do
  end do
  
  return
end subroutine get_delsqr_reg
