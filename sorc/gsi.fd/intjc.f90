subroutine intjc(rut,rvt,rtt,rprst,sut,svt,stt,sprst,mype)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    intjc        apply dynamic constraint operators
!
!   prgmmr: kleist           org: np20                date: 2005-07-01
!
! abstract: apply dynamic constraint operators and adjoint
!
! program history log:
!   2005-07-01  kleist
!   2005-09-29  kleist, expand to include more terms
!   2005-11-21  kleist, remove call to calctends_ad
!   2005-11-22  derber clean up code and optimize
!   2006-02-02  treadon - rename prsi as ges_prsi
!   2006-04-06  kleist  - expand to include both formulations
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,i_kind
  use gridmod, only: lat2,lon2,nsig
  use guess_grids,only: ntguessig,ges_tv,ges_prsi
  use jcmod, only: wt_ext1,wt_ext2,z_ext1,z_ext2,z_ext3,z_int1,z_int2,z_int3,&
    wt_int1,wt_int2
  use constants, only: zero,rd_over_cp
  implicit none

! Declare passed variables
  real(r_kind),dimension(lat2,lon2,nsig),intent(in):: stt,sut,svt
  real(r_kind),dimension(lat2,lon2,nsig+1),intent(in):: sprst
  real(r_kind),dimension(lat2,lon2,nsig),intent(inout):: rut,rvt,rtt
  real(r_kind),dimension(lat2,lon2,nsig+1),intent(inout):: rprst
  integer(i_kind),intent(in):: mype

! Declare local variables
  integer(i_kind) i,j,k,it
  real(r_kind),dimension(lat2,lon2,nsig):: delp
  real(r_kind),dimension(lat2,lon2):: ubar,vbar,ubar1,vbar1
  real(r_kind) factor,uint,vint,tint

  it=ntguessig
! load delp constant for vertical integral from guess 3d pressure
  do k=1,nsig
    do j=2,lon2-1
      do i=2,lat2-1
        delp(i,j,k)=(ges_prsi(i,j,k,it)-ges_prsi(i,j,k+1,it)) / &
                    (ges_prsi(i,j,1,it)-ges_prsi(i,j,nsig+1,it))
      end do
    end do
  end do

  do j=2,lon2-1
    do i=2,lat2-1
      ubar(i,j)=zero 
      vbar(i,j)=zero
    end do
  end do

!  Forward model external mode

  do k=1,nsig
    do j=2,lon2-1
      do i=2,lat2-1
        ubar(i,j)=ubar(i,j) + sut(i,j,k)*delp(i,j,k)
        vbar(i,j)=vbar(i,j) + svt(i,j,k)*delp(i,j,k)
      end do
    end do
  end do

  do j=2,lon2-1
    do i=2,lat2-1

! Apply weights external mode

      ubar1(i,j)=wt_ext2(i,j)*(ubar(i,j)+z_ext2(i,j))
      vbar1(i,j)=wt_ext2(i,j)*(vbar(i,j)+z_ext3(i,j))

!  Surface pressure term forward - weighting - adjoint

      rprst(i,j,1) = rprst(i,j,1) + wt_ext1(i,j)*(sprst(i,j,1)+z_ext1(i,j))
    end do
  end do

  do k=1,nsig
    do j=2,lon2-1
      do i=2,lat2-1

!  Forward model and weighting for internal modes

        factor=rd_over_cp*ges_tv(i,j,k,it)/(ges_prsi(i,j,k,it)+ges_prsi(i,j,k+1,it))
        uint=wt_int2(i,j,k)*(sut(i,j,k)-ubar(i,j)+z_int2(i,j,k))
        vint=wt_int2(i,j,k)*(svt(i,j,k)-vbar(i,j)+z_int3(i,j,k))
        tint=wt_int1(i,j,k)*(stt(i,j,k)-factor*(sprst(i,j,k)+sprst(i,j,k+1))+ &
                    z_int1(i,j,k))

! Adjoint calculations for internal modes

        rtt(i,j,k) = rtt(i,j,k) + tint
        rprst(i,j,k) = rprst(i,j,k) - factor*tint
        rprst(i,j,k+1) = rprst(i,j,k+1) - factor*tint
        rut(i,j,k) = rut(i,j,k) + uint
        rvt(i,j,k) = rvt(i,j,k) + vint
        ubar1(i,j) = ubar1(i,j) - uint
        vbar1(i,j) = vbar1(i,j) - vint
      end do
    end do
  end do

! Adjoint calculations for external mode

  do k=1,nsig
    do j=2,lon2-1
      do i=2,lat2-1
        rut(i,j,k) = rut(i,j,k) + ubar1(i,j)*delp(i,j,k)
        rvt(i,j,k) = rvt(i,j,k) + vbar1(i,j)*delp(i,j,k)
      end do
    end do
  end do

  return
end subroutine intjc

subroutine intjc_divt(rdivt,ragvt,sdivt,sagvt,mype)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    intjc        apply dynamic constraint operators
!
!   prgmmr: kleist           org: np20                date: 2005-06-04
!
! abstract: apply dynamic constraint operators and adjoint
!
! program history log:
!   2006-04-06  kleist
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,i_kind
  use gridmod, only: lat2,lon2,nsig
  use guess_grids,only: ntguessig,ges_tv,ges_prsi
  use jcmod, only: wt_ext1,wt_ext2,wt_int1,wt_int2,z_ext1,z_ext2,z_int1,z_int2
  use constants, only: zero,rd_over_cp
  implicit none

! Declare passed variables
  real(r_kind),dimension(lat2,lon2,nsig),intent(in):: sdivt,sagvt
  real(r_kind),dimension(lat2,lon2,nsig),intent(inout):: rdivt,ragvt
  integer(i_kind),intent(in):: mype

! Declare local variables
  integer(i_kind) i,j,k,it
  real(r_kind),dimension(lat2,lon2,nsig):: delp
  real(r_kind),dimension(lat2,lon2):: dbar,abar,dbar1,abar1
  real(r_kind) factor,dint,aint

  it=ntguessig
! load delp constant for vertical integral from guess 3d pressure
  do k=1,nsig
    do j=2,lon2-1
      do i=2,lat2-1
        delp(i,j,k)=(ges_prsi(i,j,k,it)-ges_prsi(i,j,k+1,it)) / &
                    (ges_prsi(i,j,1,it)-ges_prsi(i,j,nsig+1,it))
      end do
    end do
  end do

  do j=2,lon2-1
    do i=2,lat2-1
      dbar(i,j)=zero
      abar(i,j)=zero
    end do
  end do

!  Forward model external mode

  do k=1,nsig
    do j=2,lon2-1
      do i=2,lat2-1
        dbar(i,j)=dbar(i,j) + sdivt(i,j,k)*delp(i,j,k)
        abar(i,j)=abar(i,j) + sagvt(i,j,k)*delp(i,j,k)
      end do
    end do
  end do

  do j=2,lon2-1
    do i=2,lat2-1

! Apply weights external mode

      dbar1(i,j)=wt_ext1(i,j)*(dbar(i,j)+z_ext1(i,j))
      abar1(i,j)=wt_ext2(i,j)*(abar(i,j)+z_ext2(i,j))

    end do
  end do

  do k=1,nsig
    do j=2,lon2-1
      do i=2,lat2-1

!  Forward model and weighting for internal modes

        dint=wt_int1(i,j,k)*(sdivt(i,j,k)-dbar(i,j)+z_int1(i,j,k))
        aint=wt_int2(i,j,k)*(sagvt(i,j,k)-abar(i,j)+z_int2(i,j,k))

! Adjoint calculations for internal modes

        rdivt(i,j,k) = rdivt(i,j,k) + dint
        ragvt(i,j,k) = ragvt(i,j,k) + aint
        dbar1(i,j) = dbar1(i,j) - dint
        abar1(i,j) = abar1(i,j) - aint
      end do
    end do
  end do

! Adjoint calculations for external mode

  do k=1,nsig
    do j=2,lon2-1
      do i=2,lat2-1
        rdivt(i,j,k) = rdivt(i,j,k) + dbar1(i,j)*delp(i,j,k)
        ragvt(i,j,k) = ragvt(i,j,k) + abar1(i,j)*delp(i,j,k)
      end do
    end do
  end do

  return
end subroutine intjc_divt
