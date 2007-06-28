subroutine stpjc(rut,rvt,rtt,rpst,sut,svt,stt,spst,mype,pen,b,c)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    stpjc        calculate penalty and contribution to
!                             stepsize for dynamic constraint term
!
!   prgmmr: kleist           org: np20                date: 2005-07-01
!
! abstract: calculate penalty and contribution to stepsize for
!           dynamic constraint term based on tendency of ps
!
! program history log:
!   2005-08-15  kleist
!   2005-09-29  kleist, expand to include more terms
!   2005-11-21  kleist, remove call to calctends_tl
!   2005-11-22  derber clean up code and optimize
!   2006-02-02  treadon - rename prsi as ges_prsi
!   2006-04-06  kleist - expand to include both formulations
!
!   input argument list:
!
!   output argument list:
!     pen      - contribution to penalty from dynamic constraint
!     b        - output for step size calculation
!     c        - output for step size calculation
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,i_kind
  use gridmod, only: lat2,lon2,nsig
  use guess_grids,only: ntguessig,ges_prsi,ges_tv
  use jcmod, only: wt_ext1,wt_ext2,wt_int1,wt_int2,z_ext1,z_ext2,z_ext3,z_int1,&
    z_int2,z_int3
  use constants, only: zero,two,rd_over_cp
  implicit none

! Declare passed variables
  real(r_kind),dimension(lat2,lon2,nsig),intent(in):: sut,svt,stt,rut,rvt,rtt
  real(r_kind),dimension(lat2,lon2,nsig+1),intent(in):: spst,rpst
  integer(i_kind),intent(in):: mype
  real(r_kind),intent(out):: pen,b,c

! Declare local variables
  integer(i_kind) i,j,k,it
  real(r_kind),dimension(lat2,lon2):: subar,svbar,rubar,rvbar
  real(r_kind) suint,svint,stint,ruint,rvint,rtint
  real(r_kind) factor,delp,subar1,svbar1,rubar1,rvbar1,spst1,rpst1

  it=ntguessig
  pen=zero ; b=zero ; c=zero

! EXTERNAL MODE COMPONENT

  do j=2,lon2-1
    do i=2,lat2-1
      subar(i,j)=zero 
      svbar(i,j)=zero
      rubar(i,j)=zero 
      rvbar(i,j)=zero
    end do
  end do

  do k=1,nsig
    do j=2,lon2-1
      do i=2,lat2-1
        delp=(ges_prsi(i,j,k,it)-ges_prsi(i,j,k+1,it)) / &
             (ges_prsi(i,j,1,it)-ges_prsi(i,j,nsig+1,it))
        subar(i,j)=subar(i,j) + sut(i,j,k)*delp
        svbar(i,j)=svbar(i,j) + svt(i,j,k)*delp
        rubar(i,j)=rubar(i,j) + rut(i,j,k)*delp
        rvbar(i,j)=rvbar(i,j) + rvt(i,j,k)*delp
      end do
    end do
  end do

  do j=2,lon2-1
    do i=2,lat2-1
      spst1=spst(i,j,1)+z_ext1(i,j)
      subar1=subar(i,j)+z_ext2(i,j)
      svbar1=svbar(i,j)+z_ext3(i,j)

      rpst1=rpst(i,j,1)
      rubar1=rubar(i,j)
      rvbar1=rvbar(i,j)

      pen = pen + wt_ext1(i,j)*spst1*spst1 +       &
                  wt_ext2(i,j)*( subar1*subar1 + svbar1*svbar1 )

      b   = b   - wt_ext1(i,j)*rpst1*spst1 -       &
                  wt_ext2(i,j)*( subar1*rubar1 + svbar1*rvbar1 )

      c   = c   + wt_ext1(i,j)*rpst1*rpst1 +       &
                  wt_ext2(i,j)*( rubar1*rubar1 + rvbar1*rvbar1 )
    end do
  end do


! INTERNAL MODE COMPONENT
  do k=1,nsig
    do j=2,lon2-1
      do i=2,lat2-1
        factor=rd_over_cp*ges_tv(i,j,k,it)/(ges_prsi(i,j,k,it)+ges_prsi(i,j,k+1,it))

        stint=stt(i,j,k)-factor*(spst(i,j,k)+spst(i,j,k+1))+z_int1(i,j,k)
        suint=sut(i,j,k)-subar(i,j)+z_int2(i,j,k)
        svint=svt(i,j,k)-svbar(i,j)+z_int3(i,j,k)

        ruint=rut(i,j,k)-rubar(i,j)
        rvint=rvt(i,j,k)-rvbar(i,j)
        rtint=rtt(i,j,k)-factor*(rpst(i,j,k)+rpst(i,j,k+1))

        pen = pen + wt_int1(i,j,k)*stint*stint +   &
                    wt_int2(i,j,k)*( suint*suint + svint*svint )

        b   = b   - wt_int1(i,j,k)*rtint*stint -   &
                    wt_int2(i,j,k)*( suint*ruint + svint*rvint )

        c   = c   + wt_int1(i,j,k)*rtint*rtint +   &
                    wt_int2(i,j,k)*( ruint*ruint + rvint*rvint )
      end do
    end do
  end do

  return
end subroutine stpjc

subroutine stpjc_divt(rdivt,ragvt,sdivt,sagvt,mype,pen,b,c)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    stpjc_divt     calculate penalty and contribution to
!                             stepsize for dynamic constraint term
!
!   prgmmr: kleist           org: np20                date: 2006-04-06
!
! abstract: calculate penalty and contribution to stepsize for
!           dynamic constraint term based on tendencies
!
! program history log:
!   2005-04-06  kleist
!
!   input argument list:
!
!   output argument list:
!     pen      - contribution to penalty from dynamic constraint
!     b        - output for step size calculation
!     c        - output for step size calculation
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$

  use kinds, only: r_kind,i_kind
  use gridmod, only: lat2,lon2,nsig
  use guess_grids,only: ntguessig,ges_prsi
  use jcmod, only: wt_ext1,wt_ext2,wt_int1,wt_int2,z_ext1,z_ext2,z_int1,z_int2
  use constants, only: zero,two,rd_over_cp
  use jfunc, only: iter,jiter,jiterstart
  implicit none

  real(r_kind),dimension(lat2,lon2,nsig),intent(in):: sdivt,sagvt,rdivt,ragvt
  integer(i_kind),intent(in):: mype
  real(r_kind),intent(out):: pen,b,c

! Declare local variables
  integer(i_kind) i,j,k,it
  real(r_kind),dimension(lat2,lon2):: sdbar,sabar,rdbar,rabar

  real(r_kind) sdint,saint,rdint,raint
  real(r_kind) delp,sdbar1,sabar1,rdbar1,rabar1
  character(255) outfile

  it=ntguessig
  pen=zero ; b=zero ; c=zero

! EXTERNAL MODE COMPONENT

  do j=2,lon2-1
    do i=2,lat2-1
      sdbar(i,j)=zero
      sabar(i,j)=zero
      rdbar(i,j)=zero
      rabar(i,j)=zero
    end do
  end do

  do k=1,nsig
    do j=2,lon2-1
      do i=2,lat2-1
        delp=(ges_prsi(i,j,k,it)-ges_prsi(i,j,k+1,it)) / &
             (ges_prsi(i,j,1,it)-ges_prsi(i,j,nsig+1,it))
        sdbar(i,j)=sdbar(i,j) + sdivt(i,j,k)*delp
        sabar(i,j)=sabar(i,j) + sagvt(i,j,k)*delp
        rdbar(i,j)=rdbar(i,j) + rdivt(i,j,k)*delp
        rabar(i,j)=rabar(i,j) + ragvt(i,j,k)*delp
      end do
    end do
  end do

  do j=2,lon2-1
    do i=2,lat2-1
      sdbar1=sdbar(i,j)+z_ext1(i,j)
      sabar1=sabar(i,j)+z_ext2(i,j)

      rdbar1=rdbar(i,j)
      rabar1=rabar(i,j)

      pen = pen + wt_ext1(i,j)*sdbar1*sdbar1 +       &
                  wt_ext2(i,j)*( sabar1*sabar1 )

      b   = b   - wt_ext1(i,j)*sdbar1*rdbar1 -       &
                  wt_ext2(i,j)*( sabar1*rabar1 )

      c   = c   + wt_ext1(i,j)*rdbar1*rdbar1 +       &
                  wt_ext2(i,j)*( rabar1*rabar1 )
    end do
  end do

! INTERNAL MODE COMPONENT
  do k=1,nsig
    do j=2,lon2-1
      do i=2,lat2-1
        sdint=sdivt(i,j,k)-sdbar(i,j)+z_int1(i,j,k)
        saint=sagvt(i,j,k)-sabar(i,j)+z_int2(i,j,k)

        rdint=rdivt(i,j,k)-rdbar(i,j)
        raint=ragvt(i,j,k)-rabar(i,j)

        pen = pen + wt_int1(i,j,k)*sdint*sdint +   &
                    wt_int2(i,j,k)*( saint*saint )

        b   = b   - wt_int1(i,j,k)*sdint*rdint -   &
                    wt_int2(i,j,k)*( saint*raint )

        c   = c   + wt_int1(i,j,k)*rdint*rdint +   &
                    wt_int2(i,j,k)*( raint*raint )
      end do
    end do
  end do

  return
end subroutine stpjc_divt
