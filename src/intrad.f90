module intradmod

!$$$ module documentation block
!                .      .    .                                       .
! module:    intradmod    module for intrad and its tangent linear intrad_tl
!  prgmmr:
!
! abstract: module for intrad and its tangent linear intrad_tl
!
! program history log:
!   2005-05-16  Yanqiu zhu - wrap intrad and its tangent linear intrad_tl into one module
!   2005-11-16  Derber - remove interfaces
!   2008-11-26  Todling - remove intrad_tl; add interface back
!   2009-08-13  lueken - update documentation
!
! subroutines included:
!   sub intrad_
!
! variable definitions:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

implicit none

PRIVATE
PUBLIC intrad

interface intrad; module procedure &
          intrad_
end interface

contains

subroutine intrad_(radhead,rt,rq,roz,ru,rv,rst,st,sq,soz,su,sv,sst,rpred,spred)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    intrad      sat radiance nonlin qc obs operator
!   prgmmr: parrish          org: np22                date: 1990-10-11
!
! abstract: apply satellite radiance operator and adjoint with
!            addition of nonlinear qc operator.
!
! program history log:
!   1990-10-11  parrish
!   1992-07-21
!   1995-07-17  derber
!   1997-03-10  wu
!   1997-12-22  weiyu yang
!   1999-08-24  derber, j., treadon, r., yang, w., first frozen mpp version
!   2004-08-02  treadon - add only to module use, add intent in/out
!   2004-10-07  parrish - add nonlinear qc option
!   2005-01-20  okamoto - add wind components
!   2005-04-11  treadon - merge intrad and intrad_qc into single routine
!   2005-09-28  derber  - modify var qc and change location and weight arrays
!   2006-04-03  derber  - clean up code
!   2006-07-28  derber  - modify to use new inner loop obs data structure
!                       - unify NL qc
!   2007-03-19  tremolet - binning of observations
!   2007-06-04  derber  - use quad precision to get reproducability over number of processors
!   2007-06-05  tremolet - use observation diagnostics structure
!   2007-07-09  tremolet - observation sensitivity
!   2008-01-04  tremolet - Don't apply H^T if l_do_adjoint is false
!   2008-05-31  safford - rm unused vars and uses
!   2008-09-05  lueken  - merged ed's changes into q1fy09 code
!   2008-10-10  derber  - flip indices for spred and rpred
!   2008-11-28  todling  - remove quad precision; mpi_allgather is reproducible
!                        - turn FOTO optional; changed ptr%time handle
!                        - internal copy of pred's to avoid reshape in calling program
!
!   input argument list:
!     radhead  - obs type pointer to obs structure
!     st       - input temperature correction field
!     sq       - input q correction field
!     soz      - input ozone correction field
!     su       - input u correction field
!     sv       - input v correction field
!     spred    - input predictor values
!     sst      - input skin temp. vector
!     rt
!     rq
!     roz
!     ru
!     rv
!     rpred
!     rst
!
!   output argument list:
!     rt       - output t vector after inclusion of radiance info.
!     rq       - output q vector after inclusion of radiance info.
!     roz      - output ozone vector after inclusion of radiance info.
!     ru       - output u vector after inclusion of radiance info.
!     rv       - output v vector after inclusion of radiance info.
!     rpred    - output predictor vector after inclusion of radiance info.
!     rst      - output skin temp. vector after inclusion of radiance info.
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,i_kind,r_quad
  use radinfo, only: npred,jpch_rad,pg_rad,b_rad
  use obsmod, only: rad_ob_type,lsaveobsens,l_do_adjoint
  use jfunc, only: jiter,l_foto,xhat_dt,dhat_dt
  use gridmod, only: latlon11,latlon1n,nsig,nsig2,&
       nsig3p1,nsig3p2,nsig3p3
  use qcmod, only: nlnqc_iter,varqc_iter
  use constants, only: zero,half,one,tiny_r_kind,cg_term,r3600
  implicit none

! Declare passed variables
  type(rad_ob_type),pointer,intent(in):: radhead
  real(r_kind),dimension(latlon1n),intent(in):: st,sq,soz,su,sv
  real(r_kind),dimension(latlon11),intent(in):: sst
  real(r_kind),dimension(npred*jpch_rad),intent(in):: spred
  real(r_kind),dimension(latlon1n),intent(inout):: rt,rq,roz,ru,rv
  real(r_kind),dimension(latlon11),intent(inout):: rst
  real(r_quad),dimension(npred*jpch_rad),intent(inout):: rpred

! Declare local variables
  integer(i_kind) j,j1,j2,j3,j4,i1,i2,i3,i4,n,n_1,n_2,k,ic,ix,nn,jn
  integer(i_kind),dimension(nsig) :: i1n,i2n,i3n,i4n
  real(r_kind) val
  real(r_kind) w1,w2,w3,w4
  real(r_kind),dimension(nsig3p3):: tval,tdir
  real(r_kind) cg_rad,p0,wnotgross,wgross,time_rad
  type(rad_ob_type), pointer :: radptr


  radptr => radhead
  do while (associated(radptr))
     j1=radptr%ij(1)
     j2=radptr%ij(2)
     j3=radptr%ij(3)
     j4=radptr%ij(4)
     w1=radptr%wij(1)
     w2=radptr%wij(2)
     w3=radptr%wij(3)
     w4=radptr%wij(4)

     do k=1,nsig3p3
        tval(k)=zero
     end do

!  Begin Forward model
!  calculate temperature, q, ozone, sst vector at observation location
     i1n(1) = j1
     i2n(1) = j2
     i3n(1) = j3
     i4n(1) = j4
     do k=2,nsig
      i1n(k) = i1n(k-1)+latlon11
      i2n(k) = i2n(k-1)+latlon11
      i3n(k) = i3n(k-1)+latlon11
      i4n(k) = i4n(k-1)+latlon11
     enddo
!$omp parallel do private(k,i1,i2,i3,i4)
     do k=1,nsig
        i1 = i1n(k)
        i2 = i2n(k)
        i3 = i3n(k)
        i4 = i4n(k)
        tdir(k)=      w1*  st(i1)+w2*  st(i2)+ &
                      w3*  st(i3)+w4*  st(i4)
        tdir(nsig+k)= w1*  sq(i1)+w2*  sq(i2)+ &
                      w3*  sq(i3)+w4*  sq(i4)
        tdir(nsig2+k)=w1* soz(i1)+w2* soz(i2)+ &
                      w3* soz(i3)+w4* soz(i4)
     end do
!$omp end parallel do
     tdir(nsig3p1)=   w1* su(j1) +w2* su(j2)+ &
                      w3* su(j3) +w4* su(j4)
     tdir(nsig3p2)=   w1* sv(j1) +w2* sv(j2)+ &
                      w3* sv(j3) +w4* sv(j4)
     tdir(nsig3p3)=   w1*sst(j1) +w2*sst(j2)+ &
                      w3*sst(j3) +w4*sst(j4)


     if (l_foto) then
       time_rad=radptr%time*r3600
       do k=1,nsig
          i1 = i1n(k)
          i2 = i2n(k)
          i3 = i3n(k)
          i4 = i4n(k)
          tdir(k)= tdir(k)+&
                       (w1* xhat_dt%t(i1)+w2*xhat_dt%t(i2)+ &
                        w3* xhat_dt%t(i3)+w4*xhat_dt%t(i4))*time_rad
          tdir(nsig+k)= tdir(nsig+k)+&
                       (w1* xhat_dt%q(i1)+w2*xhat_dt%q(i2)+ &
                        w3* xhat_dt%q(i3)+w4*xhat_dt%q(i4))*time_rad
          tdir(nsig2+k)= tdir(nsig2+k)+&
                       (w1*xhat_dt%oz(i1)+w2*xhat_dt%oz(i2)+ &
                        w3*xhat_dt%oz(i3)+w4*xhat_dt%oz(i4))*time_rad
       end do
       tdir(nsig3p1)=   tdir(nsig3p1)+&
                       (w1*xhat_dt%u(j1) +w2*xhat_dt%u(j2)+ &
                        w3*xhat_dt%u(j3) +w4*xhat_dt%u(j4))*time_rad
       tdir(nsig3p2)=   tdir(nsig3p2)+&
                       (w1*xhat_dt%v(j1) +w2*xhat_dt%v(j2)+ &
                        w3*xhat_dt%v(j3) +w4*xhat_dt%v(j4))*time_rad

     endif

!  begin channel specific calculations
     do nn=1,radptr%nchan
        ic=radptr%icx(nn)
        ix=(ic-1)*npred

!       include observation increment and lapse rate contributions to bias correction
        val=zero

!       Include contributions from atmospheric jacobian
        do k=1,nsig3p3
           val=val+tdir(k)*radptr%dtb_dvar(k,nn)
        end do

!       Include contributions from remaining bias correction terms
        do n=1,npred
           val=val+spred(ix+n)*radptr%pred(n,nn)
        end do

        if (lsaveobsens) then
          radptr%diags(nn)%ptr%obssen(jiter) = val*radptr%err2(nn)*radptr%raterr2(nn)
        else
          if (radptr%luse) radptr%diags(nn)%ptr%tldepart(jiter) = val
        endif

       if (l_do_adjoint) then
        if (lsaveobsens) then
          val=radptr%diags(nn)%ptr%obssen(jiter)

        else
          val=val-radptr%res(nn)

!         Multiply by variance.
          if (nlnqc_iter .and. pg_rad(ic) > tiny_r_kind .and. &
                               b_rad(ic)  > tiny_r_kind) then
             cg_rad=cg_term/b_rad(ic)
             wnotgross= one-pg_rad(ic)*varqc_iter
             wgross = varqc_iter*pg_rad(ic)*cg_rad/wnotgross
             p0   = wgross/(wgross+exp(-half*radptr%err2(nn)*val*val))
             val = val*(one-p0)
          endif

          val = val*radptr%err2(nn)*radptr%raterr2(nn)
        endif

!       Begin adjoint

!       Extract contributions from atmospheric jacobian
        do k=1,nsig3p3
           tval(k)=tval(k)+radptr%dtb_dvar(k,nn)*val
        end do

!       Extract contributions from bias correction terms
!       use compensated summation
        if(radptr%luse)then
          do n=1,npred
             rpred(ix+n)=rpred(ix+n)+radptr%pred(n,nn)*val
          end do
        end if
       endif
     end do

    if (l_do_adjoint) then
!    Distribute adjoint contributions over surrounding grid points
     ru(j1)=ru(j1)+w1*tval(nsig3p1)
     ru(j2)=ru(j2)+w2*tval(nsig3p1)
     ru(j3)=ru(j3)+w3*tval(nsig3p1)
     ru(j4)=ru(j4)+w4*tval(nsig3p1)
     rv(j1)=rv(j1)+w1*tval(nsig3p2)
     rv(j2)=rv(j2)+w2*tval(nsig3p2)
     rv(j3)=rv(j3)+w3*tval(nsig3p2)
     rv(j4)=rv(j4)+w4*tval(nsig3p2)
     if (l_foto) then
       dhat_dt%u(j1)=dhat_dt%u(j1)+w1*tval(nsig3p1)*time_rad
       dhat_dt%u(j2)=dhat_dt%u(j2)+w2*tval(nsig3p1)*time_rad
       dhat_dt%u(j3)=dhat_dt%u(j3)+w3*tval(nsig3p1)*time_rad
       dhat_dt%u(j4)=dhat_dt%u(j4)+w4*tval(nsig3p1)*time_rad
       dhat_dt%v(j1)=dhat_dt%v(j1)+w1*tval(nsig3p2)*time_rad
       dhat_dt%v(j2)=dhat_dt%v(j2)+w2*tval(nsig3p2)*time_rad
       dhat_dt%v(j3)=dhat_dt%v(j3)+w3*tval(nsig3p2)*time_rad
       dhat_dt%v(j4)=dhat_dt%v(j4)+w4*tval(nsig3p2)*time_rad
     endif

     rst(j1)=rst(j1)+w1*tval(nsig3p3)
     rst(j2)=rst(j2)+w2*tval(nsig3p3)
     rst(j3)=rst(j3)+w3*tval(nsig3p3)
     rst(j4)=rst(j4)+w4*tval(nsig3p3)

     do k=1,nsig
        n_1=k+nsig
        n_2=k+nsig2
        i1 = i1n(k)
        i2 = i2n(k)
        i3 = i3n(k)
        i4 = i4n(k)

        rt(i1)=rt(i1)+w1*tval(k)
        rt(i2)=rt(i2)+w2*tval(k)
        rt(i3)=rt(i3)+w3*tval(k)
        rt(i4)=rt(i4)+w4*tval(k)
        rq(i1)=rq(i1)+w1*tval(n_1)
        rq(i2)=rq(i2)+w2*tval(n_1)
        rq(i3)=rq(i3)+w3*tval(n_1)
        rq(i4)=rq(i4)+w4*tval(n_1)
        roz(i1)=roz(i1)+w1*tval(n_2)
        roz(i2)=roz(i2)+w2*tval(n_2)
        roz(i3)=roz(i3)+w3*tval(n_2)
        roz(i4)=roz(i4)+w4*tval(n_2)
        if (l_foto) then
          dhat_dt%t(i1)=dhat_dt%t(i1)+w1*tval(k)*time_rad
          dhat_dt%t(i2)=dhat_dt%t(i2)+w2*tval(k)*time_rad
          dhat_dt%t(i3)=dhat_dt%t(i3)+w3*tval(k)*time_rad
          dhat_dt%t(i4)=dhat_dt%t(i4)+w4*tval(k)*time_rad
          dhat_dt%q(i1)=dhat_dt%q(i1)+w1*tval(n_1)*time_rad
          dhat_dt%q(i2)=dhat_dt%q(i2)+w2*tval(n_1)*time_rad
          dhat_dt%q(i3)=dhat_dt%q(i3)+w3*tval(n_1)*time_rad
          dhat_dt%q(i4)=dhat_dt%q(i4)+w4*tval(n_1)*time_rad
          dhat_dt%oz(i1)=dhat_dt%oz(i1)+w1*tval(n_2)*time_rad
          dhat_dt%oz(i2)=dhat_dt%oz(i2)+w2*tval(n_2)*time_rad
          dhat_dt%oz(i3)=dhat_dt%oz(i3)+w3*tval(n_2)*time_rad
          dhat_dt%oz(i4)=dhat_dt%oz(i4)+w4*tval(n_2)*time_rad
        endif

     end do
    endif ! < l_do_adjoint >

    radptr => radptr%llpoint
  end do

  return
end subroutine intrad_

end module intradmod
