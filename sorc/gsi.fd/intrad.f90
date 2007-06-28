module intradmod

!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    intradmod    module for intrad and its tangent linear intrad_tl
!
! abstract: module for intrad and its tangent linear intrad_tl
!
! program history log:
!   2005-05-16  Yanqiu zhu - wrap intrad and its tangent linear intrad_tl into one module
!   2005-11-16  Derber - remove interfaces
!

implicit none

PRIVATE
PUBLIC intrad,intrad_tl


contains

subroutine intrad(rt,rq,roz,ru,rv,rst,& 
                  st,sq,soz,su,sv,sst,&
                  rpred,spred)
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
!
!   input argument list:
!     st       - input temperature correction field
!     sq       - input q correction field
!     soz      - input ozone correction field
!     su       - input u correction field
!     sv       - input v correction field
!     spred    - input predictor values
!     sst      - input skin temp. vector
!
!   output argument list:
!     rt       - output vector after inclusion of radiance info.
!     rq       - output q vector after inclusion of radiance info.
!     ro       - output ozone vector after inclusion of radiance info.
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
  use kinds, only: r_kind,i_kind
  use radinfo, only: npred,npred1,jpch,pg_rad,b_rad
  use obsmod, only: radptr,radhead
  use gridmod, only: latlon11,latlon1n,nsig,nsig2,nsig3,nsig4,&
       nsig3p1,nsig3p2,nsig3p3
  use qcmod, only: nlnqc_iter
  use constants, only: zero,half,one,tiny_r_kind,cg_term
  implicit none

! Declare passed variables
  real(r_kind),dimension(latlon1n),intent(in):: st,sq,soz,su,sv
  real(r_kind),dimension(latlon11),intent(in):: sst
  real(r_kind),dimension(jpch,npred),intent(in):: spred
  real(r_kind),dimension(latlon1n),intent(inout):: rt,rq,roz,ru,rv
  real(r_kind),dimension(latlon11),intent(inout):: rst
  real(r_kind),dimension(jpch,npred),intent(inout):: rpred

! Declare local variables
  integer(i_kind) i,j1,j2,j3,j4,i1,i2,i3,i4,n,n_1,n_2,k,ic,nn
  real(r_kind) valx,tlap,val,tlap2,w1,w2,w3,w4
! real(r_kind) penalty,p1
  real(r_kind),dimension(nsig3p3):: tval,tdir
  real(r_kind) cg_rad,p0,wnotgross,wgross

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

!    p1=zero
     do k=1,nsig3p3
        tval(k)=zero
     end do
     i1=j1
     i2=j2
     i3=j3
     i4=j4

!  Begin Forward model
!  calculate temperature, q, ozone, sst vector at observation location
     do k=1,nsig
        tdir(k)=      w1*st(i1) +w2*st(i2)+ &
                      w3*st(i3) +w4*st(i4)
        tdir(nsig+k)= w1*sq(i1) +w2*sq(i2)+ &
                      w3*sq(i3) +w4*sq(i4)
        tdir(nsig2+k)=w1*soz(i1)+w2*soz(i2)+ &
                      w3*soz(i3)+w4*soz(i4)
        i1=i1+latlon11
        i2=i2+latlon11
        i3=i3+latlon11
        i4=i4+latlon11
     end do
     tdir(nsig3p1)=   w1*su(j1) +w2*su(j2)+ &
                      w3*su(j3) +w4*su(j4)
     tdir(nsig3p2)=   w1*sv(j1) +w2*sv(j2)+ &
                      w3*sv(j3) +w4*sv(j4)
     tdir(nsig3p3)=   w1*sst(j1)+w2*sst(j2)+ &
                      w3*sst(j3)+w4*sst(j4)
!  begin channel specific calculations
     do nn=1,radptr%nchan
        ic=radptr%icx(nn)

!       include observation increment and lapse rate contributions to bias correction
        tlap=radptr%pred2(nn)
        tlap2=tlap*tlap
        valx=-radptr%res(nn)+spred(ic,npred)*tlap+spred(ic,npred1)*tlap2

!       Include contributions from atmospheric jacobian
        do k=1,nsig3p3
           valx=valx+tdir(k)*radptr%dtb_dvar(k,nn)
        end do

!       Include contributions from remaining bias correction terms
        do n=1,npred-2
           valx=valx+spred(ic,n)*radptr%pred1(n)
        end do

!       Multiply by variance.
        if (nlnqc_iter .and. pg_rad(ic) > tiny_r_kind .and. &
                             b_rad(ic)  > tiny_r_kind) then
           cg_rad=cg_term/b_rad(ic)
           wnotgross= one-pg_rad(ic)
           wgross = pg_rad(ic)*cg_rad/wnotgross
           p0   = wgross/(wgross+exp(-half*radptr%err2(nn)*valx*valx))
           valx = valx*(one-p0)
        endif

        val      = valx*radptr%err2(nn)
        val      = val*radptr%raterr2(nn)

!       Begin adjoint

!       Extract contributions from atmospheric jacobian
        do k=1,nsig3p3
           tval(k)=tval(k)+radptr%dtb_dvar(k,nn)*val
        end do

!       Extract contributions from bias correction terms
        if(radptr%luse)then
          do n=1,npred-2
             rpred(ic,n)=rpred(ic,n)+radptr%pred1(n)*val
          end do
          rpred(ic,npred) =rpred(ic,npred) +val*tlap
          rpred(ic,npred1)=rpred(ic,npred1)+val*tlap2
        end if
     end do


!    Distribute adjoint contributions over surrounding grid points
     ru(j1)=ru(j1)+w1*tval(nsig3p1)
     ru(j2)=ru(j2)+w2*tval(nsig3p1)
     ru(j3)=ru(j3)+w3*tval(nsig3p1)
     ru(j4)=ru(j4)+w4*tval(nsig3p1)
 
     rv(j1)=rv(j1)+w1*tval(nsig3p2)
     rv(j2)=rv(j2)+w2*tval(nsig3p2)
     rv(j3)=rv(j3)+w3*tval(nsig3p2)
     rv(j4)=rv(j4)+w4*tval(nsig3p2)

     rst(j1)=rst(j1)+w1*tval(nsig3p3)
     rst(j2)=rst(j2)+w2*tval(nsig3p3)
     rst(j3)=rst(j3)+w3*tval(nsig3p3)
     rst(j4)=rst(j4)+w4*tval(nsig3p3)

     do k=1,nsig
        n_1=k+nsig
        n_2=k+nsig2

        rt(j1)=rt(j1)+w1*tval(k)
        rt(j2)=rt(j2)+w2*tval(k)
        rt(j3)=rt(j3)+w3*tval(k)
        rt(j4)=rt(j4)+w4*tval(k)

        rq(j1)=rq(j1)+w1*tval(n_1)
        rq(j2)=rq(j2)+w2*tval(n_1)
        rq(j3)=rq(j3)+w3*tval(n_1)
        rq(j4)=rq(j4)+w4*tval(n_1)

        roz(j1)=roz(j1)+w1*tval(n_2)
        roz(j2)=roz(j2)+w2*tval(n_2)
        roz(j3)=roz(j3)+w3*tval(n_2)
        roz(j4)=roz(j4)+w4*tval(n_2)

        j1=j1+latlon11
        j2=j2+latlon11
        j3=j3+latlon11
        j4=j4+latlon11
     end do
     radptr => radptr%llpoint
  end do
  return
end subroutine intrad


subroutine intrad_tl(rt,rq,roz,ru,rv,rst,& 
     st,sq,soz,su,sv,sst,rpred,spred, &
     rt_tl,rq_tl,roz_tl,ru_tl,rv_tl,rst_tl,&
     st_tl,sq_tl,soz_tl,su_tl,sv_tl,sst_tl,rpred_tl,spred_tl)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    intrad_tl      the tangent linear of sat radiance nonlin qc obs operator
!   prgmmr: yanqiu zhu          org: GMAO                date: 2005-05-16
!
! abstract: the tangent linear of the operator that applies satellite radiance operator 
!           and adjoint with addition of nonlinear qc operator.
!
! program history log:
!   2005-05-16  yanqiu zhu - tangent linear of intrad
!
!   input argument list:
!     st       - input temperature correction field
!     sq       - input q correction field
!     soz      - input ozone correction field
!     ru       - output u vector after inclusion of radiance info.
!     rv       - output v vector after inclusion of radiance info.
!     spred    - input predictor values
!     sst      - input skin temp. vector
!     st_tl     - input tangent linear temperature correction field
!     sq_tl     - input tangent linear q correction field
!     soz_tl    - input tangent linear ozone correction field
!     ru_tl     - output tangent linear u vector after inclusion of radiance info.
!     rv_tl     - output tangent linear v vector after inclusion of radiance info.
!     spred_tl  - input tangent linear predictor values
!     sst_tl    - input tangent linear skin temp. vector
!
!   output argument list:
!     rt       - output vector after inclusion of radiance info.
!     rq       - output q vector after inclusion of radiance info.
!     roz      - output ozone vector after inclusion of radiance info.
!     ru       - output u vector after inclusion of radiance info.
!     rv       - output v vector after inclusion of radiance info.
!     rpred    - output predictor vector after inclusion of radiance info.
!     rst      - output skin temp. vector after inclusion of radiance info.
!     rt_tl     - output tangent linear vector after inclusion of radiance info.
!     rq_tl     - output tangent linear q vector after inclusion of radiance info.
!     roz_tl    - output tangent linear ozone vector after inclusion of radiance info.
!     ru_tl     - output tangent linear u vector after inclusion of radiance info.
!     rv_tl     - output tangent linear v vector after inclusion of radiance info.
!     rpred_tl  - output tangent linear predictor vector after inclusion of radiance info.
!     rst_tl    - output tangent linear skin temp. vector after inclusion of radiance info.
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,i_kind
  use radinfo, only: npred,npred1,jpch,b_rad,pg_rad
  use obsmod, only: radptr,radhead
  use obsmod_tl, only: rad_inv_tl
  use gridmod, only: latlon11,latlon1n,nsig,nsig2,nsig3,nsig4,&
       nsig3p1,nsig3p2,nsig3p3
  use qcmod, only: nlnqc_iter
  use constants, only: zero,half,one,tiny_r_kind,cg_term
  implicit none

! Declare passed variables
  real(r_kind),dimension(latlon1n),intent(in):: st,sq,soz,su,sv
  real(r_kind),dimension(latlon1n),intent(in):: st_tl,sq_tl,soz_tl,su_tl,sv_tl
  real(r_kind),dimension(latlon11),intent(in):: sst
  real(r_kind),dimension(latlon11),intent(in):: sst_tl
  real(r_kind),dimension(jpch,npred),intent(in):: spred
  real(r_kind),dimension(jpch,npred),intent(in):: spred_tl
  real(r_kind),dimension(latlon1n),intent(inout):: rt,rq,roz,ru,rv
  real(r_kind),dimension(latlon1n),intent(inout):: rt_tl,rq_tl,roz_tl,ru_tl,rv_tl
  real(r_kind),dimension(latlon11),intent(inout):: rst
  real(r_kind),dimension(latlon11),intent(inout):: rst_tl
  real(r_kind),dimension(jpch,npred),intent(inout):: rpred
  real(r_kind),dimension(jpch,npred),intent(inout):: rpred_tl

! Declare local variables
  integer(i_kind) i,j1,j2,j3,j4,i1,i2,i3,i4,n,n_1,n_2,k,ic,nn
  real(r_kind) valx,tlap,val,tlap2,w1,w2,w3,w4
  real(r_kind) valx_tl,val_tl
! real(r_kind) penalty,p1
  real(r_kind),dimension(nsig3p3):: tval,tdir
  real(r_kind),dimension(nsig3p3):: tval_tl,tdir_tl
  real(r_kind) cg_rad,p0,wnotgross,wgross,term
  real(r_kind) p0_tl,term_tl

  radptr => radhead
  i=0
  do while (associated(radptr))
     i=i+1
     j1=radptr%ij(1)
     j2=radptr%ij(2)
     j3=radptr%ij(3)
     j4=radptr%ij(4)
     w1=radptr%wij(1)
     w2=radptr%wij(2)
     w3=radptr%wij(3)
     w4=radptr%wij(4)
!    p1=zero
     do k=1,nsig3p3
        tval(k)=zero
        tval_tl(k)=zero
     end do
     i1=j1-latlon11
     i2=j2-latlon11
     i3=j3-latlon11
     i4=j4-latlon11

!  Begin Forward model
!  calculate temperature, q, ozone, sst vector at observation location
     do k=1,nsig
        i1=i1+latlon11
        i2=i2+latlon11
        i3=i3+latlon11
        i4=i4+latlon11
        tdir(k)=      w1*st(i1) +w2*st(i2)+ w3*st(i3) +w4*st(i4)
        tdir(nsig+k)= w1*sq(i1) +w2*sq(i2)+ w3*sq(i3) +w4*sq(i4)
        tdir(nsig2+k)=w1*soz(i1)+w2*soz(i2)+ w3*soz(i3)+w4*soz(i4)
        tdir_tl(k)=      w1*st_tl(i1) +w2*st_tl(i2)+w3*st_tl(i3) +w4*st_tl(i4)
        tdir_tl(nsig+k)= w1*sq_tl(i1) +w2*sq_tl(i2)+w3*sq_tl(i3) +w4*sq_tl(i4)
        tdir_tl(nsig2+k)=w1*soz_tl(i1)+w2*soz_tl(i2)+w3*soz_tl(i3)+w4*soz_tl(i4)
     end do
     tdir(nsig3p1)=   w1*su(j1) +w2*su(j2)+ w3*su(j3) +w4*su(j4)
     tdir(nsig3p2)=   w1*sv(j1) +w2*sv(j2)+ w3*sv(j3) +w4*sv(j4)
     tdir(nsig3p3)=   w1*sst(j1)+w2*sst(j2)+w3*sst(j3)+w4*sst(j4)
     tdir_tl(nsig3p1)= w1*su_tl(j1) +w2*su_tl(j2)+ w3*su_tl(j3) +w4*su_tl(j4)
     tdir_tl(nsig3p2)= w1*sv_tl(j1) +w2*sv_tl(j2)+ w3*sv_tl(j3) +w4*sv_tl(j4)
     tdir_tl(nsig3p3)= w1*sst_tl(j1)+w2*sst_tl(j2)+w3*sst_tl(j3)+w4*sst_tl(j4)
!  begin channel specific calculations
     do nn=1,radptr%nchan
        ic=radptr%icx(nn)

!       include observation increment and lapse rate contributions to bias correction
        tlap=radptr%pred2(nn)
        tlap2=tlap*tlap
        valx=-radptr%res(nn)+spred(ic,npred)*tlap+spred(ic,npred1)*tlap2
        valx_tl=-rad_inv_tl(nn)+spred_tl(ic,npred)*tlap+spred_tl(ic,npred1)*tlap2

!       Include contributions from atmospheric jacobian
        do k=1,nsig3p3
           valx=valx+tdir(k)*radptr%dtb_dvar(k,nn)
           valx_tl=valx_tl+tdir_tl(k)*radptr%dtb_dvar(k,nn)
        end do

!       Include contributions from remaining bias correction terms
        do n=1,npred-2
           valx=valx+spred(ic,n)*radptr%pred1(n)
           valx_tl=valx_tl+spred_tl(ic,n)*radptr%pred1(n)
        end do

!       Multiply by variance.
        if (pg_rad(ic) > tiny_r_kind .and. nlnqc_iter) then
           cg_rad=cg_term/b_rad(ic)
           wnotgross= one-pg_rad(ic)
           wgross = pg_rad(ic)*cg_rad
           p0   = wnotgross*exp(-half*radptr%err2(nn)*valx*valx)+wgross
           term = (p0-wgross)/p0
           p0_tl = -radptr%err2(nn)*valx*(p0-wgross)*valx_tl
           term_tl = wgross/(p0*p0)*p0_tl
        else
           term = one
           term_tl = zero
        endif
        val      = valx*radptr%err2(nn)*term
        val_tl    = valx_tl*radptr%err2(nn)*term + valx*radptr%err2(nn)*term_tl
        val      = val*radptr%raterr2(nn)
        val_tl    = val_tl*radptr%raterr2(nn)


!       Begin adjoint

!       Extract contributions from atmospheric jacobian
        do k=1,nsig3p3
           tval(k)=tval(k)+radptr%dtb_dvar(k,nn)*val
           tval_tl(k)=tval_tl(k)+radptr%dtb_dvar(k,nn)*val_tl
        end do

!       Extract contributions from bias correction terms
        if(radptr%luse)then
          do n=1,npred-2
             rpred(ic,n)=rpred(ic,n)+radptr%pred1(n)*val
             rpred_tl(ic,n)=rpred_tl(ic,n)+radptr%pred1(n)*val_tl
          end do
          rpred(ic,npred) =rpred(ic,npred) +val*tlap
          rpred(ic,npred1)=rpred(ic,npred1)+val*tlap2
          rpred_tl(ic,npred) =rpred_tl(ic,npred) +val_tl*tlap
          rpred_tl(ic,npred1)=rpred_tl(ic,npred1)+val_tl*tlap2
        end if
     end do


!    Distribute adjoint contributions over surrounding grid points
     ru(j1)=ru(j1)+w1*tval(nsig3p1)
     ru(j2)=ru(j2)+w2*tval(nsig3p1)
     ru(j3)=ru(j3)+w3*tval(nsig3p1)
     ru(j4)=ru(j4)+w4*tval(nsig3p1)
 
     rv(j1)=rv(j1)+w1*tval(nsig3p2)
     rv(j2)=rv(j2)+w2*tval(nsig3p2)
     rv(j3)=rv(j3)+w3*tval(nsig3p2)
     rv(j4)=rv(j4)+w4*tval(nsig3p2)

     rst(j1)=rst(j1)+w1*tval(nsig3p3)
     rst(j2)=rst(j2)+w2*tval(nsig3p3)
     rst(j3)=rst(j3)+w3*tval(nsig3p3)
     rst(j4)=rst(j4)+w4*tval(nsig3p3)

     ru_tl(j1)=ru_tl(j1)+w1*tval_tl(nsig3p1)
     ru_tl(j2)=ru_tl(j2)+w2*tval_tl(nsig3p1)
     ru_tl(j3)=ru_tl(j3)+w3*tval_tl(nsig3p1)
     ru_tl(j4)=ru_tl(j4)+w4*tval_tl(nsig3p1)

     rv_tl(j1)=rv_tl(j1)+w1*tval_tl(nsig3p2)
     rv_tl(j2)=rv_tl(j2)+w2*tval_tl(nsig3p2)
     rv_tl(j3)=rv_tl(j3)+w3*tval_tl(nsig3p2)
     rv_tl(j4)=rv_tl(j4)+w4*tval_tl(nsig3p2)

     rst_tl(j1)=rst_tl(j1)+w1*tval_tl(nsig3p3)
     rst_tl(j2)=rst_tl(j2)+w2*tval_tl(nsig3p3)
     rst_tl(j3)=rst_tl(j3)+w3*tval_tl(nsig3p3)
     rst_tl(j4)=rst_tl(j4)+w4*tval_tl(nsig3p3)

     do k=1,nsig
        n_1=k+nsig
        n_2=k+nsig2

        rt(j1)=rt(j1)+w1*tval(k)
        rt(j2)=rt(j2)+w2*tval(k)
        rt(j3)=rt(j3)+w3*tval(k)
        rt(j4)=rt(j4)+w4*tval(k)

        rq(j1)=rq(j1)+w1*tval(n_1)
        rq(j2)=rq(j2)+w2*tval(n_1)
        rq(j3)=rq(j3)+w3*tval(n_1)
        rq(j4)=rq(j4)+w4*tval(n_1)

        roz(j1)=roz(j1)+w1*tval(n_2)
        roz(j2)=roz(j2)+w2*tval(n_2)
        roz(j3)=roz(j3)+w3*tval(n_2)
        roz(j4)=roz(j4)+w4*tval(n_2)

        rt_tl(j1)=rt_tl(j1)+w1*tval_tl(k)
        rt_tl(j2)=rt_tl(j2)+w2*tval_tl(k)
        rt_tl(j3)=rt_tl(j3)+w3*tval_tl(k)
        rt_tl(j4)=rt_tl(j4)+w4*tval_tl(k)

        rq_tl(j1)=rq_tl(j1)+w1*tval_tl(n_1)
        rq_tl(j2)=rq_tl(j2)+w2*tval_tl(n_1)
        rq_tl(j3)=rq_tl(j3)+w3*tval_tl(n_1)
        rq_tl(j4)=rq_tl(j4)+w4*tval_tl(n_1)

        roz_tl(j1)=roz_tl(j1)+w1*tval_tl(n_2)
        roz_tl(j2)=roz_tl(j2)+w2*tval_tl(n_2)
        roz_tl(j3)=roz_tl(j3)+w3*tval_tl(n_2)
        roz_tl(j4)=roz_tl(j4)+w4*tval_tl(n_2)
        j1=j1+latlon11
        j2=j2+latlon11
        j3=j3+latlon11
        j4=j4+latlon11
     end do
     radptr=> radptr%llpoint
  end do
  return
end subroutine intrad_tl

end module intradmod
