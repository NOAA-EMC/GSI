module stpradmod

!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    stpradmod    module for stprad and its tangent linear stprad_tl
!
! abstract: module for stprad and its tangent linear stprad_tl
!
! program history log:
!   2005-05-20  Yanqiu zhu - wrap stprad and its tangent linear stprad_tl into one module
!   2005-11-16  Derber - remove interfaces
!

implicit none

PRIVATE
PUBLIC stprad,stprad_tl

contains

subroutine stprad(rt,rq,roz,ru,rv,rst,st,sq,soz,su,sv,sst, &
     rpred,spred,pen,b1,b3,sges1,sges2,sges3)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    stprad compute contribution to penalty and stepsize
!                from rad, using nonlinear qc.
!   prgmmr: parrish          org: np22                date: 1990-10-11
!
! abstract: compute contribution to penalty and stepsize from radiances.
!
! program history log:
!   1990-10-11  parrish
!   1992-07-21
!   1995-07-17  derber
!   1997-03-10  wu       
!   1998-02-02  weiyu yang
!   1999-08-24  derber, j., treadon, r., yang, w., first frozen mpp version
!   2004-07-30  treadon - add only to module use, add intent in/out
!   2004-10-07  parrish - add nonlinear qc option
!   2005-01-20  okamoto - add wind components
!   2005-04-11  treadon - merge stprad and stprad_qc into single routine
!   2005-09-28  derber  - modify var qc and change location and weight arrays
!   2007-07-28  derber  - modify to use new inner loop obs data structure
!                       - unify NL qc
!
!   input argument list:
!     rt       - search direction for temperature
!     rq       - search direction for moisture 
!     roz      - search direction for ozone
!     ru       - search direction for zonal wind
!     rv       - search direction for meridional wind
!     rst      - search direction for skin temperature
!     st       - input temperature correction field        
!     sq       - input q correction field        
!     soz      - input ozone correction field        
!     su       - input u correction field
!     sv       - input v correction field
!     sst      - input skin temp. vector 
!     rpred    - search direction for predictors
!     spred    - input predictor values
!     sges1    - estimate step size 1
!     sges2    - estimate step size 2
!     sges3    - estimate step size 3
!
!   output argument list:
!     pen      - penalty for radiance data
!     b1       - pen(sges1)-pen(sges2)
!     b3       - pen(sges3)-pen(sges2)
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,i_kind
  use radinfo, only: npred1,npred,jpch,b_rad,pg_rad
  use obsmod, only: radptr,radhead
  use qcmod, only: nlnqc_iter
  use constants, only: zero,half,one,two,tiny_r_kind,cg_term
  use gridmod, only: nsig,nsig2,nsig3,nsig4,nsig3p1,nsig3p2,nsig3p3,&
       latlon11,latlon1n,lat1,lon1
  implicit none
  
! Declare passed variables
  real(r_kind),intent(out):: pen,b1,b3
  real(r_kind),dimension(latlon1n),intent(in):: rt,st,rq,sq,roz,soz,&
      ru,su,rv,sv
  real(r_kind),dimension(latlon11),intent(in):: rst,sst
  real(r_kind),dimension(jpch,npred),intent(in):: rpred,spred
  real(r_kind),intent(in):: sges1,sges2,sges3

! Declare local variables
  integer(i_kind) nn,n,ic,k,nx,n_1,n_2,n_3,n_4,j1,j2,j3,j4,i
  real(r_kind) alpha,ccoef,bcoef1,bcoef2,cc
  real(r_kind) val2,tlap2,val,tlap,w1,w2,w3,w4
  real(r_kind),dimension(nsig3p3):: tdir,rdir
  real(r_kind) cg_rad,rad1,rad2,rad3,pen1,pen3,pencur,wgross,wnotgross
  real(r_kind) term,term1,term2,term3
  real(r_kind) exp_arg,exp_arg1,exp_arg2,exp_arg3,halfvar_rad

  pen=zero
  b1=zero; b3=zero
  alpha=one/(sges2-sges1)
  ccoef=half*alpha*alpha
  bcoef1=half*half*alpha
  bcoef2=sges2*ccoef

  radptr=>radhead
  do while(associated(radptr))
    if(radptr%luse)then
     j1=radptr%ij(1)
     j2=radptr%ij(2)
     j3=radptr%ij(3)
     j4=radptr%ij(4)
     w1=radptr%wij(1)
     w2=radptr%wij(2)
     w3=radptr%wij(3)
     w4=radptr%wij(4)
     n_1=nsig; n_2=nsig2
     tdir(nsig3p1)=w1*su(j1)  + w2*su(j2) + w3*su(j3)  + w4*su(j4)
     rdir(nsig3p1)=w1*ru(j1)  + w2*ru(j2) + w3*ru(j3)  + w4*ru(j4)
     tdir(nsig3p2)=w1*sv(j1)  + w2*sv(j2) + w3*sv(j3)  + w4*sv(j4)
     rdir(nsig3p2)=w1*rv(j1)  + w2*rv(j2) + w3*rv(j3)  + w4*rv(j4)
     tdir(nsig3p3)=w1*sst(j1) + w2*sst(j2)+ w3*sst(j3) + w4*sst(j4)
     rdir(nsig3p3)=w1*rst(j1) + w2*rst(j2)+ w3*rst(j3) + w4*rst(j4)

     do n=1,nsig
        n_1=n_1+1; n_2=n_2+1

!       Input state vector
        tdir(n)=  w1*st(j1) +w2*st(j2) + w3*st(j3) +w4*st(j4)
        tdir(n_1)=w1*sq(j1) +w2*sq(j2) + w3*sq(j3) +w4*sq(j4)
        tdir(n_2)=w1*soz(j1)+w2*soz(j2)+ w3*soz(j3)+w4*soz(j4)

!       Input search direction vector
        rdir(n)=  w1*rt(j1) +w2*rt(j2) + w3*rt(j3) +w4*rt(j4)
        rdir(n_1)=w1*rq(j1) +w2*rq(j2) + w3*rq(j3) +w4*rq(j4)
        rdir(n_2)=w1*roz(j1)+w2*roz(j2)+ w3*roz(j3)+w4*roz(j4)

        j1=j1+latlon11
        j3=j3+latlon11
        j2=j2+latlon11
        j4=j4+latlon11
     end do
     pencur=zero
     pen1=zero
     pen3=zero
     do nn=1,radptr%nchan
        ic=radptr%icx(nn)

!       contribution from bias corection
        tlap=radptr%pred2(nn)
        tlap2=tlap*tlap
        val2=-radptr%res(nn)+spred(ic,npred)*tlap+spred(ic,npred1)*tlap2
        val=                 rpred(ic,npred)*tlap+rpred(ic,npred1)*tlap2

        do nx=1,npred-2
           val2=val2+spred(ic,nx)*radptr%pred1(nx)
           val =val +rpred(ic,nx)*radptr%pred1(nx)
        end do

!       contribution from atmosphere
        do k=1,nsig3p3
           val2=val2+tdir(k)*radptr%dtb_dvar(k,nn)
           val =val +rdir(k)*radptr%dtb_dvar(k,nn)
        end do

!       multiply by variance
        rad1=val2+sges1*val
        rad2=val2+sges2*val
        rad3=val2+sges3*val
        
        term  = radptr%err2(nn)*val2*val2
        term1 = radptr%err2(nn)*rad1*rad1
        term2 = radptr%err2(nn)*rad2*rad2
        term3 = radptr%err2(nn)*rad3*rad3
        
!  Modify penalty term if nonlinear QC
        if(nlnqc_iter .and. pg_rad(ic) > tiny_r_kind .and. &
                            b_rad(ic)  > tiny_r_kind)then
           cg_rad=cg_term/b_rad(ic)
           wnotgross= one-pg_rad(ic)
           wgross = pg_rad(ic)*cg_rad/wnotgross
           term  = -two*log((exp(-half*term ) + wgross)/(one+wgross))
           term1 = -two*log((exp(-half*term1) + wgross)/(one+wgross))
           term2 = -two*log((exp(-half*term2) + wgross)/(one+wgross))
           term3 = -two*log((exp(-half*term3) + wgross)/(one+wgross))
        endif

        pencur = pencur + term*radptr%raterr2(nn)
        pen1   = pen1   + (term1-term2)*radptr%raterr2(nn)
        pen3   = pen3   + (term3-term2)*radptr%raterr2(nn)

     end do
     pen = pen+pencur
     cc  = pen1+pen3
     b1  = b1+(pen1-pen3)*bcoef1+cc*bcoef2
     b3  = b3+cc*ccoef

    end if

    radptr => radptr%llpoint
  end do
  return
end subroutine stprad


subroutine stprad_tl(rt,rq,roz,ru,rv,rst,st,sq,soz,su,sv,sst, &
     rpred,spred,pen,b1,b3,sges1,sges2,sges3, &
     rt_tl,rq_tl,roz_tl,ru_tl,rv_tl,rst_tl,st_tl,sq_tl,soz_tl,su_tl,sv_tl,sst_tl, &
     rpred_tl,spred_tl,pen_tl,b1_tl,b3_tl,sges1_tl,sges2_tl,sges3_tl)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    stprad_tl is the tangent linear of the operator that computes 
!                contribution to penalty and stepsize from rad, using nonlinear qc.
!   prgmmr: yanqiu zhu          org: GMAO                date: 2005-05-20
!
! abstract: the tangent linear of the operator that computes contribution to 
!           penalty and stepsize from radiances.
!
! program history log:
!   2005-05-20  yanqiu zhu - tangent linear of stprad
!
!   input argument list:
!     rt       - search direction for temperature
!     rq       - search direction for moisture 
!     roz      - search direction for ozone
!     ru       - search direction for zonal wind
!     rv       - search direction for meridional wind
!     rst      - search direction for skin temperature
!     st       - input temperature correction field        
!     sq       - input q correction field        
!     soz      - input ozone correction field        
!     su       - input u correction field
!     sv       - input v correction field
!     sst      - input skin temp. vector 
!     rpred    - search direction for predictors
!     spred    - input predictor values
!     sges1    - estimate step size 1
!     sges2    - estimate step size 2
!     sges3    - estimate step size 3
!     rt_tl       - tangent linear search direction for temperature
!     rq_tl       - tangent linear search direction for moisture
!     roz_tl      - tangent linear search direction for ozone
!     ru_tl       - tangent linear search direction for zonal wind
!     rv_tl       - tangent linear search direction for meridional wind
!     rst_tl      - tangent linear search direction for skin temperature
!     st_tl       - input tangent linear temperature correction field
!     sq_tl       - input tangent linear q correction field
!     soz_tl      - input tangent linear ozone correction field
!     su_tl       - input tangent linear u correction field
!     sv_tl       - input tangent linear v correction field
!     sst_tl      - input tangent linear skin temp. vector
!     rpred_tl    - tangent linear search direction for predictors
!     spred_tl    - input tangent linear predictor values
!     sges1_tl    - tangent linear estimate step size 1
!     sges2_tl    - tangent linear estimate step size 2
!     sges3_tl    - tangent linear estimate step size 3
!
!   output argument list:
!     pen      - penalty for radiance data
!     b1       - pen(sges1)-pen(sges2)
!     b3       - pen(sges3)-pen(sges2)
!     pen_tl      - tangent linear of the penalty for radiance data
!     b1_tl       - pen_tl(sges1)-pen_tl(sges2)
!     b3_tl       - pen_tl(sges3)-pen_tl(sges2)
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,i_kind
  use radinfo, only: npred1,npred,jpch,b_rad,pg_rad
  use obsmod, only: radptr,radhead
  use obsmod_tl, only: rad_inv_tl
  use qcmod, only: nlnqc_iter
  use constants, only: zero,half,one,two,tiny_r_kind,cg_term
  use gridmod, only: nsig,nsig2,nsig3,nsig4,nsig3p1,nsig3p2,nsig3p3,&
       latlon11,latlon1n,lat1,lon1
  implicit none
  
! Declare passed variables
  real(r_kind),intent(out):: pen,b1,b3
  real(r_kind),intent(out):: pen_tl,b1_tl,b3_tl
  real(r_kind),dimension(latlon1n),intent(in):: rt,st,rq,sq,roz,soz,&
      ru,su,rv,sv
  real(r_kind),dimension(latlon1n),intent(in):: rt_tl,st_tl,rq_tl,sq_tl,roz_tl,soz_tl,&
      ru_tl,su_tl,rv_tl,sv_tl
  real(r_kind),dimension(latlon11),intent(in):: rst,sst
  real(r_kind),dimension(latlon11),intent(in):: rst_tl,sst_tl
  real(r_kind),dimension(jpch,npred),intent(in):: rpred,spred
  real(r_kind),dimension(jpch,npred),intent(in):: rpred_tl,spred_tl
  real(r_kind),intent(in):: sges1,sges2,sges3
  real(r_kind),intent(in):: sges1_tl,sges2_tl,sges3_tl

! Declare local variables
  integer(i_kind) nn,n,ic,k,nx,n_1,n_2,n_3,n_4,j1,j2,j3,j4,i
  real(r_kind) val2,tlap2,val,tlap
  real(r_kind) val2_tl,val_tl
  real(r_kind),dimension(nsig3p3):: tdir,rdir
  real(r_kind),dimension(nsig3p3):: tdir_tl,rdir_tl
  real(r_kind) cg_rad,rad1,rad2,rad3,pen1,pen2,pen3,pencur,wgross,wnotgross
  real(r_kind) rad1_tl,rad2_tl,rad3_tl,pen1_tl,pen2_tl,pen3_tl,pencur_tl
  real(r_kind) term,term1,term2,term3,w1,w2,w3,w4
  real(r_kind) term_tl,term1_tl,term2_tl,term3_tl
  real(r_kind) exp_arg,exp_arg1,exp_arg2,exp_arg3,halfvar_rad
  real(r_kind) exp_arg_tl,exp_arg1_tl,exp_arg2_tl,exp_arg3_tl
  real(r_kind) temp

  pen=zero
  b1=zero; b3=zero
  pen_tl=zero
  b1_tl=zero; b3_tl=zero

  radptr=>radhead
  i=0
  do while(associated(radptr))
    i=i+1
    if(radptr%luse)then
     j1=radptr%ij(1)
     j2=radptr%ij(2)
     j3=radptr%ij(3)
     j4=radptr%ij(4)
     w1=radptr%wij(1)
     w2=radptr%wij(2)
     w3=radptr%wij(3)
     w4=radptr%wij(4)
     n_1=nsig; n_2=nsig2
     tdir(nsig3p1)=w1*su(j1)  + w2*su(j2) + w3*su(j3)  + w4*su(j4)
     rdir(nsig3p1)=w1*ru(j1)  + w2*ru(j2) + w3*ru(j3)  + w4*ru(j4)
     tdir(nsig3p2)=w1*sv(j1)  + w2*sv(j2) + w3*sv(j3)  + w4*sv(j4)
     rdir(nsig3p2)=w1*rv(j1)  + w2*rv(j2) + w3*rv(j3)  + w4*rv(j4)
     tdir(nsig3p3)=w1*sst(j1) + w2*sst(j2)+ w3*sst(j3) + w4*sst(j4)
     rdir(nsig3p3)=w1*rst(j1) + w2*rst(j2)+ w3*rst(j3) + w4*rst(j4)
     tdir_tl(nsig3p1)=w1*su_tl(j1)  + w2*su_tl(j2)+ &
                      w3*su_tl(j3)  + w4*su_tl(j4)
     rdir_tl(nsig3p1)=w1*ru_tl(j1)  + w2*ru_tl(j2)+ &
                      w3*ru_tl(j3)  + w4*ru_tl(j4)
     tdir_tl(nsig3p2)=w1*sv_tl(j1)  + w2*sv_tl(j2)+ &
                      w3*sv_tl(j3)  + w4*sv_tl(j4)
     rdir_tl(nsig3p2)=w1*rv_tl(j1)  + w2*rv_tl(j2)+ &
                      w3*rv_tl(j3)  + w4*rv_tl(j4)
     tdir_tl(nsig3p3)=w1*sst_tl(j1) + w2*sst_tl(j2) + &
                      w3*sst_tl(j3) + w4*sst_tl(j4)
     rdir_tl(nsig3p3)=w1*rst_tl(j1) + w2*rst_tl(j2) + &
                      w3*rst_tl(j3) + w4*rst_tl(j4)

     do n=1,nsig
        n_1=n_1+1; n_2=n_2+1

!       Input state vector
        tdir(n)=  w1*st(j1) +w2*st(j2)+ w3*st(j3) +w4*st(j4)
        tdir(n_1)=w1*sq(j1) +w2*sq(j2)+ w3*sq(j3) +w4*sq(j4)
        tdir(n_2)=w1*soz(j1)+w2*soz(j2)+w3*soz(j3)+w4*soz(j4)
        tdir_tl(n)=  w1*st_tl(j1) +w2*st_tl(j2)+ &
                     w3*st_tl(j3) +w4*st_tl(j4)
        tdir_tl(n_1)=w1*sq_tl(j1) +w2*sq_tl(j2)+ &
                     w3*sq_tl(j3) +w4*sq_tl(j4)
        tdir_tl(n_2)=w1*soz_tl(j1)+w2*soz_tl(j2)+ &
                     w3*soz_tl(j3)+w4*soz_tl(j4)

!       Input search direction vector
        rdir(n)=  w1*rt(j1) +w2*rt(j2) + &
                  w3*rt(j3) +w4*rt(j4)
        rdir(n_1)=w1*rq(j1) +w2*rq(j2)+ &
                  w3*rq(j3) +w4*rq(j4)
        rdir(n_2)=w1*roz(j1)+w2*roz(j2)+ &
                  w3*roz(j3)+w4*roz(j4)
        rdir_tl(n)=  w1*rt_tl(j1) +w2*rt_tl(j2) + &
                     w3*rt_tl(j3) +w4*rt_tl(j4)
        rdir_tl(n_1)=w1*rq_tl(j1) +w2*rq_tl(j2)+ &
                     w3*rq_tl(j3) +w4*rq_tl(j4)
        rdir_tl(n_2)=w1*roz_tl(j1)+w2*roz_tl(j2)+ &
                     w3*roz_tl(j3)+w4*roz_tl(j4)

        j1=j1+latlon11
        j3=j3+latlon11
        j2=j2+latlon11
        j4=j4+latlon11
     end do
     pencur=zero
     pen1=zero
     pen2=zero
     pen3=zero
     pencur_tl=zero
     pen1_tl=zero
     pen2_tl=zero
     pen3_tl=zero
     do nn=1,radptr%nchan
        ic=radptr%icx(nn)

!       contribution from bias corection
        tlap=radptr%pred2(nn)
        tlap2=tlap*tlap
        val2=-radptr%res(nn)+spred(ic,npred)*tlap+spred(ic,npred1)*tlap2
        val=           rpred(ic,npred)*tlap+rpred(ic,npred1)*tlap2
        val2_tl=-rad_inv_tl(nn)+spred_tl(ic,npred)*tlap+spred_tl(ic,npred1)*tlap2
        val_tl=         rpred_tl(ic,npred)*tlap+rpred_tl(ic,npred1)*tlap2

        do nx=1,npred-2
           val2=val2+spred(ic,nx)*radptr%pred1(nx)
           val =val +rpred(ic,nx)*radptr%pred1(nx)
           val2_tl=val2_tl+spred_tl(ic,nx)*radptr%pred1(nx)
           val_tl =val_tl +rpred_tl(ic,nx)*radptr%pred1(nx)
        end do

!       contribution from atmosphere
        do k=1,nsig3p3
           val2=val2+tdir(k)*radptr%dtb_dvar(k,nn)
           val =val +rdir(k)*radptr%dtb_dvar(k,nn)
           val2_tl=val2_tl+tdir_tl(k)*radptr%dtb_dvar(k,nn)
           val_tl =val_tl +rdir_tl(k)*radptr%dtb_dvar(k,nn)
        end do

!       multiply by variance
        rad1=val2+sges1*val
        rad2=val2+sges2*val
        rad3=val2+sges3*val
        rad1_tl=val2_tl+sges1_tl*val+sges1*val_tl
        rad2_tl=val2_tl+sges2_tl*val+sges2*val_tl
        rad3_tl=val2_tl+sges3_tl*val+sges3*val_tl
        
        halfvar_rad = -half*radptr%err2(nn)
        exp_arg  = halfvar_rad*val2*val2
        exp_arg1 = halfvar_rad*rad1*rad1
        exp_arg2 = halfvar_rad*rad2*rad2
        exp_arg3 = halfvar_rad*rad3*rad3
        exp_arg_tl  = -radptr%err2(nn)*val2*val2_tl
        exp_arg1_tl = -radptr%err2(nn)*rad1*rad1_tl
        exp_arg2_tl = -radptr%err2(nn)*rad2*rad2_tl
        exp_arg3_tl = -radptr%err2(nn)*rad3*rad3_tl
        
        if(pg_rad(ic) > tiny_r_kind .and. nlnqc_iter)then
           cg_rad=cg_term/b_rad(ic)
           wnotgross= one-pg_rad(ic)
           wgross = pg_rad(ic)*cg_rad
           temp    = wnotgross*exp(exp_arg)
           term_tl  = temp/(temp+wgross)*exp_arg_tl
           temp    = wnotgross*exp(exp_arg1)
           term1_tl = temp/(temp+wgross)*exp_arg1_tl
           temp    = wnotgross*exp(exp_arg2)
           term2_tl = temp/(temp+wgross)*exp_arg2_tl
           temp    = wnotgross*exp(exp_arg3)
           term3_tl = temp/(temp+wgross)*exp_arg3_tl
           term  = log(wnotgross*exp(exp_arg)  + wgross)
           term1 = log(wnotgross*exp(exp_arg1) + wgross)
           term2 = log(wnotgross*exp(exp_arg2) + wgross)
           term3 = log(wnotgross*exp(exp_arg3) + wgross)
        else
           term_tl  = exp_arg_tl
           term1_tl = exp_arg1_tl
           term2_tl = exp_arg2_tl
           term3_tl = exp_arg3_tl
           term  = exp_arg
           term1 = exp_arg1
           term2 = exp_arg2
           term3 = exp_arg3
        endif

        pencur_tl = pencur_tl + term_tl*radptr%raterr2(nn)
        pen1_tl   = pen1_tl   + term1_tl*radptr%raterr2(nn)
        pen2_tl   = pen2_tl   + term2_tl*radptr%raterr2(nn)
        pen3_tl   = pen3_tl   + term3_tl*radptr%raterr2(nn)
        pencur = pencur + term*radptr%raterr2(nn)
        pen1   = pen1   + term1*radptr%raterr2(nn)
        pen2   = pen2   + term2*radptr%raterr2(nn)
        pen3   = pen3   + term3*radptr%raterr2(nn)

     end do
     pen_tl = pen_tl -two*pencur_tl
     b1_tl  = b1_tl-two*(pen1_tl-pen2_tl)
     b3_tl  = b3_tl-two*(pen3_tl-pen2_tl)
     pen = pen -two*pencur
     b1  = b1-two*(pen1-pen2)
     b3  = b3-two*(pen3-pen2)

    end if

    radptr => radptr%llpoint
  end do
  return
end subroutine stprad_tl

end module stpradmod
