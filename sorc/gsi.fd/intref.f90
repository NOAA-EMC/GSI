module intrefmod

!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    intrefmod    module for intref and its tangent linear intref_tl
!
! abstract: module for intref and its tangent linear intref_tl
!
! program history log:
!   2005-05-13  Yanqiu zhu - wrap intref and its tangent linear intref_tl into one module
!   2005-11-16  Derber - remove interfaces
!

implicit none

PRIVATE
PUBLIC intref,intref_tl

contains

subroutine intref(rt,rq,rp,st,sq,sp)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    intref      apply nonlinqc obs operator refractivity
!   prgmmr: cucurull, l.     org: JCSDA/NCEP          date: 2004-04-29
!
! abstract: apply gps local refractivity operator and adjoint with
!           addition of nonlinear qc.
!
! program history log:
!   2004-04-29  cucurull- original code
!   2004-06-21  treadon - update documentation
!   2004-08-02  treadon - add only to module use, add intent in/out
!   2004-10-08  parrish - add nonlinear qc option
!   2004-11-19  cucurull- add increments for surface pressure and temperature at levels
!                          below observation. Install non-linear forward operator.
!   2005-01-26  cucurull- Implement local GPS RO operator
!   2005-03-01  parrish - nonlinear qc change as above; correct bug in zeroing of tl_AD
!   2005-03-23  cucurull- correct bounds for obs below the second level; place 
!                         bounds for k1 and k2
!   2005-04-11  treadon - merge intref and intref_qc into single routine
!   2005-08-02  derber  - modify for variational qc parameters for each ob
!   2005-09-28  derber  - consolidate location and weight arrays
!   2005-12-02  cucurull - fix bug for dimensions of sp and rp
!   2006-01-03  treadon - include r_kind type in w1,w2,...,w12 declaration
!   2006-07-28  derber  - modify to use new inner loop obs data structure
!                       - unify NL qc
!   2006-09-06  cucurull - generalize code to hybrid vertical coordinate and modify to use
!                          surface pressure
!   
!   input argument list:
!     st       - input temperature correction field
!     sq       - input q correction field
!     sp       - input p correction field
!
!   output argument list:
!     rt       - output vector after inclusion of gps local refractivity
!     rq       - output q vector after inclusion of gps local refractivity
!     rp       - output p vector after inclusion of gps local refractivity
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,i_kind
  use obsmod, only: gpshead,gpsptr
  use qcmod, only: nlnqc_iter
  use gridmod, only: latlon1n,latlon11,nsig
  use constants, only: zero,one,two,n_a,n_b,half,tiny_r_kind,cg_term
  implicit none

! Declare local parameters
  real(r_kind),parameter:: ten = 10.0_r_kind

! Declare passed variables
  real(r_kind),dimension(latlon1n),intent(in):: st,sq
  real(r_kind),dimension(latlon1n),intent(inout):: rt,rq
  real(r_kind),dimension(latlon11),intent(in):: sp
  real(r_kind),dimension(latlon11),intent(inout):: rp

! Declare local variables
  integer(i_kind) i,j,k,k1,k2
  integer(i_kind),dimension(nsig):: i1,i2,i3,i4
! real(r_kind) penalty
  real(r_kind) :: w1,w2,w3,w4,w5,w6,w7,w8,w9,w10,w11,w12
  real(r_kind) :: q_TL,p_TL,t_TL
  real(r_kind) :: q_AD,p_AD,t_AD
  real(r_kind),dimension(nsig) :: tl_TL,tl_AD
  real(r_kind) :: val
  real(r_kind) cg_gps,grad,p0,wnotgross,wgross



  gpsptr => gpshead
  do while (associated(gpsptr))

! Load location information into local variables
   do j=1,nsig
    i1(j)= gpsptr%ij(1,j)
    i2(j)= gpsptr%ij(2,j)
    i3(j)= gpsptr%ij(3,j)
    i4(j)= gpsptr%ij(4,j)
   enddo
   w1=gpsptr%wij(1)
   w2=gpsptr%wij(2)
   w3=gpsptr%wij(3)
   w4=gpsptr%wij(4)
   w5=gpsptr%wij(5)
   w6=gpsptr%wij(6)
   w7=gpsptr%wij(7)
   w8=gpsptr%wij(8)
   w9=gpsptr%wij(9)
   w10=gpsptr%wij(10)
   w11=gpsptr%wij(11)
   w12=gpsptr%wij(12)

! Set indices of model levels below (k1) and above (k2) observation.
   k=gpsptr%gpsloc
   k1=min(max(1,k),nsig)
   k2=max(1,min(k+1,nsig))

! increments

  q_TL=w1*sq(i1(k1))+w2*sq(i2(k1))+w3*sq(i3(k1))&
      +w4*sq(i4(k1))+w5*sq(i1(k2))+w6*sq(i2(k2))&
      +w7*sq(i3(k2))+w8*sq(i4(k2))

  t_TL=w1*st(i1(k1))+w2*st(i2(k1))+w3*st(i3(k1))&
      +w4*st(i4(k1))+w5*st(i1(k2))+w6*st(i2(k2))&
      +w7*st(i3(k2))+w8*st(i4(k2))

  p_TL=w9*sp(i1(1))+w10*sp(i2(1))+w11*sp(i3(1))+w12*sp(i4(1))

  do j=1,nsig
   tl_TL(j)=w9*st(i1(j))+w10*st(i2(j))+w11*st(i3(j))+w12*st(i4(j))
  enddo

! local refractivity (linear operator)

  val=gpsptr%termqin*q_TL+gpsptr%termpin*p_TL+ &
       gpsptr%termtin*t_TL
  if(k1==1) then
    val=val+two*gpsptr%termtkin*tl_TL(k1)
  else
    val=val+gpsptr%termtkin*tl_TL(k1)+gpsptr%termtkin*tl_TL(k1-1)
  endif
  if(k1.ge.2) then
  do j=2,k1
  val=val+gpsptr%termtlin(j)*tl_TL(j-1)
  enddo
  endif

  val=val-gpsptr%res

! needed for gradient of nonlinear qc operator
  if (nlnqc_iter .and. gpsptr%pg > tiny_r_kind .and.  &
                       gpsptr%b  > tiny_r_kind) then
     cg_gps=cg_term/gpsptr%b
     wnotgross= one-gpsptr%pg
     wgross = gpsptr%pg*cg_gps/wnotgross
     p0   = wgross/(wgross+exp(-half*gpsptr%err2*val**2))
     val = val*(one-p0)
  endif

  grad     = val*gpsptr%raterr2*gpsptr%err2


! adjoint 

   q_AD=zero;p_AD=zero;t_AD=zero;tl_AD=zero
   q_AD     =q_AD+grad*gpsptr%termqin
   p_AD     =p_AD+grad*gpsptr%termpin
   t_AD     =t_AD+grad*gpsptr%termtin
   if(k1 == 1) then
    tl_AD(k1)=tl_AD(k1)+two*grad*gpsptr%termtkin
   else
    tl_AD(k1)=tl_AD(k1)+grad*gpsptr%termtkin
    tl_AD(k1-1)=tl_AD(k1-1)+grad*gpsptr%termtkin
   endif
   if(k1.ge.2) then
   do j=2,k1
   tl_AD(j-1) =tl_AD(j-1)+grad*gpsptr%termtlin(j)
   enddo
   endif

! Interpolation

  do j=1,nsig
  rt(i1(j))=rt(i1(j))+w9 *tl_AD(j)
  rt(i2(j))=rt(i2(j))+w10*tl_AD(j)
  rt(i3(j))=rt(i3(j))+w11*tl_AD(j)
  rt(i4(j))=rt(i4(j))+w12*tl_AD(j)
  enddo

  rt(i1(k1))=rt(i1(k1))+w1*t_AD
  rt(i2(k1))=rt(i2(k1))+w2*t_AD
  rt(i3(k1))=rt(i3(k1))+w3*t_AD
  rt(i4(k1))=rt(i4(k1))+w4*t_AD
  rt(i1(k2))=rt(i1(k2))+w5*t_AD
  rt(i2(k2))=rt(i2(k2))+w6*t_AD
  rt(i3(k2))=rt(i3(k2))+w7*t_AD
  rt(i4(k2))=rt(i4(k2))+w8*t_AD

  rq(i1(k1))=rq(i1(k1))+w1*q_AD
  rq(i2(k1))=rq(i2(k1))+w2*q_AD
  rq(i3(k1))=rq(i3(k1))+w3*q_AD
  rq(i4(k1))=rq(i4(k1))+w4*q_AD
  rq(i1(k2))=rq(i1(k2))+w5*q_AD
  rq(i2(k2))=rq(i2(k2))+w6*q_AD
  rq(i3(k2))=rq(i3(k2))+w7*q_AD
  rq(i4(k2))=rq(i4(k2))+w8*q_AD

  rp(i1(1))=rp(i1(1))+w9*p_AD
  rp(i2(1))=rp(i2(1))+w10*p_AD
  rp(i3(1))=rp(i3(1))+w11*p_AD
  rp(i4(1))=rp(i4(1))+w12*p_AD

  gpsptr => gpsptr%llpoint

  end do

  return
end subroutine intref


subroutine intref_tl(rt,rq,rp,st,sq,sp,rt_tl,rq_tl,rp_tl,st_tl,sq_tl,sp_tl)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    intref_tl      the tangent linear of the operator that applies 
!                              nonlinqc obs operator refractivity
!   prgmmr: yanqiu zhu     org: GMAO          date: 2005-05-13
!
! abstract: the tangent linear of the operator that applies gps local 
!           refractivity operator and adjoint with addition of nonlinear qc.
!
! program history log:
!   2005-05-13  yanqiu zhu - tangent linear of intref
!   2005-08-02  derber  - modify for variational qc parameters for each ob
!   2005-12-02  cucurull - fix bug for dimensions of sp and rp
!   2006-01-03  treadon - include r_kind type in w1,w2,...,w12 declaration
!   
!   input argument list:
!     st       - input temperature correction field
!     sq       - input q correction field
!     sp       - input p correction field
!     st_tl     - input tangent linear temperature correction field
!     sq_tl     - input tangent linear q correction field
!     sp_tl     - input tangent linear p correction field
!
!   output argument list:
!     rt       - output vector after inclusion of gps local refractivity
!     rq       - output q vector after inclusion of gps local refractivity
!     rp       - output p vector after inclusion of gps local refractivity
!     rt_tl     - output tangent linear vector after inclusion of gps local refractivity
!     rq_tl     - output tangent linear q vector after inclusion of gps local refractivity
!     rp_tl     - output tangent linear p vector after inclusion of gps local refractivity
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,i_kind
  use obsmod, only: gpshead,gpsptr
  use obsmod_tl, only: gpsdataerr_tl
  use qcmod, only: nlnqc_iter
  use gridmod, only: latlon1n,latlon11,nsig
  use constants, only: zero,one,two,n_a,n_b,half,tiny_r_kind,cg_term
  implicit none

! Declare local parameters
  real(r_kind),parameter:: ten = 10.0_r_kind

! Declare passed variables
  real(r_kind),dimension(latlon1n),intent(in):: st,sq
  real(r_kind),dimension(latlon1n),intent(in):: st_tl,sq_tl
  real(r_kind),dimension(latlon1n),intent(inout):: rt,rq
  real(r_kind),dimension(latlon1n),intent(inout):: rt_tl,rq_tl
  real(r_kind),dimension(latlon11),intent(in):: sp,sp_tl
  real(r_kind),dimension(latlon11),intent(inout) :: rp,rp_tl

! Declare local variables
  integer(i_kind) i,j,k,k1,k2
  integer(i_kind),dimension(nsig):: i1,i2,i3,i4
! real(r_kind) penalty
  real(r_kind) :: w1,w2,w3,w4,w5,w6,w7,w8,w9,w10,w11,w12
  real(r_kind) :: q_TL,p_TL,t_TL
  real(r_kind) :: q_TL_tl,p_TL_tl,t_TL_tl
  real(r_kind) :: q_AD,p_AD,t_AD
  real(r_kind) :: q_AD_tl,p_AD_tl,t_AD_tl
  real(r_kind),dimension(nsig) :: tl_TL,tl_AD
  real(r_kind),dimension(nsig) :: tl_TL_tl,tl_AD_tl
  real(r_kind) :: val
  real(r_kind) :: val_tl
  real(r_kind) cg_gps,grad,p0,wnotgross,wgross,term
  real(r_kind) grad_tl,p0_tl,term_tl

! Loop over observations
  gpsptr => gpshead
  i=0
  do while (associated(gpsptr))

   i=i+1

! Load location information into local variables
   do j=1,nsig
    i1(j)= gpsptr%ij(1,j)
    i2(j)= gpsptr%ij(2,j)
    i3(j)= gpsptr%ij(3,j)
    i4(j)= gpsptr%ij(4,j)
   enddo
   w1=gpsptr%wij(1)
   w2=gpsptr%wij(2)
   w3=gpsptr%wij(3)
   w4=gpsptr%wij(4)
   w5=gpsptr%wij(5)
   w6=gpsptr%wij(6)
   w7=gpsptr%wij(7)
   w8=gpsptr%wij(8)
   w9=gpsptr%wij(9)
   w10=gpsptr%wij(10)
   w11=gpsptr%wij(11)
   w12=gpsptr%wij(12)

! Set indices of model levels below (k1) and above (k2) observation.
   k=gpsptr%gpsloc
   k1=min(max(1,k),nsig)
   k2=max(1,min(k+1,nsig))

! increments

  q_TL=w1*sq(i1(k1))+w2*sq(i2(k1))+w3*sq(i3(k1))&
      +w4*sq(i4(k1))+w5*sq(i1(k2))+w6*sq(i2(k2))&
      +w7*sq(i3(k2))+w8*sq(i4(k2))

  t_TL=w1*st(i1(k1))+w2*st(i2(k1))+w3*st(i3(k1))&
      +w4*st(i4(k1))+w5*st(i1(k2))+w6*st(i2(k2))&
      +w7*st(i3(k2))+w8*st(i4(k2))

  p_TL=w9*sp(i1(1))+w10*sp(i2(1))+w11*sp(i3(1))+w12*sp(i4(1))

  q_TL_tl=w1*sq_tl(i1(k1))+w2*sq_tl(i2(k1))+w3*sq_tl(i3(k1))&
        +w4*sq_tl(i4(k1))+w5*sq_tl(i1(k2))+w6*sq_tl(i2(k2))&
        +w7*sq_tl(i3(k2))+w8*sq_tl(i4(k2))

  t_TL_tl=w1*st_tl(i1(k1))+w2*st_tl(i2(k1))+w3*st_tl(i3(k1))&
        +w4*st_tl(i4(k1))+w5*st_tl(i1(k2))+w6*st_tl(i2(k2))&
        +w7*st_tl(i3(k2))+w8*st_tl(i4(k2))

  p_TL_tl=w9*sp_tl(i1(1))+w10*sp_tl(i2(1))+w11*sp_tl(i3(1))+w12*sp_tl(i4(1))

  do j=1,nsig
   tl_TL(j)=w9*st(i1(j))+w10*st(i2(j))+w11*st(i3(j))+w12*st(i4(j))
   tl_TL_tl(j)=w9*st_tl(i1(j))+w10*st_tl(i2(j))+w11*st_tl(i3(j))+w12*st_tl(i4(j))
  enddo

! local refractivity (linear operator)

  val=gpsptr%termqin*q_TL+gpsptr%termpin*p_TL+ &
       gpsptr%termtin*t_TL
  val_tl=gpsptr%termqin*q_TL_tl+gpsptr%termpin*p_TL_tl+ &
       gpsptr%termtin*t_TL_tl
  if(k1==1) then
    val=val+two*gpsptr%termtkin*tl_TL(k1)
    val_tl=val_tl+two*gpsptr%termtkin*tl_TL_tl(k1)
  else
    val=val+gpsptr%termtkin*tl_TL(k1)+gpsptr%termtkin*tl_TL(k1-1)
    val_tl=val_tl+gpsptr%termtkin*tl_TL_tl(k1)+gpsptr%termtkin*tl_TL_tl(k1-1)
  endif
  if(k1.ge.2) then
  do j=2,k1
  val=val+gpsptr%termtlin(j)*tl_TL(j-1)
  val_tl=val_tl+gpsptr%termtlin(j)*tl_TL_tl(j-1)
  enddo
  endif

  val=val-gpsptr%res
  val_tl=val_tl-gpsdataerr_tl(i)

! needed for gradient of nonlinear qc operator
  if (nlnqc_iter .and. gpsptr%pg > tiny_r_kind) then
     cg_gps=cg_term/gpsptr%b
     wnotgross= one-gpsptr%pg
     wgross = gpsptr%pg*cg_gps
     p0   = wnotgross*exp(-half*gpsptr%err2*val**2)+wgross
     term = (p0-wgross)/p0
     p0_tl = -val*(p0-wgross)*val_tl*gpsptr%err2
     term_tl = wgross/(p0*p0)*p0_tl
  else
     term = one
     term_tl = zero
  endif
  grad     = val * term
  grad_tl   = val_tl * term + val * term_tl
  grad     = grad*gpsptr%raterr2*gpsptr%err2
  grad_tl   = grad_tl*gpsptr%raterr2*gpsptr%err2


! adjoint 

   q_AD=zero;p_AD=zero;t_AD=zero;tl_AD=zero
   q_AD     =q_AD+grad*gpsptr%termqin
   p_AD     =p_AD+grad*gpsptr%termpin
   t_AD     =t_AD+grad*gpsptr%termtin
   if(k1 == 1) then
    tl_AD(k1)=tl_AD(k1)+two*grad*gpsptr%termtkin
   else
    tl_AD(k1)=tl_AD(k1)+grad*gpsptr%termtkin
    tl_AD(k1-1)=tl_AD(k1-1)+grad*gpsptr%termtkin
   endif
   if(k1.ge.2) then
   do j=2,k1
   tl_AD(j-1) =tl_AD(j-1)+grad*gpsptr%termtlin(j)
   enddo
   endif

   q_AD_tl=zero;p_AD_tl=zero;t_AD_tl=zero;tl_AD_tl=zero
   q_AD_tl     =q_AD_tl+grad_tl*gpsptr%termqin
   p_AD_tl     =p_AD_tl+grad_tl*gpsptr%termpin
   t_AD_tl     =t_AD_tl+grad_tl*gpsptr%termtin
   if(k1 == 1) then
    tl_AD_tl(k1)=tl_AD_tl(k1)+two*grad_tl*gpsptr%termtkin
   else
    tl_AD_tl(k1)=tl_AD_tl(k1)+grad_tl*gpsptr%termtkin
    tl_AD_tl(k1-1)=tl_AD_tl(k1-1)+grad_tl*gpsptr%termtkin
   endif
   if(k1.gt.2) then
   do j=2,k1
   tl_AD_tl(j-1) =tl_AD_tl(j-1)+grad_tl*gpsptr%termtlin(j)
   enddo
   endif

! Interpolation

  do j=1,nsig
  rt(i1(j))=rt(i1(j))+w9 *tl_AD(j)
  rt(i2(j))=rt(i2(j))+w10*tl_AD(j)
  rt(i3(j))=rt(i3(j))+w11*tl_AD(j)
  rt(i4(j))=rt(i4(j))+w12*tl_AD(j)
  rt_tl(i1(j))=rt_tl(i1(j))+w9 *tl_AD_tl(j)
  rt_tl(i2(j))=rt_tl(i2(j))+w10*tl_AD_tl(j)
  rt_tl(i3(j))=rt_tl(i3(j))+w11*tl_AD_tl(j)
  rt_tl(i4(j))=rt_tl(i4(j))+w12*tl_AD_tl(j)
  enddo

  rt(i1(k1))=rt(i1(k1))+w1*t_AD
  rt(i2(k1))=rt(i2(k1))+w2*t_AD
  rt(i3(k1))=rt(i3(k1))+w3*t_AD
  rt(i4(k1))=rt(i4(k1))+w4*t_AD
  rt(i1(k2))=rt(i1(k2))+w5*t_AD
  rt(i2(k2))=rt(i2(k2))+w6*t_AD
  rt(i3(k2))=rt(i3(k2))+w7*t_AD
  rt(i4(k2))=rt(i4(k2))+w8*t_AD

  rq(i1(k1))=rq(i1(k1))+w1*q_AD
  rq(i2(k1))=rq(i2(k1))+w2*q_AD
  rq(i3(k1))=rq(i3(k1))+w3*q_AD
  rq(i4(k1))=rq(i4(k1))+w4*q_AD
  rq(i1(k2))=rq(i1(k2))+w5*q_AD
  rq(i2(k2))=rq(i2(k2))+w6*q_AD
  rq(i3(k2))=rq(i3(k2))+w7*q_AD
  rq(i4(k2))=rq(i4(k2))+w8*q_AD

  rp(i1(1))=rp(i1(1))+w9*p_AD
  rp(i2(1))=rp(i2(1))+w10*p_AD
  rp(i3(1))=rp(i3(1))+w11*p_AD
  rp(i4(1))=rp(i4(1))+w12*p_AD

  rt_tl(i1(k1))=rt_tl(i1(k1))+w1*t_AD_tl
  rt_tl(i2(k1))=rt_tl(i2(k1))+w2*t_AD_tl
  rt_tl(i3(k1))=rt_tl(i3(k1))+w3*t_AD_tl
  rt_tl(i4(k1))=rt_tl(i4(k1))+w4*t_AD_tl
  rt_tl(i1(k2))=rt_tl(i1(k2))+w5*t_AD_tl
  rt_tl(i2(k2))=rt_tl(i2(k2))+w6*t_AD_tl
  rt_tl(i3(k2))=rt_tl(i3(k2))+w7*t_AD_tl
  rt_tl(i4(k2))=rt_tl(i4(k2))+w8*t_AD_tl

  rq_tl(i1(k1))=rq_tl(i1(k1))+w1*q_AD_tl
  rq_tl(i2(k1))=rq_tl(i2(k1))+w2*q_AD_tl
  rq_tl(i3(k1))=rq_tl(i3(k1))+w3*q_AD_tl
  rq_tl(i4(k1))=rq_tl(i4(k1))+w4*q_AD_tl
  rq_tl(i1(k2))=rq_tl(i1(k2))+w5*q_AD_tl
  rq_tl(i2(k2))=rq_tl(i2(k2))+w6*q_AD_tl
  rq_tl(i3(k2))=rq_tl(i3(k2))+w7*q_AD_tl
  rq_tl(i4(k2))=rq_tl(i4(k2))+w8*q_AD_tl

  rp_tl(i1(1))=rp_tl(i1(1))+w9*p_AD_tl
  rp_tl(i2(1))=rp_tl(i2(1))+w10*p_AD_tl
  rp_tl(i3(1))=rp_tl(i3(1))+w11*p_AD_tl
  rp_tl(i4(1))=rp_tl(i4(1))+w12*p_AD_tl


  gpsptr => gpsptr%llpoint

  end do

  return
end subroutine intref_tl

end module intrefmod
