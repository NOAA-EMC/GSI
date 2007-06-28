module stprefmod

!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    stprefmod    module for stpref and its tangent linear stpref_tl
!
! abstract: module for stpref and its tangent linear stpref_tl
!
! program history log:
!   2005-05-19  Yanqiu zhu - wrap stpref and its tangent linear stpref_tl into one module
!   2005-11-16  Derber - remove interfaces
!

implicit none

PRIVATE
PUBLIC stpref,stpref_tl

contains

subroutine stpref(rt,rq,rp,st,sq,sp,pen,b1,b3,sges1,sges2,sges3)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram: stpref    compute contribution to penalty and stepsize
!                       from ref, using nonlinear qc    
!   prgmmr: cucurull,l.     org: JCSDA/NCEP           date: 2004-05-06
!
! abstract:  This routine applies the (linear) operator for local 
!            refractivity and linear linear estimate for step size. 
!            This version includes nonlinear qc.
!
! program history log:
!   2004-05-06  cucurull 
!   2004-06-21  treadon - update documentation
!   2004-08-02  treadon - add only to module use, add intent in/out
!   2004=10-08  parrish - add nonlinear qc option
!   2004-11-30  cucurull- add increments for surface pressure and temperature at levels
!                         below observation. Install non-linear forward operator.
!   2005-01-26  cucurull- Implement local GPS RO operator
!   2005-03-23  cucurull- correct bouds for obs below the second level
!   2005-04-11  treadon - merge stpref and stpref_qc into single routine
!   2005-08-02  derber  - modify for variational qc parameters for each ob
!   2005-09-28  derber  - consolidate location and weight arrays
!   2005-12-02  cucurull - fix bug for dimensions of sp and rp
!   2007-07-28  derber  - modify to use new inner loop obs data structure
!                       - unify NL qc
!   2006-09-06  cucurull - generalize code to hybrid vertical coordinate and modify to use 
!                          surface pressure
!
!   input argument list:
!     rt    - search direction (gradxJ) for virtual temperature
!     rq    - search direction (gradxJ) for specific humidity
!     rp    - search direction (gradxJ) for pressure
!     st    - analysis increment (correction) for virtual temperature
!     sq    - analysis increment (correction) for specific humidity
!     sp    - analysis increment (correction) for pressure
!                                         
!   output argument list:
!     pen   - contribution to penalty from local gps refractivity
!     b1       - pen(sges1)-pen(sges2)
!     b3       - pen(sges3)-pen(sges2)
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,i_kind
  use obsmod, only: gpshead,gpsptr
  use qcmod, only: nlnqc_iter
  use constants, only: zero,one,two,n_a,n_b,half,tiny_r_kind,cg_term
  use gridmod, only: latlon1n,latlon11,nsig
  implicit none

! Declare local parameters
  real(r_kind),parameter:: ten = 10.0_r_kind

! Declare passed variables
  real(r_kind),intent(out):: pen,b1,b3
  real(r_kind),dimension(latlon1n),intent(in):: rt,rq,st,sq
  real(r_kind),dimension(latlon11),intent(in):: rp,sp
  real(r_kind),intent(in):: sges1,sges2,sges3

! Declare local variables
  integer(i_kind) i,j,k,k1,k2
  integer(i_kind),dimension(nsig):: i1,i2,i3,i4
  real(r_kind) val,val2
  real(r_kind) w1,w2,w3,w4,w5,w6,w7,w8,w9,w10,w11,w12
  real(r_kind) :: q_TL,p_TL,t_TL
  real(r_kind) :: rq_TL,rp_TL,rt_TL
  real(r_kind),dimension(nsig) :: tl_TL,rtl_TL

  real(r_kind) cg_gps,pen1,pen2,pen3,pencur,nref1,nref2,nref3,wgross,wnotgross
  real(r_kind) alpha,ccoef,bcoef1,bcoef2,cc

! Initialize penalty, b1, and b3 to zero
  pen=zero
  b1=zero; b3=zero
  alpha=one/(sges2-sges1)
  ccoef=half*alpha*alpha
  bcoef1=half*half*alpha
  bcoef2=sges2*ccoef

! Initialize local variables/arrays
  q_TL= zero
  t_TL= zero
  p_TL= zero
  rq_TL=zero
  rt_TL=zero
  rp_TL=zero
  tl_TL= zero
  rtl_TL=zero

! Loop over observations
  gpsptr => gpshead
  do while (associated(gpsptr))

    if(gpsptr%luse)then

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

! get weights

  rq_TL=w1*rq(i1(k1))+w2*rq(i2(k1))+w3*rq(i3(k1))&
       +w4*rq(i4(k1))+w5*rq(i1(k2))+w6*rq(i2(k2))&
       +w7*rq(i3(k2))+w8*rq(i4(k2))

  rt_TL=w1*rt(i1(k1))+w2*rt(i2(k1))+w3*rt(i3(k1))&
       +w4*rt(i4(k1))+w5*rt(i1(k2))+w6*rt(i2(k2))&
       +w7*rt(i3(k2))+w8*rt(i4(k2))

  rp_TL=w9*rp(i1(1))+w10*rp(i2(1))+w11*rp(i3(1))+w12*rp(i4(1))

  q_TL=w1*sq(i1(k1))+w2*sq(i2(k1))+w3*sq(i3(k1))&
      +w4*sq(i4(k1))+w5*sq(i1(k2))+w6*sq(i2(k2))&
      +w7*sq(i3(k2))+w8*sq(i4(k2))

  t_TL=w1*st(i1(k1))+w2*st(i2(k1))+w3*st(i3(k1))&
      +w4*st(i4(k1))+w5*st(i1(k2))+w6*st(i2(k2))&
      +w7*st(i3(k2))+w8*st(i4(k2))

  p_TL=w9*sp(i1(1))+w10*sp(i2(1))+w11*sp(i3(1))+w12*sp(i4(1))

 do j=1,nsig
   tl_TL(j)=w9*st(i1(j))+w10*st(i2(j))+w11*st(i3(j))+w12*st(i4(j))
   rtl_TL(j)=w9*rt(i1(j))+w10*rt(i2(j))+w11*rt(i3(j))+w12*rt(i4(j))
  enddo


! local refractivity (linear operator)

! penalty

  val2=gpsptr%termqin*q_TL+gpsptr%termpin*p_TL+ &
       gpsptr%termtin*t_TL
  if(k==1) then
    val2=val2+two*gpsptr%termtkin*tl_TL(k1)
  else
    val2=val2+gpsptr%termtkin*tl_TL(k1)+gpsptr%termtkin*tl_TL(k1-1)
  endif
  if(k1.ge.2) then
  do j=2,k1
  val2=val2+gpsptr%termtlin(j)*tl_TL(j-1)
  enddo
  endif
  val2=val2-gpsptr%res

! gradient
  val=gpsptr%termqin*rq_TL+gpsptr%termpin*rp_TL+ &
      gpsptr%termtin*rt_TL
  if(k==1) then
    val=val+two*gpsptr%termtkin*rtl_TL(k1)
  else
    val=val+gpsptr%termtkin*rtl_TL(k1)+gpsptr%termtkin*rtl_TL(k1-1)
  endif
  if(k1.ge.2) then
  do j=2,k1
  val=val+gpsptr%termtlin(j)*rtl_TL(j-1)
  enddo
  endif

  nref1=val2+sges1*val
  nref2=val2+sges2*val
  nref3=val2+sges3*val

  pencur = val2*val2*gpsptr%err2
  pen1   = nref1*nref1*gpsptr%err2
  pen2   = nref2*nref2*gpsptr%err2
  pen3   = nref3*nref3*gpsptr%err2

!  Modify penalty term if nonlinear QC
  if (nlnqc_iter .and. gpsptr%pg > tiny_r_kind .and. gpsptr%b > tiny_r_kind) then
     cg_gps=cg_term/gpsptr%b
     wnotgross= one-gpsptr%pg
     wgross = gpsptr%pg*cg_gps/wnotgross
     pencur = -two*log((exp(-half*pencur) + wgross)/(one+wgross))
     pen1   = -two*log((exp(-half*pen1  ) + wgross)/(one+wgross))
     pen2   = -two*log((exp(-half*pen2  ) + wgross)/(one+wgross))
     pen3   = -two*log((exp(-half*pen3  ) + wgross)/(one+wgross))
  endif
  
! Cost function, b1, and b3
  pen = pen+pencur*gpsptr%raterr2
  cc  = (pen1+pen3-two*pen2)*gpsptr%raterr2
  b1  = b1+(pen1-pen3)*gpsptr%raterr2*bcoef1+cc*bcoef2
  b3  = b3+cc*ccoef

  endif
  
  gpsptr => gpsptr%llpoint

  end do

 return
end subroutine stpref


subroutine stpref_tl(rt,rq,rp,st,sq,sp,pen,b1,b3,sges1,sges2,sges3, &
                    rt_tl,rq_tl,rp_tl,st_tl,sq_tl,sp_tl,pen_tl,b1_tl,b3_tl,sges1_tl,sges2_tl,sges3_tl)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram: stpref_tl   the tangent linear of the operator that computes contribution to 
!                        penalty and stepsize from ref, using nonlinear qc    
!   prgmmr: yanqiu zhu    org: GMAO           date: 2005-05-19
!
! abstract:  This routine is the tangent linear of the operator that applies the (linear) 
!            operator for local refractivity and linear linear estimate for step size. 
!            This version includes nonlinear qc.
!
! program history log:
!   2005-05-19  yanqiu zhu - tangent linear of stpref
!   2005-08-02  derber  - modify for variational qc parameters for each ob
!   2005-12-02  cucurull - fix bug for dimensions of sp and rp
!
!   input argument list:
!     rt    - search direction (gradxJ) for virtual temperature
!     rq    - search direction (gradxJ) for specific humidity
!     rp    - search direction (gradxJ) for pressure
!     st    - analysis increment (correction) for virtual temperature
!     sq    - analysis increment (correction) for specific humidity
!     sp    - analysis increment (correction) for pressure
!     rt_tl    - tangent linear search direction (gradxJ) for virtual temperature
!     rq_tl    - tangent linear search direction (gradxJ) for specific humidity
!     rp_tl    - tangent linear search direction (gradxJ) for pressure
!     st_tl    - tangent linear analysis increment (correction) for virtual temperature
!     sq_tl    - tangent linear analysis increment (correction) for specific humidity
!     sp_tl    - tangent linear analysis increment (correction) for pressure
!                                         
!   output argument list:
!     pen   - contribution to penalty from local gps refractivity
!     b1       - pen(sges1)-pen(sges2)
!     b3       - pen(sges3)-pen(sges2)
!     pen_tl   - tangent linear of the contribution to penalty from local gps refractivity
!     b1_tl       - pen_tl(sges1)-pen_tl(sges2)
!     b3_tl       - pen_tl(sges3)-pen_tl(sges2)
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
  use constants, only: zero,one,two,n_a,n_b,half,tiny_r_kind,cg_term
  use gridmod, only: latlon1n,latlon11,nsig
  implicit none

! Declare local parameters
  real(r_kind),parameter:: ten = 10.0_r_kind

! Declare passed variables
  real(r_kind),intent(out):: pen,b1,b3
  real(r_kind),intent(out):: pen_tl,b1_tl,b3_tl
  real(r_kind),dimension(latlon1n),intent(in):: rt,rq,st,sq
  real(r_kind),dimension(latlon1n),intent(in):: rt_tl,rq_tl,st_tl,sq_tl
  real(r_kind),dimension(latlon11),intent(in):: rp,sp,rp_tl,sp_tl
  real(r_kind),intent(in):: sges1,sges2,sges3
  real(r_kind),intent(in):: sges1_tl,sges2_tl,sges3_tl

! Declare local variables
  integer(i_kind) i,j,k,k1,k2
  integer(i_kind),dimension(nsig):: i1,i2,i3,i4
  real(r_kind) val,val2
  real(r_kind) w1,w2,w3,w4,w5,w6,w7,w8,w9,w10,w11,w12
  real(r_kind) val_tl,val2_tl
  real(r_kind) :: q_TL,p_TL,t_TL
  real(r_kind) :: q_TL_tl,p_TL_tl,t_TL_tl
  real(r_kind) :: rq_TLM,rp_TLM,rt_TLM
  real(r_kind) :: rq_TLM_tl,rp_TLM_tl,rt_TLM_tl
  real(r_kind),dimension(nsig) :: tl_TL,rtl_TL
  real(r_kind),dimension(nsig) :: tl_TL_tl,rtl_TL_tl

  real(r_kind) cg_gps,pen1,pen2,pen3,pencur,nref1,nref2,nref3,wgross,wnotgross
  real(r_kind) pen1_tl,pen2_tl,pen3_tl,pencur_tl,nref1_tl,nref2_tl,nref3_tl
  real(r_kind) term,term1,term2,term3
  real(r_kind) term_tl,term1_tl,term2_tl,term3_tl
  real(r_kind) exp_arg,exp_arg1,exp_arg2,exp_arg3
  real(r_kind) exp_arg_tl,exp_arg1_tl,exp_arg2_tl,exp_arg3_tl
  real(r_kind) temp

! Initialize penalty, b1, and b3 to zero
  pen=zero
  b1=zero; b3=zero
  pen_tl=zero
  b1_tl=zero; b3_tl=zero

! Initialize local variables/arrays
  q_TL= zero
  t_TL= zero
  p_TL= zero
  rq_TLM=zero
  rt_TLM=zero
  rp_TLM=zero
  tl_TL= zero
  rtl_TL=zero
  q_TL_tl= zero
  t_TL_tl= zero
  p_TL_tl= zero
  rq_TLM_tl=zero
  rt_TLM_tl=zero
  rp_TLM_tl=zero
  tl_TL_tl= zero
  rtl_TL_tl=zero

! Loop over observations
  gpsptr => gpshead
  i=0
  do while (associated(gpsptr))
    i=i+1
    if(gpsptr%luse)then

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

! get weights

  rq_TLM=w1*rq(i1(k1))+w2*rq(i2(k1))+w3*rq(i3(k1))&
       +w4*rq(i4(k1))+w5*rq(i1(k2))+w6*rq(i2(k2))&
       +w7*rq(i3(k2))+w8*rq(i4(k2))

  rt_TLM=w1*rt(i1(k1))+w2*rt(i2(k1))+w3*rt(i3(k1))&
       +w4*rt(i4(k1))+w5*rt(i1(k2))+w6*rt(i2(k2))&
       +w7*rt(i3(k2))+w8*rt(i4(k2))

  rp_TLM=w9*rp(i1(1))+w10*rp(i2(1))+w11*rp(i3(1))+w12*rp(i4(1))

  q_TL=w1*sq(i1(k1))+w2*sq(i2(k1))+w3*sq(i3(k1))&
      +w4*sq(i4(k1))+w5*sq(i1(k2))+w6*sq(i2(k2))&
      +w7*sq(i3(k2))+w8*sq(i4(k2))

  t_TL=w1*st(i1(k1))+w2*st(i2(k1))+w3*st(i3(k1))&
      +w4*st(i4(k1))+w5*st(i1(k2))+w6*st(i2(k2))&
      +w7*st(i3(k2))+w8*st(i4(k2))

  p_TL=w9*sp(i1(1))+w10*sp(i2(1))+w11*sp(i3(1))+w11*sp(i4(1))

 do j=1,nsig
   tl_TL(j)=w9*st(i1(j))+w10*st(i2(j))+w11*st(i3(j))+w12*st(i4(j))
   rtl_TL(j)=w9*rt(i1(j))+w10*rt(i2(j))+w11*rt(i3(j))+w12*rt(i4(j))
  enddo

  rq_TLM_tl=w1*rq_tl(i1(k1))+w2*rq_tl(i2(k1))+w3*rq_tl(i3(k1))&
         +w4*rq_tl(i4(k1))+w5*rq_tl(i1(k2))+w6*rq_tl(i2(k2))&
         +w7*rq_tl(i3(k2))+w8*rq_tl(i4(k2))

  rt_TLM_tl=w1*rt_tl(i1(k1))+w2*rt_tl(i2(k1))+w3*rt_tl(i3(k1))&
         +w4*rt_tl(i4(k1))+w5*rt_tl(i1(k2))+w6*rt_tl(i2(k2))&
         +w7*rt_tl(i3(k2))+w8*rt_tl(i4(k2))

  rp_TLM_tl=w9*rp_tl(i1(1))+w10*rp_tl(i2(1))+w11*rp_tl(i3(1))&
         +w12*rp_tl(i4(1))

  q_TL_tl=w1*sq_tl(i1(k1))+w2*sq_tl(i2(k1))+w3*sq_tl(i3(k1))&
        +w4*sq_tl(i4(k1))+w5*sq_tl(i1(k2))+w6*sq_tl(i2(k2))&
        +w7*sq_tl(i3(k2))+w8*sq_tl(i4(k2))

  t_TL_tl=w1*st_tl(i1(k1))+w2*st_tl(i2(k1))+w3*st_tl(i3(k1))&
        +w4*st_tl(i4(k1))+w5*st_tl(i1(k2))+w6*st_tl(i2(k2))&
        +w7*st_tl(i3(k2))+w8*st_tl(i4(k2))

  p_TL_tl=w9*sp_tl(i1(1))+w10*sp_tl(i2(1))+w11*sp_tl(i3(1))+w12*sp_tl(i4(1))

 do j=1,nsig
   tl_TL_tl(j)=w9*st_tl(i1(j))+w10*st_tl(i2(j))+w11*st_tl(i3(j))+w12*st_tl(i4(j))
   rtl_TL_tl(j)=w9*rt_tl(i1(j))+w10*rt_tl(i2(j))+w11*rt_tl(i3(j))+w12*rt_tl(i4(j))
  enddo

! local refractivity (linear operator)

! penalty

  val2=gpsptr%termqin*q_TL+gpsptr%termpin*p_TL+ &
       gpsptr%termtin*t_TL
  if(k==1) then
    val2=val2+two*gpsptr%termtkin*tl_TL(k1)
  else
    val2=val2+gpsptr%termtkin*tl_TL(k1)+gpsptr%termtkin*tl_TL(k1-1)
  endif

  val2_tl=gpsptr%termqin*q_TL_tl+gpsptr%termpin*p_TL_tl+ &
          gpsptr%termtin*t_TL_tl
 if(k==1) then
    val2_tl=val2_tl+two*gpsptr%termtkin*tl_TL_tl(k1)
  else
    val2_tl=val2_tl+gpsptr%termtkin*tl_TL_tl(k1)+gpsptr%termtkin*tl_TL_tl(k1-1)
  endif

  if(k1.ge.2) then
  do j=2,k1
  val2=val2+gpsptr%termtlin(j)*tl_TL(j-1)
  val2_tl=val2_tl+gpsptr%termtlin(j)*tl_TL_tl(j-1)
  enddo
  endif
  val2=val2-gpsptr%res
  val2_tl=val2_tl-gpsdataerr_tl(i)

! gradient
  val=gpsptr%termqin*rq_TLM+gpsptr%termpin*rp_TLM+ &
      gpsptr%termtin*rt_TLM
  val_tl=gpsptr%termqin*rq_TLM_tl+gpsptr%termpin*rp_TLM_tl+ &
      gpsptr%termtin*rt_TLM_tl
  if(k==1) then
    val=val+two*gpsptr%termtkin*rtl_TL(k1)
    val_tl=val_tl+two*gpsptr%termtkin*rtl_TL_tl(k1)
  else
    val=val+gpsptr%termtkin*rtl_TL(k1)+gpsptr%termtkin*rtl_TL(k1-1)
    val_tl=val_tl+gpsptr%termtkin*rtl_TL_tl(k1)+gpsptr%termtkin*rtl_TL_tl(k1-1)
  endif
  if(k1.ge.2) then
  do j=2,k1
  val=val+gpsptr%termtlin(j)*rtl_TL(j-1)
  val_tl=val_tl+gpsptr%termtlin(j)*rtl_TL_tl(j-1)
  enddo
  endif

  nref1=val2+sges1*val
  nref2=val2+sges2*val
  nref3=val2+sges3*val
  nref1_tl=val2_tl+sges1_tl*val+sges1*val_tl
  nref2_tl=val2_tl+sges2_tl*val+sges2*val_tl
  nref3_tl=val2_tl+sges3_tl*val+sges3*val_tl

  exp_arg  = -half*val2*val2*gpsptr%err2
  exp_arg1 = -half*nref1*nref1*gpsptr%err2
  exp_arg2 = -half*nref2*nref2*gpsptr%err2
  exp_arg3 = -half*nref3*nref3*gpsptr%err2
  exp_arg_tl  = -val2*val2_tl*gpsptr%err2
  exp_arg1_tl = -nref1*nref1_tl*gpsptr%err2
  exp_arg2_tl = -nref2*nref2_tl*gpsptr%err2
  exp_arg3_tl = -nref3*nref3_tl*gpsptr%err2

  if (nlnqc_iter .and. gpsptr%pg > tiny_r_kind) then
     cg_gps=cg_term/gpsptr%b
     wnotgross= one-gpsptr%pg
     wgross = gpsptr%pg*cg_gps
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
  
  pencur_tl = term_tl
  pen1_tl   = term1_tl
  pen2_tl   = term2_tl
  pen3_tl   = term3_tl
  pencur = term
  pen1   = term1
  pen2   = term2
  pen3   = term3

! Cost function, b1, and b3
  pen_tl = pen_tl-two*pencur_tl*gpsptr%raterr2
  b1_tl  = b1_tl-two*(pen1_tl-pen2_tl)*gpsptr%raterr2
  b3_tl  = b3_tl-two*(pen3_tl-pen2_tl)*gpsptr%raterr2
  pen = pen-two*pencur*gpsptr%raterr2
  b1  = b1-two*(pen1-pen2)*gpsptr%raterr2
  b3  = b3-two*(pen3-pen2)*gpsptr%raterr2
  endif
  
  gpsptr => gpsptr%llpoint

  end do

 return
end subroutine stpref_tl

end module stprefmod
