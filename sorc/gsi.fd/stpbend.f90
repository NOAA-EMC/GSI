module stpbendmod

!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    stpbendmod    module for stpbend and its tangent linear stpbend_tl
!
! abstract: module for stpbend and its tangent linear stpbend_tl
!
! program history log:
!   2005-12-02  cucurull 
!

implicit none

PRIVATE
PUBLIC stpbend,stpbend_tl

contains

subroutine stpbend(rt,rq,rp,st,sq,sp,pen,b1,b3,sges1,sges2,sges3)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram: stpbend    compute contribution to penalty and stepsize
!                        from GPS local bending angle, using nonlinear qc    
!   prgmmr: cucurull,l.     org: JCSDA/NCEP           date: 2005-12-01
!
! abstract:  This routine applies the (linear) operator for local 
!            gps bending nagle and linear linear estimate for step size. 
!            This version includes nonlinear qc.
!
! program history log:
!   2005-12-01  cucurull 
!   2006-03-07  todling  - bug fix: nsig_up was declared as real
!   2007-07-28  derber   - modify to use new inner loop obs data structure
!                        - unify NL qc
!   2006-09-18  derber   - modify output parameters b1 and b3
!   2006-09-20  cucurull - generalize code to hybrid vertical coordinate and modify to use
!                          surface pressure
!                        - fix bug in gpsptr%ij (replace "i" with "j")
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
!     pen   - contribution to penalty from local gps bending angle
!     b1    - contribution to numerator from local gps bending angle
!     b3    - contribution to denomenator from local gps bending angle
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,i_kind
  use obsmod, only: gpshead,gpsptr,grids_dim
  use qcmod, only: nlnqc_iter
  use constants, only: zero,one,two,n_a,n_b,rd,grav,half,eps,tiny_r_kind,cg_term
  use gridmod, only: latlon1n,latlon11,nsig,bk5
  use lagmod
  use jfunc, only: iter
  implicit none

! Declare local parameters
  real(r_kind),parameter:: ten = 10.0_r_kind
  real(r_kind),parameter:: ds=5000.0_r_kind
  real(r_kind),parameter:: r1em6=1.0e-6_r_kind

! Declare passed variables
  real(r_kind),intent(out):: pen,b1,b3
  real(r_kind),dimension(latlon1n),intent(in):: rt,rq,st,sq
  real(r_kind),dimension(latlon11),intent(in):: rp,sp 
  real(r_kind),intent(in):: sges1,sges2,sges3

! Declare local variables
  integer(i_kind) i,j,k,ihob,nsig_up
  integer(i_kind),dimension(nsig):: i1,i2,i3,i4
  real(r_kind) val,val2
  real(r_kind) w1_gps,w2_gps,w3_gps,w4_gps
  real(r_kind) :: p_TL,rp_TL
  real(r_kind),dimension(nsig) :: rq_TL,rt_TL,q_TL,t_TL
  real(r_kind) qmean_TL,tmean_TL,rqmean_TL,rtmean_TL

  real(r_kind) cg_gps,pen1,pen2,pen3,pencur,nref1,nref2,nref3,wgross,wnotgross

  real(r_kind) ddbend,dz_TL,rdog,rsig,hob_s,d_ref_rad_TL,d_ref_rad
  real(r_kind) alpha,ccoef,bcoef1,bcoef2,cc
  real(r_kind),dimension(nsig) :: irefges,height_TL,h_TL
  real(r_kind),dimension(3,nsig+10) :: q_w,q_w_TL
  real(r_kind),dimension(4) :: dw4_TL,w4,dw4,w4_TL
  real(r_kind),dimension(grids_dim) :: dbeta_TL
  real(r_kind),dimension(nsig+10) :: n_TL
  real(r_kind),dimension(0:nsig+11) ::  ref_rad,xi_TL


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
  w1_gps=gpsptr%wij(1)
  w2_gps=gpsptr%wij(2)
  w3_gps=gpsptr%wij(3)
  w4_gps=gpsptr%wij(4)

! get weights

  rp_TL=w1_gps*rp(i1(1))+w2_gps*rp(i2(1))+w3_gps*rp(i3(1))&
       +w4_gps*rp(i4(1))

  p_TL=w1_gps*sp(i1(1))+w2_gps*sp(i2(1))+w3_gps*sp(i3(1))&
       +w4_gps*sp(i4(1))

  do j=1,nsig
   t_TL(j)=w1_gps*st(i1(j))+w2_gps*st(i2(j))+w3_gps*st(i3(j))&
           +w4_gps*st(i4(j))
   rt_TL(j)=w1_gps*rt(i1(j))+w2_gps*rt(i2(j))+w3_gps*rt(i3(j))&
            +w4_gps*rt(i4(j))
   q_TL(j)=w1_gps*sq(i1(j))+w2_gps*sq(i2(j))+w3_gps*sq(i3(j))&
           +w4_gps*sq(i4(j))
   rq_TL(j)=w1_gps*rq(i1(j))+w2_gps*rq(i2(j))+w3_gps*rq(i3(j))&
            +w4_gps*rq(i4(j))
  enddo

! penalty

! Initialize some arrays
  h_TL=zero;n_TL=zero;xi_TL=zero
  d_ref_rad_TL=zero; dbeta_TL=zero
  q_w_TL=zero;w4_TL=zero;dw4_TL=zero ! Lagrange weights

  nsig_up=nsig+10 ! extend 10 levels above level nsig
  rsig=float(nsig)

! Geopotential heights
  rdog = rd/grav
  height_TL(1) = zero
  do k=2,nsig
     dz_TL = t_TL(k-1) * (gpsptr%b_pkges(k-1)-gpsptr%b_pkges(k)) +&
             gpsptr%b_tkges(k-1)*&
             (bk5(k-1)/exp(gpsptr%b_pkges(k-1))-bk5(k)/exp(gpsptr%b_pkges(k)))*p_TL
     height_TL(k) = height_TL(k-1) + rdog * dz_TL
  end do
  do k=1,nsig
     h_TL(k)=height_TL(k)
  end do

! Increment of refractivity and index of refractivity - radius product
  do k=1,nsig
    if(k>1) then
     qmean_TL=(q_TL(k)+q_TL(k-1))/two
     tmean_TL=(t_TL(k)+t_TL(k-1))/two
    else
     qmean_TL=q_TL(1)
     tmean_TL=t_TL(1)
    endif
    irefges(k)= one+(r1em6*gpsptr%b_n(k))  ! index of refractivity n_i
    ref_rad(k)=irefges(k)*gpsptr%b_rges(k) ! refractivity index-radius product x_i
    n_TL(k)=gpsptr%b_tin(k)*tmean_TL+gpsptr%b_qin(k)*qmean_TL+ &
            gpsptr%b_pin(k)*p_TL
    xi_TL(k)=r1em6*gpsptr%b_rges(k)*n_TL(k)+ &
                   gpsptr%b_gp2gm(k)*h_TL(k)*irefges(k)
  end do

! Extending atmosphere above nsig
  d_ref_rad=ref_rad(nsig)-ref_rad(nsig-1)
  d_ref_rad_TL=xi_TL(nsig)-xi_TL(nsig-1)
  do k=1,10
   ref_rad(nsig+k)=ref_rad(nsig)+ k*d_ref_rad
   xi_TL(nsig+k)=xi_TL(nsig)+ k*d_ref_rad_TL
   n_TL(nsig+k)=(two*gpsptr%b_n(nsig+k-1)*n_TL(nsig+k-1)/gpsptr%b_n(nsig+k-2))-&
                  (gpsptr%b_n(nsig+k-1)**2/gpsptr%b_n(nsig+k-2)**2)*n_TL(nsig+k-2)
  end do

! Lagrange coefficients
  ref_rad(0)=ref_rad(3)
  ref_rad(nsig_up+1)=ref_rad(nsig_up-2)
  xi_TL(0)=xi_TL(3)
  xi_TL(nsig_up+1)=xi_TL(nsig_up-2)
  do k=1,nsig_up
   call setq_TL(q_w(:,k),q_w_TL(:,k),ref_rad(k-1:k+1),xi_TL(k-1:k+1),3)
  enddo
!
  intloop: do j=1,grids_dim
    hob_s=gpsptr%b_loc(j)
    ihob=hob_s
    w4_TL=zero;dw4_TL=zero

! Compute refractivity and derivative at target points using Lagrange interpolators
    call slagdw_TL(ref_rad(ihob-1:ihob+2),xi_TL(ihob-1:ihob+2),&
               gpsptr%b_xj(j),&
               q_w(:,ihob),q_w_TL(:,ihob),&
               q_w(:,ihob+1),q_w_TL(:,ihob+1),&
               w4_TL,dw4,dw4_TL,4)
    if(ihob==1) then
      dw4(4)=dw4(4)+dw4(1);dw4(1:3)=dw4(2:4);dw4(4)=zero
      dw4_TL(4)=dw4_TL(4)+dw4_TL(1);dw4_TL(1:3)=dw4_TL(2:4);dw4_TL(4)=zero
      ihob=ihob+1
    endif
    if(ihob==nsig_up-1) then
      dw4(1)=dw4(1)+dw4(4); dw4(2:4)=dw4(1:3);dw4(1)=zero
      dw4_TL(1)=dw4_TL(1)+dw4_TL(4); dw4_TL(2:4)=dw4_TL(1:3);dw4_TL(1)=zero
      ihob=ihob-1
    endif
    dbeta_TL(j)=(r1em6/gpsptr%b_xj(j))*&
      (dot_product(dw4_TL,gpsptr%b_n(ihob-1:ihob+2))+&
       dot_product(dw4,n_TL(ihob-1:ihob+2)))
  end do intloop

  val2=ds*dbeta_TL(1)
  do j=2,grids_dim
     ddbend=ds*dbeta_TL(j)
     val2=val2+two*ddbend
  end do
  val2=-gpsptr%b_imp*val2

  val2=val2-gpsptr%res

! gradient

! Initialize some arrays
  h_TL=zero;n_TL=zero;xi_TL=zero
  d_ref_rad_TL=zero; dbeta_TL=zero
  q_w_TL=zero;w4_TL=zero;dw4_TL=zero ! Lagrange weights

! Geopotential heights
  height_TL(1) = zero
  do k=2,nsig
     dz_TL = rt_TL(k-1) * (gpsptr%b_pkges(k-1)-gpsptr%b_pkges(k)) +&
            gpsptr%b_tkges(k-1)*&
            (bk5(k-1)/exp(gpsptr%b_pkges(k-1))-bk5(k)/exp(gpsptr%b_pkges(k)))*rp_TL
     height_TL(k) = height_TL(k-1) + rdog * dz_TL
  end do
  do k=1,nsig
     h_TL(k)=height_TL(k)
  end do

! Increment of refractivity and index of refractivity - radius product
  do k=1,nsig
   if(k>1) then
    rqmean_TL=(rq_TL(k)+rq_TL(k-1))/two
    rtmean_TL=(rt_TL(k)+rt_TL(k-1))/two
   else
    rqmean_TL=rq_TL(1)
    rtmean_TL=rt_TL(1)
   endif
   n_TL(k)=gpsptr%b_tin(k)*rtmean_TL+gpsptr%b_qin(k)*rqmean_TL+ &
           gpsptr%b_pin(k)*rp_TL
   xi_TL(k)=r1em6*gpsptr%b_rges(k)*n_TL(k)+ &
            gpsptr%b_gp2gm(k)*h_TL(k)*irefges(k)
  end do

! Extending atmosphere above nsig
  d_ref_rad=ref_rad(nsig)-ref_rad(nsig-1)
  d_ref_rad_TL=xi_TL(nsig)-xi_TL(nsig-1)
  do k=1,10
   ref_rad(nsig+k)=ref_rad(nsig)+ k*d_ref_rad
   xi_TL(nsig+k)=xi_TL(nsig)+ k*d_ref_rad_TL
   n_TL(nsig+k)=(two*gpsptr%b_n(nsig+k-1)*n_TL(nsig+k-1)/gpsptr%b_n(nsig+k-2))-&
                  (gpsptr%b_n(nsig+k-1)**2/gpsptr%b_n(nsig+k-2)**2)*n_TL(nsig+k-2)
  end do

! Lagrange coefficients
  ref_rad(0)=ref_rad(3)
  ref_rad(nsig_up+1)=ref_rad(nsig_up-2)
  xi_TL(0)=xi_TL(3)
  xi_TL(nsig_up+1)=xi_TL(nsig_up-2)
  do k=1,nsig_up
   call setq_TL(q_w(:,k),q_w_TL(:,k),ref_rad(k-1:k+1),xi_TL(k-1:k+1),3)
  enddo
!
  do j=1,grids_dim
    hob_s=gpsptr%b_loc(j)
    ihob=hob_s
    w4_TL=zero;dw4_TL=zero
! Compute refractivity and derivative at target points using Lagrange interpolators
    call slagdw_TL(ref_rad(ihob-1:ihob+2),xi_TL(ihob-1:ihob+2),&
               gpsptr%b_xj(j),&
               q_w(:,ihob),q_w_TL(:,ihob),&
               q_w(:,ihob+1),q_w_TL(:,ihob+1),&
               w4_TL,dw4,dw4_TL,4)
    if(ihob==1) then
      dw4(4)=dw4(4)+dw4(1);dw4(1:3)=dw4(2:4);dw4(4)=zero
      dw4_TL(4)=dw4_TL(4)+dw4_TL(1);dw4_TL(1:3)=dw4_TL(2:4);dw4_TL(4)=zero
      ihob=ihob+1
    endif
    if(ihob==nsig_up-1) then
      dw4(1)=dw4(1)+dw4(4); dw4(2:4)=dw4(1:3);dw4(1)=zero
      dw4_TL(1)=dw4_TL(1)+dw4_TL(4); dw4_TL(2:4)=dw4_TL(1:3);dw4_TL(1)=zero
      ihob=ihob-1
    endif
    dbeta_TL(j)=(r1em6/gpsptr%b_xj(j))*&
      (dot_product(dw4_TL,gpsptr%b_n(ihob-1:ihob+2))+&
       dot_product(dw4,n_TL(ihob-1:ihob+2)))
  end do

  val=ds*dbeta_TL(1)
  do j=2,grids_dim
     ddbend=ds*dbeta_TL(j)
     val=val+two*ddbend
  end do
  val=-gpsptr%b_imp*val

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
     pencur  = -two*log((exp(-half*pencur) + wgross)/(one+wgross))
     pen1    = -two*log((exp(-half*pen1  ) + wgross)/(one+wgross))
     pen2    = -two*log((exp(-half*pen2  ) + wgross)/(one+wgross))
     pen3    = -two*log((exp(-half*pen3  ) + wgross)/(one+wgross))
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
end subroutine stpbend


subroutine stpbend_tl(rt,rq,rp,st,sq,sp,pen,b1,b3,sges1,sges2,sges3, &
                    rt_tl,rq_tl,rp_tl,st_tl,sq_tl,sp_tl,pen_tl,b1_tl,b3_tl,sges1_tl,sges2_tl,sges3_tl)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram: stpbend_tl   the tangent linear of the operator that computes contribution to 
!                         penalty and stepsize from bend, using nonlinear qc    
!   prgmmr: cucurull    org: JCSDA/NCEP          date: 2005-12-02
!
! abstract:  This routine is the tangent linear of the operator that applies the (linear) 
!            operator for local refractivity and linear linear estimate for step size. 
!            This version includes nonlinear qc.
!
! program history log:
!   2005-12-02  cucurull - tangent linear of stpbend
!   2006-03-07  todling  - bug fix: nsig_up was declared as real
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
  use obsmod, only: gpshead,gpsptr,grids_dim
  use obsmod_tl, only: gpsdataerr_tl
  use qcmod, only: nlnqc_iter
  use constants, only: zero,one,two,rd,grav,n_a,n_b,half,tiny_r_kind,cg_term
  use gridmod, only: latlon1n,latlon11,nsig,bk5
  use lagmod
  implicit none

! Declare local parameters
  real(r_kind),parameter:: ten = 10.0_r_kind
  real(r_kind),parameter:: ds=5000.0_r_kind
  real(r_kind),parameter:: r1em6=1.0e-6_r_kind

! Declare passed variables
  real(r_kind),intent(out):: pen,b1,b3
  real(r_kind),intent(out):: pen_tl,b1_tl,b3_tl
  real(r_kind),dimension(latlon1n),intent(in):: rt,rq,st,sq
  real(r_kind),dimension(latlon1n),intent(in):: rt_tl,rq_tl,st_tl,sq_tl
  real(r_kind),dimension(latlon11),intent(in):: rp,sp,rp_tl,sp_tl
  real(r_kind),intent(in):: sges1,sges2,sges3
  real(r_kind),intent(in):: sges1_tl,sges2_tl,sges3_tl

! Declare local variables
  integer(i_kind) i,j,k,ihob,nsig_up
  integer(i_kind),dimension(nsig):: i1,i2,i3,i4
  real(r_kind) val,val2
  real(r_kind) w1_gps,w2_gps,w3_gps,w4_gps
  real(r_kind) val_tl,val2_tl
  real(r_kind) :: p_TL,rp_TLM
  real(r_kind),dimension(nsig) :: rq_TLM,rt_TLM,q_TL,t_TL
  real(r_kind) :: p_TL_tl,rp_TLM_tl
  real(r_kind),dimension(nsig) :: rq_TLM_tl,rt_TLM_tl,q_TL_tl,t_TL_tl
  real(r_kind) qmean_TL,tmean_TL,qmean_TL_tl,tmean_TL_tl


  real(r_kind) cg_gps,pen1,pen2,pen3,pencur,nref1,nref2,nref3,wgross,wnotgross
  real(r_kind) pen1_tl,pen2_tl,pen3_tl,pencur_tl,nref1_tl,nref2_tl,nref3_tl
  real(r_kind) term,term1,term2,term3
  real(r_kind) term_tl,term1_tl,term2_tl,term3_tl
  real(r_kind) exp_arg,exp_arg1,exp_arg2,exp_arg3
  real(r_kind) exp_arg_tl,exp_arg1_tl,exp_arg2_tl,exp_arg3_tl
  real(r_kind) temp

  real(r_kind) ddbend,dz_TL,rdog,rsig,h,hob_s
  real(r_kind) :: d_ref_rad_TLM,d_ref_rad
  real(r_kind),dimension(nsig) :: irefges,height_TL,h_TL
  real(r_kind),dimension(3,nsig+10) :: q_w,q_w_TL
  real(r_kind),dimension(4) :: dw4_TL,w4,dw4,w4_TL
  real(r_kind),dimension(grids_dim) :: dbeta_TL
  real(r_kind),dimension(nsig+10) :: n_TL
  real(r_kind),dimension(0:nsig+11) ::  ref_rad,xi_TL

  real(r_kind) :: d_ref_rad_TLM_tl,d_ref_rad_tl
  real(r_kind),dimension(nsig) :: height_TL_tl,h_TL_tl
  real(r_kind),dimension(3,nsig+10) :: q_w_TL_tl
  real(r_kind),dimension(4) :: dw4_TL_tl,w4_TL_tl
  real(r_kind),dimension(grids_dim) :: dbeta_TL_tl
  real(r_kind),dimension(nsig+10) :: n_TL_tl
  real(r_kind),dimension(0:nsig+11) ::  xi_TL_tl


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
  q_TL_tl= zero
  t_TL_tl= zero
  p_TL_tl= zero
  rq_TLM_tl=zero
  rt_TLM_tl=zero
  rp_TLM_tl=zero

! Loop over observations
  gpsptr => gpshead
  i=0
  do while (associated(gpsptr))
    i=i+1
    if(gpsptr%luse)then

  do j=1,nsig
   i1(j)= gpsptr%ij(1,i)
   i2(j)= gpsptr%ij(2,i)
   i3(j)= gpsptr%ij(3,i)
   i4(j)= gpsptr%ij(4,i)
  enddo
  w1_gps=gpsptr%wij(1)
  w2_gps=gpsptr%wij(2)
  w3_gps=gpsptr%wij(3)
  w4_gps=gpsptr%wij(4)

! get weights
  rp_TLM=w1_gps*rp(i1(1))+w2_gps*rp(i2(1))+w3_gps*rp(i3(1))&
       +w4_gps*rp(i4(1))

  p_TL=w1_gps*sp(i1(1))+w2_gps*sp(i2(1))+w3_gps*sp(i3(1))&
       +w4_gps*sp(i4(1))

  do j=1,nsig
   t_TL(j)=w1_gps*st(i1(j))+w2_gps*st(i2(j))+w3_gps*st(i3(j))&
           +w4_gps*st(i4(j))
   rt_TLM(j)=w1_gps*rt(i1(j))+w2_gps*rt(i2(j))+w3_gps*rt(i3(j))&
            +w4_gps*rt(i4(j))
   q_TL(j)=w1_gps*sq(i1(j))+w2_gps*sq(i2(j))+w3_gps*sq(i3(j))&
           +w4_gps*sq(i4(j))
   rq_TLM(j)=w1_gps*rq(i1(j))+w2_gps*rq(i2(j))+w3_gps*rq(i3(j))&
            +w4_gps*rq(i4(j))
   t_TL_tl(j)=w1_gps*st_tl(i1(j))+w2_gps*st_tl(i2(j))+w3_gps*st_tl(i3(j))&
           +w4_gps*st_tl(i4(j))
   rt_TLM_tl(j)=w1_gps*rt_tl(i1(j))+w2_gps*rt_tl(i2(j))+w3_gps*rt_tl(i3(j))&
            +w4_gps*rt_tl(i4(j))
   q_TL_tl(j)=w1_gps*sq_tl(i1(j))+w2_gps*sq_tl(i2(j))+w3_gps*sq_tl(i3(j))&
           +w4_gps*sq_tl(i4(j))
   rq_TLM_tl(j)=w1_gps*rq_tl(i1(j))+w2_gps*rq_tl(i2(j))+w3_gps*rq_tl(i3(j))&
            +w4_gps*rq_tl(i4(j))
  enddo

  rp_TLM_tl=w1_gps*rp_tl(i1(1))+w2_gps*rp_tl(i2(1))+w3_gps*rp_tl(i3(1))&
       +w4_gps*rp_tl(i4(1))

  p_TL_tl=w1_gps*sp_tl(i1(1))+w2_gps*sp_tl(i2(1))+w3_gps*sp_tl(i3(1))&
       +w4_gps*sp_tl(i4(1))

! penalty

! Initialize some arrays
  h_TL=zero;n_TL=zero;xi_TL=zero
  d_ref_rad_TLM=zero; dbeta_TL=zero
  q_w_TL=zero;w4_TL=zero;dw4_TL=zero ! Lagrange weights

  h_TL_tl=zero;n_TL_tl=zero;xi_TL_tl=zero
  d_ref_rad_TLM_tl=zero; dbeta_TL_tl=zero
  q_w_TL_tl=zero;w4_TL_tl=zero;dw4_TL_tl=zero ! Lagrange weights

  nsig_up=nsig+10 ! extend levels
  rsig=float(nsig)

! Geopotential heights
  rdog = rd/grav
  height_TL(1) = zero
  do k=2,nsig
     dz_TL = t_TL(k-1) * (gpsptr%b_pkges(k-1)-gpsptr%b_pkges(k)) +&
             gpsptr%b_tkges(k-1)*&
             (bk5(k-1)/exp(gpsptr%b_pkges(k-1))-bk5(k)/exp(gpsptr%b_pkges(k)))*p_TL
     height_TL(k) = height_TL(k-1) + rdog * dz_TL
  end do
  do k=1,nsig
     h_TL(k)=height_TL(k)
  end do

  height_TL_tl(1) = zero
  do k=2,nsig
     dz_TL = t_TL_tl(k-1) * (gpsptr%b_pkges(k-1)-gpsptr%b_pkges(k)) +&
             gpsptr%b_tkges(k-1)*&
             (bk5(k-1)/exp(gpsptr%b_pkges(k-1))-bk5(k)/exp(gpsptr%b_pkges(k)))*p_TL_tl
     height_TL_tl(k) = height_TL_tl(k-1) + rdog * dz_TL
  end do
  do k=1,nsig
     h_TL_tl(k)=height_TL_tl(k)
  end do

! Increment of refractivity and index of refractivity - radius product
  do k=1,nsig
    if(k>1) then
     qmean_TL=(q_TL(k)+q_TL(k-1))/two
     tmean_TL=(t_TL(k)+t_TL(k-1))/two
     qmean_TL_tl=(q_TL_tl(k)+q_TL_tl(k-1))/two
     tmean_TL_tl=(t_TL_tl(k)+t_TL_tl(k-1))/two
    else
     qmean_TL=q_TL(1)
     tmean_TL=t_TL(1)
     qmean_TL_tl=q_TL_tl(1)
     tmean_TL_tl=t_TL_tl(1)
    endif
    irefges(k)= one+(r1em6*gpsptr%b_n(k))  ! index of refractivity
    ref_rad(k)=irefges(k)*gpsptr%b_rges(k) ! refractivity index-radius product
    n_TL(k)=gpsptr%b_tin(k)*tmean_TL+gpsptr%b_qin(k)*qmean_TL+ &
            gpsptr%b_pin(k)*p_TL
    xi_TL(k)=r1em6*gpsptr%b_rges(k)*n_TL(k)+ &
                   gpsptr%b_gp2gm(k)*h_TL(k)*irefges(k)
    n_TL_tl(k)=gpsptr%b_tin(k)*tmean_TL_tl+gpsptr%b_qin(k)*qmean_TL_tl+ &
               gpsptr%b_pin(k)*p_TL_tl
    xi_TL_tl(k)=r1em6*gpsptr%b_rges(k)*n_TL_tl(k)+ &
                gpsptr%b_gp2gm(k)*h_TL_tl(k)*irefges(k)
  end do

! Extending atmosphere above nsig
  d_ref_rad=ref_rad(nsig)-ref_rad(nsig-1)
  d_ref_rad_TLM=xi_TL(nsig)-xi_TL(nsig-1)
  d_ref_rad_TLM_tl=xi_TL_tl(nsig)-xi_TL_tl(nsig-1)
  do k=1,10
   ref_rad(nsig+k)=ref_rad(nsig)+ k*d_ref_rad
   xi_TL(nsig+k)=xi_TL(nsig)+ k*d_ref_rad_TLM
   n_TL(nsig+k)=(two*gpsptr%b_n(nsig+k-1)*n_TL(nsig+k-1)/gpsptr%b_n(nsig+k-2))-&
                  (gpsptr%b_n(nsig+k-1)**2/gpsptr%b_n(nsig+k-2)**2)*n_TL(nsig+k-2)
   xi_TL_tl(nsig+k)=xi_TL_tl(nsig)+ k*d_ref_rad_TLM_tl
   n_TL_tl(nsig+k)=(two*gpsptr%b_n(nsig+k-1)*n_TL_tl(nsig+k-1)/gpsptr%b_n(nsig+k-2))-&
                  (gpsptr%b_n(nsig+k-1)**2/gpsptr%b_n(nsig+k-2)**2)*n_TL_tl(nsig+k-2)
  end do

! Lagrange coefficients
  ref_rad(0)=ref_rad(3)
  ref_rad(nsig_up+1)=ref_rad(nsig_up-2)
  xi_TL(0)=xi_TL(3)
  xi_TL(nsig_up+1)=xi_TL(nsig_up-2)
  xi_TL_tl(0)=xi_TL_tl(3)
  xi_TL_tl(nsig_up+1)=xi_TL_tl(nsig_up-2)
  do k=1,nsig_up
   call setq_TL(q_w(:,k),q_w_TL(:,k),ref_rad(k-1:k+1),xi_TL(k-1:k+1),3)
   call setq_TL(q_w(:,k),q_w_TL_tl(:,k),ref_rad(k-1:k+1),xi_TL_tl(k-1:k+1),3)
  enddo
!
  intloop: do j=1,grids_dim
    hob_s=gpsptr%b_loc(j)
    ihob=hob_s
    w4_TL=zero;dw4_TL=zero
    w4_TL_tl=zero;dw4_TL_tl=zero

! Compute refractivity and derivative at target points using Lagrange interpolators
    call slagdw_TL(ref_rad(ihob-1:ihob+2),xi_TL(ihob-1:ihob+2),&
               gpsptr%b_xj(j),&
               q_w(:,ihob),q_w_TL(:,ihob),&
               q_w(:,ihob+1),q_w_TL(:,ihob+1),&
               w4_TL,dw4,dw4_TL,4)
    call slagdw_TL(ref_rad(ihob-1:ihob+2),xi_TL_tl(ihob-1:ihob+2),&
               gpsptr%b_xj(j),&
               q_w(:,ihob),q_w_TL_tl(:,ihob),&
               q_w(:,ihob+1),q_w_TL_tl(:,ihob+1),&
               w4_TL_tl,dw4,dw4_TL_tl,4)
    if(ihob==1) then
      dw4(4)=dw4(4)+dw4(1);dw4(1:3)=dw4(2:4);dw4(4)=zero
      dw4_TL(4)=dw4_TL(4)+dw4_TL(1);dw4_TL(1:3)=dw4_TL(2:4);dw4_TL(4)=zero
      dw4_TL_tl(4)=dw4_TL_tl(4)+dw4_TL_tl(1);dw4_TL_tl(1:3)=dw4_TL_tl(2:4);dw4_TL_tl(4)=zero
      ihob=ihob+1
    endif
    if(ihob==nsig_up-1) then
      dw4(1)=dw4(1)+dw4(4); dw4(2:4)=dw4(1:3);dw4(1)=zero
      dw4_TL(1)=dw4_TL(1)+dw4_TL(4);dw4_TL(2:4)=dw4_TL(1:3);dw4_TL(1)=zero
      dw4_TL_tl(1)=dw4_TL_tl(1)+dw4_TL_tl(4);dw4_TL_tl(2:4)=dw4_TL_tl(1:3);dw4_TL_tl(1)=zero
      ihob=ihob-1
    endif
    dbeta_TL(j)=(r1em6/gpsptr%b_xj(j))*&
      (dot_product(dw4_TL,gpsptr%b_n(ihob-1:ihob+2))+&
       dot_product(dw4,n_TL(ihob-1:ihob+2)))
    dbeta_TL_tl(j)=(r1em6/gpsptr%b_xj(j))*&
      (dot_product(dw4_TL_tl,gpsptr%b_n(ihob-1:ihob+2))+&
       dot_product(dw4,n_TL_tl(ihob-1:ihob+2)))
  end do intloop

  val2=ds*dbeta_TL(1)
  do j=2,grids_dim
     ddbend=ds*dbeta_TL(j)
     val2=val2+two*ddbend
  end do
  val2=-gpsptr%b_imp*val2
  val2=val2-gpsptr%res

  val2_tl=ds*dbeta_TL_tl(1)
  do j=2,grids_dim
     ddbend=ds*dbeta_TL_tl(j)
     val2_tl=val2_tl+two*ddbend
  end do
  val2_tl=-gpsptr%b_imp*val2_tl
  val2_tl=val2_tl-gpsdataerr_tl(i)

! gradient

! Initialize some arrays
  h_TL=zero;n_TL=zero;xi_TL=zero
  d_ref_rad_TLM=zero; dbeta_TL=zero
  q_w_TL=zero;w4_TL=zero;dw4_TL=zero ! Lagrange weights

  h_TL_tl=zero;n_TL_tl=zero;xi_TL_tl=zero
  d_ref_rad_TLM_tl=zero; dbeta_TL_tl=zero
  q_w_TL_tl=zero;w4_TL_tl=zero;dw4_TL_tl=zero ! Lagrange weights

! Geopotential heights
  height_TL(1) = zero
  do k=2,nsig
     dz_TL = rt_TLM(k-1) * (gpsptr%b_pkges(k-1)-gpsptr%b_pkges(k)) +&
             gpsptr%b_tkges(k-1)*&
             (bk5(k-1)/exp(gpsptr%b_pkges(k-1))-bk5(k)/exp(gpsptr%b_pkges(k)))*rp_TLM
     height_TL(k) = height_TL(k-1) + rdog * dz_TL
  end do
  do k=1,nsig
     h_TL(k)=height_TL(k)
  end do

  height_TL_tl(1) = zero
  do k=2,nsig
     dz_TL = rt_TLM_tl(k-1) * (gpsptr%b_pkges(k-1)-gpsptr%b_pkges(k)) +&
             gpsptr%b_tkges(k-1)*&
             (bk5(k-1)/exp(gpsptr%b_pkges(k-1))-bk5(k)/exp(gpsptr%b_pkges(k)))*rp_TLM_tl
     height_TL_tl(k) = height_TL_tl(k-1) + rdog * dz_TL
  end do
  do k=1,nsig
     h_TL_tl(k)=height_TL_tl(k)
  end do

! Increment of refractivity and index of refractivity - radius product
  do k=1,nsig
   irefges(k)= one+(r1em6*gpsptr%b_n(k))  ! index of refractivity
   ref_rad(k)=irefges(k)*gpsptr%b_rges(k)               ! refractivity index-radius product
   n_TL(k)=gpsptr%b_tin(k)*rt_TLM(k)+gpsptr%b_qin(k)*rq_TLM(k)+ &
           gpsptr%b_pin(k)*rp_TLM
   xi_TL(k)=r1em6*gpsptr%b_rges(k)*n_TL(k)+ &
                  gpsptr%b_gp2gm(k)*h_TL(k)*irefges(k)
   n_TL_tl(k)=gpsptr%b_tin(k)*rt_TLM_tl(k)+gpsptr%b_qin(k)*rq_TLM_tl(k)+ &
              gpsptr%b_pin(k)*rp_TLM_tl
   xi_TL_tl(k)=r1em6*gpsptr%b_rges(k)*n_TL_tl(k)+ &
                     gpsptr%b_gp2gm(k)*h_TL_tl(k)*irefges(k)
  end do

! Extending atmosphere above nsig
  d_ref_rad_TLM=xi_TL(nsig)-xi_TL(nsig-1)
  d_ref_rad_TLM_tl=xi_TL_tl(nsig)-xi_TL_tl(nsig-1)
  do k=1,10
   xi_TL(nsig+k)=xi_TL(nsig)+ k*d_ref_rad_TLM
   n_TL(nsig+k)=(two*gpsptr%b_n(nsig+k-1)*n_TL(nsig+k-1)/gpsptr%b_n(nsig+k-2))-&
                  (gpsptr%b_n(nsig+k-1)**2/gpsptr%b_n(nsig+k-2)**2)*n_TL(nsig+k-2)
   xi_TL_tl(nsig+k)=xi_TL_tl(nsig)+ k*d_ref_rad_TLM_tl
   n_TL_tl(nsig+k)=(two*gpsptr%b_n(nsig+k-1)*n_TL_tl(nsig+k-1)/gpsptr%b_n(nsig+k-2))-&
                  (gpsptr%b_n(nsig+k-1)**2/gpsptr%b_n(nsig+k-2)**2)*n_TL_tl(nsig+k-2)
  end do

! Lagrange coefficients
  xi_TL(0)=xi_TL(3)
  xi_TL(nsig_up+1)=xi_TL(nsig_up-2)
  xi_TL_tl(0)=xi_TL_tl(3)
  xi_TL_tl(nsig_up+1)=xi_TL_tl(nsig_up-2)
  do k=1,nsig_up
   call setq_TL(q_w(:,k),q_w_TL(:,k),ref_rad(k-1:k+1),xi_TL(k-1:k+1),3)
   call setq_TL(q_w(:,k),q_w_TL_tl(:,k),ref_rad(k-1:k+1),xi_TL_tl(k-1:k+1),3)
  enddo

  do j=1,grids_dim
     hob_s=gpsptr%b_loc(j)
     ihob=hob_s
     w4_TL=zero;dw4_TL=zero
     w4_TL_tl=zero;dw4_TL_tl=zero

! Compute refractivity and derivative at target points using Lagrange interpolators
     call slagdw_TL(ref_rad(ihob-1:ihob+2),xi_TL(ihob-1:ihob+2),&
                gpsptr%b_xj(j),&
                q_w(:,ihob),q_w_TL(:,ihob),&
                q_w(:,ihob+1),q_w_TL(:,ihob+1),&
                w4_TL,dw4,dw4_TL,4)
     call slagdw_TL(ref_rad(ihob-1:ihob+2),xi_TL_tl(ihob-1:ihob+2),&
               gpsptr%b_xj(j),&
               q_w(:,ihob),q_w_TL_tl(:,ihob),&
               q_w(:,ihob+1),q_w_TL_tl(:,ihob+1),&
               w4_TL_tl,dw4,dw4_TL_tl,4)
     if(ihob==1) then
       dw4(4)=dw4(4)+dw4(1);dw4(1:3)=dw4(2:4);dw4(4)=zero
       dw4_TL(4)=dw4_TL(4)+dw4_TL(1);dw4_TL(1:3)=dw4_TL(2:4);dw4_TL(4)=zero
       dw4_TL_tl(4)=dw4_TL_tl(4)+dw4_TL_tl(1);dw4_TL_tl(1:3)=dw4_TL_tl(2:4);dw4_TL_tl(4)=zero
       ihob=ihob+1
     endif
     if(ihob==nsig_up-1) then
       dw4(1)=dw4(1)+dw4(4); dw4(2:4)=dw4(1:3);dw4(1)=zero
       dw4_TL(1)=dw4_TL(1)+dw4_TL(4); dw4_TL(2:4)=dw4_TL(1:3);dw4_TL(1)=zero
       dw4_TL_tl(1)=dw4_TL_tl(1)+dw4_TL_tl(4);dw4_TL_tl(2:4)=dw4_TL_tl(1:3);dw4_TL_tl(1)=zero
       ihob=ihob-1
     endif
     dbeta_TL(j)=(r1em6/gpsptr%b_xj(j))*&
       (dot_product(dw4_TL,gpsptr%b_n(ihob-1:ihob+2))+&
        dot_product(dw4,n_TL(ihob-1:ihob+2)))
     dbeta_TL_tl(j)=(r1em6/gpsptr%b_xj(j))*&
       (dot_product(dw4_TL_tl,gpsptr%b_n(ihob-1:ihob+2))+&
       dot_product(dw4,n_TL_tl(ihob-1:ihob+2)))
  end do 

  val=ds*dbeta_TL(1)
  do j=2,grids_dim
    ddbend=ds*dbeta_TL(j)
    val=val+two*ddbend
  end do
  val=-gpsptr%b_imp*val

  val_tl=ds*dbeta_TL_tl(1)
  do j=2,grids_dim
    ddbend=ds*dbeta_TL_tl(j)
    val_tl=val_tl+two*ddbend
  end do
  val_tl=-gpsptr%b_imp*val_tl

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
end subroutine stpbend_tl

end module stpbendmod
