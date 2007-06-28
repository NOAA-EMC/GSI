module intbendmod

!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    intbendmod    module for intbend and its tangent linear intbend_tl
!
! abstract: module for intbend and its tangent linear intbend_tl
!
! program history log:
!   2005-12-02 cucurull 
!

implicit none

PRIVATE
PUBLIC intbend,intbend_tl

contains

subroutine intbend(rt,rq,rp,st,sq,sp)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    intbend      apply nonlinqc obs operator for GPS bending angle 
!   prgmmr: cucurull, l.     org: JCSDA/NCEP          date: 2005-12-02
!
! abstract: apply gps bending angle operator and adjoint with
!           addition of nonlinear qc.
!
! program history log:
!   2004-04-29  cucurull- original code
!   2006-03-07  todling - bug fix: nsig_up was declared as real
!   2006-07-28  derber  - modify to use new inner loop obs data structure
!                       - unify NL qc
!   2006-09-19  cucurull - generalize code to hybrid vertical coordinate and modify to use
!                          surface pressure
!   
!   input argument list:
!     st       - input temperature correction field
!     sq       - input q correction field
!     sp       - input p correction field
!
!   output argument list:
!     rt       - output vector after inclusion of gps bending angle
!     rq       - output q vector after inclusion of gps bending angle
!     rp       - output p vector after inclusion of gps bending angle
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,i_kind
  use obsmod, only: gpshead,gpsptr,grids_dim
  use qcmod, only: nlnqc_iter
  use gridmod, only: latlon1n,latlon11,nsig,bk5
  use lagmod
  use jfunc, only: jiter,iter
  use constants, only: zero,one,two,n_a,n_b,grav,rd,half,tiny_r_kind,cg_term
  implicit none

! Declare local parameters
  real(r_kind),parameter:: ten = 10.0_r_kind
  real(r_kind),parameter:: ds=5000.0_r_kind
  real(r_kind),parameter:: r1em6=1.0e-6_r_kind

! Declare passed variables
  real(r_kind),dimension(latlon1n),intent(in):: st,sq
  real(r_kind),dimension(latlon1n),intent(inout):: rt,rq
  real(r_kind),dimension(latlon11),intent(in):: sp
  real(r_kind),dimension(latlon11),intent(inout):: rp


! Declare local variables
  integer(i_kind) i,j,k,ihob,nsig_up
  integer(i_kind),dimension(nsig):: i1,i2,i3,i4
! real(r_kind) penalty
  real(r_kind) w1_gps,w2_gps,w3_gps,w4_gps
  real(r_kind) :: p_TL,p_AD
  real(r_kind),dimension(nsig) :: q_TL,t_TL,q_AD,t_AD
  real(r_kind) :: val
  real(r_kind) cg_gps,grad,p0,wnotgross,wgross

  real(r_kind) ddbend,dz_TL,rdog,rsig,h,hob_s,d_ref_rad_TL,d_ref_rad
  real(r_kind) tmean_TL,qmean_TL,tmean_AD,qmean_AD,tmean_TL_tl,qmean_TL_tl
  real(r_kind) tmean_AD_tl,qmean_AD_tl
  real(r_kind) dbend_AD,ddbend_AD,d_ref_rad_AD
  real(r_kind),dimension(nsig) :: irefges,height_TL,height_AD,dz_AD,h_TL,h_AD
  real(r_kind),dimension(3,nsig+10) :: q_w,q_w_TL,q_w_AD
  real(r_kind),dimension(4) :: dw4_TL,w4,dw4,w4_TL,w4_AD,dw4_AD
  real(r_kind),dimension(grids_dim) :: dbeta_TL,dbeta_AD
  real(r_kind),dimension(nsig+10) :: n_TL,n_AD
  real(r_kind),dimension(0:nsig+11) ::  ref_rad,xi_TL,xi_AD

! Loop over observations

  gpsptr => gpshead
  do while (associated(gpsptr))

! Load location information into local variables
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

!   increments

    p_TL=w1_gps*sp(i1(1))+w2_gps*sp(i2(1))+w3_gps*sp(i3(1))&
        +w4_gps*sp(i4(1))

    do j=1,nsig
      t_TL(j)=w1_gps*st(i1(j))+w2_gps*st(i2(j))+w3_gps*st(i3(j))&
             +w4_gps*st(i4(j))
      q_TL(j)=w1_gps*sq(i1(j))+w2_gps*sq(i2(j))+w3_gps*sq(i3(j))&
             +w4_gps*sq(i4(j))
    enddo


!   local bending angle

!   Initialize some arrays
    h_TL=zero;n_TL=zero;xi_TL=zero
    d_ref_rad_TL=zero; dbeta_TL=zero
    q_w_TL=zero;w4_TL=zero;dw4_TL=zero ! Lagrange weights

    nsig_up=nsig+10 ! extend levels
    rsig=float(nsig)

!   Geopotential heights
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

!   Increment of refractivity and index of refractivity - radius product
    do k=1,nsig
      if(k>1) then
       qmean_TL=(q_TL(k)+q_TL(k-1))/two
       tmean_TL=(t_TL(k)+t_TL(k-1))/two
      else
       qmean_TL=q_TL(1)
       tmean_TL=t_TL(1)
      endif
      irefges(k)= one+(r1em6*gpsptr%b_n(k))  ! index of refractivity
      ref_rad(k)=irefges(k)*gpsptr%b_rges(k) ! refractivity index-radius product
      n_TL(k)=gpsptr%b_tin(k)*tmean_TL+gpsptr%b_qin(k)*qmean_TL+ &
              gpsptr%b_pin(k)*p_TL
      xi_TL(k)=r1em6*gpsptr%b_rges(k)*n_TL(k)+ &
               gpsptr%b_gp2gm(k)*h_TL(k)*irefges(k)
    end do

!   Extending atmosphere above nsig
    d_ref_rad=ref_rad(nsig)-ref_rad(nsig-1)
    d_ref_rad_TL=xi_TL(nsig)-xi_TL(nsig-1)
    do k=1,10
      ref_rad(nsig+k)=ref_rad(nsig)+ k*d_ref_rad
      xi_TL(nsig+k)=xi_TL(nsig)+ k*d_ref_rad_TL
      n_TL(nsig+k)=(two*gpsptr%b_n(nsig+k-1)*n_TL(nsig+k-1)/gpsptr%b_n(nsig+k-2))-&
               (gpsptr%b_n(nsig+k-1)**2/gpsptr%b_n(nsig+k-2)**2)*n_TL(nsig+k-2)
    end do

!   Lagrange coefficients
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

!   Compute refractivity and derivative at target points using Lagrange interpolators
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

    val=ds*dbeta_TL(1)
    do j=2,grids_dim
       ddbend=ds*dbeta_TL(j)
       val=val+two*ddbend
    end do
    val=-gpsptr%b_imp*val

    val=val-gpsptr%res

!   needed for gradient of nonlinear qc operator
    if (nlnqc_iter .and. gpsptr%pg > tiny_r_kind .and. &
                         gpsptr%b  > tiny_r_kind) then
       cg_gps=cg_term/gpsptr%b
       wnotgross= one-gpsptr%pg
       wgross = gpsptr%pg*cg_gps/wnotgross
       p0   = wgross/(wgross+exp(-half*gpsptr%err2*val**2))
       val = val*(one-p0)
    endif

    grad     = val*gpsptr%raterr2*gpsptr%err2

!   adjoint 

!   Initialize (profile dependendant) adjoint variables
    n_AD=zero;q_w_AD=zero;xi_AD=zero
    h_AD=zero
    q_AD=zero;p_AD=zero;t_AD=zero ! model terms

!   initialize obs dependent adjoint variables
    dbend_AD=zero;ddbend_AD=zero;dbeta_AD=zero 

    dbend_AD=dbend_AD-gpsptr%b_imp*grad
    do j=2,grids_dim  
       ddbend_AD=2*dbend_AD
       dbeta_AD(j)=dbeta_AD(j)+ds*ddbend_AD
    enddo
    dbeta_AD(1)=dbeta_AD(1)+ds*dbend_AD

    do j=grids_dim,1,-1
      w4_AD=zero;dw4_AD=zero
      hob_s=gpsptr%b_loc(j)
      ihob=hob_s
!
      call slagdw(ref_rad(ihob-1:ihob+2),gpsptr%b_xj(j),&
                   q_w(:,ihob),q_w(:,ihob+1),&
                   w4,dw4,4)
      if(ihob==1) then
         dw4(4)=dw4(4)+dw4(1);dw4(1:3)=dw4(2:4);dw4(4)=zero
         ihob=ihob+1
      endif
      if(ihob==nsig_up-1) then
         dw4(1)=dw4(1)+dw4(4); dw4(2:4)=dw4(1:3);dw4(1)=zero
         ihob=ihob-1
      endif

      dw4_AD(1)=dw4_AD(1)+&
                (r1em6/gpsptr%b_xj(j))*dbeta_AD(j)*gpsptr%b_n(ihob-1)
      dw4_AD(2)=dw4_AD(2)+&
                (r1em6/gpsptr%b_xj(j))*dbeta_AD(j)*gpsptr%b_n(ihob)
      dw4_AD(3)=dw4_AD(3)+&
                (r1em6/gpsptr%b_xj(j))*dbeta_AD(j)*gpsptr%b_n(ihob+1)
      dw4_AD(4)=dw4_AD(4)+&
                (r1em6/gpsptr%b_xj(j))*dbeta_AD(j)*gpsptr%b_n(ihob+2)
!
      n_AD(ihob-1)=n_AD(ihob-1)+&
                      (r1em6/gpsptr%b_xj(j))*dw4(1)*dbeta_AD(j)
      n_AD(ihob)  =n_AD(ihob)  +&
                      (r1em6/gpsptr%b_xj(j))*dw4(2)*dbeta_AD(j)
      n_AD(ihob+1)=n_AD(ihob+1)+&
                      (r1em6/gpsptr%b_xj(j))*dw4(3)*dbeta_AD(j)
      n_AD(ihob+2)=n_AD(ihob+2)+&
                      (r1em6/gpsptr%b_xj(j))*dw4(4)*dbeta_AD(j)
!
      if(int(hob_s)==nsig_up-1) then
       ihob=ihob+1
       dw4_AD(1:3)=dw4_AD(2:4)
       dw4_AD(4)=dw4_AD(1)
      endif
      if(int(hob_s)==1) then
        ihob=ihob-1
        dw4_AD(2:4)=dw4_AD(1:3)
        dw4_AD(1)=dw4_AD(4)
      endif

!     Compute refractivity and derivative at target points using Lagrange interpolators
      call slagdw_AD(ref_rad(ihob-1:ihob+2),xi_AD(ihob-1:ihob+2),&
          gpsptr%b_xj(j),&
          q_w(:,ihob),q_w_AD(:,ihob),&
          q_w(:,ihob+1),q_w_AD(:,ihob+1),&
          w4_AD,dw4,dw4_AD,4)
    end do ! grids dim

!   Lagrange coefficients
    do k=nsig_up,1,-1
      call setq_AD(q_w_AD(:,k),ref_rad(k-1:k+1),xi_AD(k-1:k+1),3)
    enddo
    xi_AD(nsig_up-2)=xi_AD(nsig_up-2)+xi_AD(nsig_up+1)
    xi_AD(3)=xi_AD(3)+xi_AD(0)

!   Extending atmosphere above nsig
    d_ref_rad_AD=zero
    do k=10,1,-1
      n_AD(nsig+k-1)=n_AD(nsig+k-1)+&
                     n_AD(nsig+k)*two*gpsptr%b_n(nsig+k-1)/gpsptr%b_n(nsig+k-2)
      n_AD(nsig+k-2)=n_AD(nsig+k-2)-&
                     (gpsptr%b_n(nsig+k-1)**2/gpsptr%b_n(nsig+k-2)**2)*n_AD(nsig+k)
      xi_AD(nsig)=xi_AD(nsig)+xi_AD(nsig+k)
      d_ref_rad_AD=d_ref_rad_AD+k*xi_AD(nsig+k)
    end do
    xi_AD(nsig)=xi_AD(nsig)+d_ref_rad_AD
    xi_AD(nsig-1)=xi_AD(nsig-1)-d_ref_rad_AD

!   Increment of refractivity and index of refractivity - radius product
    do k=nsig,1,-1
      n_AD(k)=n_AD(k)+r1em6*gpsptr%b_rges(k)*xi_AD(k)
      h_AD(k)=h_AD(k)+gpsptr%b_gp2gm(k)*irefges(k)*xi_AD(k)
      tmean_AD=gpsptr%b_tin(k)*n_AD(k)
      qmean_AD=gpsptr%b_qin(k)*n_AD(k)
      p_AD=p_AD+gpsptr%b_pin(k)*n_AD(k)
      if(k>1) then
         q_AD(k)=q_AD(k)+qmean_AD/two
         t_AD(k)=t_AD(k)+tmean_AD/two
         q_AD(k-1)=q_AD(k-1)+qmean_AD/two
         t_AD(k-1)=t_AD(k-1)+tmean_AD/two
      endif
      if(k==1) then
         q_AD(1)=q_AD(1)+qmean_AD
         t_AD(1)=t_AD(1)+tmean_AD
      endif
    end do

!   geopotential heights
    height_AD=zero;dz_AD=zero
    do k=1,nsig
       height_AD(k)=height_AD(k)+h_AD(k)
    end do
    do k=nsig,2,-1
      height_AD(k-1)=height_AD(k-1)+height_AD(k)
      dz_AD(k)=dz_AD(k)+ rdog * height_AD(k)
      t_AD(k-1)=t_AD(k-1)+((gpsptr%b_pkges(k-1)-gpsptr%b_pkges(k))*dz_AD(k))
      p_AD=p_AD+gpsptr%b_tkges(k-1)*&
          (bk5(k-1)/exp(gpsptr%b_pkges(k-1))-bk5(k)/exp(gpsptr%b_pkges(k)))*dz_AD(k)
    end do

!   Interpolation

    do j=1,nsig
      rt(i1(j))=rt(i1(j))+w1_gps*t_AD(j)
      rt(i2(j))=rt(i2(j))+w2_gps*t_AD(j)
      rt(i3(j))=rt(i3(j))+w3_gps*t_AD(j)
      rt(i4(j))=rt(i4(j))+w4_gps*t_AD(j)

      rq(i1(j))=rq(i1(j))+w1_gps*q_AD(j)
      rq(i2(j))=rq(i2(j))+w2_gps*q_AD(j)
      rq(i3(j))=rq(i3(j))+w3_gps*q_AD(j)
      rq(i4(j))=rq(i4(j))+w4_gps*q_AD(j)
    enddo

    rp(i1(1))=rp(i1(1))+w1_gps*p_AD
    rp(i2(1))=rp(i2(1))+w2_gps*p_AD
    rp(i3(1))=rp(i3(1))+w3_gps*p_AD
    rp(i4(1))=rp(i4(1))+w4_gps*p_AD

    gpsptr => gpsptr%llpoint

  end do

  return
end subroutine intbend


subroutine intbend_tl(rt,rq,rp,st,sq,sp,rt_tl,rq_tl,rp_tl,st_tl,sq_tl,sp_tl)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    intbend_tl     the tangent linear of the operator that applies 
!                               nonlinqc obs operator local bending angle
!   prgmmr: cucurull    org: JCSDA/NCEP          date: 2005-12-02
!
! abstract: the tangent linear of the operator that applies gps local 
!           bending angle operator and adjoint with addition of nonlinear qc.
!
! program history log:
!   2005-12-02  cucurull - tangent linear of intbend
!   2006-03-07  todling  - bug fix: nsig_up was declared as real
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
!     rt       - output vector after inclusion of gps bending angle
!     rq       - output q vector after inclusion of gps bending angle
!     rp       - output p vector after inclusion of gps bending angle
!     rt_tl     - output tangent linear vector after inclusion of gps bending angle
!     rq_tl     - output tangent linear q vector after inclusion of gps bending angle
!     rp_tl     - output tangent linear p vector after inclusion of gps bending angle
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
  use gridmod, only: latlon1n,latlon11,nsig,bk5
  use lagmod
  use constants, only: zero,one,two,n_a,n_b,grav,rd,half,tiny_r_kind,cg_term
  implicit none

! Declare local parameters
  real(r_kind),parameter:: ten = 10.0_r_kind
  real(r_kind),parameter:: ds=5000.0_r_kind
  real(r_kind),parameter:: r1em6=1.0e-6_r_kind

! Declare passed variables
  real(r_kind),dimension(latlon1n),intent(in):: st,sq
  real(r_kind),dimension(latlon1n),intent(in):: st_tl,sq_tl
  real(r_kind),dimension(latlon1n),intent(inout):: rt,rq
  real(r_kind),dimension(latlon1n),intent(inout):: rt_tl,rq_tl
  real(r_kind),dimension(latlon11),intent(in):: sp,sp_tl
  real(r_kind),dimension(latlon11),intent(inout):: rp,rp_tl


! Declare local variables
  integer(i_kind) i,j,k,ihob,nsig_up
  integer(i_kind),dimension(nsig):: i1,i2,i3,i4
! real(r_kind) penalty
  real(r_kind) w1_gps,w2_gps,w3_gps,w4_gps
  real(r_kind) :: p_TL,p_AD
  real(r_kind),dimension(nsig) :: q_TL,t_TL,q_AD,t_AD
  real(r_kind) :: p_TL_tl,p_AD_tl
  real(r_kind),dimension(nsig) :: q_TL_tl,t_TL_tl,q_AD_tl,t_AD_tl
  real(r_kind) :: val
  real(r_kind) :: val_tl
  real(r_kind) cg_gps,grad,p0,wnotgross,wgross,term
  real(r_kind) grad_tl,p0_tl,term_tl

  real(r_kind) ddbend,dz_TL,dz_TL_tl,rdog,rsig,h,hob_s,d_ref_rad_TL,d_ref_rad
  real(r_kind) tmean_TL,qmean_TL,tmean_AD,qmean_AD
  real(r_kind) tmean_TL_tl,qmean_TL_tl,tmean_AD_tl,qmean_AD_tl
  real(r_kind) dbend_AD,ddbend_AD,d_ref_rad_AD
  real(r_kind),dimension(nsig) :: irefges,height_TL,height_AD,dz_AD,h_TL,h_AD
  real(r_kind),dimension(3,nsig+10) :: q_w,q_w_TL,q_w_AD
  real(r_kind),dimension(4) :: dw4_TL,w4,dw4,w4_TL,w4_AD,dw4_AD
  real(r_kind),dimension(grids_dim) :: dbeta_TL,dbeta_AD
  real(r_kind),dimension(nsig+10) :: n_TL,n_AD
  real(r_kind),dimension(0:nsig+11) ::  ref_rad,xi_TL,xi_AD

  real(r_kind) d_ref_rad_TL_tl
  real(r_kind) dbend_AD_tl,ddbend_AD_tl,d_ref_rad_AD_tl
  real(r_kind),dimension(nsig) :: height_TL_tl,height_AD_tl,h_TL_tl,h_AD_tl
  real(r_kind),dimension(nsig) :: dz_AD_tl
  real(r_kind),dimension(3,nsig+10) :: q_w_TL_tl,q_w_AD_tl
  real(r_kind),dimension(4) :: dw4_TL_tl,w4_TL_tl,w4_AD_tl,dw4_AD_tl
  real(r_kind),dimension(grids_dim) :: dbeta_TL_tl,dbeta_AD_tl
  real(r_kind),dimension(nsig+10) :: n_TL_tl,n_AD_tl
  real(r_kind),dimension(0:nsig+11) ::  xi_TL_tl,xi_AD_tl


! Loop over observations
  gpsptr => gpshead
  do while (associated(gpsptr))

! Load location information into local variables
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

! increments

  p_TL=w1_gps*sp(i1(1))+w2_gps*sp(i2(1))+w3_gps*sp(i3(1))&
       +w4_gps*sp(i4(1))
  p_TL_tl=w1_gps*sp_tl(i1(1))+w2_gps*sp_tl(i2(1))+w3_gps*sp_tl(i3(1))&
       +w4_gps*sp_tl(i4(1))

  do j=1,nsig
   t_TL(j)=w1_gps*st(i1(j))+w2_gps*st(i2(j))+w3_gps*st(i3(j))&
            +w4_gps*st(i4(j))
   q_TL(j)=w1_gps*sq(i1(j))+w2_gps*sq(i2(j))+w3_gps*sq(i3(j))&
           +w4_gps*sq(i4(j))
   t_TL_tl(j)=w1_gps*st_tl(i1(j))+w2_gps*st_tl(i2(j))+w3_gps*st_tl(i3(j))&
            +w4_gps*st_tl(i4(j))
   q_TL_tl(j)=w1_gps*sq_tl(i1(j))+w2_gps*sq_tl(i2(j))+w3_gps*sq_tl(i3(j))&
           +w4_gps*sq_tl(i4(j))
  enddo

! Initialize some arrays
  n_TL=zero;xi_TL=zero
  d_ref_rad_TL=zero; dbeta_TL=zero
  q_w_TL=zero;w4_TL=zero;dw4_TL=zero ! Lagrange weights

  n_TL_tl=zero;xi_TL_tl=zero
  d_ref_rad_TL_tl=zero;dbeta_TL_tl=zero
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
      dz_TL_tl = t_TL_tl(k-1) * (gpsptr%b_pkges(k-1)-gpsptr%b_pkges(k)) +&
           gpsptr%b_tkges(k-1)*&
           (bk5(k-1)/exp(gpsptr%b_pkges(k-1))-bk5(k)/exp(gpsptr%b_pkges(k)))*p_TL_tl
      height_TL_tl(k) = height_TL_tl(k-1) + rdog * dz_TL_tl
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
   ref_rad(k)=irefges(k)*gpsptr%b_rges(k)               ! refractivity index-radius product
   n_TL(k)=gpsptr%b_tin(k)*tmean_TL+gpsptr%b_qin(k)*qmean_TL+ &
           gpsptr%b_pin(k)*p_TL
   xi_TL(k)=r1em6*gpsptr%b_rges(k)*n_TL(k)+ &
            gpsptr%b_gp2gm(k)*h_TL(k)*irefges(k)
   n_TL_tl(k)=gpsptr%b_tin(k)*tmean_TL_tl+gpsptr%b_qin(k)*qmean_TL_tl+ &
           gpsptr%b_pin(k)*p_TL_tl
   xi_TL(k)=r1em6*gpsptr%b_rges(k)*n_TL_tl(k)+ &
            gpsptr%b_gp2gm(k)*h_TL_tl(k)*irefges(k)
  end do

! Extending atmosphere above nsig
  d_ref_rad=ref_rad(nsig)-ref_rad(nsig-1)
  d_ref_rad_TL=xi_TL(nsig)-xi_TL(nsig-1)
  d_ref_rad_TL_tl=xi_TL_tl(nsig)-xi_TL_tl(nsig-1)
  do k=1,10
    ref_rad(nsig+k)=ref_rad(nsig)+ k*d_ref_rad
    xi_TL(nsig+k)=xi_TL(nsig)+ k*d_ref_rad_TL
    n_TL(nsig+k)=(two*gpsptr%b_n(nsig+k-1)*n_TL(nsig+k-1)/gpsptr%b_n(nsig+k-2))-&
             (gpsptr%b_n(nsig+k-1)**2/gpsptr%b_n(nsig+k-2)**2)*n_TL(nsig+k-2)
    xi_TL_tl(nsig+k)=xi_TL_tl(nsig)+ k*d_ref_rad_TL_tl
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

  val=ds*dbeta_TL(1)
  do j=2,grids_dim
     ddbend=ds*dbeta_TL(j)
     val=val+two*ddbend
  end do
  val=-gpsptr%b_imp*val
  val=val-gpsptr%res

  val_tl=ds*dbeta_TL_tl(1)
  do j=2,grids_dim
     ddbend=ds*dbeta_TL_tl(j)
     val_tl=val_tl+two*ddbend
  end do
  val_tl=-gpsptr%b_imp*val_tl
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

! Initialize (profile dependendant) adjoint variables
  n_AD=zero;q_w_AD=zero;xi_AD=zero
  h_AD=zero
  q_AD=zero;p_AD=zero;t_AD=zero ! model terms

  n_AD_tl=zero;q_w_AD_tl=zero;xi_AD_tl=zero
  h_AD_tl=zero
  q_AD_tl=zero;p_AD_tl=zero;t_AD_tl=zero ! model terms

! initialize obs dependent adjoint variables
  dbend_AD=zero;ddbend_AD=zero;dbeta_AD=zero
  dbend_AD_tl=zero;ddbend_AD_tl=zero;dbeta_AD_tl=zero

  dbend_AD=dbend_AD-gpsptr%b_imp*grad
  dbend_AD_tl=dbend_AD_tl-gpsptr%b_imp*grad_tl
  do j=2,grids_dim  ! no reverse order needed
     ddbend_AD=2*dbend_AD
     dbeta_AD(j)=dbeta_AD(j)+ds*ddbend_AD
     ddbend_AD_tl=2*dbend_AD_tl
     dbeta_AD_tl(j)=dbeta_AD_tl(j)+ds*ddbend_AD_tl
  enddo
  dbeta_AD(1)=dbeta_AD(1)+ds*dbend_AD
  dbeta_AD_tl(1)=dbeta_AD_tl(1)+ds*dbend_AD_tl

  do j=grids_dim,1,-1
    w4_AD=zero;dw4_AD=zero
    w4_AD_tl=zero;dw4_AD_tl=zero
    hob_s=gpsptr%b_loc(j)
    ihob=hob_s
!
    call slagdw(ref_rad(ihob-1:ihob+2),gpsptr%b_xj(j),&
                   q_w(:,ihob),q_w(:,ihob+1),&
                   w4,dw4,4)
    if(ihob==1) then
       dw4(4)=dw4(4)+dw4(1);dw4(1:3)=dw4(2:4);dw4(4)=zero
       ihob=ihob+1
    endif
    if(ihob==nsig_up-1) then
       dw4(1)=dw4(1)+dw4(4); dw4(2:4)=dw4(1:3);dw4(1)=zero
       ihob=ihob-1
    endif

    dw4_AD(1)=dw4_AD(1)+&
                (r1em6/gpsptr%b_xj(j))*dbeta_AD(j)*gpsptr%b_n(ihob-1)
    dw4_AD(2)=dw4_AD(2)+&
                (r1em6/gpsptr%b_xj(j))*dbeta_AD(j)*gpsptr%b_n(ihob)
    dw4_AD(3)=dw4_AD(3)+&
                (r1em6/gpsptr%b_xj(j))*dbeta_AD(j)*gpsptr%b_n(ihob+1)
    dw4_AD(4)=dw4_AD(4)+&
                (r1em6/gpsptr%b_xj(j))*dbeta_AD(j)*gpsptr%b_n(ihob+2)
    dw4_AD_tl(1)=dw4_AD_tl(1)+&
                (r1em6/gpsptr%b_xj(j))*dbeta_AD_tl(j)*gpsptr%b_n(ihob-1)
    dw4_AD_tl(2)=dw4_AD_tl(2)+&
                (r1em6/gpsptr%b_xj(j))*dbeta_AD_tl(j)*gpsptr%b_n(ihob)
    dw4_AD_tl(3)=dw4_AD_tl(3)+&
                (r1em6/gpsptr%b_xj(j))*dbeta_AD_tl(j)*gpsptr%b_n(ihob+1)
    dw4_AD_tl(4)=dw4_AD_tl(4)+&
                (r1em6/gpsptr%b_xj(j))*dbeta_AD_tl(j)*gpsptr%b_n(ihob+2)

    n_AD(ihob-1)=n_AD(ihob-1)+&
                      (r1em6/gpsptr%b_xj(j))*dw4(1)*dbeta_AD(j)
    n_AD(ihob)  =n_AD(ihob)  +&
                      (r1em6/gpsptr%b_xj(j))*dw4(2)*dbeta_AD(j)
    n_AD(ihob+1)=n_AD(ihob+1)+&
                      (r1em6/gpsptr%b_xj(j))*dw4(3)*dbeta_AD(j)
    n_AD(ihob+2)=n_AD(ihob+2)+&
                      (r1em6/gpsptr%b_xj(j))*dw4(4)*dbeta_AD(j)
    n_AD_tl(ihob-1)=n_AD_tl(ihob-1)+&
                      (r1em6/gpsptr%b_xj(j))*dw4(1)*dbeta_AD_tl(j)
    n_AD_tl(ihob)  =n_AD_tl(ihob)  +&
                      (r1em6/gpsptr%b_xj(j))*dw4(2)*dbeta_AD_tl(j)
    n_AD_tl(ihob+1)=n_AD_tl(ihob+1)+&
                      (r1em6/gpsptr%b_xj(j))*dw4(3)*dbeta_AD_tl(j)
    n_AD_tl(ihob+2)=n_AD_tl(ihob+2)+&
                      (r1em6/gpsptr%b_xj(j))*dw4(4)*dbeta_AD_tl(j)
!
   if(int(hob_s)==nsig_up-1) then
    ihob=ihob+1
    dw4_AD(1:3)=dw4_AD(2:4)
    dw4_AD(4)=dw4_AD(1)
    dw4_AD_tl(1:3)=dw4_AD_tl(2:4)
    dw4_AD_tl(4)=dw4_AD_tl(1)
   endif
   if(int(hob_s)==1) then
    ihob=ihob-1
    dw4_AD(2:4)=dw4_AD(1:3)
    dw4_AD(1)=dw4_AD(4)
    dw4_AD_tl(2:4)=dw4_AD_tl(1:3)
    dw4_AD_tl(1)=dw4_AD_tl(4)
   endif

! Compute refractivity and derivative at target points using Lagrange interpolators
    call slagdw_AD(ref_rad(ihob-1:ihob+2),xi_AD(ihob-1:ihob+2),&
         gpsptr%b_xj(j),&
         q_w(:,ihob),q_w_AD(:,ihob),&
         q_w(:,ihob+1),q_w_AD(:,ihob+1),&
         w4_AD,dw4,dw4_AD,4)
    call slagdw_AD(ref_rad(ihob-1:ihob+2),xi_AD_tl(ihob-1:ihob+2),&
         gpsptr%b_xj(j),&
         q_w(:,ihob),q_w_AD_tl(:,ihob),&
         q_w(:,ihob+1),q_w_AD_tl(:,ihob+1),&
         w4_AD_tl,dw4,dw4_AD_tl,4)
  end do ! grids dim
! END DO LOOP OVER OBS - each obs will have contributed to its profile

 ! Lagrange coefficients
  do k=nsig_up,1,-1
    call setq_AD(q_w_AD(:,k),ref_rad(k-1:k+1),xi_AD(k-1:k+1),3)
    call setq_AD(q_w_AD_tl(:,k),ref_rad(k-1:k+1),xi_AD_tl(k-1:k+1),3)
  enddo
  xi_AD(nsig_up-2)=xi_AD(nsig_up-2)+xi_AD(nsig_up+1)
  xi_AD(3)=xi_AD(3)+xi_AD(0)
  xi_AD_tl(nsig_up-2)=xi_AD_tl(nsig_up-2)+xi_AD_tl(nsig_up+1)
  xi_AD_tl(3)=xi_AD_tl(3)+xi_AD_tl(0)

! Extending atmosphere above nsig
  d_ref_rad_AD=zero
  d_ref_rad_AD_tl=zero
  do k=10,1,-1
      n_AD(nsig+k-1)=n_AD(nsig+k-1)+&
                     n_AD(nsig+k)*two*gpsptr%b_n(nsig+k-1)/gpsptr%b_n(nsig+k-2)
      n_AD(nsig+k-2)=n_AD(nsig+k-2)-&
                     (gpsptr%b_n(nsig+k-1)**2/gpsptr%b_n(nsig+k-2)**2)*n_AD(nsig+k)
      xi_AD(nsig)=xi_AD(nsig)+xi_AD(nsig+k)
      d_ref_rad_AD=d_ref_rad_AD+k*xi_AD(nsig+k)

      n_AD_tl(nsig+k-1)=n_AD_tl(nsig+k-1)+&
                     n_AD_tl(nsig+k)*two*gpsptr%b_n(nsig+k-1)/gpsptr%b_n(nsig+k-2)
      n_AD_tl(nsig+k-2)=n_AD_tl(nsig+k-2)-&
                     (gpsptr%b_n(nsig+k-1)**2/gpsptr%b_n(nsig+k-2)**2)*n_AD_tl(nsig+k)
      xi_AD_tl(nsig)=xi_AD_tl(nsig)+xi_AD_tl(nsig+k)
      d_ref_rad_AD_tl=d_ref_rad_AD_tl+k*xi_AD_tl(nsig+k)
  end do
  xi_AD(nsig)=xi_AD(nsig)+d_ref_rad_AD
  xi_AD(nsig-1)=xi_AD(nsig-1)-d_ref_rad_AD
  xi_AD_tl(nsig)=xi_AD_tl(nsig)+d_ref_rad_AD_tl
  xi_AD_tl(nsig-1)=xi_AD_tl(nsig-1)-d_ref_rad_AD_tl

! Increment of refractivity and index of refractivity - radius product
  do k=nsig,1,-1
     n_AD(k)=n_AD(k)+r1em6*gpsptr%b_rges(k)*xi_AD(k)
     h_AD(k)=h_AD(k)+gpsptr%b_gp2gm(k)*irefges(k)*xi_AD(k)
     tmean_AD=gpsptr%b_tin(k)*n_AD(k)
     qmean_AD=gpsptr%b_qin(k)*n_AD(k)
     p_AD=p_AD+gpsptr%b_pin(k)*n_AD(k)
     n_AD_tl(k)=n_AD_tl(k)+r1em6*gpsptr%b_rges(k)*xi_AD_tl(k)
     h_AD_tl(k)=h_AD_tl(k)+gpsptr%b_gp2gm(k)*irefges(k)*xi_AD_tl(k)
     tmean_AD_tl=gpsptr%b_tin(k)*n_AD_tl(k)
     qmean_AD_tl=gpsptr%b_qin(k)*n_AD_tl(k)
     p_AD_tl=p_AD_tl+gpsptr%b_pin(k)*n_AD_tl(k)
     if(k>1) then
        q_AD(k)=q_AD(k)+qmean_AD/two
        t_AD(k)=t_AD(k)+tmean_AD/two
        q_AD(k-1)=q_AD(k-1)+qmean_AD/two
        t_AD(k-1)=t_AD(k-1)+tmean_AD/two
        q_AD_tl(k)=q_AD_tl(k)+qmean_AD_tl/two
        t_AD_tl(k)=t_AD_tl(k)+tmean_AD_tl/two
        q_AD_tl(k-1)=q_AD_tl(k-1)+qmean_AD_tl/two
        t_AD_tl(k-1)=t_AD_tl(k-1)+tmean_AD_tl/two
     endif
     if(k==1) then
        q_AD(1)=q_AD(1)+qmean_AD
        t_AD(1)=t_AD(1)+tmean_AD
        q_AD_tl(1)=q_AD_tl(1)+qmean_AD_tl
        t_AD_tl(1)=t_AD_tl(1)+tmean_AD_tl
     endif
  end do


! geopotential heights
  height_AD=zero;dz_AD=zero
  height_AD_tl=zero;dz_AD_tl=zero
  do k=1,nsig
     height_AD(k)=height_AD(k)+h_AD(k)
     height_AD_tl(k)=height_AD_tl(k)+h_AD_tl(k)
  end do
! rest of levels
  do k=nsig,2,-1
     height_AD(k-1)=height_AD(k-1)+height_AD(k)
     dz_AD(k)=dz_AD(k)+ rdog * height_AD(k)
     t_AD(k-1)=t_AD(k-1)+((gpsptr%b_pkges(k-1)-gpsptr%b_pkges(k))*dz_AD(k))
     p_AD=p_AD+gpsptr%b_tkges(k-1)*&
          (bk5(k-1)/exp(gpsptr%b_pkges(k-1))-bk5(k)/exp(gpsptr%b_pkges(k)))*dz_AD(k)
     height_AD_tl(k-1)=height_AD_tl(k-1)+height_AD_tl(k)
     dz_AD_tl(k)=dz_AD_tl(k)+ rdog * height_AD_tl(k)
     t_AD_tl(k-1)=t_AD_tl(k-1)+((gpsptr%b_pkges(k-1)-gpsptr%b_pkges(k))*dz_AD_tl(k))
     p_AD_tl=p_AD_tl+gpsptr%b_tkges(k-1)*&
            (bk5(k-1)/exp(gpsptr%b_pkges(k-1))-bk5(k)/exp(gpsptr%b_pkges(k)))*dz_AD_tl(k)
  end do

! Interpolation

  do j=1,nsig
  rt(i1(j))=rt(i1(j))+w1_gps*t_AD(j)
  rt(i2(j))=rt(i2(j))+w2_gps*t_AD(j)
  rt(i3(j))=rt(i3(j))+w3_gps*t_AD(j)
  rt(i4(j))=rt(i4(j))+w4_gps*t_AD(j)

  rq(i1(j))=rq(i1(j))+w1_gps*q_AD(j)
  rq(i2(j))=rq(i2(j))+w2_gps*q_AD(j)
  rq(i3(j))=rq(i3(j))+w3_gps*q_AD(j)
  rq(i4(j))=rq(i4(j))+w4_gps*q_AD(j)

  rt_tl(i1(j))=rt_tl(i1(j))+w1_gps*t_AD_tl(j)
  rt_tl(i2(j))=rt_tl(i2(j))+w2_gps*t_AD_tl(j)
  rt_tl(i3(j))=rt_tl(i3(j))+w3_gps*t_AD_tl(j)
  rt_tl(i4(j))=rt_tl(i4(j))+w4_gps*t_AD_tl(j)

  rq_tl(i1(j))=rq_tl(i1(j))+w1_gps*q_AD_tl(j)
  rq_tl(i2(j))=rq_tl(i2(j))+w2_gps*q_AD_tl(j)
  rq_tl(i3(j))=rq_tl(i3(j))+w3_gps*q_AD_tl(j)
  rq_tl(i4(j))=rq_tl(i4(j))+w4_gps*q_AD_tl(j)
  enddo

  rp(i1(1))=rp(i1(1))+w1_gps*p_AD
  rp(i2(1))=rp(i2(1))+w2_gps*p_AD
  rp(i3(1))=rp(i3(1))+w3_gps*p_AD
  rp(i4(1))=rp(i4(1))+w4_gps*p_AD

  rp_tl(i1(1))=rp_tl(i1(1))+w1_gps*p_AD_tl
  rp_tl(i2(1))=rp_tl(i2(1))+w2_gps*p_AD_tl
  rp_tl(i3(1))=rp_tl(i3(1))+w3_gps*p_AD_tl
  rp_tl(i4(1))=rp_tl(i4(1))+w4_gps*p_AD_tl

  gpsptr => gpsptr%llpoint

  end do

  return
end subroutine intbend_tl

end module intbendmod
