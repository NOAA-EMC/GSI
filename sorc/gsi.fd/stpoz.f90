module stpozmod

!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    stpozmod    module for stpoz and its tangent linear stpoz_tl
!
! abstract: module for stpoz and its tangent linear stpoz_tl
!
! program history log:
!   2005-05-17  Yanqiu zhu - wrap stpoz and its tangent linear stpoz_tl into one module
!   2005-11-16  Derber - remove interfaces
!

implicit none

PRIVATE
PUBLIC stpoz,stpoz_tl

contains

subroutine stpoz(roz,soz,pen,b1,b3,sges1,sges2,sges3)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    stpoz       compute contribution to penalty and
!                            stepsize for ozone, using nonlinear qc
!   prgmmr: derber          org: np23                 date: 1995-07-11
!
! abstract: The routine computes the contribution to the penalty from ozone
!           observations.  The routine also computes the contribution of
!           ozone observations to the step size.  This version includes
!           nonlinear qc.
!
! program history log:
!   1995-07-11  derber
!   1999-03-01  wu - port cray90 code to ibm-sp (mpi version)
!   2004-06-16  treadon - update documenation
!   2004-07-29  treadon - add only to module use, add intent in/out
!   2004-10-07  parrish - add nonlinear qc option
!   2005-04-11  treadon - merge stpoz and stpoz_qc into single routine
!   2005-06-14  wu      - add OMI toz
!   2005-09-28  derber  - consolidate location and weight arrays
!   2006-07-28  derber  - modify to use new inner loop obs data structure
!                       - unify NL qc
!   2006-09-18  derber  - modify output values of b1 and b3
!
!   input argument list:
!     roz  - search direction for ozone
!     soz  - input ozone correction field
!     sges1    - estimate step size 1
!     sges2    - estimate step size 2
!     sges3    - estimate step size 3
!
!   output argument list:
!     pen  - contribution of ozone data to penalty
!     b1   - contribution to numerator from ozone
!     b3   - contribution to denomonator from ozone
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,i_kind
  use obsmod, only: ozhead,ozptr,nloz,ozohead,ozoptr
  use ozinfo, only: b_oz,pg_oz
  use qcmod, only: nlnqc_iter
  use constants, only: zero,one,half,two,tiny_r_kind,cg_term
  use gridmod, only: lat2,lon2,nsig
  implicit none

! Declare passed variables
  real(r_kind),intent(out):: pen,b1,b3
  real(r_kind),dimension(lat2*lon2,nsig),intent(in):: roz,soz
  real(r_kind),intent(in):: sges1,sges2,sges3

! Declare local variables
  integer(i_kind) i,k,j1,j2,j3,j4,kk,iz1,iz2,kx
  real(r_kind) dz1,pob,val1,valx,delz
  real(r_kind) w1,w2,w3,w4
  real(r_kind) val
  real(r_kind) cg_oz,oz1,oz2,oz3,pen1,pen2,pen3,pencur,wgross,wnotgross
  real(r_kind) alpha,ccoef,bcoef1,bcoef2,cc

! Initialize output variables to zero
  pen=zero
  b1=zero; b3=zero
  alpha=one/(sges2-sges1)
  ccoef=half*alpha*alpha
  bcoef1=half*half*alpha
  bcoef2=sges2*ccoef

! SBUV OZONE: LAYER O3 and TOATL O3
!
! Loop over ozone observations
  ozptr => ozhead
  do while (associated(ozptr))
    if(ozptr%luse)then

!    Get location
     j1=ozptr%ij(1)
     j2=ozptr%ij(2)
     j3=ozptr%ij(3)
     j4=ozptr%ij(4)
     w1=ozptr%wij(1)
     w2=ozptr%wij(2)
     w3=ozptr%wij(3)
     w4=ozptr%wij(4)

!    Accumulate contribution from layer observations
     dz1=nsig+1
     do k=1,nloz
        val1= -ozptr%res(k)
        val = zero
        pob = ozptr%prs(k)
        iz1 = dz1
        if (iz1>nsig) iz1=nsig
        iz2 = pob
        
        do kk=iz1,iz2,-1
           delz=one
           if (kk==iz1) delz=dz1-iz1
           if (kk==iz2) delz=delz-pob+iz2
           val=val + ( &
                w1*roz(j1,kk) + &
                w2*roz(j2,kk)+ &
                w3*roz(j3,kk)+ &
                w4*roz(j4,kk) )*delz
           val1=val1 + ( &
                w1*soz(j1,kk) + &
                w2*soz(j2,kk)+ &
                w3*soz(j3,kk)+ &
                w4*soz(j4,kk) )*delz
        end do
        oz1=val1+sges1*val
        oz2=val1+sges2*val
        oz3=val1+sges3*val

        pencur = ozptr%err2(k)*val1*val1
        pen1   = ozptr%err2(k)*oz1*oz1
        pen2   = ozptr%err2(k)*oz2*oz2
        pen3   = ozptr%err2(k)*oz3*oz3

        kx = ozptr%ipos(k)
!  Modify penalty term if nonlinear QC
        if (nlnqc_iter .and. pg_oz(kx) > tiny_r_kind .and. &
                             b_oz(kx)  > tiny_r_kind) then
           cg_oz=cg_term/b_oz(kx)
           wnotgross= one-pg_oz(kx)
           wgross = pg_oz(kx)*cg_oz/wnotgross
           pencur = -two*log((exp(-half*pencur) + wgross)/(one+wgross))
           pen1   = -two*log((exp(-half*pen1  ) + wgross)/(one+wgross))
           pen2   = -two*log((exp(-half*pen2  ) + wgross)/(one+wgross))
           pen3   = -two*log((exp(-half*pen3  ) + wgross)/(one+wgross))
        endif
        
        pen = pen+pencur*ozptr%raterr2(k)
        cc  = (pen1+pen3-two*pen2)*ozptr%raterr2(k)
        b1  = b1+(pen1-pen3)*ozptr%raterr2(k)*bcoef1+cc*bcoef2
        b3  = b3+cc*ccoef
        dz1=pob
     end do

!    Add contribution from total column observation
     k   = nloz+1
     val1= -ozptr%res(k)
     val  = zero
     do kk=nsig,1,-1
        val=val+ ( &
             w1*roz(j1,kk) + &
             w2*roz(j2,kk)+ &
             w3*roz(j3,kk)+ &
             w4*roz(j4,kk) )
        val1=val1 + ( &
             w1*soz(j1,kk) + &
             w2*soz(j2,kk)+ &
             w3*soz(j3,kk)+ & 
             w4*soz(j4,kk) )
     enddo
     oz1=val1+sges1*val
     oz2=val1+sges2*val
     oz3=val1+sges3*val

     pencur = ozptr%err2(k)*val1*val1
     pen1   = ozptr%err2(k)*oz1*oz1
     pen2   = ozptr%err2(k)*oz2*oz2
     pen3   = ozptr%err2(k)*oz3*oz3

     kx = ozptr%ipos(k)
!  Modify penalty term if nonlinear QC
     if (nlnqc_iter .and. pg_oz(kx) > tiny_r_kind) then
        cg_oz=cg_term/b_oz(kx)
        wnotgross= one-pg_oz(kx)
        wgross = pg_oz(kx)*cg_oz
        pencur = -two*log(wnotgross*exp(-half*pencur)  + wgross)
        pen1   = -two*log(wnotgross*exp(-half*pen1  ) + wgross)
        pen2   = -two*log(wnotgross*exp(-half*pen2  ) + wgross)
        pen3   = -two*log(wnotgross*exp(-half*pen3  ) + wgross)
     endif
     
     pen = pen +pencur*ozptr%raterr2(k)
     cc  = (pen1+pen3-two*pen2)*ozptr%raterr2(k)
     b1  = b1+(pen1-pen3)*ozptr%raterr2(k)*bcoef1+cc*bcoef2
     b3  = b3+cc*ccoef
    end if

    ozptr => ozptr%llpoint

! End of loop over observations
  enddo
!
! OMI TOTAL OZONE
!

! Loop over ozone observations
  ozoptr => ozohead
  do while (associated(ozoptr))
    if(ozoptr%luse)then

!    Get location
     j1=ozoptr%ij(1)
     j2=ozoptr%ij(2)
     j3=ozoptr%ij(3)
     j4=ozoptr%ij(4)
     w1=ozoptr%wij(1)
     w2=ozoptr%wij(2)
     w3=ozoptr%wij(3)
     w4=ozoptr%wij(4)

!    Add contribution from total column observation
     k   = 1
     val1= -ozoptr%res
     val  = zero
     do kk=nsig,1,-1
        val=val+ ( &
             w1*roz(j1,kk)+ &
             w2*roz(j2,kk)+ &
             w3*roz(j3,kk)+ &
             w4*roz(j4,kk) )
        val1=val1 + ( &
             w1*soz(j1,kk)+ &
             w2*soz(j2,kk)+ &
             w3*soz(j3,kk)+ &
             w4*soz(j4,kk) )
     enddo
     oz1=val1+sges1*val
     oz2=val1+sges2*val
     oz3=val1+sges3*val

     pencur = val1*val1*ozoptr%err2
     pen1   = oz1*oz1*ozoptr%err2
     pen2   = oz2*oz2*ozoptr%err2
     pen3   = oz3*oz3*ozoptr%err2

     kx=ozoptr%ipos
!  Modify penalty term if nonlinear QC
     if (nlnqc_iter .and. pg_oz(kx) > tiny_r_kind) then
        cg_oz=cg_term/b_oz(kx)
        wnotgross= one-pg_oz(kx)
        wgross = pg_oz(kx)*cg_oz
        pencur = -two*log(wnotgross*exp(-half*pencur) + wgross)
        pen1   = -two*log(wnotgross*exp(-half*pen1  ) + wgross)
        pen2   = -two*log(wnotgross*exp(-half*pen2  ) + wgross)
        pen3   = -two*log(wnotgross*exp(-half*pen3  ) + wgross)
     endif

     pen = pen+pencur*ozoptr%raterr2
     cc  = (pen1+pen3-two*pen2)*ozoptr%raterr2
     b1  = b1+(pen1-pen3)*ozoptr%raterr2*bcoef1+cc*bcoef2
     b3  = b3+cc*ccoef
    end if

    ozoptr => ozoptr%llpoint

! End of loop over observations
  enddo



! End of routine.
 return
end subroutine stpoz


subroutine stpoz_tl(roz,soz,pen,b1,b3,sges1,sges2,sges3, &
                   roz_tl,soz_tl,pen_tl,b1_tl,b3_tl,sges1_tl,sges2_tl,sges3_tl)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    stpoz_tl   the tangent linear of the operator that computes 
!                          contribution to penalty and stepsize for ozone, using nonlinear qc
!   prgmmr: yanqiu zhu          org: GMAO                 date: 2005-05-17
!
! abstract: The routine is the tangent linear of the operator that computes the 
!           contribution to the penalty from ozone observations.  The routine also 
!           computes the contribution of ozone observations to the step size.  This 
!           version includes nonlinear qc.
!
! program history log:
!   2005-05-17  yanqiu zhu - tangent linear of stpoz
!   2005-06-14  wu      - add OMI toz
!
!   input argument list:
!     roz  - search direction for ozone
!     soz  - input ozone correction field
!     sges1    - estimate step size 1
!     sges2    - estimate step size 2
!     sges3    - estimate step size 3
!     roz_tl  - tangent linear search direction for ozone
!     soz_tl  - input tangent linear ozone correction field
!     sges1_tl    - tangent linear estimate step size 1
!     sges2_tl    - tangent linear estimate step size 2
!     sges3_tl    - tangent linear estimate step size 3
!
!   output argument list:
!     pen  - contribution of ozone data to penalty
!     b1       - pen(sges1)-pen(sges2)
!     b3       - pen(sges3)-pen(sges2)
!     pen_tl  - tangent linear of the contribution of ozone data to penalty
!     b1_tl       - pen_tl(sges1)-pen_tl(sges2)
!     b3_tl       - pen_tl(sges3)-pen_tl(sges2)
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,i_kind
  use obsmod, only: ozhead,ozptr,nloz,ozohead,ozoptr
  use obsmod_tl, only: oz_inv_tl,ozo_inv_tl
  use ozinfo, only: b_oz,pg_oz
  use qcmod, only: nlnqc_iter
  use constants, only: zero,one,half,two,tiny_r_kind,cg_term
  use gridmod, only: lat2,lon2,nsig
  implicit none

! Declare passed variables
  real(r_kind),intent(out):: pen,b1,b3
  real(r_kind),intent(out):: pen_tl,b1_tl,b3_tl
  real(r_kind),dimension(lat2*lon2,nsig),intent(in):: roz,soz
  real(r_kind),dimension(lat2*lon2,nsig),intent(in):: roz_tl,soz_tl
  real(r_kind),intent(in):: sges1,sges2,sges3
  real(r_kind),intent(in):: sges1_tl,sges2_tl,sges3_tl

! Declare local variables
  integer(i_kind) i,k,j1,j2,j3,j4,kk,iz1,iz2,kx
  real(r_kind) w1,w2,w3,w4
  real(r_kind) dz1,pob,val1,valx,delz
  real(r_kind) val1_tl,valx_tl
  real(r_kind) val
  real(r_kind) val_tl
  real(r_kind) cg_oz,oz1,oz2,oz3,pen1,pen2,pen3,pencur,wgross,wnotgross
  real(r_kind) oz1_tl,oz2_tl,oz3_tl,pen1_tl,pen2_tl,pen3_tl,pencur_tl
  real(r_kind) term,term1,term2,term3,halfvar_oz
  real(r_kind) term_tl,term1_tl,term2_tl,term3_tl
  real(r_kind) exp_arg,exp_arg1,exp_arg2,exp_arg3
  real(r_kind) arg_exp_tl,exp_arg1_tl,exp_arg2_tl,exp_arg3_tl
  real(r_kind) temp

! Initialize output variables to zero
  pen=zero
  b1=zero; b3=zero
  pen_tl=zero
  b1_tl=zero; b3_tl=zero


! SBUV OZONE: LAYER O3 and TOATL O3
!
! Loop over ozone observations
  ozptr => ozhead
  i=0
  do while (associated(ozptr))
    i=i+1
    if(ozptr%luse)then

!    Get location
     j1=ozptr%ij(1)
     j2=ozptr%ij(2)
     j3=ozptr%ij(3)
     j4=ozptr%ij(4)
     w1=ozptr%wij(1)
     w2=ozptr%wij(2)
     w3=ozptr%wij(3)
     w4=ozptr%wij(4)

!    Accumulate contribution from layer observations
     dz1=nsig+1
     do k=1,nloz
        val1_tl= -oz_inv_tl(k,i)
        val_tl = zero
        val1= -ozptr%res(k)
        val = zero
        pob = ozptr%prs(k)
        iz1 = dz1
        if (iz1>nsig) iz1=nsig
        iz2 = pob
        
        do kk=iz1,iz2,-1
           delz=one
           if (kk==iz1) delz=dz1-iz1
           if (kk==iz2) delz=delz-pob+iz2
           val_tl=val_tl + ( &
                w1*roz_tl(j1,kk) + &
                w2*roz_tl(j2,kk)+ &
                w3*roz_tl(j3,kk)+ &
                w4*roz_tl(j4,kk) )*delz
           val1_tl=val1_tl + ( &
                w1*soz_tl(j1,kk) + &
                w2*soz_tl(j2,kk)+ &
                w3*soz_tl(j3,kk)+ &
                w4*soz_tl(j4,kk) )*delz
           val=val + ( &
                w1*roz(j1,kk) + &
                w2*roz(j2,kk)+ &
                w3*roz(j3,kk)+ &
                w4*roz(j4,kk) )*delz
           val1=val1 + ( &
                w1*soz(j1,kk) + &
                w2*soz(j2,kk)+ &
                w3*soz(j3,kk)+ &
                w4*soz(j4,kk) )*delz
        end do
        oz1_tl=val1_tl+sges1_tl*val+sges1*val_tl
        oz2_tl=val1_tl+sges2_tl*val+sges2*val_tl
        oz3_tl=val1_tl+sges3_tl*val+sges3*val_tl
        oz1=val1+sges1*val
        oz2=val1+sges2*val
        oz3=val1+sges3*val

        halfvar_oz = -half*ozptr%err2(k)
        exp_arg  = halfvar_oz*val1*val1
        exp_arg1 = halfvar_oz*oz1*oz1
        exp_arg2 = halfvar_oz*oz2*oz2
        exp_arg3 = halfvar_oz*oz3*oz3
        arg_exp_tl  = -val1*val1_tl*ozptr%err2(k)
        exp_arg1_tl = -oz1*oz1_tl*ozptr%err2(k)
        exp_arg2_tl = -oz2*oz2_tl*ozptr%err2(k)
        exp_arg3_tl = -oz3*oz3_tl*ozptr%err2(k)

        kx = ozptr%ipos(k)
        if (nlnqc_iter .and. pg_oz(kx) > tiny_r_kind) then
           cg_oz=cg_term/b_oz(kx)
           wnotgross= one-pg_oz(kx)
           wgross = pg_oz(kx)*cg_oz
           temp    = wnotgross*exp(exp_arg)
           term_tl  = temp/(temp+wgross)*arg_exp_tl
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
           term_tl  = arg_exp_tl
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
        
        pen_tl = pen_tl -two*pencur_tl*ozptr%raterr2(k)
        b1_tl  = b1_tl-two*(pen1_tl-pen2_tl)*ozptr%raterr2(k)
        b3_tl  = b3_tl-two*(pen3_tl-pen2_tl)*ozptr%raterr2(k)
        pen = pen -two*pencur*ozptr%raterr2(k)
        b1  = b1-two*(pen1-pen2)*ozptr%raterr2(k)
        b3  = b3-two*(pen3-pen2)*ozptr%raterr2(k)
        dz1=pob
     end do

!    Add contribution from total column observation
     k   = nloz+1
     val1_tl = -oz_inv_tl(k,i)
     val_tl  = zero
     val1= -ozptr%res(k)
     val  = zero
     do kk=nsig,1,-1
        val_tl=val_tl+ ( &
             w1*roz_tl(j1,kk) + &
             w2*roz_tl(j2,kk)+ &
             w3*roz_tl(j3,kk)+ &
             w4*roz_tl(j4,kk) )
        val1_tl=val1_tl + ( &
             w1*soz_tl(j1,kk) + &
             w2*soz_tl(j2,kk)+ &
             w3*soz_tl(j3,kk)+ &
             w4*soz_tl(j4,kk) )
        val=val+ ( &
             w1*roz(j1,kk) + &
             w2*roz(j2,kk)+ &
             w3*roz(j3,kk)+ &
             w4*roz(j4,kk) )
        val1=val1 + ( &
             w1*soz(j1,kk) + &
             w2*soz(j2,kk)+ &
             w3*soz(j3,kk)+ & 
             w4*soz(j4,kk) )
     enddo
     oz1_tl=val1_tl+sges1_tl*val+sges1*val_tl
     oz2_tl=val1_tl+sges2_tl*val+sges2*val_tl
     oz3_tl=val1_tl+sges3_tl*val+sges3*val_tl
     oz1=val1+sges1*val
     oz2=val1+sges2*val
     oz3=val1+sges3*val

     halfvar_oz = -half*ozptr%err2(k)
     exp_arg  = halfvar_oz*val1*val1
     exp_arg1 = halfvar_oz*oz1*oz1
     exp_arg2 = halfvar_oz*oz2*oz2
     exp_arg3 = halfvar_oz*oz3*oz3
     arg_exp_tl  = -val1*val1_tl*ozptr%err2(k)
     exp_arg1_tl = -oz1*oz1_tl*ozptr%err2(k)
     exp_arg2_tl = -oz2*oz2_tl*ozptr%err2(k)
     exp_arg3_tl = -oz3*oz3_tl*ozptr%err2(k)

     kx = ozptr%ipos(k)
     if (nlnqc_iter .and. pg_oz(kx) > tiny_r_kind) then
        cg_oz=cg_term/b_oz(kx)
        wnotgross= one-pg_oz(kx)
        wgross = pg_oz(kx)*cg_oz
        temp    = wnotgross*exp(exp_arg)
        term_tl  = temp/(temp+wgross)*arg_exp_tl
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
        term_tl  = arg_exp_tl
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

     pen_tl = pen_tl -two*pencur_tl*ozptr%raterr2(k)
     b1_tl  = b1_tl-two*(pen1_tl-pen2_tl)*ozptr%raterr2(k)
     b3_tl  = b3_tl-two*(pen3_tl-pen2_tl)*ozptr%raterr2(k)
     pen = pen -two*pencur*ozptr%raterr2(k)
     b1  = b1-two*(pen1-pen2)*ozptr%raterr2(k)
     b3  = b3-two*(pen3-pen2)*ozptr%raterr2(k)
    end if

    ozptr => ozptr%llpoint

! End of loop over observations
  enddo

!
! OMI TOTAL OZONE
!
! Loop over ozone observations
  ozoptr => ozohead
  i=0
  do while (associated(ozoptr))
    i=i+1
    if(ozoptr%luse)then

!    Get location
     j1=ozoptr%ij(1)
     j2=ozoptr%ij(2)
     j3=ozoptr%ij(3)
     j4=ozoptr%ij(4)
     w1=ozoptr%wij(1)
     w2=ozoptr%wij(2)
     w3=ozoptr%wij(3)
     w4=ozoptr%wij(4)


!    Add contribution from total column observation
     k   = +1
     val1_tl = -ozo_inv_tl(i)
     val_tl  = zero
     val1= -ozoptr%res
     val  = zero
     do kk=nsig,1,-1
        val_tl=val_tl+ ( &
             w1*roz_tl(j1,kk) + &
             w2*roz_tl(j2,kk)+ &
             w3*roz_tl(j3,kk)+ &
             w4*roz_tl(j4,kk) )
        val1_tl=val1_tl + ( &
             w1*soz_tl(j1,kk) + &
             w2*soz_tl(j2,kk)+ &
             w3*soz_tl(j3,kk)+ &
             w4*soz_tl(j4,kk) )
        val=val+ ( &
             w1*roz(j1,kk) + &
             w2*roz(j2,kk)+ &
             w3*roz(j3,kk)+ &
             w4*roz(j4,kk) )
        val1=val1 + ( &
             w1*soz(j1,kk) + &
             w2*soz(j2,kk)+ &
             w3*soz(j3,kk)+ & 
             w4*soz(j4,kk) )
     enddo
     oz1_tl=val1_tl+sges1_tl*val+sges1*val_tl
     oz2_tl=val1_tl+sges2_tl*val+sges2*val_tl
     oz3_tl=val1_tl+sges3_tl*val+sges3*val_tl
     oz1=val1+sges1*val
     oz2=val1+sges2*val
     oz3=val1+sges3*val

     halfvar_oz = -half*ozoptr%err2
     exp_arg  = halfvar_oz*val1*val1
     exp_arg1 = halfvar_oz*oz1*oz1
     exp_arg2 = halfvar_oz*oz2*oz2
     exp_arg3 = halfvar_oz*oz3*oz3
     arg_exp_tl  = -val1*val1_tl*ozoptr%err2
     exp_arg1_tl = -oz1*oz1_tl*ozoptr%err2
     exp_arg2_tl = -oz2*oz2_tl*ozoptr%err2
     exp_arg3_tl = -oz3*oz3_tl*ozoptr%err2

     kx = ozoptr%ipos
     if (nlnqc_iter .and. pg_oz(kx) > tiny_r_kind) then
        cg_oz=cg_term/b_oz(kx)
        wnotgross= one-pg_oz(kx)
        wgross = pg_oz(kx)*cg_oz
        temp    = wnotgross*exp(exp_arg)
        term_tl  = temp/(temp+wgross)*arg_exp_tl
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
        term_tl  = arg_exp_tl
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

     pen_tl = pen_tl -two*pencur_tl*ozoptr%raterr2
     b1_tl  = b1_tl-two*(pen1_tl-pen2_tl)*ozoptr%raterr2
     b3_tl  = b3_tl-two*(pen3_tl-pen2_tl)*ozoptr%raterr2
     pen = pen -two*pencur*ozoptr%raterr2
     b1  = b1-two*(pen1-pen2)*ozoptr%raterr2
     b3  = b3-two*(pen3-pen2)*ozoptr%raterr2
    end if

    ozoptr => ozoptr%llpoint

! End of loop over observations
  enddo

! End of routine.
 return
end subroutine stpoz_tl

end module stpozmod
