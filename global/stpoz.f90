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
!   2008-12-02  Todling - remove stpoz_tl
!   2009-01-21  Sienkiewicz - add stpo3l (level ozone) again
!

implicit none

PRIVATE
PUBLIC stpoz

contains

subroutine stpoz(ozhead,o3lhead,roz,soz,out,sges)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    stpoz       call components to calculate contrib. to
!                            penalty and stepsize for ozone
!   prgmmr: sienkiewicz     org: GMAO                 date: 2009-01-22
!
! abstract: The routine calls individual components that calculate 
!           contribution to the penalty and step size from layer 
!           and level ozone measurements
!
! program history log:
!   2009-01-22  Sienkiewicz - incorporation of level ozone routine
!
!   input argument list:
!     roz  - search direction for ozone
!     soz  - input ozone correction field
!     sges - step size estimates (4)
!
!   output argument list:
!     out(1) - contribution of ozone data to penalty sges(1)
!     out(2) - contribution of ozone data to penalty sges(2)
!     out(3) - contribution of ozone data to penalty sges(3)
!     out(4) - contribution of ozone data to penalty sges(4)
!     out(5) - contribution to numerator from ozone
!     out(6) - contribution to denomonator from ozone
!
! attributes:
!   language: f90
!   machine:  
  use kinds, only: r_kind,i_kind,r_quad
  use obsmod, only: oz_ob_type,o3l_ob_type
  use gridmod, only: latlon1n
  use constants, only: zero_quad
  use jfunc, only: l_foto,xhat_dt,dhat_dt
  implicit none

! Declare passed variables

  type( oz_ob_type),pointer,intent(in):: ozhead
  type(o3l_ob_type),pointer,intent(in):: o3lhead
  real(r_kind),dimension(latlon1n),intent(in):: soz
  real(r_kind),dimension(latlon1n),intent(in):: roz
  real(r_kind),dimension(4),intent(in):: sges
  real(r_quad),dimension(6),intent(out):: out

  out=zero_quad

  call stpozlay_(ozhead,roz,soz,out,sges)
  call stpozlev_(o3lhead,roz,soz,out,sges)

  return

end subroutine stpoz

subroutine stpozlay_(ozhead,roz,soz,out,sges)
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
!   2007-02-15  rancic  - add foto
!   2007-03-19  tremolet - binning of observations
!   2007-05-30  h.liu   - move interpolation weights w1-w4 inside k loop
!   2007-06-04  derber  - use quad precision to get reproducability over number of processors
!   2008-12-03  todling - update handle of foto
!
!   input argument list:
!     ozhead  - layer ozone obs type pointer to obs structure
!     roz  - search direction for ozone
!     soz  - input ozone correction field
!     sges - step size estimates (4)
!
!   output argument list:
!     out(1) - contribution of ozone data to penalty sges(1)
!     out(2) - contribution of ozone data to penalty sges(2)
!     out(3) - contribution of ozone data to penalty sges(3)
!     out(4) - contribution of ozone data to penalty sges(4)
!     out(5) - contribution to numerator from ozone
!     out(6) - contribution to denomonator from ozone
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,i_kind,r_quad
  use obsmod, only: oz_ob_type
  use ozinfo, only: b_oz,pg_oz
  use qcmod, only: nlnqc_iter
  use constants, only: zero,one,half,two,tiny_r_kind,cg_term,zero_quad,r3600
  use gridmod, only: lat2,lon2,nsig
  use jfunc, only: l_foto,xhat_dt,dhat_dt
  implicit none

! Declare passed variables
  type( oz_ob_type),pointer,intent(in)::  ozhead
  real(r_quad),dimension(6),intent(inout):: out
  real(r_kind),dimension(lat2*lon2,nsig),intent(in):: roz,soz
  real(r_kind),dimension(4),intent(in):: sges

! Declare local variables
  integer(i_kind) k,j1,j2,j3,j4,kk,iz1,iz2,j1x,j2x,j3x,j4x
  real(r_kind) dz1,pob,delz
  real(r_kind) w1,w2,w3,w4,time_oz
  real(r_kind) oz0,oz1,oz2,oz3,pen1,pen2,pen3,pencur
  real(r_kind) alpha,ccoef,bcoef1,bcoef2,cc
  type( oz_ob_type), pointer ::  ozptr

  real(r_quad) val,val1

! Initialize output variables to zero
  alpha=one/(sges(3)-sges(2))
  ccoef=half*alpha*alpha
  bcoef1=half*half*alpha
  bcoef2=sges(3)*ccoef

! SBUV OZONE: LAYER O3 and TOTAL O3
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
        if(l_foto)time_oz=ozptr%time*r3600

!    Accumulate contribution from layer observations
        dz1=nsig+1

        if ( ozptr%nloz >= 1 ) then

           do k=1,ozptr%nloz
              val1= -ozptr%res(k)
              val = zero_quad
              pob = ozptr%prs(k)
              iz1 = dz1
              if (iz1>nsig) iz1=nsig
              iz2 = pob
        
              do kk=iz2,iz1
                 delz=one
                 if (kk==iz1) delz=dz1-iz1
                 if (kk==iz2) delz=delz-pob+iz2
                 w1=ozptr%wij(1,kk)
                 w2=ozptr%wij(2,kk)
                 w3=ozptr%wij(3,kk)
                 w4=ozptr%wij(4,kk)
                 val=val + ( &
                      w1* roz(j1,kk)+ &
                      w2* roz(j2,kk)+ &
                      w3* roz(j3,kk)+ &
                      w4* roz(j4,kk))*delz
                 val1=val1 + ( &
                      w1* soz(j1,kk)+ &
                      w2* soz(j2,kk)+ &
                      w3* soz(j3,kk)+ &
                      w4* soz(j4,kk))*delz
                 if(l_foto) then
                   j1x=j1+(kk-1)*lat2*lon2
                   j2x=j2+(kk-1)*lat2*lon2
                   j3x=j3+(kk-1)*lat2*lon2
                   j4x=j4+(kk-1)*lat2*lon2
                   val=val + ( &
                     (w1*dhat_dt%oz(j1x)+ &
                      w2*dhat_dt%oz(j2x)+ &
                      w3*dhat_dt%oz(j3x)+ &
                      w4*dhat_dt%oz(j4x))*time_oz )*delz
                   val1=val1 + ( &
                     (w1*xhat_dt%oz(j1x)+ &
                      w2*xhat_dt%oz(j2x)+ &
                      w3*xhat_dt%oz(j3x)+ &
                      w4*xhat_dt%oz(j4x))*time_oz )*delz
                 end if
              end do
              oz0=val1+sges(1)*val
              oz1=val1+sges(2)*val
              oz2=val1+sges(3)*val
              oz3=val1+sges(4)*val

              pencur = ozptr%err2(k)*oz0*oz0
              pen1   = ozptr%err2(k)*oz1*oz1
              pen2   = ozptr%err2(k)*oz2*oz2
              pen3   = ozptr%err2(k)*oz3*oz3

              cc     = (pen1+pen3-two*pen2)*ozptr%raterr2(k)
              out(1) = out(1)+pencur*ozptr%raterr2(k)
              out(2) = out(2)+pen1  *ozptr%raterr2(k)
              out(3) = out(3)+pen2  *ozptr%raterr2(k)
              out(4) = out(4)+pen3  *ozptr%raterr2(k)
              out(5) = out(5)+(pen1-pen3)*ozptr%raterr2(k)*bcoef1+cc*bcoef2
              out(6) = out(6)+cc*ccoef
              dz1=pob
           end do
           
        end if

!    Add contribution from total column observation
        k   = ozptr%nloz+1
        val1= -ozptr%res(k)
        val  = zero_quad
        do kk=1,nsig
           w1=ozptr%wij(1,kk)
           w2=ozptr%wij(2,kk)
           w3=ozptr%wij(3,kk)
           w4=ozptr%wij(4,kk)
           val=val+  (          &
                w1* roz(j1,kk)+ &
                w2* roz(j2,kk)+ &
                w3* roz(j3,kk)+ &
                w4* roz(j4,kk))
           val1=val1 +  (       &
                w1* soz(j1,kk)+ &
                w2* soz(j2,kk)+ &
                w3* soz(j3,kk)+ & 
                w4* soz(j4,kk))
           if(l_foto)then
             j1x=j1+(kk-1)*lat2*lon2
             j2x=j2+(kk-1)*lat2*lon2
             j3x=j3+(kk-1)*lat2*lon2
             j4x=j4+(kk-1)*lat2*lon2
             val=val+ ( &
               (w1*xhat_dt%oz(j1x)+ &
                w2*xhat_dt%oz(j2x)+ &
                w3*xhat_dt%oz(j3x)+ & 
                w4*xhat_dt%oz(j4x))*time_oz )
             val1=val1 + ( &
               (w1*dhat_dt%oz(j1x)+ &
                w2*dhat_dt%oz(j2x)+ &
                w3*dhat_dt%oz(j3x)+ &
                w4*dhat_dt%oz(j4x))*time_oz )
           end if
        enddo
        oz0=val1+sges(1)*val
        oz1=val1+sges(2)*val
        oz2=val1+sges(3)*val
        oz3=val1+sges(4)*val

        pencur = ozptr%err2(k)*oz0*oz0
        pen1   = ozptr%err2(k)*oz1*oz1
        pen2   = ozptr%err2(k)*oz2*oz2
        pen3   = ozptr%err2(k)*oz3*oz3
        
        cc     = (pen1+pen3-two*pen2)*ozptr%raterr2(k)
        out(1) = out(1) +pencur*ozptr%raterr2(k)
        out(2) = out(2) +pen1  *ozptr%raterr2(k)
        out(3) = out(3) +pen2  *ozptr%raterr2(k)
        out(4) = out(4) +pen3  *ozptr%raterr2(k)
        out(5) = out(5)+(pen1-pen3)*ozptr%raterr2(k)*bcoef1+cc*bcoef2
        out(6) = out(6)+cc*ccoef
     end if

     ozptr => ozptr%llpoint

! End of loop over observations
  enddo

! End of routine.
  return
end subroutine stpozlay_

subroutine stpozlev_(o3lhead,roz1d,soz1d,out,sges)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    stpozlev    compute contribution to penalty and
!                            stepsize for o3 level obs, using nonlinear qc
!   prgmmr: sienkiewicz     org: GMAO                 date: 2006-09-14
!
! abstract: The routine computes the contribution to the penalty from ozone
!           observations.  The routine also computes the contribution of
!           ozone observations to the step size.  This version includes
!           nonlinear qc.
!
! program history log:
!   2006-09-14  sienkiewicz - add level ozone obs
!   2007-01-02  sienkiewicz - separate subroutine
!   2007-01-05  sienkiewicz - update to 9/2006 GSI (new obs structure)
!   2009-01-21  sienkiewicz - update to 1/2009 GSI, changes based on stpq & stpoz
!
!   input argument list:
!     o3lhead - level ozone obs type pointer to obs structure
!     roz1d  - search direction for ozone (as 1d var)
!     soz1d  - input ozone correction field (as 1d var)
!     sges - step size estimates (4)
!
!   output argument list:
!     out(1) - contribution of ozone data to penalty sges(1)
!     out(2) - contribution of ozone data to penalty sges(2)
!     out(3) - contribution of ozone data to penalty sges(3)
!     out(4) - contribution of ozone data to penalty sges(4)
!     out(5) - contribution to numerator from ozone
!     out(6) - contribution to denomonator from ozone
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,i_kind,r_quad
  use obsmod, only: o3l_ob_type
  use qcmod, only: nlnqc_iter
  use constants, only: zero,one,half,two,tiny_r_kind,cg_term,r3600
  use gridmod, only: latlon1n
  use jfunc, only: l_foto,xhat_dt,dhat_dt
  implicit none

! Declare passed variables
  type(o3l_ob_type),pointer,intent(in):: o3lhead
  real(r_quad),dimension(6),intent(out):: out
  real(r_kind),dimension(latlon1n),intent(in):: roz1d,soz1d
  real(r_kind),dimension(4),intent(in):: sges

! Declare local variables
  integer(i_kind) i,j1,j2,j3,j4,j5,j6,j7,j8
  real(r_kind) cg_oz,pen1,pen2,pen3,pencur,oz0,oz1,oz2,oz3,wgross,wnotgross
  real(r_kind) w1,w2,w3,w4,w5,w6,w7,w8,val,val2, time_oz
  real(r_kind) alpha,ccoef,bcoef1,bcoef2,cc
  type(o3l_ob_type), pointer :: o3lptr

! Initialize output variables to zero

  alpha=one/(sges(3)-sges(2))
  ccoef=half*alpha*alpha
  bcoef1=half*half*alpha
  bcoef2=sges(3)*ccoef
  time_oz = zero

  o3lptr => o3lhead

! Loop over level ozone observations
!
  do while (associated(o3lptr))
     if(o3lptr%luse)then
        j1=o3lptr%ij(1)
        j2=o3lptr%ij(2)
        j3=o3lptr%ij(3)
        j4=o3lptr%ij(4)
        j5=o3lptr%ij(5)
        j6=o3lptr%ij(6)
        j7=o3lptr%ij(7)
        j8=o3lptr%ij(8)
        w1=o3lptr%wij(1)
        w2=o3lptr%wij(2)
        w3=o3lptr%wij(3)
        w4=o3lptr%wij(4)
        w5=o3lptr%wij(5)
        w6=o3lptr%wij(6)
        w7=o3lptr%wij(7)
        w8=o3lptr%wij(8)


        val= w1*roz1d(j1)+w2*roz1d(j2)+w3*roz1d(j3)+w4*roz1d(j4)+ &
             w5*roz1d(j5)+w6*roz1d(j6)+w7*roz1d(j7)+w8*roz1d(j8)   
        val2=w1*soz1d(j1)+w2*soz1d(j2)+w3*soz1d(j3)+w4*soz1d(j4)+ &
             w5*soz1d(j5)+w6*soz1d(j6)+w7*soz1d(j7)+w8*soz1d(j8)-o3lptr%res

        if(l_foto) then
           time_oz=o3lptr%time*r3600
           val=val+ (w1*dhat_dt%oz(j1)+w2*dhat_dt%oz(j2)+ &
                     w3*dhat_dt%oz(j3)+w4*dhat_dt%oz(j4)+ &
                     w5*dhat_dt%oz(j5)+w6*dhat_dt%oz(j6)+ &
                     w7*dhat_dt%oz(j7)+w8*dhat_dt%oz(j8))*time_oz
           val2=val2+ (w1*xhat_dt%oz(j1)+w2*xhat_dt%oz(j2)+ &
                       w3*xhat_dt%oz(j3)+w4*xhat_dt%oz(j4)+ &
                       w5*xhat_dt%oz(j5)+w6*xhat_dt%oz(j6)+ &
                       w7*xhat_dt%oz(j7)+w8*xhat_dt%oz(j8))*time_oz
        end if
        oz0=val2+sges(1)*val
        oz1=val2+sges(2)*val
        oz2=val2+sges(3)*val
        oz3=val2+sges(4)*val

        pencur = oz0*oz0*o3lptr%err2
        pen1   = oz1*oz1*o3lptr%err2
        pen2   = oz2*oz2*o3lptr%err2
        pen3   = oz3*oz3*o3lptr%err2

        out(1) = out(1)+pencur*o3lptr%raterr2
        out(2) = out(2)+pen1  *o3lptr%raterr2
        out(3) = out(3)+pen2  *o3lptr%raterr2
        out(4) = out(4)+pen3  *o3lptr%raterr2
        cc     = (pen1+pen3-two*pen2)*o3lptr%raterr2
        out(5) = out(5)+(pen1-pen3)*o3lptr%raterr2*bcoef1+cc*bcoef2
        out(6) = out(6)+cc*ccoef
     end if

     o3lptr => o3lptr%llpoint

  end do

! End of routine.
  return
end subroutine stpozlev_

end module stpozmod
