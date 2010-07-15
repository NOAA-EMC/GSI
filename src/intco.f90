module intcomod

!$$$ module documentation block
!           .      .    .                                       .
! module:   intcomod    module for intco and its tangent linear intco_tl
!   prgmmr:
!
! abstract: module for intco and its tangent linear intco_tl
!
! program history log:
!   2005-05-13  Yanqiu zhu - wrap intoz and its tangent linear intoz_tl into one module
!   2005-11-16  Derber - remove interfaces
!   2008-11-26  Todling - remove intoz_tl; add interface back
!   2009-08-13  lueken - update documentation
!   2010-06-02  tangborn - converted intoz into intco 
!
! subroutines included:
!   sub intco_
!   sub intcolev_
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
PUBLIC intco

interface intco; module procedure &
          intco_
end interface

contains

subroutine intco_(co3lhead,rval,sval)

!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    intco       call individual carbon monoxide obs operators
!   prgmmr: todling       org: np23                date: 2008-11-28
!
! abstract:  This routine calls the individual components of the 
!            carbon monoxide observation operator.
!
! program history log:
!   2008-11-28  todling
!   2009-01-08  todling - remove reference to ozohead
!   2010-05-13  todling - update to use gsi_bundle
!   2010-06-02  tangborn - made version for carbon monoxide
!
!   input argument list:
!     co3lhead  - level carbon monoxide obs type pointer to obs structure for MOPITT
!     sco     - carbon monoxide increment in grid space
!
!   output argument list:
!     rco    - carbon monoxide results from observation operator 
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
!--------
  use obsmod, only: co3l_ob_type
  use gsi_bundlemod, only: gsi_bundle
  implicit none

! Declare passed variables
  type(co3l_ob_type),pointer,intent(in   ) :: co3lhead
  type(gsi_bundle),intent(in   ) :: sval
  type(gsi_bundle),intent(inout) :: rval

  call intcolev_(co3lhead,rval,sval)

end subroutine intco_

subroutine intcolev_(co3lhead,rval,sval)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    intco       apply nonlin qc obs operator for carbon monoxide
!   prgmmr: derber           org: np23                date: 1995-07-11
!
! abstract:  This routine applies the observation operator (forward
!            model) and adjoint of this operator for ozone observations
!            with the addition of nonlinear qc.
!
! program history log:
!   1995-07-11  derber
!   2010-06-07  tangborn - carbon monoxide based on ozone code
!
!   input argument list:
!     co3lhead  - level carbon monoxide obs type pointer to obs structure
!     sco     - carbon monoxide increment in grid space
!
!   output argument list:
!     rco     - carbon monoxide results from observation operator 
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
!--------
  use kinds, only: r_kind,i_kind,r_quad
  use obsmod, only: co3l_ob_type,lsaveobsens,l_do_adjoint
  use gridmod, only: lat2,lon2,nsig
  use jfunc, only: jiter,xhat_dt,dhat_dt
  use constants, only: one,zero,r3600,zero_quad
  use gsi_bundlemod, only: gsi_bundle
  use gsi_bundlemod, only: gsi_bundlegetpointer
  implicit none

! Declare passed variables
  type(co3l_ob_type),pointer,intent(in   ) :: co3lhead
  type(gsi_bundle)          ,intent(in   ) :: sval
  type(gsi_bundle)          ,intent(inout) :: rval

! Declare local variables
  integer(i_kind) i,j,ij,ier,istatus
  integer(i_kind) k,k1,k2,j1,j2,j3,j4,kk,iz1,iz2,j1x,j2x,j3x,j4x
  real(r_kind) pob,time_co
  real(r_quad) val1,valx
  real(r_kind) w1,w2,w3,w4,w5,w6,w7,w8
  real(r_kind),pointer,dimension(:) :: xhat_dt_co
  real(r_kind),pointer,dimension(:) :: dhat_dt_co
  real(r_kind),pointer,dimension(:,:,:)  :: scop
  real(r_kind),pointer,dimension(:,:,:)  :: rcop
  real(r_kind),allocatable,dimension(:,:) :: sco
  real(r_kind),allocatable,dimension(:,:) :: rco
  real(r_kind),allocatable,dimension(:)   :: coak
  real(r_kind),allocatable,dimension(:)   :: vali
  type(co3l_ob_type), pointer :: co3lptr

! Retrieve pointers
! Simply return if any pointer not found
  ier=0 
  call gsi_bundlegetpointer(sval,'co',scop,istatus);ier=istatus+ier
  call gsi_bundlegetpointer(rval,'co',rcop,istatus);ier=istatus+ier
  if(ier/=0)return

! Can't do rank-2 pointer into rank-2, therefore, allocate work space
  allocate(sco(lat2*lon2,nsig),rco(lat2*lon2,nsig))
  do k=1,nsig
     ij=0
     do j=1,lon2
        do i=1,lat2
           ij=ij+1
           sco(ij,k) = scop(i,j,k)
           rco(ij,k) = rcop(i,j,k)
        enddo
     enddo
  enddo
!
! MOPITT CARBON MONOXIDE: LAYER CO 
!
! Loop over carbon monoxide observations.
  co3lptr => co3lhead
  do while (associated(co3lptr))

!    Set location
     j1=co3lptr%ij(1)
     j2=co3lptr%ij(2)
     j3=co3lptr%ij(3)
     j4=co3lptr%ij(4)


!    Accumulate contribution from layer observations
!    This repeats the algorithm inside intrp3co
!    with several of the terms already calculated 

     if ( co3lptr%nlco >= 1 ) then

        allocate(vali(co3lptr%nlco))
        allocate(coak(co3lptr%nlco))

        do k=1,co3lptr%nlco
           val1= zero_quad
           pob = co3lptr%prs(k)
           k1=int(pob)
           k2=min(k1+1,nsig)
           w1=co3lptr%wij(1,k)
           w2=co3lptr%wij(2,k)
           w3=co3lptr%wij(3,k)
           w4=co3lptr%wij(4,k)
           w5=co3lptr%wij(5,k)
           w6=co3lptr%wij(6,k)
           w7=co3lptr%wij(7,k)
           w8=co3lptr%wij(8,k)
           val1=val1 +  &
                   w1* sco(j1,k1)+ &
                   w2* sco(j2,k1)+ &
                   w3* sco(j3,k1)+ &
                   w4* sco(j4,k1)+ &
                   w5* sco(j1,k2)+ &
                   w6* sco(j2,k2)+ &
                   w7* sco(j3,k2)+ & 
                   w8* sco(j4,k2)
           vali(k)=val1
        enddo

!       Averaging kernel and a priori 
!       Will need to revisit this linearization. This is 
!       a simplified linearization assuming very small perturbations 

        do k=1,co3lptr%nlco
           val1=zero_quad
           do j=1,co3lptr%nlco
              val1=val1+co3lptr%ak(k,j)*vali(j)
           enddo 

           if (lsaveobsens) then
              co3lptr%diags(k)%ptr%obssen(jiter)=val1*co3lptr%err2(k)*co3lptr%raterr2(k)
           else
              if (co3lptr%luse) co3lptr%diags(k)%ptr%tldepart(jiter)=val1
           endif

           if (l_do_adjoint) then
              if (lsaveobsens) then
                 valx = co3lptr%diags(k)%ptr%obssen(jiter)

              else
                 val1=val1-co3lptr%res(k)

                 valx= val1*co3lptr%err2(k) 
                 valx= valx*co3lptr%raterr2(k)
              endif

!  Averaging kernel First 
!  For small perturbations, ignore log terms in MOPITT Averaging Kernel (AVT)

              do kk=co3lptr%nlco,1,-1 
                 coak(kk)=zero
                 do j=1,co3lptr%nlco
                    coak(kk)=coak(kk)+co3lptr%ak(kk,j)*valx
                 enddo
              enddo 

              do kk=co3lptr%nlco,1,-1
                 pob = co3lptr%prs(kk)
                 k1=int(pob)
                 k2=min(k1+1,nsig)
                 w1=co3lptr%wij(1,kk)
                 w2=co3lptr%wij(2,kk)
                 w3=co3lptr%wij(3,kk)
                 w4=co3lptr%wij(4,kk)
                 w5=co3lptr%wij(5,kk)
                 w6=co3lptr%wij(6,kk)
                 w7=co3lptr%wij(7,kk) 
                 w8=co3lptr%wij(8,kk) 
                 rco(j1,k1)  =  rco(j1,k1) + coak(kk)*w1
                 rco(j2,k1)  =  rco(j2,k1) + coak(kk)*w2
                 rco(j3,k1)  =  rco(j3,k1) + coak(kk)*w3
                 rco(j4,k1)  =  rco(j4,k1) + coak(kk)*w4
                 rco(j1,k2)  =  rco(j1,k2) + coak(kk)*w5
                 rco(j2,k2)  =  rco(j2,k2) + coak(kk)*w6
                 rco(j3,k2)  =  rco(j3,k2) + coak(kk)*w7
                 rco(j4,k2)  =  rco(j4,k2) + coak(kk)*w8
              enddo

           endif
        end do

        deallocate(coak,vali)
     end if   ! (co3lptr%nlco >= 1)

!    Add contribution from total column observation
!     k=co3lptr%nlco+1
!     val1= zero_quad
!     do kk=nsig,1,-1
!        w1=co3lptr%wij(1,kk)
!        w2=co3lptr%wij(2,kk)
!        w3=co3lptr%wij(3,kk)
!        w4=co3lptr%wij(4,kk)
!        val1=val1 + &
!             w1* sco(j1,kk)+ &
!             w2* sco(j2,kk)+ &
!             w3* sco(j3,kk)+ &
!             w4* sco(j4,kk)
!     enddo
!
!     if (lsaveobsens) then
!        co3lptr%diags(k)%ptr%obssen(jiter)=val1*co3lptr%err2(k)*co3lptr%raterr2(k)
!     else
!        if (co3lptr%luse) co3lptr%diags(k)%ptr%tldepart(jiter)=val1
!     endif
!
!     if (l_do_adjoint) then
!        if (lsaveobsens) then
!           valx = co3lptr%diags(k)%ptr%obssen(jiter)
!
!        else
!           val1=val1-co3lptr%res(k)
!
!           valx     = val1*co3lptr%err2(k)
!           valx     = valx*co3lptr%raterr2(k)
!        endif
!
!        do kk=nsig,1,-1
!           w1=co3lptr%wij(1,kk)
!           w2=co3lptr%wij(2,kk)
!           w3=co3lptr%wij(3,kk)
!           w4=co3lptr%wij(4,kk)
!           rco(j1,kk)  = rco(j1,kk) + valx*w1
!           rco(j2,kk)  = rco(j2,kk) + valx*w2
!           rco(j3,kk)  = rco(j3,kk) + valx*w3
!           rco(j4,kk)  = rco(j4,kk) + valx*w4
!        enddo
!     endif

     co3lptr => co3lptr%llpoint

! End loop over observations
  enddo

! Copy output and clean up 
  do k=1,nsig
     ij=0
     do j=1,lon2
        do i=1,lat2
           ij=ij+1
           rcop(i,j,k) = rco(ij,k)
        enddo
     enddo
  enddo
  deallocate(sco,rco)

! End of routine
  return
end subroutine intcolev_


end module intcomod
