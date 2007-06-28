module intozmod

!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    intozmod    module for intoz and its tangent linear intoz_tl
!
! abstract: module for intoz and its tangent linear intoz_tl
!
! program history log:
!   2005-05-13  Yanqiu zhu - wrap intoz and its tangent linear intoz_tl into one module
!   2005-11-16  Derber - remove interfaces
!

implicit none

PRIVATE
PUBLIC intoz,intoz_tl

contains

subroutine intoz(roz,soz)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    intoz       apply nonlin qc obs operator for ozone
!   prgmmr: derber           org: np23                date: 1995-07-11
!
! abstract:  This routine applies the observation operator (forward
!            model) and adjoint of this operator for ozone observations
!            with the addition of nonlinear qc.
!
! program history log:
!   1995-07-11  derber
!   1999-03-01  wu - port cray90 code to ibm-sp (mpi version)
!   2004-06-16  treadon - update documentation
!   2004-08-02  treadon - add only to module use, add intent in/out
!   2004-10-08  parrish - add nonlinear qc
!   2005-03-01  parrish - nonlinear qc change to account for inflated obs error
!   2005-04-11  treadon - merge intoz and intoz_qc into single routine
!   2005-06-14  wu      - add OMI total ozone
!   2005-09-28  derber  - consolidate location and weight arrays
!   2006-07-28  derber  - modify to use new inner loop obs data structure
!                       - unify NL qc
!
!   input argument list:
!     soz - ozone increment in grid space
!
!   output argument list:
!     roz - results from observation operator (0 for no data)
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
!--------
  use kinds, only: r_kind,i_kind
  use ozinfo, only:b_oz,pg_oz
  use obsmod, only: ozhead,ozptr,ozohead,ozoptr,nloz
  use qcmod, only: nlnqc_iter
  use gridmod, only: lat2,lon2,nsig
  use constants, only: one,half,two,zero,tiny_r_kind,cg_term
  implicit none

! Declare passed variables
  real(r_kind),dimension(lat2*lon2,nsig),intent(in):: soz
  real(r_kind),dimension(lat2*lon2,nsig),intent(inout):: roz

! Declare local variables
  integer(i_kind) i,k,j1,j2,j3,j4,kk,iz1,iz2,kx
  real(r_kind) dz1,pob,val1,valx,delz
! real(r_kind) penalty
  real(r_kind) cg_oz,p0,wnotgross,wgross
  real(r_kind) w1,w2,w3,w4


!
! SBUV OZONE: LAYER O3 and TOATL O3
!
! Loop over ozone observations.
  ozptr => ozhead
  do while (associated(ozptr))

!    Set location
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
        pob = ozptr%prs(k)
        iz1=dz1
        if (iz1 > nsig) iz1=nsig
        iz2=pob
        do kk=iz1,iz2,-1
           delz=one
           if (kk==iz1) delz=dz1-iz1
           if (kk==iz2) delz=delz-pob+iz2
           val1=val1 + ( &
                w1*soz(j1,kk) + &
                w2*soz(j2,kk)+ &
                w3*soz(j3,kk)+ &
                w4*soz(j4,kk) )*delz
        enddo

!       needed for gradient of nonlinear qc operator
        kx=ozptr%ipos(k)
        if (nlnqc_iter .and. pg_oz(kx) > tiny_r_kind .and.  &
                             b_oz(kx)  > tiny_r_kind) then
           cg_oz=cg_term/b_oz(kx)
           wnotgross= one-pg_oz(kx)
           wgross = pg_oz(kx)*cg_oz/wnotgross
           p0   = wgross/(wgross+exp(-half*ozptr%err2(k)*val1**2))
           val1 = val1*(one-p0)
        endif

        valx     = val1*ozptr%err2(k) 
        valx     = valx*ozptr%raterr2(k)
       
        do kk=iz1,iz2,-1
           delz=one
           if(kk.eq.iz1)delz=dz1-iz1
           if(kk.eq.iz2)delz=delz-pob+iz2
           roz(j1,kk) = roz(j1,kk) + valx*w1*delz
           roz(j2,kk) = roz(j2,kk) + valx*w2*delz
           roz(j3,kk) = roz(j3,kk) + valx*w3*delz
           roz(j4,kk) = roz(j4,kk) + valx*w4*delz
        enddo
        dz1=pob
     end do

!    Add contribution from total column observation
     k=nloz+1
     val1= -ozptr%res(k)
     do kk=nsig,1,-1
        val1=val1 + ( &
             w1*soz(j1,kk) + &
             w2*soz(j2,kk)+ &
             w3*soz(j3,kk)+ &
             w4*soz(j4,kk) )
     enddo

!    needed for gradient of nonlinear qc operator
     kx = ozptr%ipos(k)
     if (nlnqc_iter .and. pg_oz(kx) > tiny_r_kind .and. &
                          b_oz(kx)  > tiny_r_kind) then
        cg_oz=cg_term/b_oz(kx)
        wnotgross= one-pg_oz(kx)
        wgross = pg_oz(kx)*cg_oz/wnotgross
        p0   = wgross/(wgross+exp(-half*ozptr%err2(k)*val1**2))
        val1 = val1*(one-p0)
     endif

     valx     = val1*ozptr%err2(k)
     valx     = valx*ozptr%raterr2(k)


     do kk=nsig,1,-1
        roz(j1,kk) =roz(j1,kk) + valx*w1
        roz(j2,kk) =roz(j2,kk) + valx*w2
        roz(j3,kk) =roz(j3,kk) + valx*w3
        roz(j4,kk) =roz(j4,kk) + valx*w4
     enddo

     ozptr => ozptr%llpoint

! End loop over observations
  enddo

!
! OMI TOTAL OZONE
!
! Loop over ozone observations.
  ozoptr => ozohead
  do while (associated(ozoptr))

!    Set location
     j1=ozoptr%ij(1)
     j2=ozoptr%ij(2)
     j3=ozoptr%ij(3)
     j4=ozoptr%ij(4)
     w1=ozoptr%wij(1)
     w2=ozoptr%wij(2)
     w3=ozoptr%wij(3)
     w4=ozoptr%wij(4)

!    Add contribution from total column observation
     val1= -ozoptr%res
     do kk=nsig,1,-1
        val1=val1 + ( &
             w1*soz(j1,kk) + &
             w2*soz(j2,kk)+ &
             w3*soz(j3,kk)+ &
             w4*soz(j4,kk) )
     enddo

!    needed for gradient of nonlinear qc operator
     kx = ozoptr%ipos
     if (nlnqc_iter .and. pg_oz(kx) > tiny_r_kind .and. &
                          b_oz(kx)  > tiny_r_kind) then
        cg_oz=cg_term/b_oz(kx)
        wnotgross= one-pg_oz(kx)
        wgross = pg_oz(kx)*cg_oz/wnotgross
        p0   = wgross/(wgross+exp(-half*ozoptr%err2*val1**2))
        val1 = val1*(one-p0)
     endif

     valx     = val1*ozoptr%err2
     valx     = valx*ozoptr%raterr2


     do kk=nsig,1,-1
        roz(j1,kk) =roz(j1,kk) + valx*w1
        roz(j2,kk) =roz(j2,kk) + valx*w2
        roz(j3,kk) =roz(j3,kk) + valx*w3
        roz(j4,kk) =roz(j4,kk) + valx*w4
     enddo

     ozoptr => ozoptr%llpoint

! End loop over observations
  enddo



! End of routine
  return
end subroutine intoz


subroutine intoz_tl(roz,soz,roz_tl,soz_tl)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    intoz_tl       the tangent linear of the operator that applies 
!                              nonlin qc obs operator for ozone
!   prgmmr: yanqiu zhu           org: GMAO                date: 2005-05-13
!
! abstract:  This routine is the tangent linear of the operator that applies 
!            the observation operator (forward model) and adjoint of this 
!            operator for ozone observations with the addition of nonlinear qc.
!
! program history log:
!   2005-05-13  yanqiu zhu - tangent linear of intoz
!   2005-06-14  wu         - add OMI total ozone
!
!   input argument list:
!     soz - ozone increment in grid space
!     soz_tl - tangent linear ozone increment in grid space
!
!   output argument list:
!     roz - results from observation operator (0 for no data)
!     roz_tl - tangent linear results from observation operator (0 for no data)
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
!--------
  use kinds, only: r_kind,i_kind
  use obsmod, only: ozhead,ozptr,ozohead,ozoptr,nloz
  use obsmod_tl, only: oz_inv_tl,ozo_inv_tl
  use qcmod, only: nlnqc_iter
  use ozinfo, only: b_oz,pg_oz
  use gridmod, only: lat2,lon2,nsig
  use constants, only: one,half,two,zero,tiny_r_kind,cg_term
  implicit none

! Declare passed variables
  real(r_kind),dimension(lat2*lon2,nsig),intent(in):: soz
  real(r_kind),dimension(lat2*lon2,nsig),intent(in):: soz_tl
  real(r_kind),dimension(lat2*lon2,nsig),intent(inout):: roz
  real(r_kind),dimension(lat2*lon2,nsig),intent(inout):: roz_tl

! Declare local variables
  integer(i_kind) i,k,j1,j2,j3,j4,kk,iz1,iz2,kx
  real(r_kind) dz1,pob,val1,valx,delz
  real(r_kind) w1,w2,w3,w4
  real(r_kind) val1_tl,valx_tl
! real(r_kind) penalty
  real(r_kind) cg_oz,p0,wnotgross,wgross,term
  real(r_kind) p0_tl,term_tl


!
! SBUV OZONE: LAYER O3 and TOATL O3
!
! Loop over ozone observations.
  ozptr => ozhead
  i=0
  do while (associated(ozptr))

     i=i+1
!    Set location
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
        pob = ozptr%prs(k)
        iz1=dz1
        if (iz1 > nsig) iz1=nsig
        iz2=pob
        do kk=iz1,iz2,-1
           delz=one
           if (kk==iz1) delz=dz1-iz1
           if (kk==iz2) delz=delz-pob+iz2
           val1=val1 + ( &
                w1*soz(j1,kk) + &
                w2*soz(j2,kk)+ &
                w3*soz(j3,kk)+ &
                w4*soz(j4,kk) )*delz
        enddo

        val1_tl= -oz_inv_tl(k,i)
        do kk=iz1,iz2,-1
           delz=one
           if (kk==iz1) delz=dz1-iz1
           if (kk==iz2) delz=delz-pob+iz2
           val1_tl=val1_tl + ( &
                w1*soz_tl(j1,kk) + &
                w2*soz_tl(j2,kk)+ &
                w3*soz_tl(j3,kk)+ &
                w4*soz_tl(j4,kk) )*delz
        enddo

!       needed for gradient of nonlinear qc operator
        kx = ozptr%ipos(k)
        if (nlnqc_iter .and. pg_oz(kx) > tiny_r_kind) then
           cg_oz=cg_term/b_oz(kx)
           wnotgross= one-pg_oz(kx)
           wgross = pg_oz(kx)*cg_oz
           p0   = wnotgross*exp(-half*ozptr%err2(k)*val1**2)+wgross
           term = (p0-wgross)/p0
           p0_tl = -ozptr%err2(k)*val1*(p0-wgross)*val1_tl
           term_tl = wgross/(p0*p0)*p0_tl
        else
           term = one
           term_tl = zero
        endif
        valx     = val1*ozptr%err2(k) * term
        valx_tl   = val1_tl*ozptr%err2(k)*term + val1*ozptr%err2(k)*term_tl
        valx     = valx*ozptr%raterr2(k)
        valx_tl   = valx_tl*ozptr%raterr2(k)
       

        do kk=iz1,iz2,-1
           delz=one
           if(kk.eq.iz1)delz=dz1-iz1
           if(kk.eq.iz2)delz=delz-pob+iz2
           roz(j1,kk) = roz(j1,kk) + valx*w1*delz
           roz(j2,kk) = roz(j2,kk) + valx*w2*delz
           roz(j3,kk) = roz(j3,kk) + valx*w3*delz
           roz(j4,kk) = roz(j4,kk) + valx*w4*delz
           roz_tl(j1,kk) = roz_tl(j1,kk) + valx_tl*w1*delz
           roz_tl(j2,kk) = roz_tl(j2,kk) + valx_tl*w2*delz
           roz_tl(j3,kk) = roz_tl(j3,kk) + valx_tl*w3*delz
           roz_tl(j4,kk) = roz_tl(j4,kk) + valx_tl*w4*delz
        enddo
        dz1=pob
     end do

!    Add contribution from total column observation
     k=nloz+1
     val1= -ozptr%res(k)
     do kk=nsig,1,-1
        val1=val1 + ( &
             w1*soz(j1,kk) + &
             w2*soz(j2,kk)+ &
             w3*soz(j3,kk)+ &
             w4*soz(j4,kk) )
     enddo

     val1_tl= -oz_inv_tl(k,i)
     do kk=nsig,1,-1
        val1_tl=val1_tl + ( &
             w1*soz_tl(j1,kk) + &
             w2*soz_tl(j2,kk)+ &
             w3*soz_tl(j3,kk)+ &
             w4*soz_tl(j4,kk) )
     enddo

!    needed for gradient of nonlinear qc operator
     kx = ozptr%ipos(k)
     if (nlnqc_iter .and. pg_oz(kx) > tiny_r_kind) then
        cg_oz=cg_term/b_oz(kx)
        wnotgross= one-pg_oz(kx)
        wgross = pg_oz(kx)*cg_oz
        p0   = wnotgross*exp(-half*ozptr%err2(k)*val1**2)+wgross
        term = (p0-wgross)/p0
        p0_tl = -ozptr%err2(k)*val1*(p0-wgross)*val1_tl
        term_tl = wgross/(p0*p0)*p0_tl
     else
        term = one
        term_tl = zero
     endif
     valx     = val1*ozptr%err2(k)*term
     valx_tl   = val1_tl*ozptr%err2(k)*term + val1*ozptr%err2(k)*term_tl
     valx     = valx*ozptr%raterr2(k)
     valx_tl   = valx_tl*ozptr%raterr2(k)


     do kk=nsig,1,-1
        roz(j1,kk) =roz(j1,kk) + valx*w1
        roz(j2,kk) =roz(j2,kk) + valx*w2
        roz(j3,kk) =roz(j3,kk) + valx*w3
        roz(j4,kk) =roz(j4,kk) + valx*w4
        roz_tl(j1,kk) =roz_tl(j1,kk) + valx_tl*w1
        roz_tl(j2,kk) =roz_tl(j2,kk) + valx_tl*w2
        roz_tl(j3,kk) =roz_tl(j3,kk) + valx_tl*w3
        roz_tl(j4,kk) =roz_tl(j4,kk) + valx_tl*w4
     enddo

    ozptr => ozptr%llpoint

! End loop over observations
  enddo

!
! OMI TOTAL OZONE
!
! Loop over ozone observations.
  ozoptr => ozohead
  i=0
  do while (associated(ozoptr))
     i=i+1

!    Set location
     j1=ozoptr%ij(1)
     j2=ozoptr%ij(2)
     j3=ozoptr%ij(3)
     j4=ozoptr%ij(4)
     w1=ozoptr%wij(1)
     w2=ozoptr%wij(2)
     w3=ozoptr%wij(3)
     w4=ozoptr%wij(4)


     k = 1
!    Add contribution from total column observation
     val1= -ozoptr%res
     do kk=nsig,1,-1
        val1=val1 + ( &
             w1*soz(j1,kk) + &
             w2*soz(j2,kk)+ &
             w3*soz(j3,kk)+ &
             w4*soz(j4,kk) )
     enddo

     val1_tl= -ozo_inv_tl(i)
     do kk=nsig,1,-1
        val1_tl=val1_tl + ( &
             w1*soz_tl(j1,kk) + &
             w2*soz_tl(j2,kk)+ &
             w3*soz_tl(j3,kk)+ &
             w4*soz_tl(j4,kk) )
     enddo

!    needed for gradient of nonlinear qc operator
     kx = ozoptr%ipos
     if (nlnqc_iter .and. pg_oz(kx) > tiny_r_kind) then
        cg_oz=cg_term/b_oz(kx)
        wnotgross= one-pg_oz(kx)
        wgross = pg_oz(kx)*cg_oz
        p0   = wnotgross*exp(-half*ozoptr%err2*val1**2)+wgross
        term = (p0-wgross)/p0
        p0_tl = -ozoptr%err2*val1*(p0-wgross)*val1_tl
        term_tl = wgross/(p0*p0)*p0_tl
     else
        term = one
        term_tl = zero
     endif
     valx     = val1*ozoptr%err2*term
     valx_tl   = val1_tl*ozoptr%err2*term + val1*ozoptr%err2*term_tl
     valx     = valx*ozoptr%raterr2
     valx_tl   = valx_tl*ozoptr%raterr2


     do kk=nsig,1,-1
        roz(j1,kk) =roz(j1,kk) + valx*w1
        roz(j2,kk) =roz(j2,kk) + valx*w2
        roz(j3,kk) =roz(j3,kk) + valx*w3
        roz(j4,kk) =roz(j4,kk) + valx*w4
        roz_tl(j1,kk) =roz_tl(j1,kk) + valx_tl*w1
        roz_tl(j2,kk) =roz_tl(j2,kk) + valx_tl*w2
        roz_tl(j3,kk) =roz_tl(j3,kk) + valx_tl*w3
        roz_tl(j4,kk) =roz_tl(j4,kk) + valx_tl*w4
     enddo

     ozoptr => ozoptr%llpoint

! End loop over observations
  enddo

! End of routine
  return
end subroutine intoz_tl

end module intozmod
