!-------------------------------------------------------------------------
!    NOAA/NCEP, National Centers for Environmental Prediction GSI        !
!-------------------------------------------------------------------------
!BOP
!
! !MODULE:  guess_grids --- Guess-related grid definitions
!
! !INTERFACE:
!

module guess_grids_enspro

! !USES:
 
  use kinds, only: r_single,r_kind,i_kind
  use gridmod, only: regional
  use gridmod, only: wrf_nmm_regional,nems_nmmb_regional
  use gridmod, only: eta1_ll
  use gridmod, only: eta2_ll
  use gridmod, only: aeta1_ll
  use gridmod, only: aeta2_ll
  use gridmod, only: pdtop_ll
  use gridmod, only: pt_ll
  use guess_grids, only: ges_prsl,ges_prsi,ges_lnprsl,ges_lnprsi
  use guess_grids, only: nfldsig,ntguessig

  use gsi_bundlemod, only : gsi_bundlegetpointer
  use gsi_metguess_mod, only: gsi_metguess_bundle

  ! meteorological guess (beyond standard ones)

  use mpeu_util, only: die,tell
  implicit none

! !DESCRIPTION: module containing variables related to the guess fields
!
! !REVISION HISTORY:
!
!EOP
!-------------------------------------------------------------------------

! set default to private
  private
! set subroutines to public
  public :: load_prsges_enspro

  character(len=*),parameter::myname='guess_grids'

contains

!-------------------------------------------------------------------------
!    NOAA/NCEP, National Centers for Environmental Prediction GSI        !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE: load_prsges --- Populate guess pressure arrays
!
! !INTERFACE:
!
  subroutine load_prsges_enspro

! !USES:

    use constants,only: zero,one,rd_over_cp,one_tenth,half,ten
    use gridmod, only: lat2,lon2,nsig,ak5,bk5,ck5,tref5,idvc5,&
         regional,wrf_nmm_regional,nems_nmmb_regional,wrf_mass_regional,&
         cmaq_regional,pt_ll,aeta2_ll,&
         aeta1_ll,eta2_ll,pdtop_ll,eta1_ll,twodvar_regional,idsl5
    implicit none

! !DESCRIPTION: populate guess pressure arrays
!
! !REVISION HISTORY:
!   2003-10-15  kleist
!   2004-03-22  parrish, regional capability added
!   2004-05-14  kleist, documentation
!   2004-07-15  todling, protex-compliant prologue; added onlys
!   2004-07-28  treadon - remove subroutine call list, pass variables via modules
!   2005-05-24  pondeca - add regional surface analysis option
!   2006-04-14  treadon - unify global calculations to use ak5,bk5
!   2006-04-17  treadon - add ges_psfcavg and ges_prslavg for regional
!   2006-07-31  kleist  - use ges_ps instead of ln(ps)
!   2007-05-08  kleist  - add fully generalized coordinate for pressure calculation
!   2011-07-07  todling - add cap for log(pressure) calculation
!
! !REMARKS:
!   language: f90
!   machine:  ibm rs/6000 sp; SGI Origin 2000; Compaq/HP
!
! !AUTHOR:
!   kleist          org: w/nmc20     date: 2003-10-15
!
!EOP
!-------------------------------------------------------------------------

!   Declare local parameter
    character(len=*),parameter::myname_=myname//'*load_prsges'
    real(r_kind),parameter:: r1013=1013.0_r_kind

!   Declare local variables
    real(r_kind) kap1,kapr,trk
    real(r_kind),dimension(:,:)  ,pointer::ges_ps=>NULL()
    real(r_kind),dimension(:,:,:),pointer::ges_tv=>NULL()
    integer(i_kind) i,j,k,jj,itv,ips
    logical ihaveprs(nfldsig)

    kap1=rd_over_cp+one
    kapr=one/rd_over_cp

    ihaveprs=.false.
    do jj=1,nfldsig
       call gsi_bundlegetpointer(gsi_metguess_bundle(jj),'ps' ,ges_ps,ips)
       if(ips/=0) call die(myname_,': ps not available in guess, abort',ips)
       call gsi_bundlegetpointer(gsi_metguess_bundle(jj),'tv' ,ges_tv,itv)
       if(idvc5==3) then
          if(itv/=0) call die(myname_,': tv must be present when idvc5=3, abort',itv)
       endif
       do k=1,nsig+1
          do j=1,lon2
             do i=1,lat2
                if(regional) then
                   if (wrf_nmm_regional.or.nems_nmmb_regional.or.&
                        cmaq_regional ) &
                      ges_prsi(i,j,k,jj)=one_tenth* &
                             (eta1_ll(k)*pdtop_ll + &
                              eta2_ll(k)*(ten*ges_ps(i,j)-pdtop_ll-pt_ll) + &
                              pt_ll)

                   if (wrf_mass_regional .or. twodvar_regional) &
                      ges_prsi(i,j,k,jj)=one_tenth*(eta1_ll(k)*(ten*ges_ps(i,j)-pt_ll) + pt_ll)
                endif
                ges_prsi(i,j,k,jj)=max(ges_prsi(i,j,k,jj),zero)
                ges_lnprsi(i,j,k,jj)=log(max(ges_prsi(i,j,k,jj),0.0001_r_kind))
             end do
          end do
       end do
       ihaveprs(jj)=.true.
    end do

    if(regional) then
       if (wrf_nmm_regional.or.nems_nmmb_regional.or.cmaq_regional) then
! load using aeta coefficients
          do jj=1,nfldsig
             call gsi_bundlegetpointer(gsi_metguess_bundle(jj),'ps' ,ges_ps ,ips)
             do k=1,nsig
                do j=1,lon2
                   do i=1,lat2
                      ges_prsl(i,j,k,jj)=one_tenth* &
                                  (aeta1_ll(k)*pdtop_ll + &
                                   aeta2_ll(k)*(ten*ges_ps(i,j)-pdtop_ll-pt_ll) + &
                                   pt_ll)
                      ges_lnprsl(i,j,k,jj)=log(ges_prsl(i,j,k,jj))

                   end do
                end do
             end do
          end do
       end if   ! end if wrf_nmm regional block
       if (wrf_mass_regional .or. twodvar_regional) then
! load using aeta coefficients
          do jj=1,nfldsig
             call gsi_bundlegetpointer(gsi_metguess_bundle(jj),'ps' ,ges_ps ,ips)
          write(*,*) 'ps==',maxval(ges_ps),minval(ges_ps)
             
             do k=1,nsig
                do j=1,lon2
                   do i=1,lat2
                      ges_prsl(i,j,k,jj)=one_tenth*(aeta1_ll(k)*(ten*ges_ps(i,j)-pt_ll)+pt_ll)
                      ges_lnprsl(i,j,k,jj)=log(ges_prsl(i,j,k,jj))
                   end do
                end do
             end do
          end do
       end if   ! end if wrf_mass regional block

    else

    end if  !  end regional/global block

    return
  end subroutine load_prsges_enspro

    
end module guess_grids_enspro
