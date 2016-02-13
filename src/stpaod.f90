module stpaodmod
  
!$$$ module documentation block
!           .      .    .                                       .
! module:   stpaodmod    module for stpaod and its tangent linear stpaod_tl
!  pgrmmr:
!
! abstract: module for stpaod and its tangent linear stpaod_tl
!
! program history log:
!   2005-05-19  yanqiu zhu - wrap stpq and its tangent linear stpq_tl into one module
!   2005-11-16  derber - remove interfaces
!   2008-12-02  todling - remove stpq_tl
!   2009-08-12  lueken - update documentation
!   2010-05-13  todling - uniform interface across stp routines
!   2014-01-15  pagowski - convert for aod following stprad
!
! subroutines included:
!   sub stpaod
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block
  implicit none

  private
  public stpaod
  
contains
  
  subroutine stpaod(aerohead,rval,sval,out,sges,nstep)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    stpaod        calcuate penalty and stepsize from q
!                            with addition of nonlinear qc.
!   prgmmr: derber           org: np23                date: 1991-02-26
!
! abstract: calculate penalty and contribution to stepsize from pm2_5
!           using nonlinear qc.
!
! program history log:
!   1991-02-26  derber
!   1993-08-25  wu
!   1998-02-03  weiyu yang
!   1999-08-24  derber, j., treadon, r., yang, w., first frozen mpp version
!   2004-08-02  treadon - add only to module use, add intent in/out
!   2004-10-05  parrish - add non-linear qc option
!   2005-04-11  treadon - merge stpq and stpq_qc into single routine
!   2005-08-02  derber  - modify for variational qc parameters for each ob
!   2005-09-28  derber  - consolidate location and weight arrays
!   2005-10-21  su      - modify for variational qc
!   2007-03-19  tremolet - binning of observations
!   2007-07-28  derber  - modify to use new inner loop obs data structure
!   2007-02-15  rancic  - add foto
!   2007-06-04  derber  - use quad precision to get reproducability over number of processors
!   2008-12-03  todling - changed handling of ptr%time
!   2010-01-04  zhang,b - bug fix: accumulate penalty for multiple obs bins
!   2010-05-13  todling - udpate to use gsi_bundle
!   2014-01-15  pagowski - udpate for aod
!
!   input argument list:
!     aerohead
!     rv_chem       - search direction for aero
!     sv_chem       - analysis increment for aero
!     sges     - stepsize estimates (nstep)
!     nstep    - number of stepsize estimates (== 0 means use outer iteration values)
!
!   output argument list:
!     out(1:nstep)   - contribution of penalty from aod sges(1:nstep)
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp
!
!$$$
    use kinds, only: r_kind,i_kind,r_quad
    use aeroinfo, only: aerojacnames,aerojacindxs,nsigaerojac,pg_aero,&
         b_aero
    use obsmod, only: aero_ob_type
    use qcmod, only: nlnqc_iter,varqc_iter
    use constants, only: half,one,two,tiny_r_kind,cg_term,zero_quad,zero
    use gsi_bundlemod, only: gsi_bundle
    use gsi_bundlemod, only: gsi_bundlegetpointer
    use gridmod, only: cmaq_regional,wrf_mass_regional,latlon11,nsig
    implicit none
    
! declare passed variables
    type(aero_ob_type),pointer             ,intent(in   ) :: aerohead
    integer(i_kind)                     ,intent(in   ) :: nstep
    real(r_quad),dimension(max(1,nstep)),intent(inout) :: out
    type(gsi_bundle)                    ,intent(in   ) :: rval,sval
    real(r_kind),dimension(max(1,nstep)),intent(in   ) :: sges
    
! declare local variables
    integer(i_kind) ier,istatus,naero
    integer(i_kind) j1,j2,j3,j4,kk,k,ic,nn
    real(r_kind) cg_aero,val,val2,wgross,wnotgross
    integer(i_kind),dimension(nsig) :: j1n,j2n,j3n,j4n
    real(r_kind),dimension(max(1,nstep)):: term,rad
    real(r_kind) w1,w2,w3,w4,qq
    real(r_kind),pointer,dimension(:):: sv_chem,rv_chem
    type(aero_ob_type), pointer :: aeroptr
    real(r_kind),dimension(nsigaerojac) :: tdir,rdir

    out=zero_quad

    naero = size(aerojacnames)
    if ( naero <= 0 ) return


!   if no aero data return
    if(.not. associated(aerohead))return

    if (cmaq_regional) then

       write(6,*)'aod for cmaq not implemented. stopping'       
       call stop2(460)

    endif

    IF (wrf_mass_regional) THEN

       tdir=zero
       rdir=zero

       aeroptr => aerohead

       do while (associated(aeroptr))
          if(aeroptr%luse)then
             if(nstep > 0)then

                j1=aeroptr%ij(1)
                j2=aeroptr%ij(2)
                j3=aeroptr%ij(3)
                j4=aeroptr%ij(4)
                w1=aeroptr%wij(1)
                w2=aeroptr%wij(2)
                w3=aeroptr%wij(3)
                w4=aeroptr%wij(4)

                j1n(1) = j1
                j2n(1) = j2
                j3n(1) = j3
                j4n(1) = j4

                do k=2,nsig
                   j1n(k) = j1n(k-1)+latlon11
                   j2n(k) = j2n(k-1)+latlon11
                   j3n(k) = j3n(k-1)+latlon11
                   j4n(k) = j4n(k-1)+latlon11
                enddo

                do ic = 1, naero
                   call gsi_bundlegetpointer (sval,trim(aerojacnames(ic)),sv_chem,istatus)
                   call gsi_bundlegetpointer (rval,trim(aerojacnames(ic)),rv_chem,istatus)

                   do k=1,nsig
                      j1 = j1n(k)
                      j2 = j2n(k)
                      j3 = j3n(k)
                      j4 = j4n(k)
                      tdir(k+nsig*(ic-1))=&
                           w1* sv_chem(j1)+w2* sv_chem(j2)+ &
                           w3* sv_chem(j3)+w4* sv_chem(j4)

                      rdir(k+nsig*(ic-1))=&
                           w1* rv_chem(j1)+w2* rv_chem(j2)+ &
                           w3* rv_chem(j3)+w4* rv_chem(j4)

                   end do
                   nullify(sv_chem,rv_chem)
                end do

             endif


             do nn=1,aeroptr%nlaero
                ic=aeroptr%icx(nn)

                val2=-aeroptr%res(nn)

                if(nstep > 0)then
                   val = zero

                   do k=1,nsigaerojac
                      val2=val2+tdir(k)*aeroptr%daod_dvar(k,nn)
                      val =val +rdir(k)*aeroptr%daod_dvar(k,nn)
                   end do

                   do kk=1,nstep
                      rad(kk)=val2+sges(kk)*val
                   end do

                else
                   rad(kk)= val2
                end if

!          calculate contribution to j

                do kk=1,max(1,nstep)
                   term(kk)  = aeroptr%err2(nn)*rad(kk)*rad(kk)
                end do

!          modify penalty term if nonlinear qc

                if(nlnqc_iter .and. pg_aero(ic) > tiny_r_kind .and. &
                     b_aero(ic)  > tiny_r_kind)then
                   cg_aero=cg_term/b_aero(ic)
                   wnotgross= one-pg_aero(ic)*varqc_iter
                   wgross = varqc_iter*pg_aero(ic)*cg_aero/wnotgross
                   do kk=1,max(1,nstep)
                      term(kk)  = -two*log((exp(-half*term(kk) ) + wgross)/&
                           (one+wgross))
                   end do
                endif

                out(1) = out(1) + term(1)*aeroptr%raterr2(nn)

                do kk=2,nstep
                   out(kk) = out(kk) + (term(kk)-term(1))*aeroptr%raterr2(nn)
                end do

             end do

          endif

          aeroptr => aeroptr%llpoint

       end do

    endif

    return

  end subroutine stpaod

end module stpaodmod
