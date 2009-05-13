subroutine tpause(mype,method)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    tpause      locate tropopause
!   prgmmr: treadon          org: np23                date: 2003-09-13
!
! abstract: locate tropopause using one of two methods.  The default
!           method uses the temperature lapse rate to identify the
!           tropopause.  Method 'pvoz' uses a combination of the 
!           potential voriticy and ozone mixing ration to locate
!           the tropopause.
!
! program history log:
!   2003-09-13 treadon - initial routine
!   2003-12-23 kleist  - generalized to use guess pressure
!   2004-06-15 treadon - reformat documentation
!   2004-07-26 treadon - remove call smooth121 (leads to different
!                           results when running code with different
!                           number of mpi tasks)
!   2004-07-27 treadon - add use only; add intent in/out
!   2005-11-29 derber  - remove psfcg and use ges_lnps instead
!   2006-02-02 treadon - rename prsl as ges_prsl
!   2006-07-28 derber  - use r1000 from constants
!                      - use sensible temperature rather than virtual
!   2006-07-31  kleist - change to ges_ps from ln(ps)
!   2008-04-03  safford - rm unused vars and uses
!
!   input argument list:
!     mype   - mpi task id
!     method - method used to locate tropopause
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,i_kind
  use constants, only: rd_over_cp,grav,rad2deg,one,r1000
  use guess_grids, only: ges_ps,tropprs,ges_oz,ges_vor,geop_hgtl,&
       ntguessig,ges_prsl,ges_tv,ges_tsen
  use gridmod, only: istart,nlat,rlats,nsig,lat2,lon2
  implicit none

! Declare passed variables
  character(4),intent(in):: method
  integer(i_kind),intent(in):: mype

! Declare local parameters
  real(r_kind),parameter:: r3em7=3.0e-7_r_kind
  real(r_kind),parameter:: r2em6=2.0e-6_r_kind
  real(r_kind),parameter:: r0_001=0.001_r_kind
  real(r_kind),parameter:: r0_01=0.01_r_kind
  real(r_kind),parameter:: r0_7=0.7_r_kind
  real(r_kind),parameter:: r20=20.0_r_kind
  real(r_kind),parameter:: r40=40.0_r_kind
  real(r_kind),parameter:: r1e5=1.0e5_r_kind

! Declare local variables
  logical t_method

  integer(i_kind) i,j,k,mm1
  integer(i_kind) npassh,npassv,latrad
  integer(i_kind) ifound_pv,ifound_oz,itrp_pv,itrp_oz,itrop_k

  real(r_kind) pm1,pp1
  real(r_kind) thetam1,thetap1,pv,wgt1,ptrop
  real(r_kind),dimension(nsig):: prs,tdry,hgt,pvort

  real(r_kind),dimension(lat2):: slatd
  real(r_kind),dimension(lat2,lon2):: trop_t,trop_pv,trop_oz
  real(r_kind),dimension(lat2,lon2):: trop_pvoz

  real(r_kind) psi

!================================================================================
! Set local constants
  npassh=6; npassv=0
  t_method = .false.
  if (index(method,'pvoz') == 0) t_method = .true.

! Locate tropopause based on temperature profile (WMO approach)
  if (t_method) then
     do j=1,lon2
        do i=1,lat2
           do k=1,nsig
              prs(k) = r1000*ges_prsl(i,j,k,ntguessig)
              tdry(k)= ges_tsen(i,j,k,ntguessig)
              hgt(k) = geop_hgtl(i,j,k,ntguessig)
           end do
           call tpause_t(nsig,prs,tdry,hgt,ptrop)
           trop_t(i,j) = ptrop*r0_01 !hPa
        end do
     end do

!    Load tropopause pressure (hPa) into output array (passed through module guess_grids)
     do j=1,lon2
        do i=1,lat2
           tropprs(i,j) = trop_t(i,j)
        end do
     end do


! Locate tropopause using combination of potential vorticity (pv) and ozone.  
  else

!    Compute latitudes on subdomain
     mm1=mype+1
     do i=1,lat2
        latrad=min(max(1,istart(mm1)+i-2),nlat)
        slatd(i)=abs(rlats(latrad))*rad2deg
     end do

!     Compute pv.  Use for locating tropopause poleward 30S/N
     do j=1,lon2
        do i = 1,lat2
           psi=one/ges_ps(i,j,ntguessig)
           do k=1,nsig
              prs(k) = r1000*ges_prsl(i,j,k,ntguessig)
           end do
        
!          Compute pv         
           do k = 2,nsig-1
              pm1 = prs(k-1)
              pp1 = prs(k+1)
              thetam1 = ges_tv(i,j,k-1,ntguessig)*(r1e5/pm1)**(rd_over_cp)
              thetap1 = ges_tv(i,j,k+1,ntguessig)*(r1e5/pp1)**(rd_over_cp)
              pv = grav*ges_vor(i,j,k,ntguessig)*(thetam1-thetap1)/(pm1-pp1)
              pvort(k) = abs(pv)
           end do
           pvort(1) = pvort(2)
           pvort(nsig) = pvort(nsig-1)
           
!          Locate tropopause
           ifound_pv=0; ifound_oz=0
           itrp_pv=nsig; itrp_oz=nsig

!          Search upward (decreasing pressure) for tropopause above sigma 0.7
           do k=2,nsig
              if ((prs(k)*(r0_001)*psi) < r0_7) then
!                Trop at level where pv=2e-6
                 if (pvort(k)>r2em6 .and. ifound_pv==0) then
                    ifound_pv=1
                    itrp_pv = k
                 endif
               
!                Trop at level where ozone greater than 3e-7
                 if (ges_oz(i,j,k,ntguessig)>r3em7 .and. ifound_oz==0) then
                    ifound_oz=1
                    itrp_oz = k
                 endif
              endif
           end do
           
!          Merge pv and ozone tropopause levels between 20 and 40 deg latitude
           if (slatd(i) >= r40 ) then
              itrop_k = itrp_pv
           elseif (slatd(i) >= r20) then
              wgt1 = (slatd(i)-r20)/r20
              itrop_k = wgt1*itrp_pv + (one-wgt1)*itrp_oz
           else
              itrop_k = itrp_oz
           endif
           itrop_k        = max(1,min(itrop_k,nsig))
           trop_pv(i,j)   = prs(itrp_pv)*r0_01 !hPa
           trop_oz(i,j)   = prs(itrp_oz)*r0_01 !hPa
           trop_pvoz(i,j) = prs(itrop_k)*r0_01 !hPa
        end do
     end do
  
!    Load tropopause pressure (hPa) into output array
     do j=1,lon2
        do i=1,lat2
           tropprs(i,j) = trop_pvoz(i,j)
        end do
     end do

! End of tropopause location method blocks     
  endif

! End of routine
  return
end subroutine tpause
