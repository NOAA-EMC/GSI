SUBROUTINE cloud_saturation(mype,nlat,nlon,nsig,q_bk,t_bk,p_bk, &
                 cld_cover_3d,wthr_type,  &
                 cldwater_3d,cldice_3d)
!
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:  cloud_saturation  to ensure water vapor saturation at all cloudy grid points
!                               also to ensure sub saturation in clear point
!
!   PRGMMR: Ming Hu          ORG: GSD/AMB        DATE: 2006-11-20
!
! ABSTRACT: 
!  This subroutine calculate liquid water content for stratiform cloud
!
! PROGRAM HISTORY LOG:
!    2010-10-06  Hu  check whole 3D mositure field and get rid of supersaturation
!    2009-01-20  Hu  Add NCO document block
!
!
!   input argument list:
!     nlon        - no. of lons on subdomain (buffer points on ends)
!     nlat        - no. of lats on subdomain (buffer points on ends)
!     nsig        - no. of levels
!     q_bk        - 3D moisture
!     t_bk        - 3D background potential temperature (K)
!     p_bk        - 3D background pressure  (hPa)
!     cldwater_3d - 3D analysis cloud water mixing ratio (g/kg)
!     cldice_3d   - 3D analysis cloud ice mixing ratio (g/kg)
!     cld_cover_3d- 3D cloud cover
!     wthr_type   - 3D weather type
!
!   output argument list:
!     q_bk        - 3D moisture
!
! USAGE:
!   INPUT FILES: 
!
!   OUTPUT FILES:
!
! REMARKS:
!
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 90 
!   MACHINE:  Linux cluster (WJET)
!
!$$$
!
!_____________________________________________________________________
!

  use constants, only: rd_over_cp, h1000,one,zero
  use kinds, only: r_single,i_kind, r_kind

  implicit none

  integer(i_kind),intent(in):: mype
  integer(i_kind),intent(in):: nlat,nlon,nsig
!
!  background
!
  real(r_single),intent(in)    :: t_bk(nlon,nlat,nsig)   ! potential temperature (K)
  real(r_single),intent(inout) :: q_bk(nlon,nlat,nsig)   ! mixing ratio (kg/kg)
  real(r_single),intent(in)    :: p_bk(nlon,nlat,nsig)   ! pressure  (hpa)
!
!  Variables for cloud analysis
!
  real (r_single),intent(in) :: cld_cover_3d(nlon,nlat,nsig) 
  integer(i_kind),intent(in) :: wthr_type(nlon,nlat)   
!
! cloud water and cloud ice
!
  real (r_single),intent(in) :: cldwater_3d(nlon,nlat,nsig) ! kg/kg
  real (r_single),intent(in) :: cldice_3d(nlon,nlat,nsig)   ! kg/kg
!-----------------------------------------------------------
!
! temp.
!
  INTEGER(i_kind) :: i,j,k,ilvl,nlvl
  INTEGER(i_kind) :: kb,kt,k1
  real(r_single) :: p_pa_1d(nsig), thv(nsig)
  real(r_single) :: cloudqvis,cloudqvis2

! --- Key parameters
!     Rh_clear_p        = 0.80          RH to use when clearing cloud

  real(r_single)    rh_cld3_p
  real(r_single)    rh_clear_p
  data  rh_cld3_p         /0.98_r_single/    ! mhu, do we need to adjust this number to 0.94, WPP has PBL top set as 0.95
  data  rh_clear_p        /0.8_r_single/

  real(r_kind) ::  es0_p
  parameter (es0_p=6.1121_r_kind)     ! saturation vapor pressure (mb)
  real(r_kind) SVP1,SVP2,SVP3
  data SVP1,SVP2,SVP3/es0_p,17.67_r_kind,29.65_r_kind/

  INTEGER(i_kind) :: kp3,km3

  REAL(r_kind) :: qv, Temp, evs, qvs1, eis, qvi1, watwgt
!
!====================================================================
!  Begin
!
!
  DO j = 2,nlat-1
    DO i = 2,nlon-1
      DO k = 2,nsig-1

        p_pa_1d(k) = p_bk(i,j,k)*100.0_r_single
        qv= q_bk(i,j,k)/(one+q_bk(i,j,k))     !  qv = water vapor specific humidity
                                              !  q_bk = water vapor mixing ratio
! now, tmperature from GSI s potential temperature. get temperature
        Temp = t_bk(i,j,k)*(p_bk(i,j,k)/h1000)**rd_over_cp
! evs, eis in mb
!   For this part, must use the water/ice saturation as f(temperature)
        evs = svp1*exp(SVP2*(Temp-273.15_r_kind)/(Temp-SVP3))
        qvs1 = 0.62198_r_kind*evs/(p_bk(i,j,k)-evs)  ! qvs1 is mixing ratio kg/kg
                                                     !  so no need next line
!      qvs1 = qvs1/(1.0-qvs1)
!   Get ice saturation and weighted ice/water saturation ready to go
!    for ensuring cloud saturation below.
        eis = svp1 *exp(22.514_r_kind - 6.15e3_r_kind/Temp)
        qvi1 = 0.62198_r_kind*eis/(p_bk(i,j,k)-eis)  ! qvi1 is mixing ratio kg/kg,
                                                     ! so no need next line
!      qvi1 = qvi1/(1.0-qvi1)
!      watwgt = max(0.,min(1.,(Temp-233.15)/(263.15-233.15)))
        watwgt = max(zero,min(one,(Temp-251.15_r_kind)/&
                         (263.15_r_kind-251.15_r_kind)))
!        watwgt = max(zero,min(one,(Temp-263.15_r_kind)/&
!                         (268.15_r_kind-263.15_r_kind)))
        cloudqvis= (watwgt*qvs1 + (one-watwgt)*qvi1)  !  kg/kg
!
! moisture adjustment based on cloud
!

!
! check each grid point to make sure no supersaturation
!
        q_bk(i,j,k) = min(q_bk(i,j,k), cloudqvis * 1.00_r_single)
!
        if(cld_cover_3d(i,j,k) > -0.0001_r_kind .and.           &
           cld_cover_3d(i,j,k) < 2.0_r_kind) then
          if(cld_cover_3d(i,j,k) <= 0.0001_r_kind) then
! adjust RH to be below 85 percent(50%?) if
!     1) cloudyn = 0
!     2) at least 100 mb above sfc
!     3) no precip from sfc obs
!make sure that clear volumes are no more than rh_clear_p RH.
            if( (cldwater_3d(i,j,k)+cldice_3d(i,j,k))>0.0_r_kind .and.  &
                (p_pa_1d(1) - p_pa_1d(k))>10000._r_kind  .and.  &
                 wthr_type(i,j) <=0 ) then 
                  q_bk(i,j,k) = min(q_bk(i,j,k), cloudqvis * rh_clear_p)
            endif
!C  - moisten layers above and below cloud layer
            if(cld_cover_3d(i,j,k+1) > 0.6_r_kind .or.          &
               cld_cover_3d(i,j,k-1) > 0.6_r_kind  ) then
                    q_bk(i,j,k) = q_bk(i,j,k) +                 &
                         0.7_r_single*(max(0.0_r_single, cloudqvis-q_bk(i,j,k)))
            endif
! -- If SCT/FEW present, reduce RH only down to rh_cld3_p (0.98)
!         corresponding with cloudyn=3
          elseif(cld_cover_3d(i,j,k) > 0.0001_r_kind .and.      &
                 cld_cover_3d(i,j,k) < 0.6_r_kind ) then
             q_bk(i,j,k) = min(q_bk(i,j,k), cloudqvis * rh_cld3_p)
!
          else   ! set qv at 102%RH
             q_bk(i,j,k) = max(q_bk(i,j,k), cloudqvis * 1.02_r_single)
          endif
        else    ! cloud cover is missing
!  Ensure saturation in all cloudy volumes.
!  Since saturation has already been ensured for new cloudy areas (cld_cover_3d > 0.6)
!  we now ensure saturation for all cloud 3-d points, whether cloudy from background
!   (and not changed - cld_cover_3d < 0)
          cloudqvis2 = min (cloudqvis, 0.018_r_single)  ! Limit new water vapor mixing ratio
                                                     ! in cloud to 18 g/kg
          if ((cldwater_3d(i,j,k)+cldice_3d(i,j,k))>1.0e-5_r_kind) &
                   q_bk(i,j,k) = max(cloudqvis2,q_bk(i,j,k))
        endif   
!
! check each grid point to make sure no supersaturation
!
        q_bk(i,j,k) = min(q_bk(i,j,k), cloudqvis * 1.00_r_single)

      enddo ! k
    enddo   ! i
  enddo     ! j

END SUBROUTINE cloud_saturation   

