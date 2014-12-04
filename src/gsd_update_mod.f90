module gsd_update_mod
!$$$   module documentation block
!                .      .    .                                       .
! module:    gsd_update_mod module for updating surface, soil, moisture from GSD for RR
!   prgmmr: Hu              org: gsd                date: 2012-01-12
!
! abstract: module for updating surface, soil, moisture from GSD for RR
!
! program history log:
!   2012-01-12  parrish
!
! subroutines included:
!   sub gsd_update_soil_tq  - change surface and soil based on analysis increment
!   sub gsd_limit_ocean_q   - limits to analysis increments over oceans 
!   sub gsd_update_th2      - adjust 2-m t based on analysis increment
!   sub gsd_update_q2       - adjust 2-m q based on analysis increment
!
! Variable Definitions:

  use gsi_metguess_mod, only: GSI_MetGuess_Bundle
  use gsi_bundlemod, only: gsi_bundlegetpointer
  use mpeu_util, only: die
  implicit none

! set default to private
  private
! set subroutines to public
  public :: gsd_update_soil_tq
  public :: gsd_limit_ocean_q
  public :: gsd_update_th2
  public :: gsd_update_q2
! set passed variables to public

contains

subroutine gsd_update_soil_tq(tinc,is_t,qinc,is_q)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    update_surface    change surface and soil based on analysis increment
!   prgmmr: Hu          org: GSD                date: 2011-08-18
!
! abstract:  This routine does the following things:
!              1) add lowest level t increment to T2 
! 
! 
! program history log:
!   1990-10-06  parrish - original code
!   2013-10-19  todling - metguess now holds background
!
!   input argument list:
!    tinc : first level temperature analysis increment
!    qinc : first level moisture analysis increment
!
!   output argument list:
!
!   comments:
!
! attributes:
!$$$
  use kinds, only: r_kind,i_kind
  use mpimod, only: mype
  use jfunc, only:  tsensible,qoption
  use derivsmod, only: qsatg
  use constants, only: zero,one,fv,rd_over_cp_mass,one_tenth,deg2rad, rad2deg, pi
  use gridmod, only: lat2,lon2,nsig,aeta1_ll,pt_ll,nsig_soil
  use gridmod, only: regional_time
  use guess_grids, only: ges_tsen,isli,nfldsig,sno
  use wrf_mass_guess_mod, only: ges_xlon,ges_xlat
  use guess_grids, only: ges_prsl,nfldsig,ntguessig
  use rapidrefresh_cldsurf_mod, only: l_gsd_soilTQ_nudge

  implicit none

! Declare passed variables
  integer(i_kind) is_t,is_q
  real(r_kind),dimension(lat2,lon2), intent(in) :: tinc
  real(r_kind),dimension(lat2,lon2), intent(in) :: qinc

! Declare local variables
  real(r_kind),dimension(lat2,lon2) :: coast_prox
  real(r_kind),dimension(lat2,lon2) :: csza
  INTEGER(i_kind)  :: gmt,nday,iyear,imonth,iday
  REAL(r_kind)     :: declin
  real(r_kind)     :: hrang,xxlat
  real(r_kind)     :: sumqc

  logical ice
  integer(i_kind) :: iderivative
  real(r_kind),allocatable,dimension(:,:,:):: rhgues

  integer(i_kind) i,j,k,it,ier,istatus
  real(r_kind) :: ainc, tinct
  real(r_kind) :: coast_fac,temp,temp_fac,dts_min,tincf
  real(r_kind) :: snowthreshold
! 
  REAL(r_kind), pointer :: ges_qc(:,:,:)  ! cloud water
  REAL(r_kind), pointer :: ges_qi(:,:,:)  ! could ice
  real(r_kind),dimension(:,:  ),pointer:: ges_tsk   =>NULL()
  real(r_kind),dimension(:,:  ),pointer:: ges_soilt1=>NULL()
  real(r_kind),dimension(:,:,:),pointer:: ges_tslb  =>NULL()
  real(r_kind),dimension(:,:,:),pointer:: ges_smois =>NULL()
  real(r_kind),dimension(:,:,:),pointer:: ges_q     =>NULL()

  integer(i_kind) ico, ja,jb,ia,ib,nco,nip,jc,ic
  INTEGER(i_kind) :: itsig
  
!*******************************************************************************
!

  snowthreshold=1.0e-10_r_kind
  itsig=1
  ier=0
  call gsi_bundlegetpointer (GSI_MetGuess_Bundle(itsig),'ql',ges_qc,istatus);ier=ier+istatus
  call gsi_bundlegetpointer (GSI_MetGuess_Bundle(itsig),'qi',ges_qi,istatus);ier=ier+istatus
  if(ier/=0) then
     write(6,*) 'No cloud water and ice for soil nudging!'
     return ! no guess, nothing to do
  endif

!
!  isli = 0 water, =1 land, =2 sea ice (on water)
  if( l_gsd_soilTQ_nudge) then
     ico = 5
     do j=1,lon2
        ja = max(1   ,j-ico)
        jb = min(lon2,j+ico+1)
        do i=1,lat2
          coast_prox(i,j) = 0.

          if (isli(i,j,1)==1) then
             ia = max(1   ,i-ico)
             ib = min(lat2,i+ico+1)
             nco = 0
             nip = 0
             do jc=ja,jb
             do ic=ia,ib
                if (isli(ic,jc,1)==1) nco = nco+1
                nip = nip+1
             end do
             end do
             coast_prox(i,j) = float(nco)/float (nip)
          end if
        end do
     end do

!     write (6,*) 'Coast_prox values: ',mype,coast_prox(1,1)
  endif
                                                                              
!   calculation solar declination
! 
  iyear=regional_time(1)   
  imonth=regional_time(2)
  iday=regional_time(3)
  call w3fs13(iyear,imonth,iday,nday)
  declin=deg2rad*23.45_r_kind*sin(2.0_r_kind*pi*(284+nday)/365.0_r_kind)
!
!  csza = fraction of solar constant (cos of zenith angle)
   gmt = regional_time(4)   ! UTC 
   do j=2,lon2
     do i=1,lat2   
       hrang=15._r_kind*gmt*deg2rad + ges_xlon(i,j,1) - pi
       xxlat=ges_xlat(i,j,1)
       csza(i,j)=sin(xxlat)*sin(declin)                &
                +cos(xxlat)*cos(declin)*cos(hrang)
     end do
   end do

  if( l_gsd_soilTQ_nudge .and. is_t > 0) then
!     --------------------------------------------
! --- Increment top level of soil temp and snow temp
!       ONLY AT LAND POINTS according to
!       sfc temp increment.  
!     --------------------------------------------

! -- modifications to reintroduce soil temperature nudging
!   -- Stan and Tanya - 15 July 2004
!  -- allow cooling of soil only (not warming)
!      - allow only up to 1.0 K (half of negative ainc for temp)
!      - don't allow if snow water is > 6 mm

     do it=1,nfldsig
        ier=0
        call gsi_bundlegetpointer (GSI_MetGuess_Bundle(it),'tskn' ,ges_tsk   ,istatus)
        ier=ier+istatus
        call gsi_bundlegetpointer (GSI_MetGuess_Bundle(it),'tsoil',ges_soilt1,istatus)
        ier=ier+istatus
        call GSI_BundleGetPointer (GSI_MetGuess_Bundle(it),'smoist',ges_smois,istatus)
        ier=ier+istatus
        call GSI_BundleGetPointer (GSI_MetGuess_Bundle(it),'tslb' ,ges_tslb  ,istatus)
        ier=ier+istatus
        call gsi_bundlegetpointer (GSI_MetGuess_Bundle(it),'q'    ,ges_q     ,istatus)
        ier=ier+istatus
        if(ier/=0) then
!          code doesn't have to die here ... needs attention for generalization
           call die('gsd_update_soil_tq',': cannot find q in guess, ier=',ier)
        endif 
        do j=1,lon2
           do i=1,lat2
              if(tsensible) then
                 ainc=tinc(i,j)
              else
                 ainc=tinc(i,j)/(one+fv*ges_q(i,j,1))
              endif
              coast_fac = max(0._r_kind,(coast_prox(i,j)-0.5_r_kind))/0.5_r_kind
              temp = ges_tsen(i,j,1,it)

! *** Increase soil adjustment by factor of 2.0 if temps are 
!       25 deg C, and 2.5 if temp is 32.5 C .
!             -- Stan B. (John, Tanya) - 31 July 2006

              temp_fac  = 1._r_kind+min (1.5_r_kind,max(0._r_kind,(temp-283._r_kind)/15._r_kind))
              dts_min = -2._r_kind
! -- Allow soil temp cooling to be up to 2.5 * 0.6 = 1.5 X 2 C ( = 3K)
              dts_min = dts_min*temp_fac*0.6_r_kind

              IF (isli(i,j,it) == 1) THEN
                 tincf = ainc*temp_fac*coast_fac
                 if (sno(i,j,it) < snowthreshold) THEN
                    if(nsig_soil == 9) then
! - top level soil temp
                       ges_tslb(i,j,1) = ges_tslb(i,j,1) +   &
                                       min(1._r_kind,max(dts_min,tincf*0.6_r_kind)) 
! - 0-1 cm level -  soil temp
                       ges_tslb(i,j,2) = ges_tslb(i,j,2) +   &
                                       min(1._r_kind,max(dts_min,tincf*0.55_r_kind))
! - 1-4 cm level -  soil temp
                       ges_tslb(i,j,3) = ges_tslb(i,j,3) +   &
                                       min(1._r_kind,max(dts_min,tincf*0.4_r_kind))
! - 4-10 cm level -  soil temp
                       ges_tslb(i,j,4) = ges_tslb(i,j,4) +   &
                                       min(1._r_kind,max(dts_min,tincf*0.3_r_kind))
! - 10-30 cm level -  soil temp
                       ges_tslb(i,j,5) = ges_tslb(i,j,5) +   &
                                       min(1._r_kind,max(dts_min,tincf*0.2_r_kind))
                    else
! - top level soil temp
                       ges_tslb(i,j,1) = ges_tslb(i,j,1) +   &
                                       min(1._r_kind,max(dts_min,tincf*0.6_r_kind))
! - 0-5 cm level -  soil temp
                       ges_tslb(i,j,2) = ges_tslb(i,j,2) +   &
                                       min(1._r_kind,max(dts_min,tincf*0.4_r_kind))
! - 5-20 cm level -  soil temp
                       ges_tslb(i,j,3) = ges_tslb(i,j,3) +   &
                                       min(1._r_kind,max(dts_min,tincf*0.2_r_kind))
                    endif
                    ges_tsk(i,j) = ges_tsk(i,j) + min(1._r_kind,max(dts_min,tincf*0.6_r_kind))
                    ges_soilt1(i,j) = ges_soilt1(i,j) + min(1._r_kind,max(dts_min,tincf*0.6_r_kind))
                 else  ! if snow cover, then only adjust TSK and SOILT1
                    ges_tsk(i,j) = ges_tsk(i,j) + min(1._r_kind,max(-2._r_kind,tincf*0.6_r_kind))
                    ges_soilt1(i,j) = ges_soilt1(i,j) + min(1._r_kind,max(-2._r_kind,tincf*0.6_r_kind))
                    if (sno(i,j,it) > 32.0_r_kind) then
                       ges_tsk(i,j) = min(ges_tsk(i,j), 273.15_r_kind)
                       ges_soilt1(i,j) = min(ges_soilt1(i,j), 273.15_r_kind)
                    endif
                 endif ! sno(i,j,it) < snowthreshold
              endif   ! isli(i,j,it) == 1
           end do
        end do
     end do

!---------------------------------------------------------
!  Nudge soil moisture
!     Tanya S. and Stan B. - 21 July 2004 - first version
!---------------------------------------------------------

! Compute saturation specific humidity.

     iderivative = 0
     if(qoption == 1 )then
         iderivative = 1
     else
         iderivative = 2
     end if

     ice=.true.
     call genqsat(qsatg,ges_tsen(1,1,1,ntguessig),ges_prsl(1,1,1,ntguessig), &
                  lat2,lon2,nsig,ice,iderivative)
     allocate(rhgues(lat2,lon2,nsig))

     call gsi_bundlegetpointer (GSI_MetGuess_Bundle(ntguessig),'q',ges_q,istatus)
     if(istatus/=0) then
!       code doesn't have to die here ... needs attention for generalization
        call die('gsd_update_soil_tq',': cannot find q in guess')
     endif 
     do k=1,nsig
        do j=1,lon2
           do i=1,lat2
              rhgues(i,j,k)=ges_q(i,j,k)/qsatg(i,j,k)
           end do
        end do
     end do

     if( is_q > 0) then
     do it=1,nfldsig
        call gsi_bundlegetpointer (GSI_MetGuess_Bundle(it),'q',ges_q,istatus)
        ier=istatus
        call gsi_bundlegetpointer (GSI_MetGuess_Bundle(it),'smoist',ges_smois,istatus)
        ier=ier+istatus
        if(ier/=0) then
!          code doesn't have to die here ... needs attention for generalization
           call die('gsd_update_soil_tq',': cannot find q in guess')
        endif 
        do j=1,lon2
           do i=1,lat2
              if(tsensible) then
                 tinct=tinc(i,j)
              else
                 tinct=tinc(i,j)/(one+fv*ges_q(i,j,1))
              endif

              ainc=qinc(i,j)/qsatg(i,j,1)  ! analysis increment in RH

! -- use overall limits based on k level
              ainc = max(-0.3_r_kind,min(0.3_r_kind,ainc))

! -- When background is already dry and pRH increment
!      is negative (toward drier still), limit ainc further.
              if (rhgues(i,j,1) < 0.2_r_kind .and. ainc < 0.0_r_kind ) then
                  ainc=ainc*rhgues(i,j,1)/0.2_r_kind
              end if
              if (rhgues(i,j,1) < 0.4_r_kind .and. ainc < 0.0_r_kind ) then
                  ainc=ainc*rhgues(i,j,1)/0.4_r_kind
              end if

              ainc = max(-0.15_r_kind,min(0.15_r_kind,ainc))
!mhu test              ges_smois(i,j,4,it)=ges_smois(i,j,4,it)+ainc   ! test

! - Only do nudging over land, if daytime (defined as
!          cos of sun zenith angle > 0.1), and if
!          sfc temp increment is negative (meaning that
!          background sfc temp was too warm)

! -- some adjustments below to soil moisture adjustment,
!      which seems to have resulted in too much moistening
!      overall.  Stan B. - 24 Oct 04 - 04z


              if (isli(i,j,it) == 1 .and. csza(i,j) > 0.3_r_kind) then
                 sumqc=0
                 do k=1,nsig
                    sumqc=max(sumqc,max(ges_qc(i,j,k),ges_qi(i,j,k)))
                 enddo
                 sumqc=0  ! trun off cloud
                 if( sumqc < 1.0e-6_r_kind) then
                 if( sno(i,j,it) < snowthreshold ) then  ! don't do the 
                                                         ! moisture adjustment if there is snow     

                    if (tinct < -0.15_r_kind) then

! - top level soil moisture
! -- mod - 3/15/13
!      increase moistening from factor of 0.2 to 0.3
                       ges_smois(i,j,1) = min (max(ges_smois(i,j,1),ges_smois(i,j,2)), &
                                   ges_smois(i,j,1) + min(0.03_r_kind,max(0._r_kind,(ainc*0.2_r_kind))))
                       ges_smois(i,j,2) = min (max(ges_smois(i,j,2),ges_smois(i,j,3)), &
                                   ges_smois(i,j,2) + min(0.03_r_kind,max(0._r_kind,(ainc*0.2_r_kind))))
                       if(nsig_soil == 9) then
                          ges_smois(i,j,3) = min (max(ges_smois(i,j,3),ges_smois(i,j,4)), &
                                   ges_smois(i,j,3) + min(0.03_r_kind,max(0._r_kind,(ainc*0.2_r_kind))))
                          ges_smois(i,j,4) = min (max(ges_smois(i,j,4),ges_smois(i,j,5)), &  
                                   ges_smois(i,j,4) + min(0.03_r_kind,max(0._r_kind,(ainc*0.1_r_kind))))
                       endif
! -- above logic
!     7/26/04 - 
!       previously - min was sm1_p (level 2)
!       now   - min is (max of level 1 and level 2)
!       Implication - If level 1 was already more moist than
!       level 2, don't force level 1 SM back down to level 2.
! -- mod - 5/1/05
!      Decrease moistening from factor of 0.2 to 0.1
! -- mod - 3/15/13
!      increase moistening from factor of 0.1 to 0.3
                    endif

                    if (tinct >  0.15_r_kind) then
! - top level soil moisture
! -- addition 5/1/05
!     Now also dry soil if tinc is positive (warming)
!      and the RH_inc is negative.
                        ges_smois(i,j,1) = max(0.0_r_kind,ges_smois(i,j,1) + & 
                                                  max(-0.03_r_kind,min(0._r_kind,(ainc*0.2_r_kind))))
                        ges_smois(i,j,2) = max(0.0_r_kind,ges_smois(i,j,2) + & 
                                                  max(-0.03_r_kind,min(0._r_kind,(ainc*0.2_r_kind))))
                        if(nsig_soil == 9) then
                           ges_smois(i,j,3) = max(0.0_r_kind,ges_smois(i,j,3) + & 
                                                  max(-0.03_r_kind,min(0._r_kind,(ainc*0.2_r_kind))))
                           ges_smois(i,j,4) = max(0.0_r_kind,ges_smois(i,j,4) + & 
                                                  max(-0.03_r_kind,min(0._r_kind,(ainc*0.1_r_kind))))

                        endif
                    END IF
                 endif  !  sno(i,j,it) < snowthreshold
                 endif  !  sumqc < 1.0e-6_r_kind
              endif
           end do
        end do
     end do
     endif ! is_q > 0
     deallocate(rhgues)
  endif

  return
end subroutine gsd_update_soil_tq

subroutine gsd_limit_ocean_q(qinc)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    gsd_limit_ocean_q Rlimits to analysis increments over oceans
!   prgmmr: Hu          org: GSD                date: 2011-08-31
!
! abstract:  This routine does the following things:
! 
! 
! program history log:
!   2011-08-31  Hu - original code
!
!   input argument list:
!    qinc : moisture analysis increment
!
!   output argument list:
!
!   comments:
!
! attributes:
!$$$
  use kinds, only: r_kind,i_kind
  use mpimod, only: mype
  use jfunc, only:  qoption
  use derivsmod, only:  qsatg
  use constants, only: zero,one,fv,rd_over_cp_mass,one_tenth,deg2rad, rad2deg, pi
  use gridmod, only: lat2,lon2,nsig
  use guess_grids, only: ges_tsen,ges_prsl,nfldsig,ntguessig
  use guess_grids, only: isli
  
  implicit none
  
! Declare passed variables
  integer(i_kind) istatus
  real(r_kind),dimension(lat2,lon2,nsig), intent(inout) :: qinc

! Declare local variables
  logical ice
  integer(i_kind) :: i,j,k,iderivative,it
  real(r_kind),allocatable,dimension(:,:,:):: rhgues
  real(r_kind) :: qinc_rh
  real(r_kind),dimension(:,:,:),pointer:: ges_q=>NULL()


! Compute saturation specific humidity.   

  iderivative = 0
  if(qoption == 1 )then
      iderivative = 1 
  else
      iderivative = 2
  end if

  ice=.true.
  call genqsat(qsatg,ges_tsen(1,1,1,ntguessig),ges_prsl(1,1,1,ntguessig),lat2,lon2,nsig,ice,iderivative)
  allocate(rhgues(lat2,lon2,nsig))

  call gsi_bundlegetpointer (GSI_MetGuess_Bundle(ntguessig),'q',ges_q,istatus)
  if(istatus/=0) then
!    code doesn't have to die here ... needs attention for generalization
     call die('gsd_update_soil_tq',': cannot find q in guess')
  endif 
  do k=1,nsig
     do j=1,lon2
        do i=1,lat2
           rhgues(i,j,k)=ges_q(i,j,k)/qsatg(i,j,k)
        end do
     end do
  end do

  do it=1,nfldsig
     do k=1,nsig
        do j=1,lon2
           do i=1,lat2
              if (isli(i,j,1) ==  0) then
                 qinc_rh=qinc(i,j,k)/qsatg(i,j,k)
! -- limit pseudo-RH change over water to +/- 0.1
                 if( k <= 10) then
                    qinc_rh=max(-0.1_r_kind,min(0.1_r_kind,qinc_rh))
                 elseif(  k >= 11.and.k <=18 ) then
                    qinc_rh=max(-0.2_r_kind,min(0.2_r_kind,qinc_rh))
                 endif
! -- Limit further drying out over water and near surface.
                 if(rhgues(i,j,k) < 0.6_r_kind .and. k <=4 .and. qinc_rh < zero) then
                    qinc_rh=qinc_rh*rhgues(i,j,k)/1.0_r_kind
                 endif
                 qinc(i,j,k)=qinc_rh*qsatg(i,j,k) 
              else
                 qinc(i,j,k)=qinc(i,j,k) 
              end if   ! isli(i,j,1)
           end do
        end do
     end do
  end do

  deallocate(rhgues)
end subroutine gsd_limit_ocean_q 

subroutine gsd_update_th2(tinc)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    gsd_update_th2    adjust 2-m t based on analysis increment
!   prgmmr: Hu          org: GSD                date: 2011-10-04
!
! abstract:  This routine does the following things:
!              1) add lowest level t increment to T2 
! 
! 
! program history log:
!   2011-10-04  parrish - original code
!   2013-10-19  todling - get guess fileds from bundle
!
!   input argument list:
!    tinc : first level temperature analysis increment
!
!   output argument list:
!
!   comments:
!
! attributes:
!$$$
  use kinds, only: r_kind,i_kind
  use mpimod, only: mype
  use jfunc, only:  tsensible
  use constants, only: zero,one,fv,rd_over_cp_mass,one_tenth,deg2rad, rad2deg, pi
  use gridmod, only: lat2,lon2,nsig,aeta1_ll,pt_ll
  use guess_grids, only: nfldsig
  use guess_grids, only: ges_prsl,ntguessig

  implicit none

! Declare passed variables
  real(r_kind),dimension(lat2,lon2), intent(in) :: tinc

  real(r_kind),parameter:: r10=10.0_r_kind
  real(r_kind),parameter:: r100=100.0_r_kind
  integer(i_kind) i,j,it,ier,ihaveq
  real(r_kind) :: dth2, work_prsl,work_prslk

  real(r_kind),dimension(:,:  ),pointer:: ges_ps =>NULL()
  real(r_kind),dimension(:,:  ),pointer:: ges_th2=>NULL()
  real(r_kind),dimension(:,:,:),pointer:: ges_q  =>NULL()

!*******************************************************************************
!
! 2-m temperature
  do it=1,nfldsig
     call gsi_bundlegetpointer(gsi_metguess_bundle(it),'th2m',ges_th2,ier)
     if(ier/=0) cycle
     call gsi_bundlegetpointer(gsi_metguess_bundle(it),'ps',ges_ps,ier)
     if(ier/=0) cycle
! NOTE: for some odd reason the orig. code before bundle change was getting q
!       from slot it=1 - to preserve zero diff I left as such - RTodling
     call gsi_bundlegetpointer(gsi_metguess_bundle(1),'q' ,ges_q ,ihaveq)
!    call gsi_bundlegetpointer(gsi_metguess_bundle(it),'q' ,ges_q ,ihaveq)
     do j=1,lon2
        do i=1,lat2
           if(tsensible) then
              dth2=tinc(i,j)
           else
              if(ihaveq/=0) cycle
              dth2=tinc(i,j)/(one+fv*ges_q(i,j,1))
           endif
!          Convert sensible temperature to potential temperature
           work_prsl  = one_tenth*(aeta1_ll(1)*(r10*ges_ps(i,j)-pt_ll)+pt_ll)
           work_prslk = (work_prsl/r100)**rd_over_cp_mass
           ges_th2(i,j) = ges_th2(i,j) + dth2/work_prslk
        end do
     end do
  end do

  return
end subroutine gsd_update_th2

subroutine gsd_update_q2(qinc)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    gsd_update_q2    adjust 2-m q based on analysis increment
!   prgmmr: Hu          org: GSD                date: 2011-10-04
!
! abstract:  This routine does the following things:
!              1) add lowest level q increment to Q2 
! 
! 
! program history log:
!   2014-01-22  Hu - original code
!   2014-04-04  Todling - ges_q2 now in MetGuess
!
!   input argument list:
!    qinc : first level moisture analysis increment
!
!   output argument list:
!
!   comments:
!
! attributes:
!$$$
  use kinds, only: r_kind,i_kind
  use mpimod, only: mype
  use gridmod, only: lat2,lon2
  use guess_grids, only: nfldsig

  implicit none

! Declare passed variables
  real(r_kind),dimension(lat2,lon2), intent(in) :: qinc

  real(r_kind),dimension(:,:  ),pointer:: ges_q2=>NULL()
  integer(i_kind) i,j,it,ier

!*******************************************************************************
!
! 2-m temperature
  do it=1,nfldsig
     call gsi_bundlegetpointer(gsi_metguess_bundle(it),'q2m',ges_q2,ier)
     if(ier/=0) cycle
     do j=1,lon2
        do i=1,lat2
           ges_q2(i,j) = ges_q2(i,j) + qinc(i,j)
        end do
     end do
  end do

  return
end subroutine gsd_update_q2

end module gsd_update_mod
