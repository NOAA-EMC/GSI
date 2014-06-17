  module normal_rhtot_mod

!$$$ module documentation block
!           .      .    .                                       .
! module:   normal_rhtot_mod 
!   prgmmr: eliu  
!
! abstract: Convert normalized RH total increments into water vapor and hydrometeors
!
! program history log:
!   2011-08-01  eliu - initial code
!
! subroutines included:
!   sub normal_rhtot_tl 
!   sub normal_rhtot_ad 
!
! variable definitions:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

  use kinds,       only: r_kind,i_kind
  use constants,   only: zero,one,two,quarter,half,r0_05,r1000,t0c,fv
  use constants,   only: qmin,qcmin
  use derivsmod,   only: qsatg,rhtgues,cwgues,qgues,qtgues,qtdist_gues,cfgues 
  use derivsmod,   only: dqdrh,dqdp,dqdt,dqsdt,dqsdp
  use jfunc,       only: iter 
  use gridmod,     only: lat2,lon2,nsig,nlat,nlon,nnnn1o
  use gridmod,     only: istart,rbs2
  use guess_grids, only: ges_prsl,ges_tsen,fact_tv,ntguessig
  use mpimod,      only: mype
  implicit none

  PRIVATE 

  PUBLIC normal_rhtot_tl
  PUBLIC normal_rhtot_ad
  PUBLIC cw2hydro_beta_tl
  PUBLIC cw2hydro_beta_ad

  contains

  subroutine normal_rhtot_tl(rhtnorm,t,p,q,qc,tsen,pdf)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    normal_rhtot_tl  tlm for normalized RH total to q and hydrometeors 
!   prgmmr: eliu               org: np20                date: 2011-08-01
!
! abstract: split RH total increment into increments of water vapor and hydrometeors 
!
! program history log:
!   2011-08-01  eliu 
!
!   input argument list:
!      rhtnorm - normalized RH total increment
!      t       - virtual temperature increment
!      p       - pressure increment
!
!   output argument list:
!      q      - specific humidity increment
!      qc     - cloud liquid water mixing ratio increment
!      tsen   - sensible temperature
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$

  implicit none

! Declare input/output variables
  real(r_kind),intent(in   ) :: rhtnorm(lat2,lon2,nsig)
  real(r_kind),intent(in   ) ::       t(lat2,lon2,nsig)
  real(r_kind),intent(in   ) ::       p(lat2,lon2,nsig+1)  
  real(r_kind),intent(  out) ::       q(lat2,lon2,nsig)       
  real(r_kind),intent(  out) ::      qc(lat2,lon2,nsig)    
  real(r_kind),intent(  out) ::    tsen(lat2,lon2,nsig)    
  logical,     intent(in)    :: pdf 

! Declare local variables  
  integer(i_kind)            :: i,j,k,it
  real(r_kind), parameter    :: rh1    = 0.95_r_kind
  real(r_kind), parameter    :: rh2    = 1.05_r_kind
  real(r_kind), parameter    :: csplit = 0.50_r_kind
  real(r_kind), parameter    :: fact1  = rh1-csplit*rh1 
  real(r_kind), parameter    :: fact2  = rh1+csplit*(rh2-rh1) 
  real(r_kind)               :: qtot
  real(r_kind)               :: qx, xgues, xgues_tl

! Linearized about guess solution, so set it flag accordingly
  it=ntguessig

! Initialize TL output variables
  do i=1,lat2
     do j=1,lon2
        do k=1,nsig
           q(i,j,k)    = zero
           qc(i,j,k)   = zero
           tsen(i,j,k) = zero
        enddo
      enddo
  enddo

! If (mype==0)write(6,*)'normal_rhtot_tl: pdf = ', pdf
  if (pdf) then 

  do i=1,lat2
     do j=1,lon2
        do k=1,nsig

!         Initialize local TL variables
          qtot     = zero
          xgues_tl = zero

!         Calculate tsen increment from tv (assuming fixed qgues)
!!        ges_tsen(i,j,k) = ges_tv(i,j,k)*fact_tv(i,j,k)                         ! FW
          tsen(i,j,k)     = t(i,j,k)*fact_tv(i,j,k)                              ! TL

!         Calculate qtot increment
!!        qtgues(i,j,k) = rhtgues(i,j,k)*qsatg(i,j,k)                            ! FW
          qtot          = dqdrh(i,j,k)*rhtnorm(i,j,k) &                          ! TL
                         +dqdt(i,j,k)*tsen(i,j,k) &                              ! TL
                         -dqdp(i,j,k)*(p(i,j,k)+p(i,j,k+1))                      ! TL

!         Diagnose qc increment

          if (qgues(i,j,k) >= qsatg(i,j,k)) then ! RH>=1 saturation

!!           cwgues(i,j,k) = qtgues(i,j,k)-qsatg(i,j,k)                          ! FW    
             qc(i,j,k)     = qtot &                                              ! TL        
                            -dqsdt(i,j,k)*tsen(i,j,k) &                          ! TL    
                            +dqsdp(i,j,k)*(p(i,j,k)+p(i,j,k+1))                  ! TL   

          else ! RH<1

             qx=qtgues(i,j,k)-qsatg(i,j,k)    
             if (qtgues(i,j,k) > 2.0e-12) then 

                if (qx <= -qtdist_gues(i,j,k)) then  ! cfgues=0 no cloud    
!!                 cwgues(i,j,k) = zero                                          ! FW  
                   qc(i,j,k)     = zero                                          ! TL 
                else if (qx >= qtdist_gues(i,j,k)) then ! cfgues=1 saturation                             
!!                 cwgues(i,j,k) = qtgues(i,j,k)-qsatg(i,j,k)                    ! FW    
                   qc(i,j,k)     = qtot &                                        ! TL        
                                  -dqsdt(i,j,k)*tsen(i,j,k) &                    ! TL    
                                  +dqsdp(i,j,k)*(p(i,j,k)+p(i,j,k+1))            ! TL   
                else ! 0<cfgues<1 partly cloudy
!!                 xgues         = qtgues(i,j,k)+qtdist_gues(i,j,k)-qsatg(i,j,k) ! FW
                   xgues         = qtgues(i,j,k)+qtdist_gues(i,j,k)-qsatg(i,j,k) ! FW
                   xgues_tl      = qtot  &                                       ! TL
                                  -dqsdt(i,j,k)*tsen(i,j,k) &                    ! TL
                                  +dqsdp(i,j,k)*(p(i,j,k)+p(i,j,k+1))            ! TL 
!!                 cwgues(i,j,k) = (quarter/qtdist_gues(i,j,k))*xgues**two       ! FW
                   qc(i,j,k)     = half*(xgues/qtdist_gues(i,j,k))*xgues_tl      ! TL
                endif !

             else  ! qtgues<=2.0e-12

                if (qx > 0) then
!!                 cwgues(i,j,k) = qtgues(i,j,k)-qsatg(i,j,k)                    ! FW
                   qc(i,j,k)     = qtot &                                        ! TL
                                  -dqsdt(i,j,k)*tsen(i,j,k) &                    ! TL
                                  +dqsdp(i,j,k)*(p(i,j,k)+p(i,j,k+1))            ! TL
                else
!!                 cwgues(i,j,k) = zero                                          ! FW
                   qc(i,j,k)     = zero                                          ! TL

                endif  

             endif ! uniform distribution width (qtgues) condition

          endif ! RH condition 

!         Diagnose q increment
!!        qgues(i,j,k)  = qtgues(i,j,k)-cwgues(i,j,k)                            ! FW
          q(i,j,k)      = qtot-qc(i,j,k)                                         ! TL

       enddo
    enddo
  enddo

  else ! simple

  do i=1,lat2
     do j=1,lon2
        do k=1,nsig

!          Initialize local TL output variables
           qc(i,j,k)= zero                                                ! TL
           qtot     = zero                                                ! TL

!          Calculate tsen increment from tv (assuming fixed qgues)
!*         ges_tsen(i,j,k) = ges_tv(i,j,k)*fact_tv(i,j,k)                 ! FW
           tsen(i,j,k)     = t(i,j,k)*fact_tv(i,j,k)                      ! TL

!          Calculate qtot increment
!*         qtgues(i,j,k) = rhtgues(i,j,k)*qsatg(i,j,k)                    ! FW
           qtot          = dqdrh(i,j,k)*rhtnorm(i,j,k) &                  ! TL
                          +dqdt(i,j,k)*tsen(i,j,k) &                      ! TL
                          -dqdp(i,j,k)*(p(i,j,k)+p(i,j,k+1))              ! TL

!          Diagnose q increment
           if (rhtgues(i,j,k) < rh1) then
!*            qgues(i,j,k) = qtgues(i,j,k)                                ! FW
              q(i,j,k)     = qtot                                         ! TL
           else if (rhtgues(i,j,k) < rh2 .and. rhtgues(i,j,k) >= rh1) then
!*            qgues(i,j,k) = csplit*qtgues(i,j,k)+fact1*qsatg(i,j,k)      ! FW
              q(i,j,k)     = csplit*qtot &                                ! TL
                            +fact1*dqsdt(i,j,k)*tsen(i,j,k) &             ! TL
                            -fact1*dqsdp(i,j,k)*(p(i,j,k)+p(i,j,k+1))     ! TL
           else
!*            qgues(i,j,k) = fact2*qsatg(i,j,k)                           ! FW
              q(i,j,k)     = fact2*dqsdt(i,j,k)*tsen(i,j,k) &             ! TL
                            -fact2*dqsdp(i,j,k)*(p(i,j,k)+p(i,j,k+1))     ! TL
           endif

!          Diagnose qc increment
!*         cwgues(i,j,k) = qtgues(i,j,k)-qgues(i,j,k)                     ! FW
           qc(i,j,k)     = qtot-q(i,j,k)                                  ! TL

        enddo ! k-loop
     enddo ! j-loop
  enddo ! i-loop

  endif

  end subroutine normal_rhtot_tl

  subroutine normal_rhtot_ad(rhtnorm,t,p,q,qc,tsen,pdf)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    normal_rhtot_ad  adjoint of normal_rhtot_tl
!   prgmmr: eliu               org: np20                date: 2011-08-01
!
! abstract: adjoint of normal_rhtot_ad
!
! program history log:
!   2011-08-01  eliu 
!
!   input argument list:
!      q      - specific humidity increment
!      qc     - cloud liquid water mixing ratio increment 
!      tsen   - sensible temperature 
!
!   output argument list:
!      rhtnorm - normalized RH total increment
!      t       - virtual temperature increment
!      p       - pressure increment
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$ end documentation block

  implicit none

! Declare input/output variables
  real(r_kind),intent(inout) ::    tsen(lat2,lon2,nsig)         
  real(r_kind),intent(inout) ::       q(lat2,lon2,nsig)         
  real(r_kind),intent(inout) ::      qc(lat2,lon2,nsig)      
  real(r_kind),intent(inout) :: rhtnorm(lat2,lon2,nsig)    
  real(r_kind),intent(inout) ::       t(lat2,lon2,nsig)         
  real(r_kind),intent(inout) ::       p(lat2,lon2,nsig+1)
  logical,     intent(in)    :: pdf 
  
! Declare local variables
  integer(i_kind)            :: i,j,k,it
  real(r_kind), parameter    :: rh1    = 0.95_r_kind
  real(r_kind), parameter    :: rh2    = 1.05_r_kind
  real(r_kind), parameter    :: csplit = 0.50_r_kind
  real(r_kind), parameter    :: fact1  = rh1-csplit*rh1
  real(r_kind), parameter    :: fact2  = rh1+csplit*(rh2-rh1)
  real(r_kind)               :: qtot
  real(r_kind)               :: qx, xgues,xgues_ad

! Linearized about guess solution, so set it flag accordingly
  it=ntguessig

! If (mype==0)write(6,*)'normal_rhtot_ad: pdf = ', pdf
  if (pdf) then
  do i=1,lat2
     do j=1,lon2
        do k=1,nsig

!          Initialize local adjoint variable
           qtot      = zero
           xgues_ad  = zero

!          Diagnose q increment
!!         q(i,j,k)  = qtot-qc(i,j,k)                                      ! TL
           qtot      = qtot+q(i,j,k)                                       ! AD
           qc(i,j,k) = qc(i,j,k)-q(i,j,k)                                  ! AD
           q(i,j,k)  = zero                                                ! AD

!          Diagnose qc increment

           if (qgues(i,j,k) >= qsatg(i,j,k)) then ! RH>=1 saturation

!!             qc(i,j,k)      = qtot &                                     ! TL
!!                             -dqsdt(i,j,k)*tsen(i,j,k) &                 ! TL
!!                             +dqsdp(i,j,k)*(p(i,j,k)+p(i,j,k+1))         ! TL
               qtot           = qtot+qc(i,j,k)                             ! AD
               tsen(i,j,k)    = tsen(i,j,k)-dqsdt(i,j,k)*qc(i,j,k)         ! AD
               p(i,j,k)       = p(i,j,k)   +dqsdp(i,j,k)*qc(i,j,k)         ! AD
               p(i,j,k+1)     = p(i,j,k+1) +dqsdp(i,j,k)*qc(i,j,k)         ! AD
               qc(i,j,k)      = zero                                       ! AD

           else ! RH<1

              qx=qtgues(i,j,k)-qsatg(i,j,k)                                            ! FW
              if (qtgues(i,j,k) > 2.0e-12) then
                 if (qx <= -qtdist_gues(i,j,k)) then  ! cfgues=0 no cloud 
!!                  qc(i,j,k)     = zero                                               ! TL
                    qc(i,j,k)     = zero                                               ! AD 
                 else if (qx >= qtdist_gues(i,j,k)) then  ! cfgues=1 saturation
!!                  qc(i,j,k)     = qtot &                                             ! TL         
!!                                 -dqsdt(i,j,k)*tsen(i,j,k) &                         ! TL    
!!                                 +dqsdp(i,j,k)*(p(i,j,k)+p(i,j,k+1))                 ! TL
                    qtot          = qtot+qc(i,j,k)                                     ! AD
                    tsen(i,j,k)   = tsen(i,j,k)-dqsdt(i,j,k)*qc(i,j,k)                 ! AD
                    p(i,j,k)      = p(i,j,k)   +dqsdp(i,j,k)*qc(i,j,k)                 ! AD
                    p(i,j,k+1)    = p(i,j,k+1) +dqsdp(i,j,k)*qc(i,j,k)                 ! AD
                    qc(i,j,k)     = zero                                               ! AD
                 else ! 0<cfgues<1 partly cloudy                             
                    xgues         = qtgues(i,j,k)+qtdist_gues(i,j,k)-qsatg(i,j,k)      ! FW 
!!                  qc(i,j,k)     = half*(xgues/qtdist_gues(i,j,k))*xgues_tl           ! TL         
                    xgues_ad      = xgues_ad+half*(xgues/qtdist_gues(i,j,k))*qc(i,j,k) ! AD                 
                    qc(i,j,k)     = zero                                               ! AD     
!!                  xgues_tl      = qtot  &                                            ! TL
!!                                 -dqsdt(i,j,k)*tsen(i,j,k) &                         ! TL
!!                                 +dqsdp(i,j,k)*(p(i,j,k)+p(i,j,k+1))                 ! TL
                    qtot          = qtot+xgues_ad                                      ! AD
                    tsen(i,j,k)   = tsen(i,j,k)-dqsdt(i,j,k)*xgues_ad                  ! AD
                    p(i,j,k)      = p(i,j,k)   +dqsdp(i,j,k)*xgues_ad                  ! AD
                    p(i,j,k+1)    = p(i,j,k+1) +dqsdp(i,j,k)*xgues_ad                  ! AD
                    xgues_ad      = zero                                               ! AD
                 endif !
              else  ! qtgues<=1.0e-12
                 if (qx > 0) then
!!                  qc(i,j,k)     = qtot &                                             ! TL              
!!                                 -dqsdt(i,j,k)*tsen(i,j,k) &                         ! TL      
!!                                 +dqsdp(i,j,k)*(p(i,j,k)+p(i,j,k+1))                 ! TL     
                    qtot          = qtot+qc(i,j,k)                                     ! AD       
                    tsen(i,j,k)   = tsen(i,j,k)-dqsdt(i,j,k)*qc(i,j,k)                 ! AD         
                    p(i,j,k)      = p(i,j,k)   +dqsdp(i,j,k)*qc(i,j,k)                 ! AD         
                    p(i,j,k+1)    = p(i,j,k+1) +dqsdp(i,j,k)*qc(i,j,k)                 ! AD         
                    qc(i,j,k)     = zero                                               ! AD     
                 else
!!                  qc(i,j,k)     = zero                                               ! TL
                    qc(i,j,k)     = zero                                               ! AD 
                 endif
              endif ! uniform distribution width (qtgues) condition

           endif ! RH condition

!          Calculate qtot increment
!!         qtot           = dqdrh(i,j,k)*rhtnorm(i,j,k) &                  ! TL
!!                         +dqdt(i,j,k)*tsen(i,j,k) &                      ! TL
!!                         -dqdp(i,j,k)*(p(i,j,k)+p(i,j,k+1))              ! TL
           rhtnorm(i,j,k) = rhtnorm(i,j,k)+dqdrh(i,j,k)*qtot               ! AD
           tsen(i,j,k)    = tsen(i,j,k)   + dqdt(i,j,k)*qtot               ! AD
           p(i,j,k     )  = p(i,j,k)      - dqdp(i,j,k)*qtot               ! AD
           p(i,j,k+1)     = p(i,j,k+1)    - dqdp(i,j,k)*qtot               ! AD
           qtot           = zero

!          Calculate tsen increment from tv (assuming fixed qgues)
!!         tsen           = t(i,j,k)*fact_tv(i,j,k)                        ! TL
           t(i,j,k)       = t(i,j,k)+fact_tv(i,j,k)*tsen(i,j,k)            ! AD
           tsen(i,j,k)    = zero                                           ! AD

        enddo
     enddo
  enddo

  else  ! simple
 
  do i=1,lat2
     do j=1,lon2
        do k=1,nsig

!          Initialize local adjoint variable
           qtot      = zero                                                ! AD

!          Diagnose qc increment
!!         qc(i,j,k) = qtot-q(i,j,k)                                       ! TL
           qtot      = qtot+qc(i,j,k)                                      ! AD
           q(i,j,k)  = q(i,j,k)-qc(i,j,k)                                  ! AD
           qc(i,j,k) = zero                                                ! AD

!          Diagnose q increment
           if (rhtgues(i,j,k) < rh1) then
!!            q(i,j,k)    = qtot                                           ! TL
              qtot        = qtot+q(i,j,k)                                  ! AD
              q(i,j,k)    = zero                                           ! AD
           else if (rhtgues(i,j,k) < rh2 .and. rhtgues(i,j,k) >= rh1) then
!!            q(i,j,k)      = csplit*qtot &                                ! TL
!!                           +fact1*dqsdt(i,j,k)*tsen(i,j,k) &             ! TL
!!                           -fact1*dqsdp(i,j,k)*(p(i,j,k)+p(i,j,k+1))     ! TL
	      qtot          = qtot         +csplit*q(i,j,k)                ! AD    
              tsen(i,j,k)   = tsen(i,j,k)  +fact1*dqsdt(i,j,k)*q(i,j,k)    ! AD
              p(i,j,k)      = p(i,j,k)     -fact1*dqsdp(i,j,k)*q(i,j,k)    ! AD 
              p(i,j,k+1)    = p(i,j,k+1)   -fact1*dqsdp(i,j,k)*q(i,j,k)    ! AD 
              q(i,j,k)      = zero
           else
!!            q(i,j,k     ) = fact2*dqsdt(i,j,k)*tsen(i,j,k) &             ! TL
!!                           -fact2*dqsdp(i,j,k)*(p(i,j,k)+p(i,j,k+1))     ! TL
	      tsen(i,j,k)   = tsen(i,j,k)  +fact2*dqsdt(i,j,k)*q(i,j,k)    ! AD
              p(i,j,k)      = p(i,j,k)     -fact2*dqsdp(i,j,k)*q(i,j,k)    ! AD
              p(i,j,k+1)    = p(i,j,k+1)   -fact2*dqsdp(i,j,k)*q(i,j,k)    ! AD
              q(i,j,k)      = zero
           endif  
                                                                                               
!          Calculate qtot increment
!!         qtot           = dqdrh(i,j,k)*rhtnorm(i,j,k)           &        ! TL
!!                         +dqdt(i,j,k)*tsen(i,j,k)               &        ! TL
!!                         -dqdp(i,j,k)*(p(i,j,k)+p(i,j,k+1))              ! TL
           rhtnorm(i,j,k) = rhtnorm(i,j,k)+dqdrh(i,j,k)*qtot               ! AD
           tsen(i,j,k)    = tsen(i,j,k)   + dqdt(i,j,k)*qtot               ! AD
           p(i,j,k)       = p(i,j,k)      - dqdp(i,j,k)*qtot               ! AD
           p(i,j,k+1)     = p(i,j,k+1)    - dqdp(i,j,k)*qtot               ! AD
           qtot           = zero

!          Calculate tsen increment from tv (assuming fixed qgues)
!!         tsen(i,j,k)     = t(i,j,k)*fact_tv(i,j,k)                       ! TL
           t(i,j,k)        = t(i,j,k)+fact_tv(i,j,k)*tsen(i,j,k)           ! AD
           tsen(i,j,k)     = zero                                          ! AD

        enddo ! k-loop
     enddo ! j-loop
  enddo ! i-loop

  endif ! pdf
 
  end subroutine normal_rhtot_ad

  subroutine cw2hydro_beta_tl(qc,tsen,ql,qi)

! Declare input/output variables
  real(r_kind),intent(in   ) ::    qc(lat2,lon2,nsig)
  real(r_kind),intent(in   ) :: tsen(lat2,lon2,nsig)
  real(r_kind),intent(  out) ::   ql(lat2,lon2,nsig)
  real(r_kind),intent(  out) ::   qi(lat2,lon2,nsig)

! Declare local variables
  integer(i_kind):: i,j,k,it 
  real(r_kind), dimension(lat2,lon2,nsig) :: work0,work

! Split total cloud condensates (qc) into cloud liquid water (ql) and cloud ice (qi)
! if (mype==0)write(6,*)'cw2hydro_beta_tl: do_qc_to_hydro'
  it=ntguessig
  do i=1,lat2
     do j=1,lon2
        do k=1,nsig
           ql(i,j,k)=zero
           qi(i,j,k)=zero
        enddo
     enddo
  enddo
  do k=1,nsig
    do j=1,lon2
       do i=1,lat2
          work0(i,j,k) =-r0_05*(ges_tsen(i,j,k,it)-t0c)
          work0(i,j,k) = max(zero,work0(i,j,k))
          work0(i,j,k) = min(one,work0(i,j,k))
          work(i,j,k)  =-r0_05*tsen(i,j,k)
          if (work0(i,j,k)<=zero) work(i,j,k)=zero
          if (work0(i,j,k)>=one)  work(i,j,k)=zero
          ql(i,j,k)=qc(i,j,k)*(one-work0(i,j,k))-cwgues(i,j,k)*work(i,j,k)
          qi(i,j,k)=qc(i,j,k)*work0(i,j,k)+cwgues(i,j,k)*work(i,j,k)
        end do
     end do
  end do
  end subroutine cw2hydro_beta_tl 

  subroutine cw2hydro_beta_ad(qc,tsen,ql,qi)

! Declare input/output variables
  real(r_kind),intent(inout) ::    qc(lat2,lon2,nsig)
  real(r_kind),intent(inout) :: tsen(lat2,lon2,nsig)
  real(r_kind),intent(inout) ::   ql(lat2,lon2,nsig)
  real(r_kind),intent(inout) ::   qi(lat2,lon2,nsig)

! Declare local variables
  integer(i_kind):: i,j,k,it
  real(r_kind)   :: work
  real(r_kind), dimension(lat2,lon2,nsig) :: work0

! Split cloud water (qc) into cloud liquid water (ql) and ice water (qi)
! if (mype==0)write(6,*)'normal_rhtot_simple_to_q_hydro_ad: do_qc_to_hydro'
  it=ntguessig
  do k=1,nsig
     do j=1,lon2
        do i=1,lat2
           qc(i,j,k)=zero
        enddo
     enddo
  enddo
  do k=1,nsig
     do j=1,lon2
        do i=1,lat2
           work0(i,j,k) =-r0_05*(ges_tsen(i,j,k,it)-t0c)                     ! FW
           work0(i,j,k) = max(zero,work0(i,j,k))                             ! FW
           work0(i,j,k) = min(one,work0(i,j,k))                              ! FW

!*         ql(i,j,k) = qc(i,j,k)*(one-work0(i,j,k))-cwgues(i,j,k)*work(i,j,k)! TL
           work      = zero                                                  ! AD
           work      = work-cwgues(i,j,k)*ql(i,j,k)                          ! AD
           qc(i,j,k) = qc(i,j,k)+(one-work0(i,j,k))*ql(i,j,k)                ! AD
           ql(i,j,k) = zero                                                  ! AD

!*         qi(i,j,k) = qc(i,j,k)*work0(i,j,k)+cwgues(i,j,k)*work(i,j,k)      ! TL
           work      = work+cwgues(i,j,k)*qi(i,j,k)                          ! AD
           qc(i,j,k) = qc(i,j,k)+work0(i,j,k)*qi(i,j,k)                      ! AD
           qi(i,j,k) = zero                                                  ! AD

!*         if (work0(i,j,k)<=zero) work(i,j,k)=zero                          ! TL
!*         if (work0(i,j,k)>=one)  work(i,j,k)=zero                          ! TL
           if (work0(i,j,k)<=zero) work=zero                                 ! TL
           if (work0(i,j,k)>=one)  work=zero                                 ! TL

!*         work(i,j,k) = -r0_05*tsen(i,j,k)                                  ! TL
           tsen(i,j,k) = tsen(i,j,k)-r0_05*work                              ! AD
           work        = zero                                                ! AD
        enddo
     enddo
  enddo

  end subroutine cw2hydro_beta_ad 

end module normal_rhtot_mod
