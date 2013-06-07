module gfs_moistphys_mod

!$$$ module documentation block
!           .      .    .                                       .
! module:   gfs_moistphys_mod 
!   prgmmr: eliu
!
! abstract: Tangent linear and adjoint models of GFS moisture physics 
!           Three moisture physic processes are included:
!           (1) Deep/Shallow simplified SAS scheme (not included yet)
!           (2) Grid scale condensation/evaporation processes
!           (3) Large scale precipitation processes 
!           Also see pcp_k.f90 for reference
!
! program history log:
!   2011-12-28  eliu - initial code
!
! subroutines included:
!   sub moistphys_tl
!   sub moistphys_ad
!
! variable definitions:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

  use kinds,       only: r_kind,i_kind
  use constants,   only: zero,one,two,quarter,half,r0_05,r1000,t0c,fv,max_varname_length
  use constants,   only: rhctop,rhcbot,dx_min,dx_inv
  use constants,   only: qmin,qcmin
  use jfunc,       only: qsatg,cwgues,qgues,sl,del_si
  use jfunc,       only: tgs,qgs,cwgs                               
  use jfunc,       only: tlrg,qlrg,cwlrg,rnlrg                     
  use gridmod,     only: lat2,lon2,nsig,nlat,nlon,nnnn1o
  use gridmod,     only: istart,rbs2
  use guess_grids, only: ges_tsen,ges_q,ges_tv,fact_tv,ges_ps,ntguessig
  use guess_grids, only: ges_tv_ten,ges_tsen_ten,ges_q_ten,ges_prs_ten
  use mpimod,      only: mype
  use pcpinfo,     only: deltim,dtphys

  implicit none

  real(r_kind),parameter :: r0_99=0.99_r_kind

  PRIVATE
  PUBLIC moistphys_tl
  PUBLIC moistphys_ad

contains

subroutine moistphys_tl(tsen,q,cw)

!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    moistphys_tl  tlm for GFS moisture physics 
!   prgmmr: eliu               org: np20                date: 2011-12-28
!
! abstract: 
!
! program history log:
!   2011-12-28  eliu
!
!   input argument list:
!      tsen - sensible temperature 
!      q    - specific humidity
!      cw   - cloud water
!   output argument list:
!      tsen - sensible temperature 
!      q    - specific humidity
!      cw   - cloud water
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$

  implicit none

! Declare passed variables
  real(r_kind),dimension(lat2,lon2,nsig),intent(inout):: tsen,q,cw 

! Declare local variables
  integer(i_kind):: im,ix,it
  integer(i_kind):: i,j,k,ii,mm1
  integer(i_kind):: nsphys

  real(r_kind),dimension(lat2,lon2,nsig):: tsen_in,  q_in, cw_in
  real(r_kind),dimension(lat2,lon2,nsig):: tsen_out, q_out,cw_out

  real(r_kind)::rcl,rcs,tem,work1,work2
  real(r_kind)::dtp
  real(r_kind):: ges_ps_i
  real(r_kind),dimension(nsig):: rhc,sl_i,del_si_i
  real(r_kind),dimension(nsig):: ges_tsen_i,ges_q_i,ges_cw_i
  real(r_kind),dimension(nsig):: ges_tsen_o,ges_q_o,ges_cw_o
  real(r_kind),dimension(nsig):: ges_tsen_ten_i,ges_q_ten_i,ges_prs_ten_i
! gscond
  real(r_kind),dimension(nsig):: tsen_ten_i,q_ten_i,p_ten_i 
  real(r_kind),dimension(nsig):: del_tgs_i,del_qgs_i,del_cwgs_i
  real(r_kind),dimension(nsig):: del_tgs_o,del_qgs_o,del_cwgs_o
  real(r_kind),dimension(nsig):: tgs_o,qgs_o,cwgs_o
! precip
  real(r_kind),dimension(nsig):: del_tlrg_i,del_qlrg_i,del_cwlrg_i
  real(r_kind),dimension(nsig):: del_tlrg_o,del_qlrg_o,del_cwlrg_o
  real(r_kind),dimension(nsig):: tlrg_o,qlrg_o,cwlrg_o
  real(r_kind):: del_rnlrg_i,del_rnlrg_o,rnlrg_o
  

!--------------------------------------------------------------------------------------------

! Initialize I/O variables
  do k=1,nsig
     do j=1,lon2
        do i=1,lat2
           tsen_in(i,j,k)  = tsen(i,j,k)      
              q_in(i,j,k)  =    q(i,j,k)      
             cw_in(i,j,k)  =   cw(i,j,k)      
           tsen_out(i,j,k) = zero      
              q_out(i,j,k) = zero      
             cw_out(i,j,k) = zero      
        enddo
     enddo
  enddo

! Linearized about guess solution, so set it flag accordingly
  it=ntguessig

  nsphys = max(int(two*deltim/dtphys+0.9999_r_kind),1)
  dtp    = two*deltim/nsphys
  mm1    = mype+1
  im     = 1 
  ix     = 1 

! Loop through latitudes
  do i=1,lat2
     ii    = i+istart(mm1)-2
     ii    = max(1,min(ii,nlat))
     rcs   = sqrt(rbs2(ii))
     tem   = (rhctop-rhcbot)/(nsig-one)
     work1 = (log(one/(rcs*nlon))-dx_min)*dx_inv
     work1 = max(zero,min(one,work1))
     work2 = one - work1
     do k=1,nsig
        rhc(k) = rhcbot + tem*(k-1)
        rhc(k) = r0_99*work1 + rhc(k)*work2
        rhc(k) = max(zero,min(one,rhc(k)))
     enddo

!    Loop through longitudes
     do j=1,lon2

!       if (mype==0 .and. i*j==1) write(6,*) 'gfs_moistphys_tl: do gfs_gscond_tl '
!       GFS grid-scale condensation/evaporation processes
!       Initialize TL I/O
        ges_ps_i = ges_ps(i,j,it)
        do k = 1, nsig
!          Initialize TL input
!          Guess profiles (input) --- from outer loop   
           sl_i(k)       = sl(i,j,k)
           ges_tsen_i(k) = ges_tsen(i,j,k,it)       
           ges_q_i(k)    = qgues(i,j,k)
           ges_cw_i(k)   = cwgues(i,j,k)

!          Guess tendencies (input) --- from outer loop
           ges_tsen_ten_i(k) = ges_tsen_ten(i,j,k)  
           ges_q_ten_i(k)    = ges_q_ten(i,j,k)
           ges_prs_ten_i(k)  = ges_prs_ten(i,j,k+1) ! note : k=1 is surface pressure tendency   

!          Increments (input)
           del_tgs_i(k)  = tsen_in(i,j,k)
           del_qgs_i(k)  = q_in(i,j,k)
           del_cwgs_i(k) = cw_in(i,j,k)

!          Increments for tendencies (input)
!          Set to zero for now
           tsen_ten_i(k) = zero
           p_ten_i(k)    = zero
           q_ten_i(k)    = zero

!          Initialize output
           tgs_o(k)   = zero
           qgs_o(k)   = zero
           cwgs_o(k)  = zero
           del_tgs_o(k)  = zero
           del_qgs_o(k)  = zero
           del_cwgs_o(k) = zero
        enddo

!       Run TL of GFS condensation/evaporation processes for a vertical grid column
        call gfs_gscond_tl(im,ix,nsig,dtp,sl_i, &                        ! in
                           ges_ps_i,ges_tsen_i,ges_q_i,ges_cw_i,rhc, &   ! in
                           ges_tsen_ten_i,ges_q_ten_i,ges_prs_ten_i, &   ! in
                           tgs_o,qgs_o,cwgs_o, &                         ! out
                           tsen_ten_i,q_ten_i,p_ten_i, &                 ! TL in
                           del_tgs_i,del_qgs_i,del_cwgs_i, &             ! TL in
                           del_tgs_o,del_qgs_o,del_cwgs_o)               ! TL out

!       Saving trajectories (output)
        do k = 1, nsig
           tgs(i,j,k)    = tgs_o(k)
           qgs(i,j,k)    = qgs_o(k)
           cwgs(i,j,k)   = cwgs_o(k)
        enddo

!       if (mype==0 .and. i*j==1) write(6,*) 'gfs_moistphys_tl: do gfs_precip_tl '
!       GFS grid-scale condensation/evaporation processes
!       Initialize TL I/O
        ges_ps_i = ges_ps(i,j,it)
        do k = 1, nsig
!          Initialize TL input
!          Guess profiles (input) --- from gscond 
           sl_i(k)       = sl(i,j,k)
           del_si_i(k)   = del_si(i,j,k)
           ges_tsen_i(k) = tgs_o(k)
           ges_q_i(k)    = qgs_o(k)
           ges_cw_i(k)   = cwgs_o(k)

!          Increments (input) --- from gscond
           del_tlrg_i(k)  = del_tgs_o(k)
           del_qlrg_i(k)  = del_qgs_o(k)
           del_cwlrg_i(k) = del_cwgs_o(k)

!          Initialize output
           del_tlrg_o(k)      = zero
           del_qlrg_o(k)      = zero
           del_cwlrg_o(k)     = zero
           tlrg_o(k)          = zero
           qlrg_o(k)          = zero
           cwlrg_o(k)         = zero

        enddo
        del_rnlrg_i = zero
        del_rnlrg_o = zero
        rnlrg_o     = zero

!       Run TL of GFS precipitation processes for a vertical grid column
        call gfs_precip_tl(im,ix,nsig,dtp,del_si_i,sl_i, &                  ! in
                           ges_ps_i,ges_tsen_i,ges_q_i,ges_cw_i,rhc, &      ! in
                           tlrg_o,qlrg_o,cwlrg_o,rnlrg_o, &                 ! out
                           del_tlrg_i,del_qlrg_i,del_cwlrg_i,del_rnlrg_i, & ! TL in
                           del_tlrg_o,del_qlrg_o,del_cwlrg_o,del_rnlrg_o)   ! TL out

        do k = 1, nsig
!          TL output
           tsen_out(i,j,k) = del_tlrg_o(k)
           q_out(i,j,k)    = del_qlrg_o(k)
           cw_out(i,j,k)   = del_cwlrg_o(k)

!          Saving trajectories (output)
           tlrg(i,j,k)     = tlrg_o(k)
           qlrg(i,j,k)     = qlrg_o(k)
           cwlrg(i,j,k)    = cwlrg_o(k)
           rnlrg(i,j)      = rnlrg_o

        enddo

     enddo  ! j-loop
  enddo ! i=loop 

  do i=1,lat2   
     do j=1,lon2   
        do k=1,nsig   
           tsen(i,j,k) = tsen_out(i,j,k)
           q(i,j,k)    = q_out(i,j,k)
           cw(i,j,k)   = cw_out(i,j,k)
        enddo
     enddo
  enddo

end subroutine moistphys_tl

subroutine moistphys_ad(tsen,q,cw)

!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    moistphys_ad  adjoint for GFS moisture physics
!   prgmmr: eliu               org: np20                date: 2011-12-28
!
! abstract:
!
! program history log:
!   2011-12-28  eliu
!
!   input argument list:
!      tsen - sensible temperature
!      q    - specific humidity
!      cw   - cloud water
!   output argument list:
!      tsen - sensible temperature
!      q    - specific humidity
!      cw   - cloud water
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$

  implicit none

! Declare passed variables
  real(r_kind),dimension(lat2,lon2,nsig),intent(inout):: tsen,q,cw

! Declare local variables
  integer(i_kind):: im,ix,it
  integer(i_kind):: i,j,k,ii,mm1
  integer(i_kind):: nsphys
  logical        :: adjoint

  real(r_kind),dimension(lat2,lon2,nsig):: tsen_in,  q_in, cw_in
  real(r_kind),dimension(lat2,lon2,nsig):: tsen_out, q_out,cw_out

  real(r_kind)::rcl,rcs,tem,work1,work2
  real(r_kind)::dtp
  real(r_kind):: ges_ps_i
  real(r_kind),dimension(nsig):: rhc,sl_i,del_si_i
  real(r_kind),dimension(nsig):: ges_tsen_i,ges_q_i,ges_cw_i
  real(r_kind),dimension(nsig):: ges_tsen_o,ges_q_o,ges_cw_o
  real(r_kind),dimension(nsig):: ges_tsen_ten_i,ges_q_ten_i,ges_prs_ten_i
! gscond
  real(r_kind),dimension(nsig):: tsen_ten_i,q_ten_i,p_ten_i
  real(r_kind),dimension(nsig):: del_tgs_i,del_qgs_i,del_cwgs_i
  real(r_kind),dimension(nsig):: del_tgs_o,del_qgs_o,del_cwgs_o
  real(r_kind),dimension(nsig):: tgs_o,qgs_o,cwgs_o
! precip
  real(r_kind),dimension(nsig):: del_tlrg_i,del_qlrg_i,del_cwlrg_i
  real(r_kind),dimension(nsig):: del_tlrg_o,del_qlrg_o,del_cwlrg_o
  real(r_kind),dimension(nsig):: tlrg_o,qlrg_o,cwlrg_o
  real(r_kind):: del_rnlrg_i,del_rnlrg_o,rnlrg_o

!--------------------------------------------------------------------------------------------
! Initialize local adjoint variables
  do k=1,nsig
     do j=1,lon2
        do i=1,lat2
           tsen_out(i,j,k) = zero
           tsen_in(i,j,k)  = zero
           q_out(i,j,k)    = zero
           q_in(i,j,k)     = zero
           cw_out(i,j,k)   = zero
           cw_in(i,j,k)    = zero
        enddo
     enddo
  enddo

  do k=1,nsig
     do j=1,lon2
        do i=1,lat2
           tsen_out(i,j,k) = tsen(i,j,k)
           tsen(i,j,k)     = zero
           q_out(i,j,k)    = q(i,j,k)
           q(i,j,k)        = zero 
           cw_out(i,j,k)   = cw(i,j,k)
           cw(i,j,k)       = zero
        enddo
     enddo
  enddo

! Linearized about guess solution, so set it flag accordingly
  it=ntguessig

  nsphys = max(int(two*deltim/dtphys+0.9999_r_kind),1)
  dtp    = two*deltim/nsphys
  mm1    = mype+1
  im     = 1 
  ix     = 1 

! Loop through latitudes
  do i=1,lat2
     ii    = i+istart(mm1)-2
     ii    = max(1,min(ii,nlat))
     rcs   = sqrt(rbs2(ii))
     tem   = (rhctop-rhcbot)/(nsig-one)
     work1 = (log(one/(rcs*nlon))-dx_min)*dx_inv
     work1 = max(zero,min(one,work1))
     work2 = one - work1
     do k=1,nsig
        rhc(k) = rhcbot + tem*(k-1)
        rhc(k) = r0_99*work1 + rhc(k)*work2
        rhc(k) = max(zero,min(one,rhc(k)))
     enddo

!    Loop through longitudes
     do j=1,lon2

!       if (mype==0 .and. i*j==1) write(6,*) 'gfs_moistphys_ad: do gfs_precip_ad '
!       GFS grid-scale condensation/evaporaton processes
!       Initialize AD I/O
        ges_ps_i = ges_ps(i,j,it)
        do k = 1, nsig
!          Initialize AD input
!          Guess profiles (input)  --- from gscond
           sl_i(k)        = sl(i,j,k)
           del_si_i(k)    = del_si(i,j,k)
           ges_tsen_i(k)  = tgs(i,j,k) 
           ges_q_i(k)     = qgs(i,j,k)
           ges_cw_i(k)    = cwgs(i,j,k)

!          Increments (input)
           del_tlrg_o(k)  = tsen_out(i,j,k)
           del_qlrg_o(k)  = q_out(i,j,k)
           del_cwlrg_o(k) = cw_out(i,j,k)

!          initialize output
           del_tlrg_i(k)  = zero
           del_qlrg_i(k)  = zero
           del_cwlrg_i(k) = zero
           tlrg_o(k)      = zero
           qlrg_o(k)      = zero
           cwlrg_o(k)     = zero

        enddo
        del_rnlrg_o = zero   ! in
        del_rnlrg_i = zero   ! out
        rnlrg_o     = zero   ! out

!       Run AD of GFS precipitation processes for a vertical grid column
        adjoint = .true.
        call gfs_precip_ad(im,ix,nsig,dtp,del_si_i,sl_i, &                        ! in
                           ges_ps_i,ges_tsen_i,ges_q_i,ges_cw_i,rhc, &            ! in
                           tlrg_o,qlrg_o,cwlrg_o,rnlrg_o, &                       ! out
                           del_tlrg_i,del_qlrg_i,del_cwlrg_i,del_rnlrg_i, &       ! AD out
                           del_tlrg_o,del_qlrg_o,del_cwlrg_o,del_rnlrg_o,adjoint) ! AD in

!       if (mype==0 .and. i*j==1) write(6,*) 'gfs_moistphys_ad: do gfs_gscond_ad '
!       GFS grid-scale condensation/evaporaton processes
!       Initialize AD I/O
        ges_ps_i = ges_ps(i,j,it)
        do k = 1, nsig
!          Initialize AD input
!          Guess profiles (input) --- from outer loop 
           sl_i(k)       = sl(i,j,k)
           ges_tsen_i(k) = ges_tsen(i,j,k,it)
           ges_q_i(k)    = qgues(i,j,k)
           ges_cw_i(k)   = cwgues(i,j,k)

!          Guess tendencies (input) --- from outer loop
           ges_tsen_ten_i(k) = ges_tsen_ten(i,j,k)
           ges_q_ten_i(k)    = ges_q_ten(i,j,k)
           ges_prs_ten_i(k)  = ges_prs_ten(i,j,k+1) ! note : k=1 is surface pressure tendency

!          Increments (input) --- from precip
           del_tgs_o(k)  = del_tlrg_i(k)
           del_qgs_o(k)  = del_qlrg_i(k)
           del_cwgs_o(k) = del_cwlrg_i(k)

!          Initialize output
           tsen_ten_i(k) = zero
           p_ten_i(k)    = zero
           q_ten_i(k)    = zero
           del_tgs_i(k)  = zero
           del_qgs_i(k)  = zero
           del_cwgs_i(k) = zero
           tgs_o(k)      = zero
           qgs_o(k)      = zero
           cwgs_o(k)     = zero

        enddo

        adjoint = .true.
        call gfs_gscond_ad(im,ix,nsig,dtp,sl_i, &                        ! in
                           ges_ps_i,ges_tsen_i,ges_q_i,ges_cw_i,rhc, &   ! in
                           ges_tsen_ten_i,ges_q_ten_i,ges_prs_ten_i, &   ! in
                           tgs_o,qgs_o,cwgs_o, &                         ! out
                           tsen_ten_i,q_ten_i,p_ten_i, &                 ! AD out
                           del_tgs_i,del_qgs_i,del_cwgs_i, &             ! AD out
                           del_tgs_o,del_qgs_o,del_cwgs_o,adjoint)       ! AD in

        do k=1,nsig
           tsen_in(i,j,k) = tsen_in(i,j,k)+del_tgs_i(k)
           del_tgs_i(k)   = zero
           q_in(i,j,k)    = q_in(i,j,k)+del_qgs_i(k)
           del_qgs_i(k)   = zero
           cw_in(i,j,k)   = cw_in(i,j,k)+del_cwgs_i(k)
           del_cwgs_i(k)  = zero
        enddo


     enddo  ! j-loop
  enddo ! i-loop

     do k =1,nsig 
        do j =1,lon2
           do i =1,lat2
              tsen(i,j,k)    = tsen(i,j,k)+tsen_in(i,j,k)
              tsen_in(i,j,k) = zero
              q(i,j,k)       = q(i,j,k)+q_in(i,j,k)
              q_in(i,j,k)    = zero
              cw(i,j,k)      = cw(i,j,k)+cw_in(i,j,k)
              cw_in(i,j,k)   = zero
           enddo
        enddo
     enddo

end subroutine moistphys_ad

end module gfs_moistphys_mod
