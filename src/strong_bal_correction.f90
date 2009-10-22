subroutine strong_bal_correction(u_t,v_t,t_t,ps_t,mype,psi,chi,t,ps,bal_diagnostic,fullfield,update)

!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    strong_bal_correction  strong balance correction
!   prgmmr: parrish          org: np23                date: 2007-02-15
!
! abstract: given input perturbation tendencies of u,v,t,ps from TLM,
!           and input perturbation u,v,t,ps, compute balance adjustment to u,v,t,ps
!           which zeroes out input gravity component of perturbation tendencies.
!           also output, for later use, input tendencies projected onto gravity modes.
!           this is higher level routine, which calls more specific routines, based
!           on the value of parameter jcstrong_option, passed through module mod_jcstrong
!
!               jcstrong_option = 1   -- then call original slow global application
!                               = 2   -- then call faster global applicatioin
!                               = 3   -- then call regional application
!                               = 4   -- then call version 3 regional application
!           
!
! program history log:
!   2007-02-15  parrish
!   2008-08-10  derber - update to output correction to psi and chi for global
!
!   input argument list:
!     u_t      - input perturbation u tendency (subdomains)
!     v_t      - input perturbation v tendency (subdomains)
!     t_t      - input perturbation T tendency (subdomains)
!     ps_t     - input perturbation surface pressure tendency (subdomains)
!     mype     - current processor
!     psi
!     chi
!     t        - input perturbation T (subdomains)
!     ps       - input perturbation surface pressure (subdomains)
!     bal_diagnostic - if true, then compute BAL diagnostic, a measure of amplitude
!                      of balanced gravity mode tendencies
!     fullfield - if true, the BAL diagnostics are full field, if false, they are 
!                 incremental.
!     update   - if false, then do not update u,v,t,ps with balance increment
!
!   output argument list:
!     u_t      - output perturbation u tendency (subdomains)
!     v_t      - output perturbation v tendency (subdomains)
!     t_t      - output perturbation T tendency (subdomains)
!     ps_t     - output perturbation surface pressure tendency (subdomains)
!     psi
!     chi
!     t        - output balanced perturbation T (subdomains)
!     ps       - output balanced perturbation surface pressure (subdomains)
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$

  use kinds, only: r_kind,i_kind
  use mod_strong, only: jcstrong_option
  use zrnmi_mod, only: zrnmi_strong_bal_correction
  use strong_slow_global_mod, only: strong_bal_correction_slow_global
  use strong_fast_global_mod, only: strong_bal_correction_fast_global
  use gridmod, only: lat2,lon2,nsig
  implicit none

  integer(i_kind),intent(in)::mype
  logical,intent(in)::bal_diagnostic,update,fullfield
  real(r_kind),dimension(lat2,lon2,nsig),intent(inout)::u_t,v_t,t_t
  real(r_kind),dimension(lat2,lon2),intent(inout)::ps_t
  real(r_kind),dimension(lat2,lon2,nsig),intent(inout)::psi,chi,t
  real(r_kind),dimension(lat2,lon2),intent(inout)::ps

  if(jcstrong_option.eq.1) then

!    slow global option:

    call strong_bal_correction_slow_global(u_t,v_t,t_t,ps_t,mype,psi,chi,t,ps,bal_diagnostic,fullfield,update)

  elseif(jcstrong_option.eq.2) then

!    faster global option:

    call strong_bal_correction_fast_global(u_t,v_t,t_t,ps_t,mype,psi,chi,t,ps,bal_diagnostic,fullfield,update)

  elseif(jcstrong_option.eq.3) then

!    regional option:

    call zrnmi_strong_bal_correction(u_t,v_t,t_t,ps_t,psi,chi,t,ps,bal_diagnostic,fullfield,update,mype)

  elseif(jcstrong_option.eq.4) then

!    version 3 regional option

   !call fmg_strong_bal_correction_ad_test(u_t,v_t,t_t,ps_t,psi,chi,t,ps,mype)
   !call zrnmi_filter_uvm_ad_test(mype)

    call fmg_strong_bal_correction(u_t,v_t,t_t,ps_t,psi,chi,t,ps,bal_diagnostic,fullfield,update,mype)

  end if

end subroutine strong_bal_correction

subroutine strong_bal_correction_ad(u_t,v_t,t_t,ps_t,mype,psi,chi,t,ps)

!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    strong_bal_correction_ad  adjoint strong balance correction
!   prgmmr: parrish          org: np23                date: 2007-02-15
!
! abstract: given input perturbation tendencies of u,v,t,ps from TLM,
!           and input perturbation u,v,t,ps, compute balance adjustment to u,v,t,ps
!           which zeroes out input gravity component of perturbation tendencies.
!           also output, for later use, input tendencies projected onto gravity modes.
!           this is higher level routine, which calls more specific routines, based
!           on the value of parameter jcstrong_option, passed through module mod_jcstrong
!
!               jcstrong_option = 1   -- then call original slow global application
!                               = 2   -- then call faster global applicatioin
!                               = 3   -- then call regional application
!                               = 4   -- then call version 3 regional application
!           
!
! program history log:
!   2007-02-15  parrish
!   2008-08-10  derber - update to output correction to psi and chi for global
!
!   input argument list:
!     u_t      - input perturbation u tendency (subdomains)
!     v_t      - input perturbation v tendency (subdomains)
!     t_t      - input perturbation T tendency (subdomains)
!     ps_t     - input perturbation surface pressure tendency (subdomains)
!     mype     - current processor
!     psi
!     chi
!     t        - input perturbation T (subdomains)
!     ps       - input perturbation surface pressure (subdomains)
!
!   output argument list:
!     u_t      - output adjusted perturbation u_t (subdomains)
!     v_t      - output adjusted perturbation v_t (subdomains)
!     t_t      - output adjusted perturbation T_t (subdomains)
!     ps_t     - output adjusted perturbation ps_t (subdomains)
!     psi
!     chi
!     t        - output perturbation T (subdomains)
!     ps       - output perturbation surface pressure (subdomains)
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$

  use kinds, only: r_kind,i_kind
  use mod_strong, only: jcstrong_option
  use zrnmi_mod, only: zrnmi_strong_bal_correction_ad
  use strong_slow_global_mod, only: strong_bal_correction_slow_global_ad
  use strong_fast_global_mod, only: strong_bal_correction_fast_global_ad
  use gridmod, only: lat2,lon2,nsig
  implicit none

  integer(i_kind),intent(in)::mype
  real(r_kind),dimension(lat2,lon2,nsig),intent(inout)::u_t,v_t,t_t
  real(r_kind),dimension(lat2,lon2),intent(inout)::ps_t
  real(r_kind),dimension(lat2,lon2,nsig),intent(inout)::psi,chi,t
  real(r_kind),dimension(lat2,lon2),intent(inout)::ps

  logical update

  if(jcstrong_option.eq.1) then

!    slow global option:

    call strong_bal_correction_slow_global_ad(u_t,v_t,t_t,ps_t,mype,psi,chi,t,ps)

  elseif(jcstrong_option.eq.2) then

!    faster global option:

    call strong_bal_correction_fast_global_ad(u_t,v_t,t_t,ps_t,mype,psi,chi,t,ps)

  elseif(jcstrong_option.eq.3) then

!    regional option:

    update=.true.
    call zrnmi_strong_bal_correction_ad(u_t,v_t,t_t,ps_t,psi,chi,t,ps,update,mype)

  elseif(jcstrong_option.eq.4) then

!    version 3 regional option

    update=.true.
    call fmg_strong_bal_correction_ad(u_t,v_t,t_t,ps_t,psi,chi,t,ps,update,mype)

  end if

end subroutine strong_bal_correction_ad

