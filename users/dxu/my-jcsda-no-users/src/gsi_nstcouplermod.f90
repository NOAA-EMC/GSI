!----------------------------------------------------------------------------
!BOP
!  
! !MODULE:  GSI_NSTCouplerMod ---
!
! !INTERFACE:
!
! !DESCRIPTION: This module provides general interface for
!               NST- sea surface skin temp analysis
!
! !REVISION HISTORY:
!
!  2011-10-20 RT/ Akella- Initial code
!  2012-03-05 SA-         _full fields: tref, dt_cool, dt_warm, z_c, z_w, ... are declared here INSTEAD of satthin     
!
!EOP
!-------------------------------------------------------------------------
!  def tref_full      - sea surface reference temperature-- foundation SST
!  def dt_cool_full   - sea cooling amount across sub-layer (or, cool-layer)
!  def z_c_full       - sub-layer thickness
!  def dt_warm_full   - sea diurnal warming amount
!  def z_w_full       - diurnal warming layer thickness
!  *********************************************************
!   FOLLOWING 4 FIELDS ARE FOR GFS
!  *********************************************************
!   def c_0_full       - coefficient 1 to calculate d(Tz)/d(Tr)
!   def c_d_full       - coefficient 2 to calculate d(Tz)/d(Tr)
!   def w_0_full       - coefficient 3 to calculate d(Tz)/d(Tr)
!   def w_d_full       - coefficient 4 to calculate d(Tz)/d(Tr)
!  *********************************************************
!-------------------------------------------------------------------------

module GSI_NSTCouplerMod

! !USES:
use kinds,         only: r_kind, i_kind

implicit none
private

!
! !PUBLIC MEMBER FUNCTIONS:
!
public GSI_NSTCoupler_init
public GSI_NSTCoupler_set
public GSI_NSTCoupler_skindepth
public GSI_NSTCoupler_deter
public GSI_NSTCoupler_final

public :: tref_full,dt_cool_full,z_c_full,dt_warm_full,z_w_full
public :: c_0_full,c_d_full,w_0_full,w_d_full
public :: nst_mask_full

real(r_kind),allocatable,dimension(:,:,:):: tref_full,dt_cool_full,z_c_full,dt_warm_full,z_w_full
real(r_kind),allocatable,dimension(:,:,:):: c_0_full,c_d_full,w_0_full,w_d_full
real(i_kind),allocatable,dimension(:,:)  :: nst_mask_full

!-------------------
interface gsi_nstcoupler_init
  subroutine nst_init_()
     implicit none
  end subroutine nst_init_
end interface
!-------------------

interface gsi_nstcoupler_set
  subroutine nst_set_(mype)
     use kinds,         only: i_kind
     implicit none

     integer(i_kind), intent(in   ) :: mype
     
  end subroutine nst_set_
end interface
!-------------------

interface gsi_nstcoupler_final
  subroutine nst_final_()
     implicit none
  end subroutine nst_final_
end interface
!-------------------

interface gsi_nstcoupler_skindepth
  subroutine skindepth_(obstype, zob)
     use kinds,   only: r_kind
     implicit none

     character(10), intent(in)  :: obstype
     real(r_kind),  intent(out) :: zob
  end subroutine skindepth_
end interface
!-------------------

interface gsi_nstcoupler_deter
  subroutine deter_nst_(dlat_earth,dlon_earth,obstime,zob,tref,dtw,dtc,tz_tr)
     use kinds,   only: r_kind
     implicit none

     real(r_kind), intent(in ) :: dlat_earth,dlon_earth,obstime,zob
     real(r_kind), intent(out) :: tref,dtw,dtc,tz_tr
  end subroutine deter_nst_
end interface
!-------------------

end module GSI_NSTCouplerMod

