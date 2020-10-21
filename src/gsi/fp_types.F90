#ifdef FCA_REF_MOD
module fp_types_m
!$$$ module documentation block
!           .      .    .                                       .
! module:   fp_types_m
!  prgmmr: 
!
! abstract: Definition for floating point types
!
! program history log:
!
! subroutines included:
!
! functions included:
!
! variable definition:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block
#endif

  ! The floating point types
  PUBLIC :: Single 
  PUBLIC :: Double 
  PUBLIC :: Quad   
  PUBLIC :: FP_Kind  ! Default integer set by IFP
  PUBLIC :: FP       ! Aliases for FP_Kind
  
  ! -------------------------------------------------------------------
  ! THE DEFAULT FLOATING POINT INDEX. Change the value of IFP for the
  ! required floating point kind. The following chart details the
  ! correspondence:
  !
  !    IFP          REAL(FP)
  !  ==============================
  !     1       Single (4  bytes)
  !     2       Double (8  bytes)
  !     3       Quad   (16 bytes)  **IF AVAILABLE, Double OTHERWISE**
  !
  ! -------------------------------------------------------------------
  INTEGER, PARAMETER :: IFP = 2  ! 1=Single, 2=Double, 3=Quad
 
  ! --------------------------
  ! Floating point definitions
  ! --------------------------
  ! Floating point types
  INTEGER, PARAMETER :: Single = SELECTED_REAL_KIND(6)  ! Single precision
  INTEGER, PARAMETER :: Double = SELECTED_REAL_KIND(15) ! Double precision
  INTEGER, PARAMETER :: Quad   = SELECTED_REAL_KIND(20) ! Quad precision

  ! Define arrays for default definition
  INTEGER, PARAMETER :: N_FP = 3
  INTEGER, PARAMETER, DIMENSION(N_FP) :: FP_KIND_TYPES = (/ Single, &
                                                            Double, &
                                                            Quad    /) 
  ! Default values
  INTEGER, PARAMETER :: FP_Kind         = FP_KIND_TYPES(IFP)
  INTEGER, PARAMETER :: FP        =FP_Kind
#ifdef FCA_REF_MOD
end module fp_types_m
#endif
