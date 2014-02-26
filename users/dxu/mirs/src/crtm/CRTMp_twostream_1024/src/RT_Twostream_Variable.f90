!
 MODULE RT_Twostream_Variable

  ! ----------
  ! Module use
  ! ----------

  USE Type_Kinds
  USE CRTM_Parameters, ONLY : MAX_N_LAYERS 

  ! -----------------------
  ! Disable implicit typing
  ! -----------------------

  IMPLICIT NONE

  ! ------------
  ! Visibilities
  ! ------------

    REAL( fp_kind ), PUBLIC, SAVE, DIMENSION(0:MAX_N_LAYERS) :: ednc, rdnc  ! K
    REAL( fp_kind ), PUBLIC, SAVE, DIMENSION(1 + MAX_N_LAYERS) :: eupc, rupc, down_r_lev, up_r_lev  ! K
    REAL( fp_kind ), PUBLIC, SAVE, DIMENSION(1 + MAX_N_LAYERS) ::  upstream_lev, downstream_lev  ! K
    REAL( fp_kind ), PUBLIC, SAVE, DIMENSION( MAX_N_LAYERS ) :: eup_lay, edn_lay, ref_lay, tran_lay  ! K

 END MODULE RT_Twostream_Variable
!


