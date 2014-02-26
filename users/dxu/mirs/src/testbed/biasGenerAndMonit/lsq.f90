module LSQ

!  Module for unconstrained linear least-squares calculations.
!  The algorithm is suitable for updating LS calculations as more
!  data are added.   This is sometimes called recursive estimation.
!  Only one dependent variable is allowed.
!  Based upon Applied Statistics algorithm AS 274.
!  Translation from Fortran 77 to Fortran 90 by Alan Miller.
!  A subroutine, VARPRD, has been added for calculating the variances
!  of predicted values, and this uses a subroutine BKSUB2.

!  Version 1.11, 8 November 1999 - ELF90 compatible version
!  F version - 9 April 2000
!  Author: Alan Miller
!          CSIRO Mathematical Information Sciences
!          Private Bag 10, Clayton South MDC
!          Clayton 3169, Victoria, Australia
!  Phone: (+61) 3 9545-8036      Fax: (+61) 3 9545-8080
!  e-mail: Alan.Miller @ vic.cmis.csiro.au  & milleraj @ ozemail.com.au
!  WWW-page: http://www.ozemail.com.au/~milleraj

!  Bug fixes:
!  1. In REGCF a call to TOLSET has been added in case the user had
!     not set tolerances.
!  2. In SING, each time a singularity is detected, unless it is in the
!     variables in the last position, INCLUD is called.   INCLUD assumes
!     that a new observation is being added and increments the number of
!     cases, NOBS.   The line:  nobs = nobs - 1 has been added.
!  3. row_ptr was left out of the DEALLOCATE statement in routine startup
!     in version 1.07.
!  4. In COV, now calls SS if rss_set = .FALSE.  29 August 1997

!  Other changes:
!  1. Array row_ptr added 18 July 1997.   This points to the first element
!     stored in each row thus saving a small amount of time needed to
!     calculate its position.
!  2. Optional parameter, EPS, added to routine TOLSET, so that the user
!     can specify the accuracy of the input data.

!  The PUBLIC variables are:
!  DP       = a KIND parameter for the floating-point quantities calculated
!             in this module.   See the more detailed explanation below.
!             This KIND parameter should be used for all floating-point
!             arguments passed to routines in this module.

!  nobs    = the number of observations processed to date.
!  ncol    = the total number of variables, including one for the constant,
!            if a constant is being fitted.
!  r_dim   = the dimension of array r = ncol*(ncol-1)/2
!  vorder  = an integer vector storing the current order of the variables
!            in the QR-factorization.   The initial order is 0, 1, 2, ...
!            if a constant is being fitted, or 1, 2, ... otherwise.
!  initialized = a logical variable which indicates whether space has
!                been allocated for various arrays.
!  tol_set = a logical variable which is set when subroutine TOLSET has
!            been called to calculate tolerances for use in testing for
!            singularities.
!  rss_set = a logical variable indicating whether residual sums of squares
!            are available and usable.
!  d()     = array of row multipliers for the Cholesky factorization.
!            The factorization is X = Q.sqrt(D).R where Q is an ortho-
!            normal matrix which is NOT stored, D is a diagonal matrix
!            whose diagonal elements are stored in array d, and R is an
!            upper-triangular matrix with 1's as its diagonal elements.
!  rhs()   = vector of RHS projections (after scaling by sqrt(D)).
!            Thus Q'y = sqrt(D).rhs
!  r()     = the upper-triangular matrix R.   The upper triangle only,
!            excluding the implicit 1's on the diagonal, are stored by
!            rows.
!  tol()   = array of tolerances used in testing for singularities.
!  rss()   = array of residual sums of squares.   rss(i) is the residual
!            sum of squares with the first i variables in the model.
!            By changing the order of variables, the residual sums of
!            squares can be found for all possible subsets of the variables.
!            The residual sum of squares with NO variables in the model,
!            that is the total sum of squares of the y-values, can be
!            calculated as rss(1) + d(1)*rhs(1)^2.   If the first variable
!            is a constant, then rss(1) is the sum of squares of
!            (y - ybar) where ybar is the average value of y.
!  sserr   = residual sum of squares with all of the variables included.
!  row_ptr() = array of indices of first elements in each row of R.
!
!--------------------------------------------------------------------------

!     General declarations

implicit none

public       :: STARTUP, INCLUD, REGCF, TOLSET, SING, SS, COV, PARTIAL_CORR, &
                VMOVE, REORDR, HDIAG, VARPRD, BKSUB2
private      :: INV

integer, save, public                     :: NOBS, NCOL, R_DIM
integer, allocatable, save, dimension(:), public  :: VORDER, ROW_PTR
logical, save, public                     :: INITIALIZED = .false.,    &
                                             TOL_SET = .false.,  &
                                             RSS_SET = .false.

! Note. DP is being set to give at least 10 decimal digit
!       representation of floating point numbers.   This should be adequate
!       for most problems except the fitting of polynomials.   DP is
!       being set so that the same code can be run on PCs and Unix systems,
!       which will usually represent floating-point numbers in `double
!       precision', and other systems with larger word lengths which will
!       give similar accuracy in `single precision'.

integer, parameter, public      :: DP = selected_real_kind(10,70)
real (kind=DP), allocatable, save, dimension(:), public :: D, RHS, R, TOL, RSS
real (kind=DP), save, private   :: ZERO = 0.0_DP, ONE = 1.0_DP, VSMALL
real (kind=DP), save, public    :: SSERR, TOLY


contains

subroutine STARTUP(NVAR, FIT_CONST)

!     Allocates dimensions for arrays and initializes to zero
!     The calling program must set nvar = the number of variables, and
!     fit_const = .true. if a constant is to be included in the model,
!     otherwise fit_const = .false.
!
!--------------------------------------------------------------------------

integer, intent(in)  :: NVAR
logical, intent(in)  :: FIT_CONST

!     Local variable
integer              :: I

VSMALL = 10.0_DP * tiny(ZERO)

NOBS = 0
if (FIT_CONST) then
  NCOL = NVAR + 1
else
  NCOL = NVAR
end if

if (INITIALIZED) then
  deallocate(D, RHS, R, TOL, RSS, VORDER, ROW_PTR)
end if
R_DIM = NCOL * (NCOL - 1)/2
allocate( D(NCOL), RHS(NCOL), R(R_DIM), TOL(NCOL), RSS(NCOL), VORDER(NCOL),  &
          ROW_PTR(NCOL) )

D = ZERO
RHS = ZERO
R = ZERO
SSERR = ZERO

if (FIT_CONST) then
  do I = 1, NCOL
    VORDER(I) = I-1
  end do
else
  do I = 1, NCOL
    VORDER(I) = I
  end do
end if ! (fit_const)

! row_ptr(i) is the position of element R(i,i+1) in array r().

ROW_PTR(1) = 1
do I = 2, NCOL-1
  ROW_PTR(I) = ROW_PTR(I-1) + NCOL - I + 1
end do
ROW_PTR(NCOL) = 0

INITIALIZED = .true.
TOL_SET = .false.
RSS_SET = .false.

return
end subroutine STARTUP




subroutine INCLUD(WEIGHT, XROW, YELEM)

!     ALGORITHM AS75.1  APPL. STATIST. (1974) VOL.23, NO. 3

!     Calling this routine updates D, R, RHS and SSERR by the
!     inclusion of xrow, yelem with the specified weight.

!     *** WARNING  Array XROW is overwritten.

!     N.B. As this routine will be called many times in most applications,
!          checks have been eliminated.
!
!--------------------------------------------------------------------------

real (kind=DP), intent(in)                   :: WEIGHT, YELEM
real (kind=DP), dimension(:), intent(in out) :: XROW

!     Local variables

integer         :: I, K, NEXTR
real (kind=DP)  :: W, Y, XI, DI, WXI, DPI, CBAR, SBAR, XK

NOBS = NOBS + 1
W = WEIGHT
Y = YELEM
RSS_SET = .false.
NEXTR = 1
do I = 1, NCOL

!     Skip unnecessary transformations.   Test on exact zeroes must be
!     used or stability can be destroyed.

  if (abs(W) < VSMALL) then
    return
  end if
  XI = XROW(I)
  if (abs(XI) < VSMALL) then
    NEXTR = NEXTR + NCOL - I
  else
    DI = D(I)
    WXI = W * XI
    DPI = DI + WXI*XI
    CBAR = DI / DPI
    SBAR = WXI / DPI
    W = CBAR * W
    D(I) = DPI
    do K = I+1, NCOL
      XK = XROW(K)
      XROW(K) = XK - XI * R(NEXTR)
      R(NEXTR) = CBAR * R(NEXTR) + SBAR * XK
      NEXTR = NEXTR + 1
    end do
    XK = Y
    Y = XK - XI * RHS(I)
    RHS(I) = CBAR * RHS(I) + SBAR * XK
  end if
end do ! i = 1, ncol

!     Y * SQRT(W) is now equal to the Brown, Durbin & Evans recursive
!     residual.

SSERR = SSERR + W * Y * Y

return
end subroutine INCLUD



subroutine REGCF(BETA, NREQ, IFAULT)

!     ALGORITHM AS274  APPL. STATIST. (1992) VOL.41, NO. 2

!     Modified version of AS75.4 to calculate regression coefficients
!     for the first NREQ variables, given an orthogonal reduction from
!     AS75.1.
!
!--------------------------------------------------------------------------

integer, intent(in)                        :: NREQ
integer, intent(out)                       :: IFAULT
real (kind=DP), dimension(:), intent(out)  :: BETA

!     Local variables

integer     :: I, J, NEXTR

!     Some checks.

IFAULT = 0
if (NREQ < 1 .or. NREQ > NCOL) then
  IFAULT = IFAULT + 4
end if
if (IFAULT /= 0) then
  return
end if

if (.not. TOL_SET) then
  call TOLSET()
end if

do I = NREQ, 1, -1
  if (sqrt(D(I)) < TOL(I)) then
    BETA(I) = ZERO
    D(I) = ZERO
    IFAULT = -I
  else
    BETA(I) = RHS(I)
    NEXTR = ROW_PTR(I)
    do J = I+1, NREQ
      BETA(I) = BETA(I) - R(NEXTR) * BETA(J)
      NEXTR = NEXTR + 1
    end do ! j = i+1, nreq
  end if
end do ! i = nreq, 1, -1

return
end subroutine REGCF



subroutine TOLSET(EPS)

!     ALGORITHM AS274  APPL. STATIST. (1992) VOL.41, NO. 2

!     Sets up array TOL for testing for zeroes in an orthogonal
!     reduction formed using AS75.1.

real (kind=DP), intent(in), optional :: EPS

!     Unless the argument eps is set, it is assumed that the input data are
!     recorded to full machine accuracy.   This is often not the case.
!     If, for instance, the data are recorded to `single precision' of about
!     6-7 significant decimal digits, then singularities will not be detected.
!     It is suggested that in this case eps should be set equal to
!     10.0 * EPSILON(1.0)
!     If the data are recorded to say 4 significant decimals, then eps should
!     be set to 1.0E-03
!     The above comments apply to the predictor variables, not to the
!     dependent variable.

!     Local variables.
!
!--------------------------------------------------------------------------

!     Local variables

integer          :: COL, ROW, POS
real (kind=DP)   :: EPS1, TOTAL
real (kind=DP), dimension(NCOL)  :: WORK
real (kind=DP), parameter        :: TEN = 10.0_DP

!     EPS is a machine-dependent constant.

if (present(EPS)) then
  EPS1 = max(abs(EPS), TEN * epsilon(TEN))
else
  EPS1 = TEN * epsilon(TEN)
end if

!     Set tol(i) = sum of absolute values in column I of R after scaling each
!     element by the square root of its row multiplier, multiplied by EPS1.

WORK = sqrt(D)
do COL = 1, NCOL
  POS = COL - 1
  TOTAL = WORK(COL)
  do ROW = 1, COL-1
    TOTAL = TOTAL + abs(R(POS)) * WORK(ROW)
    POS = POS + NCOL - ROW - 1
  end do
  TOL(COL) = EPS1 * TOTAL
end do

TOL_SET = .true.
return
end subroutine TOLSET




subroutine SING(LINDEP, IFAULT)

!     ALGORITHM AS274  APPL. STATIST. (1992) VOL.41, NO. 2

!     Checks for singularities, reports, and adjusts orthogonal
!     reductions produced by AS75.1.

!     Auxiliary routines called: INCLUD, TOLSET
!
!--------------------------------------------------------------------------

integer, intent(out)                :: IFAULT
logical, dimension(:), intent(out)  :: LINDEP

!     Local variables

real (kind=DP)   :: TEMP, Y, WEIGHT
integer          :: COL, POS, ROW, POS2
real (kind=DP), dimension(NCOL)  :: X, WORK

IFAULT = 0

WORK = sqrt(D)
if (.not. TOL_SET) then
  call TOLSET()
end if

do COL = 1, NCOL
  TEMP = TOL(COL)
  POS = COL - 1
  do ROW = 1, COL-1
    POS = POS + NCOL - ROW - 1
  end do

!     If diagonal element is near zero, set it to zero, set appropriate
!     element of LINDEP, and use INCLUD to augment the projections in
!     the lower rows of the orthogonalization.

  LINDEP(COL) = .false.
  if (WORK(COL) <= TEMP) then
    LINDEP(COL) = .true.
    IFAULT = IFAULT - 1
    if (COL < NCOL) then
      POS2 = POS + NCOL - COL + 1
      X = ZERO
      X(COL+1:NCOL) = R(POS+1:POS2-1)
      Y = RHS(COL)
      WEIGHT = D(COL)
      R(POS+1:POS2-1) = ZERO
      D(COL) = ZERO
      RHS(COL) = ZERO
      call INCLUD(WEIGHT, X, Y)
                             ! INCLUD automatically increases the number
                             ! of cases each time it is called.
      NOBS = NOBS - 1
    else
      SSERR = SSERR + D(COL) * RHS(COL)**2
    end if ! (col < ncol)
  end if ! (work(col) <= temp)
end do ! col = 1, ncol
return
end subroutine SING



subroutine SS()

!     ALGORITHM AS274  APPL. STATIST. (1992) VOL.41, NO. 2

!     Calculates partial residual sums of squares from an orthogonal
!     reduction from AS75.1.
!
!--------------------------------------------------------------------------

!     Local variables

integer          :: I
real (kind=DP)   :: TOTAL

TOTAL = SSERR
RSS(NCOL) = SSERR
do I = NCOL, 2, -1
  TOTAL = TOTAL + D(I) * RHS(I)**2
  RSS(I-1) = TOTAL
end do

RSS_SET = .true.
return
end subroutine SS



subroutine COV(NREQ, VAR, COVMAT, DIMCOV, STERR, IFAULT)

!     ALGORITHM AS274  APPL. STATIST. (1992) VOL.41, NO. 2

!     Calculate covariance matrix for regression coefficients for the
!     first nreq variables, from an orthogonal reduction produced from
!     AS75.1.

!     Auxiliary routine called: INV
!
!--------------------------------------------------------------------------

integer, intent(in)                        :: NREQ, DIMCOV
integer, intent(out)                       :: IFAULT
real (kind=DP), intent(out)                :: VAR
real (kind=DP), dimension(:), intent(out)  :: COVMAT, STERR

!     Local variables.

integer           :: DIM_RINV, POS, ROW, START, POS2, COL, POS1, K
real (kind=DP)    :: TOTAL
real (kind=DP), allocatable, dimension(:)  :: RINV

!     Check that dimension of array covmat is adequate.

if (DIMCOV < NREQ*(NREQ+1)/2) then
  IFAULT = 1
  return
end if

!     Check for small or zero multipliers on the diagonal.

IFAULT = 0
do ROW = 1, NREQ
  if (abs(D(ROW)) < VSMALL) then
    IFAULT = -ROW
  end if
end do
if (IFAULT /= 0) then
  return
end if

!     Calculate estimate of the residual variance.

if (NOBS > NREQ) then
  if (.not. RSS_SET) then
    call SS()
  end if
  VAR = RSS(NREQ) / (NOBS - NREQ)
else
  IFAULT = 2
  return
end if

DIM_RINV = NREQ*(NREQ-1)/2
allocate ( RINV(DIM_RINV) )

call INV(NREQ, RINV)
POS = 1
START = 1
do ROW = 1, NREQ
  POS2 = START
  do COL = ROW, NREQ
    POS1 = START + COL - ROW
    if (ROW == COL) then
      TOTAL = ONE / D(COL)
    else
      TOTAL = RINV(POS1-1) / D(COL)
    end if
    do K = COL+1, NREQ
      TOTAL = TOTAL + RINV(POS1) * RINV(POS2) / D(K)
      POS1 = POS1 + 1
      POS2 = POS2 + 1
    end do
    COVMAT(POS) = TOTAL * VAR
    if (ROW == COL) then
      STERR(ROW) = sqrt(COVMAT(POS))
    end if
    POS = POS + 1
  end do
  START = START + NREQ - ROW
end do

deallocate(RINV)
return
end subroutine COV



subroutine INV(NREQ, RINV)

!     ALGORITHM AS274  APPL. STATIST. (1992) VOL.41, NO. 2

!     Invert first nreq rows and columns of Cholesky factorization
!     produced by AS 75.1.
!
!--------------------------------------------------------------------------

integer, intent(in)                        :: NREQ
real (kind=DP), dimension(:), intent(out)  :: RINV

!     Local variables.

integer          :: POS, ROW, COL, START, K, POS1, POS2
real (kind=DP)   :: TOTAL

!     Invert R ignoring row multipliers, from the bottom up.

POS = NREQ * (NREQ-1)/2
do ROW = NREQ-1, 1, -1
  START = ROW_PTR(ROW)
  do COL = NREQ, ROW+1, -1
    POS1 = START
    POS2 = POS
    TOTAL = ZERO
    do K = ROW+1, COL-1
      POS2 = POS2 + NREQ - K
      TOTAL = TOTAL - R(POS1) * RINV(POS2)
      POS1 = POS1 + 1
    end do ! k = row+1, col-1
    RINV(POS) = TOTAL - R(POS1)
    POS = POS - 1
  end do ! col = nreq, row+1, -1
end do ! row = nreq-1, 1, -1

return
end subroutine INV



subroutine PARTIAL_CORR(INVAR, CORMAT, DIMC, YCORR, IFAULT)

!     Replaces subroutines PCORR and COR of:
!     ALGORITHM AS274  APPL. STATIST. (1992) VOL.41, NO. 2

!     Calculate partial correlations after the variables in rows
!     1, 2, ..., INVAR have been forced into the regression.
!     If INVAR = 1, and the first row of R represents a constant in the
!     model, then the usual simple correlations are returned.

!     If INVAR = 0, the value returned in array CORMAT for the correlation
!     of variables Xi & Xj is:
!       sum ( Xi.Xj ) / Sqrt ( sum (Xi^2) . sum (Xj^2) )

!     On return, array CORMAT contains the upper triangle of the matrix of
!     partial correlations stored by rows, excluding the 1's on the diagonal.
!     e.g. if INVAR = 2, the consecutive elements returned are:
!     (3,4) (3,5) ... (3,ncol), (4,5) (4,6) ... (4,ncol), etc.
!     Array YCORR stores the partial correlations with the Y-variable
!     starting with YCORR(INVAR+1) = partial correlation with the variable in
!     position (INVAR+1).
!
!--------------------------------------------------------------------------

integer, intent(in)                        :: INVAR, DIMC
integer, intent(out)                       :: IFAULT
real (kind=DP), dimension(:), intent(out)  :: CORMAT, YCORR

!     Local variables.

integer          :: BASE_POS, POS, ROW, COL, COL1, COL2, POS1, POS2
real (kind=DP)   :: SUMXX, SUMXY, SUMYY
real (kind=DP), dimension(INVAR+1:NCOL)  :: RMS, WORK

!     Some checks.

IFAULT = 0
if (INVAR < 0 .or. INVAR > NCOL-1) then
  IFAULT = IFAULT + 4
end if
if (DIMC < (NCOL-INVAR)*(NCOL-INVAR-1)/2) then
  IFAULT = IFAULT + 8
end if
if (IFAULT /= 0) then
  return
end if

!     Base position for calculating positions of elements in row (INVAR+1) of R.

BASE_POS = INVAR*NCOL - (INVAR+1)*(INVAR+2)/2

!     Calculate 1/RMS of elements in columns from INVAR to (ncol-1).

if (D(INVAR+1) > ZERO) then
  RMS(INVAR+1) = ONE / sqrt(D(INVAR+1))
end if
do COL = INVAR+2, NCOL
  POS = BASE_POS + COL
  SUMXX = D(COL)
  do ROW = INVAR+1, COL-1
    SUMXX = SUMXX + D(ROW) * R(POS)**2
    POS = POS + NCOL - ROW - 1
  end do ! row = INVAR+1, col-1
  if (SUMXX > ZERO) then
    RMS(COL) = ONE / sqrt(SUMXX)
  else
    RMS(COL) = ZERO
    IFAULT = -COL
  end if
end do

!     Calculate 1/RMS for the Y-variable

SUMYY = SSERR
do ROW = INVAR+1, NCOL
  SUMYY = SUMYY + D(ROW) * RHS(ROW)**2
end do ! row = INVAR+1, ncol
if (SUMYY > ZERO) then
  SUMYY = ONE / sqrt(SUMYY)
end if

!     Calculate sums of cross-products.
!     These are obtained by taking dot products of pairs of columns of R,
!     but with the product for each row multiplied by the row multiplier
!     in array D.

POS = 1
do COL1 = INVAR+1, NCOL
  SUMXY = ZERO
  WORK(COL1+1:NCOL) = ZERO
  POS1 = BASE_POS + COL1
  do ROW = INVAR+1, COL1-1
    POS2 = POS1 + 1
    do COL2 = COL1+1, NCOL
      WORK(COL2) = WORK(COL2) + D(ROW) * R(POS1) * R(POS2)
      POS2 = POS2 + 1
    end do ! col2 = col1+1, ncol
    SUMXY = SUMXY + D(ROW) * R(POS1) * RHS(ROW)
    POS1 = POS1 + NCOL - ROW - 1
  end do ! row = INVAR+1, col1-1

!     Row COL1 has an implicit 1 as its first element (in column COL1)

  POS2 = POS1 + 1
  do COL2 = COL1+1, NCOL
    WORK(COL2) = WORK(COL2) + D(COL1) * R(POS2)
    POS2 = POS2 + 1
    CORMAT(POS) = WORK(COL2) * RMS(COL1) * RMS(COL2)
    POS = POS + 1
  end do ! col2 = col1+1, ncol
  SUMXY = SUMXY + D(COL1) * RHS(COL1)
  YCORR(COL1) = SUMXY * RMS(COL1) * SUMYY
end do ! col1 = INVAR+1, ncol-1

YCORR(1:INVAR) = ZERO

return
end subroutine PARTIAL_CORR




subroutine VMOVE(FROM, DEST, IFAULT)

!     ALGORITHM AS274 APPL. STATIST. (1992) VOL.41, NO. 2

!     Move variable from position FROM to position DEST in an
!     orthogonal reduction produced by AS75.1.
!
!--------------------------------------------------------------------------

integer, intent(in)    :: FROM, DEST
integer, intent(out)   :: IFAULT

!     Local variables

real (kind=DP)   :: D1, D2, X, D1NEW, D2NEW, CBAR, SBAR, Y
integer          :: M, FIRST, LAST, INC, M1, M2, MP1, COL, POS, ROW

!     Check input parameters

IFAULT = 0
if (FROM < 1 .or. FROM > NCOL) then
  IFAULT = IFAULT + 4
end if
if (DEST < 1 .or. DEST > NCOL) then
  IFAULT = IFAULT + 8
end if
if (IFAULT /= 0) then
  return
end if

if (FROM == DEST) then
  return
end if

if (.not. RSS_SET) then
  call SS()
end if

if (FROM < DEST) then
  FIRST = FROM
  LAST = DEST - 1
  INC = 1
else
  FIRST = FROM - 1
  LAST = DEST
  INC = -1
end if

do M = FIRST, LAST, INC

!     Find addresses of first elements of R in rows M and (M+1).

  M1 = ROW_PTR(M)
  M2 = ROW_PTR(M+1)
  MP1 = M + 1
  D1 = D(M)
  D2 = D(MP1)

!     Special cases.

  if (D1 >= VSMALL .or. D2 >= VSMALL) then
    X = R(M1)
    if (abs(X) * sqrt(D1) < TOL(MP1)) then
      X = ZERO
    end if
    if (D1 < VSMALL .or. abs(X) < VSMALL) then
      D(M) = D2
      D(MP1) = D1
      R(M1) = ZERO
      do COL = M+2, NCOL
        M1 = M1 + 1
        X = R(M1)
        R(M1) = R(M2)
        R(M2) = X
        M2 = M2 + 1
      end do
      X = RHS(M)
      RHS(M) = RHS(MP1)
      RHS(MP1) = X
    else if (D2 < VSMALL) then
      D(M) = D1 * X**2
      R(M1) = ONE / X
      R(M1+1:M1+NCOL-M-1) = R(M1+1:M1+NCOL-M-1) / X
      RHS(M) = RHS(M) / X
    else

!     Planar rotation in regular case.

      D1NEW = D2 + D1*X**2
      CBAR = D2 / D1NEW
      SBAR = X * D1 / D1NEW
      D2NEW = D1 * CBAR
      D(M) = D1NEW
      D(MP1) = D2NEW
      R(M1) = SBAR
      do COL = M+2, NCOL
        M1 = M1 + 1
        Y = R(M1)
        R(M1) = CBAR*R(M2) + SBAR*Y
        R(M2) = Y - X*R(M2)
        M2 = M2 + 1
      end do
      Y = RHS(M)
      RHS(M) = CBAR*RHS(MP1) + SBAR*Y
      RHS(MP1) = Y - X*RHS(MP1)
    end if
  end if

!     Swap columns M and (M+1) down to row (M-1).

  POS = M
  do ROW = 1, M-1
    X = R(POS)
    R(POS) = R(POS-1)
    R(POS-1) = X
    POS = POS + NCOL - ROW - 1
  end do ! row = 1, m-1

!     Adjust variable order (VORDER), the tolerances (TOL) and
!     the vector of residual sums of squares (RSS).

  M1 = VORDER(M)
  VORDER(M) = VORDER(MP1)
  VORDER(MP1) = M1
  X = TOL(M)
  TOL(M) = TOL(MP1)
  TOL(MP1) = X
  RSS(M) = RSS(MP1) + D(MP1) * RHS(MP1)**2
end do

return
end subroutine VMOVE



subroutine REORDR(LIST, N, POS1, IFAULT)

!     ALGORITHM AS274  APPL. STATIST. (1992) VOL.41, NO. 2

!     Re-order the variables in an orthogonal reduction produced by
!     AS75.1 so that the N variables in LIST start at position POS1,
!     though will not necessarily be in the same order as in LIST.
!     Any variables in VORDER before position POS1 are not moved.

!     Auxiliary routine called: VMOVE
!
!--------------------------------------------------------------------------

integer, intent(in)                :: N, POS1
integer, dimension(:), intent(in)  :: LIST
integer, intent(out)               :: IFAULT

!     Local variables.

integer     :: NEXT, I, L, J
logical     :: FOUND

!     Check N.

IFAULT = 0
if (N < 1 .or. N > NCOL+1-POS1) then
  IFAULT = IFAULT + 4
end if
if (IFAULT /= 0) then
  return
end if

!     Work through VORDER finding variables which are in LIST.

NEXT = POS1
do I = POS1, NCOL
  L = VORDER(I)
  FOUND = .false.
  do J = 1, N
    if (L == LIST(J)) then
      FOUND = .true.
      exit
    end if
  end do

!     Variable L is in LIST; move it up to position NEXT if it is not
!     already there.

  if (FOUND) then
    if (I > NEXT) then
      call VMOVE(I, NEXT, IFAULT)
    end if
    NEXT = NEXT + 1
  end if
end do

if (NEXT >= N+POS1) then
  return
end if

!     If this point is reached, one or more variables in LIST has not
!     been found.

IFAULT = 8

return
end subroutine REORDR



subroutine HDIAG(XROW, NREQ, HII, IFAULT)

!     ALGORITHM AS274  APPL. STATIST. (1992) VOL.41, NO. 2
!
!--------------------------------------------------------------------------

integer, intent(in)                       :: NREQ
integer, intent(out)                      :: IFAULT
real (kind=DP), dimension(:), intent(in)  :: XROW
real (kind=DP), intent(out)               :: HII

!     Local variables

integer         :: COL, ROW, POS
real (kind=DP)  :: TOTAL
real (kind=DP), dimension(NCOL)  :: WK

!     Some checks

IFAULT = 0
if (NREQ > NCOL) then
  IFAULT = IFAULT + 4
end if
if (IFAULT /= 0) then
  return
end if

!     The elements of xrow.inv(R).sqrt(D) are calculated and stored
!     in WK.

HII = ZERO
do COL = 1, NREQ
  if (sqrt(D(COL)) <= TOL(COL)) then
    WK(COL) = ZERO
  else
    POS = COL - 1
    TOTAL = XROW(COL)
    do ROW = 1, COL-1
      TOTAL = TOTAL - WK(ROW)*R(POS)
      POS = POS + NCOL - ROW - 1
    end do ! row = 1, col-1
    WK(COL) = TOTAL
    HII = HII + TOTAL**2 / D(COL)
  end if
end do ! col = 1, nreq

return
end subroutine HDIAG



subroutine VARPRD(X, NREQ, FN_VAL)

!     Calculate the variance of x'b where b consists of the first nreq
!     least-squares regression coefficients.
!
!--------------------------------------------------------------------------

integer, intent(in)                       :: NREQ
real (kind=DP), dimension(:), intent(in)  :: X
real (kind=DP), intent(out)               :: FN_VAL

!     Local variables

integer         :: IFAULT, ROW
real (kind=DP)  :: VAR
real (kind=DP), dimension(NREQ) :: WK

!     Check input parameter values

FN_VAL = ZERO
IFAULT = 0
if (NREQ < 1 .or. NREQ > NCOL) then
  IFAULT = IFAULT + 4
end if
if (NOBS <= NREQ) then
  IFAULT = IFAULT + 8
end if
if (IFAULT /= 0) then
  write(unit=*, fmt="(1x, a, i4)") "Error in function VARPRD: ifault =", IFAULT
  return
end if

!     Calculate the residual variance estimate.

VAR = SSERR / (NOBS - NREQ)

!     Variance of x'b = var.x'(inv R)(inv D)(inv R')x
!     First call BKSUB2 to calculate (inv R')x by back-substitution.

call BKSUB2(X, WK, NREQ)
do ROW = 1, NREQ
  if(D(ROW) > TOL(ROW)) then
    FN_VAL = FN_VAL + WK(ROW)**2 / D(ROW)
  end if
end do

FN_VAL = FN_VAL * VAR

return
end subroutine VARPRD



subroutine BKSUB2(X, B, NREQ)

!     Solve x = R'b for b given x, using only the first nreq rows and
!     columns of R, and only the first nreq elements of R.
!
!--------------------------------------------------------------------------

integer, intent(in)                        :: NREQ
real (kind=DP), dimension(:), intent(in)   :: X
real (kind=DP), dimension(:), intent(out)  :: B

!     Local variables

integer           :: POS, ROW, COL
real (kind=DP)   :: TEMP

!     Solve by back-substitution, starting from the top.

do ROW = 1, NREQ
  POS = ROW - 1
  TEMP = X(ROW)
  do COL = 1, ROW-1
    TEMP = TEMP - R(POS)*B(COL)
    POS = POS + NCOL - COL - 1
  end do
  B(ROW) = TEMP
end do

return
end subroutine BKSUB2


end module LSQ
