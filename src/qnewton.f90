module qnewton
!$$$ module documentation block
!           .      .    .                                       .
! module:   qnewton
!   prgmmr:
!
! abstract:
!
! program history log:
!   2009-08-10  lueken - added module doc block
!   2010-08-19  lueken - add only to module use
!
! subroutines included:
!   sub LBFGS
!   sub MCSRCH
!   sub MCSTEP
!   sub matinv
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block
! ------------------------------------------------------------------------------
!     This file contains the LBFGS algorithm and supporting routines
! ------------------------------------------------------------------------------

use kinds, only: r_kind,i_kind,r_quad
use constants, only: zero, one
use mpimod, only: mype
use control_vectors, only: control_vector,allocate_cv, &
    deallocate_cv,dot_product

implicit none

private
public lbfgs

real(r_kind), parameter :: GTOL = 0.9_r_kind
real(r_kind), parameter :: FTOL = 1.0e-4_r_kind
real(r_kind), parameter :: XTOL = 1.0e-8_r_kind
real(r_kind), parameter :: STPMIN = 1.0e-20_r_kind
real(r_kind), parameter :: STPMAX = 1.0e+20_r_kind

! --------------------------------------
REAL             :: Z_DEFAULT_REAL      ! intentionally not real(r_kind)
integer(i_kind), PARAMETER :: N_DEFAULT_REAL_KIND = KIND(Z_DEFAULT_REAL)
DOUBLE PRECISION :: DL_DOUBLE_PRECISION ! intentionally not real(r_double)
integer(i_kind), PARAMETER :: N_DOUBLE_KIND       = KIND(DL_DOUBLE_PRECISION)
! --------------------------------------

! ------------------------------------------------------------------------------
CONTAINS
! ------------------------------------------------------------------------------
SUBROUTINE LBFGS(xhat,cost,grad,eps,maxfev,nprt,maxvecs)
!
!        LIMITED MEMORY BFGS METHOD FOR LARGE SCALE OPTIMIZATION
!                          JORGE NOCEDAL
!                        *** July 1990 ***
!
! 
!     This subroutine solves the unconstrained minimization problem
! 
!                      min F(x),    x= (x1,x2,...,xN),
!
!      using the limited memory BFGS method. The routine is especially
!      effective on problems involving a large number of variables. In
!      a typical iteration of this method an approximation Hk to the
!      inverse of the Hessian is obtained by applying M BFGS updates to
!      a diagonal matrix Hk0, using information from the previous M steps.
!      The user specifies the number M, which determines the amount of
!      storage required by the routine. The user may also provide the
!      diagonal matrices Hk0 if not satisfied with the default choice.
!      The algorithm is described in "On the limited memory BFGS method
!      for large scale optimization", by D. Liu and J. Nocedal,
!      Mathematical Programming B 45 (1989) 503-528.
! 
!      The user is required to calculate the function value F and its
!      gradient G. In order to allow the user complete control over
!      these computations, reverse  communication is used. The routine
!      must be called repeatedly under the control of the parameter
!      IFLAG. 
!
!      The steplength is determined at each iteration by means of the
!      line search routine MCVSRCH, which is a slight modification of
!      the routine CSRCH written by More' and Thuente.
! 
!      The calling statement is 
! 
!          CALL LBFGS(N,M,X,F,G,IPRINT,EPS,XTOL,W,IFLAG)
! 
!      where
! 
!     N       is an INTEGER variable that must be set by the user to the
!             number of variables. It is not altered by the routine.
!             Restriction: N>0.
! 
!     M       is an INTEGER variable that must be set by the user to
!             the number of corrections used in the BFGS update. It
!             is not altered by the routine. Values of M less than 3 are
!             not recommended; large values of M will result in excessive
!             computing time. 3<= M <=7 is recommended. Restriction: M>0.
! 
!     X       is a DOUBLE PRECISION array of length N. On initial entry
!             it must be set by the user to the values of the initial
!             estimate of the solution vector. On exit with IFLAG=0, it
!             contains the values of the variables at the best point
!             found (usually a solution).
! 
!     F       is a DOUBLE PRECISION variable. Before initial entry and on
!             a re-entry with IFLAG=1, it must be set by the user to
!             contain the value of the function F at the point X.
! 
!     G       is a DOUBLE PRECISION array of length N. Before initial
!             entry and on a re-entry with IFLAG=1, it must be set by
!             the user to contain the components of the gradient G at
!             the point X.
! 
!     DIAGCO  Removed
! 
!     DIAG    Local
! 
!     IPRINT  is an INTEGER array of length two which must be set by the
!             user.
! 
!             IPRINT(1) specifies the frequency of the output:
!                IPRINT(1) < 0 : no output is generated,
!                IPRINT(1) = 0 : output only at first and last iteration,
!                IPRINT(1) > 0 : output every IPRINT(1) iterations.
! 
!             IPRINT(2) specifies the type of output generated:
!                IPRINT(2) = 0 : iteration count, number of function 
!                                evaluations, function value, norm of the
!                                gradient, and steplength,
!                IPRINT(2) = 1 : same as IPRINT(2)=0, plus vector of
!                                variables and  gradient vector at the
!                                initial point,
!                IPRINT(2) = 2 : same as IPRINT(2)=1, plus vector of
!                                variables,
!                IPRINT(2) = 3 : same as IPRINT(2)=2, plus gradient vector.
! 
! 
!     EPS     is a positive DOUBLE PRECISION variable that must be set by
!             the user, and determines the accuracy with which the solution
!             is to be found. The subroutine terminates when
!
!                         ||G|| < EPS max(1,||X||),
!
!             where ||.|| denotes the Euclidean norm.
! 
!     XTOL    is a  positive DOUBLE PRECISION variable that must be set by
!             the user to an estimate of the machine precision (e.g.
!             10**(-16) on a SUN station 3/60). The line search routine will
!             terminate if the relative width of the interval of uncertainty
!             is less than XTOL.
! 
!     W       is a DOUBLE PRECISION array of length N(2M+1)+2M used as
!             workspace for LBFGS. This array must not be altered by the
!             user.
! 
!     IFLAG   is an INTEGER variable that must be set to 0 on initial entry
!             to the subroutine. A return with IFLAG<0 indicates an error,
!             and IFLAG=0 indicates that the routine has terminated without
!             detecting errors.
! 
!             The following negative values of IFLAG, detecting an error,
!             are possible:
! 
!              IFLAG=-1  The line search routine MCSRCH failed. The
!                        parameter INFO provides more detailed information
!                        (see also the documentation of MCSRCH):
!
!                       INFO = 0  IMPROPER INPUT PARAMETERS.
!
!                       INFO = 2  RELATIVE WIDTH OF THE INTERVAL OF
!                                 UNCERTAINTY IS AT MOST XTOL.
!
!                       INFO = 3  MORE THAN 20 FUNCTION EVALUATIONS WERE
!                                 REQUIRED AT THE PRESENT ITERATION.
!
!                       INFO = 4  THE STEP IS TOO SMALL.
!
!                       INFO = 5  THE STEP IS TOO LARGE.
!
!                       INFO = 6  ROUNDING ERRORS PREVENT FURTHER PROGRESS. 
!                                 THERE MAY NOT BE A STEP WHICH SATISFIES
!                                 THE SUFFICIENT DECREASE AND CURVATURE
!                                 CONDITIONS. TOLERANCES MAY BE TOO SMALL.
!
! 
!              IFLAG=-3  Improper input parameters for LBFGS (N or M are
!                        not positive).
! 
!
!
!     The subroutine contains one common area, which the user may wish to
!    reference:
! 
!    MP  is an INTEGER variable with default value 6. It is used as the
!        unit number for the printing of the monitoring information
!        controlled by IPRINT.
! 
!    LP  is an INTEGER variable with default value 6. It is used as the
!        unit number for the printing of error messages. This printing
!        may be suppressed by setting LP to a non-positive value.
! 
!    GTOL is a DOUBLE PRECISION variable with default value 0.9, which
!        controls the accuracy of the line search routine MCSRCH. If the
!        function and gradient evaluations are inexpensive with respect
!        to the cost of the iteration (which is sometimes the case when
!        solving very large problems) it may be advantageous to set GTOL
!        to a small value. A typical small value is 0.1.  Restriction:
!        GTOL should be greater than 1.D-04.
! 
!    STPMIN and STPMAX are non-negative DOUBLE PRECISION variables which
!        specify lower and uper bounds for the step in the line search.
!        Their default values are 1.D-20 and 1.D+20, respectively. These
!        values need not be modified unless the exponents are too large
!        for the machine being used, or unless the problem is extremely
!        badly scaled (in which case the exponents should be increased).
! 
!
!  MACHINE DEPENDENCIES
!
!        The only variables that are machine-dependent are XTOL,
!        STPMIN and STPMAX.
! 
!
!  GENERAL INFORMATION
! 
!    Input/Output  :  No input; diagnostic messages on unit MP and
!                     error messages on unit LP.
! 
!     - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

use qcmod, only: nlnqc_iter
use jfunc, only: iter,jiter,niter_no_qc

implicit none

type(control_vector), intent(inout) :: xhat,grad
real(r_kind)        , intent(inout) :: cost
real(r_kind)        , intent(inout) :: EPS
integer(i_kind)     , intent(in   ) :: MAXFEV
integer(i_kind)     , intent(in   ) :: maxvecs
integer(i_kind)     , intent(in   ) :: nprt

type(control_vector) :: diag, wy(maxvecs), ws(maxvecs), ww
real(r_kind) :: rho(maxvecs),alpha(maxvecs)

real(r_kind) :: GNORM,STP1,STP,YS,YY,SQ,YR,BETA,XNORM
integer(i_kind) :: NFUN,POINT,INFO,BOUND,NPT,CP,I,NFEV,ii,jj
real(r_kind) :: gamk
logical :: lldone
!
!     INITIALIZE
!     ----------
!
IF (maxvecs<=0) then
   write(6,*)'LBFGS: maxvecs is not positive.',maxvecs
   call stop2(158)
end if
IF (GTOL<1.0e-04_r_kind) then
   write(6,*)'LBFGS: GTOL is smaller than 1.0e-4.'
   call stop2(159)
end if

call allocate_cv(diag)
call allocate_cv(ww)
do ii=1,maxvecs
   call allocate_cv(ws(ii))
   call allocate_cv(wy(ii))
enddo

ITER= 0
NFUN= 1
POINT= 1
lldone= .FALSE.

!diag=zero

DO jj=1,ws(1)%lencv
   WS(1)%values(jj)= -grad%values(jj)
end do

gnorm = sqrt( dot_product(grad,grad) )
STP1= ONE/GNORM
!
!    --------------------
!     MAIN ITERATION LOOP
!    --------------------
main_loop: DO WHILE (.not.lldone)

   nlnqc_iter = iter >= niter_no_qc(jiter)
   ITER= ITER+1
   if (mype==0) write(6,*)'Minimization iteration',iter
   BOUND=ITER-1
   IF (ITER==1) GO TO 165
   IF (ITER>maxvecs) BOUND=maxvecs

   YS=dot_product(WY(NPT),WS(NPT))
   YY=dot_product(WY(NPT),WY(NPT))
   gamk=YS/YY

!  COMPUTE -H*G USING THE FORMULA GIVEN IN: Nocedal, J. 1980,
!  "Updating quasi-Newton matrices with limited storage",
!  Mathematics of Computation, Vol.24, No.151, pp. 773-782.
!  ---------------------------------------------------------

   CP = POINT
   rho(CP)= ONE/YS
   DO jj=1,ww%lencv
      WW%values(jj) = -grad%values(jj)
   end do

   DO I=1,BOUND
      CP=CP-1
      IF (CP==0) CP=maxvecs
      SQ=dot_product(WS(CP),WW)
      alpha(CP)=rho(CP)*SQ
      CALL AXPY(-alpha(CP),WY(CP),WW)
   end do
!
   DO jj=1,ww%lencv
!     WW%values(jj)=DIAG%values(jj)*WW%values(jj)
      WW%values(jj)=gamk*WW%values(jj)
   end do
!
   DO I=1,BOUND
      YR=dot_product(WY(CP),WW)
      BETA= rho(CP)*YR
      BETA= alpha(CP)-BETA
      CALL AXPY(BETA,WS(CP),WW)
      CP=CP+1
      IF (CP>maxvecs) CP=1
   end do
!
!  STORE THE NEW SEARCH DIRECTION
!  ------------------------------
!
   WS(POINT) = WW
!
!  OBTAIN THE ONE-DIMENSIONAL MINIMIZER OF THE FUNCTION 
!  BY USING THE LINE SEARCH ROUTINE MCSRCH
!  ----------------------------------------------------
 165  CONTINUE
   NFEV=0
   INFO=0
   STP=ONE
   IF (ITER==1) STP=STP1
   WW=grad

   CALL MCSRCH(xhat,cost,grad,WS(POINT),STP,MAXFEV,INFO,NFEV,DIAG,nprt)

   IF (INFO/=1.and.info/=3) then
      WRITE(6,200) INFO
      write(6,*)'LBFGS: line search failed'
      call stop2(160)
   endif
   NFUN= NFUN + NFEV
!
!  COMPUTE THE NEW STEP AND GRADIENT CHANGE 
!  -----------------------------------------
!
   NPT=POINT

   do jj=1,ws(npt)%lencv
      WS(NPT)%values(jj)= STP*WS(NPT)%values(jj)
      WY(NPT)%values(jj)= grad%values(jj)-WW%values(jj)
   enddo

   POINT=POINT+1
   IF (POINT>maxvecs) POINT=1
!
!  TERMINATION TEST
!  ----------------
!
   gnorm = sqrt( dot_product(grad,grad) )
   xnorm = sqrt( dot_product(xhat,xhat) )
 
   XNORM= MAX(one,XNORM)
   IF (GNORM/XNORM<=EPS) lldone=.TRUE.
   IF (NFUN>=MAXFEV) lldone=.true.

   if (mype==0) then
      write(6,999)'LBFGS iter,step,gnorm=',iter,stp,gnorm
   endif

end do main_loop
!
!     ------------------------------------------------------------
!     END OF MAIN ITERATION LOOP.
!     ------------------------------------------------------------

! Release memory
call deallocate_cv(diag)
call deallocate_cv(ww)
do ii=1,maxvecs
   call deallocate_cv(ws(ii))
   call deallocate_cv(wy(ii))
enddo

888 format(A,3(1X,ES24.18))
999 format(A,1X,I3,3(1X,ES24.18))
200 FORMAT(/' IFLAG= -1 ',/' LINE SEARCH FAILED. SEE', &
   &          ' DOCUMENTATION OF ROUTINE MCSRCH',/' ERROR RETURN', &
   &          ' OF LINE SEARCH: INFO= ',I2,/ &
   &          ' POSSIBLE CAUSES: FUNCTION OR GRADIENT ARE INCORRECT',/, &
   &          ' OR INCORRECT TOLERANCES')

RETURN
END SUBROUTINE LBFGS
! ------------------------------------------------------------------------------
SUBROUTINE MCSRCH(X,F,G,S,STP,MAXFEV,INFO,NFEV,WA,nprt)

use constants, only: half,four
implicit none

integer(i_kind)     , intent(in   ) :: MAXFEV,nprt
integer(i_kind)     , intent(inout) :: NFEV,INFO
real(r_kind)        , intent(inout) :: F,STP
type(control_vector), intent(inout) :: X,G,S,WA
!
!                     SUBROUTINE MCSRCH
!                
!     A slight modification of the subroutine CSRCH of More' and Thuente.
!     The changes are to allow reverse communication, and do not affect
!     the performance of the routine. 
!
!     THE PURPOSE OF MCSRCH IS TO FIND A STEP WHICH SATISFIES
!     A SUFFICIENT DECREASE CONDITION AND A CURVATURE CONDITION.
!
!     AT EACH STAGE THE SUBROUTINE UPDATES AN INTERVAL OF
!     UNCERTAINTY WITH ENDPOINTS STX AND STY. THE INTERVAL OF
!     UNCERTAINTY IS INITIALLY CHOSEN SO THAT IT CONTAINS A
!     MINIMIZER OF THE MODIFIED FUNCTION
!
!          F(X+STP*S) - F(X) - FTOL*STP*(GRADF(X)'S).
!
!     IF A STEP IS OBTAINED FOR WHICH THE MODIFIED FUNCTION
!     HAS A NONPOSITIVE FUNCTION VALUE AND NONNEGATIVE DERIVATIVE,
!     THEN THE INTERVAL OF UNCERTAINTY IS CHOSEN SO THAT IT
!     CONTAINS A MINIMIZER OF F(X+STP*S).
!
!     THE ALGORITHM IS DESIGNED TO FIND A STEP WHICH SATISFIES
!     THE SUFFICIENT DECREASE CONDITION
!
!           F(X+STP*S) .LE. F(X) + FTOL*STP*(GRADF(X)'S),
!
!     AND THE CURVATURE CONDITION
!
!           ABS(GRADF(X+STP*S)'S)) .LE. GTOL*ABS(GRADF(X)'S).
!
!     IF FTOL IS LESS THAN GTOL AND IF, FOR EXAMPLE, THE FUNCTION
!     IS BOUNDED BELOW, THEN THERE IS ALWAYS A STEP WHICH SATISFIES
!     BOTH CONDITIONS. IF NO STEP CAN BE FOUND WHICH SATISFIES BOTH
!     CONDITIONS, THEN THE ALGORITHM USUALLY STOPS WHEN ROUNDING
!     ERRORS PREVENT FURTHER PROGRESS. IN THIS CASE STP ONLY
!     SATISFIES THE SUFFICIENT DECREASE CONDITION.
!
!     THE SUBROUTINE STATEMENT IS
!
!        SUBROUTINE MCSRCH(N,X,F,G,S,STP,FTOL,XTOL, MAXFEV,INFO,NFEV,WA)
!     WHERE
!
!       N IS A POSITIVE INTEGER INPUT VARIABLE SET TO THE NUMBER
!         OF VARIABLES.
!
!       X IS AN ARRAY OF LENGTH N. ON INPUT IT MUST CONTAIN THE
!         BASE POINT FOR THE LINE SEARCH. ON OUTPUT IT CONTAINS
!         X + STP*S.
!
!       F IS A VARIABLE. ON INPUT IT MUST CONTAIN THE VALUE OF F
!         AT X. ON OUTPUT IT CONTAINS THE VALUE OF F AT X + STP*S.
!
!       G IS AN ARRAY OF LENGTH N. ON INPUT IT MUST CONTAIN THE
!         GRADIENT OF F AT X. ON OUTPUT IT CONTAINS THE GRADIENT
!         OF F AT X + STP*S.
!
!       S IS AN INPUT ARRAY OF LENGTH N WHICH SPECIFIES THE
!         SEARCH DIRECTION.
!
!       STP IS A NONNEGATIVE VARIABLE. ON INPUT STP CONTAINS AN
!         INITIAL ESTIMATE OF A SATISFACTORY STEP. ON OUTPUT
!         STP CONTAINS THE FINAL ESTIMATE.
!
!       FTOL AND GTOL ARE NONNEGATIVE INPUT VARIABLES. (In this reverse
!         communication implementation GTOL is defined in a COMMON
!         statement.) TERMINATION OCCURS WHEN THE SUFFICIENT DECREASE
!         CONDITION AND THE DIRECTIONAL DERIVATIVE CONDITION ARE
!         SATISFIED.
!
!       XTOL IS A NONNEGATIVE INPUT VARIABLE. TERMINATION OCCURS
!         WHEN THE RELATIVE WIDTH OF THE INTERVAL OF UNCERTAINTY
!         IS AT MOST XTOL.
!
!       STPMIN AND STPMAX ARE NONNEGATIVE INPUT VARIABLES WHICH
!         SPECIFY LOWER AND UPPER BOUNDS FOR THE STEP. (In this reverse
!         communication implementatin they are defined in a COMMON
!         statement).
!
!       MAXFEV IS A POSITIVE INTEGER INPUT VARIABLE. TERMINATION
!         OCCURS WHEN THE NUMBER OF CALLS TO FCN IS AT LEAST
!         MAXFEV BY THE END OF AN ITERATION.
!
!       INFO IS AN INTEGER OUTPUT VARIABLE SET AS FOLLOWS:
!
!         INFO = 0  IMPROPER INPUT PARAMETERS.
!
!         INFO =-1  A RETURN IS MADE TO COMPUTE THE FUNCTION AND GRADIENT.
!
!         INFO = 1  THE SUFFICIENT DECREASE CONDITION AND THE
!                   DIRECTIONAL DERIVATIVE CONDITION HOLD.
!
!         INFO = 2  RELATIVE WIDTH OF THE INTERVAL OF UNCERTAINTY
!                   IS AT MOST XTOL.
!
!         INFO = 3  NUMBER OF CALLS TO FCN HAS REACHED MAXFEV.
!
!         INFO = 4  THE STEP IS AT THE LOWER BOUND STPMIN.
!
!         INFO = 5  THE STEP IS AT THE UPPER BOUND STPMAX.
!
!         INFO = 6  ROUNDING ERRORS PREVENT FURTHER PROGRESS.
!                   THERE MAY NOT BE A STEP WHICH SATISFIES THE
!                   SUFFICIENT DECREASE AND CURVATURE CONDITIONS.
!                   TOLERANCES MAY BE TOO SMALL.
!
!       NFEV IS AN INTEGER OUTPUT VARIABLE SET TO THE NUMBER OF
!         CALLS TO FCN.
!
!       WA IS A WORK ARRAY OF LENGTH N.
!
!     SUBPROGRAMS CALLED
!
!       MCSTEP
!
!       FORTRAN-SUPPLIED...ABS,MAX,MIN
!
!     ARGONNE NATIONAL LABORATORY. MINPACK PROJECT. JUNE 1983
!     JORGE J. MORE', DAVID J. THUENTE
!
! REVISION HISTORY:
!
!  2009-01-18 Todling - minimal change to interface w/ evaljgrad (quad) properly
!                       NOTE: no attempt made to make code reproduce across pe's yet   
!
!     **********
integer(i_kind) :: INFOC,jj,ifev
LOGICAL BRACKT,STAGE1,lsavinc
real(r_kind) :: DG,DGM,DGINIT,DGTEST,DGX,DGXM,DGY,DGYM
real(r_kind) :: FINIT,FTEST1,FM,FX,FXM,FY,FYM,STX,STY
real(r_kind) :: STMIN,STMAX,WIDTH,WIDTH1
real(r_quad) :: FQUAD

real(r_kind), parameter :: p66=0.66_r_kind
real(r_kind), parameter :: xtrapf=four

INFOC = 1
!
!     CHECK THE INPUT PARAMETERS FOR ERRORS.
!
IF (STP<=ZERO .OR. FTOL<ZERO .OR. &
   & GTOL<ZERO .OR. XTOL<ZERO .OR. STPMIN<ZERO &
   & .OR. STPMAX<STPMIN .OR. MAXFEV<=0) then
   write(6,*)'MCSRCH: Error input',stp,ftol,gtol,xtol,stpmin,stpmax,maxfev
   call stop2(161)
endif
!
!     COMPUTE THE INITIAL GRADIENT IN THE SEARCH DIRECTION
!     AND CHECK THAT S IS A DESCENT DIRECTION.
!
dginit = dot_product(g,s)

IF (DGINIT>=ZERO) then
   write(6,*)'MCSRCH: THE SEARCH DIRECTION IS NOT A DESCENT DIRECTION',dginit
   call stop2(162)
ENDIF
!
!     INITIALIZE LOCAL VARIABLES.
!
BRACKT = .FALSE.
STAGE1 = .TRUE.
FINIT  = F
DGTEST = FTOL*DGINIT
WIDTH  = STPMAX - STPMIN
WIDTH1 = WIDTH/half
wa     = x
!
!     THE VARIABLES STX, FX, DGX CONTAIN THE VALUES OF THE STEP,
!     FUNCTION, AND DIRECTIONAL DERIVATIVE AT THE BEST STEP.
!     THE VARIABLES STY, FY, DGY CONTAIN THE VALUE OF THE STEP,
!     FUNCTION, AND DERIVATIVE AT THE OTHER ENDPOINT OF
!     THE INTERVAL OF UNCERTAINTY.
!     THE VARIABLES STP, F, DG CONTAIN THE VALUES OF THE STEP,
!     FUNCTION, AND DERIVATIVE AT THE CURRENT STEP.
!
STX = ZERO
FX  = FINIT
DGX = DGINIT
STY = ZERO
FY  = FINIT
DGY = DGINIT
!
!     START OF ITERATION.
!
do ifev=1,maxfev
!
!        SET THE MINIMUM AND MAXIMUM STEPS TO CORRESPOND
!        TO THE PRESENT INTERVAL OF UNCERTAINTY.
!
   IF (BRACKT) THEN
      STMIN = MIN(STX,STY)
      STMAX = MAX(STX,STY)
   ELSE
      STMIN = STX
      STMAX = STP + XTRAPF*(STP - STX)
   END IF
!
!        FORCE THE STEP TO BE WITHIN THE BOUNDS STPMAX AND STPMIN.
!
   STP = MAX(STP,STPMIN)
   STP = MIN(STP,STPMAX)
!
!        IF AN UNUSUAL TERMINATION IS TO OCCUR THEN LET
!        STP BE THE LOWEST POINT OBTAINED SO FAR.
!
   IF ((BRACKT .AND. (STP<=STMIN .OR. STP>=STMAX)) &
    &  .OR. iFEV>=MAXFEV-1 .OR. INFOC==0 &
    &  .OR. (BRACKT .AND. STMAX-STMIN<=XTOL*STMAX)) STP = STX
!
!        EVALUATE THE FUNCTION AND GRADIENT AT STP
!        AND COMPUTE THE DIRECTIONAL DERIVATIVE.
!
   DO jj=1,x%lencv
      X%values(jj) = WA%values(jj) + STP*S%values(jj)
   end do
!
   lsavinc=.false.
   call evaljgrad(x,fquad,g,lsavinc,nprt,'MCSRCH')
   f=fquad

   dg = dot_product(g,s)
   FTEST1 = FINIT + STP*DGTEST
!
!        TEST FOR CONVERGENCE.
!
   IF ((BRACKT .AND. (STP<=STMIN .OR. STP>=STMAX)) .OR. INFOC==0) INFO = 6
   IF (STP==STPMAX .AND. F<=FTEST1 .AND. DG<=DGTEST) INFO = 5
   IF (STP==STPMIN .AND. (F>FTEST1 .OR. DG>=DGTEST)) INFO = 4
   IF (iFEV>=MAXFEV) INFO = 3
   IF (BRACKT .AND. STMAX-STMIN<=XTOL*STMAX) INFO = 2
   IF (F<=FTEST1 .AND. ABS(DG)<=GTOL*(-DGINIT)) INFO = 1
!
!        CHECK FOR TERMINATION.
!
   IF (INFO/=0) exit
!
!        IN THE FIRST STAGE WE SEEK A STEP FOR WHICH THE MODIFIED
!        FUNCTION HAS A NONPOSITIVE VALUE AND NONNEGATIVE DERIVATIVE.
!
   IF (STAGE1 .AND. F<=FTEST1 .AND. DG>=MIN(FTOL,GTOL)*DGINIT) STAGE1=.FALSE.
!
!        A MODIFIED FUNCTION IS USED TO PREDICT THE STEP ONLY IF
!        WE HAVE NOT OBTAINED A STEP FOR WHICH THE MODIFIED
!        FUNCTION HAS A NONPOSITIVE FUNCTION VALUE AND NONNEGATIVE
!        DERIVATIVE, AND IF A LOWER FUNCTION VALUE HAS BEEN
!        OBTAINED BUT THE DECREASE IS NOT SUFFICIENT.
!
   IF (STAGE1 .AND. F<=FX .AND. F>FTEST1) THEN
!
!           DEFINE THE MODIFIED FUNCTION AND DERIVATIVE VALUES.
!
      FM   = F   - STP*DGTEST
      FXM  = FX  - STX*DGTEST
      FYM  = FY  - STY*DGTEST
      DGM  = DG  - DGTEST
      DGXM = DGX - DGTEST
      DGYM = DGY - DGTEST
!
!           CALL CSTEP TO UPDATE THE INTERVAL OF UNCERTAINTY
!           AND TO COMPUTE THE NEW STEP.
!
      CALL MCSTEP(STX,FXM,DGXM,STY,FYM,DGYM,STP,FM,DGM, &
       &          BRACKT,STMIN,STMAX,INFOC)
!
!           RESET THE FUNCTION AND GRADIENT VALUES FOR F.
!
      FX  = FXM  + STX*DGTEST
      FY  = FYM  + STY*DGTEST
      DGX = DGXM + DGTEST
      DGY = DGYM + DGTEST
   ELSE
!
!           CALL MCSTEP TO UPDATE THE INTERVAL OF UNCERTAINTY
!           AND TO COMPUTE THE NEW STEP.
!
      CALL MCSTEP(STX,FX,DGX,STY,FY,DGY,STP,F,DG, &
       &         BRACKT,STMIN,STMAX,INFOC)
   END IF
!
!        FORCE A SUFFICIENT DECREASE IN THE SIZE OF THE
!        INTERVAL OF UNCERTAINTY.
!
   IF (BRACKT) THEN
      IF (ABS(STY-STX)>=P66*WIDTH1) STP = STX + half*(STY - STX)
      WIDTH1 = WIDTH
      WIDTH  = ABS(STY-STX)
   END IF

end do

NFEV=ifev

return
END SUBROUTINE MCSRCH
! ------------------------------------------------------------------------------
SUBROUTINE MCSTEP(STX,FX,DX,STY,FY,DY,STP,FP,DP,BRACKT, &
                & STPMIN,STPMAX,INFO)

use constants, only: three
implicit none

real(r_kind)   , intent(inout) :: STX,FX,DX,STY,FY,DY,STP,FP,DP,STPMIN,STPMAX
integer(i_kind), intent(  out) :: INFO
logical        , intent(inout) :: BRACKT
!
!     SUBROUTINE MCSTEP
!
!     THE PURPOSE OF MCSTEP IS TO COMPUTE A SAFEGUARDED STEP FOR
!     A LINESEARCH AND TO UPDATE AN INTERVAL OF UNCERTAINTY FOR
!     A MINIMIZER OF THE FUNCTION.
!
!     THE PARAMETER STX CONTAINS THE STEP WITH THE LEAST FUNCTION
!     VALUE. THE PARAMETER STP CONTAINS THE CURRENT STEP. IT IS
!     ASSUMED THAT THE DERIVATIVE AT STX IS NEGATIVE IN THE
!     DIRECTION OF THE STEP. IF BRACKT IS SET TRUE THEN A
!     MINIMIZER HAS BEEN BRACKETED IN AN INTERVAL OF UNCERTAINTY
!     WITH ENDPOINTS STX AND STY.
!
!     THE SUBROUTINE STATEMENT IS
!
!       SUBROUTINE MCSTEP(STX,FX,DX,STY,FY,DY,STP,FP,DP,BRACKT,
!                        STPMIN,STPMAX,INFO)
!
!     WHERE
!
!       STX, FX, AND DX ARE VARIABLES WHICH SPECIFY THE STEP,
!         THE FUNCTION, AND THE DERIVATIVE AT THE BEST STEP OBTAINED
!         SO FAR. THE DERIVATIVE MUST BE NEGATIVE IN THE DIRECTION
!         OF THE STEP, THAT IS, DX AND STP-STX MUST HAVE OPPOSITE
!         SIGNS. ON OUTPUT THESE PARAMETERS ARE UPDATED APPROPRIATELY.
!
!       STY, FY, AND DY ARE VARIABLES WHICH SPECIFY THE STEP,
!         THE FUNCTION, AND THE DERIVATIVE AT THE OTHER ENDPOINT OF
!         THE INTERVAL OF UNCERTAINTY. ON OUTPUT THESE PARAMETERS ARE
!         UPDATED APPROPRIATELY.
!
!       STP, FP, AND DP ARE VARIABLES WHICH SPECIFY THE STEP,
!         THE FUNCTION, AND THE DERIVATIVE AT THE CURRENT STEP.
!         IF BRACKT IS SET TRUE THEN ON INPUT STP MUST BE
!         BETWEEN STX AND STY. ON OUTPUT STP IS SET TO THE NEW STEP.
!
!       BRACKT IS A LOGICAL VARIABLE WHICH SPECIFIES IF A MINIMIZER
!         HAS BEEN BRACKETED. IF THE MINIMIZER HAS NOT BEEN BRACKETED
!         THEN ON INPUT BRACKT MUST BE SET FALSE. IF THE MINIMIZER
!         IS BRACKETED THEN ON OUTPUT BRACKT IS SET TRUE.
!
!       STPMIN AND STPMAX ARE INPUT VARIABLES WHICH SPECIFY LOWER
!         AND UPPER BOUNDS FOR THE STEP.
!
!       INFO IS AN INTEGER OUTPUT VARIABLE SET AS FOLLOWS:
!         IF INFO = 1,2,3,4,5, THEN THE STEP HAS BEEN COMPUTED
!         ACCORDING TO ONE OF THE FIVE CASES BELOW. OTHERWISE
!         INFO = 0, AND THIS INDICATES IMPROPER INPUT PARAMETERS.
!
!     SUBPROGRAMS CALLED
!
!       FORTRAN-SUPPLIED ... ABS,MAX,MIN,SQRT
!
!     ARGONNE NATIONAL LABORATORY. MINPACK PROJECT. JUNE 1983
!     JORGE J. MORE', DAVID J. THUENTE
!
real(r_kind) :: GAMMA,P,Q,R,S,SGND,STPC,STPF,STPQ,THETA
LOGICAL BOUND
! ------------------------------------------------------------------------------
INFO = 0
!
!     CHECK THE INPUT PARAMETERS FOR ERRORS.
!
IF ((BRACKT .AND. (STP<=MIN(STX,STY) .OR. &
   &  STP>=MAX(STX,STY))) .OR. &
   &  DX*(STP-STX)>=zero .OR. STPMAX<STPMIN) then
   write(6,*)'MCSTEP: error in input values',BRACKT,STP,STX,STY,DX,STPMAX,STPMIN
   call stop2(163)
end if
!
!     DETERMINE IF THE DERIVATIVES HAVE OPPOSITE SIGN.
!
SGND = DP*(DX/ABS(DX))
!
!     FIRST CASE. A HIGHER FUNCTION VALUE.
!     THE MINIMUM IS BRACKETED. IF THE CUBIC STEP IS CLOSER
!     TO STX THAN THE QUADRATIC STEP, THE CUBIC STEP IS TAKEN,
!     ELSE THE AVERAGE OF THE CUBIC AND QUADRATIC STEPS IS TAKEN.
!
IF (FP>FX) THEN
   INFO  = 1
   BOUND = .TRUE.
   THETA = three*(FX - FP)/(STP - STX) + DX + DP
   S     = MAX(ABS(THETA),ABS(DX),ABS(DP))
   GAMMA = S*SQRT((THETA/S)**2 - (DX/S)*(DP/S))
   IF (STP<STX) GAMMA = -GAMMA
   P    =  (GAMMA - DX) + THETA
   Q    = ((GAMMA - DX) + GAMMA) + DP
   R    = P/Q
   STPC = STX + R*(STP - STX)
   STPQ = STX + ((DX/((FX-FP)/(STP-STX)+DX))/2)*(STP - STX)
   IF (ABS(STPC-STX)<ABS(STPQ-STX)) THEN
      STPF = STPC
   ELSE
      STPF = STPC + (STPQ - STPC)/2
   END IF
   BRACKT = .TRUE.
!
!     SECOND CASE. A LOWER FUNCTION VALUE AND DERIVATIVES OF
!     OPPOSITE SIGN. THE MINIMUM IS BRACKETED. IF THE CUBIC
!     STEP IS CLOSER TO STX THAN THE QUADRATIC (SECANT) STEP,
!     THE CUBIC STEP IS TAKEN, ELSE THE QUADRATIC STEP IS TAKEN.
!
ELSE IF (SGND<zero) THEN
   INFO  = 2
   BOUND = .FALSE.
   THETA = three*(FX - FP)/(STP - STX) + DX + DP
   S     = MAX(ABS(THETA),ABS(DX),ABS(DP))
   GAMMA = S*SQRT((THETA/S)**2 - (DX/S)*(DP/S))
   IF (STP>STX) GAMMA = -GAMMA
   P    =  (GAMMA - DP) + THETA
   Q    = ((GAMMA - DP) + GAMMA) + DX
   R    = P/Q
   STPC = STP + R*(STX - STP)
   STPQ = STP + (DP/(DP-DX))*(STX - STP)
   IF (ABS(STPC-STP)>ABS(STPQ-STP)) THEN
      STPF = STPC
   ELSE
      STPF = STPQ
   END IF
   BRACKT = .TRUE.
!
!     THIRD CASE. A LOWER FUNCTION VALUE, DERIVATIVES OF THE
!     SAME SIGN, AND THE MAGNITUDE OF THE DERIVATIVE DECREASES.
!     THE CUBIC STEP IS ONLY USED IF THE CUBIC TENDS TO INFINITY
!     IN THE DIRECTION OF THE STEP OR IF THE MINIMUM OF THE CUBIC
!     IS BEYOND STP. OTHERWISE THE CUBIC STEP IS DEFINED TO BE
!     EITHER STPMIN OR STPMAX. THE QUADRATIC (SECANT) STEP IS ALSO
!     COMPUTED AND IF THE MINIMUM IS BRACKETED THEN THE THE STEP
!     CLOSEST TO STX IS TAKEN, ELSE THE STEP FARTHEST AWAY IS TAKEN.
!
ELSE IF (ABS(DP)<ABS(DX)) THEN
   INFO  = 3
   BOUND = .TRUE.
   THETA = three*(FX - FP)/(STP - STX) + DX + DP
   S     = MAX(ABS(THETA),ABS(DX),ABS(DP))
!
!        THE CASE GAMMA = 0 ONLY ARISES IF THE CUBIC DOES NOT TEND
!        TO INFINITY IN THE DIRECTION OF THE STEP.
!
   GAMMA = S*SQRT(MAX(0.0D0,(THETA/S)**2 - (DX/S)*(DP/S)))
   IF (STP>STX) GAMMA = -GAMMA
   P =  (GAMMA - DP) + THETA
   Q = (GAMMA + (DX - DP)) + GAMMA
   R = P/Q
   IF (R<zero .AND. GAMMA/=zero) THEN
      STPC = STP + R*(STX - STP)
   ELSE IF (STP>STX) THEN
      STPC = STPMAX
   ELSE
      STPC = STPMIN
   END IF
   STPQ = STP + (DP/(DP-DX))*(STX - STP)
   IF (BRACKT) THEN
      IF (ABS(STP-STPC)<ABS(STP-STPQ)) THEN
         STPF = STPC
      ELSE
         STPF = STPQ
      END IF
   ELSE
      IF (ABS(STP-STPC)>ABS(STP-STPQ)) THEN
         STPF = STPC
      ELSE
         STPF = STPQ
      END IF
   END IF
!
!     FOURTH CASE. A LOWER FUNCTION VALUE, DERIVATIVES OF THE
!     SAME SIGN, AND THE MAGNITUDE OF THE DERIVATIVE DOES
!     NOT DECREASE. IF THE MINIMUM IS NOT BRACKETED, THE STEP
!     IS EITHER STPMIN OR STPMAX, ELSE THE CUBIC STEP IS TAKEN.
!
ELSE
   INFO = 4
   BOUND = .FALSE.
   IF (BRACKT) THEN
      THETA = three*(FP - FY)/(STY - STP) + DY + DP
      S = MAX(ABS(THETA),ABS(DY),ABS(DP))
      GAMMA = S*SQRT((THETA/S)**2 - (DY/S)*(DP/S))
      IF (STP>STY) GAMMA = -GAMMA
      P    =  (GAMMA - DP) + THETA
      Q    = ((GAMMA - DP) + GAMMA) + DY
      R    = P/Q
      STPC = STP + R*(STY - STP)
      STPF = STPC
   ELSE IF (STP>STX) THEN
      STPF = STPMAX
   ELSE
      STPF = STPMIN
   END IF
END IF
!
!     UPDATE THE INTERVAL OF UNCERTAINTY. THIS UPDATE DOES NOT
!     DEPEND ON THE NEW STEP OR THE CASE ANALYSIS ABOVE.
!
IF (FP>FX) THEN
   STY = STP
   FY  = FP
   DY  = DP
ELSE
   IF (SGND<zero) THEN
      STY = STX
      FY  = FX
      DY  = DX
   END IF
   STX = STP
   FX  = FP
   DX  = DP
END IF
!
!     COMPUTE THE NEW STEP AND SAFEGUARD IT.
!
STPF = MIN(STPMAX,STPF)
STPF = MAX(STPMIN,STPF)
STP  = STPF
IF (BRACKT .AND. BOUND) THEN
   IF (STY>STX) THEN
      STP = MIN(STX+0.66_r_kind*(STY-STX),STP)
   ELSE
      STP = MAX(STX+0.66_r_kind*(STY-STX),STP)
  END IF
END IF

RETURN
END SUBROUTINE MCSTEP
! ------------------------------------------------------------------------------
!  MATINV - Simplified interface to LAPACK routines SGETRF+SGETRS/DGETRF+DGETRS
! ------------------------------------------------------------------------------
subroutine matinv(pmat,pvec,kiter,kmaxit)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    matinv
!   prgmmr:
!
! abstract:
!
! program history log:
!   2009-08-10  lueken - added subprogram doc block
!
!   input argument list:
!    kiter,kmaxit
!    pmat
!    pvec
!
!   output argument list:
!    pmat
!    pvec
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

implicit none
integer(i_kind), intent(in   ) :: kiter,kmaxit
real(r_kind)   , intent(inout) :: pmat(2*kmaxit,2*kmaxit)
real(r_kind)   , intent(inout) :: pvec(2*kmaxit)

integer(i_kind) :: info,iwork(2*kmaxit)

if (r_kind==N_DEFAULT_REAL_KIND) then
   call SGETRF(2*kiter,2*kiter,pmat,2*kmaxit,iwork,info)
   if (info/=0) then
      write(6,*)'Error in qnewton: SGETRF returned info=',info
      call ABOR1('QNEWTON: SGETRF returned non-zero info')
   endif
   call SGETRS('N',2*kiter,1,pmat,2*kmaxit,iwork,pvec,2*kmaxit,info)
   if (info/=0) then
      write(6,*)'Error in qnewton: SGETRS returned info=',info
      call ABOR1('QNEWTON: SGETRS returned non-zero info')
   endif
ELSEIF (r_kind==N_DOUBLE_KIND) then
   call DGETRF(2*kiter,2*kiter,pmat,2*kmaxit,iwork,info)
   if (info/=0) then
      write(6,*)'Error in qnewton: DGETRF returned info=',info
      call ABOR1('QNEWTON: DGETRF returned non-zero info')
   endif
   call DGETRS('N',2*kiter,1,pmat,2*kmaxit,iwork,pvec,2*kmaxit,info)
   if (info/=0) then
      write(6,*)'Error in qnewton: DGETRS returned info=',info
      call ABOR1('QNEWTON: DGETRS returned non-zero info')
   endif
else
   call ABOR1('r_kind is neither default real nor double precision')
endif

end subroutine matinv
! ------------------------------------------------------------------------------
end module qnewton
