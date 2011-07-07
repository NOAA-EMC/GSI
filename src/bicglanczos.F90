module bicglanczos
!$$$ module documentation block
!           .      .    .                                       .
! module:   bicglanczos
!   prgmmr: tremolet
!
! abstract: Contains variables and routines for preconditioned
!           Lanczos minimizer based on Bi-CG
!
! program history log:
!   2009-08-10  tremolet
!   2010-10-01  el akkraoui - revisit original implementation (still w/o precond)
!   2011-04-19  el akkraoui - add preconditioning and orthogonalization
!
! Subroutines Included:
!   save_pcgprecond - Save eigenvectors for constructing the next preconditioner
!   setup_pcgprecond - Reads preconditioning
!   pcglanczos       - Actual Lanczos account for left and right eigen-problems
!   pcgprecond       - Apply preconditioning
!
! Variable Definitions:
!   LMPCGL  : .T. ====> precondition conjugate-gradient minimization
!   R_MAX_CNUM_PC : Maximum allowed condition number for the preconditioner
!   NPCVECS : number of vectors which make up the preconditioner
!
!   YVCGLPC: eigenvectors (from an earlier minimization)
!            that are used to construct the preconditioner.
!   RCGLPC : eigenvalues (from an earlier minimization)
!            that are used to construct the preconditioner.
!   NVCGLPC: the number of eigenpairs used to form the preconditioner.
!
!   YVCGLEV: eigenvectors for the current minimization.
!   RCGLEV : eigenvalues for the current minimization.
!   NVCGLEV: the number of eigenpairs for the current minimization.
!
!   YVCGLWK: work array of eigenvectors
!   YUCGLWK: work array of eigenvectors
!
! attributes:
!   language: f90
!   machine:
!   
! ------------------------------------------------------------------------------

!=============================================================
use kinds    , only : r_kind,i_kind,r_quad
use constants, only : zero, one, half,two, zero_quad
use timermod , only : timer_ini, timer_fnl
use lanczos  , only : save_precond
use gsi_4dvar, only : iorthomax
use control_vectors, only: control_vector
use control_vectors, only: allocate_cv,deallocate_cv,inquire_cv
use control_vectors, only: read_cv,write_cv
use control_vectors, only: dot_product,assignment(=)
use gsi_bundlemod, only: gsi_bundle
use gsi_bundlemod, only: assignment(=)
use mpimod  ,  only : mpi_comm_world
use mpimod,    only: mype
use jfunc   ,  only : iter, jiter
use gsi_4dvar, only : nwrvecs,l4dvar,lanczosave
use gsi_4dvar, only : nsubwin, nobs_bins

implicit none
private
save
public pcglanczos, setup_pcglanczos, save_pcgprecond, setup_pcgprecond,& 
       pcgprecond, LMPCGL

!=============================================================
logical      :: LMPCGL = .false.
real(r_kind) :: R_MAX_CNUM_PC = 10.0_r_kind
real(r_kind) :: xmin_ritz = 1.0_r_kind
real(r_kind) :: pkappa = 0.1_r_kind

integer(i_kind)           :: NPCVECS, NVCGLPC, NVCGLEV
REAL(r_kind), ALLOCATABLE :: RCGLPC(:)
REAL(r_kind), ALLOCATABLE :: RCGLEV(:)

integer(i_kind)           :: nprt,maxiter
REAL(r_kind), allocatable :: zlancs(:,:)

TYPE(control_vector), ALLOCATABLE, DIMENSION(:) :: YVCGLPC
TYPE(control_vector), ALLOCATABLE, DIMENSION(:) :: YVCGLEV
TYPE(control_vector), ALLOCATABLE, DIMENSION(:) :: YUCGLEV
TYPE(control_vector), ALLOCATABLE, DIMENSION(:) :: YVCGLWK
type(control_vector), allocatable, dimension(:) :: cglwork
type(control_vector), allocatable, dimension(:) :: cglworkhat

! --------------------------------------
REAL                       :: Z_DEFAULT_REAL      ! intentionally not real(r_kind)
integer(i_kind), PARAMETER :: N_DEFAULT_REAL_KIND = KIND(Z_DEFAULT_REAL)
DOUBLE PRECISION           :: DL_DOUBLE_PRECISION ! intentionally not real(r_double)
integer(i_kind), PARAMETER :: N_DOUBLE_KIND       = KIND(DL_DOUBLE_PRECISION)

!=============================================================
contains

!=============================================================
subroutine setup_pcglanczos(kpe,kprt,kiter,kiterstart,kmaxit,kwrvecs, &
                         ld4dvar,ldsave)
implicit none
integer(i_kind), intent(in) :: kpe,kprt,kiter,kiterstart,kmaxit,kwrvecs
logical        , intent(in) :: ld4dvar,ldsave
integer(i_kind)             :: ii

mype=kpe
nprt=kprt
jiter=kiter
maxiter=kmaxit
nwrvecs=kwrvecs
l4dvar=ld4dvar
lanczosave=ldsave

if (allocated(zlancs)) deallocate(zlancs)
allocate(zlancs(maxiter+1,4))
zlancs=zero

allocate(cglwork(maxiter+1))
DO ii=1,kmaxit+1
  CALL allocate_cv(cglwork(ii))
  cglwork(ii)=zero
ENDDO

allocate(cglworkhat(maxiter+1))
DO ii=1,kmaxit+1
   CALL allocate_cv(cglworkhat(ii))
   cglworkhat(ii)=zero
END DO 

if (jiter==kiterstart) then
  NPCVECS=0
  NVCGLPC=0
  NVCGLEV=0
endif

 if (jiter>1 .and. ldsave )then
    call setup_pcgprecond() !  Calculates the preconditioner for congrad
 endif

call inquire_cv

end subroutine setup_pcglanczos
! -------------------------------------------------------------------

!================================================================
subroutine pcglanczos(xhat,yhat,pcost,gradx,grady,preduc,kmaxit,iobsconv,lsavevecs)

!$$$  subprogram documentation block
!
! abstract: solve inner loop of analysis equation using conjugate
!           gradient preconditioned with B.
!
! program history log:
!   2999-99-99  tremolet - initial code
!
!$$$

! Notes:
! o The algorithm can be implemented without the vectors xtry and ytry 
!   to save memory. For now we keep them to make the code more readable.
! o It is possible to reduce further the number of vectors being used
!   by computing dirx=B*diry or xhat=B*yhat at the cost of another
!   product by the B matrix per iteration (likely to be more expensive
!   than the linear algebra to keep both sets of vectors updated).


implicit none

! Declare local variables
character(len=*), parameter :: myname='pcglanczos'
character(len=*), parameter :: subname='truecost'

type(control_vector),intent(inout)      :: xhat,yhat,gradx,grady
real(r_kind)    , intent(out)           :: pcost
real(r_kind)    , intent(inout)         :: preduc
integer(i_kind) , intent(inout)         :: kmaxit
integer(i_kind) , intent(in)            :: iobsconv
logical         , intent(in)            :: lsavevecs

type(control_vector)      :: grad0,xtry,ytry,gradw,dirx,diry,xtrue,gradtrue
real(r_kind), allocatable :: alpha(:),beta(:),delta(:),gam(:)
real(r_kind), allocatable :: zdiag(:),ztoff(:),zwork(:)
real(r_kind), allocatable :: zritz(:),zbnds(:),zevecs(:,:)
real(r_quad)              :: zg0,zgk,zgnew,zfk,zgg,zf0,zff,zgf,zge,zfg
real(r_kind)              :: zeta,zreqrd
integer(i_kind)           :: jj,ilen,ii,info
integer(i_kind)           :: kminit,kmaxevecs
logical                   :: lsavinc,lldone
character(len=12)         :: clfile

! --------------------------------------
REAL             :: Z_DEFAULT_REAL      ! intentionally not real(r_kind)
integer(i_kind), PARAMETER :: N_DEFAULT_REAL_KIND = KIND(Z_DEFAULT_REAL)
DOUBLE PRECISION :: DL_DOUBLE_PRECISION ! intentionally not real(r_double)
integer(i_kind), PARAMETER :: N_DOUBLE_KIND       = KIND(DL_DOUBLE_PRECISION)


!<<<
integer(i_kind) :: jk,isize,jm
real(r_kind)    :: zdla,zbndlm
REAL(r_kind),allocatable        :: zdp(:)

type(control_vector) :: gradf,gradff
type(gsi_bundle)     :: sval(nobs_bins)
type(gsi_bundle)     :: mval(nsubwin)
real(r_quad)         :: pcostq
integer              :: iortho
!---------------------------------------------------

#ifdef ibm_sp
    logical,allocatable:: select(:)
    integer(i_kind):: n,i,j,ldz
    integer(i_kind):: iopt,lda,naux
    integer(i_kind),allocatable:: indx(:)
    real(r_kind),allocatable:: aux(:),w_order(:)
    complex(r_kind),allocatable:: w(:),z(:)
#endif

! Initialize timer
call timer_ini('pcglanczos')

kminit =  kmaxit
kmaxevecs = kmaxit
lldone=.false.
if (kmaxit>maxiter) then
   write(6,*) 'setup_pcglanczos : kmaxit>maxiter', kmaxit,maxiter
   call stop2(138)
end if

if (mype==0) write(6,*) '---- BICG Solver -----'

! Allocate local variables
call allocate_cv(grad0) ! not in PCGSOI (use ydiff instead)
call allocate_cv(xtry)  ! not in PCGSOI
call allocate_cv(ytry)  ! not in PCGSOI
call allocate_cv(gradw)
call allocate_cv(dirx)
call allocate_cv(diry)
if(nprt>=1) call allocate_cv(gradf)
if(nprt>=1) call allocate_cv(gradff)

!--- 'zeta' is an upper bound on the relative error of the gradient.

zeta  = 1.0e-4_r_kind
zreqrd = preduc

ilen=xhat%lencv


allocate(alpha(kmaxit),beta(kmaxit),delta(kmaxit),gam(0:kmaxit))
alpha(:)=zero_quad
beta(:)=zero_quad

!$omp parallel do
do jj=1,ilen
  grad0%values(jj)=gradx%values(jj)
  dirx%values(jj)=-grady%values(jj)
  diry%values(jj)=-gradx%values(jj)
end do
!$omp end parallel do

if(LMPCGL) then 
   dirx=zero
   call pcgprecond(gradx,dirx,1)
   do jj=1,ilen
       dirx%values(jj) = -dirx%values(jj)
   end do
end if

zg0=dot_product(gradx,grady,r_quad)
zgk=zg0
zg0=sqrt(zg0)
gam(0)=one/zg0

zgg=dot_product(dirx,dirx,r_quad)
zgg=sqrt(zgg)
do jj=1,ilen
  cglwork(1)%values(jj)=gradx%values(jj)/zg0
  cglworkhat(1)%values(jj)=grady%values(jj)/zg0
enddo

! ------------------------------------------------------------------------------
! Perform CG inner iterations
! ------------------------------------------------------------------------------
inner_iteration: do iter=1,kmaxit
  if (mype==0) write(6,*)trim(myname),': Minimization iteration',iter

! Estimate
!  do jj=1,ilen
!    xtry%values(jj)=xhat%values(jj)+dirx%values(jj)
!    ytry%values(jj)=yhat%values(jj)+diry%values(jj)
!  end do
   xtry=dirx
   ytry=diry

! Apply the Hessian
  lsavinc=.false.
  call jgrad(xtry,ytry,zfk,gradw,lsavinc,nprt,myname)
  pcost=zfk

  
! Get A q_k
  do jj=1,ilen
    gradw%values(jj)=gradw%values(jj)-grad0%values(jj)
  end do

! Calculate stepsize
  delta(iter)=dot_product(dirx,gradw,r_quad)
  alpha(iter)=zgk/delta(iter)

! Update estimates
  do jj=1,ilen
    xhat%values(jj) =xhat%values(jj) +alpha(iter)*dirx%values(jj)
    yhat%values(jj) =yhat%values(jj) +alpha(iter)*diry%values(jj)
    gradx%values(jj)=gradx%values(jj)+alpha(iter)*gradw%values(jj)
  end do

! First re-orthogonalization
  
  if(iorthomax>0) then
    iortho=min(iter,iorthomax)
    do jm=iortho,1,-1
       zdla = DOT_PRODUCT(gradx,cglworkhat(jm))
       do jj=1,ilen
          gradx%values(jj) = gradx%values(jj) - zdla*cglwork(jm)%values(jj)
       end do
    end do
  end if

! Apply B or the preconditioner

  if(LMPCGL) then 
     call pcgprecond(gradx,grady,1)
  else 
     call bkerror(gradx,grady)
  endif

! Second re-orthogonalization  

  if(iorthomax>0) then
     iortho=min(iter,iorthomax)
     do jm=iortho,1,-1
        zdla = DOT_PRODUCT(grady,cglwork(jm))
        do jj=1,ilen
           grady%values(jj) = grady%values(jj) - zdla*cglworkhat(jm)%values(jj)
        end do
     end do
  end if

! Compute beta
  zgnew=dot_product(gradx,grady,r_quad)
  beta(iter)=zgnew/zgk
  zgk=zgnew

! Evaluate cost for printout
  if(nprt>=1) call jgrad(xhat,yhat,zfk,gradf,lsavinc,nprt,subname)
 
! Update search direction
  do jj=1,ilen
    dirx%values(jj)=-grady%values(jj)+beta(iter)*dirx%values(jj)
    diry%values(jj)=-gradx%values(jj)+beta(iter)*diry%values(jj)
  end do

! Diagnostics
  if(zgk < zero) then 
     if(mype==0) write(6,*) '*** STOP : Breakdown ***'
     STOP
  end if
  zgg=sqrt(zgk)
  gam(iter)=one/zgg
  if (mype==0) then
    write(6,999)trim(myname),': grepgrad cost,grad,reduction=', &
      jiter,iter,zfk,zgg,zgg/zg0
    write(6,999)trim(myname),': grepgrad alpha,beta=', &
      jiter,iter,alpha(iter),beta(iter)
  endif

! Save Lanczos vectors
  do jj=1,ilen
    cglwork(iter+1)%values(jj)=gradx%values(jj)/zgg
    cglworkhat(iter+1)%values(jj)=grady%values(jj)/zgg
  enddo

end do inner_iteration
! ------------------------------------------------------------------------------
! Done CG inner iterations
! ------------------------------------------------------------------------------

! ------------------------------------------------------------------------------
! Lanczos diagnostics
! ------------------------------------------------------------------------------
allocate(zdiag(0:kmaxit),ztoff(kmaxit),zwork(2*kmaxit))
allocate(zritz(0:kmaxit),zbnds(0:kmaxit))
allocate(zevecs(kmaxit+1,kmaxit+1))

! Build tri-diagonal matrix
zdiag(0)=delta(0)
do ii=1,kmaxit
  zdiag(ii)=delta(ii)+beta(ii)*beta(ii)*delta(ii-1)
enddo
do ii=0,kmaxit
  zdiag(ii)=zdiag(ii)*gam(ii)*gam(ii)
enddo
do ii=1,kmaxit
  ztoff(ii)=-beta(ii)*gam(ii)*gam(ii-1)*delta(ii-1)
enddo

zge=sqrt(DOT_PRODUCT(cglwork(kmaxit+1),cglwork(kmaxit+1)))
zge=zge* ztoff(kmaxit)

#ifdef ibm_sp

!   Use ESSL
    iopt=1
    n=kmaxit+1
    lda=n
    ldz=kmaxit+1
    naux=2*n
    allocate(select(n),indx(n),w(n),z(n),aux(naux),w_order(n))

!   Additional initializations
    select=.false.    ! select not used for iopt=1
    w=zero
    z=zero
    aux=zero

!   Call ESSL routines
    if (r_kind == N_DEFAULT_REAL_KIND) then
       call SGEEV(iopt, zdiag, lda, w, z, ldz, select, n, aux, naux)
       do i=1,n
          w_order(i)=real(w(i),r_kind)
       end do
       call SSORTX(w_order,1,n,indx)  ! sort eigenvalues into ascending order
    ELSEIF (r_kind == N_DOUBLE_KIND) then
       call DGEEV(iopt, zdiag, lda, w, z, ldz, select, n, aux, naux)
       do i=1,n
          w_order(i)=real(w(i),r_kind)
       end do
       call DSORTX(w_order,1,n,indx)  ! sort eigenvalues into ascending order
    else
       write(6,*)'STEQR: r_kind is neither default real nor double precision'
       call stop2(319)
    endif

!   Load ESSL eigenvalues and eigenvectors into output arrays
    do j=1,n
       zdiag(j)=w_order(j)          ! eigenvalues
       jj=indx(j)
       ztoff(j)=real(z(j),r_kind) ! eigenvectors
    end do

!   Deallocate work arrays
    deallocate(select,indx,w,z,aux,w_order)

#else

!   Use LAPACK
! Get eigen-pairs of tri-diagonal matrix
if(iter /= 1) then
   if (r_kind==N_DEFAULT_REAL_KIND) then
      call SSTEQR('I',kmaxit+1,zdiag,ztoff,zevecs,kmaxit+1,zwork,info)
   elseif (r_kind==N_DOUBLE_KIND) then
      call DSTEQR('I',kmaxit+1,zdiag,ztoff,zevecs,kmaxit+1,zwork,info)
   else
      write(6,*)trim(myname),': r_kind is neither default real nor double precision'
      call stop2(319)
   endif
else 
   zevecs(1,1) =one
endif
#endif

if (info/=0) then
  write(6,*)trim(myname),': SSTEQR/DSTEQR returned info=',info
  call stop2(320)
endif

! Error bounds
zritz(:)=zdiag(:)
zbndlm = zeta*zritz(kmaxit)

if (mype==0) write(6,*)trim(myname),': ritz values are:  ',zritz(:)
zbnds(:)=abs(zge*zevecs(kmaxit+1,:))
if (mype==0) write(6,*)trim(myname),': error bounds are: ',zbnds(:)
zbnds(:)=zbnds(:)/zritz(:)
if (mype==0) write(6,*)trim(myname),': relative errors:  ',zbnds(:)

! Compute the eigenvectors

if (lsavevecs) then

   NVCGLEV = 0
   do jk=iter,1,-1
      if (zbnds(jk) <= pkappa .AND. zritz(jk) > xmin_ritz) then
         NVCGLEV=NVCGLEV+1
      endif
   ENDDO
   if (mype==0) write(6,*) &
        & 'Number of eigenpairs converged to requested accuracy NVCGLEV=',NVCGLEV
   
   NVCGLEV= min(nwrvecs,NVCGLEV) 
   if(mype==0) write(6,*) 'Number of eigenvectors to be calculated is', NVCGLEV
   
   ALLOCATE(RCGLEV(NVCGLEV))
   ALLOCATE(YVCGLEV(NVCGLEV))
   ALLOCATE(YUCGLEV(NVCGLEV))
   DO ii=1,NVCGLEV
      CALL allocate_cv(YVCGLEV(ii))
      CALL allocate_cv(YUCGLEV(ii))
   ENDDO

   ii=0
   do jk=iter,1,-1
      if (zbnds(jk) <= pkappa .AND. zritz(jk) > xmin_ritz .AND. ii < NVCGLEV) then
         ii = ii+1
         RCGLEV(ii) = zritz(jk)
         YVCGLEV(ii) = zero
         YUCGLEV(ii) = zero
         xtry=zero
         ytry=zero
         isize=size(xtry%values)
    
         do jm=1,iter
            do jj=1,isize
               xtry%values(jj)=xtry%values(jj)+cglwork(jm)%values(jj)*zevecs(jm,jk)
            enddo
         enddo
         do jm=1,iter
            do jj=1,isize
               ytry%values(jj)= ytry%values(jj)+cglworkhat(jm)%values(jj)*zevecs(jm,jk)
            enddo
         enddo
         zdla=DOT_PRODUCT(xtry,ytry)
         do jj=1,isize
            xtry%values(jj)=xtry%values(jj)/sqrt(zdla)
            ytry%values(jj)=ytry%values(jj)/sqrt(zdla)
         end do
         
         do jj=1,isize
            YVCGLEV(ii)%values(jj) = xtry%values(jj)
            YUCGLEV(ii)%values(jj) = ytry%values(jj)
         end do

         do jm=1,ii-1
            zdla=DOT_PRODUCT (YUCGLEV(jm),YVCGLEV(ii))
            do jj=1,isize
               YVCGLEV(ii)%values(jj) = YVCGLEV(ii)%values(jj) - zdla*YVCGLEV(jm)%values(jj)
            enddo
         enddo
         do jm=1,ii-1
            zdla=DOT_PRODUCT (YVCGLEV(jm),YUCGLEV(ii))
            do jj=1,isize
               YUCGLEV(ii)%values(jj) = YUCGLEV(ii)%values(jj) - zdla*YUCGLEV(jm)%values(jj)
            enddo
         enddo
         zdla=DOT_PRODUCT (YVCGLEV(ii),YUCGLEV(ii))
         YVCGLEV(ii)%values = YVCGLEV(ii)%values / sqrt(zdla)
         YUCGLEV(ii)%values = YUCGLEV(ii)%values / sqrt(zdla)
      endif
   ENDDO
   
   ii=0
   do jk=iter,1,-1
      if((zbnds(jk) < pkappa).and.(zritz(jk) > xmin_ritz) .and. (ii < NVCGLEV)) then 
         ii = ii+1
         YVCGLEV(ii) = zero 
         YVCGLEV(ii)%values = YUCGLEV(ii)%values  ! Since we only need to save ytry, we keep in YVCGLEV
                                                  ! to avoid too much change in the preconditiong code
      end if
   end do
   
   if (mype==0.and.NVCGLEV>0) then
      write(6,'(/)')
      write(6,*)'Calculated eigenvectors for the following eigenvalues:'
      write(6,*)'RCGLEV=',RCGLEV(1:NVCGLEV)
      write(6,'(/)')
   endif
   
   DO jj=1,NVCGLEV
      CALL DEALLOCATE_CV(YUCGLEV(jj))
   ENDDO
   DEALLOCATE(YUCGLEV)
end if



! ------------------------------------------------------------------------------

deallocate(zdiag,ztoff,zwork,zritz,zbnds,zevecs)

! Release memory
call deallocate_cv(grad0)
call deallocate_cv(xtry)
call deallocate_cv(ytry)
call deallocate_cv(gradw)
call deallocate_cv(dirx)
call deallocate_cv(diry)
if(nprt>=1) call deallocate_cv(gradf)
if(nprt>=1) call deallocate_cv(gradff)

call inquire_cv

! Finalize timer
call timer_fnl('pcglanczos')

888 format(2A,3(1X,ES24.18))
999 format(2A,2(1X,I3),3(1X,ES24.18))

return
end subroutine pcglanczos

!=============================================================
!==========================================================
! ------------------------------------------------------------------------------
!   SAVE_PRECOND - Save eigenvectors from CONGRAD for next minimization
! ------------------------------------------------------------------------------
subroutine save_pcgprecond(ldsave)

IMPLICIT NONE

logical, intent(in)       :: ldsave

REAL(r_kind), ALLOCATABLE :: zmat(:,:)
INTEGER(i_kind)           :: ii,jj, info, iunit, ivecs
REAL(r_kind)              :: zz
CHARACTER(LEN=13)         :: clfile

if (ldsave) then

!--- read eigenvalues of the preconditioner

  NPCVECS = NVCGLEV+NVCGLPC
  if (mype==0) write(6,*)'save_pcgprecond: NVCGLEV,NVCGLPC,NPCVECS=', &
                                      & NVCGLEV,NVCGLPC,NPCVECS

  ALLOCATE(YVCGLWK(npcvecs))
  ii=0

!--- copy preconditioner vectors to work file

  if (mype==0.and.NVCGLPC>0) write(6,*)'save_pcgprecond: RCGLPC=',RCGLPC
  DO jj=1,NVCGLPC
    ii=ii+1
    zz=sqrt(RCGLPC(jj)-one)
    CALL allocate_cv(YVCGLWK(ii))
    YVCGLWK(ii)%values = zz * YVCGLPC(jj)%values
    CALL deallocate_cv(YVCGLPC(jj))
  ENDDO
  IF (ALLOCATED(YVCGLPC)) DEALLOCATE(YVCGLPC)
  IF (ALLOCATED( RCGLPC)) deallocate( RCGLPC)
  NVCGLPC=0

!--- copy and transform eigenvectors of preconditioned Hessian
 
  if (mype==0.and.NVCGLEV>0) write(6,*)'save_pcgprecond: RCGLEV=',RCGLEV
  DO jj=1,NVCGLEV
    ii=ii+1
  !  zz=sqrt(one - one/RCGLEV(jj))
    zz = MIN(10._r_kind,RCGLEV(jj))
    zz = sqrt(one - one/zz)
    CALL allocate_cv(YVCGLWK(ii))
    YVCGLWK(ii)%values = zz * YVCGLEV(jj)%values
    CALL deallocate_cv(YVCGLEV(jj))
  ENDDO
 
  IF (ALLOCATED(YVCGLEV)) DEALLOCATE(YVCGLEV)
  IF (ALLOCATED(YUCGLEV)) DEALLOCATE(YUCGLEV)
  IF (ALLOCATED( RCGLEV)) deallocate( RCGLEV)
  NVCGLEV=0
  
!--- Save the eigenvectors

!+  if (l4dvar) then
   ivecs=MIN(npcvecs,nwrvecs)
    DO jj=1,ivecs
      clfile='evec.XXX.YYYY'
      WRITE(clfile(6:8) ,'(I3.3)') jiter
      WRITE(clfile(10:13),'(I4.4)') jj
      call write_cv(YVCGLWK(jj),clfile)
    ENDDO

    if (mype==0) then
      iunit=78
      clfile='numpcvecs.XXX'
      WRITE(clfile(11:13),'(I3.3)') jiter
      open(iunit,file=clfile)
      write(iunit,*)ivecs
      close(iunit)
    endif

    DO ii=1,npcvecs
      CALL deallocate_cv(YVCGLWK(ii))
    ENDDO
    DEALLOCATE(YVCGLWK)
!+  else
!+   ! do ii=nwrvecs+1,npcvecs   ! nwrvecs here is -1, the do loop would with 0 ==> allocation pb.  
!+     do ii=1,npcvecs 
!+      CALL deallocate_cv(YVCGLWK(ii))
!+    enddo
   !  npcvecs=MIN(npcvecs,nwrvecs)
!+  endif

endif

do ii=1,maxiter+1
  call deallocate_cv(cglwork(ii))
enddo
deallocate(cglwork)
do ii=1,maxiter+1
  call deallocate_cv(cglworkhat(ii))
enddo
deallocate(cglworkhat)

return
end subroutine save_pcgprecond
!==========================================================
! ------------------------------------------------------------------------------
!   SETUP_PCGPRECOND - Calculates the preconditioner for congrad
! ------------------------------------------------------------------------------
subroutine setup_pcgprecond()

IMPLICIT NONE

INTEGER(i_kind)  , allocatable :: indarr(:)
REAL(r_kind)     , allocatable :: zq(:),zlam(:),zU(:,:),zUUT(:,:),zwork(:),zzz(:)
INTEGER(i_kind)                :: info,ik,inpcv,ji,jj,jk,ii,iunit
REAL(r_kind)                   :: za, zps
CHARACTER(LEN=13)              :: clfile

!--- read vectors, apply change of variable and copy to work file


!+ if (l4dvar) then
  iunit=78
  clfile='numpcvecs.XXX'
  WRITE(clfile(11:13),'(I3.3)') jiter-1
  open(iunit,file=clfile)
  read(iunit,*)npcvecs
  close(iunit)
  if (npcvecs<1) then
    write(6,*)'setup_pcgprecond: no vectors for preconditioner',npcvecs
    call stop2(140)
  end if

 

  ALLOCATE(YVCGLWK(npcvecs))
  DO ii=1,npcvecs
    CALL allocate_cv(YVCGLWK(ii))
  ENDDO

  

  do jj=1,npcvecs
    clfile='evec.XXX.YYYY'
    WRITE(clfile(6:8) ,'(I3.3)') jiter-1
    WRITE(clfile(10:13),'(I4.4)') jj
    call read_cv(yvcglwk(jj),clfile)
  ENDDO
!+ endif
  

  IF (ALLOCATED(YVCGLPC)) THEN
     DO jj=1,NVCGLPC
        CALL DEALLOCATE_CV(YVCGLPC(jj))
     ENDDO
     DEALLOCATE(YVCGLPC)
     NVCGLPC=0
  ENDIF
  IF (ALLOCATED(RCGLPC)) DEALLOCATE(RCGLPC)
 
  NVCGLPC = npcvecs
  
  ALLOCATE (YVCGLPC(NVCGLPC))
  DO jj=1,NVCGLPC
    CALL ALLOCATE_CV(YVCGLPC(jj))
  ENDDO
 
  DO jj=1,NVCGLPC
     YVCGLPC(jj) = zero
     do jk=1,YVCGLPC(jj)%lencv
        YVCGLPC(jj)%values(jk) = yvcglwk(jj)%values(jk)
     enddo
  ENDDO
  LMPCGL = .true.


DO jj=1,npcvecs
   CALL DEALLOCATE_CV(YVCGLWK(jj))
ENDDO
DEALLOCATE(YVCGLWK)

NPCVECS = 0


return
end subroutine setup_pcgprecond
!===============================================================================

! ------------------------------------------------------------------------------
!   PRECOND - Preconditioner for minimization
! ------------------------------------------------------------------------------
subroutine pcgprecond(xcvx,ycvx,kmat)

IMPLICIT NONE

TYPE(CONTROL_VECTOR) , INTENT(IN)  :: xcvx
TYPE(CONTROL_VECTOR) , INTENT(INOUT) :: ycvx
INTEGER(i_kind)      , INTENT(IN)  :: kmat

REAL(r_kind)        :: zdp(NVCGLPC)
INTEGER(i_kind)     :: jk, ji
INTEGER(i_kind)     :: isize
 
ycvx=zero
do jk=1,NVCGLPC
   zdp(jk) = 0.
enddo

!Apply B
call bkerror(xcvx,ycvx)

do jk=1,NVCGLPC
    zdp(jk) = DOT_PRODUCT(xcvx,YVCGLPC(jk))
enddo

DO jk=1,NVCGLPC
   DO ji=1,xcvx%lencv
      ycvx%values(ji) = ycvx%values(ji) - YVCGLPC(jk)%values(ji) * zdp(jk)
   ENDDO
ENDDO

return
end subroutine pcgprecond
!=========================================================
!===========
end module bicglanczos
