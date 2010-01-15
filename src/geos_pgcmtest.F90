subroutine geos_pgcmtest(xini,xobs,ldprt)
!$$$  subprogram documentation block
!
! abstract:  Test AGCM tangent linear and adjoint models
!
! program history log:
!   2007-05-29  todling  - initial code
!   2009-01-17  todling  - update var name in state vector
!
!   input argument list:
!     xini - State variable at control times (left untouched)
!   output argument list:
!     xobs - State variable at observations times (left untouched)
!
!$$$
use kinds, only: r_kind,i_kind
use gsi_4dvar, only: nsubwin,nobs_bins,winlen,winsub,hr_obsbin
use gsi_4dvar, only: iadatebgn,idmodel
use constants, only: izero,ione,zero,one,tiny_r_kind,r3600
#ifdef GEOS_PERT
use prognostics, only: dyn_prog
use prognostics, only: prognostics_initial
use prognostics, only: prognostics_final
use prognostics, only: prognostics_dotp
use prognostics, only: prognostics_dup
use prognostics, only: prognostics_cnst
use geos_pertmod, only: gsi2pgcm
use geos_pertmod, only: pgcm2gsi
use geos_pertmod, only: ndtpert
use m_model_tl, only: amodel_tl
use m_model_ad, only: amodel_ad
#endif /* GEOS_PERT */
use state_vectors
implicit none
  
! Declare passed variables
type(state_vector), intent(inout) :: xini(nsubwin)
type(state_vector), intent(inout) :: xobs(nobs_bins)
logical           , intent(in   ) :: ldprt

#ifdef GEOS_PERT
! Declare local variables
integer(i_kind) :: i,nstep,nfrctl,nfrobs,ierr
integer(i_kind) :: nymdi,nhmsi,ndt
integer(i_kind) :: nymdt,nhmst
type(state_vector) :: ww
type(state_vector) :: xx
type(state_vector) :: yy
type(state_vector) :: zz(nobs_bins)
type(dyn_prog) :: xpert 
type(dyn_prog) :: ypert 
real(r_kind)   :: tstep, zt
real(r_kind)   :: d0(10),d1(nobs_bins*nsubwin),d2(nsubwin)
logical cases(0:5)
integer(i_kind), save :: mycount = izero

cases = .false.
cases(4) = .true.
cases(0:2) = .true.
!cases(3) = .true.
!******************************************************************************

if ( mycount==-ione ) then  ! not active on purpose

   ! Initialize variables
   if (idmodel) then
      tstep  = r3600
   else
      tstep  = REAL(ndtpert,r_kind)
   endif
   nstep  = NINT(winlen*r3600/tstep)
   nfrctl = NINT(winsub*r3600/tstep)
   nfrobs = NINT(hr_obsbin*r3600/tstep)
   ndt    = NINT(r3600/ndtpert)
   nymdi  =  iadatebgn/100
   nhmsi  = (iadatebgn - 100*nymdi)*10000
 
   ! Checks
   zt=real(nstep,r_kind)*tstep
   if (ABS(winlen*r3600   -zt)>epsilon(zt)) then
      write(6,*)'geos_pgcmtest: error nstep',winlen,zt
      call stop2(127)
   end if
   zt=real(nfrctl,r_kind)*tstep
   if (ABS(winsub*r3600   -zt)>epsilon(zt)) then
      write(6,*)'geos_pgcmtest: error nfrctl',winsub,zt
      call stop2(128)
   end if
   zt=real(nfrobs,r_kind)*tstep
   if (ABS(hr_obsbin*r3600-zt)>epsilon(zt)) then
      write(6,*)'geos_pgcmtest: error nfrobs',hr_obsbin,zt
      call stop2(129)
   end if
   if (ndt<ione)then
      write(6,*)'geos_pgcmtest: error ndt',ndt
      call stop2(130)
   end if

   if (ldprt) write(6,*)'geos_pgcmtest: nstep,nfrctl,nfrobs=',nstep,nfrctl,nfrobs

   if ( ldprt ) then
      print *
      print *,'==================================================================='
      print *,'    BEGIN TESTING INTERFACES BETWEEN GSI AND AGCM AD/TL MODELS     '
      print *,'-------------------------------------------------------------------'
   endif

   nymdt = nymdi
   nhmst = nhmsi

   ! Initialize GCM TLM and its perturbation vector
   call allocate_state      ( xx )
   call allocate_state      ( yy )
   call allocate_state      ( ww )
   call prognostics_initial ( xpert )
   call prognostics_initial ( ypert )

   if ( cases(0) ) then

!     yy=one
      call set_random ( yy )
      call prognostics_cnst(zero,xpert)
      d0(1) = dot_product(yy,yy)
      call gsi2pgcm ( yy, xpert, 'tlm', ierr )  ! T
      call pgcm2gsi ( xpert, yy, 'tlm', ierr )  ! T(-1)
      d0(2) = dot_product(yy,yy)
      if(d0(1)>tiny_r_kind) d0(1) = abs(d0(1) - d0(2))/d0(1)

      yy=one
      call prognostics_cnst(zero,xpert)
      d0(2) = dot_product(yy,yy)
      call gsi2pgcm ( yy, xpert, 'adm', ierr )  ! T'(-1)
      call pgcm2gsi ( xpert, yy, 'adm', ierr )  ! T'
      d0(3) = dot_product(yy,yy)
      if(d0(2)>tiny_r_kind) d0(2) = abs(d0(2) - d0(3))/d0(2)

      yy=zero
      call prognostics_cnst(one,xpert)
      d0(3) = prognostics_dotp(xpert,xpert)
      call pgcm2gsi ( xpert, yy, 'adm', ierr )  ! T'
      call gsi2pgcm ( yy, xpert, 'adm', ierr )  ! T'(-1)
      d0(4) = prognostics_dotp(xpert,xpert)
      if(d0(3)>tiny_r_kind) d0(3) = abs(d0(3) - d0(4))/d0(3)

      yy=zero
      call prognostics_cnst(one,xpert)
      d0(4) = prognostics_dotp(xpert,xpert)
      call pgcm2gsi ( xpert, yy, 'tlm', ierr )  ! T(-1)
      call gsi2pgcm ( yy, xpert, 'tlm', ierr )  ! T
      d0(5) = prognostics_dotp(xpert,xpert)
      if(d0(4)>tiny_r_kind) d0(4) = abs(d0(4) - d0(5))/d0(4)

      if ( ldprt ) then
         print *
         print *, '  Test Case 0: are transforms inverse of each other? '
         print *, '  Relative errors:'
         print *, '       d1(T(-1)T)     = ', d0(1)
         print *, '       d2(T''T''(-1))    = ', d0(2)
         print *, '       d3(T''(-1)T'') = ', d0(3)
         print *, '       d4(T (-1)T) = ', d0(4)
         print *
      endif


   endif ! case 0

   if ( cases(1) ) then

      yy=one
      yy%oz=zero
      yy%cw=zero
      yy%sst=zero
      xx=yy
      call gsi2pgcm ( yy, xpert, 'tlm', ierr ) ! T
      call pgcm2gsi ( xpert, yy, 'tlm', ierr ) ! T(-1)
      ! z  = Ty
      ! d1 = z'z
      d1(1) = dot_product (yy,yy) 
      call gsi2pgcm ( yy, xpert, 'adm', ierr ) ! T'(-1)
      call pgcm2gsi ( xpert, yy, 'adm', ierr ) ! T'
      ! d2 = y'T'z
      d2(1) = dot_product(xx,yy)
      if ( ldprt ) then
         print *
         print *, '  Test Case 1: does adm of transforms verify when M=I? '
         print *, '  Test: d= x''T''T''(-1)T(-1)Ty ; z:= Ty =>'
         print *, '       d=d1 = z''z          =', d1(1)
         print *, '       d=d2 = y''T''T''(-1)z  =', d2(1)
         print *, ' abs(d1-d2) = ', abs(d1(1)-d2(1))
         if(abs(d1(1))>zero) print *, ' abs(d1-d2)/abs(d1) = ', abs(d1(1)-d2(1))/abs(d1(1))
         print *
      endif

   endif ! case 1

   if ( cases(2) ) then

      yy=one
      ww=yy
      call gsi2pgcm ( yy, xpert, 'tlm', ierr )  ! T
      d1(1) = prognostics_dotp (xpert,xpert) 
      ! z  = Tx
      ! d1 = z'z
      call pgcm2gsi ( xpert, yy, 'adm', ierr )  ! T'
      d2(1) = dot_product(ww,yy,which='u+v+tv+q+p+')
      ! d2 = x'T'z
      if ( ldprt ) then
         print *
         print *, '  Test Case 2: does adm of T transform verify? '
         print *, '  Test: d= x''T''Tx ; z:= Tx =>'
         print *, '       d=d1 = z''z     =', d1(1)
         print *, '       d=d2 = x''T''Tz  =', d2(1)
         print *, ' abs(d1-d2) = ', abs(d1(1)-d2(1))
         if(abs(d1(1))>zero) print *, ' abs(d1-d2)/abs(d1) = ', abs(d1(1)-d2(1))/abs(d1(1))
         print *
      endif

      yy=zero
      call prognostics_cnst(one,xpert)
      call pgcm2gsi ( xpert, yy, 'tlm', ierr )  ! T(-1)
      d1(1) = dot_product(yy,yy,which='u+v+tv+q+p+')
      ! z  = T(-1)y
      ! d1 = z'z
      call gsi2pgcm ( yy, xpert, 'adm', ierr )  ! T'(-1)
      ! d2 = y'T'(-1)z
      d2(1) = prognostics_dotp(xpert,xpert)
      if ( ldprt ) then
         print *
         print *, '  Test Case 2: does adm of T(-1) transform verify? '
         print *, '  Test: d= x''T''(-1)T(-1)x ; z:= T(-1)x =>'
         print *, '       d=d1 = z''z             =', d1(1)
         print *, '       d=d2 = x''T''(-1)T(-1)z  =', d2(1)
         print *, ' abs(d1-d2) = ', abs(d1(1)-d2(1))
         if(abs(d1(1))>zero) print *, ' abs(d1-d2)/abs(d1) = ', abs(d1(1)-d2(1))/abs(d1(1))
         print *
      endif

   endif ! case 2

   if ( cases(3) ) then

      yy=one
      yy%oz=zero
      yy%cw=zero
      yy%sst=zero
      call gsi2pgcm ( yy, xpert, 'tlm', ierr )
!     call prognostics_cnst(one,xpert)
      call prognostics_dup (xpert,ypert)
      call amodel_tl( xpert )
      ! z  = Mx
      ! d1 = z'z
      d1(1) = prognostics_dotp (xpert,xpert)
      call amodel_ad( xpert )
      ! d2 = x'M'z
      d2(1) = prognostics_dotp (xpert,ypert)
      if ( ldprt ) then
         print *
         print *, '  Test Case 3: does adjoint of AGCM verify? '
         print *, '  Test (gcm space): d= x''M''Mx ; z:= Mx =>'
         print *, '       d=d1 = z''z    =', d1(1)
         print *, '       d=d2 = x''M''z  =', d2(1)
         print *, ' abs(d1-d2) = ', abs(d1(1)-d2(1))
         if(abs(d1(1))>zero) print *, ' abs(d1-d2)/abs(d1) = ', abs(d1(1)-d2(1))/abs(d1(1))
         print *
      endif

   endif ! case 3

   if ( cases(4) .and. (nsubwin==ione) ) then

      ! temporarily store state
      ! -----------------------
      yy=xini(1)
      do i=1,nobs_bins
         call allocate_state(zz(i))
         xx=xobs(i)
         zz(i)=xx
      end do

      ! run test
      ! --------
      xini(1)=one
      xini(1)%oz=zero
      xini(1)%cw=zero
      xini(1)%sst=zero
      xx=xini(1)

      call gsi2pgcm ( xx, xpert, 'tlm', ierr )
      call pgcm2gsi ( xpert, xx, 'tlm', ierr )
      xini(1)=xx

      call model_tl(xini,xobs,ldprt) 
      d0(1)=dot_product(xx,xx)
      d1(1)=zero
      do i=1,nobs_bins
         d1(1) = d1(1) + dot_product(xobs(i),xobs(i),which='u+v+tv+q+p+')
      enddo
      call model_ad(xini,xobs,ldprt) 
      d2(1) = dot_product(xx,xini(1))

      if ( ldprt ) then
         print *
         print *, '  Test Case 4: does adjoint of full operator verify? '
         print *, '  Test (gsi space): d= x''M''Mx ; z:= Mx =>'
         print *, '       d=d0 = x''x    =', d0(1)
         print *, '       d=d1 = z''z    =', d1(1)
         print *, '       d=d2 = x''M''z  =', d2(1)
         print *, ' abs(d1-d2) = ', abs(d1(1)-d2(1))
         if(abs(d1(1))>zero) print *, ' abs(d1-d2)/abs(d1) = ', abs(d1(1)-d2(1))/abs(d1(1))
         print *
      endif

      ! recover original state
      ! ----------------------
      xini(1) = yy
      do i=1,nobs_bins
         xx=zz(i)
         xobs(i)=xx
         call deallocate_state(zz(i))
      end do

   endif ! case 4

!  Finalize GCM TLM and its perturbation vector
   call prognostics_final ( ypert )
   call prognostics_final ( xpert )
   call deallocate_state  ( ww )
   call deallocate_state  ( yy )
   call deallocate_state  ( xx )

   if ( ldprt ) then
      print *
      print *,'-------------------------------------------------------------------'
      print *,'    END   TESTING INTERFACES BETWEEN GSI AND AGCM AD/TL MODELS     '
      print *,'==================================================================='
   endif

endif
mycount = mycount + ione

#endif /* GEOS_PERT */
end subroutine geos_pgcmtest
