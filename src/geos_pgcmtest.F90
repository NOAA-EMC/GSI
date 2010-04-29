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
!     xobs - State variable at observations times (left untouched)
!   output argument list:
!
!$$$
use kinds, only: r_kind,i_kind
use mpimod, only: mype
use gsi_4dvar, only: nsubwin,nobs_bins,winlen,winsub,hr_obsbin
use gsi_4dvar, only: iadatebgn,idmodel
use constants, only: zero,one,tiny_r_kind
#ifdef GEOS_PERT
use prognostics, only: dyn_prog
use prognostics, only: prognostics_initial
use prognostics, only: prognostics_final
use prognostics, only: prognostics_zero
use prognostics, only: prognostics_dotp
use prognostics, only: prognostics_dup
use prognostics, only: prognostics_cnst
use prognostics, only: prognostics_rand
use prognostics, only: nc
use geos_pertmod, only: gsi2pgcm
use geos_pertmod, only: pgcm2gsi
use geos_pertmod, only: ndtpert
use m_model_tl, only: initial_tl
use m_model_tl, only: amodel_tl
use m_model_tl, only: final_tl
use m_model_ad, only: initial_ad
use m_model_ad, only: amodel_ad
use m_model_ad, only: final_ad
#endif /* GEOS_PERT */
use state_vectors
implicit none
  
! Declare passed variables
type(state_vector), intent(inout) :: xini(nsubwin)
type(state_vector), intent(inout) :: xobs(nobs_bins)
logical, intent(in) :: ldprt

#ifdef GEOS_PERT
! Declare local variables
real(r_kind), parameter :: R3600 = 3600.0_r_kind
integer(i_kind) :: i,j,ij,nstep,istep,nfrctl,nfrobs,ii,ierr
integer(i_kind) :: nymdi,nhmsi,ndt
integer(i_kind) :: nymdt,nhmst
type(state_vector) :: ww
type(state_vector) :: xx
type(state_vector) :: yy
type(state_vector) :: zz(nobs_bins)
type(dyn_prog) :: xpert 
type(dyn_prog) :: ypert 
real(r_kind)   :: tstep, zt
real(r_quad)   :: d0(10),d1(nobs_bins*nsubwin),d2(nsubwin)
logical cases(0:5)
integer, save :: mycount = 0
integer, parameter :: inow = -1 ! set so test occur during this iteration of inner loop
                                ! default -1, is no test

cases = .false.
cases(0) = .true.
cases(2) = .true.
cases(1) = .true.
cases(3) = .true.
cases(4) = .true.
cases(5) = .true.
!******************************************************************************

if ( mycount==inow  ) then  ! not active on purpose (to activate set to iteration
                                ! number when to apply test)

! Initialize variables
if (idmodel) then
   tstep  = R3600
   ndt    = 1
else
   tstep  = REAL(ndtpert,r_kind)
   ndt    = NINT(R3600/ndtpert)
endif
nstep  = NINT(winlen*R3600/tstep)
nfrctl = NINT(winsub*R3600/tstep)
nfrobs = NINT(hr_obsbin*R3600/tstep)
nymdi  =  iadatebgn/100
nhmsi  = (iadatebgn - 100*nymdi)*10000

! Checks
zt=real(nstep,r_kind)*tstep
if (ABS(winlen*R3600   -zt)>epsilon(zt)) then
  write(6,*)'geos_pgcmtest: error nstep',winlen,zt
  call stop2(127)
end if
zt=real(nfrctl,r_kind)*tstep
if (ABS(winsub*R3600   -zt)>epsilon(zt)) then
  write(6,*)'geos_pgcmtest: error nfrctl',winsub,zt
  call stop2(128)
end if
zt=real(nfrobs,r_kind)*tstep
if (ABS(hr_obsbin*R3600-zt)>epsilon(zt)) then
  write(6,*)'geos_pgcmtest: error nfrobs',hr_obsbin,zt
  call stop2(129)
end if
if (ndt<1)then
 write(6,*)'geos_pgcmtest: error ndt',ndt
 call stop2(130)
end if

if (ldprt.and.mype==0) write(6,*)'geos_pgcmtest: nstep,nfrctl,nfrobs=',nstep,nfrctl,nfrobs

     if ( ldprt ) then
       if (mype==0) then
           print *
           print *,'==================================================================='
           print *,'    BEGIN TESTING INTERFACES BETWEEN GSI AND AGCM AD/TL MODELS     '
           print *,'-------------------------------------------------------------------'
       endif
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

!      yy=one
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
            if (mype==0) then
               print *
               print *, '  Test Case 0: are transforms inverse of each other? '
               print *, '  Relative errors:'
               print *, '       d1(T(-1)T)   = '  , d0(1)
!              print *, '       d2(T''T''(-1)) = ', d0(2)    ! doesn't really need to be statisfied
               print *, '       d3(T''(-1)T'') = ', d0(3)
!              print *, '       d4(TT(-1))     = ', d0(4)    ! doesn't really need to be statisfied
               print *
            endif
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
            if (mype==0) then
               print *
               print *, '  Test Case 1: does adm of transforms verify when M=I? '
               print *, '  Test: d= x''T''T''(-1)T(-1)Ty ; z:= Ty =>'
               print *, '       d=d1 = z''z          =', d1(1)
               print *, '       d=d2 = y''T''T''(-1)z  =', d2(1)
               print *, ' abs(d1-d2) = ', abs(d1(1)-d2(1))
               if(abs(d1(1))>zero) print *, ' abs(d1-d2)/abs(d1) = ', abs(d1(1)-d2(1))/abs(d1(1))
               print *
            endif
          endif

       endif ! case 1

       if ( cases(2) ) then

              call set_random ( yy )
              yy%oz=zero
              yy%cw=zero
              yy%sst=zero
              ww=yy
              call prognostics_cnst(zero,xpert)
       call getprs_tl (yy%p,yy%t,yy%p3d)
       call tv_to_tsen(yy%t,yy%q,yy%tsen)
       call gsi2pgcm ( yy, xpert, 'tlm', ierr )  ! T
              d1(1) = prognostics_dotp (xpert,xpert) 
                   ! z  = Tx
                   ! d1 = z'z
              yy=zero
       call pgcm2gsi ( xpert, yy, 'adm', ierr )  ! T'
       call tv_to_tsen_ad(yy%t,yy%q,yy%tsen)
       call getprs_ad    (yy%p,yy%t,yy%p3d)
              d2(1) = dot_product(ww,yy)
                   ! d2 = x'T'z
          if ( ldprt ) then
            if (mype==0) then
               print *
               print *, '  Test Case 2: does adm of T transform verify? '
               print *, '  Test: d= x''T''Tx ; z:= Tx =>'
               print *, '       d=d1 = z''z     =', d1(1)
               print *, '       d=d2 = x''T''Tz  =', d2(1)
               print *, ' abs(d1-d2) = ', abs(d1(1)-d2(1))
               if(abs(d1(1))>zero) print *, ' abs(d1-d2)/abs(d1) = ', abs(d1(1)-d2(1))/abs(d1(1))
               print *
            endif
          endif

       yy=zero
!      call prognostics_cnst(one,xpert) ! this is too special of a case
!      call prognostics_rand(xpert)     ! this is not working properly ...
                                        ! to get a random input vector, use what
                                        ! comes out from previous test
       xpert%q(:,:,:,2:nc) = zero
       call prognostics_dup (xpert,ypert)
          call pgcm2gsi ( xpert, yy, 'tlm', ierr )  ! T(-1)
              d1(1) = dot_product(yy,yy)
                   ! z  = T(-1)y
                   ! d1 = z'z
          call gsi2pgcm ( yy, xpert, 'adm', ierr )  ! T'(-1)
                   ! d2 = y'T'(-1)z
              d2(1) = prognostics_dotp(ypert,xpert)
          if ( ldprt ) then
            if (mype==0) then
               print *
               print *, '  Test Case 2: does adm of T(-1) transform verify? '
               print *, '  Test: d= x''T''(-1)T(-1)x ; z:= T(-1)x =>'
               print *, '       d=d1 = z''z             =', d1(1)
               print *, '       d=d2 = x''T''(-1)T(-1)z  =', d2(1)
               print *, ' abs(d1-d2) = ', abs(d1(1)-d2(1))
               if(abs(d1(1))>zero) print *, ' abs(d1-d2)/abs(d1) = ', abs(d1(1)-d2(1))/abs(d1(1))
               print *
            endif
          endif

       endif ! case 2

       if ( cases(3) ) then

          if ( mycount<0 ) then
              call prognostics_rand(xpert,seed=0) ! picking seed so I can easily compare w/ offline call to TLM/ADM check
          else
              xx=xini(1)
              call gsi2pgcm ( xx, xpert, 'tlm', ierr )
          endif
              call prognostics_dup (xpert,ypert)
              d0(1) = prognostics_dotp (xpert,xpert)
          call amodel_tl( xpert, g5pert=.true. )
                   ! z  = Mx
                   ! d1 = z'z
              d1(1) = prognostics_dotp (xpert,xpert)
          call amodel_ad( xpert, g5pert=.true. )
                   ! d2 = x'M'z
              d2(1) = prognostics_dotp (xpert,ypert)
          if ( ldprt ) then
            if (mype==0) then
               print *
               print *, '  Test Case 3: does adjoint of AGCM verify? '
               print *, '  Test (gcm space): d= x''M''Mx ; z:= Mx =>'
               print *, '       d=d0 = x''x    =', d0(1)
               print *, '       d=d1 = z''z    =', d1(1)
               print *, '       d=d2 = x''M''z  =', d2(1)
               print *, ' abs(d1-d2) = ', abs(d1(1)-d2(1))
               if(abs(d1(1))>zero) print *, ' abs(d1-d2)/abs(d0) = ', abs(d1(1)-d2(1))/abs(d0(1))
               print *
            endif
          endif

       endif ! case 3

       if ( cases(4) .and. (nsubwin==1) ) then

          ! temporarily store state in yy and zz (these should not be touched within this test)
          ! ------------------------------------
          yy=xini(1)
          do i=1,nobs_bins
             call allocate_state(zz(i))
             xx=xobs(i)
             zz(i)=xx
             xobs(i)=zero
          end do

          ! run test
          ! --------
          if ( mycount<0 ) then       ! if so, set to random
              call set_random ( xx )
              xini(1)=xx
              xini(1)%oz=zero
              xini(1)%cw=zero
              xini(1)%sst=zero
          endif
          xx=xini(1)
          d0(1)=dot_product(xx,xx)

          call model_tl(xini,xobs,.false.) 
          d1(1)=zero
          do i=1,nobs_bins
             d1(1) = d1(1) + dot_product(xobs(i),xobs(i))
          enddo
          call model_ad(xini,xobs,.false.) 
             d2(1) = dot_product(xx,xini(1))

          if ( ldprt ) then
            if (mype==0) then
               print *
               print *, '  Test Case 4: does adjoint of full operator verify? '
               print *, '  Test (gsi space): d= x''M''Mx ; z:= Mx =>'
               print *, '       d=d0 = x''x    =', d0(1)
               print *, '       d=d1 = z''z    =', d1(1)
               print *, '       d=d2 = x''M''z  =', d2(1)
               print *, ' abs(d1-d2) = ', abs(d1(1)-d2(1))
               if(abs(d1(1))>zero) print *, ' abs(d1-d2)/abs(d0) = ', abs(d1(1)-d2(1))/abs(d0(1))
               print *
            endif
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

       if ( cases(5) ) then

            call set_random ( yy )
            xx=yy
                d0(1) = dot_product(yy,yy)
              call getprs_tl (xx%p,xx%t,yy%p3d)
              call tv_to_tsen(xx%t,xx%q,yy%tsen)
                d1(1) = dot_product(yy,yy)
              call tv_to_tsen_ad(yy%t,yy%q,yy%tsen)
              call getprs_ad    (yy%p,yy%t,yy%p3d)
                d2(1) = dot_product(yy,xx)

          if ( ldprt ) then
            if (mype==0) then
               print *
               print *, '  Test Case 5: do adjoints getprs and tv_to_tsen verify? '
               print *, '  Test (gcm space): d= x''M''Mx ; z:= Mx =>'
               print *, '       d=d0 = x''x    =', d0(1)
               print *, '       d=d1 = z''z    =', d1(1)
               print *, '       d=d2 = x''M''z  =', d2(1)
               print *, ' abs(d1-d2) = ', abs(d1(1)-d2(1))
               if(abs(d1(1))>zero) print *, ' abs(d1-d2)/abs(d0) = ', abs(d1(1)-d2(1))/abs(d0(1))
               print *
            endif
          endif


       endif ! case 5

!   Finalize GCM TLM and its perturbation vector
    call prognostics_final ( ypert )
    call prognostics_final ( xpert )
    call deallocate_state  ( ww )
    call deallocate_state  ( yy )
    call deallocate_state  ( xx )

    if ( ldprt ) then
      if (mype==0) then
         print *
         print *,'-------------------------------------------------------------------'
         print *,'    END   TESTING INTERFACES BETWEEN GSI AND AGCM AD/TL MODELS     '
         print *,'==================================================================='
      endif
    endif

endif
mycount = mycount + 1

#endif /* GEOS_PERT */
end subroutine geos_pgcmtest
