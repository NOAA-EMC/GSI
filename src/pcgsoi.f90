module pcgsoimod

!$$$ module documentation block
!           .      .    .                                       .
! module:   pcgsoimod    module for pcgsoi and its tangent linear pcgsoi_tl
!  prgmmr:
!   
! abstract: module for pcgsoi and its tangent linear pcgsoi_tl
!
! program history log:
!   2005-05-17  Yanqiu zhu - wrap pcgsoi and its tangent linear pcgsoi_tl into one module
!   2005-11-21  Derber - remove interface
!   2008-11-26  Todling - remove pcgsoi_tl
!   2009-08-12  lueken  - update documentation
!   2009-09-17  parrish - add bkerror_a_en and anbkerror_reg_a_en for hybrid ensemble control variable a_en
!
! subroutines included:
!   sub pcgsoi
!
! variable definitions:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

implicit none

PRIVATE
PUBLIC pcgsoi

contains

subroutine pcgsoi()

!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    pcgsoi      solve inner loop of analysis equation
!   prgmmr: parrish          org: np22                date: 1991-04-02
!
! abstract: solve inner loop of analysis equation. at end update outer
!           loop variables
!
! program history log:
!   1991-04-02  parrish, d., derber,j.
!   1998-01-16  derber,j.
!   1998-07-10  yang w., mpp version
!   1999-06-28  yang w., second structure mpp version
!   1999-07-30  wu, update handling of ozone data
!   1999-08-24  derber, j., treadon, r., yang, w., first frozen mpp version
!   1999-12-07  wu               grid version
!   2003-12-18  derber, j. modify minimization procedures
!   2004-07-28  treadon - add only to module use, add intent in/out
!   2004-10-06  kleist - separate control vector for u,v
!   2004-10-15  parrish - add nonlinear qc option
!   2005-03-09  parrish - add optional call to regional anisotropic
!                         background error
!   2005-03-28  treadon - combine hopers.f90 and update_ggrid.f90 in update_guess.f90
!   2005-04-11  treadon - add logical flag for nonlinear qc, remove calls
!                         to intall_qc and stpcalc_qc
!   2005-05-02  treadon - add call getuv for restart option
!   2005-05-17  yanqiu zhu - add 'use intallmod', 'use stpcalcmod' and 'use dprodxmod' 
!   2005-05-27  derber  - changes to minimization and output
!   2005-06-03  parrish - add horizontal derivatives
!   2005-09-29  kleist,parrish - include _t (time derivatives) array
!   2006-04-06  treadon - move bias cor. and tskin update into update_guess
!   2006-04-16  derber - change call to stpcalc - move stepsize print to stpcalc
!   2006-04-21  kleist - add calls to update Jc terms
!   2006-05-26  derber - modify to improve convergence testing
!   2006-07-28  derber - remove calls to makeobs
!   2006-08-04  parrish - add changes for strong constraint option
!   2007-03-09  su      - add option for observation perturbation
!   2007-04-13  tremolet - use control vectors and state vectors                         
!   2007-05-15  todling - add AGCM/TLM/ADM tester; fix one-ob case NaN's
!   2007-07-05  todling - allow 4dvar to write out increment
!   2007-09-30  todling - add timer
!   2008-03-24  wu      - oberror tuning
!   2008-11-03  sato - enables to use global anisotropic mode
!   2008-12-01  todling - add init_/clean_ to allow clean way out from obs-error tuning
!                       - updated interface to penal
!   2009-01-28  todling - move write_all from glbsoi to here (consistent w/ 4dvar mods)
!   2009-02-06  pondeca - add option to re-biorthogonalize the gradx and and grady vectors.
!                         hardwired to work for 2dvar only
!   2009-09-17  parrish - add bkerror_a_en and anbkerror_reg_a_en for hybrid ensemble
!                         control variable a_en
!   2009-10-12  parrish - add beta12mult for scaling by hybrid blending parameters beta1inv, beta2inv
!                           called only when l_hyb_ens=.true.
!
! input argument list:
!
!
! output argument list:      
!      none
!
!
! remarks:
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,i_kind,r_quad
  use qcmod, only: nlnqc_iter,varqc_iter,c_varqc
  use obsmod, only: destroyobs,oberror_tune
  use jfunc, only: iter,jiter,jiterstart,niter,miter,iout_iter,&
       nclen,penorig,gnormorig,xhatsave,yhatsave,&
       iguess,read_guess_solution, &
       niter_no_qc,l_foto,xhat_dt,print_diag_pcg,lgschmidt
  use gsi_4dvar, only: nobs_bins, nsubwin, l4dvar, lwrtinc, ladtest
  use gridmod, only: twodvar_regional
  use constants, only: zero,one,five,tiny_r_kind
  use anberror, only: anisotropic
  use mpimod, only: mype
  use intallmod, only: intall
  use stpcalcmod, only: stpcalc
  use mod_strong, only: jcstrong,baldiag_inc
  use adjtest, only : adtest
  use control_vectors
  use state_vectors
  use bias_predictors
  use xhat_vordivmod, only : xhat_vordiv_init, xhat_vordiv_calc, xhat_vordiv_clean
  use timermod, only: timer_ini,timer_fnl
  use projmethod_support, only: init_mgram_schmidt, &
                                mgram_schmidt,destroy_mgram_schmidt
  use hybrid_ensemble_parameters,only : l_hyb_ens,aniso_a_en

  implicit none

! Declare passed variables

! Declare local parameters
  real(r_kind),parameter:: start_step=1.e-4_r_kind
  real(r_kind),parameter:: small_step=1.e-5_r_kind

! Declare local variables  
  logical iout_6,restart,end_iter,llprt,llouter
  character(5) step(2)
  integer(i_kind) i,istep,iobs,ii,nprt
  real(r_kind) stp,b,converge,gsave
  real(r_kind) gnormx,penx,penalty,pennorm
  real(r_quad) zjo
  real(r_kind),dimension(2):: gnorm
  real(r_kind) :: zgini,zfini,fjcost(4),zgend,zfend
  real(r_kind) :: fjcost_e
  type(control_vector) :: xhat,gradx,grady,dirx,diry,ydiff
  type(state_vector) :: sval(nobs_bins), rval(nobs_bins)
  type(state_vector) :: mval(nsubwin)
  type(predictors) :: sbias, rbias
  logical:: lanlerr
  

!    note that xhatt,dirxt,xhatp,dirxp are added to carry corrected grid fields
!      of t and p from implicit normal mode initialization (strong constraint option)
!     inmi generates a linear correction to t,u,v,p.  already have xhatuv which can
!      be used for the corrected wind, but nothing for t,p.  xhatt, etc are used exactly
!       like xhatuv,dirxuv.

! Step size diagnostic strings
  data step /'good', 'SMALL'/

!**********************************************************************

! Initialize timer
  call timer_ini('pcgsoi')

  if (ladtest) call adtest()

! Set constants.  Initialize variables.
  restart=.false.
  if (jiter==0 .and. (iguess==1 .or. iguess==2)) restart=.true.
  pennorm=10.e50_r_kind
  iout_6=.true.
  stp=start_step
  if (iout_iter==6) iout_6=.false.
  end_iter=.false.
  gsave=zero
  llouter=.false.

! Convergence criterion needs to be relaxed a bit for anisotropic mode,
! because the anisotropic recursive filter, for reasons of computational
! efficiency, uses r_single (4 byte) arithmetic.  this generally triggers
! the warning about penalty increasing, but this doesn't happen until
! the gradient has been reduced by more than 9 orders of magnitude.
  converge=1.e-10_r_kind
  if(anisotropic) converge=1.e-9_r_kind

! Allocate required memory and initialize fields
  call init_
  if(print_diag_pcg)call prt_guess('guess')

  lanlerr=.false.
  if ( twodvar_regional .and. jiter==1 ) lanlerr=.true.
  if ( lanlerr .and. lgschmidt ) call init_mgram_schmidt

! Perform inner iteration
  inner_iteration: do iter=0,niter(jiter)

! Gradually turn on variational qc to avoid possible convergence problems
     nlnqc_iter = iter >= niter_no_qc(jiter)
     if(jiter == jiterstart) then
        varqc_iter=c_varqc*(iter-niter_no_qc(1)+one)
        if(varqc_iter >=one) varqc_iter= one
     else
        varqc_iter=one
     endif

     do ii=1,nobs_bins
        rval(ii)=zero
     end do
     gradx=zero
     llprt=(mype==0).and.(iter<=1)

     if (l4dvar) then
!       Convert from control space to model space
        call control2state(xhat,mval,sbias)

!       Perform test of AGCM TLM and ADM
        call geos_pgcmtest(mval,sval,llprt)

!       Run TL model to fill sval
        call model_tl(mval,sval,llprt)
     else

!       Convert from control space directly to physical
!       space for comparison with obs.
        call control2state(xhat,sval,sbias)
     end if

     if (iter<=1 .and. print_diag_pcg) then
        do ii=1,nobs_bins
           call prt_state_norms(sval(ii),'sval')
        enddo
     endif

!    Compare obs to solution and transpose back to grid
     call intall(sval,sbias,rval,rbias)

     if (iter<=1 .and. print_diag_pcg) then
        do ii=1,nobs_bins
           call prt_state_norms(rval(ii),'rval')
        enddo
     endif

     if (l4dvar) then
!       Run adjoint model
        call model_ad(mval,rval,llprt)

!       Adjoint of convert control var to physical space
        call state2control(mval,rbias,gradx)
     else

!       Convert to control space directly from physical space.
        if (nobs_bins>1) then
           do ii=nobs_bins,2,-1
              call self_add(rval(1),rval(ii))
           end do
        end if
        call state2control(rval,rbias,gradx)
     end if

!    Print initial Jo table
     if (iter==0 .and. print_diag_pcg) then
        nprt=2
        call evaljo(zjo,iobs,nprt,llouter)
        call prt_control_norms(gradx,'gradx')
     endif

!    Add contribution from background term
!$omp parallel do
     do i=1,nclen
        gradx%values(i)=gradx%values(i)+yhatsave%values(i)
     end do
!$omp end parallel do

!    Multiply by background error
     if(anisotropic) then
        call anbkerror(gradx,grady)
        if(lanlerr .and. lgschmidt) call mgram_schmidt(gradx,grady)
     else
        call bkerror(gradx,grady)
     end if

!    If hybrid ensemble run, then multiply ensemble control variable a_en 
!                                    by its localization correlation
     if(l_hyb_ens) then
        if(aniso_a_en) then
    !      call anbkerror_a_en(gradx,grady)    !  not available yet
           write(6,*)' ANBKERROR_A_EN not written yet, program stops'
           stop
        else
           call bkerror_a_en(gradx,grady)
        end if

!       multiply static (Jb) part of grady by beta1_inv, and
!       multiply ensemble (Je) part of grady by beta2_inv = ( 1 - beta1_inv )
!         (this determines relative contributions from static background Jb and ensemble background Je)

        call beta12mult(grady)

     end if

     if (lanlerr) then
!$omp parallel do
        do i=1,nclen
           ydiff%values(i)=gradx%values(i)
        end do
!$omp end parallel do
     else
!$omp parallel do
        do i=1,nclen
           ydiff%values(i)=gradx%values(i)-ydiff%values(i)
        end do
!$omp end parallel do
     end if

     if (iter==0 .and. print_diag_pcg) then
        call prt_control_norms(grady,'grady3')
     endif

!    Calculate new norm of gradients
     if (iter>0) gsave=gnorm(1)
     gnorm(1)=dot_product(gradx,grady,r_quad)
     gnorm(2)=dot_product(ydiff,grady,r_quad)
     b=zero
     if (gsave>1.e-16_r_kind .and. iter>0) b=gnorm(2)/gsave
     if (b<zero .or. b>five) then
        if (mype==0) then
           if (iout_6) write(6,105) gnorm(2),gsave,b
           write(iout_iter,105) gnorm(2),gsave,b
        endif
        b=zero
     endif
     if (mype==0) write(6,888)'pcgsoi: gnorm(1:2),b=',gnorm,b

!    Calculate new search direction
     if (.not. restart) then
!$omp parallel do
        do i=1,nclen
           ydiff%values(i)=gradx%values(i)
           dirx%values(i)=-grady%values(i)+b*dirx%values(i)
           diry%values(i)=-gradx%values(i)+b*diry%values(i)
        end do
!$omp end parallel do
     else
!    If previous solution available, transfer into local arrays.
        ydiff=zero
        call read_guess_solution(dirx,diry,mype)
        stp=one
     endif

     if (l4dvar) then
!       Convert from control space to model space
        call control2state(dirx,mval,rbias)

!       Run TL model to fill rval
!       The TLM being linear, rval could be updated in the same way as the
!       control variable. That would eliminate the need for running the TLM
!       at the cost of storing an additional 4D state variable.
        call model_tl(mval,rval,llprt)
     else

!       Convert search direction form control space to physical space
        call control2state(dirx,rval,rbias)
     end if

!    Calculate stepsize
     call stpcalc(stp,sval,sbias,xhat,dirx,rval,rbias, &
                  diry,penalty,fjcost,end_iter)

     if (lanlerr) call writeout_gradients(gradx,grady,niter(jiter),stp,b,mype)

!    Diagnostic calculations
     if (iter==0) then
        zgini=gnorm(1)
        zfini=penalty
        if (mype==0) then
           write(6,888)'Initial cost function =',zfini
           write(6,888)'Initial gradient norm =',sqrt(zgini)
        endif
        if(jiter==jiterstart .or. oberror_tune) then
           gnormorig=gnorm(1)
           penorig=penalty
        end if
     endif

     gnormx=gnorm(1)/gnormorig
     penx=penalty/penorig

     if (mype==0) then
        write(6,*)'Minimization iteration',iter
        write(6,999)'grepcost J,Jb,Jo,Jc,Jl =',jiter,iter,penalty,fjcost
        if (zgini>tiny_r_kind) then
           write(6,999)'grepgrad grad,reduction=',jiter,iter,sqrt(gnorm(1)),sqrt(gnorm(1)/zgini)
        else
           write(6,999)'grepgrad grad,reduction(N/A)=',jiter,iter,sqrt(gnorm(1))
        endif
        write(6,999)'pcgsoi: cost,grad,step =',jiter,iter,penalty,sqrt(gnorm(1)),stp
        istep=1
        if (stp<small_step) istep=2
!       if (iout_6) write(6,110) jiter,iter,penalty,gnorm(1),stp,b
!       if (iout_6) write(6,120) jiter,iter,penx,gnormx,step(istep)
        write(iout_iter,110) jiter,iter,penalty,gnorm(1),stp,b
        write(iout_iter,120) jiter,iter,penx,gnormx,step(istep)

110     format(' penalty,grad ,a,b= ',i3,i4,1x,4(e24.18,1x),2(g12.6,1x))
120     format(' pnorm,gnorm, step? ',i3,i4,1x,2(e24.18,1x),a5)
     endif
999  format(A,2(1X,I3),5(1X,ES25.18))
9991 format(A,2(1X,I3),6(1X,ES25.18))

!    Check for convergence or failure of algorithm
     if(gnormx < converge .or. penalty < converge  .or.  &
        penx >= pennorm .or. end_iter)then

        if(mype == 0)then
           if(iout_6) write(6,101)
           write(iout_iter,101)

           if(gnormx < converge) then
              if(iout_6)write(6,130)gnormx,converge
              write(iout_iter,130) gnormx,converge
           end if
           if(penalty < converge) then
              if(iout_6)write(6,131)penalty,converge
              write(iout_iter,131) penalty,converge
           end if
           if(penx >= pennorm) then
              if(iout_6)write(6,100)jiter,iter,penx,pennorm
              write(iout_iter,100)jiter,iter,penx,pennorm
           end if
           if(end_iter)then
              if(stp > zero)then
                 if(iout_6)write(6,140)
                 write(iout_iter,140)
              else
                 if(iout_6)write(6,141)
                 write(iout_iter,141)
              end if
           end if
        end if
101     format(' PCGSOI: WARNING **** Stopping inner iteration ***')
100     format(' Penalty increase or constant ',I3,1x,i4,1x,2(e24.18,1x))
105     format(' PCGSOI: WARNING **** Reset to steepest descent, gnorm(2),gsave,b= ',3(e24.18,1x))
130     format(' gnorm ', e24.18,' less than ',e24.18)
131     format(' penalty ', e24.18,' less than ',e24.18)
140     format(' Stepsize calculation terminates inner iteration - probable convergence')
141     format(' Stepsize calculation terminates inner iteration - probable error')
        exit inner_iteration
     end if
     pennorm=penx

  end do inner_iteration

  if (lanlerr .and. lgschmidt) call destroy_mgram_schmidt

! Calculate adjusted observation error factor
  if( oberror_tune .and. (.not.l4dvar) ) then
     if (mype == 0) write(6,*) 'PCGSOI:  call penal for obs perturbation'
     call control2state(xhat,sval,sbias)
     call penal(sval(1))
     xhatsave=zero
     yhatsave=zero
     call clean_
     return
  endif

! Update contributions of incremental values from current outer loop

  if (jcstrong .and. baldiag_inc) call strong_baldiag_inc(sval)

! Evaluate final cost function and gradient
  if (mype==0) write(6,*)'Minimization final diagnostics'

  do ii=1,nobs_bins
     rval(ii)=zero
  end do
  gradx=zero
  llprt=(mype==0)
  if (l4dvar) then
     call control2state(xhat,mval,sbias)
     call model_tl(mval,sval,llprt)
  else
     call control2state(xhat,sval,sbias)
  end if
  call intall(sval,sbias,rval,rbias)
  if (l4dvar) then
     call model_ad(mval,rval,llprt)
     call state2control(mval,rbias,gradx)
  else
     if (nobs_bins>1) then
        do ii=nobs_bins,2,-1
           call self_add(rval(1),rval(ii))
        end do
     end if
     call state2control(rval,rbias,gradx)
  end if

! Multiply by background error
  if(anisotropic) then
     call anbkerror(gradx,grady)
  else
     call bkerror(gradx,grady)
  end if

!    If hybrid ensemble run, then multiply ensemble control variable a_en 
!                                    by its localization correlation
  if(l_hyb_ens) then
     if(aniso_a_en) then
    !   call anbkerror_a_en(gradx,grady)    !  not available yet
        write(6,*)' ANBKERROR_A_EN not written yet, program stops'
        stop
     else
        call bkerror_a_en(gradx,grady)
     end if

!    multiply static (Jb) part of grady by beta1_inv, and
!    multiply ensemble (Je) part of grady by beta2_inv = ( 1 - beta1_inv )
!      (this determines relative contributions from static background Jb and ensemble background Je)

     call beta12mult(grady)

  end if


! Print final Jo table
  if(print_diag_pcg)then
     zgend=dot_product(gradx,grady,r_quad)
     nprt=2
     call evaljo(zjo,iobs,nprt,llouter)
     call prt_control_norms(gradx,'gradx')

     if(l_hyb_ens) then

!       If hybrid ensemble run, compute contribution to Jb and Je separately

        fjcost_e=   dot_product(xhatsave,yhatsave,r_quad,'cost_e')
        fjcost(1) = dot_product(xhatsave,yhatsave,r_quad,'cost_b')

     else
        fjcost(1) = dot_product(xhatsave,yhatsave,r_quad)
     end if
     fjcost(2) = zjo
     zfend=SUM(fjcost(:))
     if(l_hyb_ens) zfend=zfend+fjcost_e

     if (mype==0) then
        if(l_hyb_ens) then

!          If hybrid ensemble run, print out contribution to Jb and Je separately

           write(6,9991)'grepcost J,Jb,Je,Jo,Jc,Jl =',jiter,iter,zfend,fjcost(1),fjcost_e,fjcost(2:4)

        else
           write(6,999)'grepcost J,Jb,Jo,Jc,Jl =',jiter,iter,zfend,fjcost
        end if
        if (zgini>tiny_r_kind) then
           write(6,999)'grepgrad grad,reduction=',jiter,iter,sqrt(zgend),sqrt(zgend/zgini)
        else
           write(6,999)'grepgrad grad,reduction(N/A)=',jiter,iter,sqrt(zgend)
        endif
     endif

!    Print final diagnostics
     if (mype==0) then
        write(6,888)'Final   cost function=',zfend
        write(6,888)'Final   gradient norm=',sqrt(zgend)
        write(6,888)'Final/Initial cost function=',zfend/zfini
        if(zgini>tiny_r_kind) write(6,888)'Final/Initial gradient norm=',sqrt(zgend/zgini)
     endif
888  format(A,5(1X,ES25.18))

  end if
! Calculate increments of vorticity/divergence
  call xhat_vordiv_init
  call xhat_vordiv_calc(sval)

! Update guess (model background, bias correction) fields
  if (mype==0) write(6,*)'pcgsoi: Updating guess'
  call update_guess(sval,sbias)
  if(l_foto) call update_geswtend(xhat_dt%u,xhat_dt%v,xhat_dt%t,&
                                  xhat_dt%q,xhat_dt%oz,xhat_dt%cw,&
                                  xhat_dt%p)
! Write output analysis files
  if(jiter == miter)call write_all(.false.,mype)
  call prt_guess('analysis')

! Overwrite guess with increment (4d-var only, for now)
  if (lwrtinc) then
     call inc2guess(sval)
     call write_all(lwrtinc,mype)
     call prt_guess('increment')
  endif

! Clean up increments of vorticity/divergence
  call xhat_vordiv_clean

! Clean up major fields
  call clean_

! Finalize timer
  call timer_fnl('pcgsoi')

! End of routine
  return

contains

subroutine init_
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    init_    initialize pcgsoi
!   prgmmr:      Todling
!   
! abstract: initialize pcgsoi required fields on way in
!
! program history log:
!   2008-12-01  Todling
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language: f90
!   mschine:
!
!$$$ end documentation block

implicit none

! Allocate local variables
  call allocate_cv(xhat)
  call allocate_cv(gradx)
  call allocate_cv(grady)
  call allocate_cv(dirx)
  call allocate_cv(diry)
  call allocate_cv(ydiff)
  do ii=1,nobs_bins
     call allocate_state(sval(ii))
     call allocate_state(rval(ii))
  end do
  call allocate_preds(sbias)
  call allocate_preds(rbias)
  if (l4dvar) then
     do ii=1,nsubwin
        call allocate_state(mval(ii))
     end do
  end if

  gradx=zero
  grady=zero
  dirx=zero
  diry=zero
  ydiff=zero
  xhat=zero

  if(l_foto)then
     call allocate_state(xhat_dt)
     call assign_scalar2state(xhat_dt,zero)
  end if

end subroutine init_

subroutine clean_
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    clean_    clean pcgsoi
!   prgmmr:      Todling
!   
! abstract: clean pcgsoi required fields on way out
!
! program history log:
!   2008-12-01  Todling
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

implicit none

! Deallocate obs file
  if (.not.l4dvar) call destroyobs

! Release state-vector memory
  call deallocate_cv(xhat)
  call deallocate_cv(gradx)
  call deallocate_cv(grady)
  call deallocate_cv(dirx)
  call deallocate_cv(diry)
  call deallocate_cv(ydiff)

! Release bias-predictor memory
  call deallocate_preds(sbias)
  call deallocate_preds(rbias)

  do ii=1,nobs_bins
     call deallocate_state(sval(ii))
     call deallocate_state(rval(ii))
  end do
  if (l4dvar) then
     do ii=1,nsubwin
        call deallocate_state(mval(ii))
     end do
  end if
  if(l_foto)then
     call deallocate_state(xhat_dt)
  end if
  call inquire_state

end subroutine clean_

end subroutine pcgsoi

end module pcgsoimod
