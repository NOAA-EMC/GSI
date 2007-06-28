module stpcalcmod

!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    stpcalcmod    module for stpcalc and its tangent linear stpcalc_tl
!
! abstract: module for stpcalc and its tangent linear stpcalc_tl
!
! program history log:
!   2005-05-21  Yanqiu zhu - wrap stpcalc and its tangent linear stpcalc_tl into one module
!   2005-11-21  Derber - remove interfaces and clean up code
!

implicit none

PRIVATE
PUBLIC stpcalc,stpcalc_tl

contains

subroutine stpcalc(stpinout,xhat,xhatt,xhatp,xhatuv,xhat_x,xhat_y,xhat_t, &
     dirx,dirxt,dirxp,dirxuv,dirx_x,dirx_y,dirx_t,diry,penalty,mype,end_iter)

!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    stpcalc     calculate penalty and stepsize
!   prgmmr: derber           org: np23                date: 2003-12-18
!
! abstract: calculate current penalty and estimate stepsize
!               (nonlinear qc version)
!
!    A description of nonlinear qc follows:
!
!    The observation penalty Jo is defined as
!
!          Jo =  - (sum over obs) 2*log(Po)
!
!      where,
!
!          Po = Wnotgross*exp(-.5*(Hn(x+xb) - yo)**2 ) + Wgross
!            with
!                Hn = the forward model (possibly non-linear) normalized by 
!                     observation error
!                x  = the current estimate of the analysis increment
!                xb = the background state
!                yo = the observation normalized by observation error
!
!            Note:  The factor 2 in definition of Jo is present because the 
!                   penalty Jo as used in this code is 2*(usual definition 
!                   of penalty)
!
!          Wgross = Pgross*cg
!
!          Wnotgross = 1 - Wgross
!
!          Pgross = probability of gross error for observation (assumed
!                   here to have uniform distribution over the possible
!                   range of values)
!
!          cg = sqrt(2*pi)/2b
!
!          b = possible range of variable for gross errors, normalized by 
!              observation error
!
!    The values for the above parameters that Bill Collins used in the
!    eta 3dvar are:
!
!          cg = cg_term/b, where cg_term = sqrt(2*pi)/2 
!
!          b = 10.        ! range for gross errors, normalized by obs error
!
!          pg_q=.002      ! probability of gross error for specific humidity
!          pg_pw=.002     ! probability of gross error for precipitable water
!          pg_p=.002      ! probability of gross error for pressure
!          pg_w=.005      ! probability of gross error for wind
!          pg_t=.007      ! probability of gross error for temperature
!          pg_rad=.002    ! probability of gross error for radiances
!
!
!    Given the above Jo, the gradient of Jo is as follows:
!
!                                             T
!        gradx(Jo) = - (sum over observations) 2*H (Hn(x+xb)-yo)*(Po - Wgross)/Po
!
!      where, 
!
!          H = tangent linear model of Hn about x+xb
!
! 
!    Note that if Pgross = 0.0, then Wnotgross=1.0 and Wgross=0.0.  That is,
!    the code runs as though nonlinear quality control were not present
!    (which is indeed the case since the gross error probability is 0).  
!
!    As a result the same stp* routines may be used for use with or without
!    nonlinear quality control.
!    
!    Please note, however, that using the nonlinear qc routines makes the
!    stp* and int* operators nonlinear.  Hence, the need to evaluate the
!    step size operators twice for each observation type, give the current
!    step size algorithm coded below. 
!
!
! program history log:
!   2003-12-18  derber,j.
!   2004-07-23  derber  - modify to include conventional sst
!   2004-07-28  treadon - add only to module use, add intent in/out
!   2004-10-06  parrish - add nonlinear qc option
!   2004-10-06  kleist  - separate control vector for u,v, get search
!                         direction for u,v from dir for st,vp
!   2004-11-30  treadon - add brightness temperatures to nonlinear
!                         quality control
!   2005-01-20  okamoto - add u,v to stprad_qc
!   2005-01-26  cucurull- implement local GPS RO linear operator
!   2005-02-10  treadon - add u,v to stprad_qc (okamoto change not present)
!   2005-02-23  wu      - add call to normal_rh_to_q to convert normalized 
!                         RH to q
!   2005-04-11  treadon - rename stpcalc_qc as stpcalc
!   2005-05-21  yanqiu zhu - add 'use stp*mod', and modify call interfaces for using these modules
!   2005-05-27  derber - remove linear stepsize estimate
!   2005-06-03  parrish - add horizontal derivatives
!   2005-07-10  kleist  - add dynamic constraint term (linear)
!   2005-09-29  kleist  - expand Jc term, include time derivatives vector
!   2005-11-21  kleist  - separate tendencies from Jc term, add call to calctends tlm
!   2005-12-01  cucurull - add code for GPS local bending angle, add use obsmod for ref_obs
!   2005-12-20  parrish - add arguments to call to stpt to enable boundary layer forward
!                         model option.
!   2006-04-18  derber - add explicit iteration over stepsize (rather than 
!                        repeated calls) - clean up and simplify
!   2006-04-24  kleist - include both Jc formulations
!   2006-05-26  derber - modify to improve convergence checking
!   2006-07-26  parrish - correct inconsistency in computation of space and time derivatives of q
!                          currently, if derivatives computed, for q it is normalized q, but
!                          should be mixing ratio.
!   2006-08-04  parrish - add strong constraint initialization option
!   2006-09-18  derber - modify output from nonlinear operators to make same as linear operators
!   2006-09-20  derber - add sensible temperatures for conventional obs.
!   2006-10-12  treadon - replace virtual temperature with sensible in stppcp
!
!   input argument list:
!     stpinout - guess stepsize
!     xhat     - current solution
!     xhatt  - current solution for temp
!     xhatp  - current solution for psfc
!     xhatuv   - current solution for u,v
!     xhat_x   - current solution x derivative
!     xhat_y   - current solution y derivative
!     xhat_t   - current solution t derivative
!     dirx     - search direction for x
!     diry     - search direction for y (B-1 dirx)
!     mype     - pe number
!     end_iter - end iteration flag
!
!   output argument list:
!     dirxt  - search direction for temp
!     dirxp  - search direction for psfc
!     dirxuv   - search direction for u,v
!     dirx_x   - x derivative of search direction for x 
!     dirx_y   - y derivative of search direction for x
!     dirx_t   - t derivative of search direction for x
!     stpinout - final estimate of stepsize
!     penalty  - penalty
!     end_iter - end iteration flag false if stepsize successful
!
!
! remarks:
!     The part of xhat and dirx containing temps and psfc are values before strong initialization,
!     xhatt, xhatp and dirxt, dirxp contain temps and psfc after strong initialization.
!     If strong initialization is turned off, then xhatt, etc are equal to the corresponding 
!     fields in xhat, dirx.
!     xhatuv, xhat_x, xhat_y, xhat_t and dirxuv, dirx_x, dirx_y, dirx_t are all after
!     strong initialization if it is turned on.
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,r_double,i_kind
  use mpimod, only: ierror,mpi_comm_world,mpi_rtype,mpi_sum,levs_id,npe
  use constants, only:  zero,one_tenth,quarter,half,one,two
  use jfunc, only: noz,nq,nt,nsst,ncw,np,iout_iter,nst,nvp,&
       nclen,nclen1,nclen2,nsclen,npclen,xhatsave,yhatsave,factqmin,factqmax,&
       nuvlen,nu,nv,iter,switch_on_derivatives,ntendlen,nut,nvt,ntt,nprst,&
       nqt,nozt,ncwt,ndivt,nagvt,tendsflag
  use gridmod, only: latlon1n,latlon11,lat2,lon2,nsig,nsig1o
  use jcmod, only: jcterm,jcdivt
  use obsmod, only: ref_obs
  use radinfo, only: npred,jpch
  use pcpinfo, only: jtype,npredp
  use stptmod
  use stpwmod
  use stppsmod
  use stppwmod
  use stpqmod
  use stpradmod
  use stprefmod
  use stpbendmod
  use stprwmod
  use stpspdmod
  use stpsrwmod
  use stpsstmod
  use stpdwmod
  use stppcpmod
  use stpozmod
  use stplimqmod
  use dprodxmod
  use mod_vtrans,only: nvmodes_keep
  use mod_inmi,only: nstrong
  use specmod, only: jcap
  implicit none

! Declare passed variables
  integer(i_kind),intent(in):: mype
  real(r_kind),intent(inout):: stpinout
  logical,intent(inout):: end_iter
  real(r_kind),intent(out):: penalty
  real(r_kind),dimension(nuvlen),intent(out):: dirxuv
  real(r_kind),dimension(nclen),intent(in)::dirx,xhat,diry
  real(r_kind),dimension(latlon1n),intent(in)::xhatt
  real(r_kind),dimension(latlon11),intent(in)::xhatp
  real(r_kind),dimension(nuvlen),intent(in):: xhatuv
  real(r_kind),dimension(nclen),intent(in)::xhat_x,xhat_y
  real(r_kind),dimension(nclen),intent(out)::dirx_x,dirx_y
  real(r_kind),dimension(ntendlen),intent(in)::xhat_t
  real(r_kind),dimension(ntendlen),intent(out)::dirx_t
  real(r_kind),dimension(latlon1n),intent(out)::dirxt
  real(r_kind),dimension(latlon11),intent(out)::dirxp

! Declare local parameters
  integer(i_kind),parameter:: ipen = 20
  integer(i_kind),parameter:: ipen_tot = ipen*3
  integer(i_kind),parameter:: istp_iter = 2

! Declare local variables
  logical limq_warning
  integer(i_kind) i,j,k,nnn,mm1,ii
           integer(i_kind) istrong
  real(r_kind) dels,sges1,sges2,sges3,delpen
  real(r_kind) bx,cx
  real(r_kind),dimension(ipen):: pen,bpen,cpen,stpx   
  real(r_kind),dimension(0:istp_iter):: stp   
  real(r_kind),dimension(ipen,3,npe):: pbc,pbc1   
  real(r_kind),dimension(latlon1n):: dirx_q,xhat_q,dirx_tsen,xhat_tsen
  real(r_kind),dimension(latlon1n)::u_t_g,v_t_g,t_t_g
  real(r_kind),dimension(latlon11)::ps_t_g

!************************************************************************************  
! Initialize variable
  mm1=mype+1
  stp(0)=stpinout

!   Begin calculating contributions to penalty and stepsize for various terms
!   stepsize = sum(b)/sum(c)
!
!    pbc(*,*,mm1) - mm1 = processor + 1
!
!    pbc(*,1,*) - penalty
!    pbc(*,2,*) - b
!    pbc(*,3,*) - c
!
!    linear terms
!
!    pbc(1,*,*)  contribution from background
!    pbc(2,*,*)  contribution from bias coeff. background term
!    pbc(3,*,*)  contribution from precip. bias correction background term
!    pbc(4,*,*)  contribution from dynamic constraint term (Jc)
!
!  nonlinear terms
!
!    pbc(5,*,*)  contribution from wind observation term
!    pbc(6,*,*)  contribution from satellite radiance observation term
!    pbc(7,*,*)  contribution from temperature observation term
!    pbc(8,*,*)  contribution from precipitable water obs. term
!    pbc(9,*,*)  contribution from specific humidity obs.term
!    pbc(10,*,*) contribution from ozone obs. term
!    pbc(11,*,*) contribution from doppler lidar wind
!    pbc(12,*,*) contribution from doppler radar wind
!    pbc(13,*,*) contribution from radar superob wind
!    pbc(14,*,*) contribution from GPS local observations
!    pbc(15,*,*) contribution from conventional sst
!    pbc(16,*,*) contribution from wind speed obs. term
!    pbc(17,*,*) contribution from precipitation term
!    pbc(18,*,*) contribution from negative moisture constraint term
!    pbc(19,*,*) contribution from excess moisture term
!    pbc(20,*,*) contribution from surface pressure observation term
!

  pbc=zero

! penalty, b and c for background terms

  pbc(1,1,mm1)= dplev(xhatsave,yhatsave)
  pbc(1,2,mm1)=-dplev(dirx,yhatsave)
  pbc(1,3,mm1)= dplev(dirx,diry)

! bias correction terms

  if(mype == 0)then

! penalty, b, c terms for satellite bias correction

   do i=nclen1+1,nclen2
      pbc(2,1,mm1)=pbc(2,1,mm1)+xhatsave(i)*yhatsave(i)
      pbc(2,2,mm1)=pbc(2,2,mm1)-dirx(i)*yhatsave(i)
      pbc(2,3,mm1)=pbc(2,3,mm1)+dirx(i)*diry(i)
   end do

! penalty, b, c terms for precipitation bias correction

   do i=nclen2+1,nclen
      pbc(3,1,mm1)=pbc(3,1,mm1)+xhatsave(i)*yhatsave(i)
      pbc(3,2,mm1)=pbc(3,2,mm1)-dirx(i)*yhatsave(i)
      pbc(3,3,mm1)=pbc(3,3,mm1)+dirx(i)*diry(i)
   end do
  end if


! Convert search direction for st/vp to u/v for stp routines
  call getuv(dirxuv(nu),dirxuv(nv),dirx(nst),dirx(nvp))

!  load dirxt and dirxp
  do i=1,latlon1n
    dirxt(i)=dirx(nt+i-1)
  end do
  do i=1,latlon11
    dirxp(i)=dirx(np+i-1)
  end do

!   convert normalized rh variable to q
  call normal_rh_to_q(dirx(nq),dirxt,dirxp,dirx_q)
  call normal_rh_to_q(xhat(nq),xhatt,xhatp,xhat_q)

  if(switch_on_derivatives) then
!     compute derivatives
! Determine how many vertical levels each mpi task will
! handle in computing horizontal derivatives
    nnn=0
    do k=1,nsig1o
      if (levs_id(k)/=0) nnn=nnn+1
    end do
    call get_derivatives( &
       dirxuv(nu) ,dirxuv(nv) ,dirxt     ,dirxp   ,  &
       dirx_q     ,dirx  (noz),dirx  (nsst),dirx  (ncw), &
       dirx_x(nst),dirx_x(nvp),dirx_x(nt)  ,dirx_x(np),  &
       dirx_x(nq) ,dirx_x(noz),dirx_x(nsst),dirx_x(ncw), &
       dirx_y(nst),dirx_y(nvp),dirx_y(nt)  ,dirx_y(np),  &
       dirx_y(nq) ,dirx_y(noz),dirx_y(nsst),dirx_y(ncw), &
       nnn,mype,1)
  end if

  if (tendsflag) then
    call calctends_tl( &
       dirxuv(nu) ,dirxuv(nv)  ,dirxt    ,dirxp  ,     &
       dirx_q     ,dirx(noz)   ,dirx(ncw)  ,               &
       dirx_x(nst),dirx_y(nst) ,dirx_x(nvp),dirx_y(nvp),   &
       dirx_x(nt) ,dirx_y(nt)  ,dirx_x(np) ,dirx_y(np),    &
       dirx_x(nq) ,dirx_y(nq)  ,dirx_x(noz),dirx_y(noz),   &
       dirx_x(ncw),dirx_y(ncw) ,jcdivt,     mype,          &
       dirx_t(nut),dirx_t(nvt) ,dirx_t(ntt),dirx_t(nprst), &
       dirx_t(nqt),dirx_t(nozt),dirx_t(ncwt),dirx_t(ndivt),&
       dirx_t(nagvt))
    if(nvmodes_keep > 0 .and. nstrong > 0) then
      do istrong=1,nstrong
        call strong_bal_correction(dirx_t(nut),dirx_t(nvt),dirx_t(ntt), &
             dirx_t(nprst),mype,dirxuv(nu),dirxuv(nv),dirxt,dirxp, &
             u_t_g,v_t_g,t_t_g,ps_t_g,.false.,.true.)

!    update dirx_q to be consistent with new dirxt, dirxp
        call normal_rh_to_q(dirx(nq),dirxt,dirxp,dirx_q)

        if(istrong < nstrong) then

          call get_derivatives( &
             dirxuv(nu) ,dirxuv(nv) ,dirxt     ,dirxp   ,  &
             dirx_q     ,dirx  (noz),dirx  (nsst),dirx  (ncw), &
             dirx_x(nst),dirx_x(nvp),dirx_x(nt)  ,dirx_x(np),  &
             dirx_x(nq) ,dirx_x(noz),dirx_x(nsst),dirx_x(ncw), &
             dirx_y(nst),dirx_y(nvp),dirx_y(nt)  ,dirx_y(np),  &
             dirx_y(nq) ,dirx_y(noz),dirx_y(nsst),dirx_y(ncw), &
             nnn,mype,1)
          call calctends_tl( &
             dirxuv(nu) ,dirxuv(nv)  ,dirxt     ,dirxp  ,    &
             dirx_q     ,dirx(noz)   ,dirx(ncw)  ,               &
             dirx_x(nst),dirx_y(nst) ,dirx_x(nvp),dirx_y(nvp),   &
             dirx_x(nt) ,dirx_y(nt)  ,dirx_x(np) ,dirx_y(np),    &
             dirx_x(nq) ,dirx_y(nq)  ,dirx_x(noz),dirx_y(noz),   &
             dirx_x(ncw),dirx_y(ncw) ,jcdivt,     mype,          &
             dirx_t(nut),dirx_t(nvt) ,dirx_t(ntt),dirx_t(nprst), &
             dirx_t(nqt),dirx_t(nozt),dirx_t(ncwt),dirx_t(ndivt),&
             dirx_t(nagvt))
        end if
      end do
    end if
  end if

  call tv_to_tsen(dirxt,dirx_q,dirx_tsen)
  call tv_to_tsen(xhatt,xhat_q,xhat_tsen)

! step size from dynamic constraint based on time tendencies
  if (jcterm) then
    if (jcdivt) then
      call stpjc_divt(dirx_t(ndivt),dirx_t(nagvt),xhat_t(ndivt),xhat_t(nagvt),&
                 mype,pbc(4,1,mm1),pbc(4,2,mm1),pbc(4,3,mm1))
    else
      call stpjc(dirx_t(nut),dirx_t(nvt),dirx_t(ntt),dirx_t(nprst),&
                 xhat_t(nut),xhat_t(nvt),xhat_t(ntt),xhat_t(nprst),&
                 mype,pbc(4,1,mm1),pbc(4,2,mm1),pbc(4,3,mm1))
    end if
  end if


! iterate over number of stepsize iterations (istp_iter - currently set to 2
  do ii=1,istp_iter

    dels=one_tenth ** ii
!   Do nonlinear terms
  
    sges1=(one-dels)*stp(ii-1)
    sges2=stp(ii-1)
    sges3=(one+dels)*stp(ii-1)

!   penalty, b, and c for winds
    call stpw(dirxuv(nu),dirxuv(nv),xhatuv(nu),xhatuv(nv), &
              pbc(5,1,mm1),pbc(5,2,mm1),pbc(5,3,mm1),sges1,sges2,sges3)

!   penalty, b, and c for radiances
    call stprad(dirxt  ,dirx_q,dirx(noz),dirxuv(nu),dirxuv(nv),dirx(nsst), &
                xhatt  ,xhat_q,xhat(noz),xhatuv(nu),xhatuv(nv),xhat(nsst), &
                dirx(nclen1+1),xhat(nclen1+1), &
                pbc(6,1,mm1),pbc(6,2,mm1),pbc(6,3,mm1),sges1,sges2,sges3)

!   penalty, b, and c for temperature
    call stpt(dirx_tsen,xhat_tsen,dirxt,xhatt,dirx_q,xhat_q, &
              dirxuv(nu),xhatuv(nu),dirxuv(nv),xhatuv(nv), &
              dirxp  ,xhatp  ,dirx(nsst),xhat(nsst), &
              pbc(7,1,mm1),pbc(7,2,mm1),pbc(7,3,mm1),sges1,sges2,sges3)

!   penalty, b, and c for precipitable water
    call stppw(dirx_q,xhat_q, &
               pbc(8,1,mm1),pbc(8,2,mm1),pbc(8,3,mm1),sges1,sges2,sges3)

!   penalty, b, and c for moisture
    call stpq(dirx_q,xhat_q, &
              pbc(9,1,mm1),pbc(9,2,mm1),pbc(9,3,mm1),sges1,sges2,sges3)

!   penalty, b, and c for ozone
    call stpoz(dirx(noz),xhat(noz), &
               pbc(10,1,mm1),pbc(10,2,mm1),pbc(10,3,mm1),sges1,sges2,sges3)

!   penalty, b, and c for wind lidar
    call stpdw(dirxuv(nu),dirxuv(nv),xhatuv(nu),xhatuv(nv), &
               pbc(11,1,mm1),pbc(11,2,mm1),pbc(11,3,mm1),sges1,sges2,sges3)

!   penalty, b, and c for radar
    call stprw(dirxuv(nu),dirxuv(nv),xhatuv(nu),xhatuv(nv), &
               pbc(12,1,mm1),pbc(12,2,mm1),pbc(12,3,mm1),sges1,sges2,sges3)

!   penalty, b, and c for radar superob wind
    call stpsrw(dirxuv(nu),dirxuv(nv),xhatuv(nu),xhatuv(nv), &
                pbc(13,1,mm1),pbc(13,2,mm1),pbc(13,3,mm1),sges1,sges2,sges3)

!   penalty, b, and c for GPS local observation
    if (ref_obs) then
       call stpref(dirxt  ,dirx_q,dirxp  ,xhatt  ,xhat_q,xhatp  ,&
            pbc(14,1,mm1),pbc(14,2,mm1),pbc(14,3,mm1),sges1,sges2,sges3)
    else
       call stpbend(dirxt  ,dirx_q,dirxp  ,xhatt  ,xhat_q,xhatp  ,&
            pbc(14,1,mm1),pbc(14,2,mm1),pbc(14,3,mm1),sges1,sges2,sges3)
    endif

!   penalty, b, and c for conventional sst
    call stpsst(dirx(nsst),xhat(nsst),&
                pbc(15,1,mm1),pbc(15,2,mm1),pbc(15,3,mm1),sges1,sges2,sges3)

!   penalty, b, and c for wind speed
    call stpspd(dirxuv(nu),dirxuv(nv),xhatuv(nu),xhatuv(nv), &
                pbc(16,1,mm1),pbc(16,2,mm1),pbc(16,3,mm1),sges1,sges2,sges3)

!   penalty, b, and c for precipitation
    call stppcp(dirx_tsen,dirx_q,dirxuv(nu),dirxuv(nv),dirx(ncw), &
                xhat_tsen,xhat_q,xhatuv(nu),xhatuv(nv),xhat(ncw), &
                dirx(nclen2+1),xhat(nclen2+1),                   &
                pbc(17,1,mm1),pbc(17,2,mm1),pbc(17,3,mm1),sges1,sges2,sges3)

!   penalty, b, and c for moisture constraint
    call stplimq(dirx_q,xhat_q,sges1,sges2,sges3,       &
                 pbc(18,1,mm1),pbc(18,2,mm1),pbc(18,3,mm1),&
                 pbc(19,1,mm1),pbc(19,2,mm1),pbc(19,3,mm1))
  
!   penalty, b, and c for surface pressure
    call stpps(dirxp  ,xhatp  , &
               pbc(20,1,mm1),pbc(20,2,mm1),pbc(20,3,mm1),sges1,sges2,sges3)

!   Reduce output over all processors

    call mpi_allreduce(pbc,pbc1,ipen_tot*npe,mpi_rtype,mpi_sum, &
               mpi_comm_world,ierror)

!   penalty and sum b and sum c

    pen=zero
    bpen=zero
    cpen=zero
    do j=1,npe
     do i=1,ipen
       pen(i) = pen(i)+pbc1(i,1,j)
       bpen(i)=bpen(i)+pbc1(i,2,j)
       cpen(i)=cpen(i)+pbc1(i,3,j)
     end do
    end do
    penalty=zero
    bx=zero
    cx=zero
    stpx=zero
    do i=1,ipen
      penalty=penalty+pen(i)
      bx=bx+bpen(i)
      cx=cx+cpen(i)
      if(cpen(i) > 1.e-20_r_kind)stpx(i)=bpen(i)/cpen(i)
    end do

!   estimate of stepsize

    stp(ii)=stp(ii-1)
    if(cx > 1.e-20_r_kind) stp(ii)=min(bx/cx,one)         ! step size estimate


    delpen = two*stp(ii)*bx - stp(ii)**2*cx  

    if(abs(delpen/penalty) < 1.e-17_r_kind) then
      stpinout=stp(ii)
      if(mype == 0)then
        if(ii == 1)write(iout_iter,100) (pen(i),i=1,ipen)
        write(iout_iter,140) ii,delpen,bx,cx,stp(ii)
        write(iout_iter,101) (stpx(i),i=1,ipen)
        write(iout_iter,105) (bpen(i),i=1,ipen)
        write(iout_iter,110) (cpen(i),i=1,ipen)
      end if
      end_iter = .true.
      return
    end if

    if(cx < 1.e-20_r_kind) then
      stpinout=-999._r_kind
      if(mype == 0)then
        if(ii == 1)write(iout_iter,100) (pen(i),i=1,ipen)
        write(iout_iter,130) ii,bx,cx,stp(ii)
        write(iout_iter,101) (stpx(i),i=1,ipen)
        write(iout_iter,105) (bpen(i),i=1,ipen)
        write(iout_iter,110) (cpen(i),i=1,ipen)
      end if
      end_iter = .true.
      return
    end if

    if(ii == 1 .and. mype == 0) then
      write(iout_iter,100) (pen(i),i=1,ipen)
      write(iout_iter,101) (stpx(i),i=1,ipen)
!     write(iout_iter,105) (bpen(i),i=1,ipen)
!     write(iout_iter,110) (cpen(i),i=1,ipen)
      limq_warning=.true.
      do i=1,ipen
         if (pen(i)>pen(18)) limq_warning=.false.
      end do
      if (limq_warning) write(iout_iter,120) factqmin,factqmax
100   format(' J=',3e25.18/,(3x,3e25.18))
101   format(' S=',3e25.18/,(3x,3e25.18))
105   format(' b=',3e25.18/,(3x,3e25.18))
110   format(' c=',3e25.18/,(3x,3e25.18))
120   format('***WARNING***  large q<0 penalty, consider reducing factqmin = ',&
          g12.6,' and/or factqmax = ',g12.6)
130   format('***WARNING***  negative or small cx inner', &
             ' iteration terminated - probable error',i2,3e25.18)
140   format('***WARNING***  expected penalty reduction small ',/,  &
             ' inner iteration terminated - probable convergence',i2,4e25.18)
    endif

  end do

  if(mype == 0)then
    write(iout_iter,200) (stp(i),i=0,istp_iter)
  end if
200 format(' stepsize estimates = ',5(e24.18,1x))

  stpinout=stp(istp_iter)

  return
end subroutine stpcalc


subroutine stpcalc_tl(stpinout,xhat,xhatuv,xhat_x,xhat_y,&
     dirx,dirxuv,dirx_x,dirx_y,diry,penalty,mype, &
     stpinout_tl,xhat_tl,xhatuv_tl,xhat_x_tl,xhat_y_tl,&
     dirx_tl,dirxuv_tl,dirx_x_tl,dirx_y_tl,diry_tl,penalty_tl,end_iter)

!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    stpcalc_tl  the tangent linear of the operator that calculates 
!                           penalty and stepsize
!   prgmmr: yanqiu zhu           org: GMAO                date: 2005-05-21
!
! abstract: the tangent linear of the operator that calculates current penalty 
!           and estimate stepsize (nonlinear qc version)
!
!    A description of nonlinear qc follows:
!
!    The observation penalty Jo is defined as
!
!          Jo =  - (sum over obs) 2*log(Po)
!
!      where,
!
!          Po = Wnotgross*exp(-.5*(Hn(x+xb) - yo)**2 ) + Wgross
!            with
!                Hn = the forward model (possibly non-linear) normalized by 
!                     observation error
!                x  = the current estimate of the analysis increment
!                xb = the background state
!                yo = the observation normalized by observation error
!
!            Note:  The factor 2 in definition of Jo is present because the 
!                   penalty Jo as used in this code is 2*(usual definition 
!                   of penalty)
!
!          Wgross = Pgross*cg
!
!          Wnotgross = 1 - Wgross
!
!          Pgross = probability of gross error for observation (assumed
!                   here to have uniform distribution over the possible
!                   range of values)
!
!          cg = sqrt(2*pi)/2b
!
!          b = possible range of variable for gross errors, normalized by 
!              observation error
!
!    The values for the above parameters that Bill Collins used in the
!    eta 3dvar are:
!
!          cg = cg_term/b, where cg_term = sqrt(2*pi)/2 
!
!          b = 10.        ! range for gross errors, normalized by obs error
!
!          pg_q=.002      ! probability of gross error for specific humidity
!          pg_pw=.002     ! probability of gross error for precipitable water
!          pg_p=.002      ! probability of gross error for pressure
!          pg_w=.005      ! probability of gross error for wind
!          pg_t=.007      ! probability of gross error for temperature
!          pg_rad=.002    ! probability of gross error for radiances
!
!
!    Given the above Jo, the gradient of Jo is as follows:
!
!                                             T
!        gradx(Jo) = - (sum over observations) 2*H (Hn(x+xb)-yo)*(Po - Wgross)/Po
!
!      where, 
!
!          H = tangent linear model of Hn about x+xb
!
! 
!    Note that if Pgross = 0.0, then Wnotgross=1.0 and Wgross=0.0.  That is,
!    the code runs as though nonlinear quality control were not present
!    (which is indeed the case since the gross error probability is 0).  
!
!    As a result the same stp* routines may be used for use with or without
!    nonlinear quality control.
!    
!    Please note, however, that using the nonlinear qc routines makes the
!    stp* and int* operators nonlinear.  Hence, the need to evaluate the
!    step size operators twice for each observation type, give the current
!    step size algorithm coded below. 
!
!
! program history log:
!   2005-05-21  yanqiu zhu - tangent linear of stpcalc
!   2005-05-27  derber - remove linear stepsize estimate
!   2005-06-03  parrish - add horizontal derivatives
!   2005-12-01  cucurull - add code for GPS local bending angle, add use obsmod for ref_obs
!   2006-04-18  derber - add explicit iteration over stepsize (rather than 
!                        repeated calls) - clean up and simplify
!   2006-05-26  derber - modify to improve convergence checking
!
!   input argument list:
!     stp2     - guess stepsize
!     xhat     - current solution
!     xhatuv   - current solution for u,v
!     xhat_x   - current solution x derivative
!     xhat_y   - current solution y derivative
!     dirx     - search direction for x
!     dirx_x   - x derivative of search direction for x
!     dirx_y   - y derivative of serach direction for x
!     dirxuv   - search direction for u,v
!     diry     - search direction for y (B-1 dirx)
!     stp2_tl     - tangent linear guess stepsize
!     xhat_tl     - current tangent linear solution
!     xhatuv_tl   - current tangent linear solution for u,v
!     xhat_x_tl   - current tangent linear solution x derivative
!     xhat_y_tl   - current tangent linear solution y derivative
!     dirx_tl     - tangent linear search direction for x
!     dirxuv_tl   - tangent linear search direction for u,v
!     dirx_x_tl   - tangent linear x derivative of search direction for x
!     dirx_y_tl   - tangent linear y derivative of search direction for x
!     diry_tl     - tangent linear search direction for y (B-1 dirx)
!     mype     - pe number
!     end_iter - end iteration flag
!
!   output argument list:
!     stp2     - second estimate of stepsize
!     penalty  - penalty
!     stp2_tl     - tangent linear of second estimate of stepsize
!     penalty_tl  - tangent linear of penalty
!     end_iter - end iteration flag false if stepsize successful
!
!
! remarks:
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,r_double,i_kind
  use mpimod, only: ierror,mpi_comm_world,mpi_rtype,mpi_sum,levs_id,npe
  use constants, only:  zero,one_tenth,quarter,half,one,two
  use jfunc, only: noz,nq,nt,nsst,ncw,np,iout_iter,nst,nvp,&
       nclen,nclen1,nclen2,nsclen,npclen,xhatsave,yhatsave,factqmin,factqmax,&
       nuvlen,nu,nv,iter,switch_on_derivatives
  use jfunc_tl, only: xhatsave_tl,yhatsave_tl
  use obsmod, only: ref_obs
  use gridmod, only: latlon1n,latlon11,lat2,lon2,nsig,nsig1o
  use radinfo, only: npred,jpch
  use pcpinfo, only: jtype,npredp
  use stptmod
  use stpwmod
  use stppsmod
  use stppwmod
  use stpqmod
  use stpradmod
  use stprefmod
  use stpbendmod
  use stprwmod
  use stpspdmod
  use stpsrwmod
  use stpsstmod
  use stpdwmod
  use stppcpmod
  use stpozmod
  use stplimqmod
  use dprodxmod
  implicit none

! Declare passed variables
  integer(i_kind),intent(in):: mype
  real(r_kind),intent(inout):: stpinout,stpinout_tl
  real(r_kind),intent(out):: penalty
  logical,intent(inout):: end_iter
  real(r_kind),intent(out):: penalty_tl
  real(r_kind),dimension(nuvlen),intent(out):: dirxuv
  real(r_kind),dimension(nuvlen),intent(out):: dirxuv_tl
  real(r_kind),dimension(nclen),intent(in)::dirx,xhat,diry
  real(r_kind),dimension(nclen),intent(in)::dirx_tl,xhat_tl,diry_tl
  real(r_kind),dimension(nuvlen),intent(in):: xhatuv
  real(r_kind),dimension(nuvlen),intent(in):: xhatuv_tl
  real(r_kind),dimension(nclen),intent(in)::dirx_x,dirx_y,xhat_x,xhat_y
  real(r_kind),dimension(nclen),intent(in)::dirx_x_tl,dirx_y_tl,xhat_x_tl,xhat_y_tl

! Declare local parameters
  integer(i_kind),parameter:: ipen = 20
  integer(i_kind),parameter:: ipenl = 4
  integer(i_kind),parameter:: ipen_tot = ipen*3
  integer(i_kind),parameter:: istp_iter = 2

! Declare local variables
  logical limq_warning
  integer(i_kind) i,j,k,nnn,ii,mm1
  real(r_kind) dels,sges1,sges2,sges3,delpen
  real(r_kind) sges1_tl,sges2_tl,sges3_tl
  real(r_kind) alpha,b1,b3,b13,bx,cx,bsum,csum,bb1,cc1
  real(r_kind) alpha_tl,b1_tl,b3_tl,b13_tl,bx_tl,cx_tl,bsum_tl,csum_tl,bb1_tl,cc1_tl
  real(r_kind),dimension(ipen):: pen,bpen,cpen,pen_tl,bpen_tl,cpen_tl
  real(r_kind),dimension(ipen,3,npe):: pbc_tl,pbc,pbc1_tl,pbc1
  real(r_kind),dimension(0:istp_iter):: stp,stp_tl 
  real(r_double) dplevtmp
  real(r_double) dplevtmp_tl
  real(r_kind) dirx_q(latlon1n)
  real(r_kind) dirx_q_tl(latlon1n)
  real(r_kind) xhat_q(latlon1n)
  real(r_kind) xhat_q_tl(latlon1n)

!************************************************************************************  
! For nonlinear terms use dels to increment stepsize
  mm1=mype+1
  stp(0)=stpinout
  stp_tl(0)=stpinout_tl


!   Begin calculating contributions to penalty and stepsize for various terms
!
!    linear terms
!    pbc(*,1,*) - penalty
!    pbc(*,2,*) - b
!    pbc(*,3,*) - c
!
!    pbc(1,*,*)  contribution from background
!    pbc(2,*,*)  contribution from bias coeff. background term
!    pbc(3,*,*)  contribution from precip. bias correction background term
!    pbc(4,*,*)  contribution from dynamic constraint term (Jc)
!
!  nonlinear terms
!    pbc(:,1,*) penalty
!    pbc(:,2,*) b1 penalty(sges-dels)-penalty(sges)
!    pbc(:,3,*) b2 penalty(sges+dels)-penalty(sges)
!
!    pbc(5,*,*)  contribution from wind observation term
!    pbc(6,*,*)  contribution from satellite radiance observation term
!    pbc(7,*,*)  contribution from temperature observation term
!    pbc(8,*,*)  contribution from precipitable water obs. term
!    pbc(9,*,*)  contribution from specific humidity obs.term
!    pbc(10,*,*) contribution from ozone obs. term
!    pbc(11,*,*) contribution from doppler lidar wind
!    pbc(12,*,*) contribution from doppler radar wind
!    pbc(13,*,*) contribution from radar superob wind
!    pbc(14,*,*) contribution from GPS local observations
!    pbc(15,*,*) contribution from conventional sst
!    pbc(16,*,*) contribution from wind speed obs. term
!    pbc(17,*,*) contribution from precipitation term
!    pbc(18,*,*) contribution from negative moisture constraint term
!    pbc(19,*,*) contribution from excess moisture term
!    pbc(20,*,*) contribution from surface pressure observation term
!

  pbc=zero
  pbc_tl=zero
! penalty, b, c for background terms

! pbc(1,1,mm1)= dplev(xhatsave,yhatsave)
! pbc(1,2,mm1)=-dplev(dirx,yhatsave)
! pbc(1,3,mm1)= dplev(dirx,diry)
  call dplev_tl(xhatsave,yhatsave,dplevtmp,xhatsave_tl,yhatsave_tl,dplevtmp_tl)
  pbc(1,1,mm1)   = dplevtmp
  pbc_tl(1,1,mm1) = dplevtmp_tl
  call dplev_tl(dirx,yhatsave,dplevtmp,dirx_tl,yhatsave_tl,dplevtmp_tl)
  pbc(1,2,mm1)   = -dplevtmp
  pbc_tl(1,2,mm1) = -dplevtmp_tl
  call dplev_tl(dirx,diry,dplevtmp,dirx_tl,diry_tl,dplevtmp_tl)
  pbc(1,3,mm1)   = dplevtmp
  pbc_tl(1,3,mm1) = dplevtmp_tl

! bias correction terms

  if(mype == 0)then

! penalty, b, c terms for satellite bias correction

   do i=nclen1+1,nclen2
      pbc(2,1,mm1)=pbc(2,1,mm1)+xhatsave(i)*yhatsave(i)
      pbc(2,2,mm1)=pbc(2,2,mm1)-dirx(i)*yhatsave(i)
      pbc(2,3,mm1)=pbc(2,3,mm1)+dirx(i)*diry(i)

      pbc_tl(2,1,mm1)=pbc_tl(2,1,mm1)+xhatsave_tl(i)*  yhatsave(i)+ &
                                      xhatsave(i)*yhatsave_tl(i)
      pbc_tl(2,2,mm1)=pbc_tl(2,2,mm1)-dirx_tl(i)*  yhatsave(i)-     &
                                      dirx(i)*yhatsave_tl(i)
      pbc_tl(2,3,mm1)=pbc_tl(2,3,mm1)+dirx_tl(i)*  diry(i)+         &
                                       dirx(i)*diry_tl(i)
   end do

! penalty, b, c terms for precipitation bias correction

   do i=nclen2+1,nclen
      pbc(3,1,mm1)=pbc(3,1,mm1)+xhatsave(i)*yhatsave(i)
      pbc(3,2,mm1)=pbc(3,2,mm1)-dirx(i)*yhatsave(i)
      pbc(3,3,mm1)=pbc(3,3,mm1)+dirx(i)*diry(i)

      pbc_tl(3,1,mm1)=pbc_tl(3,1,mm1)+xhatsave_tl(i)*  yhatsave(i)+ &
                                      xhatsave(i)*yhatsave_tl(i)
      pbc_tl(3,2,mm1)=pbc_tl(3,2,mm1)-dirx_tl(i)*  yhatsave(i)-     &
                                      dirx(i)*yhatsave_tl(i)
      pbc_tl(3,3,mm1)=pbc_tl(3,3,mm1)+dirx_tl(i)*  diry(i)+         &
                                      dirx(i)*diry_tl(i)
   end do
  end if


! Convert search direction for st/vp to u/v for stp routines
  call getuv(dirxuv(nu),dirxuv(nv),dirx(nst),dirx(nvp))

  if(switch_on_derivatives) then
! compute derivatives
! Determine how many vertical levels each mpi task will
! handle in computing horizontal derivatives

    do k=1,nsig1o
      if (levs_id(k)/=0) nnn=nnn+1
    end do
    call get_derivatives( &
       dirxuv(nu) ,dirxuv(nv) ,dirx  (nt)  ,dirx  (np),  &
       dirx  (nq) ,dirx  (noz),dirx  (nsst),dirx  (ncw), &
       dirx_x(nst),dirx_x(nvp),dirx_x(nt)  ,dirx_x(np),  &
       dirx_x(nq) ,dirx_x(noz),dirx_x(nsst),dirx_x(ncw), &
       dirx_y(nst),dirx_y(nvp),dirx_y(nt)  ,dirx_y(np),  &
       dirx_y(nq) ,dirx_y(noz),dirx_y(nsst),dirx_y(ncw), &
       nnn,mype,1)

    call get_derivatives( &
       dirxuv_tl(nu) ,dirxuv_tl(nv) ,dirx_tl  (nt)  ,dirx_tl  (np),  &
       dirx_tl  (nq) ,dirx_tl  (noz),dirx_tl  (nsst),dirx_tl  (ncw), &
       dirx_x_tl(nst),dirx_x_tl(nvp),dirx_x_tl(nt)  ,dirx_x_tl(np),  &
       dirx_x_tl(nq) ,dirx_x_tl(noz),dirx_x_tl(nsst),dirx_x_tl(ncw), &
       dirx_y_tl(nst),dirx_y_tl(nvp),dirx_y_tl(nt)  ,dirx_y_tl(np),  &
       dirx_y_tl(nq) ,dirx_y_tl(noz),dirx_y_tl(nsst),dirx_y_tl(ncw), &
       nnn,mype,1)

  end if


!   convert normalized rh variable to q
  call normal_rh_to_q(dirx(nq),dirx(nt),dirx(np),dirx_q)
  call normal_rh_to_q(xhat(nq),xhat(nt),xhat(np),xhat_q)

  call normal_rh_to_q(dirx_tl(nq),dirx_tl(nt),dirx_tl(np),dirx_q_tl)
  call normal_rh_to_q(xhat_tl(nq),xhat_tl(nt),xhat_tl(np),xhat_q_tl)


! tangent linear of stepsize for JC term is missing

! iterate over number of stepsize iterations (istp_iter - currently set to 2
  do ii=1,istp_iter

    dels=one_tenth ** ii
!   Do nonlinear terms
  
    sges1=(one-dels)*stp(ii-1)
    sges2=stp(ii-1)
    sges3=(one+dels)*stp(ii-1)

    sges1_tl=(one-dels)*stp_tl(ii-1)
    sges2_tl=stp_tl(ii-1)
    sges3_tl=(one+dels)*stp_tl(ii-1)


!   stepsize and background for winds
    call stpw_tl(dirxuv(nu),dirxuv(nv),xhatuv(nu),xhatuv(nv), &
              pbc(5,1,mm1),pbc(5,2,mm1),pbc(5,3,mm1),sges1,sges2,sges3, &
              dirxuv_tl(nu),dirxuv_tl(nv),xhatuv_tl(nu),xhatuv_tl(nv), &
              pbc_tl(5,1,mm1),pbc_tl(5,2,mm1),pbc_tl(5,3,mm1),         &
              sges1_tl,sges2_tl,sges3_tl)

!   stepsize and background for radiances
    call stprad_tl(dirx(nt),dirx_q,dirx(noz),dirxuv(nu),dirxuv(nv),dirx(nsst), &
                xhat(nt),xhat_q,xhat(noz),xhatuv(nu),xhatuv(nv),xhat(nsst), &
                dirx(nclen1+1),xhat(nclen1+1),&
                pbc(6,1,mm1),pbc(6,2,mm1),pbc(6,3,mm1),sges1,sges2,sges3, &
                dirx_tl(nt),dirx_q_tl,dirx_tl(noz), &
                dirxuv_tl(nu),dirxuv_tl(nv),dirx_tl(nsst), &
                xhat_tl(nt),xhat_q_tl,xhat_tl(noz), &
                xhatuv_tl(nu),xhatuv_tl(nv),xhat_tl(nsst), &
                dirx_tl(nclen1+1),xhat_tl(nclen1+1),&
                pbc_tl(6,1,mm1),pbc_tl(6,2,mm1),pbc_tl(6,3,mm1),     &
                sges1_tl,sges2_tl,sges3_tl)

!   stepsize and background for temperature
    call stpt_tl(dirx(nt),xhat(nt), &
              pbc(7,1,mm1),pbc(7,2,mm1),pbc(7,3,mm1),sges1,sges2,sges3, &
              dirx_tl(nt),xhat_tl(nt), &
              pbc_tl(7,1,mm1),pbc_tl(7,2,mm1),pbc_tl(7,3,mm1), &
              sges1_tl,sges2_tl,sges3_tl)

!   stepsize and background for precipitable water
    call stppw_tl(dirx_q,xhat_q, &
               pbc(8,1,mm1),pbc(8,2,mm1),pbc(8,3,mm1),sges1,sges2,sges3, &
               dirx_q_tl,xhat_q_tl, &
               pbc_tl(8,1,mm1),pbc_tl(8,2,mm1),pbc_tl(8,3,mm1), &
               sges1_tl,sges2_tl,sges3_tl)

!   stepsize and background for moisture
    call stpq_tl(dirx_q,xhat_q, &
              pbc(9,1,mm1),pbc(9,2,mm1),pbc(9,3,mm1),sges1,sges2,sges3, &
              dirx_q_tl,xhat_q_tl, &
              pbc_tl(9,1,mm1),pbc_tl(9,2,mm1),pbc_tl(9,3,mm1), &
              sges1_tl,sges2_tl,sges3_tl)
  
!   stepsize and background for ozone
    call stpoz_tl(dirx(noz),xhat(noz), &
               pbc(10,1,mm1),pbc(10,2,mm1),pbc(10,3,mm1),sges1,sges2,sges3, &
               dirx_tl(noz),xhat_tl(noz),  &
               pbc_tl(10,1,mm1),pbc_tl(10,2,mm1),pbc_tl(10,3,mm1), &
               sges1_tl,sges2_tl,sges3_tl)
  
!   stepsize and background for wind lidar
    call stpdw_tl(dirxuv(nu),dirxuv(nv),xhatuv(nu),xhatuv(nv), &
               pbc(11,1,mm1),pbc(11,2,mm1),pbc(11,3,mm1),sges1,sges2,sges3, &
               dirxuv_tl(nu),dirxuv_tl(nv),xhatuv_tl(nu),xhatuv_tl(nv), &
               pbc_tl(11,1,mm1),pbc_tl(11,2,mm1),pbc_tl(11,3,mm1), &
               sges1_tl,sges2_tl,sges3_tl)

!   stepsize and background for radar
    call stprw_tl(dirxuv(nu),dirxuv(nv),xhatuv(nu),xhatuv(nv), &
               pbc(12,1,mm1),pbc(12,2,mm1),pbc(12,3,mm1),sges1,sges2,sges3, &
               dirxuv_tl(nu),dirxuv_tl(nv),xhatuv_tl(nu),xhatuv_tl(nv), &
               pbc_tl(12,1,mm1),pbc_tl(12,2,mm1),pbc_tl(12,3,mm1), &
               sges1_tl,sges2_tl,sges3_tl )

!   stepsize and background for radar superob wind
    call stpsrw_tl(dirxuv(nu),dirxuv(nv),xhatuv(nu),xhatuv(nv), &
                pbc(13,1,mm1),pbc(13,2,mm1),pbc(13,3,mm1),sges1,sges2,sges3, &
                dirxuv_tl(nu),dirxuv_tl(nv),xhatuv_tl(nu),xhatuv_tl(nv), &
                pbc_tl(13,1,mm1),pbc_tl(13,2,mm1),pbc_tl(13,3,mm1), &
                sges1_tl,sges2_tl,sges3_tl)

!   stepsize and background for GPS local observation
    if(ref_obs) then
       call stpref_tl(dirx(nt),dirx_q,dirx(np),xhat(nt),xhat_q,xhat(np),&
            pbc(14,1,mm1),pbc(14,2,mm1),pbc(14,3,mm1),sges1,sges2,sges3,&
            dirx_tl(nt),dirx_q_tl,dirx_tl(np),xhat_tl(nt),xhat_q_tl,xhat_tl(np),&
            pbc_tl(14,1,mm1),pbc_tl(14,2,mm1),pbc_tl(14,3,mm1), &
            sges1_tl,sges2_tl,sges3_tl)
    else
       call stpbend_tl(dirx(nt),dirx_q,dirx(np),xhat(nt),xhat_q,xhat(np),&
            pbc(14,1,mm1),pbc(14,2,mm1),pbc(14,3,mm1),sges1,sges2,sges3,&
            dirx_tl(nt),dirx_q_tl,dirx_tl(np),xhat_tl(nt),xhat_q_tl,xhat_tl(np),&
            pbc_tl(14,1,mm1),pbc_tl(14,2,mm1),pbc_tl(14,3,mm1), &
            sges1_tl,sges2_tl,sges3_tl)
    endif

!   stepsize and background for conventional sst
    call stpsst_tl(dirx(nsst),xhat(nsst),&
                pbc(15,1,mm1),pbc(15,2,mm1),pbc(15,3,mm1),sges1,sges2,sges3, &
                dirx_tl(nsst),xhat_tl(nsst),&
                pbc_tl(15,1,mm1),pbc_tl(15,2,mm1),pbc_tl(15,3,mm1), &
                sges1_tl,sges2_tl,sges3_tl)
  
!   stepsize and background for wind speed
    call stpspd_tl(dirxuv(nu),dirxuv(nv),xhatuv(nu),xhatuv(nv), &
                pbc(16,1,mm1),pbc(16,2,mm1),pbc(16,3,mm1),sges1,sges2,sges3, &
                dirxuv_tl(nu),dirxuv_tl(nv),xhatuv_tl(nu),xhatuv_tl(nv), &
                pbc_tl(16,1,mm1),pbc_tl(16,2,mm1),pbc_tl(16,3,mm1), &
                sges1_tl,sges2_tl,sges3_tl)

!   stepsize and background for precipitation
    call stppcp_tl(dirx(nt),dirx_q,dirxuv(nu),dirxuv(nv),dirx(ncw), &
                xhat(nt),xhat_q,xhatuv(nu),xhatuv(nv),xhat(ncw), &
                dirx(nclen2+1),xhat(nclen2+1),                 &
                pbc(17,1,mm1),pbc(17,2,mm1),pbc(17,3,mm1),sges1,sges2,sges3, &
                dirx_tl(nt),dirx_q_tl,dirxuv_tl(nu),dirxuv_tl(nv),dirx_tl(ncw), &
                xhat_tl(nt),xhat_q_tl,xhatuv_tl(nu),xhatuv_tl(nv),xhat_tl(ncw), &
                dirx_tl(nclen2+1),xhat_tl(nclen2+1),                 &
                pbc_tl(17,1,mm1),pbc_tl(17,2,mm1),pbc_tl(17,3,mm1), &
                sges1_tl,sges2_tl,sges3_tl)

!   stepsize and background for moisture constraint
    call stplimq_tl(dirx_q,xhat_q,sges1,sges2,sges3,&
                 pbc(18,1,mm1),pbc(18,2,mm1),pbc(18,3,mm1),&
                 pbc(19,1,mm1),pbc(19,2,mm1),pbc(19,3,mm1), &
                 dirx_q_tl,xhat_q_tl,sges1_tl,sges2_tl,sges3_tl,&
                 pbc_tl(18,1,mm1),pbc_tl(18,2,mm1),pbc_tl(18,3,mm1),&
                 pbc_tl(19,1,mm1),pbc_tl(19,2,mm1),pbc_tl(19,3,mm1))
  
!   stepsize and background for surface pressure
    call stpps_tl(dirx(np),xhat(np),                                     &
               pbc(20,1,mm1),pbc(20,2,mm1),pbc(20,3,mm1),sges1,sges2,sges3, &
               dirx_tl(np),xhat_tl(np),                                 &
               pbc_tl(20,1,mm1),pbc_tl(20,2,mm1),pbc_tl(20,3,mm1),         &
               sges1_tl,sges2_tl,sges3_tl)
  
!   Reduce output over all processors
      call mpi_allreduce(pbc,pbc1,ipen_tot*npe,mpi_rtype,mpi_sum, &
                 mpi_comm_world,ierror)
      call mpi_allreduce(pbc_tl,pbc1_tl,ipen_tot*npe,mpi_rtype,mpi_sum, &
                 mpi_comm_world,ierror)
  
!   estimate of stepsize

    pen=zero
    bpen=zero
    cpen=zero
    pen_tl=zero
    bpen_tl=zero
    cpen_tl=zero
    do j=1,npe
     do i=1,ipen
       pen(i) = pen(i)+pbc1(i,1,j)
       bpen(i)=bpen(i)+pbc1(i,2,j)
       cpen(i)=cpen(i)+pbc1(i,3,j)
     end do
    end do
    if(ii == 1)then
      penalty=zero
      penalty_tl=zero
      do i=1,ipen
         penalty=penalty+pen(i)
         penalty_tl=penalty_tl+pen_tl(i)
      end do
      bsum=zero
      csum=zero
      bsum_tl=zero
      csum_tl=zero
      do i=1,ipenl
        bsum=bsum+bpen(i)
        csum=csum+cpen(i)
        bsum_tl=bsum_tl+bpen_tl(i)
        csum_tl=csum_tl+cpen_tl(i)
      end do
    end if


    b1=zero
    b3=zero
    b1_tl=zero
    b3_tl=zero
    do i=ipenl+1,ipen
      b1=b1+bpen(i)
      b3=b3+cpen(i)
      b1_tl=b1_tl+bpen_tl(i)
      b3_tl=b3_tl+cpen_tl(i)
    end do
    alpha= one/(dels*sges2)
    b13  = b1+b3
    cc1  = half*b13*alpha*alpha
    bb1  = half*alpha*(alpha*sges2*b13 + half*(b1-b3))


    alpha_tl=-one/(dels*sges2*sges2) * sges2_tl
    b13_tl  = b1_tl+b3_tl
    cc1_tl  = half*b13_tl*alpha*alpha + b13*alpha*alpha_tl
    bb1_tl  = sges2_tl*cc1+sges2*cc1_tl +  &
           half*half*(alpha_tl*(b1-b3)+alpha*(b1_tl-b3_tl)) 
         

!   estimate of stepsize

    bx=bsum+bb1
    cx=csum+cc1
    bx_tl=bsum_tl+bb1_tl
    cx_tl=csum_tl+cc1_tl

    stp(ii)=stp(ii-1)
    if(cx > 1.e-20_r_kind) stp(ii)=min(bx/cx,one)                         ! step size estimate
    stp_tl(ii)=stp_tl(ii-1)
    if(cx > 1.e-20_r_kind) stp_tl(ii)=bx_tl/cx-bx/(cx*cx)*cx_tl

    delpen = two*stp(ii)*bx - stp(ii)**2*cx  

    if(delpen/penalty < 1.e-17_r_kind) then
      stpinout=stp(ii)
      stpinout_tl=stp_tl(ii)
      if(mype == 0)then
        if(ii == 1)write(iout_iter,100) (pen(i),i=1,ipen)
        write(iout_iter,140) ii,delpen,bx,cx,stp(ii)
        write(iout_iter,105) (bpen(i),i=1,ipen),bsum,bb1
        write(iout_iter,110) (cpen(i),i=1,ipen),csum,cc1
      end if
      end_iter = .true.
      return
    end if

    if(cx < 1.e-20_r_kind) then
      stpinout=-999._r_kind
      stpinout_tl=-999._r_kind
      if(mype == 0)then
        if(ii == 1)write(iout_iter,100) (pen(i),i=1,ipen)
        write(iout_iter,130) ii,bx,cx,stp(ii)
        write(iout_iter,105) (bpen(i),i=1,ipen),bsum,bb1
        write(iout_iter,110) (cpen(i),i=1,ipen),csum,cc1
      end if
      end_iter = .true.
      return
    end if

    if(ii == 1 .and. mype == 0) then
      write(iout_iter,100) (pen(i),i=1,ipen)
!     write(iout_iter,105) (bpen(i),i=1,ipen),bsum,bb1
!     write(iout_iter,110) (cpen(i),i=1,ipen),csum,cc1
      limq_warning=.true.
      do i=1,ipen
         if (pen(i)>pen(18)) limq_warning=.false.
      end do
      if (limq_warning) write(iout_iter,120) factqmin,factqmax
100   format(' J=',3e25.18/,(3x,3e25.18))
105   format(' b','=',3e25.18/,(3x,3e25.18))
110   format(' c','=',3e25.18/,(3x,3e25.18))
120   format('***WARNING***  large q<0 penalty, consider reducing factqmin = ',&
          g12.6,' and/or factqmax = ',g12.6)
130   format('***WARNING***  negative or small cx inner', &
             ' iteration terminated - probable error',i2,3e25.18)
140   format('***WARNING***  expected penalty reduction small ',/,  &
             ' inner iteration terminated - probable convergence',i2,4e25.18)
    endif

  end do
  if(mype == 0)then
    write(iout_iter,200) (stp(i),i=0,istp_iter)
    write(iout_iter,201) (stp_tl(i),i=0,istp_iter)
  end if
200 format(' stepsize estimates = ',5(e24.18,1x))
201 format(' tl stepsize estimates = ',5(e24.18,1x))
  stpinout=stp(istp_iter)
  stpinout_tl=stp_tl(istp_iter)

  return
end subroutine stpcalc_tl

end module stpcalcmod
