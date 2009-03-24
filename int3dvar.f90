subroutine int3dvar(rval,rval_dt)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    intall      calculate RHS for analysis equation
!   prgmmr: derber           org: np23                date: 2003-12-18
!
! abstract: complementary components pertinent to the RHS of 3dvar 
!
! program history log:
!   2003-12-18  derber
!   2004-07-23  derber  - modify to include conventional sst
!   2004-07-28  treadon - add only to module use, add intent in/out
!   2004-10-06  parrish - add nonlinear qc option
!   2004-10-06  kleist  - separate control vector for u,v, & convert int
!                         for wind components into int for st,vp
!   2004-11-30  treadon - add brightness temperatures to nonlinear 
!                         quality control
!   2004-12-03  treadon - replace mpe_iallreduce (IBM extension) with
!                         standard mpi_allreduce
!   2005-01-20  okamoto - add u,v to intrad
!   2005-02-23  wu      - changes related to normalized rh option
!   2005-04-11  treadon - rename intall_qc as intall
!   2005-05-18  yanqiu zhu - add 'use int*mod',and modify call interfaces for using these modules
!   2005-05-24  pondeca - take into consideration that npred=npredp=0
!                         for 2dvar only surface analysis option
!   2005-06-03  parrish - add horizontal derivatives
!   2005-07-10  kleist  - add dynamic constraint term
!   2005-09-29  kleist  - expand Jc term, include time derivatives vector
!   2005-11-21  kleist  - separate tendencies from Jc term, add call to calctends adjoint
!   2005-12-01  cucurull - add code for GPS local bending angle, add use obsmod for ref_obs
!   2005-12-20  parrish - add arguments to call to intt to allow for option of using boundary
!                         layer forward tlm.
!   2006-02-03  derber  - modify to increase reproducibility
!   2006-03-17  park    - correct error in call to intt--rval,sval --> rvaluv,svaluv
!                          in order to correctly pass wind variables.
!   2006-04-06  kleist  - include both Jc formulations
!   2006-07-26  parrish - correct inconsistency in computation of space and time derivatives of q
!                          currently, if derivatives computed, for q it is normalized q, but
!                          should be mixing ratio.
!   2006-07-26  parrish - add strong constraint initialization option
!   2006-09-20  derber  - add sensible temperatures for conventional temperatures
!   2006-10-12  treadon - replace virtual temperature with sensible in intpcp
!   2007-03-13  derber  - modify call to strong_bal_correction remove u_t_g, etc. arrays
!   2007-03-13  derber  - fix bug when nstrong > 1
!   2007-02-15  rancic  - add foto
!   2007-03-19  tremolet - binning of observations
!   2007-04-13  tremolet - split 3dvar specific components from intall
!   2007-04-16  kleist  - modified calls to tendency and constraint routines
!   2007-07-26 cucurull - update gps code to generalized vertical coordinate;
!                         get current solution for 3d pressure (sval3dp);
!                         move getprs_ad out of calctends_ad; add pri and 
!                         remove ps in calctends_ad argument list;
!                         add rval3dp; use getprs_ad
!   2007-08-08  derber - optimize - ensure that only necessary time derivatives are calculated
!   2008-06-02  safford - rm unused vars
!   2008-10-08  derber - move strong balance constraint to background error covariance
!   2008-12-01  todling - merge with 4dvar code (split of intall)
!
!   input argument list:
!
!   output argument list:      
!     rval     - RHS on grid
!
!
! remarks:
!       are all grid fields after strong initialization.
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,i_kind
  use mpimod, only: ierror,mpi_comm_world,mpi_sum,mpi_rtype,levs_id,npe,mype
  use jfunc, only: nclen,nclen1,nclen2,nrclen,nsclen,&
       npclen,ncw,np,nt,nsst,noz,nq,nst,nvp,nu,nv,nuvlen,&
       ntendlen,nut,nvt,ntt,nprst,nqt,nozt,ncwt,ndivt,nagvt,l_foto
  use constants, only: zero
  use gridmod, only: latlon1n,latlon11,lat2,lon2,nsig,nsig1o
  use state_vectors
  implicit none
  
! Declare passed variables  
  type(state_vector),intent(inout):: rval
  type(state_vector),intent(inout):: rval_dt

! Declare local variables  	
  logical:: tracer
  integer(i_kind) i,k,nnn
  real(r_kind),dimension(nclen):: rval_x,rval_y
  integer(i_kind) istrong

!******************************************************************************

 if(l_foto )then

!   Adjoint of virtual to sensible temperature conversion
    call tv_to_tsen_ad(rval_dt%t,rval_dt%q,rval_dt%tsen)
    nnn=0
    do k=1,nsig1o
       if (levs_id(k)/=0) nnn=nnn+1
    end do
    do i=1,nclen
       rval_x(i)=zero
       rval_y(i)=zero
    end do
    tracer=.true.

    call calctends_ad(rval%u,rval%v,rval%t,                               &
                      rval%q,rval%oz,rval%cw,                             &
                      rval_x(nst),rval_y(nst),rval_x(nvp),rval_y(nvp),    &
                      rval_x(nt), rval_y(nt), rval_x(np), rval_y(np),     &
                      rval_x(nq), rval_y(nq), rval_x(noz),rval_y(noz),    &
                      rval_x(ncw),rval_y(ncw),mype,                       &
                      rval_dt%u,rval_dt%v,rval_dt%t,rval_dt%p3d,  &
                      rval_dt%q,rval_dt%oz,rval_dt%cw,rval%p3d,tracer)  

! add contributions from derivatives
    call tget_derivatives( &
         rval%u     ,rval%v     ,rval%t      ,rval%p    ,  &
         rval%q     ,rval%oz    ,rval%sst    ,rval%cw   ,  &
         rval_x(nst),rval_x(nvp),rval_x(nt)  ,rval_x(np),  &
         rval_x(nq) ,rval_x(noz),rval_x(nsst),rval_x(ncw), &
         rval_y(nst),rval_y(nvp),rval_y(nt)  ,rval_y(np),  &
         rval_y(nq) ,rval_y(noz),rval_y(nsst),rval_y(ncw),nnn,mype)
  end if

  return
  end subroutine int3dvar
