module stpradmod

!$$$ module documentation block
!           .      .    .                                       .
! module:   stpradmod    module for stprad and its tangent linear stprad_tl
!  prgmmr:
!
! abstract: module for stprad and its tangent linear stprad_tl
!
! program history log:
!   2005-05-20  Yanqiu zhu - wrap stprad and its tangent linear stprad_tl into one module
!   2005-11-16  Derber - remove interfaces
!   2008-12-02  Todling - remove stprad_tl
!   2009-08-12  lueken - update documentation
!
! subroutines included:
!   sub stprad
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

implicit none

PRIVATE
PUBLIC stprad

contains

subroutine stprad(radhead,rt,rq,roz,ru,rv,rst,st,sq,soz,su,sv,sst, &
     rpred,spred,out,sges,nstep)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    stprad compute contribution to penalty and stepsize
!                from rad, using nonlinear qc.
!   prgmmr: parrish          org: np22                date: 1990-10-11
!
! abstract: compute contribution to penalty and stepsize from radiances.
!
! program history log:
!   1990-10-11  parrish
!   1992-07-21
!   1995-07-17  derber
!   1997-03-10  wu       
!   1998-02-02  weiyu yang
!   1999-08-24  derber, j., treadon, r., yang, w., first frozen mpp version
!   2004-07-30  treadon - add only to module use, add intent in/out
!   2004-10-07  parrish - add nonlinear qc option
!   2005-01-20  okamoto - add wind components
!   2005-04-11  treadon - merge stprad and stprad_qc into single routine
!   2005-09-28  derber  - modify var qc and change location and weight arrays
!   2007-03-19  tremolet - binning of observations
!   2007-07-28  derber  - modify to use new inner loop obs data structure
!                       - unify NL qc
!   2007-02-15  rancic  - add foto
!   2007-06-04  derber  - use quad precision to get reproducability over number of processors
!   2008-04-09  safford - rm unused vars and uses
!   2008-12-03  todling - changed handling of ptr%time
!
!   input argument list:
!     radhead
!     rt       - search direction for temperature
!     rq       - search direction for moisture 
!     roz      - search direction for ozone
!     ru       - search direction for zonal wind
!     rv       - search direction for meridional wind
!     rst      - search direction for skin temperature
!     st       - input temperature correction field        
!     sq       - input q correction field        
!     soz      - input ozone correction field        
!     su       - input u correction field
!     sv       - input v correction field
!     sst      - input skin temp. vector 
!     rpred    - search direction for predictors
!     spred    - input predictor values
!     sges     - step size estimates(nstep)
!     nstep    - number of stepsizes (==0 means use outer iteration value)
!
!   output argument list:
!     out(1:nstep)   - penalty for radiance data sges(1:nstep)
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,i_kind,r_quad
  use radinfo, only: npred,jpch_rad,b_rad,pg_rad
  use obsmod, only: rad_ob_type
  use qcmod, only: nlnqc_iter,varqc_iter
  use constants, only: zero,half,one,two,tiny_r_kind,cg_term,r3600,zero_quad,one_quad
  use gridmod, only: nsig,nsig2,nsig3p1,nsig3p2,nsig3p3,&
       latlon11,latlon1n
  use jfunc, only: l_foto,xhat_dt,dhat_dt
  implicit none
  
! Declare passed variables
  type(rad_ob_type),pointer,intent(in):: radhead
  integer(i_kind),intent(in)::nstep
  real(r_quad),dimension(max(1,nstep)),intent(out):: out
  real(r_kind),dimension(latlon1n),intent(in):: rt,st,rq,sq,roz,soz,&
      ru,su,rv,sv
  real(r_kind),dimension(latlon11),intent(in):: rst,sst
  real(r_kind),dimension(npred,jpch_rad),intent(in):: rpred,spred
  real(r_kind),dimension(max(1,nstep)),intent(in):: sges

! Declare local variables
  integer(i_kind) nn,n,ic,k,nx,j1,j2,j3,j4,kk
  real(r_kind) val2,val,w1,w2,w3,w4
  real(r_kind),dimension(nsig3p3):: tdir,rdir
  real(r_kind) cg_rad,wgross,wnotgross
  real(r_kind) time_rad
  integer(i_kind),dimension(nsig) :: j1n,j2n,j3n,j4n
  real(r_kind),dimension(max(1,nstep)) :: term,rad
  type(rad_ob_type), pointer :: radptr

  out=zero_quad

  radptr=>radhead
  do while(associated(radptr))
    if(radptr%luse)then
     if(nstep > 0)then
       j1=radptr%ij(1)
       j2=radptr%ij(2)
       j3=radptr%ij(3)
       j4=radptr%ij(4)
       w1=radptr%wij(1)
       w2=radptr%wij(2)
       w3=radptr%wij(3)
       w4=radptr%wij(4)
       tdir(nsig3p1)=w1* su(j1) + w2* su(j2) + w3* su(j3) + w4* su(j4)
       rdir(nsig3p1)=w1* ru(j1) + w2* ru(j2) + w3* ru(j3) + w4* ru(j4)
       tdir(nsig3p2)=w1* sv(j1) + w2* sv(j2) + w3* sv(j3) + w4* sv(j4)
       rdir(nsig3p2)=w1* rv(j1) + w2* rv(j2) + w3* rv(j3) + w4* rv(j4)
       tdir(nsig3p3)=w1*sst(j1) + w2*sst(j2) + w3*sst(j3) + w4*sst(j4)   
       rdir(nsig3p3)=w1*rst(j1) + w2*rst(j2) + w3*rst(j3) + w4*rst(j4)   
       if(l_foto)then
         time_rad=radptr%time*r3600
         tdir(nsig3p1)=tdir(nsig3p1)+ &
                (w1*xhat_dt%u(j1) + w2*xhat_dt%u(j2) + &
                 w3*xhat_dt%u(j3) + w4*xhat_dt%u(j4))*time_rad
         rdir(nsig3p1)=rdir(nsig3p1)+ &
                (w1*dhat_dt%u(j1) + w2*dhat_dt%u(j2) + &
                 w3*dhat_dt%u(j3) + w4*dhat_dt%u(j4))*time_rad
         tdir(nsig3p2)=tdir(nsig3p2)+ &
                (w1*xhat_dt%v(j1) + w2*xhat_dt%v(j2) + &
                 w3*xhat_dt%v(j3) + w4*xhat_dt%v(j4))*time_rad
         rdir(nsig3p2)=rdir(nsig3p2)+ &
                (w1*dhat_dt%v(j1) + w2*dhat_dt%v(j2) + &
                 w3*dhat_dt%v(j3) + w4*dhat_dt%v(j4))*time_rad
       end if

       j1n(1) = j1
       j2n(1) = j2
       j3n(1) = j3
       j4n(1) = j4
       do n=2,nsig
        j1n(n) = j1n(n-1)+latlon11
        j2n(n) = j2n(n-1)+latlon11
        j3n(n) = j3n(n-1)+latlon11
        j4n(n) = j4n(n-1)+latlon11
       enddo
!$omp parallel do private(n,j1,j2,j3,j4)
       do n=1,nsig
          j1 = j1n(n)
          j2 = j2n(n)
          j3 = j3n(n)
          j4 = j4n(n)
  
!         Input state vector
          tdir(n)    =  w1* st(j1) +w2* st(j2) + w3* st(j3) +w4*  st(j4)
          tdir(nsig+n)= w1* sq(j1) +w2* sq(j2) + w3* sq(j3) +w4*  sq(j4)
          tdir(nsig2+n)=w1* soz(j1)+w2* soz(j2)+ w3* soz(j3)+w4* soz(j4)

!         Input search direction vector
          rdir(n)    =  w1* rt(j1) +w2* rt(j2) + w3* rt(j3) +w4*  rt(j4)
          rdir(nsig+n)= w1* rq(j1) +w2* rq(j2) + w3* rq(j3) +w4*  rq(j4)
          rdir(nsig2+n)=w1* roz(j1)+w2* roz(j2)+ w3* roz(j3)+w4* roz(j4)

       end do
!$omp end parallel do
       if(l_foto)then
!$omp parallel do private(n,j1,j2,j3,j4)
         do n=1,nsig
          j1 = j1n(n)
          j2 = j2n(n)
          j3 = j3n(n)
          j4 = j4n(n)
  
!         Input state vector
          tdir(n)    =  tdir(n)+                                &
                   (w1*xhat_dt%t(j1) +w2*xhat_dt%t(j2) +        &
                    w3*xhat_dt%t(j3) +w4*xhat_dt%t(j4))*time_rad
          tdir(nsig+n)= tdir(nsig+n)+                           &
                   (w1*xhat_dt%q(j1) +w2*xhat_dt%q(j2) +        &
                    w3*xhat_dt%q(j3) +w4*xhat_dt%q(j4))*time_rad
          tdir(nsig2+n)=tdir(nsig2+n)+                          &
                   (w1*xhat_dt%oz(j1)+w2*xhat_dt%oz(j2)+        &
                    w3*xhat_dt%oz(j3)+w4*xhat_dt%oz(j4))*time_rad

!         Input search direction vector
          rdir(n)    =  rdir(n)+                                &
                   (w1*dhat_dt%t(j1) +w2*dhat_dt%t(j2) +        &
                    w3*dhat_dt%t(j3) +w4*dhat_dt%t(j4))*time_rad
          rdir(nsig+n)= rdir(nsig+n)+                           &
                   (w1*dhat_dt%q(j1) +w2*dhat_dt%q(j2) +        &
                    w3*dhat_dt%q(j3) +w4*dhat_dt%q(j4))*time_rad
          rdir(nsig2+n)=rdir(nsig2+n)+                          &
                   (w1*dhat_dt%oz(j1)+w2*dhat_dt%oz(j2)+        &
                    w3*dhat_dt%oz(j3)+w4*dhat_dt%oz(j4))*time_rad

         end do
!$omp end parallel do
       end if
     end if
     do nn=1,radptr%nchan

        val2=-radptr%res(nn)

        if(nstep > 0)then
          val = zero
!         contribution from bias corection
          ic=radptr%icx(nn)
          do nx=1,npred
             val2=val2+spred(nx,ic)*radptr%pred(nx,nn)
             val =val +rpred(nx,ic)*radptr%pred(nx,nn)
          end do

!         contribution from atmosphere
          do k=1,nsig3p3
             val2=val2+tdir(k)*radptr%dtb_dvar(k,nn)
             val =val +rdir(k)*radptr%dtb_dvar(k,nn)
          end do

!         calculate radiances for each stepsize
          do kk=1,nstep
            rad(kk)=val2+sges(kk)*val
          end do
        else
          rad(kk)= val2
        end if
        
!       calculate contribution to J
        do kk=1,max(1,nstep)
          term(kk)  = radptr%err2(nn)*rad(kk)*rad(kk)
        end do
        
!       Modify penalty term if nonlinear QC
        if(nlnqc_iter .and. pg_rad(ic) > tiny_r_kind .and. &
                            b_rad(ic)  > tiny_r_kind)then
           cg_rad=cg_term/b_rad(ic)
           wnotgross= one-pg_rad(ic)*varqc_iter
           wgross = varqc_iter*pg_rad(ic)*cg_rad/wnotgross
           do kk=1,max(1,nstep)
             term(kk)  = -two*log((exp(-half*term(kk) ) + wgross)/(one+wgross))
           end do
        endif

        out(1) = out(1) + term(1)*radptr%raterr2(nn)
        do kk=2,nstep
          out(kk) = out(kk) + (term(kk)-term(1))*radptr%raterr2(nn)
        end do

     end do

    end if

    radptr => radptr%llpoint
  end do
  return
end subroutine stprad

end module stpradmod
