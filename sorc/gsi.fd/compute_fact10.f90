subroutine compute_fact10(u,v,t,q,ps,prsi,skint,z0rl,slimsk,f10m)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    compute_fact10                compute 10m wind factor
!   prgmmr: treadon          org: np23                date: 2006-09-28
!
! abstract: Use GFS surface physics routines to compute 10m wind factor
!
! program history log:
!   2006-09-28 treadon - initial routine
!
!   input argument list:
!      u       - u wind component (2d field, 1st model layer)
!      v       - v wind component
!      t       - sensible temperature
!      q       - specific humidity
!      ps      - surface pressure
!      prsi    - interface pressure
!      skint   - skin temperature
!      z0rl    - surface roughness
!      slimsk  - land/sea/ice mask
!
!   output argument list:
!     f10m     - 10m wind factor
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$

  use kinds, only: r_kind,i_kind
  use constants, only: grav,zero,half,one,two,four,quarter,izero,&
       fv,rd,rd_over_cp
  use gridmod, only: lat2,lon2,nsig
  implicit none

! Passed Variables
  real(r_kind),dimension(lat2,lon2),intent(in):: u,v,t,q,ps,skint,z0rl
  integer(i_kind),dimension(lat2,lon2),intent(in):: slimsk
  real(r_kind),dimension(lat2,lon2),intent(out):: f10m
  real(r_kind),dimension(lat2,lon2,nsig+1):: prsi

! Local Variables
  integer(i_kind) i,j,k
  real(r_kind):: q0,tem,del,rkap,rkapi,rkapp1
  real(r_kind),dimension(lat2)::ustar,wind,z0,z0max,ztmax,tv1,z1, &
       rat,thv1,theta1,tvs,dtv,rb,fm,fh,hlinf, &
       hl1,pm,ph,pm10,hl12,ph2,fm10
  real(r_kind),dimension(lat2):: psurf,ps1,stress,prslk
  real(r_kind),dimension(lat2,nsig):: prsl,prkl
  real(r_kind),dimension(lat2,nsig+1):: prki
  real(r_kind),dimension(lat2,lon2):: fh2,cm,ffmmin,rslimsk
  real(r_kind) restar,aa0,bb0,fhs,fms,hl0,hlt,adtv,bb,aa,hl0inf,hltinf,&
       hl110,olinf,lnsig

! Local Parameters
    real(r_kind),parameter::  charnok=0.014_r_kind
    real(r_kind),parameter::  ca=0.4_r_kind
    real(r_kind),parameter::  alpha=5.0_r_kind
    real(r_kind),parameter::  a0=-3.975_r_kind
    real(r_kind),parameter::  a1=12.32_r_kind
    real(r_kind),parameter::  b1=-7.755_r_kind
    real(r_kind),parameter::  b2=6.041_r_kind
    real(r_kind),parameter::  a0p=-7.941_r_kind
    real(r_kind),parameter::  a1p=24.75_r_kind
    real(r_kind),parameter::  b1p=-8.705_r_kind
    real(r_kind),parameter::  b2p=7.899_r_kind 
    real(r_kind),parameter::  vis=1.4e-5_r_kind
    real(r_kind),parameter::  aa1=-1.076_r_kind
    real(r_kind),parameter::  bb1=0.7045_r_kind
    real(r_kind),parameter::  cc1=-0.05808_r_kind
    real(r_kind),parameter::  bb2=-0.1954_r_kind
    real(r_kind),parameter::  cc2=0.009999_r_kind
    real(r_kind),parameter::  rnu=1.51e-5_r_kind
    real(r_kind),parameter::  arnu=0.135_r_kind*rnu
    real(r_kind),parameter::  ten=10.0_r_kind


!  INITIALIZE VARIABLES. ALL UNITS ARE SUPPOSEDLY M.K.S. UNLESS SPECIFIED
!  PSURF IS IN PASCALS
!  WIND IS WIND SPEED, THETA1 IS ADIABATIC SURFACE TEMP FROM LEVEL 1
!  SURFACE ROUGHNESS LENGTH IS CONVERTED TO M FROM CM

   rat=zero ; restar=zero ; ustar=zero

   rkap = rd_over_cp
   RKAPI  = 1.0 / RKAP
   RKAPP1 = 1.0 + RKAP

   do j=1,lon2
      do i=1,lat2
         rslimsk(i,j)=float(slimsk(i,j))
      end do
   end do

    do j=1,lon2
      do i=1,lat2

         prki(i,1) = (prsi(i,j,1)*0.01_r_kind)**rkap
         do k=1,nsig
            del = prsi(i,j,k)-prsi(i,j,k+1)
            prki(i,k+1) = (prsi(i,j,k+1)*0.01_r_kind)**rkap
            tem         = rkapp1*del
            prkl(i,k)   = (prki(i,k)*prsi(i,j,k)-prki(i,k+1)*prsi(i,j,k+1))/tem
            prsl(i,k)   = 100.0_r_kind*prkl(i,k)**rkapi
         end do

        psurf(i)=ps(i,j)*1000.0_r_kind
        ps1(i)=prsl(i,1)*1000.0_r_kind
        wind(i)= sqrt( u(i,j)*u(i,j) + v(i,j)*v(i,j) )
        wind(i)=max(wind(i),one)
        q0=max(q(i,j),1.e-8_r_kind)
        theta1(i)=t(i,j)*(prki(i,1)/prkl(i,1))
        tv1(i) =t(i,j)    *(one+fv*q0)
        thv1(i)=theta1(i) *(one+fv*q0)
        tvs(i) =skint(i,j)*(one+fv*q0)
        z0(i)=0.01_r_kind*z0rl(i,j)
        z1(i)=-rd*tv1(i)*log(ps1(i)/psurf(i))/grav

      end do


!  COMPUTE STABILITY DEPENDENT EXCHANGE COEFFICIENTS
      do i=1,lat2
        if (rslimsk(i,j).EQ.zero) then
          ustar(i) = sqrt(grav * z0(i) / charnok)
        end if
!  COMPUTE STABILITY INDICES (RB AND HLINF)
        z0max(i) = min(z0(i),one*z1(i))
        ztmax(i) = z0max(i)
        if (rslimsk(i,j).EQ.zero) then
          restar=ustar(i)*z0max(i)/vis
          restar=max(restar,1.e-6_r_kind)

!         Rat taken from Zeng, Zhao and Dickinson 1997
          rat(i) = 2.67_r_kind * restar**quarter - 2.57_r_kind
          rat(i) = min(rat(i),7.0_r_kind)
          ztmax(i) = z0max(i) * exp(-rat(i))
        end if

      end do


      do i=1,lat2
        dtv(i) = thv1(i) - tvs(i)
        adtv = abs(dtv(i))
        adtv = max(adtv,1.e-3_r_kind)
        dtv(i) = sign(one,dtv(i))*adtv
        rb(i) = grav*dtv(i)*z1(i) / (half*(thv1(i)+tvs(i))    &
          *wind(i)*wind(i))
        rb(i)=max(rb(i),-5.e3_r_kind)
        fm(i)=log((z0max(i)+z1(i)) / z0max(i))
        fh(i)=log((ztmax(i)+z1(i)) / ztmax(i))
        hlinf(i)=rb(i)*fm(i)*fm(i) / fh(i)
        fm10(i)=log((z0max(i)+ten) / z0max(i))
        fh2(i,j)=log((ztmax(i)+two) / ztmax(i))
      end do


!  STABLE CASE
      do i=1,lat2
        if (dtv(i).GE.zero) then
          hl1(i)=hlinf(i)
        end if        
        if ((dtv(i).GE.zero) .AND. (hlinf(i).GT.0.25_r_kind)) then
          hl0inf=z0max(i)*hlinf(i)/z1(i)
          hltinf=ztmax(i)*hlinf(i)/z1(i)
          aa=sqrt(one + four*alpha*hlinf(i))
          aa0=sqrt(one + four*alpha*hl0inf)
          bb=aa
          bb0=sqrt(one + four*alpha*hltinf)
          pm(i)=aa0 - aa + log((aa+one)/(aa0+one))
          ph(i)=bb0 - bb + log((bb+one)/(bb0+one))
          fms=fm(i)-pm(i)
          fhs=fh(i)-ph(i)
          hl1(i)=fms*fms*rb(i)/fhs
        end if
      end do


!  SECOND ITERATION
      do i=1,lat2
        if (dtv(i).GE.zero) then
          hl0=z0max(i)*hl1(i)/z1(i)
          hlt=ztmax(i)*hl1(i)/z1(i)
          aa=sqrt(one + four*alpha*hl1(i))
          aa0=sqrt(one + four*alpha*hl0)
          bb=aa
          bb0=sqrt(one + four*alpha*hlt)
          pm(i)=aa0 - aa + log((aa+one)/(aa0+one))
          ph(i)=bb0 - bb + log((bb+one)/(bb0+one))
          hl110=hl1(i)*ten/z1(i)
          aa=sqrt(one + four*alpha*hl110)
          pm10(i)=aa0 - aa + log((aa+one)/(aa0+one))
          hl12(I)=hl1(i)*two/z1(i)
          bb=sqrt(one + four*alpha*hl12(i))
          ph2(i)=bb0 - bb + log((bb+one)/(bb0+one))
        end if
      end do


!  UNSTABLE CASE
!  CHECK FOR UNPHYSICAL OBUKHOV LENGTH
      do i=1,lat2
        if (dtv(i).LT.zero) then
          olinf = z1(i)/hlinf(i)
          if ( abs(olinf) .LE. z0max(i)*50.0_r_kind ) then
            hlinf(i) = -z1(i)/(50.0_r_kind*z0max(i))
          end if
        end if
      end do

!  GET PM AND PH
      do i=1,lat2
        if (dtv(i).LT.zero .AND. hlinf(i).GE.(-0.5_r_kind)) then
          hl1(i)=hlinf(i)
          pm(i)=(a0 + a1*hl1(i))*hl1(i)      &
             /(one + b1*hl1(i) + b2*hl1(i)*hl1(i))
          ph(i)=(a0p + a1p*hl1(i))*hl1(i)    &
             /(one + b1p*hl1(i) + b2*hl1(i)*hl1(i))
          hl110=hl1(i)*ten/z1(i)
          pm10(i)=(a0 + a1*hl110)*hl110      &
             /(one + b1*hl110 + b2*hl110*hl110)
          hl12(i)=hl1(i)*two/z1(i)
          ph2(i)=(a0p + a1p*hl12(i))*hl12(i) &
             /(one + b1p*hl12(i) + b2p*hl12(i)*hl12(i))
        end if
        if (dtv(i).LT.zero .AND. hlinf(i).LT.(-0.5_r_kind)) then
          hl1(i)=-hlinf(i)
          pm(i)=log(hl1(i)) + two*hl1(i)**(-quarter) - 0.8776_r_kind
          ph(i)=log(hl1(i)) + half*hl1(i)**(-half) + 1.386_r_kind
          hl110=hl1(i)*ten/z1(i)
          pm10(i)=log(hl110) + two*hl110**(-quarter) - 0.8776_r_kind
          hl12(i)=hl1(i)*two/z1(i)
          ph2(i)=log(hl12(i)) + half*hl12(i)**(-half) + 1.386_r_kind
        end if
     end do


!  FINISH THE EXCHANGE COEFFICIENT COMPUTATION TO PROVIDE FM AND FH
      do i=1,lat2
        fm(i)=fm(i)-pm(i)
        fh(i)=fh(i)-ph(i)
        fm10(i)=fm10(i)-pm10(i)
        f10m(i,j)=fm10(i)/fm(i)
        fh2(i,j)=fh2(i,j)-ph2(i)       
      end do

    end do  ! end do lon2

    return
  end subroutine compute_fact10

