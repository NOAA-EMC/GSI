  subroutine genqsat(temp,qsat,lat2,lon2,ps,icesat,a5,b5,c5)
!   input argument list:
!     t,ps     - temperature, psfc on gaussian grid
!     lat1     - number of gaussian lats in the sub-domain array
!     lon1     - number of gaussian longitudes in the sub-domain array
!     nsig     - number of sigma levels
!     icesat   - logical flag:  T=include ice and ice-water effects,
!                depending on t, in qsat and esat calcuations.
!                otherwise, compute esat and qsat with respect to water surface
!                depending on t, in qsat and esat calcuations.
!                otherwise, compute esat and qsat with respect to water surface
!
!   output argument list:
!     qsat     - specific humidity (input), saturation specific humidity (output)
  use kinds, only: r_kind
  use variables,only: nsig,rd,zero,psat,fv,one,rv,cliq,cvap,hvap,csol,&
      hfus,ttp
  implicit none

  real(r_kind) dldt,xa,xb,hsub,dldti,xai,xbi,tmix
  parameter(dldt=cvap-cliq,xa=-dldt/rv,xb=xa+hvap/(rv*ttp))
  parameter(hsub=hvap+hfus)
  parameter(dldti=cvap-csol,xai=-dldti/rv,xbi=xai+hsub/(rv*ttp))
  parameter (tmix=ttp-20.0_r_kind)
  real(r_kind) eps,epsm1,omeps
  real(r_kind),dimension(lat2,lon2):: mint
  integer,dimension(lat2,lon2):: lmint

  parameter (eps=rd/rv,epsm1=rd/rv-1._r_kind)
  parameter (omeps = 1._r_kind-eps)
!
  logical icesat
  integer k,j,i,ntsig
  real(r_kind) pw,q,tdry,tr,es,qs,esi,esw
  real(r_kind) w,onep3,esmax,estmax
  real(r_kind),dimension(lat2,lon2,nsig):: qsat
! real(r_kind),dimension(lat2,lon2,nsig):: esat
  real(r_kind),dimension(lat2,lon2,nsig):: temp
  real(r_kind),dimension(lat2,lon2):: ps
  real(r_kind),dimension(nsig+1):: a5,b5,c5
  integer lat2,lon2
!
  onep3 = 1.e3_r_kind

  lmint=1
  do j=1,lon2
    do i=1,lat2
      mint(i,j)=340._r_kind
    end do
  end do

  do k=1,nsig
    do j=1,lon2
      do i=1,lat2
          pw = 0.5*(a5(k)+a5(k+1)+(b5(k)+b5(k+1))* &
                         ps(i,j))
        if((pw < 30._r_kind .and. pw > 2._r_kind) .and.  &
            temp(i,j,k) < mint(i,j))then
           lmint(i,j)=k
           mint(i,j)=temp(i,j,k)
        end if
      end do
    end do
  end do

  if (icesat) then
     do k = 1,nsig
        do j = 1,lon2
           do i = 1,lat2
               pw = 0.5*(a5(k)+a5(k+1)+(b5(k)+b5(k+1))* &
                          ps(i,j))
              pw  = onep3*pw
! maximum vapor pressure 10% of atmospheric pressure
              esmax=.1*pw

              if(lmint(i,j) < k)then
                 tdry = mint(i,j)
                 tr = ttp/tdry
                 if (tdry >= ttp) then
                    estmax = psat * (tr**xa) * exp(xb*(one-tr))
                 elseif (tdry < tmix) then
                    estmax = psat * (tr**xai) * exp(xbi*(one-tr))
                 else
                    w  = (tdry - tmix) / (ttp - tmix)
                    estmax =  w * psat * (tr**xa) * exp(xb*(one-tr)) &
                              + (one-w) * psat * (tr**xai) * exp(xbi*(one-tr))
                 endif
                 esmax=min(esmax,estmax)
              end if

              tdry = temp(i,j,k)
              tr = ttp/tdry
              if (tdry >= ttp) then
                 es = psat * (tr**xa) * exp(xb*(one-tr))
              elseif (tdry < tmix) then
                 es = psat * (tr**xai) * exp(xbi*(one-tr))
              else
                 w  = (tdry - tmix) / (ttp - tmix)
                 es =  w * psat * (tr**xa) * exp(xb*(one-tr)) &
                      + (one-w) * psat * (tr**xai) * exp(xbi*(one-tr))
              endif

              es = min(es,esmax)
              qs = eps * es / (pw - omeps * es)

              qsat(i,j,k) = qs

           end do
        end do
     end do

!
!     Compute saturation values with respect to water surface
  else
     do k = 1,nsig
        do j = 1,lon2
           do i = 1,lat2
               pw = 0.5*(a5(k)+a5(k+1)+(b5(k)+b5(k+1))* &
                          ps(i,j))

              pw  = onep3*pw
! maximum vapor pressure 10% of atmospheric pressure
              esmax=.1*pw

              if(lmint(i,j) < k)then
                 tr=ttp/mint(i,j)
                 estmax = psat * (tr**xa) * exp(xb*(one-tr))
                 esmax=min(esmax,estmax)
              end if


              tdry = temp(i,j,k)
              tr = ttp/tdry
              es = psat * (tr**xa) * exp(xb*(one-tr))
              es = min(es,esmax)
              qs = eps * es / (pw - omeps * es)

              qsat(i,j,k) = qs


           end do
        end do
     end do

  endif

  return
  end subroutine genqsat

