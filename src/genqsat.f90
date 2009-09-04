subroutine genqsat(qsat,ice,itime,dlnesdtv,dmax)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    genqsat
!   prgmmr: derber           org: np23                date: 1998-01-14
!
! abstract: obtain saturation specific humidity for given temperature.
!
! program history log:
!   1998-01-14  derber
!   1998-04-05  weiyu yang
!   1999-08-24  derber, j., treadon, r., yang, w., first frozen mpp version
!   1903-10-07  Wei Gu, bug fixes,if qs<0,then set qs=0; merge w/ GSI by R Todling
!   2003-12-23  kleist, use guess pressure, adapt module framework
!   2004-05-13  kleist, documentation
!   2004-06-03  treadon, replace ggrid_g3 array with ges_* arrays
!   2005-02-23  wu, output dlnesdtv
!   2005-11-21  kleist, derber  add dmax array to decouple moisture from temp and
!               pressure for questionable qsat
!   2006-02-02  treadon - rename prsl as ges_prsl
!   2006-09-18  derber - modify to limit saturated values near top
!   2006-11-22  derber - correct bug:  es<esmax should be es<=esmax
!   2008-06-04  safford - rm unused vars
!
!   input argument list:
!     ggrid_g31 - guess grids, contains temperature and log(ps)               
!     qsat      - guess specific humidity
!     ice       - logical flag:  T=include ice and ice-water effects,
!                 depending on t, in qsat calcuations.
!                 otherwise, compute qsat with respect to water surface
!     npes      - last dimension of guess array
!     itime     - integer id for position of time in array
!
!   output argument list:
!     qsat      - saturation specific humidity (output)
!
! remarks: see modules used
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_single,r_kind,i_kind
  use constants, only: xai,tmix,xb,omeps,eps,xbi,one,zero,&
       xa,psat,ttp
  use gridmod, only: nsig,lon2,lat2
  use guess_grids, only: ges_tsen,ges_prsl
  implicit none

  logical,intent(in):: ice
  real(r_kind),dimension(lat2,lon2,nsig),intent(inout):: qsat
  real(r_kind),dimension(lat2,lon2,nsig),intent(out):: dlnesdtv,dmax
  integer(i_kind),intent(in):: itime

  integer(i_kind) k,j,i
  real(r_kind) pw,tdry,tr,es
  real(r_kind) w,onep3,esmax
  real(r_kind) desidt,deswdt,dwdt,desdt,esi,esw
  real(r_kind),dimension(lat2,lon2):: mint,estmax
  real(r_kind),dimension(nsig)::maxrh
  integer(i_kind),dimension(lat2,lon2):: lmint

  onep3 = 1.e3_r_kind

  maxrh = zero
  lmint=1
  do j=1,lon2
    do i=1,lat2
      mint(i,j)=340._r_kind
    end do
  end do
  do k=1,nsig
    do j=1,lon2
      do i=1,lat2
        if((ges_prsl(i,j,k,itime) < 30._r_kind .and.  &
            ges_prsl(i,j,k,itime) > 2._r_kind) .and.  &
            ges_tsen(i,j,k,itime) < mint(i,j))then
           lmint(i,j)=k
           mint(i,j)=ges_tsen(i,j,k,itime)
         end if
       end do
    end do
  end do
  do j=1,lon2
    do i=1,lat2
      tdry = mint(i,j)
      tr = ttp/tdry
      if (tdry >= ttp .or. .not. ice) then
        estmax(i,j) = psat * (tr**xa) * exp(xb*(one-tr))
      elseif (tdry < tmix) then
        estmax(i,j) = psat * (tr**xai) * exp(xbi*(one-tr))
      else
        w  = (tdry - tmix) / (ttp - tmix)
        estmax(i,j) =  w * psat * (tr**xa) * exp(xb*(one-tr)) &
                + (one-w) * psat * (tr**xai) * exp(xbi*(one-tr))
      endif
    end do
  end do
  if (ice) then
    do k = 1,nsig
      do j = 1,lon2
        do i = 1,lat2

          pw = onep3*ges_prsl(i,j,k,itime)
              
          tdry = ges_tsen(i,j,k,itime)
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

          esmax = es
          if(lmint(i,j) < k)then
             esmax=0.1_r_single*pw
             esmax=min(esmax,estmax(i,j))
          end if

          if(es <= esmax)then

            if (tdry >= ttp) then
               desdt = es * (-xa/tdry + xb*ttp/(tdry*tdry))
            elseif (tdry < tmix) then
               desdt = es * (-xai/tdry + xbi*ttp/(tdry*tdry))
            else
               esw = (psat * (tr**xa) * exp(xb*(one-tr))) 
               esi = (psat * (tr**xai) * exp(xbi*(one-tr))) 
               w  = (tdry - tmix) / (ttp - tmix)

               dwdt = one/(ttp-tmix)
               deswdt = esw * (-xa/tdry + xb*ttp/(tdry*tdry))
               desidt = esi * (-xai/tdry + xbi*ttp/(tdry*tdry))
               desdt = dwdt*esw + w*deswdt - dwdt*esi + (one-w)*desidt
            endif

            dlnesdtv(i,j,k) = desdt /es
            dmax(i,j,k) = one
          else
            es = esmax
            dlnesdtv(i,j,k) = zero
            dmax(i,j,k) = zero
          end if
          qsat(i,j,k) = eps * es / (pw - omeps * es)

        end do
      end do
    end do

! Compute saturation values with respect to water surface
  else
    do k = 1,nsig
      do j = 1,lon2
        do i = 1,lat2
              
          pw = onep3*ges_prsl(i,j,k,itime) 

          tdry = ges_tsen(i,j,k,itime)
          tr = ttp/tdry
          es = psat * (tr**xa) * exp(xb*(one-tr))
          esmax = es
          if(lmint(i,j) < k)then
             esmax=0.1_r_single*pw
             esmax=min(esmax,estmax(i,j))
          end if
          if(es <= esmax)then
            dlnesdtv(i,j,k) = (-xa/tdry + xb*ttp/(tdry*tdry))
            dmax(i,j,k) = one
          else
            dlnesdtv(i,j,k) = zero
            es = esmax
            dmax(i,j,k) = zero
          end if
          qsat(i,j,k) = eps * es / (pw - omeps * es)

        end do
      end do
    end do
     
  endif   ! end if ice
! write(6,*) (maxrh(k),k=1,nsig)
  
  return
end subroutine genqsat
