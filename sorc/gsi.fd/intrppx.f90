subroutine intrppx(obstime,h,q,poz,prsl,prsi,zz,ts,tsavg, &
                   vty,vfr,sty,stp,sm,sn,trop5,    &
                   uu5,vv5,ff10,dx,dy,mype)       
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    intrppx     creates vertical profile of t,q,p,zs    
!   prgmmr: parrish          org: np22                date: 1990-10-11
!
! abstract: interpolates to create vertical profiles of t,q,p,zs for
!           satellite data
!
! program history log:
!   1990-10-11  parrish
!   1995-07-17  derber
!   1997-08-15  matsumura
!   1998-05-08  weiyu yang mpp version
!   1999-08-24  derber, j., treadon, r., yang, w., first frozen mpp version
!   2004-05-18  kleist, documentation
!   2004-08-02  treadon - add only to module use, add intent in/out
!   2004-11-22  derber - add openMP
!   2004-12-16  treadon - change order of passed variable declaration
!   2005-01-20  okamoto - add uu5,vv5,ff10 to out arguments
!   2005-02-16  derber - modify land sea flag calculation
!   2005-09-28  derber - use land/sea/ice/snow calculations from read routines
!   2006-04-27  derber - add pressure interpolation and modify to do single profile
!   2006-06-16  kleist - bug fix: niy,niy1 used before being d
!   2006-07-27  derber - work from tsen rather than tv
!   2006-07-31  kleist - remove interpolation of ln(ps) to ob location
!
!   input argument list:
!     obstime  - time of observations for which to get profile
!     dx,dy    - input x,y of interpolation points (grid units)
!     mype     - mpi task id
!
!   output argument list:
!     h        - interpolated temperature
!     q        - interpolated mixing ratio
!     poz      - interpolated ozone
!     zz       - interpolated surface elevation
!     ts       - interpolated skin temperature
!     uu5      - interpolated bottom sigma level zonal wind    
!     vv5      - interpolated bottom sigma level meridional wind  
!     ff10     - interpolated 10m factor
!     vty      - nearest vegetation type  
!     vfr      - interpolated vegetation fraction
!     sty      - nearest soil type 
!     stp      - interpolated soil temperature 
!     sm       - interpolated soil moisture
!     sn       - interpolated snow depth
!     trop5    - interpolated tropopause pressure
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
!--------
  use kinds, only: r_kind,i_kind
  use guess_grids, only: ges_z,ges_u,ges_v,ges_tsen,ges_q,ges_oz,&
       ges_prsl,ges_prsi, &
       sfct,fact10,sno,isli,soil_moi,soil_temp,tropprs,veg_type,soil_type,&
       hrdifsig,hrdifsfc,nfldsig,nfldsfc,ntguessfc,veg_frac
  use gridmod, only: istart,jstart,nlon,nlat,nsig,lon1
  use constants, only: zero,one
  implicit none

! Declare passed variables
  integer(i_kind),intent(in):: mype
  real(r_kind),intent(in):: dx,dy,obstime
  real(r_kind),intent(out):: zz,vty,vfr,sty,stp,sm,sn,trop5
  real(r_kind),dimension(nsig),intent(out):: h,q,poz,prsl
  real(r_kind),dimension(nsig+1),intent(out):: prsi
  real(r_kind),dimension(0:3),intent(out)::ts
  real(r_kind),intent(out):: ff10,uu5,vv5,tsavg

! Declare local parameters
! real(r_kind),parameter:: fifty=50.0_r_kind
  real(r_kind),parameter:: minsnow=0.1_r_kind

! Declare local variables  
  integer(i_kind) i,j,k,m1,ix,ix1,ixp,iy,iy1,iyp
  integer(i_kind) itsfc,itsfcp,itsig,itsigp
  integer(i_kind) istyp00,istyp10,istyp01,istyp11
  real(r_kind) u1,v1,f10,u10,v10,w00,w01,w10,w11
  real(r_kind) sno00,sno01,sno10,sno11,wgtmin
  real(r_kind) delx,dely,delx1,dely1,dtsfc,dtsfcp,dtsig,dtsigp
  real(r_kind),dimension(0:3)::wgtavg


  m1=mype+1

! Set spatial interpolation indices and weights
  ix1=dx
  ix1=max(1,min(ix1,nlat))
  delx=dx-ix1
  delx=max(zero,min(delx,one))
  ix=ix1-istart(m1)+2
  ixp=ix+1
  if(ix1==nlat) then
     ixp=ix
  end if
  delx1=one-delx

  iy1=dy
  dely=dy-iy1
  iy=iy1-jstart(m1)+2
  if(iy<1) then
     iy1=iy1+nlon
     iy=iy1-jstart(m1)+2
  end if
  if(iy>lon1+1) then
     iy1=iy1-nlon
     iy=iy1-jstart(m1)+2
  end if
  iyp=iy+1
  dely1=one-dely

  w00=delx1*dely1; w10=delx*dely1; w01=delx1*dely; w11=delx*dely


! Get time interpolation factors for surface files
  if(obstime > hrdifsfc(1) .and. obstime < hrdifsfc(nfldsfc))then
     do j=1,nfldsfc-1
        if(obstime > hrdifsfc(j) .and. obstime <= hrdifsfc(j+1))then
           itsfc=j
           itsfcp=j+1
           dtsfc=((hrdifsfc(j+1)-obstime)/(hrdifsfc(j+1)-hrdifsfc(j)))
        end if
     end do
  else if(obstime <=hrdifsfc(1))then
     itsfc=1
     itsfcp=1
     dtsfc=one
  else
     itsfc=nfldsfc
     itsfcp=nfldsfc
     dtsfc=one
  end if
  dtsfcp=one-dtsfc

  istyp00 = isli(ix ,iy ,ntguessfc)
  istyp01 = isli(ix ,iyp,ntguessfc)
  istyp10 = isli(ixp,iy ,ntguessfc)
  istyp11 = isli(ixp,iyp,ntguessfc)
  sno00= sno(ix ,iy ,itsfc)*dtsfc+sno(ix ,iy ,itsfcp)*dtsfcp
  sno01= sno(ix ,iyp,itsfc)*dtsfc+sno(ix ,iyp,itsfcp)*dtsfcp
  sno10= sno(ixp,iy ,itsfc)*dtsfc+sno(ixp,iy ,itsfcp)*dtsfcp
  sno11= sno(ixp,iyp,itsfc)*dtsfc+sno(ixp,iyp,itsfcp)*dtsfcp
! sn= sno00*w00+sno10*w10+sno01*w01+sno11*w11
! sn=min(sn,fifty)
  if(istyp00 >= 1 .and. sno00 > minsnow)istyp00 = 3
  if(istyp01 >= 1 .and. sno01 > minsnow)istyp01 = 3
  if(istyp10 >= 1 .and. sno10 > minsnow)istyp10 = 3
  if(istyp11 >= 1 .and. sno11 > minsnow)istyp11 = 3

! Interpolate fields which only vary in space (no time component)
!    zz   = surface height
!    trop5= tropopause pressure
!    vty  = vegetation type
!    sty  = soil type

  zz   = ges_z(ix,iy,1) *w00 + ges_z(ixp,iy,1) *w10 + &
         ges_z(ix,iyp,1)*w01 + ges_z(ixp,iyp,1)*w11
  trop5= tropprs(ix,iy )*w00+tropprs(ixp,iy )*w10+ &
         tropprs(ix,iyp)*w01+tropprs(ixp,iyp)*w11
  wgtmin= -1.
  ts(0:3)=zero
  wgtavg(0:3)=zero
  vfr=zero
  stp=zero
  sty=zero
  vty=zero
  sm=zero
  sn=zero
  if(istyp00 == 1)then
    wgtmin = w00
    vty  = veg_type(ix ,iy ,ntguessfc)
    sty  = soil_type(ix ,iy ,ntguessfc)
    wgtavg(1) = wgtavg(1) + w00
    ts(1)=ts(1)+w00*(sfct(ix ,iy ,itsfc )     *dtsfc+   &
                     sfct(ix ,iy ,itsfcp)     *dtsfcp)
    vfr  =vfr  +w00*(veg_frac(ix ,iy ,itsfc ) *dtsfc+   &
                     veg_frac(ix ,iy ,itsfcp) *dtsfcp)
    stp  =stp  +w00*(soil_temp(ix ,iy ,itsfc )*dtsfc+   &
                     soil_temp(ix ,iy ,itsfcp)*dtsfcp)
    sm   =sm   +w00*(soil_moi(ix ,iy ,itsfc ) *dtsfc+   &
                     soil_moi(ix ,iy ,itsfcp) *dtsfcp)
  else if(istyp00 == 2)then
    wgtavg(2) = wgtavg(2) + w00
    ts(2)=ts(2)+w00*(sfct(ix ,iy ,itsfc )     *dtsfc+   &
                     sfct(ix ,iy ,itsfcp)     *dtsfcp)
  else if(istyp00 == 3)then
    wgtavg(3) = wgtavg(3) + w00
    ts(3)=ts(3)+w00*(sfct(ix ,iy ,itsfc )     *dtsfc+   &
                     sfct(ix ,iy ,itsfcp)     *dtsfcp)
    sn = sn + w00*sno00
  else 
    wgtavg(0) = wgtavg(0) + w00
    ts(0)=ts(0)+w00*(sfct(ix ,iy ,itsfc )     *dtsfc+   &
                     sfct(ix ,iy ,itsfcp)     *dtsfcp)
  end if

  if(istyp01 == 1)then
    if(wgtmin < w01)then
      wgtmin = w01
      vty  = veg_type(ix ,iyp,ntguessfc)
      sty  = soil_type(ix ,iyp,ntguessfc)
    end if
    wgtavg(1) = wgtavg(1) + w01
    ts(1)=ts(1)+w01*(sfct(ix ,iyp,itsfc )     *dtsfc+   &
                     sfct(ix ,iyp,itsfcp)     *dtsfcp)
    vfr  =vfr  +w01*(veg_frac(ix ,iyp,itsfc ) *dtsfc+   &
                     veg_frac(ix ,iyp,itsfcp) *dtsfcp)
    stp  =stp  +w01*(soil_temp(ix ,iyp,itsfc )*dtsfc+   &
                     soil_temp(ix ,iyp,itsfcp)*dtsfcp)
    sm   =sm   +w01*(soil_moi(ix ,iyp,itsfc ) *dtsfc+   &
                     soil_moi(ix ,iyp,itsfcp) *dtsfcp)
  else if(istyp01 == 2)then
    wgtavg(2) = wgtavg(2) + w01
    ts(2)=ts(2)+w01*(sfct(ix ,iyp,itsfc )     *dtsfc+   &
                     sfct(ix ,iyp,itsfcp)     *dtsfcp)
  else if(istyp01 == 3)then
    wgtavg(3) = wgtavg(3) + w01
    ts(3)=ts(3)+w01*(sfct(ix ,iyp,itsfc )     *dtsfc+   &
                     sfct(ix ,iyp,itsfcp)     *dtsfcp)
    sn = sn + w01*sno01
  else 
    wgtavg(0) = wgtavg(0) + w01
    ts(0)=ts(0)+w01*(sfct(ix ,iyp,itsfc )     *dtsfc+   &
                     sfct(ix ,iyp,itsfcp)     *dtsfcp)
  end if

  if(istyp10 == 1)then
    if(wgtmin < w10)then
      wgtmin = w10
      vty  = veg_type(ixp,iy ,ntguessfc)
      sty  = soil_type(ixp,iy ,ntguessfc)
    end if
    wgtavg(1) = wgtavg(1) + w10
    ts(1)=ts(1)+w10*(sfct(ixp,iy ,itsfc )     *dtsfc+   &
                     sfct(ixp,iy ,itsfcp)     *dtsfcp)
    vfr  =vfr  +w10*(veg_frac(ixp,iy ,itsfc ) *dtsfc+   &
                     veg_frac(ixp,iy ,itsfcp) *dtsfcp)
    stp  =stp  +w10*(soil_temp(ixp,iy ,itsfc )*dtsfc+   &
                     soil_temp(ixp,iy ,itsfcp)*dtsfcp)
    sm   =sm   +w10*(soil_moi(ixp,iy ,itsfc ) *dtsfc+   &
                     soil_moi(ixp,iy ,itsfcp) *dtsfcp)
  else if(istyp10 == 2)then
    wgtavg(2) = wgtavg(2) + w10
    ts(2)=ts(2)+w10*(sfct(ixp,iy ,itsfc )     *dtsfc+   &
                     sfct(ixp,iy ,itsfcp)     *dtsfcp)
  else if(istyp10 == 3)then
    wgtavg(3) = wgtavg(3) + w10
    ts(3)=ts(3)+w10*(sfct(ixp,iy ,itsfc )     *dtsfc+   &
                     sfct(ixp,iy ,itsfcp)     *dtsfcp)
    sn = sn + w10*sno10
  else 
    wgtavg(0) = wgtavg(0) + w10
    ts(0)=ts(0)+w10*(sfct(ixp,iy ,itsfc )     *dtsfc+   &
                     sfct(ixp,iy ,itsfcp)     *dtsfcp)
  end if

  if(istyp11 == 1)then
    if(wgtmin < w11)then
      wgtmin = w11
      vty  = veg_type(ixp,iyp,ntguessfc)
      sty  = soil_type(ixp,iyp,ntguessfc)
    endif
    wgtavg(1) = wgtavg(1) + w11
    ts(1)=ts(1)+w11*(sfct(ixp,iyp,itsfc )     *dtsfc+   &
                     sfct(ixp,iyp,itsfcp)     *dtsfcp)
    vfr  =vfr  +w11*(veg_frac(ixp,iyp,itsfc ) *dtsfc+   &
                     veg_frac(ixp,iyp,itsfcp) *dtsfcp)
    stp  =stp  +w11*(soil_temp(ixp,iyp,itsfc )*dtsfc+   &
                     soil_temp(ixp,iyp,itsfcp)*dtsfcp)
    sm   =sm   +w11*(soil_moi(ixp,iyp,itsfc ) *dtsfc+   &
                     soil_moi(ixp,iyp,itsfcp) *dtsfcp)
  else if(istyp11 == 2)then
    wgtavg(2) = wgtavg(2) + w11
    ts(2)=ts(2)+w11*(sfct(ixp,iyp,itsfc )     *dtsfc+   &
                     sfct(ixp,iyp,itsfcp)     *dtsfcp)
  else if(istyp11 == 3)then
    wgtavg(3) = wgtavg(3) + w11
    ts(3)=ts(3)+w11*(sfct(ixp,iyp,itsfc )     *dtsfc+   &
                     sfct(ixp,iyp,itsfcp)     *dtsfcp)
    sn = sn + w11*sno11
  else 
    wgtavg(0) = wgtavg(0) + w11
    ts(0)=ts(0)+w11*(sfct(ixp,iyp,itsfc )     *dtsfc+   &
                     sfct(ixp,iyp,itsfcp)     *dtsfcp)
  end if

  tsavg=(sfct(ix,iy ,itsfc )*w00+sfct(ixp,iy ,itsfc )*w10+ &
         sfct(ix,iyp,itsfc )*w01+sfct(ixp,iyp,itsfc )*w11)*dtsfc + &
        (sfct(ix,iy ,itsfcp)*w00+sfct(ixp,iy ,itsfcp)*w10+ &
         sfct(ix,iyp,itsfcp)*w01+sfct(ixp,iyp,itsfcp)*w11)*dtsfcp

  if(wgtavg(0) > zero)then
    ts(0) = ts(0)/wgtavg(0)
  else
    ts(0) = tsavg
  end if
  if(wgtavg(1) > zero)then
    ts(1) = ts(1)/wgtavg(1)
    sm = sm/wgtavg(1)
    vfr = vfr/wgtavg(1)
    stp = stp/wgtavg(1)
  else
    ts(1) = tsavg
    sm=one
  end if
  if(wgtavg(2) > zero)then
    ts(2) = ts(2)/wgtavg(2)
  else
    ts(2) = tsavg
  end if
  if(wgtavg(3) > zero)then
    ts(3) = ts(3)/wgtavg(3)
    sn = sn/wgtavg(3)
  else
    ts(3) = tsavg
  end if
  ts(0)=max(ts(0),270._r_kind)
  ts(2)=min(ts(2),280._r_kind)
  ts(3)=min(ts(3),280._r_kind)

! Space-time interpolation of fields from surface wind speed

  ff10=(fact10(ix,iy ,itsfc )*w00+fact10(ixp,iy ,itsfc )*w10+ &
        fact10(ix,iyp,itsfc )*w01+fact10(ixp,iyp,itsfc )*w11)*dtsfc + &
       (fact10(ix,iy ,itsfcp)*w00+fact10(ixp,iy ,itsfcp)*w10+ &
        fact10(ix,iyp,itsfcp)*w01+fact10(ixp,iyp,itsfcp)*w11)*dtsfcp

! Space-time interpolation of fields from sigma files

! Get time interpolation factors for sigma files
  if(obstime > hrdifsig(1) .and. obstime < hrdifsig(nfldsig))then
     do j=1,nfldsig-1
        if(obstime > hrdifsig(j) .and. obstime <= hrdifsig(j+1))then
           itsig=j
           itsigp=j+1
           dtsig=((hrdifsig(j+1)-obstime)/(hrdifsig(j+1)-hrdifsig(j)))
        end if
     end do
  else if(obstime <=hrdifsig(1))then
     itsig=1
     itsigp=1
     dtsig=one
  else
     itsig=nfldsig
     itsigp=nfldsig
     dtsig=one
  end if
  dtsigp=one-dtsig

  uu5=(ges_u(ix,iy ,1,itsig )*w00+ges_u(ixp,iy ,1,itsig )*w10+ &
       ges_u(ix,iyp,1,itsig )*w01+ges_u(ixp,iyp,1,itsig )*w11)*dtsig + &
      (ges_u(ix,iy ,1,itsigp)*w00+ges_u(ixp,iy ,1,itsigp)*w10+ &
       ges_u(ix,iyp,1,itsigp)*w01+ges_u(ixp,iyp,1,itsigp)*w11)*dtsigp
  vv5=(ges_v(ix,iy ,1,itsig )*w00+ges_v(ixp,iy ,1,itsig )*w10+ &
       ges_v(ix,iyp,1,itsig )*w01+ges_v(ixp,iyp,1,itsig )*w11)*dtsig + &
      (ges_v(ix,iy ,1,itsigp)*w00+ges_v(ixp,iy ,1,itsigp)*w10+ &
       ges_v(ix,iyp,1,itsigp)*w01+ges_v(ixp,iyp,1,itsigp)*w11)*dtsigp


  do k=1,nsig
    h(k)  =(ges_tsen(ix ,iy ,k,itsig )*w00+ &
            ges_tsen(ixp,iy ,k,itsig )*w10+ &
            ges_tsen(ix ,iyp,k,itsig )*w01+ &
            ges_tsen(ixp,iyp,k,itsig )*w11)*dtsig + &
           (ges_tsen(ix ,iy ,k,itsigp)*w00+ &
            ges_tsen(ixp,iy ,k,itsigp)*w10+ &
            ges_tsen(ix ,iyp,k,itsigp)*w01+ &
            ges_tsen(ixp,iyp,k,itsigp)*w11)*dtsigp
    q(k)  =(ges_q(ix ,iy ,k,itsig )*w00+ &
            ges_q(ixp,iy ,k,itsig )*w10+ &
            ges_q(ix ,iyp,k,itsig )*w01+ &
            ges_q(ixp,iyp,k,itsig )*w11)*dtsig + &
           (ges_q(ix ,iy ,k,itsigp)*w00+ &
            ges_q(ixp,iy ,k,itsigp)*w10+ &
            ges_q(ix ,iyp,k,itsigp)*w01+ &
            ges_q(ixp,iyp,k,itsigp)*w11)*dtsigp
    poz(k)=(ges_oz(ix ,iy ,k,itsig )*w00+ &
            ges_oz(ixp,iy ,k,itsig )*w10+ &
            ges_oz(ix ,iyp,k,itsig )*w01+ &
            ges_oz(ixp,iyp,k,itsig )*w11)*dtsig + &
           (ges_oz(ix ,iy ,k,itsigp)*w00+ &
            ges_oz(ixp,iy ,k,itsigp)*w10+ &
            ges_oz(ix ,iyp,k,itsigp)*w01+ &
            ges_oz(ixp,iyp,k,itsigp)*w11)*dtsigp
    prsl(k)=(ges_prsl(ix ,iy ,k,itsig )*w00+ &
             ges_prsl(ixp,iy ,k,itsig )*w10+ &
             ges_prsl(ix ,iyp,k,itsig )*w01+ &
             ges_prsl(ixp,iyp,k,itsig )*w11)*dtsig + &
            (ges_prsl(ix ,iy ,k,itsigp)*w00+ &
             ges_prsl(ixp,iy ,k,itsigp)*w10+ &
             ges_prsl(ix ,iyp,k,itsigp)*w01+ &
             ges_prsl(ixp,iyp,k,itsigp)*w11)*dtsigp

  end do
  do k=1,nsig+1
    prsi(k)=(ges_prsi(ix ,iy ,k,itsig )*w00+ &
             ges_prsi(ixp,iy ,k,itsig )*w10+ &
             ges_prsi(ix ,iyp,k,itsig )*w01+ &
             ges_prsi(ixp,iyp,k,itsig )*w11)*dtsig + &
            (ges_prsi(ix ,iy ,k,itsigp)*w00+ &
             ges_prsi(ixp,iy ,k,itsigp)*w10+ &
             ges_prsi(ix ,iyp,k,itsigp)*w01+ &
             ges_prsi(ixp,iyp,k,itsigp)*w11)*dtsigp
  end do

  return
  end subroutine intrppx
