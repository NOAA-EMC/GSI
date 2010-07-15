module intrppx
! 2010-06-10 Todling turn into module (lightly)
public 
contains
subroutine intrppx1(obstime,h,q,poz,co2,aero,prsl,prsi, &
                   trop5,dtskin,dtsavg,uu5,vv5,dx,dy,&
                   ico2,n_aerosols,iaero)
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
!   2007-12-12  kim - add cloud profiles
!   2008-12-05  todling - use dsfct(:,:,ntguessfc) for calculation
!   2010-04-15  hou - add co2 to output arguments
!   2010-05-19  todling - revisit Hou's implementation of CO2
!   2010-06-10  todling - remove (frequent) CO2 pointer check from inside this routine
!                       - add aerosols capability (also update interface)
!
!   input argument list:
!     obstime  - time of observations for which to get profile
!     dx,dy    - input x,y of interpolation points (grid units)
!     mype     - mpi task id
!
!   output argument list:
!     h        - interpolated temperature
!     q        - interpolated specific humidity
!     poz      - interpolated ozone
!     co2      - interpolated co2 mixing ratio
!     uu5      - interpolated bottom sigma level zonal wind    
!     vv5      - interpolated bottom sigma level meridional wind  
!     trop5    - interpolated tropopause pressure
!     dtskin      - interpolated delta surface temperature
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
!--------
  use kinds, only: r_kind,i_kind
  use mpimod, only: mype
  use guess_grids, only: ges_u,ges_v,ges_tsen,ges_q,ges_oz,&
       ges_prsl,ges_prsi,tropprs,dsfct, &
       hrdifsig,nfldsig,hrdifsfc,nfldsfc,ntguessfc,ges_tv,isli2,sno2
  use ncepgfs_ghg, only: co2vmr_def
  use gsi_chemtracer_mod, only: gsi_chem_bundle   ! for now, a common block
  use gsi_chemtracer_mod, only: gsi_chemtracer_get
  use gridmod, only: istart,jstart,nlon,nlat,nsig,lon1
  use constants, only: zero,one,one_tenth
  implicit none

! Declare passed variables
  real(r_kind)                  ,intent(in   ) :: dx,dy,obstime
  integer(i_kind)               ,intent(in   ) :: n_aerosols
  integer(i_kind),dimension(:)  ,intent(in   ) :: iaero
  integer(i_kind)               ,intent(in   ) :: ico2
  real(r_kind)                  ,intent(  out) :: trop5
  real(r_kind),dimension(nsig)  ,intent(  out) :: h,q,poz,prsl,co2
  real(r_kind),dimension(:,:)   ,intent(  out) :: aero
  real(r_kind),dimension(nsig+1),intent(  out) :: prsi
  real(r_kind)                  ,intent(  out) :: uu5,vv5,dtsavg
  real(r_kind),dimension(0:3)   ,intent(  out) :: dtskin

! Declare local parameters
  real(r_kind),parameter:: minsnow=one_tenth

! Declare local variables  
  integer(i_kind) ier,ii,igfsco2
  integer(i_kind) j,k,m1,ix,ix1,ixp,iy,iy1,iyp
  integer(i_kind) itsig,itsigp,itsfc,itsfcp
  integer(i_kind) istyp00,istyp01,istyp10,istyp11
  real(r_kind) w00,w01,w10,w11
  real(r_kind),dimension(0:3):: wgtavg
  real(r_kind) tv
  real(r_kind) delx,dely,delx1,dely1,dtsig,dtsigp,dtsfc,dtsfcp
  real(r_kind):: sst00,sst01,sst10,sst11
  real(r_kind):: sno00,sno01,sno10,sno11


! Get information for how to use CO2
  igfsco2=0
  if (ico2>0) then
     call gsi_chemtracer_get ( 'i4crtm::co2', igfsco2, ier )
  endif

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

  trop5= tropprs(ix,iy )*w00+tropprs(ixp,iy )*w10+ &
         tropprs(ix,iyp)*w01+tropprs(ixp,iyp)*w11

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

!    Set surface type flag.  (Same logic as in subroutine deter_sfc)
  istyp00 = isli2(ix ,iy )
  istyp10 = isli2(ixp,iy )
  istyp01 = isli2(ix ,iyp)
  istyp11 = isli2(ixp,iyp)
  sno00= sno2(ix ,iy ,itsfc)*dtsfc+sno2(ix ,iy ,itsfcp)*dtsfcp
  sno01= sno2(ix ,iyp,itsfc)*dtsfc+sno2(ix ,iyp,itsfcp)*dtsfcp
  sno10= sno2(ixp,iy ,itsfc)*dtsfc+sno2(ixp,iy ,itsfcp)*dtsfcp
  sno11= sno2(ixp,iyp,itsfc)*dtsfc+sno2(ixp,iyp,itsfcp)*dtsfcp
  if(istyp00 >= 1 .and. sno00 > minsnow)istyp00 = 3
  if(istyp01 >= 1 .and. sno01 > minsnow)istyp01 = 3
  if(istyp10 >= 1 .and. sno10 > minsnow)istyp10 = 3
  if(istyp11 >= 1 .and. sno11 > minsnow)istyp11 = 3

  sst00= dsfct(ix ,iy,ntguessfc) ; sst01= dsfct(ix ,iyp,ntguessfc)
  sst10= dsfct(ixp,iy,ntguessfc) ; sst11= dsfct(ixp,iyp,ntguessfc) 
  dtsavg=sst00*w00+sst10*w10+sst01*w01+sst11*w11

  dtskin(0:3)=zero
  wgtavg(0:3)=zero

  if(istyp00 == 1)then
     wgtavg(1) = wgtavg(1) + w00
     dtskin(1)=dtskin(1)+w00*sst00
  else if(istyp00 == 2)then
     wgtavg(2) = wgtavg(2) + w00
     dtskin(2)=dtskin(2)+w00*sst00
  else if(istyp00 == 3)then
     wgtavg(3) = wgtavg(3) + w00
     dtskin(3)=dtskin(3)+w00*sst00
  else
     wgtavg(0) = wgtavg(0) + w00
     dtskin(0)=dtskin(0)+w00*sst00
  end if

  if(istyp01 == 1)then
     wgtavg(1) = wgtavg(1) + w01
     dtskin(1)=dtskin(1)+w01*sst01
  else if(istyp01 == 2)then
     wgtavg(2) = wgtavg(2) + w01
     dtskin(2)=dtskin(2)+w01*sst01
  else if(istyp01 == 3)then
     wgtavg(3) = wgtavg(3) + w01
     dtskin(3)=dtskin(3)+w01*sst01
  else
     wgtavg(0) = wgtavg(0) + w01
     dtskin(0)=dtskin(0)+w01*sst01
  end if

  if(istyp10 == 1)then
     wgtavg(1) = wgtavg(1) + w10
     dtskin(1)=dtskin(1)+w10*sst10
  else if(istyp10 == 2)then
     wgtavg(2) = wgtavg(2) + w10
     dtskin(2)=dtskin(2)+w10*sst10
  else if(istyp10 == 3)then
     wgtavg(3) = wgtavg(3) + w10
     dtskin(3)=dtskin(3)+w10*sst10
  else
     wgtavg(0) = wgtavg(0) + w10
     dtskin(0)=dtskin(0)+w10*sst10
  end if

  if(istyp11 == 1)then
     wgtavg(1) = wgtavg(1) + w11
     dtskin(1)=dtskin(1)+w11*sst11
  else if(istyp11 == 2)then
     wgtavg(2) = wgtavg(2) + w11
     dtskin(2)=dtskin(2)+w11*sst11
  else if(istyp11 == 3)then
     wgtavg(3) = wgtavg(3) + w11
     dtskin(3)=dtskin(3)+w11*sst11
  else
     wgtavg(0) = wgtavg(0) + w11
     dtskin(0)=dtskin(0)+w11*sst11
  end if

  if(wgtavg(0) > zero)then
     dtskin(0) = dtskin(0)/wgtavg(0)
  else
     dtskin(0) = dtsavg
  end if
  if(wgtavg(1) > zero)then
     dtskin(1) = dtskin(1)/wgtavg(1)
  else
     dtskin(1) = dtsavg
  end if
  if(wgtavg(2) > zero)then
     dtskin(2) = dtskin(2)/wgtavg(2)
  else
     dtskin(2) = dtsavg
  end if
  if(wgtavg(3) > zero)then
     dtskin(3) = dtskin(3)/wgtavg(3)
  else
     dtskin(3) = dtsavg
  end if

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
     tv    =(ges_tv(ix ,iy ,k,itsig )*w00+ &
             ges_tv(ixp,iy ,k,itsig )*w10+ &
             ges_tv(ix ,iyp,k,itsig )*w01+ &
             ges_tv(ixp,iyp,k,itsig )*w11)*dtsig + &
            (ges_tv(ix ,iy ,k,itsigp)*w00+ &
             ges_tv(ixp,iy ,k,itsigp)*w10+ &
             ges_tv(ix ,iyp,k,itsigp)*w01+ &
             ges_tv(ixp,iyp,k,itsigp)*w11)*dtsigp

     if(ico2>0.and.igfsco2/=0)then
        if(size(gsi_chem_bundle)==1) then
           co2(k) =(gsi_chem_bundle(1)%r3(ico2)%q(ix ,iy ,k)*w00+ &
                    gsi_chem_bundle(1)%r3(ico2)%q(ixp,iy ,k)*w10+ &
                    gsi_chem_bundle(1)%r3(ico2)%q(ix ,iyp,k)*w01+ &
                    gsi_chem_bundle(1)%r3(ico2)%q(ixp,iyp,k)*w11)
        else
           co2(k) =(gsi_chem_bundle(itsig )%r3(ico2)%q(ix ,iy ,k)*w00+ &
                    gsi_chem_bundle(itsig )%r3(ico2)%q(ixp,iy ,k)*w10+ &
                    gsi_chem_bundle(itsig )%r3(ico2)%q(ix ,iyp,k)*w01+ &
                    gsi_chem_bundle(itsig )%r3(ico2)%q(ixp,iyp,k)*w11)*dtsig + &
                   (gsi_chem_bundle(itsigp)%r3(ico2)%q(ix ,iy ,k)*w00+ &
                    gsi_chem_bundle(itsigp)%r3(ico2)%q(ixp,iy ,k)*w10+ &
                    gsi_chem_bundle(itsigp)%r3(ico2)%q(ix ,iyp,k)*w01+ &
                    gsi_chem_bundle(itsigp)%r3(ico2)%q(ixp,iyp,k)*w11)*dtsigp
        endif
     else
        co2(k) = co2vmr_def
     endif

     if(n_aerosols>0)then
        if(size(gsi_chem_bundle)==1) then
           do ii=1,n_aerosols
              aero(k,ii) =(gsi_chem_bundle(1)%r3(iaero(ii))%q(ix ,iy ,k)*w00+ &
                           gsi_chem_bundle(1)%r3(iaero(ii))%q(ixp,iy ,k)*w10+ &
                           gsi_chem_bundle(1)%r3(iaero(ii))%q(ix ,iyp,k)*w01+ &
                           gsi_chem_bundle(1)%r3(iaero(ii))%q(ixp,iyp,k)*w11)
           enddo
        else
           do ii=1,n_aerosols
              aero(k,ii) =(gsi_chem_bundle(itsig )%r3(iaero(ii))%q(ix ,iy ,k)*w00+ &
                           gsi_chem_bundle(itsig )%r3(iaero(ii))%q(ixp,iy ,k)*w10+ &
                           gsi_chem_bundle(itsig )%r3(iaero(ii))%q(ix ,iyp,k)*w01+ &
                           gsi_chem_bundle(itsig )%r3(iaero(ii))%q(ixp,iyp,k)*w11)*dtsig + &
                          (gsi_chem_bundle(itsigp)%r3(iaero(ii))%q(ix ,iy ,k)*w00+ &
                           gsi_chem_bundle(itsigp)%r3(iaero(ii))%q(ixp,iy ,k)*w10+ &
                           gsi_chem_bundle(itsigp)%r3(iaero(ii))%q(ix ,iyp,k)*w01+ &
                           gsi_chem_bundle(itsigp)%r3(iaero(ii))%q(ixp,iyp,k)*w11)*dtsigp
           enddo
        endif
     endif

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
  end subroutine intrppx1
  end module intrppx
