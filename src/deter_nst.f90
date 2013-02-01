subroutine deter_nst(dlat_earth,dlon_earth,obstime,zob,tref,dtw,dtc,tz_tr)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    deter_nst                     determine NSST variable at observation location over water
!   prgmmr: Xu Li           org: np2                date: 2011-04-08
!
! abstract:  determines NSST variables over water surface type based on surrounding surface types
!
! program history log:
!   2011-04-08 Li
!   2013-01-23  parrish - change from grdcrd to grdcrd1 (to allow successful debug compile on WCOSS)
!
!   input argument list:
!     obstime                             - observation time relative to analysis time
!     dlat_earth                          - earth latitude in radians
!     dlon_earth                          - earth longitude in radians
!     zob                                 - obs. depth in the water
!
!   output argument list:
!     tref                                - oceanic foundation temperature
!      dtw                                - diurnal warming at depth zob
!      dtc                                - sublayer cooling at depth zob
!    tz_tr                                - d(Tz)/d(tr)
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
     use kinds, only: r_kind,i_kind
     use constants, only: zero,one,z_w_max
     use satthin, only: isli_full,tref_full,dt_cool_full,z_c_full,dt_warm_full,z_w_full,c_0_full,c_d_full,w_0_full,w_d_full
     use gridmod, only: nlat,nlon,regional,tll2xy,nlat_sfc,nlon_sfc,rlats_sfc,rlons_sfc
     use guess_grids, only: nfldnst,hrdifnst
     use radinfo, only: fac_dtl,fac_tsl
     implicit none

     real(r_kind), intent(in ) :: dlat_earth,dlon_earth,obstime,zob
     real(r_kind), intent(out) :: tref,dtw,dtc,tz_tr

!    local variables
     real(r_kind):: dt_cool,z_c,dt_warm,z_w,c_0,c_d,w_0,w_d
     integer(i_kind) istyp00,istyp01,istyp10,istyp11
     integer(i_kind):: itnst,itnstp
     integer(i_kind):: ix,iy,ixp,iyp,j,i,k
     real(r_kind):: dx,dy,dx1,dy1,w00,w10,w01,w11,dtnst,dtnstp,wgtmin
     real(r_kind):: tref_00,tref_01,tref_10,tref_11,tr_tmp
     real(r_kind):: dt_cool_00,dt_cool_01,dt_cool_10,dt_cool_11
     real(r_kind):: z_c_00,z_c_01,z_c_10,z_c_11
     real(r_kind):: dt_warm_00,dt_warm_01,dt_warm_10,dt_warm_11
     real(r_kind):: z_w_00,z_w_01,z_w_10,z_w_11,z_w_tmp
     real(r_kind):: c_0_00,c_0_01,c_0_10,c_0_11
     real(r_kind):: c_d_00,c_d_01,c_d_10,c_d_11
     real(r_kind):: w_0_00,w_0_01,w_0_10,w_0_11
     real(r_kind):: w_d_00,w_d_01,w_d_10,w_d_11
     real(r_kind):: wgtavg,dlat,dlon
     logical outside


     if(regional)then
        call tll2xy(dlon_earth,dlat_earth,dlon,dlat,outside)
     else
        dlat=dlat_earth
        dlon=dlon_earth
        call grdcrd1(dlat,rlats_sfc,nlat_sfc,1)
        call grdcrd1(dlon,rlons_sfc,nlon_sfc,1)
     end if

     iy=int(dlon); ix=int(dlat)
     dy  =dlon-iy; dx  =dlat-ix
     dx1 =one-dx;    dy1 =one-dy
     w00=dx1*dy1; w10=dx*dy1; w01=dx1*dy; w11=one-w00-w10-w01

     ix=min(max(1,ix),nlat_sfc); iy=min(max(0,iy),nlon_sfc)
     ixp=min(nlat_sfc,ix+1); iyp=iy+1
     if(iy==0) iy=nlon_sfc
     if(iyp==nlon_sfc+1) iyp=1

!    Get time interpolation factors for nst files
     if(obstime > hrdifnst(1) .and. obstime <= hrdifnst(nfldnst))then
        do j=1,nfldnst-1
           if(obstime > hrdifnst(j) .and. obstime <= hrdifnst(j+1))then
              itnst=j
              itnstp=j+1
              dtnst=(hrdifnst(j+1)-obstime)/(hrdifnst(j+1)-hrdifnst(j))
           end if
        end do
     else if(obstime <=hrdifnst(1))then
        itnst=1
        itnstp=1
        dtnst=one
     else
        itnst=nfldnst
        itnstp=nfldnst
        dtnst=one
     end if
     dtnstp=one-dtnst

!    Set surface type flag.

     istyp00 = isli_full(ix ,iy )
     istyp10 = isli_full(ixp,iy )
     istyp01 = isli_full(ix ,iyp)
     istyp11 = isli_full(ixp,iyp)
!
!    Use the time interpolation factors for nst files
!
     tref_00    = tref_full   (ix ,iy ,itnst)*dtnst + tref_full   (ix ,iy ,itnstp)*dtnstp
     tref_01    = tref_full   (ix ,iyp,itnst)*dtnst + tref_full   (ix ,iyp,itnstp)*dtnstp
     tref_10    = tref_full   (ixp,iy ,itnst)*dtnst + tref_full   (ixp,iy ,itnstp)*dtnstp
     tref_11    = tref_full   (ixp,iyp,itnst)*dtnst + tref_full   (ixp,iyp,itnstp)*dtnstp

     dt_cool_00 = dt_cool_full(ix ,iy ,itnst)*dtnst + dt_cool_full(ix ,iy ,itnstp)*dtnstp
     dt_cool_01 = dt_cool_full(ix ,iyp,itnst)*dtnst + dt_cool_full(ix ,iyp,itnstp)*dtnstp
     dt_cool_10 = dt_cool_full(ixp,iy ,itnst)*dtnst + dt_cool_full(ixp,iy ,itnstp)*dtnstp
     dt_cool_11 = dt_cool_full(ixp,iyp,itnst)*dtnst + dt_cool_full(ixp,iyp,itnstp)*dtnstp

     z_c_00     = z_c_full    (ix ,iy ,itnst)*dtnst + z_c_full    (ix ,iy ,itnstp)*dtnstp
     z_c_01     = z_c_full    (ix ,iyp,itnst)*dtnst + z_c_full    (ix ,iyp,itnstp)*dtnstp
     z_c_10     = z_c_full    (ixp,iy ,itnst)*dtnst + z_c_full    (ixp,iy ,itnstp)*dtnstp
     z_c_11     = z_c_full    (ixp,iyp,itnst)*dtnst + z_c_full    (ixp,iyp,itnstp)*dtnstp

     dt_warm_00 = dt_warm_full(ix ,iy ,itnst)*dtnst + dt_warm_full(ix ,iy ,itnstp)*dtnstp
     dt_warm_01 = dt_warm_full(ix ,iyp,itnst)*dtnst + dt_warm_full(ix ,iyp,itnstp)*dtnstp
     dt_warm_10 = dt_warm_full(ixp,iy ,itnst)*dtnst + dt_warm_full(ixp,iy ,itnstp)*dtnstp
     dt_warm_11 = dt_warm_full(ixp,iyp,itnst)*dtnst + dt_warm_full(ixp,iyp,itnstp)*dtnstp

     z_w_00     = z_w_full    (ix ,iy ,itnst)*dtnst + z_w_full    (ix ,iy ,itnstp)*dtnstp
     z_w_01     = z_w_full    (ix ,iyp,itnst)*dtnst + z_w_full    (ix ,iyp,itnstp)*dtnstp
     z_w_10     = z_w_full    (ixp,iy ,itnst)*dtnst + z_w_full    (ixp,iy ,itnstp)*dtnstp
     z_w_11     = z_w_full    (ixp,iyp,itnst)*dtnst + z_w_full    (ixp,iyp,itnstp)*dtnstp

     c_0_00     = c_0_full    (ix ,iy ,itnst)*dtnst + c_0_full    (ix ,iy ,itnstp)*dtnstp
     c_0_01     = c_0_full    (ix ,iyp,itnst)*dtnst + c_0_full    (ix ,iyp,itnstp)*dtnstp
     c_0_10     = c_0_full    (ixp,iy ,itnst)*dtnst + c_0_full    (ixp,iy ,itnstp)*dtnstp
     c_0_11     = c_0_full    (ixp,iyp,itnst)*dtnst + c_0_full    (ixp,iyp,itnstp)*dtnstp

     c_d_00     = c_d_full    (ix ,iy ,itnst)*dtnst + c_d_full    (ix ,iy ,itnstp)*dtnstp
     c_d_01     = c_d_full    (ix ,iyp,itnst)*dtnst + c_d_full    (ix ,iyp,itnstp)*dtnstp
     c_d_10     = c_d_full    (ixp,iy ,itnst)*dtnst + c_d_full    (ixp,iy ,itnstp)*dtnstp
     c_d_11     = c_d_full    (ixp,iyp,itnst)*dtnst + c_d_full    (ixp,iyp,itnstp)*dtnstp

     w_0_00     = w_0_full    (ix ,iy ,itnst)*dtnst + w_0_full    (ix ,iy ,itnstp)*dtnstp
     w_0_01     = w_0_full    (ix ,iyp,itnst)*dtnst + w_0_full    (ix ,iyp,itnstp)*dtnstp
     w_0_10     = w_0_full    (ixp,iy ,itnst)*dtnst + w_0_full    (ixp,iy ,itnstp)*dtnstp
     w_0_11     = w_0_full    (ixp,iyp,itnst)*dtnst + w_0_full    (ixp,iyp,itnstp)*dtnstp

     w_d_00     = w_d_full    (ix ,iy ,itnst)*dtnst + w_d_full    (ix ,iy ,itnstp)*dtnstp
     w_d_01     = w_d_full    (ix ,iyp,itnst)*dtnst + w_d_full    (ix ,iyp,itnstp)*dtnstp
     w_d_10     = w_d_full    (ixp,iy ,itnst)*dtnst + w_d_full    (ixp,iy ,itnstp)*dtnstp
     w_d_11     = w_d_full    (ixp,iyp,itnst)*dtnst + w_d_full    (ixp,iyp,itnstp)*dtnstp

!    Interpolate nst variables to obs location (water surface only)

     wgtavg  = zero
     tr_tmp  = zero
     dt_cool = zero
     z_c     = zero
     dt_warm = zero
     z_w_tmp = zero
     c_0     = zero
     c_d     = zero
     w_0     = zero
     w_d     = zero

     if (istyp00 == 0)then
        wgtavg  = wgtavg  + w00
        tr_tmp  = tr_tmp  + w00*tref_00
        dt_cool = dt_cool + w00*dt_cool_00
        z_c     = z_c     + w00*z_c_00
        dt_warm = dt_warm + w00*dt_warm_00
        z_w_tmp = z_w_tmp + w00*z_w_00
        c_0     = c_0     + w00*c_0_00
        c_d     = c_d     + w00*c_d_00
        w_0     = w_0     + w00*w_0_00
        w_d     = w_d     + w00*w_d_00
     endif
     if(istyp01 == 0)then
        wgtavg  = wgtavg  + w01
        tr_tmp  = tr_tmp  + w01*tref_01
        dt_cool = dt_cool + w01*dt_cool_01
        z_c     = z_c     + w01*z_c_01
        dt_warm = dt_warm + w01*dt_warm_01
        z_w_tmp = z_w_tmp + w01*z_w_01
        c_0     = c_0     + w01*c_0_01
        c_d     = c_d     + w01*c_d_01
        w_0     = w_0     + w01*w_0_01
        w_d     = w_d     + w01*w_d_01
     end if
     if(istyp10 == 0)then
        wgtavg  = wgtavg  + w10
        tr_tmp  = tr_tmp  + w10*tref_10
        dt_cool = dt_cool + w10*dt_cool_10
        z_c     = z_c     + w10*z_c_10
        dt_warm = dt_warm + w10*dt_warm_10
        z_w_tmp = z_w_tmp + w10*z_w_10
        c_0     = c_0     + w10*c_0_10
        c_d     = c_d     + w10*c_d_10
        w_0     = w_0     + w10*w_0_10
        w_d     = w_d     + w10*w_d_10
     end if
     if(istyp11 == 0)then
        wgtavg  = wgtavg  + w11
        tr_tmp  = tr_tmp  + w11*tref_11
        dt_cool = dt_cool + w11*dt_cool_11
        z_c     = z_c     + w11*z_c_11
        dt_warm = dt_warm + w11*dt_warm_11
        z_w_tmp = z_w_tmp + w11*z_w_11
        c_0     = c_0     + w11*c_0_11
        c_d     = c_d     + w11*c_d_11
        w_0     = w_0     + w11*w_0_11
        w_d     = w_d     + w11*w_d_11
     end if

     if(wgtavg > zero)then
        tr_tmp  = tr_tmp/wgtavg
        tref    = tr_tmp

        z_w_tmp = z_w_tmp/wgtavg
        z_w     = z_w_tmp

        dt_cool = dt_cool/wgtavg
        z_c     = z_c/wgtavg
        dt_warm = dt_warm/wgtavg
        c_0     = c_0/wgtavg
        c_d     = c_d/wgtavg
        w_0     = w_0/wgtavg
        w_d     = w_d/wgtavg

        dtw = fac_dtl*dt_warm*(one-min(zob,z_w)/z_w)
        if ( z_c > zero ) then
          dtc = fac_tsl*dt_cool*(one-min(zob,z_c)/z_c)
        else
          dtc = zero
        endif

        call cal_tztr(dt_warm,c_0,c_d,w_0,w_d,z_c,z_w,zob,tz_tr)

     end if
end subroutine deter_nst

