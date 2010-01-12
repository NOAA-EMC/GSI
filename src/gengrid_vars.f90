subroutine gengrid_vars
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    gengrid_vars
!   prgmmr: treadon          org: np23                date: 2003-11-24 
!
! abstract: initialize and define grid related variables
!
! program history log:
!   2003-11-24  treadon
!   2004-05-13  kleist, documentation and cleanup
!   2004-08-04  treadon - add only on use declarations; add intent in/out
!   2006-04-12  treadon - remove nsig,sigl (not used)
!   2006-10-17  kleist  - add coriolis parameter
!   2010-01-12  treadon - add hires_b section
!
!   input argument list:
!
!   output argument list:
!
! remarks:  see modules used
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,i_kind
  use gridmod, only: sinlon,coslon,region_lat,rbs2,&
       rlons,rlats,corlats,nlon,nlat,regional,wgtlats,&
       sinlon_b,coslon_b,rbs2_b,&
       rlons_b,rlats_b,corlats_b,nlon_b,nlat_b,wgtlats_b,&
       compute_b2a,hires_b
  use specmod, only: slat,wlat,jb,je,slat_b,wlat_b,jb_b,je_b
  use constants, only: ione,zero,half,one,four,pi,two,omega
  implicit none

! Declare local variables
  integer(i_kind) i,i1
  real(r_kind) anlon,dlon,pih

  if(regional) then
! This is regional run, so transfer previously defined regional lats, lons
    do i=1,nlon
       rlons(i)=i
    end do

    do i=1,nlat
       rlats(i)=i
    end do

    i1=nlon/4
    do i=1,nlat
      wgtlats(i)=zero
      rbs2(i)=one/cos(region_lat(i,i1))**2
    end do

  else

! This is global run, so get global lons, lats, wgtlats

! Set local constants
    anlon=float(nlon)
    pih=half*pi
    dlon=two*pi/anlon

! Load grid lat,lon arrays.  rbs2 is used in pcp.
    do i=1,nlon
      rlons(i)=float(i-ione)*dlon
      coslon(i)=cos(rlons(i))
      sinlon(i)=sin(rlons(i))
    end do

    do i=jb,je
       i1=i+ione
       rlats(i1)=-asin(slat(i))
       rbs2(i1)=one/cos(rlats(i1))**2
       wgtlats(i1)=wlat(i)

       i1=nlat-i
       rlats(i1)=asin(slat(i))
       rbs2(i1)=one/cos(rlats(i1))**2
       wgtlats(i1)=wlat(i)
    end do

    rlats(1)=-pih
    rlats(nlat)=pih
   
    wgtlats(1)=zero
    wgtlats(nlat)=zero

    rbs2(1)=zero
    rbs2(nlat)=zero

    do i=1,nlat
      corlats(i)=two*omega*sin(rlats(i))
    end do


!   Repeat calculations, if necessary, for background grid.
    if (hires_b) then

!      Set local constants
       anlon=float(nlon_b)
       dlon=two*pi/anlon

!      Load grid lat,lon arrays.  rbs2 is used in pcp.
       do i=1,nlon_b
          rlons_b(i)=float(i-ione)*dlon
          coslon_b(i)=cos(rlons_b(i))
          sinlon_b(i)=sin(rlons_b(i))
       end do
       
       do i=jb_b,je_b
          i1=i+ione
          rlats_b(i1)=-asin(slat_b(i))
          rbs2_b(i1)=one/cos(rlats_b(i1))**2
          wgtlats_b(i1)=wlat_b(i)
          
          i1=nlat_b-i
          rlats_b(i1)=asin(slat_b(i))
          rbs2_b(i1)=one/cos(rlats_b(i1))**2
          wgtlats_b(i1)=wlat_b(i)
       end do
       
       rlats_b(1)=-pih
       rlats_b(nlat_b)=pih
       
       wgtlats_b(1)=zero
       wgtlats_b(nlat_b)=zero
       
       rbs2_b(1)=zero
       rbs2_b(nlat_b)=zero

       do i=1,nlat_b
          corlats_b(i)=two*omega*sin(rlats_b(i))
       end do

!      Compute interplation weights and ij indexes between
!      background and analysis grid
       call compute_b2a

    endif

  end if  !end if global

  return
end subroutine gengrid_vars
