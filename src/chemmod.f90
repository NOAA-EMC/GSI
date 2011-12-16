module chemmod

!   prgmmr: pagowski                date: 2010-09-13
! module contains functions
! 1) to convert units between obs and model function
!obs2model_anowbufr_pm2_5
! 2) allocate and assign initial values to pm2_5 guess
!init_pm2_5_guess
! 3) declare/assign &CHEM namelist parameters 
!init_chem
! subroutine oneobschem
! for testing one observation assimilation

! NB: keep aerosol names capital for consistency with cmaq output names
! 2011-09-09 pagowski - add codes for PM2.5 for prepbufr and bufr dump files


  use kinds, only : i_kind, r_kind, r_single
  use gridmod, only: cmaq_regional,wrf_mass_regional,lat2,lon2,nsig
  use constants, only : tiny_single,max_varname_length

  implicit none

  private

  public :: obs2model_anowbufr_pm2_5,cmaq_pm2_5,wrf_chem_pm2_5,&
        cmaq_o3,wrf_chem_o3
  public :: iconc,ierror,ilat,ilon,itime,iid,ielev,isite,iikx,ilate,ilone

  public :: elev_tolerance,elev_missing,pm2_5_teom_max
  public :: ngrid1d_cmaq,ngrid2d_cmaq,nmet2d_cmaq,nmet3d_cmaq,&
        naero_cmaq,aeronames_cmaq

  public :: pm2_5_guess,init_pm2_5_guess
  public :: init_chem
  public :: berror_chem,oneobtest_chem,maginnov_chem,magoberr_chem,oneob_type_chem,conconeobs
  public :: oblat_chem,oblon_chem,obpres_chem,diag_incr,oneobschem
  public :: site_scale,nsites
  public :: tunable_error
  public :: in_fname,out_fname,incr_fname,maxstr
  public :: code_pm25_bufr,code_pm25_prepbufr

  logical :: oneobtest_chem,diag_incr,berror_chem
  character(len=max_varname_length) :: oneob_type_chem
  integer(i_kind), parameter :: maxstr=256
  real(r_kind) :: maginnov_chem,magoberr_chem,conconeobs,&
        oblon_chem,oblat_chem,obpres_chem,elev_tolerance,tunable_error
  
  integer(i_kind), parameter :: code_pm25_bufr=11, code_pm25_prepbufr=102
  

  real(r_kind),parameter :: pm2_5_teom_max=200_r_kind !ug/m3
!some parameters need to be put here since convinfo file won't
!accomodate, stands for maximum realistic value of surface pm2.5

  real(r_kind),parameter :: elev_missing=-9999_r_kind
!when elevation of the obs site is missing assign that

  integer(i_kind), parameter :: nsites=4

!character of sites:
! 1 - unknown
! 2 - urban
! 3 - suburban
! 4 - rural
! depending on the character of measurement site  observation
! error is assigned - see read_anowbufr.f90 for error calculation
!error_2=tunable_error*error_1*sqrt(dx/site_scale)

  real(r_kind), dimension (nsites), parameter:: &
        site_scale= (/3000._r_kind,2000._r_kind,&
        4000._r_kind,10000._r_kind/)

!conversion ratios between obs and model units
  real(r_kind),parameter :: kg2ug=1.e+9_r_kind,ppbv2ppmv=0.001_r_kind
  real(r_kind),parameter :: cmaq_pm2_5=kg2ug,wrf_chem_pm2_5=kg2ug,&
        cmaq_o3=ppbv2ppmv,wrf_chem_o3=ppbv2ppmv

!position of obs parameters in obs output file record
  integer(i_kind), parameter :: &
        iconc = 1,ierror= 2,&
        ilat  = 3,ilon  = 4,&
        itime = 5,iid=6,&
        ielev = 7,isite = 8,&
        iikx  = 9,ilate = 10,&
        ilone =11
  
!parameters for erading cmaq input file
  integer(i_kind), parameter :: &
        ngrid1d_cmaq=2,  &     !aeta1,eta1
        ngrid2d_cmaq=4,  &     !lat,lon,dx_mc,dy_mc
        nmet2d_cmaq=2, &       !ht,psfc
        nmet3d_cmaq=4,  &  !t,qv,u,v
        naero_cmaq=16  !number of cmaq aerosol species
  
  character(len=max_varname_length), dimension(naero_cmaq), parameter :: &
        aeronames_cmaq=(/&
        'ASO4I     ','ASO4J     ','ANH4I     ','ANH4J     ',&
        'ANO3I     ','ANO3J     ','AORGAI    ','AORGAJ    ',&
        'AORGPAI   ','AORGPAJ   ','AORGBI    ','AORGBJ    ',&
        'AECI      ','AECJ      ','A25I      ','A25J      '/)

  real(r_single), allocatable, dimension(:,:,:) :: pm2_5_guess

  character(len=maxstr) :: in_fname,out_fname,incr_fname
  
contains

  function obs2model_anowbufr_pm2_5()

!   prgmmr: pagowski                date: 2010-09-13

! assigns conversion factors between AIRNow pm2.5 units and model units

    real(r_kind) :: obs2model_anowbufr_pm2_5
    
    if (cmaq_regional) then 
       obs2model_anowbufr_pm2_5=cmaq_pm2_5
    elseif (wrf_mass_regional) then
       obs2model_anowbufr_pm2_5=wrf_chem_pm2_5
    else
       write(6,*)'unknown chem model. stopping'
       call stop2(414)
    endif
    
  end function obs2model_anowbufr_pm2_5

  subroutine init_pm2_5_guess

!   prgmmr: pagowski                date: 2010-09-13
!   allocates and assigns initial values to pm2_5_guess
    integer(i_kind) :: i,j,k

    allocate(pm2_5_guess(lat2,lon2,nsig))

    do i=1,lon2
       do j=1,lat2
          do k=1,nsig
             pm2_5_guess(j,i,k)=tiny_single
          enddo
       enddo
    enddo
    
  end subroutine init_pm2_5_guess

  subroutine init_chem

!   prgmmr: pagowski                date: 2010-09-13

!initialiazes default values to &CHEM namelist parameters
    
    berror_chem=.false.
    oneobtest_chem=.false.
    maginnov_chem=30_r_kind
    magoberr_chem=2_r_kind
    oneob_type_chem='pm2_5'
    oblat_chem=45_r_kind
    oblon_chem=270_r_kind
    obpres_chem=1000_r_kind
    diag_incr=.false.
    elev_tolerance=500_r_kind
    tunable_error=0.5_r_kind
    in_fname='cmaq_input.bin'
    out_fname='cmaq_output.bin'
    incr_fname='chem_increment.bin'

  end subroutine init_chem

  subroutine oneobschem(nread,ndata,nodata,gstime,&
        infile,obstype,lunout,sis)

!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:  one obs test for in-situ chem
!   prgmmr: pagowski                date: 2010-11-18
!
! program history log:
!   2010-11-18  pagowski
!
!   input argument list:
!     obstype  - observation type to process
!     lunout   - unit to which to write data for further processing
!
!   output argument list:
!     nread    - number of type "obstype" observations read
!     ndata    - number of type "obstype" observations retained for further processing
!     nodata   - number of individual "obstype" observations retained for !further processing
!     sis      - satellite/instrument/sensor indicator
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$

    use constants, only: zero,one,deg2rad,rad2deg
    use gridmod, only: diagnostic_reg,regional,nlon,nlat,&
         tll2xy,txy2ll,rlats,rlons
    use convinfo, only: nconvtype,icuse,ioctype
    
    implicit none
    
! declare passed variables
    character(len=*),intent(in   ) :: obstype
    character(len=*),intent(out) :: infile
    integer(i_kind) ,intent(in   ) :: lunout
    integer(i_kind) ,intent(inout) :: nread,ndata,nodata
    real(r_kind)    ,intent(in   ) :: gstime
    character(len=*),intent(in   ) :: sis
    
    
! declare local parameters
    
    integer(i_kind), parameter :: nsid=1,nxob=2,&
          nyob=3,ndhr=4,ntyp=5,ncopopm=6
!see headr input format below
    
    integer(i_kind), parameter:: nchanl=0,nreal=ilone
    
    real(r_kind),parameter :: r100 = 100.0_r_kind
    real(r_kind),parameter :: r360 = 360.0_r_kind
    
! declare local variables
    logical outside
    
    integer(i_kind) lunin,i
    integer(i_kind) ikx
    
    real(r_kind) :: tdiff,obstime
    real(r_kind) :: dlat,dlon,obserror,dlat_earth,dlon_earth
    
    real(r_kind) cdist,disterr,disterrmax,rlon00,rlat00
    integer(i_kind) ntest
    
    integer(i_kind) k,site_char,site_id
    real(r_kind) :: conc,site_elev
    real(r_kind), dimension(nreal,1):: cdata_all
    
    data lunin / 10 /
    
    site_id=123456789
    site_char=1 ! set unknown site character
    site_elev=elev_missing ! set unknown site elevation
    
!**************************************************************************
! initialize variables
    infile='namelist'
    disterrmax=zero
    ntest=1
    nread=1
    ndata = 1
    nodata = 1
    
    if(oblon_chem >= r360)  oblon_chem = oblon_chem - r360
    if(oblon_chem <  zero)  oblon_chem = oblon_chem + r360
    
    dlon_earth=oblon_chem*deg2rad
    dlat_earth=oblat_chem*deg2rad
    obstime=gstime
    tdiff=zero
    conc=zero ! this is unimportant since only innovation counts
  
    if(regional)then
       
       call tll2xy(dlon_earth,dlat_earth,dlon,dlat,outside)    ! convert to rotated coordinate
       
       if(diagnostic_reg) then
          
          call txy2ll(dlon,dlat,rlon00,rlat00)
          cdist=sin(dlat_earth)*sin(rlat00)+cos(dlat_earth)*cos(rlat00)* &
               (sin(dlon_earth)*sin(rlon00)+cos(dlon_earth)*cos(rlon00))
          cdist=max(-one,min(cdist,one))
          disterr=acos(cdist)*rad2deg
          disterrmax=max(disterrmax,disterr)
       end if
       
       if(outside) then 
          write(6,*)'oneobtest_chem outside domain - stopping'
          call stop2(511)
       endif
       
    else
       dlat = dlat_earth
       dlon = dlon_earth
       call grdcrd(dlat,1,rlats,nlat,1)
       call grdcrd(dlon,1,rlons,nlon,1)
    endif
    
    do i = 1, nconvtype
       if (obstype == ioctype(i) .and. abs(icuse(i))== 1) ikx=i
    end do
           
    obserror=magoberr_chem
    
    cdata_all(iconc,ndata)  = conc                    ! pm2_5 obs     
    cdata_all(ierror,ndata) = obserror                ! pm2_5 obs error
    cdata_all(ilat,ndata)   = dlat                    ! grid relative latitude 
    
    cdata_all(ilon,ndata)   = dlon                    ! grid relative longitude 
    cdata_all(itime,ndata)  = obstime                 ! time of obs
    cdata_all(iid,ndata)    = site_id                 ! site id
    cdata_all(ielev,ndata)  = site_elev               ! elevation
    cdata_all(isite,ndata)  = site_char               ! site character
    cdata_all(iikx,ndata)   = ikx                     ! ordered number in convinfo table
    cdata_all(ilate,ndata)  = dlat_earth*rad2deg      ! earth relative latitude (degrees)
    cdata_all(ilone,ndata)  = dlon_earth*rad2deg      ! earth relative longitude (degrees)
    
    
! write header record and data to output file for further processing
    write(lunout) obstype,sis,nreal,nchanl,ilat,ilon
    write(lunout) ((cdata_all(k,i),k=1,nreal),i=1,ndata)
    
    return
    
  end subroutine oneobschem
  
end module chemmod
