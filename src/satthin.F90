module satthin
!$$$ subprogram documenation block
!                .      .    .                                       .
! subprogram:    satthin
!   prgmmr: treadon          org: np20                date: 2002-10-17
!
! abstract:  This module contains code which may be used to selectively
!            thin satellite data.
!
! program history log:
!   2002-10-17 treadon
!   2004-05-28  kleist, subroutine call update
!   2005-03-28  wu, remove unused variable mlath
!   2005-10-06  treadon - add routine destroy_sfc
!   2005-11-29  parrish - add routines getsfc_nmm_binary, getsfc_nmm_netcdf,
!                         getsfc_mass_binary, getsfc_mass_netcdf
!   2005-12-08  treadon - rename global getsfc to getsfc_global, add getsfc
!                         to branch to appropriate getsfc* routine
!   2006-02-01  parrish - remove all getsfc routines, and create new getsfc
!                         which makes full sfc fields from guess_grids.
!                         rename surface fields to sst_full,sno_full, etc.
!                         to eliminate conflict with some fields with same
!                         names in guess_grids.
!                         also add new sfc fields u10_full,v10_full
!   2006-05-23  parrish - add new sfc field zs_full, the model terrain height
!   2006-07-28  derber  - use r1000 from constants
!   2007-05-01  wu      - correct error in subroutine makegvals which incorrectly
!                         defines longitude range on regional grid when domain
!                         includes north pole.
!   2009-04-21  derber  - add ithin to call to makegrids - account for negative ithin
!   2009-08-19  guo     - added assertions of ntguessig and ntguessfc.
!   2009-09-14  guo     - added an experimenting description of the usecase.
!			- added an an array size assertion on istart_val(:).
!   2011-04-01  li      - add getnst to read nst fields, add destroy_nst
!   2011-05-26  todling - add create_nst
!
! Subroutines Included:
!   sub makegvals      - set up for superob weighting
!   sub makegrids      - set up thinning grids
!   sub getsfc         - create full horizontal fields of surface arrays
!   sub getnst         - create full horizontal fields of nst arrays
!   sub map2tgrid      - map observation to location on thinning grid
!   sub destroygrids   - deallocate thinning grid arrays
!   sub destroy_sfc    - deallocate full horizontal surface arrays
!   sub create_nst     - allocate full horizontal nst arrays
!   sub destroy_nst    - deallocate full horizontal nst arrays
!   sub indexx         - sort array into ascending order
!
! Usecase destription:
!     read_obs    -->  read_airs, etc
!   []_makegvals			- set up for superob weighting
!   []_getsfc				- create full horizontal fields of surface arrays
!                     []_makegrids	- set up thinning grids
!                     []_map2tgrid	- map observation to location on thinning grid
!                     []_checkob	- intermediate ob checking to see if it should not be used
!                     []_finalcheck	- the final criterion check for sat obs and increments counters
!                     combine_radobs    - 
!                     []_destroygrids	- deallocate thinning grid arrays
!   []_destroy_sfc			- deallocate full horizontal fields of surface arrays
!
! Variable Definitions:
!   def mlat           - number of latitudes in thinning grid
!   def mlon           - number of longitudes in thinning grid
!   def superp         - maximum number of data types
!   def maxthin        - maximum number of obs to retain in thinning grid box
!   def itxmax         - total number of points in thinning grid
!   def istart_val     - starting location on thinning grid for superobs
!   def icount         - observation flag  true - no obs in this thinning box previously
!                                          false - previous ob in box
!   def isli_full      - snow/land/ice mask
!   def glat           - latitudes on thinning grid
!   def super_val      - super obs factor across data types
!   def super_val1     - super obs factors summed over all mpi tasks (data types)
!   def glon           - longitudes on thinning grid
!   def hll            - (i,j) index of point on thinning grid
!   def sli_full       - 0=sea/1=land/2=ice mask
!   def sst_full       - skin temperature
!   def sno_full       - snow-ice mask
!   def zs_full        - model terrain elevation
!   def score_crit     - "best" quality obs score in thinning grid box
!   def use_all        - parameter for turning satellite thinning algorithm off
!   def tref_full      - sea reference temperature
!   def dt_cool_full   - sea cooling amount across sub-layer
!   def z_c_full       - sub-layer thickness
!   def dt_warm_full   - sea diurnal warming amount
!   def z_w_full       - diurnal warming layer thickness
!   def c_0_full       - coefficient 1 to calculate d(Tz)/d(Tr)
!   def c_d_full       - coefficient 2 to calculate d(Tz)/d(Tr)
!   def w_0_full       - coefficient 3 to calculate d(Tz)/d(Tr)
!   def w_d_full       - coefficient 4 to calculate d(Tz)/d(Tr)
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$ end documentation block

  use kinds, only: r_kind,i_kind
  use mpeu_util, only: die, perr
  implicit none

! set default to private
  private
! set subroutines to public
  public :: makegvals
  public :: makegrids
  public :: getsfc
  public :: getnst
  public :: map2tgrid
  public :: destroygrids
  public :: destroy_sfc
  public :: create_nst
  public :: destroy_nst
  public :: indexx
! set passed variables to public
  public :: rlat_min,rlon_min,dlat_grid,dlon_grid,superp,super_val1,super_val
  public :: veg_type_full,soil_type_full,sfc_rough_full,sno_full,sst_full
  public :: fact10_full,isli_full,soil_moi_full,veg_frac_full,soil_temp_full
  public :: checkob,score_crit,itxmax,finalcheck,zs_full_gfs,zs_full
  public :: tref_full,dt_cool_full,z_c_full,dt_warm_full,z_w_full
  public :: c_0_full,c_d_full,w_0_full,w_d_full

  integer(i_kind) mlat,superp,maxthin,itxmax
  integer(i_kind),dimension(0:51):: istart_val
  
  integer(i_kind),allocatable,dimension(:):: mlon
  integer(i_kind),allocatable,dimension(:,:):: isli_full
  logical,allocatable,dimension(:)::icount

  real(r_kind) rlat_min,rlat_max,rlon_min,rlon_max,dlat_grid,dlon_grid
  real(r_kind),allocatable,dimension(:):: glat,score_crit
  real(r_kind),allocatable,dimension(:):: super_val,super_val1
  real(r_kind),allocatable,dimension(:,:):: glon,hll,zs_full,zs_full_gfs
  real(r_kind),allocatable,dimension(:,:):: veg_type_full,soil_type_full
  real(r_kind),allocatable,dimension(:,:,:):: veg_frac_full,soil_temp_full
  real(r_kind),allocatable,dimension(:,:,:):: soil_moi_full,sfc_rough_full
  real(r_kind),allocatable,dimension(:,:,:):: sst_full,sno_full,fact10_full
  real(r_kind),allocatable,dimension(:,:,:):: tref_full,dt_cool_full,z_c_full,dt_warm_full,z_w_full
  real(r_kind),allocatable,dimension(:,:,:):: c_0_full,c_d_full,w_0_full,w_d_full

  logical use_all

contains

  subroutine makegvals
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    makegvals                            
!     prgmmr:    derber      org: np23                date: 2002-10-17
!
! abstract:  This routine allocates and initializes arrays
!            used for superobs weighting.
!
! program history log:
!   2002-10-17  derber
!   2004-06-22  treadon - update documentation
!   2004-12-09  treadon - allocate thinning grids consistent with analysis domain
!   2006-07-28  derber  - use r1000 from constants
!   2007-05-01  wu      - correct error which incorrectly defines longitude range 
!                         on regional grid when domain includes north pole.
!   2008-05-23  safford - rm unused vars
!   2008-09-08  lueken  - merged ed's changes into q1fy09 code
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp
!
!$$$
    use constants, only: deg2rad,rearth_equator,zero,two,pi,half,one,&
       rad2deg,r1000
    use obsmod, only: dmesh,dthin,ndat
    use gridmod, only: regional,nlat,nlon,txy2ll
    use mpeu_util, only: die
    implicit none

    real(r_kind),parameter:: r90   = 90.0_r_kind
    real(r_kind),parameter:: r360  = 360.0_r_kind
    real(r_kind),parameter:: r999  = 999.0_r_kind

    integer(i_kind) i,ii,j
    integer(i_kind) mlonx,icnt,mlony,mlonj
    real(r_kind) delonx,delat,dgv,dx,dy
    real(r_kind) twopi,dlon_g,dlat_g,dlon_e,dlat_e
    real(r_kind) factor,delon
    real(r_kind) rkm2dg,glatm,glatx

!   Initialize variables, set constants
    maxthin=0
    do i=1,ndat
       maxthin=max(maxthin,abs(dthin(i)))
    end do
    istart_val=0
    twopi  = two*pi
    rkm2dg = r360/(twopi*rearth_equator)*r1000

!   Set lat,lon limits for analysis grid.
    rlat_min = -r90
    rlat_max = r90
    rlon_min = zero
    rlon_max = r360
    if (regional) then
       rlat_min =  r999
       rlat_max = -r999
       rlon_min =  r999
       rlon_max = -r999
       do j=1,nlon
          dlon_g=j
          do i=1,nlat
             dlat_g=i
             call txy2ll(dlon_g,dlat_g,dlon_e,dlat_e)
             dlat_e=dlat_e*rad2deg
             dlon_e=dlon_e*rad2deg
             if (dlon_e < zero) dlon_e = dlon_e + r360
             rlat_min = min(rlat_min,dlat_e)
             rlat_max = max(rlat_max,dlat_e)
             rlon_min = min(rlon_min,dlon_e)
             rlon_max = max(rlon_max,dlon_e)
          end do
       end do
    endif
    dlat_grid = rlat_max - rlat_min
    dlon_grid = rlon_max - rlon_min

    do ii=1,maxthin

!      Set up dimensions for thinning grids
       istart_val(ii+1)=istart_val(ii)
       if(abs(dmesh(ii)) > one)then
          dx    = dmesh(ii)*rkm2dg
          dy    = dx
          mlat  = dlat_grid/dy + half
          mlonx = dlon_grid/dx + half
          delat = dlat_grid/mlat
          delonx= dlon_grid/mlonx	
          dgv   = delat*half
          mlat=max(2,mlat);   mlonx=max(2,mlonx)

          icnt=0
          do j = 1,mlat
             glatx = rlat_min + (j-1)*delat
             glatx = glatx*deg2rad
             glatm = glatx + dgv*deg2rad
             
             factor = abs(cos(abs(glatm)))
             if (dmesh(ii)>zero) then
                mlonj = nint(mlonx*factor)
                mlony = max(2,mlonj)
             else
                delon = factor*dmesh(ii)
                delon = min(delon,r360)
                mlony = dlon_grid/delon
             endif
             do i = 1,mlony
                icnt=icnt+1
                istart_val(ii+1)=istart_val(ii+1)+1
             enddo

          enddo
       end if
    end do
    superp=istart_val(maxthin+1)
    
!   Allocate and initialize arrays for superobs weighthing
    allocate(super_val(0:superp),super_val1(0:superp))
    do i=0,superp
       super_val(i)=zero
    end do
    
    return
  end subroutine makegvals


  subroutine makegrids(rmesh,ithin)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    makegrids                            
!     prgmmr:    treadon     org: np23                date: 2002-10-17
!
! abstract:  This routine sets up dimensions for and allocates
!            thinning grids.
!
! program history log:
!   2002-10-17  treadon
!   2004-06-22  treadon - update documentation
!   2004-12-09  treadon - allocate thinning grids consistent with analysis domain
!   2008-05-23  safford - rm unused vars
!   2008-09-08  lueken  - merged ed's changes into q1fy09 code
!
!   input argument list:
!     rmesh - mesh size (km) of thinning grid.  If (rmesh <= one), 
!             then no thinning of the data will occur.  Instead,
!             all data will be used without thinning.
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp
!
!$$$
    use constants, only: rearth_equator,deg2rad,zero,half,one,two,pi

    implicit none

    real(r_kind)   ,intent(in   ) :: rmesh
    integer(i_kind),intent(in   ) :: ithin
    real(r_kind),parameter:: r360 = 360.0_r_kind
    integer(i_kind) i,j
    integer(i_kind) mlonx,mlonj
    real(r_kind) delonx,delat,dgv,halfpi,dx,dy
    real(r_kind) twopi
    real(r_kind) factor,delon
    real(r_kind) rkm2dg,glatm


!   If there is to be no thinning, simply return to calling routine
    use_all=.false.
    if(abs(rmesh) <= one .or. ithin <= 0)then
      use_all=.true.
      itxmax=1e7
      allocate(icount(itxmax))
      allocate(score_crit(itxmax))
      do j=1,itxmax
         icount(j) = .true.
         score_crit(j) = 9.99e10_r_kind
      end do
      return
    end if

!   Set constants
    halfpi = half*pi
    twopi  = two*pi
    rkm2dg = r360/(twopi*rearth_equator)*1.e3_r_kind

!   Set up dimensions and allocate arrays for thinning grids
    if (rmesh<zero) rkm2dg=one
    dx    = rmesh*rkm2dg
    dy    = dx
    mlat  = dlat_grid/dy + half
    mlonx = dlon_grid/dx + half
	delat = dlat_grid/mlat
	delonx= dlon_grid/mlonx
    dgv  = delat*half
    mlat=max(2,mlat);   mlonx=max(2,mlonx)

    allocate(mlon(mlat),glat(mlat),glon(mlonx,mlat),hll(mlonx,mlat))


!   Set up thinning grid lon & lat.  The lon & lat represent the location of the
!   lower left corner of the thinning grid box.
    itxmax=0
    do j = 1,mlat
       glat(j) = rlat_min + (j-1)*delat
       glat(j) = glat(j)*deg2rad
       glatm = glat(j) + dgv*deg2rad

       factor = abs(cos(abs(glatm)))
       if (rmesh>zero) then
          mlonj   = nint(mlonx*factor)	
          mlon(j) = max(2,mlonj)
          delon = dlon_grid/mlon(j)
       else
          delon = factor*rmesh
          delon = min(delon,r360)
          mlon(j) = dlon_grid/delon
       endif

       glat(j) = min(max(-halfpi,glat(j)),halfpi)
       do i = 1,mlon(j)
          itxmax=itxmax+1
          hll(i,j)=itxmax
          glon(i,j) = rlon_min + (i-1)*delon
          glon(i,j) = glon(i,j)*deg2rad
          glon(i,j) = min(max(zero,glon(i,j)),twopi)
       enddo

    end do


!   Allocate  and initialize arrays
    allocate(icount(itxmax))
    allocate(score_crit(itxmax))

    do j=1,itxmax
       icount(j) = .true.
       score_crit(j) = 9.99e10_r_kind
    end do

    return
  end subroutine makegrids

  subroutine getsfc(mype,use_sfc)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    getsfc
!     prgmmr:    parrish     org: np23                date: 2006-02-02
!
! abstract:  This routine converts subdomain surface fields in
!            guess_grids to full horizontal fields for use in 
!            reading of observations.
!
! program history log:
!   2002-10-17  parrish
!   2008-12-05  todling - add ESMF blocks; update bias_tskin
!   2009-01-28  todling - remove reference to original GMAO interface
!   2010-04-01  treadon - move strip to gridmod
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp
!
!$$$
    use kinds, only: r_single
    use gridmod, only:  nlat,nlon,lat2,lon2,lat1,lon1,jstart,&
       ltosi,ltosj,iglobal,itotsub,ijn,displs_g,regional,istart, &
       rlats,rlons,nlat_sfc,nlon_sfc,rlats_sfc,rlons_sfc,strip, use_gfs_nemsio
    use guess_grids, only: ntguessig,isli,sfct,sno,fact10,ges_z, &
       nfldsfc,ntguessfc,soil_moi,soil_temp,veg_type,soil_type, &
       veg_frac,sfc_rough,ifilesfc,nfldsig,isli2,sno2
    use m_gsiBiases, only: bias_tskin,compress_bias,bias_hour
    use jfunc, only: biascor

    use mpimod, only: mpi_comm_world,ierror,mpi_rtype
    use constants, only: zero,half,pi,two,one
    use ncepgfs_io, only: read_gfssfc,sfc_interpolate
    use ncepnems_io, only: read_nemssfc
    use sfcio_module, only: sfcio_realfill

    implicit none

    integer(i_kind),intent(in   ) :: mype
    logical        ,intent(in   ) :: use_sfc


! Local variables
    real(r_kind),dimension(lat1*lon1):: zsm
    real(r_kind),dimension(itotsub):: work1
    real(r_kind),dimension(lat2,lon2):: work2
    real(r_kind),dimension(lat2,lon2):: b_tskin
    real(r_kind),dimension(nlat,nlon):: bias
    real(r_kind),dimension(nlat):: ailoc
    real(r_kind),dimension(nlon):: ajloc
    real(r_kind),allocatable,dimension(:)::wlatx,slatx
    real(r_kind) :: dlon, missing
    real(r_kind),allocatable,dimension(:,:)::dum
    real(r_kind),allocatable,dimension(:,:)::dummy
    integer(i_kind) mm1,i,j,k,it,il,jl,jmax,idrt
    character(24) filename

    if( (ntguessig<1.or.ntguessig>nfldsig) .or. &
        (ntguessfc<1.or.ntguessfc>nfldsfc) ) then
        call perr('satthin.getsfc','ntguessig = ',ntguessig)
        call perr('satthin.getsfc','ntguessfc = ',ntguessfc)
	call die('satthin.getsfc')
    endif
    mm1=mype+1

    if(mype == 0)write(6,*)'GETSFC:  enter with nlat_sfc,nlon_sfc=',nlat_sfc,nlon_sfc,&
	' and nlat,nlon=',nlat,nlon
    if(regional)then
       nlat_sfc=nlat
       nlon_sfc=nlon
    end if
    if(mype == 0)write(6,*)'GETSFC: set nlat_sfc,nlon_sfc=',nlat_sfc,nlon_sfc
    allocate(rlats_sfc(nlat_sfc),rlons_sfc(nlon_sfc))

    allocate(isli_full(nlat_sfc,nlon_sfc),fact10_full(nlat_sfc,nlon_sfc,nfldsfc))
    allocate(sst_full(nlat_sfc,nlon_sfc,nfldsfc),sno_full(nlat_sfc,nlon_sfc,nfldsfc))
    allocate(zs_full(nlat,nlon))
    allocate(sfc_rough_full(nlat_sfc,nlon_sfc,nfldsfc))

    if(use_sfc)then
       allocate(soil_moi_full(nlat_sfc,nlon_sfc,nfldsfc),soil_temp_full(nlat_sfc,nlon_sfc,nfldsfc))
       allocate(veg_frac_full(nlat_sfc,nlon_sfc,nfldsfc),soil_type_full(nlat_sfc,nlon_sfc))
       allocate(veg_type_full(nlat_sfc,nlon_sfc))
    else
       allocate(dum(nlat_sfc,nlon_sfc))
    end if


!  Global read
#ifndef HAVE_ESMF
    if (.not. regional) then
       if (nlon == nlon_sfc .and. nlat == nlat_sfc) then
          rlats_sfc=rlats
          rlons_sfc=rlons
       else
          idrt=4
          jmax=nlat_sfc-2
          allocate(slatx(jmax),wlatx(jmax))
          call splat(idrt,jmax,slatx,wlatx)
          dlon=two*pi/float(nlon_sfc)
          do i=1,nlon_sfc
             rlons_sfc(i)=float(i-1)*dlon
          end do
          do i=1,(nlat_sfc-1)/2
             rlats_sfc(i+1)=-asin(slatx(i))
             rlats_sfc(nlat_sfc-i)=asin(slatx(i))
          end do
          rlats_sfc(1)=-half*pi
          rlats_sfc(nlat_sfc)=half*pi
          deallocate(slatx,wlatx)
       end if


       allocate(zs_full_gfs(nlat_sfc,nlon_sfc))
       if(use_sfc)then
          do it=1,nfldsfc
             write(filename,200)ifilesfc(it)
200          format('sfcf',i2.2)
             if ( use_gfs_nemsio ) then
                call read_nemssfc(filename,mype,&
                   fact10_full(:,:,it),sst_full(:,:,it),sno_full(:,:,it), &
                   veg_type_full,veg_frac_full(:,:,it), &
                   soil_type_full,soil_temp_full(:,:,it),&
                   soil_moi_full(:,:,it),isli_full,sfc_rough_full(:,:,it),zs_full_gfs)
             else
                call read_gfssfc(filename,mype,&
                   fact10_full(1,1,it),sst_full(1,1,it),sno_full(1,1,it), &
                   veg_type_full(1,1),veg_frac_full(1,1,it), &
                   soil_type_full(1,1),soil_temp_full(1,1,it),&
                   soil_moi_full(1,1,it),isli_full(1,1),sfc_rough_full(1,1,it),zs_full_gfs)
             end if
          end do
       else
          do it=1,nfldsfc
             write(filename,200)ifilesfc(it)
             if ( use_gfs_nemsio ) then
                call read_nemssfc(filename,mype,&
                   fact10_full(:,:,it),sst_full(:,:,it),sno_full(:,:,it), &
                   dum,dum,dum,dum,dum,isli_full,sfc_rough_full(:,:,it),zs_full_gfs)
             else
                call read_gfssfc(filename,mype,&
                   fact10_full(1,1,it),sst_full(1,1,it),sno_full(1,1,it), &
                   dum,dum,dum,dum,dum,isli_full(1,1),sfc_rough_full(1,1,it),zs_full_gfs)
             end if
          end do
          deallocate(dum)
       end if
 
       if (biascor > zero) then
          if (mype==0) write(6,*)'GETSFC:   add bias correction to guess field ',&
                                      filename
          do j=1,lon1*lat1
             zsm(j)=zero
          end do
 
          call compress_bias(b_tskin,bias_tskin,bias_hour)
          do j=1,lon2
             do i=1,lat2
                work2(i,j)=b_tskin(i,j)
             end do
          end do
          call strip(work2,zsm,1)
 
          call mpi_allgatherv(zsm,ijn(mm1),mpi_rtype,&
             work1,ijn,displs_g,mpi_rtype,&
             mpi_comm_world,ierror)
 
          do k=1,iglobal
             i=ltosi(k) ; j=ltosj(k)
             bias(i,j)=nint(work1(k))
          end do
!  Need to add interpolation of bias to nlon_sfc, nlat_sfc grid if
          if(nlon == nlon_sfc .and. nlat == nlat_sfc)then
             do it=1,nfldsfc
                do j=1,nlon
                   do i=1,nlat
                      sst_full(i,j,it)=sst_full(i,j,it)+bias(i,j)
                   end do
                end do
             end do
          else
             write(6,*)'satthin bias correction - incompatible surface resolution',&
                 nlon,nlon_sfc,nlat,nlat_sfc
             call stop2(82)
          end if
       end if

    else
#endif /* HAVE_ESMF */

       it=ntguessfc
       rlats_sfc=rlats
       rlons_sfc=rlons

! isli_full
       do j=1,lon1*lat1
          zsm(j)=zero
       end do
       do j=1,lon2
          do i=1,lat2
             work2(i,j)=isli(i,j,it)
          end do
       end do
       call strip(work2,zsm,1)
       call mpi_allgatherv(zsm,ijn(mm1),mpi_rtype,&
          work1,ijn,displs_g,mpi_rtype,&
          mpi_comm_world,ierror)
       do k=1,iglobal
          i=ltosi(k) ; j=ltosj(k)
          isli_full(i,j)=nint(work1(k))
       end do

! Fields with multiple time levels
       do it=1,nfldsfc

! sst_full
          do j=1,lon1*lat1
             zsm(j)=zero
          end do
          call strip(sfct(1,1,it),zsm,1)
          call mpi_allgatherv(zsm,ijn(mm1),mpi_rtype,&
             work1,ijn,displs_g,mpi_rtype,&
             mpi_comm_world,ierror)
          do k=1,iglobal
             i=ltosi(k) ; j=ltosj(k)
             sst_full(i,j,it)=work1(k)
          end do

! fact10_full
          do j=1,lon1*lat1
             zsm(j)=zero
          end do
          call strip(fact10(1,1,it),zsm,1)
          call mpi_allgatherv(zsm,ijn(mm1),mpi_rtype,&
             work1,ijn,displs_g,mpi_rtype,&
             mpi_comm_world,ierror)
          do k=1,iglobal
             i=ltosi(k) ; j=ltosj(k)
             fact10_full(i,j,it)=work1(k)
          end do

! sfc_rough_full
          do j=1,lon1*lat1
             zsm(j)=zero
          end do
          call strip(sfc_rough(1,1,it),zsm,1)
          call mpi_allgatherv(zsm,ijn(mm1),mpi_rtype,&
             work1,ijn,displs_g,mpi_rtype,&
             mpi_comm_world,ierror)
          do k=1,iglobal
             i=ltosi(k) ; j=ltosj(k)
             sfc_rough_full(i,j,it)=work1(k)
          end do

! sno_full
          do j=1,lon1*lat1
             zsm(j)=zero
          end do
          call strip(sno(1,1,it),zsm,1)
          call mpi_allgatherv(zsm,ijn(mm1),mpi_rtype,&
             work1,ijn,displs_g,mpi_rtype,&
             mpi_comm_world,ierror)
          do k=1,iglobal
             i=ltosi(k) ; j=ltosj(k)
             sno_full(i,j,it)=work1(k)
          end do

! veg_frac_full
          do j=1,lon1*lat1
             zsm(j)=zero
          end do
          call strip(veg_frac(1,1,it),zsm,1)
          call mpi_allgatherv(zsm,ijn(mm1),mpi_rtype,&
             work1,ijn,displs_g,mpi_rtype,&
             mpi_comm_world,ierror)
          if(use_sfc)then
             do k=1,iglobal
                i=ltosi(k) ; j=ltosj(k)
                veg_frac_full(i,j,it)=work1(k)
             end do
          end if
! soil_temp_full
          do j=1,lon1*lat1
             zsm(j)=zero
          end do
          call strip(soil_temp(1,1,it),zsm,1)
          call mpi_allgatherv(zsm,ijn(mm1),mpi_rtype,&
             work1,ijn,displs_g,mpi_rtype,&
             mpi_comm_world,ierror)
          if(use_sfc)then
             do k=1,iglobal
                i=ltosi(k) ; j=ltosj(k)
                soil_temp_full(i,j,it)=work1(k)
             end do
          end if


! soil_moi_full
          do j=1,lon1*lat1
             zsm(j)=zero
          end do
          call strip(soil_moi(1,1,it),zsm,1)
          call mpi_allgatherv(zsm,ijn(mm1),mpi_rtype,&
             work1,ijn,displs_g,mpi_rtype,&
             mpi_comm_world,ierror)
          if(use_sfc)then
             do k=1,iglobal
                i=ltosi(k) ; j=ltosj(k)
                soil_moi_full(i,j,it)=work1(k)
             end do
          end if
       end do  !end do ntguessfc



! Now single time level surface fields
       it=ntguessfc
! soil_type_full
       do j=1,lon1*lat1
          zsm(j)=zero
       end do
       call strip(soil_type(1,1,it),zsm,1)
       call mpi_allgatherv(zsm,ijn(mm1),mpi_rtype,&
          work1,ijn,displs_g,mpi_rtype,&
          mpi_comm_world,ierror)
       if(use_sfc)then
          do k=1,iglobal
             i=ltosi(k) ; j=ltosj(k)
             soil_type_full(i,j)=work1(k)
          end do
       end if

! veg_type_full
       do j=1,lon1*lat1
          zsm(j)=zero
       end do
       call strip(veg_type(1,1,it),zsm,1)
       call mpi_allgatherv(zsm,ijn(mm1),mpi_rtype,&
          work1,ijn,displs_g,mpi_rtype,&
          mpi_comm_world,ierror)
       if(use_sfc)then
          do k=1,iglobal
             i=ltosi(k) ; j=ltosj(k)
             veg_type_full(i,j)=work1(k)
          end do
       end if

#ifndef HAVE_ESMF
    end if
#endif /* HAVE_ESMF */

! Now stuff that isn't model dependent
! zs_full
    it=ntguessig
    do j=1,lon1*lat1
       zsm(j)=zero
    end do
    call strip(ges_z(1,1,it),zsm,1)
    call mpi_allgatherv(zsm,ijn(mm1),mpi_rtype,&
       work1,ijn,displs_g,mpi_rtype,&
       mpi_comm_world,ierror)
    do k=1,iglobal
       i=ltosi(k) ; j=ltosj(k)
       zs_full(i,j)=work1(k)
    end do

!   old gfs surface files do not have a terrain record.
!   therefore, need to interpolate the terrain from the atm
!   grid to the surface grid, which may not be the same.
    if (.not. regional) then
       missing = sfcio_realfill + one
       if (maxval(zs_full_gfs) < missing) then
          if (nlon == nlon_sfc .and. nlat == nlat_sfc) then
             zs_full_gfs = zs_full
          else
             allocate(dummy(nlat_sfc,nlon_sfc))
             call sfc_interpolate(zs_full,nlon,nlat,dummy,nlon_sfc,nlat_sfc)
             zs_full_gfs = dummy
             deallocate(dummy)
          endif
       endif
    endif

!   find subdomain for isli2
    if (nlon == nlon_sfc .and. nlat == nlat_sfc) then
       do j=1,lon2
          jl=j+jstart(mm1)-2
          jl=min0(max0(1,jl),nlon)
          do i=1,lat2
             il=i+istart(mm1)-2
             il=min0(max0(1,il),nlat)
             isli2(i,j)=isli_full(il,jl)
 	     do k=1,nfldsfc
                sno2(i,j,k)=sno_full(il,jl,k)
             end do
          end do
       end do
    else
       ailoc=rlats
       ajloc=rlons
       call grdcrd(ailoc,nlat,rlats_sfc,nlat_sfc,1)
       call grdcrd(ajloc,nlon,rlons_sfc,nlon_sfc,1)
       do j=1,lon2
          jl=j+jstart(mm1)-2
          jl=min0(max0(1,jl),nlon)
          jl=nint(ajloc(jl))
          jl=min0(max0(1,jl),nlon_sfc)
          do i=1,lat2
             il=i+istart(mm1)-2
             il=min0(max0(1,il),nlat)
             il=nint(ailoc(il))
             il=min0(max0(1,il),nlat_sfc)
             isli2(i,j)=isli_full(il,jl)
             do k=1,nfldsfc
                sno2(i,j,k) =sno_full(il,jl,k)
             end do
          end do
       end do

    end if
    return

  end subroutine getsfc

  subroutine getnst(mype)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    getnst
!     prgmmr:    xu li     org: np23                date: 2008-04-16
!
! abstract:  This routine read full horizontal nst fields for use in
!            reading of observations.
!
! program history log:
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp
!
!$$$
    use kinds, only: r_kind,i_kind
    use gridmod, only: nlat,nlon,nlat_sfc,nlon_sfc
    use guess_grids, only: ntguesnst,nfldnst,ifilenst
    use ncepgfs_io, only: read_gfsnst

    implicit none

    integer(i_kind) mype,it

    character(24) filename

    if( ntguesnst < 1 .or. ntguesnst > nfldnst ) then
        call perr('satthin.getnst','ntguesnst = ',ntguesnst)
	call die('satthin.getnst')
    endif

    if(mype == 0)write(6,*)'GETNST:  enter with nlat_sfc,nlon_sfc=',nlat_sfc,nlon_sfc,&
	' and nlat,nlon=',nlat,nlon

    do it=1,nfldnst
      write(filename,200)ifilenst(it)
200   format('nstf',i2.2)
      call read_gfsnst(filename,mype,&
           tref_full(1,1,it),dt_cool_full(1,1,it),z_c_full(1,1,it), &
           dt_warm_full(1,1,it), z_w_full(1,1,it), &
           c_0_full(1,1,it),c_d_full(1,1,it),w_0_full(1,1,it),w_d_full(1,1,it))

    end do

  end subroutine getnst

  subroutine map2tgrid(dlat_earth,dlon_earth,dist1,crit1,itx,ithin,itt,iuse,sis)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    map2tgrid
!     prgmmr:    derber      org: np2                 date: 2006-05-03
!
! abstract:  This routine maps observations to the thinning grid.
!
! program history log:
!   2006-05-03  derber (created from map2grids)
!   2006-09-13  treadon - set itx=1 for the case use_all=.true.
!
!   input argument list:
!     dlat_earth - earth relative observation latitude (radians)
!     dlon_earth - earth relative observation longitude (radians)
!     crit1      - quality indicator for observation (smaller = better)
!     ithin      - number of obs to retain per thinning grid box
!     sis        - sensor/instrument/satellite
!
!   output argument list:
!     itx   - combined (i,j) index of observation on thinning grid
!     itt   - superobs thinning counter
!     iuse  - .true. if observation should be used
!     
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp
!
!$$$
    use constants, only: one, half
    implicit none

    logical        ,intent(  out) :: iuse
    integer(i_kind),intent(in   ) :: ithin
    integer(i_kind),intent(  out) :: itt,itx
    real(r_kind)   ,intent(in   ) :: dlat_earth,dlon_earth,crit1
    real(r_kind)   ,intent(  out) :: dist1
    character(20)  ,intent(in   ) :: sis

    integer(i_kind) ix,iy
    real(r_kind) dlat1,dlon1,dx,dy,dxx,dyy


!   If using all data (no thinning), simply return to calling routine
    if(use_all .or. ithin <= 0)then
       iuse=.true.
       itt=1
       if(itx < itxmax) then
          itx=itx+1
       else
          iuse = .false.
          write(6,*)'MAP2TGRID:  ndata > maxobs when reading data for ',sis,itxmax
       end if
       return
    end if

!   Compute (i,j) indices of coarse mesh grid (grid number 1) which 
!   contains the current observation.
    dlat1=dlat_earth
    dlon1=dlon_earth

    call grdcrd(dlat1,1,glat,mlat,1)
    iy=int(dlat1)
    dy=dlat1-iy
    iy=max(1,min(iy,mlat))

    call grdcrd(dlon1,1,glon(1,iy),mlon(iy),1)
    ix=int(dlon1)
    dx=dlon1-ix
    ix=max(1,min(ix,mlon(iy)))

    dxx=half-min(dx,one-dx)
    dyy=half-min(dy,one-dy)
    dist1=dxx*dxx+dyy*dyy+half
    itx=hll(ix,iy)
    itt=istart_val(ithin)+itx
    if(ithin == 0) itt=0

!   Increment obs counter on coarse mesh grid.  Also accumulate observation
!   score and distance functions

!   ratio=1.e9
!   if ( dx > zero ) ratio=dy/dx
!   dista=sin(two*atan(ratio))
!   distb=sin(pi*dx)                !dista+distb is max at grid box center
!   dist1=one - quarter*(dista + distb)  !dist1 is min at grid box center and 
                                    !ranges from 1 (at corners)to 
                                    !.5 (at center of box)
    iuse=.true.

    if(dist1*crit1 > score_crit(itx) .and. icount(itx))iuse=.false.

    return
  end subroutine map2tgrid

  subroutine checkob(dist1,crit1,itx,iuse)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    checkob
!     prgmmr:    checkob     org: np23                date: 2002-10-17
!
! abstract:  intermediate ob checking routine to see if we should not use.
!
! program history log:
!   2006-05-03  derber
!
!   input argument list:
!     dist1  - quality indicator for distance (smaller = better)
!     crit1      - quality indicator for observation (smaller = better)
!     itx   - combined (i,j) index of observation on thinning grid
!     iuse  - .true. if observation should be used
!
!   output argument list:
!     iuse  - .true. if observation should be used
!     
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp
!
!$$$
    implicit none

    logical        ,intent(inout) :: iuse
    integer(i_kind),intent(in   ) :: itx
    real(r_kind)   ,intent(in   ) :: dist1,crit1

!   If using all data (no thinning), simply return to calling routine
    if(use_all .or. .not. iuse .or. icount(itx))return

    if(crit1*dist1 > score_crit(itx))iuse=.false.

    return
  end subroutine checkob

  subroutine finalcheck(dist1,crit1,itx,iuse)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    finalcheck
!     prgmmr:    derber     org: np23                date: 2002-10-17
!
! abstract:  This routine performs the final criterion check for sat 
!              obs and increments counters
!
! program history log:
!   2006-05-03  derber
!
!   input argument list:
!     dist1  - quality indicator for distance (smaller = better)
!     crit1  - quality indicator for observation (smaller = better)
!     itx    - combined (i,j) index of observation on thinning grid
!
!   output argument list:
!     iuse   - .true. if observation should be used
!     
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp
!
!$$$
    implicit none
    logical        ,intent(inout) :: iuse
    integer(i_kind),intent(in   ) :: itx
    real(r_kind)   ,intent(in   ) :: dist1,crit1

    real(r_kind) crit

!   If not using data, simply return to calling routine
    if(.not. iuse)return

    crit=crit1*dist1

!   Increment obs counter

    if(icount(itx) .or. crit < score_crit(itx))then
       score_crit(itx)= crit
       icount(itx)=.false.
    else
       iuse = .false.
    end if

    return
  end subroutine finalcheck


  subroutine destroygrids
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    destroygrids
!     prgmmr:    treadon     org: np23                date: 2002-10-17
!
! abstract:  This deallocate arrays used in thinning of satellite data.
!
! program history log:
!   2002-10-17  treadon
!   2004-06-22  treadon - update documentation
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp
!
!$$$
    implicit none


    deallocate(icount)
    deallocate(score_crit)

!   If using all data (no thinning), the arrays following the
!   return are never allocated --> therefore nothing to deallocate
    if(use_all) return

    deallocate(mlon,glat,glon,hll)
    return
  end subroutine destroygrids

  subroutine destroy_sfc
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    destroy_sfc
!     prgmmr:    treadon     org: np23                date: 2005-10-06
!
! abstract:  This deallocate surface arrays
!
! program history log:
!   2005-10-06  treadon
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp
!
!$$$
    implicit none

    if(allocated(sst_full))deallocate(sst_full)
    if(allocated(sno_full))deallocate(sno_full)
    if(allocated(fact10_full))deallocate(fact10_full)
    if(allocated(isli_full))deallocate(isli_full)
    if(allocated(veg_type_full))deallocate(veg_type_full)
    if(allocated(soil_type_full))deallocate(soil_type_full)
    if(allocated(veg_frac_full))deallocate(veg_frac_full)
    if(allocated(soil_temp_full))deallocate(soil_temp_full)
    if(allocated(soil_moi_full))deallocate(soil_moi_full)
    if(allocated(zs_full))deallocate(zs_full)
    if(allocated(sfc_rough_full))deallocate(sfc_rough_full)
    if(allocated(zs_full_gfs)) deallocate(zs_full_gfs)

    return
  end subroutine destroy_sfc

  subroutine create_nst
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    create_nst
!     prgmmr:    todling   org: np23                date: 2011-05-26
!
! abstract:  Allocate nst-related variables
!
! program history log:
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp
!
!$$$
    use gridmod, only: nlat_sfc,nlon_sfc
    use guess_grids, only: nfldnst
    implicit none

    if(.not.allocated(w_d_full))     allocate(w_d_full    (nlat_sfc,nlon_sfc,nfldnst))
    if(.not.allocated(w_0_full))     allocate(w_0_full    (nlat_sfc,nlon_sfc,nfldnst))
    if(.not.allocated(c_d_full))     allocate(c_d_full    (nlat_sfc,nlon_sfc,nfldnst))
    if(.not.allocated(c_0_full))     allocate(c_0_full    (nlat_sfc,nlon_sfc,nfldnst))
    if(.not.allocated(z_w_full))     allocate(z_w_full    (nlat_sfc,nlon_sfc,nfldnst))
    if(.not.allocated(dt_warm_full)) allocate(dt_warm_full(nlat_sfc,nlon_sfc,nfldnst))
    if(.not.allocated(z_c_full))     allocate(z_c_full    (nlat_sfc,nlon_sfc,nfldnst))
    if(.not.allocated(dt_cool_full)) allocate(dt_cool_full(nlat_sfc,nlon_sfc,nfldnst))
    if(.not.allocated(tref_full))    allocate(tref_full   (nlat_sfc,nlon_sfc,nfldnst))

    return
  end subroutine create_nst

  subroutine destroy_nst
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    destroy_nst
!     prgmmr:    xu li     org: np23                date: 2009-08-31
!
! abstract:  This deallocate nst arrays
!
! program history log:
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp
!
!$$$
    implicit none

    if(allocated(tref_full))    deallocate(tref_full)
    if(allocated(dt_cool_full)) deallocate(dt_cool_full)
    if(allocated(z_c_full))     deallocate(z_c_full)
    if(allocated(dt_warm_full)) deallocate(dt_warm_full)
    if(allocated(z_w_full))     deallocate(z_w_full)
    if(allocated(c_0_full))     deallocate(c_0_full)
    if(allocated(c_d_full))     deallocate(c_d_full)
    if(allocated(w_0_full))     deallocate(w_0_full)
    if(allocated(w_d_full))     deallocate(w_d_full)

    return
  end subroutine destroy_nst

  subroutine indexx(n,arr,indx)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    indexx
!     prgmmr:    treadon     org: np23                date: 2002-10-17
!
! abstract:  This routine indexes a sort key array, arr, such that
!            arr(indx(i),i=1,n) is the sort key in ascending order.
!
! program history log:
!   2002-10-17  treadon
!   2004-06-22  treadon - update documentation
!
!   input argument list:
!     n    - size of sort key array
!     arr  - sort key array
!
!   output argument list:
!     indx - index array
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp
!
!$$$
    use kinds, only: r_double
    use constants, only: one
    implicit none

    integer(i_kind) maxblock
    parameter (maxblock=1000)

    integer(i_kind),intent(in   ) :: n
    real(r_kind)   ,intent(in   ) :: arr(n)

    integer(i_kind),intent(  out) :: indx(n)

#ifdef ibm_sp
    real(r_kind),dimension(maxblock)::work
    real(r_double) oned

    oned=1._r_double
    if (digits(one)<digits(oned)) then
       call ssorts(arr,1,n,indx,work,maxblock)
    else
       call dsorts(arr,1,n,indx,work,maxblock)
    endif
#else
    integer(i_kind) m,nstack
    parameter (m=7,nstack=500)
    integer(i_kind) i,indxt,ir,itemp,j,jstack,k,l,istack(nstack)
    real(r_kind) a
    
    do j=1,n
       indx(j)=j
    end do
    jstack=0
    l=1
    ir=n
    
1   continue
    
    if(ir-l<m)then
       do j=l+1,ir
          indxt=indx(j)
          a=arr(indxt)
          do i=j-1,l,-1
             if(arr(indx(i))<=a)goto 2
             indx(i+1)=indx(i)
          end do
          i=l-1
2         continue
          indx(i+1)=indxt
       end do
       if(jstack==0)return
       ir=istack(jstack)
       l=istack(jstack-1)
       jstack=jstack-2
       
    else
       k=(l+ir)/2
       itemp=indx(k)
       indx(k)=indx(l+1)
       indx(l+1)=itemp
       if(arr(indx(l))>arr(indx(ir)))then
          itemp=indx(l)
          indx(l)=indx(ir)
          indx(ir)=itemp
       endif
       if(arr(indx(l+1))>arr(indx(ir)))then
          itemp=indx(l+1)
          indx(l+1)=indx(ir)
          indx(ir)=itemp
       endif
       if(arr(indx(l))>arr(indx(l+1)))then
          itemp=indx(l)
          indx(l)=indx(l+1)
          indx(l+1)=itemp
       endif
       i=l+1
       j=ir
       indxt=indx(l+1)
       a=arr(indxt)
3      continue
       i=i+1
       if(arr(indx(i))<a)goto 3
       
4      continue
       j=j-1
       if(arr(indx(j))>a)goto 4
       if(j<i)goto 5
       itemp=indx(i)
       indx(i)=indx(j)
       indx(j)=itemp
       goto 3
       
5      continue
       indx(l+1)=indx(j)
       indx(j)=indxt
       jstack=jstack+2
       if(jstack>nstack)then
          write(6,*)'INDEXX:  nstack=',nstack,' too small in indexx'
          call stop2(32)
       endif
       if(ir-i+1>=j-l)then
          istack(jstack)=ir
          istack(jstack-1)=i
          ir=j-1
       else
          istack(jstack)=j-1
          istack(jstack-1)=l
          l=i
       endif
    endif
    goto 1
#endif
  end subroutine indexx

end module satthin
