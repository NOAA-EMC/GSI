module ncepnems_io
!$$$ module documentation block
!           .      .    .                                       .
! module:   ncepnems_io
!   prgmmr: Huang       org: np23                date: 2010-02-22
!
! abstract: This module contains routines which handle input/output
!           operations for NCEP NEMS global atmospheric and surface files.
!
! program history log:
!   2010-02-22 Huang    Initial version.  Based on ncepgfs_io
!   2010-10-18 Huang    Remove subroutine reorder_gfsgrib for no longer been called in GSI
!                       For Now, subroutine sfc_interpolate is kept in ncepgfs_io.f90.
!                       When sigio and gfsio are both retired, i.e., remove ncepgfs_io.f90.
!                       move this routines back to this module
!   2011-03-03 Huang    Changes has been made to adopt to high resolution GSI run (T382 & T574)
!                       both for CPU and memory issues.
!                       Future development of nemsio need to consider a mapping routine be
!                       inserted between read-in forecast field and GSI first guess field,
!                       as well as GSI analysis field and write-out data field for forecast
!                       model. Due to computation resource, GSI may not be able to run at
!                       the same resolution as that of forecast model, e.g., run GSI at T382
!                       w/ T574 forecast model output.
!   2011-10-25 Huang    (1) Add unified error message routine to make the code cleaner
!                       (2) To reduce the memory allocation as low as possible, remove all
!                           reference to sfc_head and re-used the same local arrays.
!                           Remove unneeded nemsio_data & gfsdata.
!                       (3) Add parallel IO code to read_atm_
!   2011-11-01 Huang    (1) add integer nst_gsi to control the mode of NSST
!                       (2) add read_nemsnst to read ncep nst file
!                       (3) add subroutine write_nemssfc_nst to save sfc and nst files
!
! Subroutines Included:
!   sub read_nems       - driver to read ncep nems atmospheric and surface
!   sub read_nems_chem
!   sub read_nemsatm    - read ncep nems atmospheric file, scatter
!                         on grid to analysis subdomains
!   sub read_nemssfc    - read ncep nems surface file, scatter on grid to 
!                         analysis subdomains
!   sub write_nems      - driver to write ncep nems atmospheric and surface
!                         analysis files
!   sub write_nemsatm   - gather on grid, write ncep nems atmospheric analysis file
!   sub write_nemssfc   - gather/write on grid ncep surface analysis file
!   sub read_nemsnst    - read ncep nst file, scatter on grid to analysis subdomains
!   sub write_nemssfc_nst - gather/write on grid ncep surface & nst analysis file
!   sub sfc_interpolate   - interpolate from gfs atm grid to gfs sfc grid
!
! Variable Definitions:
!   The difference of time Info between operational GFS IO (gfshead%, sfc_head%),
!      analysis time (iadate), and NEMSIO (idate=)
!
!       gfshead & sfc_head            NEMSIO Header           Analysis time (obsmod)    
!       ===================   ============================  ==========================
!         %idate(1)  Hour     idate(1)  Year                iadate(1)  Year
!         %idate(2)  Month    idate(2)  Month               iadate(2)  Month
!         %idate(3)  Day      idate(3)  Day                 iadate(3)  Day
!         %idate(4)  Year     idate(4)  Hour                iadate(4)  Hour
!                             idate(5)  Minute              iadate(5)  Minute
!                             idate(6)  Scaled seconds
!                             idate(7)  Seconds multiplier
!
!   The difference of header forecasting hour Info bewteen operational GFS IO
!      (gfshead%, sfc_head%) and NEMSIO
!
!           gfshead & sfc_head                NEMSIO Header       
!       ==========================     ============================
!       %fhour  FCST Hour (r_kind)     nfhour     FCST Hour (i_kind)
!                                      nfminute   FCST Mins (i_kind)
!                                      nfsecondn  FCST Secs (i_kind) numerator
!                                      nfsecondd  FCST Secs (i_kind) denominator
!
!       %fhour = float(nfhour) + float(nfminute)/r60 + float(nfsecondn)/float(nfsecondd)/r3600
!
!   nframe     - nframe is the number of grids extend outward from the
!                edge of modeling domain.
!
!                NEMSIO provides a more flexible read.  User can get the
!                size of record (1D) to be read from file header. The
!                normal record size should be delx*dely, i.e., total model
!                grid points.  However, some regional models also ouput
!                additional data of grids around the modeling domain 
!                (buffer zone). For this type of output, nframe needs to
!                be know to calculate the size of record, i.e.,
!                   array size = (delx+2*nframe) * (dely+2*nframe)
!
!                However, nframe should always be zero for global model.
!                To simplify the code for reading and writing global model
!                files, we will not factor in the nframe for computing
!                array size or array index shift (by nframe) between 
!                input/output array and internal GSI array.  The normal
!                size of I/O record remains as delx*dely.  Add a checking
!                routine to assure nframe=zero.
!
! attributes:
!   language: f90
!   machine:
!
! NOTE: When global meteorology switched to NEMS/GFS, all routines and 
!       modules of old GFS (sigio) can be deactivated.  To keep the code
!       clean, all "nems" can be replaced by "gfs" for minimal changes 
!       of GSI code structure.  For dual purpose, two distincit routine
!       names are used to accomodiate old and new systems.  It is now 
!       controled by a namelist argument "use_gfs_nemsio"
!
!
!$$$ end documentation block

  use constants, only: zero,one,fv,r60,r3600
  implicit none

  private
  public read_nems
  public read_nems_chem
  public read_nemsatm
  public read_nemssfc
  public write_nemsatm
  public write_nemssfc
  public read_nemsnst
  public write_nemssfc_nst
  public sfc_interpolate
  public error_msg

  interface read_nems
     module procedure read_
  end interface

  interface read_nems_chem
     module procedure read_chem_
  end interface

  interface read_nemsatm
     module procedure read_atm_
  end interface

  interface read_nemssfc
     module procedure read_sfc_
  end interface

  interface read_nemsnst
     module procedure read_sfc_nst_
  end interface

  interface write_nemsatm
     module procedure write_atm_
  end interface

  interface write_nemssfc
     module procedure write_sfc_
  end interface

  interface write_nemssfc_nst
     module procedure write_sfc_nst_
  end interface

  interface error_msg
     module procedure error_msg_
  end interface

  character(len=*),parameter::myname='ncepnems_io'

contains

  subroutine read_ (mype)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    read_nems
!
!   prgrmmr: Ho-Chun Huang
!
! abstract:
!
! program history log:
!   2010-03-31  Huang   - create routine based on read_gfs
!   2010-10-19  Huang   - remove spectral part for gridded NEMS/GFS
!   2011-05-01  todling - cwmr no longer in guess-grids; use metguess bundle now
!   2013-10-19  todling - metguess now holds background
!
!   input argument list:
!     mype              - mpi task id
!
!   output argument list:
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block

    use kinds, only: i_kind,r_kind
    use gridmod, only: sp_a,grd_a,jcap_b
    use guess_grids, only: ifilesig,nfldsig
    use gsi_metguess_mod, only: gsi_metguess_bundle
    use gsi_bundlemod, only: gsi_bundlegetpointer
    use general_sub2grid_mod, only: sub2grid_info,general_sub2grid_create_info,general_sub2grid_destroy_info
    use mpimod, only: npe
    implicit none

    integer(i_kind),intent(in   ) :: mype

    character(len=*),parameter::myname_=myname//'*read_'
    character(24) filename
    integer(i_kind):: it, iret, ier, inner_vars, num_fields
    real(r_kind),pointer,dimension(:,:  ):: ges_ps_it  =>NULL()
    real(r_kind),pointer,dimension(:,:  ):: ges_z_it   =>NULL()
    real(r_kind),pointer,dimension(:,:,:):: ges_u_it   =>NULL()
    real(r_kind),pointer,dimension(:,:,:):: ges_v_it   =>NULL()
    real(r_kind),pointer,dimension(:,:,:):: ges_div_it =>NULL()
    real(r_kind),pointer,dimension(:,:,:):: ges_vor_it =>NULL()
    real(r_kind),pointer,dimension(:,:,:):: ges_tv_it  =>NULL()
    real(r_kind),pointer,dimension(:,:,:):: ges_q_it   =>NULL()
    real(r_kind),pointer,dimension(:,:,:):: ges_oz_it  =>NULL()
    real(r_kind),pointer,dimension(:,:,:):: ges_cwmr_it=>NULL()

    type(sub2grid_info) :: grd_t
    logical regional

    regional=.false.
    inner_vars=1
    num_fields=min(8*grd_a%nsig+2,npe)
!  Create temporary communication information fore read routines
    call general_sub2grid_create_info(grd_t,inner_vars,grd_a%nlat,grd_a%nlon, &
          grd_a%nsig,num_fields,regional)


    do it=1,nfldsig

!      Get pointer to could water mixing ratio
       ier=0
       call gsi_bundlegetpointer (gsi_metguess_bundle(it),'ps',ges_ps_it,iret)
       ier=ier+iret
       call gsi_bundlegetpointer (gsi_metguess_bundle(it),'z',ges_z_it,iret)
       ier=ier+iret
       call gsi_bundlegetpointer (gsi_metguess_bundle(it),'u',ges_u_it,iret)
       ier=ier+iret
       call gsi_bundlegetpointer (gsi_metguess_bundle(it),'v',ges_v_it,iret)
       ier=ier+iret
       call gsi_bundlegetpointer (gsi_metguess_bundle(it),'div',ges_div_it,iret)
       ier=ier+iret
       call gsi_bundlegetpointer (gsi_metguess_bundle(it),'vor',ges_vor_it,iret)
       ier=ier+iret
       call gsi_bundlegetpointer (gsi_metguess_bundle(it),'tv',ges_tv_it,iret)
       ier=ier+iret
       call gsi_bundlegetpointer (gsi_metguess_bundle(it),'q',ges_q_it,iret)
       ier=ier+iret
       call gsi_bundlegetpointer (gsi_metguess_bundle(it),'oz',ges_oz_it,iret)
       ier=ier+iret
       call gsi_bundlegetpointer (gsi_metguess_bundle(it),'cw',ges_cwmr_it,iret)
       ier=ier+iret
       if (ier/=0) cycle ! this allows code to be free from met-fields

       write(filename,'(''sigf'',i2.2)') ifilesig(it)
       
       call general_read_gfsatm_nems(grd_t,sp_a,filename,mype,.true.,.true.,.true.,&
            ges_z_it,ges_ps_it,&
            ges_vor_it,ges_div_it,&
            ges_u_it,ges_v_it,&
            ges_tv_it,ges_q_it,&
            ges_cwmr_it,ges_oz_it,.true.,ier)
!      call read_atm_ (grd_a,filename,mype,sp_a,.true.,.true.,.true.,&
!           ges_z_it,ges_ps_it,&
!           ges_vor_it,ges_div_it,&
!           ges_u_it,ges_v_it,&
!           ges_tv_it,ges_q_it,&
!           ges_cwmr_it,ges_oz_it)
    end do
    call general_sub2grid_destroy_info(grd_t)
  end subroutine read_

  subroutine read_chem_ ( iyear, month,idd )
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    read_nems_chem
!
!   prgrmmr: todling
!
! abstract: fills chemguess_bundle with GFS chemistry. 
!
! remarks:
!    1. Right now, only CO2 is done and even this is treated
!        as constant througout the assimialation window.
!    2. iyear and month could come from obsmod, but logically
!       this program should never depend on obsmod
! 
!
! program history log:
!   2010-12-23  Huang   - initial code, based on read_gfs_chem
!   2011-06-29  todling - no explict reference to internal bundle arrays
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block

    use kinds,   only: i_kind, r_kind
    use mpimod,  only: mype
    use gridmod, only: lat2,lon2,nsig,nlat,rlats,istart
    use ncepgfs_ghg, only: read_gfsco2
    use guess_grids, only: nfldsig,ntguessig
    use gsi_bundlemod, only: gsi_bundlegetpointer
    use gsi_chemguess_mod, only: gsi_chemguess_bundle
    use gsi_chemguess_mod, only: gsi_chemguess_get

    implicit none

!   Declared argument list
    integer(i_kind), intent(in):: iyear
    integer(i_kind), intent(in):: month
    integer(i_kind), intent(in):: idd

!   Declare local variables
    integer(i_kind) :: igfsco2, i, j, n, iret
    real(r_kind),dimension(lat2):: xlats
    real(r_kind),pointer,dimension(:,:,:)::p_co2=>NULL()
    real(r_kind),pointer,dimension(:,:,:)::ptr3d=>NULL()

    if(.not.associated(gsi_chemguess_bundle)) return
    call gsi_bundlegetpointer(gsi_chemguess_bundle(1),'co2',p_co2,iret)
    if(iret /= 0) return

!   Get subdomain latitude array
    j = mype + 1
    do i = 1, lat2
       n = min(max(1, istart(j)+i-2), nlat)
       xlats(i) = rlats(n)
    enddo

!   Read in CO2
    call gsi_chemguess_get ( 'i4crtm::co2', igfsco2, iret )
    call read_gfsco2 ( iyear,month,idd,igfsco2,xlats,&
                       lat2,lon2,nsig,mype, p_co2 )

! Approximation: setting all times co2 values equal to the daily co2 values

    do n = 2, nfldsig
       call gsi_bundlegetpointer(gsi_chemguess_bundle(n),'co2',ptr3d,iret)
       ptr3d = p_co2
    enddo

  end subroutine read_chem_

  subroutine read_atm_ (grd,filename,mype,sp_a,uvflag,vordivflag,zflag, &
       g_z,g_ps,g_vor,g_div,g_u,g_v,&
       g_tv,g_q,g_cwmr,g_oz)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    read_nemsatm    read nems atm and send to all mpi tasks
!   prgmmr: Huang            org: np23                date: 2010-02-22
!
! abstract: read ncep nems/gfs atmospheric guess field and 
!           scatter to subdomains
!
! program history log:
!   2010-02-22 Huang    Initial version.  Based on sub read_gfsatm
!   2011-02-28 Huang    Re-arrange the read sequence to be same as model
!                       write sequence.  Alsom allocate and deallocate
!                       temporary working array immediatelt before and after
!                       the processing and scattering first guess field to reduce
!                       maximum resident memory size.  Page fault can happen
!                       when running at high resolution GSI, e.g., T574.
!   2011-09-23 Huang    Add NEMS parallel IO capability
!   2013-10-25 todling  reposition fill_ns,filluv_ns to commvars
!
!   input argument list:
!     grd      - structure variable containing information about grid
!                    (initialized by general_sub2grid_create_info, located in
!                    general_sub2grid_mod.f90)
!     sp_a     - structure variable containing spectral information for analysis
!                    (initialized by general_init_spec_vars, located in
!                    general_specmod.f90)
!     mype     - mpi task id
!     uvflag   - logical to use u,v (.true.) or st,vp (.false.) perturbations
!     vordivflag - logical to determine if routine should output vorticity and
!                  divergence
!     zflag    - logical to determine if surface height field should be output
!
!   output argument list:
!     g_*      - guess fields
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
    use kinds, only: r_kind,i_kind
    use gridmod, only: ntracer,ncloud,reload,itotsub,jcap_b
    use general_commvars_mod, only: fill_ns,filluv_ns,fill2_ns,filluv2_ns,ltosj_s,ltosi_s
    use general_specmod, only: spec_vars
    use general_sub2grid_mod, only: sub2grid_info
    use mpimod, only: npe,mpi_comm_world,ierror,mpi_rtype
    use nemsio_module, only: nemsio_init,nemsio_open,nemsio_close
    use nemsio_module, only: nemsio_gfile,nemsio_getfilehead,nemsio_readrecv
    use egrid2agrid_mod,only: g_egrid2agrid,g_create_egrid2agrid,egrid2agrid_parm,destroy_egrid2agrid
    use constants, only: two,pi,half,deg2rad
    implicit none
    
!   Declare local parameters
    real(r_kind),parameter:: r0_001 = 0.001_r_kind

!   Declare passed variables
    type(sub2grid_info)                   ,intent(in   ) :: grd
    character(LEN=24)                     ,intent(in   ) :: filename
    integer(i_kind)                       ,intent(in   ) :: mype
    logical                               ,intent(in   ) :: uvflag,vordivflag,zflag
    real(r_kind),dimension(grd%lat2,grd%lon2)     ,intent(  out) :: g_z,g_ps
    real(r_kind),dimension(grd%lat2,grd%lon2,grd%nsig),intent(  out) :: g_u,g_v,&
         g_vor,g_div,g_cwmr,g_q,g_oz,g_tv
    type(spec_vars)                       ,intent(in   ) :: sp_a
    
!   Declare local variables
    character(len=120) :: my_name = 'READ_NEMSATM'
    character(len=1)   :: null = ' '
    integer(i_kind),dimension(7):: idate
    integer(i_kind),dimension(4):: odate
    integer(i_kind) :: iret,nlatm2,nflds
    integer(i_kind) :: k,icount,icount_prev,mm1,i,j,kk
    integer(i_kind) :: mype_hs, mype_ps,nord_int
    integer(i_kind) :: latb, lonb, levs, nframe
    integer(i_kind) :: nfhour, nfminute, nfsecondn, nfsecondd
    integer(i_kind) :: istop = 101
    real(r_kind),allocatable,dimension(:,:) :: grid, grid_v, &
         grid_vor, grid_div, grid_b, grid_b2
    real(r_kind),allocatable,dimension(:,:,:) :: grid_c, grid2, grid_c2
    real(r_kind),allocatable,dimension(:)   :: work, work_vor, work_div, &
         work_v
    real(r_kind),allocatable,dimension(:,:) :: sub, sub_vor, sub_div, &
         sub_v
    real(r_kind),dimension(sp_a%nc):: spec_vor,spec_div
    real(r_kind),allocatable,dimension(:) :: rwork1d0, rwork1d1, rwork1d2
    real(r_kind),allocatable,dimension(:) :: rlats,rlons,clons,slons
    real(4),allocatable,dimension(:) :: r4lats,r4lons
    real(r_kind) :: fhour
    type(nemsio_gfile) :: gfile
    logical diff_res,eqspace
    logical,dimension(1) :: vector
    type(egrid2agrid_parm) :: p_high
    
!******************************************************************************  
!   Initialize variables used below
    mm1=mype+1
    mype_hs=min(1,npe-1)
    mype_ps=0
    nlatm2=grd%nlat-2
    nflds=5*grd%nsig+1
    if(zflag) nflds=nflds+1
    if(vordivflag .or. .not. uvflag)nflds=nflds+2*grd%nsig
!   nflds=npe
    nflds=grd%nsig
    levs=grd%nsig

    allocate( work(grd%itotsub),work_v(grd%itotsub) )
    work=zero
    work_v=zero
    allocate( sub(grd%lat2*grd%lon2,max(grd%nsig,npe)),sub_v(grd%lat2*grd%lon2,max(grd%nsig,npe)) )
    allocate( sub_div(grd%lat2*grd%lon2,max(grd%nsig,npe)),sub_vor(grd%lat2*grd%lon2,max(grd%nsig,npe)) )
    if(mype < nflds)then

      call nemsio_init(iret=iret)
      if (iret /= 0) call error_msg(mype,trim(my_name),trim(filename),null,'init',istop,iret)

      call nemsio_open(gfile,filename,'READ',iret=iret)
      if (iret /= 0) call error_msg(mype,trim(my_name),trim(filename),null,'open',istop+1,iret)

      call nemsio_getfilehead(gfile,iret=iret, nframe=nframe, &
           nfhour=nfhour, nfminute=nfminute, nfsecondn=nfsecondn, nfsecondd=nfsecondd, &
           idate=idate, dimx=lonb, dimy=latb,dimz=levs)
  
      if( nframe /= 0 ) then
         if ( mype == 0 ) &
         write(6,*)trim(my_name),': ***ERROR***  nframe /= 0 for global model read, nframe = ', nframe
         call stop2(101)
      end if

      fhour = float(nfhour) + float(nfminute)/r60 + float(nfsecondn)/float(nfsecondd)/r3600
      odate(1) = idate(4)  !hour
      odate(2) = idate(2)  !month
      odate(3) = idate(3)  !day
      odate(4) = idate(1)  !year
!
!  g_* array already pre-allocate as (lat2,lon2,<nsig>) => 2D and <3D> array
!
      diff_res=.false.
      if(latb /= nlatm2) then
         diff_res=.true.
         if ( mype == 0 ) write(6, &
            '(a,'': different spatial dimension nlatm2 = '',i4,tr1,''latb = '',i4)') &
            trim(my_name),nlatm2,latb
  !      call stop2(101)
      end if
      if(lonb /= grd%nlon) then
         diff_res=.true.
         if ( mype == 0 ) write(6, &
            '(a,'': different spatial dimension nlon   = '',i4,tr1,''lonb = '',i4)') &
            trim(my_name),grd%nlon,lonb
  !      call stop2(101)
      end if
      if(levs /= grd%nsig)then
         if ( mype == 0 ) write(6, &
            '(a,'': inconsistent spatial dimension nsig   = '',i4,tr1,''levs = '',i4)') &
            trim(my_name),grd%nsig,levs
         call stop2(101)
      end if
!
      allocate( grid(grd%nlon,nlatm2), grid_v(grd%nlon,nlatm2) )
      if(diff_res)then
         allocate( grid_b(lonb,latb),grid_c(latb+2,lonb,1),grid2(grd%nlat,grd%nlon,1))
         allocate( grid_b2(lonb,latb),grid_c2(latb+2,lonb,1))
      end if
      allocate( rwork1d0(latb*lonb) )
      allocate( rlats(latb+2),rlons(lonb),clons(lonb),slons(lonb),r4lats(lonb*latb),r4lons(lonb*latb))
      allocate(rwork1d1(latb*lonb))
      call nemsio_getfilehead(gfile,lat=r4lats,iret=iret)
      call nemsio_getfilehead(gfile,lon=r4lons,iret=iret)
      do j=1,latb
        rlats(latb+2-j)=deg2rad*r4lats(lonb/2+(j-1)*lonb)
      end do
      do j=1,lonb
        rlons(j)=deg2rad*r4lons(j)
      end do
      deallocate(r4lats,r4lons)
      rlats(1)=-half*pi
      rlats(latb+2)=half*pi
      do j=1,lonb
         clons(j)=cos(rlons(j))
         slons(j)=sin(rlons(j))
      end do

      nord_int=4
      eqspace=.false.
      call g_create_egrid2agrid(grd%nlat,sp_a%rlats,grd%nlon,sp_a%rlons, &
                              latb+2,rlats,lonb,rlons,&
                              nord_int,p_high,.true.,eqspace)
      deallocate(rlats,rlons)
    end if
!
!   Load values into rows for south and north pole before scattering
!
!   Terrain:  scatter to all mpi tasks
!
    if(zflag)then
       if (mype==mype_hs) then
          call nemsio_readrecv(gfile,'hgt', 'sfc',1,rwork1d0,iret=iret)
          if (iret /= 0) call error_msg(mype,trim(my_name),trim(filename),'hgt','read',istop+2,iret)
          if(diff_res)then
             grid_b=reshape(rwork1d0,(/size(grid_b,1),size(grid_b,2)/))
             vector(1)=.false.
             call fill2_ns(grid_b,grid_c(:,:,1),latb+2,lonb)
             call g_egrid2agrid(p_high,grid_c,grid2,1,1,vector)
             do kk=1,itotsub
               i=ltosi_s(kk)
               j=ltosj_s(kk)
               work(kk)=grid2(i,j,1)
             end do
          else
             grid=reshape(rwork1d0,(/size(grid,1),size(grid,2)/))
             call fill_ns(grid,work)
          end if
       endif
       call mpi_scatterv(work,grd%ijn_s,grd%displs_s,mpi_rtype,&
          g_z,grd%ijn_s(mm1),mpi_rtype,mype_hs,mpi_comm_world,ierror)
    end if

!   Surface pressure:  same procedure as terrain, but handled by task mype_ps
!
    if (mype==mype_ps) then
       call nemsio_readrecv(gfile,'pres','sfc',1,rwork1d0,iret=iret)
       if (iret /= 0) call error_msg(mype,trim(my_name),trim(filename),'pres','read',istop+3,iret)
       rwork1d1 = r0_001*rwork1d0
       if(diff_res)then
          vector(1)=.false.
          grid_b=reshape(rwork1d1,(/size(grid_b,1),size(grid_b,2)/))
          call fill2_ns(grid_b,grid_c(:,:,1),latb+2,lonb)
          call g_egrid2agrid(p_high,grid_c,grid2,1,1,vector)
          do kk=1,itotsub
            i=ltosi_s(kk)
            j=ltosj_s(kk)
            work(kk)=grid2(i,j,1)
          end do
       else
          grid=reshape(rwork1d1,(/size(grid,1),size(grid,2)/)) ! convert Pa to cb
          call fill_ns(grid,work)
       endif
    endif
    call mpi_scatterv(work,grd%ijn_s,grd%displs_s,mpi_rtype,&
       g_ps,grd%ijn_s(mm1),mpi_rtype,mype_ps,mpi_comm_world,ierror)

!   Divergence and voriticity.  Compute u and v from div and vor
    sub_vor=zero
    sub_div=zero
    sub    =zero
    sub_v  =zero
    icount     =0
    icount_prev=1
    allocate( work_vor(grd%itotsub),work_div(grd%itotsub) )
    do k=1,levs
       icount=icount+1
       if (mype==mod(icount-1,npe)) then
          ! Convert grid u,v to div and vor
          call nemsio_readrecv(gfile,'ugrd','mid layer',k,rwork1d0,iret=iret)
          if (iret /= 0) call error_msg(mype,trim(my_name),trim(filename),'ugrd','read',istop+4,iret)
          call nemsio_readrecv(gfile,'vgrd','mid layer',k,rwork1d1,iret=iret)
          if (iret /= 0) call error_msg(mype,trim(my_name),trim(filename),'vgrd','read',istop+5,iret)
          if(diff_res)then
             grid_b=reshape(rwork1d0,(/size(grid_b,1),size(grid_b,2)/))
             grid_b2=reshape(rwork1d1,(/size(grid_b,1),size(grid_b,2)/))
             vector(1)=.true.
             call filluv2_ns(grid_b,grid_b2,grid_c(:,:,1),grid_c2(:,:,1),latb+2,lonb,slons,clons)
             call g_egrid2agrid(p_high,grid_c,grid2,1,1,vector)
             do kk=1,itotsub
               i=ltosi_s(kk)
               j=ltosj_s(kk)
               work(kk)=grid2(i,j,1)
             end do
             do j=1,grd%nlon
               do i=2,grd%nlat-1
                 grid(j,grd%nlat-i)=grid2(i,j,1)
               end do
             end do
             call g_egrid2agrid(p_high,grid_c2,grid2,1,1,vector)
             do kk=1,itotsub
               i=ltosi_s(kk)
               j=ltosj_s(kk)
               work_v(kk)=grid2(i,j,1)
             end do
             do j=1,grd%nlon
               do i=2,grd%nlat-1
                 grid_v(j,grd%nlat-i)=grid2(i,j,1)
               end do
             end do
          else
             grid=reshape(rwork1d0,(/size(grid,1),size(grid,2)/))
             grid_v=reshape(rwork1d1,(/size(grid_v,1),size(grid_v,2)/))
             call filluv_ns(grid,grid_v,work,work_v)
          end if

          if(vordivflag .or. .not. uvflag)then
             
             allocate( grid_vor(grd%nlon,nlatm2), grid_div(grd%nlon,nlatm2) )
             call general_sptez_v(sp_a,spec_div,spec_vor,grid,grid_v,-1)
             call general_sptez_s_b(sp_a,sp_a,spec_div,grid_div,1)
             call general_sptez_s_b(sp_a,sp_a,spec_vor,grid_vor,1)

          ! Load values into rows for south and north pole
             call fill_ns(grid_div,work_div)
             call fill_ns(grid_vor,work_vor)
             deallocate(grid_vor,grid_div)
          end if
       endif
       ! Scatter to sub
       if (mod(icount,npe)==0 .or. icount==levs) then
          if(vordivflag .or. .not. uvflag)then
             call mpi_alltoallv(work_vor,grd%ijn_s,grd%displs_s,mpi_rtype,&
                sub_vor(1,icount_prev),grd%irc_s,grd%ird_s,mpi_rtype,&
                mpi_comm_world,ierror)
             call mpi_alltoallv(work_div,grd%ijn_s,grd%displs_s,mpi_rtype,&
                sub_div(1,icount_prev),grd%irc_s,grd%ird_s,mpi_rtype,&
                mpi_comm_world,ierror)
          end if
          if(uvflag)then
             call mpi_alltoallv(work,grd%ijn_s,grd%displs_s,mpi_rtype,&
                sub(1,icount_prev),grd%irc_s,grd%ird_s,mpi_rtype,&
                mpi_comm_world,ierror)
             call mpi_alltoallv(work_v,grd%ijn_s,grd%displs_s,mpi_rtype,&
                sub_v(1,icount_prev),grd%irc_s,grd%ird_s,mpi_rtype,&
                mpi_comm_world,ierror)
          end if
          icount_prev=icount+1
       endif
    end do
    deallocate(work_vor,work_div)

    ! Transfer vor,div,u,v into real(r_kind) guess arrays
    call reload(sub_vor,g_vor)
    call reload(sub_div,g_div)
    call reload(sub,g_u)
    call reload(sub_v,g_v)
    deallocate(sub_vor,sub_div)

!   Thermodynamic variable and Specific humidity:  communicate to all tasks
!
    sub=zero
    icount=0
    icount_prev=1
    do k=1,levs
       icount=icount+1
       if (mype==mod(icount-1,npe)) then

          call nemsio_readrecv(gfile,'spfh','mid layer',k,rwork1d0,iret=iret)
          if (iret /= 0) call error_msg(mype,trim(my_name),trim(filename),'spfh','read',istop+6,iret)
          if(diff_res)then
             grid_b=reshape(rwork1d0,(/size(grid_b,1),size(grid_b,2)/))
             vector(1)=.false.
             call fill2_ns(grid_b,grid_c(:,:,1),latb+2,lonb)
             call g_egrid2agrid(p_high,grid_c,grid2,1,1,vector)
             do kk=1,itotsub
               i=ltosi_s(kk)
               j=ltosj_s(kk)
               work(kk)=grid2(i,j,1)
             end do
          else
             grid=reshape(rwork1d0,(/size(grid,1),size(grid,2)/))
             call fill_ns(grid,work)
          end if

          call nemsio_readrecv(gfile,'tmp','mid layer',k,rwork1d1,iret=iret)
          if (iret /= 0) call error_msg(mype,trim(my_name),trim(filename),'tmp','read',istop+7,iret)
          allocate(rwork1d2(latb*lonb))
          rwork1d2 = rwork1d1*(one+fv*rwork1d0)
          if(diff_res)then
             grid_b=reshape(rwork1d2,(/size(grid_b,1),size(grid_b,2)/))
             vector(1)=.false.
             call fill2_ns(grid_b,grid_c(:,:,1),latb+2,lonb)
             call g_egrid2agrid(p_high,grid_c,grid2,1,1,vector)
             do kk=1,itotsub
               i=ltosi_s(kk)
               j=ltosj_s(kk)
               work_v(kk)=grid2(i,j,1)
             end do
          else
             grid_v=reshape(rwork1d2,(/size(grid_v,1),size(grid_v,2)/))
             call fill_ns(grid_v,work_v)
          end if

          deallocate(rwork1d2)
       endif

       if (mod(icount,npe)==0 .or. icount==levs) then
          call mpi_alltoallv(work_v,grd%ijn_s,grd%displs_s,mpi_rtype,&
             sub_v(1,icount_prev),grd%irc_s,grd%ird_s,mpi_rtype,&
             mpi_comm_world,ierror)
          call mpi_alltoallv(work,grd%ijn_s,grd%displs_s,mpi_rtype,&
             sub(1,icount_prev),grd%irc_s,grd%ird_s,mpi_rtype,&
             mpi_comm_world,ierror)
          icount_prev=icount+1
       endif
    end do
    call reload(sub_v,g_tv)
    call reload(sub,g_q)
    deallocate(sub_v,work_v)

    sub=zero
    icount=0
    icount_prev=1
    do k=1,levs
       icount=icount+1
       if (mype==mod(icount-1,npe)) then
          call nemsio_readrecv(gfile,'o3mr','mid layer',k,rwork1d0,iret=iret)
          if (iret /= 0) call error_msg(mype,trim(my_name),trim(filename),'o3mr','read',istop+8,iret)
          if(diff_res)then
             grid_b=reshape(rwork1d0,(/size(grid_b,1),size(grid_b,2)/))
             vector(1)=.false.
             call fill2_ns(grid_b,grid_c(:,:,1),latb+2,lonb)
             call g_egrid2agrid(p_high,grid_c,grid2,1,1,vector)
             do kk=1,itotsub
               i=ltosi_s(kk)
               j=ltosj_s(kk)
               work(kk)=grid2(i,j,1)
             end do
          else
             grid=reshape(rwork1d0,(/size(grid,1),size(grid,2)/))
             call fill_ns(grid,work)
          end if
       endif
       if (mod(icount,npe)==0 .or. icount==levs) then
          call mpi_alltoallv(work,grd%ijn_s,grd%displs_s,mpi_rtype,&
             sub(1,icount_prev),grd%irc_s,grd%ird_s,mpi_rtype,&
             mpi_comm_world,ierror)
          icount_prev=icount+1
       endif
    end do
    call reload(sub,g_oz)

!   Cloud condensate mixing ratio.

    if (ntracer>2 .or. ncloud>=1) then
       sub=zero
       icount=0
       icount_prev=1
       do k=1,levs
          icount=icount+1
          if (mype==mod(icount-1,npe)) then
             call nemsio_readrecv(gfile,'clwmr','mid layer',k,rwork1d0,iret=iret)
             if (iret /= 0) call error_msg(mype,trim(my_name),trim(filename),'clwmr','read',istop+9,iret)
             if(diff_res)then
                grid_b=reshape(rwork1d0,(/size(grid_b,1),size(grid_b,2)/))
                vector(1)=.false.
                call fill2_ns(grid_b,grid_c(:,:,1),latb+2,lonb)
                call g_egrid2agrid(p_high,grid_c,grid2,1,1,vector)
                do kk=1,itotsub
                  i=ltosi_s(kk)
                  j=ltosj_s(kk)
                  work(kk)=grid2(i,j,1)
                end do
             else
                grid=reshape(rwork1d0,(/size(grid,1),size(grid,2)/))
                call fill_ns(grid,work)
             end if
          endif
          if (mod(icount,npe)==0 .or. icount==levs) then
             call mpi_alltoallv(work,grd%ijn_s,grd%displs_s,mpi_rtype,&
                sub(1,icount_prev),grd%irc_s,grd%ird_s,mpi_rtype,&
                mpi_comm_world,ierror)
             icount_prev=icount+1
          endif
       end do
       call reload(sub,g_cwmr)
    else
       g_cwmr = zero
    endif

    if(mype < nflds)then
       if(diff_res) deallocate(grid_b,grid_b2,grid_c,grid_c2,grid2)
       call destroy_egrid2agrid(p_high)
       deallocate(rwork1d1,clons,slons)
       deallocate(rwork1d0)
       deallocate(grid,grid_v)
       call nemsio_close(gfile,iret=iret)
       if (iret /= 0) call error_msg(mype,trim(my_name),trim(filename),null,'close',istop+9,iret)
    end if
    deallocate(work,sub)

!   Print date/time stamp 
    if ( mype == 0 ) write(6, &
       '(a,'': ges read/scatter,lonb,latb,levs= '',3i6,'',hour= '',f4.1,'',idate= '',4i5)') &
       trim(my_name),lonb,latb,levs,fhour,odate

  end subroutine read_atm_



  subroutine read_sfc_ (filename,mype,fact10,sfct,sno,veg_type,&
       veg_frac,soil_type,soil_temp,soil_moi,isli,sfc_rough,terrain)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    read_nemssfc     read nems surface file
!   prgmmr: Huang            org: np23                date: 2010-02-22
!
! abstract: read nems surface file
!
! program history log:
!   2010-02-22  Huang    Initial version.  Based on read_gfssfc
!   2011-02-14  Huang    Re-arrange the read sequence to be same as model
!                        write sequence.  Also remove unused array.
!
!   input argument list:
!     filename - name of surface guess file
!     mype     - mpi task id
!
!   output argument list:
!     fact10    - 10 meter wind factor
!     sfct      - surface temperature (skin temp)
!     sno       - snow depth
!     veg_type  - vegetation type
!     veg_frac  - vegetation fraction
!     soil_type - soil type
!     soil_temp - soil temperature of first layer
!     soil_moi  - soil moisture of first layer
!     isli      - sea/land/ice mask (subdomain)
!     isli_g    - global sea/land/ice mask
!     sfc_rough - surface roughness
!     terrain   - terrain height
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
    use kinds, only: r_kind,i_kind
    use gridmod, only: nlat_sfc,nlon_sfc
    use constants, only: zero
    use nemsio_module, only:  nemsio_init,nemsio_open,nemsio_close
    use nemsio_module, only:  nemsio_gfile,nemsio_getfilehead,nemsio_readrecv
    implicit none

!   Declare passed variables
    character(LEN=24)                           ,intent(in   ) :: filename
    integer(i_kind)                             ,intent(in   ) :: mype
    integer(i_kind),dimension(nlat_sfc,nlon_sfc),intent(  out) :: isli
    real(r_kind)   ,dimension(nlat_sfc,nlon_sfc),intent(  out) :: fact10,sfct,sno,&
         veg_type,veg_frac,soil_type,soil_temp,soil_moi,sfc_rough,terrain

!   Declare local parameters
    integer(i_kind),parameter:: nsfc=11
    integer(i_kind),dimension(7):: idate
    integer(i_kind),dimension(4):: odate


!   Declare local variables
    character(len=120) :: my_name = 'READ_NEMSSFC'
    character(len=1)   :: null = ' '
    integer(i_kind) :: i,j,k,mm1
    integer(i_kind) :: iret, nframe, lonb, latb
    integer(i_kind) :: nfhour, nfminute, nfsecondn, nfsecondd
    integer(i_kind) :: istop = 102
    real(r_kind)    :: sumn, sums, fhour
    real(r_kind), allocatable, dimension(:,:)   :: rwork2d
    real(r_kind), allocatable, dimension(:,:,:) :: work, sfcges

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!  Define read variable property   !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
    type(nemsio_gfile) :: gfile
!-----------------------------------------------------------------------------
    mm1=mype+1

    call nemsio_init(iret=iret)
    if (iret /= 0) call error_msg(mype,trim(my_name),null,null,'init',istop,iret)

    call nemsio_open(gfile,filename,'READ',iret=iret)
    if (iret /= 0) call error_msg(mype,trim(my_name),trim(filename),null,'open',istop,iret)

    call nemsio_getfilehead(gfile, idate=idate, iret=iret, nframe=nframe,   &
       nfhour=nfhour, nfminute=nfminute, nfsecondn=nfsecondn, nfsecondd=nfsecondd, &
       dimx=lonb, dimy=latb )

    if( nframe /= 0 ) then
       if ( mype == 0 ) &
       write(6,*)trim(my_name),': ***ERROR***  nframe /= 0 for global model read, nframe = ', nframe
       call stop2(102)
    end if

    fhour = float(nfhour) + float(nfminute)/r60 + float(nfsecondn)/float(nfsecondd)/r3600
    odate(1) = idate(4)  !hour
    odate(2) = idate(2)  !month
    odate(3) = idate(3)  !day
    odate(4) = idate(1)  !year

    if ( (latb /= nlat_sfc-2) .or. (lonb /= nlon_sfc) ) then
       if ( mype == 0 ) write(6, &
          '(a,'': inconsistent spatial dimension '',''nlon,nlatm2 = '',2(i4,tr1),''-vs- sfc file lonb,latb = '',i4)') &
          trim(my_name),nlon_sfc,nlat_sfc-2,lonb,latb
       call stop2(102)
    endif
!
!   Load surface fields into local work array
!   Follow NEMS/GFS sfcf read order
!
    allocate(work(lonb,latb,nsfc),sfcges(latb+2,lonb,nsfc))
    allocate(rwork2d(size(work,1)*size(work,2),size(work,3)))
    work    = zero
    rwork2d = zero

!   Tsea
    call nemsio_readrecv(gfile, 'tmp', 'sfc', 1, rwork2d(:,1), iret=iret)
    if (iret /= 0) call error_msg(mype,trim(my_name),trim(filename),'tmp','read',istop,iret)

!   sheleg
    call nemsio_readrecv(gfile, 'weasd','sfc', 1, rwork2d(:,2), iret=iret)
    if (iret /= 0) call error_msg(mype,trim(my_name),trim(filename),'weasd','read',istop,iret)

!   zorl
    call nemsio_readrecv(gfile, 'sfcr', 'sfc', 1, rwork2d(:,3),iret=iret)
    if (iret /= 0) call error_msg(mype,trim(my_name),trim(filename),'sfcr','read',istop,iret)

!   slmsk
    call nemsio_readrecv(gfile, 'land', 'sfc', 1, rwork2d(:,4), iret=iret)
    if (iret /= 0) call error_msg(mype,trim(my_name),trim(filename),'land','read',istop,iret)

!   vfrac
    call nemsio_readrecv(gfile, 'veg',  'sfc', 1, rwork2d(:,5), iret=iret)
    if (iret /= 0) call error_msg(mype,trim(my_name),trim(filename),'veg','read',istop,iret)

!   f10m
    call nemsio_readrecv(gfile, 'f10m', '10 m above gnd', 1, rwork2d(:,6), iret=iret)
    if (iret /= 0) call error_msg(mype,trim(my_name),trim(filename),'f10m','read',istop,iret)

!   vtype
    call nemsio_readrecv(gfile, 'vtype','sfc', 1, rwork2d(:,7), iret=iret)
    if (iret /= 0) call error_msg(mype,trim(my_name),trim(filename),'vtype','read',istop,iret)

!   stype
    call nemsio_readrecv(gfile, 'sotyp','sfc', 1, rwork2d(:,8), iret=iret)
    if (iret /= 0) call error_msg(mype,trim(my_name),trim(filename),'sotyp','read',istop,iret)

!   orog
    call nemsio_readrecv(gfile, 'orog', 'sfc', 1, rwork2d(:,9),iret=iret)
    if (iret /= 0) call error_msg(mype,trim(my_name),trim(filename),'orog','read',istop,iret)

!   smc
    call nemsio_readrecv(gfile, 'smc', 'soil layer', 1, rwork2d(:,10), iret=iret)
    if (iret /= 0) call error_msg(mype,trim(my_name),trim(filename),'smc','read',istop,iret)

!   stc
    call nemsio_readrecv(gfile, 'stc',  'soil layer', 1, rwork2d(:,11), iret=iret)
    if (iret /= 0) call error_msg(mype,trim(my_name),trim(filename),'stc','read',istop,iret)

!   Fill surface guess array
    do k=1,nsfc
       work(:,:,k)=reshape(rwork2d(:,k),(/size(work,1),size(work,2)/))

!      Compute mean for southern- and northern-most rows
!      of surface guess array
       sumn = zero
       sums = zero
       do i=1, lonb
          sumn = work(i,1,k)    + sumn
          sums = work(i,latb,k) + sums
       end do
       sumn = sumn/float(lonb)
       sums = sums/float(lonb)

!      Transfer from local work array to surface guess array
       do j = 1, lonb
          sfcges(1,j,k)=sums
          sfcges(latb+2,j,k)=sumn
          do i=2,latb+1
             sfcges(i,j,k) = work(j,latb+2-i,k)
          end do
       end do

!   End of loop over data records
    end do

!   Deallocate local work arrays
    deallocate(work,rwork2d)

!   Load data into output arrays

    do j=1,lonb
       do i=1,latb+2
          sfct(i,j)      = sfcges(i,j,1)
          sno(i,j)       = sfcges(i,j,2)
          sfc_rough(i,j) = sfcges(i,j,3)
          isli(i,j)      = nint(sfcges(i,j,4)+0.0000001_r_kind)
          veg_frac(i,j)  = sfcges(i,j,5)
          fact10(i,j)    = sfcges(i,j,6)
          veg_type(i,j)  = sfcges(i,j,7)
          soil_type(i,j) = sfcges(i,j,8)
          terrain(i,j)   = sfcges(i,j,9)
          soil_moi(i,j)  = sfcges(i,j,10)
          soil_temp(i,j) = sfcges(i,j,11)
       end do
    end do
    deallocate(sfcges)

    call nemsio_close(gfile,iret=iret)
    if (iret /= 0) call error_msg(mype,trim(my_name),trim(filename),null,'close',istop,iret)
!
!   Print date/time stamp
    if ( mype == 0 ) write(6, &
       '(a,'': sfc read,nlon,nlat= '',2i6,'',hour= '',f4.1,'',idate= '',4i5)') &
       trim(my_name),lonb,latb,fhour,odate

  end subroutine read_sfc_

  subroutine read_sfc_nst_ (filename,mype,tref,dt_cool,z_c,dt_warm,z_w,c_0,c_d,w_0,w_d)

!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    read_sfc_nst     read nems nst surface guess file (quadratic 
!                                 Gaussin grids) without scattering to tasks
!   prgmmr: Huang            org: np23                date: 2011-11-01
!
! abstract: read nems surface NST file
!
! program history log:
!   2011-11-01  Huang    Initial version based on sub read_gfsnst
!
!   input argument list:
!     filename - name of surface guess file
!     mype     - mpi task id
!
!   output argument list:
!   tref     (:,:)                ! oceanic foundation temperature
!   dt_cool  (:,:)                ! sub-layer cooling amount at sub-skin layer
!   z_c      (:,:)                ! depth of sub-layer cooling layer
!   dt_warm  (:,:)                ! diurnal warming amount at sea surface (skin layer)
!   z_w      (:,:)                ! depth of diurnal warming layer
!   c_0      (:,:)                ! coefficient to calculate d(Tz)/d(tr) in dimensionless
!   c_d      (:,:)                ! coefficient to calculate d(Tz)/d(tr) in m^-1
!   w_0      (:,:)                ! coefficient to calculate d(Tz)/d(tr) in dimensionless
!   w_d      (:,:)                ! coefficient to calculate d(Tz)/d(tr) in m^-1
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
    use kinds, only: r_kind,i_kind
    use gridmod, only: nlat_sfc,nlon_sfc
    use constants, only: zero,two
    use nemsio_module, only:  nemsio_init,nemsio_open,nemsio_close
    use nemsio_module, only:  nemsio_gfile,nemsio_getfilehead,nemsio_readrecv
    implicit none

!   Declare passed variables
    character(LEN=6)                            ,intent(in   ) :: filename
    integer(i_kind)                             ,intent(in   ) :: mype
    real(r_kind)   ,dimension(nlat_sfc,nlon_sfc),intent(  out) :: tref,dt_cool, &
         z_c,dt_warm,z_w,c_0,c_d,w_0,w_d

!   Declare local parameters
    integer(i_kind),parameter:: n_nst=9
    integer(i_kind),dimension(7):: idate
    integer(i_kind),dimension(4):: odate

!   Declare local variables
    character(len=120) :: my_name = 'READ_NEMSNST'
    character(len=1)   :: null = ' '
    integer(i_kind) :: i,j,k,mm1,latb,lonb
    integer(i_kind) :: iret, nframe
    integer(i_kind) :: nfhour, nfminute, nfsecondn, nfsecondd
    integer(i_kind) :: istop = 103
    real(r_kind)    :: sumn, sums, fhour
    real(r_kind), allocatable, dimension(:,:)   :: rwork2d
    real(r_kind), allocatable, dimension(:,:,:) :: work, nstges

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!  Define read variable property   !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
    type(nemsio_gfile) :: gfile
!-----------------------------------------------------------------------------
    mm1=mype+1

    call nemsio_init(iret=iret)
    if (iret /= 0) call error_msg(mype,trim(my_name),null,null,'init',istop,iret)

    call nemsio_open(gfile,trim(filename),'READ',iret=iret)
    if (iret /= 0) call error_msg(mype,trim(my_name),trim(filename),null,'open',istop,iret)

    call nemsio_getfilehead(gfile, idate=idate, iret=iret, nframe=nframe,   &
       nfhour=nfhour, nfminute=nfminute, nfsecondn=nfsecondn, nfsecondd=nfsecondd, &
       dimx=lonb, dimy=latb )

    if( nframe /= 0 ) then
       if ( mype == 0 ) &
       write(6,*)trim(my_name),': ***ERROR***  nframe /= 0 for global model read, nframe = ', nframe
       call stop2(istop)
    end if

    fhour = float(nfhour) + float(nfminute)/r60 + float(nfsecondn)/float(nfsecondd)/r3600
    odate(1) = idate(4)  !hour
    odate(2) = idate(2)  !month
    odate(3) = idate(3)  !day
    odate(4) = idate(1)  !year

    if ( (latb /= nlat_sfc-2) .or. (lonb /= nlon_sfc) ) then
       if ( mype == 0 ) &
          write(6,'(a,'': inconsistent spatial dimension nlon,nlatm2 = '',2(i4,tr1),''-vs- sfc file lonb,latb = '',i4)') &
          trim(my_name),nlon_sfc,nlat_sfc-2,lonb,latb
       call stop2(102)
    endif
!
!   Load surface fields into local work array
!   Follow NEMS/GFS sfcf read order
!
    allocate(work(lonb,latb,n_nst),nstges(latb+2,lonb,n_nst))
    allocate(rwork2d(size(work,1)*size(work,2),size(work,3)))
    work    = zero
    rwork2d = zero

!   Tref
    call nemsio_readrecv(gfile, 'tref', 'sfc', 1, rwork2d(:,1), iret=iret)
    if (iret /= 0) call error_msg(mype,trim(my_name),trim(filename),'tref','read',istop,iret)

!   dt_cool
    call nemsio_readrecv(gfile, 'dtcool','sfc', 1, rwork2d(:,2), iret=iret)
    if (iret /= 0) call error_msg(mype,trim(my_name),trim(filename),'dt_cool','read',istop,iret)

!   z_c
    call nemsio_readrecv(gfile, 'zc',  'sfc', 1, rwork2d(:,3),iret=iret)
    if (iret /= 0) call error_msg(mype,trim(my_name),trim(filename),'z_c','read',istop,iret)

!   xt
    call nemsio_readrecv(gfile, 'xt',   'sfc', 1, rwork2d(:,4), iret=iret)
    if (iret /= 0) call error_msg(mype,trim(my_name),trim(filename),'xt','read',istop,iret)

!   xz
    call nemsio_readrecv(gfile, 'xz',   'sfc', 1, rwork2d(:,5), iret=iret)
    if (iret /= 0) call error_msg(mype,trim(my_name),trim(filename),'xz','read',istop,iret)
!
    rwork2d(:,4) = two*rwork2d(:,4)/rwork2d(:,5)   
!
!   c_0
    call nemsio_readrecv(gfile, 'c0',  'sfc', 1, rwork2d(:,6), iret=iret)
    if (iret /= 0) call error_msg(mype,trim(my_name),trim(filename),'c_0','read',istop,iret)

!   c_d
    call nemsio_readrecv(gfile, 'cd',  'sfc', 1, rwork2d(:,7), iret=iret)
    if (iret /= 0) call error_msg(mype,trim(my_name),trim(filename),'c_d','read',istop,iret)

!   w_0
    call nemsio_readrecv(gfile, 'w0',  'sfc', 1, rwork2d(:,8), iret=iret)
    if (iret /= 0) call error_msg(mype,trim(my_name),trim(filename),'w_0','read',istop,iret)

!   w_d
    call nemsio_readrecv(gfile, 'wd',  'sfc', 1, rwork2d(:,9),iret=iret)
    if (iret /= 0) call error_msg(mype,trim(my_name),trim(filename),'w_d','read',istop,iret)

!   Fill surface guess array
    do k=1,n_nst
       work(:,:,k)=reshape(rwork2d(:,k),(/size(work,1),size(work,2)/))

!      Compute mean for southern- and northern-most rows
!      of surface guess array
       sumn = zero
       sums = zero
       do i=1, lonb
          sumn = work(i,1,k)    + sumn
          sums = work(i,latb,k) + sums
       end do
       sumn = sumn/float(lonb)
       sums = sums/float(lonb)

!      Transfer from local work array to surface guess array
       do j = 1, lonb
          nstges(1,j,k)=sums
          nstges(latb+2,j,k)=sumn
          do i=2,latb+1
             nstges(i,j,k) = work(j,latb+2-i,k)
          end do
       end do

!   End of loop over data records
    end do

!   Deallocate local work arrays
    deallocate(work,rwork2d)

!   Load data into output arrays

    do j=1,lonb
       do i=1,latb+2
          tref(i,j)    = nstges(i,j,1)
          dt_cool(i,j) = nstges(i,j,2)
          z_c(i,j)     = nstges(i,j,3)
          dt_warm(i,j) = nstges(i,j,4)
          z_w(i,j)     = nstges(i,j,5)
          c_0(i,j)     = nstges(i,j,6)
          c_d(i,j)     = nstges(i,j,7)
          w_0(i,j)     = nstges(i,j,8)
          w_d(i,j)     = nstges(i,j,9)
       end do
    end do
    deallocate(nstges)

    call nemsio_close(gfile,iret=iret)
    if (iret /= 0) call error_msg(mype,trim(my_name),trim(filename),null,'close',istop,iret)
!
!   Print date/time stamp
    if ( mype == 0 ) &
       write(6,'(a,'': nst sfc read, nlon,nlat= '', 2i6, '', hour= '',f4.1,'', idate= '',4i5)') &
          trim(my_name),lonb,latb,fhour,odate
  end subroutine read_sfc_nst_



  subroutine write_atm_ (grd,sp_a,filename,mype,mype_out,sub_ps,&
       sub_tv,sub_q,sub_oz,sub_cwmr,sub_prsl,&
       sub_u,sub_v,sub_prsi,ibin)

!$$$  subprogram documentation block
!                .      .    .
! subprogram:    write_nemsatm --- Gather, transform, and write out 
!      
!   prgmmr: Huang            org: np23                date: 2010-02-22
!
! abstract: This routine gathers fields needed for the GSI analysis
!           file from subdomains and then transforms the fields from
!           analysis grid to model guess grid, then written to an 
!           atmospheric analysis file.
!
! program history log:
!   2010-02-22  Huang    Initial version.  Based on write_gfsatm
!   2011-02-14  Huang    Re-arrange the write sequence to be same as model
!                        read/rite sequence.
!   2013-10-25  todling  reposition load_grid to commvars
!
!   input argument list:
!     filename  - file to open and write to
!     mype      - mpi task number
!     mype_out  - mpi task to write output file
!     sub_ps    - surface pressure on subdomains
!     sub_tv    - virtual temperature on subdomains
!     sub_q     - specific humidity on subdomains
!     sub_oz    - ozone on subdomains
!     sub_cwmr  - cloud condensate mixing ratio on subdomains
!     sub_prsl  - layer midpoint pressure
!     sub_u     - zonal wind
!     sub_v     - meridional wind
!     sub_prsi  - interface  pressure
!     ibin      - time bin
!
!   output argument list:
!
! attributes:
!   language: f90
!   machines: ibm RS/6000 SP; SGI Origin 2000; Compaq HP
!
!$$$ end documentation block

! !USES:
    use kinds, only: r_kind,i_kind
    
    use constants, only: r1000,fv,one,zero
  
    use mpimod, only: mpi_rtype
    use mpimod, only: mpi_comm_world
    use mpimod, only: ierror
    use mpimod, only: npe
    
    use guess_grids, only: ntguessig, ifilesig
    
    use gridmod, only: ntracer
    use gridmod, only: ncloud
    use gridmod, only: strip,itotsub,iglobal,jcap_b
    
    use general_commvars_mod, only: load_grid,fill2_ns,filluv2_ns,ltosj_s,ltosi_s,ltosj,ltosi
    use general_specmod, only: spec_vars

    use obsmod, only: iadate
    
    use nemsio_module, only: nemsio_gfile,nemsio_open,nemsio_init,&
         nemsio_getfilehead,nemsio_close,nemsio_writerecv,nemsio_readrecv
    use gsi_4dvar, only: ibdate,nhr_obsbin
    use general_sub2grid_mod, only: sub2grid_info
    use egrid2agrid_mod,only: g_egrid2agrid,g_create_egrid2agrid,egrid2agrid_parm,destroy_egrid2agrid
    use constants, only: two,pi,half,deg2rad
  
    implicit none

! !INPUT PARAMETERS:

    character(LEN=24)                          ,intent(in   ) :: filename     ! file to open and write to

    integer(i_kind)                            ,intent(in   ) :: mype      ! mpi task number
    integer(i_kind)                            ,intent(in   ) :: mype_out  ! mpi task to write output file
    integer(i_kind)                            ,intent(in   ) :: ibin      ! time bin
    
    real(r_kind),parameter:: r0_001 = 0.001_r_kind
    type(sub2grid_info)                    ,intent(in   ) :: grd
    real(r_kind),dimension(grd%lat2,grd%lon2)          ,intent(in   ) :: sub_ps   ! surface pressure on subdomains
    real(r_kind),dimension(grd%lat2,grd%lon2,grd%nsig)     ,intent(in   ) :: sub_tv   ! virtual temperature on subdomains
    real(r_kind),dimension(grd%lat2,grd%lon2,grd%nsig)     ,intent(in   ) :: sub_q    ! specific humidity on subdomains
    real(r_kind),dimension(grd%lat2,grd%lon2,grd%nsig)     ,intent(in   ) :: sub_oz   ! ozone on subdomains
    real(r_kind),dimension(grd%lat2,grd%lon2,grd%nsig)     ,intent(in   ) :: sub_cwmr ! cloud condensate mixing ratio on subdomains
    real(r_kind),dimension(grd%lat2,grd%lon2,grd%nsig)     ,intent(in   ) :: sub_prsl ! layer midpoint pressure
    real(r_kind),dimension(grd%lat2,grd%lon2,grd%nsig)     ,intent(in   ) :: sub_u    ! zonal wind
    real(r_kind),dimension(grd%lat2,grd%lon2,grd%nsig)     ,intent(in   ) :: sub_v    ! meridional wind
    real(r_kind),dimension(grd%lat2,grd%lon2,grd%nsig+1)   ,intent(in   ) :: sub_prsi ! interface  pressure
    type(spec_vars)                        ,intent(in   ) :: sp_a

!-------------------------------------------------------------------------

    character(6):: fname_ges
    character(len=120) :: my_name = 'WRITE_NEMSATM'
    character(len=1)   :: null = ' '
    integer(i_kind),dimension(7):: idate, jdate
    integer(i_kind),dimension(4):: odate
    integer(i_kind) :: k, mm1, nlatm2, nord_int, i, j, kk
    integer(i_kind) :: iret, lonb, latb, levs
    integer(i_kind) :: nfhour, nfminute, nfsecondn, nfsecondd
    integer(i_kind) :: istop = 104
    integer(i_kind),dimension(5):: mydate
    integer(i_kind),dimension(8) :: ida,jda
    real(r_kind),dimension(5)    :: fha
    real(r_kind)    :: fhour
    
    real(r_kind),dimension(grd%lat1*grd%lon1)     :: psm
    real(r_kind),dimension(grd%lat2,grd%lon2,grd%nsig):: sub_dp
    real(r_kind),dimension(grd%lat1*grd%lon1,grd%nsig):: tvsm,prslm, usm, vsm
    real(r_kind),dimension(grd%lat1*grd%lon1,grd%nsig):: dpsm, qsm, ozsm, cwsm
    real(r_kind),dimension(max(grd%iglobal,grd%itotsub))     :: work1,work2
    real(r_kind),dimension(grd%nlon,grd%nlat-2):: grid
    real(r_kind),allocatable,dimension(:) :: rwork1d,rwork1d1,rlats,rlons,clons,slons
    real(4),allocatable,dimension(:) :: r4lats,r4lons
    real(r_kind),allocatable,dimension(:,:) :: grid_b,grid_b2
    real(r_kind),allocatable,dimension(:,:,:) :: grid_c, grid3, grid_c2

    type(nemsio_gfile) :: gfile,gfileo
    logical diff_res,eqspace
    logical,dimension(1) :: vector
    type(egrid2agrid_parm) :: p_low,p_high

!*************************************************************************
!   Initialize local variables
    mm1=mype+1
    nlatm2=grd%nlat-2
    diff_res=.false.

    if(sp_a%jcap /= jcap_b) then
       if ( mype == 0 ) write(6, &
          '('' dual resolution for nems sp_a%jcap,jcap_b = '',2i6)') &
          sp_a%jcap,jcap_b
        diff_res=.true.
    end if


!   Single task writes analysis data to analysis file
    if (mype==mype_out) then
       write(fname_ges,'(''sigf'',i2.2)') ifilesig(ntguessig)
!
!      Read header information from first guess file.
       call nemsio_init(iret)
       if (iret /= 0) call error_msg(0,trim(my_name),null,null,'init',istop,iret)

       call nemsio_open(gfile,trim(fname_ges),'read',iret)
       if (iret /= 0) call error_msg(0,trim(my_name),trim(fname_ges),null,'open',istop,iret)

       call nemsio_getfilehead(gfile, iret=iret, nfhour=nfhour,        &
          nfminute=nfminute, nfsecondn=nfsecondn, nfsecondd=nfsecondd, &
          idate=idate, dimx=lonb, dimy=latb, dimz=levs )
       if ( iret/=0 ) then
          write(6,*)trim(my_name),': problem with nemsio_getfilehead, Status = ',iret
          call stop2(103)
       end if
       if(levs/=grd%nsig) then
          write(6,*)trim(my_name),': problem in data dimension background levs = ',levs,' nsig = ',grd%nsig
          call stop2(103)
       end if

!      copy input header info to output header info
       gfileo=gfile
 
!      Update header information (with ibdate) and write it to analysis file (w/ _open statement).
       mydate=ibdate
       fha(:)=zero ; ida=0; jda=0
       fha(2)=real(nhr_obsbin*(ibin-1))  ! relative time interval in hours
       ida(1)=mydate(1) ! year
       ida(2)=mydate(2) ! month
       ida(3)=mydate(3) ! day
       ida(4)=0         ! time zone
       ida(5)=mydate(4) ! hour

   ! Move date-time forward by nhr_assimilation hours
       call w3movdat(fha,ida,jda)

       jdate(1) = jda(1)     ! analysis year
       jdate(2) = jda(2)     ! analysis month
       jdate(3) = jda(3)     ! analysis day
       jdate(4) = jda(5)     ! analysis hour
       jdate(5) = iadate(5)  ! analysis minute
       jdate(6) = 0          ! analysis scaled seconds
       jdate(7) = idate(7)   ! analysis seconds multiplier

       nfhour   =0       !  new forecast hour, zero at analysis time
       nfminute =0    
       nfsecondn=0     
       nfsecondd=100      ! default for denominator

       fhour = zero
       odate(1) = jdate(4)  !hour
       odate(2) = jdate(2)  !month
       odate(3) = jdate(3)  !day
       odate(4) = jdate(1)  !year
!
!      open new output file with new header gfileo with "write" access. 
!      Use this call to update header as well
!
       call nemsio_open(gfileo,trim(filename),'write',iret=iret, &
          idate=jdate, nfhour=nfhour, nfminute=nfminute, &
          nfsecondn=nfsecondn, nfsecondd=nfsecondd)
       if (iret /= 0) call error_msg(0,trim(my_name),trim(filename),null,'open',istop,iret)

!      Allocate structure arrays to hold data
       allocate(rwork1d(latb*lonb),rwork1d1(latb*lonb))
       if(diff_res)then
          allocate( grid_b(lonb,latb),grid_c(latb+2,lonb,1),grid3(grd%nlat,grd%nlon,1))
          allocate( grid_b2(lonb,latb),grid_c2(latb+2,lonb,1))
          allocate( rlats(latb+2),rlons(lonb),clons(lonb),slons(lonb),r4lats(lonb*latb),r4lons(lonb*latb))
          call nemsio_getfilehead(gfile,lat=r4lats,iret=iret)
          call nemsio_getfilehead(gfile,lon=r4lons,iret=iret)
          do j=1,latb
            rlats(latb+2-j)=deg2rad*r4lats(lonb/2+(j-1)*lonb)
          end do
          rlats(1)=-half*pi
          rlats(latb+2)=half*pi
          do j=1,lonb
            rlons(j)=deg2rad*r4lons(j)
          end do
          do j=1,lonb
             clons(j)=cos(rlons(j))
             slons(j)=sin(rlons(j))
          end do

          nord_int=4
          eqspace=.false.
          call g_create_egrid2agrid(grd%nlat,sp_a%rlats,grd%nlon,sp_a%rlons, &
                                latb+2,rlats,lonb,rlons,&
                                nord_int,p_low,.false.,eqspace=eqspace)
          call g_create_egrid2agrid(latb+2,rlats,lonb,rlons, &
                                grd%nlat,sp_a%rlats,grd%nlon,sp_a%rlons,&
                                nord_int,p_high,.false.,eqspace=eqspace)

          deallocate(rlats,rlons,r4lats,r4lons)
       end if

!   Terrain
!   Write out input file surface height

       call nemsio_readrecv(gfile,'hgt', 'sfc',1,rwork1d,iret=iret)
       if (iret /= 0) call error_msg(0,trim(my_name),trim(filename),'hgt','writeread',istop,iret)
       call nemsio_writerecv(gfileo,'hgt','sfc',1,rwork1d,iret=iret)
       if (iret /= 0) call error_msg(0,trim(my_name),trim(filename),'hgt','write',istop,iret)
    end if

    do k=1,grd%nsig
       sub_dp(:,:,k) = sub_prsi(:,:,k)-sub_prsi(:,:,k+1)
    end do

!   Strip off boundary points from subdomains
    call strip(sub_ps  ,psm)
    call strip(sub_tv  ,tvsm  ,grd%nsig)
    call strip(sub_q   ,qsm   ,grd%nsig)
    call strip(sub_oz  ,ozsm  ,grd%nsig)
    call strip(sub_cwmr,cwsm  ,grd%nsig)
    call strip(sub_dp  ,dpsm  ,grd%nsig)
    call strip(sub_prsl,prslm ,grd%nsig)
    call strip(sub_u   ,usm   ,grd%nsig)
    call strip(sub_v   ,vsm   ,grd%nsig)
!
!   Thermodynamic variable
!   The GSI analysis variable is virtual temperature (Tv).   For NEMSIO
!   output we need the sensible temperature.
!
!   Convert Tv to T
    tvsm = tvsm/(one+fv*qsm)
!   Generate and write analysis fields

!   Surface pressure.  
    call mpi_gatherv(psm,grd%ijn(mm1),mpi_rtype,&
         work1,grd%ijn,grd%displs_g,mpi_rtype,&
         mype_out,mpi_comm_world,ierror)
    if (mype==mype_out) then
       if(diff_res)then
          call nemsio_readrecv(gfile,'pres','sfc',1,rwork1d,iret=iret)
          if (iret /= 0) call error_msg(mype,trim(my_name),trim(filename),'pres','read',istop,iret)
          rwork1d1 = r0_001*rwork1d
          grid_b=reshape(rwork1d1,(/size(grid_b,1),size(grid_b,2)/))
          vector(1)=.false.
          call fill2_ns(grid_b,grid_c(:,:,1),latb+2,lonb)
          call g_egrid2agrid(p_low,grid_c,grid3,1,1,vector)
          do kk=1,grd%iglobal
             i=grd%ltosi(kk)
             j=grd%ltosj(kk)
             grid3(i,j,1)=work1(kk)-grid3(i,j,1)
          end do

          call g_egrid2agrid(p_high,grid3,grid_c,1,1,vector)
          do j=1,latb
             do i=1,lonb
                grid_b(i,j)=r1000*(grid_b(i,j)+grid_c(latb-j+2,i,1))
             end do
          end do
          rwork1d = reshape(grid_b,(/size(rwork1d)/))
       else
          call load_grid(work1,grid)
          grid = grid*r1000
          rwork1d = reshape(grid,(/size(rwork1d)/))
       end if
       call nemsio_writerecv(gfileo,'pres','sfc',1,rwork1d,iret=iret)
       if (iret /= 0) call error_msg(0,trim(my_name),trim(filename),'psfc','write',istop,iret)
    endif

!   Pressure depth
    do k=1,grd%nsig
       call mpi_gatherv(dpsm(1,k),grd%ijn(mm1),mpi_rtype,&
            work1,grd%ijn,grd%displs_g,mpi_rtype,&
            mype_out,mpi_comm_world,ierror)
       if (mype==mype_out) then
          if(diff_res)then
             call nemsio_readrecv(gfile,'dpres','mid layer',k,rwork1d,iret=iret)
             if (iret /= 0) call error_msg(mype,trim(my_name),trim(filename),'pres','read',istop,iret)
             rwork1d1 = r0_001*rwork1d
             grid_b=reshape(rwork1d1,(/size(grid_b,1),size(grid_b,2)/))
             vector(1)=.false.
             call fill2_ns(grid_b,grid_c(:,:,1),latb+2,lonb)
             call g_egrid2agrid(p_low,grid_c,grid3,1,1,vector)
             do kk=1,grd%iglobal
                i=grd%ltosi(kk)
                j=grd%ltosj(kk)
                grid3(i,j,1)=work1(kk)-grid3(i,j,1)
             end do
             call g_egrid2agrid(p_high,grid3,grid_c,1,1,vector)
             do j=1,latb
                do i=1,lonb
                   grid_b(i,j)=r1000*(grid_b(i,j)+grid_c(latb-j+2,i,1))
                end do
             end do
             rwork1d = reshape(grid_b,(/size(rwork1d)/))
          else
             call load_grid(work1,grid)
             grid = grid*r1000
             rwork1d = reshape(grid,(/size(rwork1d)/))
          end if
          call nemsio_writerecv(gfileo,'dpres','mid layer',k,rwork1d,iret=iret)
          if (iret /= 0) call error_msg(0,trim(my_name),trim(filename),'dpres','write',istop,iret)
       endif
    end do

!   Layer mean pressure
    do k=1,grd%nsig
       call mpi_gatherv(prslm(1,k),grd%ijn(mm1),mpi_rtype,&
            work1,grd%ijn,grd%displs_g,mpi_rtype,&
            mype_out,mpi_comm_world,ierror)
       if (mype==mype_out) then
          if(diff_res)then
             call nemsio_readrecv(gfile,'dpres','mid layer',k,rwork1d,iret=iret)
             if (iret /= 0) call error_msg(mype,trim(my_name),trim(filename),'pres','read',istop,iret)
             rwork1d1 = r0_001*rwork1d
             grid_b=reshape(rwork1d1,(/size(grid_b,1),size(grid_b,2)/))
             vector(1)=.false.
             call fill2_ns(grid_b,grid_c(:,:,1),latb+2,lonb)
             call g_egrid2agrid(p_low,grid_c,grid3,1,1,vector)
             do kk=1,grd%iglobal
                i=grd%ltosi(kk)
                j=grd%ltosj(kk)
                grid3(i,j,1)=work1(kk)-grid3(i,j,1)
             end do
             call g_egrid2agrid(p_high,grid3,grid_c,1,1,vector)
             do j=1,latb
                do i=1,lonb
                   grid_b(i,j)=r1000*(grid_b(i,j)+grid_c(latb-j+2,i,1))
                end do
             end do
             rwork1d = reshape(grid_b,(/size(rwork1d)/))
          else
             call load_grid(work1,grid)
             grid = grid*r1000
             rwork1d = reshape(grid,(/size(rwork1d)/))
          end if
          call nemsio_writerecv(gfileo,'pres','mid layer',k,rwork1d,iret=iret)
          if (iret /= 0) call error_msg(0,trim(my_name),trim(filename),'pres','write',istop,iret)
       endif
    end do

    do k=1,grd%nsig
       call mpi_gatherv(usm(1,k),grd%ijn(mm1),mpi_rtype,&
            work1,grd%ijn,grd%displs_g,mpi_rtype,&
            mype_out,mpi_comm_world,ierror)
       call mpi_gatherv(vsm(1,k),grd%ijn(mm1),mpi_rtype,&
            work2,grd%ijn,grd%displs_g,mpi_rtype,&
            mype_out,mpi_comm_world,ierror)
       if (mype==mype_out) then
          if(diff_res)then
             call nemsio_readrecv(gfile,'ugrd','mid layer',k,rwork1d,iret=iret)
             call nemsio_readrecv(gfile,'vgrd','mid layer',k,rwork1d1,iret=iret)
             grid_b=reshape(rwork1d,(/size(grid_b,1),size(grid_b,2)/))
             grid_b2=reshape(rwork1d1,(/size(grid_b,1),size(grid_b,2)/))
             vector(1)=.true.
             call filluv2_ns(grid_b,grid_b2,grid_c(:,:,1),grid_c2(:,:,1),latb+2,lonb,slons,clons)
             call g_egrid2agrid(p_low,grid_c,grid3,1,1,vector)
             do kk=1,grd%iglobal
                i=grd%ltosi(kk)
                j=grd%ltosj(kk)
                grid3(i,j,1)=work1(kk)-grid3(i,j,1)
             end do
             call g_egrid2agrid(p_high,grid3,grid_c,1,1,vector)
             do j=1,latb
                do i=1,lonb
                   grid_b(i,j)=grid_b(i,j)+grid_c(latb-j+2,i,1)
                end do
             end do
             call g_egrid2agrid(p_low,grid_c2,grid3,1,1,vector)
             do kk=1,grd%iglobal
                i=grd%ltosi(kk)
                j=grd%ltosj(kk)
                grid3(i,j,1)=work2(kk)-grid3(i,j,1)
             end do
             call g_egrid2agrid(p_high,grid3,grid_c,1,1,vector)
             do j=1,latb
                do i=1,lonb
                   grid_b2(i,j)=grid_b2(i,j)+grid_c(latb-j+2,i,1)
                end do
             end do
             rwork1d = reshape(grid_b,(/size(rwork1d)/))
             rwork1d1 = reshape(grid_b2,(/size(rwork1d1)/))

          else
             call load_grid(work1,grid)
             rwork1d = reshape(grid,(/size(rwork1d)/))
             call load_grid(work2,grid)
             rwork1d1 = reshape(grid,(/size(rwork1d1)/))
          end if

!   Zonal wind
          call nemsio_writerecv(gfileo,'ugrd','mid layer',k,rwork1d,iret=iret)
          if (iret /= 0) call error_msg(0,trim(my_name),trim(filename),'ugrd','write',istop,iret)
!   Meridional wind
          call nemsio_writerecv(gfileo,'vgrd','mid layer',k,rwork1d1,iret=iret)
          if (iret /= 0) call error_msg(0,trim(my_name),trim(filename),'vgrd','write',istop,iret)
       endif
    end do

!   Thermodynamic variable
    do k=1,grd%nsig
       call mpi_gatherv(tvsm(1,k),grd%ijn(mm1),mpi_rtype,&
            work1,grd%ijn,grd%displs_g,mpi_rtype,&
            mype_out,mpi_comm_world,ierror)
       if (mype == mype_out) then
          if(diff_res)then
             call nemsio_readrecv(gfile,'tmp','mid layer',k,rwork1d,iret=iret)
             if (iret /= 0) call error_msg(mype,trim(my_name),trim(filename),'pres','read',istop,iret)
             grid_b=reshape(rwork1d,(/size(grid_b,1),size(grid_b,2)/))
             vector(1)=.false.
             call fill2_ns(grid_b,grid_c(:,:,1),latb+2,lonb)
             call g_egrid2agrid(p_low,grid_c,grid3,1,1,vector)
             do kk=1,grd%iglobal
                i=grd%ltosi(kk)
                j=grd%ltosj(kk)
                grid3(i,j,1)=work1(kk)-grid3(i,j,1)
             end do
             call g_egrid2agrid(p_high,grid3,grid_c,1,1,vector)
             do j=1,latb
                do i=1,lonb
                   grid_b(i,j)=grid_b(i,j)+grid_c(latb-j+2,i,1)
                end do
             end do
             rwork1d = reshape(grid_b,(/size(rwork1d)/))
          else
             call load_grid(work1,grid)
             rwork1d = reshape(grid,(/size(rwork1d)/))
          end if
          call nemsio_writerecv(gfileo,'tmp','mid layer',k,rwork1d,iret=iret)
          if (iret /= 0) call error_msg(0,trim(my_name),trim(filename),'tmp','write',istop,iret)
       endif
    end do

!   Specific humidity
    do k=1,grd%nsig
       call mpi_gatherv(qsm(1,k),grd%ijn(mm1),mpi_rtype,&
            work1,grd%ijn,grd%displs_g,mpi_rtype,&
            mype_out,mpi_comm_world,ierror)
       if (mype == mype_out) then
          if(diff_res)then
             call nemsio_readrecv(gfile,'spfh','mid layer',k,rwork1d,iret=iret)
             if (iret /= 0) call error_msg(mype,trim(my_name),trim(filename),'pres','read',istop,iret)
             grid_b=reshape(rwork1d,(/size(grid_b,1),size(grid_b,2)/))
             vector(1)=.false.
             call fill2_ns(grid_b,grid_c(:,:,1),latb+2,lonb)
             call g_egrid2agrid(p_low,grid_c,grid3,1,1,vector)
             do kk=1,grd%iglobal
                i=grd%ltosi(kk)
                j=grd%ltosj(kk)
                grid3(i,j,1)=work1(kk)-grid3(i,j,1)
             end do
             call g_egrid2agrid(p_high,grid3,grid_c,1,1,vector)
             do j=1,latb
                do i=1,lonb
                   grid_b(i,j)=grid_b(i,j)+grid_c(latb-j+2,i,1)
                end do
             end do
             rwork1d = reshape(grid_b,(/size(rwork1d)/))
          else
             call load_grid(work1,grid)
             rwork1d = reshape(grid,(/size(rwork1d)/))
          end if
          call nemsio_writerecv(gfileo,'spfh','mid layer',k,rwork1d,iret=iret)
          if (iret /= 0) call error_msg(0,trim(my_name),trim(filename),'spfh','write',istop,iret)
       endif
    end do

!   Ozone
    do k=1,grd%nsig
       call mpi_gatherv(ozsm(1,k),grd%ijn(mm1),mpi_rtype,&
            work1,grd%ijn,grd%displs_g,mpi_rtype,&
            mype_out,mpi_comm_world,ierror)
       if (mype == mype_out) then
          if(diff_res)then
             call nemsio_readrecv(gfile,'o3mr','mid layer',k,rwork1d,iret=iret)
             if (iret /= 0) call error_msg(mype,trim(my_name),trim(filename),'pres','read',istop,iret)
             grid_b=reshape(rwork1d,(/size(grid_b,1),size(grid_b,2)/))
             vector(1)=.false.
             call fill2_ns(grid_b,grid_c(:,:,1),latb+2,lonb)
             call g_egrid2agrid(p_low,grid_c,grid3,1,1,vector)
             do kk=1,grd%iglobal
                i=grd%ltosi(kk)
                j=grd%ltosj(kk)
                grid3(i,j,1)=work1(kk)-grid3(i,j,1)
             end do
             call g_egrid2agrid(p_high,grid3,grid_c,1,1,vector)
             do j=1,latb
                do i=1,lonb
                   grid_b(i,j)=grid_b(i,j)+grid_c(latb-j+2,i,1)
                end do
             end do
             rwork1d = reshape(grid_b,(/size(rwork1d)/))
          else
             call load_grid(work1,grid)
             rwork1d = reshape(grid,(/size(rwork1d)/))
          end if
          call nemsio_writerecv(gfileo,'o3mr','mid layer',k,rwork1d,iret=iret)
          if (iret /= 0) call error_msg(0,trim(my_name),trim(filename),'o3mr','write',istop,iret)
       endif
    end do
       
!   Cloud condensate mixing ratio
    if (ntracer>2 .or. ncloud>=1) then
       do k=1,grd%nsig
          call mpi_gatherv(cwsm(1,k),grd%ijn(mm1),mpi_rtype,&
               work1,grd%ijn,grd%displs_g,mpi_rtype,&
               mype_out,mpi_comm_world,ierror)
          if (mype == mype_out) then
             if(diff_res)then
                call nemsio_readrecv(gfile,'clwmr','mid layer',k,rwork1d,iret=iret)
                if (iret /= 0) call error_msg(mype,trim(my_name),trim(filename),'pres','read',istop,iret)
                grid_b=reshape(rwork1d,(/size(grid_b,1),size(grid_b,2)/))
                vector(1)=.false.
                call fill2_ns(grid_b,grid_c(:,:,1),latb+2,lonb)
                call g_egrid2agrid(p_low,grid_c,grid3,1,1,vector)
                do kk=1,grd%iglobal
                   i=grd%ltosi(kk)
                   j=grd%ltosj(kk)
                   grid3(i,j,1)=work1(kk)-grid3(i,j,1)
                end do
                call g_egrid2agrid(p_high,grid3,grid_c,1,1,vector)
                do j=1,latb
                   do i=1,lonb
                      grid_b(i,j)=grid_b(i,j)+grid_c(latb-j+2,i,1)
                   end do
                end do
                rwork1d = reshape(grid_b,(/size(rwork1d)/))
             else
                call load_grid(work1,grid)
                rwork1d = reshape(grid,(/size(rwork1d)/))
             endif
             call nemsio_writerecv(gfileo,'clwmr','mid layer',k,rwork1d,iret=iret)
             if (iret /= 0) call error_msg(0,trim(my_name),trim(filename),'clwmr','write',istop,iret)
          end if
       end do
    endif
!
! Deallocate local array
!
    if (mype==mype_out) then
       if(diff_res) deallocate(grid_b,grid_b2,grid_c,grid_c2,grid3,clons,slons)
       call nemsio_close(gfile,iret)
       if (iret /= 0) call error_msg(0,trim(my_name),trim(fname_ges),null,'close',istop,iret)

       call nemsio_close(gfileo,iret)
       if (iret /= 0) call error_msg(0,trim(my_name),trim(filename),null,'close',istop,iret)
!
! Deallocate local array
!
       deallocate(rwork1d,rwork1d1)
!
       write(6,'(a,'': atm anal written for lonb,latb,levs= '',3i6,'',valid hour= '',f4.1,'',idate= '',4i5)') &
          trim(my_name),lonb,latb,levs,fhour,odate
    endif

  end subroutine write_atm_

  subroutine write_sfc_ (filename,mype,mype_sfc,dsfct)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    write_nemssfc --- Write surface analysis to file
!
!   prgmmr: Huang            org: np23                date: 2010-02-22
!
! abstract:     This routine writes the updated surface analysis.  At
!               this point (20101020) the only surface field update by 
!               the gsi is the skin temperature.  The current (20101020)
!               GDAS setup does use the updated surface file.  Rather,
!               the output from surface cycle is used as the surface
!               analysis for subsequent NEMS/GFS runs.
!
!               The routine gathers surface fields from subdomains, 
!               reformats the data records, and then writes each record
!               to the output file.  
!
!               Since the gsi only update the skin temperature, all
!               other surface fields are simply read from the guess
!               surface file and written to the analysis file.
!
! program history log:
!   2010-02-22  Huang    Initial version.  Based on write_gfssfc
!   2011-04-01  Huang    change type of buffer2, grid2 from single to r_kind
!   2013-10-25  todling - reposition ltosi and others to commvars
!
!   input argument list:
!     filename  - file to open and write to
!     dsfct     - delta skin temperature
!     mype      - mpi task number
!     mype_sfc  - mpi task to write output file
!
!   output argument list:
!
! attributes:
!   language: f90
!   machines: ibm RS/6000 SP; SGI Origin 2000; Compaq HP
!
!$$$ end documentation block

! !USES:
    use kinds, only: r_kind,i_kind
  
    use mpimod, only: mpi_rtype
    use mpimod, only: mpi_comm_world
    use mpimod, only: ierror
    
    use gridmod, only: nlat,nlon
    use gridmod, only: lat1,lon1
    use gridmod, only: lat2,lon2
    use gridmod, only: iglobal
    use gridmod, only: ijn
    use gridmod, only: displs_g
    use gridmod, only: itotsub
    
    use general_commvars_mod, only: ltosi,ltosj

    use obsmod, only: iadate
    
    use constants, only: zero
    
    use nemsio_module, only:  nemsio_init,nemsio_open,nemsio_close,nemsio_readrecv
    use nemsio_module, only:  nemsio_gfile,nemsio_getfilehead
    use nemsio_module, only:  nemsio_readrec, nemsio_writerec, nemsio_writerecv

    implicit none

! !INPUT PARAMETERS:
    character(24)                    ,intent(in   ) :: filename  ! file to open and write to

    real(r_kind),dimension(lat2,lon2),intent(in   ) :: dsfct   ! delta skin temperature

    integer(i_kind)                  ,intent(in   ) :: mype     ! mpi task number
    integer(i_kind)                  ,intent(in   ) :: mype_sfc ! mpi task to write output file

! !OUTPUT PARAMETERS:

!-------------------------------------------------------------------------

!   Declare local parameters
    character( 6),parameter:: fname_ges='sfcf06'
!   Declare local variables
    character(len=120) :: my_name = 'WRITE_NEMSSFC'
    character(len=1)   :: null = ' '
    integer(i_kind),dimension(7):: idate, jdate
    integer(i_kind),dimension(4):: odate
    integer(i_kind) :: i, j, ip1, jp1, ilat, ilon, jj, mm1
    integer(i_kind) :: nlatm2, n, nrec, lonb, latb, iret
    integer(i_kind) :: nfhour, nfminute, nfsecondn, nfsecondd
    integer(i_kind) :: istop = 105
    real(r_kind)    :: fhour

    real(r_kind),dimension(nlon,nlat):: buffer
    real(r_kind),dimension(lat1,lon1):: sfcsub
    real(r_kind),dimension(nlon,nlat):: grid
    real(r_kind),dimension(max(iglobal,itotsub)):: sfcall
    real(r_kind),allocatable,dimension(:,:) :: buffer2, grid2, tsea
    real(r_kind),allocatable,dimension(:)   :: rwork1d

    type(nemsio_gfile) :: gfile, gfileo
!*****************************************************************************

!   Initialize local variables
    mm1=mype+1
    nlatm2=nlat-2

!   Gather skin temperature information from all tasks.  
    do j=1,lon1
       jp1 = j+1
       do i=1,lat1
          ip1 = i+1
          sfcsub(i,j)=dsfct(ip1,jp1)
       end do
    end do
    call mpi_gatherv(sfcsub,ijn(mm1),mpi_rtype,&
         sfcall,ijn,displs_g,mpi_rtype,mype_sfc,&
         mpi_comm_world,ierror)

!   Only MPI task mype_sfc writes the surface file.
    if (mype==mype_sfc) then

!      Reorder updated skin temperature to output format
       do i=1,iglobal
          ilon=ltosj(i)
          ilat=ltosi(i)
          grid(ilon,ilat)=sfcall(i)
       end do
       do j=1,nlat
          jj=nlat-j+1
          do i=1,nlon
             buffer(i,j)=grid(i,jj)
          end do
       end do

!      Read surface guess file
       call nemsio_init(iret=iret)
       if (iret /= 0) call error_msg(0,trim(my_name),null,null,'init',istop,iret)

       call nemsio_open(gfile,fname_ges,'read',iret=iret)
       if (iret /= 0) call error_msg(0,trim(my_name),trim(fname_ges),null,'open',istop,iret)
!
       call nemsio_getfilehead(gfile, nrec=nrec, idate=idate, dimx=lonb, &
          dimy=latb, nfhour=nfhour, nfminute=nfminute, nfsecondn=nfsecondn, &
          nfsecondd=nfsecondd, iret=iret)
!
!      Replace header record date with analysis time from iadate
!
       jdate(1) = iadate(1)  ! analysis year
       jdate(2) = iadate(2)  ! analysis month
       jdate(3) = iadate(3)  ! analysis day
       jdate(4) = iadate(4)  ! analysis hour
       jdate(5) = iadate(5)  ! analysis minute
       jdate(5) = 0          ! analysis minute
       jdate(6) = 0          ! analysis scaled seconds
       jdate(7) = idate(7)   ! analysis seconds multiplier

       nfhour=0       !  new forecast hour, zero at analysis time
       nfminute=0
       nfsecondn=0
       nfsecondd=100      ! default for denominator

       fhour    = zero
       odate(1) = jdate(4)  !hour
       odate(2) = jdate(2)  !month
       odate(3) = jdate(3)  !day
       odate(4) = jdate(1)  !year
!
! Start to write output sfc file : filename
!      open new output file with new header gfileo with "write" access. 
!      Use this call to update header as well
!
!
       gfileo=gfile      ! copy input header info to output header info
                         ! need to do this before nemsio_close(gfile)
       call nemsio_open(gfileo,filename,'write',iret=iret, idate=jdate, nfhour=nfhour,&
          nfminute=nfminute, nfsecondn=nfsecondn, nfsecondd=nfsecondd )
       if (iret /= 0) call error_msg(0,trim(my_name),trim(filename),null,'open',istop,iret)
!
!      First copy entire data from fname_ges to filename, then do selective update
!
       allocate(rwork1d(lonb*latb))
       allocate(buffer2(lonb,latb))
       allocate(grid2(lonb,latb))
       allocate(tsea(lonb,latb))

       do n = 1, nrec
         call nemsio_readrec (gfile, n,rwork1d,iret=iret)
         if ( iret /= 0 ) write(6,*) 'readrec  nrec = ', n, '  Status = ', iret
         call nemsio_writerec(gfileo,n,rwork1d,iret=iret)
         if ( iret /= 0 ) write(6,*) 'writerec nrec = ', n, '  Status = ', iret
       end do
!
! Only sea surface temperature will be updated in the SFC files
!

       call nemsio_readrecv(gfile,'tmp','sfc',1,rwork1d,iret=iret)
       if (iret /= 0) call error_msg(mype,trim(my_name),trim(fname_ges),'tmp','read',istop,iret)
       tsea=reshape(rwork1d,(/size(tsea,1),size(tsea,2)/))

       if ( (latb /= nlatm2) .or. (lonb /= nlon) ) then
          write(6,*)trim(my_name),':  different grid dimensions analysis', &
             ' vs sfc. interpolating sfc temperature nlon,nlat-2=',nlon,  &
             nlatm2,' -vs- sfc file lonb,latb=',lonb,latb
          call sfc_interpolate(buffer,nlon,nlat,buffer2,lonb,latb)
       else
          do j=1,latb
             do i=1,lonb
                buffer2(i,j)=buffer(i,j+1)
             end do
          end do
       endif

       grid2 = tsea + buffer2
       rwork1d = reshape( grid2,(/size(rwork1d)/) )

       deallocate(buffer2)

!      update tsea record
       call nemsio_writerecv(gfileo,'tmp','sfc',1,rwork1d,iret=iret)
       if (iret /= 0) call error_msg(0,trim(my_name),trim(filename),'tmp','write',istop,iret)
       deallocate(rwork1d)

       call nemsio_close(gfile, iret=iret)
       if (iret /= 0) call error_msg(0,trim(my_name),trim(fname_ges),null,'close',istop,iret)

       call nemsio_close(gfileo,iret=iret)
       if (iret /= 0) call error_msg(0,trim(my_name),trim(filename),null,'close',istop,iret)

       write(6,'(a,'': sfc anal written for lonb,latb= '',2i6,'',valid hour= '',f4.1,'',idate= '',4i5)') &
          trim(my_name),lonb,latb,fhour,odate
    endif
  end subroutine write_sfc_

  subroutine write_sfc_nst_ (filename,fname_nst,mype,mype_nst,dsfct)

!$$$  subprogram documentation block
!                .      .    .
! subprogram:    write_sfc_nst --- Write both sfc and nst surface analysis to file
!
!   prgmmr: Huang            org: np23                date: 2011-11-01
!
! abstract:     This routine writes the sfc & nst analysis files and is nst_gsi dependent.
!               Tr (foundation temperature), instead of skin temperature, is the analysis variable.
!               nst_gsi >  2: Tr analysis is on
!               nst_gsi <= 2: Tr analysis is off
!
!               The routine gathers Tr field from subdomains,
!               reformats the data records, and then writes each record
!               to the output files.
!
!               Since the gsi only update the Tr temperature, all
!               other fields in surface are simply read from the guess
!               files and written to the analysis file.
!
! program history log:
!   2011-11-01  Huang    initial version based on routine write_gfs_sfc_nst
!   2013-10-25  todling - reposition ltosi and others to commvars
!
!   input argument list:
!     filename  - file to open and write to for sfc file
!     fname_nst - file to open and write to for nst file
!     dsfct     - delta skin temperature
!     mype      - mpi task number
!     mype_nst  - mpi task to write output file
!
!   output argument list:
!
! attributes:
!   language: f90
!   machines: ibm RS/6000 SP; SGI Origin 2000; Compaq HP
!
!$$$ end documentation block

! !USES:
    use kinds, only: r_kind,i_kind
  
    use mpimod, only: mpi_rtype,mpi_itype
    use mpimod, only: mpi_comm_world
    use mpimod, only: ierror
    
    use gridmod, only: nlat,nlon
    use gridmod, only: lat1,lon1
    use gridmod, only: lat2,lon2
    use gridmod, only: iglobal
    use gridmod, only: ijn
    use gridmod, only: displs_g
    use gridmod, only: itotsub
    
    use general_commvars_mod, only: ltosi,ltosj

    use obsmod, only: iadate
    
    use constants, only: zero,two
    
    use guess_grids, only: isli2
    use radinfo, only: nst_gsi

    use nemsio_module, only:  nemsio_init,nemsio_open,nemsio_close,nemsio_readrecv
    use nemsio_module, only:  nemsio_gfile,nemsio_getfilehead
    use nemsio_module, only:  nemsio_readrec, nemsio_writerec, nemsio_writerecv

    implicit none

! !INPUT PARAMETERS:

    character(24)                    ,intent(in   ) :: filename  ! sfc file to open and write to
    character(24)                    ,intent(in   ) :: fname_nst ! nst file to open and write to
    real(r_kind),dimension(lat2,lon2),intent(in   ) :: dsfct     ! delta skin temperature
    integer(i_kind)                  ,intent(in   ) :: mype      ! mpi task number
    integer(i_kind)                  ,intent(in   ) :: mype_nst  ! mpi task to write output file

! !OUTPUT PARAMETERS:

!-------------------------------------------------------------------------

!   Declare local parameters
    character( 6),parameter:: fname_ges_sfc='sfcf06'
    character( 6),parameter:: fname_ges_nst='nstf06'

!   Declare local variables
    character(len=120) :: my_name = 'WRITE_SFC_NST'
    character(len=1)   :: null = ' '
    integer(i_kind),dimension(7):: idate, jdate
    integer(i_kind),dimension(4):: odate
    integer(i_kind) :: i, j, ip1, jp1, ilat, ilon, jj, mm1
    integer(i_kind) :: lonb, latb, nlatm2, n, nrec, nrec_nst, iret
    integer(i_kind) :: nfhour, nfminute, nfsecondn, nfsecondd
    integer(i_kind) :: istop = 106
    real(r_kind)    :: fhour

    real(r_kind),dimension(nlon,nlat):: buffer
    real(r_kind),dimension(lat1,lon1):: sfcsub
    real(r_kind),dimension(nlon,nlat):: grid
    real(r_kind),dimension(max(iglobal,itotsub)):: sfcall
    real(r_kind),allocatable,dimension(:,:) :: buffer2, grid2, grid2_nst
    real(r_kind),allocatable,dimension(:,:) :: tsea,tref,dt_cool,xt,xz,slmsk
    real(r_kind),allocatable,dimension(:)   :: rwork1d

    integer(i_kind),dimension(nlon,nlat):: isli
    integer(i_kind),dimension(lat1,lon1):: isosub
    integer(i_kind),dimension(nlon,nlat):: igrid
    integer(i_kind),dimension(max(iglobal,itotsub)):: isoall

    type(nemsio_gfile) :: gfile, gfileo, gfile_nst, gfileo_nst

!*****************************************************************************

!   Initialize local variables
    mm1=mype+1
    nlatm2=nlat-2

!   Gather skin temperature information from all tasks.  
    do j=1,lon1
       jp1 = j+1
       do i=1,lat1
          ip1 = i+1
          sfcsub(i,j)=dsfct(ip1,jp1)
       end do
    end do
    call mpi_gatherv(sfcsub,ijn(mm1),mpi_rtype,&
         sfcall,ijn,displs_g,mpi_rtype,mype_nst,&
         mpi_comm_world,ierror)

!   Gather land/ice/sea mask information from all tasks.
    do j=1,lon1
       jp1 = j+1
       do i=1,lat1
          ip1 = i+1
          isosub(i,j)=isli2(ip1,jp1)
       end do
    end do
    call mpi_gatherv(isosub,ijn(mm1),mpi_itype,&
         isoall,ijn,displs_g,mpi_itype,mype_nst,&
         mpi_comm_world,ierror)

!   Only MPI task mype_nst writes the surface file.
    if (mype==mype_nst) then

!      Reorder updated skin temperature to output format
       do i=1,iglobal
          ilon=ltosj(i)
          ilat=ltosi(i)
          grid(ilon,ilat)=sfcall(i)
       end do
       do j=1,nlat
          jj=nlat-j+1
          do i=1,nlon
             buffer(i,j)=grid(i,jj)
          end do
       end do

!      Record updated isli to output format
       do i=1,iglobal
          ilon=ltosj(i)
          ilat=ltosi(i)
          igrid(ilon,ilat)=isoall(i)
       end do
       do j=1,nlat
          jj=nlat-j+1
          do i=1,nlon
             isli(i,j)=igrid(i,jj)
          end do
       end do
!
!      set dsfct to be zero over non-water grids
!
       do j=1,nlat
         do i=1,nlon
           if ( isli(i,j) > 0 ) then
             buffer(i,j)=zero
           endif
         end do
       end do

!      Read surface guess file
       call nemsio_init(iret=iret)
       if (iret /= 0) call error_msg(0,trim(my_name),null,null,'init',istop,iret)

       call nemsio_open(gfile,trim(fname_ges_sfc),'read',iret=iret)
       if (iret /= 0) call error_msg(0,trim(my_name),trim(fname_ges_sfc),null,'open',istop,iret)
!
       call nemsio_getfilehead(gfile, nrec=nrec, idate=idate, &
          dimx=lonb, dimy=latb, nfhour=nfhour, nfminute=nfminute, &
          nfsecondn=nfsecondn, nfsecondd=nfsecondd, iret=iret)

       call nemsio_open(gfile_nst,trim(fname_ges_nst),'read',iret=iret)
       if (iret /= 0) call error_msg(0,trim(my_name),trim(fname_ges_nst),null,'open',istop,iret)
       call nemsio_getfilehead(gfile_nst, nrec=nrec_nst, iret=iret)
!
!      Replace header record date with analysis time from iadate
!
       jdate(1) = iadate(1)  ! analysis year
       jdate(2) = iadate(2)  ! analysis month
       jdate(3) = iadate(3)  ! analysis day
       jdate(4) = iadate(4)  ! analysis hour
       jdate(5) = iadate(5)  ! analysis minute
       jdate(5) = 0          ! analysis minute
       jdate(6) = 0          ! analysis scaled seconds
       jdate(7) = idate(7)   ! analysis seconds multiplier

       nfhour=0       !  new forecast hour, zero at analysis time
       nfminute=0
       nfsecondn=0
       nfsecondd=100      ! default for denominator

       fhour    = zero
       odate(1) = jdate(4)  !hour
       odate(2) = jdate(2)  !month
       odate(3) = jdate(3)  !day
       odate(4) = jdate(1)  !year
!
! Start to write output sfc file : filename
!      open new output file with new header gfileo and gfileo_nst with "write" access. 
!      Use this call to update header as well
!
       gfileo=gfile          ! copy input header info to output header info
                             ! need to do this before nemsio_close(gfile)
       call nemsio_open(gfileo,trim(filename),'write',iret=iret, idate=jdate, nfhour=nfhour,&
          nfminute=nfminute, nfsecondn=nfsecondn, nfsecondd=nfsecondd )
       if (iret /= 0) call error_msg(0,trim(my_name),trim(filename),null,'open',istop,iret)
!
       gfileo_nst=gfile_nst  ! copy input header info to output header info
                             ! need to do this before nemsio_close(gfile_nst)
       call nemsio_open(gfileo_nst,trim(fname_nst),'write',iret=iret, idate=jdate, nfhour=nfhour,&
          nfminute=nfminute, nfsecondn=nfsecondn, nfsecondd=nfsecondd )
       if (iret /= 0) call error_msg(0,trim(my_name),trim(fname_nst),null,'open',istop,iret)
!
!      First copy entire data from fname_ges to filename, then do selective update
!
       allocate(rwork1d(lonb*latb))
       allocate(buffer2(lonb,latb))
       allocate(grid2(lonb,latb))
       allocate(grid2_nst(lonb,latb))
       allocate(tsea(lonb,latb))
       allocate(tref(lonb,latb))
       allocate(dt_cool(lonb,latb))
       allocate(xt(lonb,latb))
       allocate(xz(lonb,latb))
       allocate(slmsk(lonb,latb))

       do n = 1, nrec
          call nemsio_readrec (gfile, n,rwork1d,iret=iret)
          if ( iret /= 0 ) write(6,*) 'readrec  nrec = ', n, '  Status = ', iret
          call nemsio_writerec(gfileo,n,rwork1d,iret=iret)
          if ( iret /= 0 ) write(6,*) 'writerec nrec = ', n, '  Status = ', iret
       end do

       do n = 1, nrec_nst
          call nemsio_readrec (gfile_nst, n,rwork1d,iret=iret)
          if ( iret /= 0 ) write(6,*) 'readrec  nrec = ', n, '  Status = ', iret
          call nemsio_writerec(gfileo_nst,n,rwork1d,iret=iret)
          if ( iret /= 0 ) write(6,*) 'writerec nrec = ', n, '  Status = ', iret
       end do
!
! Only sea surface temperature will be updated in the SFC & NST files.
! Need values from ges_nst for tref update
!
       call nemsio_readrecv(gfile,'tmp','sfc',1,rwork1d,iret=iret)
       if (iret /= 0) call error_msg(mype,trim(my_name),trim(fname_ges_sfc),'tmp','read',istop,iret)
       tsea=reshape(rwork1d,(/size(tsea,1),size(tsea,2)/))

       call nemsio_readrecv(gfile_nst, 'tref',  'sfc', 1, rwork1d, iret=iret)
       if (iret /= 0) call error_msg(mype,trim(my_name),trim(fname_ges_nst),'tref','read',istop,iret)
       tref=reshape(rwork1d,(/size(tref,1),size(tref,2)/))

       call nemsio_readrecv(gfile_nst, 'dtcool','sfc', 1, rwork1d, iret=iret)
       if (iret /= 0) call error_msg(mype,trim(my_name),trim(fname_ges_nst),'dt_cool','read',istop,iret)
       dt_cool=reshape(rwork1d,(/size(dt_cool,1),size(dt_cool,2)/))

       call nemsio_readrecv(gfile_nst, 'xt',    'sfc', 1, rwork1d, iret=iret)
       if (iret /= 0) call error_msg(mype,trim(my_name),trim(fname_ges_nst),'xt','read',istop,iret)
       xt=reshape(rwork1d,(/size(xt,1),size(xt,2)/))

       call nemsio_readrecv(gfile_nst, 'xz',    'sfc', 1, rwork1d, iret=iret)
       if (iret /= 0) call error_msg(mype,trim(my_name),trim(fname_ges_nst),'xz','read',istop,iret)
       xz=reshape(rwork1d,(/size(xz,1),size(xz,2)/))

       call nemsio_readrecv(gfile_nst, 'slmsk', 'sfc', 1, rwork1d, iret=iret)
       if (iret /= 0) call error_msg(mype,trim(my_name),trim(fname_ges_nst),'slmsk','read',istop,iret)
       slmsk=reshape(rwork1d,(/size(slmsk,1),size(slmsk,2)/))

       grid2 = zero
       if ( (latb /= nlatm2) .or. (lonb /= nlon) ) then
          write(6,*)trim(my_name),':  different grid dimensions analysis', &
             ' vs sfc. interpolating sfc temperature nlon,nlat-2=',nlon,  &
             nlatm2,' -vs- sfc file lonb,latb=',lonb,latb
          call sfc_interpolate(buffer,nlon,nlat,grid2,lonb,latb)
       else
          do j=1,latb
             do i=1,lonb
                grid2(i,j)=buffer(i,j+1)
             end do
          end do
       endif

       where ( nint(slmsk) > 0 )
          buffer2 = zero
       elsewhere
          buffer2 = grid2
       end where
!
!      update tsea (in the surface file; grid2    ) When Tr analysis is on
!      update tref (in the nst     file; grid2_nst) When Tr analysis is on
!
       grid2     = zero
       grid2_nst = zero
       if ( nst_gsi > 2 ) then
          where ( nint(slmsk) == 0 )
             grid2     = max(tref+buffer2+two*xt/xz-dt_cool,271.0_r_kind)
             grid2_nst = max(tref+buffer2,271.0_r_kind)
          elsewhere
             grid2     = tsea
             grid2_nst = tsea
          end where
       else
          grid2  = max(tsea+buffer2,271.0_r_kind)
          where ( nint(slmsk) == 0 )
             grid2_nst = tsea
          elsewhere
             grid2_nst = tref
          end where
       endif
!
!      update tsea record
       rwork1d = reshape(grid2, (/size(rwork1d)/) )
       call nemsio_writerecv(gfileo,'tmp','sfc',1,rwork1d,iret=iret)
       if (iret /= 0) call error_msg(0,trim(my_name),trim(filename),'tmp','write',istop,iret)
!
!      update tref record
       rwork1d = reshape( grid2_nst,(/size(rwork1d)/) )
       call nemsio_writerecv(gfileo_nst,'tref','sfc',1,rwork1d,iret=iret)
       if (iret /= 0) call error_msg(0,trim(my_name),trim(fname_nst),'tref','write',istop,iret)

       deallocate(grid2,dt_cool,xt,xz)
       deallocate(buffer2,grid2_nst,tref,tsea,slmsk)
       deallocate(rwork1d)

       call nemsio_close(gfile, iret=iret)
       if (iret /= 0) call error_msg(0,trim(my_name),trim(fname_ges_sfc),null,'close',istop,iret)

       call nemsio_close(gfile_nst, iret=iret)
       if (iret /= 0) call error_msg(0,trim(my_name),trim(fname_ges_nst),null,'close',istop,iret)

       call nemsio_close(gfileo,iret=iret)
       if (iret /= 0) call error_msg(0,trim(my_name),trim(filename),null,'close',istop,iret)

       call nemsio_close(gfileo_nst,iret=iret)
       if (iret /= 0) call error_msg(0,trim(my_name),trim(fname_nst),null,'close',istop,iret)

       write(6,'(a,'': sfc_nst anal written for lonb,latb= '',2i6,'',valid hour= '',f4.1,'',idate= '',4i5)') &
          trim(my_name),lonb,latb,fhour,odate
    endif
  end subroutine write_sfc_nst_



  subroutine error_msg_(mype,sub_name,file_name,var_name,action,stop_code,error_code)
    use kinds, only: i_kind
    implicit none

    character(len=*), intent(in) :: sub_name,file_name,var_name,action
    integer(i_kind),  intent(in) :: mype, stop_code, error_code

    if ( mype == 0 ) then
       select case (trim(action))
       case('init')
          write(6,'(a,'':  problem with nemsio_init, Status = '', i3)') &
             trim(sub_name), error_code
       case('open')
          write(6,'(a,'':  problem opening file '',a,'', Status = '', i3)') &
             trim(sub_name), trim(file_name), error_code
       case('close')
          write(6,'(a,'':  problem closing file '',a,'', Status = '', i3)') &
             trim(sub_name), trim(file_name), error_code
       case default
          write(6,'(a,'':  ***ERROR*** '',a,tr1,a,'',variable = '',a,'',Status = '',i3)') &
             trim(sub_name),trim(action),trim(file_name),trim(var_name),error_code
       end select
     end if
     if ( stop_code /= 0 ) call stop2(stop_code)
  end subroutine error_msg_


  subroutine sfc_interpolate(a,na_lon,na_lat,b,ns_lon,ns_lat)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    sfc_interpolate --- interpolates from analysis grid to 
!                                    surface grid
!   prgrmmr:     derber -  initial version; org: np2
!
! abstract:      This routine interpolates a on analysis grid to b on 
!                surface grid
!
! program history log:
!   2008-02-26  derber  - original routine
!   2008-05-28  safford - add subprogram doc block, rm unused uses
!   2011-04-01  li - change kind of output field (b: single to r_kind)
!   2013-01-26  parrish - change from grdcrd to grdcrd1 (to allow successful debug compile on WCOSS)
!
!   input argument list:
!     na_lon  - number of longitude grid analysis 
!     na_lat  - number of latitude grid analysis
!     ns_lon  - number of longitude grid sfc 
!     ns_lat  - number of latitude grid sfc
!     a       - analysis values
!
!   output argument list:
!     b       - surface values
!
! attributes:
!   language: f90
!   machines: ibm RS/6000 SP; SGI Origin 2000; Compaq HP
!
!$$$ end documentation block

! !USES:
    use kinds, only: r_kind,i_kind,r_single
    use constants, only: zero,one
    use gridmod, only: rlats,rlons,rlats_sfc,rlons_sfc
    
    implicit none

! !INPUT PARAMETERS:
    integer(i_kind)                        ,intent(in   ) :: na_lon  ! number of longitude grid analysis 
    integer(i_kind)                        ,intent(in   ) :: na_lat  ! number of latitude grid analysis
    integer(i_kind)                        ,intent(in   ) :: ns_lon  ! number of longitude grid sfc 
    integer(i_kind)                        ,intent(in   ) :: ns_lat  ! number of latitude grid sfc

    real(r_kind), dimension(na_lon,na_lat),intent(in   ) :: a   ! analysis values

! !OUTPUT PARAMETERS:
    real(r_kind), dimension(ns_lon,ns_lat),intent(  out) :: b   ! surface values


!   Declare local variables
    integer(i_kind) i,j,ix,iy,ixp,iyp
    real(r_kind) dx1,dy1,dx,dy,w00,w01,w10,w11,bout,dlat,dlon

!*****************************************************************************

    b=zero
!   Loop over all points to get interpolated value
    do j=1,ns_lat
       dlat=rlats_sfc(j)
       call grdcrd1(dlat,rlats,na_lat,1)
       iy=int(dlat)
       iy=min(max(1,iy),na_lat)
       dy  =dlat-iy
       dy1 =one-dy
       iyp=min(na_lat,iy+1)


       do i=1,ns_lon
          dlon=rlons_sfc(i)
          call grdcrd1(dlon,rlons,na_lon,1)
          ix=int(dlon)
          dx  =dlon-ix
          dx=max(zero,min(dx,one))
          dx1 =one-dx
          w00=dx1*dy1; w10=dx1*dy; w01=dx*dy1; w11=dx*dy

          ix=min(max(0,ix),na_lon)
          ixp=ix+1
          if(ix==0) ix=na_lon
          if(ixp==na_lon+1) ixp=1
          bout=w00*a(ix,iy)+w01*a(ix,iyp)+w10*a(ixp,iy)+w11*a(ixp,iyp)
          b(i,j)=bout

       end do
    end do

    
!   End of routine
    return
  end subroutine sfc_interpolate
end module ncepnems_io

