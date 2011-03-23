module ncepnems_io
!$$$ module documentation block
!           .      .    .                                       .
! module:   ncepnems_io
!   prgmmr: hcHuang     org: np23                date: 2010-02-22
!
! abstract: This module contains routines which handle input/output
!           operations for NCEP NEMS global atmospheric and surface files.
!
! program history log:
!   2010-02-22 hcHuang  Initial version.  Based on ncepgfs_io
!   2010-10-18 hcHuang  Remove subroutine reorder_gfsgrib for no longer been called in GSI
!                       For Now, subroutine sfc_interpolate is kept in ncepgfs_io.f90.
!                       When sigio and gfsio are both retired, i.e., remove ncepgfs_io.f90.
!                       move this routines back to this module
!   2011-03-03 hcHuang  Changes has been made to adopt to high resolution GSI run (T382 & T574)
!                       both for CPU and memory issues.
!                       Future development of nemsio need to consider a mapping routine be
!                       inserted between read-in forecast field and GSI first guess field,
!                       as well as GSI analysis field and write-out data field for forecast
!                       model. Due to computation resource, GSI may not be able to run at
!                       the same resolution as that of forecast model, e.g., run GSI at T382
!                       w/ T574 forecast model output.
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
  public write_nems
  public write_nemsatm
  public write_nemssfc

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

  interface write_nems
     module procedure write_
  end interface

  interface write_nemsatm
     module procedure write_atm_
  end interface

  interface write_nemssfc
     module procedure write_sfc_
  end interface


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
!   2010-03-31  hcHuang - create routine based on read_gfs
!   2010-10-19  hcHuang - remove spectral part for gridded NEMS/GFS
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

    use kinds, only: i_kind
    use gridmod, only: sp_a
    use guess_grids, only: ges_z,ges_ps,ges_vor,ges_div,&
         ges_u,ges_v,ges_tv,ges_q,ges_cwmr,ges_oz,&
         ifilesig,nfldsig
    implicit none

    integer(i_kind),intent(in   ) :: mype

    character(24) filename
    integer(i_kind):: it,iret

    do it=1,nfldsig
       write(filename,100) ifilesig(it)
       call read_atm_ (filename,mype,sp_a,&
            ges_z(1,1,it),ges_ps(1,1,it),&
            ges_vor(1,1,1,it),ges_div(1,1,1,it),&
            ges_u(1,1,1,it),ges_v(1,1,1,it),&
            ges_tv(1,1,1,it),ges_q(1,1,1,it),&
            ges_cwmr(1,1,1,it),ges_oz(1,1,1,it), iret)
    end do
100 format('sigf',i2.2)
  end subroutine read_

  subroutine read_chem_ ( iyear, month )
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    read_nems_chem
!
!   prgrmmr: todling
!
! abstract: fills chem_bundle with GFS chemistry. 
!
! remarks:
!    1. Right now, only CO2 is done and even this is treated
!        as constant througout the assimialation window.
!    2. iyear and month could come from obsmod, but logically
!       this program should never depend on obsmod
! 
!
! program history log:
!   2010-12-23  hcHuang - initial code, based on read_gfs_chem
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
    use guess_grids, only: ges_prsi,nfldsig,ntguessig
    use gsi_bundlemod, only: gsi_bundlegetpointer
    use gsi_chemtracer_mod, only: gsi_chem_bundle
    use gsi_chemtracer_mod, only: gsi_chemtracer_get

    implicit none

!   Declared argument list
    integer(i_kind), intent(in):: iyear
    integer(i_kind), intent(in):: month

!   Declare local variables
    integer(i_kind) :: igfsco2, i, j, k, n, ico2, iret
    real(r_kind),dimension(lat2):: xlats

    if(.not.associated(gsi_chem_bundle)) return
    call gsi_bundlegetpointer(gsi_chem_bundle(1),'co2',ico2,iret)
    if(iret /= 0) return

!   Get subdomain latitude array
    j = mype + 1
    do i = 1, lat2
       n = min(max(1, istart(j)+i-2), nlat)
       xlats(i) = rlats(n)
    enddo

!   Read in CO2
    call gsi_chemtracer_get ( 'i4crtm::co2', igfsco2, iret )
    call read_gfsco2 ( iyear,month,igfsco2,xlats,ges_prsi(:,:,:,ntguessig),&
                       lat2,lon2,nsig,mype, gsi_chem_bundle(1)%r3(ico2)%q )

    do n = 2, ntguessig
       gsi_chem_bundle(n)%r3(ico2)%q = gsi_chem_bundle(1)%r3(ico2)%q
    enddo

  end subroutine read_chem_

  subroutine read_atm_ (filename,mype,sp,g_z,g_ps,g_vor,g_div,g_u,g_v,&
       g_tv,g_q,g_cwmr,g_oz,iret_read)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    read_nemsatm    read nems atm and send to all mpi tasks
!   prgmmr: hcHuang          org: np23                date: 2010-02-22
!
! abstract: read ncep nems/gfs atmospheric guess field and 
!           scatter to subdomains
!
! program history log:
!   2010-02-22 hcHuang  Initial version.  Based on sub read_gfsatm
!   2011-02-28 hcHuang  Re-arrange the read sequence to be same as model
!                       write sequence.  Alsom allocate and deallocate
!                       temporary working array immediatelt before and after
!                       the processing and scattering first guess field to reduce
!                       maximum resident memory size.  Page fault can happen
!                       when running at high resolution GSI, e.g., T574.
!   input argument list:
!     mype     - mpi task id
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
    use gridmod, only: displs_s,irc_s,ijn_s,&
         ird_s,nsig,nlat,nlon,lat2,lon2,&
         itotsub,fill_ns,filluv_ns,ncepgfs_head,&
         ntracer,reload
    use general_specmod, only: spec_vars
    use mpimod, only: npe,mpi_comm_world,ierror,mpi_rtype
    use nemsio_module, only: nemsio_init,nemsio_open,nemsio_close
    use nemsio_module, only: nemsio_gfile,nemsio_getfilehead,nemsio_readrecv

    implicit none
    
!   Declare local parameters
    real(r_kind),parameter:: r0_001 = 0.001_r_kind

!   Declare passed variables
    character(LEN=24)                     ,intent(in   ) :: filename
    integer(i_kind)                       ,intent(in   ) :: mype
    integer(i_kind)                       ,intent(  out) :: iret_read
    real(r_kind),dimension(lat2,lon2)     ,intent(  out) :: g_z,g_ps
    real(r_kind),dimension(lat2,lon2,nsig),intent(  out) :: g_u,g_v,&
         g_vor,g_div,g_cwmr,g_q,g_oz,g_tv
    type(spec_vars)                       ,intent(in   ) :: sp
    
!   Declare local variables
    integer(i_kind),dimension(7):: idate
    integer(i_kind) :: iret,nlatm2,ij,n,l,m
    integer(i_kind) :: i,j,k,icount,icount_prev,mm1
    integer(i_kind) :: mype_hs, mype_ps, nframe
    integer(i_kind) :: nfhour, nfminute, nfsecondn, nfsecondd
    real(r_kind), dimension(nlon,nlat-2):: grid, grid_u, grid_v, &
         grid_vor, grid_div
    real(r_kind),dimension(itotsub):: work,work_vor,work_div,&
         work_u,work_v
    real(r_kind),dimension(lat2*lon2,max(2*nsig,npe)):: sub,sub_div,sub_vor,&
         sub_u,sub_v
    real(r_kind),dimension(sp%nc):: spec_vor,spec_div
    real(r_kind),allocatable,dimension(:) :: rwork1d
!
    type:: nemsio_data
       real(r_kind),allocatable:: hs(:)     ! surface height, m
       real(r_kind),allocatable:: ps(:)     ! surface pressure, pa
       real(r_kind),allocatable:: t(:,:)    ! layer temperature, k
       real(r_kind),allocatable:: u(:,:)    ! layer zonal wind, m/s
       real(r_kind),allocatable:: v(:,:)    ! layer meridional wind, m/s
       real(r_kind),allocatable:: q(:,:)    ! tracers, layer specifi humidity
       real(r_kind),allocatable:: oz(:,:)   ! tracers, layer ozone, kg/kg
       real(r_kind),allocatable:: cw(:,:)   ! tracers, layer cloud liquid water, kg/kg
    end type nemsio_data

    type(nemsio_data)  :: gfsdata
    type(nemsio_gfile) :: gfile
    type(ncepgfs_head) :: gfshead
!
!******************************************************************************  
!   Initialize variables used below
    mm1=mype+1
    mype_hs=0
    mype_ps=npe-1
    iret_read=0
    nlatm2=nlat-2

    call nemsio_init(iret=iret)
    if( iret /= 0 ) then
       write(6,*)'READ_NEMSATM:  problem with nemsio_init, Status = ',iret
       call stop2(101)
    end if
    call nemsio_open(gfile,filename,'READ',iret=iret)
    if( iret /= 0 ) then
       write(6,*)'READ_NEMSATM:  problem opening file ',trim(filename),', Status = ',iret
       call stop2(101)
    end if
    call nemsio_getfilehead(gfile,iret=iret, nframe=nframe, &
         nfhour=nfhour, nfminute=nfminute, nfsecondn=nfsecondn, nfsecondd=nfsecondd, &
         idate=idate, ntrac=gfshead%ntrac, ncldt=gfshead%ncldt, &
         dimx=gfshead%lonb, dimy=gfshead%latb,dimz=gfshead%levs)

    if( nframe /= 0 ) then
       write(6,*)'READ_NEMSATM: ***ERROR***  nframe /= 0 for global model read, nframe = ', nframe
       call stop2(101)
    end if

    gfshead%fhour = float(nfhour) + float(nfminute)/r60 + float(nfsecondn)/float(nfsecondd)/r3600
    gfshead%idate(1) = idate(4)  !hour
    gfshead%idate(2) = idate(2)  !month
    gfshead%idate(3) = idate(3)  !day
    gfshead%idate(4) = idate(1)  !year
!
!! NEMS output in latitudial direction does not include pole, i.e.,
!!         gfshead%latb = nlatm2
!  g_* array already pre-allocate as (lat2,lon2,<nsig>) => 2D and <3D> array
!
    if(gfshead%latb /= nlatm2) then
       write(6,*)'READ_NEMSATM: problem in data dimension nlatm2 = ',nlatm2,' gfshead%latb = ',gfshead%latb
       call stop2(101)
    end if
    if(gfshead%lonb /= nlon) then
       write(6,*)'READ_NEMSATM: problem in data dimension nlon  = ',nlon,' gfshead%lonb = ',gfshead%lonb
       call stop2(101)
    end if
    if(gfshead%levs /= nsig)then
       write(6,*)'READ_NEMSATM: problem in data dimension nsig = ',nsig,' gfshead%levs = ',gfshead%levs
       call stop2(101)
    end if
    allocate(rwork1d(gfshead%latb*gfshead%lonb))
!
!   Load values into rows for south and north pole before scattering
!
!   Terrain:  scatter to all mpi tasks
!
    allocate(gfsdata%hs(gfshead%latb*gfshead%lonb))
    call nemsio_readrecv(gfile,'hgt', 'sfc',1,gfsdata%hs,iret=iret)
    iret_read=iret_read+iret
!
    if (mype==mype_hs) then
       grid=reshape(gfsdata%hs,(/size(grid,1),size(grid,2)/))
       call fill_ns(grid,work)
    endif
    call mpi_scatterv(work,ijn_s,displs_s,mpi_rtype,&
       g_z,ijn_s(mm1),mpi_rtype,mype_hs,mpi_comm_world,ierror)
    deallocate(gfsdata%hs)

!   Surface pressure:  same procedure as terrain, but handled by task mype_ps
!
    allocate(gfsdata%ps(gfshead%latb*gfshead%lonb))
    call nemsio_readrecv(gfile,'pres','sfc',1,gfsdata%ps(:),iret=iret)
    iret_read=iret_read+iret
!   
    if (mype==mype_ps) then
       rwork1d = r0_001*gfsdata%ps
       grid=reshape(rwork1d,(/size(grid,1),size(grid,2)/)) ! convert Pa to cb
       call fill_ns(grid,work)
    endif
    call mpi_scatterv(work,ijn_s,displs_s,mpi_rtype,&
       g_ps,ijn_s(mm1),mpi_rtype,mype_ps,mpi_comm_world,ierror)
    deallocate(gfsdata%ps)

!   Divergence and voriticity.  Compute u and v from div and vor
    allocate(gfsdata%u(gfshead%latb*gfshead%lonb,gfshead%levs))
    allocate(gfsdata%v(gfshead%latb*gfshead%lonb,gfshead%levs))
    do k=1,gfshead%levs
       call nemsio_readrecv(gfile,'ugrd','mid layer',k,gfsdata%u(:,k),iret=iret)
       iret_read=iret_read+iret
    end do

    do k=1,gfshead%levs
       call nemsio_readrecv(gfile,'vgrd','mid layer',k,gfsdata%v(:,k),iret=iret)
       iret_read=iret_read+iret
    end do

    sub_vor=zero
    sub_div=zero
    sub_u=zero
    sub_v=zero
    icount=0
    icount_prev=1
    do k=1,gfshead%levs
       icount=icount+1

!      The work in the loop below is spread over all mpi tasks
       if (mype==mod(icount-1,npe)) then

!         Convert grid u,v to div and vor
          grid_u=reshape(gfsdata%u(:,k),(/size(grid_u,1),size(grid_u,2)/))
          grid_v=reshape(gfsdata%v(:,k),(/size(grid_v,1),size(grid_v,2)/))

          call general_sptez_v(sp,spec_div,spec_vor,grid_u,grid_v,-1)
          call general_sptez_s(sp,spec_div,grid_div,1)
          call general_sptez_s(sp,spec_vor,grid_vor,1)

!         Load values into rows for south and north pole
          call fill_ns(grid_div,work_div)
          call fill_ns(grid_vor,work_vor)
          call filluv_ns(grid_u,grid_v,work_u,work_v)

       endif

!      Periodically exchange vor,div,u,v between all mpi tasks.
       if (mod(icount,npe)==0 .or. icount==gfshead%levs) then
          call mpi_alltoallv(work_vor,ijn_s,displs_s,mpi_rtype,&
             sub_vor(1,icount_prev),irc_s,ird_s,mpi_rtype,&
             mpi_comm_world,ierror)
          call mpi_alltoallv(work_div,ijn_s,displs_s,mpi_rtype,&
             sub_div(1,icount_prev),irc_s,ird_s,mpi_rtype,&
             mpi_comm_world,ierror)
          call mpi_alltoallv(work_u,ijn_s,displs_s,mpi_rtype,&
             sub_u(1,icount_prev),irc_s,ird_s,mpi_rtype,&
             mpi_comm_world,ierror)
          call mpi_alltoallv(work_v,ijn_s,displs_s,mpi_rtype,&
             sub_v(1,icount_prev),irc_s,ird_s,mpi_rtype,&
             mpi_comm_world,ierror)
          icount_prev=icount+1
       endif
    end do

!   Transfer vor,div,u,v into real(r_kind) guess arrays
    call reload(sub_vor,g_vor)
    call reload(sub_div,g_div)
    call reload(sub_u,g_u)
    call reload(sub_v,g_v)
    deallocate(gfsdata%u,gfsdata%v)

!   Thermodynamic variable and Specific humidity:  communicate to all tasks
!
!   For multilevel fields, each task handles a given level.  Periodic
!   mpi_alltoallv calls communicate the grids to all mpi tasks.  
!   Finally, the grids are loaded into guess arrays used later in the 
!   code.

    allocate(gfsdata%t(gfshead%latb*gfshead%lonb,gfshead%levs))
    allocate(gfsdata%q(gfshead%latb*gfshead%lonb,gfshead%levs))

    do k=1,gfshead%levs
       call nemsio_readrecv(gfile,'tmp','mid layer',k,gfsdata%t(:,k),iret=iret)
       iret_read=iret_read+iret
    end do

    do k=1,gfshead%levs
       call nemsio_readrecv(gfile,'spfh','mid layer',k,gfsdata%q(:,k),iret=iret)
       iret_read=iret_read+iret
    end do

    sub=zero
    icount=0
    icount_prev=1
    do k=1,gfshead%levs
       icount=icount+1
       if (mype==mod(icount-1,npe)) then
          rwork1d = gfsdata%t(:,k)*(one+fv*gfsdata%q(:,k))
          grid=reshape(rwork1d,(/size(grid,1),size(grid,2)/))
          call fill_ns(grid,work)
       endif

       if (mod(icount,npe)==0 .or. icount==gfshead%levs) then
          call mpi_alltoallv(work,ijn_s,displs_s,mpi_rtype,&
             sub(1,icount_prev),irc_s,ird_s,mpi_rtype,&
             mpi_comm_world,ierror)
          icount_prev=icount+1
       endif
    end do
    call reload(sub,g_tv)
    deallocate(gfsdata%t)

    sub=zero
    icount=0
    icount_prev=1
    do k=1,gfshead%levs
       icount=icount+1
       if (mype==mod(icount-1,npe)) then
          grid=reshape(gfsdata%q(:,k),(/size(grid,1),size(grid,2)/))
          call fill_ns(grid,work)
       endif
       if (mod(icount,npe)==0 .or. icount==gfshead%levs) then
          call mpi_alltoallv(work,ijn_s,displs_s,mpi_rtype,&
             sub(1,icount_prev),irc_s,ird_s,mpi_rtype,&
             mpi_comm_world,ierror)
          icount_prev=icount+1
       endif
    end do
    call reload(sub,g_q)
    deallocate(gfsdata%q)

!   Ozone mixing ratio
    allocate(gfsdata%oz(gfshead%latb*gfshead%lonb,gfshead%levs))
    do k=1,gfshead%levs
       call nemsio_readrecv(gfile,'o3mr','mid layer',k,gfsdata%oz(:,k),iret=iret)
       iret_read=iret_read+iret
    end do

    sub=zero
    icount=0
    icount_prev=1
    do k=1,gfshead%levs
       icount=icount+1
       if (mype==mod(icount-1,npe)) then
          grid=reshape(gfsdata%oz(:,k),(/size(grid,1),size(grid,2)/))
          call fill_ns(grid,work)
       endif
       if (mod(icount,npe)==0 .or. icount==gfshead%levs) then
          call mpi_alltoallv(work,ijn_s,displs_s,mpi_rtype,&
             sub(1,icount_prev),irc_s,ird_s,mpi_rtype,&
             mpi_comm_world,ierror)
          icount_prev=icount+1
       endif
    end do
    call reload(sub,g_oz)
    deallocate(gfsdata%oz)
    
!   Cloud condensate mixing ratio.
    allocate(gfsdata%cw(gfshead%latb*gfshead%lonb,gfshead%levs))
    do k=1,gfshead%levs
       call nemsio_readrecv(gfile,'clwmr','mid layer',k,gfsdata%cw(:,k),iret=iret)
       iret_read=iret_read+iret
    end do

    if (gfshead%ntrac>2 .or. gfshead%ncldt>=1) then
       sub=zero
       icount=0
       icount_prev=1
       do k=1,gfshead%levs
          icount=icount+1
          if (mype==mod(icount-1,npe)) then
             grid=reshape(gfsdata%cw(:,k),(/size(grid,1),size(grid,2)/))
             call fill_ns(grid,work)
          endif
          if (mod(icount,npe)==0 .or. icount==gfshead%levs) then
             call mpi_alltoallv(work,ijn_s,displs_s,mpi_rtype,&
                sub(1,icount_prev),irc_s,ird_s,mpi_rtype,&
                mpi_comm_world,ierror)
             icount_prev=icount+1
          endif
       end do
       call reload(sub,g_cwmr)
    else
       g_cwmr = zero
    endif
    deallocate(gfsdata%cw)

    call nemsio_close(gfile,iret=iret)
    if( iret /= 0 ) then
       write(6,*)'READ_NEMSATM:  problem closing file ',trim(filename),', Status = ',iret
       call stop2(101)
    end if

! Deallocate local array
!
    deallocate(rwork1d)

    if (iret_read /= 0) goto 1000

!   Print date/time stamp 
    if(mype==0) then
       write(6,700) gfshead%lonb,gfshead%latb,gfshead%levs,&
          gfshead%fhour,gfshead%idate
700    format('READ_NEMSATM:  ges read/scatter, lonb,latb,levs=',&
            3i6,', hour=',f10.1,', idate=',4i5)
    end if

    return

!   ERROR detected while reading file
1000 continue
    if (mype==0) write(6,*)'READ_NEMSATM:  ***ERROR*** while reading ',&
         trim(filename),'.   Status =',iret
    
!   End of routine.  Return
    return
  end subroutine read_atm_



  subroutine read_sfc_ (filename,mype,fact10,sfct,sno,veg_type,&
       veg_frac,soil_type,soil_temp,soil_moi,isli,sfc_rough,terrain)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    read_nemssfc     read nems surface file
!   prgmmr: hcHuang          org: np23                date: 2010-02-22
!
! abstract: read nems surface file
!
! program history log:
!   2010-02-22  hcHuang  Initial version.  Based on read_gfssfc
!   2011-02-14  hcHuang  Re-arrange the read sequence to be same as model
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
    use sfcio_module, only: sfcio_head
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


!   Declare local variables
    integer(i_kind) :: i,j,k,mm1, ij
    integer(i_kind) :: iret, iret_read, nframe
    integer(i_kind) :: nfhour, nfminute, nfsecondn, nfsecondd, nsoil
    real(r_kind)    :: sumn, sums
    real(r_kind), allocatable, dimension(:,:)   :: rwork2d
    real(r_kind), allocatable, dimension(:,:,:) :: work, sfcges

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!  Define read variable property   !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
    type(nemsio_gfile) :: gfile
    type(sfcio_head):: sfc_head

!-----------------------------------------------------------------------------
    iret_read = 0
    mm1=mype+1

    call nemsio_init(iret=iret)
    if ( iret/=0 ) then
       write(6,*)'READ_NEMSSFC: problem nemsio_init, Status = ',iret
       call stop2(102)
    end if
    call nemsio_open(gfile,filename,'READ',iret=iret)
    if ( iret/=0 ) then
       write(6,*)'READ_NEMSSFC: problem opening file ',trim(filename),' Status = ',iret
       call stop2(102)
    end if
    call nemsio_getfilehead(gfile, idate=idate, iret=iret, nframe=nframe,   &
       nfhour=nfhour, nfminute=nfminute, nfsecondn=nfsecondn, nfsecondd=nfsecondd, &
       dimx=sfc_head%lonb, dimy=sfc_head%latb, nsoil=sfc_head%lsoil )

    if( nframe /= 0 ) then
       write(6,*)'READ_NEMSSFC: ***ERROR***  nframe /= 0 for global model read, nframe = ', nframe
       call stop2(102)
    end if

    sfc_head%fhour = float(nfhour) + float(nfminute)/r60 + float(nfsecondn)/float(nfsecondd)/r3600
    sfc_head%idate(1) = idate(4)  !hour
    sfc_head%idate(2) = idate(2)  !month
    sfc_head%idate(3) = idate(3)  !day
    sfc_head%idate(4) = idate(1)  !year

    if ( (sfc_head%latb /= nlat_sfc-2) .or. (sfc_head%lonb /= nlon_sfc) ) then
       write(6,*)'READ_NEMSSFC:  ***ERROR*** inconsistent grid dimensions.  ', &
          ', nlon,nlatm2=',nlon_sfc,nlat_sfc-2,' -vs- sfc file lonb,latb=',    &
          sfc_head%lonb,sfc_head%latb
       call stop2(102)
    endif
!
!   Load surface fields into local work array
!   Follow NEMS/GFS sfcf read order
!
    allocate(work(sfc_head%lonb,sfc_head%latb,nsfc),sfcges(sfc_head%latb+2,sfc_head%lonb,nsfc))
    allocate(rwork2d(size(work,1)*size(work,2),size(work,3)))
    work    = zero
    rwork2d = zero

!   Tsea
    call nemsio_readrecv(gfile, 'tmp', 'sfc', 1, rwork2d(:,1), iret=iret)
    iret_read = iret_read + iret

!   sheleg
    call nemsio_readrecv(gfile, 'weasd','sfc', 1, rwork2d(:,2), iret=iret)
    iret_read = iret_read + iret

!   zorl
    call nemsio_readrecv(gfile, 'sfcr', 'sfc', 1, rwork2d(:,3),iret=iret)
    iret_read = iret_read + iret

!   slmsk
    call nemsio_readrecv(gfile, 'land', 'sfc', 1, rwork2d(:,4), iret=iret)
    iret_read = iret_read + iret

!   vfrac
    call nemsio_readrecv(gfile, 'veg',  'sfc', 1, rwork2d(:,5), iret=iret)
    iret_read = iret_read + iret

!   f10m
    call nemsio_readrecv(gfile, 'f10m', '10 m above gnd', 1, rwork2d(:,6), iret=iret)
    iret_read = iret_read + iret

!   vtype
    call nemsio_readrecv(gfile, 'vtype','sfc', 1, rwork2d(:,7), iret=iret)
    iret_read = iret_read + iret

!   stype
    call nemsio_readrecv(gfile, 'sotyp','sfc', 1, rwork2d(:,8), iret=iret)
    iret_read = iret_read + iret

!   orog
    call nemsio_readrecv(gfile, 'orog', 'sfc', 1, rwork2d(:,9),iret=iret)
    iret_read = iret_read + iret

!   smc
    call nemsio_readrecv(gfile, 'smc', 'soil layer', 1, rwork2d(:,10), iret=iret)
    iret_read = iret_read + iret

!   stc
    call nemsio_readrecv(gfile, 'stc',  'soil layer', 1, rwork2d(:,11), iret=iret)
    iret_read = iret_read + iret

    if (iret_read /= 0) goto 1000
 
!   Fill surface guess array
    do k=1,nsfc
       work(:,:,k)=reshape(rwork2d(:,k),(/size(work,1),size(work,2)/))

!      Compute mean for southern- and northern-most rows
!      of surface guess array
       sumn = zero
       sums = zero
       do i=1, sfc_head%lonb
          sumn = work(i,1,k)    + sumn
          sums = work(i,sfc_head%latb,k) + sums
       end do
       sumn = sumn/float(sfc_head%lonb)
       sums = sums/float(sfc_head%lonb)

!      Transfer from local work array to surface guess array
       do j = 1, sfc_head%lonb
          sfcges(1,j,k)=sums
          sfcges(sfc_head%latb+2,j,k)=sumn
          do i=2,sfc_head%latb+1
             sfcges(i,j,k) = work(j,sfc_head%latb+2-i,k)
          end do
       end do

!   End of loop over data records
    end do

!   Deallocate local work arrays
    deallocate(work,rwork2d)

!   Load data into output arrays

    do j=1, sfc_head%lonb
       do i=1,sfc_head%latb+2
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
    if ( iret /= 0 ) then
       write(6,*)'READ_NEMSSFC:  problem closing file ',trim(filename),', Status = ',iret
       call stop2(102)
    end if
!
!   Print date/time stamp
    if (mype==0) then
       write(6,700) sfc_head%latb,sfc_head%lonb,sfc_head%fhour,sfc_head%idate
700    format('READ_NEMSSFC:  ges read/scatter, nlat,nlon=',&
            2i6,', hour=',f10.1,', idate=',4i5)
    end if
    return
!   ERROR detected while reading file
1000 continue
    if (mype==0) write(6,*)'READ_NEMSSFC:  ***ERROR*** while reading ',&
       trim(filename),'.   Statue = ',iret_read

!   End of routine.  Return
  end subroutine read_sfc_


  subroutine write_ (increment,mype,mype_atm,mype_sfc)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    write_nems
!
!   prgmmr: hcHuang          org: np23                date: 2010-02-22
!   prgrmmr:
!
! abstract:
!
! program history log:
!   2010-02-22  hcHuang  Initial version.  Based on write_gfs
!
!   input argument list:
!     increment          - when .t. will name files as increment files
!     mype               - mpi task id
!     mype_atm,mype_sfc  -
!
!   output argument list:
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block

    use kinds, only: i_kind
    use guess_grids, only: ges_z,ges_ps,ges_vor,ges_div,&
         ges_tv,ges_q,ges_oz,ges_cwmr,ges_prsl,&
         ges_u,ges_v,ges_prsi,dsfct 
    use guess_grids, only: ntguessig,ntguessfc

    implicit none

    logical        ,intent(in   ) :: increment
    integer(i_kind),intent(in   ) :: mype,mype_atm,mype_sfc
    character(len=24)             :: filename
    integer(i_kind)               :: iret

!   Write atmospheric analysis file
    if (increment) then
       filename='nemsinc'
       if(mype==0) write(6,'(a)') 'WRITE_NEMS: sorry, I do not know how to write increments yet'
       return
    else
       filename='siganl'
    endif
    call write_atm_ (filename,mype,mype_atm,&
         ges_z(1,1,ntguessig),ges_ps(1,1,ntguessig),&
         ges_vor(1,1,1,ntguessig),ges_div(1,1,1,ntguessig),&
         ges_tv(1,1,1,ntguessig),ges_q(1,1,1,ntguessig),&
         ges_oz(1,1,1,ntguessig),ges_cwmr(1,1,1,ntguessig),&
         ges_prsl(1,1,1,ntguessig),ges_u(1,1,1,ntguessig),&
         ges_v(1,1,1,ntguessig),ges_prsi(1,1,1,ntguessig),iret)

!   Write surface analysis file
    if (increment) then
       filename='sfcinc.gsi'
    else
       filename='sfcanl.gsi'
    endif
    call write_sfc_ (filename,mype,mype_sfc,dsfct(1,1,ntguessfc))
  end subroutine write_


  subroutine write_atm_ (filename,mype,mype_out,sub_z,sub_ps,&
       sub_vor,sub_div,sub_tv,sub_q,sub_oz,sub_cwmr,sub_prsl,&
       sub_u,sub_v,sub_prsi,iret_wrt)

!$$$  subprogram documentation block
!                .      .    .
! subprogram:    write_nemsatm --- Gather, transform, and write out 
!      
!   prgmmr: hcHuang          org: np23                date: 2010-02-22
!
! abstract: This routine gathers fields needed for the GSI analysis
!           file from subdomains and then transforms the fields from
!           analysis grid to model guess grid, then written to an 
!           atmospheric analysis file.
!
! program history log:
!   2010-02-22  hcHuang  Initial version.  Based on write_gfsatm
!   2011-02-14  hcHuang  Re-arrange the write sequence to be same as model
!                        read/rite sequence.
!
!   input argument list:
!     filename  - file to open and write to
!     mype      - mpi task number
!     mype_out  - mpi task to write output file
!     sub_z     - NEMS terrain field on subdomains
!     sub_ps    - surface pressure on subdomains
!     sub_vor   - vorticity on subdomains
!     sub_div   - divergence on subdomains
!     sub_tv    - virtual temperature on subdomains
!     sub_q     - specific humidity on subdomains
!     sub_oz    - ozone on subdomains
!     sub_cwmr  - cloud condensate mixing ratio on subdomains
!     sub_prsl  - layer midpoint pressure
!     sub_u     - zonal wind
!     sub_v     - meridional wind
!     sub_prsi  - interface  pressure
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
    
    use constants, only: zero_single,r1000,fv,one,zero
  
    use mpimod, only: mpi_rtype,mpi_rtype4
    use mpimod, only: mpi_comm_world
!! does it used
    use mpimod, only: mpi_status_size
    use mpimod, only: ierror
    use mpimod, only: npe
    
    use guess_grids, only: ntguessig, ifilesig
    
    use gridmod, only: nlat, nlon     ! no. lat/lon on analysis grid
    use gridmod, only: lat1, lon1     ! no. lat/lon on subdomain (no buffer)
    use gridmod, only: lat2, lon2     ! no. lat/lon on subdomain (buffer pnts on ends)
    use gridmod, only: nsig           ! no. levels
    use gridmod, only: iglobal        ! no. of horizontal points on global grid
    use gridmod, only: ijn            ! no. of horiz. pnts for each subdomain (no buffer)
    use gridmod, only: displs_g       ! comm. array, displacement for receive on global grid
    use gridmod, only: itotsub        ! no. of horizontal points of all subdomains combined
    use gridmod, only: load_grid
    use gridmod, only: ntracer
    use gridmod, only: ncloud
    use gridmod, only: ncepgfs_head
    use gridmod, only: strip
    
    use obsmod, only: iadate
    
    use nemsio_module, only: nemsio_gfile,nemsio_open,nemsio_init,&
         nemsio_getfilehead,nemsio_close,nemsio_writerecv
  
    implicit none

! !INPUT PARAMETERS:

    character(LEN=24)                          ,intent(in   ) :: filename     ! file to open and write to

    integer(i_kind)                            ,intent(in   ) :: mype      ! mpi task number
    integer(i_kind)                            ,intent(in   ) :: mype_out  ! mpi task to write output file
    
    real(r_kind),dimension(lat2,lon2)          ,intent(in   ) :: sub_z    ! NEMS terrain field on subdomains
    real(r_kind),dimension(lat2,lon2)          ,intent(in   ) :: sub_ps   ! surface pressure on subdomains
    real(r_kind),dimension(lat2,lon2,nsig)     ,intent(in   ) :: sub_vor  ! vorticity on subdomains
    real(r_kind),dimension(lat2,lon2,nsig)     ,intent(in   ) :: sub_div  ! divergence on subdomains
    real(r_kind),dimension(lat2,lon2,nsig)     ,intent(in   ) :: sub_tv   ! virtual temperature on subdomains
    real(r_kind),dimension(lat2,lon2,nsig)     ,intent(in   ) :: sub_q    ! specific humidity on subdomains
    real(r_kind),dimension(lat2,lon2,nsig)     ,intent(in   ) :: sub_oz   ! ozone on subdomains
    real(r_kind),dimension(lat2,lon2,nsig)     ,intent(in   ) :: sub_cwmr ! cloud condensate mixing ratio on subdomains
    real(r_kind),dimension(lat2,lon2,nsig)     ,intent(in   ) :: sub_prsl ! layer midpoint pressure
    real(r_kind),dimension(lat2,lon2,nsig)     ,intent(in   ) :: sub_u    ! zonal wind
    real(r_kind),dimension(lat2,lon2,nsig)     ,intent(in   ) :: sub_v    ! meridional wind
    real(r_kind),dimension(lat2,lon2,nsig+1)   ,intent(in   ) :: sub_prsi ! interface  pressure
    integer(i_kind)                            ,intent(  out) :: iret_wrt ! status flag

!-------------------------------------------------------------------------

    character(5):: string
    character(6):: fname_ges
    integer(i_kind),dimension(7):: idate, jdate
    integer(i_kind) :: i, j, ij, k, n, mm1, nlatm2
    integer(i_kind) :: iret
    integer(i_kind) :: nfhour, nfminute, nfsecondn, nfsecondd
    
    real(r_kind),dimension(lat1*lon1)     :: hsm, psm
    real(r_kind),dimension(lat2,lon2,nsig):: sub_dp
    real(r_kind),dimension(lat1*lon1,nsig):: tvsm,prslm, usm, vsm
    real(r_kind),dimension(lat1*lon1,nsig):: dpsm, qsm, ozsm, cwsm
    real(r_kind),dimension(max(iglobal,itotsub))     :: work1
    real(r_kind),dimension(nlon,nlat-2):: grid, grid2
    real(r_kind),allocatable,dimension(:) :: rwork1d

    type(nemsio_gfile) :: gfile,gfileo
    type(ncepgfs_head) :: gfshead
!*************************************************************************
!   Initialize local variables
    iret_wrt = 0
    mm1=mype+1
    nlatm2=nlat-2

    do k=1,nsig
       sub_dp(:,:,k) = sub_prsi(:,:,k)-sub_prsi(:,:,k+1)
    end do

!   Strip off boundary points from subdomains
    call strip(sub_z   ,hsm   ,1)
    call strip(sub_ps  ,psm   ,1)
    call strip(sub_tv  ,tvsm  ,nsig)
    call strip(sub_q   ,qsm   ,nsig)
    call strip(sub_oz  ,ozsm  ,nsig)
    call strip(sub_cwmr,cwsm  ,nsig)
    call strip(sub_dp  ,dpsm  ,nsig)
    call strip(sub_prsl,prslm ,nsig)
    call strip(sub_u   ,usm   ,nsig)
    call strip(sub_v   ,vsm   ,nsig)

!   Single task writes analysis data to analysis file
    if (mype==mype_out) then
       write(fname_ges,'(''sigf'',i2.2)') ifilesig(ntguessig)
!
!      Read header information from first guess file.
       call nemsio_init(iret)
       if ( iret/=0 ) then
          write(6,*)'WRITE_NEMSATM: problem with nemsio_init, Status = ',iret
          call stop2(103)
       end if
       call nemsio_open(gfile,trim(fname_ges),'read',iret)
       if ( iret/=0 ) then
          write(6,*)'WRITE_NEMSATM:  problem opening file ',trim(fname_ges),', Status = ',iret
          call stop2(103)
       end if
       call nemsio_getfilehead(gfile, iret=iret, nfhour=nfhour,        &
          nfminute=nfminute, nfsecondn=nfsecondn, nfsecondd=nfsecondd, &
          idate=idate, ntrac=gfshead%ntrac, ncldt=gfshead%ncldt,       &
          dimx=gfshead%lonb, dimy=gfshead%latb, dimz=gfshead%levs )
       if ( iret/=0 ) then
          write(6,*)'WRITE_NEMSATM: problem with nemsio_getfilehead, Status = ',iret
          call stop2(103)
       end if
       if(gfshead%levs/=nsig) then
          write(6,*)'WRITE_NEMSATM: problem in data dimension background levs = ',gfshead%levs,' nsig = ',nsig
          call stop2(103)
       end if

!      copy input header info to output header info
       gfileo=gfile
 
!      Update header information (with iadate) and write it to analysis file (w/ _open statement).
       jdate(1) = iadate(1)  ! analysis year
       jdate(2) = iadate(2)  ! analysis month
       jdate(3) = iadate(3)  ! analysis day
       jdate(4) = iadate(4)  ! analysis hour
       jdate(5) = iadate(5)  ! analysis minute
       jdate(6) = 0      ! analysis scaled seconds
       jdate(7) = idate(7)   ! analysis seconds multiplier

       nfhour   =0       !  new forecast hour, zero at analysis time
       nfminute =0    
       nfsecondn=0     
       nfsecondd=100      ! default for denominator

       gfshead%fhour = zero_single
       gfshead%idate(1) = jdate(4)  !hour
       gfshead%idate(2) = jdate(2)  !month
       gfshead%idate(3) = jdate(3)  !day
       gfshead%idate(4) = jdate(1)  !year
!
!      open new output file with new header gfileo with "write" access. 
!      Use this call to update header as well
!
       call nemsio_open(gfileo,filename,'write',iret=iret, idate=jdate, nfhour=nfhour,&
          nfminute=nfminute, nfsecondn=nfsecondn, nfsecondd=nfsecondd)
       if ( iret/=0 ) then
          write(6,*)'WRITE_NEMSATM:  problem opening file ',trim(filename),', Status = ',iret
          call stop2(103)
       end if

!      Allocate structure arrays to hold data
       allocate(rwork1d(gfshead%latb*gfshead%lonb))

       call nemsio_close(gfile,iret)
          
    endif
!
!   Thermodynamic variable
!   The GSI analysis variable is virtual temperature (Tv).   For NEMSIO
!   output we need the sensible temperature.
!
!   Convert Tv to T
    tvsm = tvsm/(one+fv*qsm)

!   Generate and write analysis fields
!   Terrain
    call mpi_gatherv(hsm,ijn(mm1),mpi_rtype,&
         work1,ijn,displs_g,mpi_rtype,&
         mype_out,mpi_comm_world,ierror)
    if (mype==mype_out) then
       call load_grid(work1,grid)
       rwork1d = reshape(grid,(/size(rwork1d)/))
       call nemsio_writerecv(gfileo,'hgt','sfc',1,rwork1d,iret=iret)
       iret_wrt = iret_wrt + iret
    endif

!   Surface pressure.  
    call mpi_gatherv(psm,ijn(mm1),mpi_rtype,&
         work1,ijn,displs_g,mpi_rtype,&
         mype_out,mpi_comm_world,ierror)
    if (mype==mype_out) then
       call load_grid(work1,grid)
       grid2 = grid*r1000
       rwork1d = reshape(grid2,(/size(rwork1d)/))
       call nemsio_writerecv(gfileo,'pres','sfc',1,rwork1d,iret=iret)
       iret_wrt = iret_wrt + iret
    endif

!   Pressure depth
    do k=1,nsig
       call mpi_gatherv(dpsm(1,k),ijn(mm1),mpi_rtype,&
            work1,ijn,displs_g,mpi_rtype,&
            mype_out,mpi_comm_world,ierror)
       if (mype==mype_out) then
          call load_grid(work1,grid)
          grid2 = grid*r1000
          rwork1d = reshape(grid2,(/size(rwork1d)/))
          call nemsio_writerecv(gfileo,'dpres','mid layer',k,rwork1d,iret=iret)
          iret_wrt = iret_wrt + iret
       endif
    end do

!   Layer mean pressure
    do k=1,nsig
       call mpi_gatherv(prslm(1,k),ijn(mm1),mpi_rtype,&
            work1,ijn,displs_g,mpi_rtype,&
            mype_out,mpi_comm_world,ierror)
       if (mype==mype_out) then
          call load_grid(work1,grid)
          grid2 = grid*r1000
          rwork1d = reshape(grid2,(/size(rwork1d)/))
          call nemsio_writerecv(gfileo,'pres','mid layer',k,rwork1d,iret=iret)
          iret_wrt = iret_wrt + iret
       endif
    end do

!   Zonal wind
    do k=1,nsig
       call mpi_gatherv(usm(1,k),ijn(mm1),mpi_rtype,&
            work1,ijn,displs_g,mpi_rtype,&
            mype_out,mpi_comm_world,ierror)
       if (mype==mype_out) then
          call load_grid(work1,grid)
          rwork1d = reshape(grid,(/size(rwork1d)/))
          call nemsio_writerecv(gfileo,'ugrd','mid layer',k,rwork1d,iret=iret)
          iret_wrt = iret_wrt + iret
       endif
    end do

!   Meridional wind
    do k=1,nsig
       call mpi_gatherv(vsm(1,k),ijn(mm1),mpi_rtype,&
            work1,ijn,displs_g,mpi_rtype,&
            mype_out,mpi_comm_world,ierror)
       if (mype==mype_out) then
          call load_grid(work1,grid)
          rwork1d = reshape(grid,(/size(rwork1d)/))
          call nemsio_writerecv(gfileo,'vgrd','mid layer',k,rwork1d,iret=iret)
          iret_wrt = iret_wrt + iret
       endif
    end do

!   Thermodynamic variable
    do k=1,nsig
       call mpi_gatherv(tvsm(1,k),ijn(mm1),mpi_rtype,&
            work1,ijn,displs_g,mpi_rtype,&
            mype_out,mpi_comm_world,ierror)
       if (mype == mype_out) then
          call load_grid(work1,grid)
          rwork1d = reshape(grid,(/size(rwork1d)/))
          call nemsio_writerecv(gfileo,'tmp','mid layer',k,rwork1d,iret=iret)
          iret_wrt = iret_wrt + iret
       endif
    end do
!   Specific humidity
    do k=1,nsig
       call mpi_gatherv(qsm(1,k),ijn(mm1),mpi_rtype,&
            work1,ijn,displs_g,mpi_rtype,&
            mype_out,mpi_comm_world,ierror)
       if (mype == mype_out) then
          call load_grid(work1,grid)
          rwork1d = reshape(grid,(/size(rwork1d)/))
          call nemsio_writerecv(gfileo,'spfh','mid layer',k,rwork1d,iret=iret)
          iret_wrt = iret_wrt + iret
       endif
    end do

!   Ozone
    do k=1,nsig
       call mpi_gatherv(ozsm(1,k),ijn(mm1),mpi_rtype,&
            work1,ijn,displs_g,mpi_rtype,&
            mype_out,mpi_comm_world,ierror)
       if (mype == mype_out) then
          call load_grid(work1,grid)
           rwork1d = reshape(grid,(/size(rwork1d)/))
           call nemsio_writerecv(gfileo,'o3mr','mid layer',k,rwork1d,iret=iret)
          iret_wrt = iret_wrt + iret
       endif
    end do
       
!   Cloud condensate mixing ratio
    if (ntracer>2 .or. ncloud>=1) then
       do k=1,nsig
          call mpi_gatherv(cwsm(1,k),ijn(mm1),mpi_rtype,&
               work1,ijn,displs_g,mpi_rtype,&
               mype_out,mpi_comm_world,ierror)
          if (mype == mype_out) then
             call load_grid(work1,grid)
             rwork1d = reshape(grid,(/size(rwork1d)/))
             call nemsio_writerecv(gfileo,'clwmr','mid layer',k,rwork1d,iret=iret)
             iret_wrt = iret_wrt + iret
          endif
       end do
    endif
!
! Deallocate local array
!
    if (mype==mype_out) then
       deallocate(rwork1d)
!
       string='nemsio'
       call nemsio_close(gfileo,iret)
       if ( iret /= 0 ) then
          write(6,*)'WRITE_NEMSATM:  problem closing file ',trim(filename),', Status = ',iret
          call stop2(103)
       end if
       write(6,110) string,gfshead%latb,gfshead%lonb,&
          gfshead%levs,gfshead%fhour,gfshead%idate, iret_wrt
110    format('WRITE_NEMSATM:  NCEP ',a5,&
          ' atm anal written for latb,lonb,levs= ',3i6,&
          ' valid hour,idate= ',f3.1,4(i4,1x), 'iret = ',i4)
    endif

    return
  end subroutine write_atm_


  subroutine write_sfc_ (filename,mype,mype_sfc,dsfct)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    write_nemssfc --- Write surface analysis to file
!
!   prgrmmr:     treadon -  initial version; org: np23
!   prgmmr: hcHuang          org: np23                date: 2010-02-22
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
!   2010-02-22  hcHuang  Initial version.  Based on write_gfssfc
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
    use kinds, only: r_kind,r_single,i_kind
  
    use mpimod, only: mpi_rtype
    use mpimod, only: mpi_comm_world
    use mpimod, only: ierror
    
    use gridmod, only: nlat,nlon
    use gridmod, only: lat1,lon1
    use gridmod, only: lat2,lon2
    use gridmod, only: iglobal
    use gridmod, only: ijn
    use gridmod, only: ltosi,ltosj
    use gridmod, only: displs_g
    use gridmod, only: itotsub
    
    use obsmod, only: iadate
    
    use constants, only: zero,zero_single
    
    use sfcio_module, only: sfcio_intkind,sfcio_head,sfcio_data,&
                            sfcio_aldata,sfcio_axdata

    use nemsio_module, only:  nemsio_init,nemsio_open,nemsio_close,nemsio_readrecv
    use nemsio_module, only:  nemsio_gfile,nemsio_getfilehead
    use nemsio_module, only:  nemsio_readrec, nemsio_writerec, nemsio_writerecv

!! should be remove if subroutine sfc_interpolate move to this module
    use ncepgfs_io, only :  sfc_interpolate
    
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
    integer(sfcio_intkind),parameter:: ioges = 12
    integer(sfcio_intkind),parameter:: ioanl = 52

!   Declare local variables
    integer(i_kind),dimension(7):: idate, jdate
    integer(sfcio_intkind):: iret
    integer(i_kind) :: i, j, ip1, jp1, ilat, ilon, jj, mm1, ij
    integer(i_kind) :: nlatm2, n, nrec
    integer(i_kind) :: nfhour, nfminute, nfsecondn, nfsecondd

    real(r_kind),dimension(nlon,nlat):: buffer
    real(r_kind),dimension(lat1,lon1):: sfcsub
    real(r_kind),dimension(nlon,nlat):: grid
    real(r_kind),dimension(max(iglobal,itotsub)):: sfcall
    real(r_single),allocatable,dimension(:,:) :: buffer2, grid2
    real(r_kind),  allocatable,dimension(:)   :: rwork1d

    type(nemsio_gfile) :: gfile, gfileo
    type(sfcio_head)   :: sfc_head
    type(sfcio_data)   :: sfc_data

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
       if ( iret/=0 ) then
          write(6,*)'WRITE_NEMSSFC: problem with nemsio_init, Status = ',iret
          call stop2(104)
       end if
       call nemsio_open(gfile,fname_ges,'read',iret=iret)
!
       if ( iret/=0 ) then
          write(6,*)'WRITE_NEMSSFC:  problem opening file ',trim(fname_ges),', Status = ',iret
          call stop2(104)
       end if
!
       call nemsio_getfilehead(gfile, nrec=nrec, idate=idate,           &
          dimx=sfc_head%lonb, dimy=sfc_head%latb, nsoil=sfc_head%lsoil, &
          nfhour=nfhour, nfminute=nfminute, nfsecondn=nfsecondn, nfsecondd=nfsecondd,  &
          iret=iret)
!
!      Replace header record date with analysis time from iadate
!
       jdate(1) = iadate(1)  ! analysis year
       jdate(2) = iadate(2)  ! analysis month
       jdate(3) = iadate(3)  ! analysis day
       jdate(4) = iadate(4)  ! analysis hour
       jdate(5) = iadate(5)  ! analysis minute
       jdate(5) = 0      ! analysis minute
       jdate(6) = 0      ! analysis scaled seconds
       jdate(7) = idate(7)   ! analysis seconds multiplier

       nfhour=0       !  new forecast hour, zero at analysis time
       nfminute=0
       nfsecondn=0
       nfsecondd=100      ! default for denominator

       sfc_head%fhour    = zero_single
       sfc_head%idate(1) = jdate(4)  !hour
       sfc_head%idate(2) = jdate(2)  !month
       sfc_head%idate(3) = jdate(3)  !day
       sfc_head%idate(4) = jdate(1)  !year
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
       if ( iret/=0 ) then
          write(6,*)'WRITE_NEMSSFC:  problem opening file ',trim(filename),', Status = ',iret
          call stop2(104)
       end if
!
!      First copy entire data from fname_ges to filename, then do selective update
!
       allocate(rwork1d(sfc_head%lonb*sfc_head%latb))
       allocate(buffer2(sfc_head%lonb,sfc_head%latb))
       allocate(grid2(sfc_head%lonb,sfc_head%latb))
       allocate(sfc_data%tsea(sfc_head%lonb,sfc_head%latb))

       do n = 1, nrec
         call nemsio_readrec (gfile, n,rwork1d,iret=iret)
         if ( iret /= 0 ) write(6,*) 'readrec  nrec = ', n, '  iret = ', iret
         call nemsio_writerec(gfileo,n,rwork1d,iret=iret)
         if ( iret /= 0 ) write(6,*) 'writerec nrec = ', n, '  iret = ', iret
       end do
!
! Only sea surface temperature will be updated in the SFC files
!

       call nemsio_readrecv(gfile,'tmp','sfc',1,rwork1d,iret=iret)
       sfc_data%tsea=reshape(rwork1d,(/size(sfc_data%tsea,1),size(sfc_data%tsea,2)/))

       if ( (sfc_head%latb /= nlatm2) .or. (sfc_head%lonb /= nlon) ) then
          write(6,*)'WRITE_NEMSSFC:  different grid dimensions analysis', &
             ' vs sfc. interpolating sfc temperature nlon,nlat-2=',nlon,  &
             nlatm2,' -vs- sfc file lonb,latb=',sfc_head%lonb,sfc_head%latb
          call sfc_interpolate(buffer,nlon,nlat,buffer2,sfc_head%lonb,sfc_head%latb)
       else
          do j=1,sfc_head%latb
             do i=1,sfc_head%lonb
                buffer2(i,j)=buffer(i,j+1)
             end do
          end do
       endif

       grid2 = sfc_data%tsea + buffer2
       rwork1d = reshape( grid2,(/size(rwork1d)/) )

       deallocate(buffer2)

!      update tsea record
       call nemsio_writerecv(gfileo,'tmp','sfc',1,rwork1d,iret=iret)
       if ( iret /= 0 ) then
          write(6,*)'WRITE_NEMSSFC:  ***ERROR***  with write Tsea ',iret
          call stop2(104)
       endif
       deallocate(rwork1d)

       call nemsio_close(gfile, iret=iret)
       call nemsio_close(gfileo,iret=iret)

       if ( iret /= 0 ) then
          write(6,*)'WRITE_NEMSSFC:  problem closing file ',trim(filename),', Status = ',iret
          call stop2(104)
       end if
       write(6,100) sfc_head%lonb,sfc_head%latb,sfc_head%fhour,sfc_head%idate,iret
100    format(' WRITE_NEMSSFC:  sfc analysis written  for ',&
          2i6,1x,f3.1,4(i4,1x),' with iret=',i2)

    endif
    
!   End of routine
    return
  end subroutine write_sfc_

end module ncepnems_io

