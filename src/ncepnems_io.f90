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
    use gridmod, only: sp_a
    use guess_grids, only: ges_z,ges_ps,ges_vor,ges_div,&
         ges_u,ges_v,ges_tv,ges_q,ges_oz,&
         ifilesig,nfldsig
    use gsi_metguess_mod, only: gsi_metguess_bundle
    use gsi_bundlemod, only: gsi_bundlegetpointer
    use mpeu_util, only: die
    implicit none

    integer(i_kind),intent(in   ) :: mype

    character(len=*),parameter::myname_=myname//'*read_'
    character(24) filename
    integer(i_kind):: it, iret
    real(r_kind),pointer,dimension(:,:,:):: ges_cwmr_it

    do it=1,nfldsig

!      Get pointer to could water mixing ratio
       call gsi_bundlegetpointer (gsi_metguess_bundle(it),'cw',ges_cwmr_it,iret)
       if (iret/=0) call die(trim(myname_),'cannot get pointer to cwmr, iret =',iret)

       write(filename,'(''sigf'',i2.2)') ifilesig(it)
       call read_atm_ (filename,mype,sp_a,&
            ges_z(1,1,it),ges_ps(1,1,it),&
            ges_vor(1,1,1,it),ges_div(1,1,1,it),&
            ges_u(1,1,1,it),ges_v(1,1,1,it),&
            ges_tv(1,1,1,it),ges_q(1,1,1,it),&
            ges_cwmr_it,ges_oz(1,1,1,it))
    end do
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
    use guess_grids, only: ges_ps,nfldsig,ntguessig
    use gsi_bundlemod, only: gsi_bundlegetpointer
    use gsi_chemguess_mod, only: gsi_chemguess_bundle
    use gsi_chemguess_mod, only: gsi_chemguess_get

    implicit none

!   Declared argument list
    integer(i_kind), intent(in):: iyear
    integer(i_kind), intent(in):: month
    integer(i_kind), intent(in):: idd

!   Declare local variables
    integer(i_kind) :: igfsco2, i, j, k, n, iret
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

  subroutine read_atm_ (filename,mype,sp,g_z,g_ps,g_vor,g_div,g_u,g_v,&
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
!
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
         itotsub,fill_ns,filluv_ns,ntracer,ncloud,reload
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
    real(r_kind),dimension(lat2,lon2)     ,intent(  out) :: g_z,g_ps
    real(r_kind),dimension(lat2,lon2,nsig),intent(  out) :: g_u,g_v,&
         g_vor,g_div,g_cwmr,g_q,g_oz,g_tv
    type(spec_vars)                       ,intent(in   ) :: sp
    
!   Declare local variables
    character(len=120) :: my_name = 'READ_NEMSATM'
    character(len=1)   :: null = ' '
    integer(i_kind),dimension(7):: idate
    integer(i_kind),dimension(4):: odate
    integer(i_kind) :: iret,nlatm2,n,l,m
    integer(i_kind) :: i,j,k,icount,icount_prev,mm1
    integer(i_kind) :: mype_hs, mype_ps
    integer(i_kind) :: latb, lonb, levs, nframe
    integer(i_kind) :: nfhour, nfminute, nfsecondn, nfsecondd
    integer(i_kind) :: istop = 101
    real(r_kind),allocatable,dimension(:,:) :: grid, grid_v, &
         grid_vor, grid_div
    real(r_kind),allocatable,dimension(:)   :: work, work_vor, work_div, &
         work_u, work_v
    real(r_kind),allocatable,dimension(:,:) :: sub, sub_vor, sub_div, &
         sub_u, sub_v
    real(r_kind),dimension(sp%nc):: spec_vor,spec_div
    real(r_kind),allocatable,dimension(:) :: rwork1d0, rwork1d1, rwork1d2
    real(r_kind) :: fhour
    type(nemsio_gfile) :: gfile
!******************************************************************************  
!   Initialize variables used below
    mm1=mype+1
    mype_hs=npe-2
    mype_ps=npe-1
    nlatm2=nlat-2

    call nemsio_init(iret=iret)
    if (iret /= 0) call error_msg(mype,trim(my_name),trim(filename),null,'init',istop,iret)

    call nemsio_open(gfile,filename,'READ',iret=iret)
    if (iret /= 0) call error_msg(mype,trim(my_name),trim(filename),null,'open',istop,iret)

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
    if(latb /= nlatm2) then
       if ( mype == 0 ) write(6, &
          '(a,'': inconsistent spatial dimension nlatm2 = '',i4,tr1,''latb = '',i4)') &
          trim(my_name),nlatm2,latb
       call stop2(101)
    end if
    if(lonb /= nlon) then
       if ( mype == 0 ) write(6, &
          '(a,'': inconsistent spatial dimension nlon   = '',i4,tr1,''lonb = '',i4)') &
          trim(my_name),nlon,lonb
       call stop2(101)
    end if
    if(levs /= nsig)then
       if ( mype == 0 ) write(6, &
          '(a,'': inconsistent spatial dimension nsig   = '',i4,tr1,''levs = '',i4)') &
          trim(my_name),nsig,levs
       call stop2(101)
    end if
!
    allocate( grid(nlon,nlatm2), grid_v(nlon,nlatm2) )
    allocate( grid_vor(nlon,nlatm2), grid_div(nlon,nlatm2) )
    allocate( work(itotsub),work_v(itotsub) )
    allocate( work_vor(itotsub),work_div(itotsub) )
    allocate( sub(lat2*lon2,max(2*nsig,npe)),sub_v(lat2*lon2,max(2*nsig,npe)) )
    allocate( sub_div(lat2*lon2,max(2*nsig,npe)),sub_vor(lat2*lon2,max(2*nsig,npe)) )
    allocate( rwork1d0(latb*lonb) )
!
!   Load values into rows for south and north pole before scattering
!
!   Terrain:  scatter to all mpi tasks
!
    if (mype==mype_hs) then
       call nemsio_readrecv(gfile,'hgt', 'sfc',1,rwork1d0,iret=iret)
       if (iret /= 0) call error_msg(mype,trim(my_name),trim(filename),'hgt','read',istop,iret)
       grid=reshape(rwork1d0,(/size(grid,1),size(grid,2)/))
       call fill_ns(grid,work)
    endif
    call mpi_scatterv(work,ijn_s,displs_s,mpi_rtype,&
       g_z,ijn_s(mm1),mpi_rtype,mype_hs,mpi_comm_world,ierror)

!   Surface pressure:  same procedure as terrain, but handled by task mype_ps
!
    if (mype==mype_ps) then
       allocate(rwork1d1(latb*lonb))
       call nemsio_readrecv(gfile,'pres','sfc',1,rwork1d0,iret=iret)
       if (iret /= 0) call error_msg(mype,trim(my_name),trim(filename),'pres','read',istop,iret)
       rwork1d1 = r0_001*rwork1d0
       grid=reshape(rwork1d1,(/size(grid,1),size(grid,2)/)) ! convert Pa to cb
       call fill_ns(grid,work)
       deallocate(rwork1d1)
    endif
    call mpi_scatterv(work,ijn_s,displs_s,mpi_rtype,&
       g_ps,ijn_s(mm1),mpi_rtype,mype_ps,mpi_comm_world,ierror)

!   Divergence and voriticity.  Compute u and v from div and vor
    sub_vor=zero
    sub_div=zero
    sub    =zero
    sub_v  =zero
    icount     =0
    icount_prev=1
    do k=1,levs
       icount=icount+1
       if (mype==mod(icount-1,npe)) then
          ! Convert grid u,v to div and vor
          call nemsio_readrecv(gfile,'ugrd','mid layer',k,rwork1d0,iret=iret)
          if (iret /= 0) call error_msg(mype,trim(my_name),trim(filename),'ugrd','read',istop,iret)
          grid=reshape(rwork1d0,(/size(grid,1),size(grid,2)/))

          call nemsio_readrecv(gfile,'vgrd','mid layer',k,rwork1d0,iret=iret)
          if (iret /= 0) call error_msg(mype,trim(my_name),trim(filename),'vgrd','read',istop,iret)
          grid_v=reshape(rwork1d0,(/size(grid_v,1),size(grid_v,2)/))

          call general_sptez_v(sp,spec_div,spec_vor,grid,grid_v,-1)
          call general_sptez_s(sp,spec_div,grid_div,1)
          call general_sptez_s(sp,spec_vor,grid_vor,1)

          ! Load values into rows for south and north pole
          call fill_ns(grid_div,work_div)
          call fill_ns(grid_vor,work_vor)
          call filluv_ns(grid,grid_v,work,work_v)
       endif
       ! Scatter to sub
       if (mod(icount,npe)==0 .or. icount==levs) then
          call mpi_alltoallv(work_vor,ijn_s,displs_s,mpi_rtype,&
             sub_vor(1,icount_prev),irc_s,ird_s,mpi_rtype,&
             mpi_comm_world,ierror)
          call mpi_alltoallv(work_div,ijn_s,displs_s,mpi_rtype,&
             sub_div(1,icount_prev),irc_s,ird_s,mpi_rtype,&
             mpi_comm_world,ierror)
          call mpi_alltoallv(work,ijn_s,displs_s,mpi_rtype,&
             sub(1,icount_prev),irc_s,ird_s,mpi_rtype,&
             mpi_comm_world,ierror)
          call mpi_alltoallv(work_v,ijn_s,displs_s,mpi_rtype,&
             sub_v(1,icount_prev),irc_s,ird_s,mpi_rtype,&
             mpi_comm_world,ierror)
          icount_prev=icount+1
       endif
    end do

    ! Transfer vor,div,u,v into real(r_kind) guess arrays
    call reload(sub_vor,g_vor)
    call reload(sub_div,g_div)
    call reload(sub,g_u)
    call reload(sub_v,g_v)
    deallocate(sub_vor,sub_div,work_vor,work_div)

!   Thermodynamic variable and Specific humidity:  communicate to all tasks
!
    sub=zero
    icount=0
    icount_prev=1
    do k=1,levs
       icount=icount+1
       if (mype==mod(icount-1,npe)) then
          allocate(rwork1d1(latb*lonb))
          allocate(rwork1d2(latb*lonb))

          call nemsio_readrecv(gfile,'spfh','mid layer',k,rwork1d0,iret=iret)
          if (iret /= 0) call error_msg(mype,trim(my_name),trim(filename),'spfh','read',istop,iret)
          grid=reshape(rwork1d0,(/size(grid,1),size(grid,2)/))
          call fill_ns(grid,work)

          call nemsio_readrecv(gfile,'tmp','mid layer',k,rwork1d1,iret=iret)
          if (iret /= 0) call error_msg(mype,trim(my_name),trim(filename),'tmp','read',istop,iret)
          rwork1d2 = rwork1d1*(one+fv*rwork1d0)
          grid_v=reshape(rwork1d2,(/size(grid_v,1),size(grid_v,2)/))
          call fill_ns(grid_v,work_v)

          deallocate(rwork1d1)
          deallocate(rwork1d2)
       endif

       if (mod(icount,npe)==0 .or. icount==levs) then
          call mpi_alltoallv(work_v,ijn_s,displs_s,mpi_rtype,&
             sub_v(1,icount_prev),irc_s,ird_s,mpi_rtype,&
             mpi_comm_world,ierror)
          call mpi_alltoallv(work,ijn_s,displs_s,mpi_rtype,&
             sub(1,icount_prev),irc_s,ird_s,mpi_rtype,&
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
          if (iret /= 0) call error_msg(mype,trim(my_name),trim(filename),'o3mr','read',istop,iret)
          grid=reshape(rwork1d0,(/size(grid,1),size(grid,2)/))
          call fill_ns(grid,work)
       endif
       if (mod(icount,npe)==0 .or. icount==levs) then
          call mpi_alltoallv(work,ijn_s,displs_s,mpi_rtype,&
             sub(1,icount_prev),irc_s,ird_s,mpi_rtype,&
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
             if (iret /= 0) call error_msg(mype,trim(my_name),trim(filename),'clwmr','read',istop,iret)
             grid=reshape(rwork1d0,(/size(grid,1),size(grid,2)/))
             call fill_ns(grid,work)
          endif
          if (mod(icount,npe)==0 .or. icount==levs) then
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

    deallocate(rwork1d0)
    deallocate(grid,work,sub)
    call nemsio_close(gfile,iret=iret)
    if (iret /= 0) call error_msg(mype,trim(my_name),trim(filename),null,'close',istop,iret)

!   Print date/time stamp 
    if ( mype == 0 ) write(6, &
       '(a,'': ges read/scatter,lonb,latb,levs= '',3i6,'',hour= '',f3.1,'',idate= '',4i5)') &
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
       '(a,'': sfc read,nlon,nlat= '',2i6,'',hour= '',f3.1,'',idate= '',4i5)') &
       trim(my_name),lonb,latb,fhour,odate

  end subroutine read_sfc_

  subroutine write_ (increment,mype,mype_atm,mype_sfc)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    write_nems
!
!   prgmmr: Huang            org: np23                date: 2010-02-22
!
! abstract:
!
! program history log:
!   2010-02-22  Huang    Initial version.  Based on write_gfs
!   2011-05-01  todling - cwmr no longer in guess-grids; use metguess bundle now
!
!   input argument list:
!     increment          - when >0 will write increment from increment-index slot
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

    use kinds, only: i_kind,r_kind
    use guess_grids, only: ges_z,ges_ps,ges_vor,ges_div,&
         ges_tv,ges_q,ges_oz,ges_prsl,&
         ges_u,ges_v,ges_prsi,dsfct 
    use guess_grids, only: ntguessig,ntguessfc
    use gsi_metguess_mod, only: gsi_metguess_bundle
    use gsi_bundlemod, only: gsi_bundlegetpointer
    use mpeu_util, only: die

    implicit none

    integer(i_kind),intent(in   ) :: increment
    integer(i_kind),intent(in   ) :: mype,mype_atm,mype_sfc

    character(len=*),parameter::myname_=myname//'*write_'
    character(len=24)             :: filename
    integer(i_kind)               :: itoutsig, iret
    real(r_kind),pointer,dimension(:,:,:):: ges_cwmr_it

!   Write atmospheric analysis file
    if (increment>0) then
       filename='siginc'
       itoutsig=increment
    else
       filename='siganl'
       itoutsig=ntguessig
    endif
!   Get pointer to could water mixing ratio
    call gsi_bundlegetpointer (gsi_metguess_bundle(itoutsig),'cw',ges_cwmr_it,iret)
    if (iret/=0) call die(trim(myname_),'cannot get pointer to cwmr, iret =',iret)

    call write_atm_ (filename,mype,mype_atm,&
         ges_z(1,1,itoutsig),ges_ps(1,1,itoutsig),&
         ges_vor(1,1,1,itoutsig),ges_div(1,1,1,itoutsig),&
         ges_tv(1,1,1,itoutsig),ges_q(1,1,1,itoutsig),&
         ges_oz(1,1,1,itoutsig),ges_cwmr_it,&
         ges_prsl(1,1,1,itoutsig),ges_u(1,1,1,itoutsig),&
         ges_v(1,1,1,itoutsig),ges_prsi(1,1,1,itoutsig))

!   Write surface analysis file
    if (increment>0) then
       filename='sfcinc.gsi'
    else
       filename='sfcanl.gsi'
    endif
    call write_sfc_ (filename,mype,mype_sfc,dsfct(1,1,ntguessfc))
  end subroutine write_


  subroutine write_atm_ (filename,mype,mype_out,sub_z,sub_ps,&
       sub_vor,sub_div,sub_tv,sub_q,sub_oz,sub_cwmr,sub_prsl,&
       sub_u,sub_v,sub_prsi)

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
    
    use constants, only: r1000,fv,one,zero
  
    use mpimod, only: mpi_rtype,mpi_rtype4
    use mpimod, only: mpi_comm_world
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

!-------------------------------------------------------------------------

    character(6):: fname_ges
    character(len=120) :: my_name = 'WRITE_NEMSATM'
    character(len=1)   :: null = ' '
    integer(i_kind),dimension(7):: idate, jdate
    integer(i_kind),dimension(4):: odate
    integer(i_kind) :: i, j, ij, k, n, mm1, nlatm2
    integer(i_kind) :: iret, lonb, latb, levs
    integer(i_kind) :: nfhour, nfminute, nfsecondn, nfsecondd
    integer(i_kind) :: istop = 103
    real(r_kind)    :: fhour
    
    real(r_kind),dimension(lat1*lon1)     :: hsm, psm
    real(r_kind),dimension(lat2,lon2,nsig):: sub_dp
    real(r_kind),dimension(lat1*lon1,nsig):: tvsm,prslm, usm, vsm
    real(r_kind),dimension(lat1*lon1,nsig):: dpsm, qsm, ozsm, cwsm
    real(r_kind),dimension(max(iglobal,itotsub))     :: work1
    real(r_kind),dimension(nlon,nlat-2):: grid, grid2
    real(r_kind),allocatable,dimension(:) :: rwork1d

    type(nemsio_gfile) :: gfile,gfileo
!*************************************************************************
!   Initialize local variables
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
       if(levs/=nsig) then
          write(6,*)trim(my_name),': problem in data dimension background levs = ',levs,' nsig = ',nsig
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
       allocate(rwork1d(latb*lonb))

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
       if (iret /= 0) call error_msg(0,trim(my_name),trim(filename),'hgt','write',istop,iret)
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
       if (iret /= 0) call error_msg(0,trim(my_name),trim(filename),'psfc','write',istop,iret)
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
          if (iret /= 0) call error_msg(0,trim(my_name),trim(filename),'dpres','write',istop,iret)
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
          if (iret /= 0) call error_msg(0,trim(my_name),trim(filename),'pres','write',istop,iret)
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
          if (iret /= 0) call error_msg(0,trim(my_name),trim(filename),'ugrd','write',istop,iret)
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
          if (iret /= 0) call error_msg(0,trim(my_name),trim(filename),'vgrd','write',istop,iret)
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
          if (iret /= 0) call error_msg(0,trim(my_name),trim(filename),'tmp','write',istop,iret)
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
          if (iret /= 0) call error_msg(0,trim(my_name),trim(filename),'spfh','write',istop,iret)
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
          if (iret /= 0) call error_msg(0,trim(my_name),trim(filename),'o3mr','write',istop,iret)
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
             if (iret /= 0) call error_msg(0,trim(my_name),trim(filename),'clwmr','write',istop,iret)
          endif
       end do
    endif
!
! Deallocate local array
!
    if (mype==mype_out) then
       call nemsio_close(gfile,iret)
       if (iret /= 0) call error_msg(0,trim(my_name),trim(fname_ges),null,'close',istop,iret)

       call nemsio_close(gfileo,iret)
       if (iret /= 0) call error_msg(0,trim(my_name),trim(filename),null,'close',istop,iret)
!
! Deallocate local array
!
       deallocate(rwork1d)
!
       write(6,'(a,'': atm anal written for lonb,latb,levs= '',3i6,'',valid hour= '',f3.1,'',idate= '',4i5)') &
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
    use gridmod, only: ltosi,ltosj
    use gridmod, only: displs_g
    use gridmod, only: itotsub
    
    use obsmod, only: iadate
    
    use constants, only: zero
    
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
!   Declare local variables
    character(len=120) :: my_name = 'WRITE_NEMSSFC'
    character(len=1)   :: null = ' '
    integer(i_kind),dimension(7):: idate, jdate
    integer(i_kind),dimension(4):: odate
    integer(i_kind) :: i, j, ip1, jp1, ilat, ilon, jj, mm1, ij
    integer(i_kind) :: nlatm2, n, nrec, lonb, latb, iret
    integer(i_kind) :: nfhour, nfminute, nfsecondn, nfsecondd
    integer(i_kind) :: istop = 104
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

       write(6,'(a,'': sfc anal written for lonb,latb= '',2i6,'',valid hour= '',f3.1,'',idate= '',4i5)') &
          trim(my_name),lonb,latb,fhour,odate
    endif
  end subroutine write_sfc_

  subroutine error_msg(mype,sub_name,file_name,var_name,action,stop_code,error_code)
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
  end subroutine error_msg

end module ncepnems_io

