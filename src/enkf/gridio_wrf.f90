module gridio

  !========================================================================

  !$$$ Module documentation block
  ! 
  ! This module contains various routines to ingest and update
  ! variables from Weather Research and Forecasting (WRF) model Advanced
  ! Research WRF (ARW) and Non-hydrostatic Mesoscale Model (NMM) dynamical
  ! cores which are required by the Ensemble Kalman Filter (ENKF) currently
  ! designed for operations within the National Centers for Environmental
  ! Prediction (NCEP) Global Forecasting System (GFS)
  !
  ! prgmmr: Winterbottom        org: ESRL/PSD1       date: 2011-11-30
  !
  ! program history log:
  !   
  !   2011-11-30  Initial version.
  !
  ! attributes:
  !   language:  f95
  !
  !$$$

  !=========================================================================

  ! Define associated modules

  use gridinfo, only: dimensions, gridvarstring, npts, cross2dot, dot2cross
  use kinds,    only: r_double, r_kind, r_single
  use mpisetup, only: nproc
  use netcdf_io
  use params,   only: nlevs, nvars, nlons, nlats, cliptracers, datapath,     &
                      arw, nmm, datestring, pseudo_rh,                       &
                      nbackgrounds,fgfileprefixes,anlfileprefixes
  use constants, only: zero,one,cp,fv,rd,grav,zero

  implicit none

  !-------------------------------------------------------------------------

  ! Define all public subroutines within this module

  private
  public :: readgriddata
  public :: writegriddata

  !-------------------------------------------------------------------------

contains

  subroutine readgriddata(nanal,vargrid,qsat)
   integer,                                                   intent(in)  :: nanal
   real(r_single), dimension(npts,nvars*nlevs+1,nbackgrounds), intent(out) :: vargrid
   real(r_double), dimension(npts,nlevs,nbackgrounds),         intent(out) :: qsat
   if (arw) then
     call readgriddata_arw(nanal,vargrid,qsat)
   else
     call readgriddata_nmm(nanal,vargrid,qsat)
   endif
  end subroutine readgriddata

  !========================================================================

  ! readgriddata_arw.f90: This subroutine will receive a WRF-ARW
  ! netcdf file name and variable string and will subsequently return
  ! the respective variable interpolated to an unstaggered grid; all
  ! checks for grid staggering are contained within this subroutine

  !-------------------------------------------------------------------------

  subroutine readgriddata_arw(nanal,vargrid,qsat)

    use constants

    !======================================================================

    ! Define array dimension variables

    integer                                                                      :: xdim, ydim, zdim

    ! Define variables passed to subroutine

    character(len=500)                                                           :: filename
    character(len=3)                                                             :: charnanal
    integer,                                                         intent(in)  :: nanal

    ! Define variables returned by subroutine

    real(r_single), dimension(npts,nvars*nlevs+1,nbackgrounds), intent(out) :: vargrid
    real(r_double), dimension(npts,nlevs,nbackgrounds),         intent(out) :: qsat

    ! Define variables computed within subroutine

    logical                                                                      :: ice
    real,       dimension(:,:,:),               allocatable              :: wrfarw_pert_pottemp
    real,       dimension(:,:,:),               allocatable              :: wrfarw_znu
    real,       dimension(:,:,:),               allocatable              :: wrfarw_psfc
    real,       dimension(:,:,:),               allocatable              :: wrfarw_mu
    real,       dimension(:,:,:),               allocatable              :: wrfarw_mub
    real,       dimension(:,:,:),               allocatable              :: wrfarw_mixratio
    real,       dimension(:,:,:),               allocatable              :: wrfarw_ptop
    real,       dimension(:,:,:),               allocatable              :: workgrid
    real,       dimension(:,:,:),               allocatable              :: vargrid_native
    real(r_single),     dimension(:,:),                 allocatable              :: enkf_virttemp
    real(r_single),     dimension(:,:),                 allocatable              :: enkf_pressure
    real(r_single),     dimension(:,:),                 allocatable              :: enkf_spechumd
    real(r_single)                                                               :: kap
    real(r_single)                                                               :: kap1
    real(r_single)                                                               :: kapr
    integer                                                                      :: xdim_native
    integer                                                                      :: ydim_native
    integer                                                                      :: zdim_native
    integer                                                                      :: xdim_local
    integer                                                                      :: ydim_local
    integer                                                                      :: zdim_local

    ! Define variables requiredfor netcdf variable I/O

    character(len=12)                                                            :: varstrname
    character(len=50)                                                            :: attstr
    character(len=12)                                                            :: varstagger
    character(len=12)                                                            :: varmemoryorder

    ! Define counting variables

    integer                                                                      :: i, j, k, l, nb
    integer                                                                      :: counth, countv
    integer                                                                      :: count

    !======================================================================

    ! Initialize all constants required by routine

    call init_constants(.true.)

    ! Define all local variables

    xdim = dimensions%xdim
    ydim = dimensions%ydim
    zdim = dimensions%zdim
       
    !======================================================================

    ! Begin: Loop through each (prognostic) variable (defined in
    ! gridio.F90), determine and define the spatial array
    ! dimensions, and allocate memory for ARW dynamical core

    !----------------------------------------------------------------------

    if (nbackgrounds > 1) then
       write(6,*)'gridio/readgriddata: reading multiple backgrounds not yet supported'
       call stop2(23)
    endif
    backgroundloop: do nb=1,nbackgrounds

    ! Initialize counting variable

    countv = 1

    ! Define character string for ensemble member file

    write(charnanal,'(i3.3)') nanal
    filename = trim(adjustl(datapath))//trim(adjustl(fgfileprefixes(nb)))//"mem"//charnanal

    !----------------------------------------------------------------------

    ! Loop through all variables to be update via the EnKF

    do l = 1, nvars + 1

    !----------------------------------------------------------------------

       ! Define staggering attributes for variable grid

       attstr = 'stagger'
       call variableattribute_char(filename,gridvarstring(l),attstr,        &
            & varstagger)

       ! If variable grid is staggered in X-direction, assign array
       ! dimensions appropriately

       if(varstagger(1:1) .eq. 'X') then

          ! Assign array dimensions appropriately

          xdim_native = xdim + 1
          ydim_native = ydim
          zdim_native = zdim

       ! If variable grid is staggered in Y-direction, assign array
       ! dimensions appropriately

       else if(varstagger(1:1) .eq. 'Y') then ! if(varstagger(1:1) .eq. '
                                              ! X')

          ! Assign array dimensions appropriately

          xdim_native = xdim
          ydim_native = ydim + 1
          zdim_native = zdim

       ! If variable grid is staggered in Z-direction, assign array
       ! dimensions appropriately

       else if(varstagger(1:1) .eq. 'Z') then ! if(varstagger(1:1) .eq. '
                                              ! X')

          ! Assign array dimensions appropriately

          xdim_native = xdim
          ydim_native = ydim
          zdim_native = zdim + 1
             
       ! If variable grid is not staggered, assign array dimensions
       ! appropriately

       else ! if(varstagger(1:1) .eq. 'X')

          ! Assign array dimensions appropriately

          xdim_native = xdim
          ydim_native = ydim
          zdim_native = zdim

       end if ! if(varstagger(1:1) .eq. 'X')

    !----------------------------------------------------------------------

       ! Define memory attributes for variable grid

       attstr = 'MemoryOrder'
       call variableattribute_char(filename,gridvarstring(l),attstr,       &
            & varmemoryorder)

          ! If variable is a 2-dimensional field, rescale variables
          ! appropriately

          if(varmemoryorder(1:3) .eq. 'XY ') then

             ! Rescale grid dimension variables appropriately

             zdim_local = 1
             zdim_native = 1
       
          else

             ! Define local array dimension

             zdim_local = zdim

          end if ! if(varmemoryorder(1:3) .eq. 'XY ')

          ! Define local variable dimensions

          xdim_local = xdim
          ydim_local = ydim

          ! Allocate memory for local variable arrays

          if(.not. allocated(workgrid))                                     &
               & allocate(workgrid(xdim_local,ydim_local,zdim_local))
          if(.not. allocated(vargrid_native))                               &
               & allocate(vargrid_native(xdim_native,ydim_native,           &
               & zdim_native))

          ! Ingest variable from external netcdf formatted file
       
          call readnetcdfdata(filename,vargrid_native,gridvarstring(l),     &
               & xdim_native,ydim_native,zdim_native)
       
          ! Interpolate variable from staggered (i.e., C-) grid to
          ! unstaggered (i.e., A-) grid. If variable is staggered in
          ! vertical, intepolate from model layer interfaces
          ! (including surface and top) to model layer midpoints.

          call cross2dot(vargrid_native,xdim_native,ydim_native,            &
               & zdim_native,xdim_local,ydim_local,zdim_local,workgrid)

    !----------------------------------------------------------------------

          ! Loop through vertical coordinate

          do k = 1, zdim_local

             ! Initialize counting variable
          
             counth = 1

             ! Loop through meridional horizontal coordinate
             
             do j = 1, ydim_local

                ! Loop through zonal horizontal coordinate

                do i = 1, xdim_local

                   ! Assign values to output variable array
             
                   vargrid(counth,countv,nb) = workgrid(i,j,k)

                   ! Update counting variable

                   counth = counth + 1

                end do ! do i = 1, xdim_local

             end do ! do j = 1, ydim_local

             ! Print message to user

             if (nproc .eq. 0)                                               &
                  write(6,*) 'READGRIDDATA_ARW: ', trim(gridvarstring(l)),   &
                  & countv, minval(vargrid(:,countv,nb)),                       &
                  & maxval(vargrid(:,countv,nb))

             ! Update counting variable

             countv = countv + 1

          end do ! do k = 1, zdim_local

    !----------------------------------------------------------------------

          ! Deallocate memory for local variables

          if(allocated(vargrid_native)) deallocate(vargrid_native)
          if(allocated(workgrid))       deallocate(workgrid)

    !----------------------------------------------------------------------

       end do ! do l = 1, nvars + 1

    !----------------------------------------------------------------------

    ! End: Loop through each (prognostic) variable (defined in
    ! gridio.F90), determine and define the spatial array
    ! dimensions, and allocate memory for ARW dynamical core

    !======================================================================

    ! Begin: Ingest the necessary variables and compute the saturated
    ! specific humidity along the WRF-ARW grid; this routine assumes
    ! that all mass variables are defined along the unstaggered grid

    !----------------------------------------------------------------------

    ! Define all constants required by routine

    ice = .false.
    kap = rd/cp
    kapr = cp/rd
    kap1 = kap + 1

    !----------------------------------------------------------------------

    ! Allocate memory for all variables ingested by routine

    if(.not. allocated(wrfarw_pert_pottemp))                                &
         & allocate(wrfarw_pert_pottemp(xdim,ydim,zdim))
    if(.not. allocated(wrfarw_mixratio))                                    &
         & allocate(wrfarw_mixratio(xdim,ydim,zdim))
    if(.not. allocated(wrfarw_mu))                                          &
         & allocate(wrfarw_mu(xdim,ydim,1))
    if(.not. allocated(wrfarw_mub))                                         &
         & allocate(wrfarw_mub(xdim,ydim,1))
    if(.not. allocated(wrfarw_psfc))                                        &
         & allocate(wrfarw_psfc(xdim,ydim,1))
    if(.not. allocated(wrfarw_znu))                                         &
         & allocate(wrfarw_znu(1,1,zdim))
    if(.not. allocated(wrfarw_ptop))                                        &
         & allocate(wrfarw_ptop(1,1,1))

    ! Allocate memory for variables computed within routine

    if(.not. allocated(enkf_virttemp)) allocate(enkf_virttemp(npts,nlevs))
    if(.not. allocated(enkf_pressure)) allocate(enkf_pressure(npts,nlevs))
    if(.not. allocated(enkf_spechumd)) allocate(enkf_spechumd(npts,nlevs))

    !----------------------------------------------------------------------

    ! Ingest the perturbation potential temperature from the external
    ! file

    varstrname= 'T'
    call readnetcdfdata(filename,wrfarw_pert_pottemp,varstrname,xdim,       &
         & ydim,zdim)

    ! Ingest the water vapor mixing ratio from the external file

    varstrname = 'QVAPOR'
    call readnetcdfdata(filename,wrfarw_mixratio,varstrname,xdim,ydim,      &
         & zdim)

    ! Ingest the model vertical (eta) levels from the external file

    varstrname = 'ZNU'
    call readnetcdfdata(filename,wrfarw_znu,varstrname,1,1,zdim)

    ! Ingest the model perturbation dry air mass from the external
    ! file

    varstrname = 'MU'
    call readnetcdfdata(filename,wrfarw_mu,varstrname,xdim,ydim,1)

    ! Ingest the model base state dry air mass from the external file

    varstrname = 'MUB'
    call readnetcdfdata(filename,wrfarw_mub,varstrname,xdim,ydim,1)

    ! Ingest the model top pressure level from the external file

    varstrname = 'P_TOP'
    call readnetcdfdata(filename,wrfarw_ptop,varstrname,1,1,1)

    !----------------------------------------------------------------------

    ! Loop through vertical coordinate; compute the hydrostatic
    ! pressure level and subsequently the temperature at the
    ! respective level
    
    do k = 1, zdim

       ! Initialize counting variable

       count = 1

       ! Loop through meridional horizontal coordinate

       do j = 1, ydim

          ! Loop through zonal horizontal coordinate

          do i = 1, xdim
             
             ! Compute the dry hydrostatic pressure at the respective
             ! grid coordinate; This is dry pressure not full
             ! pressure, ignore this difference, since we are only
             ! using this to compute qsat, which in turn is only used
             ! to compute normalized humidity analysis variable

             enkf_pressure(count,k) = wrfarw_znu(1,1,k)*(wrfarw_mu(i,j,1)   &
                  & + wrfarw_mub(i,j,1)) + wrfarw_ptop(1,1,1)

             ! Compute mixing ratio from specific humidity.
             
             enkf_spechumd(count,k) = (wrfarw_mixratio(i,j,k))/(1.0 +       &
                  & wrfarw_mixratio(i,j,k))

             ! Compute virtual temp (this is only used to compute
             ! saturation specific humidity (call genqsat1)

             enkf_virttemp(count,k) = ((wrfarw_pert_pottemp(i,j,k) +        &
                  & 300.0)/((1000.0/(enkf_pressure(count,k)/100.0))         &
                  & **(rd/cp))) * (1. + fv*enkf_spechumd(count,k))

             ! Update counting variable

             count = count + 1

          end do ! do i = 1, xdim

       end do ! do j = 1, ydim

    end do ! do k = 1, zdim

    !----------------------------------------------------------------------

    ! Compute the saturation specific humidity

    if (pseudo_rh) then
       call genqsat1(enkf_spechumd,qsat(:,:,nb),enkf_pressure/100.0,enkf_virttemp,ice,  &
                     npts,nlevs)
    else
       qsat(:,:,nb) = 1._r_double
    endif
          

    !---------------------------------------------------------------------

    ! End: Ingest the necessary variables and compute the saturated
    ! specific humidity along the WRF-ARW grid; this routine assumes
    ! that all mass variables are defined along the unstaggered grid

    !======================================================================

    ! Deallocate memory for variables ingested by routine

    if(allocated(wrfarw_pert_pottemp)) deallocate(wrfarw_pert_pottemp)
    if(allocated(wrfarw_mixratio))     deallocate(wrfarw_mixratio)
    if(allocated(wrfarw_mu))           deallocate(wrfarw_mu)
    if(allocated(wrfarw_mub))          deallocate(wrfarw_mub)
    if(allocated(wrfarw_znu))          deallocate(wrfarw_znu)
    if(allocated(wrfarw_ptop))         deallocate(wrfarw_ptop)

    ! Deallocate memory for variables computed within routine

    if(allocated(enkf_virttemp))       deallocate(enkf_virttemp)
    if(allocated(enkf_pressure))       deallocate(enkf_pressure)
    if(allocated(enkf_spechumd))       deallocate(enkf_spechumd)

    end do backgroundloop ! loop over backgrounds to read in

    !======================================================================

    ! Return calculated values

    return

    !======================================================================

  end subroutine readgriddata_arw

  !========================================================================

  ! readgriddata_nmm.f90: This subroutine will receive a WRF-NMM
  ! netcdf file name and variable string and will subsequently return
  ! the respective variable interpolated to an unstaggered grid; all
  ! checks for grid staggering are contained within this subroutine

  !-------------------------------------------------------------------------

  subroutine readgriddata_nmm(nanal,vargrid,qsat)

    use constants
    !======================================================================

    ! Define array dimension variables

    integer                                                                      :: xdim, ydim, zdim

    ! Define variables passed to subroutine

    character(len=500)                                                           :: filename
    character(len=3)                                                             :: charnanal
    integer,                                                         intent(in)  :: nanal

    ! Define variables returned by subroutine

    real(r_single),  dimension(npts,nvars*nlevs+1,nbackgrounds),  intent(out) :: vargrid
    real(r_double),  dimension(npts,nlevs,nbackgrounds),          intent(out) :: qsat

    ! Define variables computed within subroutine

    logical                                                                      :: ice
    real,       dimension(:,:,:),               allocatable              :: wrfnmm_temp
    real,       dimension(:,:,:),               allocatable              :: wrfnmm_pres
    real,       dimension(:,:,:),               allocatable              :: wrfnmm_mixratio
    real,       dimension(:,:,:),               allocatable              :: wrfnmm_pd
    real,       dimension(:,:,:),               allocatable              :: wrfnmm_psfc
    real,       dimension(:,:,:),               allocatable              :: wrfnmm_eta1
    real,       dimension(:,:,:),               allocatable              :: wrfnmm_eta2
    real,       dimension(:,:,:),               allocatable              :: wrfnmm_pdtop
    real,       dimension(:,:,:),               allocatable              :: wrfnmm_pt
    real,       dimension(:,:,:),               allocatable              :: workgrid
    real,       dimension(:,:,:),               allocatable              :: vargrid_native
    real(r_single),     dimension(:,:),                 allocatable              :: enkf_virttemp
    real(r_single),     dimension(:,:),                 allocatable              :: enkf_pressure
    real(r_single),     dimension(:,:),                 allocatable              :: enkf_spechumd
    real(r_kind)                                                               :: kap
    real(r_kind)                                                               :: kap1
    real(r_kind)                                                               :: kapr
    integer                                                                      :: xdim_native
    integer                                                                      :: ydim_native
    integer                                                                      :: zdim_native
    integer                                                                      :: xdim_local
    integer                                                                      :: ydim_local
    integer                                                                      :: zdim_local

    ! Define variables requiredfor netcdf variable I/O

    character(len=12)                                                            :: varstrname
    character(len=50)                                                            :: attstr
    character(len=12)                                                            :: varstagger
    character(len=12)                                                            :: varmemoryorder

    ! Define counting variables

    integer                                                                      :: i, j, k, l, nb
    integer                                                                      :: counth, countv
    integer                                                                      :: count

    !======================================================================

    ! Initialize all constants required by routine

    call init_constants(.true.)

    ! Define all local variables

    xdim = dimensions%xdim
    ydim = dimensions%ydim
    zdim = dimensions%zdim
       
    !======================================================================

    ! Begin: Loop through each (prognostic) variable (defined in
    ! gridio.F90), determine and define the spatial array
    ! dimensions, and allocate memory for NMM dynamical core

    !----------------------------------------------------------------------
    if (nbackgrounds > 1) then
       write(6,*)'gridio/readgriddata: reading multiple backgrounds not yet supported'
       call stop2(23)
    endif
    backgroundloop: do nb=1,nbackgrounds

    ! Initialize counting variable

    countv = 1

    ! Define character string for ensemble member file

    write(charnanal,'(i3.3)') nanal
    filename = trim(adjustl(datapath))//trim(adjustl(fgfileprefixes(nb)))//"mem"//charnanal

    !----------------------------------------------------------------------

    ! Loop through all variables to be update via the EnKF

    do l = 1, nvars + 1

    !----------------------------------------------------------------------

       ! Define staggering attributes for variable grid

       attstr = 'stagger'
       call variableattribute_char(filename,gridvarstring(l),attstr,        &
            & varstagger)

       ! If variable grid is staggered in X-direction, assign array
       ! dimensions appropriately

       if(varstagger(1:1) .eq. 'X') then

          ! Assign array dimensions appropriately

          xdim_native = xdim + 1
          ydim_native = ydim
          zdim_native = zdim

       ! If variable grid is staggered in Y-direction, assign array
       ! dimensions appropriately

       else if(varstagger(1:1) .eq. 'Y') then ! if(varstagger(1:1) .eq. '
                                              ! X')

          ! Assign array dimensions appropriately

          xdim_native = xdim
          ydim_native = ydim + 1
          zdim_native = zdim

       ! If variable grid is staggered in Z-direction, assign array
       ! dimensions appropriately

       else if(varstagger(1:1) .eq. 'Z') then ! if(varstagger(1:1) .eq. '
                                              ! X')

          ! Assign array dimensions appropriately

          xdim_native = xdim
          ydim_native = ydim
          zdim_native = zdim + 1
             
       ! If variable grid is not staggered, assign array dimensions
       ! appropriately

       else ! if(varstagger(1:1) .eq. 'X')

          ! Assign array dimensions appropriately

          xdim_native = xdim
          ydim_native = ydim
          zdim_native = zdim

       end if ! if(varstagger(1:1) .eq. 'X')

    !----------------------------------------------------------------------

       ! Define memory attributes for variable grid

       attstr = 'MemoryOrder'
       call variableattribute_char(filename,gridvarstring(l),attstr,       &
            & varmemoryorder)

          ! If variable is a 2-dimensional field, rescale variables
          ! appropriately

          if(varmemoryorder(1:3) .eq. 'XY ') then

             ! Rescale grid dimension variables appropriately

             zdim_local = 1
             zdim_native = 1
       
          else

             ! Define local array dimension

             zdim_local = zdim

          end if ! if(varmemoryorder(1:3) .eq. 'XY ')

          ! Define local variable dimensions

          xdim_local = xdim
          ydim_local = ydim

          ! Allocate memory for local variable arrays

          if(.not. allocated(workgrid))                                     &
               & allocate(workgrid(xdim_local,ydim_local,zdim_local))
          if(.not. allocated(vargrid_native))                               &
               & allocate(vargrid_native(xdim_native,ydim_native,           &
               & zdim_native))

          ! Ingest variable from external netcdf formatted file
       
          call readnetcdfdata(filename,vargrid_native,gridvarstring(l),     &
               & xdim_native,ydim_native,zdim_native)
       
          ! Interpolate variable from staggered (i.e., E-) grid to
          ! unstaggered (i.e., A-) grid. If variable is staggered in
          ! vertical, intepolate from model layer interfaces
          ! (including surface and top) to model layer midpoints.

          call cross2dot(vargrid_native,xdim_native,ydim_native,            &
               & zdim_native,xdim_local,ydim_local,zdim_local,workgrid)

    !----------------------------------------------------------------------

          ! Loop through vertical coordinate

          do k = 1, zdim_local

             ! Initialize counting variable
          
             counth = 1

             ! Loop through meridional horizontal coordinate
             
             do j = 1, ydim_local

                ! Loop through zonal horizontal coordinate

                do i = 1, xdim_local

                   ! Assign values to output variable array
             
                   vargrid(counth,countv,nb) = workgrid(i,j,k)

                   ! Update counting variable

                   counth = counth + 1

                end do ! do i = 1, xdim_local

             end do ! do j = 1, ydim_local

             ! Print message to user

             if (nproc .eq. 0)                                               &
                  write(6,*) 'READGRIDDATA_NMM: ', trim(gridvarstring(l)),   &
                  & countv, minval(vargrid(:,countv,nb)),                    &
                  & maxval(vargrid(:,countv,nb))

             ! Update counting variable

             countv = countv + 1

          end do ! do k = 1, zdim_local

    !----------------------------------------------------------------------

          ! Deallocate memory for local variables

          if(allocated(vargrid_native)) deallocate(vargrid_native)
          if(allocated(workgrid))       deallocate(workgrid)

    !----------------------------------------------------------------------

       end do ! do l = 1, nvars + 1

    !----------------------------------------------------------------------

    ! End: Loop through each (prognostic) variable (defined in
    ! gridio.F90), determine and define the spatial array
    ! dimensions, and allocate memory for NMM dynamical core

    !======================================================================

    ! Begin: Ingest the necessary variables and compute the saturated
    ! specific humidity along the WRF-NMM grid; this routine assumes
    ! that all mass variables are defined along the unstaggered grid

    !----------------------------------------------------------------------

    ! Define all constants required by routine

    ice = .false.
    kap = rd/cp
    kapr = cp/rd
    kap1 = kap + 1

    !----------------------------------------------------------------------

    ! Allocate memory for all variables ingested by routine

    if(.not. allocated(wrfnmm_temp))                                        &
         & allocate(wrfnmm_temp(xdim,ydim,zdim))
    if(.not. allocated(wrfnmm_pres))                                        &
         & allocate(wrfnmm_pres(xdim,ydim,zdim))
    if(.not. allocated(wrfnmm_mixratio))                                    &
         & allocate(wrfnmm_mixratio(xdim,ydim,zdim))
    if(.not. allocated(wrfnmm_psfc))                                        &
         & allocate(wrfnmm_psfc(xdim,ydim,1)) 
    if(.not. allocated(wrfnmm_pd))                                          &
         & allocate(wrfnmm_pd(xdim,ydim,1)) 
    if(.not. allocated(wrfnmm_eta1))                                        &
         & allocate(wrfnmm_eta1(1,1,zdim))
    if(.not. allocated(wrfnmm_eta2))                                        &
         & allocate(wrfnmm_eta2(1,1,zdim))
    if(.not. allocated(wrfnmm_pdtop))                                       &
         & allocate(wrfnmm_pdtop(1,1,1))
    if(.not. allocated(wrfnmm_pt))                                          &
         & allocate(wrfnmm_pt(1,1,1))

    ! Allocate memory for variables computed within routine

    if(.not. allocated(enkf_virttemp)) allocate(enkf_virttemp(npts,nlevs))
    if(.not. allocated(enkf_pressure)) allocate(enkf_pressure(npts,nlevs))
    if(.not. allocated(enkf_spechumd)) allocate(enkf_spechumd(npts,nlevs))

    !----------------------------------------------------------------------

    ! Ingest the (sensible) temperature from the external file

    varstrname= 'T'
    call readnetcdfdata(filename,wrfnmm_temp,varstrname,xdim,ydim,zdim)

    ! Ingest the water vapor mixing ratio from the external file

    varstrname = 'Q'
    call readnetcdfdata(filename,wrfnmm_mixratio,varstrname,xdim,ydim,      &
         & zdim)

    ! Ingest surface pressure from the external file

    varstrname = 'PD'
    call readnetcdfdata(filename,wrfnmm_pd,varstrname,xdim,ydim,1)

    ! Ingest hybrid vertical coordinate from the external file

    varstrname = 'AETA1'
    call readnetcdfdata(filename,wrfnmm_eta1,varstrname,1,1,zdim)

    ! Ingest hybrid vertical coordinate from the external file

    varstrname = 'AETA2'
    call readnetcdfdata(filename,wrfnmm_eta2,varstrname,1,1,zdim)

    ! Ingest pressure at top of domain from the external file

    varstrname = 'PT'
    call readnetcdfdata(filename,wrfnmm_pt,varstrname,1,1,1)

    ! Ingest mass within pressure domain from the external file

    varstrname = 'PDTOP'
    call readnetcdfdata(filename,wrfnmm_pdtop,varstrname,1,1,1)

    !----------------------------------------------------------------------

    ! Loop through meridional horizontal coordinate
    
    do j = 1, ydim
       
       ! Loop through zonal horizontal coordinate
       
       do i = 1, xdim

          ! Compute the surface pressure profile

          wrfnmm_psfc(i,j,1) = (wrfnmm_pd(i,j,1) + wrfnmm_pdtop(1,1,1) +    &
               & wrfnmm_pt(1,1,1))

       end do ! do i = 1, xdim
          
    end do ! do j = 1, ydim

    ! Loop through vertical horizontal coordinate

    do k = 1, zdim
    
       ! Loop through meridional horizontal coordinate

       do j = 1, ydim
          
          ! Loop through zonal horizontal coordinate
          
          do i = 1, xdim

             ! Compute the pressure profile; the following formulation
             ! (should be) is identical to that in the Gridpoint
             ! Statistical Interpolation (GSI) routines for the
             ! WRF-NMM dynamical core

             wrfnmm_pres(i,j,k) = wrfnmm_eta1(1,1,k)*wrfnmm_pdtop(1,1,1) +  &
                  & wrfnmm_eta2(1,1,k)*(wrfnmm_psfc(i,j,1) -                &
                  & wrfnmm_pdtop(1,1,1) - wrfnmm_pt(1,1,1)) +               &
                  & wrfnmm_pt(1,1,1)

          end do ! do i = 1, xdim

       end do ! do j = 1, ydim

    end do ! do k = 1, zdim

    !----------------------------------------------------------------------

    ! Loop through vertical coordinate; compute the hydrostatic
    ! pressure level and subsequently the temperature at the
    ! respective level
    
    do k = 1, zdim

       ! Initialize counting variable

       count = 1

       ! Loop through meridional horizontal coordinate

       do j = 1, ydim

          ! Loop through zonal horizontal coordinate

          do i = 1, xdim
             
             ! Define the full pressure within model layers

             enkf_pressure(count,k) = wrfnmm_pres(i,j,k)

             ! Define the specific humidity with model layers
             
             enkf_spechumd(count,k) = wrfnmm_mixratio(i,j,k)

             ! Compute virtual temp (this is only used to compute
             ! saturation specific humidity (call genqsat1)

             enkf_virttemp(count,k) = &
             wrfnmm_temp(i,j,k)* (1. + fv*enkf_spechumd(count,k))
             
             ! Update counting variable

             count = count + 1

          end do ! do i = 1, xdim

       end do ! do j = 1, ydim

       ! Print message to user

       if(nproc .eq. 0) then

          ! Print message to user

          write(6,*) 'level, min(pres), max(pres): ', k,                    &
               & minval(enkf_pressure(1:(count - 1),k)),                    &
               & maxval(enkf_pressure(1:(count - 1),k)) 
          write(6,*) 'level, min(virttemp), max(virttemp): ', k,            &
               & minval(enkf_virttemp(1:(count - 1),k)),                    &
               & maxval(enkf_virttemp(1:(count - 1),k)) 
          write(6,*) 'level, min(sh), max(sh): ', k,                        &
               & minval(enkf_spechumd(1:(count - 1),k)),                    &
               & maxval(enkf_spechumd(1:(count - 1),k)) 

       end if ! if(nproc .eq. 0)

    end do ! do k = 1, zdim

    !----------------------------------------------------------------------

    ! Compute the saturation specific humidity

    if (pseudo_rh) then
       call genqsat1(enkf_spechumd,qsat(:,:,nb),enkf_pressure/100.0,enkf_virttemp,ice,  &
                    npts,nlevs)
    else
       qsat(:,:,nb) = 1._r_double
    endif

    !----------------------------------------------------------------------

    ! End: Ingest the necessary variables and compute the saturated
    ! specific humidity along the WRF-NMM grid; this routine assumes
    ! that all mass variables are defined along the unstaggered grid

    !======================================================================

    ! Deallocate memory for variables ingested by routine

    if(allocated(wrfnmm_temp))         deallocate(wrfnmm_temp)
    if(allocated(wrfnmm_pres))         deallocate(wrfnmm_pres)
    if(allocated(wrfnmm_mixratio))     deallocate(wrfnmm_mixratio)
    if(allocated(wrfnmm_psfc))         deallocate(wrfnmm_psfc)
    if(allocated(wrfnmm_pd))           deallocate(wrfnmm_pd)
    if(allocated(wrfnmm_eta1))         deallocate(wrfnmm_eta1)
    if(allocated(wrfnmm_eta2))         deallocate(wrfnmm_eta2)
    if(allocated(wrfnmm_pdtop))        deallocate(wrfnmm_pdtop)
    if(allocated(wrfnmm_pt))           deallocate(wrfnmm_pt)

    ! Deallocate memory for variables computed within routine

    if(allocated(enkf_virttemp))       deallocate(enkf_virttemp)
    if(allocated(enkf_pressure))       deallocate(enkf_pressure)
    if(allocated(enkf_spechumd))       deallocate(enkf_spechumd)

    !======================================================================
    end do backgroundloop ! loop over backgrounds to read in

    ! Return calculated values

    return

    !======================================================================

  end subroutine readgriddata_nmm

  !========================================================================

  ! writegriddata.f90: This subroutine will receive a netcdf file name
  ! and variable string and will subsequently return the respective
  ! variable interpolated to the native variable grid; all checks for
  ! grid staggering are contained within this subroutine

  !-------------------------------------------------------------------------

  subroutine writegriddata(nanal,vargrid)

    use netcdf, only: nf90_open,nf90_close
    use netcdf, only: nf90_write
    use netcdf, only: nf90_put_att
    use netcdf, only: nf90_global
    use constants
    use netcdf_mod, only: nc_check

    !----------------------------------------------------------------------

    ! Define variables passed to subroutine

    real(r_single),    dimension(npts,nvars*nlevs+1,nbackgrounds),               intent(in)    :: vargrid
    integer,                                                                     intent(in)    :: nanal                                                

    !----------------------------------------------------------------------

    ! Define variables computed within subroutine

    character(len=500)                                                                         :: filename
    character(len=3)                                                                           :: charnanal
    real,    dimension(:,:,:),                allocatable                            :: vargrid_native
    real,    dimension(:,:,:),                allocatable                            :: vargridin_native
    real,    dimension(:,:,:),                allocatable                            :: workgrid
    real                                                                             :: clip
    integer iyear,imonth,iday,ihour,dh1,ierr,iw3jdn
    integer                                                                                    :: xdim_native
    integer                                                                                    :: ydim_native
    integer                                                                                    :: zdim_native
    integer                                                                                    :: xdim_local
    integer                                                                                    :: ydim_local
    integer                                                                                    :: zdim_local

    !----------------------------------------------------------------------

    ! Define array dimension variables

    integer                                                                                    :: xdim
    integer                                                                                    :: ydim
    integer                                                                                    :: zdim

    !----------------------------------------------------------------------

    ! Define variables required by for extracting netcdf variable
    ! fields

    character(len=50)                                                                          :: attstr
    character(len=12)                                                                          :: varstagger,varstrname
    character(len=12)                                                                          :: varmemoryorder
    character(len=19)                                                                          :: DateStr
    character(len=24),parameter                                                                :: myname_ = 'gridio'

    !----------------------------------------------------------------------

    ! Define counting variables

    integer                                                                                    :: i, j, k, l, nb
    integer                                                                                    :: counth, countv

    !----------------------------------------------------------------------

    ! Initialize constants required by routine

    call init_constants(.true.)

    !----------------------------------------------------------------------

    ! Define all array dimensions

    xdim = dimensions%xdim
    ydim = dimensions%ydim
    zdim = dimensions%zdim

    if (nbackgrounds > 1) then
       write(6,*)'gridio/writegriddata: writing multiple backgrounds not yet supported'
       call stop2(23)
    endif

    backgroundloop: do nb=1,nbackgrounds

    ! Allocate memory for local variable

    allocate(workgrid(xdim,ydim,zdim))

    !----------------------------------------------------------------------

    ! End: Define all local variables required by routine

    !======================================================================

    ! Begin: Loop through each prognostic variable and determine the
    ! spatial array dimensions for each variable contained within
    ! file, define appropriate array dimensions, and allocate memory;
    ! update respective analysis (e.g., prognostic model) variables

    !----------------------------------------------------------------------

    ! Initialize counting variable

    countv = 1

    !----------------------------------------------------------------------

    ! First guess file should be copied to analysis file at scripting
    ! level; only variables updated by EnKF are changed

    write(charnanal,'(i3.3)') nanal
    filename = trim(adjustl(datapath))//trim(adjustl(anlfileprefixes(nb)))//"mem"//charnanal

    !----------------------------------------------------------------------

    ! Loop through all analysis variables to be updated

    do l = 1, nvars + 1

    !----------------------------------------------------------------------

       ! For WRF-ARW; analysis variables are defined on C-grid; the
       ! check for interpolation between mass and velocity points is
       ! done here

       if(arw) then

    !----------------------------------------------------------------------

          ! Define staggering attributes for variable grid
       
          attstr = 'stagger'
          call variableattribute_char(filename,gridvarstring(l),attstr,     &
               & varstagger)

    !----------------------------------------------------------------------

          ! If variable grid is staggered in X-direction, assign array
          ! dimensions appropriately

          if(varstagger(1:1) .eq. 'X') then

             ! Assign array dimensions appropriately

             xdim_native = xdim + 1
             ydim_native = ydim
             zdim_native = zdim

    !----------------------------------------------------------------------

             ! If variable grid is staggered in Y-direction, assign
             ! array dimensions appropriately

          else if(varstagger(1:1) .eq. 'Y') then

             ! Assign array dimensions appropriately

             xdim_native = xdim
             ydim_native = ydim + 1
             zdim_native = zdim

    !----------------------------------------------------------------------

             ! If variable grid is staggered in Z-direction, assign
             ! array dimensions appropriately

          else if(varstagger(1:1) .eq. 'Z') then

             ! Assign array dimensions appropriately

             xdim_native = xdim
             ydim_native = ydim
             zdim_native = zdim + 1

    !----------------------------------------------------------------------

             ! If variable grid is not staggered, assign array
             ! dimensions appropriately

          else 

             ! Assign array dimensions appropriately

             xdim_native = xdim
             ydim_native = ydim
             zdim_native = zdim

    !----------------------------------------------------------------------

          end if ! if(varstagger(1:1) .eq. 'X')

    !----------------------------------------------------------------------

       endif ! if(arw)

    !----------------------------------------------------------------------

       ! For WRF-NMM; analysis variables are defined on E-grid;
       ! although th grid may still be staggered, the array dimensions
       ! (along the horizontal planes) remain the same dimension,
       ! however just offset

       if(nmm) then

          ! Assign array dimensions appropriately

          xdim_native = xdim
          ydim_native = ydim
          zdim_native = zdim

       end if ! if(nmm)

    !----------------------------------------------------------------------

       ! Define memory attributes for variable grid; this is done for
       ! ARW only

       if(arw) then

          attstr = 'MemoryOrder'
          call variableattribute_char(filename,gridvarstring(l),attstr,     &
               & varmemoryorder)

       end if ! if(arw)

    !----------------------------------------------------------------------

       ! If variable is a 2-dimensional field, rescale variables
       ! appropriately

       if(gridvarstring(l) .eq. 'MU' .or. gridvarstring(l) .eq. 'PD') then

          ! Rescale grid dimension variables appropriately

          zdim_local = 1
          zdim_native = 1
       
       else

          ! Define local array dimension

          zdim_local = zdim

       end if ! if(gridvarstring(l) .eq. 'MU' .or. gridvarstring(l) .eq. 
              ! 'PD')

    !----------------------------------------------------------------------

       ! Define local variable dimensions

       xdim_local = xdim
       ydim_local = ydim

    !----------------------------------------------------------------------

       ! Allocate memory local arrays (first check whether they are
       ! already allocated)

       if (allocated(vargrid_native)) deallocate(vargrid_native)
       allocate(vargrid_native(xdim_native,ydim_native,zdim_native))
       if (allocated(vargridin_native)) deallocate(vargridin_native)
       allocate(vargridin_native(xdim_native,ydim_native,zdim_native))

    !----------------------------------------------------------------------
       
       ! Read in first-guess (i.e., analysis without current
       ! increments) and store in local array

       call readnetcdfdata(filename,vargridin_native,gridvarstring(l),      &
            & xdim_native,ydim_native,zdim_native)

    !----------------------------------------------------------------------

       ! Loop through vertical coordinate

       do k = 1, zdim_local

    !----------------------------------------------------------------------

          ! Initialize counting variable

          counth = 1

    !----------------------------------------------------------------------

          ! Loop through meridional horizontal coordinate
          
          do j = 1, ydim
             
             ! Loop through zonal horizontal coordinate

             do i = 1, xdim

    !----------------------------------------------------------------------

                ! Assign values to local array

                workgrid(i,j,k) = vargrid(counth,countv,nb)

                ! Update counting variable

                counth = counth + 1

    !----------------------------------------------------------------------

             end do ! do i = 1, xdim

          end do ! do j = 1, ydim

    !----------------------------------------------------------------------

          ! Update counting variable

          countv = countv + 1

    !----------------------------------------------------------------------

       end do ! k = 1, zdim_local

    !----------------------------------------------------------------------

       ! Interpolate increments to native grid (i.e., from A-grid to
       ! C-grid; if necessary); on input, workgrid is increments on
       ! unstaggered grid; on output vargrid_native is increments on
       ! model-native (i.e., staggered grid); vargridin_native is
       ! unmodified first guess on native staggered grid

       call dot2cross(xdim_local,ydim_local,zdim_local,xdim_native,          &
            ydim_native,zdim_native,workgrid,vargrid_native)

       ! Add first guess to increment to get analysis on native grid;
       ! this currently done only for ARW grids

       if(arw) then

          if (varstagger(1:1) .eq. 'Z') then ! if 'W' or 'PH' don't update surface

             vargridin_native(:,:,2:zdim_native) =                           &
                  & vargrid_native(:,:,2:zdim_native) +                      &
                  & vargridin_native(:,:,2:zdim_native)

          else

             vargridin_native = vargrid_native + vargridin_native

          endif ! if (varstagger(1:1) .eq. 'Z')

       endif ! if(arw)

       ! Clip all tracers (assume names start with 'Q')

       if (cliptracers .and. gridvarstring(l)(1:1) .eq. 'Q') then

          clip = tiny(vargridin_native(1,1,1))
          where (vargridin_native < clip) vargridin_native = clip

       end if ! if (cliptracers .and. gridvarstring(l)(1:1) .eq. 'Q')

    !----------------------------------------------------------------------

       if(nmm) then
          
          vargridin_native = vargrid_native + vargridin_native

       end if

       ! Write analysis variable.

       call writenetcdfdata(filename,vargridin_native,gridvarstring(l),       &
             xdim_native,ydim_native,zdim_native)

    end do ! do l = 1, nvars+1

    !----------------------------------------------------------------------

    ! Deallocate memory for local variable

    deallocate(workgrid)

    ! update NSTART_HOUR in NMM (HWRF) restart file.
    read(datestring(1:4),'(i4)') iyear
    read(datestring(5:6),'(i2)') imonth
    read(datestring(7:8),'(i2)') iday
    read(datestring(9:10),'(i2)') ihour
    if (nmm) then
       varstrname = 'NSTART_HOUR'
       vargrid_native(1,1,1) = ihour
       call writenetcdfdata(filename,vargrid_native,varstrname,1,1,1)
    end if
    !
    !  update START_DATE, SIMULATION_START_DATE, GMT, JULYR, JULDAY 
    !  global attributes.
    !
    write(DateStr,'(i4,"-",i2.2,"-",i2.2,"-",i2.2,"_",i2.2,":",i2.2)') iyear,imonth,iday,ihour,0,0

    call nc_check( nf90_open(trim(filename),nf90_write,dh1),&
        myname_,'open '//trim(filename) )
    call nc_check( nf90_put_att(dh1,nf90_global,'START_DATE',trim(DateStr)),&
        myname_,'put_att:  START_DATE '//trim(filename) )
    call nc_check( nf90_put_att(dh1,nf90_global,'SIMULATION_START_DATE',trim(DateStr)),&
        myname_,'put_att:  SIMULATION_START_DATE '//trim(filename) )
    call nc_check( nf90_put_att(dh1,nf90_global,'GMT',float(ihour)),&
        myname_,'put_att: GMT '//trim(filename) )
    call nc_check( nf90_put_att(dh1,nf90_global,'JULYR',iyear),&
        myname_,'put_att: JULYR'//trim(filename) )
    call nc_check( nf90_put_att(dh1,nf90_global,'JULDAY',iw3jdn(iyear,imonth,iday)-iw3jdn(iyear,1,1)+1),&
        myname_,'put_att: JULDAY'//trim(filename) )
    call nc_check( nf90_close(dh1),&
        myname_,'close: '//trim(filename) )

    !----------------------------------------------------------------------

    ! End: Loop through each prognostic variable and determine the
    ! spatial array dimensions for each variable contained within
    ! file, define appropriate array dimensions, and allocate memory;
    ! update respective analysis (e.g., prognostic model) variables

    !======================================================================
    end do backgroundloop ! loop over backgrounds to read in

    ! Return calculated values

    return

    !======================================================================

  end subroutine writegriddata

  !========================================================================

end module gridio
