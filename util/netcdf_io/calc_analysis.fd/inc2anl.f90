!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!! module inc2anl 
!!        contains subroutines for calculating analysis fields
!!        for a given input background and increment 
!! Original: 2019-09-18   martin   - original module
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
module inc2anl 
  implicit none

  private

  public :: gen_anl

  integer, parameter :: nincv=10
  character(len=7) :: incvars_nemsio(nincv), incvars_netcdf(nincv), incvars_ncio(nincv)
  integer, parameter :: nnciov=19
  character(len=7) :: iovars_netcdf(nnciov)

  data incvars_nemsio / 'ugrd   ', 'vgrd   ', 'dpres  ', 'delz   ', 'o3mr   ',&
                        'tmp    ', 'spfh   ', 'clwmr  ', 'icmr   ', 'pres   '/
  data incvars_netcdf / 'u      ', 'v      ', 'delp   ', 'delz   ', 'o3mr   ',&
                        'T      ', 'sphum  ', 'liq_wat', 'icmr   ', 'pres   '/
  data incvars_ncio / 'ugrd   ', 'vgrd   ', 'dpres  ', 'delz   ', 'o3mr   ',&
                        'tmp    ', 'spfh   ', 'clwmr  ', 'icmr   ', 'pressfc'/
  data iovars_netcdf /  'grid_xt', 'grid_yt', 'pfull  ', 'phalf  ', 'clwmr  ',&
                        'delz   ', 'dpres  ', 'dzdt   ', 'grle   ', 'hgtsfc ',&
                        'icmr   ', 'o3mr   ', 'pressfc', 'rwmr   ', 'snmr   ',&
                        'spfh   ', 'tmp    ', 'ugrd   ', 'vgrd   '/

contains
  subroutine gen_anl
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! subroutine gen_anl
  !            loop through fields, read in first guess, read in
  !            increment, add the two together, and write out
  !            the analysis to a new file
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    use vars_calc_analysis, only: fcstfile, anlfile, nlat, nlon, &
                                  nrec, recname, reclev, reclevtyp, &
                                  use_nemsio,fcstncfile,anlncfile
    use nemsio_module, only: nemsio_readrecv, nemsio_writerecv
    use module_fv3gfs_ncio, only: read_vardata, write_vardata 
    implicit none
    ! variables local to this subroutine
    integer :: iret, i, j, iincvar
    real, allocatable, dimension(:) :: work1d
    real, allocatable, dimension(:,:) :: work2d
    real, allocatable, dimension(:,:,:) :: work3d
    logical :: use_increment

    if (use_nemsio) then ! NEMSIO case
       allocate(work1d(nlat*nlon))
       ! big loop through every 'record' in the input NEMSIO file
       do i=1,nrec
         use_increment = .false.
         iincvar = -999
         ! determine if we are computing the increment for this field
         do j=1,nincv
           if (trim(recname(i)) == trim(incvars_nemsio(j))) then
             use_increment = .true.
             iincvar = j
           end if
         end do
         if (use_increment) then
           print *, 'Adding Increment ', recname(i), reclevtyp(i), reclev(i)
           if (trim(recname(i)) == 'pres') then
             ! special case for surface pressure
             call add_psfc_increment_nems
           else
             ! call generic subroutine for all other fields
             call add_increment_nems(recname(i), reclevtyp(i), reclev(i), incvars_netcdf(iincvar))
           end if
         else
           ! otherwise just write out what is in the input to the output
           print *, 'Copying from Background ', recname(i), reclevtyp(i), reclev(i)
           call nemsio_readrecv(fcstfile, recname(i), reclevtyp(i), reclev(i), work1d, iret=iret)
           if (iret /=0) write(6,*) 'Error with NEMSIO read', recname(i),reclevtyp(i), reclev(i)
           call nemsio_writerecv(anlfile, recname(i), reclevtyp(i), reclev(i), work1d, iret=iret)
           if (iret /=0) write(6,*) 'Error with NEMSIO write', recname(i),reclevtyp(i), reclev(i)
         end if
       end do

       deallocate(work1d)
    else ! netCDF case
       do i=1,nnciov
         use_increment = .false.
         iincvar = -999
         ! determine if we are computing the increment for this field
         do j=1,nincv
           if (iovars_netcdf(i) == incvars_ncio(j)) then
             use_increment = .true.
             iincvar = j
           end if
         end do
         if (use_increment) then
           print *, 'Adding Increment to ', iovars_netcdf(i), incvars_netcdf(iincvar)
           if (iovars_netcdf(i) == 'pressfc') then
             ! special case for surface pressure
             call add_psfc_increment_netcdf
           else
             ! call generic subroutine for all other fields
             call add_increment_netcdf(iovars_netcdf(i), incvars_netcdf(iincvar))
           end if
         else
           ! otherwise just write out what is in the input to the output
           print *, 'Copying from Background ', iovars_netcdf(i) 
           select case (iovars_netcdf(i))
             case ('grid_xt')
               call read_vardata(fcstncfile, 'grid_xt', work1d)
               call write_vardata(anlncfile, 'grid_xt', work1d)
             case ('grid_yt')
               call read_vardata(fcstncfile, 'grid_yt', work1d)
               call write_vardata(anlncfile, 'grid_yt', work1d)
             case ('lat')
               call read_vardata(fcstncfile, 'lat', work2d)
               call write_vardata(anlncfile, 'lat', work2d)
             case ('lon')
               call read_vardata(fcstncfile, 'lon', work2d)
               call write_vardata(anlncfile, 'lon', work2d)
             case ('pfull  ')
               call read_vardata(fcstncfile, 'pfull', work1d)
               call write_vardata(anlncfile, 'pfull', work1d)
             case ('phalf  ')
               call read_vardata(fcstncfile, 'phalf', work1d)
               call write_vardata(anlncfile, 'phalf', work1d)
             case default
               call read_vardata(fcstncfile, trim(iovars_netcdf(i)), work3d)
               call write_vardata(anlncfile, trim(iovars_netcdf(i)), work3d)
           end select 
         end if
      end do
    end if
  end subroutine gen_anl

  subroutine add_increment_nems(recname, reclevtyp, reclev, incvar_nc)
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! subroutine add_increment_nems
  !            generic subroutine for adding increment to background
  !            and writing out to analysis 
  !  args:
  !       recname   - input string of NEMSIO record name
  !       reclevtyp - input string of NEMSIO record level type
  !       reclev    - input integer of NEMSIO record level
  !       incvar_nc - input string of name of variable in netCDF file
  !                   (without _inc suffix added)
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    use vars_calc_analysis, only: fcstfile, anlfile, nlat, nlon, nlev,&
                                  incr_file
    use module_fv3gfs_ncio, only: Dataset, read_vardata, &
                                  open_dataset, close_dataset
                                  
    use nemsio_module
    implicit none
    ! input variables 
    character(10), intent(in) :: recname, reclevtyp
    character(7), intent(in) :: incvar_nc
    integer, intent(in) :: reclev
    ! local variables
    real, allocatable, dimension(:) :: work1
    real, allocatable, dimension(:,:) :: work2d_nc, work2d_bg
    real, allocatable, dimension(:,:,:) :: work3d_inc
    integer :: ilev,j,jj,iret
    type(Dataset) :: incncfile
    
    ! read in the first guess for this field/level
    allocate(work1(nlat*nlon))
    allocate(work2d_bg(nlon,nlat))
    call nemsio_readrecv(fcstfile, recname, reclevtyp, reclev, work1, iret=iret)
    if (iret /=0) write(6,*) 'Error with NEMSIO read', recname,reclevtyp, reclev
    work2d_bg = reshape(work1,(/nlon,nlat/))

    ! get the level from the netCDF file (opposite of NEMSIO)
    ilev = (nlev+1)-reclev ! ilev = 64 when nlev=64 and reclev=1
    ! read in the increment as a 2D array
    allocate(work2d_nc(nlon,nlat))
    incncfile = open_dataset(incr_file)
    call read_vardata(incncfile, trim(incvar_nc)//"_inc", work3d_inc)
    work2d_nc(:,:) = work3d_inc(:,:,ilev)

    ! add increment to background
    do j=1,nlat
      jj=nlat+1-j ! increment is S->N, NEMSIO is N->S
      work2d_bg(:,j) = work2d_bg(:,j) + work2d_nc(:,jj)
    end do

    ! write out analysis for this field
    work1 = reshape(work2d_bg,(/size(work1)/))
    call nemsio_writerecv(anlfile, recname, reclevtyp, reclev, work1, iret=iret)
    if (iret /=0) write(6,*) 'Error with NEMSIO write', recname, reclevtyp, reclev

    ! cleanup / deallocate
    deallocate(work1, work2d_bg, work2d_nc, work3d_inc)
    call close_dataset(incncfile)

  end subroutine add_increment_nems

  subroutine add_psfc_increment_nems
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! subroutine add_psfc_increment_nems
  !            special case of getting surface pressure analysis from
  !            bk5 and delp increment to get sfc pressure increment 
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    use vars_calc_analysis, only: fcstfile, anlfile, nlat, nlon, nlev, &
                                  incr_file
    use module_fv3gfs_ncio, only: Dataset, open_dataset, close_dataset,&
                                  read_vardata 
                                  
    use nemsio_module
    implicit none

    ! local variables
    real, allocatable, dimension(:,:,:) :: nems_vcoord
    real, allocatable, dimension(:,:) :: vcoord
    integer :: nvcoord
    real, allocatable, dimension(:) :: bk5, work1
    real, allocatable, dimension(:,:) :: ps_inc, work2d
    real, allocatable, dimension(:,:,:) :: work3d_inc
    integer :: iret,j,jj,k
    type(Dataset) :: incncfile


    ! get vertical coordinate info from NEMSIO file
    allocate(nems_vcoord(nlev+1,3,2))
    call nemsio_getfilehead(fcstfile, iret=iret, vcoord=nems_vcoord)
    if ( iret /= 0) then
      write(6,*) "Error reading NEMS vcoord, stopping..."
      stop
    end if

    nvcoord = 3
    if (maxval(nems_vcoord(:,3,1)) ==0 .and. &
        minval(nems_vcoord(:,3,1)) ==0 ) then
       nvcoord=2
       if (maxval(nems_vcoord(:,2,1)) ==0 .and. &
           minval(nems_vcoord(:,2,1)) ==0 ) then
          nvcoord=1
       end if
    end if
    allocate(vcoord(nlev+1,nvcoord))
    vcoord(:,1:nvcoord) = nems_vcoord(:,1:nvcoord,1)

    ! get bk5 from NEMSIO file
    allocate(bk5(nlev+1))
    do k=1,nlev+1
      bk5(k) = 0
    end do

    if (nvcoord == 1) then
      do k=1,nlev+1
        bk5(k) = vcoord(k,1)
      end do
    else if (nvcoord == 2) then
      do k = 1,nlev+1
        bk5(k) = vcoord(k,2)
      end do
    else if (nvcoord == 3) then
      do k = 1,nlev+1
        bk5(k) = vcoord(k,2)
      end do
    else
      write(6,*) "Error reading bk5 variable, stopping..."
      stop
    end if

    ! read in delp increment to get ps increment
    ! read in the netCDF increment for delp
    allocate(work3d_inc(nlev,nlon,nlat))
    incncfile = open_dataset(incr_file)
    call read_vardata(incncfile, 'delp_inc', work3d_inc)

    ! get ps increment from delp increment and bk
    allocate(ps_inc(nlon,nlat))
    ps_inc(:,:) = work3d_inc(nlev,:,:) / (bk5(1) - bk5(2))

    ! read in psfc background
    allocate(work1(nlon*nlat))
    allocate(work2d(nlon,nlat))
    call nemsio_readrecv(fcstfile,'pres','sfc',1,work1,iret=iret)
    if (iret /=0) write(6,*) 'Error with NEMSIO read surface pressure'
    work2d = reshape(work1,(/nlon,nlat/))
    ! add increment to background
    do j=1,nlat
      jj=nlat+1-j ! increment is S->N, NEMSIO is N->S
      work2d(:,j) = work2d(:,j) + ps_inc(:,jj)
    end do

    ! now write out new psfc to NEMSIO analysis file 
    work1 = reshape(work2d,(/size(work1)/))
    call nemsio_writerecv(anlfile, 'pres', 'sfc', 1, work1, iret=iret)
    if (iret /=0) write(6,*) 'Error with NEMSIO write sfc pressure'
    
    ! deallocate things
    deallocate(work3d_inc, work2d, bk5, ps_inc,  work1)
    call close_dataset(incncfile)

  end subroutine add_psfc_increment_nems

  subroutine add_increment_netcdf(fcstvar, incvar)
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! subroutine add_increment_netcdf
  !            generic subroutine for adding increment to background
  !            and writing out to analysis 
  !  args:
  !       fcstvar - input string of netCDF fcst/anal var name
  !       incvar  - input string of netCDF increment var name
  !                 (without _inc suffix added)
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    use vars_calc_analysis, only: fcstncfile, anlncfile, incr_file, nlat
    use module_fv3gfs_ncio, only: Dataset, read_vardata, write_vardata, &
                                  open_dataset, close_dataset
    implicit none
    ! input vars
    character(7), intent(in) :: fcstvar, incvar
    ! local variables
    real, allocatable, dimension(:,:,:) :: work3d_inc, work3d_bg
    integer :: j,jj
    type(Dataset) :: incncfile
    ! get first guess
    call read_vardata(fcstncfile, fcstvar, work3d_bg)
    ! get increment
    incncfile = open_dataset(incr_file)
    call read_vardata(incncfile, trim(incvar)//"_inc", work3d_inc)
    ! add increment to background
    do j=1,nlat
       jj=nlat+1-j ! increment is S->N, history files are N->S
       work3d_bg(:,j,:) = work3d_bg(:,j,:) + work3d_inc(:,jj,:)
    end do
    ! write out analysis to file
    call write_vardata(anlncfile, fcstvar, work3d_bg)
    ! clean up and close
    call close_dataset(incncfile)
  
  end subroutine add_increment_netcdf

  subroutine add_psfc_increment_netcdf
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! subroutine add_psfc_increment_netcdf
  !            special case of getting surface pressure analysis from
  !            bk5 and delp increment to get sfc pressure increment 
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    use vars_calc_analysis, only: fcstncfile, anlncfile, nlat, nlon, incr_file
    use module_fv3gfs_ncio, only: Dataset, open_dataset, close_dataset,&
                                  read_vardata, write_vardata, read_attribute
    implicit none
    ! local variables
    real, allocatable, dimension(:,:,:) :: work3d_inc
    real, allocatable, dimension(:,:) :: ps_inc, work2d
    real, allocatable, dimension(:) :: bk5
    integer j, jj
    type(Dataset) :: incncfile

    ! get bk5 from attributes
    call read_attribute(fcstncfile, 'bk', bk5) 
    ! read in delp increment to get ps increment
    incncfile = open_dataset(incr_file)
    call read_vardata(incncfile, 'delp_inc', work3d_inc)
    ! get ps increment from delp increment and bk
    allocate(ps_inc(nlon,nlat))
    ps_inc(:,:) = work3d_inc(:,:,1) / (bk5(1) - bk5(2))
    ! read in psfc background
    call read_vardata(fcstncfile, 'pressfc', work2d)
    ! add increment to background
    do j=1,nlat
      jj=nlat+1-j ! increment is S->N, history file is N->S
      work2d(:,j) = work2d(:,j) + ps_inc(:,jj)
    end do
    ! write out to file
    call write_vardata(anlncfile, 'pressfc', work2d)
    ! deallocate and close
    call close_dataset(incncfile)
    deallocate(work2d,work3d_inc,ps_inc,bk5)

  end subroutine add_psfc_increment_netcdf

end module inc2anl
