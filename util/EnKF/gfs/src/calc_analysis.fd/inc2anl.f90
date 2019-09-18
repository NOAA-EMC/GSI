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
  character(len=7) :: incvars_nemsio(nincv), incvars_netcdf(nincv)

  data incvars_nemsio / 'ugrd   ', 'vgrd   ', 'dpres  ', 'delz   ', 'o3mr   ',&
                        'tmp    ', 'spfh   ', 'clwmr  ', 'icmr   ', 'pres   '/
  data incvars_netcdf / 'u      ', 'v      ', 'delp   ', 'delz   ', 'o3mr   ',&
                        'T      ', 'sphum  ', 'liq_wat', 'icmr   ', 'pres   '/

contains
  subroutine gen_anl
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! subroutine gen_anl
  !            loop through fields, read in first guess, read in
  !            increment, add the two together, and write out
  !            the analysis to a new NEMSIO file
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    use vars_calc_analysis, only: fcstfile, anlfile, nlat, nlon, &
                                  nrec, recname, reclev, reclevtyp
    use nemsio_module
    use netcdf
    implicit none
    ! variables local to this subroutine
    integer :: iret, i, j, iincvar
    real, allocatable, dimension(:) :: work1
    logical :: use_increment

    allocate(work1(nlat*nlon))
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
          call add_psfc_increment
        else
          ! call generic subroutine for all other fields
          call add_increment(recname(i), reclevtyp(i), reclev(i), incvars_netcdf(iincvar))
        end if
      else
        ! otherwise just write out what is in the input to the output
        print *, 'Copying from Background ', recname(i), reclevtyp(i), reclev(i)
        call nemsio_readrecv(fcstfile, recname(i), reclevtyp(i), reclev(i), work1, iret=iret)
        if (iret /=0) write(6,*) 'Error with NEMSIO read', recname(i),reclevtyp(i), reclev(i)
        call nemsio_writerecv(anlfile, recname(i), reclevtyp(i), reclev(i), work1, iret=iret)
        if (iret /=0) write(6,*) 'Error with NEMSIO write', recname(i),reclevtyp(i), reclev(i)
      end if
    end do

    deallocate(work1)
  end subroutine gen_anl

  subroutine add_increment(recname, reclevtyp, reclev, incvar_nc)
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! subroutine add_increment
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
                                  
    use nemsio_module
    use netcdf
    implicit none
    ! input variables 
    character(10), intent(in) :: recname, reclevtyp
    character(7), intent(in) :: incvar_nc
    integer, intent(in) :: reclev
    ! local variables
    real, allocatable, dimension(:) :: work1
    real, allocatable, dimension(:,:) :: work2d_nc, work2d_bg
    integer :: ilev,j,jj,iret
    integer :: ncid, varid
    integer :: start(3), count(3)
    
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
    start = (/1, 1, ilev/)
    count = (/nlon, nlat, 1/)
    call nccheck(nf90_open(trim(incr_file), nf90_nowrite, ncid)) 
    call nccheck(nf90_inq_varid(ncid, trim(incvar_nc)//"_inc", varid))
    call nccheck(nf90_get_var(ncid, varid, work2d_nc, start=start, count=count))
    call nccheck(nf90_close(ncid)) 

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
    deallocate(work1, work2d_bg, work2d_nc)

  end subroutine add_increment

  subroutine add_psfc_increment
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! subroutine add_psfc_increment
  !            special case of getting surface pressure analysis from
  !            the integral of the sum of the delp increment and delp background 
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    use vars_calc_analysis, only: fcstfile, anlfile, nlat, nlon, nlev, &
                                  incr_file
                                  
    use nemsio_module
    use netcdf
    implicit none

    ! local variables
    real, allocatable, dimension(:) :: work1
    real, allocatable, dimension(:,:) :: work2d_delp, psfc
    real, allocatable, dimension(:,:,:) :: work3d_inc, work3d_delp
    integer :: ilev,iret,k
    integer :: ncid, varid
    integer :: start(3), count(3)

    ! loop through all levels and save the background delp in memory
    allocate(work1(nlat*nlon))
    allocate(work2d_delp(nlon,nlat),work3d_delp(nlev,nlon,nlat))
   
    do k=1,nlev 
      call nemsio_readrecv(fcstfile, 'dpres', 'mid layer', k, work1, iret=iret)
      if (iret /=0) write(6,*) 'Error with NEMSIO read dpres lev=',k
      work2d_delp = reshape(work1,(/nlon,nlat/))
      work3d_delp(k,:,:) = work2d_delp
    end do

    ! read in the netCDF increment for delp
    allocate(work3d_inc(nlev,nlon,nlat))
    do k=1,nlev
      ! get the level from the netCDF file (opposite of NEMSIO)
      ilev = (nlev+1)-k ! ilev = 64 when nlev=64 and k=1
      start = (/1, 1, ilev/)
      count = (/nlon, nlat, 1/)
      call nccheck(nf90_open(trim(incr_file), nf90_nowrite, ncid)) 
      call nccheck(nf90_inq_varid(ncid, "delp_inc", varid))
      call nccheck(nf90_get_var(ncid, varid, work3d_inc(k,:,:), start=start, count=count))
      call nccheck(nf90_close(ncid))
    end do

    ! compute psfc
    allocate(psfc(nlon,nlat))
    psfc(:,:) = 0
    ! loop through all layers, add delp and delp increment, then add to psfc
    do k=1,nlev
      psfc(:,:) = psfc(:,:) + work3d_delp(k,:,:) + (work3d_inc(k,:,:)*0.1)
    end do
    
    ! now write out new psfc to NEMSIO analysis file 
    work1 = reshape(psfc,(/size(work1)/))
    call nemsio_writerecv(anlfile, 'pres', 'sfc', 1, work1, iret=iret)
    if (iret /=0) write(6,*) 'Error with NEMSIO write sfc pressure'
    
    ! deallocate things
    deallocate(psfc, work3d_inc, work3d_delp, work2d_delp, work1)

  end subroutine add_psfc_increment

  subroutine nccheck(status)
    use netcdf
    implicit none
    ! check if calls to netCDF API are successful
    integer, intent (in   ) :: status
    if (status /= nf90_noerr) then
      print *, "calc_analysis netCDF error", trim(nf90_strerror(status))
      stop
    end if

  end subroutine nccheck

end module inc2anl
