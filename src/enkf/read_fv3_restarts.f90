    program read_fv3_restarts

! ifort -I${NETCDF}/include -O2 -traceback read_fv3_restarts.f90 kinds.o
! netcdf_mod.o -L/${NETCDF}/lib -lnetcdf -lnetcdff

! read data from FV3 restart files.

    use kinds, only: i_kind,r_single,r_kind
    use netcdf, only: nf90_open,nf90_close,nf90_get_var,nf90_noerr
    use netcdf, only: nf90_inq_dimid,nf90_inq_varid
    use netcdf, only: nf90_nowrite,nf90_inquire,nf90_inquire_dimension
    use netcdf_mod, only: nc_check

    implicit none
    character(len=120) datapath,filename,fv3fixpath
    character(len=24),parameter :: myname_ = 'read_fv3_restarts'
    character(len=4) char_res
    character(len=1) char_tile
    integer(i_kind):: file_id,var_id,dim_id
    integer(i_kind):: nlevsp1,ntile,ntiles,nx,ny,res,nlevs
    real(r_single), allocatable, dimension(:,:) :: ak,bk,lon_tile,lat_tile
    real(r_single), allocatable, dimension(:,:,:) :: ps,zs
    real(r_single), allocatable, dimension(:,:,:,:) :: &
      delp,u,v,temp,q,oz,cwmr,icmr
    real(r_single) ptop
    real(r_single) :: grav=9.806

    
    ! these need to be input from namelist
    ntiles = 6
    res = 192
    datapath='/scratch3/BMC/gsienkf/whitaker/C192C192_modloc/2016010606/mem001/INPUT'
    fv3fixpath='/scratch4/NCEPDEV/global/save/glopara/svn/fv3gfs/fix/fix_fv3_gmted2010'

!   read ak,bk from fv_core.res.nc
    filename = trim(adjustl(datapath))//'/fv_core.res.nc'
    call nc_check( nf90_open(trim(adjustl(filename)),nf90_nowrite,file_id),&
    myname_,'open: '//trim(adjustl(filename)) )
    call nc_check( nf90_inq_dimid(file_id,'xaxis_1',dim_id),&
        myname_,'inq_dimid xaxis_1 '//trim(filename) )
    call nc_check( nf90_inquire_dimension(file_id,dim_id,len=nlevsp1),&
          myname_,'inquire_dimension xaxis_1 '//trim(filename) )
    nlevs = nlevsp1-1
    print *,'nlevsp1 = ',nlevsp1
    allocate(ak(nlevsp1,1),bk(nlevsp1,1))

    call read_fv3_restart_data2d('ak',filename,file_id,ak)
    call read_fv3_restart_data2d('bk',filename,file_id,bk)
    ptop = ak(1,1)
    print *,'ak=',ak
    print *,'bk=',bk

!   read lats/lons from C###_oro_data.tile#.nc
! (this requires path to FV3 fix dir)
    write(char_res, '(i4)') res
    allocate(lat_tile(res,res),lon_tile(res,res))
    do ntile=1,ntiles
       write(char_tile, '(i1)') ntile
       filename=trim(adjustl(fv3fixpath))//'/C'//trim(adjustl(char_res))//'/C'//trim(adjustl(char_res))//'_oro_data.tile'//char_tile//'.nc'
       print *,trim(adjustl(filename))
       call nc_check( nf90_open(trim(adjustl(filename)),nf90_nowrite,file_id),&
       myname_,'open: '//trim(adjustl(filename)) )

       call read_fv3_restart_data2d('geolon',filename,file_id,lon_tile)
       print *,'min/max lon_tile',minval(lon_tile),maxval(lon_tile)

       call read_fv3_restart_data2d('geolat',filename,file_id,lat_tile)
       print *,'min/max lat_tile',minval(lat_tile),maxval(lat_tile)

       call nc_check( nf90_close(file_id),&
       myname_,'close '//trim(filename) )
    enddo

!   read data from fv_core
    allocate(delp(res,res,nlevs,1),u(res,res,nlevs,1),v(res,res,nlevs,1),temp(res,res,nlevs,1))
    allocate(ps(res,res,1),zs(res,res,1))
    do ntile=1,ntiles

       write(char_tile, '(i1)') ntile
       filename = trim(adjustl(datapath))//'/fv_core.res.tile'//char_tile//'.nc'
       print *,trim(adjustl(filename))

       call nc_check( nf90_open(trim(adjustl(filename)),nf90_nowrite,file_id),&
       myname_,'open: '//trim(adjustl(filename)) )

       call read_fv3_restart_data4d('delp',filename,file_id,delp)
       print *,'min/max delp',minval(delp),maxval(delp)

       call read_fv3_restart_data4d('u',filename,file_id,u)
       print *,'min/max u',minval(u),maxval(u)

       call read_fv3_restart_data4d('v',filename,file_id,v)
       print *,'min/max v',minval(v),maxval(v)

       call read_fv3_restart_data4d('T',filename,file_id,temp)
       print *,'min/max T',minval(temp),maxval(temp)

       call read_fv3_restart_data3d('phis',filename,file_id,zs)
       zs = zs/grav
       print *,'min/max zs',minval(zs),maxval(zs)
       ps = sum(delp,3) + ptop
       print *,'min/max ps',minval(ps),maxval(ps)

       call nc_check( nf90_close(file_id),&
       myname_,'close '//trim(filename) )

    enddo

!   read data from fv_tracer
    allocate(q(res,res,nlevs,1),oz(res,res,nlevs,1),cwmr(res,res,nlevs,1),icmr(res,res,nlevs,1))
    do ntile=1,ntiles

       write(char_tile, '(i1)') ntile
       filename = trim(adjustl(datapath))//'/fv_tracer.res.tile'//char_tile//'.nc'
       print *,trim(adjustl(filename))

       call nc_check( nf90_open(trim(adjustl(filename)),nf90_nowrite,file_id),&
       myname_,'open: '//trim(adjustl(filename)) )

       call read_fv3_restart_data4d('sphum',filename,file_id,q)
       print *,'min/max spfhum',minval(q),maxval(q)

       call read_fv3_restart_data4d('o3mr',filename,file_id,oz)
       print *,'min/max o3mr',minval(oz),maxval(oz)

       call read_fv3_restart_data4d('liq_wat',filename,file_id,cwmr)
       print *,'min/max cwmr',minval(cwmr),maxval(cwmr)

       call read_fv3_restart_data4d('ice_wat',filename,file_id,icmr)
       print *,'min/max icmr',minval(icmr),maxval(icmr)

       call nc_check( nf90_close(file_id),&
       myname_,'close '//trim(filename) )

    enddo

    deallocate(ak,bk,lon_tile,lat_tile,delp,u,v,temp,zs,ps,q,oz,cwmr,icmr) 

    contains

    subroutine read_fv3_restart_data1d(varname,filename,file_id,data_arr)
       real(r_single), intent(inout), dimension(:) :: data_arr
       character(len=24),parameter :: myname_ = 'read_fv3_restart_data1d'
       include "read_fv3_restart_data.f90"
    end subroutine read_fv3_restart_data1d

    subroutine read_fv3_restart_data2d(varname,filename,file_id,data_arr)
       real(r_single), intent(inout), dimension(:,:) :: data_arr
       character(len=24),parameter :: myname_ = 'read_fv3_restart_data2d'
       include "read_fv3_restart_data.f90"
    end subroutine read_fv3_restart_data2d

    subroutine read_fv3_restart_data3d(varname,filename,file_id,data_arr)
       real(r_single), intent(inout), dimension(:,:,:) :: data_arr
       character(len=24),parameter :: myname_ = 'read_fv3_restart_data3d'
       include "read_fv3_restart_data.f90"
    end subroutine read_fv3_restart_data3d

    subroutine read_fv3_restart_data4d(varname,filename,file_id,data_arr)
       real(r_single), intent(inout), dimension(:,:,:,:) :: data_arr
       character(len=24),parameter :: myname_ = 'read_fv3_restart_data4d'
       include "read_fv3_restart_data.f90"
    end subroutine read_fv3_restart_data4d

    end program read_fv3_restarts
