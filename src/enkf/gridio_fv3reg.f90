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
  !   2011-11-30 Winterbottom - Initial version.
  !
  !   2019-01-?? Ting  -- 
  ! attributes:
  !   language:  f95
  !
  !$$$

  !=========================================================================
  ! Define associated modules
  use gridinfo, only:  npts
  use kinds,    only: r_double, r_kind, r_single, i_kind
  use mpisetup, only: nproc
  use netcdf_io
  use params,   only: nlevs, cliptracers, datapath, arw, nmm, datestring
  use params,   only: nx_res,ny_res,nlevs,ntiles
  use params,   only:  pseudo_rh
  use mpeu_util, only: getindex
  use read_fv3regional_restarts,only:read_fv3_restart_data1d,read_fv3_restart_data2d
  use read_fv3regional_restarts,only:read_fv3_restart_data3d,read_fv3_restart_data4d
  use netcdf_mod,only: nc_check

  implicit none

  !-------------------------------------------------------------------------
  ! Define all public subroutines within this module
  private
  public :: readgriddata
  public :: writegriddata

  !-------------------------------------------------------------------------

contains
  ! Generic WRF read routine, calls ARW-WRF or NMM-WRF
  subroutine readgriddata(nanal,vars3d,vars2d,n3d,n2d,levels,ndim,ntimes,fileprefixes,reducedgrid,vargrid,qsat)
   use constants, only:zero,one,half,fv, max_varname_length
   use gridinfo,only: ak,bk,eta1_ll,eta2_ll,ptop
   use netcdf, only: nf90_open,nf90_close,nf90_get_var,nf90_noerr
   use netcdf, only: nf90_inq_dimid,nf90_inq_varid
   use netcdf, only: nf90_nowrite,nf90_write,nf90_inquire,nf90_inquire_dimension
   implicit none
   integer, intent(in) :: nanal, n2d, n3d, ndim, ntimes
   character(len=max_varname_length), dimension(n2d), intent(in) :: vars2d
   character(len=max_varname_length), dimension(n3d), intent(in) :: vars3d
   integer, dimension(0:n3d), intent(in)        :: levels
   character(len=120), dimension(7), intent(in) :: fileprefixes
   logical, intent(in) :: reducedgrid

   real(r_single), dimension(npts,ndim,ntimes),  intent(out) :: vargrid
   real(r_double), dimension(npts,nlevs,ntimes), intent(out) :: qsat



    ! Define local variables 
    character(len=500) :: filename
    character(len=:),allocatable :: fv3filename
    character(len=7)   :: charnanal
    integer(i_kind) file_id,var_id,dim_id

    real(r_single), dimension(:,:,:), allocatable ::workvar3d,uworkvar3d,&
                        vworkvar3d,qvarworkvar3d,tvworkvar3d,tsenworkvar3d,&
                        workprsi,qworkvar3d
    real(r_double),dimension(:,:,:),allocatable:: qsatworkvar3d
    real(r_single), dimension(:,:),   allocatable ::pswork

    ! Define variables required for netcdf variable I/O
    character(len=12) :: varstrname
     
    character(len=4) char_nxres
    character(len=4) char_nyres
    character(len=4) char_tile
    character(len=24),parameter :: myname_ = 'fv3: getgridinfo'

    ! Define counting variables
    integer :: nlevsp1
    integer :: i,j, k,nn,ntile,nn_tile0, nb
    integer :: u_ind, v_ind, tv_ind,tsen_ind, q_ind, oz_ind
    integer :: ps_ind, sst_ind
    logical :: ice

    !======================================================================
    nlevsp1=nlevs+1
    u_ind   = getindex(vars3d, 'u')   !< indices in the state var arrays
    v_ind   = getindex(vars3d, 'v')   ! U and V (3D)
    tv_ind  = getindex(vars3d, 'tv')  ! Tv (3D)
    q_ind   = getindex(vars3d, 'q')   ! Q (3D)
    oz_ind  = getindex(vars3d, 'oz')  ! Oz (3D)
    tsen_ind = getindex(vars3d, 'tsen') !sensible T (3D)
!    prse_ind = getindex(vars3d, 'prse') ! pressure

    ps_ind  = getindex(vars2d, 'ps')  ! Ps (2D)
    sst_ind = getindex(vars2d, 'sst') ! SST (2D)

    ! Initialize all constants required by routine
    allocate(workvar3d(nx_res,ny_res,nlevs))
    allocate(qworkvar3d(nx_res,ny_res,nlevs))
    allocate(qsatworkvar3d(nx_res,ny_res,nlevs))

    if (ntimes > 1) then
       write(6,*)'gridio/readgriddata: reading multiple backgrounds not yet supported'
       call stop2(23)
    endif
    backgroundloop: do nb=1,ntimes

    ! Define character string for ensemble member file
    if (nanal > 0) then
      write(charnanal,'(a3, i3.3)') 'mem', nanal
    else
      charnanal = 'ensmean'
    endif

     do ntile=1,ntiles
      nn_tile0=(ntile-1)*nx_res*ny_res
      write(char_tile, '(i1)') ntile

      filename = trim(adjustl(datapath))//trim(adjustl(fileprefixes(nb)))//"_tile"//char_tile//trim(charnanal)

    !----------------------------------------------------------------------
    ! read u-component
      call nc_check( nf90_open(trim(adjustl(filename)),nf90_nowrite,file_id),&
        myname_,'open: '//trim(adjustl(filename)) )

    !----------------------------------------------------------------------
    ! Update u and v variables (same for NMM and ARW)
  
    if (u_ind > 0) then
    allocate(uworkvar3d(nx_res+1,ny_res,nlevs))
       varstrname = 'u'

         call read_fv3_restart_data3d(varstrname,filename,file_id,uworkvar3d)
      do k=1,nlevs
          nn = nn_tile0
        do j=1,ny_res
         do i=1,nx_res
            nn=nn+1
            vargrid(nn,levels(u_ind-1)+k,nb)=uworkvar3d(i,j,k) 
         enddo
        enddo
      enddo
       do k = levels(u_ind-1)+1, levels(u_ind)
          if (nproc .eq. 0)                                               &
             write(6,*) 'READFVregional : u ',                           &
                 & k, minval(vargrid(:,k,nb)), maxval(vargrid(:,k,nb))
       enddo

    deallocate(uworkvar3d)
    endif
    if (v_ind > 0) then
    allocate(vworkvar3d(nx_res,ny_res+1,nlevs))
       varstrname = 'v'
         call read_fv3_restart_data3d(varstrname,filename,file_id,vworkvar3d)
      do k=1,nlevs
          nn = nn_tile0
        do j=1,ny_res
         do i=1,nx_res
            nn=nn+1
            vargrid(nn,levels(v_ind-1)+k,nb)=vworkvar3d(i,j,k) 
         enddo
       enddo
      enddo
       do k = levels(v_ind-1)+1, levels(v_ind)
          if (nproc .eq. 0)                                               &
             write(6,*) 'READFVregional : v ',                           &
                 & k, minval(vargrid(:,k,nb)), maxval(vargrid(:,k,nb))
       enddo
    deallocate(vworkvar3d)

    endif

    if (tv_ind > 0.or.tsen_ind) then
      allocate(tsenworkvar3d(nx_res,ny_res,nlevs))
       varstrname = 't'
         call read_fv3_restart_data3d(varstrname,filename,file_id,tsenworkvar3d)
       if(tv_ind >0 .or. q_ind>0) then 
      allocate(qworkvar3d(nx_res,ny_res,nlevs))
       varstrname = 'sphum'
         call read_fv3_restart_data3d(varstrname,filename,file_id,qworkvar3d)


              if (q_ind > 0) then
                 varstrname = 'sphum'
                do k=1,nlevs
                    nn = nn_tile0
                 do j=1,ny_res
                   do i=1,nx_res
                      nn=nn+1
                      vargrid(nn,levels(q_ind-1)+k,nb)=qworkvar3d(i,j,k) 
                   enddo
                 enddo
                enddo
                 do k = levels(q_ind-1)+1, levels(q_ind)
                    if (nproc .eq. 0)                                               &
                       write(6,*) 'READFVregional : q ',                           &
                           & k, minval(vargrid(:,k,nb)), maxval(vargrid(:,k,nb))
                 enddo

              endif
         endif
        if(tv_ind > 0) then
           do k=1,nlevs
              do j=1,ny_res
              do i=1,nx_res
               workvar3d(i,j,k)=tsenworkvar3d(i,j,k)*(one+fv*qworkvar3d(i,j,k))
              enddo
              enddo
            enddo
        else! tsen_id >0
              workvar3d=tsenworkvar3d
        endif
           
           do k=1,nlevs
               nn = nn_tile0
              do j=1,ny_res
              do i=1,nx_res
                 nn=nn+1
                 vargrid(nn,levels(tv_ind-1)+k,nb)=workvar3d(i,j,k) 
              enddo
              enddo
            enddo
            do k = levels(tv_ind-1)+1, levels(tv_ind)
               if (nproc .eq. 0)                                               &
                  write(6,*) 'READFVregional : t ',                           &
                      & k, minval(vargrid(:,k,nb)), maxval(vargrid(:,k,nb))
            enddo

        endif
   if(allocated(tsenworkvar3d)) deallocate(tsenworkvar3d)
           

    
    if (oz_ind > 0) then
       varstrname = 'o3mr'
         call read_fv3_restart_data3d(varstrname,filename,file_id,workvar3d)
      do k=1,nlevs
          nn = nn_tile0
      do j=1,ny_res
         do i=1,nx_res
            nn=nn+1
            vargrid(nn,levels(oz_ind-1)+k,nb)=workvar3d(i,j,k) 
         enddo
      enddo
      enddo
       do k = levels(oz_ind-1)+1, levels(oz_ind)
          if (nproc .eq. 0)                                               &
             write(6,*) 'READFVregional : oz ',                           &
                 & k, minval(vargrid(:,k,nb)), maxval(vargrid(:,k,nb))
       enddo

    endif
   
      call nc_check( nf90_close(file_id),&
      myname_,'close '//trim(filename) )
    ! set SST to zero for now
    if (sst_ind > 0) then
       vargrid(:,levels(n3d)+sst_ind,nb) = zero
    endif


    !----------------------------------------------------------------------
    ! Allocate memory for variables computed within routine
 
    if (ps_ind > 0) then
       allocate(workprsi(nx_res,ny_res,nlevs))
       allocate(pswork(nx_res,ny_res))
       varstrname = 'u'
       fv3filename=trim(adjustl(filename))//"_dynvars"
       call nc_check( nf90_open(trim(adjustl(fv3filename)),nf90_nowrite,file_id),&
        myname_,'open: '//trim(adjustl(fv3filename)) )
      call read_fv3_restart_data3d('delp',filename,file_id,workvar3d)  !cltto think different files be used
      !print *,'min/max delp',ntile,minval(delp),maxval(delp)
      call nc_check( nf90_close(file_id),&
      myname_,'close '//trim(filename) )
      workprsi(:,:,nlevsp1+1)=eta1_ll(nlevsp1) !etal_ll is needed
      do i=nlevs,1,-1
        workprsi(:,:,i)=workvar3d(:,:,i)*0.01_r_kind+workprsi(:,:,i+1)
       enddo
 
      pswork(:,:)=workprsi(:,:,1)



          nn = nn_tile0
      do j=1,ny_res
         do i=1,nx_res
            nn=nn+1
            vargrid(nn,levels(ps_ind-1), nb) =pswork(i,j) 
         enddo
      enddo


      

      
      do k=1,nlevs
        do j=1,ny_res  
         do i=1,nx_res
           workvar3d(i,j,k)=(workprsi(i,j,k)+workprsi(i,j,k+1))*half
         enddo
        enddo
      enddo
       allocate(qsatworkvar3d(nx_res,ny_res,nlevs))
     ice=.true.  !tothink
    if (pseudo_rh) then
       call genqsat1(qworkvar3d,qsatworkvar3d,workvar3d,tvworkvar3d,ice,  &
                     nx_res*ny_res,nlevs)
    else
       qsatworkvar3d(:,:,:) = 1._r_double
    endif
      do k=1,nlevs
          nn = nn_tile0
      do j=1,ny_res
         do i=1,nx_res
            nn=nn+1
            qsat(nn,k,nb)=qsatworkvar3d(i,j,k) 
         enddo
      enddo
      enddo
            
          



   if(allocated(workprsi))     deallocate(workprsi)
   if(allocated(pswork))     deallocate(pswork)
   if(allocated(tvworkvar3d)) deallocate(tvworkvar3d)
   if(allocated(qworkvar3d)) deallocate(qworkvar3d)
   if(allocated(qsatworkvar3d)) deallocate(qsatworkvar3d)
    
     endif
    !======================================================================
    ! Deallocate memory 
    if(allocated(workvar3d))             deallocate(workvar3d)
     end do ! ntile loop

    end do backgroundloop ! loop over backgrounds to read in

    return

  end subroutine readgriddata

  !========================================================================
  ! readgriddata_nmm.f90: read WRF-NMM state or control vector
  !-------------------------------------------------------------------------


  !========================================================================
  ! writegriddata.f90: write WRF-ARW or WRF-NMM analysis
  !-------------------------------------------------------------------------

  subroutine writegriddata(nanal,vars3d,vars2d,n3d,n2d,levels,ndim,vargrid,no_inflate_flag)
    use constants, only: zero, one,fv,half
    use params, only: nbackgrounds, anlfileprefixes, fgfileprefixes
    use params,   only: nx_res,ny_res,nlevs,ntiles
    use netcdf, only: nf90_open,nf90_close,nf90_get_var,nf90_noerr
    use netcdf, only: nf90_inq_dimid,nf90_inq_varid
    use netcdf, only: nf90_write,nf90_write,nf90_inquire,nf90_inquire_dimension
    use write_fv3regional_restarts,only:write_fv3_restart_data1d,write_fv3_restart_data2d
    use write_fv3regional_restarts,only:write_fv3_restart_data3d,write_fv3_restart_data4d
    include 'netcdf.inc'      

    !----------------------------------------------------------------------
    ! Define variables passed to subroutine
    integer, intent(in)  :: nanal, n2d, n3d, ndim
    character(len=*), dimension(n2d), intent(in) :: vars2d
    character(len=*), dimension(n3d), intent(in) :: vars3d
    integer, dimension(0:n3d), intent(in) :: levels
    real(r_single), dimension(npts,ndim,nbackgrounds), intent(in) :: vargrid
    logical, intent(in) :: no_inflate_flag

    !----------------------------------------------------------------------
    ! Define variables computed within subroutine
    character(len=500)  :: filename
    character(len=:),allocatable :: fv3filename
    character(len=3)    :: charnanal

    !----------------------------------------------------------------------
    integer(i_kind) :: u_ind, v_ind, tv_ind, tsen_ind,q_ind, ps_ind,oz_ind
    integer(i_kind) :: w_ind, cw_ind, ph_ind

    integer(i_kind) file_id,var_id,dim_id
    real(r_single), dimension(:,:,:), allocatable ::workvar3d,workinc3d,uworkvar3d,&
                        vworkvar3d,qvarworkvar3d,tvworkvar3d,tsenworkvar3d,&
                        workprsi,qworkvar3d
    !----------------------------------------------------------------------
    ! Define variables required by for extracting netcdf variable
    ! fields
    character(len=19)  :: DateStr
    ! Define variables required for netcdf variable I/O
    character(len=12) :: varstrname
    character(len=4) char_nxres
    character(len=4) char_nyres
    character(len=4) char_tile
    character(len=:),allocatable:: varname
    character(len=24),parameter :: myname_ = 'fv3: getgridinfo'

    !----------------------------------------------------------------------
    ! Define counting variables
    integer :: i,j,k,nn,ntile,nn_tile0, nb



    real(r_single) :: ptop

    !----------------------------------------------------------------------

    u_ind   = getindex(vars3d, 'u')   !< indices in the state var arrays
    v_ind   = getindex(vars3d, 'v')   ! U and V (3D)
    tv_ind  = getindex(vars3d, 'tv')  ! Tv (3D)
    tsen_ind  = getindex(vars3d, 'tsen')  ! Tv (3D)
    q_ind   = getindex(vars3d, 'q')   ! Q (3D)
    cw_ind  = getindex(vars3d, 'cw')  ! CWM for WRF-NMM
    w_ind   = getindex(vars3d, 'w')   ! W for WRF-ARW
    ph_ind  = getindex(vars3d, 'ph')  ! PH for WRF-ARW

    ps_ind  = getindex(vars2d, 'ps')  ! Ps (2D)

    ! Initialize constants required by routine
!cltorg    call init_constants(.true.)

    !----------------------------------------------------------------------
    if (nbackgrounds > 1) then
       write(6,*)'gridio/writegriddata: writing multiple backgrounds not yet supported'
       call stop2(23)
    endif

    backgroundloop: do nb=1,nbackgrounds

    !----------------------------------------------------------------------
    ! First guess file should be copied to analysis file at scripting
    ! level; only variables updated by EnKF are changed
    write(charnanal,'(i3.3)') nanal
    filename = trim(adjustl(datapath))//trim(adjustl(anlfileprefixes(nb)))//"mem"//charnanal
     call nc_check( nf90_open(trim(adjustl(filename)),nf90_write,file_id),&
        myname_,'open: '//trim(adjustl(filename)) )

    !----------------------------------------------------------------------
    ! Update u and v variables (same for NMM and ARW)
    do ntile=1,ntiles
      nn_tile0=(ntile-1)*nx_res*ny_res
      write(char_tile, '(i1)') ntile

      filename = trim(adjustl(datapath))//trim(adjustl(anlfileprefixes(nb)))//"_tile"//char_tile//trim(charnanal)

    !----------------------------------------------------------------------
    ! read u-component
      call nc_check( nf90_open(trim(adjustl(filename)),nf90_write,file_id),&
        myname_,'open: '//trim(adjustl(filename)) )


    ! update CWM for WRF-NMM
    if (u_ind > 0) then
       varstrname = 'u'
         
       call read_fv3_restart_data3d(varstrname,filename,file_id,uworkvar3d)
      do k=1,nlevs
          nn = nn_tile0
      do j=1,ny_res
         do i=1,nx_res
            nn=nn+1
            workinc3d(i,j,k)=vargrid(nn,levels(u_ind-1)+k,nb) 
         enddo
      enddo
      enddo
      uworkvar3d(1:nx_res,:,:)=workvar3d+workinc3d
      uworkvar3d(nx_res+1,:,:)=uworkvar3d(nx_res,:,:)
       call write_fv3_restart_data3d(varstrname,filename,file_id,uworkvar3d)

    endif

    if (v_ind > 0) then
       varstrname = 'v'
         
       call read_fv3_restart_data3d(varstrname,filename,file_id,workvar3d)
      do k=1,nlevs
          nn = nn_tile0
      do j=1,ny_res
         do i=1,nx_res
            nn=nn+1
            workinc3d(i,j,k)=vargrid(nn,levels(v_ind-1)+k,nb) 
         enddo
      enddo
      enddo
      vworkvar3d(:,1:ny_res,:)=workvar3d+workinc3d
      vworkvar3d(:,ny_res+1,:)=vworkvar3d(:,ny_res,:)
       call write_fv3_restart_data3d(varstrname,filename,file_id,vworkvar3d)

    endif
    if (tv_ind > 0.or.tsen_ind>0 ) then
       varstrname = 't'
         
      if(tsen_ind>0) then
      do k=1,nlevs
          nn = nn_tile0
      do j=1,ny_res
         do i=1,nx_res
            nn=nn+1
            workinc3d(i,j,k)=vargrid(nn,levels(tsen_ind-1)+k,nb) 
         enddo
      enddo
      enddo
       call read_fv3_restart_data3d(varstrname,filename,file_id,workvar3d)
          workvar3d=workvar3d+workinc3d
       call write_fv3_restart_data3d(varstrname,filename,file_id,workvar3d)
     else  ! tv_ind >0  
      do k=1,nlevs
          nn = nn_tile0
      do j=1,ny_res
         do i=1,nx_res
            nn=nn+1
            workinc3d(i,j,k)=vargrid(nn,levels(tv_ind-1)+k,nb) 
         enddo
      enddo
      enddo

        call read_fv3_restart_data3d(varstrname,filename,file_id,tsenworkvar3d)
        call read_fv3_restart_data3d(varstrname,filename,file_id,qworkvar3d)
        tvworkvar3d=tsenworkvar3d*qworkvar3d
        tvworkvar3d=tvworkvar3d+workinc3d
       if(q_ind > 0) then
      do k=1,nlevs
          nn = nn_tile0
      do j=1,ny_res
         do i=1,nx_res
            nn=nn+1
            workinc3d(i,j,k)=vargrid(nn,levels(q_ind-1)+k,nb) 
         enddo
      enddo
      enddo
       qworkvar3d=qworkvar3d+workinc3d   
       endif
       tsenworkvar3d=tvworkvar3d/(one+fv*qworkvar3d(i,j,k))
       call write_fv3_restart_data3d(varstrname,filename,file_id,tsenworkvar3d)
       if(q_ind>0) then
       varname='sphum'
     
       call write_fv3_restart_data3d(varstrname,filename,file_id,qworkvar3d)
       endif
       
      
       
     endif

    endif
    if (oz_ind > 0) then
       varstrname = 'o3mr'
         
       call read_fv3_restart_data3d(varstrname,filename,file_id,workvar3d)
      do k=1,nlevs
          nn = nn_tile0
      do j=1,ny_res
         do i=1,nx_res
            nn=nn+1
            workinc3d(i,j,k)=vargrid(nn,levels(oz_ind-1)+k,nb) 
         enddo
      enddo
      enddo
      workvar3d=workvar3d+workinc3d
       call write_fv3_restart_data3d(varstrname,filename,file_id,workvar3d)

    endif


    !----------------------------------------------------------------------
    ! update time stamp is to be considered NSTART_HOUR in NMM (HWRF) restart file.
    !======================================================================
     end do ! tiles
    end do backgroundloop ! loop over backgrounds to read in

    ! Return calculated values
    return

    !======================================================================

  end subroutine writegriddata


end module gridio
