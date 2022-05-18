 module gridio_efsoi
!$$$  module documentation block
!
! module: gridio_efsoi               subroutines for reading and writing
!                                    ensemble members files using
!                                    EnKF internal format.  A separate
!                                    program must be run before and
!                                    after the EnKF analysis to convert
!                                    to and from the native model format.
!
! prgmmr: whitaker         org: esrl/psd               date: 2009-02-23
! prgmmr: groff            org: emc                    date: 2018-05-24
! prgmmr: eichmann, lin    org: emc                    date: 2021-02-04
!
! abstract: I/O for ensemble member files.
! 
! Public Functions:
!  readgriddata_efsoi
!
! this version reads nemsio files for EFSOI applications.
!
! Public Variables: None
!
! Modules Used: constants (must be pre-initialized).
!
! program history log:
!   2009-02-23  Initial version.
!   2015-06-29  Add ability to read/write multiple time levels
!   2016-05-02  shlyaeva: Modification for reading state vector from table
!   2016-04-20  Modify to handle the updated nemsio sig file (P, DP, DPDT
!               removed)
!   2016-11-29  shlyaeva: Add reading/calculating tsen, qi, ql. Pass filenames and
!               hours to read routine to read separately state and control files. 
!               Pass levels and dimenstions to read/write routines (dealing with
!               prse: nlevs + 1 levels). Pass "reducedgrid" parameter.
!   2017-06-14  Adding functionality to optionally write non-inflated ensembles,  
!               a required input for EFSO calculations
!   2018-05-24  Pruning available EnKF nemsio read functionality for EFSOI
!               application.  Add additional routines to compute EFSOI relevant
!               quantities from files read.
!   2019-03-13  Add precipitation components
!   2019-07-10  Add convective clouds
!   2021-02-04  Added functionality for FV3 GFS, netcdf file handling
!
! attributes:
!   language: f95
!
!$$$
 use constants, only: zero,one,cp,fv,rd,tiny_r_kind,max_varname_length,t0c,r0_05,constants_initialized, &
                      tref,pref,hvap				  
					  
 use params, only: nlons,nlats,nlevs,use_gfs_nemsio,pseudo_rh, &
                   cliptracers,datapath,imp_physics,use_gfs_ncio,cnvw_option, &
                   nanals, &
                   wmoist,andataname, &
                   nvars,forecast_impact,read_member_forecasts, &
                   read_ensmean_forecast, read_analysis_mean, &
                   read_member_forecasts, read_verification, &
                   read_member_analyses
 use kinds, only: i_kind,r_double,r_kind,r_single
 use gridinfo, only: ntrunc,npts ! getgridinfo be called first!

 use specmod, only: sptezv_s, sptez_s, init_spec_vars, ndimspec => nc, &
                    isinitialized, &
					gaulats

 use reducedgrid_mod, only: regtoreduced, reducedtoreg, &
                            lonsperlat, nlonsfull

 use mpisetup, only: nproc, numproc
 	
 use mpimod, only: mpi_comm_world, mpi_sum, mpi_real4, mpi_real8, mpi_rtype 						
 
 use mpeu_util, only: getindex
 use nemsio_module
 use loadbal_efsoi, only: numptsperproc, indxproc, npts_max

 implicit none
 private
 public :: readgriddata_efsoi
 public :: get_weight, destroy_weight, divide_weight
 real(r_kind), allocatable, dimension(:,:), save :: weight
 real(r_kind), allocatable, dimension(:), save :: grweight
 integer(i_kind),public :: ncdim
 contains

 ! =====================================================================
 ! =====================================================================
 ! =====================================================================
 subroutine get_weight()

  use sigio_module, only: sigio_head, sigio_data, sigio_sclose, sigio_sropen, &
                          sigio_srohdc, sigio_sclose, sigio_aldata, sigio_axdata
  use nemsio_module, only: nemsio_gfile,nemsio_open,nemsio_close,&
                           nemsio_getfilehead,nemsio_getheadvar,nemsio_realkind,nemsio_charkind,&
                           nemsio_readrecv,nemsio_init,nemsio_setheadvar,nemsio_writerecv
						   
  use module_ncio, only: Dataset, Variable, Dimension, open_dataset,&
                         quantize_data, read_attribute, close_dataset, get_dim, read_vardata
								
  implicit none 
 
  character(len=500) :: filename 
 
  real(r_kind), dimension(npts,nlevs+1) :: pressi 
  real(r_single), dimension(npts) :: tmpgrd 
  type(nemsio_gfile) :: gfile
  real(nemsio_realkind), dimension(nlons*nlats) :: nems_wrk
  real(r_kind), dimension(nlons*nlats) :: psfc
  real(r_single),allocatable,dimension(:,:,:)   :: nems_vcoord 
  real(r_kind), allocatable, dimension(:) :: ak,bk 
  real(r_kind) :: sumcoslat 
 
  integer(i_kind) :: nlevsin,nlonsin,nlatsin,idvc 
  integer(i_kind) :: i,j,k,iret!iunitsig,iret 
 
  ! for netcdf handling 
  type(Dataset) :: dset 
  type(Dimension) :: londim,latdim,levdim 
  integer(i_kind) :: iunitsig
  type(sigio_head)   :: sighead
  type(sigio_data)   :: sigdata
  real(r_kind), allocatable, dimension(:)       :: psg
  real(r_kind), dimension(ndimspec)             :: vrtspec
  real(r_single), allocatable, dimension(:,:)   :: values_2d  
  integer(i_kind) :: ierr  
  
  

  iunitsig = 77

  ! ============================================================
  ! Read analysis data 
  ! ==================================
  ! update ncio
  !if (nproc .eq. 0) then

  filename = trim(adjustl(datapath))//trim(adjustl(andataname))
  if (nproc == 0) print *,'reading analysis file: ',filename
  ! --- nemsio data -------------------------------------------------		
  if (use_gfs_nemsio) then
     call nemsio_init(iret=iret)
     if(iret/=0) then
        write(6,*)'gridio/readgriddata_efsoi: GFS: problem with nemsio_init,iret=',iret
        call stop2(23)
     end if
  
     call nemsio_open(gfile,filename,'READ',iret=iret)
  
     if (iret/=0) then
        write(6,*)'gridio/readgriddata_efsoi: GFS: problem with nemsio_open,iret=',iret
        call stop2(23)
     endif
  
     call nemsio_getfilehead(gfile,iret=iret, dimx=nlonsin, dimy=nlatsin,&
                             dimz=nlevsin,idvc=idvc)
     if (nlons /= nlonsin .or. nlats /= nlatsin .or. nlevs /= nlevsin) then
        print *,'incorrect dims in nemsio file'
        print *,'expected',nlons,nlats,nlevs
        print *,'got',nlonsin,nlatsin,nlevsin
        call stop2(23)
     end if
	 
	
  ! --- NetCDF data -------------------------------------------------	
  else if (use_gfs_ncio) then

	  
     dset = open_dataset(filename)
	 
     londim = get_dim(dset,'grid_xt'); nlonsin = londim%len
     latdim = get_dim(dset,'grid_yt'); nlatsin = latdim%len
     levdim = get_dim(dset,'pfull');   nlevsin = levdim%len
     idvc=2
	 
 
  ! --- Other data -------------------------------------------------	
  else
     call sigio_srohdc(iunitsig,trim(filename), &
                       sighead,sigdata,iret)
     if (iret /= 0) then
        print *,'error reading file in gridio ',trim(filename)
        call stop2(23)
     end if
  endif
  
  !endif
  ! ============================================================ 
	 
  allocate(ak(nlevs+1))
  allocate(bk(nlevs+1))
  allocate(psg(nlons*nlats))  
  allocate(weight(npts,nlevs)) 
  allocate(grweight(npts)) 
  
      
  if (.not. constants_initialized) then
     print *,'constants not initialized (with init_constants, init_constants_derived)'
     call stop2(23)
  end if

  ! calculate weight on the grid point
  sumcoslat = zero
  
  ! if(reducedgrid) then
     k=0
     do i=1,nlats 
        do j=1,lonsperlat(i) 
           k=k+1 
           grweight(k) = cos(gaulats(i)) * real(nlonsfull,r_kind) & 
                / real(lonsperlat(i),r_kind) 
           sumcoslat = sumcoslat + grweight(k) 
        end do 
     end do 
	 
  ! else 
  !    do i=1,nlons*nlats 
  !       grweight(i) = cos(latsgrd(i)) 
  !       sumcoslat = sumcoslat + grweight(i) 
  !    end do 
  ! end if 
  
  sumcoslat = 1.0_r_kind / sumcoslat 
  grweight(:) = sqrt(grweight(:)*sumcoslat)
 
  ! ====================================================
  ! Extract surface pressure in pa
  ! to aid in quantifying mass
  ! ========================================
  ! === Option ONE, nemsio === 
  if (use_gfs_nemsio) then  
	  

     call nemsio_readrecv(gfile,'pres','sfc',1,nems_wrk,iret=iret)
     if (iret/=0) then
        write(6,*)'gridio/readgriddata_efsoi: GFS: problem with nemsio_readrecv(ps), iret=',iret
        call stop2(23)
     endif
  
     ! Assign surface pressure in pa
     psfc = nems_wrk  

     ! Extract surface pressure
     ! on reduced gaussian grid
     call regtoreduced(psfc,tmpgrd)

     ! calculate half level pressures 
     if (allocated(nems_vcoord))     deallocate(nems_vcoord)
	 
     allocate(nems_vcoord(nlevs+1,3,2))
     call nemsio_getfilehead(gfile,iret=iret,vcoord=nems_vcoord)
  
     if ( iret /= 0 ) then
        write(6,*)' gridio:  ***ERROR*** problem reading header ', &
                  'vcoord, Status = ',iret
        call stop2(99)
     endif    
  
     if ( idvc == 0 ) then ! sigma coordinate, old file format.
        ak = zero
        bk = nems_vcoord(1:nlevs+1,1,1)
     else if ( idvc == 1 ) then ! sigma coordinate
        ak = zero
        bk = nems_vcoord(1:nlevs+1,2,1)
		
     else if ( idvc == 2 .or. idvc == 3 ) then ! hybrid coordinate
   
        ! AFE      ak = nems_vcoord(1:nlevs+1,1,1)
        ! AFE      ak = nems_vcoord(1:nlevs+1,2,1)
        ak = 0.01_r_kind*nems_vcoord(1:nlevs+1,1,1) ! convert to mb
        bk = nems_vcoord(1:nlevs+1,2,1)
     else
        write(6,*)'gridio:  ***ERROR*** INVALID value for idvc=',idvc
        call stop2(85)
     end if
  
     !==> pressure at interfaces. 
     do k=1,nlevs+1 
        pressi(:,k)=ak(k)+bk(k)*tmpgrd 
     end do 
     deallocate(ak,bk) 
  
  ! === Option TWO, NetCDF === 
  else if (use_gfs_ncio) then
	  
	 call mpi_barrier(mpi_comm_world,ierr)

     call read_vardata(dset, 'pressfc', values_2d,errcode=iret)
	 
     if (iret /= 0) then
        print *,'error reading ps'
        call stop2(31)
     endif
	 
     psg = 0.01_r_kind*reshape(values_2d,(/nlons*nlats/))
	 
     ! Extract surface pressure
     ! on reduced gaussian grid
     call regtoreduced(psg,tmpgrd)	 
	 
     call read_attribute(dset, 'ak', ak)
     call read_attribute(dset, 'bk', bk)
  
     ! pressure at interfaces
     do k=1,nlevs+1
        pressi(:,k) = 0.01_r_kind*ak(nlevs-k+2)+bk(nlevs-k+2)*tmpgrd
		
        if (nproc == 0) print *,'netcdf, min/max pressi',k,minval(pressi(:,k)),maxval(pressi(:,k))
     enddo
	 
     deallocate(ak,bk,values_2d)  
	 
 ! === Option THREE, other === 	 
 else
	 
    vrtspec = sigdata%ps
    call sptez_s(vrtspec,psg,1)
    !==> input psg is ln(ps) in centibars - convert to ps in millibars.
    psg = 10._r_kind*exp(psg)
    allocate(ak(nlevs+1),bk(nlevs+1))
    if (sighead%idvc .eq. 0) then ! sigma coordinate, old file format.
        ak = zero
        bk = sighead%si(1:nlevs+1)
    else if (sighead%idvc == 1) then ! sigma coordinate
        ak = zero
        bk = sighead%vcoord(1:nlevs+1,2)
    else if (sighead%idvc == 2 .or. sighead%idvc == 3) then ! hybrid coordinate
        bk = sighead%vcoord(1:nlevs+1,2)
        ak = 0.01_r_kind*sighead%vcoord(1:nlevs+1,1)  ! convert to mb
    else
        print *,'unknown vertical coordinate type',sighead%idvc
        call stop2(32)
    end if

    do k=1,nlevs+1
       pressi(:,k)=ak(k)+bk(k)*psg
       print *,'sigio, min/max pressi',k,minval(pressi(:,k)),maxval(pressi(:,k))
    enddo
	
    deallocate(ak,bk)
	
 endif
 
 if (use_gfs_nemsio) call nemsio_close(gfile,iret=iret)
 if (use_gfs_ncio)   call close_dataset(dset)   
  
  !$omp parallel do private(k) shared(weight,pressi,grweight,nlevs) 
  do k=1,nlevs 
     ! sqrt(dp)*sqrt(area) 
     weight(:,k)=sqrt( (pressi(:,k)-pressi(:,k+1))/tmpgrd(:))*grweight(:) 
  end do

  !$omp end parallel do
  
  
  

	 

   
  return 
  
 end subroutine get_weight
 







 subroutine destroy_weight 
  implicit none 
  if(allocated(weight)) deallocate(weight) 
  if(allocated(grweight)) deallocate(grweight) 
 end subroutine destroy_weight 
 
 
 
 
 
 subroutine divide_weight(grdin) 
  implicit none 
  real(r_single), dimension(npts_max,ncdim), intent(inout) :: grdin 
  real(r_single) cptr,qweight,rdtrpr 
  integer(i_kind) :: k,npt 
  cptr = real(sqrt(tref/cp),r_kind) 
  qweight = real(sqrt(cp*tref/wmoist)/hvap,r_kind) 
  rdtrpr = real(sqrt(pref/(rd*tref)),r_kind) 
  do npt=1,numptsperproc(nproc+1) 
     do k=1,nlevs 
        grdin(npt,k) = grdin(npt,k) / weight(indxproc(nproc+1,npt),k) 
        grdin(npt,nlevs+k) = grdin(npt,nlevs+k) / weight(indxproc(nproc+1,npt),k) 
        grdin(npt,2*nlevs+k) = grdin(npt,2*nlevs+k) * cptr / weight(indxproc(nproc+1,npt),k) 
        if (nvars .gt. 3) then 
           grdin(npt,3*nlevs+k) = grdin(npt,3*nlevs+k) * qweight / weight(indxproc(nproc+1,npt),k) 
        end if 
     end do 
! AFE the indexing schema needs to be cleaned up
!     grdin(npt,nvars*nlevs+1) = grdin(npt,nvars*nlevs+1) & 
     grdin(npt,ncdim) = grdin(npt,ncdim) & 
          & * rdtrpr / grweight(indxproc(nproc+1,npt)) 
  end do 
  return 
 end subroutine divide_weight 


 subroutine readgriddata_efsoi(vars3d,vars2d,n3d,n2d,levels,ndim,grdin,mode,nanal,ft,hr,infilename)
	 
  use sigio_module, only: sigio_head, sigio_data, sigio_sclose, sigio_sropen, &
                          sigio_srohdc, sigio_sclose, sigio_aldata, sigio_axdata
						  
  use nemsio_module, only: nemsio_gfile,nemsio_open,nemsio_close,&
                           nemsio_getfilehead,nemsio_getheadvar,nemsio_realkind,nemsio_charkind,&
                           nemsio_readrecv,nemsio_init,nemsio_setheadvar,nemsio_writerecv
						   
  use module_ncio, only: Dataset, Variable, Dimension, open_dataset,&
                         read_attribute, close_dataset, get_dim, read_vardata
					
  implicit none

  integer, intent(in), optional :: nanal
  integer, intent(in), optional :: ft
  integer, intent(in), optional :: hr
  
  character(len=100), intent(in), optional :: infilename 
  integer, intent(in) :: mode
  character(len=max_varname_length), dimension(n2d), intent(in) :: vars2d
  character(len=max_varname_length), dimension(n3d), intent(in) :: vars3d
  character(len=7) charnanal
  integer, intent(in) :: n2d, n3d
  integer, dimension(0:n3d), intent(in) :: levels
  integer, intent(in) :: ndim
  real(r_single), dimension(npts,ndim), intent(out) :: grdin
  real(r_kind) cptr,qweight,rdtrpr
  character(len=500) :: filename
  character(len=3) charft
  character(len=2) charhr

  real(r_kind), dimension(nlons*nlats)          :: ug,vg
  real(r_single), dimension(npts,nlevs)         :: tv, q, tv_to_t
  real(r_kind), allocatable, dimension(:)       :: psg
  
  real(r_single),allocatable,dimension(:,:,:)   :: nems_vcoord
  real(nemsio_realkind), dimension(nlons*nlats) :: nems_wrk,nems_wrk2
  type(nemsio_gfile) :: gfile

  integer(i_kind) :: u_ind, v_ind, tv_ind, q_ind
  integer(i_kind) :: ps_ind

  integer(i_kind) :: k,iret,idvc,nlonsin,nlatsin,nlevsin
  
  character(len=10) :: fileformat
  integer(i_kind)   :: iunitsig
  type(sigio_head)  :: sighead
  type(sigio_data)  :: sigdata
  type(Dataset)     :: dset
  type(Dimension)   :: londim,latdim,levdim	  
  real(r_kind), dimension(ndimspec) :: vrtspec,divspec	  
  real(r_single), allocatable, dimension(:,:,:) :: ug3d,vg3d
  real(r_single), allocatable, dimension(:,:) :: values_2d
  integer(i_kind) :: nb,ne


 
  iunitsig = 77
  nb = 1
  ne = 1

  ! ----------------------------!
  ! EFSOI Filename constructions!
  ! ----------------------------!

  ! Construct the Filename based on
  ! input arguments
  if(.not. present(infilename)) then
     write(charft, '(i3.3)') ft
     write(charhr, '(i2.2)') hr
     if (nanal > 0) then
        write(charnanal,'(a3, i3.3)') 'mem', nanal
     else
        charnanal = 'ensmean'
     endif
  endif 

  ! ======================================
  if (use_gfs_nemsio) then
	  fileformat = '.nemsio'
  else if (use_gfs_ncio) then
	  fileformat = '.nc'
  else
	  print *,'Warning in gridio_efsoi.f90 '
  end if
  ! ======================================


  ! === EFSOI filename construction =============
  if(forecast_impact) then
 
     if(mode == read_ensmean_forecast) then
         filename = trim(adjustl(datapath))//"gdas.t"//charhr//"z.atmf"//charft// &
               "."//trim(adjustl(charnanal))//trim(fileformat)
     else if(mode == read_analysis_mean) then
         filename = trim(adjustl(datapath))//"gdas.t"//charhr//"z.atmanl."// &
               trim(adjustl(charnanal))//trim(fileformat)
     else if(mode == read_member_forecasts) then
        filename = trim(adjustl(datapath))//trim(adjustl(charnanal))//"/"// &
              "gdas.t"//charhr//"z.atmf"//charft//trim(fileformat)
     else if(mode == read_verification) then
        filename = trim(adjustl(datapath))//infilename
     else
        print *,'This mode is not supported: mode=',mode
        call stop2(23)
     end if
  else
     ! Analysis Impact
     if(mode == read_ensmean_forecast) then
        filename = trim(adjustl(datapath))//"gdas.t"//charhr//"z.atmf"//charft// &
              "."//trim(adjustl(charnanal))//trim(fileformat)
     else if(mode == read_member_analyses) then
        filename = trim(adjustl(datapath))//trim(adjustl(charnanal))//"/"//"gdas.t"// &
              charhr//"z.atmanl"//trim(fileformat)
     else if(mode == read_verification) then
        filename = trim(adjustl(datapath))//infilename
     else
        print *,'This mode is not supported: mode=',mode
        call stop2(23)
     end if
  endif         

  ! --------------------------------------------!
  ! Read in state vector from file and transform!
  ! to EFSOI relevant quantities                !
  ! --------------------------------------------!


  if (nproc == 0) print *,'reading state vector file: ',filename

  if (use_gfs_nemsio) then 
	  
     call nemsio_init(iret=iret)
     if(iret/=0) then
        write(6,*)'gridio/readgriddata_efsoi: GFS: problem with nemsio_init, iret=',iret
        call stop2(23)
     end if
	 
     call nemsio_open(gfile,filename,'READ',iret=iret)
  
     if (iret/=0) then
        write(6,*)'gridio/readgriddata_efsoi: GFS: problem with nemsio_open, iret=',iret
        call stop2(23)
     endif
     call nemsio_getfilehead(gfile,iret=iret, dimx=nlonsin, dimy=nlatsin,&
                             dimz=nlevsin,idvc=idvc)
     if (nlons /= nlonsin .or. nlats /= nlatsin .or. nlevs /= nlevsin) then
        print *,'incorrect dims in nemsio file'
        print *,'expected',nlons,nlats,nlevs
        print *,'got',nlonsin,nlatsin,nlevsin
        call stop2(23)
     end if

  else if (use_gfs_ncio) then 

     dset = open_dataset(filename)
	 
     londim = get_dim(dset,'grid_xt'); nlonsin = londim%len
     latdim = get_dim(dset,'grid_yt'); nlatsin = latdim%len
     levdim = get_dim(dset,'pfull');   nlevsin = levdim%len
     idvc=2
	 	 	 
  else
	  
     call sigio_srohdc(iunitsig,trim(filename), &
                       sighead,sigdata,iret)
     if (iret /= 0) then
        print *,'error reading file in gridio ',trim(filename)
        call stop2(23)
     end if
  endif
	 
	 
  cptr = sqrt(cp/tref)
  qweight = sqrt(wmoist/(cp*tref))*hvap
  rdtrpr = sqrt(rd*tref)/pref

  
  u_ind   = getindex(vars3d, 'u')   !< indices in the state var arrays
  v_ind   = getindex(vars3d, 'v')   ! U and V (3D)
  tv_ind  = getindex(vars3d, 'tv')  ! Tv (3D)
  q_ind   = getindex(vars3d, 'q')   ! Q (3D)
  ps_ind  = getindex(vars2d, 'ps')  ! Ps (2D)


 
  if (.not. isinitialized) call init_spec_vars(nlons,nlats,ntrunc,4)

  allocate(psg(nlons*nlats))
 
  
  ! ======================================================
  ! Get surface pressure
  ! ====================
  if (use_gfs_nemsio) then
	  
     call nemsio_readrecv(gfile,'pres','sfc',1,nems_wrk,iret=iret)
     if (iret/=0) then
        write(6,*)'gridio/readgriddata_efsoi: GFS: problem with nemsio_readrecv(ps), iret=',iret
        call stop2(23)
     endif
     psg = 0.01_r_kind*nems_wrk ! convert ps to millibars.

     ! 
     if (allocated(nems_vcoord))     deallocate(nems_vcoord)
     allocate(nems_vcoord(nlevs+1,3,2))
     call nemsio_getfilehead(gfile,iret=iret,vcoord=nems_vcoord)
     if ( iret /= 0 ) then
        write(6,*)' gridio:  ***ERROR*** problem reading header ', &
           'vcoord, Status = ',iret
        call stop2(99)
     endif
	 
  else if (use_gfs_ncio) then
	  
     call read_vardata(dset, 'pressfc', values_2d,errcode=iret)
     if (iret /= 0) then
        print *,'error reading ps'
        call stop2(31)
     endif
     psg = 0.01_r_kind*reshape(values_2d,(/nlons*nlats/)) 
	 
  else
     vrtspec = sigdata%ps
     call sptez_s(vrtspec,psg,1)
     !==> input psg is ln(ps) in centibars - convert to ps in millibars.
     psg = 10._r_kind*exp(psg)
  endif 
 
    
  
  ! ============================================================================= 
  !  get U,V,temp,q,ps on gaussian grid.
  ! =====================================
  ! u is first nlevs, v is second, t is third, then tracers.
  if (use_gfs_nemsio) then
	  
     do k=1,nlevs
		
		! Get u-wind
        call nemsio_readrecv(gfile,'ugrd','mid layer',k,nems_wrk,iret=iret)
		
        if (iret/=0) then
           write(6,*)'gridio/readgriddata_efsoi: GFS: problem with nemsio_readrecv(ugrd), iret=',iret
           call stop2(23)
        endif
		
        ug = nems_wrk
		
		! Get v-wind
        call nemsio_readrecv(gfile,'vgrd','mid layer',k,nems_wrk,iret=iret)
        if (iret/=0) then
            write(6,*)'gridio/readgriddata_efsoi: GFS: problem with nemsio_readrecv(vgrd), iret=',iret
            call stop2(23)
        endif
    
        vg = nems_wrk
    
        if (u_ind > 0) call copytogrdin(ug,grdin(:,levels(u_ind-1) + k))
        if (v_ind > 0) call copytogrdin(vg,grdin(:,levels(v_ind-1) + k))
        
		
        ! Transformation to EFSOI relevant quantities
        ! Assign weighted kinetic energy components. There
        ! are no unit/metric differences for the kinetic component
        grdin(:,levels(u_ind-1) + k) = weight(:,k) * grdin(:,levels(u_ind-1) + k)
        grdin(:,levels(v_ind-1) + k) = weight(:,k) * grdin(:,levels(v_ind-1) + k)
     
        call nemsio_readrecv(gfile,'tmp','mid layer',k,nems_wrk,iret=iret)
        if (iret/=0) then
           write(6,*)'gridio/readgriddata_efsoi: GFS: problem with nemsio_readrecv(tmp), iret=',iret
           call stop2(23)
        endif
		
        call nemsio_readrecv(gfile,'spfh','mid layer',k,nems_wrk2,iret=iret)
        if (iret/=0) then
           write(6,*)'gridio/readgriddata_efsoi: GFS: problem with nemsio_readrecv(spfh), iret=',iret
           call stop2(23)
        endif
    
        ug = nems_wrk
        vg = nems_wrk2
		
        call copytogrdin(ug,tv(:,k))
        call copytogrdin(vg, q(:,k))

        ! Transformation to EFSOI relevant quantities
        ! Mass component quantities
        tv(:,k) = cptr * weight(:,k) * tv(:,k)
		
        ! Moisture component transormation to EFSOI
        ! relevant quantities
        q(:,k) = qweight * weight(:,k) * q(:,k)
        tv_to_t(:,k) = ( one / (one + q(:,k)*0.61_r_kind) )
      
        ! Approximate the necessary transformation
        ! of virtual temperature to temperature
        tv(:,k) = tv(:,k) * tv_to_t(:,k)
      
        if (tv_ind > 0) grdin(:,levels(tv_ind-1)+k) = tv(:,k)
        if (q_ind > 0) grdin(:,levels( q_ind-1)+k) =  q(:,k)

     enddo
	 
  else if (use_gfs_ncio) then
	  
	  ! ==== Get U and V ========================================
      call read_vardata(dset, 'ugrd', ug3d,errcode=iret)
	  
      if (iret /= 0) then
         print *,'error reading ugrd'
         call stop2(22)
      endif
	  
      call read_vardata(dset, 'vgrd', vg3d,errcode=iret)
      if (iret /= 0) then
         print *,'error reading vgrd'
         call stop2(23)
      endif

	  
      do k=1,nlevs
         ug = reshape(ug3d(:,:,nlevs-k+1),(/nlons*nlats/))
         vg = reshape(vg3d(:,:,nlevs-k+1),(/nlons*nlats/))
		 
         if (u_ind > 0) call copytogrdin(ug,grdin(:,levels(u_ind-1) + k))
         if (v_ind > 0) call copytogrdin(vg,grdin(:,levels(v_ind-1) + k))
		 
         ! Transformation to EFSOI relevant quantities
         ! Assign weighted kinetic energy components. There
         ! are no unit/metric differences for the kinetic component
         grdin(:,levels(u_ind-1) + k) = weight(:,k) * grdin(:,levels(u_ind-1) + k)
         grdin(:,levels(v_ind-1) + k) = weight(:,k) * grdin(:,levels(v_ind-1) + k)	 
		 
         ! calculate vertical integral of mass flux div (ps tendency)
         ! this variable is analyzed in order to enforce mass balance in the analysis
         !if (pst_ind > 0) then
         !   ug = ug*(pressi(:,k)-pressi(:,k+1))
         !   vg = vg*(pressi(:,k)-pressi(:,k+1))
         !   call sptezv_s(divspec,vrtspec,ug,vg,-1) ! u,v to div,vrt
         !   call sptez_s(divspec,vmassdiv(:,k),1) ! divspec to divgrd
         !endif
      enddo
	  
 
	  
	  ! ==== Get T and Q ===========================================
      call read_vardata(dset,'tmp', ug3d,errcode=iret)
	  
      if (iret /= 0) then
         print *,'error reading tmp'
         call stop2(24)
      endif
	  
      call read_vardata(dset,'spfh', vg3d,errcode=iret)
	  
      if (iret /= 0) then
         print *,'error reading spfh'
         call stop2(25)
      endif	   
	  	  
      do k=1,nlevs
         ug = reshape(ug3d(:,:,nlevs-k+1),(/nlons*nlats/))
         vg = reshape(vg3d(:,:,nlevs-k+1),(/nlons*nlats/))
         
		 !if (tsen_ind > 0) call copytogrdin(ug,grdin(:,levels(tsen_ind-1)+k,nb,ne))
		 	 
         call copytogrdin(vg, q(:,k))
		 
         ug = ug * ( 1.0 + fv*vg ) ! convert T to Tv
         ! 
         call copytogrdin(ug,tv(:,k))
		 
         ! Transformation to EFSOI relevant quantities
         ! Mass component quantities
         tv(:,k) = cptr * weight(:,k) * tv(:,k)
         ! Moisture component transormation to EFSOI
         ! relevant quantities
         q(:,k) = qweight * weight(:,k) * q(:,k)
         tv_to_t(:,k) = ( one / (one + q(:,k)*0.61_r_kind) )
    
         ! Approximate the necessary transformation
         ! of virtual temperature to temperature
         tv(:,k) = tv(:,k) * tv_to_t(:,k)
		 		 
         if (tv_ind > 0)   grdin(:,levels(tv_ind-1)+k) = tv(:,k)
         if (q_ind > 0)    grdin(:,levels( q_ind-1)+k) =  q(:,k)
      enddo
	  
      deallocate(ug3d,vg3d)
	  
  else
	  
	 ! $omp parallel do private(k,ug,vg,divspec,vrtspec) 
	 ! shared(sigdata,pressi,vmassdiv,grdin,tv,q,cw,u_ind,v_ind,pst_ind,q_ind,tsen_ind,cw_ind,qi_ind,ql_ind)
	 do k=1,nlevs

	    vrtspec = sigdata%z(:,k); divspec = sigdata%d(:,k)
	    call sptezv_s(divspec,vrtspec,ug,vg,1)
	    if (u_ind > 0) then
	       call copytogrdin(ug,grdin(:,levels(u_ind-1)+k))
	    endif
	    if (v_ind > 0) then
	       call copytogrdin(vg,grdin(:,levels(v_ind-1)+k))
	    endif

	    ! calculate vertical integral of mass flux div (ps tendency)
	    ! this variable is analyzed in order to enforce mass balance in the analysis
	    !if (pst_ind > 0) then
	    !   ug = ug*(pressi(:,k)-pressi(:,k+1))
	    !   vg = vg*(pressi(:,k)-pressi(:,k+1))
	    !   call sptezv_s(divspec,vrtspec,ug,vg,-1) ! u,v to div,vrt
	    !   call sptez_s(divspec,vmassdiv(:,k),1) ! divspec to divgrd
	    !endif

	    divspec = sigdata%t(:,k)
	    call sptez_s(divspec,ug,1)
	    call copytogrdin(ug,tv(:,k))
	    
	    divspec = sigdata%q(:,k,1)
	    call sptez_s(divspec,vg,1)
	    call copytogrdin(vg,q(:,k))
		
        ! Transformation to EFSOI relevant quantities
        ! Mass component quantities
        tv(:,k) = cptr * weight(:,k) * tv(:,k)
        ! Moisture component transormation to EFSOI
        ! relevant quantities
        q(:,k) = qweight * weight(:,k) * q(:,k)
        tv_to_t(:,k) = ( one / (one + q(:,k)*0.61_r_kind) )
   
        ! Approximate the necessary transformation
        ! of virtual temperature to temperature
        tv(:,k) = tv(:,k) * tv_to_t(:,k)
				
		if (tv_ind > 0)          grdin(:,levels(tv_ind-1)+k) = tv(:,k)
	    if (q_ind > 0)           grdin(:,levels( q_ind-1)+k) =  q(:,k)	  	  
	  
     enddo
  endif
  	  	  

  ! =============================================================
  ! surface pressure 
  ! ================
  if (ps_ind > 0) then
    call copytogrdin(psg,grdin(:,levels(n3d) + ps_ind))
  endif

  ! Transformation to EFSOI relevant quantities
  ! Surface pressure contribution
  grdin(:,levels(n3d) + ps_ind) = rdtrpr * grweight(:) * 100._r_kind * grdin(:,levels(n3d) + ps_ind)
  
  deallocate(psg)
 
  if (use_gfs_nemsio) call nemsio_close(gfile,iret=iret)
  if (use_gfs_ncio)   call close_dataset(dset)

  return
 
  contains
	  
  ! copying to grdin (calling regtoreduced if reduced grid)
  subroutine copytogrdin(field, grdin)
  implicit none

  real(r_kind), dimension(:), intent(in)      :: field
  real(r_single), dimension(:), intent(inout) :: grdin

  call regtoreduced(field, grdin)

  end subroutine copytogrdin

 end subroutine readgriddata_efsoi

end module gridio_efsoi
