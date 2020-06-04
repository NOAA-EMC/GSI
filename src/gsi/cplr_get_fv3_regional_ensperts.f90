module get_fv3_regional_ensperts_mod
use abstract_get_fv3_regional_ensperts_mod,only: abstract_get_fv3_regional_ensperts_class
  use kinds, only : i_kind
  type, extends(abstract_get_fv3_regional_ensperts_class) :: get_fv3_regional_ensperts_class
  contains
    procedure, pass(this) :: get_fv3_regional_ensperts => get_fv3_regional_ensperts_run
    procedure, pass(this) :: ens_spread_dualres_regional => ens_spread_dualres_regional_fv3_regional
    procedure, pass(this) :: general_read_fv3_regional
  end type get_fv3_regional_ensperts_class
contains
  subroutine get_fv3_regional_ensperts_run(this,en_perts,nelen,ps_bar)
  !$$$  subprogram documentation block
  !                .      .    .                                       .
  ! subprogram:    get_fv3_regional_ensperts  read arw model ensemble members
  !   prgmmr: Ting            org: EMC/NCEP            date: 2018-12-13
  !
  ! abstract: read ensemble members from the fv3 regional (fv3_SAR)
  ! model,following Wanshu's programs to read those background files 
  !
  !
  ! program history log:
  !   2011-08-31  todling - revisit en_perts (single-prec) in light of extended bundle
  !
  !   input argument list:
  !
  !   output argument list:
  !
  ! attributes:
  !   language: f90
  !   machine: 
  !
  !$$$ end documentation block
  
      use kinds, only: r_kind,i_kind,r_single
      use constants, only: zero,one,half,zero_single,rd_over_cp,one_tenth
      use mpimod, only: mpi_comm_world,ierror,mype
      use hybrid_ensemble_parameters, only: n_ens,grd_ens
      use hybrid_ensemble_parameters, only: ntlevs_ens,ensemble_path
      use control_vectors, only: cvars2d,cvars3d,nc2d,nc3d
      use gsi_bundlemod, only: gsi_bundlecreate
      use gsi_bundlemod, only: gsi_grid
      use gsi_bundlemod, only: gsi_bundle
      use gsi_bundlemod, only: gsi_bundlegetpointer
      use gsi_bundlemod, only: gsi_bundledestroy
      use gsi_bundlemod, only: gsi_gridcreate
      use gsi_4dvar, only: ens_fhrlevs
      use gsi_rfv3io_mod, only: type_fv3regfilenameg
  
      implicit none
      class(get_fv3_regional_ensperts_class), intent(inout) :: this
      type(gsi_bundle),allocatable, intent(inout) :: en_perts(:,:)
      integer(i_kind), intent(in   ):: nelen
      real(r_single),dimension(:,:,:),allocatable,intent(inout):: ps_bar
  
      real(r_kind),dimension(grd_ens%lat2,grd_ens%lon2,grd_ens%nsig):: u,v,tv,oz,rh
      real(r_kind),dimension(grd_ens%lat2,grd_ens%lon2):: ps
  
      real(r_single),pointer,dimension(:,:,:):: w3
      real(r_single),pointer,dimension(:,:):: w2
      real(r_kind),pointer,dimension(:,:,:):: x3
      real(r_kind),pointer,dimension(:,:):: x2
      type(gsi_bundle),allocatable,dimension(:):: en_bar
      type(gsi_grid):: grid_ens
      real(r_kind):: bar_norm,sig_norm,kapr,kap1
  
      integer(i_kind):: i,j,k,n,mm1,istatus
      integer(i_kind):: ic2,ic3
      integer(i_kind):: m

      
      character(255) ensfilenam_str
      type(type_fv3regfilenameg)::fv3_filename 
  
      call gsi_gridcreate(grid_ens,grd_ens%lat2,grd_ens%lon2,grd_ens%nsig)
      ! Allocate bundle to hold mean of ensemble members
      allocate(en_bar(ntlevs_ens))
      do m=1,ntlevs_ens
        call gsi_bundlecreate(en_bar(m),grid_ens,'ensemble',istatus,names2d=cvars2d,names3d=cvars3d,bundle_kind=r_kind)
        if(istatus/=0) then
           write(6,*)' get_fv3_regional_ensperts_netcdf: trouble creating en_bar bundle'
           call stop2(9991)
        endif
      enddo ! for m 
  

      do m=1,ntlevs_ens



  !
  ! INITIALIZE ENSEMBLE MEAN ACCUMULATORS
         en_bar(m)%values=zero
  
         do n=1,n_ens
            en_perts(n,m)%valuesr4 = zero
         enddo
  
         mm1=mype+1
         kap1=rd_over_cp+one
         kapr=one/rd_over_cp
  !
  ! LOOP OVER ENSEMBLE MEMBERS 
         do n=1,n_ens
          write(ensfilenam_str,22) trim(adjustl(ensemble_path)),ens_fhrlevs(m),n
22  format(a,'fv3SAR',i2.2,'_ens_mem',i3.3)
  ! DEFINE INPUT FILE NAME
             fv3_filename%grid_spec=trim(ensfilenam_str)//'-fv3_grid_spec' !exmaple thinktobe
             fv3_filename%ak_bk=trim(ensfilenam_str)//'-fv3_akbk'
             fv3_filename%dynvars=trim(ensfilenam_str)//'-fv3_dynvars'
             fv3_filename%tracers=trim(ensfilenam_str)//"-fv3_tracer"
             fv3_filename%sfcdata=trim(ensfilenam_str)//"-fv3_sfcdata"
             fv3_filename%couplerres=trim(ensfilenam_str)//"-coupler.res"
  ! 
  ! READ ENEMBLE MEMBERS DATA
            if (mype == 0) write(6,'(a,a)') 'CALL READ_FV3_REGIONAL_ENSPERTS FOR ENS DATA with the filename str : ',trim(ensfilenam_str)
            call this%general_read_fv3_regional(fv3_filename,ps,u,v,tv,rh,oz) 
  
  ! SAVE ENSEMBLE MEMBER DATA IN COLUMN VECTOR
            do ic3=1,nc3d
  
               call gsi_bundlegetpointer(en_perts(n,m),trim(cvars3d(ic3)),w3,istatus)
               if(istatus/=0) then
                  write(6,*)' error retrieving pointer to ',trim(cvars3d(ic3)),' for ensemble member ',n
                  call stop2(9992)
               end if
               call gsi_bundlegetpointer(en_bar(m),trim(cvars3d(ic3)),x3,istatus)
               if(istatus/=0) then
                  write(6,*)' error retrieving pointer to ',trim(cvars3d(ic3)),' for en_bar'
                  call stop2(9993)
               end if
  
               select case (trim(cvars3d(ic3)))
  
                  case('sf','SF')
     
                     do k=1,grd_ens%nsig
                        do i=1,grd_ens%lon2
                           do j=1,grd_ens%lat2
                              w3(j,i,k) = u(j,i,k)
                              x3(j,i,k)=x3(j,i,k)+u(j,i,k)
                           end do
                        end do
                     end do
  
                  case('vp','VP')
  
                     do k=1,grd_ens%nsig
                        do i=1,grd_ens%lon2
                           do j=1,grd_ens%lat2
                              w3(j,i,k) = v(j,i,k)
                              x3(j,i,k)=x3(j,i,k)+v(j,i,k)
                           end do
                        end do
                     end do
  
                  case('t','T')
  
                     do k=1,grd_ens%nsig
                        do i=1,grd_ens%lon2
                           do j=1,grd_ens%lat2
                              w3(j,i,k) = tv(j,i,k)
                              x3(j,i,k)=x3(j,i,k)+tv(j,i,k)
                           end do
                        end do
                     end do
  
                  case('q','Q')
  
                     do k=1,grd_ens%nsig
                        do i=1,grd_ens%lon2
                           do j=1,grd_ens%lat2
                              w3(j,i,k) = rh(j,i,k)
                              x3(j,i,k)=x3(j,i,k)+rh(j,i,k)
                           end do
                        end do
                     end do
  
                  case('oz','OZ')
  
                     do k=1,grd_ens%nsig
                        do i=1,grd_ens%lon2
                           do j=1,grd_ens%lat2
                              w3(j,i,k) = oz(j,i,k)
                              x3(j,i,k)=x3(j,i,k)+oz(j,i,k)
                           end do
                        end do
                     end do
  
  
               end select
            end do
  
            do ic2=1,nc2d
     
               call gsi_bundlegetpointer(en_perts(n,m),trim(cvars2d(ic2)),w2,istatus)
               if(istatus/=0) then
                  write(6,*)' error retrieving pointer to ',trim(cvars2d(ic2)),' for ensemble member ',n
                  call stop2(9994)
               end if
               call gsi_bundlegetpointer(en_bar(m),trim(cvars2d(ic2)),x2,istatus)
               if(istatus/=0) then
                  write(6,*)' error retrieving pointer to ',trim(cvars2d(ic2)),' for en_bar'
                  call stop2(9995)
               end if
  
               select case (trim(cvars2d(ic2)))
  
                  case('ps','PS')
  
                     do i=1,grd_ens%lon2
                        do j=1,grd_ens%lat2
                           w2(j,i) = ps(j,i)
                           x2(j,i)=x2(j,i)+ps(j,i)
                        end do
                     end do
  
                  case('sst','SST')
  ! IGNORE SST IN HYBRID for now
  
                     do i=1,grd_ens%lon2
                        do j=1,grd_ens%lat2
                           w2(j,i) = zero
                           x2(j,i)=zero
                        end do
                     end do
  
               end select
            end do
         enddo 
  !
  ! CALCULATE ENSEMBLE MEAN
         bar_norm = one/float(n_ens)
         en_bar(m)%values=en_bar(m)%values*bar_norm
  
  ! Copy pbar to module array.  ps_bar may be needed for vertical localization
  ! in terms of scale heights/normalized p/p
         do ic2=1,nc2d
   
            if(trim(cvars2d(ic2)) == 'ps'.or.trim(cvars2d(ic2)) == 'PS') then
  
               call gsi_bundlegetpointer(en_bar(m),trim(cvars2d(ic2)),x2,istatus)
               if(istatus/=0) then
                  write(6,*)' error retrieving pointer to ',trim(cvars2d(ic2)),' for en_bar to get ps_bar'
                  call stop2(9996)
               end if
   
               do i=1,grd_ens%lon2
                  do j=1,grd_ens%lat2
                     ps_bar(j,i,1)=x2(j,i)
                  end do
               end do
               exit
            end if
         end do
  
         call mpi_barrier(mpi_comm_world,ierror)
  !
  ! CALCULATE ENSEMBLE SPREAD
         call this%ens_spread_dualres_regional(mype,en_perts,nelen,en_bar(m))
         call mpi_barrier(mpi_comm_world,ierror)
  !
  ! CONVERT ENSEMBLE MEMBERS TO ENSEMBLE PERTURBATIONS
         sig_norm=sqrt(one/max(one,n_ens-one))
  
         do n=1,n_ens
            do i=1,nelen
               en_perts(n,m)%valuesr4(i)=(en_perts(n,m)%valuesr4(i)-en_bar(m)%values(i))*sig_norm
            end do
         end do

     enddo ! it 4d loop
      do m=1,ntlevs_ens
      call gsi_bundledestroy(en_bar(m),istatus)
      if(istatus/=0) then
        write(6,*)' in get_fv3_regional_ensperts_netcdf: trouble destroying en_bar bundle'
                call stop2(9997)
      endif
   end do

        deallocate(en_bar)
  !
  
  return

30 write(6,*) 'get_fv3_regional_ensperts_netcdf: open filelist failed '
   call stop2(555)
20 write(6,*) 'get_fv3_regional_ensperts_netcdf: read WRF-ARW ens failed ',n
   call stop2(555)

  end subroutine get_fv3_regional_ensperts_run
  
  subroutine general_read_fv3_regional(this,fv3_filenameginput,g_ps,g_u,g_v,g_tv,g_rh,g_oz)
  !$$$  subprogram documentation block
  !     first compied from general_read_arw_regional           .      .    .                                       .
  ! subprogram:    general_read_fv3_regional  read fv3sar model ensemble members
  !   prgmmr: Ting             org: emc/ncep            date: 2018
  !
  ! abstract: read ensemble members from the fv3sar model in "restart" or "cold start"  netcdf format
  !           for use with hybrid ensemble option. 
  !
  ! program history log:
  !   2018-  Ting      - intial versions  
  !
  !   input argument list:
  !
  !   output argument list:
  !
  ! attributes:
  !   language: f90
  !   machine:  ibm RS/6000 SP
  !
  !$$$ end documentation block
  
      use netcdf, only: nf90_nowrite
      use netcdf, only: nf90_open,nf90_close
      use netcdf, only: nf90_inq_dimid,nf90_inquire_dimension
      use netcdf, only: nf90_inq_varid,nf90_inquire_variable,nf90_get_var
      use kinds, only: r_kind,r_single,i_kind
      use gridmod, only: eta1_ll,eta2_ll
      use constants, only: zero,one,fv,zero_single,one_tenth,h300
      use hybrid_ensemble_parameters, only: grd_ens,q_hyb_ens
      use hybrid_ensemble_parameters, only: fv3sar_ensemble_opt 

      use mpimod, only: mpi_comm_world,mpi_rtype
      use netcdf_mod, only: nc_check
      use gsi_rfv3io_mod,only: type_fv3regfilenameg
      use gsi_rfv3io_mod,only:n2d 
      use gsi_rfv3io_mod,only:mype_t,mype_p ,mype_q,mype_oz
      use constants, only: half,zero
      use gsi_rfv3io_mod, only: gsi_fv3ncdf_read 
      use gsi_rfv3io_mod, only: gsi_fv3ncdf_read_v1
      use gsi_rfv3io_mod, only: gsi_fv3ncdf_readuv
      use gsi_rfv3io_mod, only: gsi_fv3ncdf_readuv_v1
      use gsi_rfv3io_mod, only: gsi_fv3ncdf2d_read_v1
  
      implicit none
  !
  ! Declare passed variables
      class(get_fv3_regional_ensperts_class), intent(inout) :: this
      type (type_fv3regfilenameg)                  , intent (in)   :: fv3_filenameginput
      real(r_kind),dimension(grd_ens%lat2,grd_ens%lon2,grd_ens%nsig),intent(out)::g_u,g_v,g_tv,g_rh,g_oz
      real(r_kind),dimension(grd_ens%lat2,grd_ens%lon2),intent(out):: g_ps
      real(r_kind),dimension(grd_ens%lat2,grd_ens%lon2,grd_ens%nsig) ::g_tsen, g_q,g_prsl 
      real(r_kind),dimension(grd_ens%lat2,grd_ens%lon2,grd_ens%nsig+1) ::g_prsi 
  !
  ! Declare local parameters
      real(r_kind),parameter:: r0_01 = 0.01_r_kind
      real(r_kind),parameter:: r10   = 10.0_r_kind
      real(r_kind),parameter:: r100  = 100.0_r_kind
  !
  !   Declare local variables
      
      integer(i_kind):: i,j,k,kp
      integer(i_kind) iderivative
  
      
      logical ice

      character(len=24),parameter :: myname_ = 'general_read_fv3_regional'

      character(len=:),allocatable :: grid_spec !='fv3_grid_spec'            
      character(len=:),allocatable :: ak_bk     !='fv3_akbk'
      character(len=:),allocatable :: dynvars   !='fv3_dynvars'
      character(len=:),allocatable :: tracers   !='fv3_tracer'
      character(len=:),allocatable :: sfcdata   !='fv3_sfcdata'
      character(len=:),allocatable :: couplerres!='coupler.res'
      
      associate( this => this ) ! eliminates warning for unused dummy argument needed for binding
      end associate



    grid_spec=fv3_filenameginput%grid_spec
    ak_bk=fv3_filenameginput%ak_bk
    dynvars=fv3_filenameginput%dynvars
    tracers=fv3_filenameginput%tracers
    sfcdata=fv3_filenameginput%sfcdata
    couplerres=fv3_filenameginput%couplerres

      

!cltthinktobe  should be contained in variable like grd_ens


    if(fv3sar_ensemble_opt == 0 ) then  
      call gsi_fv3ncdf_readuv(dynvars,g_u,g_v)
    else
      call gsi_fv3ncdf_readuv_v1(dynvars,g_u,g_v)
    endif
    if(fv3sar_ensemble_opt == 0) then
      call gsi_fv3ncdf_read(dynvars,'T','t',g_tsen,mype_t)
    else
      call gsi_fv3ncdf_read_v1(dynvars,'t','T',g_tsen,mype_t)
    endif
    if (fv3sar_ensemble_opt == 0) then 
      call gsi_fv3ncdf_read(dynvars,'DELP','delp',g_prsi,mype_p)
      g_prsi(:,:,grd_ens%nsig+1)=eta1_ll(grd_ens%nsig+1) !thinkto be done , should use eta1_ll from ensemble grid
      do i=grd_ens%nsig,1,-1
         g_prsi(:,:,i)=g_prsi(:,:,i)*0.001_r_kind+g_prsi(:,:,i+1)
      enddo
    g_ps(:,:)=g_prsi(:,:,1)
    else  ! for the ensemble processed frm CHGRES
      call gsi_fv3ncdf2d_read_v1(dynvars,'ps','PS',g_ps,mype_p)
      g_ps=g_ps*0.001_r_kind
      do k=1,grd_ens%nsig+1
        g_prsi(:,:,k)=eta1_ll(k)+eta2_ll(k)*g_ps
      enddo
    

    endif
     
    if(fv3sar_ensemble_opt == 0) then
      call gsi_fv3ncdf_read(tracers,'SPHUM','sphum',g_q,mype_q)
      call gsi_fv3ncdf_read(tracers,'O3MR','o3mr',g_oz,mype_oz)
    else
      call gsi_fv3ncdf_read_v1(tracers,'sphum','SPHUM',g_q,mype_q)
      call gsi_fv3ncdf_read_v1(tracers,'o3mr','O3MR',g_oz,mype_oz)
    endif

!!  tsen2tv  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    do k=1,grd_ens%nsig
       do j=1,grd_ens%lon2
          do i=1,grd_ens%lat2
             g_tv(i,j,k)=g_tsen(i,j,k)*(one+fv*g_q(i,j,k))
          enddo
       enddo
    enddo
         if (.not.q_hyb_ens) then
           ice=.true.
           iderivative=0
           do k=1,grd_ens%nsig
             kp=k+1
             do j=1,grd_ens%lon2
               do i=1,grd_ens%lat2
                 g_prsl(i,j,k)=(g_prsi(i,j,k)+g_prsi(i,j,kp))*half
                end do
             end do
           end do
           call genqsat(g_rh,g_tsen(1,1,1),g_prsl(1,1,1),grd_ens%lat2,grd_ens%lon2,grd_ens%nsig,ice,iderivative)
           do k=1,grd_ens%nsig
             do j=1,grd_ens%lon2
               do i=1,grd_ens%lat2
                 g_rh(i,j,k) = g_q(i,j,k)/g_rh(i,j,k)
               end do
             end do
           end do
         else
             do k=1,grd_ens%nsig
               do j=1,grd_ens%lon2
                 do i=1,grd_ens%lat2
                   g_rh(i,j,k) = g_q(i,j,k)
                 end do
                end do
              end do
         end if





  return       
  end subroutine general_read_fv3_regional
  subroutine ens_spread_dualres_regional_fv3_regional(this,mype,en_perts,nelen,en_bar)
  !$$$  subprogram documentation block
  !                .      .    .                                       .
  ! subprogram:    ens_spread_dualres_regional
  !   prgmmr: mizzi            org: ncar/mmm            date: 2010-08-11
  !
  ! abstract:
  !
  !
  ! program history log:
  !   2010-08-11  parrish, initial documentation
  !   2011-04-05  parrish - add pseudo-bundle capability
  !   2011-08-31  todling - revisit en_perts (single-prec) in light of extended bundle
  !
  !   input argument list:
  !     en_bar - ensemble mean
  !      mype  - current processor number
  !
  !   output argument list:
  !
  ! attributes:
  !   language: f90
  !   machine:  ibm RS/6000 SP
  !
  !$$$ end documentation block
  !
    use kinds, only: r_single,r_kind,i_kind
    use hybrid_ensemble_parameters, only: n_ens,grd_ens,grd_anl,p_e2a,uv_hyb_ens, &
                                          regional_ensemble_option
    use general_sub2grid_mod, only: sub2grid_info,general_sub2grid_create_info,general_sube2suba
    use constants, only:  zero,two,half,one
    use control_vectors, only: cvars2d,cvars3d,nc2d,nc3d
    use gsi_bundlemod, only: gsi_bundlecreate
    use gsi_bundlemod, only: gsi_grid
    use gsi_bundlemod, only: gsi_bundle
    use gsi_bundlemod, only: gsi_bundlegetpointer
    use gsi_bundlemod, only: gsi_bundledestroy
    use gsi_bundlemod, only: gsi_gridcreate
    implicit none

    class(get_fv3_regional_ensperts_class), intent(inout) :: this
    type(gsi_bundle),OPTIONAL,intent(in):: en_bar
    integer(i_kind),intent(in):: mype
    type(gsi_bundle),allocatable, intent(in   ) :: en_perts(:,:)
    integer(i_kind), intent(in   ):: nelen
  
    type(gsi_bundle):: sube,suba
    type(gsi_grid):: grid_ens,grid_anl
    real(r_kind) sp_norm,sig_norm_sq_inv
    type(sub2grid_info)::se,sa
    integer(i_kind) k
  
    integer(i_kind) i,n,ic3
    logical regional
    integer(i_kind) num_fields,inner_vars,istat,istatus
    logical,allocatable::vector(:)
    real(r_kind),pointer,dimension(:,:,:):: st,vp,tv,rh,oz,cw
    real(r_kind),pointer,dimension(:,:):: ps
    real(r_kind),dimension(grd_anl%lat2,grd_anl%lon2,grd_anl%nsig),target::dum3
    real(r_kind),dimension(grd_anl%lat2,grd_anl%lon2),target::dum2

    associate( this => this ) ! eliminates warning for unused dummy argument needed for binding
    end associate
 
  !      create simple regular grid
          call gsi_gridcreate(grid_anl,grd_anl%lat2,grd_anl%lon2,grd_anl%nsig)
          call gsi_gridcreate(grid_ens,grd_ens%lat2,grd_ens%lon2,grd_ens%nsig)
  
  !      create two internal bundles, one on analysis grid and one on ensemble grid
  
         call gsi_bundlecreate (suba,grid_anl,'ensemble work',istatus, &
                                   names2d=cvars2d,names3d=cvars3d,bundle_kind=r_kind)
         if(istatus/=0) then
            write(6,*)' in ens_spread_dualres_regional: trouble creating bundle_anl bundle'
            call stop2(9998)
         endif
         call gsi_bundlecreate (sube,grid_ens,'ensemble work ens',istatus, &
                                   names2d=cvars2d,names3d=cvars3d,bundle_kind=r_kind)
         if(istatus/=0) then
            write(6,*)' ens_spread_dualres_regional: trouble creating bundle_ens bundle'
            call stop2(9999)
         endif
  
    sp_norm=(one/float(n_ens))
  
    sube%values=zero
  !
  
    if(regional_ensemble_option == 1)then
       print *,'global ensemble'
       sig_norm_sq_inv=n_ens-one
  
       do n=1,n_ens
          do i=1,nelen
             sube%values(i)=sube%values(i) &
               +en_perts(n,1)%valuesr4(i)*en_perts(n,1)%valuesr4(i)
          end do
       end do
  
       do i=1,nelen
         sube%values(i) = sqrt(sp_norm*sig_norm_sq_inv*sube%values(i))
       end do
    else
       do n=1,n_ens
          do i=1,nelen
             sube%values(i)=sube%values(i) &
               +(en_perts(n,1)%valuesr4(i)-en_bar%values(i))*(en_perts(n,1)%valuesr4(i)-en_bar%values(i))
          end do
       end do
   
       do i=1,nelen
         sube%values(i) = sqrt(sp_norm*sube%values(i))
       end do
    end if
  
    if(grd_ens%latlon1n == grd_anl%latlon1n) then
       do i=1,nelen
          suba%values(i)=sube%values(i)
       end do
    else
       inner_vars=1
       num_fields=max(0,nc3d)*grd_ens%nsig+max(0,nc2d)
       allocate(vector(num_fields))
       vector=.false.
       do ic3=1,nc3d
          if(trim(cvars3d(ic3))=='sf'.or.trim(cvars3d(ic3))=='vp') then
             do k=1,grd_ens%nsig
                vector((ic3-1)*grd_ens%nsig+k)=uv_hyb_ens
             end do
          end if
       end do
       call general_sub2grid_create_info(se,inner_vars,grd_ens%nlat,grd_ens%nlon,grd_ens%nsig,num_fields, &
                                         regional,vector)
       call general_sub2grid_create_info(sa,inner_vars,grd_anl%nlat,grd_anl%nlon,grd_anl%nsig,num_fields, &
                                         regional,vector)
       deallocate(vector)
       call general_sube2suba(se,sa,p_e2a,sube%values,suba%values,regional)
    end if
  
    dum2=zero
    dum3=zero
    call gsi_bundlegetpointer(suba,'sf',st,istat)
    if(istat/=0) then
       write(6,*)' no sf pointer in ens_spread_dualres, point st at dum3 array'
       st => dum3
    end if
    call gsi_bundlegetpointer(suba,'vp',vp,istat)
    if(istat/=0) then
       write(6,*)' no vp pointer in ens_spread_dualres, point vp at dum3 array'
       vp => dum3
    end if
    call gsi_bundlegetpointer(suba,'t',tv,istat)
    if(istat/=0) then
       write(6,*)' no t pointer in ens_spread_dualres, point tv at dum3 array'
       tv => dum3
    end if
    call gsi_bundlegetpointer(suba,'q',rh,istat)
    if(istat/=0) then
       write(6,*)' no q pointer in ens_spread_dualres, point rh at dum3 array'
       rh => dum3
    end if
    call gsi_bundlegetpointer(suba,'oz',oz,istat)
    if(istat/=0) then
       write(6,*)' no oz pointer in ens_spread_dualres, point oz at dum3 array'
       oz => dum3
    end if
    call gsi_bundlegetpointer(suba,'cw',cw,istat)
    if(istat/=0) then
       write(6,*)' no cw pointer in ens_spread_dualres, point cw at dum3 array'
       cw => dum3
    end if
    call gsi_bundlegetpointer(suba,'ps',ps,istat)
    if(istat/=0) then
       write(6,*)' no ps pointer in ens_spread_dualres, point ps at dum2 array'
       ps => dum2
    end if
  
    call write_spread_dualres(st,vp,tv,rh,oz,cw,ps,mype)
  
    return
  end subroutine ens_spread_dualres_regional_fv3_regional
  
end module get_fv3_regional_ensperts_mod
