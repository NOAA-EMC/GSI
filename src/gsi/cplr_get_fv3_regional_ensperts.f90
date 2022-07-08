module get_fv3_regional_ensperts_mod
use abstract_get_fv3_regional_ensperts_mod,only: abstract_get_fv3_regional_ensperts_class
  use kinds, only : i_kind
  use general_sub2grid_mod, only: sub2grid_info
  use constants, only:max_varname_length
  type, extends(abstract_get_fv3_regional_ensperts_class) :: get_fv3_regional_ensperts_class
  contains
    procedure, pass(this) :: get_fv3_regional_ensperts => get_fv3_regional_ensperts_run
    procedure, pass(this) :: ens_spread_dualres_regional => ens_spread_dualres_regional_fv3_regional
    procedure, pass(this) :: general_read_fv3_regional
  end type get_fv3_regional_ensperts_class
    type(sub2grid_info):: grd_fv3lam_ens_dynvar_io_nouv,grd_fv3lam_ens_tracer_io_nouv,grd_fv3lam_ens_uv
    character(len=max_varname_length),allocatable,dimension(:) :: fv3lam_ens_io_dynmetvars3d_nouv 
                               ! copy of cvars3d excluding uv 3-d fields   
    character(len=max_varname_length),allocatable,dimension(:) :: fv3lam_ens_io_tracermetvars3d_nouv
                               ! copy of cvars3d excluding uv 3-d fields   
    character(len=max_varname_length),allocatable,dimension(:) :: fv3lam_ens_io_dynmetvars2d_nouv
    character(len=max_varname_length),allocatable,dimension(:) :: fv3lam_ens_io_tracermetvars2d_nouv
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
  !   2019-04-22  CAPS(C. Tong) - add direct reflectivity DA option
  !
  !   2021-08-10  lei     - modify for fv3-lam ensemble spread output
  !   2021-11-01  lei     - modify for fv3-lam parallel IO
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
     use hybrid_ensemble_parameters, only: write_ens_sprd
     use directDA_radaruse_mod, only: l_use_cvpqx, cvpqx_pval, cld_nt_updt
     use directDA_radaruse_mod, only: l_use_dbz_directDA
     use directDA_radaruse_mod, only: l_cvpnr
     use general_sub2grid_mod, only: sub2grid_info,general_sub2grid_create_info
     use gridmod,only: regional
     use gsi_rfv3io_mod, only: fv3lam_io_dynmetvars3d_nouv,fv3lam_io_tracermetvars3d_nouv 
     use gsi_rfv3io_mod, only: fv3lam_io_dynmetvars2d_nouv,fv3lam_io_tracermetvars2d_nouv 
     use netcdf   , only: nf90_open, nf90_close,nf90_nowrite,nf90_inquire,nf90_format_netcdf4
     use netcdf_mod , only: nc_check
    

     implicit none
     class(get_fv3_regional_ensperts_class), intent(inout) :: this
     type(gsi_bundle),allocatable, intent(inout) :: en_perts(:,:)
     integer(i_kind), intent(in   ):: nelen
     real(r_single),dimension(:,:,:),allocatable,intent(inout):: ps_bar
 
     real(r_kind),dimension(grd_ens%lat2,grd_ens%lon2,grd_ens%nsig):: u,v,tv,oz,rh
     real(r_kind),dimension(grd_ens%lat2,grd_ens%lon2):: ps
     real(r_kind),dimension(grd_ens%lat2,grd_ens%lon2,grd_ens%nsig)::w,ql,qi,qr,qg,qs,qnr
 
     real(r_single),pointer,dimension(:,:,:):: w3 =>NULL()
     real(r_single),pointer,dimension(:,:):: w2 =>NULL()
     real(r_kind),pointer,dimension(:,:,:):: x3 =>NULL()
     real(r_kind),pointer,dimension(:,:):: x2 =>NULL()
     type(gsi_bundle),allocatable,dimension(:):: en_bar
     type(gsi_grid):: grid_ens
     real(r_kind):: bar_norm,sig_norm,kapr,kap1

     character(len=64),dimension(:,:),allocatable:: names
     character(len=64),dimension(:,:),allocatable:: uvnames
     integer(i_kind),dimension(:,:),allocatable:: lnames
     integer(i_kind),dimension(:,:),allocatable:: uvlnames
 
     integer(i_kind):: i,j,k,n,mm1,istatus
     integer(i_kind):: ndynvario2d,ntracerio2d
     integer(r_kind):: ndynvario3d,ntracerio3d
     integer(i_kind):: inner_vars,numfields
     integer(i_kind):: ilev,ic2,ic3
     integer(i_kind):: m
     integer(i_kind)::loc_id,ncfmt

     
     character(255) ensfilenam_str
     type(type_fv3regfilenameg)::fv3_filename 

     call gsi_gridcreate(grid_ens,grd_ens%lat2,grd_ens%lon2,grd_ens%nsig)
     ! Allocate bundle to hold mean of ensemble members
     allocate(en_bar(ntlevs_ens))

!clt setup varnames for IO
     ndynvario2d=0
     ntracerio2d=0
     ndynvario3d=size(fv3lam_io_dynmetvars3d_nouv)
     ntracerio3d=size(fv3lam_io_tracermetvars3d_nouv)
     if (allocated(fv3lam_io_dynmetvars2d_nouv))then
       ndynvario2d=size(fv3lam_io_dynmetvars2d_nouv)
     endif
     if (allocated(fv3lam_io_tracermetvars2d_nouv)) then
       ntracerio2d=size(fv3lam_io_tracermetvars2d_nouv)
     endif
     allocate(fv3lam_ens_io_dynmetvars3d_nouv(ndynvario3d))
     allocate(fv3lam_ens_io_tracermetvars3d_nouv(ndynvario3d))
     fv3lam_ens_io_dynmetvars3d_nouv=fv3lam_io_dynmetvars3d_nouv
     fv3lam_ens_io_tracermetvars3d_nouv=fv3lam_io_tracermetvars3d_nouv
     if (ndynvario2d > 0 ) then
       allocate(fv3lam_ens_io_dynmetvars2d_nouv(ndynvario2d))
       fv3lam_ens_io_dynmetvars2d_nouv=fv3lam_io_dynmetvars2d_nouv
     endif
     if (ntracerio2d > 0 ) then
       allocate(fv3lam_ens_io_tracermetvars2d_nouv(ntracerio2d))
       fv3lam_ens_io_tracermetvars2d_nouv =fv3lam_io_tracermetvars3d_nouv 
     endif


     inner_vars=1
     numfields=inner_vars*(ndynvario3d*grd_ens%nsig+ndynvario2d)  
     allocate(lnames(1,numfields),names(1,numfields))
     ilev=1
     do i=1,ndynvario3d
       do k=1,grd_ens%nsig
         lnames(1,ilev)=k
         names(1,ilev)=fv3lam_ens_io_dynmetvars3d_nouv(i)
         ilev=ilev+1
       enddo
     enddo
     do i=1,ndynvario2d
       lnames(1,ilev)=1
       names(1,ilev)=fv3lam_ens_io_dynmetvars2d_nouv(i)
       ilev=ilev+1
     enddo
   

     call general_sub2grid_create_info(grd_fv3lam_ens_dynvar_io_nouv,inner_vars,grd_ens%nlat,&
          grd_ens%nlon,grd_ens%nsig,numfields,regional,names=names,lnames=lnames)

     inner_vars=1
     numfields=inner_vars*(ntracerio3d*grd_ens%nsig+ntracerio2d)  
     deallocate(lnames,names)
     allocate(lnames(1,numfields),names(1,numfields))
     ilev=1
     do i=1,ntracerio3d
      do k=1,grd_ens%nsig
        lnames(1,ilev)=k
        names(1,ilev)=fv3lam_ens_io_tracermetvars3d_nouv(i)
        ilev=ilev+1
      enddo
    enddo
    do i=1,ntracerio2d
       lnames(1,ilev)=1
       names(1,ilev)=fv3lam_ens_io_tracermetvars2d_nouv(i)
       ilev=ilev+1
    enddo
   

    call general_sub2grid_create_info(grd_fv3lam_ens_tracer_io_nouv,inner_vars,grd_ens%nlat,&
         grd_ens%nlon,grd_ens%nsig,numfields,regional,names=names,lnames=lnames)





    numfields=grd_ens%nsig
    inner_vars=2
    allocate(uvlnames(inner_vars,numfields),uvnames(inner_vars,numfields))
    ilev=1
    do k=1,grd_ens%nsig
      uvlnames(1,ilev)=k
      uvlnames(2,ilev)=k
      uvnames(1,ilev)='u'
      uvnames(2,ilev)='v'
      ilev=ilev+1
    enddo
    call general_sub2grid_create_info(grd_fv3lam_ens_uv,inner_vars,grd_ens%nlat,&
         grd_ens%nlon,grd_ens%nsig,numfields,regional,names=uvnames,lnames=uvlnames)

    deallocate(lnames,names,uvlnames,uvnames)


    do m=1,ntlevs_ens
       call gsi_bundlecreate(en_bar(m),grid_ens,'ensemble',istatus,names2d=cvars2d,names3d=cvars3d,bundle_kind=r_kind)
       if(istatus/=0) then
          write(6,*)' get_fv3_regional_ensperts_netcdf: trouble creating en_bar bundle'
          call stop2(9991)
       endif
    enddo ! for m 

     ! print info message for dirZDA
    if(mype==0)then
       if (l_use_cvpqx) then
         if ( cvpqx_pval == 0._r_kind ) then        ! CVlogq
             write(6,*) 'general_read_fv3_regional_dirZDA: convert qr/qs/qg to log transform.'
         else if ( cvpqx_pval > 0._r_kind ) then   ! CVpq
             write(6,*) 'general_read_fv3_regional_dirZDA: ',     &
                        'reset minimum of qr/qs/qg to specified values before power transform.' 
             write(6,*) 'general_read_fv3_regional_dirZDA: convert qr/qs/qg with power transform.'
         end if
       end if
       if ( cld_nt_updt > 0 .and. l_cvpnr) then
         write(6,*) 'general_read_fv3_regional_dirZDA: ',     &
                    'reset minimum of qnr to a specified value before power transform'
         write(6,*) 'general_read_fv3_regional_dirZDA: convert qnr with power transform .'
       end if
    end if


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

           if(mype == 0) then
              call nc_check(nf90_open(fv3_filename%dynvars,nf90_nowrite,loc_id), &
              "nf90 open ",trim(fv3_filename%dynvars))
              call nc_check(nf90_inquire(loc_id,formatNum=ncfmt), &
              "nf90_inquire formate of ",trim(fv3_filename%dynvars))
              if(ncfmt /= nf90_format_netcdf4) then
                 write(6,*) &
                 'the current GSI parallelization IO for fv3_lam only works for netcdf4' ,&
                  'ncfmt should be ', nf90_format_netcdf4,&
                  'GSI will stop while ',trim(fv3_filename%dynvars),' is ', ncfmt
                 call stop2(333)
              endif
              call nc_check(nf90_close(loc_id), &
              "nf90 close ",trim(fv3_filename%dynvars))

              call nc_check(nf90_open(fv3_filename%tracers,nf90_nowrite,loc_id), &
              "nf90 open ",trim(fv3_filename%tracers))
              call nc_check(nf90_inquire(loc_id,formatNum=ncfmt), &
              "nf90_inquire formate of ",trim(fv3_filename%tracers))
              if(ncfmt /= nf90_format_netcdf4) then
                 write(6,*) &
                 'the current GSI parallelization IO for fv3_lam only works for netcdf 4',&
                  'ncfmt should be ', nf90_format_netcdf4,&
                  'GSI will stop while ',trim(fv3_filename%tracers),' is ', ncfmt
                 call stop2(333)
              endif
              call nc_check(nf90_close(loc_id), &
              "nf90 close ",trim(fv3_filename%tracers))
           endif
 ! 
 ! READ ENEMBLE MEMBERS DATA
           if (mype == 0) write(6,'(a,a)') &
               'CALL READ_FV3_REGIONAL_ENSPERTS FOR ENS DATA with the filename str : ',trim(ensfilenam_str)
           if (.not.l_use_dbz_directDA ) then ! Read additional hydrometers and w for dirZDA
              call this%general_read_fv3_regional(fv3_filename,ps,u,v,tv,rh,oz) 
           else
              call this%general_read_fv3_regional(fv3_filename,ps,u,v,tv,rh,oz,   &
                                                  g_ql=ql,g_qi=qi,g_qr=qr,g_qs=qs,g_qg=qg,g_qnr=qnr,g_w=w)
           end if
 
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
 
! save additional ensemble varaible data for direct reflectivity DA

                 case('ql','QL')

                    do k=1,grd_ens%nsig
                       do i=1,grd_ens%lon2
                          do j=1,grd_ens%lat2
                             w3(j,i,k) = ql(j,i,k)
                             x3(j,i,k)=x3(j,i,k)+ql(j,i,k)
                          end do
                       end do
                    end do

                 case('qi','QI')

                    do k=1,grd_ens%nsig
                       do i=1,grd_ens%lon2
                          do j=1,grd_ens%lat2
                             w3(j,i,k) = qi(j,i,k)
                             x3(j,i,k)=x3(j,i,k)+qi(j,i,k)
                          end do
                       end do
                    end do

                 case('qr','QR')

                    do k=1,grd_ens%nsig
                       do i=1,grd_ens%lon2
                          do j=1,grd_ens%lat2
                             w3(j,i,k) = qr(j,i,k)
                             x3(j,i,k)=x3(j,i,k)+qr(j,i,k)
                          end do
                       end do
                    end do

                 case('qs','QS')

                    do k=1,grd_ens%nsig
                       do i=1,grd_ens%lon2
                          do j=1,grd_ens%lat2
                             w3(j,i,k) = qs(j,i,k)
                             x3(j,i,k)=x3(j,i,k)+qs(j,i,k)
                          end do
                       end do
                    end do

                 case('qg','QG')

                    do k=1,grd_ens%nsig
                       do i=1,grd_ens%lon2
                          do j=1,grd_ens%lat2
                             w3(j,i,k) = qg(j,i,k)
                             x3(j,i,k)=x3(j,i,k)+qg(j,i,k)
                          end do
                       end do
                    end do

                 case('qnr','QNR')

                      do k=1,grd_ens%nsig
                         do i=1,grd_ens%lon2
                            do j=1,grd_ens%lat2
                               if ( l_use_dbz_directDA ) then ! direct reflectivity DA
                                  if ( cld_nt_updt > 0 ) then ! Update Nc 
                                     w3(j,i,k) = qnr(j,i,k)
                                     x3(j,i,k)=x3(j,i,k)+qnr(j,i,k)
                                  end if
                               else     ! .not. l_use_dbz_directDA
                                  w3(j,i,k) = qnr(j,i,k)
                                  x3(j,i,k)=x3(j,i,k)+qnr(j,i,k)
                               end if
                            end do
                         end do
                      end do

                 case('w','W')
                    do k=1,grd_ens%nsig
                       do i=1,grd_ens%lon2
                          do j=1,grd_ens%lat2
                             w3(j,i,k) = w(j,i,k)
                             x3(j,i,k)=x3(j,i,k)+w(j,i,k)
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
 !
 ! CONVERT ENSEMBLE MEMBERS TO ENSEMBLE PERTURBATIONS
        sig_norm=sqrt(one/max(one,n_ens-one))
 
        do n=1,n_ens
           do i=1,nelen
              en_perts(n,m)%valuesr4(i)=(en_perts(n,m)%valuesr4(i)-en_bar(m)%values(i))*sig_norm
           end do
        end do

    enddo ! it 4d loop
 ! CALCULATE ENSEMBLE SPREAD
    write_ens_sprd=.true.
    if(write_ens_sprd ) then
        call this%ens_spread_dualres_regional(mype,en_perts,nelen)
        call mpi_barrier(mpi_comm_world,ierror) ! do we need this mpi_barrier here? 
    endif
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
  
  subroutine general_read_fv3_regional(this,fv3_filenameginput,g_ps,g_u,g_v,g_tv,g_rh,g_oz, &
                                        g_ql,g_qi,g_qr,g_qs,g_qg,g_qnr,g_w)
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
    use netcdf, only: nf90_format_netcdf4
    use kinds, only: r_kind,r_single,i_kind
    use gridmod, only: eta1_ll,eta2_ll
    use constants, only: zero,one,fv,zero_single,one_tenth,h300
    use hybrid_ensemble_parameters, only: grd_ens,q_hyb_ens
    use hybrid_ensemble_parameters, only: fv3sar_ensemble_opt 

    use mpimod, only: mpi_comm_world,mpi_rtype
    use gsi_rfv3io_mod,only: type_fv3regfilenameg
    use gsi_rfv3io_mod,only:n2d 
    use constants, only: half,zero
    use gsi_rfv3io_mod, only: gsi_fv3ncdf_read 
    use gsi_rfv3io_mod, only: gsi_fv3ncdf_read_v1
    use gsi_rfv3io_mod, only: gsi_fv3ncdf_readuv
    use gsi_rfv3io_mod, only: gsi_fv3ncdf_readuv_v1
    use gsi_rfv3io_mod, only: gsi_fv3ncdf2d_read_v1
    use directDA_radaruse_mod, only: l_use_dbz_directDA
    use gsi_bundlemod, only: gsi_bundle
    use gsi_bundlemod, only: gsi_gridcreate
    use gsi_bundlemod, only: gsi_grid
    use gsi_bundlemod, only: gsi_bundlecreate,gsi_bundledestroy
    use gsi_bundlemod, only: gsi_bundlegetvar
    use hybrid_ensemble_parameters, only: grd_ens
    use directDA_radaruse_mod, only: l_use_cvpqx, cvpqx_pval, cld_nt_updt
    use directDA_radaruse_mod, only: l_cvpnr, cvpnr_pval



    implicit none
!
! Declare passed variables
    class(get_fv3_regional_ensperts_class), intent(inout) :: this
    type (type_fv3regfilenameg)                  , intent (in)   :: fv3_filenameginput
    real(r_kind),dimension(grd_ens%lat2,grd_ens%lon2,grd_ens%nsig),intent(out)::g_u,g_v,g_tv,g_rh,g_oz
    real(r_kind),dimension(grd_ens%lat2,grd_ens%lon2,grd_ens%nsig),optional,intent(out)::g_ql,g_qi,g_qr
    real(r_kind),dimension(grd_ens%lat2,grd_ens%lon2,grd_ens%nsig),optional,intent(out)::g_qs,g_qg,g_qnr,g_w

    real(r_kind),dimension(grd_ens%lat2,grd_ens%lon2),intent(out):: g_ps
    real(r_kind),dimension(grd_ens%lat2,grd_ens%lon2,grd_ens%nsig+1) ::g_prsi 
    real(r_kind),dimension(grd_ens%lat2,grd_ens%lon2,grd_ens%nsig) ::g_prsl ,g_tsen,g_q,g_delp
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
    type(gsi_bundle) :: gsibundle_fv3lam_ens_dynvar_nouv
    type(gsi_bundle) :: gsibundle_fv3lam_ens_tracer_nouv
    type(gsi_grid):: grid_ens

    character(len=:),allocatable :: grid_spec !='fv3_grid_spec'            
    character(len=:),allocatable :: ak_bk     !='fv3_akbk'
    character(len=:),allocatable :: dynvars   !='fv3_dynvars'
    character(len=:),allocatable :: tracers   !='fv3_tracer'
    character(len=:),allocatable :: sfcdata   !='fv3_sfcdata'
    character(len=:),allocatable :: couplerres!='coupler.res'
    integer (i_kind) ier,istatus

    
    associate( this => this ) ! eliminates warning for unused dummy argument needed for binding
    end associate



    call gsi_gridcreate(grid_ens,grd_ens%lat2,grd_ens%lon2,grd_ens%nsig)
    grid_spec=fv3_filenameginput%grid_spec
    ak_bk=fv3_filenameginput%ak_bk
    dynvars=fv3_filenameginput%dynvars
    tracers=fv3_filenameginput%tracers
    sfcdata=fv3_filenameginput%sfcdata
    couplerres=fv3_filenameginput%couplerres


     
     
    if (allocated(fv3lam_ens_io_dynmetvars2d_nouv) ) then   
       call gsi_bundlecreate(gsibundle_fv3lam_ens_dynvar_nouv,grid_ens,'gsibundle_fv3lam_ens_dynvar_nouv', istatus,&
                names2d=fv3lam_ens_io_dynmetvars2d_nouv,names3d=fv3lam_ens_io_dynmetvars3d_nouv)
    else
       call gsi_bundlecreate(gsibundle_fv3lam_ens_dynvar_nouv,grid_ens,'gsibundle_fv3lam_ens_dynvar_nouv',istatus, &
                names3d=fv3lam_ens_io_dynmetvars3d_nouv)
    endif

     
    if (allocated(fv3lam_ens_io_tracermetvars2d_nouv) ) then   
       call gsi_bundlecreate(gsibundle_fv3lam_ens_tracer_nouv,grid_ens,'gsibundle_fv3lam_ens_tracer_nouv', istatus,&
                names2d=fv3lam_ens_io_tracermetvars2d_nouv,names3d=fv3lam_ens_io_tracermetvars3d_nouv)
    else
       call gsi_bundlecreate(gsibundle_fv3lam_ens_tracer_nouv,grid_ens,'gsibundle_fv3lam_ens_tracer_nouv',istatus, &
                names3d=fv3lam_ens_io_tracermetvars3d_nouv)
    endif
     
  


     
    if(fv3sar_ensemble_opt == 0 ) then  
      call gsi_fv3ncdf_readuv(grd_fv3lam_ens_uv,g_u,g_v,fv3_filenameginput)
    else
      call gsi_fv3ncdf_readuv_v1(grd_fv3lam_ens_uv,g_u,g_v,fv3_filenameginput)
    endif
    if(fv3sar_ensemble_opt == 0) then
      call gsi_fv3ncdf_read(grd_fv3lam_ens_dynvar_io_nouv,gsibundle_fv3lam_ens_dynvar_nouv,&
                            fv3_filenameginput%dynvars,fv3_filenameginput)
      call gsi_fv3ncdf_read(grd_fv3lam_ens_tracer_io_nouv,gsibundle_fv3lam_ens_tracer_nouv,&
                            fv3_filenameginput%tracers,fv3_filenameginput)
    else
      call gsi_fv3ncdf_read_v1(grd_fv3lam_ens_dynvar_io_nouv,gsibundle_fv3lam_ens_dynvar_nouv,&
                               fv3_filenameginput%dynvars,fv3_filenameginput)
      call gsi_fv3ncdf_read_v1(grd_fv3lam_ens_tracer_io_nouv,gsibundle_fv3lam_ens_tracer_nouv,&
                               fv3_filenameginput%tracers,fv3_filenameginput)
    endif
    ier=0
    call GSI_Bundlegetvar ( gsibundle_fv3lam_ens_dynvar_nouv, 'tsen' ,g_tsen ,istatus );ier=ier+istatus
    call GSI_Bundlegetvar ( gsibundle_fv3lam_ens_tracer_nouv, 'q'  ,g_q ,istatus );ier=ier+istatus
    call GSI_Bundlegetvar ( gsibundle_fv3lam_ens_tracer_nouv, 'oz'  ,g_oz ,istatus );ier=ier+istatus
    if (l_use_dbz_directDA) then
       call GSI_Bundlegetvar ( gsibundle_fv3lam_ens_tracer_nouv, 'ql' ,g_ql ,istatus );ier=ier+istatus
       call GSI_Bundlegetvar ( gsibundle_fv3lam_ens_tracer_nouv, 'qi' ,g_qi ,istatus );ier=ier+istatus
       call GSI_Bundlegetvar ( gsibundle_fv3lam_ens_tracer_nouv, 'qr' ,g_qr ,istatus );ier=ier+istatus
       call GSI_Bundlegetvar ( gsibundle_fv3lam_ens_tracer_nouv, 'qs' ,g_qs ,istatus );ier=ier+istatus
       call GSI_Bundlegetvar ( gsibundle_fv3lam_ens_tracer_nouv, 'qg' ,g_qg ,istatus );ier=ier+istatus
       call GSI_Bundlegetvar ( gsibundle_fv3lam_ens_tracer_nouv, 'qnr',g_qnr ,istatus );ier=ier+istatus
       call GSI_Bundlegetvar ( gsibundle_fv3lam_ens_dynvar_nouv, 'w' , g_w ,istatus );ier=ier+istatus
    end if
    

    if (fv3sar_ensemble_opt == 0) then 
      call GSI_Bundlegetvar ( gsibundle_fv3lam_ens_dynvar_nouv, 'delp'  ,g_delp ,istatus );ier=ier+istatus
      g_prsi(:,:,grd_ens%nsig+1)=eta1_ll(grd_ens%nsig+1) !thinkto be done , should use eta1_ll from ensemble grid
      do i=grd_ens%nsig,1,-1
         g_prsi(:,:,i)=g_delp(:,:,i)*0.001_r_kind+g_prsi(:,:,i+1)
      enddo
      g_ps(:,:)=g_prsi(:,:,1)
    else  ! for the ensemble processed frm CHGRES
      call GSI_Bundlegetvar ( gsibundle_fv3lam_ens_dynvar_nouv, 'ps'  ,g_ps ,istatus );ier=ier+istatus
      g_ps=g_ps*0.001_r_kind
      do k=1,grd_ens%nsig+1
        g_prsi(:,:,k)=eta1_ll(k)+eta2_ll(k)*g_ps
      enddo

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


! CV transform
    do k=1,grd_ens%nsig
       do i=1,grd_ens%lon2
          do j=1,grd_ens%lat2
             if (l_use_cvpqx) then
                ! Qr/qr
                if (g_qr(j,i,k) <= 1.0E-5_r_kind) then
                    g_qr(j,i,k) = 1.0E-5_r_kind
                end if
                if (cvpqx_pval > 0.0_r_kind ) then !CVpq
                    g_qr(j,i,k)=((g_qr(j,i,k)**cvpqx_pval)-1)/cvpqx_pval
                else  ! CVlogq
                    g_qr(j,i,k) = log(g_qr(j,i,k))
                end if
                ! Qs/qs
                if (g_qs(j,i,k) <= 1.0E-5_r_kind) then
                    g_qs(j,i,k) = 1.0E-5_r_kind
                end if
                if (cvpqx_pval > 0.0_r_kind ) then !CVpq
                    g_qs(j,i,k)=((g_qs(j,i,k)**cvpqx_pval)-1)/cvpqx_pval
                else  ! CVlogq
                    g_qs(j,i,k) = log(g_qs(j,i,k))
                end if
                ! Qg/qg
                if (g_qg(j,i,k) <= 1.0E-5_r_kind) then
                   g_qg(j,i,k) = 1.0E-5_r_kind
                end if
                if (cvpqx_pval > 0.0_r_kind ) then !CVpq
                   g_qg(j,i,k)=((g_qg(j,i,k)**cvpqx_pval)-1)/cvpqx_pval
                else  ! CVlogq
                   g_qg(j,i,k) = log(g_qg(j,i,k))
                end if
             end if
             if ( cld_nt_updt > 0 .and. l_cvpnr) then ! CVpnr
                ! Qnr/qnr
                if (g_qnr(j,i,k) < one) then
                   g_qnr(j,i,k) = one
                end if
                g_qnr(j,i,k)=((g_qnr(j,i,k)**cvpnr_pval)-1)/cvpnr_pval
             end if
          enddo
       enddo
    enddo
    call gsi_bundledestroy(gsibundle_fv3lam_ens_dynvar_nouv)
    call gsi_bundledestroy(gsibundle_fv3lam_ens_tracer_nouv)


  return       
  end subroutine general_read_fv3_regional

  subroutine ens_spread_dualres_regional_fv3_regional(this,mype,en_perts,nelen)
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
  !   2021-08-10  lei     - modify for fv3-lam ensemble option
  !   input argument list:
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
    use hybrid_ensemble_parameters, only: n_ens,grd_ens,grd_anl
    use general_sub2grid_mod, only: sub2grid_info,general_sub2grid_create_info,general_sube2suba
    use constants, only:  zero,two,half,one
    use control_vectors, only: cvars2d,cvars3d
    use gsi_bundlemod, only: gsi_bundlecreate
    use gsi_bundlemod, only: gsi_grid
    use gsi_bundlemod, only: gsi_bundle
    use gsi_bundlemod, only: gsi_bundlegetpointer
    use gsi_bundlemod, only: gsi_bundledestroy
    use gsi_bundlemod, only: gsi_gridcreate
    use write_fv3_enspread_mod, only: write_fv3_enspread
    implicit none

    class(get_fv3_regional_ensperts_class), intent(inout) :: this
    integer(i_kind),intent(in):: mype
    type(gsi_bundle),allocatable, intent(in   ) :: en_perts(:,:)
    integer(i_kind), intent(in   ):: nelen
  
    type(gsi_bundle):: sube,suba
    type(gsi_grid):: grid_ens,grid_anl
    real(r_kind) sp_norm

    integer(i_kind) i,n
    integer(i_kind) istatus
    character(255) enspreadfilename 

    associate( this => this ) ! eliminates warning for unused dummy argument needed for binding
    end associate
          if(mype==0) write(6,*) "Enter ens_spread_dualres_regional_fv3_regional"  
          enspreadfilename="fv3_enspread_regll"
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
  
         do n=1,n_ens
            do i=1,nelen
               sube%values(i)=sube%values(i) &
                 +(en_perts(n,1)%valuesr4(i))*(en_perts(n,1)%valuesr4(i))
            end do
         end do
     
         do i=1,nelen
           sube%values(i) = sqrt(sube%values(i))
         end do
  
         call write_fv3_enspread(grd_ens,enspreadfilename,sube,1)  !"1" as a placeholder
  
    return
  end subroutine ens_spread_dualres_regional_fv3_regional
end module get_fv3_regional_ensperts_mod
