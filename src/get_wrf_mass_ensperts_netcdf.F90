subroutine get_wrf_mass_ensperts_netcdf
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    get_wrf_mass_ensperts_netcdf  read arw model ensemble members
!   prgmmr: mizzi            org: ncar/mmm            date: 2010-08-11
!
! abstract: read ensemble members from the arw model in netcdf format, for use
!             with hybrid ensemble option.  ensemble spread is also written out as
!             a byproduct for diagnostic purposes.
!
!
! program history log:
!   2010-08-11  parrish, initial documentation
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

    use kinds, only: r_kind,i_kind,r_single
    use gridmod, only: nlat_regional,nlon_regional,nsig,eta1_ll,pt_ll,aeta1_ll
    use hybrid_ensemble_isotropic, only: st_en,vp_en,t_en,rh_en,oz_en,cw_en,p_en,sst_en
    use constants, only: zero,one,half,grav,fv,zero_single,rd_over_cp_mass,rd_over_cp,one_tenth
    use mpimod, only: mpi_comm_world,ierror,mype
    use hybrid_ensemble_parameters, only: n_ens,grd_ens,nlat_ens,nlon_ens,sp_ens

    implicit none

    real(r_kind),dimension(grd_ens%lat2,grd_ens%lon2,grd_ens%nsig):: vor,div,u,v,tv,cwmr,oz,rh
    real(r_kind),dimension(grd_ens%lat2,grd_ens%lon2):: ps
    real(r_kind),dimension(grd_ens%lat2,grd_ens%lon2,grd_ens%nsig):: u_sprd,v_sprd, &
       tv_sprd,rh_sprd,cwmr_sprd,oz_sprd
    real(r_kind),dimension(grd_ens%lat2,grd_ens%lon2):: ps_sprd
    real(r_kind),dimension(grd_ens%lat2,grd_ens%lon2,grd_ens%nsig,n_ens):: u_tmp,v_tmp,tv_tmp,rh_tmp
    real(r_kind),dimension(grd_ens%lat2,grd_ens%lon2,n_ens):: ps_tmp

    real(r_kind),dimension(grd_ens%latlon1n):: stbar,vpbar,tbar,rhbar,ozbar,cwbar
    real(r_kind),dimension(grd_ens%latlon11):: pbar,sstbar
    real(r_kind):: bar_norm,sig_norm,kapr,kap1

    real(r_kind),dimension(grd_ens%nlat,grd_ens%nlon,grd_ens%nsig):: u_wrk,v_wrk,tv_wrk,rh_wrk
    real(r_kind),dimension(grd_ens%nlat,grd_ens%nlon):: ps_wrk

    integer(i_kind):: iret,i,j,k,m,n,il,jl,mm1,apm_idx

    character(24) filename
    logical ice

!
! INITIALIZE ENSEMBLE MEAN ACCUMULATORS
    stbar=zero ; vpbar=zero ; tbar=zero ; rhbar=zero ; ozbar=zero ; cwbar=zero 
    pbar=zero ; sstbar =zero

    mm1=mype+1
    kap1=rd_over_cp+one
    kapr=one/rd_over_cp
!
! LOOP OVER ENSEMBLE MEMBERS 
    do n=1,n_ens
!
! DEFINE INPUT FILE NAME
       write(filename,"('wrf_mass_forecast.e',i3.3)") n
! 
! READ ENEMBLE MEMBERS DATA
       write(6,*) 'CALL READ_WRF_MASS_ENSPERTS FOR ENS DATA : ',filename
       call general_read_wrf_mass(filename,ps,vor,div,u,v,tv,rh,cwmr,oz,mype,iret, &
         u_wrk,v_wrk,tv_wrk,rh_wrk,ps_wrk)
!XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
!XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
!XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
                               ! dp test--1st member has been set equal to guess so can do direct
                               !          difference with each field
                             !  if(n==1) then
                             !     call compare_to_guess(ps,'ps',1)
                             !     call compare_to_guess(u,'u',nsig)
                             !     call compare_to_guess(v,'v',nsig)
                             !     call compare_to_guess(tv,'tv',nsig)
                             !     call compare_to_guess(rh,'rh',nsig)
                             !     call compare_to_guess(cwmr,'cw',nsig)
                             !     call compare_to_guess(oz,'oz',nsig)
                              !      call mpi_finalize(i)
                              !      stop
                             !  end if
!XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
!XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
!XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
!
! SAVE ENSEMBLE MEMBER DATA IN COLUMN VECTOR
       m=0
       do k=1,grd_ens%nsig
          do i=1,grd_ens%lon2
             do j=1,grd_ens%lat2
                m=m+1
                st_en(m,n) = u(j,i,k)
                vp_en(m,n) = v(j,i,k)
                t_en(m,n)  = tv(j,i,k)
                rh_en(m,n) = rh(j,i,k)
                oz_en(m,n) = oz(j,i,k)
                cw_en(m,n) = cwmr(j,i,k)

                stbar(m)=stbar(m)+u(j,i,k)
                vpbar(m)=vpbar(m)+v(j,i,k)
                tbar(m)= tbar(m)+ tv(j,i,k)
                rhbar(m)=rhbar(m)+rh(j,i,k)
                ozbar(m)=ozbar(m)+oz(j,i,k)
                cwbar(m)=cwbar(m)+cwmr(j,i,k)

!                if(k.eq.grd_ens%nsig/2.and.i.eq.grd_ens%lon2/2.and. \
!                j.eq.grd_ens%lat2/2) then
!                  apm_idx=m
!                  print *, 'APM: j,i,k ',j,i,k
!                  print *, 'APM: apm_idx ',apm_idx
!                  print *, 'APM: u ',u(j,i,k)
!                endif  
             enddo
          enddo
       enddo
                                    
! 
!       print *, 'APM: u-st_en ',st_en(apm_idx,n),n
!       print *, 'APM: v-st_en ',vp_en(apm_idx,n),n
!       print *, 'APM: t-st_en ',t_en(apm_idx,n),n
!       print *, 'APM: rh-st_en ',rh_en(apm_idx,n),n
!       print *, 'APM: cw-st_en ',oz_en(apm_idx,n),n
!       print *, 'APM: oz-st_en ',cw_en(apm_idx,n),n
!
!
! APM: Test printing of ensemble input fields
!       if(mype==0) then
!          i=grd_ens%lon2/2
!          j=grd_ens%lat2/2
!          k=grd_ens%nsig/2
!          print *, 'APM: lat2,lon2,sig2 ',grd_ens%lat2,grd_ens%lon2,grd_ens%nsig
!          print *, 'APM: j,i,k ',j,i,k
!          print *, 'APM: u ,n ',u(j,i,k),n
!          print *, 'APM: v ,n ',v(j,i,k),n
!          print *, 'APM: tv ,n ',tv(j,i,k),n
!          print *, 'APM: rh ,n ',rh(j,i,k),n
!          print *, 'APM: oz ,n ',oz(j,i,k),n
!          print *, 'APM: cwmr ,n ',cwmr(j,i,k),n
!          print *, 'APM: ps ,n ',ps(j,i),n
!       endif
!
       m=0
       do i=1,grd_ens%lon2
          do j=1,grd_ens%lat2
             m=m+1
             p_en(m,n) = ps(j,i)
             pbar(m)=pbar(m)+ ps(j,i)
          enddo
       enddo
!
! SAVE ENSEMBLE MEMBER DATA TO CALCULATE ENSEMBLE VARIANCE
       u_tmp(:,:,:,n)=u(:,:,:)
       v_tmp(:,:,:,n)=v(:,:,:)
       tv_tmp(:,:,:,n)=tv(:,:,:)
       rh_tmp(:,:,:,n)=rh(:,:,:)
       ps_tmp(:,:,n)=ps(:,:)
    enddo 
!
! CALCULATE ENSEMBLE SPREAD FOR TESTING
!    if(mype==0) then 
!       call ens_var_regional_3d(u_tmp,grd_ens%lat2,grd_ens%lon2,grd_ens%nsig,n_ens,u_sprd)    
!       call ens_var_regional_3d(v_tmp,grd_ens%lat2,grd_ens%lon2,grd_ens%nsig,n_ens,v_sprd)    
!       call ens_var_regional_3d(tv_tmp,grd_ens%lat2,grd_ens%lon2,grd_ens%nsig,n_ens,tv_sprd)    
!       call ens_var_regional_3d(rh_tmp,grd_ens%lat2,grd_ens%lon2,grd_ens%nsig,n_ens,rh_sprd)    
!       call ens_var_regional_2d(ps_tmp,grd_ens%lat2,grd_ens%lon2,n_ens,ps_sprd)    
!       i=grd_ens%lon2/2
!       j=grd_ens%lat2/2
!       k=grd_ens%nsig/2
!       print *, 'APM: lat,lon,sig ',grd_ens%lat2,grd_ens%lon2,grd_ens%nsig
!       print *, 'APM: j,i,k ',j,i,k
!       print *, 'APM: u_spread ',u_sprd(j,i,k)
!       print *, 'APM: v_spread ',v_sprd(j,i,k)
!       print *, 'APM: tv_spread ',tv_sprd(j,i,k)
!       print *, 'APM: rh_spread ',rh_sprd(j,i,k)
!       print *, 'APM: ps_spread ',ps_sprd(j,i)
!    endif
!
! CALCULATE ENSEMBLE MEAN
    bar_norm = one/float(n_ens)
    do i=1,grd_ens%latlon1n
       stbar(i)=stbar(i)*bar_norm
       vpbar(i)=vpbar(i)*bar_norm
       tbar(i) = tbar(i)*bar_norm
       rhbar(i)=rhbar(i)*bar_norm
       ozbar(i)=ozbar(i)*bar_norm
       cwbar(i)=cwbar(i)*bar_norm
    enddo
! 
!       print *, 'APM: u-bar ',stbar(apm_idx)
!       print *, 'APM: v-bar ',vpbar(apm_idx)
!       print *, 'APM: t-bar ',tbar(apm_idx)
!       print *, 'APM: rh-bar ',rhbar(apm_idx)
!       print *, 'APM: cw-bar ',ozbar(apm_idx)
!       print *, 'APM: oz-bar ',cwbar(apm_idx)
!
    do i=1,grd_ens%latlon11
       pbar(i)=pbar(i)*bar_norm
    enddo
    call mpi_barrier(mpi_comm_world,ierror)
!
! CALCULATE ENSEMBLE SPREAD
    call ens_spread_dualres_regional(stbar,vpbar,tbar,rhbar,ozbar,cwbar,pbar,mype)
    call mpi_barrier(mpi_comm_world,ierror)
!
! CONVERT ENSEMBLE MEMBERS TO ENSEMBLE PERTURBATIONS
    sig_norm=sqrt(one/max(one,n_ens-one))
    do n=1,n_ens
       do i=1,grd_ens%latlon1n
          st_en(i,n)=(st_en(i,n)-stbar(i))*sig_norm
          vp_en(i,n)=(vp_en(i,n)-vpbar(i))*sig_norm
          t_en(i,n) =( t_en(i,n)- tbar(i))*sig_norm
          rh_en(i,n)=(rh_en(i,n)-rhbar(i))*sig_norm
          oz_en(i,n)=(oz_en(i,n)-ozbar(i))*sig_norm
          cw_en(i,n)=(cw_en(i,n)-cwbar(i))*sig_norm
!          if (mype.eq.0) then
!          if (i.eq.1 .or. i.eq.grd_ens%latlon1n) then
!             print *, 'APM:wrf_read st_en, ens = ', n, ' indx = ', i, ' val = ', st_en(i,n)
!             print *, 'APM:wrf_read vp_en, ens = ', n, ' indx = ', i, ' val = ', vp_en(i,n)
!             print *, 'APM:wrf_read t_en, ens = ', n, ' indx = ', i, ' val = ', t_en(i,n)
!             print *, 'APM:wrf_read rh_en, ens = ', n, ' indx = ', i, ' val = ', rh_en(i,n)
!             print *, 'APM:wrf_read oz_en, ens = ', n, ' indx = ', i, ' val = ', oz_en(i,n)
!             print *, 'APM:wrf_read cw_en, ens = ', n, ' indx = ', i, ' val = ', cw_en(i,n)
!          endif
!          endif
       enddo
       do i=1,grd_ens%latlon11
          p_en(i,n)=(p_en(i,n)- pbar(i))*sig_norm
       enddo
    enddo
!
! IGNORE SST IN HYBRID
    do n=1,n_ens
       do i=1,grd_ens%latlon11
          sst_en(i,n)=zero
       enddo
    enddo
return
end subroutine get_wrf_mass_ensperts_netcdf

subroutine general_read_wrf_mass(filename,g_ps,g_vor,g_div,g_u,g_v,g_tv,g_rh,g_cwmr,g_oz,mype,iret, &
   gg_u,gg_v,gg_tv,gg_rh,gg_ps)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    general_read_wrf_mass  read arw model ensemble members
!   prgmmr: mizzi            org: ncar/mmm            date: 2010-08-11
!
! abstract: read ensemble members from the arw model in netcdf format, for use
!             with hybrid ensemble option.  ensemble spread is also written out as
!             a byproduct for diagnostic purposes.
!          PRESENTLY HARD WIRED TO WORK WITH SINGLE PROCESSOR.
! NOTE: OUTPUT SHOULD BE ON SUB_DOMAIN GRID WITH A ONE POINT HALO.
!
! ALSO READS WRFOUT FILES
! APPEAR TO BE PROBLEMS WITH WRFINPUT FILES
!
!
! program history log:
!   2010-08-11  parrish, initial documentation
!   2010-09-10  parrish, modify so ensemble variables are read in the same way as in
!                 subroutines convert_netcdf_mass and read_wrf_mass_binary_guess.
!                 There were substantial differences due to different opinion about what
!                 to use for surface pressure.  This issue should be resolved by coordinating
!                 with Ming Hu (ming.hu@noaa.gov).  At the moment, these changes result in
!                 agreement to single precision between this input method and the guess input
!                 procedure when the same file is read by both methods.
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

    use kinds, only: r_kind,r_single,i_kind
    use gridmod, only: nlat_regional,nlon_regional,nsig,eta1_ll,pt_ll,aeta1_ll
    use constants, only: zero,one,grav,fv,zero_single,rd_over_cp_mass,one_tenth,h300
    use hybrid_ensemble_parameters, only: n_ens,grd_ens
    use mpimod, only: mpi_comm_world,ierror,mpi_rtype,npe

    implicit none
!   include '/usr/local/netcdf/include/netcdf.inc'
!
! Declare passed variables
    real(r_kind),dimension(grd_ens%lat2,grd_ens%lon2,grd_ens%nsig),intent(out):: &
                                                  g_u,g_v,g_tv,g_rh,g_cwmr,g_oz
!                                                 g_vor,g_div,g_u,g_v,g_tv,g_rh,g_cwmr,g_oz
!   Remove intent(out) for g_vor and g_div since currently not used
    real(r_kind),dimension(grd_ens%lat2,grd_ens%lon2,grd_ens%nsig):: g_vor,g_div
    real(r_kind),dimension(grd_ens%lat2,grd_ens%lon2),intent(out):: g_ps
    character(24),intent(in):: filename
!
! Declare local parameters
    real(r_kind),parameter:: r0_01 = 0.01_r_kind
    real(r_kind),parameter:: r10   = 10.0_r_kind
    real(r_kind),parameter:: r100  = 100.0_r_kind
!
!   Declare local variables
    real(r_single),allocatable,dimension(:):: temp_1d
    real(r_single),allocatable,dimension(:,:):: temp_2d,temp_2d2
    real(r_single),allocatable,dimension(:,:,:):: temp_3d

    real(r_kind),allocatable,dimension(:):: p_top
    real(r_kind),allocatable,dimension(:,:):: psf,q_integral
    real(r_kind),allocatable,dimension(:,:,:):: tmp,tsn,uwd,vwd,qmr,qrl,qst,tmv,rlh,work

    real(r_kind),dimension(grd_ens%nlat,grd_ens%nlon,grd_ens%nsig):: gg_u,gg_v,gg_tv,gg_rh,gg_cwmr,gg_oz
    real(r_kind),dimension(grd_ens%nlat,grd_ens%nlon):: gg_ps

    real(r_kind),allocatable,dimension(:):: ps_wrk_fill_2d,u_wrk_fill_2d,v_wrk_fill_2d,tv_wrk_fill_2d, &
       rh_wrk_fill_2d,oz_wrk_fill_2d,cwmr_wrk_fill_2d
    real(r_kind),allocatable,dimension(:,:):: ps_wrk_fill_3d,u_wrk_fill_3d,v_wrk_fill_3d,tv_wrk_fill_3d, &
       rh_wrk_fill_3d,oz_wrk_fill_3d,cwmr_wrk_fill_3d

    integer(i_kind):: nx,ny,nz,i,j,k,d_max,iret,file_id,var_id,ndim,mype,mype_ps,mm1,icount,icount_prev
    integer(i_kind):: Time_id,s_n_id,w_e_id,b_t_id,s_n_stag_id,w_e_stag_id,b_t_stag_id
    integer(i_kind):: Time_len,s_n_len,w_e_len,b_t_len,s_n_stag_len,w_e_stag_len,b_t_stag_len
    integer(i_kind),allocatable,dimension(:):: dim,dim_id
    integer(i_kind) nf_inq_varndims,nf_inq_varid,nf_get_var_real,nf_inq_vardimid,nf_nowrite
    integer(i_kind) nf_open,nf_inq_dimlen,nf_inq_dimid

    real(r_kind):: pb,pt,del_p,p_tot,deltasigma
                                  real(r_kind) psfc_this_dry,psfc_this
                                  real(r_kind) work_prslk,work_prsl
                                 real(r_kind),allocatable::prsl(:,:,:)

    logical ice
#ifdef WRF
!
! OPEN ENSEMBLE MEMBER DATA FILE
    iret=nf_open(trim(filename),NF_NOWRITE,file_id)
    if(iret /= 0) then
       print *, 'ERROR OPENING WRF FORECAST FILE: ',filename
       stop
    endif
!    print *, 'in general_read_wrf_mass ',filename
!
! WRF FILE DIMENSIONS
    iret=nf_inq_dimid(file_id,'Time',Time_id)
    iret=nf_inq_dimid(file_id,'south_north',s_n_id)
    iret=nf_inq_dimid(file_id,'west_east',w_e_id)
    iret=nf_inq_dimid(file_id,'bottom_top',b_t_id)
    iret=nf_inq_dimid(file_id,'south_north_stag',s_n_stag_id)
    iret=nf_inq_dimid(file_id,'west_east_stag',w_e_stag_id)
    iret=nf_inq_dimid(file_id,'bottom_top_stag',b_t_stag_id)
    d_max=max(Time_id, s_n_id, w_e_id, b_t_id, s_n_stag_id, w_e_stag_id, b_t_stag_id)
    allocate(dim(d_max))
    dim(:)=-999

    iret=nf_inq_dimlen(file_id,Time_id,Time_len)
    iret=nf_inq_dimlen(file_id,s_n_id,s_n_len)
    iret=nf_inq_dimlen(file_id,w_e_id,w_e_len)
    iret=nf_inq_dimlen(file_id,b_t_id,b_t_len)
    iret=nf_inq_dimlen(file_id,s_n_stag_id,s_n_stag_len)
    iret=nf_inq_dimlen(file_id,w_e_stag_id,w_e_stag_len)
    iret=nf_inq_dimlen(file_id,b_t_stag_id,b_t_stag_len)    
    nx=w_e_len
    ny=s_n_len
    nz=b_t_len

    dim(Time_id)=Time_len
    dim(s_n_id)=s_n_len
    dim(w_e_id)=w_e_len
    dim(b_t_id)=b_t_len
    dim(s_n_stag_id)=s_n_stag_len
    dim(w_e_stag_id)=w_e_stag_len
    dim(b_t_stag_id)=b_t_stag_len
!
! OPEN FILE FOR PLOTTING WRF INPUT
    open(unit=3211,file='APM_WRF_INPUT',status='unknown',form='unformatted')
!
! READ PERTURBATION POTENTIAL TEMPERATURE (K)
!    print *, 'read T ',filename
    iret=nf_inq_varid(file_id,'T',var_id)

    iret=nf_inq_varndims(file_id,var_id,ndim)
    allocate(dim_id(ndim))

    iret=nf_inq_vardimid(file_id,var_id,dim_id)
    allocate(temp_3d(dim(dim_id(1)),dim(dim_id(2)),dim(dim_id(3))))

    iret=nf_get_var_real(file_id,var_id,temp_3d)
    if(iret /= 0) then
       print *, 'ERROR READING T ENSEMBLE DATA: ',filename
       stop
    endif
    allocate(tmp(dim(dim_id(2)),dim(dim_id(1)),dim(dim_id(3))))
    allocate(tsn(dim(dim_id(2)),dim(dim_id(1)),dim(dim_id(3))))
    do j=1,dim(dim_id(2))
       do i=1,dim(dim_id(1))
          tmp(j,i,:)=temp_3d(i,j,:)
       enddo
    enddo
!    print *, 'APM: tmp ',tmp(dim(dim_id(2))/2,dim(dim_id(1))/2, \
!    dim(dim_id(3))/2)
!    write(3211) temp_3d
!    print *, dim(dim_id(1)),dim(dim_id(2)),dim(dim_id(3))
    deallocate(temp_3d)
    deallocate(dim_id)

         !  READ MU, MUB, P_TOP  (construct psfc as done in gsi--gives different result compared to PSFC)

                     iret=nf_inq_varid(file_id,'P_TOP',var_id)

                     iret=nf_inq_varndims(file_id,var_id,ndim)
                     allocate(dim_id(ndim))

                     iret=nf_inq_vardimid(file_id,var_id,dim_id)
                     allocate(temp_1d(dim(dim_id(1))))

                     iret=nf_get_var_real(file_id,var_id,temp_1d)
                     if(iret /= 0) then
                        print *, 'ERROR READING P_TOP ENSEMBLE DATA: ',filename
                        stop
                     endif
                     allocate(p_top(dim(dim_id(1))))
                     do i=1,dim(dim_id(1))
                        p_top(i)=temp_1d(i)
                     enddo
                     deallocate(dim_id)

                     iret=nf_inq_varid(file_id,'MUB',var_id)
 
                     iret=nf_inq_varndims(file_id,var_id,ndim)
                     allocate(dim_id(ndim))
 
                     iret=nf_inq_vardimid(file_id,var_id,dim_id)
                     allocate(temp_2d(dim(dim_id(1)),dim(dim_id(2))))
 
                     iret=nf_get_var_real(file_id,var_id,temp_2d)
                     if(iret /= 0) then
                        print *, 'ERROR READING MUB ENSEMBLE DATA: ',filename
                        stop
                     endif
                     deallocate(dim_id)

                     iret=nf_inq_varid(file_id,'MU',var_id)
 
                     iret=nf_inq_varndims(file_id,var_id,ndim)
                     allocate(dim_id(ndim))
 
                     iret=nf_inq_vardimid(file_id,var_id,dim_id)
                     allocate(temp_2d2(dim(dim_id(1)),dim(dim_id(2))))
 
                     iret=nf_get_var_real(file_id,var_id,temp_2d2)
                     if(iret /= 0) then
                        print *, 'ERROR READING MU ENSEMBLE DATA: ',filename
                        stop
                     endif

                     allocate(psf(dim(dim_id(2)),dim(dim_id(1))))
                     do j=1,dim(dim_id(2))
                        do i=1,dim(dim_id(1))
                           temp_2d2(i,j)=temp_2d(i,j)+temp_2d2(i,j)+temp_1d(1)
                           psf(j,i)=temp_2d2(i,j)
                        enddo
                     enddo
                     deallocate(temp_2d,temp_2d2,temp_1d,dim_id)

!
! READ U (m/s)
!    print *, 'read U ',filename
    iret=nf_inq_varid(file_id,'U',var_id)

    iret=nf_inq_varndims(file_id,var_id,ndim)
    allocate(dim_id(ndim))

    iret=nf_inq_vardimid(file_id,var_id,dim_id)
    allocate(temp_3d(dim(dim_id(1)),dim(dim_id(2)),dim(dim_id(3))))

    iret=nf_get_var_real(file_id,var_id,temp_3d)
    if(iret /= 0) then
       print *, 'ERROR READING U ENSEMBLE DATA: ',filename
       stop
    endif
    allocate(uwd(dim(dim_id(2)),dim(dim_id(1))-1,dim(dim_id(3))))
!
! INTERPOLATE TO MASS GRID
    do k=1,dim(dim_id(3))
       do j=1,dim(dim_id(2))
          do i=1,dim(dim_id(1))-1
             uwd(j,i,k)=.5*(temp_3d(i,j,k)+temp_3d(i+1,j,k))
          enddo
       enddo
    enddo
!    print *, 'APM: uwd ',uwd(dim(dim_id(2))/2,(dim(dim_id(1))-1)/2,  \
!    dim(dim_id(3))/2)
!    write(3211) temp_3d
!    write(3211) uwd
!    print *, dim(dim_id(1)),dim(dim_id(2)),dim(dim_id(3))
    deallocate(temp_3d)
    deallocate(dim_id)
!
! READ V (m/s)
!    print *, 'read V ',filename
    iret=nf_inq_varid(file_id,'V',var_id)

    iret=nf_inq_varndims(file_id,var_id,ndim)
    allocate(dim_id(ndim))

    iret=nf_inq_vardimid(file_id,var_id,dim_id)
    allocate(temp_3d(dim(dim_id(1)),dim(dim_id(2)),dim(dim_id(3))))

    iret=nf_get_var_real(file_id,var_id,temp_3d)
    if(iret /= 0) then
       print *, 'ERROR READING V ENSEMBLE DATA: ',filename
       stop
    endif
    allocate(vwd(dim(dim_id(2))-1,dim(dim_id(1)),dim(dim_id(3))))
!
! INTERPOLATE TO MASS GRID
    do k=1,dim(dim_id(3))
       do j=1,dim(dim_id(2))-1
          do i=1,dim(dim_id(1))
             vwd(j,i,k)=.5*(temp_3d(i,j,k)+temp_3d(i,j+1,k))
          enddo
       enddo
    enddo
!    print *, 'APM: vwd ',vwd((dim(dim_id(2))-1)/2,dim(dim_id(1))/2,    \
!    dim(dim_id(3))/2)
!    write(3211) temp_3d
!    write(3211) vwd
!    print *, dim(dim_id(1)),dim(dim_id(2)),dim(dim_id(3))
    deallocate(temp_3d)
    deallocate(dim_id)
!
! READ QVAPOR (kg/kg)
!    print *, 'read QVAPOR ',filename
    iret=nf_inq_varid(file_id,'QVAPOR',var_id)

    iret=nf_inq_varndims(file_id,var_id,ndim)
    allocate(dim_id(ndim))

    iret=nf_inq_vardimid(file_id,var_id,dim_id)
    allocate(temp_3d(dim(dim_id(1)),dim(dim_id(2)),dim(dim_id(3))))

    iret=nf_get_var_real(file_id,var_id,temp_3d)
    if(iret /= 0) then
       print *, 'ERROR READING QVAPOR ENSEMBLE DATA: ',filename
       stop
    endif
    allocate(qmr(dim(dim_id(2)),dim(dim_id(1)),dim(dim_id(3))))
    allocate(qrl(dim(dim_id(2)),dim(dim_id(1)),dim(dim_id(3))))
    do j=1,dim(dim_id(2))
       do i=1,dim(dim_id(1))
          qmr(j,i,:)=temp_3d(i,j,:)
       enddo
    enddo
!    print *, 'APM: qmr ',qmr(dim(dim_id(2))/2,dim(dim_id(1))/2,     \
!    dim(dim_id(3))/2)
!    write(3211) temp_3d
!    print *, dim(dim_id(1)),dim(dim_id(2)),dim(dim_id(3))
    deallocate(temp_3d)
    deallocate(dim_id)
!
! CALCULATE TOTAL POTENTIAL TEMPERATURE (K)
!    print *, 'calculate total temperature ',filename
    do i=1,nx
       do j=1,ny
          do k=1,nz
            tmp(j,i,k)=tmp(j,i,k)+h300
          enddo
       enddo
    enddo   
!
! INTEGRATE {1 + WATER VAPOR} TO CONVERT DRY AIR PRESSURE    
!    print *, 'integrate 1 + q vertically ',filename
    allocate(q_integral(ny,nx))
    q_integral(:,:)=one
    do i=1,nx
       do j=1,ny
          do k=1,nz
             deltasigma=eta1_ll(k)-eta1_ll(k+1)
             q_integral(j,i)=q_integral(j,i)+deltasigma*qmr(j,i,k)
          enddo
       enddo
    enddo
!
! CONVERT WATER VAPOR MIXING RATIO TO SPECIFIC HUMIDITY
!    print *, 'convert qmr ',filename
    do i=1,nx
       do j=1,ny
          do k=1,nz
             qrl(j,i,k)=qmr(j,i,k)/(one+qmr(j,i,k))
          enddo
       enddo
    enddo

!  obtaining psfc as done in subroutine read_wrf_mass_netcdf_guess
    do i=1,nx
       do j=1,ny
          psfc_this_dry=r0_01*psf(j,i)
          psfc_this=(psfc_this_dry-pt_ll)*q_integral(j,i)+pt_ll
          psf(j,i)=one_tenth*psfc_this  ! convert from mb to cb
       end do
    end do
!
! CONVERT POTENTIAL TEMPERATURE TO VIRTUAL TEMPERATURE
!    print *, 'convert potential temp to virtual temp ',filename
    allocate(tmv(ny,nx,nz))      
    allocate(prsl(ny,nx,nz))
    do k=1,nz
       do i=1,nx
          do j=1,ny
             work_prsl  = one_tenth*(aeta1_ll(k)*(r10*psf(j,i)-pt_ll)+pt_ll)
             prsl(j,i,k)=work_prsl
             work_prslk = (work_prsl/r100)**rd_over_cp_mass
             tsn(j,i,k)     = tmp(j,i,k)*work_prslk
             tmv(j,i,k) = tsn(j,i,k) * (one+fv*qrl(j,i,k))
             tsn(j,i,k)= tmv(j,i,k)/(one+fv*max(zero,qrl(j,i,k)))
          end do
       end do
    end do

!
! CALCULATE RELATIVE HUMIDITY
!    print *, 'APM: calculate rh ',filename
    allocate(qst(ny,nx,nz))
    ice=.true. 

!    print *, 'APM: qrl ',     qrl(ny/2,nx/2,nz/2)
!    print *, 'APM: tsn ',     tsn(ny/2,nx/2,nz/2)
!    print *, 'APM: prs_tot ', prs_tot(ny/2,nx/2,nz/2)

    call genqsat2_regional(qrl,tsn,prsl,ice,qst,nx,ny,nz)
    deallocate(prsl)
    allocate(rlh(ny,nx,nz))
    do k=1,nz
       do i=1,nx
          do j=1,ny
             rlh(j,i,k)=qrl(j,i,k)/qst(j,i,k)
          enddo
       enddo
    enddo
!
! SAVE ENSEMBLE DATA FOR TRANSFER
!    print *, 'saving data for transfer ',filename
    do j=1,ny
       do i=1,nx
          do k=1,nz 
             gg_u(j,i,k)=uwd(j,i,k)
             gg_v(j,i,k)=vwd(j,i,k)
             gg_tv(j,i,k)=tmv(j,i,k)
             gg_rh(j,i,k)=rlh(j,i,k)
             gg_oz(j,i,k)=0.
             gg_cwmr(j,i,k)=0.
          enddo
       gg_ps(j,i)=psf(j,i)
       enddo
    enddo
!
! DEALLOCATE REMAINING TEMPORARY STORAGE
    deallocate(uwd,vwd,tmv,tmp,tsn,qmr,qrl,qst,rlh)
    deallocate(psf,q_integral)
!
! MULTIPLE PROCESSOR CODE TO TRANSFER DATA TO SUBGRID
    mm1=mype+1
    mype_ps=npe-1
!
! PS
    if(mype==mype_ps) then
       allocate(ps_wrk_fill_2d(grd_ens%itotsub))
       ps_wrk_fill_2d(:)=0.
!
! Output from fill_regional_2d is ps_wrk_fill_2d which is column vector
! with a halo.
       call fill_regional_2d(gg_ps,ny,nx,ps_wrk_fill_2d,grd_ens%itotsub)
    endif
    call mpi_scatterv(ps_wrk_fill_2d,grd_ens%ijn_s,grd_ens%displs_s,mpi_rtype, &
       g_ps,grd_ens%ijn_s(mm1),mpi_rtype,mype_ps,mpi_comm_world,ierror)       
!
!    print *, 'APM: grd_ens%ijn_s ',grd_ens%ijn_s 
!    print *, 'APM: grd_ens%displs_s ',grd_ens%displs_s 
!    print *, 'APM: grd_ens%ijn_s(mm1) ',grd_ens%ijn_s(mm1) 

!
!    if(mype==0) then
!       call comp_prt_2d(ps_wrk_fill_2d,gg_ps,nx,ny)
!    endif
!
! TV
    allocate(tv_wrk_fill_2d(grd_ens%itotsub))
    allocate(tv_wrk_fill_3d(grd_ens%lat2*grd_ens%lon2,max(2*grd_ens%nsig,npe)))
    tv_wrk_fill_2d(:)=0.
    tv_wrk_fill_3d(:,:)=0.
    icount=0
    icount_prev=1
    do k=1,nz
       icount=icount+1       
       if(mype==mod(icount-1,npe)) then
          call fill_regional_2d(gg_tv(:,:,k),ny,nx,tv_wrk_fill_2d,grd_ens%itotsub)
       endif
       if(mod(icount,npe)==0 .or. icount==nz) then
          call mpi_alltoallv(tv_wrk_fill_2d,grd_ens%ijn_s,grd_ens%displs_s,mpi_rtype, &
             tv_wrk_fill_3d(1,icount_prev),grd_ens%irc_s,grd_ens%ird_s,mpi_rtype, &
             mpi_comm_world,ierror)
          icount_prev=icount+1             
       endif
    enddo
!    if(mype==0) then
!       call comp_prt_3d(tv_wrk_fill_3d,gg_tv,nx,ny,nz)
!    endif
    call general_reload(grd_ens,tv_wrk_fill_3d,g_tv)
!
! U and V
    allocate(u_wrk_fill_2d(grd_ens%itotsub))
    allocate(u_wrk_fill_3d(grd_ens%lat2*grd_ens%lon2,max(2*grd_ens%nsig,npe)))
    allocate(v_wrk_fill_2d(grd_ens%itotsub))
    allocate(v_wrk_fill_3d(grd_ens%lat2*grd_ens%lon2,max(2*grd_ens%nsig,npe)))
    u_wrk_fill_2d(:)=0.
    v_wrk_fill_2d(:)=0.
    u_wrk_fill_3d(:,:)=0.
    v_wrk_fill_3d(:,:)=0.
    icount=0
    icount_prev=1
    do k=1,nz
       icount=icount+1
       if(mype==mod(icount-1,npe)) then
          call fill_regional_2d(gg_u(:,:,k),ny,nx,u_wrk_fill_2d,grd_ens%itotsub)
          call fill_regional_2d(gg_v(:,:,k),ny,nx,v_wrk_fill_2d,grd_ens%itotsub)
       endif
       if(mod(icount,npe)==0 .or. icount==nz) then
          call mpi_alltoallv(u_wrk_fill_2d,grd_ens%ijn_s,grd_ens%displs_s,mpi_rtype, &
             u_wrk_fill_3d(1,icount_prev),grd_ens%irc_s,grd_ens%ird_s,mpi_rtype, &
             mpi_comm_world,ierror)   
          call mpi_alltoallv(v_wrk_fill_2d,grd_ens%ijn_s,grd_ens%displs_s,mpi_rtype, &
             v_wrk_fill_3d(1,icount_prev),grd_ens%irc_s,grd_ens%ird_s,mpi_rtype, &
             mpi_comm_world,ierror)
          icount_prev=icount+1   
       endif
    enddo

    call general_reload(grd_ens,u_wrk_fill_3d,g_u)
    call general_reload(grd_ens,v_wrk_fill_3d,g_v)

!    print *, 'AFTER RELOAD'
!    j=grd_ens%lat2/2
!    i=grd_ens%lon2/2
!    k=grd_ens%nsig/2
!    print *,'j,i,k ',j,i,k
!    print *, 'APM: lat2,lon2,nsig ',grd_ens%lat2,grd_ens%lon2,grd_ens%nsig
!    print *, 'APM: g_u ',g_u(j,i,k)
!
! RH
    allocate(rh_wrk_fill_2d(grd_ens%itotsub))
    allocate(rh_wrk_fill_3d(grd_ens%lat2*grd_ens%lon2,max(2*grd_ens%nsig,npe)))
    rh_wrk_fill_2d(:)=0.
    rh_wrk_fill_3d(:,:)=0.
    icount=0
    icount_prev=1
!
! GSI specific humidity adjustment not included
    do k=1,nz
       icount=icount+1       
       if(mype==mod(icount-1,npe)) then
          call fill_regional_2d(gg_rh(:,:,k),ny,nx,rh_wrk_fill_2d,grd_ens%itotsub)
       endif
       if(mod(icount,npe)==0 .or. icount==nz) then
          call mpi_alltoallv(rh_wrk_fill_2d,grd_ens%ijn_s,grd_ens%displs_s,mpi_rtype, &
             rh_wrk_fill_3d(1,icount_prev),grd_ens%irc_s,grd_ens%ird_s,mpi_rtype, &
             mpi_comm_world,ierror)
          icount_prev=icount+1   
       endif
    enddo
    call general_reload(grd_ens,rh_wrk_fill_3d,g_rh)
!
! CWMR
    allocate(cwmr_wrk_fill_2d(grd_ens%itotsub))
    allocate(cwmr_wrk_fill_3d(grd_ens%lat2*grd_ens%lon2,max(2*grd_ens%nsig,npe)))
    cwmr_wrk_fill_2d(:)=0.
    cwmr_wrk_fill_3d(:,:)=0.
    icount=0
    icount_prev=1
    do k=1,nz
       icount=icount+1       
       if(mype==mod(icount-1,npe)) then
          call fill_regional_2d(gg_cwmr(:,:,k),ny,nx,cwmr_wrk_fill_2d,grd_ens%itotsub)
       endif
       if(mod(icount,npe)==0 .or. icount==nz) then
          call mpi_alltoallv(cwmr_wrk_fill_2d,grd_ens%ijn_s,grd_ens%displs_s,mpi_rtype, &
             cwmr_wrk_fill_3d(1,icount_prev),grd_ens%irc_s,grd_ens%ird_s,mpi_rtype, &
             mpi_comm_world,ierror)
          icount_prev=icount+1   
       endif
    enddo
    call general_reload(grd_ens,cwmr_wrk_fill_3d,g_cwmr)
!
! OZ
    allocate(oz_wrk_fill_2d(grd_ens%itotsub))
    allocate(oz_wrk_fill_3d(grd_ens%lat2*grd_ens%lon2,max(2*grd_ens%nsig,npe)))
    oz_wrk_fill_2d(:)=0.
    oz_wrk_fill_3d(:,:)=0.
    icount=0
    icount_prev=1
    do k=1,nz
       icount=icount+1       
       if(mype==mod(icount-1,npe)) then
          call fill_regional_2d(gg_oz(:,:,k),ny,nx,oz_wrk_fill_2d,grd_ens%itotsub)
       endif
       if(mod(icount,npe)==0 .or. icount==nz) then
          call mpi_alltoallv(oz_wrk_fill_2d,grd_ens%ijn_s,grd_ens%displs_s,mpi_rtype, &
             oz_wrk_fill_3d(1,icount_prev),grd_ens%irc_s,grd_ens%ird_s,mpi_rtype, &
             mpi_comm_world,ierror)
          icount_prev=icount+1   
       endif
    enddo
    call general_reload(grd_ens,oz_wrk_fill_3d,g_oz)
    print *, 'end of wrf read'
#endif /* WRF */

return       
end subroutine general_read_wrf_mass

subroutine comp_prt_3d(column,field,nx,ny,nz)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    comp_prt_3d   compare arrays and print out differences
!   prgmmr: mizzi            org: ncar/mmm            date: 2010-08-11
!
! abstract: for diagnostic purposes, compare input arrays column and field
!            and print out values/coordinates where array values are different.
!
!
! program history log:
!   2010-08-11  parrish, initial documentation
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

  use kinds, only: r_single,r_kind,i_kind
  use hybrid_ensemble_parameters, only: n_ens,grd_ens
  use mpimod, only: mpi_comm_world,mype,ierror,mpi_rtype,npe

  implicit none

!
! Declare passed variables
  real(r_kind),dimension((nx+2)*(ny+2),max(2*nz,npe)),intent(in):: column
  real(r_kind),dimension(ny,nx,nz),intent(in):: field
  integer(i_kind),intent(in):: nx,ny,nz
!
! Declare local parameters
  integer(i_kind):: nm,i,j,k,kk,kk_ind

  kk=0
  do k=1,2
     do i=1,nx+2
        do j=1,ny+2
           kk=kk+1
           if(j==1.or.j==ny+2.or.i==1.or.i==nx+2) then
!              print *, i,j,k,kk,column(kk,k)     
           else 
             if(column(kk,k)-field(j-1,i-1,k)/=0.) then
                print *, i,j,k,kk,column(kk,k),field(j-1,i-1,k),column(kk,k)-field(j-1,i-1,k)     
              endif
           endif
        enddo
     enddo
  enddo
return
end subroutine comp_prt_3d

subroutine comp_prt_2d(column,field,nx,ny)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    comp_prt_2d   compare arrays and print out differences
!   prgmmr: mizzi            org: ncar/mmm            date: 2010-08-11
!
! abstract: for diagnostic purposes, compare input arrays column and field
!            and print out values/coordinates where array values are different.
!
!
! program history log:
!   2010-08-11  parrish, initial documentation
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

  use kinds, only: r_single,r_kind,i_kind
  use hybrid_ensemble_parameters, only: n_ens,grd_ens
  use mpimod, only: mpi_comm_world,mype,ierror,mpi_rtype,npe

  implicit none

!
! Declare passed variables
  real(r_kind),dimension((nx+2)*(ny+2)),intent(in):: column
  real(r_kind),dimension(ny,nx),intent(in):: field
  integer(i_kind),intent(in):: nx,ny
!
! Declare local parameters
  integer(i_kind):: nm,i,j,k,kk,kk_ind

  kk=0
  do i=1,nx+2
     do j=1,ny+2
        kk=kk+1
        if(j==1.or.j==ny+2.or.i==1.or.i==nx+2) then
!           print *, i,j,kk,column(kk)     
        else 
           if(column(kk)-field(j-1,i-1)/=0.) then
              print *, i,j,kk,column(kk),field(j-1,i-1),column(kk)-field(j-1,i-1)     
           endif
        endif
     enddo
  enddo
return
end subroutine comp_prt_2d

subroutine ens_var_regional_3d(fld,ny,nx,nz,nm,fld_std)    
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    ens_var_regional_3d
!   prgmmr: mizzi            org: ncar/mmm            date: 2010-08-11
!
! abstract: 
!
!
! program history log:
!   2010-08-11  parrish, initial documentation
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
  use kinds, only: r_single,r_kind,i_kind
  use hybrid_ensemble_parameters, only: n_ens,grd_ens
  
  implicit none

  real(r_kind),dimension(ny,nx,nz,nm):: fld
  real(r_kind),dimension(ny,nx,nz):: fld_std
  real(r_kind),dimension(ny,nx,nz):: fld_bar,fld_var
  integer(i_kind):: kk,i,j,k,nx,ny,nz,nm
!
! INITIALIZE ACCUMULATORS
  fld_bar(:,:,:)=0.
  fld_var(:,:,:)=0.
!
! CALCULATE ENSEMBLE MEAN
  do kk=1,nm
     do i=2,nx-1
        do j=2,ny-1
           do k=1,nz
              fld_bar(j,i,k)=fld_bar(j,i,k)+fld(j,i,k,kk)/float(nm)
           enddo
        enddo
     enddo
  enddo
!       i=grd_ens%lon2/2
!       j=grd_ens%lat2/2
!       k=grd_ens%nsig/2
!       print *, 'APM: lat,lon,sig ',grd_ens%lat2,grd_ens%lon2,grd_ens%nsig
!       print *, 'APM: ny,nx,nz ',ny,nx,nz
!       print *, 'APM: j,i,k ',j,i,k
!       print *, 'APM: fld_mean ',fld_bar(j,i,k)
!
! CALCULATE ENSEMBLE VARIANCE
  do kk=1,nm
     do i=2,nx-1
        do j=2,ny-1
           do k=1,nz
              fld_var(j,i,k)=fld_var(j,i,k)+(fld(j,i,k,kk)-fld_bar(j,i,k))* &
                 (fld(j,i,k,kk)-fld_bar(j,i,k))/float(nm)
           enddo
        enddo
     enddo
  enddo
!
! CONVERT VARIANCE TO STANDARD DEVIATION
  fld_std(:,:,:)=sqrt(fld_var(:,:,:))
!       i=grd_ens%lon2/2
!       j=grd_ens%lat2/2
!       k=grd_ens%nsig/2
!       print *, 'APM: lat,lon,sig ',grd_ens%lat2,grd_ens%lon2,grd_ens%nsig
!       print *, 'APM: ny,nx,nz ',ny,nx,nz
!       print *, 'APM: j,i,k ',j,i,k
!       print *, 'APM: fld_std ',fld_std(j,i,k)

return
end subroutine ens_var_regional_3d

subroutine ens_var_regional_2d(fld,ny,nx,nm,fld_std)    
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    ens_var_regional_2d
!   prgmmr: mizzi            org: ncar/mmm            date: 2010-08-11
!
! abstract: 
!
!
! program history log:
!   2010-08-11  parrish, initial documentation
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
  use kinds, only: r_single,r_kind,i_kind
  use hybrid_ensemble_parameters, only: n_ens,grd_ens
  
  implicit none

  real(r_kind),dimension(ny,nx,nm):: fld
  real(r_kind),dimension(ny,nx):: fld_std
  real(r_kind),dimension(grd_ens%lat2,grd_ens%lon2):: fld_bar,fld_var
  integer(i_kind):: kk,i,j,k,nx,ny,nz,nm
!
! INITIALIZE ACCUMULATORS
  fld_bar(:,:)=0.
  fld_var(:,:)=0.
!
! CALCULATE ENSEMBLE MEAN
  do kk=1,nm
     do i=2,nx-1
        do j=2,ny-1
           fld_bar(j,i)=fld_bar(j,i)+fld(j,i,kk)/float(nm)
        enddo
     enddo
  enddo
!
! CALCULATE ENSEMBLE VARIANCE
  do kk=1,nm
     do i=2,nx-1
        do j=2,ny-1
           fld_var(j,i)=fld_var(j,i)+(fld(j,i,kk)-fld_bar(j,i))* &
              (fld(j,i,kk)-fld_bar(j,i))/float(nm)
        enddo
     enddo
  enddo
!
! CONVERT VARIANCE TO STANDARD DEVIATION
  fld_std(:,:)=sqrt(fld_var(:,:))

return
end subroutine ens_var_regional_2d

subroutine fill_regional_2d(fld_in,ny,nx,fld_out,itotsub)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    fill_regional_2d
!   prgmmr: mizzi            org: ncar/mmm            date: 2010-08-11
!
! abstract: 
!
!
! program history log:
!   2010-08-11  parrish, initial documentation
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
  use kinds, only: r_single,r_kind,i_kind
  use hybrid_ensemble_parameters, only: n_ens,grd_ens
  use mpimod, only: mpi_comm_world,mype,ierror,mpi_rtype,npe

  implicit none

  real(r_kind),dimension(ny,nx)::fld_in
  real(r_kind),dimension(itotsub)::fld_out
  integer(i_kind):: i,j,k,nx,ny,itotsub
!
! CHECK DIMENSIONS
  if(nx /= grd_ens%nlon) then
     print *, 'ERROR IN FILL_REGIONAL_2D: nx, grd_ens%nlon ',nx,grd_ens%nlon
     stop
  endif
  if(ny /= grd_ens%nlat) then
     print *, 'ERROR IN FILL_REGIONAL_2D: ny, grd_ens%nlat ',ny,grd_ens%nlat
     stop
  endif
  if(itotsub /= grd_ens%itotsub) then
     print *, 'ERROR IN FILL_REGIONAL_2D: itotsub, grd_ens%itotsub ',itotsub,grd_ens%itotsub
     stop
  endif
!
! SAVE DATA
!  print *, 'APM: ny,nx ',ny,nx
!  print *, 'APM: itotsub ',itotsub

  do k=1,grd_ens%itotsub
     i=grd_ens%ltosj_s(k)
     j=grd_ens%ltosi_s(k)
     fld_out(k)=fld_in(j,i)
!     print *, 'APM: j,i,k ',j,i,k
  enddo

return 
end subroutine fill_regional_2d

subroutine grd_reverse_3d(fld_inout,fld_work,ny,nx,nz)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    grd_reverse_3d   reverse ordering if longitude index
!   prgmmr: mizzi            org: ncar/mmm            date: 2010-08-11
!
! abstract: subroutine to reverse ordering of i(longitude) index (increase from east to west)
!
!
! program history log:
!   2010-08-11  parrish, initial documentation
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
  use kinds, only: r_single,r_kind,i_kind
  implicit none
  real(r_kind),dimension(ny,nx,nz):: fld_inout,fld_work
  integer(i_kind):: i,j,k,ii,nx,ny,nz
!
! SAVE THE INPUT ARRAY
  do j=1,ny
     do i=1,nx
        do k=1,nz
           fld_work(j,i,k)=fld_inout(j,i,k)
        enddo
     enddo
  enddo
!
! REVERSE ORDER OF INPUT ARRAY
  do j=1,ny
     do i=1,nx 
       ii=nx-i+1   
       do k=1,nz
           fld_inout(j,ii,k)=fld_work(j,i,k)
        enddo
     enddo
  enddo
return
end subroutine grd_reverse_3d

subroutine grd_reverse_2d(fld_inout,fld_work,ny,nx)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    grd_reverse_2d   reverse ordering if longitude index
!   prgmmr: mizzi            org: ncar/mmm            date: 2010-08-11
!
! abstract: subroutine to reverse ordering of i(longitude) index (increase from east to west)
!
!
! program history log:
!   2010-08-11  parrish, initial documentation
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
  use kinds, only: r_single,r_kind,i_kind
  implicit none
  real(r_kind),dimension(ny,nx):: fld_inout,fld_work
  integer(i_kind):: i,j,k,ii,nx,ny,nz
!
! SAVE THE INPUT ARRAY
  do j=1,ny
     do i=1,nx
        fld_work(j,i)=fld_inout(j,i)
     enddo
  enddo
!
! REVERSE ORDER OF INPUT ARRAY
  do j=1,ny
     do i=1,nx
     ii=nx-i+1   
        fld_inout(j,ii)=fld_work(j,i)
     enddo
  enddo
return
end subroutine grd_reverse_2d
        
subroutine halo_3d(fld_in,fld_out,ny,nx,nz)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    halo_3d   subroutine to add one point halo
!   prgmmr: mizzi            org: ncar/mmm            date: 2010-08-11
!
! abstract:   subroutine to add one point halo
!
!
! program history log:
!   2010-08-11  parrish, initial documentation
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
  use kinds, only: r_single,r_kind,i_kind
  implicit none
  real(r_kind),dimension(ny,nx,nz):: fld_in
  real(r_kind),dimension(ny+2,nx+2,nz):: fld_out
  integer(i_kind):: i,j,k,nx,ny,nz
!
  do i=2,nx+1
     do j=2,ny+1
        do k=1,nz
           fld_out(j,i,k)=fld_in(j-1,i-1,k)
        enddo
     enddo
  enddo
  do j=2,ny+1
     do k=1,nz
        fld_out(j,1,k)=fld_in(j-1,1,k)
        fld_out(j,nx+2,k)=fld_in(j-1,nx,k)
     enddo
  enddo
  do i=2,nx+1
     do k=1,nz
        fld_out(1,i,k)=fld_in(1,i-1,k)
        fld_out(ny+2,i,k)=fld_in(ny,i-1,k)
     enddo
  enddo
  do k=1,nz
     fld_out(1,1,k)=fld_in(1,1,k)
     fld_out(1,nx+2,k)=fld_in(1,nx,k)
     fld_out(ny+2,1,k)=fld_in(ny,1,k)
     fld_out(ny+2,nx+2,k)=fld_in(ny,nx,k)
  enddo
return
end subroutine halo_3d
        
subroutine halo_2d(fld_in,fld_out,ny,nx)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    halo_2d   subroutine to add one point halo
!   prgmmr: mizzi            org: ncar/mmm            date: 2010-08-11
!
! abstract:   subroutine to add one point halo
!
!
! program history log:
!   2010-08-11  parrish, initial documentation
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
  use kinds, only: r_single,r_kind,i_kind
  implicit none
  real(r_kind),dimension(ny,nx):: fld_in
  real(r_kind),dimension(ny+2,nx+2):: fld_out
  integer(i_kind):: i,j,k,nx,ny,nz
!
  do i=2,nx+1
     do j=2,ny+1
        fld_out(j,i)=fld_in(j-1,i-1)
     enddo
  enddo
  do j=2,ny+1
     fld_out(j,1)=fld_in(j-1,1)
     fld_out(j,nx+2)=fld_in(j-1,nx)
  enddo
  do i=2,nx+1
     fld_out(1,i)=fld_in(1,i-1)
     fld_out(ny+2,i)=fld_in(ny,i-1)
  enddo
  fld_out(1,1)=fld_in(1,1)
  fld_out(1,nx+2)=fld_in(1,nx)
  fld_out(ny+2,1)=fld_in(ny,1)
  fld_out(ny+2,nx+2)=fld_in(ny,nx)
return
end subroutine halo_2d

subroutine ens_spread_dualres_regional(stbar,vpbar,tbar,rhbar,ozbar,cwbar,pbar,mype)
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
!
  use kinds, only: r_single,r_kind,i_kind
  use hybrid_ensemble_parameters, only: n_ens,grd_ens,grd_anl,p_e2a,uv_hyb_ens
  use hybrid_ensemble_isotropic, only: st_en,vp_en,t_en,rh_en,oz_en,cw_en,p_en
  use general_sub2grid_mod, only: sub2grid_info,general_sub2grid_create_info,general_sube2suba_r_double
  use constants, only:  zero,two,half,one
  implicit none

  real(r_kind),dimension(grd_ens%latlon1n),intent(in):: stbar,vpbar,tbar,rhbar,ozbar,cwbar
  real(r_kind),dimension(grd_ens%latlon11),intent(in):: pbar
  integer(i_kind),intent(in):: mype

  real(r_kind),dimension(grd_ens%latlon11*(grd_ens%nsig*6+1)):: sube
  real(r_kind),dimension(grd_anl%latlon11*(grd_anl%nsig*6+1)):: suba
  real(r_kind) sp_norm
  type(sub2grid_info)::se,sa
!
! APM: addition
  integer(i_kind) j,k,apm_k,apm_idx

  integer(i_kind) i,ii,n
  integer(i_kind) ist,ivp,it,irh,ioz,icw,ip
  logical regional
  integer(i_kind) num_fields,inner_vars
  logical,allocatable::vector(:)

  sp_norm=(one/float(n_ens))

  sube=zero
!
  do n=1,n_ens
    ii=0
    do i=1,grd_ens%latlon1n
       ii=ii+1
       sube(ii) =sube(ii)  + (st_en(i,n)-stbar(i))*(st_en(i,n)-stbar(i)) 
    end do
    do i=1,grd_ens%latlon1n
       ii=ii+1
       sube(ii) =sube(ii)  + (vp_en(i,n)-vpbar(i))*(vp_en(i,n)-vpbar(i)) 
    end do
    do i=1,grd_ens%latlon1n
       ii=ii+1
       sube(ii) =sube(ii)  + ( t_en(i,n)- tbar(i))*( t_en(i,n)- tbar(i)) 
    end do
    do i=1,grd_ens%latlon1n
       ii=ii+1
       sube(ii) =sube(ii)  + (rh_en(i,n)-rhbar(i))*(rh_en(i,n)-rhbar(i)) 
    end do
    do i=1,grd_ens%latlon1n
       ii=ii+1
       sube(ii) =sube(ii)  + (oz_en(i,n)-ozbar(i))*(oz_en(i,n)-ozbar(i)) 
    end do
    do i=1,grd_ens%latlon1n
       ii=ii+1
       sube(ii) =sube(ii)  + (cw_en(i,n)-cwbar(i))*(cw_en(i,n)-cwbar(i)) 
    end do
    do i=1,grd_ens%latlon11
       ii=ii+1
       sube(ii) =sube(ii)  + ( p_en(i,n)- pbar(i))*( p_en(i,n)- pbar(i)) 
    end do
  end do
 
  do i=1,grd_ens%latlon11*(grd_ens%nsig*6+1)
    sube(i) = sqrt(sp_norm*sube(i))
  end do

  if(grd_ens%latlon1n == grd_anl%latlon1n) then
     suba=sube
  else
!
! APM: THIS BRANCH SHOULD NOT BE NEEDED IN REGIONAL MONO-RES CASE
!     regional=.false.
!     inner_vars=1
!     num_fields=6*grd_ens%nsig+1
!     allocate(vector(num_fields))
!     vector=.false.
!     vector(1:2*grd_ens%nsig)=uv_hyb_ens   !  assume here that 1st two 3d variables are either u,v or psi,chi
!     call general_sub2grid_create_info(se,inner_vars,grd_ens%nlat,grd_ens%nlon,grd_ens%nsig,num_fields, &
!                                       regional,vector)
!     call general_sub2grid_create_info(sa,inner_vars,grd_anl%nlat,grd_anl%nlon,grd_anl%nsig,num_fields, &
!                                       regional,vector)
!     deallocate(vector)
!     call general_sube2suba_r_double(se,sa,p_e2a,sube,suba,regional)
  end if

  ist=1
  ivp=grd_anl%latlon1n+ist
  it =grd_anl%latlon1n+ivp
  irh=grd_anl%latlon1n+it
  ioz=grd_anl%latlon1n+irh
  icw=grd_anl%latlon1n+ioz
  ip =grd_anl%latlon1n+icw
! 
! APM:test print of spread before call write_spread_dualres
!  print *, 'APM: anal latlon1n, latlon11 ',grd_anl%latlon1n, grd_anl%latlon11
!  print *, 'APM: ense latlon1n, latlon11 ',grd_ens%latlon1n, grd_ens%latlon11
!
!  do k=1,grd_ens%latlon11
!     i=grd_ens%ltosj_s(k)
!     j=grd_ens%ltosi_s(k)
!    print *, 'APM: j,i,k ',j,i,k
! APM: the -1 is because ltosj_s and ltosi_s are indexes on the nlat, nlon grid
!     if(i.eq.grd_ens%lon2/2-1 .and. j.eq.grd_ens%lat2/2-1) then
!        apm_k=k
!       print *, 'APM: j,i,k ',j,i,k
!     endif
!  enddo
! 
! print spread at selected point
! u spread
!  apm_idx=(grd_anl%nsig/2-1)*grd_anl%latlon11+apm_k
!  print *, 'apm_idx ',apm_idx
!  print *, 'APM: u spread calc ',suba(apm_idx)
! v spread
!  apm_idx=(grd_anl%nsig/2-1)*grd_anl%latlon11+apm_k
!  print *, 'APM: v spread calc ',suba(ivp+apm_idx-1)
! t spread
!  apm_idx=(grd_anl%nsig/2-1)*grd_anl%latlon11+apm_k
!  print *, 'APM: Tv spread calc ',suba(it+apm_idx-1)
! rh spread
!  apm_idx=(grd_anl%nsig/2-1)*grd_anl%latlon11+apm_k
!  print *, 'APM: RH spread calc ',suba(irh+apm_idx-1)
! oz spread
!  apm_idx=(grd_anl%nsig/2-1)*grd_anl%latlon11+apm_k
!  print *, 'APM: cw spread calc ',suba(ioz+apm_idx-1)
! icw spread
!  apm_idx=(grd_anl%nsig/2-1)*grd_anl%latlon11+apm_k
!  print *, 'APM: oz spread calc ',suba(icw+apm_idx-1)
! ps spread
!  apm_idx=apm_k
!  print *, 'APM: ps spread calc ',suba(ip+apm_idx-1)

  call write_spread_dualres(suba(ist),suba(ivp),suba(it),suba(irh),suba(ioz),suba(icw),suba(ip),mype)

  return
end subroutine ens_spread_dualres_regional

subroutine genqsat2_regional(q,tsen,prsl,ice,qsat,nx,ny,nz)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    genqsat2_dualres   adapt genqsat for general resolution
!   prgmmr: kleist           org: np22                date: 2010-01-05
!   revised by A.P.Mizzi to work on regional ensemble nx,ny,nz grid.
!
! abstract: compute qsat on ensemble grid.
!
!
! program history log:
!   2010-01-05  kleist, initial documentation
!   2010-02-28  parrish - make changes to allow dual resolution capability
!
!   input argument list:
!     q    - input specific humidity
!     tsen - input sensible temperature
!     prsl - input 3d pressure field at layer mid-points.
!     ice  - logical switch for ice phase
!
!   output argument list:
!     qsat - output saturation specific humidity
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$ end documentation block
  use kinds, only: r_single,r_kind,i_kind
  use constants, only: xai,tmix,xb,omeps,eps,xbi,one,zero,&
       xa,psat,ttp
  use hybrid_ensemble_parameters, only: grd_ens
  implicit none
  logical,intent(in):: ice

  real(r_kind),dimension(ny,nx,nz),intent(in):: q,tsen,prsl
  real(r_kind),dimension(ny,nx,nz),intent(inout):: qsat

  real(r_kind),dimension(ny,nx,nz):: dlnesdtv,dmax


  integer(i_kind) k,j,i,nx,ny,nz
  real(r_kind) pw,tdry,tr,es
  real(r_kind) w,onep3,esmax
  real(r_kind) desidt,deswdt,dwdt,desdt,esi,esw
  real(r_kind),dimension(ny,nx):: mint,estmax
  real(r_kind),dimension(nz)::maxrh
  integer(i_kind),dimension(ny,nx):: lmint

  onep3 = 1.e3_r_kind

  maxrh = zero
  lmint=1
  do j=1,nx
    do i=1,ny
      mint(i,j)=340._r_kind
    end do
  end do
  do k=1,nz
    do j=1,nx
      do i=1,ny
        if((prsl(i,j,k) < 30._r_kind .and.  &
            prsl(i,j,k) > 2._r_kind) .and.  &
            tsen(i,j,k) < mint(i,j))then
           lmint(i,j)=k
           mint(i,j)=tsen(i,j,k)
         end if
       end do
    end do
  end do
  do j=1,nx
    do i=1,ny
      tdry = mint(i,j)
      tr = ttp/tdry
      if (tdry >= ttp .or. .not. ice) then
        estmax(i,j) = psat * (tr**xa) * exp(xb*(one-tr))
      elseif (tdry < tmix) then
        estmax(i,j) = psat * (tr**xai) * exp(xbi*(one-tr))
      else
        w  = (tdry - tmix) / (ttp - tmix)
        estmax(i,j) =  w * psat * (tr**xa) * exp(xb*(one-tr)) &
                + (one-w) * psat * (tr**xai) * exp(xbi*(one-tr))
      endif
    end do
  end do
  if (ice) then
    do k = 1,nz
      do j = 1,nx
        do i = 1,ny

          pw = onep3*prsl(i,j,k)

          tdry = tsen(i,j,k)
          tr = ttp/tdry
          if (tdry >= ttp) then
            es = psat * (tr**xa) * exp(xb*(one-tr))
          elseif (tdry < tmix) then
            es = psat * (tr**xai) * exp(xbi*(one-tr))
          else
            w  = (tdry - tmix) / (ttp - tmix)
            es =  w * psat * (tr**xa) * exp(xb*(one-tr)) &
                  + (one-w) * psat * (tr**xai) * exp(xbi*(one-tr))
          endif

          esmax = es
          if(lmint(i,j) < k)then
             esmax=0.1_r_single*pw
             esmax=min(esmax,estmax(i,j))
          end if

          if(es <= esmax)then

            if (tdry >= ttp) then
               desdt = es * (-xa/tdry + xb*ttp/(tdry*tdry))
            elseif (tdry < tmix) then
               desdt = es * (-xai/tdry + xbi*ttp/(tdry*tdry))
            else
               esw = (psat * (tr**xa) * exp(xb*(one-tr)))
               esi = (psat * (tr**xai) * exp(xbi*(one-tr)))
               w  = (tdry - tmix) / (ttp - tmix)

               dwdt = one/(ttp-tmix)
               deswdt = esw * (-xa/tdry + xb*ttp/(tdry*tdry))
               desidt = esi * (-xai/tdry + xbi*ttp/(tdry*tdry))
               desdt = dwdt*esw + w*deswdt - dwdt*esi + (one-w)*desidt
            endif

            dlnesdtv(i,j,k) = desdt /es
            dmax(i,j,k) = one
          else
            es = esmax
            dlnesdtv(i,j,k) = zero
            dmax(i,j,k) = zero
          end if
          qsat(i,j,k) = eps * es / (pw - omeps * es)

        end do
      end do
    end do

! Compute saturation values with respect to water surface
  else
    do k = 1,nz
      do j = 1,nx
        do i = 1,ny

          pw = onep3*prsl(i,j,k)

          tdry = tsen(i,j,k)
          tr = ttp/tdry
          es = psat * (tr**xa) * exp(xb*(one-tr))
          esmax = es
          if(lmint(i,j) < k)then
             esmax=0.1_r_single*pw
             esmax=min(esmax,estmax(i,j))
          end if
          if(es <= esmax)then
            dlnesdtv(i,j,k) = (-xa/tdry + xb*ttp/(tdry*tdry))
            dmax(i,j,k) = one
          else
            dlnesdtv(i,j,k) = zero
            es = esmax
            dmax(i,j,k) = zero
          end if
          qsat(i,j,k) = eps * es / (pw - omeps * es)

        end do
      end do
    end do

  endif   ! end if ice
! write(6,*) (maxrh(k),k=1,nz)

  return
end subroutine genqsat2_regional

subroutine compare_to_guess(var,varname,nsig)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    compare_to_guess  check ensemble member against guess
!   prgmmr: parrish          org: np22                date: 2010-09-10
!
! abstract: diagnostic code to compare field read by general_read_wrf_mass 
!            against same field read by read_wrf_mass_guess.F90.
!
!
! program history log:
!   2010-09-10  parrish, initial documentation
!   2011-05-01  todling - cwmr no longer in guess-grids; use metguess bundle now
!
!   input argument list:
!     var     - input variable array to test
!     varname - name of input variable
!     nsig    - number of vertical levels
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$ end documentation block

  use kinds,only: r_kind,i_kind
  use constants, only: zero,one,fv
  use hybrid_ensemble_parameters, only: grd_ens,nlat_ens,nlon_ens,sp_ens
  use guess_grids, only: ntguessig,ges_u,ges_v,ges_tv,ges_q,ges_oz,ges_ps,ges_tsen,ges_prsl
  use jfunc, only: jiter,jiterstart,qoption
  use gsi_metguess_mod, only: gsi_metguess_bundle
  use gsi_bundlemod, only: gsi_bundlegetpointer
  use mpeu_util, only: die
  use mpimod, only: mype
  implicit none

  character(len=*),intent(in):: varname
  integer(i_kind),intent(in)::nsig
  real(r_kind),dimension(grd_ens%lat2,grd_ens%lon2,nsig),intent(in):: var
  real(r_kind),pointer,dimension(:,:,:):: ges_cwmr_it

  integer(i_kind) it,iderivative,i,j,k,istatus
  logical ice
  real(r_kind) work(grd_ens%lat2,grd_ens%lon2,nsig)
  real(r_kind) diffmax,varmax

  it=ntguessig

! Get pointer to could water mixing ratio
  call gsi_bundlegetpointer (gsi_metguess_bundle(it),'cw',ges_cwmr_it,istatus)
  if (istatus/=0) call die('compare_to_guess','cannot get pointer to cwmr, istatus =',istatus)

  select case (trim(varname))

     case('ps')
        work(:,:,1)=ges_ps(:,:,it)
     case('u')
        work(:,:,:)=ges_u(:,:,:,it)
     case('v')
        work(:,:,:)=ges_v(:,:,:,it)
     case('tv')
        work(:,:,:)=ges_tv(:,:,:,it)
     case('q')
        work(:,:,:)=ges_q(:,:,:,it)
     case('rh')
        iderivative = 0
        if(qoption == 1)then
            if(jiter == jiterstart)iderivative = 1
        else
            iderivative = 2
        end if
        ice=.true.
        call genqsat(work,ges_tsen(1,1,1,it),ges_prsl(1,1,1,it), &
                     grd_ens%lat2,grd_ens%lon2,nsig,ice,iderivative)
        do k=1,nsig
          do j=1,grd_ens%lon2
            do i=1,grd_ens%lat2
              work(i,j,k) = ges_q(i,j,k,it)/work(i,j,k)
            end do
          end do
        end do
     case('cw')
        work=ges_cwmr_it
     case('oz')
        work(:,:,:)=ges_oz(:,:,:,it)

  end select

  diffmax = maxval(abs(work-var))
  varmax=maxval(work)
  write(6,*)' compare ',trim(varname),', diffmax,ensmax,gesmax=',diffmax,maxval(var),varmax

  if(mype==0) then
      i=grd_ens%lat2/2
      j=grd_ens%lon2/2
      write(6,*)' single column from processor 0, i,j=',i,j,' for ',trim(varname)
      do k=1,nsig
          write(6,'(" k,ens,ges,ges-ens=",i5,3e14.5)')k,var(i,j,k),work(i,j,k),work(i,j,k)-var(i,j,k)
      end do
  end if

end subroutine compare_to_guess
