!$$$  program documentation block
! 
!  program history:
!
!  2018-03-28    T. Lei and D. Kleist - consoliated and added codes 
!                                       for the scale dependent scale localization scheme
!
!$$$ end documentation block

function fwgtofwvlen (rvlft,rvrgt,rcons,rlen,rinput)
!$$$  subprogram documentation block
! 
! subprogram: fwgtofwvlen
!  
! abstract: Calculation of spectral filter functions
!
!$$$ end documentation block

    use kinds, only: r_kind,i_kind,r_single
    implicit none

    real(r_kind),intent(in)  :: rvlft,rvrgt,rcons,rlen,rinput
    real(r_kind)  :: fwgtofwvlen
    real(r_kind)  :: rlen1,rtem1,rconshalf

    rlen1=rlen/10.0_r_kind ! rlen corresponds to a (-5,5) region
    rconshalf=0.5_r_kind*rcons
    if(rinput > rvlft .and. rinput < rvrgt) then
       fwgtofwvlen=rcons
    else
       rtem1=min(abs(rinput-rvlft),abs(rinput-rvrgt))
       fwgtofwvlen=rconshalf*(1.0_r_kind+tanh(5.0_r_kind-rtem1/rlen1))
    endif
       
end function fwgtofwvlen
!                .      .    .                                       .
subroutine init_mult_spc_wgts(jcap_in)
!$$$  subprogram documentation block
! 
! subprogram: init_mult_spc_wgts
!
!$$$ end documentation block

  use kinds, only: r_kind,i_kind,r_single
  use hybrid_ensemble_parameters,only: s_ens_hv,sp_loc,grd_ens,grd_loc,sp_ens
  use hybrid_ensemble_parameters,only: n_ens,p_sploc2ens,grd_sploc
  use hybrid_ensemble_parameters,only: use_localization_grid
  use gridmod,only: use_sp_eqspace
  use general_specmod, only: general_init_spec_vars
  use constants, only: zero,half,one,two,three,rearth,pi,tiny_r_kind
  use constants, only: rad2deg
  use mpimod, only: mype
  use general_sub2grid_mod, only: general_sub2grid_create_info
  use egrid2agrid_mod,only: g_create_egrid2agrid
  use general_sub2grid_mod, only: sub2grid_info
  use gsi_io, only: verbose
  use hybrid_ensemble_parameters, only: nsclgrp
  use hybrid_ensemble_parameters, only: spc_multwgt,spcwgt_params,r_ensloccov4scl
  implicit none

  integer(i_kind),intent(in   ) :: jcap_in
  real(r_kind),allocatable      :: totwvlength(:)

  integer(i_kind) i,ii,j,k,l,n,kk,nsigend
  integer(i_kind) ig
  real(r_kind) rwv0,rtem1,rtem2
  real (r_kind):: fwgtofwvlen
  integer(i_kind) :: l_sum_spc_weights

  ! Spectral scale decomposition is differernt between SDL-cross and SDL-nocross
  if( r_ensloccov4scl < tiny_r_kind )then
     l_sum_spc_weights = 1
  else
     l_sum_spc_weights = 0
  end if

  allocate(totwvlength(jcap_in))

  rwv0=2*pi*rearth*0.001_r_kind
  do i=1,jcap_in
     totwvlength(i)= rwv0/real(i)                   
  enddo
  do i=1,jcap_in
     rtem1=0
     do ig=1,nsclgrp
        if(ig /= 2) then
           spc_multwgt(i,ig)=fwgtofwvlen(spcwgt_params(1,ig),spcwgt_params(2,ig),&
                                         spcwgt_params(3,ig),spcwgt_params(4,ig),totwvlength(i))
           if(l_sum_spc_weights == 0 ) then
              rtem1=rtem1+spc_multwgt(i,ig)
           else
              rtem1=rtem1+spc_multwgt(i,ig)*spc_multwgt(i,ig)
           endif
        endif
     enddo
     rtem2 =1.0_r_kind - rtem1
     if(abs(rtem2) >= zero) then 
 
        if(l_sum_spc_weights == 0 ) then
           spc_multwgt(i,2)=rtem2 
        else
           spc_multwgt(i,2)=sqrt(rtem2)
        endif
     endif
  enddo
  spc_multwgt=max(spc_multwgt,0.0_r_kind)
  
  deallocate(totwvlength)
  return
end subroutine init_mult_spc_wgts

subroutine apply_scaledepwgts(grd_in,sp_in,wbundle,spwgts,wbundle2)  
!
!  Program history log:
!   2017-03-30  J. Kay, X. Wang - copied from Kleist's apply_scaledepwgts and
!                                 add the calculation of scale-dependent weighting for mixed resolution ensemble  
!                                 POC: xuguang.wang@ou.edu
!
  use constants, only:  one
  use control_vectors, only: nrf_var,cvars2d,cvars3d,control_vector
  use kinds, only: r_kind,i_kind
  use kinds, only: r_single
  use mpimod, only: mype,nvar_id,levs_id
  use hybrid_ensemble_parameters, only: oz_univ_static
  use general_specmod, only: general_spec_multwgt
  use gsi_bundlemod, only: gsi_bundle
  use general_sub2grid_mod, only: general_sub2grid,general_grid2sub   
  use general_specmod, only: spec_vars
  use general_sub2grid_mod, only: sub2grid_info
  use mpimod, only: mpi_comm_world,mype,npe,ierror
  use file_utility, only : get_lun
  implicit none

! Declare passed variables
  type(gsi_bundle),intent(in) :: wbundle
  type(gsi_bundle),intent(inout) :: wbundle2
  type(spec_vars),intent (in):: sp_in
  type(sub2grid_info),intent(in)::grd_in
  real(r_kind),dimension(0:sp_in%jcap),intent(in):: spwgts

! Declare local variables
  integer(i_kind) ii,kk
  integer(i_kind) i,j,lunit 

  real(r_kind),dimension(grd_in%lat2,grd_in%lon2):: slndt,sicet,sst
  real(r_kind),dimension(grd_in%nlat*grd_in%nlon*grd_in%nlevs_alloc)      :: hwork
  real(r_kind),dimension(grd_in%nlat,grd_in%nlon,grd_in%nlevs_alloc)      :: work
  real(r_kind),dimension(sp_in%nc):: spc1
  character*64 :: fname1
  character*5:: varname1

! Beta1 first
! Get from subdomains to
  call general_sub2grid(grd_in,wbundle%values,hwork)
  work=reshape(hwork,(/grd_in%nlat,grd_in%nlon,grd_in%nlevs_alloc/))

  do kk=1,grd_in%nlevs_alloc 
! Transform from physical space to spectral space   
     call general_g2s0(grd_in,sp_in,spc1,work(:,:,kk))

! Apply spectral weights
     call general_spec_multwgt(sp_in,spc1,spwgts)
! Transform back to physical space
     call general_s2g0(grd_in,sp_in,spc1,work(:,:,kk))

  end do

! Transfer work back to subdomains
  hwork=reshape(work,(/grd_in%nlat*grd_in%nlon*grd_in%nlevs_alloc/))
  call general_grid2sub(grd_in,hwork,wbundle2%values)    

  return
end subroutine apply_scaledepwgts
