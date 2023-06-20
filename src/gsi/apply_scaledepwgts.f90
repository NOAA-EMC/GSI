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
  use constants, only: zero,half,one,two,three,rearth,pi,tiny_r_kind
  use mpimod, only: mype
  use general_sub2grid_mod, only: general_sub2grid_create_info
  use egrid2agrid_mod,only: g_create_egrid2agrid
  use general_sub2grid_mod, only: sub2grid_info
  use hybrid_ensemble_parameters, only: nsclgrp
  use hybrid_ensemble_parameters, only: spc_multwgt,spcwgt_params,r_ensloccov4scl
  implicit none

  integer(i_kind),intent(in   ) :: jcap_in

  integer(i_kind) i
  integer(i_kind) ig
  real(r_kind) rwv0,rtem1,rtem2
  real (r_kind):: fwgtofwvlen
  real(r_kind) :: totwvlength
  logical :: l_sum_spc_weights

  ! Spectral scale decomposition is differernt between SDL-cross and SDL-nocross
  if( r_ensloccov4scl < tiny_r_kind )then
     l_sum_spc_weights = .false.
  else
     l_sum_spc_weights = .true.
  end if

  spc_multwgt(0,1)=one
  do ig=2,nsclgrp
    spc_multwgt(0,ig)=zero
  end do


  rwv0=2.0_r_kind*pi*rearth*0.001_r_kind
  do i=1,jcap_in
     totwvlength= rwv0/real(i)                   
     rtem1=zero
     do ig=1,nsclgrp
        if(ig /= 2) then
           spc_multwgt(i,ig)=fwgtofwvlen(spcwgt_params(1,ig),spcwgt_params(2,ig),&
                                         spcwgt_params(3,ig),spcwgt_params(4,ig),totwvlength)
           spc_multwgt(i,ig)=min(max(spc_multwgt(i,ig),zero),one)
           if(l_sum_spc_weights) then
              rtem1=rtem1+spc_multwgt(i,ig)
           else
              rtem1=rtem1+spc_multwgt(i,ig)*spc_multwgt(i,ig)
           endif
        endif
     enddo
     rtem2 =1.0_r_kind - rtem1
     if(rtem2 >= zero) then 
 
        if(l_sum_spc_weights) then
           spc_multwgt(i,2)=rtem2 
        else
           spc_multwgt(i,2)=sqrt(rtem2)
        endif
     else
        if(mype == 0)write(6,*) ' rtem2 < zero ',i,rtem2,(spc_multwgt(i,ig),ig=1,nsclgrp)
        spc_multwgt(i,2)=zero
     endif
  enddo
  
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
  use control_vectors, only: control_vector
  use kinds, only: r_kind,i_kind
  use kinds, only: r_single
  use general_specmod, only: general_spec_multwgt
  use gsi_bundlemod, only: gsi_bundle
  use general_sub2grid_mod, only: general_sub2grid,general_grid2sub   
  use general_specmod, only: spec_vars
  use general_sub2grid_mod, only: sub2grid_info
  use mpimod, only: mpi_comm_world,mype
  implicit none

! Declare passed variables
  type(gsi_bundle),intent(in) :: wbundle
  type(gsi_bundle),intent(inout) :: wbundle2
  type(spec_vars),intent (in):: sp_in
  type(sub2grid_info),intent(in)::grd_in
  real(r_kind),dimension(0:sp_in%jcap),intent(in):: spwgts

! Declare local variables
  integer(i_kind) kk

  real(r_kind),dimension(grd_in%nlat*grd_in%nlon*grd_in%nlevs_alloc)      :: hwork
  real(r_kind),dimension(grd_in%nlat,grd_in%nlon,grd_in%nlevs_alloc)      :: work
  real(r_kind),dimension(sp_in%nc):: spc1

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
