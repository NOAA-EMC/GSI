!$$$  subprogram documentation block
!28-03-2018    T. Lei and D. Kleist   consoliated and added codes 
!                                     for the scale dependent scale localization scheme
 function fwgtofwvlen (rvlft,rvrgt,rcons,rlen,rinput)
           use kinds, only: r_kind,i_kind,r_single
          implicit none
          real (r_kind):: fwgtofwvlen
          real(r_kind):: rvlft,rvrgt,rcons,rlen,rinput
          real (r_kind):: rlen1,rtem1,rconshalf
!          write(6,*)'rinput and ,rvlft rvlft is ',rinput,rvlft,rvrgt
          rlen1=rlen/10.0 ! rlen corresponds to a (-5,5) region
          rconshalf=0.5*rcons
          if(rinput.gt.rvlft.and.rinput.lt.rvrgt) then
              fwgtofwvlen=rcons
          else
              rtem1=min(abs(rinput-rvlft),abs(rinput-rvrgt))
              fwgtofwvlen=rconshalf*(1.0+tanh(5.0-rtem1/rlen1))
          endif
        
       
 end function fwgtofwvlen
!                .      .    .                                       .
 subroutine init_mult_spc_wgts(jcap_in)
  use kinds, only: r_kind,i_kind,r_single
  use hybrid_ensemble_parameters,only: s_ens_hv,sp_loc,grd_ens,grd_loc,sp_ens,n_ens,p_sploc2ens,grd_sploc
  use hybrid_ensemble_parameters,only: use_localization_grid
  use gridmod,only: use_sp_eqspace
  use general_specmod, only: general_init_spec_vars
  use constants, only: zero,half,one,two,three,rearth,pi
  use constants, only: rad2deg
  use mpimod, only: mype
  use general_sub2grid_mod, only: general_sub2grid_create_info
  use egrid2agrid_mod,only: g_create_egrid2agrid
  use general_sub2grid_mod, only: sub2grid_info
  use gsi_io, only: verbose
  use hybrid_ensemble_parameters, only: nsclgrp
  use hybrid_ensemble_parameters, only: spc_multwgt,spcwgt_params,l_sum_spc_weights
  implicit none

  integer(i_kind),intent(in   ) :: jcap_in
  real(r_kind),allocatable::totwvlength(:)

  integer(i_kind) i,ii,j,k,l,n,kk,nsigend
  integer ig
  real(r_kind) rwv0,rtem1,rtem2
  real (r_kind):: fwgtofwvlen


  allocate(totwvlength(jcap_in))

  rwv0=2*pi*rearth*.001_r_kind
  do i=1,jcap_in
  totwvlength(i)= rwv0/i                   
  enddo
  do i=1,jcap_in
   rtem1=0
   do ig=1,nsclgrp
    if(ig.ne.2) then
     spc_multwgt(i,ig)=fwgtofwvlen(spcwgt_params(1,ig),spcwgt_params(2,ig),&
                                   spcwgt_params(3,ig),spcwgt_params(4,ig),totwvlength(i)) !fwv2wgt(twvlength)
     if(l_sum_spc_weights.eq.0 ) then
      rtem1=rtem1+spc_multwgt(i,ig)
     else
      rtem1=rtem1+spc_multwgt(i,ig)**2
     endif
    endif
   enddo
   rtem2=1-rtem1
   if(abs(rtem2).ge.zero) then 
 
    if(l_sum_spc_weights.eq.0 ) then
     spc_multwgt(i,2)=rtem2 
    else
     spc_multwgt(i,2)=sqrt(rtem2)
    endif
   endif
  enddo
  spc_multwgt=max(spc_multwgt,0.0)
!   if(mype.eq.0) then
!   open(121,file="test-spcwght.dat",form="formatted")
!   do i=1,jcap_in
!   write(121,111)(totwvlength(i)),((spc_multwgt(i,j)),j=1,nsclgrp)
!   enddo
!   close (121)
!   endif
! 111 format(g15.6,3(g11.4,1x)) 
  
  deallocate(totwvlength)
  return
 end subroutine init_mult_spc_wgts



 subroutine apply_scaledepwgts(grd_in,sp_in,wbundle,spwgts,wbundle2,igrp,nensid)  
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
  integer(i_kind), intent(in):: igrp !group index
  integer(i_kind), intent(in):: nensid !ensemble member index

! Declare local variables
  integer(i_kind) ii,iflg,kk
  integer(i_kind) i,j,lunit 

  real(r_kind),dimension(grd_in%lat2,grd_in%lon2):: slndt,sicet,sst
  real(r_kind),dimension(grd_in%nlat*grd_in%nlon*grd_in%nlevs_alloc)      :: hwork
  real(r_kind),dimension(grd_in%nlat,grd_in%nlon,grd_in%nlevs_alloc)      :: work
  real(r_kind),dimension(grd_in%nlat,grd_in%nlon,grd_in%nlevs_alloc)      :: work1
  real(r_kind),dimension(sp_in%nc):: spc1
  real(r_single) outwork(grd_in%nlon,grd_in%nlat)
  character*64 :: fname1
  character*5:: varname1

  iflg=1
! Beta1 first
! Get from subdomains to
  call general_sub2grid(grd_in,wbundle%values,hwork)
  work=reshape(hwork,(/grd_in%nlat,grd_in%nlon,grd_in%nlevs_alloc/))
  work1=work
! Transfer work to spectral space
  do kk=1,grd_in%nlevs_alloc      
! nvar_id is not supposed to be used for ensemble-related structures fr !being now
! Shut off this bit if univariate ozone or surface temperature
!      if ( (nrf_var(nvar_id(kk))=='oz').or.(nrf_var(nvar_id(kk))=='OZ').and.oz_univ_static ) then
!        cycle
!    elseif ( (nrf_var(nvar_id(kk))=='sst').or.(nrf_var(nvar_id(kk))=='SST') ) then
!        cycle
!     elseif ( (nrf_var(nvar_id(kk))=='stl').or.(nrf_var(nvar_id(kk))=='STL') ) then
!        cycle
!     elseif ( (nrf_var(nvar_id(kk))=='sti').or.(nrf_var(nvar_id(kk))=='STI') ) then
!        cycle
!     end if
!   if(mype==0) then
     do j=1,grd_in%nlon
        do i=1,grd_in%nlat
           outwork(j,i)=work(i,j,kk)
        enddo
     enddo
!     i=5-len_trim( nrf_var(nvar_id(kk))) 
!     varname1=repeat("_",i)
!     varname1=trim(varname1)//trim(nrf_var(nvar_id(kk)))
!     write(fname1,'("grp",i2.2,"mem",i2.2,"-out_vname",A5,"lev-",i3.3)')igrp,nensid, varname1,levs_id(kk)
    

! Transform from physical space to spectral space   
     call general_g2s0(grd_in,sp_in,spc1,work(:,:,kk))

! Apply spectral weights
     call general_spec_multwgt(sp_in,spc1,spwgts)
! Transform back to physical space
     call general_s2g0(grd_in,sp_in,spc1,work(:,:,kk))
     work1(:,:,kk)=work(:,:,kk)-work1(:,:,kk)
!   if(mype==0) then
!      do j=1,grd_in%nlon
!        do i=1,grd_in%nlat
!           outwork(j,i)=work(i,j,kk)
!        enddo
!     enddo

  end do

! Transfer work back to subdomains
  hwork=reshape(work,(/grd_in%nlat*grd_in%nlon*grd_in%nlevs_alloc/))
  call general_grid2sub(grd_in,hwork,wbundle2%values)    

  return
end subroutine apply_scaledepwgts
