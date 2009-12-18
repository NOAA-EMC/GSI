module strong_slow_global_mod
!$$$   module documentation block
!                .      .    .                                       .
! module:    strong_slow_global_mod
!
! abstract: 
!          
! program history log:
!   2008-04-04  safford - add module doc block
!
! subroutines included;
!    init_strongvars_1                    --
!    initialize_strong_slow_global        --
!    strong_bal_correction_slow_global    -- compute balance adjustment
!    strong_bal_correction_slow_global_ad -- adjoint of strong_bal_correction
!    inmi_sub2grid                        -- special subdomain to horizontal grid
!    inmi_grid2sub                        -- reverse of inmi_sub2grid
!    get_mode_number                      -- define mode interleaving
!    gather_rmstends                      -- get BAL diagnostics
!
! variable definitions:
!     mode_number  - defines vertical mode layout for current processor
!     mode_number0 - defines how vertical modes are interleaved across processors
!                    in horizontal slab arrays
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!$$$ end documentation block

  use kinds, only: i_kind
  use gridmod, only: nsig
  use mpimod, only: nuvlevs,nnnuvlevs
  implicit none

  integer(i_kind),allocatable::mode_number(:),mode_number0(:)

contains

  subroutine init_strongvars_1(mype)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    init_strongvars_1
!
!   prgrmmr:
!
! abstract:
!
! program history log:
!   2008-04-04  safford -- add subprogram doc block
!
!   input argument list:
!     mype     - mpi task id
!
!   output argument list:
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$
    implicit none

    integer(i_kind),intent(in):: mype

    call initialize_strong_slow_global(mype)

  end subroutine init_strongvars_1

subroutine initialize_strong_slow_global(mype)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    initialize_strong_slow_global
!
!   prgrmmr:
!
! abstract:
!
! program history log:
!   2008-04-04  safford -- add subprogram doc block
!
!   input argument list:
!     mype     - mpi task id
!
!   output argument list:
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$
  implicit none

  integer(i_kind),intent(in):: mype

  allocate(mode_number(nuvlevs),mode_number0(nsig))
  call get_mode_number(mype)
!---------mode_number > 0 for 1st copy of u,v,m, reserved for correction delu,delv,delm
!---------mode_number < 0 for 2nd copy of u,v,m, reserved for grav part of tends, u_t_g,v_t_g,m_t_g

end subroutine initialize_strong_slow_global

subroutine strong_bal_correction_slow_global(u_t,v_t,t_t,ps_t,mype,psi,chi,t,ps,bal_diagnostic,fullfield,update)

!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    strong_bal_correction  strong balance correction
!   prgmmr: parrish          org: np23                date: 2006-07-15
!
! abstract: given input perturbation tendencies of u,v,t,ps from TLM on gaussian grid,
!           and input perturbation u,v,t,ps, compute balance adjustment to u,v,t,ps
!           which zeroes out input gravity component of perturbation tendencies.
!           also output, for later use, input tendencies projected onto gravity modes.
!
! program history log:
!   2006-07-15  parrish
!   2007-04-16  kleist  - modified for full field or incremental diagnostics
!   2008-04-04  safford - rm unused vars
!   2008-10-08  derber  - modify to output streamfunction and vel. pot. and to not update time derivatives
!   2009-11-27  parrish - add uv_hyb_ens.  if present and true, then
!                          input/output variables psi=u, chi=v.
!
!   input argument list:
!     u_t      - input perturbation u tendency on gaussian grid (subdomains)
!     v_t      - input perturbation v tendency on gaussian grid (subdomains)
!     t_t      - input perturbation T tendency on gaussian grid (subdomains)
!     ps_t     - input perturbation surface pressure tendency on gaussian grid (subdomains)
!     mype     - current processor
!     psi      - input perturbation psi on gaussian grid (subdomains)
!     chi      - input perturbation chi on gaussian grid (subdomains)
!     t        - input perturbation T on gaussian grid (subdomains)
!     ps       - input perturbation surface pressure on gaussian grid (subdomains)
!     bal_diagnostic - if true, then compute BAL diagnostic, a measure of amplitude
!                      of balanced gravity mode tendencies
!     fullfield - if true, full field diagnostics
!                 if false, incremental
!     update   - if false, then do not update u,v,t,ps with balance increment
!
!   output argument list:
!     psi      - output balanced perturbation psi on gaussian grid (subdomains)
!     chi      - output balanced perturbation chi on gaussian grid (subdomains)
!     t        - output balanced perturbation T on gaussian grid (subdomains)
!     ps       - output balanced perturbation surface pressure on gaussian grid (subdomains)
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$

  use kinds, only: r_kind
  use mod_vtrans, only: depths,nvmodes_keep,vtrans,vtrans_inv
  use mod_inmi, only: m,gspeed,mmax,dinmi,gproj
  use gridmod, only: nlat,nlon,lat2,lon2
  use specmod, only: jcap,nc
  use constants, only: zero
  use hybrid_ensemble_parameters, only: uv_hyb_ens
  implicit none

  integer(i_kind),intent(in)::mype
  logical,intent(in)::bal_diagnostic,update,fullfield
  real(r_kind),dimension(lat2,lon2,nsig),intent(inout)::u_t,v_t,t_t
  real(r_kind),dimension(lat2,lon2),intent(inout)::ps_t
  real(r_kind),dimension(lat2,lon2,nsig),intent(inout)::psi,chi,t
  real(r_kind),dimension(lat2,lon2),intent(inout)::ps

  real(r_kind),dimension(nvmodes_keep)::rmstend_uf,rmstend_g_uf
  real(r_kind),dimension(nvmodes_keep)::rmstend_f,rmstend_g_f

  real(r_kind),dimension(lat2,lon2,nsig)::delu,delv,delt
  real(r_kind),dimension(lat2,lon2)::delps
  real(r_kind),dimension(lat2,lon2,nvmodes_keep)::utilde_t,vtilde_t,mtilde_t
  real(r_kind),dimension(lat2,lon2,nvmodes_keep)::delutilde,delvtilde,delmtilde
  real(r_kind),dimension(lat2,lon2,nvmodes_keep)::utilde_t_g,vtilde_t_g,mtilde_t_g
  real(r_kind),dimension(nlat,nlon,nuvlevs)::uwork,vwork,mwork
  real(r_kind),dimension(nuvlevs)::rmstend_loc_uf,rmstend_g_loc_uf
  real(r_kind),dimension(nuvlevs)::rmstend_loc_f,rmstend_g_loc_f
  real(r_kind),dimension(nc)::divhat,vorthat,mhat,deldivhat,delvorthat,delmhat
  real(r_kind) rmstend_all_uf,rmstend_all_g_uf,rmstend_all_f,rmstend_all_g_f

  integer(i_kind) i,j,k,kk,iad,mode
  logical filtered

  filtered=.true.

  mmax=jcap

!   1.  u,v,t,ps   -->    utilde,vtilde,mtilde  (vertical mode transform)
!       (subdomains)         (subdomains)

  call vtrans(u_t,v_t,t_t,ps_t,utilde_t,vtilde_t,mtilde_t)

                      
  call inmi_sub2grid(utilde_t,utilde_t,uwork)
  call inmi_sub2grid(vtilde_t,vtilde_t,vwork)
  call inmi_sub2grid(mtilde_t,mtilde_t,mwork)

  rmstend_loc_uf=zero
  rmstend_g_loc_uf=zero
  rmstend_loc_f=zero
  rmstend_g_loc_f=zero
  do kk=1,nuvlevs
    mode=mode_number(kk)
    if(mode == 0) then
      do j=1,nlon
        do i=1,nlat
          uwork(i,j,kk)=zero
          vwork(i,j,kk)=zero
          mwork(i,j,kk)=zero
        end do
      end do
      cycle
    end if
      

!   3.  uwork,vwork,mwork    --> divhat,vorthat,mhat  (spectral transform)
!       (slabs)                  (slabs)

      vorthat=0 ; divhat=0 ; mhat=0
      call uvg2zds(vorthat,divhat,uwork(1,1,kk),vwork(1,1,kk))
      call g2s0(mhat,mwork(1,1,kk))

!   4.  divhat,vorthat,mhat --> deldivhat,delvorthat,delmhat   (inmi correction)
!          (slabs)                        (slabs)

      gspeed=sqrt(depths(abs(mode)))
      iad=1
      do m=0,jcap
        if(mode >  0) then
!              here, delvorthat, etc contain field corrections necessary to zero out gravity component
!                                         of tendencies
          call dinmi(vorthat  (iad),divhat  (iad),mhat  (iad),&
                   delvorthat(iad),deldivhat(iad),delmhat(iad))
        else
!               here, delvorthat, etc contain gravity component of tendencies
          if(bal_diagnostic) &
             call gproj(vorthat(iad),divhat(iad),mhat(iad),delvorthat(iad),deldivhat(iad),delmhat(iad), &
                     rmstend_loc_uf(kk),rmstend_g_loc_uf(kk),.not.filtered)
          call gproj(vorthat(iad),divhat(iad),mhat(iad),delvorthat(iad),deldivhat(iad),delmhat(iad), &
                     rmstend_loc_f(kk),rmstend_g_loc_f(kk),filtered)
        end if
        iad=iad+2*(jcap-m+1)
      end do

!   5.  deldivhat,delvorthat,delmhat    -->  uwork,vwork,mwork   (spectral inverse transform)
!          (slabs)                             (slabs)

      do j=1,nlon
        do i=1,nlat
          uwork(i,j,kk)=zero
          vwork(i,j,kk)=zero
          mwork(i,j,kk)=zero
        end do
      end do
      if(uv_hyb_ens) then
        call zds2uvg(delvorthat,deldivhat,uwork(1,1,kk),vwork(1,1,kk))
      else
        call zds2pcg(delvorthat,deldivhat,uwork(1,1,kk),vwork(1,1,kk))
      end if
      call s2g0(delmhat,mwork(1,1,kk))

  end do
  if(bal_diagnostic) then
    call gather_rmstends(rmstend_loc_uf,  rmstend_uf,  mype)
    call gather_rmstends(rmstend_g_loc_uf,rmstend_g_uf,mype)
    call gather_rmstends(rmstend_loc_f,  rmstend_f,  mype)
    call gather_rmstends(rmstend_g_loc_f,rmstend_g_f,mype)
    if(mype == 0) then
           rmstend_all_uf=zero
           rmstend_all_g_uf=zero

           if (fullfield) then
              write(6,*) 'STRONG_SLOW_GLOBAL:   FULL FIELD BALANCE DIAGNOSTICS --  '
           else
              write(6,*) 'STRONG_SLOW_GLOBAL:   INCREMENTAL BALANCE DIAGNOSTICS --  '
           end if

           do i=1,nvmodes_keep
             rmstend_all_uf=rmstend_all_uf+rmstend_uf(i)
             rmstend_all_g_uf=rmstend_all_g_uf+rmstend_g_uf(i)
             write(6,'(" mode,rmstend_uf,rmstend_g_uf,rat = ",i5,2e14.4,2f10.4)') &
                                i,rmstend_uf(i),rmstend_g_uf(i),&
                                rmstend_g_uf(i)/(rmstend_uf(i)-rmstend_g_uf(i)), &
                                rmstend_g_uf(i)/(rmstend_uf(1)-rmstend_g_uf(1))
           end do
           rmstend_all_f=zero
           rmstend_all_g_f=zero
           do i=1,nvmodes_keep
             rmstend_all_f=rmstend_all_f+rmstend_f(i)
             rmstend_all_g_f=rmstend_all_g_f+rmstend_g_f(i)
             write(6,'(" mode,rmstend_f,rmstend_g_f,rat = ",i5,2e14.4,2f10.4)') &
                                i,rmstend_f(i),rmstend_g_f(i),&
                                rmstend_g_f(i)/(rmstend_f(i)-rmstend_g_f(i)), &
                                rmstend_g_f(i)/(rmstend_f(1)-rmstend_g_f(1))
           end do
           write(6,'(" rmstend_all_uf,g_uf,rat = ",2e14.4,f10.4)') rmstend_all_uf,rmstend_all_g_uf, &
                                                    rmstend_all_g_uf/(rmstend_all_uf-rmstend_all_g_uf) 
           write(6,'(" rmstend_all_f,g_f,rat = ",2e14.4,f10.4)') rmstend_all_f,rmstend_all_g_f, &
                                                    rmstend_all_g_f/(rmstend_all_f-rmstend_all_g_f) 
    end if
  end if

  delutilde=0
  delvtilde=0
  delmtilde=0
  call inmi_grid2sub(delutilde,utilde_t_g,uwork)
  call inmi_grid2sub(delvtilde,vtilde_t_g,vwork)
  call inmi_grid2sub(delmtilde,mtilde_t_g,mwork)
 
!   7.  delutilde,delvtilde,delmtilde  -->  delu,delv,delt,delps   (vertical mode inverse transform)
!       (subdomains)                      (subdomains)

  call vtrans_inv(delutilde,delvtilde,delmtilde,delu,delv,delt,delps)
!????????????????????????????????in here, can insert diagnostic based on utilde_t_g and utilde_t,etc.
! call vtrans_inv(utilde_t_g,vtilde_t_g,mtilde_t_g,u_t_g,v_t_g,t_t_g,ps_t_g)


!  update u,v,t,ps


  if(update) then
    do k=1,nsig
      do j=1,lon2
        do i=1,lat2
          psi(i,j,k)=psi(i,j,k)+delu(i,j,k)
          chi(i,j,k)=chi(i,j,k)+delv(i,j,k)
          t(i,j,k)=t(i,j,k)+delt(i,j,k)
        end do
      end do
    end do
    do j=1,lon2
      do i=1,lat2
        ps(i,j)=ps(i,j)+delps(i,j)
      end do
    end do
  end if

end subroutine strong_bal_correction_slow_global

subroutine strong_bal_correction_slow_global_ad(u_t,v_t,t_t,ps_t,mype,psi,chi,t,ps)

!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    strong_bal_correction_ad adjoint of strong_bal_correction
!   prgmmr: parrish          org: np23                date: 2006-07-15
!
! abstract: adjoint of strong_bal_correction
!
! program history log:
!   2006-07-15  parrish
!   2009-11-27  parrish - add variable uv_hyb_ens.  if present and true, then
!                          input/output variables psi=u, chi=v.
!
!   input argument list:
!     u_t      - input perturbation u tendency on gaussian grid (subdomains)
!     v_t      - input perturbation v tendency on gaussian grid (subdomains)
!     t_t      - input perturbation T tendency on gaussian grid (subdomains)
!     ps_t     - input perturbation surface pressure tendency on gaussian grid (subdomains)
!     mype     - current processor
!     u        - input perturbation psi on gaussian grid (subdomains)
!     v        - input perturbation chi on gaussian grid (subdomains)
!     t        - input perturbation T on gaussian grid (subdomains)
!     ps       - input perturbation surface pressure on gaussian grid (subdomains)
!
!   output argument list:
!     u_t      - output perturbation psi tendency on gaussian grid (subdomains)
!     v_t      - output perturbation chi tendency on gaussian grid (subdomains)
!     t_t      - output perturbation T tendency on gaussian grid (subdomains)
!     ps_t     - output perturbation surface pressure tendency on gaussian grid (subdomains)
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$

  use kinds, only: r_kind
  use mod_vtrans, only: depths,nvmodes_keep,vtrans_ad,vtrans_inv_ad
  use mod_inmi, only: m,gspeed,mmax,dinmi_ad,gproj_ad
  use gridmod, only: nlat,nlon,lat2,lon2
  use specmod, only: jcap,nc
  use constants, only: zero
  use hybrid_ensemble_parameters, only: uv_hyb_ens
  implicit none

  integer(i_kind),intent(in)::mype
  real(r_kind),dimension(lat2,lon2,nsig),intent(inout)::u_t,v_t,t_t
  real(r_kind),dimension(lat2,lon2),intent(inout)::ps_t
  real(r_kind),dimension(lat2,lon2,nsig),intent(in)::psi,chi,t
  real(r_kind),dimension(lat2,lon2),intent(in)::ps

  real(r_kind),dimension(lat2,lon2,nsig)::delu,delv,delt
  real(r_kind),dimension(lat2,lon2)::delps
  real(r_kind),dimension(lat2,lon2,nvmodes_keep)::utilde_t,vtilde_t,mtilde_t
  real(r_kind),dimension(lat2,lon2,nvmodes_keep)::utilde_t2,vtilde_t2,mtilde_t2
  real(r_kind),dimension(lat2,lon2,nvmodes_keep)::delutilde,delvtilde,delmtilde
  real(r_kind),dimension(lat2,lon2,nvmodes_keep)::utilde_t_g,vtilde_t_g,mtilde_t_g
  real(r_kind),dimension(nlat,nlon,nuvlevs)::uwork,vwork,mwork
  real(r_kind),dimension(nc)::divhat,vorthat,mhat,deldivhat,delvorthat,delmhat

  integer(i_kind) mode,iad
  integer(i_kind) i,j,k,kk

  mmax=jcap

!  adjoint of update u,v,t,ps

  do j=1,lon2
    do i=1,lat2
      delps(i,j)=ps(i,j)
    end do
  end do

  do k=1,nsig
    do j=1,lon2
      do i=1,lat2
        delu(i,j,k)=psi(i,j,k)
        delv(i,j,k)=chi(i,j,k)
        delt(i,j,k)=t(i,j,k)
      end do
    end do
  end do

!   7.  adjoint of delutilde,delvtilde,delmtilde  -->  delu,delv,delt,delps  (vert mode inverse transform)
!       (subdomains)                      (subdomains)

  delutilde=zero ; delvtilde=zero ; delmtilde=zero
  utilde_t_g=zero ; vtilde_t_g=zero ; mtilde_t_g=zero
  call vtrans_inv_ad(delutilde,delvtilde,delmtilde,delu,delv,delt,delps)

  call inmi_sub2grid(delutilde,utilde_t_g,uwork)
  call inmi_sub2grid(delvtilde,vtilde_t_g,vwork)
  call inmi_sub2grid(delmtilde,mtilde_t_g,mwork)

  do kk=1,nuvlevs
    mode=mode_number(kk)
    if(mode == 0) then
      do j=1,nlon
        do i=1,nlat
          uwork(i,j,kk)=zero
          vwork(i,j,kk)=zero
          mwork(i,j,kk)=zero
        end do
      end do
      cycle
    end if


!   5.  adjoint of deldivhat,delvorthat,delmhat    -->  uwork,vwork,mwork   (spectral inverse transform)
!                     (slabs)                             (slabs)

      call s2g0_ad(delmhat,mwork(1,1,kk))
      if(uv_hyb_ens) then
        call zds2uvg_ad(delvorthat,deldivhat,uwork(1,1,kk),vwork(1,1,kk))
      else
        call zds2pcg_ad(delvorthat,deldivhat,uwork(1,1,kk),vwork(1,1,kk))
      end if

!   4.  divhat,vorthat,mhat --> deldivhat,delvorthat,delmhat   (inmi correction)
!          (slabs)                        (slabs)

      gspeed=sqrt(depths(abs(mode)))
      iad=1
      vorthat=zero ; divhat=zero ; mhat=zero
      do m=0,jcap
        if(mode >  0) then
          call dinmi_ad(vorthat(iad),divhat(iad),mhat(iad),&
                      delvorthat(iad)   ,   deldivhat(iad),   delmhat(iad))
        else
          call gproj_ad(vorthat(iad),divhat(iad),mhat(iad),delvorthat(iad),deldivhat(iad),delmhat(iad))
        end if
        iad=iad+2*(jcap-m+1)
      end do

!   3.  adjoint of uwork,vwork,mwork    --> divhat,vorthat,mhat  (spectral transform)
!                       (slabs)                  (slabs)

      call g2s0_ad(mhat,mwork(1,1,kk))
      call uvg2zds_ad(vorthat,divhat,uwork(1,1,kk),vwork(1,1,kk))

  end do

!???????????????????????this part needs some thought--probably need to have two distinct arrays,
!  which are then added together after these calls
  call inmi_grid2sub(utilde_t,utilde_t2,uwork)
  call inmi_grid2sub(vtilde_t,vtilde_t2,vwork)
  call inmi_grid2sub(mtilde_t,mtilde_t2,mwork)
  utilde_t=utilde_t+utilde_t2
  vtilde_t=vtilde_t+vtilde_t2
  mtilde_t=mtilde_t+mtilde_t2
!
!    1.  adjoint of u,v,t,ps   -->    utilde,vtilde,mtilde  (vertical mode transform)
!                      (subdomains)         (subdomains)

  call vtrans_ad(u_t,v_t,t_t,ps_t,utilde_t,vtilde_t,mtilde_t)

end subroutine strong_bal_correction_slow_global_ad

subroutine inmi_sub2grid(utilde,utilde2,uwork)

!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    inmi_sub2grid  special subdomain to horizontal grid
!   prgmmr: parrish          org: np23                date: 2006-08-03
!
! abstract: convert two subdomain 3d fields utilde,utilde2 from subdomains
!           to horizontal slab array uwork.  note that must have 
!           2*nvmodes_keep <= nsig.
!
! program history log:
!   2006-08-03  parrish
!   2008-04-04  safford  - rm unused uses
!
!   input argument list:
!     utilde   - vertical tranformed variable on subdomains
!     utilde2  - vertical tranformed variable on subdomains
!
!   output argument list:
!     uwork    - output fields in horizontal slab mode
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$

  use kinds, only: r_kind
  use constants, only: zero
  use gridmod, only: lat2,iglobal,lon1,itotsub,lon2,lat1,ltosi,ltosj,nlon,nlat
  use mpimod, only: ierror,mpi_comm_world,&
       isduv_g,iscuv_g,irduv_g,ircuv_g,mpi_rtype,&
       strip,reorder
  use mod_vtrans, only: nvmodes_keep
  implicit none

! Declare passed variables
  real(r_kind),dimension(lat2,lon2,nvmodes_keep),intent(in):: utilde,utilde2
  real(r_kind),dimension(nlat,nlon,nuvlevs),intent(out)::uwork

  real(r_kind),dimension(lat2,lon2,nsig):: u

! Declare local variables
  integer(i_kind) i,j,k,isize,mode

  real(r_kind),dimension(lat1,lon1,nsig):: usm
  real(r_kind),dimension(itotsub,nuvlevs):: work3

! Initialize variables
  isize=max(iglobal,itotsub)

! zero out work array
  do k=1,nuvlevs
    do j=1,isize
      work3(j,k)=zero
    end do
  end do

!  copy input array into bigger internal array
  do k=1,nsig
    do j=1,lon2
      do i=1,lat2
        u(i,j,k)=zero
      end do
    end do
  end do
  do k=1,nsig
    mode=mode_number0(k)
    if(mode == 0) cycle
    if(mode >  0) then
      do j=1,lon2
        do i=1,lat2
          u(i,j,k)=utilde(i,j,mode)
        end do
      end do
    else
      do j=1,lon2
        do i=1,lat2
          u(i,j,k)=utilde2(i,j,-mode)
        end do
      end do
    end if
    
  end do

!   begin strmfctn / vp to u/v section
!   strip off endpoints of input arrays on subdomains
!   note that right now, place in usm,vsm
    call strip(u,usm,nsig)

!   put on global slabs
    call mpi_alltoallv(usm(1,1,1),iscuv_g,isduv_g,&
         mpi_rtype,work3(1,1),ircuv_g,irduv_g,mpi_rtype,&
         mpi_comm_world,ierror)

!   reorder work arrays and transfer to output array
    call reorder(work3,nuvlevs,nnnuvlevs)
    do k=1,nnnuvlevs
      do i=1,iglobal
        uwork(ltosi(i),ltosj(i),k)=work3(i,k)
      end do
    end do

end subroutine inmi_sub2grid

subroutine inmi_grid2sub(utilde,utilde2,uwork)

!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    inmi_grid2sub  reverse of inmi_sub2grid
!   prgmmr: parrish          org: np23                date: 2006-08-03
!
! abstract: reverse of inmi_sub2grid
!
! program history log:
!   2006-08-03  parrish
!   2008-04-04  safford - rm unused vars and uses
!
!   input argument list:
!     uwork    - input fields in horizontal slab mode
!
!   output argument list:
!     utilde   - vertical tranformed variable on subdomains
!     utilde2  - vertical tranformed variable on subdomains
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$

  use kinds, only: r_kind
  use gridmod, only: lat2,iglobal,itotsub,lon2,nlat,nlon,ltosi_s,ltosj_s
  use mpimod, only: iscuv_s,ierror,mpi_comm_world,irduv_s,ircuv_s,&
       isduv_s,mpi_rtype,reorder2
  use mod_vtrans, only: nvmodes_keep
  implicit none

! Declare passed variables
  real(r_kind),dimension(lat2,lon2,nvmodes_keep),intent(out):: utilde,utilde2
  real(r_kind),dimension(nlat,nlon,nuvlevs),intent(in)::uwork

  real(r_kind),dimension(lat2,lon2,nsig):: u

! Declare local variables
  integer(i_kind) i,j,k,isize,mode

  real(r_kind),dimension(itotsub,nuvlevs):: work3

! Initialize variables
  isize=max(iglobal,itotsub)

    do k=1,nnnuvlevs
      do i=1,itotsub
        work3(i,k)=uwork(ltosi_s(i),ltosj_s(i),k)
      end do
    end do
!   reorder the work array for the mpi communication
    call reorder2(work3,nuvlevs,nnnuvlevs)

!   get u back on subdomains
    call mpi_alltoallv(work3(1,1),iscuv_s,isduv_s,&
         mpi_rtype,u(1,1,1),ircuv_s,irduv_s,mpi_rtype,&
         mpi_comm_world,ierror)
  do k=1,nsig
    mode=mode_number0(k)
    if(mode == 0) cycle
    if(mode >  0) then
      do j=1,lon2
        do i=1,lat2
          utilde(i,j,mode)=u(i,j,k)
        end do
      end do
    else
      do j=1,lon2
        do i=1,lat2
          utilde2(i,j,-mode)=u(i,j,k)
        end do
      end do
    end if
  end do

end subroutine inmi_grid2sub

subroutine get_mode_number(mype)

!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    get_mode_number  define mode interleaving
!   prgmmr: parrish          org: np23                date: 2006-08-03
!
! abstract: define how modes are interleaved across processors
!           when converting vertical transformed variables between
!           subdomains and horizontal slab storage
!
! program history log:
!   2006-08-03  parrish
!
!   input argument list:
!     mype     - current processor number
!
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$

  use kinds, only: r_kind
  use mpimod, only: ierror,mpi_comm_world,mpi_integer,npe
  use mod_vtrans, only: nvmodes_keep
  use constants, only: izero
  implicit none

  integer(i_kind),intent(in):: mype

  integer(i_kind) i,ii
  integer(i_kind) nuvlevs0(npe),ndisp(npe+1)
  integer(i_kind) nuvlev_use,kchk

      if(nvmodes_keep*2 >  nsig) then
           write(6,*)' error in get_mode_number, currently necessary for nvmodes_keep*2 <= nsig '
           call stop2(89)
      end if
  if (mod(nsig,npe)==izero) then
    kchk=npe
  else
    kchk=mod(nsig,npe)
  end if
  if(mype+1 <= kchk) then
    nuvlev_use=nuvlevs
  else
    nuvlev_use=nuvlevs-1
  end if
  ii=0
  mode_number=0
  do i=1,2*nvmodes_keep
    if(mod(i-1,npe) == mype.and.ii <  nuvlev_use) then
      ii=ii+1
      if(i <= nvmodes_keep) then
        mode_number(ii)=i
      else
        mode_number(ii)=nvmodes_keep-i
      end if
    end if
  end do
  call mpi_allgather(nuvlev_use,1,mpi_integer,nuvlevs0,1,mpi_integer,mpi_comm_world,ierror)
  ndisp(1)=0
  do i=2,npe+1
    ndisp(i)=ndisp(i-1)+nuvlevs0(i-1)
  end do
  call mpi_allgatherv(mode_number,nuvlev_use,mpi_integer, &
                   mode_number0,nuvlevs0,ndisp,mpi_integer,mpi_comm_world,ierror)

end subroutine get_mode_number

subroutine gather_rmstends(rmstend_loc,rmstend,mype)

!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    gather_rmstends  get BAL diagnostics
!   prgmmr: parrish          org: np23                date: 2006-08-03
!
! abstract: compute BAL diagnostics which give amplitude of 
!           gravity projection of energy norm of tendencies
!
! program history log:
!   2006-08-03  parrish
!
!   input argument list:
!     rmstend_loc - previously computed energy norms of vertical modes
!                   as distributed on local processors
!     mype     - current processor number
!
!   output argument list:
!     rmstend  -  all vertical modes of rmstend assembled across all processors
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$

  use kinds, only: r_kind
  use mpimod, only: ierror,mpi_comm_world,mpi_integer,mpi_rtype,npe
  use mod_vtrans, only: nvmodes_keep
  use constants, only: izero
  implicit none

  real(r_kind),intent(in):: rmstend_loc(nuvlevs)
  real(r_kind),intent(out):: rmstend(nvmodes_keep)
  integer(i_kind),intent(in):: mype

  integer(i_kind) i
  integer(i_kind) nuvlevs0(npe),ndisp(npe+1)
  integer(i_kind) nuvlev_use,kchk
  real(r_kind) work(nsig)

  if (mod(nsig,npe)==izero) then
    kchk=npe
  else
    kchk=mod(nsig,npe)
  end if
  if(mype+1 <= kchk) then
    nuvlev_use=nuvlevs
  else
    nuvlev_use=nuvlevs-1
  end if
  call mpi_allgather(nuvlev_use,1,mpi_integer,nuvlevs0,1,mpi_integer,mpi_comm_world,ierror)
  ndisp(1)=0
  do i=2,npe+1
    ndisp(i)=ndisp(i-1)+nuvlevs0(i-1)
  end do
  call mpi_allgatherv(rmstend_loc,nuvlev_use,mpi_rtype, &
                     work,nuvlevs0,ndisp,mpi_rtype,mpi_comm_world,ierror)
  do i=1,nsig
    if(mode_number0(i) <  0) rmstend(-mode_number0(i))=work(i)
  end do

end subroutine gather_rmstends

end module strong_slow_global_mod
