module strong_fast_global_mod
!$$$   module documentation block
!                .      .    .                                       .
! module:    strong_fast_global_mod
!
! abstract: Contains all routines for fast strong balance constraint
!

! program history log:
!   2008-04-04  safford - add moddule doc block and missing subroutine doc blocks
!
! subroutines included:
!    init_strongvars_2        --
!    initialize_strong_fast_global
!    strong_bal_correction         -- strong balance correction
!    strong_bal_correction_fast_global_ad -- adjoint of strong_bal_correction
!    gather_rmstends0         -- get BAL diagnostics
!    gather_rmstends          -- get BAL diagnostics
!    inmi_coupler_sd2ew0      --
!    inmi_coupler_sd2ew1      --
!    inmi_coupler_sd2ew       --
!    inmi_coupler_sd2ew1      --
!    inmi_coupler_sd2ew       --
!    inmi_coupler_ew2sd1      --
!    inmi_coupler_ew2sd       --
!    inmi_ew_trans            --
!    inmi_ew_invtrans_ad      --
!    inmi_ew_invtrans         --
!    inmi_ew_trans_ad         --
!    inmi_coupler_ew2ns0      --
!    inmi_coupler_ew2ns1      --
!    inmi_coupler_ew2ns       --
!    inmi_coupler_ns2ew       --
!    inmi_nsuvm2zdm           --
!    spdz2uv_ns               -- COMPUTE WINDS FROM div and vort for 1 zonal wave number
!    spuv2dz_ns               -- COMPUTE DIV,VORT FROM WINDS for one zonal wave number
!    SPANALY_ns               -- ANALYZE SPECTRAL FROM FOURIER
!    SPSYNTH_ns               -- SPSYNTH modified for one zonal wave number
!
! variable definitions:
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!$$$ end documentation block

  use kinds, only: i_kind
  implicit none

  integer(i_kind),allocatable:: mode_list(:,:)
                                                   ! mode_list(1,j) = lat index for ew strip j
                                                   !  mode_list(2,j) = vert mode number for ew strip j
                                                   !  mode_list(3,j) = pe of this lat/vert mode strip 
  integer(i_kind),allocatable:: mmode_list(:,:)
                                                  !  mmode_list(1,j) = m1 (zonal wave number 1) for ns strip
                                                  !  mmode_list(2,j) = m2 (zonal wave number 2) for ns strip
                                                  !  mmode_list(3,j) = vert mode number 1 for ns strip j
                                                  !  mmode_list(4,j) = vert mode number 2 for ns strip j
                                                  !  mmode_list(5,j) = pe for ns strip j
  integer(i_kind) nlatm_0,nlatm_1,m_0,m_1
  integer(i_kind) mthis
  integer(i_kind),allocatable:: mthis0(:),ndisp(:),indexglob(:)
  integer(i_kind),allocatable,dimension(:)::nsend_sd2ew,nrecv_sd2ew
  integer(i_kind),allocatable,dimension(:)::ndsend_sd2ew,ndrecv_sd2ew
  integer(i_kind),allocatable::info_send_sd2ew(:,:),info_recv_sd2ew(:,:)
  integer(i_kind) nallsend_sd2ew,nallrecv_sd2ew
  integer(i_kind),allocatable,dimension(:)::nsend_ew2sd,nrecv_ew2sd
  integer(i_kind),allocatable,dimension(:)::ndsend_ew2sd,ndrecv_ew2sd
  integer(i_kind),allocatable::info_send_ew2sd(:,:),info_recv_ew2sd(:,:)
  integer(i_kind) nallsend_ew2sd,nallrecv_ew2sd
  integer(i_kind) nallsend,nallrecv
  integer(i_kind),allocatable::info_send(:,:),info_recv(:,:)
  integer(i_kind),allocatable::nsend(:),nrecv(:),ndsend(:),ndrecv(:)

contains

  subroutine init_strongvars_2(mype)

!$$$  subprogram documentation block
!                .      .    .
! subprogram:    init_strongvars_2
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

    call initialize_strong_fast_global(mype)

  end subroutine init_strongvars_2

subroutine initialize_strong_fast_global(mype)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    initialize_strong_fast_global
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


  use gridmod, only: nlat
  use specmod, only: jcap
  use mod_vtrans, only: nvmodes_keep
  implicit none

  integer(i_kind),intent(in):: mype

  allocate(mode_list(3,nlat*nvmodes_keep),mmode_list(5,(jcap+1)*nvmodes_keep))
  call inmi_coupler_sd2ew0(mype)
  call inmi_coupler_ew2ns0(mype)
  call gather_rmstends0
  call inmi_coupler_sd2ew1(mype)
  call inmi_coupler_ew2ns1(mype)
  call inmi_coupler_ew2sd1(mype)

end subroutine initialize_strong_fast_global


subroutine strong_bal_correction_fast_global(u_t,v_t,t_t,ps_t,mype,psi,chi,t,ps, &
                                              bal_diagnostic,fullfield,update)

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
!   2008-10-08  parrish/derber - modify to output streamfunction and vel. pot. and not update time derivatives
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
!     psi      - output balanced perturbation u on gaussian grid (subdomains)
!     chi      - output balanced perturbation v on gaussian grid (subdomains)
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
  use gridmod, only: nlat,nlon,lat2,lon2,nsig
  use specmod, only: jcap
  use constants, only: zero,one,rearth
  implicit none

  integer(i_kind),intent(in)::mype
  logical,intent(in)::bal_diagnostic,update,fullfield
  real(r_kind),dimension(lat2,lon2,nsig),intent(in)::u_t,v_t,t_t
  real(r_kind),dimension(lat2,lon2),intent(in)::ps_t
  real(r_kind),dimension(lat2,lon2,nsig),intent(inout)::psi,chi,t
  real(r_kind),dimension(lat2,lon2),intent(inout)::ps

  real(r_kind),dimension(nvmodes_keep)::rmstend_uf,rmstend_g_uf
  real(r_kind),dimension(nvmodes_keep)::rmstend_f,rmstend_g_f

  real(r_kind),dimension(lat2,lon2,nsig)::delu,delv,delt
  real(r_kind),dimension(lat2,lon2)::delps
  real(r_kind),dimension(lat2,lon2,nvmodes_keep)::utilde_t,vtilde_t,mtilde_t
  real(r_kind),dimension(lat2,lon2,nvmodes_keep)::delutilde,delvtilde,delmtilde
  real(r_kind),dimension(lat2,lon2,nvmodes_keep)::utilde_t_g,vtilde_t_g,mtilde_t_g
  real(r_kind),allocatable,dimension(:,:)::rmstend_loc_uf,rmstend_g_loc_uf
  real(r_kind),allocatable,dimension(:,:)::rmstend_loc_f,rmstend_g_loc_f
  real(r_kind),dimension(2,0:jcap)::divhat,vorthat,mhat,deldivhat,delvorthat,delmhat
  real(r_kind) rmstend_all_uf,rmstend_all_g_uf,rmstend_all_f,rmstend_all_g_f
  real(r_kind),allocatable,dimension(:,:,:,:)::uvm_ew
  real(r_kind),allocatable,dimension(:,:,:,:,:)::uvm_ewtrans,uvm_ns,zdm_hat
  real(r_kind) del2inv

  integer(i_kind) i,ipair,j,k,kk,mode,n
  logical filtered

  filtered=.true.

  mmax=jcap

!   1.  u,v,t,ps   -->    utilde,vtilde,mtilde  (vertical mode transform)
!       (subdomains)         (subdomains)

  call vtrans(u_t,v_t,t_t,ps_t,utilde_t,vtilde_t,mtilde_t)

  allocate(uvm_ew(2,3,nlon,nlatm_0:nlatm_1),uvm_ewtrans(2,3,2,0:jcap,nlatm_0:nlatm_1))
  allocate(uvm_ns(3,2,nlat,2,m_0:m_1),zdm_hat(3,2,nlat,2,m_0:m_1))
  allocate(rmstend_loc_uf(2,m_0:m_1))
  allocate(rmstend_g_loc_uf(2,m_0:m_1))
  allocate(rmstend_loc_f(2,m_0:m_1))
  allocate(rmstend_g_loc_f(2,m_0:m_1))
  call inmi_coupler_sd2ew(utilde_t,vtilde_t,mtilde_t,utilde_t,vtilde_t,mtilde_t, &
                          uvm_ew,mype)
  call inmi_ew_trans(uvm_ew,uvm_ewtrans)
  call inmi_coupler_ew2ns(uvm_ewtrans,uvm_ns)
  call inmi_nsuvm2zdm(uvm_ns,zdm_hat)
  rmstend_loc_uf=zero
  rmstend_g_loc_uf=zero
  rmstend_loc_f=zero
  rmstend_g_loc_f=zero
  do kk=m_0,m_1
    do ipair=1,2
      m=mmode_list(ipair,kk)
      mode=mmode_list(ipair+2,kk)
      gspeed=sqrt(depths(abs(mode)))
      i=0
      do n=m,jcap
        i=i+1
        vorthat(1,n)=zdm_hat(1,1,i,ipair,kk)
        vorthat(2,n)=zdm_hat(1,2,i,ipair,kk)
        divhat( 1,n)=zdm_hat(2,1,i,ipair,kk)
        divhat( 2,n)=zdm_hat(2,2,i,ipair,kk)
        mhat(   1,n)=zdm_hat(3,1,i,ipair,kk)
        mhat(   2,n)=zdm_hat(3,2,i,ipair,kk)
      end do

!   4.  divhat,vorthat,mhat --> deldivhat,delvorthat,delmhat   (inmi correction)
!          (slabs)                        (slabs)

      if(mode.gt.0) then
!              here, delvorthat, etc contain field corrections necessary to zero out gravity component
!                                         of tendencies
        call dinmi(vorthat(1,m),divhat(1,m),mhat(1,m),delvorthat(1,m),deldivhat(1,m),delmhat(1,m))
      else
!               here, delvorthat, etc contain gravity component of tendencies
        if(bal_diagnostic) &
           call gproj(vorthat(1,m),divhat(1,m),mhat(1,m),delvorthat(1,m),deldivhat(1,m),delmhat(1,m), &
                     rmstend_loc_uf(ipair,kk),rmstend_g_loc_uf(ipair,kk),.not.filtered)
           call gproj(vorthat(1,m),divhat(1,m),mhat(1,m),delvorthat(1,m),deldivhat(1,m),delmhat(1,m), &
                     rmstend_loc_f(ipair,kk),rmstend_g_loc_f(ipair,kk),filtered)
      end if

      i=0
      do n=m,jcap
        del2inv=zero
        if(n.gt.0) del2inv=rearth**2/(n*(n+one))
        i=i+1
        zdm_hat(1,1,i,ipair,kk)=-delvorthat(1,n)*del2inv
        zdm_hat(1,2,i,ipair,kk)=-delvorthat(2,n)*del2inv
        zdm_hat(2,1,i,ipair,kk)=-deldivhat(1,n)*del2inv
        zdm_hat(2,2,i,ipair,kk)=-deldivhat(2,n)*del2inv
        zdm_hat(3,1,i,ipair,kk)=delmhat(1,n)
        zdm_hat(3,2,i,ipair,kk)=delmhat(2,n)
      end do

    end do
  end do

  call inmi_nspcm_hat2pcm(uvm_ns,zdm_hat)
  call inmi_coupler_ns2ew(uvm_ewtrans,uvm_ns)
  call inmi_ew_invtrans(uvm_ew,uvm_ewtrans)
  call inmi_coupler_ew2sd(delutilde,delvtilde,delmtilde,utilde_t_g,vtilde_t_g,mtilde_t_g,uvm_ew,mype)
  deallocate(uvm_ew,uvm_ewtrans,uvm_ns,zdm_hat)

  if(bal_diagnostic) then
    call gather_rmstends(rmstend_loc_uf,  rmstend_uf)
    call gather_rmstends(rmstend_g_loc_uf,rmstend_g_uf)
    call gather_rmstends(rmstend_loc_f,   rmstend_f)
    call gather_rmstends(rmstend_g_loc_f, rmstend_g_f)
    if(mype.eq.0) then
           rmstend_all_uf=zero
           rmstend_all_g_uf=zero

           if (fullfield) then
              write(6,*) 'STRONG_FAST_GLOBAL:   FULL FIELD BALANCE DIAGNOSTICS --  '
           else
              write(6,*) 'STRONG_FAST_GLOBAL:   INCREMENTAL BALANCE DIAGNOSTICS --  '
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
  deallocate(rmstend_loc_uf,rmstend_g_loc_uf,rmstend_loc_f,rmstend_g_loc_f)

!   7.  delutilde,delvtilde,delmtilde  -->  delu,delv,delt,delps   (vertical mode inverse transform)
!       (subdomains)                      (subdomains)

  call vtrans_inv(delutilde,delvtilde,delmtilde,delu,delv,delt,delps)
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

end subroutine strong_bal_correction_fast_global

subroutine strong_bal_correction_fast_global_ad(u_t,v_t,t_t,ps_t,mype,psi,chi,t,ps)

!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    strong_bal_correction_ad adjoint of strong_bal_correction
!   prgmmr: parrish          org: np23                date: 2006-07-15
!
! abstract: adjoint of strong_bal_correction
!
! program history log:
!   2006-07-15  parrish
!   2008-10-08  parrish/derber - modify to output streamfunction and vel. pot. and not update time derivatives
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
!
!   output argument list:
!     u_t      - output perturbation u tendency on gaussian grid (subdomains)
!     v_t      - output perturbation v tendency on gaussian grid (subdomains)
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
  use gridmod, only: nlat,nlon,lat2,lon2,nsig
  use specmod, only: jcap
  use constants, only: zero,one,rearth
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
  real(r_kind),dimension(2,0:jcap)::divhat,vorthat,mhat,deldivhat,delvorthat,delmhat
  real(r_kind),allocatable,dimension(:,:,:,:)::uvm_ew
  real(r_kind),allocatable,dimension(:,:,:,:,:)::uvm_ewtrans,uvm_ns,zdm_hat
  real(r_kind) del2inv

  integer(i_kind) i,ipair,j,k,kk,mode,n

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

  allocate(uvm_ew(2,3,nlon,nlatm_0:nlatm_1),uvm_ewtrans(2,3,2,0:jcap,nlatm_0:nlatm_1))
  allocate(uvm_ns(3,2,nlat,2,m_0:m_1),zdm_hat(3,2,nlat,2,m_0:m_1))
  call inmi_coupler_sd2ew(delutilde,delvtilde,delmtilde,utilde_t_g,vtilde_t_g,mtilde_t_g,uvm_ew,mype)
  call inmi_ew_invtrans_ad(uvm_ew,uvm_ewtrans)
  call inmi_coupler_ew2ns(uvm_ewtrans,uvm_ns)
  call inmi_nspcm_hat2pcm_ad(uvm_ns,zdm_hat)
  do kk=m_0,m_1
    do ipair=1,2
      m=mmode_list(ipair,kk)
      mode=mmode_list(ipair+2,kk)
      gspeed=sqrt(depths(abs(mode)))
      do n=m,jcap
        vorthat(1,n)=zero
        vorthat(2,n)=zero
        divhat( 1,n)=zero
        divhat( 2,n)=zero
        mhat(   1,n)=zero
        mhat(   2,n)=zero
      end do
      i=0
      do n=m,jcap
        del2inv=zero
        if(n.gt.0) del2inv=rearth**2/(n*(n+one))
        i=i+1
        delvorthat(1,n)=-zdm_hat(1,1,i,ipair,kk)*del2inv
        delvorthat(2,n)=-zdm_hat(1,2,i,ipair,kk)*del2inv
        deldivhat(1,n)=-zdm_hat(2,1,i,ipair,kk)*del2inv
        deldivhat(2,n)=-zdm_hat(2,2,i,ipair,kk)*del2inv
        delmhat(1,n)=zdm_hat(3,1,i,ipair,kk)
        delmhat(2,n)=zdm_hat(3,2,i,ipair,kk)
      end do
      if(mode.gt.0) then
        call dinmi_ad(vorthat(1,m),divhat(1,m),mhat(1,m),&
                    delvorthat(1,m)   ,   deldivhat(1,m),   delmhat(1,m))
      else
        call gproj_ad(vorthat(1,m),divhat(1,m),mhat(1,m),delvorthat(1,m),deldivhat(1,m),delmhat(1,m))
      end if

      i=0
      do n=m,jcap
        i=i+1
        zdm_hat(1,1,i,ipair,kk)=vorthat(1,n)
        zdm_hat(1,2,i,ipair,kk)=vorthat(2,n)
        zdm_hat(2,1,i,ipair,kk)=divhat(1,n)
        zdm_hat(2,2,i,ipair,kk)=divhat(2,n)
        zdm_hat(3,1,i,ipair,kk)=mhat(1,n)
        zdm_hat(3,2,i,ipair,kk)=mhat(2,n)
      end do

    end do
  end do

  call inmi_nsuvm2zdm_ad(uvm_ns,zdm_hat)
  call inmi_coupler_ns2ew(uvm_ewtrans,uvm_ns)
  call inmi_ew_trans_ad(uvm_ew,uvm_ewtrans)
  call inmi_coupler_ew2sd(utilde_t,vtilde_t,mtilde_t,utilde_t2,vtilde_t2,mtilde_t2,uvm_ew,mype)

  utilde_t=utilde_t+utilde_t2
  vtilde_t=vtilde_t+vtilde_t2
  mtilde_t=mtilde_t+mtilde_t2
!
!    1.  adjoint of u,v,t,ps   -->    utilde,vtilde,mtilde  (vertical mode transform)
!                      (subdomains)         (subdomains)

  call vtrans_ad(u_t,v_t,t_t,ps_t,utilde_t,vtilde_t,mtilde_t)
  deallocate(uvm_ew,uvm_ewtrans,uvm_ns,zdm_hat)

end subroutine strong_bal_correction_fast_global_ad

subroutine gather_rmstends0

!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    gather_rmstends0  get BAL diagnostics
!   prgmmr: parrish          org: np23                date: 2006-08-03
!
! abstract: compute BAL diagnostics which give amplitude of 
!           gravity projection of energy norm of tendencies
!
! program history log:
!   2006-08-03  parrish
!   2008-04-04  safford  - rm unused uses
!
!   input argument list:
!
!   output argument list:
!     rmstend  -  all vertical modes of rmstend assembled across all processors
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$

  use mpimod, only: ierror,mpi_comm_world,mpi_integer4,npe
  use mod_vtrans, only: nvmodes_keep
  use specmod, only: jcap
  implicit none

  integer(i_kind) indexloc(m_0:m_1)
  integer(i_kind) i
  
  allocate(mthis0(npe),ndisp(npe+1),indexglob((jcap+1)*nvmodes_keep))
  do i=m_0,m_1
    indexloc(i)=i
  end do
  mthis=m_1-m_0+1
  call mpi_allgather(mthis,1,mpi_integer4,mthis0,1,mpi_integer4,mpi_comm_world,ierror)
  ndisp(1)=0
  do i=2,npe+1
    ndisp(i)=ndisp(i-1)+mthis0(i-1)
  end do
  call mpi_allgatherv(indexloc,mthis,mpi_integer4, &
                      indexglob,mthis0,ndisp,mpi_integer4,mpi_comm_world,ierror)

end subroutine gather_rmstends0

subroutine gather_rmstends(rmstend_loc,rmstend)

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
!   2008-04-04  safford  - rm unused uses
!
!   input argument list:
!     rmstend_loc - previously computed energy norms of vertical modes
!                   as distributed on local processors
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
  use mpimod, only: ierror,mpi_comm_world,mpi_rtype,npe
  use mod_vtrans, only: nvmodes_keep
  use constants, only: zero
  use specmod, only: jcap
  implicit none

  real(r_kind),intent(in):: rmstend_loc(2,m_0:m_1)
  real(r_kind),intent(out):: rmstend(nvmodes_keep)

  integer(i_kind) i,ii,mode,mpi_string1
  real(r_kind) work(2,(jcap+1)*nvmodes_keep)
  
  call mpi_type_contiguous(2,mpi_rtype,mpi_string1,ierror)
  call mpi_type_commit(mpi_string1,ierror)
  call mpi_allgatherv(rmstend_loc,mthis,mpi_string1, &
                     work,mthis0,ndisp,mpi_string1,mpi_comm_world,ierror)
  call mpi_type_free(mpi_string1,ierror)
  rmstend=zero
  do i=1,ndisp(npe+1)
    ii=indexglob(i)
    mode=mmode_list(3,ii)
    if(mode.lt.0) rmstend(-mode)=work(1,ii)+rmstend(-mode)
    mode=mmode_list(4,ii)
    if(mode.lt.0) rmstend(-mode)=work(2,ii)+rmstend(-mode)
  end do

end subroutine gather_rmstends

subroutine inmi_coupler_sd2ew0(mype)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    inmi_coupler_sd2ew0
!   prgmmr:
!
! abstract:  create ew (lat strips) subdivision for use with inmi
!
! program history log:
!   2008-04-04  safford  - add doc block
!
!   input argument list:
!     mype        - current processor number
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$


!  create ew (lat strips) subdivision for use with inmi

  use mod_vtrans, only: nvmodes_keep
  use gridmod, only: nlat
  use mpimod, only: npe
  implicit none


  integer(i_kind),intent(in)::mype

  integer(i_kind) i,k,kchk,kk,n,nn,nlatm_this

  nlatm_this=nlat*nvmodes_keep/npe
  if(mod(nlat*nvmodes_keep,npe)/=0) nlatm_this=nlatm_this+1
  if(mod(nlat*nvmodes_keep,npe)==0) then
    kchk=npe
  else
    kchk=mod(nlat*nvmodes_keep,npe)
  end if

  nn=0
  do k=1,nvmodes_keep
    do i=1,nlat
      nn=nn+1
      mode_list(1,nn)=i
      mode_list(2,nn)=k
      mode_list(3,nn)=-1
    end do
  end do
  
  nlatm_0=-1
  nlatm_1=-2
  nn=0
  do n=1,npe
    if(n.le.kchk) then
      kk=nlatm_this
    else
      kk=nlatm_this-1
    end if
    if(kk.gt.0) then
      if(mype+1.eq.n) then
        nlatm_0=nn+1
        nlatm_1=nn+kk
      end if
      do k=1,kk
        nn=nn+1
        mode_list(3,nn)=n
      end do
    end if
  end do

end subroutine inmi_coupler_sd2ew0

subroutine inmi_coupler_sd2ew1(mype)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    inmi_coupler_sd2ew1
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


!  use mpi_alltoallv to move u_sd,v_sd,m_sd (subdomains) to uvm_ew (lat strips)

  use mod_vtrans, only: nvmodes_keep
  use gridmod, only: nlat,lon2,lat2,jstart,istart
  use mpimod, only: npe,mpi_comm_world,ierror,mpi_integer4
  implicit none


  integer(i_kind),intent(in)::mype

  integer(i_kind) mode2_list(nlat,nvmodes_keep)
  integer(i_kind) i,ii,ii0,ilat,imode,j,mm1,nn,nlonloc,ipe,ilatm,ilon,mpi_string1

  allocate(nsend_sd2ew(npe),nrecv_sd2ew(npe))
  allocate(ndsend_sd2ew(npe+1),ndrecv_sd2ew(npe+1))
  mm1=mype+1

  nn=0
  mode2_list=0
  do j=1,nlat*nvmodes_keep
    ilat=mode_list(1,j)
    imode=mode_list(2,j)
    if(mode2_list(ilat,imode).ne.0) then
           if(mype.eq.0) write(6,*)' problem in inmi_coupler_sd2ew'
                        call mpi_finalize(i)
                        stop
    end if
    mode2_list(ilat,imode)=j
  end do
  do imode=1,nvmodes_keep
    if(imode.eq.0) cycle
    do ilat=1,nlat
      if(mode2_list(ilat,imode).eq.0) then
           if(mype.eq.0) write(6,*)' problem in inmi_coupler_sd2ew'
                        call mpi_finalize(i)
                        stop
      end if
    end do
  end do

!  obtain counts of points to send to each pe from this pe

  nsend_sd2ew=0
  nlonloc=lon2-2
  do imode=1,nvmodes_keep
    if(imode.eq.0) cycle
    do i=2,lat2-1
      ilat=i+istart(mm1)-2
      j=mode2_list(ilat,imode)
      ipe=mode_list(3,j)
      nsend_sd2ew(ipe)=nsend_sd2ew(ipe)+nlonloc
    end do
  end do

  ndsend_sd2ew(1)=0
  do i=2,npe+1
    ndsend_sd2ew(i)=ndsend_sd2ew(i-1)+nsend_sd2ew(i-1)
  end do
  nallsend_sd2ew=ndsend_sd2ew(npe+1)
  allocate(info_send_sd2ew(2,nallsend_sd2ew))
  nsend_sd2ew=0
  do imode=1,nvmodes_keep
    if(imode.eq.0) cycle
    do i=2,lat2-1
      ilat=i+istart(mm1)-2
      ilatm=mode2_list(ilat,imode)
      ipe=mode_list(3,ilatm)
      do ii=2,lon2-1
        ilon=ii+jstart(mm1)-2
        nsend_sd2ew(ipe)=nsend_sd2ew(ipe)+1
        ii0=ndsend_sd2ew(ipe)+nsend_sd2ew(ipe)
        info_send_sd2ew(1,ii0)=ilon
        info_send_sd2ew(2,ii0)=ilatm
      end do
    end do
  end do

  call mpi_alltoall(nsend_sd2ew,1,mpi_integer4,nrecv_sd2ew,1,mpi_integer4,mpi_comm_world,ierror)
  ndrecv_sd2ew(1)=0
  do i=2,npe+1
    ndrecv_sd2ew(i)=ndrecv_sd2ew(i-1)+nrecv_sd2ew(i-1)
  end do
  nallrecv_sd2ew=ndrecv_sd2ew(npe+1)
  allocate(info_recv_sd2ew(2,nallrecv_sd2ew))
  call mpi_type_contiguous(2,mpi_integer4,mpi_string1,ierror)
  call mpi_type_commit(mpi_string1,ierror)
  call mpi_alltoallv(info_send_sd2ew,nsend_sd2ew,ndsend_sd2ew,mpi_string1, &
                     info_recv_sd2ew,nrecv_sd2ew,ndrecv_sd2ew,mpi_string1,mpi_comm_world,ierror)
  call mpi_type_free(mpi_string1,ierror)
    
end subroutine inmi_coupler_sd2ew1

subroutine inmi_coupler_sd2ew(u_sd1,v_sd1,m_sd1,u_sd2,v_sd2,m_sd2,uvm_ew,mype)

!$$$  subprogram documentation block
!                .      .    .
! subprogram:    init_coupler_sd2ew
!
!   prgrmmr:
!
! abstract:
!
! program history log:
!   2008-04-04  safford -- add subprogram doc block, rm unused vars
!
!   input argument list:
!     mype     - mpi task id
!     u_sd1    -
!     v_sd1    -
!     m_sd1    -
!     u_sd2    -
!     v_sd2    -
!     m_sd2    -
!
!   output argument list:
!     uvm_ew   -
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$

!  use mpi_alltoallv to move u_sd,v_sd,m_sd (subdomains) to uvm_ew (lat strips)

  use kinds, only: r_kind
  use mod_vtrans, only: nvmodes_keep
  use gridmod, only: nlon,lon2,lat2,jstart,istart
  use mpimod, only: mpi_comm_world,ierror,mpi_rtype
  implicit none


  integer(i_kind),intent(in)::mype
  real(r_kind),dimension(lat2,lon2,nvmodes_keep),intent(in)::u_sd1,v_sd1,m_sd1
  real(r_kind),dimension(lat2,lon2,nvmodes_keep),intent(in)::u_sd2,v_sd2,m_sd2
  real(r_kind),dimension(2,3,nlon,nlatm_0:nlatm_1),intent(out)::uvm_ew

  real(r_kind),allocatable::sendbuf(:,:,:),recvbuf(:,:,:)
  integer(i_kind) ilat,imode,j,mm1,ilatm,ilon,mpi_string1

  mm1=mype+1

  allocate(sendbuf(2,3,nallsend_sd2ew))
  do j=1,nallsend_sd2ew
    ilon=info_send_sd2ew(1,j)
    ilatm=info_send_sd2ew(2,j)
    ilat=mode_list(1,ilatm)
    imode=mode_list(2,ilatm)
    sendbuf(1,1,j)=u_sd1(ilat-istart(mm1)+2,ilon-jstart(mm1)+2,imode)
    sendbuf(1,2,j)=v_sd1(ilat-istart(mm1)+2,ilon-jstart(mm1)+2,imode)
    sendbuf(1,3,j)=m_sd1(ilat-istart(mm1)+2,ilon-jstart(mm1)+2,imode)
    sendbuf(2,1,j)=u_sd2(ilat-istart(mm1)+2,ilon-jstart(mm1)+2,imode)
    sendbuf(2,2,j)=v_sd2(ilat-istart(mm1)+2,ilon-jstart(mm1)+2,imode)
    sendbuf(2,3,j)=m_sd2(ilat-istart(mm1)+2,ilon-jstart(mm1)+2,imode)
  end do
  allocate(recvbuf(2,3,nallrecv_sd2ew))
  call mpi_type_contiguous(6,mpi_rtype,mpi_string1,ierror)
  call mpi_type_commit(mpi_string1,ierror)
  call mpi_alltoallv(sendbuf,nsend_sd2ew,ndsend_sd2ew,mpi_string1, &
                     recvbuf,nrecv_sd2ew,ndrecv_sd2ew,mpi_string1,mpi_comm_world,ierror)
  call mpi_type_free(mpi_string1,ierror)
  deallocate(sendbuf)
    
  do j=1,nallrecv_sd2ew
    ilon=info_recv_sd2ew(1,j)
    ilatm=info_recv_sd2ew(2,j)
    uvm_ew(1,1,ilon,ilatm)=recvbuf(1,1,j)
    uvm_ew(1,2,ilon,ilatm)=recvbuf(1,2,j)
    uvm_ew(1,3,ilon,ilatm)=recvbuf(1,3,j)
    uvm_ew(2,1,ilon,ilatm)=recvbuf(2,1,j)
    uvm_ew(2,2,ilon,ilatm)=recvbuf(2,2,j)
    uvm_ew(2,3,ilon,ilatm)=recvbuf(2,3,j)
  end do
  deallocate(recvbuf)
    
end subroutine inmi_coupler_sd2ew

subroutine inmi_coupler_ew2sd1(mype)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    inmi_coupler_ew2sd1
!
!   prgrmmr:
!
! abstract:
!
! program history log:
!   2008-04-04  safford -- add subprogram doc block, rm unused vars and uses
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

!  use mpi_alltoallv to move uvm_ew (lat strips) to u_sd,v_sd,m_sd (subdomains)

  use gridmod, only: nlon,jstart,istart,ilat1,jlon1
  use mpimod, only: npe,mpi_comm_world,ierror,mpi_integer4
  implicit none


  integer(i_kind),intent(in)::mype

  integer(i_kind) i,ilat,imode,j,k,mm1,ipe,ilon,mpi_string1,nn

  allocate(nsend_ew2sd(npe),nrecv_ew2sd(npe))
  allocate(ndsend_ew2sd(npe+1),ndrecv_ew2sd(npe+1))
  mm1=mype+1

!      1.  for each pe, gather up list of points from this set of lat strips destined
!             for subdomain of pe
  do ipe=1,npe
    nn=0
    do k=nlatm_0,nlatm_1
      ilat=mode_list(1,k)
      imode=mode_list(2,k)
      i=ilat-istart(ipe)+2
      if(i.lt.1.or.i.gt.ilat1(ipe)+2) cycle
      do j=1,jlon1(ipe)+2
        ilon=j+jstart(ipe)-2
        if(ilon.lt.1) ilon=ilon+nlon
        if(ilon.gt.nlon) ilon=ilon-nlon
        nn=nn+1
      end do
    end do
    nsend_ew2sd(ipe)=nn
  end do

  ndsend_ew2sd(1)=0
  do i=2,npe+1
    ndsend_ew2sd(i)=ndsend_ew2sd(i-1)+nsend_ew2sd(i-1)
  end do
  nallsend_ew2sd=ndsend_ew2sd(npe+1)
  allocate(info_send_ew2sd(3,nallsend_ew2sd))
  nn=0
  do ipe=1,npe
    do k=nlatm_0,nlatm_1
      ilat=mode_list(1,k)
      imode=mode_list(2,k)
      i=ilat-istart(ipe)+2
      if(i.lt.1.or.i.gt.ilat1(ipe)+2) cycle
      do j=1,jlon1(ipe)+2
        ilon=j+jstart(ipe)-2
        if(ilon.lt.1) ilon=ilon+nlon
        if(ilon.gt.nlon) ilon=ilon-nlon
        nn=nn+1
        info_send_ew2sd(1,nn)=ilon
        info_send_ew2sd(2,nn)=j
        info_send_ew2sd(3,nn)=k
      end do
    end do
  end do

  call mpi_alltoall(nsend_ew2sd,1,mpi_integer4,nrecv_ew2sd,1,mpi_integer4,mpi_comm_world,ierror)
  ndrecv_ew2sd(1)=0
  do i=2,npe+1
    ndrecv_ew2sd(i)=ndrecv_ew2sd(i-1)+nrecv_ew2sd(i-1)
  end do
  nallrecv_ew2sd=ndrecv_ew2sd(npe+1)
  allocate(info_recv_ew2sd(3,nallrecv_ew2sd))
  call mpi_type_contiguous(3,mpi_integer4,mpi_string1,ierror)
  call mpi_type_commit(mpi_string1,ierror)
  call mpi_alltoallv(info_send_ew2sd,nsend_ew2sd,ndsend_ew2sd,mpi_string1, &
                     info_recv_ew2sd,nrecv_ew2sd,ndrecv_ew2sd,mpi_string1,mpi_comm_world,ierror)
  call mpi_type_free(mpi_string1,ierror)

end subroutine inmi_coupler_ew2sd1

subroutine inmi_coupler_ew2sd(u_sd1,v_sd1,m_sd1,u_sd2,v_sd2,m_sd2,uvm_ew,mype)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    inmi_coupler_ew2sd
!
!   prgrmmr:
!
! abstract:
!
! program history log:
!   2008-04-04  safford -- add subprogram doc block, rm unused vars
!
!   input argument list:
!     mype     - mpi task id
!     uvm_ew   -
!
!   output argument list:
!     u_sd1    -
!     v_sd1    -
!     m_sd1    -
!     u_sd2    -
!     v_sd2    -
!     m_sd2    -
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$


!  use mpi_alltoallv to move uvm_ew (lat strips) to u_sd,v_sd,m_sd (subdomains)

  use kinds, only: r_kind
  use mod_vtrans, only: nvmodes_keep
  use gridmod, only: nlat,nlon,lon2,lat2,istart
  use mpimod, only: mpi_comm_world,ierror,mpi_rtype
  implicit none


  integer(i_kind),intent(in)::mype
  real(r_kind),dimension(lat2,lon2,nvmodes_keep),intent(out)::u_sd1,v_sd1,m_sd1
  real(r_kind),dimension(lat2,lon2,nvmodes_keep),intent(out)::u_sd2,v_sd2,m_sd2
  real(r_kind),dimension(2,3,nlon,nlatm_0:nlatm_1),intent(in)::uvm_ew

  real(r_kind),allocatable::sendbuf(:,:,:),recvbuf(:,:,:)
  integer(i_kind) ilat,imode,j,mm1,ilatm,ilon,mpi_string1,ilonloc

  mm1=mype+1

  allocate(sendbuf(2,3,nallsend_ew2sd))
  do j=1,nallsend_ew2sd
    ilon=info_send_ew2sd(1,j)
    ilatm=info_send_ew2sd(3,j)
    sendbuf(1,1,j)=uvm_ew(1,1,ilon,ilatm)
    sendbuf(1,2,j)=uvm_ew(1,2,ilon,ilatm)
    sendbuf(1,3,j)=uvm_ew(1,3,ilon,ilatm)
    sendbuf(2,1,j)=uvm_ew(2,1,ilon,ilatm)
    sendbuf(2,2,j)=uvm_ew(2,2,ilon,ilatm)
    sendbuf(2,3,j)=uvm_ew(2,3,ilon,ilatm)
  end do
  allocate(recvbuf(2,3,nallrecv_ew2sd))
  call mpi_type_contiguous(6,mpi_rtype,mpi_string1,ierror)
  call mpi_type_commit(mpi_string1,ierror)
  call mpi_alltoallv(sendbuf,nsend_ew2sd,ndsend_ew2sd,mpi_string1, &
                     recvbuf,nrecv_ew2sd,ndrecv_ew2sd,mpi_string1,mpi_comm_world,ierror)
  call mpi_type_free(mpi_string1,ierror)
  deallocate(sendbuf)
  do j=1,nallrecv_ew2sd
    ilonloc=info_recv_ew2sd(2,j)
    ilatm=info_recv_ew2sd(3,j)
    ilat=mode_list(1,ilatm)
    imode=mode_list(2,ilatm)
    u_sd1(ilat-istart(mm1)+2,ilonloc,imode)=recvbuf(1,1,j)
    v_sd1(ilat-istart(mm1)+2,ilonloc,imode)=recvbuf(1,2,j)
    m_sd1(ilat-istart(mm1)+2,ilonloc,imode)=recvbuf(1,3,j)
    u_sd2(ilat-istart(mm1)+2,ilonloc,imode)=recvbuf(2,1,j)
    v_sd2(ilat-istart(mm1)+2,ilonloc,imode)=recvbuf(2,2,j)
    m_sd2(ilat-istart(mm1)+2,ilonloc,imode)=recvbuf(2,3,j)
!--------------check for north or south pole
    ilat=-1
    if(mode_list(1,ilatm).eq.nlat) ilat=nlat+1
    if(mode_list(1,ilatm).eq.1) ilat=0
    if(ilat.eq.-1) cycle
!-----------------do repeat rows for north/south pole
    u_sd1(ilat-istart(mm1)+2,ilonloc,imode)=recvbuf(1,1,j)
    v_sd1(ilat-istart(mm1)+2,ilonloc,imode)=recvbuf(1,2,j)
    m_sd1(ilat-istart(mm1)+2,ilonloc,imode)=recvbuf(1,3,j)
    u_sd2(ilat-istart(mm1)+2,ilonloc,imode)=recvbuf(2,1,j)
    v_sd2(ilat-istart(mm1)+2,ilonloc,imode)=recvbuf(2,2,j)
    m_sd2(ilat-istart(mm1)+2,ilonloc,imode)=recvbuf(2,3,j)
  end do
  deallocate(recvbuf)

end subroutine inmi_coupler_ew2sd

subroutine inmi_ew_trans(uvm_ew,uvm_ewtrans)

!$$$  subprogram documentation block
!                .      .    .
! subprogram:    inmi_ew_trans
!
!   prgrmmr:
!
! abstract:
!
! program history log:
!   2008-04-04  safford -- add subprogram doc block, rm unused uses
!
!   input argument list:
!     uvm_ew   -
!
!   output argument list:
!     uvm_ewtrans -
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$

  use kinds, only: r_kind
  use gridmod, only: nlon
  use specmod, only: jcap,afft
  implicit none

  real(r_kind),dimension(2,3,nlon,nlatm_0:nlatm_1),intent(in)::uvm_ew
  real(r_kind),dimension(2,3,2,0:jcap,nlatm_0:nlatm_1),intent(out)::uvm_ewtrans

  integer(i_kind) i,j,k
  real(r_kind) grid(nlon,2),halfwave(2,0:nlon/2,2)

  do k=nlatm_0,nlatm_1
    do j=1,3
      do i=1,nlon
        grid(i,1)=uvm_ew(1,j,i,k)
        grid(i,2)=uvm_ew(2,j,i,k)
      end do
      call spffte(nlon,1+nlon/2,nlon,2,halfwave,grid,-1,afft)
      do i=0,jcap
        uvm_ewtrans(1,j,1,i,k)=halfwave(1,i,1)
        uvm_ewtrans(1,j,2,i,k)=halfwave(2,i,1)
        uvm_ewtrans(2,j,1,i,k)=halfwave(1,i,2)
        uvm_ewtrans(2,j,2,i,k)=halfwave(2,i,2)
      end do
    end do
  end do

end subroutine inmi_ew_trans

subroutine inmi_ew_invtrans_ad(uvm_ew,uvm_ewtrans)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    inmi_ew_invtrans_ad
!
!   prgrmmr:
!
! abstract:
!
! program history log:
!   2008-04-04  safford -- add subprogram doc block, rm unused uses
!
!   input argument list:
!     uvm_ew   -
!
!   output argument list:
!     uvm_ewtrans -
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$

  use kinds, only: r_kind
  use gridmod, only: nlon
  use specmod, only: jcap,afft
  use constants, only: two
  implicit none

  real(r_kind),dimension(2,3,nlon,nlatm_0:nlatm_1),intent(in)::uvm_ew
  real(r_kind),dimension(2,3,2,0:jcap,nlatm_0:nlatm_1),intent(out)::uvm_ewtrans

  integer(i_kind) i,j,k
  real(r_kind) grid(nlon,2),halfwave(2,0:nlon/2,2)

  do k=nlatm_0,nlatm_1
    do j=1,3
      do i=1,nlon
        grid(i,1)=uvm_ew(1,j,i,k)
        grid(i,2)=uvm_ew(2,j,i,k)
      end do
      call spffte(nlon,1+nlon/2,nlon,2,halfwave,grid,-1,afft)
      uvm_ewtrans(1,j,1,0,k)=halfwave(1,0,1)*float(nlon)
      uvm_ewtrans(1,j,2,0,k)=halfwave(2,0,1)*float(nlon)
      uvm_ewtrans(2,j,1,0,k)=halfwave(1,0,2)*float(nlon)
      uvm_ewtrans(2,j,2,0,k)=halfwave(2,0,2)*float(nlon)
      do i=1,jcap
        uvm_ewtrans(1,j,1,i,k)=halfwave(1,i,1)*two*float(nlon)
        uvm_ewtrans(1,j,2,i,k)=halfwave(2,i,1)*two*float(nlon)
        uvm_ewtrans(2,j,1,i,k)=halfwave(1,i,2)*two*float(nlon)
        uvm_ewtrans(2,j,2,i,k)=halfwave(2,i,2)*two*float(nlon)
      end do
    end do
  end do

end subroutine inmi_ew_invtrans_ad

subroutine inmi_ew_invtrans(uvm_ew,uvm_ewtrans)

!$$$  subprogram documentation block
!                .      .    .
! subprogram:    inmi_ew_invtrans
!
!   prgrmmr:
!
! abstract:
!
! program history log:
!   2008-04-04  safford -- add subprogram doc block, rm unused vars
!
!   input argument list:
!     uvm_ewtrans -
!
!   output argument list:
!     uvm_ew      -
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$

  use kinds, only: r_kind
  use gridmod, only: nlon
  use specmod, only: jcap,afft
  use constants, only: zero
  implicit none

  real(r_kind),dimension(2,3,nlon,nlatm_0:nlatm_1),intent(out)::uvm_ew
  real(r_kind),dimension(2,3,2,0:jcap,nlatm_0:nlatm_1),intent(in)::uvm_ewtrans

  integer(i_kind) i,j,k
  real(r_kind) grid(nlon,2),halfwave(2,0:nlon/2,2)

  do k=nlatm_0,nlatm_1
    do j=1,3
      do i=0,jcap
        halfwave(1,i,1)=uvm_ewtrans(1,j,1,i,k)
        halfwave(2,i,1)=uvm_ewtrans(1,j,2,i,k)
        halfwave(1,i,2)=uvm_ewtrans(2,j,1,i,k)
        halfwave(2,i,2)=uvm_ewtrans(2,j,2,i,k)
      end do
      do i=jcap+1,nlon/2
        halfwave(1,i,1)=zero
        halfwave(2,i,1)=zero
        halfwave(1,i,2)=zero
        halfwave(2,i,2)=zero
      end do
      call spffte(nlon,1+nlon/2,nlon,2,halfwave,grid,1,afft)
      do i=1,nlon
        uvm_ew(1,j,i,k)=grid(i,1)
        uvm_ew(2,j,i,k)=grid(i,2)
      end do
    end do
  end do

end subroutine inmi_ew_invtrans

subroutine inmi_ew_trans_ad(uvm_ew,uvm_ewtrans)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    inmi_ew_trans_ad
!
!   prgrmmr:
!
! abstract:
!
! program history log:
!   2008-04-04  safford -- add subprogram doc block, rm unused vars
!
!   input argument list:
!     uvm_ewtrans -
!
!   output argument list:
!   output argument list:
!     uvm_ew      -
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$


  use kinds, only: r_kind
  use gridmod, only: nlon
  use specmod, only: jcap,afft
  use constants, only: zero,half
  implicit none

  real(r_kind),dimension(2,3,nlon,nlatm_0:nlatm_1),intent(out)::uvm_ew
  real(r_kind),dimension(2,3,2,0:jcap,nlatm_0:nlatm_1),intent(in)::uvm_ewtrans

  integer(i_kind) i,j,k
  real(r_kind) grid(nlon,2),halfwave(2,0:nlon/2,2)

  do k=nlatm_0,nlatm_1
    do j=1,3
      halfwave(1,0,1)=uvm_ewtrans(1,j,1,0,k)/float(nlon)
      halfwave(2,0,1)=zero
      halfwave(1,0,2)=uvm_ewtrans(2,j,1,0,k)/float(nlon)
      halfwave(2,0,2)=zero
      do i=1,jcap
        halfwave(1,i,1)=half*uvm_ewtrans(1,j,1,i,k)/float(nlon)
        halfwave(2,i,1)=half*uvm_ewtrans(1,j,2,i,k)/float(nlon)
        halfwave(1,i,2)=half*uvm_ewtrans(2,j,1,i,k)/float(nlon)
        halfwave(2,i,2)=half*uvm_ewtrans(2,j,2,i,k)/float(nlon)
      end do
      do i=jcap+1,nlon/2
        halfwave(1,i,1)=zero
        halfwave(2,i,1)=zero
        halfwave(1,i,2)=zero
        halfwave(2,i,2)=zero
      end do
      call spffte(nlon,1+nlon/2,nlon,2,halfwave,grid,1,afft)
      do i=1,nlon
        uvm_ew(1,j,i,k)=grid(i,1)
        uvm_ew(2,j,i,k)=grid(i,2)
      end do
    end do
  end do

end subroutine inmi_ew_trans_ad

subroutine inmi_coupler_ew2ns0(mype)

!$$$  subprogram documentation block
!                .      .    .
! subprogram:    inmi_coupler_ew2ns0
!
!   prgrmmr:
!
! abstract:
!
! program history log:
!   2008-04-04  safford -- add subprogram doc block, rm unused uses
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

  use mod_vtrans, only: nvmodes_keep
  use mpimod, only: npe
  use specmod, only: jcap
  implicit none

  integer(i_kind),intent(in)::mype

  integer(i_kind) k,kk,m,n,num_per_pe,total_groups,nn,kchk

!   in laying out by zonal wave number/vertical mode, have two types of groupings:

!   1.  jcap odd: 

!     group zonal wave numbers in pairs as follows:

!      (0,(jcap+1)/2)    then    ( m,jcap+1-m)  for m=1,(jcap-1)/2

!   2.  jcap even:

!     0  (+,- mode pair together),  then   (m,jcap+1-m ) for m=1,jcap/2  single modes

  total_groups=(jcap+1)*nvmodes_keep
  num_per_pe=total_groups/npe
  if(mod(total_groups,npe)/=0) num_per_pe=num_per_pe+1
  if(mod(total_groups,npe)==0) then
    kchk=npe
  else
    kchk=mod(total_groups,npe)
  end if

  if(mod(jcap,2).ne.0) then

!    case  jcap odd:
   
    nn=0
    do k=1,nvmodes_keep
      nn=nn+1
      mmode_list(1,nn)=0
      mmode_list(2,nn)=(jcap+1)/2
      mmode_list(3,nn)=k
      mmode_list(4,nn)=k
      mmode_list(5,nn)=-1
      do m=1,(jcap-1)/2
        nn=nn+1
        mmode_list(1,nn)=m
        mmode_list(2,nn)=jcap+1-m
        mmode_list(3,nn)=k
        mmode_list(4,nn)=k
        mmode_list(5,nn)=-1
      end do
    end do
    do k=1,nvmodes_keep
      nn=nn+1
      mmode_list(1,nn)=0
      mmode_list(2,nn)=(jcap+1)/2
      mmode_list(3,nn)=-k
      mmode_list(4,nn)=-k
      mmode_list(5,nn)=-1
      do m=1,(jcap-1)/2
        nn=nn+1
        mmode_list(1,nn)=m
        mmode_list(2,nn)=jcap+1-m
        mmode_list(3,nn)=-k
        mmode_list(4,nn)=-k
        mmode_list(5,nn)=-1
      end do
    end do

  else

!    case  jcap even:
   
    nn=0
    do k=1,nvmodes_keep
      nn=nn+1
      mmode_list(1,nn)=0
      mmode_list(2,nn)=0
      mmode_list(3,nn)=k
      mmode_list(4,nn)=-k
      mmode_list(5,nn)=-1
      do m=1,jcap/2
        nn=nn+1
        mmode_list(1,nn)=m
        mmode_list(2,nn)=jcap+1-m
        mmode_list(3,nn)=k
        mmode_list(4,nn)=k
        mmode_list(5,nn)=-1
      end do
    end do
    do k=1,nvmodes_keep
      do m=1,jcap/2
        nn=nn+1
        mmode_list(1,nn)=m
        mmode_list(2,nn)=jcap+1-m
        mmode_list(3,nn)=-k
        mmode_list(4,nn)=-k
        mmode_list(5,nn)=-1
      end do
    end do

  end if

  m_0=-1
  m_1=-2
  nn=0
  do n=1,npe
    if(n.le.kchk) then
      kk=num_per_pe
    else
      kk=num_per_pe-1
    end if
    if(kk.gt.0) then
      if(mype+1.eq.n) then
        m_0=nn+1
        m_1=nn+kk
      end if
      do k=1,kk
        nn=nn+1
        mmode_list(5,nn)=n
      end do
    end if
  end do

end subroutine inmi_coupler_ew2ns0

subroutine inmi_coupler_ew2ns1(mype)

!$$$  subprogram documentation block
!                .      .    .
! subprogram:    inmi_coupler_ew2ns1
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

  use mod_vtrans, only: nvmodes_keep
  use mpimod, only: npe,mpi_comm_world,ierror,mpi_integer4,mpi_sum
  use specmod, only: jcap
  implicit none

  integer(i_kind),intent(in)::mype

  integer(i_kind) mmode2_list(0:jcap,-nvmodes_keep:nvmodes_keep)
  integer(i_kind) i,ip12,ipe,j,k,m,nn,m1,m2,ilat,imode,imode1,imode2
  integer(i_kind) mpi_string1
  integer(i_kind) ibad,ibad0,loop

  allocate(nsend(npe),nrecv(npe),ndsend(npe+1),ndrecv(npe+1))
  nn=0
  mmode2_list=0
  do j=1,(jcap+1)*nvmodes_keep
    m1=mmode_list(1,j)
    m2=mmode_list(2,j)
    imode1=mmode_list(3,j)
    imode2=mmode_list(4,j)
    if(imode1.eq.imode2) then
      if(mmode2_list(m1,imode1).ne.0.or.mmode2_list(m2,imode1).ne.0) then
            if(mype.eq.0) write(6,*)' problem in inmi_coupler_ew2ns'
                        call mpi_finalize(i)
                        stop
      end if
      mmode2_list(m1,imode1)=j
      mmode2_list(m2,imode1)=j
    end if
    if(m1.eq.m2) then
      if(mmode2_list(m1,imode1).ne.0.or.mmode2_list(m1,imode2).ne.0) then
            if(mype.eq.0) write(6,*)' problem in inmi_coupler_ew2ns'
                        call mpi_finalize(i)
                        stop
      end if
      mmode2_list(m1,imode1)=j
      mmode2_list(m1,imode2)=j
    end if
  end do
  do imode=-nvmodes_keep,nvmodes_keep
    if(imode.eq.0) cycle
    do m=0,jcap
      if(mmode2_list(m,imode).eq.0) then
           if(mype.eq.0) write(6,*)' problem in inmi_coupler_ew2ns'
                        call mpi_finalize(i)
                        stop
      end if
    end do
  end do
                
!  obtain counts of points to send to each pe from this pe

  nsend=0
  do k=nlatm_0,nlatm_1
    imode=mode_list(2,k)
    do m=0,jcap
      j=mmode2_list(m,imode)
      ipe=mmode_list(5,j)
      nsend(ipe)=nsend(ipe)+1
      j=mmode2_list(m,-imode)
      ipe=mmode_list(5,j)
      nsend(ipe)=nsend(ipe)+1
    end do
  end do

  ndsend(1)=0
  do i=2,npe+1
    ndsend(i)=ndsend(i-1)+nsend(i-1)
  end do
  nallsend=ndsend(npe+1)
  allocate(info_send(6,nallsend))
  nsend=0
   ibad=0
  do k=nlatm_0,nlatm_1
    ilat=mode_list(1,k)
    do loop=1,2
      imode=mode_list(2,k)
      if(loop.eq.2) imode=-mode_list(2,k)
      do m=0,jcap
        j=mmode2_list(m,imode)
        m1=mmode_list(1,j)
        m2=mmode_list(2,j)
        imode1=mmode_list(3,j)
        imode2=mmode_list(4,j)
        ipe=mmode_list(5,j)
        ip12=0
        if(m1.eq.m2.and.imode.eq.imode1) ip12=1
        if(m1.eq.m2.and.imode.eq.imode2) ip12=2
        if(imode1.eq.imode2.and.m.eq.m1) ip12=1
        if(imode1.eq.imode2.and.m.eq.m2) ip12=2
             if(ip12.eq.0) ibad=ibad+1
        ipe=mmode_list(5,j)
        nsend(ipe)=nsend(ipe)+1
        info_send(1,ndsend(ipe)+nsend(ipe))=k
        info_send(2,ndsend(ipe)+nsend(ipe))=ilat
        info_send(3,ndsend(ipe)+nsend(ipe))=imode
        info_send(4,ndsend(ipe)+nsend(ipe))=m
        info_send(5,ndsend(ipe)+nsend(ipe))=j
        info_send(6,ndsend(ipe)+nsend(ipe))=ip12
      end do
    end do
  end do

     call mpi_allreduce(ibad,ibad0,1,mpi_integer4,mpi_sum,mpi_comm_world,ierror)
     if(ibad0.gt.0) then
         if(mype.eq.0) write(0,*)' ibad = ',ibad0,'  inconsistency in inmi_coupler_ew2ns1'
         call mpi_finalize(ierror)
         stop
     end if

  call mpi_alltoall(nsend,1,mpi_integer4,nrecv,1,mpi_integer4,mpi_comm_world,ierror)
  ndrecv(1)=0
  do i=2,npe+1
    ndrecv(i)=ndrecv(i-1)+nrecv(i-1)
  end do
  nallrecv=ndrecv(npe+1)
  allocate(info_recv(6,nallrecv))
  call mpi_type_contiguous(6,mpi_integer4,mpi_string1,ierror)
  call mpi_type_commit(mpi_string1,ierror)
  call mpi_alltoallv(info_send,nsend,ndsend,mpi_string1, &
                     info_recv,nrecv,ndrecv,mpi_string1,mpi_comm_world,ierror)
  call mpi_type_free(mpi_string1,ierror)

end subroutine inmi_coupler_ew2ns1

subroutine inmi_coupler_ew2ns(uvm_ewtrans,uvm_ns)

!$$$  subprogram documentation block
!                .      .    .
! subprogram:    inmi_coupler_ew2ns
!
!   prgrmmr:
!
! abstract:
!
! program history log:
!   2008-04-04  safford -- add subprogram doc block, rm unused vars and uses
!
!   input argument list:
!     uvm_ewtrans -
!
!   output argument list:
!     uvm_ns      -
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$

  use kinds, only: r_kind
  use gridmod, only: nlat
  use mpimod, only: mpi_comm_world,ierror,mpi_rtype
  use specmod, only: jcap
  implicit none

  real(r_kind),dimension(2,3,2,0:jcap,nlatm_0:nlatm_1),intent(in)::uvm_ewtrans
  real(r_kind),dimension(3,2,nlat,2,m_0:m_1),intent(out)::uvm_ns

  integer(i_kind) ip12,j,m,mm,ilat,ilatm,imode
  integer(i_kind) mpi_string1
  real(r_kind),allocatable::sendbuf(:,:,:),recvbuf(:,:,:)
  integer(i_kind) loop

  allocate(sendbuf(3,2,nallsend))
  do j=1,nallsend
    ilatm=info_send(1,j)
    imode=info_send(3,j)
    m=info_send(4,j)
    loop=1
    if(imode.lt.0) loop=2
    sendbuf(1,1,j)=uvm_ewtrans(loop,1,1,m,ilatm)
    sendbuf(2,1,j)=uvm_ewtrans(loop,2,1,m,ilatm)
    sendbuf(3,1,j)=uvm_ewtrans(loop,3,1,m,ilatm)
    sendbuf(1,2,j)=uvm_ewtrans(loop,1,2,m,ilatm)
    sendbuf(2,2,j)=uvm_ewtrans(loop,2,2,m,ilatm)
    sendbuf(3,2,j)=uvm_ewtrans(loop,3,2,m,ilatm)
  end do
  allocate(recvbuf(3,2,nallrecv))
  call mpi_type_contiguous(6,mpi_rtype,mpi_string1,ierror)
  call mpi_type_commit(mpi_string1,ierror)
  call mpi_alltoallv(sendbuf,nsend,ndsend,mpi_string1, &
                     recvbuf,nrecv,ndrecv,mpi_string1,mpi_comm_world,ierror)
  call mpi_type_free(mpi_string1,ierror)
  deallocate(sendbuf)

  do j=1,nallrecv
    ilat=info_recv(2,j)
    mm=info_recv(5,j)
    ip12=info_recv(6,j)
    uvm_ns(1,1,ilat,ip12,mm)=recvbuf(1,1,j)
    uvm_ns(2,1,ilat,ip12,mm)=recvbuf(2,1,j)
    uvm_ns(3,1,ilat,ip12,mm)=recvbuf(3,1,j)
    uvm_ns(1,2,ilat,ip12,mm)=recvbuf(1,2,j)
    uvm_ns(2,2,ilat,ip12,mm)=recvbuf(2,2,j)
    uvm_ns(3,2,ilat,ip12,mm)=recvbuf(3,2,j)
  end do
  deallocate(recvbuf)

end subroutine inmi_coupler_ew2ns

subroutine inmi_coupler_ns2ew(uvm_ewtrans,uvm_ns)

!$$$  subprogram documentation block
!                .      .    .
! subprogram:    inmi_coupler_ns2ew
!
!   prgrmmr:
!
! abstract:
!
! program history log:
!   2008-04-04  safford -- add subprogram doc block, rm unused vars and uses
!
!   input argument list:
!     uvm_ns   -
!
!   output argument list:
!     uvm_ewtrans -
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$

  use kinds, only: r_kind
  use gridmod, only: nlat
  use mpimod, only: mpi_comm_world,ierror,mpi_rtype
  use specmod, only: jcap
  implicit none

  real(r_kind),dimension(2,3,2,0:jcap,nlatm_0:nlatm_1),intent(out)::uvm_ewtrans
  real(r_kind),dimension(3,2,nlat,2,m_0:m_1),intent(in)::uvm_ns

  integer(i_kind) ip12,j,m,mm,ilat,ilatm,imode
  integer(i_kind) mpi_string1
  real(r_kind),allocatable::sendbuf(:,:,:),recvbuf(:,:,:)
       integer(i_kind) loop


  allocate(recvbuf(3,2,nallrecv))
  do j=1,nallrecv
    ilat=info_recv(2,j)
    mm=info_recv(5,j)
    ip12=info_recv(6,j)
    recvbuf(1,1,j)=uvm_ns(1,1,ilat,ip12,mm)
    recvbuf(2,1,j)=uvm_ns(2,1,ilat,ip12,mm)
    recvbuf(3,1,j)=uvm_ns(3,1,ilat,ip12,mm)
    recvbuf(1,2,j)=uvm_ns(1,2,ilat,ip12,mm)
    recvbuf(2,2,j)=uvm_ns(2,2,ilat,ip12,mm)
    recvbuf(3,2,j)=uvm_ns(3,2,ilat,ip12,mm)
  end do
  allocate(sendbuf(3,2,nallsend))
  call mpi_type_contiguous(6,mpi_rtype,mpi_string1,ierror)
  call mpi_type_commit(mpi_string1,ierror)
  call mpi_alltoallv(recvbuf,nrecv,ndrecv,mpi_string1, &
                     sendbuf,nsend,ndsend,mpi_string1,mpi_comm_world,ierror)
  call mpi_type_free(mpi_string1,ierror)
  deallocate(recvbuf)
  do j=1,nallsend
    ilatm=info_send(1,j)
    imode=info_send(3,j)
    m=info_send(4,j)
    loop=1
    if(imode.lt.0) loop=2
    uvm_ewtrans(loop,1,1,m,ilatm)=sendbuf(1,1,j)
    uvm_ewtrans(loop,2,1,m,ilatm)=sendbuf(2,1,j)
    uvm_ewtrans(loop,3,1,m,ilatm)=sendbuf(3,1,j)
    uvm_ewtrans(loop,1,2,m,ilatm)=sendbuf(1,2,j)
    uvm_ewtrans(loop,2,2,m,ilatm)=sendbuf(2,2,j)
    uvm_ewtrans(loop,3,2,m,ilatm)=sendbuf(3,2,j)
  end do
  deallocate(sendbuf)

end subroutine inmi_coupler_ns2ew

subroutine inmi_nsuvm2zdm(uvm_ns,zdm_hat)

!$$$  subprogram documentation block
!                .      .    .
! subprogram:    inmi_nsuv2zdm
!
!   prgrmmr:
!
! abstract:
!
! program history log:
!   2008-04-04  safford -- add subprogram doc block, rm unused uses
!
!   input argument list:
!     uvm_ns   -
!
!   output argument list:
!     zdm_had  -
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!   machine:   ibm RS/6000 SP
!
!$$$



  use kinds, only: r_kind
  use gridmod, only: nlat
  use specmod, only: jcap,jb,je,pln,plntop,enn1,elonn1,eon,eontop,wlat,clat
  use constants, only: zero
  implicit none

  real(r_kind),dimension(3,2,nlat,2,m_0:m_1),intent(in)::uvm_ns
  real(r_kind),dimension(3,2,nlat,2,m_0:m_1),intent(out)::zdm_hat

  integer(i_kind) i,ics,j,jnorth,jsouth,m,mm,n,ipair
  real(r_kind) spcz(2,0:jcap),spcd(2,0:jcap),spcp(2,0:jcap),spcu(2,0:jcap+1),spcv(2,0:jcap+1)
  real(r_kind) plnloc(0:jcap+1)
  real(r_kind) fu(2,2),fv(2,2),fp(2,2)


  do mm=m_0,m_1
    do ipair=1,2
      m=mmode_list(ipair,mm)
      ics=1+m*(2*jcap+3-m)/2

      do n=m,jcap
        spcp(1,n)=zero
        spcp(2,n)=zero
      end do
      do n=m,jcap+1
        spcu(1,n)=zero
        spcu(2,n)=zero
        spcv(1,n)=zero
        spcv(2,n)=zero
      end do

      do j=jb,je
        jsouth=1+j
        jnorth=nlat-j

        fu(1,1)=uvm_ns(1,1,jnorth,ipair,mm)/clat(j)**2
        fu(2,1)=uvm_ns(1,2,jnorth,ipair,mm)/clat(j)**2
        fu(1,2)=uvm_ns(1,1,jsouth,ipair,mm)/clat(j)**2
        fu(2,2)=uvm_ns(1,2,jsouth,ipair,mm)/clat(j)**2
        fv(1,1)=uvm_ns(2,1,jnorth,ipair,mm)/clat(j)**2
        fv(2,1)=uvm_ns(2,2,jnorth,ipair,mm)/clat(j)**2
        fv(1,2)=uvm_ns(2,1,jsouth,ipair,mm)/clat(j)**2
        fv(2,2)=uvm_ns(2,2,jsouth,ipair,mm)/clat(j)**2
        fp(1,1)=uvm_ns(3,1,jnorth,ipair,mm)
        fp(2,1)=uvm_ns(3,2,jnorth,ipair,mm)
        fp(1,2)=uvm_ns(3,1,jsouth,ipair,mm)
        fp(2,2)=uvm_ns(3,2,jsouth,ipair,mm)
!           create plnloc

        do n=m,jcap
          plnloc(n)=pln(ics+n-m,j)
        end do
        plnloc(jcap+1)=plntop(m+1,j)

        call spanaly_ns(0,jcap,m,wlat(j),clat(j),plnloc(m),1,fu,spcu(1,m))
        call spanaly_ns(0,jcap,m,wlat(j),clat(j),plnloc(m),1,fv,spcv(1,m))
        call spanaly_ns(0,jcap,m,wlat(j),clat(j),plnloc(m),0,fp,spcp(1,m))

      end do

      call spuv2dz_ns(0,jcap,m,enn1(ics),elonn1(ics),eon(ics),eontop(m+1), &
              spcu(1,m),spcv(1,m),spcu(1,jcap+1),spcv(1,jcap+1),spcd(1,m),spcz(1,m))

      i=0
      do n=m,jcap
        i=i+1
        zdm_hat(1,1,i,ipair,mm)=spcz(1,n)
        zdm_hat(1,2,i,ipair,mm)=spcz(2,n)
        zdm_hat(2,1,i,ipair,mm)=spcd(1,n)
        zdm_hat(2,2,i,ipair,mm)=spcd(2,n)
        zdm_hat(3,1,i,ipair,mm)=spcp(1,n)
        zdm_hat(3,2,i,ipair,mm)=spcp(2,n)
      end do

    end do

  end do

end subroutine inmi_nsuvm2zdm

subroutine inmi_nszdm2uvm_ad(uvm_ns,zdm_hat)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    inmi_nszdm2uvm_ad
!
!   prgrmmr:
!
! abstract:
!
! program history log:
!   2008-04-04  safford -- add subprogram doc block, rm unused uses
!
!   input argument list:
!     uvm_ns   -
!
!   output argument list:
!     zdm_had  -
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$

  use kinds, only: r_kind
  use gridmod, only: nlat
  use specmod, only: jcap,jb,je,pln,plntop,enn1,elonn1,eon,eontop,wlat,clat
  use constants, only: zero
  implicit none

  real(r_kind),dimension(3,2,nlat,2,m_0:m_1),intent(in)::uvm_ns
  real(r_kind),dimension(3,2,nlat,2,m_0:m_1),intent(out)::zdm_hat

  real(r_kind),dimension(3,2,nlat,2,m_0:m_1)::uvm_ns_temp
  integer(i_kind) i,ics,j,jnorth,jsouth,m,mm,n,ipair
  real(r_kind) spcz(2,0:jcap),spcd(2,0:jcap),spcp(2,0:jcap),spcu(2,0:jcap+1),spcv(2,0:jcap+1)
  real(r_kind) plnloc(0:jcap+1)
  real(r_kind) fu(2,2),fv(2,2),fp(2,2)


  uvm_ns_temp=uvm_ns
  do mm=m_0,m_1
    do ipair=1,2
      m=mmode_list(ipair,mm)
      ics=1+m*(2*jcap+3-m)/2

      do n=m,jcap
        spcp(1,n)=zero
        spcp(2,n)=zero
      end do
      do n=m,jcap+1
        spcu(1,n)=zero
        spcu(2,n)=zero
        spcv(1,n)=zero
        spcv(2,n)=zero
      end do

!  adjoint of set pole values
      if(m.eq.0) then
        uvm_ns_temp(3,1,2,ipair,mm)=uvm_ns_temp(3,1,1,ipair,mm)+uvm_ns_temp(3,1,2,ipair,mm)
        uvm_ns_temp(3,1,nlat-1,ipair,mm)=uvm_ns_temp(3,1,nlat,ipair,mm)+uvm_ns_temp(3,1,nlat-1,ipair,mm)
      else if(m.eq.1) then
        uvm_ns_temp(1,1,2,ipair,mm)=uvm_ns_temp(1,1,1,ipair,mm)+uvm_ns_temp(1,1,2,ipair,mm)
        uvm_ns_temp(1,2,2,ipair,mm)=uvm_ns_temp(1,2,1,ipair,mm)+uvm_ns_temp(1,2,2,ipair,mm)
        uvm_ns_temp(2,1,2,ipair,mm)=uvm_ns_temp(2,1,1,ipair,mm)+uvm_ns_temp(2,1,2,ipair,mm)
        uvm_ns_temp(2,2,2,ipair,mm)=uvm_ns_temp(2,2,1,ipair,mm)+uvm_ns_temp(2,2,2,ipair,mm)
        uvm_ns_temp(1,1,nlat-1,ipair,mm)=uvm_ns_temp(1,1,nlat,ipair,mm)+uvm_ns_temp(1,1,nlat-1,ipair,mm)
        uvm_ns_temp(1,2,nlat-1,ipair,mm)=uvm_ns_temp(1,2,nlat,ipair,mm)+uvm_ns_temp(1,2,nlat-1,ipair,mm)
        uvm_ns_temp(2,1,nlat-1,ipair,mm)=uvm_ns_temp(2,1,nlat,ipair,mm)+uvm_ns_temp(2,1,nlat-1,ipair,mm)
        uvm_ns_temp(2,2,nlat-1,ipair,mm)=uvm_ns_temp(2,2,nlat,ipair,mm)+uvm_ns_temp(2,2,nlat-1,ipair,mm)
      end if

      do j=jb,je
        jsouth=1+j
        jnorth=nlat-j

        fu(1,1)=uvm_ns_temp(1,1,jnorth,ipair,mm)/(wlat(j)*clat(j)**2)
        fu(2,1)=uvm_ns_temp(1,2,jnorth,ipair,mm)/(wlat(j)*clat(j)**2)
        fu(1,2)=uvm_ns_temp(1,1,jsouth,ipair,mm)/(wlat(j)*clat(j)**2)
        fu(2,2)=uvm_ns_temp(1,2,jsouth,ipair,mm)/(wlat(j)*clat(j)**2)
        fv(1,1)=uvm_ns_temp(2,1,jnorth,ipair,mm)/(wlat(j)*clat(j)**2)
        fv(2,1)=uvm_ns_temp(2,2,jnorth,ipair,mm)/(wlat(j)*clat(j)**2)
        fv(1,2)=uvm_ns_temp(2,1,jsouth,ipair,mm)/(wlat(j)*clat(j)**2)
        fv(2,2)=uvm_ns_temp(2,2,jsouth,ipair,mm)/(wlat(j)*clat(j)**2)
        fp(1,1)=uvm_ns_temp(3,1,jnorth,ipair,mm)/wlat(j)
        fp(2,1)=uvm_ns_temp(3,2,jnorth,ipair,mm)/wlat(j)
        fp(1,2)=uvm_ns_temp(3,1,jsouth,ipair,mm)/wlat(j)
        fp(2,2)=uvm_ns_temp(3,2,jsouth,ipair,mm)/wlat(j)
!           create plnloc

        do n=m,jcap
          plnloc(n)=pln(ics+n-m,j)
        end do
        plnloc(jcap+1)=plntop(m+1,j)

        call spanaly_ns(0,jcap,m,wlat(j),clat(j),plnloc(m),1,fu,spcu(1,m))
        call spanaly_ns(0,jcap,m,wlat(j),clat(j),plnloc(m),1,fv,spcv(1,m))
        call spanaly_ns(0,jcap,m,wlat(j),clat(j),plnloc(m),0,fp,spcp(1,m))

      end do

      call spuv2dz_ns(0,jcap,m,enn1(ics),elonn1(ics),eon(ics),eontop(m+1), &
              spcu(1,m),spcv(1,m),spcu(1,jcap+1),spcv(1,jcap+1),spcd(1,m),spcz(1,m))

      i=0
      if(m.eq.0) then
        i=i+1
        zdm_hat(1,1,i,ipair,mm)=zero
        zdm_hat(1,2,i,ipair,mm)=zero
        zdm_hat(2,1,i,ipair,mm)=zero
        zdm_hat(2,2,i,ipair,mm)=zero
        zdm_hat(3,1,i,ipair,mm)=spcp(1,0)
        zdm_hat(3,2,i,ipair,mm)=spcp(2,0)
      end if
      do n=max(1,m),jcap
        i=i+1
        zdm_hat(1,1,i,ipair,mm)=spcz(1,n)/enn1(ics+n-m)
        zdm_hat(1,2,i,ipair,mm)=spcz(2,n)/enn1(ics+n-m)
        zdm_hat(2,1,i,ipair,mm)=spcd(1,n)/enn1(ics+n-m)
        zdm_hat(2,2,i,ipair,mm)=spcd(2,n)/enn1(ics+n-m)
        zdm_hat(3,1,i,ipair,mm)=spcp(1,n)
        zdm_hat(3,2,i,ipair,mm)=spcp(2,n)
      end do

    end do

  end do

end subroutine inmi_nszdm2uvm_ad

subroutine inmi_nszdm2uvm(uvm_ns,zdm_hat)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    inmi_szdm2uvm
!
!   prgrmmr:
!
! abstract:
!
! program history log:
!   2008-04-04  safford -- add subprogram doc block, rm unused uses
!
!   input argument list:
!     uvm_ns   -
!
!   output argument list:
!     zdm_had  -
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$

  use kinds, only: r_kind
  use gridmod, only: nlat
  use specmod, only: jcap,jb,je,pln,plntop,elonn1,eon,eontop,clat
  use constants, only: zero
  implicit none

  real(r_kind),dimension(3,2,nlat,2,m_0:m_1),intent(out)::uvm_ns
  real(r_kind),dimension(3,2,nlat,2,m_0:m_1),intent(in)::zdm_hat

  integer(i_kind) i,ics,j,jnorth,jsouth,m,mm,n,ipair
  real(r_kind) spcz(2,0:jcap),spcd(2,0:jcap),spcp(2,0:jcap),spcu(2,0:jcap+1),spcv(2,0:jcap+1)
  real(r_kind) plnloc(0:jcap+1)
  real(r_kind) fu(2,2),fv(2,2),fp(2,2)


  do mm=m_0,m_1
    do ipair=1,2
      m=mmode_list(ipair,mm)
      ics=1+m*(2*jcap+3-m)/2

!           gather up spcz, spcd, spcp

      i=0
      do n=m,jcap
        i=i+1
        spcz(1,n)=zdm_hat(1,1,i,ipair,mm)
        spcz(2,n)=zdm_hat(1,2,i,ipair,mm)
        spcd(1,n)=zdm_hat(2,1,i,ipair,mm)
        spcd(2,n)=zdm_hat(2,2,i,ipair,mm)
        spcp(1,n)=zdm_hat(3,1,i,ipair,mm)
        spcp(2,n)=zdm_hat(3,2,i,ipair,mm)
      end do

!           convert to spcu, spcv

      call spdz2uv_ns(0,jcap,m,elonn1(ics),eon(ics),eontop(m+1), &
              spcd(1,m),spcz(1,m),spcu(1,m),spcv(1,m),spcu(1,jcap+1),spcv(1,jcap+1))

      do j=jb,je
        jsouth=1+j
        jnorth=nlat-j

!           create plnloc

        do n=m,jcap
          plnloc(n)=pln(ics+n-m,j)
        end do
        plnloc(jcap+1)=plntop(m+1,j)

!          obtain f

        call spsynth_ns(0,jcap,m,clat(j),plnloc(m),1,spcu(1,m),fu)
        call spsynth_ns(0,jcap,m,clat(j),plnloc(m),1,spcv(1,m),fv)
        call spsynth_ns(0,jcap,m,clat(j),plnloc(m),0,spcp(1,m),fp)

!          scatter back to output pairs of lats

        uvm_ns(1,1,jnorth,ipair,mm)=fu(1,1)
        uvm_ns(1,2,jnorth,ipair,mm)=fu(2,1)
        uvm_ns(1,1,jsouth,ipair,mm)=fu(1,2)
        uvm_ns(1,2,jsouth,ipair,mm)=fu(2,2)
        uvm_ns(2,1,jnorth,ipair,mm)=fv(1,1)
        uvm_ns(2,2,jnorth,ipair,mm)=fv(2,1)
        uvm_ns(2,1,jsouth,ipair,mm)=fv(1,2)
        uvm_ns(2,2,jsouth,ipair,mm)=fv(2,2)
        uvm_ns(3,1,jnorth,ipair,mm)=fp(1,1)
        uvm_ns(3,2,jnorth,ipair,mm)=fp(2,1)
        uvm_ns(3,1,jsouth,ipair,mm)=fp(1,2)
        uvm_ns(3,2,jsouth,ipair,mm)=fp(2,2)

      end do

!  set pole values
      if(m.eq.0) then
        uvm_ns(1,1,1,ipair,mm)=zero
        uvm_ns(1,2,1,ipair,mm)=zero
        uvm_ns(2,1,1,ipair,mm)=zero
        uvm_ns(2,2,1,ipair,mm)=zero
        uvm_ns(3,1,1,ipair,mm)=uvm_ns(3,1,2,ipair,mm)
        uvm_ns(3,2,1,ipair,mm)=zero
        uvm_ns(1,1,nlat,ipair,mm)=zero
        uvm_ns(1,2,nlat,ipair,mm)=zero
        uvm_ns(2,1,nlat,ipair,mm)=zero
        uvm_ns(2,2,nlat,ipair,mm)=zero
        uvm_ns(3,1,nlat,ipair,mm)=uvm_ns(3,1,nlat-1,ipair,mm)
        uvm_ns(3,2,nlat,ipair,mm)=zero
      else if(m.eq.1) then
        uvm_ns(1,1,1,ipair,mm)=uvm_ns(1,1,2,ipair,mm)
        uvm_ns(1,2,1,ipair,mm)=uvm_ns(1,2,2,ipair,mm)
        uvm_ns(2,1,1,ipair,mm)=uvm_ns(2,1,2,ipair,mm)
        uvm_ns(2,2,1,ipair,mm)=uvm_ns(2,2,2,ipair,mm)
        uvm_ns(3,1,1,ipair,mm)=zero
        uvm_ns(3,2,1,ipair,mm)=zero
        uvm_ns(1,1,nlat,ipair,mm)=uvm_ns(1,1,nlat-1,ipair,mm)
        uvm_ns(1,2,nlat,ipair,mm)=uvm_ns(1,2,nlat-1,ipair,mm)
        uvm_ns(2,1,nlat,ipair,mm)=uvm_ns(2,1,nlat-1,ipair,mm)
        uvm_ns(2,2,nlat,ipair,mm)=uvm_ns(2,2,nlat-1,ipair,mm)
        uvm_ns(3,1,nlat,ipair,mm)=zero
        uvm_ns(3,2,nlat,ipair,mm)=zero
      else
        uvm_ns(1,1,1,ipair,mm)=zero
        uvm_ns(1,2,1,ipair,mm)=zero
        uvm_ns(2,1,1,ipair,mm)=zero
        uvm_ns(2,2,1,ipair,mm)=zero
        uvm_ns(3,1,1,ipair,mm)=zero
        uvm_ns(3,2,1,ipair,mm)=zero
        uvm_ns(1,1,nlat,ipair,mm)=zero
        uvm_ns(1,2,nlat,ipair,mm)=zero
        uvm_ns(2,1,nlat,ipair,mm)=zero
        uvm_ns(2,2,nlat,ipair,mm)=zero
        uvm_ns(3,1,nlat,ipair,mm)=zero
        uvm_ns(3,2,nlat,ipair,mm)=zero
      end if

    end do

  end do

end subroutine inmi_nszdm2uvm

subroutine inmi_nspcm_hat2pcm(pcm_ns,pcm_hat)

!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    inmi_nspcm_hat2pcm
!   prgmmr:
!
! abstract:
!
! program history log:
!   2009-08-13  lueken - added subprogram doc block
!
!   input argument list:
!    pcm_hat
!
!   output argument list:
!    pcm_ns
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

  use kinds, only: r_kind
  use gridmod, only: nlat
  use specmod, only: jcap,jb,je,pln,plntop,clat
  use constants, only: zero
  implicit none

  real(r_kind),dimension(3,2,nlat,2,m_0:m_1),intent(out)::pcm_ns
  real(r_kind),dimension(3,2,nlat,2,m_0:m_1),intent(in)::pcm_hat

  integer(i_kind) i,ics,j,jnorth,jsouth,m,mm,n,ipair
  real(r_kind) spcp(2,0:jcap),spcc(2,0:jcap),spcm(2,0:jcap)
  real(r_kind) plnloc(0:jcap+1)
  real(r_kind) fp(2,2),fc(2,2),fm(2,2)

  do mm=m_0,m_1
    do ipair=1,2
      m=mmode_list(ipair,mm)
      ics=1+m*(2*jcap+3-m)/2

!           gather up spcp, spcc, spcm

      i=0
      do n=m,jcap
        i=i+1
        spcp(1,n)=pcm_hat(1,1,i,ipair,mm)
        spcp(2,n)=pcm_hat(1,2,i,ipair,mm)
        spcc(1,n)=pcm_hat(2,1,i,ipair,mm)
        spcc(2,n)=pcm_hat(2,2,i,ipair,mm)
        spcm(1,n)=pcm_hat(3,1,i,ipair,mm)
        spcm(2,n)=pcm_hat(3,2,i,ipair,mm)
      end do

      do j=jb,je
        jsouth=1+j
        jnorth=nlat-j

!           create plnloc

        do n=m,jcap
          plnloc(n)=pln(ics+n-m,j)
        end do
        plnloc(jcap+1)=plntop(m+1,j)

!          obtain f

        call spsynth_ns(0,jcap,m,clat(j),plnloc(m),0,spcp(1,m),fp)
        call spsynth_ns(0,jcap,m,clat(j),plnloc(m),0,spcc(1,m),fc)
        call spsynth_ns(0,jcap,m,clat(j),plnloc(m),0,spcm(1,m),fm)

!          scatter back to output pairs of lats

        pcm_ns(1,1,jnorth,ipair,mm)=fp(1,1)
        pcm_ns(1,2,jnorth,ipair,mm)=fp(2,1)
        pcm_ns(1,1,jsouth,ipair,mm)=fp(1,2)
        pcm_ns(1,2,jsouth,ipair,mm)=fp(2,2)
        pcm_ns(2,1,jnorth,ipair,mm)=fc(1,1)
        pcm_ns(2,2,jnorth,ipair,mm)=fc(2,1)
        pcm_ns(2,1,jsouth,ipair,mm)=fc(1,2)
        pcm_ns(2,2,jsouth,ipair,mm)=fc(2,2)
        pcm_ns(3,1,jnorth,ipair,mm)=fm(1,1)
        pcm_ns(3,2,jnorth,ipair,mm)=fm(2,1)
        pcm_ns(3,1,jsouth,ipair,mm)=fm(1,2)
        pcm_ns(3,2,jsouth,ipair,mm)=fm(2,2)

      end do

!  set pole values
      if(m.eq.0) then
        pcm_ns(1,1,1,ipair,mm)=pcm_ns(1,1,2,ipair,mm)
        pcm_ns(1,2,1,ipair,mm)=zero
        pcm_ns(1,1,nlat,ipair,mm)=pcm_ns(1,1,nlat-1,ipair,mm)
        pcm_ns(1,2,nlat,ipair,mm)=zero
        pcm_ns(2,1,1,ipair,mm)=pcm_ns(2,1,2,ipair,mm)
        pcm_ns(2,2,1,ipair,mm)=zero
        pcm_ns(2,1,nlat,ipair,mm)=pcm_ns(2,1,nlat-1,ipair,mm)
        pcm_ns(2,2,nlat,ipair,mm)=zero
        pcm_ns(3,1,1,ipair,mm)=pcm_ns(3,1,2,ipair,mm)
        pcm_ns(3,2,1,ipair,mm)=zero
        pcm_ns(3,1,nlat,ipair,mm)=pcm_ns(3,1,nlat-1,ipair,mm)
        pcm_ns(3,2,nlat,ipair,mm)=zero
      else
        pcm_ns(1,1,1,ipair,mm)=zero
        pcm_ns(1,2,1,ipair,mm)=zero
        pcm_ns(2,1,1,ipair,mm)=zero
        pcm_ns(2,2,1,ipair,mm)=zero
        pcm_ns(3,1,1,ipair,mm)=zero
        pcm_ns(3,2,1,ipair,mm)=zero
        pcm_ns(1,1,nlat,ipair,mm)=zero
        pcm_ns(1,2,nlat,ipair,mm)=zero
        pcm_ns(2,1,nlat,ipair,mm)=zero
        pcm_ns(2,2,nlat,ipair,mm)=zero
        pcm_ns(3,1,nlat,ipair,mm)=zero
        pcm_ns(3,2,nlat,ipair,mm)=zero
      end if

    end do

  end do

end subroutine inmi_nspcm_hat2pcm

subroutine inmi_nspcm_hat2pcm_ad(pcm_ns,pcm_hat)

!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    inmi_nspcm_hat2pcm_ad
!   prgmmr:
!
! abstract:
!
! program history log:
!   2009-08-13  lueken - added subprogram doc block
!
!   input argument list:
!    pcm_ns
!
!   output argument list:
!    pcm_hat
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block
  use kinds, only: r_kind
  use gridmod, only: nlat
  use specmod, only: jcap,jb,je,pln,plntop,wlat,clat
  use constants, only: zero
  implicit none

  real(r_kind),dimension(3,2,nlat,2,m_0:m_1),intent(in)::pcm_ns
  real(r_kind),dimension(3,2,nlat,2,m_0:m_1),intent(out)::pcm_hat

  real(r_kind),dimension(3,2,nlat,2,m_0:m_1)::pcm_ns_temp
  integer(i_kind) i,ics,j,jnorth,jsouth,m,mm,n,ipair
  real(r_kind) spcp(2,0:jcap),spcc(2,0:jcap),spcm(2,0:jcap)
  real(r_kind) plnloc(0:jcap+1)
  real(r_kind) fp(2,2),fc(2,2),fm(2,2)

  pcm_ns_temp=pcm_ns
  pcm_hat=zero
  do mm=m_0,m_1
    do ipair=1,2
      m=mmode_list(ipair,mm)
      ics=1+m*(2*jcap+3-m)/2

      do n=m,jcap
        spcp(1,n)=zero
        spcp(2,n)=zero
        spcc(1,n)=zero
        spcc(2,n)=zero
        spcm(1,n)=zero
        spcm(2,n)=zero
      end do

!  adjoint of set pole values

      if(m.eq.0) then
        pcm_ns_temp(1,1,     2,ipair,mm)=pcm_ns_temp(1,1,   1,ipair,mm)+pcm_ns_temp(1,1,     2,ipair,mm)
        pcm_ns_temp(1,1,nlat-1,ipair,mm)=pcm_ns_temp(1,1,nlat,ipair,mm)+pcm_ns_temp(1,1,nlat-1,ipair,mm)
        pcm_ns_temp(2,1,     2,ipair,mm)=pcm_ns_temp(2,1,   1,ipair,mm)+pcm_ns_temp(2,1,     2,ipair,mm)
        pcm_ns_temp(2,1,nlat-1,ipair,mm)=pcm_ns_temp(2,1,nlat,ipair,mm)+pcm_ns_temp(2,1,nlat-1,ipair,mm)
        pcm_ns_temp(3,1,     2,ipair,mm)=pcm_ns_temp(3,1,   1,ipair,mm)+pcm_ns_temp(3,1,     2,ipair,mm)
        pcm_ns_temp(3,1,nlat-1,ipair,mm)=pcm_ns_temp(3,1,nlat,ipair,mm)+pcm_ns_temp(3,1,nlat-1,ipair,mm)
      end if

      do j=jb,je
        jsouth=1+j
        jnorth=nlat-j

!       adjoint of scatter back to output pairs of lats

        fp(1,1)=pcm_ns_temp(1,1,jnorth,ipair,mm)/wlat(j)
        fp(2,1)=pcm_ns_temp(1,2,jnorth,ipair,mm)/wlat(j)
        fp(1,2)=pcm_ns_temp(1,1,jsouth,ipair,mm)/wlat(j)
        fp(2,2)=pcm_ns_temp(1,2,jsouth,ipair,mm)/wlat(j)
        fc(1,1)=pcm_ns_temp(2,1,jnorth,ipair,mm)/wlat(j)
        fc(2,1)=pcm_ns_temp(2,2,jnorth,ipair,mm)/wlat(j)
        fc(1,2)=pcm_ns_temp(2,1,jsouth,ipair,mm)/wlat(j)
        fc(2,2)=pcm_ns_temp(2,2,jsouth,ipair,mm)/wlat(j)
        fm(1,1)=pcm_ns_temp(3,1,jnorth,ipair,mm)/wlat(j)
        fm(2,1)=pcm_ns_temp(3,2,jnorth,ipair,mm)/wlat(j)
        fm(1,2)=pcm_ns_temp(3,1,jsouth,ipair,mm)/wlat(j)
        fm(2,2)=pcm_ns_temp(3,2,jsouth,ipair,mm)/wlat(j)

!           create plnloc

        do n=m,jcap
          plnloc(n)=pln(ics+n-m,j)
        end do
        plnloc(jcap+1)=plntop(m+1,j)

!          adjoint of obtain f

        call spanaly_ns(0,jcap,m,wlat(j),clat(j),plnloc(m),0,fp,spcp(1,m))
        call spanaly_ns(0,jcap,m,wlat(j),clat(j),plnloc(m),0,fc,spcc(1,m))
        call spanaly_ns(0,jcap,m,wlat(j),clat(j),plnloc(m),0,fm,spcm(1,m))

      end do

!     adjoint of gather up spcp, spcc, spcm

      i=0
      do n=m,jcap
        i=i+1
        pcm_hat(1,1,i,ipair,mm)=spcp(1,n)
        pcm_hat(1,2,i,ipair,mm)=spcp(2,n)
        pcm_hat(2,1,i,ipair,mm)=spcc(1,n)
        pcm_hat(2,2,i,ipair,mm)=spcc(2,n)
        pcm_hat(3,1,i,ipair,mm)=spcm(1,n)
        pcm_hat(3,2,i,ipair,mm)=spcm(2,n)
      end do

    end do

  end do

end subroutine inmi_nspcm_hat2pcm_ad

subroutine inmi_nsuvm2zdm_ad(uvm_ns,zdm_hat)

!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    inmi_nsuvm2zdm_ad
!   prgmmr:
!
! abstract:
!
! program history log:
!   2009-08-13  lueken - added subprogram doc block
!
!   input argument list:
!    uvm_ns
!    zdm_hat
!
!   output argument list:
!    uvm_ns
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

  use kinds, only: r_kind
  use gridmod, only: nlat
  use specmod, only: jcap,jb,je,pln,plntop,enn1,elonn1,eon,eontop,clat,wlat
  implicit none

  real(r_kind),dimension(3,2,nlat,2,m_0:m_1),intent(inout)::uvm_ns
  real(r_kind),dimension(3,2,nlat,2,m_0:m_1),intent(in)::zdm_hat

  integer(i_kind) i,ics,j,jnorth,jsouth,m,mm,n,ipair
  real(r_kind) spcz(2,0:jcap),spcd(2,0:jcap),spcp(2,0:jcap),spcu(2,0:jcap+1),spcv(2,0:jcap+1)
  real(r_kind) plnloc(0:jcap+1)
  real(r_kind) fu(2,2),fv(2,2),fp(2,2)


  do mm=m_0,m_1
    do ipair=1,2
      m=mmode_list(ipair,mm)
      ics=1+m*(2*jcap+3-m)/2

!           gather up spcz, spcd, spcp

      i=0
      do n=m,jcap
        i=i+1
        spcz(1,n)=zdm_hat(1,1,i,ipair,mm)*enn1(ics+n-m)
        spcz(2,n)=zdm_hat(1,2,i,ipair,mm)*enn1(ics+n-m)
        spcd(1,n)=zdm_hat(2,1,i,ipair,mm)*enn1(ics+n-m)
        spcd(2,n)=zdm_hat(2,2,i,ipair,mm)*enn1(ics+n-m)
        spcp(1,n)=zdm_hat(3,1,i,ipair,mm)
        spcp(2,n)=zdm_hat(3,2,i,ipair,mm)
      end do

!           convert to spcu, spcv

      call spdz2uv_ns(0,jcap,m,elonn1(ics),eon(ics),eontop(m+1), &
              spcd(1,m),spcz(1,m),spcu(1,m),spcv(1,m),spcu(1,jcap+1),spcv(1,jcap+1))

      do j=jb,je
        jsouth=1+j
        jnorth=nlat-j

!           create plnloc

        do n=m,jcap
          plnloc(n)=pln(ics+n-m,j)
        end do
        plnloc(jcap+1)=plntop(m+1,j)

!          obtain f

        call spsynth_ns(0,jcap,m,clat(j),plnloc(m),1,spcu(1,m),fu)
        call spsynth_ns(0,jcap,m,clat(j),plnloc(m),1,spcv(1,m),fv)
        call spsynth_ns(0,jcap,m,clat(j),plnloc(m),0,spcp(1,m),fp)

!          scatter back to output pairs of lats

        uvm_ns(1,1,jnorth,ipair,mm)=fu(1,1)*wlat(j)
        uvm_ns(1,2,jnorth,ipair,mm)=fu(2,1)*wlat(j)
        uvm_ns(1,1,jsouth,ipair,mm)=fu(1,2)*wlat(j)
        uvm_ns(1,2,jsouth,ipair,mm)=fu(2,2)*wlat(j)
        uvm_ns(2,1,jnorth,ipair,mm)=fv(1,1)*wlat(j)
        uvm_ns(2,2,jnorth,ipair,mm)=fv(2,1)*wlat(j)
        uvm_ns(2,1,jsouth,ipair,mm)=fv(1,2)*wlat(j)
        uvm_ns(2,2,jsouth,ipair,mm)=fv(2,2)*wlat(j)
        uvm_ns(3,1,jnorth,ipair,mm)=fp(1,1)*wlat(j)
        uvm_ns(3,2,jnorth,ipair,mm)=fp(2,1)*wlat(j)
        uvm_ns(3,1,jsouth,ipair,mm)=fp(1,2)*wlat(j)
        uvm_ns(3,2,jsouth,ipair,mm)=fp(2,2)*wlat(j)

      end do

    end do

  end do

end subroutine inmi_nsuvm2zdm_ad
      subroutine spdz2uv_ns(I,M,L,ELONN1,EON,EONTOP,D,Z,U,V,UTOP,VTOP)
!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!
! SUBPROGRAM:    SPDZ2UV_ns  COMPUTE WINDS FROM div and vort for one zonal wave number
!   PRGMMR: IREDELL          ORG: W/NMC23     DATE: 92-10-31
!
! ABSTRACT: COMPUTES THE WIND COMPONENTS FROM DIVERGENCE AND VORTICITY
!           IN SPECTRAL SPACE.
!           SUBPROGRAM SPEPS SHOULD BE CALLED ALREADY.
!           IF L IS THE ZONAL WAVENUMBER, N IS THE TOTAL WAVENUMBER,
!           EPS(L,N)=SQRT((N**2-L**2)/(4*N**2-1)) AND A IS EARTH RADIUS,
!           THEN THE ZONAL WIND COMPONENT U IS COMPUTED AS
!             U(L,N)=-I*L/(N*(N+1))*A*D(L,N)
!                    +EPS(L,N+1)/(N+1)*A*Z(L,N+1)-EPS(L,N)/N*A*Z(L,N-1)
!           AND THE MERIDIONAL WIND COMPONENT V IS COMPUTED AS
!             V(L,N)=-I*L/(N*(N+1))*A*Z(L,N)
!                    -EPS(L,N+1)/(N+1)*A*D(L,N+1)+EPS(L,N)/N*A*D(L,N-1)
!           WHERE D IS DIVERGENCE AND Z IS VORTICITY.
!           U AND V ARE WEIGHTED BY THE COSINE OF LATITUDE.
!           EXTRA TERMS ARE COMPUTED OVER TOP OF THE SPECTRAL DOMAIN.
!           ADVANTAGE IS TAKEN OF THE FACT THAT EPS(L,L)=0
!           IN ORDER TO VECTORIZE OVER THE ENTIRE SPECTRAL DOMAIN.
!
! PROGRAM HISTORY LOG:
!   91-10-31  MARK IREDELL
!   2006-09-05 parrish -- modify to do one zonal wave number only for parallel
!                         computation across processors by zonal wave number.
!
! USAGE:    CALL SPDZ2UV_ns(I,M,L,ELONN1,EON,EONTOP,D,Z,U,V,UTOP,VTOP)
!
!   INPUT ARGUMENT LIST:
!     I        - INTEGER SPECTRAL DOMAIN SHAPE
!                (0 FOR TRIANGULAR, 1 FOR RHOMBOIDAL)
!     M        - INTEGER SPECTRAL TRUNCATION
!     L        - zonal wave number
!     ELONN1   - REAL (L:M+I*L) L/(N*(N+1))*A
!     EON      - REAL (L:M+I*L) EPSILON/N*A
!     EONTOP   - REAL       EPSILON/N*A OVER TOP
!     D        - REAL (2,L:M+I*L) DIVERGENCE for zonal wave number L
!     Z        - REAL (2,L:M+I*L) VORTICITY for zonal wave number L
!
!   OUTPUT ARGUMENT LIST:
!     U        - REAL (2,L:M+I*L) ZONAL WIND (TIMES COSLAT) for zonal wave number L
!     V        - REAL (2,L:M+I*L) MERID WIND (TIMES COSLAT) for zonal wave number L
!     UTOP     - REAL (2) ZONAL WIND (TIMES COSLAT) OVER TOP for zonal wave number L
!     VTOP     - REAL (2) MERID WIND (TIMES COSLAT) OVER TOP for zonal wave number L
!
! ATTRIBUTES:
!   LANGUAGE: CRAY FORTRAN
!
!$$$
      use kinds, only: r_kind
      implicit none

      integer(i_kind),intent(in):: i,m,l
      REAL(r_kind),intent(in):: ELONN1(L:M+I*L)
      REAL(r_kind),intent(in):: EON(L:M+I*L),EONTOP
      REAL(r_kind),intent(in):: D(2,L:M+I*L),Z(2,L:M+I*L)
      REAL(r_kind),intent(out):: U(2,L:M+I*L),V(2,L:M+I*L)
      REAL(r_kind),intent(out):: UTOP(2),VTOP(2)

      integer(4) n
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  COMPUTE WINDS IN THE SPECTRAL DOMAIN

      do n=l,m+i*l
        U(1,n)= elonn1(n)*d(2,n)
        U(2,n)=-elonn1(n)*d(1,n)
        V(1,n)= elonn1(n)*z(2,n)
        V(2,n)=-elonn1(n)*z(1,n)
      end do
      do n=l,m+i*l-1
        U(1,n)=u(1,n)+EON(n+1)*Z(1,n+1)
        U(2,n)=u(2,n)+EON(n+1)*Z(2,n+1)
        V(1,n)=v(1,n)-EON(n+1)*D(1,n+1)
        V(2,n)=v(2,n)-EON(n+1)*D(2,n+1)
      end do
      do n=l+1,m+i*l
        U(1,n)=u(1,n)-EON(n)*Z(1,n-1)
        U(2,n)=u(2,n)-EON(n)*Z(2,n-1)
        V(1,n)=v(1,n)+EON(n)*D(1,n-1)
        V(2,n)=v(2,n)+EON(n)*D(2,n-1)
      end do
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  COMPUTE WINDS OVER TOP OF THE SPECTRAL DOMAIN
      UTOP(1)=-EONTOP*Z(1,m+i*l)
      UTOP(2)=-EONTOP*Z(2,m+i*l)
      VTOP(1)= EONTOP*D(1,m+i*l)
      VTOP(2)= EONTOP*D(2,m+i*l)
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      RETURN
      END subroutine spdz2uv_ns
!-----------------------------------------------------------------------
      SUBROUTINE SPUV2DZ_ns(I,M,L,ENN1,ELONN1,EON,EONTOP,U,V,UTOP,VTOP,D,Z)
!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!
! SUBPROGRAM:    SPUV2DZ_ns  COMPUTE DIV,VORT FROM WINDS for one zonal wave number
!   PRGMMR: IREDELL          ORG: W/NMC23     DATE: 92-10-31
!
! ABSTRACT: COMPUTES THE DIVERGENCE AND VORTICITY FROM WIND COMPONENTS
!           IN SPECTRAL SPACE.
!           SUBPROGRAM SPEPS SHOULD BE CALLED ALREADY.
!           IF L IS THE ZONAL WAVENUMBER, N IS THE TOTAL WAVENUMBER,
!           EPS(L,N)=SQRT((N**2-L**2)/(4*N**2-1)) AND A IS EARTH RADIUS,
!           THEN THE DIVERGENCE D IS COMPUTED AS
!             D(L,N)=I*L*A*U(L,N)
!                    +EPS(L,N+1)*N*A*V(L,N+1)-EPS(L,N)*(N+1)*A*V(L,N-1)
!           AND THE VORTICITY Z IS COMPUTED AS
!             Z(L,N)=I*L*A*V(L,N)
!                    -EPS(L,N+1)*N*A*U(L,N+1)+EPS(L,N)*(N+1)*A*U(L,N-1)
!           WHERE U IS THE ZONAL WIND AND V IS THE MERIDIONAL WIND.
!           U AND V ARE WEIGHTED BY THE SECANT OF LATITUDE.
!           EXTRA TERMS ARE USED OVER TOP OF THE SPECTRAL DOMAIN.
!           ADVANTAGE IS TAKEN OF THE FACT THAT EPS(L,L)=0
!           IN ORDER TO VECTORIZE OVER THE ENTIRE SPECTRAL DOMAIN.
!
! PROGRAM HISTORY LOG:
!   91-10-31  MARK IREDELL
!   2006-09-05 parrish -- modify to do one zonal wave number only for parallel
!                         computation across processors by zonal wave number.
!
! USAGE:    CALL SPUV2DZ_ns(I,M,L,ENN1,ELONN1,EON,EONTOP,U,V,UTOP,VTOP,D,Z)
!
!   INPUT ARGUMENT LIST:
!     I        - INTEGER SPECTRAL DOMAIN SHAPE
!                (0 FOR TRIANGULAR, 1 FOR RHOMBOIDAL)
!     L        - zonal wave number
!     M        - INTEGER SPECTRAL TRUNCATION
!     ENN1     - REAL (L:M+I*L) N*(N+1)/A**2
!     ELONN1   - REAL (L:M+I*L) L/(N*(N+1))*A
!     EON      - REAL (L:M+I*L) EPSILON/N*A
!     EONTOP   - REAL    EPSILON/N*A OVER TOP
!     U        - REAL (2,L:M+I*L) ZONAL WIND (OVER COSLAT)
!     V        - REAL (2,L:M+I*L) MERID WIND (OVER COSLAT)
!     UTOP     - REAL (2) ZONAL WIND (OVER COSLAT) OVER TOP
!     VTOP     - REAL (2) MERID WIND (OVER COSLAT) OVER TOP
!
!   OUTPUT ARGUMENT LIST:
!     D        - REAL (2,L:M+I*L) DIVERGENCE
!     Z        - REAL (2,L:M+I*L) VORTICITY
!
! ATTRIBUTES:
!   LANGUAGE: CRAY FORTRAN
!
!$$$
      use kinds, only: r_kind
      implicit none

      integer(i_kind),intent(in):: i,m,l
      REAL(r_kind),intent(in):: ENN1(L:M+I*L),ELONN1(L:M+I*L)
      REAL(r_kind),intent(in):: EON(L:M+I*L),EONTOP
      REAL(r_kind),intent(in):: U(2,L:M+I*L),V(2,L:M+I*L)
      REAL(r_kind),intent(in):: UTOP(2),VTOP(2)
      REAL(r_kind),intent(out):: D(2,L:M+I*L),Z(2,L:M+I*L)

      integer(i_kind) n
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  COMPUTE TERMS FROM THE SPECTRAL DOMAIN

      do n=l,m+i*l
        D(1,n)=-ELONN1(n)*U(2,n)
        D(2,n)= ELONN1(n)*U(1,n)
        Z(1,n)=-ELONN1(n)*V(2,n)
        Z(2,n)= ELONN1(n)*V(1,n)
      end do
      do n=l,m+i*l-1
        D(1,n)=d(1,n)+EON(n+1)*V(1,n+1)
        D(2,n)=d(2,n)+EON(n+1)*V(2,n+1)
        Z(1,n)=z(1,n)-EON(n+1)*U(1,n+1)
        Z(2,n)=z(2,n)-EON(n+1)*U(2,n+1)
      end do
      do n=l+1,m+i*l
        D(1,n)=d(1,n)-EON(n)*V(1,n-1)
        D(2,n)=d(2,n)-EON(n)*V(2,n-1)
        Z(1,n)=z(1,n)+EON(n)*U(1,n-1)
        Z(2,n)=z(2,n)+EON(n)*U(2,n-1)
      end do

!  COMPUTE TERMS FROM OVER TOP OF THE SPECTRAL DOMAIN
      n=m+i*l
      d(1,n)=d(1,n)+eontop*vtop(1)
      d(2,n)=d(2,n)+eontop*vtop(2)
      z(1,n)=z(1,n)-eontop*utop(1)
      z(2,n)=z(2,n)-eontop*utop(2)

!  MULTIPLY BY LAPLACIAN TERM
      do n=l,m+i*l
        d(1,n)=d(1,n)*enn1(n)
        d(2,n)=d(2,n)*enn1(n)
        z(1,n)=z(1,n)*enn1(n)
        z(2,n)=z(2,n)*enn1(n)
      end do

      RETURN
      end SUBROUTINE SPUV2DZ_ns

!-----------------------------------------------------------------------
      SUBROUTINE SPANALY_ns(I,M,L,WGT,CLAT,PLN,MP,F,SPC)
!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!
! SUBPROGRAM:    SPANALY     ANALYZE SPECTRAL FROM FOURIER
!   PRGMMR: IREDELL          ORG: W/NMC23     DATE: 92-10-31
!
! ABSTRACT: ANALYZES SPECTRAL COEFFICIENTS FROM FOURIER COEFFICIENTS
!           FOR A LATITUDE PAIR (NORTHERN AND SOUTHERN HEMISPHERES).
!           VECTOR COMPONENTS ARE MULTIPLIED BY COSINE OF LATITUDE.
!
! PROGRAM HISTORY LOG:
!   91-10-31  MARK IREDELL
!   94-08-01  MARK IREDELL   MOVED ZONAL WAVENUMBER LOOP INSIDE
! 1998-12-15  IREDELL  OPENMP DIRECTIVES INSERTED
! 2006-09-10  parrish -- modify to do one zonal wave number only for parallel
!                        computation across processors by zonal wave number
!
! USAGE:    CALL SPANALY_ns(I,M,L,WGT,CLAT,PLN,MP,F,SPC)
!
!   INPUT ARGUMENT LIST:
!     I        - INTEGER SPECTRAL DOMAIN SHAPE
!                (0 FOR TRIANGULAR, 1 FOR RHOMBOIDAL)
!     M        - INTEGER SPECTRAL TRUNCATION
!     L        - zonal wave number to process for this call
!     WGT      - REAL GAUSSIAN WEIGHT
!     CLAT     - REAL COSINE OF LATITUDE
!     PLN      - REAL (L:M+I*L+MP) LEGENDRE POLYNOMIALS
!     MP       - INTEGER  IDENTIFIERS (0 FOR SCALAR, 1 FOR VECTOR)
!     F        - REAL (2,2) input zonal wave number coefficients for this lat pair
!
!   OUTPUT ARGUMENT LIST:
!     SPC      - REAL (2,L:M+I*L+MP) SPECTRAL COEFFICIENTS
!
! ATTRIBUTES:
!   LANGUAGE: CRAY FORTRAN
!
!$$$
      use kinds, only: r_kind
      implicit none

      integer(i_kind),intent(in):: i,m,l
      INTEGER(i_kind),intent(in):: MP
      real(r_kind),intent(in):: wgt,clat
      REAL(r_kind),intent(in):: PLN(L:M+I*L+MP)
      REAL(r_kind),intent(in):: F(2,2)
      REAL(r_kind),intent(inout):: SPC(2,L:M+I*L+MP)

      REAL(r_kind) FW(2,2)
      real(r_kind) wgtloc
      integer(i_kind) n
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      if(mp.eq.0) then
        wgtloc=wgt
      else
        wgtloc=wgt*clat
      end if
      FW(1,1)=wgtloc*(F(1,1)+F(1,2))
      FW(2,1)=wgtloc*(F(2,1)+F(2,2))
      FW(1,2)=wgtloc*(F(1,1)-F(1,2))
      FW(2,2)=wgtloc*(F(2,1)-F(2,2))
      do N=L,I*L+M+MP,2
        SPC(1,N)=SPC(1,N)+PLN(N)*FW(1,1)
        SPC(2,N)=SPC(2,N)+PLN(N)*FW(2,1)
      end do
      do N=L+1,I*L+M+MP,2
        SPC(1,N)=SPC(1,N)+PLN(N)*FW(1,2)
        SPC(2,N)=SPC(2,N)+PLN(N)*FW(2,2)
      end do
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      RETURN
      end SUBROUTINE SPANALY_ns
!-----------------------------------------------------------------------
      SUBROUTINE SPSYNTH_ns(I,M,L,CLAT,PLN,MP,SPC,F)
!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!
! SUBPROGRAM:    SPSYNTH_ns  SPSYNTH modified for one zonal wave number
!   PRGMMR: IREDELL          ORG: W/NMC23     DATE: 92-10-31
!
! ABSTRACT: SYNTHESIZES FOURIER COEFFICIENTS FROM SPECTRAL COEFFICIENTS
!           FOR A LATITUDE PAIR (NORTHERN AND SOUTHERN HEMISPHERES).
!           VECTOR COMPONENTS ARE DIVIDED BY COSINE OF LATITUDE.
!
! PROGRAM HISTORY LOG:
!   91-10-31  MARK IREDELL
! 1998-12-18  MARK IREDELL  INCLUDE SCALAR AND GRADIENT OPTION
! 2006-09-06  parrish -- modify to do one zonal wave number only for parallel
!                        computation across processors by zonal wave number
!
! USAGE:    CALL SPSYNTH_ns(I,M,L,CLAT,PLN,MP,SPC,F)
!
!   INPUT ARGUMENT LIST:
!     I        - INTEGER SPECTRAL DOMAIN SHAPE
!                (0 FOR TRIANGULAR, 1 FOR RHOMBOIDAL)
!     M        - INTEGER SPECTRAL TRUNCATION
!     L        - zonal wave number to process for this call
!     CLAT     - REAL COSINE OF LATITUDE
!     PLN      - REAL (L:M+I*L+MP) LEGENDRE POLYNOMIAL
!     SPC      - REAL (2,L:M+I*L+MP) SPECTRAL COEFFICIENTS
!     MP       - INTEGER  IDENTIFIERS (0 FOR SCALAR, 1 FOR VECTOR)
!
!   OUTPUT ARGUMENT LIST:
!     F        - REAL (2,2) zonal wave number for this LATITUDE PAIR
!
! ATTRIBUTES:
!   LANGUAGE: CRAY FORTRAN
!
!$$$
      use kinds, only: r_kind
      use constants, only: zero
      implicit none

      integer(i_kind),intent(in):: i,m,l
      INTEGER(i_kind),intent(in):: MP
      real(r_kind),intent(in):: clat
      REAL(r_kind),intent(in):: PLN(L:M+I*L+MP)
      REAL(r_kind),intent(in):: SPC(2,L:M+I*L+MP)
      REAL(r_kind),intent(out):: F(2,2)

      integer(i_kind) n
      real(r_kind) f1r,f1i
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  ZERO OUT FOURIER COEFFICIENTS.
      F(1,1)=zero
      F(2,1)=zero
      F(1,2)=zero
      F(2,2)=zero
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  SYNTHESIS OVER FINITE LATITUDE.
!  FOR EACH ZONAL WAVENUMBER, SYNTHESIZE TERMS OVER TOTAL WAVENUMBER.
!  SYNTHESIZE EVEN AND ODD POLYNOMIALS SEPARATELY.
            DO N=L,I*L+M+MP,2
              F(1,1)=F(1,1)+PLN(N)*SPC(1,n)
              F(2,1)=F(2,1)+PLN(N)*SPC(2,n)
            ENDDO
            DO N=L+1,I*L+M+MP,2
              F(1,2)=F(1,2)+PLN(N)*SPC(1,n)
              F(2,2)=F(2,2)+PLN(N)*SPC(2,n)
            ENDDO
!  SEPARATE FOURIER COEFFICIENTS FROM EACH HEMISPHERE.
!  ODD POLYNOMIALS CONTRIBUTE NEGATIVELY TO THE SOUTHERN HEMISPHERE.
!  DIVIDE VECTOR COMPONENTS BY COSINE LATITUDE.
            F1R=F(1,1)
            F1I=F(2,1)
            F(1,1)=F1R+F(1,2)
            F(2,1)=F1I+F(2,2)
            F(1,2)=F1R-F(1,2)
            F(2,2)=F1I-F(2,2)
          IF(MP.EQ.1) THEN
            F(1,1)=F(1,1)/CLAT
            F(2,1)=F(2,1)/CLAT
            F(1,2)=F(1,2)/CLAT
            F(2,2)=F(2,2)/CLAT
          ENDIF
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      end SUBROUTINE SPSYNTH_ns

end module strong_fast_global_mod
