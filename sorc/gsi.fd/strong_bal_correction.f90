subroutine strong_bal_correction(u_t,v_t,t_t,ps_t,mype,u,v,t,ps,u_t_g,v_t_g,t_t_g,ps_t_g, &
                 bal_diagnostic,update)

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
!
!   input argument list:
!     u_t      - input perturbation u tendency on gaussian grid (subdomains)
!     v_t      - input perturbation v tendency on gaussian grid (subdomains)
!     t_t      - input perturbation T tendency on gaussian grid (subdomains)
!     ps_t     - input perturbation surface pressure tendency on gaussian grid (subdomains)
!     mype     - current processor
!     u        - input perturbation u on gaussian grid (subdomains)
!     v        - input perturbation v on gaussian grid (subdomains)
!     t        - input perturbation T on gaussian grid (subdomains)
!     ps       - input perturbation surface pressure on gaussian grid (subdomains)
!     bal_diagnostic - if true, then compute BAL diagnostic, a measure of amplitude
!                      of balanced gravity mode tendencies
!     update   - if false, then do not update u,v,t,ps with balance increment
!
!   output argument list:
!     u        - output balanced perturbation u on gaussian grid (subdomains)
!     v        - output balanced perturbation v on gaussian grid (subdomains)
!     t        - output balanced perturbation T on gaussian grid (subdomains)
!     ps       - output balanced perturbation surface pressure on gaussian grid (subdomains)
!     u_t_g    - output perturbation u tendency projected on gravity modes (subdomains)
!     v_t_g    - output perturbation v tendency projected on gravity modes (subdomains)
!     t_t_g    - output perturbation T tendency projected on gravity modes (subdomains)
!     ps_t_g   - output perturbation ps tendency projected on gravity modes (subdomains)
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$

  use kinds, only: r_kind,i_kind
  use mod_vtrans, only: depths,nvmodes_keep,vtrans,vtrans_inv
  use mod_inmi, only: m,gspeed,mmax,dinmi,gproj
  use gridmod, only: nlat,nlon,lat2,lon2,nsig
  use specmod, only: jcap,nc
  use mpimod, only: nuvlevs
  use constants, only: zero
  implicit none

  integer(i_kind),intent(in)::mype
  logical,intent(in)::bal_diagnostic,update
  real(r_kind),dimension(lat2,lon2,nsig),intent(in)::u_t,v_t,t_t
  real(r_kind),dimension(lat2,lon2),intent(in)::ps_t
  real(r_kind),dimension(lat2,lon2,nsig),intent(inout)::u,v,t
  real(r_kind),dimension(lat2,lon2),intent(inout)::ps
  real(r_kind),dimension(lat2,lon2,nsig),intent(out)::u_t_g,v_t_g,t_t_g
  real(r_kind),dimension(lat2,lon2),intent(out)::ps_t_g

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
  real(r_kind),dimension(nc)::divhat_g,vorthat_g,mhat_g
  real(r_kind) rmstend_all_uf,rmstend_all_g_uf,rmstend_all_f,rmstend_all_g_f

  integer(i_kind) i,j,k,kk,iad,mode
  integer(i_kind) mode_number(nuvlevs),mode_number0(nsig)
  logical filtered

  filtered=.true.

  mmax=jcap

!   1.  u,v,t,ps   -->    utilde,vtilde,mtilde  (vertical mode transform)
!       (subdomains)         (subdomains)

  call vtrans(u_t,v_t,t_t,ps_t,utilde_t,vtilde_t,mtilde_t)

                      
  call get_mode_number(mode_number,mode_number0,mype)
!---------mode_number > 0 for 1st copy of u,v,m, reserved for correction delu,delv,delm
!---------mode_number < 0 for 2nd copy of u,v,m, reserved for grav part of tends, u_t_g,v_t_g,m_t_g
  call inmi_sub2grid(utilde_t,utilde_t,uwork,mype,mode_number0)
  call inmi_sub2grid(vtilde_t,vtilde_t,vwork,mype,mode_number0)
  call inmi_sub2grid(mtilde_t,mtilde_t,mwork,mype,mode_number0)

  rmstend_loc_uf=zero
  rmstend_g_loc_uf=zero
  rmstend_loc_f=zero
  rmstend_g_loc_f=zero
  do kk=1,nuvlevs
    mode=mode_number(kk)
    if(mode.eq.0) then
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
        if(mode.gt.0) then
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
      call zds2uvg(delvorthat,deldivhat,uwork(1,1,kk),vwork(1,1,kk))
      call s2g0(delmhat,mwork(1,1,kk))

  end do
  if(bal_diagnostic) then
    call gather_rmstends(rmstend_loc_uf,  rmstend_uf,  mode_number0,mype)
    call gather_rmstends(rmstend_g_loc_uf,rmstend_g_uf,mode_number0,mype)
    call gather_rmstends(rmstend_loc_f,  rmstend_f,  mode_number0,mype)
    call gather_rmstends(rmstend_g_loc_f,rmstend_g_f,mode_number0,mype)
    if(mype.eq.0) then
           rmstend_all_uf=zero
           rmstend_all_g_uf=zero
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
  call inmi_grid2sub(delutilde,utilde_t_g,uwork,mype,mode_number0)
  call inmi_grid2sub(delvtilde,vtilde_t_g,vwork,mype,mode_number0)
  call inmi_grid2sub(delmtilde,mtilde_t_g,mwork,mype,mode_number0)
 
!   7.  delutilde,delvtilde,delmtilde  -->  delu,delv,delt,delps   (vertical mode inverse transform)
!       (subdomains)                      (subdomains)

  call vtrans_inv(delutilde,delvtilde,delmtilde,delu,delv,delt,delps)
!????????????????????????????????in here, can insert diagnostic based on utilde_t_g and utilde_t,etc.
  call vtrans_inv(utilde_t_g,vtilde_t_g,mtilde_t_g,u_t_g,v_t_g,t_t_g,ps_t_g)


!  update u,v,t,ps


  if(update) then
    do k=1,nsig
      do j=1,lon2
        do i=1,lat2
          u(i,j,k)=u(i,j,k)+delu(i,j,k)
          v(i,j,k)=v(i,j,k)+delv(i,j,k)
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

end subroutine strong_bal_correction

subroutine strong_bal_correction_ad(u_t,v_t,t_t,ps_t,mype,u,v,t,ps,u_t_g,v_t_g,t_t_g,ps_t_g)

!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    strong_bal_correction_ad adjoint of strong_bal_correction
!   prgmmr: parrish          org: np23                date: 2006-07-15
!
! abstract: adjoint of strong_bal_correction
!
! program history log:
!   2006-07-15  parrish
!
!   input argument list:
!     u_t      - input perturbation u tendency on gaussian grid (subdomains)
!     v_t      - input perturbation v tendency on gaussian grid (subdomains)
!     t_t      - input perturbation T tendency on gaussian grid (subdomains)
!     ps_t     - input perturbation surface pressure tendency on gaussian grid (subdomains)
!     mype     - current processor
!     u        - input perturbation u on gaussian grid (subdomains)
!     v        - input perturbation v on gaussian grid (subdomains)
!     t        - input perturbation T on gaussian grid (subdomains)
!     ps       - input perturbation surface pressure on gaussian grid (subdomains)
!     u_t_g    - input perturbation u tendency projected on gravity modes (subdomains)
!     v_t_g    - input perturbation v tendency projected on gravity modes (subdomains)
!     t_t_g    - input perturbation T tendency projected on gravity modes (subdomains)
!     ps_t_g   - input perturbation ps tendency projected on gravity modes (subdomains)
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

  use kinds, only: r_kind,i_kind
  use mod_vtrans, only: depths,nvmodes_keep,vtrans_ad,vtrans_inv_ad
  use mod_inmi, only: m,gspeed,mmax,dinmi_ad
  use gridmod, only: nlat,nlon,lat2,lon2,nsig
  use specmod, only: jcap,nc
  use mpimod, only: nuvlevs
  use constants, only: zero
  implicit none

  integer(i_kind),intent(in)::mype
  real(r_kind),dimension(lat2,lon2,nsig),intent(inout)::u_t,v_t,t_t
  real(r_kind),dimension(lat2,lon2),intent(inout)::ps_t
  real(r_kind),dimension(lat2,lon2,nsig),intent(in)::u,v,t
  real(r_kind),dimension(lat2,lon2),intent(in)::ps
  real(r_kind),dimension(lat2,lon2,nsig),intent(in)::u_t_g,v_t_g,t_t_g
  real(r_kind),dimension(lat2,lon2),intent(in)::ps_t_g

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
  integer(i_kind) mode_number(nuvlevs),mode_number0(nsig)

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
        delu(i,j,k)=u(i,j,k)
        delv(i,j,k)=v(i,j,k)
        delt(i,j,k)=t(i,j,k)
      end do
    end do
  end do

!   7.  adjoint of delutilde,delvtilde,delmtilde  -->  delu,delv,delt,delps  (vert mode inverse transform)
!       (subdomains)                      (subdomains)

  delutilde=zero ; delvtilde=zero ; delmtilde=zero
  utilde_t_g=zero ; vtilde_t_g=zero ; mtilde_t_g=zero
  call vtrans_inv_ad(delutilde,delvtilde,delmtilde,delu,delv,delt,delps)
  call vtrans_inv_ad(utilde_t_g,vtilde_t_g,mtilde_t_g,u_t_g,v_t_g,t_t_g,ps_t_g)

  call get_mode_number(mode_number,mode_number0,mype)
  call inmi_sub2grid(delutilde,utilde_t_g,uwork,mype,mode_number0)
  call inmi_sub2grid(delvtilde,vtilde_t_g,vwork,mype,mode_number0)
  call inmi_sub2grid(delmtilde,mtilde_t_g,mwork,mype,mode_number0)

  do kk=1,nuvlevs
    mode=mode_number(kk)
    if(mode.eq.0) then
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
      call zds2uvg_ad(delvorthat,deldivhat,uwork(1,1,kk),vwork(1,1,kk))

!   4.  divhat,vorthat,mhat --> deldivhat,delvorthat,delmhat   (inmi correction)
!          (slabs)                        (slabs)

      gspeed=sqrt(depths(abs(mode)))
      iad=1
      vorthat=zero ; divhat=zero ; mhat=zero
      do m=0,jcap
        if(mode.gt.0) then
          call dinmi_ad(vorthat(iad),divhat(iad),mhat(iad),&
                      delvorthat(iad)   ,   deldivhat(iad),   delmhat(iad))
        else
    !     call gproj_ad(vorthat(iad),divhat(iad),mhat(iad),delvorthat(iad),deldivhat(iad),delmhat(iad))
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
  call inmi_grid2sub(utilde_t,utilde_t2,uwork,mype,mode_number0)
  call inmi_grid2sub(vtilde_t,vtilde_t2,vwork,mype,mode_number0)
  call inmi_grid2sub(mtilde_t,mtilde_t2,mwork,mype,mode_number0)
  utilde_t=utilde_t+utilde_t2
  vtilde_t=vtilde_t+vtilde_t2
  mtilde_t=mtilde_t+mtilde_t2
!
!!   1.  adjoint of u,v,t,ps   -->    utilde,vtilde,mtilde  (vertical mode transform)
!!                     (subdomains)         (subdomains)

  call vtrans_ad(u_t,v_t,t_t,ps_t,utilde_t,vtilde_t,mtilde_t)

end subroutine strong_bal_correction_ad

subroutine inmi_sub2grid(utilde,utilde2,uwork,mype,mode_number0)

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
!
!   input argument list:
!     utilde   - vertical tranformed variable on subdomains
!     utilde2  - vertical tranformed variable on subdomains
!     mype     - current processor number
!     mode_number0 - defines how input vertical modes are interleaved
!                    in output horizontal arrays
!
!   output argument list:
!     uwork    - output fields in horizontal slab mode
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$

  use kinds, only: r_kind,i_kind
  use constants, only: zero
  use gridmod, only: regional,lat2,nsig,iglobal,lon1,itotsub,lon2,lat1,ltosi,ltosj,nlon,nlat
  use mpimod, only: iscuv_s,ierror,mpi_comm_world,irduv_s,ircuv_s,&
       isduv_s,isduv_g,iscuv_g,nuvlevs,irduv_g,ircuv_g,mpi_rtype,&
       strip,reorder,reorder2
  use mod_vtrans, only: nvmodes_keep
  implicit none

! Declare passed variables
  integer(i_kind) mype,mode_number0(nsig)
  real(r_kind),dimension(lat2,lon2,nvmodes_keep),intent(in):: utilde,utilde2
  real(r_kind),dimension(nlat,nlon,nuvlevs),intent(out)::uwork

  real(r_kind),dimension(lat2,lon2,nsig):: u

! Declare local variables
  integer(i_kind) i,j,k,isize,mode
            integer(i_kind) ii

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
    if(mode.eq.0) cycle
    if(mode.gt.0) then
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
    call reorder(work3,nuvlevs)
    do k=1,nuvlevs
      do i=1,iglobal
        uwork(ltosi(i),ltosj(i),k)=work3(i,k)
      end do
    end do

end subroutine inmi_sub2grid

subroutine inmi_grid2sub(utilde,utilde2,uwork,mype,mode_number0)

!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    inmi_grid2sub  reverse of inmi_sub2grid
!   prgmmr: parrish          org: np23                date: 2006-08-03
!
! abstract: reverse of inmi_sub2grid
!
! program history log:
!   2006-08-03  parrish
!
!   input argument list:
!     uwork    - input fields in horizontal slab mode
!     mype     - current processor number
!     mode_number0 - defines how vertical modes are interleaved across processors
!                    in horizontal slab arrays
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

  use kinds, only: r_kind,i_kind
  use constants, only: zero
  use gridmod, only: regional,lat2,nsig,iglobal,lon1,itotsub,lon2,lat1,nlat,nlon,ltosi_s,ltosj_s
  use mpimod, only: iscuv_s,ierror,mpi_comm_world,irduv_s,ircuv_s,&
       isduv_s,isduv_g,iscuv_g,nuvlevs,irduv_g,ircuv_g,mpi_rtype,&
       strip,reorder,reorder2
  use mod_vtrans, only: nvmodes_keep
  implicit none

! Declare passed variables
  integer(i_kind) mype,mode_number0(nsig)
  real(r_kind),dimension(lat2,lon2,nvmodes_keep),intent(out):: utilde,utilde2
  real(r_kind),dimension(nlat,nlon,nuvlevs),intent(in)::uwork

  real(r_kind),dimension(lat2,lon2,nsig):: u

! Declare local variables
  integer(i_kind) i,j,k,isize,mode

  real(r_kind),dimension(lat1,lon1,nsig):: usm
  real(r_kind),dimension(itotsub,nuvlevs):: work3

! Initialize variables
  isize=max(iglobal,itotsub)

    do k=1,nuvlevs
      do i=1,itotsub
        work3(i,k)=uwork(ltosi_s(i),ltosj_s(i),k)
      end do
    end do
!   reorder the work array for the mpi communication
    call reorder2(work3,nuvlevs)

!   get u back on subdomains
    call mpi_alltoallv(work3(1,1),iscuv_s,isduv_s,&
         mpi_rtype,u(1,1,1),ircuv_s,irduv_s,mpi_rtype,&
         mpi_comm_world,ierror)
  do k=1,nsig
    mode=mode_number0(k)
    if(mode.eq.0) cycle
    if(mode.gt.0) then
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

subroutine get_mode_number(mode_number,mode_number0,mype)

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
!   output argument list:
!     mode_number  - defines vertical mode layout for current processor
!     mode_number0 - defines how vertical modes are interleaved across processors
!                    in horizontal slab arrays
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$

  use kinds, only: r_kind,i_kind
  use mpimod, only: ierror,mpi_comm_world,nuvlevs,mpi_integer,npe
  use mod_vtrans, only: nvmodes_keep
  use gridmod, only: nsig
  use constants, only: izero
  implicit none

  integer(i_kind),intent(in):: mype
  integer(i_kind),intent(out):: mode_number(nuvlevs),mode_number0(nsig)

  integer(i_kind) i,ii
  integer(i_kind) nuvlevs0(npe),ndisp(npe+1)
  integer(i_kind) nuvlev_use,kchk

      if(nvmodes_keep*2.gt.nsig) then
           write(6,*)' error in get_mode_number, currently necessary for nvmodes_keep*2 <= nsig '
           call stop2(89)
      end if
  if (mod(nsig,npe)==izero) then
    kchk=npe
  else
    kchk=mod(nsig,npe)
  end if
  if(mype+1.le.kchk) then
    nuvlev_use=nuvlevs
  else
    nuvlev_use=nuvlevs-1
  end if
  ii=0
  mode_number=0
  do i=1,2*nvmodes_keep
    if(mod(i-1,npe).eq.mype.and.ii.lt.nuvlev_use) then
      ii=ii+1
      if(i.le.nvmodes_keep) then
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

subroutine gather_rmstends(rmstend_loc,rmstend,mode_number0,mype)

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
!     mode_number0 - defines how vertical modes are interleaved across processors
!                    in horizontal slab arrays
!
!   output argument list:
!     rmstend  -  all vertical modes of rmstend assembled across all processors
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$

  use kinds, only: r_kind,i_kind
  use mpimod, only: ierror,mpi_comm_world,nuvlevs,mpi_integer,mpi_rtype,npe
  use mod_vtrans, only: nvmodes_keep
  use gridmod, only: nsig
  use constants, only: izero
  implicit none

  real(r_kind),intent(in):: rmstend_loc(nuvlevs)
  real(r_kind),intent(out):: rmstend(nvmodes_keep)
  integer(i_kind),intent(in):: mype
  integer(i_kind),intent(in):: mode_number0(nsig)

  integer(i_kind) i
  integer(i_kind) nuvlevs0(npe),ndisp(npe+1)
  integer(i_kind) nuvlev_use,kchk
  real(r_kind) work(nsig)

  if (mod(nsig,npe)==izero) then
    kchk=npe
  else
    kchk=mod(nsig,npe)
  end if
  if(mype+1.le.kchk) then
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
    if(mode_number0(i).lt.0) rmstend(-mode_number0(i))=work(i)
  end do

end subroutine gather_rmstends
