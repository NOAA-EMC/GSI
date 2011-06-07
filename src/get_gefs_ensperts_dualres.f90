subroutine get_gefs_ensperts_dualres
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    get_gefs_ensperts_dualres copy of get_gefs_ensperts for dual resolution
!   prgmmr: kleist           org: np22                date: 2010-01-05
!
! abstract: read ensemble members, and construct ensemble perturbations, for use
!             with hybrid ensemble option.  ensemble spread is also written out as
!             a byproduct for diagnostic purposes.
!
!
! program history log:
!   2010-01-05  kleist, initial documentation
!   2010-02-17  parrish - make changes to allow dual resolution capability
!   2010-03-24  derber - use generalized genqsat rather than specialized for this resolution
!   2010-03-29  kleist  - make changes to allow for st/vp perturbations
!   2010-04-14  kleist  - add ensemble mean ps array for use with vertical localizaion (lnp)
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

  use gridmod, only: idsl5
  use hybrid_ensemble_parameters, only: n_ens,write_ens_sprd,oz_univ_static
  use hybrid_ensemble_isotropic, only: st_en,vp_en,t_en,rh_en,oz_en,cw_en,p_en,sst_en,ps_bar
  use constants,only: zero,half,fv,rd_over_cp,one
  use mpimod, only: mpi_comm_world,ierror,mype,npe
  use kinds, only: r_kind,i_kind
  use hybrid_ensemble_parameters, only: grd_ens,nlat_ens,nlon_ens,sp_ens,uv_hyb_ens
  implicit none

  real(r_kind),dimension(grd_ens%lat2,grd_ens%lon2,grd_ens%nsig):: vor,div,u,v,tv,q,cwmr,oz,qs
  real(r_kind),dimension(grd_ens%lat2,grd_ens%lon2):: z,ps,sst2
  real(r_kind),dimension(grd_ens%nlat,grd_ens%nlon):: sst_full,dum
  real(r_kind),dimension(grd_ens%latlon1n):: stbar,vpbar,tbar,rhbar,ozbar,cwbar
  real(r_kind),dimension(grd_ens%latlon11):: pbar,sstbar
  real(r_kind) bar_norm,sig_norm,kapr,kap1,rh
  real(r_kind),allocatable,dimension(:,:,:) :: tsen,prsl,pri

  integer(i_kind),dimension(grd_ens%nlat,grd_ens%nlon):: idum
  integer(i_kind) iret,i,j,k,m,n,il,jl,mm1,iderivative,im,jm,km
  character(24) filename
  logical ice

  stbar=zero ; vpbar=zero ; tbar=zero ; rhbar=zero ; ozbar=zero ; cwbar=zero 
  pbar=zero ; sstbar =zero

  mm1=mype+1
  kap1=rd_over_cp+one
  kapr=one/rd_over_cp

  im=grd_ens%lat2
  jm=grd_ens%lon2
  km=grd_ens%nsig
  do n=1,n_ens
    write(filename,100) n
100        format('sigf06_ens_mem',i3.3)

! Use read_gfs routine to get ensemble members
! *** NOTE ***
! For now, everything is assumed to be at the same resolution
    if (mype==0)write(6,*) 'CALL READ_GFSATM FOR ENS FILE : ',filename
    call general_read_gfsatm(grd_ens,sp_ens,filename,mype,uv_hyb_ens,z,ps,vor,div,u,v,tv,q,cwmr,oz,iret)

! Temporarily, try to use the u,v option (i.e. uv_hyb_ens)...eventually two
! options exist:
!   1. Create modified version of read_gfsatm that only passes back out the things
!      we need, i.e. SF, VP, TV, RH, OZ, CW, and PS
!   2. Create subroutine to go from gridded values of vor/div on subdomain to get
!      SF/VP on subdomain (I can't believe this doesn't exist yet!)
  
! Compute RH
! Get 3d pressure field now on interfaces
    allocate(pri(im,jm,km+1))
    call general_getprs_glb(ps,tv,pri)
    allocate(prsl(im,jm,km),tsen(im,jm,km))
! Get sensible temperature and 3d layer pressure
    if (idsl5 /= 2) then
!$omp parallel do schedule(dynamic,1) private(k,j,i)
      do k=1,km
        do j=1,jm
          do i=1,im
            prsl(i,j,k)=((pri(i,j,k)**kap1-pri(i,j,k+1)**kap1)/&
                           (kap1*(pri(i,j,k)-pri(i,j,k+1))))**kapr
            tsen(i,j,k)= tv(i,j,k)/(one+fv*max(zero,q(i,j,k)))
          end do
        end do
      end do
    else 
!$omp parallel do schedule(dynamic,1) private(k,j,i)
      do k=1,km
        do j=1,jm
          do i=1,im
            prsl(i,j,k)=(pri(i,j,k)+pri(i,j,k+1))*half
            tsen(i,j,k)= tv(i,j,k)/(one+fv*max(zero,q(i,j,k)))
          end do
        end do
      end do
    end if
    deallocate(pri)

    ice=.true.
    iderivative=0
    call genqsat(qs,tsen,prsl,grd_ens%lat2,grd_ens%lon2,grd_ens%nsig,ice,iderivative)
    deallocate(tsen,prsl)

!$omp parallel do schedule(dynamic,1) private(m,i,j,k,rh)
    do k=1,km
      do j=1,jm
        do i=1,im
          m=(k-1)*im*jm+(j-1)*im+i
          rh=q(i,j,k)/qs(i,j,k)
          st_en(m,n) = u(i,j,k)
          vp_en(m,n) = v(i,j,k)
          t_en(m,n)  = tv(i,j,k)
          rh_en(m,n) = rh
          oz_en(m,n) = oz(i,j,k)
          cw_en(m,n) = cwmr(i,j,k)

          stbar(m)=stbar(m)+u(i,j,k)  
          vpbar(m)=vpbar(m)+v(i,j,k)  
           tbar(m)= tbar(m)+tv(i,j,k)
          rhbar(m)=rhbar(m)+rh
          ozbar(m)=ozbar(m)+oz(i,j,k) 
          cwbar(m)=cwbar(m)+cwmr(i,j,k)
        end do
      end do
      if(k == 1)then
        do j=1,jm
          do i=1,im
            m=(j-1)*im+i
            p_en(m,n) = ps(i,j)
            pbar(m)=pbar(m)+ ps(i,j)
          end do
        end do
      end if
    end do
  end do ! end do over ensemble

! Convert to mean
  bar_norm = one/float(n_ens)
!$omp parallel do schedule(dynamic,1) private(i)
  do i=1,grd_ens%latlon1n
    stbar(i)=stbar(i)*bar_norm
    vpbar(i)=vpbar(i)*bar_norm
    tbar(i) = tbar(i)*bar_norm
    rhbar(i)=rhbar(i)*bar_norm
    ozbar(i)=ozbar(i)*bar_norm
    cwbar(i)=cwbar(i)*bar_norm
  end do
! Copy pbar to module array.  ps_bar may be needed for vertical localization
! in terms of scale heights/normalized p/p 
  do i=1,grd_ens%latlon11
    pbar(i)=pbar(i)*bar_norm
    ps_bar(i)=pbar(i)
  end do

! Before converting to perturbations, get ensemble spread
  if (write_ens_sprd) call ens_spread_dualres(stbar,vpbar,tbar,rhbar,ozbar,cwbar,pbar,mype)

! Convert ensemble members to perturbations
  sig_norm=sqrt(one/max(one,n_ens-one))

!$omp parallel do schedule(dynamic,1) private(n,i)
  do n=1,n_ens
    do i=1,grd_ens%latlon1n
      st_en(i,n)=(st_en(i,n)-stbar(i))*sig_norm
      vp_en(i,n)=(vp_en(i,n)-vpbar(i))*sig_norm
      t_en(i,n) =( t_en(i,n)- tbar(i))*sig_norm
      rh_en(i,n)=(rh_en(i,n)-rhbar(i))*sig_norm
      cw_en(i,n)=(cw_en(i,n)-cwbar(i))*sig_norm
    end do

! If request, zero out ozone perturbations for hybrid
    if (.not. oz_univ_static) then
       do i=1,grd_ens%latlon1n
          oz_en(i,n)=(oz_en(i,n)-ozbar(i))*sig_norm  
       end do
    else
       do i=1,grd_ens%latlon1n
          oz_en(i,n)=zero
       end do
    end if

    do i=1,grd_ens%latlon11
      p_en(i,n)=(p_en(i,n)- pbar(i))*sig_norm
! Ignore sst perturbations in hybrid
      sst_en(i,n)=zero
    end do
  end do

!  since initial version is ignoring sst perturbations, skip following code for now.  revisit
!   later--creating general_read_gfssfc, analogous to general_read_gfsatm above.
!! GET SST PERTURBATIONS HERE
!  do n=1,n_ens
!    write(filename,105) n
!105        format('sfcf06_ens_mem',i3.3)
!
!! This will get full 2d nlat x nlon sst field
!    if(mype==0)write(6,*) 'CALL READ_GFSSFC FOR ENS FILE : ',filename
!    call read_gfssfc(filename,mype,&
!         dum,sst_full,dum, &
!         dum,dum,dum,dum,dum,idum,dum,dum)
!
!    call mpi_barrier(mpi_comm_world,ierror)
!
!! mpi barrier here?
!    do j=1,jm
!      jl=j+grd_ens%jstart(mm1)-2
!      jl=min0(max0(1,jl),grd_ens%nlon)
!      do i=1,im
!        il=i+grd_ens%istart(mm1)-2
!        il=min0(max0(1,il),grd_ens%nlat)
!        sst2(i,j)=sst_full(il,jl)
!      end do
!    end do
!  
!    m=0
!    do j=1,jm
!      do i=im
!        m=m+1
!        sst_en(m,n) = sst2(i,j)
!        sstbar(m)=sstbar(m)+ sst2(i,j)
!      end do
!    end do
!  end do ! end do over ensemble
!
!  do n=1,n_ens
!    do i=1,grd_ens%latlon11
!      sst_en(i,n)=(sst_en(i,n)- sstbar(i)*bar_norm)
!    end do
!  end do


  return
end subroutine get_gefs_ensperts_dualres

subroutine ens_spread_dualres(stbar,vpbar,tbar,rhbar,ozbar,cwbar,pbar,mype)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    ens_spread_dualres  output ensemble spread for diagnostics
!   prgmmr: kleist           org: np22                date: 2010-01-05
!
! abstract: compute ensemble spread on ensemble grid, interpolate to analysis grid
!             and write out for diagnostic purposes.
!
!
! program history log:
!   2010-01-05  kleist, initial documentation
!   2010-02-28  parrish - make changes to allow dual resolution capability
!
!   input argument list:
!     stbar  - ensemble mean stream function (or u wind component)
!     vpbar  - ensemble mean stream function (or v wind component)
!      tbar  - ensemble mean Tv
!     rhbar  - ensemble mean rh
!     ozbar  - ensemble mean ozone
!     cwbar  - ensemble mean cloud water
!      pbar  - ensemble mean surface pressure
!      mype  - current processor number
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$ end documentation block
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

  integer(i_kind) i,ii,n
  integer(i_kind) ist,ivp,it,irh,ioz,icw,ip
  logical regional
  integer(i_kind) num_fields,inner_vars
  logical,allocatable::vector(:)

  sp_norm=(one/float(n_ens))

  sube=zero
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
     regional=.false.
     inner_vars=1
     num_fields=6*grd_ens%nsig+1
     allocate(vector(num_fields))
     vector=.false.
     vector(1:2*grd_ens%nsig)=uv_hyb_ens   !  assume here that 1st two 3d variables are either u,v or psi,chi
     call general_sub2grid_create_info(se,inner_vars,grd_ens%nlat,grd_ens%nlon,grd_ens%nsig,num_fields, &
                                       regional,vector)
     call general_sub2grid_create_info(sa,inner_vars,grd_anl%nlat,grd_anl%nlon,grd_anl%nsig,num_fields, &
                                       regional,vector)
     deallocate(vector)
     call general_sube2suba_r_double(se,sa,p_e2a,sube,suba,regional)
  end if

  ist=1
  ivp=grd_anl%latlon1n+ist
  it =grd_anl%latlon1n+ivp
  irh=grd_anl%latlon1n+it
  ioz=grd_anl%latlon1n+irh
  icw=grd_anl%latlon1n+ioz
  ip =grd_anl%latlon1n+icw
  call write_spread_dualres(suba(ist),suba(ivp),suba(it),suba(irh),suba(ioz),suba(icw),suba(ip),mype)

  return
end subroutine ens_spread_dualres


subroutine write_spread_dualres(a,b,c,d,e,f,g2in,mype)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    write_spread_dualres   write ensemble spread for diagnostics
!   prgmmr: kleist           org: np22                date: 2010-01-05
!
! abstract: write ensemble spread (previously interpolated to analysis grid)
!             for diagnostic purposes.
!
!
! program history log:
!   2010-01-05  kleist, initial documentation
!   2010-02-28  parrish - make changes to allow dual resolution capability
!
!   input argument list:
!     a    -  spread variable 1
!     b    -  spread variable 2
!     c    -  spread variable 3
!     d    -  spread variable 4
!     e    -  spread variable 5
!     f    -  spread variable 6
!     g    -  spread variable 7
!     mype -  current processor number
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$ end documentation block
  use kinds, only: r_kind,i_kind,r_single
  use hybrid_ensemble_parameters, only: grd_anl
  use constants, only: zero
  implicit none

  integer(i_kind),intent(in):: mype
  character(255):: grdfile

  real(r_kind),dimension(grd_anl%lat2,grd_anl%lon2,grd_anl%nsig),intent(in):: a,b,c,d,e,f
  real(r_kind),dimension(grd_anl%lat2,grd_anl%lon2),intent(in):: g2in
  real(r_kind),dimension(grd_anl%lat2,grd_anl%lon2,grd_anl%nsig,6):: g3in

  real(r_kind),dimension(grd_anl%nlat,grd_anl%nlon,grd_anl%nsig):: work8_3d
  real(r_kind),dimension(grd_anl%nlat,grd_anl%nlon):: work8_2d

  real(r_single),dimension(grd_anl%nlon,grd_anl%nlat,grd_anl%nsig):: work4_3d
  real(r_single),dimension(grd_anl%nlon,grd_anl%nlat):: work4_2d

  integer(i_kind) ncfggg,iret,i,j,k,n,mem2d,mem3d,num3d

! Initial memory used by 2d and 3d grids
  mem2d = 4*grd_anl%nlat*grd_anl%nlon
  mem3d = 4*grd_anl%nlat*grd_anl%nlon*grd_anl%nsig
  num3d=6

! transfer 2d arrays to generic work aray
  do k=1,grd_anl%nsig
    do j=1,grd_anl%lon2
       do i=1,grd_anl%lat2
         g3in(i,j,k,1)=a(i,j,k)
         g3in(i,j,k,2)=b(i,j,k)
         g3in(i,j,k,3)=c(i,j,k)
         g3in(i,j,k,4)=d(i,j,k)
         g3in(i,j,k,5)=e(i,j,k)
         g3in(i,j,k,6)=f(i,j,k)
       end do
     end do
  end do

  if (mype==0) then
    grdfile='ens_spread.grd'
    ncfggg=len_trim(grdfile)
    call baopenwt(22,grdfile(1:ncfggg),iret)
    write(6,*)'WRITE_SPREAD_DUALRES:  open 22 to ',trim(grdfile),' with iret=',iret
  endif

! Process 3d arrays
  do n=1,num3d
    work8_3d=zero
    do k=1,grd_anl%nsig
      call gather_stuff2(g3in(1,1,k,n),work8_3d(1,1,k),mype,0)
    end do
    if (mype==0) then
      do k=1,grd_anl%nsig
        do j=1,grd_anl%nlon
          do i=1,grd_anl%nlat
            work4_3d(j,i,k) =work8_3d(i,j,k)
          end do
        end do
      end do
      call wryte(22,mem3d,work4_3d)
      write(6,*)'WRITE_SPREAD_DUALRES FOR VARIABLE NUM ',n
    endif
  end do

! Process 2d array
  work8_2d=zero
  call gather_stuff2(g2in,work8_2d,mype,0)
  if (mype==0) then
     do j=1,grd_anl%nlon
        do i=1,grd_anl%nlat
           work4_2d(j,i)=work8_2d(i,j)
        end do
     end do
     call wryte(22,mem2d,work4_2d)
     write(6,*)'WRITE_SPREAD_DUALRES FOR 2D FIELD '
  endif


! Close byte-addressable binary file for grads
  if (mype==0) then
     call baclose(22,iret)
     write(6,*)'WRITE_SPREAD_DUALRES:  close 22 with iret=',iret
  end if


  return
end subroutine write_spread_dualres

subroutine general_getprs_glb(ps,tv,prs)
! subprogram:    getprs       get 3d pressure or 3d pressure deriv
!   prgmmr: kleist           org: np20                date: 2005-09-29
!
! abstract: calculate 3d pressure and its horizontal derivatives
!
! program history log:
!   2005-09-29  kleist
!   2006-04-12  treadon - replace sigi with bk5
!   2006-07-31  kleist  - analysis variable change from ln(ps) to ps
!   2007-05-08  kleist  - add generalized vert coord and derivative call
!   2007-07-26  cucurull- compute 3d pressure and derivatives in different subroutines
!                       - remove gues_tv from argument list; clean up code
!   2008-06-04  safford - rm unused uses
!   2008-09-05  lueken  - merged ed's changes into q1fy09 code
!   2010-02-23  parrish - copy getprs and generalize for dual resolution.
!
!   input argument list:
!     ps       - surface pressure
!
!   output argument list:
!     prs        - 3d pressure
!
! attributes:
!   language:  f90
!   machine:   ibm/RS6000 SP
!
!$$$ end documentation block

  use kinds,only: r_kind,i_kind
  use constants,only: zero,half,one_tenth,rd_over_cp,one
  use gridmod,only: nsig,lat2,lon2,ak5,bk5,ck5,tref5,idvc5
  use gridmod,only: wrf_nmm_regional,nems_nmmb_regional,eta1_ll,eta2_ll,pdtop_ll,pt_ll,&
       regional,wrf_mass_regional,twodvar_regional
  use hybrid_ensemble_parameters, only: grd_ens
! use guess_grids, only: ges_tv,ntguessig
                                                                   use mpimod, only: mype
  implicit none

! Declare passed variables
  real(r_kind),dimension(grd_ens%lat2,grd_ens%lon2)       ,intent(in   ) :: ps
  real(r_kind),dimension(grd_ens%lat2,grd_ens%lon2,nsig)  ,intent(in   ) :: tv
  real(r_kind),dimension(grd_ens%lat2,grd_ens%lon2,nsig+1),intent(  out) :: prs

! Declare local variables
  real(r_kind) kapr,trk
  integer(i_kind) i,j,k,k2    ! ,it

! Declare local parameter
  real(r_kind),parameter:: ten = 10.0_r_kind

                                     
  kapr=one/rd_over_cp

  if (regional) then
     if(wrf_nmm_regional.or.nems_nmmb_regional) then
        do k=1,nsig+1
           do j=1,grd_ens%lon2
              do i=1,grd_ens%lat2
                 prs(i,j,k)=one_tenth* &
                      (eta1_ll(k)*pdtop_ll + &
                      eta2_ll(k)*(ten*ps(i,j)-pdtop_ll-pt_ll) + &
                      pt_ll)
              end do
           end do
        end do
     elseif (wrf_mass_regional .or. twodvar_regional) then
        do k=1,nsig+1
           do j=1,grd_ens%lon2
              do i=1,grd_ens%lat2
                 prs(i,j,k)=one_tenth*(eta1_ll(k)*(ten*ps(i,j)-pt_ll) + pt_ll)
              end do
           end do
        end do
     endif
  else
     k=1
     k2=nsig+1
     do j=1,grd_ens%lon2
        do i=1,grd_ens%lat2
           prs(i,j,k)=ps(i,j)
           prs(i,j,k2)=zero
        end do
     end do
     if (idvc5 /= 3) then
!$omp parallel do schedule(dynamic,1) private(k,j,i)
        do k=2,nsig
           do j=1,grd_ens%lon2
              do i=1,grd_ens%lat2
                 prs(i,j,k)=ak5(k)+bk5(k)*ps(i,j)
              end do
           end do
        end do
     else
!$omp parallel do schedule(dynamic,1) private(k,j,i,trk)
        do k=2,nsig
           do j=1,grd_ens%lon2
              do i=1,grd_ens%lat2
                 trk=(half*(tv(i,j,k-1)+tv(i,j,k))/tref5(k))**kapr
                 prs(i,j,k)=ak5(k)+(bk5(k)*ps(i,j))+(ck5(k)*trk)
              end do
           end do
        end do
     end if
  end if

  return
end subroutine general_getprs_glb
