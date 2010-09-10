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
!   2010-04-06  parrish - comment out call to ensemble_spread_dualres for now--there is a bug
!                           and daryl has a newer version which is faster and uses less memory.
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
  use hybrid_ensemble_parameters, only: n_ens,grd_ens,nlat_ens,nlon_ens,sp_ens
  use hybrid_ensemble_isotropic, only: st_en,vp_en,t_en,rh_en,oz_en,cw_en,p_en,sst_en,ps_bar
  use constants,only: zero,ione,izero,half,fv,rd_over_cp,one
  use mpimod, only: mpi_comm_world,ierror,mype
  use kinds, only: r_kind,i_kind
  implicit none

  real(r_kind),dimension(grd_ens%lat2,grd_ens%lon2,grd_ens%nsig+1) :: pri
  real(r_kind),dimension(grd_ens%lat2,grd_ens%lon2,grd_ens%nsig):: vor,div,u,v,tv,q,cwmr,oz,qs,rh,tsen,prsl
  real(r_kind),dimension(grd_ens%lat2,grd_ens%lon2):: z,ps,sst2
  real(r_kind),dimension(grd_ens%nlat,grd_ens%nlon):: sst_full,dum
  real(r_kind),dimension(grd_ens%latlon1n):: stbar,vpbar,tbar,rhbar,ozbar,cwbar
  real(r_kind),dimension(grd_ens%latlon11):: pbar,sstbar
  real(r_kind) bar_norm,sig_norm,kapr,kap1

  integer(i_kind),dimension(grd_ens%nlat,grd_ens%nlon):: idum
  integer(i_kind) iret,i,j,k,m,n,il,jl,mm1,iderivative
  character(24) filename
  logical ice

  stbar=zero ; vpbar=zero ; tbar=zero ; rhbar=zero ; ozbar=zero ; cwbar=zero 
  pbar=zero ; sstbar =zero

  mm1=mype+ione
  kap1=rd_over_cp+one
  kapr=one/rd_over_cp

  do n=1,n_ens
    write(filename,100) n
100        format('sigf06_ens_mem',i3.3)

! Use read_gfs routine to get ensemble members
! *** NOTE ***
! For now, everything is assumed to be at the same resolution
    if (mype==0)write(6,*) 'CALL READ_GFSATM FOR ENS FILE : ',filename
    call general_read_gfsatm(grd_ens,sp_ens,filename,mype,z,ps,vor,div,u,v,tv,q,cwmr,oz,iret)

! Temporarily, try to use the u,v option (i.e. uv_hyb_ens)...eventually two
! options exist:
!   1. Create modified version of read_gfsatm that only passes back out the things
!      we need, i.e. SF, VP, TV, RH, OZ, CW, and PS
!   2. Create subroutine to go from gridded values of vor/div on subdomain to get
!      SF/VP on subdomain (I can't believe this doesn't exist yet!)
  
! Compute RH
! First step is go get sensible temperature and 3d pressure
    do k=1,grd_ens%nsig
      do j=1,grd_ens%lon2
        do i=1,grd_ens%lat2
          tsen(i,j,k)= tv(i,j,k)/(one+fv*max(zero,q(i,j,k)))
        end do
      end do
    end do
! Get 3d pressure field now on interfaces
    call general_getprs_glb(ps,tv,pri)
    if (idsl5/=2_i_kind) then
      do j=1,grd_ens%lon2
        do i=1,grd_ens%lat2
          do k=1,grd_ens%nsig
            prsl(i,j,k)=((pri(i,j,k)**kap1-pri(i,j,k+ione)**kap1)/&
                           (kap1*(pri(i,j,k)-pri(i,j,k+ione))))**kapr
          end do
        end do
      end do
    else 
      do j=1,grd_ens%lon2
        do i=1,grd_ens%lat2
          do k=1,grd_ens%nsig
            prsl(i,j,k)=(pri(i,j,k)+pri(i,j,k+ione))*half
          end do
        end do
      end do
    end if

    ice=.true.
    iderivative=0
    call genqsat(qs,tsen,prsl,grd_ens%lat2,grd_ens%lon2,grd_ens%nsig,ice,iderivative)

    do k=1,grd_ens%nsig
      do j=1,grd_ens%lon2
        do i=1,grd_ens%lat2
          rh(i,j,k) = q(i,j,k)/qs(i,j,k)
        end do
      end do
    end do

    m=izero
    do k=1,grd_ens%nsig
      do j=1,grd_ens%lon2
        do i=1,grd_ens%lat2
          m=m+ione
          st_en(m,n) = u(i,j,k)
          vp_en(m,n) = v(i,j,k)
          t_en(m,n)  = tv(i,j,k)
          rh_en(m,n) = rh(i,j,k)
          oz_en(m,n) = oz(i,j,k)
          cw_en(m,n) = cwmr(i,j,k)

          stbar(m)=stbar(m)+u(i,j,k)
          vpbar(m)=vpbar(m)+v(i,j,k)
           tbar(m)= tbar(m)+ tv(i,j,k)
          rhbar(m)=rhbar(m)+rh(i,j,k)
          ozbar(m)=ozbar(m)+oz(i,j,k)
          cwbar(m)=cwbar(m)+cwmr(i,j,k)
        end do
      end do
    end do
    m=izero
    do j=1,grd_ens%lon2
      do i=1,grd_ens%lat2
        m=m+ione
        p_en(m,n) = ps(i,j)
        pbar(m)=pbar(m)+ ps(i,j)
      end do
    end do
  end do ! end do over ensemble

! Convert to mean
  bar_norm = one/float(n_ens)
  do i=1,grd_ens%latlon1n
    stbar(i)=stbar(i)*bar_norm
    vpbar(i)=vpbar(i)*bar_norm
    tbar(i) = tbar(i)*bar_norm
    rhbar(i)=rhbar(i)*bar_norm
    ozbar(i)=ozbar(i)*bar_norm
    cwbar(i)=cwbar(i)*bar_norm
  end do
  do i=1,grd_ens%latlon11
    pbar(i)=pbar(i)*bar_norm
  end do

! Copy pbar to module array.  ps_bar may be needed for vertical localization
! in terms of scale heights/normalized p/p
  ps_bar(:)=pbar(:)

  call mpi_barrier(mpi_comm_world,ierror)
! Before converting to perturbations, get ensemble spread
! call ens_spread_dualres(stbar,vpbar,tbar,rhbar,ozbar,cwbar,pbar,mype)
! call mpi_barrier(mpi_comm_world,ierror)

! Convert ensemble members to perturbations
   sig_norm=sqrt(one/max(one,n_ens-one))

  do n=1,n_ens
    do i=1,grd_ens%latlon1n
      st_en(i,n)=(st_en(i,n)-stbar(i))*sig_norm
      vp_en(i,n)=(vp_en(i,n)-vpbar(i))*sig_norm
      t_en(i,n) =( t_en(i,n)- tbar(i))*sig_norm
      rh_en(i,n)=(rh_en(i,n)-rhbar(i))*sig_norm
      oz_en(i,n)=(oz_en(i,n)-ozbar(i))*sig_norm
      cw_en(i,n)=(cw_en(i,n)-cwbar(i))*sig_norm
    end do
    do i=1,grd_ens%latlon11
      p_en(i,n)=(p_en(i,n)- pbar(i))*sig_norm
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
!    do j=1,grd_ens%lon2
!      jl=j+grd_ens%jstart(mm1)-2
!      jl=min0(max0(1,jl),grd_ens%nlon)
!      do i=1,grd_ens%lat2
!        il=i+grd_ens%istart(mm1)-2
!        il=min0(max0(1,il),grd_ens%nlat)
!        sst2(i,j)=sst_full(il,jl)
!      end do
!    end do
!  
!    m=izero
!    do j=1,grd_ens%lon2
!      do i=1,grd_ens%lat2
!        m=m+ione
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

! dtk: temporarily ignore sst perturbations in hybrid
  do n=1,n_ens
    do i=1,grd_ens%latlon11
      sst_en(i,n)=zero
    end do
  end do


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


subroutine write_spread_dualres(a,b,c,d,e,f,g,mype)
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
  implicit none

  integer(i_kind),intent(in):: mype
  character(255):: grdfile

  real(r_kind),dimension(grd_anl%lat2,grd_anl%lon2,grd_anl%nsig),intent(in):: a,b,c,d,e,f
  real(r_kind),dimension(grd_anl%lat2,grd_anl%lon2),intent(in):: g

  real(r_kind),dimension(grd_anl%nlat,grd_anl%nlon,grd_anl%nsig):: ag,bg,cg,dg,eg,fg
  real(r_kind),dimension(grd_anl%nlat,grd_anl%nlon):: gg

  real(r_single),dimension(grd_anl%nlon,grd_anl%nlat,grd_anl%nsig):: a4,b4,c4,d4,e4,f4
  real(r_single),dimension(grd_anl%nlon,grd_anl%nlat):: g4

  integer(i_kind) ncfggg,iret,i,j,k

! gather stuff to processor 0
  do k=1,grd_anl%nsig
    call gather_stuff2(a(1,1,k),ag(1,1,k),mype,0)
    call gather_stuff2(b(1,1,k),bg(1,1,k),mype,0)
    call gather_stuff2(c(1,1,k),cg(1,1,k),mype,0)
    call gather_stuff2(d(1,1,k),dg(1,1,k),mype,0)
    call gather_stuff2(e(1,1,k),eg(1,1,k),mype,0)
    call gather_stuff2(f(1,1,k),fg(1,1,k),mype,0)
  end do
  call gather_stuff2(g,gg,mype,0)

  if (mype==0) then
    write(6,*) 'WRITE OUT NEW VARIANCES'
! load single precision arrays
    do k=1,grd_anl%nsig
      do j=1,grd_anl%nlon
        do i=1,grd_anl%nlat
          a4(j,i,k)=ag(i,j,k)
          b4(j,i,k)=bg(i,j,k)
          c4(j,i,k)=cg(i,j,k)
          d4(j,i,k)=dg(i,j,k)
          e4(j,i,k)=eg(i,j,k)
          f4(j,i,k)=fg(i,j,k)
        end do
      end do
    end do
    do j=1,grd_anl%nlon
      do i=1,grd_anl%nlat
        g4(j,i)=gg(i,j)
      end do
    end do

! Create byte-addressable binary file for grads
    grdfile='ens_spread.grd'
    ncfggg=len_trim(grdfile)
    call baopenwt(22,grdfile(1:ncfggg),iret)
    call wryte(22,4*grd_anl%nlat*grd_anl%nlon*grd_anl%nsig,a4)
    call wryte(22,4*grd_anl%nlat*grd_anl%nlon*grd_anl%nsig,b4)
    call wryte(22,4*grd_anl%nlat*grd_anl%nlon*grd_anl%nsig,c4)
    call wryte(22,4*grd_anl%nlat*grd_anl%nlon*grd_anl%nsig,d4)
    call wryte(22,4*grd_anl%nlat*grd_anl%nlon*grd_anl%nsig,e4)
    call wryte(22,4*grd_anl%nlat*grd_anl%nlon*grd_anl%nsig,f4)
    call wryte(22,4*grd_anl%nlat*grd_anl%nlon,g4)
    call baclose(22,iret)
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
  use constants,only: ione,zero,half,one_tenth,rd_over_cp,one
  use gridmod,only: nsig,lat2,lon2,ak5,bk5,ck5,tref5,idvc5
  use gridmod,only: wrf_nmm_regional,nems_nmmb_regional,eta1_ll,eta2_ll,pdtop_ll,pt_ll,&
       regional,wrf_mass_regional,twodvar_regional
  use hybrid_ensemble_parameters, only: grd_ens
! use guess_grids, only: ges_tv,ntguessig
                                                                   use mpimod, only: mype
  implicit none

! Declare passed variables
  real(r_kind),dimension(grd_ens%lat2,grd_ens%lon2)          ,intent(in   ) :: ps
  real(r_kind),dimension(grd_ens%lat2,grd_ens%lon2,nsig)     ,intent(in   ) :: tv
  real(r_kind),dimension(grd_ens%lat2,grd_ens%lon2,nsig+ione),intent(  out) :: prs

! Declare local variables
  real(r_kind) kapr,trk
  integer(i_kind) i,j,k,k2    ! ,it

! Declare local parameter
  real(r_kind),parameter:: ten = 10.0_r_kind

                                     
  kapr=one/rd_over_cp
  prs=zero 
! it=ntguessig

  if (regional) then
     if(wrf_nmm_regional.or.nems_nmmb_regional) then
        do k=1,nsig+ione
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
        do k=1,nsig+ione
           do j=1,grd_ens%lon2
              do i=1,grd_ens%lat2
                 prs(i,j,k)=one_tenth*(eta1_ll(k)*(ten*ps(i,j)-pt_ll) + pt_ll)
              end do
           end do
        end do
     endif
  else
     k=ione
     k2=nsig+ione
     do j=1,grd_ens%lon2
        do i=1,grd_ens%lat2
           prs(i,j,k)=ps(i,j)
           prs(i,j,k2)=zero
        end do
     end do
     if (idvc5 /= 3_i_kind) then
        do k=2,nsig
           do j=1,grd_ens%lon2
              do i=1,grd_ens%lat2
                 prs(i,j,k)=ak5(k)+bk5(k)*ps(i,j)
              end do
           end do
        end do
     else
        do k=2,nsig
           do j=1,grd_ens%lon2
              do i=1,grd_ens%lat2
                 trk=(half*(tv(i,j,k-ione)+tv(i,j,k))/tref5(k))**kapr
                 prs(i,j,k)=ak5(k)+(bk5(k)*ps(i,j))+(ck5(k)*trk)
              end do
           end do
        end do
     end if
  end if

  return
end subroutine general_getprs_glb
