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
!   2011-08-31  todling - revisit en_perts (single-prec) in light of extended bundle
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
  use hybrid_ensemble_isotropic, only: en_perts,ps_bar,nelen
  use constants,only: zero,half,fv,rd_over_cp,one
  use mpimod, only: mpi_comm_world,ierror,mype,npe
  use kinds, only: r_kind,i_kind,r_single
  use hybrid_ensemble_parameters, only: grd_ens,nlat_ens,nlon_ens,sp_ens,uv_hyb_ens,beta1_inv
  use control_vectors, only: cvars2d,cvars3d,nc2d,nc3d
  use gsi_bundlemod, only: gsi_bundlecreate
  use gsi_bundlemod, only: gsi_grid
  use gsi_bundlemod, only: gsi_bundle
  use gsi_bundlemod, only: gsi_bundlegetpointer
  use gsi_bundlemod, only: gsi_bundledestroy
  use gsi_bundlemod, only: gsi_gridcreate
  implicit none

  real(r_kind),dimension(grd_ens%lat2,grd_ens%lon2,grd_ens%nsig):: vor,div,u,v,tv,q,cwmr,oz,qs
  real(r_kind),dimension(grd_ens%lat2,grd_ens%lon2):: z,ps,sst2
  real(r_kind),dimension(grd_ens%nlat,grd_ens%nlon):: sst_full,dum
  real(r_single),pointer,dimension(:,:,:):: w3
  real(r_single),pointer,dimension(:,:):: w2
  real(r_kind),pointer,dimension(:,:,:):: x3
  real(r_kind),pointer,dimension(:,:):: x2
  type(gsi_bundle):: en_bar
  type(gsi_grid)  :: grid_ens
  real(r_kind) bar_norm,sig_norm,kapr,kap1,rh
  real(r_kind),allocatable,dimension(:,:,:) :: tsen,prsl,pri

  integer(i_kind),dimension(grd_ens%nlat,grd_ens%nlon):: idum
  integer(i_kind) istatus,iret,i,ic2,ic3,j,k,n,il,jl,mm1,iderivative,im,jm,km
  character(70) filename
  logical ice

  call gsi_gridcreate(grid_ens,grd_ens%lat2,grd_ens%lon2,grd_ens%nsig)
  call gsi_bundlecreate(en_bar,grid_ens,'ensemble',istatus,names2d=cvars2d,names3d=cvars3d)
  if(istatus/=0) then
     write(6,*)' get_gefs_ensperts_dualres: trouble creating en_bar bundle'
     call stop2(999)
  endif

  do n=1,n_ens
     en_perts(n)%valuesr4=zero
  end do
  en_bar%values=zero
  sst2=zero        !    for now, sst not used in ensemble perturbations, so if sst array is called for
                   !      then sst part of en_perts will be zero when sst2=zero

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

! Check read return code.  Revert to static B if read error detected
    if (iret/=0) then
       beta1_inv=one
       if (mype==0) &
            write(6,*)'***WARNING*** ERROR READING ENS FILE : ',filename,' IRET=',IRET,' RESET beta1_inv=',beta1_inv
       cycle
    endif


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

    do ic3=1,nc3d

       call gsi_bundlegetpointer(en_perts(n),trim(cvars3d(ic3)),w3,istatus)
       if(istatus/=0) then
          write(6,*)' error retrieving pointer to ',trim(cvars3d(ic3)),' for ensemble member ',n
          call stop2(999)
       end if
       call gsi_bundlegetpointer(en_bar,trim(cvars3d(ic3)),x3,istatus)
       if(istatus/=0) then
          write(6,*)' error retrieving pointer to ',trim(cvars3d(ic3)),' for en_bar'
          call stop2(999)
       end if

       select case (trim(cvars3d(ic3)))

          case('sf','SF')

!$omp parallel do schedule(dynamic,1) private(i,j,k)
             do k=1,km
                do j=1,jm
                   do i=1,im
                      w3(i,j,k) = u(i,j,k)
                      x3(i,j,k)=x3(i,j,k)+u(i,j,k)
                   end do
                end do
             end do

          case('vp','VP')

!$omp parallel do schedule(dynamic,1) private(i,j,k)
             do k=1,km
                do j=1,jm
                   do i=1,im
                      w3(i,j,k) = v(i,j,k)
                      x3(i,j,k)=x3(i,j,k)+v(i,j,k)
                   end do
                end do
             end do

          case('t','T')

!$omp parallel do schedule(dynamic,1) private(i,j,k)
             do k=1,km
                do j=1,jm
                   do i=1,im
                      w3(i,j,k) = tv(i,j,k)
                      x3(i,j,k)=x3(i,j,k)+tv(i,j,k)
                   end do
                end do
             end do

          case('q','Q')

!$omp parallel do schedule(dynamic,1) private(i,j,k,rh)
             do k=1,km
                do j=1,jm
                   do i=1,im
                      rh=q(i,j,k)/qs(i,j,k)
                      w3(i,j,k) = rh
                      x3(i,j,k)=x3(i,j,k)+rh
                   end do
                end do
             end do

          case('oz','OZ')

!$omp parallel do schedule(dynamic,1) private(i,j,k)
             do k=1,km
                do j=1,jm
                   do i=1,im
                      w3(i,j,k) = oz(i,j,k)
                      x3(i,j,k)=x3(i,j,k)+oz(i,j,k)
                   end do
                end do
             end do

          case('cw','CW')

!$omp parallel do schedule(dynamic,1) private(i,j,k)
             do k=1,km
                do j=1,jm
                   do i=1,im
                      w3(i,j,k) = cwmr(i,j,k)
                      x3(i,j,k)=x3(i,j,k)+cwmr(i,j,k)
                   end do
                end do
             end do

       end select
    end do

    do ic2=1,nc2d

       call gsi_bundlegetpointer(en_perts(n),trim(cvars2d(ic2)),w2,istatus)
       if(istatus/=0) then
          write(6,*)' error retrieving pointer to ',trim(cvars2d(ic2)),' for ensemble member ',n
          call stop2(999)
       end if
       call gsi_bundlegetpointer(en_bar,trim(cvars2d(ic2)),x2,istatus)
       if(istatus/=0) then
          write(6,*)' error retrieving pointer to ',trim(cvars2d(ic2)),' for en_bar'
          call stop2(999)
       end if

       select case (trim(cvars2d(ic2)))

          case('ps','PS')

!$omp parallel do schedule(dynamic,1) private(i,j)
             do j=1,jm
                do i=1,im
                   w2(i,j) = ps(i,j)
                   x2(i,j)=x2(i,j)+ps(i,j)
                end do
             end do

          case('sst','SST')

!$omp parallel do schedule(dynamic,1) private(i,j)
             do j=1,jm
                do i=1,im
                   w2(i,j) = sst2(i,j)
                   x2(i,j)=x2(i,j)+sst2(i,j)
                end do
             end do

       end select
    end do
  end do ! end do over ensemble

! Convert to mean
  bar_norm = one/float(n_ens)
!$omp parallel do schedule(dynamic,1) private(i)
  do i=1,nelen
     en_bar%values(i)=en_bar%values(i)*bar_norm
  end do

! Copy pbar to module array.  ps_bar may be needed for vertical localization
! in terms of scale heights/normalized p/p 
  do ic2=1,nc2d

     if(trim(cvars2d(ic2))=='ps'.or.trim(cvars2d(ic2))=='PS') then

        call gsi_bundlegetpointer(en_bar,trim(cvars2d(ic2)),x2,istatus)
        if(istatus/=0) then
           write(6,*)' error retrieving pointer to ',trim(cvars2d(ic2)),' for en_bar'
           call stop2(999)
        end if

        do j=1,grd_ens%lon2
           do i=1,grd_ens%lat2
              ps_bar(i,j)=x2(i,j)
           end do
        end do
        exit
     end if
  end do

! Before converting to perturbations, get ensemble spread
  if (write_ens_sprd) call ens_spread_dualres(en_bar,mype)

! Convert ensemble members to perturbations
  sig_norm=sqrt(one/max(one,n_ens-one))
   

!$omp parallel do schedule(dynamic,1) private(n,i)
  do n=1,n_ens
    do i=1,nelen
      en_perts(n)%valuesr4(i)=(en_perts(n)%valuesr4(i)-en_bar%values(i))*sig_norm
    end do
  end do

! If request, zero out ozone perturbations for hybrid
  if (oz_univ_static) then
     do ic3=1,nc3d
        if(trim(cvars3d(ic3))=='oz'.or.trim(cvars3d(ic3))=='OZ') then

           do n=1,n_ens
              call gsi_bundlegetpointer(en_perts(n),trim(cvars3d(ic3)),w3,istatus)
              if(istatus/=0) then
                 write(6,*)' error retrieving pointer to ',trim(cvars3d(ic3)),' for ensemble member ',n
                 call stop2(999)
              end if
              do k=1,km
                 do j=1,jm
                    do i=1,im
                       w3(i,j,k) = zero
                    end do
                 end do
              end do
           end do
        end if
     end do
  end if

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

!     following commented out--see above where sst2=zero at beginning of subroutine
!! dtk: temporarily ignore sst perturbations in hybrid
!  do n=1,n_ens
!    do i=1,grd_ens%latlon11
!      sst_en(i,n)=zero
!    end do
!  end do

   call gsi_bundledestroy(en_bar,istatus)
   if(istatus/=0) then
      write(6,*)' in get_gefs_ensperts_dualres: trouble destroying en_bar bundle'
             call stop2(999)
          endif

  return
end subroutine get_gefs_ensperts_dualres

subroutine ens_spread_dualres(en_bar,mype)
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
!   2011-03-19  parrish - add pseudo-bundle capability
!
!   input argument list:
!     en_bar - ensemble mean
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
  use hybrid_ensemble_isotropic, only: en_perts,nelen
  use general_sub2grid_mod, only: sub2grid_info,general_sub2grid_create_info,general_sube2suba
  use constants, only:  zero,two,half,one
  use control_vectors, only: cvars2d,cvars3d,nc2d,nc3d
  use gsi_bundlemod, only: gsi_bundlecreate
  use gsi_bundlemod, only: gsi_grid
  use gsi_bundlemod, only: gsi_bundle
  use gsi_bundlemod, only: gsi_bundlegetpointer
  use gsi_bundlemod, only: gsi_bundledestroy
  use gsi_bundlemod, only: gsi_gridcreate
  implicit none

  type(gsi_bundle),intent(in):: en_bar
  integer(i_kind),intent(in):: mype

  type(gsi_bundle):: sube,suba
  type(gsi_grid):: grid_ens,grid_anl
  real(r_kind) sp_norm
  type(sub2grid_info)::se,sa

  integer(i_kind) i,ii,n,ic3,k
  logical regional
  integer(i_kind) num_fields,inner_vars,istat,istatus
  logical,allocatable::vector(:)
  real(r_kind),pointer,dimension(:,:,:):: st,vp,tv,rh,oz,cw
  real(r_kind),pointer,dimension(:,:):: ps
  real(r_kind),dimension(grd_anl%lat2,grd_anl%lon2,grd_anl%nsig),target::dum3
  real(r_kind),dimension(grd_anl%lat2,grd_anl%lon2),target::dum2

!      create simple regular grid
        call gsi_gridcreate(grid_anl,grd_anl%lat2,grd_anl%lon2,grd_anl%nsig)
        call gsi_gridcreate(grid_ens,grd_ens%lat2,grd_ens%lon2,grd_ens%nsig)

!      create two internal bundles, one on analysis grid and one on ensemble grid

       call gsi_bundlecreate (suba,grid_anl,'ensemble work',istatus, &
                                 names2d=cvars2d,names3d=cvars3d)
       if(istatus/=0) then
          write(6,*)' ens_spread_dualres: trouble creating bundle_anl bundle'
          call stop2(999)
       endif
       call gsi_bundlecreate (sube,grid_ens,'ensemble work ens',istatus, &
                                 names2d=cvars2d,names3d=cvars3d)
       if(istatus/=0) then
          write(6,*)' ens_spread_dualres: trouble creating bundle_ens bundle'
          call stop2(999)
       endif

  sp_norm=(one/float(n_ens))

  sube%values=zero
  do n=1,n_ens
     do i=1,nelen
        sube%values(i)=sube%values(i) &
           +(en_perts(n)%valuesr4(i)-en_bar%values(i))*(en_perts(n)%valuesr4(i)-en_bar%values(i))
     end do
  end do

  do i=1,nelen
    sube%values(i) = sqrt(sp_norm*sube%values(i))
  end do

  if(grd_ens%latlon1n == grd_anl%latlon1n) then
     do i=1,nelen
        suba%values(i)=sube%values(i)
     end do
  else
     regional=.false.
     inner_vars=1
     num_fields=max(0,nc3d)*grd_ens%nsig+max(0,nc2d)
     allocate(vector(num_fields))
     vector=.false.
     do ic3=1,nc3d
        if(trim(cvars3d(ic3))=='sf'.or.trim(cvars3d(ic3))=='vp') then
           do k=1,grd_ens%nsig
              vector((ic3-1)*grd_ens%nsig+k)=uv_hyb_ens
           end do
        end if
     end do
     call general_sub2grid_create_info(se,inner_vars,grd_ens%nlat,grd_ens%nlon,grd_ens%nsig,num_fields, &
                                       regional,vector)
     call general_sub2grid_create_info(sa,inner_vars,grd_anl%nlat,grd_anl%nlon,grd_anl%nsig,num_fields, &
                                       regional,vector)
     deallocate(vector)
     call general_sube2suba(se,sa,p_e2a,sube%values,suba%values,regional)
  end if

  dum2=zero
  dum3=zero
  call gsi_bundlegetpointer(suba,'sf',st,istat)
  if(istat/=0) then
     write(6,*)' no sf pointer in ens_spread_dualres, point st at dum3 array'
     st => dum3
  end if
  call gsi_bundlegetpointer(suba,'vp',vp,istat)
  if(istat/=0) then
     write(6,*)' no vp pointer in ens_spread_dualres, point vp at dum3 array'
     vp => dum3
  end if
  call gsi_bundlegetpointer(suba,'t',tv,istat)
  if(istat/=0) then
     write(6,*)' no t pointer in ens_spread_dualres, point tv at dum3 array'
     tv => dum3
  end if
  call gsi_bundlegetpointer(suba,'q',rh,istat)
  if(istat/=0) then
     write(6,*)' no q pointer in ens_spread_dualres, point rh at dum3 array'
     rh => dum3
  end if
  call gsi_bundlegetpointer(suba,'oz',oz,istat)
  if(istat/=0) then
     write(6,*)' no oz pointer in ens_spread_dualres, point oz at dum3 array'
     oz => dum3
  end if
  call gsi_bundlegetpointer(suba,'cw',cw,istat)
  if(istat/=0) then
     write(6,*)' no cw pointer in ens_spread_dualres, point cw at dum3 array'
     cw => dum3
  end if
  call gsi_bundlegetpointer(suba,'ps',ps,istat)
  if(istat/=0) then
     write(6,*)' no ps pointer in ens_spread_dualres, point ps at dum2 array'
     ps => dum2
  end if

  call write_spread_dualres(st,vp,tv,rh,oz,cw,ps,mype)

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
