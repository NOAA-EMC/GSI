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
!   2011-09-14  todling - add prototype for general ensemble reader via
!   2011-11-01  kleist  - 4d capability for ensemble/hybrid
!   2013-01-16  parrish - strange error in make debug on wcoss related to
!                          grd_ens%lat2, grd_ens%lon2, grd_ens%nsig
!                        replaced with im, jm, km which are set equal to these
!                        at beginning of program and this made error go away.
!                         FOLLOWING is sample error message from make debug on tide:
!
!                         get_gefs_ensperts_dualres.f90(182): error #6460: This is not a field name that
!                                 is defined in the encompassing structure.   [LAT2]
!                         call genqsat(qs,tsen,prsl,grd_ens%lat2,grd_ens%lon2,grd_ens%nsig,ice,iderivative)
!   2014-11-30  todling - partially generalized to handle any control vector
!                         (GFS hook needs further attention)
!                       - also, take SST from members of ensemble
!                       - avoid alloc GFS workscape when not GFS
!   2014-12-03  derber  - Simplify code and optimize routine - turn off reading
!                         of vort/div and surface height since not needed
!   2014-12-05  zhu     - set lower bound for cwmr
!   2016-07-01  mahajan - use GSI ensemble coupler
!   2018-02-15  wu      - add code for fv3_regional option 
!   2019-03-13  eliu    - add precipitation component 
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

  use mpeu_util, only: die
  use gridmod, only: idsl5
  use hybrid_ensemble_parameters, only: n_ens,write_ens_sprd,oz_univ_static,ntlevs_ens
  use hybrid_ensemble_parameters, only: en_perts,ps_bar,nelen
  use constants,only: zero,zero_single,half,fv,rd_over_cp,one,qcmin
  use mpimod, only: mpi_comm_world,mype,npe
  use kinds, only: r_kind,i_kind,r_single
  use hybrid_ensemble_parameters, only: grd_ens,q_hyb_ens
  use hybrid_ensemble_parameters, only: beta_s0,beta_s,beta_e
  use control_vectors, only: cvars2d,cvars3d,nc2d,nc3d
  use gsi_bundlemod, only: gsi_bundlecreate
  use gsi_bundlemod, only: gsi_grid
  use gsi_bundlemod, only: gsi_bundle
  use gsi_bundlemod, only: gsi_bundlegetpointer
  use gsi_bundlemod, only: gsi_bundledestroy
  use gsi_bundlemod, only: gsi_gridcreate
  use gsi_enscouplermod, only: gsi_enscoupler_get_user_nens
  use gsi_enscouplermod, only: gsi_enscoupler_create_sub2grid_info
  use gsi_enscouplermod, only: gsi_enscoupler_destroy_sub2grid_info
  use general_sub2grid_mod, only: sub2grid_info,general_sub2grid_create_info,general_sub2grid_destroy_info
  implicit none

  real(r_kind),pointer,dimension(:,:)   :: ps
  real(r_kind),pointer,dimension(:,:,:) :: tv
  real(r_kind),pointer,dimension(:,:,:) :: q
! real(r_kind),dimension(grd_ens%nlat,grd_ens%nlon):: sst_full,dum
  real(r_kind),pointer,dimension(:,:,:):: p3
  real(r_kind),pointer,dimension(:,:):: x2
  type(gsi_bundle),allocatable,dimension(:) :: en_read
  type(gsi_bundle):: en_bar
! type(gsi_grid)  :: grid_ens
  real(r_kind) bar_norm,sig_norm,kapr,kap1
! real(r_kind),allocatable,dimension(:,:):: z,sst2
  real(r_kind),allocatable,dimension(:,:,:) :: tsen,prsl,pri,qs

! integer(i_kind),dimension(grd_ens%nlat,grd_ens%nlon):: idum
  integer(i_kind) istatus,iret,i,ic3,j,k,n,iderivative,im,jm,km,m,ipic
! integer(i_kind) mm1
  integer(i_kind) ipc3d(nc3d),ipc2d(nc2d)
  integer(i_kind) ier
! integer(i_kind) il,jl
  logical ice,hydrometeor 
  type(sub2grid_info) :: grd_tmp

! Create perturbations grid and get variable names from perturbations
  if(en_perts(1,1)%grid%im/=grd_ens%lat2.or. &
     en_perts(1,1)%grid%jm/=grd_ens%lon2.or. &
     en_perts(1,1)%grid%km/=grd_ens%nsig ) then
     if (mype==0) then
        write(6,*) 'get_gefs_ensperts_dualres: grd_ens ', grd_ens%lat2,grd_ens%lon2,grd_ens%nsig
        write(6,*) 'get_gefs_ensperts_dualres: pertgrid', en_perts(1,1)%grid%im, en_perts(1,1)%grid%jm, en_perts(1,1)%grid%km
        write(6,*) 'get_gefs_ensperts_dualres: inconsistent dims, aborting ...'
     endif
     call stop2(999)
 endif

  call gsi_bundlegetpointer (en_perts(1,1),cvars3d,ipc3d,istatus)
  if(istatus/=0) then
    write(6,*) ' get_gefs_ensperts_dualres',': cannot find 3d pointers'
    call stop2(999)
  endif
  call gsi_bundlegetpointer (en_perts(1,1),cvars2d,ipc2d,istatus)
  if(istatus/=0) then
    write(6,*) ' get_gefs_ensperts_dualres',': cannot find 2d pointers'
    call stop2(999)
  endif


  im=en_perts(1,1)%grid%im
  jm=en_perts(1,1)%grid%jm
  km=en_perts(1,1)%grid%km
  bar_norm = one/float(n_ens)
  sig_norm=sqrt(one/max(one,n_ens-one))

  ! Create temporary communication information for read ensemble routines
  call gsi_enscoupler_create_sub2grid_info(grd_tmp,km,npe,grd_ens)

  ! Allocate bundle to hold mean of ensemble members
  call gsi_bundlecreate(en_bar,en_perts(1,1)%grid,'ensemble',istatus,names2d=cvars2d,names3d=cvars3d)
  if ( istatus /= 0 ) &
     call die('get_gefs_ensperts_dualres',': trouble creating en_bar bundle, istatus =',istatus)

  ! Allocate bundle used for reading members
  allocate(en_read(n_ens))
  do n=1,n_ens
     call gsi_bundlecreate(en_read(n),en_perts(1,1)%grid,'ensemble member',istatus,names2d=cvars2d,names3d=cvars3d)
     if ( istatus /= 0 ) &
        call die('get_gefs_ensperts_dualres',': trouble creating en_read bundle, istatus =',istatus)
  end do

! allocate(z(im,jm))
! allocate(sst2(im,jm))

! sst2=zero        !    for now, sst not used in ensemble perturbations, so if sst array is called for
                   !      then sst part of en_perts will be zero when sst2=zero

!$omp parallel do schedule(dynamic,1) private(m,n)
  do m=1,ntlevs_ens
     do n=1,n_ens
       en_perts(n,m)%valuesr4=zero_single
     end do
  end do


  ntlevs_ens_loop: do m=1,ntlevs_ens

     call gsi_enscoupler_get_user_Nens(grd_tmp,n_ens,m,en_read,iret)

     ! Check read return code.  Revert to static B if read error detected
     if ( iret /= 0 ) then
        beta_s0=one
        beta_s=one
        beta_e=zero
        if ( mype == 0 ) &
           write(6,'(A,I4,A,F6.3)')'***WARNING*** ERROR READING ENS FILE, iret = ',iret,' RESET beta_s0 = ',beta_s0
        cycle
     endif

     if (.not.q_hyb_ens) then !use RH
       kap1=rd_over_cp+one
       kapr=one/rd_over_cp
       do n=1,n_ens

         call gsi_bundlegetpointer(en_read(n),'ps',ps,ier);istatus=ier
         call gsi_bundlegetpointer(en_read(n),'t' ,tv,ier);istatus=istatus+ier
         call gsi_bundlegetpointer(en_read(n),'q' ,q ,ier);istatus=istatus+ier
! Compute RH
! Get 3d pressure field now on interfaces
         allocate(pri(im,jm,km+1))
         call general_getprs_glb(ps,tv,pri)
         allocate(prsl(im,jm,km),tsen(im,jm,km),qs(im,jm,km))
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
         call genqsat(qs,tsen,prsl,im,jm,km,ice,iderivative)
         do k=1,km
            do j=1,jm
               do i=1,im
                  q(i,j,k)=q(i,j,k)/qs(i,j,k)
               end do
            end do
         end do
         deallocate(tsen,prsl,qs)
       enddo
     end if


     en_bar%values=zero

     n_ens_loop: do n=1,n_ens


!$omp parallel do schedule(dynamic,1) private(i,k,j,ic3,hydrometeor,istatus,p3)
       do ic3=1,nc3d

          hydrometeor = trim(cvars3d(ic3))=='cw' .or. trim(cvars3d(ic3))=='ql' .or. &
                        trim(cvars3d(ic3))=='qi' .or. trim(cvars3d(ic3))=='qr' .or. &
                        trim(cvars3d(ic3))=='qs' .or. trim(cvars3d(ic3))=='qg' .or. &
                        trim(cvars3d(ic3))=='qh'

          call gsi_bundlegetpointer(en_read(n),trim(cvars3d(ic3)),p3,istatus)
          if(istatus/=0) then
             write(6,*)' error retrieving pointer to ',trim(cvars3d(ic3)),' from read in member ',n,m
             call stop2(999)
          end if


          if ( hydrometeor ) then                
             do k=1,km
                do j=1,jm
                   do i=1,im
                      p3(i,j,k) = max(p3(i,j,k),qcmin)
                   end do
                end do
             end do

          else if ( trim(cvars3d(ic3)) == 'oz' .and. oz_univ_static ) then
             p3 = zero
          end if

       end do !c3d
       do i=1,nelen
          en_perts(n,m)%valuesr4(i)=en_read(n)%values(i)
          en_bar%values(i)=en_bar%values(i)+en_read(n)%values(i)
       end do


! NOTE: if anyone implements alternative use of SST (as from sst2) care need
!       be given to those applications getting SST directly from the members of
!       the ensemble for which this code is already handling - i.e., I don't
!       know who would want to commented out code below but be mindful 
!       of how it interacts with option sst_staticB, please - Todling.

     end do n_ens_loop ! end do over ensemble

     do i=1,nelen
        en_bar%values(i)=en_bar%values(i)*bar_norm
     end do

! Before converting to perturbations, get ensemble spread
     !-- if (m == 1 .and. write_ens_sprd )  call ens_spread_dualres(en_bar,1)
     !!! it is not clear of the next statement is thread/$omp safe.
     if (write_ens_sprd )  call ens_spread_dualres(en_bar,m)


     call gsi_bundlegetpointer(en_bar,'ps',x2,istatus)
     if(istatus/=0) &
          call die('get_gefs_ensperts_dualres:',' error retrieving pointer to (ps) for en_bar, istatus = ', istatus)

! Copy pbar to module array.  ps_bar may be needed for vertical localization
! in terms of scale heights/normalized p/p
! Convert to mean
     do j=1,jm
        do i=1,im
           ps_bar(i,j,m)=x2(i,j)
        end do
     end do

! Convert ensemble members to perturbations

!$omp parallel do schedule(dynamic,1) private(n,i,ic3,ipic,k,j)
     do n=1,n_ens
        do i=1,nelen
           en_perts(n,m)%valuesr4(i)=en_perts(n,m)%valuesr4(i)-en_bar%values(i)
        end do
        if(.not. q_hyb_ens) then
          do ic3=1,nc3d
             if(trim(cvars3d(ic3)) == 'q' .or. trim(cvars3d(ic3)) == 'Q')then
                ipic=ipc3d(ic3)
                do k=1,km
                   do j=1,jm
                      do i=1,im
                         en_perts(n,m)%r3(ipic)%qr4(i,j,k) = min(en_perts(n,m)%r3(ipic)%qr4(i,j,k),1._r_single)
                         en_perts(n,m)%r3(ipic)%qr4(i,j,k) = max(en_perts(n,m)%r3(ipic)%qr4(i,j,k),-1._r_single)
                      end do
                   end do
                end do
             end if
          end do
        end if
        do i=1,nelen
           en_perts(n,m)%valuesr4(i)=en_perts(n,m)%valuesr4(i)*sig_norm
        end do
     end do
  end do  ntlevs_ens_loop !end do over bins

  do n=n_ens,1,-1
     call gsi_bundledestroy(en_read(n),istatus)
     if ( istatus /= 0 ) &
        call die('get_gefs_ensperts_dualres',': trouble destroying en_read bundle, istatus = ', istatus)
  end do
  deallocate(en_read)
  call gsi_enscoupler_destroy_sub2grid_info(grd_tmp)

! mm1=mype+1
!  since initial version is ignoring sst perturbations, skip following code for now.  revisit
!   later--creating general_read_gfssfc, analogous to general_read_gfsatm above.
!! GET SST PERTURBATIONS HERE
!  do n=1,n_ens
!    write(filename,105) n
!105        format('sfcf06_ens_mem',i3.3)
!
!! This will get full 2d nlat x nlon sst field
!    if(mype==0)write(6,*) 'CALL READ_GFSSFC FOR ENS FILE : ',filename
!    call read_gfssfc(filename,&
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

!  deallocate(sst2)
!  deallocate(z)

  return
end subroutine get_gefs_ensperts_dualres

subroutine ens_spread_dualres(en_bar,ibin)
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
!   2011-11-01  kleist  - 4d capability for ensemble/hybrid
!   2019-07-10  todling - truly handling 4d output; and upd to out all ens c-variables
!
!   input argument list:
!     en_bar - ensemble mean
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
  use hybrid_ensemble_parameters, only: en_perts,nelen
  use general_sub2grid_mod, only: sub2grid_info,general_sub2grid_create_info,general_sube2suba
  use constants, only:  zero,two,half,one
  use control_vectors, only: cvars2d,cvars3d,nc2d,nc3d
  use mpeu_util, only: getindex   
  use gsi_bundlemod, only: gsi_bundlecreate
  use gsi_bundlemod, only: gsi_grid
  use gsi_bundlemod, only: gsi_bundle
  use gsi_bundlemod, only: gsi_bundlegetpointer
  use gsi_bundlemod, only: gsi_bundledestroy
  use gsi_bundlemod, only: gsi_gridcreate
  implicit none

  type(gsi_bundle),intent(in):: en_bar
  integer(i_kind),intent(in):: ibin

  type(gsi_bundle):: sube,suba
  type(gsi_grid):: grid_ens,grid_anl
  real(r_kind) sp_norm
  type(sub2grid_info)::se,sa

  integer(i_kind) i,n,ic3,k
  logical regional
  integer(i_kind) num_fields,inner_vars,istatus
  logical,allocatable::vector(:)

!      create simple regular grid
  call gsi_gridcreate(grid_anl,grd_anl%lat2,grd_anl%lon2,grd_anl%nsig)
  call gsi_gridcreate(grid_ens,grd_ens%lat2,grd_ens%lon2,grd_ens%nsig)

! create two internal bundles, one on analysis grid and one on ensemble grid

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
           +(en_perts(n,ibin)%valuesr4(i)-en_bar%values(i))*(en_perts(n,ibin)%valuesr4(i)-en_bar%values(i))
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

  call write_spread_dualres(ibin,suba)

  return
end subroutine ens_spread_dualres


subroutine write_spread_dualres(ibin,bundle)
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
!   2018-04-01  eliu - add hydrometeors 
!   2019-07-10  todling - generalize to write out all variables in the ensemble
!                       - also allows for print out of different time bins
!
!   input argument list:
!     bundle -  spread bundle
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$ end documentation block
  use mpimod, only: mype
  use kinds, only: r_kind,i_kind,r_single
  use guess_grids, only: get_ref_gesprs 
  use hybrid_ensemble_parameters, only: grd_anl
  use gsi_bundlemod, only: gsi_grid
  use gsi_bundlemod, only: gsi_bundle
  use gsi_bundlemod, only: gsi_bundlegetpointer
  use control_vectors, only: cvars2d,cvars3d,nc2d,nc3d
  use constants, only: zero
  implicit none

  integer(i_kind), intent(in) :: ibin
  type(gsi_bundle):: bundle

! local variables
  character(255):: grdfile,grdctl

  real(r_kind),allocatable,dimension(:,:,:):: work8_3d
  real(r_kind),allocatable,dimension(:,:):: work8_2d

  real(r_single),allocatable,dimension(:,:,:):: work4_3d
  real(r_single),allocatable,dimension(:,:):: work4_2d

  real(r_kind),pointer,dimension(:,:,:):: ptr3d
  real(r_kind),pointer,dimension(:,:):: ptr2d

  integer(i_kind) iret,i,j,k,n,mem2d,mem3d,num3d,lu,istat
  real(r_kind),dimension(grd_anl%nsig+1) :: prs

! Initial memory used by 2d and 3d grids
  mem2d = 4*grd_anl%nlat*grd_anl%nlon
  mem3d = 4*grd_anl%nlat*grd_anl%nlon*grd_anl%nsig
  num3d=11

  allocate(work8_3d(grd_anl%nlat,grd_anl%nlon,grd_anl%nsig))
  allocate(work8_2d(grd_anl%nlat,grd_anl%nlon))
  allocate(work4_3d(grd_anl%nlon,grd_anl%nlat,grd_anl%nsig))
  allocate(work4_2d(grd_anl%nlon,grd_anl%nlat))

  if (mype==0) then
    write(grdfile,'(a,i3.3,a)') 'ens_spread_',ibin, '.grd'
    call baopenwt(22,trim(grdfile),iret)
    write(6,*)'WRITE_SPREAD_DUALRES:  open 22 to ',trim(grdfile),' with iret=',iret
  endif

! Process 3d arrays
  do n=1,nc3d
    call gsi_bundlegetpointer(bundle,cvars3d(n),ptr3d,istat)
    work8_3d=zero
    do k=1,grd_anl%nsig
      call gather_stuff2(ptr3d(1,1,k),work8_3d(1,1,k),mype,0)
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
  do n=1,nc2d
    call gsi_bundlegetpointer(bundle,cvars2d(n),ptr2d,istat)
    work8_2d=zero
    call gather_stuff2(ptr2d,work8_2d,mype,0)
    if (mype==0) then
       do j=1,grd_anl%nlon
          do i=1,grd_anl%nlat
             work4_2d(j,i)=work8_2d(i,j)
          end do
       end do
       call wryte(22,mem2d,work4_2d)
       write(6,*)'WRITE_SPREAD_DUALRES FOR 2D FIELD '
    endif
  end do

! Close byte-addressable binary file for grads
  if (mype==0) then
     call baclose(22,iret)
     write(6,*)'WRITE_SPREAD_DUALRES:  close 22 with iret=',iret
  end if

! Get reference pressure levels for grads purposes
  call get_ref_gesprs(prs)

! Write out a corresponding grads control file
  if (mype==0) then
     write(grdctl,'(a,i3.3,a)') 'ens_spread_',ibin, '.ctl'
     open(newunit=lu,file=trim(grdctl),form='formatted')
     write(lu,'(2a)') 'DSET  ^', trim(grdfile)
     write(lu,'(2a)') 'TITLE ', 'gsi ensemble spread'
     write(lu,'(a,2x,e13.6)') 'UNDEF', 1.E+15 ! any other preference for this?
     write(lu,'(a,2x,i4,2x,a,2x,f5.1,2x,f9.6)') 'XDEF',grd_anl%nlon, 'LINEAR',   0.0, 360./grd_anl%nlon
     write(lu,'(a,2x,i4,2x,a,2x,f5.1,2x,f9.6)') 'YDEF',grd_anl%nlat, 'LINEAR', -90.0, 180./(grd_anl%nlat-1.)
     write(lu,'(a,2x,i4,2x,a,100(1x,f10.5))')      'ZDEF',grd_anl%nsig, 'LEVELS', prs(1:grd_anl%nsig)
     write(lu,'(a,2x,i4,2x,a)')   'TDEF', 1, 'LINEAR 12:00Z04JUL1776 6hr' ! any date suffices
     write(lu,'(a,2x,i4)')        'VARS',nc3d+nc2d
     do n=1,nc3d
        write(lu,'(a,1x,2(i4,1x),a)') trim(cvars3d(n)),grd_anl%nsig,0,trim(cvars3d(n))
     enddo
     do n=1,nc2d
        write(lu,'(a,1x,2(i4,1x),a)') trim(cvars2d(n)),           1,0,trim(cvars2d(n))
     enddo
     write(lu,'(a)') 'ENDVARS'
     close(lu)
  endif

! clean up
  deallocate(work4_2d)
  deallocate(work4_3d)
  deallocate(work8_2d)
  deallocate(work8_3d)

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
!   2017-03-23  Hu      - add code to use hybrid vertical coodinate in WRF MASS CORE.
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
  use gridmod,only: nsig,ak5,bk5,ck5,tref5,idvc5
  use gridmod,only: wrf_nmm_regional,nems_nmmb_regional,eta1_ll,eta2_ll,pdtop_ll,pt_ll,&
       regional,wrf_mass_regional,twodvar_regional,fv3_regional
  use hybrid_ensemble_parameters, only: grd_ens
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
     elseif (fv3_regional) then
        do k=1,nsig+1
           do j=1,grd_ens%lon2
              do i=1,grd_ens%lat2
                 prs(i,j,k)=eta1_ll(k)+ eta2_ll(k)*ps(i,j)
              end do
           end do
        end do

     elseif (twodvar_regional) then
        do k=1,nsig+1
           do j=1,grd_ens%lon2
              do i=1,grd_ens%lat2
                 prs(i,j,k)=one_tenth*(eta1_ll(k)*(ten*ps(i,j)-pt_ll) + pt_ll)
              end do
           end do
        end do
     elseif (wrf_mass_regional) then
        do k=1,nsig+1
           do j=1,grd_ens%lon2
              do i=1,grd_ens%lat2
                 prs(i,j,k)=one_tenth*(eta1_ll(k)*(ten*ps(i,j)-pt_ll) + &
                                       eta2_ll(k) + pt_ll)
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
