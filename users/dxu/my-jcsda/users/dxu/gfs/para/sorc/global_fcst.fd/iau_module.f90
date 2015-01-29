module iau_module

! module for iau data, io and time interpolation.

 use layout1, only: me, len_trie_ls, len_trio_ls, ls_dim, nodes, lats_node_r, &
                    lats_node_a, me_l_0
 use machine, only: kind_evod, kind_phys
 use namelist_def, only: iaufiles_fg,iaufiles_anl,iaufhrs,iau,iau_delthrs,ens_mem
 use resol_def, only: nlevs=>levs,ntrac,lonf,lonr,latg,num_p2d,num_a2d,num_a3d,num_p3d,latr,latr2
 use mpi_def, only: mc_comp

! iaufiles_fg: filenames for first-guess fields.
! iaufiles_anl: filenames for analysis fields.
! iaufhrs: forecast hours for input files.
! iau_delthrs: length of iau window (in hours).

 implicit none
 private

 public :: init_iau, destroy_iau, getiauforcing

 real(kind_evod), dimension(:,:,:,:),allocatable ::  vrtspec_e,divspec_e,virtempspec_e,&
   vrtspec_o,divspec_o,virtempspec_o
 real(kind_evod), dimension(:,:,:,:,:),allocatable :: tracerspec_e,tracerspec_o
 real(kind_evod), dimension(:,:,:), allocatable :: lnpsspec_e,lnpsspec_o
 integer, public :: nfiles
 logical, public :: iau_initialized = .false.

 contains

 subroutine init_iau(ls_node,ls_nodes,max_ls_nodes,lats_nodes_r,lats_nodes_a,global_lats_r,&
   global_lats_a,lonsperlar,lonsperlat,snnp1ev,snnp1od,plnev_r,plnod_r,plnew_r,plnow_r)

! read in first-guess and analysis files, compute and store increments in
! spectral space.
 
   real(kind=kind_evod), allocatable, dimension(:,:,:) :: &
     vrtspectmp_e,vrtspectmp_o,divspectmp_e,divspectmp_o,virtempspectmp_e,&
     virtempspectmp_o
   real(kind=kind_evod), allocatable, dimension(:,:) :: &
     lnpsspectmp_e,lnpsspectmp_o,topospectmp_e,topospectmp_o
   real(kind=kind_evod), allocatable, dimension(:,:,:,:) :: &
     tracerspectmp_e,tracerspectmp_o
   real(kind=kind_phys) phy_f3d(lonr,nlevs,num_p3d,lats_node_r), &
         phy_f2d(lonr,num_p2d,lats_node_r)
   real (kind=kind_evod) dyn_f3d(lonf,nlevs,num_a3d,lats_node_a), &
         dyn_f2d(lonf,num_a2d,lats_node_a)
   integer, intent(in) :: ls_node(ls_dim,3),ls_nodes(ls_dim,nodes),&
     max_ls_nodes(nodes),lats_nodes_r(nodes),lats_nodes_a(nodes),&
     global_lats_r(latr),global_lats_a(latg),lonsperlar(latr),lonsperlat(latg)
   real(kind=kind_evod),intent(in) ::  &
    snnp1ev(len_trie_ls),snnp1od(len_trio_ls),&
    plnev_r(len_trie_ls,latr2),plnod_r(len_trio_ls,latr2),&
    plnew_r(len_trie_ls,latr2),plnow_r(len_trio_ls,latr2)
   integer, allocatable, dimension(:) :: idt
   character(len=120) filename
   character(len=3) charmem
   real(kind=kind_evod) fhour,pdryini
   integer n,nfilesall,iprint,idate(4),iunit,ierr
   iau_initialized = .true.
   nfilesall = size(iaufiles_anl)
   nfiles = 0
   iprint = 1
   iunit = 77
   do n=1,nfilesall
      if (me .eq. me_l_0) then
         print *,n,trim(adjustl(iaufiles_anl(n)))
         print *,n,trim(adjustl(iaufiles_fg(n)))
      endif
      filename = iaufiles_anl(n)
      if (trim(filename) .eq. '' .or. iaufhrs(n) .lt. 0) exit
      nfiles = nfiles + 1
   enddo
   if (me .eq. me_l_0) print *,'nfiles = ',nfiles
   call mpi_barrier(mc_comp,ierr)
   if (nfiles < 2) then
     print *,'must be at least two files in iaufiles_fg and iaufiles_anal'
     call mpi_quit(9999)
   endif
   allocate(idt(nfiles-1))
   idt = iaufhrs(2:nfiles)-iaufhrs(1:nfiles-1)
   do n=1,nfiles-1
      if (idt(n) .ne. iaufhrs(2)-iaufhrs(1)) then
        print *,'forecast intervals in iaufhrs must be constant'
        call mpi_quit(9999)
      endif
   enddo
   if (me .eq. me_l_0) print *,'iau interval = ',iau_delthrs,' hours'
   deallocate(idt)
   allocate(vrtspec_e(len_trie_ls,2,nlevs,nfiles),vrtspec_o(len_trio_ls,2,nlevs,nfiles))
   allocate(divspec_e(len_trie_ls,2,nlevs,nfiles),divspec_o(len_trio_ls,2,nlevs,nfiles))
   allocate(virtempspec_e(len_trie_ls,2,nlevs,nfiles),virtempspec_o(len_trio_ls,2,nlevs,nfiles))
   allocate(tracerspec_e(len_trie_ls,2,nlevs,ntrac,nfiles),tracerspec_o(len_trio_ls,2,nlevs,ntrac,nfiles))
   allocate(lnpsspec_e(len_trie_ls,2,nfiles),lnpsspec_o(len_trio_ls,2,nfiles))
   allocate(vrtspectmp_e(len_trie_ls,2,nlevs),vrtspectmp_o(len_trio_ls,2,nlevs))
   allocate(divspectmp_e(len_trie_ls,2,nlevs),divspectmp_o(len_trio_ls,2,nlevs))
   allocate(virtempspectmp_e(len_trie_ls,2,nlevs),virtempspectmp_o(len_trio_ls,2,nlevs))
   allocate(tracerspectmp_e(len_trie_ls,2,nlevs,ntrac),tracerspectmp_o(len_trio_ls,2,nlevs,ntrac))
   allocate(lnpsspectmp_e(len_trie_ls,2),lnpsspectmp_o(len_trio_ls,2))
   allocate(topospectmp_e(len_trie_ls,2),topospectmp_o(len_trio_ls,2))
   do n=1,nfiles
      filename = iaufiles_fg(n)
      if (ens_mem > 0) then
        write(charmem,'(i3.3)') ens_mem
        filename = trim(filename) // charmem
      endif
      if (me .eq. me_l_0) print *,'reading ',trim(filename)
      call treadeo(iunit,fhour,idate, &
                   topospectmp_e(1,1), lnpsspectmp_e(1,1), &
                   virtempspectmp_e(1,1,1), divspectmp_e(1,1,1), &
                   vrtspectmp_e(1,1,1), tracerspectmp_e(1,1,1,1), &
                   topospectmp_o(1,1), lnpsspectmp_o(1,1), &
                   virtempspectmp_o(1,1,1), divspectmp_o(1,1,1), &
                   vrtspectmp_o(1,1,1), tracerspectmp_o(1,1,1,1), &
                   ls_node,ls_nodes,max_ls_nodes, &
                   plnev_r, plnod_r, plnew_r, plnow_r,&
                   lats_nodes_r, lats_nodes_a, &
                   snnp1ev,snnp1od,pdryini,iprint, &
                   phy_f3d, phy_f2d, global_lats_r, lonsperlar, &
                   dyn_f3d, dyn_f2d, global_lats_a, lonsperlat, &
                   trim(filename))
      filename = iaufiles_anl(n)
      if (ens_mem > 0) filename = trim(filename) // charmem
      if (me .eq. me_l_0) print *,'reading ',trim(filename)

      call treadeo(iunit,fhour,idate, &
                   topospectmp_e(1,1), lnpsspec_e(1,1,n), &
                   virtempspec_e(1,1,1,n), divspec_e(1,1,1,n), &
                   vrtspec_e(1,1,1,n), tracerspec_e(1,1,1,1,n), &
                   topospectmp_o(1,1), lnpsspec_o(1,1,n), &
                   virtempspec_o(1,1,1,n), divspec_o(1,1,1,n), &
                   vrtspec_o(1,1,1,n), tracerspec_o(1,1,1,1,n), &
                   ls_node,ls_nodes,max_ls_nodes, &
                   plnev_r, plnod_r, plnew_r, plnow_r,&
                   lats_nodes_r, lats_nodes_a, &
                   snnp1ev,snnp1od,pdryini,iprint, &
                   phy_f3d, phy_f2d, global_lats_r, lonsperlar, &
                   dyn_f3d, dyn_f2d, global_lats_a, lonsperlat, &
                   trim(filename))
!!$omp workshare
      vrtspec_e(:,:,:,n) = vrtspec_e(:,:,:,n) - vrtspectmp_e
      divspec_e(:,:,:,n) = divspec_e(:,:,:,n) - divspectmp_e
      virtempspec_e(:,:,:,n) = virtempspec_e(:,:,:,n) - virtempspectmp_e
      tracerspec_e(:,:,:,:,n) = tracerspec_e(:,:,:,:,n) - tracerspectmp_e
      lnpsspec_e(:,:,n) = lnpsspec_e(:,:,n) - lnpsspectmp_e

      vrtspec_o(:,:,:,n) = vrtspec_o(:,:,:,n) - vrtspectmp_o
      divspec_o(:,:,:,n) = divspec_o(:,:,:,n) - divspectmp_o
      virtempspec_o(:,:,:,n) = virtempspec_o(:,:,:,n) - virtempspectmp_o
      tracerspec_o(:,:,:,:,n) = tracerspec_o(:,:,:,:,n) - tracerspectmp_o
      lnpsspec_o(:,:,n) = lnpsspec_o(:,:,n) - lnpsspectmp_o

!!$omp end workshare
   enddo
   deallocate(vrtspectmp_e,vrtspectmp_o)
   deallocate(divspectmp_e,divspectmp_o)
   deallocate(virtempspectmp_e,virtempspectmp_o)
   deallocate(tracerspectmp_e,tracerspectmp_o)
   deallocate(lnpsspectmp_e,lnpsspectmp_o)
   deallocate(topospectmp_e,topospectmp_o)
 end subroutine init_iau

 subroutine getiauforcing(vrtspeciau_e,divspeciau_e,virtempspeciau_e,tracerspeciau_e,lnpsspeciau_e,&
       vrtspeciau_o,divspeciau_o,virtempspeciau_o,tracerspeciau_o,lnpsspeciau_o,t)
        
! compute an iau forcing by interpolating increments to model time set
! and dividing by length of iau window (in seconds).
      
   real(kind_evod), dimension(len_trie_ls,2,nlevs), intent(out) :: &
     vrtspeciau_e,divspeciau_e,virtempspeciau_e
   real(kind_evod), dimension(len_trio_ls,2,nlevs), intent(out) :: &
     vrtspeciau_o,divspeciau_o,virtempspeciau_o
   real(kind_evod), dimension(len_trie_ls,2,nlevs,ntrac), intent(out) ::  &
     tracerspeciau_e
   real(kind_evod), dimension(len_trio_ls,2,nlevs,ntrac), intent(out) ::  &
     tracerspeciau_o
   real(kind_evod), dimension(len_trie_ls,2), intent(out) :: &
     lnpsspeciau_e
   real(kind_evod), dimension(len_trio_ls,2), intent(out) :: &
     lnpsspeciau_o
   real(kind_evod), intent(in) :: t
   real(kind_evod) delt, dt
   integer n
   vrtspeciau_e = 0.; divspeciau_e = 0.
   virtempspeciau_e = 0; lnpsspeciau_e = 0.
   tracerspeciau_e = 0.
   vrtspeciau_o = 0.; divspeciau_o = 0.
   virtempspeciau_o = 0; lnpsspeciau_o = 0.
   tracerspeciau_o = 0.
   dt = iau_delthrs*3600.
   ! set forcing to zero and return if outside iau window.
   if (t < iaufhrs(1)*3600. .or. t > iaufhrs(nfiles)*3600.) then
      if (me .eq. me_l_0) print *,'no iau forcing'
      return
   endif
   if (t .eq. 3600.*iaufhrs(nfiles)) then
!!$omp workshare
     vrtspeciau_e = vrtspec_e(:,:,:,nfiles)/dt
     divspeciau_e = divspec_e(:,:,:,nfiles)/dt
     virtempspeciau_e = virtempspec_e(:,:,:,nfiles)/dt
     tracerspeciau_e = tracerspec_e(:,:,:,:,nfiles)/dt
     vrtspeciau_o = vrtspec_o(:,:,:,nfiles)/dt
     divspeciau_o = divspec_o(:,:,:,nfiles)/dt
     virtempspeciau_o = virtempspec_o(:,:,:,nfiles)/dt
     tracerspeciau_o = tracerspec_o(:,:,:,:,nfiles)/dt
!!$omp end workshare
     lnpsspeciau_e = lnpsspec_e(:,:,nfiles)/dt
     lnpsspeciau_o = lnpsspec_o(:,:,nfiles)/dt
     return
   else if (t .eq. 3600.*iaufhrs(1)) then
!!$omp workshare
     vrtspeciau_e = vrtspec_e(:,:,:,1)/dt
     divspeciau_e = divspec_e(:,:,:,1)/dt
     virtempspeciau_e = virtempspec_e(:,:,:,1)/dt
     tracerspeciau_e = tracerspec_e(:,:,:,:,1)/dt
     vrtspeciau_o = vrtspec_o(:,:,:,1)/dt
     divspeciau_o = divspec_o(:,:,:,1)/dt
     virtempspeciau_o = virtempspec_o(:,:,:,1)/dt
     tracerspeciau_o = tracerspec_o(:,:,:,:,1)/dt
!!$omp end workshare
     lnpsspeciau_e = lnpsspec_e(:,:,1)/dt
     lnpsspeciau_o = lnpsspec_o(:,:,1)/dt
     return
   endif
   do n=1,nfiles
      if (iaufhrs(n)*3600. > t) exit
   enddo
   if (me .eq. me_l_0) print *,'n,t,to',n,t/3600.,iaufhrs(n)
   delt = (iaufhrs(n)-(t/3600.))/(iaufhrs(n)-iaufhrs(n-1))
!!$omp workshare
   vrtspeciau_e = ((1.-delt)*vrtspec_e(:,:,:,n) + delt*vrtspec_e(:,:,:,n-1))/dt
   divspeciau_e = ((1.-delt)*divspec_e(:,:,:,n) + delt*divspec_e(:,:,:,n-1))/dt
   virtempspeciau_e = ((1.-delt)*virtempspec_e(:,:,:,n) + delt*virtempspec_e(:,:,:,n-1))/dt
   tracerspeciau_e = ((1.-delt)*tracerspec_e(:,:,:,:,n) + delt*tracerspec_e(:,:,:,:,n-1))/dt
   vrtspeciau_o = ((1.-delt)*vrtspec_o(:,:,:,n) + delt*vrtspec_o(:,:,:,n-1))/dt
   divspeciau_o = ((1.-delt)*divspec_o(:,:,:,n) + delt*divspec_o(:,:,:,n-1))/dt
   virtempspeciau_o = ((1.-delt)*virtempspec_o(:,:,:,n) + delt*virtempspec_o(:,:,:,n-1))/dt
   tracerspeciau_o = ((1.-delt)*tracerspec_o(:,:,:,:,n) + delt*tracerspec_o(:,:,:,:,n-1))/dt
!!$omp end workshare
   lnpsspeciau_e = ((1.-delt)*lnpsspec_e(:,:,n) + delt*lnpsspec_e(:,:,n-1))/dt
   lnpsspeciau_o = ((1.-delt)*lnpsspec_o(:,:,n) + delt*lnpsspec_o(:,:,n-1))/dt
   if (me .eq. me_l_0) print *,'getiauforcing:',t/3600.,1.-delt,n,iaufhrs(n),delt,n-1,iaufhrs(n-1)

 end subroutine getiauforcing

 subroutine destroy_iau()

! deallocate arrays.

   deallocate(vrtspec_e,vrtspec_o)
   deallocate(divspec_e,divspec_o)
   deallocate(virtempspec_e,virtempspec_o)
   deallocate(tracerspec_e,tracerspec_o)
   deallocate(lnpsspec_e,lnpsspec_o)

 end subroutine destroy_iau

end module iau_module
