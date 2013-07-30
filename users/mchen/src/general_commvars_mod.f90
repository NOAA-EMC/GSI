module general_commvars_mod
!$$$ module documentation block
!           .      .    .                                       .
! module:   general_commvars_mod
!   prgmmr: parrish     org: np22                date: 2012-06-25
!
! abstract: Replace subroutine init_commvars with this module to replace specialized sub2grid/grid2sub
!           type operations with the easier to use general_sub2grid.
!
! program history log:
!   2012-06-25 parrish
!
! Subroutines Included:
!   sub init_general_commvars - initialize type(sub2grid_info) structure variables 
!   sub destroy_general_commvars - deallocate various pointer arrays in type(sub2grid_info) structures
!
! Variable Definitions:
!   def s2g_raf - used for subdomain to horizontal grid transfers of full control vector with motley variables
!   def s2g_cv  - used in bkerror.f90 (full control vector without motley variables)
!   def s2g2    - used in getprs.f90
!   def s2g4    - used in get_derivatives2.f90
!   def s2guv   - used in getuv.f90
!   def s2g_d   - used in get_derivatives.f90
!   def g1      - used in get_derivatives.f90
!   def g3      - used in bkgcov_rewgt.f90
!   def g33p1   - used in bkgcov_rewgt.f90
!
! attributes:
!   langauge: f90
!   machgine:
!
!$$$ end documentation block

   use kinds, only: r_kind,i_kind
   use general_sub2grid_mod, only: sub2grid_info

   implicit none

! set default to private
   private
! set subroutines to public
   public :: init_general_commvars
   public :: destroy_general_commvars
! set passed variables to public
   public :: s2g_raf                 !  structure used with all grid components of control vector,
                                     !    including motley variables.
   public :: s2g_cv                  !  structure used in bkerror.f90
   public :: s2g2                    !  structure used in getprs.f90
   public :: s2g4                    !  structure used in get_derivatives2.f90
   public :: s2guv                   !  structure used in getuv.f90
   public :: s2g_d                   !  structure used in get_derivatives.f90
   public :: g1                      !  structure used in get_derivatives.f90
   public :: g3                      !  structure used in bkgcov_rewgt.f90
   public :: g33p1                   !  for 3 3d fields + 1 2d field, no particular order

! Declare types

   type(sub2grid_info),save :: s2g_raf,s2g_cv,s2g2,s2g4,s2guv,s2g_d,g1,g3,g33p1

contains

!   create general_sub2grid structure variables currently made locally for get_derivatives, etc.

   subroutine init_general_commvars
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    init_general_commvars
!     prgmmr:    parrish     org: np22                date: 2012-06-25
!
! abstract:  create various type(sub2grid_info) struture variables for use in transformations between
!            subdomain and full horizontal grid data array decomposition.
!
! program history log:
!   2012-06-25  parrish
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp
!
!$$$ end documentation block

      use gridmod, only: regional,nlat,nlon,nsig,nnnn1o,nsig1o
      use gridmod, only: displs_s,ird_s,ltosj,itotsub,ltosi_s,ltosj_s,&
                         ijn_s,irc_s,ijn,displs_g,ltosi,isc_g,isd_g,vlevs
      use mpimod, only: npe,levs_id,nvar_id,nvar_pe
      use control_vectors, only: cvars2d,cvars3d,mvars,cvarsmd,nrf_var
      use general_sub2grid_mod, only: general_sub2grid_create_info
      use mpeu_util, only: getindex

      implicit none

!  Local Variables
      integer(i_kind) i,j,k,kk,num_fields,inner_vars,l,n,n_one,n2d,n3d
      character(len=64),allocatable,dimension(:,:) :: names_s2g_d,names_s2g_raf
      integer(i_kind),allocatable,dimension(:,:) :: lnames_s2g_raf
      logical,allocatable,dimension(:) :: vector_s2g_d
!     character(len=8) names(4*nsig+2)
      character(len=64) names2(2,2*nsig+1)
      integer(i_kind) lnames2(2,2*nsig+1)
              integer(i_kind) nskip


!  create general_sub2grid structure variable s2g_raf, which is used in sub2grid.f90
!  NOTE:  the order of names and lnames corresponds to the order that the control + motley variables
!               are stored in the merge of control vector bundle with motley bundle.

      inner_vars=1
      n2d=size(cvars2d)
      n3d=size(cvars3d)
      num_fields=n2d+nsig*n3d+mvars
      vlevs=num_fields
      allocate(names_s2g_raf(inner_vars,num_fields),lnames_s2g_raf(inner_vars,num_fields))
      kk=0
      do k=1,n3d
         do l=1,nsig
            kk=kk+1
            names_s2g_raf(1,kk)=cvars3d(k)
            lnames_s2g_raf(1,kk)=l
         end do
      end do
      do k=1,n2d
         kk=kk+1
         names_s2g_raf(1,kk)=cvars2d(k)
         lnames_s2g_raf(1,kk)=1
      end do
      do k=1,mvars
         kk=kk+1
         names_s2g_raf(1,kk)=cvarsmd(k)
         lnames_s2g_raf(1,kk)=1
      end do

      call general_sub2grid_create_info(s2g_raf,inner_vars,nlat,nlon,nsig,num_fields,regional, &
             names=names_s2g_raf,lnames=lnames_s2g_raf)

!   set various constants previously defined in init_mpi_vars

      nsig1o=s2g_raf%nlevs_alloc
      nnnn1o=s2g_raf%nlevs_loc
      allocate(levs_id(nsig1o),nvar_id(nsig1o))
      allocate(nvar_pe(s2g_raf%num_fields,2))
      levs_id=0
      nvar_id=0
      nvar_pe=-999
      kk=0
      do k=s2g_raf%kbegin_loc,s2g_raf%kend_loc
         kk=kk+1
         levs_id(kk)=s2g_raf%lnames(1,k)
         nvar_id(kk)=getindex(nrf_var,trim(s2g_raf%names(1,k)))
      end do
      kk=0
      do n=1,npe
         do k=s2g_raf%kbegin(n-1),s2g_raf%kend(n-1)
            kk=kk+1
            nvar_pe(kk,1)=n-1
            nvar_pe(kk,2)=k-s2g_raf%kbegin(n-1)+1
         end do
      end do

!   set constants previously defined in init_commvars:

      ijn=s2g_raf%ijn
      ijn_s=s2g_raf%ijn_s
      irc_s=s2g_raf%irc_s
      isc_g=s2g_raf%isc_g
      allocate(ltosi(nlat*nlon),ltosj(nlat*nlon))
      ltosi=s2g_raf%ltosi
      ltosj=s2g_raf%ltosj
      isd_g=s2g_raf%isd_g
      displs_g=s2g_raf%displs_g
      ird_s=s2g_raf%ird_s
      displs_s=s2g_raf%displs_s
      itotsub=s2g_raf%itotsub
      allocate(ltosi_s(itotsub),ltosj_s(itotsub))
      ltosi_s=s2g_raf%ltosi_s
      ltosj_s=s2g_raf%ltosj_s

!  create general_sub2grid structure variable s2g_cv

      inner_vars=1
      num_fields=size(cvars2d)+nsig*size(cvars3d)

      call general_sub2grid_create_info(s2g_cv,inner_vars,nlat,nlon,nsig,num_fields,regional,s_ref=s2g_raf)

!  create general_sub2grid structure variable s2g_d, which is used in get_derivatives.f90

      inner_vars=1
      num_fields=size(cvars2d)+nsig*size(cvars3d)

!  obtain pointer to each variable in bundle, then populate corresponding names in names_s2g_d for
!     general_sub2grid_create_info.  this is needed for replacing nvar_id.
      allocate(names_s2g_d(inner_vars,num_fields),vector_s2g_d(num_fields))
!             bundlemod stores 3d fields first, followed by 2d fields, followed by 1d fields
      i=0
      do k=1,size(cvars3d)
         do j=1,nsig
            i=i+1
            names_s2g_d(1,i)=cvars3d(k)
            vector_s2g_d(i)=names_s2g_d(1,i) == 'sf'.or.names_s2g_d(1,i) == 'vp'
         end do
      end do
      do k=1,size(cvars2d)
         i=i+1
         names_s2g_d(1,i)=cvars2d(k)
         vector_s2g_d(i)=names_s2g_d(1,i) == 'sf'.or.names_s2g_d(1,i) == 'vp'
      end do
      call general_sub2grid_create_info(s2g_d,inner_vars,nlat,nlon,nsig,num_fields,regional, &
                                        vector=vector_s2g_d,names=names_s2g_d,s_ref=s2g_raf)
      deallocate(names_s2g_d,vector_s2g_d)

!  create general_sub2grid structure variable g1, which is used in get_derivatives.f90

      inner_vars=1
      num_fields=1
      n_one=1
      call general_sub2grid_create_info(g1,inner_vars,nlat,nlon,n_one,num_fields,regional,s_ref=s2g_raf)

!  create general_sub2grid structure variable g3, which is used in bkgcov_rewgt.f90

      inner_vars=1
      num_fields=nsig
      call general_sub2grid_create_info(g3,inner_vars,nlat,nlon,nsig,num_fields,regional,s_ref=s2g_raf)

!  create general_sub2grid structure variable g33p1

      inner_vars=1
      num_fields=3*nsig+1
      call general_sub2grid_create_info(g33p1,inner_vars,nlat,nlon,nsig,num_fields,regional,s_ref=s2g_raf)

!  create general_sub2grid structure variable s2g4, which is used in get_derivatives2.f90

      num_fields=2*nsig+1
      inner_vars=2
      kk=0
      do k=1,nsig
         kk=kk+1
         names2(1,kk)='sf'
         names2(2,kk)='vp'
         lnames2(1,kk)=k
         lnames2(2,kk)=k
      end do
      do k=1,nsig
         kk=kk+1
         names2(1,kk)='p3d'
         names2(2,kk)='t'
         lnames2(1,kk)=k
         lnames2(2,kk)=k
      end do
         kk=kk+1
         names2(1,kk)='p3d'
         names2(2,kk)='X'
         lnames2(1,kk)=k
         lnames2(2,kk)=0

      call general_sub2grid_create_info(s2g4,inner_vars,nlat,nlon,nsig,num_fields,regional, &
                                names=names2,lnames=lnames2,s_ref=s2g_raf)

!  create general_sub2grid structure variable s2g2, used in getprs.f90

      num_fields=nsig+1
      inner_vars=1
    ! nskip=2
      call general_sub2grid_create_info(s2g2,inner_vars,nlat,nlon,nsig,num_fields,regional,s_ref=s2g_raf)
    !                                   nskip=nskip)

!  create general_sub2grid structure variable s2guv

      num_fields=nsig
      inner_vars=2
      call general_sub2grid_create_info(s2guv,inner_vars,nlat,nlon,nsig,num_fields,regional,s_ref=s2g_raf)

   end subroutine init_general_commvars

   subroutine destroy_general_commvars
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    destroy_general_commvars
!     prgmmr:    parrish     org: np22                date: 2012-06-25
!
! abstract:  deallocate all pointer arrays in struture variables created in subroutine init_general_commvars
!
! program history log:
!   2012-06-25  parrish
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp
!
!$$$ end documentation block
 
       use general_sub2grid_mod, only: general_sub2grid_destroy_info
       use mpimod, only: levs_id,nvar_id,nvar_pe
       implicit none
 
       deallocate(levs_id,nvar_id,nvar_pe)
       call general_sub2grid_destroy_info(s2g_cv,s_ref=s2g_raf)
       call general_sub2grid_destroy_info(s2g2,s_ref=s2g_raf)
       call general_sub2grid_destroy_info(s2g4,s_ref=s2g_raf)
       call general_sub2grid_destroy_info(s2guv,s_ref=s2g_raf)
       call general_sub2grid_destroy_info(s2g_d,s_ref=s2g_raf)
       call general_sub2grid_destroy_info(g1,s_ref=s2g_raf)
       call general_sub2grid_destroy_info(g3,s_ref=s2g_raf)
       call general_sub2grid_destroy_info(g33p1,s_ref=s2g_raf)
       call general_sub2grid_destroy_info(s2g_raf)
 
    end subroutine destroy_general_commvars

end module general_commvars_mod
