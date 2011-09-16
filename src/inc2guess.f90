subroutine inc2guess(sval)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    inc2guess          replaces background with increment
!   prgmmr: todling          org: np22                date: 2007-07-05
!
! abstract:  This routine replaces the background fields with the
!            increments. Its main purpose is to be used in the 4d-var
!            case, though it could be considered for the 3d-var case 
!            as well.
!
!            As it is, this routine assumes a call to update_guess 
!            preceeds this and changes sval appropriately, including
!            change the scales of ozone and calculating vor and div.
!
! program history log:
!   2010-05-13  todling - update to use gsi_bundle
!   2010-06-01  todling - only upd when pointer defined
!   2010-06-15  todling - generalize handling of chemistry
!   2010-05-01  todling - add support for generalized guess (use met-guess)
!                       - cwmr now in met-guess
!   2011-06-29  todling - no explict reference to internal bundle arrays
!
!   input argument list:
!     sval     - analysis increment in grid space
!
!   output argument list:
!
!   comments:
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,i_kind
  use mpimod, only: mype
  use gridmod, only: lat2,lon2,nsig
  use guess_grids, only: ges_div,ges_vor,ges_ps,ges_tv,ges_q,&
       ges_oz,ges_u,ges_v,nfldsig,hrdifsig,&
       nfldsfc,sfct
  use state_vectors, only: svars3d,svars2d
  use gsi_4dvar, only: nobs_bins, hr_obsbin
  use xhat_vordivmod, only: xhat_vor,xhat_div
  use gsi_bundlemod, only: gsi_bundle
  use gsi_bundlemod, only: gsi_bundlegetpointer
  use gsi_metguess_mod, only: gsi_metguess_bundle
  use gsi_metguess_mod, only: gsi_metguess_get
  use gsi_chemguess_mod, only: gsi_chemguess_bundle
  use gsi_chemguess_mod, only: gsi_chemguess_get
  use mpeu_util, only: getindex

  implicit none

! Declare passed variables
  type(gsi_bundle), intent(in   ) :: sval(nobs_bins)

! Declare local variables
  character(len=10),allocatable,dimension(:) :: gases
  character(len=10),allocatable,dimension(:) :: guess
  integer(i_kind) i,j,k,it,ii,ic,id,ngases,nguess,istatus
  integer(i_kind) ipinc,ipges
  real(r_kind) :: zt
  real(r_kind),pointer,dimension(:,:  ) :: ptr2dinc=>NULL()
  real(r_kind),pointer,dimension(:,:  ) :: ptr2dges=>NULL()
  real(r_kind),pointer,dimension(:,:,:) :: ptr3dinc=>NULL()
  real(r_kind),pointer,dimension(:,:,:) :: ptr3dges=>NULL()

! Inquire about guess fields
call gsi_metguess_get('dim',nguess,istatus)
if (nguess>0) then  
    allocate(guess(nguess))
    call gsi_metguess_get('gsinames',guess,istatus)
endif

! Inquire about chemistry fields
call gsi_chemguess_get('dim',ngases,istatus)
if (ngases>0) then  
    allocate(gases(ngases))
    call gsi_chemguess_get('gsinames',gases,istatus)
endif

!*******************************************************************************

! Overwrite guess fields by increments
  do it=1,nfldsig
     if (nobs_bins>1) then
        zt = hrdifsig(it)
        ii = NINT(zt/hr_obsbin)+1
     else
        ii = 1
     endif
     call copyfld_ (ges_u,  'u' )
     call copyfld_ (ges_v,  'v' )
     call copyfld_ (ges_tv, 'tv')
     call copyfld2_(ges_q,  'q' )
     call copyfld_ (ges_oz, 'oz')
     do k=1,nsig
        do j=1,lon2
           do i=1,lat2
              ges_div(i,j,k,it)  = xhat_div(i,j,k,ii)
              ges_vor(i,j,k,it)  = xhat_vor(i,j,k,ii)
           end do
        end do
     end do
     call gsi_bundlegetpointer (sval(ii),'ps',ptr2dinc,istatus)
     if(istatus==0)then
        do j=1,lon2
           do i=1,lat2
              ges_ps(i,j,it) = ptr2dinc(i,j)
           end do
        end do
     end if

!    Update met-guess
     do ic=1,nguess
        id=getindex(svars3d,guess(ic))
        if (id>0) then
           call gsi_bundlegetpointer (sval(ii),               guess(ic),ptr3dinc,istatus)
           call gsi_bundlegetpointer (gsi_metguess_bundle(it),guess(ic),ptr3dges,istatus)
           ptr3dges = ptr3dinc
        endif
        id=getindex(svars2d,guess(ic))
        if (id>0) then
           call gsi_bundlegetpointer (sval(ii),               guess(ic),ptr2dinc,istatus)
           call gsi_bundlegetpointer (gsi_metguess_bundle(it),guess(ic),ptr2dges,istatus)
           ptr2dges = ptr2dinc
        endif
     enddo

!    Update trace gases
     do ic=1,ngases
        id=getindex(svars3d,gases(ic))
        if (id>0) then
           call gsi_bundlegetpointer (sval(ii),                gases(ic),ptr3dinc,istatus)
           call gsi_bundlegetpointer (gsi_chemguess_bundle(it),gases(ic),ptr3dges,istatus)
           ptr3dges = ptr3dinc
        endif
        id=getindex(svars2d,gases(ic))
        if (id>0) then
           call gsi_bundlegetpointer (sval(ii),                gases(ic),ptr2dinc,istatus)
           call gsi_bundlegetpointer (gsi_chemguess_bundle(it),gases(ic),ptr2dges,istatus)
           ptr2dges = ptr2dinc
        endif
     enddo
  end do

  if(ngases>0)then
    deallocate(gases)
  endif

  call gsi_bundlegetpointer (sval(ii),'sst',ptr2dinc,istatus)
  if(istatus==0)then
     do k=1,nfldsfc
        do j=1,lon2
           do i=1,lat2
              sfct(i,j,k)= ptr2dinc(i,j)
           end do
        end do
     end do
  end if
  if(mype==0) write(6,*) 'inc2guess: overwriting guess with increment'

  return
  contains
  subroutine copyfld_(fld,var)
  real(r_kind) :: fld(:,:,:,:)
  character(len=*) :: var
  integer(i_kind) istatus
  real(r_kind),pointer,dimension(:,:,:)::ptr3d=>NULL()
  call gsi_bundlegetpointer (sval(ii),var,ptr3d,istatus)
  if(istatus==0) return
     do k=1,nsig
        do j=1,lon2
           do i=1,lat2
              fld(i,j,k,it) = ptr3d(i,j,k)
           end do
        end do
     end do
  end subroutine copyfld_
  subroutine copyfld2_(fld,var)
  real(r_kind) :: fld(:,:,:,:)
  real(r_kind) :: ana
  character(len=*) :: var
  integer(i_kind) istatus
  real(r_kind),pointer,dimension(:,:,:)::ptr3d=>NULL()
  call gsi_bundlegetpointer (sval(ii),var,ptr3d,istatus)
  if(istatus==0) return
     do k=1,nsig
        do j=1,lon2
           do i=1,lat2
              ana = max(fld(i,j,k,it)+ ptr3d(i,j,k),1.e-10_r_kind)
              fld(i,j,k,it) = ana - fld(i,j,k,it)
           end do
        end do
     end do
  end subroutine copyfld2_
end subroutine inc2guess
