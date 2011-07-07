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
  integer(i_kind) i_u,i_v,i_t,i_q,i_oz,i_cw,i_ps,i_sst
  integer(i_kind) ipinc,ipges
  real(r_kind) :: zt

! Get pointers (enough to get for 1st instance)
  call gsi_bundlegetpointer(sval(1),'u',  i_u,  istatus)
  call gsi_bundlegetpointer(sval(1),'v',  i_v,  istatus)
  call gsi_bundlegetpointer(sval(1),'tv', i_t,  istatus)
  call gsi_bundlegetpointer(sval(1),'q',  i_q,  istatus)
  call gsi_bundlegetpointer(sval(1),'oz', i_oz, istatus)
  call gsi_bundlegetpointer(sval(1),'cw', i_cw, istatus)
  call gsi_bundlegetpointer(sval(1),'ps', i_ps, istatus)
  call gsi_bundlegetpointer(sval(1),'sst',i_sst,istatus)

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
     call copyfld_(ges_u,   i_u)
     call copyfld_(ges_v,   i_v)
     call copyfld_(ges_tv,  i_t)
     call copyfld2_(ges_q,   i_q)
     call copyfld_(ges_oz,  i_oz)
     do k=1,nsig
        do j=1,lon2
           do i=1,lat2
              ges_div(i,j,k,it)  = xhat_div(i,j,k,ii)
              ges_vor(i,j,k,it)  = xhat_vor(i,j,k,ii)
           end do
        end do
     end do
     if(i_ps>0)then
        do j=1,lon2
           do i=1,lat2
              ges_ps(i,j,it) = sval(ii)%r2(i_ps)%q(i,j)
           end do
        end do
     end if

!    Update met-guess
     do ic=1,nguess
        id=getindex(svars3d,guess(ic))
        if (id>0) then
           call gsi_bundlegetpointer (sval(ii),               guess(ic),ipinc,istatus)
           call gsi_bundlegetpointer (gsi_metguess_bundle(it),guess(ic),ipges,istatus)
           gsi_metguess_bundle(it)%r3(ipges)%q = sval(ii)%r3(ipinc)%q 
        endif
        id=getindex(svars2d,guess(ic))
        if (id>0) then
           call gsi_bundlegetpointer (sval(ii),               guess(ic),ipinc,istatus)
           call gsi_bundlegetpointer (gsi_metguess_bundle(it),guess(ic),ipges,istatus)
           gsi_metguess_bundle(it)%r2(ipges)%q = sval(ii)%r2(ipinc)%q 
        endif
     enddo

!    Update trace gases
     do ic=1,ngases
        id=getindex(svars3d,gases(ic))
        if (id>0) then
           call gsi_bundlegetpointer (sval(ii),                gases(ic),ipinc,istatus)
           call gsi_bundlegetpointer (gsi_chemguess_bundle(it),gases(ic),ipges,istatus)
           gsi_chemguess_bundle(it)%r3(ipges)%q = sval(ii)%r3(ipinc)%q 
        endif
        id=getindex(svars2d,gases(ic))
        if (id>0) then
           call gsi_bundlegetpointer (sval(ii),                gases(ic),ipinc,istatus)
           call gsi_bundlegetpointer (gsi_chemguess_bundle(it),gases(ic),ipges,istatus)
           gsi_chemguess_bundle(it)%r2(ipges)%q = sval(ii)%r2(ipinc)%q 
        endif
     enddo
  end do

  if(ngases>0)then
    deallocate(gases)
  endif

  if(i_sst>0)then
     do k=1,nfldsfc
        do j=1,lon2
           do i=1,lat2
              sfct(i,j,k)= sval(ii)%r2(i_sst)%q(i,j)
           end do
        end do
     end do
  end if
  if(mype==0) write(6,*) 'inc2guess: overwriting guess with increment'

  return
  contains
  subroutine copyfld_(fld,ipnt)
  real(r_kind) :: fld(:,:,:,:)
  integer(i_kind) :: ipnt
  if(ipnt<0) return
     do k=1,nsig
        do j=1,lon2
           do i=1,lat2
              fld(i,j,k,it)    = sval(ii)%r3(ipnt)%q(i,j,k)
           end do
        end do
     end do
  end subroutine copyfld_
  subroutine copyfld2_(fld,ipnt)
  real(r_kind) :: fld(:,:,:,:)
  real(r_kind) :: ana
  integer(i_kind) :: ipnt
  if(ipnt<0) return
     do k=1,nsig
        do j=1,lon2
           do i=1,lat2
              ana = max(fld(i,j,k,it)+ sval(ii)%r3(ipnt)%q(i,j,k),1.e-10_r_kind)
              fld(i,j,k,it) = ana - fld(i,j,k,it)
           end do
        end do
     end do
  end subroutine copyfld2_
end subroutine inc2guess
