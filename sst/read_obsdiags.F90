subroutine read_obsdiags(cdfile)

!$$$  subprogram documentation block
!
! abstract: Read obsdiags data structure from file.
!
! program history log:
!   2007-07-05  tremolet
!   2007-08-04  todling  - using get_lun to determine file unit number
!   2007-10-03  todling  - expanded to account for full observer 
!   2009-01-08  todling  - remove reference to ozohead
!   2009-01-23  todling  - add read_gpshead
!
!   input argument list:
!     cdfile - filename to read data from
!
! remarks: ozhead still cannot handle omi data
!
!$$$

use kinds, only: r_kind,i_kind
use constants, only: tiny_r_kind
use obsmod, only: nobs_type,obsdiags,obsptr,lobsdiag_allocated,lobserver
use obsmod, only: destroyobs
use obsmod, only: i_ps_ob_type, i_t_ob_type, i_w_ob_type, i_q_ob_type, &
                  i_spd_ob_type, i_srw_ob_type, i_rw_ob_type, i_dw_ob_type, &
                  i_sst_ob_type, i_pw_ob_type, i_pcp_ob_type, i_oz_ob_type, &
                  i_o3l_ob_type, i_gps_ob_type, i_rad_ob_type

use obs_sensitivity, only: lobsensfc, lsensrecompute
use gsi_4dvar, only: l4dvar, nobs_bins
use mpimod, only: mype
use constants, only: zero
use jfunc, only: jiter, miter
use file_utility, only : get_lun

implicit none
character(len=*), intent(in) :: cdfile

character(len=100) :: clfile
character(len=5) :: clmype
integer(i_kind) :: iunit,ii,jj,ki,kj,kobs,kiter,kindx,kk,mchanl,ierr
logical :: lluse, lmuse(1:miter), gogetit, root
real(r_kind) :: znldepart(1:miter), ztldepart(1:miter), zwgtjo, zobssen(1:miter)
! ----------------------------------------------------------

iunit=get_lun()
clmype='.YYYY'
write(clmype(2:5),'(I4.4)')mype
clfile=trim(cdfile)//clmype
if (mype==0) write(6,*)'Start reading obsdiags from file ',clfile
root = mype==0
gogetit = .true.
if(lobserver .and. jiter==1) gogetit = .false.

open(iunit,file=trim(clfile),form='unformatted',action='read',iostat=ierr)
if (ierr/=0) call abor1('read_obsdiags: error open')

do ii=1,nobs_bins
  do jj=1,nobs_type

    read(iunit)ki,kj,kobs,kiter
    if (ki/=ii) call abor1('read_obsdiags: error ii')
    if (kj/=jj) call abor1('read_obsdiags: error jj')
    if (lobsensfc.and..not.lsensrecompute) then
      if (kiter/=miter) call abor1('read_obsdiags: error kiter')
    else
      if (lobserver) then
        if (kiter/=jiter-1) call abor1('read_obsdiags: error kiter')
      else
        if (kiter/=jiter)   call abor1('read_obsdiags: error kiter')
      endif
    endif

    do kk=1,kobs
      if (.not.associated(obsdiags(jj,ii)%head)) then
        allocate(obsdiags(jj,ii)%head,stat=ierr)
        if (ierr/=0) call abor1('setupt: fail to allocate obsdiags')
        obsdiags(jj,ii)%tail => obsdiags(jj,ii)%head
      else
        allocate(obsdiags(jj,ii)%tail%next,stat=ierr)
        if (ierr/=0) call abor1('setupt: fail to allocate next obsdiags')
        obsdiags(jj,ii)%tail => obsdiags(jj,ii)%tail%next
      end if
      allocate(obsdiags(jj,ii)%tail%muse(miter+1))
      allocate(obsdiags(jj,ii)%tail%nldepart(miter+1))
      allocate(obsdiags(jj,ii)%tail%tldepart(miter))
      allocate(obsdiags(jj,ii)%tail%obssen(miter))
      obsdiags(jj,ii)%tail%indxglb=-99999
      obsdiags(jj,ii)%tail%nchnperobs=-99999
      obsdiags(jj,ii)%tail%luse=.false.
      obsdiags(jj,ii)%tail%muse(:)=.false.
      obsdiags(jj,ii)%tail%nldepart(:)=-huge(zero)
      obsdiags(jj,ii)%tail%tldepart(:)=zero
      obsdiags(jj,ii)%tail%wgtjo=-huge(zero)
      obsdiags(jj,ii)%tail%obssen(:)=zero

      read(iunit) kindx, mchanl, lluse, lmuse(1:kiter), &
                  znldepart(1:kiter), ztldepart(1:kiter), &
                  zwgtjo, zobssen(1:kiter)

      obsdiags(jj,ii)%tail%indxglb=kindx
      obsdiags(jj,ii)%tail%nchnperobs=mchanl
      obsdiags(jj,ii)%tail%luse  = lluse
      obsdiags(jj,ii)%tail%wgtjo = zwgtjo
      obsdiags(jj,ii)%tail%muse(1:kiter)     = lmuse(1:kiter)
      obsdiags(jj,ii)%tail%nldepart(1:kiter) = znldepart(1:kiter)
      obsdiags(jj,ii)%tail%tldepart(1:kiter) = ztldepart(1:kiter)
      if (lobsensfc.and..not.lsensrecompute) then
        obsdiags(jj,ii)%tail%obssen(jiter+1:miter)=zobssen(jiter+1:miter)
      else
        if (lobserver) then
          obsdiags(jj,ii)%tail%obssen(1:jiter-1)=zobssen(1:jiter-1)
        else
          obsdiags(jj,ii)%tail%obssen(1:miter)=zobssen(1:miter)
        endif
      endif
    enddo  ! < kobs >

    if (l4dvar.and.gogetit) then
      if(jj==i_ps_ob_type)  call read_pshead_  ()
      if(jj==i_t_ob_type)   call read_thead_   ()
      if(jj==i_w_ob_type)   call read_whead_   ()
      if(jj==i_q_ob_type)   call read_qhead_   ()
      if(jj==i_spd_ob_type) call read_spdhead_ ()
      if(jj==i_srw_ob_type) call read_srwhead_ ()
      if(jj==i_rw_ob_type)  call read_rwhead_  ()
      if(jj==i_dw_ob_type)  call read_dwhead_  ()
      if(jj==i_sst_ob_type) call read_ssthead_ ()
      if(jj==i_pw_ob_type)  call read_pwhead_  ()
      if(jj==i_oz_ob_type)  call read_ozhead_  ()
      if(jj==i_o3l_ob_type) call read_o3lhead_ ()
      if(jj==i_pcp_ob_type) call read_pcphead_ ()
      if(jj==i_gps_ob_type) call read_gpshead_ ()
      if(jj==i_rad_ob_type) call read_radhead_ ()
    endif

    read(iunit)ki,kj
    if (ki/=ii) call abor1('read_obsdiags: error ii')
    if (kj/=jj) call abor1('read_obsdiags: error jj')
  enddo
enddo

close(iunit)
if(lobserver) call destroyobs ( skipit=.true. )
lobsdiag_allocated=.true.
if (mype==0) write(6,*)'Finish reading obsdiags from file ',clfile

! ----------------------------------------------------------
return

contains

subroutine read_pshead_ ()
!$$$  subprogram documentation block
!
! abstract: Read obs-specific data structure from file.
!
! program history log:
!   2007-10-03  todling
!   2008-12-08  todling - update to May08 version
!
!   input argument list:
!
!$$$

    use obsmod, only: pshead,pstail
    implicit none

    real(r_kind)    :: zres           !  residual
    real(r_kind)    :: zerr2          !  error squared
    real(r_kind)    :: zraterr2       !  square of ratio of final obs error
                                      !  to original obs error
    real(r_kind)    :: ztime          !  observation time
    real(r_kind)    :: zb             !  variational quality control parameter
    real(r_kind)    :: zpg            !  variational quality control parameter
    real(r_kind)    :: zppertb        !  random number added to obs
    real(r_kind)    :: zwij(4)        !  horizontal interpolation weights
    integer(i_kind) :: zij(4)         !  horizontal locations
    integer(i_kind) :: zkx            !  observation type
    logical         :: zluse          !  flag indicating if ob is used in pen.

    integer(i_kind) :: j,mobs,jread,icount,iostat
    logical         :: mymuse   

!   Read in obs-specific entries
!   ----------------------------   
    read(iunit) mobs,jread
    if(jj/=jread) call abor1('read_pshead_: unmatched ob type')
    if(kobs<=0.or.mobs<=0) return

    do kk=1,mobs
       if(.not. associated(pshead(ii)%head))then
          allocate(pshead(ii)%head,stat=ierr)
          if(ierr /= 0)write(6,*)' fail to alloc pshead '
          pstail(ii)%head => pshead(ii)%head
       else
          allocate(pstail(ii)%head%llpoint,stat=ierr)
          if(ierr /= 0)write(6,*)' fail to alloc pstail%llpoint '
          pstail(ii)%head => pstail(ii)%head%llpoint
       end if
       read(iunit,iostat=iostat) zres,  zerr2,    zraterr2,&
                                 ztime, zb,       zpg, &
                                 zluse, zppertb,  zkx, zwij, zij
         if (iostat/=0) call abor1('read_pshead_: error reading record')
       pstail(ii)%head%res      = zres
       pstail(ii)%head%err2     = zerr2
       pstail(ii)%head%raterr2  = zraterr2
       pstail(ii)%head%time     = ztime
       pstail(ii)%head%b        = zb
       pstail(ii)%head%pg       = zpg
       pstail(ii)%head%wij      = zwij
       pstail(ii)%head%ij       = zij
       pstail(ii)%head%luse     = zluse
       pstail(ii)%head%ppertb   = zppertb
       pstail(ii)%head%kx       = zkx
    enddo
    if(lobserver) return

!   Now set obsdiag pointer properly
!   --------------------------------
    icount=0
    pstail(ii)%head => NULL()
!   do j=1,kiter
       j=kiter
       obsdiags(jj,ii)%tail => NULL()
       do kk=1,kobs
          if (.not.associated(obsdiags(jj,ii)%tail)) then
            obsdiags(jj,ii)%tail => obsdiags(jj,ii)%head
          else
            obsdiags(jj,ii)%tail => obsdiags(jj,ii)%tail%next
          endif
          mymuse = obsdiags(jj,ii)%tail%muse(j)
          if ( mymuse ) then
            if(.not. associated(pstail(ii)%head))then
               pstail(ii)%head => pshead(ii)%head
            else
               pstail(ii)%head => pstail(ii)%head%llpoint
            end if
            pstail(ii)%head%diags    => obsdiags(jj,ii)%tail
            icount = icount + 1
          endif
       enddo
!   enddo
!   if(mobs>0) print *, 'Read ps from obsdiag, ii =', ii, ' mobs =', mobs, ', pe ', mype
    if(icount.ne.mobs) call abor1('read_pshead_: error counting ob')
end subroutine read_pshead_

subroutine read_thead_ ()
!$$$  subprogram documentation block
!
! abstract: Read obs-specific data structure from file.
!
! program history log:
!   2007-10-03  todling
!   2008-12-08  todling - update to May08 version
!
!   input argument list:
!
!$$$

    use obsmod, only: thead,ttail
    implicit none

    real(r_kind)    :: zres           !  residual
    real(r_kind)    :: zerr2          !  error squared
    real(r_kind)    :: zraterr2       !  square of ratio of final obs error
                                      !  to original obs error
    real(r_kind)    :: ztime          !  observation time
    real(r_kind)    :: zb             !  variational quality control parameter
    real(r_kind)    :: zpg            !  variational quality control parameter
    real(r_kind)    :: ztlm_tsfc(6)   !  sensitivity vector for sfc temp
                                      !  forward model
    real(r_kind)    :: zwij(8)        !  horizontal interpolation weights
    real(r_kind)    :: ztpertb        !  random number added to the obs
    integer(i_kind) :: zij(8)         !  horizontal locations
    logical         :: ztv_ob         !  logical flag for virtual temperature or
    integer(i_kind) :: zk1            !  level of errtable 1-33
    integer(i_kind) :: zkx            !  ob type
    logical         :: zluse          !  flag indicating if ob is used in pen.
    logical         :: zuse_sfc_model !  logical flag for using boundary model

    integer         :: j,mobs,jread,icount,iostat
    logical         :: mymuse   

    read(iunit) mobs,jread
    if(jj/=jread) call abor1('read_thead_: unmatched ob type')
    if(kobs<=0.or.mobs<=0) return
    do kk=1,mobs
         if(.not. associated(thead(ii)%head))then
            allocate(thead(ii)%head,stat=ierr)
            if(ierr /= 0)write(6,*)' fail to alloc thead '
            ttail(ii)%head => thead(ii)%head
         else
            allocate(ttail(ii)%head%llpoint,stat=ierr)
            if(ierr /= 0)write(6,*)' fail to alloc ttail%llpoint '
            ttail(ii)%head => ttail(ii)%head%llpoint
         end if
         read(iunit,iostat=iostat) zres,  zerr2,    zraterr2,&
                                   ztime, zb,       zpg, &
                                   zuse_sfc_model,  ztlm_tsfc, &
                                   zluse, ztpertb,  ztv_ob, &
                                   zk1,   zkx,      zwij, zij
         if (iostat/=0) call abor1('read_thead_: error reading record')
         ttail(ii)%head%res      = zres
         ttail(ii)%head%err2     = zerr2
         ttail(ii)%head%raterr2  = zraterr2
         ttail(ii)%head%time     = ztime
         ttail(ii)%head%b        = zb
         ttail(ii)%head%pg       = zpg
         ttail(ii)%head%tlm_tsfc = ztlm_tsfc
         ttail(ii)%head%tpertb   = ztpertb
         ttail(ii)%head%tv_ob    = ztv_ob
         ttail(ii)%head%k1       = zk1
         ttail(ii)%head%kx       = zkx
         ttail(ii)%head%luse     = zluse
         ttail(ii)%head%wij      = zwij
         ttail(ii)%head%ij       = zij
         ttail(ii)%head%use_sfc_model = zuse_sfc_model  
    enddo
    if(lobserver) return

!   Now set obsdiag pointer properly
!   --------------------------------
    icount=0
    ttail(ii)%head => NULL()
!   do j=1,kiter
       j=kiter
    obsdiags(jj,ii)%tail => NULL()
    do kk=1,kobs
       if (.not.associated(obsdiags(jj,ii)%tail)) then
         obsdiags(jj,ii)%tail => obsdiags(jj,ii)%head
       else
         obsdiags(jj,ii)%tail => obsdiags(jj,ii)%tail%next
       endif
       mymuse = obsdiags(jj,ii)%tail%muse(j)
       if ( mymuse ) then
         if(.not. associated(ttail(ii)%head))then
            ttail(ii)%head => thead(ii)%head
         else
            ttail(ii)%head => ttail(ii)%head%llpoint
         end if
         ttail(ii)%head%diags    => obsdiags(jj,ii)%tail
         icount = icount + 1
       endif      
    enddo
!   enddo

!   if(mobs>0) print *, 'Read t from obsdiag, ii =', ii, ' mobs =', mobs, ', pe ', mype
    if(icount.ne.mobs) call abor1('read_thead_: error counting ob')
end subroutine read_thead_

subroutine read_whead_ ()
!$$$  subprogram documentation block
!
! abstract: Read obs-specific data structure from file.
!
! program history log:
!   2007-10-03  todling
!   2008-12-08  todling - update to May08 version
!
!   input argument list:
!
!$$$

    use obsmod, only: whead,wtail
    implicit none

    real(r_kind)    :: zures          !  zonal wind residual
    real(r_kind)    :: zvres          !  meridional wind residual
    real(r_kind)    :: zerr2          !  temperature error squared
    real(r_kind)    :: zraterr2       !  square of ratio of final obs error
                                      !  to original obs error
    real(r_kind)    :: ztime          !  observation time
    real(r_kind)    :: zb             !  variational quality control parameter
    real(r_kind)    :: zpg            !  variational quality control parameter
    real(r_kind)    :: zupertb        !  random number added to the obs
    real(r_kind)    :: zvpertb        !  random number added to the obs
    real(r_kind)    :: zwij(8)        !  horizontal interpolation weights
    integer(i_kind) :: zij(8)         !  horizontal locations
    integer(i_kind) :: zk1            !  level of errtable 1-33
    integer(i_kind) :: zkx            !  ob type
    logical         :: zluse          !  flag indicating if ob is used in pen.

    integer         :: j,mm,mobs,jread,icount,iostat
    logical         :: mymuse
   
    read(iunit) mobs,jread
    if(jj/=jread) call abor1('read_whead_: unmatched ob type')
    if(kobs<=0.or.mobs<=0) return

    do kk=1,mobs
         if(.not. associated(whead(ii)%head))then
            allocate(whead(ii)%head,stat=ierr)
            if(ierr /= 0)write(6,*)' fail to alloc whead '
            wtail(ii)%head => whead(ii)%head
         else
            allocate(wtail(ii)%head%llpoint,stat=ierr)
            if(ierr /= 0)write(6,*)' fail to alloc wtail%llpoint '
            wtail(ii)%head => wtail(ii)%head%llpoint
         end if
         read(iunit,iostat=iostat) zures, zvres, zerr2, zraterr2,&
                                   ztime, zb,    zpg, &
                                   zluse, zupertb, zvpertb, &
                                   zk1,   zkx,   zwij,  zij
         if (iostat/=0) call abor1('read_whead_: error reading record')
         wtail(ii)%head%ij       = zij
         wtail(ii)%head%wij      = zwij
         wtail(ii)%head%ures     = zures
         wtail(ii)%head%vres     = zvres
         wtail(ii)%head%err2     = zerr2
         wtail(ii)%head%raterr2  = zraterr2
         wtail(ii)%head%time     = ztime
         wtail(ii)%head%b        = zb
         wtail(ii)%head%pg       = zpg
         wtail(ii)%head%upertb   = zupertb
         wtail(ii)%head%vpertb   = zvpertb
         wtail(ii)%head%k1       = zk1
         wtail(ii)%head%kx       = zkx
         wtail(ii)%head%luse     = zluse
    enddo
    if(lobserver) return

!   Now set obsdiag pointer properly
!   --------------------------------
    icount=0
    wtail(ii)%head => NULL()
!   do j=1,kiter
       j=kiter
    obsdiags(jj,ii)%tail => NULL()
    do kk=1,kobs/2

       do mm=1,2
         if (.not.associated(obsdiags(jj,ii)%tail)) then
           obsdiags(jj,ii)%tail => obsdiags(jj,ii)%head
         else
           obsdiags(jj,ii)%tail => obsdiags(jj,ii)%tail%next
         end if
         if (mm==1) obsptr => obsdiags(jj,ii)%tail
       enddo

       mymuse =  obsdiags(jj,ii)%tail%muse(j)
       if ( mymuse ) then
         if(.not. associated(wtail(ii)%head))then
            wtail(ii)%head => whead(ii)%head
         else
            wtail(ii)%head => wtail(ii)%head%llpoint
         end if

         wtail(ii)%head%diagu    => obsptr
         wtail(ii)%head%diagv    => obsdiags(jj,ii)%tail
         icount = icount + 1
       endif

    enddo
!   enddo

    if(icount.ne.mobs) call abor1('read_whead_: error counting ob')
!   if(mobs>0) print *, 'Read w from obsdiag, ii =', ii, ' mobs =', mobs, ', pe ', mype
end subroutine read_whead_

subroutine read_qhead_ ()
!$$$  subprogram documentation block
!
! abstract: Read obs-specific data structure from file.
!
! program history log:
!   2007-10-03  todling
!   2008-12-08  todling - update to May08 version
!
!   input argument list:
!
!$$$

    use obsmod, only: qhead,qtail
    implicit none

    real(r_kind)    :: zres           !  residual
    real(r_kind)    :: zerr2          !  error squared
    real(r_kind)    :: zraterr2       !  square of ratio of final obs error
                                      !  to original obs error
    real(r_kind)    :: ztime          !  observation time
    real(r_kind)    :: zb             !  variational quality control parameter
    real(r_kind)    :: zpg            !  variational quality control parameter
    real(r_kind)    :: zqpertb        !  random number added to the obs
    real(r_kind)    :: zwij(8)        !  horizontal interpolation weights
    integer(i_kind) :: zij(8)         !  horizontal locations
    integer(i_kind) :: zk1            !  level of errtable 1-33
    integer(i_kind) :: zkx            !  ob type
    logical         :: zluse          !  flag indicating if ob is used in pen.

    integer         :: j,mobs,jread,icount,iostat
    logical         :: mymuse   
   
    read(iunit) mobs,jread
    if(jj/=jread) call abor1('read_qhead_: unmatched ob type')
    if(kobs<=0.or.mobs<=0) return

    do kk=1,mobs

         if(.not. associated(qhead(ii)%head))then
            allocate(qhead(ii)%head,stat=ierr)
            if(ierr /= 0)write(6,*)' fail to alloc qhead '
            qtail(ii)%head => qhead(ii)%head
         else
            allocate(qtail(ii)%head%llpoint,stat=ierr)
            if(ierr /= 0)write(6,*)' fail to alloc qtail%llpoint '
            qtail(ii)%head => qtail(ii)%head%llpoint
         end if
         read(iunit,iostat=iostat) zres,  zerr2,   zraterr2,&
                                   ztime, zb,      zpg, &
                                   zluse, zqpertb, zk1, zkx, &
                                   zwij, zij
         if(iostat/=0) call abor1('read_qhead_: error reading record')
         qtail(ii)%head%ij       = zij
         qtail(ii)%head%wij      = zwij
         qtail(ii)%head%res      = zres
         qtail(ii)%head%err2     = zerr2
         qtail(ii)%head%raterr2  = zraterr2
         qtail(ii)%head%time     = ztime
         qtail(ii)%head%b        = zb
         qtail(ii)%head%pg       = zpg
         qtail(ii)%head%qpertb   = zqpertb
         qtail(ii)%head%k1       = zk1
         qtail(ii)%head%kx       = zkx
         qtail(ii)%head%luse     = zluse
       
    enddo
    if(lobserver) return

!   Now set obsdiag pointer properly
!   --------------------------------
    icount=0
    qtail(ii)%head => NULL()
!   do j=1,kiter
       j=kiter
    obsdiags(jj,ii)%tail => NULL()
    do kk=1,kobs

       if (.not.associated(obsdiags(jj,ii)%tail)) then
         obsdiags(jj,ii)%tail => obsdiags(jj,ii)%head
       else
         obsdiags(jj,ii)%tail => obsdiags(jj,ii)%tail%next
       endif

       mymuse = obsdiags(jj,ii)%tail%muse(j)
       if ( mymuse ) then
         if(.not. associated(qtail(ii)%head))then
            qtail(ii)%head => qhead(ii)%head
         else
            qtail(ii)%head => qtail(ii)%head%llpoint
         end if
         qtail(ii)%head%diags    => obsdiags(jj,ii)%tail

         icount = icount + 1
       endif
       
    enddo
!   enddo

    if(icount.ne.mobs) call abor1('read_qhead_: error counting ob')
!   if(mobs>0) print *, 'Read q from obsdiag, ii =', ii, ' mobs =', mobs, ', pe ', mype
end subroutine read_qhead_

subroutine read_spdhead_ ()
!$$$  subprogram documentation block
!
! abstract: Read obs-specific data structure from file.
!
! program history log:
!   2007-10-03  todling
!
!   input argument list:
!
!$$$

    use obsmod, only: spdhead,spdtail
    implicit none

    real(r_kind)    :: zres           !  residual
    real(r_kind)    :: zerr2          !  error squared
    real(r_kind)    :: zraterr2       !  square of ratio of final obs error
                                      !  to original obs error
    real(r_kind)    :: ztime          !  observation time
    real(r_kind)    :: zb             !  variational quality control parameter
    real(r_kind)    :: zpg            !  variational quality control parameter
    real(r_kind)    :: zwij(4)        !  horizontal interpolation weights
    integer(i_kind) :: zij(4)         !  horizontal locations
    real(r_kind)    :: zuges          !  zonal wind guess
    real(r_kind)    :: zvges          !  meridional guess
    logical         :: zluse          !  flag indicating if ob is used in pen.

    integer         :: j,mobs,jread,icount,iostat
    logical         :: mymuse   
   
    read(iunit) mobs,jread
    if(jj/=jread) call abor1('read_spdhead_: unmatched ob type')
    if(kobs<=0.or.mobs<=0) return

    do kk=1,mobs

         if(.not. associated(spdhead(ii)%head))then
            allocate(spdhead(ii)%head,stat=ierr)
            if(ierr /= 0)write(6,*)' fail to alloc spdhead '
            spdtail(ii)%head => spdhead(ii)%head
         else
            allocate(spdtail(ii)%head%llpoint,stat=ierr)
            if(ierr /= 0)write(6,*)' fail to alloc spdtail%llpoint '
            spdtail(ii)%head => spdtail(ii)%head%llpoint
         end if
         read(iunit,iostat=iostat) zres,  zerr2,    zraterr2,&
                                   ztime, zb,       zpg, &
                                   zuges, zvges, &
                                   zluse, zwij, zij
         if (iostat/=0) call abor1('read_spdhead_: error reading record')
         spdtail(ii)%head%ij       = zij
         spdtail(ii)%head%wij      = zwij
         spdtail(ii)%head%res      = zres
         spdtail(ii)%head%err2     = zerr2
         spdtail(ii)%head%raterr2  = zraterr2
         spdtail(ii)%head%time     = ztime
         spdtail(ii)%head%b        = zb
         spdtail(ii)%head%pg       = zpg
         spdtail(ii)%head%luse     = zluse
         spdtail(ii)%head%uges     = zuges
         spdtail(ii)%head%vges     = zvges
    
    enddo
    if(lobserver) return

!   Now set obsdiag pointer properly
!   --------------------------------
    icount=0
    spdtail(ii)%head => NULL()
!   do j=1,kiter
       j=kiter
    obsdiags(jj,ii)%tail => NULL()
    do kk=1,kobs
       if (.not.associated(obsdiags(jj,ii)%tail)) then
         obsdiags(jj,ii)%tail => obsdiags(jj,ii)%head
       else
         obsdiags(jj,ii)%tail => obsdiags(jj,ii)%tail%next
       endif
       mymuse = obsdiags(jj,ii)%tail%muse(j)
       if ( mymuse ) then
         if(.not. associated(spdtail(ii)%head))then
            spdtail(ii)%head => spdhead(ii)%head
         else
            spdtail(ii)%head => spdtail(ii)%head%llpoint
         end if
         spdtail(ii)%head%diags    => obsdiags(jj,ii)%tail
         icount = icount + 1
       endif      
    enddo
!   enddo

    if(icount.ne.mobs) call abor1('read_spdhead_: error counting ob')
!   if(mobs>0) print *, 'Read spd from obsdiag, ii =', ii, ' mobs =', mobs, ', pe ', mype
end subroutine read_spdhead_

subroutine read_srwhead_ ()
!$$$  subprogram documentation block
!
! abstract: Read obs-specific data structure from file.
!
! program history log:
!   2007-10-03  todling
!
!   input argument list:
!
!$$$

    use obsmod, only: srwhead,srwtail
    implicit none

    real(r_kind)    :: zres1          !  residual
    real(r_kind)    :: zres2          !  residual
    real(r_kind)    :: zerr2          !  error squared
    real(r_kind)    :: zraterr2       !  square of ratio of final obs error
                                      !  to original obs error
    real(r_kind)    :: ztime          !  observation time
    real(r_kind)    :: zb             !  variational quality control parameter
    real(r_kind)    :: zpg            !  variational quality control parameter
    real(r_kind)    :: zrsrw(4)       !  forward model for radar superob wind
    real(r_kind)    :: zwij(8)        !  horizontal interpolation weights
    integer(i_kind) :: zij(8)         !  horizontal locations
    real(r_kind)    :: zges1          !  first component guess
    real(r_kind)    :: zges2          !  second component guess
    logical         :: zluse          !  flag indicating if ob is used in pen.

    integer         :: j,mm,mobs,jread,icount,iostat
    logical         :: mymuse   
   
    icount=0
    read(iunit) mobs,jread
    if(jj/=jread) call abor1('read_srwhead_: unmatched ob type')
    if(kobs<=0.or.mobs<=0) return

    do kk=1,mobs

          if(.not. associated(srwhead(ii)%head))then
             allocate(srwhead(ii)%head,stat=ierr)
             if(ierr /= 0)write(6,*)' fail to alloc srwhead '
             srwtail(ii)%head => srwhead(ii)%head
          else
             allocate(srwtail(ii)%head%llpoint,stat=ierr)
             if(ierr /= 0)write(6,*)' fail to alloc srwtail%llpoint '
             srwtail(ii)%head => srwtail(ii)%head%llpoint
          end if
          read(iunit,iostat=iostat) zres1, zres2, zerr2, zraterr2,&
                                    ztime, zb,    zpg, &
                                    zges1, zges2, &
                                    zluse, zrsrw, zwij, zij
          if (iostat/=0) call abor1('read_srwhead_: error reading record')
          srwtail(ii)%head%res1     = zres1
          srwtail(ii)%head%res2     = zres2
          srwtail(ii)%head%err2     = zerr2
          srwtail(ii)%head%raterr2  = zraterr2
          srwtail(ii)%head%time     = ztime
          srwtail(ii)%head%b        = zb
          srwtail(ii)%head%pg       = zpg
          srwtail(ii)%head%luse     = zluse
          srwtail(ii)%head%ges1     = zges1
          srwtail(ii)%head%ges1     = zges2
          srwtail(ii)%head%rsrw     = zrsrw
          srwtail(ii)%head%wij      = zwij
          srwtail(ii)%head%ij       = zij

    enddo
    if(lobserver) return

!   Now set obsdiag pointer properly
!   --------------------------------
    icount=0
    srwtail(ii)%head => NULL()
!   do j=1,kiter
       j=kiter
    obsdiags(jj,ii)%tail => NULL()
    do kk=1,kobs/2

       do mm=1,2
         if (.not.associated(obsdiags(jj,ii)%tail)) then
           obsdiags(jj,ii)%tail => obsdiags(jj,ii)%head
         else
           obsdiags(jj,ii)%tail => obsdiags(jj,ii)%tail%next
         end if
         if (mm==1) obsptr => obsdiags(jj,ii)%tail
       enddo
      
       mymuse = obsdiags(jj,ii)%tail%muse(j)
       if ( mymuse ) then
          if(.not. associated(srwtail(ii)%head))then
             srwtail(ii)%head => srwhead(ii)%head
          else
             srwtail(ii)%head => srwtail(ii)%head%llpoint
          end if
          srwtail(ii)%head%diagu    => obsptr
          srwtail(ii)%head%diagv    => obsdiags(jj,ii)%tail
          icount = icount + 1
       endif

    enddo
!   enddo

    if(icount.ne.mobs) call abor1('read_srwhead_: error counting ob')
!   if(mobs>0) print *, 'Read srw from obsdiag, ii =', ii, ' mobs =', mobs, ', pe ', mype
end subroutine read_srwhead_

subroutine read_rwhead_ ()
!$$$  subprogram documentation block
!
! abstract: Read obs-specific data structure from file.
!
! program history log:
!   2007-10-03  todling
!
!   input argument list:
!
!$$$

    use obsmod, only: rwhead,rwtail
    implicit none

    real(r_kind)    :: zres           !  residual
    real(r_kind)    :: zerr2          !  error squared
    real(r_kind)    :: zraterr2       !  square of ratio of final obs error
                                      !  to original obs error
    real(r_kind)    :: ztime          !  observation time
    real(r_kind)    :: zb             !  variational quality control parameter
    real(r_kind)    :: zpg            !  variational quality control parameter
    real(r_kind)    :: zcosazm        !  zonal wind factor
    real(r_kind)    :: zsinazm        !  meridional factor
    real(r_kind)    :: zwij(8)        !  horizontal interpolation weights
    integer(i_kind) :: zij(8)         !  horizontal locations
    logical         :: zluse          !  flag indicating if ob is used in pen.

    integer         :: j,mobs,jread,icount,iostat
    logical         :: mymuse   
   
    read(iunit) mobs,jread
    if(jj/=jread) call abor1('read_rwhead_: unmatched ob type')
    if(kobs<=0.or.mobs<=0) return

    do kk=1,mobs
       if(.not. associated(rwhead(ii)%head))then
          allocate(rwhead(ii)%head,stat=ierr)
          if(ierr /= 0)write(6,*)' fail to alloc rwhead '
          rwtail(ii)%head => rwhead(ii)%head
       else
          allocate(rwtail(ii)%head%llpoint,stat=ierr)
          if(ierr /= 0)write(6,*)' fail to alloc rwtail%llpoint '
          rwtail(ii)%head => rwtail(ii)%head%llpoint
       end if
       read(iunit,iostat=iostat) zres,  zerr2, zraterr2,&
                                 ztime, zb,   zpg, &
                                 zcosazm,     zsinazm, &
                                 zluse, zwij, zij
       if (iostat/=0) call abor1('read_rwhead_: error reading record')
       rwtail(ii)%head%res      = zres
       rwtail(ii)%head%err2     = zerr2
       rwtail(ii)%head%raterr2  = zraterr2
       rwtail(ii)%head%time     = ztime
       rwtail(ii)%head%b        = zb
       rwtail(ii)%head%pg       = zpg
       rwtail(ii)%head%cosazm   = zcosazm
       rwtail(ii)%head%sinazm   = zsinazm
       rwtail(ii)%head%wij      = zwij
       rwtail(ii)%head%ij       = zij
       rwtail(ii)%head%luse     = zluse
    enddo
    if(lobserver) return

!   Now set obsdiag pointer properly
!   --------------------------------
    icount=0
    rwtail(ii)%head => NULL()
!   do j=1,kiter
       j=kiter
    obsdiags(jj,ii)%tail => NULL()
    do kk=1,kobs
       if (.not.associated(obsdiags(jj,ii)%tail)) then
         obsdiags(jj,ii)%tail => obsdiags(jj,ii)%head
       else
         obsdiags(jj,ii)%tail => obsdiags(jj,ii)%tail%next
       endif
       mymuse = obsdiags(jj,ii)%tail%muse(j)
       if ( mymuse ) then
         if(.not. associated(rwtail(ii)%head))then
            rwtail(ii)%head => rwhead(ii)%head
         else
            rwtail(ii)%head => rwtail(ii)%head%llpoint
         end if
         rwtail(ii)%head%diags    => obsdiags(jj,ii)%tail
         icount = icount + 1
       endif
    enddo
!   enddo

    if(icount.ne.mobs) call abor1('read_rwhead_: error counting ob')
!   if(mobs>0) print *, 'Read rw from obsdiag, ii =', ii, ' mobs =', mobs, ', pe ', mype
end subroutine read_rwhead_

subroutine read_dwhead_ ()
!$$$  subprogram documentation block
!
! abstract: Read obs-specific data structure from file.
!
! program history log:
!   2007-10-03  todling
!
!   input argument list:
!
!$$$

    use obsmod, only: dwhead,dwtail
    implicit none

    real(r_kind)    :: zres           !  residual
    real(r_kind)    :: zerr2          !  error squared
    real(r_kind)    :: zraterr2       !  square of ratio of final obs error
                                      !  to original obs error
    real(r_kind)    :: ztime          !  observation time
    real(r_kind)    :: zb             !  variational quality control parameter
    real(r_kind)    :: zpg            !  variational quality control parameter
    real(r_kind)    :: zcosazm        !  zonal wind factor
    real(r_kind)    :: zsinazm        !  meridional factor
    real(r_kind)    :: zwij(8)        !  horizontal interpolation weights
    integer(i_kind) :: zij(8)         !  horizontal locations
    logical         :: zluse          !  flag indicating if ob is used in pen.

    integer         :: j,mobs,jread,icount,iostat
    logical         :: mymuse   
   
    read(iunit) mobs,jread
    if(jj/=jread) call abor1('read_dwhead_: unmatched ob type')
    if(kobs<=0.or.mobs<=0) return

    do kk=1,mobs
       if(.not. associated(dwhead(ii)%head))then
          allocate(dwhead(ii)%head,stat=ierr)
          if(ierr /= 0)write(6,*)' fail to alloc dwdhead '
          dwtail(ii)%head => dwhead(ii)%head
       else
          allocate(dwtail(ii)%head%llpoint,stat=ierr)
          if(ierr /= 0)write(6,*)' fail to alloc dwtail%llpoint '
          dwtail(ii)%head => dwtail(ii)%head%llpoint
       end if
       read(iunit,iostat=iostat) zres,  zerr2, zraterr2,&
                                 ztime, zb,   zpg, &
                                 zcosazm,     zsinazm, &
                                 zluse, zwij, zij
       if (iostat/=0) call abor1('read_dwhead_: error reading record')
       dwtail(ii)%head%ij       = zij
       dwtail(ii)%head%wij      = zwij
       dwtail(ii)%head%res      = zres
       dwtail(ii)%head%err2     = zerr2
       dwtail(ii)%head%raterr2  = zraterr2
       dwtail(ii)%head%time     = ztime
       dwtail(ii)%head%b        = zb
       dwtail(ii)%head%pg       = zpg
       dwtail(ii)%head%luse     = zluse
       dwtail(ii)%head%cosazm   = zcosazm
       dwtail(ii)%head%sinazm   = zsinazm
    enddo
    if(lobserver) return

!   Now set obsdiag pointer properly
!   --------------------------------
    icount=0
    dwtail(ii)%head => NULL()
!   do j=1,kiter
       j=kiter
    obsdiags(jj,ii)%tail => NULL()
    do kk=1,kobs
       if (.not.associated(obsdiags(jj,ii)%tail)) then
         obsdiags(jj,ii)%tail => obsdiags(jj,ii)%head
       else
         obsdiags(jj,ii)%tail => obsdiags(jj,ii)%tail%next
       endif
       mymuse = obsdiags(jj,ii)%tail%muse(j)
       if ( mymuse ) then
         if(.not. associated(dwtail(ii)%head))then
            dwtail(ii)%head => dwhead(ii)%head
         else
            dwtail(ii)%head => dwtail(ii)%head%llpoint
         end if
         dwtail(ii)%head%diags    => obsdiags(jj,ii)%tail
         icount = icount + 1
       endif
    enddo
!   enddo

     if(icount.ne.mobs) call abor1('read_dwhead_: error counting ob')
!   if(mobs>0) print *, 'Read dw from obsdiag, ii =', ii, ' mobs =', mobs, ', pe ', mype
end subroutine read_dwhead_

subroutine read_ssthead_ ()
!$$$  subprogram documentation block
!
! abstract: Read obs-specific data structure from file.
!
! program history log:
!   2007-10-03  todling
!
!   input argument list:
!
!$$$

    use obsmod, only: ssthead,ssttail
    implicit none

    real(r_kind)    :: zres           !  residual
    real(r_kind)    :: zerr2          !  error squared
    real(r_kind)    :: zraterr2       !  square of ratio of final obs error
                                      !  to original obs error
    real(r_kind)    :: ztime          !  observation time
    real(r_kind)    :: zb             !  variational quality control parameter
    real(r_kind)    :: zpg            !  variational quality control parameter
    real(r_kind)    :: zwij(4)        !  horizontal interpolation weights
    integer(i_kind) :: zij(4)         !  horizontal locations
    logical         :: zluse          !  flag indicating if ob is used in pen.

    integer(i_kind) :: j,mobs,jread,icount,iostat
    logical         :: mymuse   
   
    read(iunit) mobs,jread
    if(jj/=jread) call abor1('read_ssthead_: unmatched ob type')
    if(kobs<=0.or.mobs<=0) return

    do kk=1,mobs
       if(.not. associated(ssthead(ii)%head))then
          allocate(ssthead(ii)%head,stat=ierr)
          if(ierr /= 0)write(6,*)' fail to alloc pshead '
          ssttail(ii)%head => ssthead(ii)%head
       else
          allocate(ssttail(ii)%head%llpoint,stat=ierr)
          if(ierr /= 0)write(6,*)' fail to alloc ssttail%llpoint '
          ssttail(ii)%head => ssttail(ii)%head%llpoint
       end if
       read(iunit,iostat=iostat) zres,  zerr2,    zraterr2,&
                                 ztime, zb,       zpg, &
                                 zluse, zwij, zij
       if (iostat/=0) call abor1('read_ssthead_: error reading record')
       ssttail(ii)%head%res      = zres
       ssttail(ii)%head%err2     = zerr2
       ssttail(ii)%head%raterr2  = zraterr2
       ssttail(ii)%head%time     = ztime
       ssttail(ii)%head%b        = zb
       ssttail(ii)%head%pg       = zpg
       ssttail(ii)%head%wij      = zwij
       ssttail(ii)%head%ij       = zij
       ssttail(ii)%head%luse     = zluse
    enddo
    if(lobserver) return

!   Now set obsdiag pointer properly
!   --------------------------------
    icount=0
    ssttail(ii)%head => NULL()
!   do j=1,kiter
       j=kiter
    obsdiags(jj,ii)%tail => NULL()
    do kk=1,kobs
       if (.not.associated(obsdiags(jj,ii)%tail)) then
         obsdiags(jj,ii)%tail => obsdiags(jj,ii)%head
       else
         obsdiags(jj,ii)%tail => obsdiags(jj,ii)%tail%next
       endif
       mymuse = obsdiags(jj,ii)%tail%muse(j)
       if ( mymuse ) then
       if(.not. associated(ssttail(ii)%head))then
          ssttail(ii)%head => ssthead(ii)%head
       else
          ssttail(ii)%head => ssttail(ii)%head%llpoint
       end if
       ssttail(ii)%head%diags    => obsdiags(jj,ii)%tail
       icount = icount + 1
       endif
    enddo
!   enddo
  
    if(icount.ne.mobs) call abor1('read_ssthead_: error counting ob')
!   if(mobs>0) print *, 'Read sst from obsdiag, ii =', ii, ' mobs =', mobs, ', pe ', mype
end subroutine read_ssthead_

subroutine read_pwhead_ ()
!$$$  subprogram documentation block
!
! abstract: Read obs-specific data structure from file.
!
! program history log:
!   2007-10-03  todling
!
!   input argument list:
!
!$$$

    use gridmod, only: nsig
    use obsmod, only: pwhead,pwtail
    implicit none

    real(r_kind)    :: zres           !  residual
    real(r_kind)    :: zerr2          !  error squared
    real(r_kind)    :: zraterr2       !  square of ratio of final obs error
                                      !  to original obs error
    real(r_kind)    :: ztime          !  observation time
    real(r_kind)    :: zb             !  variational quality control parameter
    real(r_kind)    :: zpg            !  variational quality control parameter
    real(r_kind)    :: zwij(4)        !  horizontal interpolation weights
    real(r_kind),dimension(:),allocatable :: zdp ! delta pressure at mid layers at obs locations 
    integer(i_kind) :: zij(4)         !  horizontal locations
    logical         :: zluse          !  flag indicating if ob is used in pen.

    integer(i_kind) :: j,mobs,jread,znsig,iostat,icount,istatus
    logical         :: mymuse   
   
    read(iunit) mobs,jread,znsig
    if(jj/=jread) call abor1('read_pwhead_: unmatched ob type')
    if(nsig /=znsig) call abor1('read_pwhead_: unmatched nsig')
    if(kobs<=0.or.mobs<=0) return

    allocate(zdp(nsig),stat=istatus)
    if (istatus/=0) write(6,*)'read_pwhead:  allocate error for zdp, istatus=',istatus

    do kk=1,mobs
       if(.not. associated(pwhead(ii)%head))then
          allocate(pwhead(ii)%head,stat=ierr)
          if(ierr /= 0)write(6,*)' fail to alloc pwhead '
          pwtail(ii)%head => pwhead(ii)%head
       else
          allocate(pwtail(ii)%head%llpoint,stat=ierr)
          if(ierr /= 0)write(6,*)' fail to alloc pwtail%llpoint '
          pwtail(ii)%head => pwtail(ii)%head%llpoint
       end if
       allocate(pwtail(ii)%head%dp(nsig),stat=istatus)
       if (istatus/=0) write(6,*)'read_pwhead:  allocate error for pw_dp, istatus=',istatus
       read(iunit,iostat=iostat) zres,  zerr2,    zraterr2,&
                                 ztime, zb,       zpg, &
                                 zluse, zwij, zij, zdp
       if (iostat/=0) call abor1('read_pwhead_: error reading record')
       pwtail(ii)%head%ij       = zij
       pwtail(ii)%head%wij      = zwij
       pwtail(ii)%head%res      = zres
       pwtail(ii)%head%err2     = zerr2
       pwtail(ii)%head%raterr2  = zraterr2
       pwtail(ii)%head%time     = ztime
       pwtail(ii)%head%b        = zb
       pwtail(ii)%head%pg       = zpg
       pwtail(ii)%head%luse     = zluse
       pwtail(ii)%head%dp       = zdp
    enddo

    deallocate(zdp,stat=istatus)
    if (istatus/=0) write(6,*)'read_pwhead:  deallocate error for zdp, istatus=',istatus
    if(lobserver) return

!   Now set obsdiag pointer properly
!   --------------------------------
    icount=0
    pwtail(ii)%head => NULL()
!   do j=1,kiter
       j=kiter
    obsdiags(jj,ii)%tail => NULL()
    do kk=1,kobs
       if (.not.associated(obsdiags(jj,ii)%tail)) then
         obsdiags(jj,ii)%tail => obsdiags(jj,ii)%head
       else
         obsdiags(jj,ii)%tail => obsdiags(jj,ii)%tail%next
       endif
       mymuse = obsdiags(jj,ii)%tail%muse(j)
       if ( mymuse ) then
         if(.not. associated(pwtail(ii)%head))then
            pwtail(ii)%head => pwhead(ii)%head
         else
            pwtail(ii)%head => pwtail(ii)%head%llpoint
         end if
         pwtail(ii)%head%diags    => obsdiags(jj,ii)%tail
         icount = icount + 1
       endif
    enddo
!   enddo

    if(icount.ne.mobs) call abor1('read_pwhead_: error counting ob')
!   if(mobs>0) print *, 'Read pw from obsdiag, ii =', ii, ' mobs =', mobs, ', pe ', mype
end subroutine read_pwhead_

subroutine read_ozhead_ ()
!$$$  subprogram documentation block
!
! abstract: Read obs-specific data structure from file.
!
! program history log:
!   2007-10-03  todling
!   2008-11-25  todling - merged with NCEP-May-2008
!   2009-01-28  todling - accommodate single level-type data
!
!   input argument list:
!
!$$$

    use gridmod, only: nsig
    use obsmod, only: ozhead,oztail
    implicit none

    real(r_kind),dimension(:),allocatable :: zres      ! residual
    real(r_kind),dimension(:),allocatable :: zerr2     ! error squared
    real(r_kind),dimension(:),allocatable :: zraterr2  ! square of ratio of final obs error
                                                       ! to original obs error
    real(r_kind)    :: ztime                           ! observation time
    real(r_kind)    :: zwij(4,nsig)                    ! horizontal interpolation weights
    real(r_kind),dimension(:),allocatable :: zprs      ! delta pressure at mid layers at obs locations 
    integer(i_kind),dimension(:),allocatable :: zipos  !
    integer(i_kind) :: zij(4)                          ! horizontal locations
    logical         :: zluse                           ! flag indicating if ob is used in pen.

    integer(i_kind) :: j,k,mobs,jread,nloz,nlevp,iostat,icount,istatus
    logical         :: first,mymuse   
   
    read(iunit) mobs,jread
    if(  jj/=jread) call abor1('read_ozhead_: unmatched ob type')
    if(kobs<=0.or.mobs<=0) return

    do kk=1,mobs

       read(iunit,iostat=iostat) nloz
       nlevp=max(nloz,1)
       allocate(zres(nloz+1),zerr2(nloz+1),zraterr2(nloz+1), &
                zprs(nlevp),zipos(nloz+1),stat=istatus)
       if (istatus/=0) write(6,*)'read_ozhead:  allocate error for zoz_point, istatus=',istatus

       if(.not. associated(ozhead(ii)%head))then
          allocate(ozhead(ii)%head,stat=ierr)
          if(ierr /= 0)write(6,*)' fail to alloc ozhead '
          oztail(ii)%head => ozhead(ii)%head
       else
          allocate(oztail(ii)%head%llpoint,stat=ierr)
          if(ierr /= 0)write(6,*)' fail to alloc oztail%llpoint '
          oztail(ii)%head => oztail(ii)%head%llpoint
       end if
       allocate(oztail(ii)%head%res(nloz+1),oztail(ii)%head%diags(nloz+1), &
                oztail(ii)%head%err2(nloz+1),oztail(ii)%head%raterr2(nloz+1), &
                oztail(ii)%head%prs(nlevp),oztail(ii)%head%ipos(nloz+1), &
                oztail(ii)%head%wij(4,nsig),stat=istatus)
       if (istatus/=0) write(6,*)'read_ozhead:  allocate error for oz_point, istatus=',istatus

       read(iunit,iostat=iostat) zres,  zerr2, zraterr2, ztime, &
                                 zluse, zwij, zij, zprs, zipos
       if (iostat/=0) call abor1('read_ozhead_: error reading record')
       oztail(ii)%head%nloz     = nloz
       oztail(ii)%head%time     = ztime
       oztail(ii)%head%luse     = zluse
       oztail(ii)%head%wij      = zwij
       oztail(ii)%head%ij       = zij

       do k=1,nloz+1
          oztail(ii)%head%res(k)       = zres(k)
          oztail(ii)%head%err2(k)      = zerr2(k)
          oztail(ii)%head%raterr2(k)   = zraterr2(k)
          oztail(ii)%head%ipos(k)      = zipos(k)
       enddo
       do k=1,nlevp
          oztail(ii)%head%prs(k)       = zprs(k)
       enddo

       deallocate(zres,zerr2,zraterr2,zprs,zipos,stat=istatus)

    enddo

    if (istatus/=0) write(6,*)'read_ozhead:  deallocate error for zoz_point, istatus=',istatus

    if(lobserver) return

!   Now set obsdiag pointer properly
!   --------------------------------
    icount=0
    oztail(ii)%head => NULL()
!   do j=1,kiter
       j=kiter
    obsdiags(jj,ii)%tail => NULL()
    obsptr => obsdiags(jj,ii)%head
    do while (associated(obsptr))

       if(.not. associated(oztail(ii)%head))then
          oztail(ii)%head => ozhead(ii)%head
       else
          oztail(ii)%head => oztail(ii)%head%llpoint
       end if

       first=.true.
       do k=1,nloz+1
          mymuse = obsptr%muse(j)
          if ( mymuse ) then
            if(first) then
               icount = icount + 1
               if(icount>mobs) call abor1('read_ozhead_: error large counter')
               first=.false.
            endif
            oztail(ii)%head%diags(k)%ptr => obsptr
            obsptr => obsptr%next
          endif
       enddo

    enddo
!   enddo
    if(icount.ne.mobs) call abor1('read_ozhead_: error counting ob')

end subroutine read_ozhead_

subroutine read_o3lhead_ ()
!$$$  subprogram documentation block
!
! abstract: Read obs-specific data structure from file.
!
! program history log:
!   2007-10-03  todling
!
!   input argument list:
!
!$$$

    use obsmod, only: o3lhead,o3ltail
    implicit none

    real(r_kind)    :: zres           !  residual
    real(r_kind)    :: zerr2          !  error squared
    real(r_kind)    :: zraterr2       !  square of ratio of final obs error
                                      !  to original obs error
    real(r_kind)    :: ztime          !  observation time
    real(r_kind)    :: zb             !  variational quality control parameter
    real(r_kind)    :: zpg            !  variational quality control parameter
    real(r_kind)    :: zwij(8)        !  horizontal interpolation weights
    integer(i_kind) :: zij(8)         !  horizontal locations
    logical         :: zluse          !  flag indicating if ob is used in pen.

    integer(i_kind) :: j,mobs,jread,icount,iostat
    logical         :: mymuse   
   
    read(iunit) mobs,jread
    if(jj/=jread) call abor1('read_o3lhead_: unmatched ob type')
    if(kobs<=0.or.mobs<=0) return

    do kk=1,mobs

       if(.not. associated(o3lhead(ii)%head))then
          allocate(o3lhead(ii)%head,stat=ierr)
          if(ierr /= 0)write(6,*)' fail to alloc o3lhead '
          o3ltail(ii)%head => o3lhead(ii)%head
       else
          allocate(o3ltail(ii)%head%llpoint,stat=ierr)
          if(ierr /= 0)write(6,*)' fail to alloc o3ltail%llpoint '
          o3ltail(ii)%head => o3ltail(ii)%head%llpoint
       end if
       read(iunit,iostat=iostat) zres,  zerr2,    zraterr2,&
                                 ztime, zb,       zpg, &
                                 zluse, zwij, zij
       if (iostat/=0) call abor1('read_o3lhead_: error reading record')
       o3ltail(ii)%head%res      = zres
       o3ltail(ii)%head%err2     = zerr2
       o3ltail(ii)%head%raterr2  = zraterr2
       o3ltail(ii)%head%time     = ztime
       o3ltail(ii)%head%b        = zb
       o3ltail(ii)%head%pg       = zpg
       o3ltail(ii)%head%wij      = zwij
       o3ltail(ii)%head%ij       = zij
       o3ltail(ii)%head%luse     = zluse

    enddo
    if(lobserver) return

!   Now set obsdiag pointer properly
!   --------------------------------
    icount=0
    o3ltail(ii)%head => NULL()
!   do j=1,kiter
       j=kiter
    obsdiags(jj,ii)%tail => NULL()
    do kk=1,kobs
       if (.not.associated(obsdiags(jj,ii)%tail)) then
         obsdiags(jj,ii)%tail => obsdiags(jj,ii)%head
       else
         obsdiags(jj,ii)%tail => obsdiags(jj,ii)%tail%next
       endif
       mymuse = obsdiags(jj,ii)%tail%muse(j)
       if ( mymuse ) then
         if(.not. associated(o3ltail(ii)%head))then
            o3ltail(ii)%head => o3lhead(ii)%head
         else
            o3ltail(ii)%head => o3ltail(ii)%head%llpoint
         end if
         o3ltail(ii)%head%diags    => obsdiags(jj,ii)%tail
         icount = icount + 1
       endif
    enddo
!   enddo

end subroutine read_o3lhead_

subroutine read_pcphead_ ()
!$$$  subprogram documentation block
!
! abstract: Read obs-specific data structure from file.
!
! program history log:
!   2007-10-03  todling
!
!   input argument list:
!
!$$$

    use obsmod, only: pcphead,pcptail
    use gridmod, only: nsig5
    use pcpinfo, only: npredp
    implicit none

    real(r_kind)    :: zobs           !  observated precipitation
    real(r_kind)    :: zerr2          !  error squared
    real(r_kind)    :: zraterr2       !  square of ratio of final obs error
                                      !  to original obs error
    real(r_kind)    :: ztime          !  observation time
    real(r_kind)    :: zges           !  guess observation
    real(r_kind)    :: zwij(4)        !  horizontal interpolation weights
    real(r_kind),allocatable :: zpredp(:)     ! predictors (npredp)
    real(r_kind),allocatable :: zdpcp_dvar(:) ! error variances squared (nsig5)
    integer(i_kind) :: zij(4)         !  horizontal locations
    integer(i_kind) :: zicxp          !  type of precipitation rate observation
    logical         :: zluse          !  flag indicating if ob is used in pen.

    integer(i_kind) :: j,mobs,jread,mpredp,msig5,iostat,icount,istatus
    logical         :: mymuse   
   
    read(iunit) mobs,jread,mpredp,msig5
    if(    jj/=jread ) call abor1('read_pcphead_: unmatched ob type')
    if(npredp/=mpredp) call abor1('read_pcphead_: unmatched number of predictors')
    if( nsig5/=msig5 ) call abor1('read_pcphead_: unmatched number of layers')
    if(kobs<=0.or.mobs<=0) return

    allocate(zpredp(npredp),zdpcp_dvar(nsig5),stat=istatus)
    if(istatus/=0)write(6,*)'read_pcphead: fail to write zpcp arrays '

    do kk=1,mobs

         if(.not. associated(pcphead(ii)%head))then
            allocate(pcphead(ii)%head,stat=ierr)
            if(ierr /= 0)write(6,*)' fail to alloc pcphead '
            pcptail(ii)%head => pcphead(ii)%head
         else
            allocate(pcptail(ii)%head%llpoint,stat=ierr)
            if(ierr /= 0)write(6,*)' fail to alloc pcptail%llpoint '
            pcptail(ii)%head => pcptail(ii)%head%llpoint
         end if
         allocate(pcptail(ii)%head%predp(npredp),pcptail(ii)%head%dpcp_dvar(nsig5), &
                  stat=istatus)
         if(istatus/=0)write(6,*)'read_pcphead: fail to alloc pcptail arrays '

         read(iunit,iostat=iostat) zobs,  zerr2,  zraterr2,&
                                   ztime, zges, zicxp, &
                                   zluse, zwij, zij, &
                                   zpredp, zdpcp_dvar
         if (iostat/=0) call abor1('read_pcphead_: error reading record')
         pcptail(ii)%head%obs      = zobs
         pcptail(ii)%head%err2     = zerr2
         pcptail(ii)%head%raterr2  = zraterr2
         pcptail(ii)%head%time     = ztime
         pcptail(ii)%head%ges      = zges
         pcptail(ii)%head%wij      = zwij
         pcptail(ii)%head%ij       = zij
         pcptail(ii)%head%icxp     = zicxp
         pcptail(ii)%head%predp    = zpredp
         pcptail(ii)%head%dpcp_dvar= zdpcp_dvar
         pcptail(ii)%head%luse     = zluse

    enddo

    deallocate(zpredp,zdpcp_dvar,stat=istatus)
    if(istatus/=0)write(6,*)'read_pcphead: fail to dealloc zpcp arrays '
    if(lobserver) return

!   Now set obsdiag pointer properly
!   --------------------------------
    icount=0
    pcptail(ii)%head => NULL()
!   do j=1,kiter
       j=kiter
    obsdiags(jj,ii)%tail => NULL()
    do kk=1,kobs
       if (.not.associated(obsdiags(jj,ii)%tail)) then
         obsdiags(jj,ii)%tail => obsdiags(jj,ii)%head
       else
         obsdiags(jj,ii)%tail => obsdiags(jj,ii)%tail%next
       endif
       mymuse = obsdiags(jj,ii)%tail%muse(j)
       if ( mymuse ) then
         if(.not. associated(pcptail(ii)%head))then
            pcptail(ii)%head => pcphead(ii)%head
         else
            pcptail(ii)%head => pcptail(ii)%head%llpoint
         end if
         pcptail(ii)%head%diags    => obsdiags(jj,ii)%tail
         icount = icount + 1
       endif
    enddo
!   enddo

!   if(mobs>0) print *, 'Read pcp from obsdiag, ii =', ii, ' mobs =', mobs, ', pe ', mype
end subroutine read_pcphead_

subroutine read_gpshead_ ()
!$$$  subprogram documentation block
!
! abstract: Read obs-specific data structure from file.
!
! program history log:
!   2009-01-27  todling
!
!   input argument list:
!
!$$$

    use obsmod, only: gpshead,gpstail
    use gridmod, only: nsig
    implicit none

    real(r_kind)    :: zres           !  residual
    real(r_kind)    :: zb,zpg         !  var QC parameters
    real(r_kind)    :: zerr2          !  error squared
    real(r_kind)    :: zraterr2       !  square of ratio of final obs error
                                      !  to original obs error
    real(r_kind)    :: ztime          !  observation time
    real(r_kind)    :: zges           !  guess observation
    real(r_kind)    :: zwij(4)        !  horizontal interpolation weights
    real(r_kind),allocatable:: zjac_t(:)   !
    real(r_kind),allocatable:: zjac_q(:)   !
    real(r_kind),allocatable:: zjac_p(:)   !
    integer(i_kind),allocatable:: zij(:,:) !  horizontal locations
    logical         :: zluse          !  flag indicating if ob is used in pen.

    integer(i_kind) :: j,mobs,msig,kk,jread,iostat,icount,istatus
    logical         :: mymuse   
   
    read(iunit) mobs,jread,msig
    if(   jj/=jread ) call abor1('read_gpshead_: unmatched ob type')
    if( nsig/=msig  ) call abor1('read_gpshead_: unmatched number of layers')
    if(kobs<=0.or.mobs<=0) return

    allocate(zjac_t(nsig),zjac_q(nsig),zjac_p(nsig+1),zij(4,nsig),stat=istatus)
      if(istatus/=0)write(6,*)'read_gpshead: fail to alloc gpstail arrays '
    do kk=1,mobs

        if(.not. associated(gpshead(ii)%head))then
            allocate(gpshead(ii)%head,stat=istatus)
            if(istatus /= 0)write(6,*)' failure to write gpshead '
            gpstail(ii)%head => gpshead(ii)%head
        else
            allocate(gpstail(ii)%head%llpoint,stat=istatus)
            if(istatus /= 0)write(6,*)' failure to write gpstail%llpoint '
            gpstail(ii)%head => gpstail(ii)%head%llpoint
        end if
        allocate(gpstail(ii)%head%jac_t(nsig),gpstail(ii)%head%jac_q(nsig), &
                 gpstail(ii)%head%jac_p(nsig+1),gpstail(ii)%head%ij(4,nsig),&
                 stat=istatus)
        if (istatus/=0) write(6,*)'READ_OBSDIAGS:  allocate error for gps_point, istat=',istatus

         read(iunit,iostat=iostat) zjac_t,zjac_q,zjac_p,&
                                   zres, zerr2, zraterr2, ztime,&
                                   zb, zpg, zij, zwij, zluse
         if (iostat/=0) call abor1('read_gpshead_: error reading record')
         gpstail(ii)%head%jac_t    = zjac_t
         gpstail(ii)%head%jac_q    = zjac_q
         gpstail(ii)%head%jac_p    = zjac_p
         gpstail(ii)%head%res      = zres
         gpstail(ii)%head%err2     = zerr2
         gpstail(ii)%head%raterr2  = zraterr2
         gpstail(ii)%head%time     = ztime
         gpstail(ii)%head%b        = zb
         gpstail(ii)%head%pg       = zpg
         gpstail(ii)%head%wij      = zwij
         gpstail(ii)%head%ij       = zij
         gpstail(ii)%head%luse     = zluse

    enddo

    deallocate(zjac_t,zjac_q,zjac_p,zij)
    if(istatus/=0)write(6,*)'read_gpshead: fail to dealloc zgps arrays '
    if(lobserver) return

!   Now set obsdiag pointer properly
!   --------------------------------
    icount=0
    gpstail(ii)%head => NULL()
!   do j=1,kiter
       j=kiter
    obsdiags(jj,ii)%tail => NULL()
    do kk=1,kobs
       if (.not.associated(obsdiags(jj,ii)%tail)) then
         obsdiags(jj,ii)%tail => obsdiags(jj,ii)%head
       else
         obsdiags(jj,ii)%tail => obsdiags(jj,ii)%tail%next
       endif
       mymuse = obsdiags(jj,ii)%tail%muse(j)
       if ( mymuse ) then
         if(.not. associated(gpstail(ii)%head))then
            gpstail(ii)%head => gpshead(ii)%head
         else
            gpstail(ii)%head => gpstail(ii)%head%llpoint
         end if
         gpstail(ii)%head%diags    => obsdiags(jj,ii)%tail
         icount = icount + 1
       endif
    enddo
!   enddo

!   if(mobs>0) print *, 'Read gps from obsdiag, ii =', ii, ' mobs =', mobs, ', pe ', mype
end subroutine read_gpshead_

subroutine read_radhead_ ()
!$$$  subprogram documentation block
!
! abstract: Read obs-specific data structure from file.
!
! program history log:
!   2007-10-24  todling
!
!   input argument list:
!
!$$$

    use obsmod, only: radhead,radtail,radptr
    use radinfo, only: npred,retrieval
    use gridmod, only: nsig3p3
    implicit none

    real(r_kind),dimension(:),allocatable :: res
                                     !  error variances squared (nchan)
    real(r_kind),dimension(:),allocatable :: err2
                                     !  error variances squared (nchan)
    real(r_kind),dimension(:),allocatable :: raterr2
                                     !  ratio of error variances squared (nchan)
    real(r_kind)    :: time          !  observation time
    real(r_kind)    :: wij(4)        !  horizontal interpolation weights
    real(r_kind),dimension(:),allocatable :: pred1
                                     !  predictors (not channel dependent)(npred-2)
    real(r_kind),dimension(:),allocatable :: pred2
                                     !  predictors (channel dependent) (nchan)
    real(r_kind),dimension(:,:),allocatable :: dtb_dvar
                                     !  error variances squared (nsig3p3,nchan)
    integer(i_kind) :: nchan         !  number of channels for this profile
    integer(i_kind) :: nchnperobs    !  number of channels per observation
    integer(i_kind) :: ij(4)         !  horizontal locations
    integer(i_kind),dimension(:),allocatable :: icx
    logical         :: luse          !  flag indicating if ob is used in pen.

    integer(i_kind) :: i,j,iii,kkk,mm,mobs,jread,k,mpred,msig3p3,iostat
    logical         :: mymuse   

    if(retrieval) call abor1('read_radhead: cannot handle retrieval')

!   Read in radhead
!   ----------------
    read(iunit) mobs,jread,mpred,msig3p3

    if(   jj/=jread      ) call abor1('read_radhead_: unmatched ob type')
    if(   npred/=mpred   ) call abor1('read_radhead_: unmatched number of predictors')
    if( nsig3p3/=msig3p3 ) call abor1('read_radhead_: unmatched levels')
    if(kobs<=0.or.mobs<=0) return

    kkk=0
    do kk=1,mobs
       read(iunit,iostat=iostat) nchan
         if (iostat/=0) call abor1('read_radhead_: error reading record nchan')

       if(.not. associated(radhead(ii)%head))then
          allocate(radhead(ii)%head,stat=ierr)
            if(ierr/=0) call abor1('read_radhead_: alloc(radhead)')
          radtail(ii)%head => radhead(ii)%head
       else
          allocate(radtail(ii)%head%llpoint,stat=ierr)
            if(ierr/=0) call abor1('read_radhead_: alloc(radtail%llpoint)')
          radtail(ii)%head => radtail(ii)%head%llpoint
       end if
       radtail(ii)%head%nchan = nchan

       allocate(res(nchan),err2(nchan),raterr2(nchan), &
                pred1(npred-2),pred2(nchan), &
                dtb_dvar(nsig3p3,nchan),icx(nchan), &
                stat=ierr)
          if(ierr/=0) call abor1(' fail to alloc various ')

       read(iunit,iostat=iostat) time, luse, wij, ij
         if (iostat/=0) call abor1('read_radhead_: error reading record time, etc')
       read(iunit,iostat=iostat) res
         if (iostat/=0) call abor1('read_radhead_: error reading record res')
       read(iunit,iostat=iostat) err2
         if (iostat/=0) call abor1('read_radhead_: error reading record err2')
       read(iunit,iostat=iostat) raterr2
         if (iostat/=0) call abor1('read_radhead_: error reading record raterr2')
       read(iunit,iostat=iostat) pred1
         if (iostat/=0) call abor1('read_radhead_: error reading record pred1')
       read(iunit,iostat=iostat) pred2
         if (iostat/=0) call abor1('read_radhead_: error reading record pred2')
       read(iunit,iostat=iostat) icx
         if (iostat/=0) call abor1('read_radhead_: error reading record icx')
       read(iunit,iostat=iostat) dtb_dvar
         if (iostat/=0) call abor1('read_radhead_: error reading record dtb_dvar')

       allocate(radtail(ii)%head%res(nchan), radtail(ii)%head%diags(nchan), &
                radtail(ii)%head%err2(nchan),radtail(ii)%head%raterr2(nchan), &
                radtail(ii)%head%pred1(npred-2),radtail(ii)%head%pred2(nchan), &
                radtail(ii)%head%dtb_dvar(nsig3p3,nchan),radtail(ii)%head%icx(nchan), &
                stat=ierr)
          if(ierr/=0) call abor1('fail to alloc radtail%various ')

       radtail(ii)%head%time = time
       radtail(ii)%head%luse = luse
       radtail(ii)%head%wij  = wij
       radtail(ii)%head%ij   = ij
       radtail(ii)%head%pred1(1:npred-2) = pred1(1:npred-2)

       iii=0
       do i=1,nchan
         iii = iii + 1
         radtail(ii)%head%res(iii)    = res(iii)
         radtail(ii)%head%err2(iii)   = err2(iii)
         radtail(ii)%head%raterr2(iii)= raterr2(iii)
         radtail(ii)%head%pred2(iii)  = pred2(iii)
         radtail(ii)%head%icx(iii)    = icx(iii)
         do k=1,nsig3p3 
            radtail(ii)%head%dtb_dvar(k,iii) = dtb_dvar(k,iii)
         enddo
       enddo

       deallocate(res,err2,raterr2,pred1,pred2,dtb_dvar,icx, stat=ierr)
          if(ierr/=0) call abor1('fail to dealloc various ')

    enddo

    if(lobserver) return

!   Now set radtail-obsdiag pointer properly
!   ----------------------------------------
    mm=0
    radtail(ii)%head => NULL()
!   do j=1,kiter
       j=kiter
    kk=0
    obsdiags(jj,ii)%tail => NULL()
    obsptr => obsdiags(jj,ii)%head
    do while (associated(obsptr))

       nchnperobs = obsptr%nchnperobs
       if (nchnperobs>0) then
          iii=0
          nchan=0
          do i=1,nchnperobs
             mymuse = obsptr%muse(j)
             if (mymuse) then
                iii=iii+1
                if (iii==1) then
                   mm=mm+1
                   if(.not. associated(radtail(ii)%head))then
                      radtail(ii)%head => radhead(ii)%head
                   else
                      radtail(ii)%head => radtail(ii)%head%llpoint
                   end if
                   nchan=radtail(ii)%head%nchan
                endif
                radtail(ii)%head%diags(iii)%ptr => obsptr  ! ?? obsdiags(jj,ii)%tail
             endif
             kk=kk+1
             if(kk>kobs) call abor1('read_radhead_: error troubled obs counter 1')
             obsptr => obsptr%next
          enddo
          if(iii/=nchan) call abor1('read_radhead_: unmatched iii/nchan')
       else
          kk=kk+1
          if(kk>kobs) call abor1('read_radhead_: error troubled obs counter 2')
          obsptr => obsptr%next
       end if

    enddo
!   enddo
    if(mm/=mobs) call abor1('read_radhead_: error radtail final obs counter')
    if(kk/=kobs) call abor1('read_radhead_: error obsdiag final obs counter')
    
end subroutine read_radhead_

end subroutine read_obsdiags
