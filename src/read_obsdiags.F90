subroutine read_obsdiags(cdfile)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    read_obdiags
!   prgmmr:      tremolet
!
! abstract: Read obsdiags data structure from file.
!
! program history log:
!   2007-07-05  tremolet
!   2007-08-04  todling  - using get_lun to determine file unit number
!   2007-10-03  todling  - expanded to account for full observer 
!   2009-01-08  todling  - remove reference to ozohead
!   2009-01-23  todling  - add read_gpshead
!   2009-04-02  meunier  - add read_laghead
!
!   input argument list:
!     cdfile - filename to read data from
!
!   output argument list:
!
! remarks: ozhead still cannot handle omi data
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

use kinds, only: r_kind,i_kind
use obsmod, only: nobs_type,obsdiags,obsptr,lobsdiag_allocated,lobserver
use obsmod, only: destroyobs
use obsmod, only: i_ps_ob_type, i_t_ob_type, i_w_ob_type, i_q_ob_type, &
                  i_spd_ob_type, i_srw_ob_type, i_rw_ob_type, i_dw_ob_type, &
                  i_sst_ob_type, i_pw_ob_type, i_pcp_ob_type, i_oz_ob_type, &
                  i_o3l_ob_type, i_gps_ob_type, i_rad_ob_type, i_lag_ob_type

use obs_sensitivity, only: lobsensfc, lsensrecompute
use gsi_4dvar, only: l4dvar, nobs_bins
use mpimod, only: mype
use constants, only: izero,ione,zero
use jfunc, only: jiter, miter
use file_utility, only : get_lun
use lag_traj, only : lag_rk2itenpara_r,lag_rk2itenpara_i

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
if (mype==izero) write(6,*)'Start reading obsdiags from file ',clfile
root = mype==izero
gogetit = .true.
if(lobserver .and. jiter==ione) gogetit = .false.

open(iunit,file=trim(clfile),form='unformatted',action='read',iostat=ierr)
if (ierr/=izero) then
   write(6,*)'read_obsdiags: error open'
   call stop2(171)
end if

do ii=1,nobs_bins
   do jj=1,nobs_type

      read(iunit)ki,kj,kobs,kiter
      if (ki/=ii) then
         write(6,*)'read_obsdiags: error ii',ii,ki
         call stop2(172)
      end if
      if (kj/=jj) then
         write(6,*)'read_obsdiags: error jj',jj,kj
         call stop2(173)
      end if
      if (lobsensfc.and..not.lsensrecompute) then
         if (kiter/=miter) then
            write(6,*)'read_obsdiags: error kiter',kiter,miter
            call stop2(174)
         end if
      else
         if (lobserver) then
            if (kiter/=jiter-ione) then
               write(6,*)'read_obsdiags: error kiter',kiter,jiter-ione
               call stop2(175)
            end if
         else
            if (kiter/=jiter) then
               write(6,*)'read_obsdiags: error kiter',kiter,jiter
               call stop2(176)
            end if
         endif
      endif

      do kk=1,kobs
         if (.not.associated(obsdiags(jj,ii)%head)) then
            allocate(obsdiags(jj,ii)%head,stat=ierr)
            if (ierr/=izero) then
               write(6,*)'read_obsdiags: fail to allocate obsdiags',ierr
               call stop2(177)
            end if
            obsdiags(jj,ii)%tail => obsdiags(jj,ii)%head
         else
            allocate(obsdiags(jj,ii)%tail%next,stat=ierr)
            if (ierr/=izero) then
               write(6,*)'read_obsdiags: fail to allocate next obsdiags',ierr
               call stop2(178)
            end if
            obsdiags(jj,ii)%tail => obsdiags(jj,ii)%tail%next
         end if
         allocate(obsdiags(jj,ii)%tail%muse(miter+ione))
         allocate(obsdiags(jj,ii)%tail%nldepart(miter+ione))
         allocate(obsdiags(jj,ii)%tail%tldepart(miter))
         allocate(obsdiags(jj,ii)%tail%obssen(miter))
         obsdiags(jj,ii)%tail%indxglb=-99999_i_kind
         obsdiags(jj,ii)%tail%nchnperobs=-99999_i_kind
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
            obsdiags(jj,ii)%tail%obssen(jiter+ione:miter)=zobssen(jiter+ione:miter)
         else
            if (lobserver) then
               obsdiags(jj,ii)%tail%obssen(1:jiter-ione)=zobssen(1:jiter-ione)
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
         if(jj==i_lag_ob_type) call read_laghead_ ()
      endif

      read(iunit)ki,kj
      if (ki/=ii) then
         write(6,*)'read_obsdiags: error ii',ii,ki
         call stop2(179)
      end if
      if (kj/=jj) then
         write(6,*)'read_obsdiags: error jj',jj,kj
         call stop2(180)
      end if
   enddo
enddo

close(iunit)
if(lobserver) call destroyobs ( skipit=.true. )
lobsdiag_allocated=.true.
if (mype==izero) write(6,*)'Finish reading obsdiags from file ',clfile

! ----------------------------------------------------------
return

contains

subroutine read_pshead_ ()
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    read_pshead_
!   prgmmr:      todling
!
! abstract: Read obs-specific data structure from file.
!
! program history log:
!   2007-10-03  todling
!   2008-12-08  todling - update to May08 version
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

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
    if(jj/=jread) then
       write(6,*)'read_pshead_: unmatched ob type',jj,jread
       call stop2(181)
    end if
    if(kobs<=izero.or.mobs<=izero) return

    do kk=1,mobs
       if(.not. associated(pshead(ii)%head))then
          allocate(pshead(ii)%head,stat=ierr)
          if(ierr /= izero)write(6,*)' fail to alloc pshead '
          pstail(ii)%head => pshead(ii)%head
       else
          allocate(pstail(ii)%head%llpoint,stat=ierr)
          if(ierr /= izero)write(6,*)' fail to alloc pstail%llpoint '
          pstail(ii)%head => pstail(ii)%head%llpoint
       end if
       read(iunit,iostat=iostat) zres,  zerr2,    zraterr2,&
                                 ztime, zb,       zpg, &
                                 zluse, zppertb,  zkx, zwij, zij
       if (iostat/=izero) then
          write(6,*)'read_pshead_: error reading record',iostat
          call stop2(182)
       end if
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
    icount=izero
    pstail(ii)%head => NULL()
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
          icount = icount + ione
       endif
    enddo
    if(icount/=mobs) then
       write(6,*)'read_pshead_: error counting ob',icount,mobs
       call stop2(183)
    end if
end subroutine read_pshead_

subroutine read_thead_ ()
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    read_thead_
!   prgmmr:      todling
!
! abstract: Read obs-specific data structure from file.
!
! program history log:
!   2007-10-03  todling
!   2008-12-08  todling - update to May08 version
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

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

    integer(i_kind) :: j,mobs,jread,icount,iostat
    logical         :: mymuse   

    read(iunit) mobs,jread
    if(jj/=jread) then
       write(6,*)'read_thead_: unmatched ob type',jj,jread
       call stop2(184)
    end if
    if(kobs<=izero.or.mobs<=izero) return
    do kk=1,mobs
       if(.not. associated(thead(ii)%head))then
          allocate(thead(ii)%head,stat=ierr)
          if(ierr /= izero)write(6,*)' fail to alloc thead '
          ttail(ii)%head => thead(ii)%head
       else
          allocate(ttail(ii)%head%llpoint,stat=ierr)
          if(ierr /= izero)write(6,*)' fail to alloc ttail%llpoint '
          ttail(ii)%head => ttail(ii)%head%llpoint
       end if
       read(iunit,iostat=iostat) zres,  zerr2,    zraterr2,&
                                 ztime, zb,       zpg, &
                                 zuse_sfc_model,  ztlm_tsfc, &
                                 zluse, ztpertb,  ztv_ob, &
                                 zk1,   zkx,      zwij, zij
       if (iostat/=izero) then
          write(6,*)'read_thead_: error reading record',iostat
          call stop2(185)
       end if
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
    icount=izero
    ttail(ii)%head => NULL()
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
          icount = icount + ione
       endif
    enddo

    if(icount/=mobs) then
       write(6,*)'read_thead_: error counting ob',icount,mobs
       call stop2(186)
    end if
end subroutine read_thead_

subroutine read_whead_ ()
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    read_whead_
!   prgmmr:      todling
!
! abstract: Read obs-specific data structure from file.
!
! program history log:
!   2007-10-03  todling
!   2008-12-08  todling - update to May08 version
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

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

    integer(i_kind) :: j,mm,mobs,jread,icount,iostat
    logical         :: mymuse
   
    read(iunit) mobs,jread
    if(jj/=jread) then
       write(6,*)'read_whead_: unmatched ob type',jj,jread
       call stop2(187)
    end if
    if(kobs<=izero.or.mobs<=izero) return

    do kk=1,mobs
       if(.not. associated(whead(ii)%head))then
          allocate(whead(ii)%head,stat=ierr)
          if(ierr /= izero)write(6,*)' fail to alloc whead '
          wtail(ii)%head => whead(ii)%head
       else
          allocate(wtail(ii)%head%llpoint,stat=ierr)
          if(ierr /= izero)write(6,*)' fail to alloc wtail%llpoint '
          wtail(ii)%head => wtail(ii)%head%llpoint
       end if
       read(iunit,iostat=iostat) zures, zvres, zerr2, zraterr2,&
                                 ztime, zb,    zpg, &
                                 zluse, zupertb, zvpertb, &
                                 zk1,   zkx,   zwij,  zij
       if (iostat/=izero) then
          write(6,*)'read_whead_: error reading record',iostat
          call stop2(188)
       end if
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
    icount=izero
    wtail(ii)%head => NULL()
    j=kiter
    obsdiags(jj,ii)%tail => NULL()
    do kk=1,kobs/2

       do mm=1,2
          if (.not.associated(obsdiags(jj,ii)%tail)) then
             obsdiags(jj,ii)%tail => obsdiags(jj,ii)%head
          else
             obsdiags(jj,ii)%tail => obsdiags(jj,ii)%tail%next
          end if
          if (mm==ione) obsptr => obsdiags(jj,ii)%tail
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
          icount = icount + ione
       endif

    enddo

    if(icount/=mobs) then
       write(6,*)'read_whead_: error counting ob',icount,mobs
       call stop2(189)
    end if
end subroutine read_whead_

subroutine read_qhead_ ()
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    read_qhead_
!   prgmmr:      todling
!
! abstract: Read obs-specific data structure from file.
!
! program history log:
!   2007-10-03  todling
!   2008-12-08  todling - update to May08 version
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

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

    integer(i_kind) :: j,mobs,jread,icount,iostat
    logical         :: mymuse   
   
    read(iunit) mobs,jread
    if(jj/=jread) then
       write(6,*)'read_qhead_: unmatched ob type',jj,jread
       call stop2(190)
    end if
    if(kobs<=izero.or.mobs<=izero) return

    do kk=1,mobs

       if(.not. associated(qhead(ii)%head))then
          allocate(qhead(ii)%head,stat=ierr)
          if(ierr /= izero)write(6,*)' fail to alloc qhead '
          qtail(ii)%head => qhead(ii)%head
       else
          allocate(qtail(ii)%head%llpoint,stat=ierr)
          if(ierr /= izero)write(6,*)' fail to alloc qtail%llpoint '
          qtail(ii)%head => qtail(ii)%head%llpoint
       end if
       read(iunit,iostat=iostat) zres,  zerr2,   zraterr2,&
                                 ztime, zb,      zpg, &
                                 zluse, zqpertb, zk1, zkx, &
                                 zwij, zij
       if(iostat/=izero) then
          write(6,*)'read_qhead_: error reading record',iostat
          call stop2(191)
       end if
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
    icount=izero
    qtail(ii)%head => NULL()
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

          icount = icount + ione
       endif
       
    enddo

    if(icount/=mobs) then
       write(6,*)'read_qhead_: error counting ob',icount,mobs
       call stop2(192)
    end if
end subroutine read_qhead_

subroutine read_spdhead_ ()
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    read_spdhead_
!   prgmmr:      todling
!
! abstract: Read obs-specific data structure from file.
!
! program history log:
!   2007-10-03  todling
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

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

    integer(i_kind) :: j,mobs,jread,icount,iostat
    logical         :: mymuse   
   
    read(iunit) mobs,jread
    if(jj/=jread) then
       write(6,*)'read_spdhead_: unmatched ob type',jj,jread
       call stop2(193)
    end if
    if(kobs<=izero.or.mobs<=izero) return

    do kk=1,mobs

       if(.not. associated(spdhead(ii)%head))then
          allocate(spdhead(ii)%head,stat=ierr)
          if(ierr /= izero)write(6,*)' fail to alloc spdhead '
          spdtail(ii)%head => spdhead(ii)%head
       else
          allocate(spdtail(ii)%head%llpoint,stat=ierr)
          if(ierr /= izero)write(6,*)' fail to alloc spdtail%llpoint '
          spdtail(ii)%head => spdtail(ii)%head%llpoint
       end if
       read(iunit,iostat=iostat) zres,  zerr2,    zraterr2,&
                                 ztime, zb,       zpg, &
                                 zuges, zvges, &
                                 zluse, zwij, zij
       if (iostat/=izero) then
          write(6,*)'read_spdhead_: error reading record',iostat
          call stop2(194)
       end if
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
    icount=izero
    spdtail(ii)%head => NULL()
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
          icount = icount + ione
       endif      
    enddo

    if(icount/=mobs) then
       write(6,*)'read_spdhead_: error counting ob',icount,mobs
       call stop2(195)
    end if
end subroutine read_spdhead_

subroutine read_srwhead_ ()
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    read_srwhead_
!   prgmmr:      todling
!
! abstract: Read obs-specific data structure from file.
!
! program history log:
!   2007-10-03  todling
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

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

    integer(i_kind) :: j,mm,mobs,jread,icount,iostat
    logical         :: mymuse   
   
    icount=izero
    read(iunit) mobs,jread
    if(jj/=jread) then
       write(6,*)'read_srwhead_: unmatched ob type',jj,jread
       call stop2(196)
    end if
    if(kobs<=izero.or.mobs<=izero) return

    do kk=1,mobs

       if(.not. associated(srwhead(ii)%head))then
          allocate(srwhead(ii)%head,stat=ierr)
          if(ierr /= izero)write(6,*)' fail to alloc srwhead '
          srwtail(ii)%head => srwhead(ii)%head
       else
          allocate(srwtail(ii)%head%llpoint,stat=ierr)
          if(ierr /= izero)write(6,*)' fail to alloc srwtail%llpoint '
          srwtail(ii)%head => srwtail(ii)%head%llpoint
       end if
       read(iunit,iostat=iostat) zres1, zres2, zerr2, zraterr2,&
                                 ztime, zb,    zpg, &
                                 zges1, zges2, &
                                 zluse, zrsrw, zwij, zij
       if (iostat/=izero) then
          write(6,*)'read_srwhead_: error reading record',iostat
          call stop2(197)
       end if
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
    icount=izero
    srwtail(ii)%head => NULL()
    j=kiter
    obsdiags(jj,ii)%tail => NULL()
    do kk=1,kobs/2

       do mm=1,2
          if (.not.associated(obsdiags(jj,ii)%tail)) then
             obsdiags(jj,ii)%tail => obsdiags(jj,ii)%head
          else
             obsdiags(jj,ii)%tail => obsdiags(jj,ii)%tail%next
          end if
          if (mm==ione) obsptr => obsdiags(jj,ii)%tail
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
          icount = icount + ione
       endif

    enddo

    if(icount/=mobs) then
       write(6,*)'read_srwhead_: error counting ob',icount,mobs
       call stop2(198)
    end if
end subroutine read_srwhead_

subroutine read_rwhead_ ()
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    read_rwhead_
!   prgmmr:      todling
!
! abstract: Read obs-specific data structure from file.
!
! program history log:
!   2007-10-03  todling
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

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

    integer(i_kind) :: j,mobs,jread,icount,iostat
    logical         :: mymuse   
   
    read(iunit) mobs,jread
    if(jj/=jread) then
       write(6,*)'read_rwhead_: unmatched ob type',jj,jread
       call stop2(199)
    end if
    if(kobs<=izero.or.mobs<=izero) return

    do kk=1,mobs
       if(.not. associated(rwhead(ii)%head))then
          allocate(rwhead(ii)%head,stat=ierr)
          if(ierr /= izero)write(6,*)' fail to alloc rwhead '
          rwtail(ii)%head => rwhead(ii)%head
       else
          allocate(rwtail(ii)%head%llpoint,stat=ierr)
          if(ierr /= izero)write(6,*)' fail to alloc rwtail%llpoint '
          rwtail(ii)%head => rwtail(ii)%head%llpoint
       end if
       read(iunit,iostat=iostat) zres,  zerr2, zraterr2,&
                                 ztime, zb,   zpg, &
                                 zcosazm,     zsinazm, &
                                 zluse, zwij, zij
       if (iostat/=izero) then
          write(6,*)'read_rwhead_: error reading record',iostat
          call stop2(200)
       end if
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
    icount=izero
    rwtail(ii)%head => NULL()
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
          icount = icount + ione
       endif
    enddo

    if(icount/=mobs) then
       write(6,*)'read_rwhead_: error counting ob',icount,mobs
       call stop2(201)
    end if
end subroutine read_rwhead_

subroutine read_dwhead_ ()
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    read_dwhead_
!   prgmmr:      todling
!
! abstract: Read obs-specific data structure from file.
!
! program history log:
!   2007-10-03  todling
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

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

    integer(i_kind) :: j,mobs,jread,icount,iostat
    logical         :: mymuse   
   
    read(iunit) mobs,jread
    if(jj/=jread) then
       write(6,*)'read_dwhead_: unmatched ob type',jj,jread
       call stop2(202)
    end if
    if(kobs<=izero.or.mobs<=izero) return

    do kk=1,mobs
       if(.not. associated(dwhead(ii)%head))then
          allocate(dwhead(ii)%head,stat=ierr)
          if(ierr /= izero)write(6,*)' fail to alloc dwdhead '
          dwtail(ii)%head => dwhead(ii)%head
       else
          allocate(dwtail(ii)%head%llpoint,stat=ierr)
          if(ierr /= izero)write(6,*)' fail to alloc dwtail%llpoint '
          dwtail(ii)%head => dwtail(ii)%head%llpoint
       end if
       read(iunit,iostat=iostat) zres,  zerr2, zraterr2,&
                                 ztime, zb,   zpg, &
                                 zcosazm,     zsinazm, &
                                 zluse, zwij, zij
       if (iostat/=izero) then
          write(6,*)'read_dwhead_: error reading record',iostat
          call stop2(203)
       end if
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
    icount=izero
    dwtail(ii)%head => NULL()
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
          icount = icount + ione
       endif
    enddo

    if(icount/=mobs) then
       write(6,*)'read_dwhead_: error counting ob',icount,mobs
       call stop2(204)
    end if
end subroutine read_dwhead_

subroutine read_ssthead_ ()
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    read_ssthead_
!   prgmmr:      todling
!
! abstract: Read obs-specific data structure from file.
!
! program history log:
!   2007-10-03  todling
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

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
    if(jj/=jread) then
       write(6,*)'read_ssthead_: unmatched ob type',jj,jread
       call stop2(205)
    end if
    if(kobs<=izero.or.mobs<=izero) return

    do kk=1,mobs
       if(.not. associated(ssthead(ii)%head))then
          allocate(ssthead(ii)%head,stat=ierr)
          if(ierr /= izero)write(6,*)' fail to alloc pshead '
          ssttail(ii)%head => ssthead(ii)%head
       else
          allocate(ssttail(ii)%head%llpoint,stat=ierr)
          if(ierr /= izero)write(6,*)' fail to alloc ssttail%llpoint '
          ssttail(ii)%head => ssttail(ii)%head%llpoint
       end if
       read(iunit,iostat=iostat) zres,  zerr2,    zraterr2,&
                                 ztime, zb,       zpg, &
                                 zluse, zwij, zij
       if (iostat/=izero) then
          write(6,*)'read_ssthead_: error reading record',iostat
          call stop2(206)
       end if
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
    icount=izero
    ssttail(ii)%head => NULL()
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
          icount = icount + ione
       endif
    enddo
  
    if(icount/=mobs) then
       write(6,*)'read_ssthead_: error counting ob',icount,mobs
       call stop2(207)
    end if
end subroutine read_ssthead_

subroutine read_pwhead_ ()
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    read_pwhead_
!   prgmmr:      todling
!
! abstract: Read obs-specific data structure from file.
!
! program history log:
!   2007-10-03  todling
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

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
    if(jj/=jread) then
       write(6,*)'read_pwhead_: unmatched ob type',jj,jread
       call stop2(208)
    end if
    if(nsig /=znsig) then
       write(6,*)'read_pwhead_: unmatched nsig',nsig,znsig
       call stop2(209)
    end if
    if(kobs<=izero.or.mobs<=izero) return

    allocate(zdp(nsig),stat=istatus)
    if (istatus/=izero) write(6,*)'read_pwhead:  allocate error for zdp, istatus=',istatus

    do kk=1,mobs
       if(.not. associated(pwhead(ii)%head))then
          allocate(pwhead(ii)%head,stat=ierr)
          if(ierr /= izero)write(6,*)' fail to alloc pwhead '
          pwtail(ii)%head => pwhead(ii)%head
       else
          allocate(pwtail(ii)%head%llpoint,stat=ierr)
          if(ierr /= izero)write(6,*)' fail to alloc pwtail%llpoint '
          pwtail(ii)%head => pwtail(ii)%head%llpoint
       end if
       allocate(pwtail(ii)%head%dp(nsig),stat=istatus)
       if (istatus/=izero) write(6,*)'read_pwhead:  allocate error for pw_dp, istatus=',istatus
       read(iunit,iostat=iostat) zres,  zerr2,    zraterr2,&
                                 ztime, zb,       zpg, &
                                 zluse, zwij, zij, zdp
       if (iostat/=izero) then
          write(6,*)'read_pwhead_: error reading record',iostat
          call stop2(210)
       end if
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
    if (istatus/=izero) write(6,*)'read_pwhead:  deallocate error for zdp, istatus=',istatus
    if(lobserver) return

!   Now set obsdiag pointer properly
!   --------------------------------
    icount=izero
    pwtail(ii)%head => NULL()
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
          icount = icount + ione
       endif
    enddo

    if(icount/=mobs) then
       write(6,*)'read_pwhead_: error counting ob',icount,mobs
       call stop2(211)
    end if
end subroutine read_pwhead_

subroutine read_ozhead_ ()
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    read_ozhead_
!   prgmmr:      todling
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
!   output argument list:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

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
    if(  jj/=jread) then
       write(6,*)'read_ozhead_: unmatched ob type',jj,jread
       call stop2(212)
    end if
    if(kobs<=izero.or.mobs<=izero) return

    do kk=1,mobs

       read(iunit,iostat=iostat) nloz
       nlevp=max(nloz,ione)
       allocate(zres(nloz+ione),zerr2(nloz+ione),zraterr2(nloz+ione), &
                zprs(nlevp),zipos(nloz+ione),stat=istatus)
       if (istatus/=izero) write(6,*)'read_ozhead:  allocate error for zoz_point, istatus=',istatus

       if(.not. associated(ozhead(ii)%head))then
          allocate(ozhead(ii)%head,stat=ierr)
          if(ierr /= izero)write(6,*)' fail to alloc ozhead '
          oztail(ii)%head => ozhead(ii)%head
       else
          allocate(oztail(ii)%head%llpoint,stat=ierr)
          if(ierr /= izero)write(6,*)' fail to alloc oztail%llpoint '
          oztail(ii)%head => oztail(ii)%head%llpoint
       end if
       allocate(oztail(ii)%head%res(nloz+ione),oztail(ii)%head%diags(nloz+ione), &
                oztail(ii)%head%err2(nloz+ione),oztail(ii)%head%raterr2(nloz+ione), &
                oztail(ii)%head%prs(nlevp),oztail(ii)%head%ipos(nloz+ione), &
                oztail(ii)%head%wij(4,nsig),stat=istatus)
       if (istatus/=izero) write(6,*)'read_ozhead:  allocate error for oz_point, istatus=',istatus

       read(iunit,iostat=iostat) zres,  zerr2, zraterr2, ztime, &
                                 zluse, zwij, zij, zprs, zipos
       if (iostat/=izero) then
          write(6,*)'read_ozhead_: error reading record',iostat
          call stop2(213)
       end if
       oztail(ii)%head%nloz     = nloz
       oztail(ii)%head%time     = ztime
       oztail(ii)%head%luse     = zluse
       oztail(ii)%head%wij      = zwij
       oztail(ii)%head%ij       = zij

       do k=1,nloz+ione
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

    if (istatus/=izero) write(6,*)'read_ozhead:  deallocate error for zoz_point, istatus=',istatus

    if(lobserver) return

!   Now set obsdiag pointer properly
!   --------------------------------
    icount=izero
    oztail(ii)%head => NULL()
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
       do k=1,nloz+ione
          mymuse = obsptr%muse(j)
          if ( mymuse ) then
             if(first) then
                icount = icount + ione
                if(icount>mobs) then
                   write(6,*)'read_ozhead_: error large counter',icount,mobs
                   call stop2(213)
                end if
                first=.false.
             endif
             oztail(ii)%head%diags(k)%ptr => obsptr
             obsptr => obsptr%next
          endif
       enddo

    enddo
    if(icount/=mobs) then
       write(6,*)'read_ozhead_: error counting ob',icount,mobs
       call stop2(214)
    end if

end subroutine read_ozhead_

subroutine read_o3lhead_ ()
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    read_o3lhead_
!   prgmmr:      todling
!
! abstract: Read obs-specific data structure from file.
!
! program history log:
!   2007-10-03  todling
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

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
    if(jj/=jread) then
       write(6,*)'read_o3lhead_: unmatched ob type',jj,jread
       call stop2(215)
    end if
    if(kobs<=izero.or.mobs<=izero) return

    do kk=1,mobs

       if(.not. associated(o3lhead(ii)%head))then
          allocate(o3lhead(ii)%head,stat=ierr)
          if(ierr /= izero)write(6,*)' fail to alloc o3lhead '
          o3ltail(ii)%head => o3lhead(ii)%head
       else
          allocate(o3ltail(ii)%head%llpoint,stat=ierr)
          if(ierr /= izero)write(6,*)' fail to alloc o3ltail%llpoint '
          o3ltail(ii)%head => o3ltail(ii)%head%llpoint
       end if
       read(iunit,iostat=iostat) zres,  zerr2,    zraterr2,&
                                 ztime, zb,       zpg, &
                                 zluse, zwij, zij
       if (iostat/=izero) then
          write(6,*)'read_o3lhead_: error reading record',iostat
          call stop2(216)
       end if
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
    icount=izero
    o3ltail(ii)%head => NULL()
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
          icount = icount + ione
       endif
    enddo
    if(icount/=mobs) then
       write(6,*)'read_o3lhead_: error counting ob',icount,mobs
       call stop2(217)
    end if

end subroutine read_o3lhead_

subroutine read_pcphead_ ()
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    read_pcphead_
!   prgmmr:      todling
!
! abstract: Read obs-specific data structure from file.
!
! program history log:
!   2007-10-03  todling
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

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
    if(    jj/=jread ) then
       write(6,*)'read_pcphead_: unmatched ob type',jj,jread
       call stop2(218)
    end if
    if(npredp/=mpredp) then
       write(6,*)'read_pcphead_: unmatched number of predictors',npredp,mpredp
       call stop2(219)
    end if
    if( nsig5/=msig5 ) then
       write(6,*)'read_pcphead_: unmatched number of layers',nsig5,msig5
       call stop2(220)
    end if
    if(kobs<=izero.or.mobs<=izero) return

    allocate(zpredp(npredp),zdpcp_dvar(nsig5),stat=istatus)
    if(istatus/=izero)write(6,*)'read_pcphead: fail to write zpcp arrays '

    do kk=1,mobs

       if(.not. associated(pcphead(ii)%head))then
          allocate(pcphead(ii)%head,stat=ierr)
          if(ierr /= izero)write(6,*)' fail to alloc pcphead '
          pcptail(ii)%head => pcphead(ii)%head
       else
          allocate(pcptail(ii)%head%llpoint,stat=ierr)
          if(ierr /= izero)write(6,*)' fail to alloc pcptail%llpoint '
          pcptail(ii)%head => pcptail(ii)%head%llpoint
       end if
       allocate(pcptail(ii)%head%predp(npredp),pcptail(ii)%head%dpcp_dvar(nsig5), &
                stat=istatus)
       if(istatus/=izero)write(6,*)'read_pcphead: fail to alloc pcptail arrays '

       read(iunit,iostat=iostat) zobs,  zerr2,  zraterr2,&
                                 ztime, zges, zicxp, &
                                 zluse, zwij, zij, &
                                 zpredp, zdpcp_dvar
       if (iostat/=izero) then
          write(6,*)'read_pcphead_: error reading record',iostat
          call stop2(221)
       end if
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
    if(istatus/=izero)write(6,*)'read_pcphead: fail to dealloc zpcp arrays '
    if(lobserver) return

!   Now set obsdiag pointer properly
!   --------------------------------
    icount=izero
    pcptail(ii)%head => NULL()
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
          icount = icount + ione
       endif
    enddo
    if(icount/=mobs) then
       write(6,*)'read_pcphead_: error counting ob',icount,mobs
       call stop2(222)
    end if

end subroutine read_pcphead_

subroutine read_gpshead_ ()
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    read_gpshead_
!   prgmmr:      todling
!
! abstract: Read obs-specific data structure from file.
!
! program history log:
!   2009-01-27  todling
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

    use obsmod, only: gpshead,gpstail
    use gridmod, only: nsig
    implicit none

    real(r_kind)    :: zres           !  residual
    real(r_kind)    :: zb,zpg         !  var QC parameters
    real(r_kind)    :: zerr2          !  error squared
    real(r_kind)    :: zraterr2       !  square of ratio of final obs error
                                      !  to original obs error
    real(r_kind)    :: ztime          !  observation time
    real(r_kind)    :: zwij(4)        !  horizontal interpolation weights
    real(r_kind),allocatable:: zjac_t(:)   !
    real(r_kind),allocatable:: zjac_q(:)   !
    real(r_kind),allocatable:: zjac_p(:)   !
    integer(i_kind),allocatable:: zij(:,:) !  horizontal locations
    logical         :: zluse          !  flag indicating if ob is used in pen.

    integer(i_kind) :: j,mobs,msig,kk,jread,iostat,icount,istatus
    logical         :: mymuse   
   
    read(iunit) mobs,jread,msig
    if(   jj/=jread ) then
       write(6,*)'read_gpshead_: unmatched ob type',jj,jread
       call stop2(223)
    end if
    if( nsig/=msig  ) then
       write(6,*)'read_gpshead_: unmatched number of layers',nsig,msig
       call stop2(224)
    end if
    if(kobs<=izero.or.mobs<=izero) return

    allocate(zjac_t(nsig),zjac_q(nsig),zjac_p(nsig+ione),zij(4,nsig),stat=istatus)
    if(istatus/=izero)write(6,*)'read_gpshead: fail to alloc gpstail arrays '
    do kk=1,mobs

       if(.not. associated(gpshead(ii)%head))then
          allocate(gpshead(ii)%head,stat=istatus)
          if(istatus /= izero)write(6,*)' failure to write gpshead '
          gpstail(ii)%head => gpshead(ii)%head
       else
          allocate(gpstail(ii)%head%llpoint,stat=istatus)
          if(istatus /= izero)write(6,*)' failure to write gpstail%llpoint '
          gpstail(ii)%head => gpstail(ii)%head%llpoint
       end if
       allocate(gpstail(ii)%head%jac_t(nsig),gpstail(ii)%head%jac_q(nsig), &
                gpstail(ii)%head%jac_p(nsig+1),gpstail(ii)%head%ij(4,nsig),&
                stat=istatus)
       if (istatus/=izero) write(6,*)'READ_OBSDIAGS:  allocate error for gps_point, istat=',istatus

       read(iunit,iostat=iostat) zjac_t,zjac_q,zjac_p,&
                                 zres, zerr2, zraterr2, ztime,&
                                 zb, zpg, zij, zwij, zluse
       if (iostat/=izero) then
          write(6,*)'read_gpshead_: error reading record',iostat
          call stop2(225)
       end if
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
    if(istatus/=izero)write(6,*)'read_gpshead: fail to dealloc zgps arrays '
    if(lobserver) return

!   Now set obsdiag pointer properly
!   --------------------------------
    icount=izero
    gpstail(ii)%head => NULL()
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
          icount = icount + ione
       endif
    enddo

    if(icount/=mobs) then
       write(6,*)'read_gpshead_: error counting ob',icount,mobs
       call stop2(226)
    end if
end subroutine read_gpshead_

subroutine read_radhead_ ()
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    read_radhead_
!   prgmmr:      todling
!
! abstract: Read obs-specific data structure from file.
!
! program history log:
!   2007-10-24  todling
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

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
    real(r_kind),dimension(:,:),allocatable :: pred
                                     !  predictors (not channel dependent)(npred-2)
    real(r_kind),dimension(:,:),allocatable :: dtb_dvar
                                     !  error variances squared (nsig3p3,nchan)
    integer(i_kind) :: nchan         !  number of channels for this profile
    integer(i_kind) :: nchnperobs    !  number of channels per observation
    integer(i_kind) :: ij(4)         !  horizontal locations
    integer(i_kind),dimension(:),allocatable :: icx
    logical         :: luse          !  flag indicating if ob is used in pen.

    integer(i_kind) :: i,j,iii,kkk,mm,mobs,jread,k,mpred,msig3p3,iostat
    logical         :: mymuse   

    if(retrieval) then
       write(6,*)'read_radhead: cannot handle retrieval'
       call stop2(227)
    end if

!   Read in radhead
!   ----------------
    read(iunit) mobs,jread,mpred,msig3p3

    if(   jj/=jread      ) then
       write(6,*)'read_radhead_: unmatched ob type',jj,jread
       call stop2(228)
    end if
    if(   npred/=mpred   ) then
       write(6,*)'read_radhead_: unmatched number of predictors',npred,mpred
       call stop2(229)
    end if
    if( nsig3p3/=msig3p3 ) then
       write(6,*)'read_radhead_: unmatched levels',nsig3p3,msig3p3
       call stop2(230)
    end if
    if(kobs<=izero.or.mobs<=izero) return

    kkk=izero
    do kk=1,mobs
       read(iunit,iostat=iostat) nchan
       if (iostat/=izero) then
          write(6,*)'read_radhead_: error reading record nchan',iostat
          call stop2(231)
       end if

       if(.not. associated(radhead(ii)%head))then
          allocate(radhead(ii)%head,stat=ierr)
          if(ierr/=izero) then
             write(6,*)'read_radhead_: alloc(radhead)',ierr
             call stop2(232)
          end if
          radtail(ii)%head => radhead(ii)%head
       else
          allocate(radtail(ii)%head%llpoint,stat=ierr)
          if(ierr/=izero) then
             write(6,*)'read_radhead_: alloc(radtail%llpoint)',ierr
             call stop2(233)
          end if
          radtail(ii)%head => radtail(ii)%head%llpoint
       end if
       radtail(ii)%head%nchan = nchan

       allocate(res(nchan),err2(nchan),raterr2(nchan), &
                pred(npred,nchan), &
                dtb_dvar(nsig3p3,nchan),icx(nchan), &
                stat=ierr)
       if(ierr/=izero) then
          write(6,*)' fail to alloc various ',ierr
          call stop2(234)
       end if

       read(iunit,iostat=iostat) time, luse, wij, ij
       if (iostat/=izero) then
          write(6,*)'read_radhead_: error reading record time, etc',iostat
          call stop2(235)
       end if
       read(iunit,iostat=iostat) res
       if (iostat/=izero) then
          write(6,*)'read_radhead_: error reading record res',iostat
          call stop2(236)
       end if
       read(iunit,iostat=iostat) err2
       if (iostat/=izero) then
          write(6,*)'read_radhead_: error reading record err2',iostat
          call stop2(237)
       end if
       read(iunit,iostat=iostat) raterr2
       if (iostat/=izero) then
          write(6,*)'read_radhead_: error reading record raterr2',iostat
          call stop2(238)
       end if
       read(iunit,iostat=iostat) pred
       if (iostat/=izero) then
          write(6,*)'read_radhead_: error reading record pred',iostat
          call stop2(239)
       end if
       read(iunit,iostat=iostat) icx
       if (iostat/=izero) then
          write(6,*)'read_radhead_: error reading record icx',iostat
          call stop2(241)
       end if
       read(iunit,iostat=iostat) dtb_dvar
       if (iostat/=izero) then
          write(6,*)'read_radhead_: error reading record dtb_dvar',iostat
          call stop2(242)
       end if

       allocate(radtail(ii)%head%res(nchan), radtail(ii)%head%diags(nchan), &
                radtail(ii)%head%err2(nchan),radtail(ii)%head%raterr2(nchan), &
                radtail(ii)%head%pred(npred,nchan),&
                radtail(ii)%head%dtb_dvar(nsig3p3,nchan),radtail(ii)%head%icx(nchan), &
                stat=ierr)
       if(ierr/=izero) then
          write(6,*)'fail to alloc radtail%various ',ierr
          call stop2(243)
       end if

       radtail(ii)%head%time = time
       radtail(ii)%head%luse = luse
       radtail(ii)%head%wij  = wij
       radtail(ii)%head%ij   = ij

       iii=izero
       do i=1,nchan
          iii = iii + ione
          radtail(ii)%head%res(iii)    = res(iii)
          radtail(ii)%head%err2(iii)   = err2(iii)
          radtail(ii)%head%raterr2(iii)= raterr2(iii)
          radtail(ii)%head%icx(iii)    = icx(iii)
          do k=1,npred
             radtail(ii)%head%pred(k,iii)  = pred(k,iii)
          end do
          do k=1,nsig3p3 
             radtail(ii)%head%dtb_dvar(k,iii) = dtb_dvar(k,iii)
          enddo
       enddo

       deallocate(res,err2,raterr2,pred,dtb_dvar,icx, stat=ierr)
       if(ierr/=izero) then
          write(6,*)'fail to dealloc various ',ierr
          call stop2(244)
       end if

    enddo

    if(lobserver) return

!   Now set radtail-obsdiag pointer properly
!   ----------------------------------------
    mm=izero
    radtail(ii)%head => NULL()
    j=kiter
    kk=izero
    obsdiags(jj,ii)%tail => NULL()
    obsptr => obsdiags(jj,ii)%head
    do while (associated(obsptr))

       nchnperobs = obsptr%nchnperobs
       if (nchnperobs>izero) then
          iii=izero
          nchan=izero
          do i=1,nchnperobs
             mymuse = obsptr%muse(j)
             if (mymuse) then
                iii=iii+ione
                if (iii==ione) then
                   mm=mm+ione
                   if(.not. associated(radtail(ii)%head))then
                      radtail(ii)%head => radhead(ii)%head
                   else
                      radtail(ii)%head => radtail(ii)%head%llpoint
                   end if
                   nchan=radtail(ii)%head%nchan
                endif
                radtail(ii)%head%diags(iii)%ptr => obsptr  ! ?? obsdiags(jj,ii)%tail
             endif
             kk=kk+ione
             if(kk>kobs) then
                write(6,*)'read_radhead_: error troubled obs counter 1',kk,kobs
                call stop2(245)
             end if
             obsptr => obsptr%next
          enddo
          if(iii/=nchan) then
             write(6,*)'read_radhead_: unmatched iii/nchan',iii,nchan
             call stop2(246)
          end if
       else
          kk=kk+ione
          if(kk>kobs) then
             write(6,*)'read_radhead_: error troubled obs counter 2',kk,kobs
             call stop2(247)
          end if
          obsptr => obsptr%next
       end if

    enddo
    if(mm/=mobs) then
       write(6,*)'read_radhead_: error radtail final obs counter',mm,mobs
       call stop2(248)
    end if
    if(kk/=kobs) then
       write(6,*)'read_radhead_: error obsdiag final obs counter',kk,kobs
       call stop2(249)
    end if
    
end subroutine read_radhead_

subroutine read_laghead_ ()
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    read_laghead_
!   prgmmr:      meunier
!
! abstract: Read obs-specific data structure from file (lagrangian data).
!
! program history log:
!   2009-04-02  meunier
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

    use obsmod, only: laghead,lagtail
    implicit none

    real(r_kind)    :: res_lon       ! residual
    real(r_kind)    :: res_lat       ! residual
    real(r_kind)    :: err2_lon      ! error squared
    real(r_kind)    :: err2_lat      ! error squared
    real(r_kind)    :: raterr2       ! square of ratio of final obs error 
                                     !  to original obs error
    real(r_kind)    :: obslon        ! observed longitude (rad)
    real(r_kind)    :: obslat        ! observed latitude  (rad)
    real(r_kind)    :: geslon        ! guessed longitude (rad)
    real(r_kind)    :: geslat        ! guessed latitude  (rad)
    integer(i_kind) :: intnum        ! internal number of balloon
    integer(i_kind),dimension(:),allocatable :: speci  ! TL parameter
    real(r_kind)   ,dimension(:),allocatable :: specr  ! TL parameter
    real(r_kind)    :: time          ! observation time in sec     
    real(r_kind)    :: b             ! variational quality control parameter
    real(r_kind)    :: pg            ! variational quality control parameter
    logical         :: luse          ! flag indicating if ob is used in pen.

    integer(i_kind) :: j,mm,mobs,jread,icount,iostat
    logical         :: mymuse

    allocate(speci(lag_rk2itenpara_i),stat=ierr)
    if(ierr /= izero)write(6,*)' failure to allocate temporary speci '
    allocate(specr(lag_rk2itenpara_r),stat=ierr)
    if(ierr /= izero)write(6,*)' failure to allocate temporary specr '
   
    read(iunit) mobs,jread
    if(jj/=jread)then
       write(6,*) 'read_laghead_: unmatched ob type jj,jread', jj,jread
       call stop2(250)
    endif
    if(kobs<=izero.or.mobs<=izero) return

    do kk=1,mobs
       if(.not. associated(laghead(ii)%head))then
          allocate(laghead(ii)%head,stat=ierr)
          if(ierr /= izero)write(6,*)' fail to alloc whead '
          lagtail(ii)%head => laghead(ii)%head
       else
          allocate(lagtail(ii)%head%llpoint,stat=ierr)
          if(ierr /= izero)write(6,*)' fail to alloc wtail%llpoint '
          lagtail(ii)%head => lagtail(ii)%head%llpoint
       end if
       allocate(lagtail(ii)%head%speci(lag_rk2itenpara_i),stat=ierr)
       if(ierr /= izero)write(6,*)' failure to allocate lagtail%speci '
       allocate(lagtail(ii)%head%specr(lag_rk2itenpara_r),stat=ierr)
       if(ierr /= izero)write(6,*)' failure to allocate lagtail%specr '

       read(iunit,iostat=iostat) res_lon, res_lat, err2_lon, err2_lat,&
         raterr2, obslon, obslat, geslon, geslat, intnum, speci, specr,&
         time, b, pg, luse 

       if (iostat/=izero) then
          write(6,*) 'read_laghead_: error reading record, iostat=', iostat
          call stop2(251)
       endif
       lagtail(ii)%head%res_lon = res_lon
       lagtail(ii)%head%res_lat = res_lat
       lagtail(ii)%head%err2_lon = err2_lon
       lagtail(ii)%head%err2_lat = err2_lat
       lagtail(ii)%head%raterr2 = raterr2
       lagtail(ii)%head%obslon = obslon
       lagtail(ii)%head%obslat = obslat
       lagtail(ii)%head%geslon = geslon
       lagtail(ii)%head%geslat = geslat
       lagtail(ii)%head%intnum = intnum
       lagtail(ii)%head%speci = speci
       lagtail(ii)%head%specr = specr
       lagtail(ii)%head%time = time
       lagtail(ii)%head%b =  b
       lagtail(ii)%head%pg = pg
       lagtail(ii)%head%luse = luse
    enddo
    if(lobserver) return

!   Now set obsdiag pointer properly
!   --------------------------------
    icount=izero
    j=kiter
    lagtail(ii)%head => NULL()
    obsdiags(jj,ii)%tail => NULL()

    do kk=1,kobs/2

       do mm=1,2
          if (.not.associated(obsdiags(jj,ii)%tail)) then
             obsdiags(jj,ii)%tail => obsdiags(jj,ii)%head
          else
             obsdiags(jj,ii)%tail => obsdiags(jj,ii)%tail%next
          end if
          if (mm==ione) obsptr => obsdiags(jj,ii)%tail
       enddo

       mymuse =  obsdiags(jj,ii)%tail%muse(j)
       if ( mymuse ) then
          if(.not. associated(lagtail(ii)%head))then
             lagtail(ii)%head => laghead(ii)%head
          else
             lagtail(ii)%head => lagtail(ii)%head%llpoint
          end if

          lagtail(ii)%head%diag_lon => obsptr
          lagtail(ii)%head%diag_lat => obsdiags(jj,ii)%tail
          icount = icount + ione
       endif

    enddo

    if(icount/=mobs) then
       write(6,*) 'read_laghead_: error counting ob, icount,mobs=',icount,mobs
       call stop2(252)
    endif
end subroutine read_laghead_

end subroutine read_obsdiags
