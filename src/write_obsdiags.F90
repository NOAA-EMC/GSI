subroutine write_obsdiags(cdfile)

!$$$  subprogram documentation block
!
! abstract: Write obsdiags data structure to file.
!
! program history log:
!   2007-07-05  tremolet
!   2007-10-03  todling - expanded to account for full observer
!   2007-10-24  todling - add parameter nchnperobs to obsdiag 
!   2009-01-08  todling - remove reference to ozohead
!   2009-01-27  todling - add gps write
!
!   input argument list:
!     cdfile - filename to write data
!
!$$$

use kinds, only: r_kind,i_kind
use obsmod, only: nobs_type,obsdiags,obsptr,lobserver
use obsmod, only: gpshead
use obsmod, only: gpsptr
use obsmod, only: i_ps_ob_type, i_t_ob_type, i_w_ob_type, i_q_ob_type, &
                  i_spd_ob_type, i_srw_ob_type, i_rw_ob_type, i_dw_ob_type, &
                  i_sst_ob_type, i_pw_ob_type, i_pcp_ob_type, i_oz_ob_type, &
                  i_o3l_ob_type, i_gps_ob_type, i_rad_ob_type, i_lag_ob_type
use gsi_4dvar, only: nobs_bins,l4dvar
use mpimod, only: mype
use jfunc, only: jiter, miter, last

implicit none
character(len=*), intent(in) :: cdfile

character(len=100) :: clfile
character(len=5) :: clmype
integer(i_kind) :: iunit,ii,jj,iobs,ierr
integer(i_kind) :: icount(nobs_type,nobs_bins)
logical :: muse
! ----------------------------------------------------------

iunit=77
clmype='.YYYY'
write(clmype(2:5),'(I4.4)')mype
clfile=trim(cdfile)//clmype
if (mype==0) write(6,*)'Start writing obsdiags to file ',clfile

open(iunit,file=trim(clfile),form='unformatted',action='write',iostat=ierr)
if (ierr/=0) then
  write(6,*)'write_obsdiags: error open',ierr
  call stop2(316)
end if

icount = 0
do ii=1,nobs_bins
  do jj=1,nobs_type
    obsptr => obsdiags(jj,ii)%head
    iobs=0
    do while (associated(obsptr))
      obsptr => obsptr%next
      iobs=iobs+1
    enddo
    write(iunit)ii,jj,iobs,jiter

    obsptr => obsdiags(jj,ii)%head
    do while (associated(obsptr))
      write(iunit) obsptr%indxglb, obsptr%nchnperobs, obsptr%luse, obsptr%muse(1:jiter), &
                   obsptr%nldepart(1:jiter), obsptr%tldepart(1:jiter), &
                   obsptr%wgtjo, obsptr%obssen(1:jiter)
      obsptr => obsptr%next
    enddo

    if (l4dvar) then
      if(jj==i_ps_ob_type)  call write_pshead_  ()
      if(jj==i_t_ob_type)   call write_thead_   ()
      if(jj==i_w_ob_type)   call write_whead_   ()
      if(jj==i_q_ob_type)   call write_qhead_   ()
      if(jj==i_spd_ob_type) call write_spdhead_ ()
      if(jj==i_srw_ob_type) call write_srwhead_ ()
      if(jj==i_rw_ob_type)  call write_rwhead_  ()
      if(jj==i_dw_ob_type)  call write_dwhead_  ()
      if(jj==i_sst_ob_type) call write_ssthead_ ()
      if(jj==i_pw_ob_type)  call write_pwhead_  ()
      if(jj==i_oz_ob_type)  call write_ozhead_  ()
      if(jj==i_o3l_ob_type) call write_o3lhead_ ()
      if(jj==i_pcp_ob_type) call write_pcphead_ ()
      if(jj==i_gps_ob_type) call write_gpshead_ ()
      if(jj==i_rad_ob_type) call write_radhead_ ()
      if(jj==i_lag_ob_type) call write_laghead_ ()
    endif

    write(iunit)ii,jj
  enddo
enddo

close(iunit)
if (mype==0) write(6,*)'Finish writing obsdiags to file ',clfile

! ----------------------------------------------------------
return

contains

subroutine write_pshead_ ()
!$$$  subprogram documentation block
!
! abstract: Write obs-specific data structure to file.
!
! program history log:
!   2007-10-03  todling
!   2008-12-08  todling - update to May08 version
!
!   input argument list:
!
!$$$
    use obsmod, only: pshead, psptr
    implicit none 
    integer(i_kind) mobs

    psptr   => pshead(ii)%head
    mobs=0
    do while (associated(psptr))
      psptr => psptr%llpoint
      mobs=mobs+1
    enddo
    icount(jj,ii) = mobs
    write(iunit)mobs,jj
    if(mobs==0) return
    psptr   => pshead(ii)%head
    do while (associated(psptr))
       write(iunit) psptr%res,  psptr%err2,psptr%raterr2,&
                    psptr%time, psptr%b,   psptr%pg, &
                    psptr%luse, psptr%ppertb, psptr%kx, &
                    psptr%wij,  psptr%ij 
       psptr => psptr%llpoint
    enddo
!   if (mobs>0) write(6,*)'Wrote ps to obsdiag file, ii=', ii, ' mobs =', mobs
end subroutine write_pshead_

subroutine write_thead_ ()
!$$$  subprogram documentation block
!
! abstract: Write obs-specific data structure to file.
!
! program history log:
!   2007-10-03  todling
!   2008-12-08  todling - update to May08 version
!
!   input argument list:
!
!$$$

    use obsmod, only: thead,tptr
    implicit none

    integer(i_kind) mobs

    tptr   => thead(ii)%head
    mobs=0
    do while (associated(tptr))
      tptr => tptr%llpoint
      mobs=mobs+1
    enddo
    write(iunit)mobs,jj
    icount(jj,ii) = mobs
    if(mobs==0) return
    tptr   => thead(ii)%head
    do while (associated(tptr))
       write(iunit) tptr%res,  tptr%err2,tptr%raterr2,&
                    tptr%time, tptr%b,   tptr%pg, &
                    tptr%use_sfc_model,  tptr%tlm_tsfc, &
                    tptr%luse, tptr%tpertb, tptr%tv_ob, &
                    tptr%k1,   tptr%kx,  tptr%wij,  tptr%ij 
       tptr => tptr%llpoint
    enddo
!   if (mobs>0) write(6,*)'Wrote t to obsdiag file, ii=', ii, ' mobs =', mobs
end subroutine write_thead_

subroutine write_whead_ ()
!$$$  subprogram documentation block
!
! abstract: Write obs-specific data structure to file.
!
! program history log:
!   2007-10-03  todling
!   2008-12-08  todling - update to May08 version
!
!   input argument list:
!
!$$$

    use obsmod, only: whead,wptr
    implicit none

    integer(i_kind) mobs

    wptr   => whead(ii)%head
    mobs=0
    do while (associated(wptr))
      wptr => wptr%llpoint
      mobs=mobs+1
    enddo
    write(iunit)mobs,jj
    icount(jj,ii) = mobs
    if(mobs==0) return
    wptr   => whead(ii)%head
    do while (associated(wptr))
       write(iunit) wptr%ures, wptr%vres, wptr%err2,wptr%raterr2,&
                    wptr%time, wptr%b,    wptr%pg, &
                    wptr%luse, wptr%upertb, wptr%vpertb, & 
                    wptr%k1,   wptr%kx,   wptr%wij, wptr%ij 
       wptr => wptr%llpoint
    enddo
!   if (mobs>0) write(6,*)'Wrote w to obsdiag file, ii=', ii, ' mobs =', mobs
end subroutine write_whead_

subroutine write_qhead_ ()
!$$$  subprogram documentation block
!
! abstract: Write obs-specific data structure to file.
!
! program history log:
!   2007-10-03  todling
!   2008-12-08  todling - update to May08 version
!
!   input argument list:
!
!$$$

    use obsmod, only: qhead,qptr
    implicit none

    integer(i_kind) mobs

    qptr   => qhead(ii)%head
    mobs=0
    do while (associated(qptr))
      qptr => qptr%llpoint
      mobs=mobs+1
    enddo
    write(iunit)mobs,jj
    icount(jj,ii) = mobs
    if(mobs==0) return
    qptr   => qhead(ii)%head
    do while (associated(qptr))
       write(iunit) qptr%res,  qptr%err2,qptr%raterr2,&
                    qptr%time, qptr%b,   qptr%pg, &
                    qptr%luse, qptr%qpertb, &
                    qptr%k1,   qptr%kx,  qptr%wij, qptr%ij 
       qptr => qptr%llpoint
    enddo
!   if (mobs>0) write(6,*)'Wrote q to obsdiag file, ii=', ii, ' mobs =', mobs
end subroutine write_qhead_

subroutine write_spdhead_ ()
!$$$  subprogram documentation block
!
! abstract: Write obs-specific data structure to file.
!
! program history log:
!   2007-10-03  todling
!
!   input argument list:
!
!$$$

    use obsmod, only: spdhead,spdptr
    implicit none

    integer(i_kind) mobs

    spdptr   => spdhead(ii)%head
    mobs=0
    do while (associated(spdptr))
      spdptr => spdptr%llpoint
      mobs=mobs+1
    enddo
    write(iunit)mobs,jj
    icount(jj,ii) = mobs
    if(mobs==0) return
    spdptr   => spdhead(ii)%head
    do while (associated(spdptr))
       write(iunit) spdptr%res,  spdptr%err2,spdptr%raterr2,&
                    spdptr%time, spdptr%b,   spdptr%pg, &
                    spdptr%uges, spdptr%vges, &
                    spdptr%luse, spdptr%wij, spdptr%ij 
       spdptr => spdptr%llpoint
    enddo
!   if (mobs>0) write(6,*)'Wrote spd to obsdiag file, ii=', ii, ' mobs =', mobs
end subroutine write_spdhead_

subroutine write_srwhead_ ()
!$$$  subprogram documentation block
!
! abstract: Write obs-specific data structure to file.
!
! program history log:
!   2007-10-03  todling
!
!   input argument list:
!
!$$$

    use obsmod, only: srwhead,srwptr
    implicit none

    integer(i_kind) mobs

    srwptr   => srwhead(ii)%head
    mobs=0
    do while (associated(srwptr))
      srwptr => srwptr%llpoint
      mobs=mobs+1
    enddo
    write(iunit)mobs,jj
    icount(jj,ii) = mobs
    if(mobs==0) return
    srwptr   => srwhead(ii)%head
    do while (associated(srwptr))
       write(iunit) srwptr%res1, srwptr%res2,srwptr%err2,srwptr%raterr2,&
                    srwptr%time, srwptr%b,   srwptr%pg, &
                    srwptr%ges1, srwptr%ges2, &
                    srwptr%luse, srwptr%rsrw, srwptr%wij, srwptr%ij 
       srwptr => srwptr%llpoint
    enddo

end subroutine write_srwhead_

subroutine write_rwhead_ ()
!$$$  subprogram documentation block
!
! abstract: Write obs-specific data structure to file.
!
! program history log:
!   2007-10-03  todling
!
!   input argument list:
!
!$$$

    use obsmod, only: rwhead,rwptr
    implicit none

    integer(i_kind) mobs

    rwptr   => rwhead(ii)%head
    mobs=0
    do while (associated(rwptr))
      rwptr => rwptr%llpoint
      mobs=mobs+1
    enddo
    write(iunit)mobs,jj
    icount(jj,ii) = mobs
    if(mobs==0) return
    rwptr   => rwhead(ii)%head
    do while (associated(rwptr))
       write(iunit) rwptr%res,  rwptr%err2,rwptr%raterr2,&
                    rwptr%time, rwptr%b,   rwptr%pg, &
                    rwptr%cosazm, rwptr%sinazm, &
                    rwptr%luse, rwptr%wij, rwptr%ij 
       rwptr => rwptr%llpoint
    enddo

end subroutine write_rwhead_

subroutine write_dwhead_ ()
!$$$  subprogram documentation block
!
! abstract: Write obs-specific data structure to file.
!
! program history log:
!   2007-10-03  todling
!
!   input argument list:
!
!$$$

    use obsmod, only: dwhead,dwptr
    implicit none

    integer(i_kind) mobs

    dwptr   => dwhead(ii)%head
    mobs=0
    do while (associated(dwptr))
      dwptr => dwptr%llpoint
      mobs=mobs+1
    enddo
    write(iunit)mobs,jj
    icount(jj,ii) = mobs
    if(mobs==0) return
    dwptr   => dwhead(ii)%head
    do while (associated(dwptr))
       write(iunit) dwptr%res,  dwptr%err2,dwptr%raterr2,&
                    dwptr%time, dwptr%b,   dwptr%pg, &
                    dwptr%cosazm, dwptr%sinazm, &
                    dwptr%luse, dwptr%wij, dwptr%ij 
       dwptr => dwptr%llpoint
    enddo

end subroutine write_dwhead_

subroutine write_ssthead_ ()
!$$$  subprogram documentation block
!
! abstract: Write obs-specific data structure to file.
!
! program history log:
!   2007-10-03  todling
!
!   input argument list:
!
!$$$
    use obsmod, only: ssthead, sstptr
    implicit none 
    integer(i_kind) mobs

    sstptr   => ssthead(ii)%head
    mobs=0
    do while (associated(sstptr))
      sstptr => sstptr%llpoint
      mobs=mobs+1
    enddo
    write(iunit)mobs,jj
    icount(jj,ii) = mobs
    if(mobs==0) return
    sstptr   => ssthead(ii)%head
    do while (associated(sstptr))
       write(iunit) sstptr%res,  sstptr%err2,sstptr%raterr2,&
                    sstptr%time, sstptr%b,   sstptr%pg, &
                    sstptr%luse, sstptr%wij, sstptr%ij 
       sstptr => sstptr%llpoint
    enddo

end subroutine write_ssthead_

subroutine write_pwhead_ ()
!$$$  subprogram documentation block
!
! abstract: Write obs-specific data structure to file.
!
! program history log:
!   2007-10-03  todling
!
!   input argument list:
!
!$$$
    use gridmod, only: nsig
    use obsmod, only: pwhead, pwptr
    implicit none 
    integer(i_kind) mobs

    pwptr   => pwhead(ii)%head
    mobs=0
    do while (associated(pwptr))
      pwptr => pwptr%llpoint
      mobs=mobs+1
    enddo
    write(iunit)mobs,jj,nsig
    icount(jj,ii) = mobs
    if(mobs==0) return
    pwptr   => pwhead(ii)%head
    do while (associated(pwptr))
       write(iunit) pwptr%res,  pwptr%err2,pwptr%raterr2,&
                    pwptr%time, pwptr%b,   pwptr%pg, &
                    pwptr%luse, pwptr%wij, pwptr%ij, pwptr%dp 
       pwptr => pwptr%llpoint
    enddo

end subroutine write_pwhead_

subroutine write_ozhead_ ()
!$$$  subprogram documentation block
!
! abstract: Write obs-specific data structure to file.
!
! program history log:
!   2007-10-03  todling
!   2008-11-25  todling - merged with NCEP-May-2008
!
!   input argument list:
!
!$$$
    use obsmod, only: ozhead, ozptr
    implicit none 
    integer(i_kind) mobs

    ozptr   => ozhead(ii)%head
    mobs=0
    do while (associated(ozptr))
      ozptr => ozptr%llpoint
      mobs=mobs+1
    enddo
    write(iunit)mobs,jj
    icount(jj,ii) = mobs
    if(mobs==0) return
    ozptr   => ozhead(ii)%head
    do while (associated(ozptr))
       write(iunit) ozptr%nloz
       write(iunit) ozptr%res,  ozptr%err2,ozptr%raterr2, ozptr%time, & 
                    ozptr%luse, ozptr%wij, ozptr%ij, ozptr%prs , ozptr%ipos
       ozptr => ozptr%llpoint
    enddo

end subroutine write_ozhead_

subroutine write_o3lhead_ ()
!$$$  subprogram documentation block
!
! abstract: Write obs-specific data structure to file.
!
! program history log:
!   2007-10-03  todling
!
!   input argument list:
!
!$$$
    use obsmod, only: o3lhead, o3lptr
    implicit none 
    integer(i_kind) mobs

    o3lptr   => o3lhead(ii)%head
    mobs=0
    do while (associated(o3lptr))
      o3lptr => o3lptr%llpoint
      mobs=mobs+1
    enddo
    write(iunit)mobs,jj
    icount(jj,ii) = mobs
    if(mobs==0) return
    o3lptr   => o3lhead(ii)%head
    do while (associated(o3lptr))
       write(iunit) o3lptr%res,  o3lptr%err2,o3lptr%raterr2,&
                    o3lptr%time, o3lptr%b,   o3lptr%pg, &
                    o3lptr%luse, o3lptr%wij, o3lptr%ij 
       o3lptr => o3lptr%llpoint
    enddo

end subroutine write_o3lhead_

subroutine write_gpshead_ ()
!$$$  subprogram documentation block
!
! abstract: Write obs-specific data structure to file.
!
! program history log:
!   2009-01-27  todling
!
!   input argument list:
!
!$$$
    use obsmod, only: gpshead, gpsptr
    use gridmod, only : nsig
    implicit none 
    integer(i_kind) mobs

    gpsptr   => gpshead(ii)%head
    mobs=0
    do while (associated(gpsptr))
      gpsptr => gpsptr%llpoint
      mobs=mobs+1
    enddo
    write(iunit)mobs,jj,nsig
    icount(jj,ii) = mobs
    if(mobs==0) return
    gpsptr   => gpshead(ii)%head
    do while (associated(gpsptr))
       write(iunit) gpsptr%jac_t,gpsptr%jac_q,gpsptr%jac_p,&
                    gpsptr%res,gpsptr%err2,&
                    gpsptr%raterr2,gpsptr%time,&
                    gpsptr%b,gpsptr%pg,&
                    gpsptr%ij,gpsptr%wij,&
                    gpsptr%luse
       gpsptr => gpsptr%llpoint
    enddo

end subroutine write_gpshead_

subroutine write_pcphead_ ()
!$$$  subprogram documentation block
!
! abstract: Write obs-specific data structure to file.
!
! program history log:
!   2007-10-03  todling
!
!   input argument list:
!
!$$$
    use obsmod, only: pcphead, pcpptr
    use gridmod, only : nsig5
    use pcpinfo, only : npredp
    implicit none 
    integer(i_kind) mobs

    pcpptr   => pcphead(ii)%head
    mobs=0
    do while (associated(pcpptr))
      pcpptr => pcpptr%llpoint
      mobs=mobs+1
    enddo
    write(iunit)mobs,jj,npredp,nsig5
    icount(jj,ii) = mobs
    if(mobs==0) return
    pcpptr   => pcphead(ii)%head
    do while (associated(pcpptr))
       write(iunit) pcpptr%obs,  pcpptr%err2,pcpptr%raterr2,&
                    pcpptr%time, pcpptr%ges, pcpptr%icxp, &
                    pcpptr%luse, pcpptr%wij, pcpptr%ij, &
                    pcpptr%predp, pcpptr%dpcp_dvar
       pcpptr => pcpptr%llpoint
    enddo

end subroutine write_pcphead_

subroutine write_radhead_ ()
!$$$  subprogram documentation block
!
! abstract: Write obs-specific data structure to file.
!
! program history log:
!   2007-10-03  todling
!
!   input argument list:
!
!$$$
    use obsmod, only: radhead, radptr
    use radinfo, only: npred
    use gridmod, only: nsig3p3
    implicit none 
    integer(i_kind) mobs
 
    integer(i_kind) i,j,nchan

    radptr   => radhead(ii)%head
    mobs=0
    do while (associated(radptr))
      radptr => radptr%llpoint
      mobs=mobs+1
    enddo
    write(iunit)mobs,jj,npred,nsig3p3
    icount(jj,ii) = mobs
    if(mobs==0) return
    radptr   => radhead(ii)%head
    do while (associated(radptr))
       nchan = radptr%nchan
       write(iunit) nchan
       write(iunit) radptr%time, radptr%luse, radptr%wij, radptr%ij
       write(iunit) radptr%res
       write(iunit) radptr%err2
       write(iunit) radptr%raterr2
       write(iunit) radptr%pred1
       write(iunit) radptr%pred2
       write(iunit) radptr%icx
       write(iunit) radptr%dtb_dvar

!      write(iunit) radptr%res,  radptr%err2, radptr%raterr2,&
!                   radptr%time, radptr%pred1,radptr%pred2,&
!                   radptr%luse, radptr%wij,  radptr%ij, &
!                   radptr%icx,  radptr%dtb_dvar
       radptr => radptr%llpoint
    enddo
!   if (mobs>0) write(6,*)'Wrote rad to obsdiag file, ii=', ii, ' mobs =', mobs
end subroutine write_radhead_

subroutine write_laghead_ ()
!$$$  subprogram documentation block
!
! abstract: Write obs-specific data structure to file (for lagrangian data).
!
! program history log:
!   2009-04-02  meunier
!
!   input argument list:
!
!$$$
    use obsmod, only: laghead,lagptr
    implicit none

    integer(i_kind)::mobs

    lagptr   => laghead(ii)%head
    mobs=0
    do while (associated(lagptr))
      lagptr => lagptr%llpoint
      mobs=mobs+1
    enddo
    write(iunit) mobs,jj
    icount(jj,ii) = mobs
    if(mobs==0) return
    lagptr   => laghead(ii)%head
    do while (associated(lagptr))
       write(iunit) lagptr%res_lon, lagptr%res_lat, lagptr%err2_lon,&
         lagptr%err2_lat, lagptr%raterr2, lagptr%obslon, lagptr%obslat,&
         lagptr%geslon, lagptr%geslat, lagptr%intnum, lagptr%speci,&
         lagptr%specr, lagptr%time, lagptr%b, lagptr%pg, lagptr%luse
       lagptr => lagptr%llpoint
    enddo
end subroutine write_laghead_

end subroutine write_obsdiags
