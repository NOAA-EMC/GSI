    subroutine init_var_tl (zt,zpw,zq,zu,zv,zsrw,zrdw,zrrw,zspd, &
                            zoz,zpr,zgps,zsst,zrad,zpcp,mype)

! abstract: an example for initializing obs arrays for tlm
!
! program history log:
!   2004-10-21  yanqiu zhu
!   2005-04-12   yanqiu zhu - add zsst,use module kinds,add only,changed 
!                             makeobs call variable list
!   2005-06-14  wu - add OMI toz
!   2006-07-28  derber  - modify to use new inner loop obs data structure

    use kinds, only: r_kind
    use obsmod, only: thead,tptr,pshead,psptr,whead,wptr,qhead,qptr,spdhead, &
                      spdptr,srwhead,srwptr,rwhead,rwptr,dwhead,dwptr,radhead, &
                      radptr,pcphead,pcpptr,ssthead,sstptr,pwhead,pwptr,ozhead,&
                      ozptr,gpshead,gpsptr,nloz
    use obsmod_tl, only: tdataerr_tl,pwdataerr_tl,qdataerr_tl,ures_tl,vres_tl,&
         srw1res_tl,srw2res_tl,spdres_tl,oz_inv_tl,presier_tl,gpsdataerr_tl,&
         rdw_tl,rrw_tl,sstdataerr_tl,usges_tl,vsges_tl,rad_inv_tl,pcpobs_tl,&
         makecobs_tl,ozo_inv_tl
    use constants, only: zero
    implicit none

!   declare variables
    integer mype,i,k,j
    real(r_kind)  zt,zpw,zq,zu,zv,zsrw,zrdw,zrrw,zspd, &
                   zoz,zpr,zgps,zsst,zrad,zpcp

    call makecobs_tl(.false.)

    i=0
    radptr=>radhead
    do while(associated(radptr))
      do j=1,radptr%nchan
        i=i+1
        rad_inv_tl(i) = zrad*radptr%res(j)
      end do
      radptr=>radptr%llpoint
    end do

    i=0
    pcpptr=>pcphead
    do while(associated(pcpptr))
      i=i+1
      pcpobs_tl(i) = zpcp * (pcpptr%obs-pcpptr%ges)
      pcpptr=>pcpptr%llpoint
    end do

    i=0
    psptr=>pshead
    do while(associated(psptr))
      i=i+1
      presier_tl(i) = zpr* psptr%res
      psptr=>psptr%llpoint
    end do

    i=0
    tptr=>thead
    do while(associated(tptr))
      i=i+1
      tdataerr_tl(i) = zt* tptr%res
      tptr=>tptr%llpoint
    end do

    i=0
    wptr=>whead
    do while(associated(wptr))
      i=i+1
      usges_tl(i)= zero
      vsges_tl(i)= zero
      ures_tl(i) = zu* wptr%ures
      vres_tl(i) = zv* wptr%vres
      wptr=>wptr%llpoint
    end do

    i=0
    qptr=>qhead
    do while(associated(qptr))
      i=i+1
      qdataerr_tl(i) = zq* qptr%res
      qptr=>qptr%llpoint
    end do

    i=0
    spdptr=>spdhead
    do while(associated(spdptr))
      i=i+1
      spdres_tl(i) = zspd * spdptr%res
      spdptr=>spdptr%llpoint
    end do

    i=0
    srwptr=>srwhead
    do while(associated(srwptr))
      i=i+1
      srw1res_tl(i) = zsrw * srwptr%res1
      srw2res_tl(i) = zsrw * srwptr%res2
      srwptr=>srwptr%llpoint
    end do

    i=0
    rwptr=>rwhead
    do while(associated(rwptr))
      i=i+1
      rrw_tl(i) = zrrw * rwptr%res
      rwptr=>rwptr%llpoint
    end do

    i=0
    dwptr=>dwhead
    do while(associated(dwptr))
      i=i+1
      rdw_tl(i) = zrdw * dwptr%res
      dwptr=>dwptr%llpoint
    end do

    i=0
    sstptr=>ssthead
    do while(associated(sstptr))
      i=i+1
      sstdataerr_tl(i) = zsst * sstptr%res
      sstptr=>sstptr%llpoint
    end do


!   pwdataerr_tl = zpw * pw_ob(1:npwdat_s)%res*pw_ob(1:npwdat_s)%err2
!   do i=1,nozdata_s
!     do k=1,nloz+1
!       oz_inv_tl(i,k)  = zoz * oz_ob(i)%res(k)* oz_ob(i)%err2(k)
!     end do
!   end do
!   ozo_inv_tl    = zoz * ozo_ob(1:nozodata_s)%res*ozo_ob(1:nozodata_s)%err2
!   gpsdataerr_tl= zgps * gps_ob(1:ngpsdat_s)%res*gps_ob(1:ngpsdat_s)%err2


    return
  end subroutine init_var_tl
