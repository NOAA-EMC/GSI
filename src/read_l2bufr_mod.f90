module read_l2bufr_mod
!$$$   module documentation block
!                .      .    .                                       .
! module:    read_l2bufr_mod module for processing level2 radar bufr files
!   prgmmr: parrish          org: np22                date: 2005-06-06
!
! abstract: Process level 2 radar bufr files, converting radial wind observations
!             to superobs, which are read by subroutine read_superwinds.
!             Because these files can be very big (5-10 Gb), some additions were
!             made to bufrlib to allow use of mpi-io routines when reading the
!             bufr file.  This reduced processing time from 5-8 minutes to
!             60-90 seconds for a 5Gb test file.
!
! program history log:
!   2005-08-01  parrish
!   2006-04-21  parrish, complete rewrite.
!   2006-05-22  parrish, fix bug which causes infinite loop when no data pass initial checks
!   2007-10-24  parrish  add l2superob_only option
!   2009-04-18  woollen  improve mpi_io interface with bufrlib routines
!   2009-11-24  parrish  change time variable from regional_time (passed from gridmod) to
!                          iadate (passed from obsmod), to prevent all radar data being tossed.
!
! subroutines included:
!   sub initialize_superob_radar - initialize superob parameters to defaults
!   sub radar_bufr_read_all      - process input level 2 bufr file and write out superobs
!
! Variable Definitions:
!   def del_azimuth     - azimuth range for superob box  (default 5 degrees)
!   def del_elev        - elevation angle range for superob box  (default .05 degrees)
!   def del_range       - radial range for superob box  (default 5 km)
!   def del_time        - 1/2 time range for superob box  (default .5 hours)
!   def elev_angle_max  - max elevation angle (default of 5 deg recommended by S. Liu)
!   def minnum                  - minimum number of samples needed to make a superob
!   def range_max       - max radial range to use in constructing superobs  (default 100km)
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$ end documentation block

  use kinds, only: r_kind,i_kind,r_single
  implicit none


  integer(i_kind) minnum
  real(r_kind) del_azimuth,del_elev,del_range,del_time,elev_angle_max,range_max
  logical l2superob_only

contains

  subroutine initialize_superob_radar
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    initialize_superob_radar
!
!   prgrmmr:
!
! abstract:
!
! program history log:
!   2008-04-21  safford -- add subprogram doc block, rm unused uses
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language:  f90
!   machine:
!
!$$$
    use constants, only: quarter,half,five
    implicit none

    del_azimuth=five            !   (5 degrees)
    del_elev=quarter            !   (.25 degrees for elevation angle bin)
    del_range=5000._r_kind      !  (5 km)
    del_time=half               !   (hours)
    elev_angle_max=five         !  recommended by S. Liu to avoid heavy convection problems
    minnum=50
    range_max=100000._r_kind    !  (100km)
    l2superob_only=.false.

  end subroutine initialize_superob_radar

  subroutine radar_bufr_read_all(npe,mype)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    radar_bufr_read_all
!
!   prgrmmr:
!
! abstract:
!
! program history log:
!   2008-04-21  safford -- add subprogram doc block, rm unused uses
!
!   input argument list:
!     mype     - mpi task id
!     npe
!
!   output argument list:
!
! attributes:
!   language:  f90
!   machine:
!
!$$$  end documentation block

    use kinds, only: r_double
    use constants, only: zero,half,one,two,rearth,deg2rad,rad2deg
    use mpimod, only: mpi_comm_world,mpi_min,mpi_sum,mpi_real4,mpi_real8,ierror
    use mpimod, only: mpi_max,mpi_integer4
    use obsmod,only: iadate
    implicit none

    integer(i_kind),intent(in):: npe,mype

    integer(i_kind),parameter:: max_num_radars=150
    integer(i_kind),parameter:: n_gates_max=4000
    real(r_single),allocatable::bins(:,:,:,:,:)
    real(r_single),allocatable::bin0all(:,:,:,:,:)
    real(r_single) bins_work(7,max_num_radars)
!            bins(1,...) radial distance
!            bins(2,...) azimuth
!            bins(3,...) elev angle
!            bins(4,...) vr
!            bins(5,...) vr**2
!            bins(6,...) relative time
!            bins(7,...) count

!       bins(  :,   :,    :,    :,   :)
!            var  rbin azbin elbin rad#

    integer(i_kind) nazbin,nrbin,nelbin
    integer(i_kind) i,ibyte,idate,inbufr,iret,isubset,krad,levs,lundx,n_gates
    integer(i_kind) k,ii,iii,j,jjj,numzzzz,num_radars_0
    integer(i_kind) iyref,imref,idref,ihref
    integer(i_kind) iazbin,irbin,ielbin
    integer(i_kind) nminref,nminthis
    integer(i_kind) num_radars,ireadsb,ireadmg,irec,isub,next
    integer(i_kind) idate5(5)
    real(r_double) hdr(14),rwnd(3,n_gates_max),rdisttest(n_gates_max)
    character(8) chdr
    equivalence (chdr,hdr(1))
    character(8) subset
    real(r_kind) delaz,delel,delr,t
    real(r_kind) ddiffmin,ddiffmin0,distfact,range
    integer(i_kind) idups,idups0
    character(4) stn_id_table(max_num_radars)
    character(4) stn_id_table_all(max_num_radars,0:npe-1)
    character(4) stn_id
    character(4*max_num_radars) cstn_id_table
    equivalence (stn_id_table(1),cstn_id_table)
    character(4) work_table(max_num_radars,0:npe-1)
    character(4) master_stn_table(max_num_radars)
    character(4*max_num_radars) cmaster_stn_table
    equivalence(master_stn_table(1),cmaster_stn_table)
    real(r_single) master_lat_table(max_num_radars)
    real(r_single) master_lon_table(max_num_radars)
    real(r_single) master_hgt_table(max_num_radars)
    real(r_single) stn_lat,stn_lon,stn_hgt,stn_az,stn_el
    real(r_single) stn_lat_table(max_num_radars),stn_lon_table(max_num_radars)
    real(r_single) stn_hgt_table(max_num_radars)
    real(r_single) stn_lat_table_all(max_num_radars,0:npe-1)
    real(r_single) stn_lon_table_all(max_num_radars,0:npe-1)
    real(r_single) stn_hgt_table_all(max_num_radars,0:npe-1)
    integer(i_kind) krad_map(max_num_radars)
    integer(i_kind),allocatable::histo_el(:)
    real(r_kind) timemax,timemin
    real(r_kind) timemax1,timemin1
    integer(i_kind) nradials_in,nradials_fail_angmax,nradials_fail_time,nradials_fail_elb
    integer(i_kind) nradials_in1,nradials_fail_angmax1,nradials_fail_time1,nradials_fail_elb1
    integer(i_kind) nobs_in,nobs_badvr,nobs_badsr,nobs_lrbin,nobs_hrbin,nrange_max
    integer(i_kind) nobs_in1,nobs_badvr1,nobs_badsr1,nobs_lrbin1,nobs_hrbin1,nrange_max1
    integer(i_kind) num_radars_max,num_radars_min
    integer(i_kind) loops_total
    real(r_single) this_stalat,this_stalon,this_stahgt
    real(r_kind) rlon0,clat0,slat0,rlonglob,rlatglob,clat1,caz0,saz0,cdlon,sdlon,caz1,saz1
    real(r_kind) this_stalatr,thisazimuthr,thistiltr
    real(r_single) thiscount,thisrange,thisazimuth,thistilt,thisvr,thisvr2
    real(r_single) corrected_tilt
    real(r_single) thiserr,thistime,thislat,thislon,corrected_azimuth
    real(r_single) rad_per_meter,thishgt
    real(r_kind) rlonloc,rlatloc
    real(r_single) a43,aactual,b,c,selev0,celev0,epsh,erad,h,ha
    real(r_single) celev,selev,gamma
    character(4) this_staid
    integer(i_kind) nsuper,nsuperall
    integer(i_kind) nthisbins
    real(r_single) vrmax,vrmin,errmax,errmin
    real(r_single) vrmaxall,vrminall,errmaxall,errminall
    real(r_single) delazmmax
    real(r_single) delazmmaxall
    real(r_single) deltiltmaxall,deltiltmax
    real(r_single) deltiltminall,deltiltmin
    real(r_single) deldistmaxall,deldistmax
    real(r_single) deldistminall,deldistmin
    logical rite
    character(10) date
    
    rad_per_meter= one/rearth
    erad = rearth


    nazbin=nint(360._r_kind/del_azimuth)
    nrbin=nint(range_max/del_range)
    nelbin=nint(elev_angle_max/del_elev)
    delaz=360._r_kind/nazbin
    delr=range_max/nrbin
    delel=elev_angle_max/nelbin
    allocate(bins(7,nrbin,nazbin,nelbin,max_num_radars))
    bins=zero
    num_radars=0
    do i=1,max_num_radars
       stn_id_table(i)='ZZZZ'
    end do
    stn_lat_table=99999._r_single
    stn_lon_table=99999._r_single
    stn_hgt_table=99999._r_single

    rite = .false.
    if (mype==0) rite=.true.
    
!   Open bufr file with openbf to initialize bufr table, etc in bufrlib
    inbufr=10
    open(inbufr,file='l2rwbufr',form='unformatted')
    read(inbufr,iostat=iret)subset
    if(iret.ne.0) then
       if(rite) write(6,*)'RADAR_BUFR_READ_ALL:  problem opening level 2 bufr file "l2rwbufr"'
       deallocate(bins)
       close(inbufr)                                       
       return
    end if
    rewind inbufr
    lundx=inbufr
    call openbf(inbufr,'IN',lundx)
    call datelen(10)
    if(l2superob_only) then
      write(date,'( i10)') idate
      read (date,'(i4,3i2)') iyref,imref,idref,ihref
      if(rite) write(6,*)' create superobs only, radar file date = ',iyref,imref,idref,ihref
    else
      iyref=iadate(1)              !????  add mods so can be used in global mode
      imref=iadate(2)
      idref=iadate(3)
      ihref=iadate(4)
      if(rite) write(6,*)' using restart file date = ',iyref,imref,idref,ihref
    end if
    if(rite) write(6,*)'RADAR_BUFR_READ_ALL: analysis time is ',iyref,imref,idref,ihref
    idate5(1)=iyref
    idate5(2)=imref
    idate5(3)=idref
    idate5(4)=ihref
    idate5(5)=0             ! minutes
    call w3fs21(idate5,nminref)

!    Do an initial read of a bit of data to infer what multiplying factor is for 
!    radial distance.  There is a possible ambiguity, where the scaling is either 
!    125 or 250. If the minimum difference between gate distances is 2, then factor 
!    is 125, if = 1 then factor is 250.

    idups=0
    ddiffmin=huge(ddiffmin)
    next=mype+1
    do while(ireadmg(inbufr,subset,idate)>=0)
    call ufbcnt(inbufr,irec,isub)
    if(irec/=next)cycle; next=next+npe
       read(subset,'(2x,i6)')isubset
       if(isubset.gt.6033) then
          iret=6034
          exit
       end if
       do while (ireadsb(inbufr).eq.0)
          call ufbint(inbufr,rdisttest,1,n_gates_max,n_gates,'DIST125M')
          if(n_gates.gt.1) then
             do i=1,n_gates-1
                if(nint(abs(rdisttest(i+1)-rdisttest(i))).eq.0) then
                   idups=idups+1
                else
                   ddiffmin=min(abs(rdisttest(i+1)-rdisttest(i)),ddiffmin)
                end if
             end do
          end if
       end do
    end do
    call mpi_barrier(mpi_comm_world,ierror)
    call mpi_allreduce(ddiffmin,ddiffmin0,1,mpi_real8,mpi_min,mpi_comm_world,ierror)
    call mpi_allreduce(idups,idups0,1,mpi_integer4,mpi_sum,mpi_comm_world,ierror)
    distfact=0
    if(nint(ddiffmin0).eq.1) distfact=250._r_kind
    if(nint(ddiffmin0).eq.2) distfact=125._r_kind
    if(distfact.eq.0) then
       write(6,*)'RADAR_BUFR_READ_ALL:  problem with level 2 bufr file, ',&
            'gate distance scale factor undetermined, going with 125'
       distfact=125.
    end if
    if(rite) write(6,*)'RADAR_BUFR_READ_ALL:  ddiffmin,distfact,idups=',&
         ddiffmin0,distfact,idups0

    timemax=-huge(timemax)
    timemin=huge(timemin)
    nradials_in=0
    nradials_fail_angmax=0
    nradials_fail_time=0
    nradials_fail_elb=0
    nobs_in=0
    nobs_badvr=0
    nobs_badsr=0
    nobs_lrbin=0
    nobs_hrbin=0
    nrange_max=0

! reopen and reread the file for data this time

    call closbf(inbufr)
    open(inbufr,file='l2rwbufr',form='unformatted')
    call openbf(inbufr,'IN',inbufr)

    next=mype+1
    do while(ireadmg(inbufr,subset,idate)>=0)
    call ufbcnt(inbufr,irec,isub)
    if(irec/=next)cycle; next=next+npe
          read(subset,'(2x,i6)')isubset
          if(isubset.gt.6033) then
             iret=6034
             exit
          end if
          do while (ireadsb(inbufr).eq.0)
             call ufbint(inbufr,hdr,14,1,levs, &
                  'SSTN YEAR MNTH DAYS HOUR MINU SECO CLAT CLON HSMSL HSALG ANAZ ANEL QCRW')
             nradials_in=nradials_in+1
             if(hdr(13).gt.elev_angle_max) then
                nradials_fail_angmax=nradials_fail_angmax+1
                cycle
             end if
             idate5(1)=nint(hdr(2)) ; idate5(2)=nint(hdr(3)) ; idate5(3)=nint(hdr(4))
             idate5(4)=nint(hdr(5)) ; idate5(5)=nint(hdr(6))
             call w3fs21(idate5,nminthis)
             t=(nminthis+(hdr(7)/60._r_single)-nminref)/60._r_single
             timemax=max(t,timemax)
             timemin=min(t,timemin)
             if(abs(t).gt.del_time) then
                nradials_fail_time=nradials_fail_time+1
                cycle
             end if
             call ufbint(inbufr,rwnd,3,n_gates_max,n_gates,'DIST125M DMVR DVSW')
             nobs_in=nobs_in+n_gates
             stn_az=90-hdr(12)
             stn_el=hdr(13)
             iazbin=stn_az/delaz
             iazbin=mod(iazbin,nazbin)
             if(iazbin.lt.0) iazbin=iazbin+nazbin
             iazbin=iazbin+1
             if(iazbin.le.0.or.iazbin.gt.nazbin) then
                write(6,*)'RADAR_BUFR_READ_ALL:  error in getting iazbin, program stops'
                call stop2(99)
             end if
             ielbin=ceiling(stn_el/delel)
             if(ielbin.lt.1.or.ielbin.gt.nelbin) then
                nradials_fail_elb=nradials_fail_elb+1
                cycle
             end if
             stn_id=chdr ; stn_lat=hdr(8)
             stn_lon=hdr(9)
             stn_hgt=hdr(10)+hdr(11)
             ibyte=index(cstn_id_table,stn_id)
             if(ibyte.eq.0) then
                num_radars=num_radars+1
                if(num_radars.gt.max_num_radars) then
                   write(6,*)'RADAR_BUFR_READ_ALL:  stop processing level 2 radar ',&
                        'bufr file--increase parameter max_num_radars'
                   call stop2(99)
                end if
                krad=num_radars
                stn_id_table(krad)=stn_id
                stn_lon_table(krad)=stn_lon
                stn_lat_table(krad)=stn_lat
                stn_hgt_table(krad)=stn_hgt
             else
                krad=1+(ibyte-1)/4
             end if
             
             do i=1,n_gates
                range=distfact*rwnd(1,i)
                if(range.gt.range_max) then
                   nrange_max=nrange_max+1
                   cycle
                end if
                if(rwnd(2,i).gt.1.e5) then
                   nobs_badvr=nobs_badvr+1
                   cycle
                end if
                if(rwnd(3,i).gt.1.e5) then
                   nobs_badsr=nobs_badsr+1
                   cycle
                end if
                irbin=ceiling(range/delr)
                if(irbin.lt.1) then
                   nobs_lrbin=nobs_lrbin+1
                   cycle
                end if
                if(irbin.gt.nrbin) then
                   nobs_hrbin=nobs_hrbin+1
                   cycle
                end if
                bins(1,irbin,iazbin,ielbin,krad)=bins(1,irbin,iazbin,ielbin,krad)+range
                bins(2,irbin,iazbin,ielbin,krad)=bins(2,irbin,iazbin,ielbin,krad)+stn_az
                bins(3,irbin,iazbin,ielbin,krad)=bins(3,irbin,iazbin,ielbin,krad)+stn_el
                bins(4,irbin,iazbin,ielbin,krad)=bins(4,irbin,iazbin,ielbin,krad)+rwnd(2,i)
                bins(5,irbin,iazbin,ielbin,krad)=bins(5,irbin,iazbin,ielbin,krad)+rwnd(2,i)**2
                bins(6,irbin,iazbin,ielbin,krad)=bins(6,irbin,iazbin,ielbin,krad)+t
                bins(7,irbin,iazbin,ielbin,krad)=bins(7,irbin,iazbin,ielbin,krad)+one
             end do
             
          end do          !  end do while
    end do              !  loop over blocks
    call closbf(inbufr)
    call mpi_barrier(mpi_comm_world,ierror)
    call mpi_allreduce(num_radars,num_radars_max,1,&
         mpi_integer4,mpi_max,mpi_comm_world,ierror)
    if(num_radars_max.le.0) then
       if(rite) write(6,*)'RADAR_BUFR_READ_ALL:  NO RADARS KEPT IN radar_bufr_read_all, ',&
            'continue without level 2 data'
       return
    end if
    call mpi_reduce(nradials_in,nradials_in1,1,mpi_integer4,mpi_sum,0,mpi_comm_world,ierror)
    call mpi_reduce(nradials_fail_angmax,nradials_fail_angmax1,1,&
         mpi_integer4,mpi_sum,0,mpi_comm_world,ierror)
    call mpi_reduce(nradials_fail_time,nradials_fail_time1,1,&
         mpi_integer4,mpi_sum,0,mpi_comm_world,ierror)
    call mpi_reduce(nradials_fail_elb,nradials_fail_elb1,1,&
         mpi_integer4,mpi_sum,0,mpi_comm_world,ierror)
    call mpi_reduce(nobs_in,nobs_in1,1,mpi_integer4,mpi_sum,0,mpi_comm_world,ierror)
    call mpi_reduce(nobs_badvr,nobs_badvr1,1,mpi_integer4,mpi_sum,0,mpi_comm_world,ierror)
    call mpi_reduce(nobs_badsr,nobs_badsr1,1,mpi_integer4,mpi_sum,0,mpi_comm_world,ierror)
    call mpi_reduce(nobs_lrbin,nobs_lrbin1,1,mpi_integer4,mpi_sum,0,mpi_comm_world,ierror)
    call mpi_reduce(nobs_hrbin,nobs_hrbin1,1,mpi_integer4,mpi_sum,0,mpi_comm_world,ierror)
    call mpi_reduce(nrange_max,nrange_max1,1,mpi_integer4,mpi_sum,0,mpi_comm_world,ierror)
    call mpi_reduce(timemax,timemax1,1,mpi_real8,mpi_max,0,mpi_comm_world,ierror)
    call mpi_reduce(timemin,timemin1,1,mpi_real8,mpi_min,0,mpi_comm_world,ierror)
    
!  Create master station list

!  First gather all stn id and lat,lon,hgt lists
    call mpi_allgather(stn_id_table,max_num_radars,mpi_integer4, &
         stn_id_table_all,max_num_radars,mpi_integer4,mpi_comm_world,ierror)
    call mpi_allgather(stn_lat_table,max_num_radars,mpi_real4, &
         stn_lat_table_all,max_num_radars,mpi_real4,mpi_comm_world,ierror)
    call mpi_allgather(stn_lon_table,max_num_radars,mpi_real4, &
         stn_lon_table_all,max_num_radars,mpi_real4,mpi_comm_world,ierror)
    call mpi_allgather(stn_hgt_table,max_num_radars,mpi_real4, &
         stn_hgt_table_all,max_num_radars,mpi_real4,mpi_comm_world,ierror)

!   Create unique master list of all radar names,lats,lons
    do j=0,npe-1
       do i=1,max_num_radars
          work_table(i,j)=stn_id_table_all(i,j)
       end do
    end do
    ii=0
    loops_total=0
    outer: do
       do j=0,npe-1
          do i=1,max_num_radars
             if(work_table(i,j).ne.'ZZZZ') then
                ii=ii+1
                if(ii.gt.max_num_radars) then
                   write(6,*)'RADAR_BUFR_READ_ALL:  stop processing level 2 radar ',&
                        'bufr file--increase parameter max_num_radars'
                   call stop2(99)
                end if
                master_stn_table(ii)=work_table(i,j)
                master_lat_table(ii)=stn_lat_table_all(i,j)
                master_lon_table(ii)=stn_lon_table_all(i,j)
                master_hgt_table(ii)=stn_hgt_table_all(i,j)
                work_table(i,j)='ZZZZ'
                numzzzz=0
                do jjj=0,npe-1
                   do iii=1,max_num_radars
                      if(work_table(iii,jjj).eq.master_stn_table(ii)) work_table(iii,jjj)='ZZZZ'
                      if(work_table(iii,jjj).eq.'ZZZZ') numzzzz=numzzzz+1
                      loops_total=loops_total+1
                   end do
                end do
                if(numzzzz.eq.max_num_radars*npe) exit outer
             end if
          end do
       end do
    end do outer
    num_radars_0=ii
    write(6,*)'RADAR_BUFR_READ_ALL:  num_radars_0,loops_total = ',num_radars_0,loops_total
    if(rite) then
       do i=1,num_radars_0
          write(6,'(" master list radar ",i3," stn id,lat,lon,hgt = ",a4,2f10.2,f8.1)') &
               i,master_stn_table(i),master_lat_table(i),master_lon_table(i)
       end do
    end if

!   Reorganize entries in bins based on master list
    do krad=1,num_radars
       ibyte=index(cmaster_stn_table,stn_id_table(krad))
       if(ibyte.eq.0) then
          write(6,*)'RADAR_BUFR_READ_ALL:  impossible place to be, ',&
               'problem with master radar table'
          call stop2(99)
       else
          krad_map(krad)=1+(ibyte-1)/4
       end if
    end do
    do ielbin=1,nelbin
       do iazbin=1,nazbin
          do irbin=1,nrbin
             do krad=1,num_radars_0
                bins_work(1,krad)=zero ; bins_work(2,krad)=zero ; bins_work(3,krad)=zero
                bins_work(4,krad)=zero ; bins_work(5,krad)=zero ; bins_work(6,krad)=zero
                bins_work(7,krad)=zero
             end do
             do krad=1,num_radars
                bins_work(1,krad_map(krad))=bins(1,irbin,iazbin,ielbin,krad)
                bins_work(2,krad_map(krad))=bins(2,irbin,iazbin,ielbin,krad)
                bins_work(3,krad_map(krad))=bins(3,irbin,iazbin,ielbin,krad)
                bins_work(4,krad_map(krad))=bins(4,irbin,iazbin,ielbin,krad)
                bins_work(5,krad_map(krad))=bins(5,irbin,iazbin,ielbin,krad)
                bins_work(6,krad_map(krad))=bins(6,irbin,iazbin,ielbin,krad)
                bins_work(7,krad_map(krad))=bins(7,irbin,iazbin,ielbin,krad)
             end do
             do krad=1,num_radars_0
                bins(1,irbin,iazbin,ielbin,krad)=bins_work(1,krad)
                bins(2,irbin,iazbin,ielbin,krad)=bins_work(2,krad)
                bins(3,irbin,iazbin,ielbin,krad)=bins_work(3,krad)
                bins(4,irbin,iazbin,ielbin,krad)=bins_work(4,krad)
                bins(5,irbin,iazbin,ielbin,krad)=bins_work(5,krad)
                bins(6,irbin,iazbin,ielbin,krad)=bins_work(6,krad)
                bins(7,irbin,iazbin,ielbin,krad)=bins_work(7,krad)
             end do
          end do
       end do
    end do
    
    call mpi_reduce(num_radars,num_radars_max,1,mpi_integer4,mpi_max,0,mpi_comm_world,ierror)
    call mpi_reduce(num_radars,num_radars_min,1,mpi_integer4,mpi_min,0,mpi_comm_world,ierror)
    if(mype.eq.0) write(6,*)' min,max num_radars=',num_radars_min,num_radars_max
    nthisbins=7*nrbin*nazbin*nelbin
    if(rite) write(6,*)' nthisbins=',nthisbins
    
    if(mype.eq.0) allocate(bin0all(7,nrbin,nazbin,nelbin,0:npe-1))
    do krad=1,num_radars_0
       call mpi_gather(bins(1,1,1,1,krad),nthisbins,mpi_real4, &
            bin0all,nthisbins,mpi_real4,0,mpi_comm_world,ierror)
       if(mype.eq.0) then
          do ielbin=1,nelbin
             do iazbin=1,nazbin
                do irbin=1,nrbin
                   do i=1,7
                      bins(i,irbin,iazbin,ielbin,krad)=bin0all(i,irbin,iazbin,ielbin,0)
                   end do
                end do
             end do
          end do
          if(npe.gt.1) then
             do k=1,npe-1
                do ielbin=1,nelbin
                   do iazbin=1,nazbin
                      do irbin=1,nrbin
                         do i=1,7
                            bins(i,irbin,iazbin,ielbin,krad)= &
                                 bins(i,irbin,iazbin,ielbin,krad)+ &
                                 bin0all(i,irbin,iazbin,ielbin,k)
                         end do
                      end do
                   end do
                end do
             end do
          end if
       end if
    end do
    if(mype.eq.0) deallocate(bin0all)
    
!   Print out histogram of counts by ielbin to see where angles are
    if(rite) then
       write(6,*)' timemin,max=',timemin1,timemax1
       write(6,*)' nradials_in=',nradials_in1
       write(6,*)' nradials_fail_angmax=',nradials_fail_angmax1
       write(6,*)' nradials_fail_time=',nradials_fail_time1
       write(6,*)' nradials_fail_elb=',nradials_fail_elb1
       write(6,*)' nobs_in=',nobs_in1
       write(6,*)' nobs_badvr=',nobs_badvr1
       write(6,*)' nobs_badsr=',nobs_badsr1
       write(6,*)' nobs_lrbin=',nobs_lrbin1
       write(6,*)' nobs_hrbin=',nobs_hrbin1
       write(6,*)' nrange_max=',nrange_max1
       allocate(histo_el(nelbin))
       histo_el=0
       do krad=1,num_radars_0
          do ielbin=1,nelbin
             do iazbin=1,nazbin
                do irbin=1,nrbin
                   histo_el(ielbin)=histo_el(ielbin)+bins(7,irbin,iazbin,ielbin,krad)
                end do
             end do
          end do
       end do
       do ielbin=1,nelbin
          write(6,'(" ielbin,histo_el=",i6,i20)')ielbin,histo_el(ielbin)
       end do
       deallocate(histo_el)
    end if

!   Create superobs and write out.
    if(mype.eq.0) then
       open(inbufr,file='radar_supobs_from_level2',form='unformatted',iostat=iret)
       rewind inbufr
       nsuperall=0
       vrmaxall=-huge(vrmaxall)
       vrminall=huge(vrminall)
       errmaxall=-huge(errmaxall)
       errminall=huge(errminall)
       delazmmaxall=-huge(delazmmaxall)
       deltiltmaxall=-huge(deltiltmaxall)
       deldistmaxall=-huge(deldistmaxall)
       deltiltminall=huge(deltiltminall)
       deldistminall=huge(deldistminall)
       do krad=1,num_radars_0
          nsuper=0
          vrmax=-huge(vrmax)
          vrmin=huge(vrmin)
          errmax=-huge(errmax)
          errmin=huge(errmin)
          delazmmax=-huge(delazmmax)
          deltiltmax=-huge(deltiltmax)
          deldistmax=-huge(deldistmax)
          deltiltmin=huge(deltiltmin)
          deldistmin=huge(deldistmin)
          this_stalat=master_lat_table(krad)
          if(abs(this_stalat).gt.89.5) cycle
          this_stalon=master_lon_table(krad)
          rlon0=deg2rad*this_stalon
          this_stalatr=this_stalat*deg2rad
          clat0=cos(this_stalatr) ; slat0=sin(this_stalatr)
          this_staid=master_stn_table(krad)
          this_stahgt=master_hgt_table(krad)
          do ielbin=1,nelbin
             do iazbin=1,nazbin
                do irbin=1,nrbin
                   thiscount=bins(7,irbin,iazbin,ielbin,krad)
                   if(nint(thiscount).lt.minnum) cycle
                   thisrange=  bins(1,irbin,iazbin,ielbin,krad)/thiscount
                   thisazimuth=bins(2,irbin,iazbin,ielbin,krad)/thiscount
                   thistilt=bins(3,irbin,iazbin,ielbin,krad)/thiscount
                   thisvr=bins(4,irbin,iazbin,ielbin,krad)/thiscount
                   vrmax=max(vrmax,thisvr)
                   vrmin=min(vrmin,thisvr)
                   thisvr2=bins(5,irbin,iazbin,ielbin,krad)/thiscount
                   thiserr=sqrt(abs(thisvr2-thisvr**2))
                   errmax=max(errmax,thiserr)
                   errmin=min(errmin,thiserr)
                   thistime=bins(6,irbin,iazbin,ielbin,krad)/thiscount
                   
!                  Keep away from poles, rather than properly deal with polar singularity
                   if(abs(thislat).gt.89.5_r_single) cycle

!                  Compute obs height here
!                  Use 4/3rds rule to get elevation of radar beam
!                  (if local temperature, moisture available, then vertical position 
!                   might be estimated with greater accuracy by ray tracing )

                   aactual=erad+this_stahgt
                   a43=4*aactual/3
                   thistiltr=thistilt*deg2rad
                   selev0=sin(thistiltr)
                   celev0=cos(thistiltr)
                   b=thisrange*(thisrange+two*aactual*selev0)
                   c=sqrt(aactual*aactual+b)
                   ha=b/(aactual+c)
                   epsh=(thisrange*thisrange-ha*ha)/(8._r_single*aactual)
                   h=ha-epsh
                   thishgt=this_stahgt+h
                   
!                  Get corrected tilt angle
                   celev=celev0
                   selev=selev0
                   if(thisrange.ge.1) then
                      celev=a43*celev0/(a43+h)
                      selev=(thisrange*thisrange+h*h+two*a43*h)/(two*thisrange*(a43+h))
                   end if
                   corrected_tilt=atan2(selev,celev)*rad2deg
                   deltiltmax=max(corrected_tilt-thistilt,deltiltmax)
                   deltiltmin=min(corrected_tilt-thistilt,deltiltmin)
                   gamma=half*thisrange*(celev0+celev)
                   deldistmax=max(gamma-thisrange,deldistmax)
                   deldistmin=min(gamma-thisrange,deldistmin)

!                  Get earth lat lon of superob
                   thisazimuthr=thisazimuth*deg2rad
                   rlonloc=rad_per_meter*gamma*cos(thisazimuthr)
                   rlatloc=rad_per_meter*gamma*sin(thisazimuthr)
                   call invtllv(rlonloc,rlatloc,rlon0,clat0,slat0,rlonglob,rlatglob)
                   thislat=rlatglob*rad2deg
                   thislon=rlonglob*rad2deg

!                  Get corrected azimuth
                   clat1=cos(rlatglob)
                   caz0=cos(thisazimuthr)
                   saz0=sin(thisazimuthr)
                   cdlon=cos(rlonglob-rlon0)
                   sdlon=sin(rlonglob-rlon0)
                   caz1=clat0*caz0/clat1
                   saz1=saz0*cdlon-caz0*sdlon*slat0
                   corrected_azimuth=atan2(saz1,caz1)*rad2deg
                   delazmmax=max(min(abs(corrected_azimuth-thisazimuth-720.),&
                        abs(corrected_azimuth-thisazimuth-360.),&
                        abs(corrected_azimuth-thisazimuth     ),&
                        abs(corrected_azimuth-thisazimuth+360.),&
                        abs(corrected_azimuth-thisazimuth+720.)),delazmmax)

                   write(inbufr) this_staid,this_stalat,this_stalon,this_stahgt, &
                        thistime,thislat,thislon,thishgt,thisvr,corrected_azimuth,&
                        thiserr,corrected_tilt
                   nsuper=nsuper+1
                end do
             end do
          end do
          write(6,*)' for radar ',this_staid,' nsuper=',nsuper
          write(6,*)'  vrmin,max=',vrmin,vrmax
          write(6,*)' errmin,max=',errmin,errmax
          write(6,*)' delazmmax=',delazmmax
          write(6,*)' deltiltmin,max=',deltiltmin,deltiltmax
          write(6,*)' deldistmin,max=',deldistmin,deldistmax
          vrminall=min(vrminall,vrmin)
          vrmaxall=max(vrmaxall,vrmax)
          errminall=min(errminall,errmin)
          errmaxall=max(errmaxall,errmax)
          delazmmaxall=max(delazmmaxall,delazmmax)
          deltiltmaxall=max(deltiltmaxall,deltiltmax)
          deldistmaxall=max(deldistmaxall,deldistmax)
          deltiltminall=min(deltiltminall,deltiltmin)
          deldistminall=min(deldistminall,deldistmin)
          nsuperall=nsuperall+nsuper
       end do
       close(inbufr)
       write(6,*)' total number of superobs written=',nsuperall
       write(6,*)'  vrmin,maxall=',vrminall,vrmaxall
       write(6,*)' errmin,maxall=',errminall,errmaxall
       write(6,*)' delazmmaxall=',delazmmaxall
       write(6,*)' deltiltmin,maxall=',deltiltminall,deltiltmaxall
       write(6,*)' deldistmin,maxall=',deldistminall,deldistmaxall
    end if
    deallocate(bins)
          if(l2superob_only) then
             call mpi_finalize(ierror)
             stop
          end if

  end subroutine radar_bufr_read_all

end module read_l2bufr_mod

SUBROUTINE tllv(ALM,APH,TLMO,CTPH0,STPH0,TLM,TPH)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    tllv             
!
!   prgrmmr:
!
! abstract:
!
! program history log:
!   2008-04-21  safford -- add subprogram doc block, rm unused uses
!
!   input argument list:
!     alm   -- input earth longitude
!     aph   -- input earth latitude
!     tlmo  -- input earth longitude of rotated grid origin (radrees)
!     ctph0 -- cos(earth lat of rotated grid origin)
!     stph0 -- sin(earth lat of rotated grid origin)
!
!   output argument list:
!     tlm   -- rotated grid longitude
!     tph   -- rotated grid latitude
!
! attributes:
!   language:  f90
!   machine:
!
!$$$ end documentation block

  use kinds, only:  r_kind
  implicit none
  real(r_kind),intent(in):: alm,aph,tlmo,ctph0,stph0
  real(r_kind),intent(out):: tlm,tph
  real(r_kind):: relm,srlm,crlm,sph,cph,cc,anum,denom

  RELM=ALM-TLMO
  SRLM=SIN(RELM)
  CRLM=COS(RELM)
  SPH=SIN(APH)
  CPH=COS(APH)
  CC=CPH*CRLM
  ANUM=CPH*SRLM
  DENOM=CTPH0*CC+STPH0*SPH
  TLM=ATAN2(ANUM,DENOM)
  TPH=ASIN(CTPH0*SPH-STPH0*CC)

END SUBROUTINE tllv

SUBROUTINE invtllv(ALM,APH,TLMO,CTPH0,STPH0,TLM,TPH)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    invtllv
!
!   prgrmmr:
!
! abstract:  inverse of tllv:  input ALM,APH is rotated lon,lat
!                   output is earth lon,lat, TLM,TPH
!
! program history log:
!   2008-03-25  safford -- add subprogram doc block, rm unused uses
!
!   input argument list:
!     alm   -- input earth longitude
!     aph   -- input earth latitude
!     tlmo  -- input earth longitude of rotated grid origin (radrees)
!     ctph0 -- cos(earth lat of rotated grid origin)
!     stph0 -- sin(earth lat of rotated grid origin)
!
!   output argument list:
!     tlm   -- rotated grid longitude
!     tph   -- rotated grid latitude
!
! attributes:
!   language:  f90
!   machine:
!
!$$$ end documentation block

  use kinds, only:  r_kind
  implicit none

  real(r_kind),intent(in):: alm,aph,tlmo,ctph0,stph0
  real(r_kind),intent(out):: tlm,tph
  real(r_kind):: relm,srlm,crlm,sph,cph,cc,anum,denom

  RELM=ALM
  SRLM=SIN(RELM)
  CRLM=COS(RELM)
  SPH=SIN(APH)
  CPH=COS(APH)
  CC=CPH*CRLM
  ANUM=CPH*SRLM
  DENOM=CTPH0*CC-STPH0*SPH
  TLM=tlmo+ATAN2(ANUM,DENOM)
  TPH=ASIN(CTPH0*SPH+STPH0*CC)
  
END SUBROUTINE invtllv
