subroutine read_satwnd(nread,ndata,nodata,infile,obstype,lunout,gstime,twind,sis,&
     prsl_full)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    read_satwnd                    read satellite winds  
!   prgmmr: su, xiujuan      org: np23                date: 2010-10-13
!
! abstract:  This routine reads satellite winds from satellite wind dump.  
!            it also has options to thin the data by using conventional 
!            thinning programs 
!            When running the gsi in regional mode, the code only
!            retains those observations that fall within the regional
!            domain
!
! program history log:
!   2010-10-13 su, x.  
!   2011-08-09 pondeca - add support for twodvar option
!   2011-08-27 todling - bypass this routine when SATWND from prepbufr are used
!   2011-10-03 su      - read AVHRR wind into system and modify satellite id range 
!                        and put subset as screen criterie since AVHRR from 
!                        different satellites
!   2011-10-24         -add reading observation error in this subroutine and stop
!                       statement if no prepbufr error table available.
!   2011-12-08 Su      -modify GOES reading program for new bufrtab.005 new format, reading        
!                       SDM quality mark 
!   2011-12-20 Su      -modify to read deep layer WV winds as monitor with qm=9,considering short 
!                       wave winds as subset 1 0f 245         
!
!   input argument list:
!     ithin    - flag to thin data
!     rmesh    - thinning mesh size (km)
!     gstime   - analysis time in minutes from reference date
!     infile   - unit from which to read BUFR data
!     lunout   - unit to which to write data for further processing
!     obstype  - observation type to process
!     twind    - input group time window (hours)
!     sis      - satellite/instrument/sensor indicator
!
!   output argument list:
!     nread    - number of satellite winds read 
!     ndata    - number of satellite winds retained for further processing
!     nodata   - number of satellite winds retained for further processing
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,r_double,i_kind,r_single
  use gridmod, only: diagnostic_reg,regional,nlon,nlat,nsig,&
       tll2xy,txy2ll,rotate_wind_ll2xy,rotate_wind_xy2ll,&
       rlats,rlons,twodvar_regional
  use qcmod, only: errormod,noiqc
  use convthin, only: make3grids,map3grids,del3grids,use_all
  use constants, only: deg2rad,zero,rad2deg,one_tenth,&
        tiny_r_kind,huge_r_kind,r60inv,one_tenth,&
        one,two,three,four,five,half,quarter,r60inv,r100,r2000
!  use converr,only: etabl
  use obsmod, only: iadate,oberrflg,perturb_obs,perturb_fact,ran01dom,bmiss
  use convinfo, only: nconvtype,ctwind, &
       ncmiter,ncgroup,ncnumgrp,icuse,ictype,icsubtype,ioctype, &
       ithin_conv,rmesh_conv,pmesh_conv, &
       id_bias_ps,id_bias_t,conv_bias_ps,conv_bias_t,use_prepb_satwnd
  use gsi_4dvar, only: l4dvar,iwinbgn,winlen,time_4dvar
  use deter_sfc_mod, only: deter_sfc_type,deter_sfc2
  implicit none

! Declare passed variables
  character(len=*)                      ,intent(in   ) :: infile,obstype
  character(len=*)                      ,intent(in   ) :: sis
  integer(i_kind)                       ,intent(in   ) :: lunout
  integer(i_kind)                       ,intent(inout) :: nread,ndata,nodata
  real(r_kind)                          ,intent(in   ) :: twind
  real(r_kind),dimension(nlat,nlon,nsig),intent(in   ) :: prsl_full

! Declare local parameters

  integer(i_kind),parameter:: mxtb=5000000
  integer(i_kind),parameter:: nmsgmax=10000 ! max message count
  real(r_kind),parameter:: r1_2= 1.2_r_kind
  real(r_kind),parameter:: r3_33= 3.33_r_kind
  real(r_kind),parameter:: r6= 6.0_r_kind
  real(r_kind),parameter:: r50= 50.0_r_kind
  real(r_kind),parameter:: r54= 54.0_r_kind
  real(r_kind),parameter:: r55= 55.0_r_kind
  real(r_kind),parameter:: r56= 56.0_r_kind
  real(r_kind),parameter:: r70= 70.0_r_kind
  real(r_kind),parameter:: r85= 85.0_r_kind
  real(r_kind),parameter:: r90= 90.0_r_kind
  real(r_kind),parameter:: r105= 105.0_r_kind
  real(r_kind),parameter:: r110= 110.0_r_kind
  real(r_kind),parameter:: r125=125.0_r_kind
  real(r_kind),parameter:: r200=200.0_r_kind
  real(r_kind),parameter:: r250=250.0_r_kind
  real(r_kind),parameter:: r360 = 360.0_r_kind
  real(r_kind),parameter:: r600=600.0_r_kind
  real(r_kind),parameter:: r700=700.0_r_kind
  real(r_kind),parameter:: r199=199.0_r_kind
  real(r_kind),parameter:: r299=299.0_r_kind
  real(r_kind),parameter:: r799=799.0_r_kind
  real(r_kind),parameter:: r1200= 1200.0_r_kind
  real(r_kind),parameter:: r10000= 10000.0_r_kind
  
  

! Declare local variables
  logical outside,inflate_error
  logical asort
  logical luse,ithinp
  logical,allocatable,dimension(:,:):: lmsg     ! set true when convinfo entry id found in a message

  character(70) obstr,hdrtr
  character(50) satqctr,qcstr
  character(8) subset
  character(20) derdwtr,heightr
  character(8) c_prvstg,c_sprvstg
  character(8) c_station_id

  integer(i_kind) ireadmg,ireadsb,iuse
  integer(i_kind) i,maxobs,idomsfc,itemp,nsattype
  integer(i_kind) nc,nx,id,isflg,itx,j,nchanl
  integer(i_kind) ntb,ntmatch,ncx,ncsave,ntread
  integer(i_kind) kk,klon1,klat1,klonp1,klatp1
  integer(i_kind) nmind,lunin,idate,ilat,ilon,iret,k
  integer(i_kind) nreal,ithin,iout,ntmp,icount,iiout,icntpnt,ii,icntpnt2
  integer(i_kind) itype,iosub,ixsub,isubsub,iobsub 
  integer(i_kind) pqm,qm,lim_qm
  integer(i_kind) nlevp         ! vertical level for thinning
  integer(i_kind) pflag
  integer(i_kind) ntest,nvtest
  integer(i_kind) kl,k1,k2
  integer(i_kind) nmsg                ! message index
  integer(i_kind) tab(mxtb,3)
  
  
 
  integer(i_kind),dimension(nconvtype) :: ntxall 
  integer(i_kind),dimension(nconvtype+1) :: ntx  
  
  integer(i_kind),dimension(5):: idate5 
  integer(i_kind),dimension(nmsgmax):: nrep
  integer(i_kind),allocatable,dimension(:):: isort,iloc

  integer(i_kind) ietabl,itypex,lcount,iflag,m

  real(r_single),allocatable,dimension(:,:,:) :: etabl

  real(r_kind) toff,t4dv
  real(r_kind) rmesh,ediff,usage,tdiff
  real(r_kind) u0,v0,uob,vob,dx,dy,dx1,dy1,w00,w10,w01,w11
  real(r_kind) dlnpob,ppb,ppb2,qifn,qify,ee
  real(r_kind) woe,errout,dlat,dlon,dlat_earth,dlon_earth
  real(r_kind) cdist,disterr,disterrmax,rlon00,rlat00
  real(r_kind) vdisterrmax,u00,v00,u01,v01,uob1,vob1
  real(r_kind) del,werrmin,obserr,ppb1
  real(r_kind) tsavg,ff10,sfcr,sstime,gstime,zz
  real(r_kind) crit1,timedif,xmesh,pmesh
  real(r_kind),dimension(nsig):: presl
  real(r_kind),dimension(nsig-1):: dpres
  
  real(r_kind),dimension(22) :: ctwind_s,rmesh_conv_s,pmesh_conv_s
  real(r_double),dimension(13):: hdrdat
  real(r_double),dimension(4):: obsdat,satqc
  real(r_double),dimension(3,5) :: heightdat
  real(r_double),dimension(6,4) :: derdwdat
  real(r_double),dimension(3,12) :: qcdat
  real(r_double),dimension(1,1):: r_prvstg,r_sprvstg
  real(r_kind),allocatable,dimension(:):: presl_thin
  real(r_kind),allocatable,dimension(:,:):: cdata_all,cdata_out

  real(r_double) rstation_id

! equivalence to handle character names
  equivalence(r_prvstg(1,1),c_prvstg)
  equivalence(r_sprvstg(1,1),c_sprvstg)
  equivalence(rstation_id,c_station_id)

  data hdrtr /'SAID CLAT CLON YEAR MNTH DAYS HOUR MINU SWCM SAZA GCLONG SCCF SWQM'/ 
  data obstr/'HAMD PRLC WDIR WSPD'/ 
  data heightr/'MDPT '/ 
  data derdwtr/'TWIND'/
  data satqctr/'RFFL EEQF QIFN QIFY'/
  data qcstr /' OGCE GNAP PCCF'/

  
  
  data ithin / -9 /
  data lunin / 11 /
  data rmesh / -99.999_r_kind /

!**************************************************************************

! Return when SATWND are coming from prepbufr file
  if(use_prepb_satwnd) return

! read observation error table

  allocate(etabl(300,33,6))
  etabl=1.e9_r_kind
  ietabl=19
  open(ietabl,file='errtable',form='formatted')
  rewind ietabl
  etabl=1.e9_r_kind
  lcount=0
  loopd : do
     read(ietabl,100,IOSTAT=iflag) itypex
     if( iflag /= 0 ) exit loopd
     lcount=lcount+1
     do k=1,33
        read(ietabl,110)(etabl(itypex,k,m),m=1,6)
     end do
  end do   loopd
100     format(1x,i3)
110        format(1x,6e12.5)
  if(lcount<=0 ) then
     write(6,*)'READ_SATWND:obs error table not available to 3dvar. the program will stop'
     call stop2(49) 
  else
     write(6,*)'READ_SATWND:  observation errors provided by local file errtable'
  endif

  close(ietabl)

! Set lower limits for observation errors
  werrmin=one
  nsattype=0
  nreal=24
  if(perturb_obs ) nreal=nreal+2
  if (noiqc) then
     lim_qm=8
  else
     lim_qm=4
  endif
  ntread=1
  ntmatch=0
  ntx(ntread)=0
  ntxall=0
  do nc=1,nconvtype
     if(trim(ioctype(nc)) == 'uv' .and. ictype(nc) >=241 &
             .and. ictype(nc) <260) then
        ntmatch=ntmatch+1
        ntxall(ntmatch)=nc
        ithin=ithin_conv(nc)
        if(ithin > 0)then
           ntread=ntread+1
           ntx(ntread)=nc
        end if
     end if
  end do
  if(ntmatch == 0)then
     write(6,*) ' READ_SATWND: no matching obstype found in obsinfo ',obstype
     return
  end if
      
!!  go through the satedump to find out how many subset to process

  allocate(lmsg(nmsgmax,ntread))
  lmsg = .false.
  maxobs=0
  tab=0
  nmsg=0
  nrep=0
  ntb =0
  call closbf(lunin)
  open(lunin,file=infile,form='unformatted')
  call openbf(lunin,'IN',lunin)
  call datelen(10)
  
  msg_report: do while (ireadmg(lunin,subset,idate) == 0)
!    if(trim(subset) == 'NC005012') cycle msg_report 

!    Time offset
     if(nmsg == 0) call time_4dvar(idate,toff)
     nmsg=nmsg+1
     if (nmsg>nmsgmax) then
        write(6,*)'READ_SATWND: messages exceed maximum ',nmsgmax
        call stop2(49)
     endif
     loop_report: do while (ireadsb(lunin) == 0)
        ntb = ntb+1
        maxobs=maxobs+1
        nrep(nmsg)=nrep(nmsg)+1
        if (ntb>mxtb) then
           write(6,*)'READ_SATWND: reports exceed maximum ',mxtb   
           call stop2(49)
        endif
!       Extract type information
        call ufbint(lunin,hdrdat,13,1,iret,hdrtr)
!       determine the satellite wind type as in prepbufr
!       241: India, 242:JMA Visible,243: EUMETSAT visible
!       245: GOES IR. 246: GOES WV cloud top, 247: GOES WV deep layer
!       250: JMA WV deep layer. 251:GOES visible, 252: JMA IR
!       253: EUMETSAT IR , 254: EUMETSAT WV deep layer
!       257: MODIS IR, 258: WV cloud top, 259:  WV deep layer
        iobsub=0
        itype=-1
        if(trim(subset) == 'NC005064' .or. trim(subset) == 'NC005065' .or. &
           trim(subset) == 'NC005066') then
           if( hdrdat(1) <r70 .and. hdrdat(1) >= r50) then          !     EUMETSAT wind
              if(hdrdat(1) == r55) iobsub=55
              if(hdrdat(1) == r56) iobsub=56
              if(hdrdat(9) == one)  then                  ! IR winds
                 itype=253
              else if(hdrdat(9) == two) then              ! visible winds
                 itype=243
              else if(hdrdat(9) == three) then            ! WV cloud top, try to assimilate
                 itype=254
              else if(hdrdat(9) >= four) then             ! WV deep layer,discard
!                 cycle loop_report
                itype=254
              endif
           endif
        else if(trim(subset) == 'NC005044' .or. trim(subset) == 'NC005045' .or. &
           trim(subset) == 'NC005046') then
           if( hdrdat(1) >=r100 .and. hdrdat(1) <=r199 ) then    ! JMA
              if(hdrdat(9) == one)  then                            ! IR winds
                 itype=252
              else if(hdrdat(9) == two) then                        ! visible winds
                 itype=242
              else if(hdrdat(9) == three) then                      ! WV cloud top
                 itype=250
              else if(hdrdat(9) >= four) then                       ! WV deep layer.discard
                 itype=250
              endif
           endif
        else if(trim(subset) == 'NC005010' .or. trim(subset) == 'NC005011' .or. &
           trim(subset) == 'NC005012' ) then
           if( hdrdat(1) >=r250 .and. hdrdat(1) <=r299 ) then  ! NESDIS GOES 
              if(hdrdat(1) == 259.0_r_kind) iobsub=15
              if(hdrdat(9) == one)  then                            ! IR winds
                 if(hdrdat(12) <50000000000000.0_r_kind) then
                    itype=245
                 else
                    itype=245
                    iobsub=1
                 endif
              else if(hdrdat(9) == two  ) then    ! visible winds
                 itype=251
              else if(hdrdat(9) == three ) then   ! WV cloud top
                 itype=246
              else if(hdrdat(9) >= four ) then    ! WV deep layer.discard
                 itype=247
              endif
           endif
        else if(trim(subset) == 'NC005070' .or. trim(subset) == 'NC005071'  ) then
           if( hdrdat(1) >=r700 .and. hdrdat(1) <= r799 ) then      ! MODIS
              if(hdrdat(9) == one)  then                            ! IR winds
                 itype=257
              else if(hdrdat(9) == three) then                      ! WV cloud top
                 itype=258
              else if(hdrdat(9) >= four) then                       ! WV deep layer
                 itype=259
              endif
           endif
        else if( trim(subset) == 'NC005080') then                   ! AVHRR 
           if( hdrdat(1) <10.0_r_kind .or. (hdrdat(1) >= 200.0_r_kind .and. &
               hdrdat(1) <=223.0_r_kind) ) then      
              if(hdrdat(9) == one)  then                            ! IR winds
                 itype=244
              else
                 write(6,*) 'READ_SATWND: wrong derived method value'
              endif
           endif
        endif
!  Match ob to proper convinfo type
        ncsave=0
        matchloop:do ncx=1,ntmatch
           nc=ntxall(ncx)
           if (itype /= ictype(nc)) cycle matchloop
!  Find convtype which match ob type and subtype
           if(icsubtype(nc) == iobsub) then
              ncsave=nc
              exit matchloop
           else
!  Find convtype which match ob type and subtype group (isubtype == ?*)
!       where ? specifies the group and icsubtype = ?0)
              ixsub=icsubtype(nc)/10
              iosub=iobsub/10
              isubsub=icsubtype(nc)-ixsub*10
              if(ixsub == iosub .and. isubsub == 0) then
                 ncsave=nc
!  Find convtype which match ob type and subtype is all remaining
!       (icsubtype(nc) = 0)
              else if (ncsave == 0 .and. icsubtype(nc) == 0) then
                 ncsave=nc
              end if
           end if
        end do matchloop

!  Save information for next read
        if(ncsave /= 0) then
           maxobs=maxobs+1
           nx=1
           if(ithin_conv(ncsave) > 0)then
              do ii=2,ntread
                 if(ntx(ii) == ncsave)nx=ii
              end do
           end if
           tab(ntb,1)=ncsave
           tab(ntb,2)=nx
           tab(ntb,3)=1
           lmsg(nmsg,nx) = .true.
        end if
     enddo loop_report
  enddo msg_report


  allocate(cdata_all(nreal,maxobs),isort(maxobs))
  isort = 0
  cdata_all=zero
  nread=0
  ntest=0
  nvtest=0
  nchanl=0
  ilon=2
  ilat=3

! Open, then read date from bufr data
!!  read satellite winds one type a time

  loop_convinfo: do nx=1,ntread 
     use_all = .true.
     ithin=0
     if(nx >1) then
        nc=ntx(nx)
        ithin=ithin_conv(nc)
        if (ithin > 0 ) then
           rmesh=rmesh_conv(nc)
           pmesh=pmesh_conv(nc)
           use_all = .false.
           if(pmesh > zero) then
              pflag=1
              nlevp=r1200/pmesh
           else
              pflag=0
              nlevp=nsig
           endif
           xmesh=rmesh
           call make3grids(xmesh,nlevp)
           if (.not.use_all) then
              allocate(presl_thin(nlevp))
              if (pflag==1) then
                 do k=1,nlevp
                    presl_thin(k)=(r1200-(k-1)*pmesh)*one_tenth
                 enddo
              endif
           endif
           write(6,*)'READ_SATWND: ictype(nc),rmesh,pflag,nlevp,pmesh,nc ',&
                   ioctype(nc),ictype(nc),rmesh,pflag,nlevp,pmesh,nc
        endif
     endif

     call closbf(lunin)
     open(lunin,file=infile,form='unformatted')
     call openbf(lunin,'IN',lunin)
     call datelen(10)
     ntb = 0
     nmsg = 0
     loop_msg:  do while(IREADMG(lunin,subset,idate) == 0)
        nmsg = nmsg+1
        if(.not.lmsg(nmsg,nx)) then
           ntb=ntb+nrep(nmsg)
           cycle loop_msg ! no useable reports this mesage, skip ahead report count
        end if
        loop_readsb: do while(ireadsb(lunin) == 0)
           ntb = ntb+1
           nc=tab(ntb,1)
           if(nc <= 0 .or. tab(ntb,2) /= nx) cycle loop_readsb
           hdrdat=bmiss
           obsdat=bmiss
           satqc=bmiss
           heightdat=bmiss
           derdwdat=bmiss
           qcdat=bmiss
           iobsub=0
           itype=-1
           uob=bmiss
           vob=bmiss
           ppb=bmiss
           ppb1=bmiss
           ppb2=bmiss
           uob1=bmiss
           vob1=bmiss
           ee=r110
           qifn=r110
           qify=r110
           call ufbint(lunin,hdrdat,13,1,iret,hdrtr) 
           call ufbint(lunin,obsdat,4,1,iret,obstr)
           ppb=obsdat(2)
           if (ppb > 100000000.0_r_kind .or. hdrdat(3) >100000000.0_r_kind &
            .or. obsdat(4) > 100000000.0_r_kind) cycle loop_readsb
           if(ppb >r10000) ppb=ppb/r100
           if (ppb <r125) cycle loop_readsb    !  reject data above 125mb
           if (twodvar_regional .and. ppb <r600) cycle loop_readsb
!   reject the data with bad quality mark from SDM
           if(hdrdat(13) == 12.0_r_kind .or. hdrdat(13) == 14.0_r_kind) cycle loop_readsb      
!       Compare relative obs time with window.  If obs 
!       falls outside of window, don't use this obs
           idate5(1) = hdrdat(4)     !year
           idate5(2) = hdrdat(5)     ! month
           idate5(3) = hdrdat(6)     ! day
           idate5(4) = hdrdat(7)     ! hours
           idate5(5) = hdrdat(8)     ! minutes
           call w3fs21(idate5,nmind)
           t4dv = real((nmind-iwinbgn),r_kind)*r60inv
           if (l4dvar) then
              if (t4dv<zero .OR. t4dv>winlen) cycle loop_readsb 
           else
              sstime = real(nmind,r_kind) 
              tdiff=(sstime-gstime)*r60inv
              if (abs(tdiff)>twind) cycle loop_readsb 
           endif
!       determine the satellite wind type as in prepbufr
!       241: India, 242:JMA visible,243: EUMETSAT visible
!       245: GOES IR. 246: GOES WV cloud top, 247: GOES WV deep layer
!       250: JMA WV deep layer. 251:GOES visible, 252: JMA IR
!       253: EUMETSAT IR , 254: EUMETSAT WV deep layer
!       257: MODIS IR, 258: WV cloud top, 259:  WV deep layer
           iosub=0
           if(abs(hdrdat(2)) >r90 ) cycle loop_readsb 
           if(hdrdat(3) <zero) hdrdat(3)=hdrdat(3)+r360
           if(hdrdat(3) == r360) hdrdat(3)=hdrdat(3)-r360
           if(hdrdat(3) >r360) cycle loop_readsb 
              pqm=2
              qm=2
           if(trim(subset) == 'NC005064' .or. trim(subset) == 'NC005065' .or. &    !     EUMETSAT wind
              trim(subset) == 'NC005066') then
              if( hdrdat(1) <r70 .and. hdrdat(1) >= r50) then          
                 if(hdrdat(10) >68.0_r_kind) cycle loop_readsb   !   reject data zenith angle >68.0 degree 
                 if(hdrdat(1) == r55) iobsub=55 
                 if(hdrdat(1) == r56) iobsub=56 
                 if(hdrdat(9) == one)  then                  ! IR winds
                    itype=253
                 else if(hdrdat(9) == two) then              ! visible winds
                    itype=243
                 else if(hdrdat(9) == three) then            ! WV cloud top, try to assimilate
                    itype=254                                
                 else if(hdrdat(9) >= four) then             ! WV deep layer,monitoring
                    itype=254
                    pqm=9
                    qm=9
                 endif
!  get quality information
                 call ufbrep(lunin,qcdat,3,12,iret,qcstr) 
                 do j=4,9
                    if( qify <r105 .and. qifn <r105 .and. ee <r105) exit
                    if(qcdat(2,j) < r10000 .and. qcdat(3,j) <r10000) then
                       if(qcdat(2,j) == one .and. qify >r105) then
                          qify=qcdat(3,j)
                       else if(qcdat(2,j) == two .and. qifn >r105) then
                          qifn=qcdat(3,j)
                       else if(qcdat(2,j) ==  three .and. ee >r105) then
                          ee=qcdat(3,j)
                       endif
                    endif
                 enddo
                 if(qifn <85.0_r_kind)  then
                    qm=15
                    pqm=15
                 endif 
              endif
           else if(trim(subset) == 'NC005044' .or. trim(subset) == 'NC005045' .or. &   ! JMA
                   trim(subset) == 'NC005046') then           
              if(hdrdat(1) >=r100 .and. hdrdat(1) <=r199 ) then 
                 if(hdrdat(10) >68.0_r_kind) cycle loop_readsb   !   reject data zenith angle >68.0 degree 
                 if(hdrdat(9) == one)  then                      ! IR winds
                    itype=252
                 else if(hdrdat(9) == two) then                  ! visible winds
                    itype=242
                 else if(hdrdat(9) == three) then                ! WV cloud top 
                    itype=250
                 else if(hdrdat(9) >=four) then                  ! WV deep layer,as monitoring
!                    cycle loop_readsb
                    itype=250
                    qm=9
                    pqm=9
                 endif
! get quality information
                 call ufbrep(lunin,qcdat,3,12,iret,qcstr)
                 do j=4,9
                    if( qify <=r105 .and. qifn <r105 .and. ee <r105) exit
                    if(qcdat(2,j) <= r10000 .and. qcdat(3,j) <r10000) then
                       if(qcdat(2,j) == 101.0_r_kind .and. qify >r105 ) then
                          qify=qcdat(3,j)
                       else if(qcdat(2,j) == 102.0_r_kind .and. qifn >r105 ) then
                          qifn=qcdat(3,j)
                       else if(qcdat(2,j) == 103.0_r_kind .and. ee >r105) then
                          ee=qcdat(3,j)
                       endif   
                    endif
                 enddo 

                 if(qifn <85.0_r_kind)  then
                    qm=15
                    pqm=15
                 endif 
              endif
           else if(trim(subset) == 'NC005010' .or. trim(subset) == 'NC005011' .or. &  ! NESDIS GOES 
                   trim(subset) == 'NC005012' ) then
              if(hdrdat(1) >=r250 .and. hdrdat(1) <=r299 ) then
                 if(hdrdat(10) >68.0_r_kind) cycle loop_readsb   !   reject data zenith angle >68.0 degree 
                 if(hdrdat(1) == 259.0_r_kind) iobsub=15 
                 if(hdrdat(9) == one)  then                            ! IR winds
                    if(hdrdat(12) <50000000000000.0_r_kind) then        ! for channel 4
                       itype=245
                    else
!                      cycle loop_readsb                              ! for short wave IR
                       itype=245
                       iobsub=1
                    endif
                 else if(hdrdat(9) == two ) then                       ! visible winds
                    itype=251
                 else if(hdrdat(9) == three) then                      ! WV cloud top
                    itype=246
                 else if(hdrdat(9) >= four) then                       ! WV deep layer.discard
!                     cycle loop_readsb 
                     itype=247
                 endif
                 call ufbrep(lunin,qcdat,3,8,iret,qcstr)
! get quality information
                 do j=1,8
                    if( qify <=r105 .and. qifn <r105 .and. ee < r105) exit
                    if(qcdat(2,j) <= r10000 .and. qcdat(3,j) <r10000) then
                       if( qcdat(2,j) == one .and. qifn >r105 ) then
                          qifn=qcdat(3,j)
                       else if(qcdat(2,j) == three .and. qify >r105) then
                          qify=qcdat(3,j)
                       else if( qcdat(2,j) == four .and. ee >r105) then
                          ee=qcdat(3,j) 
                       endif  
                    endif
                 enddo
              endif
           else if(trim(subset) == 'NC005070' .or. trim(subset) == 'NC005071') then  ! MODIS  
              if(hdrdat(1) >=r700 .and. hdrdat(1) <= r799 ) then
                 if(hdrdat(9) == one)  then                            ! IR winds
                    itype=257
                 else if(hdrdat(9) == three) then                      ! WV cloud top
                    itype=258
                 else if(hdrdat(9) >= four) then                       ! WV deep layer
                    itype=259 
                 endif
!  get quality information
                 call ufbrep(lunin,qcdat,3,8,iret,qcstr) 
                 do j=1,8
                    if( qify <=r105 .and. qifn <r105 .and. ee < r105) exit
                    if(qcdat(2,j) <= r10000 .and. qcdat(3,j) <r10000) then
                       if(qcdat(2,j) == one .and. qifn >r105) then
                          qifn=qcdat(3,j)
                       else if(qcdat(2,j) == three .and. qify >r105) then
                          qify=qcdat(3,j)
                       else if( qcdat(2,j) == four .and. ee >r105 ) then
                          ee=qcdat(3,j) 
                       endif  
                    endif
                 enddo
              endif
           else if( trim(subset) == 'NC005080') then                   ! AVHRR 
              if(hdrdat(1) <10.0_r_kind .or. (hdrdat(1) >= 200.0_r_kind .and. &
                 hdrdat(1) <=223.0_r_kind) ) then      
                 if(hdrdat(9) == one)  then                            ! IR winds
                    itype=244
                    qm=15
                    pqm=15
                 else
                    write(6,*) 'READ_SATWND: wrong derived method value'
                 endif
! get quality information
                 call ufbrep(lunin,qcdat,3,8,iret,qcstr)
                 do j=1,6
                    if( qify <=r105 .and. qifn <r105 .and. ee <r105) exit
                    if(qcdat(2,j) <= r10000 .and. qcdat(3,j) <r10000 ) then
                       if(qcdat(2,j) ==  one  .and. qifn >r105) then
                          qifn=qcdat(3,j)
                       else if(qcdat(2,j) ==  three .and. qify >105) then
                          qify=qcdat(3,j)
                       else if( qcdat(2,j) == four .and. ee >105) then
                          ee=qcdat(3,j)
                       endif 
                    endif
                 enddo
              endif
           endif
           if ( qify == zero) qify=r110
           if ( qifn == zero) qifn=r110
           if (  ee == zero) ee=r110

           nread=nread+1
           dlon_earth=hdrdat(3)*deg2rad
           dlat_earth=hdrdat(2)*deg2rad
                              
!       If regional, map obs lat,lon to rotated grid.
           if(regional)then
              call tll2xy(dlon_earth,dlat_earth,dlon,dlat,outside)
              if(diagnostic_reg) then
                 call txy2ll(dlon,dlat,rlon00,rlat00)
                 ntest=ntest+1
                 cdist=sin(dlat_earth)*sin(rlat00)+cos(dlat_earth)*cos(rlat00)* &
                       (sin(dlon_earth)*sin(rlon00)+cos(dlon_earth)*cos(rlon00))
                 cdist=max(-one,min(cdist,one))
                 disterr=acos(cdist)*rad2deg
                 disterrmax=max(disterrmax,disterr)
              end if
              if(outside) cycle loop_readsb 
           else
              dlon=dlon_earth
              dlat=dlat_earth
              call grdcrd(dlat,1,rlats,nlat,1)
              call grdcrd(dlon,1,rlons,nlon,1)
           endif

!!   detect surface type for infrad IR winds monitoring over land for lat greter than 20N
!     isflg    - surface flag
!                0 sea
!                1 land
!                2 sea ice
!                3 snow
!                4 mixed
           if( .not. twodvar_regional) then
              if(itype ==245 .or. itype ==252 .or. itype ==253 ) then 
                 if(hdrdat(2) >20.0_r_kind) then 
                    call deter_sfc_type(dlat_earth,dlon_earth,t4dv,isflg,tsavg)
!                   if (isflg /= 0) cycle loop_readsb 
                    qm=15
                 endif
              endif
           endif
       
!!    convert from wind direction and speed to u,v component
           uob=-obsdat(4)*sin(obsdat(3)*deg2rad)
           vob=-obsdat(4)*cos(obsdat(3)*deg2rad)
!!!  some information only has in NESDIS satellite winds
!          if(hdrdat(1) >=r200 .and. hdrdat(1) <= r299 ) then
!             call ufbseq(lunin,heightdat,3,5,iret,heightr)         
!             call ufbseq(lunin,derdwdat,6,4,iret,derdwtr)         
!             write(99,*) 'heightdat ',itype
!             write(99,101) heightdat(2,1),heightdat(2,2),heightdat(2,3),heightdat(2,4),heightdat(2,5)
!101 format(5e10.2)
!             uob1=-derdwdat(6,2)*sin(derdwdat(5,2)*deg2rad)    ! get originial wind info
!             vob1=-derdwdat(6,2)*cos(derdwdat(5,2)*deg2rad)    ! get originial wind info
!             if(itype == 245 ) then
!                ppb1=heightdat(2,1)/r100                        ! window height assignment value
!                ppb2=heightdat(2,4)/r100                        ! co2 height assignment value  
!             else if(itype == 246) then
!                ppb1=heightdat(2,3)/r100                        !  H2O height assignment value
!                ppb2=heightdat(2,4)/r100                        ! co2 height assignment value
!             endif
!          endif

!!  first to get observation error from PREPBUFR observation error table
           ppb=max(zero,min(ppb,r2000))
           if(ppb>=etabl(itype,1,1)) k1=1          
           do kl=1,32
              if(ppb>=etabl(itype,kl+1,1).and.ppb<=etabl(itype,kl,1)) k1=kl
           end do
           if(ppb<=etabl(itype,33,1)) k1=5
           k2=k1+1
           ediff = etabl(itype,k2,1)-etabl(itype,k1,1)
           if (abs(ediff) > tiny_r_kind) then
              del = (ppb-etabl(itype,k1,1))/ediff
           else
              del = huge_r_kind
           endif
           del=max(zero,min(del,one))
           obserr=(one-del)*etabl(itype,k1,4)+del*etabl(itype,k2,4)
           obserr=max(obserr,werrmin)
!         Set usage variable
           usage = 0 
           iuse=icuse(nc)
           if(iuse <= 0)usage=r100
           if(qm == 15 .or. qm == 12 .or. qm == 9)usage=r100
           if(itype==242) then;  c_prvstg='JMA'      ;  c_sprvstg='VI'       ; endif
           if(itype==243) then;  c_prvstg='EUMETSAT' ;  c_sprvstg='VI'       ; endif
           if(itype==245) then;  c_prvstg='NESDIS'   ;  c_sprvstg='IR'       ; endif
           if(itype==246) then;  c_prvstg='NESDIS'   ;  c_sprvstg='WV'       ; endif
           if(itype==250) then;  c_prvstg='JMA'      ;  c_sprvstg='WV'       ; endif
           if(itype==251) then;  c_prvstg='NESDIS'   ;  c_sprvstg='VI'       ; endif
           if(itype==252) then;  c_prvstg='JMA'      ;  c_sprvstg='IR'       ; endif
           if(itype==253) then;  c_prvstg='EUMETSAT' ;  c_sprvstg='IR'       ; endif
           if(itype==254) then;  c_prvstg='EUMETSAT' ;  c_sprvstg='WV'       ; endif
           if(itype==257) then;  c_prvstg='MODIS'    ;  c_sprvstg='IR'       ; endif
           if(itype==258) then;  c_prvstg='MODIS'    ;  c_sprvstg='WVCTOP'   ; endif
           if(itype==259) then;  c_prvstg='MODIS'    ;  c_sprvstg='WVDLAYER' ; endif

           c_station_id='SATWND'

! Get information from surface file necessary for conventional data here
           call deter_sfc2(dlat_earth,dlon_earth,t4dv,idomsfc,tsavg,ff10,sfcr,zz)
 
!!    process the thining procedure
                
           ithin=ithin_conv(nc)
           ithinp = ithin > 0 .and. pflag /= 0
!          if(ithinp  .and. iuse >=0 )then
           if(ithinp   )then
!          Interpolate guess pressure profile to observation location
              klon1= int(dlon);  klat1= int(dlat)
              dx   = dlon-klon1; dy   = dlat-klat1
              dx1  = one-dx;     dy1  = one-dy
              w00=dx1*dy1; w10=dx1*dy; w01=dx*dy1; w11=dx*dy
              klat1=min(max(1,klat1),nlat); klon1=min(max(0,klon1),nlon)
              if (klon1==0) klon1=nlon
              klatp1=min(nlat,klat1+1); klonp1=klon1+1
              if (klonp1==nlon+1) klonp1=1
              do kk=1,nsig
                 presl(kk)=w00*prsl_full(klat1 ,klon1 ,kk) +  &
                           w10*prsl_full(klatp1,klon1 ,kk) + &
                           w01*prsl_full(klat1 ,klonp1,kk) + &
                           w11*prsl_full(klatp1,klonp1,kk)
              end do
 
 !          Compute depth of guess pressure layersat observation location
           end if
           dlnpob=log(one_tenth*ppb)  ! ln(pressure in cb)
           ppb=one_tenth*ppb         ! from mb to cb
 !         Special block for data thinning - if requested
           if (ithin > 0 .and. iuse >=0) then
              ntmp=ndata  ! counting moved to map3gridS
 !         Set data quality index for thinning
              if (l4dvar) then
                 timedif = zero
              else
                 timedif=abs(t4dv-toff)
              endif
              if(itype == 243 .or. itype == 253 .or. itype == 254) then
                 if(qifn <r105) then
                    crit1 = timedif/r6+half + four*(one-qifn/r100)*r3_33
                 else
                    crit1 = timedif/r6+half
                 endif
              else if(itype == 245 .or. itype == 246) then
                 if(qifn <r105 .and. ee <r105) then
                    crit1 = timedif/r6+half + four*(one-qifn/r100)*r3_33+(one-ee/r100)*r3_33
                 else
                    crit1 = timedif/r6+half
                 endif
              else
                 crit1 = timedif/r6+half
              endif
              if (pflag==0) then
                 do kk=1,nsig
                    presl_thin(kk)=presl(kk)
                 end do
              endif
 
              call map3grids(-1,pflag,presl_thin,nlevp,dlat_earth,dlon_earth,&
                              ppb,crit1,ithin,ndata,iout,ntb,iiout,luse)
              if (.not. luse) cycle loop_readsb
              if(iiout > 0) isort(iiout)=0
              if (ndata > ntmp) then
                 nodata=nodata+1
              endif
              isort(ntb)=iout
           else
 !            write(6,*) 'READ_SATWND,ndata=',ndata,iout
              ndata=ndata+1
              nodata=nodata+1
              iout=ndata
              isort(ntb)=iout
           endif
           inflate_error=.false.
           if (qm==3 .or. qm==7) inflate_error=.true.
           woe=obserr
           if (inflate_error) woe=woe*r1_2
           if(regional)then
              u0=uob
              v0=vob
              call rotate_wind_ll2xy(u0,v0,uob,vob,dlon_earth,dlon,dlat)
              if(diagnostic_reg) then
                 call rotate_wind_xy2ll(uob,vob,u00,v00,dlon_earth,dlon,dlat)
                 nvtest=nvtest+1
                 disterr=sqrt((u0-u00)**2+(v0-v00)**2)
                 vdisterrmax=max(vdisterrmax,disterr)
              end if
           endif
           cdata_all(1,iout)=woe                  ! wind error
           cdata_all(2,iout)=dlon                 ! grid relative longitude
           cdata_all(3,iout)=dlat                 ! grid relative latitude
           cdata_all(4,iout)=dlnpob               ! ln(pressure in cb)
           cdata_all(5,iout)=ee                   !  quality information 
           cdata_all(6,iout)=uob                  ! u obs
           cdata_all(7,iout)=vob                  ! v obs 
           cdata_all(8,iout)=rstation_id          ! station id 
           cdata_all(9,iout)=t4dv                 ! time
           cdata_all(10,iout)=nc                  ! index of type in convinfo file
           cdata_all(11,iout)=qifn +1000.0_r_kind*qify   ! quality mark infor  
           cdata_all(12,iout)=qm                  ! quality mark
           cdata_all(13,iout)=obserr              ! original obs error
           cdata_all(14,iout)=usage               ! usage parameter
           cdata_all(15,iout)=idomsfc             ! dominate surface type
           cdata_all(16,iout)=tsavg               ! skin temperature
           cdata_all(17,iout)=ff10                ! 10 meter wind factor
           cdata_all(18,iout)=sfcr                ! surface roughness
           cdata_all(19,iout)=dlon_earth*rad2deg  ! earth relative longitude (degrees)
           cdata_all(20,iout)=dlat_earth*rad2deg  ! earth relative latitude (degrees)
           cdata_all(21,iout)=zz                  ! terrain height at ob location
           cdata_all(22,iout)=r_prvstg(1,1)       ! provider name
           cdata_all(23,iout)=r_sprvstg(1,1)      ! subprovider name

           if(perturb_obs)then
              cdata_all(24,iout)=ran01dom()*perturb_fact ! u perturbation
              cdata_all(25,iout)=ran01dom()*perturb_fact ! v perturbation
           endif

        enddo  loop_readsb
 !   End of bufr read loop
     enddo loop_msg
!    Close unit to bufr file
     call closbf(lunin)
!    Deallocate arrays used for thinning data
     if (.not.use_all) then
        deallocate(presl_thin)
        call del3grids
     endif
! Normal exit

  enddo loop_convinfo! loops over convinfo entry matches
  deallocate(lmsg)
 

  ! Write header record and data to output file for further processing
  allocate(iloc(ndata))
  icount=0
  do i=1,maxobs
     if(isort(i) > 0)then
        icount=icount+1
        iloc(icount)=isort(i)
     end if
  end do
  if(ndata /= icount)then
     write(6,*) ' READ_SATWND: mix up in read_satwnd ,ndata,icount ',ndata,icount
     call stop2(49)
  end if

  allocate(cdata_out(nreal,ndata))
  do i=1,ndata
     itx=iloc(i)
     do k=1,nreal
        cdata_out(k,i)=cdata_all(k,itx)
     end do
  end do
  deallocate(iloc,isort,cdata_all)
  deallocate(etabl)
  
  write(lunout) obstype,sis,nreal,nchanl,ilat,ilon
  write(lunout) cdata_out

  deallocate(cdata_out)
900 continue
  if(diagnostic_reg .and. ntest>0) write(6,*)'READ_SATWND:  ',&
       'ntest,disterrmax=',ntest,disterrmax
  if(diagnostic_reg .and. nvtest>0) write(6,*)'READ_SATWND:  ',&
       'nvtest,vdisterrmax=',ntest,vdisterrmax



  if (ndata == 0) then
     call closbf(lunin)
     write(6,*)'READ_SATWND:  closbf(',lunin,')'
  endif
  
  write(6,*) 'READ_SATWND,nread,ndata,nreal,nodata=',nread,ndata,nreal,nodata

  close(lunin)

! End of routine
  return



end subroutine read_satwnd
