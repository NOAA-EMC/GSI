PROGRAM read_diag_conv
!
!  This program is to show how to 
!  read GSI diagnositic file for conventional data, which are
!  generated from subroutine:
!      setupps.f90
!      setupt.f90
!      setupq.f90
!      setuppw.f90
!      setupuv.f90
!      setupsst.f90
!      setupgps.f90
!
!  For example in setupt.f90:
!      the arrary contents disgnosis information is rdiagbuf.
!        cdiagbuf(ii)       ! station id
!        rdiagbuf(1,ii)     ! observation type
!        rdiagbuf(2,ii)     ! observation subtype
!        rdiagbuf(3,ii)     ! observation latitude (degrees)
!        rdiagbuf(4,ii)     ! observation longitude (degrees)
!        rdiagbuf(5,ii)     ! station elevation (meters)
!        rdiagbuf(6,ii)     ! observation pressure (hPa)
!        rdiagbuf(7,ii)     ! observation height (meters)
!        rdiagbuf(8,ii)     ! obs time (hours relative to analysis time)
!        rdiagbuf(9,ii)     ! input prepbufr qc or event mark
!        rdiagbuf(10,ii)    ! setup qc or event mark (currently qtflg only)
!        rdiagbuf(11,ii)    ! read_prepbufr data usage flag
!        rdiagbuf(12,ii)    ! analysis usage flag (1=use, -1=not used)
!        rdiagbuf(13,ii)    ! nonlinear qc relative weight
!        rdiagbuf(14,ii)    ! prepbufr inverse obs error (K**-1)
!        rdiagbuf(15,ii)    ! read_prepbufr inverse obs error (K**-1)
!        rdiagbuf(16,ii)    ! final inverse observation error (K**-1)
!        rdiagbuf(17,ii)    ! temperature observation (K)
!        rdiagbuf(18,ii)    ! obs-ges used in analysis (K)
!        rdiagbuf(19,ii)    ! obs-ges w/o bias correction (K) (future slot)
!
!  It is written out as:
!     write(7)'  t',nchar,nreal,ii,mype
!     write(7)cdiagbuf(1:ii),rdiagbuf(:,1:ii)
!

  use kinds, only: r_kind,r_single,i_kind

  implicit none

  real(r_kind) tiny_r_kind
!
! read in variables
!
  character(8),allocatable,dimension(:):: cdiagbuf
  character(8),allocatable,dimension(:):: cprvstg,csprvstg ! obs provider/sub-provider
  character(8)::cprovider,csubprovider
  real(r_single),allocatable,dimension(:,:)::rdiagbuf
  integer(i_kind) nchar,nreal,ii,mype,iip   ! iip: number of pseudo-obs if existing
  integer(i_kind) idate
!
!  namelist files
!
  character(180) :: infilename        ! file from GSI running directory
  character(180) :: outfilename       ! file name saving results
  logical        :: l_obsprvdiag      ! if true, read/write obs provider/sub-provider info
  logical        :: dump_pseudo_obs_too !if true write out pseudo obs as well
  namelist/iosetup/ infilename, outfilename, l_obsprvdiag, dump_pseudo_obs_too
!
! output variables
!
  character(len=3)  :: var
  real(r_single) :: rlat,rlon,rprs,rhgt,robs1,rdpt1,robs2,rdpt2,ruse,rerr
  real(r_single) :: rdhr,iusev,ddiff
  character(8) :: stationID
  integer(i_kind) :: itype,iuse
  integer(i_kind) :: isubtype,isubtype0
!
!  misc.
!
  character ::  ch
  integer(i_kind) :: i,j,k,ios
  integer(i_kind) :: ic, iflg

  logical fexist

!
! initialization of variables in namelist
  data infilename /'diag_conv.dat'/
  data outfilename /'diag_results'/
  data l_obsprvdiag /.false./        ! no obs provider info (by default)
  data dump_pseudo_obs_too /.false./
! outfilename='diag_results'

  inquire(file='namelist.conv',exist=fexist)
  if(fexist) then
     open(11,file='namelist.conv')
     read(11,iosetup)
     close(11)
  else
     write(6,*) "no reading from namelist file."
  endif
  write(6,*) "checking the input/output setup info:"
  write(6,iosetup)
!
  open(42, file=trim(outfilename),IOSTAT=ios)
  if(ios > 0 ) then
       write(*,*) ' cannot open file ', trim(outfilename)
       stop 123
  else
       write(*,*) ' open file ', trim(outfilename)
  endif
!
  OPEN (17,FILE=trim(infilename),STATUS='OLD',IOSTAT=ios,ACCESS='SEQUENTIAL',  &
             FORM='UNFORMATTED')
     if(ios > 0 ) then
       write(*,*) ' file is unavailabe: ', trim(infilename)
       stop 123
     endif

     read(17, ERR=999) idate
     write(*,*) 'process date: ',idate
100  continue
     read(17, ERR=999,end=110) var, nchar,nreal,ii,mype
     write(*,*) var, nchar,nreal,ii,mype
     if (ii > 0) then
          allocate(cdiagbuf(ii),rdiagbuf(nreal,ii))
          if (l_obsprvdiag) allocate(cprvstg(ii),csprvstg(ii))
          read(17,ERR=999,end=110) cdiagbuf, rdiagbuf
          if (l_obsprvdiag) then
             cprvstg='XXXXXXXX' 
             csprvstg='XXXXXXXX'
             if (var(1:3)=='  t' .or. var(1:3)=='  q' .or. var(2:3)=='ps' .or. &
                 var(2:3)=='uv'  .or. var(1:3)=='spd') read(17)cprvstg,csprvstg
          endif
          do i=1,ii
             if (l_obsprvdiag) then
                cprovider=cprvstg(i)
                csubprovider=csprvstg(i)
             endif
             itype=rdiagbuf(1,i)    ! observation type
             isubtype=rdiagbuf(2,i) ! observation subtype
             rlat=rdiagbuf(3,i)     ! observation latitude (degrees)
             rlon=rdiagbuf(4,i)     ! observation longitude (degrees)
             rprs=rdiagbuf(6,i)     ! observation pressure (hPa)
             rhgt=rdiagbuf(7,i)     ! observation height (meters)
             rdhr=rdiagbuf(8,i)     ! obs time (hours relative to analysis time)
             iuse=int(rdiagbuf(12,i))    ! analysis usage flag (1=use, -1=monitoring ) 
             iusev=int(rdiagbuf(11,i))    ! analysis usage flag ( value ) 
             ddiff=rdiagbuf(18,i)   ! obs-ges used in analysis (K)
             rerr = 0._r_single
             if (rdiagbuf(16,i) > 1.0E-12_r_single) then   ! final inverse observation error (K**-1)
               rerr = 1.0/rdiagbuf(16,i)
             else
               rerr = 0._r_single
             end if
             robs1=rdiagbuf(17,i)    !  observation (K)
             rdpt1=rdiagbuf(18,i)    !  obs-ges used in analysis 

! get station ID
             stationID = cdiagbuf(i)
!           Remove odd spaces in the station ID
             iflg = 0
             do ic=8,1,-1
              ch = stationID(ic:ic)
              if (ch > ' ' .and. ch <= 'z') then
                iflg = 1
              else
                 stationID(ic:ic) = ' '
              end if
              if (ch == ' '  .and. iflg == 1) then
                 stationID(ic:ic) = '_'
              endif 
             enddo

!           Remove odd spaces in the obs provider, and subprovider names
             if (l_obsprvdiag) then
               iflg = 0
               do ic=8,1,-1
                ch = cprovider(ic:ic)
                if (ch > ' ' .and. ch <= 'z') then
                  iflg = 1
                else
                   cprovider(ic:ic) = ' '
                end if
                if (ch == ' '  .and. iflg == 1) then
                   cprovider(ic:ic) = '_'
                endif
               enddo

               iflg = 0
               do ic=8,1,-1
                ch = csubprovider(ic:ic)
                if (ch > ' ' .and. ch <= 'z') then
                  iflg = 1
                else
                   csubprovider(ic:ic) = ' '
                end if
                if (ch == ' '  .and. iflg == 1) then
                   csubprovider(ic:ic) = '_'
                endif
               enddo
             endif
!
!   When the data is q, unit convert kg/kg -> g/kg **/
             if (var == "  q") then
                robs1 = robs1 * 1000.0
                rdpt1 = rdpt1 * 1000.0
                rerr = rerr * 1000.0
                ddiff = ddiff * 1000.0
             end if
!   When the data is pw, replase the rprs to -999.0 **/
             if (var == " pw") rprs=-999.0
!
             if(robs1 > 1.0e8) then
               robs1=-99999.9
               ddiff=-99999.9
             endif

!           check up the information in the obs provider, and subprovider names
             if (l_obsprvdiag) then
               if (cprovider(1:4)=='B7Hv' .or. cprovider(5:8)=='vH7B') then !this provider name comes with strange characters
                   cprovider(1:4)='B7Hv'
                   cprovider(5:8)='   '
               endif

               if (csubprovider(1:4)=='B7Hv' .or. csubprovider(5:8)=='vH7B') then
                   csubprovider(1:4)='B7Hv'
                   csubprovider(5:8)='   '
               endif

               if (itype==154 .and. trim(adjustl(var))=='tca') then
                   stationID='GOESSKY'
                   cprovider='GOESSKY'
                   csubprovider='GOESSKY'
               end if

               if (trim(cprovider)=='') cprovider='EMPTY'
               if (trim(csubprovider)=='') csubprovider='EMPTY'
             endif

!            special treatment to obs tca/cei/vis/gst
!            If we have ceiling or total cloud amount obs less than 0,
!            set to missing
             if ((trim(adjustl(var))=="tca" .or. trim(adjustl(var))=="cei" .or.   &
                  trim(adjustl(var))=="vis" .or. trim(adjustl(var))=="gst") .and. &
                  robs1 < 0.) then
               robs1=0.10000E+10
               ddiff=0.10000E+10
             end if

             if (dump_pseudo_obs_too) then 
                isubtype0=0               ! reset subtype (esp.for pseudo-obs) to be zero in order to print it our 
             else
                isubtype0=isubtype
             endif
!
!  write out result for one variable on one pitch
             if (l_obsprvdiag) then       ! write out obs provider info with other info together 
               if ( var .ne. " uv" .and. isubtype0 >=0 ) then
                 write (42,'(A3,1x,A8,1x,A8,1x,A8,1x,I3,1x,F10.2,F8.2,F8.2,2F20.5,I5,2E15.5,1x,"NaN  NaN   ",E15.5,F10.3)') &
                   var,stationID,cprovider,csubprovider,itype,rdhr,rlat,rlon,rprs,rhgt,iuse,robs1,ddiff,rerr,iusev
               else if ( var .eq. " uv" .and. isubtype0 >=0 ) then
!  ** When the data is uv, additional output is needed **/
                 robs2=rdiagbuf(20,i)
                 rdpt2=rdiagbuf(21,i)
                 write (42,'(A3,1x,A8,1x,A8,1x,A8,1x,I3,1x,F10.2,F8.2,F8.2,2F20.5,I5,4E15.5,1x,E15.5,F10.3)') &
                   var,stationID,cprovider,csubprovider,itype,rdhr,rlat,rlon,rprs,rhgt,iuse,robs1,ddiff,robs2,rdpt2,rerr,iusev
               endif
             else   ! if no need to write out obs provider info
               if (var .ne. " uv") then
                 write (42,'(A3," @ ",A8," : ",I3,F10.2,F8.2,F8.2,F8.2,I5,2F10.2)') &
                   var,stationID,itype,rdhr,rlat,rlon,rprs,iuse,robs1,ddiff
               else
!  ** When the data is uv, additional output is needed **/
                 robs2=rdiagbuf(20,i)
                 rdpt2=rdiagbuf(21,i)
                 write (42,'(A3," @ ",A8," : ",I3,F10.2,F8.2,F8.2,F8.2,I5,4F10.2)') &
                   var,stationID,itype,rdhr,rlat,rlon,rprs,iuse,robs1,ddiff,robs2, rdpt2
               endif
             endif

          enddo   ! i  end for one station

          deallocate(cdiagbuf,rdiagbuf)
          if (l_obsprvdiag) deallocate(cprvstg,csprvstg)
     else
        read(17)
     endif
     goto 100  ! goto another variable
110  continue

    close(17)
    close(42)

  STOP 9999

999    PRINT *,'error read in diag file'
      stop 1234

END PROGRAM read_diag_conv
