PROGRAM read_diag_rad
!
!  This is to read GSI radiance diagnositic data from subroutine setuprad.f90
!
!
!  Here is code in setuprad.f90 that write out diagnostic information
!
!       write(14) isis,dplat(is),obstype,jiter,nchanl,npred,idate,ireal,ipchan,iextra,jextra
!        do i=1,nchanl
!           write(14)freq4,pol4,wave4,varch4,tlap4,iuse_rad(n),nuchan(n),ich(i)
!        end do
!
!       if (.not.lextra) then
!          write(14) diagbuf,diagbufchan
!       else
!          write(14) diagbuf,diagbufchan,diagbufex
!       endif
!
!       diagbuf(1)  = cenlat                         ! observation latitude (degrees)
!       diagbuf(2)  = cenlon                         ! observation longitude (degrees)
!       diagbuf(3)  = zsges                          ! model (guess) elevation at observation location
!
!       diagbuf(4)  = dtime                          ! observation time (hours relative to analysis time)
!
!       diagbuf(5)  = data_s(iscan_pos,n)            ! sensor scan position
!       diagbuf(6)  = zasat*rad2deg                  ! satellite zenith angle (degrees)
!       diagbuf(7)  = data_s(ilazi_ang,n)            ! satellite azimuth angle (degrees)
!       diagbuf(8)  = pangs                          ! solar zenith angle (degrees)
!       diagbuf(9)  = data_s(isazi_ang,n)            ! solar azimuth angle (degrees)
!       diagbuf(10) = sgagl                          ! sun glint angle (degrees) (sgagl)
!
!       diagbuf(11) = surface(1)%water_coverage         ! fractional coverage by water
!       diagbuf(12) = surface(1)%land_coverage          ! fractional coverage by land
!       diagbuf(13) = surface(1)%ice_coverage           ! fractional coverage by ice
!       diagbuf(14) = surface(1)%snow_coverage          ! fractional coverage by snow
!       diagbuf(15) = surface(1)%water_temperature      ! surface temperature over water (K)
!       diagbuf(16) = surface(1)%land_temperature       ! surface temperature over land (K)
!       diagbuf(17) = surface(1)%ice_temperature        ! surface temperature over ice (K)
!       diagbuf(18) = surface(1)%snow_temperature       ! surface temperature over snow (K)
!       diagbuf(19) = surface(1)%soil_temperature       ! soil temperature (K)
!       diagbuf(20) = surface(1)%soil_moisture_content  ! soil moisture
!       diagbuf(21) = surface(1)%land_type              ! surface land type
!       diagbuf(22) = surface(1)%vegetation_fraction    ! vegetation fraction
!       diagbuf(23) = surface(1)%snow_depth             ! snow depth
!       diagbuf(24) = surface(1)%wind_speed             ! surface wind speed (m/s)
!
!       if (.not.microwave) then
!          diagbuf(25)  = cld                        ! cloud fraction (%)
!          diagbuf(26)  = cldp                       ! cloud top pressure (hPa)
!       else
!          diagbuf(25)  = clw                        ! cloud liquid water (kg/m**2)
!          diagbuf(26)  = tpwc                       ! total column precip. water (km/m**2)
!       endif
!
!       do i=1,nchanl
!          diagbufchan(1,i)=tb_obs(i)       ! observed brightness temperature (K)
!          diagbufchan(2,i)=tbc(i)          ! observed - simulated Tb with bias corrrection (K)
!          diagbufchan(3,i)=tbcnob(i)       ! observed - simulated Tb with no bias correction (K)
!          errinv = sqrt(varinv(i))
!          diagbufchan(4,i)=errinv          ! inverse observation error
!          useflag=one
!          if (iuse_rad(ich(i))/=1) useflag=-one
!          diagbufchan(5,i)= id_qc(i)*useflag! quality control mark or event indicator
!
!          diagbufchan(6,i)=emissivity(i)   ! surface emissivity
!          diagbufchan(7,i)=tlapchn(i)      ! stability index
!          do j=1,npred+1
!             diagbufchan(7+j,i)=predterms(j,i) ! Tb bias correction terms (K)
!          end do
!       end do
!
!
!
  use kinds, only: r_kind,r_single,i_kind
!
  implicit none

!
! read in variables
!
  character(10) :: obstype
  character(20) :: isis
  character(10) :: dplat
  integer(i_kind) :: jiter
  integer(i_kind) :: nchanl
  integer(i_kind) :: npred
  integer(i_kind) :: idate
  integer(i_kind) :: ireal
  integer(i_kind) :: ipchan
  integer(i_kind) :: iextra,jextra

  real(r_single) freq4,pol4,wave4,varch4,tlap4
  integer(i_kind) :: iuse_rad
  integer(i_kind) :: nuchan
  integer(i_kind),allocatable :: ich(:)   ! nchanl
  
  integer(i_kind) :: ifirst

  real(r_single),allocatable,dimension(:,:):: diagbufchan   !  ipchan+npred+1,nchanl
  real(r_single),allocatable,dimension(:):: diagbuf   ! ireal
!
!  namelist files
!
  character(180) :: infilename         ! file from GSI running directory
  character(180) ::  outfilename       ! file name saving results
  namelist/iosetup/ infilename,outfilename
!
! output variables
!
  real :: rlat,rlon,rprs
  real :: rdhr
!
!  misc.
!
  character(10) ::  cipe,cloop
  character(20) ::  this_instrument
  character ::  ch
  integer :: ipe,i,j,k,ios, iloop, iinstrument
  integer :: ic, iflg

!
!
!
  outfilename='diag_results'
  open(11,file='namelist.rad')
   read(11,iosetup)
  close(11)
!
!
!  read diag for this_instrument
!
   open(12, file=trim(outfilename))
   OPEN(17,FILE=trim(infilename),STATUS='OLD',IOSTAT=ios,ACCESS='SEQUENTIAL',  &
           FORM='UNFORMATTED')
   if(ios > 0 ) then
      write(*,*) ' no diag file availabe :', trim(infilename)
   endif

   read(17) isis,dplat,obstype,jiter,nchanl,npred,idate,ireal,ipchan,iextra,jextra
   write(12,'(3a20)') isis,dplat,obstype
   write(12,'(8I10)') jiter,nchanl,npred,idate,ireal,ipchan,iextra,jextra

   allocate(ich(nchanl))
   do i=1,nchanl
      read(17) freq4,pol4,wave4,varch4,tlap4,iuse_rad,nuchan,ich(i)
      write(12,'(a,I3,5f10.3,3I5)') 'nchanl=',i,freq4,pol4,wave4,varch4,tlap4,& 
                                    iuse_rad,nuchan,ich(i)
   end do

   allocate(diagbufchan(ipchan+npred+1,nchanl))
   allocate(diagbuf(ireal))

100  continue
        read(17, ERR=999,end=110) diagbuf,diagbufchan
        rlat=diagbuf(1)     ! observation latitude (degrees)
        rlon=diagbuf(2)     ! observation longitude (degrees)
        rprs=diagbuf(3)     ! observation pressure (hPa)
        rdhr=diagbuf(4)     ! obs time (hours relative to analysis time)
!
!  write out result for one observation
         write (12,'(4F8.2)') rlat,rlon,rprs,rdhr
!         write (12,*) diagbuf,diagbufchan

     goto 100  ! goto another record

110  continue

      close(17)
      close(12)

       deallocate(diagbufchan,diagbuf)
       deallocate(ich)

  STOP 9999

999    PRINT *,'error read in diag file'
      stop 1234

END PROGRAM read_diag_rad
