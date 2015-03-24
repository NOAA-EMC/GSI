program bcoef
!************************************************************************
!
!  bcoef.f90
!
!  Extract necessary data for bcoef plots from *.ieee_d files (ges and anl) 
!  and export to an ascii data file for use by javascript.
!
!  In addition to the namelist, a 1 other file -- times.txt
!  and chan.txt must be available.  
!    times.txt --> must contain the 10 digit date(s) to be processed, 
!                  one time per line and comma separated. 
!    chan.txt  --> must contain the actual channel number for each channel,
!                  the use flag, the wavelength, and frequency, in CSV format, 
!                  one entry for a given channel per line.
!
!  Note to self:  apparently bcoef plots have never used region.  Bcor does, 
!    but not bcoef.  
!************************************************************************

  use IFPORT

  implicit none

  integer ftyp,cyc,chan,open_status

  logical eof, exist

  character(20) str_nchanl
  character(60) sat_out_file

  character(40) data_file

  character(len=10),allocatable,dimension(:)::times
  character(len=2), allocatable,dimension(:)::useflg
  character(len=5), allocatable,dimension(:)::chan_nums,wave,freq

!        mean       pred(1) = global offset (mean)
!        atmpath    pred(2) = not used when adp_anglebc=.true. and
!        newpc4pred=.true.
!        clw        pred(3) = cloud liquid water term
!        lapse2     pred(4) = (temperature lapse rate)**2
!        lapse      pred(5) = temperature lapse rate
!        cos_ssmis  pred(6) = cosine term for SSMIS
!        sin_ssmis  pred(7) = sine term for SSMIS
!        emiss      pred(8) = emissivity sensitivity term
!        ordang4    pred(9) = 4th order angle term
!        ordang3    pred(10) = 3rd order angle term
!        ordang2    pred(11) = 2nd order angle term
!        ordang1    pred(12) = 1st order angle term

  integer luname,ldname,lpname,lsatchan,lsatout
  integer ii, jj
  
  real,allocatable,dimension(:,:,:):: mean,atmpath,clw,lapse2,lapse,cos_ssmis
  real,allocatable,dimension(:,:,:):: sin_ssmis,emiss,ordang4,ordang3,ordang2,ordang1
  real,allocatable,dimension(:)    :: penalty
!  real,allocatable,dimension(:,:,:)  :: chi

! Namelist with defaults
  integer               :: nchanl               = 19
  integer               :: ncycle               = 1
  character(15)         :: satname              ='ssmis_f18'
  namelist /input/ satname,nchanl,ncycle

  data luname,ldname,lpname,lsatchan / 5, 50, 51, 52 /

!************************************************************************
! Read namelist input
!************************************************************************
  read(luname,input)
  write(6,input)

!************************************************************************
! Read times.txt input file, which is the dates to process in order
! from most recent to oldest.
!************************************************************************
   allocate( times (ncycle) )

   open( lpname, file='times.txt' )

   do ii=1,ncycle
      read(lpname, *) times(ii)
   end do
   close(lpname)
   write(6,*) 'read times.txt'

!************************************************************************
! Don't bother with chan.txt.  Don't see any reason I can't use the 
! channel file that time produces.  If I just use the region 1 value the
! chi values should be the same, and freq, wave, and use values have to be
! the same.
!       - not so fast.  chan_nums is written to the output txt file so
!         will need a way around that.
!
! Read chan.txt input file, which is the actual channel number for each 
! channel.
!************************************************************************
   allocate( chan_nums (nchanl) )
   allocate( useflg(nchanl), wave(nchanl), freq(nchanl) )
!   allocate( chi(2, nchanl, nregion) )

   open( lpname, file='chan.txt' )

   do ii=1,nchanl
      read(lpname, *) chan_nums(ii), useflg(ii), wave(ii), freq(ii)
!      do jj=1,nregion
!         chi(1,ii,jj) = 0.00
!      end do
   end do
   close(lpname)

   write(6,*) 'read chan.txt'


!************************************************************************
!  Explanation of the fype array
!     These are values used within the generated GrADS control file
!     (bcoef.ctl).  A fuller discription of the terms is thus:
!
!        mean       pred(1) = global offset (mean)
!        atmpath    pred(2) = not used when adp_anglebc=.true. and newpc4pred=.true.
!        clw        pred(3) = cloud liquid water term
!        lapse2     pred(4) = (temperature lapse rate)**2
!        lapse      pred(5) = temperature lapse rate
!        cos_ssmis  pred(6) = cosine term for SSMIS
!        sin_ssmis  pred(7) = sine term for SSMIS
!        emiss      pred(8) = emissivity sensitivity term
!        ordang4    pred(9) = 4th order angle term
!        ordang3    pred(10) = 3rd order angle term
!        ordang2    pred(11) = 2nd order angle term
!        ordang1    pred(12) = 1st order angle term
!
!************************************************************************

!**************************************************
!  Allocate space for variables
!    note:  first 2 is for ges|anl, 
!           second 2 is for value, value**2
!**************************************************
   allocate ( penalty   (nchanl)          )
   allocate ( mean      (2,ncycle,nchanl) )
   allocate ( atmpath   (2,ncycle,nchanl) )
   allocate ( clw       (2,ncycle,nchanl) )
   allocate ( lapse2    (2,ncycle,nchanl) )
   allocate ( lapse     (2,ncycle,nchanl) )
   allocate ( cos_ssmis (2,ncycle,nchanl) )
   allocate ( sin_ssmis (2,ncycle,nchanl) )
   allocate ( emiss     (2,ncycle,nchanl) )
   allocate ( ordang4   (2,ncycle,nchanl) )
   allocate ( ordang3   (2,ncycle,nchanl) )
   allocate ( ordang2   (2,ncycle,nchanl) )
   allocate ( ordang1   (2,ncycle,nchanl) )

   write(6,*) 'allocated space for the 12 terms'


!**************************************************
!  Loop over ftyp (ges|anl), cyc, chan, region and
!    read data from ges and anl files.
!
!  NOTE:  omg_bc/nbc are used for both ges and anl
!         as a programming simplicity; the values 
!         are really omabc|omanbc for anl files.
!**************************************************
   do ftyp=1,2
      do cyc=1,ncycle
         if ( ftyp == 1 ) then
            data_file= trim(satname) // '.' // trim(times(cyc)) // '.ieee_d'
         else
            data_file= trim(satname) // '_anl.' // trim(times(cyc)) // '.ieee_d'
         end if

         inquire(file=data_file, exist=exist)
!         write(6,*) 'data_file,exist = ', data_file, ' ', exist

         if ( exist == .TRUE. ) then
            open(ldname,file=data_file,form='unformatted')
            read(ldname) (penalty(jj),jj=1,nchanl)
!            write(6,*) 'penalty(1) = ', penalty(1)

            read(ldname) (mean      (ftyp,cyc,jj),jj=1,nchanl)
            read(ldname) (atmpath   (ftyp,cyc,jj),jj=1,nchanl)
            read(ldname) (clw       (ftyp,cyc,jj),jj=1,nchanl)
            read(ldname) (lapse2    (ftyp,cyc,jj),jj=1,nchanl)
            read(ldname) (lapse     (ftyp,cyc,jj),jj=1,nchanl)
            read(ldname) (cos_ssmis (ftyp,cyc,jj),jj=1,nchanl)
            read(ldname) (sin_ssmis (ftyp,cyc,jj),jj=1,nchanl)
            read(ldname) (emiss     (ftyp,cyc,jj),jj=1,nchanl)
            read(ldname) (ordang4   (ftyp,cyc,jj),jj=1,nchanl)
            read(ldname) (ordang3   (ftyp,cyc,jj),jj=1,nchanl)
            read(ldname) (ordang2   (ftyp,cyc,jj),jj=1,nchanl)
            read(ldname) (ordang1   (ftyp,cyc,jj),jj=1,nchanl)

            close(ldname)
         else
            write(6,*)' data file does not exist:  ', data_file
            do jj=1,nchanl
               mean     (ftyp,cyc,jj) = 0.0
               atmpath  (ftyp,cyc,jj) = 0.0
               clw      (ftyp,cyc,jj) = 0.0
               lapse2   (ftyp,cyc,jj) = 0.0
               lapse    (ftyp,cyc,jj) = 0.0
               cos_ssmis(ftyp,cyc,jj) = 0.0
               sin_ssmis(ftyp,cyc,jj) = 0.0
               emiss    (ftyp,cyc,jj) = 0.0
               ordang4  (ftyp,cyc,jj) = 0.0
               ordang3  (ftyp,cyc,jj) = 0.0
               ordang2  (ftyp,cyc,jj) = 0.0
               ordang1  (ftyp,cyc,jj) = 0.0
            end do
         end if

      end do
   end do

!*****************************************************************************
!  Write output files:  loop over channel and write bcoef terms for
!    every cycle for the channel to [satname].[chan].bcoef.txt file.
!
!    File format is channel number, cycle time and the 24 terms (12 each for 
!      ges and anl data), with 1 line for every cycle time.
!
   80 FORMAT(A5,',',A10,',',F9.3,',',F9.3,',',F9.3,',',F9.3,',',F9.3,',',F9.3 &
                        ',',F9.3,',',F9.3,',',F9.3,',',F9.3,',',F9.3,',',F9.3 &
                        ',',F9.3,',',F9.3,',',F9.3,',',F9.3,',',F9.3,',',F9.3 &
                        ',',F9.3,',',F9.3,',',F9.3,',',F9.3,',',F9.3,',',F9.3)
          
   do chan=1,nchanl

      write(str_nchanl, '(i20)') chan-1
      sat_out_file= trim(satname) // '.' // trim(adjustl(str_nchanl)) // '.bcoef.txt'
      write(6,*)' after sat_out_file assigned:  ', sat_out_file

      lsatout = 61 + chan
      open(lsatout,file=sat_out_file,iostat=open_status, &
                             action='write',status='new',form='formatted')
      write(6,*)' sat_out_file opened, status:  ', open_status

      do cyc=1,ncycle
         write(lsatout,80) trim(chan_nums(chan)),  trim(times(cyc)),       &
                           mean(1,cyc,chan),       mean(2,cyc,chan),       &
                           atmpath(1,cyc,chan),    atmpath(2,cyc,chan),    &
                           clw(1,cyc,chan),        clw(2,cyc,chan),        &
                           lapse2(1,cyc,chan),     lapse2(1,cyc,chan),     &
                           lapse(1,cyc,chan),      lapse(1,cyc,chan),      &
                           cos_ssmis(1,cyc,chan),  cos_ssmis(1,cyc,chan),  &
                           sin_ssmis(1,cyc,chan),  sin_ssmis(1,cyc,chan),  &
                           emiss(1,cyc,chan),      emiss(1,cyc,chan),      &
                           ordang4(1,cyc,chan),    ordang4(2,cyc,chan),    &
                           ordang3(1,cyc,chan),    ordang3(2,cyc,chan),    &
                           ordang2(1,cyc,chan),    ordang2(2,cyc,chan),    &
                           ordang1(1,cyc,chan),    ordang1(2,cyc,chan)

      end do                      
      
      close( lsatout )

   end do


  stop
end program bcoef
