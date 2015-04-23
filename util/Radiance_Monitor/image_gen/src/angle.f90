program angle
!************************************************************************
!
!  angle.f90
!
!  Extract necessary data for angle plots from *.ieee_d files (ges and anl) 
!  and export to an ascii data file (.txt) for use by javascript.
!
!  In addition to the namelist, a 2 other files -- times.txt and chan.txt
!    times.txt    --> must contain the 10 digit date(s) to be processed, 
!                  one time per line and comma separated. 
!    chan.txt     --> must contain the actual channel number for each channel,
!                  the use flag, the wavelength, and frequency, in CSV format, 
!                  one entry for a given channel per line.
!
!       note to self:  the plots can probably use time/chan.txt but the 
!                      chan_num is written to the *.txt files so short term
!                      I'm going to continue using the chan.txt file
!
!       Better:  modify the time/chan.txt to replace ncyc with nsteps.  The plot 
!                needs to know the nsteps in order to properly dimension the
!                data from the *cnt.angle.txt files.
!       
!                Make this modification by copying the time/[sat].chan.txt file
!                to angle and just change the ncycles (last value in 1st line) to
!                nsteps value.  Make sure order of operation does times first,
!                then angle afterwards so the chi values are correct for any
!                given cycle.
!
!                Do this in the script nu_plot_angle.sh.  Need to set up the
!                calling script to make angle dependent upon the completion of
!                time with respect to job submission.
!************************************************************************
  implicit none

  integer ftyp,cyc,chan,open_status,nregion,prd
  integer d1, d7, d30, ctr
  integer ges, anl, avg, sdv

  logical exist
  real    rmiss

  character(10) str_nchanl
  character(60) cnt_out_file, pen_out_file, omgnbc_out_file, totcor_out_file
  character(60) omgbc_out_file, fixang_out_file, lapse_out_file, lapse2_out_file
  character(60) mean_out_file, scangl_out_file, clw_out_file, cos_out_file
  character(60) sin_out_file, emiss_out_file, ordang4_out_file
  character(60) ordang3_out_file, ordang2_out_file , ordang1_out_file

  character(40) data_file
!  character(50) scan_sat
  character(len=:),allocatable :: fmt_str(:)

  character(len=10),allocatable,dimension(:)::times
!  character(len=2), allocatable,dimension(:)::useflg
!  character(len=5), allocatable,dimension(:)::chan_nums,wave,freq

  integer luname,ldname,lpname,lsatout,lpenout
  integer ii, jj, i, j, k
  integer nstep, io_stat, nfloats
  integer rgn, astep
  
  real start, stepsz

  ! arrays with file values
  real,allocatable,dimension(:,:,:,:,:)   :: fcnt               !file count
  real,allocatable,dimension(:,:,:,:,:)   :: fpen               !file penalty
  real,allocatable,dimension(:,:,:,:,:,:) :: fomg_nbc           !file omg_nbc 
  real,allocatable,dimension(:,:,:,:,:,:) :: ftot_cor           !file total correction
  real,allocatable,dimension(:,:,:,:,:,:) :: fomg_bc            !file omg_bc 
  real,allocatable,dimension(:,:,:,:,:,:) :: ffixang            !file fixang
  real,allocatable,dimension(:,:,:,:,:,:) :: flapse             !file lapse
  real,allocatable,dimension(:,:,:,:,:,:) :: flapse2            !file lapse2
  real,allocatable,dimension(:,:,:,:,:,:) :: fmean, fscangl, fclw, fcos, fsin, femiss
  real,allocatable,dimension(:,:,:,:,:,:) :: fordang4, fordang3, fordang2, fordang1

  ! arrays for total values (across all cycles, by scan position)
  real,allocatable,dimension(:,:,:,:)  :: ttl_cnt, ttl_pen
  real,allocatable,dimension(:,:,:,:,:):: ttl_omgnbc,ttl_totcor, ttl_omgbc
  real,allocatable,dimension(:,:,:,:,:):: ttl_fixang, ttl_lapse, ttl_lapse2
  real,allocatable,dimension(:,:,:,:,:):: ttl_mean, ttl_scangl, ttl_clw, ttl_cos
  real,allocatable,dimension(:,:,:,:,:):: ttl_sin, ttl_emiss, ttl_ordang4
  real,allocatable,dimension(:,:,:,:,:):: ttl_ordang3, ttl_ordang2, ttl_ordang1

  ! arrays for d1, d7, and d30 values, which are written to data files
  real,allocatable,dimension(:,:,:,:,:)   :: count, penalty 
  real,allocatable,dimension(:,:,:,:,:,:) :: omg_nbc, tot_cor, omg_bc
  real,allocatable,dimension(:,:,:,:,:,:) :: fixang, lapse, lapse2
  real,allocatable,dimension(:,:,:,:,:,:) :: mean, scangl, clw, cos, sin, emiss
  real,allocatable,dimension(:,:,:,:,:,:) :: ordang4, ordang3, ordang2, ordang1

  ! time series values
  real,allocatable,dimension(:,:,:,:)   :: t_cnt
  real,allocatable,dimension(:,:,:,:,:) :: t_fixang, t_lapse, t_lapse2, t_mean
  real,allocatable,dimension(:,:,:,:,:) :: t_scangl, t_clw, t_cos, t_sin, t_emiss
  real,allocatable,dimension(:,:,:,:,:) :: t_ordang4, t_ordang3, t_ordang2, t_ordang1


  real,allocatable,dimension(:,:)    :: timang

!************************************************************************
!  Namelist with defaults
!************************************************************************
   integer               :: nchanl               = 19
   integer               :: ncycle               = 1
   character(15)         :: satname              = 'ssmis_f18'
   real                  :: scan_start           = 0.00
   real                  :: scan_stepsz          = 1.00
   integer               :: scan_nstep           = 90
   namelist /input/ satname, nchanl, ncycle, scan_start, &
                    scan_stepsz, scan_nstep


!************************************************************************
!  Data assignments
!************************************************************************
   data nregion / 5 /
   data luname,ldname,lpname / 5,50,51 /
   data rmiss /-999.0/
   data ges, anl, avg, sdv / 1, 2, 1, 2 /

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


!*******************************************************************************
! Read chan.txt input file, which is the actual channel number for each 
! channel.
!
! This is no longer necessary; we're copying the {sat}.chan.txt from pngs/time
! and swapping the ncycles with number of scan positions (done in scripting).
!*******************************************************************************
!   allocate( chan_nums (nchanl) )
!   allocate( useflg(nchanl), wave(nchanl), freq(nchanl) )
!   allocate( chi(2, nchanl, nregion) )

!   open( lpname, file='chan.txt' )

!   do ii=1,nchanl
!      read(lpname, *) chan_nums(ii), useflg(ii), wave(ii), freq(ii)
!      do jj=1,nregion
!         chi(1,ii,jj) = 0.00
!      end do
!   end do
!   close(lpname)


   start  = scan_start
   stepsz = scan_stepsz
   nstep  = scan_nstep

!************************************************************************
!  Going to need some meta-data. 
!       scaninfo file has number of steps (scanpos)
!                step size, and starting position,
!                and I'll need at least the starting position
!                and number of steps to make this work and
!                to minimize data file size.
!       Does this argue for a chan.txt file like time but 
!        with more goodies in it?
!       Don't think so; this info is satype specific.
!        I could put it as line 0 in all angle.[channel] data files.

!************************************************************************
!  Allocate space for variables and initialize to 0.00 or rmiss
!    note:  first 2 is for ges|anl, 
!           nchanl
!           nregion 
!           nstep is #scan positions 
!               count and penalty have only avg at 1d, 7d, 30d 
!               others have avg & sdv, do I want another variable dimension
!                 or 2 separate variables?
!************************************************************************
   allocate ( timang    (nstep,nchanl) )
   allocate ( fcnt      (2,ncycle,nstep,nchanl,nregion) )
   allocate ( fpen      (2,ncycle,nstep,nchanl,nregion) )
   allocate ( fomg_nbc  (2,ncycle,nstep,nchanl,nregion,2) )
   allocate ( ftot_cor  (2,ncycle,nstep,nchanl,nregion,2) )
   allocate ( fomg_bc   (2,ncycle,nstep,nchanl,nregion,2) )
   allocate ( ffixang   (2,ncycle,nstep,nchanl,nregion,2) )
   allocate ( flapse    (2,ncycle,nstep,nchanl,nregion,2) )
   allocate ( flapse2   (2,ncycle,nstep,nchanl,nregion,2) )
   allocate ( fmean     (2,ncycle,nstep,nchanl,nregion,2) )
   allocate ( fscangl   (2,ncycle,nstep,nchanl,nregion,2) )
   allocate ( fclw      (2,ncycle,nstep,nchanl,nregion,2) )
   allocate ( fcos      (2,ncycle,nstep,nchanl,nregion,2) )
   allocate ( fsin      (2,ncycle,nstep,nchanl,nregion,2) )
   allocate ( femiss    (2,ncycle,nstep,nchanl,nregion,2) )
   allocate ( fordang4  (2,ncycle,nstep,nchanl,nregion,2) )
   allocate ( fordang3  (2,ncycle,nstep,nchanl,nregion,2) )
   allocate ( fordang2  (2,ncycle,nstep,nchanl,nregion,2) )
   allocate ( fordang1  (2,ncycle,nstep,nchanl,nregion,2) )

   allocate ( ttl_cnt    (2,nstep,nchanl,nregion) )
   allocate ( ttl_pen    (2,nstep,nchanl,nregion) )
   allocate ( ttl_omgnbc (2,nstep,nchanl,nregion,2) )
   allocate ( ttl_totcor (2,nstep,nchanl,nregion,2) )
   allocate ( ttl_omgbc  (2,nstep,nchanl,nregion,2) )
   allocate ( ttl_fixang (2,nstep,nchanl,nregion,2) )
   allocate ( ttl_lapse  (2,nstep,nchanl,nregion,2) )
   allocate ( ttl_lapse2 (2,nstep,nchanl,nregion,2) )
   allocate ( ttl_mean   (2,nstep,nchanl,nregion,2) )
   allocate ( ttl_scangl (2,nstep,nchanl,nregion,2) )
   allocate ( ttl_clw    (2,nstep,nchanl,nregion,2) )
   allocate ( ttl_cos    (2,nstep,nchanl,nregion,2) )
   allocate ( ttl_sin    (2,nstep,nchanl,nregion,2) )
   allocate ( ttl_emiss  (2,nstep,nchanl,nregion,2) )
   allocate ( ttl_ordang4(2,nstep,nchanl,nregion,2) )
   allocate ( ttl_ordang3(2,nstep,nchanl,nregion,2) )
   allocate ( ttl_ordang2(2,nstep,nchanl,nregion,2) )
   allocate ( ttl_ordang1(2,nstep,nchanl,nregion,2) )

   allocate ( count     (2,3,nstep,nchanl,nregion) )
   allocate ( penalty   (2,3,nstep,nchanl,nregion) )
   allocate ( omg_nbc   (2,3,nstep,nchanl,nregion,2) )  ! Q:  diminsion by 2 for avg & sdv
   allocate ( tot_cor   (2,3,nstep,nchanl,nregion,2) )   
   allocate ( omg_bc    (2,3,nstep,nchanl,nregion,2) )   
   allocate ( fixang    (2,3,nstep,nchanl,nregion,2) )   
   allocate ( lapse     (2,3,nstep,nchanl,nregion,2) )   
   allocate ( lapse2    (2,3,nstep,nchanl,nregion,2) )    
   allocate ( mean      (2,3,nstep,nchanl,nregion,2) )    
   allocate ( scangl    (2,3,nstep,nchanl,nregion,2) )    
   allocate ( clw       (2,3,nstep,nchanl,nregion,2) )    
   allocate ( cos       (2,3,nstep,nchanl,nregion,2) )    
   allocate ( sin       (2,3,nstep,nchanl,nregion,2) )    
   allocate ( emiss     (2,3,nstep,nchanl,nregion,2) )    
   allocate ( ordang4   (2,3,nstep,nchanl,nregion,2) )    
   allocate ( ordang3   (2,3,nstep,nchanl,nregion,2) )    
   allocate ( ordang2   (2,3,nstep,nchanl,nregion,2) )    
   allocate ( ordang1   (2,3,nstep,nchanl,nregion,2) )    

  do rgn=1,nregion
     write(6,*) 'rgn = ', rgn
  end do

  !--------------------
  ! initialize arrays
  !--------------------
  do ii=1,2            ! ges=1, anl=2
     do cyc=1,ncycle
        do astep=1,nstep                 ! angle step
           do chan=1,nchanl
              do rgn=1,nregion

                 !---------------------------------------
                 !  ttl indicates total values or what's
                 !  accumulated from all data files.
                 !---------------------------------------
                 if( cyc == 1 ) then
                    ttl_cnt(ii,astep,chan,rgn)     = 0.00
                    ttl_pen(ii,astep,chan,rgn)     = 0.00
                    do k=1,2
                       ttl_omgnbc  (ii,astep,chan,rgn,k)  = 0.00
                       ttl_totcor  (ii,astep,chan,rgn,k)  = 0.00
                       ttl_omgbc   (ii,astep,chan,rgn,k)  = 0.00
                       ttl_fixang  (ii,astep,chan,rgn,k)  = 0.00
                       ttl_lapse   (ii,astep,chan,rgn,k)  = 0.00
                       ttl_lapse2  (ii,astep,chan,rgn,k)  = 0.00
                       ttl_mean    (ii,astep,chan,rgn,k)  = 0.00
                       ttl_scangl  (ii,astep,chan,rgn,k)  = 0.00
                       ttl_clw     (ii,astep,chan,rgn,k)  = 0.00
                       ttl_cos     (ii,astep,chan,rgn,k)  = 0.00
                       ttl_sin     (ii,astep,chan,rgn,k)  = 0.00
                       ttl_emiss   (ii,astep,chan,rgn,k)  = 0.00
                       ttl_ordang4 (ii,astep,chan,rgn,k)  = 0.00
                       ttl_ordang3 (ii,astep,chan,rgn,k)  = 0.00
                       ttl_ordang2 (ii,astep,chan,rgn,k)  = 0.00
                       ttl_ordang1 (ii,astep,chan,rgn,k)  = 0.00
                    end do

                    !----------------------------------------------------------
                    ! Use rmiss in the final output arrays.  In the javascript
                    ! on the client I can swap missing values for null. 
                    ! For the stats other than count and penalty there's a 
                    ! chance that 0.00 is a valid value, so that's an
                    ! inappropriate default.
                    !----------------------------------------------------------
                    do jj=1,3
                       count(ii,jj,astep,chan,rgn)   = rmiss
                       penalty(ii,jj,astep,chan,rgn) = rmiss
                       do k=1,2
                          omg_nbc(ii,jj,astep,chan,rgn,k) = rmiss
                          tot_cor(ii,jj,astep,chan,rgn,k) = rmiss
                          omg_bc (ii,jj,astep,chan,rgn,k) = rmiss
                          fixang (ii,jj,astep,chan,rgn,k) = rmiss
                          lapse  (ii,jj,astep,chan,rgn,k) = rmiss
                          lapse2 (ii,jj,astep,chan,rgn,k) = rmiss
                          mean   (ii,jj,astep,chan,rgn,k) = rmiss
                          scangl (ii,jj,astep,chan,rgn,k) = rmiss
                          clw    (ii,jj,astep,chan,rgn,k) = rmiss
                          cos    (ii,jj,astep,chan,rgn,k) = rmiss
                          sin    (ii,jj,astep,chan,rgn,k) = rmiss
                          emiss  (ii,jj,astep,chan,rgn,k) = rmiss
                          ordang4(ii,jj,astep,chan,rgn,k) = rmiss
                          ordang3(ii,jj,astep,chan,rgn,k) = rmiss
                          ordang2(ii,jj,astep,chan,rgn,k) = rmiss
                          ordang1(ii,jj,astep,chan,rgn,k) = rmiss
                       end do
                    end do
                 end if

                 !---------------------------------
                 !  f indicates data _file_ values
                 !---------------------------------
                 fcnt(ii,cyc,astep,chan,rgn)   = 0.00
                 fpen(ii,cyc,astep,chan,rgn)   = 0.00
                 do jj=1,2
                   fomg_nbc(ii,cyc,astep,chan,rgn,jj) = 0.00
                   ftot_cor(ii,cyc,astep,chan,rgn,jj) = 0.00
                   fomg_bc (ii,cyc,astep,chan,rgn,jj) = 0.00
                   ffixang (ii,cyc,astep,chan,rgn,jj) = 0.00
                   flapse  (ii,cyc,astep,chan,rgn,jj) = 0.00
                   flapse2 (ii,cyc,astep,chan,rgn,jj) = 0.00
                   fmean   (ii,cyc,astep,chan,rgn,jj) = 0.00
                   fscangl (ii,cyc,astep,chan,rgn,jj) = 0.00
                   fclw    (ii,cyc,astep,chan,rgn,jj) = 0.00
                   fcos    (ii,cyc,astep,chan,rgn,jj) = 0.00
                   fsin    (ii,cyc,astep,chan,rgn,jj) = 0.00
                   femiss  (ii,cyc,astep,chan,rgn,jj) = 0.00
                   fordang4(ii,cyc,astep,chan,rgn,jj) = 0.00
                   fordang3(ii,cyc,astep,chan,rgn,jj) = 0.00
                   fordang2(ii,cyc,astep,chan,rgn,jj) = 0.00
                   fordang1(ii,cyc,astep,chan,rgn,jj) = 0.00
                 end do

              end do
           end do
        end do
     end do
  end do
  write( 6,* ) 'initialized array variables'


!************************************************************************
! Step through data files, pull out the data and load into
!  accumulator (ttl_) arrays.
!************************************************************************
  do ftyp=1,2
     do cyc=1,ncycle
        if ( ftyp == 1 ) then
           data_file= trim(satname) // '.' // trim(times(cyc)) // '.ieee_d'
        else
           data_file= trim(satname) // '_anl.' // trim(times(cyc)) // '.ieee_d'
        end if

        inquire(file=data_file, exist=exist)

        if ( exist == .TRUE. ) then
!           write( 6,* ) 'reading data file: ', data_file

           open(ldname,file=data_file,form='unformatted')

           read(ldname) ((timang(i,j),i=1,nstep),j=1,nchanl)   ! not used anymore
           do k=1,nregion
              read(ldname) ((fcnt(ftyp,cyc,i,j,k),i=1,nstep),j=1,nchanl)
           end do

           do k=1,nregion
              read(ldname) ((fpen(ftyp,cyc,i,j,k),i=1,nstep),j=1,nchanl)
           end do

           do ii=1,2                ! 1=sum(value) 2=sum(value**2)
              do k=1,nregion
                 read(ldname)((fomg_nbc(ftyp,cyc,i,j,k,ii),i=1,nstep),j=1,nchanl)
                 !-------------------------------------------------------------
                 ! note that when ii=2 this is invalid data until all variables
                 ! are used in the read.  I need the value**2 (sum of squares) 
                 ! to plug into the avgsdv().
                 !-------------------------------------------------------------
              end do
              do k=1,nregion
                 read(ldname)((ftot_cor(ftyp,cyc,i,j,k,ii),i=1,nstep),j=1,nchanl)
              end do
              do k=1,nregion
                 read(ldname)((fomg_bc(ftyp,cyc,i,j,k,ii),i=1,nstep),j=1,nchanl)
              end do
              do k=1,nregion
                 read(ldname)((ffixang(ftyp,cyc,i,j,k,ii),i=1,nstep),j=1,nchanl)
              end do
              do k=1,nregion
                 read(ldname)((flapse(ftyp,cyc,i,j,k,ii),i=1,nstep),j=1,nchanl)
              end do
              do k=1,nregion
                 read(ldname)((flapse2(ftyp,cyc,i,j,k,ii),i=1,nstep),j=1,nchanl)
              end do
              do k=1,nregion
                 read(ldname)((fmean(ftyp,cyc,i,j,k,ii),i=1,nstep),j=1,nchanl)
              end do
              do k=1,nregion
                 read(ldname)((fscangl(ftyp,cyc,i,j,k,ii),i=1,nstep),j=1,nchanl)
              end do
              do k=1,nregion
                 read(ldname)((fclw(ftyp,cyc,i,j,k,ii),i=1,nstep),j=1,nchanl)
              end do
              do k=1,nregion
                 read(ldname)((fcos(ftyp,cyc,i,j,k,ii),i=1,nstep),j=1,nchanl)
              end do
              do k=1,nregion
                 read(ldname)((fsin(ftyp,cyc,i,j,k,ii),i=1,nstep),j=1,nchanl)
              end do
              do k=1,nregion
                 read(ldname)((femiss(ftyp,cyc,i,j,k,ii),i=1,nstep),j=1,nchanl)
              end do
              do k=1,nregion
                 read(ldname)((fordang4(ftyp,cyc,i,j,k,ii),i=1,nstep),j=1,nchanl)
              end do
              do k=1,nregion
                 read(ldname)((fordang3(ftyp,cyc,i,j,k,ii),i=1,nstep),j=1,nchanl)
              end do
              do k=1,nregion
                 read(ldname)((fordang2(ftyp,cyc,i,j,k,ii),i=1,nstep),j=1,nchanl)
              end do
              do k=1,nregion
                 read(ldname)((fordang1(ftyp,cyc,i,j,k,ii),i=1,nstep),j=1,nchanl)
              end do

           end do

           close( ldname )

        end if

     end do       ! loop over cycles (times)
  end do       ! loop over ftyp (ges|anl)

  deallocate ( timang )

  write(6,*)
  write(6,*) 'finished reading data'

!   Q: should I average the data here or do that in javascript?  If js can do it 
!      quickly enough that would be the most flexible solution, allowing for 
!      modifying the date range and re-adjusting the averages on the fly.
!      But hang on.  There is nothing in the data to link a date to the values,
!      so that won't work.  I think I'm going to have to include date
!      information.  Hmmm.  Check how time works for this.
!
!   a: Time writes chan, cyc, data for each channel.  Angle will have bigger
!      files; step*rgn*2 for each cycle, or max of 900 data items/cycle.  Could
!      try this and if it doesn't work then drop back to just storing avg
!      values.  Recommend try a very limited set just count first and prove or
!      disprove the concept.   
!
!   a2: Alternately since the display is done in d1, d7, and d30 increments,
!       could store info for each day.  Recommend going with above solution
!       though for the short term; get that working and modify if someone wants
!       to see more.

!************************************************************************
!  Process data for the 3 time periods (d1, d7, d30)
!************************************************************************
  d1  =4
  d7  =28
  d30 = ncycle

  do ftyp=1,2
     do chan=1,nchanl
        do astep=1,nstep
           do rgn=1, nregion
              do cyc=1, ncycle

                 !*******************************************************************
                 ! accumulate totals by ftype, astep, chan, region across all cycles
                 !*******************************************************************
                 ttl_cnt(ftyp, astep, chan, rgn) = ttl_cnt(ftyp, astep, chan, rgn) + &
                                               fcnt(ftyp, cyc, astep, chan, rgn)    
                 ttl_pen(ftyp, astep, chan, rgn) = ttl_pen(ftyp, astep, chan, rgn) + & 
                                               fpen(ftyp, cyc, astep, chan, rgn)    
                 do k=1,2
                    ttl_omgnbc(ftyp, astep, chan, rgn,k) = &
                                               ttl_omgnbc(ftyp, astep, chan, rgn,k) + & 
                                               fomg_nbc(ftyp, cyc, astep, chan, rgn,k)    
                    ttl_totcor(ftyp, astep, chan, rgn,k) = &
                                               ttl_totcor(ftyp, astep, chan, rgn,k) + & 
                                               ftot_cor(ftyp, cyc, astep, chan, rgn,k)    
                    ttl_omgbc (ftyp, astep, chan, rgn,k) = &
                                               ttl_omgbc(ftyp, astep, chan, rgn,k) + & 
                                               fomg_bc(ftyp, cyc, astep, chan, rgn,k)    
                    ttl_fixang (ftyp, astep, chan, rgn,k) = &
                                               ttl_fixang(ftyp, astep, chan, rgn,k) + & 
                                               ffixang(ftyp, cyc, astep, chan, rgn,k)    
                    ttl_lapse  (ftyp, astep, chan, rgn,k) = &
                                               ttl_lapse (ftyp, astep, chan, rgn,k) + & 
                                               flapse(ftyp, cyc, astep, chan, rgn,k)    
                    ttl_lapse2 (ftyp, astep, chan, rgn,k) = &
                                               ttl_lapse2(ftyp, astep, chan, rgn,k) + & 
                                               flapse2(ftyp, cyc, astep, chan, rgn,k)    
                    ttl_mean   (ftyp, astep, chan, rgn,k) = &
                                               ttl_mean(ftyp, astep, chan, rgn,k) + & 
                                               fmean(ftyp, cyc, astep, chan, rgn,k)    
                    ttl_scangl (ftyp, astep, chan, rgn,k) = &
                                               ttl_scangl(ftyp, astep, chan, rgn,k) + & 
                                               fscangl(ftyp, cyc, astep, chan, rgn,k)    
                    ttl_clw    (ftyp, astep, chan, rgn,k) = &
                                               ttl_clw(ftyp, astep, chan, rgn,k) + & 
                                               fclw(ftyp, cyc, astep, chan, rgn,k)    
                    ttl_cos    (ftyp, astep, chan, rgn,k) = &
                                               ttl_cos(ftyp, astep, chan, rgn,k) + & 
                                               fcos(ftyp, cyc, astep, chan, rgn,k)    
                    ttl_sin    (ftyp, astep, chan, rgn,k) = &
                                               ttl_sin(ftyp, astep, chan, rgn,k) + & 
                                               fsin(ftyp, cyc, astep, chan, rgn,k)    
                    ttl_emiss  (ftyp, astep, chan, rgn,k) = &
                                               ttl_emiss(ftyp, astep, chan, rgn,k) + & 
                                               femiss(ftyp, cyc, astep, chan, rgn,k)    
                    ttl_ordang4 (ftyp, astep, chan, rgn,k) = &
                                               ttl_ordang4(ftyp, astep, chan, rgn,k) + & 
                                               fordang4(ftyp, cyc, astep, chan, rgn,k)    
                    ttl_ordang3 (ftyp, astep, chan, rgn,k) = &
                                               ttl_ordang3(ftyp, astep, chan, rgn,k) + & 
                                               fordang3(ftyp, cyc, astep, chan, rgn,k)    
                    ttl_ordang2 (ftyp, astep, chan, rgn,k) = &
                                               ttl_ordang2(ftyp, astep, chan, rgn,k) + & 
                                               fordang2(ftyp, cyc, astep, chan, rgn,k)    
                    ttl_ordang1 (ftyp, astep, chan, rgn,k) = &
                                               ttl_ordang1(ftyp, astep, chan, rgn,k) + & 
                                               fordang1(ftyp, cyc, astep, chan, rgn,k)    
                 end do

                 !*************************************
                 ! generate 1, 7, and 30 day averages
                 !*************************************
                 if( cyc == d1 ) then  
                    if( ttl_cnt(ftyp,astep,chan,rgn) > 0.00 ) then 
                       count(ftyp,1,astep,chan,rgn)   = ttl_cnt(ftyp,astep,chan,rgn)/d1
                       penalty(ftyp,1,astep,chan,rgn) = ttl_pen(ftyp,astep,chan,rgn) / &
                                                        ttl_cnt(ftyp,astep,chan,rgn)

                       omg_nbc(ftyp,1,astep,chan,rgn,1) = ttl_omgnbc(ftyp,astep,chan,rgn,1)
                       omg_nbc(ftyp,1,astep,chan,rgn,2) = ttl_omgnbc(ftyp,astep,chan,rgn,2)
                       call avgsdv( ttl_cnt(ftyp,astep,chan,rgn),   &
                                    omg_nbc(ftyp,1,astep,chan,rgn,1), &
                                    omg_nbc(ftyp,1,astep,chan,rgn,2), rmiss )
                       tot_cor(ftyp,1,astep,chan,rgn,1) = ttl_totcor(ftyp,astep,chan,rgn,1)
                       tot_cor(ftyp,1,astep,chan,rgn,2) = ttl_totcor(ftyp,astep,chan,rgn,2)
                       call avgsdv( ttl_cnt(ftyp,astep,chan,rgn),   &
                                    tot_cor(ftyp,1,astep,chan,rgn,1), &
                                    tot_cor(ftyp,1,astep,chan,rgn,2), rmiss )
                       omg_bc (ftyp,1,astep,chan,rgn,1) = ttl_omgbc(ftyp,astep,chan,rgn,1)
                       omg_bc (ftyp,1,astep,chan,rgn,2) = ttl_omgbc(ftyp,astep,chan,rgn,2)
                       call avgsdv( ttl_cnt(ftyp,astep,chan,rgn),   &
                                    omg_bc(ftyp,1,astep,chan,rgn,1), &
                                    omg_bc(ftyp,1,astep,chan,rgn,2), rmiss )
                       fixang (ftyp,1,astep,chan,rgn,1) = ttl_fixang(ftyp,astep,chan,rgn,1)
                       fixang (ftyp,1,astep,chan,rgn,2) = ttl_fixang(ftyp,astep,chan,rgn,2)
                       call avgsdv( ttl_cnt(ftyp,astep,chan,rgn),   &
                                    fixang(ftyp,1,astep,chan,rgn,1), &
                                    fixang(ftyp,1,astep,chan,rgn,2), rmiss )

                       lapse  (ftyp,1,astep,chan,rgn,1) = ttl_lapse(ftyp,astep,chan,rgn,1)
                       lapse  (ftyp,1,astep,chan,rgn,2) = ttl_lapse(ftyp,astep,chan,rgn,2)
                       call avgsdv( ttl_cnt(ftyp,astep,chan,rgn),   &
                                    lapse(ftyp,1,astep,chan,rgn,1), &
                                    lapse(ftyp,1,astep,chan,rgn,2), rmiss )

                       lapse2 (ftyp,1,astep,chan,rgn,1) = ttl_lapse2(ftyp,astep,chan,rgn,1)
                       lapse2 (ftyp,1,astep,chan,rgn,2) = ttl_lapse2(ftyp,astep,chan,rgn,2)
                       call avgsdv( ttl_cnt(ftyp,astep,chan,rgn),   &
                                    lapse2(ftyp,1,astep,chan,rgn,1), &
                                    lapse2(ftyp,1,astep,chan,rgn,2), rmiss )

                       mean   (ftyp,1,astep,chan,rgn,1) = ttl_mean(ftyp,astep,chan,rgn,1)
                       mean   (ftyp,1,astep,chan,rgn,2) = ttl_mean(ftyp,astep,chan,rgn,2)
                       call avgsdv( ttl_cnt(ftyp,astep,chan,rgn),   &
                                    mean  (ftyp,1,astep,chan,rgn,1), &
                                    mean  (ftyp,1,astep,chan,rgn,2), rmiss )

                       scangl (ftyp,1,astep,chan,rgn,1) = ttl_scangl(ftyp,astep,chan,rgn,1)
                       scangl (ftyp,1,astep,chan,rgn,2) = ttl_scangl(ftyp,astep,chan,rgn,2)
                       call avgsdv( ttl_cnt(ftyp,astep,chan,rgn),   &
                                    scangl(ftyp,1,astep,chan,rgn,1), &
                                    scangl(ftyp,1,astep,chan,rgn,2), rmiss )

                       clw    (ftyp,1,astep,chan,rgn,1) = ttl_clw(ftyp,astep,chan,rgn,1)
                       clw    (ftyp,1,astep,chan,rgn,2) = ttl_clw(ftyp,astep,chan,rgn,2)
                       call avgsdv( ttl_cnt(ftyp,astep,chan,rgn),   &
                                    clw(ftyp,1,astep,chan,rgn,1), &
                                    clw(ftyp,1,astep,chan,rgn,2), rmiss )

                       cos    (ftyp,1,astep,chan,rgn,1) = ttl_cos(ftyp,astep,chan,rgn,1)
                       cos    (ftyp,1,astep,chan,rgn,2) = ttl_cos(ftyp,astep,chan,rgn,2)
                       call avgsdv( ttl_cnt(ftyp,astep,chan,rgn),   &
                                    cos(ftyp,1,astep,chan,rgn,1), &
                                    cos(ftyp,1,astep,chan,rgn,2), rmiss )

                       sin    (ftyp,1,astep,chan,rgn,1) = ttl_sin(ftyp,astep,chan,rgn,1)
                       sin    (ftyp,1,astep,chan,rgn,2) = ttl_sin(ftyp,astep,chan,rgn,2)
                       call avgsdv( ttl_cnt(ftyp,astep,chan,rgn),   &
                                    sin(ftyp,1,astep,chan,rgn,1), &
                                    sin(ftyp,1,astep,chan,rgn,2), rmiss )

                       emiss  (ftyp,1,astep,chan,rgn,1) = ttl_emiss(ftyp,astep,chan,rgn,1)
                       emiss  (ftyp,1,astep,chan,rgn,2) = ttl_emiss(ftyp,astep,chan,rgn,2)
                       call avgsdv( ttl_cnt(ftyp,astep,chan,rgn),   &
                                    emiss(ftyp,1,astep,chan,rgn,1), &
                                    emiss(ftyp,1,astep,chan,rgn,2), rmiss )

                       ordang4(ftyp,1,astep,chan,rgn,1) = ttl_ordang4(ftyp,astep,chan,rgn,1)
                       ordang4(ftyp,1,astep,chan,rgn,2) = ttl_ordang4(ftyp,astep,chan,rgn,2)
                       call avgsdv( ttl_cnt(ftyp,astep,chan,rgn),   &
                                    ordang4(ftyp,1,astep,chan,rgn,1), &
                                    ordang4(ftyp,1,astep,chan,rgn,2), rmiss )

                       ordang3(ftyp,1,astep,chan,rgn,1) = ttl_ordang3(ftyp,astep,chan,rgn,1)
                       ordang3(ftyp,1,astep,chan,rgn,2) = ttl_ordang3(ftyp,astep,chan,rgn,2)
                       call avgsdv( ttl_cnt(ftyp,astep,chan,rgn),   &
                                    ordang3(ftyp,1,astep,chan,rgn,1), &
                                    ordang3(ftyp,1,astep,chan,rgn,2), rmiss )

                       ordang2(ftyp,1,astep,chan,rgn,1) = ttl_ordang2(ftyp,astep,chan,rgn,1)
                       ordang2(ftyp,1,astep,chan,rgn,2) = ttl_ordang2(ftyp,astep,chan,rgn,2)
                       call avgsdv( ttl_cnt(ftyp,astep,chan,rgn),   &
                                    ordang2(ftyp,1,astep,chan,rgn,1), &
                                    ordang2(ftyp,1,astep,chan,rgn,2), rmiss )

                       ordang1(ftyp,1,astep,chan,rgn,1) = ttl_ordang1(ftyp,astep,chan,rgn,1)
                       ordang1(ftyp,1,astep,chan,rgn,2) = ttl_ordang1(ftyp,astep,chan,rgn,2)
                       call avgsdv( ttl_cnt(ftyp,astep,chan,rgn),   &
                                    ordang1(ftyp,1,astep,chan,rgn,1), &
                                    ordang1(ftyp,1,astep,chan,rgn,2), rmiss )
                    end if

                 else if( cyc == d7 ) then
                    if( ttl_cnt(ftyp,astep,chan,rgn) > 0.00 ) then 
                       count(ftyp,2,astep,chan,rgn)   = ttl_cnt(ftyp,astep,chan,rgn)/d7 
                       penalty(ftyp,2,astep,chan,rgn) = ttl_pen(ftyp,astep,chan,rgn) / &
                                                        ttl_cnt(ftyp,astep,chan,rgn)

                       omg_nbc(ftyp,2,astep,chan,rgn,1) = ttl_omgnbc(ftyp,astep,chan,rgn,1)
                       omg_nbc(ftyp,2,astep,chan,rgn,2) = ttl_omgnbc(ftyp,astep,chan,rgn,2)
                       call avgsdv( ttl_cnt(ftyp,astep,chan,rgn),   &
                                    omg_nbc(ftyp,2,astep,chan,rgn,1), &
                                    omg_nbc(ftyp,2,astep,chan,rgn,2), rmiss )
                       tot_cor(ftyp,2,astep,chan,rgn,1) = ttl_totcor(ftyp,astep,chan,rgn,1)
                       tot_cor(ftyp,2,astep,chan,rgn,2) = ttl_totcor(ftyp,astep,chan,rgn,2)
                       call avgsdv( ttl_cnt(ftyp,astep,chan,rgn),   &
                                    tot_cor(ftyp,2,astep,chan,rgn,1), &
                                    tot_cor(ftyp,2,astep,chan,rgn,2), rmiss )
                       omg_bc (ftyp,2,astep,chan,rgn,1) = ttl_omgbc(ftyp,astep,chan,rgn,1)
                       omg_bc (ftyp,2,astep,chan,rgn,2) = ttl_omgbc(ftyp,astep,chan,rgn,2)
                       call avgsdv( ttl_cnt(ftyp,astep,chan,rgn),   &
                                    omg_bc(ftyp,2,astep,chan,rgn,1), &
                                    omg_bc(ftyp,2,astep,chan,rgn,2), rmiss )
                       fixang (ftyp,2,astep,chan,rgn,1) = ttl_fixang(ftyp,astep,chan,rgn,1)
                       fixang (ftyp,2,astep,chan,rgn,2) = ttl_fixang(ftyp,astep,chan,rgn,2)
                       call avgsdv( ttl_cnt(ftyp,astep,chan,rgn),   &
                                    fixang(ftyp,2,astep,chan,rgn,1), &
                                    fixang(ftyp,2,astep,chan,rgn,2), rmiss )
                       lapse  (ftyp,2,astep,chan,rgn,1) = ttl_lapse(ftyp,astep,chan,rgn,1)
                       lapse  (ftyp,2,astep,chan,rgn,2) = ttl_lapse(ftyp,astep,chan,rgn,2)
                       call avgsdv( ttl_cnt(ftyp,astep,chan,rgn),   &
                                    lapse(ftyp,2,astep,chan,rgn,1), &
                                    lapse(ftyp,2,astep,chan,rgn,2), rmiss )

                       lapse2 (ftyp,2,astep,chan,rgn,1) = ttl_lapse2(ftyp,astep,chan,rgn,1)
                       lapse2 (ftyp,2,astep,chan,rgn,2) = ttl_lapse2(ftyp,astep,chan,rgn,2)
                       call avgsdv( ttl_cnt(ftyp,astep,chan,rgn),   &
                                    lapse2(ftyp,2,astep,chan,rgn,1), &
                                    lapse2(ftyp,2,astep,chan,rgn,2), rmiss )

                       mean   (ftyp,2,astep,chan,rgn,1) = ttl_mean(ftyp,astep,chan,rgn,1)
                       mean   (ftyp,2,astep,chan,rgn,2) = ttl_mean(ftyp,astep,chan,rgn,2)
                       call avgsdv( ttl_cnt(ftyp,astep,chan,rgn),   &
                                    mean  (ftyp,2,astep,chan,rgn,1), &
                                    mean  (ftyp,2,astep,chan,rgn,2), rmiss )

                       scangl (ftyp,2,astep,chan,rgn,1) = ttl_scangl(ftyp,astep,chan,rgn,1)
                       scangl (ftyp,2,astep,chan,rgn,2) = ttl_scangl(ftyp,astep,chan,rgn,2)
                       call avgsdv( ttl_cnt(ftyp,astep,chan,rgn),   &
                                    scangl(ftyp,2,astep,chan,rgn,1), &
                                    scangl(ftyp,2,astep,chan,rgn,2), rmiss )

                       clw    (ftyp,2,astep,chan,rgn,1) = ttl_clw(ftyp,astep,chan,rgn,1)
                       clw    (ftyp,2,astep,chan,rgn,2) = ttl_clw(ftyp,astep,chan,rgn,2)
                       call avgsdv( ttl_cnt(ftyp,astep,chan,rgn),   &
                                    clw(ftyp,2,astep,chan,rgn,1), &
                                    clw(ftyp,2,astep,chan,rgn,2), rmiss )

                       cos    (ftyp,2,astep,chan,rgn,1) = ttl_cos(ftyp,astep,chan,rgn,1)
                       cos    (ftyp,2,astep,chan,rgn,2) = ttl_cos(ftyp,astep,chan,rgn,2)
                       call avgsdv( ttl_cnt(ftyp,astep,chan,rgn),   &
                                    cos(ftyp,2,astep,chan,rgn,1), &
                                    cos(ftyp,2,astep,chan,rgn,2), rmiss )

                       sin    (ftyp,2,astep,chan,rgn,1) = ttl_sin(ftyp,astep,chan,rgn,1)
                       sin    (ftyp,2,astep,chan,rgn,2) = ttl_sin(ftyp,astep,chan,rgn,2)
                       call avgsdv( ttl_cnt(ftyp,astep,chan,rgn),   &
                                    sin(ftyp,2,astep,chan,rgn,1), &
                                    sin(ftyp,2,astep,chan,rgn,2), rmiss )

                       emiss  (ftyp,2,astep,chan,rgn,1) = ttl_emiss(ftyp,astep,chan,rgn,1)
                       emiss  (ftyp,2,astep,chan,rgn,2) = ttl_emiss(ftyp,astep,chan,rgn,2)
                       call avgsdv( ttl_cnt(ftyp,astep,chan,rgn),   &
                                    emiss(ftyp,2,astep,chan,rgn,1), &
                                    emiss(ftyp,2,astep,chan,rgn,2), rmiss )

                       ordang4(ftyp,2,astep,chan,rgn,1) = ttl_ordang4(ftyp,astep,chan,rgn,1)
                       ordang4(ftyp,2,astep,chan,rgn,2) = ttl_ordang4(ftyp,astep,chan,rgn,2)
                       call avgsdv( ttl_cnt(ftyp,astep,chan,rgn),   &
                                    ordang4(ftyp,2,astep,chan,rgn,1), &
                                    ordang4(ftyp,2,astep,chan,rgn,2), rmiss )

                       ordang3(ftyp,2,astep,chan,rgn,1) = ttl_ordang3(ftyp,astep,chan,rgn,1)
                       ordang3(ftyp,2,astep,chan,rgn,2) = ttl_ordang3(ftyp,astep,chan,rgn,2)
                       call avgsdv( ttl_cnt(ftyp,astep,chan,rgn),   &
                                    ordang3(ftyp,2,astep,chan,rgn,1), &
                                    ordang3(ftyp,2,astep,chan,rgn,2), rmiss )

                       ordang2(ftyp,2,astep,chan,rgn,1) = ttl_ordang2(ftyp,astep,chan,rgn,1)
                       ordang2(ftyp,2,astep,chan,rgn,2) = ttl_ordang2(ftyp,astep,chan,rgn,2)
                       call avgsdv( ttl_cnt(ftyp,astep,chan,rgn),   &
                                    ordang2(ftyp,2,astep,chan,rgn,1), &
                                    ordang2(ftyp,2,astep,chan,rgn,2), rmiss )

                       ordang1(ftyp,2,astep,chan,rgn,1) = ttl_ordang1(ftyp,astep,chan,rgn,1)
                       ordang1(ftyp,2,astep,chan,rgn,2) = ttl_ordang1(ftyp,astep,chan,rgn,2)
                       call avgsdv( ttl_cnt(ftyp,astep,chan,rgn),   &
                                    ordang1(ftyp,2,astep,chan,rgn,1), &
                                    ordang1(ftyp,2,astep,chan,rgn,2), rmiss )
                    end if

                 else if( cyc == d30 ) then
                    if( ttl_cnt(ftyp,astep,chan,rgn) > 0.00 ) then 
                       count(ftyp,3,astep,chan,rgn)   = ttl_cnt(ftyp,astep,chan,rgn)/d30
                       penalty(ftyp,3,astep,chan,rgn) = ttl_pen(ftyp,astep,chan,rgn) / &
                                                        ttl_cnt(ftyp,astep,chan,rgn)

                       omg_nbc(ftyp,3,astep,chan,rgn,1) = ttl_omgnbc(ftyp,astep,chan,rgn,1)
                       omg_nbc(ftyp,3,astep,chan,rgn,2) = ttl_omgnbc(ftyp,astep,chan,rgn,2)
                       call avgsdv( ttl_cnt(ftyp,astep,chan,rgn),   &
                                    omg_nbc(ftyp,3,astep,chan,rgn,1), &
                                    omg_nbc(ftyp,3,astep,chan,rgn,2), rmiss )

                       tot_cor(ftyp,3,astep,chan,rgn,1) = ttl_totcor(ftyp,astep,chan,rgn,1)
                       tot_cor(ftyp,3,astep,chan,rgn,2) = ttl_totcor(ftyp,astep,chan,rgn,2)
                       call avgsdv( ttl_cnt(ftyp,astep,chan,rgn),   &
                                    tot_cor(ftyp,3,astep,chan,rgn,1), &
                                    tot_cor(ftyp,3,astep,chan,rgn,2), rmiss )

                       omg_bc (ftyp,3,astep,chan,rgn,1) = ttl_omgbc(ftyp,astep,chan,rgn,1)
                       omg_bc (ftyp,3,astep,chan,rgn,2) = ttl_omgbc(ftyp,astep,chan,rgn,2)
                       call avgsdv( ttl_cnt(ftyp,astep,chan,rgn),   &
                                    omg_bc(ftyp,3,astep,chan,rgn,1), &
                                    omg_bc(ftyp,3,astep,chan,rgn,2), rmiss )

                       fixang (ftyp,3,astep,chan,rgn,1) = ttl_fixang(ftyp,astep,chan,rgn,1)
                       fixang (ftyp,3,astep,chan,rgn,2) = ttl_fixang(ftyp,astep,chan,rgn,2)
                       call avgsdv( ttl_cnt(ftyp,astep,chan,rgn),   &
                                    fixang(ftyp,3,astep,chan,rgn,1), &
                                    fixang(ftyp,3,astep,chan,rgn,2), rmiss )

                       lapse  (ftyp,3,astep,chan,rgn,1) = ttl_lapse(ftyp,astep,chan,rgn,1)
                       lapse  (ftyp,3,astep,chan,rgn,2) = ttl_lapse(ftyp,astep,chan,rgn,2)
                       call avgsdv( ttl_cnt(ftyp,astep,chan,rgn),   &
                                    lapse(ftyp,3,astep,chan,rgn,1), &
                                    lapse(ftyp,3,astep,chan,rgn,2), rmiss )

                       lapse2 (ftyp,3,astep,chan,rgn,1) = ttl_lapse2(ftyp,astep,chan,rgn,1)
                       lapse2 (ftyp,3,astep,chan,rgn,2) = ttl_lapse2(ftyp,astep,chan,rgn,2)
                       call avgsdv( ttl_cnt(ftyp,astep,chan,rgn),   &
                                    lapse2(ftyp,3,astep,chan,rgn,1), &
                                    lapse2(ftyp,3,astep,chan,rgn,2), rmiss )

                       mean   (ftyp,3,astep,chan,rgn,1) = ttl_mean(ftyp,astep,chan,rgn,1)
                       mean   (ftyp,3,astep,chan,rgn,2) = ttl_mean(ftyp,astep,chan,rgn,2)
                       call avgsdv( ttl_cnt(ftyp,astep,chan,rgn),   &
                                    mean  (ftyp,3,astep,chan,rgn,1), &
                                    mean  (ftyp,3,astep,chan,rgn,2), rmiss )

                       scangl (ftyp,3,astep,chan,rgn,1) = ttl_scangl(ftyp,astep,chan,rgn,1)
                       scangl (ftyp,3,astep,chan,rgn,2) = ttl_scangl(ftyp,astep,chan,rgn,2)
                       call avgsdv( ttl_cnt(ftyp,astep,chan,rgn),   &
                                    scangl(ftyp,3,astep,chan,rgn,1), &
                                    scangl(ftyp,3,astep,chan,rgn,2), rmiss )

                       clw    (ftyp,3,astep,chan,rgn,1) = ttl_clw(ftyp,astep,chan,rgn,1)
                       clw    (ftyp,3,astep,chan,rgn,2) = ttl_clw(ftyp,astep,chan,rgn,2)
                       call avgsdv( ttl_cnt(ftyp,astep,chan,rgn),   &
                                    clw(ftyp,3,astep,chan,rgn,1), &
                                    clw(ftyp,3,astep,chan,rgn,2), rmiss )

                       cos    (ftyp,3,astep,chan,rgn,1) = ttl_cos(ftyp,astep,chan,rgn,1)
                       cos    (ftyp,3,astep,chan,rgn,2) = ttl_cos(ftyp,astep,chan,rgn,2)
                       call avgsdv( ttl_cnt(ftyp,astep,chan,rgn),   &
                                    cos(ftyp,3,astep,chan,rgn,1), &
                                    cos(ftyp,3,astep,chan,rgn,2), rmiss )

                       sin    (ftyp,3,astep,chan,rgn,1) = ttl_sin(ftyp,astep,chan,rgn,1)
                       sin    (ftyp,3,astep,chan,rgn,2) = ttl_sin(ftyp,astep,chan,rgn,2)
                       call avgsdv( ttl_cnt(ftyp,astep,chan,rgn),   &
                                    sin(ftyp,3,astep,chan,rgn,1), &
                                    sin(ftyp,3,astep,chan,rgn,2), rmiss )

                       emiss  (ftyp,3,astep,chan,rgn,1) = ttl_emiss(ftyp,astep,chan,rgn,1)
                       emiss  (ftyp,3,astep,chan,rgn,2) = ttl_emiss(ftyp,astep,chan,rgn,2)
                       call avgsdv( ttl_cnt(ftyp,astep,chan,rgn),   &
                                    emiss(ftyp,3,astep,chan,rgn,1), &
                                    emiss(ftyp,3,astep,chan,rgn,2), rmiss )

                       ordang4(ftyp,3,astep,chan,rgn,1) = ttl_ordang4(ftyp,astep,chan,rgn,1)
                       ordang4(ftyp,3,astep,chan,rgn,2) = ttl_ordang4(ftyp,astep,chan,rgn,2)
                       call avgsdv( ttl_cnt(ftyp,astep,chan,rgn),   &
                                    ordang4(ftyp,3,astep,chan,rgn,1), &
                                    ordang4(ftyp,3,astep,chan,rgn,2), rmiss )

                       ordang3(ftyp,3,astep,chan,rgn,1) = ttl_ordang3(ftyp,astep,chan,rgn,1)
                       ordang3(ftyp,3,astep,chan,rgn,2) = ttl_ordang3(ftyp,astep,chan,rgn,2)
                       call avgsdv( ttl_cnt(ftyp,astep,chan,rgn),   &
                                    ordang3(ftyp,3,astep,chan,rgn,1), &
                                    ordang3(ftyp,3,astep,chan,rgn,2), rmiss )

                       ordang2(ftyp,3,astep,chan,rgn,1) = ttl_ordang2(ftyp,astep,chan,rgn,1)
                       ordang2(ftyp,3,astep,chan,rgn,2) = ttl_ordang2(ftyp,astep,chan,rgn,2)
                       call avgsdv( ttl_cnt(ftyp,astep,chan,rgn),   &
                                    ordang2(ftyp,3,astep,chan,rgn,1), &
                                    ordang2(ftyp,3,astep,chan,rgn,2), rmiss )

                       ordang1(ftyp,3,astep,chan,rgn,1) = ttl_ordang1(ftyp,astep,chan,rgn,1)
                       ordang1(ftyp,3,astep,chan,rgn,2) = ttl_ordang1(ftyp,astep,chan,rgn,2)
                       call avgsdv( ttl_cnt(ftyp,astep,chan,rgn),   &
                                    ordang1(ftyp,3,astep,chan,rgn,1), &
                                    ordang1(ftyp,3,astep,chan,rgn,2), rmiss )
                    end if   

                 end if
 
 
              end do       ! cycle
           end do          ! region
        end do             ! astep
     end do                ! chan
  end do                   ! ftyp


!  deallocate( fcnt, fpen, fomg_nbc, ftot_cor, fomg_bc, ffixang, flapse, flapse2, fmean )
!  deallocate( fscangl, fclw, fcos, fsin, femiss, fordang4, fordang3, fordang2, fordang1 )

  deallocate( ttl_cnt, ttl_pen, ttl_omgnbc, ttl_totcor, ttl_omgbc, ttl_fixang )
  deallocate( ttl_lapse, ttl_lapse2, ttl_mean, ttl_scangl, ttl_clw, ttl_cos )
  deallocate( ttl_sin, ttl_emiss, ttl_ordang4, ttl_ordang3, ttl_ordang2, ttl_ordang1 )

!************************************************************************
!  Write output to channel specific data files.
!        output file names are [satype].[chan].cnt.angle.txt
!************************************************************************

  !************************
  !  build CSV file format
  !************************
  774 FORMAT( (i10,',',a12,',',f10.2,',',f10.2,',',i10) )

  nfloats = nstep*nregion
  allocate(character(12) :: fmt_str(nfloats))

  fmt_str(1) = trim("(") // trim("f10.3") // trim(",',',")
  do ii=2, nfloats-1
     fmt_str(ii) = trim("f10.3") // trim(",',',")
  end do
  fmt_str(nfloats) = trim("f10.3") // trim(")")


  !**********************************************
  !  loop over channels & generate output files
  !**********************************************
  do chan=1,nchanl
     write(str_nchanl, '(i10)') chan-1
     cnt_out_file    = trim(satname) // '.' // trim(adjustl(str_nchanl)) // '.cnt.angle.txt'
     pen_out_file    = trim(satname) // '.' // trim(adjustl(str_nchanl)) // '.pen.angle.txt'
     omgnbc_out_file = trim(satname) // '.' // trim(adjustl(str_nchanl)) &
                                    // '.omgnbc.angle.txt'
     totcor_out_file = trim(satname) // '.' // trim(adjustl(str_nchanl)) &
                                    // '.totcor.angle.txt'
     omgbc_out_file  = trim(satname) // '.' // trim(adjustl(str_nchanl)) &
                                    // '.omgbc.angle.txt'
     fixang_out_file = trim(satname) // '.' // trim(adjustl(str_nchanl)) &
                                    // '.fixang.angle.txt'
     lapse_out_file  = trim(satname) // '.' // trim(adjustl(str_nchanl)) &
                                    // '.lapse.angle.txt'
     lapse2_out_file = trim(satname) // '.' // trim(adjustl(str_nchanl)) &
                                    // '.lapse2.angle.txt'
     mean_out_file   = trim(satname) // '.' // trim(adjustl(str_nchanl)) &
                                    // '.mean.angle.txt'
     scangl_out_file = trim(satname) // '.' // trim(adjustl(str_nchanl)) &
                                    // '.scangl.angle.txt'
     clw_out_file    = trim(satname) // '.' // trim(adjustl(str_nchanl)) &
                                    // '.clw.angle.txt'
     cos_out_file    = trim(satname) // '.' // trim(adjustl(str_nchanl)) &
                                    // '.cos.angle.txt'
     sin_out_file    = trim(satname) // '.' // trim(adjustl(str_nchanl)) &
                                    // '.sin.angle.txt'
     emiss_out_file  = trim(satname) // '.' // trim(adjustl(str_nchanl)) &
                                    // '.emiss.angle.txt'
     ordang4_out_file= trim(satname) // '.' // trim(adjustl(str_nchanl)) &
                                    // '.ordang4.angle.txt'
     ordang3_out_file= trim(satname) // '.' // trim(adjustl(str_nchanl)) &
                                    // '.ordang3.angle.txt'
     ordang2_out_file= trim(satname) // '.' // trim(adjustl(str_nchanl)) &
                                    // '.ordang2.angle.txt'
     ordang1_out_file= trim(satname) // '.' // trim(adjustl(str_nchanl)) &
                                    // '.ordang1.angle.txt'

     lsatout = 70 + chan
     open(lsatout,file=cnt_out_file,iostat=open_status, &
                             action='write',status='new',form='formatted')
     write(6,*)' cnt_out_file opened, status:  ', open_status

     !----------------------------------------------------------------------------
     !  data is arranged:
     !                   line 1, chan, times, start, stepsz, nstep
     !                   line 2:  ges, time period 1 for each step and region
     !                   line 3:  ges, time period 2 for each step and region
     !                   line 4:  ges, time period 3 for each step and region
     !                   line 5:  anl, time period 1 for each step and region
     !                   line 6:  anl, time period 2 for each step and region
     !                   line 7:  anl, time period 3 for each step and region
     !
     !  Ideally I would start each line with a self-documenting tag to indicate
     !  what the data is (e.g. DAY7AVG, DAY30SDV).  Need to figure out how to
     !  implement that with Fortran's lousy formating.  Maybe line w/ tag and
     !  then line with data would be good enough, and read them in groups of 2?
     !  That would avoid having to read the file in javascript in any precise
     !  order.
     write(lsatout,774)  chan, times(1), start, stepsz, nstep

     do prd=1,3
        write(lsatout, fmt_str) ((count(1,prd,i,chan,k),i=1,nstep),k=1,nregion)
     end do

     do prd=1,3
        write(lsatout, fmt_str) ((count(2,prd,i,chan,k),i=1,nstep),k=1,nregion)
     end do
     close( lsatout )


     !----------------------------------------------------------------------------
     ! write penalty data files
     !
     open(lsatout,file=pen_out_file,iostat=open_status, &
                             action='write',status='new',form='formatted')
     write(6,*)' pen_out_file opened, status:  ', open_status

     write(lsatout,774)  chan, times(1), start, stepsz, nstep
     do prd=1,3
        write(lsatout, fmt_str) ((penalty(1,prd,i,chan,k),i=1,nstep),k=1,nregion)
     end do

     do prd=1,3
        write(lsatout, fmt_str) ((penalty(2,prd,i,chan,k),i=1,nstep),k=1,nregion)
     end do

     close( lsatout )


     !----------------------------------------------------------------------------
     ! write omg_nbc data files (rows 1-3 are ges data, 4-6 analysis)
     !
     !    row 0    = chan, time, start, stepsz, nstep
     !    row 1    = ges pd1, avg 
     !    row 2    = ges pd1, sdv
     !    rows 3,4 = ges pd2, avg, sdv
     !    rows 5,6 = ges pd3, avg, sdv
     !    rows 7,13= anl, pd1-3, avg, sdv
     open(lsatout,file=omgnbc_out_file,iostat=open_status, &
                             action='write',status='new',form='formatted')
     write(6,*)' omgnbc_out_file opened, status:  ', open_status

     write(lsatout,774)  chan, times(1), start, stepsz, nstep
     do prd=1,3
        write(lsatout, fmt_str) ((omg_nbc(1,prd,i,chan,k,1),i=1,nstep),k=1,nregion)
        write(lsatout, fmt_str) ((omg_nbc(1,prd,i,chan,k,2),i=1,nstep),k=1,nregion)
     end do

     do prd=1,3
        write(lsatout, fmt_str) ((omg_nbc(2,prd,i,chan,k,1),i=1,nstep),k=1,nregion)
        write(lsatout, fmt_str) ((omg_nbc(2,prd,i,chan,k,2),i=1,nstep),k=1,nregion)
     end do

     close( lsatout )

     !----------------------------------------------------------------------------
     ! write tot_cor data files (rows 1-3 are ges data, 4-6 analysis)
     !    row 0     = chan, time, start, stepsz, nstep
     !    rows 1-6  = ges data
     !    rows 7-13 = analysis data
     open(lsatout,file=totcor_out_file,iostat=open_status, &
                             action='write',status='new',form='formatted')
     write(6,*)' totcor_out_file opened, status:  ', open_status

     write(lsatout,774)  chan, times(1), start, stepsz, nstep
     do prd=1,3
        write(lsatout, fmt_str) ((tot_cor(1,prd,i,chan,k,1),i=1,nstep),k=1,nregion)
        write(lsatout, fmt_str) ((tot_cor(1,prd,i,chan,k,2),i=1,nstep),k=1,nregion)
     end do

     do prd=1,3
        write(lsatout, fmt_str) ((tot_cor(2,prd,i,chan,k,1),i=1,nstep),k=1,nregion)
        write(lsatout, fmt_str) ((tot_cor(2,prd,i,chan,k,2),i=1,nstep),k=1,nregion)
     end do

     close( lsatout )

     !----------------------------------------------------------------------------
     ! write omg_bc data files 
     !    row 0     = chan, time, start, stepsz, nstep
     !    rows 1-6  = ges data
     !    rows 7-13 = analysis data
     open(lsatout,file=omgbc_out_file,iostat=open_status, &
                             action='write',status='new',form='formatted')
     write(6,*)' omgbc_out_file opened, status:  ', open_status

     write(lsatout,774)  chan, times(1), start, stepsz, nstep
     do prd=1,3
        write(lsatout, fmt_str) ((omg_bc(1,prd,i,chan,k,1),i=1,nstep),k=1,nregion)
        write(lsatout, fmt_str) ((omg_bc(1,prd,i,chan,k,2),i=1,nstep),k=1,nregion)
     end do

     do prd=1,3
        write(lsatout, fmt_str) ((omg_bc(2,prd,i,chan,k,1),i=1,nstep),k=1,nregion)
        write(lsatout, fmt_str) ((omg_bc(2,prd,i,chan,k,2),i=1,nstep),k=1,nregion)
     end do

     close( lsatout )

     !----------------------------------------------------------------------------
     ! write fixang data files 
     !    row 0     = chan, time, start, stepsz, nstep
     !    rows 1-6  = ges data
     !    rows 7-13 = analysis data
     open(lsatout,file=fixang_out_file,iostat=open_status, &
                             action='write',status='new',form='formatted')
     write(6,*)' fixang_out_file opened, status:  ', open_status

     write(lsatout,774)  chan, times(1), start, stepsz, nstep
     do prd=1,3
        write(lsatout, fmt_str) ((fixang(1,prd,i,chan,k,1),i=1,nstep),k=1,nregion)
        write(lsatout, fmt_str) ((fixang(1,prd,i,chan,k,2),i=1,nstep),k=1,nregion)
     end do

     do prd=1,3
        write(lsatout, fmt_str) ((fixang(2,prd,i,chan,k,1),i=1,nstep),k=1,nregion)
        write(lsatout, fmt_str) ((fixang(2,prd,i,chan,k,2),i=1,nstep),k=1,nregion)
     end do

     close( lsatout )

     !----------------------------------------------------------------------------
     ! write lapse data files 
     !    row 0     = chan, time, start, stepsz, nstep
     !    rows 1-6  = ges data
     !    rows 7-13 = analysis data
     open(lsatout,file=lapse_out_file,iostat=open_status, &
                             action='write',status='new',form='formatted')
     write(6,*)' lapse_out_file opened, status:  ', open_status

     write(lsatout,774)  chan, times(1), start, stepsz, nstep
     do prd=1,3
        write(lsatout, fmt_str) ((lapse(1,prd,i,chan,k,1),i=1,nstep),k=1,nregion)
        write(lsatout, fmt_str) ((lapse(1,prd,i,chan,k,2),i=1,nstep),k=1,nregion)
     end do

     do prd=1,3
        write(lsatout, fmt_str) ((lapse(2,prd,i,chan,k,1),i=1,nstep),k=1,nregion)
        write(lsatout, fmt_str) ((lapse(2,prd,i,chan,k,2),i=1,nstep),k=1,nregion)
     end do

     close( lsatout )

     !----------------------------------------------------------------------------
     ! write lapse2 data files 
     !    row 0     = chan, time, start, stepsz, nstep
     !    rows 1-6  = ges data
     !    rows 7-13 = analysis data
     open(lsatout,file=lapse2_out_file,iostat=open_status, &
                             action='write',status='new',form='formatted')
     write(6,*)' lapse2_out_file opened, status:  ', open_status

     write(lsatout,774)  chan, times(1), start, stepsz, nstep
     do prd=1,3
        write(lsatout, fmt_str) ((lapse2(1,prd,i,chan,k,1),i=1,nstep),k=1,nregion)
        write(lsatout, fmt_str) ((lapse2(1,prd,i,chan,k,2),i=1,nstep),k=1,nregion)
     end do

     do prd=1,3
        write(lsatout, fmt_str) ((lapse2(2,prd,i,chan,k,1),i=1,nstep),k=1,nregion)
        write(lsatout, fmt_str) ((lapse2(2,prd,i,chan,k,2),i=1,nstep),k=1,nregion)
     end do

     close( lsatout )

     !----------------------------------------------------------------------------
     ! write mean data files 
     !    row 0     = chan, time, start, stepsz, nstep
     !    rows 1-6  = ges data
     !    rows 7-13 = analysis data
     open(lsatout,file=mean_out_file,iostat=open_status, &
                             action='write',status='new',form='formatted')
     write(6,*)' mean_out_file opened, status:  ', open_status

     write(lsatout,774)  chan, times(1), start, stepsz, nstep
     do prd=1,3
        write(lsatout, fmt_str) ((mean(1,prd,i,chan,k,1),i=1,nstep),k=1,nregion)
        write(lsatout, fmt_str) ((mean(1,prd,i,chan,k,2),i=1,nstep),k=1,nregion)
     end do

     do prd=1,3
        write(lsatout, fmt_str) ((mean(2,prd,i,chan,k,1),i=1,nstep),k=1,nregion)
        write(lsatout, fmt_str) ((mean(2,prd,i,chan,k,2),i=1,nstep),k=1,nregion)
     end do

     close( lsatout )

     !----------------------------------------------------------------------------
     ! write scangl data files 
     !    row 0     = chan, time, start, stepsz, nstep
     !    rows 1-6  = ges data
     !    rows 7-13 = analysis data
     open(lsatout,file=scangl_out_file,iostat=open_status, &
                             action='write',status='new',form='formatted')
     write(6,*)' scangl_out_file opened, status:  ', open_status

     write(lsatout,774)  chan, times(1), start, stepsz, nstep
     do prd=1,3
        write(lsatout, fmt_str) ((scangl(1,prd,i,chan,k,1),i=1,nstep),k=1,nregion)
        write(lsatout, fmt_str) ((scangl(1,prd,i,chan,k,2),i=1,nstep),k=1,nregion)
     end do

     do prd=1,3
        write(lsatout, fmt_str) ((scangl(2,prd,i,chan,k,1),i=1,nstep),k=1,nregion)
        write(lsatout, fmt_str) ((scangl(2,prd,i,chan,k,2),i=1,nstep),k=1,nregion)
     end do

     !----------------------------------------------------------------------------
     ! write clw data files 
     !    row 0     = chan, time, start, stepsz, nstep
     !    rows 1-6  = ges data
     !    rows 7-13 = analysis data
     open(lsatout,file=clw_out_file,iostat=open_status, &
                             action='write',status='new',form='formatted')
     write(6,*)' clw_out_file opened, status:  ', open_status

     write(lsatout,774)  chan, times(1), start, stepsz, nstep
     do prd=1,3
        write(lsatout, fmt_str) ((clw(1,prd,i,chan,k,1),i=1,nstep),k=1,nregion)
        write(lsatout, fmt_str) ((clw(1,prd,i,chan,k,2),i=1,nstep),k=1,nregion)
     end do

     do prd=1,3
        write(lsatout, fmt_str) ((clw(2,prd,i,chan,k,1),i=1,nstep),k=1,nregion)
        write(lsatout, fmt_str) ((clw(2,prd,i,chan,k,2),i=1,nstep),k=1,nregion)
     end do

     close( lsatout )

     !----------------------------------------------------------------------------
     ! write cos data files 
     !    row 0     = chan, time, start, stepsz, nstep
     !    rows 1-6  = ges data
     !    rows 7-13 = analysis data
     open(lsatout,file=cos_out_file,iostat=open_status, &
                             action='write',status='new',form='formatted')
     write(6,*)' cos_out_file opened, status:  ', open_status

     write(lsatout,774)  chan, times(1), start, stepsz, nstep
     do prd=1,3
        write(lsatout, fmt_str) ((cos(1,prd,i,chan,k,1),i=1,nstep),k=1,nregion)
        write(lsatout, fmt_str) ((cos(1,prd,i,chan,k,2),i=1,nstep),k=1,nregion)
     end do

     do prd=1,3
        write(lsatout, fmt_str) ((cos(2,prd,i,chan,k,1),i=1,nstep),k=1,nregion)
        write(lsatout, fmt_str) ((cos(2,prd,i,chan,k,2),i=1,nstep),k=1,nregion)
     end do

     close( lsatout )

     !----------------------------------------------------------------------------
     ! write sin data files 
     !    row 0     = chan, time, start, stepsz, nstep
     !    rows 1-6  = ges data
     !    rows 7-13 = analysis data
     open(lsatout,file=sin_out_file,iostat=open_status, &
                             action='write',status='new',form='formatted')
     write(6,*)' sin_out_file opened, status:  ', open_status

     write(lsatout,774)  chan, times(1), start, stepsz, nstep
     do prd=1,3
        write(lsatout, fmt_str) ((sin(1,prd,i,chan,k,1),i=1,nstep),k=1,nregion)
        write(lsatout, fmt_str) ((sin(1,prd,i,chan,k,2),i=1,nstep),k=1,nregion)
     end do

     do prd=1,3
        write(lsatout, fmt_str) ((sin(2,prd,i,chan,k,1),i=1,nstep),k=1,nregion)
        write(lsatout, fmt_str) ((sin(2,prd,i,chan,k,2),i=1,nstep),k=1,nregion)
     end do

     close( lsatout )

     !----------------------------------------------------------------------------
     ! write emiss data files 
     !    row 0     = chan, time, start, stepsz, nstep
     !    rows 1-6  = ges data
     !    rows 7-13 = analysis data
     open(lsatout,file=emiss_out_file,iostat=open_status, &
                             action='write',status='new',form='formatted')
     write(6,*)' emiss_out_file opened, status:  ', open_status

     write(lsatout,774)  chan, times(1), start, stepsz, nstep
     do prd=1,3
        write(lsatout, fmt_str) ((emiss(1,prd,i,chan,k,1),i=1,nstep),k=1,nregion)
        write(lsatout, fmt_str) ((emiss(1,prd,i,chan,k,2),i=1,nstep),k=1,nregion)
     end do

     do prd=1,3
        write(lsatout, fmt_str) ((emiss(2,prd,i,chan,k,1),i=1,nstep),k=1,nregion)
        write(lsatout, fmt_str) ((emiss(2,prd,i,chan,k,2),i=1,nstep),k=1,nregion)
     end do

     close( lsatout )

     !----------------------------------------------------------------------------
     ! write ordang4 data files 
     !    row 0     = chan, time, start, stepsz, nstep
     !    rows 1-6  = ges data
     !    rows 7-13 = analysis data
     open(lsatout,file=ordang4_out_file,iostat=open_status, &
                             action='write',status='new',form='formatted')
     write(6,*)' ordang4_out_file opened, status:  ', open_status

     write(lsatout,774)  chan, times(1), start, stepsz, nstep
     do prd=1,3
        write(lsatout, fmt_str) ((ordang4(1,prd,i,chan,k,1),i=1,nstep),k=1,nregion)
        write(lsatout, fmt_str) ((ordang4(1,prd,i,chan,k,2),i=1,nstep),k=1,nregion)
     end do

     do prd=1,3
        write(lsatout, fmt_str) ((ordang4(2,prd,i,chan,k,1),i=1,nstep),k=1,nregion)
        write(lsatout, fmt_str) ((ordang4(2,prd,i,chan,k,2),i=1,nstep),k=1,nregion)
     end do

     close( lsatout )

     !----------------------------------------------------------------------------
     ! write ordang3 data files 
     !    row 0     = chan, time, start, stepsz, nstep
     !    rows 1-6  = ges data
     !    rows 7-13 = analysis data
     open(lsatout,file=ordang3_out_file,iostat=open_status, &
                             action='write',status='new',form='formatted')
     write(6,*)' ordang3_out_file opened, status:  ', open_status

     write(lsatout,774)  chan, times(1), start, stepsz, nstep
     do prd=1,3
        write(lsatout, fmt_str) ((ordang3(1,prd,i,chan,k,1),i=1,nstep),k=1,nregion)
        write(lsatout, fmt_str) ((ordang3(1,prd,i,chan,k,2),i=1,nstep),k=1,nregion)
     end do

     do prd=1,3
        write(lsatout, fmt_str) ((ordang3(2,prd,i,chan,k,1),i=1,nstep),k=1,nregion)
        write(lsatout, fmt_str) ((ordang3(2,prd,i,chan,k,2),i=1,nstep),k=1,nregion)
     end do

     close( lsatout )

     !----------------------------------------------------------------------------
     ! write ordang2 data files 
     !    row 0     = chan, time, start, stepsz, nstep
     !    rows 1-6  = ges data
     !    rows 7-13 = analysis data
     open(lsatout,file=ordang2_out_file,iostat=open_status, &
                             action='write',status='new',form='formatted')
     write(6,*)' ordang2_out_file opened, status:  ', open_status

     write(lsatout,774)  chan, times(1), start, stepsz, nstep
     do prd=1,3
        write(lsatout, fmt_str) ((ordang2(1,prd,i,chan,k,1),i=1,nstep),k=1,nregion)
        write(lsatout, fmt_str) ((ordang2(1,prd,i,chan,k,2),i=1,nstep),k=1,nregion)
     end do

     do prd=1,3
        write(lsatout, fmt_str) ((ordang2(2,prd,i,chan,k,1),i=1,nstep),k=1,nregion)
        write(lsatout, fmt_str) ((ordang2(2,prd,i,chan,k,2),i=1,nstep),k=1,nregion)
     end do

     close( lsatout )

     !----------------------------------------------------------------------------
     ! write ordang1 data files 
     !    row 0     = chan, time, start, stepsz, nstep
     !    rows 1-6  = ges data
     !    rows 7-13 = analysis data
     open(lsatout,file=ordang1_out_file,iostat=open_status, &
                             action='write',status='new',form='formatted')
     write(6,*)' ordang1_out_file opened, status:  ', open_status

     write(lsatout,774)  chan, times(1), start, stepsz, nstep
     do prd=1,3
        write(lsatout, fmt_str) ((ordang1(1,prd,i,chan,k,1),i=1,nstep),k=1,nregion)
        write(lsatout, fmt_str) ((ordang1(1,prd,i,chan,k,2),i=1,nstep),k=1,nregion)
     end do

     do prd=1,3
        write(lsatout, fmt_str) ((ordang1(2,prd,i,chan,k,1),i=1,nstep),k=1,nregion)
        write(lsatout, fmt_str) ((ordang1(2,prd,i,chan,k,2),i=1,nstep),k=1,nregion)
     end do

     close( lsatout )

  end do


  deallocate( count, penalty, omg_nbc, tot_cor, omg_bc, fixang, lapse, lapse2 )
  deallocate( mean, scangl, clw, cos, sin, emiss, ordang4, ordang3, ordang2, ordang1 )


!---------------------------------------------------
! Re-sort data, converging on cycle instead of scan
! position and export to time series files.
!  t_* indicates time series arrays
!---------------------------------------------------

  allocate( t_cnt    (2,ncycle,nchanl,nregion) )
  allocate( t_fixang ( 2,ncycle,nchanl,nregion,2 ) )
  allocate( t_lapse  ( 2,ncycle,nchanl,nregion,2 ) )
  allocate( t_lapse2 ( 2,ncycle,nchanl,nregion,2 ) )
  allocate( t_mean   ( 2,ncycle,nchanl,nregion,2 ) )
  allocate( t_scangl ( 2,ncycle,nchanl,nregion,2 ) )
  allocate( t_clw    ( 2,ncycle,nchanl,nregion,2 ) )
  allocate( t_cos    ( 2,ncycle,nchanl,nregion,2 ) )
  allocate( t_sin    ( 2,ncycle,nchanl,nregion,2 ) )
  allocate( t_emiss  ( 2,ncycle,nchanl,nregion,2 ) )
  allocate( t_ordang4( 2,ncycle,nchanl,nregion,2 ) )
  allocate( t_ordang3( 2,ncycle,nchanl,nregion,2 ) )
  allocate( t_ordang2( 2,ncycle,nchanl,nregion,2 ) )
  allocate( t_ordang1( 2,ncycle,nchanl,nregion,2 ) )
 
! -------------------
!  initialize to 0 
!
  do ftyp=1,2
     do cyc=1,ncycle
        do chan=1,nchanl
           do rgn=1,nregion
              t_cnt( ftyp, cyc, chan, rgn ) = 0.0
              do ii=1,2
                 t_fixang ( ftyp,cyc,chan,rgn,ii ) = 0.00
                 t_lapse  ( ftyp,cyc,chan,rgn,ii ) = 0.00
                 t_lapse2 ( ftyp,cyc,chan,rgn,ii ) = 0.00
                 t_mean   ( ftyp,cyc,chan,rgn,ii ) = 0.00
                 t_scangl ( ftyp,cyc,chan,rgn,ii ) = 0.00
                 t_clw    ( ftyp,cyc,chan,rgn,ii ) = 0.00
                 t_cos    ( ftyp,cyc,chan,rgn,ii ) = 0.00
                 t_sin    ( ftyp,cyc,chan,rgn,ii ) = 0.00
                 t_emiss  ( ftyp,cyc,chan,rgn,ii ) = 0.00
                 t_ordang4( ftyp,cyc,chan,rgn,ii ) = 0.00
                 t_ordang3( ftyp,cyc,chan,rgn,ii ) = 0.00
                 t_ordang2( ftyp,cyc,chan,rgn,ii ) = 0.00
                 t_ordang1( ftyp,cyc,chan,rgn,ii ) = 0.00
              end do
           end do
        end do
     end do
  end do


! ----------------------------------------------------------------
!  Total each term by cycle, collapsing the nstep (scan position)
!
  do ftyp=1,2
     do cyc=1,ncycle
        do chan=1,nchanl
           do rgn=1,nregion
              do i=1,nstep
                 t_cnt(ftyp,cyc,chan,rgn) = t_cnt(ftyp,cyc,chan,rgn) +   &
                                         fcnt(ftyp,cyc,i,chan,rgn)
                 do ii=1,2
                    t_fixang (ftyp,cyc,chan,rgn,ii) = t_fixang(ftyp,cyc,chan,rgn,ii) + &
                                         ffixang(ftyp,cyc,i,chan,rgn,ii)
                    t_lapse  (ftyp,cyc,chan,rgn,ii) = t_lapse (ftyp,cyc,chan,rgn,ii) + &
                                           flapse(ftyp,cyc,i,chan,rgn,ii)
                    t_lapse2 (ftyp,cyc,chan,rgn,ii) = t_lapse2(ftyp,cyc,chan,rgn,ii) + &
                                           flapse2(ftyp,cyc,i,chan,rgn,ii)
                    t_mean   (ftyp,cyc,chan,rgn,ii) = t_mean(ftyp,cyc,chan,rgn,ii) + &
                                           fmean(ftyp,cyc,i,chan,rgn,ii)
                    t_scangl (ftyp,cyc,chan,rgn,ii) = t_scangl(ftyp,cyc,chan,rgn,ii) + &
                                           fscangl(ftyp,cyc,i,chan,rgn,ii)
                    t_clw    (ftyp,cyc,chan,rgn,ii) = t_clw(ftyp,cyc,chan,rgn,ii) + &
                                           fclw(ftyp,cyc,i,chan,rgn,ii)
                    t_cos    (ftyp,cyc,chan,rgn,ii) = t_cos(ftyp,cyc,chan,rgn,ii) + &
                                           fcos(ftyp,cyc,i,chan,rgn,ii)
                    t_sin    (ftyp,cyc,chan,rgn,ii) = t_sin(ftyp,cyc,chan,rgn,ii) + &
                                           fsin(ftyp,cyc,i,chan,rgn,ii)
                    t_emiss  (ftyp,cyc,chan,rgn,ii) = t_emiss(ftyp,cyc,chan,rgn,ii) + &
                                           femiss(ftyp,cyc,i,chan,rgn,ii)
                    t_ordang4(ftyp,cyc,chan,rgn,ii) = t_ordang4(ftyp,cyc,chan,rgn,ii) + &
                                           fordang4(ftyp,cyc,i,chan,rgn,ii)
                    t_ordang3(ftyp,cyc,chan,rgn,ii) = t_ordang3(ftyp,cyc,chan,rgn,ii) + &
                                           fordang3(ftyp,cyc,i,chan,rgn,ii)
                    t_ordang2(ftyp,cyc,chan,rgn,ii) = t_ordang2(ftyp,cyc,chan,rgn,ii) + &
                                           fordang2(ftyp,cyc,i,chan,rgn,ii)
                    t_ordang1(ftyp,cyc,chan,rgn,ii) = t_ordang1(ftyp,cyc,chan,rgn,ii) + &
                                           fordang1(ftyp,cyc,i,chan,rgn,ii)
                 end do
              end do
           end do
        end do
     end do
  end do

! --------------------------------------
!  Calculate avg and sdv for each term
!
  do ftyp=1,2
      do cyc=1,ncycle
         do chan=1,nchanl
            do rgn=1,nregion

               call avgsdv( t_cnt( ftyp,cyc,chan,rgn ),        &
                               t_fixang ( ftyp,cyc,chan,rgn,1 ), &
                               t_fixang ( ftyp,cyc,chan,rgn,2 ), rmiss )
               call avgsdv( t_cnt( ftyp,cyc,chan,rgn ),        &
                               t_lapse  ( ftyp,cyc,chan,rgn,1 ), &
                               t_lapse  ( ftyp,cyc,chan,rgn,2 ), rmiss )
               call avgsdv( t_cnt( ftyp,cyc,chan,rgn ),        &
                               t_lapse2 ( ftyp,cyc,chan,rgn,1 ), &
                               t_lapse2 ( ftyp,cyc,chan,rgn,2 ), rmiss )
               call avgsdv( t_cnt( ftyp,cyc,chan,rgn ),        &
                               t_mean   ( ftyp,cyc,chan,rgn,1 ), &
                               t_mean   ( ftyp,cyc,chan,rgn,2 ), rmiss )
               call avgsdv( t_cnt( ftyp,cyc,chan,rgn ),        &
                               t_scangl ( ftyp,cyc,chan,rgn,1 ), &
                               t_scangl ( ftyp,cyc,chan,rgn,2 ), rmiss )
               call avgsdv( t_cnt( ftyp,cyc,chan,rgn ),        &
                               t_clw    ( ftyp,cyc,chan,rgn,1 ), &
                               t_clw    ( ftyp,cyc,chan,rgn,2 ), rmiss )
               call avgsdv( t_cnt( ftyp,cyc,chan,rgn ),        &
                               t_cos    ( ftyp,cyc,chan,rgn,1 ), &
                               t_cos    ( ftyp,cyc,chan,rgn,2 ), rmiss )
               call avgsdv( t_cnt( ftyp,cyc,chan,rgn ),        &
                               t_sin    ( ftyp,cyc,chan,rgn,1 ), &
                               t_sin    ( ftyp,cyc,chan,rgn,2 ), rmiss )
               call avgsdv( t_cnt( ftyp,cyc,chan,rgn ),        &
                               t_emiss  ( ftyp,cyc,chan,rgn,1 ), &
                               t_emiss  ( ftyp,cyc,chan,rgn,2 ), rmiss )
               call avgsdv( t_cnt( ftyp,cyc,chan,rgn ),        &
                               t_ordang4( ftyp,cyc,chan,rgn,1 ), &
                               t_ordang4( ftyp,cyc,chan,rgn,2 ), rmiss )
               call avgsdv( t_cnt( ftyp,cyc,chan,rgn ),        &
                               t_ordang3( ftyp,cyc,chan,rgn,1 ), &
                               t_ordang3( ftyp,cyc,chan,rgn,2 ), rmiss )
               call avgsdv( t_cnt( ftyp,cyc,chan,rgn ),        &
                               t_ordang2( ftyp,cyc,chan,rgn,1 ), &
                               t_ordang2( ftyp,cyc,chan,rgn,2 ), rmiss )
               call avgsdv( t_cnt( ftyp,cyc,chan,rgn ),        &
                               t_ordang1( ftyp,cyc,chan,rgn,1 ), &
                               t_ordang1( ftyp,cyc,chan,rgn,2 ), rmiss )
            end do
         end do
      end do
   end do


!**********************************
!  write results to time.txt files
!**********************************
  82 FORMAT(A10,',',A10,',',F9.3,',',F9.3,',',F9.3,',',F9.3,',',F9.3,',' &
                           ,F9.3,',',F9.3,',',F9.3,',',F9.3,',',F9.3,',' &
                           ,F9.3,',',F9.3,',',F9.3,',',F9.3,',',F9.3,',' &
                           ,F9.3,',',F9.3,',',F9.3,',',F9.3,',',F9.3,',')

  do chan=1,nchanl

     !-------------------------
     !  rename out_files
     !    NOTE:  in file names chan starts at 0 so javascript can use the 
     !           channel select index to access the correct data file.
     write(str_nchanl, '(i10)') chan-1
     fixang_out_file = trim(satname) // '.' // trim(adjustl(str_nchanl)) &
                                    // '.fixang.time.txt'
     lapse_out_file  = trim(satname) // '.' // trim(adjustl(str_nchanl)) &
                                    // '.lapse.time.txt'
     lapse2_out_file = trim(satname) // '.' // trim(adjustl(str_nchanl)) &
                                    // '.lapse2.time.txt'
     mean_out_file   = trim(satname) // '.' // trim(adjustl(str_nchanl)) &
                                    // '.mean.time.txt'
     scangl_out_file = trim(satname) // '.' // trim(adjustl(str_nchanl)) &
                                    // '.scangl.time.txt'
     clw_out_file    = trim(satname) // '.' // trim(adjustl(str_nchanl)) &
                                    // '.clw.time.txt'
     cos_out_file    = trim(satname) // '.' // trim(adjustl(str_nchanl)) &
                                    // '.cos.time.txt'
     sin_out_file    = trim(satname) // '.' // trim(adjustl(str_nchanl)) &
                                    // '.sin.time.txt'
     emiss_out_file  = trim(satname) // '.' // trim(adjustl(str_nchanl)) &
                                    // '.emiss.time.txt'
     ordang4_out_file= trim(satname) // '.' // trim(adjustl(str_nchanl)) &
                                    // '.ordang4.time.txt'
     ordang3_out_file= trim(satname) // '.' // trim(adjustl(str_nchanl)) &
                                    // '.ordang3.time.txt'
     ordang2_out_file= trim(satname) // '.' // trim(adjustl(str_nchanl)) &
                                    // '.ordang2.time.txt'
     ordang1_out_file= trim(satname) // '.' // trim(adjustl(str_nchanl)) &
                                    // '.ordang1.time.txt'

     write(str_nchanl, '(i10)') chan
     lsatout = 70 + chan
     open(lsatout,file=cnt_out_file,iostat=open_status, &
                             action='write',status='new',form='formatted')
     write(6,*)' cnt_out_file opened, status:  ', open_status

     !----------------------------------------------------------------------------
     !  fixang data is arranged:
     !     one row for each time step (cycle) consisting of:
     !        chan, time, 
     !        avg ges fixang rgn 1, avg ges fixang rgn 2, avg ges fixang rgn 3,
     !        avg ges fixang rgn 4, avg ges fixang rgn 5, avg anl fixang rgn 1,
     !        avg anl fixang rgn 2, avg anl fixang rgn 3, avg anl fixang rgn 4,
     !        avg anl fixang rgn 5, 
     !        sdv ges fixang rgn 1, sdv ges fixang rgn 2, sdv ges fixang rgn 3,
     !        sdv ges fixang rgn 4, sdv ges fixang rgn 5, sdv anl fixang rgn 1,
     !        sdv anl fixang rgn 2, sdv anl fixang rgn 3, sdv anl fixang rgn 4,
     !        sdv anl fixang rgn 5
     !
     open(lsatout,file=fixang_out_file,iostat=open_status, &
                             action='write',status='new',form='formatted')
     write(6,*)' fixang_out_file opened, status:  ', open_status

     do cyc=1,ncycle
         write(lsatout,82) trim(str_nchanl), trim(times(cyc)),     &
                 t_fixang(ges,cyc,chan,1,avg), t_fixang(ges,cyc,chan,2,avg), &
                 t_fixang(ges,cyc,chan,3,avg), t_fixang(ges,cyc,chan,4,avg), &
                 t_fixang(ges,cyc,chan,5,avg), t_fixang(anl,cyc,chan,1,avg), &
                 t_fixang(anl,cyc,chan,2,avg), t_fixang(anl,cyc,chan,3,avg), &
                 t_fixang(anl,cyc,chan,4,avg), t_fixang(anl,cyc,chan,5,avg), &
                 t_fixang(ges,cyc,chan,1,sdv), t_fixang(anl,cyc,chan,2,sdv), &
                 t_fixang(ges,cyc,chan,3,sdv), t_fixang(anl,cyc,chan,4,sdv), &
                 t_fixang(ges,cyc,chan,5,sdv), t_fixang(ges,cyc,chan,1,sdv), &
                 t_fixang(ges,cyc,chan,2,sdv), t_fixang(ges,cyc,chan,3,sdv), &
                 t_fixang(ges,cyc,chan,4,sdv), t_fixang(ges,cyc,chan,5,sdv)
     end do
     close(lsatout)

     !----------------------------------------------------------------------------
     !  lapse data is arranged:
     !     one row for each time step (cycle) consisting of:
     !        chan, time, 
     !        avg ges lapse rgn 1-5, avg anl lapse rgn 1-5
     !        sdv ges lapse rgn 1-5, sdv anl lapse rgn 1-5
     !
     open(lsatout,file=lapse_out_file,iostat=open_status, &
                             action='write',status='new',form='formatted')
     write(6,*)' lapse_out_file opened, status:  ', open_status

     do cyc=1,ncycle
         write(lsatout,82) trim(str_nchanl), trim(times(cyc)),     &
                 t_lapse(ges,cyc,chan,1,avg), t_lapse(ges,cyc,chan,2,avg), &
                 t_lapse(ges,cyc,chan,3,avg), t_lapse(ges,cyc,chan,4,avg), &
                 t_lapse(ges,cyc,chan,5,avg), t_lapse(anl,cyc,chan,1,avg), &
                 t_lapse(anl,cyc,chan,2,avg), t_lapse(anl,cyc,chan,3,avg), &
                 t_lapse(anl,cyc,chan,4,avg), t_lapse(anl,cyc,chan,5,avg), &
                 t_lapse(ges,cyc,chan,1,sdv), t_lapse(anl,cyc,chan,2,sdv), &
                 t_lapse(ges,cyc,chan,3,sdv), t_lapse(anl,cyc,chan,4,sdv), &
                 t_lapse(ges,cyc,chan,5,sdv), t_lapse(ges,cyc,chan,1,sdv), &
                 t_lapse(ges,cyc,chan,2,sdv), t_lapse(ges,cyc,chan,3,sdv), &
                 t_lapse(ges,cyc,chan,4,sdv), t_lapse(ges,cyc,chan,5,sdv)
     end do
     close(lsatout)


     !----------------------------------------------------------------------------
     !  lapse2 data is arranged:
     !     one row for each time step (cycle) consisting of:
     !        chan, time, 
     !        avg ges lapse2 rgn 1-5, avg anl lapse2 rgn 1-5
     !        sdv ges lapse2 rgn 1-5, sdv anl lapse2 rgn 1-5
     !
     open(lsatout,file=lapse2_out_file,iostat=open_status, &
                             action='write',status='new',form='formatted')
     write(6,*)' lapse2_out_file opened, status:  ', open_status

     do cyc=1,ncycle
         write(lsatout,82) trim(str_nchanl), trim(times(cyc)),     &
                 t_lapse2(ges,cyc,chan,1,avg), t_lapse2(ges,cyc,chan,2,avg), &
                 t_lapse2(ges,cyc,chan,3,avg), t_lapse2(ges,cyc,chan,4,avg), &
                 t_lapse2(ges,cyc,chan,5,avg), t_lapse2(anl,cyc,chan,1,avg), &
                 t_lapse2(anl,cyc,chan,2,avg), t_lapse2(anl,cyc,chan,3,avg), &
                 t_lapse2(anl,cyc,chan,4,avg), t_lapse2(anl,cyc,chan,5,avg), &
                 t_lapse2(ges,cyc,chan,1,sdv), t_lapse2(anl,cyc,chan,2,sdv), &
                 t_lapse2(ges,cyc,chan,3,sdv), t_lapse2(anl,cyc,chan,4,sdv), &
                 t_lapse2(ges,cyc,chan,5,sdv), t_lapse2(ges,cyc,chan,1,sdv), &
                 t_lapse2(ges,cyc,chan,2,sdv), t_lapse2(ges,cyc,chan,3,sdv), &
                 t_lapse2(ges,cyc,chan,4,sdv), t_lapse2(ges,cyc,chan,5,sdv)
     end do
     close(lsatout)

     !----------------------------------------------------------------------------
     !  mean data is arranged:
     !     one row for each time step (cycle) consisting of:
     !        chan, time, 
     !        avg ges mean rgn 1-5, avg anl mean rgn 1-5
     !        sdv ges mean rgn 1-5, sdv anl mean rgn 1-5
     !
     open(lsatout,file=mean_out_file,iostat=open_status, &
                             action='write',status='new',form='formatted')
     write(6,*)' mean_out_file opened, status:  ', open_status

     do cyc=1,ncycle
         write(lsatout,82) trim(str_nchanl), trim(times(cyc)),     &
                 t_mean(ges,cyc,chan,1,avg), t_mean(ges,cyc,chan,2,avg), &
                 t_mean(ges,cyc,chan,3,avg), t_mean(ges,cyc,chan,4,avg), &
                 t_mean(ges,cyc,chan,5,avg), t_mean(anl,cyc,chan,1,avg), &
                 t_mean(anl,cyc,chan,2,avg), t_mean(anl,cyc,chan,3,avg), &
                 t_mean(anl,cyc,chan,4,avg), t_mean(anl,cyc,chan,5,avg), &
                 t_mean(ges,cyc,chan,1,sdv), t_mean(anl,cyc,chan,2,sdv), &
                 t_mean(ges,cyc,chan,3,sdv), t_mean(anl,cyc,chan,4,sdv), &
                 t_mean(ges,cyc,chan,5,sdv), t_mean(ges,cyc,chan,1,sdv), &
                 t_mean(ges,cyc,chan,2,sdv), t_mean(ges,cyc,chan,3,sdv), &
                 t_mean(ges,cyc,chan,4,sdv), t_mean(ges,cyc,chan,5,sdv)
     end do
     close(lsatout)


     !----------------------------------------------------------------------------
     !  scangl data is arranged:
     !     one row for each time step (cycle) consisting of:
     !        chan, time, 
     !        avg ges scangl rgn 1-5, avg anl scangl rgn 1-5
     !        sdv ges scangl rgn 1-5, sdv anl scangl rgn 1-5
     !
     open(lsatout,file=scangl_out_file,iostat=open_status, &
                             action='write',status='new',form='formatted')
     write(6,*)' scangl_out_file opened, status:  ', open_status

     do cyc=1,ncycle
         write(lsatout,82) trim(str_nchanl), trim(times(cyc)),     &
                 t_scangl(ges,cyc,chan,1,avg), t_scangl(ges,cyc,chan,2,avg), &
                 t_scangl(ges,cyc,chan,3,avg), t_scangl(ges,cyc,chan,4,avg), &
                 t_scangl(ges,cyc,chan,5,avg), t_scangl(anl,cyc,chan,1,avg), &
                 t_scangl(anl,cyc,chan,2,avg), t_scangl(anl,cyc,chan,3,avg), &
                 t_scangl(anl,cyc,chan,4,avg), t_scangl(anl,cyc,chan,5,avg), &
                 t_scangl(ges,cyc,chan,1,sdv), t_scangl(anl,cyc,chan,2,sdv), &
                 t_scangl(ges,cyc,chan,3,sdv), t_scangl(anl,cyc,chan,4,sdv), &
                 t_scangl(ges,cyc,chan,5,sdv), t_scangl(ges,cyc,chan,1,sdv), &
                 t_scangl(ges,cyc,chan,2,sdv), t_scangl(ges,cyc,chan,3,sdv), &
                 t_scangl(ges,cyc,chan,4,sdv), t_scangl(ges,cyc,chan,5,sdv)
     end do
     close(lsatout)


     !----------------------------------------------------------------------------
     !  clw data is arranged:
     !     one row for each time step (cycle) consisting of:
     !        chan, time, 
     !        avg ges clw rgn 1-5, avg anl clw rgn 1-5
     !        sdv ges clw rgn 1-5, sdv anl clw rgn 1-5
     !
     open(lsatout,file=clw_out_file,iostat=open_status, &
                             action='write',status='new',form='formatted')
     write(6,*)' clw_out_file opened, status:  ', open_status

     do cyc=1,ncycle
         write(lsatout,82) trim(str_nchanl), trim(times(cyc)),     &
                 t_clw(ges,cyc,chan,1,avg), t_clw(ges,cyc,chan,2,avg), &
                 t_clw(ges,cyc,chan,3,avg), t_clw(ges,cyc,chan,4,avg), &
                 t_clw(ges,cyc,chan,5,avg), t_clw(anl,cyc,chan,1,avg), &
                 t_clw(anl,cyc,chan,2,avg), t_clw(anl,cyc,chan,3,avg), &
                 t_clw(anl,cyc,chan,4,avg), t_clw(anl,cyc,chan,5,avg), &
                 t_clw(ges,cyc,chan,1,sdv), t_clw(anl,cyc,chan,2,sdv), &
                 t_clw(ges,cyc,chan,3,sdv), t_clw(anl,cyc,chan,4,sdv), &
                 t_clw(ges,cyc,chan,5,sdv), t_clw(ges,cyc,chan,1,sdv), &
                 t_clw(ges,cyc,chan,2,sdv), t_clw(ges,cyc,chan,3,sdv), &
                 t_clw(ges,cyc,chan,4,sdv), t_clw(ges,cyc,chan,5,sdv)
     end do
     close(lsatout)


     !----------------------------------------------------------------------------
     !   cos data is arranged:
     !     one row for each time step (cycle) consisting of:
     !        chan, time, 
     !        avg ges  cos rgn 1-5, avg anl  cos rgn 1-5
     !        sdv ges  cos rgn 1-5, sdv anl  cos rgn 1-5
     !
     open(lsatout,file=cos_out_file,iostat=open_status, &
                             action='write',status='new',form='formatted')
     write(6,*)' cos_out_file opened, status:  ', open_status

     do cyc=1,ncycle
         write(lsatout,82) trim(str_nchanl), trim(times(cyc)),     &
                 t_cos(ges,cyc,chan,1,avg), t_cos(ges,cyc,chan,2,avg), &
                 t_cos(ges,cyc,chan,3,avg), t_cos(ges,cyc,chan,4,avg), &
                 t_cos(ges,cyc,chan,5,avg), t_cos(anl,cyc,chan,1,avg), &
                 t_cos(anl,cyc,chan,2,avg), t_cos(anl,cyc,chan,3,avg), &
                 t_cos(anl,cyc,chan,4,avg), t_cos(anl,cyc,chan,5,avg), &
                 t_cos(ges,cyc,chan,1,sdv), t_cos(anl,cyc,chan,2,sdv), &
                 t_cos(ges,cyc,chan,3,sdv), t_cos(anl,cyc,chan,4,sdv), &
                 t_cos(ges,cyc,chan,5,sdv), t_cos(ges,cyc,chan,1,sdv), &
                 t_cos(ges,cyc,chan,2,sdv), t_cos(ges,cyc,chan,3,sdv), &
                 t_cos(ges,cyc,chan,4,sdv), t_cos(ges,cyc,chan,5,sdv)
     end do
     close(lsatout)


     !----------------------------------------------------------------------------
     !   sin data is arranged:
     !     one row for each time step (cycle) consisting of:
     !        chan, time, 
     !        avg ges  sin rgn 1-5, avg anl  sin rgn 1-5
     !        sdv ges  sin rgn 1-5, sdv anl  sin rgn 1-5
     !
     open(lsatout,file=sin_out_file,iostat=open_status, &
                             action='write',status='new',form='formatted')
     write(6,*)' sin_out_file opened, status:  ', open_status

     do cyc=1,ncycle
         write(lsatout,82) trim(str_nchanl), trim(times(cyc)),     &
                 t_sin(ges,cyc,chan,1,avg), t_sin(ges,cyc,chan,2,avg), &
                 t_sin(ges,cyc,chan,3,avg), t_sin(ges,cyc,chan,4,avg), &
                 t_sin(ges,cyc,chan,5,avg), t_sin(anl,cyc,chan,1,avg), &
                 t_sin(anl,cyc,chan,2,avg), t_sin(anl,cyc,chan,3,avg), &
                 t_sin(anl,cyc,chan,4,avg), t_sin(anl,cyc,chan,5,avg), &
                 t_sin(ges,cyc,chan,1,sdv), t_sin(anl,cyc,chan,2,sdv), &
                 t_sin(ges,cyc,chan,3,sdv), t_sin(anl,cyc,chan,4,sdv), &
                 t_sin(ges,cyc,chan,5,sdv), t_sin(ges,cyc,chan,1,sdv), &
                 t_sin(ges,cyc,chan,2,sdv), t_sin(ges,cyc,chan,3,sdv), &
                 t_sin(ges,cyc,chan,4,sdv), t_sin(ges,cyc,chan,5,sdv)
     end do
     close(lsatout)


     !----------------------------------------------------------------------------
     !   emiss data is arranged:
     !     one row for each time step (cycle) consisting of:
     !        chan, time, 
     !        avg ges  emiss rgn 1-5, avg anl  emiss rgn 1-5
     !        sdv ges  emiss rgn 1-5, sdv anl  emiss rgn 1-5
     !
     open(lsatout,file=emiss_out_file,iostat=open_status, &
                             action='write',status='new',form='formatted')
     write(6,*)' emiss_out_file opened, status:  ', open_status

     do cyc=1,ncycle
         write(lsatout,82) trim(str_nchanl), trim(times(cyc)),     &
                 t_emiss(ges,cyc,chan,1,avg), t_emiss(ges,cyc,chan,2,avg), &
                 t_emiss(ges,cyc,chan,3,avg), t_emiss(ges,cyc,chan,4,avg), &
                 t_emiss(ges,cyc,chan,5,avg), t_emiss(anl,cyc,chan,1,avg), &
                 t_emiss(anl,cyc,chan,2,avg), t_emiss(anl,cyc,chan,3,avg), &
                 t_emiss(anl,cyc,chan,4,avg), t_emiss(anl,cyc,chan,5,avg), &
                 t_emiss(ges,cyc,chan,1,sdv), t_emiss(anl,cyc,chan,2,sdv), &
                 t_emiss(ges,cyc,chan,3,sdv), t_emiss(anl,cyc,chan,4,sdv), &
                 t_emiss(ges,cyc,chan,5,sdv), t_emiss(ges,cyc,chan,1,sdv), &
                 t_emiss(ges,cyc,chan,2,sdv), t_emiss(ges,cyc,chan,3,sdv), &
                 t_emiss(ges,cyc,chan,4,sdv), t_emiss(ges,cyc,chan,5,sdv)
     end do
     close(lsatout)


     !----------------------------------------------------------------------------
     !   ordang4 data is arranged:
     !     one row for each time step (cycle) consisting of:
     !        chan, time, 
     !        avg ges  ordang4 rgn 1-5, avg anl  ordang4 rgn 1-5
     !        sdv ges  ordang4 rgn 1-5, sdv anl  ordang4 rgn 1-5
     !
     open(lsatout,file=ordang4_out_file,iostat=open_status, &
                             action='write',status='new',form='formatted')
     write(6,*)' ordang4_out_file opened, status:  ', open_status

     do cyc=1,ncycle
         write(lsatout,82) trim(str_nchanl), trim(times(cyc)),     &
                 t_ordang4(ges,cyc,chan,1,avg), t_ordang4(ges,cyc,chan,2,avg), &
                 t_ordang4(ges,cyc,chan,3,avg), t_ordang4(ges,cyc,chan,4,avg), &
                 t_ordang4(ges,cyc,chan,5,avg), t_ordang4(anl,cyc,chan,1,avg), &
                 t_ordang4(anl,cyc,chan,2,avg), t_ordang4(anl,cyc,chan,3,avg), &
                 t_ordang4(anl,cyc,chan,4,avg), t_ordang4(anl,cyc,chan,5,avg), &
                 t_ordang4(ges,cyc,chan,1,sdv), t_ordang4(anl,cyc,chan,2,sdv), &
                 t_ordang4(ges,cyc,chan,3,sdv), t_ordang4(anl,cyc,chan,4,sdv), &
                 t_ordang4(ges,cyc,chan,5,sdv), t_ordang4(ges,cyc,chan,1,sdv), &
                 t_ordang4(ges,cyc,chan,2,sdv), t_ordang4(ges,cyc,chan,3,sdv), &
                 t_ordang4(ges,cyc,chan,4,sdv), t_ordang4(ges,cyc,chan,5,sdv)
     end do
     close(lsatout)


     !----------------------------------------------------------------------------
     !   ordang3 data is arranged:
     !     one row for each time step (cycle) consisting of:
     !        chan, time, 
     !        avg ges  ordang3 rgn 1-5, avg anl  ordang3 rgn 1-5
     !        sdv ges  ordang3 rgn 1-5, sdv anl  ordang3 rgn 1-5
     !
     open(lsatout,file=ordang3_out_file,iostat=open_status, &
                             action='write',status='new',form='formatted')
     write(6,*)' ordang3_out_file opened, status:  ', open_status

     do cyc=1,ncycle
         write(lsatout,82) trim(str_nchanl), trim(times(cyc)),     &
                 t_ordang3(ges,cyc,chan,1,avg), t_ordang3(ges,cyc,chan,2,avg), &
                 t_ordang3(ges,cyc,chan,3,avg), t_ordang3(ges,cyc,chan,4,avg), &
                 t_ordang3(ges,cyc,chan,5,avg), t_ordang3(anl,cyc,chan,1,avg), &
                 t_ordang3(anl,cyc,chan,2,avg), t_ordang3(anl,cyc,chan,3,avg), &
                 t_ordang3(anl,cyc,chan,4,avg), t_ordang3(anl,cyc,chan,5,avg), &
                 t_ordang3(ges,cyc,chan,1,sdv), t_ordang3(anl,cyc,chan,2,sdv), &
                 t_ordang3(ges,cyc,chan,3,sdv), t_ordang3(anl,cyc,chan,4,sdv), &
                 t_ordang3(ges,cyc,chan,5,sdv), t_ordang3(ges,cyc,chan,1,sdv), &
                 t_ordang3(ges,cyc,chan,2,sdv), t_ordang3(ges,cyc,chan,3,sdv), &
                 t_ordang3(ges,cyc,chan,4,sdv), t_ordang3(ges,cyc,chan,5,sdv)
     end do
     close(lsatout)


     !----------------------------------------------------------------------------
     !   ordang2 data is arranged:
     !     one row for each time step (cycle) consisting of:
     !        chan, time, 
     !        avg ges  ordang2 rgn 1-5, avg anl  ordang2 rgn 1-5
     !        sdv ges  ordang2 rgn 1-5, sdv anl  ordang2 rgn 1-5
     !
     open(lsatout,file=ordang2_out_file,iostat=open_status, &
                             action='write',status='new',form='formatted')
     write(6,*)' ordang2_out_file opened, status:  ', open_status

     do cyc=1,ncycle
         write(lsatout,82) trim(str_nchanl), trim(times(cyc)),     &
                 t_ordang2(ges,cyc,chan,1,avg), t_ordang2(ges,cyc,chan,2,avg), &
                 t_ordang2(ges,cyc,chan,3,avg), t_ordang2(ges,cyc,chan,4,avg), &
                 t_ordang2(ges,cyc,chan,5,avg), t_ordang2(anl,cyc,chan,1,avg), &
                 t_ordang2(anl,cyc,chan,2,avg), t_ordang2(anl,cyc,chan,3,avg), &
                 t_ordang2(anl,cyc,chan,4,avg), t_ordang2(anl,cyc,chan,5,avg), &
                 t_ordang2(ges,cyc,chan,1,sdv), t_ordang2(anl,cyc,chan,2,sdv), &
                 t_ordang2(ges,cyc,chan,3,sdv), t_ordang2(anl,cyc,chan,4,sdv), &
                 t_ordang2(ges,cyc,chan,5,sdv), t_ordang2(ges,cyc,chan,1,sdv), &
                 t_ordang2(ges,cyc,chan,2,sdv), t_ordang2(ges,cyc,chan,3,sdv), &
                 t_ordang2(ges,cyc,chan,4,sdv), t_ordang2(ges,cyc,chan,5,sdv)
     end do
     close(lsatout)


     !----------------------------------------------------------------------------
     !   ordang1 data is arranged:
     !     one row for each time step (cycle) consisting of:
     !        chan, time, 
     !        avg ges  ordang1 rgn 1-5, avg anl  ordang1 rgn 1-5
     !        sdv ges  ordang1 rgn 1-5, sdv anl  ordang1 rgn 1-5
     !
     open(lsatout,file=ordang1_out_file,iostat=open_status, &
                             action='write',status='new',form='formatted')
     write(6,*)' ordang1_out_file opened, status:  ', open_status

     do cyc=1,ncycle
         write(lsatout,82) trim(str_nchanl), trim(times(cyc)),     &
                 t_ordang1(ges,cyc,chan,1,avg), t_ordang1(ges,cyc,chan,2,avg), &
                 t_ordang1(ges,cyc,chan,3,avg), t_ordang1(ges,cyc,chan,4,avg), &
                 t_ordang1(ges,cyc,chan,5,avg), t_ordang1(anl,cyc,chan,1,avg), &
                 t_ordang1(anl,cyc,chan,2,avg), t_ordang1(anl,cyc,chan,3,avg), &
                 t_ordang1(anl,cyc,chan,4,avg), t_ordang1(anl,cyc,chan,5,avg), &
                 t_ordang1(ges,cyc,chan,1,sdv), t_ordang1(anl,cyc,chan,2,sdv), &
                 t_ordang1(ges,cyc,chan,3,sdv), t_ordang1(anl,cyc,chan,4,sdv), &
                 t_ordang1(ges,cyc,chan,5,sdv), t_ordang1(ges,cyc,chan,1,sdv), &
                 t_ordang1(ges,cyc,chan,2,sdv), t_ordang1(ges,cyc,chan,3,sdv), &
                 t_ordang1(ges,cyc,chan,4,sdv), t_ordang1(ges,cyc,chan,5,sdv)
     end do
     close(lsatout)

  end do  !  loop over channels

!  end of re-sort 

  deallocate( times )
  deallocate( fmt_str )

  deallocate( fcnt, fpen, fomg_nbc, ftot_cor, fomg_bc, ffixang, flapse, flapse2, fmean )
  deallocate( fscangl, fclw, fcos, fsin, femiss, fordang4, fordang3, fordang2, fordang1 )

  deallocate( t_cnt, t_fixang, t_lapse, t_lapse2, t_scangl, t_clw, t_cos, t_sin )
  deallocate( t_emiss, t_ordang4, t_ordang3, t_ordang2, t_ordang1 )

! End of program
  stop
end program angle
