program angle
  use read_diag

  implicit none
  integer ntype,mstep
  parameter (ntype=21,mstep=100)

  character(10),dimension(ntype):: ftype
  character(8) stid
  character(20) satname,stringd,dfile
  character(10) satype,dplat
  character(20) dum,satsis,satscan_sis
  character(40) string,diag_rad,data_file,ctl_file
  character(10) suffix

  integer luname,lungrd,lunctl,lndiag,lunang
  integer iyy,imm,idd,ihh,idhh,incr,iread,iflag,ipos
  integer iyy2,imm2,idd2,ihh2,ntime
  integer n_chan,j,i,k,ii,nsub,jiter,jj
  integer,allocatable,dimension(:):: io_chan,nu_chan
  integer npred_radiag,angord

  real start,step
  integer nstep,iscan
  character(1) cflg
  real rang,pen
  real weight,rlat,rlon,rmiss,obs,biascor,obsges,obsgesnbc,rterm,rread
  real,dimension(2):: cor_tot,nbc_omg,bc_omg
  real,dimension(2):: cor_fixang,cor_lapse,cor_lapse2,cor_const,cor_scangl,cor_clw

  real,allocatable,dimension(:):: wavenumbr,error,use,frequency
  real,allocatable,dimension(:,:):: timang
  real,allocatable,dimension(:,:):: count,penalty
  real,allocatable,dimension(:,:,:):: tot_cor,omg_nbc,omg_bc
  real,allocatable,dimension(:,:,:):: fixang_cor,lapse_cor,lapse2_cor
  real,allocatable,dimension(:,:,:):: const_cor,scangl_cor,clw_cor 


! Variables for reading satellite data
  type(diag_header_fix_list )             :: header_fix
  type(diag_header_chan_list),allocatable :: header_chan(:)
  type(diag_data_name_list  )             :: data_name
  type(diag_data_fix_list   )             :: data_fix
  type(diag_data_chan_list  ),allocatable :: data_chan(:)
  type(diag_data_extra_list ),allocatable :: data_extra(:,:)

! Namelist with defaults
  logical               :: retrieval            = .false.
  integer               :: nchanl               = 19
  integer               :: imkctl               = 1
  integer               :: imkdata              = 1
  character(3)          :: gesanl               = 'ges'
  integer               :: little_endian        = 1
  namelist /input/ satname,iyy,imm,idd,ihh,idhh,incr,&
       nchanl,suffix,imkctl,imkdata,retrieval,gesanl,little_endian

  data luname,lungrd,lunctl,lndiag,iscan / 5, 51, 52, 21, 31 /
  data lunang / 22 /
  data rmiss /-999./
  data stringd / '.%y4%m2%d2%h2' /
  data ftype / 'satang', 'count', 'penalty', &
       'omgnbc', 'total', 'omgbc', &
       'fixang', 'lapse', 'lapse2', &
       'const', 'scangl', 'clw', &
       'omgnbc_2', 'total_2', 'omgbc_2', &
       'fixang_2', 'lapse_2', 'lapse2_2', & 
       'const_2', 'scangl_2', 'clw_2' /


!************************************************************************
!
! Initialize variables
  iread=0
  npred_radiag = 5

! Read namelist input
  read(luname,input)
  write(6,input)
  write(6,*)' '
  write(6,*)' suffix = ', suffix
  write(6,*)'gesanl = ', gesanl
  write(6,*)' '

  if ( trim(gesanl) == 'anl' ) then
     ftype(4)  = 'omanbc'
     ftype(6)  = 'omabc'
     ftype(13) = 'omanbc_2'
     ftype(15) = 'omabc_2'
  endif



! Create filenames for diagnostic input, binary output file
  write(stringd,100) iyy,imm,idd,ihh
100 format('.',i4.4,3i2.2)

  if ( trim(gesanl) == 'ges' ) then
     diag_rad = trim(satname)
     data_file= trim(satname) // trim(stringd) // '.ieee_d'
     ctl_file = trim(satname) // '.ctl'
  else
     diag_rad = trim(satname) // '_anl'
     data_file= trim(satname) // '_anl' // trim(stringd) // '.ieee_d'
     ctl_file = trim(satname) // '_anl.ctl'
  endif

  write(6,*)'diag_rad =',diag_rad
  write(6,*)'data_file=',data_file
  write(6,*)'ctl_file =',ctl_file

! Open unit to diagnostic file.  Read portion of header to 
! see if file exists
  open(lndiag,file=diag_rad,form='unformatted')
  read(lndiag,err=900,end=900) dum
  rewind lndiag

! File exists.  Read header
  write(6,*)'call read_diag_header'
  call read_radiag_header( lndiag, npred_radiag, retrieval, header_fix,&
     header_chan, data_name, iflag )

! Extract observation type, satellite id, and number of channels
  satype = header_fix%obstype
  satsis = header_fix%isis       
  dplat  = header_fix%id
  n_chan = header_fix%nchan
  jiter  = header_fix%jiter
  angord = header_fix%angord

  write(6,*)'satsis,satype,satid,n_chan=',satsis,' ',satype,' ',dplat,' ',n_chan

  string = trim(satype) //'_'// trim(dplat)
  write(6,*)'string,satname=',string,' ',satname
  if ( trim(string) /= trim(satname) ) then
     write(6,*)'***ERROR*** inconsistent instrument types'
     write(6,*)'  satname,string  =',satname,' ',string
     call errexit(92)
  endif

!  if ( jiter /=1) then
!   write(6,*)  '***ERROR***  not the guess vs. satellite radiance'
!   write(6,*) 'outloop no. ',jiter
!   call errexit(92)
!  endif
    
!  open scan info file compiled in the source directory

 
  open(iscan,file='scaninfo.txt',form='formatted')
  do 
   read(iscan,1000,IOSTAT=iflag) cflg,satscan_sis,start,step,nstep
   if( iflag /= 0 ) exit
   if(trim(satname) == trim(satscan_sis)) exit
  enddo
1000 format(a1,a20,2f10.2,i10)
  write(6,*) 'from scaninfo.txt,satscan_sis,start,step,nstep:  ', satscan_sis,start,step,nstep


! Allocate arrays to hold observational information
  write(6,*)' '
  write(6,*)'allocate arrays'
  allocate (io_chan(n_chan), nu_chan(n_chan), wavenumbr(n_chan), &
       error(n_chan), use(n_chan), frequency(n_chan))
  allocate (timang(mstep,n_chan))
  allocate (tot_cor(mstep,n_chan,2), &
       omg_nbc(mstep,n_chan,2), &
       omg_bc(mstep,n_chan,2), &
       count(mstep,n_chan), &
       penalty(mstep,n_chan),&
      fixang_cor(mstep,n_chan,2),lapse_cor(mstep,n_chan,2),&
      lapse2_cor(mstep,n_chan,2),clw_cor(mstep,n_chan,2),&
       const_cor(mstep,n_chan,2), scangl_cor(mstep,n_chan,2))

! Zero accumulator arrays
  do ii=1,2
        do j=1,n_chan
           do i=1,mstep
              if (ii==1) then
                 count(i,j) = 0.0
                 penalty(i,j) = 0.0
              endif
              tot_cor(i,j,ii) = 0.0
              omg_nbc(i,j,ii) = 0.0
              omg_bc(i,j,ii)  = 0.0
              fixang_cor(i,j,ii)  = 0.0
              lapse_cor(i,j,ii)  = 0.0
              lapse2_cor(i,j,ii)  = 0.0
              clw_cor(i,j,ii)  = 0.0
              const_cor(i,j,ii)  = 0.0
              scangl_cor(i,j,ii)  = 0.0
           end do
        end do
  end do


! Extract satinfo relative index
  do j=1,n_chan
     nu_chan(j)   = real( header_chan(j)%nuchan, 4 )
     io_chan(j)   = real( header_chan(j)%iochan, 4 )
     wavenumbr(j) = real( header_chan(j)%wave, 4 )
     error(j)     = real( header_chan(j)%varch, 4)
     use(j)       = real( header_chan(j)%iuse, 4 )
     frequency(j) = real( header_chan(j)%freq, 4)
!    print *,nu_chan(j),io_chan(j),wavenumbr(j),error(j),use(j),frequency(j)
  end do
        


! Loop to read entries in diagnostic file
  iflag = 0
  loopd:  do while (iflag == 0)

!    Read a record.  If read flag, iflag does not equal zero, exit loopd
     call read_radiag_data( lndiag, header_fix, retrieval, data_fix, data_chan, data_extra, iflag )
     if( iflag /= 0 ) exit loopd
     iread=iread+1

!    Extract observation location, scan position, and mpi weight.  
!    Convert (0-360) lon to (-180,180)
     rlat   = data_fix%lat
     rlon   = data_fix%lon
     ipos   = data_fix%senscn_pos       !! sensor scan position (integer)
     rang   = data_fix%satzen_ang	!! satellite zenith angle (deg)


     if (rlon>180.) rlon = rlon - 360.
     if (ipos<1) then
        write(6,*)'scan position less than 1.  ipos=',ipos
        ipos=1
     endif
     if (ipos>nstep) then
        write(6,*)'scan position > nstep.  ipos,nstep,',&
             ipos,nstep
        ipos=nstep
     endif
     rread  = rread + 1.0 

!    Detemine which subdomains the observation falls into

!    Channel loop
     do j = 1, n_chan

!       If observation was assimilated, accumulate sums for
!       appropriate scan angle 
        if (data_chan(j)%errinv > 1.e-6) then
           pen        =  data_chan(j)%errinv*(data_chan(j)%omgbc)**2
           cor_tot(1) =  (data_chan(j)%omgnbc - data_chan(j)%omgbc)
           nbc_omg(1) =  (data_chan(j)%omgnbc)
           bc_omg(1)  =  (data_chan(j)%omgbc)

           cor_tot(2) =  (data_chan(j)%omgnbc - data_chan(j)%omgbc)**2
           nbc_omg(2) =  (data_chan(j)%omgnbc)**2
           bc_omg(2)  =  (data_chan(j)%omgbc)**2

           cor_fixang(1) =  data_chan(j)%bifix(angord+1)
           cor_lapse(1)  =  data_chan(j)%bilap
           cor_lapse2(1) =  data_chan(j)%bilap2
           cor_const(1)  =  data_chan(j)%bicons
           cor_scangl(1) =  data_chan(j)%biang
           cor_clw(1)    =  data_chan(j)%biclw

           cor_fixang(2) =  (data_chan(j)%bifix(angord+1))**2
           cor_lapse(2)  =  (data_chan(j)%bilap)**2
           cor_lapse2(2) =  (data_chan(j)%bilap2)**2
           cor_const(2)  =  (data_chan(j)%bicons)**2
           cor_scangl(2) =  (data_chan(j)%biang)**2
           cor_clw(2)    =  (data_chan(j)%biclw)**2

           
           count(ipos,j)  = count(ipos,j) + 1.0 
           penalty(ipos,j) = penalty(ipos,j) + pen
           do ii=1,2
              tot_cor(ipos,j,ii) = tot_cor(ipos,j,ii) + cor_tot(ii)
              omg_nbc(ipos,j,ii) = omg_nbc(ipos,j,ii) + nbc_omg(ii)
              omg_bc(ipos,j,ii)  = omg_bc(ipos,j,ii)  + bc_omg(ii)
              fixang_cor(ipos,j,ii) = fixang_cor(ipos,j,ii) + cor_fixang(ii)
              lapse_cor(ipos,j,ii)  = lapse_cor(ipos,j,ii)  + cor_lapse(ii)
              lapse2_cor(ipos,j,ii) = lapse2_cor(ipos,j,ii) + cor_lapse2(ii)
              const_cor(ipos,j,ii)  = const_cor(ipos,j,ii)  + cor_const(ii)
              scangl_cor(ipos,j,ii) = scangl_cor(ipos,j,ii) + cor_scangl(ii)
              clw_cor(ipos,j,ii)    = clw_cor(ipos,j,ii)    + cor_clw(ii)
           end do
        endif

     enddo ! channel loop

! End of loop over diagnostic file
  enddo loopd
  close(lndiag)
  write(6,*)' '
  write(6,*)'read in ',iread,' obs ',rread
  write(6,*)' '


  open(lunang,file='satang.txt',form='formatted')
  call read_satang(lunang,satsis,nstep,mstep,n_chan,rmiss,timang)
  close(lunang)
  write(6,*)'read satang.txt'

! Create control file
  if ( imkctl == 1 ) then
     write(6,*)'call create_ctl_angle'

     if ( trim(gesanl) == 'ges' ) then
        dfile = trim(satname)
     else
        dfile = trim(satname) // '_anl'
     endif

     call create_ctl_angle(ntype,ftype,n_chan,iyy,imm,idd,ihh,&
          ctl_file,lunctl,rmiss,dfile,satype,dplat,1,nu_chan,&
          use,error,frequency,wavenumbr,nstep,start,step,little_endian)
  endif


! Write output to binary data file
  if ( imkdata == 1 ) then
     write(6,*)' '
     open(lungrd,file=data_file,form='unformatted')
     write(lungrd) ((timang(i,j),i=1,nstep),j=1,n_chan)
     write(lungrd) ((count(i,j),i=1,nstep),j=1,n_chan)
     write(lungrd) ((penalty(i,j),i=1,nstep),j=1,n_chan)
     do ii=1,2
        write(lungrd)((omg_nbc(i,j,ii),i=1,nstep),j=1,n_chan)
        write(lungrd)((tot_cor(i,j,ii),i=1,nstep),j=1,n_chan)
        write(lungrd)((omg_bc(i,j,ii),i=1,nstep),j=1,n_chan)
        write(lungrd)((fixang_cor(i,j,ii),i=1,nstep),j=1,n_chan)
        write(lungrd)((lapse_cor(i,j,ii),i=1,nstep),j=1,n_chan)
        write(lungrd)((lapse2_cor(i,j,ii),i=1,nstep),j=1,n_chan)
        write(lungrd)((const_cor(i,j,ii),i=1,nstep),j=1,n_chan)
        write(lungrd)((scangl_cor(i,j,ii),i=1,nstep),j=1,n_chan)
        write(lungrd)((clw_cor(i,j,ii),i=1,nstep),j=1,n_chan)
     end do
     write(6,*)'write output to lungrd=',lungrd,', file=',trim(data_file)
     close(lungrd)
  endif


! Deallocate arrays
  write(6,*)' '
  write(6,*)'deallocate arrays'
  deallocate(penalty,io_chan,nu_chan,wavenumbr,error,use,frequency)
  deallocate(timang,tot_cor,omg_nbc,omg_bc,count)
  goto 950

! Jump to here if eof or error reading diagnostic file.
900 continue
  write(6,*)'***PROBLEM reading diagnostic file.  diag_rad=',diag_rad
  close(lndiag)

  n_chan=nchanl
  if (n_chan<=0) then
     write(6,*)'***ERROR*** invalid nchanl=',nchanl,'  STOP program'
     call errexit(93)
  endif

  allocate(timang(mstep,n_chan))
  allocate(count(mstep,n_chan),&
       penalty(mstep,n_chan),&
       omg_nbc(mstep,n_chan,2),&
       tot_cor(mstep,n_chan,2),&
       omg_bc(mstep,n_chan,2))

  open(lunang,file='satang.txt',form='formatted')
  call read_satang(lunang,satsis,nstep,mstep,n_chan,rmiss,timang)
  close(lunang)
  write(6,*)'read satang.txt'

  write(6,*)'load missing value ',rmiss,' into output arrays.  ',&
       nstep,n_chan
  do ii=1,2
        do j=1,n_chan
           do i=1,mstep
              if (ii==1) then
                 count(i,j)  = rmiss
                 penalty(i,j)= rmiss
              endif
              omg_nbc(i,j,ii) = rmiss
              tot_cor(i,j,ii) = rmiss
              omg_bc(i,j,ii)  = rmiss
              fixang_cor(i,j,ii)  = rmiss 
              lapse_cor(i,j,ii)  =  rmiss
              lapse2_cor(i,j,ii)  =  rmiss
              clw_cor(i,j,ii)  =  rmiss
              const_cor(i,j,ii)  =  rmiss
              scangl_cor(i,j,ii)  = rmiss
           end do
        end do
  end do

  if ( imkdata == 1 ) then
     open(lungrd,file=data_file,form='unformatted')
     write(lungrd) ((timang(i,j),i=1,nstep),j=1,n_chan)
     write(lungrd) ((count(i,j),i=1,nstep),j=1,n_chan)
     write(lungrd) ((penalty(i,j),i=1,nstep),j=1,n_chan)
     do ii=1,2
        write(lungrd)((omg_nbc(i,j,ii),i=1,nstep),j=1,n_chan)
        write(lungrd)((tot_cor(i,j,ii),i=1,nstep),j=1,n_chan)
        write(lungrd)((omg_bc(i,j,ii),i=1,nstep),j=1,n_chan)
        write(lungrd)((fixang_cor(i,j,ii),i=1,nstep),j=1,n_chan)
        write(lungrd)((lapse_cor(i,j,ii),i=1,nstep),j=1,n_chan)
        write(lungrd)((lapse2_cor(i,j,ii),i=1,nstep),j=1,n_chan)
        write(lungrd)((const_cor(i,j,ii),i=1,nstep),j=1,n_chan)
        write(lungrd)((scangl_cor(i,j,ii),i=1,nstep),j=1,n_chan)
        write(lungrd)((clw_cor(i,j,ii),i=1,nstep),j=1,n_chan)
     end do
     write(6,*)'write output to lungrd=',lungrd,', file=',trim(data_file)
     close(lungrd)
  endif
  deallocate(timang,count,penalty,omg_nbc,omg_bc,tot_cor,&
             fixang_cor,lapse_cor,lapse2_cor,const_cor,scangl_cor,clw_cor)


! End of program
950 continue
  stop
end program angle
