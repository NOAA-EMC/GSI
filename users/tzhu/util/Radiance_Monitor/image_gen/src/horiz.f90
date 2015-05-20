program horiz
  use read_diag

  implicit none
  integer ntype
  parameter (ntype=4)

  logical first

  character(6),dimension(ntype):: ftype
  character(8) stid
  character(20) satname,stringd,satsis
  character(10) dum,satype,dplat
  character(40) string,diag_rad,grad_file,ctl_file,suffix

  integer luname,lungrd,lunctl,lndiag,isave
  integer iyy,imm,idd,ihh,idhh,incr,iread,irite,iflag
  integer n_chan,j,nlev,nflag,i
  integer,allocatable,dimension(:):: iuse,io_chan,nu_chan
  integer npred_radiag

  real weight,rlat,rlon,rtim,rmiss,obs,biascor,obsges,obsgesnbc
  real,allocatable,dimension(:):: frequency,wavenumbr,error
  real,allocatable,dimension(:,:):: var


! Variables for reading satellite data
  type(diag_header_fix_list )             :: header_fix
  type(diag_header_chan_list),allocatable :: header_chan(:)
  type(diag_data_name_list  )             :: data_name
  type(diag_data_fix_list   )             :: data_fix
  type(diag_data_chan_list  ),allocatable :: data_chan(:)
  type(diag_data_extra_list) ,allocatable :: data_extra(:,:)

! Namelist with defaults
  logical               :: retrieval            = .false.
  integer               :: nchanl               = 19
  integer               :: little_endian        = 1
  namelist /input/ satname,iyy,imm,idd,ihh,idhh,incr,nchanl,&
            suffix,retrieval,little_endian

  data luname,lungrd,lunctl,lndiag / 5, 51, 52, 21 /
  data rmiss /-999./
  data ftype / 'obs', 'cor', 'obsges', 'obsnbc' /
  data first / .true. /
  data stringd / '.%y4%m2%d2%h2' /
  


!************************************************************************
!
! Initialize variables
  iread=0; irite=0
  rtim=0.0; nlev=1; nflag=1
  npred_radiag = 12 

! Read namelist input
  read(luname,input)
  write(6,input)
  write(6,*)' '

!   Create filenames for diagnostic input, GrADS output, and
!   GrADS control files    

  write(stringd,100) iyy,imm,idd,ihh
100 format('.',i4.4,3i2.2)
  diag_rad = trim(satname)
  grad_file= trim(satname) // trim(stringd) // '.ieee_d'
  ctl_file = trim(satname) // '.ctl'

  write(6,*)'diag_rad =',diag_rad
  write(6,*)'grad_file=',grad_file
  write(6,*)'ctl_file =',ctl_file

! Open unit to diagnostic file.  Read portion of header to 
! see if file exists
  open(lndiag,file=diag_rad,form='unformatted')
  read(lndiag,err=900,end=900) dum
  rewind lndiag

! File exists.  Read header
  write(6,*)'call read_radiag_header'
  call read_radiag_header( lndiag, npred_radiag, retrieval, header_fix,&
        header_chan, data_name, iflag )
  if( iflag/=0 ) then
     write(6,*)'***ERROR*** problem reading diag file header, iflag=',iflag
     call errexit(91)
  endif

! Extract observation type, satellite id, and number of channels
  satype = header_fix%obstype
  satsis = header_fix%isis
  dplat  = header_fix%id
  n_chan = header_fix%nchan

  write(6,*)'satype,n_chan=',satype,' ',dplat,n_chan

!  string = trim(dplat)//'_'//trim(satype)
  string = trim(satype)//'_'//trim(dplat)
  write(6,*)'string,satname=',string,' ',satname
  if ( trim(string) /= trim(satname) ) then
     write(6,*)'***ERROR*** inconsistent instrument types'
     write(6,*)'  satname,string  =',satname,' ',string
     call errexit(91)
  endif


! Allocate arrays to hold observational information
  write(6,*)'allocate arrays'
  allocate (var(n_chan,ntype), iuse(n_chan), io_chan(n_chan), &
       nu_chan(n_chan), frequency(n_chan), wavenumbr(n_chan), &
       error(n_chan))

! Extract satinfo relative index
  do j=1,n_chan
     frequency(j) = real( header_chan(j)%freq, 4)
     wavenumbr(j) = real( header_chan(j)%wave, 4)
     error(j)     = real( header_chan(j)%varch, 4)
     nu_chan(j)   = real( header_chan(j)%nuchan, 4 )
     io_chan(j)   = real( header_chan(j)%iochan, 4 )
     iuse(j)      = real( header_chan(j)%iuse, 4 )
  end do


! Create GrADS control file
  write(6,*)'call create_ctl_horiz'
  write(6,*)'iyy, imm, idd, ihh, idhh = ', iyy, imm, idd, ihh, idhh
  call create_ctl_horiz(ntype,ftype,n_chan,iyy,imm,idd,ihh,idhh,incr,&
       ctl_file,lunctl,rmiss,satname,io_chan,nu_chan,frequency,&
       wavenumbr,error,iuse,satype,dplat,little_endian)

! Loop to read entries in diagnostic file
  iflag = 0
  loopd:  do while (iflag == 0)

!    Read a record.  If read flag, iflag does not equal zero, exit loopd
     call read_radiag_data( lndiag, header_fix, retrieval, data_fix, data_chan, data_extra, iflag )

     if( iflag /= 0 ) exit loopd
     iread=iread+1

!    Extract scan angle and mpi weight
     rlat   = data_fix%lat
     rlon   = data_fix%lon
     

!    Channel loop
     isave=0
     do j = 1, n_chan

!       If observation was assimilated, save it
        if (data_chan(j)%errinv > 1.e-6 ) then
           isave = 1
           obs      = data_chan(j)%tbobs
           biascor  = data_chan(j)%omgnbc - data_chan(j)%omgbc
           obsges   = data_chan(j)%omgbc 
           obsgesnbc= data_chan(j)%omgnbc
           
!         Set data values to missing flag
        else
           obs       = rmiss
           biascor   = rmiss
           obsges    = rmiss
           obsgesnbc = rmiss
        endif

!       Load into output array
        var(j,1) = obs
        var(j,2) = biascor
        var(j,3) = obsges
        var(j,4) = obsgesnbc

     enddo ! channel loop

!      Write GrADS record
     if (isave==1) then
        if (first) then
           first=.false.
           open(lungrd,file=grad_file,form='unformatted')
        endif
        irite=irite+1
        write(stid,'(i8)') irite
        write(lungrd) stid,rlat,rlon,rtim,nlev,nflag
        write(lungrd) ((var(j,i),j=1,n_chan),i=1,ntype)
     endif

!   End of loop over diagnostic file
  enddo loopd

  write(6,*)'read in ',iread,' obs & write out ',irite,' obs'

! Deallocate arrays
  deallocate(var,iuse,io_chan,nu_chan,frequency,wavenumbr,error)
  goto 950


!   Jump to here if eof or error reading diagnostic file.
900 continue
  write(6,*)'***PROBLEM reading diagnostic file.  diag_rad=',diag_rad

  n_chan=nchanl
  if (n_chan<=0) then
     write(6,*)'***ERROR*** invalid nchanl=',nchanl,'  STOP program'
     call errexit(93)
  endif
     
  write(6,*)'update date for control file'
  call update_ctl_horiz(n_chan,iyy,imm,idd,ihh,idhh,incr,&
       ctl_file,lunctl)

  write(6,*)'load missing value ',rmiss,' into output arrays.  ',&
       n_chan,ntype
  allocate(var(n_chan,ntype))
  do j=1,ntype
     do i=1,n_chan
        var(i,j)=rmiss
     end do
  end do
  open(lungrd,file=grad_file,form='unformatted')
  stid='missing'
  rlat=0.0
  rlon=0.0
  write(lungrd) stid,rlat,rlon,rtim,nlev,nflag
  write(lungrd) ((var(i,j),i=1,n_chan),j=1,ntype)
  irite=1
  write(6,*)'write output to lungrd=',lungrd,', file=',trim(grad_file)
  deallocate(var)


!   Close unit to diagnostic file
950 continue
  close(lndiag)


!   If data was written to GrADS file, write terminator and close file

  if (irite>0) then
     stid ='tovs1b'
     rlat =0.0
     rlon =0.0
     rtim =0.0
     nlev =0
     nflag=0
     write(lungrd) stid,rlat,rlon,rtim,nlev,nflag
     close(lungrd)
  endif

! End of program
  stop
end program horiz
