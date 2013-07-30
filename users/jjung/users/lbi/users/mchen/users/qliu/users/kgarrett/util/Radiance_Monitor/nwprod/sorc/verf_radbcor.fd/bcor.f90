!
!  bcor.f90
!
program bcor
  use read_diag

  implicit none
  integer ntype,mregion,surf_nregion
  parameter (ntype=16,mregion=25,surf_nregion=5)
  integer iglobal, iland, iwater, isnowice, imixed
  parameter( iglobal=1, iland=2, iwater=3, isnowice=4, imixed=5 )

  character(10),dimension(ntype):: ftype
  character(20) satname,stringd,satsis,mod_satname
  character(10) dum,satype,dplat
  character(40) string,diag_rad,data_file,ctl_file
  character(40),dimension(surf_nregion):: region
  character(10) suffix

  integer luname,lungrd,lunctl,lndiag,nregion
  integer iyy,imm,idd,ihh,idhh,incr,iread,iflag
  integer n_chan,j,idsat,i,k,ii,nsub
  integer,dimension(mregion):: jsub
  integer,allocatable,dimension(:):: io_chan,nu_chan
  integer npred_radiag,angord

  real pen,rread
  real weight,rlat,rlon,rmiss,obs,biascor,obsges,obsgesnbc,rterm
  real,dimension(2):: cor_total,cor_fixang,cor_lapse,cor_lapse2,&
       cor_const,cor_scangl,cor_clw
  real,dimension(surf_nregion):: rlatmin,rlatmax,rlonmin,rlonmax

  real,allocatable,dimension(:):: wavenumbr
  real,allocatable,dimension(:,:):: count,error,use,frequency,penalty
  real,allocatable,dimension(:,:,:):: total_cor,fixang_cor,lapse_cor,&
       lapse2_cor,const_cor,scangl_cor,clw_cor

  logical no_obs

! Variables for reading satellite data
  type(diag_header_fix_list )             :: header_fix
  type(diag_header_chan_list),allocatable :: header_chan(:)
  type(diag_data_name_list  )             :: data_name
  type(diag_data_fix_list   )             :: data_fix
  type(diag_data_chan_list  ),allocatable :: data_chan(:)
  type(diag_data_extra_list) ,allocatable :: data_extra(:,:)

  integer  nsnow, nland, nwater, nice, nmixed, ntotal
  integer  nnsnow, nnland, nnwater, nnmixed, nntotal

! Namelist with defaults
  logical               :: retrieval            = .false.
  integer               :: nchanl               = 19
  integer               :: imkctl               = 1
  integer               :: imkdata              = 1
  character(3)          :: gesanl               ='ges'
  integer               :: little_endian        = 1
  namelist /input/ satname,iyy,imm,idd,ihh,idhh,incr,&
       nchanl,suffix,imkctl,imkdata,retrieval,gesanl,little_endian

  data luname,lungrd,lunctl,lndiag / 5, 51, 52, 21 /
  data rmiss /-999./
  data stringd / '.%y4%m2%d2%h2' /
  data ftype / 'count', 'penalty', &
       'avgtotal', 'avgfixang', 'avglapse', 'avglapse2', &
       'avgconst', 'avgscangl', 'avgclw', &
       'sdvtotal', 'sdvfixang', 'sdvlapse', 'sdvlapse2', &
       'sdvconst', 'sdvscangl', 'sdvclw' /
  data region / 'global', 'land', 'water', 'ice/snow', 'mixed'/
  data rlonmin / -180., -180., -180., -180., -180./
  data rlonmax / 180., 180., 180., 180., 180./
  data rlatmin / -90., -90., -90., -90., -90./
  data rlatmax / 90., 90., 90., 90., 90./



!************************************************************************
!
! Initialize variables
  iread=0
  nregion=surf_nregion
  npred_radiag = 5

! Read namelist input
  read(luname,input)
  write(6,input)
  write(6,*)'gesanl = ', gesanl
  write(6,*)' '

! Ensure number of requested regions does not exceed specified upper limit
  if (surf_nregion>mregion) then
     write(6,*)'***ERROR*** too many regions specified'
     write(6,*)'   maximum allowed:  mregion=',mregion
     write(6,*)'    user requested:  surf_nregion=',surf_nregion
     call errexit(91)
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
  write(6,*)'suffix   =',suffix


! Open unit to diagnostic file.  Read portion of header to 
! see if file exists
  open(lndiag,file=diag_rad,form='unformatted')
  read(lndiag,err=900,end=900) dum
  rewind lndiag

! File exists.  Read header
  write(6,*)'call read_diag_header'
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
  angord = header_fix%angord

  write(6,*)'satype,dplat,n_chan=',satype,' ',dplat,n_chan

  string = trim(satype)//'_'//trim(dplat)
  write(6,*)'string,satname=',string,' ',satname
  if ( trim(string) /= trim(satname) ) then
     write(6,*)'***ERROR*** inconsistent instrument types'
     write(6,*)'  satname,string  =',satname,' ',string
     call errexit(92)
  endif


! Allocate arrays to hold observational information
  write(6,*)' '
  write(6,*)'allocate arrays'
  allocate (io_chan(n_chan), nu_chan(n_chan), wavenumbr(n_chan))
  allocate (total_cor(n_chan,mregion,2), fixang_cor(n_chan,mregion,2), &
       lapse_cor(n_chan,mregion,2), lapse2_cor(n_chan,mregion,2), &
       const_cor(n_chan,mregion,2), scangl_cor(n_chan,mregion,2), &
       clw_cor(n_chan,mregion,2), count(n_chan,mregion), & 
       penalty(n_chan,mregion), &
       error(n_chan,mregion), use(n_chan,mregion), &
       frequency(n_chan,mregion))

! Zero accumulator arrays
  do ii=1,2
     do k=1,mregion
        do j=1,n_chan
           if (ii==1) then
              count(j,k) = 0.0
              penalty(j,k) = 0.0
           endif
           total_cor(j,k,ii)  = 0.0
           fixang_cor(j,k,ii) = 0.0
           lapse_cor(j,k,ii)  = 0.0
           lapse2_cor(j,k,ii) = 0.0
           const_cor(j,k,ii)  = 0.0
           scangl_cor(j,k,ii) = 0.0
           clw_cor(j,k,ii)    = 0.0
        end do
     end do
  end do


! Extract satinfo relative index
  do j=1,n_chan
     nu_chan(j)   = real( header_chan(j)%nuchan, 4 )
     io_chan(j)   = real( header_chan(j)%iochan, 4 )
     wavenumbr(j) = real( header_chan(j)%wave, 4 )
  end do
  do k=1,mregion
     do j=1,n_chan
        error(j,k)     = real( header_chan(j)%varch, 4)
        use(j,k)       = real( header_chan(j)%iuse, 4 )
        frequency(j,k) = real( header_chan(j)%freq, 4)
     end do
  end do
        

! Create GrADS control file
  if ( imkctl == 1 ) then
     write(6,*)'call create_ctl_bcor'

     if ( trim(gesanl) == 'ges' ) then
        mod_satname = trim(satname)
     else
        mod_satname = trim(satname) // '_anl'
     endif


     call create_ctl_bcor(ntype,ftype,n_chan,iyy,imm,idd,ihh,idhh,&
          incr,ctl_file,lunctl,rmiss,mod_satname,satype,dplat,surf_nregion,&
          region,rlonmin,rlonmax,rlatmin,rlatmax,nu_chan,use(1,1),error(1,1),&
          frequency(1,1),wavenumbr,little_endian)
  endif

  nwater = 0; nnwater = 0
  nland  = 0; nnland  = 0
  nsnow  = 0; nnsnow  = 0
  nice   = 0
  nmixed = 0; nnmixed = 0
  ntotal = 0

! Loop to read entries in diagnostic file
  if ( imkdata == 1 ) then
     iflag = 0
     loopd:  do while (iflag == 0)

!       Read a record.  If read flag, iflag does not equal zero, exit loopd
        call read_radiag_data( lndiag, header_fix, retrieval, data_fix, data_chan,&
           data_extra, iflag )
        if( iflag /= 0 ) exit loopd
        iread=iread+1

!       Extract obervation location and mpi weight.  Convert (0-360) lon to (-180,180)
        rlat   = data_fix%lat
        rlon   = data_fix%lon
        if (rlon>180.) rlon = rlon - 360.
        rread  = rread + 1.0 

        if ( data_fix%water_frac > 0.99 ) then
           nwater = nwater + 1
        else if ( data_fix%land_frac  > 0.99 ) then
           nland  = nland  + 1
        else if ( data_fix%snow_frac  > 0.99 ) then
           nsnow  = nsnow  + 1
        else if ( data_fix%ice_frac   > 0.99 ) then
           nice   = nice   + 1
        else
           nmixed = nmixed + 1
        end if

        ntotal = ntotal + 1


!       Detemine into which subdomains the observation falls.
!       These are now based on surface type, not geography.  All
!       obs match global (surf_region 1).
!
        ii=0; jsub=0;
        jsub(1)=iglobal
        if ( data_fix%land_frac  > 0.99 ) then
           jsub(2)=iland
           nsub=2
           nnland=nnland+1
        else if ( data_fix%water_frac > 0.99 ) then
           jsub(2)=iwater
           nsub=2
           nnwater=nnwater+1
        else if (( data_fix%snow_frac > 0.99 ) .OR. ( data_fix%ice_frac > 0.99 )) then
           jsub(2)=isnowice
           nsub=2
           nnsnow=nnsnow+1
        else
           jsub(2)=imixed
           nsub=2
           nnmixed=nnmixed+1
           write(6,*)'data_fix%land_frac,water,snow,ice = ',data_fix%land_frac, data_fix%water_frac, data_fix%snow_frac, data_fix%ice_frac
        end if


!       Channel loop
        do j = 1, n_chan

!          If observation was assimilated, accumulate sums in appropriate regions
           if (data_chan(j)%errinv > 1.e-6) then
              pen           =  data_chan(j)%errinv*(data_chan(j)%omgbc)**2
              cor_total(1)  =  (data_chan(j)%omgnbc - data_chan(j)%omgbc)
              cor_fixang(1) =  data_chan(j)%bifix(angord+1)
              cor_lapse(1)  =  data_chan(j)%bilap
              cor_lapse2(1) =  data_chan(j)%bilap2
              cor_const(1)  =  data_chan(j)%bicons
              cor_scangl(1) =  data_chan(j)%biang
              cor_clw(1)    =  data_chan(j)%biclw

              cor_total(2)  =  (data_chan(j)%omgnbc - data_chan(j)%omgbc)**2
              cor_fixang(2) =  (data_chan(j)%bifix(angord+1))**2
              cor_lapse(2)  =  (data_chan(j)%bilap)**2
              cor_lapse2(2) =  (data_chan(j)%bilap2)**2
              cor_const(2)  =  (data_chan(j)%bicons)**2
              cor_scangl(2) =  (data_chan(j)%biang)**2
              cor_clw(2)    =  (data_chan(j)%biclw)**2
           
              do i=1,nsub
                 k=jsub(i)
                 count(j,k) = count(j,k) +1.0 
                 penalty(j,k) = penalty(j,k) + pen

                 do ii=1,2
                    total_cor(j,k,ii)  = total_cor(j,k,ii)  + cor_total(ii)
                    fixang_cor(j,k,ii) = fixang_cor(j,k,ii) + cor_fixang(ii)
                    lapse_cor(j,k,ii)  = lapse_cor(j,k,ii)  + cor_lapse(ii)
                    lapse2_cor(j,k,ii) = lapse2_cor(j,k,ii) + cor_lapse2(ii)
                    const_cor(j,k,ii)  = const_cor(j,k,ii)  + cor_const(ii)
                    scangl_cor(j,k,ii) = scangl_cor(j,k,ii) + cor_scangl(ii)
                    clw_cor(j,k,ii)    = clw_cor(j,k,ii)    + cor_clw(ii)
                 end do

              end do
           endif

        enddo ! channel loop

!    End of loop over diagnostic file
     enddo loopd
     close(lndiag)
     write(6,*)' '
     write(6,*)'read in ',iread,' obs ',rread
     write(6,*)' '

     write(6,*)'nwater, nland, nice, nsnow, nmixed, ntotal = ', nwater, nland, nice, nsnow, nmixed, ntotal
     nntotal=nnwater+nnland+nnsnow+nnmixed
     write(6,*)'nnwater, nnland, nnsnow, nnmixed, nntotal = ', nnwater, nnland, nnsnow, nnmixed, nntotal

!    Compute average and standard deviation
     do k=1,surf_nregion
        do j=1,n_chan
           call avgsdv(count(j,k),total_cor(j,k,1), total_cor(j,k,2), rmiss)
           call avgsdv(count(j,k),fixang_cor(j,k,1),fixang_cor(j,k,2),rmiss)
           call avgsdv(count(j,k),lapse_cor(j,k,1), lapse_cor(j,k,2), rmiss)
           call avgsdv(count(j,k),lapse2_cor(j,k,1),lapse2_cor(j,k,2),rmiss)
           call avgsdv(count(j,k),const_cor(j,k,1), const_cor(j,k,2), rmiss)
           call avgsdv(count(j,k),scangl_cor(j,k,1),scangl_cor(j,k,2),rmiss)
           call avgsdv(count(j,k),clw_cor(j,k,1),   clw_cor(j,k,2),   rmiss)
           if (count(j,k)>0) then
              penalty(j,k)=penalty(j,k)/count(j,k)
           else
              count(j,k)=rmiss
              penalty(j,k)=rmiss
           endif
        end do
     end do

!    Write output to binary output file
     write(6,*)' '
     open(lungrd,file=data_file,form='unformatted')
     write(lungrd) ((count(j,k),j=1,n_chan),k=1,surf_nregion)
     write(lungrd) ((penalty(j,k),j=1,n_chan),k=1,surf_nregion)
     do ii=1,2
        write(lungrd) ((total_cor (j,k,ii),j=1,n_chan),k=1,surf_nregion)
        write(lungrd) ((fixang_cor(j,k,ii),j=1,n_chan),k=1,surf_nregion)
        write(lungrd) ((lapse_cor (j,k,ii),j=1,n_chan),k=1,surf_nregion)
        write(lungrd) ((lapse2_cor(j,k,ii),j=1,n_chan),k=1,surf_nregion)
        write(lungrd) ((const_cor (j,k,ii),j=1,n_chan),k=1,surf_nregion)
        write(lungrd) ((scangl_cor(j,k,ii),j=1,n_chan),k=1,surf_nregion)
        write(lungrd) ((clw_cor   (j,k,ii),j=1,n_chan),k=1,surf_nregion)
     end do
     write(6,*)'write output to lungrd=',lungrd,', file=',trim(data_file)
     close(lungrd)
  endif 

! Deallocate arrays
  write(6,*)' '
  write(6,*)'deallocate arrays'
  deallocate(io_chan,nu_chan,wavenumbr,total_cor,fixang_cor,lapse_cor,&
       lapse2_cor,const_cor,scangl_cor,clw_cor,count,penalty,error,use,&
       frequency)
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
     
  write(6,*)'load missing value ',rmiss,' into output arrays.  ',&
       surf_nregion,n_chan
  allocate(count(n_chan,surf_nregion),penalty(n_chan,surf_nregion))
  allocate(total_cor(n_chan,mregion,2), fixang_cor(n_chan,mregion,2), &
       lapse_cor(n_chan,mregion,2), lapse2_cor(n_chan,mregion,2), &
       const_cor(n_chan,mregion,2), scangl_cor(n_chan,mregion,2), &
       clw_cor(n_chan,mregion,2))

  write(6,*)'load missing value ',rmiss,' into output arrays'
  do ii=1,2
     do k=1,surf_nregion
        do j=1,n_chan
           if (ii==1) then
              count(j,k)  =rmiss
              penalty(j,k)=rmiss
           endif
           total_cor(j,k,ii) =rmiss
           fixang_cor(j,k,ii)=rmiss
           lapse_cor(j,k,ii) =rmiss
           lapse2_cor(j,k,ii)=rmiss
           const_cor(j,k,ii) =rmiss
           scangl_cor(j,k,ii)=rmiss
           clw_cor(j,k,ii)   =rmiss
        end do
     end do
  end do

  if ( imkdata == 1 ) then
     open(lungrd,file=data_file,form='unformatted')
     write(lungrd) ((count(j,k),j=1,n_chan),k=1,surf_nregion)
     write(lungrd) ((penalty(j,k),j=1,n_chan),k=1,surf_nregion)
     do ii=1,2
        write(lungrd) ((total_cor (j,k,ii),j=1,n_chan),k=1,surf_nregion)
        write(lungrd) ((fixang_cor(j,k,ii),j=1,n_chan),k=1,surf_nregion)
        write(lungrd) ((lapse_cor (j,k,ii),j=1,n_chan),k=1,surf_nregion)
        write(lungrd) ((lapse2_cor(j,k,ii),j=1,n_chan),k=1,surf_nregion)
        write(lungrd) ((const_cor (j,k,ii),j=1,n_chan),k=1,surf_nregion)
        write(lungrd) ((scangl_cor(j,k,ii),j=1,n_chan),k=1,surf_nregion)
        write(lungrd) ((clw_cor   (j,k,ii),j=1,n_chan),k=1,surf_nregion)
     end do
     write(6,*)'write output to lungrd=',lungrd,', file=',trim(data_file)
     close(lungrd)
  endif
  deallocate(count,penalty,total_cor,fixang_cor,lapse_cor,lapse2_cor,&
       const_cor,scangl_cor,clw_cor)

! End of program
950 continue
  stop
end program bcor
