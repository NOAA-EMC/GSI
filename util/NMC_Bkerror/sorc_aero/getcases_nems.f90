subroutine getcases_nems(numcases,mype)
! This routine gets the names and number of available
! forecast pairs
  use kinds, only: r_single,r_double,r_kind, i_kind
  use variables, only: ak5,bk5,ck5,maxcases,nlat,nlon,nsig,dimbig,hybrid,&
      filename,na,nb,zero,idpsfc5,idvm5,idthrm5,idvc5,ntrac5,cp5, &
      r60,r3600
!>swei
  use nemsio_module, only: nemsio_init,nemsio_open,nemsio_close, &
                           nemsio_gfile,nemsio_getfilehead
!<swei
  implicit none
 
  integer,dimension(4):: idate4
  integer(i_kind),dimension(7):: idate

  integer nmin24(dimbig),nmin48(dimbig),idate5(5)
  integer nmina,nminb

  real*4 fhour4

  integer(i_kind) :: i24,ierror,j48,ncount,ncases,loop,numcases,&
                     mype,nming,ncase,inges,i,j,k,iret,iret2
  integer nvcoord5

  integer(i_kind) :: latb, lonb, levs
  integer(i_kind) :: nframe
  integer(i_kind) :: nfhour, nfminute, nfsecondn, nfsecondd
  real(r_kind) fhour5,ps0
 
  real(r_single),allocatable,dimension(:,:,:) :: vcoord5
!  real(r_kind),allocatable,dimension(:,:):: vcoord5 

  type(nemsio_gfile) :: gfile

  if (mype==0) write(6,*) 'BEGIN TESTCASES'

  rewind 10
  ncases=0
  do loop=1,dimbig
    read(10,'(a100)',err=20,end=20)filename(loop)
    ncases=ncases+1
  end do
20  continue
  close(10)


  nmin24=-1
  nmin48=-1
  inges=50

  call nemsio_init(iret=iret)

  do loop=1,ncases

    call nemsio_open(gfile,filename(loop),'READ',iret=iret)

    call nemsio_getfilehead(gfile,iret=iret, nframe=nframe, &
         nfhour=nfhour, nfminute=nfminute, nfsecondn=nfsecondn,nfsecondd=nfsecondd, &
        idate=idate, dimx=lonb, dimy=latb, dimz=levs)

    if ( iret .ne. 0 ) then
       write(6,*) "Error iret not equal to zero, iret= ",iret
    else if ( levs .ne. nsig ) then !.or. lonb .ne. nlon .or. latb .ne. nlat ) then
       write(6,*) "Error: resolution is different with namelist setting"
       write(6,*) "Input file levs=",levs !,", lonb=",lonb,", latb=",latb
       stop
    end if
    
    if (iret==0 ) then
      fhour4 = float(nfhour) + float(nfminute)/r60 + float(nfsecondn)/float(nfsecondd)/r3600
      idate4(1) = idate(1)
      idate4(2) = idate(2)
      idate4(3) = idate(3)
      idate4(4) = idate(4)
    else
      fhour4=9999
      idate4=9999
    end if

    call nemsio_close(gfile,iret=iret)

    fhour5 = fhour4
    idate5(1)=idate4(4)
    idate5(2)=idate4(2)
    idate5(3)=idate4(3)
    idate5(4)=idate4(1)
    idate5(5)=0
    call w3fs21(idate5,nming)
    nming=nming+60*fhour5
    if(nint(fhour5).eq.24) nmin24(loop)=nming
    if(nint(fhour5).eq.48) nmin48(loop)=nming
25 continue
  enddo

  ncase=0
  ncount=0
  do loop=1,ncases
    i24=-1
    nmina=-1
    nminb=-1
    if(nmin24(loop).gt.0) then
      ncount=ncount+1
      if(ncount.eq.1)then
        nmina=nmin24(loop)
        i24=loop
        j48=-1
        do j=1,ncases
          if(nmin48(j).eq.nmin24(loop)) then
            nminb=nmin48(j)
            ncase=ncase+1
            na(ncase)=i24
            nb(ncase)=j
! write(6,*) 'nmin,na,nb=',ncase,nmin24(loop),na(ncase),nb(ncase)
          end if
        end do
        ncount=0
      end if  ! endif ncount
    end if    ! endif nmin24(loop)
  enddo       ! end loop to ncases

  if(mype==0)write(6,*)' number of cases available = ',ncase
  if(ncase.eq.0) then
    write(6,*)' no cases to process'
    call mpi_finalize(ierror)
    stop
  end if

  numcases=min(ncase,maxcases)
  if(mype==0)write(6,*)' number of cases to process for generating background stats = ',numcases

  
! DTK NEW  EXTRACT SOME STUFF FROM THE FIRST LISTED FILE

  call nemsio_open(gfile,filename(1),'READ',iret=iret)


  call nemsio_getfilehead(gfile, iret=iret, nframe=nframe, &
       nfhour=nfhour, nfminute=nfminute, nfsecondn=nfsecondn,nfsecondd=nfsecondd, &
       idate=idate, dimx=lonb, dimy=latb, dimz=levs, idvc=idvc5, idvm=idvm5, &
       ntrac=ntrac5) 

  allocate(vcoord5(nsig+1,3,2),cp5(ntrac5+1))
  call nemsio_getfilehead(gfile, iret=iret, vcoord=vcoord5)
  !call nemsio_getfilehead(gfile, iret=iret, vcoord=vcoord5, Cpi=cp5)

  idpsfc5 = mod ( idvm5,10 )
  idthrm5 = mod ( idvm5/10,10 )

  write(6,*) 'GETCASES: idpsfc5,idthrm5 = ',idpsfc5,idthrm5

  do k=1,nsig+1
     ak5(k)=vcoord5(k,1,1)*0.001_r_kind
     bk5(k)=vcoord5(k,2,1)
     ck5(k)=vcoord5(k,3,1)*0.001_r_kind
  end do

  deallocate(vcoord5)

  
  if (idthrm5/=3) then
     do k=1,ntrac5+1
      cp5(k)=zero
    end do
  end if

  call nemsio_close(gfile,iret=iret)

  if (mype==0) write(6,*) 'END GETCASES'

  return
  end subroutine getcases_nems


