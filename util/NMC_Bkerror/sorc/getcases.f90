subroutine getcases(numcases,mype)
!   2017-10-25  Gael Descombes (NCAR) - capability to read nemsio files
! This routine gets the names and number of available
! forecast pairs
  use kinds, only: r_kind,r_single
  use variables, only: ak5,bk5,ck5,maxcases,nsig,dimbig,hybrid,&
      filename,na,nb,zero,idpsfc5,idvm5,idthrm5,idvc5,ntrac5,cp5,&
      use_enkf,use_gfs_nemsio,ncepgfs_head,nlonin,nlatin
  use sigio_module, only: sigio_head,sigio_srhead,sigio_sclose,&
       sigio_sropen
  use nemsio_module, only:  nemsio_init,nemsio_open,nemsio_close
  use nemsio_module, only:  nemsio_gfile,nemsio_getfilehead
  implicit none

  integer,parameter:: i_missing=-9999 
  integer,dimension(4):: idate4
  integer nmin24(dimbig),nmin48(dimbig),idate5(5)
  integer nmina,nminb

  real*4 fhour4

  integer i24,ierror,j48,ncount,ncases,loop,numcases,&
       mype,nming,ncase,inges,i,j,k,iret,iret2
  integer nvcoord5
  real(r_kind) fhour5,ps0
  real(r_kind),allocatable,dimension(:,:):: vcoord5

 ! for nemsio
  character(8) filetype, mdlname
  character(100) fname
  integer,dimension(7):: idate
  integer :: nfhour, nfminute, nfsecondn, nfsecondd, ncount24, ncount48
  real(r_single),allocatable,dimension(:,:,:) :: nems_vcoord

  type(sigio_head):: sighead
  type(ncepgfs_head):: gfshead
  type(nemsio_gfile) :: gfile
  logical :: isfile

  if (mype==0) write(6,*) 'BEGIN TESTCASES'

  rewind 10
  ncases=0
  do loop=1,dimbig
    read(10,'(a)',err=20,end=20)filename(loop)
    ncases=ncases+1
  end do
20  continue
  close(10)

  nmin24=-1
  nmin48=-1
  ncount24 = 0
  ncount48 = 0
  inges=50
  do loop=1,ncases
     if ( use_gfs_nemsio ) then
        call nemsio_init(iret=iret2)
        if ( iret2 /= 0 ) then
           write(6,*)' getcases:  ***ERROR*** problem nemsio_init file = ', &
              trim(filename(loop)),', Status = ',iret2
           stop
        end if
        call nemsio_open(gfile,filename(loop),'READ',iret=iret2)
        if ( iret2 /= 0 ) then
           write(6,*)' getcases:  ***ERROR*** problem opening file = ', &
              trim(filename(loop)),', Status = ',iret2
           stop
        end if

        idate         = i_missing
        nfhour        = i_missing
        nfminute      = i_missing
        nfsecondn     = i_missing
        nfsecondd     = i_missing
        gfshead%idsl  = i_missing
        call nemsio_getfilehead(gfile, idate=idate, gtype=filetype,  &
           modelname=mdlname, nfhour=nfhour, nfminute=nfminute,       &
           nfsecondn=nfsecondn, nfsecondd=nfsecondd,                  &
           dimx=gfshead%lonb, dimy=gfshead%latb,   dimz=gfshead%levs, &
           jcap=gfshead%jcap, ntrac=gfshead%ntrac, idvc=gfshead%idvc, &
           idsl=gfshead%idsl,   ncldt=gfshead%ncldt, iret=iret2)
        fhour5 = float(nfhour) + float(nfminute)/60.0 + &
                 float(nfsecondn)/float(nfsecondd)/3600.0
        idate5(1) = idate(1)  !year
        idate5(2) = idate(2)  !month
        idate5(3) = idate(3)  !day
        idate5(4) = idate(4)  !hour
        idate5(5) = 0
        call nemsio_close(gfile,iret=iret2)

        nlonin=gfshead%lonb
        nlatin=gfshead%latb

     else ! not use_gfs_nemsio

       call sigio_sropen(inges,filename(loop),iret)
       call sigio_srhead(inges,sighead,iret2)
    
       if (iret==0 .and. iret2==0) then
         fhour4=sighead%fhour
         idate4=sighead%idate
       else
         fhour4=9999
         idate4=9999
       end if

       call sigio_sclose(inges,iret)

       fhour5 = fhour4
       idate5(1)=idate4(4)
       idate5(2)=idate4(2)
       idate5(3)=idate4(3)
       idate5(4)=idate4(1)
       idate5(5)=0
    endif

    call w3fs21(idate5,nming)
    if (use_enkf) then
       ! file contains ensmean file names and ens member file names
       ! (ens mean files first, ens mem files after or vice versa)
       fname = filename(loop)
       if (fname(INDEX(fname,'_',BACK=.TRUE.)+1:len(fname)) == 'ensmean') then
           nmin24(loop) = nming
           ncount24 = ncount24 + 1
           na(ncount24) = loop
       else
           ncount48 = ncount48 + 1
           nmin48(loop) = nming
           nb(ncount48) = loop  
       endif
    else
       nming=nming+60*fhour5
       if(nint(fhour5).eq.24) nmin24(loop)=nming
       if(nint(fhour5).eq.48) nmin48(loop)=nming
    endif
25 continue
  enddo

  if (.not. use_enkf) then
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
             end if
           end do
           ncount=0
         end if  ! endif ncount
       end if    ! endif nmin24(loop)
     enddo       ! end loop to ncases
  else
     if (ncount24 .ne. ncount48) then
        write(6,*) '# of ensmean filenames not equal to # of ens mem filenames',ncount24,ncount48
        call mpi_finalize(ierror)
        stop
     else
       ncase = ncount24
     endif
  endif

  if(mype==0)write(6,*)' number of cases available = ',ncase
  if(ncase.eq.0) then
    write(6,*)' no cases to process'
    call mpi_finalize(ierror)
    stop
  end if

  numcases=min(ncase,maxcases)
  if(mype==0)write(6,*)' number of cases to process for generating background stats = ',numcases

  if ( use_gfs_nemsio ) then
     ! hardwired for now
     idpsfc5 = 2
     idvc5   = 2
     idthrm5 = 2
     ntrac5  = 3

     call nemsio_init(iret=iret2)
     if ( iret2 /= 0 ) then
        write(6,*)' GESINFO:  ***ERROR*** problem nemsio_init file = ', &
           trim(filename(1)),', Status = ',iret2
        stop
     end if
     call nemsio_open(gfile,filename(1),'READ',iret=iret2)
     if ( iret2 /= 0 ) then
        write(6,*)' GESINFO:  ***ERROR*** problem opening file = ', &
           trim(filename(1)),', Status = ',iret2
        stop
     end if

     allocate(nems_vcoord(nsig+1,3,2))
     call nemsio_getfilehead(gfile,iret=iret2,vcoord=nems_vcoord)
     if ( iret2 /= 0 ) then
        write(6,*)' GESINFO:  ***ERROR*** problem reading header ', &
           'vcoord, Status = ',iret2
        stop
     endif

!    Determine the type of vertical coordinate used by model because that
!    gfshead%nvcoord is no longer part of NEMSIO header output.
     nvcoord5=3
     if(maxval(nems_vcoord(:,3,1))==zero .and. &
        minval(nems_vcoord(:,3,1))==zero ) then
        nvcoord5=2
        if(maxval(nems_vcoord(:,2,1))==zero .and. &
           minval(nems_vcoord(:,2,1))==zero ) then
           nvcoord5=1
        end if
     end if

     allocate(vcoord5(nsig+1,nvcoord5))
     vcoord5(:,1:nvcoord5)=nems_vcoord(:,1:nvcoord5,1)

     deallocate(nems_vcoord)

  else ! not use_gfs_nemsio
  
! DTK NEW  EXTRACT SOME STUFF FROM THE FIRST LISTED FILE
     call sigio_sropen(inges,filename(1),iret)
     call sigio_srhead(inges,sighead,iret2)
  
     idvc5=sighead%idvc
     idvm5=sighead%idvm
     ntrac5=sighead%ntrac
     nvcoord5=sighead%nvcoord

     allocate(vcoord5(nsig+1,nvcoord5))
     vcoord5=sighead%vcoord

     idpsfc5 = mod ( sighead%idvm,10 )
     idthrm5 = mod ( sighead%idvm/10,10 )

     write(6,*) 'GETCASES: idpsfc5,idthrm5 = ',idpsfc5,idthrm5


     allocate(cp5(ntrac5+1))
  
     if (idthrm5==3) then
       do k=1,sighead%ntrac+1
         cp5(k)=sighead%cpi(k)
         if (mype==0) write(6,*) 'k,cp5 = ',cp5(k)
       end do
     else
        do k=1,ntrac5+1
         cp5(k)=zero
        end do
     end if

     call sigio_sclose(inges,iret)
  endif !no use_gfs_nemsio

  do k=1,nsig+1
     ak5(k)=zero
     bk5(k)=zero
     ck5(k)=zero
  end do

  do k=1,nsig+1
     if (nvcoord5 ==1 ) then
        bk5(k)=vcoord5(k,1)
     else if (nvcoord5 >= 2) then
        ak5(k)=vcoord5(k,1)*0.001_r_kind
        bk5(k)=vcoord5(k,2)
     else if (nvcoord5 >= 3) then
        ck5(k)=vcoord5(k,3)*0.001_r_kind
     end if
  end do
 
  deallocate(vcoord5)


  if (mype==0) write(6,*) 'END GETCASES'

  return
end subroutine getcases


