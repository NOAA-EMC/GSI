subroutine getcases(numcases,mype)
! This routine gets the names and number of available
! forecast pairs
  use kinds, only: r_kind
  use variables, only: ak5,bk5,ck5,maxcases,nsig,dimbig,hybrid,&
      filename,na,nb,zero,idpsfc5,idvm5,idthrm5,idvc5,ntrac5,cp5
  use sigio_module, only: sigio_head,sigio_srhead,sigio_sclose,&
       sigio_sropen
  implicit none
 
  integer,dimension(4):: idate4
  integer nmin24(dimbig),nmin48(dimbig),idate5(5)
  integer nmina,nminb

  real*4 fhour4

  integer i24,ierror,j48,ncount,ncases,loop,numcases,&
       mype,nming,ncase,inges,i,j,k,iret,iret2
  integer nvcoord5
  real(r_kind) fhour5,ps0
  real(r_kind),allocatable,dimension(:,:):: vcoord5

  type(sigio_head):: sighead

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
  do loop=1,ncases
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
  call sigio_sropen(inges,filename(1),iret)
  call sigio_srhead(inges,sighead,iret2)
  
  idvc5=sighead%idvc
  idvm5=sighead%idvm
  ntrac5=sighead%ntrac
  nvcoord5=sighead%nvcoord

  idpsfc5 = mod ( sighead%idvm,10 )
  idthrm5 = mod ( sighead%idvm/10,10 )

  write(6,*) 'GETCASES: idpsfc5,idthrm5 = ',idpsfc5,idthrm5

  allocate(vcoord5(nsig+1,nvcoord5))
  vcoord5=sighead%vcoord

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

  if (mype==0) write(6,*) 'END GETCASES'

  return
  end subroutine getcases


