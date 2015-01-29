program sigavg
  use sigio_module
  implicit none
  integer iopt,iret,narg,iargc,npos
  integer(sigio_intkind),parameter:: lusig1=11,lusig2=51
  integer(sigio_intkind):: irets
  character*255 copt,carg,cw,cfsig1,cfsig2
  type(sigio_head),allocatable:: head1(:)
  type(sigio_data):: data1,data2
  integer ncfsig1,ncfsig2
  integer navg,n,iw
  real,allocatable:: w(:)
  iopt=0
  iw=0
  do 
    call getopts('w:',copt,carg,iopt,iret)
    if(iret.ne.0) exit
    select case(copt)
    case('w')
      iw=1
      cw=carg
    case('?',':')
      call errmsg('sigavg: invalid option '//carg(1:1))
      call errexit(1)
    case default
      call errmsg('sigavg: invalid option '//copt(1:1))
      call errexit(1)
    end select
  enddo
  narg=iargc()
  npos=narg-iopt+1
  if(npos.lt.2) then
     if(npos.ne.0) call errmsg('sigavg: incorrect number of arguments')
     call eusage
     call errexit(1)
  endif
  navg=npos-1
  allocate(w(navg))
  if(iw.eq.1) then
    w=-9999.e9
    call fparser(cw,navg,w)
    if(any(w.eq.-9999.e9)) then
      call errmsg('sigavg: invalid weight specification')
      call errexit(3)
    endif
  else
    w=1./navg
  endif
  allocate(head1(navg))
  do n=1,navg
    call getarg(iopt+n-1,cfsig1)
    ncfsig1=len_trim(cfsig1)
    call sigio_srohdc(lusig1,cfsig1(1:ncfsig1),head1(n),data1,irets)
    if(irets.ne.0) then
      call errmsg('sigavg: error reading file '//cfsig1(1:ncfsig1))
      call errexit(2)
    endif
    if(n.eq.1) then
      call sigio_aldata(head1(n),data2,irets)
      data2%hs=w(n)*data1%hs
      data2%ps=w(n)*data1%ps
      data2%t=w(n)*data1%t
      data2%d=w(n)*data1%d
      data2%z=w(n)*data1%z
      data2%q=w(n)*data1%q
    else
      if(head1(n)%jcap.ne.head1(1)%jcap) then
        call errmsg('sigavg: spectral truncations differ')
        call errexit(3)
      endif
      if(head1(n)%levs.ne.head1(1)%levs) then
        call errmsg('sigavg: number of vertical levels differ')
        call errexit(4)
      endif
      if(sum((head1(n)%sl(:head1(1)%levs)-head1(1)%sl(:head1(1)%levs))**2)&
         .ge.1.e-10*head1(1)%levs) then
        call errmsg('sigavg: vertical structures differ')
        call errexit(4)
      endif
      if(head1(n)%ntrac.ne.head1(1)%ntrac) then
        call errmsg('sigavg: numbers of tracers differ')
        call errexit(5)
      endif
      data2%hs=data2%hs+w(n)*data1%hs
      data2%ps=data2%ps+w(n)*data1%ps
      data2%t=data2%t+w(n)*data1%t
      data2%d=data2%d+w(n)*data1%d
      data2%z=data2%z+w(n)*data1%z
      data2%q=data2%q+w(n)*data1%q
    endif
    print '(i3,2x,"ID=",i4.4,3i2.2,2x,"FH=",f6.1,2x,"WT=",g12.4,2x,"FN=",a)',&
     n,head1(n)%idate(4),head1(n)%idate(2),&
     head1(n)%idate(3),head1(n)%idate(1),head1(n)%fhour,w(n),cfsig1(1:ncfsig1)
  enddo
  call getarg(narg,cfsig2)
  ncfsig2=len_trim(cfsig2)
  call sigio_swohdc(lusig2,cfsig2(1:ncfsig2),head1(1),data2,irets)
  if(irets.ne.0) then
     call errmsg('sigavg: error writing file '//cfsig2(1:ncfsig2))
     call errexit(6)
  endif
  print '(">>>  ID=",i4.4,3i2.2,2x,"FH=",f6.1,2x,"WT=",g12.4,2x,"FN=",a)',&
   head1(1)%idate(4),head1(1)%idate(2),&
   head1(1)%idate(3),head1(1)%idate(1),head1(1)%fhour,sum(w),cfsig2(1:ncfsig2)
end program
subroutine eusage
  implicit none
  call errmsg('usage: sigavg [-w weight1,...,weightn] sigfile1 [... sigfilen] sigfileavg')
end subroutine
