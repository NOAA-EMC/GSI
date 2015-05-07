program ss2lv
  use sigio_module
  implicit none
  integer narg,iargc
  integer(sigio_intkind),parameter:: lusig=11,luvar=51,luctl=52
  integer(sigio_intkind):: irets
  character(255) cfsig,cfvar,cfctl
  integer ncfsig,ncfvar,ncfctl
  integer iret,nsig,n
  type(sigio_head),allocatable:: head(:)
  type(sigio_data):: data
  narg=iargc()
  if(narg.lt.3) then
     if(narg.ne.0) call errmsg('ss2lv: at least 3 arguments required')
     call eusage
     call errexit(1)
  endif
  call getarg(narg-1,cfvar)
  ncfvar=len_trim(cfvar)
  call baopenwt(luvar,cfvar(1:ncfvar),iret)
  if(iret.ne.0) then
     call errmsg('ss2lv: error opening file '//cfvar(1:ncfvar))
     call errexit(2)
  endif
  call getarg(narg-0,cfctl)
  ncfctl=len_trim(cfctl)
  open(luctl,file=cfctl(1:ncfctl),status='replace',iostat=iret)
  if(iret.ne.0) then
     call errmsg('ss2lv: error opening file '//cfctl(1:ncfctl))
     call errexit(2)
  endif
  nsig=narg-2
  allocate(head(nsig))
  do n=1,nsig
    call getarg(n,cfsig)
    ncfsig=len_trim(cfsig)
    call sigio_srohdc(lusig,cfsig(1:ncfsig),head(n),data,irets)
    if(irets.ne.0) then
       call errmsg('ss2lv: error opening file '//cfsig(1:ncfsig))
       call errexit(2)
    endif
    if(head(n)%levs.ne.head(1)%levs) then
       call errmsg('ss2lv: incompatible levels in file '//cfsig(1:ncfsig))
       call errexit(2)
    endif
    if(any(head(n)%sl.ne.head(1)%sl)) then
       call errmsg('ss2lv: incompatible levels in file '//cfsig(1:ncfsig))
       call errexit(2)
    endif
    call ss2lv1(luvar,head(n),data)
    call sigio_axdata(data,irets)
  enddo
  call ss2lv2(luctl,nsig,head,cfvar(1:ncfvar))
contains
subroutine eusage
  implicit none
  call errmsg('Usage: ss2lv sigfile(s) varfile ctlfile')
end subroutine
end program
subroutine ss2lv1(luvar,head,data)
  use sigio_module
  implicit none
  integer,intent(in):: luvar
  type(sigio_head),intent(in):: head
  type(sigio_data),intent(in):: data
  include 'physcons.h'
  real(4) v1(0:head%jcap)
  integer k,n
  call spvar(0,head%jcap,data%hs,v1,1)
  call wryte(luvar,4*head%jcap,log10(max(v1(1:),tiny(v1))))
  call spvar(0,head%jcap,data%ps,v1,1)
  call wryte(luvar,4*head%jcap,log10(max(v1(1:),tiny(v1))))
  do k=1,head%levs
    call spvar(0,head%jcap,data%t(1,k),v1,1)
    call wryte(luvar,4*head%jcap,log10(max(v1(1:),tiny(v1))))
  enddo
  do k=1,head%levs
    call spvar(0,head%jcap,data%d(1,k),v1,1)
    call wryte(luvar,4*head%jcap,log10(max(v1(1:),tiny(v1))))
  enddo
  do k=1,head%levs
    call spvar(0,head%jcap,data%z(1,k),v1,1)
    call wryte(luvar,4*head%jcap,log10(max(v1(1:),tiny(v1))))
  enddo
  do n=1,head%ntrac
    do k=1,head%levs
      call spvar(0,head%jcap,data%q(1,k,n),v1,1)
      call wryte(luvar,4*head%jcap,log10(max(v1(1:),tiny(v1))))
    enddo
  enddo
end subroutine
subroutine ss2lv2(luctl,nsig,head,cfvar)
  use sigio_module
  implicit none
  integer,intent(in):: luctl,nsig
  type(sigio_head),intent(in):: head(nsig)
  character*(*) cfvar
  integer idat(8),jdat(8),jhr
  real(4) rinc(5)
  character*10 cdat(8)
  integer n
  call w3movdat((/0.,head(1)%fhour,0.,0.,0./),&
                (/head(1)%idate(4),head(1)%idate(2),head(1)%idate(3),0,&
                  head(1)%idate(1),0,0,0/),idat)
  call w3pradat(idat,cdat)
  if(nsig.gt.1) then
    call w3movdat((/0.,head(2)%fhour,0.,0.,0./),&
                  (/head(2)%idate(4),head(2)%idate(2),head(2)%idate(3),0,&
                    head(2)%idate(1),0,0,0/),jdat)
    call w3difdat(jdat,idat,2,rinc)
    jhr=nint(rinc(2))
  else
    jhr=12
  endif
  write(luctl,'("dset ^",a)') cfvar
  write(luctl,'("options yrev")')
  write(luctl,'("undef -9.99E+33")')
  write(luctl,'("title ss2lv")')
  write(luctl,'("xdef",i6," levels")') head(1)%jcap
  write(luctl,'(5f12.6)') (log10(real(n)),n=1,head(1)%jcap)
  write(luctl,'("ydef",i6," linear",2f12.6)') 1,0.,1.
!!  write(luctl,'("zdef",i6," levels")') head(1)%levs
!  if (head(1)%idvc == 1) then
!    write(luctl,'("zdef",i6," levels")') head(1)%levs
!    write(luctl,'(5f12.6)') sl
!  else
    write(luctl,'("zdef",i6," linear 1 1")') head(1)%levs
!  endif
  write(luctl,'("tdef",i6," linear ",i2.2,"Z",i2.2,a3,i4.4,1x,i6,"hr")')&
   nsig,idat(5),idat(3),cdat(2)(1:3),idat(1),jhr
  write(luctl,'("vars",i6)') 5+head(1)%ntrac
  write(luctl,'("HS  ",i3," 99 surface orography (m)")') 1
  write(luctl,'("PS  ",i3," 99 surface pressure (Pa)")') 1
  write(luctl,'("T   ",i3," 99 temperature (K)")') head(1)%levs
  write(luctl,'("DIV ",i3," 99 divergence (m/s**2)")') head(1)%levs
  write(luctl,'("VOR ",i3," 99 vorticity (m/s**2)")') head(1)%levs
  do n=1,head(1)%ntrac
    write(luctl,'("Q",i1,2x,i3," 99 tracer ",i1," (kg/kg)")') n,head(1)%levs,n
  enddo
  write(luctl,'("endvars")')
end subroutine
