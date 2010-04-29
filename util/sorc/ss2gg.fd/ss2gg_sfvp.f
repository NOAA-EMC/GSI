program ss2gg
  use sigio_module
  implicit none
  integer narg,iargc
  integer(sigio_intkind),parameter:: lusig=11,luggg=51,luctl=52
  integer(sigio_intkind):: irets
  character(255) cfsig,cfggg,cfctl,cidrt,cimax,cjmax
  integer ncfsig,ncfggg,ncfctl,ncidrt,ncimax,ncjmax
  integer iret,idrt,imax,jmax,nsig,n
  type(sigio_head),allocatable:: head(:)
  type(sigio_data):: data
  narg=iargc()
  if(narg.lt.6) then
     if(narg.ne.0) call errmsg('ss2gg: at least 6 arguments required')
     call eusage
     call errexit(1)
  endif
  call getarg(narg-2,cidrt)
  ncidrt=len_trim(cidrt)
  call fparsei(cidrt(1:ncidrt),1,idrt)
  if(idrt.ne.0.and.idrt.ne.4.and.idrt.ne.256) then
     call errmsg('ss2gg: invalid value for idrt '//cidrt(1:ncidrt))
     call errexit(2)
  endif
  call getarg(narg-1,cimax)
  ncimax=len_trim(cimax)
  call fparsei(cimax(1:ncimax),1,imax)
  if(imax.le.0) then
     call errmsg('ss2gg: invalid value for imax '//cimax(1:ncimax))
     call errexit(2)
  endif
  call getarg(narg-0,cjmax)
  ncjmax=len_trim(cjmax)
  call fparsei(cjmax(1:ncjmax),1,jmax)
  if(jmax.le.0) then
     call errmsg('ss2gg: invalid value for jmax '//cjmax(1:ncjmax))
     call errexit(2)
  endif
  call getarg(narg-4,cfggg)
  ncfggg=len_trim(cfggg)
  call baopenwt(luggg,cfggg(1:ncfggg),iret)
  if(iret.ne.0) then
     call errmsg('ss2gg: error opening file '//cfggg(1:ncfggg))
     call errexit(2)
  endif
  call getarg(narg-3,cfctl)
  ncfctl=len_trim(cfctl)
  open(luctl,file=cfctl(1:ncfctl),status='replace',iostat=iret)
  if(iret.ne.0) then
     call errmsg('ss2gg: error opening file '//cfctl(1:ncfctl))
     call errexit(2)
  endif
  nsig=narg-5
  allocate(head(nsig))
  do n=1,nsig
    call getarg(n,cfsig)
    ncfsig=len_trim(cfsig)
    call sigio_srohdc(lusig,cfsig(1:ncfsig),head(n),data,irets)
    if(irets.ne.0) then
       call errmsg('ss2gg: error opening file '//cfsig(1:ncfsig))
       call errexit(2)
    endif
    if(head(n)%levs.ne.head(1)%levs.or.&
       head(n)%idvc.ne.head(1)%idvc.or.&
       head(n)%idsl.ne.head(1)%idsl.or.&
       any(head(n)%si.ne.head(1)%si).or.&
       any(head(n)%ak.ne.head(1)%ak).or.&
       any(head(n)%bk.ne.head(1)%bk)) then
       call errmsg('ss2gg: incompatible levels in file '//cfsig(1:ncfsig))
       call errexit(2)
    endif
    call ss2gg1(luggg,idrt,imax,jmax,head(n),data)
    call sigio_axdata(data,irets)
  enddo
  call ss2gg2(luctl,idrt,imax,jmax,nsig,head,cfggg(1:ncfggg))
contains
subroutine eusage
  implicit none
  call errmsg('Usage: ss2gg sigfile(s) gggfile ctlfile idrt imax jmax')
end subroutine
end program
subroutine ss2gg1(luggg,idrt,imax,jmax,head,data)
  use sigio_module
  implicit none
  integer,intent(in):: luggg,idrt,imax,jmax
  type(sigio_head),intent(in):: head
  type(sigio_data),intent(in):: data
  include 'physcons.h'
  real(4) f1(imax*jmax),f2(imax*jmax),f3(imax*jmax),f4(imax*jmax)
  real(4) g1(imax*jmax,head%levs),g2(imax*jmax,head%levs)
  real(4) pm(imax*jmax,head%levs)
  real(8) del2i((head%jcap+1)*(head%jcap+2))
  real(4),dimension((head%jcap+1)*(head%jcap+2),head%levs):: sf,vp

  real(4),allocatable:: eps(:),epstop(:),enn1(:),elonn1(:),eon(:),eontop(:)

  integer k,n

! for inverse laplacian --> SF/VP
  allocate(eps((head%jcap+1)*(head%jcap+2)/2),epstop(head%jcap+1))
  allocate(enn1((head%jcap+1)*(head%jcap+2)/2))
  allocate(elonn1((head%jcap+1)*(head%jcap+2)/2))
  allocate(eon((head%jcap+1)*(head%jcap+2)/2),eontop(head%jcap+1))

  call spwget(0,head%jcap,eps,epstop,enn1,elonn1,eon,eontop)

  call sptez(0,head%jcap,idrt,imax,jmax,data%hs,f1,1)
  call wryte(luggg,4*imax*jmax,f1)
  call sptez(0,head%jcap,idrt,imax,jmax,data%ps,f1,1)
  f1=exp(f1)*1.e3
  call wryte(luggg,4*imax*jmax,f1)
  call modpr(imax*jmax,imax*jmax,head%levs,head%idvc,head%idsl,&
             head%si,head%ak,head%bk,f1,pm,g2)
  call wryte(luggg,4*imax*jmax*head%levs,pm)
  call wryte(luggg,4*imax*jmax*head%levs,g2)
  call sptezm(0,head%jcap,idrt,imax,jmax,head%levs,data%t,g1,1)
  call sptezm(0,head%jcap,idrt,imax,jmax,head%levs,data%q,g2,1)
  call wryte(luggg,4*imax*jmax*head%levs,g1)
  g1=g1/(1+con_fvirt*g2)
  call wryte(luggg,4*imax*jmax*head%levs,g1)
  call wryte(luggg,4*imax*jmax*head%levs,g2)
  do k=1,head%levs
    call getrh(imax*jmax,pm(1,k),g2(1,k),g1(1,k),f3,f4)
    call wryte(luggg,4*imax*jmax,f4)
  enddo

  call sptezmv(0,head%jcap,idrt,imax,jmax,head%levs,data%d,data%z,g1,g2,1)
  call wryte(luggg,4*imax*jmax*head%levs,g1)
  call wryte(luggg,4*imax*jmax*head%levs,g2)

  call sptezm(0,head%jcap,idrt,imax,jmax,head%levs,data%d,g1,1)
  call sptezm(0,head%jcap,idrt,imax,jmax,head%levs,data%z,g2,1)
  call wryte(luggg,4*imax*jmax*head%levs,g1)
  call wryte(luggg,4*imax*jmax*head%levs,g2)

  do k=1,head%levs
    vp(:,k)=data%d(:,k)
    sf(:,k)=data%z(:,k)
    call splaplac(0,head%jcap,enn1,vp(1,k),vp(1,k),-1)
    call splaplac(0,head%jcap,enn1,sf(1,k),sf(1,k),-1)
    vp(1:2,k)=0.
    sf(1:2,k)=0.
  enddo
  call sptezm(0,head%jcap,idrt,imax,jmax,head%levs,vp,g1,1)
  call sptezm(0,head%jcap,idrt,imax,jmax,head%levs,sf,g2,1)
  call wryte(luggg,4*imax*jmax*head%levs,g1)
  call wryte(luggg,4*imax*jmax*head%levs,g2)

  do n=2,head%ntrac
    call sptezm(0,head%jcap,idrt,imax,jmax,head%levs,data%q(1,1,n),g1,1)
    call wryte(luggg,4*imax*jmax*head%levs,g1)
  enddo
end subroutine
subroutine ss2gg2(luctl,idrt,imax,jmax,nsig,head,cfggg)
  use sigio_module
  implicit none
  integer,intent(in):: luctl,idrt,imax,jmax,nsig
  type(sigio_head),intent(in):: head(nsig)
  character*(*) cfggg
  real(4) slat(jmax),wlat(jmax)
  integer idat(8),jdat(8),jhr
  real(4) rinc(5)
  character*10 cdat(8)
  integer n
  real(4) sl(head(1)%levs),dl(head(1)%levs)
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
  call modpr(1,1,head(1)%levs,head(1)%idvc,head(1)%idsl,&
             head(1)%si,head(1)%ak,head(1)%bk,1.e5,sl,dl)
  sl=sl/1.e5
  dl=dl/1.e5
  if(cfggg(1:1).eq.'/') then
    write(luctl,'("dset ",a)') cfggg
  else
    write(luctl,'("dset ^",a)') cfggg
  endif
  write(luctl,'("options yrev")')
  write(luctl,'("undef -9.99E+33")')
  write(luctl,'("title ss2gg")')
  write(luctl,'("xdef",i6," linear",2f12.6)') imax,0.d0,360.d0/imax
  if(idrt.eq.0) then
    write(luctl,'("ydef",i6," linear",2f12.6)')&
     jmax,-90.d0,180.d0/(jmax-1)
  elseif(idrt.eq.256) then
    write(luctl,'("ydef",i6," linear",2f12.6)')&
     jmax,-90.d0*(jmax-1)/jmax,180.d0/jmax
  elseif(idrt.eq.4) then
    call splat(idrt,jmax,slat,wlat)
    write(luctl,'("ydef",i6," levels")') jmax
    write(luctl,'(5f12.6)') 180.d0/acos(-1.d0)*asin(dble(slat(jmax:1:-1)))
  endif
  write(luctl,'("zdef",i6," levels")') head(1)%levs
  write(luctl,'(5f12.6)') sl
  write(luctl,'("tdef",i6," linear ",i2.2,"Z",i2.2,a3,i4.4,1x,i6,"hr")')&
   nsig,idat(5),idat(3),cdat(2)(1:3),idat(1),jhr
  write(luctl,'("vars",i6)') 13+head(1)%ntrac
  write(luctl,'("HS  ",i3," 99 surface orography (m)")') 1
  write(luctl,'("PS  ",i3," 99 surface pressure (Pa)")') 1
  write(luctl,'("P   ",i3," 99 pressure (Pa)")') head(1)%levs
  write(luctl,'("DP  ",i3," 99 delta pressure (Pa)")') head(1)%levs
  write(luctl,'("TV  ",i3," 99 virtual temperature (K)")') head(1)%levs
  write(luctl,'("T   ",i3," 99 sensible temperature (K)")') head(1)%levs
  write(luctl,'("Q   ",i3," 99 specific humidity (kg/kg)")') head(1)%levs
  write(luctl,'("RH  ",i3," 99 relative humidity (%)")') head(1)%levs
  write(luctl,'("U   ",i3," 99 zonal wind (m/s)")') head(1)%levs
  write(luctl,'("V   ",i3," 99 meridional wind (m/s)")') head(1)%levs
  write(luctl,'("DIV ",i3," 99 divergence (m/s**2)")') head%levs
  write(luctl,'("VOR ",i3," 99 vorticity (m/s**2)")') head%levs
  write(luctl,'("VP  ",i3," 99 potential (m)")') head%levs
  write(luctl,'("SF  ",i3," 99 streamfcn (m)")') head%levs
  do n=2,head(1)%ntrac
    write(luctl,'("Q",i1,2x,i3," 99 tracer ",i1," (kg/kg)")') n,head(1)%levs,n
  enddo
  write(luctl,'("endvars")')
end subroutine
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  subroutine getrh(km,p,sh,t,shs,rh)
    implicit none
    integer,intent(in):: km
    real(4),intent(in):: p(km),sh(km),t(km)
    real(4),intent(out):: shs(km),rh(km)
    include 'physcons.h'
    real(4) tr,es
    integer k
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    do k=1,km
      tr=con_ttp/t(k)
      es=con_psat*(tr**con_xpona)*exp(con_xponb*(1.-tr))
      es=min(es,p(k))
      shs(k)=con_eps*es/(p(k)+con_epsm1*es)
!     rh(k)=1.e2*min(max(sh(k)/shs(k),0.),1.)
      rh(k)=1.e2*sh(k)/shs(k)
    enddo
  end subroutine
!-----------------------------------------------------------------------
subroutine modpr(im,ix,km,idvc,idsl,si,ak,bk,ps,pm,pd)
!$$$  subprogram documentation block
!
! subprogram:    modpr       compute model pressures
!   prgmmr: iredell          org: w/nmc23     date: 92-10-31
!
! abstract: compute model pressures.
!
! program history log:
! 2001-07-25  mark iredell
!
! usage:    call modpr(im,ix,km,idvc,idsl,si,ak,bk,ps,pm,pd)
!   input argument list:
!     im           integer number of points to compute
!     ix           integer first dimension
!     km           integer number of levels
!     idvc         integer vertical coordinate id
!                  (1 for sigma and 2 for hybrid)
!     idsl         integer type of sigma structure
!                  (1 for phillips or 2 for mean)
!     si           real (km+1) sigma interface values (idvc=1)
!     ak           real (km+1) hybrid interface a (idvc=2)
!     bk           real (km+1) hybrid interface b (idvc=2)
!     ps           real (ix) surface pressure (pa)
!   output argument list:
!     pm           real (ix,km) mid-layer pressure (pa)
!     pd           real (ix,km) delta pressure (pa)
!
! attributes:
!   language: fortran
!
!$$$
  implicit none
  integer,intent(in):: im,ix,km,idvc,idsl
  real,intent(in):: si(km+1),ak(km+1),bk(km+1),ps(im)
  real,intent(out):: pm(ix,km),pd(ix,km)
  real,parameter:: rocp=287.05/1004.6,rocp1=rocp+1,rocpr=1/rocp
  real pid,piu
  integer i,k
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  do k=1,km
    do i=1,im
      if(idvc.eq.2) then
        pid=ak(k)+bk(k)*ps(i)
        piu=ak(k+1)+bk(k+1)*ps(i)
      else
        pid=si(k)*ps(i)
        piu=si(k+1)*ps(i)
      endif
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      if(idsl.eq.2) then
        pm(i,k)=(pid+piu)/2
      else
        pm(i,k)=((pid**rocp1-piu**rocp1)/(rocp1*(pid-piu)))**rocpr
      endif
      pd(i,k)=pid-piu
    enddo
  enddo
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
end subroutine
