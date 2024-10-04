subroutine initvars(mype,npe)
  use kinds, only: r_kind,r_double
  use variables, only: nlon,rlats,wgtlats,ak5,bk5,ck5,&
      deg2rad,rlons,nsig,&
      dimbig,filename,nlat,sweight,&
      na,nb,pi,db_prec,coriolis, &
      two,omega,idpsfc5,idvm5,idvc5,idthrm5,&
      scaling,varscale
  use specgrid, only: wlat,slat,jb,je
  implicit none

  integer,intent(in):: mype,npe
  integer i,ii,l,m,i1,k
  real(r_kind) anlon,dlon,pih
  real(r_kind) onetest
  real(r_double) onedouble

  allocate(filename(dimbig))
  allocate(na(dimbig),nb(dimbig))

  allocate(rlats(nlat),rlons(nlon),wgtlats(nlat))
  allocate(ak5(nsig+1),bk5(nsig+1),ck5(nsig+1))

! constant for deg/radians conversion
  deg2rad=acos(-1.0)/180.0

! Set local constants
  anlon=float(nlon)
  pih=0.5*pi
  dlon=4*pih/anlon

! Init grid stuff to defaults
  idpsfc5=1
  idvm5=1
  idvc5=1
  idthrm5=1

! Load grid lat,lon arrays.  rbs2 is used in pcp.
  do i=1,nlon
    rlons(i)=float(i-1)*dlon
  end do

  do i=jb,je
    i1=i+1
    rlats(i1)=-asin(slat(i))
    wgtlats(i1)=wlat(i)

    i1=nlat-i
    rlats(i1)=asin(slat(i))
    wgtlats(i1)=wlat(i)
  end do

  rlats(1)=-pih
  rlats(nlat)=pih

  wgtlats(1)=0.0_r_kind
  wgtlats(nlat)=0.0_r_kind

  do i=1,nlat
    coriolis(i)=two*omega*sin(rlats(i))
  end do

! test for precision at which code was compiled
  onetest=1.; onedouble=1.
  if(digits(onetest).lt.digits(onedouble)) then
    db_prec=.false.
  else
    db_prec=.true.
  endif
  if (mype==0) write(6,*) 'INITVARS: DB_PREC = ',db_prec

  if (scaling .eqv. .true.) then
     allocate (varscale(nsig))
     open(12,file='scaling.txt',form='formatted')
     do k=1,nsig
        read(12,'(F4.2)') varscale(k)
        print*,varscale(k)
     enddo
  endif

  return
end subroutine initvars

