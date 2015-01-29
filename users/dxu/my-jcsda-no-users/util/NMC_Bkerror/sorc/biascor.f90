subroutine biascor(numcases,mype)
  use kinds, only: r_kind,r_single,i_kind,r_double
  use variables,only: nlat,nlon,nsig,lat1,lon1,filunit1,filunit2,zero,&
      displs_g,ijn,two,db_prec,one,&
      bbiasz,bbiasd,bbiast,bcorrz,bcorrd,bcorrt,bbiasp,bcorrp,&
      istart,ilat1,jstart,iglobal,ltosi,ltosj
  implicit none
  include 'mpif.h'

  integer(i_kind),intent(in):: numcases,mype

  real(r_kind),dimension(lat1,lon1,nsig):: sf1,vp1,t1,rh1,oz1,cw1
  real(r_kind),dimension(lat1,lon1):: ps1
  real(r_kind),dimension(lat1,lon1,nsig):: sf2,vp2,t2,rh2,oz2,cw2
  real(r_kind),dimension(lat1,lon1):: ps2

  real(r_double),dimension(lat1,lon1,nsig,4):: bfactz,bfactd,bfactt
  real(r_double),dimension(lat1,lon1,4):: bfactp
  real(r_kind),dimension(iglobal):: work1
  real(r_kind),dimension(nlat,nlon):: workgrd

  real(r_kind),dimension(nlat,nlon,nsig):: bcz,bcd,bct,bbz,bbd,bbt
  real(r_kind),dimension(nlat,nlon):: bbp,bcp
  real(r_single),dimension(nlon,nlat,nsig):: bcz4,bcd4,bct4,bbz4,bbd4,bbt4
  real(r_single),dimension(nlon,nlat):: bcp4,bbp4
  character(255) grdfile
  integer ncfggg,ifile

  integer(i_kind) mpi_rtype,i,j,k,n,ni1,ni2,iret,kk,mype_work,mm1,ierror

! Allocate bias arrays if performing bias correction
  allocate( bbiasz(lat1,lon1,nsig),bbiasd(lat1,lon1,nsig),bbiast(lat1,lon1,nsig) )
  allocate( bcorrz(lat1,lon1,nsig),bcorrd(lat1,lon1,nsig),bcorrt(lat1,lon1,nsig) )
  allocate( bbiasp(lat1,lon1),bcorrp(lat1,lon1) )

  if (db_prec) then
    mpi_rtype=mpi_real8
  else
    mpi_rtype=mpi_real4
  end if

  mype_work=0
  mm1=mype+1

! Initialize arrays
  bcorrp=one ; bcorrt=one ; bcorrd=one ; bcorrz=one
  bbiasp=zero ; bbiast=zero ; bbiasd=zero ; bbiasz=zero
  bfactp=0._r_double ; bfactz=0._r_double ; bfactd=0._r_double ; bfactt=0._r_double
  
! BIAS CORR ARRAYS
  open(filunit1,form='unformatted',action='read')
  rewind(filunit1)
  open(filunit2,form='unformatted',action='read')
  rewind(filunit2)

  do n=1,numcases
    if (mype==0)  write(6,*) 'BIASCOR, PROCESSING PAIR # ',n
! Read in subdomain grids
    read(filunit1) sf1,vp1,t1,rh1,oz1,cw1,ps1
    read(filunit2) sf2,vp2,t2,rh2,oz2,cw2,ps2

    do k=1,nsig
      do j=1,lon1
        do i=1,lat1
          bfactz(i,j,k,1)=bfactz(i,j,k,1)+(sf1(i,j,k)*sf2(i,j,k))
          bfactz(i,j,k,2)=bfactz(i,j,k,2)+(sf2(i,j,k)*sf2(i,j,k))
          bfactz(i,j,k,3)=bfactz(i,j,k,3)+sf1(i,j,k)
          bfactz(i,j,k,4)=bfactz(i,j,k,4)+sf2(i,j,k)
       
          bfactd(i,j,k,1)=bfactd(i,j,k,1)+(vp1(i,j,k)*vp2(i,j,k))
          bfactd(i,j,k,2)=bfactd(i,j,k,2)+(vp2(i,j,k)*vp2(i,j,k))
          bfactd(i,j,k,3)=bfactd(i,j,k,3)+vp1(i,j,k)
          bfactd(i,j,k,4)=bfactd(i,j,k,4)+vp2(i,j,k)

          bfactt(i,j,k,1)=bfactt(i,j,k,1)+(t1(i,j,k)*t2(i,j,k))
          bfactt(i,j,k,2)=bfactt(i,j,k,2)+(t2(i,j,k)*t2(i,j,k))
          bfactt(i,j,k,3)=bfactt(i,j,k,3)+t1(i,j,k)
          bfactt(i,j,k,4)=bfactt(i,j,k,4)+t2(i,j,k)
        end do
      end do
    end do

    do j=1,lon1
      do i=1,lat1
        bfactp(i,j,1)=bfactp(i,j,1)+(ps1(i,j)*ps2(i,j))
        bfactp(i,j,2)=bfactp(i,j,2)+(ps2(i,j)*ps2(i,j))
        bfactp(i,j,3)=bfactp(i,j,3)+ps1(i,j)
        bfactp(i,j,4)=bfactp(i,j,4)+ps2(i,j)
      end do
    end do

  end do ! END DO NUMCASES
  close(filunit1)
  close(filunit2)

  do k=1,nsig
    do j=1,lon1
      do i=1,lat1
        do n=1,4
          bfactz(i,j,k,n) = bfactz(i,j,k,n)/float(numcases)
          bfactd(i,j,k,n) = bfactd(i,j,k,n)/float(numcases)
          bfactt(i,j,k,n) = bfactt(i,j,k,n)/float(numcases)
        end do

        if(abs(bfactz(i,j,k,2)-bfactz(i,j,k,4)**2) > 1.e-26)then
          bcorrz(i,j,k)=(bfactz(i,j,k,1)-bfactz(i,j,k,3)*bfactz(i,j,k,4)) &
                        /(bfactz(i,j,k,2)-bfactz(i,j,k,4)**2.)
        end if
        bbiasz(i,j,k)=bfactz(i,j,k,3)-bcorrz(i,j,k)*bfactz(i,j,k,4)

        if(abs(bfactd(i,j,k,2)-bfactd(i,j,k,4)**2) > 1.e-26)then
          bcorrd(i,j,k)=(bfactd(i,j,k,1)-bfactd(i,j,k,3)*bfactd(i,j,k,4)) &
                        /(bfactd(i,j,k,2)-bfactd(i,j,k,4)**2.)
        end if
        bbiasd(i,j,k)=bfactd(i,j,k,3)-bcorrd(i,j,k)*bfactd(i,j,k,4)

        if(abs(bfactt(i,j,k,2)-bfactt(i,j,k,4)**2) > 1.e-26)then
          bcorrt(i,j,k)=(bfactt(i,j,k,1)-bfactt(i,j,k,3)*bfactt(i,j,k,4)) &
                        /(bfactt(i,j,k,2)-bfactt(i,j,k,4)**2.)
        end if
        bbiast(i,j,k)=bfactt(i,j,k,3)-bcorrt(i,j,k)*bfactt(i,j,k,4)
      end do
    end do
  end do


  do j=1,lon1
    do i=1,lat1
      do n=1,4
        bfactp(i,j,n) = bfactp(i,j,n)/float(numcases)
      end do

      if(abs(bfactp(i,j,2)-bfactp(i,j,4)**2) > 1.e-26)then
        bcorrp(i,j)=(bfactp(i,j,1)-bfactp(i,j,3)*bfactp(i,j,4)) &
                      /(bfactp(i,j,2)-bfactp(i,j,4)**2.)
      end if
      bbiasp(i,j)=bfactp(i,j,3)-bcorrp(i,j)*bfactp(i,j,4)
    end do
  end do


  do k=1,nsig
    call mpi_gatherv(bcorrz(1,1,k),ijn(mm1),mpi_rtype,&
           work1,ijn,displs_g,mpi_rtype,&
           mype_work,mpi_comm_world,ierror)
    if (mype==mype_work) then
      do kk=1,iglobal
        ni1=ltosi(kk); ni2=ltosj(kk)
        bcz(ni1,ni2,k)=work1(kk)
      end do
    end if
  end do

  do k=1,nsig
    call mpi_gatherv(bbiasz(1,1,k),ijn(mm1),mpi_rtype,&
           work1,ijn,displs_g,mpi_rtype,&
           mype_work,mpi_comm_world,ierror)
    if (mype==mype_work) then
      do kk=1,iglobal
        ni1=ltosi(kk); ni2=ltosj(kk)
        bbz(ni1,ni2,k)=work1(kk)
      end do
    end if
  end do

  do k=1,nsig
    call mpi_gatherv(bcorrd(1,1,k),ijn(mm1),mpi_rtype,&
           work1,ijn,displs_g,mpi_rtype,&
           mype_work,mpi_comm_world,ierror)
    if (mype==mype_work) then
      do kk=1,iglobal
        ni1=ltosi(kk); ni2=ltosj(kk)
        bcd(ni1,ni2,k)=work1(kk)
      end do
    end if
  end do

  do k=1,nsig
    call mpi_gatherv(bbiasd(1,1,k),ijn(mm1),mpi_rtype,&
           work1,ijn,displs_g,mpi_rtype,&
           mype_work,mpi_comm_world,ierror)
    if (mype==mype_work) then
      do kk=1,iglobal
        ni1=ltosi(kk); ni2=ltosj(kk)
        bbd(ni1,ni2,k)=work1(kk)
      end do
    end if
  end do

  do k=1,nsig
    call mpi_gatherv(bcorrt(1,1,k),ijn(mm1),mpi_rtype,&
           work1,ijn,displs_g,mpi_rtype,&
           mype_work,mpi_comm_world,ierror)
    if (mype==mype_work) then
      do kk=1,iglobal
        ni1=ltosi(kk); ni2=ltosj(kk)
        bct(ni1,ni2,k)=work1(kk)
      end do
    end if
  end do

  do k=1,nsig
    call mpi_gatherv(bbiast(1,1,k),ijn(mm1),mpi_rtype,&
           work1,ijn,displs_g,mpi_rtype,&
           mype_work,mpi_comm_world,ierror)
    if (mype==mype_work) then
      do kk=1,iglobal
        ni1=ltosi(kk); ni2=ltosj(kk)
        bbt(ni1,ni2,k)=work1(kk)
      end do
    end if
  end do

  call mpi_gatherv(bbiasp,ijn(mm1),mpi_rtype,&
       work1,ijn,displs_g,mpi_rtype,&
       mype_work,mpi_comm_world,ierror)
  if (mype==mype_work) then
     do kk=1,iglobal
        ni1=ltosi(kk); ni2=ltosj(kk)
        bbp(ni1,ni2)=work1(kk)
     end do
  end if

  call mpi_gatherv(bcorrp,ijn(mm1),mpi_rtype,&
       work1,ijn,displs_g,mpi_rtype,&
       mype_work,mpi_comm_world,ierror)
  if (mype==mype_work) then
     do kk=1,iglobal
        ni1=ltosi(kk); ni2=ltosj(kk)
        bcp(ni1,ni2)=work1(kk)
     end do
  end if

  if (mype==mype_work) then
      write(6,*) 'REVERSE LAT/LON'
      do k=1,nsig
        do j=1,nlon
          do i=1,nlat
            bcz4(j,i,k) = bcz(i,j,k)
            bcd4(j,i,k) = bcd(i,j,k)
            bct4(j,i,k) = bct(i,j,k)
            bbz4(j,i,k) = bbz(i,j,k)
            bbd4(j,i,k) = bbd(i,j,k)
            bbt4(j,i,k) = bbt(i,j,k)
          end do
        end do
      end do

      do j=1,nlon
        do i=1,nlat
          bcp4(j,i) = bcp(i,j)
          bbp4(j,i) = bbp(i,j)
        end do
      end do

      write(6,*) 'WRITE OUT GRID'

      grdfile='biascor.grd'
      ncfggg=len_trim(grdfile)
      call baopenwt(22,grdfile(1:ncfggg),iret)
      call wryte(22,4*nlat*nlon*nsig,bcz4)
      call wryte(22,4*nlat*nlon*nsig,bbz4)
      call wryte(22,4*nlat*nlon*nsig,bcd4)
      call wryte(22,4*nlat*nlon*nsig,bbd4)
      call wryte(22,4*nlat*nlon*nsig,bct4)
      call wryte(22,4*nlat*nlon*nsig,bbt4)
      call wryte(22,4*nlat*nlon,bcp4)
      call wryte(22,4*nlat*nlon,bbp4)

    end if

    call mpi_barrier(mpi_comm_world,ierror)

  return
end subroutine biascor
