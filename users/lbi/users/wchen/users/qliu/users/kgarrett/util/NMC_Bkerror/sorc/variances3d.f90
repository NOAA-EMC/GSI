subroutine variances3d(numcases,mype)
  use kinds, only: r_kind,r_single,i_kind
  use postmod, only: smoothlat
  use variables,only: nlat,nlon,nsig,lat1,lon1,filunit1,filunit2,zero,&
      displs_g,ijn,two,db_prec,biasrm,iglobal,ltosi,ltosj,&
      bbiasz,bbiasd,bbiast,bcorrz,bcorrd,bcorrt,bbiasp,bcorrp
  implicit none
  include 'mpif.h'

  integer(i_kind),intent(in):: numcases,mype

  real(r_kind),dimension(lat1,lon1,nsig):: sf1,vp1,t1,rh1,oz1,cw1
  real(r_kind),dimension(lat1,lon1):: ps1
  real(r_kind),dimension(lat1,lon1,nsig):: sf2,vp2,t2,rh2,oz2,cw2
  real(r_kind),dimension(lat1,lon1):: ps2
  real(r_kind),dimension(lat1,lon1,nsig):: sfvar,vpvar,tvar,qvar,ozvar,cwvar
  real(r_kind),dimension(lat1,lon1):: psvar3

! Global Grid
  real(r_kind),dimension(iglobal):: work
  real(r_single),dimension(nlat,nlon,nsig):: sfg,vpg,tg,qg,ozg,cwg
  real(r_single),dimension(nlat,nlon):: psg
! Variables for grads file (re-ordered)
  real(r_single),dimension(nlon,nlat,nsig):: sf4,vp4,t4,q4,oz4,cw4
  real(r_single),dimension(nlon,nlat):: ps4

  integer(i_kind) i,j,k,n,mype_post,ncfggg,mm1,ierror,iret,i1,i2,kk
  integer(i_kind) mpi_rtype
  character(255) grdfile

  if (db_prec) then
    mpi_rtype=mpi_real8
  else
    mpi_rtype=mpi_real4
  end if

  mype_post=0
  mm1=mype+1

! Initialize subdomain variance arrays
  sfvar=zero ; vpvar=zero ; tvar=zero ; qvar=zero ; ozvar=zero ; cwvar=zero ; psvar3=zero

! Each mpi task will carry two files, which contains all variables, for each of the time levels
  open(filunit1,form='unformatted',action='read')
  rewind(filunit1)
  open(filunit2,form='unformatted',action='read')
  rewind(filunit2)
  do n=1,numcases
    if (mype==0)  write(6,*) 'VARIANCES3D, PROCESSING PAIR # ',n
! Read in subdomain grids
    read(filunit1) sf1,vp1,t1,rh1,oz1,cw1,ps1
    read(filunit2) sf2,vp2,t2,rh2,oz2,cw2,ps2

    call delvars(sf1,vp1,t1,rh1,oz1,cw1,ps1,sf2,vp2,t2,rh2,oz2,cw2,ps2,mype)

    do k=1,nsig
      do j=1,lon1
        do i=1,lat1
          sfvar(i,j,k) = sfvar(i,j,k) + (sf1(i,j,k)*sf1(i,j,k))/float(numcases)
          vpvar(i,j,k) = vpvar(i,j,k) + (vp1(i,j,k)*vp1(i,j,k))/float(numcases)
          tvar(i,j,k)  = tvar(i,j,k)  + ( t1(i,j,k)* t1(i,j,k))/float(numcases)
          qvar(i,j,k)  = qvar(i,j,k)  + (rh1(i,j,k)*rh1(i,j,k))/float(numcases)
          ozvar(i,j,k) = ozvar(i,j,k) + (oz1(i,j,k)*oz1(i,j,k))/float(numcases)
          cwvar(i,j,k) = cwvar(i,j,k) + (cw1(i,j,k)*cw1(i,j,k))/float(numcases)
        end do
      end do 
    end do
    do j=1,lon1
      do i=1,lat1
        psvar3(i,j) = psvar3(i,j) + (ps1(i,j)*ps1(i,j))/float(numcases)
      end do
    end do
  end do !End do over number of cases to process
  close(filunit1)
  close(filunit2)

! Create global grid by gathering from subdomains
  do k=1,nsig
    call mpi_gatherv(sfvar(1,1,k),ijn(mm1),mpi_rtype,&
             work,ijn,displs_g,mpi_rtype,&
             mype_post,mpi_comm_world,ierror)
    do kk=1,iglobal
      i1=ltosi(kk); i2=ltosj(kk)
      sfg(i1,i2,k)=work(kk)
    end do
  end do
  do k=1,nsig
    call mpi_gatherv(vpvar(1,1,k),ijn(mm1),mpi_rtype,&
             work,ijn,displs_g,mpi_rtype,&
             mype_post,mpi_comm_world,ierror)
    do kk=1,iglobal
      i1=ltosi(kk); i2=ltosj(kk)
      vpg(i1,i2,k)=work(kk)
    end do
  end do
  do k=1,nsig
    call mpi_gatherv(tvar(1,1,k),ijn(mm1),mpi_rtype,&
             work,ijn,displs_g,mpi_rtype,&
             mype_post,mpi_comm_world,ierror)
    do kk=1,iglobal
      i1=ltosi(kk); i2=ltosj(kk)
      tg(i1,i2,k)=work(kk)
    end do

  end do
  do k=1,nsig
    call mpi_gatherv(qvar(1,1,k),ijn(mm1),mpi_rtype,&
             work,ijn,displs_g,mpi_rtype,&
             mype_post,mpi_comm_world,ierror)
    do kk=1,iglobal
      i1=ltosi(kk); i2=ltosj(kk)
      qg(i1,i2,k)=work(kk)
    end do
  end do
  do k=1,nsig
    call mpi_gatherv(ozvar(1,1,k),ijn(mm1),mpi_rtype,&
             work,ijn,displs_g,mpi_rtype,&
             mype_post,mpi_comm_world,ierror)
    do kk=1,iglobal
      i1=ltosi(kk); i2=ltosj(kk)
      ozg(i1,i2,k)=work(kk)
    end do
  end do
  do k=1,nsig
    call mpi_gatherv(cwvar(1,1,k),ijn(mm1),mpi_rtype,&
             work,ijn,displs_g,mpi_rtype,&
             mype_post,mpi_comm_world,ierror)
    do kk=1,iglobal
      i1=ltosi(kk); i2=ltosj(kk)
      cwg(i1,i2,k)=work(kk)
    end do
  end do

  call mpi_gatherv(psvar3,ijn(mm1),mpi_rtype,&
           work,ijn,displs_g,mpi_rtype,&
           mype_post,mpi_comm_world,ierror)
  if (mype==mype_post) then
    do kk=1,iglobal
      i1=ltosi(kk); i2=ltosj(kk)
      psg(i1,i2)=work(kk)
    end do
  end if

  if (mype==mype_post) then
    do k=1,nsig
      do j=1,nlon
        do i=1,nlat
          sf4(j,i,k)=sqrt(sfg(i,j,k))
          vp4(j,i,k)=sqrt(vpg(i,j,k))
          t4(j,i,k)=sqrt(tg(i,j,k))
          q4(j,i,k)=sqrt(qg(i,j,k))
          oz4(j,i,k)=sqrt(ozg(i,j,k))
          cw4(j,i,k)=sqrt(cwg(i,j,k))
        end do
      end do
    end do
    do j=1,nlon
      do i=1,nlat
        ps4(j,i)=sqrt(psg(i,j))
      end do
    end do

    grdfile='bgstats3d.grd'
    ncfggg=len_trim(grdfile)
    call baopenwt(22,grdfile(1:ncfggg),iret)
    call wryte(22,4*nlat*nlon*nsig,sf4)
    call wryte(22,4*nlat*nlon*nsig,vp4)
    call wryte(22,4*nlat*nlon*nsig,t4)
    call wryte(22,4*nlat*nlon*nsig,q4)
    call wryte(22,4*nlat*nlon*nsig,oz4)
    call wryte(22,4*nlat*nlon*nsig,cw4)
    call wryte(22,4*nlat*nlon,ps4)
    call baclose(22,iret)

  end if

  return
end subroutine variances3d


