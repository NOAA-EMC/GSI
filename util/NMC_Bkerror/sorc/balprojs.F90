subroutine balprojs(numcases,mype)
  use kinds, only: r_kind,r_single,i_kind
  use variables,only: nlat,nlon,nsig,lat1,lon1,filunit1,filunit2,zero,&
      displs_g,ijn,two,db_prec,tcon,vpcon,pscon,npe,iglobal,ltosi,ltosj,&
      biasrm,bbiasz,bbiasd,bbiast,bcorrz,bcorrd,bcorrt,bbiasp,bcorrp,&
      smoothdeg,vertavg,half
  use postmod, only: smoothlat
  implicit none
  include 'mpif.h'


  integer(i_kind),intent(in):: numcases,mype

  real(r_kind),dimension(lat1,lon1,nsig):: sf1,vp1,t1,rh1,oz1,cw1
  real(r_kind),dimension(lat1,lon1):: ps1
  real(r_kind),dimension(lat1,lon1,nsig):: sf2,vp2,t2,rh2,oz2,cw2
  real(r_kind),dimension(lat1,lon1):: ps2
  real(r_kind),dimension(lat1,lon1,nsig):: sf3,vp3,t3

  real(r_kind),dimension(lat1,lon1,nsig,nsig):: zzcor3,ztcor3
  real(r_kind),dimension(lat1,lon1,nsig):: zdcor2,zzcor2
  real(r_kind),dimension(lat1,lon1):: zpcor2
  real(r_kind),dimension(nlat,nsig,nsig):: zz3_av,zt3_av
  real(r_kind),dimension(nlat,nsig):: zd2_av,zz2_av
  real(r_kind),dimension(nlat):: zp2_av
  real(r_kind),dimension(iglobal):: work1
  real(r_kind),dimension(nlat,nlon):: workgrd

! variables for ESSL routines
  real(r_kind),dimension(100*nsig):: auxwrk
  real(r_kind),dimension(4*nsig):: auxsv
  real(r_kind),dimension(nsig):: sval
  real(r_kind),dimension(nsig,nsig):: sigmtx,matred,bmtx,matrix
  
  real(r_kind) r025
  integer(i_kind) i,j,k,m,n,mype_post,ncfggg,mm1,ierror,iret
  integer(i_kind) mpi_rtype,mype_work,kk,ni1,ni2,info

  if (db_prec) then
    mpi_rtype=mpi_real8
  else
    mpi_rtype=mpi_real4
  end if

  mype_work=npe/2
  mm1=mype+1
  zz3_av=zero ; zt3_av=zero ; zd2_av=zero ; zz2_av=zero ; zp2_av=zero
  zzcor3=zero ; ztcor3=zero ; zzcor2=zero ; zdcor2=zero ; zpcor2=zero

  r025=0.25_r_kind

  open(filunit1,form='unformatted',action='read')
  rewind(filunit1)
  open(filunit2,form='unformatted',action='read')
  rewind(filunit2)
  do n=1,numcases
    if (mype==0)  write(6,*) 'BALPROJS, PROCESSING PAIR # ',n
! Read in subdomain grids
    read(filunit1) sf1,vp1,t1,rh1,oz1,cw1,ps1
    read(filunit2) sf2,vp2,t2,rh2,oz2,cw2,ps2

    if(biasrm) then
      do k=1,nsig
        do j=1,lon1
          do i=1,lat1
            sf1(i,j,k) = sf1(i,j,k)-bcorrz(i,j,k)*sf2(i,j,k)-bbiasz(i,j,k)
            vp1(i,j,k) = vp1(i,j,k)-bcorrd(i,j,k)*vp2(i,j,k)-bbiasd(i,j,k)
            t1(i,j,k)  = t1(i,j,k) -bcorrt(i,j,k)* t2(i,j,k)-bbiast(i,j,k)
          end do
        end do
      end do
      do j=1,lon1
        do i=1,lat1
          ps1(i,j) = ps1(i,j)-bcorrp(i,j)*ps2(i,j)-bbiasp(i,j)
        end do
      end do
    else
      do k=1,nsig
        do j=1,lon1
          do i=1,lat1
            sf1(i,j,k) = sf1(i,j,k)-sf2(i,j,k)
            vp1(i,j,k) = vp1(i,j,k)-vp2(i,j,k)
            t1(i,j,k)  =  t1(i,j,k)- t2(i,j,k)
          end do
        end do
      end do
      do j=1,lon1
        do i=1,lat1
          ps1(i,j) = ps1(i,j)-ps2(i,j)
        end do
      end do
    end if

    if (vertavg) then
      do k=2,nsig-1
        do j=1,lon1
          do i=1,lat1
            sf3(i,j,k) = half*sf1(i,j,k)+r025*(sf1(i,j,k+1)+sf1(i,j,k-1))
            vp3(i,j,k) = half*vp1(i,j,k)+r025*(vp1(i,j,k+1)+vp1(i,j,k-1))
             t3(i,j,k) = half* t1(i,j,k)+r025*( t1(i,j,k+1)+ t1(i,j,k-1))
          end do
        end do
      end do
! for k=1 and k=nsig now
      do j=1,lon1
        do i=1,lat1
          sf3(i,j,1) =half*sf1(i,j,1)+half*sf1(i,j,2)
          vp3(i,j,1) =half*vp1(i,j,1)+half*vp1(i,j,2)
           t3(i,j,1) =half* t1(i,j,1)+half* t1(i,j,2)
          sf3(i,j,nsig) =half*sf1(i,j,nsig)+half*sf1(i,j,nsig-1)
          vp3(i,j,nsig) =half*vp1(i,j,nsig)+half*vp1(i,j,nsig-1)
           t3(i,j,nsig) =half* t1(i,j,nsig)+half* t1(i,j,nsig-1)
        end do
      end do
      do k=1,nsig
        do j=1,lon1
          do i=1,lat1
            sf1(i,j,k)=sf3(i,j,k)
            vp1(i,j,k)=vp3(i,j,k)
             t1(i,j,k)= t3(i,j,k)
          end do
        end do
      end do
    end if


    do m=1,nsig
      do k=1,nsig
        do j=1,lon1
          do i=1,lat1
! streamfunction-streamfunction
            zzcor3(i,j,k,m)=zzcor3(i,j,k,m)+sf1(i,j,k)*sf1(i,j,m)
! streamfunction-temparature
            ztcor3(i,j,k,m)=ztcor3(i,j,k,m)+sf1(i,j,k)*t1(i,j,m)
          end do
        end do
      end do
    end do

! correlation matrices for velocity potential balance constraint
    do k=1,nsig
      do j=1,lon1
        do i=1,lat1
! streamfunction-velocity potential
          zdcor2(i,j,k)=zdcor2(i,j,k)+sf1(i,j,k)*vp1(i,j,k)
! streamfunction-streamfunction
          zzcor2(i,j,k)=zzcor2(i,j,k)+sf1(i,j,k)*sf1(i,j,k)
        end do
      end do
    end do
    do j=1,lon1
      do i=1,lat1
! streamfunction-surface pressure
        zpcor2(i,j)=zpcor2(i,j)+sf1(i,j,1)*ps1(i,j)
      end do
    end do
  end do ! end do n cases

  zzcor3=zzcor3/float(numcases)
  ztcor3=ztcor3/float(numcases)
  zzcor2=zzcor2/float(numcases)
  zdcor2=zdcor2/float(numcases)
  zpcor2=zpcor2/float(numcases)

! Need to convert full subdomain corrleation matrices into arrays
! That contain zonal mean
  do n=1,nsig
    do k=1,nsig
      call mpi_gatherv(zzcor3(1,1,k,n),ijn(mm1),mpi_rtype,&
           work1,ijn,displs_g,mpi_rtype,&
           mype_work,mpi_comm_world,ierror)
      if (mype==mype_work) then
        do kk=1,iglobal
          ni1=ltosi(kk) ; ni2=ltosj(kk)
          workgrd(ni1,ni2) = work1(kk)
        end do
        do i=1,nlat
          do j=1,nlon
            zz3_av(i,k,n) = zz3_av(i,k,n) + workgrd(i,j)/float(nlon)
          end do
        end do
      end if
    end do
  end do

  do n=1,nsig
    do k=1,nsig
      call mpi_gatherv(ztcor3(1,1,k,n),ijn(mm1),mpi_rtype,&
           work1,ijn,displs_g,mpi_rtype,&
           mype_work,mpi_comm_world,ierror)
      if (mype==mype_work) then
        do kk=1,iglobal
          ni1=ltosi(kk) ; ni2=ltosj(kk)
          workgrd(ni1,ni2) = work1(kk)
        end do
        do i=1,nlat
          do j=1,nlon
            zt3_av(i,k,n) = zt3_av(i,k,n) + workgrd(i,j)/float(nlon)
          end do
        end do
      end if
    end do
  end do

  do k=1,nsig
    call mpi_gatherv(zzcor2(1,1,k),ijn(mm1),mpi_rtype,&
           work1,ijn,displs_g,mpi_rtype,&
           mype_work,mpi_comm_world,ierror)
    if (mype==mype_work) then
      do kk=1,iglobal
        ni1=ltosi(kk) ; ni2=ltosj(kk)
        workgrd(ni1,ni2) = work1(kk)
      end do
      do i=1,nlat
        do j=1,nlon
          zz2_av(i,k) = zz2_av(i,k) + workgrd(i,j)/float(nlon)
        end do
      end do
    end if
  end do

  do k=1,nsig
    call mpi_gatherv(zdcor2(1,1,k),ijn(mm1),mpi_rtype,&
           work1,ijn,displs_g,mpi_rtype,&
           mype_work,mpi_comm_world,ierror)
    if (mype==mype_work) then
      do kk=1,iglobal
        ni1=ltosi(kk) ; ni2=ltosj(kk)
        workgrd(ni1,ni2) = work1(kk)
      end do
      do i=1,nlat
        do j=1,nlon
          zd2_av(i,k) = zd2_av(i,k) + workgrd(i,j)/float(nlon)
        end do
      end do
    end if
  end do

  call mpi_gatherv(zpcor2(1,1),ijn(mm1),mpi_rtype,&
         work1,ijn,displs_g,mpi_rtype,&
         mype_work,mpi_comm_world,ierror)
  if (mype==mype_work) then
    do kk=1,iglobal
      ni1=ltosi(kk) ; ni2=ltosj(kk)
      workgrd(ni1,ni2) = work1(kk)
    end do
    do i=1,nlat
      do j=1,nlon
        zp2_av(i) = zp2_av(i) + workgrd(i,j)/float(nlon)
      end do
    end do
  end if

! Smooth correlation matrices in latitudinal direction
  call smoothlat(zz3_av,nsig*nsig,smoothdeg)
  call smoothlat(zt3_av,nsig*nsig,smoothdeg)
  call smoothlat(zd2_av,nsig,smoothdeg)
  call smoothlat(zz2_av,nsig,smoothdeg)
  call smoothlat(zp2_av,1,smoothdeg)


! Have matrices loaded with zonal means only on task=mype_work 
  if (mype==mype_work) then
! invert 3d streamfunction-streamfunction correlation matrix for computing
! temperature balance projections
    matrix=zero
    do i=1,nlat
      do m=1,nsig
        do k=1,nsig
          matrix(k,m)=zz3_av(i,k,m)
        end do
      end do
! BMTX = U(transpose)
      bmtx=zero
      do j=1,nsig
        bmtx(j,j)=1.0_r_kind
      end do
      sval=zero
! get singular values
#ifdef _LAPACK_
    if (db_prec) then
      call dgesvd('S','S',nsig,nsig,matrix,nsig,sval,bmtx,nsig, &
                  matred,nsig,work1,5*nsig,info)
    else
      call sgesvd('S','S',nsig,nsig,matrix,nsig,sval,bmtx,nsig, &
                  matred,nsig,work1,5*nsig,info)
    end if
    if(info.ne.0)then
       write(6,*)'something is wrong in SVD'; stop 30
    endif
#else
      if (db_prec) then
        call dgesvf(12,matrix,nsig,bmtx,nsig,nsig,sval,nsig,nsig,&
                  auxsv,4*nsig)
      else
        call sgesvf(12,matrix,nsig,bmtx,nsig,nsig,sval,nsig,nsig,&
                    auxsv,4*nsig)
      end if
#endif

! keep 20 leading singular values
      sigmtx=zero
      do j=1,20
        sigmtx(j,j)=1./sval(j)
      end do

! perform matrix multiplication sval*V(transpose)
#ifdef _LAPACK_
    matred = 0.
    do j = 1,nsig
      do k = 1,nsig
         matred(k,j) = sigmtx(k,k)*bmtx(j,k)
      enddo
    enddo
#else
      if (db_prec) then
        call dgemul(sigmtx,nsig,'N',matrix,nsig,'T',matred,nsig,nsig,nsig,nsig)
      else
        call sgemul(sigmtx,nsig,'N',matrix,nsig,'T',matred,nsig,nsig,nsig,nsig)
      end if
#endif

      matrix=zero

#ifdef _LAPACK_
    if (db_prec) then
      call dgemm ('N','N',nsig,nsig,nsig,1.0,bmtx,nsig,matred,nsig,0.0,matrix,nsig)
    else
      call sgemm ('N','N',nsig,nsig,nsig,1.0,bmtx,nsig,matred,nsig,0.0,matrix,nsig)
    end if
#else
      if (db_prec) then
        call dgemul(bmtx,nsig,'T',matred,nsig,'N',matrix,nsig,nsig,nsig,nsig)
      else
        call sgemul(bmtx,nsig,'T',matred,nsig,'N',matrix,nsig,nsig,nsig,nsig)
      end if
#endif

! load back into original 3d array
      do m=1,nsig
        do k=1,nsig
          zz3_av(i,k,m)=matrix(k,m)
        end do
      end do
    end do ! enddo lat

    tcon=zero
    do n=1,nsig
      do m=1,nsig
        do i=1,nlat
          do k=1,nsig
            tcon(i,n,m)=tcon(i,n,m)+zz3_av(i,m,k)*zt3_av(i,k,n)
          end do
        end do
      end do
! fill 'pole points'
      do k=1,nsig
        tcon(1,n,k)=tcon(2,n,k)
        tcon(nlat,n,k)=tcon(nlat-1,n,k)
      end do
    end do !end do n

! velocity potential constraint
    vpcon=zero
    do k=1,nsig
      do i=1,nlat
        vpcon(i,k)=vpcon(i,k)+zd2_av(i,k)/zz2_av(i,k)
      end do
! fill 'pole points'
      vpcon(1,k)=vpcon(2,k)
      vpcon(nlat,k)=vpcon(nlat-1,k)
    end do

    pscon=zero
    do i=1,nlat
      pscon(i,1)=pscon(i,1)+zp2_av(i)/zz2_av(i,1)
    end do
    pscon(1,1)=pscon(2,1)
    pscon(nlat,1)=pscon(nlat-1,1)

  end if  !END IF mype

! Broadcast to other tasks 
  call mpi_bcast(tcon,nlat*nsig*nsig,mpi_rtype,mype_work,mpi_comm_world,ierror)
  call mpi_bcast(vpcon,nlat*nsig,mpi_rtype,mype_work,mpi_comm_world,ierror)
  call mpi_bcast(pscon,nlat*nsig,mpi_rtype,mype_work,mpi_comm_world,ierror)

  return
end subroutine balprojs