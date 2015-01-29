subroutine vertsc(numcases,mype)
  use kinds, only: r_kind,r_single,i_kind,r_double
  use postmod, only: smoothlat
  use variables,only: nlat,nlon,nsig,lat1,lon1,zero,&
      displs_g,ijn,db_prec,filunit1,filunit2,npe,&
      sfvln,vpvln,tvln,qvln,ozvln,cvln,&
      iglobal,ltosi,ltosj,smoothdeg,one
  implicit none
  include 'mpif.h'

  integer(i_kind),intent(in):: numcases,mype

  real(r_kind),dimension(lat1,lon1,nsig):: sf1,vp1,t1,rh1,oz1,cw1
  real(r_kind),dimension(lat1,lon1):: ps1
  real(r_kind),dimension(lat1,lon1,nsig):: sf2,vp2,t2,rh2,oz2,cw2
  real(r_kind),dimension(lat1,lon1):: ps2

  real(r_kind),dimension(lat1,lon1,nsig,nsig):: sf4,vp4,t4,rh4,oz4,cw4
  real(r_kind),dimension(iglobal):: work1
  real(r_double),dimension(nlat,nlon):: workgrd
  real(r_kind),dimension(nlat,nsig*6):: vsc_out
  real(r_kind) r_norm

  real(r_double),dimension(nlat,nsig,nsig):: sfvc,vpvc,tvc,rhvc,ozvc,cwvc
  real(r_double),dimension(nsig,6):: diag
  real(r_double) small

  integer i,j,k,m,n,mpi_rtype,mm1,mype_work,ierror
  integer kk,ni1,ni2

  if (db_prec) then
    mpi_rtype=mpi_real8
  else
    mpi_rtype=mpi_real4
  end if

  mype_work=0
  mm1=mype+1
  r_norm=one/float(numcases)

  small=1.e-8_r_double

  sf4=zero ; vp4=zero ; t4=zero ; rh4=zero
  oz4=zero ; cw4=zero
  sfvc=0._r_double ; vpvc=0._r_double ; tvc=0._r_double ; rhvc=0._r_double
  ozvc=0._r_double ; cwvc=0._r_double

  open(filunit1,form='unformatted',action='read')
  rewind(filunit1)
  open(filunit2,form='unformatted',action='read')
  rewind(filunit2)

  do n=1,numcases
    if (mype==0)  write(6,*) 'VERTSC, PROCESSING PAIR # ',n
! Read in subdomain grids
    read(filunit1) sf1,vp1,t1,rh1,oz1,cw1,ps1
    read(filunit2) sf2,vp2,t2,rh2,oz2,cw2,ps2

    call delvars(sf1,vp1,t1,rh1,oz1,cw1,ps1,sf2,vp2,t2,rh2,oz2,cw2,ps2,mype)

    do m=1,nsig
      do k=1,nsig
        do j=1,lon1
          do i=1,lat1
            sf4(i,j,k,m) = sf4(i,j,k,m) + sf1(i,j,k)*sf1(i,j,m)
            vp4(i,j,k,m) = vp4(i,j,k,m) + vp1(i,j,k)*vp1(i,j,m)
             t4(i,j,k,m) =  t4(i,j,k,m) +  t1(i,j,k)* t1(i,j,m)
            rh4(i,j,k,m) = rh4(i,j,k,m) + rh1(i,j,k)*rh1(i,j,m)
            oz4(i,j,k,m) = oz4(i,j,k,m) + oz1(i,j,k)*oz1(i,j,m)
            cw4(i,j,k,m) = cw4(i,j,k,m) + cw1(i,j,k)*cw1(i,j,m)
          end do
        end do
      end do
    end do
  end do ! end do numcases
  close(filunit1)
  close(filunit2)

  sf4=sf4*r_norm
  vp4=vp4*r_norm
   t4= t4*r_norm
  rh4=rh4*r_norm
  oz4=oz4*r_norm
  cw4=cw4*r_norm

! Need to convert full subdomain corrleation matrices into arrays
! That contain zonal mean
  do n=1,nsig
    do k=1,nsig
      call mpi_gatherv(sf4(1,1,k,n),ijn(mm1),mpi_rtype,&
           work1,ijn,displs_g,mpi_rtype,&
           mype_work,mpi_comm_world,ierror)
      if (mype==mype_work) then
        do kk=1,iglobal
          ni1=ltosi(kk); ni2=ltosj(kk)
          workgrd(ni1,ni2)=work1(kk)
        end do
        do i=1,nlat
          do j=1,nlon
            sfvc(i,k,n) = sfvc(i,k,n) + workgrd(i,j)/float(nlon)
          end do
        end do
      end if
    end do
  end do

  do n=1,nsig
    do k=1,nsig
      call mpi_gatherv(vp4(1,1,k,n),ijn(mm1),mpi_rtype,&
           work1,ijn,displs_g,mpi_rtype,&
           mype_work,mpi_comm_world,ierror)
      if (mype==mype_work) then
        do kk=1,iglobal
          ni1=ltosi(kk); ni2=ltosj(kk)
          workgrd(ni1,ni2)=work1(kk)
        end do
        do i=1,nlat
          do j=1,nlon
            vpvc(i,k,n) = vpvc(i,k,n) + workgrd(i,j)/float(nlon)
          end do
        end do
      end if
    end do
  end do

  do n=1,nsig
    do k=1,nsig
      call mpi_gatherv(t4(1,1,k,n),ijn(mm1),mpi_rtype,&
           work1,ijn,displs_g,mpi_rtype,&
           mype_work,mpi_comm_world,ierror)
      if (mype==mype_work) then
        do kk=1,iglobal
          ni1=ltosi(kk); ni2=ltosj(kk)
          workgrd(ni1,ni2)=work1(kk)
        end do
        do i=1,nlat
          do j=1,nlon
            tvc(i,k,n) = tvc(i,k,n) + workgrd(i,j)/float(nlon)
          end do
        end do
      end if
    end do
  end do

  do n=1,nsig
    do k=1,nsig
      call mpi_gatherv(rh4(1,1,k,n),ijn(mm1),mpi_rtype,&
           work1,ijn,displs_g,mpi_rtype,&
           mype_work,mpi_comm_world,ierror)
      if (mype==mype_work) then
        do kk=1,iglobal
          ni1=ltosi(kk); ni2=ltosj(kk)
          workgrd(ni1,ni2)=work1(kk)
        end do
        do i=1,nlat
          do j=1,nlon
            rhvc(i,k,n) = rhvc(i,k,n) + workgrd(i,j)/float(nlon)
          end do
        end do
      end if
    end do
  end do

  do n=1,nsig
    do k=1,nsig
      call mpi_gatherv(oz4(1,1,k,n),ijn(mm1),mpi_rtype,&
           work1,ijn,displs_g,mpi_rtype,&
           mype_work,mpi_comm_world,ierror)
      if (mype==mype_work) then
        do kk=1,iglobal
          ni1=ltosi(kk); ni2=ltosj(kk)
          workgrd(ni1,ni2)=work1(kk)
        end do
        do i=1,nlat
          do j=1,nlon
            ozvc(i,k,n) = ozvc(i,k,n) + workgrd(i,j)/float(nlon)
          end do
        end do
      end if
    end do
  end do

  do n=1,nsig
    do k=1,nsig
      call mpi_gatherv(cw4(1,1,k,n),ijn(mm1),mpi_rtype,&
           work1,ijn,displs_g,mpi_rtype,&
           mype_work,mpi_comm_world,ierror)
      if (mype==mype_work) then
        do kk=1,iglobal
          ni1=ltosi(kk); ni2=ltosj(kk)
          workgrd(ni1,ni2)=work1(kk)
        end do
        do i=1,nlat
          do j=1,nlon
            cwvc(i,k,n) = cwvc(i,k,n) + workgrd(i,j)/float(nlon)
          end do
        end do
      end if
    end do
  end do

  if (mype==mype_work) then
    do i=1,nlat
      do k=1,nsig
        diag(k,1)=sqrt(sfvc(i,k,k))
        diag(k,2)=sqrt(vpvc(i,k,k))
        diag(k,3)=sqrt(tvc(i,k,k))
        diag(k,4)=sqrt(rhvc(i,k,k))
        diag(k,5)=sqrt(ozvc(i,k,k))
        diag(k,6)=max(small,sqrt(cwvc(i,k,k)))
      end do
      do m=1,nsig
        do k=1,nsig
          sfvc(i,k,m)=sfvc(i,k,m)/(diag(k,1)*diag(m,1))
          vpvc(i,k,m)=vpvc(i,k,m)/(diag(k,2)*diag(m,2))
           tvc(i,k,m)= tvc(i,k,m)/(diag(k,3)*diag(m,3))
          rhvc(i,k,m)=rhvc(i,k,m)/(diag(k,4)*diag(m,4))
          ozvc(i,k,m)=ozvc(i,k,m)/(diag(k,5)*diag(m,5))
          cwvc(i,k,m)=cwvc(i,k,m)/(diag(k,6)*diag(m,6))
        end do
      end do
    end do !end do over lat
    call smoothvsc(sfvc,vpvc,tvc,rhvc,ozvc,cwvc,vsc_out)

    do k=1,nsig
      do i=1,nlat
        sfvln(i,k)=vsc_out(i,k)
        vpvln(i,k)=vsc_out(i,nsig+k)
        tvln(i,k)=vsc_out(i,2*nsig+k)
        qvln(i,k)=vsc_out(i,3*nsig+k)
        ozvln(i,k)=vsc_out(i,4*nsig+k)
        cvln(i,k)=vsc_out(i,5*nsig+k)
      end do
    end do

! set bounds on q vertical length scales
!    do k=41,nsig
!      do i=1,nlat
!        qvln(i,k)=max(2.0_r_kind,qvln(i,k))
!      end do
!    end do
! Make sure that vertical scales for cloud water are real values, else set to rh

    do k=1,nsig
      do i=1,nlat
        cvln(i,k)=max(min(10.0_r_kind,cvln(i,k)),0.1)
      end do
    end do

    call smoothlat(sfvln,nsig,smoothdeg)
    call smoothlat(vpvln,nsig,smoothdeg)
    call smoothlat(tvln,nsig,smoothdeg)
    call smoothlat(qvln,nsig,smoothdeg)
    call smoothlat(ozvln,nsig,smoothdeg)
    call smoothlat(cvln,nsig,smoothdeg)
  end if ! end if mype_work

  call mpi_bcast(sfvln,nlat*nsig,mpi_rtype,mype_work,mpi_comm_world,ierror)
  call mpi_bcast(vpvln,nlat*nsig,mpi_rtype,mype_work,mpi_comm_world,ierror)
  call mpi_bcast(tvln,nlat*nsig,mpi_rtype,mype_work,mpi_comm_world,ierror)
  call mpi_bcast(qvln,nlat*nsig,mpi_rtype,mype_work,mpi_comm_world,ierror)
  call mpi_bcast(ozvln,nlat*nsig,mpi_rtype,mype_work,mpi_comm_world,ierror)
  call mpi_bcast(cvln,nlat*nsig,mpi_rtype,mype_work,mpi_comm_world,ierror)

  return 
end subroutine vertsc

subroutine smoothvsc(sfvc,vpvc,tvc,qvc,ozvc,cvc,vsc_out)
  use kinds,only: r_kind,r_double
  use postmod, only: ndeg,nasm
  use variables,only: nlat,nsig,one
  implicit none

  real(r_double),dimension(nlat,nsig,nsig),intent(in):: sfvc,vpvc,tvc,qvc,ozvc,cvc
  real(r_kind),dimension(nlat,nsig*6),intent(out):: vsc_out


  real(r_double),dimension(nsig,nasm):: table
  real(r_double),dimension(nasm)::sum
  real(r_kind) amin,scale
  integer i,j,l,k,ll,kkkk

  real(r_kind),dimension(nsig,ndeg):: alv
  real(r_kind),dimension(nsig):: be,rate,dssv,vwl
  real(r_kind) samp,fact,ak,delta,awgt
  real(r_kind) turn(ndeg,ndeg)
  real(r_kind) w(nsig)
  real(r_double)  weights(nsig)
  integer nav

  vwl=1.3_r_kind
  call rfdpar1(be,rate,ndeg)
  call rfdpar2(be,rate,turn,samp,ndeg)

  scale=.01
  nav=530
  vsc_out=0.

  do l=1,nsig
    do i=1,nasm
      vwl=scale*float(i)
      w=0.
      w(l)=1.
      call rfdparv(vwl,rate,alv,nsig,ndeg)
      do k=1,nsig
        dssv(k)=sqrt(samp*vwl(k))
      enddo

      call smoothz(w,nsig,1,ndeg,alv,be,dssv,1)
      call smoothz(w,nsig,1,ndeg,alv,be,dssv,2)
      fact=1./w(l)
      do k=1,nsig
        table(k,i)=w(k)*fact
      enddo
    enddo

    awgt=10.0
    do k=1-l,nsig-l
      weights(l+k)=exp(-(awgt*k*k)/(nsig*nsig))
    end do
!!    print *,weights

    do ll=1,6
! ll=1 z
! ll=2 d
! ll=3 t
! ll=4 q
! ll=5 oz
! ll=6 clw
      do j=1,nlat
        ak=0.
        amin=999.
        sum=0.
        do i=1,nav
          do k=1-l,nsig-l
            if (ll.eq.1) then
              sum(i)=sum(i)+weights(l+k)*(sfvc(j,l,l+k)-table(l+k,i))**2.
            else if (ll.eq.2) then
              sum(i)=sum(i)+weights(l+k)*(vpvc(j,l,l+k)-table(l+k,i))**2.
            else if (ll.eq.3) then
              sum(i)=sum(i)+weights(l+k)*(tvc(j,l,l+k)-table(l+k,i))**2.
            else if (ll.eq.4) then
              sum(i)=sum(i)+weights(l+k)*(qvc(j,l,l+k)-table(l+k,i))**2.
            else if (ll.eq.5) then
              sum(i)=sum(i)+weights(l+k)*(ozvc(j,l,l+k)-table(l+k,i))**2.
            else if (ll.eq.6) then
              sum(i)=sum(i)+weights(l+k)*(cvc(j,l,l+k)-table(l+k,i))**2.
            end if
          enddo
          if(sum(i) < amin)then
            amin=sum(i)
            kkkk=i
          endif
        enddo
        i=kkkk
        ak=float(i)
        delta=0.
        if(i > 1 .and. i < nav)delta=.5*(sum(i-1)-sum(i+1))/ &
          (sum(i+1)+sum(i-1)-2.*sum(i))
        vsc_out(j,(ll-1)*nsig+l)=scale*(ak+delta)
      enddo !enddo nlat
    enddo !enddo ll
  enddo ! end l -- nsig

  return
end subroutine smoothvsc


