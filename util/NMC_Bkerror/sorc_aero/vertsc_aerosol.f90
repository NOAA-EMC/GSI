subroutine vertsc_aerosol(numcases,mype)
  use kinds, only: r_kind,r_single,i_kind,r_double
  use postmod, only: smoothlat
  use variables,only: nlat,nlon,nsig,lat1,lon1,zero,&
      displs_g,ijn,db_prec,filunit1,filunit2,npe,&
      d1vln,d2vln,d3vln,d4vln,d5vln,s1vln,s2vln,s3vln,s4vln, &
      so4vln,oc1vln,oc2vln,bc1vln,bc2vln, &
      iglobal,ltosi,ltosj,smoothdeg,one
  implicit none
  include 'mpif.h'

  integer,intent(in):: numcases,mype

  real(r_kind),dimension(lat1,lon1,nsig):: d1a,d2a,d3a,d4a,d5a, &
      s1a,s2a,s3a,s4a,so4a,oc1a,oc2a,bc1a,bc2a
  real(r_kind),dimension(lat1,lon1,nsig):: d1b,d2b,d3b,d4b,d5b, &
      s1b,s2b,s3b,s4b,so4b,oc1b,oc2b,bc1b,bc2b

  real(r_kind),dimension(lat1,lon1,nsig,nsig):: d1d,d2d,d3d,d4d,d5d, &
      s1d,s2d,s3d,s4d,so4d,oc1d,oc2d,bc1d,bc2d

  real(r_kind),dimension(iglobal):: work1
  real(r_double),dimension(nlat,nlon):: workgrd
  real(r_kind),dimension(nlat,nsig*14):: vsc_out
  real(r_kind) r_norm

  real(r_double),dimension(nlat,nsig,nsig):: d1vc,d2vc,d3vc,d4vc,d5vc, &
      s1vc,s2vc,s3vc,s4vc,so4vc,oc1vc,oc2vc,bc1vc,bc2vc
  real(r_double),dimension(nsig,14):: diag
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


  d1d=zero ; d2d=zero ; d3d=zero ; d4d=zero ; d5d=zero ; 
  s1d=zero ; s2d=zero ; s3d=zero ; s4d=zero ; so4d=zero ;
  oc1d=zero ; oc2d=zero ; bc1d=zero ; bc2d=zero

  d1vc=0._r_double ; d2vc=0._r_double ; d3vc=0._r_double ; d4vc=0._r_double ; d5vc=0._r_double ; 
  s1vc=0._r_double ; s2vc=0._r_double ; s3vc=0._r_double ; s4vc=0._r_double ; so4vc=0._r_double ;
  oc1vc=0._r_double ; oc2vc=0._r_double ; bc1vc=0._r_double ; bc2vc=0._r_double

  open(filunit1,form='unformatted',action='read')
  rewind(filunit1)
  open(filunit2,form='unformatted',action='read')
  rewind(filunit2)

  do n=1,numcases
    if (mype==0)  write(6,*) 'VERTSC, PROCESSING PAIR # ',n
! Read in subdomain grids
    read(filunit1) d1a,d2a,d3a,d4a,d5a,s1a,s2a,s3a,s4a,so4a,oc1a,oc2a,bc1a,bc2a
    read(filunit2) d1b,d2b,d3b,d4b,d5b,s1b,s2b,s3b,s4b,so4b,oc1b,oc2b,bc1b,bc2b

    call delvars_aerosol(d1a,d2a,d3a,d4a,d5a,s1a,s2a,s3a,s4a,so4a,oc1a,oc2a,bc1a,bc2a, &
                         d1b,d2b,d3b,d4b,d5b,s1b,s2b,s3b,s4b,so4b,oc1b,oc2b,bc1b,bc2b,mype)

    do m=1,nsig
      do k=1,nsig
        do j=1,lon1
          do i=1,lat1
            d1d(i,j,k,m) = d1d(i,j,k,m) + d1a(i,j,k)*d1a(i,j,m)
            d2d(i,j,k,m) = d2d(i,j,k,m) + d2a(i,j,k)*d2a(i,j,m)
            d3d(i,j,k,m) = d3d(i,j,k,m) + d3a(i,j,k)*d3a(i,j,m)
            d4d(i,j,k,m) = d4d(i,j,k,m) + d4a(i,j,k)*d4a(i,j,m)
            d5d(i,j,k,m) = d5d(i,j,k,m) + d5a(i,j,k)*d5a(i,j,m)
            s1d(i,j,k,m) = s1d(i,j,k,m) + s1a(i,j,k)*s1a(i,j,m)
            s2d(i,j,k,m) = s2d(i,j,k,m) + s2a(i,j,k)*s2a(i,j,m)
            s3d(i,j,k,m) = s3d(i,j,k,m) + s3a(i,j,k)*s3a(i,j,m)
            s4d(i,j,k,m) = s4d(i,j,k,m) + s4a(i,j,k)*s4a(i,j,m)
            so4d(i,j,k,m) = so4d(i,j,k,m) + so4a(i,j,k)*so4a(i,j,m)
            oc1d(i,j,k,m) = oc1d(i,j,k,m) + oc1a(i,j,k)*oc1a(i,j,m)
            oc2d(i,j,k,m) = oc2d(i,j,k,m) + oc2a(i,j,k)*oc2a(i,j,m)
            bc1d(i,j,k,m) = bc1d(i,j,k,m) + bc1a(i,j,k)*bc1a(i,j,m)
            bc2d(i,j,k,m) = bc2d(i,j,k,m) + bc2a(i,j,k)*bc2a(i,j,m)
          end do
        end do
      end do
    end do
  end do ! end do numcases
  close(filunit1)
  close(filunit2)

  d1d=d1d*r_norm ; d2d=d2d*r_norm ; d3d=d3d*r_norm ; d4d=d4d*r_norm ; d5d=d5d*r_norm 
  s1d=s1d*r_norm ; s2d=s2d*r_norm ; s3d=s3d*r_norm ; s4d=s4d*r_norm ; so4d=so4d*r_norm 
  oc1d=oc1d*r_norm ; oc2d=oc2d*r_norm ; bc1d=bc1d*r_norm ; bc2d=bc2d*r_norm

! Need to convert full subdomain corrleation matrices into arrays
! That contain zonal mean
  do n=1,nsig
    do k=1,nsig
      call mpi_gatherv(d1d(1,1,k,n),ijn(mm1),mpi_rtype,&
           work1,ijn,displs_g,mpi_rtype,&
           mype_work,mpi_comm_world,ierror)
      if (mype==mype_work) then
        do kk=1,iglobal
          ni1=ltosi(kk); ni2=ltosj(kk)
          workgrd(ni1,ni2)=work1(kk)
        end do
        do i=1,nlat
          do j=1,nlon
            d1vc(i,k,n) = d1vc(i,k,n) + workgrd(i,j)/float(nlon)
          end do
        end do
      end if
    end do
  end do
  if (mype==mype_work) write(6,*) 'vertsc process at dust1'

  do n=1,nsig
    do k=1,nsig
      call mpi_gatherv(d2d(1,1,k,n),ijn(mm1),mpi_rtype,&
           work1,ijn,displs_g,mpi_rtype,&
           mype_work,mpi_comm_world,ierror)
      if (mype==mype_work) then
        do kk=1,iglobal
          ni1=ltosi(kk); ni2=ltosj(kk)
          workgrd(ni1,ni2)=work1(kk)
        end do
        do i=1,nlat
          do j=1,nlon
            d2vc(i,k,n) = d2vc(i,k,n) + workgrd(i,j)/float(nlon)
          end do
        end do
      end if
    end do
  end do
  if (mype==mype_work) write(6,*) 'vertsc process at dust2'

  do n=1,nsig
    do k=1,nsig
      call mpi_gatherv(d3d(1,1,k,n),ijn(mm1),mpi_rtype,&
           work1,ijn,displs_g,mpi_rtype,&
           mype_work,mpi_comm_world,ierror)
      if (mype==mype_work) then
        do kk=1,iglobal
          ni1=ltosi(kk); ni2=ltosj(kk)
          workgrd(ni1,ni2)=work1(kk)
        end do
        do i=1,nlat
          do j=1,nlon
            d3vc(i,k,n) = d3vc(i,k,n) + workgrd(i,j)/float(nlon)
          end do
        end do
      end if
    end do
  end do
  if (mype==mype_work) write(6,*) 'vertsc process at dust3'

  do n=1,nsig
    do k=1,nsig
      call mpi_gatherv(d4d(1,1,k,n),ijn(mm1),mpi_rtype,&
           work1,ijn,displs_g,mpi_rtype,&
           mype_work,mpi_comm_world,ierror)
      if (mype==mype_work) then
        do kk=1,iglobal
          ni1=ltosi(kk); ni2=ltosj(kk)
          workgrd(ni1,ni2)=work1(kk)
        end do
        do i=1,nlat
          do j=1,nlon
            d4vc(i,k,n) = d4vc(i,k,n) + workgrd(i,j)/float(nlon)
          end do
        end do
      end if
    end do
  end do
  if (mype==mype_work) write(6,*) 'vertsc process at dust4'

  do n=1,nsig
    do k=1,nsig
      call mpi_gatherv(d5d(1,1,k,n),ijn(mm1),mpi_rtype,&
           work1,ijn,displs_g,mpi_rtype,&
           mype_work,mpi_comm_world,ierror)
      if (mype==mype_work) then
        do kk=1,iglobal
          ni1=ltosi(kk); ni2=ltosj(kk)
          workgrd(ni1,ni2)=work1(kk)
        end do
        do i=1,nlat
          do j=1,nlon
            d5vc(i,k,n) = d5vc(i,k,n) + workgrd(i,j)/float(nlon)
          end do
        end do
      end if
    end do
  end do
  if (mype==mype_work) write(6,*) 'vertsc process at dust5'

  do n=1,nsig
    do k=1,nsig
      call mpi_gatherv(s1d(1,1,k,n),ijn(mm1),mpi_rtype,&
           work1,ijn,displs_g,mpi_rtype,&
           mype_work,mpi_comm_world,ierror)
      if (mype==mype_work) then
        do kk=1,iglobal
          ni1=ltosi(kk); ni2=ltosj(kk)
          workgrd(ni1,ni2)=work1(kk)
        end do
        do i=1,nlat
          do j=1,nlon
            s1vc(i,k,n) = s1vc(i,k,n) + workgrd(i,j)/float(nlon)
          end do
        end do
      end if
    end do
  end do
  if (mype==mype_work) write(6,*) 'vertsc process at seas1'

  do n=1,nsig
    do k=1,nsig
      call mpi_gatherv(s2d(1,1,k,n),ijn(mm1),mpi_rtype,&
           work1,ijn,displs_g,mpi_rtype,&
           mype_work,mpi_comm_world,ierror)
      if (mype==mype_work) then
        do kk=1,iglobal
          ni1=ltosi(kk); ni2=ltosj(kk)
          workgrd(ni1,ni2)=work1(kk)
        end do
        do i=1,nlat
          do j=1,nlon
            s2vc(i,k,n) = s2vc(i,k,n) + workgrd(i,j)/float(nlon)
          end do
        end do
      end if
    end do
  end do
  if (mype==mype_work) write(6,*) 'vertsc process at seas2'

  do n=1,nsig
    do k=1,nsig
      call mpi_gatherv(s3d(1,1,k,n),ijn(mm1),mpi_rtype,&
           work1,ijn,displs_g,mpi_rtype,&
           mype_work,mpi_comm_world,ierror)
      if (mype==mype_work) then
        do kk=1,iglobal
          ni1=ltosi(kk); ni2=ltosj(kk)
          workgrd(ni1,ni2)=work1(kk)
        end do
        do i=1,nlat
          do j=1,nlon
            s3vc(i,k,n) = s3vc(i,k,n) + workgrd(i,j)/float(nlon)
          end do
        end do
      end if
    end do
  end do
  if (mype==mype_work) write(6,*) 'vertsc process at seas3'

  do n=1,nsig
    do k=1,nsig
      call mpi_gatherv(s4d(1,1,k,n),ijn(mm1),mpi_rtype,&
           work1,ijn,displs_g,mpi_rtype,&
           mype_work,mpi_comm_world,ierror)
      if (mype==mype_work) then
        do kk=1,iglobal
          ni1=ltosi(kk); ni2=ltosj(kk)
          workgrd(ni1,ni2)=work1(kk)
        end do
        do i=1,nlat
          do j=1,nlon
            s4vc(i,k,n) = s4vc(i,k,n) + workgrd(i,j)/float(nlon)
          end do
        end do
      end if
    end do
  end do
  if (mype==mype_work) write(6,*) 'vertsc process at seas4'

  do n=1,nsig
    do k=1,nsig
      call mpi_gatherv(so4d(1,1,k,n),ijn(mm1),mpi_rtype,&
           work1,ijn,displs_g,mpi_rtype,&
           mype_work,mpi_comm_world,ierror)
      if (mype==mype_work) then
        do kk=1,iglobal
          ni1=ltosi(kk); ni2=ltosj(kk)
          workgrd(ni1,ni2)=work1(kk)
        end do
        do i=1,nlat
          do j=1,nlon
            so4vc(i,k,n) = so4vc(i,k,n) + workgrd(i,j)/float(nlon)
          end do
        end do
      end if
    end do
  end do
  if (mype==mype_work) write(6,*) 'vertsc process at sulfate'

  do n=1,nsig
    do k=1,nsig
      call mpi_gatherv(oc1d(1,1,k,n),ijn(mm1),mpi_rtype,&
           work1,ijn,displs_g,mpi_rtype,&
           mype_work,mpi_comm_world,ierror)
      if (mype==mype_work) then
        do kk=1,iglobal
          ni1=ltosi(kk); ni2=ltosj(kk)
          workgrd(ni1,ni2)=work1(kk)
        end do
        do i=1,nlat
          do j=1,nlon
            oc1vc(i,k,n) = oc1vc(i,k,n) + workgrd(i,j)/float(nlon)
          end do
        end do
      end if
    end do
  end do
  if (mype==mype_work) write(6,*) 'vertsc process at oc1'

  do n=1,nsig
    do k=1,nsig
      call mpi_gatherv(oc2d(1,1,k,n),ijn(mm1),mpi_rtype,&
           work1,ijn,displs_g,mpi_rtype,&
           mype_work,mpi_comm_world,ierror)
      if (mype==mype_work) then
        do kk=1,iglobal
          ni1=ltosi(kk); ni2=ltosj(kk)
          workgrd(ni1,ni2)=work1(kk)
        end do
        do i=1,nlat
          do j=1,nlon
            oc2vc(i,k,n) = oc2vc(i,k,n) + workgrd(i,j)/float(nlon)
          end do
        end do
      end if
    end do
  end do
  if (mype==mype_work) write(6,*) 'vertsc process at oc2'

  do n=1,nsig
    do k=1,nsig
      call mpi_gatherv(bc1d(1,1,k,n),ijn(mm1),mpi_rtype,&
           work1,ijn,displs_g,mpi_rtype,&
           mype_work,mpi_comm_world,ierror)
      if (mype==mype_work) then
        do kk=1,iglobal
          ni1=ltosi(kk); ni2=ltosj(kk)
          workgrd(ni1,ni2)=work1(kk)
        end do
        do i=1,nlat
          do j=1,nlon
            bc1vc(i,k,n) = bc1vc(i,k,n) + workgrd(i,j)/float(nlon)
          end do
        end do
      end if
    end do
  end do
  if (mype==mype_work) write(6,*) 'vertsc process at bc1'

  do n=1,nsig
    do k=1,nsig
      call mpi_gatherv(bc2d(1,1,k,n),ijn(mm1),mpi_rtype,&
           work1,ijn,displs_g,mpi_rtype,&
           mype_work,mpi_comm_world,ierror)
      if (mype==mype_work) then
        do kk=1,iglobal
          ni1=ltosi(kk); ni2=ltosj(kk)
          workgrd(ni1,ni2)=work1(kk)
        end do
        do i=1,nlat
          do j=1,nlon
            bc2vc(i,k,n) = bc2vc(i,k,n) + workgrd(i,j)/float(nlon)
          end do
        end do
      end if
    end do
  end do
  if (mype==mype_work) write(6,*) 'vertsc process at bc2'

  if (mype==mype_work) write(150,*) d1vc(:,:,1)

  if (mype==mype_work) then
    do i=1,nlat
      do k=1,nsig
        diag(k, 1)=max(small,sqrt( d1vc(i,k,k)))
        diag(k, 2)=max(small,sqrt( d2vc(i,k,k)))
        diag(k, 3)=max(small,sqrt( d3vc(i,k,k)))
        diag(k, 4)=max(small,sqrt( d4vc(i,k,k)))
        diag(k, 5)=max(small,sqrt( d5vc(i,k,k)))
        diag(k, 6)=max(small,sqrt( s1vc(i,k,k)))
        diag(k, 7)=max(small,sqrt( s2vc(i,k,k)))
        diag(k, 8)=max(small,sqrt( s3vc(i,k,k)))
        diag(k, 9)=max(small,sqrt( s4vc(i,k,k)))
        diag(k,10)=max(small,sqrt(so4vc(i,k,k)))
        diag(k,11)=max(small,sqrt(oc1vc(i,k,k)))
        diag(k,12)=max(small,sqrt(oc2vc(i,k,k)))
        diag(k,13)=max(small,sqrt(bc1vc(i,k,k)))
        diag(k,14)=max(small,sqrt(bc2vc(i,k,k)))
      end do
      do m=1,nsig
        do k=1,nsig
          d1vc(i,k,m)=d1vc(i,k,m)/(diag(k,1)*diag(m,1))
          d2vc(i,k,m)=d2vc(i,k,m)/(diag(k,2)*diag(m,2))
          d3vc(i,k,m)=d3vc(i,k,m)/(diag(k,3)*diag(m,3))
          d4vc(i,k,m)=d4vc(i,k,m)/(diag(k,4)*diag(m,4))
          d5vc(i,k,m)=d5vc(i,k,m)/(diag(k,5)*diag(m,5))
          s1vc(i,k,m)=s1vc(i,k,m)/(diag(k,6)*diag(m,6))
          s2vc(i,k,m)=s2vc(i,k,m)/(diag(k,7)*diag(m,7))
          s3vc(i,k,m)=s3vc(i,k,m)/(diag(k,8)*diag(m,8))
          s4vc(i,k,m)=s4vc(i,k,m)/(diag(k,9)*diag(m,9))
          so4vc(i,k,m)=so4vc(i,k,m)/(diag(k,10)*diag(m,10))
          oc1vc(i,k,m)=oc1vc(i,k,m)/(diag(k,11)*diag(m,11))
          oc2vc(i,k,m)=oc2vc(i,k,m)/(diag(k,12)*diag(m,12))
          bc1vc(i,k,m)=bc1vc(i,k,m)/(diag(k,13)*diag(m,13))
          bc2vc(i,k,m)=bc2vc(i,k,m)/(diag(k,14)*diag(m,14))
        end do
      end do
    end do !end do over lat

    write(151,*) d1vc(:,:,1)

    call smoothvsc(d1vc,d2vc,d3vc,d4vc,d5vc,s1vc,s2vc,s3vc,s4vc, &
                   so4vc,oc1vc,oc2vc,bc1vc,bc2vc,vsc_out)

    do k=1,nsig
      do i=1,nlat
        d1vln(i,k)=vsc_out(i,k)
        d2vln(i,k)=vsc_out(i,nsig+k)
        d3vln(i,k)=vsc_out(i,2*nsig+k)
        d4vln(i,k)=vsc_out(i,3*nsig+k)
        d5vln(i,k)=vsc_out(i,4*nsig+k)
        s1vln(i,k)=vsc_out(i,5*nsig+k)
        s2vln(i,k)=vsc_out(i,6*nsig+k)
        s3vln(i,k)=vsc_out(i,7*nsig+k)
        s4vln(i,k)=vsc_out(i,8*nsig+k)
        so4vln(i,k)=vsc_out(i,9*nsig+k)
        oc1vln(i,k)=vsc_out(i,10*nsig+k)
        oc2vln(i,k)=vsc_out(i,11*nsig+k)
        bc1vln(i,k)=vsc_out(i,12*nsig+k)
        bc2vln(i,k)=vsc_out(i,13*nsig+k)
      end do
    end do
    write(152,*) d1vln

! set bounds on q vertical length scales
!    do k=41,nsig
!      do i=1,nlat
!        qvln(i,k)=max(2.0_r_kind,qvln(i,k))
!      end do
!    end do
! Make sure that vertical scales for cloud water are real values, else set to rh
!
    do k=1,nsig
      do i=1,nlat
!        cvln(i,k)=max(min(10.0_r_kind,cvln(i,k)),0.1)
        d1vln(i,k)=max(min(10.0_r_kind,d1vln(i,k)),0.1)
        d2vln(i,k)=max(min(10.0_r_kind,d2vln(i,k)),0.1)
        d3vln(i,k)=max(min(10.0_r_kind,d3vln(i,k)),0.1)
        d4vln(i,k)=max(min(10.0_r_kind,d4vln(i,k)),0.1)
        d5vln(i,k)=max(min(10.0_r_kind,d5vln(i,k)),0.1)
        s1vln(i,k)=max(min(10.0_r_kind,s1vln(i,k)),0.1)
        s2vln(i,k)=max(min(10.0_r_kind,s2vln(i,k)),0.1)
        s3vln(i,k)=max(min(10.0_r_kind,s3vln(i,k)),0.1)
        s4vln(i,k)=max(min(10.0_r_kind,s4vln(i,k)),0.1)
        so4vln(i,k)=max(min(10.0_r_kind,so4vln(i,k)),0.1)
        oc1vln(i,k)=max(min(10.0_r_kind,oc1vln(i,k)),0.1)
        oc2vln(i,k)=max(min(10.0_r_kind,oc2vln(i,k)),0.1)
        bc1vln(i,k)=max(min(10.0_r_kind,bc1vln(i,k)),0.1)
        bc2vln(i,k)=max(min(10.0_r_kind,bc2vln(i,k)),0.1)
      end do
    end do

    call smoothlat(d1vln,nsig,smoothdeg)
    call smoothlat(d2vln,nsig,smoothdeg)
    call smoothlat(d3vln,nsig,smoothdeg)
    call smoothlat(d4vln,nsig,smoothdeg)
    call smoothlat(d5vln,nsig,smoothdeg)
    call smoothlat(s1vln,nsig,smoothdeg)
    call smoothlat(s2vln,nsig,smoothdeg)
    call smoothlat(s3vln,nsig,smoothdeg)
    call smoothlat(s4vln,nsig,smoothdeg)
    call smoothlat(so4vln,nsig,smoothdeg)
    call smoothlat(oc1vln,nsig,smoothdeg)
    call smoothlat(oc2vln,nsig,smoothdeg)
    call smoothlat(bc1vln,nsig,smoothdeg)
    call smoothlat(bc2vln,nsig,smoothdeg)

  end if ! end if mype_work

  call mpi_bcast(d1vln,nlat*nsig,mpi_rtype,mype_work,mpi_comm_world,ierror)
  call mpi_bcast(d2vln,nlat*nsig,mpi_rtype,mype_work,mpi_comm_world,ierror)
  call mpi_bcast(d3vln,nlat*nsig,mpi_rtype,mype_work,mpi_comm_world,ierror)
  call mpi_bcast(d4vln,nlat*nsig,mpi_rtype,mype_work,mpi_comm_world,ierror)
  call mpi_bcast(d5vln,nlat*nsig,mpi_rtype,mype_work,mpi_comm_world,ierror)
  call mpi_bcast(s1vln,nlat*nsig,mpi_rtype,mype_work,mpi_comm_world,ierror)
  call mpi_bcast(s2vln,nlat*nsig,mpi_rtype,mype_work,mpi_comm_world,ierror)
  call mpi_bcast(s3vln,nlat*nsig,mpi_rtype,mype_work,mpi_comm_world,ierror)
  call mpi_bcast(s4vln,nlat*nsig,mpi_rtype,mype_work,mpi_comm_world,ierror)
  call mpi_bcast(so4vln,nlat*nsig,mpi_rtype,mype_work,mpi_comm_world,ierror)
  call mpi_bcast(oc1vln,nlat*nsig,mpi_rtype,mype_work,mpi_comm_world,ierror)
  call mpi_bcast(oc2vln,nlat*nsig,mpi_rtype,mype_work,mpi_comm_world,ierror)
  call mpi_bcast(bc1vln,nlat*nsig,mpi_rtype,mype_work,mpi_comm_world,ierror)
  call mpi_bcast(bc2vln,nlat*nsig,mpi_rtype,mype_work,mpi_comm_world,ierror)

  return 
end subroutine vertsc_aerosol

subroutine smoothvsc(d1vc,d2vc,d3vc,d4vc,d5vc,s1vc,s2vc,s3vc,s4vc, &
                     so4vc,oc1vc,oc2vc,bc1vc,bc2vc,vsc_out)
  use kinds,only: r_kind,r_double
  use postmod, only: ndeg,nasm  ! ndeg=6, nasm=560
  use variables,only: nlat,nsig,one
  implicit none

  real(r_double),dimension(nlat,nsig,nsig),intent(in):: d1vc,d2vc,d3vc,d4vc,d5vc, &
                                s1vc,s2vc,s3vc,s4vc,so4vc,oc1vc,oc2vc,bc1vc,bc2vc
  real(r_kind),dimension(nlat,nsig*14),intent(out):: vsc_out


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

    do ll=1,14
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
              sum(i)=sum(i)+weights(l+k)*(d1vc(j,l,l+k)-table(l+k,i))**2.
            else if (ll.eq.2) then
              sum(i)=sum(i)+weights(l+k)*(d2vc(j,l,l+k)-table(l+k,i))**2.
            else if (ll.eq.3) then
              sum(i)=sum(i)+weights(l+k)*(d3vc(j,l,l+k)-table(l+k,i))**2.
            else if (ll.eq.4) then
              sum(i)=sum(i)+weights(l+k)*(d4vc(j,l,l+k)-table(l+k,i))**2.
            else if (ll.eq.5) then
              sum(i)=sum(i)+weights(l+k)*(d5vc(j,l,l+k)-table(l+k,i))**2.
            else if (ll.eq.6) then
              sum(i)=sum(i)+weights(l+k)*(s1vc(j,l,l+k)-table(l+k,i))**2.
            else if (ll.eq.7) then
              sum(i)=sum(i)+weights(l+k)*(s2vc(j,l,l+k)-table(l+k,i))**2.
            else if (ll.eq.8) then
              sum(i)=sum(i)+weights(l+k)*(s3vc(j,l,l+k)-table(l+k,i))**2.
            else if (ll.eq.9) then
              sum(i)=sum(i)+weights(l+k)*(s4vc(j,l,l+k)-table(l+k,i))**2.
            else if (ll.eq.10) then
              sum(i)=sum(i)+weights(l+k)*(so4vc(j,l,l+k)-table(l+k,i))**2.
            else if (ll.eq.11) then
              sum(i)=sum(i)+weights(l+k)*(oc1vc(j,l,l+k)-table(l+k,i))**2.
            else if (ll.eq.12) then
              sum(i)=sum(i)+weights(l+k)*(oc2vc(j,l,l+k)-table(l+k,i))**2.
            else if (ll.eq.13) then
              sum(i)=sum(i)+weights(l+k)*(bc1vc(j,l,l+k)-table(l+k,i))**2.
            else if (ll.eq.14) then
              sum(i)=sum(i)+weights(l+k)*(bc2vc(j,l,l+k)-table(l+k,i))**2.
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


