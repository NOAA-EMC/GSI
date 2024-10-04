subroutine readpairs_aerosol(npe,mype,numcases)
  use variables, only: nlat,nlon,nsig,ak5,bk5,ck5,&
      na,nb,filename,hybrid,db_prec,zero,one,grav,fv,&
      idpsfc5,idthrm5,cp5,ntrac5,idvc5,idvm5,lat1,lon1,&
      iglobal,ijn_s,displs_s,filunit1,filunit2,&
      ird_s,irc_s,displs_g,modelname,fv3aeroname,ngacaeroname,aeroname
  use specgrid, only: sptez_s,sptezv_s,nc,ncin,factvml,&
      factsml,enn1,ncd2,jcaptrans,jcap,jcapin,unload_grid
  use comm_mod, only: levs_id,nvar_id,grid2sub,nsig1o,spec_send,&
      disp_spec
  use kinds, only: r_kind,r_single,r_double
  use nemsio_module, only: nemsio_init,nemsio_open,nemsio_close, & 
                  nemsio_gfile,nemsio_getfilehead,nemsio_readrecv
  implicit none
  include 'mpif.h'

  integer npe,mype,numcases,ierror,mpi_rtype,iret,iret2
  integer mm1,kk,proc1,proc2
  integer i,j,k,m,n,inges,inge2,i2,i2m1
  integer k1,k2,k3,k4,k5,k6,k7,k8,k9,k10,k11,k12,k13

  real(r_kind),dimension(lat1,lon1,nsig):: d1a,d2a,d3a,d4a,d5a, &
      s1a,s2a,s3a,s4a,so4a,oc1a,oc2a,bc1a,bc2a,d1b,d2b,d3b,d4b,d5b, &
      s1b,s2b,s3b,s4b,so4b,oc1b,oc2b,bc1b,bc2b 

  real(r_kind),dimension(nc):: z,z2,wd,wd2,wz,wz2
  real(r_kind),dimension(ncin):: s1tmp,s2tmp,tmp
  real(r_kind),dimension(ncin,nsig1o):: z41,z42
  real(r_kind),dimension(ncin,14*nsig):: z4all

  real(r_kind),dimension(nlon,nlat-2):: grid1,grid2
  real(r_kind),dimension(iglobal,nsig1o):: work1,work2

  real(r_kind),dimension(iglobal,ntrac5):: gridtrac1,gridtrac2

  type(nemsio_gfile) :: gfile

  logical ice
  if (db_prec) then
    mpi_rtype=mpi_real8
  else
    mpi_rtype=mpi_real4
  end if

  if (modelname=='fv3') then
     aeroname=fv3aeroname
  else if (modelname=='ngac') then
     aeroname=ngacaeroname
  end if

  inges=50
  inge2=51
  mm1=mype+1
  proc1=0
  proc2=npe-1

  filunit1=(10000+(mype+1))
  filunit2=(20000+(mype+1))

! Each mpi task will carry two files, which contains all variables, for each of the time levels
  open(filunit1,form='unformatted',action='write')
  rewind(filunit1)
  open(filunit2,form='unformatted',action='write')
  rewind(filunit2)

  do n=1,numcases
    if (mype==0)  write(6,*)'opening=', filename(na(n))
    if (mype==0)  write(6,*)'opening=', filename(nb(n))

! Get information from NEMSIO
    if (mype==proc1) then
       call nemsio_init(iret=iret)
       call nemsio_open(gfile,filename(na(n)),'READ',iret=iret)
    end if
    if (mype==proc2) then
       call nemsio_init(iret=iret)
       call nemsio_open(gfile,filename(nb(n)),'READ',iret=iret)
    end if
    call mpi_barrier(mpi_comm_world,iret2)

    if (mype==proc1 .or. mype==proc2) then
         k1=nsig ; k2=2*nsig ; k3=3*nsig ; k4=4*nsig ; k5=5*nsig
         k6=6*nsig ; k7=7*nsig ; k8=8*nsig ; k9=9*nsig ; k10=10*nsig
         k11=11*nsig ; k12=12*nsig ; k13=13*nsig 

      do k=1,nsig
         call nemsio_readrecv(gfile,trim(aeroname(1)),'mid layer',k,tmp,iret=iret)
              z4all(:,k)=tmp
         call nemsio_readrecv(gfile,trim(aeroname(2)),'mid layer',k,tmp,iret=iret)
              z4all(:,k1+k)=tmp
         call nemsio_readrecv(gfile,trim(aeroname(3)),'mid layer',k,tmp,iret=iret)
              z4all(:,k2+k)=tmp
         call nemsio_readrecv(gfile,trim(aeroname(4)),'mid layer',k,tmp,iret=iret)
              z4all(:,k3+k)=tmp
         call nemsio_readrecv(gfile,trim(aeroname(5)),'mid layer',k,tmp,iret=iret)
              z4all(:,k4+k)=tmp
         call nemsio_readrecv(gfile,trim(aeroname(6)),'mid layer',k,s1tmp,iret=iret)
         call nemsio_readrecv(gfile,trim(aeroname(7)),'mid layer',k,s2tmp,iret=iret)
              z4all(:,k5+k)=(s1tmp+s2tmp)
         call nemsio_readrecv(gfile,trim(aeroname(8)),'mid layer',k,tmp,iret=iret)
              z4all(:,k6+k)=tmp
         call nemsio_readrecv(gfile,trim(aeroname(9)),'mid layer',k,tmp,iret=iret)
              z4all(:,k7+k)=tmp
         call nemsio_readrecv(gfile,trim(aeroname(10)),'mid layer',k,tmp,iret=iret)
              z4all(:,k8+k)=tmp
         call nemsio_readrecv(gfile,trim(aeroname(11)),'mid layer',k,tmp,iret=iret)
              z4all(:,k9+k)=tmp
         call nemsio_readrecv(gfile,trim(aeroname(12)),'mid layer',k,tmp,iret=iret)
              z4all(:,k10+k)=tmp
         call nemsio_readrecv(gfile,trim(aeroname(13)),'mid layer',k,tmp,iret=iret)
              z4all(:,k11+k)=tmp
         call nemsio_readrecv(gfile,trim(aeroname(14)),'mid layer',k,tmp,iret=iret)
              z4all(:,k12+k)=tmp
         call nemsio_readrecv(gfile,trim(aeroname(15)),'mid layer',k,tmp,iret=iret)
              z4all(:,k13+k)=tmp
         if (iret/=0) then
            write(6,*) 'read in aerosol failed'
            call exit(999)
         end if 
      end do
      if (modelname=='ngac') z4all=z4all*1e+09 ! convert the units from kg/kg to µg/kg
    end if

    call mpi_scatterv(z4all,spec_send,disp_spec,mpi_rtype,&
       z41,spec_send(mm1),mpi_rtype,proc1,mpi_comm_world,ierror)
    call mpi_scatterv(z4all,spec_send,disp_spec,mpi_rtype,&
       z42,spec_send(mm1),mpi_rtype,proc2,mpi_comm_world,ierror)

!    write(6,*) 'mype is ',mype,'z41 shape is ',shape(z41)

    work1=zero ; work2=zero 

    do k=1,nsig1o
! Check: Dust1 level?
      if(nvar_id(k).eq.1) then ! SF
        kk=levs_id(k)
        if (kk.gt.0 .and. kk.le.nsig) then
           grid1=reshape(z41(:,k),(/nlon,nlat-2/))
           grid2=reshape(z42(:,k),(/nlon,nlat-2/))
          call unload_grid(grid1,work1(1,k))
          call unload_grid(grid2,work2(1,k))
        end if  !end if kk check

! Check: Dust2 level?
      else if(nvar_id(k).eq.2) then
        kk=levs_id(k)
        if (kk.gt.0 .and. kk.le.nsig) then
           grid1=reshape(z41(:,k),(/nlon,nlat-2/))
           grid2=reshape(z42(:,k),(/nlon,nlat-2/))
          call unload_grid(grid1,work1(1,k))
          call unload_grid(grid2,work2(1,k))
        end if  !end if kk check

! Check: Dust3 Level?
      else if(nvar_id(k).eq.3) then ! Temp
        kk=levs_id(k)
        if (kk.gt.0 .and. kk.le.nsig) then
           grid1=reshape(z41(:,k),(/nlon,nlat-2/))
           grid2=reshape(z42(:,k),(/nlon,nlat-2/))
           call unload_grid(grid1,work1(1,k))
           call unload_grid(grid2,work2(1,k))
        end if

! Check: Dust4 level?
      else if(nvar_id(k).eq.4) then ! Q
        kk=levs_id(k)
        if (kk.gt.0 .and. kk.le.nsig) then
           grid1=reshape(z41(:,k),(/nlon,nlat-2/))
           grid2=reshape(z42(:,k),(/nlon,nlat-2/))
          call unload_grid(grid1,work1(1,k))
          call unload_grid(grid2,work2(1,k))
        end if

! Check: Dust5 Level?
      else if(nvar_id(k).eq.5) then ! SF
        kk=levs_id(k)
        if (kk.gt.0 .and. kk.le.nsig) then
           grid1=reshape(z41(:,k),(/nlon,nlat-2/))
           grid2=reshape(z42(:,k),(/nlon,nlat-2/))
          call unload_grid(grid1,work1(1,k))
          call unload_grid(grid2,work2(1,k))
        end if

! Check: Seasalt1 Level?
      else if(nvar_id(k).eq.6) then ! SF
        kk=levs_id(k)
        if (kk.gt.0 .and. kk.le.nsig) then
           grid1=reshape(z41(:,k),(/nlon,nlat-2/))
           grid2=reshape(z42(:,k),(/nlon,nlat-2/))
          call unload_grid(grid1,work1(1,k))
          call unload_grid(grid2,work2(1,k))
        end if

! Check: Seasalt2 level ?
      else if(nvar_id(k).eq.7) then ! PS
        kk=levs_id(k)
        if (kk.gt.0 .and. kk.le.nsig) then
           grid1=reshape(z41(:,k),(/nlon,nlat-2/))
           grid2=reshape(z42(:,k),(/nlon,nlat-2/))
          call unload_grid(grid1,work1(1,k))
          call unload_grid(grid2,work2(1,k))
        end if

! Check: Seasalt3 level ?
      else if(nvar_id(k).eq.8) then ! PS
        kk=levs_id(k)
        if (kk.gt.0 .and. kk.le.nsig) then
           grid1=reshape(z41(:,k),(/nlon,nlat-2/))
           grid2=reshape(z42(:,k),(/nlon,nlat-2/))
          call unload_grid(grid1,work1(1,k))
          call unload_grid(grid2,work2(1,k))
        end if

! Check: Seasalt4 level ?
      else if(nvar_id(k).eq.9) then ! PS
        kk=levs_id(k)
        if (kk.gt.0 .and. kk.le.nsig) then
           grid1=reshape(z41(:,k),(/nlon,nlat-2/))
           grid2=reshape(z42(:,k),(/nlon,nlat-2/))
          call unload_grid(grid1,work1(1,k))
          call unload_grid(grid2,work2(1,k))
        end if

! Check: Sulfate level ?
      else if(nvar_id(k).eq.10) then ! PS
        kk=levs_id(k)
        if (kk.gt.0 .and. kk.le.nsig) then
           grid1=reshape(z41(:,k),(/nlon,nlat-2/))
           grid2=reshape(z42(:,k),(/nlon,nlat-2/))
          call unload_grid(grid1,work1(1,k))
          call unload_grid(grid2,work2(1,k))
        end if

! Check: OC phobic level ?
      else if(nvar_id(k).eq.11) then ! PS
        kk=levs_id(k)
        if (kk.gt.0 .and. kk.le.nsig) then
           grid1=reshape(z41(:,k),(/nlon,nlat-2/))
           grid2=reshape(z42(:,k),(/nlon,nlat-2/))
          call unload_grid(grid1,work1(1,k))
          call unload_grid(grid2,work2(1,k))
        end if

! Check: OC philic level ?
      else if(nvar_id(k).eq.12) then ! PS
        kk=levs_id(k)
        if (kk.gt.0 .and. kk.le.nsig) then
           grid1=reshape(z41(:,k),(/nlon,nlat-2/))
           grid2=reshape(z42(:,k),(/nlon,nlat-2/))
          call unload_grid(grid1,work1(1,k))
          call unload_grid(grid2,work2(1,k))
        end if

! Check: BC phobic level ?
      else if(nvar_id(k).eq.13) then ! PS
        kk=levs_id(k)
        if (kk.gt.0 .and. kk.le.nsig) then
           grid1=reshape(z41(:,k),(/nlon,nlat-2/))
           grid2=reshape(z42(:,k),(/nlon,nlat-2/))
          call unload_grid(grid1,work1(1,k))
          call unload_grid(grid2,work2(1,k))
        end if

! Check: BC philic level ?
      else if(nvar_id(k).eq.14) then ! PS
        kk=levs_id(k)
        if (kk.gt.0 .and. kk.le.nsig) then
           grid1=reshape(z41(:,k),(/nlon,nlat-2/))
           grid2=reshape(z42(:,k),(/nlon,nlat-2/))
          call unload_grid(grid1,work1(1,k))
          call unload_grid(grid2,work2(1,k))
        end if
      else ! No nsig1o level to process
!!        write(6,*) 'READPAIRS:  No Level to process, k,mype,levs_id,nvar_id = ',k,mype,levs_id(k),nvar_id(k)
      endif
    end do  !End do nsig1o levs

! CALL GRID2SUB HERE
    call grid2sub(work1,d1a,d2a,d3a,d4a,d5a,s1a,s2a,s3a,s4a,so4a,oc1a,oc2a,bc1a,bc2a)
    call grid2sub(work2,d1b,d2b,d3b,d4b,d5b,s1b,s2b,s3b,s4b,so4b,oc1b,oc2b,bc1b,bc2b)

! Write out the grids
    write(filunit1) d1a,d2a,d3a,d4a,d5a,s1a,s2a,s3a,s4a,so4a,oc1a,oc2a,bc1a,bc2a
    write(filunit2) d1b,d2b,d3b,d4b,d5b,s1b,s2b,s3b,s4b,so4b,oc1b,oc2b,bc1b,bc2b

!  write(1000+mype,*) s4a,s4b
!  write(1100+mype,*) so4a,so4b

  end do   ! END DO LOOP OVER CASES
  close(filunit1)
  close(filunit2)

  call mpi_barrier(mpi_comm_world,iret2)

  return
end subroutine readpairs_aerosol

