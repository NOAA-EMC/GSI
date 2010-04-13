subroutine antest_maps0(mype,theta0f,z0f)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    antest_maps0
!   prgmmr:
!
! abstract: this routine creates output maps of background error correlations
!
! program history log:
!   2009-09-18  lueken - added subprogram doc block
!   2010-03-30  zhu    - use nvars from control_vectors
!
!   input argument list:
!    mype
!    theta0f
!    z0f
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

  use kinds, only: r_kind,i_kind,r_single
  use anberror, only: kvar_start,kvar_end,var_names,pf2aP1,indices
  use gridmod, only: nsig,nsig1o,nlon,nlat,istart,jstart,lat2,lon2,twodvar_regional
  use constants, only: zero_single,izero,ione,zero,one,rd_over_cp
  use mpimod, only: ierror,mpi_real4,mpi_real8,mpi_sum,mpi_comm_world
  use guess_grids, only: ges_tv,ges_z,ntguessig,ges_prsl
  use fgrid2agrid_mod, only: fgrid2agrid
  use control_vectors, only: nvars
  implicit none

  integer(i_kind),intent(in   ) :: mype
  real(r_single) ,intent(in   ) :: theta0f(pf2aP1%nlatf,pf2aP1%nlonf,nsig1o)
  real(r_single) ,intent(in   ) :: z0f(pf2aP1%nlatf,pf2aP1%nlonf,nsig1o)

  real(r_kind),dimension(nlat,nlon,nsig1o):: hwork
  real(r_kind) tempf(nlat,nlon),tempc(pf2aP1%nlatf,pf2aP1%nlonf)
  real(r_single) outwork(nlon,nlat),outwork0(nlon,nlat)
  character(80) ref_plotcor
  character(80) var_plotcor
  integer(i_kind) i_plotcor,j_plotcor,k_plotcor

  real(r_kind)h00,h000
  integer(i_kind) lunin,i,j,k,ivar,iglob,jglob,ivar_plot,k_plot
  integer(i_kind) it,mm1
  real(r_kind),parameter:: r100=100.0_r_kind

  integer(i_kind):: iref, idxy

!*********************************************************************
!          variable names expected for var_plotcor are
!
!    st  -- stream function
!    vp  -- velocity potential
!    ps  -- surface pressure
!    tv  -- virtual temperature
!    q   -- specific humidity
!    oz  -- ozone
!    sst -- sea surface temperature
!    stl -- skin temp over land
!    sti -- skin temp over ice
!    cw  -- cloud water
!*********************************************************************
! Make choices here!
! i_plotcor=500_i_kind
! j_plotcor=500_i_kind
! k_plotcor=25_i_kind
  var_plotcor='st'
!Note: Must call this subroutine from anprewgt_reg.f90
!Make sure statement has been uncommented!
! End of choice section
!*********************************************************************

  if(twodvar_regional) then
     idxy=144_i_kind
  else
     idxy=36_i_kind
  end if

  do iref=1,5 !--- element loop start --------------------------------------------------

     if     (iref==ione)     then ; var_plotcor='st'
     else if(iref==2_i_kind) then ; var_plotcor='vp'
     else if(iref==3_i_kind) then ; var_plotcor='tv'
     else if(iref==4_i_kind) then ; var_plotcor='ps'
     else if(iref==5_i_kind) then ; var_plotcor='q'
     end if

!    ref_plotcor='tv'
     ref_plotcor=var_plotcor
     it=ntguessig
     lunin=ione
     if(mype==izero) then
        open(lunin,file="cormaps_"//trim(var_plotcor),form='unformatted')
        rewind lunin
     end if
     ivar_plot=izero
     do ivar=1,nvars
        if(trim(var_names(ivar))==trim(var_plotcor)) then
           ivar_plot=ivar
           exit
        end if
     end do
     if(ivar_plot==izero) then
        write(0,*)' in antest_maps0, variable ',trim(var_plotcor),'  not found.  program stops'
        call mpi_finalize(ierror)
        stop
     end if

     hwork=zero
     if( var_plotcor=='ps' .or. twodvar_regional ) then
        k_plotcor=ione
        k_plot=kvar_start(ivar_plot)+k_plotcor-ione
        if(k_plot>=indices%kps.and.k_plot<=indices%kpe) then
           do j_plotcor=1,nlon,idxy
              do i_plotcor=1,nlat,idxy
                 hwork(i_plotcor,j_plotcor,k_plot-indices%kps+ione)=one
              end do
           end do
        end if
     else
        do k_plotcor=5,40,35
           k_plot=kvar_start(ivar_plot)+k_plotcor-ione
           if(k_plot>=indices%kps.and.k_plot<=indices%kpe) then
              do j_plotcor=1,nlon,idxy
                 do i_plotcor=1,nlat,idxy
                    hwork(i_plotcor,j_plotcor,k_plot-indices%kps+ione)=one
                 end do
              end do
           end if
        end do
     end if

     call ansmoothrf(hwork)
     if(mype==izero) write(lunin) ref_plotcor,var_plotcor,j_plotcor,i_plotcor,k_plotcor, &
                  nlon,nlat,kvar_end(ivar_plot)-kvar_start(ivar_plot)+ione
     if(mype==izero) write(0,*) ' refvar= ',trim(ref_plotcor),' corvar= ',trim(var_plotcor), &
            '  i,j,k_plotcor =', j_plotcor,i_plotcor,k_plotcor, ' nlon,nlat,nsig=', &
                  nlon,nlat,kvar_end(ivar_plot)-kvar_start(ivar_plot)+ione

!---------------------in case we haven't normalized, divide by value of correlation point
     h00=zero
     j_plotcor=idxy
     i_plotcor=idxy
     if(k_plot>=indices%kps.and.k_plot<=indices%kpe) h00=hwork(i_plotcor,j_plotcor,k_plot-indices%kps+ione)
     call mpi_allreduce(h00,h000,ione,mpi_real8,mpi_sum,mpi_comm_world,ierror)
     hwork=hwork/h000

!     output original pot temp  (slow way to get full 2d field)  -- this is reference field

     mm1=mype+ione
     do k=1,nsig
        outwork=zero_single
        do j=2,lon2-ione
           jglob=jstart(mm1)-2_i_kind+j
           do i=2,lat2-ione
              iglob=istart(mm1)-2_i_kind+i
              outwork(jglob,iglob)=ges_tv(i,j,k,it)/(ges_prsl(i,j,k ,it)/r100)**rd_over_cp
           end do
        end do
        call mpi_reduce(outwork,outwork0,nlon*nlat,mpi_real4,mpi_sum,izero,mpi_comm_world,ierror)
        if(mype==izero) write(lunin) outwork0
     end do

!             output "smoothed pot temp"

     do k=kvar_start(ivar_plot),kvar_end(ivar_plot)
        outwork=zero_single
        if(k>=indices%kps.and.k<=indices%kpe) then
           do j=1,pf2aP1%nlonf
              do i=1,pf2aP1%nlatf
                 tempc(i,j)=theta0f(i,j,k-indices%kps+ione)
              end do
           end do
           call fgrid2agrid(pf2aP1,tempc,tempf)
           do j=1,nlon
              do i=1,nlat
                 outwork(j,i)=tempf(i,j)
              end do
           end do
        end if
        call mpi_reduce(outwork,outwork0,nlon*nlat,mpi_real4,mpi_sum,izero,mpi_comm_world,ierror)
        if(mype==izero) write(lunin) outwork0
     end do

     do k=kvar_start(ivar_plot),kvar_end(ivar_plot)
        outwork=zero_single
        if(k>=indices%kps.and.k<indices%kpe) then
           do j=1,nlon
              do i=1,nlat
                 outwork(j,i)=hwork(i,j,k-indices%kps+ione)
              end do
           end do
        end if

!             very slow way to move field from local processor to processor 0

        call mpi_reduce(outwork,outwork0,nlon*nlat,mpi_real4,mpi_sum,izero,mpi_comm_world,ierror)
        if(mype==izero) write(lunin) outwork0
     end do

!     output original terrain (slow way to get full 2d field)  -- this is reference field

     mm1=mype+ione
     outwork=zero_single
     do j=2,lon2-ione
        jglob=jstart(mm1)-2_i_kind+j
        do i=2,lat2-ione
           iglob=istart(mm1)-2_i_kind+i
           outwork(jglob,iglob)=ges_z(i,j,it)
        end do
     end do
     call mpi_reduce(outwork,outwork0,nlon*nlat,mpi_real4,mpi_sum,izero,mpi_comm_world,ierror)
     if(mype==izero) write(lunin) outwork0

!             output "smoothed terrain"

     PRINT*,'IN ANPREWGT_REG,KPS,KPE=',indices%KPS,indices%KPE
     do k=1, 1  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!kvar_start(ivar_plot),kvar_end(ivar_plot)
        outwork=zero_single
        if(k>=indices%kps.and.k<=indices%kpe) then
           do j=1,pf2aP1%nlonf
              do i=1,pf2aP1%nlatf
                 tempc(i,j)=z0f(i,j,k)!  theta0f(i,j,k-indices%kps+ione)
              end do
           end do
           call fgrid2agrid(pf2aP1,tempc,tempf)
           do j=1,nlon
              do i=1,nlat
                 outwork(j,i)=tempf(i,j)
              end do
           end do
        end if
        call mpi_reduce(outwork,outwork0,nlon*nlat,mpi_real4,mpi_sum,izero,mpi_comm_world,ierror)
        if(mype==izero) write(lunin) outwork0
     end do

     close(lunin)
!    if(mype>-1000_i_kind) then
!       call mpi_finalize(i)
!       stop
!    end if

  end do !--- element loop end ---------------------------------------------------------

end subroutine antest_maps0

!-------------------------------------------------------------------------------------

subroutine antest_maps0_subdomain_option(mype,theta0f,z0f)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    antest_maps0_subdomain_option
!   prgmmr:
!
! abstract: this routine creates output maps of background error correlations 
!
! program history log:
!   2009-09-18  lueken - added subprogram doc block
!
!   input argument list:
!    mype
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

  use kinds, only: r_kind,i_kind,r_single
  use anberror, only: kvar_start,kvar_end,var_names,levs_jdvar,indices,pf2aP1
  use gridmod, only: nsig,nlon,nlat,istart,jstart,lat2,lon2
  use constants, only: zero_single,izero,ione,zero,one,rd_over_cp
  use mpimod, only: ierror,mpi_real4,mpi_real8,mpi_sum,mpi_comm_world
  use guess_grids, only: ges_tv,ges_z,ntguessig,ges_prsl
  use control_vectors, only: nvars

  implicit none

  integer(i_kind),intent(in   ) :: mype
  real(r_single) ,intent(in   ) :: theta0f(lat2,lon2,nsig)
  real(r_single) ,intent(in   ) :: z0f(lat2,lon2,nsig)

  real(r_kind),dimension(lat2,lon2,nsig):: twork,qwork,stwork,vpwork
  real(r_kind),dimension(lat2,lon2):: pwork
  real(r_single) outwork(nlon,nlat),outwork0(nlon,nlat)
  character(80) ref_plotcor
  character(80) var_plotcor
  character(80) plotname
  integer(i_kind) i_plotcor,j_plotcor,k_plotcor
  integer(i_kind) iloc,jloc,kloc

  real(r_kind) h00,h000
  integer(i_kind) lunin,i,j,k,ivar,iglob,jglob,ivar_plot,k_plot
  integer(i_kind) it,mm1
  real(r_kind),parameter:: r100=100.0_r_kind
  integer(i_kind) lvar
  integer(i_kind):: ips,ipe,jps,jpe,kps,kpe
  integer(i_kind):: nlatf,nlonf

  ips=indices%ips; ipe=indices%ipe
  jps=indices%jps; jpe=indices%jpe
  kps=indices%kps; kpe=indices%kpe
  nlatf=pf2aP1%nlatf
  nlonf=pf2aP1%nlonf

!*********************************************************************
!          variable names expected for var_plotcor are
!
!    st  -- stream function
!    vp  -- velocity potential
!    ps  -- surface pressure
!    tv  -- virtual temperature
!    q   -- specific humidity
!    oz  -- ozone
!    sst -- sea surface temperature
!    stl -- skin temp over land
!    sti -- skin temp over ice
!    cw  -- cloud water
!*********************************************************************
! Make choices here!
  i_plotcor=460_i_kind !440_i_kind!  430_i_kind
  j_plotcor=265_i_kind!  250_i_kind
  k_plotcor=ione
  iloc=i_plotcor-istart(mype+ione)+2_i_kind
  jloc=j_plotcor-jstart(mype+ione)+2_i_kind
!  var_plotcor='st'
!Note: Must call this subroutine from anprewgt_reg.f90
!Make sure statement has been uncommented!
! End of choice section
!*********************************************************************

  ref_plotcor='theta'
  it=ntguessig
  do 200 lvar=1,5
     if (lvar==ione)      var_plotcor='st'
     if (lvar==2_i_kind)  var_plotcor='vp'
     if (lvar==3_i_kind)  var_plotcor='ps'
     if (lvar==4_i_kind)  var_plotcor='tv'
     if (lvar==5_i_kind)  var_plotcor='q'
     lunin=ione
     if(mype==izero) then
!       open(lunin,file='cormaps_'//trim(var_plotcor),form='unformatted')
!       rewind lunin
     end if
     ivar_plot=izero
     do ivar=1,nvars
        if(trim(var_names(ivar))==trim(var_plotcor)) then
           ivar_plot=ivar
           exit
        end if
     end do
     if(ivar_plot==izero) then
        write(6,*)' in antest_maps0, variable ',trim(var_plotcor),'  not found.  program stops'
        call mpi_finalize(ierror)
        stop
     end if
     kloc=k_plotcor
     twork=zero
     pwork=zero
     qwork=zero
     stwork=zero
     vpwork=zero
     if(i_plotcor>=ips.and.i_plotcor<=ipe.and. &
        j_plotcor>=jps.and.j_plotcor<=jpe) then
        if(ivar_plot==ione)     stwork(iloc,jloc,kloc)=one
        if(ivar_plot==2_i_kind) vpwork(iloc,jloc,kloc)=one
        if(ivar_plot==3_i_kind) pwork(iloc,jloc)=one
        if(ivar_plot==4_i_kind) twork(iloc,jloc,kloc)=one
        if(ivar_plot==5_i_kind) qwork(iloc,jloc,kloc)=one
     end if


     call ansmoothrf_reg_subdomain_option(twork,pwork,qwork,stwork,vpwork)
 !   if(mype==izero) write(lunin) ref_plotcor,var_plotcor,j_plotcor,i_plotcor,k_plotcor, &
 !                nlon,nlat,kvar_end(ivar_plot)-kvar_start(ivar_plot)+ione
     if(mype==izero) write(6,*) ' refvar= ',trim(ref_plotcor),' corvar= ',trim(var_plotcor), &
            '  i,j,k_plotcor =', j_plotcor,i_plotcor,k_plotcor, ' nlon,nlat,nsig=', &
                  nlon,nlat,kvar_end(ivar_plot)-kvar_start(ivar_plot)+ione

    !---------------------in case we haven't normalized, divide by value of correlation point
     h00=zero
     if(i_plotcor>=ips.and.i_plotcor<=ipe.and. &
        j_plotcor>=jps.and.j_plotcor<=jpe) then
        if(ivar_plot==ione)     h00=stwork(iloc,jloc,kloc)
        if(ivar_plot==2_i_kind) h00=vpwork(iloc,jloc,kloc)
        if(ivar_plot==3_i_kind) h00=pwork(iloc,jloc)
        if(ivar_plot==4_i_kind) h00=twork(iloc,jloc,kloc)
        if(ivar_plot==5_i_kind) h00=qwork(iloc,jloc,kloc)
     end if
     call mpi_allreduce(h00,h000,ione,mpi_real8,mpi_sum,mpi_comm_world,ierror)
     stwork=stwork/h000
     vpwork=vpwork/h000
     pwork=pwork/h000
     twork=twork/h000
     qwork=qwork/h000


!     output original pot temp  (slow way to get full 2d field)  -- this is reference field

     mm1=mype+ione
     do k=1,nsig
        outwork=zero_single
        do j=2,lon2-ione
           jglob=jstart(mm1)-2_i_kind+j
           do i=2,lat2-ione
              iglob=istart(mm1)-2_i_kind+i
              outwork(jglob,iglob)=ges_tv(i,j,k,it)/(ges_prsl(i,j,k ,it)/r100)**rd_over_cp
           end do
        end do
        call mpi_reduce(outwork,outwork0,nlon*nlat,mpi_real4,mpi_sum,izero,mpi_comm_world,ierror)
        !if(mype==izero) write(lunin) outwork0
        if(mype==izero) call outgrads1(outwork0,nlon,nlat,'theta')
     end do

!             output "smoothed pot temp"

     do k=1,nsig
        outwork=zero_single
        do j=2,lon2-ione
           jglob=jstart(mm1)-2_i_kind+j
           do i=2,lat2-ione
              iglob=istart(mm1)-2_i_kind+i
              outwork(jglob,iglob)=theta0f(i,j,k)
           end do
        end do
        call mpi_reduce(outwork,outwork0,nlon*nlat,mpi_real4,mpi_sum,izero,mpi_comm_world,ierror)
        !if(mype==izero) write(lunin) outwork0
        if(mype==izero) call outgrads1(outwork0,nlon,nlat,'sm_theta')
     end do

     do k=kvar_start(ivar_plot),kvar_end(ivar_plot)
        kloc=levs_jdvar(k)
        outwork=zero_single
        do j=2,lon2-ione
           jglob=jstart(mm1)-2_i_kind+j
           do i=2,lat2-ione
              iglob=istart(mm1)-2_i_kind+i
              if(ivar_plot==ione)     outwork(jglob,iglob)=stwork(i,j,kloc)
              if(ivar_plot==2_i_kind) outwork(jglob,iglob)=vpwork(i,j,kloc)
              if(ivar_plot==3_i_kind) outwork(jglob,iglob)=pwork(i,j)
              if(ivar_plot==4_i_kind) outwork(jglob,iglob)=twork(i,j,kloc)
              if(ivar_plot==5_i_kind) outwork(jglob,iglob)=qwork(i,j,kloc)
           end do
        end do
!             very slow way to move field from local processor to processor 0

        call mpi_reduce(outwork,outwork0,nlon*nlat,mpi_real4,mpi_sum,izero,mpi_comm_world,ierror)
        !if(mype==izero) write(lunin) outwork0
        if(mype==izero) then
           write(plotname,'("sub_",a)')trim(var_plotcor)
           call outgrads1(outwork0,nlon,nlat,plotname)
        end if
     end do

!     output original terrain (slow way to get full 2d field)  -- this is reference field

     mm1=mype+ione
     outwork=zero_single
     do j=2,lon2-ione
        jglob=jstart(mm1)-2_i_kind+j
        do i=2,lat2-ione
           iglob=istart(mm1)-2_i_kind+i
           outwork(jglob,iglob)=ges_z(i,j,it)
        end do
     end do
     call mpi_reduce(outwork,outwork0,nlon*nlat,mpi_real4,mpi_sum,izero,mpi_comm_world,ierror)
     !if(mype==izero) write(lunin) outwork0
     if(mype==izero) call outgrads1(outwork0,nlon,nlat,'z')

!             output "smoothed terrain"

     PRINT*,'IN ANPREWGT_REG_subdomain_option,KPS,KPE=',KPS,KPE

     do k=1,1
        outwork=zero_single
        do j=2,lon2-ione
           jglob=jstart(mm1)-2_i_kind+j
           do i=2,lat2-ione
              iglob=istart(mm1)-2_i_kind+i
              outwork(jglob,iglob)=z0f(i,j,k)
           end do
        end do
        call mpi_reduce(outwork,outwork0,nlon*nlat,mpi_real4,mpi_sum,izero,mpi_comm_world,ierror)
        !if(mype==izero) write(lunin) outwork0
        if(mype==izero) call outgrads1(outwork0,nlon,nlat,'sm_z')
     end do

     !close(lunin)
200 continue

end subroutine antest_maps0_subdomain_option

!-------------------------------------------------------------------------------------

subroutine outgrads1(f,nx,ny,label)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    outgrads1
!   prgmmr:
!
! abstract:
!
! program history log:
!   2009-09-18  lueken - added subprogram doc block
!
!   input argument list:
!    label
!    nx,ny
!    f
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block
  use kinds, only: i_kind,r_single
  use constants, only: ione
  implicit none

  character(*)   ,intent(in   ) :: label
  integer(i_kind),intent(in   ) :: nx,ny
  real(r_single) ,intent(in   ) :: f(nx,ny)

  integer(i_kind) i,l,next,last,np,ntime,ioutdat,ioutcor,koutmax
  real(r_single) rlonmap0,undef,dlonmap,pinc,startp,rlatmap0,dlatmap
  character(80) dsdes,dsdat
  character(80) datdes(1000)
  character(1) blank
  data blank/' '/
  data undef/-9.99e33_r_single/

  ioutcor=10_i_kind
  ioutdat=11_i_kind

  write(dsdes,'(a,".des")')trim(label)
  write(dsdat,'(a,".dat")')trim(label)
  open(unit=ioutcor,file=dsdes,form='formatted')
  open(unit=ioutdat,file=dsdat,form='unformatted')
  ntime=ione
  rlonmap0=1._r_single
  dlonmap=1._r_single
  rlatmap0=1._r_single
  dlatmap=1._r_single
  startp=1._r_single
  pinc=1._r_single
  koutmax=ione
  do i=1,1000
     write(datdes(i),'(80a1)')(blank,l=1,80)
  end do
  write(datdes(1),'("DSET ",a)')trim(dsdat)
  write(datdes(2),'("options big_endian sequential")')
  write(datdes(3),'("TITLE ",a)')trim(label)
  write(datdes(4),'("UNDEF ",e11.2)')undef
  write(datdes(5),'("XDEF ",i5," LINEAR ",f7.2,f7.2)')nx,rlonmap0,dlonmap
  write(datdes(6),'("YDEF ",i5," LINEAR ",f7.2,f7.2)')ny,rlatmap0,dlatmap
  next=7_i_kind
  write(datdes(next),'("ZDEF ",i5," LINEAR ",f7.2,f7.2)')np,startp,pinc
  next=next+ione
  write(datdes(next),'("TDEF ",i5," LINEAR 0Z23may1992 24hr")')koutmax
  next=next+ione
  write(datdes(next),'("VARS 1")')
  next=next+ione
  write(datdes(next),'("f   ",i5," 99 f   ")')np
  next=next+ione
  write(datdes(next),'("ENDVARS")')
  last=next
  write(ioutcor,'(a80)')(datdes(i),i=1,last)
  close(ioutcor)

  write(ioutdat) f
  close(ioutdat)

end subroutine outgrads1
