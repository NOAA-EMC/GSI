subroutine antest_maps0(mype,theta0f,z0f)

!   this routine creates output maps of background error correlations

  use kinds, only: r_kind,i_kind,r_single
  use anberror, only: nvars,kvar_start,kvar_end,var_names,pf2aP1,indices
  use gridmod, only: nsig,nsig1o,nlon,nlat,istart,jstart,lat2,lon2,twodvar_regional
  use constants, only: zero,one,rd_over_cp
  use mpimod, only: npe,ierror,mpi_integer4,mpi_real4,mpi_real8,mpi_max,mpi_min,mpi_sum,mpi_comm_world
  use guess_grids, only: ges_tv,ges_z,ntguessig,ges_prsl
  use fgrid2agrid_mod, only: fgrid2agrid
  implicit none

  integer(i_kind) mype
  real(r_single) theta0f(pf2aP1%nlatf,pf2aP1%nlonf,nsig1o)
  real(r_single) z0f(pf2aP1%nlatf,pf2aP1%nlonf,nsig1o)

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

  integer:: iref, idxy

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
! i_plotcor=500
! j_plotcor=500
! k_plotcor=25
  var_plotcor='st'
!Note: Must call this subroutine from anprewgt_reg.f90
!Make sure statement has been uncommented!
! End of choice section
!*********************************************************************

if(twodvar_regional) then
  idxy=144
else
  idxy=36
end if

do iref=1,5 !--- element loop start --------------------------------------------------

  if     (iref==1) then ; var_plotcor='st'
  else if(iref==2) then ; var_plotcor='vp'
  else if(iref==3) then ; var_plotcor='tv'
  else if(iref==4) then ; var_plotcor='ps'
  else if(iref==5) then ; var_plotcor='q'
  end if

! ref_plotcor='tv'
  ref_plotcor=var_plotcor
  it=ntguessig
  lunin=1
  if(mype.eq.0) then
   open(lunin,file="cormaps_"//trim(var_plotcor),form='unformatted')
   rewind lunin
  end if
  ivar_plot=0
  do ivar=1,nvars
    if(trim(var_names(ivar)).eq.trim(var_plotcor)) then
      ivar_plot=ivar
      exit
    end if
  end do
  if(ivar_plot.eq.0) then
     write(0,*)' in antest_maps0, variable ',trim(var_plotcor),'  not found.  program stops'
     call mpi_finalize(ierror)
     stop
  end if

  hwork=zero
  if( var_plotcor=='ps' .or. twodvar_regional ) then
    k_plotcor=1
      k_plot=kvar_start(ivar_plot)+k_plotcor-1
      if(k_plot.ge.indices%kps.and.k_plot.le.indices%kpe) then
        do j_plotcor=1,nlon,idxy
        do i_plotcor=1,nlat,idxy
          hwork(i_plotcor,j_plotcor,k_plot-indices%kps+1)=one
        end do
        end do
      end if
  else
    do k_plotcor=5,40,35
      k_plot=kvar_start(ivar_plot)+k_plotcor-1
      if(k_plot.ge.indices%kps.and.k_plot.le.indices%kpe) then
        do j_plotcor=1,nlon,idxy
        do i_plotcor=1,nlat,idxy
          hwork(i_plotcor,j_plotcor,k_plot-indices%kps+1)=one
        end do
        end do
      end if
    end do
  end if

  call ansmoothrf(hwork)
  if(mype.eq.0) write(lunin) ref_plotcor,var_plotcor,j_plotcor,i_plotcor,k_plotcor, &
               nlon,nlat,kvar_end(ivar_plot)-kvar_start(ivar_plot)+1
  if(mype.eq.0) write(0,*) ' refvar= ',trim(ref_plotcor),' corvar= ',trim(var_plotcor), &
         '  i,j,k_plotcor =', j_plotcor,i_plotcor,k_plotcor, ' nlon,nlat,nsig=', &
               nlon,nlat,kvar_end(ivar_plot)-kvar_start(ivar_plot)+1

!---------------------in case we haven't normalized, divide by value of correlation point
  h00=zero
  j_plotcor=idxy
  i_plotcor=idxy
  if(k_plot.ge.indices%kps.and.k_plot.le.indices%kpe) h00=hwork(i_plotcor,j_plotcor,k_plot-indices%kps+1)
  call mpi_allreduce(h00,h000,1,mpi_real8,mpi_sum,mpi_comm_world,ierror)
  hwork=hwork/h000

!     output original pot temp  (slow way to get full 2d field)  -- this is reference field

  mm1=mype+1
  do k=1,nsig
   outwork=0._r_single
   do j=2,lon2-1
     jglob=jstart(mm1)-2+j
     do i=2,lat2-1
       iglob=istart(mm1)-2+i
       outwork(jglob,iglob)=ges_tv(i,j,k,it)/(ges_prsl(i,j,k ,it)/r100)**rd_over_cp
     end do
   end do
   call mpi_reduce(outwork,outwork0,nlon*nlat,mpi_real4,mpi_sum,0,mpi_comm_world,ierror)
   if(mype.eq.0) write(lunin) outwork0
  end do

!             output "smoothed pot temp"

  do k=kvar_start(ivar_plot),kvar_end(ivar_plot)
   outwork=0._r_single
   if(k.ge.indices%kps.and.k.le.indices%kpe) then
     do j=1,pf2aP1%nlonf
      do i=1,pf2aP1%nlatf
       tempc(i,j)=theta0f(i,j,k-indices%kps+1)
      end do
     end do
     call fgrid2agrid(pf2aP1,tempc,tempf)
     do j=1,nlon
      do i=1,nlat
       outwork(j,i)=tempf(i,j)
      end do
     end do
   end if
   call mpi_reduce(outwork,outwork0,nlon*nlat,mpi_real4,mpi_sum,0,mpi_comm_world,ierror)
   if(mype.eq.0) write(lunin) outwork0
  end do

  do k=kvar_start(ivar_plot),kvar_end(ivar_plot)
   outwork=0._r_single
   if(k.ge.indices%kps.and.k.le.indices%kpe) then
    do j=1,nlon
     do i=1,nlat
      outwork(j,i)=hwork(i,j,k-indices%kps+1)
     end do
    end do
   end if

!             very slow way to move field from local processor to processor 0

   call mpi_reduce(outwork,outwork0,nlon*nlat,mpi_real4,mpi_sum,0,mpi_comm_world,ierror)
   if(mype.eq.0) write(lunin) outwork0
  end do

!     output original terrain (slow way to get full 2d field)  -- this is reference field

   mm1=mype+1
   outwork=0._r_single
   do j=2,lon2-1
     jglob=jstart(mm1)-2+j
     do i=2,lat2-1
       iglob=istart(mm1)-2+i
       outwork(jglob,iglob)=ges_z(i,j,it)
     end do
   end do
   call mpi_reduce(outwork,outwork0,nlon*nlat,mpi_real4,mpi_sum,0,mpi_comm_world,ierror)
   if(mype.eq.0) write(lunin) outwork0

!             output "smoothed terrain"

  PRINT*,'IN ANPREWGT_REG,KPS,KPE=',indices%KPS,indices%KPE
  do k=1, 1  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!kvar_start(ivar_plot),kvar_end(ivar_plot)
   outwork=0._r_single
   if(k.ge.indices%kps.and.k.le.indices%kpe) then
     do j=1,pf2aP1%nlonf
      do i=1,pf2aP1%nlatf
       tempc(i,j)=z0f(i,j,k)!  theta0f(i,j,k-indices%kps+1)
      end do
     end do
     call fgrid2agrid(pf2aP1,tempc,tempf)
     do j=1,nlon
      do i=1,nlat
       outwork(j,i)=tempf(i,j)
      end do
     end do
   end if
   call mpi_reduce(outwork,outwork0,nlon*nlat,mpi_real4,mpi_sum,0,mpi_comm_world,ierror)
   if(mype.eq.0) write(lunin) outwork0
  end do

  close(lunin)
!          if(mype.gt.-1000) then
!             call mpi_finalize(i)
!             stop
!          end if

end do !--- element loop end ---------------------------------------------------------

end subroutine antest_maps0

!-------------------------------------------------------------------------------------

subroutine antest_maps0_subdomain_option(mype,theta0f,z0f)

!   this routine creates output maps of background error correlations

  use kinds, only: r_kind,i_kind,r_single
  use anberror, only: nvars,kvar_start,kvar_end,var_names,levs_jdvar,indices,pf2aP1
  use gridmod, only: nsig,nlon,nlat,istart,jstart,lat2,lon2
  use constants, only: zero,one,rd_over_cp
  use mpimod, only: npe,ierror,mpi_integer4,mpi_real4,mpi_real8,mpi_max,mpi_min,mpi_sum,mpi_comm_world
  use guess_grids, only: ges_tv,ges_z,ntguessig,ges_prsl

  implicit none

  integer(i_kind) mype
  real(r_single) theta0f(lat2,lon2,nsig)
  real(r_single) z0f(lat2,lon2,nsig)

  real(r_kind),dimension(lat2,lon2,nsig):: twork,qwork,stwork,vpwork
  real(r_kind),dimension(lat2,lon2):: pwork
  real(r_kind) tempf(nlat,nlon),tempc(pf2aP1%nlatf,pf2aP1%nlonf)
  real(r_single) outwork(nlon,nlat),outwork0(nlon,nlat)
  character(80) ref_plotcor
  character(80) var_plotcor
  character(80) plotname
  integer(i_kind) i_plotcor,j_plotcor,k_plotcor
  integer(i_kind) iloc,jloc,kloc

  real(r_kind)h00,h000
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
  i_plotcor=460 !440!  430
  j_plotcor=265!  250
  k_plotcor=1
  iloc=i_plotcor-istart(mype+1)+2
  jloc=j_plotcor-jstart(mype+1)+2
!  var_plotcor='st'
!Note: Must call this subroutine from anprewgt_reg.f90
!Make sure statement has been uncommented!
! End of choice section
!*********************************************************************

  ref_plotcor='theta'
  it=ntguessig
do 200 lvar=1,5
  if (lvar.eq.1)  var_plotcor='st'
  if (lvar.eq.2)  var_plotcor='vp'
  if (lvar.eq.3)  var_plotcor='ps'
  if (lvar.eq.4)  var_plotcor='tv'
  if (lvar.eq.5)  var_plotcor='q'
  lunin=1
  if(mype.eq.0) then
!  open(lunin,file='cormaps_'//trim(var_plotcor),form='unformatted')
!  rewind lunin
  end if
  ivar_plot=0
  do ivar=1,nvars
    if(trim(var_names(ivar)).eq.trim(var_plotcor)) then
      ivar_plot=ivar
      exit
    end if
  end do
  if(ivar_plot.eq.0) then
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
  if(i_plotcor.ge.ips.and.i_plotcor.le.ipe.and. &
            j_plotcor.ge.jps.and.j_plotcor.le.jpe) then
    if(ivar_plot.eq.1) stwork(iloc,jloc,kloc)=one
    if(ivar_plot.eq.2) vpwork(iloc,jloc,kloc)=one
    if(ivar_plot.eq.3) pwork(iloc,jloc)=one
    if(ivar_plot.eq.4) twork(iloc,jloc,kloc)=one
    if(ivar_plot.eq.5) qwork(iloc,jloc,kloc)=one
  end if


  call ansmoothrf_reg_subdomain_option(twork,pwork,qwork,stwork,vpwork)
 !if(mype.eq.0) write(lunin) ref_plotcor,var_plotcor,j_plotcor,i_plotcor,k_plotcor, &
 !             nlon,nlat,kvar_end(ivar_plot)-kvar_start(ivar_plot)+1
  if(mype.eq.0) write(6,*) ' refvar= ',trim(ref_plotcor),' corvar= ',trim(var_plotcor), &
         '  i,j,k_plotcor =', j_plotcor,i_plotcor,k_plotcor, ' nlon,nlat,nsig=', &
               nlon,nlat,kvar_end(ivar_plot)-kvar_start(ivar_plot)+1

    !---------------------in case we haven't normalized, divide by value of correlation point
        h00=zero
        if(i_plotcor.ge.ips.and.i_plotcor.le.ipe.and. &
            j_plotcor.ge.jps.and.j_plotcor.le.jpe) then
          if(ivar_plot.eq.1) h00=stwork(iloc,jloc,kloc)
          if(ivar_plot.eq.2) h00=vpwork(iloc,jloc,kloc)
          if(ivar_plot.eq.3) h00=pwork(iloc,jloc)
          if(ivar_plot.eq.4) h00=twork(iloc,jloc,kloc)
          if(ivar_plot.eq.5) h00=qwork(iloc,jloc,kloc)
        end if
        call mpi_allreduce(h00,h000,1,mpi_real8,mpi_sum,mpi_comm_world,ierror)
        stwork=stwork/h000
        vpwork=vpwork/h000
        pwork=pwork/h000
        twork=twork/h000
        qwork=qwork/h000


!     output original pot temp  (slow way to get full 2d field)  -- this is reference field

  mm1=mype+1
  do k=1,nsig
   outwork=0._r_single
   do j=2,lon2-1
     jglob=jstart(mm1)-2+j
     do i=2,lat2-1
       iglob=istart(mm1)-2+i
       outwork(jglob,iglob)=ges_tv(i,j,k,it)/(ges_prsl(i,j,k ,it)/r100)**rd_over_cp
     end do
   end do
   call mpi_reduce(outwork,outwork0,nlon*nlat,mpi_real4,mpi_sum,0,mpi_comm_world,ierror)
  !if(mype.eq.0) write(lunin) outwork0
   if(mype.eq.0) call outgrads1(outwork0,nlon,nlat,'theta')
  end do

!             output "smoothed pot temp"

  do k=1,nsig
   outwork=0._r_single
     do j=2,lon2-1
      jglob=jstart(mm1)-2+j
      do i=2,lat2-1
       iglob=istart(mm1)-2+i
       outwork(jglob,iglob)=theta0f(i,j,k)
      end do
     end do
   call mpi_reduce(outwork,outwork0,nlon*nlat,mpi_real4,mpi_sum,0,mpi_comm_world,ierror)
  !if(mype.eq.0) write(lunin) outwork0
   if(mype.eq.0) call outgrads1(outwork0,nlon,nlat,'sm_theta')
  end do

  do k=kvar_start(ivar_plot),kvar_end(ivar_plot)
    kloc=levs_jdvar(k)
    outwork=0._r_single
    do j=2,lon2-1
      jglob=jstart(mm1)-2+j
      do i=2,lat2-1
       iglob=istart(mm1)-2+i
       if(ivar_plot.eq.1) outwork(jglob,iglob)=stwork(i,j,kloc)
       if(ivar_plot.eq.2) outwork(jglob,iglob)=vpwork(i,j,kloc)
       if(ivar_plot.eq.3) outwork(jglob,iglob)=pwork(i,j)
       if(ivar_plot.eq.4) outwork(jglob,iglob)=twork(i,j,kloc)
       if(ivar_plot.eq.5) outwork(jglob,iglob)=qwork(i,j,kloc)
     end do
    end do
!             very slow way to move field from local processor to processor 0

   call mpi_reduce(outwork,outwork0,nlon*nlat,mpi_real4,mpi_sum,0,mpi_comm_world,ierror)
  !if(mype.eq.0) write(lunin) outwork0
   if(mype.eq.0) then
      write(plotname,'("sub_",a)')trim(var_plotcor)
      call outgrads1(outwork0,nlon,nlat,plotname)
   end if
  end do

!     output original terrain (slow way to get full 2d field)  -- this is reference field

   mm1=mype+1
   outwork=0._r_single
   do j=2,lon2-1
     jglob=jstart(mm1)-2+j
     do i=2,lat2-1
       iglob=istart(mm1)-2+i
       outwork(jglob,iglob)=ges_z(i,j,it)
     end do
   end do
   call mpi_reduce(outwork,outwork0,nlon*nlat,mpi_real4,mpi_sum,0,mpi_comm_world,ierror)
  !if(mype.eq.0) write(lunin) outwork0
   if(mype.eq.0) call outgrads1(outwork0,nlon,nlat,'z')

!             output "smoothed terrain"

  PRINT*,'IN ANPREWGT_REG_subdomain_option,KPS,KPE=',KPS,KPE

  do k=1,1
   outwork=0._r_single
     do j=2,lon2-1
      jglob=jstart(mm1)-2+j
      do i=2,lat2-1
       iglob=istart(mm1)-2+i
       outwork(jglob,iglob)=z0f(i,j,k)
      end do
     end do
   call mpi_reduce(outwork,outwork0,nlon*nlat,mpi_real4,mpi_sum,0,mpi_comm_world,ierror)
 ! if(mype.eq.0) write(lunin) outwork0
   if(mype.eq.0) call outgrads1(outwork0,nlon,nlat,'sm_z')
  end do

 !close(lunin)
200 continue

end subroutine antest_maps0_subdomain_option

!-------------------------------------------------------------------------------------

subroutine outgrads1(f,nx,ny,label)

  character(*) label
  integer(4) nx,ny
  real(4) f(nx,ny)

  integer(4) i,l,next,last,np,ntime,ioutdat,ioutcor,koutmax
  real(4) rlonmap0,undef,dlonmap,pinc,startp,rlatmap0,dlatmap
  character(80) dsdes,dsdat
  character(80) datdes(1000)
  character(1) blank
  data blank/' '/
  data undef/-9.99e33/

         ioutcor=10
         ioutdat=11

         write(dsdes,'(a,".des")')trim(label)
         write(dsdat,'(a,".dat")')trim(label)
         open(unit=ioutcor,file=dsdes,form='formatted')
         open(unit=ioutdat,file=dsdat,form='unformatted')
         ntime=1
         rlonmap0=1.
         dlonmap=1.
         rlatmap0=1.
         dlatmap=1.
         startp=1.
         pinc=1.
         koutmax=1
         do i=1,1000
          write(datdes(i),'(80a1)')(blank,l=1,80)
         end do
         write(datdes(1),'("DSET ",a)')trim(dsdat)
         write(datdes(2),'("options big_endian sequential")')
         write(datdes(3),'("TITLE ",a)')trim(label)
         write(datdes(4),'("UNDEF ",e11.2)')undef
         write(datdes(5),'("XDEF ",i5," LINEAR ",f7.2,f7.2)')nx,rlonmap0,dlonmap
         write(datdes(6),'("YDEF ",i5," LINEAR ",f7.2,f7.2)')ny,rlatmap0,dlatmap
         next=7
         write(datdes(next),'("ZDEF ",i5," LINEAR ",f7.2,f7.2)')np,startp,pinc
         next=next+1
         write(datdes(next),'("TDEF ",i5," LINEAR 0Z23may1992 24hr")')koutmax
         next=next+1
         write(datdes(next),'("VARS 1")')
         next=next+1
         write(datdes(next),'("f   ",i5," 99 f   ")')np
         next=next+1
         write(datdes(next),'("ENDVARS")')
         last=next
         write(ioutcor,'(a80)')(datdes(i),i=1,last)
         close(ioutcor)

         write(ioutdat) f
         close(ioutdat)

end subroutine outgrads1
