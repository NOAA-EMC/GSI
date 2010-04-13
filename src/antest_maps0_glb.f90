subroutine antest_maps0_glb(mype,theta0f,z0f,theta2f,z2f,theta3f,z3f)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    antest_maps0_glb
!   prgmmr:
!
! abstract: this routine creates output maps of background error correlations
!
! program history log:
!   2009-09-21  lueken - added subprogram doc block
!   2010-03-30  zhu    - use nvars from control_vectors
!
!   input argument list:
!    mype
!    theta0f
!    z0f
!    theta2f
!    z2f
!    theta3f
!    z3f
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

  use kinds, only: r_kind,i_kind,r_single
  use anberror, only: kvar_start,kvar_end,var_names,indices,pf2aP1,pf2aP2,pf2aP3
  use gridmod, only: nsig,nsig1o,nlon,nlat,istart,jstart,lat2,lon2
  use constants, only: zero_single,izero,ione,zero,one,rd_over_cp
  use mpimod, only: ierror,mpi_real4,mpi_real8,mpi_sum,mpi_comm_world
  use guess_grids, only: ges_tv,ges_z,ntguessig,ges_prsl
  use patch2grid_mod, only: vpatch2grid
  use control_vectors, only: nvars
  implicit none

  integer(i_kind),intent(in   ) :: mype
  real(r_single) ,intent(in   ) :: theta0f(pf2aP1%nlatf,pf2aP1%nlonf,nsig1o)
  real(r_single) ,intent(in   ) :: z0f(pf2aP1%nlatf,pf2aP1%nlonf,nsig1o)
  real(r_single) ,intent(in   ) :: theta2f(pf2aP2%nlatf,pf2aP2%nlonf,nsig1o)
  real(r_single) ,intent(in   ) :: z2f(pf2aP2%nlatf,pf2aP2%nlonf,nsig1o)
  real(r_single) ,intent(in   ) :: theta3f(pf2aP3%nlatf,pf2aP3%nlonf,nsig1o)
  real(r_single) ,intent(in   ) :: z3f(pf2aP3%nlatf,pf2aP3%nlonf,nsig1o)

  real(r_kind),dimension(nlat,nlon,nsig1o):: hwork
  real(r_kind) tempf(nlat,nlon), &
               tempc  (pf2aP1%nlatf,pf2aP1%nlonf), &
               tempcp2(pf2aP2%nlatf ,pf2aP2%nlonf ), &
               tempcp3(pf2aP3%nlatf ,pf2aP3%nlonf )
  real(r_single) outwork(nlon,nlat),outwork0(nlon,nlat)
  character(80) ref_plotcor
  character(80) var_plotcor
  integer(i_kind) i_plotcor,j_plotcor,k_plotcor

  real(r_kind) h00,h000
  integer(i_kind) lunin,i,j,k,ivar,iglob,jglob,ivar_plot,k_plot,ielm
  integer(i_kind) it,mm1
  real(r_kind),parameter:: r100=100.0_r_kind

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
! var_plotcor='st'
!Note: Must call this subroutine from anprewgt_reg.f90
!Make sure statement has been uncommented!
! End of choice section
!*********************************************************************

  do ielm=1,5
     select case (ielm)
        case(ione); var_plotcor='st'
        case(2)   ; var_plotcor='vp'
        case(3)   ; var_plotcor='tv'
        case(4)   ; var_plotcor='q'
        case(5)   ; var_plotcor='ps'
     end select

     ref_plotcor=var_plotcor
     it=ntguessig
     lunin=91_i_kind
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
        write(6,*)' in antest_maps0, variable ',trim(var_plotcor),'  not found.  program stops'
        call mpi_finalize(ierror)
        stop
     end if

     hwork=zero
     do k_plotcor=10,50,20
        if(var_plotcor=='ps') then
           k_plot=kvar_start(ivar_plot)
        else
           k_plot=kvar_start(ivar_plot)+k_plotcor-ione
        end if

        if(k_plot>=indices%kps.and.k_plot<=indices%kpe) then
           if( var_plotcor /= 'hires' ) then

              if(k_plotcor<50_i_kind.or.var_plotcor=='ps') then
                 do i_plotcor=nlat/8,nlat-nlat/8+ione,nlat/8
                    do j_plotcor= nlon/4,nlon,nlon/2
                       hwork(i_plotcor,j_plotcor,k_plot-indices%kps+ione)=one
                    end do
                 end do
                 do i_plotcor=nlat/6,nlat-nlat/6+ione,nlat/6
                    do j_plotcor=   1,nlon,nlon/2
                       hwork(i_plotcor,j_plotcor,k_plot-indices%kps+ione)=one
                    end do
                 end do
              else
                 do i_plotcor=nlat/4,nlat-nlat/4+ione,nlat/4
                    do j_plotcor= 1,nlon,nlon/4
                       hwork(i_plotcor,j_plotcor,k_plot-indices%kps+ione)=one
                    end do
                 end do
              end if

              i_plotcor=ione
              j_plotcor=ione
              hwork(i_plotcor,j_plotcor,k_plot-indices%kps+ione)=one

              i_plotcor=nlat
              j_plotcor=ione
              hwork(i_plotcor,j_plotcor,k_plot-indices%kps+ione)=one

           else

              do j_plotcor= 1,nlon,nlon/16
                 do i_plotcor=(nlat+2_i_kind)/8,nlat-(nlat+2_i_kind)/8-ione,(nlat+2_i_kind)/16
                    hwork(i_plotcor,j_plotcor,k_plot-indices%kps+ione)=one
                 end do
              end do

              i_plotcor=ione
              j_plotcor=ione
              hwork(i_plotcor,j_plotcor,k_plot-indices%kps+ione)=one
 
              i_plotcor=nlat
              j_plotcor=ione
              hwork(i_plotcor,j_plotcor,k_plot-indices%kps+ione)=one
 
              i_plotcor=(nlat+2_i_kind)/32
              do j_plotcor= 1, nlon,nlon/4
                 hwork(i_plotcor,j_plotcor,k_plot-indices%kps+ione)=one
              end do
              i_plotcor=nlat-(nlat+2_i_kind)/32
              do j_plotcor= 1, nlon,nlon/4
                 hwork(i_plotcor,j_plotcor,k_plot-indices%kps+ione)=one
              end do

              i_plotcor=(nlat+2_i_kind)/16
              do j_plotcor= 1, nlon,nlon/8
                 hwork(i_plotcor,j_plotcor,k_plot-indices%kps+ione)=one
              end do
              i_plotcor=nlat-(nlat+2_i_kind)/16
              do j_plotcor= 1, nlon,nlon/8
                 hwork(i_plotcor,j_plotcor,k_plot-indices%kps+ione)=one
              end do

           end if
        end if
        if(var_plotcor=='ps') exit
     end do

     call ansmoothrf(hwork)

     if(mype==izero) write(lunin) ref_plotcor,var_plotcor,j_plotcor,i_plotcor,k_plotcor, &
                  nlon,nlat,kvar_end(ivar_plot)-kvar_start(ivar_plot)+ione

     if(mype==izero) write(6,*) ' refvar= ',trim(ref_plotcor),' corvar= ',trim(var_plotcor), &
            '  i,j,k_plotcor =', j_plotcor,i_plotcor,k_plotcor, ' nlon,nlat,nsig=', &
                  nlon,nlat,kvar_end(ivar_plot)-kvar_start(ivar_plot)+ione

     !---------------------in case we haven't normalized, divide by value of correlation point

     h00=zero
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
           do j=1,pf2aP2%nlonf
              do i=1,pf2aP2%nlatf
                 tempcp2(i,j)=theta2f(i,j,k-indices%kps+ione)
              end do
           end do
           do j=1,pf2aP3%nlonf
              do i=1,pf2aP3%nlatf
                 tempcp3(i,j)=theta3f(i,j,k-indices%kps+ione)
              end do
           end do
           call vpatch2grid(tempf,tempc,tempcp2,tempcp3)
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
        if(k>=indices%kps.and.k<=indices%kpe) then
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

     PRINT*,'IN ANPREWGT,KPS,KPE=',indices%KPS,indices%KPE
     do k=1, 1  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!kvar_start(ivar_plot),kvar_end(ivar_plot)
        outwork=zero_single
        if(k>=indices%kps.and.k<=indices%kpe) then
           do j=1,pf2aP1%nlonf
              do i=1,pf2aP1%nlatf
                 tempc(i,j)=z0f(i,j,k)!  theta0f(i,j,k-indices%kps+ione)
              end do
           end do
           do j=1,pf2aP2%nlonf
              do i=1,pf2aP2%nlatf
                 tempcp2(i,j)=z2f(i,j,k)!  theta0f(i,j,k-indices%kps+ione)
              end do
           end do
           do j=1,pf2aP3%nlonf
              do i=1,pf2aP3%nlatf
                 tempcp3(i,j)=z3f(i,j,k)!  theta0f(i,j,k-indices%kps+ione)
              end do
           end do
           call vpatch2grid(tempf,tempc,tempcp2,tempcp3)
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

  end do

end subroutine antest_maps0_glb

!-------------------------------------------------------------------------------------
