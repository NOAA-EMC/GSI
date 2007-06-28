module jfunc_tl
!$$$   module documentation block
!                .      .    .                                       .
! module:    jfunc_tl
!   prgmmr: yanqiu zhu          org: GMAO                date: 2005-03-28
!
! abstract: module containing tangent linear variables used in inner loop minimzation
!
! program history log:
!   2005-04-08  yanqiu zhu - tangent linear of jfunc
!
! Subroutines Included:
!   create_jfunc_tl       - allocate cost function arrays 
!   destroy_jfunc_tl      - deallocate cost function arrays
!   write_solution       - write guess solution
!   strip3               - strip off halo from subdomain arrays
!
! remarks: variable definitions below
!   def nclen      - length of control (x,y) vectors
!   dev nuvlen     - length of special control vector for u,v
!   def nval_levs  - number of 2d (x/y) variables
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,i_kind
  use jfunc, only: nclen, nuvlen, iguess, nval_levs

  real(r_kind),allocatable,dimension(:,:,:):: qgues_tl
  real(r_kind),allocatable,dimension(:):: xhatsave_tl,yhatsave_tl,xhatsave_r_tl,yhatsave_r_tl

contains

  subroutine create_jfunc_tl
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    create_jfunc_tl
!   prgmmr: yanqiu zhu          org: GMAO               date:  2005-04-08
!
! abstract: allocate memory for cost function tangent linear variables
!
! program history log:
!   2005-04-08  yanqiu zhu - tangent linear of create_jfunc
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp
!
!$$$
    use kinds, only: i_kind
    use constants, only: zero
    use gridmod, only: lat2,lon2,nsig
    use guess_grids, only: nfldsig
    implicit none

    integer(i_kind) i,j,k,l

    allocate(xhatsave_tl(nclen),yhatsave_tl(nclen))
    allocate(qgues_tl(lat2,lon2,nsig))

    do i=1,nclen
      xhatsave_tl(i)=zero
      yhatsave_tl(i)=zero
    end do

    if (iguess>0) then
       allocate(xhatsave_r_tl(nclen),yhatsave_r_tl(nclen))
       do i=1,nclen
          xhatsave_r_tl(i)=zero
          yhatsave_r_tl(i)=zero
       end do
    endif
    
    do k=1,nsig
       do j=1,lon2
          do i=1,lat2
             qgues_tl(i,j,k)=zero
          end do
       end do
    end do

    return
  end subroutine create_jfunc_tl
    
  subroutine destroy_jfunc_tl
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    destroy_jfunc_tl
!   prgmmr: yanqiu zhu          org: GMAO               date: 2005-04-08
!
! abstract: deallocate memory from cost function tangent linear variables
!
! program history log:
!   2005-04-08  yanqiu zhu - tangent linear of destroy_jfunc
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp
!
!$$$
    deallocate(xhatsave_tl,yhatsave_tl)
    deallocate(qgues_tl)

! NOTE:  xhatsave_r and yhatsave_r are deallocated in
!        pcgsoi following transfer of their contents
!        to xhatsave and yhatsave.  The deallocate is
!        releases this memory since it is no longer needed.

    return
  end subroutine destroy_jfunc_tl


  subroutine write_solution(xhats,xhatsuv,filename,mype)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    write_guess_solution
!   prgmmr: yanqiu zhu          org: GMAO               date:  2005-04-11
!
! abstract: write out solution (not from spectral forecast)
!
! program history log:
!   2005-04-11  yanqiu zhu - modify based on write_guess_solution
!   2006-04-06  middlecoff - changed filename unit (lnout) from 61 
!                            to lendian_out so it can be set to little endian.
!
!   input argument list:
!     mype   - mpi task id
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp
!
!$$$
    use kinds, only: r_kind,i_kind
    use mpimod, only: ierror, mpi_comm_world, mpi_rtype
    use gridmod, only: ijn,latlon11,displs_g,ltosj,ltosi,nsig,&
         nlat,nlon,lat1,lon1,itotsub,iglobal
    use obsmod, only: iadate
    use gsi_io, only: lendian_out
    implicit none

    character*255 filename
    integer(i_kind),intent(in):: mype

    integer(i_kind) i,j,k,mm1,mypew,kk,i1,i2
    real(r_kind),dimension(nclen):: xhats
    real(r_kind),dimension(nuvlen):: xhatsuv
    real(r_kind),dimension(lat1,lon1):: field
    real(r_kind),dimension(max(iglobal,itotsub)):: fieldx
!   real*4,dimension(nlat,nlon):: xhats_g
!   real*4,dimension(nlon,nlat):: xhats_g
    real(r_kind),dimension(nlon,nlat):: xhats_g

    mm1=mype+1
    mypew=0

! Write header record to output file
    if (mype==mypew) then
      open(lendian_out,file=filename,form='unformatted')
    endif

! Loop over levels.  Gather guess solution xhats and write to output
    do k=1,nval_levs
      i=(k-1)*latlon11 + 1
      call strip3(xhats(i),field)
      call mpi_gatherv(field(1,1),ijn(mm1),mpi_rtype,&
           fieldx,ijn,displs_g,mpi_rtype,mypew,&
           mpi_comm_world,ierror)

! Transfer to global arrays
      do j=1,nlat
        do i=1,nlon
          xhats_g(i,j)=0.0
        end do
      end do

      if (mype==mypew) then
        do kk=1,iglobal
          i1=ltosi(kk); i2=ltosj(kk)
          xhats_g(i2,i1)=fieldx(kk)
        end do
        write(lendian_out) xhats_g
      end if

    end do  !end do over nval_levs

! Loop over levels.  Gather guess solution xhatsuv and write to output
    do k=1, 2*nsig
      i=(k-1)*latlon11 + 1
      call strip3(xhatsuv(i),field)
      call mpi_gatherv(field(1,1),ijn(mm1),mpi_rtype,&
           fieldx,ijn,displs_g,mpi_rtype,mypew,&
           mpi_comm_world,ierror)

! Transfer to global arrays
      do j=1,nlat
        do i=1,nlon
          xhats_g(i,j)=0.0
        end do
      end do

      if (mype==mypew) then
        do kk=1,iglobal
          i1=ltosi(kk); i2=ltosj(kk)
          xhats_g(i2,i1)=fieldx(kk)
        end do
        write(lendian_out) xhats_g
      end if

    end do  !end do over 2*nsig levs

    if (mype==mypew) close (lendian_out)

    return
  end subroutine write_solution

    subroutine strip3(field_in,field_out)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    strip3
!   prgmmr: yanqiu zhu          org: GMAO                date: 2005-05-11
!
! abstract: strip off halo from subdomain array and put into output array
!
! program history log:
!   2005-05-11  yanqiu zhu, modify for one field based on strip2
!
!   input argument list:
!     field_in - subdomain field with halo
!
!   output argument list:
!     field_out - subdomain field with halo stripped
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp
!
!$$$
    use kinds, only: r_kind,i_kind
    use gridmod, only: lat1,lon1,lat2,lon2
    implicit none

    integer(i_kind) i,j,jp1
    real(r_kind),dimension(lat1,lon1):: field_out
    real(r_kind),dimension(lat2,lon2):: field_in

    do j=1,lon1
      jp1 = j+1
      do i=1,lat1
        field_out(i,j)=field_in(i+1,jp1)
      end do
    end do

    return
  end subroutine strip3

end module jfunc_tl
