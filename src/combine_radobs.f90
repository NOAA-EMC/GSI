subroutine combine_radobs(mype_sub,mype_root,&
     npe_sub,mpi_comm_sub,nele,itxmax,nread,ndata,&
     data_all,data_crit)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    combine_radobs        merge input from multiple tasks
!   prgmmr: treadon          org: np23                date: 2006-06-19
!
! abstract:  This routine combines observation from multile tasks
!            into a single data array.
!
! program history log:
!   2006-06-19  treadon
!   2008-06-21  derber - introduce new algorithm for merging data
!
!   input argument list:
!     mype_sub - mpi task id for mpi_comm_sub
!     mype_root - mpi task id for task which combines data
!     npe_sub   - number of tasks in mpi_comm_sub
!     mpi_comm_sub  - sub-communicator
!     nele     - total number of data elements
!     itxmax   - maximum number of observations
!     data_all - observation data array
!     data_crit- array containing observation "best scores"
!     nread    - task specific number of obesrvations read from data file
!     ndata    - task specific number of observations keep for assimilation
!
!   output argument list:
!     nread    - total number of observations read from data file (mype_root)
!     ndata    - total number of observations keep for assimilation (mype_root)
!     data_all - merged observation data array (mype_root)
!     data_crit- merged array containing observation "best scores" (mype_root)
!     
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,i_kind
  use constants, only: zero,izero,ione
  use mpimod, only: ierror,mpi_rtype,mpi_itype,mpi_sum,mpi_min
  implicit none

! Declare passed variables
  integer(i_kind)                    ,intent(in   ) :: mype_sub
  integer(i_kind)                    ,intent(in   ) :: mype_root
  integer(i_kind)                    ,intent(in   ) :: npe_sub,itxmax
  integer(i_kind)                    ,intent(in   ) :: nele
  integer(i_kind)                    ,intent(in   ) :: mpi_comm_sub
  integer(i_kind)                    ,intent(inout) :: nread,ndata
  real(r_kind),dimension(itxmax)     ,intent(inout) :: data_crit
  real(r_kind),dimension(nele,itxmax),intent(inout) :: data_all

! Declare local variables
  integer(i_kind):: i,k,l,ndata1,ndata2
  integer(i_kind):: ncounts,ncounts1

  real(r_kind),dimension(itxmax):: data_crit_min
  real(r_kind),dimension(nele,itxmax):: data_all_in

  ndata=izero
  if(npe_sub > ione)then
!    Determine total number of data read and retained.
     ncounts=nread
     call mpi_allreduce(ncounts,ncounts1,ione,mpi_itype,mpi_sum,mpi_comm_sub,ierror)

!    Set total number of observations summed over all tasks and
!    construct starting location of subset in reduction array

     nread=izero
     if (mype_sub==mype_root) nread = ncounts1
     if (ncounts1 == izero)return

!    Allocate arrays to hold data

!    gather arrays over all tasks in mpi_comm_sub.  Reduction result
!    is only needed on task mype_root
     call mpi_allreduce(data_crit,data_crit_min,itxmax,mpi_rtype,mpi_min,mpi_comm_sub,ierror)

     ndata=izero
     ndata1=izero
     data_all_in=zero
     do k=1,itxmax
        if(data_crit_min(k) < 5.e9_r_kind)then
           ndata=ndata+ione
           if(data_crit_min(k) /= data_crit(k)) then
              data_crit(ndata)=1.e10_r_kind
              do l=1,nele
                 data_all_in(l,ndata)=zero
              end do
           else
              ndata1=ndata1+ione
              data_crit(ndata)=data_crit(k)
              do l=1,nele
                 data_all_in(l,ndata)=data_all(l,k)
              end do
           end if
        end if
     end do
     call mpi_allreduce(ndata1,ndata2,ione,mpi_itype,mpi_sum,mpi_comm_sub,ierror)

!    Following code only in the unlikely circumstance that 2 min crit's in one grid box are identical
     if(ndata /= ndata2)then
        do i=1,ndata
           data_crit(i)=data_crit(i)+float(mype_sub)
        end do
        call mpi_allreduce(data_crit,data_crit_min,ndata,mpi_rtype,mpi_min,mpi_comm_sub,ierror)

        do k=1,ndata
           if(data_crit_min(k) < 5.e9_r_kind)then
              if(data_crit_min(k) /= data_crit(k)) then
                 do l=1,nele
                    data_all_in(l,k)=zero
                 end do
              end if
           end if
        end do
      
     end if

!    get all data on process mype_root
     call mpi_reduce(data_all_in,data_all,nele*ndata,mpi_rtype,mpi_sum,&
          mype_root,mpi_comm_sub,ierror)
  else

     if(nread <= izero)return
     do k=1,itxmax
        if(data_crit(k) < 1.e9_r_kind)then
           ndata=ndata+ione
           do l=1,nele
              data_all(l,ndata)=data_all(l,k)
           end do
        end if
     end do
  end if

! End of routine
  return
end subroutine combine_radobs
