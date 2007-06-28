subroutine combine_radobs(mype,mype_sub,mype_root,&
     npe_sub,mpi_comm_sub,nele,itxmax,nread,ndata,&
     data_all,data_crit,idata_itx)

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
!
!   input argument list:
!     mype     - mpi task id for mpi_comm_world
!     mype_sub - mpi task id for mpi_comm_sub
!     mype_root - mpi task id for task which combines data
!     npe_sub   - number of tasks in mpi_comm_sub
!     mpi_comm_sub  - sub-communicator
!     nele     - total number of data elements
!     itxmax   - maximum number of observations
!     data_all - observation data array
!     data_crit- array containing observation "best scores"
!     idata_itx- array containing thinning grid location of obs
!     nread    - task specific number of obesrvations read from data file
!     ndata    - task specific number of observations keep for assimilation
!
!   output argument list:
!     nread    - total number of observations read from data file (mype_root)
!     ndata    - total number of observations keep for assimilation (mype_root)
!     data_all - merged observation data array (mype_root)
!     data_crit- merged array containing observation "best scores" (mype_root)
!     idata_itx- merged array containing thinning grid location of obs (mype_root)
!     
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,i_kind
  use constants, only: zero,izero
  use mpimod, only: ierror,mpi_rtype,mpi_itype,mpi_sum
  implicit none

! Declare passed variables
  integer(i_kind),intent(in):: mype_sub,mype
  integer(i_kind),intent(in):: mype_root
  integer(i_kind),intent(in):: npe_sub,itxmax
  integer(i_kind),intent(in):: nele
  integer(i_kind),intent(in):: mpi_comm_sub
  integer(i_kind),intent(inout):: nread,ndata
  integer(i_kind),dimension(itxmax),intent(inout):: idata_itx
  real(r_kind),dimension(itxmax),intent(inout):: data_crit
  real(r_kind),dimension(nele,itxmax),intent(inout):: data_all



! Declare local variables
  logical:: add
  integer(i_kind):: i,j,k,kk,l,n
  integer(i_kind):: itx_sub,itx,ndata1,nele1,nele2
  integer(i_kind),dimension(npe_sub):: nstart
  integer(i_kind),dimension(npe_sub+2):: ncounts,ncounts1

  real(r_kind):: crit,crit_sub
  real(r_kind),allocatable,dimension(:,:):: data_all_sub,data_all_sub1


  nele1=nele+1
  nele2=nele+2

! Determine total number of data read and retained.
  do i=1,npe_sub
     ncounts(i)=izero
  end do
  ncounts(mype_sub+1)=ndata
  ncounts(npe_sub+1)=nread
  ncounts(npe_sub+2)=ndata
  call mpi_allreduce(ncounts,ncounts1,npe_sub+2,mpi_itype,mpi_sum,mpi_comm_sub,ierror)


! Set total number of observations summed over all tasks and
! construct array containing starting location of each subset
! in reduction array
  ndata1=ncounts1(npe_sub+2)
  nstart(1)=1
  do i=2,npe_sub
     nstart(i) = nstart(i-1) + ncounts1(i-1)
  end do


! Allocate arrays to hold data
  allocate(data_all_sub(nele2,ndata1),data_all_sub1(nele2,ndata1))
  do j=1,ndata1
     do i=1,nele2
        data_all_sub(i,j) =zero
     end do
  end do


! Load data from each task into reduction arrays
  do i=1,npe_sub
     if (i-1==mype_sub) then
        j=nstart(i)-1
        do n=1,ndata
           j=j+1
           do k=1,nele
              data_all_sub(k,j)=data_all(k,n)
           end do
           data_all_sub(nele1,j)=data_crit(n)
           data_all_sub(nele2,j)=idata_itx(n)+0.0000001_r_kind
        end do
     end if
  end do


! Reduce arrays over all tasks in mpi_comm_sub.  Reduction result
! is only needed on task mype_root
  call mpi_reduce(data_all_sub, data_all_sub1,nele2*ndata1,&
       mpi_rtype,mpi_sum,mype_root,mpi_comm_sub,ierror)



! Reset counters
  nread=0
  ndata=0

  
! A single task, mype_root, merges the all the data together
  if (mype_sub==mype_root) then

     nread = ncounts1(npe_sub+1)


!    Initialize "merge data" output arrays
     do k=1,itxmax
        do i=1,nele
           data_all(i,k)=9.99e33
        end do
        data_crit(k)=9.99e33
        idata_itx(k)=-999
     end do

!    Loop over task specific obs.  Retain "best"
!    observations from each task
     task_loop: do i=1,npe_sub

        ndata1 = ncounts1(i)

!       Loop over contents of scratch file just read in.
!       Retain best observation in each thinning grid box

        data_loop: do k=1,ndata1
           kk=nstart(i)-1+k
           itx_sub=nint(data_all_sub1(nele2,kk))
           crit_sub=data_all_sub1(nele1,kk)
           add  = .true.
           
!          Loop through array of "best" values.  Compare current
!          values with best values.  Replace best as needed.
           do j=1,ndata
              itx=idata_itx(j)
              crit=data_crit(j)
              if (itx_sub==itx) then
                 add = .false.
                 if (crit_sub<crit) then
                    do l=1,nele
                       data_all(l,j)=data_all_sub1(l,kk)
                    end do
                    idata_itx(j)=nint(data_all_sub1(nele2,kk))
                    data_crit(j)=data_all_sub1(nele1,kk)
                 endif
              endif
           end do
              
           if (add) then
              ndata=ndata + 1
              do l=1,nele
                 data_all(l,ndata) = data_all_sub1(l,kk)
              end do
              data_crit(ndata) = data_all_sub1(nele1,kk)
              idata_itx(ndata) = nint(data_all_sub1(nele2,kk))
           endif
           
        end do data_loop
     end do task_loop

! End of block for mype_root task
  endif


! Deallocate arrays
  deallocate(data_all_sub,data_all_sub1)


! End of routine
  return
end subroutine combine_radobs
