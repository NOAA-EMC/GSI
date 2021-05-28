module scatter_chunks_efsoi

use mpisetup, only: numproc, nproc, mpi_real4
use mpimod, only : mpi_comm_world
use kinds, only: i_kind, r_single, r_kind
use loadbal_efsoi, only: npts_max, numptsperproc, indxproc
use params, only: nanals
use gridio_efsoi

! Fill in documentation here
implicit none
private
public :: scatter_chunks_ob_impact
real(r_kind),public, allocatable, dimension(:,:,:) :: anal_chunk 
real(r_single),public, allocatable, dimension(:,:) :: ensmean_chunk 
real(r_single),public, allocatable, dimension(:,:) :: fcerror_chunk, analmean_chunk
contains

subroutine scatter_chunks_ob_impact
! distribute chunks from grdin (read in controlvec) according to
! decomposition from load_balance
use statevec_efsoi, only: grdin,grdin3,grdin5
use gridio_efsoi, only: ncdim
implicit none

integer(i_kind), allocatable, dimension(:) :: scounts, displs, rcounts
real(r_single), allocatable, dimension(:) :: sendbuf,recvbuf,sendbuf2,recvbuf2
integer(i_kind) :: np, nn, n, nanal, i, ierr


allocate(scounts(0:numproc-1))
allocate(displs(0:numproc-1))
allocate(rcounts(0:numproc-1))
! only IO tasks send any data.
! scounts is number of data elements to send to processor np.
! rcounts is number of data elements to recv from processor np.
! displs is displacement into send array for data to go to proc np
do np=0,numproc-1
   displs(np) = np*npts_max*ncdim
enddo
if (nproc <= nanals-1) then
   scounts = npts_max*ncdim
else
   scounts = 0
endif
! displs is also the displacement into recv array for data to go into anal_chunk
! on
! task np.
do np=0,numproc-1
   if (np <= nanals-1) then
      rcounts(np) = npts_max*ncdim
   else
      rcounts(np) = 0
   end if
enddo

! allocate array to hold pieces of state vector on each proc.
allocate(anal_chunk(nanals,npts_max,ncdim))
if (nproc == 0) print *,'anal_chunk size = ',size(anal_chunk)

allocate(ensmean_chunk(npts_max,ncdim))
ensmean_chunk = 0.
allocate(sendbuf(numproc*npts_max*ncdim))
allocate(recvbuf(numproc*npts_max*ncdim))

! send and receive buffers.
if (nproc <= nanals-1) then
   ! fill up send buffer.
   do np=1,numproc
     do nn=1,ncdim
      do i=1,numptsperproc(np)
       n = ((np-1)*ncdim + (nn-1))*npts_max + i
       sendbuf(n) = grdin(indxproc(np,i),nn)
     enddo
    enddo
   enddo
end if

call mpi_alltoallv(sendbuf, scounts, displs, mpi_real4, recvbuf,rcounts, displs,&
                   mpi_real4, mpi_comm_world, ierr)

!==> compute ensemble of first guesses on each task, remove mean from anal.
!$omp parallel do schedule(dynamic,1)  private(nn,i,nanal,n)
do nn=1,ncdim
   do i=1,numptsperproc(nproc+1)
	   
      do nanal=1,nanals
         n = ((nanal-1)*ncdim + (nn-1))*npts_max + i
         anal_chunk(nanal,i,nn) = recvbuf(n)
      enddo
	  
      ensmean_chunk(i,nn) = sum(anal_chunk(:,i,nn))/float(nanals)
	  
      ! remove mean from ensemble.
      do nanal=1,nanals
         anal_chunk(nanal,i,nn) = anal_chunk(nanal,i,nn)-ensmean_chunk(i,nn)
      end do
	  
   end do
end do
!$omp end parallel do

deallocate(sendbuf, recvbuf)

! Get forecast error at the evaluation time and distribute them to all
! processors
allocate(fcerror_chunk(npts_max,ncdim))
allocate(analmean_chunk(npts_max,ncdim))
allocate(sendbuf(numproc*npts_max*ncdim))
allocate(sendbuf2(numproc*npts_max*ncdim))
allocate(recvbuf(npts_max*ncdim))
allocate(recvbuf2(npts_max*ncdim))
if(nproc == 0) then
   ! Assign sendbuf recbuf from state variables
   do np=1,numproc
      do nn=1,ncdim
         do i=1,numptsperproc(np)
            n = ((np-1)*ncdim + (nn-1))*npts_max + i
            sendbuf(n) = grdin5(indxproc(np,i),nn)
            sendbuf2(n) = grdin3(indxproc(np,i),nn)
         end do
      end do
   end do
   call mpi_scatter(sendbuf,ncdim*npts_max,mpi_real4,recvbuf, &
        & ncdim*npts_max,mpi_real4,0,mpi_comm_world,ierr)
   call mpi_scatter(sendbuf2,ncdim*npts_max,mpi_real4,recvbuf2, &
        & ncdim*npts_max,mpi_real4,0,mpi_comm_world,ierr)
else
   call mpi_scatter(sendbuf,ncdim*npts_max,mpi_real4,recvbuf, &
        & ncdim*npts_max,mpi_real4,0,mpi_comm_world,ierr)
   call mpi_scatter(sendbuf2,ncdim*npts_max,mpi_real4,recvbuf2, &
        & ncdim*npts_max,mpi_real4,0,mpi_comm_world,ierr)
end if

!$omp parallel do schedule(dynamic,1)  private(nn,i,n) 

   do nn=1,ncdim
      do i=1,numptsperproc(nproc+1)
         n = (nn-1)*npts_max + i
         fcerror_chunk(i,nn) = recvbuf2(n)
         analmean_chunk(i,nn) = recvbuf(n)
      end do
   end do
   deallocate(sendbuf,recvbuf,sendbuf2,recvbuf2)
   if(allocated(grdin5)) deallocate(grdin5)
   call divide_weight(analmean_chunk)
   call divide_weight(ensmean_chunk(:,:))

call destroy_weight()

end subroutine scatter_chunks_ob_impact

end module scatter_chunks_efsoi
