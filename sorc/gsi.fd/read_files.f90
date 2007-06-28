subroutine read_files(mype)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    read_files       get info about atm & sfc guess files
!   prgmmr: derber           org: np23                date: 2002-11-14
!
! abstract:  This routine determines how many global atmospheric and
!            surface guess files are present.  The valid time for each
!            guess file is determine.  The time are then sorted in
!            ascending order.  This information is broadcast to all
!            mpi tasks.
!
! program history log:
!   2002-11-14  derber
!   2004-06-16  treadon - update documentation
!   2004-08-02  treadon - add only to module use, add intent in/out
!   2004-12-02  treadon - replace mpe_ibcast (IBM extension) with
!                         standard mpi_bcast
!   2005-01-27  treadon - make use of sfcio module
!   2005-02-18  todling - no need to read entire sfc file; only head needed
!   2005-03-30  treadon - clean up formatting of write statements
!   2006-01-09  treadon - use sigio to read gfs spectral coefficient file header
!
!   input argument list:
!     mype     - mpi task id
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,r_single,i_kind
  use mpimod, only: mpi_rtype,mpi_comm_world,ierror,npe
  use guess_grids, only: nfldsig,nfldsfc,ntguessig,ntguessfc,&
       ifilesig,ifilesfc,hrdifsig,hrdifsfc
  use gridmod, only: nhr_assimilation
  use constants, only: izero,zero,one
  use obsmod, only: iadate
  use sfcio_module, only: sfcio_head,sfcio_sropen,&
       sfcio_sclose,sfcio_srhead
  use sigio_module, only: sigio_head,sigio_sropen,&
       sigio_sclose,sigio_srhead
  
  implicit none

! Declare passed variables
  integer(i_kind),intent(in):: mype

! Declare local parameters
  integer(i_kind),parameter:: lunsfc=11
  integer(i_kind),parameter:: lunatm=12
  real(r_kind),parameter:: r0_001=0.001_r_kind
  real(r_kind),parameter:: r60=60.0_r_kind

! Declare local variables
  logical(4) fexist
  character(6) filename
  integer(i_kind) i,j,iwan,npem1,iret
  integer(i_kind) nsigg,nhr_half
  integer(i_kind) nminanl,nmings,nming2,ndiff
  integer(i_kind),dimension(4):: idateg
  integer(i_kind),dimension(5):: idate5
  real(r_single) hourg4
  real(r_kind) hourg,temp
  real(r_kind),dimension(202,2):: time_ges

  type(sfcio_head):: gfssfc_head
  type(sigio_head):: gfsatm_head


!-----------------------------------------------------------------------------
! Initialize variables
  nhr_half=nhr_assimilation/2
  if(nhr_half*2 < nhr_assimilation) nhr_half=nhr_half+1
  npem1=npe-1

  do i=1,nfldsig
     ifilesig(i) = -100
     hrdifsig(i) = zero
  end do

  do i=1,nfldsfc
     ifilesfc(i) = -100
     hrdifsfc(i) = zero
  end do

  do i=1,202
     time_ges(i,1) = 999
     time_ges(i,2) = 999
  end do


! Let a single task query the guess files.
  if(mype==npem1) then

!    Convert analysis time to minutes relative to fixed date
     call w3fs21(iadate,nminanl)
     write(6,*)'READ_FILES:  analysis date,minutes ',iadate,nminanl

!    Check for consistency of times from atmospheric guess files.
     iwan=izero
     do i=0,99
        write(filename,100)i
100     format('sigf',i2.2)
        inquire(file=filename,exist=fexist)
        if(fexist)then
           call sigio_sropen(lunatm,filename,iret)
           call sigio_srhead(lunatm,gfsatm_head,iret)
           hourg4=gfsatm_head%fhour
           idateg=gfsatm_head%idate
           call sigio_sclose(lunatm,iret)
           hourg = hourg4
           idate5(1)=idateg(4); idate5(2)=idateg(2)
           idate5(3)=idateg(3); idate5(4)=idateg(1); idate5(5)=izero
           call w3fs21(idate5,nmings)
           nming2=nmings+60*hourg
           write(6,*)'READ_FILES:  atm guess file, nming2 ',hourg,idateg,nming2
           ndiff=nming2-nminanl
           if(abs(ndiff) > 60*nhr_half ) go to 110
           iwan=iwan+1
           time_ges(iwan,1) = (nming2-nminanl)/r60
           time_ges(iwan+100,1)=i+r0_001
        end if
110     continue
     end do
     time_ges(201,1)=one
     time_ges(202,1)=one
     if(iwan > 1)then
        do i=1,iwan
           do j=i+1,iwan 
              if(time_ges(j,1) < time_ges(i,1))then
                 temp=time_ges(i+100,1)
                 time_ges(i+100,1)=time_ges(j+100,1)
                 time_ges(j+100,1)=temp
                 temp=time_ges(i,1)
                 time_ges(i,1)=time_ges(j,1)
                 time_ges(j,1)=temp
              end if
           end do
           if(time_ges(i,1) < r0_001)time_ges(202,1) = i
        end do
     end if
     time_ges(201,1) = iwan+r0_001

!    Check for consistency of times from surface guess files.
     iwan=izero
     do i=0,99
        write(filename,200)i
200     format('sfcf',i2.2)
        inquire(file=filename,exist=fexist)
        if(fexist)then
           call sfcio_sropen(lunsfc,filename,iret)
           call sfcio_srhead(lunsfc,gfssfc_head,iret)
           hourg4=gfssfc_head%fhour
           idateg=gfssfc_head%idate
           call sfcio_sclose(lunsfc,iret)
           hourg = hourg4
           idate5(1)=idateg(4); idate5(2)=idateg(2)
           idate5(3)=idateg(3); idate5(4)=idateg(1); idate5(5)=izero
           call w3fs21(idate5,nmings)
           nming2=nmings+60*hourg
           write(6,*)'READ_FILES:  sfc guess file, nming2 ',hourg,idateg,nming2
           ndiff=nming2-nminanl
           if(abs(ndiff) > 60*nhr_half ) go to 210
           iwan=iwan+1
           time_ges(iwan,2) = (nming2-nminanl)/r60
           time_ges(iwan+100,2)=i+r0_001
        end if
210     continue
     end do
     time_ges(201,2)=one
     time_ges(202,2)=one
     if(iwan > 1)then
        do i=1,iwan
           do j=i+1,iwan 
              if(time_ges(j,2) < time_ges(i,2))then
                 temp=time_ges(i+100,2)
                 time_ges(i+100,2)=time_ges(j+100,2)
                 time_ges(j+100,2)=temp
                 temp=time_ges(i,2)
                 time_ges(i,2)=time_ges(j,2)
                 time_ges(j,2)=temp
              end if
           end do
           if(time_ges(i,2) < r0_001)time_ges(202,2) = i
        end do
     end if
     time_ges(201,2) = iwan+r0_001
  end if


! Broadcast guess file information to all tasks
  call mpi_bcast(time_ges,404,mpi_rtype,npem1,mpi_comm_world,ierror)


! Load time information for atm guess field sinfo into output arrays
  ntguessig = nint(time_ges(202,1))
  nfldsig   = nint(time_ges(201,1))
  do i=1,nfldsig
     hrdifsig(i) = time_ges(i,1)
     ifilesig(i) = nint(time_ges(i+100,1))
  end do
  if(mype == 0) write(6,*)'READ_FILES:  atm fcst files used in analysis  :  ',&
       (ifilesig(i),i=1,nfldsig),(hrdifsig(i),i=1,nfldsig),ntguessig
  

! Load time information for surface guess field info into output arrays
  ntguessfc = nint(time_ges(202,2))
  nfldsfc   = nint(time_ges(201,2))
  do i=1,nfldsfc
     hrdifsfc(i) = time_ges(i,2)
     ifilesfc(i) = nint(time_ges(i+100,2))
  end do
  if(mype == 0) write(6,*)'READ_FILES:  sfc fcst files used in analysis:  ',&
       (ifilesfc(i),i=1,nfldsfc),(hrdifsfc(i),i=1,nfldsfc),ntguessfc
  

! End of routine
  return
end subroutine read_files
