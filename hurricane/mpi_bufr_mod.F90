module mpi_bufr_mod
!$$$   module documentation block
!                .      .    .                                       .
! module:    mpi_bufr_mod extra bufr routines for reading with mpi-io
!   prgmmr: parrish          org: np22                date: 2005-06-06
!
! abstract: Add new routines to augment bufrlib with the capability to
!             read a bufr file using mpi-io.
!
! program history log:
!   2005-08-01  parrish
!   2009-01-05  todling - add gsi_inquire
!
! subroutines included:
!   sub mpi_openbf
!   sub mpi_closbf 
!   sub mpi_readmg
!   sub mpi_nextblock
!   sub gsi_inquire   -  inquire statement supporting fortran earlier than 2003
!
! Variable Definitions:
!   def buffer
!   def block_offsets
!   def block_lenwords4
!   def nblocks
!   def ibegin
!   def iend
!
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$ end documentation block

    use kinds, only: i_kind,r_kind,i_long,i_llong
#ifdef ibm_sp
    use mpi,only: mpi_mode_rdonly,mpi_info_null,mpi_status_size,mpi_offset_kind
    use mpi,only: mpi_max,mpi_integer4,mpi_integer8
#else
    use m_mpif,only: mpi_mode_rdonly,mpi_info_null,mpi_status_size,mpi_offset_kind
    use m_mpif,only: mpi_max,mpi_integer4,mpi_integer8
#endif
    implicit none
    private
    public:: mpi_openbf, mpi_closbf, mpi_nextblock, lenbuf, mpi_readmg
    public:: gsi_inquire

    integer(i_llong),parameter:: lenbuf=8388608  ! lenbuf=8*1024*1024
    integer(i_long),allocatable,dimension(:):: buffer
    integer(kind=mpi_offset_kind),allocatable,dimension(:):: block_offsets
    integer(4),allocatable,dimension(:):: block_lenwords4
    integer(4) ibegin,iend

contains

  subroutine mpi_openbf(filename,npe,mype,mpi_com,file_handle,ierror,nblocks)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    mpi_openbf
!
!   prgrmmr:
!
! abstract:
!
! program history log:
!   2008-05-01  safford - add subprogram doc block
!
!   input argument list:
!     filename -
!     npe      -
!     mype     - mpi task id
!     mpi_com  -
!
!   output argument list:
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block
    use kinds, only: i_kind
    use mpi, only:  mpi_max_error_string,mpi_max_processor_name
    implicit none

    character(*) filename
    integer(i_kind) npe,mype,mpi_com,file_handle,ierror
    integer(i_kind):: nblocks

    integer(8) len4file,offset_words
    integer(8),parameter:: len13000=13000
    integer(kind=mpi_offset_kind) lenbytes,offset
    integer(4) buf
    character(4) cbuf
    equivalence(cbuf,buf)
    integer(4) lenwords4
    integer(8) status(mpi_status_size)
    integer(4) i,iblocks
    integer(4) maxwords
    integer(8),allocatable::block_offsets0(:),block_offsets1(:)

    logical:: lexist
    integer(4):: ierrmsg,lenout,ier,lpname
    character(mpi_max_error_string):: string
    character(mpi_max_processor_name):: pname


    call mpi_file_open(mpi_com,trim(filename),mpi_mode_rdonly,mpi_info_null,file_handle,ier)
    ierror=ier
    if (ier /=0 ) then
       call mpi_error_string(ier,string,lenout,ierrmsg)
       inquire(file=filename,exist=lexist)
       call mpi_get_processor_name(pname,lpname,ierrmsg)
       write(6,*)'MPI_OPENBF:  mpi_file_open ERROR ',ier,' ',string(1:lenout),&
            ' file_handle=',file_handle,' filename= ',trim(filename),&
            ' lexist=',lexist,' pname= ',pname(1:lpname)
       call system('sleep 60')
       call stop2(ier)
    endif

    call mpi_file_get_size(file_handle,lenbytes,ier)
    ierror=ierror+ier
    if (ier /= 0) then
       call mpi_error_string(ier,string,lenout,ierrmsg)
       write(6,*)'MPI_OPENBF:  mpi_file_get_size ERROR ',ier,' ',string(1:lenout),&
	' file_handle=',file_handle,' lenbytes=',lenbytes
    endif

    len4file=lenbytes/4

    nblocks=len4file/lenbuf
    if(nblocks*lenbuf .lt. len4file) nblocks=nblocks+1
    allocate(block_offsets0(0:nblocks),block_offsets1(0:nblocks),&
         block_lenwords4(0:nblocks-1),block_offsets(0:nblocks),&
         buffer(len13000))
    do i=0,nblocks-1
      block_offsets0(i)=-1_8
    end do
    block_offsets0(nblocks)=len4file*4

    do iblocks=0,nblocks-1,npe
      offset_words= (iblocks+mype)*lenbuf
      offset=4*offset_words
      lenwords4=min(len13000,max(0_8,len4file-offset_words))
      call mpi_file_read_at(file_handle,offset,buffer,lenwords4,mpi_integer4,status,ier)
      ierror=ierror+ier
      if(lenwords4.gt.0) then
        do i=1,lenwords4
          buf=buffer(i)
          if(cbuf.eq.'BUFR') then
            block_offsets0(iblocks+mype)=offset+4*(i-2)
            exit
          end if
        end do
      end if
    end do
    call mpi_allreduce(block_offsets0,block_offsets1,nblocks+1,mpi_integer8,mpi_max,mpi_com,ier)
    ierror=ierror+ier
    block_offsets=block_offsets1
    deallocate(block_offsets0,block_offsets1)
    maxwords=0
    do i=0,nblocks-1
      block_lenwords4(i)=(block_offsets(i+1)-block_offsets(i))/4_8
      maxwords=max(block_lenwords4(i),maxwords)
    end do
    deallocate(buffer)
    allocate(buffer(maxwords))
  end subroutine mpi_openbf


  subroutine mpi_closbf(file_handle,ierror)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    mpi_closbf
!
!   prgrmmr:
!
! abstract:
!
! program history log:
!   2008-05-01  safford -- add subprogram doc block
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block
    use kinds, only: i_kind
    implicit none

    integer(i_kind):: file_handle,ierror

    call mpi_file_close(file_handle,ierror)
    deallocate(buffer,block_offsets,block_lenwords4)

  end subroutine mpi_closbf


  subroutine mpi_nextblock(next,file_handle,ierror)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    mpi_nextblock
!
!   prgrmmr:
!
! abstract:
!
! program history log:
!   2008-05-01  safford -- add subprogram doc block
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block
    use kinds, only: i_kind
    implicit none

    integer(i_kind) next,file_handle,ierror
    integer(8) status(mpi_status_size)

    call mpi_file_read_at(file_handle,block_offsets(next),buffer, &
                          block_lenwords4(next),mpi_integer4,status,ierror)
    ibegin=0
    iend=ibegin+block_lenwords4(next)

  end subroutine mpi_nextblock

  subroutine mpi_readmg(lunit,subset,idate,iret)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    mpi_readmg
!
!   prgrmmr:woollen
!
! abstract:mpi_readmg interfaces mpi reading routines which block read bufr 
!          files with bufrlib decoding routines which require inputs of 
!          bufr messages which are split out of the reading blocks.
!          The interface is accomplished by passing start addresses of each
!          bufr message into the bufrlib interface routine readerme which
!          accepts bufr message inouts from an array passed as an argument.
!
! program history log:
!   2008-05-01  safford - add subprogram doc block
!   2009-04-01  woollen - add subprogram doc block
!
!   input argument list:
!     lunit - logical unit associated with bufr file being decoded
!
!   output argument list:
!     subset   - message type mnemonic of bufr meaasge being decoded
!     idate    - message date of bufr message being decoded  
!     iret     - return code
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block
    use kinds, only: i_kind
    implicit none

!   Delcare passed variables
    character(len=8),intent(out):: subset
    integer(i_kind) ,intent(in ):: lunit
    integer(i_kind) ,intent(out):: idate
    integer(i_kind) ,intent(out):: iret

!   Declare local variables
    integer(i_kind) mbytes,nbytes,nwords4

1   if(ibegin.ge.iend) then
      iret=-1
      return
    end if
    nbytes=buffer(ibegin+1)
    nwords4=nbytes/4
    mbytes=buffer(ibegin+1+nwords4+1)
    if(mbytes.ne.nbytes) then
      iret=-3
      return
    end if

    call readerme(buffer(ibegin+2),lunit,subset,idate,iret)
    ibegin=ibegin+1+nwords4+1
    if(iret==11)goto 1

  end subroutine mpi_readmg
    subroutine gsi_inquire (lbytes,lexist,filename,mype)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    gsi_inquire        inquire file presence and size
!   prgmmr: todling      org: np22                date: 2009-01-05
!
! abstract:  Inquire file presence and size; to be used when fortran
!            2003 not available or non-compliant.
!
! program history log:
!   2009-01-05  todling
!
!   input argument list:
!     mype     - mpi task id
!    filename  - input filename
!
!   output argument list:
!    lexist     - file presence flag
!    lbytes     - file size (bytes)
!
! attributes:
!   language: f90
!   machine:  Linux-cluster
!
!$$$  end documentation block

  use kinds, only: i_kind
  implicit none
  integer(8),intent(out) :: lbytes
  logical,intent(out) :: lexist
  character(len=*),intent(in) :: filename
  integer(i_kind),intent(in) :: mype

  integer(i_kind) :: lenb
  character(len=256) command, fname

#ifdef ibm_sp
  inquire(file=trim(filename),exist=lexist,size=lbytes)
#else
  lenb=0; lbytes = lenb
  inquire(file=trim(filename),exist=lexist)
  if(lexist)then
    write(fname,'(2a,i4.4)') 'fsize_',trim(filename),mype
   write(command,'(4a)') 'wc -c ', trim(filename),' > ', trim(fname)
    call system(command)
    open(unit=999,file=trim(fname),form='formatted')
    read(999,*) lenb
    close(999)
    lbytes=lenb
  endif
#endif
  return
  end subroutine gsi_inquire


end module mpi_bufr_mod
