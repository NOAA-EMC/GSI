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
!   fn  mpi_ireadsb
!   sub mpi_readsb
!   sub mpi_ufbint
!   sub mpi_ufbrep
!   fn  mpi_ireadmg
!   sub gsi_inquire   -  inquire statement supporting fortran earlier than 2003
!
! Variable Definitions:
!   def file_handle
!   def buffer
!   def block_offsets
!   def block_lenwords4
!   def nblocks
!   def ierror
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

    public mpi_openbf
    public mpi_closbf
    public mpi_readmg
    public mpi_nextblock
    public mpi_ireadsb
    public mpi_readsb
    public mpi_ufbint
    public mpi_ufbrep
    public mpi_ireadmg
    public mpi_querybf
    public gsi_inquire

    public lenbuf
!   public file_handle
!   public buffer
!   public buffer_offsets
    public nblocks

    integer(i_llong),parameter:: lenbuf=8388608  ! lenbuf=8*1024*1024
    integer(i_kind) file_handle
    integer(i_long),allocatable:: buffer(:)
    integer(kind=mpi_offset_kind),allocatable::block_offsets(:)
    integer(4),allocatable::block_lenwords4(:)
    integer(4) nblocks
    integer(4) ierror
    integer(4) ibegin,iend

contains

  subroutine mpi_openbf(filename,npe,mype,mpi_com)
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

    character(*) filename
    integer(i_kind) npe,mype,mpi_com

    integer(8) len4file,offset_words
    integer(8),parameter:: len13000=13000
    integer(kind=mpi_offset_kind) lenbytes,offset
    integer(4) buf
    character(4) cbuf
    equivalence(cbuf,buf)
    integer(4) lenwords4
    integer(8) status(mpi_status_size)
    integer(4)   i,iblocks
    integer(4) maxwords
    integer(8),allocatable::block_offsets0(:),block_offsets1(:)


	call mpi_file_open(mpi_com,trim(filename),mpi_mode_rdonly,mpi_info_null,file_handle,ierror)
    call mpi_file_get_size(file_handle,lenbytes,ierror)
    len4file=lenbytes/4

    nblocks=len4file/lenbuf
    if(nblocks*lenbuf .lt. len4file) nblocks=nblocks+1
    allocate(block_offsets0(0:nblocks),block_offsets(0:nblocks),block_lenwords4(0:nblocks-1))
    allocate(block_offsets1(0:nblocks))
    do i=0,nblocks-1
      block_offsets0(i)=-1_8
    end do
    block_offsets0(nblocks)=len4file*4

    allocate(buffer(len13000))
    do iblocks=0,nblocks-1,npe
      offset_words= (iblocks+mype)*lenbuf
      offset=4*offset_words
      lenwords4=min(len13000,max(0_8,len4file-offset_words))
      call mpi_file_read_at(file_handle,offset,buffer,lenwords4,mpi_integer4,status,ierror)
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
	call mpi_allreduce(block_offsets0,block_offsets1,nblocks+1,mpi_integer8,mpi_max,mpi_com,ierror)
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


  subroutine mpi_closbf
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

    call mpi_file_close(file_handle,ierror)
    deallocate(buffer,block_offsets,block_lenwords4)

  end subroutine mpi_closbf


  subroutine mpi_nextblock(next)
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

    integer(i_kind) next
    integer(8) status(mpi_status_size)

    call mpi_file_read_at(file_handle,block_offsets(next),buffer, &
                          block_lenwords4(next),mpi_integer4,status,ierror)
    ibegin=0
    iend=ibegin+block_lenwords4(next)

  end subroutine mpi_nextblock


  subroutine mpi_readmg(lunit,subset,jdate,iret)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    mpi_readmg
!
!   prgrmmr:
!
! abstract:
!
! program history log:
!   2008-05-01  safford -- add subprogram doc block
!
!   input argument list:
!     lunit  -
!     subset -
!     jdate  -
!
!   output argument list:
!     iret   - return code
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block

    integer(i_kind) lunit,jdate,iret

    integer(4),PARAMETER::  NFILES=32
    integer(4),PARAMETER:: MXMSGL=50000

    integer(4) maxbyt,ibit,ibay,mbyt,mbay
      COMMON /BITBUF/ MAXBYT,IBIT,IBAY(MXMSGL/4),MBYT(NFILES), &
                      MBAY(MXMSGL/4,NFILES)

      CHARACTER*8 SEC0,SUBSET
      CHARACTER*4 BUFR
      CHARACTER*1 CEC0(8)
      integer(i_kind)   IEC0(2)
      EQUIVALENCE (SEC0,IEC0,CEC0)

    integer(i_kind) i,mbytes,nbytes,nwords4,mwords4,lun
    integer(4) lmsg

    lun=1

1   continue

    iret=0
    if(ibegin.ge.iend) then
      iret=-1
      return
    end if
    nbytes=buffer(ibegin+1)
    nwords4=nbytes/4
    iec0(1)=buffer(ibegin+2)
    iec0(2)=buffer(ibegin+3)
    CALL CHRTRNA(BUFR,CEC0,4)
    IF(BUFR.NE.'BUFR') then
      iret=-2
      return
    end if
    mwords4=lmsg(sec0)
    do i=1,mwords4
      mbay(i,lun)=buffer(ibegin+1+i)
    end do
    mbytes=buffer(ibegin+1+nwords4+1)
    if(mbytes.ne.nbytes) then
      iret=-3
      return
    end if
    ibegin=ibegin+1+nwords4+1

!         skip dictionary messages

    CALL CKTABA(LUN,SUBSET,JDATE,IRET)
    IF(IRET.NE.0) GO TO 1

  end subroutine mpi_readmg


  integer(4) function mpi_ireadsb(lunit)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    mpi_ireadsb
!
!   prgrmmr:
!
! abstract:
!
! program history log:
!   2008-05-01  safford -- add subprogram doc block
!
!   input argument list:
!     lunit      - 
!
!   return:
!     integer(4) -
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block

    integer(4) lunit
    integer(4) iret
    call mpi_readsb(lunit,iret)
    mpi_ireadsb=iret

  end function mpi_ireadsb


  integer(4) function mpi_ireadmg(lunit,subset,jdate)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    mpi_ireadmg
!
!   prgrmmr:
!
! abstract:
!
! program history log:
!   2008-05-01  safford -- add subprogram doc block
!
!   input argument list:
!     lunit      -
!     subset     -
!     jdate      -
!
!   return:
!     integer(4) -
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block

    integer(4) lunit
    integer(4) iret
    integer(4) jdate
    character(8) subset
    call mpi_readmg(lunit,subset,jdate,iret)
    mpi_ireadmg=iret

  end function mpi_ireadmg


  subroutine mpi_readsb(lunit,iret)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    mpi_readsb
!
!   prgrmmr:
!
! abstract:
!
! program history log:
!   2008-05-01  safford -- add subprogram doc block
!
!   input argument list:
!     lunit  -
!     
!   output argument list:
!     iret   - return code
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block

    integer(4) lunit,iret

    integer(4),PARAMETER:: NFILES=32
    integer(4),PARAMETER:: MXMSGL=50000

    integer(4) nmsg,nsub,msub,inode,idate
      COMMON /MSGCWD/ NMSG(NFILES),NSUB(NFILES),MSUB(NFILES), &
                      INODE(NFILES),IDATE(NFILES)
    integer(4) maxbyt,ibit,ibay,mbyt,mbay
      COMMON /BITBUF/ MAXBYT,IBIT,IBAY(MXMSGL/4),MBYT(NFILES), &
                      MBAY(MXMSGL/4,NFILES)
    integer(4) msgunp
      COMMON /UNPTYP/ MSGUNP(NFILES)

      CHARACTER*128 BORT_STR
    integer(4) lun,nbyt

      IRET = 0
      lun=1

!  SEE IF THERE IS ANOTHER SUBSET IN THE MESSAGE
!  ---------------------------------------------

      IF(NSUB(LUN).EQ.MSUB(LUN)) THEN
         IRET = -1
         return
      ELSE
         NSUB(LUN) = NSUB(LUN) + 1
      ENDIF

!  READ THE NEXT SUBSET AND RESET THE POINTERS
!  -------------------------------------------

      IF(MSGUNP(LUN).EQ.0) THEN
         IBIT = MBYT(LUN)*8
         CALL UPB(NBYT,16,MBAY(1,LUN),IBIT)
         CALL RDTREE(LUN)
         MBYT(LUN) = MBYT(LUN) + NBYT
      ELSEIF(MSGUNP(LUN).EQ.1) THEN
!  .... message with "standard" Section 3
         IBIT = MBYT(LUN)
         CALL RDTREE(LUN)
         MBYT(LUN) = IBIT
      ELSEIF(MSGUNP(LUN).EQ.2) THEN
!  .... compressed message
         CALL RDCMPS(LUN)
      ELSE
         WRITE(BORT_STR,'("BUFRLIB: READSB - MESSAGE UNPACK TYPE",I3,"IS'// &
            ' NOT RECOGNIZED")') MSGUNP
         CALL BORT(BORT_STR)
      ENDIF

  end subroutine mpi_readsb

  
      SUBROUTINE mpi_UFBINT(LUNIN,USR,I1,I2,IRET,STR)

!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!
! SUBPROGRAM:    UFBINT
!   PRGMMR: WOOLLEN          ORG: NP20       DATE: 1994-01-06
!
! ABSTRACT: THIS SUBROUTINE WRITES OR READS SPECIFIED VALUES TO OR FROM
!   THE CURRENT BUFR DATA SUBSET WITHIN INTERNAL ARRAYS, WITH THE
!   DIRECTION OF THE DATA TRANSFER DETERMINED BY THE CONTEXT OF
!   ABS(LUNIN) {I.E., IF ABS(LUNIN) POINTS TO A BUFR FILE THAT IS OPEN
!   FOR INPUT, THEN DATA VALUES ARE READ FROM THE INTERNAL DATA SUBSET;
!   OTHERWISE, DATA VALUES ARE WRITTEN TO THE INTERNAL DATA SUBSET.
!   THE DATA VALUES CORRESPOND TO MNEMONICS WHICH ARE PART OF A
!   DELAYED-REPLICATION SEQUENCE, OR FOR WHICH THERE IS NO REPLICATION
!   AT ALL. IF UFBINT IS READING VALUES, THEN EITHER BUFR ARCHIVE
!   LIBRARY SUBROUTINE READSB OR READNS MUST HAVE BEEN PREVIOUSLY
!   CALLED TO READ THE SUBSET FROM UNIT ABS(LUNIN) INTO
!   INTERNAL MEMORY.  IF IT IS WRITING VALUES, THEN EITHER BUFR ARCHIVE
!   LIBRARY SUBROUTINE OPENMG OR OPENMB MUST HAVE BEEN PREVIOUSLY
!   CALLED TO OPEN AND INITIALIZE A BUFR MESSAGE WITHIN MEMORY FOR THIS
!   ABS(LUNIN).
!
! PROGRAM HISTORY LOG:
! 1994-01-06  J. WOOLLEN -- ORIGINAL AUTHOR
! 1996-11-25  J. WOOLLEN -- MODIFIED TO ADD A RETURN CODE WHEN
!                           MNEMONICS ARE NOT FOUND WHEN READING
! 1996-12-11  J. WOOLLEN -- REMOVED A HARD ABORT FOR USERS WHO TRY TO
!                           WRITE NON-EXISTING MNEMONICS
! 1996-12-17  J. WOOLLEN -- MODIFIED TO ALWAYS INITIALIZE "USR" ARRAY
!                           TO MISSING (10E10) WHEN BUFR FILE IS BEING
!                           READ
! 1998-07-08  J. WOOLLEN -- REPLACED CALL TO CRAY LIBRARY ROUTINE
!                           "ABORT" WITH CALL TO NEW INTERNAL BUFRLIB
!                           ROUTINE "BORT"; IMPROVED MACHINE
!                           PORTABILITY
! 1999-11-18  J. WOOLLEN -- THE NUMBER OF BUFR FILES WHICH CAN BE
!                           OPENED AT ONE TIME INCREASED FROM 10 TO 32
!                           (NECESSARY IN ORDER TO PROCESS MULTIPLE
!                           BUFR FILES UNDER THE MPI)
! 2002-05-14  J. WOOLLEN -- REMOVED OLD CRAY COMPILER DIRECTIVES
! 2003-11-04  S. BENDER  -- ADDED REMARKS/BUFRLIB ROUTINE
!                           INTERDEPENDENCIES
! 2003-11-04  D. KEYSER  -- MAXJL (MAXIMUM NUMBER OF JUMP/LINK ENTRIES)
!                           INCREASED FROM 15000 TO 16000 (WAS IN
!                           VERIFICATION VERSION); UNIFIED/PORTABLE FOR
!                           WRF; ADDED DOCUMENTATION (INCLUDING
!                           HISTORY); OUTPUTS MORE COMPLETE DIAGNOSTIC
!                           INFO WHEN ROUTINE TERMINATES ABNORMALLY OR
!                           UNUSUAL THINGS HAPPEN; CHANGED CALL FROM
!                           BORT TO BORT2 IN SOME CASES
! 2004-08-18  J. ATOR    -- ADDED SAVE FOR IFIRST1 AND IFIRST2 FLAGS
!
! USAGE:    CALL UFBINT (LUNIN, USR, I1, I2, IRET, STR)
!   INPUT ARGUMENT LIST:
!     LUNIN    - INTEGER: ABSOLUTE VALUE IS FORTRAN LOGICAL UNIT NUMBER
!                FOR BUFR FILE
!                  - IF BUFR FILE OPEN FOR OUTPUT AND LUNIN IS LESS
!                    THAN ZERO, UFBINT TREATS THE BUFR FILE AS THOUGH
!                    IT WERE OPEN FOR INPUT
!     USR      - ONLY IF BUFR FILE OPEN FOR OUTPUT:
!                   REAL*8: (I1,I2) STARTING ADDRESS OF DATA VALUES
!                   WRITTEN TO DATA SUBSET
!     I1       - INTEGER: LENGTH OF FIRST DIMENSION OF USR OR THE
!                NUMBER OF BLANK-SEPARATED MNEMONICS IN STR (FORMER
!                MUST BE AT LEAST AS LARGE AS LATTER)
!     I2       - INTEGER:
!                  - IF BUFR FILE OPEN FOR INPUT:  LENGTH OF SECOND
!                    DIMENSION OF USR
!                  - IF BUFR FILE OPEN FOR OUTPUT: NUMBER OF "LEVELS"
!                    OF DATA VALUES TO BE WRITTEN TO DATA SUBSET
!     STR      - CHARACTER*(*): STRING OF BLANK-SEPARATED TABLE B
!                MNEMONICS IN ONE-TO-ONE CORRESPONDENCE WITH FIRST
!                DIMENSION OF USR
!                  - IF BUFR FILE OPEN FOR INPUT: THIS CAN ALSO BE A
!                    SINGLE TABLE D (SEQUENCE) MNEMONIC WITH EITHER 8-
!                    OR 16-BIT DELAYED REPLICATION (SEE REMARKS 1)
!
!   OUTPUT ARGUMENT LIST:
!     USR      - ONLY IF BUFR FILE OPEN FOR INPUT:
!                   REAL*8: (I1,I2) STARTING ADDRESS OF DATA VALUES
!                   READ FROM DATA SUBSET
!     IRET     - INTEGER:
!                  - IF BUFR FILE OPEN FOR INPUT: NUMBER OF "LEVELS" OF
!                    DATA VALUES READ FROM DATA SUBSET (MUST BE NO
!                    LARGER THAN I2)
!                  - IF BUFR FILE OPEN FOR OUTPUT: NUMBER OF "LEVELS"
!                    OF DATA VALUES WRITTEN TO DATA SUBSET (SHOULD BE
!                    SAME AS I2)
!
!   OUTPUT FILES:
!     UNIT 06  - STANDARD OUTPUT PRINT
!
! REMARKS:
!    1) UFBINT CAN ALSO BE CALLED TO PROVIDE INFORMATION ABOUT A SINGLE
!       TABLE D (SEQUENCE) MNEMONIC WITH EITHER 8- OR 16-BIT DELAYED
!       REPLICATION IN A SUBSET WHEN THE BUFR FILE IS OPEN FOR INPUT.
!       THE MNEMONIC IN STR MUST APPEAR AS IT DOES IN THE BUFR TABLE,
!       I.E., BRACKETED BY "{" AND "}" OR "[" AND "]" FOR 8-BIT DELAYED
!       REPLICATION, OR BRACKETED BY "(" AND ")" FOR 16-BIT DELAYED
!       REPLICATION.  {NOTE: THIS WILL NOT WORK FOR SEQUENCES WITH
!       1-BIT DELAYED REPLICATION (BRACKETED BY "<" AND ">"), STANDARD
!       REPLICATION (BRACKETED BY "'s), OR NO REPLICATION (NO
!       BRACKETING SYMBOLS).}
!       
!       FOR EXAMPLE:
!
!       CALL UFBINT(LUNIN,PLEVL,1, 50,IRET,'{PLEVL}')
!
!       WILL RETURN WITH IRET EQUAL TO THE NUMBER OF OCCURRENCES OF THE
!       8-BIT DELAYED REPLICATION SEQUENCE PLEVL IN THE SUBSET AND WITH
!       (PLEVL(I),I=1,IRET) EQUAL TO THE NUMBER OF REPLICATIONS IN EACH
!       OCCURRENCE OF PLEVL IN THE SUBSET.  IF THERE ARE NO OCCURRENCES
!       OF PLEVL IN THE SUBSET, IRET IS RETURNED AS ZERO.
!
!    2) WHEN BUFR FILE IS OPEN FOR OUTPUT, CALLING UFBINT WITH ONE OR
!       MORE TABLE B MNEMONICS IN STR WHICH ARE PART OF A DELAYED
!       REPLICATION SEQUENCE (AS DEFINED IN THE BUFR MNEMONIC TABLE)
!       WILL NOT ONLY WRITE THE VALUES FOR THE SPECIFIED MNEMONIC(S)
!       INTO THE SUBSET, IT WILL ALSO ALLOCATE SPACE FOR ALL OTHER
!       MNEMONICS DEFINED FOR THE SEQUENCE IN THE BUFR TABLE, WITH THE
!       NUMBER OF REPLICATIONS BASED ON THE VALUE OF I2.
!
!       THIS ALLOWS AN APPLICATION PROGRAM TO STORE VALUES WHICH HAVE
!       STANDARD REPLICATION NESTED INSIDE OF A DELAYED REPLICATION
!       SEQUENCE.  FOR EXAMPLE, IF AN APPLICATION PROGRAM WANTED TO
!       WRITE 50 LEVELS OF WIND SPEED, WIND DIRECTION, OBSERVED
!       PRESSURE, FIRST GUESS PRESSURE AND ANALYZED PRESSURE, THE BUFR
!       TABLE COULD HAVE THE FOLLOWING STRUCTURE (NOTE 16 CHARACTERS
!       HAVE BEEN REMOVED FROM THE LAST COLUMN TO ALLOW THE TABLE TO
!       FIT IN THIS DOCBLOCK):
!
!       | SEQNCE   | {PLEVL}                                           |
!       | PLEVL    | WSPD WDIR TSIG PRLC TSIG PRLC TSIG PRLC           |
!
!              -- OR --
!
!       | SEQNCE   | {PLEVL}                                           |
!       | PLEVL    | WSPD WDIR "PSEQ"3                                 |
!       | PSEQ     | TSIG PRLC                                         |
!
!       THE FOLLOWING LOGIC WOULD THEN ENCODE VALUES PROPERLY:
!.....................................................................
!       REAL*8 DROBS(2,50)
!       REAL*8 SROBS(2,150)
!              ....
!              ....
!       DO I=1,50
!         DROBS(1,I) = Value of wind speed on level "I"
!         DROBS(2,I) = Value of wind direction on level "I"
!       ENDDO
!       DO I=1,50
!         SROBS(1,I*1) = Value of observed pressure on level "I"
!         SROBS(2,I*1) = 25. ! Value in Code Table 0-08-021 (TSIG) for
!                            !  time significance (Nominal reporting
!                            !  time) for observed pressure on level
!                            !  "I"
!         SROBS(1,I*2) = Value of first guess pressure on level "I"
!         SROBS(2,I*2) = 27. ! Value in Code Table 0-08-021 (TSIG) for
!                            !  time significance (First guess) for
!                            !  first guess pressure on level "I"
!         SROBS(1,I*3) = Value of analyzed pressure on level "I"
!         SROBS(2,I*3) = 16. ! Value in Code Table 0-08-021 (TSIG) for
!                            !  time significance (Analysis) for
!                            !  analyzed pressure on level "I"
!       ENDDO
!
!       CALL UFBINT(LUNIN,DROBS,2, 50,IRET,'WSPD WDIR')
!       CALL UFBREP(LUNIN,SROBS,2,150,IRET,'TSIG PRLC')
!.....................................................................
!
!       A SIMILAR EXAMPLE COULD BE PROVIDED FOR READING VALUES WHICH
!       HAVE STANDARD REPLICATION NESTED WITHIN DELAYED REPLICATION,
!       FROM BUFR FILES OPEN FOR INPUT.  (NOT SHOWN HERE.)
!
!
!
!    THIS ROUTINE CALLS:        BORT     BORT2    STATUS   STRING
!                               TRYBUMP  UFBRW
!    THIS ROUTINE IS CALLED BY: UFBINX   UFBRMS
!                               Also called by application programs.
!
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 77
!   MACHINE:  PORTABLE TO ALL PLATFORMS
!
!$$$

      integer(4),PARAMETER:: MAXJL=16000
      integer(4),PARAMETER:: NFILES=32

      integer(4) i1,i2,lunin,iret

      integer(4) io,i,j,lun,ifirst1,ifirst2,lunit
      integer(4) nmsg,nsub,msub,inode,idate
      COMMON /MSGCWD/ NMSG(NFILES),NSUB(NFILES),MSUB(NFILES), &
                      INODE(NFILES),IDATE(NFILES)
      integer(4) nnod,ncon,nods,nodc,ivls,kons
      COMMON /USRSTR/ NNOD,NCON,NODS(20),NODC(10),IVLS(10),KONS(10)
      integer(4) nval,inv
      COMMON /USRINT/ NVAL(NFILES),INV(MAXJL,NFILES),VAL(MAXJL,NFILES)
      integer(4) iprt
      COMMON /QUIET / IPRT

      CHARACTER*(*) STR
      CHARACTER*128 BORT_STR1,BORT_STR2
      REAL*8        USR(I1,I2),VAL,BMISS

      DATA BMISS /10E10/,IFIRST1/0/,IFIRST2/0/

      SAVE IFIRST1, IFIRST2

!----------------------------------------------------------------------
!----------------------------------------------------------------------

      IRET = 0

!  CHECK THE FILE STATUS AND I-NODE
!  --------------------------------

      LUNIT = ABS(LUNIN)
      lun=1
      io=0
    ! CALL STATUS(LUNIT,LUN,IL,IM)
    ! IF(IL.EQ.0) GOTO 900
    ! IF(IM.EQ.0) GOTO 901
    ! IF(INODE(LUN).NE.INV(1,LUN)) GOTO 902

    ! IO = MIN(MAX(0,IL),1)
    ! IF(LUNIT.NE.LUNIN) IO = 0

      IF(I1.LE.0) THEN
         IF(IPRT.GE.0) THEN
      write(6,*)
      write(6,*)'+++++++++++++++++++++++WARNING+++++++++++++++++++++++++'
         write(6,*)'BUFRLIB: UFBINT - THIRD ARGUMENT (INPUT) IS .LE. 0', &
          ' -  RETURN WITH FIFTH ARGUMENT (IRET) = 0'
         write(6,*)'STR = ',STR
      write(6,*)'+++++++++++++++++++++++WARNING+++++++++++++++++++++++++'
      write(6,*)
         ENDIF
         GOTO 100
      ELSEIF(I2.LE.0) THEN
         IF(IPRT.EQ.-1)  IFIRST1 = 1
         IF(IO.EQ.0 .OR. IFIRST1.EQ.0 .OR. IPRT.GE.1)  THEN
      write(6,*)
      write(6,*)'+++++++++++++++++++++++WARNING+++++++++++++++++++++++++'
            write(6,*)'BUFRLIB: UFBINT - FOURTH ARGUMENT (INPUT) IS .LE.', &
             ' 0 -  RETURN WITH FIFTH ARGUMENT (IRET) = 0'
            write(6,*)'STR = ',STR
            IF(IPRT.EQ.0 .AND. IO.EQ.1)  write(6,101)
101   FORMAT('Note: Only the first occurrence of this WARNING message ', &
       'is printed, there may be more.  To output'/6X,'ALL WARNING ', &
       'messages, modify your application program to add ', &
       '"CALL OPENBF(0,''QUIET'',1)" prior'/6X,'to the first call to a', &
       ' BUFRLIB routine.')
      write(6,*)'+++++++++++++++++++++++WARNING+++++++++++++++++++++++++'
      write(6,*)
            IFIRST1 = 1
         ENDIF
         GOTO 100
      ENDIF

!  PARSE OR RECALL THE INPUT STRING
!  --------------------------------

      CALL STRING(STR,LUN,I1,IO)

!  INITIALIZE USR ARRAY PRECEEDING AN INPUT OPERATION
!  --------------------------------------------------

      IF(IO.EQ.0) THEN
         DO J=1,I2
         DO I=1,I1
         USR(I,J) = BMISS
         ENDDO
         ENDDO
      ENDIF

!  CALL THE MNEMONIC READER/WRITER
!  -------------------------------

      CALL UFBRW(LUN,USR,I1,I2,IO,IRET)

!  IF INCOMPLETE WRITE TRY TO INITIALIZE REPLICATION SEQUENCE OR RETURN
!  ---------------------------------------------------------------------

      IF(IO.EQ.1 .AND. IRET.NE.I2 .AND. IRET.GE.0) THEN
         CALL TRYBUMP(LUNIT,LUN,USR,I1,I2,IO,IRET)
         IF(IRET.NE.I2) GOTO 903
      ELSEIF(IRET.EQ.-1) THEN
         IRET = 0
      ENDIF

      IF(IRET.EQ.0)  THEN
         IF(IO.EQ.0) THEN
            IF(IPRT.GE.1)  THEN
      write(6,*)
      write(6,*)'+++++++++++++++++++++++WARNING+++++++++++++++++++++++++'
               write(6,*)'BUFRLIB: UFBINT - NO SPECIFIED VALUES READ IN', &
                ' -  RETURN WITH FIFTH ARGUMENT (IRET) = 0'
               write(6,*)'STR = ',STR
      write(6,*)'+++++++++++++++++++++++WARNING+++++++++++++++++++++++++'
      write(6,*)
            ENDIF
         ELSE
            IF(IPRT.EQ.-1)  IFIRST2 = 1
            IF(IFIRST2.EQ.0 .OR. IPRT.GE.1)  THEN
      write(6,*)
      write(6,*)'+++++++++++++++++++++++WARNING+++++++++++++++++++++++++'
               write(6,*)'BUFRLIB: UFBINT - NO SPECIFIED VALUES WRITTEN ', &
                'OUT -  RETURN WITH FIFTH ARGUMENT (IRET) = 0'
               write(6,*)'STR = ',STR,' MAY NOT BE IN THE BUFR TABLE(?)'
               IF(IPRT.EQ.0)  write(6,101)
      write(6,*)'+++++++++++++++++++++++WARNING+++++++++++++++++++++++++'
      write(6,*)
               IFIRST2 = 1
            ENDIF
         ENDIF
      ENDIF

!  EXITS
!  -----

100   RETURN
903   WRITE(BORT_STR1,'("BUFRLIB: UFBINT - MNEMONIC STRING READ IN IS'// &
       ': ",A)') STR
      WRITE(BORT_STR2,'(18X,"THE NUMBER OF ''LEVELS'' ACTUALLY '// &
       'WRITTEN (",I3,") DOES NOT EQUAL THE NUMBER REQUESTED (",I3,")'// &
       ' - INCOMPLETE WRITE")')  IRET,I2
      CALL BORT2(BORT_STR1,BORT_STR2)
      END subroutine mpi_ufbint


      SUBROUTINE mpi_UFBREP(LUNIO,USR,I1,I2,IRET,STR)

!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!
! SUBPROGRAM:    mpi_UFBREP
!   PRGMMR: WOOLLEN          ORG: NP20       DATE: 1994-01-06
!
! ABSTRACT: THIS SUBROUTINE WRITES OR READS SPECIFIED VALUES TO OR
!   FROM THE CURRENT BUFR DATA SUBSET WITHIN INTERNAL ARRAYS, WITH THE
!   DIRECTION OF THE DATA TRANSFER DETERMINED BY THE CONTEXT OF
!   ABS(LUNIO) (I.E., IF ABS(LUNIO) POINTS TO A BUFR FILE THAT IS OPEN
!   FOR INPUT, THEN DATA VALUES ARE READ FROM THE INTERNAL DATA SUBSET;
!   OTHERWISE, DATA VALUES ARE WRITTEN TO THE INTERNAL DATA SUBSET.
!   THE DATA VALUES CORRESPOND TO MNEMONICS WHICH ARE PART OF A REGULAR
!   (I.E., NON-DELAYED) REPLICATION SEQUENCE OR FOR THOSE WHICH ARE
!   REPLICATED VIA BEING DIRECTLY LISTED MORE THAN ONCE WITHIN AN
!   OVERALL SUBSET DEFINITION RATHER THAN BY BEING INCLUDED WITHIN A
!   REPLICATION SEQUENCE.  IF UFBREP IS READING VALUES, THEN EITHER
!   BUFR ARCHIVE LIBRARY SUBROUTINE READSB OR READNS MUST HAVE
!   BEEN PREVIOUSLY CALLED TO READ THE SUBSET FROM UNIT ABS(LUNIO) INTO
!   INTERNAL MEMORY.  IF IT IS WRITING VALUES, THEN EITHER BUFR ARCHIVE
!   LIBRARY SUBROUTINE OPENMG OR OPENMB MUST HAVE BEEN PREVIOUSLY
!   CALLED TO OPEN AND INITIALIZE A BUFR MESSAGE WITHIN MEMORY FOR THIS
!   ABS(LUNIO).
!
! PROGRAM HISTORY LOG:
! 1994-01-06  J. WOOLLEN -- ORIGINAL AUTHOR
! 1998-07-08  J. WOOLLEN -- REPLACED CALL TO CRAY LIBRARY ROUTINE
!                           "ABORT" WITH CALL TO NEW INTERNAL BUFRLIB
!                           ROUTINE "BORT"
! 1999-11-18  J. WOOLLEN -- THE NUMBER OF BUFR FILES WHICH CAN BE
!                           OPENED AT ONE TIME INCREASED FROM 10 TO 32
!                           (NECESSARY IN ORDER TO PROCESS MULTIPLE
!                           BUFR FILES UNDER THE MPI)
! 2003-05-19  J. WOOLLEN -- DISABLED THE PARSING SWITCH WHICH CONTROLS
!                           CHECKING FOR IN THE SAME REPLICATION GROUP,
!                           UFBREP DOES NOT NEED THIS CHECK, AND IT
!                           INTERFERES WITH WHAT UFBREP CAN DO
!                           OTHERWISE
! 2003-11-04  S. BENDER  -- ADDED REMARKS/BUFRLIB ROUTINE
!                           INTERDEPENDENCIES
! 2003-11-04  D. KEYSER  -- MAXJL (MAXIMUM NUMBER OF JUMP/LINK ENTRIES)
!                           INCREASED FROM 15000 TO 16000 (WAS IN
!                           VERIFICATION VERSION); UNIFIED/PORTABLE FOR
!                           WRF; ADDED DOCUMENTATION (INCLUDING
!                           HISTORY); OUTPUTS MORE COMPLETE DIAGNOSTIC
!                           INFO WHEN ROUTINE TERMINATES ABNORMALLY OR
!                           UNUSUAL THINGS HAPPEN; CHANGED CALL FROM
!                           BORT TO BORT2 IN SOME CASES
! 2004-08-18  J. ATOR    -- ADDED SAVE FOR IFIRST1 AND IFIRST2 FLAGS
! 2008-05-01  safford    -- rm unused vars
!
! USAGE:    CALL UFBREP (LUNIO, USR, I1, I2, IRET, STR)
!   INPUT ARGUMENT LIST:
!     LUNIO    - INTEGER: ABSOLUTE VALUE IS FORTRAN LOGICAL UNIT NUMBER
!                FOR BUFR FILE
!                  - IF BUFR FILE OPEN FOR OUTPUT AND LUNIO IS LESS
!                    THAN ZERO, UFBREP TREATS THE BUFR FILE AS THOUGH
!                    IT WERE OPEN FOR INPUT
!     USR      - ONLY IF BUFR FILE OPEN FOR OUTPUT:
!                   REAL*8: (I1,I2) STARTING ADDRESS OF DATA VALUES
!                   WRITTEN TO DATA SUBSET
!     I1       - INTEGER: LENGTH OF FIRST DIMENSION OF USR OR THE
!                NUMBER OF BLANK-SEPARATED MNEMONICS IN STR (FORMER
!                MUST BE AT LEAST AS LARGE AS LATTER)
!     I2       - INTEGER:
!                  - IF BUFR FILE OPEN FOR INPUT:  LENGTH OF SECOND
!                    DIMENSION OF USR
!                  - IF BUFR FILE OPEN FOR OUTPUT: NUMBER OF "LEVELS"
!                    OF DATA VALUES TO BE WRITTEN TO DATA SUBSET
!                    (MAXIMUM VALUE IS 255)
!     STR      - CHARACTER*(*): STRING OF BLANK-SEPARATED TABLE B
!                MNEMONICS IN ONE-TO-ONE CORRESPONDENCE WITH FIRST
!                DIMENSION OF USR
!                  - IF BUFR FILE OPEN FOR INPUT: THERE ARE THREE
!                     "GENERIC" MNEMONICS NOT RELATED TO TABLE B,
!                     THESE RETURN THE FOLLOWING INFORMATION IN
!                     CORRESPONDING USR LOCATION:
!                     'NUL'  WHICH ALWAYS RETURNS MISSING (10E10)
!                     'IREC' WHICH ALWAYS RETURNS THE CURRENT BUFR
!                            MESSAGE (RECORD) NUMBER IN WHICH THIS
!                            SUBSET RESIDES
!                     'ISUB' WHICH ALWAYS RETURNS THE CURRENT SUBSET
!                            NUMBER OF THIS SUBSET WITHIN THE BUFR
!                            MESSAGE (RECORD) NUMBER 'IREC'
!
!   OUTPUT ARGUMENT LIST:
!     USR      - ONLY IF BUFR FILE OPEN FOR INPUT:
!                   REAL*8: (I1,I2) STARTING ADDRESS OF DATA VALUES
!                   READ FROM DATA SUBSET
!     IRET     - INTEGER:
!                  - IF BUFR FILE OPEN FOR INPUT: NUMBER OF "LEVELS" OF
!                    DATA VALUES READ FROM DATA SUBSET (MUST BE NO
!                    LARGER THAN I2)
!                  - IF BUFR FILE OPEN FOR OUTPUT: NUMBER OF "LEVELS"
!                    OF DATA VALUES WRITTEN TO DATA SUBSET (SHOULD BE
!                    SAME AS I2)
!
!   OUTPUT FILES:
!     UNIT 06  - STANDARD OUTPUT PRINT
!
! REMARKS:
!    THIS ROUTINE CALLS:        BORT     BORT2    STATUS   STRING
!                               UFBRP
!    THIS ROUTINE IS CALLED BY: None
!                               Normally called only by application
!                               programs.
!
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 77
!   MACHINE:  PORTABLE TO ALL PLATFORMS
!
!$$$

      integer(4),PARAMETER:: MAXJL=16000
      integer(4),PARAMETER:: NFILES=32

      integer(4) i1,i2,lunio,iret

      integer(4) io,i,j,lun,ifirst1,ifirst2,lunit
      integer(4) nmsg,nsub,msub,inode,idate
      integer(4) ia2

      COMMON /MSGCWD/ NMSG(NFILES),NSUB(NFILES),MSUB(NFILES), &
           INODE(NFILES),IDATE(NFILES)
      integer(4) nval,inv
      COMMON /USRINT/ NVAL(NFILES),INV(MAXJL,NFILES),VAL(MAXJL,NFILES)
      integer(4) iprt,iac
      COMMON /ACMODE/ IAC
      COMMON /QUIET / IPRT

      CHARACTER*(*) STR
      CHARACTER*128 BORT_STR1,BORT_STR2
      REAL*8        USR(I1,I2),VAL,BMISS

      DATA BMISS /10E10/,IFIRST1/0/,IFIRST2/0/

      SAVE IFIRST1, IFIRST2

!----------------------------------------------------------------------
!----------------------------------------------------------------------

      IRET = 0

!  CHECK THE FILE STATUS AND I-NODE
!  --------------------------------

      LUNIT = ABS(LUNIO)
      lun=1
      io=0

      IF(I1.LE.0) THEN
         IF(IPRT.GE.0) THEN
      write(6,*)
      write(6,*)'+++++++++++++++++++++++WARNING+++++++++++++++++++++++++'
         write(6,*)'BUFRLIB: UFBREP - THIRD ARGUMENT (INPUT) IS .LE. 0', &
              ' -  RETURN WITH FIFTH ARGUMENT (IRET) = 0'
         write(6,*)'STR = ',STR
      write(6,*)'+++++++++++++++++++++++WARNING+++++++++++++++++++++++++'
      write(6,*)
         ENDIF
         GOTO 100
      ELSEIF(I2.LE.0) THEN
         IF(IPRT.EQ.-1)  IFIRST1 = 1
         IF(IO.EQ.0 .OR. IFIRST1.EQ.0 .OR. IPRT.GE.1)  THEN
      write(6,*)
      write(6,*)'+++++++++++++++++++++++WARNING+++++++++++++++++++++++++'
            write(6,*)'BUFRLIB: UFBREP - FOURTH ARGUMENT (INPUT) IS .LE. ', &
                 '0 -  RETURN WITH FIFTH ARGUMENT (IRET) = 0'
            write(6,*)'STR = ',STR
            IF(IPRT.EQ.0 .AND. IO.EQ.1)  PRINT 101
101   FORMAT('Note: Only the first occurrence of this WARNING message ', &
           'is printed, there may be more.  To output'/6X,'ALL WARNING ', &
           'messages, modify your application program to add ', &
           '"CALL OPENBF(0,''QUIET'',1)" prior'/6X,'to the first call to a', &
           ' BUFRLIB routine.')
      write(6,*)'+++++++++++++++++++++++WARNING+++++++++++++++++++++++++'
      write(6,*)
            IFIRST1 = 1
         ENDIF
         GOTO 100
      ENDIF

!  INITIALIZE USR ARRAY PRECEEDING AN INPUT OPERATION
!  --------------------------------------------------

      IF(IO.EQ.0) THEN
         DO J=1,I2
         DO I=1,I1
         USR(I,J) = BMISS
         ENDDO
         ENDDO
      ENDIF

!  PARSE OR RECALL THE INPUT STRING - READ/WRITE VALUES
!  ----------------------------------------------------

      IA2 = IAC
      IAC = 1
      CALL STRING(STR,LUN,I1,IO)

!  CALL THE MNEMONIC READER/WRITER
!  -------------------------------

      CALL UFBRP(LUN,USR,I1,I2,IO,IRET)
      IAC = IA2

      IF(IO.EQ.1 .AND. IRET.LT.I2) GOTO 903

      IF(IRET.EQ.0)  THEN
         IF(IO.EQ.0) THEN
            IF(IPRT.GE.1)  THEN
      write(6,*)
      write(6,*)'+++++++++++++++++++++++WARNING+++++++++++++++++++++++++'
               write(6,*)'BUFRLIB: UFBREP - NO SPECIFIED VALUES READ IN', &
                    ' -  RETURN WITH FIFTH ARGUMENT (IRET) = 0'
               write(6,*)'STR = ',STR
      write(6,*)'+++++++++++++++++++++++WARNING+++++++++++++++++++++++++'
      write(6,*)
            ENDIF
         ELSE
            IF(IPRT.EQ.-1)  IFIRST2 = 1
            IF(IFIRST2.EQ.0 .OR. IPRT.GE.1)  THEN
      write(6,*)
      write(6,*)'+++++++++++++++++++++++WARNING+++++++++++++++++++++++++'
               write(6,*)'BUFRLIB: UFBREP - NO SPECIFIED VALUES WRITTEN ', &
                    'OUT -  RETURN WITH FIFTH ARGUMENT (IRET) = 0'
               write(6,*)'STR = ',STR,' MAY NOT BE IN THE BUFR TABLE(?)'
               IF(IPRT.EQ.0)  PRINT 101
      write(6,*)'+++++++++++++++++++++++WARNING+++++++++++++++++++++++++'
      write(6,*)
               IFIRST2 = 1
            ENDIF
         ENDIF
      ENDIF

!  EXITS
!  -----

100   RETURN
900   CALL BORT('BUFRLIB: UFBREP - BUFR FILE IS CLOSED, IT MUST BE'// &
           ' OPEN')
901   CALL BORT('BUFRLIB: UFBREP - A MESSAGE MUST BE OPEN IN BUFR '// &
           'FILE, NONE ARE')
902   CALL BORT('BUFRLIB: UFBREP - LOCATION OF INTERNAL TABLE FOR '// &
           'BUFR FILE DOES NOT AGREE WITH EXPECTED LOCATION IN INTERNAL '// &
           'SUBSET ARRAY')
903   WRITE(BORT_STR1,'("BUFRLIB: UFBREP - MNEMONIC STRING READ IN IS'// &
           ': ",A)') STR
      WRITE(BORT_STR2,'(18X,"THE NUMBER OF ''LEVELS'' ACTUALLY '// &
           'WRITTEN (",I3,") LESS THAN THE NUMBER REQUESTED (",I3,") - '// &
           'INCOMPLETE WRITE")')  IRET,I2
      CALL BORT2(BORT_STR1,BORT_STR2)
    END SUBROUTINE mpi_UFBREP


  subroutine mpi_querybf(filename,ntasks_read,mype)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    mpi_querybf
!
!   prgrmmr:
!
! abstract:
!
! program history log:
!   2008-05-01  safford -- add subprogram doc block, rm unused vars and uses
!
!   input argument list:
!     filename    -
!
!   output argument list:
!     ntasks_read -
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block

    use kinds, only: i_kind
    implicit none
    character(*),intent(in):: filename
    integer(i_kind),intent(in):: mype
    integer(i_kind),intent(out):: ntasks_read

    integer(8) len4file,lenxfile,lenbytes
    logical lexist

    call gsi_inquire (lenbytes,lexist,filename,mype)
    len4file=lenbytes/4

    ntasks_read=len4file/lenbuf
    if(ntasks_read*lenbuf < len4file) ntasks_read=ntasks_read+1

  end subroutine mpi_querybf

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
