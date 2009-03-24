      PROGRAM prepbykx

!     read a prepbufr file and write out a new prepbufr file with
!     selected (by kx type) observations
!     
!     input:
!     kxlist - input file contains list of kx types to be processed
!     its format is simply one kx type per line
!     (see kxinfo for details)
!     input prepbufr
!     
!     output:
!     output prepbufr
!     
      CHARACTER msgtyp*8
      character*8 subset,last
      integer newtyp,jdatep,lsttyp,kx(512),nsub,ntot,nsel,nargs,n
      character*(80) head1
      CHARACTER (len=255) :: fin, fout
      character*255, allocatable :: arg(:)
      real*8 hdr(14)

      logical opt_r, writeopt
      integer iin

      LOGICAL found,writeit

      DATA head1
     +     / 'SID YOB XOB ELV DHR RPT TCOR TYP TSB T29 ITP SQN' /

!-----------------------------------------------------------------------

      fout='prepbufr.out'
      nargs =  iargc()
      if( nargs.eq.0 ) then
         print *, 'usage: prepbykx.x {-r} infilename {-o outfilename}'
         stop
      end if
      opt_r = .false.

      allocate (arg(nargs))
      do n=1,nargs
         call getarg(n,arg(n))
      enddo

      iin = 1
      if( trim(arg(1)).eq.'-r' ) then
          opt_r = .true.
          iin = 2
      endif

      do n=1,nargs
         if( trim(arg(n)).eq.'-o' ) fout = trim(arg(n+1))
      end do
      fin = trim(arg(iin))
      numkx=0
      open(unit=10,file='kxlist',form='formatted')
      do k=1,512
         read(10,*,end=999)kx(k)
         numkx=numkx+1
      end do
 999  close(10)

      if (opt_r) then
         write(*,*) numkx,' kx types  will be removed:'
      else
         write(*,*) numkx,' kx types  will be processed:'
      endif
      do k=1,numkx
         write(*,*)'  kx(',k,') = ',kx(k)
      end do

!     Open the PREPBUFR files.

      OPEN  ( UNIT = 11, FILE = fin, FORM = 'UNFORMATTED' )
      CALL OPENBF(11,'IN',11 )  ! BUFRLIB routine to open file
      OPEN  ( UNIT = 51, FILE = fout, FORM = 'UNFORMATTED' )
      CALL OPENBF(51,'OUT',11)

      CALL DATELEN(10)          ! BUFRLIB routine to use 10-digit date

      lsttyp=0
      NEWTYP = 0
      ntot=0
      nsub=0
      nsel=0
      found = .false.
      writeopt = .not. opt_r    ! if opt_r = .true. then the writeopt will
                                !        be remove (do not write)
                                !        the matching observations
                                ! if opt_r - .false. then writeopt will
                                !        be keep (do write) the matching.
      writeit = .not. writeopt

      print *, 'Read records...'
      DO WHILE(IREADMG(11,SUBSET,JDATEP).EQ.0)
         nsub=nsub+1
         IF(SUBSET.NE.LAST)  THEN
            NEWTYP = 1
            print *, 'New input message type read in: ',SUBSET
         END IF
         CALL OPENMB(51,SUBSET,JDATEP)
         DO WHILE(IREADSB(11).EQ.0)
            ntot=ntot+1
            CALL UFBINT(11,hdr,12,1,jret,head1)
!     check if hdr is in list
            do k=1,numkx
               if(nint(hdr(8)).eq.kx(k)) then
                  writeit= writeopt
                  goto 88
               end if
            end do
 88         if(writeit) then
               nsel=nsel+1
               CALL UFBCPY(11,51)
               CALL WRITSB(51)
            end if
            writeit= .not. writeopt
            NEWTYP = 0
         ENDDO
         LAST = SUBSET
      ENDDO
      CALL CLOSBF(11)
      CALL CLOSBF(51)
      
      print *, nsub,' subsets read in'
      print *, ntot,' obs read in'
      print *, nsel,' obs written out'
      
      stop
      END



