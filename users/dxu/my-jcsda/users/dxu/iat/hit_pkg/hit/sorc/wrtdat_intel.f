      program wrtdat
c
c     This program reads in data from an ascii text file and writes 
c     the data back out in GrADS format (direct access).
c     The input for this program is an ascii file containing both
c     the intensity data and the intensity data wrt shifor (or the
c     track data and the track data wrt cliper).
c     So you will be writing out 2 sets of data in total, and you will 
c     use this data to make a 2-panel GrADS plot.  The data in the
c     ascii file should be in f6.1 format to be read in.
c
      parameter(npts=11)
      real cdat(npts), cdat1(npts)
      character  INFILE*65,OUTFILE*65,cmodname*8,cmodname1*8
      character CASEFILE*65
c
      namelist/namin/INFILE

c     Read name of input file from the namelist....
      read (5,namin,end=1000)
 1000 continue

      print *,'After namelist read, infile= ',infile

      open (unit=11
     &  ,file=INFILE
     &  ,access='sequential',action='read')

      CASEFILE = INFILE(1:LASTDOT(INFILE))//'.case'
      open (unit=52,file=CASEFILE,form='formatted',access='sequential')

      OUTFILE = INFILE(1:LASTDOT(INFILE))//'.gr'
!     open (unit=51
!    &  ,file=OUTFILE
!    &  ,access='direct',form='unformatted',status='replace'
!    &  ,recl=npts*4)
      open (unit=51,file=OUTFILE,form='unformatted',status='unknown')

      print *,'Name of input ascii track file is  ',infile
      print *,'Name of output GrADS track file is ',outfile
      print *,' '

      ict = 0
      do while (.true.)
        read (11,31,end=99) cmodname,(cdat(i),i=1,npts)
           if (ict == 0) then
             cmodname1 = cmodname
             do i = 1, npts
             cdat1(i) = cdat(i)
             enddo
           endif
        write (6,31) cmodname,(cdat(i),i=1,npts)
        ict = ict + 1
c        print *,'Reading in values for ',cmodname
!       write (51,rec=ict,err=940) cdat
        write (51) cdat
        if (cmodname /= cmodname1) then
        write(52,9) (int(cdat1(j)),j=1,5),
     &         int(cdat1(7)),int(cdat1(9)),int(cdat1(npts))
        endif
        cmodname1 = cmodname
             do i = 1, npts
             cdat1(i) = cdat(i)
             enddo
   9   FORMAT (1X,'#CASES   ',8(I4,3X))

      enddo
c
  31  format (1x,a4,3x,11(1x,f6.1))
c
      goto 99

 940  print *,'ERROR writing output file'

  99  continue
        write(52,9) (int(cdat(j)),j=1,5),
     &         int(cdat(7)),int(cdat(9)),int(cdat(npts))
      print *,' '
      print *,'Number of models = ',ict
      print *,' '
      stop
      end
c
c--------------------------------------------
c
c--------------------------------------------
      FUNCTION LASTDOT (STRING)
C
C**   RETURNS THE POSITION OF THE LAST CHARACTER of a 
c     OF A STRING before the last dot in the string.  For
c     example, in the string trkdat.ep.ascii, it would 
c     return "9", for the position of the "p".
C
      CHARACTER*(*) STRING
C
      LAST = LEN(STRING)
C
      DO 10 I = LAST,1,-1
        IF (STRING(I:I).EQ.'.') then
          itmp = i - 1
          GO TO 20
        endif
   10 CONTINUE
C
      LASTDOT = 0
      RETURN
C
   20 LASTDOT = itmp
      RETURN
C
      END

