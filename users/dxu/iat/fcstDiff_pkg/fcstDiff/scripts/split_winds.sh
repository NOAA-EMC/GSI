set -x

#  Revised by Greg Krasowski (IMSG) and Dennis Keyser (EMC) on 05/30/07
#  Input positional arg 1 - full path to input satwnd BUFR dump file
#  Input positional arg 2 - file name for output satwnd BUFR file (in $DATA)
#  Input positional arg 3 - minimum latitude boundary
#  Input positional arg 4 - maximum latitude boundary
#  Input positional arg 5 - minimum longitude boundary
#  Input positional arg 6 - maximum longitude boundary

test $# -ne 6 && exit

DATA=/stmp/$USER/split_winds # path to working directory
mkdir -p $DATA
cd $DATA

cat <<EOF > stdin
 &INPUT
   LATMIN = $3, LATMAX=$4, LONMIN=$5, LONMAX=$6
 /
EOF

ierr=$?

##################[ $ierr -eq 0 ]  &&  rm *


#######################################################################
#  In-line FORTRAN source code
#######################################################################

cat <<\inputEOF > split_winds.f

       PROGRAM SPLIT_WINDS

C-----------------------------------------------------------------------
C  MAIN PROGRAM SPLIT_WINDS
C
C  THIS SCRIPT WILL READ THROUGH A SATWND BUFR DUMP FILE (SPECIFIED AS 
C   POSITIONAL PARAMETER 1) AND WRITE OUT ALL REPORTS (I.E., SUBSETS) 
C   OF MESSAGE TYPES NC005010 AND NC005011 AS WELL AS NC005012 (VIS)
C   THAT ARE OUTSIDE OF THE READ LATITUDE AND LONGITUDE BOUNDARIES 
C   (SPECIFIED AS POSITIONAL PARAMETERS 3 FOR LATMIN, 4 FOR LATMAX, 
C   5 FOR LONMIN, & 6 FOR LONMAX) INTO AN OUTPUT SATWND BUFR FILE.
C-----------------------------------------------------------------------

      CHARACTER*8 SUBSET,SUBSET_LAST
     
      REAL*8 LALO_8(2)

      REAL*8 LATMIN,LATMAX,LONMIN,LONMAX
      
      DATA  LUBFI/21/  ! Fortran unit number for input  BUFR file
      DATA  LUBFJ/51/  ! Fortran unit number for output BUFR file

      DATA  IRECI/0/,IRECO/0/,IRECO_LAST/0/,SUBSET_LAST/'XXXXXXXX'/,
     $ IKNTSKP/0/,IKNTKEP/0/,ISUBO_LAST/-99/,IDATE_LAST/-99/

      NAMELIST/INPUT/LATMIN,LATMAX,LONMIN,LONMAX

      READ(5,INPUT)
      WRITE(6,INPUT)

      CALL DATELEN(10) ! we want to return 10-digit date in IREADMG

      CALL OPENBF(LUBFI,'IN',LUBFI) ! open BUFR file in unit LUBFI for
                                    !  input, use internal BUFR table
                                    !  to define data

      PRINT 100, LUBFI
  100 FORMAT(/5X,'===> BUFR DATA SET IN UNIT',I3,' SUCCESSFULLY ',
     $ 'OPENED FOR INPUT; READ PAST FIRST X-MESSAGES CONTAINING BUFR ',
     $ 'TABLES A,B,D'/)

      CALL OPENBF(LUBFJ,'OUT',LUBFI) ! open BUFR file in unit LUBFJ for
                                     !  output, use BUFR table internal
                                     !  to input file (in LUBFI) to
                                     !  define data

      PRINT 101, LUBFJ
  101 FORMAT(/5X,'===> BUFR DATA SET IN UNIT',I3,' SUCCESSFULLY',
     $ ' OPENED FOR OUTPUT; WRITE OUT FIRST X-MESSAGES CONTAINING ',
     $ 'INPUT BUFR TABLES A,B,D'/)

C  READ IN NEXT BUFR MESSAGE FROM INPUT BUFR FILE
C   (BELOW ANY DICTIONARY MESSAGES CONTAINING BUFR TABLES A, B, D)
C  ---------------------------------------------------------------

      DO WHILE(IREADMG(LUBFI,SUBSET,IDATE).EQ.0) ! if non-zero done
                                                 !  reading input file

         CALL UFBCNT(LUBFI,IRECI,IDUM) ! this just keeps a count of
                                       !  messages read in

         NSUB = NMSUB(LUBFI) ! BUFRLIB function NMSUB returns the
                             !  total number of subsets (reports) in
                             !  the current input BUFR message

         PRINT 102, IRECI,SUBSET,IDATE,NSUB
  102    FORMAT(/5X,'===> READ IN BUFR DATA MESSAGE NUMBER ',I5,' -- ',
     $    'TABLE A ENTRY IS "',A8,'", DATE IS',I11,', # OF SUBSETS ',
     $    'IS ',I4/)


C  If this is input message number 1 or number 2 and there are ZERO
C   subsets in the message this is a dummy message containing dump
C   center date in Section 1 (message 1) or a dummy message containing
C   dump processing dat in Section 1 (message 2) - simply copy these
C   messages directly to output file where they will provide the same
C   information
C  -------------------------------------------------------------------

         IF(IRECI.LE.2.AND.NSUB.EQ.0)  THEN
cppppp
            print *, ' --- DUMMY message - copy as is'
            print *, ' LUBFI= ',LUBFI
            print *, ' LUBFJ= ',LUBFJ
cppppp
            CALL COPYMG(LUBFI,LUBFJ)
            CALL UFBCNT(LUBFJ,IRECO,ISUBO) ! this just keeps a count of
                                           !  messages written out
            PRINT 103, SUBSET,IDATE,IRECO,ISUBO
            CYCLE ! Now go read in next message from input BUFR file

         END IF
         IF(SUBSET.NE.'NC005010' .and. SUBSET.NE.'NC005011' .and. 
     $  SUBSET.NE.'NC005012')  THEN
            IF(SUBSET_LAST.EQ.'NC005010' .or. SUBSET_LAST.EQ.'NC005011'
     $  .or. SUBSET_LAST.EQ.'NC005012') THEN

C  If current message read in is not NC005010 and NC005011 and NC005012
C   but last message read was NC005010 or NC005011 or NC005012, then we 
C   must close the last BUFR message since we had been writing subsets 
C   to (i.e., it is still open at this point)
C  --------------------------------------------------------------------

               CALL CLOSMG(LUBFJ)
               CALL UFBCNT(LUBFJ,IRECO,ISUBO) ! this just keeps a count
                                              !  of messages written out
               ipty=1
               PRINT 104, ipty,ISUBO,SUBSET_LAST,IDATE_LAST,IRECO
            END IF

C  Otherwise, simply copy message from input BUFR file to output
C   BUFR file and indicate which message is copied to output BUFR file
C  --------------------------------------------------------------------

cppppp
            print *, ' Msg copied as is'
            print *, ' LUBFI= ',LUBFI
            print *, ' LUBFJ= ',LUBFJ
cppppp
            CALL COPYMG(LUBFI,LUBFJ)
            CALL UFBCNT(LUBFJ,IRECO,ISUBO) ! this just keeps a count of
                                           !  messages written out

            PRINT 103, SUBSET,IDATE,IRECO,ISUBO
  103 FORMAT(/5X,'------> COPIED THIS MESSAGE TO OUTPUT FILE -- (ENTRY',
     $ ' "',A8,'", DATE ',I11,' - NO. OF DATA MESSAGES WRITTEN SO FAR:',
     $ I6,' - NO. OF SUBSETS IN THIS MESSAGE: ',I4/)
            IRECO_LAST  = IRECO
            ISUBO_LAST  = ISUBO
            SUBSET_LAST = SUBSET
            IDATE_LAST  = IDATE

C  Now go read in next message from input BUFR file
C  ------------------------------------------------

            CYCLE
         END IF

C  If current message read in is NC005010 or NC005011 or NC005012 
C   then we will need to unpack each subset to check and see if it is 
C   within the read latitude and longitude boundaries. 
C  ------------------------------------------------------------------

C  READ IN NEXT SUBSET (REPORT) FROM THIS MESSAGE IN INPUT BUFR FILE
C  -----------------------------------------------------------------

         DO WHILE(IREADSB(LUBFI).EQ.0) ! if non-zero all subsets read
                                       !  from this input file message,
                                       !  go read in next message from
                                       !  input BUFR file

C  Now check the CLAT and CLON for this subset, will tell us if this
C   subset is inside or out
C  --------------------------------------------------------------
            CALL UFBINT(LUBFI,LALO_8,2,1,NLEV,'CLAT CLON')

  ! LALO_8 will hold returned ****** * * **** ***********
  !  CLAT and CLON                   * * **** ***********
  !                                  * * **** ***********
  !            2 mnemonics in string * * **** ***********
  !                                    * **** ***********
  !         only 1 replication of LALO * **** ***********
  !                                      **** ***********
  !               number of replications **** ***********
  !     returned should always be 1 here      ***********
  !                                           ***********
  !          mnemonic LALO references station ***********
  !                   identification (CHAR*8)


            print *, 'Init Subset with LALO ',LALO_8(1),' & ',LALO_8(2)
            IF(LALO_8(1).GT.LATMIN.AND.LALO_8(1).LT.LATMAX.AND.
     $         LALO_8(2).GT.LONMIN.AND.LALO_8(2).LT.LONMAX) THEN
               
C  If we get here, this report is in the lat/lon box
C  -------------------------------------------------

               print *, '%% Subset with LALO ',LALO_8(1),'&',LALO_8(2),
     $' inside - skip'
cppppp

               IKNTSKP = IKNTSKP + 1 !  keep count of skipped 
                                     !   reports in NC005010 and 
                                     !   NC005011 and NC005012 messages

               CYCLE  ! all done with this subset, skipped it - go read
                      !  in next subset (report) from same message in
                      !  input BUFR file

            END IF

C  If we get here, this subset is good, we want to write it to output
C   BUFR file
C  ------------------------------------------------------------------

Cxxxxxxxxxxxxxxxxxxxxxxxxxx

            CALL OPENMB(LUBFJ,SUBSET,IDATE)  ! OPENMB must be called
              !  before potentially writing any new subsets to output
              !  messages; it will open a new output message (i.e.,
              !  ready to write subsets into) if either the SUBSET and/
              !  or the IDATE is different than the previous output
              !  message (note: new messages are automatically opened
              !  by WRTISB with the same SUBSET and IDATE as the
              !  current open message if the next subset can't fit into
              !  the current open message - the current open message is
              !  first closed automatically by WRITSB)

            CALL UFBCNT(LUBFJ,IRECO,ISUBO) ! again keep count of
                                           !  messages written out in
                                           !  case a new message has
                                           !  just been opened (meaning
                                           !  the previous message was
                                           !  just written out - test
                                           !  for that in next line)

cppppp
            print *, 'after call to OPENMB - SUBSET_LAST = ',subset_last
cppppp

            ipty=2
            IF(IRECO.NE.IRECO_LAST.AND.IRECO_LAST.NE.0.AND.SUBSET_LAST
     $       .EQ.'NC005010'.OR.SUBSET_LAST.EQ.'NC005011'
     $       .OR.SUBSET_LAST.EQ.'NC005012') 
cgsk        This means a new msg was just written out
     $       PRINT 104, ipty,ISUBO_LAST,SUBSET_LAST,IDATE_LAST,IRECO-1
  104 FORMAT(/3X,I2,'---> WROTE MESSAGE TO OUTPUT, # OF SUBSETS IS ',I4,
     $ ' (ENTRY "',A8,'", DATE',I11,', # OF DATA MESSAGES WRITTEN SO ',
     $ 'FAR:',I5/)

            SUBSET_LAST = SUBSET
            IRECO_LAST  = IRECO
            ISUBO_LAST  = ISUBO
            IDATE_LAST  = IDATE
Cxxxxxxxxxxxxxxxxxxxxxxxxxx
            print *, 'Subset with LALO1 ',LALO_8(1),' outside - copy'
            print *, 'Subset with LALO2 ',LALO_8(2),' outside - copy'
cppppp

            IKNTKEP = IKNTKEP + 1 ! keep count of kept outside reports in
                                  !  NC005010, NC005011, & NC005012 messages

            CALL UFBCPY(LUBFI,LUBFJ)  ! 2-step process to copy subset
                                      !  as is from current BUFR
                                      !  message being read in input
                                      !  BUFR file to current BUFR
            CALL WRITSB(LUBFJ)        !  message being written to in
                                      !  output BUFR file

            CALL UFBCNT(LUBFJ,IRECO,ISUBO) ! again keep count of
                                           !  messages written out in
                                           !  case a new message has
                                           !  just been opened (meaning
                                           !  the previous message was
                                           !  just written out - test
                                           !  for that in next line)
cppppp
            print *, 'after call to WRITSB - IRECO_LAST = ',ireco_last
            print *, 'after call to WRITSB - ISUBO = ',isubo
            print *, 'after call to WRITSB - SUBSET_LAST = ',subset_last
cppppp

            ipty=3
            IF(IRECO.NE.IRECO_LAST.AND.IRECO_LAST.NE.0.AND.SUBSET_LAST
     $       .EQ.'NC005010'.OR.SUBSET_LAST.EQ.'NC005011'
     $       .OR.SUBSET_LAST.EQ.'NC005012') 
cgsk        This means a new msg was just written out
     $      PRINT 104, IPTY,ISUBO_LAST,SUBSET_LAST,IDATE_LAST,IRECO-1

            IRECO_LAST  = IRECO
            ISUBO_LAST  = ISUBO
            SUBSET_LAST = SUBSET
            IDATE_LAST  = IDATE

         END DO

C  Come here when all subsets have been read from current BUFR message
C   in input file - move on to next BUFR message in input file
C  -------------------------------------------------------------------

      END DO

C  ALL MESSAGES IN INPUT BUFR FILE HAVE BEEN READ AND PROCESSED
C  ------------------------------------------------------------

      CALL CLOSBF(LUBFI)  ! CLOSE input  BUFR file
      CALL CLOSBF(LUBFJ)  ! CLOSE output BUFR file

      print *, 'COMPLETED'
      print *
      print *, '    - number of satwnd BUFR reports inside boundaries ',
     $ 'in message types NC005010, 011, & 012 skipped = ', ikntskp
      print *
      print *, '    - number of satwnd BUFR reports outside boundaries',
     $ ' in message types NC005010, 011, & 012 kept = ',ikntkep
      print *

      STOP
 
      END

inputEOF
#######################################################################

# make object module split_winds.o from split_winds.f using ncepxlf
#  compiler with optimization 3; list back source and specifically list lines
#  of source where there were compile problems; compile with 4-byte reals and
#  integers; provide additional compiler coutput to terminal; in stdout any
#  positive reals less than 1.0 are expressed as 0.xxxx rather than an .xxxxx

xlf -c -O3 -qlist -qsource -qnosave -qintsize=4 -qrealsize=4 -bnoquiet \
 -qxlf77=leadzero split_winds.f

# link object module split_winds.o to BUFR l and W3 libraies (4-byte,
#  64-bit object module) and greate executable split_winds

xlf /nwprod/lib/libbufr_4_64.a /nwprod/lib/libw3_4.a \
 -o split_winds split_winds.o

rm fort.*  # Remove any fortran unit number files from $DATA (shouldn't usually
           # be any)

cp $1 bufr.IN # Input positional arg 1 (full path to input satwnd BUFR dump file)
              #  is copied to temporary file $DATA/bufr.IN

ln -sf bufr.IN       fort.21  # Input file is assigned to Fortran unit number 21
ln -sf $2            fort.51  # Input positional arg 2 {file name for output
                              #  satwnd BUFR dump file (in $DATA directory)} is
                              #  assigned to Fortran unit number 51

# Execute program split_winds, writing stdout to $DATA/split_winds.out
#  and stderr to $DATA/errfile (timex provides run time statistics in stderr)

timex ./split_winds < stdin >split_winds.out 2> errfile
err=$?  # error return code from running of split_winds

cat errfile >> split_winds.out

if test "$err" -ne '0'
then
     echo "split_winds failed - abnormal stop"

else
     echo "split_winds successful - all done"
fi
##cat split_winds.out
rm fort.*
