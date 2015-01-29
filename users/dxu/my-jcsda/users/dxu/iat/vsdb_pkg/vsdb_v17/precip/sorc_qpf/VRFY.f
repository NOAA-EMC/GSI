      SUBROUTINE VERF(FCST,ANA,LFC,LAN,MASKREG,JO,NUMREG,NUMTHR,THRESH,
     &  lthrun,igrid,igmdl,ianatyp,fmask,isyr,ismn,isda,ishr,
     &  iyr,imn,ida,ihr,ifhr,iacc,MDLNAM,iounit)
C
C  PERFORM PRECIP VERIFICATION
C
C   INPUT:
C     FCST - FORECAST GRID
C     ANA  - ANALYSIS GRID
C     LFC  - BIT MAP FOR FORECAST GRID
C     LAN  - BIT MAP FOR ANALYSIS GRID TO DETERMINE VERF DOMAIN
C     MASKREG - INTEGER MAP FOR TO DETERMINE REGIONAL VERF DOMAINS
C     JO  - SIZE OF FCST,ANA,LFC,LAN
C     NUMREG  - NUMBER OF REGIONS
C     NUMTHR  - NUMBER OF THRESHOLDS
C     THRESH  - VERIFICATION THRESHOLDS
C     LTHRUN  - UNIT OF THRESHOLD INDICATOR (1=mm,2=in)
C     IGRID   - GRID NUMBER
C     IANATYP - ANALYSIS TYPE (1=NATV, 2=MPCP)
C     FMASK   - NAMES OF THE REGIONAL MASKS
C     ISYR    - START DATE YEAR
C     ISMN    - START DATE MONTH
C     ISDA    - START DATE DAY
C     ISHR    - START DATE HOUR
C     IYR     - VALID DATE YEAR
C     IMN     - VALID DATE MONTH
C     IDA     - VALID DATE DAY
C     IHR     - VALID DATE HOUR
C     IFHR    - FCST HOUR
C     IACC    - LENGTH OF ACCUMULATION
C     MDLNAM  - NAME OF MODEL
C     IOUNIT  - UNIT TO WRITE STATS OUT TO
C
C     OUTPUT IS WRITTEN TO IOUNIT IN THIS FORM:
C2345678*1*2345678*2*2345678*3*2345678*4*2345678*5*2345678*6*2345678*7*2345678*8
Cmodl    yyyymmddhh yyyymmddhh fhr acc grd grd thr un #obs #for #hit #tot
c                                  len ver MDL     it  pts  pts  pts  pts
CERL     1997032412 1997032600 036 024 096 096 00.2mm 1777 1223 1432 7773
Columns 02-08 is the forecast model verified.
Columns 10-19 is the start date for the forecast.
Columns 21-30 is the valid date for the forecast.
Columns 32-34 is the forecast hour.
Columns 36-38 is the accumulation length.
Columns 40-42 is the verifying grid.
Columns 43    is the verifying grid sub-domain.
Columns 44-46 is the model grid.
Columns 48-51 is the threshold.
Columns 52-53 is the threshold unit.
Columns 55-58 is the number of obs points.
Columns 60-63 is the number of fcst points.
Columns 65-68 is the number of correct points.
Columns 70-73 is the number of total verifying points.

C
      INTEGER   MASKREG(JO,NUMREG)
      INTEGER   iyr,imn,ida,ihr,ifhr
      INTEGER   isyr,ismn,isda,ishr
      INTEGER   f(numthr),h(numthr),o(numthr),t(numthr)
      REAL      FCST(JO),ANA(JO),THRESH(NUMTHR)
      LOGICAL*1 LFC(JO),LAN(JO)
      CHARACTER MDLNAM*8,reg*1
      CHARACTER fmask(numreg)*4,regful*4
      CHARACTER anaty(2)*4,unit(2)*2
      DATA ANATY/'NATV','MPCP'/
      DATA UNIT/'mm','in'/
C
      FCTR=1.0
      if (lthrun.eq.2) FCTR=25.4
      DO K=1,NUMREG
       regful=fmask(k)
       reg=regful(1:1)
       DO L=1,NUMTHR
        THRP=THRESH(L)*FCTR
        T(L)=0.
        O(L)=0.
        F(L)=0.
        H(L)=0.
        DO IJ=1,JO
         IF (LAN(IJ).AND.LFC(IJ)) THEN
          T(L)=T(L)+MASKREG(IJ,K)
          IF (ANA(IJ).GT.THRP) O(L)=O(L)+MASKREG(IJ,K)
          IF (FCST(IJ).GT.THRP) F(L)=F(L)+MASKREG(IJ,K)
          IF (ANA(IJ).GT.THRP.AND.FCST(IJ).GT.THRP)
     &     H(L)=H(L)+MASKREG(IJ,K)
         ENDIF
        ENDDO
       ENDDO
       WRITE(IOUNIT,331)MDLNAM,isyr,ismn,isda,
     &  ishr,IYR,IMN,IDA,IHR,
     &  ifhr+12-ishr,iacc,igrid,reg,igmdl,numthr,unit(lthrun)
       WRITE(IOUNIT,333) (THRESH(L),O(L),F(L),H(L),T(L),L=1,numthr)
      ENDDO
 331  FORMAT(1x,a,1x,I4.4,3I2.2,1x,I4.4,3I2.2,1x,I3.3,1x,I3.3,
     &    1x,I3.3,a1,1x,I3.3,1x,i2,1x,a2,' THR OBS FCS HIT TOT')
 333  FORMAT(3(f6.2,4i5))

      RETURN
      END

