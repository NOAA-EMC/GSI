      SUBROUTINE CALPW(PW,p,q,lm)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .     
C SUBPROGRAM:    CALPW       COMPUTES 
C   PRGRMMR: TREADON         ORG: W/NP2      DATE: 92-12-24       
C     
C ABSTRACT:  
C     THIS ROUTINE COMPUTES PRECIPITABLE WATER IN A COLUMN
C     EXTENDING FROM THE FIRST ATMOSPHERIC ETA LAYER TO THE
C     MODEL TOP.  THE DEFINITION USED IS
C                                 TOP
C            PRECIPITABLE WATER = SUM (Q+CLDW) DP*HTM/G
C                                 BOT
C     WHERE,
C        BOT IS THE FIRST ETA LAYER,
C        TOP IS THE MODEL TOP,
C        Q IS THE SPECIFIC HUMIDITY (KG/KG) IN THE LAYER
C        CLDW IS THE CLOUD WATER (KG/KG) IN THE LAYER
C        DP (Pa) IS THE LAYER THICKNESS.
C        HTM IS THE HEIGHT MASK AT THAT LAYER (=0 IF BELOW GROUND)
C        G IS THE GRAVITATIONAL CONSTANT
C     
C PROGRAM HISTORY LOG:
C   92-12-24  RUSS TREADON
C   96-03-04  MIKE BALDWIN - ADD CLOUD WATER AND SPEED UP CODE
C   98-06-15  T BLACK      - CONVERSION FROM 1-D TO 2-D
C   00-01-04  JIM TUCCILLO - MPI VERSION                 
C   02-06-19  MIKE BALDWIN - WRF VERSION 
C   04-12-30  H CHUANG      - UPDATE TO CALCULATE TOTAL COLUMN FOR OTHER
C                                     HYDROMETEORS                
C     
C USAGE:    CALL CALPW(PW)
C   INPUT ARGUMENT LIST:
C     PW       - ARRAY OF PRECIPITABLE WATER.
C
C   OUTPUT ARGUMENT LIST: 
C     NONE     
C     
C   OUTPUT FILES:
C     NONE
C     
C   SUBPROGRAMS CALLED:
C     UTILITIES:
C       NONE
C     LIBRARY:
C       COMMON   - LOOPS
C                  MASKS
C     
C   ATTRIBUTES:
C     LANGUAGE: FORTRAN
C     MACHINE : CRAY C-90
C$$$  
C     
c     use vrbls3d
c     use masks
C     
C     INCLUDE/SET PARAMETERS.
cc!      INCLUDE "parmeta"
c     INCLUDE "params"
C
C     INCLUDE COMMON BLOCKS.
c     INCLUDE "CTLBLK.comm"
C     
C     SET DENSITY OF WATER AT 1 ATMOSPHERE PRESSURE, 0C.
C     UNITS ARE KG/M**3.
      PARAMETER (RHOWAT=1.E3)
C     
C     DECLARE VARIABLES.
C     
      parameter(g=9.8, gi=1./g)
      parameter(im=1,jm=1,idecid=1)
      INTEGER LLMH
      REAL ALPM,DZ,PM,PWSUM,RHOAIR
      REAL PW(IM,JM),QDUM(IM,JM)
      real htm(im,jm,lm)
      real p(im,jm,lm),q(im,jm,lm)
C
C***************************************************************
C     START CALPW HERE.
C
C     INITIALIZE PW TO 0.    
C     
      PW = 0.
C     
C     OUTER LOOP OVER VERTICAL DIMENSION.
C     INNER LOOP OVER HORIZONTAL GRID.
C     
c     print*,'inside calpw'
      jsta=1
      jend=1
         do l=1,lm-1
          DO J=JSTA,JEND
            DO I=1,IM
              Qdum(I,J)=Q(I,J,L)
            ENDDO
          ENDDO
c       print*,'end of 1st loop'

        DO J=JSTA,JEND
          DO I=1,IM
c           print*,'i,j,l,l+1=',i,j,l,l+1
c           print*,'p(i,j,l+1)=',p(i,j,l+1)
c           print*,'p(i,j,l)=',p(i,j,l)
c           DP   =PINT(I,J,L+1)-PINT(I,J,L)
            DP   =P(I,J,L+1)-P(I,J,L)
            htm(i,j,l)=1.0
c           print*,'pw(i,j)=',pw(i,j)
c           print*,'qdum(i,j)=',qdum(i,j)
c           print*,'dp,gi,htm(i,j,l)=',dp,gi,htm(i,j,l)
            PW(I,J)=PW(I,J)+Qdum(I,J)*DP*GI*HTM(I,J,L)
c           print*,'i,j,pw=',i,j,pw(i,j)
          ENDDO
        ENDDO
        enddo
c       print*,'end of 2nd loop'
c       print*,'pw=',pw(1,1)
C     
C     END OF ROUTINE.
C     
      RETURN
      END
