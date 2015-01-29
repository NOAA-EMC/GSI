       subroutine img2x6t(imgtyp, IMAGE,imgsiz, maxiword_inp,inlncount,
     1                    npxl_out, nrowshow, iret_ras2)
C      ... copied ~/ncod/v4/ras2x6t.f into img2x6t.f 17-Jun-1996/dss
C      ...    while dissecting ras2x6t into two functions:
C      ...       rdpur() to read the pure-raster file into image plane;
C      ...       img2x6t() Given: image; start the conversion;
C 
C
       INTEGER    LASTCH
       INTEGER    LINE
       EXTERNAL   LASTCH
   
       INTEGER    NOTRAIL
       EXTERNAL   NOTRAIL

C      . . . . . . . . . . . . . . . . . . . . . . . . .
C      . . . . . . . . . . . . . . . . . . . . . . . . . . .
C ...  USAGE:  call img2x6t(imgtyp,IMAGE,imgsiz, maxiword_inp,inlncount,
C ...1                      npxl_out, nrowshow, iret_ras2)
       integer        imgtyp          	!... =0 main; =1 stitle;
       INTEGER        IMAGE(imgsiz)
       integer        maxiword_inp  	!... used here
       INTEGER        inlncount   	!... Arg(4)
       integer        npxl_out   	!... used here
       integer        nrowshow  		!... used here
       integer        iret_ras2
C      . . . . . . . . . . . . . . . . . . . . . . . . .
C      ----------------------------------------------------------------

       INTEGER       NBYTPWRD
       PARAMETER    (NBYTPWRD=8)                !... CRAY
C      . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
C      . . . . . . .   limiting size of input pure raster scanline . .
 
       integer       maxnmcpxl   		!... input nmc scanline 
       parameter    (maxnmcpxl=4224)     	!... moded to longword

       integer       maxnmcbyt
       parameter    (maxnmcbyt=maxnmcpxl/8)  	!... =(528) bytes

       integer       maxnmcwrd
       parameter    (maxnmcwrd=maxnmcbyt/nbytpwrd) 	!... =(66)

C      . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .

       integer        jiline(maxnmcwrd) 

C      . . . . . . . . . . . . . . . . . . . . . . . . . . .

       integer        iandmaskra(maxnmcwrd)
       integer        npixelshow
       integer        npixel_limit
       LOGICAL        LSTRIPTITLEQ

       INTEGER        IRET_OTR

       character*1    NULL

       SAVE   

C      . . . .   s t a r t   . . . . . . . . . . . . . . . . . . . . .
       NULL = char(0)

       iret_ras2 = 0
C
       LSTRIPTITLEQ = .FALSE.

       if(imgtyp .eq. 0) then
         LSTRIPTITLEQ = .FALSE.
         write(6,101)inlncount
  101    format(1h ,'img2x6t: was called w/ main image with ',
     1              'inlncount=', I6)
       else
C        ... IMGTYP WAS NON-ZERO,
         LSTRIPTITLEQ = .TRUE.
         write(6,102)inlncount
  102    format(1h ,'img2x6t: was called w/ strip-title image ',
     1              'with inlncount=', I6)
       endif
C
C      . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
C
C      ... To gen an IAND mask array which will clip a scanline
C      ...    to the specified npxl_out;
       npixelshow = npxl_out
       npixel_limit = 64*maxiword_inp
       if(npixelshow .GT. npixel_limit) then
         npixelshow = npixel_limit
       endif

       call genanmsk(npixelshow,iandmaskra,maxnmcwrd)

       do iw = 1,MAXNMCWRD
         jiline(iw) = 0
       enddo

C
       IF(LSTRIPTITLEQ) THEN      
         CALL otrncdx6('STRIP',jiline,maxiword_inp,IRET_OTR)
C        ... for initialize for strip-titles ...
       ELSE
         CALL otrncdx6('INIT ',jiline,maxiword_inp,IRET_OTR)
C        ... for initialize START-OF-MAP ...
       ENDIF


C      ===============================================================

       J_IMG = -1
       DO  LINE = 1,nrowshow
         J_IMG = J_IMG + 1    	 !... VALUE = 0 ON FIRST LOOP
         IF(J_IMG .GE. inlncount) THEN
C          ... THERE IS NO MORE DATA IN IMAGE ARRAY ...
C          ... SO JUMP OUT OF THIS LOOP
           GO TO 600
         ENDIF

C        ... GET ONE PURE RASTER SCANLINE HERE ...
         do iw = 1,MAXNMCWRD
           jiline(iw) = 0
         enddo

         DO  IWD = 1,MAXIWORD_INP
           M1 =  (J_IMG * MAXIWORD_INP)           
           JILINE(IWD) = IMAGE(M1+IWD)
         ENDDO
C          ... I have one pure raster in JILINE(1) to (maxiword_inp)
        
C        ... NOW I HAVE IN jiline ONE PURE RASTER SCANLINE ...
C        ... Clip it to npxl_out pixels
             DO  I = 1,maxnmcwrd
               JILINE(I) = IAND(JILINE(I),IANDMASKRA(I))
             ENDDO

C        ...    SO ENCODE IT ...

         CALL otrncdx6('ENDLN',jiline,maxiword_inp,IRET_OTR)
         IF(IRET_OTR .NE. 0) GO TO 600

       ENDDO

  600  continue

C      ... THE END OF DATA ...      
       CALL otrncdx6('ENDMP',jiline,maxiword_inp,IRET_OTR)

       go to 999

  999  continue
       return
       end
