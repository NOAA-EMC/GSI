       subroutine W3AS00(nch_parm,cparm,iret_parm)                              
C$$$  SUBPROGRAM DOCUMENTATION BLOCK                                            
C                .      .    .                                       .          
C SUBPROGRAM:    W3AS00      GET PARM FIELD FROM COMMAND-LINE                   
C   PRGMMR: SHIMOMURA        ORG: W/NMC41    DATE: 95-05-23                     
C                                                                               
C ABSTRACT: TO GET THE ONE COMMAND-LINE ARGUMENT WHICH STARTS WITH              
C   "PARM="; RETURNING THE PARM FIELD (WITHOUT THE KEYWORD "PARM=")             
C   AS A NULL-TERMINATED STRING IN THE CHARACTER STRING:CPARM.                  
C                                                                               
C PROGRAM HISTORY LOG:                                                          
C   95-05-23  DAVID SHIMOMURA                                                   
C   98-03-10  B. VUONG         REMOVE THE CDIR$ INTEGER=64 DIRECTIVE
C                                                                               
C USAGE:    CALL W3AS00(NCH_PARM, CPARM, iret_parm)                             
C                            1       2       3                                  
C                                                                               
C   OUTPUT ARGUMENT LIST:      (INCLUDING WORK ARRAYS)                          
C     (1.) NCH_PARM - NO. OF CHARACTERS IN THE PARM FIELD                       
C     (2.) CPARM    - C*(*) CPARM -- THE DESTINATION FOR THE PARMFIELD          
C                       OBTAINED FROM THE COMMAND LINE;                         
C                       USER SHOULD DEFINE THE CHARACTER STRING FOR             
C                       A SIZE .LE. 101-BYTES, WHICH WOULD BE                   
C                       BIG ENOUGH FOR THE 100-CHAR IBM LIMIT PLUS              
C                       ONE EXTRA BYTE FOR MY NULL-TERMINATOR.                  
C                                                                               
C     (3.) iret_parm - RETURN CODE                                              
C                    = 0;  NORMAL RETURN                                        
C                    = -1; ABNORMAL EXIT.  THE USER HAS FAILED                  
C                           TO DEFINE THE CPARM DESTINATION                     
C                           AS A CHARACTER STRING.                              
C                                                                               
C                    = +1; A WARNING:                                           
C                           THE GIVEN ARG IN THE COMMAND LINE WAS               
C                           TOO LONG TO FIT IN THE DESTINATION: CPARM,          
C                           SO I HAVE TRUNCATED IT.                             
C                                                                               
C                    = +2; A WARNING:  NO ARGS AT ALL ON COMMAND LINE,          
C                           SO I COULD NOT FETCH THE PARM FIELD.                
C                                                                               
C                    = +3; A WARNING:  NO "PARM="-ARGUMENT EXISTS               
C                           AMONG THE ARGS ON THE COMMAND LINE,                 
C                           SO I COULD NOT FETCH THE PARM FIELD.                
C                                                                               
C   OUTPUT FILES:                                                               
C     FT06F001 - SOME CHECKOUT PRINTOUT                                         
C                                                                               
C REMARKS:                                                                      
C                                                                               
C     TO EMULATE THE IBM PARM FIELD, THE USER SHOULD KEY_IN ON THE              
C        COMMAND LINE:                                                          
C              PARM='IN BETWEEN THE SINGLE_QUOTES IS THE PARM FIELD'            
C        WHAT IS RETURNED FROM W3AS00() FROM THE PARM= ARG IS                   
C        THE PARM FIELD: WHICH STARTS WITH THE LOCATION BEYOND THE              
C        EQUAL_SIGN OF THE KEYWORD "PARM=", AND INCLUDES EVERYTHING             
C        WHICH WAS WITHIN THE BOUNDS OF THE SINGLE-QUOTE SIGNS.                 
C        BUT THE QUOTE SIGNS THEMSELVES WILL DISAPPEAR; AND A NULL-             
C        TERMINATOR WILL BE ADDED.                                              
C        THE STARTING "PARM=" IS A KEY WORD FOR THE PARMS, AND SHOULD           
C        NOT BE USED TO START ANY OTHER ARGUMENT.                               
C                                                                               
C     CAUTION:  I HAVE CHANGED THE CALL SEQUENCE BY ADDING A RETURN CODE        
C                                                                               
C                                                                               
C ATTRIBUTES:                                                                   
C   LANGUAGE: CRAY FORTRAN77                                                    
C   MACHINE:  CRAY2                                                             
C                                                                               
C$$$                                                                            
C                                                                               
       integer    kbytpwrd                                                      
       parameter (kbytpwrd=8)                                                   
       integer    maxnbyt                                                       
       parameter (maxnbyt=112)                                                  
C      ... WHERE 112 CHARACTERS IS SIZE OF CWORK FOR 100 CHARACTERS             
C      ...   WITHIN QUOTES + 'PARM=' + BACKSLASHES + LINEFEEDS                  
                                                                                
       integer    maxnwrds                                                      
       parameter (maxnwrds=maxnbyt/kbytpwrd)                                    
                                                                                
C      ... call seq. args ...                                                   
       INTEGER       NCH_PARM                                                   
       CHARACTER*(*) CPARM                                                      
       integer       iret_parm                                                  
                                                                                
C                                                                               
C      ... FUNCTIONS ...                                                        
       external  lastch                                                         
       integer   lastch                                                         
       external  notrail                                                        
       integer   notrail                                                        
C      -------------------------------------------------------------            
       integer        jwork(maxnwrds)                                           
       character*112  cwork                                                     
       equivalence   (jwork,cwork)                                              
                                                                                
       integer(4)     nargsinline,iargc,iar
       integer        nchars                                                    
       integer        lmt_txt                                                   
       integer        non_parm                                                  
                                                                                
       LOGICAL        LPARMQQ                                                   
       character*1    KLF                                                       
       character*1    NULLCHR                                                   
       character*1    lonech                                                    
                                                                                
C      . . . . . . . .   S T A R T   . . . . . . . . . . . . . . . .            
                                                                                
       NULLCHR = char(0)                                                        
       KLF     = char(10)                                                       
C                                                                               
       iret_parm = 0                                                            
       non_parm = 0                                                             
                                                                                
       LPARMQQ = .FALSE.                                                        
       NCH_PARM = 0                                                             
                                                                                
       lmt_dest = len(cparm)                                                    
       write(6,103)lmt_dest                                                     
  103  format(1h ,'W3AS00: dimensioned size (in bytes) of dest strng=',         
     1             I11)                                                         
       if(lmt_dest .le. 0) then                                                 
         write(6,105)                                                           
  105    format(1h ,'W3AS00:FAILED on undefined destination ',                  
     1              'character string: CPARM')                                  
         iret_parm = -1                                                         
         nch_parm = 0                                                           
         go to 999                                                              
       else if (lmt_dest .gt. 101) then                                         
         lmt_dest = 101                                                         
       endif                                                                    
       lmt_txt = lmt_dest - 1                                                   
                                                                                
       cparm(1:lmt_dest) = ' '                                                  
                                                                                
       narg_got = 0                                                             
C                                                                               
       nargsinline = iargc()                                                    
                                                                                
       write(6,115) nargsinline                                                 
  115  format(1h ,'W3AS00: count of args found in command line =', I3)          
                                                                                
       if(nargsinline .gt. 0) then                                              
C        ... to scan every argument, looking only for the Arg which             
C        ...    starts with "PARM="                                             
         do  iar = 1,nargsinline                                                
           LPARMQQ = .FALSE.                                                    
                                                                                
           cwork(1:) = ' '                                                      
                                                                                
           call getarg(iar,cwork)                                               
                                                                                
           narg_got = narg_got + 1                                              
           nchars = lastch(cwork)                                               
                                                                                
           if(nchars .le. 0) then                                               
             write(6,125)iar                                                    
  125        format(1h ,'W3AS00:getarg() returned an empty arg for',            
     A                  ' no.',I3 )                                             
           else                                                                 
C            ... SOME TEXT EXISTS IN THIS ARG ...                               
C            ...   DOES IT START WITH "PARM=" ???                               
             if((cwork(1:5) .EQ. 'PARM=') .OR.                                  
     1          (cwork(1:5) .EQ. 'parm=') ) then                                
               LPARMQQ = .TRUE.                                                 
C              ... this arg is special case of PARM=                            
C              ... which can include blanks, so cannot lastch() it ...          
               nchars = notrail(cwork)                                          
             endif                                                              
C ...             iwdss = ((nchars-1)/kbytpwrd) + 1                             
C            ... where iwdss points to last word so I could hex dump            
C            ...    that last word, to see if NULL is there                     
C            ... There was no NULL; only blank fill.                            
             IF(LPARMQQ) THEN                                                   
C              ... FILTER OUT ANY BACKSLASH or LINE_FEED ...                    
               ioutc = 0                                                        
               do  inc = 6,nchars                                               
                 if(ioutc .LT. lmt_txt) then                                    
                   lonech = cwork(inc:inc)                                      
                   if((lonech .EQ. '\\') .OR.                                   
     1                (lonech .EQ. KLF)) then                                   
                   else                                                         
                     ioutc = ioutc + 1                                          
                     cparm(ioutc:ioutc) = lonech                                
                   endif                                                        
                 else                                                           
C                  ... comes here if ioutc .GE. lmt_txt,                        
C                  ... so I cannot increment ioutc for this inc char            
C                  ... so truncate the string at (1:ioutc)                      
C                  ... a warning be return-coded ...                            
                   iret_parm = +1                                               
                   go to 155                                                    
                 endif                                                          
               enddo                                                            
  155          continue                                                         
               nch_parm = ioutc                                                 
               np1 = nchars+1                                                   
               cparm(np1:np1) = NULLCHR                                         
               go to 999                                                        
C              ... jump out of DO when PARM has been processed ...              
             else                                                               
C              ... this is .not. a PARM field, do nothing w/ those,             
               non_parm = non_parm + 1                                          
             endif                                                              
                                                                                
           endif                                                                
         enddo                                                                  
C        ... IF IT FALLS THRU BOTTOM OF DO, THEN IT DID NOT FIND                
C        ...    THE PARM FIELD AMONG THE EXISTING ARGS                          
         iret_parm = 3                                                          
         nch_parm = 0                                                           
                                                                                
       ELSE                                                                     
C        ... COMES HERE IF nargsinline = 0, so there were no args at all        
         iret_parm = 2                                                          
         nch_parm = 0                                                           
       endif                                                                    
       go to 999                                                                
                                                                                
  999  continue                                                                 
       return                                                                   
       end                                                                      
        integer function lastch(str)                                            
C       ... lastch() ... to point to the last character of a character          
C       ...              string                                                 
C       ...        String terminators are first BLANK or NULL character         
C       ...        encountered.                                                 
C       ... Caution:  I will limit scan on LEN(str)                             
C                     so you must give me a character string.                   
C                                                                               
                                                                                
        character*(*) str                                                       
                                                                                
        character*1  NULLCHR                                                    
        character*1  BLANK                                                      
C                                                                               
        integer    i                                                          
        integer    limit                                                      
C                                                                               
        NULLCHR = char(0)                                                       
        BLANK = ' '                                                             
        limit = len(str)                                                        
        i = 0                                                                   
        do while(i .LT. limit .AND. str(i+1:i+1) .NE. NULLCHR                   
     1                        .AND. str(i+1:i+1) .NE. BLANK)                    
           i = i + 1                                                            
        enddo                                                                   
                                                                                
        lastch = i                                                              
        return                                                                  
        end                                                                     
        integer function notrail(str)                                         
C       ...       mods for CRAY version               8-Dec-1994/dss            
C                                                                               
C       ... notrail() ... to point to the last non-blank character of a         
C       ...               character string (which can have leading              
C                         blanks and intermediate blanks); but after            
C                         ignoring all trailing blank characters.               
C       ...        String terminators are last BLANK or first NULL              
C       ...        character encountered.                                       
C                                                                               
C       ...        This differs from LASTCH() which stops on first              
C       ...        BLANK encountered when scanning from the start;              
C       ...        NOTRAIL() will scan backwards from the end of the            
C       ...        string, skipping over trailing blanks, until the             
C       ...        last non-blank character is hit.                             
C       ...                                                                     
C       ... Caution:  I will limit scan on LEN(str)                             
C                     so you must give me a character string.                   
C                                                                               
                                                                                
        character*(*) str                                                       
                                                                                
        character*1  BLANK                                                      
        parameter   (BLANK = ' ')                                               
C                                                                               
        integer    i                                                          
        integer    limit                                                      
        integer    limitnl                                                    
        character*1  NULLCHR                                                    
C                                                                               
        NULLCHR = char(0)                                                       
        i = 0                                                                   
        limitnl = 0                                                             
        limit = len(str)                                                        
        if(limit .le. 0) go to 999                                              
C       ... otherwise, at least one char len string ...                         
        limitnl = index(str(1:limit),NULLCHR)                                   
        if(limitnl .le. 0) then                                                 
C         ... no NULLCHR exists in str(1:limit) ...                             
C         ... so go scan from limit                                             
          go to 300                                                             
                                                                                
        else if(limitnl .eq. 1) then                                            
          go to 999                                                             
C         ... which jumped out w/ pointer=0 if NULL in first position           
        else                                                                    
C         ... a NULLCHR existed within str(1:limit); so                         
C         ...   I want to scan backwards from before that NULLCHR               
C         ...   which is located at limitnl                                     
          limit = limitnl - 1                                                   
        endif                                                                   
        if(limit .le. 0) go to 999                                              
  300   continue                                                                
C       ... otherwise, we have a string of at least one char to look at         
C       ... which has no NULLCHR in interval (1:limit)                          
        i = limit                                                               
        do while((i .GT. 0) .AND. (str(i:i) .EQ. BLANK))                        
           i = i - 1                                                            
        enddo                                                                   
                                                                                
  999   continue                                                                
        notrail = i                                                             
        return                                                                  
        end                                                                     
