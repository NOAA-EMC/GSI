       LOGICAL FUNCTION ISALPHA(C)
C      ... =.TRUE. IF THE ONE CHARACTER IS ANY LETTER OR DIGIT ...
       CHARACTER*1 C
       ISALPHA = ((LGE(C,'0') .AND. LLE(C,'9'))
     1       .OR. (LGE(C,'a') .AND. LLE(C,'z'))
     2       .OR. (LGE(C,'A') .AND. LLE(C,'Z')))
       RETURN
       END
