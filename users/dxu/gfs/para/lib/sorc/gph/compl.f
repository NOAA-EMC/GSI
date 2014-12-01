C
C DESCRIPTION
C The COMPL intrinsic function computes the complement of i.  
C
C Remarks: COMPL function is a CRAY SPECIFIC routine.
C          This routine is the IBM equivalent function
C          to CRAY's COMPL function
C Krishna Kumar tested this routine along with the gphlib
C routines on IBM RS/6000 1999-07-01 
C
       integer (kind=8) function compl(i)
       compl=not(i)
       return
       end
C
