C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:    args_mod    Wrapper for routines iargc and getarg
C   PRGMMR: Iredell          ORG: W/NMC23    DATE: 98-11-DD
C
C ABSTRACT: This Fortran Module acts as a wrapper to the system
C   routines IARGC and GETARG.  Use of this module allows IARGC and
C   GETARG to work properly with 4-byte or 8-byte integer arguments.
C
C PROGRAM HISTORY LOG:
C   98-11-DD  Iredell
C
C USAGE:    use args_mod
C
C REMARKS: None
C
C ATTRIBUTES:
C   LANGUAGE: XL Fortran 
C   MACHINE:  IBM SP
C
C$$$
      module args_mod
        interface iargc
          module procedure iargc_8
        end interface
        interface getarg
          subroutine getarg(k,c)
            integer(4) k
            character*(*) c
          end subroutine getarg
          module procedure getarg_8
        end interface
      contains
        integer(8) function iargc_8()
          integer(4) iargc
          iargc_8=iargc()
        end function iargc_8
        subroutine getarg_8(k,c)
          integer(8) k
          character*(*) c
          integer(4) k4
          k4=k
          call getarg(k4,c)
        end subroutine getarg_8
      end module args_mod
