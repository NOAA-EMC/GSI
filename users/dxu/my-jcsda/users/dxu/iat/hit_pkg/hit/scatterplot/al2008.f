! read track errors, write out for making scatter plots

      integer, parameter :: nmd=5           !number of models 
      integer, parameter :: nhr=8           !forecast hours: 00, 12, 24, 36, 48, 72, 96, 120
      real*4         :: terr(nhr, nmd)
      character*4    :: run(nmd)
      character      :: subname(80)
      data bad /9999.0/

      open (1, file="al2008.txt",form="formatted",status="old")
      open (2, file="al2008.bin",form="unformatted",status="unknown")

      icount=0
 100  continue
      read(1,'(80a)',end=200) subname
        write(10,'(80a)') subname
      read(1,'(80a)',end=200) subname
        write(10,'(80a)') subname
      do m=1,nmd
      read(1,'(x,a,3x,8f7.1)',end=200) run(m),(terr(n,m),n=1,nhr)
        write(10,'(x,a,3x,8f7.1)') run(m),(terr(n,m),n=1,nhr)
      enddo 
      read(1,*,end=200)
        write(10,*)

      write(2)((terr(n,m),n=1,nhr),m=1,nmd)
      icount=icount+1
      goto 100

200   continue
      write(10,*)"icount=",icount
      write(99,*)icount

      stop
      end

