! derive mean analysis from multiple models
! Fanglin Yang, NCEP/EMC, Nov 2010
!
      program mean_anl  

      real, allocatable :: var(:,:),var1(:)                            
      real, allocatable :: varm(:)                            
      logical*1, allocatable::  lb(:)
      character*200 :: argument    ! space for command-line argument
      character*200 :: gribfile, string, outname
      integer :: ipds(200),igds(200),iens(5)
      integer :: jpds(200),jgds(200),jens(5)
      integer :: kpds(200),kgds(200),kens(5)
      real    :: bad
      data bad /-1.0E+9/



      nargs = iargc()              ! iargc() - number of arguments
      call getarg(1,argument)      !number of experiments      
      read(argument,*) nexp
      call getarg(2,argument)      !grib kpds5, variable   
      read(argument,*) jpds5
      call getarg(3,argument)      !grib kpds6, variable type   
      read(argument,*) jpds6
      call getarg(4,argument)      !grib kpds7, variable level   
      read(argument,*) jpds7
      call getarg(5,argument)      !number of points           
      read(argument,*) npts
!     write(6,*)"nexp,jpds5,jpds6,jpds7,npts"
!     write(6,*) nexp,jpds5,jpds6,jpds7,npts

      allocate ( var(npts,nexp), var1(npts) )
      allocate ( varm(npts)     )
      allocate ( lb(npts)       )


      icount=0
      varm=0.0

      do n=1,nexp

        jgds=-1;  jpds=-1
        igds=-1;  ipds=-1
        jpds(5)=jpds5
        jpds(6)=jpds6
        jpds(7)=jpds7

       nf=10+n
       write(string,'(i0)')n
       gribfile="input"//trim(string) 
       call baopenr(nf,gribfile,iret)
       if (iret .ne. 0) write(6,*)" failed to open " , gribfile

       if (iret .eq. 0) then
        call getgb(nf,0,npts,-1,jpds,jgds,kf,k,kpds,kgds,lb,var1,iret)
!       write(6,*) "kpds(1-20): ", (kpds(i),i=1,20)
       endif

       if (iret .eq. 0) then
         icount=icount+1
         do i=1,npts
!         var(n,i)=var1(i)
          varm(i)=varm(i)+var1(i)
         enddo
         ipds=kpds
         igds=kgds
       endif
      enddo
      
      if(icount.ne.0) then
        do i=1,npts
         varm(i)=varm(i)/icount 
        enddo
      else
        do i=1,npts
         varm(i)=bad                
        enddo
      endif
        

      outname="outtmp"
      call baopen(99,trim(outname),iret)
      call putgbe(99,npts,ipds,igds,iens,lb,varm,iret)
       if (iret.ne.0) write(6,*) "write grib failed"          

      deallocate ( var, var1 )
      deallocate ( varm     )
      deallocate ( lb       )

      stop
      end
