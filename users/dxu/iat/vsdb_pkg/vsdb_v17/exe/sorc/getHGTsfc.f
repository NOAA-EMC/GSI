c       real HGTsfc(96673)
c       ngrid=96673
c       call getHGTsfc(HGTsfc,ngrid)
c       stop
c       end       

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  This subroutine is to get surface height
c  Author: Binbin Zhou
c          May, 2005   
c
       subroutine getHGTsfc(HGTsfc,ngrid)
  
       real HGTsfc(ngrid)
       dimension jpds(25),jgds(25),kpds(25),kgds(25)          !grib
       logical, allocatable, dimension(:)  :: lb

       integer grbunit, indxunit
       CHARACTER*80 grbfile, indxfile

        grbunit=35
        indxunit=36
        grbfile='sfc.grib'
        indxfile='sfc.indx'

       allocate(lb(ngrid))

       call baopen(grbunit,grbfile, ierr)
       if(ierr.ne.0) then
          write(*,*) 'open grib file ',trim(grbfile), ' error'
          stop 98
        end if
        call baopen(indxunit,indxfile, ierr)
        if(ierr.ne.0) then
          write(*,*) 'open index file ',trim(indxfile), ' error'
          stop 99
        end if

         jgds=-1
         jpds=-1
         kgds=-1
         jpds(5)=7
         jpds(6)=1
         jpds(7)=0

         call getgb(grbunit, indxunit, ngrid, 0, jpds, jgds,
     &              kf, k, kpds, kgds, lb, HGTsfc, iret)

          if(iret.ne.0) then
             write(*,*)'no sfc height data'
                stop 97 
           end if

        call baclose(grbunit, ierr)
        call baclose(indxunit, ierr)
                                                                                                                                               
        deallocate(lb)
                                                                                                                                               
        return
        end

