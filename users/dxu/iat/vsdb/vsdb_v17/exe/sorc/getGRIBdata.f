c 
c   This program is to read data from GRIB file
c   Author: Binbin Zhou
c           Mar, 2005, NOAA/NCEP/EMC
c


      subroutine getGRIBdata (grbunit, indxunit,
     +         grbfile, indxfile, data,u,v,levels,
     +         numfcst,numvarbl,numlevel,ngrid,
     +         yy,mm,dd,hh,ff, k5,k6,k7,
     +         plevel, namvarbl)

      include 'parm.inc'

      integer numfcst,numvarbl,numlevel,ngrid   

      real var(ngrid),
     + data(numfcst,numvarbl,numlevel,ngrid),
     + u(numfcst,numvarbl,numlevel,ngrid),
     + v(numfcst,numvarbl,numlevel,ngrid)

      integer k5(mxvrbl),k6(mxvrbl),k7(mxvrbl)
      integer plevel(maxlvl)
      integer levels(mxvrbl)          !levels for different variables
      integer yy(mxfcst), mm(mxfcst), dd(mxfcst), hh(mxfcst), 
     +        ff(mxfcst) 
      CHARACTER*24 namvarbl(mxvrbl)
                                                                                                                                                    
      integer jpds(25),jgds(25),kpds(25),kgds(25),jjpds(25)          !grib
      integer kens(5),kprob(2),xprob(2),kclust(16),kmembr(80) !grib extension
      integer yyfcst(mxfcst),yyobsv(maxobs)
      logical, allocatable, dimension(:)  :: lb


      integer grbunit, indxunit
      CHARACTER*80 grbfile, indxfile

      integer nodata(mxfcst,mxvrbl,maxlvl)
      COMMON /nofile/nodata

      integer igribid
      COMMON /grb/igribid

          allocate(lb(ngrid))
 
         write(*,*)trim(grbfile),' ',trim(indxfile),
     +             grbunit,indxunit


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



        do nfcst = 1, numfcst

C         write(*,*) 'igribid=',igribid
C         if(igribid.gt.10) jpds(14)=ff(nfcst)           !skip GFS grib


!!       write(*,*) 'Forecast time = ',  nfcst

         do nvar = 1, numvarbl

!!       write(*,*) 'Variable#', nvar
         jgds=-1
         jpds=-1
         kgds=-1
         kpds=0                                   
                                                                                                     
         jpds(8)=yy(nfcst)
         jpds(9)=mm(nfcst)
         jpds(10)=dd(nfcst)
         jpds(11)=hh(nfcst)   !cycle time
          jpds(14)=ff(nfcst)   !forecast time (lead time)

         jpds(5) = k5(nvar)
         jpds(6) = k6(nvar)


           jp = jpds(6)                                  !Binbin: these 2 lines are used to
           if(jpds(6).eq.100.or.jpds(6).eq.107) jp=100   !deal with both jpds=100 and jpds=107

           if (jp.eq.100) then
            levels(nvar) = numlevel
           else
            levels(nvar) = 1
           end if

!!         write(*,*) 'Var for ', jpds(5),jpds(6),
!!   +     'jp, levels =', jp, levels(nvar)

 
           if(jpds(5).ne.32) then

            do np = 1, levels(nvar)

             if (jp.eq.100) then
              jpds(7) = plevel(np)
             else
              jpds(7) = k7(nvar)
             end if

!!           write(*,*)  ' before    jpds=',jpds
             call getgb(grbunit, indxunit, ngrid, 0, jpds, jgds,
     &                      kf, k, kpds, kgds, lb, var, iret)
!!           write(*,*) ' after     jpds=',jpds
!!           write(*,*) ' after     kpds=',kpds

             if(iret.ne.0) then
               data(nfcst,nvar,np,:) = - 1.0E9
               nodata(nfcst,nvar,np) = 1 
               write(*,*)'read error=',iret,' at ',jpds(7) 
             else
               if(jpds(5).ne.154) then
                data(nfcst,nvar,np,:)=var(:)
               else
                !! scale O3 mixing ratio to ppmg
                data(nfcst,nvar,np,:)=1.0E6*var(:)
               endif
             end if
!!             write(*,'(4i6,10f9.2)') nfcst,(jpds(i),i=5,7),
!!   &         (data(nfcst,nvar,np,j),j=1,10)

c              do j = 1, ngrid
c               if(data(nfcst,nvar,np,j).gt.0.0) then
c                write(*,*) j, data(nfcst,nvar,np,j)
c               end if
c              end do


            end do

           else if(jpds(5).eq.32) then

            do np = 1, levels(nvar)

             jjpds(:)=jpds(:)      !This is needed just for gefs reading U/V? Don't delete this line    

             if (jp.eq.100) then
              jpds(7) = plevel(np)
             else
              jpds(7) = k7(nvar)
             end if

              jpds(5)=33

c              write(*,*) ' before u jpds=',jpds
              call getgb(grbunit, indxunit, ngrid, 0, jpds, jgds,
     &                      kf, k, kpds, kgds, lb, var, iret)

c              write(*,*) ' after u jpds=',jpds

              if(iret.ne.0) then
                u(nfcst,nvar,np,:) = - 1.0E9
                nodata(nfcst,nvar,np) = 1
                write(*,*) '   read u error=',iret,' at ',jpds(7)
              else
                u(nfcst,nvar,np,:)=var(:)
!!             write(*,'(4i6,10f9.2)') nfcst,(jpds(i),i=5,7),
!!   &         (u(nfcst,nvar,np,j),j=1,10)
              end if

              jpds(5)=34

c              write(*,*) ' before v jpds=',jpds
              call getgb(grbunit, indxunit, ngrid, 0, jpds, jgds,
     &                      kf, k, kpds, kgds, lb, var, iret)
 
c              write(*,*) ' after v jpds=',jpds
 
              if(iret.ne.0) then
                v(nfcst,nvar,np,:) = - 1.0E9
                nodata(nfcst,nvar,np) = 1
                write(*,*) '   read u error=',iret,' at ',jpds(7)
              else
                v(nfcst,nvar,np,:)=var(:)
!!             write(*,'(4i6,10f9.2)') nfcst,(jpds(i),i=5,7),
!!   &         (v(nfcst,nvar,np,j),j=1,10)
              end if
                   

 
              data(nfcst,nvar,np,:) = sqrt(
     &           u(nfcst,nvar,np,:)*u(nfcst,nvar,np,:)+
     &           v(nfcst,nvar,np,:)*v(nfcst,nvar,np,:) )
                          
c              write(*,*)'nfcst,nvar,np,u,v,wind:'  
c              write(*,'(3i3, 3x, 3f9.2)') nfcst,nvar,np,
c     &          u(nfcst,nvar,np, 1), v(nfcst,nvar,np, 1),
c     &          data(nfcst,nvar,np,1)
                           
            end do
  
           else
            write(*,*) ' No data are read! '
           end if

          end do
         end do
                
        call baclose(grbunit, ierr)
        call baclose(indxunit, ierr)

        deallocate(lb)

        return
        end

        


      
