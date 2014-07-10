c 
c   This program is to read data from GRIB file
c   Author: Binbin Zhou
c           Mar, 2005, NOAA/NCEP/EMC
c


      subroutine getTndGRIBdata (grbunit, indxunit,
     +         grbfile, indxfile, data,u,v,levels,
     +         numfcst,numvarbl,numlevel,ngrid,
     +         yy,mm,dd,hh,ff, k5,k6,k7,
     +         plevel, namvarbl, tendencymrk)

      include 'parm.inc'

      integer numfcst,numvarbl,numlevel,ngrid   

      real var(ngrid),
     + data(numfcst,numvarbl,numlevel,ngrid),
     + u(numfcst,numvarbl,numlevel,ngrid),
     + v(numfcst,numvarbl,numlevel,ngrid)

      integer k5(mxvrbl),k6(mxvrbl),k7(mxvrbl)
      integer plevel(maxlvl)
      integer levels(mxvrbl)          !levels for different variables
      integer tendencymrk(mxvrbl)
      integer yy(mxfcst), mm(mxfcst), dd(mxfcst), hh(mxfcst), 
     +        ff(mxfcst) 
      CHARACTER*24 namvarbl(mxvrbl)
                                                                                                                                                    
      dimension jpds(25),jgds(25),kpds(25),kgds(25),jjpds(25)          !grib
      dimension kens(5),kprob(2),xprob(2),kclust(16),kmembr(80) !grib extension
      integer yyfcst(mxfcst),yyobsv(maxobs)
      logical, allocatable, dimension(:)  :: lb

      integer grbunit, indxunit
      CHARACTER*80 grbfile, indxfile

      integer igribid
      COMMON /grb/igribid

c        grbunit=40
c        indxunit=41

          allocate(lb(ngrid))
              
        write(*,*) 'Tendency grib file:', 
     +        trim(grbfile), '  ',trim(indxfile)

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
        
         jgds=-1
         jpds=-1
         kgds=-1
        
         jpds(8)=yy(nfcst)
         jpds(9)=mm(nfcst)
         jpds(10)=dd(nfcst)
         Jpds(11)=hh(nfcst)
         jpds(14)=ff(nfcst)

c         if (igribid.gt.10) jpds(14)=ff(nfcst)

         write(*,*) 'Forecast time = ', nfcst

         do 2000 nvar = 1, numvarbl
 
           write(*,*) 'tendencymrk for var ', nvar, '=',
     +                 tendencymrk(nvar)

           if(tendencymrk(nvar).eq.0) goto 2000       

           jpds(5) = k5(nvar)
           jpds(6) = k6(nvar)
 
           jp = jpds(6)                                  !Binbin: these 2 lines are used to
           if(jpds(6).eq.100.or.jpds(6).eq.107) jp=100   !deal with both jpds=100 and jpds=107

           if(jp.eq.100) then
            levels(nvar) = numlevel
           else
            levels(nvar) = 1
           end if

           write(*,*) '  jpds=',jpds 

           if(jpds(5).ne.32) then

             do np = 1, levels(nvar)
               if(jp.eq.100) then
                jpds(7) = plevel(np)
               else
                jpds(7) = k7(nvar)
               end if
            
               call getgb(grbunit, indxunit, ngrid, 0, jpds, jgds,
     &                      kf, k, kpds, kgds, lb, var, iret)
               if(iret.ne.0) then
                 data(nfcst,nvar,np,:) = - 1.0E9
                 write(*,*)'read data error=',iret,' at ', jpds(7)
               else
                 data(nfcst,nvar,np,:)=var(:)
               end if
c                 write(*,'(4i6,10f10.2)') nfcst,(jpds(i),i=5,7),
c     &           (data(nfcst,nvar,np,j),j=1,10)
             end do

            else if(jpds(5).eq.32) then

             do np = 1, levels(nvar)

               jjpds=jpds

               if(jp.eq.100) then
                 jpds(7) = plevel(np)
               else
                 jpds(7) = k7(nvar)
               end if

               jpds(5)=33

               call getgb(grbunit, indxunit, ngrid, 0, jpds, jgds,
     &                      kf, k, kpds, kgds, lb, var, iret)
               if(iret.ne.0) then
                 u(nfcst,nvar,np,:) = - 1.0E9
                 write(*,*)'read u error=',iret,' at ', jpds(7)
               else
                 u(nfcst,nvar,np,:)=var(:)
               end if
                           
               jpds(5)=34

               call getgb(grbunit, indxunit, ngrid, 0, jpds, jgds,
     &                      kf, k, kpds, kgds, lb, var, iret)
               if(iret.ne.0) then
                 v(nfcst,nvar,np,:) = - 1.0E9
                 write(*,*)'read v error=',iret,' at ', jpds(7)
               else
                 v(nfcst,nvar,np,:)=var(:)
               end if
                           
                     
               data(nfcst,nvar,np,:) = sqrt(
     &            u(nfcst,nvar,np,:)*u(nfcst,nvar,np,:)+
     &            v(nfcst,nvar,np,:)*v(nfcst,nvar,np,:) )

c               write(*,*)'nfcst,nvar,np,u,v,wind:'                           
c               write(*,'(3i3, 3x, 3f9.2)') nfcst,nvar,np,
c     &           u(nfcst,nvar,np, 1001), v(nfcst,nvar,np, 1001),
c     &           data(nfcst,nvar,np,1001)
                           
             end do

            else
             write(*,*) 'Wind setting in control file has problem'
            end if
                           
2000     continue          

         end do
                
        call baclose(grbunit, ierr)
        call baclose(indxunit, ierr)

          deallocate(lb)

        return
        end

        


      
