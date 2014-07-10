	subroutine getMeanClimData(data,uclim,vclim,
     +         levels,numfcst,numvfyobs,numvarbl,numlevel,ngrid,
     +         yy,mm,dd,hh,ff, k5,k6,k7,
     +         plevel, namvarbl,anomly_mrk,anomlylev,
     +         cmm,cdd)


      include 'parm.inc'
                                                                                                                                             
      integer numvfyobs,numfcst,numvarbl,numlevel,ngrid
                                                                                                                                             
      real var(ngrid),
     + data(numfcst,numvarbl,numlevel,ngrid),
     + u(2,numlevel,ngrid),v(2,numlevel,ngrid)
     
      real uclim(numfcst,numvarbl,numlevel,ngrid),
     +     vclim(numfcst,numvarbl,numlevel,ngrid)
                                                                                                                                            
      integer k5(mxvrbl),k6(mxvrbl),k7(mxvrbl)
      integer plevel(maxlvl)
      integer levels(mxvrbl)          !levels for different variables
      integer yy(maxobs), mm(maxobs), dd(maxobs), hh(maxobs),
     +        ff(maxobs)
      CHARACTER*24 namvarbl(mxvrbl)
      CHARACTER*2  cmm(maxobs),cdd(maxobs)      
                                                                                                                                      
      dimension jpds(25),jgds(25),kpds(25),kgds(25),jjpds(25)          !grib
      dimension kens(5),kprob(2),xprob(2),kclust(16),kmembr(80) !grib extension
      integer yyfcst(mxfcst),yyobsv(maxobs)
      logical, allocatable, dimension(:)  :: lb
                                                                                                                                             
      CHARACTER*80 grbfile, indxfile
      integer grbunit, indxunit

      integer h(5),anomly_mrk(mxvrbl),h12(2),
     +             anomlylev(mxvrbl,maxlvl)
      real cintp(2,numlevel,ngrid)


 
      data (h(i),i=1,5)
     + /0, 6, 12, 18, 24/

      allocate(lb(ngrid))

      data = 0.0

      grbunit=201
      indxunit=202

      write(*,*) ' In getMeanClimData' 


      do 2000 nobsv = 1, numvfyobs

       write(*,*) 'Observ time = ', nobsv

       grbfile='climat.1959'//cmm(nobsv)//cdd(nobsv)
       indxfile='climat.1959'//cmm(nobsv)//cdd(nobsv)//'.indx'

       write(*,*) trim(grbfile),' ',trim(indxfile)

       call baopen(grbunit,grbfile, ierr)
       if(ierr.ne.0) then
        write(*,*)'open climat grib file ',trim(grbfile), ' error'
        stop 118
       end if

       call baopen(indxunit,indxfile, ierr)
       if(ierr.ne.0) then
        write(*,*) 'open climat index file ',trim(indxfile), ' error'
        stop 218
       end if


       !get which two cycles        
       do i=1,4
        if(hh(nobsv).ge.h(i).and.hh(nobsv).lt.h(i+1)) then
          h12(1)=h(i)
          h12(2)=h(i+1)
          if(h12(2).eq.24) h12(2)=0
        end if
       end do
 
         jgds=-1
         jpds=-1
         kgds=-1
                                                                                                                                             
         jpds(8)=59
         jpds(9)=mm(nobsv)
c         jpds(10)=dd(nobsv)

         write(*,*)'h1,h2=',h12(1),h12(2)
         write(*,*)'anomly_mrk=',(anomly_mrk(nvar),nvar=1,numvarbl)

        do 1000 nvar = 1, numvarbl

         jpds(14)=0

         jpds(5) = k5(nvar)
         jpds(6) = k6(nvar)

         if(anomly_mrk(nvar).eq.0) goto 1000

          write(*,*) 'nvar=', nvar, ' jpds(5)=',jpds(5)

          jp = jpds(6)                                  !Binbin: these 2 lines are used to
          if(jpds(6).eq.100.or.jpds(6).eq.107) jp=100   !deal with both jpds=100 and jpds=107
          
          if(jp.eq.100) then
            levels(nvar) = numlevel
          else
            levels(nvar) = 1
          end if


          !get 2 cycle's climatologic data      

          do 800 mh=1,2

           cintp(mh,:,:) = 0.

           write(*,*) 'mh=',mh, 'h12(mh)=', h12(mh)  

           jpds(11)=h12(mh)

           if(jpds(5).ne.32) then

              do 400 np = 1, levels(nvar)
              
               if(anomlylev(nvar,np).eq.0) goto 400

               if(jp.eq.100) then
                jpds(7) = plevel(np)
               else
                jpds(7) = k7(nvar)
                !based on YueJian's data set, T2m, Tmax, Tmin,U10,V10, are diagnositic,
                !and cycle-time should be substrcted by 6 hr
                if(jpds(5).eq.11.or.jpds(5).eq.15.or.
     +             jpds(5).eq.16.or.jpds(5).eq.33.or.
     +             jpds(5).eq.34) then
                   jpds(11) = jpds(11) - 6
                   if (jpds(11).lt.0) jpds(11) = 18
                   jpds(14) = 6
                 end if
               end if

               write(*,*) '   jpds',jpds

               call getgb(grbunit, indxunit, ngrid, 0, jpds, jgds,
     &                      kf, k, kpds, kgds, lb, var, iret)
               if(iret.ne.0) then
                 cintp(mh,np,:) = - 1.0E9
                 write(*,*)'read data error=',iret,  
     &              'for mh=',mh,' at ', jpds(7)
                else
                 cintp(mh,np,:)=var(:)
               end if

c               write(*,*) ' cintp:',h12(mh), 'level=', jpds(7)
c               write(*,*) (cintp(mh,np,i),i=1,5)

400           continue

            else if(jpds(5).eq.32) then

              do 500 np = 1, levels(nvar)

               jjpds=jpds
               if(jp.eq.100) then
                jjpds(7) = plevel(np)
               else
                jjpds(7) = k7(nvar)
                !based on YueJian's data set, W10m are diagnositic,
                !and cycle-time should be substrcted by 6 hr 
                jjpds(11) = jjpds(11) - 6
                if (jjpds(11).lt.0) jjpds(11) = 18
                jjpds(14) = 6
               end if

               if(anomlylev(nvar,np).eq.0) goto 500

               jjpds(5)=33

               write(*,*) '   jjpds',jjpds

               call getgb(grbunit, indxunit, ngrid, 0, jjpds, jgds,
     &                      kf, k, kpds, kgds, lb, var, iret)
               if(iret.ne.0) then
                 u(mh,np,:) = - 1.0E9
                 write(*,*)'read u error=',iret,
     &           ' for mh=', mh, ' at ',jjpds(7)
               else
                 u(mh,np,:)=var(:)
               end if

               jjpds=jpds
               if(jp.eq.100) then
                jjpds(7) = plevel(np)
               else
                jjpds(7) = k7(nvar)
                jjpds(11) = jjpds(11) - 6
                if (jjpds(11).lt.0) jjpds(11) = 18
                jjpds(14) = 6
               end if

               jjpds(5)=34

               write(*,*) '   jjpds',jjpds

               call getgb(grbunit, indxunit, ngrid, 0, jjpds, jgds,
     &                      kf, k, kpds, kgds, lb, var, iret)
               if(iret.ne.0) then
                 v(mh,np,:) = - 1.0E9
                 write(*,*)'read v error=',iret,
     &           ' for mh=', mh, ' at ',jjpds(7)
               else
                 v(mh,np,:)=var(:)
               end if

               cintp(mh,np,:)=sqrt(
     &            u(mh,np,:)*u(mh,np,:)+
     &            v(mh,np,:)*v(mh,np,:) )

c               write(*,*) ' cintp:',h12(mh), 'level=', jjpds(7)
c               write(*,*) (cintp(mh,np,i),i=1,5)
c               write(*,*) 'u(mh,np,:), v(mh,np,:)=',
c     +         (u(mh,np,i),i=1,5),(v(mh,np,i),i=1,5)

500           continue

           end if

800       continue !end of 2 mh
       
         !get weighted avearge of climatologic mean
 
          do 900 np = 1, levels(nvar)

           if(anomlylev(nvar,np).eq.0) goto 900

           data(nobsv,nvar,np,:)=cintp(1,np,:)+
     &      (cintp(2,np,:)-cintp(1,np,:))
     &      /(h12(2)-h12(1))*(hh(nobsv)-h12(1))

c       write(*,'(a14,i5, 5f10.2)') 'data at level ', np,
c     &     (data(nobsv,nvar,np,i),i=1,5)

           if(jpds(5).eq.32) then
            uclim(nobsv,nvar,np,:)=u(1,np,:)+
     &      (u(2,np,:)-u(1,np,:))
     &      /(h12(2)-h12(1))*(hh(nobsv)-h12(1))

            vclim(nobsv,nvar,np,:)=v(1,np,:)+
     &      (v(2,np,:)-v(1,np,:))
     &      /(h12(2)-h12(1))*(hh(nobsv)-h12(1))

c       write(*,'(a21,i5,10f8.2)')'uclim,vclim at level ',np,
c     &       (uclim(nobsv,nvar,np,i),i=1,5),
c     &       (vclim(nobsv,nvar,np,i),i=1,5)
           end if

900        continue

1000      continue      !end of nvar
2000     continue       !end of nobsv          
 
        if(numvfyobs.eq.1) then
          do nfcst = 2, numfcst
            data(nfcst,:,:,:)=data(1,:,:,:)
            uclim(nfcst,:,:,:)=uclim(1,:,:,:)
            vclim(nfcst,:,:,:)=vclim(1,:,:,:)
          end do
        end if
 
               
        call baclose(grbunit, ierr)
        call baclose(indxunit, ierr)
                                                                                                                      
          deallocate(lb)

                
        return
        end

