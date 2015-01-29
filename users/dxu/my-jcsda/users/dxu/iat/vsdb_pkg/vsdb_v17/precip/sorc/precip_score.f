!------------------------------------------
      program precip_score
!------------------------------------------
! Fanglin Yang, December 2009
! EMC/NCEP, fanglin.yang@noaa.gov
!
! This program reads in FHO data saved in plain ascii format used 
! in NCEP/ECM Global Branch, computes precip skill scores,  and uses 
! resampling (Monte Carlo)  method to assess the significance of 
! score differences between two models 
!   Biase Score:             BIS = (a+b)/(a+c)
!   Equitable Threat Score:  ETS = (a-R(a))/(a+b+c-R(a))
!           where R(a)=(a+b)*(a+c)/(a+b+c+d)
!   which based on
!       ------------------------------------
!       |                |OBS YES | OBS NO |
!       |----------------------------------|
!       | FCST YES       |   a    |   b    |
!       |----------------------------------|
!       | FCST NO        |   c    |   d    |
!       ------------------------------------
! a-hit; b-false alram; c-miss; d-no event
!

      integer, parameter :: ncat=9      !precip intensity: 0.2 2 5 10 15 25 35 50 75 mm/24hrs
      integer, parameter :: nfhr=3      !fcst legth, 12-36hr, 36-60hr, 60-84hr for 00Z cycle
                                        !fcst legth, 00-24hr, 24-48hr, 48-72hr for 12Z cycle

      integer, allocatable :: hit(:,:,:,:),obs(:,:,:,:),fcs(:,:,:,:),tot(:,:,:,:)
      real, allocatable :: aa(:,:,:,:),bb(:,:,:,:),cc(:,:,:,:),dd(:,:,:,:)   !contigency table for daily samples
      real, allocatable :: taa(:,:,:), tbb(:,:,:), tcc(:,:,:), tdd(:,:,:)    !contigency table for total samples
      real, allocatable :: dbis(:,:,:,:),dets(:,:,:,:)                       !skill scores for daily samples
      real, allocatable :: mbis(:,:,:),  mets(:,:,:)                         !mean of daily skill scores 
      real, allocatable :: vbis(:,:,:),  vets(:,:,:)                         !standard deviation of daily skill scores 
      real, allocatable :: bis(:,:,:),   ets(:,:,:)                          !skill scores for all samples
      real, allocatable :: ngood(:,:,:)                                      !numbers of days with valid scores  
      real, allocatable :: obcount(:,:,:)                                    !observed counts above threshold for all days    
      integer, allocatable :: missday(:)                                     !used to mark days with missing values
      integer, allocatable :: ncount(:,:,:,:)                                !index of days with valid scores  

      character*200 :: dataname, string
      character*200 :: argument    ! space for command-line argument

! arrays used for monte carlo test
      real, allocatable :: randx(:) 
      real, allocatable :: aa1(:,:,:), bb1(:,:,:), cc1(:,:,:), dd1(:,:,:)  !model 1 from random selection
      real, allocatable :: aa2(:,:,:), bb2(:,:,:), cc2(:,:,:), dd2(:,:,:)  !model 2 from random selection
      real, allocatable :: taa1(:,:), tbb1(:,:), tcc1(:,:), tdd1(:,:)      !contigency table for all samples
      real, allocatable :: taa2(:,:), tbb2(:,:), tcc2(:,:), tdd2(:,:)      !contigency table for all samples
      real, allocatable :: bis1(:,:,:),   ets1(:,:,:)                   
      real, allocatable :: bis2(:,:,:),   ets2(:,:,:)                   
      real, allocatable :: bisd(:,:,:),   etsd(:,:,:)                      !diference of scores
      real, allocatable :: std_bis(:,:,:), std_ets(:,:,:)                  !standard deviation of scores
      integer cycle

      data bad/-9999.0e+0/

!--------------------------------------------------------
!--------------------------------------------------------
      nargs = iargc()   ! iargc() - number of arguments 
      call getarg(1,argument)      !sample days
      read(argument,*) nday       
      call getarg(2,argument)      !number of models
      read(argument,*) nexp       
      call getarg(3,argument)      !verification region
      read(argument,*) nreg       
      call getarg(4,argument)      !number of monte carlo tests
      read(argument,*) ntest       
      if (nargs.ge.5) then
       call getarg(5,argument)      !forecast cycles                
       read(argument,*) cycle       
      else
       cycle=0
      endif
      write(6,*) "nday=",nday," nexp=",nexp, " nreg=",nreg, " ntest=",ntest, " cycle=",cycle


      allocate ( hit(ncat,nfhr,nday,nexp), obs(ncat,nfhr,nday,nexp), fcs(ncat,nfhr,nday,nexp), tot(ncat,nfhr,nday,nexp) )
      allocate ( aa(ncat,nfhr,nday,nexp),  bb(ncat,nfhr,nday,nexp),  cc(ncat,nfhr,nday,nexp),  dd(ncat,nfhr,nday,nexp) )
      allocate ( taa(ncat,nfhr,nexp),      tbb(ncat,nfhr,nexp),      tcc(ncat,nfhr,nexp),      tdd(ncat,nfhr,nexp) )  
      allocate ( dbis(ncat,nfhr,nday,nexp), dets(ncat,nfhr,nday,nexp) )
      allocate ( bis(ncat,nfhr,nexp),       ets(ncat,nfhr,nexp) )
      allocate ( mbis(ncat,nfhr,nexp),      mets(ncat,nfhr,nexp) )
      allocate ( vbis(ncat,nfhr,nexp),      vets(ncat,nfhr,nexp) )
      allocate ( ngood(ncat,nfhr,nexp), ncount(ncat,nfhr,nday,nexp), obcount(ncat,nfhr,nexp) )
      allocate ( missday(nday) )


      hit=bad; obs=bad; fcs=bad; tot=bad        
       aa =0.0;  bb =0.0;  cc=0.0;  dd=0.0
      taa =0.0; tbb =0.0; tcc=0.0; tdd=0.0
      dbis=0.0; dets=0.0
      mbis=0.0; mets=0.0
      vbis=bad; vets=bad
      bis=0.0;  ets=0.0
      ngood=0.0; ncount=-1; obcount=0.0
      missday=0
   
!-------------------------------------------------------------------
!-------------------------------------------------------------------
!--read data
      do 10 np=1,nexp
       write(string,'(i0)')np
       open(2,file="filelist"//trim(string),form="formatted",status="unknown")
      do 20 id = 1, nday                    
       read(2,*) dataname
       open(11,file=trim(dataname),form="formatted",status="old",iostat=iostat)
       print*, "open  ", trim(dataname), " status: ",iostat

       if(iostat .ne. 0) goto 25
       do 30 ih = 1, nfhr     !fcst hour
         ihour=ih*24+12-cycle
         rewind 11
         line2skip=(ih-1)*11*4+nreg*4
         do lskip = 1, line2skip
           read (11,900,end=25)   
         enddo
         read (11,902,end=25) ifhour                         
         if (ifhour.ne.ihour) goto 25 
         do line=1,3
          nn=3*(line-1)
          read (11,901,end=25) obs(nn+1,ih,id,np),fcs(nn+1,ih,id,np), hit(nn+1,ih,id,np),tot(nn+1,ih,id,np), &
                        obs(nn+2,ih,id,np),fcs(nn+2,ih,id,np), hit(nn+2,ih,id,np),tot(nn+2,ih,id,np), &
                        obs(nn+3,ih,id,np),fcs(nn+3,ih,id,np), hit(nn+3,ih,id,np),tot(nn+3,ih,id,np) 
         enddo

          do ic = 1, ncat
           aa(ic,ih,id,np) = hit(ic,ih,id,np)
           bb(ic,ih,id,np) = fcs(ic,ih,id,np) - hit(ic,ih,id,np)
           cc(ic,ih,id,np) = obs(ic,ih,id,np) - hit(ic,ih,id,np)
           dd(ic,ih,id,np) = tot(ic,ih,id,np) - fcs(ic,ih,id,np) - obs(ic,ih,id,np) + hit(ic,ih,id,np)
          enddo
          do ic = 1, ncat
           taa(ic,ih,np)=taa(ic,ih,np)+aa(ic,ih,id,np)
           tbb(ic,ih,np)=tbb(ic,ih,np)+bb(ic,ih,id,np)
           tcc(ic,ih,np)=tcc(ic,ih,np)+cc(ic,ih,id,np)
           tdd(ic,ih,np)=tdd(ic,ih,np)+dd(ic,ih,id,np)
          enddo
 30    continue 
       goto 26
 25    missday(id)=1
 26    close(11)
 20   continue
 10   continue

      do 16 np = 1,nexp
      do 16 ih = 1, nfhr   
      do 16 ic = 1, ncat
      do 16 id = 1, nday                    
       if(missday(id).ne.1) then
        taa(ic,ih,np)=taa(ic,ih,np)+aa(ic,ih,id,np)
        tbb(ic,ih,np)=tbb(ic,ih,np)+bb(ic,ih,id,np)
        tcc(ic,ih,np)=tcc(ic,ih,np)+cc(ic,ih,id,np)
        tdd(ic,ih,np)=tdd(ic,ih,np)+dd(ic,ih,id,np)
       endif
 16   continue 

 900  format (1x)
 901  format (6x,4(i5),6x,4(i5),6x,4(i5))         
 902  format (32x,i3)                             

!-------------------------------------------------------------------
!--compute skill scores
      do 50 np = 1, nexp
      do 50 ih = 1, nfhr
      do 50 ic = 1, ncat
         do 60 id = 1, nday
          if (tot(ic,ih,id,np).eq.-9999.0 .or. missday(id).eq.1 ) then
           dbis(ic,ih,id,np)=bad                        
           dets(ic,ih,id,np)=bad                                 
          else
           if (tot(ic,ih,id,np).eq.0) goto 60  
           ra=(aa(ic,ih,id,np) + bb(ic,ih,id,np)) * (aa(ic,ih,id,np) + cc(ic,ih,id,np)) / tot(ic,ih,id,np)
           rb=(aa(ic,ih,id,np) + cc(ic,ih,id,np)) * (bb(ic,ih,id,np) + dd(ic,ih,id,np))
           rd=(aa(ic,ih,id,np) + cc(ic,ih,id,np))
!!           print *, "model=",np, " fhour=",12+24*ih, " icat=",ic, " day=",id, " R(a)=",ra      
           if (rd.eq.0.0.or.rb.eq.0.0) goto 60  
           ncount(ic,ih,id,np)=1                                            
           ngood(ic,ih,np)=ngood(ic,ih,np) + 1                          
           dbis(ic,ih,id,np)=(aa(ic,ih,id,np) + bb(ic,ih,id,np)) / rd
           dets(ic,ih,id,np)=(aa(ic,ih,id,np) - ra) / (aa(ic,ih,id,np) + bb(ic,ih,id,np) + cc(ic,ih,id,np) - ra)
           mbis(ic,ih,np)=mbis(ic,ih,np) + dbis(ic,ih,id,np)                          
           mets(ic,ih,np)=mets(ic,ih,np) + dets(ic,ih,id,np)                          
          endif
 60      continue
         
       !derive standard deviations of daily skill scores
         if(ngood(ic,ih,np) .gt. 1) then    
           mbis(ic,ih,np)=mbis(ic,ih,np) / ngood(ic,ih,np)                           
           mets(ic,ih,np)=mets(ic,ih,np) / ngood(ic,ih,np)                           
           do id = 1, nday
           if (ncount(ic,ih,id,np) .eq. 1) then
            vbis(ic,ih,np)=vbis(ic,ih,np)+(dbis(ic,ih,id,np)-mbis(ic,ih,np))**2                                          
            vets(ic,ih,np)=vets(ic,ih,np)+(dets(ic,ih,id,np)-mets(ic,ih,np))**2                                          
           endif
           enddo
           vbis(ic,ih,np)=sqrt( vbis(ic,ih,np)/(ngood(ic,ih,np)-1) )
           vets(ic,ih,np)=sqrt( vets(ic,ih,np)/(ngood(ic,ih,np)-1) )
         else
           mbis(ic,ih,np)=bad
           mets(ic,ih,np)=bad 
           vbis(ic,ih,np)=bad
           vets(ic,ih,np)=bad 
         endif


       !compute skill scores with all samples together
         obcount(ic,ih,np) = taa(ic,ih,np)+tcc(ic,ih,np)
         ttot=taa(ic,ih,np)+tbb(ic,ih,np)+ tcc(ic,ih,np)+tdd(ic,ih,np)
         if (ttot.eq.0.0) goto 40
         ra=(taa(ic,ih,np) + tbb(ic,ih,np)) * (taa(ic,ih,np) + tcc(ic,ih,np)) / ttot
         rb=(taa(ic,ih,np) + tcc(ic,ih,np)) * (tbb(ic,ih,np) + tdd(ic,ih,np)) 
         rd=(taa(ic,ih,np) + tcc(ic,ih,np))
         if (rd.eq.0.0.or.rb.eq.0.0) goto 40
         bis(ic,ih,np)=(taa(ic,ih,np) + tbb(ic,ih,np)) / rd
         ets(ic,ih,np)=(taa(ic,ih,np) - ra) / (taa(ic,ih,np) + tbb(ic,ih,np) + tcc(ic,ih,np) - ra)
         goto 50
 40     bis(ic,ih,np)=bad
        ets(ic,ih,np)=bad
 50     continue
!
!-------------------------------------------------------------------
!--write out scores, each fcst hour in one file                           
      do ih = 1, nfhr
      do id = 1, nday
        write(20+ih)((dbis(ic,ih,id,np),ic=1,ncat),np=1,nexp)
        write(20+ih)((dets(ic,ih,id,np),ic=1,ncat),np=1,nexp)
        write(20+ih)((float(obs(ic,ih,id,np)),ic=1,ncat),np=1,nexp)
      enddo
      close(20+ih)
      enddo

!      do np=1,nexp
!      do ih=1,nfhr
!       do id=1,nday
!        write(6,*)"daily BIAS:  model ",np, " fcst hour ", 12+ih*24, " day", id
!        write(6,'("BIAS: ",9f12.4)') (dbis(ic,ih,id,np),ic=1,ncat) 
!       enddo
!       do id=1,nday
!        write(6,*)"daily ETS: model ",np, " fcst hour ", 12+ih*24, " day", id
!        write(6,'("ETS:  ",9f12.4)') (dets(ic,ih,id,np),ic=1,ncat) 
!       enddo
!      enddo
!      enddo

      do ih = 1, nfhr
       write(30+ih) ((bis(ic,ih,np),ic=1,ncat),np=1,nexp)
       write(30+ih) ((ets(ic,ih,np),ic=1,ncat),np=1,nexp)
       write(30+ih) ((obcount(ic,ih,np),ic=1,ncat),np=1,nexp)
       write(30+ih) ((mbis(ic,ih,np),ic=1,ncat),np=1,nexp)
       write(30+ih) ((mets(ic,ih,np),ic=1,ncat),np=1,nexp)
       write(30+ih) ((vbis(ic,ih,np),ic=1,ncat),np=1,nexp)
       write(30+ih) ((vets(ic,ih,np),ic=1,ncat),np=1,nexp)
       write(30+ih) ((ngood(ic,ih,np),ic=1,ncat),np=1,nexp)
        do np=1,nexp
         write(6,*)"--- all-sampe BIAS, ETS and COUNT:  model ",np, " fcst hour ", 12+ih*24
         write(6,'("BIAS: ",9f12.4)') (bis(ic,ih,np),ic=1,ncat) 
         write(6,'("ETS:  ",9f12.4)') (ets(ic,ih,np),ic=1,ncat) 
         write(6,'("COUNT:",9i10)') (int(obcount(ic,ih,np)),ic=1,ncat) 
        enddo
      close(30+ih)
      enddo
       do ih = 1, nfhr
        write(99,'(9i10)') (int(obcount(ic,ih,1)),ic=1,ncat) 
       enddo


!-------------------------------------------------------------------
! monte carlo significance test
!-------------------------------------------------------------------
      allocate ( std_bis(ncat,nfhr,nexp), std_ets(ncat,nfhr,nexp) )
      std_bis=bad;  std_ets=bad

      if(nexp .ge. 2) then
       write(6,*) 
       write(6,*) "------ Start Monte Carlo Significance Test -----------"
       allocate ( randx(nday) ) 
       allocate ( aa1(ncat,nfhr,nday), bb1(ncat,nfhr,nday), cc1(ncat,nfhr,nday), dd1(ncat,nfhr,nday) )  
       allocate ( aa2(ncat,nfhr,nday), bb2(ncat,nfhr,nday), cc2(ncat,nfhr,nday), dd2(ncat,nfhr,nday) )  
       allocate ( taa1(ncat,nfhr), tbb1(ncat,nfhr), tcc1(ncat,nfhr), tdd1(ncat,nfhr) )  
       allocate ( taa2(ncat,nfhr), tbb2(ncat,nfhr), tcc2(ncat,nfhr), tdd2(ncat,nfhr) )  
       allocate ( bis1(ncat,nfhr,ntest), ets1(ncat,nfhr,ntest) )
       allocate ( bis2(ncat,nfhr,ntest), ets2(ncat,nfhr,ntest) )
       allocate ( bisd(ncat,nfhr,ntest), etsd(ncat,nfhr,ntest) )

       do 100 np=2,nexp
        bis1=0.0; ets1=0.0
        bis2=0.0; ets2=0.0
        bisd=0.0; etsd=0.0

       do 110 nr=1,ntest
       call random_number(randx)
!!     write(6,'(31i3)') int(sign(1.0,randx-0.5))
    
       !construct two times of contigency table by randomly picking samples
       do 120 id = 1, nday
        if((randx(id)-0.5) .ge. 0)  then
         do ih = 1, nfhr
         do ic = 1, ncat
          aa1(ic,ih,id)=aa(ic,ih,id,1)
          bb1(ic,ih,id)=bb(ic,ih,id,1)
          cc1(ic,ih,id)=cc(ic,ih,id,1)
          dd1(ic,ih,id)=dd(ic,ih,id,1)
          aa2(ic,ih,id)=aa(ic,ih,id,np)
          bb2(ic,ih,id)=bb(ic,ih,id,np)
          cc2(ic,ih,id)=cc(ic,ih,id,np)
          dd2(ic,ih,id)=dd(ic,ih,id,np)
         enddo
         enddo
        else
         do ih = 1, nfhr
         do ic = 1, ncat
          aa1(ic,ih,id)=aa(ic,ih,id,np)
          bb1(ic,ih,id)=bb(ic,ih,id,np)
          cc1(ic,ih,id)=cc(ic,ih,id,np)
          dd1(ic,ih,id)=dd(ic,ih,id,np)
          bb2(ic,ih,id)=bb(ic,ih,id,1)
          aa2(ic,ih,id)=aa(ic,ih,id,1)
          cc2(ic,ih,id)=cc(ic,ih,id,1)
          dd2(ic,ih,id)=dd(ic,ih,id,1)
         enddo
         enddo
        endif
 120   continue

        taa1=0; tbb1=0; tcc1=0; tdd1=0
        taa2=0; tbb2=0; tcc2=0; tdd2=0
        do ih = 1, nfhr
        do ic = 1, ncat
        do id = 1, nday
          taa1(ic,ih)=taa1(ic,ih)+aa1(ic,ih,id)
          tbb1(ic,ih)=tbb1(ic,ih)+bb1(ic,ih,id)
          tcc1(ic,ih)=tcc1(ic,ih)+cc1(ic,ih,id)
          tdd1(ic,ih)=tdd1(ic,ih)+dd1(ic,ih,id)
          taa2(ic,ih)=taa2(ic,ih)+aa2(ic,ih,id)
          tbb2(ic,ih)=tbb2(ic,ih)+bb2(ic,ih,id)
          tcc2(ic,ih)=tcc2(ic,ih)+cc2(ic,ih,id)
          tdd2(ic,ih)=tdd2(ic,ih)+dd2(ic,ih,id)
        enddo
        enddo
        enddo

        !compute scores for each randomly-generated group
        do 150 ih = 1, nfhr
        do 150 ic = 1, ncat
         ttot1=taa1(ic,ih)+tbb1(ic,ih)+tcc1(ic,ih)+tdd1(ic,ih)
         if (ttot1.eq.0.0) goto 140
          ra=(taa1(ic,ih) + tbb1(ic,ih)) * (taa1(ic,ih) + tcc1(ic,ih)) / ttot1
          rb=(taa1(ic,ih) + tcc1(ic,ih)) * (tbb1(ic,ih) + tdd1(ic,ih)) 
          rd=(taa1(ic,ih) + tcc1(ic,ih))
          if (rd.eq.0.0.or.rb.eq.0.0) goto 140
          bis1(ic,ih,nr)=(taa1(ic,ih) + tbb1(ic,ih)) / rd
          ets1(ic,ih,nr)=(taa1(ic,ih) - ra) / (taa1(ic,ih) + tbb1(ic,ih) + tcc1(ic,ih) - ra)
 140     continue

         ttot2=taa2(ic,ih)+tbb2(ic,ih)+tcc2(ic,ih)+tdd2(ic,ih)
         if (ttot2.eq.0.0) goto 145
          ra=(taa2(ic,ih) + tbb2(ic,ih)) * (taa2(ic,ih) + tcc2(ic,ih)) / ttot2
          rb=(taa2(ic,ih) + tcc2(ic,ih)) * (tbb2(ic,ih) + tdd2(ic,ih)) 
          rd=(taa2(ic,ih) + tcc2(ic,ih))
          if (rd.eq.0.0.or.rb.eq.0.0) goto 145
          bis2(ic,ih,nr)=(taa2(ic,ih) + tbb2(ic,ih)) / rd
          ets2(ic,ih,nr)=(taa2(ic,ih) - ra) / (taa2(ic,ih) + tbb2(ic,ih) + tcc2(ic,ih) - ra)
 145     continue
         bisd(ic,ih,nr)=bis2(ic,ih,nr)-bis1(ic,ih,nr)
         etsd(ic,ih,nr)=ets2(ic,ih,nr)-ets1(ic,ih,nr)
 150    continue
 110  continue

      !derive standard deviaiton of score differences from monte carlo runs!
        do 160 ih = 1, nfhr
        do 160 ic = 1, ncat
        bisdm=0.0; etsdm=0.0
        bisdv=0.0; etsdv=0.0
         do nr=1,ntest
          bisdm=bisdm+bisd(ic,ih,nr)/ntest             
          etsdm=etsdm+etsd(ic,ih,nr)/ntest             
         enddo
         do nr=1,ntest
          bisdv=bisdv+(bisd(ic,ih,nr)-bisdm)**2         
          etsdv=etsdv+(etsd(ic,ih,nr)-etsdm)**2         
         enddo
         std_bis(ic,ih,np)=sqrt(bisdv/(ntest-1))
         std_ets(ic,ih,np)=sqrt(etsdv/(ntest-1))
 160    continue

        do ih = 1, nfhr
         write(6,*)"--- Monte Carlo differences of BIAS:  model ",np, " fcst hour ", 12+ih*24
         do nr=1,ntest
!         write(6,'(i7, 9e12.4)') nr,(bisd(ic,ih,nr),ic=1,ncat)
          write(50+ih) (bisd(ic,ih,nr),ic=1,ncat)
         enddo
         write(6,*)"--- Monte Carlo differences of ETS:  model ",np, " fcst hour ", 12+ih*24
         do nr=1,ntest
!         write(6,'(i7, 9e12.4)') nr,(etsd(ic,ih,nr),ic=1,ncat)
          write(50+ih) (etsd(ic,ih,nr),ic=1,ncat)
         enddo
         write(6,*)"--- STD,  model ",np, " fcst hour ", 12+ih*24," ntest=",ntest
         write(6,'("BIAS STD: ",9e12.4)') (std_bis(ic,ih,np),ic=1,ncat) 
         write(6,'("ETS STD:  ",9e12.4)') (std_ets(ic,ih,np),ic=1,ncat) 
        enddo

 100   continue

      do ih = 1, nfhr
       write(40+ih) ((std_bis(ic,ih,np),ic=1,ncat),np=1,nexp)
       write(40+ih) ((std_ets(ic,ih,np),ic=1,ncat),np=1,nexp)
      enddo
      
       deallocate ( randx ) 
       deallocate ( aa1, bb1, cc1, dd1 )  
       deallocate ( aa2, bb2, cc2, dd2 )  
       deallocate ( taa1, tbb1, tcc1, tdd1 )  
       deallocate ( taa2, tbb2, tcc2, tdd2 )  
       deallocate ( bis1, ets1 )  
       deallocate ( bis2, ets2 )  
       deallocate ( bisd, etsd )  
!------------------------------------------------------
      endif    !end monte carlo test   
!------------------------------------------------------
!

      deallocate ( hit, obs, fcs, tot )
      deallocate ( aa, bb, cc, dd )
      deallocate ( taa, tbb, tcc, tdd )  
      deallocate ( dbis, dets )
      deallocate ( mbis, mets ) 
      deallocate ( vbis, vets ) 
      deallocate ( bis, ets) 
      deallocate ( ngood, ncount ,obcount )
      deallocate ( std_bis, std_ets )  
      deallocate ( missday )  

      stop
      end 
