!------------------------------------------
      program precip_score_vsdb
!------------------------------------------
! Fanglin Yang, October 2010
! EMC/NCEP, fanglin.yang@noaa.gov
!
! This program reads in precip FHO data saved in VSDB format, 
! computes precip skill scores, and uses resampling (Monte Carlo) 
! method to assess the significance of score differences between two models 
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
      integer, allocatable :: tot(:,:,:)
      real, allocatable :: hit(:,:,:),obs(:,:,:),fcs(:,:,:)
      real, allocatable :: aa(:,:,:),bb(:,:,:),cc(:,:,:),dd(:,:,:)   !contigency table for daily samples
      real, allocatable :: taa(:,:), tbb(:,:), tcc(:,:), tdd(:,:)    !contigency table for total samples
      real, allocatable :: dbis(:,:,:),dets(:,:,:)                   !skill scores for daily samples
      real, allocatable :: mbis(:,:),  mets(:,:)                     !mean of daily skill scores 
      real, allocatable :: vbis(:,:),  vets(:,:)                     !standard deviation of daily skill scores 
      real, allocatable :: bis(:,:),   ets(:,:)                      !skill scores for all samples
      real, allocatable :: ngood(:,:)                                !numbers of days with valid scores  
      real, allocatable :: obcount(:,:)                              !observed counts above threshold for all days    
      integer, allocatable :: missday(:)                             !used to mark days with missing values
      integer, allocatable :: ncount(:,:,:)                          !index of days with valid scores  
      integer, allocatable :: nchar(:,:),nhead(:,:)

      character (1000) :: string
      character(1)     :: substring
      character*200 :: argument    ! space for command-line argument
      data substring/"="/

! arrays used for monte carlo test
      real, allocatable :: randx(:) 
      real, allocatable :: aa1(:,:), bb1(:,:), cc1(:,:), dd1(:,:)  !model 1 from random selection
      real, allocatable :: aa2(:,:), bb2(:,:), cc2(:,:), dd2(:,:)  !model 2 from random selection
      real, allocatable :: taa1(:), tbb1(:), tcc1(:), tdd1(:)      !contigency table for all samples
      real, allocatable :: taa2(:), tbb2(:), tcc2(:), tdd2(:)      !contigency table for all samples
      real, allocatable :: bis1(:,:),   ets1(:,:)                   
      real, allocatable :: bis2(:,:),   ets2(:,:)                   
      real, allocatable :: bisd(:,:),   etsd(:,:)                  !diference of scores
      real, allocatable :: std_bis(:,:), std_ets(:,:)              !standard deviation of scores

      data bad/-9999.0/
      data badi/-9999/

!--------------------------------------------------------
!--------------------------------------------------------
      nargs = iargc()   ! iargc() - number of arguments 
      call getarg(1,argument)      !number of ending fcst hours 
      read(argument,*) nfhr       
      call getarg(2,argument)      !number of intensity category
      read(argument,*) ncat       
      call getarg(3,argument)      !sample days
      read(argument,*) nday       
      call getarg(4,argument)      !number of models
      read(argument,*) nexp       
      call getarg(5,argument)      !number of monte carlo tests
      read(argument,*) ntest       
      write(6,*) "nfhr ncat ndays nexp ntest :"
      write(6,*)  nfhr,ncat,ndays,nexp,ntest

      allocate ( hit(ncat,nday,nexp), obs(ncat,nday,nexp), fcs(ncat,nday,nexp), tot(ncat,nday,nexp) )
      allocate ( aa(ncat,nday,nexp),  bb(ncat,nday,nexp),  cc(ncat,nday,nexp),  dd(ncat,nday,nexp) )
      allocate ( taa(ncat,nexp),      tbb(ncat,nexp),      tcc(ncat,nexp),      tdd(ncat,nexp) )  
      allocate ( dbis(ncat,nday,nexp), dets(ncat,nday,nexp) )
      allocate ( bis(ncat,nexp),       ets(ncat,nexp) )
      allocate ( mbis(ncat,nexp),      mets(ncat,nexp) )
      allocate ( vbis(ncat,nexp),      vets(ncat,nexp) )
      allocate ( ngood(ncat,nexp), ncount(ncat,nday,nexp), obcount(ncat,nexp) )
      allocate ( nchar(ncat,nday),nhead(ncat,nday) )
      allocate ( missday(nday) )


      hit=0.0; obs=0.0; fcs=0.0; tot=0           
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
       open(2,file="vsdbdata"//trim(string),form="formatted",status="unknown")

      do 11 nh=1,nfhr

! find length of character header
       do 15 k=1,nday
       do 15 i=1,ncat
         read(2,'(1A)') string
         nchar(i,k)=len_trim(string)
         nhead(i,k)=index(string,substring)  !find character header length before "="
         write(3,*) string(nhead(i,k)+1:nchar(i,k))
!        write(4,*) trim(string), nchar(i,k), nhead(i,k)
 15    continue 

! read data
       rewind (3)
       do 16 k=1,nday
       do 16 i=1,ncat
       if(nhead(i,k).eq.0) then
         read(3,'(1A)') string(1:nchar(i,k))    !data missing
         tot(i,k,np)=badi
         fcs(i,k,np)=bad
         obs(i,k,np)=bad
         hit(i,k,np)=bad
         missday(k)=1
       else
         read(3,*) ntottmp, fcstmp, hittmp, obstmp     
          tot(i,k,np) = tot(i,k,np) + ntottmp
          fcs(i,k,np) = fcs(i,k,np) + fcstmp*ntottmp
          hit(i,k,np) = hit(i,k,np) + hittmp*ntottmp
          obs(i,k,np) = obs(i,k,np) + obstmp*ntottmp
       endif
 16    continue 
       close(3)
 11   continue
       close(2)
 10   continue


       do 17 np=1,nexp
       do 17 ic = 1, ncat
       do 17 id = 1, nday
        if(missday(id).ne.1) then
         aa(ic,id,np)=hit(ic,id,np)
         bb(ic,id,np)=fcs(ic,id,np)-hit(ic,id,np)
         cc(ic,id,np)=obs(ic,id,np)-hit(ic,id,np)
         dd(ic,id,np)=tot(ic,id,np)-fcs(ic,id,np)-obs(ic,id,np)+hit(ic,id,np)
         taa(ic,np)=taa(ic,np)+aa(ic,id,np)
         tbb(ic,np)=tbb(ic,np)+bb(ic,id,np)
         tcc(ic,np)=tcc(ic,np)+cc(ic,id,np)
         tdd(ic,np)=tdd(ic,np)+dd(ic,id,np)
        endif
 17    continue  


!-------------------------------------------------------------------
!--compute skill scores
      do 50 np = 1, nexp
      do 50 ic = 1, ncat
         do 60 id = 1, nday
          if (tot(ic,id,np).eq.badi .or. missday(id).eq.1 ) then
           dbis(ic,id,np)=bad                        
           dets(ic,id,np)=bad                                 
          else
           if (tot(ic,id,np).eq.0) goto 60  
           ra=(aa(ic,id,np) + bb(ic,id,np)) * (aa(ic,id,np) + cc(ic,id,np)) / tot(ic,id,np)
           rb=(aa(ic,id,np) + cc(ic,id,np)) * (bb(ic,id,np) + dd(ic,id,np))
           rd=(aa(ic,id,np) + cc(ic,id,np))
!!           print *, "model=",np, " icat=",ic, " day=",id, " R(a)=",ra      
           if (rd.eq.0.0.or.rb.eq.0.0) goto 60  
           ncount(ic,id,np)=1                                            
           ngood(ic,np)=ngood(ic,np) + 1                          
           dbis(ic,id,np)=(aa(ic,id,np) + bb(ic,id,np)) / rd
           dets(ic,id,np)=(aa(ic,id,np) - ra) / (aa(ic,id,np) + bb(ic,id,np) + cc(ic,id,np) - ra)
           mbis(ic,np)=mbis(ic,np) + dbis(ic,id,np)                          
           mets(ic,np)=mets(ic,np) + dets(ic,id,np)                          
          endif
 60      continue
         
       !derive standard deviations of daily skill scores
         if(ngood(ic,np) .gt. 1) then    
           mbis(ic,np)=mbis(ic,np) / ngood(ic,np)                           
           mets(ic,np)=mets(ic,np) / ngood(ic,np)                           
           do id = 1, nday
           if (ncount(ic,id,np) .eq. 1) then
            vbis(ic,np)=vbis(ic,np)+(dbis(ic,id,np)-mbis(ic,np))**2                                          
            vets(ic,np)=vets(ic,np)+(dets(ic,id,np)-mets(ic,np))**2                                          
           endif
           enddo
           vbis(ic,np)=sqrt( vbis(ic,np)/(ngood(ic,np)-1) )
           vets(ic,np)=sqrt( vets(ic,np)/(ngood(ic,np)-1) )
         else
           mbis(ic,np)=bad
           mets(ic,np)=bad 
           vbis(ic,np)=bad
           vets(ic,np)=bad 
         endif


       !compute skill scores with all samples together
         obcount(ic,np) = taa(ic,np)+tcc(ic,np)
         ttot=taa(ic,np)+tbb(ic,np)+ tcc(ic,np)+tdd(ic,np)
         if (ttot.eq.0.0) goto 40
         ra=(taa(ic,np) + tbb(ic,np)) * (taa(ic,np) + tcc(ic,np)) / ttot
         rb=(taa(ic,np) + tcc(ic,np)) * (tbb(ic,np) + tdd(ic,np)) 
         rd=(taa(ic,np) + tcc(ic,np))
         if (rd.eq.0.0.or.rb.eq.0.0) goto 40
         bis(ic,np)=(taa(ic,np) + tbb(ic,np)) / rd
         ets(ic,np)=(taa(ic,np) - ra) / (taa(ic,np) + tbb(ic,np) + tcc(ic,np) - ra)
         goto 50
 40     bis(ic,np)=bad
        ets(ic,np)=bad
 50     continue
!
!-------------------------------------------------------------------
!--write out scores, each fcst hour in one file                           
      do id = 1, nday
        write(20)((dbis(ic,id,np),ic=1,ncat),np=1,nexp)
        write(20)((dets(ic,id,np),ic=1,ncat),np=1,nexp)
        write(20)((obs(ic,id,np),ic=1,ncat),np=1,nexp)
      enddo
      close(20)

!      do np=1,nexp
!       do id=1,nday
!        write(6,*)"daily BIAS:  model ",np, " day", id
!        write(6,'("BIAS: ",9f12.4)') (dbis(ic,id,np),ic=1,ncat) 
!       enddo
!       do id=1,nday
!        write(6,*)"daily ETS: model ",np, " day", id
!        write(6,'("ETS:  ",9f12.4)') (dets(ic,id,np),ic=1,ncat) 
!       enddo
!      enddo

       write(30) ((bis(ic,np),ic=1,ncat),np=1,nexp)
       write(30) ((ets(ic,np),ic=1,ncat),np=1,nexp)
       write(30) ((obcount(ic,np),ic=1,ncat),np=1,nexp)
       write(30) ((mbis(ic,np),ic=1,ncat),np=1,nexp)
       write(30) ((mets(ic,np),ic=1,ncat),np=1,nexp)
       write(30) ((vbis(ic,np),ic=1,ncat),np=1,nexp)
       write(30) ((vets(ic,np),ic=1,ncat),np=1,nexp)
       write(30) ((ngood(ic,np),ic=1,ncat),np=1,nexp)
        do np=1,nexp
         write(6,*)"--- all-sampe BIAS, ETS and COUNT:  model ",np
         write(6,'("BIAS: ",9f12.4)') (bis(ic,np),ic=1,ncat) 
         write(6,'("ETS:  ",9f12.4)') (ets(ic,np),ic=1,ncat) 
         write(6,'("COUNT:",9i10)') (int(obcount(ic,np)),ic=1,ncat) 
        enddo
      close(30)
        write(99,'(9i10)') (int(obcount(ic,1)),ic=1,ncat) 


!-------------------------------------------------------------------
! monte carlo significance test
!-------------------------------------------------------------------
      allocate ( std_bis(ncat,nexp), std_ets(ncat,nexp) )
      std_bis=bad;  std_ets=bad

      if(nexp .ge. 2) then
       write(6,*) 
       write(6,*) "------ Start Monte Carlo Significance Test -----------"
       allocate ( randx(nday) ) 
       allocate ( aa1(ncat,nday), bb1(ncat,nday), cc1(ncat,nday), dd1(ncat,nday) )  
       allocate ( aa2(ncat,nday), bb2(ncat,nday), cc2(ncat,nday), dd2(ncat,nday) )  
       allocate ( taa1(ncat), tbb1(ncat), tcc1(ncat), tdd1(ncat) )  
       allocate ( taa2(ncat), tbb2(ncat), tcc2(ncat), tdd2(ncat) )  
       allocate ( bis1(ncat,ntest), ets1(ncat,ntest) )
       allocate ( bis2(ncat,ntest), ets2(ncat,ntest) )
       allocate ( bisd(ncat,ntest), etsd(ncat,ntest) )

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
         do ic = 1, ncat
          aa1(ic,id)=aa(ic,id,1)
          bb1(ic,id)=bb(ic,id,1)
          cc1(ic,id)=cc(ic,id,1)
          dd1(ic,id)=dd(ic,id,1)
          aa2(ic,id)=aa(ic,id,np)
          bb2(ic,id)=bb(ic,id,np)
          cc2(ic,id)=cc(ic,id,np)
          dd2(ic,id)=dd(ic,id,np)
         enddo
        else
         do ic = 1, ncat
          aa1(ic,id)=aa(ic,id,np)
          bb1(ic,id)=bb(ic,id,np)
          cc1(ic,id)=cc(ic,id,np)
          dd1(ic,id)=dd(ic,id,np)
          bb2(ic,id)=bb(ic,id,1)
          aa2(ic,id)=aa(ic,id,1)
          cc2(ic,id)=cc(ic,id,1)
          dd2(ic,id)=dd(ic,id,1)
         enddo
        endif
 120   continue

        taa1=0; tbb1=0; tcc1=0; tdd1=0
        taa2=0; tbb2=0; tcc2=0; tdd2=0
        do ic = 1, ncat
        do id = 1, nday
          taa1(ic)=taa1(ic)+aa1(ic,id)
          tbb1(ic)=tbb1(ic)+bb1(ic,id)
          tcc1(ic)=tcc1(ic)+cc1(ic,id)
          tdd1(ic)=tdd1(ic)+dd1(ic,id)
          taa2(ic)=taa2(ic)+aa2(ic,id)
          tbb2(ic)=tbb2(ic)+bb2(ic,id)
          tcc2(ic)=tcc2(ic)+cc2(ic,id)
          tdd2(ic)=tdd2(ic)+dd2(ic,id)
        enddo
        enddo

        !compute scores for each randomly-generated group
        do 150 ic = 1, ncat
         ttot1=taa1(ic)+tbb1(ic)+tcc1(ic)+tdd1(ic)
         if (ttot1.eq.0.0) goto 140
          ra=(taa1(ic) + tbb1(ic)) * (taa1(ic) + tcc1(ic)) / ttot1
          rb=(taa1(ic) + tcc1(ic)) * (tbb1(ic) + tdd1(ic)) 
          rd=(taa1(ic) + tcc1(ic))
          if (rd.eq.0.0.or.rb.eq.0.0) goto 140
          bis1(ic,nr)=(taa1(ic) + tbb1(ic)) / rd
          ets1(ic,nr)=(taa1(ic) - ra) / (taa1(ic) + tbb1(ic) + tcc1(ic) - ra)
 140     continue

         ttot2=taa2(ic)+tbb2(ic)+tcc2(ic)+tdd2(ic)
         if (ttot2.eq.0.0) goto 145
          ra=(taa2(ic) + tbb2(ic)) * (taa2(ic) + tcc2(ic)) / ttot2
          rb=(taa2(ic) + tcc2(ic)) * (tbb2(ic) + tdd2(ic)) 
          rd=(taa2(ic) + tcc2(ic))
          if (rd.eq.0.0.or.rb.eq.0.0) goto 145
          bis2(ic,nr)=(taa2(ic) + tbb2(ic)) / rd
          ets2(ic,nr)=(taa2(ic) - ra) / (taa2(ic) + tbb2(ic) + tcc2(ic) - ra)
 145     continue
         bisd(ic,nr)=bis2(ic,nr)-bis1(ic,nr)
         etsd(ic,nr)=ets2(ic,nr)-ets1(ic,nr)
 150    continue
 110  continue

      !derive standard deviaiton of score differences from monte carlo runs!
        do 160 ic = 1, ncat
        bisdm=0.0; etsdm=0.0
        bisdv=0.0; etsdv=0.0
         do nr=1,ntest
          bisdm=bisdm+bisd(ic,nr)/ntest             
          etsdm=etsdm+etsd(ic,nr)/ntest             
         enddo
         do nr=1,ntest
          bisdv=bisdv+(bisd(ic,nr)-bisdm)**2         
          etsdv=etsdv+(etsd(ic,nr)-etsdm)**2         
         enddo
         std_bis(ic,np)=sqrt(bisdv/(ntest-1))
         std_ets(ic,np)=sqrt(etsdv/(ntest-1))
 160    continue

         write(6,*)"--- Monte Carlo differences of BIAS:  model ",np
         do nr=1,ntest
!         write(6,'(i7, 9e12.4)') nr,(bisd(ic,nr),ic=1,ncat)
          write(50) (bisd(ic,nr),ic=1,ncat)
         enddo
         write(6,*)"--- Monte Carlo differences of ETS:  model ",np
         do nr=1,ntest
!         write(6,'(i7, 9e12.4)') nr,(etsd(ic,nr),ic=1,ncat)
          write(50) (etsd(ic,nr),ic=1,ncat)
         enddo
         write(6,*)"--- STD,  model ",np, " ntest=",ntest
         write(6,'("BIAS STD: ",9e12.4)') (std_bis(ic,np),ic=1,ncat) 
         write(6,'("ETS STD:  ",9e12.4)') (std_ets(ic,np),ic=1,ncat) 

 100   continue

       write(40) ((std_bis(ic,np),ic=1,ncat),np=1,nexp)
       write(40) ((std_ets(ic,np),ic=1,ncat),np=1,nexp)
      
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
      deallocate ( nchar, nhead ) 
      deallocate ( missday ) 

      stop
      end 
