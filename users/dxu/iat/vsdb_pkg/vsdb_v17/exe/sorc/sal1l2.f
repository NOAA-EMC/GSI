      subroutine SAL1L2(nvsdb,imodel,ist,numfcst,numvfyobs,numarea,
     +  numvarbl,numlevel,ngrid,levels,hasdata,fdata,odata,
     +  climdata,tendencymrk)

c    This program is to generate SL1L2 vsdb records for all fcst times,
c    all variables, all levels, all requested sub-regions, then write them
c    into the vsdb file
c    Author: Binbin Zhou
c            March, 2005
                                                                                                                 


      INCLUDE 'parm.inc'
      
      INTEGER, intent(IN) :: imodel, ist, numvfyobs,numarea,
     +         numfcst,numvarbl,numlevel,ngrid, levels(mxvrbl),
     +         hasdata(mxfcst,mxvrbl,maxlvl),tendencymrk(mxvrbl)

      real, dimension(numfcst,numvarbl,numlevel,ngrid),intent(IN) ::
     +          fdata, odata,  climdata
      
      real*8, allocatable, dimension(:,:,:,:,:) :: sumdata,
     +          sumgrid,sumprod,ssqdata,ssqgrid,count,weigh

      real, allocatable, dimension(:,:,:,:) :: fcstdata,obsvdata
                                                                                                                                                   
      DIMENSION nchrmodel(maxmod), nchrfcst(mxfcst), nchrvfdate(mxdate),
     +            nchrvfyobs(maxobs), nchrarea(mxarea),
     +            nchrstat(mxstat), nchrvarbl(mxvrbl),
     +            nchrlevel(maxlvl)
      CHARACTER*24 namodel(maxmod), namfcst(mxfcst), namvfdate(mxdate),
     +            namvfyobs(maxobs), namarea(mxarea), namstat(mxstat),
     +            namvarbl(mxvrbl), namlevel(maxlvl)
                                                                                                                                                         
      COMMON /names/ namodel, namfcst, namvfdate, namvfyobs, namarea,
     +            namstat, namvarbl, namlevel
      COMMON /nchrs/ nchrmodel, nchrfcst, nchrvfdate, nchrvfyobs,
     +            nchrarea, nchrstat, nchrvarbl, nchrlevel
      CHARACTER*3 regions (100)
      COMMON /grdef/mode(mxarea), imax(mxarea), imin(mxarea),
     +            jmax(mxarea), jmin(mxarea), alat1(mxarea),
     +            elon1(mxarea), dxx(mxarea), dyy(mxarea),
     +            elonv(mxarea), alatan(mxarea), latlong(mxarea),
     +            lambert(mxarea), polarstereo(mxarea)
      COMMON /grdef1/numreg(mxarea), ig104(147,110),regions


      CHARACTER*3 namversion
      CHARACTER*132 vdbhdr132, input, substr (3)
                                                                                                                                                         
      CHARACTER*1 blank, equal
C

      LOGICAL*1 latlong, lambert, polarstereo    !Add  by Binbin, otherwise, they are can not be retrieved
      CHARACTER*24 fcst_ymdhf(mxfcst), obsv_ymdhf(maxobs)      !store YYYYMMDDHHFF string for fcst and obsv read from control
      CHARACTER*4 cyyyyfcst(mxfcst),cyyyyobsv(maxobs)
      CHARACTER*2 cmmfcst(mxfcst),cmmobsv(maxobs)
      CHARACTER*2 cddfcst(mxfcst),cddobsv(maxobs)
      CHARACTER*2 chhfcst(mxfcst),chhobsv(maxobs)
      CHARACTER*3 cfffcst(mxfcst),cffobsv(maxobs)
      CHARACTER*6 ck7(mxvrbl)

      CHARACTER*24 namlvl(mxvrbl,maxlvl) 
      integer nchrlvl(mxvrbl,maxlvl) 

      integer yyyyfcst(mxfcst), mmfcst(mxfcst),
     +        ddfcst(mxfcst), hhfcst(mxfcst), fffcst(mxfcst),
     +        yyyyobsv(maxobs), mmobsv(maxobs),
     +        ddobsv(maxobs), hhobsv(maxobs), ffobsv(maxobs)
      integer k5(mxvrbl),k6(mxvrbl),k7(mxvrbl),vectormrk(mxvrbl)
 
      CHARACTER*24 fho(mxvrbl),    fhothr(mxvrbl,20)
      CHARACTER*24 afho(mxvrbl),  afhothr(mxvrbl,20)
      integer  nchrfho(mxvrbl),nchrfhothr(mxvrbl,20),fhomrk(mxvrbl)
      integer  nchrafho(mxvrbl),nchrafhothr(mxvrbl,20),afhomrk(mxvrbl)
      real rfhothr(mxvrbl,20), rafhothr(mxvrbl,20)
      integer continue_mrk(mxvrbl)
      integer anomlylev(mxvrbl,maxlvl),anomly_mrk(mxvrbl)


      COMMON /g2g/cyyyyfcst,cmmfcst,cddfcst,chhfcst,cfffcst,
     +            cyyyyobsv,cmmobsv,cddobsv,chhobsv,cffobsv,
     +             yyyyfcst, mmfcst, ddfcst, hhfcst, fffcst,
     +             yyyyobsv, mmobsv, ddobsv, hhobsv, ffobsv,
     +             k5,k6,k7,ck7,vectormrk,namlvl,nchrlvl,
     +       fhomrk,fho,nchrfho,fhothr,nchrfhothr,rfhothr,
     +             continue_mrk, anomly_mrk,anomlylev,
     +     afhomrk,afho,nchrafho,afhothr,nchrafhothr,rafhothr


      integer plevel(maxlvl)
      integer region_id(maxpts)
      real region_latlon(2,maxpts),ptr1(2,mxarea), ptr2(2,mxarea)
 
      COMMON /reg/region_id, region_latlon,ptr1,ptr2
      COMMON /layer/ modelvl(maxlvl), iplevel(maxlvl,2)


      integer nodata(mxfcst,mxvrbl,maxlvl)
      COMMON /nofile/nodata

      real area_factor(maxpts)
      COMMON /weight/area_factor

      DATA blank /' '/
      DATA equal /'='/
      DATA namversion /'V01'/
      DATA bmiss /10E10/
      DATA rmiss /99999./

      write(*,*) 'In SAL1L2:',imodel,ist,numfcst,numvfyobs,numarea,
     +  numvarbl,numlevel,ngrid

      write(*,*) 'fhomrk', (fhomrk(ivr),ivr=1,numvarbl)
      write(*,*) 'afhomrk', (afhomrk(ivr),ivr=1,numvarbl)

      write(*,*) 'anomly_mrk=',(anomly_mrk(i),i=1,numvarbl)
      do i=1,numvarbl
       write(*,*) 'levels=', levels(i)
       write(*,*)'anomlylev=',(anomlylev(i,j),j=1,numlevel)
      end do

        allocate (sumdata(numfcst,numvarbl,numlevel,numarea,numvfyobs))
        allocate (sumgrid(numfcst,numvarbl,numlevel,numarea,numvfyobs))
        allocate (sumprod(numfcst,numvarbl,numlevel,numarea,numvfyobs))
        allocate (ssqdata(numfcst,numvarbl,numlevel,numarea,numvfyobs))
        allocate (ssqgrid(numfcst,numvarbl,numlevel,numarea,numvfyobs))
        allocate (count(numfcst,numvarbl,numlevel,numarea,numvfyobs))
        allocate (weigh(numfcst,numvarbl,numlevel,numarea,numvfyobs))
        allocate (fcstdata(numfcst,numvarbl,numlevel,ngrid))
        allocate (obsvdata(numfcst,numvarbl,numlevel,ngrid))



      do nfcst=1,numfcst
       do ivr = 1, numvarbl
         if(anomly_mrk(ivr).eq.1.and.tendencymrk(ivr).eq.0) then
          do ilv=1,numlevel
           if(anomlylev(ivr,ilv).eq.1) then
             do i=1,ngrid 
               fcstdata(nfcst,ivr,ilv,i)=fdata(nfcst,ivr,ilv,i)
     +                         -climdata(nfcst,ivr,ilv,i)
               obsvdata(nfcst,ivr,ilv,i)=odata(nfcst,ivr,ilv,i)
     +                         -climdata(nfcst,ivr,ilv,i)
             end do
            end if
          end do
         end if
       end do
      end do


c       do nfcst=1,numfcst
c        do ivr = 1, numvarbl
c         write(*,*) 'nfcst=',nfcst,' ivr=',ivr
c         write(*,*)'anomly_mrk=',anomly_mrk(ivr)
c         do np = 1, numlevel
c          write(*,*)'anomlylev=',anomlylev(ivr,np)
c          do i=1,10   
c           write(*,*) i, 
c     +     fdata(nfcst,ivr,np,i),
c     +     odata(nfcst,ivr,np,i),
c     +     climdata(nfcst,ivr,np,i),
c     +     fcstdata(nfcst,ivr,np,i),
c     +     obsvdata(nfcst,ivr,np,i)
c          end do
c         end do
c        end do
c       end do

          count = 0.0
          weigh = 0.0
          sumdata = 0.0
          ssqdata = 0.0
          sumgrid = 0.0
          ssqgrid = 0.0
          sumprod = 0.0

      do 90 ifh = 1, numfcst
        do 80 iob = 1, numvfyobs
          do 70 iar = 1, numarea
           do 60 ivr = 1, numvarbl
              if(anomly_mrk(ivr).eq.0) goto 60
              if(vectormrk(ivr).ne.0) goto 60     
              if(fhomrk(ivr).ne.0)    goto 60
              if(afhomrk(ivr).ne.0)    goto 60
              if(tendencymrk(ivr).ne.0)    goto 60     !tendency not work on anomly
             do 50 ilv = 1, levels(ivr)
               if(nodata(ifh,ivr,ilv).eq.1) goto 50
               if(anomlylev(ivr,ilv).eq.0) goto 50
                if (mode(iar).eq.1) then                          !all GRID domain (mode 1)
                  do 5001 i = 1,ngrid
                    if(continue_mrk(ivr).eq.1 ) goto 5001
                    
                       sumdata(ifh,ivr,ilv,iar,iob) = 
     +                        sumdata(ifh,ivr,ilv,iar,iob)
     +                        + obsvdata(ifh,ivr,ilv,i)
     +                        *area_factor(i)

                       ssqdata(ifh,ivr,ilv,iar,iob) = 
     +                        ssqdata(ifh,ivr,ilv,iar,iob) 
     +                        + obsvdata(ifh,ivr,ilv,i) *
     +                          obsvdata(ifh,ivr,ilv,i)
     +                        *area_factor(i)

                       sumgrid(ifh,ivr,ilv,iar,iob) = 
     +                        sumgrid(ifh,ivr,ilv,iar,iob) 
     +                        + fcstdata(ifh,ivr,ilv,i)
     +                        *area_factor(i)

                       ssqgrid(ifh,ivr,ilv,iar,iob) = 
     +                        ssqgrid(ifh,ivr,ilv,iar,iob) 
     +                        + fcstdata(ifh,ivr,ilv,i) *
     +                          fcstdata(ifh,ivr,ilv,i)
     +                        *area_factor(i)

                       sumprod(ifh,ivr,ilv,iar,iob) = 
     +                        sumprod(ifh,ivr,ilv,iar,iob) 
     +                        + fcstdata(ifh,ivr,ilv,i) *
     +                          obsvdata(ifh,ivr,ilv,i)
     +                        *area_factor(i)

                       count(ifh,ivr,ilv,iar,iob) =
     +                        count(ifh,ivr,ilv,iar,iob)
     +                        + 1.0
                       weigh(ifh,ivr,ilv,iar,iob) =
     +                 weigh(ifh,ivr,ilv,iar,iob) + area_factor(i)

c                       if(i.ge.66395.and.i.le.66399) then
c                      write(*,*)i,sumdata(ifh,ivr,ilv,iar,iob),
c     +                 sumgrid(ifh,ivr,ilv,iar,iob),
c     +                 count(ifh,ivr,ilv,iar,iob)
c                       end if

5001                continue
                else if (mode(iar).eq.2) then                    ! GRID#104  (mode 2)
                  if (numreg(iar).le.30) then                    !         sub-region
                    do 5002 i = 1,ngrid
                    if(continue_mrk(ivr).eq.1 ) goto 5002

                      if(region_id(i).eq.numreg(iar)) then
                       sumdata(ifh,ivr,ilv,iar,iob) =
     +                        sumdata(ifh,ivr,ilv,iar,iob)
     +                        + obsvdata(ifh,ivr,ilv,i)
     +                        *area_factor(i)                                                                                                                                                           
                       ssqdata(ifh,ivr,ilv,iar,iob) =
     +                        ssqdata(ifh,ivr,ilv,iar,iob)
     +                        + obsvdata(ifh,ivr,ilv,i) *
     +                          obsvdata(ifh,ivr,ilv,i)
     +                        *area_factor(i)
                                                                                                                                                           
                       sumgrid(ifh,ivr,ilv,iar,iob) =
     +                        sumgrid(ifh,ivr,ilv,iar,iob)
     +                        + fcstdata(ifh,ivr,ilv,i)
     +                        *area_factor(i)
                                                                                                                                                           
                       ssqgrid(ifh,ivr,ilv,iar,iob) =
     +                        ssqgrid(ifh,ivr,ilv,iar,iob)
     +                        + fcstdata(ifh,ivr,ilv,i) *
     +                          fcstdata(ifh,ivr,ilv,i)
     +                        *area_factor(i)
                                                                                                                                                           
                       sumprod(ifh,ivr,ilv,iar,iob) =
     +                        sumprod(ifh,ivr,ilv,iar,iob)
     +                        + fcstdata(ifh,ivr,ilv,i) *
     +                          obsvdata(ifh,ivr,ilv,i)
     +                        *area_factor(i)

                       count(ifh,ivr,ilv,iar,iob) =
     +                        count(ifh,ivr,ilv,iar,iob)
     +                        + 1.0
                       weigh(ifh,ivr,ilv,iar,iob) =
     +                 weigh(ifh,ivr,ilv,iar,iob) + area_factor(i)
 
                      end if
5002                continue  
                   else if(numreg(iar).eq.31 ) then             !        N. hemisphere
 
                     do 5003 i = 1,ngrid
                    if(continue_mrk(ivr).eq.1 ) goto 5003

                      if(region_latlon(1,i).gt.0.0) then

                       sumdata(ifh,ivr,ilv,iar,iob) =
     +                        sumdata(ifh,ivr,ilv,iar,iob)
     +                        + obsvdata(ifh,ivr,ilv,i)
     +                        *area_factor(i)                                                                                                                                                           
                       ssqdata(ifh,ivr,ilv,iar,iob) =
     +                        ssqdata(ifh,ivr,ilv,iar,iob)
     +                        + obsvdata(ifh,ivr,ilv,i) *
     +                          obsvdata(ifh,ivr,ilv,i)
     +                        *area_factor(i)
                                                                                                                                                           
                       sumgrid(ifh,ivr,ilv,iar,iob) =
     +                        sumgrid(ifh,ivr,ilv,iar,iob)
     +                        + fcstdata(ifh,ivr,ilv,i)
     +                        *area_factor(i)
                                                                                                                                                           
                       ssqgrid(ifh,ivr,ilv,iar,iob) =
     +                        ssqgrid(ifh,ivr,ilv,iar,iob)
     +                        + fcstdata(ifh,ivr,ilv,i) *
     +                          fcstdata(ifh,ivr,ilv,i)
     +                        *area_factor(i)
                                                                                                                                                           
                       sumprod(ifh,ivr,ilv,iar,iob) =
     +                        sumprod(ifh,ivr,ilv,iar,iob)
     +                        + fcstdata(ifh,ivr,ilv,i) *
     +                          obsvdata(ifh,ivr,ilv,i)
     +                        *area_factor(i)

                       count(ifh,ivr,ilv,iar,iob) =
     +                        count(ifh,ivr,ilv,iar,iob)
     +                        + 1.0
                       weigh(ifh,ivr,ilv,iar,iob) =
     +                 weigh(ifh,ivr,ilv,iar,iob) + area_factor(i)

                      end if
5003                continue  
                   else if(numreg(iar).eq.32 ) then             !        S. hemisphere
 
                     do 5004 i = 1,ngrid
                    if(continue_mrk(ivr).eq.1 ) goto 5004

                      if(region_latlon(1,i).lt.0.0) then
                       sumdata(ifh,ivr,ilv,iar,iob) =
     +                        sumdata(ifh,ivr,ilv,iar,iob)
     +                        + obsvdata(ifh,ivr,ilv,i)
     +                        *area_factor(i)                                                                                                                                                           
                       ssqdata(ifh,ivr,ilv,iar,iob) =
     +                        ssqdata(ifh,ivr,ilv,iar,iob)
     +                        + obsvdata(ifh,ivr,ilv,i) *
     +                          obsvdata(ifh,ivr,ilv,i)
     +                        *area_factor(i)
                                                                                                                                                           
                       sumgrid(ifh,ivr,ilv,iar,iob) =
     +                        sumgrid(ifh,ivr,ilv,iar,iob)
     +                        + fcstdata(ifh,ivr,ilv,i)
     +                        *area_factor(i)
                                                                                                                                                           
                       ssqgrid(ifh,ivr,ilv,iar,iob) =
     +                        ssqgrid(ifh,ivr,ilv,iar,iob)
     +                        + fcstdata(ifh,ivr,ilv,i) *
     +                          fcstdata(ifh,ivr,ilv,i)
     +                        *area_factor(i)
                                                                                                                                                           
                       sumprod(ifh,ivr,ilv,iar,iob) =
     +                        sumprod(ifh,ivr,ilv,iar,iob)
     +                        + fcstdata(ifh,ivr,ilv,i) *
     +                          obsvdata(ifh,ivr,ilv,i)
     +                        *area_factor(i)

                       count(ifh,ivr,ilv,iar,iob) =
     +                        count(ifh,ivr,ilv,iar,iob)
     +                        + 1.0
                       weigh(ifh,ivr,ilv,iar,iob) =
     +                 weigh(ifh,ivr,ilv,iar,iob) + area_factor(i)

                      end if
5004                continue  

                   else if(numreg(iar).eq.33) then             ! User defined region
                     if (ptr1(1,iar).eq.ptr2(1,iar).and.         !user defined case 1, all points along latitude
     +                   ptr1(2,iar).ne.ptr2(2,iar)) then                !
                       do 5005 i = 1,ngrid                               !  x(ptr2(1), ptr2(2))
                    if(continue_mrk(ivr).eq.1) goto 5005        
                        if(region_latlon(1,i).ge.ptr1(2,iar).and.        !  |
     +                     region_latlon(1,i).le.ptr2(2,iar)) then       !  |
                          sumdata(ifh,ivr,ilv,iar,iob) =                 !  x(ptr1(1), ptr1(2))  
     +                        sumdata(ifh,ivr,ilv,iar,iob)               !  where ptr1(1) = ptr2(1) 
     +                        + obsvdata(ifh,ivr,ilv,i)
     +                        *area_factor(i)
                          ssqdata(ifh,ivr,ilv,iar,iob) =
     +                        ssqdata(ifh,ivr,ilv,iar,iob)
     +                        + obsvdata(ifh,ivr,ilv,i) *
     +                          obsvdata(ifh,ivr,ilv,i)
     +                        *area_factor(i)
                          sumgrid(ifh,ivr,ilv,iar,iob) =
     +                        sumgrid(ifh,ivr,ilv,iar,iob)
     +                        + fcstdata(ifh,ivr,ilv,i)
     +                        *area_factor(i)
                          ssqgrid(ifh,ivr,ilv,iar,iob) =
     +                        ssqgrid(ifh,ivr,ilv,iar,iob)
     +                        + fcstdata(ifh,ivr,ilv,i) *
     +                          fcstdata(ifh,ivr,ilv,i)
     +                        *area_factor(i)
                          sumprod(ifh,ivr,ilv,iar,iob) =
     +                        sumprod(ifh,ivr,ilv,iar,iob)
     +                        + fcstdata(ifh,ivr,ilv,i) *
     +                          obsvdata(ifh,ivr,ilv,i)
     +                        *area_factor(i)
                          count(ifh,ivr,ilv,iar,iob) =
     +                        count(ifh,ivr,ilv,iar,iob)
     +                        + 1.0
                       weigh(ifh,ivr,ilv,iar,iob) =
     +                 weigh(ifh,ivr,ilv,iar,iob) + area_factor(i)

                                   
c                         write(*,*) i,fcstdata(ifh,ivr,ilv,i),
c     +                                obsvdata(ifh,ivr,ilv,i)
c                         write(*,*) sumdata(ifh,ivr,ilv,iar,iob),
c     +                              sumgrid(ifh,ivr,ilv,iar,iob),
c     +                     count(ifh,ivr,ilv,iar,iob)


                         end if
5005                  continue 
                     else if (ptr1(1,iar).ne.ptr2(1,iar).and.    !user defined case 2, all points along longitude 
     +                        ptr1(2,iar).eq.ptr2(2,iar)) then
                       do 5006 i = 1,ngrid
                    if(continue_mrk(ivr).eq.1 ) goto 5006        
                                                                         !
                        if(region_latlon(2,i).ge.ptr1(1,iar).and.        !       x -----------------x 
     +                     region_latlon(2,i).le.ptr2(1,iar)) then       ! (ptr1(1), ptr1(2))   (ptr2(1), ptr2(2))
                          sumdata(ifh,ivr,ilv,iar,iob) =                 !  where ptr1(2) = ptr2(2)  
     +                        sumdata(ifh,ivr,ilv,iar,iob)               !
     +                        + obsvdata(ifh,ivr,ilv,i)
     +                        *area_factor(i)
                          ssqdata(ifh,ivr,ilv,iar,iob) =
     +                        ssqdata(ifh,ivr,ilv,iar,iob)
     +                        + obsvdata(ifh,ivr,ilv,i) *
     +                          obsvdata(ifh,ivr,ilv,i)
     +                        *area_factor(i)
                          sumgrid(ifh,ivr,ilv,iar,iob) =
     +                        sumgrid(ifh,ivr,ilv,iar,iob)
     +                        + fcstdata(ifh,ivr,ilv,i)
     +                        *area_factor(i)
                          ssqgrid(ifh,ivr,ilv,iar,iob) =
     +                        ssqgrid(ifh,ivr,ilv,iar,iob)
     +                        + fcstdata(ifh,ivr,ilv,i) *
     +                          fcstdata(ifh,ivr,ilv,i)
     +                        *area_factor(i)
                          sumprod(ifh,ivr,ilv,iar,iob) =
     +                        sumprod(ifh,ivr,ilv,iar,iob)
     +                        + fcstdata(ifh,ivr,ilv,i) *
     +                          obsvdata(ifh,ivr,ilv,i)
     +                        *area_factor(i)
                          count(ifh,ivr,ilv,iar,iob) =
     +                        count(ifh,ivr,ilv,iar,iob)
     +                        + 1.0
                       weigh(ifh,ivr,ilv,iar,iob) =
     +                 weigh(ifh,ivr,ilv,iar,iob) + area_factor(i)

                         end if
5006                  continue 
                     else if (ptr1(1,iar).ne.ptr2(1,iar).and.    !user defined case 3, all points in a regtangular
     +                        ptr1(2,iar).ne.ptr2(2,iar)) then        !
                       do 5007 i = 1,ngrid                            !
                    if(continue_mrk(ivr).eq.1 ) goto 5007             !            (ptr2(1), ptr2(2))
                        if(region_latlon(2,i).ge.ptr1(1,iar).and.     !      ----------------x
     +                    region_latlon(2,i).le.ptr2(1,iar).and.      !     |                |
     +                    region_latlon(1,i).ge.ptr1(2,iar).and.      !     |                |
     +                    region_latlon(1,i).le.ptr2(2,iar)) then     !     |                |
                          sumdata(ifh,ivr,ilv,iar,iob) =              !     x----------------
     +                        sumdata(ifh,ivr,ilv,iar,iob)            !  (ptr1(1), ptr1(2))
     +                        + obsvdata(ifh,ivr,ilv,i)
     +                        *area_factor(i)
                          ssqdata(ifh,ivr,ilv,iar,iob) =
     +                        ssqdata(ifh,ivr,ilv,iar,iob)
     +                        + obsvdata(ifh,ivr,ilv,i) *
     +                          obsvdata(ifh,ivr,ilv,i)
     +                        *area_factor(i)
                          sumgrid(ifh,ivr,ilv,iar,iob) =
     +                        sumgrid(ifh,ivr,ilv,iar,iob)
     +                        + fcstdata(ifh,ivr,ilv,i)
     +                        *area_factor(i)
                          ssqgrid(ifh,ivr,ilv,iar,iob) =
     +                        ssqgrid(ifh,ivr,ilv,iar,iob)
     +                        + fcstdata(ifh,ivr,ilv,i) *
     +                          fcstdata(ifh,ivr,ilv,i)
     +                        *area_factor(i)
                          sumprod(ifh,ivr,ilv,iar,iob) =
     +                        sumprod(ifh,ivr,ilv,iar,iob)
     +                        + fcstdata(ifh,ivr,ilv,i) *
     +                          obsvdata(ifh,ivr,ilv,i)
     +                        *area_factor(i)
                          count(ifh,ivr,ilv,iar,iob) =
     +                        count(ifh,ivr,ilv,iar,iob)
     +                        + 1.0
                       weigh(ifh,ivr,ilv,iar,iob) =
     +                 weigh(ifh,ivr,ilv,iar,iob) + area_factor(i)

                         end if
5007                  continue                        
                     end if
                   end if

                  end if    !end of region mode
 
50               continue
60             continue
70            continue
80           continue
90         continue

      write(*,*) 'Sum done!'

      do 91 ifh = 1, numfcst
       do 81 iob = 1, numvfyobs
        do 71 iar = 1, numarea
         do 61 ivr = 1, numvarbl

           if(anomly_mrk(ivr).eq.0) goto 61
           if(vectormrk(ivr).ne.0) goto 61
           if(fhomrk(ivr).ne.0)    goto 61
           if(afhomrk(ivr).ne.0)    goto 61
           if(tendencymrk(ivr).ne.0) goto 61

          do 51 ilv = 1, levels(ivr)
           if(nodata(ifh,ivr,ilv).eq.1) goto 51
           if(anomlylev(ivr,ilv).eq.0) goto 51

           if(weigh(ifh,ivr,ilv,iar,iob).le.0.0 ) goto 51
                                                                                                                                                                                   
           sumdata(ifh,ivr,ilv,iar,iob)=
     +     sumdata(ifh,ivr,ilv,iar,iob)/weigh(ifh,ivr,ilv,iar,iob)
                                                                                                                                                                                   
           ssqdata(ifh,ivr,ilv,iar,iob)=
     +     ssqdata(ifh,ivr,ilv,iar,iob)/weigh(ifh,ivr,ilv,iar,iob)
                                                                                                                                                                                   
           sumgrid(ifh,ivr,ilv,iar,iob)=
     +     sumgrid(ifh,ivr,ilv,iar,iob)/weigh(ifh,ivr,ilv,iar,iob)
                                                                                                                                                                                   
           ssqgrid(ifh,ivr,ilv,iar,iob)=
     +     ssqgrid(ifh,ivr,ilv,iar,iob)/weigh(ifh,ivr,ilv,iar,iob)
                                                                                                                                                                                   
           sumprod(ifh,ivr,ilv,iar,iob)=
     +     sumprod(ifh,ivr,ilv,iar,iob)/weigh(ifh,ivr,ilv,iar,iob)
                                                                                                                                                                                   

c           write(*,*) sumdata(ifh,ivr,ilv,iar,iob)
          
51        continue
61       continue
71      continue
81     continue
91    continue
       
           do 190 iob = 1, numvfyobs
            do 180 iar = 1, numarea
             do 170 ifh = 1, numfcst
                ivfdate = ifh
              do 160 ivr = 1, numvarbl
                if(anomly_mrk(ivr).eq.0) goto 160
                if(vectormrk(ivr).ne.0) goto 160
                if(fhomrk(ivr).ne.0)    goto 160
                if(afhomrk(ivr).ne.0)    goto 160
                if(tendencymrk(ivr).ne.0) goto 160
               do 150 ilv = 1, levels(ivr)
                if(nodata(ifh,ivr,ilv).eq.1) goto 150
                if(anomlylev(ivr,ilv).eq.0) goto 150

                    IF (count(ifh,ivr,ilv,iar,iob).gt.0.0) THEN
                      iend = 3
                      vdbhdr132(1:iend) = namversion(:3)
                      iend = iend + 1
                      vdbhdr132(iend:iend) = blank
                      istrt = iend + 1
                      iend = iend + nchrmodel(imodel)
                      vdbhdr132(istrt:iend) =
     +                            namodel(imodel)(:nchrmodel(imodel))
                      iend = iend + 1
                      vdbhdr132(iend:iend) = blank
                      istrt = iend + 1
                      iend = iend + nchrfcst(ifh)
                      vdbhdr132(istrt:iend) =
     +                            namfcst(ifh)(:nchrfcst(ifh))
                      iend = iend + 1
                      vdbhdr132(iend:iend) = blank
                      istrt = iend + 1
                      iend = iend + nchrvfdate(ivfdate)
                      vdbhdr132(istrt:iend) =
     +                            namvfdate(ivfdate)(:
     +                            nchrvfdate(ivfdate))
                      iend = iend + 1
                      vdbhdr132(iend:iend) = blank
                      istrt = iend + 1
                      iend = iend + nchrvfyobs(iob)
                      vdbhdr132(istrt:iend) =
     +                            namvfyobs(iob)(:nchrvfyobs(iob))
                      iend = iend + 1
                      vdbhdr132(iend:iend) = blank
                      istrt = iend + 1
                      iend = iend + nchrarea(iar)
                      vdbhdr132(istrt:iend) =
     +                            namarea(iar)(:nchrarea(iar))
                      iend = iend + 1
                      vdbhdr132(iend:iend) = blank
                      istrt = iend + 1
                      ivstrt = istrt
                      iend = iend + nchrstat(ist)
                      vdbhdr132(istrt:iend) =
     +                            namstat(ist)(:nchrstat(ist))
                      iend = iend + 1
                      vdbhdr132(iend:iend) = blank
                      istrt = iend + 1
                      iend = iend + nchrvarbl(ivr)
                      vdbhdr132(istrt:iend) =
     +                            namvarbl(ivr)(:nchrvarbl(ivr))
                     iend = iend + 1
                      vdbhdr132(iend:iend) = blank
                      istrt = iend + 1
                      iend = iend + nchrlevel(ilv)
                      vdbhdr132(istrt:iend) =
     +                       namlvl(ivr,ilv)(:nchrlvl(ivr,ilv))
                      iend = iend + 1
                      vdbhdr132(iend:iend) = blank
                      iend = iend + 1
                      vdbhdr132(iend:iend) = equal
                      iend = iend + 1
                      vdbhdr132(iend:iend) = blank
                       if(hasdata(ifh,ivr,ilv).gt.0) then
                        WRITE (nvsdb,1000) vdbhdr132(:iend),
     +                              count(ifh,ivr,ilv,iar,iob),
     +                              sumgrid(ifh,ivr,ilv,iar,iob),
     +                              sumdata(ifh,ivr,ilv,iar,iob),
     +                              sumprod(ifh,ivr,ilv,iar,iob),
     +                              ssqgrid(ifh,ivr,ilv,iar,iob),
     +                              ssqdata(ifh,ivr,ilv,iar,iob)
 1000                   FORMAT (A,F7.0,5E18.9)
                       end if
                    END IF
150            continue
160           continue
170          continue
180         continue
190        continue

           deallocate(sumdata)
           deallocate(sumgrid)
           deallocate(sumprod)
           deallocate(ssqdata)
           deallocate(ssqgrid)
           deallocate(count)
           deallocate(weigh)
           deallocate(fcstdata)                                                                                                                                         
           deallocate(obsvdata)     

           return
           end
