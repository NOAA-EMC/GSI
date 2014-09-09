        subroutine ESL1L2(nvsdb,Nmodel,ensname,ist,numfcst,
     +    numvfyobs,numarea,numvarbl,numlevel,ngrid,levels,
     +    hasdata,fcstmdl,obsvdata)

c    This program is to generate ESL1L2 partial sum for ensemble forecast
c    Author: Binbin Zhou
c            Feb 29 2007
                                                                                                                 


      INCLUDE 'parm.inc'
      
      INTEGER, intent(IN) :: Nmodel, ist, numvfyobs,numarea,
     +         numfcst,numvarbl,numlevel,ngrid, levels(mxvrbl),
     +         hasdata(mxfcst,mxvrbl,maxlvl)

       REAL,dimension(Nmodel,numfcst,numvarbl,numlevel,ngrid),
     +       intent(IN) :: fcstmdl
                                                                                                                                                            
      CHARACTER*20, intent(IN) ::  ensname           !such as WRF/6, ETA/10, RSM/5, SREF/21

      REAL,dimension(numfcst,numvarbl,numlevel,ngrid),intent(IN) ::
     +          obsvdata


      real*8,  allocatable, dimension(:,:,:,:,:) :: sumdata,
     +      sumgrid,sumprod,ssqdata,ssqgrid,count,weigh,
     +      sumensvar,sumfrcle3
                                                                                                                                                        
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

      integer nodata(mxfcst,mxvrbl,maxlvl)
      COMMON /nofile/nodata

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
      CHARACTER*24 afho(mxvrbl),   afhothr(mxvrbl,20)
      integer  nchrfho(mxvrbl),nchrfhothr(mxvrbl,20),fhomrk(mxvrbl)
      integer  nchrafho(mxvrbl),nchrafhothr(mxvrbl,20),afhomrk(mxvrbl)
      real rfhothr(mxvrbl,20)
      real rafhothr(mxvrbl,20)
      integer continue_mrk(mxvrbl)
      integer anomlylev(mxvrbl,maxlvl),anomly_mrk(mxvrbl)


      COMMON /g2g/cyyyyfcst,cmmfcst,cddfcst,chhfcst,cfffcst,
     +            cyyyyobsv,cmmobsv,cddobsv,chhobsv,cffobsv,
     +             yyyyfcst, mmfcst, ddfcst, hhfcst, fffcst,
     +             yyyyobsv, mmobsv, ddobsv, hhobsv, ffobsv,
     +             k5,k6,k7,ck7,vectormrk,namlvl,nchrlvl,
     +        fhomrk,fho,nchrfho,fhothr,nchrfhothr,rfhothr,
     +             continue_mrk, anomly_mrk, anomlylev,
     +     afhomrk,afho,nchrafho,afhothr,nchrafhothr,rafhothr


      integer plevel(maxlvl)
      integer region_id(maxpts)
      real region_latlon(2,maxpts),ptr1(2,mxarea), ptr2(2,mxarea)
 
      COMMON /reg/region_id, region_latlon,ptr1,ptr2
      COMMON /layer/ modelvl(maxlvl), iplevel(maxlvl,2)


      real area_factor(maxpts)               !add for area weighting of grid area in high latitude
      COMMON /weight/area_factor

      CHARACTER*4 CNmodel
      integer d1,d2,d3,numCNmodel

      DATA blank /' '/
      DATA equal /'='/
      DATA namversion /'V01'/
      DATA bmiss /10E10/
      DATA rmiss /99999./

      if(Nmodel.lt.10) then
       CNmodel='/'//achar(Nmodel+48)
       numCNmodel = 2
      else if(Nmodel.ge.10.and.Nmodel.lt.100) then
       d1 = Nmodel/10
       d2 = Nmodel - d1*10
       CNmodel='/'//achar(d1+48)//achar(d2+48)
       numCNmodel = 3
      else
       d1 = Nmodel/100
       d2 = Nmodel/10 - d1*10
       d3 = Nmodel - d1*100 -d2*10 
       CNmodel='/'//achar(d1+48)//achar(d2+48)//achar(d3+48)
       numCNmodel = 4
      end if


      write(*,*) 'In ESL1L2:',Nmodel,ist,numfcst,numvfyobs,numarea,
     +  numvarbl,numlevel,ngrid

       
      write(*,*) 'fhomrk', (fhomrk(ivr),ivr=1,numvarbl)
      write(*,*) 'afhomrk', (afhomrk(ivr),ivr=1,numvarbl)
      write(*,*) 'rafhothr', (rafhothr(6,i),i=1,5)
      do i=1,numfcst
       write(*,*) 'nfcst=',i
       do j=1,numvarbl
        write(*,*) 'ivr=',j,' nodata', (nodata(i,j,k),k=1,numfcst)
       end do
      end do

        allocate (sumdata(numfcst,numvarbl,numlevel,numarea,numvfyobs))
        allocate (sumgrid(numfcst,numvarbl,numlevel,numarea,numvfyobs))
        allocate (sumprod(numfcst,numvarbl,numlevel,numarea,numvfyobs))
        allocate (ssqdata(numfcst,numvarbl,numlevel,numarea,numvfyobs))
        allocate (ssqgrid(numfcst,numvarbl,numlevel,numarea,numvfyobs))
        allocate   (count(numfcst,numvarbl,numlevel,numarea,numvfyobs))
        allocate   (weigh(numfcst,numvarbl,numlevel,numarea,numvfyobs))
        allocate (sumensvar(numfcst,numvarbl,numlevel,numarea,
     +            numvfyobs))
        allocate (sumfrcle3(numfcst,numvarbl,numlevel,numarea,
     +            numvfyobs))

c       do nvar=1,numvarbl
c         do np =1, levels(nvar)
c          write (*,*) namlevel(np), namlvl(nvar,np)
c         end do
c       end do


c      do ifh = 1, numfcst
c      do ivr = 1, numvarbl
c      write(*,*)'ifh=',ifh,' ivr=',ivr
c      do ilv = 1, levels(ivr)
c       write(*,'(i8, 10f10.2)')ilv,(fcstdata(ifh,ivr,ilv,i),i=1,10)
c       write(*,'(i8, 10f10.2)')ilv,(obsvdata(ifh,ivr,ilv,i),i=1,10)
c      end do
c      end do
c      end do
 

          count = 0.0
          weigh = 0.0
          sumdata = 0.0
          ssqdata = 0.0
          sumgrid = 0.0
          ssqgrid = 0.0
          sumprod = 0.0
          smiss=-1.0E5
          sumensvar=0.0
          sunfrcle3=0.0

      do 90 ifh = 1, numfcst
        do 80 iob = 1, numvfyobs
          do 70 iar = 1, numarea
           do 60 ivr = 1, numvarbl
              if(vectormrk(ivr).ne.0) goto 60     !
              if(fhomrk(ivr).ne.0)    goto 60
              if(afhomrk(ivr).ne.0)    goto 60
             do 50 ilv = 1, levels(ivr)
              if(nodata(ifh,ivr,ilv).eq.1) goto 50
                if (mode(iar).eq.1) then                          !all GRID domain (mode 1)
                  do 5001 i = 1,ngrid
                    if(continue_mrk(ivr).eq.1 .and.               
     +                 obsvdata(ifh,ivr,ilv,i).lt.0.0 ) goto 5001    !for cloud-like uncontinuous parameters

c                    if(continue_mrk(ivr).eq.2 .and.(
c     +                 obsvdata(ifh,ivr,ilv,i).lt.0.0 .or.
c     +                  fcstdata(ifh,ivr,ilv,i).lt.0.0) ) goto 5001  !for aerosol/smoke
 

CCCCCCCCCCC      Prepare Ensemble mean/spread CCCCCCCCCCCCCCCCCCCCCCCCCC 

              forcst = 0.
              do imodel=1,Nmodel                                 !mean of fcst
               forcst = forcst + fcstmdl(imodel,ifh,ivr,ilv,i)
              end do
          
              forcst = forcst / float(Nmodel)
              obsval = obsvdata(ifh,ivr,ilv,i)                    !mean of obsv (actually single)
 
              fcstva = 0.                                         
              fcstdiffsq=0.0
              do imodel=1,Nmodel
               fcstdiffsq=fcstdiffsq+(fcstmdl(imodel,ifh,ivr,ilv,i)
     +           - forcst)**2.0
              end do
              fcstva=(fcstdiffsq/float(Nmodel-1))**0.5            !spread

              frcle3 = 0.
              do imodel=1,Nmodel
                fcstdiff=abs(fcstmdl(imodel,ifh,ivr,ilv,i)-forcst)             
                if(fcstdiff.ge. (3.0*fcstva)) frcle3=frcle3+1.0
              end do
              frcle3 = frcle3/float(Nmodel)

c              if(i.lt.1000) then
c               write(*,'(i7, 8f8.2)') i, 
c     +         (fcstmdl(imodel,ifh,ivr,ilv,i),imodel=1,Nmodel),
c     +         forcst,obsval,fcstva 
c              end if
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

                       sumdata(ifh,ivr,ilv,iar,iob) =           !Mean[o]
     +                        sumdata(ifh,ivr,ilv,iar,iob)
     +                        + obsvdata(ifh,ivr,ilv,i)
     +                        *area_factor(i)

                       ssqdata(ifh,ivr,ilv,iar,iob) =           !mean [o*o]
     +                        ssqdata(ifh,ivr,ilv,iar,iob) 
     +                        + obsvdata(ifh,ivr,ilv,i) *
     +                          obsvdata(ifh,ivr,ilv,i)
     +                        *area_factor(i)*area_factor(i)

                       sumgrid(ifh,ivr,ilv,iar,iob) =           !mean [f]
     +                        sumgrid(ifh,ivr,ilv,iar,iob) 
     +                        + forcst*area_factor(i)

                       ssqgrid(ifh,ivr,ilv,iar,iob) =           !mean [f*f] 
     +                        ssqgrid(ifh,ivr,ilv,iar,iob) 
     +                        + forcst*forcst
     +                        *area_factor(i)*area_factor(i)

                       sumprod(ifh,ivr,ilv,iar,iob) =           !mean [f*o] 
     +                        sumprod(ifh,ivr,ilv,iar,iob) 
     +                        + forcst*obsvdata(ifh,ivr,ilv,i)
     +                        *area_factor(i)*area_factor(i)

                       sumensvar(ifh,ivr,ilv,iar,iob) =         !variance
     +                        sumensvar(ifh,ivr,ilv,iar,iob)
     +                        + fcstva**2
     +                        *area_factor(i)*area_factor(i)

                       sumfrcle3(ifh,ivr,ilv,iar,iob) =
     +                        sumfrcle3(ifh,ivr,ilv,iar,iob)
     +                        + frcle3*area_factor(i)

                       count(ifh,ivr,ilv,iar,iob) =
     +                        count(ifh,ivr,ilv,iar,iob)
     +                        + 1.0
                       weigh(ifh,ivr,ilv,iar,iob) =
     +                 weigh(ifh,ivr,ilv,iar,iob) + area_factor(i)

5001                continue
                else if (mode(iar).eq.2) then                    ! GRID#104  (mode 2)
                  if (numreg(iar).le.30) then                    !         sub-region
                    do 5002 i = 1,ngrid
                    if(continue_mrk(ivr).eq.1.and.
     +               obsvdata(ifh,ivr,ilv,i).lt.0.0 ) goto 5002

c                    if(continue_mrk(ivr).eq.2 .and.(
c     +                 obsvdata(ifh,ivr,ilv,i).lt.0.0 .or.
c     +                  fcstdata(ifh,ivr,ilv,i).lt.0.0) ) goto 5002 !for aerosol/smoke

                      if(region_id(i).eq.numreg(iar)) then

CCCCCCCCCCC      Prepare Ensemble mean/spread CCCCCCCCCCCCCCCCCCCCCCCCCC
                                                                                                                                                 
              forcst = 0.
              do imodel=1,Nmodel
               forcst = forcst + fcstmdl(imodel,ifh,ivr,ilv,i)
              end do
              forcst = forcst / Nmodel
              obsval = obsvdata(ifh,ivr,ilv,i)
                                                                                                                                                 
              fcstva = 0.
              fcstdiffsq=0.0
              do imodel=1,Nmodel
               fcstdiffsq=fcstdiffsq+(fcstmdl(imodel,ifh,ivr,ilv,i)
     +           - forcst)**2.0
              end do
              fcstva=(fcstdiffsq/float(Nmodel-1))**0.5
                                                                                                                                                 
              frcle3 = 0.
              do imodel=1,Nmodel
                fcstdiff=abs(fcstmdl(imodel,ifh,ivr,ilv,i)-forcst)
                if(fcstdiff.ge. (3.0*fcstva)) frcle3=frcle3+1.0
              end do
              frcle3 = frcle3/float(Nmodel)
                                                                                                                                                 
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

                       sumdata(ifh,ivr,ilv,iar,iob) =
     +                        sumdata(ifh,ivr,ilv,iar,iob)
     +                        + obsvdata(ifh,ivr,ilv,i)
     +                        *area_factor(i)
                                                                                                                                                 
                       ssqdata(ifh,ivr,ilv,iar,iob) =
     +                        ssqdata(ifh,ivr,ilv,iar,iob)
     +                        + obsvdata(ifh,ivr,ilv,i) *
     +                          obsvdata(ifh,ivr,ilv,i)
     +                        *area_factor(i)*area_factor(i)
                                                                                                                                                 
                       sumgrid(ifh,ivr,ilv,iar,iob) =
     +                        sumgrid(ifh,ivr,ilv,iar,iob)
     +                        + forcst*area_factor(i)
                                                                                                                                                 
                       ssqgrid(ifh,ivr,ilv,iar,iob) =
     +                        ssqgrid(ifh,ivr,ilv,iar,iob)
     +                        + forcst*forcst
     +                        *area_factor(i)*area_factor(i)
                                                                                                                                                 
                       sumprod(ifh,ivr,ilv,iar,iob) =
     +                        sumprod(ifh,ivr,ilv,iar,iob)
     +                        + forcst*obsvdata(ifh,ivr,ilv,i)
     +                        *area_factor(i)*area_factor(i)
                                                       
                       sumensvar(ifh,ivr,ilv,iar,iob) =
     +                        sumensvar(ifh,ivr,ilv,iar,iob)
     +                        + fcstva**2
     +                        *area_factor(i)*area_factor(i)
                                                                                                                                                 
                       sumfrcle3(ifh,ivr,ilv,iar,iob) =
     +                        sumfrcle3(ifh,ivr,ilv,iar,iob)
     +                        + frcle3*area_factor(i)
                                                                                                                                                 
                       count(ifh,ivr,ilv,iar,iob) =
     +                        count(ifh,ivr,ilv,iar,iob)
     +                        + 1.0
                       weigh(ifh,ivr,ilv,iar,iob) =
     +                 weigh(ifh,ivr,ilv,iar,iob) + area_factor(i)
 
                      end if

5002                continue
  
                   else if(numreg(iar).eq.31 ) then             !        N. hemisphere
 
                     do 5003 i = 1,ngrid
                    if(continue_mrk(ivr).eq.1.and.
     +                 obsvdata(ifh,ivr,ilv,i).lt.0.0 ) goto 5003

c                    if(continue_mrk(ivr).eq.2 .and.(
c     +                 obsvdata(ifh,ivr,ilv,i).lt.0.0 .or.
c     +                  fcstdata(ifh,ivr,ilv,i).lt.0.0) ) goto 5003  !for aerosol/smoke

                      if(region_latlon(1,i).gt.0.0) then

CCCCCCCCCCC      Prepare Ensemble mean/spread CCCCCCCCCCCCCCCCCCCCCCCCCC
                                                                                                                                                 
              forcst = 0.
              do imodel=1,Nmodel
               forcst = forcst + fcstmdl(imodel,ifh,ivr,ilv,i)
              end do
              forcst = forcst / Nmodel
              obsval = obsvdata(ifh,ivr,ilv,i)
                                                                                                                                                 
              fcstva = 0.
              fcstdiffsq=0.0
              do imodel=1,Nmodel
               fcstdiffsq=fcstdiffsq+(fcstmdl(imodel,ifh,ivr,ilv,i)
     +           - forcst)**2.0
              end do
              fcstva=(fcstdiffsq/float(Nmodel-1))**0.5
                                                                                                                                                 
              frcle3 = 0.
              do imodel=1,Nmodel
                fcstdiff=abs(fcstmdl(imodel,ifh,ivr,ilv,i)-forcst)
                if(fcstdiff.ge. (3.0*fcstva)) frcle3=frcle3+1.0
              end do
              frcle3 = frcle3/float(Nmodel)
                                                                                                                                                 
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
                                                                                                                                                 
                       sumdata(ifh,ivr,ilv,iar,iob) =
     +                        sumdata(ifh,ivr,ilv,iar,iob)
     +                        + obsvdata(ifh,ivr,ilv,i)
     +                        *area_factor(i)
                                                                                                                                                 
                       ssqdata(ifh,ivr,ilv,iar,iob) =
     +                        ssqdata(ifh,ivr,ilv,iar,iob)
     +                        + obsvdata(ifh,ivr,ilv,i) *
     +                          obsvdata(ifh,ivr,ilv,i)
     +                        *area_factor(i)*area_factor(i)
                                                                                                                                                 
                       sumgrid(ifh,ivr,ilv,iar,iob) =
     +                        sumgrid(ifh,ivr,ilv,iar,iob)
     +                        + forcst*area_factor(i)
                               
                       ssqgrid(ifh,ivr,ilv,iar,iob) =
     +                        ssqgrid(ifh,ivr,ilv,iar,iob)
     +                        + forcst*forcst
     +                        *area_factor(i)*area_factor(i)
                               
                       sumprod(ifh,ivr,ilv,iar,iob) =
     +                        sumprod(ifh,ivr,ilv,iar,iob)
     +                        + forcst*obsvdata(ifh,ivr,ilv,i)
     +                        *area_factor(i)*area_factor(i)
                               
                       sumensvar(ifh,ivr,ilv,iar,iob) =
     +                        sumensvar(ifh,ivr,ilv,iar,iob)
     +                        + fcstva**2
     +                        *area_factor(i)*area_factor(i)
                               
                       sumfrcle3(ifh,ivr,ilv,iar,iob) =
     +                        sumfrcle3(ifh,ivr,ilv,iar,iob)
     +                        + frcle3*area_factor(i)
                               
                       count(ifh,ivr,ilv,iar,iob) =
     +                        count(ifh,ivr,ilv,iar,iob)
     +                        + 1.0
                       weigh(ifh,ivr,ilv,iar,iob) =
     +                 weigh(ifh,ivr,ilv,iar,iob) + area_factor(i)



                      end if
5003                continue  
                   else if(numreg(iar).eq.32 ) then             !        S. hemisphere
 
                     do 5004 i = 1,ngrid
                    if(continue_mrk(ivr).eq.1.and.
     +                 obsvdata(ifh,ivr,ilv,i).lt.0.0 ) goto 5004

c                    if(continue_mrk(ivr).eq.2 .and.(
c     +                 obsvdata(ifh,ivr,ilv,i).lt.0.0 .or.
c     +                  fcstdata(ifh,ivr,ilv,i).lt.0.0) ) goto 5004  !for aerosol/smoke

                      if(region_latlon(1,i).lt.0.0) then

CCCCCCCCCCC      Prepare Ensemble mean/spread CCCCCCCCCCCCCCCCCCCCCCCCCC
                                                                                                                                                 
              forcst = 0.
              do imodel=1,Nmodel
               forcst = forcst + fcstmdl(imodel,ifh,ivr,ilv,i)
              end do
              forcst = forcst / Nmodel
              obsval = obsvdata(ifh,ivr,ilv,i)
                                                                                                                                                 
              fcstva = 0.
              fcstdiffsq=0.0
              do imodel=1,Nmodel
               fcstdiffsq=fcstdiffsq+(fcstmdl(imodel,ifh,ivr,ilv,i)
     +           - forcst)**2.0
              end do
              fcstva=(fcstdiffsq/float(Nmodel-1))**0.5
                                                                                                                                                 
              frcle3 = 0.
              do imodel=1,Nmodel
                fcstdiff=abs(fcstmdl(imodel,ifh,ivr,ilv,i)-forcst)
                if(fcstdiff.ge. (3.0*fcstva)) frcle3=frcle3+1.0
              end do
              frcle3 = frcle3/float(Nmodel)
                                                                                                                                                 
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
                                                                                                                                                 
                       sumdata(ifh,ivr,ilv,iar,iob) =
     +                        sumdata(ifh,ivr,ilv,iar,iob)
     +                        + obsvdata(ifh,ivr,ilv,i)
     +                        *area_factor(i)
                                                                                                                                                 
                       ssqdata(ifh,ivr,ilv,iar,iob) =
     +                        ssqdata(ifh,ivr,ilv,iar,iob)
     +                        + obsvdata(ifh,ivr,ilv,i) *
     +                          obsvdata(ifh,ivr,ilv,i)
     +                        *area_factor(i)*area_factor(i)
                                                                                                                                                 
                       sumgrid(ifh,ivr,ilv,iar,iob) =
     +                        sumgrid(ifh,ivr,ilv,iar,iob)
     +                        + forcst*area_factor(i)
                               
                       ssqgrid(ifh,ivr,ilv,iar,iob) =
     +                        ssqgrid(ifh,ivr,ilv,iar,iob)
     +                        + forcst*forcst
     +                        *area_factor(i)*area_factor(i)
                               
                       sumprod(ifh,ivr,ilv,iar,iob) =
     +                        sumprod(ifh,ivr,ilv,iar,iob)
     +                        + forcst*obsvdata(ifh,ivr,ilv,i)
     +                        *area_factor(i)*area_factor(i)
                               
                       sumensvar(ifh,ivr,ilv,iar,iob) =
     +                        sumensvar(ifh,ivr,ilv,iar,iob)
     +                        + fcstva**2
     +                        *area_factor(i)*area_factor(i)
                               
                       sumfrcle3(ifh,ivr,ilv,iar,iob) =
     +                        sumfrcle3(ifh,ivr,ilv,iar,iob)
     +                        + frcle3*area_factor(i)
                               
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
                    if(continue_mrk(ivr).eq.1.and.                       !  |
     +                 obsvdata(ifh,ivr,ilv,i).lt.0.0 ) goto 5005        !  |
                                                                         !  |
c                    if(continue_mrk(ivr).eq.2 .and.(                     !  |
c     +                 obsvdata(ifh,ivr,ilv,i).lt.0.0 .or.               !  |
c     +                  fcstdata(ifh,ivr,ilv,i).lt.0.0) ) goto 5005      !  |
                                                                         !  |
                        if(region_latlon(1,i).ge.ptr1(2,iar).and.        !  |
     +                     region_latlon(1,i).le.ptr2(2,iar)) then       !  |
                                                                         !  x(ptr1(1), ptr1(2))  
                                                                         !  where ptr1(1) = ptr2(1)
CCCCCCCCCCC      Prepare Ensemble mean/spread CCCCCCCCCCCCCCCCCCCCCCCCCC
                                                                                                                                                 
              forcst = 0.
              do imodel=1,Nmodel
               forcst = forcst + fcstmdl(imodel,ifh,ivr,ilv,i)
              end do
              forcst = forcst / Nmodel
              obsval = obsvdata(ifh,ivr,ilv,i)
                                                                                                                                                 
              fcstva = 0.
              fcstdiffsq=0.0
              do imodel=1,Nmodel
               fcstdiffsq=fcstdiffsq+(fcstmdl(imodel,ifh,ivr,ilv,i)
     +           - forcst)**2.0
              end do
              fcstva=(fcstdiffsq/float(Nmodel-1))**0.5
                                                                                                                                                 
              frcle3 = 0.
              do imodel=1,Nmodel
                fcstdiff=abs(fcstmdl(imodel,ifh,ivr,ilv,i)-forcst)
                if(fcstdiff.ge. (3.0*fcstva)) frcle3=frcle3+1.0
              end do
              frcle3 = frcle3/float(Nmodel)
                                                                                                                                                 
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
                                                                                                                                                 
                       sumdata(ifh,ivr,ilv,iar,iob) =
     +                        sumdata(ifh,ivr,ilv,iar,iob)
     +                        + obsvdata(ifh,ivr,ilv,i)
     +                        *area_factor(i)
                                                                                                                                                 
                       ssqdata(ifh,ivr,ilv,iar,iob) =
     +                        ssqdata(ifh,ivr,ilv,iar,iob)
     +                        + obsvdata(ifh,ivr,ilv,i) *
     +                          obsvdata(ifh,ivr,ilv,i)
     +                        *area_factor(i)*area_factor(i)
                                                                                                                                                 
                       sumgrid(ifh,ivr,ilv,iar,iob) =
     +                        sumgrid(ifh,ivr,ilv,iar,iob)
     +                        + forcst*area_factor(i)
                               
                       ssqgrid(ifh,ivr,ilv,iar,iob) =
     +                        ssqgrid(ifh,ivr,ilv,iar,iob)
     +                        + forcst*forcst
     +                        *area_factor(i)*area_factor(i)
                               
                       sumprod(ifh,ivr,ilv,iar,iob) =
     +                        sumprod(ifh,ivr,ilv,iar,iob)
     +                        + forcst*obsvdata(ifh,ivr,ilv,i)
     +                        *area_factor(i)*area_factor(i)
                               
                       sumensvar(ifh,ivr,ilv,iar,iob) =
     +                        sumensvar(ifh,ivr,ilv,iar,iob)
     +                        + fcstva**2
     +                        *area_factor(i)*area_factor(i)
                               
                       sumfrcle3(ifh,ivr,ilv,iar,iob) =
     +                        sumfrcle3(ifh,ivr,ilv,iar,iob)
     +                        + frcle3*area_factor(i)
                               
                       count(ifh,ivr,ilv,iar,iob) =
     +                        count(ifh,ivr,ilv,iar,iob)
     +                        + 1.0
                       weigh(ifh,ivr,ilv,iar,iob) =
     +                 weigh(ifh,ivr,ilv,iar,iob) + area_factor(i)

 
                         end if
5005                  continue 
                     else if (ptr1(1,iar).ne.ptr2(1,iar).and.    !user defined case 2, all points along longitude 
     +                        ptr1(2,iar).eq.ptr2(2,iar)) then
                       do 5006 i = 1,ngrid
                    if(continue_mrk(ivr).eq.1.and.
     +                 obsvdata(ifh,ivr,ilv,i).lt.0.0 ) goto 5006  

c                    if(continue_mrk(ivr).eq.2 .and.(
c     +                 obsvdata(ifh,ivr,ilv,i).lt.0.0 .or.
c     +                  fcstdata(ifh,ivr,ilv,i).lt.0.0) ) goto 5006  !for aerosol/smoke

                                                                         !
                        if(region_latlon(2,i).ge.ptr1(1,iar).and.        !       x -----------------x 
     +                     region_latlon(2,i).le.ptr2(1,iar)) then       ! (ptr1(1), ptr1(2))   (ptr2(1), ptr2(2))
                                                                         !  where ptr1(2) = ptr2(2)
CCCCCCCCCCC      Prepare Ensemble mean/spread CCCCCCCCCCCCCCCCCCCCCCCCCC
                                                                                                                                                 
              forcst = 0.
              do imodel=1,Nmodel
               forcst = forcst + fcstmdl(imodel,ifh,ivr,ilv,i)
              end do
              forcst = forcst / Nmodel
              obsval = obsvdata(ifh,ivr,ilv,i)
                                                                                                                                                 
              fcstva = 0.
              fcstdiffsq=0.0
              do imodel=1,Nmodel
               fcstdiffsq=fcstdiffsq+(fcstmdl(imodel,ifh,ivr,ilv,i)
     +           - forcst)**2.0
              end do
              fcstva=(fcstdiffsq/float(Nmodel-1))**0.5
                                                                                                                                                 
              frcle3 = 0.
              do imodel=1,Nmodel
                fcstdiff=abs(fcstmdl(imodel,ifh,ivr,ilv,i)-forcst)
                if(fcstdiff.ge. (3.0*fcstva)) frcle3=frcle3+1.0
              end do
              frcle3 = frcle3/float(Nmodel)
                                                                                                                                                 
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
                                                                                                                                                 
                       sumdata(ifh,ivr,ilv,iar,iob) =
     +                        sumdata(ifh,ivr,ilv,iar,iob)
     +                        + obsvdata(ifh,ivr,ilv,i)
     +                        *area_factor(i)
                                                                                                                                                 
                       ssqdata(ifh,ivr,ilv,iar,iob) =
     +                        ssqdata(ifh,ivr,ilv,iar,iob)
     +                        + obsvdata(ifh,ivr,ilv,i) *
     +                          obsvdata(ifh,ivr,ilv,i)
     +                        *area_factor(i)*area_factor(i)
                                                                                                                                                 
                       sumgrid(ifh,ivr,ilv,iar,iob) =
     +                        sumgrid(ifh,ivr,ilv,iar,iob)
     +                        + forcst*area_factor(i)
                               
                       ssqgrid(ifh,ivr,ilv,iar,iob) =
     +                        ssqgrid(ifh,ivr,ilv,iar,iob)
     +                        + forcst*forcst
     +                        *area_factor(i)*area_factor(i)
                               
                       sumprod(ifh,ivr,ilv,iar,iob) =
     +                        sumprod(ifh,ivr,ilv,iar,iob)
     +                        + forcst*obsvdata(ifh,ivr,ilv,i)
     +                        *area_factor(i)*area_factor(i)
                               
                       sumensvar(ifh,ivr,ilv,iar,iob) =
     +                        sumensvar(ifh,ivr,ilv,iar,iob)
     +                        + fcstva**2
     +                        *area_factor(i)*area_factor(i)
                               
                       sumfrcle3(ifh,ivr,ilv,iar,iob) =
     +                        sumfrcle3(ifh,ivr,ilv,iar,iob)
     +                        + frcle3*area_factor(i)
                               
                       count(ifh,ivr,ilv,iar,iob) =
     +                        count(ifh,ivr,ilv,iar,iob)
     +                        + 1.0
                       weigh(ifh,ivr,ilv,iar,iob) =
     +                 weigh(ifh,ivr,ilv,iar,iob) + area_factor(i)
 
                         end if
5006                  continue 
                     else if (ptr1(1,iar).ne.ptr2(1,iar).and.    !user defined case 3, all points in a regtangular
     +                        ptr1(2,iar).ne.ptr2(2,iar)) then        !
                       do 5007 i = 1,ngrid      
                    if(continue_mrk(ivr).eq.1.and.
     +                 obsvdata(ifh,ivr,ilv,i).lt.0.0 ) goto 5007

c                    if(continue_mrk(ivr).eq.2 .and.(
c     +                 obsvdata(ifh,ivr,ilv,i).lt.0.0 .or.
c     +                  fcstdata(ifh,ivr,ilv,i).lt.0.0) ) goto 5007  !for aerosol/smoke

                                                                      !            (ptr2(1), ptr2(2))
                        if(region_latlon(2,i).ge.ptr1(1,iar).and.     !      ----------------x
     +                    region_latlon(2,i).le.ptr2(1,iar).and.      !     |                |
     +                    region_latlon(1,i).ge.ptr1(2,iar).and.      !     |                |
     +                    region_latlon(1,i).le.ptr2(2,iar)) then     !     |                |
                                                                      !     x----------------
                                                                      !  (ptr1(1), ptr1(2))
CCCCCCCCCCC      Prepare Ensemble mean/spread CCCCCCCCCCCCCCCCCCCCCCCCCC
                                                                                                                                                 
              forcst = 0.
              do imodel=1,Nmodel
               forcst = forcst + fcstmdl(imodel,ifh,ivr,ilv,i)
              end do
              forcst = forcst / Nmodel
              obsval = obsvdata(ifh,ivr,ilv,i)
                                                                                                                                                 
              fcstva = 0.
              fcstdiffsq=0.0
              do imodel=1,Nmodel
               fcstdiffsq=fcstdiffsq+(fcstmdl(imodel,ifh,ivr,ilv,i)
     +           - forcst)**2.0
              end do
              fcstva=(fcstdiffsq/float(Nmodel-1))**0.5
                                                                                                                                                 
              frcle3 = 0.
              do imodel=1,Nmodel
                fcstdiff=abs(fcstmdl(imodel,ifh,ivr,ilv,i)-forcst)
                if(fcstdiff.ge. (3.0*fcstva)) frcle3=frcle3+1.0
              end do
              frcle3 = frcle3/float(Nmodel)
                                                                                                                                                 
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
                                                                                                                                                 
                       sumdata(ifh,ivr,ilv,iar,iob) =
     +                        sumdata(ifh,ivr,ilv,iar,iob)
     +                        + obsvdata(ifh,ivr,ilv,i)
     +                        *area_factor(i)
                                                                                                                                                 
                       ssqdata(ifh,ivr,ilv,iar,iob) =
     +                        ssqdata(ifh,ivr,ilv,iar,iob)
     +                        + obsvdata(ifh,ivr,ilv,i) *
     +                          obsvdata(ifh,ivr,ilv,i)
     +                        *area_factor(i)*area_factor(i)
                                                                                                                                                 
                       sumgrid(ifh,ivr,ilv,iar,iob) =
     +                        sumgrid(ifh,ivr,ilv,iar,iob)
     +                        + forcst*area_factor(i)
                               
                       ssqgrid(ifh,ivr,ilv,iar,iob) =
     +                        ssqgrid(ifh,ivr,ilv,iar,iob)
     +                        + forcst*forcst
     +                        *area_factor(i)*area_factor(i)
                               
                       sumprod(ifh,ivr,ilv,iar,iob) =
     +                        sumprod(ifh,ivr,ilv,iar,iob)
     +                        + forcst*obsvdata(ifh,ivr,ilv,i)
     +                        *area_factor(i)*area_factor(i)
                               
                       sumensvar(ifh,ivr,ilv,iar,iob) =
     +                        sumensvar(ifh,ivr,ilv,iar,iob)
     +                        + fcstva**2
     +                        *area_factor(i)*area_factor(i)
                               
                       sumfrcle3(ifh,ivr,ilv,iar,iob) =
     +                        sumfrcle3(ifh,ivr,ilv,iar,iob)
     +                        + frcle3*area_factor(i)
                               
                       count(ifh,ivr,ilv,iar,iob) =
     +                        count(ifh,ivr,ilv,iar,iob)
     +                        + 1.0
                       weigh(ifh,ivr,ilv,iar,iob) =
     +                 weigh(ifh,ivr,ilv,iar,iob) + area_factor(i)
                         
                         end if
5007                  continue                
        
                     end if !end of sub-region

                   end if   !end of region mode

                  end if    !end of nodata check
 
50               continue
60             continue
70            continue
80           continue
90         continue

      do 91 ifh = 1, numfcst
       do 81 iob = 1, numvfyobs
        do 71 iar = 1, numarea
         do 61 ivr = 1, numvarbl
           if(vectormrk(ivr).ne.0) goto 61
           if(fhomrk(ivr).ne.0)    goto 61
           if(afhomrk(ivr).ne.0)    goto 61
          do 51 ilv = 1, levels(ivr)
           if(nodata(ifh,ivr,ilv).eq.1) goto 51
           if(weigh(ifh,ivr,ilv,iar,iob).eq.0.0) goto 51
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

           sumensvar(ifh,ivr,ilv,iar,iob)=
     +     sumensvar(ifh,ivr,ilv,iar,iob)/weigh(ifh,ivr,ilv,iar,iob)

           sumfrcle3(ifh,ivr,ilv,iar,iob)=
     +     sumfrcle3(ifh,ivr,ilv,iar,iob)/weigh(ifh,ivr,ilv,iar,iob)


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
                if(vectormrk(ivr).ne.0) goto 160
                if(fhomrk(ivr).ne.0)    goto 160        !
                if(afhomrk(ivr).ne.0)    goto 160
               do 150 ilv = 1, levels(ivr)
                if(nodata(ifh,ivr,ilv).eq.1) goto 150
                    IF (count(ifh,ivr,ilv,iar,iob).gt.0.0) THEN
                      iend = 3
                      vdbhdr132(1:iend) = namversion(:3)

                      iend = iend + 1
                      vdbhdr132(iend:iend) = blank
                      istrt = iend + 1
                      iend = iend + 10
                      vdbhdr132(istrt:iend) = ensname

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
                      iend = iend + nchrstat(ist)
                      vdbhdr132(istrt:iend) =
     +                            namstat(ist)(:nchrstat(ist))
                    
                      istrt = iend + 1
                      iend = iend + numCNmodel 
                      vdbhdr132(istrt:iend) =  
     +                           CNmodel(:numCNmodel)
                      

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
     +                              count(ifh,ivr,ilv,iar,iob),       !count
     +                              sumgrid(ifh,ivr,ilv,iar,iob),     !mean [f]
     +                              sumdata(ifh,ivr,ilv,iar,iob),     !mean [o]
     +                              sumprod(ifh,ivr,ilv,iar,iob),     !mean [f*o]
     +                              ssqgrid(ifh,ivr,ilv,iar,iob),     !mean [f**2]
     +                              ssqdata(ifh,ivr,ilv,iar,iob),     !mean [o**2]
     +                            sumensvar(ifh,ivr,ilv,iar,iob),     !vairance
     +                            sumfrcle3(ifh,ivr,ilv,iar,iob)      !fraction

 1000                   FORMAT (A,F7.0,7(1x,E13.6))
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
           deallocate(sumensvar)
           deallocate(sumfrcle3)

           return
           end
