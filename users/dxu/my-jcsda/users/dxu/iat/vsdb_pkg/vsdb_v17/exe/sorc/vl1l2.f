      subroutine VL1L2(nvsdb,imodel,ist,numfcst,numvfyobs,numarea,
     +  numvarbl,numlevel,ngrid,levels,plevel,hasdata,
     +  ufcst,vfcst, uobsv,vobsv)

c    This program is to generate VL1L2 vsdb records for all fcst times,
c    all vector variables, all levels, all requested sub-regions, then write them
c    into the vsdb file
c
c March, 2005, Author: Binbin Zhou
c April, 2007, Changed weighting method for variances, Fanglin Yang
                                                                                                                 


      INCLUDE 'parm.inc'
      
      INTEGER, intent(IN) :: imodel, ist, numvfyobs,numarea,
     +         numfcst,numvarbl,numlevel,ngrid, levels(mxvrbl),
     +         hasdata(mxfcst,mxvrbl,maxlvl)

      REAL,dimension(numfcst,numvarbl,numlevel,ngrid),intent(IN) 
     +                                       ::ufcst,vfcst
      REAL,dimension(numfcst,numvarbl,numlevel,ngrid),intent(IN) 
     +                                       ::uobsv,vobsv

 
       real*8, allocatable, dimension(:,:,:,:,:) :: uf,vf,uo, vo,
     +       ufuo_vfvo,u2v2f,u2v2o,count,weigh
                                                                                                                                                        
      DIMENSION nchrmodel(maxmod), nchrfcst(mxfcst), nchrvfdate(mxdate),
     +            nchrvfyobs(maxobs), nchrarea(mxarea),
     +            nchrstat(mxstat), nchrvarbl(mxvrbl),
     +            nchrlevel(maxlvl)
      CHARACTER*24 namodel(maxmod), namfcst(mxfcst), namvfdate(mxdate),
     +            namvfyobs(maxobs), namarea(mxarea), namstat(mxstat),
     +            namvarbl(mxvrbl), namlevel(maxlvl)
                                                                                                                                                         
      COMMON /names/namodel, namfcst, namvfdate, namvfyobs, namarea,
     +            namstat, namvarbl, namlevel
      COMMON /nchrs/nchrmodel, nchrfcst, nchrvfdate, nchrvfyobs,
     +            nchrarea, nchrstat, nchrvarbl, nchrlevel
      CHARACTER*3 regions (100)
      COMMON /grdef/mode(mxarea), imax(mxarea), imin(mxarea),
     +            jmax(mxarea), jmin(mxarea), alat1(mxarea),
     +            elon1(mxarea), dxx(mxarea), dyy(mxarea),
     +            elonv(mxarea), alatan(mxarea), latlong(mxarea),
     +            lambert(mxarea), polarstereo(mxarea)
      COMMON /grdef1/numreg(mxarea), ig104(147,110),regions


c feel strange for following
c     +       elonv(mxarea), alatan(mxarea),
c     +       numreg(mxarea), ig104(147,110),regions,
c     +       latlong(mxarea),lambert(mxarea),
c     +       polarstereo(mxarea)


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
      CHARACTER*24 afho(mxvrbl),  afhothr(mxvrbl,20)
      integer  nchrfho(mxvrbl),nchrfhothr(mxvrbl,20),fhomrk(mxvrbl)
      integer  nchrafho(mxvrbl),nchrafhothr(mxvrbl,20),afhomrk(mxvrbl)
      real rfhothr(mxvrbl,20)
      real rafhothr(mxvrbl,20)
      integer  continue_mrk(mxvrbl)
      integer anomlylev(mxvrbl,maxlvl),anomly_mrk(mxvrbl)

      COMMON /g2g/cyyyyfcst,cmmfcst,cddfcst,chhfcst,cfffcst,
     +            cyyyyobsv,cmmobsv,cddobsv,chhobsv,cffobsv,
     +             yyyyfcst, mmfcst, ddfcst, hhfcst, fffcst,
     +             yyyyobsv, mmobsv, ddobsv, hhobsv, ffobsv,
     +             k5,k6,k7,ck7,vectormrk,namlvl,nchrlvl,
     +      fhomrk,fho,nchrfho,fhothr,nchrfhothr,rfhothr,
     +             continue_mrk,anomly_mrk, anomlylev,
     +      afhomrk,afho,nchrafho,afhothr,nchrafhothr,rafhothr

      integer plevel(maxlvl)
      integer region_id(maxpts)
      real region_latlon(2,maxpts), ptr1(2,mxarea),ptr2(2,mxarea)
 
      COMMON /reg/region_id, region_latlon, ptr1,ptr2
      COMMON /layer/ modelvl(maxlvl), iplevel(maxlvl,2)

      real area_factor(maxpts)
      COMMON /weight/area_factor


      DATA blank /' '/
      DATA equal /'='/
      DATA namversion /'V01'/
      DATA bmiss /10E10/
      DATA rmiss /99999./

      write(*,*) 'In VL1L2:',imodel,ist,numfcst,numvfyobs,numarea,
     +  numvarbl,numlevel,ngrid
      write(*,*) 'vectormrk', (vectormrk(ivr),ivr=1,numvarbl)
      write(*,*) 'fhomrk', (fhomrk(ivr),ivr=1,numvarbl)
      write(*,*) 'afhomrk', (afhomrk(ivr),ivr=1,numvarbl)
      write(*,*) 'rafhothr=', (rafhothr(6,i),i=1,5)     
      write(*,*) 'nchrafho=', (nchrafho(ivr),ivr=1,numvarbl)  
      write(*,*) 'anomly_mrk=',(anomly_mrk(ivr),ivr=1,numvarbl)   

      write(*,*) 'mode(iar)=',mode
      write(*,*) 'numreg(iar)=',numreg

       allocate(uf(numfcst,numvarbl,numlevel,numarea,numvfyobs))
       allocate(vf(numfcst,numvarbl,numlevel,numarea,numvfyobs))
       allocate(uo(numfcst,numvarbl,numlevel,numarea,numvfyobs))
       allocate(vo(numfcst,numvarbl,numlevel,numarea,numvfyobs))
       allocate(ufuo_vfvo(numfcst,numvarbl,numlevel,numarea,numvfyobs))
       allocate(u2v2f(numfcst,numvarbl,numlevel,numarea,numvfyobs))
       allocate(u2v2o(numfcst,numvarbl,numlevel,numarea,numvfyobs))
       allocate(count(numfcst,numvarbl,numlevel,numarea,numvfyobs))
       allocate(weigh(numfcst,numvarbl,numlevel,numarea,numvfyobs))

c      do nfcst=1,numfcst
c       do nvar=1,numvarbl
c        write(*,*) 'nfcst,nvar=',nfcst
c        if(vectormrk(nvar).eq.1) then
c        do np =1,levels(nvar)
c         write(*,'(i3,10f10.2)')np, (ufcst(nfcst,nvar,np,i),i=1,10) 
c         write(*,'(i3,10f10.2)')np, (vfcst(nfcst,nvar,np,i),i=1,10) 
c        end do
c        end if
c       end do
c      end do


          count = 0.0
          weigh = 0.0
          uf = 0
          vf = 0
          uo = 0
          vo = 0
          ufuo_vfvo = 0
          u2v2f = 0
          u2v2o = 0


      do 90 ifh = 1, numfcst
        do 80 iob = 1, numvfyobs
          do 70 iar = 1, numarea
           do 60 ivr = 1, numvarbl

             if(vectormrk(ivr).eq.0) goto 60                     !scaler in SL1L2
             if(fhomrk(ivr).ne.0)    goto 60                     !fho in FHO
             if(afhomrk(ivr).ne.0)   goto 60                     !afho in AFHO
             do 50 ilv = 1, levels(ivr)
              if(nodata(ifh,ivr,ilv).eq.1) goto 50
     
                if (mode(iar).eq.1) then                          !all GRID domain (mode 1)
                  do i = 1,ngrid
                       ufc=ufcst(ifh,ivr,ilv,i)
                       vfc=vfcst(ifh,ivr,ilv,i)
                       uob=uobsv(ifh,ivr,ilv,i)
                       vob=vobsv(ifh,ivr,ilv,i)

                       uf(ifh,ivr,ilv,iar,iob) = 
     +                  uf(ifh,ivr,ilv,iar,iob) + ufc*area_factor(i)
    
                       vf(ifh,ivr,ilv,iar,iob) =
     +                  vf(ifh,ivr,ilv,iar,iob) + vfc*area_factor(i)

                       uo(ifh,ivr,ilv,iar,iob) =
     +                  uo(ifh,ivr,ilv,iar,iob) + uob*area_factor(i)
                                                                                                                                                           
                       vo(ifh,ivr,ilv,iar,iob) =
     +                  vo(ifh,ivr,ilv,iar,iob) + vob*area_factor(i)

                       ufuo_vfvo(ifh,ivr,ilv,iar,iob) =
     +                  ufuo_vfvo(ifh,ivr,ilv,iar,iob) 
     +                  + (ufc*uob+vfc*vob)*area_factor(i)

                       u2v2f(ifh,ivr,ilv,iar,iob) =
     +                  u2v2f(ifh,ivr,ilv,iar,iob) 
     +                  + (ufc*ufc + vfc*vfc)*area_factor(i)

                       u2v2o(ifh,ivr,ilv,iar,iob) =
     +                  u2v2o(ifh,ivr,ilv,iar,iob)
     +                  + (uob*uob+vob*vob)*area_factor(i)                                        

                       count(ifh,ivr,ilv,iar,iob) =
     +                 count(ifh,ivr,ilv,iar,iob) + 1.0
                       weigh(ifh,ivr,ilv,iar,iob) =
     +                 weigh(ifh,ivr,ilv,iar,iob) + area_factor(i)

                  end do
                else if (mode(iar).eq.2) then                    ! GRID#104  (mode 2)
                 if (numreg(iar).le.30) then                    !         sub-region
                   do i = 1,ngrid
                    if(region_id(i).eq.numreg(iar)) then

                       ufc=ufcst(ifh,ivr,ilv,i)
                       vfc=vfcst(ifh,ivr,ilv,i)
                       uob=uobsv(ifh,ivr,ilv,i)
                       vob=vobsv(ifh,ivr,ilv,i)

                       uf(ifh,ivr,ilv,iar,iob) =
     +                  uf(ifh,ivr,ilv,iar,iob) + ufc*area_factor(i)
                                                                                                               
                       vf(ifh,ivr,ilv,iar,iob) =
     +                  vf(ifh,ivr,ilv,iar,iob) + vfc*area_factor(i)
                                                                                                               
                       uo(ifh,ivr,ilv,iar,iob) =
     +                  uo(ifh,ivr,ilv,iar,iob) + uob*area_factor(i)
                                                                                                               
                                                                                                               
                       vo(ifh,ivr,ilv,iar,iob) =
     +                  vo(ifh,ivr,ilv,iar,iob) + vob*area_factor(i)
                                                                                                               
                       ufuo_vfvo(ifh,ivr,ilv,iar,iob) =
     +                  ufuo_vfvo(ifh,ivr,ilv,iar,iob)
     +                  + (ufc*uob+vfc*vob)*area_factor(i)
                                                                                                               
                       u2v2f(ifh,ivr,ilv,iar,iob) =
     +                  u2v2f(ifh,ivr,ilv,iar,iob)
     +                  + (ufc*ufc + vfc*vfc)*area_factor(i)
                                                                                                               
                       u2v2o(ifh,ivr,ilv,iar,iob) =
     +                  u2v2o(ifh,ivr,ilv,iar,iob)
     +                  + (uob*uob+vob*vob)*area_factor(i)
                                                                                                               
                       count(ifh,ivr,ilv,iar,iob) =
     +                 count(ifh,ivr,ilv,iar,iob) + 1.0
                       weigh(ifh,ivr,ilv,iar,iob) =
     +                 weigh(ifh,ivr,ilv,iar,iob) + area_factor(i)

                      end if
                     end do

                  else if(numreg(iar).eq.31 ) then             !        N. hemisphere
 
                     do i = 1,ngrid
                      if(region_latlon(1,i).gt.0.0) then

                       ufc=ufcst(ifh,ivr,ilv,i)
                       vfc=vfcst(ifh,ivr,ilv,i)
                       uob=uobsv(ifh,ivr,ilv,i)
                       vob=vobsv(ifh,ivr,ilv,i)

                       uf(ifh,ivr,ilv,iar,iob) =
     +                  uf(ifh,ivr,ilv,iar,iob) + ufc*area_factor(i)
                                                                                                                                                           
                       vf(ifh,ivr,ilv,iar,iob) =
     +                  vf(ifh,ivr,ilv,iar,iob) + vfc*area_factor(i)
                                                                                                                                                           
                       uo(ifh,ivr,ilv,iar,iob) =
     +                  uo(ifh,ivr,ilv,iar,iob) + uob*area_factor(i)
                                                                                                                                                           
                       vo(ifh,ivr,ilv,iar,iob) =
     +                  vo(ifh,ivr,ilv,iar,iob) + vob*area_factor(i)
                                                                                                                                                           
                       ufuo_vfvo(ifh,ivr,ilv,iar,iob) =
     +                  ufuo_vfvo(ifh,ivr,ilv,iar,iob)
     +                  + (ufc*uob+vfc*vob)*area_factor(i)
                                                                                                                                                           
                       u2v2f(ifh,ivr,ilv,iar,iob) =
     +                  u2v2f(ifh,ivr,ilv,iar,iob)
     +                  + (ufc*ufc + vfc*vfc)*area_factor(i)
                                                                                                                                                           
                       u2v2o(ifh,ivr,ilv,iar,iob) =
     +                  u2v2o(ifh,ivr,ilv,iar,iob)
     +                  + (uob*uob+vob*vob)*area_factor(i)
                                                                                                                                                           
                       count(ifh,ivr,ilv,iar,iob) =
     +                 count(ifh,ivr,ilv,iar,iob) + 1.0
                       weigh(ifh,ivr,ilv,iar,iob) =
     +                 weigh(ifh,ivr,ilv,iar,iob) + area_factor(i)

                      end if
                     end do

                   else if(numreg(iar).eq.32 ) then             !        S. hemisphere
                     do i = 1,ngrid
                      if(region_latlon(1,i).gt.0.0) then

                       ufc=ufcst(ifh,ivr,ilv,i)
                       vfc=vfcst(ifh,ivr,ilv,i)
                       uob=uobsv(ifh,ivr,ilv,i)
                       vob=vobsv(ifh,ivr,ilv,i)

                       uf(ifh,ivr,ilv,iar,iob) =
     +                  uf(ifh,ivr,ilv,iar,iob) + ufc*area_factor(i)

                       vf(ifh,ivr,ilv,iar,iob) =
     +                  vf(ifh,ivr,ilv,iar,iob) + vfc*area_factor(i)

                       uo(ifh,ivr,ilv,iar,iob) =
     +                  uo(ifh,ivr,ilv,iar,iob) + uob*area_factor(i)

                       vo(ifh,ivr,ilv,iar,iob) =
     +                  vo(ifh,ivr,ilv,iar,iob) + vob*area_factor(i)

                       ufuo_vfvo(ifh,ivr,ilv,iar,iob) =
     +                  ufuo_vfvo(ifh,ivr,ilv,iar,iob)
     +                  + (ufc*uob+vfc*vob)*area_factor(i)

                       u2v2f(ifh,ivr,ilv,iar,iob) =
     +                  u2v2f(ifh,ivr,ilv,iar,iob)
     +                  + (ufc*ufc + vfc*vfc)*area_factor(i)

                       u2v2o(ifh,ivr,ilv,iar,iob) =
     +                  u2v2o(ifh,ivr,ilv,iar,iob)
     +                  + (uob*uob+vob*vob)*area_factor(i)

                       count(ifh,ivr,ilv,iar,iob) =
     +                 count(ifh,ivr,ilv,iar,iob) + 1.0
                       weigh(ifh,ivr,ilv,iar,iob) =
     +                 weigh(ifh,ivr,ilv,iar,iob) + area_factor(i)

                      end if
                     end do

                  else if(numreg(iar).eq.33) then             ! User defined region
                   if (ptr1(1,iar).eq.ptr2(1,iar).and.         !user defined case 1, all points along latitude
     +                 ptr1(2,iar).ne.ptr2(2,iar)) then               
                    do i = 1,ngrid                                    !  x(ptr2(1), ptr2(2))
                     if(region_latlon(1,i).ge.ptr1(2,iar).and.        !  |
     +                     region_latlon(1,i).le.ptr2(2,iar)) then    !  |
                                                                      !  x(ptr1(1), ptr1(2))
                       ufc=ufcst(ifh,ivr,ilv,i)
                       vfc=vfcst(ifh,ivr,ilv,i)
                       uob=uobsv(ifh,ivr,ilv,i)
                       vob=vobsv(ifh,ivr,ilv,i)

                       uf(ifh,ivr,ilv,iar,iob) =
     +                  uf(ifh,ivr,ilv,iar,iob) + ufc*area_factor(i)

                       vf(ifh,ivr,ilv,iar,iob) =
     +                  vf(ifh,ivr,ilv,iar,iob) + vfc*area_factor(i)

                       uo(ifh,ivr,ilv,iar,iob) =
     +                  uo(ifh,ivr,ilv,iar,iob) + uob*area_factor(i)

                       vo(ifh,ivr,ilv,iar,iob) =
     +                  vo(ifh,ivr,ilv,iar,iob) + vob*area_factor(i)

                       ufuo_vfvo(ifh,ivr,ilv,iar,iob) =
     +                  ufuo_vfvo(ifh,ivr,ilv,iar,iob)
     +                  + (ufc*uob+vfc*vob)*area_factor(i)

                       u2v2f(ifh,ivr,ilv,iar,iob) =
     +                  u2v2f(ifh,ivr,ilv,iar,iob)
     +                  + (ufc*ufc + vfc*vfc)*area_factor(i)

                       u2v2o(ifh,ivr,ilv,iar,iob) =
     +                  u2v2o(ifh,ivr,ilv,iar,iob)
     +                  + (uob*uob+vob*vob)*area_factor(i)

                       count(ifh,ivr,ilv,iar,iob) =
     +                 count(ifh,ivr,ilv,iar,iob) + 1.0
                       weigh(ifh,ivr,ilv,iar,iob) =
     +                 weigh(ifh,ivr,ilv,iar,iob) + area_factor(i)

                      end if
                     end do
                   else if (ptr1(1,iar).ne.ptr2(1,iar).and.    !user defined case 2, all points along longitude
     +                     ptr1(2,iar).eq.ptr2(2,iar)) then
                    do i = 1,ngrid                                    !
                     if(region_latlon(2,i).ge.ptr1(1,iar).and.        !       x -----------------x
     +                     region_latlon(2,i).le.ptr2(1,iar)) then 

                       ufc=ufcst(ifh,ivr,ilv,i)
                       vfc=vfcst(ifh,ivr,ilv,i)
                       uob=uobsv(ifh,ivr,ilv,i)
                       vob=vobsv(ifh,ivr,ilv,i)

                       uf(ifh,ivr,ilv,iar,iob) =
     +                  uf(ifh,ivr,ilv,iar,iob) + ufc*area_factor(i)

                       vf(ifh,ivr,ilv,iar,iob) =
     +                  vf(ifh,ivr,ilv,iar,iob) + vfc*area_factor(i)

                       uo(ifh,ivr,ilv,iar,iob) =
     +                  uo(ifh,ivr,ilv,iar,iob) + uob*area_factor(i)

                       vo(ifh,ivr,ilv,iar,iob) =
     +                  vo(ifh,ivr,ilv,iar,iob) + vob*area_factor(i)

                       ufuo_vfvo(ifh,ivr,ilv,iar,iob) =
     +                  ufuo_vfvo(ifh,ivr,ilv,iar,iob)
     +                  + (ufc*uob+vfc*vob)*area_factor(i)

                       u2v2f(ifh,ivr,ilv,iar,iob) =
     +                  u2v2f(ifh,ivr,ilv,iar,iob)
     +                  + (ufc*ufc + vfc*vfc)*area_factor(i)

                       u2v2o(ifh,ivr,ilv,iar,iob) =
     +                  u2v2o(ifh,ivr,ilv,iar,iob)
     +                  + (uob*uob+vob*vob)*area_factor(i)

                       count(ifh,ivr,ilv,iar,iob) =
     +                 count(ifh,ivr,ilv,iar,iob) + 1.0
                       weigh(ifh,ivr,ilv,iar,iob) =
     +                 weigh(ifh,ivr,ilv,iar,iob) + area_factor(i)

                      end if
                     end do
                   else if(ptr1(1,iar).ne.ptr2(1,iar).and.    !user defined case 3, all points in a regtangular
     +                     ptr1(2,iar).ne.ptr2(2,iar)) then      !
                    do i = 1,ngrid                               !                     (ptr2(1), ptr2(2))
                     if(region_latlon(2,i).ge.ptr1(1,iar).and.   !              ----------------x
     +                  region_latlon(2,i).le.ptr2(1,iar).and.   !             |                |
     +                  region_latlon(1,i).ge.ptr1(2,iar).and.   !             |                |
     +                  region_latlon(1,i).le.ptr2(2,iar)) then  !             |                |
                                                                 !             x----------------
                                                                 !     (ptr1(1), ptr1(2))
                       ufc=ufcst(ifh,ivr,ilv,i)
                       vfc=vfcst(ifh,ivr,ilv,i)
                       uob=uobsv(ifh,ivr,ilv,i)
                       vob=vobsv(ifh,ivr,ilv,i)

                       uf(ifh,ivr,ilv,iar,iob) =
     +                  uf(ifh,ivr,ilv,iar,iob) + ufc*area_factor(i)

                       vf(ifh,ivr,ilv,iar,iob) =
     +                  vf(ifh,ivr,ilv,iar,iob) + vfc*area_factor(i)

                       uo(ifh,ivr,ilv,iar,iob) =
     +                  uo(ifh,ivr,ilv,iar,iob) + uob*area_factor(i)

                       vo(ifh,ivr,ilv,iar,iob) =
     +                  vo(ifh,ivr,ilv,iar,iob) + vob*area_factor(i)

                       ufuo_vfvo(ifh,ivr,ilv,iar,iob) =
     +                  ufuo_vfvo(ifh,ivr,ilv,iar,iob)
     +                  + (ufc*uob+vfc*vob)*area_factor(i)

                       u2v2f(ifh,ivr,ilv,iar,iob) =
     +                  u2v2f(ifh,ivr,ilv,iar,iob)
     +                  + (ufc*ufc + vfc*vfc)*area_factor(i)

                       u2v2o(ifh,ivr,ilv,iar,iob) =
     +                  u2v2o(ifh,ivr,ilv,iar,iob)
     +                  + (uob*uob+vob*vob)*area_factor(i)

                       count(ifh,ivr,ilv,iar,iob) =
     +                 count(ifh,ivr,ilv,iar,iob) + 1.0
                       weigh(ifh,ivr,ilv,iar,iob) =
     +                 weigh(ifh,ivr,ilv,iar,iob) + area_factor(i)

                      end if
                     end do
                    end if

                   end if   !end of mode=2 (# 104sub-region)
 
                  end if    !end of region mode
 
50               continue
60             continue
70            continue
80           continue
90         continue

      do 91 ifh = 1, numfcst
       do 81 iob = 1, numvfyobs
        do 71 iar = 1, numarea
         do 61 ivr = 1, numvarbl
          if(vectormrk(ivr).eq.0) goto 61
          if(fhomrk(ivr).ne.0)    goto 61
          if(afhomrk(ivr).ne.0)   goto 61
          do 51 ilv = 1, levels(ivr)
           if(nodata(ifh,ivr,ilv).eq.1) goto 51

           uf(ifh,ivr,ilv,iar,iob) =
     +     uf(ifh,ivr,ilv,iar,iob)/weigh(ifh,ivr,ilv,iar,iob)
                                                                                                                                                           
           vf(ifh,ivr,ilv,iar,iob) =
     +     vf(ifh,ivr,ilv,iar,iob)/weigh(ifh,ivr,ilv,iar,iob)
                                                                                                                                                           
           uo(ifh,ivr,ilv,iar,iob) =
     +     uo(ifh,ivr,ilv,iar,iob)/weigh(ifh,ivr,ilv,iar,iob)
                                                                                                                                                           
           vo(ifh,ivr,ilv,iar,iob) =
     +     vo(ifh,ivr,ilv,iar,iob)/weigh(ifh,ivr,ilv,iar,iob)
                                                                                                                                                           
           ufuo_vfvo(ifh,ivr,ilv,iar,iob) =
     +     ufuo_vfvo(ifh,ivr,ilv,iar,iob)/
     +             weigh(ifh,ivr,ilv,iar,iob)
                                                                                                                                                           
           u2v2f(ifh,ivr,ilv,iar,iob) =
     +     u2v2f(ifh,ivr,ilv,iar,iob)/
     +             weigh(ifh,ivr,ilv,iar,iob)
                                                                                                                                                           
           u2v2o(ifh,ivr,ilv,iar,iob) =
     +     u2v2o(ifh,ivr,ilv,iar,iob)/
     +            weigh(ifh,ivr,ilv,iar,iob)

           if(uf(ifh,ivr,ilv,iar,iob).lt. -99.0 .or.
     +        vf(ifh,ivr,ilv,iar,iob).lt. -99.0 .or.
     +        uo(ifh,ivr,ilv,iar,iob).lt. -99.0 .or.
     +        vo(ifh,ivr,ilv,iar,iob).lt. -99.0) then
              count(ifh,ivr,ilv,iar,iob) = 0.0
           end if

51         continue
61        continue
71       continue
81      continue
91     continue

           do 190 iob = 1, numvfyobs
            do 180 iar = 1, numarea
             do 170 ifh = 1, numfcst
                ivfdate = ifh
              do 160 ivr = 1, numvarbl
               if(vectormrk(ivr).eq.0) goto 160
               if(fhomrk(ivr).ne.0)    goto 160
               if(afhomrk(ivr).ne.0)    goto 160
               do 150 ilv = 1, levels(ivr)
                if(nodata(ifh,ivr,ilv).eq.1) goto 150
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
     +                              uf(ifh,ivr,ilv,iar,iob),
     +                              vf(ifh,ivr,ilv,iar,iob),
     +                              uo(ifh,ivr,ilv,iar,iob),
     +                              vo(ifh,ivr,ilv,iar,iob),
     +                              ufuo_vfvo(ifh,ivr,ilv,iar,iob),
     +                              u2v2f(ifh,ivr,ilv,iar,iob),
     +                              u2v2o(ifh,ivr,ilv,iar,iob)
 1000                   FORMAT (A,F7.0,7E18.9)
                       end if
                    END IF
150            continue
160           continue
170          continue
180         continue
190        continue

           deallocate(uf)
           deallocate(vf)
           deallocate(uo)
           deallocate(vo)
           deallocate(ufuo_vfvo)
           deallocate(u2v2f)
           deallocate(u2v2o)
           deallocate(count)
           deallocate(weigh)

           return
           end
