       parameter(nlevs=21)
       CHARACTER*1  kdbug
       CHARACTER*2  kincr
       CHARACTER*250 iname
       CHARACTER*250 inputdir,outfile
       CHARACTER*10 ksyr,ksmth,ksday,kscy
       CHARACTER*10 keyr,kemth,keday,kecy
c
       dimension pmand(nlevs)
       dimension levp(0:4000)
c
       DATA PMAND / 1000, 925, 850, 700, 500, 400, 300,
     .               250, 200, 150, 100,  70,  50,  30,
     .                20,  10,   7,   5,   3,   2,   1/
c
       call getenv("idbug",kdbug)
       read(kdbug,'(i1)') idbug
       write(*,*) "idbug= ",idbug
c
       call getenv("incr",kincr)
       read(kincr,'(i2)') incr
       write(*,*) "incr= ",incr
c
       call getenv("inputdir",inputdir)
       write(*,*) "inputdir= ",inputdir
c
       call getenv("outfile",outfile)
       write(*,*) "outfile= ",outfile
c
       call getenv("iname",iname)
       write(*,*) "iname= ",iname
C
       call getenv("syear",ksyr)
       read(ksyr,'(i4)') isyr
       write(*,*) "syear= ",isyr
C
       call getenv("smonth",ksmth)
       read(ksmth,'(i2)') ismth
       write(*,*) "smonth= ",ismth
C
       call getenv("sday",ksday)
       read(ksday,'(i2)') isday
       write(*,*) "sday= ",isday
C
       call getenv("shour",kscy)
       read(kscy,'(i2)') iscy
       write(*,*) "shour= ",iscy
C
       call getenv("eyear",keyr)
       read(keyr,'(i4)') ieyr
       write(*,*) "eyear= ",ieyr
C
       call getenv("emonth",kemth)
       read(kemth,'(i2)') iemth
       write(*,*) "emonth= ",iemth
C
       call getenv("eday",keday)
       read(keday,'(i2)') ieday
       write(*,*) "eday= ",ieday
C
       call getenv("ehour",kecy)
       read(kecy,'(i2)') iecy
       write(*,*) "ehour= ",iecy
c
       DO I=0,4000
       LEVP(I) = 0
       ENDDO
c
       DO I=0,4000
       DO L=1,nlevs
       IF(FLOAT(I).EQ.PMAND(L)) LEVP(I) = L
       ENDDO
       ENDDO
c
c      do i=0,4000
c      print *,i,levp(i)
c      enddo
c
       call stats(inputdir,iname,outfile,idbug,incr,
     * isyr,ismth,isday,iscy,ieyr,iemth,ieday,iecy,levp,nlevs)
c
       stop
       end
       subroutine stats(inputdir,iname,outfile,idbug,incr,
     * isyr,ismth,isday,iscy,ieyr,iemth,ieday,iecy,levp,nlevs)
c
       parameter(nst=40000)
       character*4  cycl(5),cyclx
       CHARACTER*10 indate,sdate,edate
       CHARACTER*4  LABY
       CHARACTER*2  LABM(12)
       CHARACTER*2  LABD(31)
       CHARACTER*2  LABC(4)
C
       character*8 tstn(nst),id,idn
       character*1 id6,idb
       CHARACTER*250 iname,inputdir
       CHARACTER*250 infile,outfile
c
       CHARACTER*8  idpr1,idpr2
C
       DIMENSION PLEV(nlevs)
       DIMENSION tlon(nst),tlat(nst),typ(nst)
C
       dimension po(nlevs,nst)
       dimension pso(nst),psa(nst),psf(nst)
       dimension psos(nst),psas(nst),psfs(nst)
       dimension psao(nst),psfo(nst)
       dimension psae(nst),psfe(nst)
       dimension zo(nlevs,nst),za(nlevs,nst),zf(nlevs,nst)
       dimension zos(nlevs,nst),zas(nlevs,nst),zfs(nlevs,nst)
       dimension zao(nlevs,nst),zfo(nlevs,nst)
       dimension zae(nlevs,nst),zfe(nlevs,nst)
       dimension to(nlevs,nst),ta(nlevs,nst),tf(nlevs,nst)
       dimension tos(nlevs,nst),tas(nlevs,nst),tfs(nlevs,nst)
       dimension tao(nlevs,nst),tfo(nlevs,nst)
       dimension tae(nlevs,nst),tfe(nlevs,nst)
       dimension qo(nlevs,nst),qa(nlevs,nst),qf(nlevs,nst)
       dimension qos(nlevs,nst),qas(nlevs,nst),qfs(nlevs,nst)
       dimension qao(nlevs,nst),qfo(nlevs,nst)
       dimension qae(nlevs,nst),qfe(nlevs,nst)
       dimension uo(nlevs,nst),ua(nlevs,nst),uf(nlevs,nst)
       dimension vo(nlevs,nst),va(nlevs,nst),vf(nlevs,nst)
       dimension uvo(nlevs,nst),uva(nlevs,nst),uvf(nlevs,nst)
       dimension uvao(nlevs,nst),uvfo(nlevs,nst)
       dimension spda(nlevs,nst),spdf(nlevs,nst)
       dimension wae(nlevs,nst),wfe(nlevs,nst)
c
       dimension fps(nst)
       dimension fz(nlevs,nst)
       dimension ft(nlevs,nst)
       dimension fq(nlevs,nst)
       dimension fw(nlevs,nst)
       integer icycl(4)
c
       dimension pog(nlevs)
       dimension zog(nlevs),zag(nlevs),zfg(nlevs),zqg(nlevs)
       dimension zcg(nlevs),zaeg(nlevs),zfeg(nlevs)
       dimension tog(nlevs),tag(nlevs),tfg(nlevs),tqg(nlevs)
       dimension tcg(nlevs),taeg(nlevs),tfeg(nlevs)
       dimension qog(nlevs),qag(nlevs),qfg(nlevs),qqg(nlevs)
       dimension qcg(nlevs),qaeg(nlevs),qfeg(nlevs)
       dimension uog(nlevs),uag(nlevs),ufg(nlevs),wqg(nlevs)
       dimension vog(nlevs),vag(nlevs),vfg(nlevs)
       dimension wcg(nlevs),waeg(nlevs),wfeg(nlevs)
       dimension spdag(nlevs),spdfg(nlevs)
c
       dimension levp(0:4000)
c
c      DATA idpr1/'74794   '/
c      DATA idpr2/'747941  '/
       DATA idpr1/'746412  '/
       DATA idpr2/'74641   '/
c
       DATA BMISS /10E10/
c
       DATA LABC/'00','06','12','18'/
       DATA ICYCL/0,6,12,18/
       data cycl/'.00Z','.06Z','.12Z','.18Z','    '/
C
       DATA LABD/'01','02','03','04','05','06','07','08','09','10',
     *           '11','12','13','14','15','16','17','18','19','20',
     *           '21','22','23','24','25','26','27','28','29','30',
     *           '31'/
C
       DATA LABM/'01','02','03','04','05','06','07','08','09','10',
     *           '11','12'/
C
       indate='          '
       sdate='          '
       edate='          '
c
       write(laby,'(i4)') isyr
       sdate(1:4)=laby
       sdate(5:6)=labm(ismth)
       sdate(7:8)=labd(isday)
       if(iscy.eq.0)  sdate(9:10)=labc(1)
       if(iscy.eq.6)  sdate(9:10)=labc(2)
       if(iscy.eq.12) sdate(9:10)=labc(3)
       if(iscy.eq.18) sdate(9:10)=labc(4)
       print *,' sdate ',sdate
c
       write(laby,'(i4)') ieyr
       edate(1:4)=laby
       edate(5:6)=labm(iemth)
       edate(7:8)=labd(ieday)
       if(iecy.eq.0)  edate(9:10)=labc(1)
       if(iecy.eq.6)  edate(9:10)=labc(2)
       if(iecy.eq.12) edate(9:10)=labc(3)
       if(iecy.eq.18) edate(9:10)=labc(4)
       print *,' edate ',edate
c
       ncns=iw3jdn(isyr,ismth,isday)
       print *,' ncns ',ncns
       ncne=iw3jdn(ieyr,iemth,ieday)
       print *,' ncne ',ncne
       ndays=ncne-ncns+1
       print *,' ndays ',ndays
c
       do ist=1,nst
       fps(ist)=0.
       pso(ist)=bmiss
       psa(ist)=bmiss
       psf(ist)=bmiss
       psos(ist)=bmiss
       psas(ist)=bmiss
       psfs(ist)=bmiss
       psao(ist)=bmiss
       psfo(ist)=bmiss
       psae(ist)=bmiss
       psfe(ist)=bmiss
       enddo
c
       do ist=1,nst
       do k=1,nlevs
c
       fz(k,ist)=0.
       ft(k,ist)=0.
       fq(k,ist)=0.
       fw(k,ist)=0.
c
       po(k,ist)=bmiss
       zo(k,ist)=bmiss
       za(k,ist)=bmiss
       zf(k,ist)=bmiss
       zos(k,ist)=bmiss
       zas(k,ist)=bmiss
       zfs(k,ist)=bmiss
       zao(k,ist)=bmiss
       zfo(k,ist)=bmiss
       zae(k,ist)=bmiss
       zfe(k,ist)=bmiss
       to(k,ist)=bmiss
       ta(k,ist)=bmiss
       tf(k,ist)=bmiss
       tos(k,ist)=bmiss
       tas(k,ist)=bmiss
       tfs(k,ist)=bmiss
       tao(k,ist)=bmiss
       tfo(k,ist)=bmiss
       tae(k,ist)=bmiss
       tfe(k,ist)=bmiss
       qo(k,ist)=bmiss
       qa(k,ist)=bmiss
       qf(k,ist)=bmiss
       qos(k,ist)=bmiss
       qas(k,ist)=bmiss
       qfs(k,ist)=bmiss
       qao(k,ist)=bmiss
       qfo(k,ist)=bmiss
       qae(k,ist)=bmiss
       qfe(k,ist)=bmiss
       uo(k,ist)=bmiss
       ua(k,ist)=bmiss
       uf(k,ist)=bmiss
       vo(k,ist)=bmiss
       va(k,ist)=bmiss
       vf(k,ist)=bmiss
       uvo(k,ist)=bmiss
       uva(k,ist)=bmiss
       uvf(k,ist)=bmiss
       uvao(k,ist)=bmiss
       uvfo(k,ist)=bmiss
       spda(k,ist)=bmiss
       spdf(k,ist)=bmiss
       wae(k,ist)=bmiss
       wfe(k,ist)=bmiss
c
       enddo
       enddo
c
       if(iscy.eq.0) ncysx=1
       if(iscy.eq.6) ncysx=2
       if(iscy.eq.12) ncysx=3
       if(iscy.eq.18) ncysx=4
c
       if(iecy.eq.0) ncyex=1
       if(iecy.eq.6) ncyex=2
       if(iecy.eq.12) ncyex=3
       if(iecy.eq.18) ncyex=4
c
C      START THE TIME LOOP HERE...
C
       ntmx=0
       ist=0
       irka=0
       DO 555 NCN=NCNS,NCNE
C
       CALL W3FS26(NCN,IYR,IMTH,IDAY,IDAYWK,IDAYYR)
       WRITE(LABY,'(I4)') IYR
       indate(1:4)=LABY
       indate(5:6)=LABM(IMTH)
       indate(7:8)=LABD(IDAY)
C
c.... once daily mean....
       if(incr.eq.24) then
       ncys=ncysx
       ncye=ncysx
       incrx=1
       endif
c.... 4 times daily mean....
       if(incr.eq.6) then
       ncys=1
       ncye=4
       incrx=1
       if(ncn.eq.ncns) ncys=ncysx
       if(ncn.eq.ncne) ncye=ncyex
       endif
c.... twice-daily mean....
       if(incr.eq.12) then
       ncys=ncysx
       ncye=ncyex
       incrx=2
       endif
c
       DO 444 ncy=ncys,ncye,incrx
       ICY=icycl(ncy)
C
       IF(ICY.EQ.0)  indate(9:10)=LABC(1)
       IF(ICY.EQ.6)  indate(9:10)=LABC(2)
       IF(ICY.EQ.12) indate(9:10)=LABC(3)
       IF(ICY.EQ.18) indate(9:10)=LABC(4)
C
       infile = inputdir(1:nfill(inputdir)) // '/' // 
     * iname(1:nfill(iname)) // '.' // indate
c
       write(*,*) " infile= ",infile
       write(*,*) "ist ",ist,"  ncy=",ncy, "  NCN=",NCN
!!     open(11,file=infile,form='unformatted',iostat=ierr)
       call opendian(11,infile,ierr)
       if(ierr.ne.0) go to 101
c   update time level ntmx only if you can actually read the file. Then rewind it.
       read(11,end=101) id,rlat,rlon,rt,nl,iflag
       ntmx=ntmx+1
       write(*,*) " ntmx= ",ntmx
       rewind 11
c
c....  read grads data in...
c
100    continue
c
       psog=bmiss
       psag=bmiss
       psfg=bmiss
       psqg=bmiss
c
       do l=1,nlevs
       pog(l)=bmiss
       zog(l)=bmiss
       zag(l)=bmiss
       zfg(l)=bmiss
       zqg(l)=bmiss
       tog(l)=bmiss
       tag(l)=bmiss
       tfg(l)=bmiss
       tqg(l)=bmiss
       qog(l)=bmiss
       qag(l)=bmiss
       qfg(l)=bmiss
       qqg(l)=bmiss
       uog(l)=bmiss
       uag(l)=bmiss
       ufg(l)=bmiss
       vog(l)=bmiss
       vag(l)=bmiss
       vfg(l)=bmiss
       wqg(l)=bmiss
       enddo
c
       read(11,end=101) id,rlat,rlon,rt,nl,iflag
c
       read(indate,'(i10)') idate
c      if(idate.le.1999061700) then
c      read(11,end=101) (pog(l),
c    *            typx,elvx,psog,psag,psfg,psqg,pog(l),
c    *            zog(l),zag(l),zfg(l),zqg(l),
c    *            tog(l),tag(l),tfg(l),tqg(l),
c    *            qog(l),qag(l),qfg(l),qqg(l),
c    *            uog(l),uag(l),ufg(l),wqg(l),
c    *            vog(l),vag(l),vfg(l),l=1,nl)
c      else
       read(11,end=101) (pog(l),
     *            typx,typint,elvx,psog,psag,psfg,psqg,pog(l),
     *            zog(l),zag(l),zfg(l),zqg(l),
     *            tog(l),tag(l),tfg(l),tqg(l),
     *            qog(l),qag(l),qfg(l),qqg(l),
     *            uog(l),uag(l),ufg(l),wqg(l),
     *            vog(l),vag(l),vfg(l),l=1,nl)
c       endif
c
       id6=id(6:6)
       idb=' '
       if((idbug.eq.1).and.(id6.ne.idb)) then
       print *,'duplicate station for ',id
       go to 100
       endif
c
       if(typx.ne.20.) go to 100
c
c... first time store station id's 
       if(ntmx.eq.1) then
c
       ist=ist+1
c      print *,ist,' new id ',id
       tstn(ist)=id
       tlon(ist)=rlon
       tlat(ist)=rlat
       typ(ist)=typx
c
       if(idbug.eq.1) then
       if((rlat.ge.30.0).and.(rlat.le.40.)) then
       if((rlon.ge.250.0).and.(rlon.le.270.)) then
c      if((id.eq.idpr1).or.(id.eq.idpr2)) then
c      write(6,510) ist,indate,id,rlon,rlat,typx,rt,
c    * psog,psag,psfg,psqg
c      print *,indate,' ',id,' number of levels ',nl
c      do l=1,nl
       l=3
       pval=pog(l)
       if(pval.ne.bmiss) then
       k=levp(nint(pval))
       if(k.gt.0) then
c      write(6,511) ist,indate,id,rlon,rlat,typx,rt,
c    * l,k,pval,zog(l),zag(l),zfg(l),zqg(l)
       write(6,512) ist,indate,id,rlon,rlat,typx,rt,
     * l,k,pval,tog(l),tag(l),tfg(l),tqg(l)
c      write(6,513) ist,indate,id,rlon,rlat,typx,rt,
c    * l,k,pval,qog(l),qag(l),qfg(l),qqg(l)
c      write(6,514) ist,indate,id,rlon,rlat,typx,rt,
c    * l,k,pval,uog(l),uag(l),ufg(l),wqg(l)
c      write(6,515) ist,indate,id,rlon,rlat,typx,rt,
c    * l,k,pval,vog(l),vag(l),vfg(l)
       endif
       endif
c      enddo
       endif
       endif
       endif
c
       if((id.eq.idpr1).or.(id.eq.idpr2)) then
       idprstn=ist
       irka=1
       print *,' station number ',idprstn,' idpr ',id
       endif
c
       if(psqg.le.3) then
       x1=psog
       x2=psag
       x3=psfg
       if((x1.ne.bmiss).and.(x2.ne.bmiss).and.(x3.ne.bmiss)) then
       fps(ist)=fps(ist)+1.
       pso(ist)=psog
       psos(ist)=psog*psog
       psa(ist)=psag
       psas(ist)=psag*psag
       psao(ist)=psag*psog
       psf(ist)=psfg
       psfs(ist)=psfg*psfg
       psfo(ist)=psfg*psog
       endif
       endif
c
       do l=1,nl
       pval=pog(l)
c
       if(pval.ne.bmiss) then
       k=levp(nint(pval))
c
       if(k.gt.0) then
       po(k,ist)=pog(l)
c
       if(zqg(l).le.3) then
       x1=zog(l)
       x2=zag(l)
       x3=zfg(l)
       if((x1.ne.bmiss).and.(x2.ne.bmiss).and.(x3.ne.bmiss)) then
       fz(k,ist)=fz(k,ist)+1.
       zo(k,ist)=zog(l)
       zos(k,ist)=zog(l)*zog(l)
       za(k,ist)=zag(l)
       zas(k,ist)=zag(l)*zag(l)
       zao(k,ist)=zag(l)*zog(l)
       zf(k,ist)=zfg(l)
       zfs(k,ist)=zfg(l)*zfg(l)
       zfo(k,ist)=zfg(l)*zog(l)
       endif
       endif
c
       if(tqg(l).le.3) then
       x1=tog(l)
       x2=tag(l)
       x3=tfg(l)
       if((x1.ne.bmiss).and.(x2.ne.bmiss).and.(x3.ne.bmiss)) then
       ft(k,ist)=ft(k,ist)+1.
       to(k,ist)=tog(l)
       tos(k,ist)=tog(l)*tog(l)
       ta(k,ist)=tag(l)
       tas(k,ist)=tag(l)*tag(l)
       tao(k,ist)=tag(l)*tog(l)
       tf(k,ist)=tfg(l)
       tfs(k,ist)=tfg(l)*tfg(l)
       tfo(k,ist)=tfg(l)*tog(l)
       endif
       endif
c
       if(qqg(l).le.3) then
       x1=qog(l)
       x2=qag(l)
       x3=qfg(l)
       if((x1.ne.bmiss).and.(x2.ne.bmiss).and.(x3.ne.bmiss)) then
       fq(k,ist)=fq(k,ist)+1.
       qo(k,ist)=qog(l)
       qos(k,ist)=qog(l)*qog(l)
       qa(k,ist)=qag(l)
       qas(k,ist)=qag(l)*qag(l)
       qao(k,ist)=qag(l)*qog(l)
       qf(k,ist)=qfg(l)
       qfs(k,ist)=qfg(l)*qfg(l)
       qfo(k,ist)=qfg(l)*qog(l)
       endif
       endif
C
       if(wqg(l).le.3) then
       x1=uog(l)
       x2=uag(l)
       x3=ufg(l)
       y1=vog(l)
       y2=vag(l)
       y3=vfg(l)
       if((x1.ne.bmiss).and.(x2.ne.bmiss).and.(x3.ne.bmiss).and.
     *    (y1.ne.bmiss).and.(y2.ne.bmiss).and.(y3.ne.bmiss)) then
       fw(k,ist)=fw(k,ist)+1.
       uo(k,ist)=uog(l)
       vo(k,ist)=vog(l)
       uvo(k,ist)=uog(l)*uog(l)+vog(l)*vog(l)
       ua(k,ist)=uag(l)
       va(k,ist)=vag(l)
       uva(k,ist)=uag(l)*uag(l)+vag(l)*vag(l)
       uvao(k,ist)=uag(l)*uog(l)+vag(l)*vog(l)
       spda(k,ist)=sqrt(uva(k,ist))-sqrt(uvo(k,ist))
       uf(k,ist)=ufg(l)
       vf(k,ist)=vfg(l)
       uvf(k,ist)=ufg(l)*ufg(l)+vfg(l)*vfg(l)
       uvfo(k,ist)=ufg(l)*uog(l)+vfg(l)*vog(l)
       spdf(k,ist)=sqrt(uvf(k,ist))-sqrt(uvo(k,ist))
       endif
       endif
c
       endif
       endif
       enddo
c
       go to 100
c
       endif
c
c..   check for unique station....
       do n=1,ist
c
       idn=tstn(n)
       dlon=abs(rlon-tlon(n))
       dlat=abs(rlat-tlat(n))
c      print *,n,id,tstn(n),rlon,tlon(n),dlon,rlat,tlat(n),dlat
c
       if((dlat.le.0.01).and.(dlon.le.0.01)) then
c
       if(idbug.eq.1) then
       if((rlat.ge.30.0).and.(rlat.le.40.)) then
       if((rlon.ge.250.0).and.(rlon.le.270.)) then
c      if((id.eq.idpr1).or.(id.eq.idpr2)) then
c      write(6,510) ist,indate,id,rlon,rlat,typx,rt,
c    * psog,psag,psfg,psqg
c      print *,indate,' ',id,' number of levels ',nl
c      do l=1,nl
       l=3
       pval=pog(l)
       if(pval.ne.bmiss) then
       k=levp(nint(pval))
       if(k.gt.0) then
c      write(6,511) n,indate,id,rlon,rlat,typx,rt,
c    * l,k,pval,zog(l),zag(l),zfg(l),zqg(l)
       write(6,512) n,indate,id,rlon,rlat,typx,rt,
     * l,k,pval,tog(l),tag(l),tfg(l),tqg(l)
c      write(6,513) n,indate,id,rlon,rlat,typx,rt,
c    * l,k,pval,qog(l),qag(l),qfg(l),qqg(l)
c      write(6,514) n,indate,id,rlon,rlat,typx,rt,
c    * l,k,pval,uog(l),uag(l),ufg(l),wqg(l)
c      write(6,515) n,indate,id,rlon,rlat,typx,rt,
c    * l,k,pval,vog(l),vag(l),vfg(l)
       endif
       endif
c      enddo
       endif
       endif
       endif
c
       if(psqg.le.3) then
       x1=psog
       x2=psag
       x3=psfg
       if((x1.ne.bmiss).and.(x2.ne.bmiss).and.(x3.ne.bmiss)) then
       fps(n)=fps(n)+1.
       if(pso(n).eq.bmiss) then
       pso(n)=psog
       psos(n)=psog*psog
       else
       pso(n)=pso(n)+psog
       psos(n)=psos(n)+psog*psog
       endif
       if(psa(n).eq.bmiss) then
       psa(n)=psag
       psas(n)=psag*psag
       psao(n)=psag*psog
       else
       psa(n)=psa(n)+psag
       psas(n)=psas(n)+psag*psag
       psao(n)=psao(n)+psag*psog
       endif
       if(psf(n).eq.bmiss) then
       psf(n)=psfg
       psfs(n)=psfg*psfg
       psfo(n)=psfg*psog
       else
       psf(n)=psf(n)+psfg
       psfs(n)=psfs(n)+psfg*psfg
       psfo(n)=psfo(n)+psfg*psog
       endif
       endif
       endif
c
       do l=1,nl
       pval=pog(l)
       k=levp(nint(pval))
c
       if(k.gt.0) then
c
       if(zqg(l).le.3) then
       x1=zog(l)
       x2=zag(l)
       x3=zfg(l)
       if((x1.ne.bmiss).and.(x2.ne.bmiss).and.(x3.ne.bmiss)) then
       fz(k,n)=fz(k,n)+1.
       if(zo(k,n).eq.bmiss) then
       zo(k,n)=zog(l)
       zos(k,n)=zog(l)*zog(l)
       else
       zo(k,n)=zo(k,n)+zog(l)
       zos(k,n)=zos(k,n)+zog(l)*zog(l)
       endif
       if(za(k,n).eq.bmiss) then
       za(k,n)=zag(l)
       zas(k,n)=zag(l)*zag(l)
       zao(k,n)=zag(l)*zog(l)
       else
       za(k,n)=za(k,n)+zag(l)
       zas(k,n)=zas(k,n)+zag(l)*zag(l)
       zao(k,n)=zao(k,n)+zag(l)*zog(l)
       endif
       if(zf(k,n).eq.bmiss) then
       zf(k,n)=zfg(l)
       zfs(k,n)=zfg(l)*zfg(l)
       zfo(k,n)=zfg(l)*zog(l)
       else
       zf(k,n)=zf(k,n)+zfg(l)
       zfs(k,n)=zfs(k,n)+zfg(l)*zfg(l)
       zfo(k,n)=zfo(k,n)+zfg(l)*zog(l)
       endif
       endif
       endif
c
       if(tqg(l).le.3) then
       x1=tog(l)
       x2=tag(l)
       x3=tfg(l)
       if((x1.ne.bmiss).and.(x2.ne.bmiss).and.(x3.ne.bmiss)) then
       ft(k,n)=ft(k,n)+1.
       if(to(k,n).eq.bmiss) then
       to(k,n)=tog(l)
       tos(k,n)=tog(l)*tog(l)
       else
       to(k,n)=to(k,n)+tog(l)
       tos(k,n)=tos(k,n)+tog(l)*tog(l)
       endif
       if(ta(k,n).eq.bmiss) then
       ta(k,n)=tag(l)
       tas(k,n)=tag(l)*tag(l)
       tao(k,n)=tag(l)*tog(l)
       else
       ta(k,n)=ta(k,n)+tag(l)
       tas(k,n)=tas(k,n)+tag(l)*tag(l)
       tao(k,n)=tao(k,n)+tag(l)*tog(l)
       endif
       if(tf(k,n).eq.bmiss) then
       tf(k,n)=tfg(l)
       tfs(k,n)=tfg(l)*tfg(l)
       tfo(k,n)=tfg(l)*tog(l)
       else
       tf(k,n)=tf(k,n)+tfg(l)
       tfs(k,n)=tfs(k,n)+tfg(l)*tfg(l)
       tfo(k,n)=tfo(k,n)+tfg(l)*tog(l)
       endif
       endif
       endif
c
       if(qqg(l).le.3) then
       x1=qog(l)
       x2=qag(l)
       x3=qfg(l)
       if((x1.ne.bmiss).and.(x2.ne.bmiss).and.(x3.ne.bmiss)) then
       fq(k,n)=fq(k,n)+1.
       if(qo(k,n).eq.bmiss) then
       qo(k,n)=qog(l)
       qos(k,n)=qog(l)*qog(l)
       else
       qo(k,n)=qo(k,n)+qog(l)
       qos(k,n)=qos(k,n)+qog(l)*qog(l)
       endif
       if(qa(k,n).eq.bmiss) then
       qa(k,n)=qag(l)
       qas(k,n)=qag(l)*qag(l)
       qao(k,n)=qag(l)*qog(l)
       else
       qa(k,n)=qa(k,n)+qag(l)
       qas(k,n)=qas(k,n)+qag(l)*qag(l)
       qao(k,n)=qao(k,n)+qag(l)*qog(l)
       endif
       if(qf(k,n).eq.bmiss) then
       qf(k,n)=qfg(l)
       qfs(k,n)=qfg(l)*qfg(l)
       qfo(k,n)=qfg(l)*qog(l)
       else
       qf(k,n)=qf(k,n)+qfg(l)
       qfs(k,n)=qfs(k,n)+qfg(l)*qfg(l)
       qfo(k,n)=qfo(k,n)+qfg(l)*qog(l)
       endif
       endif
       endif
c
       if(wqg(l).le.3) then
       x1=uog(l)
       x2=uag(l)
       x3=ufg(l)
       y1=vog(l)
       y2=vag(l)
       y3=vfg(l)
       if((x1.ne.bmiss).and.(x2.ne.bmiss).and.(x3.ne.bmiss).and.
     *    (y1.ne.bmiss).and.(y2.ne.bmiss).and.(y3.ne.bmiss)) then
       fw(k,n)=fw(k,n)+1.
       if((uo(k,n).eq.bmiss).and.(vo(k,n).eq.bmiss)) then
       uo(k,n)=uog(l)
       vo(k,n)=vog(l)
       uvo(k,n)=uog(l)*uog(l)+vog(l)*vog(l)
       else
       uo(k,n)=uo(k,n)+uog(l)
       vo(k,n)=vo(k,n)+vog(l)
       uvo(k,n)=uvo(k,n)+uog(l)*uog(l)+vog(l)*vog(l)
       endif
       if((ua(k,n).eq.bmiss).and.(va(k,n).eq.bmiss)) then
       ua(k,n)=uag(l)
       va(k,n)=vag(l)
       uva(k,n)=uag(l)*uag(l)+vag(l)*vag(l)
       uvao(k,n)=uag(l)*uog(l)+vag(l)*vog(l)
       spda(k,n)=sqrt(uva(k,n))-sqrt(uvo(k,n))
       else
       ua(k,n)=ua(k,n)+uag(l)
       va(k,n)=va(k,n)+vag(l)
       uva(k,n)=uva(k,n)+uag(l)*uag(l)+vag(l)*vag(l)
       uvao(k,n)=uvao(k,n)+uag(l)*uog(l)+vag(l)*vog(l)
       spda(k,n)=spda(k,n)+sqrt(uva(k,n))-sqrt(uvo(k,n))
       endif
       if((uf(k,n).eq.bmiss).and.(vf(k,n).eq.bmiss)) then
       uf(k,n)=ufg(l)
       vf(k,n)=vfg(l)
       uvf(k,n)=ufg(l)*ufg(l)+vfg(l)*vfg(l)
       uvfo(k,n)=ufg(l)*uog(l)+vfg(l)*vog(l)
       spdf(k,n)=sqrt(uvf(k,n))-sqrt(uvo(k,n))
       else
       uf(k,n)=uf(k,n)+ufg(l)
       vf(k,n)=vf(k,n)+vfg(l)
       uvf(k,n)=uvf(k,n)+ufg(l)*ufg(l)+vfg(l)*vfg(l)
       uvfo(k,n)=uvfo(k,n)+ufg(l)*uog(l)+vfg(l)*vog(l)
       spdf(k,n)=spdf(k,n)+sqrt(uvf(k,n))-sqrt(uvo(k,n))
       endif
       endif
       endif
c
       endif
       enddo
c
       go to 100
       endif
c
c... go thru all stations...
       enddo
c
c...  if not, then add a new station....
       ist=ist+1
       tstn(ist)=id
       tlon(ist)=rlon
       tlat(ist)=rlat
c      print *,ist,' new id ',id
c
       if(psqg.le.3) then
       x1=psog
       x2=psag
       x3=psfg
       if((x1.ne.bmiss).and.(x2.ne.bmiss).and.(x3.ne.bmiss)) then
       fps(ist)=fps(ist)+1.
       pso(ist)=psog
       psos(ist)=psog*psog
       psa(ist)=psag
       psas(ist)=psag*psag
       psao(ist)=psag*psog
       psf(ist)=psfg
       psfs(ist)=psfg*psfg
       psfo(ist)=psfg*psog
       endif
       endif
c
       do l=1,nl
       pval=pog(l)
c
       if(pval.ne.bmiss) then
       k=levp(nint(pval))
c
       if(k.gt.0) then
       po(k,ist)=pog(l)
c
       if(zqg(l).le.3) then
       x1=zog(l)
       x2=zag(l)
       x3=zfg(l)
       if((x1.ne.bmiss).and.(x2.ne.bmiss).and.(x3.ne.bmiss)) then
       fz(k,ist)=fz(k,ist)+1.
       zo(k,ist)=zog(l)
       zos(k,ist)=zog(l)*zog(l)
       za(k,ist)=zag(l)
       zas(k,ist)=zag(l)*zag(l)
       zao(k,ist)=zag(l)*zog(l)
       zf(k,ist)=zfg(l)
       zfs(k,ist)=zfg(l)*zfg(l)
       zfo(k,ist)=zfg(l)*zog(l)
       endif
       endif
c
       if(tqg(l).le.3) then
       x1=tog(l)
       x2=tag(l)
       x3=tfg(l)
       if((x1.ne.bmiss).and.(x2.ne.bmiss).and.(x3.ne.bmiss)) then
       ft(k,ist)=ft(k,ist)+1.
       to(k,ist)=tog(l)
       tos(k,ist)=tog(l)*tog(l)
       ta(k,ist)=tag(l)
       tas(k,ist)=tag(l)*tag(l)
       tao(k,ist)=tag(l)*tog(l)
       tf(k,ist)=tfg(l)
       tfs(k,ist)=tfg(l)*tfg(l)
       tfo(k,ist)=tfg(l)*tog(l)
       endif
       endif
c
       if(qqg(l).le.3) then
       x1=qog(l)
       x2=qag(l)
       x3=qfg(l)
       if((x1.ne.bmiss).and.(x2.ne.bmiss).and.(x3.ne.bmiss)) then
       fq(k,ist)=fq(k,ist)+1.
       qo(k,ist)=qog(l)
       qos(k,ist)=qog(l)*qog(l)
       qa(k,ist)=qag(l)
       qas(k,ist)=qag(l)*qag(l)
       qao(k,ist)=qag(l)*qog(l)
       qf(k,ist)=qfg(l)
       qfs(k,ist)=qfg(l)*qfg(l)
       qfo(k,ist)=qfg(l)*qog(l)
       endif
       endif
C
       if(wqg(l).le.3) then
       x1=uog(l)
       x2=uag(l)
       x3=ufg(l)
       y1=vog(l)
       y2=vag(l)
       y3=vfg(l)
       if((x1.ne.bmiss).and.(x2.ne.bmiss).and.(x3.ne.bmiss).and.
     *    (y1.ne.bmiss).and.(y2.ne.bmiss).and.(y3.ne.bmiss)) then
       fw(k,ist)=fw(k,ist)+1.
       uo(k,ist)=uog(l)
       vo(k,ist)=vog(l)
       uvo(k,ist)=uog(l)*uog(l)+vog(l)*vog(l)
       ua(k,ist)=uag(l)
       va(k,ist)=vag(l)
       uva(k,ist)=uag(l)*uag(l)+vag(l)*vag(l)
       uvao(k,ist)=uag(l)*uog(l)+vag(l)*vog(l)
       spda(k,ist)=sqrt(uva(k,ist))-sqrt(uvo(k,ist))
       uf(k,ist)=ufg(l)
       vf(k,ist)=vfg(l)
       uvf(k,ist)=ufg(l)*ufg(l)+vfg(l)*vfg(l)
       uvfo(k,ist)=ufg(l)*uog(l)+vfg(l)*vog(l)
       spdf(k,ist)=sqrt(uvf(k,ist))-sqrt(uvo(k,ist))
       endif
       endif
c
       endif
       endif
       enddo
c
       go to 100
c
c... now read a new date in..
 101   continue
       close(11)
c
       if((idbug.eq.1).and.(irka.eq.1)) then
       istn=idprstn
       id=tstn(istn)
       rlon=tlon(istn)
       rlat=tlat(istn)
       write(6,610) istn,indate,id,rlon,rlat,
     * fps(istn),pso(istn),psa(istn),psf(istn)
       write(6,711) istn,indate,id,rlon,rlat,
     * psos(istn),psas(istn),psfs(istn),psao(istn),psfo(istn)
       do l=1,nlevs
       pval=po(l,istn)
       if(pval.ne.bmiss) then
       k=levp(nint(pval))
       if(k.gt.0) then
       write(6,611) istn,indate,id,rlon,rlat,
     * l,k,pval,fz(k,istn),zo(k,istn),za(k,istn),zf(k,istn)
       write(6,612) istn,indate,id,rlon,rlat,
     * l,k,pval,ft(k,istn),to(k,istn),ta(k,istn),tf(k,istn)
       write(6,613) istn,indate,id,rlon,rlat,
     * l,k,pval,fq(k,istn),qo(k,istn),qa(k,istn),qf(k,istn)
       write(6,614) istn,indate,id,rlon,rlat,
     * l,k,pval,fw(k,istn),uo(k,istn),ua(k,istn),uf(k,istn)
       write(6,615) istn,indate,id,rlon,rlat,
     * l,k,pval,fw(k,istn),vo(k,istn),va(k,istn),vf(k,istn)
c
       write(6,711) istn,indate,id,rlon,rlat,l,k,pval,
     * zos(k,istn),zas(k,istn),zfs(k,istn),zao(k,istn),zfo(k,istn)
       write(6,712) istn,indate,id,rlon,rlat,l,k,pval,
     * tos(k,istn),tas(k,istn),tfs(k,istn),tao(k,istn),tfo(k,istn)
       write(6,713) istn,indate,id,rlon,rlat,l,k,pval,
     * qos(k,istn),qas(k,istn),qfs(k,istn),qao(k,istn),qfo(k,istn)
       write(6,714) istn,indate,id,rlon,rlat,l,k,pval,
     * uvo(k,istn),uva(k,istn),uvf(k,istn),uvao(k,istn),uvfo(k,istn)
       write(6,715) istn,indate,id,rlon,rlat,l,k,pval,
     * spda(k,istn),spdf(k,istn)
       endif
       endif
       enddo
       endif
c
 444   continue
c
 555   continue
c
       nstns=ist
       print *,'total number of stations is ',nstns
       print *,'total number of time levels is ',ntmx
c
c... compute time means
c
       do ist=1,nstns
c
       if(fps(ist).ge.1.) then
       fips=1.0/fps(ist)
       if(pso(ist).ne.bmiss) pso(ist)=pso(ist)*fips
       x1=psos(ist)
       if(x1.ne.bmiss) psos(ist)=psos(ist)*fips
       if(psa(ist).ne.bmiss) psa(ist)=psa(ist)*fips
       x2=psas(ist)
       if(x2.ne.bmiss) psas(ist)=psas(ist)*fips
       x3=psao(ist)
       if(x3.ne.bmiss) psao(ist)=psao(ist)*fips
       if((x2.ne.bmiss).and.(x1.ne.bmiss).and.(x3.ne.bmiss)) then
       val=psas(ist)+psos(ist)-2*psao(ist)
       if(val.gt.0.) psae(ist)=sqrt(val)
       endif
       if(psf(ist).ne.bmiss) psf(ist)=psf(ist)*fips
       x2=psfs(ist)
       if(x2.ne.bmiss) psfs(ist)=psfs(ist)*fips
       x3=psfo(ist)
       if(x3.ne.bmiss) psfo(ist)=psfo(ist)*fips
       if((x2.ne.bmiss).and.(x1.ne.bmiss).and.(x3.ne.bmiss)) then
       val=psfs(ist)+psos(ist)-2*psfo(ist)
       if(val.gt.0.) psfe(ist)=sqrt(val)
       endif
       endif
c 
       do k=1,nlevs
c
       if(fz(k,ist).ge.1.) then
       fiz=1.0/fz(k,ist)
       if(zo(k,ist).ne.bmiss) zo(k,ist)=zo(k,ist)*fiz
       x1=zos(k,ist)
       if(x1.ne.bmiss) zos(k,ist)=zos(k,ist)*fiz
       if(za(k,ist).ne.bmiss) za(k,ist)=za(k,ist)*fiz
       x2=zas(k,ist)
       if(x2.ne.bmiss) zas(k,ist)=zas(k,ist)*fiz
       x3=zao(k,ist)
       if(x3.ne.bmiss) zao(k,ist)=zao(k,ist)*fiz
       if((x2.ne.bmiss).and.(x1.ne.bmiss).and.(x3.ne.bmiss)) then
       val=zas(k,ist)+zos(k,ist)-2*zao(k,ist)
       if(val.gt.0.) zae(k,ist)=sqrt(val)
       endif
       if(zf(k,ist).ne.bmiss) zf(k,ist)=zf(k,ist)*fiz
       x2=zfs(k,ist)
       if(x2.ne.bmiss) zfs(k,ist)=zfs(k,ist)*fiz
       x3=zfo(k,ist)
       if(x3.ne.bmiss) zfo(k,ist)=zfo(k,ist)*fiz
       if((x2.ne.bmiss).and.(x1.ne.bmiss).and.(x3.ne.bmiss)) then
       val=zfs(k,ist)+zos(k,ist)-2*zfo(k,ist)
       if(val.gt.0.) zfe(k,ist)=sqrt(val)
       endif
       endif
c 
       if(ft(k,ist).ge.1.) then
       fit=1.0/ft(k,ist)
       if(to(k,ist).ne.bmiss) to(k,ist)=to(k,ist)*fit
       x1=tos(k,ist)
       if(x1.ne.bmiss) tos(k,ist)=tos(k,ist)*fit
       if(ta(k,ist).ne.bmiss) ta(k,ist)=ta(k,ist)*fit
       x2=tas(k,ist)
       if(x2.ne.bmiss) tas(k,ist)=tas(k,ist)*fit
       x3=tao(k,ist)
       if(x3.ne.bmiss) tao(k,ist)=tao(k,ist)*fit
       if((x2.ne.bmiss).and.(x1.ne.bmiss).and.(x3.ne.bmiss)) then
       val=tas(k,ist)+tos(k,ist)-2*tao(k,ist)
       if(val.gt.0.) tae(k,ist)=sqrt(val)
       endif
       if(tf(k,ist).ne.bmiss) tf(k,ist)=tf(k,ist)*fit
       x2=tfs(k,ist)
       if(x2.ne.bmiss) tfs(k,ist)=tfs(k,ist)*fit
       x3=tfo(k,ist)
       if(x3.ne.bmiss) tfo(k,ist)=tfo(k,ist)*fit
       if((x2.ne.bmiss).and.(x1.ne.bmiss).and.(x3.ne.bmiss)) then
       val=tfs(k,ist)+tos(k,ist)-2*tfo(k,ist)
       if(val.gt.0.) tfe(k,ist)=sqrt(val)
       endif
       endif
c 
       if(fq(k,ist).ge.1.) then
       fiq=1.0/fq(k,ist)
       if(qo(k,ist).ne.bmiss) qo(k,ist)=qo(k,ist)*fiq
       x1=qos(k,ist)
       if(x1.ne.bmiss) qos(k,ist)=qos(k,ist)*fiq
       if(qa(k,ist).ne.bmiss) qa(k,ist)=qa(k,ist)*fiq
       x2=qas(k,ist)
       if(x2.ne.bmiss) qas(k,ist)=qas(k,ist)*fiq
       x3=qao(k,ist)
       if(x3.ne.bmiss) qao(k,ist)=qao(k,ist)*fiq
       if((x2.ne.bmiss).and.(x1.ne.bmiss).and.(x3.ne.bmiss)) then
       val=qas(k,ist)+qos(k,ist)-2*qao(k,ist)
       if(val.gt.0.) qae(k,ist)=sqrt(val)
       endif
       if(qf(k,ist).ne.bmiss) qf(k,ist)=qf(k,ist)*fiq
       x2=qfs(k,ist)
       if(x2.ne.bmiss) qfs(k,ist)=qfs(k,ist)*fiq
       x3=qfo(k,ist)
       if(x3.ne.bmiss) qfo(k,ist)=qfo(k,ist)*fiq
       if((x2.ne.bmiss).and.(x1.ne.bmiss).and.(x3.ne.bmiss)) then
       val=qfs(k,ist)+qos(k,ist)-2*qfo(k,ist)
       if(val.gt.0.) qfe(k,ist)=sqrt(val)
       endif
       endif
c 
       if(fw(k,ist).ge.1.) then
       fiw=1.0/fw(k,ist)
       if(uo(k,ist).ne.bmiss) uo(k,ist)=uo(k,ist)*fiw
       if(vo(k,ist).ne.bmiss) vo(k,ist)=vo(k,ist)*fiw
       x1=uvo(k,ist)
       if(x1.ne.bmiss) uvo(k,ist)=uvo(k,ist)*fiw
       if(ua(k,ist).ne.bmiss) ua(k,ist)=ua(k,ist)*fiw
       if(va(k,ist).ne.bmiss) va(k,ist)=va(k,ist)*fiw
       x2=uva(k,ist)
       if(x2.ne.bmiss) uva(k,ist)=uva(k,ist)*fiw
       x3=uvao(k,ist)
       if(x3.ne.bmiss) uvao(k,ist)=uvao(k,ist)*fiw
       if((x2.ne.bmiss).and.(x1.ne.bmiss).and.(x3.ne.bmiss)) then
       val=uva(k,ist)+uvo(k,ist)-2*uvao(k,ist)
       if(val.gt.0.) wae(k,ist)=sqrt(val)
       endif
       if(spda(k,ist).ne.bmiss) spda(k,ist)=spda(k,ist)*fiw
       if(uf(k,ist).ne.bmiss) uf(k,ist)=uf(k,ist)*fiw
       if(vf(k,ist).ne.bmiss) vf(k,ist)=vf(k,ist)*fiw
       x2=uvf(k,ist)
       if(x2.ne.bmiss) uvf(k,ist)=uvf(k,ist)*fiw
       x3=uvfo(k,ist)
       if(x3.ne.bmiss) uvfo(k,ist)=uvfo(k,ist)*fiw
       if((x2.ne.bmiss).and.(x1.ne.bmiss).and.(x3.ne.bmiss)) then
       val=uvf(k,ist)+uvo(k,ist)-2*uvfo(k,ist)
       if(val.gt.0.) wfe(k,ist)=sqrt(val)
       endif
       if(spdf(k,ist).ne.bmiss) spdf(k,ist)=spdf(k,ist)*fiw
       endif
c
       enddo
       enddo
c
c... now write grads file out
c
       open(51,file=outfile,form='unformatted')
c
       nstf=0
       do ist=1,nstns
       id=tstn(ist)
c
       pscg=bmiss
       psog=bmiss
       psag=bmiss
       psfg=bmiss
       psqg=bmiss
       psaeg=bmiss
       psfeg=bmiss
c
       do l=1,nlevs
       pog(l)=bmiss
       zcg(l)=bmiss
       zog(l)=bmiss
       zag(l)=bmiss
       zfg(l)=bmiss
       zqg(l)=bmiss
       zaeg(l)=bmiss
       zfeg(l)=bmiss
       tcg(l)=bmiss
       tog(l)=bmiss
       tag(l)=bmiss
       tfg(l)=bmiss
       tqg(l)=bmiss
       taeg(l)=bmiss
       tfeg(l)=bmiss
       qcg(l)=bmiss
       qog(l)=bmiss
       qag(l)=bmiss
       qfg(l)=bmiss
       qqg(l)=bmiss
       qaeg(l)=bmiss
       qfeg(l)=bmiss
       uog(l)=bmiss
       uag(l)=bmiss
       ufg(l)=bmiss
       vog(l)=bmiss
       vag(l)=bmiss
       vfg(l)=bmiss
       wqg(l)=bmiss
       wcg(l)=bmiss
       waeg(l)=bmiss
       wfeg(l)=bmiss
       spdag(l)=bmiss
       spdfg(l)=bmiss
       enddo
c
c... check how many levels are there...
       nl=0
       do l=1,nlevs
       pval=po(l,ist)
       if(pval.ne.bmiss) then
       k=levp(nint(pval))
       if(k.gt.0.) then
       nl=nl+1
       pog(nl)=po(k,ist)
       zcg(nl)=fz(k,ist)
       zog(nl)=zo(k,ist)
       zag(nl)=za(k,ist)
       zfg(nl)=zf(k,ist)
       zaeg(nl)=zae(k,ist)
       zfeg(nl)=zfe(k,ist)
       tcg(nl)=ft(k,ist)
       tog(nl)=to(k,ist)
       tag(nl)=ta(k,ist)
       tfg(nl)=tf(k,ist)
       taeg(nl)=tae(k,ist)
       tfeg(nl)=tfe(k,ist)
       qcg(nl)=fq(k,ist)
       qog(nl)=qo(k,ist)
       qag(nl)=qa(k,ist)
       qfg(nl)=qf(k,ist)
       qaeg(nl)=qae(k,ist)
       qfeg(nl)=qfe(k,ist)
       wcg(nl)=fw(k,ist)
       uog(nl)=uo(k,ist)
       uag(nl)=ua(k,ist)
       ufg(nl)=uf(k,ist)
       vog(nl)=vo(k,ist)
       vag(nl)=va(k,ist)
       vfg(nl)=vf(k,ist)
       waeg(nl)=wae(k,ist)
       wfeg(nl)=wfe(k,ist)
       spdag(nl)=spda(k,ist)
       spdfg(nl)=spdf(k,ist)
       endif
       endif
       enddo
c
       if(nl.eq.0) go to 1234
       nstf=nstf+1
c
       id=tstn(ist)
       rlon=tlon(ist)
       rlat=tlat(ist)
       typx=typ(ist)
c
       pscg=fps(ist)
       psog=pso(ist)
       psag=psa(ist)
       psfg=psf(ist)
       psaeg=psae(ist)
       psfeg=psfe(ist)
c
       rt=0.
       iflag=0
       id(7:7)=' '
       id(8:8)='\0'
c
       write(51) id,rlat,rlon,rt,nl,iflag
       write(51) (pog(l),
     *            typx,pscg,psog,psag,psfg,psaeg,psfeg,pog(l),
     *            zcg(l),zog(l),zag(l),zfg(l),zaeg(l),zfeg(l),
     *            tcg(l),tog(l),tag(l),tfg(l),taeg(l),tfeg(l),
     *            qcg(l),qog(l),qag(l),qfg(l),qaeg(l),qfeg(l),
     *            wcg(l),uog(l),uag(l),ufg(l),
     *                   vog(l),vag(l),vfg(l),
     *                   waeg(l),wfeg(l),spdag(l),spdfg(l),l=1,nl)
c
       if(idbug.eq.1) then
       if((rlat.ge.30.0).and.(rlat.le.40.)) then
       if((rlon.ge.250.0).and.(rlon.le.270.)) then
       print *,indate,' levels for station ',ist,' ',id,' ',nl
c      if((id.eq.idpr1).or.(id.eq.idpr2)) then
c      write(6,810) ist,id,rlon,rlat,typx,psog,psag,psfg
c      write(6,910) ist,id,rlon,rlat,typx,pscg,psaeg,psfeg
       do l=1,nl
       pval=pog(l)
c      write(6,811) ist,id,rlon,rlat,typx,l,pval,zog(l),zag(l),zfg(l)
       write(6,812) ist,id,rlon,rlat,typx,l,pval,tog(l),tag(l),tfg(l)
c      write(6,813) ist,id,rlon,rlat,typx,l,pval,qog(l),qag(l),qfg(l)
c      write(6,814) ist,id,rlon,rlat,typx,l,pval,uog(l),uag(l),ufg(l)
c      write(6,815) ist,id,rlon,rlat,typx,l,pval,vog(l),vag(l),vfg(l)
c
c      write(6,911) ist,id,rlon,rlat,typx,l,pval,zcg(l),zaeg(l),zfeg(l)
       write(6,912) ist,id,rlon,rlat,typx,l,pval,tcg(l),taeg(l),tfeg(l)
c      write(6,913) ist,id,rlon,rlat,typx,l,pval,qcg(l),qaeg(l),qfeg(l)
c      write(6,914) ist,id,rlon,rlat,typx,l,pval,wcg(l),waeg(l),wfeg(l)
c      write(6,915) ist,id,rlon,rlat,typx,l,pval,wcg(l),
c    * spdag(l),spdfg(l)
       enddo
c      endif
       endif
       endif
       endif
c
 1234  continue
c... end station-loop
       enddo
c
       print *,indate,' total number of stations ',nstf
c
c..  write footer to end this time level
       id='       '
       rlat=0.
       rlon=0.
       rt=0.
       nlev=0
       iflag=0
       write(51) id,rlat,rlon,rt,nlev,iflag
       close(51)
c
 510   format(i6,1x,a8,1x,a8,1x,f6.2,1x,f6.2,1x,f5.0,1x,f5.2,1x,
     * 'p',1x,3f10.2,1x,f3.0)
 511   format(i6,1x,a8,1x,a8,1x,f6.2,1x,f6.2,1x,f5.0,1x,f5.2,1x,
     * 'z',1x,i2,1x,i2,1x,f5.0,1x,3f10.2,1x,f3.0)
 512   format(i6,1x,a8,1x,a8,1x,f6.2,1x,f6.2,1x,f5.0,1x,f5.2,1x,
     * 't',1x,i2,1x,i2,1x,f5.0,1x,3f10.2,1x,f3.0)
 513   format(i6,1x,a8,1x,a8,1x,f6.2,1x,f6.2,1x,f5.0,1x,f5.2,1x,
     * 'q',1x,i2,1x,i2,1x,f5.0,1x,3f10.2,1x,f3.0)
 514   format(i6,1x,a8,1x,a8,1x,f6.2,1x,f6.2,1x,f5.0,1x,f5.2,1x,
     * 'u',1x,i2,1x,i2,1x,f5.0,1x,3f10.2,1x,f3.0)
 515   format(i6,1x,a8,1x,a8,1x,f6.2,1x,f6.2,1x,f5.0,1x,f5.2,1x,
     * 'v',1x,i2,1x,i2,1x,f5.0,1x,3f10.2)
c
 610   format(i6,1x,a8,1x,a8,1x,f6.2,1x,f6.2,1x,
     * 'pm',1x,4f10.2)
 611   format(i6,1x,a8,1x,a8,1x,f6.2,1x,f6.2,1x,
     * 'zm',1x,i2,1x,i2,1x,f5.0,1x,4f10.2)
 612   format(i6,1x,a8,1x,a8,1x,f6.2,1x,f6.2,1x,
     * 'tm',1x,i2,1x,i2,1x,f5.0,1x,4f10.2)
 613   format(i6,1x,a8,1x,a8,1x,f6.2,1x,f6.2,1x,
     * 'qm',1x,i2,1x,i2,1x,f5.0,1x,4f10.2)
 614   format(i6,1x,a8,1x,a8,1x,f6.2,1x,f6.2,1x,
     * 'um',1x,i2,1x,i2,1x,f5.0,1x,4f10.2)
 615   format(i6,1x,a8,1x,a8,1x,f6.2,1x,f6.2,1x,
     * 'vm',1x,i2,1x,i2,1x,f5.0,1x,4f10.2)
c
 710   format(i6,1x,a8,1x,a8,1x,f6.2,1x,f6.2,1x,
     * 'pe',1x,5f10.2)
 711   format(i6,1x,a8,1x,a8,1x,f6.2,1x,f6.2,1x,
     * 'ze',1x,i2,1x,i2,1x,f5.0,1x,5f10.2)
 712   format(i6,1x,a8,1x,a8,1x,f6.2,1x,f6.2,1x,
     * 'te',1x,i2,1x,i2,1x,f5.0,1x,5f10.2)
 713   format(i6,1x,a8,1x,a8,1x,f6.2,1x,f6.2,1x,
     * 'qe',1x,i2,1x,i2,1x,f5.0,1x,5f10.2)
 714   format(i6,1x,a8,1x,a8,1x,f6.2,1x,f6.2,1x,
     * 'we',1x,i2,1x,i2,1x,f5.0,1x,5f10.2)
 715   format(i6,1x,a8,1x,a8,1x,f6.2,1x,f6.2,1x,
     * 'ws',1x,i2,1x,i2,1x,f5.0,1x,2f10.2)
c
 810   format(i6,1x,a8,1x,f6.2,1x,f6.2,1x,f5.0,1x,
     * 'pm',1x,3f10.2)
 811   format(i6,1x,a8,1x,f6.2,1x,f6.2,1x,f5.0,1x,
     * 'zm',1x,i2,1x,f5.0,1x,3f10.2)
 812   format(i6,1x,a8,1x,f6.2,1x,f6.2,1x,f5.0,1x,
     * 'tm',1x,i2,1x,f5.0,1x,3f10.2)
 813   format(i6,1x,a8,1x,f6.2,1x,f6.2,1x,f5.0,1x,
     * 'qm',1x,i2,1x,f5.0,1x,3f10.2)
 814   format(i6,1x,a8,1x,f6.2,1x,f6.2,1x,f5.0,1x,
     * 'um',1x,i2,1x,f5.0,1x,3f10.2)
 815   format(i6,1x,a8,1x,f6.2,1x,f6.2,1x,f5.0,1x,
     * 'vm',1x,i2,1x,f5.0,1x,3f10.2)
c
 910   format(i6,1x,a8,1x,f6.2,1x,f6.2,1x,f5.0,1x,
     * 'pe',1x,3f10.2)
 911   format(i6,1x,a8,1x,f6.2,1x,f6.2,1x,f5.0,1x,
     * 'ze',1x,i2,1x,f5.0,1x,3f10.2)
 912   format(i6,1x,a8,1x,f6.2,1x,f6.2,1x,f5.0,1x,
     * 'te',1x,i2,1x,f5.0,1x,3f10.2)
 913   format(i6,1x,a8,1x,f6.2,1x,f6.2,1x,f5.0,1x,
     * 'qe',1x,i2,1x,f5.0,1x,3f10.2)
 914   format(i6,1x,a8,1x,f6.2,1x,f6.2,1x,f5.0,1x,
     * 'we',1x,i2,1x,f5.0,1x,3f10.2)
 915   format(i6,1x,a8,1x,f6.2,1x,f6.2,1x,f5.0,1x,
     * 'ws',1x,i2,1x,f5.0,1x,3f10.2)
c
       return
       end
