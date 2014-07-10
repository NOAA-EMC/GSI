       CHARACTER*1  kdbug,kmas,kwnd
       CHARACTER*2  kincr
       CHARACTER*250 iname
       CHARACTER*250 inputdir,outfile
       CHARACTER*10 ksyr,ksmth,ksday,kscy
       CHARACTER*10 keyr,kemth,keday,kecy
c
       call getenv("idbug",kdbug)
       read(kdbug,'(i1)') idbug
       write(*,*) "idbug= ",idbug
c
       call getenv("incr",kincr)
       read(kincr,'(i2)') incr
       write(*,*) "incr= ",incr
c
       call getenv("imas",kmas)
       read(kmas,'(i1)') imas
       write(*,*) "imas= ",imas
c
       call getenv("iwnd",kwnd)
       read(kwnd,'(i1)') iwnd
       write(*,*) "iwnd= ",iwnd
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
C
       call stats(inputdir,iname,outfile,idbug,incr,
     * isyr,ismth,isday,iscy,ieyr,iemth,ieday,iecy,
     * imas,iwnd)
c
       stop
       end
       subroutine stats(inputdir,iname,outfile,idbug,incr,
     * isyr,ismth,isday,iscy,ieyr,iemth,ieday,iecy,
     * imas,iwnd)
c
       parameter(nst=75000,ntyp=4)
       character*4  cycl(5),cyclx
       CHARACTER*10 indate,sdate,edate
       CHARACTER*4  LABY
       CHARACTER*2  LABM(12)
       CHARACTER*2  LABD(31)
       CHARACTER*2  LABC(4)
C
       character*8 tstn(nst),id,idn,idpr
       character*1 id6,idb
       CHARACTER*250 iname,inputdir
       CHARACTER*250 infile,outfile
c
       DIMENSION tlon(nst),tlat(nst),typ(nst)
C
       dimension po(nst),pa(nst),pf(nst)
       dimension pos(nst),pas(nst),pfs(nst)
       dimension pao(nst),pfo(nst),fp(nst)
       dimension pae(nst),pfe(nst)
       dimension zo(nst),za(nst),zf(nst)
       dimension zos(nst),zas(nst),zfs(nst)
       dimension zao(nst),zfo(nst),fz(nst)
       dimension zae(nst),zfe(nst)
       dimension to(nst),ta(nst),tf(nst)
       dimension tos(nst),tas(nst),tfs(nst)
       dimension tao(nst),tfo(nst),ft(nst)
       dimension tae(nst),tfe(nst)
       dimension qo(nst),qa(nst),qf(nst)
       dimension qos(nst),qas(nst),qfs(nst)
       dimension qao(nst),qfo(nst),fq(nst)
       dimension qae(nst),qfe(nst)
       dimension uo(nst),ua(nst),uf(nst)
       dimension vo(nst),va(nst),vf(nst)
       dimension uvo(nst),uva(nst),uvf(nst)
       dimension uvao(nst),uvfo(nst),fw(nst)
       dimension spda(nst),spdf(nst)
       dimension wae(nst),wfe(nst)
c
       dimension typset(ntyp)
       integer icycl(4)
c
       data typset/80.,81.,84.,81./
       DATA idpr/'16320   '/
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
c   
       if(imas.eq.1) then
       fp(ist)=0.
       po(ist)=bmiss
       pa(ist)=bmiss
       pf(ist)=bmiss
       pos(ist)=bmiss
       pas(ist)=bmiss
       pfs(ist)=bmiss
       pae(ist)=bmiss
       pfe(ist)=bmiss
       fz(ist)=0.
       zo(ist)=bmiss
       za(ist)=bmiss
       zf(ist)=bmiss
       zos(ist)=bmiss
       zas(ist)=bmiss
       zfs(ist)=bmiss
       zae(ist)=bmiss
       zfe(ist)=bmiss
       ft(ist)=0.
       to(ist)=bmiss
       ta(ist)=bmiss
       tf(ist)=bmiss
       tos(ist)=bmiss
       tas(ist)=bmiss
       tfs(ist)=bmiss
       tae(ist)=bmiss
       tfe(ist)=bmiss
       fq(ist)=0.
       qo(ist)=bmiss
       qa(ist)=bmiss
       qf(ist)=bmiss
       qos(ist)=bmiss
       qas(ist)=bmiss
       qfs(ist)=bmiss
       qae(ist)=bmiss
       qfe(ist)=bmiss
       endif
c
       if(iwnd.eq.1) then
       fw(ist)=0.
       uo(ist)=bmiss
       ua(ist)=bmiss
       uf(ist)=bmiss
       vo(ist)=bmiss
       va(ist)=bmiss
       vf(ist)=bmiss
       uvo(ist)=bmiss
       uva(ist)=bmiss
       uvf(ist)=bmiss
       uvao(ist)=bmiss
       uvfo(ist)=bmiss
       spda(ist)=bmiss
       spdf(ist)=bmiss
       wae(ist)=bmiss
       wfe(ist)=bmiss
       endif
c
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
       write(*,*) ntmx,ist," infile= ",infile
c
!!     open(11,file=infile,form='unformatted',iostat=ierr)
       call opendian(11,infile,ierr)
       if(ierr.ne.0) go to 101
c.. update time level ntmx only if you can actually read the file. Then rewind it.
       read(11,end=101) id,rlat,rlon,rt,nl,iflag
       ntmx=ntmx+1
       print *,' ntmx ',ntmx,' ist ',ist
       rewind 11
c
c....  read grads data in...
c
100    continue
c
       if(imas.eq.1) then
       pog=bmiss
       pag=bmiss
       pfg=bmiss
       pqg=bmiss
       zog=bmiss
       zag=bmiss
       zfg=bmiss
       zqg=bmiss
       tog=bmiss
       tag=bmiss
       tfg=bmiss
       tqg=bmiss
       qog=bmiss
       qag=bmiss
       qfg=bmiss
       qqg=bmiss
       endif
       if(iwnd.eq.1) then
       uog=bmiss
       uag=bmiss
       ufg=bmiss
       vog=bmiss
       vag=bmiss
       vfg=bmiss
       wqg=bmiss
       endif
c
       read(11,end=101) id,rlat,rlon,rt,nl,iflag
c
       read(indate,'(i10)') idate
       if(idate.le.1999061700) then
c
       if((imas.eq.1).and.(iwnd.eq.1)) then
       read(11,end=101) typx,elvx,
     *           pog,pag,pfg,pqg,
     *           zog,zag,zfg,zqg,
     *           tog,tag,tfg,tqg,
     *           qog,qag,qfg,qqg,
     *           uog,uag,ufg,wqg,
     *           vog,vag,vfg,bl
       endif
       if((imas.eq.1).and.(iwnd.eq.0)) then
       read(11,end=101) typx,elvx,
     *           pog,pag,pfg,pqg,
     *           zog,zag,zfg,zqg,
     *           tog,tag,tfg,tqg,
     *           qog,qag,qfg,qqg,bl
       endif
       if((imas.eq.0).and.(iwnd.eq.1)) then
       read(11,end=101) typx,elvx,
     *           uog,uag,ufg,wqg,
     *           vog,vag,vfg,bl
       endif
c
       else
c 
       if((imas.eq.1).and.(iwnd.eq.1)) then
       read(11,end=101) typx,typint,elvx,
     *           pog,pag,pfg,pqg,
     *           zog,zag,zfg,zqg,
     *           tog,tag,tfg,tqg,
     *           qog,qag,qfg,qqg,
     *           uog,uag,ufg,wqg,
     *           vog,vag,vfg,bl
       endif
       if((imas.eq.1).and.(iwnd.eq.0)) then
       read(11,end=101) typx,typint,elvx,
     *           pog,pag,pfg,pqg,
     *           zog,zag,zfg,zqg,
     *           tog,tag,tfg,tqg,
     *           qog,qag,qfg,qqg,bl
       endif
       if((imas.eq.0).and.(iwnd.eq.1)) then
       read(11,end=101) typx,typint,elvx,
     *           uog,uag,ufg,wqg,
     *           vog,vag,vfg,bl
       endif
c
       endif
c
       do ityp=1,ntyp
       typs=typset(ityp)
c      print *,'before ',typs,typx
       if(typx.eq.typs) go to 9876
       enddo
       go to 100
c
 9876  continue
c      print *,'after ',typs,typx
c
       istx=istx+1
       if(idbug.eq.1) then
       if(id.eq.idpr) then
       if(imas.eq.1) then
       write(6,511) istx,indate,id,rlon,rlat,typx,rt,pog,pag,pfg,pqg
       write(6,512) istx,indate,id,rlon,rlat,typx,rt,zog,zag,zfg,zqg
       write(6,513) istx,indate,id,rlon,rlat,typx,rt,tog,tag,tfg,tqg
       write(6,514) istx,indate,id,rlon,rlat,typx,rt,qog,qag,qfg,qqg
       endif
       if(iwnd.eq.1) then
       write(6,515) istx,indate,id,rlon,rlat,typx,rt,uog,uag,ufg,wqg
       write(6,516) istx,indate,id,rlon,rlat,typx,rt,vog,vag,vfg
       endif
       endif
       endif
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
       if(id.eq.idpr) idprstn=ist
c
       if(imas.eq.1) then
c
       if(pqg.le.3) then
       x1=pog
       x2=pag
       x3=pfg
       if((x1.ne.bmiss).and.(x2.ne.bmiss).and.(x3.ne.bmiss)) then
       fp(ist)=fp(ist)+1.
       po(ist)=pog
       pos(ist)=pog*pog
       pa(ist)=pag
       pas(ist)=pag*pag
       pao(ist)=pag*pog
       pf(ist)=pfg
       pfs(ist)=pfg*pfg
       pfo(ist)=pfg*pog
       endif
       endif
c
       if(zqg.le.3) then
       x1=zog
       x2=zag
       x3=zfg
       if((x1.ne.bmiss).and.(x2.ne.bmiss).and.(x3.ne.bmiss)) then
       fz(ist)=fz(ist)+1.
       zo(ist)=zog
       zos(ist)=zog*zog
       za(ist)=zag
       zas(ist)=zag*zag
       zao(ist)=zag*zog
       zf(ist)=zfg
       zfs(ist)=zfg*zfg
       zfo(ist)=zfg*zog
       endif
       endif
c
       if(tqg.le.3) then
       x1=tog
       x2=tag
       x3=tfg
       if((x1.ne.bmiss).and.(x2.ne.bmiss).and.(x3.ne.bmiss)) then
       ft(ist)=ft(ist)+1.
       to(ist)=tog
       tos(ist)=tog*tog
       ta(ist)=tag
       tas(ist)=tag*tag
       tao(ist)=tag*tog
       tf(ist)=tfg
       tfs(ist)=tfg*tfg
       tfo(ist)=tfg*tog
       endif
       endif
c
       if(qqg.le.3) then
       x1=qog
       x2=qag
       x3=qfg
       if((x1.ne.bmiss).and.(x2.ne.bmiss).and.(x3.ne.bmiss)) then
       fq(ist)=fq(ist)+1.
       qo(ist)=qog
       qos(ist)=qog*qog
       qa(ist)=qag
       qas(ist)=qag*qag
       qao(ist)=qag*qog
       qf(ist)=qfg
       qfs(ist)=qfg*qfg
       qfo(ist)=qfg*qog
       endif
       endif
c
       endif
c       
       if(iwnd.eq.1) then
c
       if(wqg.le.3) then
       x1=uog
       x2=uag
       x3=ufg
       y1=vog
       y2=vag
       y3=vfg
       if((x1.ne.bmiss).and.(x2.ne.bmiss).and.(x3.ne.bmiss).and.
     *    (y1.ne.bmiss).and.(y2.ne.bmiss).and.(y3.ne.bmiss)) then
       fw(ist)=fw(ist)+1.
       uo(ist)=uog
       vo(ist)=vog
       uvo(ist)=uog*uog+vog*vog
       ua(ist)=uag
       va(ist)=vag
       uva(ist)=uag*uag+vag*vag
       uvao(ist)=uag*uog+vag*vog
       spda(ist)=sqrt(uva(ist))-sqrt(uvo(ist))
       uf(ist)=ufg
       vf(ist)=vfg
       uvf(ist)=ufg*ufg+vfg*vfg
       uvfo(ist)=ufg*uog+vfg*vog
       spdf(ist)=sqrt(uvf(ist))-sqrt(uvo(ist))
       endif
       endif
c
       endif
c
       go to 100
c
       endif
c
c..   check for unique station....
       do n=1,ist
       idn=tstn(n)
c
c      id6=idn(6:6)
c      idb=' '
c      if(id6.ne.idb) then
c      print *,'duplicate station for ',idn
c      go to 100
c      endif
c
       dlon=abs(rlon-tlon(n))
       dlat=abs(rlat-tlat(n))
c
       if((dlat.le.0.01).and.(dlon.le.0.01)) then
c
       if(imas.eq.1) then
c
       if(pqg.le.3) then
       x1=pog
       x2=pag
       x3=pfg
       if((x1.ne.bmiss).and.(x2.ne.bmiss).and.(x3.ne.bmiss)) then
       fp(n)=fp(n)+1.
       if(po(n).eq.bmiss) then
       po(n)=pog
       pos(n)=pog*pog
       else
       po(n)=po(n)+pog
       pos(n)=pos(n)+pog*pog
       endif
       if(pa(n).eq.bmiss) then
       pa(n)=pag
       pas(n)=pag*pag
       pao(n)=pag*pog
       else
       pa(n)=pa(n)+pag
       pas(n)=pas(n)+pag*pag
       pao(n)=pao(n)+pag*pog
       endif
       if(pf(n).eq.bmiss) then
       pf(n)=pfg
       pfs(n)=pfg*pfg
       pfo(n)=pfg*pog
       else
       pf(n)=pf(n)+pfg
       pfs(n)=pfs(n)+pfg*pfg
       pfo(n)=pfo(n)+pfg*pog
       endif
       endif
       endif
c
       if(zqg.le.3) then
       x1=zog
       x2=zag
       x3=zfg
       if((x1.ne.bmiss).and.(x2.ne.bmiss).and.(x3.ne.bmiss)) then
       fz(n)=fz(n)+1.
       if(zo(n).eq.bmiss) then
       zo(n)=zog
       zos(n)=zog*zog
       else
       zo(n)=zo(n)+zog
       zos(n)=zos(n)+zog*zog
       endif
       if(za(n).eq.bmiss) then
       za(n)=zag
       zas(n)=zag*zag
       zao(n)=zag*zog
       else
       za(n)=za(n)+zag
       zas(n)=zas(n)+zag*zag
       zao(n)=zao(n)+zag*zog
       endif
       if(zf(n).eq.bmiss) then
       zf(n)=zfg
       zfs(n)=zfg*zfg
       zfo(n)=zfg*zog
       else
       zf(n)=zf(n)+zfg
       zfs(n)=zfs(n)+zfg*zfg
       zfo(n)=zfo(n)+zfg*zog
       endif
       endif
       endif
c
       if(tqg.le.3) then
       x1=tog
       x2=tag
       x3=tfg
       if((x1.ne.bmiss).and.(x2.ne.bmiss).and.(x3.ne.bmiss)) then
       ft(n)=ft(n)+1.
       if(to(n).eq.bmiss) then
       to(n)=tog
       tos(n)=tog*tog
       else
       to(n)=to(n)+tog
       tos(n)=tos(n)+tog*tog
       endif
       if(ta(n).eq.bmiss) then
       ta(n)=tag
       tas(n)=tag*tag
       tao(n)=tag*tog
       else
       ta(n)=ta(n)+tag
       tas(n)=tas(n)+tag*tag
       tao(n)=tao(n)+tag*tog
       endif
       if(tf(n).eq.bmiss) then
       tf(n)=tfg
       tfs(n)=tfg*tfg
       tfo(n)=tfg*tog
       else
       tf(n)=tf(n)+tfg
       tfs(n)=tfs(n)+tfg*tfg
       tfo(n)=tfo(n)+tfg*tog
       endif
       endif
       endif
c
       if(qqg.le.3) then
       x1=qog
       x2=qag
       x3=qfg
       if((x1.ne.bmiss).and.(x2.ne.bmiss).and.(x3.ne.bmiss)) then
       fq(n)=fq(n)+1.
       if(qo(n).eq.bmiss) then
       qo(n)=qog
       qos(n)=qog*qog
       else
       qo(n)=qo(n)+qog
       qos(n)=qos(n)+qog*qog
       endif
       if(qa(n).eq.bmiss) then
       qa(n)=qag
       qas(n)=qag*qag
       qao(n)=qag*qog
       else
       qa(n)=qa(n)+qag
       qas(n)=qas(n)+qag*qag
       qao(n)=qao(n)+qag*qog
       endif
       if(qf(n).eq.bmiss) then
       qf(n)=qfg
       qfs(n)=qfg*qfg
       qfo(n)=qfg*qog
       else
       qf(n)=qf(n)+qfg
       qfs(n)=qfs(n)+qfg*qfg
       qfo(n)=qfo(n)+qfg*qog
       endif
       endif
       endif
c
       endif
c
       if(iwnd.eq.1) then
c
       if(wqg.le.3) then
       x1=uog
       x2=uag
       x3=ufg
       y1=vog
       y2=vag
       y3=vfg
       if((x1.ne.bmiss).and.(x2.ne.bmiss).and.(x3.ne.bmiss).and.
     *    (y1.ne.bmiss).and.(y2.ne.bmiss).and.(y3.ne.bmiss)) then
       fw(n)=fw(n)+1.
       if((uo(n).eq.bmiss).and.(vo(n).eq.bmiss)) then
       uo(n)=uog
       vo(n)=vog
       uvo(n)=uog*uog+vog*vog
       else
       uo(n)=uo(n)+uog
       vo(n)=vo(n)+vog
       uvo(n)=uvo(n)+uog*uog+vog*vog
       endif
       if((ua(n).eq.bmiss).and.(va(n).eq.bmiss)) then
       ua(n)=uag
       va(n)=vag
       uva(n)=uag*uag+vag*vag
       uvao(n)=uag*uog+vag*vog
       spda(n)=sqrt(uva(n))-sqrt(uvo(n))
       else
       ua(n)=ua(n)+uag
       va(n)=va(n)+vag
       uva(n)=uva(n)+uag*uag+vag*vag
       uvao(n)=uvao(n)+uag*uog+vag*vog
       spda(n)=spda(n)+sqrt(uva(n))-sqrt(uvo(n))
       endif
       if((uf(n).eq.bmiss).and.(vf(n).eq.bmiss)) then
       uf(n)=ufg
       vf(n)=vfg
       uvf(n)=ufg*ufg+vfg*vfg
       uvfo(n)=ufg*uog+vfg*vog
       spdf(n)=sqrt(uvf(n))-sqrt(uvo(n))
       else
       uf(n)=uf(n)+ufg
       vf(n)=vf(n)+vfg
       uvf(n)=uvf(n)+ufg*ufg+vfg*vfg
       uvfo(n)=uvfo(n)+ufg*uog+vfg*vog
       spdf(n)=spdf(n)+sqrt(uvf(n))-sqrt(uvo(n))
       endif
       endif
       endif
c
       endif
c
       go to 100
c
       endif
       enddo
c
c...  if not, then add a new station....
       ist=ist+1
       tstn(ist)=id
       tlon(ist)=rlon
       tlat(ist)=rlat
c      print *,ist,' new id ',id
c
       if(imas.eq.1) then
c
       if(pqg.le.3) then
       x1=pog
       x2=pag
       x3=pfg
       if((x1.ne.bmiss).and.(x2.ne.bmiss).and.(x3.ne.bmiss)) then
       fp(ist)=fp(ist)+1.
       po(ist)=pog
       pos(ist)=pog*pog
       pa(ist)=pag
       pas(ist)=pag*pag
       pao(ist)=pag*pog
       pf(ist)=pfg
       pfs(ist)=pfg*pfg
       pfo(ist)=pfg*pog
       endif
       endif
c
       if(zqg.le.3) then
       x1=zog
       x2=zag
       x3=zfg
       if((x1.ne.bmiss).and.(x2.ne.bmiss).and.(x3.ne.bmiss)) then
       fz(ist)=fz(ist)+1.
       zo(ist)=zog
       zos(ist)=zog*zog
       za(ist)=zag
       zas(ist)=zag*zag
       zao(ist)=zag*zog
       zf(ist)=zfg
       zfs(ist)=zfg*zfg
       zfo(ist)=zfg*zog
       endif
       endif
c
       if(tqg.le.3) then
       x1=tog
       x2=tag
       x3=tfg
       if((x1.ne.bmiss).and.(x2.ne.bmiss).and.(x3.ne.bmiss)) then
       ft(ist)=ft(ist)+1.
       to(ist)=tog
       tos(ist)=tog*tog
       ta(ist)=tag
       tas(ist)=tag*tag
       tao(ist)=tag*tog
       tf(ist)=tfg
       tfs(ist)=tfg*tfg
       tfo(ist)=tfg*tog
       endif
       endif
c
       if(qqg.le.3) then
       x1=qog
       x2=qag
       x3=qfg
       if((x1.ne.bmiss).and.(x2.ne.bmiss).and.(x3.ne.bmiss)) then
       fq(ist)=fq(ist)+1.
       qo(ist)=qog
       qos(ist)=qog*qog
       qa(ist)=qag
       qas(ist)=qag*qag
       qao(ist)=qag*qog
       qf(ist)=qfg
       qfs(ist)=qfg*qfg
       qfo(ist)=qfg*qog
       endif
       endif
c
       endif
c       
       if(iwnd.eq.1) then
c
       if(wqg.le.3) then
       x1=uog
       x2=uag
       x3=ufg
       y1=vog
       y2=vag
       y3=vfg
       if((x1.ne.bmiss).and.(x2.ne.bmiss).and.(x3.ne.bmiss).and.
     *    (y1.ne.bmiss).and.(y2.ne.bmiss).and.(y3.ne.bmiss)) then
       fw(ist)=fw(ist)+1.
       uo(ist)=uog
       vo(ist)=vog
       uvo(ist)=uog*uog+vog*vog
       ua(ist)=uag
       va(ist)=vag
       uva(ist)=uag*uag+vag*vag
       uvao(ist)=uag*uog+vag*vog
       spda(ist)=sqrt(uva(ist))-sqrt(uvo(ist))
       uf(ist)=ufg
       vf(ist)=vfg
       uvf(ist)=ufg*ufg+vfg*vfg
       uvfo(ist)=ufg*uog+vfg*vog
       spdf(ist)=sqrt(uvf(ist))-sqrt(uvo(ist))
       endif
       endif
c
       endif
c
       go to 100
c
c... now read a new date in..
 101   continue
       close(11)
c
       if(idbug.eq.1) then
       istn=idprstn
       id=tstn(istn)
       rlon=tlon(istn)
       rlat=tlat(istn)
       if(imas.eq.1) then
       write(6,611) istn,indate,id,rlon,rlat,
     * fp(istn),po(istn),pa(istn),pf(istn)
       write(6,711) istn,indate,id,rlon,rlat,
     * pos(istn),pas(istn),pfs(istn),pao(istn),pfo(istn)
       write(6,612) istn,indate,id,rlon,rlat,
     * fz(istn),zo(istn),za(istn),zf(istn)
       write(6,712) istn,indate,id,rlon,rlat,
     * zos(istn),zas(istn),zfs(istn),zao(istn),zfo(istn)
       write(6,613) istn,indate,id,rlon,rlat,
     * ft(istn),to(istn),ta(istn),tf(istn)
       write(6,713) istn,indate,id,rlon,rlat,
     * tos(istn),tas(istn),tfs(istn),tao(istn),tfo(istn)
       write(6,614) istn,indate,id,rlon,rlat,
     * fq(istn),qo(istn),qa(istn),qf(istn)
       write(6,714) istn,indate,id,rlon,rlat,
     * qos(istn),qas(istn),qfs(istn),qao(istn),qfo(istn)
       endif
       if(iwnd.eq.1) then
       write(6,615) istn,indate,id,rlon,rlat,
     * fw(istn),uo(istn),ua(istn),uf(istn)
       write(6,616) istn,indate,id,rlon,rlat,
     * fw(istn),vo(istn),va(istn),vf(istn)
       write(6,715) istn,indate,id,rlon,rlat,
     * uvo(istn),uva(istn),uvf(istn),uvao(istn),uvfo(istn)
       write(6,716) istn,indate,id,rlon,rlat,
     * spda(istn),spdf(istn)
       endif
       endif
c
 444   continue
c
 555   continue
c
       nstns=ist
       print *,'total number of stations is ',nstns
c
c... compute time means
c
       do ist=1,nstns
c
       if(imas.eq.1) then
c
       if(fp(ist).gt.0.) then
       fip=1.0/fp(ist)
       if(po(ist).ne.bmiss) po(ist)=po(ist)*fip
       x1=pos(ist)
       if(x1.ne.bmiss) pos(ist)=pos(ist)*fip
       if(pa(ist).ne.bmiss) pa(ist)=pa(ist)*fip
       x2=pas(ist)
       if(x2.ne.bmiss) pas(ist)=pas(ist)*fip
       x3=pao(ist)
       if(x3.ne.bmiss) pao(ist)=pao(ist)*fip
       if((x2.ne.bmiss).and.(x1.ne.bmiss).and.(x3.ne.bmiss)) then
       val=pas(ist)+pos(ist)-2*pao(ist)
       if(val.gt.0.) pae(ist)=sqrt(val)
       endif
       if(pf(ist).ne.bmiss) pf(ist)=pf(ist)*fip
       x2=pfs(ist)
       if(x2.ne.bmiss) pfs(ist)=pfs(ist)*fip
       x3=pfo(ist)
       if(x3.ne.bmiss) pfo(ist)=pfo(ist)*fip
       if((x2.ne.bmiss).and.(x1.ne.bmiss).and.(x3.ne.bmiss)) then
       val=pfs(ist)+pos(ist)-2*pfo(ist)
       if(val.gt.0.) pfe(ist)=sqrt(val)
       endif
       endif
c 
       if(fz(ist).gt.0.) then
       fiz=1.0/fz(ist)
       if(zo(ist).ne.bmiss) zo(ist)=zo(ist)*fiz
       x1=zos(ist)
       if(x1.ne.bmiss) zos(ist)=zos(ist)*fiz
       if(za(ist).ne.bmiss) za(ist)=za(ist)*fiz
       x2=zas(ist)
       if(x2.ne.bmiss) zas(ist)=zas(ist)*fiz
       x3=zao(ist)
       if(x3.ne.bmiss) zao(ist)=zao(ist)*fiz
       if((x2.ne.bmiss).and.(x1.ne.bmiss).and.(x3.ne.bmiss)) then
       val=zas(ist)+zos(ist)-2*zao(ist)
       if(val.gt.0.) zae(ist)=sqrt(val)
       endif
       if(zf(ist).ne.bmiss) zf(ist)=zf(ist)*fiz
       x2=zfs(ist)
       if(x2.ne.bmiss) zfs(ist)=zfs(ist)*fiz
       x3=zfo(ist)
       if(x3.ne.bmiss) zfo(ist)=zfo(ist)*fiz
       if((x2.ne.bmiss).and.(x1.ne.bmiss).and.(x3.ne.bmiss)) then
       val=zfs(ist)+zos(ist)-2*zfo(ist)
       if(val.gt.0.) zfe(ist)=sqrt(val)
       endif
       endif
c 
       if(ft(ist).gt.0.) then
       fit=1.0/ft(ist)
       if(to(ist).ne.bmiss) to(ist)=to(ist)*fit
       x1=tos(ist)
       if(x1.ne.bmiss) tos(ist)=tos(ist)*fit
       if(ta(ist).ne.bmiss) ta(ist)=ta(ist)*fit
       x2=tas(ist)
       if(x2.ne.bmiss) tas(ist)=tas(ist)*fit
       x3=tao(ist)
       if(x3.ne.bmiss) tao(ist)=tao(ist)*fit
       if((x2.ne.bmiss).and.(x1.ne.bmiss).and.(x3.ne.bmiss)) then
       val=tas(ist)+tos(ist)-2*tao(ist)
       if(val.gt.0.) tae(ist)=sqrt(val)
       endif
       if(tf(ist).ne.bmiss) tf(ist)=tf(ist)*fit
       x2=tfs(ist)
       if(x2.ne.bmiss) tfs(ist)=tfs(ist)*fit
       x3=tfo(ist)
       if(x3.ne.bmiss) tfo(ist)=tfo(ist)*fit
       if((x2.ne.bmiss).and.(x1.ne.bmiss).and.(x3.ne.bmiss)) then
       val=tfs(ist)+tos(ist)-2*tfo(ist)
       if(val.gt.0.) tfe(ist)=sqrt(val)
       endif
       endif
c 
       if(fq(ist).gt.0.) then
       fiq=1.0/fq(ist)
       if(qo(ist).ne.bmiss) qo(ist)=qo(ist)*fiq
       x1=qos(ist)
       if(x1.ne.bmiss) qos(ist)=qos(ist)*fiq
       if(qa(ist).ne.bmiss) qa(ist)=qa(ist)*fiq
       x2=qas(ist)
       if(x2.ne.bmiss) qas(ist)=qas(ist)*fiq
       x3=qao(ist)
       if(x3.ne.bmiss) qao(ist)=qao(ist)*fiq
       if((x2.ne.bmiss).and.(x1.ne.bmiss).and.(x3.ne.bmiss)) then
       val=qas(ist)+qos(ist)-2*qao(ist)
       if(val.gt.0.) qae(ist)=sqrt(val)
       endif
       if(qf(ist).ne.bmiss) qf(ist)=qf(ist)*fiq
       x2=qfs(ist)
       if(x2.ne.bmiss) qfs(ist)=qfs(ist)*fiq
       x3=qfo(ist)
       if(x3.ne.bmiss) qfo(ist)=qfo(ist)*fiq
       if((x2.ne.bmiss).and.(x1.ne.bmiss).and.(x3.ne.bmiss)) then
       val=qfs(ist)+qos(ist)-2*qfo(ist)
       if(val.gt.0.) qfe(ist)=sqrt(val)
       endif
       endif
c 
       endif
c 
       if(iwnd.eq.1) then
c
       if(fw(ist).gt.0.) then
       fiw=1.0/fw(ist)
       if(uo(ist).ne.bmiss) uo(ist)=uo(ist)*fiw
       if(vo(ist).ne.bmiss) vo(ist)=vo(ist)*fiw
       x1=uvo(ist)
       if(x1.ne.bmiss) uvo(ist)=uvo(ist)*fiw
       if(ua(ist).ne.bmiss) ua(ist)=ua(ist)*fiw
       if(va(ist).ne.bmiss) va(ist)=va(ist)*fiw
       x2=uva(ist)
       if(x2.ne.bmiss) uva(ist)=uva(ist)*fiw
       x3=uvao(ist)
       if(x3.ne.bmiss) uvao(ist)=uvao(ist)*fiw
       if((x2.ne.bmiss).and.(x1.ne.bmiss).and.(x3.ne.bmiss)) then
       val=uva(ist)+uvo(ist)-2*uvao(ist)
       if(val.gt.0.) wae(ist)=sqrt(val)
       endif
       if(spda(ist).ne.bmiss) spda(ist)=spda(ist)*fiw
       if(uf(ist).ne.bmiss) uf(ist)=uf(ist)*fiw
       if(vf(ist).ne.bmiss) vf(ist)=vf(ist)*fiw
       x2=uvf(ist)
       if(x2.ne.bmiss) uvf(ist)=uvf(ist)*fiw
       x3=uvfo(ist)
       if(x3.ne.bmiss) uvfo(ist)=uvfo(ist)*fiw
       if((x2.ne.bmiss).and.(x1.ne.bmiss).and.(x3.ne.bmiss)) then
       val=uvf(ist)+uvo(ist)-2*uvfo(ist)
       if(val.gt.0.) wfe(ist)=sqrt(val)
       endif
       if(spdf(ist).ne.bmiss) spdf(ist)=spdf(ist)*fiw
       endif
c
       endif
c
       enddo
c
c... now write grads file out
c
c      if(idbug.eq.1) write(*,*) "outfile= ",outfile
       open(51,file=outfile,form='unformatted')
c
       do ist=1,nstns
c
       if(imas.eq.1) then
       pcg=bmiss
       pog=bmiss
       pag=bmiss
       pfg=bmiss
       paeg=bmiss
       pfeg=bmiss
       zcg=bmiss
       zog=bmiss
       zag=bmiss
       zfg=bmiss
       zaeg=bmiss
       zfeg=bmiss
       tcg=bmiss
       tog=bmiss
       tag=bmiss
       tfg=bmiss
       taeg=bmiss
       tfeg=bmiss
       qcg=bmiss
       qog=bmiss
       qag=bmiss
       qfg=bmiss
       qaeg=bmiss
       qfeg=bmiss
       endif
       if(iwnd.eq.1) then
       uog=bmiss
       uag=bmiss
       ufg=bmiss
       vog=bmiss
       vag=bmiss
       vfg=bmiss
       wcg=bmiss
       waeg=bmiss
       wfeg=bmiss
       spdag=bmiss
       spdfg=bmiss
       endif
c
       if(imas.eq.1) then
       if(id.eq.idpr)print *,id,pcg,pog,pag,pfg,paeg,pfeg
       pcg=fp(ist)
       pog=po(ist)
       pag=pa(ist)
       pfg=pf(ist)
       paeg=pae(ist)
       pfeg=pfe(ist)
       zcg=fz(ist)
       zog=zo(ist)
       zag=za(ist)
       zfg=zf(ist)
       zaeg=zae(ist)
       zfeg=zfe(ist)
       tcg=ft(ist)
       tog=to(ist)
       tag=ta(ist)
       tfg=tf(ist)
       taeg=tae(ist)
       tfeg=tfe(ist)
       qcg=fq(ist)
       qog=qo(ist)
       qag=qa(ist)
       qfg=qf(ist)
       qaeg=qae(ist)
       qfeg=qfe(ist)
       endif
       if(iwnd.eq.1) then
       wcg=fw(ist)
       uog=uo(ist)
       uag=ua(ist)
       ufg=uf(ist)
       vog=vo(ist)
       vag=va(ist)
       vfg=vf(ist)
       waeg=wae(ist)
       wfeg=wfe(ist)
       spdag=spda(ist)
       spdfg=spdf(ist)
       endif
c
       id=tstn(ist)
       rlon=tlon(ist)
       rlat=tlat(ist)
       typx=typ(ist)
       rt=0.
       nl=1
       iflag=1
c
       write(51) id,rlat,rlon,rt,nl,iflag
       if((imas.eq.1).and.(iwnd.eq.1)) then
       write(51)  typ(ist),
     *            pcg,pog,pag,pfg,paeg,pfeg,
     *            zcg,zog,zag,zfg,zaeg,zfeg,
     *            tcg,tog,tag,tfg,taeg,tfeg,
     *            qcg,qog,qag,qfg,qaeg,qfeg,
     *            wcg,uog,uag,ufg,
     *            vog,vag,vfg,
     *            waeg,wfeg,spdag,spdfg,bl
        endif
       if((imas.eq.1).and.(iwnd.eq.0)) then
       write(51)  typ(ist),
     *            pcg,pog,pag,pfg,paeg,pfeg,
     *            zcg,zog,zag,zfg,zaeg,zfeg,
     *            tcg,tog,tag,tfg,taeg,tfeg,
     *            qcg,qog,qag,qfg,qaeg,qfeg,bl
        endif
       if((imas.eq.0).and.(iwnd.eq.1)) then
       write(51)  typ(ist),
     *            wcg,uog,uag,ufg,
     *            vog,vag,vfg,
     *            waeg,wfeg,spdag,spdfg,bl
        endif
c
       if(idbug.eq.1) then
       if(id.eq.idpr) then
       if(imas.eq.1) then
       write(6,811) ist,id,rlon,rlat,typx,pog,pag,pfg
       write(6,812) ist,id,rlon,rlat,typx,zog,zag,zfg
       write(6,813) ist,id,rlon,rlat,typx,tog,tag,tfg
       write(6,814) ist,id,rlon,rlat,typx,qog,qag,qfg
       write(6,911) ist,id,rlon,rlat,typx,pcg,paeg,pfeg
       write(6,912) ist,id,rlon,rlat,typx,zcg,zaeg,zfeg
       write(6,913) ist,id,rlon,rlat,typx,tcg,taeg,tfeg
       write(6,914) ist,id,rlon,rlat,typx,qcg,qaeg,qfeg
       endif
       if(iwnd.eq.1) then
       write(6,815) ist,id,rlon,rlat,typx,uog,uag,ufg
       write(6,816) ist,id,rlon,rlat,typx,vog,vag,vfg
       write(6,915) ist,id,rlon,rlat,typx,wcg,waeg,wfeg
       write(6,916) ist,id,rlon,rlat,typx,wcg,spdag,spdfg
       endif
       endif
       endif
c
c... end station-loop
       enddo
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
 511   format(i6,1x,a8,1x,a8,1x,f6.1,1x,f6.1,1x,f5.0,1x,f5.2,1x,
     * 'p',1x,3f10.2,1x,'qf ',f4.0)
 512   format(i6,1x,a8,1x,a8,1x,f6.1,1x,f6.1,1x,f5.0,1x,f5.2,1x,
     * 'm',1x,3f10.2,1x,'qf ',f4.0)
 513   format(i6,1x,a8,1x,a8,1x,f6.1,1x,f6.1,1x,f5.0,1x,f5.2,1x,
     * 't',1x,3f10.2,1x,'qf ',f4.0)
 514   format(i6,1x,a8,1x,a8,1x,f6.1,1x,f6.1,1x,f5.0,1x,f5.2,1x,
     * 'q',1x,3f10.2,1x,'qf ',f4.0)
 515   format(i6,1x,a8,1x,a8,1x,f6.1,1x,f6.1,1x,f5.0,1x,f5.2,1x,
     * 'u',1x,3f10.2,1x,'qf ',f4.0)
 516   format(i6,1x,a8,1x,a8,1x,f6.1,1x,f6.1,1x,f5.0,1x,f5.2,1x,
     * 'v',1x,3f10.2)
c
 611   format(i6,1x,a8,1x,a8,1x,f6.1,1x,f6.1,1x,
     * 'pm',1x,4f10.2)
 612   format(i6,1x,a8,1x,a8,1x,f6.1,1x,f6.1,1x,
     * 'mm',1x,4f10.2)
 613   format(i6,1x,a8,1x,a8,1x,f6.1,1x,f6.1,1x,
     * 'tm',1x,4f10.2)
 614   format(i6,1x,a8,1x,a8,1x,f6.1,1x,f6.1,1x,
     * 'qm',1x,4f10.2)
 615   format(i6,1x,a8,1x,a8,1x,f6.1,1x,f6.1,1x,
     * 'um',1x,4f10.2)
 616   format(i6,1x,a8,1x,a8,1x,f6.1,1x,f6.1,1x,
     * 'vm',1x,4f10.2)
c
 711   format(i6,1x,a8,1x,a8,1x,f6.1,1x,f6.1,1x,
     * 'pe',1x,5f10.2)
 712   format(i6,1x,a8,1x,a8,1x,f6.1,1x,f6.1,1x,
     * 'me',1x,5f10.2)
 713   format(i6,1x,a8,1x,a8,1x,f6.1,1x,f6.1,1x,
     * 'te',1x,5f10.2)
 714   format(i6,1x,a8,1x,a8,1x,f6.1,1x,f6.1,1x,
     * 'qe',1x,5f10.2)
 715   format(i6,1x,a8,1x,a8,1x,f6.1,1x,f6.1,1x,
     * 'we',1x,5f10.2)
 716   format(i6,1x,a8,1x,a8,1x,f6.1,1x,f6.1,1x,
     * 'ws',1x,2f10.2)
c
 811   format(i6,1x,a8,1x,f6.1,1x,f6.1,1x,f5.0,1x,
     * 'pm',1x,3f10.2)
 812   format(i6,1x,a8,1x,f6.1,1x,f6.1,1x,f5.0,1x,
     * 'mm',1x,3f10.2)
 813   format(i6,1x,a8,1x,f6.1,1x,f6.1,1x,f5.0,1x,
     * 'tm',1x,3f10.2)
 814   format(i6,1x,a8,1x,f6.1,1x,f6.1,1x,f5.0,1x,
     * 'qm',1x,3f10.2)
 815   format(i6,1x,a8,1x,f6.1,1x,f6.1,1x,f5.0,1x,
     * 'um',1x,3f10.2)
 816   format(i6,1x,a8,1x,f6.1,1x,f6.1,1x,f5.0,1x,
     * 'vm',1x,3f10.2)
c
 911   format(i6,1x,a8,1x,f6.1,1x,f6.1,1x,f5.0,1x,
     * 'pe',1x,3f10.2)
 912   format(i6,1x,a8,1x,f6.1,1x,f6.1,1x,f5.0,1x,
     * 'me',1x,3f10.2)
 913   format(i6,1x,a8,1x,f6.1,1x,f6.1,1x,f5.0,1x,
     * 'te',1x,3f10.2)
 914   format(i6,1x,a8,1x,f6.1,1x,f6.1,1x,f5.0,1x,
     * 'qe',1x,3f10.2)
 915   format(i6,1x,a8,1x,f6.1,1x,f6.1,1x,f5.0,1x,
     * 'we',1x,3f10.2)
 916   format(i6,1x,a8,1x,f6.1,1x,f6.1,1x,f5.0,1x,
     * 'ws',1x,3f10.2)
c
       return
       end
