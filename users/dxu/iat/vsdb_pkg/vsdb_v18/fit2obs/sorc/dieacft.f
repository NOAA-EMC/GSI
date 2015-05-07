       parameter(nvar=4,nlev=3,nstcs=6,nstcw=9)
       parameter(nreg=7,nsub=1,ntx=1000)
c      parameter(nreg=2,nsub=1,ntx=100)
       parameter(iprreg=1,iprsub=1)
c
       CHARACTER*1  kdbug
       CHARACTER*250 mdl,mdldir,fcshr,iname,infile
       CHARACTER*250 outfile,outdir
       CHARACTER*10 ksyr,ksmth,ksday,kscy,kincr,type,ityp
       CHARACTER*10 keyr,kemth,keday,kecy
       CHARACTER*10 indate,datstr,datend
       CHARACTER*2 labm(12),labd(31),labc(4)
       CHARACTER*4 laby
       integer mand(21)
c
       dimension sprs(nreg,nsub,nlev,nstcw,nvar,ntx)
       dimension gdata(nreg,nsub)
       dimension fm(nreg,nsub,nlev)
       dimension om(nreg,nsub,nlev)
       dimension fom(nreg,nsub,nlev)
       dimension fsm(nreg,nsub,nlev)
       dimension osm(nreg,nsub,nlev)
       dimension tcnt(nreg,nsub,nlev)
       dimension rmse(nreg,nsub,nlev)
       dimension bias(nreg,nsub,nlev)
       dimension ufm(nreg,nsub,nlev)
       dimension vfm(nreg,nsub,nlev)
       dimension uom(nreg,nsub,nlev)
       dimension vom(nreg,nsub,nlev)
       dimension uvm(nreg,nsub,nlev)
       dimension uvfm(nreg,nsub,nlev)
       dimension uvom(nreg,nsub,nlev)
       dimension spdm(nreg,nsub,nlev)
       dimension icycl(4)

       real(4),allocatable,dimension(:,:,:,:,:)    :: out   
       real(4),allocatable,dimension(:)            :: xcnt,xrms,xavg
c
       DATA LABC/'00','06','12','18'/
       DATA icycl/0,6,12,18/
C
       DATA LABD/'01','02','03','04','05','06','07','08','09','10',
     *           '11','12','13','14','15','16','17','18','19','20',
     *           '21','22','23','24','25','26','27','28','29','30',
     *           '31'/
C
       DATA LABM/'01','02','03','04','05','06','07','08','09','10',
     *           '11','12'/
C
       DATA MAND / 1000, 925, 850, 700, 500, 400, 300,
     *               250, 200, 150, 100,  70,  50,  30,
     *                20,  10,   7,   5,   3,   2,   1/
c
       data undef/0.0/
c
       call getenv("idbug",kdbug)
       read(kdbug,'(i1)') idbug
       write(*,*) "idbug= ",idbug
c
       call getenv("mdl",mdl)
       write(*,*) "mdl= ",mdl
c
       call getenv("mdldir",mdldir)
       write(*,*) "mdldir= ",mdldir
c
       call getenv("outdir",outdir)
       write(*,*) "outdir= ",outdir
c
       call getenv("fcshr",fcshr)
       write(*,*) "fcshr= ",fcshr
c
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
       call getenv("incr",kincr)
       read(kincr,'(i2)') incr
       write(*,*) "incr= ",incr
c
       iname = trim(mdldir)//'/f'//trim(fcshr)//'.acft.' 
       write(*,*) "iname= ",iname
c
       ncns=iw3jdn(isyr,ismth,isday)
       print *,' ncns ',ncns
       ncne=iw3jdn(ieyr,iemth,ieday)
       print *,' ncne ',ncne
       ndays=ncne-ncns+1
       print *,' ndays ',ndays
c
       if(iscy.eq.0) ncysx=1
       if(iscy.eq.6) ncysx=2
       if(iscy.eq.12) ncysx=3
       if(iscy.eq.18) ncysx=4
       ncysi=ncysx
c
       if(iecy.eq.0) ncyex=1
       if(iecy.eq.6) ncyex=2
       if(iecy.eq.12) ncyex=3
       if(iecy.eq.18) ncyex=4
       ncyei=ncyex
c
       if(incr.eq.24) then
       jincr=1
       ncyex=ncysx
       endif
       if(incr.eq.12) then
       jincr=2
       ncyex=ncysx+2
       endif
       if(incr.eq.6) then
       jincr=1
       ncysx=1
       ncyex=4
       endif
c
C      START THE TIME LOOP HERE...
C
       ntime=0
       sprs=0

       DO 555 NCN=NCNS,NCNE
C
       CALL W3FS26(NCN,IYR,IMTH,IDAY,IDAYWK,IDAYYR)
       WRITE(LABY,'(I4)') IYR
       indate(1:4)=LABY
       indate(5:6)=LABM(IMTH)
       indate(7:8)=LABD(IDAY)
C
       ncysf=ncysx
       ncyef=ncyex
       if(ncn.eq.ncns) ncysf=ncysi
       if(ncn.eq.ncne) ncyef=ncyei
c
c      print *,'ncysx ',ncysx,' ncyex ',ncyex,' jincr ',jincr
c
       DO 444 ncy=ncysf,ncyef,jincr
       ICY=icycl(ncy)
C
       IF(ICY.EQ.0)  indate(9:10)=LABC(1)
       IF(ICY.EQ.6)  indate(9:10)=LABC(2)
       IF(ICY.EQ.12) indate(9:10)=LABC(3)
       IF(ICY.EQ.18) indate(9:10)=LABC(4)
C
       infile = trim(iname)//indate
       write(*,*) "infile= ",infile
!!     open(11,file=infile,form='unformatted',err=445)
       ntime=ntime+1
       call opendian(11,infile,ierr)
       if(ierr.ne.0) goto 445
c
c.... read the full data set in....
      ntxx=0
      do ivar=1,nvar
c
      if(ivar.eq.1) nstat=nstcs
      if(ivar.eq.2) nstat=nstcs
      if(ivar.eq.3) nstat=nstcw
      if(ivar.eq.4) nstat=nstcs
c
      do nst=1,nstat
      do ilev=1,nlev
c
      read(11,end=445,err=445) gdata
      ntxx=ntxx+1
c
      do isub=1,nsub
      do ireg=1,nreg
      sprs(ireg,isub,ilev,nst,ivar,ntime)=gdata(ireg,isub)
       if( nst.eq.2 .and. abs(gdata(ireg,isub)) .ge. 1.0E+6) then
        sprs(ireg,isub,ilev,nst,ivar,ntime)=undef
        sprs(ireg,isub,ilev,1,ivar,ntime)=undef
!!      stop    ! Suru opposes to skip missing data. Let job fail
       endif
      enddo
      enddo
c
c... end level-loop
      enddo
c... end stat-loop
      enddo
c... end variable-loop
      enddo
c
      close(11)
      go to 444
c
 445   print *,'end of file for ',infile
c... end cycle-loop
 444  continue
c
c... end day-loop
 555  continue
c
       print *,'number of time levels ',ntime
c
c... output file name
       write(LABY,'(I4)') ISYR
       datstr(1:4)=LABY
       datstr(5:6)=LABM(ISMTH)
       datstr(7:8)=LABD(ISDAY)
       if(iscy.eq.0) datstr(9:10)=LABC(1)
       if(iscy.eq.6) datstr(9:10)=LABC(2)
       if(iscy.eq.12) datstr(9:10)=LABC(3)
       if(iscy.eq.18) datstr(9:10)=LABC(4)
c
       write(LABY,'(I4)') IEYR
       datend(1:4)=LABY
       datend(5:6)=LABM(IEMTH)
       datend(7:8)=LABD(IEDAY)
       if(iecy.eq.0) datend(9:10)=LABC(1)
       if(iecy.eq.6) datend(9:10)=LABC(2)
       if(iecy.eq.12) datend(9:10)=LABC(3)
       if(iecy.eq.18) datend(9:10)=LABC(4)
       outfile = trim(outdir)//'/'//trim(mdl) //
     * '.f'//trim(fcshr)//'.acft.'// 
     * datstr//'.'//datend
       write(*,*) "outfile= ",outfile
       open(51,file=outfile,form='unformatted')

!  allocate arrays for the output data

       allocate(out(nreg,0:ntime,nlev,nvar,3));out=0
       allocate(xcnt(ntime))
       allocate(xrms(ntime))
       allocate(xavg(ntime))
c
c... store ensembles of count, rmse, and mean 
c
      do ivar=1,nvar
      do ilev=1,nlev
      do isub=1,nsub
      do ireg=1,nreg
      do nt=1,ntime
      cnt=sprs(ireg,isub,ilev,1,ivar,nt)
      if(ivar==3) then
         uf=sprs(ireg,isub,ilev,2,ivar,nt)
         vf=sprs(ireg,isub,ilev,3,ivar,nt)
         uo=sprs(ireg,isub,ilev,4,ivar,nt)
         vo=sprs(ireg,isub,ilev,5,ivar,nt)
         uv=sprs(ireg,isub,ilev,6,ivar,nt)
         uvf=sprs(ireg,isub,ilev,7,ivar,nt)
         uvo=sprs(ireg,isub,ilev,8,ivar,nt)
         spd=sprs(ireg,isub,ilev,9,ivar,nt)
         xcnt(nt) = cnt
         xrms(nt) = sqrt(uvf+uvo-2*uv) 
         xavg(nt) = spd 
      else
         f=sprs(ireg,isub,ilev,2,ivar,nt)
         o=sprs(ireg,isub,ilev,3,ivar,nt)
         fo=sprs(ireg,isub,ilev,4,ivar,nt)
         fs=sprs(ireg,isub,ilev,5,ivar,nt)
         os=sprs(ireg,isub,ilev,6,ivar,nt)
         xcnt(nt) = cnt
         xrms(nt) = sqrt(os+fs-2*fo)
         xavg(nt) = f-o                
      endif
      out(ireg,nt,ilev,ivar,1) = xcnt(nt)
      out(ireg,nt,ilev,ivar,2) = xrms(nt)
      out(ireg,nt,ilev,ivar,3) = xavg(nt)
      enddo 

      ! add the ensemble count, rmse, and, mean
      cnt = ntime !!sum(xcnt(:))/float(ntime)
      rms = sqrt(sum(xrms(:)**2*xcnt(:))/sum(xcnt(:)))
      avg = sum(xavg(:)*xcnt(:))/sum(xcnt(:))
      out(ireg,00,ilev,ivar,1) = cnt
      out(ireg,00,ilev,ivar,2) = rms
      out(ireg,00,ilev,ivar,3) = avg

      enddo; enddo; enddo; enddo 

!  write the dieoff data out
!  -------------------------
      
      do ivar=1,nvar
      do ista=1,3      
      do ilev=1,nlev
      write(51)out(:,:,ilev,ivar,ista)
      enddo;enddo;enddo

      stop
      end
