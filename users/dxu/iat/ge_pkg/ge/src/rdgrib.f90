!
! This file reads a pgbgrib (grib1) file and saves the requested
! variables into respective three dimensional arrays for further use 
!
! Author: v. krishna kumar ncep/central operations/sib september 2010 
! Update history :
!   7/9/2014, d. xu / rti@jcsda, fixed variable initialization issue.
!
!.... use fcmpsp to compile....
!     kpds         integer (200) unpacked pds parameters
!     kgds         integer (200) unpacked gds parameters
!     jpds         integer (200) pds parameters for which to search
!                  (=-1 for wildcard)
!          (1)   - id of center
!          (2)   - generating process id number
!          (3)   - grid definition
!          (4)   - gds/bms flag (right adj copy of octet 8)
!          (5)   - indicator of parameter
!          (6)   - type of level
!          (7)   - height/pressure , etc of level
!          (8)   - year including (century-1)
!          (9)   - month of year
!          (10)  - day of month
!          (11)  - hour of day
!          (12)  - minute of hour
!          (13)  - indicator of forecast time unit
!          (14)  - time range 1
!          (15)  - time range 2
!          (16)  - time range flag
!          (17)  - number included in average
!          (18)  - version nr of grib specification
!          (19)  - version nr of parameter table
!          (20)  - nr missing from average/accumulation
!          (21)  - century of reference time of data
!          (22)  - units decimal scale factor
!          (23)  - subcenter number
!          (24)  - pds byte 29, for nmc ensemble products
!                  128 if forecast field error
!                   64 if bias corrected fcst field
!                   32 if smoothed field
!                  warning: can be combination of more than 1
!          (25)  - pds byte 30, not used
!     jgds         integer (200) gds parameters for which to search
!                  (only searched if jpds(3)=255)
!                  (=-1 for wildcard)
!          (1)   - data representation type
!          (19)  - number of vertical coordinate parameters
!          (20)  - octet number of the list of vertical coordinate
!                  parameters
!                  or
!                  octet number of the list of numbers of points
!                  in each row
!                  or
!                  255 if neither are present
!          (21)  - for grids with pl, number of points in grid
!          (22)  - number of words in each row
!       latitude/longitude grids
!          (2)   - n(i) nr points on latitude circle
!          (3)   - n(j) nr points on longitude meridian
!          (4)   - la(1) latitude of origin
!          (5)   - lo(1) longitude of origin
!          (6)   - resolution flag (right adj copy of octet 17)
!          (7)   - la(2) latitude of extreme point
!          (8)   - lo(2) longitude of extreme point
!          (9)   - di longitudinal direction of increment
!          (10)  - dj latitudinal direction increment
!          (11)  - scanning mode flag (right adj copy of octet 28)
!
!   output arguments of getgb:
!     kf           integer number of data points unpacked
!     k            integer message number unpacked
!                  (can be same as j in calling program
!                  in order to facilitate multiple searches)
!     kpds         integer (200) unpacked pds parameters
!     kgds         integer (200) unpacked gds parameters
!     lb           logical*1 (kf) unpacked bitmap if present
!     f            real (kf) unpacked data
!     iret         integer return code
!                    0      all ok
!                    96     error reading index file
!                    97     error reading grib file
!                    98     number of data points greater than jf
!                    99     request not found
!                    other  w3fi63 grib unpacker return code
!******************************************************
character (len=80) :: input_crnt_anl, input_ref_anl, input_gfs_ges
character*4 cleva,clevs(14)  
!
real, allocatable, dimension(:,:,:) :: hgt, temp, uwind, vwind, rh   ! ncep anl
real, allocatable, dimension(:,:,:) :: hgte, tempe, uwinde, vwinde, rhe ! ecmwf anl
real, allocatable, dimension(:,:,:) :: hgtg, tempg, uwindg, vwindg, rhg   ! ncep ges
real, allocatable, dimension(:,:,:) :: hgtd, tempd, windd, rhd  ! ncep - ecmwf anl
real, allocatable, dimension(:,:,:) :: hgtdg, tempdg, winddg, rhdg  ! ncep anl - ges
real, allocatable, dimension(:,:,:) :: hgtdeg, tempdeg, winddeg, rhdeg  ! ecmwf anl - ges
real, allocatable, dimension(:,:) :: spressa, spresse,spressg   ! spressa holds ncep analysis surf p 
real, allocatable, dimension(:,:) :: presstl, presszl  ! are limits pressures for temps and heights to be used
! spresse will be missing for ecmwf and spressg holds guess
real, allocatable, dimension(:) :: tbias,zbias,wbias,trms,zrms,wrms
dimension nlevs(14)
integer :: ipdifamax=0, jpdifamax=0
integer :: ipdifemax=0, jpdifemax=0
!
dimension wlat(181)
dimension pwgtz(14),pwgtt(14),pwgtw(14),play(14)
dimension pwgtza(14),pwgtta(14),pwgtwa(14)
integer :: ilev,i,j,k,n,nr,nlev, ii, ll, status, ierr, leng, lu
integer zext1i(20000),zext1j(20000),zext1k(20000),zext1p(20000)
dimension zext1id(20000),zext1vd(20000),zext1gd(20000)
dimension zext1mx(20000),zext1mn(20000)
integer zext2i(20000),zext2j(20000),zext2k(20000),zext2p(20000)
dimension zext2id(20000),zext2vd(20000),zext2gd(20000)
dimension zext2mx(20000),zext2mn(20000)
integer text1i(20000),text1j(20000),text1k(20000),text1p(20000)
dimension text1id(20000),text1vd(20000),text1gd(20000)
dimension text1mx(20000),text1mn(20000)
integer text2i(20000),text2j(20000),text2k(20000),text2p(20000)
dimension text2id(20000),text2vd(20000),text2gd(20000)
dimension text2mx(20000),text2mn(20000)
integer wext1i(20000),wext1j(20000),wext1k(20000),wext1p(20000)
dimension wext1id(20000),wext1vd(20000),wext1gd(20000)
dimension wext1mx(20000),wext1mn(20000)
integer wext2i(20000),wext2j(20000),wext2k(20000),wext2p(20000)
dimension wext2id(20000),wext2vd(20000),wext2gd(20000)
dimension wext2mx(20000),wext2mn(20000)

integer zexta1i(20000),zexta1j(20000),zexta1k(20000),zexta1p(20000)
dimension zexta1id(20000),zexta1vd(20000),zexta1ed(20000)
dimension zexta1mx(20000),zexta1mn(20000)
integer zexta2i(20000),zexta2j(20000),zexta2k(20000),zexta2p(20000)
dimension zexta2id(20000),zexta2vd(20000),zexta2ed(20000)
dimension zexta2mx(20000),zexta2mn(20000)
integer texta1i(20000),texta1j(20000),texta1k(20000),texta1p(20000)
dimension texta1id(20000),texta1vd(20000),texta1ed(20000)
dimension texta1mx(20000),texta1mn(20000)
integer texta2i(20000),texta2j(20000),texta2k(20000),texta2p(20000)
dimension texta2id(20000),texta2vd(20000),texta2ed(20000)
dimension texta2mx(20000),texta2mn(20000)
integer wexta1i(20000),wexta1j(20000),wexta1k(20000),wexta1p(20000)
dimension wexta1id(20000),wexta1vd(20000),wexta1ed(20000)
dimension wexta1mx(20000),wexta1mn(20000)
integer wexta2i(20000),wexta2j(20000),wexta2k(20000),wexta2p(20000)
dimension wexta2id(20000),wexta2vd(20000),wexta2ed(20000)
dimension wexta2mx(20000),wexta2mn(20000)

data nlevs/10,20,50,100,150,200,250,300,400,500,700,850,925,1000/
data clevs/'10','20','50','100','150','200','250','300','400','500','700','850','925','1000'/
data pwgtz/0.4,0.6,0.8,0.9,1.0,1.0,1.0,1.0,1.0,1.0,0.8,0.6,0.3,0.2/
data pwgtt/0.6,0.8,0.9,1.0,1.0,1.0,1.0,1.0,1.0,1.0,0.9,0.8,0.7,0.6/
data pwgtw/0.8,0.9,1.0,1.0,0.9,0.8,0.9,1.0,1.0,1.0,0.9,0.8,0.7,0.6/
data pwgtza/0.5,0.7,0.8,0.9,1.0,1.0,1.0,1.0,1.0,1.0,0.7,0.6,0.5,0.3/
data pwgtta/0.5,0.7,0.8,0.9,1.0,1.0,1.0,1.0,1.0,0.8,0.5,0.5,0.4,0.3/
!
idim = 360  ! is number of longitude points 0 to 360 degrees
jdim = 181  ! is number of latitude points 90 to -90 degrees
nlev = 14
nzext1 = 0  ! is counter for initial points with large z difs that may be max difs
nzmaxout = 0 ! is counter for output of extreme z difs
ntext1 = 0
ntmaxout = 0
nwext1 = 0
nwmaxout = 0
nzexta1 = 0  ! is counter for initial points with large z anl difs that may be max difs
nzmaxaout = 0 ! is counter for output of extreme anl z difs
ntexta1 = 0
ntmaxaout = 0
nwexta1 = 0
nwmaxaout = 0
ngemouta=0  ! is number of gempak outputs a=all or total
ngemoutt=0  ! is number of gempak outputs t=temps
ngemoutw=0  ! is number of gempak outputs w=winds
ngemoutz=0  ! is number of gempak outputs z=heights
pi=4.0*atan(1.0)
!
call getenv("GFS_GES_F90_ENV",input_gfs_ges)
write(*,*) "input_gfs_ges= ", input_gfs_ges
!
call getenv("CRNT_ANL_F90_ENV",input_crnt_anl)
write(*,*) "input_crnt_anl= ", input_crnt_anl
!
call getenv("REF_ANL_F90_ENV",input_ref_anl)
write(*,*) "input_ref_anl= ", input_ref_anl
!
print*,'idim,jdim  ',idim,jdim

allocate (tbias(nlev),zbias(nlev),wbias(nlev), stat=istatusb)
allocate (trms(nlev),zrms(nlev),wrms(nlev), stat=istatusr)
allocate (hgt(idim,jdim,nlev), temp(idim,jdim,nlev), uwind(idim,jdim,nlev), vwind(idim,jdim,nlev), rh(idim,jdim,nlev), stat=status)
if (status .ne. 0 ) then
write(*,*) "allocation is not successful! stop here."
stop
end if 

allocate (hgte(idim,jdim,nlev), tempe(idim,jdim,nlev), uwinde(idim,jdim,nlev), vwinde(idim,jdim,nlev), rhe(idim,jdim,nlev), stat=status)
if (status .ne. 0 ) then
write(*,*) "allocation is not successful! stop here."
stop
end if

allocate (hgtg(idim,jdim,nlev), tempg(idim,jdim,nlev), uwindg(idim,jdim,nlev), vwindg(idim,jdim,nlev), rhg(idim,jdim,nlev), stat=status)
if (status .ne. 0 ) then
write(*,*) "allocation is not successful! stop here."
stop
end if

allocate (hgtd(idim,jdim,nlev), tempd(idim,jdim,nlev), windd(idim,jdim,nlev),  rhd(idim,jdim,nlev), stat=status)
if (status .ne. 0 ) then
write(*,*) "allocation is not successful! stop here."
stop
end if

allocate (hgtdg(idim,jdim,nlev), tempdg(idim,jdim,nlev), winddg(idim,jdim,nlev),  rhdg(idim,jdim,nlev), stat=status)
if (status .ne. 0 ) then
write(*,*) "allocation is not successful! stop here."
stop
end if

allocate (hgtdeg(idim,jdim,nlev), tempdeg(idim,jdim,nlev), winddeg(idim,jdim,nlev),  rhdeg(idim,jdim,nlev), stat=status)
if (status .ne. 0 ) then
write(*,*) "allocation is not successful! stop here."
stop
end if

allocate (spressa(idim,jdim),spresse(idim,jdim),spressg(idim,jdim), stat=status)
if (status .ne. 0 ) then
write(*,*) "allocation is not successful! stop here."
stop
end if

allocate (presstl(idim,jdim),presszl(idim,jdim), stat=status)
if (status .ne. 0 ) then
write(*,*) "allocation is not successful! stop here."
stop
end if

!  read gfs grib data and store them in hgt, temp, uwind, vwind, rh
lu = 11
leng = len_trim(input_crnt_anl)
write(*,*)  leng, input_crnt_anl
call read_grib(lu, input_crnt_anl, leng, idim, jdim, nlev, hgt, temp, uwind, vwind, rh, spressa, ierr) 
print*,'ierr=  ', ierr
write(*,313) spressa(1,1),spressa(359,1),spressa(1,181),spressa(359,181)
313   format('Surf press test values ',4(' ',e13.4))

!  read ecmwf grib data and store them in hgte, tempe, uwinde, vwinde, rhe
lu = 12
leng = len_trim(input_ref_anl)
write(*,*)  leng, input_ref_anl
call read_grib(lu, input_ref_anl, leng, idim, jdim, nlev, hgte, tempe, uwinde, vwinde, rhe, spresse, ierr)
print*,'ierr=  ', ierr
write(*,313) spresse(1,1),spresse(359,1),spresse(1,181),spresse(359,181)

!  read ges grib data and store them in hgtg, tempg, uwindg, vwindg, rhg
lu = 13
leng = len_trim(input_gfs_ges)
write(*,*)  leng, input_gfs_ges
call read_grib(lu, input_gfs_ges, leng, idim, jdim, nlev, hgtg, tempg, uwindg, vwindg, rhg, spressg, ierr)
print*,'ierr=  ', ierr
write(*,313) spressg(1,1),spressg(359,1),spressg(1,181),spressg(359,181)

rmspe=0.0
rmspa=0.0
nrmspe=0
nrmspa=0
pdifemax=0.0
pdifamax=0.0

do i = 1, idim
do j = 1, jdim
   spressa(i,j)=.01*spressa(i,j)  ! convert surface press to hpa
   spressg(i,j)=.01*spressg(i,j)  ! convert surface press to hpa
   spresse(i,j)=.01*spresse(i,j)  ! convert surface press to hpa
   pmin=1200.0
   difpa=abs(spressa(i,j)-spressg(i,j))
   difpe=abs(spresse(i,j)-spressa(i,j))
   if(spressa(i,j) .lt. pmin) pmin=spressa(i,j)
   if(spresse(i,j) .lt. pmin) pmin=spresse(i,j)
   if(spressg(i,j) .lt. pmin) pmin=spressg(i,j)
   presszl(i,j)=pmin+50.0  ! do not use heights with p higher than presszl
   presstl(i,j)=pmin-10.0  ! do not use temps or winds with p higher than presstl

   if(difpa .lt. 50.0) then
   nrmspa=nrmspa+1
   rmspa=rmspa+difpa*difpa
      if(difpa .gt. pdifamax) then
	 pdifamax=difpa
	 ipdifamax=i
	 jpdifamax=j
      endif
   endif

   if(difpe .lt. 50.0) then
      nrmspe=nrmspe+1
      rmspe=rmspe+difpe*difpe
      if(difpe .gt. pdifemax) then
	 pdifemax=difpe
	 ipdifemax=i
	 jpdifemax=j
      endif
   endif

enddo
enddo

rmspe=sqrt(rmspe/float(nrmspe))
rmspa=sqrt(rmspa/float(nrmspa))
i1=ipdifamax
j1=jpdifamax
i2=ipdifemax
j2=jpdifemax
write(*,209) nrmspa,nrmspe,rmspa,rmspe,i1,j1,pdifamax,i2,j2,pdifemax
209   format('Surface pdif stats=',2i8,2f8.1,2(2i6,f7.1))

! calculate pressure thickness array play based on mandatory thickness levels
do k = 2, nlev -1
   play(k)=0.0050*float(nlevs(k+1) - nlevs(k-1))
enddo
play(1)=0.20
play(14)=.375

do k = 1, nlev
   write(*,126) k,nlevs(k),play(k),pwgtz(k)
enddo
126   format('K, NLEVS, PLAY, PWGTZ= ',i2,' ',i6,' ',2f7.2)

! calculate average cosine weights, skip poles in this loop
do j = 2, jdim -1
   ang1=pi*float(91-(j+1))/180.0
   ang2=pi*float(91-(j-1))/180.0
   w1=sin(ang1)
   w2=sin(ang2)
   wlat(j)=abs(w1-w2)
   lat=91-j
   write(*,127) j,lat,wlat(j),w1,w2
enddo
127   format('J,LAT, WLAT, SIN1 SIN2= ',2i3,' ',3f9.4)

! calculate average cosine weights, at poles, j=1 and 181
ang1=pi*float(91-1)/180.0   ! is equivalent to 90 degrees
ang2=pi*float(91-2)/180.0
w1=sin(ang1)
w2=sin(ang2)
wlat(1)=abs(w1-w2)
wlat(181)=wlat(1)
j=1
lat=91-j
write(*,127) j,lat,wlat(j),w1,w2

do k = 1, nlev
do i = 1, idim
do j = 1, jdim
   hgtd(i,j,k) = hgte(i,j,k) - hgt(i,j,k)
   tempd(i,j,k) = tempe(i,j,k) - temp(i,j,k)
   wx = (uwind(i,j,k) - uwinde(i,j,k))**2 + (vwind(i,j,k) - vwinde(i,j,k))**2
   windd(i,j,k)=sqrt(wx)
   rhd(i,j,k) = 0
   hgtdg(i,j,k) = hgt(i,j,k) - hgtg(i,j,k)
   tempdg(i,j,k) = temp(i,j,k) - tempg(i,j,k)
   wx = (uwind(i,j,k) - uwindg(i,j,k))**2 + (vwind(i,j,k) - vwindg(i,j,k))**2
   winddg(i,j,k)=sqrt(wx)
   rhdg(i,j,k) = 0
   hgtdeg(i,j,k) = hgte(i,j,k) - hgtg(i,j,k)
   tempdeg(i,j,k) = tempe(i,j,k) - tempg(i,j,k)
   wx = (uwinde(i,j,k) - uwindg(i,j,k))**2 + (vwinde(i,j,k) - vwindg(i,j,k))**2
   winddeg(i,j,k)=sqrt(wx)
   rhdeg(i,j,k) = 0
end do
end do
end do 


101 format(3i5, 5f10.2)

do k = 1, nlev
   iplev=nlevs(k)
   tbias(k)=0.0 
   zbias(k)=0.0 
   wbias(k)=0.0 
   trms(k)=0.0 
   zrms(k)=0.0 
   wrms(k)=0.0
   tmax=0.0 
   wmax=0.0 
   zmax=0.0 
   do i = 1, idim
   do j = 1, jdim
      lat=91-j
      dz=hgtd(i,j,k)
      dt=tempd(i,j,k)
      dw=windd(i,j,k)
      dzg=hgtdg(i,j,k)
      dzeg=hgtdeg(i,j,k)
      dtg=tempdg(i,j,k)
      dteg=tempdeg(i,j,k)
      dwg=winddg(i,j,k)
      dweg=winddeg(i,j,k)
      tbias(k)=tbias(k)+dt
      zbias(k)=zbias(k)+dz
      wbias(k)=wbias(k)+dw
      trms(k)=trms(k)+dt*dt
      zrms(k)=zrms(k)+dz*dz
      wrms(k)=wrms(k)+dw*dw

      dzw=pwgtz(k)*abs(dz)
      if(dzw .gt. 30.0 .and. float(iplev) .lt. presstl(i,j)) then
	 !        if(pwgtz(k)*abs(dz) .gt. 60.0) then
	 !        call gribdifs(difsz,dmax,dmin,numz,hgtd,wlat,play,0,i,j,k,nlevs,presstl)
	 ! argument 1, output difsz is the integrated height differences about point i,j,k 
	 ! argument 2, output dmax is the maximum height differences about point i,j,k 
	 ! argument 3, output dmin is the minimum height differences about point i,j,k 
	 ! argument 4, output numz is the number of points used in the integration
	 ! argument 13, input presszl is press limit for underground or not
	 ! argument 12, input nlevs  is array of pressure levels of gribfiles
	 ! argument 11, input k is vertical lev index
	 ! argument 10, input j is latitude index
	 ! argument 9, input i is longitude index
	 ! argument 8, input is number of vertical levs above and below curret level k to use
	 ! argument 7, input play is the measure of the pressure layer thickness
	 ! argument 6, input wlat is the measure of the n-s distance for the grib box centered at i j
	 ! argument 5, input hgtd is an array of height differences
	 call gribdifs(difsz,dmax,dmin,numz,hgtd,wlat,play,2,i,j,k,nlevs,presstl)
	 nzext1=nzext1+1
	 zext1p(nzext1)=iplev
	 zext1i(nzext1)=i
	 zext1j(nzext1)=j
	 zext1k(nzext1)=k
	 zext1id(nzext1)=difsz
	 zext1vd(nzext1)=dz
	 zext1gd(nzext1)=dzg
	 zext1mx(nzext1)=dmax
	 zext1mn(nzext1)=dmin
	 write(*,301) iplev,i,lat,dz,dzw,dzg,dzeg,dmax,dmin,difsz,numz
	 301 format('Z',' ',3i5, 7f7.1,' ',i5)
      endif

      izatest=0
      if(pwgtza(k)*abs(dzg) .gt. 25.0) then
	 !        if(pwgtza(k)*abs(dzg) .gt. 30.0) then
	 if(pwgtza(k)*abs(dzg) .gt. 60.0) izatest=izatest+1

	 !         if(dzg .lt. 0.0 .and. dzeg .gt. -10.0) then
	 if(dzg .lt. 0.0 .and. (dzg-dzeg) .lt. -30.0) then
	    izatest=izatest+1
	    401 format('AZ',' ',3i5, 6f8.1)
	 endif

	 if(dzg .gt. 0.0 .and. (dzg-dzeg) .gt. 30.0) then
	    izatest=izatest+1
	 endif

	 if(izatest .gt. 0 .and. float(iplev) .lt. presstl(i,j)) then 
	    write(*,401) iplev,i,lat,hgt(i,j,k),hgtg(i,j,k),hgte(i,j,k),dzg,dz,dzeg
	    ! compute volumetric difs and store possible output points
	    call gribdifs(difszg,dmax,dmin,numz,hgtdg,wlat,play,2,i,j,k,nlevs,presstl)
	    !         call gribdifs(difszeg,dmax,dmin,numz,hgtdeg,wlat,play,2,i,j,k,,nlevs,presstl)
	    nzexta1=nzexta1+1
	    zexta1p(nzexta1)=iplev
	    zexta1i(nzexta1)=i
	    zexta1j(nzexta1)=j
	    zexta1k(nzexta1)=k
	    zexta1id(nzexta1)=difszg
	    zexta1vd(nzexta1)=dzg
	    zexta1mx(nzexta1)=dmax
	    zexta1mn(nzexta1)=dmin
	    zexta1ed(nzexta1)=dzeg
	 endif

      endif  ! endif for  pwgtza(k)*abs(dzg) .gt. 30.0

      dtw=pwgtt(k)*abs(dt)
      if(dtw .gt. 5.0 .and. float(iplev) .lt. presstl(i,j)) then
	 !        call gribdifs(difstg,dmax,dmin,numt,tempd,wlat,play,0,i,j,k,nlevs,presstl)
	 call gribdifs(difst,dmax,dmin,numt,tempd,wlat,play,2,i,j,k,nlevs,presstl)
	 ntext1=ntext1+1
	 text1p(ntext1)=iplev
	 text1i(ntext1)=i
	 text1j(ntext1)=j
	 text1k(ntext1)=k
	 text1id(ntext1)=difst
	 text1vd(ntext1)=dt
	 text1mx(ntext1)=dmax
	 text1mn(ntext1)=dmin
	 text1gd(ntext1)=dtg
	 write(*,302) iplev,i,lat,dt,dtw,dtg,dteg,difst,dmax,dmin,numt
	 302 format('T',' ',3i5, 7f7.1,' ',i5)
      endif

      itatest=0
      if(pwgtta(k)*abs(dtg) .gt. 2.0) then
	 if(pwgtta(k)*abs(dtg) .gt. 6.0) itatest=itatest+1

	 if(dtg .lt. 0.0 .and. (dtg-dteg) .lt. -2.5) then
	    itatest=itatest+1
	    402 format('AT',' ',3i5, 6f8.1)
	 endif

	 if(dtg .gt. 0.0 .and. (dtg-dteg) .gt. 2.5) then
	    itatest=itatest+1
	 endif

	 if(itatest .gt. 0 .and. float(iplev) .lt. presstl(i,j)) then 
	    write(*,402) iplev,i,lat,temp(i,j,k),tempg(i,j,k),tempe(i,j,k),dtg,dt,dteg
	    ! compute volumetric difs and store possible output points
	    call gribdifs(difstg,dmax,dmin,numt,tempdg,wlat,play,2,i,j,k,nlevs,presstl)
	    !         call gribdifs(difsteg,dmax,dmin,numt,tempdeg,wlat,play,2,i,j,k,nlevs,presstl)
	    ntexta1=ntexta1+1
	    texta1p(ntexta1)=iplev
	    texta1i(ntexta1)=i
	    texta1j(ntexta1)=j
	    texta1k(ntexta1)=k
	    texta1id(ntexta1)=difstg
	    !         texta1id(ntexta1)=difstg-difsteg
	    texta1vd(ntexta1)=dtg
	    texta1mx(ntexta1)=dmax
	    texta1mn(ntexta1)=dmin
	    texta1ed(ntexta1)=dteg
	 endif

      endif  ! endif for  pwgtta(k)*abs(dtg) .gt. 2.0

      dww=pwgtw(k)*dw
      if(dww .gt. 20.0 .and. float(iplev) .lt. presstl(i,j)) then
	 !        call gribdifs(difsw,dmax,dmin,numw,windd,wlat,play,0,i,j,k,nlevs,presstl)
	 call gribdifs(difsw,dmax,dmin,numw,windd,wlat,play,2,i,j,k,nlevs,presstl)
	 nwext1=nwext1+1
	 wext1p(nwext1)=iplev
	 wext1i(nwext1)=i
	 wext1j(nwext1)=j
	 wext1k(nwext1)=k
	 wext1id(nwext1)=difsw
	 wext1vd(nwext1)=dw
	 wext1mx(nwext1)=dmax
	 wext1mn(nwext1)=dmin
	 wext1gd(nwext1)=dwg
	 write(*,303) iplev,i,lat,dw,dww,dwg,dweg,difsw,dmax,dmin,numw
	 303 format('W',' ',3i5, 7f7.1,' ',i5)
      endif

      iwatest=0
      if(dwg .gt. 12.0 .and. dweg .lt. 6.0) iwatest=iwatest+1
      if(dwg .gt. 18.0 ) iwatest=iwatest+1
      if(iwatest .gt. 0 .and. float(iplev) .lt. presstl(i,j)) then

	 write(*,403) iplev,i,lat,dwg,dw,dweg
	 403 format('AW',' ',3i5, 3f8.1)
	 ! compute volumetric difs and store possible output points
	 call gribdifs(difswg,dmax,dmin,numw,winddg,wlat,play,2,i,j,k,nlevs,presstl)
	 nwexta1=nwexta1+1
	 write(*,903) iplev,i,lat,dwg,dw,dweg,difswg,dmax,dmin
	 903 format('AWPLUS',' ',3i5, 6f8.1)
	 wexta1p(nwexta1)=iplev
	 wexta1i(nwexta1)=i
	 wexta1j(nwexta1)=j
	 wexta1k(nwexta1)=k
	 wexta1id(nwexta1)=difswg
	 wexta1vd(nwexta1)=dwg
	 wexta1mx(nwexta1)=dmax
	 wexta1mn(nwexta1)=dmin
	 wexta1ed(nwexta1)=dweg

      endif  ! endif for  iwatest .gt. 0

      if(abs(dz) .gt. zmax) zmax=abs(dz) 
      if(abs(dt) .gt. tmax) tmax=abs(dt) 
      if(dw .gt. wmax) wmax=dw 
   end do   ! for lat loop on j
   end do   ! for lon loop on i

   tbias(k)=tbias(k)/65160.0
   zbias(k)=zbias(k)/65160.0
   wbias(k)=wbias(k)/65160.0
   vart=trms(k)/65160.0 - tbias(k)*tbias(k)
   tstd=sqrt(vart)
   trms(k)=sqrt(trms(k)/65160.0)
   varz=zrms(k)/65160.0 - zbias(k)*zbias(k)
   zstd=sqrt(varz)
   zrms(k)=sqrt(zrms(k)/65160.0)
   varw=wrms(k)/65160.0 - wbias(k)*wbias(k)
   wstd=sqrt(varw)
   wrms(k)=sqrt(wrms(k)/65160.0)
   write(*,501) k,zbias(k),zstd,zrms(k),zmax
   write(*,502) k,tbias(k),tstd,trms(k),tmax
   write(*,503) k,wbias(k),wstd,wrms(k),wmax
   501 format('STAT Z',' ',i3, 4f10.2)
   502 format('STAT T',' ',i3, 4f10.2)
   503 format('STAT W',' ',i3, 4f10.2)

end do    ! for vertical loop on k

!! start finding height extremes for output
605   continue

if(nzext1 .gt. 0) then
   zextmax=0.0
   do n=1,nzext1
      if(zext1id(n) .gt. zextmax) then
	 zextmax= zext1id(n)
	 dzmax= zext1vd(n)
	 dzmaxt= zext1mx(n)
	 dzmint= zext1mn(n)
	 dgzmax= zext1gd(n)
	 izmax=zext1i(n)
	 jzmax=zext1j(n)
	 latm=91-jzmax
	 kzmax=zext1k(n)
	 lzmax=zext1p(n)
      endif
   enddo
   nzmaxout=nzmaxout+1
   905   format(i4)
   write(cleva,905) lzmax
   cleva=clevs(kzmax)
   call gribdifs(difsw,dmax,dmin,numw,windd,wlat,play,2,izmax,jzmax,kzmax,nlevs,presstl)
   write(*,601) nzmaxout,izmax,latm,lzmax,dzmax,dgzmax,dzmaxt,dzmint,zextmax,difsw,dmax,dmin
   ngemouta=ngemouta+1
   ngemoutz=ngemoutz+1
   icol=2
   if(abs(zextmax) .gt. 90.0) icol=1
   ilong=izmax
   if(izmax .gt. 180.) ilong= izmax -360  ! ilong is longitude for gempak
   write(55,801) ngemouta,cleva,latm,ilong,zextmax,icol 
   801   format(i4,' ','Z',a4,2('  ',i4),f7.1,' 1  88  ',i2)
   write(60,201) ngemouta,cleva,latm,izmax
   201   format('Z  ',i2,'  ',a4,2('  ',i4))
endif
601   format('EMNMAXZ=',4i5,' ',8f7.1)

nzext2=0
if(nzext1 .gt. 1) then
   do n=1,nzext1
      i1=zext1i(n)
      j1=zext1j(n)
      ! find spherical distance in km output in dist from point 1 to 2 by calling grib_dist
      ! point 1 has lon lat from i1 j1   point two has lon lat from izmax jzmax
      call grib_dist(dist,i1,j1,izmax,jzmax)
      lzp=zext1p(n)
      if(dist .gt. 1500.0 .or. iabs(lzp-lzmax) .gt. 400) then
	 nzext2=nzext2+1
	 zext2i(nzext2)=zext1i(n)
	 zext2j(nzext2)=zext1j(n)
	 zext2k(nzext2)=zext1k(n)
	 zext2p(nzext2)=zext1p(n)
	 zext2id(nzext2)=zext1id(n)
	 zext2vd(nzext2)=zext1vd(n)
	 zext2mx(nzext2)=zext1mx(n)
	 zext2mn(nzext2)=zext1mn(n)
	 zext2gd(nzext2)=zext1gd(n)
      endif
   enddo
endif

if(nzext2 .gt. 0) then
   do n=1,nzext2
      zext1i(n)=zext2i(n)
      zext1j(n)=zext2j(n)
      zext1k(n)=zext2k(n)
      zext1p(n)=zext2p(n)
      zext1id(n)=zext2id(n)
      zext1vd(n)=zext2vd(n)
      zext1mx(n)=zext2mx(n)
      zext1mn(n)=zext2mn(n)
      zext1gd(n)=zext2gd(n)
   enddo
   nzext1=nzext2
   if(nzmaxout .lt. 3) go to 605
endif
!! end finding height extremes for output

!! start finding analysis-ges height extremes for output
705   continue

if(nzexta1 .gt. 0) then
   zextamax=0.0
   do n=1,nzexta1
      if(zexta1id(n) .gt. zextamax) then
	 zextamax= zexta1id(n)
	 dzmax= zexta1vd(n)
	 dzmaxt= zexta1mx(n)
	 dzmint= zexta1mn(n)
	 dezmax= zexta1ed(n)
	 izmax=zexta1i(n)
	 latm=91 - zexta1j(n)
	 jzmax=zexta1j(n)
	 kzmax=zexta1k(n)
	 lzmax=zexta1p(n)
      endif
   enddo
   nzmaxaout=nzmaxaout+1
   call gribdifs(difswg,dmax,dmin,numw,winddg,wlat,play,2,izmax,jzmax,kzmax,nlevs,presstl)
   write(*,701) nzmaxaout,izmax,latm,lzmax,dzmax,dezmax,dzmaxt,dzmint,zextamax,difswg,dmax,dmin
   ngemouta=ngemouta+1
   ngemoutz=ngemoutz+1
   icol=2
   if(abs(zextamax) .gt. 50.0) icol=1
   ilong=izmax
   if(izmax .gt. 180.) ilong= izmax -360  ! ilong is longitude for gempak
   write(cleva,905) lzmax
   cleva=clevs(kzmax)
   write(55,801) ngemouta,cleva,latm,ilong,zextamax,icol 
   write(60,201) ngemouta,cleva,latm,izmax
endif
701   format('AMGMAXZ=',4i5,' ',8f7.1)

nzexta2=0
if(nzexta1 .gt. 1) then
   do n=1,nzexta1
      i1=zexta1i(n)
      j1=zexta1j(n)
      ! find spherical distance in km output in dist from point 1 to 2 by calling grib_dist
      ! point 1 has lon lat from i1 j1   point two has lon lat from izmax jzmax
      call grib_dist(dist,i1,j1,izmax,jzmax)
      lzp=zexta1p(n)
      if(dist .gt. 1500.0 .or. iabs(lzp-lzmax) .gt. 400) then
	 !      if(dist .gt. 3000.0 .or. iabs(lzp-lzmax) .gt. 400) then
	 nzexta2=nzexta2+1
	 zexta2i(nzexta2)=zexta1i(n)
	 zexta2j(nzexta2)=zexta1j(n)
	 zexta2k(nzexta2)=zexta1k(n)
	 zexta2p(nzexta2)=zexta1p(n)
	 zexta2id(nzexta2)=zexta1id(n)
	 zexta2vd(nzexta2)=zexta1vd(n)
	 zexta2mx(nzexta2)=zexta1mx(n)
	 zexta2mn(nzexta2)=zexta1mn(n)
	 zexta2ed(nzexta2)=zexta1ed(n)
      endif
   enddo
endif

if(nzexta2 .gt. 0) then
   do n=1,nzexta2
      zexta1i(n)=zexta2i(n)
      zexta1j(n)=zexta2j(n)
      zexta1k(n)=zexta2k(n)
      zexta1p(n)=zexta2p(n)
      zexta1id(n)=zexta2id(n)
      zexta1vd(n)=zexta2vd(n)
      zexta1mx(n)=zexta2mx(n)
      zexta1mn(n)=zexta2mn(n)
      zexta1ed(n)=zexta2ed(n)
   enddo
   nzexta1=nzexta2
   if(nzmaxaout .lt. 3) go to 705
   !      if(nzmaxaout .lt. 6) go to 705
endif
!! end finding analysis minus ges height extremes for output

!! start finding temperature extremes for output
606   continue

if(ntext1 .gt. 0) then
   textmax=0.0
   do n=1,ntext1
      if(text1id(n) .gt. textmax) then
	 textmax= text1id(n)
	 dtmax= text1vd(n)
	 dtmaxt= text1mx(n)
	 dtmint= text1mn(n)
	 dgtmax= text1gd(n)
	 itmax=text1i(n)
	 jtmax=text1j(n)
	 latm=91-jtmax
	 ktmax=text1k(n)
	 ltmax=text1p(n)
      endif
   enddo
   ntmaxout=ntmaxout+1
   call gribdifs(difsw,dmax,dmin,numw,windd,wlat,play,2,itmax,jtmax,ktmax,nlevs,presstl)
   write(*,602) ntmaxout,itmax,latm,ltmax,dtmax,dgtmax,dtmaxt,dtmint,textmax,difsw,dmax,dmin
   ngemouta=ngemouta+1
   ngemoutt=ngemoutt+1
   icol=2
   if(abs(textmax) .gt. 5.0) icol=1
   ilong=itmax
   if(itmax .gt. 180.) ilong= itmax -360  ! ilong is longitude for gempak
   write(cleva,905) ltmax
   cleva=clevs(ktmax)
   write(55,802) ngemouta,cleva,latm,ilong,textmax,icol 
   802   format(i4,' ','T',a4,2('  ',i4),f7.1,' 1  88  ',i2)
   write(60,202) ngemouta,cleva,latm,itmax
   202   format('T  ',i2,'  ',a4,2('  ',i4))
endif
602   format('EMNMAXT=',4i5,' ',8f7.1)

ntext2=0
if(ntext1 .gt. 1) then
   do n=1,ntext1
      i1=text1i(n)
      j1=text1j(n)
      call grib_dist(dist,i1,j1,itmax,jtmax)
      ltp=text1p(n)
      if(dist .gt. 1500.0 .or. iabs(ltp-ltmax) .gt. 400) then
	 ntext2=ntext2+1
	 text2i(ntext2)=text1i(n)
	 text2j(ntext2)=text1j(n)
	 text2k(ntext2)=text1k(n)
	 text2p(ntext2)=text1p(n)
	 text2id(ntext2)=text1id(n)
	 text2vd(ntext2)=text1vd(n)
	 text2mx(ntext2)=text1mx(n)
	 text2mn(ntext2)=text1mn(n)
	 text2gd(ntext2)=text1gd(n)
      endif
   enddo
endif

if(ntext2 .gt. 0) then
   do n=1,ntext2
      text1i(n)=text2i(n)
      text1j(n)=text2j(n)
      text1k(n)=text2k(n)
      text1p(n)=text2p(n)
      text1id(n)=text2id(n)
      text1vd(n)=text2vd(n)
      text1mx(n)=text2mx(n)
      text1mn(n)=text2mn(n)
      text1gd(n)=text2gd(n)
   enddo
   ntext1=ntext2
   if(ntmaxout .lt. 3) go to 606
endif
!! end finding temperature extremes for output

!! start finding analysis-ges temp extremes for output
706   continue

if(ntexta1 .gt. 0) then
   textamax=0.0
   do n=1,ntexta1
      if(texta1id(n) .gt. textamax) then
	 textamax= texta1id(n)
	 dtmax= texta1vd(n)
	 dtmaxt= texta1mx(n)
	 dtmint= texta1mn(n)
	 detmax= texta1ed(n)
	 itmax=texta1i(n)
	 latm=91 - texta1j(n)
	 jtmax=texta1j(n)
	 ktmax=texta1k(n)
	 ltmax=texta1p(n)
      endif
   enddo
   ntmaxaout=ntmaxaout+1
   call gribdifs(difswg,dmax,dmin,numw,winddg,wlat,play,2,itmax,jtmax,ktmax,nlevs,presstl)
   write(*,702) ntmaxaout,itmax,latm,ltmax,dtmax,detmax,dtmaxt,dtmint,textamax,difswg,dmax,dmin
   ngemouta=ngemouta+1
   ngemoutt=ngemoutt+1
   icol=2
   if(abs(textamax) .gt. 5.0) icol=1
   ilong=itmax
   if(itmax .gt. 180.) ilong= itmax -360  ! ilong is longitude for gempak
   write(cleva,905) ltmax
   cleva=clevs(ktmax)
   write(55,802) ngemouta,cleva,latm,ilong,textamax,icol 
   write(60,202) ngemouta,cleva,latm,itmax
endif
702   format('AMGMAXT=',4i5,' ',8f7.1)

ntexta2=0
if(ntexta1 .gt. 1) then
   do n=1,ntexta1
      i1=texta1i(n)
      j1=texta1j(n)
      ! find spherical distance in km output in dist from point 1 to 2 by calling grib_dist
      ! point 1 has lon lat from i1 j1   point two has lon lat from itmax jtmax
      call grib_dist(dist,i1,j1,itmax,jtmax)
      lzp=texta1p(n)
      if(dist .gt. 1500.0 .or. iabs(lzp-ltmax) .gt. 400) then
	 ntexta2=ntexta2+1
	 texta2i(ntexta2)=texta1i(n)
	 texta2j(ntexta2)=texta1j(n)
	 texta2k(ntexta2)=texta1k(n)
	 texta2p(ntexta2)=texta1p(n)
	 texta2id(ntexta2)=texta1id(n)
	 texta2vd(ntexta2)=texta1vd(n)
	 texta2mx(ntexta2)=texta1mx(n)
	 texta2mn(ntexta2)=texta1mn(n)
	 texta2ed(ntexta2)=texta1ed(n)
      endif
   enddo
endif

if(ntexta2 .gt. 0) then
   do n=1,ntexta2
      texta1i(n)=texta2i(n)
      texta1j(n)=texta2j(n)
      texta1k(n)=texta2k(n)
      texta1p(n)=texta2p(n)
      texta1id(n)=texta2id(n)
      texta1vd(n)=texta2vd(n)
      texta1mx(n)=texta2mx(n)
      texta1mn(n)=texta2mn(n)
      texta1ed(n)=texta2ed(n)
   enddo
   ntexta1=ntexta2
   if(ntmaxaout .lt. 3) go to 706
endif
!! end finding analysis minus ges temp extremes for output

!! start finding wind extremes for output
607   continue

if(nwext1 .gt. 0) then
   wextmax=0.0
   do n=1,nwext1
      if(wext1id(n) .gt. wextmax) then
	 wextmax= wext1id(n)
	 dwmax= wext1vd(n)
	 dwmaxt= wext1mx(n)
	 dwmint= wext1mn(n)
	 dgwmax= wext1gd(n)
	 iwmax=wext1i(n)
	 jwmax=wext1j(n)
	 latm=91-wext1j(n)
	 kwmax=wext1k(n)
	 lwmax=wext1p(n)
      endif
   enddo
   nwmaxout=nwmaxout+1
   call gribdifs(difsz,dmax,dmin,numz,hgtd,wlat,play,2,iwmax,jwmax,kwmax,nlevs,presstl)
   write(*,603) nwmaxout,iwmax,latm,lwmax,dwmax,dgwmax,dwmaxt,dwmint,wextmax,difsz,dmax,dmin
   ngemouta=ngemouta+1
   ngemoutw=ngemoutw+1
   icol=2
   if(abs(wextmax) .gt. 30.0) icol=1
   ilong=iwmax
   if(iwmax .gt. 180.) ilong= iwmax -360  ! ilong is longitude for gempak
   write(cleva,905) lwmax
   cleva=clevs(kwmax)
   write(55,803) ngemouta,cleva,latm,ilong,wextmax,icol 
   803   format(i4,' ','W',a4,2('  ',i4),f7.1,' 1  88  ',i2)
   write(60,203) ngemouta,cleva,latm,iwmax
   203   format('W  ',i2,'  ',a4,2('  ',i4))
endif
603   format('EMNMAXW=',4i5,' ',8f7.1)

nwext2=0
if(nwext1 .gt. 1) then
   do n=1,nwext1
      i1=wext1i(n)
      j1=wext1j(n)
      call grib_dist(dist,i1,j1,iwmax,jwmax)
      lwp=wext1p(n)
      if(dist .gt. 1500.0 .or. iabs(lwp-lwmax) .gt. 400) then
	 nwext2=nwext2+1
	 wext2i(nwext2)=wext1i(n)
	 wext2j(nwext2)=wext1j(n)
	 wext2k(nwext2)=wext1k(n)
	 wext2p(nwext2)=wext1p(n)
	 wext2id(nwext2)=wext1id(n)
	 wext2vd(nwext2)=wext1vd(n)
	 wext2mx(nwext2)=wext1mx(n)
	 wext2mn(nwext2)=wext1mn(n)
	 wext2gd(nwext2)=wext1gd(n)
      endif
   enddo
endif

if(nwext2 .gt. 0) then
   do n=1,nwext2
      wext1i(n)=wext2i(n)
      wext1j(n)=wext2j(n)
      wext1k(n)=wext2k(n)
      wext1p(n)=wext2p(n)
      wext1id(n)=wext2id(n)
      wext1vd(n)=wext2vd(n)
      wext1mx(n)=wext2mx(n)
      wext1mn(n)=wext2mn(n)
      wext1gd(n)=wext2gd(n)
   enddo
   nwext1=nwext2
   if(nwmaxout .lt. 3) go to 607
endif
!! end finding wind extremes for output

!! start finding analysis-ges wind extremes for output
707   continue

if(nwexta1 .gt. 0) then
   wextamax=0.0
   do n=1,nwexta1
      if(wexta1id(n) .gt. wextamax) then
	 wextamax= wexta1id(n)
	 dwmax= wexta1vd(n)
	 dwmaxt= wexta1mx(n)
	 dwmint= wexta1mn(n)
	 dewmax= wexta1ed(n)
	 iwmax=wexta1i(n)
	 latm=91 - wexta1j(n)
	 jwmax=wexta1j(n)
	 kwmax=wexta1k(n)
	 lwmax=wexta1p(n)
      endif
   enddo
   nwmaxaout=nwmaxaout+1
   call gribdifs(difszg,dmax,dmin,numz,hgtdg,wlat,play,2,iwmax,jwmax,kwmax,nlevs,presstl)
   write(*,703) nwmaxaout,iwmax,latm,lwmax,dwmax,dewmax,dwmaxt,dwmint,wextamax,difszg,dmax,dmin
   ngemouta=ngemouta+1
   ngemoutw=ngemoutw+1
   icol=2
   if(abs(wextamax) .gt. 20.0) icol=1
   ilong=iwmax
   if(iwmax .gt. 180.) ilong= iwmax -360  ! ilong is longitude for gempak
   write(cleva,905) lwmax
   cleva=clevs(kwmax)
   write(55,803) ngemouta,cleva,latm,ilong,wextamax,icol 
   write(60,203) ngemouta,cleva,latm,iwmax
endif
703   format('AMGMAXW=',4i5,' ',8f7.1)

nwexta2=0
if(nwexta1 .gt. 1) then
    do n=1,nwexta1
       i1=wexta1i(n)
       j1=wexta1j(n)
       ! find spherical distance in km output in dist from point 1 to 2 by calling grib_dist
       ! point 1 has lon lat from i1 j1   point two has lon lat from itmax jtmax
       call grib_dist(dist,i1,j1,iwmax,jwmax)
       lzp=wexta1p(n)
       if(dist .gt. 1500.0 .or. iabs(lzp-lwmax) .gt. 400) then
	  nwexta2=nwexta2+1
	  wexta2i(nwexta2)=wexta1i(n)
	  wexta2j(nwexta2)=wexta1j(n)
	  wexta2k(nwexta2)=wexta1k(n)
	  wexta2p(nwexta2)=wexta1p(n)
	  wexta2id(nwexta2)=wexta1id(n)
	  wexta2vd(nwexta2)=wexta1vd(n)
	  wexta2mx(nwexta2)=wexta1mx(n)
	  wexta2mn(nwexta2)=wexta1mn(n)
	  wexta2ed(nwexta2)=wexta1ed(n)
       endif
    enddo
endif

if(nwexta2 .gt. 0) then
   do n=1,nwexta2
      wexta1i(n)=wexta2i(n)
      wexta1j(n)=wexta2j(n)
      wexta1k(n)=wexta2k(n)
      wexta1p(n)=wexta2p(n)
      wexta1id(n)=wexta2id(n)
      wexta1vd(n)=wexta2vd(n)
      wexta1mx(n)=wexta2mx(n)
      wexta1mn(n)=wexta2mn(n)
      wexta1ed(n)=wexta2ed(n)
   enddo
   nwexta1=nwexta2
   if(nwmaxaout .lt. 3) go to 707
endif
!! end finding analysis minus ges wind extremes for output

stop
end
