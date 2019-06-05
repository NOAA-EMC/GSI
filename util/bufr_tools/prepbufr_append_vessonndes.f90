program prepbufr_append_vessonndes
!
!  append a upper air observation into prepbufr file
!
 implicit none

 integer, parameter :: mxmn=35, mxlv=200
 character(80):: hdstr='SID XOB YOB DHR TYP ELV SAID T29'
 character(80):: obstr='POB QOB TOB ZOB UOB VOB PWO CAT PRSS'
 character(80):: qcstr='PQM QQM TQM ZQM WQM NUL PWQ     '
 character(80):: oestr='POE QOE TOE NUL WOE NUL PWE     '
 real(8) :: hdr(mxmn),obs(mxmn,mxlv),qcf(mxmn,mxlv),oer(mxmn,mxlv)

 character(8) :: subset
 integer      :: unit_out=10,unit_table=20,idate,iret,nlvl

 character(8) :: c_sid
 real(8)      :: rstation_id
 equivalence(rstation_id,c_sid)

 character(len=40) :: sondefilelist
 integer :: numSondes
 integer :: cycledate,cyclehour
 namelist/setup/ sondefilelist,numSondes,cycledate,cyclehour
!
 character(len=250),allocatable :: filenames(:)
 character(len=11) :: chead1,chead2
 integer :: i,j
 real,allocatable :: Pres(:),Tdry(:),Rho(:),Uwind(:),Vwind(:)
 real :: lat,lon,lev
 integer :: numlvl
 integer :: idat,ihr,imin
 character (len=250) :: filename
!
!
!
 open(12,file='namelist_vsesonde')
    read(12,setup)
 close(12)
 write(*,setup)
!
 if(numSondes > 0) then
    write(*,*) 'Append ',numSondes,' VSE sondes'
    allocate(filenames(numSondes))
 else
    write(*,*) 'No VSE Sonde available !'
    stop 
 endif
!
 open(12,file=trim(sondefilelist))
    do i=1,numSondes
      read(12,'(a)') filenames(i)
      write(*,'(I5,a)') i,trim(filenames(i))
    enddo
 close(12)
!
! get bufr table from existing bufr file
 open(unit_table,file='prepbufr.table')
 open(unit_out,file='prepbufr_vsesondes',status='old',form='unformatted')
 call openbf(unit_out,'IN',unit_out)
 call dxdump(unit_out,unit_table)
 call closbf(unit_out)
!
! write observation into prepbufr file
!
 open(unit_out,file='prepbufr_vsesondes',status='old',form='unformatted')
 call datelen(10)
 call openbf(unit_out,'APN',unit_table)

   idate=cycledate*100+cyclehour ! cycle time: YYYYMMDDHH
   subset='ADPUPA'  ! upper-air (raob, drops) reports
   call openmb(unit_out,subset,idate)

   do i=1,numSondes
!
! read in one sonde data
!
      filename=trim(filenames(i))
      open(12, file=trim(filename))       
         read(12,'(a6)') c_sid
         read(12,'(10x,2f9.4)') lon,lat
         read(12,'(10x,f5.0)') lev
         read(12,'(11x,i8,i2,i2)') idat,ihr,imin
         read(12,'(I2)') numlvl
  
         allocate(Pres(numlvl),Tdry(numlvl))
         allocate(Rho(numlvl))
         allocate(Uwind(numlvl),Vwind(numlvl))
         write(*,*) numlvl
         read(12,'(12x,100(f7.1,1x))') (Pres(j),j=1,numlvl)
         read(12,'(12x,100(f7.2,1x))') (Tdry(j),j=1,numlvl)
         read(12,'(12x,100(f7.3,1x))') (Rho(j),j=1,numlvl)
         read(12,'(12x,100(f7.2,1x))') (Uwind(j),j=1,numlvl)
         read(12,'(12x,100(f7.2,1x))') (Vwind(j),j=1,numlvl)
      close(12)
!      write(*,*) c_sid,lon,lat,lev,idat,ihr,imin,numlvl
!      write(*,*) Pres
!      write(*,*) Tdry
!      write(*,*) Rho
!      write(*,*) Uwind
!      write(*,*) Vwind
!
!
! set headers
      hdr=10.0e10
      hdr(1)=rstation_id
      hdr(2)=lon; hdr(3)=lat; hdr(4)=0.0; hdr(6)=lev
      hdr(8)=11.0

! set obs, qcf, oer for  wind
      hdr(5)=220          ! report type: sounding
      obs=10.0e10;qcf=10.0e10;oer=10.0e10
      do j=1,numlvl
         obs(1,j)=Pres(j)
         obs(5,j)=Uwind(j)
         obs(6,j)=Vwind(j)
         qcf(1,j)=2.0
         qcf(5,j)=2.0
         obs(8,j)=1.0
      enddo
      nlvl=numlvl
! encode  wind obs
      call ufbint(unit_out,hdr,mxmn,1   ,iret,hdstr)
      call ufbint(unit_out,obs,mxmn,nlvl,iret,obstr)
      call ufbint(unit_out,oer,mxmn,nlvl,iret,oestr)
      call ufbint(unit_out,qcf,mxmn,nlvl,iret,qcstr)
      call writsb(unit_out)

! set obs, qcf, oer for  temperature and moisture
      hdr(5)=120          ! report type: sounding
      obs=10.0e10;qcf=10.0e10;oer=10.0e10
      do j=1,numlvl
         obs(1,j)=Pres(j)
         obs(2,j)=Rho(j)*1000.0
         obs(3,j)=Tdry(j)
         obs(8,j)=1.0
         qcf(1,j)=2.0
         qcf(2,j)=2.0
         qcf(3,j)=2.0
      enddo
      nlvl=numlvl
! encode temperature and moisture
      call ufbint(unit_out,hdr,mxmn,1   ,iret,hdstr)
      call ufbint(unit_out,obs,mxmn,nlvl,iret,obstr)
      call ufbint(unit_out,oer,mxmn,nlvl,iret,oestr)
      call ufbint(unit_out,qcf,mxmn,nlvl,iret,qcstr)
      call writsb(unit_out)


      deallocate(Pres,Tdry)
      deallocate(Rho)
      deallocate(Uwind,Vwind)

   enddo ! i = loop through sonde

   call closmg(unit_out)
 call closbf(unit_out)

end program
