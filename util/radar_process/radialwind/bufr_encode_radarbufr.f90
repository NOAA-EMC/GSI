program bufr_encode_radarbufr
!
! Read Level 2.5/3 radar radial velocity obs and 
! encode them into a bufr that can be used by GSI. 
!
! MNEMONIC used in this code
!    SSTN CLAT CLON HSMSL HSALG ANEL ANAZ QCRW
!    YEAR MNTH DAYS HOUR MINU SECO
!    SCID HNQV VOCP VOID
!    DIST125M DMVR DVSW
! Please refer BUFR table for explanations of MNEMONIC
!
!
 implicit none

 integer, parameter :: mxmn=35, mxlv=1000
 character(80):: hdstr= 'SSTN CLAT CLON HSMSL HSALG ANEL ANAZ QCRW'
 character(80):: hdstr2='YEAR MNTH DAYS HOUR MINU SECO' 
 character(80):: hdstr3='SCID HNQV VOCP VOID' 
 character(80):: obstr='DIST125M DMVR DVSW'
 real(8) :: hdr(mxmn),hdr2(mxmn),hdr3(mxmn),obs(3,mxlv)

 INTEGER        :: ireadmg,ireadsb

 character(8)   :: subset
 integer        :: unit_in=10,unit_out=20,unit_table=30
 integer        :: idate

 character(8)   :: c_sid
 real(8)        :: rstation_id
 equivalence(rstation_id,c_sid)

 integer        :: numrwbin,ntab,ntb
 integer        :: i,k,iret,ii,istat
!
 integer          :: valid_time
 character(len=8) :: subset2
!
!
!
 open(unit_table,file='bufr_radar.table')
 open(unit_out,file='radarbufr_new',form='unformatted',status='new')
 call openbf(unit_out,'OUT',unit_table)
 call datelen(10)
!
 ntab=0
 open(unit_in,file='l2rwbufr.bin',form='unformatted',status='old')
 readrw: do 

    hdr=10.0e+10
    hdr2=10.0e+10
    hdr3=10.0e+10
    obs=10.0e+10
!
!    read(unit_in,'(2I10,a14,10f8.1)',iostat=istat) &
!                    ntb,numrwbin,c_sid,(hdr(i),i=2,8)
    read(unit_in,iostat=istat) ntb,numrwbin,(hdr(i),i=1,8)
    if(istat/=0) exit
    ntab=ntab + 1
    if(ntab > 10) cycle
!    read(unit_in,'(34x,10f8.1)') (hdr2(i),i=1,6)
    read(unit_in) (hdr2(i),i=1,6)
    read(unit_in) (hdr3(i),i=1,4)
    do i=1,numrwbin
!       read(unit_in,'(10x,i10,5f16.2)') ii,obs(:,i)
       read(unit_in) obs(1:3,i)
    enddo
!
!    hdr(1)=rstation_id
    rstation_id=hdr(1)
!    hdr2(1)=2008
!    hdr2(2)=5
!    hdr2(3)=11
!    hdr2(4)=12
    valid_time=(int(hdr2(1))*1000000)+ &
               (int(hdr2(2))*10000)+ &
               (int(hdr2(3))*100)+(int(hdr2(4)))
    write(*,*) valid_time
!
! set the report subtype based on the report hour - see the bufrtab.006  
! for the hour windows
! 
    subset2(1:6) = 'NC0060'
    WRITE (UNIT=subset2(7:8),FMT='(I2)') int(hdr2(4)) + 10
!
    call openmb(unit_out,subset2,valid_time)
    call ufbint(unit_out,hdr ,8,1  ,iret,hdstr)
    call ufbint(unit_out,hdr2,6,1  ,iret,hdstr2)
    call ufbint(unit_out,hdr3,4,1  ,iret,hdstr3)
    call ufbint(unit_out,obs ,3,  numrwbin,iret,obstr)
    call writsb(unit_out)
!    if(ntab > 10 ) exit
!    write(*,*) ntab,ntb
 enddo readrw
!
 call writsb(unit_out)
 call closbf(unit_out)
 close(unit_table)
 close(unit_in)

 write(*,*) 'The subset processed =',ntab

end program
