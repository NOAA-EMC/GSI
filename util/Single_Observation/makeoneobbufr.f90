program makeoneobbufr
!! xlf90 makeoneobbufr.f90 -qintsize=4 -qrealsize=8 -o makeoneobbufr.x /nwprod/lib/libbufr_d_64.a
    implicit none

    real,parameter:: r0_01=0.0
    real,parameter:: r20=20.0
    real,parameter:: r100=100.0

    integer ludx,nobs,nlev,idate
    character(8) subset,sid(1)
    real,dimension(1):: typ
    real,dimension(1,1):: zob,cat
    real ob1,ob2
    real,dimension(1,1):: qm
    real,dimension(1):: xob,yob,dhr

    integer n,k,iret
    real:: bmiss=10.e10
    real hdr(10),obs(10,255),qms(10,255),err(10,255)

    character(80):: hdrstr='SID XOB YOB DHR TYP'
    character(80):: obsstr='POB QOB TOB ZOB UOB VOB CAT'
    character(80):: qmsstr='PQM QQM TQM ZQM WQM'
    character(80):: errstr='POE QOE TOE WOE'

    real oblat, oblon,obhourset,obpres,oberror
    integer lendian_in
    character(10) oneob_type

    namelist/setup/oneob_type,oblat,oblon,obpres,ob1,ob2,obhourset,&
         oberror,idate

! SET DEFAULTS
    oneob_type='ps'
    oblat=45.
    oblon=180.
    ob1=1.
    ob2=0.
    obpres=1000
    obhourset=0
    oberror=1.
    idate=2008091000

! READ IN NAMELIST
    open(11,file='oneob.parm')
    read(11,setup)
    close(11)

    write(6,*) 'GENERATE PREPQC WITH THE FOLLOWING PARMS :'
    write(6,setup)

! set values from parameter list
    lendian_in=15
    xob=oblon
    yob=oblat
    dhr=obhourset
    write(6,*)idate

! set default values for this routine
    ludx=22
    nobs=1
    nlev=1
    subset='ADPUPA'
    sid='SID00001'
    zob=0
    qm=1

    if (oneob_type=='ps') then
       cat(1,1)=0
       typ(1)=87
       write(6,*) 'set to type 187'
    else
       typ(1)=20.
       cat(1,1)=1
    endif

    open(ludx,file='prepobs_prep.bufrtable',action='read')
    open(lendian_in,file='prepqc',action='write',form='unformatted')

    call datelen(10)
    call openbf(lendian_in,'OUT',ludx)
    do n=1,nobs
       hdr(1)=transfer(sid(n),hdr(1))
       hdr(2)=xob(n)
       hdr(3)=yob(n)
       hdr(4)=dhr(n)     
       if ( (oneob_type=='uv') ) then
          hdr(5)=200+typ(n)
       else
          hdr(5)=100+typ(n)
       end if

       obs=bmiss
       qms=bmiss
       err=bmiss

       do k=1,nlev
          obs(1,k)=obpres
          obs(7,k)=cat(k,n)
          qms(1,k)=qm(k,n)
          err(1,k)=oberror

          if (oneob_type=='t') then
            obs(3,k)=ob1
            qms(3,k)=qm(k,n)
            err(3,k)=oberror
            qms(2,k)=qm(k,n)
          else if (oneob_type=='ps') then
            obs(4,k)=ob1  ! observation "height"
            qms(4,k)=qm(k,n)
          else if (oneob_type=='q') then
            obs(2,k)=ob1
            qms(2,k)=qm(k,n)
            err(2,k)=oberror
            qms(3,k)=qm(k,n)
          else if ( (oneob_type=='uv') ) then
            obs(5,k)=ob1
            obs(6,k)=ob2
            qms(5,k)=qm(k,n)
            err(4,k)=oberror
          end if
       enddo

       call openmb(lendian_in,subset,idate)
       call ufbint(lendian_in,hdr,10,1,iret,hdrstr)
       call ufbint(lendian_in,obs,10,nlev,iret,obsstr)
       call ufbint(lendian_in,qms,10,nlev,iret,qmsstr)
       call ufbint(lendian_in,err,10,nlev,iret,errstr)
       call writsb(lendian_in)

    enddo
    call closbf(lendian_in)

end program makeoneobbufr

