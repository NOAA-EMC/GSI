PROGRAM ecm_gfs_look_alike
!-----------------------------------------------------------------------
!$$$  MAIN PROGRAM DOCUMENTATION BLOCK
!                .      .    .                                       .
! Usage: ecm_gfs_look_alike ecm_file ecm_gfs_look-alike_file
!
! MAIN PROGRAM:  ecm_gfs_look_alike  make gfs look-alike files from ecm files 
!                for forecast files .ge. 24hrs, convert precip to 12hr accumulation
!   PRGMMR: Kistler          ORG: W/NP23      DATE: 2004-01-22
!
! ABSTRACT: convert ECMWF grib files to look like those produced from GFS
!           decimal scaling is that of the gfs files
!           
!PROGRAM HISTORY LOG:
!
! USAGE:
!   INPUT FILES:
!     FORT.11  -  ecm grib file 
!     FORT.21  -  avn grib file valid t-12
!
!   OUTPUT FILES:
!     FORT.51  -  gfs look-alike file
!
!   SUBPROGRAMS CALLED:
!     UNIQUE:    getgbh,getgb,putgb,idsdef
!
!   LIBRARY:
!     COMMON:    W3LIB,BUFRLIB
!
!   EXIT STATES:
!     COND =   0 - SUCCESSFUL RUN
!              .ne.0 - see return codes for the library subroutine
!
! REMARKS:
!
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 90
!
!$$$

!-----------------------------------------------------------------------

    implicit none

! command line parameter stuff
	integer narg,iargc
	character(255) cfpgb1,cfpgb2,cfpgb3
	integer ncfpgb1,ncfpgb2,ncfpgb3
	integer,parameter:: lupgb1=11,lupgb2=51,lupgb3=21
! end command line parameter stuff

	real grav_polar / 9.8321849378/        !                     (m/s2)

    integer jpds(200),jgds(200),kpds(200),kgds(200)

    real,      allocatable ::  ecm(:),  gfs(:) , ecm2(:)
    logical(1),allocatable ::  lecm(:), lgfs (:), lecm2(:)

	integer ids(255)
    integer ji,i,j,jj,k,kk,iret,kpds4,kpds14,kpds15,ki,kb1,kb2,lb
	integer kpds5,kpds6,kpds7

    !-----------------------------------------------------------------------
	! read/process 2/3 file names from command line parameters 
    !-----------------------------------------------------------------------
	narg=iargc()
	if(narg.lt.2 .or. narg.gt.4) then
		call errmsg('Usage: ecm_file ecm_gfs_look-alike_file [ecm_file at t-12]')
		call errexit(1)
	endif

	call getarg(1,cfpgb1)
	ncfpgb1=len_trim(cfpgb1)
    call baopenr(lupgb1,cfpgb1,iret)
	print*,' baopen ',cfpgb1,' iret =',iret
    if (iret .ne. 0) then
		call errexit(1)
	endif

	call getarg(2,cfpgb2)
	ncfpgb2=len_trim(cfpgb2)
    call baopenwt(lupgb2,cfpgb2,iret)
	print*,' baopenwt ',cfpgb2,' iret =',iret
    if (iret .ne. 0) then
		call errexit(1)
    endif

	if (narg .eq. 3) then
		call getarg(3,cfpgb3)
		ncfpgb3=len_trim(cfpgb3)
		call baopenr(lupgb3,cfpgb3,iret)
		print*,' baopenr ',cfpgb3,' iret =',iret
		if (iret .ne. 0) then
			call errexit(1)
		endif
	endif


    !-----------------------------------------------------------------------
	! get default decimal scaling values
    !-----------------------------------------------------------------------
	call idsdef(1,ids) 

    !-----------------------------------------------------------------------
    !  determine horizontal grid dimensions, ji
    !-----------------------------------------------------------------------
    j=-1
    jpds=-1  
    jgds=-1
    call getgbh(lupgb1,0,j,jpds,jgds,j,ji,j,kpds,kgds,iret)
        print*,' getgbh ',cfpgb1,'iret = ',iret
    if (iret .ne. 0) then
		call errexit(1)
    endif
    print*,'ji =',ji
    !-----------------------------------------------------------------------
    ! allocate fcst grids and horizonatal bitmaps
    !-----------------------------------------------------------------------
    allocate (ecm(ji),gfs(ji),lecm(ji),lgfs(ji))
	if (narg .eq. 3) then
		allocate (ecm2(ji),lecm2(ji))
    endif

    ! read ecm and write gfs
    !------------------------------ 
    iret=0 
    k=0 
    do while (iret .eq. 0) 
    ! use cfpgb1 as template for cfpgb2 - i.e. read sequentially 
        j= -1-k
        jpds=-1 
        call getgb(lupgb1,0,ji,j,jpds,jgds,ki,k,kpds,kgds,lecm,ecm,iret)
        if(iret.ne.0) exit
        print*,'get 1,k,kpds(2-7,14-16) = ',k,(kpds(i),i=2,7),(kpds(i),i=14,16)
		kpds14=kpds(14) ! start time
		kpds15=kpds(15) ! end time
        kb1=mod(kpds(4)/64,2) ! .ne.0-bitmap exits
		gfs=ecm
		lgfs=lecm

        !----------------------------------------------------------------
        !reset kpds(5,6,7) and convert values when necessary
        !----------------------------------------------------------------
        select case (kpds(5))
		!           
		! no10Usfc  0 165,1,0  ** surface 10 metre U wind component [m s**-1]
		! UGRD10m   0 33,105,10 ** 10 m u wind [m/s]
			case(165)
				kpds(5)=33; kpds(6)=105 ;kpds(7)=10
		!
		! no10Vsfc  0 166,1,0  ** surface 10 metre V wind component [m s**-1]
		! VGRD10m   0 34,105,10 ** 10 m v wind [m/s]
			case(166)
				kpds(5)=34; kpds(6)=105 ;kpds(7)=10
		!
		! no2Dsfc  0 168,1,0  ** surface 2 metre dewpoint temperature [K]
		! DPT2m    0 17,105,2 ** 2 m Dew Point Temp. [K]
			case(168)
				kpds(5)=17; kpds(6)=105 ;kpds(7)=2
		!
		! no2Tsfc  0 167,1,0  ** surface 2 metre temperature [K]
		! TMP2m    0 11,105,2 ** 2 m Temp. [K]
			case(167)
				kpds(5)=11; kpds(6)=105 ;kpds(7)=2
		!
		! GHprs  14 156,100,0 ** Height [m]
		! HGTprs 14 7,100,0 ** Geopotential height [gpm]
			case(156)
				kpds(5)=7
		!
		! Z kpds5=129 kpds6=100 ** Z=Geopotential [m**2 s**-2]
		! HGTprs 14 7,100,0 ** Geopotential height [gpm]
			case(129)
				gfs=ecm/grav_polar
				kpds(5)=7
		!
		! LNSPhbl  0 152,109,1  ** Logarithm of surface pressure
		! PRESsfc  0 1,1,0      ** surface Pressure [Pa]
			case(152)
				kpds(5)=1; kpds(6)=1 ;kpds(7)=0
				gfs=exp(ecm)
		!
		! MN2Tsfc  0 202,1,0  ** surface Min 2 m temp since previous post [K]
		! TMIN2m   0 16,105,2 ** 2 m Min. temp. [K]
			case(202)
				kpds(5)=16; kpds(6)=105 ;kpds(7)=2
		!
		! MSLsfc    0 151,1,0  ** surface Mean sea-level pressure [Pa]
		! PRMSLmsl  0 2,102,0  ** Pressure reduced to MSL [Pa]
			case(151)
				kpds(5)=2; kpds(6)=102 ;kpds(7)=0
		!
		! MX2Tsfc  0 201,1,0  ** surface Max 2 m temp since previous post [K]
		! TMAX2m   0 15,105,2 ** 2 m Max. temp. [K]
			case(201)
				kpds(5)=15; kpds(6)=105 ;kpds(7)=2
		!
		! Rprs 14 157,100,0 ** Relative humidity [%]
		! RHprs 14 52,100,0 ** Relative humidity [%]
			case(157)
				kpds(5)=52
		!
		! Tprs    14 130,100,0 ** Temperature [K]
		! TMPprs  14  11,100,0 ** Temp. [K]
			case(130)
				kpds(5)=11; kpds(6)=100 
		!
		! TCCsfc  0 164,1,0  ** surface Total cloud cover [(0 - 1)]
		! TCDCclm 0 71,200,0 ** atmos column Total cloud cover [%]
			case(164)
				kpds(5)=71; kpds(6)=244 ;kpds(7)=0
				gfs=100.*ecm
		!
		! TPsfc    0 228,1,0  ** surface Total precipitation [m]
		! APCPsfc  0  61,1,0  ** surface Total precipitation [kg/m^2]
			case(228)
				if (narg .eq. 3) then
					! search via jpds(5,6,7) in cfpgb3 for matching record
					kpds5=228;kpds6=1;kpds7=0
					kk=0
					do while (iret .eq. 0 )
						jj=-1-kk
						jpds=-1
						call getgb(21,0,ji,jj,jpds,jgds,ki,kk,kpds,kgds,lecm2,ecm2,iret)
						if(iret.ne.0) then
							call errmsg('Unable to locate precip on unit 21 :abort')
							call errexit(1)
						endif
						!print*,'get 21,kk,kpds(2-7,14-16) = ',kk,(kpds(i),i=2,7),(kpds(i),i=14,16)
						if (kpds(5).eq.kpds5.and.kpds(6).eq.kpds6.and.kpds(7).eq.kpds7) exit
					enddo

					kpds15=kpds(14) ! reset start time to previous accumulation end time
					kb2=mod(kpds(4)/64,2) ! .ne.0-bitmap exits
					kpds4=max(kpds(4),kpds4)
					print*,'get 2,j,kpds(2-7,14-16) = ',j,(kpds(i),i=2,7),(kpds(i),i=14,16)

					call two_grids('dif',kb1,ecm,lecm, kb2,ecm2,lecm2,ji,gfs ,lgfs)
					kpds(4)=kpds4
					kpds(14)=kpds14
					kpds(15)=kpds15
				endif
				kpds(5)=61; kpds(6)=1 ;kpds(7)=0
				gfs=1e3*gfs
		!
		! Uprs 14 131,100,0 ** U velocity [m s**-1]
		! Uprs 14  33,100,0 ** U velocity [m s**-1]
			case(131)
				kpds(5)=33
		!
		! Vprs 14 132,100,0 ** V velocity [m s**-1]
		! Vprs 14  34,100,0 ** V velocity [m s**-1]
			case(132)
				kpds(5)=34

            case default 
				print*,"record not recognized:kpds(5,6,7)=",(kpds(i),i=5,7)
				cycle
        end select
		!-----------------------------------------------------------------------
		! set version nr of parameter table
		!-----------------------------------------------------------------------
		kpds(19)=3
		!-----------------------------------------------------------------------
		! set default decimal scaling for the variable kpds(5)
		!-----------------------------------------------------------------------
		kpds(22)=ids(kpds(5))

        print*,'put  ,k,kpds(5-7,22) = ',k,(kpds(i),i=5,7),kpds(22)
        call putgb(lupgb2,ji,kpds,kgds,lgfs,gfs,iret)
        if(iret.ne.0) then
            print*,'putgb failed iret = ',iret; stop 1 
        endif
    enddo
    print*,'getgb iret = ',iret,' after j = ',j,' records'

    contains
        subroutine two_grids(coperation,kb1,g1,l1,kb2,g2,l2,ij,g3,l3)
            implicit none
            character*3,    intent(in)  :: coperation
            integer,        intent(in)  :: ij,kb1,kb2
            real,           intent(in)  :: g1(ij),g2(ij)
            logical*1,      intent(in)  :: l1(ij),l2(ij)
            real,           intent(out) :: g3(ij)
            logical*1,      intent(out) :: l3(ij)
            integer i,lb1,lb2,lb3
            real bmiss/1.e10/
            save bmiss

            if (kb1.eq.0.and.kb2.eq.0) then
                l3=.true.
                do i=1,ji 
                    select case(coperation)
                        case('dif')  
                            g3(i)=g1(i)-g2(i)
                        case('avg')  
                            g3(i)=0.5*(g1(i)+g2(i))
                        case('acc')  
                            g3(i)=g1(i)+g2(i)
                        case('min')  
                            g3(i)=min(g1(i),g2(i))
                        case('max')  
                            g3(i)=max(g1(i),g2(i))
                        case default
                            print*, coperation,' not defined';stop 1
                    end select
                enddo
                print*,coperation,' without bitmap'
            else 
				lb1=0;lb2=0;lb3=0
                l3=l1.or.l2
                do i=1,ji 
                    if(l1(i).and.l2(i) ) then
                        l3(i)=.true.
                        select case(coperation)
                            case('avg')  
                                g3(i)=0.5*(g1(i)+g2(i))
                            case('dif')  
                                g3(i)=g1(i)-g2(i)
                            case('acc')  
                                g3(i)=g1(i)+g2(i)
                            case('min')  
                                g3(i)=min(g1(i),g2(i))
                            case('max')  
                                g3(i)=max(g1(i),g2(i))
                            case default
                                print*, coperation,' not defined';stop 1
                        end select
                    else if (l1(i)) then 
						lb2=lb2+0
                        l3(i)=.true.
                        g3(i)=g1(i)
                    else if (l2(i)) then 
						lb1=lb1+1
                        l3(i)=.true.
                        g3(i)=g2(i)
                    else
                        l3(i)=.false.
                        g3(i)=bmiss
						lb3=lb3+1
                    endif
                enddo
                print*,coperation,' kb1,k2b = ',kb1,kb2,' with ',lb1,lb2,lb3, 'bit-map points'
            endif
        end subroutine two_grids
end program ecm_gfs_look_alike 
