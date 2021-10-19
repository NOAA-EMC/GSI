!   intype  : the observarion type like t for tem., uv for wind
!   stype   : the observation sub type, like t120 uv220

!   use conmon_read_time_diag
   use conmon_process_time_data

   implicit none


   integer np,mregion,nobs, np_gps
   integer ntype_ps,ntype_q,ntype_t,ntype_uv,ntype_gps

   parameter(np=13)
   parameter(np_gps=25)
   parameter(mregion=10)
   real(4),dimension(np)  :: ptop,pbot,ptopq,pbotq
   real(4),dimension(np_gps) :: htop_gps, hbot_gps
   integer,dimension(200) :: iotype_ps,iotype_q,iotype_uv,iotype_t, iotype_gps
   integer,dimension(200) :: iosubtype_ps,iosubtype_q,iosubtype_uv,iosubtype_t, iosubtype_gps
   integer,dimension(200) :: ituse_ps,ituse_q,ituse_uv,ituse_t, ituse_gps
   real(4),dimension(200,2) :: varqc_ps,varqc_q,varqc_uv,varqc_t, varqc_gps
   character(len=7) dtype_ps,dtype_uv,dtype_t,dtype_q, dtype_gps

   character(40),dimension(mregion):: region

   real,dimension(mregion):: rlatmin,rlatmax,rlonmin,rlonmax
   integer lunin,lunot,nregion

   data lunin / 11 /
   data lunot / 21 /

   character(100)        :: input_file          = "conv_diag"
   logical               :: netcdf              = .false.
   character(3)          :: run                 = "ges"
   character(3)          :: ctype               = "   "         ! this only has meaning with 
                                                                ! NetCDF formatted diag files
   namelist /input/input_file,nregion,netcdf,run,ctype,region,rlatmin,rlatmax,rlonmin,rlonmax

   read(5,input)
   write(6,*)' User input below'
   write(6,input)

   call set_netcdf_flag( netcdf )

   dtype_ps='ps'
   dtype_uv='uv'
   dtype_t='t'
   dtype_q='q'
   dtype_gps='gps'


   nobs=0
   ptop(1)  =    0.0;    pbot(1)  =  2000.0
   ptop(2)  = 1000.0;    pbot(2)  =  2000.0
   ptop(3)  =  900.0;    pbot(3)  =   999.9
   ptop(4)  =  800.0;    pbot(4)  =   899.9
   ptop(5)  =  600.0;    pbot(5)  =   799.9
   ptop(6)  =  400.0;    pbot(6)  =   599.9
   ptop(7)  =  300.0;    pbot(7)  =   399.9
   ptop(8)  =  250.0;    pbot(8)  =   299.9
   ptop(9)  =  200.0;    pbot(9)  =   249.9
   ptop(10) =  150.0;    pbot(10) =   199.9
   ptop(11) =  100.0;    pbot(11) =   149.9
   ptop(12) =   50.0;    pbot(12) =    99.9
   ptop(13) =    0.0;    pbot(13) =    49.9

   ptopq(1)  =    0.0;   pbotq(1)  = 2000.0
   ptopq(2)  = 1000.0;   pbotq(2)  = 2000.0
   ptopq(3)  =  950.0;   pbotq(3)  =  999.9
   ptopq(4)  =  900.0;   pbotq(4)  =  949.9
   ptopq(5)  =  850.0;   pbotq(5)  =  899.9
   ptopq(6)  =  800.0;   pbotq(6)  =  849.9
   ptopq(7)  =  750.0;   pbotq(7)  =  799.9
   ptopq(8)  =  700.0;   pbotq(8)  =  749.9
   ptopq(9)  =  600.0;   pbotq(9)  =  699.9
   ptopq(10) =  500.0;   pbotq(10) =  599.9
   ptopq(11) =  400.0;   pbotq(11) =  499.9
   ptopq(12) =  300.0;   pbotq(12) =  399.9
   ptopq(13) =    0.0;   pbotq(13) =  299.9

   !------------------------------------------------------------
   ! htop and hbot for gpsro data is height in km, not pressure
   !
   htop_gps(1)  =    0.0;    hbot_gps(1)  = 60.0         ! all levels
   htop_gps(2)  =   55.01;   hbot_gps(2)  = 60.0
   htop_gps(3)  =   50.01;   hbot_gps(3)  = 55.0
   htop_gps(4)  =   45.01;   hbot_gps(4)  = 50.0
   htop_gps(5)  =   40.01;   hbot_gps(5)  = 45.0
   htop_gps(6)  =   35.01;   hbot_gps(6)  = 40.0
   htop_gps(7)  =   30.01;   hbot_gps(7)  = 35.0
   htop_gps(8)  =   25.01;   hbot_gps(8)  = 30.0
   htop_gps(9)  =   20.01;   hbot_gps(9)  = 25.0
   htop_gps(10) =   15.01;   hbot_gps(10) = 20.0
   htop_gps(11) =   10.01;   hbot_gps(11) = 15.0
   htop_gps(12) =    5.01;   hbot_gps(12) = 10.0
   htop_gps(13) =    0.0;    hbot_gps(13) =  5.0


   call convinfo(iotype_ps,iotype_q,iotype_t,iotype_uv,iotype_gps,ntype_ps,ntype_q,ntype_t,ntype_uv, ntype_gps,&
                varqc_ps,varqc_q,varqc_t,varqc_uv, varqc_gps,&
                ituse_ps,ituse_q,ituse_t,ituse_uv, ituse_gps,&
                iosubtype_ps,iosubtype_q,iosubtype_t,iosubtype_uv, iosubtype_gps)
   print *, 'iotype_gps = ', iotype_gps

   call process_conv_diag(input_file,ctype,mregion,nregion,np,ptop,pbot,ptopq,pbotq,&
                          htop_gps, hbot_gps, &
                 rlatmin,rlatmax,rlonmin,rlonmax,iotype_ps,iotype_q,&
                 iotype_t,iotype_uv,iotype_gps, varqc_ps,varqc_q,varqc_t,varqc_uv, varqc_gps,&
                 ntype_ps,ntype_q,ntype_t,ntype_uv, ntype_gps,&
                 iosubtype_ps,iosubtype_q,iosubtype_t,iosubtype_uv, iosubtype_gps) 
   

   call creatstas_ctl(dtype_ps,iotype_ps,ituse_ps,100,ntype_ps,1,nregion,18,region,&
                     rlatmin,rlatmax,rlonmin,rlonmax,iosubtype_ps) 
   call creatstas_ctl(dtype_q,iotype_q,ituse_q,100,ntype_q,np,nregion,18,region,&
                     rlatmin,rlatmax,rlonmin,rlonmax,iosubtype_q) 
   call creatstas_ctl(dtype_t,iotype_t,ituse_t,100,ntype_t,np,nregion,18,&
                     region,rlatmin,rlatmax,rlonmin,rlonmax,iosubtype_t) 
   call creatstas_ctl(dtype_uv,iotype_uv,ituse_uv,100,ntype_uv,np,nregion,18,&
                     region,rlatmin,rlatmax,rlonmin,rlonmax,iosubtype_uv) 

   call creatstas_ctl(dtype_gps,iotype_gps,ituse_gps,100,ntype_gps,np,nregion,18,region,&
                     rlatmin,rlatmax,rlonmin,rlonmax,iosubtype_gps) 

   stop

   end 
