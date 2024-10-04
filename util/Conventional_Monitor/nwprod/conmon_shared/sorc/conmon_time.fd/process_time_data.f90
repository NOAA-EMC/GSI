!---------------------------------------------------------------------------------
!  process_time_data.f90
! 
!  module conmon_process_time_data
!
!  This module reads and processes the conventional time series data,
!  storing the results in files that are ready to be plotted in GrADS.
!
!  Both binary and NetCDF formatted conventional diagnostic files are supported. 
!  The binary read is contained in this module.  Reading NetCDF formatted files
!  is done using the conmon_read_diag.F90 module.  The conmon_read_diag module 
!  supports reading binary cnvstat files as well, but since what is here works,
!  and the binary format is being replaced by NetCDF, it didn't make sense to 
!  spend the time to rebuild that which is not broken.
!
!---------------------------------------------------------------------------------

!   intype  : the observarion type like t for tem., uv for wind
!   stype   : the observation sub type, like t120 uv220
!   twork   : the array to hold statistics for temperature: the first variable of 
!             array is vertical level, the second variable is the number of data type
!             the third variable tatistics variable: 1: the total number
!             2:the number of data rejected by variational qc
!             3:bias,4:rms, 5: penalty,6: variational penalty 
!             the fourth variable is region, the fifth variable is the data usuage type
!             1, used, 2, rejected, 3, monited


module conmon_process_time_data


   !--- use ---!
   use nc_diag_read_mod, only: nc_diag_read_init, nc_diag_read_close, &
                           nc_diag_read_get_dim, nc_diag_read_get_global_attr, &
                           nc_diag_read_get_var_names, &
                           nc_diag_read_get_global_attr_names, &
                           nc_diag_read_get_var

   use ncdr_vars, only:    nc_diag_read_check_var

   use conmon_read_diag
 
 
   !--- implicit ---!
   implicit none

   !--- public & private ---!
   private 

   public :: set_netcdf_flag
   public :: process_conv_diag

   !--- common data structures ---!
   logical,save                           :: netcdf           = .false.


   contains

   !------------------------------------------------------------
   ! subroutine set_netcdf_read
   !
   ! set the use_netcdf flag to read either binary (default) or
   !    netcdf formatted diagnostic files.
   !------------------------------------------------------------
   subroutine set_netcdf_flag( use_netcdf )
      logical,intent(in)                     :: use_netcdf


      netcdf = use_netcdf

      call set_netcdf_read( use_netcdf )

   end subroutine set_netcdf_flag


   !------------------------------------------------------------
   ! subroutine process_conv_diag 
   !
   ! Read and process conventional diagnostic files in either 
   ! binary or NetCDF format.  Use the set_netcdf_read routine
   ! to set the netcdf flag.
   !------------------------------------------------------------
   !
   subroutine process_conv_diag(input_file,ctype,mregion,nregion,np, &
           ptop,pbot,ptopq,pbotq, htop_gps, hbot_gps, &
           rlatmin,rlatmax,rlonmin,rlonmax,iotype_ps,iotype_q,&
           iotype_t,iotype_uv,iotype_gps,varqc_ps,varqc_q,varqc_t,varqc_uv,varqc_gps,&
           ntype_ps,ntype_q,ntype_t,ntype_uv,ntype_gps,&
           iosubtype_ps,iosubtype_q,iosubtype_t,iosubtype_uv,iosubtype_gps)

      character(100)           :: input_file
      character(3)             :: ctype         ! only used with NetCDF formatted diag files
      integer                     mregion,nregion,np
      real(4),dimension(np)    :: ptop,pbot,ptopq,pbotq,htop_gps,hbot_gps
      real,dimension(mregion)  :: rlatmin,rlatmax,rlonmin,rlonmax
      integer,dimension(100)   :: iotype_ps,iotype_q,iotype_t,iotype_uv,iotype_gps
      real(4),dimension(100,2) :: varqc_ps,varqc_q,varqc_t,varqc_uv,varqc_gps
      integer                     ntype_ps,ntype_q,ntype_t,ntype_uv,ntype_gps
      integer,dimension(100)   :: iosubtype_ps,iosubtype_q,iosubtype_t,iosubtype_uv, iosubtype_gps

      real(4),dimension(np,100,6,nregion,3)  :: twork,qwork,uwork,vwork,uvwork
      real(4),dimension(1,100,6,nregion,3)   :: pswork

      write(6,*) 'input_file = ', input_file

      if( netcdf ) then
         write(6,*) ' call nc read subroutine'
         call process_conv_nc( input_file, ctype, mregion,nregion,np,ptop,pbot,ptopq,pbotq,&
                               htop_gps, hbot_gps,&
                 rlatmin,rlatmax,rlonmin,rlonmax,iotype_ps,iotype_q,&
                 iotype_t,iotype_uv,iotype_gps,varqc_ps,varqc_q,varqc_t,varqc_uv,varqc_gps,&
                 ntype_ps,ntype_q,ntype_t,ntype_uv,ntype_gps,&
                 iosubtype_ps,iosubtype_q,iosubtype_t,iosubtype_uv,iosubtype_gps,&
                 twork,uwork,vwork,uvwork )
      else
         write(6,*) ' call bin read subroutine'
         call process_conv_bin( input_file,mregion,nregion,np,ptop,pbot,ptopq,pbotq,&
                 rlatmin,rlatmax,rlonmin,rlonmax,iotype_ps,iotype_q,&
                 iotype_t,iotype_uv,varqc_ps,varqc_q,varqc_t,varqc_uv,&
                 ntype_ps,ntype_q,ntype_t,ntype_uv,&
                 iosubtype_ps,iosubtype_q,iosubtype_t,iosubtype_uv, &
                 twork,qwork,uwork,vwork,uvwork, pswork )

         call output_data( twork, qwork, uwork, vwork, uvwork, pswork, &
                        ntype_ps, ntype_q, ntype_t, ntype_uv, nregion, np )
      end if 


   end subroutine process_conv_diag



   !-----------------------------------------------------------
   !  subroutine process_conv_bin
   ! 
   !  This routine reads and processes binary formatted 
   !  conventional diag files. 
   !-----------------------------------------------------------
   subroutine process_conv_bin(input_file,mregion,nregion,np,ptop,pbot,ptopq,pbotq,&
           rlatmin,rlatmax,rlonmin,rlonmax,iotype_ps,iotype_q,&
           iotype_t,iotype_uv,varqc_ps,varqc_q,varqc_t,varqc_uv,&
           ntype_ps,ntype_q,ntype_t,ntype_uv,&
           iosubtype_ps,iosubtype_q,iosubtype_t,iosubtype_uv, &
           twork,qwork,uwork,vwork,uvwork,pswork )

      implicit none


      character(100),intent(in)              :: input_file
      integer, intent(in)                    :: mregion
      integer, intent(in)                    :: nregion
      integer, intent(in)                    :: np
      real(4),dimension(np),intent(in)       :: ptop,pbot,ptopq,pbotq
      real,dimension(mregion),intent(in)     :: rlatmin,rlatmax,rlonmin,rlonmax
      integer,dimension(100),intent(in)      :: iotype_ps,iotype_q,iotype_t,iotype_uv
      real(4),dimension(100,2),intent(in)    :: varqc_ps,varqc_q,varqc_t,varqc_uv
      integer, intent(in)                    :: ntype_ps,ntype_q,ntype_t
      integer,dimension(100),intent(in)      :: iosubtype_ps,iosubtype_q,iosubtype_uv,iosubtype_t

      real(4),dimension(np,100,6,nregion,3), intent(out)  :: twork,qwork,uwork,vwork,uvwork
      real(4),dimension(1,100,6,nregion,3), intent(out)   :: pswork



      real(4),allocatable,dimension(:,:)     :: rdiag 
      character(8),allocatable,dimension(:)  :: cdiag 

      character(3)             :: dtype

      integer nchar,nreal,ii,mype,idate,iflag,itype
      integer lunin,lunot,nreal1,nreal2,ldtype,intype
      integer ilat,ilon,ipress,iqc,iuse,imuse,iwgt,ierr1
      integer ierr2,ierr3,ipsobs,iqobs,ioff02
      integer i,j,k,ltype,iregion,ntype_uv
      integer iobg,iobgu,iobgv
      integer nobs

      data lunin / 11 /
      data lunot / 21 /


      twork=0.0;qwork=0.0;uwork=0.0;vwork=0.0;uvwork=0.0
      pswork=0.0

      itype=1;ilat=3;ilon=4;ipress=6;iqc=9;iuse=11;imuse=12
      iwgt=13;ierr1=14;ierr2=15;ierr3=16;iobg=18;iobgu=18;iobgv=21
   
      write(6,*) 'input_file = ', input_file
      open(lunin,file=input_file,form='unformatted')  
      rewind(lunin)

      read(lunin) idate

      nobs = 0
      loopd: do  
         read(lunin,IOSTAT=iflag) dtype,nchar,nreal,ii,mype,ioff02
         if( iflag /= 0 ) exit loopd

         allocate(cdiag(ii),rdiag(nreal,ii))
         read(lunin,IOSTAT=iflag) cdiag,rdiag

         if( iflag /= 0 ) exit loopd


         if(trim(dtype) == ' ps') then
            call stascal(dtype,rdiag,nreal,ii,iotype_ps,varqc_ps,ntype_ps,&
                         pswork,uwork,vwork,1,ptop,pbot,nregion,mregion,&
                         rlatmin,rlatmax,rlonmin,rlonmax,iosubtype_ps)
            nobs = nobs + 1
         else if(trim(dtype) == '  q') then
            call stascal(dtype,rdiag,nreal,ii,iotype_q,varqc_q,ntype_q,&
                         qwork,uwork,vwork,np,ptopq,pbotq,nregion,mregion,&
                         rlatmin,rlatmax,rlonmin,rlonmax,iosubtype_q)
            nobs = nobs + 1

         else if(trim(dtype) == '  t') then
            call stascal(dtype,rdiag,nreal,ii,iotype_t,varqc_t,ntype_t,&
                         twork,uwork,vwork,np,ptop,pbot,nregion,mregion,&
                         rlatmin,rlatmax,rlonmin,rlonmax,iosubtype_t)
            nobs = nobs + 1

         else if(trim(dtype) == ' uv') then
            call stascal(dtype,rdiag,nreal,ii,iotype_uv,varqc_uv,ntype_uv,&
                         uvwork,uwork,vwork,np,ptop,pbot,nregion,mregion,&
                         rlatmin,rlatmax,rlonmin,rlonmax,iosubtype_uv)
            nobs = nobs + 1
         endif

         deallocate(cdiag,rdiag)

      enddo   loopd               !  ending read data do loop
    
      close(lunin)

      print *, 'nobs processed = ', nobs
    
   end subroutine process_conv_bin



   !-----------------------------------------------------------
   !  subroutine process_conv_nc
   ! 
   !  This routine processes NetCDF formatted conventional
   !  diag files.  Note that NetCDF diag files contain only 
   !  a single type of data (ps, q, t, uv); the cnvstat
   !  tar file contains 4 ges and 4 anl diag files.
   !-----------------------------------------------------------
   subroutine process_conv_nc( input_file, ctype, mregion, nregion, np, &
           ptop, pbot, ptopq, pbotq, htop_gps, hbot_gps, rlatmin, rlatmax, rlonmin, rlonmax, &
           iotype_ps, iotype_q, iotype_t, iotype_uv, iotype_gps, varqc_ps, varqc_q, &
           varqc_t, varqc_uv, varqc_gps, ntype_ps, ntype_q, ntype_t, ntype_uv, ntype_gps,&
           iosubtype_ps, iosubtype_q, iosubtype_t, iosubtype_uv, iosubtype_gps,&
           twork, uwork, vwork, uvwork )

      use generic_list
      use data

      implicit none


      character(100),intent(in)              :: input_file
      character(3),intent(in)                :: ctype
      integer, intent(in)                    :: mregion
      integer, intent(in)                    :: nregion
      integer, intent(in)                    :: np
      real(4),dimension(np),intent(in)       :: ptop,pbot,ptopq,pbotq,htop_gps,hbot_gps
      real,dimension(mregion),intent(in)     :: rlatmin,rlatmax,rlonmin,rlonmax
      integer,dimension(100),intent(in)      :: iotype_ps,iotype_q,iotype_t,iotype_uv,iotype_gps
      real(4),dimension(100,2),intent(in)    :: varqc_ps,varqc_q,varqc_t,varqc_uv,varqc_gps
      integer, intent(in)                    :: ntype_ps,ntype_q,ntype_t,ntype_gps
      integer,dimension(100),intent(in)      :: iosubtype_ps,iosubtype_q,iosubtype_uv,iosubtype_t,iosubtype_gps

      !========================================================================
      !  NOTE:  I think the *work arrays don't need to be params -- they are
      !  just used here and can then be deallocated w/o issue.
      !========================================================================
      
      real(4),dimension(np,100,6,nregion,3), intent(out)  :: twork,uwork,vwork,uvwork
      real(4),dimension(np,100,6,nregion,3)  :: qwork, gpswork
      real(4),dimension(1,100,6,nregion,3)   :: pswork


      type(list_node_t), pointer             :: list => null()
      type(list_node_t), pointer             :: next => null()
      type(data_ptr)                         :: ptr

      real(4),allocatable,dimension(:,:)     :: rdiag 
      character(8),allocatable,dimension(:)  :: cdiag 
      integer                                :: nobs = 0
      character(3)                           :: dtype

      integer jj, obs_ctr, k,ltype,iregion,ntype_uv


      print *, '--> process_conv_nc'
      print *, '      input_file = ', input_file
      print *, '      ctype      = ', ctype     

      twork=0.0; qwork=0.0; uwork=0.0; vwork=0.0; uvwork=0.0; pswork=0.0; gpswork=0.0

      call conmon_return_all_obs( input_file, ctype, nobs, list )
      print *, 'nobs read = ', nobs

      allocate( rdiag( max_rdiag_reals, nobs ))
      next => list
      
      !-------------------------------------------------------------------
      ! transfer data from list to rdiag array and accumulate stats using
      ! stascal
      !
      select case( adjustl( trim( ctype ))) 

         case ( 'ps' )          
            print *, ' select, case ps' 

            obs_ctr = 0
            do while ( associated( next ) .eqv. .TRUE. )
               obs_ctr = obs_ctr + 1
               ptr = transfer(list_get( next ), ptr)
               next => list_next( next )

               do jj = 1, max_rdiag_reals
                  rdiag(jj, obs_ctr) = ptr%p%rdiag( jj )
               end do

            end do

            call stascal( ctype, rdiag, max_rdiag_reals, nobs, iotype_ps, varqc_ps, ntype_ps, &
                          pswork, uwork, vwork, 1, ptop, pbot, nregion, mregion, &
                          rlatmin, rlatmax, rlonmin, rlonmax, iosubtype_ps )

            call list_free( list )

            print *, 'found nobs in list = ', obs_ctr

            call output_data_ps( pswork, ntype_ps, nregion, 1 )

         case ( 'q' )
            print *, ' select, case q'

            obs_ctr = 0
            do while ( associated( next ) .eqv. .TRUE. )
               obs_ctr = obs_ctr + 1
               ptr = transfer(list_get( next ), ptr)
               next => list_next( next )

               do jj = 1, max_rdiag_reals
                  rdiag(jj, obs_ctr) = ptr%p%rdiag( jj )
               end do

            end do

            print *, 'found nobs in list = ', obs_ctr
            call list_free( list )

            call stascal(ctype, rdiag, max_rdiag_reals, nobs, iotype_q, varqc_q, ntype_q, &
                         qwork, uwork, vwork, np, ptop, pbot, nregion, mregion, &
                         rlatmin, rlatmax, rlonmin, rlonmax, iosubtype_q)

            call output_data_q( qwork, ntype_q, nregion, np )


         case ( 't' )
            print *, ' select, case t'

            obs_ctr = 0
            do while ( associated( next ) .eqv. .TRUE. )
               obs_ctr = obs_ctr + 1
               ptr = transfer(list_get( next ), ptr)
               next => list_next( next )

               do jj = 1, max_rdiag_reals
                  rdiag(jj, obs_ctr) = ptr%p%rdiag( jj )
               end do

            end do

            print *, 'found nobs in list = ', obs_ctr
            call list_free( list )

            call stascal(ctype, rdiag, max_rdiag_reals, nobs, iotype_t, varqc_t, ntype_t, &
                         twork, uwork, vwork, np, ptop, pbot, nregion, mregion, &
                         rlatmin, rlatmax, rlonmin, rlonmax, iosubtype_t)

            call output_data_t( twork, ntype_t, nregion, np )

         case ( 'uv' )
            print *, ' select, case uv'

            obs_ctr = 0
            do while ( associated( next ) .eqv. .TRUE. )
               obs_ctr = obs_ctr + 1
               ptr = transfer(list_get( next ), ptr)
               next => list_next( next )

               do jj = 1, max_rdiag_reals
                  rdiag(jj, obs_ctr) = ptr%p%rdiag( jj )
               end do

            end do

            print *, 'found nobs in list = ', obs_ctr
            call list_free( list )

            call stascal(ctype, rdiag, max_rdiag_reals, nobs, iotype_uv, varqc_uv, ntype_uv, &
                         uvwork, uwork, vwork, np, ptop, pbot, nregion, mregion, &
                         rlatmin, rlatmax, rlonmin, rlonmax, iosubtype_uv)

            call output_data_uv( uvwork, uwork, vwork, ntype_uv, nregion, np )

         case ( 'gps' )
            print *, ' select, case gps'
            obs_ctr = 0
            do while ( associated( next ) .eqv. .TRUE. )
               obs_ctr = obs_ctr + 1
               ptr = transfer(list_get( next ), ptr)
               next => list_next( next )

               do jj = 1, max_rdiag_reals
                  rdiag(jj, obs_ctr) = ptr%p%rdiag( jj )
               end do

            end do

            print *, 'found nobs in list = ', obs_ctr
            call list_free( list )
      
            call stascal_gps(ctype, rdiag, max_rdiag_reals, nobs, iotype_gps, varqc_gps, ntype_gps, &
                         gpswork, np, htop_gps, hbot_gps, nregion, mregion, &
                         rlatmin, rlatmax, rlonmin, rlonmax, iosubtype_gps)

            call output_data_gps( gpswork, ntype_gps, nregion, np, iotype_gps )

      end select


      if( allocated( rdiag )) deallocate( rdiag )

      print *, '<-- process_conv_nc'

   end subroutine process_conv_nc



   subroutine output_data_ps( pswork, ntype_ps, nregion, np )

      real(4),dimension(1,100,6,nregion,3), intent(inout)   :: pswork
      integer, intent(in)                                   :: ntype_ps, nregion, np

      integer                                               :: ii, jj, ltype, iregion
      integer, parameter                                    :: outfile = 21

      write(6,*) '--> output_data_ps'

      do iregion = 1, nregion

         do jj = 1, 3

            do ltype=1,ntype_ps

               pswork(1,ntype_ps+1,1,iregion,jj)= &
                     pswork(1,ntype_ps+1,1,iregion,jj) + pswork(1,ltype,1,iregion,jj)
               pswork(1,ntype_ps+1,2,iregion,jj)= &
                     pswork(1,ntype_ps+1,2,iregion,jj) + pswork(1,ltype,2,iregion,jj)
               pswork(1,ntype_ps+1,3,iregion,jj)= &
                     pswork(1,ntype_ps+1,3,iregion,jj) + pswork(1,ltype,3,iregion,jj)
               pswork(1,ntype_ps+1,4,iregion,jj)= &
                     pswork(1,ntype_ps+1,4,iregion,jj) + pswork(1,ltype,4,iregion,jj)
               pswork(1,ntype_ps+1,5,iregion,jj)= &
                     pswork(1,ntype_ps+1,5,iregion,jj) + pswork(1,ltype,5,iregion,jj)
               pswork(1,ntype_ps+1,6,iregion,jj)= &
                     pswork(1,ntype_ps+1,6,iregion,jj) + pswork(1,ltype,6,iregion,jj)

               if(pswork(1,ltype,1,iregion,jj) >=1.0) then
                  pswork(1,ltype,3,iregion,jj)= &
                        pswork(1,ltype,3,iregion,jj)/pswork(1,ltype,1,iregion,jj)
                  pswork(1,ltype,4,iregion,jj)= &
                        sqrt(pswork(1,ltype,4,iregion,jj)/pswork(1,ltype,1,iregion,jj))
                  pswork(1,ltype,5,iregion,jj)= &
                        pswork(1,ltype,5,iregion,jj)/pswork(1,ltype,1,iregion,jj)
                  pswork(1,ltype,6,iregion,jj)= &
                        pswork(1,ltype,6,iregion,jj)/pswork(1,ltype,1,iregion,jj)
               endif
            enddo

            !----------------------------------------------
            !   for the total surface pressure statistics
            !
            if(pswork(1,ntype_ps+1,1,iregion,jj) >=1.0) then
               pswork(1,ntype_ps+1,3,iregion,jj) = pswork(1,ntype_ps+1,3,iregion,jj)/&
                                       pswork(1,ntype_ps+1,1,iregion,jj)
               pswork(1,ntype_ps+1,4,iregion,jj) = sqrt(pswork(1,ntype_ps+1,4,iregion,jj)&
                                    /pswork(1,ntype_ps+1,1,iregion,jj))
               pswork(1,ntype_ps+1,5,iregion,jj) = pswork(1,ntype_ps+1,5,iregion,jj)/&
                                    pswork(1,ntype_ps+1,1,iregion,jj)
               pswork(1,ntype_ps+1,6,iregion,jj) = pswork(1,ntype_ps+1,6,iregion,jj)/&
                                    pswork(1,ntype_ps+1,1,iregion,jj)
            endif

         enddo
      enddo

      !--------------------
      !  write stas file
      open( outfile, file='ps_stas', form='unformatted')    
      do jj=1,3
         do ii=1,6
            write( outfile ) ((pswork(1,ltype,ii,iregion,jj),ltype=1,ntype_ps+1),iregion=1,nregion)
         enddo
      enddo

      close( outfile )
      write(6,*) '<-- output_data_ps'

   end subroutine output_data_ps


   subroutine output_data_q( qwork, ntype_q, nregion, np )

      real(4),dimension(np,100,6,nregion,3), intent(inout)  :: qwork
      integer, intent(in)                                   :: ntype_q, nregion, np

      integer                                               :: ii, jj, kk, ltype, iregion
      integer, parameter                                    :: outfile = 31

                                    
      do iregion=1,nregion
         do jj=1,3
            do kk=1,np
               do ltype=1,ntype_q
                  qwork(kk,ntype_q+1,1,iregion,jj) = &
                           qwork(kk,ntype_q+1,1,iregion,jj)+qwork(kk,ltype,1,iregion,jj)
                  qwork(kk,ntype_q+1,2,iregion,jj) = &
                           qwork(kk,ntype_q+1,2,iregion,jj)+qwork(kk,ltype,2,iregion,jj)
                  qwork(kk,ntype_q+1,3,iregion,jj) = &
                           qwork(kk,ntype_q+1,3,iregion,jj)+qwork(kk,ltype,3,iregion,jj)
                  qwork(kk,ntype_q+1,4,iregion,jj) = &
                           qwork(kk,ntype_q+1,4,iregion,jj)+qwork(kk,ltype,4,iregion,jj)
                  qwork(kk,ntype_q+1,5,iregion,jj) = &
                           qwork(kk,ntype_q+1,5,iregion,jj)+qwork(kk,ltype,5,iregion,jj)
                  qwork(kk,ntype_q+1,6,iregion,jj) = &
                           qwork(kk,ntype_q+1,6,iregion,jj)+qwork(kk,ltype,6,iregion,jj)

                  if(qwork(kk,ltype,1,iregion,jj) >=1.0) then
                     qwork(kk,ltype,3,iregion,jj) = &
                           qwork(kk,ltype,3,iregion,jj)/qwork(kk,ltype,1,iregion,jj)
                     qwork(kk,ltype,4,iregion,jj) = &
                           sqrt(qwork(kk,ltype,4,iregion,jj)/qwork(kk,ltype,1,iregion,jj))
                     qwork(kk,ltype,5,iregion,jj) = &
                           qwork(kk,ltype,5,iregion,jj)/qwork(kk,ltype,1,iregion,jj)
                     qwork(kk,ltype,6,iregion,jj) = &
                           qwork(kk,ltype,6,iregion,jj)/qwork(kk,ltype,1,iregion,jj)
                  endif
               enddo

               if(qwork(kk,ntype_q+1,1,iregion,jj) >=1.0) then
                  qwork(kk,ntype_q+1,3,iregion,jj)=qwork(kk,ntype_q+1,3,iregion,jj)/&
                                    qwork(kk,ntype_q+1,1,iregion,jj)
                  qwork(kk,ntype_q+1,4,iregion,jj)=sqrt(qwork(kk,ntype_q+1,4,iregion,jj)/&
                                    qwork(kk,ntype_q+1,1,iregion,jj))
                  qwork(kk,ntype_q+1,5,iregion,jj)=qwork(kk,ntype_q+1,5,iregion,jj)/&
                                    qwork(kk,ntype_q+1,1,iregion,jj)
                  qwork(kk,ntype_q+1,6,iregion,jj)=qwork(kk,ntype_q+1,6,iregion,jj)/&
                                    qwork(kk,ntype_q+1,1,iregion,jj)
               endif
            enddo
         enddo
      enddo


      !--------------------
      !  write stas file
      !
      open( outfile, file='q_stas', form='unformatted' )

      do jj=1,3
         do ii=1,6
            do kk=1,np
               write( outfile ) (( qwork( kk,ltype,ii,iregion,jj ), ltype=1, ntype_q+1 ), iregion=1, nregion )
            enddo
         enddo
      enddo

      close( outfile )

      write(6,*) '<-- output_data_q'

   end subroutine output_data_q



   subroutine output_data_t( twork, ntype_t, nregion, np )

      real(4),dimension(np,100,6,nregion,3), intent(inout)  :: twork
      integer, intent(in)                                   :: ntype_t, nregion, np

      integer                                               :: ii, jj, kk, ltype, iregion
      integer, parameter                                    :: outfile = 41


      write(6,*) '--> output_data'
      do iregion=1,nregion
         do jj=1,3
            do kk=1,np
               do ltype=1,ntype_t
                  twork(kk,ntype_t+1,1,iregion,jj) = &
                           twork(kk,ntype_t+1,1,iregion,jj)+twork(kk,ltype,1,iregion,jj)
                  twork(kk,ntype_t+1,2,iregion,jj) = &
                           twork(kk,ntype_t+1,2,iregion,jj)+twork(kk,ltype,2,iregion,jj)
                  twork(kk,ntype_t+1,3,iregion,jj) = &
                           twork(kk,ntype_t+1,3,iregion,jj)+twork(kk,ltype,3,iregion,jj)
                  twork(kk,ntype_t+1,4,iregion,jj) = &
                           twork(kk,ntype_t+1,4,iregion,jj)+twork(kk,ltype,4,iregion,jj)
                  twork(kk,ntype_t+1,5,iregion,jj) = &
                           twork(kk,ntype_t+1,5,iregion,jj)+twork(kk,ltype,5,iregion,jj)
                  twork(kk,ntype_t+1,6,iregion,jj) = &
                           twork(kk,ntype_t+1,6,iregion,jj)+twork(kk,ltype,6,iregion,jj)
   
                  if(twork(kk,ltype,1,iregion,jj) >=1.0) then
                     twork(kk,ltype,3,iregion,jj) = &
                           twork(kk,ltype,3,iregion,jj)/twork(kk,ltype,1,iregion,jj)
                     twork(kk,ltype,4,iregion,jj) = &
                           sqrt(twork(kk,ltype,4,iregion,jj)/twork(kk,ltype,1,iregion,jj))
                     twork(kk,ltype,5,iregion,jj) = &
                           twork(kk,ltype,5,iregion,jj)/twork(kk,ltype,1,iregion,jj)
                     twork(kk,ltype,6,iregion,jj) = &
                           twork(kk,ltype,6,iregion,jj)/twork(kk,ltype,1,iregion,jj)
                  endif
               enddo

               if(twork(kk,ntype_t+1,1,iregion,jj) >=1.0) then
                  twork(kk,ntype_t+1,3,iregion,jj) = twork(kk,ntype_t+1,3,iregion,jj)/&
                                    twork(kk,ntype_t+1,1,iregion,jj)
                  twork(kk,ntype_t+1,4,iregion,jj)=sqrt(twork(kk,ntype_t+1,4,iregion,jj)/&
                                    twork(kk,ntype_t+1,1,iregion,jj))
                  twork(kk,ntype_t+1,5,iregion,jj)=twork(kk,ntype_t+1,5,iregion,jj)/&
                                    twork(kk,ntype_t+1,1,iregion,jj)
                  twork(kk,ntype_t+1,6,iregion,jj)=twork(kk,ntype_t+1,6,iregion,jj)/&
                                    twork(kk,ntype_t+1,1,iregion,jj)
               endif
            enddo       ! kk
         enddo          ! jj
      enddo             ! nregion


      !--------------------
      !  write stas file
      !
      open( outfile, file='t_stas', form='unformatted' )

      do jj=1,3
         do ii=1,6
            do kk=1,np
               write( outfile ) (( twork( kk,ltype,ii,iregion,jj ), ltype=1, ntype_t+1 ), iregion=1, nregion )
            enddo
         enddo
      enddo

      close( outfile )


   end subroutine output_data_t



   subroutine output_data_uv( uvwork, uwork, vwork, ntype_uv, nregion, np )

      real(4),dimension(np,100,6,nregion,3), intent(inout)  :: uvwork, uwork, vwork
      integer, intent(in)                                   :: ntype_uv, nregion, np

      integer                                               :: ii, jj, kk, ltype, iregion
      integer, parameter                                    :: outfile = 51


      write(6,*) '--> output_data_uv'
      do iregion=1,nregion
         do jj=1,3
            do kk=1,np
               do ltype=1,ntype_uv
                  uvwork(kk,ntype_uv+1,1,iregion,jj) = &
                           uvwork(kk,ntype_uv+1,1,iregion,jj)+uvwork(kk,ltype,1,iregion,jj)
                  uvwork(kk,ntype_uv+1,2,iregion,jj) = &
                           uvwork(kk,ntype_uv+1,2,iregion,jj)+uvwork(kk,ltype,2,iregion,jj)
                  uvwork(kk,ntype_uv+1,3,iregion,jj) = &
                           uvwork(kk,ntype_uv+1,3,iregion,jj)+uvwork(kk,ltype,3,iregion,jj)
                  uvwork(kk,ntype_uv+1,4,iregion,jj) = &
                           uvwork(kk,ntype_uv+1,4,iregion,jj)+uvwork(kk,ltype,4,iregion,jj)
                  uvwork(kk,ntype_uv+1,5,iregion,jj) = &
                           uvwork(kk,ntype_uv+1,5,iregion,jj)+uvwork(kk,ltype,5,iregion,jj)
                  uvwork(kk,ntype_uv+1,6,iregion,jj) = &
                           uvwork(kk,ntype_uv+1,6,iregion,jj)+uvwork(kk,ltype,6,iregion,jj)
                  uwork(kk,ntype_uv+1,3,iregion,jj) = &
                           uwork(kk,ntype_uv+1,3,iregion,jj)+uwork(kk,ltype,3,iregion,jj)
                  uwork(kk,ntype_uv+1,4,iregion,jj) = &
                           uwork(kk,ntype_uv+1,4,iregion,jj)+uwork(kk,ltype,4,iregion,jj)
                  vwork(kk,ntype_uv+1,3,iregion,jj) = &
                           vwork(kk,ntype_uv+1,3,iregion,jj)+vwork(kk,ltype,3,iregion,jj)
                  vwork(kk,ntype_uv+1,4,iregion,jj) = &
                           vwork(kk,ntype_uv+1,4,iregion,jj)+vwork(kk,ltype,4,iregion,jj)

                  if(uvwork(kk,ltype,1,iregion,jj) >=1.0) then
                     uvwork(kk,ltype,3,iregion,jj) = &
                           uvwork(kk,ltype,3,iregion,jj)/uvwork(kk,ltype,1,iregion,jj)
                     uvwork(kk,ltype,4,iregion,jj) = &
                           sqrt(uvwork(kk,ltype,4,iregion,jj)/uvwork(kk,ltype,1,iregion,jj))
                     uvwork(kk,ltype,5,iregion,jj) = &
                           uvwork(kk,ltype,5,iregion,jj)/uvwork(kk,ltype,1,iregion,jj)
                     uvwork(kk,ltype,6,iregion,jj) = &
                           uvwork(kk,ltype,6,iregion,jj)/uvwork(kk,ltype,1,iregion,jj)
                     uwork(kk,ltype,1,iregion,jj) = uvwork(kk,ltype,1,iregion,jj)
                     vwork(kk,ltype,1,iregion,jj) = uvwork(kk,ltype,1,iregion,jj)
                     uwork(kk,ltype,2,iregion,jj) = uvwork(kk,ltype,2,iregion,jj)
                     vwork(kk,ltype,2,iregion,jj) = uvwork(kk,ltype,2,iregion,jj)
                     uwork(kk,ltype,3,iregion,jj) = &
                           uwork(kk,ltype,3,iregion,jj)/uvwork(kk,ltype,1,iregion,jj)
                     uwork(kk,ltype,4,iregion,jj) = &
                           sqrt(uwork(kk,ltype,4,iregion,jj)/uvwork(kk,ltype,1,iregion,jj))
                     vwork(kk,ltype,3,iregion,jj) = &
                           vwork(kk,ltype,3,iregion,jj)/uvwork(kk,ltype,1,iregion,jj)
                     vwork(kk,ltype,4,iregion,jj) = &
                           sqrt(vwork(kk,ltype,4,iregion,jj)/uvwork(kk,ltype,1,iregion,jj))
                  endif
               enddo

               if(uvwork(kk,ntype_uv+1,1,iregion,jj) >=1.0) then
                  uvwork(kk,ntype_uv+1,3,iregion,jj)=uvwork(kk,ntype_uv+1,3,iregion,jj)&
                                  /uvwork(kk,ntype_uv+1,1,iregion,jj)
                  uvwork(kk,ntype_uv+1,4,iregion,jj)=sqrt(uvwork(kk,ntype_uv+1,4,iregion,jj)&
                                  /uvwork(kk,ntype_uv+1,1,iregion,jj))
                  uvwork(kk,ntype_uv+1,5,iregion,jj)=uvwork(kk,ntype_uv+1,5,iregion,jj)&
                                  /uvwork(kk,ntype_uv+1,1,iregion,jj)
                  uvwork(kk,ntype_uv+1,6,iregion,jj)=uvwork(kk,ntype_uv+1,6,iregion,jj)&
                                  /uvwork(kk,ntype_uv+1,1,iregion,jj)
                  uwork(kk,ntype_uv+1,1,iregion,jj)=uvwork(kk,ntype_uv+1,1,iregion,jj)
                  uwork(kk,ntype_uv+1,2,iregion,jj)=uvwork(kk,ntype_uv+1,2,iregion,jj)
                  vwork(kk,ntype_uv+1,1,iregion,jj)=uvwork(kk,ntype_uv+1,1,iregion,jj)
                  vwork(kk,ntype_uv+1,2,iregion,jj)=uvwork(kk,ntype_uv+1,2,iregion,jj)
   
                  uwork(kk,ntype_uv+1,3,iregion,jj)=uwork(kk,ntype_uv+1,3,iregion,jj)&
                                  /uwork(kk,ntype_uv+1,1,iregion,jj)
                  uwork(kk,ntype_uv+1,4,iregion,jj)=sqrt(uwork(kk,ntype_uv+1,4,iregion,jj)&
                                  /uwork(kk,ntype_uv+1,1,iregion,jj))
                  vwork(kk,ntype_uv+1,3,iregion,jj)=vwork(kk,ntype_uv+1,3,iregion,jj)&
                                  /vwork(kk,ntype_uv+1,1,iregion,jj)
                  vwork(kk,ntype_uv+1,4,iregion,jj)=sqrt(vwork(kk,ntype_uv+1,4,iregion,jj)&
                                  /vwork(kk,ntype_uv+1,1,iregion,jj))
               endif
   
            enddo    !!! enddo k height
         enddo       !!! enddo j, j=1 assimilated, j=2 rejected, j=3 monitored 
      enddo          !!! enddo iregion region 


      open(51,file='u_stas',form='unformatted')
      do jj=1,3
         do ii=1,6
            do kk=1,np
               write(51) ((uwork(kk,ltype,ii,iregion,jj),ltype=1,ntype_uv+1),iregion=1,nregion)
            enddo
         enddo
      enddo

      open(61,file='v_stas',form='unformatted')
      do jj=1,3
         do ii=1,6
            do kk=1,np
               write(61) ((vwork(kk,ltype,ii,iregion,jj),ltype=1,ntype_uv+1),iregion=1,nregion)
            enddo
         enddo
      enddo
     
      open(71,file='uv_stas',form='unformatted')
      do jj=1,3
         do ii=1,6
            do kk=1,np
               write(71) ((uvwork(kk,ltype,ii,iregion,jj),ltype=1,ntype_uv+1), &
                                   iregion=1,nregion)
            enddo
         enddo
      enddo


      write(6,*) '<-- output_data_uv'

   end subroutine output_data_uv


   subroutine output_data_gps( gpswork, ntype_gps, nregion, np, iotype_gps )

      real(4),dimension(np,100,6,nregion,3), intent(inout)  :: gpswork
      integer, intent(in)                                   :: ntype_gps, nregion, np
      integer,dimension(100), intent(in)                    :: iotype_gps
      integer                                               :: ii, jj, kk, ltype, iregion
      integer, parameter                                    :: outfile = 61
      integer, parameter                                    :: nobsfile = 62
      character(100)                                        :: nobs_outfile
                              
      write(6,*) '--> output_data_gps'

      do iregion=1,nregion
         do jj=1,3
            do kk=1,np
               do ltype=1,ntype_gps
                  gpswork(kk,ntype_gps+1,1,iregion,jj) = &
                           gpswork(kk,ntype_gps+1,1,iregion,jj)+gpswork(kk,ltype,1,iregion,jj)
                  gpswork(kk,ntype_gps+1,2,iregion,jj) = &
                           gpswork(kk,ntype_gps+1,2,iregion,jj)+gpswork(kk,ltype,2,iregion,jj)
                  gpswork(kk,ntype_gps+1,3,iregion,jj) = &
                           gpswork(kk,ntype_gps+1,3,iregion,jj)+gpswork(kk,ltype,3,iregion,jj)
                  gpswork(kk,ntype_gps+1,4,iregion,jj) = &
                           gpswork(kk,ntype_gps+1,4,iregion,jj)+gpswork(kk,ltype,4,iregion,jj)
                  gpswork(kk,ntype_gps+1,5,iregion,jj) = &
                           gpswork(kk,ntype_gps+1,5,iregion,jj)+gpswork(kk,ltype,5,iregion,jj)
                  gpswork(kk,ntype_gps+1,6,iregion,jj) = &
                           gpswork(kk,ntype_gps+1,6,iregion,jj)+gpswork(kk,ltype,6,iregion,jj)

                  if(gpswork(kk,ltype,1,iregion,jj) >=1.0) then
                     gpswork(kk,ltype,3,iregion,jj) = &
                           gpswork(kk,ltype,3,iregion,jj)/gpswork(kk,ltype,1,iregion,jj)
                     gpswork(kk,ltype,4,iregion,jj) = &
                           sqrt(gpswork(kk,ltype,4,iregion,jj)/gpswork(kk,ltype,1,iregion,jj))
                     gpswork(kk,ltype,5,iregion,jj) = &
                           gpswork(kk,ltype,5,iregion,jj)/gpswork(kk,ltype,1,iregion,jj)
                     gpswork(kk,ltype,6,iregion,jj) = &
                           gpswork(kk,ltype,6,iregion,jj)/gpswork(kk,ltype,1,iregion,jj)
                  endif
               enddo

               if(gpswork(kk,ntype_gps+1,1,iregion,jj) >=1.0) then
                  gpswork(kk,ntype_gps+1,3,iregion,jj)=gpswork(kk,ntype_gps+1,3,iregion,jj)/&
                                    gpswork(kk,ntype_gps+1,1,iregion,jj)
                  gpswork(kk,ntype_gps+1,4,iregion,jj)=sqrt(gpswork(kk,ntype_gps+1,4,iregion,jj)/&
                                    gpswork(kk,ntype_gps+1,1,iregion,jj))
                  gpswork(kk,ntype_gps+1,5,iregion,jj)=gpswork(kk,ntype_gps+1,5,iregion,jj)/&
                                    gpswork(kk,ntype_gps+1,1,iregion,jj)
                  gpswork(kk,ntype_gps+1,6,iregion,jj)=gpswork(kk,ntype_gps+1,6,iregion,jj)/&
                                    gpswork(kk,ntype_gps+1,1,iregion,jj)
               endif
            enddo
         enddo
      enddo

      !--------------------
      !  write stas file
      !
      open( outfile, file='gps_stas', form='unformatted' )

      do jj=1,3
         do ii=1,6
            do kk=1,np
               write( outfile ) (( gpswork( kk,ltype,ii,iregion,jj ), ltype=1, ntype_gps+1 ), iregion=1, nregion )
            enddo
         enddo
      enddo

      close( outfile )


      !--------------------
      !  write nobs file
      !
      nobs_outfile='gps.nobs.ges'
      open( nobsfile, file=nobs_outfile, form='formatted', status='new' )

      do ltype=1,ntype_gps
         write(nobsfile,910) ' gps', iotype_gps(ltype), ',00,', int( gpswork(1,ltype,1,1,1) + gpswork(1,ltype,1,1,2) + gpswork(1,ltype,1,1,3) )
         910 format(A,I0,A,I6)
      enddo
      close( nobsfile )


      write(6,*) '<-- output_data_gps'

   end subroutine output_data_gps


   !-------------------------------------
   !  May need to break this into types
   !  for netcdf, because the files only
   !  contain a single type of data.
   !-------------------------------------
   subroutine output_data( twork, qwork, uwork, vwork, uvwork, pswork, &
                           ntype_ps, ntype_q, ntype_t, ntype_uv, nregion, np )

      real(4),dimension(np,100,6,nregion,3), intent(inout)  :: twork,qwork,uwork,vwork,uvwork
      real(4),dimension(1,100,6,nregion,3), intent(inout)   :: pswork
      integer, intent(in)                                   :: ntype_ps,ntype_q,ntype_t,ntype_uv,nregion,np

      integer                                               :: i,j,k,ltype,iregion


      write(6,*) '--> output_data'
      do iregion=1,nregion
         do j=1,3
            do ltype=1,ntype_ps
               pswork(1,ntype_ps+1,1,iregion,j)= &
                     pswork(1,ntype_ps+1,1,iregion,j)+pswork(1,ltype,1,iregion,j)
               pswork(1,ntype_ps+1,2,iregion,j)= &
                     pswork(1,ntype_ps+1,2,iregion,j)+pswork(1,ltype,2,iregion,j)
               pswork(1,ntype_ps+1,3,iregion,j)= &
                     pswork(1,ntype_ps+1,3,iregion,j)+pswork(1,ltype,3,iregion,j)
               pswork(1,ntype_ps+1,4,iregion,j)= &
                     pswork(1,ntype_ps+1,4,iregion,j)+pswork(1,ltype,4,iregion,j)
               pswork(1,ntype_ps+1,5,iregion,j)= &
                     pswork(1,ntype_ps+1,5,iregion,j)+pswork(1,ltype,5,iregion,j)
               pswork(1,ntype_ps+1,6,iregion,j)= &
                     pswork(1,ntype_ps+1,6,iregion,j)+pswork(1,ltype,6,iregion,j)

               if(pswork(1,ltype,1,iregion,j) >=1.0) then
                  pswork(1,ltype,3,iregion,j)= &
                        pswork(1,ltype,3,iregion,j)/pswork(1,ltype,1,iregion,j)
                  pswork(1,ltype,4,iregion,j)= &
                        sqrt(pswork(1,ltype,4,iregion,j)/pswork(1,ltype,1,iregion,j))
                  pswork(1,ltype,5,iregion,j)= &
                        pswork(1,ltype,5,iregion,j)/pswork(1,ltype,1,iregion,j)
                  pswork(1,ltype,6,iregion,j)= &
                        pswork(1,ltype,6,iregion,j)/pswork(1,ltype,1,iregion,j)
               endif
            enddo

            !----------------------------------------------
            !   for the total surface pressure statistics
            !
            if(pswork(1,ntype_ps+1,1,iregion,j) >=1.0) then
               pswork(1,ntype_ps+1,3,iregion,j) = pswork(1,ntype_ps+1,3,iregion,j)/&
                                       pswork(1,ntype_ps+1,1,iregion,j)
               pswork(1,ntype_ps+1,4,iregion,j) = sqrt(pswork(1,ntype_ps+1,4,iregion,j)&
                                    /pswork(1,ntype_ps+1,1,iregion,j))
               pswork(1,ntype_ps+1,5,iregion,j) = pswork(1,ntype_ps+1,5,iregion,j)/&
                                    pswork(1,ntype_ps+1,1,iregion,j)
               pswork(1,ntype_ps+1,6,iregion,j) = pswork(1,ntype_ps+1,6,iregion,j)/&
                                    pswork(1,ntype_ps+1,1,iregion,j)
            endif
                                    
            do k=1,np
               do ltype=1,ntype_q
                  qwork(k,ntype_q+1,1,iregion,j) = &
                           qwork(k,ntype_q+1,1,iregion,j)+qwork(k,ltype,1,iregion,j)
                  qwork(k,ntype_q+1,2,iregion,j) = &
                           qwork(k,ntype_q+1,2,iregion,j)+qwork(k,ltype,2,iregion,j)
                  qwork(k,ntype_q+1,3,iregion,j) = &
                           qwork(k,ntype_q+1,3,iregion,j)+qwork(k,ltype,3,iregion,j)
                  qwork(k,ntype_q+1,4,iregion,j) = &
                           qwork(k,ntype_q+1,4,iregion,j)+qwork(k,ltype,4,iregion,j)
                  qwork(k,ntype_q+1,5,iregion,j) = &
                           qwork(k,ntype_q+1,5,iregion,j)+qwork(k,ltype,5,iregion,j)
                  qwork(k,ntype_q+1,6,iregion,j) = &
                           qwork(k,ntype_q+1,6,iregion,j)+qwork(k,ltype,6,iregion,j)

                  if(qwork(k,ltype,1,iregion,j) >=1.0) then
                     qwork(k,ltype,3,iregion,j) = &
                           qwork(k,ltype,3,iregion,j)/qwork(k,ltype,1,iregion,j)
                     qwork(k,ltype,4,iregion,j) = &
                           sqrt(qwork(k,ltype,4,iregion,j)/qwork(k,ltype,1,iregion,j))
                     qwork(k,ltype,5,iregion,j) = &
                           qwork(k,ltype,5,iregion,j)/qwork(k,ltype,1,iregion,j)
                     qwork(k,ltype,6,iregion,j) = &
                           qwork(k,ltype,6,iregion,j)/qwork(k,ltype,1,iregion,j)
                  endif
               enddo

               if(qwork(k,ntype_q+1,1,iregion,j) >=1.0) then
                  qwork(k,ntype_q+1,3,iregion,j)=qwork(k,ntype_q+1,3,iregion,j)/&
                                    qwork(k,ntype_q+1,1,iregion,j)
                  qwork(k,ntype_q+1,4,iregion,j)=sqrt(qwork(k,ntype_q+1,4,iregion,j)/&
                                    qwork(k,ntype_q+1,1,iregion,j))
                  qwork(k,ntype_q+1,5,iregion,j)=qwork(k,ntype_q+1,5,iregion,j)/&
                                    qwork(k,ntype_q+1,1,iregion,j)
                  qwork(k,ntype_q+1,6,iregion,j)=qwork(k,ntype_q+1,6,iregion,j)/&
                                    qwork(k,ntype_q+1,1,iregion,j)
               endif

               do ltype=1,ntype_t
                  twork(k,ntype_t+1,1,iregion,j) = &
                           twork(k,ntype_t+1,1,iregion,j)+twork(k,ltype,1,iregion,j)
                  twork(k,ntype_t+1,2,iregion,j) = &
                           twork(k,ntype_t+1,2,iregion,j)+twork(k,ltype,2,iregion,j)
                  twork(k,ntype_t+1,3,iregion,j) = &
                           twork(k,ntype_t+1,3,iregion,j)+twork(k,ltype,3,iregion,j)
                  twork(k,ntype_t+1,4,iregion,j) = &
                           twork(k,ntype_t+1,4,iregion,j)+twork(k,ltype,4,iregion,j)
                  twork(k,ntype_t+1,5,iregion,j) = &
                           twork(k,ntype_t+1,5,iregion,j)+twork(k,ltype,5,iregion,j)
                  twork(k,ntype_t+1,6,iregion,j) = &
                           twork(k,ntype_t+1,6,iregion,j)+twork(k,ltype,6,iregion,j)
   
                  if(twork(k,ltype,1,iregion,j) >=1.0) then
                     twork(k,ltype,3,iregion,j) = &
                           twork(k,ltype,3,iregion,j)/twork(k,ltype,1,iregion,j)
                     twork(k,ltype,4,iregion,j) = &
                           sqrt(twork(k,ltype,4,iregion,j)/twork(k,ltype,1,iregion,j))
                     twork(k,ltype,5,iregion,j) = &
                           twork(k,ltype,5,iregion,j)/twork(k,ltype,1,iregion,j)
                     twork(k,ltype,6,iregion,j) = &
                           twork(k,ltype,6,iregion,j)/twork(k,ltype,1,iregion,j)
                  endif
               enddo

               if(twork(k,ntype_t+1,1,iregion,j) >=1.0) then
                  twork(k,ntype_t+1,3,iregion,j) = twork(k,ntype_t+1,3,iregion,j)/&
                                    twork(k,ntype_t+1,1,iregion,j)
                  twork(k,ntype_t+1,4,iregion,j)=sqrt(twork(k,ntype_t+1,4,iregion,j)/&
                                    twork(k,ntype_t+1,1,iregion,j))
                  twork(k,ntype_t+1,5,iregion,j)=twork(k,ntype_t+1,5,iregion,j)/&
                                    twork(k,ntype_t+1,1,iregion,j)
                  twork(k,ntype_t+1,6,iregion,j)=twork(k,ntype_t+1,6,iregion,j)/&
                                    twork(k,ntype_t+1,1,iregion,j)
               endif
   
               do ltype=1,ntype_uv
                  uvwork(k,ntype_uv+1,1,iregion,j) = &
                           uvwork(k,ntype_uv+1,1,iregion,j)+uvwork(k,ltype,1,iregion,j)
                  uvwork(k,ntype_uv+1,2,iregion,j) = &
                           uvwork(k,ntype_uv+1,2,iregion,j)+uvwork(k,ltype,2,iregion,j)
                  uvwork(k,ntype_uv+1,3,iregion,j) = &
                           uvwork(k,ntype_uv+1,3,iregion,j)+uvwork(k,ltype,3,iregion,j)
                  uvwork(k,ntype_uv+1,4,iregion,j) = &
                           uvwork(k,ntype_uv+1,4,iregion,j)+uvwork(k,ltype,4,iregion,j)
                  uvwork(k,ntype_uv+1,5,iregion,j) = &
                           uvwork(k,ntype_uv+1,5,iregion,j)+uvwork(k,ltype,5,iregion,j)
                  uvwork(k,ntype_uv+1,6,iregion,j) = &
                           uvwork(k,ntype_uv+1,6,iregion,j)+uvwork(k,ltype,6,iregion,j)
                  uwork(k,ntype_uv+1,3,iregion,j) = &
                           uwork(k,ntype_uv+1,3,iregion,j)+uwork(k,ltype,3,iregion,j)
                  uwork(k,ntype_uv+1,4,iregion,j) = &
                           uwork(k,ntype_uv+1,4,iregion,j)+uwork(k,ltype,4,iregion,j)
                  vwork(k,ntype_uv+1,3,iregion,j) = &
                           vwork(k,ntype_uv+1,3,iregion,j)+vwork(k,ltype,3,iregion,j)
                  vwork(k,ntype_uv+1,4,iregion,j) = &
                           vwork(k,ntype_uv+1,4,iregion,j)+vwork(k,ltype,4,iregion,j)

                  if(uvwork(k,ltype,1,iregion,j) >=1.0) then
                     uvwork(k,ltype,3,iregion,j) = &
                           uvwork(k,ltype,3,iregion,j)/uvwork(k,ltype,1,iregion,j)
                     uvwork(k,ltype,4,iregion,j) = &
                           sqrt(uvwork(k,ltype,4,iregion,j)/uvwork(k,ltype,1,iregion,j))
                     uvwork(k,ltype,5,iregion,j) = &
                           uvwork(k,ltype,5,iregion,j)/uvwork(k,ltype,1,iregion,j)
                     uvwork(k,ltype,6,iregion,j) = &
                           uvwork(k,ltype,6,iregion,j)/uvwork(k,ltype,1,iregion,j)
                     uwork(k,ltype,1,iregion,j) = uvwork(k,ltype,1,iregion,j)
                     vwork(k,ltype,1,iregion,j) = uvwork(k,ltype,1,iregion,j)
                     uwork(k,ltype,2,iregion,j) = uvwork(k,ltype,2,iregion,j)
                     vwork(k,ltype,2,iregion,j) = uvwork(k,ltype,2,iregion,j)
                     uwork(k,ltype,3,iregion,j) = &
                           uwork(k,ltype,3,iregion,j)/uvwork(k,ltype,1,iregion,j)
                     uwork(k,ltype,4,iregion,j) = &
                           sqrt(uwork(k,ltype,4,iregion,j)/uvwork(k,ltype,1,iregion,j))
                     vwork(k,ltype,3,iregion,j) = &
                           vwork(k,ltype,3,iregion,j)/uvwork(k,ltype,1,iregion,j)
                     vwork(k,ltype,4,iregion,j) = &
                           sqrt(vwork(k,ltype,4,iregion,j)/uvwork(k,ltype,1,iregion,j))
                  endif
               enddo

               if(uvwork(k,ntype_uv+1,1,iregion,j) >=1.0) then
                  uvwork(k,ntype_uv+1,3,iregion,j)=uvwork(k,ntype_uv+1,3,iregion,j)&
                                  /uvwork(k,ntype_uv+1,1,iregion,j)
                  uvwork(k,ntype_uv+1,4,iregion,j)=sqrt(uvwork(k,ntype_uv+1,4,iregion,j)&
                                  /uvwork(k,ntype_uv+1,1,iregion,j))
                  uvwork(k,ntype_uv+1,5,iregion,j)=uvwork(k,ntype_uv+1,5,iregion,j)&
                                  /uvwork(k,ntype_uv+1,1,iregion,j)
                  uvwork(k,ntype_uv+1,6,iregion,j)=uvwork(k,ntype_uv+1,6,iregion,j)&
                                  /uvwork(k,ntype_uv+1,1,iregion,j)
                  uwork(k,ntype_uv+1,1,iregion,j)=uvwork(k,ntype_uv+1,1,iregion,j)
                  uwork(k,ntype_uv+1,2,iregion,j)=uvwork(k,ntype_uv+1,2,iregion,j)
                  vwork(k,ntype_uv+1,1,iregion,j)=uvwork(k,ntype_uv+1,1,iregion,j)
                  vwork(k,ntype_uv+1,2,iregion,j)=uvwork(k,ntype_uv+1,2,iregion,j)
   
                  uwork(k,ntype_uv+1,3,iregion,j)=uwork(k,ntype_uv+1,3,iregion,j)&
                                  /uwork(k,ntype_uv+1,1,iregion,j)
                  uwork(k,ntype_uv+1,4,iregion,j)=sqrt(uwork(k,ntype_uv+1,4,iregion,j)&
                                  /uwork(k,ntype_uv+1,1,iregion,j))
                  vwork(k,ntype_uv+1,3,iregion,j)=vwork(k,ntype_uv+1,3,iregion,j)&
                                  /vwork(k,ntype_uv+1,1,iregion,j)
                  vwork(k,ntype_uv+1,4,iregion,j)=sqrt(vwork(k,ntype_uv+1,4,iregion,j)&
                                  /vwork(k,ntype_uv+1,1,iregion,j))
               endif
   
            enddo    !!! enddo k height
         enddo       !!! enddo j, j=1 assimilated, j=2 rejected, j=3 monitored 
      enddo          !!! enddo iregion region 


      !--------------------
      !  write stas files
      open(21,file='ps_stas',form='unformatted')    
      do j=1,3
         do i=1,6
            write(21) ((pswork(1,ltype,i,iregion,j),ltype=1,ntype_ps+1),iregion=1,nregion)
         enddo
      enddo

      open(31,file='q_stas',form='unformatted')
      do j=1,3
         do i=1,6
            do k=1,np
               write(31) ((qwork(k,ltype,i,iregion,j),ltype=1,ntype_q+1),iregion=1,nregion)
            enddo
         enddo
      enddo

      open(41,file='t_stas',form='unformatted')
      do j=1,3
         do i=1,6
            do k=1,np
               write(41) ((twork(k,ltype,i,iregion,j),ltype=1,ntype_t+1),iregion=1,nregion)
            enddo
         enddo
      enddo

      write(6,900) (twork(k,1,1,1,1),k=1,np) 
      900 format(13f10.1)

      open(51,file='u_stas',form='unformatted')
      do j=1,3
         do i=1,6
            do k=1,np
               write(51) ((uwork(k,ltype,i,iregion,j),ltype=1,ntype_uv+1),iregion=1,nregion)
            enddo
         enddo
      enddo

      open(61,file='v_stas',form='unformatted')
      do j=1,3
         do i=1,6
            do k=1,np
               write(61) ((vwork(k,ltype,i,iregion,j),ltype=1,ntype_uv+1),iregion=1,nregion)
            enddo
         enddo
      enddo
     
      open(71,file='uv_stas',form='unformatted')
      do j=1,3
         do i=1,6
            do k=1,np
               write(71) ((uvwork(k,ltype,i,iregion,j),ltype=1,ntype_uv+1), &
                                   iregion=1,nregion)
            enddo
         enddo
      enddo


      close(21)
      close(31)
      close(41)
      close(51)
      close(61)
      close(71)

      write(6,*) '<-- output_data'
   end subroutine output_data

end module conmon_process_time_data
