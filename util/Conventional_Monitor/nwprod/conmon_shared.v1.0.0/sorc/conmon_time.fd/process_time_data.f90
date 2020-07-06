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


!
!  Notes: 
!
!  Binary files are read and processed element by element.  This won't work for
!  NetCDF files.
!
!  The NetCDF processing will have to be by type
!  and I guess subtype in order to use the conmon_read_diag.F90 module.  Check
!  the read convinfo routine -- does that get me a list of all types/subtypes?
!
!  The conmon_read_diag.F90 module returns a linked list of elements so I'll
!  have to add a routine here to translate that.
!

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
   subroutine process_conv_diag(input_file,mregion,nregion,np, &
           ptop,pbot,ptopq,pbotq, &
           rlatmin,rlatmax,rlonmin,rlonmax,iotype_ps,iotype_q,&
           iotype_t,iotype_uv,varqc_ps,varqc_q,varqc_t,varqc_uv,&
           ntype_ps,ntype_q,ntype_t,ntype_uv,&
           iosubtype_ps,iosubtype_q,iosubtype_t,iosubtype_uv)

      character(100)           :: input_file
      integer                     mregion,nregion,np
      real(4),dimension(np)    :: ptop,pbot,ptopq,pbotq
      real,dimension(mregion)  :: rlatmin,rlatmax,rlonmin,rlonmax
      integer,dimension(100)   :: iotype_ps,iotype_q,iotype_t,iotype_uv
      real(4),dimension(100,2) :: varqc_ps,varqc_q,varqc_t,varqc_uv
      integer                     ntype_ps,ntype_q,ntype_t,ntype_uv
      integer,dimension(100)   :: iosubtype_ps,iosubtype_q,iosubtype_t,iosubtype_uv

      real(4),dimension(np,100,6,nregion,3)  :: twork,qwork,uwork,vwork,uvwork
      real(4),dimension(1,100,6,nregion,3)   :: pswork

      write(6,*) 'input_file = ', input_file

      if( netcdf ) then
         write(6,*) ' call nc read subroutine'
         call process_conv_nc( input_file, mregion,nregion,np,ptop,pbot,ptopq,pbotq,&
                 rlatmin,rlatmax,rlonmin,rlonmax,iotype_ps,iotype_q,&
                 iotype_t,iotype_uv,varqc_ps,varqc_q,varqc_t,varqc_uv,&
                 ntype_ps,ntype_q,ntype_t,ntype_uv,&
                 iosubtype_ps,iosubtype_q,iosubtype_t,iosubtype_uv, &
                 twork,qwork,uwork,vwork,uvwork,pswork)
      else
         write(6,*) ' call bin read subroutine'
         call process_conv_bin( input_file,mregion,nregion,np,ptop,pbot,ptopq,pbotq,&
                 rlatmin,rlatmax,rlonmin,rlonmax,iotype_ps,iotype_q,&
                 iotype_t,iotype_uv,varqc_ps,varqc_q,varqc_t,varqc_uv,&
                 ntype_ps,ntype_q,ntype_t,ntype_uv,&
                 iosubtype_ps,iosubtype_q,iosubtype_t,iosubtype_uv, &
                 twork,qwork,uwork,vwork,uvwork,pswork)

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
           twork,qwork,uwork,vwork,uvwork,pswork)

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

      print *, 'idate=',idate 
      print *,ptop(1),ptop(5)
      print *,pbot(1),pbot(5)

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

         else if(trim(dtype) == '  q') then
            call stascal(dtype,rdiag,nreal,ii,iotype_q,varqc_q,ntype_q,&
                         qwork,uwork,vwork,np,ptopq,pbotq,nregion,mregion,&
                         rlatmin,rlatmax,rlonmin,rlonmax,iosubtype_q)

         else if(trim(dtype) == '  t') then
            call stascal(dtype,rdiag,nreal,ii,iotype_t,varqc_t,ntype_t,&
                         twork,uwork,vwork,np,ptop,pbot,nregion,mregion,&
                         rlatmin,rlatmax,rlonmin,rlonmax,iosubtype_t)

         else if(trim(dtype) == ' uv') then
            call stascal(dtype,rdiag,nreal,ii,iotype_uv,varqc_uv,ntype_uv,&
                         uvwork,uwork,vwork,np,ptop,pbot,nregion,mregion,&
                         rlatmin,rlatmax,rlonmin,rlonmax,iosubtype_uv)
         endif
          
         deallocate(cdiag,rdiag)

      enddo   loopd               !  ending read data do loop
    
      close(lunin)

   end subroutine process_conv_bin



   !-----------------------------------------------------------
   !  subroutine process_conv_nc
   ! 
   !  This routine reads and processes NetCDF formatted 
   !  conventional diag files. 
   !-----------------------------------------------------------
   subroutine process_conv_nc(input_file,mregion,nregion,np,ptop,pbot,ptopq,pbotq,&
           rlatmin,rlatmax,rlonmin,rlonmax,iotype_ps,iotype_q,&
           iotype_t,iotype_uv,varqc_ps,varqc_q,varqc_t,varqc_uv,&
           ntype_ps,ntype_q,ntype_t,ntype_uv,&
           iosubtype_ps,iosubtype_q,iosubtype_t,iosubtype_uv, &
           twork,qwork,uwork,vwork,uvwork,pswork)

      use generic_list
      use data

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


      type(list_node_t), pointer   :: list => null()
      type(list_node_t), pointer   :: next => null()
      type(data_ptr)               :: ptr

      real(4),allocatable,dimension(:,:)     :: rdiag 
      character(8),allocatable,dimension(:)  :: cdiag 

      character(3)             :: dtype

      integer nchar,nreal,ii,mype,idate,iflag,itype
      integer lunin,lunot,nreal1,nreal2,ldtype,intype
      integer ilat,ilon,ipress,iqc,iuse,imuse,iwgt,ierr1
      integer ierr2,ierr3,ipsobs,iqobs,ioff02
      integer i,j,k,ltype,iregion,ntype_uv
      integer iobg,iobgu,iobgv

      data lunin / 11 /
      data lunot / 21 /


      twork=0.0;qwork=0.0;uwork=0.0;vwork=0.0;uvwork=0.0
      pswork=0.0

!      call conmon_read_diag_file( input_file,intype,stype,itype,nreal,nobs,isubtype,subtype,list )
!  need intype, stype, itype, nreal, subtype 
!  how is subtype use?

!      itype=1;ilat=3;ilon=4;ipress=6;iqc=9;iuse=11;imuse=12
!      iwgt=13;ierr1=14;ierr2=15;ierr3=16;iobg=18;iobgu=18;iobgv=21
!   
!      write(6,*) 'input_file = ', input_file
!      open(lunin,file=input_file,form='unformatted')  
!      rewind(lunin)
!
!      read(lunin) idate
!
!      print *, 'idate=',idate 
!      print *,ptop(1),ptop(5)
!      print *,pbot(1),pbot(5)
!
!      loopd: do  
!         read(lunin,IOSTAT=iflag) dtype,nchar,nreal,ii,mype,ioff02
!         if( iflag /= 0 ) exit loopd
!
!         allocate(cdiag(ii),rdiag(nreal,ii))
!         read(lunin,IOSTAT=iflag) cdiag,rdiag
!
!         if( iflag /= 0 ) exit loopd
!
!
!         if(trim(dtype) == ' ps') then
!            call stascal(dtype,rdiag,nreal,ii,iotype_ps,varqc_ps,ntype_ps,&
!                         pswork,uwork,vwork,1,ptop,pbot,nregion,mregion,&
!                         rlatmin,rlatmax,rlonmin,rlonmax,iosubtype_ps)
!
!         else if(trim(dtype) == '  q') then
!            call stascal(dtype,rdiag,nreal,ii,iotype_q,varqc_q,ntype_q,&
!                         qwork,uwork,vwork,np,ptopq,pbotq,nregion,mregion,&
!                         rlatmin,rlatmax,rlonmin,rlonmax,iosubtype_q)
!
!         else if(trim(dtype) == '  t') then
!            call stascal(dtype,rdiag,nreal,ii,iotype_t,varqc_t,ntype_t,&
!                         twork,uwork,vwork,np,ptop,pbot,nregion,mregion,&
!                         rlatmin,rlatmax,rlonmin,rlonmax,iosubtype_t)
!
!         else if(trim(dtype) == ' uv') then
!            call stascal(dtype,rdiag,nreal,ii,iotype_uv,varqc_uv,ntype_uv,&
!                         uvwork,uwork,vwork,np,ptop,pbot,nregion,mregion,&
!                         rlatmin,rlatmax,rlonmin,rlonmax,iosubtype_uv)
!         endif
!          
!         deallocate(cdiag,rdiag)
!
!      enddo   loopd               !  ending read data do loop
!    
!      close(lunin)

   end subroutine process_conv_nc



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
