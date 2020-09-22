!-----------------------------------------------------------------------------
!  maingrads_sfctime
!
!    This program reads the conventional data and converts it into a GrADS
!    data file.  The data is a profile type having multiple levels.
!
!-----------------------------------------------------------------------------
program maingrads_sfctime

   use generic_list
   use data
   use conmon_read_diag

   implicit none


   interface

      subroutine grads_sfctime(fileo,ifileo,nobs,nreal,&
                    nlev,plev,iscater,igrads,isubtype,subtype,list,run)

         use generic_list
         character(ifileo)       :: fileo
         integer                 :: ifileo,nobs,nreal,nlev 
         real(4),dimension(nlev) :: plev
         integer                 :: iscater,igrdas,isubtype
         character(3)            :: subtype
         type(list_node_t),pointer   :: list
         character(3)            :: run
         
      end subroutine grads_sfctime

   end interface


   type(list_node_t), pointer   :: list => null()
   type(list_node_t), pointer   :: next => null()
   type(data_ptr)               :: ptr

   real(4),dimension(11) :: ptime11 
   real(4),dimension(7) :: ptime7
   character(10) :: fileo,stype,timecard 
   character(3) :: intype
   character(3) :: subtype
   integer nreal,iscater,igrads,isubtype 
   integer nobs,lstype
   integer n_time7,n_time11,itype

   !--- namelist with defaults
   logical               :: netcdf              = .false.
   character(100)        :: input_file          = "conv_diag" 
   character(3)          :: run                 = "ges" 
   namelist /input/input_file,intype,stype,itype,nreal,iscater,igrads,timecard,isubtype,subtype,netcdf,run


   data n_time11 / 11 /
   data n_time7 / 7 /
   data ptime11 / -2.5,-2.0,-1.5,-1.0,-0.5,0.0,0.5,1.0,1.5,2.0,2.5 /
   data ptime7 / -3.0,-2.0,-1.0,0.0,1.0,2.0,3.0 /

   read(5,input)
   write(6,*)' User input below'
   write(6,input)

   lstype=len_trim(stype) 

   write(6,*)'netcdf       =', netcdf
   call set_netcdf_read( netcdf )
   call conmon_read_diag_file( input_file,intype,itype,nreal,nobs,isubtype,list )


   if( nobs > 0 ) then

      if( trim(timecard) == 'time11') then
         call grads_sfctime(stype,lstype,nobs,nreal,n_time11,&
                            ptime11,iscater,igrads,isubtype,subtype, list, run) 
      else if( trim(timecard) == 'time7') then 
         call grads_sfctime(stype,lstype,nobs,nreal,n_time7,&
                            ptime7,iscater,igrads,isubtype,subtype,list, run) 
      endif
   
   else
      print *, 'NOBS <= 0, NO OUTPUT GENERATED'
   end if

   call list_free( list )

   stop
end
