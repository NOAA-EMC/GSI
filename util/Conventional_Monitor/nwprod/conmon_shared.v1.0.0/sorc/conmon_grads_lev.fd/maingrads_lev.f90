!-----------------------------------------------------------------------------
!  maingrads_lev
!
!    This program reads the conventional data and converts it into a GrADS
!    ready data file.  
!
!    The data is from a vertical profile having multiple levels.  The
!    hint value is the vertical level thickness, for example, level 925 
!    includes the range of 925-hint to 925+hint.
!-----------------------------------------------------------------------------

   use generic_list
   use data
   use conmon_read_diag
 
   implicit none

   interface

      subroutine grads_lev(fileo,ifileo,nobs,nreal,nlev,plev,iscater,igrads, &
                           levcard,hint,isubtype,subtype,list,run)
         use generic_list

         integer ifileo
         character(ifileo)              :: fileo
         integer                        :: nobs,nreal,nlev,igrads,isubtype
         real(4),dimension(nlev)        :: plev
         character(10)                  :: levcard
         real*4                         :: hint
         character(3)                   :: subtype
         type(list_node_t), pointer     :: list
         character(3)                   :: run
      end subroutine grads_lev

   end interface

   integer itype
   real(4),dimension(3) :: plowlev 
   real(4),dimension(6) :: pacft 
   real(4),dimension(4) :: pupair
   real(4),dimension(10) :: palllev
   character(10) :: levcard,fileo,stype 
   character(3) :: intype
   character(3) :: subtype
   integer nreal,iscater,igrads 
   integer n_alllev,n_acft,n_lowlev,n_upair,nobs,lstype,intv,isubtype
   real hint

   type(list_node_t), pointer   :: list => null()
   type(list_node_t), pointer   :: next => null()
   type(data_ptr)               :: ptr

   !--- namelist with defaults
   logical               :: netcdf              = .false.
   character(100)        :: input_file          = "conv_diag"
   character(3)          :: run                 = "ges"
   namelist /input/input_file,intype,stype,itype,nreal,iscater,igrads,levcard,intv,subtype,isubtype,netcdf,run

   data pacft /700.,600.,500.,400.,300.,100./
   data palllev /950.,850.,700.,600.,500.,400.,300.,250.,200.,100./
   data plowlev /950.,850.,700./
   data pupair /300.,250.,200.,100./
   data n_alllev / 10 /
   data n_acft / 6 /
   data n_lowlev / 3 /
   data n_upair / 4 /



   write(6,*) '----> BEGIN maingrads_lev'
   write(6,*) '        levcard = ', levcard
   read(5,input)
   write(6,*)' User input:'
   write(6,input)

   lstype=len_trim(stype) 
   hint=real(intv)

   write(6,*)'netcdf       =', netcdf
   call set_netcdf_read( netcdf )

   call conmon_read_diag_file( input_file,intype,itype,nreal,nobs,isubtype,list )
   
   if( nobs > 0 ) then
      if(trim(levcard) == 'alllev' ) then
         call grads_lev(stype,lstype,nobs,nreal,n_alllev,palllev,iscater, &
                        igrads,levcard,hint,isubtype,subtype,list,run)
      else if (trim(levcard) == 'acft' ) then
         call grads_lev(stype,lstype,nobs,nreal,n_acft,pacft,iscater,igrads,&
                        levcard,hint,isubtype,subtype,list,run)
      else if(trim(levcard) == 'lowlev' ) then
         call grads_lev(stype,lstype,nobs,nreal,n_lowlev,plowlev,iscater,&
                        igrads,levcard,hint,isubtype,subtype,list,run)
      else if(trim(levcard) == 'upair' ) then
         call grads_lev(stype,lstype,nobs,nreal,n_upair,pupair,iscater,&
                        igrads,levcard,hint,isubtype,subtype,list,run)
      end if
   else
      print *, 'NOBS <= 0, NO OUTPUT GENERATED' 
   end if

   call list_free( list ) 
   write(6,*) '<---- END maingrads_lev'

   stop
end
