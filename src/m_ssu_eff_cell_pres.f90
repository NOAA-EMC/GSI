module m_ssu_eff_cell_pres

private
public ssu_eff_cell_pres

contains

   subroutine ssu_eff_cell_pres(yyyymmdd,satid,set_eff_cell,iprt)

   use kinds, only: r_kind,r_double,i_kind

   implicit none          


! Passed Variables

   real(r_kind),    dimension(3), intent(inout) :: set_eff_cell  ! effective cell pressure
   integer(i_kind), intent(in)                  :: yyyymmdd      ! Sounding date 
   integer(i_kind), intent(in)                  :: satid         ! NOAA Satellite Id 
   integer(i_kind), intent(in)                  :: iprt          ! print flag

! Local Variables

   integer(i_kind)            :: ios                 ! io flag 
   integer(i_kind)            :: i, l, isat          ! counter
   integer(i_kind)            :: satid1              !  
   integer(i_kind), parameter :: ssu_lun     = 8     !  
   integer(i_kind), parameter :: ssu_success = 0     ! Success flag - continue
   integer(i_kind), parameter :: ssu_error   = -1    ! Fatial Error will not continue
   integer(i_kind), parameter :: ssu_warning = -2    ! Warning message will continue
   integer(i_kind), parameter :: ssu_maxcha  = 3     ! Max number of ssu channels

   integer(i_kind), parameter :: mx_cell_tsat= 12
   integer(i_kind), parameter :: mx_cell_dsat= 7
   integer(i_kind), parameter :: mx_cell_ent = 15

   integer(i_kind), parameter, dimension(3):: ssu_sat_index=(/25,26,27/)
   integer(i_kind), parameter, dimension(3):: ssu_ins_index=(/1,2,3/)

   real(r_kind)                            :: rdate       ! date converted to real
   integer(i_kind), dimension(1)           :: lmm         ! minimum value location
   real(r_kind),    dimension(mx_cell_ent) :: diff_date   ! diff between sounding date

   character(LEN=2),dimension(mx_cell_tsat) :: sat_ref_id = (/'TN',  &
    'NA', 'NB', 'NC', 'ND', 'NE', 'NF', 'NG', 'NH', 'NI', 'NJ', 'NK'/)
   integer, dimension(mx_cell_tsat)        :: sat_ref_idx = (/ 1,    &
    6,     0,    7,    12,   8,    9,    10,   11,   13,   14,   15/)

   character(len=220) :: ssu_data_file

   TYPE ssu_sat_cell
      character(len=1)                     :: id
      integer(i_kind)                      :: num
      real(r_kind), dimension(mx_cell_ent) :: date
      real(r_kind), dimension(mx_cell_ent) :: eprs1
      real(r_kind), dimension(mx_cell_ent) :: eprs2
      real(r_kind), dimension(mx_cell_ent) :: eprs3
   END TYPE ssu_sat_cell

   TYPE (ssu_sat_cell), dimension(mx_cell_dsat) :: ssu_cell
   TYPE (ssu_sat_cell):: set_cell

   integer(i_kind) :: error_ssu = 0


! Open SSU effective pressure data

   ssu_data_file = 'ssu_effcell_prs' 
   write(6,*) 'ssu_eff_cell_pres: Opening ', trim(ssu_data_file)
   open(unit=ssu_lun,file=ssu_data_file,action='read',iostat=ios, status='old')
    
   if (ios /=0 ) then
      write(6,*) 'Error during opsning of ', trim(ssu_data_file)  
      error_ssu = ssu_error
      return
   endif

! Read in the effective cell pressure for the selected satellite

   satid1 = satid
   if (satid1 == 5) satid1 = 1

   if (iprt >= 3) &
       print *, 'mx_cell_dsat ',mx_cell_dsat,' satid ',satid1, &
                'mx_cell_ent ',mx_cell_ent
   do isat = 1, mx_cell_dsat
      read (ssu_lun,*) ssu_cell(isat)%id
      read (ssu_lun,*) ssu_cell(isat)%num
      if (iprt >= 3) then
         write (6, *) ' '
         write (6,100) isat, ssu_cell(isat)%id
         write (6,101) isat, ssu_cell(isat)%num
      endif
      do i = 1, mx_cell_ent
         read (ssu_lun,*) ssu_cell(isat)%date(i),ssu_cell(isat)%eprs1(i), &
                          ssu_cell(isat)%eprs2(i),ssu_cell(isat)%eprs3(i)
         if (iprt >= 3) &
            write (6,102) i, ssu_cell(isat)%date(i), ssu_cell(isat)%eprs1(i), &
                          ssu_cell(isat)%eprs2(i),ssu_cell(isat)%eprs3(i)
      end do
      if( ssu_cell(isat)%num == satid1) then
         set_cell = ssu_cell(isat)
      endif
   end do

100 format('read_eff_cell_pres: ',i4, ' ssu_cell%id ',a3)
101 format('read_eff_cell_pres: ',i4, ' ssu_cell%num',i3)
102 format('read_eff_cell_pres: ',i4, ' date',f10.0, ' eprs',3f8.2)

! Close file for effective cell press list

   close ( ssu_lun)


! Initialize errors

   error_ssu = ssu_success

! Initialize ssu pressure

   set_eff_cell = 0.0

! Determine valid channels 

   rdate     = REAL(yyyymmdd)
   diff_date = set_cell%date - rdate

!  set_cell%date = rdate

   lmm  = minloc( diff_date, diff_date >= 0.0 )
   l    = lmm(1)

!  print out info related to ssu effective cell pressure
  
   if (iprt >= 1) then
      print *, ' '
      print *, 'ssu_eff_cell_pres: ' 
      print *, 'satid     : ', satid1 
      print *, 'date      : ', yyyymmdd
      print *, 'rdate     : ', rdate
      print *, 'diff_data : ', diff_date
      print *, 'set_cell%date'
      write (6,105) set_cell%date
      print *, 'set_cell%eprs1'
      write (6,104) set_cell%eprs1
      print *, 'set_cell%eprs2'
      write (6,104) set_cell%eprs2
      print *, 'set_cell%eprs3'
      write (6,104) set_cell%eprs3
      print *, 'selected '
      write (6,105) set_cell%date(l)
   endif
104 format (6f10.2)
105 format (6f12.0)

   set_eff_cell(1) = set_cell%eprs1(l) 
   set_eff_cell(2) = set_cell%eprs2(l) 
   set_eff_cell(3) = set_cell%eprs3(l) 

   error_ssu = ssu_success

  end subroutine ssu_eff_cell_pres   

end module m_ssu_eff_cell_pres
