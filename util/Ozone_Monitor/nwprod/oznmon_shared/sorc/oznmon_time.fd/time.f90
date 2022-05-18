program main
   use oznmon_read_diag
   use valid
   use kinds, only: i_kind

   implicit none

   integer ntype,mregion,mls2_levs,mls3_levs
   parameter (ntype=4,mregion=25,mls2_levs=37,mls3_levs=55)

   character(10),dimension(ntype):: var_list
   character(10),dimension(ntype):: anl_vars
   character(10),dimension(ntype):: ges_vars
   character(20) satname,stringd,satsis
   character(10) dum,obstype,dplat
   character(40) string,grad_file,ctl_file
   character(500) diag_oz
   character(40) bad_pen_file, bad_cnt_file
   character(40),dimension(mregion):: region

   integer luname,lungrd,lunctl,lndiag,nregion
   integer lupen, lucnt, fiosp, fiosc
   integer iyy,imm,idd,ihh,idhh,incr,iflag,ier,iret
   integer n_levs,j,idsat,i,k,ii,nreg,nlevs,iobs,iread,nobs
   integer,dimension(mregion):: jsub
   real,allocatable,dimension(:):: prs_nlev

   real pen,pbound,cbound
   real weight,rlat,rlon,rmiss,obs,biascor,obsges,obsgesnbc,rterm
   real,dimension(2):: cor_omg
   real,dimension(mregion):: rlatmin,rlatmax,rlonmin,rlonmax

   real,allocatable,dimension(:,:):: cnt,error,use,penalty
   real,allocatable,dimension(:,:,:):: omg_cor
   logical validate, valid_penalty, valid_count
   character(60) penformat,cntformat

   integer( i_kind )                :: istatus


!  Variables for reading ozone data
   type(diag_header_fix_list )         :: header_fix
   type(diag_header_nlev_list),pointer :: header_nlev(:)
   type(diag_data_fix_list   ),pointer :: data_fix(:)
   type(diag_data_nlev_list  ),pointer :: data_nlev(:,:)
   type(diag_data_extra_list) ,pointer :: data_extra(:,:)

!  Namelist with defaults
   logical               :: new_hdr            = .false.
   character(10)         :: ptype              = "ges"
   logical               :: netcdf              = .false.
   namelist /input/ satname,iyy,imm,idd,ihh,idhh,incr,&
        nregion,region,rlonmin,rlonmax,rlatmin,rlatmax,validate,new_hdr,ptype,netcdf

   data luname,lungrd,lunctl,lndiag,lupen,lucnt / 5, 100, 51, 21, 52, 53 /
   data rmiss /-999./
   data stringd / '.%y4%m2%d2%h2' /
   data anl_vars / 'cnt', 'cpen', 'avgoma', 'sdvoma' /
   data ges_vars / 'cnt', 'cpen', 'avgomg', 'sdvomg' /


   penformat = "(A15,A7,I3,A8,I1,A10,ES13.7E2,A8,ES13.7E2)"
   cntformat = "(A15,A7,I3,A8,I1,A10,F7.2,A8,F7.2)"

!************************************************************************
!
! Initialize variables
   nobs=0
   region=' '
   rlatmin=0.; rlatmax=0.; rlonmin=0.; rlonmax=0.

! Read namelist input
   read(luname,input)
   write(6,input)
   write(6,*)' '

! Ensure number of requested regions does not exceed specified upper limit
   if (nregion>mregion) then
      write(6,*)'***ERROR*** too many regions specified'
      write(6,*)'   maximum allowed:  mregion=',mregion
      write(6,*)'    user requested:  nregion=',nregion
      call errexit(91)
   endif


! Create filenames for diagnostic input, GrADS output, and GrADS control files    
   write(stringd,100) iyy,imm,idd,ihh
100 format('.',i4.4,3i2.2)

   diag_oz   = trim(satname) // '.' // trim(ptype)
   grad_file = trim(satname) // '.' // trim(ptype) // trim(stringd) // '.ieee_d'
   ctl_file  = trim(satname) // '.' // trim(ptype) // '.ctl'

   bad_pen_file = 'bad_pen' // trim(stringd)
   bad_cnt_file = 'bad_cnt' // trim(stringd)
 
   write(6,*)'diag_oz =',diag_oz
   write(6,*)'grad_file=',grad_file
   write(6,*)'ctl_file =',ctl_file
   write(6,*)'bad_pen_file =', bad_pen_file
   write(6,*)'bad_cnt_file =', bad_cnt_file
   write(6,*)'netcdf       =', netcdf

   call set_netcdf_read( netcdf )

   call open_ozndiag( diag_oz, lndiag, istatus )
   if ( istatus /= 0 ) then
      write(6,*)'***PROBLEM opening diagnostic file.  diag_oz=',diag_oz
      write(6,*)'***        exiting with error code 93'
      close(lndiag)
      call errexit(93)
   end if

!  File exists.  Read header

   call read_ozndiag_header( lndiag, header_fix, header_nlev, new_hdr, istatus )
  

! Extract observation type, satellite id, and number of levels
   obstype = header_fix%obstype     
   satsis  = header_fix%isis
   dplat   = header_fix%id
   n_levs  = header_fix%nlevs

   !----------------------------------------------------------------
   !  mls = microwave limb sounder 
   !
   !  This assignment potentially overrides the nlevs values from
   !  the file header.  We don't currently have any mls data 
   !  available so it doesn't do any harm.  I'll leave this note 
   !  here as a reminder if/when we get mls instrument data this 
   !  may need to change if the header level number doesn't agree 
   !  with these settings. 
   !
   if(index(obstype,'mls2')/=0 ) then
      n_levs = mls2_levs
   end if
   if(index(obstype,'mls3')/=0 ) then
      n_levs = mls3_levs
   end if


   string = trim(obstype)//'_'//trim(dplat)
   if ( trim(string) /= trim(satname) ) then
      write(6,*)'***ERROR*** inconsistent instrument types'
      write(6,*)'  satname,string  =',satname,' ',string
      call errexit(92)
   endif

!------------------------------------------------------
!   Allocate arrays to hold observational information
!
   allocate ( prs_nlev(n_levs))
   allocate (omg_cor(n_levs,mregion,2), &
       cnt(n_levs,mregion), & 
       penalty(n_levs,mregion), &
       error(n_levs,mregion), use(n_levs,mregion))

! Zero accumulator arrays
   do ii=1,2
      do k=1,mregion
         do j=1,n_levs
            if (ii==1) then
               cnt(j,k) = 0.0
               penalty(j,k) = 0.0
            endif
            omg_cor(j,k,ii) = 0.0
         end do
      end do
   end do

! Extract ozinfo relative index
   do j=1,n_levs
      prs_nlev(j)       = real( header_nlev(j)%pob, 4)
   end do
   do k=1,mregion
      do j=1,n_levs
         error(j,k)     = real( header_nlev(j)%err, 4)
         use(j,k)       = real( header_nlev(j)%iouse, 4 )
      end do
   end do
        

! ----------------------------------------------------
! Set the var_list list to use either the ges or anl 
! version per the ptype value

   if( trim(ptype) == 'ges' ) then
      var_list=ges_vars
   else
      var_list=anl_vars
   end if

!-----------------------------------------------------
!  create GrADS contol file
!
   call create_ctl_oz(ntype,ptype, var_list,n_levs,iyy,imm,idd,ihh,idhh,&
       incr,ctl_file,lunctl,rmiss,satname,obstype,dplat,nregion,&
       region,rlonmin,rlonmax,rlatmin,rlatmax,prs_nlev,use(1,1),error(1,1))


! Loop to read entries in diagnostic file
   iflag = 0

   loopd:  do while (iflag == 0)

      !---------------------------------------------------------------------
      ! Read a record.  If read flag, iflag does not equal zero, exit loopd
      !
      call read_ozndiag_data( lndiag, header_fix, data_fix, data_nlev, data_extra, iread, iflag )
      if( iflag /= 0 ) exit loopd

      nobs=nobs+iread
      write(6,*) ' iread, nobs now = ', iread, nobs

!     Extract obervation location and mpi weight.  Convert (0-360) lon to (-180,180)

      do iobs=1,iread
         rlat   = data_fix(iobs)%lat
         rlon   = data_fix(iobs)%lon
         if (rlon>180.) rlon = rlon - 360.

!        Determine subdomain based on observation location
         ii=0; jsub=0
         do k=1,nregion
            if ( (rlonmin(k)<=rlon .and. rlon<rlonmax(k)) .and. &
                 (rlatmin(k)<=rlat .and. rlat<rlatmax(k)) ) then
               ii=ii+1
               jsub(ii)=k
            endif
         end do
         nreg=ii

         if(index(obstype,'mls')==0 ) then       ! meaning if this is NOT an mls* source

            do j = 1, n_levs

               if (data_nlev(1,iobs)%varinv > 1.e-6) then

                  !------------------------------------------------------------------
                  ! Accumulate sums in appropriate regions.
                  ! This is done for all obs, assimilated or not.  
                  ! Earlier versions of this code included a commented out line
                  ! which contained a check for assimlated status:
                  !
                  !    if (data_nlev(j,iobs)%varinv > 1.e-6) then
                  !
                  ! around the code (below) encompassing everything with this 
                  ! do loop (above).  Adding this check back into the code 
                  ! results in all non-assimilated sources producing
                  ! zeroed out plots, which isn't desireable.  I've preserved the
                  ! check in this comment just in case it's needed some day.
                  !

                  pen         =  data_nlev(j,iobs)%varinv*(data_nlev(j,iobs)%ozone_inv)**2
                  cor_omg(1)  =  data_nlev(j,iobs)%ozone_inv
                  cor_omg(2)  =  (cor_omg(1))**2

                  do i=1,nreg
                     k=jsub(i)
                     cnt(j,k) = cnt(j,k) +1.0 
                     penalty(j,k) = penalty(j,k) + pen

                     do ii=1,2
                        omg_cor(j,k,ii)  = omg_cor(j,k,ii)  + cor_omg(ii)
                     end do
   
                  end do

               end if
            enddo 

         else           !  mls data sources

            !------------------------------------------------------------------------
            ! If observation was assimilated, accumulate sums in appropriate regions
            !
            ! Note that this block still has a check for assimilated data.  This
            ! is only used by mls data sources.  Currently there are no mls data
            ! sources.  If they do get added at some point be advised that this
            ! check will produce zeroed output files if the sources are not
            ! assimilated. 
            !

            if (data_nlev(1,iobs)%varinv > 1.e-6) then

               pen         =  data_nlev(1,iobs)%varinv*(data_nlev(1,iobs)%ozone_inv)**2
               cor_omg(1)  =  data_nlev(1,iobs)%ozone_inv
               cor_omg(2)  =  (cor_omg(1))**2
               j=mod(iobs,n_levs)
               if(j==0) j=n_levs

               do i=1,nreg
                  k=jsub(i)
                  cnt(j,k) = cnt(j,k) +1.0 
                  penalty(j,k) = penalty(j,k) + pen

                  do ii=1,2
                     omg_cor(j,k,ii)  = omg_cor(j,k,ii)  + cor_omg(ii)
                  end do
               end do
            endif

         endif

      enddo   ! END do iobs=1,iread

   enddo loopd !  End of loop over diagnostic file


   call close_ozndiag( diag_oz, lndiag )

   write(6,*)' '
   print*, 'read in ', nobs, ' observations in total'
   write(6,*)' '

!  Compute average and standard deviation
   do k=1,nregion
      do j=1,n_levs
         call avgsdv(cnt(j,k),omg_cor(j,k,1), omg_cor(j,k,2), rmiss)

         if (cnt(j,k)>0) then
            penalty(j,k)=penalty(j,k)/cnt(j,k) ! convert penalty to cpen
         else
            cnt(j,k)=rmiss
            penalty(j,k)=rmiss
         endif
      end do
   end do

!  Do validation
   if( validate .eqv. .TRUE. ) then
      call load_base( satname, ier )

      write(6,*) 'ier from load_base = ', ier
      open(lupen,file=bad_pen_file,form='formatted')
      open(lucnt,file=bad_cnt_file,form='formatted')

   endif

!  Validate penalty values

   k=1
   do j=1,n_levs
      if ( use(j,k) > 0.0 ) then

         if( validate .eqv. .TRUE. .AND. ier >= 0 ) then

            pbound = 0.00
            call validate_penalty( j, k, penalty(j,k), valid_penalty, pbound, iret )

            if( (iret == 0) .AND. (valid_penalty .eqv. .FALSE.) ) then
               write(6,*) 'BAD PEN j,k,penalty, valid_penalty,bound = ', &
                         j,k,penalty(j,k), valid_penalty, pbound
               write(lupen,penformat) satname, ' level= ',j, ' region= ', k, &
                         ' penalty= ', penalty(j,k), ' bound= ', pbound
            endif

            cbound = 0.00
            call validate_count( j, k, cnt(j,k), valid_count, cbound, iret )

            if( (iret == 0) .AND. (valid_count .eqv. .FALSE.) ) then
               write(6,*) 'BAD CNT j,k, count = ', j,k,cnt(j,k), valid_count
               write(lucnt,cntformat) satname, ' level= ',j, ' region= ', k, &
                        ' count= ', cnt(j,k), ' bound= ', cbound
            endif

         endif
      endif
   end do

   if( validate .eqv. .TRUE. ) then
      close( lupen )
      close( lucnt )
   endif

   !------------------------------------  
   ! Write output to GrADS ready file
   !
  
   open(lungrd,file=grad_file,form='unformatted')
   write(lungrd) ((cnt(j,k),j=1,n_levs),k=1,nregion)
   write(lungrd) ((penalty(j,k),j=1,n_levs),k=1,nregion)
   do ii=1,2
      write(lungrd) ((omg_cor (j,k,ii),j=1,n_levs),k=1,nregion)
   end do

   write(6,*)''
   write(6,*)'finished writing output to lungrd=',lungrd,', file=',trim(grad_file)
   close(lungrd)


   !------------------------
   ! Deallocate arrays
   !
   deallocate(prs_nlev,omg_cor,cnt,penalty,error,use)
  

  stop
end program main
