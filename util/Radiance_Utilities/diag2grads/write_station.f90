module write_station

contains
  subroutine write_station_data(lunstn,stid,rlat,rlon,rtim,n_chan,nelem_lv,nelem_sf,nuchan,data_lv,data_sf)
    implicit none

    character(8):: stid
    integer:: lunstn,n_chan,nelem_lv,nelem_sf
    real(4),dimension(n_chan):: nuchan
    real(4):: rlat,rlon,rtim
    real(4),dimension(nelem_sf):: data_sf
    real(4),dimension(nelem_lv,n_chan):: data_lv
    integer:: nflag,i,k
    
    nflag=1
    write(lunstn) stid,rlat,rlon,rtim,n_chan+1,nflag
    write(lunstn) (data_sf(i),i=1,nelem_sf),&
         (nuchan(k),(data_lv(i,k),i=1,nelem_lv),k=1,n_chan)
    
    return
  end subroutine write_station_data
  

  subroutine write_station_ctl(lunctl,comment,filename,datetime,tint,nt,n_chan,nelem_lvl,nelem_sfc,data_name)
    use grads_hdr
    use read_diag
    implicit none
    
    integer :: lunctl
    character(len=GRADS_MAXLEN_COMMENT)  :: comment 
    character(len=GRADS_MAXLEN_FILENAME) :: filename
    integer :: datetime(5)
    integer :: tint, nt,i
    integer :: n_chan,nelem_lvl,nelem_sfc
    real(4):: nuchan(n_chan)
    type(diag_data_name_list) :: data_name
    character(len=GRADS_MAXLEN_FILENAME+4):: dsetname
    character(len=3),dimension(12):: cmonth
    character(len=15):: string
    integer :: imissing
    
    data cmonth / "Jan", "Feb", "Mar", "Apr", "May", "Jun", &
         "Jul", "Aug", "Sep", "Oct", "Nov", "Dec" /
    
    
    dsetname=trim(filename) // '.dat'
    if (dsetname(1:1) == '/') then
       write(lunctl,'("dset ",a)') trim(dsetname)
    else
       write(lunctl,'("dset ^",a)') trim(dsetname)
    endif
    write(lunctl,'("dtype station")')
    
    dsetname=trim(filename) // '.map'
    if (dsetname(1:1) == '/') then
       write(lunctl,'("stnmap ",a)') trim(dsetname)
    else
       write(lunctl,'("stnmap ^",a)') trim(dsetname)
    endif
    
    write(lunctl,'("options sequential big_endian template")')
    imissing = grads_missing
    write(lunctl,'("undef ",i6)') imissing
    write(lunctl,'("title ",a)') trim(comment)
    
    write(string,'(i2.2,":",i2.2,"Z",i2.2,a3,i4.4)') datetime(4),datetime(5),datetime(3),cmonth(datetime(2)),datetime(1)
    write(lunctl,'("tdef ",i3," linear ",a,1x,i2,"hr")') nt,string,tint
    write(lunctl,'("vars ",i3)') nelem_sfc+nelem_lvl
    do i=3,nelem_sfc+2
       write(lunctl,'(a,(" 0 99 "),a)') data_name%fix(i),data_name%fix(i)
    end do
    do i=1,nelem_lvl
       write(lunctl,'(a,1x,i6,(" 99 "),a)') data_name%chn(i),n_chan,data_name%chn(i)
    end do
    
    write(lunctl,'("endvars")')
    
  end subroutine write_station_ctl

end module write_station
