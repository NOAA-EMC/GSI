! nc_diag_write - NetCDF Layer Diag Writing Module
! Copyright 2015 Albert Huang - SSAI/NASA for NASA GSFC GMAO (610.1).
! 
! Licensed under the Apache License, Version 2.0 (the "License");
! you may not use this file except in compliance with the License.
! You may obtain a copy of the License at
! 
!   http://www.apache.org/licenses/LICENSE-2.0
! 
! Unless required by applicable law or agreed to in writing, software
! distributed under the License is distributed on an "AS IS" BASIS,
! WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
! implied. See the License for the specific language governing
! permissions and limitations under the License.
! 
! Main module - nc_diag_write_mod
!

module nc_diag_write_mod
    ! Library that provides a high level interface for storing channel-
    ! based and observation-based data.
    ! 
    ! This library allows developers to easily store channel-based data
    ! (e.g. chaninfo) and observation-based data (metadata and data2d)
    ! to a NetCDF file via an easy to use API.
    ! 
    ! Internally, the process for storing this data looks like this:
    !   -> When the developer calls nc_diag_init, the NetCDF file is
    !      opened internally. The corresponding NCID is stored, and
    !      any memory allocation needed is done at this step.
    !      => If the file was opened in append mode, nc_diag_write will
    !         attempt to load any existing variable definitions for all
    !         types of variables - chaninfo, metadata, and data2d.
    !         Appropriate variable counters and data for each variable
    !         type will be set during init, and data writing will start
    !         at the end of the variable.
    !      
    !   -> Headers are essentially NetCDF global attributes, or
    !      attributes that describe a file. These can be added at any
    !      time during the writing session.
    !      
    !   -> varattr, or variable attributes, describe an associated
    !      variable. (This is a NetCDF4 variable attribute!) These can
    !      only be added after variable definitions have been locked.
    !      
    !   -> chaninfo variables:
    !      => nc_diag_chaninfo_dim_set must be called first to set
    !         the nchans dimension. If it isn't called, doing any
    !         chaninfo operation will result in an error.
    !      => chaninfo variables are 1D, with nchans number of elements.
    !      
    !   -> metadata and data2d variables:
    !      => metadata and data2d variables do not require any initial
    !         dimension setting - nc_diag_write will keep track of your
    !         number of observations for you!
    !      => metadata variables are 1D, with nobs number of elements.
    !         nobs can increase infinitely to fit the number of
    !         observations recorded.
    !      => data2d variables are 2D, with dimensions of nobs by
    !         another fixed dimension.
    !      
    !   -> Definition locking is sometimes necessary for certain
    !      operations, such as defining variable attributes. They are
    !      necessary due to needing information from NetCDF after
    !      variables are defined, or needing to assert that certain
    !      variable properties are constant. Locking uses the following
    !      steps:
    !      => nc_diag_*_write_def is called to send the variable
    !         definitions to NetCDF. This include defining any
    !         dimensions necessary, as well as defining the variables
    !         stored as well.
    !      => Once each nc_diag_*_write_def completes, their
    !         corresponding def_lock state will be set to TRUE, locking
    !         the definitions in place.
    !      => Attempts to make repeated calls will result in an error,
    !         due to the def_lock state being set to TRUE.
    !      
    !   -> Data calls will store the input data into memory. The
    !      implementation and design of the variable storage is
    !      dependent on the variable type being stored. chaninfo
    !      variables have a certain storage format, and metadata/data2d
    !      variables have another storage format. Note that metadata
    !      and data2d code have a few similarities in data storage
    !      since the variables themselves share common features, like
    !      the nobs dimension.
    !      
    !   -> Sometimes, there is a significant amount of data that needs
    !      to be processed and stored. Since nc_diag_write stores all
    !      of the data into memory (RAM) before it is written out,
    !      there may not be enough memory to store the entirety of the
    !      data. To alleviate that, nc_diag_flush_buffer can be called
    !      to flush the data from the memory and write them to disk.
    !      In reality, this doesn't actually free any memory... but the
    !      memory savings gained is still there. Calling the flushing
    !      subroutine performs the following steps:
    !      => It first checks to make sure that definitions are locked.
    !         The NetCDF variable IDs are needed in order to actually
    !         write (or "put") any data into the file.
    !      => It also checks to see if the data has already been locked.
    !         No more data can be written if the data has been locked.
    !      => It then calls all of the nc_diag_*_write_data subroutines
    !         with a special flag to indicate data flushing. When the
    !         data flushing flag is set, each of the variable
    !         subroutines will take measures to operate as a buffer
    !         flush, and not as a finalized data write.
    !      => When flushing within the variable subroutine, the
    !         subroutine first writes out any data using the variable-
    !         specific, memory-stored data.
    !      => It then resets any internal data counters that it may use
    !         to store and keep track of the data.
    !      => As mentioned before, it does not actually free any memory
    !         since deallocating and subsequently reallocating from
    !         scratch will take a long time, and is inefficient. With
    !         a counter reset, each variable type's internal data
    !         storage will start at the beginning of the data array,
    !         effectively avoiding any need to add any more memory, and
    !         thus achieving the goal of not using any more memory.
    !      => Finally, since the writing is in buffer flushing mode,
    !         the data_lock flag for each variable type is NOT set.
    !         This is so that more data can be written, either with
    !         the flushing method or with the regular write.
    !      
    !   -> Once data is done being queued ("stored"), nc_diag_write can
    !      be called. The variables will have their data re-read from
    !      memory and actually written to the file. This is also very
    !      much variable type independent, since every variable has its
    !      own way of storing variable data. Again, metadata and data2d
    !      have similar code, with the only difference being the
    !      dimensionality. Note that this is where NetCDF calls are
    !      made to define and "put" data. Once done, if we are NOT in
    !      append mode, we call nf90_enddef to end define mode.
    !      
    !   -> Once all the data has been queued and/or written out, it is 
    !      safe to call nc_diag_finish. We call this from nc_diag_write.
    !      => This will first write definitions and data, if applicable.
    !         The calls will have a special flag set to ensure that no
    !         errors are triggered for already having a lock set, since
    !         this subroutine will be closing the file anyways.
    !      => Once all of the data has been sent to NetCDF, this will
    !         tell NetCDF to close the file being written. Note that
    !         NetCDF also keeps a memory cache of the data being stored
    !         as well, so actual I/O writing may not be completely done
    !         until here. After the writing and closing on the NetCDF
    !         side completes, everything will be completely deallocated,
    !          and everything will be reset.
    !      
    !   -> Upon reset, nc_diag_write is again ready to write a new file
    !      via nc_diag_create!
    ! 
    ! Note that only ONE file is written as a time. This is due to the
    ! nature of the library focusing and storing data for a single
    ! file. Attempting to create another file without closing the
    ! previous one will result in an error.
    
    ! Load state variables! We need to know:
    !   init_done           - ...whether a file is currently loaded or
    !                         not.
    !   append_only         - ...whether we are in append mode or not.
    !   ncid                - ...the current NCID of our file.
    !   enable_trim         - ...whether we need to automatically trim
    !                         our strings for chaninfo string storage or
    !                         not.
    !   diag_chaninfo_store - ...chaninfo variable information.
    !                         Specifically, whether it's allocated or
    !                         not, and if it's allocated, whether the
    !                         definitions are locked or not. (def_lock)
    !   diag_metadata_store - ...metadata variable information.
    !                         Specifically, whether it's allocated or
    !                         not, and if it's allocated, whether the
    !                         definitions are locked or not. (def_lock)
    !   diag_data2d_store   - ...data2d variable information.
    !                         Specifically, whether it's allocated or
    !                         not, and if it's allocated, whether the
    !                         definitions are locked or not. (def_lock)
    use ncdw_state, only: init_done, append_only, ncid, &
        enable_trim, cur_nc_file, &
        diag_chaninfo_store, diag_metadata_store, diag_data2d_store, &
        diag_varattr_store
    
    ! Load needed NetCDF functions and constants
    use netcdf, only: nf90_inq_libvers, nf90_open, nf90_create, &
        nf90_enddef, nf90_close, nf90_sync, &
        NF90_WRITE, NF90_NETCDF4, NF90_CLOBBER
    
    !------------------------------------------------------------------
    ! API imports to expose API from this module
    ! (Plus general imports for this module as well!)
    !------------------------------------------------------------------
    
    ! Load necessary command line message subroutines and state
    ! variables
    use ncdw_climsg, only: &
#ifdef ENABLE_ACTION_MSGS
        nclayer_enable_action, nclayer_actionm, &
#endif
        nclayer_error, nclayer_warning, nclayer_info, nclayer_check, &
        nc_set_info_display, nc_set_action_display
    
    ! Load nc_diag_write specific types
    use ncdw_types, only: NLAYER_BYTE, NLAYER_SHORT, NLAYER_LONG, &
        NLAYER_FLOAT, NLAYER_DOUBLE, NLAYER_STRING
    
    ! Load header writing API
    use ncdw_lheader, only: nc_diag_header
    
    ! Load chaninfo writing API + auxillary functions for our use
    use ncdw_chaninfo, only: nc_diag_chaninfo_dim_set, &
        nc_diag_chaninfo, &
        nc_diag_chaninfo_load_def, nc_diag_chaninfo_write_def, &
        nc_diag_chaninfo_write_data, &
        nc_diag_chaninfo_set_strict, &
        nc_diag_chaninfo_allocmulti, nc_diag_chaninfo_prealloc_vars, &
        nc_diag_chaninfo_prealloc_vars_storage
    
    ! Load metadata writing API + auxillary functions for our use
    use ncdw_metadata, only: nc_diag_metadata, &
        nc_diag_metadata_load_def, nc_diag_metadata_write_def, &
        nc_diag_metadata_write_data, &
        nc_diag_metadata_set_strict, &
        nc_diag_metadata_allocmulti, &
        nc_diag_metadata_prealloc_vars, &
        nc_diag_metadata_prealloc_vars_storage, &
        nc_diag_metadata_prealloc_vars_storage_all
    
    ! Load data2d writing API + auxillary functions for our use
    use ncdw_data2d, only: nc_diag_data2d, &
        nc_diag_data2d_load_def, nc_diag_data2d_write_def, &
        nc_diag_data2d_write_data, &
        nc_diag_data2d_set_strict, &
        nc_diag_data2d_allocmulti, &
        nc_diag_data2d_prealloc_vars, &
        nc_diag_data2d_prealloc_vars_storage, &
        nc_diag_data2d_prealloc_vars_storage_all
    
    ! Load varattr (variable attribute) writing API
    use ncdw_varattr, only: nc_diag_varattr
    
    implicit none
    
    contains
        ! Creates or appends to a new NetCDF file for data writing.
        ! 
        ! Given the target NetCDF file name, attempt to create or open
        ! the file and set everything up for writing data to the file.
        ! This includes any internal memory allocation required for
        ! buffering any data sent to this file.
        ! 
        ! If the file is opened in non-append mode (default), this will
        ! attempt to create a new file and start data writing from
        ! scratch. If the file already exists, it will be OVERWRITTEN
        ! without any prompt.
        ! 
        ! If the file is opened in append mode, this will attempt to
        ! open the file specified, read the file's dimension and
        ! variable storage information, and set things up so that
        ! data writing starts at the end of the file's existing data.
        ! Note that append mode only works for nc_diag_write NetCDF
        ! files. Attempting to open a non-nc_diag_write file could
        ! result in errors!
        ! 
        ! In order for the file to be written to successfully,
        ! nc_diag_finish MUST be called for all of the data to be
        ! flushed, and the corresponding memory to be freed.
        ! 
        ! nc_diag_write may only operate on one file at a time. This is
        ! due to the nature of nc_diag_write focusing on a single file.
        ! 
        ! If a NetCDF file is already open, this will raise an error
        ! and the program will terminate.
        ! 
        ! Args:
        !     filename (character(len=*)): NetCDF file name to create or
        !         append to.
        !     append (logical, optional): whether to open the NetCDF
        !         file in append mode or not. By default, if this is
        !         not specified, the file will be opened regularly (not
        !         in append mode).
        ! 
        ! Raises:
        !     If a file is already open, an error occurs and the program
        !     will exit.
        !     
        !     If the file specified does not exist, or there are issues
        !     with NetCDF creating/opening/using the file, an error
        !     will occur with the corresponding NetCDF error.
        !     
        !     Issues with storage allocation are bugs, and will also
        !     result in an error with an indication that a bug has
        !     occurred.
        ! 
        subroutine nc_diag_init(filename, append)
            character(len=*),intent(in)    :: filename
            logical, intent(in), optional  :: append
            
            ! Buffer size variable for NetCDF optimization settings
            ! (Not sure if this helps much...)
            integer                        :: bsize = 16777216;
            
#ifdef ENABLE_ACTION_MSGS
            character(len=1000)                   :: action_str
            
            if (nclayer_enable_action) then
                if (present(append)) then
                    write(action_str, "(A, L, A)") "nc_diag_init(filename = " // trim(filename) // &
                        ", append = ", append, ")"
                else
                    write(action_str, "(A)") "nc_diag_init(filename = " // trim(filename) // &
                        ", append = (not specified))"
                end if
                call nclayer_actionm(trim(action_str))
            end if
#endif
            
            ! Inform user about NetCDF version
            call nclayer_info('Initializing netcdf layer library, version ' // trim(nf90_inq_libvers()) // '...')
            
            ! Make sure we haven't initialized yet. If we have, it
            ! means that another file is open that hasn't been closed
            ! yet!
            if (.NOT. init_done) then
                ! Special append mode - that means that we need to
                ! assume that all definitions are set and locked.
                if (present(append) .AND. (append .eqv. .TRUE.)) then
                    ! Open the file in append mode!
                    call nclayer_check( nf90_open(filename, NF90_WRITE, ncid, &
                        bsize, cache_nelems = 16777216) ) ! Optimization settings
                    
                    ! Set the append flag
                    append_only = .TRUE.
                else
                    ! Create the file from scratch!
                    
                    ! nf90_create creates the NetCDF file, and initializes
                    ! everything needed to write a NetCDF file.
                    ! 
                    ! NF90_CLOBBER forces overwriting the file, even if it already
                    ! exists.
                    ! 
                    ! ncid is a special ID that the NetCDF library uses to keep
                    ! track of what file you're working on. We're returning that
                    ! here.
                    call nclayer_check( nf90_create(filename, OR(NF90_NETCDF4, NF90_CLOBBER), ncid, &
                        0, bsize, cache_nelems = 16777216) ) ! Optimization settings
                end if
                
                ! Allocation sanity checks...
                ! These storage variables should NOT be allocated.
                ! If they are, it indicate that we have a serious problem.
                if (allocated(diag_chaninfo_store)) then
                    call nclayer_error("BUG! diag_chaninfo_store is allocated, but init_done is not set!")
                end if
                
                if (allocated(diag_metadata_store)) then
                    call nclayer_error("BUG! diag_metadata_store is allocated, but init_done is not set!")
                end if
                
                if (allocated(diag_data2d_store)) then
                    call nclayer_error("BUG! diag_data2d_store is allocated, but init_done is not set!")
                end if
                
                if (allocated(diag_varattr_store)) then
                    call nclayer_error("BUG! diag_data2d_store is allocated, but init_done is not set!")
                end if
                
                ! All good, allocate the storage variables!
                allocate(diag_chaninfo_store)
                allocate(diag_metadata_store)
                allocate(diag_data2d_store)
                allocate(diag_varattr_store)
                
                ! Set the current file being written to...
                cur_nc_file = filename
                
                ! Set the flag state to indicate that a file is open,
                ! and that initialization is done.
                init_done = .TRUE.
                
                ! "Lock and load" the definitions... or simply ask
                ! chaninfo/metadata/data2d to read the NetCDF files,
                ! build a cache, and set up anything necessary to be
                ! able to resume writing from before.
                if (present(append) .AND. (append .eqv. .TRUE.)) then
                    call nclayer_info("Loading chaninfo variables/dimensions from file:")
                    call nc_diag_chaninfo_load_def
                    
                    call nclayer_info("Loading metadata variables/dimensions from file:")
                    call nc_diag_metadata_load_def
                    
                    call nclayer_info("Loading data2d variables/dimensions from file:")
                    call nc_diag_data2d_load_def
                end if
            else
                ! Opening a new file while another file is still open is
                ! bad... let's yell at the user/developer!
                call nclayer_error("Attempted to initialize without closing previous nc_diag file!" &
                    // char(10) &
                    // "             (Previous file: " // trim(cur_nc_file) &
                    // char(10) &
                    // "              Attempted to open file: " // trim(filename) // ")")
            end if
        end subroutine nc_diag_init
        
        ! Lock and commit the variable definitions for the current
        ! NetCDF file.
        ! 
        ! Attempt to commit the currently stored variable definitions
        ! to the NetCDF file via NetCDF API calls. Once done, this will
        ! set the flag for locking the variable definitions, preventing
        ! any additional variables from being created or changed.
        ! 
        ! Locking the definitions here will enable functions that
        ! require variable definition locking. This include
        ! nc_diag_varattr and nc_diag_flush_buffer, both of which
        ! require the variable definitions to be committed and locked.
        ! 
        ! Definitions may not be locked more than once. In addition,
        ! creating new variables after definitions are locked will
        ! result in errors.
        ! 
        ! Args:
        !     None
        ! 
        ! Raises:
        !     The following errors will trigger indirectly from other
        !     subroutines called here:
        !     
        !     If definitions have already been locked, this will result
        !     in an error.
        !     
        !     If there is no file open, this will result in an error.
        !     
        !     Other errors may result from invalid data storage, NetCDF
        !     errors, or even a bug. See the called subroutines'
        !     documentation for details.
        ! 
        subroutine nc_diag_lock_def
#ifdef ENABLE_ACTION_MSGS
            if (nclayer_enable_action) then
                call nclayer_actionm("nc_diag_lock_def()")
            end if
#endif
            call nclayer_info("Locking all variable definitions!")
            
            ! Call all of the variable write_def
            call nclayer_info("Defining chaninfo:")
            call nc_diag_chaninfo_write_def
            
            call nclayer_info("Defining metadata:")
            call nc_diag_metadata_write_def
            
            call nclayer_info("Defining data2d:")
            call nc_diag_data2d_write_def
            
            call nclayer_info("All variable definitions locked!")
        end subroutine nc_diag_lock_def
        
        ! Write all of the variables to the NetCDF file, including the
        ! variable definitions and data, and close the file.
        ! 
        ! Attempt to write the currently stored variable definitions
        ! and data to the NetCDF file via NetCDF API calls.
        ! 
        ! Once done, this will lock both the definitions and the data,
        ! preventing any new variables or new data from being written
        ! after this call completes.
        ! 
        ! Once data has been written and locked, the file itself will be
        ! closed. NetCDF may internally cache/buffer variable data in
        ! memory, so actual writing may occur at this time to let NetCDF
        ! actually commit the data to disk.
        ! 
        ! Finally, nc_diag_write state cleanup and memory deallocation
        ! will occur via a call to nc_diag_finish.
        ! 
        ! Writing may not occur more than once. In addition, writing any
        ! new variables or adding any new data will result in an error.
        ! (Not that you can write any more data after this, since the
        ! file is closed and everything is reset...)
        ! 
        ! Args:
        !     None
        ! 
        ! Raises:
        !     The following errors will trigger indirectly from other
        !     subroutines called here:
        !     
        !     If the variable definitions have already been locked, this
        !     will NOT result in an error. This is due to the fact that
        !     we could've locked definitions earlier, and that we
        !     can assume that with locked definitions, we are able to
        !     write data.
        !     
        !     Data writing is the critical part. If the variable data
        !     writing has already been locked, this will result in an
        !     error.
        !     
        !     If there is no file open (or the file is already closed),
        !     this will result in an error.
        !     
        !     Other errors may result from invalid data storage, NetCDF
        !     errors, or even a bug. See the called subroutines'
        !     documentation for details.
        ! 
        subroutine nc_diag_write
#ifdef ENABLE_ACTION_MSGS
            if (nclayer_enable_action) then
                call nclayer_actionm("nc_diag_write()")
            end if
#endif
            
            ! Call all variable write_def, with an extra option to make
            ! sure that no errors occur during write, even when locked!
            ! (We could have previously locked, but here we're doing it
            ! on purpose!)
            call nclayer_info("Defining chaninfo:")
            call nc_diag_chaninfo_write_def(.TRUE.)
            
            call nclayer_info("Defining metadata:")
            call nc_diag_metadata_write_def(.TRUE.)
            
            call nclayer_info("Defining data2d:")
            call nc_diag_data2d_write_def(.TRUE.)
            
            ! Lock definition writing!
            if ((.NOT. append_only) .AND. ((.NOT. diag_chaninfo_store%def_lock) .OR. &
                (.NOT. diag_metadata_store%def_lock) .OR. &
                (.NOT. diag_data2d_store%def_lock))) &
                call nclayer_check(nf90_enddef(ncid))
            
            ! Call all variable write_data
            call nclayer_info("Writing chaninfo:")
            call nc_diag_chaninfo_write_data
            
            call nclayer_info("Writing metadata:")
            call nc_diag_metadata_write_data
            
            call nclayer_info("Writing data2d:")
            call nc_diag_data2d_write_data
            
            ! Call nf90_close to save everything to disk!
            call nclayer_info("All done queuing in data, letting NetCDF take over!")
            call nclayer_check(nf90_close(ncid))
            
            call nclayer_info("All done!")
            
            ! Call our cleanup subroutine
            call nc_diag_finish
        end subroutine nc_diag_write
        
        ! Reset nc_diag_write state, and deallocate all of the variable
        ! storage in preparation for another new NetCDF file write.
        ! 
        ! Attempt to reset nc_diag_write state and deallocate all of 
        ! the variable storage. This frees up memory, and allows for
        ! nc_diag_init to work again for a new file.
        ! 
        ! This can only be called once per open. (You can't call this
        ! without a nc_diag_init happening before it!) Calling this
        ! without any file opened (or data stored) will result in an
        ! error.
        ! 
        ! This is an internal subroutine, and is NOT meant to be called
        ! outside of nc_diag_write. Calling this subroutine in your
        ! program may result in unexpected behavior and/or data
        ! corruption!
        ! 
        ! Args:
        !     None
        ! 
        ! Raises:
        !     If there is no file open, or if no data/state needs to be
        !     cleaned up, this will result in an error.
        !     
        !     Issues with storage deallocation are bugs, and will also
        !     result in an error with an indication that a bug has
        !     occurred.
        ! 
        subroutine nc_diag_finish
#ifdef ENABLE_ACTION_MSGS
            if (nclayer_enable_action) then
                call nclayer_actionm("nc_diag_finish()")
            end if
#endif
            ! Make sure that we only deallocate if we have something
            ! open/initialized!
            if (init_done) then
                call nclayer_info("Cleaning up...")
                
                ! Do some quick sanity checks!
                if (.NOT. allocated(diag_chaninfo_store)) then
                    call nclayer_error("BUG! diag_chaninfo_store is not allocated, but init_done is set!")
                end if
                
                if (.NOT. allocated(diag_metadata_store)) then
                    call nclayer_error("BUG! diag_metadata_store is not allocated, but init_done is set!")
                end if
                
                if (.NOT. allocated(diag_data2d_store)) then
                    call nclayer_error("BUG! diag_data2d_store is not allocated, but init_done is set!")
                end if
                
                if (.NOT. allocated(diag_varattr_store)) then
                    call nclayer_error("BUG! diag_data2d_store is not allocated, but init_done is set!")
                end if
                
                ! Deallocate everything! Note that this deallocates
                ! everything within the derived type as well.
                ! (See? Fortran is better than C!)
                deallocate(diag_chaninfo_store)
                deallocate(diag_metadata_store)
                deallocate(diag_data2d_store)
                deallocate(diag_varattr_store)
                
                ! Clear initialization, append, and current file name
                ! state.
                init_done = .FALSE.
                append_only = .FALSE.
                cur_nc_file = ""
            else
                call nclayer_error("Attempted to deallocate without initializing!")
            end if
        end subroutine nc_diag_finish
        
        ! Flush all of the current variable data to NetCDF, and reset
        ! all of the variable storage to an initial state.
        ! 
        ! Attempt to write the currently stored variable definitions
        ! and data to the NetCDF file via NetCDF API calls.
        ! 
        ! Once done, this will effectively "flush" the data from the
        ! current variable buffers. Internally, this sets a starting
        ! counter and resets the buffer counter so that new data can
        ! be stored sequentially without requiring more memory, at least
        ! until memory runs out for the current buffer.
        ! 
        ! Definitions MUST be locked in order for flushing to work.
        ! Without definition locking, nc_diag_write is unable to make
        ! calls to NetCDF due to the lack of variable IDs.
        ! 
        ! If definitions are not locked, calling this will result in an
        ! error.
        ! 
        ! Data locking does NOT occur with flushing. As a result, this
        ! subroutine may be called multiple times, and a final
        ! nc_diag_write can be called once after this call.
        ! 
        ! (Note that calling nc_diag_write will lock the data and close
        ! the file, regardless of flushing the buffer here!)
        ! 
        ! Args:
        !     None
        ! 
        ! Raises:
        !     If definitions have not been locked, this will result in
        !     an error.
        !     
        !     The following errors will trigger indirectly from other
        !     subroutines called here:
        !     
        !     If the variable data writing has already been locked, this
        !     will result in an error.
        !     
        !     If there is no file open (or the file is already closed),
        !     this will result in an error.
        !     
        !     Other errors may result from invalid data storage, NetCDF
        !     errors, or even a bug. See the called subroutines'
        !     documentation for details.
        ! 
        subroutine nc_diag_flush_buffer
#ifdef ENABLE_ACTION_MSGS
            if (nclayer_enable_action) then
                call nclayer_actionm("nc_diag_flush_buffer()")
            end if
#endif
            if (.NOT. init_done) &
                call nclayer_error("Attempted to flush nc_diag_write buffers without initializing!")
            
            if ((.NOT. diag_chaninfo_store%def_lock) .OR. &
                (.NOT. diag_metadata_store%def_lock) .OR. &
                (.NOT. diag_data2d_store%def_lock)) &
                call nclayer_error("Definitions must be locked in order to flush the buffer!")
            
            ! Perform writes with the buffer flag set!
            call nclayer_info("Flushing chaninfo:")
            call nc_diag_chaninfo_write_data(.TRUE.)
            
            call nclayer_info("Flushing metadata:")
            call nc_diag_metadata_write_data(.TRUE.)
            
            call nclayer_info("Flushing data2d:")
            call nc_diag_data2d_write_data(.TRUE.)
            
            call nclayer_info("Flushing done!")
        end subroutine nc_diag_flush_buffer
        
        ! Force NetCDF to flush its buffers and write any data stored to
        ! disk.
        ! 
        ! Attempt to force the write of NetCDF's stored variable data to
        ! the NetCDF file via NetCDF API calls.
        ! 
        ! This does NOT flush nc_diag_write's buffers. It only attempts
        ! to flush NetCDF's internal buffers to disk.
        ! 
        ! If there is no file open, or the file is already closed, this
        ! will result in an error.
        ! 
        ! Args:
        !     None
        ! 
        ! Raises:
        !     If there is no file open (or the file is already closed),
        !     this will result in an error.
        !     
        !     Other errors may result from NetCDF errors. Any errors
        !     from NetCDF are likely to occur if there are problems
        !     writing to disk. Errors resulting from problems with
        !     manipulating NetCDF memory or a glitch are unlikely, but
        !     still possible.
        ! 
        subroutine nc_diag_flush_to_file
#ifdef ENABLE_ACTION_MSGS
            if (nclayer_enable_action) then
                call nclayer_actionm("nc_diag_flush_to_file()")
            end if
#endif
            ! Make sure we have something open + initialized
            if (.NOT. init_done) &
                call nclayer_error("Attempted to flush NetCDF buffers without initializing!")
            
            ! Call nf90_sync to try and commit the put'd data to disk
            call nclayer_check(nf90_sync(ncid))
        end subroutine nc_diag_flush_to_file
        
        ! Toggle whether nc_diag_write should be strict about dimensions
        ! and variable consistency.
        ! 
        ! Set the strictness of nc_diag_write for checking dimensions
        ! and stored variable consistency.
        ! 
        ! If set to TRUE, nc_diag_write will error when consistency
        ! checks fail.
        ! 
        ! If set to FALSE, nc_diag_write will only display a warning
        ! when these checks fail.
        ! 
        ! To see more details about what checks are made, see the
        ! corresponding called subroutine documentation for details.
        ! 
        ! Args:
        !     enable_strict (logical): whether to be strict with
        !         consistency checks or not.
        ! 
        ! Raises:
        !     If there is no file open (or the file is already closed),
        !     this will result in an error.
        !     
        !     Although unlikely, other errors may indirectly occur.
        !     They may be general storage errors, or even a bug.
        !     See the called subroutines' documentation for details.
        ! 
        subroutine nc_diag_set_strict(enable_strict)
            logical, intent(in) :: enable_strict
            
            ! Make sure we have something open + initialized
            if (init_done) then
                ! Call all of the variable set_strict subroutines
                call nc_diag_chaninfo_set_strict(enable_strict)
                call nc_diag_metadata_set_strict(enable_strict)
                call nc_diag_data2d_set_strict(enable_strict)
            else
                call nclayer_error("Can't set strictness level - NetCDF4 layer not initialized yet!")
            end if
        end subroutine nc_diag_set_strict
        
        ! Toggle whether nc_diag_write should trim strings or keep their
        ! original length.
        ! 
        ! Set the option to trim strings automatically with string
        ! variable data or not.
        ! 
        ! If set to TRUE, nc_diag_write will automatically trim strings
        ! to the minimum needed to hold the string. (Extra spaces at
        ! the end will be trimmed off the largest string in an array,
        ! and the result will be the bounds for that string array!)
        ! 
        ! If set to FALSE, nc_diag_write will NOT trim any strings. The
        ! given string length is assumed to be the bounds for holding
        ! the string. However, nc_diag_write will enforce strict
        ! checking of the input string length. If the length of the
        ! string changes during subsequent storage, nc_diag_write
        ! will error.
        ! 
        ! Note that this only applies to variable string storage.
        ! Attribute string storage is handled directly by NetCDF.
        ! From testing, it seems that NetCDF will trim your string when
        ! storing headers (global attributes).
        ! 
        ! Args:
        !     do_trim (logical): whether to automatically trim the
        !         stored strings or not.
        ! 
        ! Raises:
        !     Nothing... at least here. See above for potential errors
        !     outside of this subroutine.
        ! 
        subroutine nc_diag_set_trim(do_trim)
            logical, intent(in) :: do_trim
            
            enable_trim = do_trim
        end subroutine nc_diag_set_trim
end module nc_diag_write_mod
