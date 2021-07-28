import struct
from sys import exit, argv
import pandas as pd

def read_osense( filename):

#! Structure for observation sensitivity information output
#type obsense_header
#  sequence
#  integer(i_kind) :: idate              ! Base date (initial date)
#  integer(i_kind) :: obsnum             ! Observation number (total)
#  integer(i_kind) :: convnum            ! Observation number (conventional)
#  integer(i_kind) :: oznum              ! Observation number (ozone)
#  integer(i_kind) :: satnum             ! Observation number (satellite)
#  integer(i_kind) :: npred              ! Number of predictors for bias correction
#  integer(i_kind) :: nanals             ! Number of members
#end type obsense_header
#
# where i_kind    - generic specification kind for default integer
# is a long integer


#
#! Type definition for observation sensitivity information file
#type obsense_info
#  sequence
#  real(r_single)  :: obfit_prior        ! Observation fit to the first guess
#  real(r_single)  :: obsprd_prior       ! Spread of observation prior
#  real(r_single)  :: ensmean_obnobc     ! Ensemble mean first guess (no bias correction)
#  real(r_single)  :: ensmean_ob         ! Ensemble mean first guess (bias corrected)
#  real(r_single)  :: ob                 ! Observation value
#  real(r_single)  :: oberrvar           ! Observation error variance
#  real(r_single)  :: lon                ! Longitude
#  real(r_single)  :: lat                ! Latitude
#  real(r_single)  :: pres               ! Pressure
#  real(r_single)  :: time               ! Observation time
#  real(r_single)  :: oberrvar_orig      ! Original error variance
#  integer(i_kind) :: stattype           ! Observation type
#  character(len=20) :: obtype           ! Observation element / Satellite name
#  integer(i_kind) :: indxsat            ! Satellite index (channel) set to zero
#  real(r_single)  :: osense_kin         ! Observation sensitivity (kinetic energy) [J/kg]
#  real(r_single)  :: osense_dry         ! Observation sensitivity (Dry total energy) [J/kg]
#  real(r_single)  :: osense_moist       ! Observation sensitivity (Moist total energy) [J/kg]
#end type obsense_info
#



#if len(sys.argv) > 0:
#if len(argv) > 0:
#    file = argv[1]


    datacolumns =  [ 'obfit_prior',
                     'obsprd_prior',
                     'ensmean_obnobc',
                     'ensmean_ob',
                     'ob',
                     'oberrvar',
                     'lon',
                     'lat',
                     'pres',
                     'time',
                     'oberrvar_orig',
                     'stattype',
                     'obtype',
                     'indxsat',
                     'osense_kin',
                     'osense_dry',
                     'osense_moist' ] 


    headerf='<LLLLLLL'
    recordf='=fffffffffffl20slfff'

    header_size = struct.calcsize(headerf)
    osense_info_size = struct.calcsize(recordf)


    convdatatmp=[]
    convrecordtmp=[]
    convanalobstmp=[]
    satdatatmp=[]
    satrecordtmp=[]
    satanalobstmp=[]


    print('reading file ', filename)

    with open(filename,'rb') as fin:

        f = fin.read(4)
        header = fin.read(header_size)
        ( idate, obsnum, convnum, oznum, satnum, npred, nanals ) = struct.unpack(headerf,header)
        analobsf = '=' + 'f' * nanals
        analobs_size = struct.calcsize(analobsf)
        biaspredsf =  '=' + 'f' * ( npred + 1 )
        biaspreds_size = struct.calcsize(biaspredsf)

        print('read header: idate = ', idate, ', convnum + oznum = ', convnum + oznum, \
              ', satnum = ', satnum, ' npred = ', npred, ', nanals = ', nanals)

        print('reading conventional data...')
        for i in range(0, convnum + oznum):

           f = fin.read(8)
           record = fin.read(osense_info_size)
           analobs = fin.read(analobs_size)
           convdatarecord = struct.unpack( recordf, record ) 
           # pull out and clean up obtype
           obtype = str( convdatarecord[12] )[2:-1].strip() 
           convdatarecord = convdatarecord[0:12] + ( obtype, ) + convdatarecord[13:]
           convdatatmp.append( convdatarecord )

        print('reading satellite data...')
        for i in range(0, satnum ):

           f = fin.read(8)
           record = fin.read( osense_info_size )
           analobs = fin.read( analobs_size )
           biaspreds = fin.read( biaspreds_size )
           satdatarecord = struct.unpack( recordf, record ) 
           # pull out and clean up obtype
           obtype = str( satdatarecord[12] )[2:-1].strip() 
           satdatarecord = satdatarecord[0:12] + ( obtype, ) + satdatarecord[13:]
           satdatatmp.append( satdatarecord )


    convdata = pd.DataFrame( convdatatmp, columns = datacolumns )

    satdata = pd.DataFrame( satdatatmp, columns = datacolumns )

    print(convdata.describe())
    print(satdata.describe())

    return( ( convdata, satdata) )
