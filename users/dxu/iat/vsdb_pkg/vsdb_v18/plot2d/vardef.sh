#!/bin/ksh
set -x

##
##--define variables and attributes of 2D fields from GFS 
##  forecasts for making graphics using GRADS.
##--Fanglin Yang, November 2014
##


export grp_com="TMP2m DPT2m TMAX2m TMIN2m RH2m SPFH2m TMPsfc  \
APCPsfc ACPCPsfc NCPCPsfc PRATEsfc CPRATsfc WATRsfc WEASDsfc SNODsfc PWATclm  \
CRAINsfc CSNOWsfc CFRZRsfc CICEPsfc MSLETmsl PRESsfc PRESmsl PRMSLmsl  \
LFTXsfc no4LFTXsfc CAPEsfc CINsfc CWATclm CWORKclm  \
TOZNEclm VISsfc VRATEpbl HGTsfc HGThtfl HINDEXsfc HPBLsfc ICECsfc ICETKsfc LANDsfc"

export grp_wind="UGWDsfc UFLXsfc UGRD10m UGRDpbl VGWDsfc VFLXsfc VGRD10m VGRDpbl  FRICVsfc GUSTsfc"

export grp_rad="DLWRFsfc ULWRFsfc ULWRFtoa DSWRFsfc USWRFsfc USWRFtoa ALBDOsfc \
GFLUXsfc LHTFLsfc SHTFLsfc SUNSDsfc CDUVBsfc DUVBsfc PEVPRsfc BRTMPtoa "

export grp_cld="TCDC475mb TCDCclm \
TCDCbcl TCDClcl TCDCmcl TCDChcl TCDCcvl \
PRESlcb PRESmcb PREShcb PREScvb PRESlct PRESmct PREShct PREScvt  \
TMPlct  TMPmct  TMPhct  "

export grp_soil="WILTsfc FLDCPsfc SFCRsfc CNWATsfc \
SOILL0_10cm SOILL10_40cm SOILL40_100cm SOILL100_200cm \
SOILW0_10cm SOILW10_40cm SOILW40_100cm SOILW100_200cm \
TSOIL0_10cm TSOIL10_40cm TSOIL40_100cm TSOIL100_200cm "

export grp_sig="\
TMPtrp PREStrp HGTtrp ICAHTtrp UGRDtrp VGRDtrp VWSHtrp  \
RHsig995 POTsig995 VVELsig995 UGRDsig995 VGRDsig995 TMPsig995  \
MNTSF320K PVORT320K TMP320K UGRD320K VGRD320K \
TMPmwl PRESmwl HGTmwl ICAHTmwl UGRDmwl VGRDmwl  \
RHclm RHhtfl USTM0_6000m VSTM0_6000m  DPT30_0mb PWAT30_0mb PLI30_0mb \
HLCY0_3000m PRES80m SPFH80m  HGT0deg RH0deg \
PLPL255_0mb CAPE255_0mb CAPE180_0mb CIN255_0mb CIN180_0mb "

export grp_pv="\
HGTpv500  HGTpv1000  HGTpv1500  HGTpv2000  HGTpvneg500  HGTpvneg1000  HGTpvneg1500  HGTpvneg2000  \
UGRDpv500 UGRDpv1000 UGRDpv1500 UGRDpv2000 UGRDpvneg500 UGRDpvneg1000 UGRDpvneg1500 UGRDpvneg2000 \
VGRDpv500 VGRDpv1000 VGRDpv1500 VGRDpv2000 VGRDpvneg500 VGRDpvneg1000 VGRDpvneg1500 VGRDpvneg2000 \
VWSHpv500 VWSHpv1000 VWSHpv1500 VWSHpv2000 VWSHpvneg500 VWSHpvneg1000 VWSHpvneg1500 VWSHpvneg2000 \
TMPpv500  TMPpv1000  TMPpv1500  TMPpv2000  TMPpvneg500  TMPpvneg1000  TMPpvneg1500  TMPpvneg2000  \
PRESpv500 PRESpv1000 PRESpv1500 PRESpv2000 PRESpvneg500 PRESpvneg1000 PRESpvneg1500 PRESpvneg2000 "

export grp_en1="\
RH30_0mb   RH60_30mb   RH90_60mb   RH120_90mb   RH150_120mb   RH180_150mb    \
SPFH30_0mb SPFH60_30mb SPFH90_60mb SPFH120_90mb SPFH150_120mb SPFH180_150mb  \
VGRD30_0mb VGRD60_30mb VGRD90_60mb VGRD120_90mb VGRD150_120mb VGRD180_150mb  \
UGRD30_0mb UGRD60_30mb UGRD90_60mb UGRD120_90mb UGRD150_120mb UGRD180_150mb  \
TMP30_0mb  TMP60_30mb  TMP90_60mb  TMP120_90mb  TMP150_120mb  TMP180_150mb   \
RHsg33_100 RHsg44_72   RHsg44_100  RHsg72_94 "

export grp_en2="\
TMP4572m  TMP3658m  TMP2743m  TMP1829m  TMP914m  TMP610m  TMP457m  TMP305m  TMP100m  TMP80m  \
UGRD4572m UGRD3658m UGRD2743m UGRD1829m UGRD914m UGRD610m UGRD457m UGRD305m UGRD100m UGRD80m \
VGRD4572m VGRD3658m VGRD2743m VGRD1829m VGRD914m VGRD610m VGRD457m VGRD305m VGRD100m VGRD80m "

#-------------------------------------------------------------------
#-------------------------------------------------------------------
export vname_no4LFTXsfc="Surface Best (4-Layer) Lifted Index [K] "
export scale_no4LFTXsfc=1
export clevs0_no4LFTXsfc=" -20 -15 -10 -8 -6 -4 -2  -1 -0.5 0.5 1 2  4  6  8 10 15 20"   
export color0_no4LFTXsfc="49 48  47 46   45  44 43 42 41   0 21 22 23 24 25 26 27 28 29"      
export clevs_no4LFTXsfc="   -3   -2  -1.5 -1  -0.5  -0.1 0.1 0.5  1 1.5  2  3"                
export color_no4LFTXsfc="49   46   42   39  36   32    0    22  26 29  73 76   79"              

export vname_ACPCPsfc="Surface Convective Precipitation [kg/m^2] "
export scale_ACPCPsfc=1
export clevs0_ACPCPsfc=" 0.1  0.2  0.4 0.6  0.8  1  1.5  2  2.5  3"                         
export color0_ACPCPsfc="0  32    34   36  37   39 42   44  46  47  49"                      
export clevs_ACPCPsfc="  -1.5 -1.2 -0.9  -0.6 -0.3  -0.1   0.1   0.3  0.6  0.9 1.2 1.5"
export color_ACPCPsfc="49   46   42   39    36   32      0     22   26   29  73  76   79" 

export vname_ALBDOsfc="Surface Albedo [%] "
export scale_ALBDOsfc=1
export clevs0_ALBDOsfc=" 5   10    15   20  25  30  35   40  45  50"                   
export color0_ALBDOsfc="0  42    44   46  47   49 22   24  26  27  29"                     
export clevs_ALBDOsfc="   -3   -2  -1.5  -1  -0.5  -0.1   0.1   0.5     1  1.5  2  3" 
export color_ALBDOsfc="49   46   42   39    36   32      0     22   26   29  73  76   79" 

export vname_APCPsfc="Surface Total Precipitation [kg/m^2] "
export scale_APCPsfc=1
export clevs0_APCPsfc=" 0.1  0.2  0.4 0.6  0.8  1  1.5  2  2.5  3"                         
export color0_APCPsfc="0  32    34   36  37   39 42   44  46  47  49"                      
export clevs_APCPsfc="  -1.5 -1.2 -0.9  -0.6 -0.3  -0.1   0.1   0.3  0.6  0.9 1.2 1.5"
export color_APCPsfc="49   46   42   39    36   32      0     22   26   29  73  76   79" 

export vname_BRTMPtoa="Top of Atmos Brightness Temperature [K] "
export scale_BRTMPtoa=1
export clevs0_BRTMPtoa=" 240 245  250 255 260 265 270 275 280 285 290 295 300"         
export color0_BRTMPtoa="49 48  47   46  45  44   43  22  23  24  25  26  27  28"           
export clevs_BRTMPtoa="  -10  -8   -6   -4    -2    -1     1     2    4    6   8  10"                       
export color_BRTMPtoa="49   46   42   39    36   32      0     22   26   29  73  76   79" 

export vname_CAPEsfc="Surface Convective Avail Potential Energy [J/kg] "
export scale_CAPEsfc=1
export clevs0_CAPEsfc=" 100 300 500 600  700  800 900 1000 1100 1200  1300 1400 1500 1600 1700 1800"
export color0_CAPEsfc="0  41   42  43  44   45  46  47   48   22     23   24   25   26   27   28  29"           
export clevs_CAPEsfc=" -300  -200 -100 -50  -30  -10     10     30   50  100 200 300"               
export color_CAPEsfc="49   46   42   39    36   32      0     22   26   29  73  76   79" 

export vname_CAPE180_0mb="180-0 mb Above Ground Convective Avail Potential Energy [J/kg] "
export scale_CAPE180_0mb=1
export clevs0_CAPE180_0mb=" 100 300 500 600  700  800 900 1000 1100 1200  1300 1400 1500 1600 1700 1800"
export color0_CAPE180_0mb="0  41   42  43  44   45  46  47   48   22     23   24   25   26   27   28  29"           
export clevs_CAPE180_0mb=" -300  -200 -100 -50  -30  -10     10     30   50  100 200 300"
export color_CAPE180_0mb="49   46   42   39    36   32      0     22   26   29  73  76   79" 

export vname_CAPE255_0mb="255-0 mb Above Ground Convective Avail Potential Energy [J/kg] "
export scale_CAPE255_0mb=1
export clevs0_CAPE255_0mb=" 100 300 500 600  700  800 900 1000 1100 1200  1300 1400 1500 1600 1700 1800"
export color0_CAPE255_0mb="0  41   42  43  44   45  46  47   48   22     23   24   25   26   27   28  29"           
export clevs_CAPE255_0mb=" -300  -200 -100 -50  -30  -10     10     30   50  100 200 300"
export color_CAPE255_0mb="49   46   42   39    36   32      0     22   26   29  73  76   79" 

export vname_CDUVBsfc="Surface Clear Sky UV-B Downward Solar Flux [W/m^2] "
export scale_CDUVBsfc=1
export clevs0_CDUVBsfc=" 0.1  0.5  1  2  3  4  5  6  7"
export color0_CDUVBsfc="0   42   44 46 48 22 24 26 28  29"           
export clevs_CDUVBsfc="  -1.5 -1.2 -0.9  -0.6 -0.3  -0.1   0.1   0.3  0.6  0.9 1.2 1.5"
export color_CDUVBsfc="49   46   42   39    36   32      0     22   26   29  73  76   79" 

export vname_CFRZRsfc="Surface Categorical Freezing Rain [yes=1;no=0] "
export scale_CFRZRsfc=1
export clevs0_CFRZRsfc=" 0.001  0.01 0.02 0.04 0.06 0.08 0.1 0.2 0.4 0.6 0.8  1"                      
export color0_CFRZRsfc="0     32   33   34   35   36   38   42  43  45  46  47  49"             
export clevs_CFRZRsfc="    -1 -0.5 -0.1 -0.05 -0.01 -0.001 0.001 0.01 0.05 0.1 0.5 1" 
export color_CFRZRsfc="49   46   42   39    36   32      0     22   26   29  73  76   79"

export vname_CICEPsfc="Surface Categorical Ice Pellets [yes=1;no=0] "
export scale_CICEPsfc=1
export clevs0_CICEPsfc=" 0.0001 0.001  0.01 0.02 0.04 0.06 0.08 0.1 0.2 0.4 0.6 0.8"                      
export color0_CICEPsfc="0     32     33    34   35   36   38   42  43  45  46  47  49"             
export clevs_CICEPsfc="    -1 -0.5 -0.1 -0.05 -0.01 -0.001 0.001 0.01 0.05 0.1 0.5 1" 
export color_CICEPsfc="49   46   42   39    36   32      0     22   26   29  73  76   79" 

export vname_CINsfc="Surface Convective Inhibition [J/kg] "
export scale_CINsfc=1
export clevs0_CINsfc=" -160 -140 -120 -100  -80  -60  -40  -20   -10    10     20  40   60  80 "
export color0_CINsfc="49   47   45   43   39  37    35   33    31    0      21   23   25  27  29" 
export clevs_CINsfc="  -30  -20  -15  -10  -5    -2      2     5    10   15  20  30"
export color_CINsfc="49   46   42   39    36   32      0     22   26   29  73  76   79" 

export vname_CIN180_0mb="180-0 mb Above Ground Convective Inhibition [J/kg] "
export scale_CIN180_0mb=1
export clevs0_CIN180_0mb=" -100  -80  -60  -40  -20   -10    10     20  40   60  80  100"
export color0_CIN180_0mb="49   46   42   39   36   32      0     22   26   29  73  76   79" 
export clevs_CIN180_0mb="  -30  -20  -15  -10  -5    -2      2     5    10   15  20  30"
export color_CIN180_0mb="49   46   42   39    36   32      0     22   26   29  73  76   79" 

export vname_CIN255_0mb="255-0 mb Above Ground Convective Inhibition [J/kg] "
export scale_CIN255_0mb=1
export clevs0_CIN255_0mb=" -100  -80  -60  -40  -20   -10    10     20  40   60  80  100"
export color0_CIN255_0mb="49   46   42   39   36   32      0     22   26   29  73  76   79" 
export clevs_CIN255_0mb="  -30  -20  -15  -10  -5    -2      2     5    10   15  20  30"
export color_CIN255_0mb="49   46   42   39    36   32      0     22   26   29  73  76   79" 

export vname_CNWATsfc="Surface Plant Canopy Surface Water [kg/m^2] "
export scale_CNWATsfc=1
export clevs0_CNWATsfc=" 0.001  0.01 0.02 0.04 0.06 0.08 0.1 0.2 0.4 0.6 0.8 1.0"                      
export color0_CNWATsfc="0     32   33    34   35   36   38  42  43  45  46  47  49"             
export clevs_CNWATsfc="    -1 -0.5 -0.1 -0.05 -0.01 -0.001 0.001 0.01 0.05 0.1 0.5 1" 
export color_CNWATsfc="49   46   42   39    36   32      0     22   26   29  73  76   79" 

export vname_CPRATsfc="Surface Convective Precip Rate [mm/day] "
export scale_CPRATsfc=24*3600
export clevs0_CPRATsfc=" 0.1  0.2  0.4 0.6  0.8  1  1.5  2  2.5  3"                         
export color0_CPRATsfc="0  32    34   36  37   39 42   44  46  47  49"                      
export clevs_CPRATsfc="  -6   -4   -2     -1 -0.5 -0.1     0.1   0.5   1   2   4   6" 
export color_CPRATsfc="49   46   42   39    36   32      0     22   26   29  73  76   79" 

export vname_CRAINsfc="Surface Categorical Rain [yes=1;no=0] "
export scale_CRAINsfc=1
export clevs0_CRAINsfc=" 0.001  0.01 0.02 0.04 0.06 0.08 0.1 0.2 0.4 0.6 0.8  1"                      
export color0_CRAINsfc="0     32   33   34   35   36   38   42  43  45  46  47  49"             
export clevs_CRAINsfc="  -1 -0.5 -0.1 -0.05 -0.01 -0.001 0.001 0.01 0.05 0.1 0.5 1" 
export color_CRAINsfc="49  46   42   39    36   32      0     22   26   29 73  76   79" 

export vname_CSNOWsfc="Surface Categorical Snow [yes=1;no=0] "
export scale_CSNOWsfc=1
export clevs0_CSNOWsfc=" 0.001  0.01 0.02 0.04 0.06 0.08 0.1 0.2 0.4 0.6 0.8  1"                      
export color0_CSNOWsfc="0     32   33   34   35   36   38   42  43  45  46  47  49"             
export clevs_CSNOWsfc="  -1 -0.5 -0.1 -0.05 -0.01 -0.001 0.001 0.01 0.05 0.1 0.5 1" 
export color_CSNOWsfc="49  46   42   39    36   32      0     22   26   29 73  76   79" 

export vname_CWATclm="Atmos Column Cloud Water [g/m^2] "
export scale_CWATclm=1000
export clevs0_CWATclm=" 20  40  60  80  100  120  140  160  180  200"  
export color0_CWATclm="0  32  34  36  38   42   43    45  47   48   49"           
export clevs_CWATclm="  -30  -20  -15  -10  -5    -2      2     5    10   15  20  30"
export color_CWATclm="49   46   42   39    36   32      0     22   26   29  73  76   79" 

export vname_CWORKclm="Atmos Column Cloud Work Function [J/kg] "
export scale_CWORKclm=1
export clevs0_CWORKclm=" 10  20  40  60  80   100  120   140  160  180 "  
export color0_CWORKclm="0  32  34  36  38   42   43    45  47   48   49"           
export clevs_CWORKclm="  -30  -25  -20  -15  -10     -5    5      10   15  20  25  30  35"          
export color_CWORKclm="49   46   42   39    36   32      0     22   26   29  73  76   79" 

export vname_DLWRFsfc="Surface Downward LongWave Flux [W/m^2] "
export scale_DLWRFsfc=1
export clevs0_DLWRFsfc="  5 10  50  100 150 200 250 300 350 400 450 500"         
export color0_DLWRFsfc="0  32 34  36  38  42  44  46  48  22  24  26  28"                 
export clevs_DLWRFsfc="  -30  -20  -15  -10  -5    -2      2     5    10   15  20  30"
export color_DLWRFsfc="49   46   42   39    36   32      0     22   26   29  73  76   79" 

export vname_DPT2m="2m Above Ground Dew Point Temperature [K] "
export scale_DPT2m=1
export clevs0_DPT2m=" 240 245  250 255 260 265 270 275 280 285 290 295 300"         
export color0_DPT2m="49 48  47   46  45  44   43  22  23  24  25  26  27  28"           
export clevs_DPT2m="  -5   -3    -2  -1    -0.5 -0.1    0.1    0.5  1   2   3   5" 
export color_DPT2m="49   46   42   39    36   32      0     22   26   29  73  76   79" 

export vname_DPT30_0mb="30-0 mb Above Ground Dew Point Temperature [K] "
export scale_DPT30_0mb=1
export clevs0_DPT30_0mb=" 240 245  250 255 260 265 270 275 280 285 290 295 300"         
export color0_DPT30_0mb="49 48  47   46  45  44   43  22  23  24  25  26  27  28"           
export clevs_DPT30_0mb="  -5   -3    -2  -1    -0.5 -0.1    0.1    0.5  1   2   3   5" 
export color_DPT30_0mb="49   46   42   39    36   32      0     22   26   29  73  76   79" 

export vname_DSWRFsfc="Surface Downward ShortWave Flux [W/m^2] "
export scale_DSWRFsfc=1
export clevs0_DSWRFsfc="  5 10  50  100 150 200 250 300 350 400 450 500"         
export color0_DSWRFsfc="0  32 34  36  38  42  44  46  48  22  24  26  28"                 
export clevs_DSWRFsfc="  -30  -20  -15  -10  -5    -2      2     5    10   15  20  30"
export color_DSWRFsfc="49   46   42   39    36   32      0     22   26   29  73  76   79" 

export vname_DUVBsfc="Surface UV-B Downward Solar Flux [W/m^2] "
export scale_DUVBsfc=1
export clevs0_DUVBsfc=" 0.1  0.5  1  2  3  4  5  6  7"
export color0_DUVBsfc="0   42   44 46 48 22 24 26 28  29"           
export clevs_DUVBsfc="  -1.5 -1.2 -0.9  -0.6 -0.3  -0.1   0.1   0.3  0.6  0.9 1.2 1.5"
export color_DUVBsfc="49   46   42   39    36   32      0     22   26   29  73  76   79" 

export vname_FLDCPsfc="Surface Field Capacity [fraction] "
export scale_FLDCPsfc=1
export clevs0_FLDCPsfc=" 0.001  0.01 0.02 0.04 0.06 0.08 0.1 0.2 0.4 0.6 0.8  1"                      
export color0_FLDCPsfc="0     32   33   34   35   36   38   42  43  45  46  47  49"             
export clevs_FLDCPsfc="    -1 -0.5 -0.1 -0.05 -0.01 -0.001 0.001 0.01 0.05 0.1 0.5 1" 
export color_FLDCPsfc="49   46   42   39    36   32      0     22   26   29  73  76   79" 

export vname_FRICVsfc="Surface Friction Velocity [m/s] "
export scale_FRICVsfc=1
export clevs0_FRICVsfc=" 0.001  0.01 0.02 0.04 0.06 0.08 0.1 0.2 0.4 0.6 0.8  1"                      
export color0_FRICVsfc="0     32   33   34   35   36   38   42  43  45  46  47  49"             
export clevs_FRICVsfc="    -1 -0.5 -0.1 -0.05 -0.01 -0.001 0.001 0.01 0.05 0.1 0.5 1" 
export color_FRICVsfc="49   46   42   39    36   32      0     22   26   29  73  76   79" 

export vname_GFLUXsfc="Surface Ground Heat Flux [W/m^2] "
export scale_GFLUXsfc=1
export clevs0_GFLUXsfc="  -30  -20  -15  -10  -5    -2      2     5    10   15  20  30"
export color0_GFLUXsfc="49   46   42   39    36   32      0     22   26   29  73  76   79" 
export clevs_GFLUXsfc="  -30  -20  -15  -10  -5    -2      2     5    10   15  20  30"
export color_GFLUXsfc="49   46   42   39    36   32      0     22   26   29  73  76   79" 

export vname_GUSTsfc="Surface Wind Gust [m/s] "
export scale_GUSTsfc=1
export clevs0_GUSTsfc=" 0.5  1  2  4  6  8  10 12 14 16 18 20"                                             
export color0_GUSTsfc="0   32 34 36 38 42 44 46  48 22 24 26 28"      
export clevs_GUSTsfc="  -3   -2   -1   -0.5   -0.2   0.2 0.5  1  2 3"               
export color_GUSTsfc="49   47  45    44    42      0   22  23  25 27 29"                      

export vname_HGTsfc="Surface Geopotential Height [gpm] "
export scale_HGTsfc=1
export clevs0_HGTsfc=" 100 500 1000 1500 2000 2500 3000 3500 4000 4500 5000 6000 7000 8000"                
export color0_HGTsfc="0  41   42  43   44   45   46   47   48   22    24   26   27   28   29"           
export clevs_HGTsfc="  -400 -200 -100 -50   -20   -10     10     20 50   100 200 400"               
export color_HGTsfc="49   46   42   39    36   32      0     22   26   29  73  76   79" 

export vname_HGTpv500="PV=500 Units Level Geopotential Height [gpm] "
export scale_HGTpv500=1
export clevs0_HGTpv500=" 100 500 1000 1500 2000 2500 3000 3500 4000 4500 5000 6000 7000 8000"                
export color0_HGTpv500="0  41   42  43   44   45   46   47   48   22    24   26   27   28   29"           
export clevs_HGTpv500="  -400 -200 -100 -50   -20   -10     10     20 50   100 200 400" 
export color_HGTpv500="49   46   42   39    36   32      0     22   26   29  73  76   79" 

export vname_HGTpv1000="PV=1000 Units Level Geopotential Height [gpm] "
export scale_HGTpv1000=1
export clevs0_HGTpv1000=" 100 500 1000 1500 2000 2500 3000 3500 4000 4500 5000 6000 7000 8000"                
export color0_HGTpv1000="0  41   42  43   44   45   46   47   48   22    24   26   27   28   29"           
export clevs_HGTpv1000="  -400 -200 -100 -50   -20   -10     10     20 50   100 200 400" 
export color_HGTpv1000="49   46   42   39    36   32      0     22   26   29  73  76   79" 

export vname_HGTpv1500="PV=1500 Units Level Geopotential Height [gpm] "
export scale_HGTpv1500=1
export clevs0_HGTpv1500=" 100 500 1000 1500 2000 2500 3000 3500 4000 4500 5000 6000 7000 8000"                
export color0_HGTpv1500="0  41   42  43   44   45   46   47   48   22    24   26   27   28   29"           
export clevs_HGTpv1500="  -400 -200 -100 -50   -20   -10     10     20 50   100 200 400" 
export color_HGTpv1500="49   46   42   39    36   32      0     22   26   29  73  76   79" 

export vname_HGTpv2000="PV=2000 Units Level Geopotential Height [gpm] "
export scale_HGTpv2000=1
export clevs0_HGTpv2000=" 100 500 1000 1500 2000 2500 3000 3500 4000 4500 5000 6000 7000 8000"                
export color0_HGTpv2000="0  41   42  43   44   45   46   47   48   22    24   26   27   28   29"           
export clevs_HGTpv2000="  -400 -200 -100 -50   -20   -10     10     20 50   100 200 400" 
export color_HGTpv2000="49   46   42   39    36   32      0     22   26   29  73  76   79" 

export vname_HGTpvneg500="PV=-500 Units Level Geopotential Height [gpm] "
export scale_HGTpvneg500=1
export clevs0_HGTpvneg500=" 100 500 1000 1500 2000 2500 3000 3500 4000 4500 5000 6000 7000 8000"                
export color0_HGTpvneg500="0  41   42  43   44   45   46   47   48   22    24   26   27   28   29"           
export clevs_HGTpvneg500="  -400 -200 -100 -50   -20   -10     10     20 50   100 200 400" 
export color_HGTpvneg500="49   46   42   39    36   32      0     22   26   29  73  76   79" 

export vname_HGTpvneg1000="PV=-1000 Units Level Geopotential Height [gpm] "
export scale_HGTpvneg1000=1
export clevs0_HGTpvneg1000=" 100 500 1000 1500 2000 2500 3000 3500 4000 4500 5000 6000 7000 8000"                
export color0_HGTpvneg1000="0  41   42  43   44   45   46   47   48   22    24   26   27   28   29"           
export clevs_HGTpvneg1000="  -400 -200 -100 -50   -20   -10     10     20 50   100 200 400" 
export color_HGTpvneg1000="49   46   42   39    36   32      0     22   26   29  73  76   79" 

export vname_HGTpvneg1500="PV=-1500 Units Level Geopotential Height [gpm] "
export scale_HGTpvneg1500=1
export clevs0_HGTpvneg1500=" 100 500 1000 1500 2000 2500 3000 3500 4000 4500 5000 6000 7000 8000"                
export color0_HGTpvneg1500="0  41   42  43   44   45   46   47   48   22    24   26   27   28   29"           
export clevs_HGTpvneg1500="  -400 -200 -100 -50   -20   -10     10     20 50   100 200 400" 
export color_HGTpvneg1500="49   46   42   39    36   32      0     22   26   29  73  76   79" 

export vname_HGTpvneg2000="PV=-2000 Units Level Geopotential Height [gpm] "
export scale_HGTpvneg2000=1
export clevs0_HGTpvneg2000=" 100 500 1000 1500 2000 2500 3000 3500 4000 4500 5000 6000 7000 8000"                
export color0_HGTpvneg2000="0  41   42  43   44   45   46   47   48   22    24   26   27   28   29"           
export clevs_HGTpvneg2000="  -400 -200 -100 -50   -20   -10     10     20 50   100 200 400" 
export color_HGTpvneg2000="49   46   42   39    36   32      0     22   26   29  73  76   79" 

export vname_HGThtfl="Highest Trop Freezing Level Geopotential Height [gpm] "
export scale_HGThtfl=1
export clevs0_HGThtfl=" 100 500 1000 1500 2000 2500 3000 3500 4000 4500 5000 6000 7000 8000"                
export color0_HGThtfl="0  41   42  43   44   45   46   47   48   22    24   26   27   28   29"           
export clevs_HGThtfl="  -400 -200 -100 -50   -20   -10     10     20 50   100 200 400" 
export color_HGThtfl="49   46   42   39    36   32      0     22   26   29  73  76   79" 

export vname_HGT0deg="0C Isotherm Level Geopotential Height [gpm] "
export scale_HGT0deg=1
export clevs0_HGT0deg=" 100 500 1000 1500 2000 2500 3000 3500 4000 4500 5000 6000 7000 8000"                
export color0_HGT0deg="0  41   42  43   44   45   46   47   48   22    24   26   27   28   29"           
export clevs_HGT0deg="  -400 -200 -100 -50   -20   -10     10     20 50   100 200 400" 
export color_HGT0deg="49   46   42   39    36   32      0     22   26   29  73  76   79" 

export vname_HGTmwl=" Max Wind Level Geopotential Height [km] "
export scale_HGTmwl=0.001
export clevs0_HGTmwl=" 1   2   3     4   5     6   7     8   9     10   12   14    16   18"                       
export color0_HGTmwl="0  41   42  43   44   45   46   47   48   22    24   26   27   28   29"           
export clevs_HGTmwl="  -1   -0.8 -0.6 -0.4  -0.2  -0.1    0.1  0.2  0.4  0.6 0.8 1.0" 
export color_HGTmwl="49   46   42   39    36   32      0     22   26   29  73  76   79" 

export vname_HGTtrp=" Tropopause Geopotential Height [km] "
export scale_HGTtrp=0.001
export clevs0_HGTtrp=" 1   2   3     4   5     6   7     8   9     10   12   14    16   18"                       
export color0_HGTtrp="0  41   42  43   44   45   46   47   48   22    24   26   27   28   29"           
export clevs_HGTtrp="  -1   -0.8 -0.6 -0.4  -0.2  -0.1    0.1  0.2  0.4  0.6 0.8 1.0" 
export color_HGTtrp="49   46   42   39    36   32      0     22   26   29  73  76   79" 

export vname_HINDEXsfc="Surface Haines index "
export scale_HINDEXsfc=1
export clevs0_HINDEXsfc=" 1   2   3     4   5     6   7     8   9   "                       
export color0_HINDEXsfc="0  42  44   46   22   24   26   27   28   29"           
export clevs_HINDEXsfc="  -3  -2    -1    -0.5 -0.1 -0.01 0.01 0.1   0.5  1   2   3  " 
export color_HINDEXsfc="49   46   42   39    36   32      0   22   26   29  73  76   79" 

export vname_HLCY0_3000m="0-3000m Above Ground Storm Relative Helicity [m^2/s^2] "
export scale_HLCY0_3000m=1
export clevs0_HLCY0_3000m=" 300 400  500  600 700 800 900 1000 1100 1200 1400 1600 1800"                               
export color0_HLCY0_3000m="0  41   42  43   44   45  46  47  48   22    24   26   27    29"           
export clevs_HLCY0_3000m="  -400 -200 -100 -50   -20   -10     10     20 50   100 200 400"
export color_HLCY0_3000m="49   46   42   39    36   32      0     22   26   29  73  76   79" 

export vname_HPBLsfc="Planetary Boundary Layer Height [m] "
export scale_HPBLsfc=1
export clevs0_HPBLsfc=" 300 400  500  600 700 800 900 1000 1100 1200 1400 1600 1800"     
export color0_HPBLsfc="0  41   42  43   44   45  46  47  48   22    24   26   27    29"           
export clevs_HPBLsfc="  -600 -400 -200 -100 -50   -20   20 50   100 200 400 600" 
export color_HPBLsfc="49   46   42   39    36   32      0     22   26   29  73  76   79" 

export vname_ICAHTmwl="Max Wind Level ICAO Standard Atmosphere Reference Height [km] "
export scale_ICAHTmwl=0.001
export clevs0_ICAHTmwl=" 1   2   3     4   5     6   7     8   9     10   12   14    16   18"        
export color0_ICAHTmwl="0  41   42  43   44   45   46   47   48   22    24   26   27   28   29"           
export clevs_ICAHTmwl="  -1   -0.8 -0.6 -0.4  -0.2  -0.1    0.1  0.2  0.4  0.6 0.8 1.0" 
export color_ICAHTmwl="49   46   42   39    36   32      0     22   26   29  73  76   79" 

export vname_ICAHTtrp="Tropopause ICAO Standard Atmosphere Reference Height [km] "
export scale_ICAHTtrp=0.001
export clevs0_ICAHTtrp=" 1   2   3     4   5     6   7     8   9     10   12   14    16   18"           
export color0_ICAHTtrp="0  41   42  43   44   45   46   47   48   22    24   26   27   28   29"    
export clevs_ICAHTtrp="  -1   -0.8 -0.6 -0.4  -0.2  -0.1    0.1  0.2  0.4  0.6 0.8 1.0" 
export color_ICAHTtrp="49   46   42   39    36   32      0     22   26   29  73  76   79" 

export vname_ICECsfc="Surface Ice concentration (ice=1;no ice=0) [fraction] "
export scale_ICECsfc=1
export clevs0_ICECsfc=" 0.001  0.01 0.02 0.04 0.06 0.08 0.1 0.2 0.4 0.6 0.8  1"                      
export color0_ICECsfc="0     32   33   34   35   36   38   42  43  45  46  47  49"             
export clevs_ICECsfc="    -1 -0.5 -0.1 -0.05 -0.01 -0.001 0.001 0.01 0.05 0.1 0.5 1" 
export color_ICECsfc="49   46   42   39    36   32      0     22   26   29  73  76   79" 

export vname_ICETKsfc="Surface Ice Thickness [m] "
export scale_ICETKsfc=1
export clevs0_ICETKsfc=" 0.01 0.02 0.04 0.06 0.08 0.1 0.2 0.4 0.6 0.8  1   1.5  2"                      
export color0_ICETKsfc="0    32   33   34   35   36  38  42  43  45  46  47   48  49"             
export clevs_ICETKsfc="  -0.8 -0.6 -0.4  -0.2 -0.1 -0.05  0.05   0.1 0.2  0.4  0.6 0.8"
export color_ICETKsfc="49   46    42   39    36   32     0     22   26   29  73   76   79" 

export vname_LANDsfc="Surface Land Cover (land=1;sea=0) [fraction] "
export scale_LANDsfc=1
export clevs0_LANDsfc=" 0.001  0.01 0.02 0.04 0.06 0.08 0.1 0.2 0.4 0.6 0.8  1"                      
export color0_LANDsfc="0     32   33   34   35   36   38   42  43  45  46  47  49"             
export clevs_LANDsfc="    -1 -0.5 -0.1 -0.05 -0.01 -0.001 0.001 0.01 0.05 0.1 0.5 1" 
export color_LANDsfc="49   46   42   39    36   32      0     22   26   29  73  76   79" 

export vname_LFTXsfc="Surface Lifted Index [K] "
export scale_LFTXsfc=1
export clevs0_LFTXsfc=" -20 -15 -10 -8 -6 -4 -2  -1 -0.5 0.5 1 2  4  6  8 10 15 20"   
export color0_LFTXsfc="49 48  47 46   45  44 43 42 41   0 21 22 23 24 25 26 27 28 29"      
export clevs_LFTXsfc="   -3   -2  -1.5 -1  -0.5  -0.1 0.1 0.5  1 1.5  2  3"                
export color_LFTXsfc="49   46   42   39  36   32    0    22  26 29  73 76   79"              

export vname_LHTFLsfc="Surface Latent Heat Flux [W/m^2] "
export scale_LHTFLsfc=1
export clevs0_LHTFLsfc="  5 10  20  40  60  80  100 120 140 160 180 200"         
export color0_LHTFLsfc="0  32 34  36  38  42  44  46  48  22  24  26  28"                 
export clevs_LHTFLsfc="  -30  -20  -15  -10  -5    -2      2     5    10   15  20  30"
export color_LHTFLsfc="49   46   42   39    36   32      0     22   26   29  73  76   79" 

export vname_MNTSF320K="320K Level Montgomery Stream Function [1000*m^2/s^2] "
export scale_MNTSF320K=0.001
export clevs0_MNTSF320K=" 280 285 290 295 300 305 310 315  320  325 330 335 340"        
export color0_MNTSF320K="49 48  47   46  45  44   43  22  23  24  25  26  27  28"           
export clevs_MNTSF320K="  -5   -3    -2  -1    -0.5 -0.1    0.1    0.5  1   2   3   5" 
export color_MNTSF320K="49   46   42   39    36   32      0     22   26   29  73  76   79" 

export vname_MSLETmsl="Membrane Mean Sea Level Pressure [hPa] "
export scale_MSLETmsl=0.01
export clevs0_MSLETmsl=" 960 965 970 975 980 985 990 995 1000 1005 1010 1015 1020 "                         
export color0_MSLETmsl="49  48 47  46  45  44   43  33 34   35   37   21    23   25"  
export clevs_MSLETmsl="  -10   -5  -4    -3   -2   -1       1     2    3   4   5   10"
export color_MSLETmsl="49   46   42   39    36   32      0     22   26   29  73  76   79" 

export vname_NCPCPsfc="Surface Large Scale Precipitation [kg/m^2] "
export scale_NCPCPsfc=1
export clevs0_NCPCPsfc=" 0.1  0.2  0.4 0.6  0.8  1  1.5  2  2.5  3"                         
export color0_NCPCPsfc="0  32    34   36  37   39 42   44  46  47  49"                      
export clevs_NCPCPsfc="  -1.5 -1.2 -0.9  -0.6 -0.3  -0.1   0.1   0.3  0.6  0.9 1.2 1.5"
export color_NCPCPsfc="49   46   42   39    36   32      0     22   26   29  73  76   79" 

export vname_PEVPRsfc="Surface Potential Evaporation Rate [W/m^2] "
export scale_PEVPRsfc=1
export clevs0_PEVPRsfc="  5 10  50  100 150 200 250 300 350 400 450 500"
export color0_PEVPRsfc="0  32 34  36  38  42  44  46  48  22  24  26  28"
export clevs_PEVPRsfc="  -50  -30  -20  -15  -10  -5       5    10   15  20  30  50"
export color_PEVPRsfc="49   46   42   39    36   32      0     22   26   29  73  76   79" 

export vname_PLI30_0mb="30-0mb Above Ground Parcel Lifted Index (to 500 hPa) [K] "
export scale_PLI30_0mb=1
export clevs0_PLI30_0mb="   2    4   6  8  10  15  20  25  30"                         
export color0_PLI30_0mb="0   42   44  46 48  22  24  26  28  29"                            
export clevs_PLI30_0mb="   -2  -1.5  -1  -0.5  -0.1  -0.05  0.05  0.1   0.5  1  1.5  2 "
export color_PLI30_0mb="49   46   42   39    36   32      0     22   26   29  73  76   79" 

export vname_PLPL255_0mb="255-0mb Above Ground Pressure from Which Parcel Was Lifted [hPa]"
export scale_PLPL255_0mb=0.01
export clevs0_PLPL255_0mb=" 400  500 550  600 650 700 750 800 850 900 950 1000 1010  "                         
export color0_PLPL255_0mb="49  48  47  46  45  44  43  22  23  24   25   26   27  28"           
export clevs_PLPL255_0mb="  -20   -15 -10   -5   -2   -1       1     2    5   10  15  20" 
export color_PLPL255_0mb="49   46   42   39    36   32      0     22   26   29  73  76   79" 

export vname_POTsig995="Sigma=.995 Potential Temperature [K] "
export scale_POTsig995=1
export clevs0_POTsig995=" 240 245  250 255 260 265 270 275 280 285 290 295 300"         
export color0_POTsig995="49 48  47   46  45  44   43  22  23  24  25  26  27  28"           
export clevs_POTsig995="  -5   -3    -2  -1    -0.5 -0.1    0.1    0.5  1   2   3   5" 
export color_POTsig995="49   46   42   39    36   32      0     22   26   29  73  76   79" 

export vname_PRATEsfc="Surface Precipitation Rate [mm/day] "
export scale_PRATEsfc=24*3600
export clevs0_PRATsfc=" 0.1  0.2  0.4 0.6  0.8  1  1.5  2  2.5  3"                         
export color0_PRATsfc="0  32    34   36  37   39 42   44  46  47  49"                      
export clevs_PRATEsfc="  -3   -2   -1.5 -1.0  -0.5 -0.1     0.1   0.5   1   1.5 2   3"                        
export color_PRATEsfc="49   46   42   39    36    32      0     22   26   29  73  76   79" 

export vname_PRESsfc="Surface Pressure [hPa] "
export scale_PRESsfc=0.01
export clevs0_PRESsfc=" 400  500 550  600 650 700 750 800 850 900 950 1000 1010  "                         
export color0_PRESsfc="49  48 47  46  45  44   43  33 34   35   37   21    23   25"         
export clevs_PRESsfc="  -10   -5  -4    -3   -2   -1       1     2    3   4   5   10" 
export color_PRESsfc="49   46   42   39    36   32      0     22   26   29  73  76   79" 

export vname_PRESmsl="Mean-Sea Level Pressure [hPa] "
export scale_PRESmsl=0.01
export clevs0_PRESmsl=" 960 965 970 975 980 985 990 995 1000 1005 1010 1015 1020 "                         
export color0_PRESmsl="49  48 47  46  45  44   43  33 34   35   37   21    23   25"         
export clevs_PRESmsl="  -10   -5  -4    -3   -2   -1       1     2    3   4   5   10" 
export color_PRESmsl="49   46   42   39    36   32      0     22   26   29  73  76   79" 

export vname_PRES80m="80m Above Ground Pressure [hPa] "
export scale_PRES80m=0.01
export clevs0_PRES80m=" 960 965 970 975 980 985 990 995 1000 1005 1010 1015 1020 "                         
export color0_PRES80m="49  48  47  46  45  44  43  22  23  24   25   26   27  28"           
export clevs_PRES80m="  -20   -15 -10   -5   -2   -1       1     2    5   10  15  20" 
export color_PRES80m="49   46   42   39    36   32      0     22   26   29  73  76   79" 

export vname_PRESpv500="PV=500 Units Level Pressure [hPa] "
export scale_PRESpv500=0.01
export clevs0_PRESpv500=" 10 50 100 200 300  400  500 600 700 800 900 1000 "                         
export color0_PRESpv500="48 47  46  45  44  43   22  23  24  25  26   27   28"           
export clevs_PRESpv500="  -20   -15 -10   -5   -2   -1       1     2    5   10  15  20" 
export color_PRESpv500="49   46   42   39    36   32      0     22   26   29  73  76   79" 

export vname_PRESpv1000="PV=1000 Units Level Pressure [hPa] "
export scale_PRESpv1000=0.01
export clevs0_PRESpv1000=" 10 50 100 200 300  400  500 600 700 800 900 1000 "
export color0_PRESpv1000="49  48  47  46  45  44  43  22  23  24   25   26   27  28"           
export clevs_PRESpv1000="  -20   -15 -10   -5   -2   -1       1     2    5   10  15  20" 
export color_PRESpv1000="49   46   42   39    36   32      0     22   26   29  73  76   79" 

export vname_PRESpv1500="PV=1500 Units Level Pressure [hPa] "
export scale_PRESpv1500=0.01
export clevs0_PRESpv1500=" 10 50 100 200 300  400  500 600 700 800 900 1000 "
export color0_PRESpv1500="49  48  47  46  45  44  43  22  23  24   25   26   27  28"           
export clevs_PRESpv1500="  -20   -15 -10   -5   -2   -1       1     2    5   10  15  20" 
export color_PRESpv1500="49   46   42   39    36   32      0     22   26   29  73  76   79" 

export vname_PRESpv2000="PV=2000 Units Level Pressure [hPa] "
export scale_PRESpv2000=0.01
export clevs0_PRESpv2000=" 10 50 100 200 300  400  500 600 700 800 900 1000 "
export color0_PRESpv2000="49  48  47  46  45  44  43  22  23  24   25   26   27  28"           
export clevs_PRESpv2000="  -20   -15 -10   -5   -2   -1       1     2    5   10  15  20" 
export color_PRESpv2000="49   46   42   39    36   32      0     22   26   29  73  76   79" 

export vname_PRESpvneg500="PV=-500 Units Level Pressure [hPa] "
export scale_PRESpvneg500=0.01
export clevs0_PRESpvneg500=" 10 50 100 200 300  400  500 600 700 800 900 1000 "
export color0_PRESpvneg500="49  48  47  46  45  44  43  22  23  24   25   26   27  28"           
export clevs_PRESpvneg500="  -20   -15 -10   -5   -2   -1       1     2    5   10  15  20" 
export color_PRESpvneg500="49   46   42   39    36   32      0     22   26   29  73  76   79" 

export vname_PRESpvneg1000="PV=-1000 Units Level Pressure [hPa] "
export scale_PRESpvneg1000=0.01
export clevs0_PRESpvneg1000=" 10 50 100 200 300  400  500 600 700 800 900 1000 "
export color0_PRESpvneg1000="49  48  47  46  45  44  43  22  23  24   25   26   27  28"           
export clevs_PRESpvneg1000="  -20   -15 -10   -5   -2   -1       1     2    5   10  15  20" 
export color_PRESpvneg1000="49   46   42   39    36   32      0     22   26   29  73  76   79" 

export vname_PRESpvneg1500="PV=-1500 Units Level Pressure [hPa] "
export scale_PRESpvneg1500=0.01
export clevs0_PRESpvneg1500=" 10 50 100 200 300  400  500 600 700 800 900 1000 "
export color0_PRESpvneg1500="49  48  47  46  45  44  43  22  23  24   25   26   27  28"           
export clevs_PRESpvneg1500="  -20   -15 -10   -5   -2   -1       1     2    5   10  15  20" 
export color_PRESpvneg1500="49   46   42   39    36   32      0     22   26   29  73  76   79" 

export vname_PRESpvneg2000="PV=-2000 Units Level Pressure [hPa] "
export scale_PRESpvneg2000=0.01
export clevs0_PRESpvneg2000=" 10 50 100 200 300  400  500 600 700 800 900 1000 "
export color0_PRESpvneg2000="49  48  47  46  45  44  43  22  23  24   25   26   27  28"           
export clevs_PRESpvneg2000="  -20   -15 -10   -5   -2   -1       1     2    5   10  15  20" 
export color_PRESpvneg2000="49   46   42   39    36   32      0     22   26   29  73  76   79" 

export vname_PRESlcb="Low Cloud Base Pressure [hPa] "
export scale_PRESlcb=0.01
export clevs0_PRESlcb=" 780  800 820  840 860 880 900 920 940 960 980 1000 1020"
export color0_PRESlcb="49  48  47  46  45  44  43  22  23  24   25   26   27  28"           
export clevs_PRESlcb="  -20   -15 -10   -5   -2   -1       1     2    5   10  15  20" 
export color_PRESlcb="49   46   42   39    36   32      0     22   26   29  73  76   79" 

export vname_PRESlct="Low Cloud Top Pressure [hPa] "
export scale_PRESlct=0.01
export clevs0_PRESlct=" 600 620  640 660 680 700 720 740 760 780 800   820 840"
export color0_PRESlct="49  48  47  46  45  44  43  22  23  24   25   26   27  28"           
export clevs_PRESlct="  -20   -15 -10   -5   -2   -1       1     2    5   10  15  20" 
export color_PRESlct="49   46   42   39    36   32      0     22   26   29  73  76   79" 

export vname_PRESmcb="Mid-Cloud Base Pressure [hPa] "
export scale_PRESmcb=0.01
export clevs0_PRESmcb="  460 480 500 520 540 560 580 600 620  640 660  680 700 " 
export color0_PRESmcb="49  48  47  46  45  44  43  22  23  24   25   26   27  28"           
export clevs_PRESmcb="  -20   -15 -10   -5   -2   -1       1     2    5   10  15  20" 
export color_PRESmcb="49   46   42   39    36   32      0     22   26   29  73  76   79" 

export vname_PRESmct="Mid-Cloud Top Pressure [hPa] "
export scale_PRESmct=0.01
export clevs0_PRESmct=" 300 320 340 360 380 400 420  440 460 480 500  520 540" 
export color0_PRESmct="49  48  47  46  45  44  43  22  23  24  25   26  27  28"           
export clevs_PRESmct="  -20   -15 -10   -5   -2   -1       1     2    5   10  15  20" 
export color_PRESmct="49   46   42   39    36   32      0     22   26   29  73  76   79" 

export vname_PREShcb="High Cloud Base Pressure [hPa] "
export scale_PREShcb=0.01
export clevs0_PREShcb=" 180 200  220 240 260 280 300 320 340 360 380 400 420"
export color0_PREShcb="49  48  47  46  45  44  43  22  23  24   25  26   27  28"           
export clevs_PREShcb="  -20   -15 -10   -5   -2   -1       1     2    5   10  15  20" 
export color_PREShcb="49   46   42   39    36   32      0     22   26   29  73  76   79" 

export vname_PREShct="High Cloud Top Pressure [hPa] "
export scale_PREShct=0.01
export clevs0_PREShct=" 80  100  120 140 160 180 200 220 240 260 280 300 320" 
export color0_PREShct="49  48  47  46  45  44  43  22  23  24   25   26   27  28"           
export clevs_PREShct="  -20   -15 -10   -5   -2   -1       1     2    5   10  15  20" 
export color_PREShct="49   46   42   39    36   32      0     22   26   29  73  76   79" 

export vname_PREScvb="Convective Cloud Base Pressure [hPa] "
export scale_PREScvb=0.01
export clevs0_PREScvb=" 780  800 820  840 860 880 900 920 940 960 980 1000 1020"
export color0_PREScvb="49  48  47  46  45  44  43  22  23  24   25   26   27  28"           
export clevs_PREScvb="  -20   -15 -10   -5   -2   -1       1     2    5   10  15  20" 
export color_PREScvb="49   46   42   39    36   32      0     22   26   29  73  76   79" 

export vname_PREScvt="Convective Cloud Top Pressure [hPa] "
export scale_PREScvt=0.01
export clevs0_PREScvt="  150 200 250 300 350 400 450 500 550  600  650  700 800 850 "
export color0_PREScvt="49  48  47  46  45  44  43  21   22  23  24   25   26   27  28"           
export clevs_PREScvt="  -20   -15 -10   -5   -2   -1       1     2    5   10  15  20" 
export color_PREScvt="49   46   42   39    36   32      0     22   26   29  73  76   79" 

export vname_PRESmwl="Max Wind Level Pressure [hPa] "
export scale_PRESmwl=0.01
export clevs0_PRESmwl="  10  50 100 120  140 160 180 200 220 240 260 280 300"          
export color0_PRESmwl="49  48  47  46  45  44  43  22  23  24   25  26  27  28"           
export clevs_PRESmwl="  -30 -20   -15 -10   -5   -2    2    5   10  15  20 30" 
export color_PRESmwl="49   46   42   39    36   32      0     22   26   29  73  76   79" 

export vname_PREStrp="Tropopause Pressure [hPa] "
export scale_PREStrp=0.01
export clevs0_PREStrp=" 100 120  140 160 180 200 220 240 260 280 300"     
export color0_PREStrp="49  47  46  45  44  43  22   24  25  26  27  28"           
export clevs_PREStrp="  -20   -15 -10   -5   -2   -1       1     2    5   10  15  20" 
export color_PREStrp="49   46   42   39    36   32      0     22   26   29  73  76   79" 

export vname_PRMSLmsl="Mean-Sea Level Pressure Reduced to MSL [hPa] "
export scale_PRMSLmsl=0.01
export clevs0_PRMSLmsl=" 960 965 970 975 980 985 990 995 1000 1005 1010 1015 1020 "                         
export color0_PRMSLmsl="49  48 47  46  45  44   43  33 34   35   37   21    23   25"         
export clevs_PRMSLmsl="  -10   -5  -4    -3   -2   -1       1     2    3   4   5   10" 
export color_PRMSLmsl="49   46   42   39    36   32      0     22   26   29  73  76   79" 

export vname_PVORT320K="320K Level Potential Vorticity [1.0E-6*km^2/kg/s] "
export scale_PVORT320K=1000000
export clevs0_PVORT320K="  -10   -5   -4   -3    -2    -1    1      2    3    4   5   10" 
export color0_PVORT320K="49   46   42   39    36   32      0     22   26   29  73  76   79" 
export clevs_PVORT320K="  -5   -4   -3    -2    -1   -0.5   0.5   1      2   3   4   5 " 
export color_PVORT320K="49   46   42   39    36   32      0     22   26   29  73  76   79" 

export vname_PWAT30_0mb="30-0mb Above Ground Precipitable Water [kg/m^2] "
export scale_PWAT30_0mb=1
export clevs0_PWAT30_0mb=" 2  4  6  8  10  12  14  16  18  20"  
export color0_PWAT30_0mb="0  32  34  36  38   42   43    45  47   48   49"           
export clevs_PWAT30_0mb="  -5   -4   -3    -2    -1    0.5    0.5   1    2    3  4   5 "   
export color_PWAT30_0mb="49   46   42   39    36   32      0     22   26   29  73  76   79" 

export vname_PWATclm="Atmos Column Precipitable Water [kg/m^2] "
export scale_PWATclm=1
export clevs0_PWATclm=" 5   10  15  20  25   30   35   40   45   50 "  
export color0_PWATclm="0  32  34  36  38   42   43    45  47   48   49"           
export clevs_PWATclm="  -10   -5   -4   -3    -2    -1    1      2    3    4   5   10"
export color_PWATclm="49   46   42   39    36   32      0     22   26   29  73  76   79" 

export vname_RH2m="2m Above Ground Relative Humidity [%] "
export scale_RH2m=1
export clevs0_RH2m=" 10  20  30  40   50   60   70   80   90 100"  
export color0_RH2m="0  32  34  36  38   42   43    45  47   48   49"           
export clevs_RH2m="  -30   -20   -15 -10   -5   -2      2    5    10   15  20   30"
export color_RH2m="49   46   42   39    36   32      0     22   26   29  73  76   79" 

export vname_RHsig995="Sigma=.995 Relative Humidity [%] "
export scale_RHsig995=1
export clevs0_RHsig995=" 10  20  30  40   50   60   70   80   90 100"
export color0_RHsig995="0  32  34  36  38   42   43    45  47   48   49"
export clevs_RHsig995="  -30   -20   -15 -10   -5   -2      2    5    10   15  20   30"
export color_RHsig995="49   46   42   39    36   32      0     22   26   29  73  76   79" 

export vname_RHsg33_100="Sigma=0.33-1 Layer Relative Humidity [%] "
export scale_RHsg33_100=1
export clevs0_RHsg33_100=" 10  20  30  40   50   60   70   80   90 100"
export color0_RHsg33_100="0  32  34  36  38   42   43    45  47   48   49"
export clevs_RHsg33_100="    -1 -0.5 -0.1 -0.05 -0.01 -0.001 0.001 0.01 0.05 0.1 0.5 1" 
export color_RHsg33_100="49   46   42   39    36   32      0     22   26   29  73  76   79" 

export vname_RHsg44_72="Sigma=0.44-0.72 Layer Relative Humidity [%] "
export scale_RHsg44_72=1
export clevs0_RHsg44_72=" 10  20  30  40   50   60   70   80   90 100"
export color0_RHsg44_72="0  32  34  36  38   42   43    45  47   48   49"
export clevs_RHsg44_72="  -30   -20   -15 -10   -5   -2      2    5    10   15  20   30"
export color_RHsg44_72="49   46   42   39    36   32      0     22   26   29  73  76   79" 

export vname_RHsg44_100="Sigma=0.44-1 Layer Relative Humidity [%] "
export scale_RHsg44_100=1
export clevs0_RHsg44_100=" 10  20  30  40   50   60   70   80   90 100"
export color0_RHsg44_100="0  32  34  36  38   42   43    45  47   48   49"
export clevs_RHsg44_100="  -30   -20   -15 -10   -5   -2      2    5    10   15  20   30"
export color_RHsg44_100="49   46   42   39    36   32      0     22   26   29  73  76   79" 

export vname_RHsg72_94="Sigma=0.72-0.94 Layer Relative Humidity [%] "
export scale_RHsg72_94=1
export clevs0_RHsg72_94=" 10  20  30  40   50   60   70   80   90 100"
export color0_RHsg72_94="0  32  34  36  38   42   43    45  47   48   49"
export clevs_RHsg72_94="  -30   -20   -15 -10   -5   -2      2    5    10   15  20   30"
export color_RHsg72_94="49   46   42   39    36   32      0     22   26   29  73  76   79" 

export vname_RH30_0mb="30-0mb Above Ground Relative Humidity [%] "
export scale_RH30_0mb=1
export clevs0_RH30_0mb=" 10  20  30  40   50   60   70   80   90 100"
export color0_RH30_0mb="0  32  34  36  38   42   43    45  47   48   49"
export clevs_RH30_0mb="  -30   -20   -15 -10   -5   -2      2    5    10   15  20   30"
export color_RH30_0mb="49   46   42   39    36   32      0     22   26   29  73  76   79" 

export vname_RH60_30mb="60-30mb Above Ground Relative Humidity [%] "
export scale_RH60_30mb=1
export clevs0_RH60_30mb=" 10  20  30  40   50   60   70   80   90 100"
export color0_RH60_30mb="0  32  34  36  38   42   43    45  47   48   49"
export clevs_RH60_30mb="  -30   -20   -15 -10   -5   -2      2    5    10   15  20   30"
export color_RH60_30mb="49   46   42   39    36   32      0     22   26   29  73  76   79" 

export vname_RH90_60mb="90-60mb Above Ground Relative Humidity [%] "
export scale_RH90_60mb=1
export clevs0_RH90_60mb=" 10  20  30  40   50   60   70   80   90 100"
export color0_RH90_60mb="0  32  34  36  38   42   43    45  47   48   49"
export clevs_RH90_60mb="  -30   -20   -15 -10   -5   -2      2    5    10   15  20   30"
export color_RH90_60mb="49   46   42   39    36   32      0     22   26   29  73  76   79" 

export vname_RH120_90mb="120-90mb Above Ground Relative Humidity [%] "
export scale_RH120_90mb=1
export clevs_RH120_90mb="  -30   -20   -15 -10   -5   -2      2    5    10   15  20   30"
export color_RH120_90mb="49   46   42   39    36   32      0     22   26   29  73  76   79" 

export vname_RH150_120mb="150-120mb Above Ground Relative Humidity [%] "
export scale_RH150_120mb=1
export clevs0_RH150_120mb=" 10  20  30  40   50   60   70   80   90 100"
export color0_RH150_120mb="0  32  34  36  38   42   43    45  47   48   49"
export clevs_RH150_120mb="  -30   -20   -15 -10   -5   -2      2    5    10   15  20   30"
export color_RH150_120mb="49   46   42   39    36   32      0     22   26   29  73  76   79" 

export vname_RH180_150mb="180-150mb Above Ground Relative Humidity [%] "
export scale_RH180_150mb=1
export clevs0_RH180_150mb=" 10  20  30  40   50   60   70   80   90 100"
export color0_RH180_150mb="0  32  34  36  38   42   43    45  47   48   49"
export clevs_RH180_150mb="  -30   -20   -15 -10   -5   -2      2    5    10   15  20   30"
export color_RH180_150mb="49   46   42   39    36   32      0     22   26   29  73  76   79" 

export vname_RHclm="Atmos Column Relative Humidity [%] "
export scale_RHclm=1
export clevs0_RHclm=" 10  20  30  40   50   60   70   80   90 100"
export color0_RHclm="0  32  34  36  38   42   43    45  47   48   49"
export clevs_RHclm="  -30   -20   -15 -10   -5   -2      2    5    10   15  20   30"
export color_RHclm="49   46   42   39    36   32      0     22   26   29  73  76   79" 

export vname_RHhtfl="Highest Trop Freezing Level Relative Humidity [%] "
export scale_RHhtfl=1
export clevs0_RHhtfl=" 10  20  30  40   50   60   70   80   90 100"
export color0_RHhtfl="0  32  34  36  38   42   43    45  47   48   49"
export clevs_RHhtfl="  -30   -20   -15 -10   -5   -2      2    5    10   15  20   30"
export color_RHhtfl="49   46   42   39    36   32      0     22   26   29  73  76   79" 

export vname_RH0deg="0C Isotherm Level Relative Humidity [%] "
export scale_RH0deg=1
export clevs0_RH0deg=" 10  20  30  40   50   60   70   80   90 100"
export color0_RH0deg="0  32  34  36  38   42   43    45  47   48   49"
export clevs_RH0deg="  -30   -20   -15 -10   -5   -2      2    5    10   15  20   30"
export color_RH0deg="49   46   42   39    36   32      0     22   26   29  73  76   79" 

export vname_SFCRsfc="Surface Roughness [m] "
export scale_SFCRsfc=1
export clevs0_SFCRsfc=" 0.1  0.2  0.4 0.6  0.8  1  1.5  2  2.5  3"                         
export color0_SFCRsfc="0  32    34   36  37   39 42   44  46  47  49"                      
export clevs_SFCRsfc="  -1  -0.8 -0.6  -0.4 -0.2    -0.1   0.1 0.2    0.4  0.6  0.8 1.0"           
export color_SFCRsfc="49   46   42   39    36    32      0     22   26   29  73  76   79" 

export vname_SHTFLsfc="Surface Sensible Heat Flux [W/m^2] "
export scale_SHTFLsfc=1
export clevs0_SHTFLsfc="  5 10  20  40  60  80  100 120 140 160 180 200"         
export color0_SHTFLsfc="0  32 34  36  38  42  44  46  48  22  24  26  28"                 
export clevs_SHTFLsfc="  -30  -20  -15  -10  -5    -2      2     5    10   15  20  30"
export color_SHTFLsfc="49   46   42   39    36   32      0     22   26   29  73  76   79" 

export vname_SNODsfc="Surface Snow Depth [m] "
export scale_SNODsfc=1
export clevs0_SNODsfc="  0.1  0.2  0.4 0.6  0.8  1.0 1.2  1.4  1.6 1.8 2"                         
export color0_SNODsfc=" 0   32   33   34  36  37   39   42   44  46  47  49"                      
export clevs_SNODsfc="  -0.8 -0.6  -0.4 -0.2    -0.1 -0.01 0.01  0.1 0.2    0.4  0.6  0.8 "           
export color_SNODsfc="49   46   42   39    36    32      0     22   26   29  73  76   79" 

export vname_SOILL0_10cm="0-10cm Underground Liquid Volumetric Soil Moisture (non-frozen)[Fraction] "
export scale_SOILL0_10cm=100
export clevs0_SOILL0_10cm=" 10  20  30  40   50   60   70   80   90 100"
export color0_SOILL0_10cm="0  32  34  36  38   42   43    45  47   48   49"
export clevs_SOILL0_10cm="  -20   -15 -10   -5   -3     -1     1     3    5   10  15  20 "                          
export color_SOILL0_10cm="49   46   42   39    36   32      0     22   26   29  73  76   79" 

export vname_SOILL10_40cm="10-40cm Underground Liquid Volumetric Soil Moisture (non-frozen)[Fraction] "
export scale_SOILL10_40cm=100
export clevs0_SOILL10_40cm=" 10  20  30  40   50   60   70   80   90 100"
export color0_SOILL10_40cm="0  32  34  36  38   42   43    45  47   48   49"
export clevs_SOILL10_40cm="  -20   -15 -10   -5   -3     -1     1     3    5   10  15  20 "
export color_SOILL10_40cm="49   46   42   39    36   32      0     22   26   29  73  76   79" 

export vname_SOILL40_100cm="40-100cm Underground Liquid Volumetric Soil Moisture (non-frozen)[Fraction] "
export scale_SOILL40_100cm=100
export clevs0_SOILL40_100cm=" 10  20  30  40   50   60   70   80   90 100"
export color0_SOILL40_100cm="0  32  34  36  38   42   43    45  47   48   49"
export clevs_SOILL40_100cm="  -20   -15 -10   -5   -3     -1     1     3    5   10  15  20 "
export color_SOILL40_100cm="49   46   42   39    36   32      0     22   26   29  73  76   79" 

export vname_SOILL100_200cm="100-200cm Underground Liquid Volumetric Soil Moisture (non-frozen)[Fraction] "
export scale_SOILL100_200cm=100
export clevs0_SOILL100_200cm=" 10  20  30  40   50   60   70   80   90 100"
export color0_SOILL100_200cm="0  32  34  36  38   42   43    45  47   48   49"
export clevs_SOILL100_200cm="  -20   -15 -10   -5   -3     -1     1     3    5   10  15  20 "
export color_SOILL100_200cm="49   46   42   39    36   32      0     22   26   29  73  76   79" 

export vname_SOILW0_10cm="0-10cm Underground Volumetric Soil Moisture [fraction] "
export scale_SOILW0_10cm=100
export clevs0_SOILW0_10cm=" 10  20  30  40   50   60   70   80   90 100"
export color0_SOILW0_10cm="0  32  34  36  38   42   43    45  47   48   49"
export clevs_SOILW0_10cm="  -20   -15 -10   -5   -3     -1     1     3    5   10  15  20 "
export color_SOILW0_10cm="49   46   42   39    36   32      0     22   26   29  73  76   79" 

export vname_SOILW10_40cm="10-40cm Underground Volumetric Soil Moisture [fraction] "
export scale_SOILW10_40cm=100
export clevs0_SOILW10_40cm=" 10  20  30  40   50   60   70   80   90 100"
export color0_SOILW10_40cm="0  32  34  36  38   42   43    45  47   48   49"
export clevs_SOILW10_40cm="  -20   -15 -10   -5   -3     -1     1     3    5   10  15  20 "
export color_SOILW10_40cm="49   46   42   39    36   32      0     22   26   29  73  76   79" 

export vname_SOILW40_100cm="40-100cm Underground Volumetric Soil Moisture [fraction] "
export scale_SOILW40_100cm=100
export clevs0_SOILW40_100cm=" 10  20  30  40   50   60   70   80   90 100"
export color0_SOILW40_100cm="0  32  34  36  38   42   43    45  47   48   49"
export clevs_SOILW40_100cm="  -20   -15 -10   -5   -3     -1     1     3    5   10  15  20 "
export color_SOILW40_100cm="49   46   42   39    36   32      0     22   26   29  73  76   79" 

export vname_SOILW100_200cm="100-200cm Underground Volumetric Soil Moisture [fraction] "
export scale_SOILW100_200cm=100
export clevs0_SOILW100_200cm=" 10  20  30  40   50   60   70   80   90 100"
export color0_SOILW100_200cm="0  32  34  36  38   42   43    45  47   48   49"
export clevs_SOILW100_200cm="  -20   -15 -10   -5   -3     -1     1     3    5   10  15  20 "
export color_SOILW100_200cm="49   46   42   39    36   32      0     22   26   29  73  76   79" 

export vname_SPFH80m="80m Above Ground Specific Humidity [g/kg] "
export scale_SPFH80m=1000
export clevs0_SPFH80m=" 1   2   4   6   8    10   12   14    16  18"
export color0_SPFH80m="0  32  34  36  38   42   43    45  47   48   49"
export clevs_SPFH80m="   -3   -2   -1  -0.6  -0.3 -0.1  0.1   0.3  0.6   1     2  3 "   
export color_SPFH80m="49   46   42   39    36   32     0    22   26   29  73  76   79" 

export vname_SPFH2m="2m Above Ground Specific Humidity [g/kg] "
export scale_SPFH2m=1000
export clevs0_SPFH2m=" 1   2   4   6   8    10   12   14    16  18"
export color0_SPFH2m="0  32  34  36  38   42   43    45  47   48   49"
export clevs_SPFH2m="   -3   -2   -1  -0.6  -0.3 -0.1  0.1   0.3  0.6   1     2  3 "  
export color_SPFH2m="49   46   42   39    36   32      0     22   26   29  73  76   79" 

export vname_SPFH30_0mb="30-0mb Above Ground Specific Humidity [g/kg] "
export scale_SPFH30_0mb=1000
export clevs0_SPFH30_0mb=" 1   2   4   6   8    10   12   14    16  18"
export color0_SPFH30_0mb="0  32  34  36  38   42   43    45  47   48   49"
export clevs_SPFH30_0mb="   -3   -2   -1  -0.6  -0.3 -0.1  0.1   0.3  0.6   1     2  3 "  
export color_SPFH30_0mb="49   46   42   39    36   32      0     22   26   29  73  76   79" 

export vname_SPFH60_30mb="60-30mb Above Ground Specific Humidity [g/kg] "
export scale_SPFH60_30mb=1000
export clevs0_SPFH60_30mb=" 1   2   4   6   8    10   12   14    16  18"
export color0_SPFH60_30mb="0  32  34  36  38   42   43    45  47   48   49"
export clevs_SPFH60_30mb="   -3   -2   -1  -0.6  -0.3 -0.1  0.1   0.3  0.6   1     2  3 "  
export color_SPFH60_30mb="49   46   42   39    36   32      0     22   26   29  73  76   79" 

export vname_SPFH90_60mb="90-60mb Above Ground Specific Humidity [g/kg] "
export scale_SPFH90_60mb=1000
export clevs0_SPFH90_60mb=" 1   2   4   6   8    10   12   14    16  18"
export color0_SPFH90_60mb="0  32  34  36  38   42   43    45  47   48   49"
export clevs_SPFH90_60mb="   -3   -2   -1  -0.6  -0.3 -0.1  0.1   0.3  0.6   1     2  3 "  
export color_SPFH90_60mb="49   46   42   39    36   32      0     22   26   29  73  76   79" 

export vname_SPFH120_90mb="120-90mb Above Ground Specific Humidity [g/kg] "
export scale_SPFH120_90mb=1000
export clevs0_SPFH120_90mb=" 1   2   4   6   8    10   12   14    16  18"
export color0_SPFH120_90mb="0  32  34  36  38   42   43    45  47   48   49"
export clevs_SPFH120_90mb="   -3   -2   -1  -0.6  -0.3 -0.1  0.1   0.3  0.6   1     2  3 "  
export color_SPFH120_90mb="49   46   42   39    36   32      0     22   26   29  73  76   79" 

export vname_SPFH150_120mb="150-120mb Above Ground Specific Humidity [g/kg] "
export scale_SPFH150_120mb=1000
export clevs0_SPFH150_120mb=" 1   2   4   6   8    10   12   14    16  18"
export color0_SPFH150_120mb="0  32  34  36  38   42   43    45  47   48   49"
export clevs_SPFH150_120mb="   -3   -2   -1  -0.6  -0.3 -0.1  0.1   0.3  0.6   1     2  3 "  
export color_SPFH150_120mb="49   46   42   39    36   32      0     22   26   29  73  76   79" 

export vname_SPFH180_150mb="180-150mb Above Ground Specific Humidity [g/kg] "
export scale_SPFH180_150mb=1000
export clevs0_SPFH180_150mb=" 1   2   4   6   8    10   12   14    16  18"
export color0_SPFH180_150mb="0  32  34  36  38   42   43    45  47   48   49"
export clevs_SPFH180_150mb="   -3   -2   -1  -0.6  -0.3 -0.1  0.1   0.3  0.6   1     2  3 "  
export color_SPFH180_150mb="49   46   42   39    36   32      0     22   26   29  73  76   79" 

export vname_SUNSDsfc="Surface Sunshine Duration [hour] "
export scale_SUNSDsfc=0.00027778
export clevs0_SUNSDsfc=" 0.01 0.05 0.1 0.5   1   2   3    4    5   6"
export color0_SUNSDsfc="0   32   34  36   38   42  43   45  47   48   49"
export clevs_SUNSDsfc="  -2   -1  -0.5  -0.1  -0.01  -0.001 0.001 0.01  0.1   0.5  1     2  "
export colorSUNSDsfc="49   46   42   39     36     32      0     22   26   29  73  76   79" 

export vname_TCDC475mb="475mb Total Cloud Cover [%] "
export scale_TCDC475mb=1
export clevs0_TCDC475mb="  0  10  20  30  40  50   60  80 100"
export color0_TCDC475mb="0  41  43  45  47  49  21   23  25  27"   
export clevs_TCDC475mb="  -30  -20  -15  -10  -5    -2      2     5    10   15  20  30"                          
export color_TCDC475mb="49   46   42   39    36   32      0     22   26   29  73  76   79" 

export vname_TCDCclm="Atmos Column Total Cloud Cover [%] "
export scale_TCDCclm=1
export clevs0_TCDCclm="  0  10  20  30  40  50   60  80 100"
export color0_TCDCclm="0  41  43  45  47  49  21   23  25  27"   
export clevs_TCDCclm="  -30  -20  -15  -10  -5    -2      2     5    10   15  20  30"
export color_TCDCclm="49   46   42   39    36   32      0     22   26   29  73  76   79" 

export vname_TCDCbcl="Boundary Layer Total Cloud cover [%] "
export scale_TCDCbcl=1
export clevs0_TCDCbcl="  0  10  20  30  40  50   60  80 100"
export color0_TCDCbcl="0  41  43  45  47  49  21   23  25  27"   
export clevs_TCDCbcl="  -30  -20  -15  -10  -5    -2      2     5    10   15  20  30"
export color_TCDCbcl="49   46   42   39    36   32      0     22   26   29  73  76   79" 

export vname_TCDClcl="Low Level Total Cloud Cover [%] "
export scale_TCDClcl=1
export clevs0_TCDClcl="  0  10  20  30  40  50   60  80 100"
export color0_TCDClcl="0  41  43  45  47  49  21   23  25  27"   
export clevs_TCDClcl="  -30  -20  -15  -10  -5    -2      2     5    10   15  20  30"
export color_TCDClcl="49   46   42   39    36   32      0     22   26   29  73  76   79" 

export vname_TCDCmcl="Mid-Level Total Cloud Cover [%] "
export scale_TCDCmcl=1
export clevs0_TCDCmcl="  0  10  20  30  40  50   60  80 100"
export color0_TCDCmcl="0  41  43  45  47  49  21   23  25  27"   
export clevs_TCDCmcl="  -30  -20  -15  -10  -5    -2      2     5    10   15  20  30"
export color_TCDCmcl="49   46   42   39    36   32      0     22   26   29  73  76   79" 

export vname_TCDChcl="High Level Total Cloud Cover [%] "
export scale_TCDChcl=1
export clevs0_TCDChcl="  0  10  20  30  40  50   60  80 100"
export color0_TCDChcl="0  41  43  45  47  49  21   23  25  27"   
export clevs_TCDChcl="  -30  -20  -15  -10  -5    -2      2     5    10   15  20  30"
export color_TCDChcl="49   46   42   39    36   32      0     22   26   29  73  76   79" 

export vname_TCDCcvl="Convective Total Cloud cover [%] "
export scale_TCDCcvl=1
export clevs0_TCDCcvl="  0  10  20  30  40  50   60  80 100"
export color0_TCDCcvl="0  41  43  45  47  49  21   23  25  27"   
export clevs_TCDCcvl="  -30  -20  -15  -10  -5    -2      2     5    10   15  20  30"
export color_TCDCcvl="49   46   42   39    36   32      0     22   26   29  73  76   79" 

export vname_TMAX2m="2m Above Ground Maximum Temperature [K] "
export scale_TMAX2m=1
export clevs0_TMAX2m=" 240 245  250 255 260 265 270 275 280 285 290 295 300"         
export color0_TMAX2m="49 48  47   46  45  44   43  22  23  24  25  26  27  28"           
export clevs_TMAX2m="  -5   -3    -2  -1    -0.5 -0.1    0.1    0.5  1   2   3   5" 
export color_TMAX2m="49   46   42   39    36   32      0     22   26   29  73  76   79" 

export vname_TMIN2m="2m Above Ground Minimum Temperature [K] "
export scale_TMIN2m=1
export clevs0_TMIN2m=" 240 245  250 255 260 265 270 275 280 285 290 295 300"         
export color0_TMIN2m="49 48  47   46  45  44   43  22  23  24  25  26  27  28"           
export clevs_TMIN2m="  -5   -3    -2  -1    -0.5 -0.1    0.1    0.5  1   2   3   5" 
export color_TMIN2m="49   46   42   39    36   32      0     22   26   29  73  76   79" 

export vname_TMPsfc="Surface Skin Temperature [K] "
export scale_TMPsfc=1
export clevs0_TMPsfc=" 240 245  250 255 260 265 270 275 280 285 290 295 300"         
export color0_TMPsfc="49 48  47   46  45  44   43  22  23  24  25  26  27  28"           
export clevs_TMPsfc="  -5   -3    -2  -1    -0.5 -0.1    0.1    0.5  1   2   3   5" 
export color_TMPsfc="49   46   42   39    36   32      0     22   26   29  73  76   79" 

export vname_TMP4572m="4572m Above MSL Temperature [K] "
export scale_TMP4572m=1
export clevs0_TMP4572m=" 240 245  250 255 260 265 270 275 280 285 290 295 300"         
export color0_TMP4572m="49 48  47   46  45  44   43  22  23  24  25  26  27  28"           
export clevs_TMP4572m="  -5   -3    -2  -1    -0.5 -0.1    0.1    0.5  1   2   3   5" 
export color_TMP4572m="49   46   42   39    36   32      0     22   26   29  73  76   79" 

export vname_TMP3658m="3658m Above MSL Temperature [K] "
export scale_TMP3658m=1
export clevs0_TMP3658m=" 240 245  250 255 260 265 270 275 280 285 290 295 300"         
export color0_TMP3658m="49 48  47   46  45  44   43  22  23  24  25  26  27  28"           
export clevs_TMP3658m="  -5   -3    -2  -1    -0.5 -0.1    0.1    0.5  1   2   3   5" 
export color_TMP3658m="49   46   42   39    36   32      0     22   26   29  73  76   79" 

export vname_TMP2743m="2743m Above MSL Temperature [K] "
export scale_TMP2743m=1
export clevs0_TMP2743m=" 240 245  250 255 260 265 270 275 280 285 290 295 300"         
export color0_TMP2743m="49 48  47   46  45  44   43  22  23  24  25  26  27  28"           
export clevs_TMP2743m="  -5   -3    -2  -1    -0.5 -0.1    0.1    0.5  1   2   3   5" 
export color_TMP2743m="49   46   42   39    36   32      0     22   26   29  73  76   79" 

export vname_TMP1829m="1829m Above MSL Temperature [K] "
export scale_TMP1829m=1
export clevs0_TMP1829m=" 240 245  250 255 260 265 270 275 280 285 290 295 300"         
export color0_TMP1829m="49 48  47   46  45  44   43  22  23  24  25  26  27  28"           
export clevs_TMP1829m="  -5   -3    -2  -1    -0.5 -0.1    0.1    0.5  1   2   3   5" 
export color_TMP1829m="49   46   42   39    36   32      0     22   26   29  73  76   79" 

export vname_TMP914m="914m Above MSL Temperature [K] "
export scale_TMP914m=1
export clevs0_TMP914m=" 240 245  250 255 260 265 270 275 280 285 290 295 300"         
export color0_TMP914m="49 48  47   46  45  44   43  22  23  24  25  26  27  28"           
export clevs_TMP914m="  -5   -3    -2  -1    -0.5 -0.1    0.1    0.5  1   2   3   5" 
export color_TMP914m="49   46   42   39    36   32      0     22   26   29  73  76   79" 

export vname_TMP610m="610m Above MSL Temperature [K] "
export scale_TMP610m=1
export clevs0_TMP610m=" 240 245  250 255 260 265 270 275 280 285 290 295 300"         
export color0_TMP610m="49 48  47   46  45  44   43  22  23  24  25  26  27  28"           
export clevs_TMP610m="  -5   -3    -2  -1    -0.5 -0.1    0.1    0.5  1   2   3   5" 
export color_TMP610m="49   46   42   39    36   32      0     22   26   29  73  76   79" 

export vname_TMP457m="457m Above MSL Temperature [K] "
export scale_TMP457m=1
export clevs0_TMP457m=" 240 245  250 255 260 265 270 275 280 285 290 295 300"         
export color0_TMP457m="49 48  47   46  45  44   43  22  23  24  25  26  27  28"           
export clevs_TMP457m="  -5   -3    -2  -1    -0.5 -0.1    0.1    0.5  1   2   3   5" 
export color_TMP457m="49   46   42   39    36   32      0     22   26   29  73  76   79" 

export vname_TMP305m="305m Above MSL Temperature [K] "
export scale_TMP305m=1
export clevs0_TMP305m=" 240 245  250 255 260 265 270 275 280 285 290 295 300"         
export color0_TMP305m="49 48  47   46  45  44   43  22  23  24  25  26  27  28"           
export clevs_TMP305m="  -5   -3    -2  -1    -0.5 -0.1    0.1    0.5  1   2   3   5" 
export color_TMP305m="49   46   42   39    36   32      0     22   26   29  73  76   79" 

export vname_TMP100m="100m Above Ground Temperature [K] "
export scale_TMP100m=1
export clevs0_TMP100m=" 240 245  250 255 260 265 270 275 280 285 290 295 300"         
export color0_TMP100m="49 48  47   46  45  44   43  22  23  24  25  26  27  28"           
export clevs_TMP100m="  -5   -3    -2  -1    -0.5 -0.1    0.1    0.5  1   2   3   5" 
export color_TMP100m="49   46   42   39    36   32      0     22   26   29  73  76   79" 

export vname_TMP80m="80m Above Ground Temperature [K] "
export scale_TMP80m=1
export clevs0_TMP80m=" 240 245  250 255 260 265 270 275 280 285 290 295 300"         
export color0_TMP80m="49 48  47   46  45  44   43  22  23  24  25  26  27  28"           
export clevs_TMP80m="  -5   -3    -2  -1    -0.5 -0.1    0.1    0.5  1   2   3   5" 
export color_TMP80m="49   46   42   39    36   32      0     22   26   29  73  76   79" 

export vname_TMP2m="2m Above Ground Temperature [K] "
export scale_TMP2m=1
export clevs0_TMP2m=" 240 245  250 255 260 265 270 275 280 285 290 295 300"         
export color0_TMP2m="49 48  47   46  45  44   43  22  23  24  25  26  27  28"           
export clevs_TMP2m="  -5   -3    -2  -1    -0.5 -0.1    0.1    0.5  1   2   3   5" 
export color_TMP2m="49   46   42   39    36   32      0     22   26   29  73  76   79" 

export vname_TMPsig995="Sigma=.995 Temperature [K] "
export scale_TMPsig995=1
export clevs0_TMPsig995=" 240 245  250 255 260 265 270 275 280 285 290 295 300"         
export color0_TMPsig995="49 48  47   46  45  44   43  22  23  24  25  26  27  28"           
export clevs_TMPsig995="  -5   -3    -2  -1    -0.5 -0.1    0.1    0.5  1   2   3   5" 
export color_TMPsig995="49   46   42   39    36   32      0     22   26   29  73  76   79" 

export vname_TMP320K="320K Level Temperature [K] "
export scale_TMP320K=1
export clevs0_TMP320K=" 240 245  250 255 260 265 270 275 280 285 290 295 300"         
export color0_TMP320K="49 48  47   46  45  44   43  22  23  24  25  26  27  28"           
export clevs_TMP320K="  -5   -3    -2  -1    -0.5 -0.1    0.1    0.5  1   2   3   5" 
export color_TMP320K="49   46   42   39    36   32      0     22   26   29  73  76   79" 

export vname_TMP30_0mb="30-0mb Above Ground Temperature [K] "
export scale_TMP30_0mb=1
export clevs0_TMP30_0mb=" 240 245  250 255 260 265 270 275 280 285 290 295 300"         
export color0_TMP30_0mb="49 48  47   46  45  44   43  22  23  24  25  26  27  28"           
export clevs_TMP30_0mb="  -5   -3    -2  -1    -0.5 -0.1    0.1    0.5  1   2   3   5" 
export color_TMP30_0mb="49   46   42   39    36   32      0     22   26   29  73  76   79" 

export vname_TMP60_30mb="60-30mb Above Ground Temperature [K] "
export scale_TMP60_30mb=1
export clevs0_TMP60_30mb=" 240 245  250 255 260 265 270 275 280 285 290 295 300"         
export color0_TMP60_30mb="49 48  47   46  45  44   43  22  23  24  25  26  27  28"           
export clevs_TMP60_30mb="  -5   -3    -2  -1    -0.5 -0.1    0.1    0.5  1   2   3   5" 
export color_TMP60_30mb="49   46   42   39    36   32      0     22   26   29  73  76   79" 

export vname_TMP90_60mb="90-60mb Above Ground Temperature [K] "
export scale_TMP90_60mb=1
export clevs0_TMP90_60mb=" 240 245  250 255 260 265 270 275 280 285 290 295 300"         
export color0_TMP90_60mb="49 48  47   46  45  44   43  22  23  24  25  26  27  28"           
export clevs_TMP90_60mb="  -5   -3    -2  -1    -0.5 -0.1    0.1    0.5  1   2   3   5" 
export color_TMP90_60mb="49   46   42   39    36   32      0     22   26   29  73  76   79" 

export vname_TMP120_90mb="120-90mb Above Ground Temperature [K] "
export scale_TMP120_90mb=1
export clevs0_TMP120_90mb=" 240 245  250 255 260 265 270 275 280 285 290 295 300"         
export color0_TMP120_90mb="49 48  47   46  45  44   43  22  23  24  25  26  27  28"           
export clevs_TMP120_90mb="  -5   -3    -2  -1    -0.5 -0.1    0.1    0.5  1   2   3   5" 
export color_TMP120_90mb="49   46   42   39    36   32      0     22   26   29  73  76   79" 

export vname_TMP150_120mb="150-120mb Above Ground Temperature [K] "
export scale_TMP150_120mb=1
export clevs0_TMP150_120mb=" 240 245  250 255 260 265 270 275 280 285 290 295 300"         
export color0_TMP150_120mb="49 48  47   46  45  44   43  22  23  24  25  26  27  28"           
export clevs_TMP150_120mb="  -5   -3    -2  -1    -0.5 -0.1    0.1    0.5  1   2   3   5" 
export color_TMP150_120mb="49   46   42   39    36   32      0     22   26   29  73  76   79" 

export vname_TMP180_150mb="180-150mb Above Ground Temperature [K] "
export scale_TMP180_150mb=1
export clevs0_TMP180_150mb=" 240 245  250 255 260 265 270 275 280 285 290 295 300"         
export color0_TMP180_150mb="49 48  47   46  45  44   43  22  23  24  25  26  27  28"           
export clevs_TMP180_150mb="  -5   -3    -2  -1    -0.5 -0.1    0.1    0.5  1   2   3   5" 
export color_TMP180_150mb="49   46   42   39    36   32      0     22   26   29  73  76   79" 

export vname_TMPpv500="PV=500 Units Level Temperature [K] "
export scale_TMPpv500=1
export clevs0_TMPpv500=" 240 245  250 255 260 265 270 275 280 285 290 295 300"         
export color0_TMPpv500="49 48  47   46  45  44   43  22  23  24  25  26  27  28"           
export clevs_TMPpv500="  -5   -3    -2  -1    -0.5 -0.1    0.1    0.5  1   2   3   5" 
export color_TMPpv500="49   46   42   39    36   32      0     22   26   29  73  76   79" 

export vname_TMPpv1000="PV=1000 Units Level Temperature [K] "
export scale_TMPpv1000=1
export clevs0_TMPpv1000=" 240 245  250 255 260 265 270 275 280 285 290 295 300"         
export color0_TMPpv1000="49 48  47   46  45  44   43  22  23  24  25  26  27  28"           
export clevs_TMPpv1000="  -5   -3    -2  -1    -0.5 -0.1    0.1    0.5  1   2   3   5" 
export color_TMPpv1000="49   46   42   39    36   32      0     22   26   29  73  76   79" 

export vname_TMPpv1500="PV=1500 Units Level Temperature [K] "
export scale_TMPpv1500=1
export clevs0_TMPpv1500=" 240 245  250 255 260 265 270 275 280 285 290 295 300"         
export color0_TMPpv1500="49 48  47   46  45  44   43  22  23  24  25  26  27  28"           
export clevs_TMPpv1500="  -5   -3    -2  -1    -0.5 -0.1    0.1    0.5  1   2   3   5" 
export color_TMPpv1500="49   46   42   39    36   32      0     22   26   29  73  76   79" 

export vname_TMPpv2000="PV=2000 Units Level Temperature [K] "
export scale_TMPpv2000=1
export clevs0_TMPpv2000=" 240 245  250 255 260 265 270 275 280 285 290 295 300"         
export color0_TMPpv2000="49 48  47   46  45  44   43  22  23  24  25  26  27  28"           
export clevs_TMPpv2000="  -5   -3    -2  -1    -0.5 -0.1    0.1    0.5  1   2   3   5" 
export color_TMPpv2000="49   46   42   39    36   32      0     22   26   29  73  76   79" 

export vname_TMPpvneg500="PV=-500 Units Level Temperature [K] "
export scale_TMPpvneg500=1
export clevs0_TMPpvneg500=" 240 245  250 255 260 265 270 275 280 285 290 295 300"         
export color0_TMPpvneg500="49 48  47   46  45  44   43  22  23  24  25  26  27  28"           
export clevs_TMPpvneg500="  -5   -3    -2  -1    -0.5 -0.1    0.1    0.5  1   2   3   5" 
export color_TMPpvneg500="49   46   42   39    36   32      0     22   26   29  73  76   79" 

export vname_TMPpvneg1000="PV=-1000 Units Level Temperature [K] "
export scale_TMPpvneg1000=1
export clevs0_TMPpvneg1000=" 240 245  250 255 260 265 270 275 280 285 290 295 300"         
export color0_TMPpvneg1000="49 48  47   46  45  44   43  22  23  24  25  26  27  28"           
export clevs_TMPpvneg1000="  -5   -3    -2  -1    -0.5 -0.1    0.1    0.5  1   2   3   5" 
export color_TMPpvneg1000="49   46   42   39    36   32      0     22   26   29  73  76   79" 

export vname_TMPpvneg1500="PV=-1500 Units Level Temperature [K] "
export scale_TMPpvneg1500=1
export clevs0_TMPpvneg1500=" 240 245  250 255 260 265 270 275 280 285 290 295 300"         
export color0_TMPpvneg1500="49 48  47   46  45  44   43  22  23  24  25  26  27  28"           
export clevs_TMPpvneg1500="  -5   -3    -2  -1    -0.5 -0.1    0.1    0.5  1   2   3   5" 
export color_TMPpvneg1500="49   46   42   39    36   32      0     22   26   29  73  76   79" 

export vname_TMPpvneg2000="PV=-2000 Units Level Temperature [K] "
export scale_TMPpvneg2000=1
export clevs0_TMPpvneg2000=" 240 245  250 255 260 265 270 275 280 285 290 295 300"         
export color0_TMPpvneg2000="49 48  47   46  45  44   43  22  23  24  25  26  27  28"           
export clevs_TMPpvneg2000="  -5   -3    -2  -1    -0.5 -0.1    0.1    0.5  1   2   3   5" 
export color_TMPpvneg2000="49   46   42   39    36   32      0     22   26   29  73  76   79" 

export vname_TMPlct="Low Cloud Top Temperature [K] "
export scale_TMPlct=1
export clevs0_TMPlct="  220 225 230 235 240 245 250 255 260 265  270 275 280 "       
export color0_TMPlct="49 48  47   46  45  44   43  22  23  24  25  26  27  28"           
export clevs_TMPlct="  -5   -3    -2  -1    -0.5 -0.1    0.1    0.5  1   2   3   5" 
export color_TMPlct="49   46   42   39    36   32      0     22   26   29  73  76   79" 

export vname_TMPmct="Mid-Cloud Top Temperature [K] "
export scale_TMPmct=1
export clevs0_TMPmct=" 200 205  210  215 220 225 230 235 240 245 250 255 260 "        
export color0_TMPmct="49 48  47   46  45  44   43  22  23  24  25  26  27  28"           
export clevs_TMPmct="  -5   -3    -2  -1    -0.5 -0.1    0.1    0.5  1   2   3   5" 
export color_TMPmct="49   46   42   39    36   32      0     22   26   29  73  76   79" 

export vname_TMPhct="High Cloud Top Temperature [K] "
export scale_TMPhct=1
export clevs0_TMPhct=" 180 185 190 195 200 205  210  215 220 225 230 235 240"        
export color0_TMPhct="49 48  47   46  45  44   43  22  23  24  25  26  27  28"           
export clevs_TMPhct="  -5   -3    -2  -1    -0.5 -0.1    0.1    0.5  1   2   3   5" 
export color_TMPhct="49   46   42   39    36   32      0     22   26   29  73  76   79" 

export vname_TMPmwl="Max Wind Level Temperature [K] "
export scale_TMPmwl=1
export clevs0_TMPmwl=" 160 180 185 190 195 200 205  210  215 220 225 230 235 240"    
export color0_TMPmwl="49 48  47   46  45  44   43  22  23  24  25  26  27  28    29"     
export clevs_TMPmwl="  -5   -3    -2  -1    -0.5 -0.1    0.1    0.5  1   2   3   5" 
export color_TMPmwl="49   46   42   39    36   32      0     22   26   29  73  76   79" 

export vname_TMPtrp="Tropopause Temperature [K] "
export scale_TMPtrp=1
export clevs0_TMPtrp=" 160 180 185 190 195 200 205  210  215 220 225 230 235 240"        
export color0_TMPtrp="49 48  47   46  45  44   43  22  23  24  25  26  27  28    29"           
export clevs_TMPtrp="  -5   -3    -2  -1    -0.5 -0.1    0.1    0.5  1   2   3   5" 
export color_TMPtrp="49   46   42   39    36   32      0     22   26   29  73  76   79" 

export vname_TOZNEclm="Atmos Column Total Ozone [Dobson] "
export scale_TOZNEclm=1
export clevs0_TOZNEclm=" 160 180 200 220 240 260 280 300 320 340 360 380"                  
export color0_TOZNEclm="42  44  46  48  33 35  37  39  21   23  25  27  29"                
export clevs_TOZNEclm="  -10 -5  -4  -3    -2  -1    1   2   3  4  5 10" 
export color_TOZNEclm="49   46  42  39   36   32  0   22   26 29 73 76  79" 

export vname_TSOIL0_10cm="0-10cm Underground Soil Temperature [K] "
export scale_TSOIL0_10cm=1
export clevs0_TSOIL0_10cm=" 240 245  250 255 260 265 270 275 280 285 290 295 300"         
export color0_TSOIL0_10cm="49 48  47   46  45  44   43  22  23  24  25  26  27  28"           
export clevs_TSOIL0_10cm="  -5   -3    -2  -1    -0.5 -0.1    0.1    0.5  1   2   3   5" 
export color_TSOIL0_10cm="49   46   42   39    36   32      0     22   26   29  73  76   79" 

export vname_TSOIL10_40cm="10-40cm Underground Soil Temperature [K] "
export scale_TSOIL10_40cm=1
export clevs0_TSOIL10_40cm=" 240 245  250 255 260 265 270 275 280 285 290 295 300"         
export color0_TSOIL10_40cm="49 48  47   46  45  44   43  22  23  24  25  26  27  28"           
export clevs_TSOIL10_40cm="  -5   -3    -2  -1    -0.5 -0.1    0.1    0.5  1   2   3   5" 
export color_TSOIL10_40cm="49   46   42   39    36   32      0     22   26   29  73  76   79" 

export vname_TSOIL40_100cm="40-100cm Underground Soil Temperature [K] "
export scale_TSOIL40_100cm=1
export clevs0_TSOIL40_100cm=" 240 245  250 255 260 265 270 275 280 285 290 295 300"         
export color0_TSOIL40_100cm="49 48  47   46  45  44   43  22  23  24  25  26  27  28"           
export clevs_TSOIL40_100cm="  -5   -3    -2  -1    -0.5 -0.1    0.1    0.5  1   2   3   5" 
export color_TSOIL40_100cm="49   46   42   39    36   32      0     22   26   29  73  76   79" 

export vname_TSOIL100_200cm="100-200cm Underground Soil Temperature [K] "
export scale_TSOIL100_200cm=1
export clevs0_TSOIL100_200cm=" 240 245  250 255 260 265 270 275 280 285 290 295 300"         
export color0_TSOIL100_200cm="49 48  47   46  45  44   43  22  23  24  25  26  27  28"           
export clevs_TSOIL100_200cm="  -5   -3    -2  -1    -0.5 -0.1    0.1    0.5  1   2   3   5" 
export color_TSOIL100_200cm="49   46   42   39    36   32      0     22   26   29  73  76   79" 

export vname_UGWDsfc="Surface Zonal Gravity Wave Stress [0.001*N/m^2] "
export scale_UGWDsfc=1000
export clevs0_UGWDsfc="  5  10  20  40  60  80  100 120 140 160 180"
export color0_UGWDsfc="0  41  43  45  47  49  21  23   24 25   27 29"   
export clevs_UGWDsfc="  -50 -30  -20   -10  -5    -2      2     5    10   20  30  50"
export color_UGWDsfc="49   46   42   39    36   32      0     22   26   29  73  76   79" 

export vname_UFLXsfc="Surface Zonal Momentum Flux [0.001*N/m^2] "
export scale_UFLXsfc=1000
export clevs0_UFLXsfc="  -200 -160 -120 -80 -40 -10 10 40 80 120 160 200"
export color0_UFLXsfc="49   48   46   44   43  42  0 21 23  24  26  28 29"
export clevs_UFLXsfc="  -100  -50 -30  -20   -10  -5    5    10   20  30  50 100"        
export color_UFLXsfc="49   46   42   39    36   32  0     22   26   29  73  76   79" 

export vname_UGRD4572m="4572m Above MSL Zonal Wind [m/s] "
export scale_UGRD4572m=1
export clevs0_UGRD4572m="  -50  -40  -30 -20 -10 -5 -3 -1 1  3  5  10 20 30 40 50 "
export color0_UGRD4572m="49   48   47   46  45  44 43 42 0 21 22 23 24  25 26 27 28 "
export clevs_UGRD4572m="  -10   -5   -3    -2  -1    -0.5     0.5  1   2   3   5    10" 
export color_UGRD4572m="49   46   42   39    36   32      0     22   26   29  73  76   79" 

export vname_UGRD3658m="3658m Above MSL Zonal Wind [m/s] "
export scale_UGRD3658m=1
export clevs0_UGRD3658m="  -50  -40  -30 -20 -10 -5 -3 -1 1  3  5  10 20 30 40 50 "
export color0_UGRD3658m="49   48   47   46  45  44 43 42 0 21 22 23 24  25 26 27 28 "
export clevs_UGRD3658m="  -10   -5   -3    -2  -1    -0.5     0.5  1   2   3   5    10" 
export color_UGRD3658m="49   46   42   39    36   32      0     22   26   29  73  76   79" 

export vname_UGRD2743m="2743m Above MSL Zonal Wind [m/s] "
export scale_UGRD2743m=1
export clevs0_UGRD2743m="  -50  -40  -30 -20 -10 -5 -3 -1 1  3  5  10 20 30 40 50 "
export color0_UGRD2743m="49   48   47   46  45  44 43 42 0 21 22 23 24  25 26 27 28 "
export clevs_UGRD2743m="  -10   -5   -3    -2  -1    -0.5     0.5  1   2   3   5    10" 
export color_UGRD2743m="49   46   42   39    36   32      0     22   26   29  73  76   79" 

export vname_UGRD1829m="1829m Above MSL Zonal Wind [m/s] "
export scale_UGRD1829m=1
export clevs0_UGRD1829m="  -50  -40  -30 -20 -10 -5 -3 -1 1  3  5  10 20 30 40 50 "
export color0_UGRD1829m="49   48   47   46  45  44 43 42 0 21 22 23 24  25 26 27 28 "
export clevs_UGRD1829m="  -10   -5   -3    -2  -1    -0.5     0.5  1   2   3   5    10" 
export color_UGRD1829m="49   46   42   39    36   32      0     22   26   29  73  76   79" 

export vname_UGRD914m="914m Above MSL Zonal Wind [m/s] "
export scale_UGRD914m=1
export clevs0_UGRD914m="  -50  -40  -30 -20 -10 -5 -3 -1 1  3  5  10 20 30 40 50 "
export color0_UGRD914m="49   48   47   46  45  44 43 42 0 21 22 23 24  25 26 27 28 "
export clevs_UGRD914m="  -10   -5   -3    -2  -1    -0.5     0.5  1   2   3   5    10" 
export color_UGRD914m="49   46   42   39    36   32      0     22   26   29  73  76   79" 

export vname_UGRD610m="610m Above MSL Zonal Wind [m/s] "
export scale_UGRD610m=1
export clevs0_UGRD610m="  -50  -40  -30 -20 -10 -5 -3 -1 1  3  5  10 20 30 40 50 "
export color0_UGRD610m="49   48   47   46  45  44 43 42 0 21 22 23 24  25 26 27 28 "
export clevs_UGRD610m="  -10   -5   -3    -2  -1    -0.5     0.5  1   2   3   5    10" 
export color_UGRD610m="49   46   42   39    36   32      0     22   26   29  73  76   79" 

export vname_UGRD457m="457m Above MSL Zonal Wind [m/s] "
export scale_UGRD457m=1
export clevs0_UGRD457m="  -50  -40  -30 -20 -10 -5 -3 -1 1  3  5  10 20 30 40 50 "
export color0_UGRD457m="49   48   47   46  45  44 43 42 0 21 22 23 24  25 26 27 28 "
export clevs_UGRD457m="  -10   -5   -3    -2  -1    -0.5     0.5  1   2   3   5    10" 
export color_UGRD457m="49   46   42   39    36   32      0     22   26   29  73  76   79" 

export vname_UGRD305m="305m Above MSL Zonal Wind [m/s] "
export scale_UGRD305m=1
export clevs0_UGRD305m="  -50  -40  -30 -20 -10 -5 -3 -1 1  3  5  10 20 30 40 50 "
export color0_UGRD305m="49   48   47   46  45  44 43 42 0 21 22 23 24  25 26 27 28 "
export clevs_UGRD305m="  -10   -5   -3    -2  -1    -0.5     0.5  1   2   3   5    10" 
export color_UGRD305m="49   46   42   39    36   32      0     22   26   29  73  76   79" 

export vname_UGRD100m="100m Above MSL Zonal Wind [m/s] "
export scale_UGRD100m=1
export clevs0_UGRD100m="  -50  -40  -30 -20 -10 -5 -3 -1 1  3  5  10 20 30 40 50 "
export color0_UGRD100m="49   48   47   46  45  44 43 42 0 21 22 23 24  25 26 27 28 "
export clevs_UGRD100m="  -10   -5   -3    -2  -1    -0.5     0.5  1   2   3   5    10" 
export color_UGRD100m="49   46   42   39    36   32      0     22   26   29  73  76   79" 

export vname_UGRD80m="80m Above MSL Zonal Wind [m/s] "
export scale_UGRD80m=1
export clevs0_UGRD80m="  -50  -40  -30 -20 -10 -5 -3 -1 1  3  5  10 20 30 40 50 "
export color0_UGRD80m="49   48   47   46  45  44 43 42 0 21 22 23 24  25 26 27 28 "
export clevs_UGRD80m="  -10   -5   -3    -2  -1    -0.5     0.5  1   2   3   5    10" 
export color_UGRD80m="49   46   42   39    36   32      0     22   26   29  73  76   79" 

export vname_UGRD10m="10m Above MSL Zonal Wind [m/s] "
export scale_UGRD10m=1
export clevs0_UGRD10m="  -50  -40  -30 -20 -10 -5 -3 -1 1  3  5  10 20 30 40 50 "
export color0_UGRD10m="49   48   47   46  45  44 43 42 0 21 22 23 24  25 26 27 28 "
export clevs_UGRD10m="  -10   -5   -3    -2  -1    -0.5     0.5  1   2   3   5    10" 
export color_UGRD10m="49   46   42   39    36   32      0     22   26   29  73  76   79" 

export vname_UGRDsig995="Sigma=.995 Zonal Wind [m/s] "
export scale_UGRDsig995=1
export clevs0_UGRDsig995="  -50  -40  -30 -20 -10 -5 -3 -1 1  3  5  10 20 30 40 50 "
export color0_UGRDsig995="49   48   47   46  45  44 43 42 0 21 22 23 24  25 26 27 28 "
export clevs_UGRDsig995="  -10   -5   -3    -2  -1    -0.5     0.5  1   2   3   5    10" 
export color_UGRDsig995="49   46   42   39    36   32      0     22   26   29  73  76   79" 

export vname_UGRD320K="320K Level Zonal Wind [m/s] "
export scale_UGRD320K=1
export clevs0_UGRD320K="  -50  -40  -30 -20 -10 -5 -3 -1 1  3  5  10 20 30 40 50 "
export color0_UGRD320K="49   48   47   46  45  44 43 42 0 21 22 23 24  25 26 27 28 "
export clevs_UGRD320K="  -10   -5   -3    -2  -1    -0.5     0.5  1   2   3   5    10" 
export color_UGRD320K="49   46   42   39    36   32      0     22   26   29  73  76   79" 

export vname_UGRD30_0mb="30-0mb Above Ground Zonal Wind [m/s] "
export scale_UGRD30_0mb=1
export clevs0_UGRD30_0mb="  -50  -40  -30 -20 -10 -5 -3 -1 1  3  5  10 20 30 40 50 "
export color0_UGRD30_0mb="49   48   47   46  45  44 43 42 0 21 22 23 24  25 26 27 28 "
export clevs_UGRD30_0mb="  -10   -5   -3    -2  -1    -0.5     0.5  1   2   3   5    10" 
export color_UGRD30_0mb="49   46   42   39    36   32      0     22   26   29  73  76   79" 

export vname_UGRD60_30mb="60-30mb Above Ground Zonal Wind [m/s] "
export scale_UGRD60_30mb=1
export clevs0_UGRD60_30mb="  -50  -40  -30 -20 -10 -5 -3 -1 1  3  5  10 20 30 40 50 "
export color0_UGRD60_30mb="49   48   47   46  45  44 43 42 0 21 22 23 24  25 26 27 28 "
export clevs_UGRD60_30mb="  -10   -5   -3    -2  -1    -0.5     0.5  1   2   3   5    10" 
export color_UGRD60_30mb="49   46   42   39    36   32      0     22   26   29  73  76   79" 

export vname_UGRD90_60mb="90-60mb Above Ground Zonal Wind [m/s] "
export scale_UGRD90_60mb=1
export clevs0_UGRD90_60mb="  -50  -40  -30 -20 -10 -5 -3 -1 1  3  5  10 20 30 40 50 "
export color0_UGRD90_60mb="49   48   47   46  45  44 43 42 0 21 22 23 24  25 26 27 28 "
export clevs_UGRD90_60mb="  -10   -5   -3    -2  -1    -0.5     0.5  1   2   3   5    10" 
export color_UGRD90_60mb="49   46   42   39    36   32      0     22   26   29  73  76   79" 

export vname_UGRD120_90mb="120-90mb Above Ground Zonal Wind [m/s] "
export scale_UGRD120_90mb=1
export clevs0_UGRD120_90mb="  -50  -40  -30 -20 -10 -5 -3 -1 1  3  5  10 20 30 40 50 "
export color0_UGRD120_90mb="49   48   47   46  45  44 43 42 0 21 22 23 24  25 26 27 28 "
export clevs_UGRD120_90mb="  -10   -5   -3    -2  -1    -0.5     0.5  1   2   3   5    10" 
export color_UGRD120_90mb="49   46   42   39    36   32      0     22   26   29  73  76   79" 

export vname_UGRD150_120mb="150-120mb Above Ground Zonal Wind [m/s] "
export scale_UGRD150_120mb=1
export clevs0_UGRD150_120mb="  -50  -40  -30 -20 -10 -5 -3 -1 1  3  5  10 20 30 40 50 "
export color0_UGRD150_120mb="49   48   47   46  45  44 43 42 0 21 22 23 24  25 26 27 28 "
export clevs_UGRD150_120mb="  -10   -5   -3    -2  -1    -0.5     0.5  1   2   3   5    10" 
export color_UGRD150_120mb="49   46   42   39    36   32      0     22   26   29  73  76   79" 

export vname_UGRD180_150mb="180-150mb Above Ground Zonal Wind [m/s] "
export scale_UGRD180_150mb=1
export clevs0_UGRD180_150mb="  -50  -40  -30 -20 -10 -5 -3 -1 1  3  5  10 20 30 40 50 "
export color0_UGRD180_150mb="49   48   47   46  45  44 43 42 0 21 22 23 24  25 26 27 28 "
export clevs_UGRD180_150mb="  -10   -5   -3    -2  -1    -0.5     0.5  1   2   3   5    10" 
export color_UGRD180_150mb="49   46   42   39    36   32      0     22   26   29  73  76   79" 

export vname_UGRDpv500="PV=500 Units Level Zonal Wind [m/s] "
export scale_UGRDpv500=1
export clevs0_UGRDpv500="  -50  -40  -30 -20 -10 -5 -3 -1 1  3  5  10 20 30 40 50 "
export color0_UGRDpv500="49   48   47   46  45  44 43 42 0 21 22 23 24  25 26 27 28 "
export clevs_UGRDpv500="  -10   -5   -3    -2  -1    -0.5     0.5  1   2   3   5    10" 
export color_UGRDpv500="49   46   42   39    36   32      0     22   26   29  73  76   79" 

export vname_UGRDpv1000="PV=1000 Units Level Zonal Wind [m/s] "
export scale_UGRDpv1000=1
export clevs0_UGRDpv1000="  -50  -40  -30 -20 -10 -5 -3 -1 1  3  5  10 20 30 40 50 "
export color0_UGRDpv1000="49   48   47   46  45  44 43 42 0 21 22 23 24  25 26 27 28 "
export clevs_UGRDpv1000="  -10   -5   -3    -2  -1    -0.5     0.5  1   2   3   5    10" 
export color_UGRDpv1000="49   46   42   39    36   32      0     22   26   29  73  76   79" 

export vname_UGRDpv1500="PV=1500 Units Level Zonal Wind [m/s] "
export scale_UGRDpv1500=1
export clevs0_UGRDpv1500="  -50  -40  -30 -20 -10 -5 -3 -1 1  3  5  10 20 30 40 50 "
export color0_UGRDpv1500="49   48   47   46  45  44 43 42 0 21 22 23 24  25 26 27 28 "
export clevs_UGRDpv1500="  -10   -5   -3    -2  -1    -0.5     0.5  1   2   3   5    10" 
export color_UGRDpv1500="49   46   42   39    36   32      0     22   26   29  73  76   79" 

export vname_UGRDpv2000="PV=2000 Units Level Zonal Wind [m/s] "
export scale_UGRDpv2000=1
export clevs0_UGRDpv2000="  -50  -40  -30 -20 -10 -5 -3 -1 1  3  5  10 20 30 40 50 "
export color0_UGRDpv2000="49   48   47   46  45  44 43 42 0 21 22 23 24  25 26 27 28 "
export clevs_UGRDpv2000="  -10   -5   -3    -2  -1    -0.5     0.5  1   2   3   5    10" 
export color_UGRDpv2000="49   46   42   39    36   32      0     22   26   29  73  76   79" 

export vname_UGRDpvneg500="PV=-500 Units Level Zonal Wind [m/s] "
export scale_UGRDpvneg500=1
export clevs0_UGRDneg500="  -50  -40  -30 -20 -10 -5 -3 -1 1  3  5  10 20 30 40 50 "
export color0_UGRDneg500="49   48   47   46  45  44 43 42 0 21 22 23 24  25 26 27 28 "
export clevs_UGRDpvneg500="  -10   -5   -3    -2  -1    -0.5     0.5  1   2   3   5    10" 
export color_UGRDpvneg500="49   46   42   39    36   32      0     22   26   29  73  76   79" 

export vname_UGRDpvneg1000="PV=-1000 Units Level Zonal Wind [m/s] "
export scale_UGRDpvneg1000=1
export clevs0_UGRDvneg1000="  -50  -40  -30 -20 -10 -5 -3 -1 1  3  5  10 20 30 40 50 "
export color0_UGRDvneg1000="49   48   47   46  45  44 43 42 0 21 22 23 24  25 26 27 28 "
export clevs_UGRDpvneg1000="  -10   -5   -3    -2  -1    -0.5     0.5  1   2   3   5    10" 
export color_UGRDpvneg1000="49   46   42   39    36   32      0     22   26   29  73  76   79" 

export vname_UGRDpvneg1500="PV=-1500 Units Level Zonal Wind [m/s] "
export scale_UGRDpvneg1500=1
export clevs0_UGRDneg1500="  -50  -40  -30 -20 -10 -5 -3 -1 1  3  5  10 20 30 40 50 "
export color0_UGRDneg1500="49   48   47   46  45  44 43 42 0 21 22 23 24  25 26 27 28 "
export clevs_UGRDpvneg1500="  -10   -5   -3    -2  -1    -0.5     0.5  1   2   3   5    10" 
export color_UGRDpvneg1500="49   46   42   39    36   32      0     22   26   29  73  76   79" 

export vname_UGRDpvneg2000="PV=-2000 Units Level Zonal Wind [m/s] "
export scale_UGRDpvneg2000=1
export clevs0_UGRDneg2000="  -50  -40  -30 -20 -10 -5 -3 -1 1  3  5  10 20 30 40 50 "
export color0_UGRDneg2000="49   48   47   46  45  44 43 42 0 21 22 23 24  25 26 27 28 "
export clevs_UGRDpvneg2000="  -10   -5   -3    -2  -1    -0.5     0.5  1   2   3   5    10" 
export color_UGRDpvneg2000="49   46   42   39    36   32      0     22   26   29  73  76   79" 

export vname_UGRDpbl="PBL Level Zonal Wind [m/s] "
export scale_UGRDpbl=1
export clevs0_UGRDpbl="  -50  -40  -30 -20 -10 -5 -3 -1 1  3  5  10 20 30 40 50 "
export color0_UGRDpbl="49   48   47   46  45  44 43 42 0 21 22 23 24  25 26 27 28 "
export clevs_UGRDpbl="  -10   -5   -3    -2  -1    -0.5     0.5  1   2   3   5    10" 
export color_UGRDpbl="49   46   42   39    36   32      0     22   26   29  73  76   79" 

export vname_UGRDmwl="Max Wind Level Zonal Wind [m/s] "
export scale_UGRDmwl=1
export clevs0_UGRDmwl="  -50  -40  -30 -20 -10 -5 -3 -1 1  3  5  10 20 30 40 50 "
export color0_UGRDmwl="49   48   47   46  45  44 43 42 0 21 22 23 24  25 26 27 28 "
export clevs_UGRDmwl="  -20 -10   -5   -3    -2  -1  1   2   3   5    10 20" 
export color_UGRDmwl="49   46   42   39    36   32      0     22   26   29  73  76   79" 

export vname_UGRDtrp="Tropopause Zonal Wind [m/s] "
export scale_UGRDtrp=1
export clevs0_UGRDtrp="  -50  -40  -30 -20 -10 -5 -3 -1 1  3  5  10 20 30 40 50 "
export color0_UGRDtrp="49   48   47   46  45  44 43 42 0 21 22 23 24  25 26 27 28 "
export clevs_UGRDtrp="  -10   -5   -3    -2  -1    -0.5     0.5  1   2   3   5    10" 
export color_UGRDtrp="49   46   42   39    36   32      0     22   26   29  73  76   79" 

export vname_ULWRFsfc="Surface Upward Longwave Flux [W/m^2] "
export scale_ULWRFsfc=1
export clevs0_ULWRFsfc="  5 10  50  100 150 200 250 300 350 400 450 500"
export color0_ULWRFsfc="0  32 34  36  38  42  44  46  48  22  24  26  28"
export clevs_ULWRFsfc="  -30  -20  -15  -10  -5    -2      2     5    10   15  20  30"
export color_ULWRFsfc="49   46   42   39    36   32      0     22   26   29  73  76   79" 

export vname_ULWRFtoa="Top of Atmos Upward Longwave Flux [W/m^2] "
export scale_ULWRFtoa=1
export clevs0_ULWRFtoa="  5 10  50  100 150 200 250 300 350 400 450 500"
export color0_ULWRFtoa="0  32 34  36  38  42  44  46  48  22  24  26  28"
export clevs_ULWRFtoa="  -30  -20  -15  -10  -5    -2      2     5    10   15  20  30"
export color_ULWRFtoa="49   46   42   39    36   32      0     22   26   29  73  76   79" 

export vname_USTM0_6000m="0-6000m Above Ground u-Component of Storm Motion [m/s] "
export scale_USTM0_6000m=1
export clevs0_USTM0_6000m="  -50  -40  -30 -20 -10 -5 -3 -1 1  3  5  10 20 30 40 50 "
export color0_USTM0_6000m="49   48   47   46  45  44 43 42 0 21 22 23 24  25 26 27 28 "
export clevs_USTM0_6000m="  -10   -5   -3    -2  -1    -0.5     0.5  1   2   3   5    10"
export color_USTM0_6000m="49   46   42   39    36   32      0     22   26   29  73  76   79" 

export vname_USWRFsfc="Surface Upward Shortwave Flux [W/m^2] "
export scale_USWRFsfc=1
export clevs0_USWRFsfc="  5 10  20  40  60  80  100 120 140 160 180 200 300"
export color0_USWRFsfc="0  32 34  36  38  42  44  46  48  22  23   24  26  28"
export clevs_USWRFsfc="  -30  -20  -15  -10  -5    -2      2     5    10   15  20  30"
export color_USWRFsfc="49   46   42   39    36   32      0     22   26   29  73  76   79" 

export vname_USWRFtoa="Top of Atmos Upward shortwave Flux [W/m^2] "
export scale_USWRFtoa=1
export clevs0_USWRFtoa="  5 10  20  40  60  80  100 120 140 160 180 200 300"
export color0_USWRFtoa="0  32 34  36  38  42  44  46  48  22  23   24  26  28"
export clevs_USWRFtoa="  -30  -20  -15  -10  -5    -2      2     5    10   15  20  30"
export color_USWRFtoa="49   46   42   39    36   32      0     22   26   29  73  76   79" 

export vname_VGWDsfc="Surface Meridional Gravity Wave Stress [0.001*N/m^2] "
export scale_VGWDsfc=1000
export clevs0_VGWDsfc="  5  10  20  30  40  50  60  70 80 90 100"
export color0_VGWDsfc="0  41  43  45  47  49  21  23 24 25 27 29"   
export clevs_VGWDsfc="  -50 -30  -20   -10  -5    -2      2     5    10   20  30  50"
export color_VGWDsfc="49   46   42   39    36   32      0     22   26   29  73  76   79" 

export vname_VFLXsfc="Surface Meridional Momentum Flux [0.001*N/m^2] "
export scale_VFLXsfc=1000
export clevs0_VFLXsfc="  -200 -160 -120 -80 -40 -10 10 40 80 120 160 200"
export color0_VFLXsfc="49   48   46   44   43  42  0 21 23  24  26  28 29"
export clevs_VFLXsfc="  -100  -50 -30  -20   -10  -5    5    10   20  30  50 100"
export color_VFLXsfc="49   46   42   39    36   32   0   22   26   29  73  76   79" 

export vname_VGRD4572m="4572m Above MSL Meridional Wind [m/s] "
export scale_VGRD4572m=1
export clevs0_VGRD4572m="  -50  -40  -30 -20 -10 -5 -3 -1 1  3  5  10 20 30 40 50 "
export color0_VGRD4572m="49   48   47   46  45  44 43 42 0 21 22 23 24  25 26 27 28 "
export clevs_VGRD4572m="  -5   -3    -2  -1    -0.5    -0.1  0.1   0.5  1   2   3   5"
export color_VGRD4572m="49   46   42   39    36   32      0     22   26   29  73  76   79" 

export vname_VGRD3658m="3658m Above MSL Meridional Wind [m/s] "
export scale_VGRD3658m=1
export clevs0_VGRD3658m="  -50  -40  -30 -20 -10 -5 -3 -1 1  3  5  10 20 30 40 50 "
export color0_VGRD3658m="49   48   47   46  45  44 43 42 0 21 22 23 24  25 26 27 28 "
export clevs_VGRD3658m="  -4   -3   -2   -1   -0.5   -0.1    0.1   0.5   1   2   3    4" 
export color_VGRD3658m="49   46   42   39    36   32      0     22   26   29  73  76   79" 

export vname_VGRD2743m="2743m Above MSL Meridional Wind [m/s] "
export scale_VGRD2743m=1
export clevs0_VGRD2743m="  -50  -40  -30 -20 -10 -5 -3 -1 1  3  5  10 20 30 40 50 "
export color0_VGRD2743m="49   48   47   46  45  44 43 42 0 21 22 23 24  25 26 27 28 "
export clevs_VGRD2743m="  -4   -3   -2   -1   -0.5   -0.1    0.1   0.5   1   2   3    4" 
export color_VGRD2743m="49   46   42   39    36   32      0     22   26   29  73  76   79" 

export vname_VGRD1829m="1829m Above MSL Meridional Wind [m/s] "
export scale_VGRD1829m=1
export clevs0_VGRD1829m="  -50  -40  -30 -20 -10 -5 -3 -1 1  3  5  10 20 30 40 50 "
export color0_VGRD1829m="49   48   47   46  45  44 43 42 0 21 22 23 24  25 26 27 28 "
export clevs_VGRD1829m="  -4   -3   -2   -1   -0.5   -0.1    0.1   0.5   1   2   3    4" 
export color_VGRD1829m="49   46   42   39    36   32      0     22   26   29  73  76   79" 

export vname_VGRD914m="914m Above MSL Meridional Wind [m/s] "
export scale_VGRD914m=1
export clevs0_VGRD914m="  -50  -40  -30 -20 -10 -5 -3 -1 1  3  5  10 20 30 40 50 "
export color0_VGRD914m="49   48   47   46  45  44 43 42 0 21 22 23 24  25 26 27 28 "
export clevs_VGRD914m="  -4   -3   -2   -1   -0.5   -0.1    0.1   0.5   1   2   3    4" 
export color_VGRD914m="49   46   42   39    36   32      0     22   26   29  73  76   79" 

export vname_VGRD610m="610m Above MSL Meridional Wind [m/s] "
export scale_VGRD610m=1
export clevs0_VGRD610m="  -50  -40  -30 -20 -10 -5 -3 -1 1  3  5  10 20 30 40 50 "
export color0_VGRD610m="49   48   47   46  45  44 43 42 0 21 22 23 24  25 26 27 28 "
export clevs_VGRD610m="  -4   -3   -2   -1   -0.5   -0.1    0.1   0.5   1   2   3    4" 
export color_VGRD610m="49   46   42   39    36   32      0     22   26   29  73  76   79" 

export vname_VGRD457m="457m Above MSL Meridional Wind [m/s] "
export scale_VGRD457m=1
export clevs0_VGRD457m="  -50  -40  -30 -20 -10 -5 -3 -1 1  3  5  10 20 30 40 50 "
export color0_VGRD457m="49   48   47   46  45  44 43 42 0 21 22 23 24  25 26 27 28 "
export clevs_VGRD457m="  -4   -3   -2   -1   -0.5   -0.1    0.1   0.5   1   2   3    4" 
export color_VGRD457m="49   46   42   39    36   32      0     22   26   29  73  76   79" 

export vname_VGRD305m="305m Above MSL Meridional Wind [m/s] "
export scale_VGRD305m=1
export clevs0_VGRD305m="  -50  -40  -30 -20 -10 -5 -3 -1 1  3  5  10 20 30 40 50 "
export color0_VGRD305m="49   48   47   46  45  44 43 42 0 21 22 23 24  25 26 27 28 "
export clevs_VGRD305m="  -4   -3   -2   -1   -0.5   -0.1    0.1   0.5   1   2   3    4" 
export color_VGRD305m="49   46   42   39    36   32      0     22   26   29  73  76   79" 

export vname_VGRD100m="100m Above Ground Meridional Wind [m/s] "
export scale_VGRD100m=1
export clevs0_VGRD100m="  -50  -40  -30 -20 -10 -5 -3 -1 1  3  5  10 20 30 40 50 "
export color0_VGRD100m="49   48   47   46  45  44 43 42 0 21 22 23 24  25 26 27 28 "
export clevs_VGRD100m="  -4   -3   -2   -1   -0.5   -0.1    0.1   0.5   1   2   3    4" 
export color_VGRD100m="49   46   42   39    36   32      0     22   26   29  73  76   79" 

export vname_VGRD80m="80m Above Ground Meridional Wind [m/s] "
export scale_VGRD80m=1
export clevs0_VGRD80m="  -50  -40  -30 -20 -10 -5 -3 -1 1  3  5  10 20 30 40 50 "
export color0_VGRD80m="49   48   47   46  45  44 43 42 0 21 22 23 24  25 26 27 28 "
export clevs_VGRD80m="  -4   -3   -2   -1   -0.5   -0.1    0.1   0.5   1   2   3    4" 
export color_VGRD80m="49   46   42   39    36   32      0     22   26   29  73  76   79" 

export vname_VGRD10m="10m Above Ground Meridional Wind [m/s] "
export scale_VGRD10m=1
export clevs0_VGRD10m="  -50  -40  -30 -20 -10 -5 -3 -1 1  3  5  10 20 30 40 50 "
export color0_VGRD10m="49   48   47   46  45  44 43 42 0 21 22 23 24  25 26 27 28 "
export clevs_VGRD10m="  -4   -3   -2   -1   -0.5   -0.1    0.1   0.5   1   2   3    4" 
export color_VGRD10m="49   46   42   39    36   32      0     22   26   29  73  76   79" 

export vname_VGRDsig995="Sigma=.995 Meridional Wind [m/s] "
export scale_VGRDsig995=1
export clevs0_VGRDsig995="  -50  -40  -30 -20 -10 -5 -3 -1 1  3  5  10 20 30 40 50 "
export color0_VGRDsig995="49   48   47   46  45  44 43 42 0 21 22 23 24  25 26 27 28 "
export clevs_VGRDsig995="  -5   -4   -3   -2   -1   -0.5     0.5   1   2   3    4 5" 
export color_VGRDsig995="49   46   42   39    36   32      0     22   26   29  73  76   79" 

export vname_VGRD320K="320K Level Meridional Wind [m/s] "
export scale_VGRD320K=1
export clevs0_VGRD320K="  -50  -40  -30 -20 -10 -5 -3 -1 1  3  5  10 20 30 40 50 "
export color0_VGRD320K="49   48   47   46  45  44 43 42 0 21 22 23 24  25 26 27 28 "
export clevs_VGRD320K="  -4   -3   -2   -1   -0.5   -0.1    0.1   0.5   1   2   3    4" 
export color_VGRD320K="49   46   42   39    36   32      0     22   26   29  73  76   79" 

export vname_VGRD30_0mb="30-0mb Above Ground Meridional Wind [m/s] "
export scale_VGRD30_0mb=1
export clevs0_VGRD30_0mb="  -50  -40  -30 -20 -10 -5 -3 -1 1  3  5  10 20 30 40 50 "
export color0_VGRD30_0mb="49   48   47   46  45  44 43 42 0 21 22 23 24  25 26 27 28 "
export clevs_VGRD30_0mb="  -4   -3   -2   -1   -0.5   -0.1    0.1   0.5   1   2   3    4" 
export color_VGRD30_0mb="49   46   42   39    36   32      0     22   26   29  73  76   79" 

export vname_VGRD60_30mb="60-30mb Above Ground Meridional Wind [m/s] "
export scale_VGRD60_30mb=1
export clevs0_VGRD60_30mb="  -50  -40  -30 -20 -10 -5 -3 -1 1  3  5  10 20 30 40 50 "
export color0_VGRD60_30mb="49   48   47   46  45  44 43 42 0 21 22 23 24  25 26 27 28 "
export clevs_VGRD60_30mb="  -4   -3   -2   -1   -0.5   -0.1    0.1   0.5   1   2   3    4" 
export color_VGRD60_30mb="49   46   42   39    36   32      0     22   26   29  73  76   79" 

export vname_VGRD90_60mb="90-60mb Above Ground Meridional Wind [m/s] "
export scale_VGRD90_60mb=1
export clevs0_VGRD90_60mb="  -50  -40  -30 -20 -10 -5 -3 -1 1  3  5  10 20 30 40 50 "
export color0_VGRD90_60mb="49   48   47   46  45  44 43 42 0 21 22 23 24  25 26 27 28 "
export clevs_VGRD90_60mb="  -4   -3   -2   -1   -0.5   -0.1    0.1   0.5   1   2   3    4" 
export color_VGRD90_60mb="49   46   42   39    36   32      0     22   26   29  73  76   79" 

export vname_VGRD120_90mb="120-90mb Above Ground Meridional Wind [m/s] "
export scale_VGRD120_90mb=1
export clevs0_VGRD120_90mb="  -50  -40  -30 -20 -10 -5 -3 -1 1  3  5  10 20 30 40 50 "
export color0_VGRD120_90mb="49   48   47   46  45  44 43 42 0 21 22 23 24  25 26 27 28 "
export clevs_VGRD120_90mb="  -4   -3   -2   -1   -0.5   -0.1    0.1   0.5   1   2   3    4" 
export color_VGRD120_90mb="49   46   42   39    36   32      0     22   26   29  73  76   79" 

export vname_VGRD150_120mb="150-120mb Above Ground Meridional Wind [m/s] "
export scale_VGRD150_120mb=1
export clevs0_VGRD150_120mb="  -50  -40  -30 -20 -10 -5 -3 -1 1  3  5  10 20 30 40 50 "
export color0_VGRD150_120mb="49   48   47   46  45  44 43 42 0 21 22 23 24  25 26 27 28 "
export clevs_VGRD150_120mb="  -4   -3   -2   -1   -0.5   -0.1    0.1   0.5   1   2   3    4" 
export color_VGRD150_120mb="49   46   42   39    36   32      0     22   26   29  73  76   79" 

export vname_VGRD180_150mb="180-150mb Above Ground Meridional Wind [m/s] "
export scale_VGRD180_150mb=1
export clevs0_VGRD180_150mb="  -50  -40  -30 -20 -10 -5 -3 -1 1  3  5  10 20 30 40 50 "
export color0_VGRD180_150mb="49   48   47   46  45  44 43 42 0 21 22 23 24  25 26 27 28 "
export clevs_VGRD180_150mb="  -4   -3   -2   -1   -0.5   -0.1    0.1   0.5   1   2   3    4" 
export color_VGRD180_150mb="49   46   42   39    36   32      0     22   26   29  73  76   79" 

export vname_VGRDpv500="PV=500 Units Level Meridional Wind [m/s] "
export scale_VGRDpv500=1
export clevs0_VGRDpv500="  -50  -40  -30 -20 -10 -5 -3 -1 1  3  5  10 20 30 40 50 "
export color0_VGRDpv500="49   48   47   46  45  44 43 42 0 21 22 23 24  25 26 27 28 "
export clevs_VGRDpv500="  -4   -3   -2   -1   -0.5   -0.1    0.1   0.5   1   2   3    4" 
export color_VGRDpv500="49   46   42   39    36   32      0     22   26   29  73  76   79" 

export vname_VGRDpv1000="PV=1000 Units Level Meridional Wind [m/s] "
export scale_VGRDpv1000=1
export clevs0_VGRDpv1000="  -50  -40  -30 -20 -10 -5 -3 -1 1  3  5  10 20 30 40 50 "
export color0_VGRDpv1000="49   48   47   46  45  44 43 42 0 21 22 23 24  25 26 27 28 "
export clevs_VGRDpv1000="  -4   -3   -2   -1   -0.5   -0.1    0.1   0.5   1   2   3    4" 
export color_VGRDpv1000="49   46   42   39    36   32      0     22   26   29  73  76   79" 

export vname_VGRDpv1500="PV=1500 Units Level Meridional Wind [m/s] "
export scale_VGRDpv1500=1
export clevs0_VGRDpv1500="  -50  -40  -30 -20 -10 -5 -3 -1 1  3  5  10 20 30 40 50 "
export color0_VGRDpv1500="49   48   47   46  45  44 43 42 0 21 22 23 24  25 26 27 28 "
export clevs_VGRDpv1500="  -4   -3   -2   -1   -0.5   -0.1    0.1   0.5   1   2   3    4" 
export color_VGRDpv1500="49   46   42   39    36   32      0     22   26   29  73  76   79" 

export vname_VGRDpv2000="PV=2000 Units Level Meridional Wind [m/s] "
export scale_VGRDpv2000=1
export clevs0_VGRDpv2000="  -50  -40  -30 -20 -10 -5 -3 -1 1  3  5  10 20 30 40 50 "
export color0_VGRDpv2000="49   48   47   46  45  44 43 42 0 21 22 23 24  25 26 27 28 "
export clevs_VGRDpv2000="  -4   -3   -2   -1   -0.5   -0.1    0.1   0.5   1   2   3    4" 
export color_VGRDpv2000="49   46   42   39    36   32      0     22   26   29  73  76   79" 

export vname_VGRDpvneg500="PV=-500 Units Level Meridional Wind [m/s] "
export scale_VGRDpvneg500=1
export clevs0_VGRDpvneg500="  -50  -40  -30 -20 -10 -5 -3 -1 1  3  5  10 20 30 40 50 "
export color0_VGRDpvneg500="49   48   47   46  45  44 43 42 0 21 22 23 24  25 26 27 28 "
export clevs_VGRDpvneg500="  -4   -3   -2   -1   -0.5   -0.1    0.1   0.5   1   2   3    4" 
export color_VGRDpvneg500="49   46   42   39    36   32      0     22   26   29  73  76   79" 

export vname_VGRDpvneg1000="PV=-1000 Units Level Meridional Wind [m/s] "
export scale_VGRDpvneg1000=1
export clevs0_VGRDpvneg1000="  -50  -40  -30 -20 -10 -5 -3 -1 1  3  5  10 20 30 40 50 "
export color0_VGRDpvneg1000="49   48   47   46  45  44 43 42 0 21 22 23 24  25 26 27 28 "
export clevs_VGRDpvneg1000="  -4   -3   -2   -1   -0.5   -0.1    0.1   0.5   1   2   3    4" 
export color_VGRDpvneg1000="49   46   42   39    36   32      0     22   26   29  73  76   79" 

export vname_VGRDpvneg1500="PV=-1500 Units Level Meridional Wind [m/s] "
export scale_VGRDpvneg1500=1
export clevs0_VGRDpvneg1500="  -50  -40  -30 -20 -10 -5 -3 -1 1  3  5  10 20 30 40 50 "
export color0_VGRDpvneg1500="49   48   47   46  45  44 43 42 0 21 22 23 24  25 26 27 28 "
export clevs_VGRDpvneg1500="  -4   -3   -2   -1   -0.5   -0.1    0.1   0.5   1   2   3    4" 
export color_VGRDpvneg1500="49   46   42   39    36   32      0     22   26   29  73  76   79" 

export vname_VGRDpvneg2000="PV=-2000 Units Level Meridional Wind [m/s] "
export scale_VGRDpvneg2000=1
export clevs0_VGRDpvneg2000="  -50  -40  -30 -20 -10 -5 -3 -1 1  3  5  10 20 30 40 50 "
export color0_VGRDpvneg2000="49   48   47   46  45  44 43 42 0 21 22 23 24  25 26 27 28 "
export clevs_VGRDpvneg2000="  -4   -3   -2   -1   -0.5   -0.1    0.1   0.5   1   2   3    4" 
export color_VGRDpvneg2000="49   46   42   39    36   32      0     22   26   29  73  76   79" 

export vname_VGRDpbl="PBL Level Meridional Wind [m/s] "
export scale_VGRDpbl=1
export clevs0_VGRDpbl="  -50  -40  -30 -20 -10 -5 -3 -1 1  3  5  10 20 30 40 50 "
export color0_VGRDpbl="49   48   47   46  45  44 43 42 0 21 22 23 24  25 26 27 28 "
export clevs_VGRDpbl="  -4   -3   -2   -1   -0.5   -0.1    0.1   0.5   1   2   3    4" 
export color_VGRDpbl="49   46   42   39    36   32      0     22   26   29  73  76   79" 

export vname_VGRDmwl="Max Wind Level Meridional Wind [m/s] "
export scale_VGRDmwl=1
export clevs0_VGRDmwl="  -50  -40  -30 -20 -10 -5 -3 -1 1  3  5  10 20 30 40 50 "
export color0_VGRDmwl="49   48   47   46  45  44 43 42 0 21 22 23 24  25 26 27 28 "
export clevs_VGRDmwl="  -20 -10   -5   -3    -2  -1  1   2   3   5    10 20"              
export color_VGRDmwl="49   46   42   39    36   32 0   22  26  29  73  76   79" 

export vname_VGRDtrp="Tropopause Meridional Wind [m/s] "
export scale_VGRDtrp=1
export clevs0_VGRDtrp="  -50  -40  -30 -20 -10 -5 -3 -1 1  3  5  10 20 30 40 50 "
export color0_VGRDtrp="49   48   47   46  45  44 43 42 0 21 22 23 24  25 26 27 28 "
export clevs_VGRDtrp="  -5   -4   -3   -2   -1   -0.5   0.5   1   2   3    4 5" 
export color_VGRDtrp="49   46   42   39    36   32      0     22   26   29  73  76   79" 

export vname_VISsfc="Surface Visibility [km] "
export scale_VISsfc=0.001
export clevs0_VISsfc=" 1   2   4   6   8    10   12   14    16  18 20"
export color0_VISsfc="0  32  33  34  35  36   37   39    42  44  46  49"
export clevs_VISsfc="  -4   -3   -2   -1   -0.5   -0.1    0.1   0.5   1   2   3    4"
export color_VISsfc="49   46   42   39    36   32      0     22   26   29  73  76   79" 

export vname_VRATEpbl="PBL Level Ventilation Rate [m^2/s] "
export scale_VRATEpbl=1
export clevs0_VRATEpbl="  5000 10000 15000 20000 25000 30000 35000 40000 45000 "   
export color0_VRATEpbl="41   43    45    47    49    21    23     25   27    29"      
export clevs_VRATEpbl="  -15000 -12000 -9000 -6000 -3000 -1000 1000 3000 6000 9000 12000 15000"
export color_VRATEpbl="49     46     42     39    36    32     0    22   26   29  73    76   79" 

export vname_VSTM0_6000m="0-6000m Above Ground v-Component of Storm Motion [m/s] "
export scale_VSTM0_6000m=1
export clevs0_VSTM0_6000m="  -50  -40  -30 -20 -10 -5 -3 -1 1  3  5  10 20 30 40 50 "
export color0_VSTM0_6000m="49   48   47   46  45  44 43 42 0 21 22 23 24  25 26 27 28 "
export clevs_VSTM0_6000m="  -4   -3   -2   -1   -0.5   -0.1    0.1   0.5   1   2   3    4"
export color_VSTM0_6000m="49   46   42   39    36   32      0     22   26   29  73  76   79" 

export vname_VVELsig995="sigma=.995 Pressure Vertical Velocity [hPa/hour] "
export scale_VVELsig995=36
export clevs0_VVELsig995="  -50  -40  -30 -20 -10 -5 -3 -1 1  3  5  10 20 30 40 50 "
export color0_VVELsig995="49   48   47   46  45  44 43 42 0 21 22 23 24  25 26 27 28 "
export clevs_VVELsig995="  -10  -5  -4   -3   -2   -1    1   2   3    4  5   10"
export color_VVELsig995="49   46   42  39   36   32   0    22  26  29  73  76   79" 

export vname_VWSHpv500="PV=500 Units Level Vertical Speed Shear [1/s] "
export scale_VWSHpv500=1
export clevs0_VWSHpv500="  -50  -40  -30 -20 -10 -5 -3 -1 1  3  5  10 20 30 40 50 "
export color0_VWSHpv500="49   48   47   46  45  44 43 42 0 21 22 23 24  25 26 27 28 "
export clevs_VWSHpv500="  -4   -3   -2   -1   -0.5   -0.1    0.1   0.5   1   2   3    4"
export color_VWSHpv500="49   46   42   39    36   32      0     22   26   29  73  76   79" 

export vname_VWSHpv1000="PV=1000 Units Level Vertical Speed Shear [1/s] "
export scale_VWSHpv1000=1
export clevs0_VWSHpv1000="  -50  -40  -30 -20 -10 -5 -3 -1 1  3  5  10 20 30 40 50 "
export color0_VWSHpv1000="49   48   47   46  45  44 43 42 0 21 22 23 24  25 26 27 28 "
export clevs_VWSHpv1000="  -4   -3   -2   -1   -0.5   -0.1    0.1   0.5   1   2   3    4"
export color_VWSHpv1000="49   46   42   39    36   32      0     22   26   29  73  76   79" 

export vname_VWSHpv1500="PV=1500 Units Level Vertical Speed Shear [1/s] "
export scale_VWSHpv1500=1
export clevs0_VWSHpv1500="  -50  -40  -30 -20 -10 -5 -3 -1 1  3  5  10 20 30 40 50 "
export color0_VWSHpv1500="49   48   47   46  45  44 43 42 0 21 22 23 24  25 26 27 28 "
export clevs_VWSHpv1500="  -4   -3   -2   -1   -0.5   -0.1    0.1   0.5   1   2   3    4"
export color_VWSHpv1500="49   46   42   39    36   32      0     22   26   29  73  76   79" 

export vname_VWSHpv2000="PV=2000 Units Level Vertical Speed Shear [1/s] "
export scale_VWSHpv2000=1
export clevs0_VWSHpv2000="  -50  -40  -30 -20 -10 -5 -3 -1 1  3  5  10 20 30 40 50 "
export color0_VWSHpv2000="49   48   47   46  45  44 43 42 0 21 22 23 24  25 26 27 28 "
export clevs_VWSHpv2000="  -4   -3   -2   -1   -0.5   -0.1    0.1   0.5   1   2   3    4"
export color_VWSHpv2000="49   46   42   39    36   32      0     22   26   29  73  76   79" 

export vname_VWSHpvneg500="PV=-500 Units Level Vertical Speed Shear [1/s] "
export scale_VWSHpvneg500=1
export clevs0_VWSHpvneg500="  -50  -40  -30 -20 -10 -5 -3 -1 1  3  5  10 20 30 40 50 "
export color0_VWSHpvneg500="49   48   47   46  45  44 43 42 0 21 22 23 24  25 26 27 28 "
export clevs_VWSHpvneg500="  -4   -3   -2   -1   -0.5   -0.1    0.1   0.5   1   2   3    4"
export color_VWSHpvneg500="49   46   42   39    36   32      0     22   26   29  73  76   79" 

export vname_VWSHpvneg1000="PV=-1000 Units Level Vertical Speed Shear [1/s] "
export scale_VWSHpvneg1000=1
export clevs0_VWSHpvneg1000="  -50  -40  -30 -20 -10 -5 -3 -1 1  3  5  10 20 30 40 50 "
export color0_VWSHpvneg1000="49   48   47   46  45  44 43 42 0 21 22 23 24  25 26 27 28 "
export clevs_VWSHpvneg1000="  -4   -3   -2   -1   -0.5   -0.1    0.1   0.5   1   2   3    4"
export color_VWSHpvneg1000="49   46   42   39    36   32      0     22   26   29  73  76   79" 

export vname_VWSHpvneg1500="PV=-1500 Units Level Vertical Speed Shear [1/s] "
export scale_VWSHpvneg1500=1
export clevs0_VWSHpvneg1500="  -50  -40  -30 -20 -10 -5 -3 -1 1  3  5  10 20 30 40 50 "
export color0_VWSHpvneg1500="49   48   47   46  45  44 43 42 0 21 22 23 24  25 26 27 28 "
export clevs_VWSHpvneg1500="  -4   -3   -2   -1   -0.5   -0.1    0.1   0.5   1   2   3    4"
export color_VWSHpvneg1500="49   46   42   39    36   32      0     22   26   29  73  76   79" 

export vname_VWSHpvneg2000="PV=-2000 Units Level Vertical Speed Shear [1/s] "
export scale_VWSHpvneg2000=1
export clevs0_VWSHpvneg2000="  -50  -40  -30 -20 -10 -5 -3 -1 1  3  5  10 20 30 40 50 "
export color0_VWSHpvneg2000="49   48   47   46  45  44 43 42 0 21 22 23 24  25 26 27 28 "
export clevs_VWSHpvneg2000="  -4   -3   -2   -1   -0.5   -0.1    0.1   0.5   1   2   3    4"
export color_VWSHpvneg2000="49   46   42   39    36   32      0     22   26   29  73  76   79" 

export vname_VWSHtrp="Tropopause Vertical Speed Shear [1/hour] "
export scale_VWSHtrp=3600
export clevs0_VWSHtrp="  -100 -50  -40  -30 -20 -10 -5 -3  3  5  10 20 30 40 50 100"
export color0_VWSHtrp="49   48   47   46  45  44 43 42 0 21 22 23 24  25 26 27 28 "
export clevs_VWSHtrp="  -10 -5 -4   -3   -2   -1   1   2   3    4  5   10"
export color_VWSHtrp="49   46 42  39   36   32   0   22  26   29 73  76   79" 

export vname_WATRsfc="Surface Water Runoff [kg/m^2] "
export scale_WATRsfc=1
export clevs0_WATRsfc=" 0.001  0.01 0.02 0.04 0.06 0.08 0.1 0.2 0.4 0.6 0.8  1"                      
export color0_WATRsfc="0     32   33   34   35   36   38   42  43  45  46  47  49"             
export clevs_WATRsfc="    -1 -0.5 -0.1 -0.05 -0.01 -0.001 0.001 0.01 0.05 0.1 0.5 1" 
export color_WATRsfc="49   46   42   39    36   32      0     22   26   29  73  76   79"

export vname_WEASDsfc="Surface Accum Snow [kg/m^2] "
export scale_WEASDsfc=1
export clevs0_WEASDsfc=" 0.1    0.5   1    3    6   10   20  30   40  50  70  90"                                    
export color0_WEASDsfc="0     32   33   34   35   36   38   42  43  45  46  47  49"             
export clevs_WEASDsfc="   -8   -6   -4   -2   -1   -0.1    0.1   1     2   4    6  8"                     
export color_WEASDsfc="49   46   42   39    36   32      0     22   26   29  73  76   79" 

export vname_WILTsfc="Surface Wilting Point [fraction] "
export scale_WILTsfc=1
export clevs0_WILTsfc=" 0.001  0.01 0.02 0.04 0.06 0.08 0.1 0.2 0.4 0.6 0.8  1"                      
export color0_WILTsfc="0     32   33   34   35   36   38   42  43  45  46  47  49"             
export clevs_WILTsfc="    -1 -0.5 -0.1 -0.05 -0.01 -0.001 0.001 0.01 0.05 0.1 0.5 1" 
export color_WILTsfc="49   46   42   39    36   32      0     22   26   29  73  76   79" 

