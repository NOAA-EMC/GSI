############################################################
###### NCEP-EMC Global NWP Model VERIFICATION PACKAGE  #####
######              Fanglin Yang                       #####
######                                                 #####
######  Global Weather and Climate Modeling Branch     #####
######        Environmental Modeling Center            #####
######  National Centers for Environmental Prediction  #####
############################################################


Credit and Acknowledgments:
##    All scripts and Fortran programs except for those listed below are written
##    and maintained by Fanglin Yang.
##    Binbin Zhou and Geoff DiMego provided the script and code for grid-to-grid 
##    database computation. Jack Woollen and Suranjana Saha provided the script 
##    and code for making fit-to-obs maps. Perry Shafran and Geoff DiMego provided 
##    the Grid-to-Obs fortran programs. DaNa Carlis and Rebecca LaPorta developed 
##    the SCORECARD functionality. All third-party programs and scripts have been 
##    modified before being included in this package. 
##    Yuejian Zhu, Peter Caplan, and Bob Kistler had made significant contributions 
##    to a legacy system that was used to verify, monitor and archive GFS forecast 
##    skills before mid-2000s. A few of the Fortran source code used for precipitation 
##    verification and some of the climatology fields from the legacy system are 
##    still used in this verification package.
##    A few "NWPROD" libraries were adopted from the GFS para system Shrinivas 
##    Moorthi updated and built on Zeus for running on different platforms. A few 
##    changes made by Jim Jung  to the grid-to-grid Fortran source code were adopted 
##    to make the program compatible with Linux compilers. Shrinivas Moorthi helped 
##    with adding the script to port forecast data from different machines. Glenn 
##    White, Steve Lord and Xu Li made suggestions for creating and improving the 
##    significance test for AC dieoff curves and RMSE growth curves. Russ made a 
##    suggestion to include consensus analysis for verification. Helin Wei provided 
##    assistance for including the grid-to-obs verification. John Derber made 
##    a few suggestions to improve grid-to-obs verification, the SCORECARD and  
##    significane test graphics. Many users have provided valuable comments and 
##    suggestions that helped improving the usability and  portability of this package. 


--------------------------------------------------------------------
This package performs the following verifications for NWP forecasts:                 
--------------------------------------------------------------------
(1) AC, RMSE, BIAS etc: model forecasts are verified aganist analyses. Results
      are saved as partial sums in VSDB format; verification maps are then made
      to compare forecast verification metrics between different experiments and/or
      operational NWP models (up to 10 panels on one plot).

(2) QPF: precipitation threat skill scores over CONUS are first computed using either
      pgb or flx files as input and using either precip accumulation or precipiation
      rates as the verifiying variable; then precip threat skill score maps are made
      with Monte Carlo significance test being included.

(3) 2D MAPS: make maps of lat-lon distrubions and zonal mean vertical cross-sections
      to compare forecast fields, such as U,V,T,Q,RH,O3, T2m, Precip, etc.

(4) Fit-to-Obs: make fit-to-rawinsonde comparisions

(5) Grid-to-Obs:  verifying forecasts against surface station observations and upper-air RAOBS

(6) transfer maps and web templates to web servers for display. Example:     
    http://www.emc.ncep.noaa.gov/gmb/wx24fy/vsdb/gfs2015/
    http://www.emc.ncep.noaa.gov/gmb/STATS_vsdb/                        

 
--------------------------
Where to obtain the package 
--------------------------

(1)For NCEP WCOSS (Tide and Gyre) users:
   copy the driver  /global/save/Fanglin.Yang/VRFY/vsdb/vsdbjob_submit.sh and
     parameter setting script /global/save/Fanglin.Yang/VRFY/vsdb/setup_envs.sh,            
     make necessary changes following the instruction given in  the script.

(2)For NOAA Zeus users:
    driver /scratch2/portfolios/NCEPDEV/global/save/Fanglin.Yang/VRFY/vsdb/vsdbjob_submit.sh 
    parameter setting script /scratch2/portfolios/NCEPDEV/global/save/Fanglin.Yang/VRFY/vsdb/setup_envs.sh,

(3)For those who has access to NCEP/EMC SVN, find    
    https://svnemc.ncep.noaa.gov/projects/verif/global/tags/vsdb_v18/    

(5)For users who have no access to NCEP computers and wish to install the entire package, 
   please get the package vsdb_exp_v18.tar by ftp from  http://ftp.emc.ncep.noaa.gov/gc_wmb/wx24fy/VRFY/


--------------------------
How to install and use    
--------------------------

(1)Users who plan to run verification jobs on NCEP WCOSS machines or NOAA
    computer Zeus only need to copy the driver vsdbjob_submit.sh and parameter 
    setting script setup_envs.sh, make a few necessary changes as described within the two 
    scripts, then execute the driver.  NCEP WCOSS users who have passwordless links set up 
    between WCOSS and emcrzdm should be able to post results directly to the web server (emcrzdm). 
    Others can set up similar web displays, or may have to wait for all verification jobs to 
    finish, then pack the ./web directory and transfer it to a web server for display.

(2)Users who plan to buid the package from scratch on different computer platforms need to 
    get the tarball (vsdb_exp_v18.tar), unpack it, then check build.sh to see how to compile 
    libraries and program executables based on platform options. Please also note that the climate 
    data used for computing anomaly correlations are not included in this tarball, and additional 
    observations may be required for computing precipitation QPF stats and grid-to-obs stats and
    for comparing forecasts with obs in 2D-MAPS section. Please contact me for these datasets.

(3)Please see http://www.emc.ncep.noaa.gov/mmb/mmbpll/misc/pwdless_ssh2.shtml.html
   for instructions of setting ssh keys and passwordless links.



=============================================
Version Update Information
=============================================
Version 19 information, July 2015                              
1. Added a new tool for estimating GDAS/GFS analysis increments between siganl and sigges. 
   A modified version of ss2gg was included in ~nwprod/util/sorc/s2g for converting 
   sigma spectral coefficients to lat-lon binary files. The tool produces lat-lon and 
   zonal-mean maps of time-averaged bias and RMS of analysis increments.

++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
Version 18 information, January 2015                              
1. Stan Benjamin from ESRL/NOAA found that the averaged RMSE shown in the t-p map 
   does not always match the numbers printed in the RMS line plots.  Redefined mrms 
   in p-t plots as the average of rms time series instead of as root-mean square of 
   mean variances sampled over all spatial and temporal space. 
2. Extended precipitation verification from fixed 84 hours to any given forecast length.
   Added 2D-type maps to show ETS and BIAS scores for all forecast hours and all precip
   intensities.
3. It is uncovered that nonuniform sampling is used for computing precip skill scores
   in  precip/sorc/precip_score.f. If one experiment has missing stats and much less 
   samples than the other the computed mean ets and bias scores are not accurate.
4. Restructured 2D SfcMAP scripts.  Added vardef.sh to define contour and color for  
   all variables in 2D plots, changed all graphic format from gif to png to save disk space. 
5. Added NCEP w3nco lib to make copygb work for grid 193 (0.25-deg).

++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
Version 17 information, April 2014                              
1. Updated grid-to-obs for running it on WCOSS and ZEUS. Source code was upgraded to 
   /nwprod version, with modifications for global model application. Bufr lib was 
   updated for reading prepbufr data generated on WCOSS. Scripts were updated to 
   handle data retrieval from HPSS and transfer of graphics to web server in batch 
   queues.  WCOSS and ZEUS computing nodes have no access to HPSS and other computers.
2. In addition to ADPUPA, ANYAIR (AIRCAR and AIRCFT) is added to grid-to-obs upper
   air verification as an option. Sfc verification was updated to use 3-hourly fcst
   output for a better description of diurnal variation. 
3. NASA S4 and JIBB users reported disk I/O problems when making some of the anom
   and pres grids. Thanks to Jim Jung and Doris Pan, the scripts under map_util have 
   been modified to reduce I/O activities.
4. Added a new feature to generate scorecard using verification stats of AC, RMSE and 
   Bias.  It is developed by DaNa Carlis and Rebecca LaPorta. Setting "scorecard=Yes" 
   in setup_envs.sh turns on this feature. Different symbols are used to represent
   student-t test significanes at the 95%, 99% and 99.9% levels.  The test critical
   also varies with sample size.  It is more stringent for smaller samples.
5. Updated fit2obs to fix mismatched global means in vertical and horizontal plots.
6. Updated vsdbjob.sh to run stats generation for all cycles only once per day. This
   script is used by the GFS parallel system to compute on-the-fly verification stats.                   
7. Updated scripts under ./map_util and ./precip to reset significance-test intervals 
   to the upper and lower bounds of the map to force all hollow bars that depict 
   significance levels to be plotted.
8. Removed the old "fits" directory. It has been replaced by the more advanced 
   "fit2obs" verification tool developed by Jack Woollen and Suru Saha.
   

++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
Version 16 information, July 2013                              
1. Added the options to run on NCEP WCOSS Tide and Gyre machines.
2. Added verification of sea-level pressure to grid-to-obs 
3. Added the option to gather vsdb database from different directories/users. 
4. Use batch queues to run 2D maps, and added more variables.
5. Added the option to place  computation and data transfer on different
   nodes to allow graphics to be uploaded to servers using "transfer" queues 
   (a requirement of NCEP WCOSS). 
6. Updated the "fit-to-obs" verification to a new package developed by Jack Woollen,
   with certain contributions made by Suranjana Saha and Fanglin Yang. New features of 
   the package include a) a new web display interface makes web browsing easier; 
   b) multiple experiments/models (up to ten) can be included in one verification run. 
   In the old version only two experiments/models were allowed. c) forecast hours to be
   verified are extended from previous 48 hours to any user specified hours. 

++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
Version 15 information, January 2013                              
1. Previous versins only allow forecasts to be verified at a 24-hour output interval.
   For instance, for 00Z 01Jan2012 fcst cycle, the verification is only done at the hours 
   of 00Z01Jan2012, 00Z02Jan2012, 00Z03Jan2012, 00Z03Jan2012, and 00Z04Jan2012 etc.          
   While for the 12Z 01Jan2012 fcst cycle, the verification is only done at the hours of 
   12Z01Jan2012, 12Z02Jan2012, 12Z03Jan2012, 12Z03Jan2012, and 12Z04Jan2012 etc.          
   THIS UPDATE ALLOWS THE FORECASTS TO BE VERIFIED AT ANY GIVEN OUTPUT INTERVALS.
   For instance, for 00Z 01Jan2012 cycle, the verification can be done at a 6-hour
   interval, that is, at the hours of 00Z01Jan2012, 06Z01Jan2012, 12Z01Jan2012, 
   18Z01Jan2012, and 00Z02Jan2012 etc, as long as the observations/analyses are
   avaliable at these verificaiton hours. This update makes it possible to exam 
   the diurnal variaiton of forecasts, and to apply this verification package
   for high-frequency short-range forecasts. 
2. Updated the script to allow different cycles of the same forecast model to be 
   verified at the same time. This is useful for inter-cycle comparison. For instance,
   by specifying  fcycle="00 06 12 18" and mdlist="gfs" in vsdbjob_submit.sh the
   verification will compare the four cycles of GFS forecasts.
3. updated vsdb/plot2d scripts to speed up making 2D maps by a factor of four.                                 
4. updated scripts under vsdb/map_util to speed up data mining by about 30 percent. 
5. Updated the web page template vsdb_exp_webpage.tar.  Earlier versions only
   work for the 00Z cycle of forecast.  The new one works for all cycles.  
6. The following programs and scripts are updated in this release: 
   setup_envs.sh, vsdbjob_submit.sh, verify_exp_step1.sh, verify_exp_step2.sh, 
   vsdb_exp_webpage.tar, plot2d/maps2d_new.sh,  and almost all programs/scripts 
   under ./exe and ./map_util.  


++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
Version 14 information, November 2012                              
1. Added ozone (O3) to the verification, including Bias and RMSE etc on six 
   standard layers (100, 70, 50, 30, 20 and 10 hPa). 
2. Updated precip verification (QPF) to include forecasts from the 12Z cycles. 
   Previous versions can only handle 00Z-cycle forecasts.
3. Updated fits-to-obs programs to properly handle missing values. In previous 
   versions, the program fails to make vertical profile plots if there is any
   missing value.
4. Updated fits-to-obs programs to use input data in either big_endian or 
   little_endian format, or a mixture of both formats. Users need to specify 
   input data format "endianlist" for each experiment in vsdbjob_submit.sh. 
5. "No space" error occurs when running on Zeus with shared nodes.  The "sub_zeus" 
    command was updated to increase virtual memory.

++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
Version 13 information, March 2012                              
1. Restructured the package so that it can be installed and used on different computer 
   platforms. Some of the legacy fortan codes that only works on IBM CCS have been 
   updated. Scripts have been modified to follow ksh standard. All data are written in 
   big_endian format. This new package can be easily set up on different computer 
   platforms by executing the newly added build.sh script and by modifying the newly 
   added setup_envs.sh script.
2. Added "grid2obs" verification (so far only works on IBM CCS). It compares a) modeled 
   T2m, RH2, WIND10m, Total cloud and cloud base height over the North America and 
   its subregions with surface observations included in NDAS or NAM prepbufr files and, 
   b) modeled T, Wind, Q, and RH over the globe its subregions with  upper-air observations 
   (e.g. rawinsonde, pibals, profilers, and ACARS etc) included in the NCEP operational 
   GDAS prepbufr files. Please see http://www.emc.ncep.noaa.gov/gmb/STATS_vsdb/g2o/ as 
   an example of this g2o verification. This part of verification has not been merged 
   into the master driver vsdbjob_submit.sh.  Users need to run grid2obs/grid2obs.sh
   to generate stats and then grid2obs/grid2obs_plot.sh to make graphics and to post 
   results on web servers for display.

++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
Version 12 information, Dec 2011                                
1. Added the capability of computing precipitation QPF skill scores (Previous versions
   require QPF skill scores as input and can only make graphics).  The computation 
   can be applied to 00Z and 12Z forecasts, using either pgb or flx files as input, 
   with any precip bucket or without a bucket at all, and with any forecast output 
   frequency and any forecast length. Set "CONUSDATA=YES" in vsdbjob_submit.sh turns 
   on this option.  Programs and scripts are saved under ./precip.
2. Added the option to remove missing data from all models/experiments so that the 
   same number of samples is used to verify all members in the graphics. All programs 
   and scripts under ./map_util have been changed to incorporate this option.  
   Set "maskmiss=1" in vsdbjob_submit.sh turns on this option.
3. Added NWPROD to use user-defined libaries and utilities instead of pointing to
   NCEP CCS /nwprod.  Copied the libaries and utilities under /nwprod that are used 
   by this package to a local directory $sordir/nwprod. This change makes the 
   package portable to different computer platforms.  

++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
Version 11 information, September 2011                          
1. Added an option to make maps of lat-lon distributions and zonal-mean cross-sections
   to compare forecasts  among different experiments. A few selected fields are also 
   compared with observations (mostly climatologies). This tool is particularily useful
   for detecting large changes and for code debugging. See MAPS2D in vsdbjob_submit.sh.
2. Added an option to verify all forecasts against ECMWF analysis.(set anl_type=ecmwf).
3. Allows users to choose the top of stats cross-section maps (set maptop=10, 50, or 100 hPa). 
4. Corrected a bug in the source code for computing soil moisture stats (credit: George Gayno). 
5. Corrected a bug in the source code for computing vector wind anomaly correction. 
6. Allows users to verify forecasts to any given length. In previous versions the verification
   only works for up to 384 hours of forecasts.


++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
Version 10 information, April 2011                          

1.  Added anomaly correlation of sea-level pressure to the verification package. 
2.  Added verification metrics over the PNA (Pacific North America, 80E-320E, 20N-75N) 
    region to the package. Users can also use the PNA slot as a template to display 
    stats over a region of their own interest. What users need to do is to redefine 
    the latitude and longitude boundaries of PNA given in exe/cntl_anom.sh and 
    exe/cntl_pres.sh.
3.  Added stats of area means of 18 surface variables over 12 sub-regions 
    to the package.  Changing the default "sfcvsdb=NO" in vsdbjob_submit.sh to  
    "sfcvsdb=YES" will turn on this group of verification.  It is sometimes useful for  
    detecting differences of near surface quantities among different experiments.
      
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
Version 09 information, November 2010                       

1.  Added a few new verification statistics, including Murphy's Mean-Squared Error Skill
    Score (MSESS), Ration of Standard Deviation, RMSE from Mean Difference, RMSE from Pattern
    Variation, and Anomalous Pattern Correlation. Updated web page template to display
    these metrics. Please see http://www.emc.ncep.noaa.gov/gmb/wx24fy/doc/RMSE_decomposition.pdf
    for definitions of these metrics and the advantange and disadvantage of each metric.

2.  Added the option to verifiy all experiments against the same mean analysis (manl).
    "manl" is defined as the average of analyses of all experiments users select. It is
    computed on the fly before VSDB partial sums are derived.  There are now four types 
    of analyses users can use by setting anl_type in vsdbjob_submit.sh to, 
      gfs : verified against each experiment's own GFS analysis,
      gdas : verified against each experiment's own GDAS analysis,
      canl : verified against common analysis, which is the mean of GFS, ECMWF and UKM operations,
      manl : verified against mean GFS analysis of the experiments selected.                       

++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
Version 08 information, October 2010 

1. Extended the top of RMSE and BIAS 2-D maps from 50 hPa to 10 hPa (Thanks to Glenn 
   White for making the suggestion).
2. Modified the package to allow users to run applications on different computers, 
   including vapor, with minimum changes to the scripts. Users only need 
   to copy and modify vsdbjob_submit.sh. 
3. Copied operational GFS daily fit-to-obs stats to vapor.  Vapor users are
   now able to generate fit-to-obs  plots to compare their own
   experiments with operational GFS. (Thanks to Suranjana Saha for making the 
   operational stats available, and to Jia-Fong Fan for doing test on vapor).
4. Added the function to read precipitation QPF stats in FHO VSDB format, to 
   compute precip ETS and Bias scores along with Monte Carlo significances, and to 
   make GrADs plots. Please see the applicaiton template ./precip/plot_pcp_vsdb.sh.
   (Thanks to Ying Lin for providing VSDB FHO data and for helpful discussion).
5. Changed all graphic output format from "gif" to "png" (Portable Network Graphics)
   which has much smaller file size and better web display quality.  Please see
   http://www.w3.org/QA/Tips/png-gif for a comparison between gif and pgn. Users
   who has been using my old web template need to use the updated vsdb_exp_webpage.tar. 
   (Thanks to Xiujuan Su for making the suggestion)

++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
Version 07 information, 15 January 2010

Monte Carlo significance test has been added to the maps of precip skill scores.  
Please see the  attached ppt file to see the methodology and examples. Only  
BIAS score and Equitable Threat Score are now included.  The TSS Score in the 
previous package has been removed to enhance graphic presentation.

++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
Version 06 information, 15 January 2009

1.  Suru's fit-to-obs tool that compares forecasts with rawindsonde observations 
    has been automated and added to the package.
2.  Thanks to Moorthi, the package itself is now able to handle forecasts that 
    are executed on different machines. 
3.  The master script vsdbjob_submit.sh has been modified and now is more flexible 
    for turning on and off different verification components.

++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
Version 04 information  10 July 2008

1.  An option is added to allow the users to choose either the GFS analysis or GDAS 
    analysis as the verification truth (observations).  The option is called "anl_type" 
    in  vsdbjob_submit.sh.  The default (previous)  option is  GFS analysis.   
    My thank goes to Russ Treadon for his suggestion.
2.  Confidence/uncertainty tests are added to the AC dieoff curves and RMSE error 
    growth curves. For instance, in the bottom portions of the two  plots attached 
    to this mail the hollow bars indicate the 95% significance level, that is,  
    those AC/RMSE differences falling outside of the bars are significantly different 
    at the 95% significance level.  My thanks go to Glenn and Steve for their suggestions.

++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
Version 03 information

1.  Thanks to Steve Lord and Xu Li,  the dieoff curve of anomaly correlation has been 
    amended to include the AC differences.  Please see the attached sample plots.  The 
    yellow shading in the plots indicates whether  or not the difference in AC between 
    the first and second experiments  is  statistically  significant.  The null hypothesis 
    is that AC1-AC2=0.  The assumed distribution of AC1-AC2 is Guassian.   This Gaussian 
    assumption is valid for dealing with the difference of ACs, even though the 
    distribution of AC itself is not Gaussian. 
2.  Scripts are added to automate the making and displaying of maps of precipitation 
    skill scores over the CONUS.


===============================
Mailing list:

 Helin Wei <Helin.Wei@noaa.gov>,
 Sarah Lu <Sarah.Lu@noaa.gov>,
 Shrinivas Moorthi <Shrinivas.Moorthi@noaa.gov>,
 Weizhong Zheng <Weizhong.Zheng@noaa.gov>,
 Jongil Han <Jongil.Han@noaa.gov>,
 Weiyu Yang <Weiyu.Yang@noaa.gov>,
 Haixia Liu <Haixia.Liu@noaa.gov>,
 Lidia Cucurull <Lidia.Cucurull@noaa.gov>,
 Henry Juang <Henry.Juang@noaa.gov>,
 Daryl Kleist <Daryl.Kleist@noaa.gov>,
 Russ Treadon <Russ.Treadon@noaa.gov>,
 "Michael A. Young" <Michael.A.Young@noaa.gov>,
 Xu Li <Xu.Li@noaa.gov>,
 DaNa Carlis <Dana.Carlis@noaa.gov>,
 Jordan Alpert <Jordan.Alpert@noaa.gov>,
 Suranjana Saha <Suranjana.Saha@noaa.gov>,
 Glenn White <Glenn.White@noaa.gov>,
 Diane Stokes <Diane.Stokes@noaa.gov>,
! Hualu Pan <Hualu.Pan@noaa.gov>,
 Hui-ya Chuang <Hui-Ya.Chuang@noaa.gov>,
! Todd Spindler <Todd.Spindler@noaa.gov>,
 Zavisa Janjic <Zavisa.Janjic@noaa.gov>,
 Binbin Zhou <Binbin.Zhou@noaa.gov>,
 Tong Zhu <Tong.Zhu@noaa.gov>,
 Yanqiu Zhu <Yanqiu.Zhu@noaa.gov>,
 Li Bi  <Li.Bi@noaa.gov>,
 Yuejian Zhu <Yuejian.Zhu@noaa.gov>,
 Kate Howard <Kate.Howard@noaa.gov>,
! Banghua Yan <Banghua.Yan@noaa.gov>,
 Zaizhong Ma <Zaizhong.Ma@noaa.gov>,
 XiuJuan Su <Xiujuan.Su@noaa.gov>,
! Yong Chen <Yong.Chen@noaa.gov>,
! Mark Rozwodoski <Mark.Rozwodoski@noaa.gov>,
 Ying Lin <Ying.Lin@noaa.gov>,
 Jia-Fong Fan <Jia-Fong.Fan@noaa.gov>,
 George Gayno <George.Gayno@noaa.gov>,
 jim jung <Jim.Jung@noaa.gov>,
 Xingren Wu <Xingren.Wu@noaa.gov>,
 Krishna.Kumar@noaa.gov,
 Ratko Vasic <Ratko.Vasic@noaa.gov>,
! Min-Jeong.Kim@noaa.gov
 edward.safford@noaa.gov 
 Thomas.B.Henderson@noaa.gov
! Bob Kistler <rek067@gmail.com>
! Eve-Marie.Devaliere@noaa.gov
 xue.wei@noaa.gov
 Jeffrey.S.whitaker@noaa.gov
 perry.shafran@noaa.gov
 craig.long@noaa.gov
 mingjing.tong@noaa.gov
 weiguo.wang@noaa.gov
 jack.woollen@noaa.gov
 Eric.Rogers@noaa.gov
 glenn.white@noaa.gov
 Mark Iredell <Mark.Iredell@noaa.gov>
 "Wanchun Chen" <Wanchun.Chen@noaa.gov>
 xiaoyan.zhang@noaa.gov
 Sajal.Kar@noaa.gov
 Jun.Wang@noaa.gov
 Emily.Liu@noaa.gov
 Andrew.Collard@noaa.gov
 hyun-chul.lee@noaa.gov
 baoqiang.xiang@noaa.gov
 keith.brill@noaa.gov
 michael.ek@noaa.gov
 wanqiu.wang@noaa.gov
 
 Dave Santek <dave.santek@ssec.wisc.edu>,
 Vairamuthu R Durai <durai.imd@gmail.com>,
 Surya Kanti Dutta <surya@ncmrwf.gov.in>
 rajagopal <rajagopal@ncmrwf.gov.in>
 Vivek SIngh <viveks@ncmrwf.gov.in>
 hanwei@cma.gov.cn
 Bin Zhao <zhaob@cma.gov.cn>
 Jing Chen <chenj@cma.gov.cn>
 pablo.reyes@cptec.inpe.br
 peter.caplan@yahoo.com
 "Emilia Kyung Jin" <kjin@cola.iges.org>
 "Dongchan Joo" <dc.joo@kiaps.org>
 Yunho Park <csat3@korea.kr>
 songyouhong@gmail.com
 
 

 Stephen Lord <Stephen.Lord@noaa.gov>,
 Bill Lapenta <Bill.Lapenta@noaa.gov>,
! John Ward <John.Ward@noaa.gov>,
 John Derber <John.Derber@noaa.gov>,
 Geoff DiMego <Geoff.Dimego@noaa.gov>,
 Stan Benjamin <stan.benjamin@noaa.gov
 Dave McCarren <david.mccarren@noaa.gov>
 David Dewitt <david.dewitt@noaa.gov>
 Jing Huang <jing.huang@noaa.gov>
 Arun Kumar <arun.kumar@noaa.gov>









- 
Fanglin Yang, Ph.D.
Environmental Modeling Center
National Centers for Environmental Prediction
301-6833722; fanglin.yang@noaa.gov
http://www.emc.ncep.noaa.gov/gmb/wx24fy/fyang/
http://www.emc.ncep.noaa.gov/gmb/STATS_vsdb/
