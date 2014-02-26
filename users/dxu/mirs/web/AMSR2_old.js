////////////////////////////////////////////////////////////////////////////////////////////////////
//
// variable and parameter define section
//
////////////////////////////////////////////////////////////////////////////////////////////////////

// define a satellite object with some fields
function satellite_object(name, algorithm, products)
{
  this.name = name;
  this.algorithm = algorithm;
  this.products = products;
}


// Satellite and its product(layers,channels)
var satellite_f16_her    = new satellite_object("f16", "her", "clw em(19h:19v:22v:37h:37v:91h:91v) rwp tpw tskin");

var satellite_f16_adv    = new satellite_object("f16", "adv", "chisq clw em(50v:52v:53v:54v:55v:57rc:59rc:150h:190h:186h:184h:19h:19v:22v:37h:37v:91v:91h:63rc:60rc1:60rc2:60rc3:60rc4:60rc5) emspectrum gs(cyl:pn:ps) iwp lwp nattempt niter psfc qc rr rrday rrlat rwp sfc(cyl:pn:ps) sfc2(cyl:pn:ps) sice(cyl:pn:ps) sicemy(cyl:pn:ps) sicefy(cyl:pn:ps) snow(cyl:pn:ps) swe(cyl:pn:ps) tbc(50v:52v:53v:54v:55v:57rc:59rc:150h:190h:186h:184h:19h:19v:22v:37h:37v:91v:91h:63rc:60rc1:60rc2:60rc3:60rc4:60rc5) tbu(50v:52v:53v:54v:55v:57rc:59rc:150h:190h:186h:184h:19h:19v:22v:37h:37v:91v:91h:63rc:60rc1:60rc2:60rc3:60rc4:60rc5) temp(100mb:200mb:300mb:400mb:500mb:600mb:700mb:800mb:850mb:900mb:950mb) tpw tskin wspd wv(100mb:200mb:300mb:400mb:500mb:600mb:700mb:800mb:850mb:900mb:950mb) clwp(100mb:200mb:300mb:400mb:500mb:600mb:700mb:800mb:850mb:900mb:950mb) rainp(100mb:200mb:300mb:400mb:500mb:600mb:700mb:800mb:850mb:900mb:950mb) graupelp(100mb:200mb:300mb:400mb:500mb:600mb:700mb:800mb:850mb:900mb:950mb)");
var satellite_f18_adv    = new satellite_object("f18", "adv", "chisq clw em(50h:52h:53h:54h:55h:57rc:59rc:150h:190h:186h:184h:19h:19v:22v:37h:37v:91v:91h:63rc:60rc1:60rc2:60rc3:60rc4:60rc5) emspectrum gs(cyl:pn:ps) iwp lwp nattempt niter psfc qc rr rrday rrlat rwp sfc(cyl:pn:ps) sfc2(cyl:pn:ps) sice(cyl:pn:ps) sicemy(cyl:pn:ps) sicefy(cyl:pn:ps) snow(cyl:pn:ps) swe(cyl:pn:ps) tbc(50h:52h:53h:54h:55h:57rc:59rc:150h:190h:186h:184h:19h:19v:22v:37h:37v:91v:91h:63rc:60rc1:60rc2:60rc3:60rc4:60rc5) tbu(50h:52h:53h:54h:55h:57rc:59rc:150h:190h:186h:184h:19h:19v:22v:37h:37v:91v:91h:63rc:60rc1:60rc2:60rc3:60rc4:60rc5) temp(100mb:200mb:300mb:400mb:500mb:600mb:700mb:800mb:850mb:900mb:950mb) tpw tskin wspd wv(100mb:200mb:300mb:400mb:500mb:600mb:700mb:800mb:850mb:900mb:950mb) clwp(100mb:200mb:300mb:400mb:500mb:600mb:700mb:800mb:850mb:900mb:950mb) rainp(100mb:200mb:300mb:400mb:500mb:600mb:700mb:800mb:850mb:900mb:950mb) graupelp(100mb:200mb:300mb:400mb:500mb:600mb:700mb:800mb:850mb:900mb:950mb)");

var satellite_n18_her    = new satellite_object("n18",    "her", "at(23:31:50:52:53:54:89:150:184:187:190) clw em(23v:31v:50v) sice tpw ts iwp rr snow swe wet");
var satellite_n18_adv    = new satellite_object("n18",    "adv", "chisq clw em(23v:31v:50v:52v:53h:54h:54v:55h:57h1:57h2:57h3:57h4:57h5:57h6:89v1:89v2:157h:184h:186h:190v) emspectrum gs(cyl:pn:ps) iwp lwp nattempt niter psfc qc rr rrday rrlat rwp sfc(cyl:pn:ps) sfc2(cyl:pn:ps) sice(cyl:pn:ps) sicemy(cyl:pn:ps) sicefy(cyl:pn:ps) snow(cyl:pn:ps) swe(cyl:pn:ps) tbc(23v:31v:50v:52v:53h:54h:54v:55h:57h1:57h2:57h3:57h4:57h5:57h6:89v1:89v2:157h:184h:186h:190v) tbu(23v:31v:50v:52v:53h:54h:54v:55h:57h1:57h2:57h3:57h4:57h5:57h6:89v1:89v2:157h:184h:186h:190v) temp(100mb:200mb:300mb:400mb:500mb:600mb:700mb:800mb:850mb:900mb:950mb) tpw tskin wv(100mb:200mb:300mb:400mb:500mb:600mb:700mb:800mb:850mb:900mb:950mb) clwp(100mb:200mb:300mb:400mb:500mb:600mb:700mb:800mb:850mb:900mb:950mb) rainp(100mb:200mb:300mb:400mb:500mb:600mb:700mb:800mb:850mb:900mb:950mb) graupelp(100mb:200mb:300mb:400mb:500mb:600mb:700mb:800mb:850mb:900mb:950mb)");

var satellite_n19_her    = new satellite_object("n19",    "her", "at(23:31:50:52:53:54:89:150:184:187:190) clw em(23v:31v:50v) sice tpw ts iwp rr snow swe wet");
var satellite_n19_adv    = new satellite_object("n19",    "adv", "chisq clw em(23v:31v:50v:52v:53h:54h:54v:55h:57h1:57h2:57h3:57h4:57h5:57h6:89v1:89v2:157h:184h:186h:190v) emspectrum gs(cyl:pn:ps) iwp lwp nattempt niter psfc qc rr rrday rrlat rwp sfc(cyl:pn:ps) sfc2(cyl:pn:ps) sice(cyl:pn:ps) sicemy(cyl:pn:ps) sicefy(cyl:pn:ps) snow(cyl:pn:ps) swe(cyl:pn:ps) tbc(23v:31v:50v:52v:53h:54h:54v:55h:57h1:57h2:57h3:57h4:57h5:57h6:89v1:89v2:157h:184h:186h:190v) tbu(23v:31v:50v:52v:53h:54h:54v:55h:57h1:57h2:57h3:57h4:57h5:57h6:89v1:89v2:157h:184h:186h:190v) temp(100mb:200mb:300mb:400mb:500mb:600mb:700mb:800mb:850mb:900mb:950mb) tpw tskin wv(100mb:200mb:300mb:400mb:500mb:600mb:700mb:800mb:850mb:900mb:950mb) clwp(100mb:200mb:300mb:400mb:500mb:600mb:700mb:800mb:850mb:900mb:950mb) rainp(100mb:200mb:300mb:400mb:500mb:600mb:700mb:800mb:850mb:900mb:950mb) graupelp(100mb:200mb:300mb:400mb:500mb:600mb:700mb:800mb:850mb:900mb:950mb)");

var satellite_metopA_her = new satellite_object("metopA", "her", "at(23:31:50:52:53:54:89:150:184:187:190) clw em(23v:31v:50v) sice tpw ts iwp rr snow swe wet");
var satellite_metopA_adv = new satellite_object("metopA", "adv", "chisq clw em(23v:31v:50v:52v:53h:54h:54v:55h:57h1:57h2:57h3:57h4:57h5:57h6:89v1:89v2:157h:184h:186h:190v) emspectrum gs(cyl:pn:ps) iwp lwp nattempt niter psfc qc rr rrday rrlat rwp sfc(cyl:pn:ps) sfc2(cyl:pn:ps) sice(cyl:pn:ps) sicemy(cyl:pn:ps) sicefy(cyl:pn:ps) snow(cyl:pn:ps) swe(cyl:pn:ps) tbc(23v:31v:50v:52v:53h:54h:54v:55h:57h1:57h2:57h3:57h4:57h5:57h6:89v1:89v2:157h:184h:186h:190v) tbu(23v:31v:50v:52v:53h:54h:54v:55h:57h1:57h2:57h3:57h4:57h5:57h6:89v1:89v2:157h:184h:186h:190v) temp(100mb:200mb:300mb:400mb:500mb:600mb:700mb:800mb:850mb:900mb:950mb) tpw tskin wv(100mb:200mb:300mb:400mb:500mb:600mb:700mb:800mb:850mb:900mb:950mb) clwp(100mb:200mb:300mb:400mb:500mb:600mb:700mb:800mb:850mb:900mb:950mb) rainp(100mb:200mb:300mb:400mb:500mb:600mb:700mb:800mb:850mb:900mb:950mb) graupelp(100mb:200mb:300mb:400mb:500mb:600mb:700mb:800mb:850mb:900mb:950mb)");

var satellite_metopB_her = new satellite_object("metopB", "her", "at(23:31:50:52:53:54:89:150:184:187:190) clw em(23v:31v:50v) sice tpw ts iwp rr snow swe wet");
var satellite_metopB_adv = new satellite_object("metopB", "adv", "chisq clw em(23v:31v:50v:52v:53h:54h:54v:55h:57h1:57h2:57h3:57h4:57h5:57h6:89v1:89v2:157h:184h:186h:190v) emspectrum gs(cyl:pn:ps) iwp lwp nattempt niter psfc qc rr rrday rrlat rwp sfc(cyl:pn:ps) sfc2(cyl:pn:ps) sice(cyl:pn:ps) sicemy(cyl:pn:ps) sicefy(cyl:pn:ps) snow(cyl:pn:ps) swe(cyl:pn:ps) tbc(23v:31v:50v:52v:53h:54h:54v:55h:57h1:57h2:57h3:57h4:57h5:57h6:89v1:89v2:157h:184h:186h:190v) tbu(23v:31v:50v:52v:53h:54h:54v:55h:57h1:57h2:57h3:57h4:57h5:57h6:89v1:89v2:157h:184h:186h:190v) temp(100mb:200mb:300mb:400mb:500mb:600mb:700mb:800mb:850mb:900mb:950mb) tpw tskin wv(100mb:200mb:300mb:400mb:500mb:600mb:700mb:800mb:850mb:900mb:950mb) clwp(100mb:200mb:300mb:400mb:500mb:600mb:700mb:800mb:850mb:900mb:950mb) rainp(100mb:200mb:300mb:400mb:500mb:600mb:700mb:800mb:850mb:900mb:950mb) graupelp(100mb:200mb:300mb:400mb:500mb:600mb:700mb:800mb:850mb:900mb:950mb)");

var satellite_npp_adv    = new satellite_object("npp",    "adv", "chisq clw em(23v:31v:50h:51h:52h:53h:54h1:54h2:55h:57h1:57h2:57h3:57h4:57h5:57h6:88v:165h:183h1:183h2:183h3:183h4:183h5) emspectrum gs(cyl:pn:ps) iwp lwp nattempt niter psfc qc rr rrday rrlat rwp sfc(cyl:pn:ps) sfc2(cyl:pn:ps) sice(cyl:pn:ps) sicemy(cyl:pn:ps) sicefy(cyl:pn:ps) snow(cyl:pn:ps) swe(cyl:pn:ps) tbc(23v:31v:50h:51h:52h:53h:54h1:54h2:55h:57h1:57h2:57h3:57h4:57h5:57h6:88v:165h:183h1:183h2:183h3:183h4:183h5) tbu(23v:31v:50h:51h:52h:53h:54h1:54h2:55h:57h1:57h2:57h3:57h4:57h5:57h6:88v:165h:183h1:183h2:183h3:183h4:183h5) temp(100mb:200mb:300mb:400mb:500mb:600mb:700mb:800mb:850mb:900mb:950mb) tpw tskin wv(100mb:200mb:300mb:400mb:500mb:600mb:700mb:800mb:850mb:900mb:950mb) clwp(100mb:200mb:300mb:400mb:500mb:600mb:700mb:800mb:850mb:900mb:950mb) rainp(100mb:200mb:300mb:400mb:500mb:600mb:700mb:800mb:850mb:900mb:950mb) graupelp(100mb:200mb:300mb:400mb:500mb:600mb:700mb:800mb:850mb:900mb:950mb)");
var satellite_trmm_adv   = new satellite_object("trmm",   "adv", "chisq clw em(11v:11h:19v:19h:21v:37v:37h:85v:85h) iwp lwp nattempt niter psfc qc rr rwp sfc(cyl:pn:ps) tbc(11v:11h:19v:19h:21v:37v:37h:85v:85h) tbu(11v:11h:19v:19h:21v:37v:37h:85v:85h) temp(100mb:200mb:300mb:400mb:500mb:600mb:700mb:800mb:850mb:900mb:950mb) tpw tskin wspd wv(100mb:200mb:300mb:400mb:500mb:600mb:700mb:800mb:850mb:900mb:950mb)");

var satellite_mtma_adv   = new satellite_object("mtma",   "adv", "chisq clw em(19v:19h:24v:37v:37h:89v:89h:157v:157h) emspectrum iwp lwp nattempt niter psfc qc rr rwp sfc(cyl) sfc2(cyl) tbc(19v:19h:24v:37v:37h:89v:89h:157v:157h) tbu(19v:19h:24v:37v:37h:89v:89h:157v:157h) temp(100mb:200mb:300mb:400mb:500mb:600mb:700mb:800mb:850mb:900mb:950mb) tpw tskin wspd wv(100mb:200mb:300mb:400mb:500mb:600mb:700mb:800mb:850mb:900mb:950mb) clwp(100mb:200mb:300mb:400mb:500mb:600mb:700mb:800mb:850mb:900mb:950mb) rainp(100mb:200mb:300mb:400mb:500mb:600mb:700mb:800mb:850mb:900mb:950mb) graupelp(100mb:200mb:300mb:400mb:500mb:600mb:700mb:800mb:850mb:900mb:950mb)");
var satellite_mtsa_adv   = new satellite_object("mtsa",   "adv", "chisq clw em(183h:184h:186h:187h:190h:194h) iwp lwp nattempt niter psfc qc rr rwp sfc(cyl) sfc2(cyl) tbc(183h:184h:186h:187h:190h:194h) tbu(183h:184h:186h:187h:190h:194h) temp(100mb:200mb:300mb:400mb:500mb:600mb:700mb:800mb:850mb:900mb:950mb) tpw tskin wv(100mb:200mb:300mb:400mb:500mb:600mb:700mb:800mb:850mb:900mb:950mb) clwp(100mb:200mb:300mb:400mb:500mb:600mb:700mb:800mb:850mb:900mb:950mb) rainp(100mb:200mb:300mb:400mb:500mb:600mb:700mb:800mb:850mb:900mb:950mb) graupelp(100mb:200mb:300mb:400mb:500mb:600mb:700mb:800mb:850mb:900mb:950mb)");

var satellite_gcomw1_adv = new satellite_object("gcomw1", "adv", "chisq clw em(6v:6h:10v:10h:18v:18h:23v:23h:36v:36h:89v:89h) iwp lwp nattempt niter psfc qc rr rwp sfc(cyl:pn:ps) tbc(6v:6h:10v:10h:18v:18h:23v:23h:36v:36h:89v:89h) tbu(6v:6h:10v:10h:18v:18h:23v:23h:36v:36h:89v:89h) temp(100mb:200mb:300mb:400mb:500mb:600mb:700mb:800mb:850mb:900mb:950mb) tpw tskin wv(100mb:200mb:300mb:400mb:500mb:600mb:700mb:800mb:850mb:900mb:950mb)");


var satellite_list = new Array(
	satellite_n18_her,
	satellite_n18_adv,
	satellite_n19_her,
	satellite_n19_adv,
	satellite_metopA_her,
	satellite_metopA_adv,
	satellite_f16_adv,
	satellite_f18_adv,
	satellite_trmm_adv,
	satellite_mtma_adv,
	satellite_mtsa_adv,
	satellite_gcomw1_adv );

//	satellite_metopB_her,
//	satellite_metopB_adv,
//	satellite_npp_adv,


var SENSORTAG = new Array();
SENSORTAG["n18"]    = "_poes_n18_amsuamhs_";
SENSORTAG["n19"]    = "_poes_n19_amsuamhs_";
SENSORTAG["metopA"] = "_poes_metopA_amsuamhs_";
SENSORTAG["metopB"] = "_poes_metopB_amsuamhs_";
SENSORTAG["f16"]    = "_dmsp_f16_ssmis_"; 
SENSORTAG["f18"]    = "_dmsp_f18_ssmis_"; 
SENSORTAG["npp"]    = "_npoess_npp_atms_";
SENSORTAG["trmm"]   = "_eos_trmm_tmi_";
SENSORTAG["mtma"]   = "_mt_mtma_madras_";
SENSORTAG["mtsa"]   = "_mt_mtsa_saphir_";
SENSORTAG["gcomw1"] = "_eos_gcomw1_amsr2_";

// MSPPS Satellite Directory
var SAT2MSPPS = new Array();
SAT2MSPPS["f16"]    = "F16";
SAT2MSPPS["n18"]    = "N18";
SAT2MSPPS["n19"]    = "N19";
SAT2MSPPS["metopA"] = "MOA";
SAT2MSPPS["metopB"] = "MOB";

// MSPPS Antenna Temperature Frenquency to Channel name mapping
var AT2CHANNEL = new Array();
AT2CHANNEL["23"]  = "1";
AT2CHANNEL["31"]  = "2";
AT2CHANNEL["50"]  = "3";
AT2CHANNEL["52"]  = "4";
AT2CHANNEL["53"]  = "5";
AT2CHANNEL["54"]  = "6";
AT2CHANNEL["89"]  = "1";
AT2CHANNEL["150"] = "2";
AT2CHANNEL["184"] = "3";
AT2CHANNEL["187"] = "4";
AT2CHANNEL["190"] = "5";

// MSPPS Emmisivity Frenquency to Channel name mapping
var EM2CHANNEL = new Array();
EM2CHANNEL["23v"] = "1" ;
EM2CHANNEL["31v"] = "2" ;
EM2CHANNEL["50v"] = "3" ;

// algorithm
var ALG2NAME =  new Array();
ALG2NAME["her"] = "MSPPS" ;
ALG2NAME["adv"] = "MIRS" ;

// cend tag for mspps
var CENDTAG = new Array();
CENDTAG["as"] = "asc";
CENDTAG["ds"] = "des";


// some special notes concerning sencors
var NOTES = new Array();
NOTES["mtma"]  = "Note: MT-MADRAS instruments are based on simulated proxy data for testing purposes only";
NOTES["mtsa"]  = "Note: MT-SAPHIR instruments are based on simulated proxy data for testing purposes only";



// image location
var IMGDIR_ADV="images/";
var IMGDIR_HER="/corp/scsb/mspps/GIFs/";


// title for map project selection
var PROJ_TITLE = "choose a map projection type (cyl:cylindrical; pn:Northern Polar Stereographic; ps:Southern Polar Stereographic)"

var NOW = new Date();
NOW.setDate(NOW.getDate()-1);
var SECOND = NOW.getTime()/1000;

var YEARS_DEFAULT = new Array();
var MONTHS_DEFAULT = new Array();
var DAYS_DEFAULT = new Array();

YEARS_DEFAULT["1"] = "";
YEARS_DEFAULT["2"] = "";
YEARS_DEFAULT["3"] = "";
YEARS_DEFAULT["4"] = "";

MONTHS_DEFAULT["1"] = "";
MONTHS_DEFAULT["2"] = "";
MONTHS_DEFAULT["3"] = "";
MONTHS_DEFAULT["4"] = "";

DAYS_DEFAULT["1"] = "";
DAYS_DEFAULT["2"] = "";
DAYS_DEFAULT["3"] = "";
DAYS_DEFAULT["4"] = "";

var IS_TRMM_ONS = new Array();
IS_TRMM_ONS["1"] = 0;
IS_TRMM_ONS["2"] = 0;
IS_TRMM_ONS["3"] = 0;
IS_TRMM_ONS["4"] = 0;

var NDAY_TRMM = 4;

////////////////////////////////////////////////////////////////////////////////////////////////////
//
// animation parameters section
//
////////////////////////////////////////////////////////////////////////////////////////////////////
var last_image  = 31;
var first_image = 1;
var image_name  ="";
var current_image = 0; 	//number of the current image

var images1 = new Array();      	//holds the images in panel1
var images2 = new Array();      	//holds the images in panel2
var images3 = new Array();      	//holds the images in panel3
var images4 = new Array();      	//holds the images in panel4

var imageNum1  = new Array();       	//keeps track of which images to omit from loop
var imageNum2  = new Array();       	//keeps track of which images to omit from loop
var imageNum3  = new Array();       	//keeps track of which images to omit from loop
var imageNum4  = new Array();       	//keeps track of which images to omit from loop

var alts1 = new Array();
var alts2 = new Array();
var alts3 = new Array();
var alts4 = new Array();

var delay = 500;         		//delay between frames in 1/100 seconds
var timeID = null;
var PREFIX = "/mirs_"  ;
var image_type  = ".png";

var yyyy_indexes = new Array();
var mm_indexes   = new Array();
var dd_indexes   = new Array();


for (var i = 0; i < last_image; i++) {
	imageNum1[i] = false;	
	imageNum2[i] = false;	
	imageNum3[i] = false;	
	imageNum4[i] = false;	
} 


var YEAR_START = 2011;


////////////////////////////////////////////////////////////////////////////////////////////////////
//
// return product text based on a value
//
////////////////////////////////////////////////////////////////////////////////////////////////////
function getProductText( val ) {
 
  var txt = "";
  
  if 		( val=="at" )		txt = "Antenna Temp" ; 
  else if 	( val=="chisq" )	txt = "Chi Square" ;	       
  else if 	( val=="clw" )	   	txt = "CLW" ;     
  else if 	( val=="em" )	   	txt = "Emi" ; 
  else if 	( val=="emspectrum" )   txt = "Emis Spectrum" ; 
  else if 	( val=="gs" )	   	txt = "Snow Grain Size" ;  
  else if 	( val=="iwp" )	   	txt = "Ice Water Path" ;
  else if 	( val=="lwp" )	   	txt = "Liquid Water Path" ;
  else if 	( val=="nattempt" )	txt = "Attempt Number" ;
  else if 	( val=="niter" )	txt = "Iteration Number" ;
  else if 	( val=="psfc" )	   	txt = "Sfc Pressure" ;    
  else if 	( val=="qc" )	   	txt = "QC Flag" ;    
  else if 	( val=="rr" )	   	txt = "Rain Rate" ;   
  else if 	( val=="rrday" )	txt = "Precip Estimate" ;
  else if 	( val=="rrlat" )	txt = "Precip Lat Distri." ;
  else if 	( val=="rwp" )	   	txt = "Rain Water Path" ;     
  else if 	( val=="seaicvr" )   	txt = "Sea Ice Cover" ;
  else if 	( val=="sice" )   	txt = "SIC" ;
  else if 	( val=="sicemy" )   	txt = "MY SIC" ;
  else if 	( val=="sicefy" )   	txt = "FY SIC" ;
  else if 	( val=="snow" )    	txt = "Snow Cover" ;
  else if 	( val=="swe" )  	txt = "SWE" ;       
  else if 	( val=="swp" )  	txt = "Snow Water Path" ;     
  else if 	( val=="windsp" )    	txt = "Sfc Wind Speed" ;
  else if 	( val=="temp" )	   	txt = "Temp Profile" ; 
  else if 	( val=="tpw" )	   	txt = "TPW" ;
  else if 	( val=="ts" )	   	txt = "Sfc Temp" ;
  else if 	( val=="tskin" )	txt = "Skin Temp" ;    
  else if 	( val=="wspd" )		txt = "Wind Speed" ;    
  else if 	( val=="wet" )	   	txt = "Wetness Index" ; 
  else if 	( val=="wv" )	   	txt = "WV Profile" ; 
  else if 	( val=="clwp" )	   	txt = "CLW Profile" ; 
  else if 	( val=="rainp" )        txt = "Rain Profile" ; 
  else if 	( val=="graupelp" )     txt = "Graupel Profile" ; 
  else if 	( val=="tbc" )    	txt = "Corr. TB" ;
  else if 	( val=="tbu" )    	txt = "UnCorr. TB" ;
  else if 	( val=="sfc" )   	txt = "Pre Sfc Type" ;
  else if 	( val=="sfc2" )   	txt = "Post Sfc Type" ;

  return txt;

}


////////////////////////////////////////////////////////////////////////////////////////////////////
//
// Help function when algorithm or satellite changes
//
////////////////////////////////////////////////////////////////////////////////////////////////////
function changeAlgorithmSatelliteHelp( alg_obj, sat_obj, prod_obj, layer_obj, proj_obj, layer_id, proj_id, panel_id ) {

	var sat_value  = sat_obj.value;
	var alg_value  = alg_obj.value;
	
	var prod_old   = prod_obj.value;
	var layer_old  = layer_obj.value;
	
	prod_obj.options.length  = 0;
	var prod_old_exist = 0;  // to keep track whether new product list has old product or not
	
	for ( isat=0; isat < satellite_list.length; isat++ ) {
	    if (satellite_list[isat].name == sat_value && satellite_list[isat].algorithm == alg_value ) {
		var products = satellite_list[isat].products.split(" ");
		for ( i=0; i<products.length; i++ )  {
		    var left = products[i].indexOf("(");
		    var prod_value = products[i] ;
		    if ( left != -1 ) { prod_value = products[i].substring(0,left); }
		    if ( prod_value == prod_old ) { prod_old_exist = 1; }
		    var prod_text = getProductText(prod_value);
		    prod_obj.options[i] 	    = new Option();
		    prod_obj.options[i].value	    = prod_value;
		    prod_obj.options[i].text	    = prod_text;
	    	}
	    }
	}
	
	// if populated product list has old product, we use old one; otherwise, use the 1st one in the list
	var  product_value = "";
	if ( prod_old_exist == 1 ) {
	    prod_obj.value = prod_old;
	    product_value  = prod_old;
	}
	else {
	    product_value = prod_obj.options[0].value;
	}

	// Layers, which depends on product selected
	layer_obj.options.length = 0; 
	var layer_old_exist = 0;  // to keep track whether new layer list has old layer or not
	document.getElementById(layer_id).className="optioninvisible";  

	//Projection, depends on whether product is em or not
	document.getElementById(proj_id).className="optioninvisible";
	if ( product_value == 'em' && sat_value != 'trmm' ) {
	    document.getElementById(proj_id).className="optionvisible";
	}


	for ( isat=0; isat<satellite_list.length; isat++ ) {
	    if (satellite_list[isat].name == sat_value && satellite_list[isat].algorithm == alg_value ) {
		var products = satellite_list[isat].products.split(" ");
		for ( i=0; i<products.length; i++ )  {
		    var left = products[i].indexOf("(");
		    var prod_value = products[i] ;
		    if ( left != -1 ) { prod_value = products[i].substring(0,left);}
		    
		    if ( prod_value == product_value && left > 0 ) {
	   	        document.getElementById(layer_id).className="optionvisible";  
		
		    	var right = products[i].indexOf(")");
		    	var layer_str = products[i].substring(left+1,right);
		    	var layers = layer_str.split(":");
		    	
		    	for ( j=0; j<layers.length; j++ ) {
		    	    layer_obj.options[j] = new Option();
		    	    layer_obj.options[j].value = layers[j];
		    	    if ( layer_old == layers[j] ) { layer_old_exist=1; }
			    
			    // update title
		    	    if ( prod_value == 'em' || prod_value == 'tbc' || prod_value == 'tbu' ) {
		    	        layer_obj.options[j].text  = 'ch'+String(j+1)+":"+String(layers[j]);
				document.getElementById(layer_id).title = "choose a channel";
			    }
			    else if( prod_value == 'temp' || prod_value == 'wv'    ||
			             prod_value == 'clwp' || prod_value == 'rainp' || prod_value == 'graupelp' ) {
				
				layer_obj.options[j].text  = String(layers[j]);     
				document.getElementById(layer_id).title = "choose a pressure layer";   
			    
			    }
			    else if( prod_value == 'sfc'  || prod_value == 'sfc2'   || prod_value == 'snow'   ||
			             prod_value == 'sice' || prod_value == 'sicefy' || prod_value == 'sicemy' ||
				     prod_value == 'gs'   || prod_value == 'swe' ) {
				
				layer_obj.options[j].text  = String(layers[j]);     
				document.getElementById(layer_id).title = PROJ_TITLE;
			    
			    }
		    	    else { 
		    	        layer_obj.options[j].text  = String(layers[j]);
			    }
		    	}	
		    } 
	    	}
	    }
	}
	
	// if populated layer list has old layer, we use old one; otherwise, use the 1st one in the layer list
	if ( layer_old_exist == 1 ) { layer_obj.value = layer_old; }
	
	
	// this is for TRMM TMI delay
	var yr_id = "yr"+panel_id;
	var mo_id = "mo"+panel_id;
	var dy_id = "dy"+panel_id;
	
	yr_obj = document.getElementById(yr_id);
	mo_obj = document.getElementById(mo_id);
	dy_obj = document.getElementById(dy_id);

	year_cur = yr_obj.value;
	month_cur = mo_obj.value;
	day_cur = dy_obj.value;

	var id = panel_id;

	if( sat_value == 'trmm' && 
	    IS_TRMM_ONS[id] == 0 && 
	    year_cur == YEARS_DEFAULT[id] && 
	    month_cur == MONTHS_DEFAULT[id] && 
	    day_cur == DAYS_DEFAULT[id] ) { 

        	var year  = parseInt(yr_obj.value,10);
        	var month = parseInt(mo_obj.value,10);
        	var day   = parseInt(dy_obj.value,10);

        	var now = new Date(year,month-1,day);
        	now.setDate(now.getDate()-NDAY_TRMM);

		year  = now.getFullYear();
        	month = now.getMonth();
        	day   = now.getDate();

        	yr_obj.value = year;
        	mo_obj.selectedIndex = month ;
        	dy_obj.selectedIndex = day - 1;
		
		IS_TRMM_ONS[id] = 1;
	}
	
}


////////////////////////////////////////////////////////////////////////////////////////////////////
//
// when algorithm changes
//
////////////////////////////////////////////////////////////////////////////////////////////////////
function changeAlgorithm_template( alg_obj, sat_obj, prod_obj, layer_obj, proj_obj, layer_id, proj_id, panel_id ) {
	changeAlgorithmSatelliteHelp( alg_obj, sat_obj, prod_obj, layer_obj, proj_obj, layer_id, proj_id, panel_id );
}


////////////////////////////////////////////////////////////////////////////////////////////////////
//
// when satellite changes
//
////////////////////////////////////////////////////////////////////////////////////////////////////
function changeSatellite_template( alg_obj, sat_obj, prod_obj, layer_obj, proj_obj, layer_id, proj_id, panel_id ) { 
	changeAlgorithmSatelliteHelp( alg_obj, sat_obj, prod_obj, layer_obj, proj_obj, layer_id, proj_id, panel_id );
}


////////////////////////////////////////////////////////////////////////////////////////////////////
//
// simplified version of changeAlgorithmSatelliteHelp
// where id is integer 1, 2, 3, 4
//
////////////////////////////////////////////////////////////////////////////////////////////////////
function changeAlgorithmSatelliteHelp_INT( id ) {

	var panel_id = ""+id; // convert into String
	
	var alg_id = "alg" + panel_id;
	var sat_id = "sat" + panel_id;
	var prod_id = "prod" + panel_id;
	var layer_id = "layer" + panel_id;
	var proj_id = "proj" + panel_id;
	
	var alg_obj = getElementById(alg_id);
	var sat_obj = getElementById(sat_id);
	var prod_obj = getElementById(prod_id);
	var layer_obj = getElementById(layer_id);
	var proj_obj = getElementById(proj_id);
	
	var sat_value  = sat_obj.value;
	var alg_value  = alg_obj.value;
	
	var prod_old   = prod_obj.value;
	var layer_old  = layer_obj.value;
	
	prod_obj.options.length  = 0;
	var prod_old_exist = 0;  // to keep track whether new product list has old product or not
	
	for ( isat=0; isat < satellite_list.length; isat++ ) {
	    if (satellite_list[isat].name == sat_value && satellite_list[isat].algorithm == alg_value ) {
		var products = satellite_list[isat].products.split(" ");
		for ( i=0; i<products.length; i++ )  {
		    var left = products[i].indexOf("(");
		    var prod_value = products[i] ;
		    if ( left != -1 ) { prod_value = products[i].substring(0,left); }
		    if ( prod_value == prod_old ) { prod_old_exist = 1; }
		    var prod_text = getProductText(prod_value);
		    prod_obj.options[i] 	    = new Option();
		    prod_obj.options[i].value	    = prod_value;
		    prod_obj.options[i].text	    = prod_text;
	    	}
	    }
	}
	
	// if populated product list has old product, we use old one; otherwise, use the 1st one in the list
	var  product_value = "";
	if ( prod_old_exist == 1 ) {
	    prod_obj.value = prod_old;
	    product_value  = prod_old;
	}
	else {
	    product_value = prod_obj.options[0].value;
	}

	// Layers, which depends on product selected
	layer_obj.options.length = 0; 
	var layer_old_exist = 0;  // to keep track whether new layer list has old layer or not
	document.getElementById(layer_id).className="optioninvisible";  

	//Projection, depends on whether product is em or not
	document.getElementById(proj_id).className="optioninvisible";
	if ( product_value == 'em' && sat_value != 'trmm' ) {
	    document.getElementById(proj_id).className="optionvisible";
	}


	for ( isat=0; isat<satellite_list.length; isat++ ) {
	    if (satellite_list[isat].name == sat_value && satellite_list[isat].algorithm == alg_value ) {
		var products = satellite_list[isat].products.split(" ");
		for ( i=0; i<products.length; i++ )  {
		    var left = products[i].indexOf("(");
		    var prod_value = products[i] ;
		    if ( left != -1 ) { prod_value = products[i].substring(0,left);}
		    
		    if ( prod_value == product_value && left > 0 ) {
	   	        document.getElementById(layer_id).className="optionvisible";  
		
		    	var right = products[i].indexOf(")");
		    	var layer_str = products[i].substring(left+1,right);
		    	var layers = layer_str.split(":");
		    	
		    	for ( j=0; j<layers.length; j++ ) {
		    	    layer_obj.options[j] = new Option();
		    	    layer_obj.options[j].value = layers[j];
		    	    if ( layer_old == layers[j] ) { layer_old_exist=1; }
			    
			    // update title
		    	    if ( prod_value == 'em' || prod_value == 'tbc' || prod_value == 'tbu' ) {
		    	        layer_obj.options[j].text  = 'ch'+String(j+1)+":"+String(layers[j]);
				document.getElementById(layer_id).title = "choose a channel";
			    }
			    else if( prod_value == 'temp' || prod_value == 'wv'    ||
			             prod_value == 'clwp' || prod_value == 'rainp' || prod_value == 'graupelp' ) {
				
				layer_obj.options[j].text  = String(layers[j]);     
				document.getElementById(layer_id).title = "choose a pressure layer";   
			    
			    }
			    else if( prod_value == 'sfc'  || prod_value == 'sfc2'   || prod_value == 'snow'   ||
			             prod_value == 'sice' || prod_value == 'sicefy' || prod_value == 'sicemy' ||
				     prod_value == 'gs'   || prod_value == 'swe' ) {
				
				layer_obj.options[j].text  = String(layers[j]);     
				document.getElementById(layer_id).title = PROJ_TITLE;
			    
			    }
		    	    else { 
		    	        layer_obj.options[j].text  = String(layers[j]);
			    }
		    	}	
		    } 
	    	}
	    }
	}
	
	// if populated layer list has old layer, we use old one; otherwise, use the 1st one in the layer list
	if ( layer_old_exist == 1 ) { layer_obj.value = layer_old; }
	
	
	// this is for TRMM TMI latency
	var yr_id = "yr"+panel_id;
	var mo_id = "mo"+panel_id;
	var dy_id = "dy"+panel_id;
	
	yr_obj = document.getElementById(yr_id);
	mo_obj = document.getElementById(mo_id);
	dy_obj = document.getElementById(dy_id);

	year_cur = yr_obj.value;
	month_cur = mo_obj.value;
	day_cur = dy_obj.value;

	if( sat_value == 'trmm' && 
	    IS_TRMM_ONS[panel_id] == 0 && 
	    year_cur == YEARS_DEFAULT[panel_id] && 
	    month_cur == MONTHS_DEFAULT[panel_id] && 
	    day_cur == DAYS_DEFAULT[panel_id] ) { 

        	var year  = parseInt(yr_obj.value,10);
        	var month = parseInt(mo_obj.value,10);
        	var day   = parseInt(dy_obj.value,10);

        	var now = new Date(year,month-1,day);
        	now.setDate(now.getDate()-NDAY_TRMM);

		year  = now.getFullYear();
        	month = now.getMonth();
        	day   = now.getDate();

        	yr_obj.value = year;
        	mo_obj.selectedIndex = month ;
        	dy_obj.selectedIndex = day - 1;
		
		IS_TRMM_ONS[panel_id] = 1;
	}
	
}



////////////////////////////////////////////////////////////////////////////////////////////////////
//
// change product and popup layers/channels options if there are some
//
////////////////////////////////////////////////////////////////////////////////////////////////////
function changeProduct_template( alg_obj, sat_obj, prod_obj, layer_obj, layer_id, proj_id ) {
    
	var sat_value     = sat_obj.value;
	var alg_value     = alg_obj.value;
	var product_value = prod_obj.value; 
	
	// keep record of old value and then clear and make invisible
	var layer_value_old = ""; 
	if( document.getElementById(layer_id).className == "optionvisible" ) {
	    layer_value_old = layer_obj.value;
	}
		
 	layer_obj.options.length = 0; 
	document.getElementById(layer_id).className="optioninvisible";  
	layer_old_exist = 0;
	
	document.getElementById(proj_id).className="optioninvisible";  
	if ( product_value == 'em' && sat_value != 'trmm' ) {
	    document.getElementById(proj_id).className="optionvisible";
	}

	for ( isat=0; isat<satellite_list.length; isat++ ) {
	
	    if (satellite_list[isat].name == sat_value && satellite_list[isat].algorithm == alg_value ) {
	  	
		var products = satellite_list[isat].products.split(" ");
	    	
		for ( i=0; i<products.length; i++ )  {
		
		    var left = products[i].indexOf("(");
		    var prod_value = products[i] ;
		    if ( left != -1 ) { prod_value = products[i].substring(0,left);}
		    
		
		    if ( prod_value == product_value && left > 0 ) {
	   	        document.getElementById(layer_id).className="optionvisible";  
		
		    	var right = products[i].indexOf(")");
		    	var layer_str = products[i].substring(left+1,right);
		    	var layers = layer_str.split(":");
		    	
		    	for ( j=0; j<layers.length; j++ ) {
		    	    layer_obj.options[j] = new Option();
		    	    layer_obj.options[j].value = layers[j];
		    	    
			    // check whether old layer exist or not
			    if( layers[j] == layer_value_old ) { layer_old_exist = 1; }
			    
		    	    if ( prod_value == 'em' || prod_value == 'tbc' || prod_value == 'tbu' ) {
		    	        layer_obj.options[j].text  = 'ch'+String(j+1)+":"+String(layers[j]);
				document.getElementById(layer_id).title = "choose a channel";
			    }
			    else if( prod_value == 'temp' || prod_value == 'wv'    ||
			             prod_value == 'clwp' || prod_value == 'rainp' || prod_value == 'graupelp' ) {
				
				layer_obj.options[j].text  = String(layers[j]);     
				document.getElementById(layer_id).title = "choose a pressure layer";   
			    
			    }
			    else if( prod_value == 'sfc'  || prod_value == 'sfc2'   || prod_value == 'snow'   ||
			             prod_value == 'sice' || prod_value == 'sicefy' || prod_value == 'sicemy' ||
				     prod_value == 'gs'   || prod_value == 'swe' ) {
				
				layer_obj.options[j].text  = String(layers[j]);     
				document.getElementById(layer_id).title = PROJ_TITLE;
			    
			    }
		    	    else { 
		    	        layer_obj.options[j].text  = String(layers[j]);
			    }

		    	}
		    } 
	    	}
	    }
	}
	
	// if layer visible and layer old value does exist, then we use old value of layer
	if( document.getElementById(layer_id).className == "optionvisible" && layer_old_exist == 1 ) {
		layer_obj.value = layer_value_old;
	}
	
}


////////////////////////////////////////////////////////////////////////////////////////////////////
//
// is year a leap year ( 1 ) or not ( 0 )
//
////////////////////////////////////////////////////////////////////////////////////////////////////
function leapYear( year )                  
{                            
    div4   = year%4;
    div100 = year%100;
    div400 = year%400;

    if ((div4 == 0 && div100 != 0) || div400 == 0) { return 1; }
    
    return 0;    
}

    
////////////////////////////////////////////////////////////////////////////////////////////////////
//
// Pass in the year, month and day and return julian day
//
////////////////////////////////////////////////////////////////////////////////////////////////////
function julianDate( year, month, day )      
{           
    var JulianDate1 = new Array( 0, 31, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335 );
    var JulianDate2 = new Array( 0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334 );
    
    if ( leapYear(year) == 1 )  {
      return(JulianDate1[month-1] + day );
    }
    else {
      return(JulianDate2[month-1] + day );
    }
}


////////////////////////////////////////////////////////////////////////////////////////////////////
//
// get julian day after shift "step" days 
//
////////////////////////////////////////////////////////////////////////////////////////////////////
function shiftJulian(year,month,day,step) {
  
  var now = new Date(year,month-1,day);
  now.setDate(now.getDate()+step) ;
  
  var year_new  = now.getFullYear();
  var month_new = now.getMonth()+1; 
  var day_new   = now.getDate(); 
  
  var jday_new = julianDate( year_new, month_new, day_new ) ;
  return new Array(year_new,jday_new);
}


////////////////////////////////////////////////////////////////////////////////////////////////////
//
// shift number of "step" days and then return new year,new month and new day
//
////////////////////////////////////////////////////////////////////////////////////////////////////
function shift(year,month,day,step) {

  var now = new Date(year,month-1,day);
  now.setDate(now.getDate()+step) ;
  var year  = now.getFullYear();
  var month = now.getMonth()+1;
  var day   = now.getDate();

  var date = new Array(year,month,day);
  return date;
}


////////////////////////////////////////////////////////////////////////////////////////////////////
//
// return an image string
//
////////////////////////////////////////////////////////////////////////////////////////////////////
function loadImageHelper(alg,sat,prod,layer,proj,cend,sfc,region,yyyy,mm,dd) {

	if( sat == "gcomw1" ) {
	  yyyy="2012";
	  mm="11";
	  dd="01";
	}

	var ymd   = yyyy + "-" + mm + "-" + dd;
	var year  = parseInt(yyyy,10);
	var month = parseInt(mm,10);
	var day   = parseInt(dd,10);
	var jday  = julianDate(year, month, day);
	var jjj   = String(jday);
	
	if      ( jday < 10  ) jjj = "00" + jjj;
	else if ( jday < 100 ) jjj = "0"  + jjj;
	
	
	var image = ""; 
	 
	if ( alg == "her" ) {   // her, MSPPS

	  var layer_num = parseInt(layer);  // convert to numeric values for easy manipulation

	  if ( prod == "clw" || prod == "sice" || prod == "tpw" || prod == "ts" ) {
	    image=IMGDIR_HER+SAT2MSPPS[sat]+"/amsua/PROD/amsua_"+prod+"_"+yyyy+jjj+"_"+CENDTAG[cend]+".png";
	  }

	  else if ( prod == "iwp" || prod == "rr" || prod == "snow" || prod == "swe" || prod == "wet" ) {
	    image=IMGDIR_HER+SAT2MSPPS[sat]+"/mhs/PROD/mhs_"+prod+"_"+yyyy+jjj+"_"+CENDTAG[cend]+".png";
	  }

	  else if ( prod == "em" ) {
	    image=IMGDIR_HER+SAT2MSPPS[sat]+"/amsua/PROD/amsua_"+prod+EM2CHANNEL[layer]+"_"+yyyy+jjj+"_"+CENDTAG[cend]+".png";
	  }

	  else if ( prod == "at" && layer_num < 89 ) {
	    image=IMGDIR_HER+SAT2MSPPS[sat]+"/amsua/AT/amsua_"+prod+AT2CHANNEL[layer]+"_"+yyyy+jjj+"_"+CENDTAG[cend]+".png";
	  }

	  else if ( prod == "at" && layer_num >= 89 ) {
	    image=IMGDIR_HER+SAT2MSPPS[sat]+"/mhs/AT/mhs_"+prod+AT2CHANNEL[layer]+"_"+yyyy+jjj+"_"+CENDTAG[cend]+".png";
	  }

	}
	else {   // adv, MIRS

	  if   	(prod == "rrday" || prod == "rrlat" ) {
	    image=IMGDIR_ADV+"ipwg/mirs_adv_poes_glb_"+yyyy+mm+dd+"_"+prod+"_all_ad.png";
	  }
	  else if ( prod == "tbc"  || prod == "tbu"   || prod == "temp" || prod == "wv" 
	         || prod == "clwp" || prod == "rainp" || prod == "graupelp" ) {
  	    image=IMGDIR_ADV+sat+"/"+ymd+"_old/mirs_"+alg+SENSORTAG[sat]+region+"_"+yyyy+mm+dd+"_"+prod+"_"+layer+"_"+sfc+"_"+cend+".png";
	  }
	  else if ( prod == "em" ) {
  	    image=IMGDIR_ADV+sat+"/"+ymd+"_old/mirs_"+alg+SENSORTAG[sat]+region+"_"+yyyy+mm+dd+"_"+prod+"_"+layer+"_"+proj+sfc+"_"+cend+".png";
	    if( sat == "trmm" ) {
	      image=IMGDIR_ADV+sat+"/"+ymd+"_old/mirs_"+alg+SENSORTAG[sat]+region+"_"+yyyy+mm+dd+"_"+prod+"_"+layer+"_"+sfc+"_"+cend+".png";
	    }
	  }
	  
	  else if ( prod == "sfc" || prod == "sfc2" ) {
	    image=IMGDIR_ADV+sat+"/"+ymd+"_old/mirs_"+alg+SENSORTAG[sat]+region+"_"+yyyy+mm+dd+"_"+prod+"_"+layer+"_all_"+cend+".png";
	  }
	  else if ( prod == "sice" || prod == "sicefy" || prod == "sicemy" ) {
  	    image=IMGDIR_ADV+sat+"/"+ymd+"_old/mirs_"+alg+SENSORTAG[sat]+region+"_"+yyyy+mm+dd+"_"+prod+"_"+layer+"_sea_"+cend+".png";
	  } 
	  else if ( prod == "swe" || prod == "snow" || prod == "gs" ) {
  	    image=IMGDIR_ADV+sat+"/"+ymd+"_old/mirs_"+alg+SENSORTAG[sat]+region+"_"+yyyy+mm+dd+"_"+prod+"_"+layer+"_lnd_"+cend+".png";
	  } 
	  else if ( prod == "emspectrum" ) {
	    image=IMGDIR_ADV+sat+"/"+ymd+"_old/mirs_"+alg+SENSORTAG[sat]+"glb"+"_"+yyyy+mm+dd+"_"+prod+"_all_"+cend+".png";
	  }
	  else if ( prod == "wspd" ) {
	    image=IMGDIR_ADV+sat+"/"+ymd+"_old/mirs_"+alg+SENSORTAG[sat]+region+"_"+yyyy+mm+dd+"_"+prod+"_sea_"+cend+".png";
	  }
	  else    {
  	    image=IMGDIR_ADV+sat+"/"+ymd+"_old/mirs_"+alg+SENSORTAG[sat]+region+"_"+yyyy+mm+dd+"_"+prod+"_"+sfc+"_"+cend+".png";
	  }

	}

	return image;
}


////////////////////////////////////////////////////////////////////////////////////////////////////
//
// apply an image string to image object 
//
////////////////////////////////////////////////////////////////////////////////////////////////////
function image2Obj(img_str, img_obj, href_id) {
	img_obj.src = img_str; 
	document.getElementById(href_id).href = img_str;
	var index = img_str.lastIndexOf("/");
	var alt = img_str.substring(index+1,img_str.length);
	img_obj.alt = alt;
}


function loadImage1() {

	var alg1   = document.form.alg1.value;
	var sat1   = document.form.sat1.value;
	var prod1  = document.form.prod1.value;
	var layer1 = document.form.layer1.value;
	var proj1  = document.form.proj1.value;
	var cend1  = document.form.cend1.value;
	var sfc1   = document.form.sfc1.value;
	var region1= document.form.region1.value;
	var yyyy1  = document.form.yr1.value;
	var mm1    = document.form.mo1.value;
	var dd1    = document.form.dy1.value;
	
	var image1 = loadImageHelper(alg1,sat1,prod1,layer1,proj1,cend1,sfc1,region1,yyyy1,mm1,dd1)
	image2Obj(image1, document.form.img1, "href1"); 
}


function loadImage2() {

	var sat2   = document.form.sat2.value;
	var alg2   = document.form.alg2.value;
	var prod2  = document.form.prod2.value;
	var layer2 = document.form.layer2.value;
	var proj2  = document.form.proj2.value;
	var cend2  = document.form.cend2.value;
	var sfc2   = document.form.sfc2.value;
	var region2= document.form.region2.value;
	var yyyy2  = document.form.yr2.value;
	var mm2    = document.form.mo2.value;
	var dd2    = document.form.dy2.value;
	
	var image2 = loadImageHelper(alg2,sat2,prod2,layer2,proj2,cend2,sfc2,region2,yyyy2,mm2,dd2)
	image2Obj(image2, document.form.img2, "href2"); 
}


function loadImage3() {

	var sat3   = document.form.sat3.value;
	var alg3   = document.form.alg3.value;
	var prod3  = document.form.prod3.value;
	var layer3 = document.form.layer3.value;
	var proj3  = document.form.proj3.value;
	var cend3  = document.form.cend3.value;
	var sfc3   = document.form.sfc3.value;
	var region3= document.form.region3.value;
	var yyyy3  = document.form.yr3.value;
	var mm3    = document.form.mo3.value;
	var dd3    = document.form.dy3.value;
	
	var image3 = loadImageHelper(alg3,sat3,prod3,layer3,proj3,cend3,sfc3,region3,yyyy3,mm3,dd3)
	image2Obj(image3, document.form.img3, "href3"); 
}


function loadImage4() {

	var sat4   = document.form.sat4.value;
	var alg4   = document.form.alg4.value;
	var prod4  = document.form.prod4.value;
	var layer4 = document.form.layer4.value;
	var proj4  = document.form.proj4.value;
	var cend4  = document.form.cend4.value;
	var sfc4   = document.form.sfc4.value;
	var region4= document.form.region4.value;
	var yyyy4  = document.form.yr4.value;
	var mm4    = document.form.mo4.value;
	var dd4    = document.form.dy4.value;
	
	var image4 = loadImageHelper(alg4,sat4,prod4,layer4,proj4,cend4,sfc4,region4,yyyy4,mm4,dd4)
	image2Obj(image4, document.form.img4, "href4"); 
}


function loadInitialImages1( alg1,sat1,cend1,sfc1,region1,prod1,layer1,proj1,yr1,mo1,dy1 ) {
	
        populate_panel_template( "alg1", "sat1", "prod1", "layer1", "proj1", alg1, sat1, prod1, layer1, proj1 );          
	document.form.yr1.value = yr1 ;
	document.form.mo1.value = mo1 ;
	document.form.dy1.value = dy1 ;
	document.form.cend1.value = cend1;
	document.form.sfc1.value = sfc1;
	document.form.region1.value = region1;
	document.form.sat1.value = sat1;
	document.form.prod1.value = prod1;
	document.form.alg1.value = alg1;	
	document.form.proj1.value = proj1;	
	if ( layer1 != '' ) document.form.layer1.value = layer1;
	
	var image1 = loadImageHelper(alg1,sat1,prod1,layer1,proj1,cend1,sfc1,region1,yr1,mo1,dy1);
	image2Obj(image1, document.form.img1, "href1");
	
	YEARS_DEFAULT["1"] = document.form.yr1.value;
	MONTHS_DEFAULT["1"] = document.form.mo1.value;
	DAYS_DEFAULT["1"] = document.form.dy1.value;
}


function loadInitialImages( alg1,sat1,cend1,sfc1,region1,prod1,layer1,proj1,yr1,mo1,dy1,
			    alg2,sat2,cend2,sfc2,region2,prod2,layer2,proj2,yr2,mo2,dy2,
			    alg3,sat3,cend3,sfc3,region3,prod3,layer3,proj3,yr3,mo3,dy3,
			    alg4,sat4,cend4,sfc4,region4,prod4,layer4,proj4,yr4,mo4,dy4 )
{
	
        populate_panel_template( "alg1", "sat1", "prod1", "layer1", "proj1", alg1, sat1, prod1, layer1, proj1 );          
	document.form.yr1.value = yr1 ;
	document.form.mo1.value = mo1 ;
	document.form.dy1.value = dy1 ;
	document.form.cend1.value = cend1;
	document.form.sfc1.value = sfc1;
	document.form.region1.value = region1;
	document.form.sat1.value = sat1;
	document.form.prod1.value = prod1;
	document.form.alg1.value = alg1;	
	document.form.proj1.value = proj1;	
	if ( layer1 != '' ) document.form.layer1.value = layer1;
	var image1 = loadImageHelper(alg1,sat1,prod1,layer1,proj1,cend1,sfc1,region1,yr1,mo1,dy1);
	image2Obj(image1, document.form.img1, "href1"); 
	
        populate_panel_template( "alg2", "sat2", "prod2", "layer2", "proj2", alg2, sat2, prod2, layer2, proj2 );          
	document.form.yr2.value = yr2 ;
	document.form.mo2.value = mo2 ;
	document.form.dy2.value = dy2 ;
	document.form.cend2.value = cend2;
	document.form.sfc2.value = sfc2;
	document.form.region2.value = region2;
	document.form.sat2.value = sat2;
	document.form.prod2.value = prod2;
	document.form.alg2.value = alg2;	
	document.form.proj2.value = proj2;	
	if ( layer2 != '' ) document.form.layer2.value = layer2;
	var image2 = loadImageHelper(alg2,sat2,prod2,layer2,proj2,cend2,sfc2,region2,yr2,mo2,dy2);
	image2Obj(image2, document.form.img2, "href2"); 
	
        populate_panel_template( "alg3", "sat3", "prod3", "layer3", "proj3", alg3, sat3, prod3, layer3, proj3 );          
	document.form.yr3.value = yr3 ;
	document.form.mo3.value = mo3 ;
	document.form.dy3.value = dy3 ;
	document.form.cend3.value = cend3;
	document.form.sfc3.value = sfc3;
	document.form.region3.value = region3;
	document.form.sat3.value = sat3;
	document.form.prod3.value = prod3;
	document.form.alg3.value = alg3;	
	document.form.proj3.value = proj3;	
	if ( layer3 != '' ) document.form.layer3.value = layer3;
	var image3 = loadImageHelper(alg3,sat3,prod3,layer3,proj3,cend3,sfc3,region3,yr3,mo3,dy3);
	image2Obj(image3, document.form.img3, "href3"); 
	
        populate_panel_template( "alg4", "sat4", "prod4", "layer4", "proj4", alg4, sat4, prod4, layer4, proj4 );          
	document.form.yr4.value = yr4 ;
	document.form.mo4.value = mo4 ;
	document.form.dy4.value = dy4 ;
	document.form.cend4.value = cend4;
	document.form.sfc4.value = sfc4;
	document.form.region4.value = region4;
	document.form.sat4.value = sat4;
	document.form.prod4.value = prod4;
	document.form.alg4.value = alg4;	
	document.form.proj4.value = proj4;	
	if ( layer4 != '' ) document.form.layer4.value = layer4;
	var image4 = loadImageHelper(alg4,sat4,prod4,layer4,proj4,cend4,sfc4,region4,yr4,mo4,dy4);
	image2Obj(image4, document.form.img4, "href4"); 


	YEARS_DEFAULT["1"] = document.form.yr1.value;
	YEARS_DEFAULT["2"] = document.form.yr2.value;
	YEARS_DEFAULT["3"] = document.form.yr3.value;
	YEARS_DEFAULT["4"] = document.form.yr4.value;

	MONTHS_DEFAULT["1"] = document.form.mo1.value;
	MONTHS_DEFAULT["2"] = document.form.mo2.value;
	MONTHS_DEFAULT["3"] = document.form.mo3.value;
	MONTHS_DEFAULT["4"] = document.form.mo4.value;

	DAYS_DEFAULT["1"] = document.form.dy1.value;
	DAYS_DEFAULT["2"] = document.form.dy2.value;
	DAYS_DEFAULT["3"] = document.form.dy3.value;
	DAYS_DEFAULT["4"] = document.form.dy4.value;
}


function populate_panel_template( alg_id, sat_id, prod_id, layer_id, proj_id,  alg_value_arg, sat_value_arg, prod_value_arg, layer_value_arg, proj_value_arg ) {

	prod_obj  = document.getElementById(prod_id);
	layer_obj = document.getElementById(layer_id);
	
	prod_obj.options.length  = 0;
	layer_obj.options.length = 0;
	
	for ( isat=0; isat < satellite_list.length; isat++ ) {
	
	    if (satellite_list[isat].name == sat_value_arg && satellite_list[isat].algorithm == alg_value_arg ) {
	  	
		var products = satellite_list[isat].products.split(" ");
	    	
		for ( i=0; i<products.length; i++ )  {
		
		    var left = products[i].indexOf("(");
		    var prod_value = products[i] ;
		    if ( left != -1 ) { prod_value = products[i].substring(0,left);}
		    var prod_text = getProductText(prod_value);
		    
		    prod_obj.options[i]       = new Option();
		    prod_obj.options[i].value = prod_value;
		    prod_obj.options[i].text  = new String( prod_text );

	    	}
		
	    }
	
	}
	
	// Layers populate, which depend on product selected
	document.getElementById(layer_id).className="optioninvisible";  

	// Projctions populate, which depend on product selected is em or not
	document.getElementById(proj_id).className="optioninvisible";
	if ( prod_value_arg == "em" ) {
	    document.getElementById(proj_id).className="optionvisible";
	}

	for ( isat=0; isat<satellite_list.length; isat++ ) {
	
	    if (satellite_list[isat].name == sat_value_arg && satellite_list[isat].algorithm == alg_value_arg ) {
	  	
		var products = satellite_list[isat].products.split(" ");
	    	
		for ( i=0; i<products.length; i++ )  {
		
		    var left = products[i].indexOf("(");
		    var prod_value = products[i] ;
		    if ( left != -1 ) { prod_value = products[i].substring(0,left);}
		    
		    if ( prod_value == prod_value_arg && left > 0 ) {
	   	        document.getElementById(layer_id).className="optionvisible";  
		    	var right = products[i].indexOf(")");
		    	var layer_str = products[i].substring(left+1,right);
		    	var layers = layer_str.split(":");
		    	
		    	for ( j=0; j<layers.length; j++ ) {
		    	    layer_obj.options[j] = new Option();
		    	    layer_obj.options[j].value = layers[j];
		    	    //layer_obj.options[j].text  = String(layers[j]);
			    
			    // update title and text
		    	    if ( prod_value == 'em' || prod_value == 'tbc' || prod_value == 'tbu' ) {
		    	        layer_obj.options[j].text  = 'ch'+String(j+1)+":"+String(layers[j]);
				document.getElementById(layer_id).title = "choose a channel";
			    }
			    else if( prod_value == 'temp' || prod_value == 'wv'    ||
			             prod_value == 'clwp' || prod_value == 'rainp' || prod_value == 'graupelp' ) {
				
				layer_obj.options[j].text  = String(layers[j]);     
				document.getElementById(layer_id).title = "choose a pressure layer";   
			    
			    }
			    else if( prod_value == 'sfc'  || prod_value == 'sfc2'   || prod_value == 'snow'   ||
			             prod_value == 'sice' || prod_value == 'sicefy' || prod_value == 'sicemy' ||
				     prod_value == 'gs'   || prod_value == 'swe' ) {
				
				layer_obj.options[j].text  = String(layers[j]);     
				document.getElementById(layer_id).title = PROJ_TITLE;
			    
			    }
		    	    else { 
		    	        layer_obj.options[j].text  = String(layers[j]);
			    }
			    
		    	}
				
		    } 
	    	}
	    }
	}

}


function rev(yr,mo,dy,i) {

    var year  = parseInt(yr.value,10);
    var month = parseInt(mo.value,10);
    var day   = parseInt(dy.value,10);

    var now = new Date(year,month-1,day);
    now.setDate(now.getDate()-1);

    year  = now.getFullYear();
    month = now.getMonth();
    day   = now.getDate();

    yr.value = year;
    mo.selectedIndex = month ;
    dy.selectedIndex = day - 1;

    if      ( i == 1 ) loadImage1();
    else if ( i == 2 ) loadImage2();  
    else if ( i == 3 ) loadImage3();  
    else if ( i == 4 ) loadImage4();  
}


function fwd(yr,mo,dy,i) {

    var year  = parseInt(yr.value,10);
    var month = parseInt(mo.value,10);
    var day   = parseInt(dy.value,10);

    var now = new Date(year,month-1,day);
    now.setDate(now.getDate()+1);

    year  = now.getFullYear();
    month = now.getMonth();
    day   = now.getDate();

    var second = now.getTime()/1000;
    if( second <= SECOND) {
        yr.value = year;
        mo.selectedIndex = month ;
        dy.selectedIndex = day - 1;

        if      ( i == 1 ) loadImage1();
        else if ( i == 2 ) loadImage2();  
        else if ( i == 3 ) loadImage3();  
        else if ( i == 4 ) loadImage4();
    } 
}


function animate()
{
   //increment image number
   current_image++;                      
   if (current_image >= last_image)
	current_image = 0;

   //  check to ensure that current image has not been deselected from the loop
   //  if it has, then find the next image that hasn't been
   while (imageNum1[current_image] == false || 
   	  imageNum2[current_image] == false ||
   	  imageNum3[current_image] == false ||
   	  imageNum4[current_image] == false ) {
	
	current_image++;
	
	if (current_image >= last_image)
		current_image = 0;
   }
   
   // only if 4 of them are all available
   if (	imageNum1[current_image] == true && 
	imageNum2[current_image] == true &&
	imageNum3[current_image] == true &&
	imageNum4[current_image] == true ) 
   {
   	//display images onto screen only all available
	document.form.img1.src = images1[current_image].src; 
	document.form.img2.src = images2[current_image].src; 
	document.form.img3.src = images3[current_image].src; 
	document.form.img4.src = images4[current_image].src;
   
	// update image herf link 
	document.getElementById("href1").href = images1[current_image].src;   
	document.getElementById("href2").href = images2[current_image].src; 
	document.getElementById("href3").href = images3[current_image].src; 
	document.getElementById("href4").href = images4[current_image].src;
   
	// more staff need update here: yyyy, mm, dd of each panel
	document.form.yr1.selectedIndex = yyyy_indexes[current_image];
	document.form.mo1.selectedIndex = mm_indexes[current_image] ;
	document.form.dy1.selectedIndex = dd_indexes[current_image];
	
	document.form.yr2.selectedIndex = yyyy_indexes[current_image];
        document.form.mo2.selectedIndex = mm_indexes[current_image] ;
        document.form.dy2.selectedIndex = dd_indexes[current_image];
	
	document.form.yr3.selectedIndex = yyyy_indexes[current_image];
        document.form.mo3.selectedIndex = mm_indexes[current_image] ;
        document.form.dy3.selectedIndex = dd_indexes[current_image];

	document.form.yr4.selectedIndex = yyyy_indexes[current_image];
        document.form.mo4.selectedIndex = mm_indexes[current_image] ;
        document.form.dy4.selectedIndex = dd_indexes[current_image];
	
	// alt stuff update
	document.form.img1.alt = alts1[current_image];
	document.form.img2.alt = alts2[current_image];
	document.form.img3.alt = alts3[current_image];
	document.form.img4.alt = alts4[current_image];
   }
   
    
   // call "animate" again after a set time (delay) has elapsed
   timeID = setTimeout("animate()", delay);

}


////////////////////////////////////////////////////////////////////////////////////////////////////
//
// animate only panel 1
//
////////////////////////////////////////////////////////////////////////////////////////////////////
function animate1()
{
   //increment image number
   current_image++;                      
   if (current_image >= last_image)
	current_image = 0;

   //  check to ensure that current image has not been deselected from the loop
   //  if it has, then find the next image that hasn't been
   while (imageNum1[current_image] == false ) {
	current_image++;
	if (current_image >= last_image)
		current_image = 0;
   }
   
   // only if it's available
   if (	imageNum1[current_image] == true ) 
   {
   	//display images onto screen only all available
	document.form.img1.src = images1[current_image].src; 
   
	// update image herf link 
	document.getElementById("href1").href = images1[current_image].src;   
   
	// more staff need update here: yyyy, mm, dd of each panel
	document.form.yr1.selectedIndex = yyyy_indexes[current_image];
	document.form.mo1.selectedIndex = mm_indexes[current_image] ;
	document.form.dy1.selectedIndex = dd_indexes[current_image];
	
	// alt stuff update
	document.form.img1.alt = alts1[current_image];
   }
   
   // call "animate" again after a set time (delay) has elapsed
   timeID = setTimeout("animate1()", delay);

}


////////////////////////////////////////////////////////////////////////////////////////////////////
//
// Load animation images
//
////////////////////////////////////////////////////////////////////////////////////////////////////
function loadImages( alg,sat,prod,layer,proj,cend,sfc,region,year,month,day, theImages, img_obj, imageNum, alts ) 
{
  
   for (var i = first_image; i <= last_image; i++)
   {
      theImages[i-first_image] = new Image();
      
      var date      = shift(year,month,day,i-1);
      var year_tmp  = date[0];
      var month_tmp = date[1];
      var day_tmp   = date[2];
      
      var yyyy=year_tmp.toString();
      
      var mm=String(month_tmp); 
      if ( month_tmp<10 ) { mm="0"+mm; }
      
      var dd = day_tmp.toString();   
      if ( day_tmp<10   ) { dd="0"+dd; }
      
      var ymd = yyyy + "-" + mm + "-" + dd; 
      
      var yearjdy_tmp=shiftJulian(year,month,day,i-1);
      var jdy_tmp = yearjdy_tmp[1];
      var jjj     = String(jdy_tmp);
      if      ( jdy_tmp < 10 )   { jjj="00"+jdy_tmp; }
      else if ( jdy_tmp < 100  ) { jjj="0" +jdy_tmp; }

      //yyyy_indexes[i-first_image] = year_tmp - 2011;
      yyyy_indexes[i-first_image] = year_tmp - YEAR_START;
      mm_indexes[i-first_image]   = month_tmp - 1;
      dd_indexes[i-first_image]   = day_tmp - 1;

      var image_name = loadImageHelper(alg,sat,prod,layer,proj,cend,sfc,region,yyyy,mm,dd);
      theImages[i-first_image].src = image_name;
      
      imageNum[i-first_image] = true;
      
      var url = image_name;
      var index = url.lastIndexOf("/");
      var alt = url.substring(index+1,url.length); 
      alts[i-first_image] = alt;
   
   }

}



////////////////////////////////////////////////////////////////////////////////////////////////////
//
// launchAnimation for all panel case
//
////////////////////////////////////////////////////////////////////////////////////////////////////
function launchAnimation() 
{
   
   ///////////////////////////////////////////////
   // 4 panels are synchronized in time
   ///////////////////////////////////////////////
   var now = new Date();
   now.setDate(now.getDate()-31) ; // yesterday

   var year  = now.getFullYear();
   var month = now.getMonth()+1;  // month starting index is 0
   var day   = now.getDate();
   
   
   ///////////////////////////////////////////////
   // panel 1 elements
   ///////////////////////////////////////////////
   var sat  = document.form.sat1.value;
   var alg  = document.form.alg1.value;
   var prod = document.form.prod1.value;
   var layer= "";
   if (  document.form.layer1.className == "optionvisible" )
		layer = document.form.layer1.value;
   var cend = document.form.cend1.value;
   var sfc = document.form.sfc1.value;
   var region = document.form.region1.value;
   var proj = document.form.proj1.value;

   loadImages(alg,sat,prod,layer,proj,cend,sfc,region,year,month,day, images1, document.form.img1, imageNum1, alts1);
   
   
   ///////////////////////////////////////////////
   // panel 2 elements
   ///////////////////////////////////////////////
   sat  = document.form.sat2.value;
   alg  = document.form.alg2.value;
   prod = document.form.prod2.value;
   layer= "";
   if (  document.form.layer2.className == "optionvisible" )
		layer = document.form.layer2.value;
   cend = document.form.cend2.value;
   sfc  = document.form.sfc2.value;
   region = document.form.region2.value;
   var proj = document.form.proj2.value;

   loadImages(alg,sat,prod,layer,proj,cend,sfc,region,year,month,day, images2, document.form.img2, imageNum2, alts2);
   
   
   ///////////////////////////////////////////////
   // panel 3 elements
   ///////////////////////////////////////////////
   sat  = document.form.sat3.value;
   alg  = document.form.alg3.value;
   prod = document.form.prod3.value;
   layer= "";
   if (  document.form.layer3.className == "optionvisible" )
		layer = document.form.layer3.value;
   cend = document.form.cend3.value;
   sfc  = document.form.sfc3.value;
   region = document.form.region3.value;
   var proj = document.form.proj3.value;

   loadImages(alg,sat,prod,layer,proj,cend,sfc,region,year,month,day, images3, document.form.img3, imageNum3, alts3);
   
   
   ///////////////////////////////////////////////
   // panel 4 elements
   ///////////////////////////////////////////////
   sat  = document.form.sat4.value;
   alg  = document.form.alg4.value;
   prod = document.form.prod4.value;
   layer= "";
   if (  document.form.layer4.className == "optionvisible" )
		layer = document.form.layer4.value;
   cend = document.form.cend4.value;
   sfc  = document.form.sfc4.value;
   region = document.form.region4.value;
   var proj = document.form.proj4.value;

   loadImages(alg,sat,prod,layer,proj,cend,sfc,region,year,month,day, images4, document.form.img4, imageNum4, alts4);
   
   ///////////////////////////////////////////////
   // animate 4 panels
   ///////////////////////////////////////////////
      
   animate();
  
}


////////////////////////////////////////////////////////////////////////////////////////////////////
//
// launchAnimation for one panel case
//
////////////////////////////////////////////////////////////////////////////////////////////////////
function launchAnimation1() 
{
   
   ///////////////////////////////////////////////
   // 4 panels are synchronized in time
   ///////////////////////////////////////////////
   var now = new Date();
   now.setDate(now.getDate()-31) ; // yesterday

   var year  = now.getFullYear();
   var month = now.getMonth()+1;  // month starting index is 0
   var day   = now.getDate();
   
   
   ///////////////////////////////////////////////
   // panel 1 elements
   ///////////////////////////////////////////////
   var sat  = document.form.sat1.value;
   var alg  = document.form.alg1.value;
   var prod = document.form.prod1.value;
   var layer= "";
   if (  document.form.layer1.className == "optionvisible" )
		layer = document.form.layer1.value;
   var cend = document.form.cend1.value;
   var sfc  = document.form.sfc1.value;
   var region  = document.form.region1.value;
   var proj = document.form.proj1.value;

   loadImages(alg,sat,prod,layer,proj,cend,sfc,region,year,month,day, images1, document.form.img1, imageNum1, alts1);
   
   
   ///////////////////////////////////////////////
   // animate only panel 1
   ///////////////////////////////////////////////
      
   animate1();
  
}


// Stop animation
function stopAnimation() {
	clearTimeout (timeID);
}


// change panel formation, 1, 2X2, 4X1
function changePanel( count ) {
	if(      count == 2 ) document.form.action="product.php";
	else if( count == 1 ) document.form.action="product1.php"; 
	else if( count == 4 ) document.form.action="productv.php"; 
}



// if nday > 0, foward, if nday < 0, backward
function shiftDay( nday ) {

        var year  = parseInt(document.form.yr.value,10);
        var month = parseInt(document.form.mo.value,10);
        var day   = parseInt(document.form.dy.value,10);

        var now = new Date(year,month-1,day);
        now.setDate(now.getDate()+nday);

        year  = now.getFullYear();
        month = now.getMonth();
        day   = now.getDate();

        document.form.yr.value = year.toString();
        document.form.mo.selectedIndex = month ;
        document.form.dy.selectedIndex = day - 1;

        document.form.yr2.value = year.toString();
        document.form.mo2.selectedIndex = month ;
        document.form.dy2.selectedIndex = day - 1;

        loadImage();
}
