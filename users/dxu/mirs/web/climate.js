
//============================================================
//                >> jsImagePlayer 1.0 << 
//            for Netscape3.0+, September 1996
//============================================================
//                  by (c)BASTaRT 1996
//             Praha, Czech Republic, Europe
//
// feel free to copy and use as long as the credits are given
//          by having this header in the code
//
//          contact: xholecko@sgi.felk.cvut.cz
//          http://sgi.felk.cvut.cz/~xholecko
//
//============================================================
// Thanx to Karel & Martin for beta testing and suggestions!
//============================================================
//
// GUI modified by D. Watson and A. Earnhart (CIRA/CSU), 7/97
//
//============================================================


//============================================================
//
// modified by Wanchun Chen @ NOAA/ORA/NESDIS, 11/24/2008
// 
// 07/06/2011    Added TRMM TMI     Wanchun Chen
// 12/16/2011    Added NPP ATMS     Wanchun Chen
//============================================================


// define a satellite object with some fields
function satellite_object(satval, alg, products)
{
  this.satval = satval;
  this.alg = alg;
  this.products = products;
}

// Satellite and its product(layers,channels)

var satellite_n18_her   = new satellite_object("n18",    "mspps_", "clw iwp rr sice tpw ts");
var satellite_n18_adv   = new satellite_object("n18",    "mirs_", "clw em(23v:31v:50v:52v:53h:54h:54v:55h:57h1:57h2:57h3:57h4:57h5:57h6:89v1:89v2:157h:184h:186h:190v) gs iwp lwp rr rwp sfc(cyl:pn:ps) sice(cyl:pn:ps) sicefy(cyl:pn:ps) sicemy(cyl:pn:ps) snow(cyl:pn:ps) swe(cyl:pn:ps) tbc(23v:31v:50v:52v:53h:54h:54v:55h:57h1:57h2:57h3:57h4:57h5:57h6:89v1:89v2:157h:184h:186h:190v) tbu(23v:31v:50v:52v:53h:54h:54v:55h:57h1:57h2:57h3:57h4:57h5:57h6:89v1:89v2:157h:184h:186h:190v) temp(100mb:200mb:300mb:400mb:500mb:600mb:700mb:800mb:850mb:900mb:950mb) tpw ts wv(100mb:200mb:300mb:400mb:500mb:600mb:700mb:800mb:850mb:900mb:950mb)");

var satellite_metopA_her = new satellite_object("metopA", "mspps_", "clw iwp rr sice tpw ts");
var satellite_metopA_adv = new satellite_object("metopA", "mirs_", "clw em(23v:31v:50v:52v:53h:54h:54v:55h:57h1:57h2:57h3:57h4:57h5:57h6:89v1:89v2:157h:184h:186h:190v) gs iwp lwp rr rwp sfc(cyl:pn:ps) sice(cyl:pn:ps) sicefy(cyl:pn:ps) sicemy(cyl:pn:ps) snow(cyl:pn:ps) swe(cyl:pn:ps) tbc(23v:31v:50v:52v:53h:54h:54v:55h:57h1:57h2:57h3:57h4:57h5:57h6:89v1:89v2:157h:184h:186h:190v) tbu(23v:31v:50v:52v:53h:54h:54v:55h:57h1:57h2:57h3:57h4:57h5:57h6:89v1:89v2:157h:184h:186h:190v) temp(100mb:200mb:300mb:400mb:500mb:600mb:700mb:800mb:850mb:900mb:950mb) tpw ts wv(100mb:200mb:300mb:400mb:500mb:600mb:700mb:800mb:850mb:900mb:950mb)");

var satellite_metopB_her = new satellite_object("metopB", "mspps_", "clw iwp rr sice tpw ts");
var satellite_metopB_adv = new satellite_object("metopB", "mirs_", "clw em(23v:31v:50v:52v:53h:54h:54v:55h:57h1:57h2:57h3:57h4:57h5:57h6:89v1:89v2:157h:184h:186h:190v) gs iwp lwp rr rwp sfc(cyl:pn:ps) sice(cyl:pn:ps) sicefy(cyl:pn:ps) sicemy(cyl:pn:ps) snow(cyl:pn:ps) swe(cyl:pn:ps) tbc(23v:31v:50v:52v:53h:54h:54v:55h:57h1:57h2:57h3:57h4:57h5:57h6:89v1:89v2:157h:184h:186h:190v) tbu(23v:31v:50v:52v:53h:54h:54v:55h:57h1:57h2:57h3:57h4:57h5:57h6:89v1:89v2:157h:184h:186h:190v) temp(100mb:200mb:300mb:400mb:500mb:600mb:700mb:800mb:850mb:900mb:950mb) tpw ts wv(100mb:200mb:300mb:400mb:500mb:600mb:700mb:800mb:850mb:900mb:950mb)");

var satellite_n19_her   = new satellite_object("n19",    "mspps_", "clw iwp rr sice tpw ts");
var satellite_n19_adv   = new satellite_object("n19",    "mirs_", "clw em(23v:31v:50v:52v:53h:54h:54v:55h:57h1:57h2:57h3:57h4:57h5:57h6:89v1:89v2:157h:184h:186h:190v) gs iwp lwp rr rwp sfc(cyl:pn:ps) sice(cyl:pn:ps) sicefy(cyl:pn:ps) sicemy(cyl:pn:ps) snow(cyl:pn:ps) swe(cyl:pn:ps) tbc(23v:31v:50v:52v:53h:54h:54v:55h:57h1:57h2:57h3:57h4:57h5:57h6:89v1:89v2:157h:184h:186h:190v) tbu(23v:31v:50v:52v:53h:54h:54v:55h:57h1:57h2:57h3:57h4:57h5:57h6:89v1:89v2:157h:184h:186h:190v) temp(100mb:200mb:300mb:400mb:500mb:600mb:700mb:800mb:850mb:900mb:950mb) tpw ts wv(100mb:200mb:300mb:400mb:500mb:600mb:700mb:800mb:850mb:900mb:950mb)");

var satellite_f16_adv  = new satellite_object("f16", "mirs_", "clw em(50v:52v:53v:54v:55v:57rc:59rc:150h:190h:186h:184h:19h:19v:22v:37h:37v:91v:91h:63rc:60rc1:60rc2:60rc3:60rc4:60rc5) gs iwp lwp rr rwp sfc(cyl:pn:ps) sice(cyl:pn:ps) sicefy(cyl:pn:ps) sicemy(cyl:pn:ps) snow(cyl:pn:ps) swe(cyl:pn:ps) tbc(50v:52v:53v:54v:55v:57rc:59rc:150h:190h:186h:184h:19h:19v:22v:37h:37v:91v:91h:63rc:60rc1:60rc2:60rc3:60rc4:60rc5) tbu(50v:52v:53v:54v:55v:57rc:59rc:150h:190h:186h:184h:19h:19v:22v:37h:37v:91v:91h:63rc:60rc1:60rc2:60rc3:60rc4:60rc5) temp(100mb:200mb:300mb:400mb:500mb:600mb:700mb:800mb:850mb:900mb:950mb) tpw ts wv(100mb:200mb:300mb:400mb:500mb:600mb:700mb:800mb:850mb:900mb:950mb)");
var satellite_f18_adv  = new satellite_object("f18", "mirs_", "clw em(50h:52h:53h:54h:55h:57rc:59rc:150h:190h:186h:184h:19h:19v:22v:37h:37v:91v:91h:63rc:60rc1:60rc2:60rc3:60rc4:60rc5) gs iwp lwp rr rwp sfc(cyl:pn:ps) sice(cyl:pn:ps) sicefy(cyl:pn:ps) sicemy(cyl:pn:ps) snow(cyl:pn:ps) swe(cyl:pn:ps) tbc(50h:52h:53h:54h:55h:57rc:59rc:150h:190h:186h:184h:19h:19v:22v:37h:37v:91v:91h:63rc:60rc1:60rc2:60rc3:60rc4:60rc5) tbu(50h:52h:53h:54h:55h:57rc:59rc:150h:190h:186h:184h:19h:19v:22v:37h:37v:91v:91h:63rc:60rc1:60rc2:60rc3:60rc4:60rc5) temp(100mb:200mb:300mb:400mb:500mb:600mb:700mb:800mb:850mb:900mb:950mb) tpw ts wv(100mb:200mb:300mb:400mb:500mb:600mb:700mb:800mb:850mb:900mb:950mb)");

var satellite_trmm_adv = new satellite_object("trmm", "mirs_", "clw em(11v:11h:19v:19h:21v:37v:37h:85v:85h) iwp lwp rr rwp sfc(cyl:pn:ps) tbc(11v:11h:19v:19h:21v:37v:37h:85v:85h) tbu(11v:11h:19v:19h:21v:37v:37h:85v:85h) temp(100mb:200mb:300mb:400mb:500mb:600mb:700mb:800mb:850mb:900mb:950mb) tpw ts wv(100mb:200mb:300mb:400mb:500mb:600mb:700mb:800mb:850mb:900mb:950mb)");
var satellite_npp_adv = new satellite_object("npp", "mirs_", "clw em(23v:31v:50h:51h:52h:53h:54h1:54h2:55h:57h1:57h2:57h3:57h4:57h5:57h6:88v:165h:183h1:183h2:183h3:183h4:183h5) iwp lwp rr rwp sfc(cyl:pn:ps) tbc(23v:31v:50h:51h:52h:53h:54h1:54h2:55h:57h1:57h2:57h3:57h4:57h5:57h6:88v:165h:183h1:183h2:183h3:183h4:183h5) tbu(23v:31v:50h:51h:52h:53h:54h1:54h2:55h:57h1:57h2:57h3:57h4:57h5:57h6:88v:165h:183h1:183h2:183h3:183h4:183h5) temp(100mb:200mb:300mb:400mb:500mb:600mb:700mb:800mb:850mb:900mb:950mb) tpw ts wv(100mb:200mb:300mb:400mb:500mb:600mb:700mb:800mb:850mb:900mb:950mb)");

var satellite_list = new Array( 
			satellite_n18_her, 
			satellite_n18_adv, 
			satellite_metopA_her, 
			satellite_metopA_adv, 
			satellite_metopB_her, 
			satellite_metopB_adv, 
			satellite_f16_adv, 
			satellite_n19_her, 
			satellite_n19_adv, 
			satellite_f18_adv, 
			satellite_trmm_adv, 
			satellite_npp_adv); 



////////////////////////////////////////////////////////////////////////////////////////////////////
//
// return product text based on a value
//
////////////////////////////////////////////////////////////////////////////////////////////////////
function getProductText( val ) {
 
  var txt="";
  
  if 		( val=="at" )		txt = "Antenna Temperature" ; 
  else if 	( val=="chisq" )	txt = "Chi Square" ;	       
  else if 	( val=="clw" )	   	txt = "CLW" ;     
  else if 	( val=="em" )	   	txt = "Emissivity" ; 
  else if 	( val=="gs" )	   	txt = "Snow Grain Size" ;  
  else if 	( val=="iwp" )	   	txt = "Ice Water Path" ;
  else if 	( val=="lwp" )	   	txt = "Liquid Water Path" ;
  else if 	( val=="nattempt" )	txt = "Attempt Number" ;
  else if 	( val=="niter" )	txt = "Iteration Number" ;
  else if 	( val=="psfc" )	   	txt = "Surface Pressure" ;    
  else if 	( val=="qc" )	   	txt = "QC Flag" ;    
  else if 	( val=="rr" )	   	txt = "Rain Rate" ;   
  else if 	( val=="rrday" )	txt = "Precip Estimate" ;
  else if 	( val=="rrlat" )	txt = "Precip Lat Distri." ;
  else if 	( val=="rwp" )	   	txt = "Rain Water Path" ;     
  else if 	( val=="seaicvr" )   	txt = "Sea Ice Cover" ;
  else if 	( val=="sfc" )   	txt = "Surface Type" ;
  else if 	( val=="sfc2" )   	txt = "Post-Processed Sfc Type" ;
  else if 	( val=="sice" )   	txt = "Sea Ice" ;
  else if 	( val=="sicefy" )   	txt = "First Year SIC" ;
  else if 	( val=="sicemy" )   	txt = "Multiple Year SIC" ;
  else if 	( val=="snow" )    	txt = "Snow Cover" ;
  else if 	( val=="swe" )  	txt = "SWE" ;       
  else if 	( val=="windsp" )    	txt = "Surface Wind Speed" ;
  else if 	( val=="tbc" )    	txt = "Corr. TB" ;
  else if 	( val=="tbu" )    	txt = "UnCorr. TB" ;
  else if 	( val=="temp" )	   	txt = "Temp. Profile" ; 
  else if 	( val=="tpw" )	   	txt = "TPW" ;
  else if 	( val=="ts" )	   	txt = "Surface Temp." ;
  else if 	( val=="wet" )	   	txt = "Wetness Index" ; 
  else if 	( val=="wv" )	   	txt = "Water Vapor Profile" ; 

  return txt;

}



////////////////////////////////////////////////////////////////////////////////////////////////////
//
// animation parameters setup ( may need to modify for your own specification )
//
////////////////////////////////////////////////////////////////////////////////////////////////////
var YEAR_WEEK_ADJUSTMENT = new Array();

YEAR_WEEK_ADJUSTMENT["2009"] = 2;
YEAR_WEEK_ADJUSTMENT["2010"] = 3;
YEAR_WEEK_ADJUSTMENT["2011"] = 4;
YEAR_WEEK_ADJUSTMENT["2012"] = 0;
YEAR_WEEK_ADJUSTMENT["2013"] = 4;



var TIME_NOW  = new Date();
var YEAR_NOW  = TIME_NOW.getFullYear();
var MONTH_NOW = TIME_NOW.getMonth();
var DAY_NOW   = TIME_NOW.getDate();
var JDAY_NOW  = julianDate(YEAR_NOW, MONTH_NOW+1, DAY_NOW ) ; 

var FIRST_MONTH = 0;
var LAST_MONTH  = MONTH_NOW-1;
if( LAST_MONTH < 0 ) LAST_MONTH=0;

var FIRST_PEN = 0;
var LAST_PEN  = Math.floor( (JDAY_NOW-1) / 5 ) - 1;
if( LAST_PEN < 0 ) LAST_PEN=0;

var FIRST_WEEK = 0;
var LAST_WEEK  = Math.floor( (JDAY_NOW+2) / 7 ) - 1; // this +2 really depends on year
if( LAST_WEEK < 0 ) LAST_WEEK=0;

var IMGDIR = "images/" ; 
var image_type  = ".png";

// variables of upper panel
var first_image = FIRST_PEN;
var last_image  = LAST_PEN;
var theImages = new Array();      	//holds the images of MIRS
var imageNum  = new Array();       	//keeps track of which MIRS images to omit from loop
var normal_delay = 500;  
var delay = normal_delay;         	//delay between frames in 1/100 seconds
var delay_step = 100;             	//modify to 100 if need more changes each time you click on fast/slow button
var delay_max = 4000;
var delay_min = 10; 
var current_image = first_image; 	//number of the current image
var timeID = null;
var status = 0;                   	// 0-stopped, 1-playing
var play_mode = 1;                	// 1-forward, 2-backward

// variables of bottom panel
var first_image2 = FIRST_PEN;
var last_image2  = LAST_PEN;
var theImages2 = new Array();      	//holds the images of MSPPS
var imageNum2  = new Array();       	//keeps track of which MSPPS images to omit from loop
var normal_delay2 = 500;  
var delay2 = normal_delay2;         	//delay between frames in 1/100 seconds
var delay_step2 = 100;             	//modify to 100 if need more changes each time you click on fast/slow button
var delay_max2 = 4000;
var delay_min2 = 10; 
var current_image2 = first_image2; 	//number of the current image
var timeID2 = null;
var status2 = 0;                   	// 0-stopped, 1-playing
var play_mode2 = 1;                	// 1-forward, 2-backward



// gory detail for different sensors 

var penFirsts = new Array();
var penLasts = new Array();

var monFirsts = new Array();
var monLasts = new Array();


penFirsts['2008_n18'] = 64;
penFirsts['2009_n18'] = 0;
penFirsts['2010_n18'] = 0;
penFirsts['2011_n18'] = 0;
penFirsts['2012_n18'] = 0;

penFirsts['2008_n19'] = 0;
penFirsts['2009_n19'] = 9;
penFirsts['2010_n19'] = 0;
penFirsts['2011_n19'] = 0;
penFirsts['2012_n19'] = 0;

penFirsts['2008_metopA'] = 64;
penFirsts['2009_metopA'] = 0;
penFirsts['2010_metopA'] = 0;
penFirsts['2011_metopA'] = 0;
penFirsts['2012_metopA'] = 0;

penFirsts['2008_metopB'] = 0;
penFirsts['2009_metopB'] = 0;
penFirsts['2010_metopB'] = 0;
penFirsts['2011_metopB'] = 0;
penFirsts['2012_metopB'] = 58;

penFirsts['2008_f16'] = 64;
penFirsts['2009_f16'] = 0;
penFirsts['2010_f16'] = 0;
penFirsts['2011_f16'] = 0;
penFirsts['2012_f16'] = 0;

penFirsts['2008_f18'] = 0;
penFirsts['2009_f18'] = 0;
penFirsts['2010_f18'] = 10;
penFirsts['2011_f18'] = 0;
penFirsts['2012_f18'] = 0;

penFirsts['2008_npp'] = 0;
penFirsts['2009_npp'] = 0;
penFirsts['2010_npp'] = 0;
penFirsts['2011_npp'] = 64;
penFirsts['2012_npp'] = 0;

penFirsts['2008_trmm'] = 0;
penFirsts['2009_trmm'] = 0;
penFirsts['2010_trmm'] = 0;
penFirsts['2011_trmm'] = 63;
penFirsts['2012_trmm'] = 0;


penLasts['2008_n18'] = 72;
penLasts['2009_n18'] = 72;
penLasts['2010_n18'] = 72;
penLasts['2011_n18'] = 72;

penLasts['2008_n19'] = 72;
penLasts['2009_n19'] = 72;
penLasts['2010_n19'] = 72;
penLasts['2011_n19'] = 72;

penLasts['2008_metopA'] = 72;
penLasts['2009_metopA'] = 72;
penLasts['2010_metopA'] = 72;
penLasts['2011_metopA'] = 72;

penLasts['2008_metopB'] = 0;
penLasts['2009_metopB'] = 0;
penLasts['2010_metopB'] = 0;
penLasts['2011_metopB'] = 0;

penLasts['2008_f16'] = 0;
penLasts['2009_f16'] = 0;
penLasts['2010_f16'] = 0;
penLasts['2011_f16'] = 0;

penLasts['2008_f18'] = 0;
penLasts['2009_f18'] = 0;
penLasts['2010_f18'] = 0;
penLasts['2011_f18'] = 0;

penLasts['2008_npp'] = 0;
penLasts['2009_npp'] = 0;
penLasts['2010_npp'] = 0;
penLasts['2011_npp'] = 0;

penLasts['2008_trmm'] = 0;
penLasts['2009_trmm'] = 0;
penLasts['2010_trmm'] = 0;
penLasts['2011_trmm'] = 0;

penLasts['2012_n18']    = 72;
penLasts['2012_n19']    = 72;
penLasts['2012_metopA'] = 72;
penLasts['2012_metopB'] = 72;
penLasts['2012_f16']    = 72;
penLasts['2012_f18']    = 72;
penLasts['2012_npp']    = 72;
penLasts['2012_trmm']   = 72;


penFirsts['2013_n18']    = 0;
penFirsts['2013_n19']    = 0;
penFirsts['2013_metopA'] = 0;
penFirsts['2013_metopB'] = 0;
penFirsts['2013_f16']    = 0;
penFirsts['2013_f18']    = 0;
penFirsts['2013_npp']    = 0;
penFirsts['2013_trmm']   = 0;

penLasts['2013_n18']    = Math.floor( (JDAY_NOW-1) / 5 ) - 1;
penLasts['2013_n19']    = Math.floor( (JDAY_NOW-1) / 5 ) - 1;
penLasts['2013_metopA'] = Math.floor( (JDAY_NOW-1) / 5 ) - 1;
penLasts['2013_metopB'] = Math.floor( (JDAY_NOW-1) / 5 ) - 1;
penLasts['2013_f16']    = Math.floor( (JDAY_NOW-1) / 5 ) - 1;
penLasts['2013_f18']    = Math.floor( (JDAY_NOW-1) / 5 ) - 1;
penLasts['2013_npp']    = Math.floor( (JDAY_NOW-1) / 5 ) - 1;
penLasts['2013_trmm']   = Math.floor( (JDAY_NOW-1) / 5 ) - 1;



monFirsts['2008_n18'] = 10;
monFirsts['2009_n18'] = 0;
monFirsts['2010_n18'] = 0;
monFirsts['2011_n18'] = 0;
monFirsts['2012_n18'] = 0;

monFirsts['2008_n19'] = 0;
monFirsts['2009_n19'] = 1;
monFirsts['2010_n19'] = 0;
monFirsts['2011_n19'] = 0;
monFirsts['2012_n19'] = 0;

monFirsts['2008_metopA'] = 10;
monFirsts['2009_metopA'] = 0;
monFirsts['2010_metopA'] = 0;
monFirsts['2011_metopA'] = 0;
monFirsts['2012_metopA'] = 0;

monFirsts['2008_metopB'] = 0;
monFirsts['2009_metopB'] = 0;
monFirsts['2010_metopB'] = 0;
monFirsts['2011_metopB'] = 0;
monFirsts['2012_metopB'] = 0;

monFirsts['2008_f16'] = 10;
monFirsts['2009_f16'] = 0;
monFirsts['2010_f16'] = 0;
monFirsts['2011_f16'] = 0;
monFirsts['2012_f16'] = 0;

monFirsts['2008_f18'] = 0;
monFirsts['2009_f18'] = 0;
monFirsts['2010_f18'] = 1;
monFirsts['2011_f18'] = 0;
monFirsts['2012_f18'] = 0;

monFirsts['2008_npp'] = 0;
monFirsts['2009_npp'] = 0;
monFirsts['2010_npp'] = 0;
monFirsts['2011_npp'] = 10;
monFirsts['2012_npp'] = 0;

monFirsts['2008_trmm'] = 0;
monFirsts['2009_trmm'] = 0;
monFirsts['2010_trmm'] = 0;
monFirsts['2011_trmm'] = 10;
monFirsts['2012_trmm'] = 0;


monLasts['2008_n18'] = 11;
monLasts['2009_n18'] = 11;
monLasts['2010_n18'] = 11;
monLasts['2011_n18'] = 11;
monLasts['2012_n18'] = 11;

monLasts['2008_n19'] = 11;
monLasts['2009_n19'] = 11;
monLasts['2010_n19'] = 11;
monLasts['2011_n19'] = 11;
monLasts['2012_n19'] = 11;

monLasts['2008_metopA'] = 11;
monLasts['2009_metopA'] = 11;
monLasts['2010_metopA'] = 11;
monLasts['2011_metopA'] = 11;
monLasts['2012_metopA'] = 11;

monLasts['2008_metopB'] = 0;
monLasts['2009_metopB'] = 0;
monLasts['2010_metopB'] = 0;
monLasts['2011_metopB'] = 0;
monLasts['2012_metopB'] = 11;

monLasts['2008_f16'] = 11;
monLasts['2009_f16'] = 11;
monLasts['2010_f16'] = 11;
monLasts['2011_f16'] = 11;
monLasts['2012_f16'] = 11;

monLasts['2008_f18'] = 11;
monLasts['2009_f18'] = 11;
monLasts['2010_f18'] = 11;
monLasts['2011_f18'] = 11;
monLasts['2012_f18'] = 11;

monLasts['2008_npp'] = 11;
monLasts['2009_npp'] = 11;
monLasts['2010_npp'] = 11;
monLasts['2011_npp'] = 11;
monLasts['2012_npp'] = 11;

monLasts['2008_trmm'] = 11;
monLasts['2009_trmm'] = 11;
monLasts['2010_trmm'] = 11;
monLasts['2011_trmm'] = 11;
monLasts['2012_trmm'] = 11;



monFirsts['2013_n18']    = 0;
monFirsts['2013_n19']    = 0;
monFirsts['2013_metopA'] = 0;
monFirsts['2013_metopB'] = 0;
monFirsts['2013_f16']    = 0;
monFirsts['2013_f18']    = 0;
monFirsts['2013_npp']    = 0;
monFirsts['2013_trmm']   = 0;


monLasts['2013_n18']    = MONTH_NOW-1;
monLasts['2013_n19']    = MONTH_NOW-1;
monLasts['2013_metopA'] = MONTH_NOW-1;
monLasts['2013_metopB'] = MONTH_NOW-1;
monLasts['2013_f16']    = MONTH_NOW-1;
monLasts['2013_f18']    = MONTH_NOW-1;
monLasts['2013_npp']    = MONTH_NOW-1;
monLasts['2013_trmm']   = MONTH_NOW-1;



////////////////////////////////////////////////////////////////////////////////////////////////////
//
// six global parameters of first row of selection panel
//
////////////////////////////////////////////////////////////////////////////////////////////////////

var sat;
var prod;
var layer;
var resolution;
var climateType;
var alg;
var year;

var sat2;
var prod2;
var layer2;
var resolution2;
var climateType2;
var alg2;
var year2;


////////////////////////////////////////////////////////////////////////////////////////////////////
//
// set initial parameter values
//
////////////////////////////////////////////////////////////////////////////////////////////////////
function initialParameters()
{ 

  var now = new Date(); 
  year    = now.getFullYear();

  document.form1.sat.value	    = "n18" ; 
  document.form1.prod.value	    = "clw" ;	
  document.form1.layer.value	    = "" ;	
  document.form1.resolution.value   = "1.0deg" ; 
  document.form1.climateType.value  = "pentad" ;
  document.form1.alg.value	    = "mirs_" ;
  document.form1.year.value	    = year;
  
  first_image = FIRST_PEN;
  last_image  = LAST_PEN;
  
  document.form1.sat2.value	    = "n18" ; 
  document.form1.prod2.value        = "clw" ;   
  document.form1.layer2.value	    = "" ;	
  document.form1.resolution2.value  = "1.0deg" ; 
  document.form1.climateType2.value = "pentad" ;
  document.form1.alg2.value	    = "mspps_" ;
  document.form1.year2.value        = year;
  
  first_image2  = FIRST_PEN;
  last_image2   = LAST_PEN;

  sat	      = "n18" ; 
  prod	      = "clw" ;   
  layer	      = "" ;   
  resolution  = "1.0deg" ; 
  climateType = "pentad" ;
  alg	      = "mirs_" ;
  year	      = year;

  sat2	       = "n18" ; 
  prod2	       = "clw" ;   
  layer2       = "" ;   
  resolution2  = "1.0deg" ; 
  climateType2 = "pentad" ;
  alg2	       = "mspps_" ;
  year2	       = year;
  
  play_mode    = 1;
  play_mode2   = 1;
  
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

    if ( ( div4 == 0 && div100 != 0 ) || ( div400 == 0 ) ) { return 1; }
    
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
// return 73 pentad strings in a year
//
////////////////////////////////////////////////////////////////////////////////////////////////////
function getPentads( year ) {

  var pentads = new Array(73);
  
  var now = new Date();
  
  now.setFullYear(year);
  now.setMonth(0);
  now.setDate(1);
  
  var yyyy, mm, dd;
  
  for(i=0; i<73; i++ ) {
    
    yyyy = String(now.getFullYear());

    month = now.getMonth()+1;
    mm = String(month);
    if(month < 10 ) mm = '0' + mm ;
    
    day = now.getDate();
    dd  = String(day);
    if(day < 10 ) dd = '0' + dd ;
    
    ymd1 = yyyy + "-" + mm + "-" + dd ; 
    
    now.setDate(now.getDate()+4);
    
    yyyy = String(now.getFullYear());

    month = now.getMonth()+1;
    mm = String(month);
    if(month < 10 ) mm = '0' + mm ;
    
    day = now.getDate();
    dd  = String(day);
    if(day < 10 ) dd = '0' + dd ;
    
    ymd2 = yyyy + "-" + mm + "-" + dd ; 
   
    pentads[i] = ymd1 + "_" + ymd2;
    
    now.setDate(now.getDate()+1);
    
  } 
  
  return pentads;

} 


////////////////////////////////////////////////////////////////////////////////////////////////////
//
// return 12 month strings in a year: yyyy-mm
//
////////////////////////////////////////////////////////////////////////////////////////////////////
function getMonths( year ) {

  var months = new Array(12);
  
  var now = new Date();
  
  now.setFullYear(year);
  now.setMonth(0);
  now.setDate(1);
  
  var yyyy, mm ;
  
  for(i=0; i<12; i++ ) {
    
    yyyy = String(now.getFullYear());

    month = now.getMonth()+1;
    mm = String(month);
    if(month < 10 ) mm = '0' + mm ;
    
    ymd = yyyy + "-" + mm ;
    
    months[i] = ymd;
    
    now.setDate(now.getDate()+32);
    
  } 
  
  return months;

} 


////////////////////////////////////////////////////////////////////////////////////////////////////
//
// return 53 week strings in a year: 
//
////////////////////////////////////////////////////////////////////////////////////////////////////
function getWeeks( year ) {

  var weeks = new Array(53);
  
  var now = new Date();
  
  now.setFullYear(year);
  now.setMonth(0);
  now.setDate(1);
  
  //Sunday is 0, Monday is 1
  weekday = now.getDay();
  
  var yyyy, mm, dd;
  
  // special treatment of the first week
  yyyy = String(now.getFullYear());

  month = now.getMonth()+1;
  mm = String(month);
  if(month < 10 ) mm = '0' + mm ;
  
  day = now.getDate();
  dd  = String(day);
  if(day < 10 ) dd = '0' + dd ;
  
  ymd1 = yyyy + "-" + mm + "-" + dd ; 
  
  now.setDate(now.getDate()+(7-weekday));
  
  yyyy = String(now.getFullYear());

  month = now.getMonth()+1;
  mm = String(month);
  if(month < 10 ) mm = '0' + mm ;
  
  day = now.getDate();
  dd  = String(day);
  if(day < 10 ) dd = '0' + dd ;
  
  ymd2 = yyyy + "-" + mm + "-" + dd ; 
  
  weeks[0] = ymd1 + "_" + ymd2;
  
  now.setDate(now.getDate()+1);

  
  for(i=1; i<53; i++ ) {
    
    yyyy = String(now.getFullYear());

    month = now.getMonth()+1;
    mm = String(month);
    if(month < 10 ) mm = '0' + mm ;
    
    day = now.getDate();
    dd  = String(day);
    if(day < 10 ) dd = '0' + dd ;
    
    ymd1 = yyyy + "-" + mm + "-" + dd ; 
    
    
    // special treatment of last week
    now.setDate(now.getDate()+6);
    if( now.getFullYear() > year ) {
      ymd2=String(year)+"-12-31";
    }
    else {
      yyyy = String(now.getFullYear());

      month = now.getMonth()+1;
      mm = String(month);
      if(month < 10 ) mm = '0' + mm ;

      day = now.getDate();
      dd  = String(day);
      if(day < 10 ) dd = '0' + dd ;
      ymd2 = yyyy + "-" + mm + "-" + dd ; 
    }
    
    weeks[i] = ymd1 + "_" + ymd2;
    
    now.setDate(now.getDate()+1);
    
  } 
  
  return weeks;

} 


////////////////////////////////////////////////////////////////////////////////////////////////////
//
// Start the animation
//
////////////////////////////////////////////////////////////////////////////////////////////////////
function start() 
{
    if ( play_mode == 1 )
        timeID = setTimeout("animate_fwd()", delay);
    else
        timeID = setTimeout("animate_rev()", delay);
    status = 1;
}

function start2() 
{
    if ( play_mode2 == 1 )
   	timeID2 = setTimeout("animate_fwd2()", delay2);
    else
   	timeID2 = setTimeout("animate_rev2()", delay2);
    status2 = 1;
}


////////////////////////////////////////////////////////////////////////////////////////////////////
//
// Stop the animation
//
////////////////////////////////////////////////////////////////////////////////////////////////////
function stop() 
{
    //*****  cancel animation (timeID holds the expression which calls the fwd or bkwd function)
    if (status == 1) 
        clearTimeout (timeID);
    status = 0;
}

function stop2() 
{
    //*****  cancel animation (timeID holds the expression which calls the fwd or bkwd function)
    if (status2 == 1) 
        clearTimeout (timeID2);
    status2 = 0;
}



////////////////////////////////////////////////////////////////////////////////////////////////////
//
// Display animation in forward direction mode
//
////////////////////////////////////////////////////////////////////////////////////////////////////
function animate_fwd()
{
    //***** increment image number 
    current_image++;                      
    
    //***** change mode to 1
    play_mode = 1;

    //*****  check if current image has exceeded loop bound
    if (current_image > last_image) current_image = first_image; 
   
    //*****  check to ensure that current image has not been deselected from the loop
    //*****  if it has, then find the next image that hasn't been
    while (imageNum[current_image-first_image] == false) {
         current_image++;
         if (current_image > last_image) current_image = first_image;
    }  
                     
    document.animation.src  = theImages[current_image-first_image].src;
    var url = document.animation.src;
    var index = url.lastIndexOf("/");
    var alt = url.substring(index+1,url.length); 
    document.animation.alt = alt;
   
    //*****  call "animate_fwd()" again after a set time (delay) has elapsed 
    timeID = setTimeout("animate_fwd()", delay);
}

function animate_fwd2()
{   
    //***** increment image number
    current_image2++;
    
    //***** change mode to 1-forward
    play_mode2 = 1;

    //*****  check if current image has exceeded loop bound
    if (current_image2 > last_image2) current_image2 = first_image2; 
   
    //*****  check to ensure that current image has not been deselected from the loop
    //*****  if it has, then find the next image that hasn't been
    while (imageNum2[current_image2-first_image2] == false) {
        current_image2++;
        if (current_image2 > last_image2) current_image2 = first_image2; 
    }  
                     
    document.animation2.src = theImages2[current_image2-first_image2].src;
    var url = document.animation2.src;
    var index = url.lastIndexOf("/");
    var alt = url.substring(index+1,url.length); 
    document.animation2.alt = alt;
   
    //*****  call "animate_fwd2()" again after a set time (delay) has elapsed
    timeID2 = setTimeout("animate_fwd2()", delay2);
}


////////////////////////////////////////////////////////////////////////////////////////////////////
//
// Display animation in backward direction
//
////////////////////////////////////////////////////////////////////////////////////////////////////
function animate_rev()
{   
    //***** decrement image number
    current_image--;
    
    //**** change mode to 2
    play_mode = 2;

    //*****  check if image number is below lower loop bound
    if (current_image < first_image) current_image = last_image;      

    //*****  check to ensure that current image has not been deselected from the loop
    //*****  if it has, then find the next image that has not been
    while (imageNum[current_image-first_image] == false) {
        current_image--;
        if (current_image < first_image) current_image = last_image;
    }     
                        
    document.animation.src = theImages[current_image-first_image].src;
    var url = document.animation.src;
    var index = url.lastIndexOf("/");
    var alt = url.substring(index+1,url.length); 
    document.animation.alt = alt;
  
    //*****  call "animate_rev()" again after a set amount of time (delay) has elapsed
    timeID = setTimeout("animate_rev()", delay);
}

function animate_rev2() 
{
    //***** decrement image number
    current_image2--;

    //***** change mode to 2 - backward
    play_mode2 = 2;
   
    //*****  check if image number is before lower loop bound
    if( current_image2 < first_image2 ) current_image2 = last_image2;      

    //*****  check to ensure that current image has not been deselected from the loop
    //*****  if it has, then find the next image that has not been
    while( imageNum2[current_image2-first_image2] == false ) {
        current_image2--;
        if(current_image2 < first_image2) { current_image2 = last_image2; }
    }     
                        
    document.animation2.src = theImages2[current_image2-first_image2].src;
    var url = document.animation2.src;
    var index = url.lastIndexOf("/");
    var alt = url.substring(index+1,url.length); 
    document.animation2.alt = alt;
  
    //*****  call "animate_rev2()" again after a set amount of time (delay) has elapsed
    timeID2 = setTimeout("animate_rev2()", delay2);
}



////////////////////////////////////////////////////////////////////////////////////////////////////
//
// Changes playing speed by adding to or substracting from the delay between frames
//
////////////////////////////////////////////////////////////////////////////////////////////////////
function change_speed( dv ) 
{
    delay+=dv;
    //***** check to ensure max and min delay constraints have not been crossed
    if(delay > delay_max) delay = delay_max;
    if(delay < delay_min) delay = delay_min;
} 

function change_speed2( dv ) 
{
    delay2+=dv;
    //***** check to ensure max and min delay constraints have not been crossed
    if(delay2 > delay_max2) delay2 = delay_max2;
    if(delay2 < delay_min2) delay2 = delay_min2;
} 

////////////////////////////////////////////////////////////////////////////////////////////////////
//
// Increment to next image
//
////////////////////////////////////////////////////////////////////////////////////////////////////
function incrementImage( number )
{
    stop();

    //***** if image is last in loop, increment to first image
    if (number > last_image) number = first_image;

    //***** check to ensure that image has not been deselected from loop
    while (imageNum[number-first_image] == false ) {
          number++;
          if (number > last_image) number = first_image;
    }

    current_image = number;
    document.animation.src  = theImages[current_image-first_image].src;
    var url = document.animation.src;
    var index = url.lastIndexOf("/");
    var alt = url.substring(index+1,url.length); 
    document.animation.alt = alt;
    document.getElementById("href1").href = theImages[current_image-first_image].src;
}

function incrementImage2( number )
{
    stop2();

    //***** if image is last in loop, increment to first image
    if (number > last_image2) number = first_image2;

    //***** check to ensure that image has not been deselected from loop
    while (imageNum2[number-first_image2] == false) {
          number++;
          if (number > last_image2) number = first_image2;
    }

    current_image2 = number;
    document.animation2.src = theImages2[current_image2-first_image2].src;
    var url = document.animation2.src;
    var index = url.lastIndexOf("/");
    var alt = url.substring(index+1,url.length); 
    document.animation2.alt = alt;
    document.getElementById("href2").href = theImages2[current_image2-first_image2].src;
}


////////////////////////////////////////////////////////////////////////////////////////////////////
//
// Decrement to previous image
//
////////////////////////////////////////////////////////////////////////////////////////////////////
function decrementImage( number )
{
    stop();

    //***** if image is first in loop, decrement to last image
    if (number < first_image ) number = last_image;

    //*****  check to ensure that image has not been deselected from loop
    while (imageNum[number-first_image] == false) {
          number--;
          if (number < first_image) number = last_image; 
    }

    current_image = number;
    document.animation.src  = theImages[current_image-first_image].src;
    var url = document.animation.src;
    var index = url.lastIndexOf("/");
    var alt = url.substring(index+1,url.length); 
    document.animation.alt = alt;
    document.getElementById("href1").href = theImages[current_image-first_image].src;
}

function prev()
{
    stop();
    
    current_image = current_image - 1;
    if( current_image < first_image ) current_image = first_image; 
    //if( current_image < 0 ) current_image = 0; 
    
    document.animation.src  = theImages[current_image-first_image].src;
    var url = document.animation.src;
    var index = url.lastIndexOf("/");
    var alt = url.substring(index+1,url.length); 
    document.animation.alt = alt;
    document.getElementById("href1").href = theImages[current_image-first_image].src;
}



function decrementImage2( number )
{
    stop2();

    //***** if image is first in loop, decrement to last image
    if (number < first_image2) number = last_image2;

    //*****  check to ensure that image has not been deselected from loop
    while (imageNum2[current_image2-first_image2] == false) {
          number--;
          if (number < first_image2) number = last_image2; 
    }

    current_image2 = number;
    
    document.animation2.src = theImages2[current_image2-first_image2].src;
    var url = document.animation2.src;
    var index = url.lastIndexOf("/");
    var alt = url.substring(index+1,url.length); 
    document.animation2.alt = alt;
    document.getElementById("href2").href = theImages2[current_image2-first_image2].src;
}


////////////////////////////////////////////////////////////////////////////////////////////////////
//
// Play forward
//
////////////////////////////////////////////////////////////////////////////////////////////////////
function fwd() 
{
    stop();
    status = 1;
    play_mode = 1;
    animate_fwd();
}

function fwd2() 
{
    stop2();
    status2 = 1;
    play_mode2 = 1;
    animate_fwd2();
}


////////////////////////////////////////////////////////////////////////////////////////////////////
//
// Play backward
//
////////////////////////////////////////////////////////////////////////////////////////////////////
function rev() 
{
    stop();
    status = 1;
    play_mode = 2;
    animate_rev();
}

function rev2() 
{
    stop2();
    status2 = 1;
    play_mode2 = 2;
    animate_rev2();
}

////////////////////////////////////////////////////////////////////////////////////////////////////
//
// Help function to set first/last pentad/monthly
//
////////////////////////////////////////////////////////////////////////////////////////////////////
function setPenMon( year, sat ) {
	
	var key = ''+year+'_'+sat;
	
	FIRST_MONTH = monFirsts[key];
	LAST_MONTH  = monLasts[key];
	
	FIRST_PEN = penFirsts[key];
	LAST_PEN  = penLasts[key];
	
	if( FIRST_MONTH < 0 ) FIRST_MONTH=0;
	if( FIRST_PEN   < 0 ) FIRST_PEN=0;
	
	if( LAST_MONTH < 0 ) LAST_MONTH=0;
	if( LAST_PEN   < 0 ) LAST_PEN=0;
	
}


////////////////////////////////////////////////////////////////////////////////////////////////////
//
// Help function when algorithm or satellite changes
//
////////////////////////////////////////////////////////////////////////////////////////////////////
function changeSatellite( satval, layerID ) {
	
	stop();
	sat = satval;
	
	var prod_obj  = document.form1.prod ;
	var layer_obj = document.form1.layer ;
	var alg_obj   = document.form1.alg ;
	
    	if( satval == "f16" || satval == "f18" ) {
        	document.form1.alg.options.length = 0; 
        	document.form1.alg.options[0] = new Option();
        	document.form1.alg.options[0].value = "mirs_"
        	document.form1.alg.options[0].text  = "MIRS";
        	alg = "mirs_";
    	}
	else if(document.form1.alg.options.length != 2 ){
        	document.form1.alg.options.length = 0; 
        	document.form1.alg.options[0] = new Option();
        	document.form1.alg.options[0].value = "mirs_"
        	document.form1.alg.options[0].text  = "MIRS";
        	alg = "mirs_";
        	document.form1.alg.options[1] = new Option();
        	document.form1.alg.options[1].value = "mspps_"
        	document.form1.alg.options[1].text  = "MSPPS";
    	}
	
	// product drop down list update
	prod_obj.options.length = 0;
	var prod_old_exist = 0;  // to keep track whether new product list has old product or not
	
	for ( isat=0; isat < satellite_list.length; isat++ ) {
	    if (satellite_list[isat].satval == satval && satellite_list[isat].alg == alg ) {
		var products = satellite_list[isat].products.split(" ");
		for ( i=0; i<products.length; i++ )  {
		    var left = products[i].indexOf("(");
		    var prod_value = products[i] ;
		    if ( left != -1 ) { prod_value = products[i].substring(0,left); }
		    if ( prod_value == prod ) { prod_old_exist = 1; }
		    var prod_text = getProductText(prod_value);
		    prod_obj.options[i] 	    = new Option();
		    prod_obj.options[i].value	    = prod_value;
		    prod_obj.options[i].text	    = prod_text;
	    	}
	    }
	}
	
	// if populated product list has old product, we keep old one; otherwise, use 1st one in the list
	if ( prod_old_exist == 1 ) {
	    prod_obj.value = prod;
	}
	else {
	    prod = prod_obj.options[0].value;
	}



	// Layers, which depends on product selected
 	layer_obj.options.length = 0; 
	var layer_old_exist = 0;  // to keep track whether new layer list has old layer or not
	document.getElementById(layerID).className="optioninvisible";  
	for ( isat=0; isat<satellite_list.length; isat++ ) {
	    if (satellite_list[isat].satval == satval && satellite_list[isat].alg == alg ) {
		var products = satellite_list[isat].products.split(" ");
		for ( i=0; i<products.length; i++ )  {
		    var left = products[i].indexOf("(");
		    var prod_value = products[i] ;
		    if ( left != -1 ) { prod_value = products[i].substring(0,left);}

		    if ( prod_value == prod && left > 0 ) {
	   	        document.getElementById(layerID).className="optionvisible";  
		
		    	var right = products[i].indexOf(")");
		    	var layer_str = products[i].substring(left+1,right);
		    	var layers = layer_str.split(":");
		    	
		    	for ( j=0; j<layers.length; j++ ) {
		    	    layer_obj.options[j] = new Option();
		    	    layer_obj.options[j].value = layers[j];
			    layer_obj.options[j].text  = layers[j];
		    	    if ( layer == layers[j] ) { layer_old_exist=1; }
		    	}	
		    } 
	    	}
	    }
	}
	
	// if populated layer list has old layer, we use old one; otherwise, use the 1st one in the layer list
	if( layer_old_exist == 1 ) 
	{ layer_obj.value = layer; }
	else if( document.getElementById(layerID).className == "optionvisible" ) 
	{ layer = layer_obj.options[0].value; }
	
	setPenMon( year, sat );
    	loadImages();   
	
}


////////////////////////////////////////////////////////////////////////////////////////////////////
//
// change satellite
//
////////////////////////////////////////////////////////////////////////////////////////////////////

function changeSatellite2( satval, layerID ) { 
    
    stop2();
    sat2 = satval ;
    
    var prod_obj  = document.form1.prod2 ;
    var layer_obj = document.form1.layer2 ;
    var alg_obj   = document.form1.alg2 ;
	
    
    if( sat2 == "f16" || sat2 == "f18" ) {
        document.form1.alg2.options.length = 0; 
        document.form1.alg2.options[0] = new Option();
        document.form1.alg2.options[0].value = "mirs_"
        document.form1.alg2.options[0].text  = "MIRS";
        alg2 = "mirs_";
    }
    else if( document.form1.alg2.options.length != 2 ){
        document.form1.alg2.options.length = 0; 
        document.form1.alg2.options[0] = new Option();
        document.form1.alg2.options[0].value = "mirs_"
        document.form1.alg2.options[0].text  = "MIRS";
        document.form1.alg2.options[1] = new Option();
        document.form1.alg2.options[1].value = "mspps_"
        document.form1.alg2.options[1].text  = "MSPPS";
    }
    
	// product drop down list update
	prod_obj.options.length = 0;
	var prod_old_exist = 0;  // to keep track whether new product list has old product or not
	
	for ( isat=0; isat < satellite_list.length; isat++ ) {
	    if (satellite_list[isat].satval == satval && satellite_list[isat].alg == alg2 ) {
		var products = satellite_list[isat].products.split(" ");
		for ( i=0; i<products.length; i++ )  {
		    var left = products[i].indexOf("(");
		    var prod_value = products[i] ;
		    if ( left != -1 ) { prod_value = products[i].substring(0,left); }
		    if ( prod_value == prod2 ) { prod_old_exist = 1; }
		    var prod_text = getProductText(prod_value);
		    prod_obj.options[i] 	    = new Option();
		    prod_obj.options[i].value	    = prod_value;
		    prod_obj.options[i].text	    = prod_text;
	    	}
	    }
	}
	
	// if populated product list has old product, we keep old one; otherwise, use 1st one in the list
	if ( prod_old_exist == 1 ) {
	    prod_obj.value = prod2;
	}
	else {
	    prod2 = prod_obj.options[0].value;
	}



	// Layers, which depends on product selected
 	layer_obj.options.length = 0; 
	var layer_old_exist = 0;  // to keep track whether new layer list has old layer or not
	document.getElementById(layerID).className="optioninvisible";  
	for ( isat=0; isat<satellite_list.length; isat++ ) {
	    if (satellite_list[isat].satval == satval && satellite_list[isat].alg == alg2 ) {
		var products = satellite_list[isat].products.split(" ");
		for ( i=0; i<products.length; i++ )  {
		    var left = products[i].indexOf("(");
		    var prod_value = products[i] ;
		    if ( left != -1 ) { prod_value = products[i].substring(0,left);}

		    if ( prod_value == prod2 && left > 0 ) {
	   	        document.getElementById(layerID).className="optionvisible";  
		
		    	var right = products[i].indexOf(")");
		    	var layer_str = products[i].substring(left+1,right);
		    	var layers = layer_str.split(":");
		    	
		    	for ( j=0; j<layers.length; j++ ) {
		    	    layer_obj.options[j] = new Option();
		    	    layer_obj.options[j].value = layers[j];
			    layer_obj.options[j].text  = layers[j];
		    	    if ( layer2 == layers[j] ) { layer_old_exist=1; }
		    	}	
		    } 
	    	}
	    }
	}
	
	// if populated layer list has old layer, we use old one; otherwise, use the 1st one in the layer list
	if( layer_old_exist == 1 ) 
	{ layer_obj.value = layer2; }
	else if( document.getElementById(layerID).className == "optionvisible" ) 
	{ layer2 = layer_obj.options[0].value; }
	
	setPenMon( year2, sat2 );
    	loadImages2();   
} 


////////////////////////////////////////////////////////////////////////////////////////////////////
//
// change product
//
////////////////////////////////////////////////////////////////////////////////////////////////////
function changeProduct( prodval, layerID )
{
    stop();
    prod = prodval;

    var layer_obj = document.form1.layer;
    layer_obj.options.length = 0; 
    var layer_old_exist = 0;
    document.getElementById(layerID).className="optioninvisible";  
    
    for ( isat=0; isat<satellite_list.length; isat++ ) {

    	if (satellite_list[isat].satval == sat && satellite_list[isat].alg == alg ) {
    	    
    	    var products = satellite_list[isat].products.split(" ");
    	    
    	    for ( i=0; i<products.length; i++ )  {
    	    
    		var left = products[i].indexOf("(");
    		var prod_value = products[i] ;
    		if ( left != -1 ) { prod_value = products[i].substring(0,left);}
    	    
    		if ( prod_value == prod && left > 0 ) {
    		    document.getElementById(layerID).className="optionvisible";  
    	    
    		    var right = products[i].indexOf(")");
    		    var layer_str = products[i].substring(left+1,right);
    		    var layers = layer_str.split(":");
    		    
    		    for ( j=0; j<layers.length; j++ ) {
    			layer_obj.options[j] = new Option();
    			layer_obj.options[j].value = layers[j];
    			layer_obj.options[j].text  = layers[j];
			if ( layer == layers[j] ) { layer_old_exist=1; }
    		    }
    		} 
    	    }
    	}
    }
   
    if ( layer_old_exist == 1 ) 
    { layer_obj.value = layer; }
    else if ( document.getElementById(layerID).className == "optionvisible" ) 
    { layer = layer_obj.options[0].value;} 
    
    loadImages();

}	


function changeProduct2( prodval, layerID )
{	
    stop2();
    prod2 = prodval;
    
    var layer_obj = document.form1.layer2;
    layer_obj.options.length = 0; 
    var layer_old_exist = 0;
    document.getElementById(layerID).className="optioninvisible";  
    
    for ( isat=0; isat<satellite_list.length; isat++ ) {

    	if (satellite_list[isat].satval == sat2 && satellite_list[isat].alg == alg2 ) {
    	    
    	    var products = satellite_list[isat].products.split(" ");
    	    
    	    for ( i=0; i<products.length; i++ )  {
    	    
    		var left = products[i].indexOf("(");
    		var prod_value = products[i] ;
    		if ( left != -1 ) { prod_value = products[i].substring(0,left);}
    	    
    		if ( prod_value == prod2 && left > 0 ) {
    		    document.getElementById(layerID).className="optionvisible";  
    	    
    		    var right = products[i].indexOf(")");
    		    var layer_str = products[i].substring(left+1,right);
    		    var layers = layer_str.split(":");
    		    
    		    for ( j=0; j<layers.length; j++ ) {
    			layer_obj.options[j] = new Option();
    			layer_obj.options[j].value = layers[j];
    			layer_obj.options[j].text  = layers[j];
			if ( layer2 == layers[j] ) { layer_old_exist=1; }
    		    }
    		} 
    	    }
    	}
    }
   
    if ( layer_old_exist == 1 ) 
    { layer_obj.value = layer2; }
    else if ( document.getElementById(layerID).className == "optionvisible" ) 
    { layer2 = layer_obj.options[0].value;} 
	
    loadImages2();   
}


////////////////////////////////////////////////////////////////////////////////////////////////////
//
// change layers
//
////////////////////////////////////////////////////////////////////////////////////////////////////
function changeLayer( layerval )
{	stop();
  	layer = layerval;
	loadImages();   
}

function changeLayer2( layerval )
{	stop2();
  	layer2 = layerval;
	loadImages2();   
}


////////////////////////////////////////////////////////////////////////////////////////////////////
//
// change resolution ( 1.0 degree / 2.5 degree )
//
////////////////////////////////////////////////////////////////////////////////////////////////////
function changeResolution( resolutionval )
{
	stop();
  	resolution = resolutionval;
  	loadImages();   
}

function changeResolution2( resolutionval )
{
	stop2();
  	resolution2 = resolutionval;
  	loadImages2();   
}


////////////////////////////////////////////////////////////////////////////////////////////////////
//
// change climate type ( monthly/pentad/weekly,etc )
//
////////////////////////////////////////////////////////////////////////////////////////////////////
function changeClimateType( climateTypeVal )
{
	stop();
	climateType = climateTypeVal;
  	loadImages();   
}

function changeClimateType2( climateTypeVal )
{
	stop2();
	climateType2 = climateTypeVal;
  	loadImages2();   
}


////////////////////////////////////////////////////////////////////////////////////////////////////
//
// change algorithm(MIRS/MSPPS)
//
////////////////////////////////////////////////////////////////////////////////////////////////////
function changeAlg( algVal )
{
	stop();
	alg = algVal;

	if( alg == "mspps_" ) {
		document.form1.sat.options.length=0;
		
		document.form1.sat.options[0] = new Option();
		document.form1.sat.options[0].value = "n18";
		document.form1.sat.options[0].text  = "NOAA-18";
		
		document.form1.sat.options[1] = new Option();
		document.form1.sat.options[1].value = "n19";
		document.form1.sat.options[1].text  = "NOAA-19";
		
		document.form1.sat.options[2] = new Option();
		document.form1.sat.options[2].value = "metopA";
		document.form1.sat.options[2].text  = "METOP-A";
		
		document.form1.sat.options[3] = new Option();
		document.form1.sat.options[3].value = "metopB";
		document.form1.sat.options[3].text  = "METOP-B";
	}
	else {
		document.form1.sat.options.length=0;
		
		document.form1.sat.options[0] = new Option();
		document.form1.sat.options[0].value = "n18";
		document.form1.sat.options[0].text  = "NOAA-18";
		
		document.form1.sat.options[1] = new Option();
		document.form1.sat.options[1].value = "n19";
		document.form1.sat.options[1].text  = "NOAA-19";
		
		document.form1.sat.options[2] = new Option();
		document.form1.sat.options[2].value = "metopA";
		document.form1.sat.options[2].text  = "METOP-A";
		
		document.form1.sat.options[3] = new Option();
		document.form1.sat.options[3].value = "metopB";
		document.form1.sat.options[3].text  = "METOP-B";
		
		document.form1.sat.options[4] = new Option();
		document.form1.sat.options[4].value = "f16";
		document.form1.sat.options[4].text  = "F16 SSMI/S";
		
		document.form1.sat.options[5] = new Option();
		document.form1.sat.options[5].value = "f18";
		document.form1.sat.options[5].text  = "F18 SSMI/S";
	}
	
	changeSatellite( document.form1.sat.value, 'layer' );
	
	//loadImages();
}

function changeAlg2( algVal )
{
	stop2();
	alg2 = algVal;

	if( alg2 == "mspps_" ) {
		document.form1.sat2.options.length=0;
		
		document.form1.sat2.options[0] = new Option();
		document.form1.sat2.options[0].value = "n18";
		document.form1.sat2.options[0].text  = "NOAA-18";
		
		document.form1.sat2.options[1] = new Option();
		document.form1.sat2.options[1].value = "n19";
		document.form1.sat2.options[1].text  = "NOAA-19";
		
		document.form1.sat2.options[2] = new Option();
		document.form1.sat2.options[2].value = "metopA";
		document.form1.sat2.options[2].text  = "METOP-A";
		
		document.form1.sat2.options[3] = new Option();
		document.form1.sat2.options[3].value = "metopB";
		document.form1.sat2.options[3].text  = "METOP-B";
	}
	else {
		document.form1.sat2.options.length=0;
		
		document.form1.sat2.options[0] = new Option();
		document.form1.sat2.options[0].value = "n18";
		document.form1.sat2.options[0].text  = "NOAA-18";
		
		document.form1.sat2.options[1] = new Option();
		document.form1.sat2.options[1].value = "n19";
		document.form1.sat2.options[1].text  = "NOAA-19";
		
		document.form1.sat2.options[2] = new Option();
		document.form1.sat2.options[2].value = "metopA";
		document.form1.sat2.options[2].text  = "METOP-A";
		
		document.form1.sat2.options[3] = new Option();
		document.form1.sat2.options[3].value = "metopB";
		document.form1.sat2.options[3].text  = "METOP-B";
		
		document.form1.sat2.options[4] = new Option();
		document.form1.sat2.options[4].value = "f16";
		document.form1.sat2.options[4].text  = "F16 SSMI/S";
		
		document.form1.sat2.options[5] = new Option();
		document.form1.sat2.options[5].value = "f18";
		document.form1.sat2.options[5].text  = "F18 SSMI/S";
	}

	changeSatellite2( document.form1.sat2.value, 'layer2' );
	
	//loadImages2();
}


////////////////////////////////////////////////////////////////////////////////////////////////////
//
// change year
//
////////////////////////////////////////////////////////////////////////////////////////////////////
function changeYear( yearval )
{
	stop();
	setPenMon( yearval, sat );
	year = yearval;
  	loadImages();   
}

function changeYear2( yearval )
{
	stop2();
	setPenMon( yearval, sat2 );
	year2 = yearval;
  	loadImages2();   
}


////////////////////////////////////////////////////////////////////////////////////////////////////
//
// load images
//
////////////////////////////////////////////////////////////////////////////////////////////////////
function loadImages() 
{
  
  if( climateType == "pentad" )  // pentad
  { 
    var pentads = getPentads( year );  // get 73 pentads strings: yyyy-mm1-dd1_yyyy-mm2-dd2

    first_image   = FIRST_PEN;   
    last_image    = LAST_PEN;
    current_image = LAST_PEN;
    
    for (var i = first_image; i <= last_image; i++)
    {
        theImages[i-first_image]  = new Image();
	if ( prod == "em" || prod == "temp" || prod == "wv" || prod == "tbc" || prod == "tbu" || prod == "sfc" || 
	     prod == "sice" || prod == "sicemy" || prod == "sicefy" || prod == "snow" || prod == "swe" )
	{    
	    theImages[i-first_image].src  = IMGDIR+sat+'/climate/'+year+'/'+alg+sat+'_'+prod+'_avrg_'+resolution+'_'+pentads[i]+'_'+layer+image_type;
	}
	else
	{
            theImages[i-first_image].src  = IMGDIR+sat+'/climate/'+year+'/'+alg+sat+'_'+prod+'_avrg_'+resolution+'_'+pentads[i]+image_type;
        }
	
        imageNum[i-first_image] = true;
        document.animation.src  = theImages[i-first_image].src;
        var url = document.animation.src;
        var index = url.lastIndexOf("/");
        var alt = url.substring(index+1,url.length); 
        document.animation.alt = alt;
	document.getElementById("href1").href = url;
    }
  } 
  else if( climateType == "monthly" )  // monthly
  {
    var months = getMonths( year ) ; // get 12 months strings: yyyy-mm

    first_image   = FIRST_MONTH;
    last_image    = LAST_MONTH;
    current_image = LAST_MONTH;

    for (var i = first_image; i <= last_image; i++)
    {  
        theImages[i-first_image] = new Image();
	if ( prod == "em" || prod == "temp" || prod == "wv" || prod == "tbc" || prod == "tbu" || prod == "sfc" || 
	     prod == "sice" || prod == "sicemy" || prod == "sicefy" || prod == "snow" || prod == "swe" )
	{    
	    theImages[i-first_image].src = IMGDIR+sat+'/climate/'+year+'/'+alg+sat+'_'+prod+'_avrg_'+resolution+'_'+months[i]+'_'+layer+image_type;
	}
	else{
	    theImages[i-first_image].src = IMGDIR+sat+'/climate/'+year+'/'+alg+sat+'_'+prod+'_avrg_'+resolution+'_'+months[i]+image_type;
	}
        imageNum[i-first_image] = true;
        document.animation.src  = theImages[i-first_image].src;
        var url = document.animation.src;
        var index = url.lastIndexOf("/");
        var alt = url.substring(index+1,url.length); 
        document.animation.alt = alt;
	document.getElementById("href1").href = url;
    }
  } 
  else if( climateType == "weekly" )  // weekly
  {
    var weeks = getWeeks( year ) ; // get 53 weeks strings: yyyy-mm1-dd1_yyyy-mm2-dd2

    first_image   = FIRST_WEEK;
    last_image    = LAST_WEEK;
    current_image = LAST_WEEK;

    for (var i = first_image; i <= last_image; i++)
    {  
        theImages[i-first_image]  = new Image();
	if ( prod == "em" || prod == "temp" || prod == "wv" || prod == "tbc" || prod == "tbu" || prod == "sfc" ||
	     prod == "sice" || prod == "sicemy" || prod == "sicefy" || prod == "snow" || prod == "swe" )
	{    
            theImages[i-first_image].src = IMGDIR+sat+'/climate/'+year+'/'+alg+sat+'_'+prod+'_avrg_'+resolution+'_'+weeks[i]+'_'+layer+image_type;
	}
	else{
            theImages[i-first_image].src = IMGDIR+sat+'/climate/'+year+'/'+alg+sat+'_'+prod+'_avrg_'+resolution+'_'+weeks[i]+image_type;
	}
        imageNum[i-first_image] = true;
        document.animation.src  = theImages[i-first_image].src;
        var url = document.animation.src;
        var index = url.lastIndexOf("/");
        var alt = url.substring(index+1,url.length); 
        document.animation.alt = alt;
	document.getElementById("href1").href = url;
    }
  } 

  //if( play_mode == 1 ) fwd();
  //else                 rev();
  
}


function loadImages2()
{

  if( climateType2 == "pentad" )  // pentad
  { 
    var pentads = getPentads( year2 );  // get 73 pentads strings: yyyy-mm1-dd1_yyyy-mm2-dd2

    first_image2   = FIRST_PEN;
    last_image2    = LAST_PEN;
    current_image2 = LAST_PEN;
    
    for (var i = first_image2; i <= last_image2; i++)
    {
        theImages2[i-first_image2] = new Image();
        
	//theImages2[i-first_image2].src = IMGDIR+sat2+'/climate/'+alg2+sat2+'_'+prod2+'_avrg_'+resolution2+'_'+pentads[i]+image_type;
	if ( prod2 == "em" || prod2 == "temp" || prod2 == "wv" || prod2 == "tbc" || prod2 == "tbu" || prod2 == "sfc" ||
	     prod2 == "sice" || prod2 == "sicemy" || prod2 == "sicefy" || prod2 == "snow" || prod2 == "swe" )
	{    
	    theImages2[i-first_image2].src  = IMGDIR+sat2+'/climate/'+year2+'/'+alg2+sat2+'_'+prod2+'_avrg_'+resolution2+'_'+pentads[i]+'_'+layer2+image_type;
	}
	else
	{
            theImages2[i-first_image2].src  = IMGDIR+sat2+'/climate/'+year2+'/'+alg2+sat2+'_'+prod2+'_avrg_'+resolution2+'_'+pentads[i]+image_type;
        }
       
        imageNum2[i-first_image2] = true;
        document.animation2.src = theImages2[i-first_image2].src;
        var url = document.animation2.src;
        var index = url.lastIndexOf("/");
        var alt = url.substring(index+1,url.length); 
        document.animation2.alt = alt;
	document.getElementById("href2").href = url;
    }
  } 
  else if( climateType2 == "monthly" )  // monthly
  {
    var months = getMonths( year2 ) ; // get 12 months strings: yyyy-mm

    first_image2   = FIRST_MONTH;
    last_image2    = LAST_MONTH;
    current_image2 = LAST_MONTH;

    for (var i = first_image2; i <= last_image2; i++)
    {  
        theImages2[i-first_image2] = new Image();
	if ( prod2 == "em" || prod2 == "temp" || prod2 == "wv" || prod2 == "tbc" || prod2 == "tbu" || prod2 == "sfc" ||
	     prod2 == "sice" || prod2 == "sicemy" || prod2 == "sicefy" || prod2 == "snow" || prod2 == "swe" )
	{    
            theImages2[i-first_image2].src = IMGDIR+sat2+'/climate/'+year2+'/'+alg2+sat2+'_'+prod2+'_avrg_'+resolution2+'_'+months[i]+'_'+layer2+image_type;
	}
	else {
            theImages2[i-first_image2].src = IMGDIR+sat2+'/climate/'+year2+'/'+alg2+sat2+'_'+prod2+'_avrg_'+resolution2+'_'+months[i]+image_type;
	}
       	imageNum2[i-first_image2] = true;
       	document.animation2.src = theImages2[i-first_image2].src;
       	var url = document.animation2.src;
       	var index = url.lastIndexOf("/");
       	var alt = url.substring(index+1,url.length); 
       	document.animation2.alt = alt;
	document.getElementById("href2").href = url;
    }
  } 
  else if( climateType2 == "weekly" )  // weekly
  {
    var weeks = getWeeks( year2 ) ; // get 53 weeks strings: yyyy-mm1-dd1_yyyy-mm2-dd2

    first_image2   = FIRST_WEEK;
    last_image2    = LAST_WEEK;
    current_image2 = LAST_WEEK;

    for (var i = first_image2; i <= last_image2; i++)
    {  
        theImages2[i-first_image2] = new Image();
	if ( prod2 == "em" || prod2 == "temp" || prod2 == "wv" || prod2 == "tbc" || prod2 == "tbu" || prod2 == "sfc" ||
	     prod2 == "sice" || prod2 == "sicemy" || prod2 == "sicefy" || prod2 == "snow" || prod2 == "swe" )
	{    
            theImages2[i-first_image2].src = IMGDIR+sat2+'/climate/'+year2+'/'+alg2+sat2+'_'+prod2+'_avrg_'+resolution2+'_'+weeks[i]+'_'+layer2+image_type;
	}
	else {
            theImages2[i-first_image2].src = IMGDIR+sat2+'/climate/'+year2+'/'+alg2+sat2+'_'+prod2+'_avrg_'+resolution2+'_'+weeks[i]+image_type;
	}
        imageNum2[i-first_image2] = true;
        document.animation2.src = theImages2[i-first_image2].src;
        var url = document.animation2.src;
        var index = url.lastIndexOf("/");
        var alt = url.substring(index+1,url.length); 
        document.animation2.alt = alt;
	document.getElementById("href2").href = url;
    }
  } 
       
  //if( play_mode2 == 1 ) fwd2(); 
  //else                  rev2();

}
