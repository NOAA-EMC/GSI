
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
// modified by Wanchun Chen @ NOAA/NESDIS/STAR, 06/18/2007
// 		contact: Wanchun.Chen@noaa.gov
//
//============================================================




////////////////////////////////////////////////////////////////////////////////////////////////////
//
// animation parameters setup ( may need to modify for your own specification )
//
////////////////////////////////////////////////////////////////////////////////////////////////////
var IMGDIR = "images/";
var IMGDIRS = new Array(); 
IMGDIRS["n18"]    = "images/";
IMGDIRS["n19"]    = "images/";
IMGDIRS["metopA"] = "images/";
IMGDIRS["metopB"] = "images/";
IMGDIRS["f16"]    = "images/";
IMGDIRS["f18"]    = "images/";
IMGDIRS["npp"]    = "images/";
IMGDIRS["trmm"]   = "images/";

var IMGDIR_MSPPS="/corp/scsb/mspps/GIFs/";

var last_image  = 31;

var image_type  = ".png";
var first_image = 1;
var image_name  = "";
 
var animation_width  = 650;  		//width of the images in the animation  
var animation_height = 500;		//height of the images in the animation     

var theImages = new Array();      	//holds the images
var imageNum  = new Array();       	//keeps track of which images to omit from loop
var normal_delay = 500;  
var delay = normal_delay;         	//delay between frames in 1/100 seconds
var delay_step = 100;             	//modify to 100 if need more changes each time you click on fast/slow button
var delay_max = 4000;
var delay_min = 10; 
var current_image = first_image; 	//number of the current image
var timeID = null;
var status = 0;                   	// 0-stopped, 1-playing
var play_mode = 0;                	// 0-normal, 1-loop, 2-sweep
var size_valid = 0;


var SENSORTAG = new Array();
SENSORTAG["n18"]    = "mirs_adv_poes_n18_amsuamhs_";
SENSORTAG["n19"]    = "mirs_adv_poes_n19_amsuamhs_";
SENSORTAG["metopA"] = "mirs_adv_poes_metopA_amsuamhs_";
SENSORTAG["metopB"] = "mirs_adv_poes_metopB_amsuamhs_";
SENSORTAG["f16"]    = "mirs_adv_dmsp_f16_ssmis_";
SENSORTAG["f18"]    = "mirs_adv_dmsp_f18_ssmis_";
SENSORTAG["npp"]    = "mirs_adv_npoess_npp_atms_";
SENSORTAG["trmm"]   = "mirs_adv_eos_trmm_tmi_";

// MSPPS Sat Name to Dir mapping
var SAT2DIR = new Array();
SAT2DIR["n18"]    = "N18";
SAT2DIR["n19"]    = "N19";
SAT2DIR["metopA"] = "MOA";
SAT2DIR["metopB"] = "MOB";
SAT2DIR["f16"]    = "F16";
SAT2DIR["f18"]    = "F18";
SAT2DIR["npp"]    = "NPP";
SAT2DIR["trmm"]   = "TRMM";

var SATNAME= new Array();
SATNAME["n18"]    = "NOAA-18";
SATNAME["n19"]    = "NOAA-19";
SATNAME["metopA"] = "Metop-A";
SATNAME["metopB"] = "Metop-B";
SATNAME["f16"]    = "F16 SSMIS";
SATNAME["f18"]    = "F18 SSMI";
SATNAME["npp"]    = "NPP ATMS";
SATNAME["trmm"]   = "TRMM TMI";

var CEND = new Array();
CEND["as"] = "asc";
CEND["ds"] = "des";

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

var EM2CHANNEL = new Array();
EM2CHANNEL["23v"] = "1";
EM2CHANNEL["31v"] = "2";
EM2CHANNEL["50v"] = "3";


////////////////////////////////////////////////////////////////////////////////////////////////////
//
// Satellite and its product(layers,channels)
//
////////////////////////////////////////////////////////////////////////////////////////////////////

// define a satellite object with some fields
function satellite_object(name, algorithm, products)
{
    this.name = name;
    this.algorithm = algorithm;
    this.products = products;
}

var satellite_f16_her = new satellite_object("f16", "her", "clw em(19h:19v:22v:37h:37v:91h:91v) rwp seaicvr snwcvr tpw tskin windsp");
var satellite_f16_adv = new satellite_object("f16", "adv", "chisq clw em(50v:52v:53v:54v:55v:57rc:59rc:150h:190h:186h:184h:19h:19v:22v:37h:37v:91v:91h:63rc:60rc1:60rc2:60rc3:60rc4:60rc5) emspectrum gs(cyl:pn:ps) iwp lwp nattempt niter psfc qc rr rrday rrlat rwp sfc(cyl:pn:ps) sfc2(cyl:pn:ps) sice(cyl:pn:ps) sicemy(cyl:pn:ps) sicefy(cyl:pn:ps) snow(cyl:pn:ps) swe(cyl:pn:ps) tbc(50v:52v:53v:54v:55v:57rc:59rc:150h:190h:186h:184h:19h:19v:22v:37h:37v:91v:91h:63rc:60rc1:60rc2:60rc3:60rc4:60rc5) tbu(50v:52v:53v:54v:55v:57rc:59rc:150h:190h:186h:184h:19h:19v:22v:37h:37v:91v:91h:63rc:60rc1:60rc2:60rc3:60rc4:60rc5) temp(100mb:200mb:300mb:400mb:500mb:600mb:700mb:800mb:850mb:900mb:950mb) tpw tskin clwp(100mb:200mb:300mb:400mb:500mb:600mb:700mb:800mb:850mb:900mb:950mb) rainp(100mb:200mb:300mb:400mb:500mb:600mb:700mb:800mb:850mb:900mb:950mb) graupelp(100mb:200mb:300mb:400mb:500mb:600mb:700mb:800mb:850mb:900mb:950mb) wv(100mb:200mb:300mb:400mb:500mb:600mb:700mb:800mb:850mb:900mb:950mb)");

var satellite_n18_her = new satellite_object("n18", "her", "at(23:31:50:52:53:54:89:150:184:187:190) clw em(23v:31v:50v) sice tpw ts iwp rr snow swe wet");
var satellite_n18_adv = new satellite_object("n18", "adv", "chisq clw em(23v:31v:50v:52v:53h:54h:54v:55h:57h1:57h2:57h3:57h4:57h5:57h6:89v1:89v2:157h:184h:186h:190v) emspectrum gs(cyl:pn:ps) iwp lwp nattempt niter psfc qc rr rrday rrlat rwp sfc(cyl:pn:ps) sfc2(cyl:pn:ps) sice(cyl:pn:ps) sicemy(cyl:pn:ps) sicefy(cyl:pn:ps) snow(cyl:pn:ps) swe(cyl:pn:ps) tbc(23v:31v:50v:52v:53h:54h:54v:55h:57h1:57h2:57h3:57h4:57h5:57h6:89v1:89v2:157h:184h:186h:190v) tbu(23v:31v:50v:52v:53h:54h:54v:55h:57h1:57h2:57h3:57h4:57h5:57h6:89v1:89v2:157h:184h:186h:190v) temp(100mb:200mb:300mb:400mb:500mb:600mb:700mb:800mb:850mb:900mb:950mb) tpw tskin clwp(100mb:200mb:300mb:400mb:500mb:600mb:700mb:800mb:850mb:900mb:950mb) rainp(100mb:200mb:300mb:400mb:500mb:600mb:700mb:800mb:850mb:900mb:950mb) graupelp(100mb:200mb:300mb:400mb:500mb:600mb:700mb:800mb:850mb:900mb:950mb) wv(100mb:200mb:300mb:400mb:500mb:600mb:700mb:800mb:850mb:900mb:950mb)");

var satellite_n19_her = new satellite_object("n19", "her", "at(23:31:50:52:53:54:89:150:184:187:190) clw em(23v:31v:50v) sice tpw ts iwp rr snow swe wet");
var satellite_n19_adv = new satellite_object("n19", "adv", "chisq clw em(23v:31v:50v:52v:53h:54h:54v:55h:57h1:57h2:57h3:57h4:57h5:57h6:89v1:89v2:157h:184h:186h:190v) emspectrum gs(cyl:pn:ps) iwp lwp nattempt niter psfc qc rr rrday rrlat rwp sfc(cyl:pn:ps) sfc2(cyl:pn:ps) sice(cyl:pn:ps) sicemy(cyl:pn:ps) sicefy(cyl:pn:ps) snow(cyl:pn:ps) swe(cyl:pn:ps) tbc(23v:31v:50v:52v:53h:54h:54v:55h:57h1:57h2:57h3:57h4:57h5:57h6:89v1:89v2:157h:184h:186h:190v) tbu(23v:31v:50v:52v:53h:54h:54v:55h:57h1:57h2:57h3:57h4:57h5:57h6:89v1:89v2:157h:184h:186h:190v) temp(100mb:200mb:300mb:400mb:500mb:600mb:700mb:800mb:850mb:900mb:950mb) tpw tskin clwp(100mb:200mb:300mb:400mb:500mb:600mb:700mb:800mb:850mb:900mb:950mb) rainp(100mb:200mb:300mb:400mb:500mb:600mb:700mb:800mb:850mb:900mb:950mb) graupelp(100mb:200mb:300mb:400mb:500mb:600mb:700mb:800mb:850mb:900mb:950mb) wv(100mb:200mb:300mb:400mb:500mb:600mb:700mb:800mb:850mb:900mb:950mb)");

var satellite_metopA_her = new satellite_object("metopA", "her", "at(23:31:50:52:53:54:89:150:184:187:190) clw em(23v:31v:50v) sice tpw ts iwp rr snow swe wet");
var satellite_metopA_adv = new satellite_object("metopA", "adv", "chisq clw em(23v:31v:50v:52v:53h:54h:54v:55h:57h1:57h2:57h3:57h4:57h5:57h6:89v1:89v2:157h:184h:186h:190v) emspectrum gs(cyl:pn:ps) iwp lwp nattempt niter psfc qc rr rrday rrlat rwp sfc(cyl:pn:ps) sfc2(cyl:pn:ps) sice(cyl:pn:ps) sicemy(cyl:pn:ps) sicefy(cyl:pn:ps) snow(cyl:pn:ps) swe(cyl:pn:ps) tbc(23v:31v:50v:52v:53h:54h:54v:55h:57h1:57h2:57h3:57h4:57h5:57h6:89v1:89v2:157h:184h:186h:190v) tbu(23v:31v:50v:52v:53h:54h:54v:55h:57h1:57h2:57h3:57h4:57h5:57h6:89v1:89v2:157h:184h:186h:190v) temp(100mb:200mb:300mb:400mb:500mb:600mb:700mb:800mb:850mb:900mb:950mb) tpw tskin clwp(100mb:200mb:300mb:400mb:500mb:600mb:700mb:800mb:850mb:900mb:950mb) rainp(100mb:200mb:300mb:400mb:500mb:600mb:700mb:800mb:850mb:900mb:950mb) graupelp(100mb:200mb:300mb:400mb:500mb:600mb:700mb:800mb:850mb:900mb:950mb) wv(100mb:200mb:300mb:400mb:500mb:600mb:700mb:800mb:850mb:900mb:950mb)");

var satellite_metopB_her = new satellite_object("metopB", "her", "at(23:31:50:52:53:54:89:150:184:187:190) clw em(23v:31v:50v) sice tpw ts iwp rr snow swe wet");
var satellite_metopB_adv = new satellite_object("metopB", "adv", "chisq clw em(23v:31v:50v:52v:53h:54h:54v:55h:57h1:57h2:57h3:57h4:57h5:57h6:89v1:89v2:157h:184h:186h:190v) emspectrum gs(cyl:pn:ps) iwp lwp nattempt niter psfc qc rr rrday rrlat rwp sfc(cyl:pn:ps) sfc2(cyl:pn:ps) sice(cyl:pn:ps) sicemy(cyl:pn:ps) sicefy(cyl:pn:ps) snow(cyl:pn:ps) swe(cyl:pn:ps) tbc(23v:31v:50v:52v:53h:54h:54v:55h:57h1:57h2:57h3:57h4:57h5:57h6:89v1:89v2:157h:184h:186h:190v) tbu(23v:31v:50v:52v:53h:54h:54v:55h:57h1:57h2:57h3:57h4:57h5:57h6:89v1:89v2:157h:184h:186h:190v) temp(100mb:200mb:300mb:400mb:500mb:600mb:700mb:800mb:850mb:900mb:950mb) tpw tskin clwp(100mb:200mb:300mb:400mb:500mb:600mb:700mb:800mb:850mb:900mb:950mb) rainp(100mb:200mb:300mb:400mb:500mb:600mb:700mb:800mb:850mb:900mb:950mb) graupelp(100mb:200mb:300mb:400mb:500mb:600mb:700mb:800mb:850mb:900mb:950mb) wv(100mb:200mb:300mb:400mb:500mb:600mb:700mb:800mb:850mb:900mb:950mb)");

var satellite_f18_her = new satellite_object("f18", "her", "clw em(19h:19v:22v:37h:37v:91h:91v) rwp seaicvr snwcvr tpw tskin windsp");
var satellite_f18_adv = new satellite_object("f18", "adv", "chisq clw em(50h:52h:53h:54h:55h:57rc:59rc:150h:190h:186h:184h:19h:19v:22v:37h:37v:91v:91h:63rc:60rc1:60rc2:60rc3:60rc4:60rc5) emspectrum gs(cyl:pn:ps) iwp lwp nattempt niter psfc qc rr rrday rrlat rwp sfc(cyl:pn:ps) sfc2(cyl:pn:ps) sice(cyl:pn:ps) sicemy(cyl:pn:ps) sicefy(cyl:pn:ps) snow(cyl:pn:ps) swe(cyl:pn:ps) tbc(50h:52h:53h:54h:55h:57rc:59rc:150h:190h:186h:184h:19h:19v:22v:37h:37v:91v:91h:63rc:60rc1:60rc2:60rc3:60rc4:60rc5) tbu(50h:52h:53h:54h:55h:57rc:59rc:150h:190h:186h:184h:19h:19v:22v:37h:37v:91v:91h:63rc:60rc1:60rc2:60rc3:60rc4:60rc5) temp(100mb:200mb:300mb:400mb:500mb:600mb:700mb:800mb:850mb:900mb:950mb) tpw tskin clwp(100mb:200mb:300mb:400mb:500mb:600mb:700mb:800mb:850mb:900mb:950mb) rainp(100mb:200mb:300mb:400mb:500mb:600mb:700mb:800mb:850mb:900mb:950mb) graupelp(100mb:200mb:300mb:400mb:500mb:600mb:700mb:800mb:850mb:900mb:950mb) wv(100mb:200mb:300mb:400mb:500mb:600mb:700mb:800mb:850mb:900mb:950mb)");

var satellite_npp_adv  = new satellite_object("npp",   "adv", "chisq clw em(23v:31v:50h:51h:52h:53h:54h1:54h2:55h:57h1:57h2:57h3:57h4:57h5:57h6:88v:165h:183h1:183h2:183h3:183h4:183h5) gs(cyl:pn:ps) iwp lwp nattempt niter psfc qc rr rrday rrlat rwp sfc(cyl:pn:ps) sfc2(cyl:pn:ps) sice(cyl:pn:ps) sicemy(cyl:pn:ps) sicefy(cyl:pn:ps) snow(cyl:pn:ps) swe(cyl:pn:ps) tbc(23v:31v:50h:51h:52h:53h:54h1:54h2:55h:57h1:57h2:57h3:57h4:57h5:57h6:88v:165h:183h1:183h2:183h3:183h4:183h5) tbu(23v:31v:50h:51h:52h:53h:54h1:54h2:55h:57h1:57h2:57h3:57h4:57h5:57h6:88v:165h:183h1:183h2:183h3:183h4:183h5) temp(100mb:200mb:300mb:400mb:500mb:600mb:700mb:800mb:850mb:900mb:950mb) tpw tskin wv(100mb:200mb:300mb:400mb:500mb:600mb:700mb:800mb:850mb:900mb:950mb) clwp(100mb:200mb:300mb:400mb:500mb:600mb:700mb:800mb:850mb:900mb:950mb) rainp(100mb:200mb:300mb:400mb:500mb:600mb:700mb:800mb:850mb:900mb:950mb) graupelp(100mb:200mb:300mb:400mb:500mb:600mb:700mb:800mb:850mb:900mb:950mb)");
var satellite_trmm_adv = new satellite_object("trmm",  "adv", "chisq clw em(11v:11h:19v:19h:21v:37v:37h:85v:85h) iwp lwp nattempt niter psfc qc rr rwp sfc(cyl:pn:ps) tbc(11v:11h:19v:19h:21v:37v:37h:85v:85h) tbu(11v:11h:19v:19h:21v:37v:37h:85v:85h) temp(100mb:200mb:300mb:400mb:500mb:600mb:700mb:800mb:850mb:900mb:950mb) tpw tskin wv(100mb:200mb:300mb:400mb:500mb:600mb:700mb:800mb:850mb:900mb:950mb)");

var satellite_list = new Array(
			satellite_n18_her,
			satellite_n18_adv,
			satellite_metopA_her,
			satellite_metopA_adv,
			satellite_metopB_her,
			satellite_metopB_adv,
			satellite_f16_her,
			satellite_f16_adv,
			satellite_n19_her,
			satellite_n19_adv,
			satellite_f18_her,
			satellite_f18_adv,
			satellite_npp_adv,
			satellite_trmm_adv
			); 



////////////////////////////////////////////////////////////////////////////////////////////////////
//
// global parameter information from parent window document
//
////////////////////////////////////////////////////////////////////////////////////////////////////
var sat;
var prod;
var layer;
var region;
var cend;
var sfc;
var algorithm;
var year;
var month;
var day;


////////////////////////////////////////////////////////////////////////////////////////////////////
//
// set initial parameters
//
////////////////////////////////////////////////////////////////////////////////////////////////////
function initialParameters()
{ 
    sat                       = "n18" ; 
    prod                      = "tpw" ;   
    document.form1.prod.value = "tpw" ;
    layer                     = "200mb" ; 
    region                    = "glb" ; 
    cend                      = "as" ;
    sfc                       = "all" ;
    algorithm                 = "adv" ;

    var now = new Date(); 
    now.setDate(now.getDate() - 31) ; 	// back to 31 days

    year  = now.getFullYear();
    month = now.getMonth() + 1;		// month starting index is 0
    day   = now.getDate();
}


////////////////////////////////////////////////////////////////////////////////////////////////////
//
// is leap year or not
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
// shift number of "step" days and then return new year,new month and new day
//
////////////////////////////////////////////////////////////////////////////////////////////////////
function shift( year, month, day, step ) {

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
function shiftJulian( year, month, day, step ) {
  
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
// Start the animation
//
////////////////////////////////////////////////////////////////////////////////////////////////////
function start() 
{
    if ( play_mode == 0 )
   	 timeID = setTimeout("animate_fwd()", delay);
    else
   	 timeID = setTimeout("animate_rev()", delay);
    status = 1;
}


////////////////////////////////////////////////////////////////////////////////////////////////////
//
// Stop the animation
//
////////////////////////////////////////////////////////////////////////////////////////////////////
function stop() 
{
    // cancel animation (timeID holds the expression which calls the fwd or bkwd function)
    if (status == 1) 
       clearTimeout (timeID);
    status = 0;
}


////////////////////////////////////////////////////////////////////////////////////////////////////
//
// Display animation in fwd direction in either loop or sweep cend
//
////////////////////////////////////////////////////////////////////////////////////////////////////
function animate_fwd()
{
    current_image++;                      //increment image number

    //*****  check if current image has exceeded loop bound
    if (current_image > last_image) {
       if (play_mode == 1) {              //fwd loop cend - skip to first image 
          current_image = first_image; 
       }
       if (play_mode == 2) {              //sweep cend - change directions (go bkwd)
          current_image = last_image;
          animate_rev();
          return;
       }
    }  

    //*****  check to ensure that current image has not been deselected from the loop
    //*****  if it has, then find the next image that hasn't been
    while (imageNum[current_image-first_image] == false) {
          current_image++;
          if (current_image > last_image) { 
             if (play_mode == 1) 
        	current_image = first_image;
             if (play_mode == 2) {
        	current_image = last_image;
        	animate_rev();
        	return;
             }
          }
    }  

    document.animation.src = theImages[current_image-first_image].src;   //display image onto screen
    var url = document.animation.src;
    var index = url.lastIndexOf("/");
    var alt = url.substring(index+1,url.length); 
    document.animation.alt = alt;

    //*****  call "animate_fwd()" again after a set time (delay) has elapsed
    timeID = setTimeout("animate_fwd()", delay);
}


////////////////////////////////////////////////////////////////////////////////////////////////////
//
// Display animation in reverse direction
//
////////////////////////////////////////////////////////////////////////////////////////////////////
function animate_rev() 
{
    current_image--;                      //decrement image number

    //*****  check if image number is before lower loop bound
    if (current_image < first_image) {
      if (play_mode == 1) {               //rev loop cend - skip to last image
         current_image = last_image;      
      }
      if (play_mode == 2) {
         current_image = first_image;     //sweep cend - change directions (go fwd)
         animate_fwd();
         return;
      }
    }

    //*****  check to ensure that current image has not been deselected from the loop
    //*****  if it has, then find the next image that has not been
    while (imageNum[current_image-first_image] == false) {
          current_image--;
          if (current_image < first_image) {
             if (play_mode == 1) 
        	current_image = last_image;
             if (play_mode == 2) {
        	current_image = first_image;
                animate_fwd();
                return;
             }
          }
    }     

    document.animation.src = theImages[current_image-first_image].src;   //display image onto screen
    var url = document.animation.src;
    var index = url.lastIndexOf("/");
    var alt = url.substring(index+1,url.length); 
    document.animation.alt = alt;

    //*****  call "animate_rev()" again after a set amount of time (delay) has elapsed
    timeID = setTimeout("animate_rev()", delay);
}


////////////////////////////////////////////////////////////////////////////////////////////////////
//
// Changes playing speed by adding to or substracting from the delay between frames
//
////////////////////////////////////////////////////////////////////////////////////////////////////
function change_speed(dv) 
{
    delay+=dv;
    //***** check to ensure max and min delay constraints have not been crossed
    if(delay > delay_max) delay = delay_max;
    if(delay < delay_min) delay = delay_min;
} 


////////////////////////////////////////////////////////////////////////////////////////////////////
//
// Increment to next image
//
////////////////////////////////////////////////////////////////////////////////////////////////////
function incrementImage(number)
{
    stop();

    //***** if image is last in loop, increment to first image
    if (number > last_image) number = first_image;

    //***** check to ensure that image has not been deselected from loop
    while (imageNum[number-first_image] == false) {
          number++;
          if (number > last_image) number = first_image;
    }

    current_image = number;
    document.animation.src = theImages[current_image-first_image].src;   //display image 
    var url = document.animation.src;
    var index = url.lastIndexOf("/");
    var alt = url.substring(index+1,url.length); 
    document.animation.alt = alt;
}


////////////////////////////////////////////////////////////////////////////////////////////////////
//
// Decrement to next image
//
////////////////////////////////////////////////////////////////////////////////////////////////////
function decrementImage( number )
{
    stop();

    //***** if image is first in loop, decrement to last image
    if (number < first_image) number = last_image;

    //*****  check to ensure that image has not been deselected from loop
    while (imageNum[number-first_image] == false) {
          number--;
          if (number < first_image) number = last_image; 
    }

    current_image = number;
    document.animation.src = theImages[current_image-first_image].src;   //display image
    var url = document.animation.src;
    var index = url.lastIndexOf("/");
    var alt = url.substring(index+1,url.length); 
    document.animation.alt = alt;
}


////////////////////////////////////////////////////////////////////////////////////////////////////
//
// "Play forward"
//
////////////////////////////////////////////////////////////////////////////////////////////////////
function fwd() 
{
    stop();
    status = 1;
    animate_fwd();
}


////////////////////////////////////////////////////////////////////////////////////////////////////
//
// "Play reverse"
//
////////////////////////////////////////////////////////////////////////////////////////////////////
function rev() 
{
    stop();
    status = 1;
    animate_rev();
}


////////////////////////////////////////////////////////////////////////////////////////////////////
//
// Change play cend (normal, loop, swing)
//
////////////////////////////////////////////////////////////////////////////////////////////////////
function change_mode( modeval ) 
{
    play_mode = modeval;
    //launch();   
}


////////////////////////////////////////////////////////////////////////////////////////////////////
//
// change region
//
////////////////////////////////////////////////////////////////////////////////////////////////////
function changeRegion( regionval )
{
    region = regionval;    
    launch();   
}


////////////////////////////////////////////////////////////////////////////////////////////////////
//
// change sfc
//
////////////////////////////////////////////////////////////////////////////////////////////////////
function changeSfc( sfcval )
{
    sfc = sfcval;
    launch();   
}

////////////////////////////////////////////////////////////////////////////////////////////////////
//
// change cend(ascending/descending/combined)
//
////////////////////////////////////////////////////////////////////////////////////////////////////
function changeCend( cendval )
{
    cend = cendval;
    launch();   
}


////////////////////////////////////////////////////////////////////////////////////////////////////
//
// change layer
//
////////////////////////////////////////////////////////////////////////////////////////////////////
function changeLayer( layerval )
{
    layer = layerval;
    launch();   
}


////////////////////////////////////////////////////////////////////////////////////////////////////
//
// Load and initialize everything once page is downloaded (called from 'onLoad' in <BODY>)
//
////////////////////////////////////////////////////////////////////////////////////////////////////
function launch() 
{

 if ( cend != "ad" )
 { 
   last_image = 31;
   for (var i = first_image; i <= last_image; i++)
   {
      theImages[i-first_image] = new Image();
      
      var date      = shift(year,month,day,i-1);
      var year_tmp  = date[0];
      var month_tmp = date[1];
      var day_tmp   = date[2];
      
      var yyyy = String(year_tmp);
      
      var mm = String(month_tmp); 
      if ( month_tmp<10 ) { mm="0"+mm; }
      
      var dd=String(day_tmp);   
      if ( day_tmp<10   ) { dd="0"+dd; }
      
      var ymd = "/" + yyyy + "-" + mm + "-" + dd + "/";
      
      var yearjdy_tmp = shiftJulian(year,month,day,i-1);
      var jdy_tmp = yearjdy_tmp[1];
      var jjj     = String(jdy_tmp);
      if      ( jdy_tmp < 10 )   { jjj="00"+jdy_tmp; }
      else if ( jdy_tmp < 100  ) { jjj="0" +jdy_tmp; }

      if ( algorithm == "her" ) {
        
        layer_num = parseInt(layer);  // convert to numeric values for easy manipulation
        
        if ( prod == "clw" || prod == "sice" || prod == "tpw" || prod == "ts" ) {
          theImages[i-first_image].src = IMGDIR_MSPPS+SAT2DIR[sat]+"/amsua/PROD/amsua_"+prod+"_"+yyyy+jjj+"_"+CEND[cend]+image_type;
        }
        else if ( prod == "iwp" || prod == "rr" || prod == "snow" || prod == "swe" || prod == "wet" ) {
          theImages[i-first_image].src = IMGDIR_MSPPS+SAT2DIR[sat]+"/mhs/PROD/mhs_"+prod+"_"+yyyy+jjj+"_"+CEND[cend]+image_type;
        }
        else if ( prod == "em" ) {
          theImages[i-first_image].src = IMGDIR_MSPPS+SAT2DIR[sat]+"/amsua/PROD/amsua_"+prod+EM2CHANNEL[layer]+"_"+yyyy+jjj+"_"+CEND[cend]+image_type;       
        }
        else if ( prod == "at" && layer_num < 89 ) {
          theImages[i-first_image].src = IMGDIR_MSPPS+SAT2DIR[sat]+"/amsua/AT/amsua_"+prod+AT2CHANNEL[layer]+"_"+yyyy+jjj+"_"+CEND[cend]+image_type;
        }
        else if ( prod == "at" && layer_num >= 89 ) {
          theImages[i-first_image].src = IMGDIR_MSPPS+SAT2DIR[sat]+"/mhs/AT/mhs_"+prod+AT2CHANNEL[layer]+"_"+yyyy+jjj+"_"+CEND[cend]+image_type;
        }
      }
      else 
      {
        if ( prod == "rrday" || prod == "rrlat" ) {
          //theImages[i-first_image].src = IMGDIR + "ipwg/mirs_adv_poes_" + region+"_"+yyyy+mm+dd+"_"+prod+"_all_ad"+image_type;
          theImages[i-first_image].src = IMGDIR + "ipwg/mirs_adv_poes_" + "glb" +"_"+yyyy+mm+dd+"_"+prod+"_all_ad"+image_type;
        }
        else if ( prod == "em" || prod == "temp" || prod == "wv" || prod == "tbc" || prod == "tbu" || prod == "clwp" || prod == "rainp" || prod == "graupelp" ) {
          theImages[i-first_image].src = IMGDIRS[sat]+sat+ymd+SENSORTAG[sat]+region+"_"+yyyy+mm+dd+"_"+prod+"_"+layer+"_"+sfc+"_"+cend+image_type;
        }
        else if ( prod == "sfc" || prod == "sfc2" ) {
          theImages[i-first_image].src = IMGDIRS[sat]+sat+ymd+SENSORTAG[sat]+region+"_"+yyyy+mm+dd+"_"+prod+"_"+layer+"_all_"+cend+image_type;
        }
        else if ( prod == "swe" || prod == "snow" || prod == "gs" ) {
          theImages[i-first_image].src = IMGDIRS[sat]+sat+ymd+SENSORTAG[sat]+region+"_"+yyyy+mm+dd+"_"+prod+"_"+layer+"_lnd_"+cend+image_type;
        }
        else if ( prod == "sice" || prod == "sicefy" || prod == "sicemy" ) {
          theImages[i-first_image].src = IMGDIRS[sat]+sat+ymd+SENSORTAG[sat]+region+"_"+yyyy+mm+dd+"_"+prod+"_"+layer+"_sea_"+cend+image_type;
        }
        //else if ( prod == "gs" ) {
        //  theImages[i-first_image].src = IMGDIRS[sat]+sat+ymd+SENSORTAG[sat]+region+"_"+yyyy+mm+dd+"_"+prod+"_cyl_lnd_"+cend+image_type;
        //}
        else if ( prod == "emspectrum" ) {
          theImages[i-first_image].src = IMGDIRS[sat]+sat+ymd+SENSORTAG[sat]+"glb"+"_"+yyyy+mm+dd+"_"+prod+"_all_"+cend+image_type;
        }
        else {
          theImages[i-first_image].src = IMGDIRS[sat]+sat+ymd+SENSORTAG[sat]+region+"_"+yyyy+mm+dd+"_"+prod+"_"+sfc+"_"+cend+image_type;
        }
      }
      
      imageNum[i-first_image] = true;
      document.animation.src = theImages[i-first_image].src;
      
      var url = document.animation.src;
      var index = url.lastIndexOf("/");
      var alt = url.substring(index+1,url.length); 
      document.animation.alt = alt;
   }
 }  // end if cend != "ad" 
 else    // cend == "ad"
 {
   last_image = 62;  
  
   for (var i = first_image; i <= last_image/2; i++)
   {
      theImages[2*(i-first_image)]   = new Image();
      theImages[2*(i-first_image)+1] = new Image();
      
      var date      = shift(year,month,day,i-1);
      var year_tmp  = date[0];
      var month_tmp = date[1];
      var day_tmp   = date[2];
      
      var yyyy = String(year_tmp);
      
      var mm = String(month_tmp); 
      if ( month_tmp<10 ) { mm="0"+mm; }
      
      var dd=String(day_tmp);   
      if ( day_tmp<10   ) { dd="0"+dd; }
      
      var ymd = "/" + yyyy + "-" + mm + "-" + dd + "/";
      
      var yearjdy_tmp = shiftJulian(year,month,day,i-1);
      var jdy_tmp = yearjdy_tmp[1];
      var jjj     = String(jdy_tmp);
      if      ( jdy_tmp < 10 )   { jjj="00"+jdy_tmp; }
      else if ( jdy_tmp < 100  ) { jjj="0" +jdy_tmp; }

      if ( algorithm == "her" ) {
        
        layer_num = parseInt(layer);  // convert to numeric values for easy manipulation
        
        if ( prod == "clw" || prod == "sice" || prod == "tpw" || prod == "ts" ) {
          theImages[2*(i-first_image)].src    = IMGDIR_MSPPS+SAT2DIR[sat]+"/amsua/PROD/amsua_"+prod+"_"+yyyy+jjj+"_"+CEND["ds"]+image_type;
          theImages[2*(i-first_image)+1].src  = IMGDIR_MSPPS+SAT2DIR[sat]+"/amsua/PROD/amsua_"+prod+"_"+yyyy+jjj+"_"+CEND["as"]+image_type;
        }
        else if ( prod == "iwp" || prod == "rr" || prod == "snow" || prod == "swe" || prod == "wet" ) {
          theImages[2*(i-first_image)].src    = IMGDIR_MSPPS+SAT2DIR[sat]+"/mhs/PROD/mhs_"+prod+"_"+yyyy+jjj+"_"+CEND["ds"]+image_type;
          theImages[2*(i-first_image)+1].src  = IMGDIR_MSPPS+SAT2DIR[sat]+"/mhs/PROD/mhs_"+prod+"_"+yyyy+jjj+"_"+CEND["as"]+image_type;
        }        
        else if ( prod == "em" ) {
          theImages[2*(i-first_image)].src    = IMGDIR_MSPPS+SAT2DIR[sat]+"/amsua/PROD/amsua_"+prod+EM2CHANNEL[layer]+"_"+yyyy+jjj+"_"+CEND["ds"]+image_type;
          theImages[2*(i-first_image)+1].src  = IMGDIR_MSPPS+SAT2DIR[sat]+"/amsua/PROD/amsua_"+prod+EM2CHANNEL[layer]+"_"+yyyy+jjj+"_"+CEND["as"]+image_type;
        }
        else if ( prod == "at" && layer_num < 89 ) {
          theImages[2*(i-first_image)].src    = IMGDIR_MSPPS+SAT2DIR[sat]+"/amsua/AT/amsua_"+prod+AT2CHANNEL[layer]+"_"+yyyy+jjj+"_"+CEND["ds"]+image_type;
          theImages[2*(i-first_image)+1].src  = IMGDIR_MSPPS+SAT2DIR[sat]+"/amsua/AT/amsua_"+prod+AT2CHANNEL[layer]+"_"+yyyy+jjj+"_"+CEND["as"]+image_type;
        }
        else if ( prod == "at" && layer_num >= 89 ) {
          theImages[2*(i-first_image)].src    = IMGDIR_MSPPS+SAT2DIR[sat]+"/mhs/AT/mhs_"+prod+AT2CHANNEL[layer]+"_"+yyyy+jjj+"_"+CEND["ds"]+image_type;
          theImages[2*(i-first_image)+1].src  = IMGDIR_MSPPS+SAT2DIR[sat]+"/mhs/AT/mhs_"+prod+AT2CHANNEL[layer]+"_"+yyyy+jjj+"_"+CEND["as"]+image_type;
        }
      }
      else 
      {
        if ( prod == "rrday" || prod == "rrlat" ) {
          theImages[2*(i-first_image)].src   = IMGDIR + "ipwg/mirs_adv_poes_" + region+"_"+yyyy+mm+dd+"_"+prod+"_all_ad"+image_type;
          theImages[2*(i-first_image)+1].src = IMGDIR + "ipwg/mirs_adv_poes_" + region+"_"+yyyy+mm+dd+"_"+prod+"_all_ad"+image_type;
        }
        else if ( prod == "em" || prod == "temp" || prod == "wv" || prod == "tbc" || prod == "tbu" || prod == "clwp" || prod == "rainp" || prod == "graupelp" ) {
          theImages[2*(i-first_image)].src   = IMGDIRS[sat]+sat+ymd+SENSORTAG[sat]+region+"_"+yyyy+mm+dd+"_"+prod+"_"+layer+"_"+sfc+"_"+"ds"+image_type;
          theImages[2*(i-first_image)+1].src = IMGDIRS[sat]+sat+ymd+SENSORTAG[sat]+region+"_"+yyyy+mm+dd+"_"+prod+"_"+layer+"_"+sfc+"_"+"as"+image_type;
        }
        else if ( prod == "sfc" || prod == "sfc2" ) {
          theImages[2*(i-first_image)].src   = IMGDIRS[sat]+sat+ymd+SENSORTAG[sat]+region+"_"+yyyy+mm+dd+"_"+prod+"_"+layer+"_all_"+"ds"+image_type;
          theImages[2*(i-first_image)+1].src = IMGDIRS[sat]+sat+ymd+SENSORTAG[sat]+region+"_"+yyyy+mm+dd+"_"+prod+"_"+layer+"_all_"+"as"+image_type;
        }
        else if ( prod == "emspectrum" ) {
          theImages[2*(i-first_image)].src   = IMGDIRS[sat]+sat+ymd+SENSORTAG[sat]+"glb"+"_"+yyyy+mm+dd+"_"+prod+"_all_"+"ds"+image_type;
          theImages[2*(i-first_image)+1].src = IMGDIRS[sat]+sat+ymd+SENSORTAG[sat]+"glb"+"_"+yyyy+mm+dd+"_"+prod+"_all_"+"as"+image_type;
        }
	else if ( prod == "swe" || prod == "snow" ) {
          theImages[2*(i-first_image)].src   = IMGDIRS[sat]+sat+ymd+SENSORTAG[sat]+region+"_"+yyyy+mm+dd+"_"+prod+"_"+layer+"_lnd_"+"ds"+image_type;
          theImages[2*(i-first_image)+1].src = IMGDIRS[sat]+sat+ymd+SENSORTAG[sat]+region+"_"+yyyy+mm+dd+"_"+prod+"_"+layer+"_lnd_"+"as"+image_type;
        }
        else if ( prod == "sice"  ){
          theImages[2*(i-first_image)].src   = IMGDIRS[sat]+sat+ymd+SENSORTAG[sat]+region+"_"+yyyy+mm+dd+"_"+prod+"_"+layer+"_sea_"+"ds"+image_type;
          theImages[2*(i-first_image)+1].src = IMGDIRS[sat]+sat+ymd+SENSORTAG[sat]+region+"_"+yyyy+mm+dd+"_"+prod+"_"+layer+"_sea_"+"as"+image_type;
        }
        else if ( prod == "sicefy" || prod == "sicemy" ){
          theImages[2*(i-first_image)].src   = IMGDIRS[sat]+sat+ymd+SENSORTAG[sat]+region+"_"+yyyy+mm+dd+"_"+prod+"_sea_"+"ds"+image_type;
          theImages[2*(i-first_image)+1].src = IMGDIRS[sat]+sat+ymd+SENSORTAG[sat]+region+"_"+yyyy+mm+dd+"_"+prod+"_sea_"+"as"+image_type;
        }
        else if ( prod == "gs" ) {
          theImages[2*(i-first_image)].src   = IMGDIRS[sat]+sat+ymd+SENSORTAG[sat]+region+"_"+yyyy+mm+dd+"_"+prod+"_lnd_"+"ds"+image_type;
          theImages[2*(i-first_image)+1].src = IMGDIRS[sat]+sat+ymd+SENSORTAG[sat]+region+"_"+yyyy+mm+dd+"_"+prod+"_lnd_"+"as"+image_type;
        }
        else {
          theImages[2*(i-first_image)].src   = IMGDIRS[sat]+sat+ymd+SENSORTAG[sat]+region+"_"+yyyy+mm+dd+"_"+prod+"_"+sfc+"_"+"ds"+image_type;
          theImages[2*(i-first_image)+1].src = IMGDIRS[sat]+sat+ymd+SENSORTAG[sat]+region+"_"+yyyy+mm+dd+"_"+prod+"_"+sfc+"_"+"as"+image_type;
        }
      }
      
      imageNum[2*(i-first_image)] = true;
      document.animation.src = theImages[2*(i-first_image)].src;
      
      var url = document.animation.src;
      var index = url.lastIndexOf("/");
      var alt = url.substring(index+1,url.length); 
      document.animation.alt = alt;
   }
  
 } 
       
   // this needs to be done to set the right cend when the page is manually reloaded
   change_mode(1);
   fwd();
}


////////////////////////////////////////////////////////////////////////////////////////////////////
//
// return Product face text according to value
//
////////////////////////////////////////////////////////////////////////////////////////////////////
function getProductText( val ) {
 
    var txt = "";

    if     ( val == "at" )        txt = "Antenna Temperature" ; 
    else if( val == "chisq" )     txt = "Chi Square" ;		 
    else if( val == "clw" )       txt = "CLW" ;	   
    else if( val == "em" )        txt = "Emissivity" ; 
    else if( val == "emspectrum") txt = "Emissivity Spectrum" ; 
    else if( val == "gs" )        txt = "Snow Effective Grain Size" ;  
    else if( val == "iwp" )       txt = "Ice Water Path" ;
    else if( val == "lwp" )       txt = "Liquid Water Path" ;
    else if( val == "nattempt" )  txt = "Attempt Number" ;
    else if( val == "niter" )     txt = "Iteration Number" ;
    else if( val == "psfc" )      txt = "Surface Pressure" ;	 
    else if( val == "qc" )        txt = "QC Flag" ;      
    else if( val == "rr" )        txt = "Rain Rate" ;   
    else if( val == "rrday" )     txt = "Precip Estimate" ;
    else if( val == "rrlat" )     txt = "Precip Lat Distribution" ;
    else if( val == "rwp" )       txt = "Rain Water Path" ;	 
    else if( val == "seaicvr" )   txt = "Sea Ice Cover" ;
    else if( val == "sfc" )       txt = "Pre-Classif Surface Type" ;
    else if( val == "sfc2" )      txt = "Post-Process Surface Type" ;
    else if( val == "sice" )      txt = "Sea Ice Concentration" ;
    else if( val == "sicemy" )    txt = "Multiple Year SIC" ;
    else if( val == "sicefy" )    txt = "First Year SIC" ;
    else if( val == "snow" )      txt = "Snow Cover" ;
    else if( val == "swe" )       txt = "Snow Water Equivalent" ;   
    else if( val == "swp" )       txt = "Snow Water Path" ;	  
    else if( val == "tbc" )       txt = "Corr. TB" ;
    else if( val == "tbu" )       txt = "UnCorr. TB" ;
    else if( val == "temp" )      txt = "Temperature Profile" ; 
    else if( val == "tpw" )       txt = "TPW" ;
    else if( val == "ts" )        txt = "Surface Temperature" ;
    else if( val == "tskin" )     txt = "Skin Temperature" ;    
    else if( val == "wet" )       txt = "Wetness Index" ; 
    else if( val == "wv" )        txt = "Water Vapor Profile" ; 
    else if( val == "clwp" )      txt = "CLW Profile" ; 
    else if( val == "rainp" )     txt = "Rain Profile" ; 
    else if( val == "graupelp" )  txt = "Graupel Profile" ; 

    return txt;

}



////////////////////////////////////////////////////////////////////////////////////////////////////
//
// pupulate algorithm drop down list according to satelliete selected
//
////////////////////////////////////////////////////////////////////////////////////////////////////
function populateAlgorithm( satval ) { 

	document.form1.algorithm.options.length = 0;
	
	if( satval == 'n18' || satval == 'n19' || satval == 'metopA' || satval == 'metopB') {
	
		document.form1.algorithm.options[0] = new Option( );
		document.form1.algorithm.options[0].value = 'adv';
		document.form1.algorithm.options[0].text  = '1DVAR';
		
		document.form1.algorithm.options[1] = new Option( );
		document.form1.algorithm.options[1].value = 'her';
		document.form1.algorithm.options[1].text  = 'MSPPS';
	}
	else {
		document.form1.algorithm.options[0] = new Option( );
		document.form1.algorithm.options[0].value = 'adv';
		document.form1.algorithm.options[0].text  = '1DVAR';
	}

}



////////////////////////////////////////////////////////////////////////////////////////////////////
//
// pupulate algorithm drop down list according to satelliete selected
//
////////////////////////////////////////////////////////////////////////////////////////////////////
function populateSatellite( algval ) { 

	
	document.form1.sat.options.length = 0;
	
	if( algval == 'her' ) {
		
		document.form1.sat.options[0] = new Option( );
		document.form1.sat.options[0].value = 'n18';
		document.form1.sat.options[0].text  = SATNAME['n18']
		
		document.form1.sat.options[1] = new Option( );
		document.form1.sat.options[1].value = 'n19';
		document.form1.sat.options[1].text  = SATNAME['n19']

		document.form1.sat.options[2] = new Option( );
		document.form1.sat.options[2].value = 'metopA';
		document.form1.sat.options[2].text  = SATNAME['metopA']

		document.form1.sat.options[3] = new Option( );
		document.form1.sat.options[3].value = 'metopB';
		document.form1.sat.options[3].text  = SATNAME['metopB']
	}
	else {
	
		document.form1.sat.options[0] = new Option( );
		document.form1.sat.options[0].value = 'n18';
		document.form1.sat.options[0].text  = SATNAME['n18']
		
		document.form1.sat.options[1] = new Option( );
		document.form1.sat.options[1].value = 'n19';
		document.form1.sat.options[1].text  = SATNAME['n19']

		document.form1.sat.options[2] = new Option( );
		document.form1.sat.options[2].value = 'metopA';
		document.form1.sat.options[2].text  = SATNAME['metopA']

		document.form1.sat.options[3] = new Option( );
		document.form1.sat.options[3].value = 'metopB';
		document.form1.sat.options[3].text  = SATNAME['metopB']

		document.form1.sat.options[4] = new Option( );
		document.form1.sat.options[4].value = 'f16';
		document.form1.sat.options[4].text  = SATNAME['f16']
		
		document.form1.sat.options[5] = new Option( );
		document.form1.sat.options[5].value = 'f18';
		document.form1.sat.options[5].text  = SATNAME['f18']

		document.form1.sat.options[6] = new Option( );
		document.form1.sat.options[6].value = 'npp';
		document.form1.sat.options[6].text  = SATNAME['npp']

		document.form1.sat.options[7] = new Option( );
		document.form1.sat.options[7].value = 'trmm';
		document.form1.sat.options[7].text  = SATNAME['trmm']

	}

}



////////////////////////////////////////////////////////////////////////////////////////////////////
//
// change algorithm/satellite
//
////////////////////////////////////////////////////////////////////////////////////////////////////
function changeAlgorithmSatellite( alg_obj, sat_obj, prod_obj, layer_obj ) { 
 
	var sat_value       = sat_obj.value;
	var alg_value       = alg_obj.value;
	var prod_value_old  = prod_obj.value;
	var layer_value_old = layer_obj.value;
	
	var prod_old_exist  = 0;
	var layer_old_exist = 0;
	
	prod_obj.options.length = 0; 
	
	for ( isat=0; isat < satellite_list.length; isat++ ) {
	
	    if (satellite_list[isat].name == sat_value && satellite_list[isat].algorithm == alg_value ) {
	  	
		var products = satellite_list[isat].products.split(" ");
	    	
		for ( i=0; i < products.length; i++ )  {
		
		    var left = products[i].indexOf("(");
		    var prod_value = products[i] ;
		    if ( left != -1 ) { prod_value = products[i].substring(0,left);}
		    if ( prod_value == prod_value_old ) { prod_old_exist = 1; }
		    var prod_text = getProductText(prod_value);
		
		    prod_obj.options[i]        = new Option( );
		    prod_obj.options[i].value  = prod_value;
		    prod_obj.options[i].text   = prod_text;
	    	}
	    }
	
	}
	
	var product_value = "";
	if ( prod_old_exist == 1 ) {
	    product_value  = prod_value_old;
	    prod_obj.value = prod_value_old;
	}
	else {
	    product_value = prod_obj.options[0].value;
	}
	
	// Layers, which depend on product selected
 	layer_obj.options.length = 0; 
	
	document.getElementById("layer").className = "optioninvisible";  

	for ( isat=0; isat < satellite_list.length; isat++ ) {
	
	    if (satellite_list[isat].name == sat_value && satellite_list[isat].algorithm == alg_value ) {
	  	
		var products = satellite_list[isat].products.split(" ");
	    	
		for ( i=0; i < products.length; i++ )  {
		
		    var left = products[i].indexOf("(");
		    var prod_value = products[i] ;
		    if ( left != -1 ) { prod_value = products[i].substring(0,left); }
		
		    if ( prod_value == product_value && left > 0 ) {
                      document.getElementById("layer").className = "optionvisible";		    
                      var right = products[i].indexOf(")");
                      var layer_str = products[i].substring(left+1,right);
                      var layers = layer_str.split(":");
		    	
                      for ( j = 0; j < layers.length; j++ ) {
		    	  if( layers[j] == layer_value_old ) { 
                              layer_old_exist = 1; 
                          }
                          document.form1.layer.options[j]       = new Option();
                          document.form1.layer.options[j].value = layers[j];
                          document.form1.layer.options[j].text  = layers[j];
                      }
		    } 
	    	 }
	    }
	}
	
  	sat  = sat_value ;
	prod = product_value;
	if ( layer_old_exist == 1 ) {
	    layer = layer_value_old;
	    layer_obj.value = layer_value_old; 
	}
	else if( layer_obj.options.length > 0 && document.getElementById("layer").className == "optionvisible" ) {
	    layer = layer_obj.options[0].value;	
	}
	
	algorithm = alg_value;
	
	launch();   
} 


////////////////////////////////////////////////////////////////////////////////////////////////////
//
// change algorithm
//
////////////////////////////////////////////////////////////////////////////////////////////////////
function changeAlgorithm( alg_value ) { 
 
	var sat_value_old   = document.form1.sat.value;
	var prod_value_old  = document.form1.prod.value;
	var layer_value_old = document.form1.layer.value;
	
	var sat_old_exist   = 0;
	var prod_old_exist  = 0;
	var layer_old_exist = 0;
	
	document.form1.sat.options.length = 0;
	document.form1.prod.options.length = 0;
	document.form1.layer.options.length = 0;
	
	var prod_obj  = document.form1.sat;
	var layer_obj = document.form1.layer;

	if( alg_value == 'her' ) {
		document.form1.sat.options[0] = new Option( );
		document.form1.sat.options[0].value = 'n18';
		document.form1.sat.options[0].text = 'NOAA-18';
		
		document.form1.sat.options[1] = new Option( );
		document.form1.sat.options[1].value = 'n19';
		document.form1.sat.options[1].text = 'NOAA-19';

		document.form1.sat.options[2] = new Option( );
		document.form1.sat.options[2].value = 'metopA';
		document.form1.sat.options[2].text = 'METOP-A';
		
		document.form1.sat.options[3] = new Option( );
		document.form1.sat.options[3].value = 'metopB';
		document.form1.sat.options[3].text = 'METOP-B';
	} 
	else {
		document.form1.sat.options[0] = new Option( );
		document.form1.sat.options[0].value = 'n18';
		document.form1.sat.options[0].text = 'NOAA-18';
		
		document.form1.sat.options[1] = new Option( );
		document.form1.sat.options[1].value = 'n19';
		document.form1.sat.options[1].text = 'NOAA-19';
		
		document.form1.sat.options[2] = new Option( );
		document.form1.sat.options[2].value = 'metopA';
		document.form1.sat.options[2].text = 'METOP-A';
		
		document.form1.sat.options[3] = new Option( );
		document.form1.sat.options[3].value = 'metopB';
		document.form1.sat.options[3].text = 'METOP-B';
		
		document.form1.sat.options[4] = new Option( );
		document.form1.sat.options[4].value = 'f16';
		document.form1.sat.options[4].text = 'F16 SSMIS';
		
		document.form1.sat.options[5] = new Option( );
		document.form1.sat.options[5].value = 'f18';
		document.form1.sat.options[5].text = 'F18 SSMIS';
		
		document.form1.sat.options[6] = new Option( );
		document.form1.sat.options[6].value = 'npp';
		document.form1.sat.options[6].text = 'NPP ATMS';
		
		document.form1.sat.options[7] = new Option( );
		document.form1.sat.options[7].value = 'trmm';
		document.form1.sat.options[7].text = 'TRMM TMI';
	}
	
	
	var sat_value =  document.form1.sat.value;
	
	for ( isat=0; isat < satellite_list.length; isat++ ) {
	
	    if (satellite_list[isat].name == sat_value && satellite_list[isat].algorithm == alg_value ) {
	  	
		var products = satellite_list[isat].products.split(" ");
	    	
		for ( i=0; i < products.length; i++ )  {
		
		    var left = products[i].indexOf("(");
		    var prod_value = products[i] ;
		    if ( left != -1 ) { prod_value = products[i].substring(0,left);}
		    if ( prod_value == prod_value_old ) { prod_old_exist = 1; }
		    var prod_text = getProductText(prod_value);
		
		    prod_obj.options[i]        = new Option( );
		    prod_obj.options[i].value  = prod_value;
		    prod_obj.options[i].text   = prod_text;
	    	}
	    }
	
	}
	
	var product_value = "";
	if ( prod_old_exist == 1 ) {
	    product_value  = prod_value_old;
	    prod_obj.value = prod_value_old;
	}
	else {
	    product_value = prod_obj.options[0].value;
	}
	
	// Layers, which depend on product selected
 	// layer_obj.options.length = 0; 
	
	document.getElementById("layer").className = "optioninvisible";  

	for ( isat=0; isat < satellite_list.length; isat++ ) {
	
	    if (satellite_list[isat].name == sat_value && satellite_list[isat].algorithm == alg_value ) {
	  	
		var products = satellite_list[isat].products.split(" ");
	    	
		for ( i=0; i < products.length; i++ )  {
		
		    var left = products[i].indexOf("(");
		    var prod_value = products[i] ;
		    if ( left != -1 ) { prod_value = products[i].substring(0,left); }
		
		    if ( prod_value == product_value && left > 0 ) {
                      document.getElementById("layer").className = "optionvisible";		    
                      var right = products[i].indexOf(")");
                      var layer_str = products[i].substring(left+1,right);
                      var layers = layer_str.split(":");
		    	
                      for ( j = 0; j < layers.length; j++ ) {
		    	  if( layers[j] == layer_value_old ) { 
                              layer_old_exist = 1; 
                          }
                          document.form1.layer.options[j]       = new Option();
                          document.form1.layer.options[j].value = layers[j];
                          document.form1.layer.options[j].text  = layers[j];
                      }
		    } 
	    	 }
	    }
	}
	
  	sat  = sat_value ;
	prod = product_value;
	if ( layer_old_exist == 1 ) {
	    layer = layer_value_old;
	    layer_obj.value = layer_value_old; 
	}
	else if( layer_obj.options.length > 0 && document.getElementById("layer").className == "optionvisible" ) {
	    layer = layer_obj.options[0].value;	
	}
	
	algorithm = alg_value;
	
	launch();   
} 



////////////////////////////////////////////////////////////////////////////////////////////////////
//
// change product and popup layers/channels options if there are some
//
////////////////////////////////////////////////////////////////////////////////////////////////////
function changeProduct( product_value ) {
    
	var sat_value = document.form1.sat.value;
	var alg_value = document.form1.algorithm.value;;

 	document.form1.layer.options.length = 0;  	
	document.form1.layer.className="optioninvisible";  

	for ( isat=0; isat < satellite_list.length; isat++ ) {
	
	    if (satellite_list[isat].name == sat_value && satellite_list[isat].algorithm == alg_value ) {
	  	
		var products = satellite_list[isat].products.split(" ");
	    	
		for ( i=0; i < products.length; i++ )  {
		
		    var left = products[i].indexOf("(");
		    var prod_value = products[i] ;
		    if ( left != -1 ) { prod_value = products[i].substring(0,left);}
		
		    if ( prod_value == product_value && left > 0 ) {
	   	        document.form1.layer.className="optionvisible";  
		
		    	var right = products[i].indexOf(")");
		    	var layer_str = products[i].substring(left+1,right);
		    	var layers = layer_str.split(":");
		    	for ( j = 0; j < layers.length; j++ ) {
                    	    document.form1.layer.options[j] 	     = new Option();
                    	    document.form1.layer.options[j].value    = layers[j];
                    	    document.form1.layer.options[j].text     = layers[j];
                    	}
		    } 
	    	}
	    }
	}
	
	algorithm = alg_value;	
	sat       = sat_value;
  	prod      = product_value;

	if ( document.form1.layer.options.length > 0 && document.form1.layer.className == "optionvisible") {
	    layer = document.form1.layer.options[document.form1.layer.selectedIndex].value;
	}
	
	if ( product_value == "em" ||  product_value == "tbc" || product_value == "tbu" ) {
		document.form1.layer.title = "select a channel";
	}
	else if ( product_value == "temp" || product_value == "wv" || product_value == "clwp" || product_value == "rainp" || product_value == "graupelp" ) {
		document.form1.layer.title = "select a layer";
	}
	else if ( product_value == "gs"   ||  product_value == "sfc"    || product_value == "sfc2" || product_value == "snow" || product_value == "swe" ||
	          product_value == "sice" ||  product_value == "sicefy" || product_value == "sicemy" ) {
		document.form1.layer.title = "select a map projection";
	}
	
	
	launch();   
    
}

