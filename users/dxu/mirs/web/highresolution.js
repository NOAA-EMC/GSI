////////////////////////////////////////////////////////////////////////////////////////////////////
//
// variable and parameter define section
//
////////////////////////////////////////////////////////////////////////////////////////////////////

// define a satellite object with some fields
function satellite_object(name, products)
{
  this.name = name;
  this.products = products;
}


// Satellite and its product(layers,channels)
var satellite_n15    = new satellite_object("n15",    "iwp(mspps) rr(mspps) snow(mspps) swe(mspps)");
var satellite_n16    = new satellite_object("n16",    "iwp(mspps) rr(mspps) snow(mspps) swe(mspps)");
var satellite_n17    = new satellite_object("n17",    "iwp(mspps) rr(mspps)");
var satellite_n19    = new satellite_object("n19",    "iwp(mspps) rr(mspps) snow(mspps) swe(mspps)");

var satellite_n18    = new satellite_object("n18",    "chisq clw em(23v:31v:50v:52v:53h:54h:54v:55h:57h1:57h2:57h3:57h4:57h5:57h6:89v1:89v2:157h:184h:186h:190v) gs(cyl:pn:ps) iwp(1dvar:mspps) lwp nattempt niter psfc qc rr(1dvar:mspps) rwp sfc(cyl:pn:ps) sfc2(cyl:pn:ps) sice(cyl:pn:ps) sicemy(cyl:pn:ps) sicefy(cyl:pn:ps) snow(1dvar:mspps) swe(1dvar:mspps) tbc(23v:31v:50v:52v:53h:54h:54v:55h:57h1:57h2:57h3:57h4:57h5:57h6:89v1:89v2:157h:184h:186h:190v) tbu(23v:31v:50v:52v:53h:54h:54v:55h:57h1:57h2:57h3:57h4:57h5:57h6:89v1:89v2:157h:184h:186h:190v) temp(100mb:200mb:300mb:400mb:500mb:600mb:700mb:800mb:850mb:900mb:950mb) tpw tskin wv(100mb:200mb:300mb:400mb:500mb:600mb:700mb:800mb:850mb:900mb:950mb)");
var satellite_metopB = new satellite_object("metopB", "chisq clw em(23v:31v:50v:52v:53h:54h:54v:55h:57h1:57h2:57h3:57h4:57h5:57h6:89v1:89v2:157h:184h:186h:190v) gs(cyl:pn:ps) iwp(1dvar:mspps) lwp nattempt niter psfc qc rr(1dvar:mspps) rwp sfc(cyl:pn:ps) sfc2(cyl:pn:ps) sice(cyl:pn:ps) sicemy(cyl:pn:ps) sicefy(cyl:pn:ps) snow(1dvar:mspps) swe(1dvar:mspps) tbc(23v:31v:50v:52v:53h:54h:54v:55h:57h1:57h2:57h3:57h4:57h5:57h6:89v1:89v2:157h:184h:186h:190v) tbu(23v:31v:50v:52v:53h:54h:54v:55h:57h1:57h2:57h3:57h4:57h5:57h6:89v1:89v2:157h:184h:186h:190v) temp(100mb:200mb:300mb:400mb:500mb:600mb:700mb:800mb:850mb:900mb:950mb) tpw tskin wv(100mb:200mb:300mb:400mb:500mb:600mb:700mb:800mb:850mb:900mb:950mb)");
var satellite_metopA = new satellite_object("metopA", "chisq clw em(23v:31v:50v:52v:53h:54h:54v:55h:57h1:57h2:57h3:57h4:57h5:57h6:89v1:89v2:157h:184h:186h:190v) gs(cyl) iwp(1dvar:mspps) lwp nattempt niter psfc qc rr(1dvar:mspps) rwp sfc(cyl) sfc2(cyl) sice(cyl) sicemy(cyl) sicefy(cyl) snow(1dvar:mspps) swe(1dvar:mspps) tbc(23v:31v:50v:52v:53h:54h:54v:55h:57h1:57h2:57h3:57h4:57h5:57h6:89v1:89v2:157h:184h:186h:190v) tbu(23v:31v:50v:52v:53h:54h:54v:55h:57h1:57h2:57h3:57h4:57h5:57h6:89v1:89v2:157h:184h:186h:190v) temp(100mb:200mb:300mb:400mb:500mb:600mb:700mb:800mb:850mb:900mb:950mb) tpw tskin wv(100mb:200mb:300mb:400mb:500mb:600mb:700mb:800mb:850mb:900mb:950mb)");
var satellite_f16    = new satellite_object("f16",    "chisq clw em(50v:52v:53v:54v:55v:57rc:59rc:150h:190h:186h:184h:19h:19v:22v:37h:37v:91v:91h:63rc:60rc1:60rc2:60rc3:60rc4:60rc5) gs(cyl) iwp lwp nattempt niter psfc qc rr rwp sfc(cyl) sfc2(cyl) sice(cyl) sicemy(cyl) sicefy(cyl) snow(cyl) swe(cyl) tbc(50v:52v:53v:54v:55v:57rc:59rc:150h:190h:186h:184h:19h:19v:22v:37h:37v:91v:91h:63rc:60rc1:60rc2:60rc3:60rc4:60rc5) tbu(50v:52v:53v:54v:55v:57rc:59rc:150h:190h:186h:184h:19h:19v:22v:37h:37v:91v:91h:63rc:60rc1:60rc2:60rc3:60rc4:60rc5) temp(100mb:200mb:300mb:400mb:500mb:600mb:700mb:800mb:850mb:900mb:950mb) tpw tskin wspd wv(100mb:200mb:300mb:400mb:500mb:600mb:700mb:800mb:850mb:900mb:950mb)");
var satellite_f18    = new satellite_object("f18",    "chisq clw em(50h:52h:53h:54h:55h:57rc:59rc:150h:190h:186h:184h:19h:19v:22v:37h:37v:91v:91h:63rc:60rc1:60rc2:60rc3:60rc4:60rc5) gs(cyl) iwp lwp nattempt niter psfc qc rr rwp sfc(cyl) sfc2(cyl) sice(cyl) sicemy(cyl) sicefy(cyl) snow(cyl) swe(cyl) tbc(50h:52h:53h:54h:55h:57rc:59rc:150h:190h:186h:184h:19h:19v:22v:37h:37v:91v:91h:63rc:60rc1:60rc2:60rc3:60rc4:60rc5) tbu(50h:52h:53h:54h:55h:57rc:59rc:150h:190h:186h:184h:19h:19v:22v:37h:37v:91v:91h:63rc:60rc1:60rc2:60rc3:60rc4:60rc5) temp(100mb:200mb:300mb:400mb:500mb:600mb:700mb:800mb:850mb:900mb:950mb) tpw tskin wspd wv(100mb:200mb:300mb:400mb:500mb:600mb:700mb:800mb:850mb:900mb:950mb)");

var satellite_aqua   = new satellite_object("aqua",   "chisq clw em(7v:7h:11v:11h:19v:19h:24v:24h:37v:37h:89v:89h) iwp lwp nattempt niter psfc qc rr rwp sfc(cyl:pn) tbc(7v:7h:11v:11h:19v:19h:24v:24h:37v:37h:89v:89h) tbu(7v:7h:11v:11h:19v:19h:24v:24h:37v:37h:89v:89h) temp(100mb:200mb:300mb:400mb:500mb:600mb:700mb:800mb:850mb:900mb:950mb) tpw tskin wv(100mb:200mb:300mb:400mb:500mb:600mb:700mb:800mb:850mb:900mb:950mb)");
var satellite_trmm   = new satellite_object("trmm",   "chisq clw em(11v:11h:19v:19h:21v:37v:37h:85v:85h) hour iwp lwp nattempt niter psfc qc rr rwp sfc(cyl:pn:ps) tbc(11v:11h:19v:19h:21v:37v:37h:85v:85h) tbu(11v:11h:19v:19h:21v:37v:37h:85v:85h) temp(100mb:200mb:300mb:400mb:500mb:600mb:700mb:800mb:850mb:900mb:950mb) tpw tskin wspd wv(100mb:200mb:300mb:400mb:500mb:600mb:700mb:800mb:850mb:900mb:950mb)");
var satellite_gpm    = new satellite_object("gpm",    "chisq clw em(11v:11h:19v:19h:24v:37v:37h:89v:89h:166v:166h:183v1:183v2) iwp lwp nattempt niter psfc qc rr rwp sfc(cyl:pn) tbc(11v:11h:19v:19h:24v:37v:37h:89v:89h:166v:166h:183v1:183v2) tbu(11v:11h:19v:19h:24v:37v:37h:89v:89h:166v:166h:183v1:183v2) temp(100mb:200mb:300mb:400mb:500mb:600mb:700mb:800mb:850mb:900mb:950mb) tpw tskin wspd wv(100mb:200mb:300mb:400mb:500mb:600mb:700mb:800mb:850mb:900mb:950mb)");
var satellite_npp    = new satellite_object("npp",    "chisq clw em(23v:31v:50h:51h:52h:53h:54h1:54h2:55h:57h1:57h2:57h3:57h4:57h5:57h6:88v:165h:183h1:183h2:183h3:183h4:183h5) emspectrum gs(cyl:pn:ps) iwp lwp nattempt niter psfc qc rr rwp sfc(cyl:pn:ps) sfc2(cyl:pn:ps) sice(cyl:pn:ps) sicemy(cyl:pn:ps) sicefy(cyl:pn:ps) snow(cyl:pn:ps) swe(cyl:pn:ps) tbc(23v:31v:50h:51h:52h:53h:54h1:54h2:55h:57h1:57h2:57h3:57h4:57h5:57h6:88v:165h:183h1:183h2:183h3:183h4:183h5) tbu(23v:31v:50h:51h:52h:53h:54h1:54h2:55h:57h1:57h2:57h3:57h4:57h5:57h6:88v:165h:183h1:183h2:183h3:183h4:183h5) temp(100mb:200mb:300mb:400mb:500mb:600mb:700mb:800mb:850mb:900mb:950mb) tpw tskin wv(100mb:200mb:300mb:400mb:500mb:600mb:700mb:800mb:850mb:900mb:950mb)");

var satellite_gcomw1 = new satellite_object("gcomw1", "chisq clw em(6v:6h:10v:10h:18v:18h:23v:23h:36v:36h:89v:89h) iwp lwp nattempt niter psfc qc rr rwp sfc(cyl:pn:ps) tbc(6v:6h:10v:10h:18v:18h:23v:23h:36v:36h:89v:89h) tbu(6v:6h:10v:10h:18v:18h:23v:23h:36v:36h:89v:89h) temp(100mb:200mb:300mb:400mb:500mb:600mb:700mb:800mb:850mb:900mb:950mb) tpw tskin wv(100mb:200mb:300mb:400mb:500mb:600mb:700mb:800mb:850mb:900mb:950mb)");

var satellite_list   = new Array( 
	satellite_metopB,
	satellite_npp,
	satellite_trmm,
	satellite_gcomw1);

var SENSORTAG = new Array();
SENSORTAG["n18"]    = "/mirs_adv_poes_n18_amsuamhs_";
SENSORTAG["metopA"] = "/mirs_adv_poes_metopA_amsuamhs_";
SENSORTAG["metopB"] = "/mirs_adv_poes_metopB_amsuamhs_";
SENSORTAG["f16"]    = "/mirs_adv_dmsp_f16_ssmis_"; 
SENSORTAG["f18"]    = "/mirs_adv_dmsp_f18_ssmis_"; 
SENSORTAG["aqua"]   = "/mirs_adv_eos_aqua_amsre_";
SENSORTAG["trmm"]   = "/mirs_adv_eos_trmm_tmi_";
SENSORTAG["gpm"]    = "/mirs_adv_eos_gpm_gmi_";
SENSORTAG["npp"]    = "/mirs_adv_npoess_npp_atms_";
SENSORTAG["gcomw1"] = "/mirs_adv_eos_gcomw1_amsr2_";
//SENSORTAG["n16"]    = "/mirs_adv_npoess_npp_atms_";
//SENSORTAG["n19"]    = "/mirs_adv_npoess_npp_atms_";


// image location, for historic reason, we put npp into images/
var IMGDIRS = new Array();
IMGDIRS["metopB"] = "images/";
IMGDIRS["npp"]    = "images/";
IMGDIRS["trmm"]   = "images/";
IMGDIRS["gcomw1"] = "images/";

IMGDIRS["n18"]    = "img_hr/";
IMGDIRS["metopA"] = "img_hr/";
IMGDIRS["f16"]    = "img_hr/";
IMGDIRS["f18"]    = "img_hr/";
IMGDIRS["aqua"]   = "img_hr/";
IMGDIRS["gpm"]    = "img_hr/";
IMGDIRS["n16"]    = "img_hr/";
IMGDIRS["n19"]    = "img_hr/";

var SATSWITCH = new Array();
SATSWITCH['n15'] = "n15";
SATSWITCH['n16'] = "n16";
SATSWITCH['n17'] = "n17";
SATSWITCH['n18'] = "n18";
SATSWITCH['n19'] = "n19";
SATSWITCH['metopA'] = "m2";
SATSWITCH['metopB'] = "m1";


var REGIONVALS = new Array( "glb",    "us", "eu",     "gulf", "china", "westcoast", "conus", "trop");
var REGIONTXTS = new Array( "Globe", "USA", "Europe", "Gulf", "China", "W. Coast",  "CONUS", "Tropical");

var AMSRE_DEAD = 'The AMSR-E antenna failed at 0726GMT Oct 4';

////////////////////////////////////////////////////////////////////////////////////////////////////
//
// animation parameters section
//
////////////////////////////////////////////////////////////////////////////////////////////////////
var last_image  = 31;
var first_image = 1;
var image_name  ="";
var current_image = 0; 			//number of the current image

var images1 = new Array();      	//holds the images in panel1
var images2 = new Array();      	//holds the images in panel2
var images3 = new Array();      	//holds the images in panel3
var images4 = new Array();      	//holds the images in panel4

var imageNum1  = new Array();		//keeps track of which images to omit from loop
var imageNum2  = new Array();       	//keeps track of which images to omit from loop
var imageNum3  = new Array();       	//keeps track of which images to omit from loop
var imageNum4  = new Array();       	//keeps track of which images to omit from loop

var alts1 = new Array();
var alts2 = new Array();
var alts3 = new Array();
var alts4 = new Array();

var delay = 500;         		//delay between frames in 1/100 seconds
var timeID = null;

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
// function definition section
//
////////////////////////////////////////////////////////////////////////////////////////////////////

function getProductText( val ) {
 
  var txt = "" ;
  
  if(      val == "chisq" )      txt = "Chi Square" ; 	 
  else if( val == "clw" )        txt = "CLW" ;	  
  else if( val == "em" )         txt = "Emissivity" ; 
  else if( val == "emspectrum" ) txt = "Emissivity Spectrum" ; 
  else if( val == "frlp" )       txt = "Freezing Level" ; 
  else if( val == "gs" )         txt = "Snow Grain Size" ;  
  else if( val == "iwp" )        txt = "IWP" ;
  else if( val == "hour" )       txt = "Scan Hour" ;
  else if( val == "lapse" )      txt = "Lapse Rate" ;
  else if( val == "lwp" )        txt = "LWP" ;
  else if( val == "nattempt" )   txt = "Atmpt Number" ;
  else if( val == "niter" )      txt = "Iter. Number" ;
  else if( val == "psfc" )       txt = "Sfc Pressure" ;	
  else if( val == "qc" )         txt = "QC Flag" ;      
  else if( val == "rr" )         txt = "Rain Rate" ;   
  else if( val == "rwp" )        txt = "RWP" ;	
  else if( val == "sfc" )        txt = "Pre-Sfc" ;
  else if( val == "sfc2" )       txt = "Post-Sfc" ;
  else if( val == "sice" )       txt = "Sea Ice" ;
  else if( val == "sicemy" )     txt = "Many Year Ice" ;
  else if( val == "sicefy" )     txt = "First Year Ice" ;
  else if( val == "snow" )       txt = "Snow Cover" ;
  else if( val == "swe" )        txt = "SWE" ; 	
  else if( val == "tbc" )        txt = "Corr. TB" ;
  else if( val == "tbu" )        txt = "UnCorr. TB" ;
  else if( val == "temp" )       txt = "Temperature" ; 
  else if( val == "tpw" )        txt = "TPW" ;
  else if( val == "tskin" )      txt = "Skin Temp." ;	
  else if( val == "wspd" )       txt = "Wind Speed" ;	
  else if( val == "wv" )         txt = "Water Vapor" ; 

  return txt;

}


////////////////////////////////////////////////////////////////////////////////////////////////////
//
// when satellite changes
//
////////////////////////////////////////////////////////////////////////////////////////////////////
function changeSatellite_template( sat_obj, prod_obj, layer_obj, layerID ) {

	var sat_value  = sat_obj.value;
	
	var prod_old   = prod_obj.value;
	var layer_old  = layer_obj.value;
	
	prod_obj.options.length  = 0;

	var prod_old_exist = 0;  // to keep track whether new product list has old product or not
	
	for ( isat=0; isat < satellite_list.length; isat++ ) {
	    if (satellite_list[isat].name == sat_value ) {
		var products = satellite_list[isat].products.split(" ");
		for ( i=0; i<products.length; i++ )  {
		    var left = products[i].indexOf("(");
		    var prod_value = products[i] ;
		    if ( left != -1 ) { prod_value = products[i].substring(0,left); }
		    if ( prod_value == prod_old ) { prod_old_exist = 1; }
		    var prod_text = getProductText(prod_value);
		    prod_obj.options[i]       = new Option();
		    prod_obj.options[i].value = prod_value;
		    prod_obj.options[i].text  = prod_text;
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
	
	document.getElementById(layerID).className="optioninvisible";  

	for ( isat=0; isat<satellite_list.length; isat++ ) {
	    if (satellite_list[isat].name == sat_value ) {
		var products = satellite_list[isat].products.split(" ");
		for ( i=0; i<products.length; i++ )  {
		    var left = products[i].indexOf("(");
		    var prod_value = products[i] ;
		    if ( left != -1 ) { prod_value = products[i].substring(0,left);}

		    if ( prod_value == product_value && left > 0 ) {
	   	        document.getElementById(layerID).className="optionvisible";  
		
		    	var right = products[i].indexOf(")");
		    	var layer_str = products[i].substring(left+1,right);
		    	var layers = layer_str.split(":");
		    	
		    	for ( j=0; j<layers.length; j++ ) {
		    	    layer_obj.options[j] = new Option();
		    	    layer_obj.options[j].value = layers[j];
		    	    layer_obj.options[j].text  = layers[j];
			    if ( layer_old == layers[j] ) { layer_old_exist=1; }
		    	}	
		    } 
	    	}
	    }
	}
	
	// if populated layer list has old layer, we use old one; otherwise, use the 1st one in the layer list
	if ( layer_old_exist == 1 ) { layer_obj.value = layer_old; }
}


////////////////////////////////////////////////////////////////////////////////////////////////////
//
// change product and popup layers/channels options if there are some
//
////////////////////////////////////////////////////////////////////////////////////////////////////
function changeProduct_template( sat_obj, prod_obj, layer_obj, layerID ) {
    	
	var layer_old  = layer_obj.value;
	var layer_old_exist = 0;

	
	var sat_value     = sat_obj.value;
	var product_value = prod_obj.value; 

 	layer_obj.options.length = 0; 
 	
	document.getElementById(layerID).className="optioninvisible";  

	for ( isat=0; isat<satellite_list.length; isat++ ) {
	
	    if (satellite_list[isat].name == sat_value ) {
	  	
		var products = satellite_list[isat].products.split(" ");
	    	
		for ( i=0; i<products.length; i++ )  {
		
		    var left = products[i].indexOf("(");
		    var prod_value = products[i] ;
		    if ( left != -1 ) { prod_value = products[i].substring(0,left);}
		
		    if ( prod_value == product_value && left > 0 ) {
	   	        document.getElementById(layerID).className="optionvisible";  
		
		    	var right = products[i].indexOf(")");
		    	var layer_str = products[i].substring(left+1,right);
		    	var layers = layer_str.split(":");
		    	
		    	for ( j=0; j<layers.length; j++ ) {
		    	    layer_obj.options[j] = new Option();
		    	    layer_obj.options[j].value = layers[j];
			    layer_obj.options[j].text  = layers[j];
			    if ( layer_old == layers[j] ) { layer_old_exist=1; }
		    	}
		    } 
	    	}
	    }
	}
	
	// if populated layer list has old layer, we use old one; otherwise, use the 1st one in the layer list
	if ( layer_old_exist == 1 ) { layer_obj.value = layer_old; }
	
	
	if ( product_value == "iwp" ||  product_value == "rr" || product_value == "snow" || product_value == "swe" ) {
		document.getElementById(layerID).title = "select an algorithm (1dvar or mspps)" ;
	}
	else if (  product_value == "em" ||  product_value == "tbc" || product_value == "tbu" ) {
		document.getElementById(layerID).title = "select a channel" ;
	}
	else if (  product_value == "temp" ||  product_value == "wv" ) {
		document.getElementById(layerID).title = "select a pressure layer" ;
	}
	else if (  product_value == "sfc" ||  product_value == "sfc2" || product_value == "gs" ) {
		document.getElementById(layerID).title = "select a map projection type (cyl:cylindrical,pn:Northern Polar Stereographic,ps:Southern Polar Stereographic" ;
	}
	
}



////////////////////////////////////////////////////////////////////////////////////////////////////
//
// populate regions drop down menu according to different satellite selected
//
////////////////////////////////////////////////////////////////////////////////////////////////////
function populateRegions( satval, obj_region ) {}


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
function loadImageHelper(sat,region,prod,layer,cend,sfc,yyyy,mm,dd) {

	var ymd   = yyyy + "-" + mm + "-" + dd;	
	var image = "";
	
	if    ( prod == "em" || prod == "temp" || prod == "wv" || prod == "tbc" || prod == "tbu" ) {
  	  image=IMGDIRS[sat]+sat+"/"+ymd+SENSORTAG[sat]+region+"_"+yyyy+mm+dd+"_"+prod+"_"+layer+"_"+sfc+"_"+cend+".png";
	}
	else if ( prod == "sfc" || prod == "sfc2" ) {
	  image=IMGDIRS[sat]+sat+"/"+ymd+SENSORTAG[sat]+region+"_"+yyyy+mm+dd+"_"+prod+"_"+layer+"_all_"+cend+".png";
	}
	
	else if ( prod == "swe" || prod == "snow" ) {
	  if ( layer == "mspps" )
	    image=IMGDIRS[sat]+sat+"/"+ymd+"/mspps_"+SATSWITCH[sat]+"_"+prod+"_"+yyyy+mm+dd+"_"+cend+".png";
	  else if ( sat == "npp" )
  	    image=IMGDIRS[sat]+sat+"/"+ymd+SENSORTAG[sat]+region+"_"+yyyy+mm+dd+"_"+prod+"_"+layer+"_lnd_"+cend+".png";
	  else 
  	    image=IMGDIRS[sat]+sat+"/"+ymd+SENSORTAG[sat]+region+"_"+yyyy+mm+dd+"_"+prod+"_cyl_lnd_"+cend+".png";
	} 
	else if ( prod == "iwp" || prod == "rr" ) {
	  if ( layer == "mspps" )
	    image=IMGDIRS[sat]+sat+"/"+ymd+"/mspps_"+SATSWITCH[sat]+"_"+prod+"_"+yyyy+mm+dd+"_"+cend+".png";
	  else
  	    image=IMGDIRS[sat]+sat+"/"+ymd+SENSORTAG[sat]+region+"_"+yyyy+mm+dd+"_"+prod+"_"+sfc+"_"+cend+".png";
	}
	
	else if ( prod == "sice" || prod == "sicefy" || prod == "sicemy" ) {
	  image=IMGDIRS[sat]+sat+"/"+ymd+SENSORTAG[sat]+region+"_"+yyyy+mm+dd+"_"+prod+"_"+layer+"_sea_"+cend+".png";
	}
	else if ( prod == "gs" ) {
	  image=IMGDIRS[sat]+sat+"/"+ymd+SENSORTAG[sat]+region+"_"+yyyy+mm+dd+"_"+prod+"_"+layer+"_lnd_"+cend+".png";
	}
	
	else if ( prod == "clw" || prod == "lwp" ) {
	  image=IMGDIRS[sat]+sat+"/"+ymd+SENSORTAG[sat]+region+"_"+yyyy+mm+dd+"_"+prod+"_sea_"+cend+".png";
	}
	
	else if ( prod == "wspd" ) {
	  image=IMGDIRS[sat]+sat+"/"+ymd+SENSORTAG[sat]+region+"_"+yyyy+mm+dd+"_"+prod+"_sea_"+cend+".png";
	}
	
	else	{
  	  image=IMGDIRS[sat]+sat+"/"+ymd+SENSORTAG[sat]+region+"_"+yyyy+mm+dd+"_"+prod+"_"+sfc+"_"+cend+".png";
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
	
	// This is only for AMSR-E since no data since 2011-10-04
	var isAmsre = img_str.indexOf("_aqua_amsre_");
	if( isAmsre != -1 ) {
  		var yyyymmdd = alt.substring(30,38);
  		var ymd = parseInt(yyyymmdd);
  		if( ymd > 20111004 ) { 
     		img_obj.alt = AMSRE_DEAD;
  		}
	}
}


function loadImage1() {

	var sat1    = document.form.sat1.value;
	var region1 = document.form.region1.value;
	var prod1   = document.form.prod1.value;
	var layer1  = document.form.layer1.value;
	var cend1   = document.form.cend1.value;
	var sfc1    = document.form.sfc1.value;
	var yyyy1   = document.form.yr1.value;
	var mm1     = document.form.mo1.value;
	var dd1     = document.form.dy1.value;
	
	var image1 = loadImageHelper(sat1,region1,prod1,layer1,cend1,sfc1,yyyy1,mm1,dd1)
	image2Obj(image1, document.form.img1, "href1"); 
	
}

function loadImage2() {

	var sat2    = document.form.sat2.value;
	var region2 = document.form.region2.value;
	var prod2   = document.form.prod2.value;
	var layer2  = document.form.layer2.value;
	var cend2   = document.form.cend2.value;
	var sfc2    = document.form.sfc2.value;
	var yyyy2   = document.form.yr2.value;
	var mm2     = document.form.mo2.value;
	var dd2     = document.form.dy2.value;
	
	var image2 = loadImageHelper(sat2,region2,prod2,layer2,cend2,sfc2,yyyy2,mm2,dd2)
	image2Obj(image2, document.form.img2, "href2"); 
	
}

function loadImage3() {

	var sat3    = document.form.sat3.value;
	var region3 = document.form.region3.value;
	var prod3   = document.form.prod3.value;
	var layer3  = document.form.layer3.value;
	var cend3   = document.form.cend3.value;
	var sfc3    = document.form.sfc3.value;
	var yyyy3   = document.form.yr3.value;
	var mm3     = document.form.mo3.value;
	var dd3     = document.form.dy3.value;
	
	var image3 = loadImageHelper(sat3,region3,prod3,layer3,cend3,sfc3,yyyy3,mm3,dd3)
	image2Obj(image3, document.form.img3, "href3"); 

}

function loadImage4() {

	var sat4    = document.form.sat4.value;
	var region4 = document.form.region4.value;
	var prod4   = document.form.prod4.value;
	var layer4  = document.form.layer4.value;
	var cend4   = document.form.cend4.value;
	var sfc4    = document.form.sfc4.value;
	var yyyy4   = document.form.yr4.value;
	var mm4     = document.form.mo4.value;
	var dd4     = document.form.dy4.value;
	
	var image4 = loadImageHelper(sat4,region4,prod4,layer4,cend4,sfc4,yyyy4,mm4,dd4)
	image2Obj(image4, document.form.img4, "href4"); 

}


function loadInitialImagesFromScratch() {
	
	var now = new Date();
	now.setDate(now.getDate()-1) ; // yesterday
	
	var year  = now.getFullYear();
	var month = now.getMonth();  // month starting index is 0
	var day   = now.getDate();
       
	document.form.yr1.value = year;
	document.form.mo1.selectedIndex = month ;
	document.form.dy1.selectedIndex = day - 1;
	document.form.cend1.selectedIndex=0;
	document.form.sfc1.value = "all";
	document.form.sat1.selectedIndex=0;
	document.form.prod1.value='tpw';
	document.form.layer1.selectedIndex=0;
	document.form.region1.value='glb';
	
	document.form.yr2.value = year;
	document.form.mo2.selectedIndex = month ;
	document.form.dy2.selectedIndex = day - 1;
	document.form.cend2.selectedIndex=0;
	document.form.sfc2.value = "all";
	document.form.sat2.selectedIndex=0;
	document.form.prod2.value='tskin';
	document.form.layer2.selectedIndex=0;
	document.form.region2.value='glb';

	document.form.yr3.value = year;
	document.form.mo3.selectedIndex = month ;
	document.form.dy3.selectedIndex = day - 1;
	document.form.cend3.selectedIndex=0;
	document.form.sfc3.value = "all";
	document.form.sat3.selectedIndex=0;
	document.form.prod3.value='em';
	document.form.region3.value='glb';
				
	var layer_str = "23v:31v:50v:52v:53h:54h:54v:55h:57h1:57h2:57h3:57h4:57h5:57h6:89v1:89v2:157h:184h:186h:190v";
	var layers = layer_str.split(":");
	layer_obj = document.form.layer3;
	document.form.layer3.className="optionvisible";
	for ( j=0; j<layers.length; j++ ) {
		layer_obj.options[j]       = new Option();
		layer_obj.options[j].value = layers[j];
		layer_obj.options[j].text  = 'ch'+String(j+1)+':'+layers[j];
	}	


	document.form.yr4.value = year;
	document.form.mo4.selectedIndex = month ;
	document.form.dy4.selectedIndex = day - 1;
	document.form.cend4.selectedIndex=0;
	document.form.sfc4.value = "all";
	document.form.sat4.selectedIndex=0;
	document.form.prod4.value='temp';
	document.form.region4.value='glb';

	var layer_str = "100mb:200mb:300mb:400mb:500mb:600mb:700mb:800mb:850mb:900mb:950mb";
	var layers = layer_str.split(":");
	layer_obj = document.form.layer4;
	document.form.layer4.className="optionvisible";
	for ( j=0; j<layers.length; j++ ) {
		layer_obj.options[j]       = new Option();
		layer_obj.options[j].value = layers[j];
		layer_obj.options[j].text  = layers[j];
	}	
	
	loadImage1();
	loadImage2();
	loadImage3();
	loadImage4();
}


function loadInitialImages1( sat1,region1,cend1,sfc1,prod1,layer1,yr1,mo1,dy1 ) {
	
    if ( sat1 != '' && cend1 != '' && sfc1 != '' && prod1 != '' && region1 != ''
     &&   yr1 != '' && mo1   != '' && dy1  != '' ) {
            
        populate_panel_template( "sat1", "prod1", "layer1", sat1, prod1, layer1, document.form.region1 );          
	document.form.yr1.value     = yr1 ;
	document.form.mo1.value     = mo1 ;
	document.form.dy1.value     = dy1 ;
	document.form.cend1.value   = cend1;
	document.form.sfc1.value    = sfc1;
	document.form.sat1.value    = sat1;
	document.form.prod1.value   = prod1;
	document.form.region1.value = region1;
	if ( layer1 != '' ) document.form.layer1.value=layer1;
	var image1 = loadImageHelper(sat1,region1,prod1,layer1,cend1,sfc1,yr1,mo1,dy1);
	image2Obj(image1, document.form.img1, "href1");
    }
    
    else {
    	
	var now = new Date();
	now.setDate(now.getDate()-1); // yesterday
	
	var year  = now.getFullYear();
	var month = now.getMonth();  // month starting index is 0
	var day   = now.getDate();
       
	document.form.yr1.value = year ;
	document.form.mo1.selectedIndex    = month ;
	document.form.dy1.selectedIndex    = day - 1;
	document.form.cend1.selectedIndex  = 0;
	document.form.sfc1.selectedIndex   = 0;
	document.form.sat1.selectedIndex   = 0;
	document.form.prod1.value          = 'tpw';
	document.form.layer1.selectedIndex = 0;
	document.form.region1.value        = 'glb';
	
	loadImage1();
    
    } 
}


function loadInitialImages( sat1,region1,cend1,sfc1,prod1,layer1,yr1,mo1,dy1,
                            sat2,region2,cend2,sfc2,prod2,layer2,yr2,mo2,dy2,
                            sat3,region3,cend3,sfc3,prod3,layer3,yr3,mo3,dy3,
                            sat4,region4,cend4,sfc4,prod4,layer4,yr4,mo4,dy4 )
{
	
    if ( sat1 != '' && region1 != '' && cend1 != '' && sfc1 != '' && prod1 != ''
     &&  yr1  != '' && mo1     != '' && dy1   != '' ) {
            
	populate_panel_template( "sat1", "prod1", "layer1", sat1, prod1, layer1, document.form.region1 );          
	document.form.yr1.value = yr1 ;
	document.form.mo1.value = mo1 ;
	document.form.dy1.value = dy1 ;
	document.form.cend1.value=cend1;
	document.form.sfc1.value=sfc1;
	document.form.sat1.value=sat1;
	document.form.prod1.value=prod1;
	document.form.region1.value=region1;
	if ( layer1 != '' ) document.form.layer1.value=layer1;
	var image1 = loadImageHelper(sat1,region1,prod1,layer1,cend1,sfc1,yr1,mo1,dy1);
	image2Obj(image1, document.form.img1, "href1"); 
	
	populate_panel_template( "sat2", "prod2", "layer2", sat2, prod2, layer2, document.form.region2 );          
	document.form.yr2.value = yr2 ;
	document.form.mo2.value = mo2 ;
	document.form.dy2.value = dy2 ;
	document.form.cend2.value=cend2;
	document.form.sfc2.value=sfc2;
	document.form.sat2.value=sat2;
	document.form.prod2.value=prod2;
	document.form.region2.value=region2;
	if ( layer2 != '' ) document.form.layer2.value=layer2;
	var image2 = loadImageHelper(sat2,region2,prod2,layer2,cend2,sfc2,yr2,mo2,dy2);
	image2Obj(image2, document.form.img2, "href2"); 
	
	populate_panel_template( "sat3", "prod3", "layer3", sat3, prod3, layer3, document.form.region3 );          
	document.form.yr3.value = yr3 ;
	document.form.mo3.value = mo3 ;
	document.form.dy3.value = dy3 ;
	document.form.cend3.value=cend3;
	document.form.sfc3.value=sfc3;
	document.form.sat3.value=sat3;
	document.form.prod3.value=prod3;
	document.form.region3.value=region3;
	if ( layer3 != '' ) document.form.layer3.value=layer3;
	var image3 = loadImageHelper(sat3,region3,prod3,layer3,cend3,sfc3,yr3,mo3,dy3);
	image2Obj(image3, document.form.img3, "href3"); 
	
	populate_panel_template( "sat4", "prod4", "layer4", sat4, prod4, layer4, document.form.region4 );          
	document.form.yr4.value = yr4 ;
	document.form.mo4.value = mo4 ;
	document.form.dy4.value = dy4 ;
	document.form.cend4.value=cend4;
	document.form.sfc4.value=sfc4;
	document.form.sat4.value=sat4;
	document.form.prod4.value=prod4;
	document.form.region4.value=region4;
	if ( layer4 != '' ) document.form.layer4.value=layer4;
	var image4 = loadImageHelper(sat4,region4,prod4,layer4,cend4,sfc4,yr4,mo4,dy4);
	image2Obj(image4, document.form.img4, "href4"); 
	
    }
    
    else {
    
	loadInitialImagesFromScratch();

    }

}


function populate_panel_template( satId, prodId, layerId, sat, prod, layer, obj_region ) {

	prod_obj  = document.getElementById(prodId);
	layer_obj = document.getElementById(layerId);
	
	prod_obj.options.length  = 0;
	layer_obj.options.length = 0;
	
	for ( isat=0; isat < satellite_list.length; isat++ ) {
	
	    if (satellite_list[isat].name == sat ) {
	  	
		var products = satellite_list[isat].products.split(" ");
	    	
		for ( i=0; i<products.length; i++ )  {
		
		    var left = products[i].indexOf("(");
		    var prod_value = products[i] ;
		    if ( left != -1 ) { prod_value = products[i].substring(0,left);}
		    var prod_text = getProductText(prod_value);
		    
		    prod_obj.options[i]       = new Option();
		    prod_obj.options[i].value = prod_value;
		    prod_obj.options[i].text  = prod_text;

	    	}
	    }
	}
	
	// Layers populate, which depend on product selected
	document.getElementById(layerId).className="optioninvisible";  

	for ( isat=0; isat<satellite_list.length; isat++ ) {
	
	    if (satellite_list[isat].name == sat ) {
	  	
		var products = satellite_list[isat].products.split(" ");
	    	
		for ( i=0; i<products.length; i++ )  {
		
		    var left = products[i].indexOf("(");
		    var prod_value = products[i] ;
		    if ( left != -1 ) { prod_value = products[i].substring(0,left);}
		
		    if ( prod_value == prod && left > 0 ) {
                  	document.getElementById(layerId).className="optionvisible";  
                  	var right = products[i].indexOf(")");
                  	var layer_str = products[i].substring(left+1,right);
                  	var layers = layer_str.split(":");
		    	
		    	for ( j=0; j<layers.length; j++ ) {
		    	    layer_obj.options[j] = new Option();
		    	    layer_obj.options[j].value = layers[j];
		    	    layer_obj.options[j].text  = layers[j];
		    	}	
		    } 
	    	}
	    }
	}
	
	// update regions
	//populateRegions( sat, obj_region );
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

    yr.value = year;
    mo.selectedIndex = month ;
    dy.selectedIndex = day - 1;

    if      ( i == 1 ) loadImage1();
    else if ( i == 2 ) loadImage2();  
    else if ( i == 3 ) loadImage3();  
    else if ( i == 4 ) loadImage4();  
}


function animate()
{
   //increment image number
   current_image++;                      
   if (current_image >= last_image)
	current_image = 0;

   //  check to ensure that current image has not been deselected from the loop
   //  if it has, then find the next image that hasn't been
   while(imageNum1[current_image] == false || 
         imageNum2[current_image] == false ||
         imageNum3[current_image] == false ||
         imageNum4[current_image] == false ) {
	
	current_image++;
	
	if (current_image >= last_image)
		current_image = 0;
   }
   
   // only if 4 of them are all available
   if( imageNum1[current_image] == true && 
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
function loadImages(sat,region,prod,layer,cend,sfc,year,month,day, theImages, img_obj, imageNum, alts) 
{
  
   for (var i = first_image; i <= last_image; i++)
   {
      theImages[i-first_image] = new Image();
      
      var date      = shift(year,month,day,i-1);
      var year_tmp  = date[0];
      var month_tmp = date[1];
      var day_tmp   = date[2];
      
      var yyyy=String(year_tmp);
      
      var mm=String(month_tmp); 
      if ( month_tmp<10 ) { mm="0"+mm; }
      
      var dd=String(day_tmp);   
      if ( day_tmp<10   ) { dd="0"+dd; }
      
      //yyyy_indexes[i-first_image] = year_tmp - parseInt(document.form.yr1.options[0].value);
      yyyy_indexes[i-first_image] = year_tmp - YEAR_START;
      mm_indexes[i-first_image]   = month_tmp - 1;
      dd_indexes[i-first_image]   = day_tmp - 1;

      var image_name = loadImageHelper(sat,region,prod,layer,cend,sfc,yyyy,mm,dd);
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
   now.setDate(now.getDate()-31) ; // 31 days ago

   var year  = now.getFullYear();
   var month = now.getMonth()+1;  // month starting index is 0
   var day   = now.getDate();
   
   
   ///////////////////////////////////////////////
   // panel 1 elements
   ///////////////////////////////////////////////
   var sat = document.form.sat1.value;
   var region = document.form.region1.value;
   var prod = document.form.prod1.value;
   var layer= "";
   if (  document.form.layer1.className == "optionvisible" )
		layer = document.form.layer1.value;
   var cend = document.form.cend1.value;
   var sfc = document.form.sfc1.value;

   loadImages(sat,region,prod,layer,cend,sfc,year,month,day, images1, document.form.img1, imageNum1, alts1);
   
   
   ///////////////////////////////////////////////
   // panel 2 elements
   ///////////////////////////////////////////////
   sat = document.form.sat2.value;
   region = document.form.region2.value;
   prod = document.form.prod2.value;
   layer= "";
   if (  document.form.layer2.className == "optionvisible" )
		layer = document.form.layer2.value;
   cend = document.form.cend2.value;
   sfc  = document.form.sfc2.value;

   loadImages(sat,region,prod,layer,cend,sfc,year,month,day, images2, document.form.img2, imageNum2, alts2);
   
   
   ///////////////////////////////////////////////
   // panel 3 elements
   ///////////////////////////////////////////////
   sat  = document.form.sat3.value;
   region = document.form.region3.value;
   prod = document.form.prod3.value;
   layer= "";
   if (  document.form.layer3.className == "optionvisible" )
		layer = document.form.layer3.value;
   cend = document.form.cend3.value;
   sfc  = document.form.sfc3.value;

   loadImages(sat,region,prod,layer,cend,sfc,year,month,day, images3, document.form.img3, imageNum3, alts3);
   
   
   ///////////////////////////////////////////////
   // panel 4 elements
   ///////////////////////////////////////////////
   sat  = document.form.sat4.value;
   region = document.form.region4.value;
   prod = document.form.prod4.value;
   layer= "";
   if (  document.form.layer4.className == "optionvisible" )
		layer = document.form.layer4.value;
   cend = document.form.cend4.value;
   sfc  = document.form.sfc4.value;

   loadImages(sat,region,prod,layer,cend,sfc,year,month,day, images4, document.form.img4, imageNum4, alts4);
   
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
   var sat = document.form.sat1.value;
   var region = document.form.region1.value;
   var prod = document.form.prod1.value;
   var layer= "";
   if (  document.form.layer1.className == "optionvisible" )
		layer = document.form.layer1.value;
   var cend = document.form.cend1.value;
   var sfc  = document.form.sfc1.value;

   loadImages(sat,region,prod,layer,cend,sfc,year,month,day, images1, document.form.img1, imageNum1, alts1);
   
   
   ///////////////////////////////////////////////
   // animate only panel 1
   ///////////////////////////////////////////////
      
   animate1();
  
}


// Stop animation
function stopAnimation() {
	clearTimeout (timeID);
}


function setCount(count){
   	if(      count == 1 ) document.form.action="highresolution1.php"; 
   	else if( count == 2 ) document.form.action="highresolution.php"; 
   	else if( count == 4 ) document.form.action="highresolutionv.php"; 
}
