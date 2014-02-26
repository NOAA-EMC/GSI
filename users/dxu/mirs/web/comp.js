
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
var IMGDIR = "images/comp" ; 
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


var products_string = "chisq clw gs(cyl:pn:ps) iwp lwp psfc rr rwp sice(cyl:pn:ps) sicefy(cyl:pn:ps) sicemy(cyl:pn:ps) temp(100mb:200mb:300mb:400mb:500mb:600mb:700mb:800mb:850mb:900mb:950mb) swe(cyl:pn:ps) tpw tskin wv(100mb:200mb:300mb:400mb:500mb:600mb:700mb:800mb:850mb:900mb:950mb)";


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
var year;
var month;
var day;

var yyyy_indexes = new Array();
var mm_indexes   = new Array();
var dd_indexes   = new Array();


////////////////////////////////////////////////////////////////////////////////////////////////////
//
// set initial parameters
//
////////////////////////////////////////////////////////////////////////////////////////////////////
function init()
{ 
    sat                       = "comp" ; 
    prod                      = "tpw" ;   
    document.form1.prod.value = "tpw" ;
    layer                     = "200mb" ; 
    region                    = "glb" ; 
    cend                      = "ad" ;
    sfc                       = "all" ;

    var now = new Date(); 
    now.setDate(now.getDate() - 1) ; 	// default to yesterday

    year  = now.getFullYear();
    month = now.getMonth();		// month starting index is 0
    day   = now.getDate();

    document.form1.yr.value = year;
    document.form1.mo.selectedIndex = month;
    document.form1.dy.selectedIndex = day-1;
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
    
    //***** update href link, yyyy, mm and dd
    document.getElementById("href").href = theImages[current_image-first_image].src;
    document.form1.yr.selectedIndex = yyyy_indexes[current_image-first_image];
    document.form1.mo.selectedIndex = mm_indexes[current_image-first_image] ;
    document.form1.dy.selectedIndex = dd_indexes[current_image-first_image];
    
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

    //***** update href link, yyyy, mm and dd
    document.getElementById("href").href = theImages[current_image-first_image].src;
    document.form1.yr.selectedIndex = yyyy_indexes[current_image-first_image];
    document.form1.mo.selectedIndex = mm_indexes[current_image-first_image] ;
    document.form1.dy.selectedIndex = dd_indexes[current_image-first_image];
    
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
   
    //***** update href link, yyyy, mm and dd
    document.getElementById("href").href = theImages[current_image-first_image].src;
    document.form1.yr.selectedIndex = yyyy_indexes[current_image-first_image];
    document.form1.mo.selectedIndex = mm_indexes[current_image-first_image] ;
    document.form1.dy.selectedIndex = dd_indexes[current_image-first_image];
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

    //***** update href link, yyyy, mm and dd
    document.getElementById("href").href = theImages[current_image-first_image].src;
    document.form1.yr.selectedIndex = yyyy_indexes[current_image-first_image];
    document.form1.mo.selectedIndex = mm_indexes[current_image-first_image] ;
    document.form1.dy.selectedIndex = dd_indexes[current_image-first_image];
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
   last_image = 31;
   for (var i = first_image; i <= last_image+1; i++)
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


      yyyy_indexes[i-first_image] = year_tmp - 2011;
      mm_indexes[i-first_image]   = month_tmp - 1;
      dd_indexes[i-first_image]   = day_tmp - 1;


      if ( prod == "temp" || prod == "wv" || prod == "swe" || prod == "gs" || prod == "sice" || prod == "sicefy" || prod == "sicemy") {
        theImages[i-first_image].src = IMGDIR+ymd+"mirs_comp_glb_"+yyyy+mm+dd+"_"+prod+"_"+layer+"_"+sfc+"_"+cend+image_type;
      }
      else {
        theImages[i-first_image].src = IMGDIR+ymd+"mirs_comp_glb_"+yyyy+mm+dd+"_"+prod+"_"+sfc+"_"+cend+image_type;
      }
      
      imageNum[i-first_image] = true;
      document.animation.src = theImages[i-first_image].src;
      
      var url = document.animation.src;
      var index = url.lastIndexOf("/");
      var alt = url.substring(index+1,url.length); 
      document.animation.alt = alt;
   }
       
   // this needs to be done to set the right cend when the page is manually reloaded
   change_mode(1);

   // fwd();

}


////////////////////////////////////////////////////////////////////////////////////////////////////
//
// change product and popup layers/channels options if there are some
//
////////////////////////////////////////////////////////////////////////////////////////////////////
function changeProduct( product_value ) {
    
 	document.form1.layer.options.length = 0;  	
	document.form1.layer.className="optioninvisible";  

	var products = products_string.split(" ");

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
	
  	prod = product_value;

	if ( document.form1.layer.options.length > 0 && document.form1.layer.className == "optionvisible") {
	    layer = document.form1.layer.options[document.form1.layer.selectedIndex].value;
	}
	
	if ( product_value == "temp" || product_value == "wv" || product_value == "clwp" || product_value == "rainp" || product_value == "graupelp" ) {
		document.form1.layer.title = "select a layer";
	}
	else if ( product_value == "gs"   ||  product_value == "sfc"    || product_value == "sfc2" || product_value == "snow" || product_value == "swe" ||
	          product_value == "sice" ||  product_value == "sicefy" || product_value == "sicemy" ) {
		document.form1.layer.title = "select a map projection";
	}
	
	
	launch();   
    
}


////////////////////////////////////////////////////////////////////////////////////////////////////
//
// Load one image into 
//
////////////////////////////////////////////////////////////////////////////////////////////////////
function loadImage() 
{
   stop();
   
   var yyyy = document.form1.yr.value;
   var mm = document.form1.mo.value;
   var dd = document.form1.dy.value;
   var ymd = "/" + yyyy + "-" + mm + "-" + dd + "/";
   
   var img = "";
   
   if ( prod == "temp" || prod == "wv" || prod == "swe" || prod == "gs" || prod == "sice" || prod == "sicefy" || prod == "sicemy") {
       img = IMGDIR+ymd+"mirs_comp_glb_"+yyyy+mm+dd+"_"+prod+"_"+layer+"_"+sfc+"_"+cend+image_type;
   }
   else {
       img = IMGDIR+ymd+"mirs_comp_glb_"+yyyy+mm+dd+"_"+prod+"_"+sfc+"_"+cend+image_type;
   }

   document.animation.src = img;

   var url = document.animation.src;
   var index = url.lastIndexOf("/");
   var alt = url.substring(index+1,url.length); 
   document.animation.alt = alt;

}


