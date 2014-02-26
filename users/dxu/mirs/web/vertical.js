var IMGDIRS = new Array();

IMGDIRS["n18"]       = "images/" ;
IMGDIRS["n19"]       = "images/" ;
IMGDIRS["metopA"]    = "images/" ;
IMGDIRS["metopB"]    = "images/" ;
IMGDIRS["f16"]       = "images/" ;
IMGDIRS["f18"]       = "images/" ;
IMGDIRS["trmm"]      = "images/" ;
IMGDIRS["trmm2a12"]  = "images/" ;
IMGDIRS["npp"]       = "images/" ;
IMGDIRS["gcomw1"]    = "images/" ;

//var IMGDIR = "images/" ;

var ndayback = 4;

var SAT_TAG = new Array();
SAT_TAG["n18"]       = "mirs_adv_poes_n18_amsuamhs_";
SAT_TAG["n19"]       = "mirs_adv_poes_n19_amsuamhs_";
SAT_TAG["metopA"]    = "mirs_adv_poes_metopA_amsuamhs_";
SAT_TAG["metopB"]    = "mirs_adv_poes_metopB_amsuamhs_";
SAT_TAG["f16"]       = "mirs_adv_dmsp_f16_ssmis_";
SAT_TAG["f18"]       = "mirs_adv_dmsp_f18_ssmis_";
SAT_TAG["trmm"]      = "mirs_adv_eos_trmm_tmi_";
SAT_TAG["trmm2a12"]  = "mirs_trmm2a12_";
SAT_TAG["npp"]       = "mirs_adv_npoess_npp_atms_";
SAT_TAG["gcomw1"]    = "mirs_adv_poes_gcomw1_";

var SAT_TAG_VERT = new Array();
SAT_TAG_VERT["n18"]       = "mirs_adv_poes_n18_";
SAT_TAG_VERT["n19"]       = "mirs_adv_poes_n19_";
SAT_TAG_VERT["metopA"]    = "mirs_adv_poes_metopA_";
SAT_TAG_VERT["metopB"]    = "mirs_adv_poes_metopB_";
SAT_TAG_VERT["f16"]       = "mirs_adv_dmsp_f16_";
SAT_TAG_VERT["f18"]       = "mirs_adv_dmsp_f18_";
SAT_TAG_VERT["trmm"]      = "mirs_adv_eos_trmm_";
SAT_TAG_VERT["trmm2a12"]  = "mirs_trmm2a12_";
SAT_TAG_VERT["npp"]       = "mirs_adv_npoess_npp_";
SAT_TAG_VERT["gcomw1"]    = "mirs_adv_poes_gcomw1_";

var MAP_TAG = new Array();
MAP_TAG["temp"]     	= "_temp_500mb_";
MAP_TAG["wv"]       	= "_tpw_";
MAP_TAG["clwp"]     	= "_clw_";
MAP_TAG["rainp"]    	= "_rr_";
MAP_TAG["graupelp"] 	= "_iwp_";
MAP_TAG["rain_graupel"] = "_rr_";
MAP_TAG["scanday"]     	= "_temp_500mb_";

var PROD_NAME = new Array();
PROD_NAME['temp']		= "Temperature";
PROD_NAME['wv']			= "TPW";
PROD_NAME['clwp']		= "Cloud Liquid Water";
PROD_NAME['rainp']		= "Rain Rate";
PROD_NAME['graupelp']		= "Ice Water Path";
PROD_NAME['rain_graupel']	= "Rain Rate";


var PROD_VALUE = new Array('temp', 'wv', 'clwp', 'rainp', 'graupelp', 'rain_graupel');
var PROD_TEXT  = new Array('Temperature', 'Water Vapor Content', 'CLW', 'Rain', 'Graupel', 'Rain & Graupel'); 


//******************************************************************************************
// lat/lon definition for different regions. Implemented using associate array of arrays.
//******************************************************************************************

var LATS_GLB   = new Array('90','80','70','60','50','40','30','20','10','0','-10','-20','-30','-40','-50','-60','-70','-80','-90');
var LATS_GULF  = new Array('32','31','30','29','28','27','26','25','24','23','22','21','20','19','18','17');
var LATS_CHINA = new Array('35','34','33','32','31','30','29','28','27','26','25','24','23','22','21','20',
			   '19','18','17','16','15','14','13','12','11','10','9','8','7','6','5');
var LATS_US    = new Array('55','54','53','52','51','50','49','48','47','46','45','44','43','42','41','40',
                           '39','38','37','36','35','34','33','32','31','30',
			   '29','28','27','26','25','24','23','22','21','20');


var LATS = new Array();
LATS['glb']   =  LATS_GLB;
LATS['gulf']  =  LATS_GULF;
LATS['china'] =  LATS_CHINA;
LATS['us']    =  LATS_US;

var LONS_GLB = new Array('-180','-170','-160','-150','-140','-130','-120','-110','-100','-90','-80','-70','-60','-50','-40','-30','-20','-10',
		         '0','10','20','30','40','50','60','70','80','90','100','110','120','130','140','150','160','170','180');
var LONS_GULF = new Array('-98','-97','-96','-95', '-94', '-93', '-92', '-91', '-90', '-89',
			  '-88','-87','-86','-85', '-84', '-83', '-82', '-81', '-80', '-79',
			  '-78','-77','-76','-75', '-74', '-73', '-72', '-71', '-70', '-69', '-68');
var LONS_CHINA = new Array('110','111','112','113','114','115','116','117','118','119',
			   '120','121','122','123','124','125','126','127','128','129' );

var LONS_US    = new Array('-130','-129','-128','-127','-126','-125','-124','-123','-122','-121','-120',
                                  '-119','-118','-117','-116','-115','-114','-113','-112','-111','-110',
				  '-109','-108','-107','-106','-105','-104','-103','-102','-101','-100',
				  '-99','-98','-97','-96','-95','-94','-93','-92','-91','-90',
				  '-89','-88','-87','-86','-85','-84','-83','-82','-81','-80',
				  '-79','-78','-77','-76','-75','-74','-73','-72','-71','-70',
				  '-69','-68','-67','-66','-65','-64','-63','-62','-61','-60');

var LONS = new Array();
LONS['glb']   =  LONS_GLB;
LONS['gulf']  =  LONS_GULF;
LONS['china'] =  LONS_CHINA;
LONS['us']    =  LONS_US;



var swidth  = (screen.width);
var sheight = (screen.height);

// global x, y to denote mouse position
var x, y;


// to test web browser is IE or not
var IE = document.all?true:false;



//******************************************************************************************
// functions section
//******************************************************************************************


function getSatTxt( satVal ) {
 
    var txt = 'NOAA-18';
    
    if(      satVal == 'n18'    ) txt = 'NOAA-18';
    else if( satVal == 'n19'    ) txt = 'NOAA-19';
    else if( satVal == 'metopA' ) txt = 'METOP-A';
    else if( satVal == 'metopB' ) txt = 'METOP-B';
    else if( satVal == 'f16'    ) txt = 'F16/SSMIS';
    else if( satVal == 'f18'    ) txt = 'F18/SSMIS';
    else if( satVal == 'trmm'   ) txt = 'TRMM/TMI';
    else if( satVal == 'npp'    ) txt = 'NPP/ATMS';
    else if( satVal == 'gcomw1' ) txt = 'GCOMW1/AMSR2';
    
    return txt;
}


function getSatName( satVal ) {
 
    var txt = 'N18';
    
    if(      satVal == 'n18'    ) txt = 'NOAA-18';
    else if( satVal == 'n19'    ) txt = 'NOAA-19';
    else if( satVal == 'metopA' ) txt = 'METOP-A';
    else if( satVal == 'metopB' ) txt = 'METOP-B';
    else if( satVal == 'f16'    ) txt = 'F16/SSMIS';
    else if( satVal == 'f18'    ) txt = 'F18/SSMIS';
    else if( satVal == 'trmm'   ) txt = 'TRMM/TMI';
    else if( satVal == 'npp'    ) txt = 'NPP/ATMS';
    else if( satVal == 'gcomw1' ) txt = 'GCOMW1/AMSR2';

   return txt;

}



// populate lat/lon drop down menus according to regions/view selected
// adjust red line visibility and default location
function populateLatLon( view_obj, viewList_obj ) {

    var view = view_obj.value;
    viewList_obj.options.length = 0;
    
    region = document.form.region1.value;
    
    // drop down lat/lon menu populated
    if ( view == 'lat' ) {
        LATS_TMP = LATS[region];
	for( i = 0; i < LATS_TMP.length ; i++ ) {
    		viewList_obj.options[i] = new Option();
		viewList_obj.options[i].value = LATS_TMP[i];
		viewList_obj.options[i].text  = LATS_TMP[i];
	}
    }
    else if ( view == 'lon' ) {
        LONS_TMP = LONS[region];
	for( i = 0; i < LONS_TMP.length ; i++ ) {
    		viewList_obj.options[i] = new Option();
		viewList_obj.options[i].value = LONS_TMP[i];
		viewList_obj.options[i].text  = LONS_TMP[i];
	}
    }
    
    // visibility of red lines
    if ( view == 'lat' ) {
    	  document.getElementById("vertical_line").style.display    = "none"; 
    	  document.getElementById("vertical_line2").style.display   = "none"; 
    	  document.getElementById("vertical_line3").style.display   = "none"; 
    	  document.getElementById("vertical_line4").style.display   = "none"; 
    	  document.getElementById("vertical_line5").style.display   = "none"; 
    	  
	  document.getElementById("horizontal_line").style.display  = "block";
	  document.getElementById("horizontal_line2").style.display = "block";
	  document.getElementById("horizontal_line3").style.display = "block";
	  document.getElementById("horizontal_line4").style.display = "block";
	  document.getElementById("horizontal_line5").style.display = "block";
    }
    else {
    	  document.getElementById("vertical_line").style.display    = "block"; 
    	  document.getElementById("vertical_line2").style.display   = "block"; 
    	  document.getElementById("vertical_line3").style.display   = "block"; 
    	  document.getElementById("vertical_line4").style.display   = "block"; 
    	  document.getElementById("vertical_line5").style.display   = "block"; 
    	  
	  document.getElementById("horizontal_line").style.display  = "none";
	  document.getElementById("horizontal_line2").style.display = "none";
	  document.getElementById("horizontal_line3").style.display = "none";
	  document.getElementById("horizontal_line4").style.display = "none";
	  document.getElementById("horizontal_line5").style.display = "none";
    }
    
    // default location of red lines(always left most or top most position )
    if( view == 'lon' ) {
      document.getElementById("vertical_line").style.left  = 223 + 'px';
      document.getElementById("vertical_line2").style.left = 223 + 'px';
      document.getElementById("vertical_line3").style.left = 223 + 'px';
      document.getElementById("vertical_line4").style.left = 223 + 'px';
      document.getElementById("vertical_line5").style.left = 223 + 'px';
      //document.getElementById("vertical_line").style.top  = 188 + 'px';
    }
    else {
      document.getElementById("horizontal_line").style.top   = 200-14 + 'px';
      document.getElementById("horizontal_line2").style.top  = 350-14 + 'px';
      document.getElementById("horizontal_line3").style.top  = 500-14 + 'px';
      document.getElementById("horizontal_line4").style.top  = 650-14 + 'px';
      document.getElementById("horizontal_line5").style.top  = 800-14 + 'px';
      //document.getElementById("horizontal_line").style.left = 223 + 'px';
    }

}



function changeSlice() {
	
    var view   = document.form.view1.value;
    var slice  = document.form.slice1.value;
    var region = document.form.region1.value;
    
    map_dimx = 146.7;     // 163 * ( 0.95 - 0.05 )
    map_dimy = 93.75;     // 125 * ( 0.90 - 0.15 )
    
    // left = 215 ==> 0.05 * 163 = 8.15
    x00=215+8.15;
    y00=200;
    
    // this y00 changes with respect different mini-maps
    //if( y >= 185 && y <= 310 ) {
    //  y00 = 200 ;
    //}
    //else if( y >= 330 && y <= 460 ) {
    //  y00 = 350 ;
    //}
    //else if( y >= 495 && y<= 610 ) {
    //  y00 = 500;
    //}
    //else if( y >= 635 && y<= 760 ) {
    //  y00 = 650;
    //}
    //else if( y >= 785 && y<= 910 ) {
    //  y00 = 800;
    //}
    //else {
    //  y00 = -9999;
    //}
    
    y02=350;
    y03=500;
    y04=650;
    y05=800;
    
    x11=x00+map_dimx;
    y11=y00+map_dimy;
    
    lon_min=-180;
    lon_max=180;
    lat_min=-90;
    lat_max=90;
    step=10;

    if ( region == 'glb' ) {
    	step=10;
    	lon_min=-180;
    	lon_max=180;
    	lat_min=-90;
    	lat_max=90;
    }
    else if ( region == 'gulf' ) {
    	step=1;
    	lon_min=-98;
    	lon_max=-68;
    	lat_min=17;
    	lat_max=32;
    }
    else if ( region == 'china' ) {
    	step=1;
    	lon_min=110;
    	lon_max=130;
    	lat_min=5;
    	lat_max=35;
    }
    else if ( region == 'us' ) {
    	step=1;
    	lon_min=-130;
    	lon_max=-60;
    	lat_min=20;
    	lat_max=55;
    }
    
    var lon_span=lon_max-lon_min;
    var lat_span=lat_max-lat_min;
    
    if ( view == 'lat' ) 
    {
      var x = 215;
      var lat = slice;
      var y = (lat_max - lat)/lat_span * map_dimy + y00 ;
	
      document.getElementById("horizontal_line").style.display = "block"; 
      document.getElementById("horizontal_line").style.left = x + 'px';
      document.getElementById("horizontal_line").style.top  = (y-12) + 'px';
      
      y2 = y + 150;
      document.getElementById("horizontal_line2").style.display = "block"; 
      document.getElementById("horizontal_line2").style.left = x + 'px';
      document.getElementById("horizontal_line2").style.top  = (y2-12) + 'px';
      
      y3 = y + 300;
      document.getElementById("horizontal_line3").style.display = "block"; 
      document.getElementById("horizontal_line3").style.left = x + 'px';
      document.getElementById("horizontal_line3").style.top  = (y3-12) + 'px';
      
      y4 = y + 450;
      document.getElementById("horizontal_line4").style.display = "block"; 
      document.getElementById("horizontal_line4").style.left = x + 'px';
      document.getElementById("horizontal_line4").style.top  = (y4-12) + 'px';
      
      y5 = y + 600;
      document.getElementById("horizontal_line5").style.display = "block"; 
      document.getElementById("horizontal_line5").style.left = x + 'px';
      document.getElementById("horizontal_line5").style.top  = (y5-12) + 'px';
      
    }
    else if ( view == 'lon' ) 
    {
      var lon = slice;
      var x = x00 + (lon - lon_min) * map_dimx/lon_span ;

      document.getElementById("vertical_line").style.display   = "block"; 
      document.getElementById("vertical_line").style.left = x + 'px';
      document.getElementById("vertical_line").style.top  = (y00-12) + 'px';

      document.getElementById("vertical_line2").style.display   = "block"; 
      document.getElementById("vertical_line2").style.left = x + 'px';
      document.getElementById("vertical_line2").style.top  = (y02-12) + 'px';

      document.getElementById("vertical_line3").style.display   = "block"; 
      document.getElementById("vertical_line3").style.left = x + 'px';
      document.getElementById("vertical_line3").style.top  = (y03-12) + 'px';

      document.getElementById("vertical_line4").style.display   = "block"; 
      document.getElementById("vertical_line4").style.left = x + 'px';
      document.getElementById("vertical_line4").style.top  = (y04-12) + 'px';

      document.getElementById("vertical_line5").style.display   = "block"; 
      document.getElementById("vertical_line5").style.left = x + 'px';
      document.getElementById("vertical_line5").style.top  = (y05-12) + 'px';

    }	    

}



function loadImage1() {

	var sat1    = document.form.sat1.value;
	var prod1   = document.form.prod1.value;
	var view1   = document.form.view1.value;
	var slice1  = document.form.slice1.value;
	var region1 = document.form.region1.value;
	var cend1   = document.form.cend1.value;
	var yyyy1   = document.form.yr1.value;
	var mm1     = document.form.mo1.value;
	var dd1     = document.form.dy1.value;
	var ymd1    = "/" + yyyy1 + "-" + mm1 + "-" + dd1 + "/";
	
	var image1 = IMGDIRS[sat1]+sat1+ymd1+SAT_TAG_VERT[sat1]+"vert_"+region1+"_"+prod1+"_"+yyyy1+mm1+dd1+"_"+view1+"_"+slice1+"_"+cend1+".png";
	document.form.img1.src = image1;
	document.getElementById("href1").href = image1;
	var index = image1.lastIndexOf("/");
	var alt = image1.substring(index+1,image1.length);
	document.form.img1.alt = alt;
	
	// time plot
	//var image3 = IMGDIR+sat1+ymd1+SAT_TAG_VERT[sat1]+"trmm2a12_diff_vert_"+region1+"_scanday_"+yyyy1+mm1+dd1+"_"+view1+"_"+slice1+"_"+cend1+".png";
	var image3 = IMGDIRS[sat1]+sat1+ymd1+SAT_TAG_VERT[sat1]+"trmm2a12_diff_vert_"+region1+"_scanday_"+yyyy1+mm1+dd1+"_"+view1+"_"+slice1+"_as.png";
	document.form.img3.src = image3;
	document.getElementById("href3").href = image3;
	var index = image3.lastIndexOf("/");
	var alt = image3.substring(index+1,image3.length);
	document.form.img3.alt = alt;
}


function loadImage2() {

	var sat1    = document.form.sat1.value;
	var prod2   = document.form.prod2.value;
	var view1   = document.form.view1.value;
	var slice1  = document.form.slice1.value;
	var region1 = document.form.region1.value;
	var cend1   = document.form.cend1.value;
	var yyyy1   = document.form.yr1.value;
	var mm1     = document.form.mo1.value;
	var dd1     = document.form.dy1.value;
	var ymd1    = "/" + yyyy1 + "-" + mm1 + "-" + dd1 + "/";
	
	var layer1   = document.form.layer1.value;
	
	var image2 = "";
	
	if( layer1 == "trmm2a12" ) 
	    image2 = IMGDIRS[sat1]+layer1+ymd1+SAT_TAG_VERT[layer1]+"vert_"+region1+"_"+prod2+"_"+yyyy1+mm1+dd1+"_"+view1+"_"+slice1+"_"+cend1+".png"; 
	else
	    image2 = IMGDIRS[sat1]+sat1+ymd1+SAT_TAG_VERT[sat1]+layer1+"vert_"+region1+"_"+prod2+"_"+yyyy1+mm1+dd1+"_"+view1+"_"+slice1+"_"+cend1+".png";

	document.form.img2.src = image2;
	document.getElementById("href2").href = image2;
	
	var index = image2.lastIndexOf("/");
	var alt = image2.substring(index+1,image2.length);
	document.form.img2.alt = alt;
	
}



function changeSat( satVal ) {
	
	document.form.prod1.options.length=0;
	document.form.prod2.options.length=0;
	
	if( satVal == "trmm2a12" ) {
	    for(i=0; i<4; i++ ) {
	   	document.form.prod1.options[i] = new Option();
	   	document.form.prod1.options[i].value = PROD_VALUE[i+2];
	   	document.form.prod1.options[i].text  = PROD_TEXT[i+2];
	    }
	    for(i=0; i<4; i++ ) {
	   	document.form.prod2.options[i] = new Option();
	   	document.form.prod2.options[i].value = PROD_VALUE[i+2];
	   	document.form.prod2.options[i].text  = PROD_TEXT[i+2];
	    }
	}
	else {
	    for(i=0; i<PROD_VALUE.length; i++ ) {
	   	document.form.prod1.options[i] = new Option();
	   	document.form.prod1.options[i].value = PROD_VALUE[i];
	   	document.form.prod1.options[i].text  = PROD_TEXT[i];
	    }
	    for(i=0; i<PROD_VALUE.length; i++ ) {
	   	document.form.prod2.options[i] = new Option();
	   	document.form.prod2.options[i].value = PROD_VALUE[i];
	   	document.form.prod2.options[i].text  = PROD_TEXT[i];
	    }
	}
}


// the first option in the drop down menu is the same as selected satelliete sensor
// updated to reflect the changes in satellite changes.
function changeRef_1( satVal ) {
	
	
	document.form.layer1.options[0].value = "";
	document.form.layer1.options[0].text  = getSatTxt( satVal );
	
	var satName = getSatName( satVal );
	document.form.layer1.options[4].text  = satName + ' - GDAS';
	document.form.layer1.options[5].text  = satName + ' - ECMWF';
	document.form.layer1.options[6].text  = satName + ' - TRMM2A12';
	
}



function changeRef( refVal ) {
	
	document.form.prod2.options.length=0;
	
	// trmm2a12 has no rainp_graupel together since it has no temp in vertical direction
	if( refVal == "trmm2a12" ) {
	    for(i=0; i<3; i++ ) {
	   	document.form.prod2.options[i] = new Option();
	   	document.form.prod2.options[i].value = PROD_VALUE[i+2];
	   	document.form.prod2.options[i].text  = PROD_TEXT[i+2];
	    }
	}
	else if( refVal == "trmm2a12_diff_" ) {
	    for(i=0; i<4; i++ ) {
	   	document.form.prod2.options[i] = new Option();
	   	document.form.prod2.options[i].value = PROD_VALUE[i+2];
	   	document.form.prod2.options[i].text  = PROD_TEXT[i+2];
	    }
	}
	else if( refVal == "gdas_" || refVal == "gdas_diff_" ) {
	    for(i=0; i<2; i++ ) {
	   	document.form.prod2.options[i] = new Option();
	   	document.form.prod2.options[i].value = PROD_VALUE[i];
	   	document.form.prod2.options[i].text  = PROD_TEXT[i];
	    }
	}
	else {
	    for(i=0; i<PROD_VALUE.length; i++ ) {
	   	document.form.prod2.options[i] = new Option();
	   	document.form.prod2.options[i].value = PROD_VALUE[i];
	   	document.form.prod2.options[i].text  = PROD_TEXT[i];
	    }
	}
}



function changeMap() {

	var sat    = document.form.sat1.value;
	var prod   = document.form.prod1.value;
	var prod2  = document.form.prod2.value;
	var cend   = document.form.cend1.value;
	var yyyy   = document.form.yr1.value;
	var mm     = document.form.mo1.value;
	var dd     = document.form.dy1.value;
	var region = document.form.region1.value;
	var ymd    = "/" + yyyy + "-" + mm + "-" + dd + "/";
	
	var img_map  = IMGDIRS[sat]+sat+ymd+SAT_TAG[sat]+region+"_"+yyyy+mm+dd+MAP_TAG[prod]+"all_"+cend+".png";
	var img_map2 = IMGDIRS[sat]+sat+ymd+SAT_TAG[sat]+region+"_"+yyyy+mm+dd+"_clw_"+"all_"+cend+".png";
	var img_map3 = IMGDIRS[sat]+sat+ymd+SAT_TAG[sat]+region+"_"+yyyy+mm+dd+"_rwp_"+"all_"+cend+".png";
	var img_map4 = IMGDIRS[sat]+sat+ymd+SAT_TAG[sat]+region+"_"+yyyy+mm+dd+"_iwp_"+"all_"+cend+".png";
	//var img_map5 = IMGDIRS[sat]+sat+ymd+SAT_TAG['trmm2a12']+region+"_"+yyyy+mm+dd+MAP_TAG[prod2]+"all_"+cend+".png";
	var img_map5 = IMGDIRS[sat]+"trmm2a12"+ymd+SAT_TAG['trmm2a12']+region+"_"+yyyy+mm+dd+"_rr_all_"+cend+".png";
	
	document.getElementById("myMap").src  = img_map;
	document.getElementById("myMap2").src = img_map2;
	document.getElementById("myMap3").src = img_map3;
	document.getElementById("myMap4").src = img_map4;
	document.getElementById("myMap5").src = img_map5;
	
	document.getElementById("myMap").title = PROD_NAME[prod];
	document.getElementById("myMapTitle").innerHTML  = PROD_NAME[prod];
	//document.getElementById("myMap5Title").innerHTML = 'TRMM ' + PROD_NAME[prod2];
	
	document.getElementById("hrefa").href = img_map;
	document.getElementById("hrefb").href = img_map2;
	document.getElementById("hrefc").href = img_map3;
	document.getElementById("hrefd").href = img_map4;
	document.getElementById("hrefe").href = img_map5;

	
	viewList_obj = document.form.slice1;
	viewList_obj.options.length = 0;
	var view = document.form.view1.value;
	

    	if ( view == 'lat' ) {
    	    LATS_TMP = LATS[region];
	    for( i = 0; i < LATS_TMP.length ; i++ ) {
    		    viewList_obj.options[i] = new Option();
		    viewList_obj.options[i].value = LATS_TMP[i];
		    viewList_obj.options[i].text  = LATS_TMP[i];
	    }
    	}
    	else if ( view == 'lon' ) {
    	    LONS_TMP = LONS[region];
	    for( i = 0; i < LONS_TMP.length ; i++ ) {
    		    viewList_obj.options[i] = new Option();
		    viewList_obj.options[i].value = LONS_TMP[i];
		    viewList_obj.options[i].text  = LONS_TMP[i];
	    }
    	}


	if ( view == 'lat' ) {
    	      document.getElementById("vertical_line").style.display    = "none"; 
    	      document.getElementById("vertical_line2").style.display   = "none"; 
    	      document.getElementById("vertical_line3").style.display   = "none"; 
    	      document.getElementById("vertical_line4").style.display   = "none"; 
    	      document.getElementById("vertical_line5").style.display   = "none"; 

	      document.getElementById("horizontal_line").style.display  = "block";
	      document.getElementById("horizontal_line2").style.display = "block";
	      document.getElementById("horizontal_line3").style.display = "block";
	      document.getElementById("horizontal_line4").style.display = "block";
	      document.getElementById("horizontal_line5").style.display = "block";
	}
	else {
    	      document.getElementById("vertical_line").style.display    = "block"; 
    	      document.getElementById("vertical_line2").style.display   = "block"; 
    	      document.getElementById("vertical_line3").style.display   = "block"; 
    	      document.getElementById("vertical_line4").style.display   = "block"; 
    	      document.getElementById("vertical_line5").style.display   = "block"; 

	      document.getElementById("horizontal_line").style.display  = "none";
	      document.getElementById("horizontal_line2").style.display = "none";
	      document.getElementById("horizontal_line3").style.display = "none";
	      document.getElementById("horizontal_line4").style.display = "none";
	      document.getElementById("horizontal_line5").style.display = "none";
	}
}


function changeMap2() {

	var sat    = document.form.sat1.value;
	var prod   = document.form.prod1.value;
	var prod2  = document.form.prod2.value;
	var cend   = document.form.cend1.value;
	var yyyy   = document.form.yr1.value;
	var mm     = document.form.mo1.value;
	var dd     = document.form.dy1.value;
	var region = document.form.region1.value;
	var ymd    = "/" + yyyy + "-" + mm + "-" + dd + "/";
	
	var img_map  = IMGDIRS[sat]+sat+ymd+SAT_TAG[sat]+region+"_"+yyyy+mm+dd+MAP_TAG[prod]+"all_"+cend+".png";
	var img_map2 = IMGDIRS[sat]+sat+ymd+SAT_TAG[sat]+region+"_"+yyyy+mm+dd+"_clw_"+"all_"+cend+".png";
	var img_map3 = IMGDIRS[sat]+sat+ymd+SAT_TAG[sat]+region+"_"+yyyy+mm+dd+"_rwp_"+"all_"+cend+".png";
	var img_map4 = IMGDIRS[sat]+sat+ymd+SAT_TAG[sat]+region+"_"+yyyy+mm+dd+"_iwp_"+"all_"+cend+".png";
	//var img_map5 = IMGDIRS[sat]+sat+ymd+SAT_TAG['trmm2a12']+region+"_"+yyyy+mm+dd+MAP_TAG[prod2]+"all_"+cend+".png";
	var img_map5 = IMGDIRS[sat]+"trmm2a12"+ymd+SAT_TAG['trmm2a12']+region+"_"+yyyy+mm+dd+"_rr_all_"+cend+".png";
	
	document.getElementById("myMap").src = img_map;
	document.getElementById("myMap2").src = img_map2;
	document.getElementById("myMap3").src = img_map3;
	document.getElementById("myMap4").src = img_map4;
	document.getElementById("myMap5").src = img_map5;
	
	document.getElementById("myMap").title = PROD_NAME[prod];
	document.getElementById("myMapTitle").innerHTML  = PROD_NAME[prod];
	//document.getElementById("myMap5Title").innerHTML = 'TRMM ' + PROD_NAME[prod2];
	
	document.getElementById("hrefa").href = img_map;
	document.getElementById("hrefb").href = img_map2;
	document.getElementById("hrefc").href = img_map3;
	document.getElementById("hrefd").href = img_map4;
	document.getElementById("hrefe").href = img_map5;
	
}



function rev(yr,mo,dy) {
    
    var year  = parseInt(yr.value,10);
    var month = parseInt(mo.value,10);
    var day   = parseInt(dy.value,10);

    var now = new Date(year,month-1,day);
    now.setDate(now.getDate()-1);

    year  = now.getFullYear();
    month = now.getMonth();
    day   = now.getDate();

    yr.value = year ;
    mo.selectedIndex = month ;
    dy.selectedIndex = day - 1;

    changeMap2();
    loadImage1();
    loadImage2();

}



function fwd(yr,mo,dy) {
    
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
    
    changeMap2();
    loadImage1();
    loadImage2();

}



function getXY(e) {

    if ( IE ) {
	x = window.event.clientX + document.body.scrollLeft;
	y = window.event.clientY + document.body.scrollTop;     
    }
    else {
	x = e.pageX;
	y = e.pageY;    	
    }
    
    var sat1	= document.form.sat1.value;
    var prod1   = document.form.prod1.value;
    var view1	= document.form.view1.value;
    var region1 = document.form.region1.value;
    var cend1	= document.form.cend1.value;
    var yyyy1	= document.form.yr1.value;
    var mm1	= document.form.mo1.value;
    var dd1	= document.form.dy1.value;
    var ymd1	= "/" + yyyy1 + "-" + mm1 + "-" + dd1 + "/";
    
    var layer1  = document.form.layer1.value;
    var prod2   = document.form.prod2.value;
    
 
    // fine tuning is required here for different map image/line location
    // 163 X 125 map here
    // position=[0.05, 0.15, 0.95, 0.9]  The IDL map position  
   
    map_dimx = 146.7;     // 163 * ( 0.95 - 0.05 )
    map_dimy = 93.75;     // 125 * ( 0.90 - 0.15 )
    
    // left = 215 ==> 0.05 * 163 = 8.15
    x00=215+8.15;
    y00=200;
    
    // this y00 changes with respect different mini-maps
    if( y >= 185 && y <= 310 ) {
      y00 = 200 ;
    }
    else if( y >= 330 && y <= 460 ) {
      y00 = 350 ;
    }
    else if( y >= 495 && y<= 610 ) {
      y00 = 500;
    }
    else if( y >= 635 && y<= 760 ) {
      y00 = 650;
    }
    else if( y >= 785 && y<= 910 ) {
      y00 = 800;
    }
    else {
      y00 = -9999;
      //document.getElementById("vertical_line").style.display   = "none"; 
      //document.getElementById("horizontal_line").style.display = "none"; 
    }
    
    y01 = 200;
    y02 = 350;
    y03 = 500;
    y04 = 650;
    y05 = 800;
      
    x11=x00+map_dimx;
    
    y11=y01+map_dimy;
    y22=y02+map_dimy;
    y33=y03+map_dimy;
    y44=y04+map_dimy;
    y55=y05+map_dimy;
    
    
    y1 = y ;
    y2 = y + 150;
    y3 = y + 300;
    y4 = y + 450;
    y5 = y + 600;
    
    // corresponding y of 5 red lines 
    if( x >= x00 && x <= x11 && y >= y01 && y <= y11 ) {
    	y1 = y ;
    	y2 = y + 150;
    	y3 = y + 300;
    	y4 = y + 450;
    	y5 = y + 600;
    }
    else if( x >= x00 && x <= x11 && y >= y02 && y <= y22 ) {
    	y1 = y - 150;
    	y2 = y ;
    	y3 = y + 150;
    	y4 = y + 300;
    	y5 = y + 450;
    }
    else if( x >= x00 && x <= x11 && y >= y03 && y <= y33 ) {
    	y1 = y - 300;
    	y2 = y - 150;
    	y3 = y ;
    	y4 = y + 150;
    	y5 = y + 300;
    }
    else if( x >= x00 && x <= x11 && y >= y04 && y <= y44 ) {
    	y1 = y - 450;
    	y2 = y - 300;
    	y3 = y - 150;
    	y4 = y ;
    	y5 = y + 150;
    }
    else if( x >= x00 && x <= x11 && y >= y05 && y <= y55 ) {
    	y1 = y - 600;
    	y2 = y - 450;
    	y3 = y - 300;
    	y4 = y - 150;
    	y5 = y ;
    }
    else {
    	y1 = -9999;
    	y2 = -9999;
    	y3 = -9999;
    	y4 = -9999;
    	y5 = -9999;
    }
    
    // mouse cursor style changes: inside map use arrow, outside map but in frame then use pointer 
    if( x >= x00 && x <= x11 && y >= y01 && y <= y11 ) {
    	document.getElementById('myMap').style.cursor  = 'default';
    } 
    else {
    	document.getElementById('myMap').style.cursor  = 'pointer';
    }

    if( x >= x00 && x <= x11 && y >= y02 && y <= y22 ) {
    	document.getElementById('myMap2').style.cursor  = 'default';
    } 
    else {
    	document.getElementById('myMap2').style.cursor  = 'pointer';
    }

    if( x >= x00 && x <= x11 && y >= y03 && y <= y33 ) {
    	document.getElementById('myMap3').style.cursor  = 'default';
    } 
    else {
    	document.getElementById('myMap3').style.cursor  = 'pointer';
    }

    if( x >= x00 && x <= x11 && y >= y04 && y <= y44 ) {
    	document.getElementById('myMap4').style.cursor  = 'default';
    } 
    else {
    	document.getElementById('myMap4').style.cursor  = 'pointer';
    }

    if( x >= x00 && x <= x11 && y >= y05 && y <= y55 ) {
    	document.getElementById('myMap5').style.cursor  = 'default';
    } 
    else {
    	document.getElementById('myMap5').style.cursor  = 'pointer';
    }


    // 1st red line position
    document.getElementById("horizontal_line").style.left = 215 + "px";
    document.getElementById("horizontal_line").style.top  = (y1-12)+"px";
    document.getElementById("vertical_line").style.left   = x + "px";
    document.getElementById("vertical_line").style.top    = (y01-12)+"px";

    // 2nd red line position
    document.getElementById("horizontal_line2").style.left = 215 + "px";
    document.getElementById("horizontal_line2").style.top  = (y2-12)+"px";
    document.getElementById("vertical_line2").style.left   = x + "px";
    document.getElementById("vertical_line2").style.top    = (y02-12)+"px";

    // 3rd red line position
    document.getElementById("horizontal_line3").style.left = 215 + "px";
    document.getElementById("horizontal_line3").style.top  = (y3-12)+"px";
    document.getElementById("vertical_line3").style.left   = x + "px";
    document.getElementById("vertical_line3").style.top    = (y03-12)+"px";

    // 4th red line position
    document.getElementById("horizontal_line4").style.left = 215 + "px";
    document.getElementById("horizontal_line4").style.top  = (y4-12)+"px";
    document.getElementById("vertical_line4").style.left   = x + "px";
    document.getElementById("vertical_line4").style.top    = (y04-12)+"px";

    // 5th red line position
    document.getElementById("horizontal_line5").style.left = 215 + "px";
    document.getElementById("horizontal_line5").style.top  = (y5-12)+"px";
    document.getElementById("vertical_line5").style.left   = x + "px";
    document.getElementById("vertical_line5").style.top    = (y05-12)+"px";


    lon_min=-180;
    lon_max=180;
    lat_min=-90;
    lat_max=90;
    step=10;
    
    if ( region1 == 'glb' ) {
    	step=10;
	lon_min=-180;
	lon_max=180;
	lat_min=-90;
	lat_max=90;
    }
    else if ( region1 == 'gulf' ) {
    	step=1;
	lon_min=-98;
	lon_max=-68;
	lat_min=17;
	lat_max=32;
    }
    else if ( region1 == 'china' ) {
    	step=1;
	lon_min=110;
	lon_max=130;
	lat_min=5;
	lat_max=35;
    }
    else if ( region1 == 'us' ) {
    	step=1;
	lon_min=-130;
	lon_max=-60;
	lat_min=20;
	lat_max=55;
    }
	

    // 5 lines visibility setup
    if (  view1 == 'lat' ) {
      document.getElementById("vertical_line").style.display   = "none"; 
      document.getElementById("vertical_line2").style.display  = "none"; 
      document.getElementById("vertical_line3").style.display  = "none"; 
      document.getElementById("vertical_line4").style.display  = "none"; 
      document.getElementById("vertical_line5").style.display  = "none";
      
      if( x >= x00 && x <= x11 && (  ( y >= y01 && y <= y11 ) || 
      				     ( y >= y02 && y <= y22 ) || 
				     ( y >= y03 && y <= y33 ) ||
				     ( y >= y04 && y <= y44 ) ||
				     ( y >= y05 && y <= y55 ) )  ) 
      {
	document.getElementById("horizontal_line").style.display   = "block"; 
      	document.getElementById("horizontal_line2").style.display  = "block"; 
      	document.getElementById("horizontal_line3").style.display  = "block"; 
      	document.getElementById("horizontal_line4").style.display  = "block"; 
      	document.getElementById("horizontal_line5").style.display  = "block"; 
      }
      else 
      {
	document.getElementById("horizontal_line").style.display   = "none"; 
      	document.getElementById("horizontal_line2").style.display  = "none"; 
      	document.getElementById("horizontal_line3").style.display  = "none"; 
      	document.getElementById("horizontal_line4").style.display  = "none"; 
      	document.getElementById("horizontal_line5").style.display  = "none"; 
      }
    }
    else if ( view1 == 'lon' ) {   
      document.getElementById("horizontal_line").style.display   = "none"; 
      document.getElementById("horizontal_line2").style.display  = "none"; 
      document.getElementById("horizontal_line3").style.display  = "none"; 
      document.getElementById("horizontal_line4").style.display  = "none"; 
      document.getElementById("horizontal_line5").style.display  = "none";
      
      if( x >= x00 && x <= x11 && (  ( y >= y01 && y <= y11 ) || 
      				     ( y >= y02 && y <= y22 ) || 
				     ( y >= y03 && y <= y33 ) ||
				     ( y >= y04 && y <= y44 ) ||
				     ( y >= y05 && y <= y55 ) )  ) 
      {
	document.getElementById("vertical_line").style.display   = "block"; 
      	document.getElementById("vertical_line2").style.display  = "block"; 
      	document.getElementById("vertical_line3").style.display  = "block"; 
      	document.getElementById("vertical_line4").style.display  = "block"; 
      	document.getElementById("vertical_line5").style.display  = "block"; 
      }
      else {
	document.getElementById("vertical_line").style.display   = "none"; 
      	document.getElementById("vertical_line2").style.display  = "none"; 
      	document.getElementById("vertical_line3").style.display  = "none"; 
      	document.getElementById("vertical_line4").style.display  = "none"; 
      	document.getElementById("vertical_line5").style.display  = "none"; 
      }
    }
    
    
    // now compute lat/lon 
    lon_span=lon_max-lon_min;
    lat_span=lat_max-lat_min;

    lon = Math.round( ((x-x00) * lon_span/map_dimx + lon_min)/step) * step;
    lat = Math.round( (lat_max - (y-y00) * lat_span/map_dimy)/step) * step;

    if ( lon < lon_min  ) lon = lon_min;
    if ( lon > lon_max  ) lon = lon_max;
    if ( lat < lat_min  ) lat = lat_min;
    if ( lat > lat_max  ) lat = lat_max;
    
    var slice1 = '';
    if ( view1 == 'lat' ) {
	slice1 = String(lat);
    }       
    else if ( view1 == 'lon' ) {
	slice1 = String(lon);
    }

    var image1 = IMGDIRS[sat1]+sat1+ymd1+SAT_TAG_VERT[sat1]+"vert_"+region1+"_"+prod1+"_"+yyyy1+mm1+dd1+"_"+view1+"_"+slice1+"_"+cend1+".png";

    var image2 = "";
    if( layer1 == "trmm2a12" ) 
    	image2 = IMGDIRS[sat1]+layer1+ymd1+SAT_TAG_VERT[layer1]+"vert_"+region1+"_"+prod2+"_"+yyyy1+mm1+dd1+"_"+view1+"_"+slice1+"_"+cend1+".png"; 
    else
    	image2 = IMGDIRS[sat1]+sat1+ymd1+SAT_TAG_VERT[sat1]+layer1+"vert_"+region1+"_"+prod2+"_"+yyyy1+mm1+dd1+"_"+view1+"_"+slice1+"_"+cend1+".png";

    //image3 = IMGDIRS[sat1]+sat1+ymd1+SAT_TAG_VERT[sat1]+"trmm2a12_diff_vert_"+region1+"_scanday_"+yyyy1+mm1+dd1+"_"+view1+"_"+slice1+"_"+cend1+".png";
    image3 = IMGDIRS[sat1]+sat1+ymd1+SAT_TAG_VERT[sat1]+"trmm2a12_diff_vert_"+region1+"_scanday_"+yyyy1+mm1+dd1+"_"+view1+"_"+slice1+"_as.png";

    // image loading and lat/lon drop down menu change
    if( x >= x00 && x <= x11 && (  ( y >= y01 && y <= y11 ) || 
      				   ( y >= y02 && y <= y22 ) || 
				   ( y >= y03 && y <= y33 ) ||
				   ( y >= y04 && y <= y44 ) ||
				   ( y >= y05 && y <= y55 ) )  ) 
    {	
	document.form.slice1.value = slice1;
	document.form.img1.src = image1;
	document.getElementById("href1").href = image1;
	var index = image1.lastIndexOf("/");
	var alt1 = image1.substring(index+1,image1.length);
	document.form.img1.alt = alt1;
	
	document.form.img2.src = image2;
	document.getElementById("href2").href = image2;
	var index = image2.lastIndexOf("/");
	var alt2 = image2.substring(index+1,image2.length);
	document.form.img2.alt = alt2;
	
	document.form.img3.src = image3;
	document.getElementById("href3").href = image3;
	var index = image3.lastIndexOf("/");
	var alt3 = image3.substring(index+1,image3.length);
	document.form.img3.alt = alt3;
	
    }

}



function init() {

    // default to lat(horizontal line), so make lon(vertical line invisible)
    document.getElementById("vertical_line").style.display  = "none"; 
    document.getElementById("vertical_line2").style.display = "none"; 
    document.getElementById("vertical_line3").style.display = "none"; 
    document.getElementById("vertical_line4").style.display = "none"; 
    document.getElementById("vertical_line5").style.display = "none"; 
   
    // lat 60 corresponding position in those mini-maps
    document.getElementById("horizontal_line").style.top  = 200 + 3 + "px";
    document.getElementById("horizontal_line2").style.top = 350 + 3 + "px";
    document.getElementById("horizontal_line3").style.top = 500 + 3 + "px";
    document.getElementById("horizontal_line4").style.top = 650 + 3 + "px";
    document.getElementById("horizontal_line5").style.top = 800 + 3 + "px";

    var now = new Date();
    now.setDate(now.getDate()-ndayback) ; // n days ago

    var year  = now.getFullYear();
    var month = now.getMonth();  // month starting index is 0
    var day   = now.getDate();

    document.form.yr1.value = String( year );
    document.form.mo1.selectedIndex = month ;
    document.form.dy1.selectedIndex = day - 1;
    document.form.sat1.selectedIndex=0;
    document.form.prod1.selectedIndex=0;
    document.form.view1.selectedIndex=0;
    document.form.slice1.selectedIndex=3;
    document.form.region1.selectedIndex=0;
    document.form.cend1.selectedIndex=0;
   
    document.form.layer1.selectedIndex=0;
    document.form.prod2.selectedIndex=1;
    
    loadImage1();
    loadImage2();
    
    // web browser dependent event handler methods
    if( IE ) {
	document.attachEvent( "onmousemove", getXY );
	//document.attachEvent( "click", getXYClicked );
    }
    else  {
	document.addEventListener( "mousemove", getXY, false );
	//document.addEventListener( "click", getXYClicked, false );
    }
    
    // default map on the upper left	
    var yyyy   = document.form.yr1.value;
    var mm     = document.form.mo1.value;
    var dd     = document.form.dy1.value;
    var sat    = document.form.sat1.value;
    var region = document.form.region1.value;
    var prod   = document.form.prod1.value;
    var cend   = document.form.cend1.value;
    var ymd    = "/" + yyyy + "-" + mm + "-" + dd + "/";
    
    var img_map  = IMGDIRS[sat]+sat+ymd+SAT_TAG[sat]+region+"_"+yyyy+mm+dd+MAP_TAG[prod]+"all_"+cend+".png";
    var img_map2 = IMGDIRS[sat]+sat+ymd+SAT_TAG[sat]+region+"_"+yyyy+mm+dd+"_clw_"+"all_"+cend+".png";
    var img_map3 = IMGDIRS[sat]+sat+ymd+SAT_TAG[sat]+region+"_"+yyyy+mm+dd+"_rwp_"+"all_"+cend+".png";
    var img_map4 = IMGDIRS[sat]+sat+ymd+SAT_TAG[sat]+region+"_"+yyyy+mm+dd+"_iwp_"+"all_"+cend+".png";
    var img_map5 = IMGDIRS[sat]+"trmm2a12"+ymd+SAT_TAG['trmm2a12']+region+"_"+yyyy+mm+dd+"_rr_"+"all_"+cend+".png";
    
    document.getElementById("myMap").src  = img_map;
    document.getElementById("myMap2").src = img_map2;
    document.getElementById("myMap3").src = img_map3;
    document.getElementById("myMap4").src = img_map4;
    document.getElementById("myMap5").src = img_map5;
    
    // hyper link full-size image
    document.getElementById("hrefa").href = img_map;
    document.getElementById("hrefb").href = img_map2;
    document.getElementById("hrefc").href = img_map3;
    document.getElementById("hrefd").href = img_map4;
    document.getElementById("hrefe").href = img_map5;
    
    var past = new Date();
    past.setDate(past.getDate()-ndayback) ; // n days ago

    var year  = past.getFullYear();
    var month = past.getMonth()+1;  // month starting index is 0
    var day   = past.getDate();
    
    var ymd2 = String(month) + '/' + String(day) + '/' + String(year);
    document.getElementById("myMap5").alt = 'latest TRMM map available date is: ' + ymd2 + ' due to TRMM data delay';
    
    
}
