var imgDir = "images/n18";
var header = "mirs_adv_poes_n18_sim_";
var ecmwf_p2p = '_ecmwfdaily_glb_p2p_20090215_';
var ecmwf_glb = '_ecmwfdaily_glb_20090215_';
var ndayback  = 3;

var products = new Array(
'temp_mean_vert', 'temp_stdv_vert', 'wv_mean_vert', 'wv_stdv_vert', 'clw',
'tb(ch4:ch5:ch6:ch7:ch8:ch9:ch10:ch18)', 'temp(10mb:100mb:300mb:500mb:1000mb)', 
'tpwPerf', 'tskinPerf', 'wv(10mb:100mb:300mb:500mb:1000mb)',
'map_temp_mirs(300mb:500mb:700mb:850mb:950mb)','map_temp_model(300mb:500mb:700mb:850mb:950mb)','map_temp_diff(300mb:500mb:700mb:850mb:950mb)',
'map_wv_mirs(300mb:500mb:700mb:850mb:950mb)','map_wv_model(300mb:500mb:700mb:850mb:950mb)','map_wv_diff(300mb:500mb:700mb:850mb:950mb)',
'map_em_mirs(23v:31v:89v1)','map_em_model(23v:31v:89v1)','map_em_diff(23v:31v:89v1)',
'map_tskin_mirs','map_tskin_model','map_tskin_diff',
'map_tpw_mirs','map_tpw_model','map_tpw_diff',
'map_clw_mirs','map_clw_model','map_clw_diff',
'map_sice_mirs(cyl)',
'map_sicefy_mirs(cyl)',
'map_sicemy_mirs(cyl)',
'map_swe_mirs(cyl)',
'map_sfcTyp_mirs(cyl)',
'map_sfcTyp2_mirs(cyl)',
'map_niter_mirs',
'map_nattempt_mirs');


var HEADERS = new Array();
HEADERS['mirs']  = "mirs_adv_poes_n18_amsuamhs_sim_"; 
HEADERS['model'] = "mirs_adv_poes_n18_ecmwf_sim_";
HEADERS['diff']  = "mirs_adv_poes_n18_ecmwf_bias_sim_";


function getText( val ) {

	var txt = "";

	if            ( val == "tpwPerf"   	    ) txt = "Scattered TPW(MIRS vs model)"		;
	else if       ( val == "clwPerf" 	    ) txt = "Scattered CLW(MIRS vs model)"		;
	else if       ( val == "tskinPerf" 	    ) txt = "Scattered Skin Temp(MIRS vs model)" 	;
	else if       ( val == "tb"    		    ) txt = "Scattered TB(curr vs prev)" 		;
	else if       ( val == "temp"  		    ) txt = "Scattered Temp(curr vs prev)"     		;
	else if       ( val == "wv"    		    ) txt = "Scattered Water Vapor(curr vs prev)"	;
	else if       ( val == "temp_mean_vert"     ) txt = "Temp Bias(MIRS vs model)"  	    	;
	else if       ( val == "temp_stdv_vert"     ) txt = "Temp Stdv(MIRS vs model)"	    		;
	else if       ( val == "wv_mean_vert"	    ) txt = "Water Vapor Bias(MIRS vs model)" 		;
	else if       ( val == "wv_stdv_vert"	    ) txt = "Water Vapor Stdv(MIRS vs model)"		;
	
	else if       ( val == "map_temp_mirs"	    ) txt = "Map:Temp(MIRS)"		;
	else if       ( val == "map_temp_model"	    ) txt = "Map:Temp(model)"		;
	else if       ( val == "map_temp_diff"	    ) txt = "Map:Temp(MIRS-model)"	;
	
	else if       ( val == "map_wv_mirs"	    ) txt = "Map:WV(MIRS)"		;
	else if       ( val == "map_wv_model"	    ) txt = "Map:WV(model)"		;
	else if       ( val == "map_wv_diff"	    ) txt = "Map:WV(MIRS-model)"	;
	
	else if       ( val == "map_em_mirs"	    ) txt = "Map:Emissivity(MIRS)"	;
	else if       ( val == "map_em_model"	    ) txt = "Map:Emissivity(model)"	;
	else if       ( val == "map_em_diff"	    ) txt = "Map:Emissivity(MIRS-model)";
	
	else if       ( val == "map_tskin_mirs"	    ) txt = "Map:Skin Temp(MIRS)"	;
	else if       ( val == "map_tskin_model"    ) txt = "Map:Skin Temp(model)"	;
	else if       ( val == "map_tskin_diff"	    ) txt = "Map:Skin Temp(MIRS-model)"	;
	
	else if       ( val == "map_tpw_mirs"	    ) txt = "Map:TPW(MIRS)"		;
	else if       ( val == "map_tpw_model"      ) txt = "Map:TPW(model)"		;
	else if       ( val == "map_tpw_diff"	    ) txt = "Map:TPW(MIRS-model)"	;
	
	else if       ( val == "map_clw_mirs"	    ) txt = "Map:CLW(MIRS)"		;
	else if       ( val == "map_clw_model"      ) txt = "Map:CLW(model)"		;
	else if       ( val == "map_clw_diff"	    ) txt = "Map:CLW(MIRS-model)"	;
	
	else if       ( val == "map_sice_mirs"	    ) txt = "Map:Sea Ice(MIRS)"		;
	else if       ( val == "map_sice_model"     ) txt = "Map:Sea Ice(model)"	;
	else if       ( val == "map_sice_diff"	    ) txt = "Map:Sea Ice(MIRS-model)"	;
	
	else if       ( val == "map_sicefy_mirs"    ) txt = "Map:First Yr Sea Ice(MIRS)"	;
	else if       ( val == "map_sicefy_model"   ) txt = "Map:First Yr Sea Ice(model)"	;
	else if       ( val == "map_sicefy_diff"    ) txt = "Map:First Yr Sea Ice(MIRS-model)"	;
	
	else if       ( val == "map_sicemy_mirs"    ) txt = "Map:Multiple Yr Sea Ice(MIRS)"	;
	else if       ( val == "map_sicemy_model"   ) txt = "Map:Multiple Yr Sea Ice(model)"	;
	else if       ( val == "map_sicemy_diff"    ) txt = "Map:Multiple Yr Sea Ice(MIRS-model)";
	
	else if       ( val == "map_swe_mirs"	    ) txt = "Map:SWE(MIRS)"	;
	else if       ( val == "map_swe_model"      ) txt = "Map:SWE(model)"	;
	else if       ( val == "map_swe_diff"	    ) txt = "Map:SWE(MIRS-model)"	;
	
	else if       ( val == "map_sfcTyp_mirs"    ) txt = "Map:Pre-Classified Sfc Type(MIRS)"	;
	else if       ( val == "map_sfcTyp_model"   ) txt = "Map:Pre-Classified Sfc Type(model)"	;
	else if       ( val == "map_sfcTyp_diff"    ) txt = "Map:Pre-Classified Sfc Type(MIRS-model)"	;
	
	else if       ( val == "map_sfcTyp2_mirs"   ) txt = "Map:Post-Process Sfc Type(MIRS)"	;
	else if       ( val == "map_sfcTyp2_model"  ) txt = "Map:Post-Process Sfc Type(model)"	;
	else if       ( val == "map_sfcTyp2_diff"   ) txt = "Map:Post-Process Sfc Type(MIRS-model)"	;
	
	else if       ( val == "map_niter_mirs"     ) txt = "Map:Iteration Number(MIRS)"	;
	else if       ( val == "map_niter_model"    ) txt = "Map:Iteration Number(model)"	;
	else if       ( val == "map_niter_diff"     ) txt = "Map:Iteration Number(MIRS-model)"	;
	
	else if       ( val == "map_nattempt_mirs"  ) txt = "Map:Attempt Number(MIRS)"	;
	else if       ( val == "map_nattempt_model" ) txt = "Map:Attempt Number(model)"	;
	else if       ( val == "map_nattempt_diff"  ) txt = "Map:Attempt Number(MIRS-model)"	;
	
	return txt;

}


function changeProduct( prod_obj, layer_obj, layer_id, cend_obj ) {

	var prod_val  = prod_obj.value;

	var layer_old_val = layer_obj.value;
	var layer_old_exist = 0 ;
	
	layer_obj.options.length = 0; 
	document.getElementById(layer_id).className ="optioninvisible";

	for ( i=0; i<products.length; i++ )  {
	    var left = products[i].indexOf("(");
	    var prod_value = products[i] ;
	    if ( left != -1 ) { prod_value = products[i].substring(0,left);}

	    if ( prod_value == prod_val && left > 0 ) {
	        document.getElementById(layer_id).className ="optionvisible";  
	        
	    	var right = products[i].indexOf(")");
	    	var layer_str = products[i].substring(left+1,right);
	    	var layers = layer_str.split(":");
	    	
	    	for ( j=0; j<layers.length; j++ ) {
		    layer_obj.options[j] = new Option();
		    layer_obj.options[j].value = layers[j];
	    	    layer_obj.options[j].text  = layers[j];
		    if( layers[j] == layer_old_val ) { layer_old_exist = 1; }
	    	}
	    } 
	}

	// if layer list has old layer, we use old one; otherwise, use the 1st one in the layer list
	if ( layer_old_exist == 1 ) { 
		layer_obj.value = layer_old_val; 
	}

	
	var cend_old_val = cend_obj.value;
	var cend_old_exist = 0;
	
	// adjust cending if product contains map_ or not 
	var pos = prod_val.indexOf("map_");
	if( pos >= 0 ) {
		cend_obj.options.length = 0;
		
		cend_obj.options[0] = new Option(); 	
		cend_obj.options[0].value = "as";	
		cend_obj.options[0].text = "Asc";
		
		cend_obj.options[1] = new Option(); 	
		cend_obj.options[1].value = "ds";	
		cend_obj.options[1].text = "Des";
		
		if( cend_old_val == "as" || cend_old_val == "ds" ) cend_old_exist = 1;
	}
	else {
		cend_obj.options.length = 0;
		cend_obj.options[0] = new Option(); 	
		cend_obj.options[0].value = "ad";	
		cend_obj.options[0].text = "Comb";
		
		if( cend_old_val == "ad" ) cend_old_exist = 1;
	}
	
	if( cend_old_exist == 1 ) cend_obj.value = cend_old_val;
	
} 



function loadImage1() {
 	var cend 	= document.form.cend1.value;
 	var sfc  	= document.form.sfc1.value;
 	var prod  	= document.form.prod1.value;
	var layer 	= document.form.layer1.value;
	var cond 	= document.form.cond1.value;
	var yr 		= document.form.yr1.value;
	var mo 		= document.form.mo1.value;
	var dy 		= document.form.dy1.value;
	loadImageHelp(cend,sfc,prod,layer,cond,yr,mo,dy,document.form.img1,'href1');
}

function loadImage2() {
 	var cend 	= document.form.cend2.value;
 	var sfc  	= document.form.sfc2.value;
 	var prod  	= document.form.prod2.value;
	var layer 	= document.form.layer2.value;
	var cond 	= document.form.cond2.value;
	var yr 		= document.form.yr2.value;
	var mo 		= document.form.mo2.value;
	var dy 		= document.form.dy2.value;
	loadImageHelp(cend,sfc,prod,layer,cond,yr,mo,dy,document.form.img2,'href2');
}

function loadImage3() {
 	var cend 	= document.form.cend3.value;
 	var sfc  	= document.form.sfc3.value;
 	var prod  	= document.form.prod3.value;
	var layer 	= document.form.layer3.value;
	var cond 	= document.form.cond3.value;
	var yr 		= document.form.yr3.value;
	var mo 		= document.form.mo3.value;
	var dy 		= document.form.dy3.value;
	loadImageHelp(cend,sfc,prod,layer,cond,yr,mo,dy,document.form.img3,'href3');
}

function loadImage4() {
 	var cend 	= document.form.cend4.value;
 	var sfc  	= document.form.sfc4.value;
 	var prod  	= document.form.prod4.value;
	var layer 	= document.form.layer4.value;
	var cond 	= document.form.cond4.value;
	var yr 		= document.form.yr4.value;
	var mo 		= document.form.mo4.value;
	var dy 		= document.form.dy4.value;
	loadImageHelp(cend,sfc,prod,layer,cond,yr,mo,dy,document.form.img4,'href4');
}

function loadImage5() {
 	var cend 	= document.form.cend5.value;
 	var sfc  	= document.form.sfc5.value;
 	var prod  	= document.form.prod5.value;
	var layer 	= document.form.layer5.value;
	var cond 	= document.form.cond5.value;
	var yr 		= document.form.yr5.value;
	var mo 		= document.form.mo5.value;
	var dy 		= document.form.dy5.value;
	loadImageHelp(cend,sfc,prod,layer,cond,yr,mo,dy,document.form.img5,'href5');
}

function loadImage6() {
 	var cend 	= document.form.cend6.value;
 	var sfc  	= document.form.sfc6.value;
 	var prod  	= document.form.prod6.value;
	var layer 	= document.form.layer6.value;
	var cond 	= document.form.cond6.value;
	var yr 		= document.form.yr6.value;
	var mo 		= document.form.mo6.value;
	var dy 		= document.form.dy6.value;
	loadImageHelp(cend,sfc,prod,layer,cond,yr,mo,dy,document.form.img6,'href6');
}




function loadImageHelp(cend,sfc,prod,layer,cond,yr,mo,dy,img_obj,href_id) {
	
	var img_old  = img_obj.src;
	var pos_dot  = img_old.lastIndexOf(".");
	var cend_old = img_old.substring(pos_dot-2,pos_dot);
	
	var ymd =  "/" + yr + "-" + mo + "-" + dy + "/";
	var dash  = "_";
	var img="";

	if ( prod == "temp" || prod == "wv" || prod == "tb") {
		img = imgDir+ymd+header+cond+ecmwf_p2p+yr+mo+dy+dash+prod+dash+layer+'_sea_ad.png';
	}
	else if ( prod == "tpwPerf" || prod == "tskinPerf" ) {
		img = imgDir+ymd+header+cond+ecmwf_p2p+yr+mo+dy+dash+prod+'_'+sfc+'_ad.png';
	}
	else if ( prod == "clwPerf" ) {
		img = imgDir+ymd+header+'cld'+ecmwf_p2p+yr+mo+dy+dash+prod+'_sea_ad.png';
	}
	else if ( prod == "temp_mean_vert" || prod == "temp_stdv_vert" || 
		  prod == "wv_mean_vert"   || prod == "wv_stdv_vert" ) {
		img = imgDir+ymd+header+cond+ecmwf_p2p+yr+mo+dy+dash+prod+'_all_ad.png';
	}
	
	else if ( prod == "map_temp_mirs" || prod == "map_temp_model" || prod == "map_temp_diff" ||
		  prod == "map_wv_mirs"   || prod == "map_wv_model"   || prod == "map_wv_diff" ||
		  prod == "map_em_mirs"   || prod == "map_em_model"   || prod == "map_em_diff" ) {
		
		sfc = "all";
		if( cend == "ad" && cend_old != "ad" ) cend = cend_old;
		if( cend == "ad" && cend_old == "ad" ) cend = "as";
		var prods = prod.split("_");
		var prod_id = prods[1];
		var prod_src = prods[2];
		img = imgDir+ymd+HEADERS[prod_src]+cond+ecmwf_glb+yr+mo+dy+dash+prod_id+dash+layer+dash+sfc+dash+cend+'.png';
	
	}
	else if ( 
		prod == "map_sice_mirs"   || prod == "map_sice_model"   || prod == "map_sice_diff"    ||
		prod == "map_sicefy_mirs" || prod == "map_sicefy_model" || prod == "map_sicefy_diff"  ||
		prod == "map_sicemy_mirs" || prod == "map_sicemy_model" || prod == "map_sicemy_diff" ) {
		  
		sfc = "sea";
		if( cend == "ad" && cend_old != "ad" ) cend = cend_old;
		if( cend == "ad" && cend_old == "ad" ) cend = "as";
		var prods = prod.split("_");
		var prod_id = prods[1];
		var prod_src = prods[2];
		img = imgDir+ymd+HEADERS[prod_src]+cond+ecmwf_glb+yr+mo+dy+dash+prod_id+dash+layer+dash+sfc+dash+cend+'.png';
	}
	else if ( prod == "map_swe_mirs"   || prod == "map_swe_model"   || prod == "map_swe_diff" ) {
		sfc = "lnd";
		if( cend == "ad" && cend_old != "ad" ) cend = cend_old;
		if( cend == "ad" && cend_old == "ad" ) cend = "as";
		var prods = prod.split("_");
		var prod_id = prods[1];
		var prod_src = prods[2];
		img = imgDir+ymd+HEADERS[prod_src]+cond+ecmwf_glb+yr+mo+dy+dash+prod_id+dash+layer+dash+sfc+dash+cend+'.png';
	}
	
	else if ( prod == "map_sfcTyp_mirs"   || prod == "map_sfcTyp_model"   || prod == "map_sfcTyp_diff" ||
		  prod == "map_sfcTyp2_mirs"  || prod == "map_sfcTyp2_model"  || prod == "map_sfcTyp2_diff") {
		sfc = "all";
		if( cend == "ad" && cend_old != "ad" ) cend = cend_old;
		if( cend == "ad" && cend_old == "ad" ) cend = "as";
		var prods = prod.split("_");
		var prod_id = prods[1];
		var prod_src = prods[2];
		img = imgDir+ymd+HEADERS[prod_src]+cond+ecmwf_glb+yr+mo+dy+dash+prod_id+dash+layer+dash+sfc+dash+cend+'.png';
	}
	else {
		sfc = "all";
		if( cend == "ad" && cend_old != "ad" ) cend = cend_old;
		if( cend == "ad" && cend_old == "ad" ) cend = "as";
		var prods = prod.split("_");
		var prod_id = prods[1];
		var prod_src = prods[2];
		img = imgDir+ymd+HEADERS[prod_src]+cond+ecmwf_glb+yr+mo+dy+dash+prod_id+dash+sfc+dash+cend+'.png';
	}

        img_obj.src = img;
	var index = img.lastIndexOf("/");
	var alt   = img.substring(index+1,img.length);
	img_obj.title = img;
	img_obj.alt   = alt;
	document.getElementById(href_id).href = img;
	
}


function loadInitialImages(cend1,sfc1,prod1,layer1,cond1,yr1,mo1,dy1,
			   cend2,sfc2,prod2,layer2,cond2,yr2,mo2,dy2,
			   cend3,sfc3,prod3,layer3,cond3,yr3,mo3,dy3,
			   cend4,sfc4,prod4,layer4,cond4,yr4,mo4,dy4,
			   cend5,sfc5,prod5,layer5,cond5,yr5,mo5,dy5,
			   cend6,sfc6,prod6,layer6,cond6,yr6,mo6,dy6
			  ) {
	
	if ( prod1 == "" ) {
		var now = new Date();
		now.setDate(now.getDate()-ndayback) ; // n days ago

		var year  = now.getFullYear();
		var month = now.getMonth();   // month starting index is 0
		var day   = now.getDate();
		
		document.form.cend1.value="ad";
		document.form.sfc1.value="sea";
		document.form.prod1.value="temp_mean_vert";
		document.form.layer1.value="";
		document.form.cond1.selectedIndex=0;
		document.form.yr1.value = year ;
		document.form.mo1.selectedIndex = month ;
		document.form.dy1.selectedIndex = day - 1;
		loadImage1();
		
		document.form.cend2.value="ad";
		document.form.sfc2.value="sea";
		document.form.prod2.value="temp_stdv_vert";
		document.form.layer2.value="";
		document.form.cond2.selectedIndex=0;
		document.form.yr2.value = year ;
		document.form.mo2.selectedIndex = month ;
		document.form.dy2.selectedIndex = day - 1;
		loadImage2();
		
		document.form.cend3.value="ad";
		document.form.sfc3.value="sea";
		document.form.prod3.value="wv_mean_vert";
		document.form.layer3.value="";
		document.form.cond3.selectedIndex=0;
		document.form.yr3.value = year ;
		document.form.mo3.selectedIndex = month ;
		document.form.dy3.selectedIndex = day - 1;
		loadImage3();
		
		document.form.cend4.value="ad";
		document.form.sfc4.value="sea";
		document.form.prod4.value="wv_stdv_vert";
		document.form.layer4.value="";
		document.form.cond4.selectedIndex=0;
		document.form.yr4.value = year ;
		document.form.mo4.selectedIndex = month ;
		document.form.dy4.selectedIndex = day - 1;
		loadImage4();
		
		document.form.cend5.value="ad";
		document.form.sfc5.value="sea";
		document.form.prod5.value="tpwPerf";
		document.form.layer5.value="";
		document.form.cond5.selectedIndex=0;
		document.form.yr5.value = year ;
		document.form.mo5.selectedIndex = month ;
		document.form.dy5.selectedIndex = day - 1;
		loadImage5();
		
		document.form.cend6.value="ad";
		document.form.sfc6.value="sea";
		document.form.prod6.value="tskinPerf";
		document.form.layer6.value="";
		document.form.cond6.selectedIndex=0;
		document.form.yr6.value = year ;
		document.form.mo6.selectedIndex = month ;
		document.form.dy6.selectedIndex = day - 1;
		loadImage6();
		
	}
	else {
		
		document.form.yr1.value = yr1;
		document.form.mo1.value = mo1 ;
		document.form.dy1.value = dy1;
		document.form.sfc1.value = sfc1;
		document.form.prod1.value = prod1;
		document.form.cond1.value = cond1;
		changeProduct( document.form.prod1, document.form.layer1, "layer1", document.form.cend1 ); 
		document.form.cend1.value = cend1;
		document.form.layer1.value = layer1;
		loadImage1();

		document.form.yr2.value = yr2;
		document.form.mo2.value = mo2 ;
		document.form.dy2.value = dy2;
		document.form.sfc2.value = sfc2;
		document.form.prod2.value = prod2;
		document.form.cond2.value = cond2;
		changeProduct( document.form.prod2, document.form.layer2, "layer2", document.form.cend2 ); 
		document.form.cend2.value = cend2;
		document.form.layer2.value = layer2;
		loadImage2();

		document.form.yr3.value = yr3;
		document.form.mo3.value = mo3 ;
		document.form.dy3.value = dy3;
		document.form.sfc3.value = sfc3;
		document.form.prod3.value = prod3;
		document.form.cond3.value = cond3;
		changeProduct( document.form.prod3, document.form.layer3, "layer3", document.form.cend3 ); 
		document.form.cend3.value = cend3;
		document.form.layer3.value = layer3;
		loadImage3();

		document.form.yr4.value = yr4;
		document.form.mo4.value = mo4 ;
		document.form.dy4.value = dy4;
		document.form.sfc4.value = sfc4;
		document.form.prod4.value = prod4;
		document.form.cond4.value = cond4;
		changeProduct( document.form.prod4, document.form.layer4, "layer4", document.form.cend4 ); 
		document.form.cend4.value = cend4;
		document.form.layer4.value = layer4;
		loadImage4();

		document.form.yr5.value = yr5;
		document.form.mo5.value = mo5 ;
		document.form.dy5.value = dy5;
		document.form.sfc5.value = sfc5;
		document.form.prod5.value = prod5;
		document.form.cond5.value = cond5;
		changeProduct( document.form.prod5, document.form.layer5, "layer5", document.form.cend5 ); 
		document.form.cend5.value = cend5;
		document.form.layer5.value = layer5;
		loadImage5();

		document.form.yr6.value = yr6;
		document.form.mo6.value = mo6 ;
		document.form.dy6.value = dy6;
		document.form.sfc6.value = sfc6;
		document.form.prod6.value = prod6;
		document.form.cond6.value = cond6;
		changeProduct( document.form.prod6, document.form.layer6, "layer6", document.form.cend6 ); 
		document.form.cend6.value = cend6;
		document.form.layer6.value = layer6;
		loadImage6();

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
    else if ( i == 5 ) loadImage5();  
    else if ( i == 6 ) loadImage6();  
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
    else if ( i == 5 ) loadImage5();  
    else if ( i == 6 ) loadImage6();  
}
