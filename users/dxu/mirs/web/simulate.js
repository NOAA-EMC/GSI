var imgDir="images/n18/";
var header="mirs_adv_poes_amsuamhs_sim_";
var ecmwf="_ecmwf60_glb_p2p_";


var products = new Array(
		'ave_temp_mean_vert', 'ave_temp_stdv_vert', 'ave_wv_mean_vert', 'ave_wv_stdv_vert', 'em_allch_biasstd', 
		'tb(ch4:ch5:ch6:ch7:ch8:ch9:ch10:ch18)', 'temp(10mb:100mb:300mb:500mb:1000mb)', 
		'inttpw', 'tskin', 'wv(10mb:100mb:300mb:500mb:1000mb)' );


function getText( val ) {

	var text="";

	if            ( val == "tpw"   			) text = "TPW(MIRS vs model)"				;
	else if       ( val == "em"    			) text = "Emissivity(MIRS vs model)"			;
	else if       ( val == "clw"    		) text = "CLW(MIRS vs model)"				;
	else if       ( val == "tskin" 			) text = "Skin Temperature"				;
	else if       ( val == "tb"    			) text = "TB(current vs previous)"			;
	else if       ( val == "temp"  			) text = "Temperature Profile(current vs previous)"	;
	else if       ( val == "wv"    			) text = "Water Vapor Profile(current vs previous)"	;
	else if       ( val == "ave_temp_mean_vert"   	) text = "Temperature Bias(MIRS vs model)"    		;
	else if       ( val == "ave_temp_stdv_vert"   	) text = "Temperature Std Dev(MIRS vs model)" 		;
	else if       ( val == "ave_wv_mean_vert"  	) text = "Water Vapor Bias(MIRS vs model)"      	;
	else if       ( val == "ave_wv_stdv_vert"   	) text = "Water Vapor Std Dev(MIRS vs model)"   	;

	return text;

}


function changeProduct( prod_obj, layer_obj, layer_id ) {

	var prod_val  = prod_obj.value;
	var layer_val = layer_obj.value;
	
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
		    if( layers[j] == layer_val ) { layer_old_exist = 1; }
	    	}
	    } 
	}

	
	// if layer list has old layer, we use old one; otherwise, use the 1st one in the layer list
	if ( layer_old_exist == 1 ) { 
		layer_obj.value  = layer_val; 
	}
	
		
} 


function loadImage1() {
 	var prod  = document.form.prod1.value;
	var layer = document.form.layer1.value;
	var cond = document.form.cond1.value;
	var yr = document.form.yr1.value;
	var mo = document.form.mo1.value;
	var dy = document.form.dy1.value;
	loadImageHelp(prod,layer,cond,yr,mo,dy,document.form.img1,'href1');
}

function loadImage2() {
 	var prod  = document.form.prod2.value;
	var layer = document.form.layer2.value;
	var cond = document.form.cond2.value;
	var yr = document.form.yr2.value;
	var mo = document.form.mo2.value;
	var dy = document.form.dy2.value;
	loadImageHelp(prod,layer,cond,yr,mo,dy,document.form.img2,'href2');
}

function loadImage3() {
 	var prod  = document.form.prod3.value;
	var layer = document.form.layer3.value;
	var cond = document.form.cond3.value;
	var yr = document.form.yr3.value;
	var mo = document.form.mo3.value;
	var dy = document.form.dy3.value;
	loadImageHelp(prod,layer,cond,yr,mo,dy,document.form.img3,'href3');
}

function loadImage4() {
 	var prod  = document.form.prod4.value;
	var layer = document.form.layer4.value;
	var cond = document.form.cond4.value;
	var yr = document.form.yr4.value;
	var mo = document.form.mo4.value;
	var dy = document.form.dy4.value;
	loadImageHelp(prod,layer,cond,yr,mo,dy,document.form.img4,'href4');
}

function loadImage5() {
 	var prod  = document.form.prod5.value;
	var layer = document.form.layer5.value;
	var cond = document.form.cond5.value;
	var yr = document.form.yr5.value;
	var mo = document.form.mo5.value;
	var dy = document.form.dy5.value;
	loadImageHelp(prod,layer,cond,yr,mo,dy,document.form.img5,'href5');
}

function loadImage6() {
 	var prod  = document.form.prod6.value;
	var layer = document.form.layer6.value;
	var cond = document.form.cond6.value;
	var yr = document.form.yr6.value;
	var mo = document.form.mo6.value;
	var dy = document.form.dy6.value;
	loadImageHelp(prod,layer,cond,yr,mo,dy,document.form.img6,'href6');
}




function loadImageHelp(prod,layer,cond,yr,mo,dy,img_obj,href_id) {
    
	var ymd =  "/" + yr + "-" + mo + "-" + dy + "/";
	
	var dash  = "_";
	
	var img="";
	if ( prod == "temp" || prod == "wv" || prod == "tb") {
		img = imgDir+ymd+header+cond+ecmwf+yr+mo+dy+dash+prod+dash+layer+'_sea_ad.png';
	}
	else {
		img = imgDir+ymd+header+cond+ecmwf+yr+mo+dy+dash+prod+'_sea_ad.png';
	}

        img_obj.src = img;
	
	var index = img.lastIndexOf("/");
	var alt   = img.substring(index+1,img.length);
	
	img_obj.title = img;
	img_obj.alt   = alt;

	document.getElementById(href_id).href = img;
	
}


function loadInitialImages(prod1,layer1,cond1,yr1,mo1,dy1,
			   prod2,layer2,cond2,yr2,mo2,dy2,
			   prod3,layer3,cond3,yr3,mo3,dy3,
			   prod4,layer4,cond4,yr4,mo4,dy4,
			   prod5,layer5,cond5,yr5,mo5,dy5,
			   prod6,layer6,cond6,yr6,mo6,dy6
			  ) {
	
	if ( prod1 == "" ) {
		var now = new Date();
		now.setDate(now.getDate()-1) ; // 1 days ago

		var year  = now.getFullYear();
		var month = now.getMonth();   // month starting index is 0
		var day   = now.getDate();
		
		document.form.prod1.value="ave_temp_mean_vert";
		document.form.layer1.selectedIndex=0;
		document.form.cond1.selectedIndex=0;
		document.form.yr1.value = year;
		document.form.mo1.selectedIndex = month ;
		document.form.dy1.selectedIndex = day - 1;
		loadImage1();
		
		document.form.prod2.value="ave_temp_stdv_vert";
		document.form.layer2.selectedIndex=0;
		document.form.cond2.selectedIndex=0;
		document.form.yr2.value = year;
		document.form.mo2.selectedIndex = month ;
		document.form.dy2.selectedIndex = day - 1;
		loadImage2();
		
		document.form.prod3.value="ave_wv_mean_vert";
		document.form.layer3.selectedIndex=0;
		document.form.cond3.selectedIndex=0;
		document.form.yr3.value = year;
		document.form.mo3.selectedIndex = month ;
		document.form.dy3.selectedIndex = day - 1;
		loadImage3();
		
		document.form.prod4.value="ave_wv_stdv_vert";
		document.form.layer4.selectedIndex=0;
		document.form.cond4.selectedIndex=0;
		document.form.yr4.value = year;
		document.form.mo4.selectedIndex = month ;
		document.form.dy4.selectedIndex = day - 1;
		loadImage4();
		
		document.form.prod5.value="inttpw";
		document.form.layer5.selectedIndex=0;
		document.form.cond5.selectedIndex=0;
		document.form.yr5.value = year;
		document.form.mo5.selectedIndex = month ;
		document.form.dy5.selectedIndex = day - 1;
		loadImage5();
		
		document.form.prod6.value="tskin";
		document.form.layer6.selectedIndex=0;
		document.form.cond6.selectedIndex=0;
		document.form.yr6.value = year;
		document.form.mo6.selectedIndex = month ;
		document.form.dy6.selectedIndex = day - 1;
		loadImage6();
		
	}
	else {
		
		document.form.yr1.value = yr1;
		document.form.mo1.value = mo1 ;
		document.form.dy1.value = dy1;
		document.form.prod1.value = prod1;
		document.form.cond1.value = cond1;
		changeProduct( document.form.prod1, document.form.layer1, "layer1" ); 
		document.form.layer1.value = layer1;
		loadImage1();

		document.form.yr2.value = yr2;
		document.form.mo2.value = mo2 ;
		document.form.dy2.value = dy2;
		document.form.prod2.value = prod2;
		document.form.cond2.value = cond2;
		changeProduct( document.form.prod2, document.form.layer2, "layer2" ); 
		document.form.layer2.value = layer2;
		loadImage2();

		document.form.yr3.value = yr3;
		document.form.mo3.value = mo3 ;
		document.form.dy3.value = dy3;
		document.form.prod3.value = prod3;
		document.form.cond3.value = cond3;
		changeProduct( document.form.prod3, document.form.layer3, "layer3" ); 
		document.form.layer3.value = layer3;
		loadImage3();

		document.form.yr4.value = yr4;
		document.form.mo4.value = mo4 ;
		document.form.dy4.value = dy4;
		document.form.prod4.value = prod4;
		document.form.cond4.value = cond4;
		changeProduct( document.form.prod4, document.form.layer4, "layer4" ); 
		document.form.layer4.value = layer4;
		loadImage4();

		document.form.yr5.value = yr5;
		document.form.mo5.value = mo5 ;
		document.form.dy5.value = dy5;
		document.form.prod5.value = prod5;
		document.form.cond5.value = cond5;
		changeProduct( document.form.prod5, document.form.layer5, "layer5" ); 
		document.form.layer5.value = layer5;
		loadImage5();

		document.form.yr6.value = yr6;
		document.form.mo6.value = mo6 ;
		document.form.dy6.value = dy6;
		document.form.prod6.value = prod6;
		document.form.cond6.value = cond6;
		changeProduct( document.form.prod6, document.form.layer6, "layer6" ); 
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


