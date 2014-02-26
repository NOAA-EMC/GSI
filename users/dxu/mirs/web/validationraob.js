var IMGDIR="images/";

var header="mirs_adv_poes_";
var sensor="_raob";
var region="_glb_";

var TEMP_WV_LAYERS = new Array("100mb", "200mb", "300mb", "400mb", "500mb", "600mb","700mb", "800mb", "850mb", "900mb", "950mb" );
//var TEMP_LAYERS = new Array("100mb", "200mb", "300mb", "400mb", "500mb", "600mb","700mb", "800mb", "850mb", "900mb", "950mb" );
//var WV_LAYERS = new Array("200mb", "300mb", "400mb", "500mb", "600mb","700mb", "800mb", "850mb", "900mb", "950mb" );

var TB_LAYERS = new Array("ch4", "ch5", "ch6", "ch7", "ch8", "ch9", "ch10", "ch18"  );


function loadLayers(prod) {
	
	// keep track of old layer selection, if any
	var layer_value = document.form.layer.value;
	
	document.form.layer.options.length = 0;
	document.form.layer2.options.length = 0;
	
  	if ( prod == "temp" ) {
		
	    for ( i=0; i<TEMP_WV_LAYERS.length; i++ )  {
		document.form.layer.options[i] 		= new Option();
		document.form.layer.options[i].value 	= TEMP_WV_LAYERS[i];
		document.form.layer.options[i].text 	= TEMP_WV_LAYERS[i];
	    }
	    for ( i=0; i<TEMP_WV_LAYERS.length; i++ )  {
		document.form.layer2.options[i] 	= new Option();
		document.form.layer2.options[i].value 	= TEMP_WV_LAYERS[i];
		document.form.layer2.options[i].text 	= TEMP_WV_LAYERS[i];
	    }
	}

  	else if ( prod == "wv" ) {
		
	    for ( i=0; i<TEMP_WV_LAYERS.length-1; i++ )  {
		document.form.layer.options[i] 		= new Option();
		document.form.layer.options[i].value 	= TEMP_WV_LAYERS[i+1];
		document.form.layer.options[i].text 	= TEMP_WV_LAYERS[i+1];
	    }
	    for ( i=0; i<TEMP_WV_LAYERS.length-1; i++ )  {
		document.form.layer2.options[i] 	= new Option();
		document.form.layer2.options[i].value 	= TEMP_WV_LAYERS[i+1];
		document.form.layer2.options[i].text 	= TEMP_WV_LAYERS[i+1];
	    }
	}

  	else if ( prod == "tb" ) {
		
	    for ( i=0; i<TB_LAYERS.length; i++ )  {
		document.form.layer.options[i] 		= new Option();
		document.form.layer.options[i].value 	= TB_LAYERS[i];
		document.form.layer.options[i].text 	= TB_LAYERS[i];
	    }
	    for ( i=0; i<TB_LAYERS.length; i++ )  {
		document.form.layer2.options[i] 	= new Option();
		document.form.layer2.options[i].value 	= TB_LAYERS[i];
		document.form.layer2.options[i].text 	= TB_LAYERS[i];
	    }
	}
	
	// still use the old layer selection if changed panel layout
	document.form.layer.value  = layer_value;
	document.form.layer2.value = layer_value;
	
}


function loadImageHelper(sat,prod,layer,sfc,yr,mo,dy) {
    
	var ymd =  "/" + yr + "-" + mo + "-" + dy + "/";
	
	if ( sat == 'f16' || sat == 'f18' ) {
		header="mirs_adv_dmsp_";
	}
	else if ( sat == 'npp' ) {
		//header="mirs_adv_jpss_";
		header="mirs_adv_npoess_";
	}
	else if ( sat == 'gcomw1' ) {
		//header="mirs_adv_jpss_";
		header="mirs_adv_eos_";
	}
	else {
		header="mirs_adv_poes_";
	}
	
	var part = sensor + region + "p2p_";
	var dash  = "_";
	var cend  = "ad";
	
	var img1="";
	var img2="";
	var img5="";	
	var img6="";	
	var img7="";	
	var img8="";	
		
	var sfc1="all";
	var sfc2="sea";
	var sfc3="lnd";
	var sfc4="ice";
	var sfc5="snw";
	
	if ( prod == "temp" || prod == "wv" ) {
  	  img1 = IMGDIR+sat+ymd+header+sat+part+yr+mo+dy+dash+prod+dash+layer+dash+sfc1+dash+cend+".png";
  	  img2 = IMGDIR+sat+ymd+header+sat+part+yr+mo+dy+dash+prod+dash+layer+dash+sfc+dash+cend+".png"; 
	
	  img5 = IMGDIR+sat+ymd+header+sat+part+yr+mo+dy+dash+prod+"_mean_vert_"+sfc+"_ad.png";
	  img6 = IMGDIR+sat+ymd+header+sat+part+yr+mo+dy+dash+prod+"_stdv_vert_"+sfc+"_ad.png";
	  img7 = IMGDIR+sat+ymd+header+sat+part+yr+mo+dy+"_ave_"+prod+"_mean_vert_"+sfc+"_ad.png";
	  img8 = IMGDIR+sat+ymd+header+sat+part+yr+mo+dy+"_ave_"+prod+"_stdv_vert_"+sfc+"_ad.png";
	}
	else if ( prod == "tpw" ) {
  	  img1 = IMGDIR+sat+ymd+header+sat+part+yr+mo+dy+dash+"int"+prod+dash+sfc1+dash+cend+".png";
  	  img2 = IMGDIR+sat+ymd+header+sat+part+yr+mo+dy+dash+prod+dash+sfc1+dash+cend+".png"; 
	
	  img5 = IMGDIR+sat+ymd+header+sat+part+yr+mo+dy+dash+"int"+prod+dash+sfc+dash+cend+".png";
	  img6 = IMGDIR+sat+ymd+header+sat+part+yr+mo+dy+dash+prod+dash+sfc+dash+cend+".png"; 
	  
	  if ( sfc == "sea" ) {
	    img7 = IMGDIR+sat+ymd+header+sat+part+yr+mo+dy+dash+"int"+prod+dash+sfc4+dash+cend+".png";
	    img8 = IMGDIR+sat+ymd+header+sat+part+yr+mo+dy+dash+prod+dash+sfc4+dash+cend+".png"; 
	  }
	  else if ( sfc == "lnd" ) {
	    img7 = IMGDIR+sat+ymd+header+sat+part+yr+mo+dy+dash+"int"+prod+dash+sfc5+dash+cend+".png";
	    img8 = IMGDIR+sat+ymd+header+sat+part+yr+mo+dy+dash+prod+dash+sfc5+dash+cend+".png"; 
	  }
	}
	else if ( prod == "em" ) {
  	  img1 = IMGDIR+sat+ymd+header+sat+part+yr+mo+dy+dash+prod+"_allch_biasstd_"+sfc2+dash+cend+".png";
  	  img2 = IMGDIR+sat+ymd+header+sat+part+yr+mo+dy+dash+prod+"_allch_biasstd_"+sfc4+dash+cend+".png"; 
	  img5 = IMGDIR+sat+ymd+header+sat+part+yr+mo+dy+dash+prod+"_allch_biasstd_"+sfc3+dash+cend+".png";
	  img6 = IMGDIR+sat+ymd+header+sat+part+yr+mo+dy+dash+prod+"_allch_biasstd_"+sfc5+dash+cend+".png"; 
	}
	else if ( prod == "tb" ) {
  	  img1 = IMGDIR+sat+ymd+header+sat+"_atovs_glb_p2p_"+yr+mo+dy+dash+prod+dash+layer+dash+sfc2+dash+cend+".png";
  	  img2 = IMGDIR+sat+ymd+header+sat+"_atovs_glb_p2p_"+yr+mo+dy+dash+prod+dash+layer+dash+sfc3+dash+cend+".png"; 
	}

        document.form.img1.src = img1;
        document.form.img2.src = img2;
        document.form.img5.src = img5;
        document.form.img6.src = img6;
        document.form.img7.src = img7;
        document.form.img8.src = img8;
	
	var index1 = img1.lastIndexOf("/");
	var alt1   = img1.substring(index1+1,img1.length);
	
	var index2 = img2.lastIndexOf("/");
	var alt2   = img2.substring(index2+1,img2.length);
	
	var index5 = img5.lastIndexOf("/");
	var alt5   = img5.substring(index5+1,img5.length);
	
	var index6 = img6.lastIndexOf("/");
	var alt6   = img6.substring(index6+1,img6.length);
	
	var index7 = img7.lastIndexOf("/");
	var alt7   = img7.substring(index7+1,img7.length);
	
	var index8 = img8.lastIndexOf("/");
	var alt8   = img8.substring(index8+1,img8.length);
	
	document.form.img1.title=img1;
	document.form.img2.title=img2;
	document.form.img5.title=img5;
	document.form.img6.title=img6;
	document.form.img7.title=img7;
	document.form.img8.title=img8;
	
	document.form.img1.alt = alt1;
	document.form.img2.alt = alt2;
	document.form.img5.alt = alt5;
	document.form.img6.alt = alt6;
	document.form.img7.alt = alt7;
	document.form.img8.alt = alt8;
	
	document.getElementById("href1").href=img1;
	document.getElementById("href2").href=img2;
	document.getElementById("href5").href=img5;
	document.getElementById("href6").href=img6;
	document.getElementById("href7").href=img7;
	document.getElementById("href8").href=img8;
	
}


function loadImage_top() {
    
	var sat   = document.form.sat.value;
 	var prod  = document.form.prod.value;
	var layer = document.form.layer.value;
	var sfc   = document.form.sfc.value;
	var yr    = document.form.yr.value;
	var mo    = document.form.mo.value;
	var dy    = document.form.dy.value;
	
	// update bottom panel
	document.form.sat2.value   = sat;
	document.form.prod2.value  = prod; 
	document.form.layer2.value = layer;
	document.form.sfc2.value   = sfc;
	document.form.yr2.value    = yr;
	document.form.mo2.value    = mo;
	document.form.dy2.value    = dy;
	
	loadImageHelper(sat,prod,layer,sfc,yr,mo,dy);
}

function loadImage_bottom() {
    
	var sat   = document.form.sat2.value;
 	var prod  = document.form.prod2.value;
	var layer = document.form.layer2.value;
	var sfc   = document.form.sfc2.value;
	var yr    = document.form.yr2.value;
	var mo    = document.form.mo2.value;
	var dy    = document.form.dy2.value;
	
	// update upper panel
	document.form.sat.value   = sat;
	document.form.prod.value  = prod; 
	document.form.layer.value = layer;
	document.form.sfc.value   = sfc;
	document.form.yr.value    = yr;
	document.form.mo.value    = mo;
	document.form.dy.value    = dy;

	loadImageHelper(sat,prod,layer,sfc,yr,mo,dy);
}

function loadInitialImages(sat,prod,layer,sfc,yr,mo,dy) {
	
	if ( sat == "" ) {
		var now = new Date();
		now.setDate(now.getDate()-4) ; // 2 days ago

		var year  = now.getFullYear();
		var month = now.getMonth();  // month starting index is 0
		var day   = now.getDate();

		document.form.sat.selectedIndex=0;
		document.form.prod.selectedIndex=0;
		document.form.layer.selectedIndex=0;
		document.form.sfc.selectedIndex=0;
		document.form.yr.value = year;
		document.form.mo.selectedIndex = month ;
		document.form.dy.selectedIndex = day - 1;

		document.form.sat2.selectedIndex=0;
		document.form.prod2.selectedIndex=0;
		document.form.layer2.selectedIndex=0;
		document.form.sfc2.selectedIndex=0;
		document.form.yr2.value = year;
		document.form.mo2.selectedIndex = month ;
		document.form.dy2.selectedIndex = day - 1;
		
		satVal   = document.form.sat.value;
		prodVal  = document.form.prod.value;
		layerVal = document.form.layer.value;
		sfcVal   = document.form.sfc.value;
		yrVal    = document.form.yr.value;
		moVal    = document.form.mo.value;
		dyVal    = document.form.dy.value;
		
		loadImageHelper(satVal,prodVal,layerVal,sfcVal,yrVal,moVal,dyVal);
	}
	else {
		document.form.yr.value = yr;
		document.form.mo.value = mo ;
		document.form.dy.value = dy;
		document.form.sat.value=sat;
		document.form.prod.value=prod;
		document.form.layer.value=layer;
		document.form.sfc.value=sfc;
		
		document.form.yr2.value = yr;
		document.form.mo2.value = mo ;
		document.form.dy2.value = dy;
		document.form.sat2.value=sat;
		document.form.prod2.value=prod;
		document.form.layer2.value=layer;
		document.form.sfc2.value=sfc;
		
		changeProduct(prod);
		
		loadImageHelper(sat,prod,layer,sfc,yr,mo,dy);
	}
	
}

function loadInitialImagesv(sat,prod,layer,sfc,yr,mo,dy) {
	
	if ( sat == "" ) {
		var now = new Date();
		now.setDate(now.getDate()-4) ; // 2 days ago

		var year  = now.getFullYear();
		var month = now.getMonth();  // month starting index is 0
		var day   = now.getDate();

		document.form.sat.selectedIndex=0;
		document.form.prod.selectedIndex=0;
		document.form.layer.selectedIndex=0;
		document.form.sfc.selectedIndex=0;
		document.form.yr.value = year;
		document.form.mo.selectedIndex = month ;
		document.form.dy.selectedIndex = day - 1;

		document.form.sat2.selectedIndex=0;
		document.form.prod2.selectedIndex=0;
		document.form.layer2.selectedIndex=0;
		document.form.sfc2.selectedIndex=0;
		document.form.yr2.value = year;
		document.form.mo2.selectedIndex = month ;
		document.form.dy2.selectedIndex = day - 1;
		
		satVal   = document.form.sat.value;
		prodVal  = document.form.prod.value;
		layerVal = document.form.layer.value;
		sfcVal   = document.form.sfc.value;
		yrVal    = document.form.yr.value;
		moVal    = document.form.mo.value;
		dyVal    = document.form.dy.value;
		
		loadImageHelper(satVal,prodVal,layerVal,sfcVal,yrVal,moVal,dyVal);
	}
	else {
		document.form.yr.value = yr;
		document.form.mo.value = mo ;
		document.form.dy.value = dy;
		document.form.sat.value=sat;
		document.form.prod.value=prod;
		document.form.layer.value=layer;
		document.form.sfc.value=sfc;
		
		document.form.yr2.value = yr;
		document.form.mo2.value = mo ;
		document.form.dy2.value = dy;
		document.form.sat2.value=sat;
		document.form.prod2.value=prod;
		document.form.layer2.value=layer;
		document.form.sfc2.value=sfc;
		
		changeProductv(prod);
		
		loadImageHelper(sat,prod,layer,sfc,yr,mo,dy);
	}
	
}


function changeProduct( prod ) {
	
	if ( prod == "tpw" ) {
	
	    document.getElementById("layer").className="optioninvisible"; 
	    document.getElementById("layer2").className="optioninvisible";
	    document.form.prod.value=prod; 
	    document.form.prod2.value=prod;
	    
	    document.getElementById("box5").style.display  = "table-cell";
	    document.getElementById("box6").style.display  = "table-cell";
	    document.getElementById("box7").style.display  = "table-cell";
	    document.getElementById("box8").style.display  = "table-cell";

	    document.getElementById("box5").width  = 325; 
	    document.getElementById("box5").height = 275; 
	    document.form.img5.width  = 325;
	    document.form.img5.height = 250;
	    
	    document.getElementById("box6").width  = 325; 
	    document.getElementById("box6").height = 275; 
	    document.form.img6.width  = 325;
	    document.form.img6.height = 250;

	    document.getElementById("box7").width  = 325; 
	    document.getElementById("box7").height = 275; 
	    document.form.img7.width  = 325;
	    document.form.img7.height = 250;
	    
	    document.getElementById("box8").width  = 325; 
	    document.getElementById("box8").height = 275; 
	    document.form.img8.width  = 325;
	    document.form.img8.height = 250;

	}
	else if ( prod == "em" ) {
	
	    document.getElementById("layer").className="optioninvisible"; 
	    document.getElementById("layer2").className="optioninvisible";
	    document.form.prod.value=prod; 
	    document.form.prod2.value=prod;
	    
	    document.getElementById("box5").style.display  = "table-cell";
	    document.getElementById("box6").style.display  = "table-cell";
	    document.getElementById("box7").style.display  = "none";
	    document.getElementById("box8").style.display  = "none";
	    
	    document.getElementById("box5").width  = 325; 
	    document.getElementById("box5").height = 275; 
	    document.form.img5.width  = 325;
	    document.form.img5.height = 250;
	    
	    document.getElementById("box6").width  = 325; 
	    document.getElementById("box6").height = 275; 
	    document.form.img6.width  = 325;
	    document.form.img6.height = 250;

	}
	else if ( prod == "temp" || prod == "wv" ) {
	
	    document.getElementById("layer").className="optionvisible"; 
	    document.getElementById("layer2").className="optionvisible"; 
	    document.form.prod.value=prod; 
	    document.form.prod2.value=prod; 
	    
	    loadLayers(prod);
	    
	    document.getElementById("box5").style.display  = "table-cell";
	    document.getElementById("box6").style.display  = "table-cell";
	    document.getElementById("box7").style.display  = "table-cell";
	    document.getElementById("box8").style.display  = "table-cell";
	    
	    document.getElementById("box5").width  = 325; 
	    document.getElementById("box5").height = 275; 
	    document.form.img5.width  = 325;
	    document.form.img5.height = 250;
	    
	    document.getElementById("box6").width  = 325; 
	    document.getElementById("box6").height = 275; 
	    document.form.img6.width  = 325;
	    document.form.img6.height = 250;

	    document.getElementById("box7").width  = 325; 
	    document.getElementById("box7").height = 275; 
	    document.form.img7.width  = 325;
	    document.form.img7.height = 250;
	    
	    document.getElementById("box8").width  = 325; 
	    document.getElementById("box8").height = 275; 
	    document.form.img8.width  = 325;
	    document.form.img8.height = 250;

	}	
	else if ( prod == "tb" ) {
	
	    document.getElementById("layer").className="optionvisible"; 
	    document.getElementById("layer2").className="optionvisible"; 
	    document.form.prod.value=prod; 
	    document.form.prod2.value=prod; 
	    
	    loadLayers(prod);
	    
	    document.getElementById("box5").style.display  = "none";
	    document.getElementById("box6").style.display  = "none";
	    document.getElementById("box7").style.display  = "none";
	    document.getElementById("box8").style.display  = "none";

	}	
}


function changeProductv( prod ) {
	
	if ( prod == "tpw" ) {
	
	    document.getElementById("layer").className="optioninvisible"; 
	    document.getElementById("layer2").className="optioninvisible";
	    document.form.prod.value=prod; 
	    document.form.prod2.value=prod;
	    
	    document.getElementById("box5").style.display  = "table-cell";
	    document.getElementById("box6").style.display  = "table-cell";
	    document.getElementById("box7").style.display  = "table-cell";
	    document.getElementById("box8").style.display  = "table-cell";
	    
	    document.getElementById("box5").width  = 650; 
	    document.getElementById("box5").height = 500; 
	    document.form.img5.width  = 650;
	    document.form.img5.height = 500;
	    
	    document.getElementById("box6").width  = 650; 
	    document.getElementById("box6").height = 500; 
	    document.form.img6.width  = 650;
	    document.form.img6.height = 500;
	    
	    document.getElementById("box7").width  = 650; 
	    document.getElementById("box7").height = 500; 
	    document.form.img7.width  = 650;
	    document.form.img7.height = 500;
	    
	    document.getElementById("box8").width  = 650; 
	    document.getElementById("box8").height = 500; 
	    document.form.img8.width  = 650;
	    document.form.img8.height = 500;
	    
	}
	else if ( prod == "em" ) {
	
	    document.getElementById("layer").className="optioninvisible"; 
	    document.getElementById("layer2").className="optioninvisible";
	    document.form.prod.value=prod; 
	    document.form.prod2.value=prod;
	    
	    document.getElementById("box5").style.display  = "table-cell";
	    document.getElementById("box6").style.display  = "table-cell";
	    document.getElementById("box7").style.display  = "none";
	    document.getElementById("box8").style.display  = "none";
	    
	    document.getElementById("box5").width  = 650; 
	    document.getElementById("box5").height = 500; 
	    document.form.img5.width  = 650;
	    document.form.img5.height = 500;
	    
	    document.getElementById("box6").width  = 650; 
	    document.getElementById("box6").height = 500; 
	    document.form.img6.width  = 650;
	    document.form.img6.height = 500;

	}
	else if ( prod == "temp" || prod == "wv" ) {
	
	    document.getElementById("layer").className="optionvisible"; 
	    document.getElementById("layer2").className="optionvisible"; 
	    document.form.prod.value=prod; 
	    document.form.prod2.value=prod; 

	    loadLayers(prod);

	    document.getElementById("box5").style.display  = "table-cell";
	    document.getElementById("box6").style.display  = "table-cell";
	    document.getElementById("box7").style.display  = "table-cell";
	    document.getElementById("box8").style.display  = "table-cell";
	    
	    document.getElementById("box5").width  = 650; 
	    document.getElementById("box5").height = 500; 
	    document.form.img5.width  = 650;
	    document.form.img5.height = 500;
	    
	    document.getElementById("box6").width  = 650; 
	    document.getElementById("box6").height = 500; 
	    document.form.img6.width  = 650;
	    document.form.img6.height = 500;

	    document.getElementById("box7").width  = 650; 
	    document.getElementById("box7").height = 500; 
	    document.form.img7.width  = 650;
	    document.form.img7.height = 500;
	    
	    document.getElementById("box8").width  = 650; 
	    document.getElementById("box8").height = 500; 
	    document.form.img8.width  = 650;
	    document.form.img8.height = 500;

	}	
	else if ( prod == "tb" ) {
	
	    document.getElementById("layer").className="optionvisible"; 
	    document.getElementById("layer2").className="optionvisible"; 
	    document.form.prod.value=prod; 
	    document.form.prod2.value=prod; 
	    
	    loadLayers(prod);
	    
	    document.getElementById("box5").style.display  = "none";
	    document.getElementById("box6").style.display  = "none";
	    document.getElementById("box7").style.display  = "none";
	    document.getElementById("box8").style.display  = "none";

	}	
}

function rev() {

        var year  = parseInt(document.form.yr.value,10);
        var month = parseInt(document.form.mo.value,10);
        var day   = parseInt(document.form.dy.value,10);

        var now = new Date(year,month-1,day);
        now.setDate(now.getDate()-1);

        year  = now.getFullYear();
        month = now.getMonth();
        day   = now.getDate();

	document.form.yr.value = year;
	document.form.mo.selectedIndex = month ;
	document.form.dy.selectedIndex = day - 1;
	
	document.form.yr2.value = year;
	document.form.mo2.selectedIndex = month ;
	document.form.dy2.selectedIndex = day - 1;
	
	satVal   = document.form.sat.value;
	prodVal  = document.form.prod.value;
	layerVal = document.form.layer.value;
	sfcVal   = document.form.sfc.value;
	yrVal    = document.form.yr.value;
	moVal    = document.form.mo.value;
	dyVal    = document.form.dy.value;
	
	loadImageHelper(satVal,prodVal,layerVal,sfcVal,yrVal,moVal,dyVal);
}

function fwd() {

        var year  = parseInt(document.form.yr.value,10);
        var month = parseInt(document.form.mo.value,10);
        var day   = parseInt(document.form.dy.value,10);

        var now = new Date(year,month-1,day);
        now.setDate(now.getDate()+1);

        year  = now.getFullYear();
        month = now.getMonth();
        day   = now.getDate();

        document.form.yr.value = year;
        document.form.mo.selectedIndex = month ;
        document.form.dy.selectedIndex = day - 1;

        document.form.yr2.value = year;
        document.form.mo2.selectedIndex = month ;
        document.form.dy2.selectedIndex = day - 1;

	satVal   = document.form.sat.value;
	prodVal  = document.form.prod.value;
	layerVal = document.form.layer.value;
	sfcVal   = document.form.sfc.value;
	yrVal    = document.form.yr.value;
	moVal    = document.form.mo.value;
	dyVal    = document.form.dy.value;
	
	loadImageHelper(satVal,prodVal,layerVal,sfcVal,yrVal,moVal,dyVal);
}
