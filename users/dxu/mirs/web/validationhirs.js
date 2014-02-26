var IMGDIR="images/";

var header="mirs_adv_poes_";
var sensor="_atovs";
var region="_glb_";

//function loadImageHelper(sat,prod,layer,sfc,yr,mo,dy) {
function loadImageHelper(sat,prod,sfc,yr,mo,dy) {
    
	var ymd =  "/" + yr + "-" + mo + "-" + dy + "/";
	
	if ( sat == 'f16' || sat == 'f18' ) {
		header="mirs_adv_dmsp_";
	}
	else if( sat == 'npp' ) {
		header="mirs_adv_npoess_";
	}
	else if( sat == 'gcomw1' ) {
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
	var img3="";	
	var img4="";	
		
	var sfc1="all";
	var sfc2="sea";
	var sfc3="lnd";
	var sfc4="ice";
	var sfc5="snw";
	
	img1 = IMGDIR+sat+ymd+header+sat+part+yr+mo+dy+dash+prod+dash+sfc1+dash+cend+".png";
	img2 = IMGDIR+sat+ymd+header+sat+part+yr+mo+dy+dash+prod+dash+sfc+dash+cend+".png"; 
	
	if ( sfc == "sea" ) {
	    img3 = IMGDIR+sat+ymd+header+sat+part+yr+mo+dy+dash+prod+dash+sfc4+dash+cend+".png"; 
	}
	else if ( sfc == "lnd" ) {
	    img3 = IMGDIR+sat+ymd+header+sat+part+yr+mo+dy+dash+prod+dash+sfc5+dash+cend+".png"; 
	}

        document.form.img1.src = img1;
        document.form.img2.src = img2;
        document.form.img3.src = img3;
	
	var index1 = img1.lastIndexOf("/");
	var alt1   = img1.substring(index1+1,img1.length);
	
	var index2 = img2.lastIndexOf("/");
	var alt2   = img2.substring(index2+1,img2.length);
	
	var index3 = img3.lastIndexOf("/");
	var alt3   = img3.substring(index3+1,img3.length);
	
	document.form.img1.title=img1;
	document.form.img2.title=img2;
	document.form.img3.title=img3;
	
	document.form.img1.alt = alt1;
	document.form.img2.alt = alt2;
	document.form.img3.alt = alt3;
	
	document.getElementById("href1").href=img1;
	document.getElementById("href2").href=img2;
	document.getElementById("href3").href=img3;
}


function loadImage_top() {
    
	var sat   = document.form.sat.value;
 	var prod  = document.form.prod.value;
	//var layer = document.form.layer.value;
	var sfc   = document.form.sfc.value;
	var yr    = document.form.yr.value;
	var mo    = document.form.mo.value;
	var dy    = document.form.dy.value;
	
	// update bottom panel
	document.form.sat2.value   = sat;
	document.form.prod2.value  = prod; 
	//document.form.layer2.value = layer;
	document.form.sfc2.value   = sfc;
	document.form.yr2.value    = yr;
	document.form.mo2.value    = mo;
	document.form.dy2.value    = dy;
	
	//loadImageHelper(sat,prod,layer,sfc,yr,mo,dy);
	loadImageHelper(sat,prod,sfc,yr,mo,dy);
}

function loadImage_bottom() {
    
	var sat   = document.form.sat2.value;
 	var prod  = document.form.prod2.value;
	//var layer = document.form.layer2.value;
	var sfc   = document.form.sfc2.value;
	var yr    = document.form.yr2.value;
	var mo    = document.form.mo2.value;
	var dy    = document.form.dy2.value;
	
	// update upper panel
	document.form.sat.value   = sat;
	document.form.prod.value  = prod; 
	//document.form.layer.value = layer;
	document.form.sfc.value   = sfc;
	document.form.yr.value    = yr;
	document.form.mo.value    = mo;
	document.form.dy.value    = dy;

	//loadImageHelper(sat,prod,layer,sfc,yr,mo,dy);
	loadImageHelper(sat,prod,sfc,yr,mo,dy);
}

function changeBoxSize() {
	
	document.getElementById("box4").width  = 325; 
	document.getElementById("box4").height = 275; 
		
	document.form.img4.width  = 325;
	document.form.img4.height = 250;
}

function changeBoxSizev() {
	
	document.getElementById("box4").width  = 0; 
	document.getElementById("box4").height = 0; 
		
	document.form.img4.width  = 0;
	document.form.img4.height = 0;
}

//function loadInitialImages(sat,prod,layer,sfc,yr,mo,dy) {
function loadInitialImages(sat,prod,sfc,yr,mo,dy) {
	
	if ( sat == "" ) {
		var now = new Date();
		now.setDate(now.getDate()-4) ; // 4 days ago

		var year  = now.getFullYear();
		var month = now.getMonth();  // month starting index is 0
		var day   = now.getDate();

		document.form.sat.selectedIndex=0;
		document.form.prod.selectedIndex=0;
		//document.form.layer.selectedIndex=0;
		document.form.sfc.selectedIndex=0;
		document.form.yr.value = year;
		document.form.mo.selectedIndex = month ;
		document.form.dy.selectedIndex = day - 1;

		document.form.sat2.selectedIndex=0;
		document.form.prod2.selectedIndex=0;
		//document.form.layer2.selectedIndex=0;
		document.form.sfc2.selectedIndex=0;
		document.form.yr2.value = year;
		document.form.mo2.selectedIndex = month ;
		document.form.dy2.selectedIndex = day - 1;
		
		satVal   = document.form.sat.value;
		prodVal  = document.form.prod.value;
		//layerVal = document.form.layer.value;
		sfcVal   = document.form.sfc.value;
		yrVal    = document.form.yr.value;
		moVal    = document.form.mo.value;
		dyVal    = document.form.dy.value;
		
		//loadImageHelper(satVal,prodVal,layerVal,sfcVal,yrVal,moVal,dyVal);
		loadImageHelper(satVal,prodVal,sfcVal,yrVal,moVal,dyVal);
	}
	else {
		document.form.yr.value = yr;
		document.form.mo.value = mo ;
		document.form.dy.value = dy;
		document.form.sat.value=sat;
		document.form.prod.value=prod;
		//document.form.layer.value=layer;
		document.form.sfc.value=sfc;
		
		document.form.yr2.value = yr;
		document.form.mo2.value = mo ;
		document.form.dy2.value = dy;
		document.form.sat2.value=sat;
		document.form.prod2.value=prod;
		//document.form.layer2.value=layer;
		document.form.sfc2.value=sfc;
		
		//changeBoxSize();
		//loadImageHelper(sat,prod,layer,sfc,yr,mo,dy);
		loadImageHelper(sat,prod,sfc,yr,mo,dy);
	}
	
}

//function loadInitialImagesv(sat,prod,layer,sfc,yr,mo,dy) {
function loadInitialImagesv(sat,prod,sfc,yr,mo,dy) {
	
	if ( sat == "" ) {
		var now = new Date();
		now.setDate(now.getDate()-4) ; // 4 days ago

		var year  = now.getFullYear();
		var month = now.getMonth();  // month starting index is 0
		var day   = now.getDate();

		document.form.sat.selectedIndex=0;
		document.form.prod.selectedIndex=0;
		//document.form.layer.selectedIndex=0;
		document.form.sfc.selectedIndex=0;
		document.form.yr.value = year;
		document.form.mo.selectedIndex = month ;
		document.form.dy.selectedIndex = day - 1;

		document.form.sat2.selectedIndex=0;
		document.form.prod2.selectedIndex=0;
		//document.form.layer2.selectedIndex=0;
		document.form.sfc2.selectedIndex=0;
		document.form.yr2.value = year;
		document.form.mo2.selectedIndex = month ;
		document.form.dy2.selectedIndex = day - 1;
		
		satVal   = document.form.sat.value;
		prodVal  = document.form.prod.value;
		//layerVal = document.form.layer.value;
		sfcVal   = document.form.sfc.value;
		yrVal    = document.form.yr.value;
		moVal    = document.form.mo.value;
		dyVal    = document.form.dy.value;
		
		//loadImageHelper(satVal,prodVal,layerVal,sfcVal,yrVal,moVal,dyVal);
		loadImageHelper(satVal,prodVal,sfcVal,yrVal,moVal,dyVal);
	}
	else {
		document.form.yr.value = yr;
		document.form.mo.value = mo ;
		document.form.dy.value = dy;
		document.form.sat.value=sat;
		document.form.prod.value=prod;
		//document.form.layer.value=layer;
		document.form.sfc.value=sfc;
		
		document.form.yr2.value = yr;
		document.form.mo2.value = mo ;
		document.form.dy2.value = dy;
		document.form.sat2.value=sat;
		document.form.prod2.value=prod;
		//document.form.layer2.value=layer;
		document.form.sfc2.value=sfc;
		
		//changeBoxSizev();
		//loadImageHelper(sat,prod,layer,sfc,yr,mo,dy);
		loadImageHelper(sat,prod,sfc,yr,mo,dy);
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
	//layerVal = document.form.layer.value;
	sfcVal   = document.form.sfc.value;
	yrVal    = document.form.yr.value;
	moVal    = document.form.mo.value;
	dyVal    = document.form.dy.value;
	
	//loadImageHelper(satVal,prodVal,layerVal,sfcVal,yrVal,moVal,dyVal);
	loadImageHelper(satVal,prodVal,sfcVal,yrVal,moVal,dyVal);
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
	//layerVal = document.form.layer.value;
	sfcVal   = document.form.sfc.value;
	yrVal    = document.form.yr.value;
	moVal    = document.form.mo.value;
	dyVal    = document.form.dy.value;
	
	//loadImageHelper(satVal,prodVal,layerVal,sfcVal,yrVal,moVal,dyVal);
	loadImageHelper(satVal,prodVal,sfcVal,yrVal,moVal,dyVal);
}
