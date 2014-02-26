var IMGDIR = "images/";
var IMGDIR_TRMM = "img_hr/";

var header = "mirs_adv_poes_";
var sensor = "_trmm_2A12";
var region = "_glb_";
var ndayback = 7;

var prefix1 = "mirs_adv_eos_trmm_tmi_trop_";
var prefix2 = "mirs_adv_eos_trmm_2A12_trop_";
var prefix3 = "mirs_adv_eos_trmm_tmi_trmm_2A12_glb_p2p_";
var prefix4 = "mirs_adv_eos_trmm_tmi_trmm_2A12_hist_p2p_";



function changeSat( satval ) {
    
    if( satval == 'trmm' ) {
    	document.form.prod.options.length = 5;
    }
    else {
    	document.form.prod.options[5] = new Option();
        document.form.prod.options[5].value = 'pixel';
    	document.form.prod.options[5].text  = 'Collocated Pixels';
    }
}



function loadImageHelper(sat,prod,sfc,yr,mo,dy,cend) {
    
	var ymd =  "/" + yr + "-" + mo + "-" + dy + "/";
	
	var yyyymmdd = yr+mo+dy;
	
	if ( sat == 'f16' || sat == 'f18' ) {
		header="mirs_adv_dmsp_";
	}
	else if ( sat == 'npp' ) {
		header="mirs_adv_npoess_";
	}
	else if ( sat == 'gcomw1' ) {
		header="mirs_adv_eos_";
	}
	else if ( sat == 'trmm' ) {
		header="mirs_adv_eos_";
	}
	else {
		header="mirs_adv_poes_";
	}
	
	var part  = sensor + region + "p2p_";
	var part2 = sensor + "_hist_" + "p2p_";
	var dash  = "_";
	//var cend  = "ad";
	
	var img1 = "";
	var img2 = "";
	var img3 = "";	
	var img4 = "";	
		
	var sfc1 = "all";
	var sfc2 = "sea";
	
	if( sat != 'trmm' ) {
		
		cend = 'ad';
		
		if ( prod == "pixel" ) {
  		  img1 = IMGDIR+sat+ymd+header+sat+part2+yr+mo+dy+"_dist_diff_"+sfc1+dash+cend+".png";
  		  img2 = IMGDIR+sat+ymd+header+sat+part2+yr+mo+dy+"_time_diff_"+sfc1+dash+cend+".png"; 
		  img3 = "";
		  img4 = "";
		}
		else if ( prod == "clw" ){
		  img1 = IMGDIR+sat+ymd+header+sat+part +yr+mo+dy+dash+prod+dash+sfc2+dash+cend+".png";
		  img2 = IMGDIR+sat+ymd+header+sat+part2+yr+mo+dy+dash+prod+dash+sfc2+dash+cend+".png"; 
		  img3 = IMGDIR+sat+ymd+header+sat+part+yr+mo+dy+dash+prod+"_dist_diff_"+sfc2+dash+cend+".png";
		  img4 = IMGDIR+sat+ymd+header+sat+part+yr+mo+dy+dash+prod+"_time_diff_"+sfc2+dash+cend+".png"; 
		}
	
		else {
		  img1 = IMGDIR+sat+ymd+header+sat+part +yr+mo+dy+dash+prod+dash+sfc+dash+cend+".png";
		  img2 = IMGDIR+sat+ymd+header+sat+part2+yr+mo+dy+dash+prod+dash+sfc+dash+cend+".png"; 
		  img3 = IMGDIR+sat+ymd+header+sat+part+yr+mo+dy+dash+prod+"_dist_diff_"+sfc+dash+cend+".png";
		  img4 = IMGDIR+sat+ymd+header+sat+part+yr+mo+dy+dash+prod+"_time_diff_"+sfc+dash+cend+".png"; 
		}
	}
	else if( sat == 'trmm' ) {
		
		if ( prod == "clw" || prod == "lwp" ) { 
			img1 = IMGDIR_TRMM+sat+ymd+prefix1+yyyymmdd+'_'+prod+'_sea_'+cend+'.png';
			img2 = IMGDIR_TRMM+sat+ymd+prefix2+yyyymmdd+'_'+prod+'_sea_'+cend+'.png';
			img3 = IMGDIR_TRMM+sat+ymd+prefix3+yyyymmdd+'_'+prod+'_sea_'+cend+'.png';
			img4 = IMGDIR_TRMM+sat+ymd+prefix4+yyyymmdd+'_'+prod+'_sea_'+cend+'.png';
		}
		else {
			img1 = IMGDIR_TRMM+sat+ymd+prefix1+yyyymmdd+'_'+prod+'_'+sfc+'_'+cend+'.png';
			img2 = IMGDIR_TRMM+sat+ymd+prefix2+yyyymmdd+'_'+prod+'_'+sfc+'_'+cend+'.png';
			img3 = IMGDIR_TRMM+sat+ymd+prefix3+yyyymmdd+'_'+prod+'_'+sfc+'_'+cend+'.png';
			img4 = IMGDIR_TRMM+sat+ymd+prefix4+yyyymmdd+'_'+prod+'_'+sfc+'_'+cend+'.png';
		}
	
	}
	
	
        document.form.img1.src = img1;
        document.form.img2.src = img2;
        document.form.img3.src = img3;
        document.form.img4.src = img4;
	
	var index1 = img1.lastIndexOf("/");
	var alt1   = img1.substring(index1+1,img1.length);
	
	var index2 = img2.lastIndexOf("/");
	var alt2   = img2.substring(index2+1,img2.length);
	
	var index3 = img3.lastIndexOf("/");
	var alt3   = img3.substring(index3+1,img3.length);
	
	var index4 = img4.lastIndexOf("/");
	var alt4   = img4.substring(index4+1,img4.length);
	
	document.form.img1.title = img1;
	document.form.img2.title = img2;
	document.form.img3.title = img3;
	document.form.img4.title = img4;
	
	document.form.img1.alt = alt1;
	document.form.img2.alt = alt2;
	document.form.img3.alt = alt3;
	document.form.img4.alt = alt4;
	
	document.getElementById("href1").href = img1;
	document.getElementById("href2").href = img2;
	document.getElementById("href3").href = img3;
	document.getElementById("href4").href = img4;
}


function loadImage_top() {
    
	var sat   = document.form.sat.value;
	var prod  = document.form.prod.value;
	var sfc   = document.form.sfc.value;
	var yr    = document.form.yr.value;
	var mo    = document.form.mo.value;
	var dy    = document.form.dy.value;
	var cend  = document.form.cend.value;
	
	// update bottom panel
	document.form.sat2.value   = sat;
	document.form.prod2.value  = prod;
	document.form.sfc2.value   = sfc;
	document.form.yr2.value    = yr;
	document.form.mo2.value    = mo;
	document.form.dy2.value    = dy;
	document.form.cend2.value  = cend;
	
	loadImageHelper(sat,prod,sfc,yr,mo,dy,cend);
}


function loadImage_bottom() {
    
	var sat   = document.form.sat2.value;
	var prod  = document.form.prod2.value;
	var sfc   = document.form.sfc2.value;
	var yr    = document.form.yr2.value;
	var mo    = document.form.mo2.value;
	var dy    = document.form.dy2.value;
	var cend  = document.form.cend2.value;
	
	// update upper panel
	document.form.sat.value   = sat;
	document.form.prod.value  = prod;
	document.form.sfc.value   = sfc;
	document.form.yr.value    = yr;
	document.form.mo.value    = mo;
	document.form.dy.value    = dy;
	document.form.cend.value  = cend;

	loadImageHelper(sat,prod,sfc,yr,mo,dy,cend);
}

function loadInitialImages(sat,prod,sfc,yr,mo,dy,cend) {
	
	if ( sat == "" ) {
		var now = new Date();
		now.setDate(now.getDate()-ndayback) ; // n days ago

		var year  = now.getFullYear();
		var month = now.getMonth();  // month starting index is 0
		var day   = now.getDate();
		
		document.form.sat.selectedIndex = 0;
		document.form.prod.selectedIndex = 0;
		document.form.sfc.selectedIndex = 0;
		document.form.yr.value = year;
		document.form.mo.selectedIndex = month ;
		document.form.dy.selectedIndex = day - 1;
		document.form.cend.selectedIndex = 0;

		document.form.sat2.selectedIndex = 0;
		document.form.prod2.selectedIndex = 0;
		document.form.sfc2.selectedIndex = 0;
		document.form.yr2.value = year;
		document.form.mo2.selectedIndex = month ;
		document.form.dy2.selectedIndex = day - 1;
		document.form.cend2.selectedIndex = 0;
		
		satVal   = document.form.sat.value;
		prodVal  = document.form.prod.value;
		sfcVal   = document.form.sfc.value;
		yrVal    = document.form.yr.value;
		moVal    = document.form.mo.value;
		dyVal    = document.form.dy.value;
		cendVal  = document.form.cend.value;
		
		loadImageHelper(satVal,prodVal,sfcVal,yrVal,moVal,dyVal,cendVal);
	}
	else {
		document.form.yr.value = yr;
		document.form.mo.value = mo ;
		document.form.dy.value = dy;
		document.form.sat.value = sat;
		document.form.prod.value = prod;
		document.form.sfc.value = sfc;
		document.form.cend.value = cend;
		
		document.form.yr2.value = yr;
		document.form.mo2.value = mo ;
		document.form.dy2.value = dy;
		document.form.sat2.value = sat;
		document.form.prod2.value = prod;
		document.form.sfc2.value = sfc;
		document.form.cend2.value = cend;
		
		loadImageHelper(sat,prod,sfc,yr,mo,dy,cend);
	}
	
}


function loadInitialImagesv(sat,prod,sfc,yr,mo,dy,cend) {
	
	if ( sat == "" ) {
		var now = new Date();
		now.setDate(now.getDate()-ndayback) ; // n days ago

		var year  = now.getFullYear();
		var month = now.getMonth();  // month starting index is 0
		var day   = now.getDate();
		
		document.form.sat.selectedIndex = 0;
		document.form.prod.selectedIndex = 0;
		document.form.sfc.selectedIndex = 0;
		document.form.yr.value = year;
		document.form.mo.selectedIndex = month ;
		document.form.dy.selectedIndex = day - 1;
		document.form.cend.selectedIndex = 0;

		document.form.sat2.selectedIndex = 0;
		document.form.prod2.selectedIndex = 0;
		document.form.sfc2.selectedIndex = 0;
		document.form.yr2.value = year;
		document.form.mo2.selectedIndex = month ;
		document.form.dy2.selectedIndex = day - 1;
		document.form.cend2.selectedIndex = 0;
		
		satVal   = document.form.sat.value;
		prodVal  = document.form.prod.value;
		sfcVal   = document.form.sfc.value;
		yrVal    = document.form.yr.value;
		moVal    = document.form.mo.value;
		dyVal    = document.form.dy.value;
		cendVal  = document.form.cend.value;
		
		loadImageHelper(satVal,prodVal,sfcVal,yrVal,moVal,dyVal,cendVal);
	}
	else {
		document.form.yr.value = yr;
		document.form.mo.value = mo ;
		document.form.dy.value = dy;
		document.form.sat.value = sat;
		document.form.prod.value = prod;
		document.form.sfc.value = sfc;
		document.form.cend.value = cend;
		
		document.form.yr2.value = yr;
		document.form.mo2.value = mo ;
		document.form.dy2.value = dy;
		document.form.sat2.value = sat;
		document.form.prod2.value = prod;
		document.form.sfc2.value = sfc;
		document.form.cend2.value = cend;

		loadImageHelper(sat,prod,sfc,yr,mo,dy,cend);
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
	sfcVal   = document.form.sfc.value;
	yrVal    = document.form.yr.value;
	moVal    = document.form.mo.value;
	dyVal    = document.form.dy.value;
	cendVal  = document.form.cend.value;
	
	loadImageHelper(satVal,prodVal,sfcVal,yrVal,moVal,dyVal,cendVal);
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
	sfcVal   = document.form.sfc.value;
	yrVal    = document.form.yr.value;
	moVal    = document.form.mo.value;
	dyVal    = document.form.dy.value;
	cendVal  = document.form.cend.value;
	
	loadImageHelper(satVal,prodVal,sfcVal,yrVal,moVal,dyVal,cendVal);
}


// change panel formation
function changePanel( count ) {
	if     ( count == 1 ) document.form.action="validationtrmm2a12_profv.php";
	else if( count == 2 ) document.form.action="validationtrmm2a12.php"; 
	else if( count == 3 ) document.form.action="validationtrmm2a12v.php"; 
}
