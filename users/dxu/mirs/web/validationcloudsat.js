var IMGDIR="images/";

var header="mirs_adv_poes_";
var sensor="_cloudsat";
var region="_glb_";

function loadImageHelper(sat,prod,sfc,yr,mo,dy) {
    
	var ymd =  "/" + yr + "-" + mo + "-" + dy + "/";
	
	if ( sat == 'f16' || sat == 'f18') {
		header="mirs_adv_dmsp_";
	}
	else if ( sat == 'n18' || sat == 'n19' || sat == 'metopA' || sat == 'metopB' ) {
		header="mirs_adv_poes_";
	}
	else if ( sat == 'npp' ) {
		header="mirs_adv_npoess_";
	}
	
	var part  = sensor + region + "p2p_";
	var part2 = sensor + "_hist_" + "p2p_";
	var dash  = "_";
	var cend  = "ad";
	
	var img1="";
	var img2="";
	var img3="";	
	var img4="";	
	
	var img5="";	
	var img6="";	
		
	var sfc1="all";
	var sfc2="sea";
	var sfc3="lnd";
	var sfc4="ice";
	var sfc5="snw";
	
	if ( prod == "clw" || prod == "lwp" ) {
  	  img1 = IMGDIR+sat+ymd+header+sat+part+yr+mo+dy+dash+prod+dash+sfc2+dash+cend+".png";
  	  img2 = IMGDIR+sat+ymd+header+sat+part2+yr+mo+dy+dash+prod+dash+sfc2+dash+cend+".png"; 
	  img3 = IMGDIR+sat+ymd+header+sat+part+yr+mo+dy+dash+prod+"_dist_diff_"+sfc2+dash+cend+".png";
	  img4 = IMGDIR+sat+ymd+header+sat+part+yr+mo+dy+dash+prod+"_time_diff_"+sfc2+dash+cend+".png"; 
	}
	else if ( prod == "iwp" ) {
  	  img1 = IMGDIR+sat+ymd+header+sat+part +yr+mo+dy+dash+prod+dash+sfc+dash+cend+".png";
  	  img2 = IMGDIR+sat+ymd+header+sat+part2+yr+mo+dy+dash+prod+dash+sfc+dash+cend+".png"; 
	  img3 = IMGDIR+sat+ymd+header+sat+part+yr+mo+dy+dash+prod+"_dist_diff_"+sfc+dash+cend+".png";
	  img4 = IMGDIR+sat+ymd+header+sat+part+yr+mo+dy+dash+prod+"_time_diff_"+sfc+dash+cend+".png"; 
	}
	else if ( prod == "rr" ) {
	
	  var now_rr = new Date();
	  now_rr.setDate(now_rr.getDate()-414) ; // 414 days ago
	
	  var yr_rr = now_rr.getFullYear();
	  var mo_rr_tmp = now_rr.getMonth()+1;  // month starting index is 0
	  var dy_rr_tmp = now_rr.getDate();
	  
	  var mo_rr = String(mo_rr_tmp);
	  if( mo_rr_tmp < 10 ) mo_rr = '0' + mo_rr;
	  
	  var dy_rr = String(dy_rr_tmp);
	  if( dy_rr_tmp < 10 ) dy_rr = '0' + dy_rr;
	  
	  //var yr_rr=yr;
	  //var mo_rr=mo;
	  //var dy_rr=dy;
	  
	  var ymd_rr =  "/" + yr_rr + "-" + mo_rr + "-" + dy_rr + "/";
		
  	  img1 = IMGDIR+sat+ymd_rr+header+sat+'_cs_precip_glb_p2p_' +yr_rr+mo_rr+dy_rr+dash+prod+dash+sfc+dash+cend+".png";
  	  img2 = IMGDIR+sat+ymd_rr+header+sat+'_cs_precip_hist_p2p_'+yr_rr+mo_rr+dy_rr+dash+prod+dash+sfc+dash+cend+".png"; 
	  img3 = IMGDIR+sat+ymd_rr+header+sat+'_cs_precip_glb_p2p_' +yr_rr+mo_rr+dy_rr+dash+prod+"_dist_diff_"+sfc+dash+cend+".png";
	  img4 = IMGDIR+sat+ymd_rr+header+sat+'_cs_precip_glb_p2p_' +yr_rr+mo_rr+dy_rr+dash+prod+"_time_diff_"+sfc+dash+cend+".png"; 
	  img5 = IMGDIR+sat+ymd_rr+header+sat+'_cs_precip_hist_p2p_'+yr_rr+mo_rr+dy_rr+"_dist_diff_"+sfc1+dash+cend+".png";
	  img6 = IMGDIR+sat+ymd_rr+header+sat+'_cs_precip_hist_p2p_'+yr_rr+mo_rr+dy_rr+"_time_diff_"+sfc1+dash+cend+".png"; 
	}
	else if ( prod == "csrr" ) {
  	  
	  var now_rr = new Date();
	  now_rr.setDate(now_rr.getDate()-414) ; // 414 days ago
	
	  var yr_rr = now_rr.getFullYear();
	  var mo_rr_tmp = now_rr.getMonth()+1;  // month starting index is 0
	  var dy_rr_tmp = now_rr.getDate();
	  
	  var mo_rr = String(mo_rr_tmp);
	  if( mo_rr_tmp < 10 ) mo_rr = '0' + mo_rr;
	  
	  var dy_rr = String(dy_rr_tmp);
	  if( dy_rr_tmp < 10 ) dy_rr = '0' + dy_rr;
	  
	  var ymd_rr =  "/" + yr_rr + "-" + mo_rr + "-" + dy_rr + "/";

	  img1 = IMGDIR+sat+ymd_rr+header+sat+'_cs_precip_glb_p2p_' +yr_rr+mo_rr+dy_rr+dash+prod+'_clw_'+sfc+dash+cend+".png";
  	  img2 = IMGDIR+sat+ymd_rr+header+sat+'_cs_precip_glb_p2p_' +yr_rr+mo_rr+dy_rr+dash+prod+'_gwp_'+sfc+dash+cend+".png"; 
	  img3 = IMGDIR+sat+ymd_rr+header+sat+'_cs_precip_glb_p2p_' +yr_rr+mo_rr+dy_rr+dash+prod+"_rwp_"+sfc+dash+cend+".png";
	  img4 = IMGDIR+sat+ymd_rr+header+sat+'_cs_precip_glb_p2p_' +yr_rr+mo_rr+dy_rr+dash+prod+"_tpw_"+sfc+dash+cend+".png"; 
	}
	else if ( prod == "pixel" ) {
  	  img1 = IMGDIR+sat+ymd+header+sat+part2+yr+mo+dy+"_dist_diff_"+sfc1+dash+cend+".png";
  	  img2 = IMGDIR+sat+ymd+header+sat+part2+yr+mo+dy+"_time_diff_"+sfc1+dash+cend+".png"; 
	}

        document.form.img1.src = img1;
        document.form.img2.src = img2;
        document.form.img3.src = img3;
        document.form.img4.src = img4;
        
	document.form.img5.src = img5;
        document.form.img6.src = img6;
	
	var index1 = img1.lastIndexOf("/");
	var alt1   = img1.substring(index1+1,img1.length);
	
	var index2 = img2.lastIndexOf("/");
	var alt2   = img2.substring(index2+1,img2.length);
	
	var index3 = img3.lastIndexOf("/");
	var alt3   = img3.substring(index3+1,img3.length);
	
	var index4 = img4.lastIndexOf("/");
	var alt4   = img4.substring(index4+1,img4.length);
	
	var index5 = img5.lastIndexOf("/");
	var alt5   = img5.substring(index5+1,img5.length);
	
	var index6 = img6.lastIndexOf("/");
	var alt6   = img6.substring(index6+1,img6.length);
	
	document.form.img1.title=img1;
	document.form.img2.title=img2;
	document.form.img3.title=img3;
	document.form.img4.title=img4;
	document.form.img5.title=img5;
	document.form.img6.title=img6;
	
	
	document.form.img1.alt = alt1;
	document.form.img2.alt = alt2;
	document.form.img3.alt = alt3;
	document.form.img4.alt = alt4;
	document.form.img5.alt = alt5;
	document.form.img6.alt = alt6;
	
	document.getElementById("href1").href=img1;
	document.getElementById("href2").href=img2;
	document.getElementById("href3").href=img3;
	document.getElementById("href4").href=img4;
	document.getElementById("href5").href=img5;
	document.getElementById("href6").href=img6;
}


function loadImage_top() {
    
	var sat   = document.form.sat.value;
 	var prod  = document.form.prod.value;
	var sfc   = document.form.sfc.value;
	var yr    = document.form.yr.value;
	var mo    = document.form.mo.value;
	var dy    = document.form.dy.value;
	
	// update bottom panel
	document.form.sat2.value   = sat;
	document.form.prod2.value  = prod; 
	document.form.sfc2.value   = sfc;
	document.form.yr2.value    = yr;
	document.form.mo2.value    = mo;
	document.form.dy2.value    = dy;
	
	loadImageHelper(sat,prod,sfc,yr,mo,dy);
}


function loadImage_bottom() {
    
	var sat   = document.form.sat2.value;
 	var prod  = document.form.prod2.value;
	var sfc   = document.form.sfc2.value;
	var yr    = document.form.yr2.value;
	var mo    = document.form.mo2.value;
	var dy    = document.form.dy2.value;
	
	// update upper panel
	document.form.sat.value   = sat;
	document.form.prod.value  = prod; 
	document.form.sfc.value   = sfc;
	document.form.yr.value    = yr;
	document.form.mo.value    = mo;
	document.form.dy.value    = dy;

	loadImageHelper(sat,prod,sfc,yr,mo,dy);
}


function loadInitialImages(sat,prod,sfc,yr,mo,dy) {
	
	if ( sat == "" ) {
		var now = new Date();
		now.setDate(now.getDate()-16) ; // 16 days ago
		
		var year  = now.getFullYear();
		var month = now.getMonth();  // month starting index is 0
		var day   = now.getDate();
		
		var now_rr = new Date();
		now_rr.setDate(now_rr.getDate()-414) ; // 414 days ago
		
		var year_rr  = now_rr.getFullYear();
		var month_rr = now_rr.getMonth();  // month starting index is 0
		var day_rr   = now_rr.getDate();
		
		document.form.sat.selectedIndex=0;
		document.form.prod.value="rr";
		document.form.sfc.selectedIndex=0;
		document.form.yr.value = year;
		document.form.mo.selectedIndex = month ;
		document.form.dy.selectedIndex = day - 1;

		document.form.sat2.selectedIndex=0;
		document.form.prod2.value="rr";
		document.form.sfc2.selectedIndex=0;
		document.form.yr2.value = year;
		document.form.mo2.selectedIndex = month ;
		document.form.dy2.selectedIndex = day - 1;
		
		satVal   = document.form.sat.value;
		prodVal  = document.form.prod.value;
		sfcVal   = document.form.sfc.value;
		yrVal    = document.form.yr.value;
		moVal    = document.form.mo.value;
		dyVal    = document.form.dy.value;
		
		loadImageHelper(satVal,prodVal,sfcVal,yrVal,moVal,dyVal);
	}
	else {
		document.form.yr.value = yr;
		document.form.mo.value = mo ;
		document.form.dy.value = dy;
		document.form.sat.value=sat;
		document.form.prod.value=prod;
		document.form.sfc.value=sfc;
		
		document.form.yr2.value = yr;
		document.form.mo2.value = mo ;
		document.form.dy2.value = dy;
		document.form.sat2.value=sat;
		document.form.prod2.value=prod;
		document.form.sfc2.value=sfc;
		
		loadImageHelper(sat,prod,sfc,yr,mo,dy);
	}
	
}


function loadInitialImagesv(sat,prod,sfc,yr,mo,dy) {
	
	if ( sat == "" ) {
		var now = new Date();
		now.setDate(now.getDate()-16) ; // 14 days ago

		var year  = now.getFullYear();
		var month = now.getMonth();  // month starting index is 0
		var day   = now.getDate();
		
		document.form.sat.selectedIndex=0;
		document.form.prod.value="rr";
		document.form.sfc.selectedIndex=0;
		document.form.yr.value = year;
		document.form.mo.selectedIndex = month ;
		document.form.dy.selectedIndex = day - 1;

		document.form.sat2.selectedIndex=0;
		document.form.prod2.value="rr";
		document.form.sfc2.selectedIndex=0;
		document.form.yr2.value = year;
		document.form.mo2.selectedIndex = month ;
		document.form.dy2.selectedIndex = day - 1;
		
		satVal   = document.form.sat.value;
		prodVal  = document.form.prod.value;
		sfcVal   = document.form.sfc.value;
		yrVal    = document.form.yr.value;
		moVal    = document.form.mo.value;
		dyVal    = document.form.dy.value;
		
		loadImageHelper(satVal,prodVal,sfcVal,yrVal,moVal,dyVal);
	}
	else {
		document.form.yr.value = yr;
		document.form.mo.value = mo ;
		document.form.dy.value = dy;
		document.form.sat.value=sat;
		document.form.prod.value=prod;
		document.form.sfc.value=sfc;
		
		document.form.yr2.value = yr;
		document.form.mo2.value = mo ;
		document.form.dy2.value = dy;
		document.form.sat2.value=sat;
		document.form.prod2.value=prod;
		document.form.sfc2.value=sfc;
		changeProductv( prod );
		loadImageHelper(sat,prod,sfc,yr,mo,dy);
	}
	
}


function changeProduct( prod ) {

	document.form.prod.value=prod; 
	document.form.prod2.value=prod;
	
	if( prod == "pixel" ) {
		document.getElementById("box3").style.display  = "none";
		document.getElementById("box4").style.display  = "none";
		document.getElementById("box5").style.display  = "none";
		document.getElementById("box6").style.display  = "none";
	
	}
	else if( prod == "clw" || prod == "iwp" || prod == "lwp" ) {
	
		document.getElementById("box3").style.display  = "table-cell";
		document.getElementById("box4").style.display  = "table-cell";
		document.getElementById("box5").style.display  = "none";
		document.getElementById("box6").style.display  = "none";
	
		document.getElementById("box3").width  = 325; 
		document.getElementById("box3").height = 275; 
		document.form.img3.width  = 325;
		document.form.img3.height = 250;
	
		document.getElementById("box4").width  = 325; 
		document.getElementById("box4").height = 275; 
		document.form.img4.width  = 325;
		document.form.img4.height = 250;
	}
	else if( prod == "rr" ) {
	
		document.getElementById("box3").style.display  = "table-cell";
		document.getElementById("box4").style.display  = "table-cell";
		document.getElementById("box5").style.display  = "table-cell";
		document.getElementById("box6").style.display  = "table-cell";
	
		document.getElementById("box3").width  = 325; 
		document.getElementById("box3").height = 275; 
		document.form.img3.width  = 325;
		document.form.img3.height = 250;
	
		document.getElementById("box4").width  = 325; 
		document.getElementById("box4").height = 275; 
		document.form.img4.width  = 325;
		document.form.img4.height = 250;
		
		document.getElementById("box5").width  = 325; 
		document.getElementById("box5").height = 275; 
		document.form.img3.width  = 325;
		document.form.img3.height = 250;
	
		document.getElementById("box6").width  = 325; 
		document.getElementById("box6").height = 275; 
		document.form.img4.width  = 325;
		document.form.img4.height = 250;
	}
	
}


function changeProductv( prod ) {

	document.form.prod.value=prod; 
	document.form.prod2.value=prod;

	if( prod == "pixel" ) {
		document.getElementById("box3").style.display  = "none";
		document.getElementById("box4").style.display  = "none";
		document.getElementById("box5").style.display  = "none";
		document.getElementById("box6").style.display  = "none";
	
	}
	else if( prod == "clw" || prod == "iwp" || prod == "lwp" ) {
	
		document.getElementById("box3").style.display  = "table-cell";
		document.getElementById("box4").style.display  = "table-cell";
		document.getElementById("box5").style.display  = "none";
		document.getElementById("box6").style.display  = "none";
	
		document.getElementById("box3").width  = 650; 
		document.getElementById("box3").height = 500; 
		document.form.img3.width  = 650;
		document.form.img3.height = 500;
	
		document.getElementById("box4").width  = 650; 
		document.getElementById("box4").height = 500; 
		document.form.img4.width  = 650;
		document.form.img4.height = 500;
	}
	else if( prod == "rr" ) {
	
		document.getElementById("box3").style.display  = "table-cell";
		document.getElementById("box4").style.display  = "table-cell";
		document.getElementById("box5").style.display  = "table-cell";
		document.getElementById("box6").style.display  = "table-cell";
	
		document.getElementById("box3").width  = 650; 
		document.getElementById("box3").height = 500; 
		document.form.img3.width  = 650;
		document.form.img3.height = 500;
	
		document.getElementById("box4").width  = 650; 
		document.getElementById("box4").height = 500; 
		document.form.img4.width  = 650;
		document.form.img4.height = 500;
		
		document.getElementById("box5").width  = 650; 
		document.getElementById("box5").height = 500; 
		document.form.img3.width  = 650;
		document.form.img3.height = 500;
	
		document.getElementById("box6").width  = 650; 
		document.getElementById("box6").height = 500; 
		document.form.img4.width  = 650;
		document.form.img4.height = 500;
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
	sfcVal   = document.form.sfc.value;
	yrVal    = document.form.yr.value;
	moVal    = document.form.mo.value;
	dyVal    = document.form.dy.value;
	
	loadImageHelper(satVal,prodVal,sfcVal,yrVal,moVal,dyVal);
}
