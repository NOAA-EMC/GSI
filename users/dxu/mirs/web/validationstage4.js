var IMGDIR="images/st4/";

var header="mirs_adv_poes_";
var sensor="_st4";
var region="_conus_";
var ndayback=14;
var ref="st4";

function loadImageHelper(sat,prod,cend1,sfc,yr,mo,dy) {
    
	var ymd =  "/" + yr + "-" + mo + "-" + dy + "/";
	
	if ( sat == 'f16' || sat == 'f18' ) {
		header="mirs_adv_dmsp_";
	}
	else if( sat =='npp' ) {
		header="mirs_adv_poes_";
	}
	else if( sat =='gcomw1' ) {
		header="mirs_adv_eos_";
	}
	else if( sat =='trmm' ) {
		header="mirs_adv_poes_";
	}
	else {
		header="mirs_adv_poes_";
	}
	
	var part  = sensor + region + "p2p_";
	var part2 = sensor + region + "hist_" + "p2p_";
	var dash  = "_";
	var cend  = "ad";
	
	var img1="";
	var img2="";
	var img3="";	
	var img4="";	
	var img5="";	
	var img6="";	
	var img7="";
	var img8="";
	var img9="";
	var imga="";
		
	var sfc1="all";
	var sfc2="sea";
	var sfc3="lnd";
	var sfc4="ice";
	var sfc5="snw";
	
	if ( prod == "rr" ) {
		
	  img1 = IMGDIR+sat+ymd+header+sat+sensor+region+sat+"_map_"+yr+mo+dy+dash+prod+dash+sfc1+dash+cend1+".png";
  	  img2 = IMGDIR+sat+ymd+header+sat+sensor+region+ref+"_map_"+yr+mo+dy+dash+prod+dash+sfc1+dash+cend1+".png";
	  
	  img3 = IMGDIR+sat+ymd+header+sat+part+yr+mo+dy+dash+prod+dash+"day_"+sfc+dash+cend+".png";
  	  img4 = IMGDIR+sat+ymd+header+sat+part2+yr+mo+dy+dash+prod+dash+"day_"+sfc+dash+cend+".png"; 
	  
	  img5 = IMGDIR+sat+ymd+header+sat+part+yr+mo+dy+dash+prod+dash+sfc+dash+cend+".png";
  	  img6 = IMGDIR+sat+ymd+header+sat+part2+yr+mo+dy+dash+prod+dash+sfc+dash+cend+".png"; 
	  
	  img7 = IMGDIR+sat+ymd+header+sat+part+yr+mo+dy+dash+prod+"_dist_diff_"+sfc+dash+cend+".png";
	  img8 = IMGDIR+sat+ymd+header+sat+part+yr+mo+dy+dash+prod+"_time_diff_"+sfc+dash+cend+".png"; 
	  img9 = IMGDIR+sat+ymd+header+sat+part2+yr+mo+dy+"_dist_diff_"+sfc1+dash+cend+".png";
	  imga = IMGDIR+sat+ymd+header+sat+part2+yr+mo+dy+"_time_diff_"+sfc1+dash+cend+".png"; 

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
        document.form.img7.src = img7;
        document.form.img8.src = img8;
        document.form.img9.src = img9;
        document.form.imga.src = imga;
	
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
	
	var index7 = img7.lastIndexOf("/");
	var alt7   = img7.substring(index7+1,img7.length);
	
	var index8 = img8.lastIndexOf("/");
	var alt8   = img8.substring(index8+1,img8.length);

	var index9 = img9.lastIndexOf("/");
	var alt9   = img9.substring(index9+1,img9.length);

	var indexa = imga.lastIndexOf("/");
	var alta   = imga.substring(indexa+1,imga.length);

	document.form.img1.title=img1;
	document.form.img2.title=img2;
	document.form.img3.title=img3;
	document.form.img4.title=img4;
	document.form.img5.title=img5;
	document.form.img6.title=img6;
	document.form.img7.title=img7;
	document.form.img8.title=img8;
	document.form.img9.title=img9;
	document.form.imga.title=imga;
	
	document.form.img1.alt = alt1;
	document.form.img2.alt = alt2;
	document.form.img3.alt = alt3;
	document.form.img4.alt = alt4;
	document.form.img5.alt = alt5;
	document.form.img6.alt = alt6;
	document.form.img7.alt = alt7;
	document.form.img8.alt = alt8;
	document.form.img9.alt = alt9;
	document.form.imga.alt = alta;
	
	document.getElementById("href1").href=img1;
	document.getElementById("href2").href=img2;
	document.getElementById("href3").href=img3;
	document.getElementById("href4").href=img4;
	document.getElementById("href5").href=img5;
	document.getElementById("href6").href=img6;
	document.getElementById("href7").href=img7;
	document.getElementById("href8").href=img8;
	document.getElementById("href9").href=img9;
	document.getElementById("hrefa").href=imga;
}


function loadImage_top() {
    
	var sat   = document.form.sat.value;
 	var prod  = document.form.prod.value;
	var cend  = document.form.cend.value;
	var sfc   = document.form.sfc.value;
	var yr    = document.form.yr.value;
	var mo    = document.form.mo.value;
	var dy    = document.form.dy.value;
	
	// update bottom panel
	document.form.sat2.value   = sat;
	document.form.prod2.value  = prod; 
	document.form.cend2.value  = cend;
	document.form.sfc2.value   = sfc;
	document.form.yr2.value    = yr;
	document.form.mo2.value    = mo;
	document.form.dy2.value    = dy;
	
	loadImageHelper(sat,prod,cend,sfc,yr,mo,dy);
}

function loadImage_bottom() {
    
	var sat   = document.form.sat2.value;
 	var prod  = document.form.prod2.value;
	var cend  = document.form.cend2.value;
	var sfc   = document.form.sfc2.value;
	var yr    = document.form.yr2.value;
	var mo    = document.form.mo2.value;
	var dy    = document.form.dy2.value;
	
	// update upper panel
	document.form.sat.value  = sat;
	document.form.prod.value = prod; 
	document.form.cend.value = cend;
	document.form.sfc.value  = sfc;
	document.form.yr.value   = yr;
	document.form.mo.value   = mo;
	document.form.dy.value   = dy;

	loadImageHelper(sat,prod,cend,sfc,yr,mo,dy);
}

function loadInit(sat,prod,cend,sfc,yr,mo,dy) {
	
	if ( sat == "" ) {
		var now = new Date();
		now.setDate(now.getDate()-ndayback) ; // ndayback days ago
		
		var year  = now.getFullYear();
		var month = now.getMonth();  // month starting index is 0
		var day   = now.getDate();
		
		var now_rr = new Date();
		now_rr.setDate(now_rr.getDate()-7) ; // 7 days ago
		
		var year_rr  = now_rr.getFullYear();
		var month_rr = now_rr.getMonth();  // month starting index is 0
		var day_rr   = now_rr.getDate();
		
		document.form.sat.selectedIndex=0;
		document.form.prod.selectedIndex=0;
		document.form.cend.selectedIndex=0;
		document.form.sfc.selectedIndex=0;
		document.form.yr.value = year ;
		document.form.mo.selectedIndex = month ;
		document.form.dy.selectedIndex = day - 1;

		document.form.sat2.selectedIndex=0;
		document.form.prod2.selectedIndex=0;
		document.form.cend2.selectedIndex=0;
		document.form.sfc2.selectedIndex=0;
		document.form.yr2.value = year;
		document.form.mo2.selectedIndex = month ;
		document.form.dy2.selectedIndex = day - 1;
		
		satVal   = document.form.sat.value;
		prodVal  = document.form.prod.value;
		cendVal  = document.form.cend.value;
		sfcVal   = document.form.sfc.value;
		yrVal    = document.form.yr.value;
		moVal    = document.form.mo.value;
		dyVal    = document.form.dy.value;
		
		loadImageHelper(satVal,prodVal,cendVal,sfcVal,yrVal,moVal,dyVal);
	}
	else {
		document.form.yr.value    = yr;
		document.form.mo.value    = mo ;
		document.form.dy.value    = dy;
		document.form.sat.value   = sat;
		document.form.prod.value  = prod;
		document.form.sfc.value   = sfc;
		document.form.cend.value  = cend;
		
		document.form.yr2.value   = yr;
		document.form.mo2.value   = mo ;
		document.form.dy2.value   = dy;
		document.form.sat2.value  = sat;
		document.form.prod2.value = prod;
		document.form.sfc2.value  = sfc;
		document.form.cend2.value = cend;
		
		loadImageHelper(sat,prod,cend,sfc,yr,mo,dy);
	}
	
}


function changeProduct( prod ) {

	document.form.prod.value  = prod; 
	document.form.prod2.value = prod;
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

	document.form.yr.value = year ;
	document.form.mo.selectedIndex = month ;
	document.form.dy.selectedIndex = day - 1;
	
	document.form.yr2.value = year;
	document.form.mo2.selectedIndex = month ;
	document.form.dy2.selectedIndex = day - 1;
	
	satVal   = document.form.sat.value;
	prodVal  = document.form.prod.value;
	cendVal  = document.form.cend.value;
	sfcVal   = document.form.sfc.value;
	yrVal    = document.form.yr.value;
	moVal    = document.form.mo.value;
	dyVal    = document.form.dy.value;
	
	loadImageHelper(satVal,prodVal,cendVal,sfcVal,yrVal,moVal,dyVal);
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

        document.form.yr.value = year ;
        document.form.mo.selectedIndex = month ;
        document.form.dy.selectedIndex = day - 1;

        document.form.yr2.value = year ;
        document.form.mo2.selectedIndex = month ;
        document.form.dy2.selectedIndex = day - 1;

	satVal   = document.form.sat.value;
	prodVal  = document.form.prod.value;
	cendVal  = document.form.cend.value;
	sfcVal   = document.form.sfc.value;
	yrVal    = document.form.yr.value;
	moVal    = document.form.mo.value;
	dyVal    = document.form.dy.value;
	
	loadImageHelper(satVal,prodVal,cendVal,sfcVal,yrVal,moVal,dyVal);
}


// change panel formation
function changePanel( count ) {
	if     ( count == 1 ) document.form.action="validationstage4timeseriesv.php";
	else if( count == 2 ) document.form.action="validationstage4.php"; 
	else if( count == 3 ) document.form.action="validationstage4v.php"; 
}
