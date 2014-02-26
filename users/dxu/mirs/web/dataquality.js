var pathImg = "images/" ;
var dash = "_" ;
var tag = "orbit_monitoring_" ;

var SATPREFIX = new Array();
SATPREFIX['n18']    = 'mirs_adv_poes_n18_amsuamhs_';
SATPREFIX['n19']    = 'mirs_adv_poes_n19_amsuamhs_';
SATPREFIX['metopA'] = 'mirs_adv_poes_metopA_amsuamhs_';
SATPREFIX['metopB'] = 'mirs_adv_poes_metopB_amsuamhs_';
SATPREFIX['f16']    = 'mirs_adv_dmsp_f16_ssmis_';
SATPREFIX['f18']    = 'mirs_adv_dmsp_f18_ssmis_';
SATPREFIX['npp']    = 'mirs_adv_npoess_npp_atms_';

var NOW = new Date();
NOW.setDate(NOW.getDate()-1);
var SECOND = NOW.getTime()/1000;

var year  = NOW.getFullYear();
var month = NOW.getMonth();
var day   = NOW.getDate();

// starting date is 11/16/2011
var DATE_MIN = new Date(2011, 10, 16);
var SECOND_MIN = DATE_MIN.getTime()/1000;



function showYmd( i ) {

       	var yrId = 'yr' + i + "";
       	var moId = 'mo' + i + "";
       	var dyId = 'dy' + i + "";
       	var prevId = 'prev' + i + "";
       	var nextId = 'next' + i + "";

	document.getElementById(yrId).className = "optionvisible";
	document.getElementById(moId).className = "optionvisible";
	document.getElementById(dyId).className = "optionvisible";
	document.getElementById(prevId).className = "inputvisible";
	document.getElementById(nextId).className = "inputvisible";
}


function hideYmd( i ) {

       	var yrId = 'yr' + i + "";
       	var moId = 'mo' + i + "";
       	var dyId = 'dy' + i + "";
       	var prevId = 'prev' + i + "";
       	var nextId = 'next' + i + "";

	document.getElementById(yrId).className = "optioninvisible";
	document.getElementById(moId).className = "optioninvisible";
	document.getElementById(dyId).className = "optioninvisible";
	document.getElementById(prevId).className = "inputinvisible";
	document.getElementById(nextId).className = "inputinvisible";
}


function changeSat( satVal, i ) {
	
	var satId = "sat" + i;
	var scaleId = "scale" + i;
	
	var sat_obj = document.getElementById(satId);
	var scale_obj = document.getElementById(scaleId);
	
	if ( satVal == "npp" ) {
		scale_obj.options[1] = new Option();
		scale_obj.options[1].value = "daily";
		scale_obj.options[1].text  = "Daily";

		if( scale_obj.value == "daily" )
			showYmd(i);
		else
			hideYmd(i);
		
	}
	else {
		scale_obj.options.length = 1;
		
		hideYmd(i);
	}
}


function changeScale( sclVal, i ) { 
       
	if( sclVal == "daily" ) {
		showYmd(i);
	}
	else {
		hideYmd(i);
	}
}


function loadImage( i ) {
	
	var scaleId = "scale" + i ;
	var satId = "sat" + i ;
	var chanId = "chan" + i ;
	var yrId = "yr" + i ;
	var moId = "mo" + i ;
	var dyId = "dy" + i ;
	var imgId = "img" + i;
	var hrefId = "href" + i;
	
	var scale = document.getElementById(scaleId).value;
	var sat = document.getElementById(satId).value;
	var chan = document.getElementById(chanId).value;
	var yr = document.getElementById(yrId).value;
	var mo = document.getElementById(moId).value;
	var dy = document.getElementById(dyId).value;
	
	var ymd = '/'+yr+'-'+mo+'-'+dy+'/';
	var yyyymmdd = yr+mo+dy;

	var dirImg  = "";
	var img_reg = "";
 	var img_big = "";
	
	if( scale == "daily" ) { 
		//dirImg = pathImg + sat + ymd;
		dirImg = pathImg + sat + "/dataquality/";
		img_reg = dirImg + SATPREFIX[sat] + tag + chan + dash + yyyymmdd + "_small.png"
		img_big = dirImg + SATPREFIX[sat] + tag + chan + dash + yyyymmdd + ".png"
	}
	else {
		dirImg = pathImg + sat + "/dataquality/";
		img_reg = dirImg + SATPREFIX[sat] + tag + chan + "_small.png"
 		img_big = dirImg + SATPREFIX[sat] + tag + chan + ".png"
	}
	
	document.getElementById(imgId).src = img_reg;
	document.getElementById(imgId).alt = img_reg;
	document.getElementById(hrefId).href = img_big;
}

function loadImagev( i ) {
	
	var scaleId = "scale" + i ;
	var satId = "sat" + i ;
	var chanId = "chan" + i ;
	var yrId = "yr" + i ;
	var moId = "mo" + i ;
	var dyId = "dy" + i ;
	var imgId = "img" + i;
	var hrefId = "href" + i;
	
	var scale = document.getElementById(scaleId).value;
	var sat = document.getElementById(satId).value;
	var chan = document.getElementById(chanId).value;
	var yr = document.getElementById(yrId).value;
	var mo = document.getElementById(moId).value;
	var dy = document.getElementById(dyId).value;
	
	var ymd = '/'+yr+'-'+mo+'-'+dy+'/';
	var yyyymmdd = yr+mo+dy;

	var dirImg  = "";
	var img_reg = "";
 	var img_big = "";
	
	if( scale == "daily" ) { 
		//dirImg = pathImg + sat + ymd;
		dirImg = pathImg + sat + "/dataquality/";
		img_reg = dirImg + SATPREFIX[sat] + tag + chan + dash + yyyymmdd + ".png"
		img_big = dirImg + SATPREFIX[sat] + tag + chan + dash + yyyymmdd + ".png"
	}
	else {
		dirImg = pathImg + sat + "/dataquality/";
		img_reg = dirImg + SATPREFIX[sat] + tag + chan + ".png"
 		img_big = dirImg + SATPREFIX[sat] + tag + chan + ".png"
	}
	
	document.getElementById(imgId).src = img_reg;
	document.getElementById(imgId).alt = img_reg;
	document.getElementById(hrefId).href = img_big;
}


function init(	sat1,scale1,chan1,yr1,mo1,dy1,
		sat2,scale2,chan2,yr2,mo2,dy2,
		sat3,scale3,chan3,yr3,mo3,dy3,
		sat4,scale4,chan4,yr4,mo4,dy4) 
{
	if ( sat1 != '' ) 
	{
		document.form.sat1.value = sat1;
		document.form.sat2.value = sat2;
		document.form.sat3.value = sat3;
		document.form.sat4.value = sat4;

		changeSat( sat1, 1 );
		changeSat( sat2, 2 );
		changeSat( sat3, 3 );
		changeSat( sat4, 4 );
	
		document.form.scale1.value = scale1;
		document.form.scale2.value = scale2;
		document.form.scale3.value = scale3;
		document.form.scale4.value = scale4;

		document.form.chan1.value = chan1;
		document.form.chan2.value = chan2;
		document.form.chan3.value = chan3;
		document.form.chan4.value = chan4;
	}
	else {
		document.form.chan1.selectedIndex = 0;
		document.form.chan2.selectedIndex = 1;
		document.form.chan3.selectedIndex = 2;
		document.form.chan4.selectedIndex = 3;
		
		scale1 = document.form.scale1.value;
	        scale2 = document.form.scale2.value;
	        scale3 = document.form.scale3.value;
	        scale4 = document.form.scale4.value;
	}

	
	if( yr1 == '' ) 
	{
		document.form.yr1.value = year;
		document.form.yr2.value = year;
		document.form.yr3.value = year;
		document.form.yr4.value = year;
	
        	document.form.mo1.selectedIndex = month ;
        	document.form.mo2.selectedIndex = month ;
        	document.form.mo3.selectedIndex = month ;
        	document.form.mo4.selectedIndex = month ;
	
        	document.form.dy1.selectedIndex = day - 1 ;
        	document.form.dy2.selectedIndex = day - 1 ;
        	document.form.dy3.selectedIndex = day - 1 ;
        	document.form.dy4.selectedIndex = day - 1 ;
		
		yr1 = year;
		yr2 = year;
		yr3 = year;
		yr4 = year;
		
		mo1 = document.form.mo1.value;
		mo2 = document.form.mo2.value;
		mo3 = document.form.mo3.value;
		mo4 = document.form.mo4.value;
		
		dy1 = document.form.dy1.value;
		dy2 = document.form.dy2.value;
		dy3 = document.form.dy3.value;
		dy4 = document.form.dy4.value;
		
	}
	else {
		document.form.yr1.value = yr1 ;
		document.form.yr2.value = yr2 ;
		document.form.yr3.value = yr3 ;
		document.form.yr4.value = yr4 ;
	
		document.form.mo1.value = mo1 ;
		document.form.mo2.value = mo2 ;
		document.form.mo3.value = mo3 ;
		document.form.mo4.value = mo4 ;

		document.form.dy1.value = dy1 ;
		document.form.dy2.value = dy2 ;
		document.form.dy3.value = dy3 ;
		document.form.dy4.value = dy4 ;
	}
	
	
	var scales = new Array(scale1,scale2,scale3,scale4);
	for( var i=1; i <= scales.length; i++ ) {
	
		if( scales[i-1] == "daily" )
			showYmd(i);
		else
			hideYmd(i);
	}
	
	for( var i=1; i <= 4; i++ ) loadImage(i);
	
}



function initv(	sat1,scale1,chan1,yr1,mo1,dy1,
		sat2,scale2,chan2,yr2,mo2,dy2,
		sat3,scale3,chan3,yr3,mo3,dy3,
		sat4,scale4,chan4,yr4,mo4,dy4) 
{
	if ( sat1 != '' ) 
	{
		document.form.sat1.value = sat1;
		document.form.sat2.value = sat2;
		document.form.sat3.value = sat3;
		document.form.sat4.value = sat4;

		changeSat( sat1, 1 );
		changeSat( sat2, 2 );
		changeSat( sat3, 3 );
		changeSat( sat4, 4 );
	
		document.form.scale1.value = scale1;
		document.form.scale2.value = scale2;
		document.form.scale3.value = scale3;
		document.form.scale4.value = scale4;

		document.form.chan1.value = chan1;
		document.form.chan2.value = chan2;
		document.form.chan3.value = chan3;
		document.form.chan4.value = chan4;
	}
	else {
		document.form.chan1.selectedIndex = 0;
		document.form.chan2.selectedIndex = 1;
		document.form.chan3.selectedIndex = 2;
		document.form.chan4.selectedIndex = 3;
		
		scale1 = document.form.scale1.value;
	        scale2 = document.form.scale2.value;
	        scale3 = document.form.scale3.value;
	        scale4 = document.form.scale4.value;
	}

	
	if( yr1 == '' ) 
	{
		document.form.yr1.value = year;
		document.form.yr2.value = year;
		document.form.yr3.value = year;
		document.form.yr4.value = year;
	
        	document.form.mo1.selectedIndex = month ;
        	document.form.mo2.selectedIndex = month ;
        	document.form.mo3.selectedIndex = month ;
        	document.form.mo4.selectedIndex = month ;
	
        	document.form.dy1.selectedIndex = day - 1 ;
        	document.form.dy2.selectedIndex = day - 1 ;
        	document.form.dy3.selectedIndex = day - 1 ;
        	document.form.dy4.selectedIndex = day - 1 ;
		
		yr1 = year;
		yr2 = year;
		yr3 = year;
		yr4 = year;
		
		mo1 = document.form.mo1.value;
		mo2 = document.form.mo2.value;
		mo3 = document.form.mo3.value;
		mo4 = document.form.mo4.value;
		
		dy1 = document.form.dy1.value;
		dy2 = document.form.dy2.value;
		dy3 = document.form.dy3.value;
		dy4 = document.form.dy4.value;
		
	}
	else {
		document.form.yr1.value = yr1 ;
		document.form.yr2.value = yr2 ;
		document.form.yr3.value = yr3 ;
		document.form.yr4.value = yr4 ;
	
		document.form.mo1.value = mo1 ;
		document.form.mo2.value = mo2 ;
		document.form.mo3.value = mo3 ;
		document.form.mo4.value = mo4 ;

		document.form.dy1.value = dy1 ;
		document.form.dy2.value = dy2 ;
		document.form.dy3.value = dy3 ;
		document.form.dy4.value = dy4 ;
	}
	
	var scales = new Array(scale1,scale2,scale3,scale4);
	for( var i=1; i <= scales.length; i++ ) {
	
		if( scales[i-1] == "daily" )
			showYmd(i);
		else
			hideYmd(i);
	}
	
	for( var i=1; i <= 4; i++ ) loadImagev(i);
	
}



function tableFormat() {
	document.form.action="qcconvg.php";
}


function verticalView() {
	document.form.action="dataqualityv.php";
}


function normalView() {
	document.form.action="dataquality.php";
}


function prev( i ) {
       	
	var yrId = 'yr' + i + "";
       	var moId = 'mo' + i + "";
       	var dyId = 'dy' + i + "";
	
	var yr_obj = document.getElementById(yrId);
	var mo_obj = document.getElementById(moId);
	var dy_obj = document.getElementById(dyId);
	
	var year  = parseInt(yr_obj.value,10);
	var month = parseInt(mo_obj.value,10);
	var day   = parseInt(dy_obj.value,10);

	var now = new Date(year,month-1,day);
	now.setDate(now.getDate()-1);

	year  = now.getFullYear();
	month = now.getMonth();
	day   = now.getDate();
    
	var second = now.getTime()/1000;
	if( second >= SECOND_MIN ) {
		yr_obj.value = year;
		mo_obj.selectedIndex = month ;
		dy_obj.selectedIndex = day - 1;
	}
}



function next( i ) {

	var yrId = 'yr' + i + "";
       	var moId = 'mo' + i + "";
       	var dyId = 'dy' + i + "";
	
	var yr_obj = document.getElementById(yrId);
	var mo_obj = document.getElementById(moId);
	var dy_obj = document.getElementById(dyId);
	
	var year  = parseInt(yr_obj.value,10);
	var month = parseInt(mo_obj.value,10);
	var day   = parseInt(dy_obj.value,10);

	var now = new Date(year,month-1,day);
	now.setDate(now.getDate()+1);

	year  = now.getFullYear();
	month = now.getMonth();
	day   = now.getDate();
    
	var second = now.getTime()/1000;
	if( second <= SECOND ) {
		yr_obj.value = year;
		mo_obj.selectedIndex = month ;
		dy_obj.selectedIndex = day - 1;
	}
}





