var IMGPATH = "images/" ;
var IMGPRE  = "nedt" ;

var CHAN_POES = new Array("23v","31v","50v","52v","53h","54h","54v","55h","57h1","57h2","57h3","57h4","57h5","57h6","89v1","89v2","157h","184h","186h","190h");
var CHAN_ATMS = new Array("23v","31v","50h","51h","52h","53h","54h1","54h2","55h","57h1","57h2","57h3","57h4","57h5","57h6","88v","165h","183h1","183h2","183h3","183h4","183h5");

var NOW = new Date();
NOW.setDate(NOW.getDate()-1);
var SECOND = NOW.getTime()/1000;

var year  = NOW.getFullYear();
var month = NOW.getMonth();
var day   = NOW.getDate();

var DATE_MIN = new Date(2011, 11, 3);
var SECOND_MIN = DATE_MIN.getTime()/1000;




function showYmd( i ) {

       	var yrId = 'yr' + i ;
       	var moId = 'mo' + i ;
       	var dyId = 'dy' + i ;
       	var prevId = 'prev' + i ;
       	var nextId = 'next' + i ;

	document.getElementById(yrId).className = "optionvisible";
	document.getElementById(moId).className = "optionvisible";
	document.getElementById(dyId).className = "optionvisible";
	document.getElementById(prevId).className = "inputvisible";
	document.getElementById(nextId).className = "inputvisible";
}



function hideYmd( i ) {

       	var yrId = 'yr' + i ;
       	var moId = 'mo' + i ;
       	var dyId = 'dy' + i ;
       	var prevId = 'prev' + i ;
       	var nextId = 'next' + i ;

	document.getElementById(yrId).className = "optioninvisible";
	document.getElementById(moId).className = "optioninvisible";
	document.getElementById(dyId).className = "optioninvisible";
	document.getElementById(prevId).className = "inputinvisible";
	document.getElementById(nextId).className = "inputinvisible";
}



function changeSat( satVal, i ) {
	
	var satId = "sat" + i;
	var chanId = "chan" + i;
	var scaleId = "scale" + i;
	
	var sat_obj = document.getElementById(satId);
	var chan_obj = document.getElementById(chanId);
	var scale_obj = document.getElementById(scaleId);
	
	chan_obj.options.length = 0; 
	
	if ( satVal == "npp" ) {
		for ( var j=0; j<CHAN_ATMS.length; j++ ) {
			chan_obj.options[j] = new Option();
			chan_obj.options[j].value = String(j+1);
			chan_obj.options[j].text  = String(j+1) + ":" + CHAN_ATMS[j];
		}
		
		scale_obj.options[2] = new Option();
		scale_obj.options[2].value = "Daily";
		scale_obj.options[2].text  = "1-day";
		
		if( scale_obj.value == "Daily" )
			showYmd(i);
		else
			hideYmd(i);

	}
	else {
		for ( var j=0; j<CHAN_POES.length; j++ ) {
			chan_obj.options[j] = new Option();
			chan_obj.options[j].value = String(j+1);
			chan_obj.options[j].text  = String(j+1) + ":" + CHAN_POES[j];
		}
		
		scale_obj.options.length = 2;
		hideYmd(i);
		
	}
}



function changeScale( sclVal, i ) { 
       
	if( sclVal == "Daily" ) {
		showYmd(i);
	}
	else {
		hideYmd(i);
	}
}



function loadImage( i ) {
	
	var satId = "sat" + i ;
	var scaleId = "scale" + i ;
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

	var dirImg = IMGPATH + sat + "/nedt/";
	var img = dirImg + IMGPRE + scale + "_" + sat + "_ch" + chan + "_small.png" ;
 	var img_big = dirImg + IMGPRE + scale + "_" +sat + "_ch" + chan + "_big.png" ;
	
	if( scale == "Daily" ) { 
		dirImg = IMGPATH + sat + ymd;
		img = dirImg + IMGPRE + scale + "_" + sat + "_ch" + chan + "_" + yyyymmdd + "_small.png" ;
		img_big = dirImg + IMGPRE + scale + "_" + sat + "_ch" + chan + "_" + yyyymmdd + "_big.png" ;
	}

	document.getElementById(imgId).src = img;
	document.getElementById(imgId).alt = img;
	document.getElementById(hrefId).href = img_big;
}



function loadImagev( i ) {
	
	var satId = "sat" + i ;
	var scaleId = "scale" + i ;
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

	var dirImg = IMGPATH + sat + "/nedt/";
	var img = dirImg + IMGPRE + scale + "_" + sat + "_ch" + chan + ".png" ;
 	var img_big = dirImg + IMGPRE + scale + "_" +sat + "_ch" + chan + "_big.png" ;
	
	if( scale == "Daily" ) { 
		dirImg = IMGPATH + sat + ymd;
		img = dirImg + IMGPRE + scale + "_" + sat + "_ch" + chan + "_" + yyyymmdd + ".png" ;
		img_big = dirImg + IMGPRE + scale + "_" + sat + "_ch" + chan + "_" + yyyymmdd + "_big.png" ;
	}

	document.getElementById(imgId).src = img;
	document.getElementById(imgId).alt = img;
	document.getElementById(hrefId).href = img_big;
}



function fwd( i ) {
	
	var satId = "sat" + i ;
	var chanId = "chan" + i ;
	
	var sat_obj = document.getElementById(satId);
	var chan_obj = document.getElementById(chanId);
	
	var sat = sat_obj.value;
        var chan = parseInt(chan_obj.value,10);	

        chan = chan + 1 ;
	if (      sat == "npp" && chan > CHAN_ATMS.length ) chan = 1;
	else if ( sat != "npp" && chan > CHAN_POES.length ) chan = 1;
	
        chan_obj.value = chan;
	//chan_obj.selectedIndex = chan-1;
}



function rev( i ) {

	var satId = "sat" + i ;
	var chanId = "chan" + i ;
	
	var sat_obj = document.getElementById(satId);
	var chan_obj = document.getElementById(chanId);
	
	var sat = sat_obj.value;
        var chan = parseInt(chan_obj.value,10);
        
	chan = chan - 1;
	if      ( chan < 1 && sat == "npp" ) chan = CHAN_ATMS.length;
	else if ( chan < 1 && sat != "npp" ) chan = CHAN_POES.length;

        chan_obj.value = chan;
	//chan_obj.selectedIndex = chan-1;
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



function tableFormat() {
	document.form.action="qcnedt.php";
}



function verticalView() {
	document.form.action="nedtv.php";
}



function normalView() {
	document.form.action="nedt.php";
}



function init(	scale1,sat1,chan1,yr1,mo1,dy1,
		scale2,sat2,chan2,yr2,mo2,dy2,
		scale3,sat3,chan3,yr3,mo3,dy3,
		scale4,sat4,chan4,yr4,mo4,dy4) 
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
	
		if( scales[i-1] == "Daily" )
			showYmd(i);
		else
			hideYmd(i);
	}
	
	for( var i=1; i <= 4; i++ ) loadImage(i);
	
}



function initv(	scale1,sat1,chan1,yr1,mo1,dy1,
		scale2,sat2,chan2,yr2,mo2,dy2,
		scale3,sat3,chan3,yr3,mo3,dy3,
		scale4,sat4,chan4,yr4,mo4,dy4) 
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
	
		if( scales[i-1] == "Daily" )
			showYmd(i);
		else
			hideYmd(i);
	}
	
	for( var i=1; i <= 4; i++ ) loadImagev(i);
	
}
