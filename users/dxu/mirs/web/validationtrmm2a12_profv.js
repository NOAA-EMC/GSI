var IMGDIR="images/";
var header="mirs_adv_poes_";
var sensor="_trmm_2A12_coll_prof_";
var region="_glb_";
var ndayback=7;

function loadImageHelper(sat,prod,sfc,yr,mo,dy) {
    
	var ymd =  "/" + yr + "-" + mo + "-" + dy + "/";
	
	if ( sat == 'f16' || sat == 'f18') {
		header="mirs_adv_dmsp_";
	}
	else if ( sat == 'npp' ) {
		header="mirs_adv_npoess_";
	}
	else if ( sat == 'gcomw1' ) {
		header="mirs_adv_eos_";
	}
	else {
		header="mirs_adv_poes_";
	}
	
	var id = sat + "_" + prod + "_" + sfc + "_" + yr + "_" + mo + "_" + dy;
	var num = NUM_IMG[id]; 
	
	var part  = sensor; 
	var dash  = "_";
	var cend  = "ad";
	
	var img_pref = IMGDIR+sat+ymd+header+sat+part+prod+dash+sfc+dash;

    if( num === undefined  || num == 0 ) {
 	for(i=1;i<=40;i++) {
   	
	  var img_id = 'img' + parseInt(i);
	  var td_id  = 'td' + parseInt(i);
	  var tr_id  = 'tr' + parseInt(i);
	  
	  document.getElementById(img_id).style.display = "none";
	  document.getElementById(td_id).style.display  = "none";
	  document.getElementById(tr_id).style.display  = "none";
	
	}     
    }
    else 
    {
	for(i=1;i<=num;i++)
	{
	  var img_id = 'img' + parseInt(i);
	  var img = img_pref + parseInt(i) + ".png";
	  
	  document.getElementById(img_id).src = img;
	  
	  var href_id = 'href' + parseInt(i);
	  document.getElementById(href_id).href=img;
	  
	  var index = img.lastIndexOf("/");
	  var alt   = img.substring(index+1,img.length);
	  document.getElementById(img_id).title=img;
	  document.getElementById(img_id).alt=alt;
	  
	  //document.getElementById(img_id).height=500;
	  //document.getElementById(img_id).width=650;
	  document.getElementById(img_id).style.display = "block";
	  
	  var td_id = 'td' + parseInt(i);
	  document.getElementById(td_id).className="tdvisible";
	  //document.getElementById(td_id).height=500;
	  //document.getElementById(td_id).width=650;
	  document.getElementById(td_id).style.display = "block";
	  //document.getElementById(td_id).align = "center";
	  
	  var tr_id = 'tr' + parseInt(i);
	  document.getElementById(tr_id).style.display = "block";
	}
	
	for(i=num+1;i<=40;i++)
	{
	  var img_id = 'img' + parseInt(i);
	  //document.getElementById(img_id).height=0;
	  //document.getElementById(img_id).width=0;
	  document.getElementById(img_id).style.display = "none";
	  
	  var td_id = 'td' + parseInt(i);
	  //document.getElementById(td_id).height=0;
	  //document.getElementById(td_id).width=0;
	  //document.getElementById(td_id).className="tdinvisible";
	  document.getElementById(td_id).style.display  = "none";

	  var tr_id = 'tr' + parseInt(i);
	  document.getElementById(tr_id).style.display = "none";

	}
    }	
}


function loadImage_top() {
    
	var sat   = document.form.sat.value;
	var prod  = document.form.prod.value;
	var sfc   = document.form.sfc.value;
	var yr    = document.form.yr.value;
	var mo    = document.form.mo.value;
	var dy    = document.form.dy.value;
	
}


function loadInitialImages(sat,prod,sfc,yr,mo,dy) {
	
	if ( sat == "" ) {
		var now = new Date();
		now.setDate(now.getDate()-ndayback) ; // n days ago

		var year  = now.getFullYear();
		var month = now.getMonth();  // month starting index is 0
		var day   = now.getDate();
		
		document.form.sat.selectedIndex=0;
		document.form.prod.selectedIndex=0;
		document.form.sfc.selectedIndex=0;
		document.form.yr.value = year;
		document.form.mo.selectedIndex = month ;
		document.form.dy.selectedIndex = day - 1;

	}
	else {
		document.form.yr.value = yr;
		document.form.mo.value = mo ;
		document.form.dy.value = dy;
		document.form.sat.value = sat;
		document.form.prod.value = prod;
		document.form.sfc.value = sfc;
		
	}
	
}


function loadInitialImagesv(sat,prod,sfc,yr,mo,dy) {
	
	if ( sat == "" ) {
		var now = new Date();
		now.setDate(now.getDate()-ndayback) ; // n days ago

		var year  = now.getFullYear();
		var month = now.getMonth();  // month starting index is 0
		var day   = now.getDate();
		
		document.form.sat.selectedIndex=0;
		document.form.prod.selectedIndex=0;
		document.form.sfc.selectedIndex=0;
		document.form.yr.value = year;
		document.form.mo.selectedIndex = month ;
		document.form.dy.selectedIndex = day - 1;

		document.form.sat2.selectedIndex=0;
		document.form.prod2.selectedIndex=0;
		document.form.sfc2.selectedIndex=0;
		document.form.yr2.value = year ;
		document.form.mo2.selectedIndex = month ;
		document.form.dy2.selectedIndex = day - 1;
	}
	else {
		document.form.yr.value = yr;
		document.form.mo.value = mo ;
		document.form.dy.value = dy;
		document.form.sat.value =sat;
		document.form.prod.value = prod;
		document.form.sfc.value = sfc;
		
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

	document.form.yr.value = year + '';
	document.form.mo.selectedIndex = month ;
	document.form.dy.selectedIndex = day - 1;

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

        document.form.yr.value = year + '';
        document.form.mo.selectedIndex = month ;
        document.form.dy.selectedIndex = day - 1;

}
