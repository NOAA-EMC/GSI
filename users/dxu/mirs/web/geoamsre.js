var imgDir = "images/";
var ndayback = 4;

function changeProduct( prodval ) 
{
	
	prod = prodval ;
	
	if( prod == "swe" ) {
		document.getElementById("layer").className  = "optioninvisible";
		document.getElementById("layer2").className = "optioninvisible";
		
		document.form.view.options.length = 2;
		document.form.view2.options.length = 2;
		
	}
	else if ( prod == "sice" ) {
	
		document.getElementById("layer").className  = "optionvisible";
		document.getElementById("layer2").className = "optionvisible";
		
		document.form.view.options.length = 3;
		document.form.view.options[2] = new Option();
		document.form.view.options[2].value = "ps";
		document.form.view.options[2].text  = "S. Hemiphere";
		
		document.form.view2.options.length = 3;
		document.form.view2.options[2] = new Option();
		document.form.view2.options[2].value = "ps";
		document.form.view2.options[2].text  = "S. Hemiphere";
	}
	
}


function changeSize( product ) {
	
	if ( product == "sice" ) {
		document.getElementById("box5").style.display = "table-cell";	
		document.getElementById("box6").style.display = "table-cell";
		document.getElementById("box7").style.display = "table-cell";	
		document.getElementById("box8").style.display = "table-cell";	
	}
	else {
		document.getElementById("box5").style.display = "none";   
		document.getElementById("box6").style.display = "none";
		document.getElementById("box7").style.display = "none";   
		document.getElementById("box8").style.display = "none";   
	} 
}


function changeSizeV( product ) {
	
	if ( product == "sice" ) {
		document.getElementById("box5").style.display = "table-row";   
		document.getElementById("box6").style.display = "table-row";
		document.getElementById("box7").style.display = "table-row";   
		document.getElementById("box8").style.display = "table-row";   
	}
	else {
		document.getElementById("box5").style.display = "none";   
		document.getElementById("box6").style.display = "none";
		document.getElementById("box7").style.display = "none";   
		document.getElementById("box8").style.display = "none";   
	} 
}


function loadImage() {
    
	var yr = document.form.yr.value;
	var mo = document.form.mo.value;
	var dy = document.form.dy.value;
	
	var cend  = document.form.cend.value;
	var view  = document.form.view.value;
	
	var sat   = document.form.sat.value;
 	var prod  = document.form.prod.value;
	var layer = document.form.layer.value;
	
	// update bottom panel
	document.form.yr2.value = yr;
	document.form.mo2.value = mo;
	document.form.dy2.value = dy;

	document.form.cend2.value = cend;
	document.form.view2.value = view;

	document.form.sat2.value   = sat;
	document.form.prod2.value  = prod; 
	document.form.layer2.value = layer;

	loadImageHelp(sat,prod,layer,cend,view,yr,mo,dy);
	
}


function loadImage_bottom() {
    
	var yr = document.form.yr2.value;
	var mo = document.form.mo2.value;
	var dy = document.form.dy2.value;
	
	var cend = document.form.cend2.value;
	var view = document.form.view2.value;
	
	var sat   = document.form.sat2.value;
 	var prod  = document.form.prod2.value;
	var layer = document.form.layer2.value;
	
	// update upper panel
	document.form.yr.value = yr;
	document.form.mo.value = mo;
	document.form.dy.value = dy;

	document.form.cend.value = cend;
	document.form.view.value = view;

	document.form.sat.value   = sat;
	document.form.prod.value  = prod; 
	document.form.layer.value = layer;
	
	loadImageHelp(sat,prod,layer,cend,view,yr,mo,dy);
	
}


function loadImageHelp(sat,prod,layer,cend,view,yr,mo,dy) {
    
	var ymd =  "/" + yr + "-" + mo + "-" + dy + "/";
	
	if ( sat == 'f16' || sat == 'f18' ) {
		header="mirs_adv_dmsp_";
		sensor="_ssmis";
	}
	else {
		header="mirs_adv_poes_";
		sensor="_amsuamhs";
	}
	var region = "_glb_";
	
	var name1 = sensor + region ;
	
	var dash  = "_";
	
	var img1 = "";
	var img2 = "";
	var img3 = "";
	var img4 = "";	
	var img5 = "";	
	var img6 = "";	
	var img7 = "";	
	var img8 = "";	
	
	if ( prod == "sice" ) {
  		var sfc = "sea";
		img1 = imgDir+sat+ymd+header+sat+name1+yr+mo+dy+dash+prod+dash+view+dash+sfc+dash+cend+".png";
  		img2 = imgDir+sat+ymd+'valid_amsrsic_amsre_glb_'   +yr+mo+dy+dash+prod+dash+layer+dash+view+dash+sfc+dash+'dy'+".png"; 
  		img3 = imgDir+sat+ymd+'valid_'+sat+'_amsrdiff_glb_'+yr+mo+dy+dash+prod+dash+layer+'dy'+dash+view+dash+sfc+dash+cend+".png"; 
  		img4 = imgDir+sat+ymd+'valid_amsrdiff_glb_'+yr+mo+dy+dash+prod+dash+view+dash+sfc+dash+'dy'+".png";
  		img5 = imgDir+sat+ymd+'valid_'+sat+'_amsrdiff_glb_scat_'+yr+mo+dy+dash+prod+dash+layer+'dy'+dash+'pn'+dash+'all'+dash+cend+".png";
  		img6 = imgDir+sat+ymd+'valid_'+sat+'_amsrdiff_glb_scat_'+yr+mo+dy+dash+prod+dash+layer+'dy'+dash+'ps'+dash+'all'+dash+cend+".png";
		img7 = imgDir+sat+ymd+'valid_amsrdiff_glb_scat_'+yr+mo+dy+dash+prod+dash+'pn_all_dy.png'
		img8 = imgDir+sat+ymd+'valid_amsrdiff_glb_scat_'+yr+mo+dy+dash+prod+dash+'ps_all_dy.png'
	}
	else if ( prod == "swe" ) {
  		var sfc = "lnd";
		img1 = imgDir+sat+ymd+header+sat+name1+yr+mo+dy+dash+prod+dash+view+dash+sfc+dash+cend+".png";
  		img2 = imgDir+sat+ymd+'valid_amsrswe_amsre_glb_'+yr+mo+dy+dash+prod+dash+view+dash+sfc+".png"; 
  		img3 = imgDir+sat+ymd+'valid_'+sat+'_amsrdiff_glb_'+yr+mo+dy+dash+prod+dash+view+dash+sfc+dash+cend+".png"; 
  		img4 = imgDir+sat+ymd+'valid_'+sat+'_amsrdiff_glb_scat_'+yr+mo+dy+dash+prod+dash+"snw"+dash+cend+".png"; 
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
	
	if  ( prod == "sice") {       
            document.form.img5.src = img5;
            document.form.img6.src = img6;
	    document.form.img7.src = img7;
            document.form.img8.src = img8;
	   
	    document.form.img5.title = img5;
	    document.form.img6.title = img6;
	    document.form.img7.title = img7;
	    document.form.img8.title = img8;
	
	    var index5 = img5.lastIndexOf("/");
	    var alt5   = img5.substring(index5+1,img5.length);

	    var index6 = img6.lastIndexOf("/");
	    var alt6   = img6.substring(index6+1,img6.length);

	    var index7 = img7.lastIndexOf("/");
	    var alt7   = img7.substring(index7+1,img7.length);
	
	    var index8 = img8.lastIndexOf("/");
	    var alt8   = img8.substring(index8+1,img8.length);
	   
	    document.form.img5.alt = alt5;
	    document.form.img6.alt = alt6;
	    document.form.img7.alt = alt7;
	    document.form.img8.alt = alt8;
	
	    document.getElementById("href5").href = img5;
	    document.getElementById("href6").href = img6;
	    document.getElementById("href7").href = img7;
	    document.getElementById("href8").href = img8;
	}
	else {	
	    document.form.img5.src = "";
            document.form.img6.src = "";
	    document.form.img7.src = "";
            document.form.img8.src = "";
	
	    document.form.img5.title = "";
	    document.form.img6.title = "";
	    document.form.img7.title = "";
	    document.form.img8.title = "";
	
	    document.form.img5.alt = "";
	    document.form.img6.alt = "";
	    document.form.img7.alt = "";
	    document.form.img8.alt = "";
	
	    document.getElementById("href5").href = "";
	    document.getElementById("href6").href = "";
	    document.getElementById("href7").href = "";
	    document.getElementById("href8").href = "";
	}
	
}


function loadInitialImages(sat,prod,layer,cend,view,yr,mo,dy) {
	
	if ( sat == "" ) {
		var now = new Date();
		now.setDate(now.getDate()-ndayback) ; // n day ago

		var year  = now.getFullYear();
		var month = now.getMonth();  // month starting index is 0
		var day   = now.getDate();

		document.form.yr.value = year;
		document.form.mo.selectedIndex = month;
		document.form.dy.selectedIndex = day - 1;
		document.form.sat.selectedIndex = 0;
		document.form.prod.selectedIndex = 0;
		document.form.layer.selectedIndex = 0;
		document.form.cend.selectedIndex = 0;
		document.form.view.selectedIndex = 0;

		document.form.yr2.value = year;
		document.form.mo2.selectedIndex = month;
		document.form.dy2.selectedIndex = day - 1;
		document.form.sat2.selectedIndex = 0;
		document.form.prod2.selectedIndex = 0;
		document.form.layer2.selectedIndex = 0;
		document.form.cend2.selectedIndex = 0;
		document.form.view2.selectedIndex = 0;

		document.getElementById("box5").style.display = "none";
		document.getElementById("box6").style.display = "none";
		document.getElementById("box7").style.display = "none";
		document.getElementById("box8").style.display = "none";
		  
		loadImage();
	}
	else {
		
		document.form.yr.value = yr;
		document.form.mo.value = mo ;
		document.form.dy.value = dy;
		document.form.sat.value = sat;
		document.form.prod.value = prod;
		document.form.layer.value = layer;
		document.form.cend.value = cend;
		document.form.view.value = view;
		
		document.form.yr2.value = yr;
		document.form.mo2.value = mo ;
		document.form.dy2.value = dy;
		document.form.sat2.value = sat;
		document.form.prod2.value = prod;
		document.form.layer2.value = layer;
		document.form.cend2.value = cend;
		document.form.view2.value = view;
		
		changeSize(prod);
		loadImageHelp(sat,prod,layer,cend,view,yr,mo,dy);
	}
	
}


function loadInitialImagesV(sat,prod,layer,cend,view,yr,mo,dy) {
	
	if ( sat == "" ) {
		var now = new Date();
		now.setDate(now.getDate()-ndayback) ; // n day ago

		var year  = now.getFullYear();
		var month = now.getMonth();  // month starting index is 0
		var day   = now.getDate();

		document.form.yr.value = year ;
		document.form.mo.selectedIndex = month ;
		document.form.dy.selectedIndex = day - 1;
		document.form.sat.selectedIndex = 0;
		document.form.prod.selectedIndex = 0;
		document.form.layer.selectedIndex = 0;
		document.form.cend.selectedIndex = 0;
		document.form.view.selectedIndex = 0;

		document.form.yr2.value = year ;
		document.form.mo2.selectedIndex = month ;
		document.form.dy2.selectedIndex = day - 1;
		document.form.sat2.selectedIndex = 0;
		document.form.prod2.selectedIndex = 0;
		document.form.layer2.selectedIndex = 0;
		document.form.cend2.selectedIndex = 0;
		document.form.view2.selectedIndex = 0;
		
		document.getElementById("box5").style.display = "none";   
		document.getElementById("box6").style.display = "none";
		document.getElementById("box7").style.display = "none";   
		document.getElementById("box8").style.display = "none";   

		loadImage();
	}
	else {
		
		document.form.yr.value = yr;
		document.form.mo.value = mo ;
		document.form.dy.value = dy;
		document.form.sat.value = sat;
		document.form.prod.value = prod;
		document.form.layer.value = layer;
		document.form.cend.value = cend;
		document.form.view.value = view;
		
		document.form.yr2.value = yr;
		document.form.mo2.value = mo ;
		document.form.dy2.value = dy;
		document.form.sat2.value = sat;
		document.form.prod2.value = prod;
		document.form.layer2.value = layer;
		document.form.cend2.value = cend;
		document.form.view2.value = view;
		
		changeSizeV(prod);
		loadImageHelp(sat,prod,layer,cend,view,yr,mo,dy);
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

	document.forms.form.yr.value = year;
	document.forms.form.mo.selectedIndex = month ;
	document.forms.form.dy.selectedIndex = day - 1;
	
	document.forms.form.yr2.value = year;
	document.forms.form.mo2.selectedIndex = month ;
	document.forms.form.dy2.selectedIndex = day - 1;
	
	loadImage(); 
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

        loadImage();
}
