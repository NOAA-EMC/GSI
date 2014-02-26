var DIRIMGS = new Array();
DIRIMGS["n18"]    = "images/";
DIRIMGS["n19"]    = "images/";
DIRIMGS["metopA"] = "images/";
DIRIMGS["metopB"] = "images/";
DIRIMGS["f16"]    = "images/";
DIRIMGS["f18"]    = "images/";
DIRIMGS["trmm"]   = "images/";
DIRIMGS["npp"]    = "images/";
DIRIMGS["mtma"]   = "images/";
DIRIMGS["mtsa"]   = "images/";
DIRIMGS["gcomw1"] = "images/";

//var DIRIMG = "images/";

var NDAYBACK = 3;

var values = new Array();
values['n18']    = "23v:31v:50v:52v:53h:54h:54v:55h:57h1:57h2:57h3:57h4:57h5:57h6:89v1:89v2:157h:184h:186h:190v" ;
values['n19']    = "23v:31v:50v:52v:53h:54h:54v:55h:57h1:57h2:57h3:57h4:57h5:57h6:89v1:89v2:157h:184h:186h:190v" ;
values['metopA'] = "23v:31v:50v:52v:53h:54h:54v:55h:57h1:57h2:57h3:57h4:57h5:57h6:89v1:89v2:157h:184h:186h:190v" ;
values['metopB'] = "23v:31v:50v:52v:53h:54h:54v:55h:57h1:57h2:57h3:57h4:57h5:57h6:89v1:89v2:157h:184h:186h:190v" ;
values['f16']    = "50v:52v:53v:54v:55v:57rc:59rc:150h:190h:186h:184h:19h:19v:22v:37h:37v:91v:91h:63rc:60rc1:60rc2:60rc3:60rc4:60rc5";
values['f18']    = "50h:52h:53h:54h:55h:57rc:59rc:150h:190h:186h:184h:19h:19v:22v:37h:37v:91v:91h:63rc:60rc1:60rc2:60rc3:60rc4:60rc5";
values['trmm']   = "11v:11h:19v:19h:21v:37v:37h:85v:85h";
values['aqua']   = "7v:7h:11v:11h:19v:19h:24v:24h:37v:37h:89v:89h";
values["npp"]    = "23v:31v:50h:51h:52h:53h:54h1:54h2:55h:57h1:57h2:57h3:57h4:57h5:57h6:88v:165h:183h1:183h2:183h3:183h4:183h5" ;
values["mtma"]   = "19v:19h:24v:37v:37h:89v:89h:157v:157h" ;
values["mtsa"]   = "183h:184h:186h:187h:190h:194h" ;
values["gcomw1"] = "6v:6h:10v:10h:18v:18h:23v:23h:36v:36h:89v:89h" ;

var texts = new Array();
texts['n18']    = "23v:31v:50v:52v:53h:54h:54v:55h:57h1:57h2:57h3:57h4:57h5:57h6:89v1:89v2:157h:184h:186h:190v" ;
texts['n19']    = "23v:31v:50v:52v:53h:54h:54v:55h:57h1:57h2:57h3:57h4:57h5:57h6:89v1:89v2:157h:184h:186h:190v" ;
texts['metopA'] = "23v:31v:50v:52v:53h:54h:54v:55h:57h1:57h2:57h3:57h4:57h5:57h6:89v1:89v2:157h:184h:186h:190v" ;
texts['metopB'] = "23v:31v:50v:52v:53h:54h:54v:55h:57h1:57h2:57h3:57h4:57h5:57h6:89v1:89v2:157h:184h:186h:190v" ;
texts['f16']    = "50v:52v:53v:54v:55v:57rc:59rc:150h:190h:186h:184h:19h:19v:22v:37h:37v:91v:91h:63rc:60rc1:60rc2:60rc3:60rc4:60rc5";
texts['f18']    = "50h:52h:53h:54h:55h:57rc:59rc:150h:190h:186h:184h:19h:19v:22v:37h:37v:91v:91h:63rc:60rc1:60rc2:60rc3:60rc4:60rc5";
texts['trmm']   = "11v:11h:19v:19h:21v:37v:37h:85v:85h";
texts['aqua']   = "7v:7h:11v:11h:19v:19h:24v:24h:37v:37h:89v:89h";
texts["npp"]    = "23v:31v:50h:51h:52h:53h:54h1:54h2:55h:57h1:57h2:57h3:57h4:57h5:57h6:88v:165h:183h1:183h2:183h3:183h4:183h5" ;
texts["mtma"]   = "19v:19h:24v:37v:37h:89v:89h:157v:157h" ;
texts["mtsa"]   = "183h:184h:186h:187h:190h:194h" ;
texts["gcomw1"] = "6v:6h:10v:10h:18v:18h:23v:23h:36v:36h:89v:89h" ;



function changeRef( ref ) {}


function loadLayers( sat ) { 
        
       	layer_obj = document.form.layer; 
	var layer_old_value = layer_obj.value;
	
	layer_obj.options.length = 0 ;
	
	var layer_values = values[sat].split(":");
	var layer_texts  = texts[sat].split(":");
	for ( i=0; i<layer_values.length; i++ )  {
		layer_obj.options[i] = new Option();
		layer_obj.options[i].value = layer_values[i];
		layer_obj.options[i].text  = 'ch'+String(i+1)+':'+layer_texts[i];
	}
	
	for( i=0; i<layer_obj.options.length; i++) {
		if( layer_obj.options[i].value == layer_old_value )
			layer_obj.value = layer_old_value ;
	}
} 


function changeSensor( sat ) { 
	loadLayers( sat ) ;
} 


function changeProduct( prod ) { }


function loadImage() {

	var ref   = document.form.ref.value;
	var sat   = document.form.sat.value;
 	var prod  = document.form.prod.value;
	var layer = document.form.layer.value;
	var cend  = document.form.cend.value;
	var sfc   = document.form.sfc.value;
	var yr    = document.form.yr.value;
	var mo    = document.form.mo.value;
	var dy    = document.form.dy.value;
	
	loadImageHelper(ref,sat,prod,layer,cend,sfc,yr,mo,dy)    
}


function loadImageHelper(ref,sat,prod,layer,cend,sfc,yr,mo,dy) {
	
	var ymd  =  "/" + yr + "-" + mo + "-" + dy + "/";
	var dash = "_";
	
	var header = "mirs_adv_poes_";
	var name1 = "_amsuamhs_glb_";
	
	var name2 = dash + ref + "_glb_";
	var name3 = dash + ref + "_bias_glb_";
	
	//var name4 = "_gdas_biasasym_glb_";
	var name4 = dash + ref + "_asym_glb_";
	
	
	//var name5 = dash + ref + "_resi_glb_";
	var name5 = dash + ref + "_bias_glb_";
	
	//var name6 = "_gdas_biashist_glb_";
	var name6 = dash + ref + "_hist_glb_";
	
	//var name7 = "_gdas_resiasym_glb_";
	var name7 = dash + ref + "_asym_glb_";
	
	var name8 = "_amsuamhs_glb_";

	var offset = 15;
	
	if ( sat == "f16" || sat == "f18" ) {
		header = "mirs_adv_dmsp_";
		name1 = "_ssmis_glb_";
		name8 = "_ssmis_glb_";
	}
	else if ( sat == "aqua" ) {
		header = "mirs_adv_eos_";
		name1 = "_amsre_glb_";
		name8 = "_amsre_glb_";
		offset = 14;
	}
	else if ( sat == "npp" ) {
		header = "mirs_adv_npoess_";
		name1 = "_atms_glb_";
		name8 = "_atms_glb_";
		offset = 1;
	}
	else if ( sat == "trmm" ) {
		header = "mirs_adv_eos_";
		name1 = "_tmi_glb_";
		name8 = "_tmi_glb_";
		offset = 1;
	}
	else if ( sat == "mtma" ) {
		header = "mirs_adv_mt_";
		name1 = "_madras_glb_";
		name8 = "_madras_glb_";
		offset = 1;
	}
	else if ( sat == "mtsa" ) {
		header = "mirs_adv_mt_";
		name1 = "_saphir_glb_";
		name8 = "_saphir_glb_";
		offset = 1;
	}
	else if ( sat == "gcomw1" ) {
		header = "mirs_adv_eos_";
		name1 = "_amsr2_glb_";
		name8 = "_amsr2_glb_";
		offset = 1;
	}
	
	var img1 = "";
	var img2 = "";
	var img3 = "";
	var img4 = "";	
	var img5 = "";	
	var img6 = "";	
	var img7 = "";	
	var img8 = "";	
  	var DIRIMG = DIRIMGS[sat];
	
  	img1 = DIRIMG+sat+ymd+header+sat+name1+yr+mo+dy+dash+'tbc'+dash+layer+dash+sfc+dash+cend+".png";
  	img2 = DIRIMG+sat+ymd+header+sat+name2+yr+mo+dy+dash+'tb'+dash+layer+dash+sfc+dash+cend+".png"; 
  	img3 = DIRIMG+sat+ymd+header+sat+name3+yr+mo+dy+dash+'tbl'+dash+layer+dash+sfc+dash+cend+".png";
  	img4 = DIRIMG+sat+ymd+header+sat+name4+yr+mo+dy+dash+'tb'+dash+layer+dash+sfc+dash+cend+".png";
  	img5 = DIRIMG+sat+ymd+header+sat+name5+yr+mo+dy+dash+'tbc'+dash+layer+dash+sfc+dash+cend+".png";
  	img6 = DIRIMG+sat+ymd+header+sat+name6+yr+mo+dy+dash+'tb'+dash+layer+dash+sfc+dash+cend+".png";
	img7 = DIRIMG+sat+ymd+header+sat+name7+yr+mo+dy+dash+'tbc'+dash+layer+dash+sfc+dash+cend+".png";
	img8 = DIRIMG+sat+ymd+header+sat+name8+yr+mo+dy+dash+"chisq"+dash+sfc+dash+cend+".png";
	
        document.form.img1.src = img1;
        document.form.img2.src = img2;
        document.form.img3.src = img3;
        document.form.img4.src = img4;
        document.form.img5.src = img5;
        document.form.img6.src = img6;
        document.form.img7.src = img7;
        document.form.img8.src = img8;
	
	var index1 = img1.lastIndexOf("/");
	var alt1   = img1.substring(index1+offset,img1.length);
	
	var index2 = img2.lastIndexOf("/");
	var alt2   = img2.substring(index2+offset,img2.length);
	
	var index3 = img3.lastIndexOf("/");
	var alt3   = img3.substring(index3+offset,img3.length);

	var index4 = img4.lastIndexOf("/");
	var alt4   = img4.substring(index4+offset,img4.length);

	var index5 = img5.lastIndexOf("/");
	var alt5   = img5.substring(index5+offset,img5.length);
	
	var index6 = img6.lastIndexOf("/");
	var alt6   = img6.substring(index6+offset,img6.length);
	
	var index7 = img7.lastIndexOf("/");
	var alt7   = img7.substring(index7+offset,img7.length);
	
	var index8 = img8.lastIndexOf("/");
	var alt8   = img8.substring(index8+offset,img8.length);
	
	document.form.img1.alt=alt1;
	document.form.img2.alt=alt2;
	document.form.img3.alt=alt3;
	document.form.img4.alt=alt4;
	document.form.img5.alt=alt5;
	document.form.img6.alt=alt6;
	document.form.img7.alt=alt7;
	document.form.img8.alt=alt8;
	
	document.form.img1.title=alt1;
	document.form.img2.title=alt2;
	document.form.img3.title=alt3;
	document.form.img4.title=alt4;
	document.form.img5.title=alt5;
	document.form.img6.title=alt6;
	document.form.img7.title=alt7;
	document.form.img8.title=alt8;
	
	document.getElementById("href1").href=img1;
	document.getElementById("href2").href=img2;
	document.getElementById("href3").href=img3;
	document.getElementById("href4").href=img4;
	document.getElementById("href5").href=img5;
	document.getElementById("href6").href=img6;
	document.getElementById("href7").href=img7;
	document.getElementById("href8").href=img8;
	
}


function loadInitialImagesFromScratch() {
	
	var now = new Date();
	now.setDate(now.getDate()-NDAYBACK) ; // n days ago
	
	var year  = now.getFullYear();
	var month = now.getMonth();  // month starting index is 0
	var day   = now.getDate();
       
	document.form.yr.value = year;
	document.form.mo.selectedIndex = month ;
	document.form.dy.selectedIndex = day - 1;
	
	document.form.ref.selectedIndex = 0;
	document.form.sat.selectedIndex = 0;
	document.form.cend.selectedIndex = 0;
	document.form.sfc.selectedIndex = 0;
	document.form.prod.selectedIndex = 0;
	document.form.layer.selectedIndex = 0;
	
	loadImage();
}


function loadInitialImages(ref,sat,prod,layer,cend,sfc,yr,mo,dy) {
	
	if ( sat == "" ) {
		var now = new Date();
		now.setDate(now.getDate()-NDAYBACK) ; // n days ago

		var year  = now.getFullYear();
		var month = now.getMonth();  // month starting index is 0
		var day   = now.getDate();

		document.form.yr.value = year;
		document.form.mo.selectedIndex = month ;
		document.form.dy.selectedIndex = day - 1;
		document.form.ref.selectedIndex = 0;
		document.form.sat.selectedIndex = 0;
		document.form.prod.selectedIndex = 0;
		document.form.layer.selectedIndex = 0;
		document.form.cend.selectedIndex = 0;
		document.form.sfc.selectedIndex = 0;

		loadImage();
	}
	else {
	
	    	loadLayers( sat );

		document.form.yr.value = yr;
		document.form.mo.value = mo ;
		document.form.dy.value = dy;
		document.form.ref.value=ref;
		document.form.sat.value=sat;
		document.form.prod.value=prod;
		document.form.layer.value=layer;
		document.form.cend.value=cend;
		document.form.sfc.value=sfc;
		
		loadImageHelper(ref,sat,prod,layer,cend,sfc,yr,mo,dy);
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

        loadImage();
}


function prev( layer_obj ) {

        var layer_index = layer_obj.selectedIndex - 1;
	
	var indexLimit=19;
	if      ( document.form.sat.value == "f16" || document.form.sat.value == "f18" ) indexLimit=23;
	else if ( document.form.sat.value == "aqua" ) indexLimit=11;
	else if ( document.form.sat.value == "npp"  ) indexLimit=21;
	else if ( document.form.sat.value == "trmm" ) indexLimit=8;
	else if ( document.form.sat.value == "gcomw1" ) indexLimit=11;
	
	if ( layer_index < 0 ) 
		layer_obj.selectedIndex = indexLimit;
	else
		layer_obj.selectedIndex = layer_index;
		
        loadImage();
}


function next( layer_obj ) {

        var layer_index = layer_obj.selectedIndex + 1;
	var indexLimit=19;
	if      ( document.form.sat.value == "f16" || document.form.sat.value == "f18" ) indexLimit=23;
	else if ( document.form.sat.value == "aqua" ) indexLimit=11;
	else if ( document.form.sat.value == "npp"  ) indexLimit=21;
	else if ( document.form.sat.value == "trmm" ) indexLimit=8;
	else if ( document.form.sat.value == "mtma" ) indexLimit=8;
	else if ( document.form.sat.value == "mtsa" ) indexLimit=5;
	else if ( document.form.sat.value == "gcomw1" ) indexLimit=11;
	
	if ( layer_index > indexLimit ) 
		layer_obj.selectedIndex = 0;
	else 
		layer_obj.selectedIndex = layer_index;
		
        loadImage();
}
