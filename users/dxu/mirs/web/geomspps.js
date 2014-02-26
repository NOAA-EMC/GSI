var imgDir="images/";
var header="mirs_adv_poes_";
var sensor="_amsuamhs";
var region="_glb_";
var refId="_mspps";
var ndayback = 3;

// define a satellite object with name and product list
function satellite_object(name, products)
{
  this.name = name;
  this.products = products;
}

var satellite_n18    = new satellite_object("n18",    "clw em(23v:31v:50v) iwp lwp rr sice swe tskin");
var satellite_n19    = new satellite_object("n19",    "clw em(23v:31v:50v) iwp lwp rr sice swe tskin");
var satellite_metopA = new satellite_object("metopA", "clw em(23v:31v:50v) iwp lwp rr sice swe tskin");
var satellite_metopB = new satellite_object("metopB", "clw em(23v:31v:50v) iwp lwp rr sice swe tskin");

var satellite_list = new Array( satellite_n18, satellite_n19, satellite_metopA, satellite_metopB ); 


function getText( val ) {

	var txt="";

	if            ( val == "clw"   ) txt = "CLW"                   ;
	else if       ( val == "em"    ) txt = "Emissivity"	;
	else if       ( val == "iwp"   ) txt = "Ice Water Path"	;
	else if       ( val == "lwp"   ) txt = "Liquid Water Path"	;
	else if       ( val == "rr"    ) txt = "Rain Rate"	;
	else if       ( val == "sice"  ) txt = "Sea Ice Concentration"	;
	else if       ( val == "swe"   ) txt = "Snow Water Equivalent" ;
	else if       ( val == "tskin" ) txt = "Land Surface Temperature" ;

	return txt;
}


function loadLayerHelp( satval, prodval ) {

	var layer_old = document.form.layer.value; 
	var layer_old_exist  = 0 ;
		
	var layer_obj = document.form.layer; 
	layer_obj.options.length = 0; 
       	
	var layer_obj2 = document.form.layer2; 
	layer_obj2.options.length = 0; 
	
	document.getElementById("layer").className ="optioninvisible";
	document.getElementById("layer2").className="optioninvisible";

	for( isat=0; isat<satellite_list.length; isat++ ) {
	    if( satellite_list[isat].name == satval ) {
		var products = satellite_list[isat].products.split(" ");
		for ( i=0; i<products.length; i++ )  {
		    var left = products[i].indexOf("(");
		    var prod_value = products[i] ;
		    if ( left != -1 ) { prod_value = products[i].substring(0,left);}

		    if ( prod_value == prodval && left > 0 ) {
	   	        document.getElementById("layer").className ="optionvisible";  
		        document.getElementById("layer2").className="optionvisible";

		    	var right = products[i].indexOf(")");
		    	var layer_str = products[i].substring(left+1,right);
		    	var layers = layer_str.split(":");
		    	
		    	for ( j=0; j<layers.length; j++ ) {
			    layer_obj.options[j] = new Option();
			    layer_obj.options[j].value = layers[j];

			    layer_obj2.options[j] = new Option();
			    layer_obj2.options[j].value = layers[j];

		    	    if ( layer_old == layers[j] ) { layer_old_exist=1; }
		    	    if ( prod_value == 'em' ) { 
		    	        layer_obj.options[j].text  = 'ch'+String(j+1)+":"+String(layers[j]);
			        layer_obj2.options[j].text = 'ch'+String(j+1)+":"+String(layers[j]);
			    }
		    	    else { 
		    	        layer_obj.options[j].text  = String(layers[j]);
				layer_obj2.options[j].text = String(layers[j]);
			    }
		    	}	
		    } 
	    	}
	    }
	}
	
	// if layer list has old layer, we use old one; otherwise, use the 1st one in the layer list
	if ( layer_old_exist == 1 ) { 
		layer_obj.value  = layer_old; 
		layer_obj2.value = layer_old;
	}
		
} 


function changeSensor( satval ) 
{ 
	loadLayerHelp( satval, document.form.prod.value );	
} 


function changeProduct( prodval ) 
{
	loadLayerHelp( document.form.sat.value, prodval );	
}


function loadImage() {
    
	var yr = document.form.yr.value;
	var mo = document.form.mo.value;
	var dy = document.form.dy.value;
	
	var cend  =  document.form.cend.value;
	var sfc   =  document.form.sfc.value;
	
	var sat   = document.form.sat.value;
 	var prod  = document.form.prod.value;
	var layer = document.form.layer.value;
	
	// update bottom panel
	document.form.yr2.value = yr;
	document.form.mo2.value = mo;
	document.form.dy2.value = dy;

	document.form.cend2.value = cend;
	document.form.sfc2.value  = sfc;

	document.form.sat2.value   = sat;
	document.form.prod2.value  = prod; 
	document.form.layer2.value = layer;

	loadImageHelp(sat,prod,layer,cend,sfc,yr,mo,dy);
	
}


function loadImage_bottom() {
    
	var yr = document.form.yr2.value;
	var mo = document.form.mo2.value;
	var dy = document.form.dy2.value;
	
	var cend  =  document.form.cend2.value;
	var sfc   =  document.form.sfc2.value;
	
	var sat   = document.form.sat2.value;
 	var prod  = document.form.prod2.value;
	var layer = document.form.layer2.value;
	
	// update upper panel
	document.form.yr.value = yr;
	document.form.mo.value = mo;
	document.form.dy.value = dy;

	document.form.cend.value = cend;
	document.form.sfc.value  = sfc;

	document.form.sat.value   = sat;
	document.form.prod.value  = prod; 
	document.form.layer.value = layer;
	
	loadImageHelp(sat,prod,layer,cend,sfc,yr,mo,dy);
	
}


function loadImageHelp(sat,prod,layer,cend,sfc,yr,mo,dy) {
    
	var ymd =  "/" + yr + "-" + mo + "-" + dy + "/";
	
	var header="mirs_adv_poes_";
	var sensor="_amsuamhs";
	var dash  = "_";

	var name1 = sensor + region ;
	var name2 = refId  + region ;
	var name3 = refId  + "_diff"     + region ;
	var name4 = refId  + "_diffasym" + region ;
	var name5 = refId  + region + "p2p_";
	var name6 = refId  + region + "p2p_";
	
	var img1="";
	var img2="";
	var img3="";
	var img4="";	
	var img5="";	
	var img6="";	
	
	if ( prod == "em" ){
		sfc="lnd";
		img1 = imgDir+sat+ymd+header+sat+name1+yr+mo+dy+dash+prod+dash+layer+dash+sfc+dash+cend+".png";
  		img2 = imgDir+sat+ymd+header+sat+name2+yr+mo+dy+dash+prod+dash+layer+dash+sfc+dash+cend+".png"; 
  		img3 = imgDir+sat+ymd+header+sat+name3+yr+mo+dy+dash+prod+dash+layer+dash+sfc+dash+cend+".png";
  		img4 = imgDir+sat+ymd+header+sat+name4+yr+mo+dy+dash+prod+dash+layer+dash+sfc+dash+cend+".png";
  		img5 = imgDir+sat+ymd+header+sat+name5+yr+mo+dy+dash+prod+dash+layer+dash+sfc+dash+cend+".png";
  		img6 = imgDir+sat+ymd+header+sat+name6+yr+mo+dy+dash+prod+dash+layer+dash+'snw'+dash+cend+".png";
	}
	else if ( prod == "tskin" ){
		sfc="lnd";
  		img1 = imgDir+sat+ymd+header+sat+name1+yr+mo+dy+dash+prod+dash+sfc+dash+cend+".png";
  		img2 = imgDir+sat+ymd+header+sat+name2+yr+mo+dy+dash+prod+dash+sfc+dash+cend+".png"; 
  		img3 = imgDir+sat+ymd+header+sat+name3+yr+mo+dy+dash+prod+dash+sfc+dash+cend+".png";
  		img4 = imgDir+sat+ymd+header+sat+name4+yr+mo+dy+dash+prod+dash+sfc+dash+cend+".png";
  		img5 = imgDir+sat+ymd+header+sat+name5+yr+mo+dy+dash+prod+dash+sfc+dash+cend+".png";
		img6 = imgDir+sat+ymd+header+sat+name6+yr+mo+dy+dash+prod+dash+'snw'+dash+cend+".png";
	}
	else if ( prod == "swe" ){
		sfc="lnd";
  		img1 = imgDir+sat+ymd+header+sat+name1+yr+mo+dy+dash+prod+"_cyl_"+sfc+dash+cend+".png";
  		img2 = imgDir+sat+ymd+header+sat+name2+yr+mo+dy+dash+prod+dash+sfc+dash+cend+".png"; 
  		img3 = imgDir+sat+ymd+header+sat+name3+yr+mo+dy+dash+prod+dash+sfc+dash+cend+".png";
  		img4 = imgDir+sat+ymd+header+sat+name4+yr+mo+dy+dash+prod+dash+sfc+dash+cend+".png";
  		img5 = imgDir+sat+ymd+header+sat+name5+yr+mo+dy+dash+prod+dash+sfc+dash+cend+".png";
		img6 = imgDir+sat+ymd+header+sat+name6+yr+mo+dy+dash+prod+dash+'snw'+dash+cend+".png";
	}
	else if ( prod == "clw" ){
		sfc="sea";
  		img1 = imgDir+sat+ymd+header+sat+name1+yr+mo+dy+dash+prod+dash+sfc+dash+cend+".png";
  		img2 = imgDir+sat+ymd+header+sat+name2+yr+mo+dy+dash+prod+dash+sfc+dash+cend+".png"; 
  		img3 = imgDir+sat+ymd+header+sat+name3+yr+mo+dy+dash+prod+dash+sfc+dash+cend+".png";
  		img4 = imgDir+sat+ymd+header+sat+name4+yr+mo+dy+dash+prod+dash+sfc+dash+cend+".png";
  		img5 = imgDir+sat+ymd+header+sat+name5+yr+mo+dy+dash+prod+dash+sfc+dash+cend+".png";
		img6 = imgDir+sat+ymd+header+sat+name6+yr+mo+dy+dash+"clw2"+dash+sfc+dash+cend+".png";
	}
	else if ( prod == "lwp"){
		sfc="sea";
  		img1 = imgDir+sat+ymd+header+sat+name1+yr+mo+dy+dash+prod+dash+sfc+dash+cend+".png";
  		img2 = imgDir+sat+ymd+header+sat+name2+yr+mo+dy+dash+"clw"+dash+sfc+dash+cend+".png"; 
  		img3 = imgDir+sat+ymd+header+sat+name3+yr+mo+dy+dash+prod+dash+sfc+dash+cend+".png";
  		img4 = imgDir+sat+ymd+header+sat+name4+yr+mo+dy+dash+prod+dash+sfc+dash+cend+".png";
  		img5 = imgDir+sat+ymd+header+sat+name5+yr+mo+dy+dash+prod+dash+sfc+dash+cend+".png";
		img6 = imgDir+sat+ymd+header+sat+name6+yr+mo+dy+dash+"lwp2"+dash+sfc+dash+cend+".png";;
	}
	else if ( prod == "iwp" ){
		sfc="sea";
  		img1 = imgDir+sat+ymd+header+sat+name1+yr+mo+dy+dash+prod+dash+sfc+dash+cend+".png";
  		img2 = imgDir+sat+ymd+header+sat+name2+yr+mo+dy+dash+prod+dash+sfc+dash+cend+".png"; 
  		img3 = imgDir+sat+ymd+header+sat+name3+yr+mo+dy+dash+prod+dash+sfc+dash+cend+".png";
  		img4 = imgDir+sat+ymd+header+sat+name4+yr+mo+dy+dash+prod+dash+sfc+dash+cend+".png";
  		img5 = imgDir+sat+ymd+header+sat+name5+yr+mo+dy+dash+prod+dash+sfc+dash+cend+".png";
		img6 = "";
	}
	else if ( prod == "sice" ){
		sfc="sea";
  		img1 = imgDir+sat+ymd+header+sat+name1+yr+mo+dy+dash+prod+"_cyl_"+sfc+dash+cend+".png";
  		img2 = imgDir+sat+ymd+header+sat+name2+yr+mo+dy+dash+prod+dash+sfc+dash+cend+".png"; 
  		img3 = imgDir+sat+ymd+header+sat+name3+yr+mo+dy+dash+prod+dash+sfc+dash+cend+".png";
  		img4 = imgDir+sat+ymd+header+sat+name4+yr+mo+dy+dash+prod+dash+sfc+dash+cend+".png";
  		img5 = imgDir+sat+ymd+header+sat+name5+yr+mo+dy+dash+prod+dash+sfc+dash+cend+".png";
		img6 = imgDir+sat+ymd+header+sat+name6+yr+mo+dy+dash+prod+dash+"ice"+dash+cend+".png";
	}
	else if (prod == "rr"){
  		img1 = imgDir+sat+ymd+header+sat+name1+yr+mo+dy+dash+prod+dash+sfc+dash+cend+".png";
  		img2 = imgDir+sat+ymd+header+sat+name2+yr+mo+dy+dash+prod+dash+sfc+dash+cend+".png"; 
  		img3 = imgDir+sat+ymd+header+sat+name3+yr+mo+dy+dash+prod+dash+sfc+dash+cend+".png";
  		img4 = imgDir+sat+ymd+header+sat+name4+yr+mo+dy+dash+prod+dash+sfc+dash+cend+".png";
  		img5 = imgDir+sat+ymd+header+sat+name5+yr+mo+dy+dash+prod+dash+sfc+dash+cend+".png";
		img6 = imgDir+sat+ymd+header+sat+name6+yr+mo+dy+dash+"rr2"+dash+sfc+dash+cend+".png";;
	}
	else {
  		img1 = imgDir+sat+ymd+header+sat+name1+yr+mo+dy+dash+prod+dash+sfc+dash+cend+".png";
  		img2 = imgDir+sat+ymd+header+sat+name2+yr+mo+dy+dash+prod+dash+sfc+dash+cend+".png"; 
  		img3 = imgDir+sat+ymd+header+sat+name3+yr+mo+dy+dash+prod+dash+sfc+dash+cend+".png";
  		img4 = imgDir+sat+ymd+header+sat+name4+yr+mo+dy+dash+prod+dash+sfc+dash+cend+".png";
  		img5 = imgDir+sat+ymd+header+sat+name5+yr+mo+dy+dash+prod+dash+sfc+dash+cend+".png";
		if ( sfc == 'lnd' ) {
		    img6 = imgDir+sat+ymd+header+sat+name6+yr+mo+dy+dash+prod+dash+'snw'+dash+cend+".png";
		}
		else if ( sfc == 'sea' ) {
		    img6 = imgDir+sat+ymd+header+sat+name6+yr+mo+dy+dash+prod+dash+'ice'+dash+cend+".png";
		}
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


function loadInitialImages(sat,prod,layer,cend,sfc,yr,mo,dy) {
	
	if ( sat == "" ) {
		var now = new Date();
		now.setDate(now.getDate()-ndayback) ;

		var year  = now.getFullYear();
		var month = now.getMonth();  // month starting index is 0
		var day   = now.getDate();

		document.form.yr.value = year;
		document.form.mo.selectedIndex = month ;
		document.form.dy.selectedIndex = day - 1;
		document.form.sat.selectedIndex=0;
		document.form.prod.selectedIndex=0;
		document.form.layer.selectedIndex=0;
		document.form.cend.selectedIndex=0;
		document.form.sfc.selectedIndex=0;

		document.form.yr2.value = year;
		document.form.mo2.selectedIndex = month ;
		document.form.dy2.selectedIndex = day - 1;
		document.form.sat2.selectedIndex=0;
		document.form.prod2.selectedIndex=0;
		document.form.layer2.selectedIndex=0;
		document.form.cend2.selectedIndex=0;
		document.form.sfc2.selectedIndex=0;

		loadImage();
	}
	else {
		
		loadLayerHelp(sat, prod);
		
		document.form.yr.value = yr;
		document.form.mo.value = mo ;
		document.form.dy.value = dy;
		document.form.sat.value=sat;
		document.form.prod.value=prod;
		document.form.layer.value=layer;
		document.form.cend.value=cend;
		document.form.sfc.value=sfc;
		
		document.form.yr2.value = yr;
		document.form.mo2.value = mo ;
		document.form.dy2.value = dy;
		document.form.sat2.value=sat;
		document.form.prod2.value=prod;
		document.form.layer2.value=layer;
		document.form.cend2.value=cend;
		document.form.sfc2.value=sfc;
		
		loadImageHelp(sat,prod,layer,cend,sfc,yr,mo,dy);
	}
	
}


function loadInitialImagesV(sat,prod,layer,cend,sfc,yr,mo,dy) {
	
	if ( sat == "" ) {
		var now = new Date();
		now.setDate(now.getDate()-ndayback) ;

		var year  = now.getFullYear();
		var month = now.getMonth();  // month starting index is 0
		var day   = now.getDate();

		document.form.yr.value = year;
		document.form.mo.selectedIndex = month ;
		document.form.dy.selectedIndex = day - 1;
		document.form.sat.selectedIndex=0;
		document.form.prod.selectedIndex=0;
		document.form.layer.selectedIndex=0;
		document.form.cend.selectedIndex=0;
		document.form.sfc.selectedIndex=0;

		document.form.yr2.value = year;
		document.form.mo2.selectedIndex = month ;
		document.form.dy2.selectedIndex = day - 1;
		document.form.sat2.selectedIndex=0;
		document.form.prod2.selectedIndex=0;
		document.form.layer2.selectedIndex=0;
		document.form.cend2.selectedIndex=0;
		document.form.sfc2.selectedIndex=0;

		loadImage();
	}
	else {
		
		loadLayerHelp(sat, prod);
		
		document.form.yr.value = yr;
		document.form.mo.value = mo ;
		document.form.dy.value = dy;
		document.form.sat.value=sat;
		document.form.prod.value=prod;
		document.form.layer.value=layer;
		document.form.cend.value=cend;
		document.form.sfc.value=sfc;
		
		document.form.yr2.value = yr;
		document.form.mo2.value = mo ;
		document.form.dy2.value = dy;
		document.form.sat2.value=sat;
		document.form.prod2.value=prod;
		document.form.layer2.value=layer;
		document.form.cend2.value=cend;
		document.form.sfc2.value=sfc;
		
		loadImageHelp(sat,prod,layer,cend,sfc,yr,mo,dy);
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

	document.forms.form.yr.value = year ;
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

        document.form.yr2.value = year ;
        document.form.mo2.selectedIndex = month ;
        document.form.dy2.selectedIndex = day - 1;

        loadImage();
}
