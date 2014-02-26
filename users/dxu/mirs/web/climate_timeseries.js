var IMGDIR = 'images/' ;
var IMGTAG = '_timeseries_' ;

var sat   = 'aggr';
var sfc   = 'GLOBE';
var prod  = 'tskin';
var layer = '23v';
var angle = '0-20';
var cend  = 'as';
var dash  = '_';
var slash = '/';

// temp and wv layers
var LAYERS = new Array('100mb', '200mb', '300mb', '400mb', '500mb', '600mb',  '700mb', '800mb', '850mb', '900mb', '950mb');

var CHANNELS_STR =  new Array();
CHANNELS_STR['n18']    = "23v:31v:50v:52v:53h:54h:54v:55h:57h1:57h2:57h3:57h4:57h5:57h6:89v1:89v2:157h:184h:186h:190v";
CHANNELS_STR['n19']    = "23v:31v:50v:52v:53h:54h:54v:55h:57h1:57h2:57h3:57h4:57h5:57h6:89v1:89v2:157h:184h:186h:190v";
CHANNELS_STR['metopA'] = "23v:31v:50v:52v:53h:54h:54v:55h:57h1:57h2:57h3:57h4:57h5:57h6:89v1:89v2:157h:184h:186h:190v";
CHANNELS_STR['metopB'] = "23v:31v:50v:52v:53h:54h:54v:55h:57h1:57h2:57h3:57h4:57h5:57h6:89v1:89v2:157h:184h:186h:190v";
CHANNELS_STR['f16']    = "50v:52v:53v:54v:55v:57rc:59rc:150h:190h:186h:184h:19h:19v:22v:37h:37v:91v:91h:63rc:60rc1:60rc2:60rc3:60rc4:60rc5";
CHANNELS_STR['f18']    = "50h:52h:53h:54h:55h:57rc:59rc:150h:190h:186h:184h:19h:19v:22v:37h:37v:91v:91h:63rc:60rc1:60rc2:60rc3:60rc4:60rc5";
CHANNELS_STR['npp']    = "23v:31v:50h:51h:52h:53h:54h1:54h2:55h:57h1:57h2:57h3:57h4:57h5:57h6:88v:165h:183h1:183h2:183h3:183h4:183h5";
CHANNELS_STR['trmm']   = "11v:11h:19v:19h:21v:37v:37h:85v:85h";


var PRODUCTS_AGGR = new Array("chisq","clw","gs","iwp","lwp","psfc","rr","rwp","sice","sicefy","sicemy","swe","temp","tpw","tskin","wv");
var PRODUCTS_ALL  = new Array("em","ym","ymCorr");

function getText( val ) {
 
  var txt="";
  
  if        ( val == "at"       )  txt = "Antenna Temperature" ; 
  else if   ( val == "chisq"    )  txt = "Chi Square" ;         
  else if   ( val == "clw"      )  txt = "CLW" ;     
  else if   ( val == "em"       )  txt = "Emissivity" ; 
  else if   ( val == "gs"       )  txt = "Snow Grain Size" ;  
  else if   ( val == "iwp"      )  txt = "Ice Water Path" ;
  else if   ( val == "lwp"      )  txt = "Liquid Water Path" ;
  else if   ( val == "nattempt" )  txt = "Attempt Number" ;
  else if   ( val == "niter"    )  txt = "Iteration Number" ;
  else if   ( val == "psfc"     )  txt = "Surface Pressure" ;	 
  else if   ( val == "qc"       )  txt = "QC Flag" ;	
  else if   ( val == "rr"       )  txt = "Rain Rate" ;   
  else if   ( val == "rrday"    )  txt = "Precip Estimate" ;
  else if   ( val == "rrlat"    )  txt = "Precip Lat Distribution" ;
  else if   ( val == "rwp"      )  txt = "Rain Water Path" ;	 
  else if   ( val == "seaicvr"  )  txt = "Sea Ice Cover" ;
  else if   ( val == "sfcTyp"   )  txt = "Pre-Classif Surface Type" ;
  else if   ( val == "sfcTyp2"  )  txt = "Post-Process Surface Type" ;
  else if   ( val == "sice"     )  txt = "Sea Ice Concentration" ;
  else if   ( val == "sicefy"   )  txt = "First Year SIC" ;
  else if   ( val == "sicemy"   )  txt = "Multiple Year SIC" ;
  else if   ( val == "snow"     )  txt = "Snow Cover" ;
  else if   ( val == "swe"      )  txt = "Snow Water Equivalent" ;	 
  else if   ( val == "swp"      )  txt = "Snow Water Path" ;	 
  else if   ( val == "windsp"   )  txt = "Surface Wind Speed" ;
  else if   ( val == "ymCorr"   )  txt = "Corr. TB" ;
  else if   ( val == "ym"       )  txt = "UnCorr TB" ;
  else if   ( val == "temp"     )  txt = "Temperature Profile" ; 
  else if   ( val == "tpw"      )  txt = "TPW" ;
  else if   ( val == "ts"       )  txt = "Surface Temperature" ;
  else if   ( val == "tskin"    )  txt = "Skin Temperature" ;    
  else if   ( val == "wet"      )  txt = "Wetness Index" ; 
  else if   ( val == "wv"       )  txt = "Water Vapor Profile" ; 

  return txt;

}

function changeSensor( satval ) {
	sat = satval;
	
	var prod_old = document.form.prod1.value;
	var prod_old_exist = 0;
	
	var layer_old = document.form.layer1.value;
	var layer_old_exist = 0;
	
	document.getElementById('layer1').className="optioninvisible"; 
	document.getElementById('angle1').className="optioninvisible";

	if( satval == "aggr" ) {
	    document.form.prod1.options.length  = 0;
	    for ( i=0; i<PRODUCTS_AGGR.length; i++ )  {
	    	document.form.prod1.options[i] = new Option();
	    	document.form.prod1.options[i].value = PRODUCTS_AGGR[i];
	    	document.form.prod1.options[i].text  = getText(PRODUCTS_AGGR[i]);
		if(PRODUCTS_AGGR[i] == prod_old) prod_old_exist = 1;
	    }
	}
	else {
	    document.form.prod1.options.length  = 0;
	    for ( i=0; i<PRODUCTS_ALL.length; i++ )  {
	    	document.form.prod1.options[i] = new Option();
	    	document.form.prod1.options[i].value = PRODUCTS_ALL[i];
	    	document.form.prod1.options[i].text  = getText(PRODUCTS_ALL[i]);
		if(PRODUCTS_ALL[i] == prod_old) prod_old_exist = 1;
	    }
	}

	if( prod_old_exist == 1 ) { prod = prod_old; document.form.prod1.value = prod; }
	else			  { prod = document.form.prod1.value; }
	
	
	if( prod == "em" || prod == "ym" || prod == 'ymCorr' ) {
	    document.form.layer1.options.length  = 0;
	    document.form.layer1.title = "select a channel";
	    var chans = CHANNELS_STR[sat].split(":");
	    for ( i=0; i<chans.length; i++ )  {
	    	document.form.layer1.options[i] = new Option();
	    	document.form.layer1.options[i].value = chans[i];
	    	document.form.layer1.options[i].text  = chans[i];
		if(chans[i] == layer_old) layer_old_exist = 1;
	    }
	    
	    document.getElementById('layer1').className="optionvisible"; 
	    if(layer_old_exist == 1 ) {  layer = layer_old; document.form.layer1.value = layer; }
	    else		      {  layer = document.form.layer1.value; }
	    
	    if(prod == "em")  
	    	document.getElementById('angle1').className="optionvisible";
	    else
	    	document.getElementById('angle1').className="optioninvisible";
	}
	else if( prod == "temp" || prod == "wv" ) {
	    //document.getElementById('angle1').className="optioninvisible";
	    document.form.layer1.options.length  = 0;
	    document.form.layer1.title = "select a pressure layer";
	    for ( i=0; i<LAYERS.length; i++ )  {
	    	document.form.layer1.options[i] = new Option();
	    	document.form.layer1.options[i].value = LAYERS[i];
	    	document.form.layer1.options[i].text  = LAYERS[i];
		if(LAYERS[i] == layer_old) layer_old_exist = 1;
	    }
	    document.getElementById('layer1').className="optionvisible"; 
	    if(layer_old_exist == 1 ) {  layer = layer_old; document.form.layer1.value = layer; }
	    else		      {  layer = document.form.layer1.value; }

	}
	
	
	loadImage();
}

function changeProduct( prodval ) {
	prod = prodval;
	
	if( prodval == "em" ) {
	    document.getElementById('layer1').className="optionvisible"; 
	    document.getElementById('angle1').className="optionvisible";

	    document.form.layer1.options.length  = 0;
	    var chans = CHANNELS_STR[sat].split(":");
	    for ( i=0; i<chans.length; i++ )  {
	    	document.form.layer1.options[i] = new Option();
	    	document.form.layer1.options[i].value = chans[i];
	    	document.form.layer1.options[i].text  = chans[i];
	    }
	    
	    layer = document.form.layer1.value;
	    document.form.layer1.title = "select a channel";
	    //document.form.angle1.title = "select local zenith angle range";
	}
	else if( prodval == "ym" || prodval == 'ymCorr' ) {
	    document.getElementById('layer1').className="optionvisible"; 
	    document.getElementById('angle1').className="optioninvisible";
	   
	    document.form.layer1.options.length  = 0;
	    var chans = CHANNELS_STR[sat].split(":");
	    for ( i=0; i<chans.length; i++ )  {
	    	document.form.layer1.options[i] = new Option();
	    	document.form.layer1.options[i].value = chans[i];
	    	document.form.layer1.options[i].text  = chans[i];
	    }
	    
	    layer = document.form.layer1.value;
	    document.form.layer1.title = "select a channel";
	}
	else if( prodval == 'temp' || prodval == 'wv'  ) {
	    document.getElementById('layer1').className="optionvisible"; 
	    document.getElementById('angle1').className="optioninvisible";
	    document.form.layer1.options.length  = 0;
	    for ( i=0; i<LAYERS.length; i++ )  {
	    	document.form.layer1.options[i] = new Option();
	    	document.form.layer1.options[i].value = LAYERS[i];
	    	document.form.layer1.options[i].text  = LAYERS[i];
	    }
	    
	    layer = document.form.layer1.value;
	    document.form.layer1.title = "select a pressure layer";
	}
	else {
	    document.getElementById('layer1').className="optioninvisible"; 
	    document.getElementById('angle1').className="optioninvisible"; 
	}	
	
	loadImage();
}

function changeLayer( layerval ) {
	layer = layerval;
	
	loadImage();
}

function changeAngle( angleval ) {
	angle = angleval;
	loadImage();
}

function changeCend( cendval ) {
	cend = cendval;
	loadImage();
}

function changeSfc( sfcval ) {
	sfc = sfcval;
	
	if( sat != 'aggr' && prod == 'em' )
		document.getElementById('angle1').className="optionvisible";  
	else
		document.getElementById('angle1').className="optioninvisible";  
	
	loadImage();
}


function loadImage() {
     	
	var img = '';
	if( prod == 'em' ) {
 		img = IMGDIR + sat + '/timeseries/' + sat + dash + prod + dash + layer + dash + sfc + dash + angle + IMGTAG + cend + '.png';
		if( sfc == "glb" ) 
			img = IMGDIR + sat + '/timeseries/' + sat + dash + prod + dash + layer + dash + sfc + IMGTAG + cend + '.png';
	}
	else if( prod == 'ym' || prod == 'ymCorr' || prod == 'temp' || prod == 'wv' ) {
 		img = IMGDIR + sat + '/timeseries/' + sat + dash + prod + dash + layer + dash + sfc + IMGTAG + cend + '.png';
	}
	else {
 		img = IMGDIR + sat + '/timeseries/' + sat + dash + prod + dash + sfc + IMGTAG + cend + '.png';
	}
	
	document.form.img1.src = img;
	document.form.img1.alt = img;
	document.getElementById("href1").href = img;
}
