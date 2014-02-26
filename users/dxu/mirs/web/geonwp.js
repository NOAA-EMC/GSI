var imgDirs = new Array();
imgDirs["n18"]    = "images/";
imgDirs["n19"]    = "images/";
imgDirs["metopA"] = "images/";
imgDirs["metopB"] = "images/";
imgDirs["f16"]    = "images/";
imgDirs["f18"]    = "images/";
imgDirs["trmm"]   = "images/";
imgDirs["npp"]    = "images/";
imgDirs["mtma"]   = "images/";
imgDirs["mtsa"]   = "images/";
imgDirs["gcomw1"] = "images/";

//var imgDir = "images/";

var region = "glb_";
var ndayback = 2;
var dash = "_";

var IS_MTMA_ON=0;
var IS_MTSA_ON=0;

var NDAY_MTMA = 3
var NDAY_MTSA = 3;

var YEAR_DEFAULT="";
var MONTH_DEFAULT="";
var DAY_DEFAULT="";



// 04/30/2012
var DATE_COND = new Date(2012, 03, 30);
var SECOND_COND = DATE_COND.getTime()/1000;


/*
var ref = "gdas"
var sat = "n18";
var prod = "";
var layer = "";
var cond = "";
*/

var sats = new Array( 'n18', 'n19', 'metopA', 'metopB', 'f16', 'f18', 'trmm', 'npp', 'gcomw1' );

var headers = new Array();
headers["n18"]    = "mirs_adv_poes_n18_";
headers["n19"]    = "mirs_adv_poes_n19_";
headers["metopA"] = "mirs_adv_poes_metopA_";
headers["metopB"] = "mirs_adv_poes_metopB_";
headers["f16"]    = "mirs_adv_dmsp_f16_"; 
headers["f18"]    = "mirs_adv_dmsp_f18_"; 
headers["trmm"]   = "mirs_adv_eos_trmm_";
headers["npp"]    = "mirs_adv_npoess_npp_";
headers["mtma"]   = "mirs_adv_mt_mtma_";
headers["mtsa"]   = "mirs_adv_mt_mtsa_";
headers["gcomw1"] = "mirs_adv_eos_gcomw1_";

var sensors = new Array();
sensors["n18"]    = "amsuamhs_";
sensors["n19"]    = "amsuamhs_";
sensors["metopA"] = "amsuamhs_";
sensors["metopB"] = "amsuamhs_";
sensors["f16"]    = "ssmis_"; 
sensors["f18"]    = "ssmis_"; 
sensors["trmm"]   = "tmi_";
sensors["npp"]    = "atms_";
sensors["gcomw1"] = "amsr2_";

sensors["mtma"]   = "madras_";
sensors["mtsa"]   = "saphir_";

var conds_val = new Array('allcond_', 'clear_', 'cloudy_', 'rainy_' );
var conds_txt = new Array('All Cond', 'Clear',  'Cloudy',  'Rainy'  );

var cond2s_val = new Array('allcond_',   'clrcld_',    'rainy_' );
var cond2s_txt = new Array('All Cond', 'Clear+Cloudy', 'Rainy' );

var PRODS = new Array();

PRODS["gdas_n18"]     = "em(23v:31v:50v:52v:53h:54h:54v:55h:57h1:57h2:57h3:57h4:57h5:57h6:89v1:89v2:157h:184h:186h:190v) temp(100mb:200mb:300mb:400mb:500mb:600mb:700mb:800mb:850mb:900mb:950mb) tpw psfc tskin wv(200mb:300mb:400mb:500mb:600mb:700mb:800mb:850mb:900mb:950mb)";
PRODS["gdas_n19"]     = "em(23v:31v:50v:52v:53h:54h:54v:55h:57h1:57h2:57h3:57h4:57h5:57h6:89v1:89v2:157h:184h:186h:190v) temp(100mb:200mb:300mb:400mb:500mb:600mb:700mb:800mb:850mb:900mb:950mb) tpw psfc tskin wv(200mb:300mb:400mb:500mb:600mb:700mb:800mb:850mb:900mb:950mb)";
PRODS["gdas_metopA"]  = "em(23v:31v:50v:52v:53h:54h:54v:55h:57h1:57h2:57h3:57h4:57h5:57h6:89v1:89v2:157h:184h:186h:190v) temp(100mb:200mb:300mb:400mb:500mb:600mb:700mb:800mb:850mb:900mb:950mb) tpw psfc tskin wv(200mb:300mb:400mb:500mb:600mb:700mb:800mb:850mb:900mb:950mb)";
PRODS["gdas_metopB"]  = "em(23v:31v:50v:52v:53h:54h:54v:55h:57h1:57h2:57h3:57h4:57h5:57h6:89v1:89v2:157h:184h:186h:190v) temp(100mb:200mb:300mb:400mb:500mb:600mb:700mb:800mb:850mb:900mb:950mb) tpw psfc tskin wv(200mb:300mb:400mb:500mb:600mb:700mb:800mb:850mb:900mb:950mb)";
PRODS["gdas_f16"]     = "em(50v:52v:53v:54v:55v:57rc:59rc:150h:190h:186h:184h:19h:19v:22v:37h:37v:91v:91h:63rc:60rc1:60rc2:60rc3:60rc4:60rc5) temp(100mb:200mb:300mb:400mb:500mb:600mb:700mb:800mb:850mb:900mb:950mb) tpw psfc tskin wspd wv(200mb:300mb:400mb:500mb:600mb:700mb:800mb:850mb:900mb:950mb)";
PRODS["gdas_f18"]     = "em(50h:52h:53h:54h:55h:57rc:59rc:150h:190h:186h:184h:19h:19v:22v:37h:37v:91v:91h:63rc:60rc1:60rc2:60rc3:60rc4:60rc5) temp(100mb:200mb:300mb:400mb:500mb:600mb:700mb:800mb:850mb:900mb:950mb) tpw psfc tskin wspd wv(200mb:300mb:400mb:500mb:600mb:700mb:800mb:850mb:900mb:950mb)";
PRODS["gdas_trmm"]    = "em(11v:11h:19v:19h:21v:37v:37h:85v:85h) temp(100mb:200mb:300mb:400mb:500mb:600mb:700mb:800mb:850mb:900mb:950mb) tpw psfc tskin wspd wv(100mb:200mb:300mb:400mb:500mb:600mb:700mb:800mb:850mb:900mb:950mb)";
PRODS["gdas_npp"]     = "em(23v:31v:50h:51h:52h:53h:54h1:54h2:55h:57h1:57h2:57h3:57h4:57h5:57h6:88v:165h:183h1:183h2:183h3:183h4:183h5) temp(100mb:200mb:300mb:400mb:500mb:600mb:700mb:800mb:850mb:900mb:950mb) tpw psfc tskin wv(100mb:200mb:300mb:400mb:500mb:600mb:700mb:800mb:850mb:900mb:950mb)";
PRODS["gdas_aqua"]    = "em(7v:7h:11v:11h:19v:19h:24v:24h:37v:37h:89v:89h) temp(100mb:200mb:300mb:400mb:500mb:600mb:700mb:800mb:850mb:900mb:950mb) tpw psfc tskin wv(200mb:300mb:400mb:500mb:600mb:700mb:800mb:850mb:900mb:950mb)";
PRODS["gdas_mtma"]    = "em(19v:19h:24v:37v:37h:89v:89h:157v:157h) temp(100mb:200mb:300mb:400mb:500mb:600mb:700mb:800mb:850mb:900mb:950mb) tpw psfc tskin wspd wv(100mb:200mb:300mb:400mb:500mb:600mb:700mb:800mb:850mb:900mb:950mb)";
PRODS["gdas_mtsa"]    = "em(183h:184h:186h:187h:190h:194h) temp(100mb:200mb:300mb:400mb:500mb:600mb:700mb:800mb:850mb:900mb:950mb) tpw psfc tskin wv(100mb:200mb:300mb:400mb:500mb:600mb:700mb:800mb:850mb:900mb:950mb)";
PRODS["gdas_gcomw1"]  = "em(6v:6h:10v:10h:18v:18h:23v:23h:36v:36h:89v:89h) temp(100mb:200mb:300mb:400mb:500mb:600mb:700mb:800mb:850mb:900mb:950mb) tpw psfc tskin wv(200mb:300mb:400mb:500mb:600mb:700mb:800mb:850mb:900mb:950mb)";

PRODS["ecmwf_n18"]    = "clw em(23v:31v:50v:52v:53h:54h:54v:55h:57h1:57h2:57h3:57h4:57h5:57h6:89v1:89v2:157h:184h:186h:190v) iwp lwp temp(100mb:200mb:300mb:400mb:500mb:600mb:700mb:800mb:850mb:900mb:950mb) tpw tskin psfc wv(200mb:300mb:400mb:500mb:600mb:700mb:800mb:850mb:900mb:950mb)";
PRODS["ecmwf_n19"]    = "clw em(23v:31v:50v:52v:53h:54h:54v:55h:57h1:57h2:57h3:57h4:57h5:57h6:89v1:89v2:157h:184h:186h:190v) iwp lwp temp(100mb:200mb:300mb:400mb:500mb:600mb:700mb:800mb:850mb:900mb:950mb) tpw tskin psfc wv(200mb:300mb:400mb:500mb:600mb:700mb:800mb:850mb:900mb:950mb)";
PRODS["ecmwf_metopA"] = "clw em(23v:31v:50v:52v:53h:54h:54v:55h:57h1:57h2:57h3:57h4:57h5:57h6:89v1:89v2:157h:184h:186h:190v) iwp lwp temp(100mb:200mb:300mb:400mb:500mb:600mb:700mb:800mb:850mb:900mb:950mb) tpw tskin psfc wv(200mb:300mb:400mb:500mb:600mb:700mb:800mb:850mb:900mb:950mb)";
PRODS["ecmwf_metopB"] = "clw em(23v:31v:50v:52v:53h:54h:54v:55h:57h1:57h2:57h3:57h4:57h5:57h6:89v1:89v2:157h:184h:186h:190v) iwp lwp temp(100mb:200mb:300mb:400mb:500mb:600mb:700mb:800mb:850mb:900mb:950mb) tpw tskin psfc wv(200mb:300mb:400mb:500mb:600mb:700mb:800mb:850mb:900mb:950mb)";
PRODS["ecmwf_f16"]    = "clw em(50v:52v:53v:54v:55v:57rc:59rc:150h:190h:186h:184h:19h:19v:22v:37h:37v:91v:91h:63rc:60rc1:60rc2:60rc3:60rc4:60rc5) iwp lwp temp(100mb:200mb:300mb:400mb:500mb:600mb:700mb:800mb:850mb:900mb:950mb) tpw tskin psfc wspd wv(200mb:300mb:400mb:500mb:600mb:700mb:800mb:850mb:900mb:950mb)";
PRODS["ecmwf_f18"]    = "clw em(50h:52h:53h:54h:55h:57rc:59rc:150h:190h:186h:184h:19h:19v:22v:37h:37v:91v:91h:63rc:60rc1:60rc2:60rc3:60rc4:60rc5) iwp lwp temp(100mb:200mb:300mb:400mb:500mb:600mb:700mb:800mb:850mb:900mb:950mb) tpw tskin psfc wspd wv(200mb:300mb:400mb:500mb:600mb:700mb:800mb:850mb:900mb:950mb)";
PRODS["ecmwf_trmm"]   = "em(11v:11h:19v:19h:21v:37v:37h:85v:85h) temp(100mb:200mb:300mb:400mb:500mb:600mb:700mb:800mb:850mb:900mb:950mb) tpw tskin psfc wspd wv(200mb:300mb:400mb:500mb:600mb:700mb:800mb:850mb:900mb:950mb)";
PRODS["ecmwf_npp"]    = "clw em(23v:31v:50h:51h:52h:53h:54h1:54h2:55h:57h1:57h2:57h3:57h4:57h5:57h6:88v:165h:183h1:183h2:183h3:183h4:183h5) iwp lwp temp(100mb:200mb:300mb:400mb:500mb:600mb:700mb:800mb:850mb:900mb:950mb) tpw tskin psfc wv(200mb:300mb:400mb:500mb:600mb:700mb:800mb:850mb:900mb:950mb)";
PRODS["ecmwf_aqua"]   = "em(7v:7h:11v:11h:19v:19h:24v:24h:37v:37h:89v:89h) iwp lwp temp(100mb:200mb:300mb:400mb:500mb:600mb:700mb:800mb:850mb:900mb:950mb) tpw tskin psfc wv(200mb:300mb:400mb:500mb:600mb:700mb:800mb:850mb:900mb:950mb)";
PRODS["ecmwf_mtma"]   = "clw em(19v:19h:24v:37v:37h:89v:89h:157v:157h) temp(100mb:200mb:300mb:400mb:500mb:600mb:700mb:800mb:850mb:900mb:950mb) tpw tskin psfc wspd wv(200mb:300mb:400mb:500mb:600mb:700mb:800mb:850mb:900mb:950mb)";
PRODS["ecmwf_mtsa"]   = "clw em(183h:184h:186h:187h:190h:194h) temp(100mb:200mb:300mb:400mb:500mb:600mb:700mb:800mb:850mb:900mb:950mb) tpw tskin psfc wv(200mb:300mb:400mb:500mb:600mb:700mb:800mb:850mb:900mb:950mb)";
PRODS["ecmwf_gcomw1"] = "em(6v:6h:10v:10h:18v:18h:23v:23h:36v:36h:89v:89h) iwp lwp temp(100mb:200mb:300mb:400mb:500mb:600mb:700mb:800mb:850mb:900mb:950mb) tpw tskin psfc wv(200mb:300mb:400mb:500mb:600mb:700mb:800mb:850mb:900mb:950mb)";

PRODS["gfs_n18"]      = "clw em(23v:31v:50v:52v:53h:54h:54v:55h:57h1:57h2:57h3:57h4:57h5:57h6:89v1:89v2:157h:184h:186h:190v) rr temp(200mb:300mb:400mb:500mb:600mb:700mb:800mb:850mb:900mb:950mb) tpw psfc tskin wv(200mb:300mb:400mb:500mb:600mb:700mb:800mb:850mb:900mb:950mb)";
PRODS["gfs_n19"]      = "clw em(23v:31v:50v:52v:53h:54h:54v:55h:57h1:57h2:57h3:57h4:57h5:57h6:89v1:89v2:157h:184h:186h:190v) rr temp(200mb:300mb:400mb:500mb:600mb:700mb:800mb:850mb:900mb:950mb) tpw psfc tskin wv(200mb:300mb:400mb:500mb:600mb:700mb:800mb:850mb:900mb:950mb)";
PRODS["gfs_metopA"]   = "clw em(23v:31v:50v:52v:53h:54h:54v:55h:57h1:57h2:57h3:57h4:57h5:57h6:89v1:89v2:157h:184h:186h:190v) rr temp(200mb:300mb:400mb:500mb:600mb:700mb:800mb:850mb:900mb:950mb) tpw psfc tskin wv(200mb:300mb:400mb:500mb:600mb:700mb:800mb:850mb:900mb:950mb)";
PRODS["gfs_metopB"]   = "clw em(23v:31v:50v:52v:53h:54h:54v:55h:57h1:57h2:57h3:57h4:57h5:57h6:89v1:89v2:157h:184h:186h:190v) rr temp(200mb:300mb:400mb:500mb:600mb:700mb:800mb:850mb:900mb:950mb) tpw psfc tskin wv(200mb:300mb:400mb:500mb:600mb:700mb:800mb:850mb:900mb:950mb)";
PRODS["gfs_f16"]      = "clw em(50v:52v:53v:54v:55v:57rc:59rc:150h:190h:186h:184h:19h:19v:22v:37h:37v:91v:91h:63rc:60rc1:60rc2:60rc3:60rc4:60rc5) rr temp(200mb:300mb:400mb:500mb:600mb:700mb:800mb:850mb:900mb:950mb) tpw psfc tskin wspd wv(200mb:300mb:400mb:500mb:600mb:700mb:800mb:850mb:900mb:950mb)";
PRODS["gfs_f18"]      = "clw em(50h:52h:53h:54h:55h:57rc:59rc:150h:190h:186h:184h:19h:19v:22v:37h:37v:91v:91h:63rc:60rc1:60rc2:60rc3:60rc4:60rc5) rr temp(200mb:300mb:400mb:500mb:600mb:700mb:800mb:850mb:900mb:950mb) tpw psfc tskin wspd wv(200mb:300mb:400mb:500mb:600mb:700mb:800mb:850mb:900mb:950mb)";
PRODS["gfs_trmm"]     = "clw em(11v:11h:19v:19h:21v:37v:37h:85v:85h) rr temp(200mb:300mb:400mb:500mb:600mb:700mb:800mb:850mb:900mb:950mb) tpw psfc tskin wspd wv(200mb:300mb:400mb:500mb:600mb:700mb:800mb:850mb:900mb:950mb)";
PRODS["gfs_npp"]      = "clw em(23v:31v:50h:51h:52h:53h:54h1:54h2:55h:57h1:57h2:57h3:57h4:57h5:57h6:88v:165h:183h1:183h2:183h3:183h4:183h5) rr temp(200mb:300mb:400mb:500mb:600mb:700mb:800mb:850mb:900mb:950mb) tpw psfc tskin wv(200mb:300mb:400mb:500mb:600mb:700mb:800mb:850mb:900mb:950mb)";
PRODS["gfs_aqua"]     = "clw em(7v:7h:11v:11h:19v:19h:24v:24h:37v:37h:89v:89h) rr temp(200mb:300mb:400mb:500mb:600mb:700mb:800mb:850mb:900mb:950mb) tpw psfc tskin wv(200mb:300mb:400mb:500mb:600mb:700mb:800mb:850mb:900mb:950mb)";
PRODS["gfs_mtma"]     = "clw em(19v:19h:24v:37v:37h:89v:89h:157v:157h) rr temp(200mb:300mb:400mb:500mb:600mb:700mb:800mb:850mb:900mb:950mb) tpw psfc tskin wspd wv(200mb:300mb:400mb:500mb:600mb:700mb:800mb:850mb:900mb:950mb)";
PRODS["gfs_mtsa"]     = "clw em(183h:184h:186h:187h:190h:194h) rr temp(200mb:300mb:400mb:500mb:600mb:700mb:800mb:850mb:900mb:950mb) tpw psfc tskin wv(200mb:300mb:400mb:500mb:600mb:700mb:800mb:850mb:900mb:950mb)";




function getTxt( val ) {

        var txt = "";

        if            ( val == "tpw"   ) txt = "TPW"                    ;
        else if       ( val == "em"    ) txt = "Emissivity"             ;
        else if       ( val == "temp"  ) txt = "Temp. Profile"          ;
        else if       ( val == "wv"    ) txt = "Water Vapor Profile"    ;
        else if       ( val == "tskin" ) txt = "Skin Temp."             ;
        else if       ( val == "wspd"  ) txt = "Wind Speed"             ;
        else if       ( val == "swe"   ) txt = "Snow Water Equivalent"  ;
        else if       ( val == "iwp"   ) txt = "Ice Water Path"         ;
        else if       ( val == "lwp"   ) txt = "Liquid Water Path"      ;
        else if       ( val == "clw"   ) txt = "Cloud Liquid Water"     ;
        else if       ( val == "rr"    ) txt = "Rain Rate"              ;
        else if       ( val == "psfc"  ) txt = "Sfc Pressure"           ;

        return txt;
}



function changeRef( refval ) 
{ 
	sat = document.form.sat.value;
	ref = refval;
	
	prod = document.form.prod.value;
	layer = document.form.layer.value;
	cond = document.form.cond.value;
	
	var prod_old_exist = 0;
	var layer_old_exist = 0;
	var cond_old_exist = 0;
	
	////////////////////////////////////////////////////////////////////////////////////////////
	// product list update
	////////////////////////////////////////////////////////////////////////////////////////////
        
	document.form.prod.options.length = 0; 
	document.form.prod2.options.length = 0; 
	
	var ref_sat = ref + '_' + sat;
	var prods = PRODS[ref_sat].split(" ");
	
	for ( i=0; i<prods.length; i++ )  {
	    var left = prods[i].indexOf("(");
	    var prod_cur = prods[i] ;
	    if ( left != -1 ) {
	        prod_cur = prods[i].substring(0,left);
	    }
	    if ( prod_cur == prod ) {
	        prod_old_exist = 1;
	    }
	    var prod_text = getTxt(prod_cur);
	    document.form.prod.options[i]       = new Option();
	    document.form.prod.options[i].value = prod_cur;
	    document.form.prod.options[i].text  = prod_text;
	    
	    document.form.prod2.options[i]       = new Option();
	    document.form.prod2.options[i].value = prod_cur;
	    document.form.prod2.options[i].text  = prod_text;
	   
	}
		
	// if populated product list has old product value, we use old one; otherwise, use the 1st one in the list
	
	if ( prod_old_exist == 1 ) {
	    document.form.prod.value = prod;
	    document.form.prod2.value = prod;
	}
	else {
	    prod = document.form.prod.options[0].value;
	}


	////////////////////////////////////////////////////////////////////////////////////////////
	// layer and cond list update
	////////////////////////////////////////////////////////////////////////////////////////////
         
	document.form.layer.options.length = 0; 
	document.form.layer2.options.length = 0; 

	document.form.cond.options.length = 0; 
	document.form.cond2.options.length = 0; 

	document.getElementById("layer").className ="optioninvisible";
	document.getElementById("layer2").className="optioninvisible";
	
	document.getElementById("cond").className ="optioninvisible";
	document.getElementById("cond2").className="optioninvisible";


	for ( i=0; i<prods.length; i++ )  {
	    var left = prods[i].indexOf("(");
	    var prod_cur = prods[i] ;
	    if ( left != -1 ) { 
	        prod_cur = prods[i].substring(0,left);
	    }

	    if ( prod_cur == prod && left > 0 ) {  // start more than one level ( channel/layer ) 
	        document.getElementById("layer").className ="optionvisible";  
	        document.getElementById("layer2").className="optionvisible";

	    	var right = prods[i].indexOf(")");
	    	var layer_str = prods[i].substring(left+1,right);
	    	var layers = layer_str.split(":");
	    	
	    	for ( j=0; j<layers.length; j++ ) {
		    document.form.layer.options[j] = new Option();
		    document.form.layer.options[j].value = layers[j];

		    document.form.layer2.options[j] = new Option();
		    document.form.layer2.options[j].value = layers[j];

	    	    if ( layer == layers[j] ) { 
		        layer_old_exist = 1;
		    }
	    	    if ( prod_cur == 'em' ) { 
	    	        document.form.layer.options[j].text  = 'ch'+String(j+1)+":"+String(layers[j]);
		        document.form.layer2.options[j].text = 'ch'+String(j+1)+":"+String(layers[j]);
			document.form.layer.title  = "select a channel";
			document.form.layer2.title = "select a channel";
		    }
	    	    else { 
	    	        document.form.layer.options[j].text  = String(layers[j]);
			document.form.layer2.options[j].text = String(layers[j]);
			document.form.layer.title  = "select a layer";
			document.form.layer2.title = "select a layer";
		    }
	    	}
		
		if( prod_cur == 'temp' || prod_cur == 'wv' || prod_cur == 'em' ) { 
		
		    document.getElementById("cond").className ="optionvisible";  
	            document.getElementById("cond2").className="optionvisible";
		    
	    	    for ( j=0; j<conds_val.length; j++ ) {
		        document.form.cond.options[j] = new Option();
		        document.form.cond.options[j].value = conds_val[j];
	    	        document.form.cond.options[j].text  = conds_txt[j];

		        document.form.cond2.options[j] = new Option();
		        document.form.cond2.options[j].value = conds_val[j];
			document.form.cond2.options[j].text  = conds_txt[j];

	    	        if ( cond == conds_val[j] ) { 
			    cond_old_exist = 1; 
			}
		    
	    	    }
		}
	    }  // end more than one level ( channel/layer )

	    if ( prod_cur == prod && left < 0 && prod_cur == 'wspd' ) {  // wind speed
	    
		    document.getElementById("cond").className ="optionvisible";  
	            document.getElementById("cond2").className="optionvisible";
		    
	    	    for ( j=0; j<cond2s_val.length; j++ ) {
		        document.form.cond.options[j] = new Option();
		        document.form.cond.options[j].value = cond2s_val[j];
	    	        document.form.cond.options[j].text  = cond2s_txt[j];

		        document.form.cond2.options[j] = new Option();
		        document.form.cond2.options[j].value = cond2s_val[j];
			document.form.cond2.options[j].text  = cond2s_txt[j];

	    	        if ( cond == cond2s_val[j] ) { 
			    cond_old_exist = 1; 
			}
		    
	    	    }
	    }

	    if ( prod_cur == prod && left < 0 && ( prod_cur == 'tpw' || prod_cur == 'tskin' || prod_cur == 'psfc' ) ) {  // tpw / tskin / psfc
	    
		    document.getElementById("cond").className ="optionvisible";  
	            document.getElementById("cond2").className="optionvisible";
		    
	    	    for ( j=0; j<conds_val.length; j++ ) {
		        document.form.cond.options[j] = new Option();
		        document.form.cond.options[j].value = conds_val[j];
	    	        document.form.cond.options[j].text  = conds_txt[j];

		        document.form.cond2.options[j] = new Option();
		        document.form.cond2.options[j].value = conds_val[j];
			document.form.cond2.options[j].text  = conds_txt[j];

	    	        if ( cond == conds_val[j] ) { 
			    cond_old_exist = 1; 
			}
		    
	    	    }

	    }


	}

	// if layer list has old layer, we use old one; otherwise, use the 1st one in the layer list
	if ( layer_old_exist == 1 ) { 
		document.form.layer.value  = layer; 
		document.form.layer2.value = layer;
	}
	else if( document.getElementById("layer").className == "optionvisible" ) {
		layer = document.form.layer.options[0].value;
	}
	
	// if cond list has old cond, we use old one; otherwise, use the 1st one in the cond list	
	if ( cond_old_exist == 1 ) { 
		document.form.cond.value  = cond; 
		document.form.cond2.value = cond;
	}
	else if( document.getElementById("cond").className == "optionvisible" ) {
		cond = document.form.cond.options[0].value;
	}

} 



function changeSat( satval )
{ 
	ref = document.form.ref.value;
	sat = satval;
	
	prod = document.form.prod.value;
	layer = document.form.layer.value;
	cond = document.form.cond.value;

	document.form.sat.value = satval;
	document.form.sat2.value = satval;
	
	var prod_old_exist = 0;
	var layer_old_exist = 0;
	var cond_old_exist = 0;
	
	////////////////////////////////////////////////////////////////////////////////////////////
	// product list update
	////////////////////////////////////////////////////////////////////////////////////////////
        
	document.form.prod.options.length = 0; 
	document.form.prod2.options.length = 0; 
	
	var ref_sat = ref + '_' + sat;
	var prods = PRODS[ref_sat].split(" ");
	
	for ( i=0; i<prods.length; i++ )  {
	    var left = prods[i].indexOf("(");
	    var prod_cur = prods[i] ;
	    if ( left != -1 ) {
	        prod_cur = prods[i].substring(0,left);
	    }
	    if ( prod_cur == prod ) {
	        prod_old_exist = 1;
	    }
	    var prod_text = getTxt(prod_cur);
	    document.form.prod.options[i]       = new Option();
	    document.form.prod.options[i].value = prod_cur;
	    document.form.prod.options[i].text  = prod_text;
	    
	    document.form.prod2.options[i]       = new Option();
	    document.form.prod2.options[i].value = prod_cur;
	    document.form.prod2.options[i].text  = prod_text;
	   
	}
		
	// if populated product list has old product value, we use old one; otherwise, use the 1st one in the list
	
	if ( prod_old_exist == 1 ) {
	    document.form.prod.value = prod;
	    document.form.prod2.value = prod;
	}
	else {
	    prod = document.form.prod.options[0].value;
	}


	////////////////////////////////////////////////////////////////////////////////////////////
	// layer and cond list update
	////////////////////////////////////////////////////////////////////////////////////////////
         
	document.form.layer.options.length = 0; 
	document.form.layer2.options.length = 0; 

	document.form.cond.options.length = 0; 
	document.form.cond2.options.length = 0; 

	document.getElementById("layer").className ="optioninvisible";
	document.getElementById("layer2").className="optioninvisible";
	
	document.getElementById("cond").className ="optioninvisible";
	document.getElementById("cond2").className="optioninvisible";


	for ( i=0; i<prods.length; i++ )  {
	    var left = prods[i].indexOf("(");
	    var prod_cur = prods[i] ;
	    if ( left != -1 ) { 
	        prod_cur = prods[i].substring(0,left);
	    }

	    if ( prod_cur == prod && left > 0 ) {  // start more than one level ( channel/layer ) 
	        document.getElementById("layer").className ="optionvisible";  
	        document.getElementById("layer2").className="optionvisible";

	    	var right = prods[i].indexOf(")");
	    	var layer_str = prods[i].substring(left+1,right);
	    	var layers = layer_str.split(":");
	    	
	    	for ( j=0; j<layers.length; j++ ) {
		    document.form.layer.options[j] = new Option();
		    document.form.layer.options[j].value = layers[j];

		    document.form.layer2.options[j] = new Option();
		    document.form.layer2.options[j].value = layers[j];

	    	    if ( layer == layers[j] ) { 
		        layer_old_exist = 1;
		    }
	    	    if ( prod_cur == 'em' ) { 
	    	        document.form.layer.options[j].text  = 'ch'+String(j+1)+":"+String(layers[j]);
		        document.form.layer2.options[j].text = 'ch'+String(j+1)+":"+String(layers[j]);
			document.form.layer.title  = "select a channel";
			document.form.layer2.title = "select a channel";
		    }
	    	    else { 
	    	        document.form.layer.options[j].text  = String(layers[j]);
			document.form.layer2.options[j].text = String(layers[j]);
			document.form.layer.title  = "select a layer";
			document.form.layer2.title = "select a layer";
		    }
	    	}
		
		if( prod_cur == 'temp' || prod_cur == 'wv' || prod_cur == 'em' ) { 
		
		    document.getElementById("cond").className ="optionvisible";  
	            document.getElementById("cond2").className="optionvisible";
		    
	    	    for ( j=0; j<conds_val.length; j++ ) {
		        document.form.cond.options[j] = new Option();
		        document.form.cond.options[j].value = conds_val[j];
	    	        document.form.cond.options[j].text  = conds_txt[j];

		        document.form.cond2.options[j] = new Option();
		        document.form.cond2.options[j].value = conds_val[j];
			document.form.cond2.options[j].text  = conds_txt[j];

	    	        if ( cond == conds_val[j] ) { 
			    cond_old_exist = 1; 
			}
	    	    }
		}
	    }  // end more than one level ( channel/layer )

	    if ( prod_cur == prod && left < 0 && prod_cur == 'wspd' ) {  // wind speed
	    
		    document.getElementById("cond").className ="optionvisible";  
	            document.getElementById("cond2").className="optionvisible";
		    
	    	    for ( j=0; j<cond2s_val.length; j++ ) {
		        document.form.cond.options[j] = new Option();
		        document.form.cond.options[j].value = cond2s_val[j];
	    	        document.form.cond.options[j].text  = cond2s_txt[j];

		        document.form.cond2.options[j] = new Option();
		        document.form.cond2.options[j].value = cond2s_val[j];
			document.form.cond2.options[j].text  = cond2s_txt[j];

	    	        if ( cond == cond2s_val[j] ) { 
			    cond_old_exist = 1; 
			}
		    
	    	    }
	    }

	    if ( prod_cur == prod && left < 0 && ( prod_cur == 'tpw' || prod_cur == 'tskin' || prod_cur == 'psfc' ) ) {  // tpw / tskin / psfc
	    
		    document.getElementById("cond").className ="optionvisible";  
	            document.getElementById("cond2").className="optionvisible";
		    
	    	    for ( j=0; j<conds_val.length; j++ ) {
		        document.form.cond.options[j] = new Option();
		        document.form.cond.options[j].value = conds_val[j];
	    	        document.form.cond.options[j].text  = conds_txt[j];

		        document.form.cond2.options[j] = new Option();
		        document.form.cond2.options[j].value = conds_val[j];
			document.form.cond2.options[j].text  = conds_txt[j];

	    	        if ( cond == conds_val[j] ) { 
			    cond_old_exist = 1; 
			}
	    	    }

	    }

	}

	// if layer list has old layer, we use old one; otherwise, use the 1st one in the layer list
	if ( layer_old_exist == 1 ) { 
		document.form.layer.value  = layer; 
		document.form.layer2.value = layer;
	}
	else if( document.getElementById("layer").className == "optionvisible" ) {
		layer = document.form.layer.options[0].value;
	}
	
	// if cond list has old cond, we use old one; otherwise, use the 1st one in the cond list	
	if ( cond_old_exist == 1 ) { 
		document.form.cond.value  = cond; 
		document.form.cond2.value = cond;
	}
	else if( document.getElementById("cond").className == "optionvisible" )  {
		cond = document.form.cond.options[0].value;
	}
	
	
	year_cur = document.form.yr.value;
	month_cur = document.form.mo.value;
	day_cur = document.form.dy.value;
	
	
	if( satval == 'mtma' && IS_MTMA_ON == 0 && year_cur == YEAR_DEFAULT && month_cur == MONTH_DEFAULT && day_cur == DAY_DEFAULT ) {
		shift( -NDAY_MTMA );
		IS_MTMA_ON = 1;
	}
	
	if( satval == 'mtsa' && IS_MTSA_ON == 0 && year_cur == YEAR_DEFAULT && month_cur == MONTH_DEFAULT && day_cur == DAY_DEFAULT ) {
		shift( -NDAY_MTSA );
		IS_MTSA_ON = 1;
	}
	
} 



function changeProd( prodval )
{
	ref = document.form.ref.value;
	sat = document.form.sat.value;
	
	layer = document.form.layer.value;
	cond = document.form.cond.value;

	prod = prodval;
	document.form.prod.value = prodval;
	document.form.prod2.value = prodval;
	
	var ref_sat = ref + '_' + sat;
	var prods = PRODS[ref_sat].split(" ");
	
	var layer_old_exist = 0;
	var cond_old_exist = 0;

	
	////////////////////////////////////////////////////////////////////////////////////////////
	// layer and cond list update
	////////////////////////////////////////////////////////////////////////////////////////////
         
	document.form.layer.options.length = 0; 
	document.form.layer2.options.length = 0; 

	document.form.cond.options.length = 0; 
	document.form.cond2.options.length = 0; 

	document.getElementById("layer").className ="optioninvisible";
	document.getElementById("layer2").className="optioninvisible";
	
	document.getElementById("cond").className ="optioninvisible";
	document.getElementById("cond2").className="optioninvisible";


	for ( i=0; i<prods.length; i++ )  {  // start i loop over prods
	    var left = prods[i].indexOf("(");
	    var prod_cur = prods[i] ;
	    if ( left != -1 ) { 
	        prod_cur = prods[i].substring(0,left);
	    }

	    if ( prod_cur == prod && left > 0 ) {  // start more than one level ( channel/layer ) 
	        document.getElementById("layer").className ="optionvisible";  
	        document.getElementById("layer2").className="optionvisible";

	    	var right = prods[i].indexOf(")");
	    	var layer_str = prods[i].substring(left+1,right);
	    	var layers = layer_str.split(":");
	    	
	    	for ( j=0; j<layers.length; j++ ) {
		    document.form.layer.options[j] = new Option();
		    document.form.layer.options[j].value = layers[j];

		    document.form.layer2.options[j] = new Option();
		    document.form.layer2.options[j].value = layers[j];

	    	    if ( layer == layers[j] ) { 
		        layer_old_exist = 1;
		    }
	    	    if ( prod_cur == 'em' ) { 
	    	        document.form.layer.options[j].text  = 'ch'+String(j+1)+":"+String(layers[j]);
		        document.form.layer2.options[j].text = 'ch'+String(j+1)+":"+String(layers[j]);
			document.form.layer.title  = "select a channel";
			document.form.layer2.title = "select a channel";
		    }
	    	    else { 
	    	        document.form.layer.options[j].text  = String(layers[j]);
			document.form.layer2.options[j].text = String(layers[j]);
			document.form.layer.title  = "select a layer";
			document.form.layer2.title = "select a layer";
		    }
	    	}
		
		if( prod_cur == 'temp' || prod_cur == 'wv' || prod_cur == 'em' ) { 
		
		    document.getElementById("cond").className ="optionvisible";  
	            document.getElementById("cond2").className="optionvisible";
		    
	    	    for ( j=0; j<conds_val.length; j++ ) {
		        document.form.cond.options[j] = new Option();
		        document.form.cond.options[j].value = conds_val[j];
	    	        document.form.cond.options[j].text  = conds_txt[j];

		        document.form.cond2.options[j] = new Option();
		        document.form.cond2.options[j].value = conds_val[j];
			document.form.cond2.options[j].text  = conds_txt[j];

	    	        if ( cond == conds_val[j] ) { 
			    cond_old_exist = 1; 
			}
	    	    }
		}
	    }  // end more than one level ( channel/layer )

	    if ( prod_cur == prod && left < 0 && prod_cur == 'wspd' ) {  // wind speed
	    
		    document.getElementById("cond").className ="optionvisible";  
	            document.getElementById("cond2").className="optionvisible";
		    
	    	    for ( j=0; j<cond2s_val.length; j++ ) {
		        document.form.cond.options[j] = new Option();
		        document.form.cond.options[j].value = cond2s_val[j];
	    	        document.form.cond.options[j].text  = cond2s_txt[j];

		        document.form.cond2.options[j] = new Option();
		        document.form.cond2.options[j].value = cond2s_val[j];
			document.form.cond2.options[j].text  = cond2s_txt[j];

	    	        if ( cond == cond2s_val[j] ) { 
			    cond_old_exist = 1; 
			}
		    
	    	    }
	    }

	    if ( prod_cur == prod && left < 0 && ( prod_cur == 'tpw' || prod_cur == 'tskin' || prod_cur == 'psfc' ) ) {  // tpw / tskin / psfc
	    
		    document.getElementById("cond").className ="optionvisible";  
	            document.getElementById("cond2").className="optionvisible";
		    
	    	    for ( j=0; j<conds_val.length; j++ ) {
		        document.form.cond.options[j] = new Option();
		        document.form.cond.options[j].value = conds_val[j];
	    	        document.form.cond.options[j].text  = conds_txt[j];

		        document.form.cond2.options[j] = new Option();
		        document.form.cond2.options[j].value = conds_val[j];
			document.form.cond2.options[j].text  = conds_txt[j];

	    	        if ( cond == conds_val[j] ) { 
			    cond_old_exist = 1; 
			}
	    	    }
	    }

	} // end i loop over prods


	// if layer list has old layer, we use old one; otherwise, use the 1st one in the layer list
	if ( layer_old_exist == 1 ) { 
		document.form.layer.value  = layer; 
		document.form.layer2.value = layer;
	}
	else if ( document.getElementById("layer").className == "optionvisible" ) {
		layer = document.form.layer.options[0].value;
	}
	
	// if cond list has old cond, we use old one; otherwise, use the 1st one in the cond list	
	if ( cond_old_exist == 1 ) { 
		document.form.cond.value  = cond; 
		document.form.cond2.value = cond;
	}
	else if ( document.getElementById("cond").className == "optionvisible" ) {
		cond = document.form.cond.options[0].value;
	}
	

	// added 04/30/2010 to handle CLW and LWP, no retrieval over land for this two products
	if( prod == "clw" || prod == "lwp" ) {
		document.form.sfc.options.length = 0;
		
		document.form.sfc.options[0] = new Option();
		document.form.sfc.options[0].value = "sea";
		document.form.sfc.options[0].text  = "Sea";
	
		document.form.sfc.options[1] = new Option();
		document.form.sfc.options[1].value = "all";
		document.form.sfc.options[1].text  = "All";
	}
	else {
		document.form.sfc.options.length = 0;
		
		document.form.sfc.options[0] = new Option();
		document.form.sfc.options[0].value = "sea";
		document.form.sfc.options[0].text  = "Sea";
	
		document.form.sfc.options[1] = new Option();
		document.form.sfc.options[1].value = "lnd";
		document.form.sfc.options[1].text  = "Land";
	
		document.form.sfc.options[2] = new Option();
		document.form.sfc.options[2].value = "all";
		document.form.sfc.options[2].text  = "All";
	}

}



function changeLayer( layerval ) 
{
	layer = layerval;
	
	document.form.layer.value = layerval;	
	document.form.layer2.value = layerval;	
} 



function changeCond( condval ) 
{
	cond = condval;
	
	document.form.cond.value = condval;	
	document.form.cond2.value = condval;	
} 



function changeCend( cendval ) 
{	
	cend = cendval;
	document.form.cend.value = cendval;	
	document.form.cend2.value = cendval;	
} 



function changeSfc( sfcval ) 
{	
	sfc = sfcval;
	document.form.sfc.value = sfcval;	
	document.form.sfc2.value = sfcval;	
} 



function changeYear( yrval ) 
{
	yr = yrval;
	document.form.yr.value = yrval;	
	document.form.yr2.value = yrval;	
} 



function changeMonth( moval ) 
{
	mo = moval;
	document.form.mo.value = moval;	
	document.form.mo2.value = moval;	
} 



function changeDay( dyval ) 
{
	dy = dyval;
	document.form.dy.value = dyval;	
	document.form.dy2.value = dyval;	
} 



function changeSize( product ) {
	
	if ( product == "temp" || product == "wv" ) {
		document.getElementById("box7").style.display  = "table-cell";   
		document.getElementById("box8").style.display  = "table-cell";   
		document.getElementById("box9").style.display  = "table-cell";   
		document.getElementById("boxa").style.display  = "table-cell";
	}
	else {
	      	document.getElementById("box7").style.display  = "none";   
		document.getElementById("box8").style.display  = "none";   
		document.getElementById("box9").style.display  = "none";   
		document.getElementById("boxa").style.display  = "none";
	} 
}



function changeSizeV( product ) {
	
	if ( product == "temp" || product == "wv" ) {
		document.getElementById("box7").style.display  = "table-row";   
		document.getElementById("box8").style.display  = "table-row";   
		document.getElementById("box9").style.display  = "table-row";   
		document.getElementById("boxa").style.display  = "table-row";

	}
	else {
		document.getElementById("box7").style.display  = "none";   
		document.getElementById("box8").style.display  = "none";   
		document.getElementById("box9").style.display  = "none";   
		document.getElementById("boxa").style.display  = "none";
	} 
}



function loadImage() {

	var yr1    = document.form.yr.value;
	var mo1    = document.form.mo.value;
	var dy1    = document.form.dy.value;
	var cend1  = document.form.cend.value;
	var sfc1   = document.form.sfc.value;
	var ref1   = document.form.ref.value;
	var sat1   = document.form.sat.value;
	var prod1  = document.form.prod.value;
	var layer1 = document.form.layer.value;
	var cond1  = document.form.cond.value;
	
	// update bottom panel
	document.form.yr2.value    = yr1;
	document.form.mo2.value    = mo1;
	document.form.dy2.value    = dy1;
	document.form.cend2.value  = cend1;
	document.form.sfc2.value   = sfc1;
	document.form.ref2.value   = ref1;
	document.form.sat2.value   = sat1;
	document.form.prod2.value  = prod1; 
	document.form.layer2.value = layer1;
	document.form.cond2.value  = cond1;

	loadImageHelp(ref1,sat1,prod1,layer1,cond1,cend1,sfc1,yr1,mo1,dy1);
}



function loadImage_bottom() {

	var yr1    = document.form.yr2.value;
	var mo1    = document.form.mo2.value;
	var dy1    = document.form.dy2.value;
	var cend1  = document.form.cend2.value;
	var sfc1   = document.form.sfc2.value;
	var ref1   = document.form.ref2.value;
	var sat1   = document.form.sat2.value;
 	var prod1  = document.form.prod2.value;
	var layer1 = document.form.layer2.value;
	var cond1  = document.form.cond2.value;
	
	// update upper panel
	document.form.yr.value    = yr1;
	document.form.mo.value    = mo1;
	document.form.dy.value    = dy1;
	document.form.cend.value  = cend1;
	document.form.sfc.value   = sfc1;
	document.form.ref.value   = sat1;
	document.form.sat.value   = sat1;
	document.form.prod.value  = prod1; 
	document.form.layer.value = layer1;
	document.form.cond.value  = cond1;
	
	loadImageHelp(ref1,sat1,prod1,layer1,cond1,cend1,sfc1,yr1,mo1,dy1);
}



function loadImageHelp(ref1,sat1,prod1,layer1,cond1,cend1,sfc1,yr1,mo1,dy1) {
    
	var ymd =  "/" + yr1 + "-" + mo1 + "-" + dy1 + "/";
	var nwpId = ref1 + dash ;
	
	var name1 = sensors[sat1] + region ;
	var name2 = nwpId  + region ;
	var name3 = nwpId  + "bias_" + region ;
	var name4 = nwpId  + "asym_" + region ;
	var name5 = nwpId  + region  + "p2p_" ;
	var name6 = nwpId  + region  + "p2p_" ;
	var name7 = nwpId  + region  + "p2p_" ;
	var name8 = nwpId  + region  + "p2p_" ;
	var name9 = nwpId  + region  + "p2p_" ;
	var namea = nwpId  + region  + "p2p_" ;
	
	var img1 = "";
	var img2 = "";
	var img3 = "";
	var img4 = "";	
	var img5 = "";	
	var img6 = "";	
	var img7 = "";	
	var img8 = "";	
	var img9 = "";	
	var imga = "";	

	var DATE1 = new Date(yr1,mo1-1,dy1);
	var SECOND1 = DATE1.getTime()/1000;
	
	var imgDir = imgDirs[sat1];
	
    if( SECOND1 >= SECOND_COND ) { 


	if ( prod1 == "em" ) {
  		img1 = imgDir+sat1+ymd+headers[sat1]+name1+yr1+mo1+dy1+dash+prod1+dash+layer+dash+sfc1+dash+cend1+".png";
  		img2 = imgDir+sat1+ymd+headers[sat1]+name2+yr1+mo1+dy1+dash+prod1+dash+layer+dash+sfc1+dash+cend1+".png"; 
  		
                img3 = imgDir+sat1+ymd+headers[sat1]+name3+yr1+mo1+dy1+dash+prod1+dash+layer+dash+cond1+sfc1+dash+cend1+".png";
  		img4 = imgDir+sat1+ymd+headers[sat1]+name4+yr1+mo1+dy1+dash+prod1+dash+layer+dash+cond1+sfc1+dash+cend1+".png";
  		img5 = imgDir+sat1+ymd+headers[sat1]+name5+yr1+mo1+dy1+dash+prod1+dash+layer+dash+cond1+sfc1+dash+cend1+".png";
		if ( sfc1 == 'lnd' ) {
		    img6 = imgDir+sat1+ymd+headers[sat1]+name6+yr1+mo1+dy1+dash+prod1+dash+layer+dash+cond1+'snw'+dash+cend1+".png";
		}
		else if ( sfc1 == 'sea' ) {
		    img6 = imgDir+sat1+ymd+headers[sat1]+name6+yr1+mo1+dy1+dash+prod1+dash+layer+dash+cond1+'ice'+dash+cend1+".png";
		}
	}
	else if ( prod1 == "temp" || prod1 == "wv" ) {
  		img1 = imgDir+sat1+ymd+headers[sat1]+name1+yr1+mo1+dy1+dash+prod1+dash+layer+dash+sfc1+dash+cend1+".png";
  		img2 = imgDir+sat1+ymd+headers[sat1]+name2+yr1+mo1+dy1+dash+prod1+dash+layer+dash+sfc1+dash+cend1+".png"; 
  		
                img3 = imgDir+sat1+ymd+headers[sat1]+name3+yr1+mo1+dy1+dash+prod1+dash+layer+dash+cond1+sfc1+dash+cend1+".png";
  		img4 = imgDir+sat1+ymd+headers[sat1]+name4+yr1+mo1+dy1+dash+prod1+dash+layer+dash+cond1+sfc1+dash+cend1+".png";
		
  		img5 = imgDir+sat1+ymd+headers[sat1]+name5+yr1+mo1+dy1+dash+prod1+dash+layer+dash+cond1+sfc1+dash+cend1+".png";
		if ( sfc1 == 'lnd' ) {
		    img6 = imgDir+sat1+ymd+headers[sat1]+name6+yr1+mo1+dy1+dash+prod1+dash+layer+dash+cond1+'snw'+dash+cend1+".png";
		}
		else if ( sfc1 == 'sea' ) {
		    img6 = imgDir+sat1+ymd+headers[sat1]+name6+yr1+mo1+dy1+dash+prod1+dash+layer+dash+cond1+'ice'+dash+cend1+".png";
		}
		
		img7 = imgDir+sat1+ymd+headers[sat1]+name7+yr1+mo1+dy1+dash+prod1+"_mean_vert_"+cond1+cend1+".png";
		img8 = imgDir+sat1+ymd+headers[sat1]+name8+yr1+mo1+dy1+dash+prod1+"_stdv_vert_"+cond1+cend1+".png";
		img9 = imgDir+sat1+ymd+headers[sat1]+name9+yr1+mo1+dy1+dash+prod1+"_mean_vert_"+cond1+"ad.png";
		imga = imgDir+sat1+ymd+headers[sat1]+namea+yr1+mo1+dy1+dash+prod1+"_stdv_vert_"+cond1+"ad.png";
	}
	
	else if (prod1 == "clw"){
  		var cond2 = 'allcond_';
                img1 = imgDir+sat1+ymd+headers[sat1]+name1+yr1+mo1+dy1+dash+prod1+dash+sfc1+dash+cend1+".png";
  		img2 = imgDir+sat1+ymd+headers[sat1]+name2+yr1+mo1+dy1+dash+prod1+dash+sfc1+dash+cend1+".png";
  		img3 = imgDir+sat1+ymd+headers[sat1]+name3+yr1+mo1+dy1+dash+prod1+dash+cond2+sfc1+dash+cend1+".png";
  		img4 = imgDir+sat1+ymd+headers[sat1]+name4+yr1+mo1+dy1+dash+prod1+dash+cond2+sfc1+dash+cend1+".png";
  		img5 = imgDir+sat1+ymd+headers[sat1]+name5+yr1+mo1+dy1+dash+prod1+dash+cond2+sfc1+dash+cend1+".png";
		img6 = "";
	}
	else if (prod1 == "lwp"){
  		var cond2 = 'allcond_';
                img1 = imgDir+sat1+ymd+headers[sat1]+name1+yr1+mo1+dy1+dash+prod1+dash+'sea'+dash+cend1+".png";
  		img2 = imgDir+sat1+ymd+headers[sat1]+name2+yr1+mo1+dy1+dash+'clw'+dash+'sea'+dash+cend1+".png"; 
  		img3 = imgDir+sat1+ymd+headers[sat1]+name3+yr1+mo1+dy1+dash+prod1+dash+cond2+'sea'+dash+cend1+".png";
  		img4 = imgDir+sat1+ymd+headers[sat1]+name4+yr1+mo1+dy1+dash+prod1+dash+cond2+'sea'+dash+cend1+".png";
  		img5 = imgDir+sat1+ymd+headers[sat1]+name5+yr1+mo1+dy1+dash+prod1+dash+cond2+'sea'+dash+cend1+".png";
		img6 = ""
	}
	else if (prod1 == "iwp"){
  		var cond2 = 'allcond_';
  		img1 = imgDir+sat1+ymd+headers[sat1]+name1+yr1+mo1+dy1+dash+prod1+dash+sfc1+dash+cend1+".png";
  		img2 = imgDir+sat1+ymd+headers[sat1]+name2+yr1+mo1+dy1+dash+prod1+dash+sfc1+dash+cend1+".png"; 
  		img3 = imgDir+sat1+ymd+headers[sat1]+name3+yr1+mo1+dy1+dash+prod1+dash+cond2+sfc1+dash+cend1+".png";
  		img4 = imgDir+sat1+ymd+headers[sat1]+name4+yr1+mo1+dy1+dash+prod1+dash+cond2+sfc1+dash+cend1+".png";
  		img5 = imgDir+sat1+ymd+headers[sat1]+name5+yr1+mo1+dy1+dash+prod1+dash+cond2+sfc1+dash+cend1+".png";
		img6 = ""
	}
	else if (prod1 == "sice"){
		sfc1="sea";
  		img1 = imgDir+sat1+ymd+headers[sat1]+name1+yr1+mo1+dy1+dash+prod1+dash+sfc1+dash+cend1+".png";
  		img2 = imgDir+sat1+ymd+headers[sat1]+name2+yr1+mo1+dy1+dash+prod1+dash+sfc1+dash+cend1+".png"; 
  		img3 = imgDir+sat1+ymd+headers[sat1]+name3+yr1+mo1+dy1+dash+prod1+dash+sfc1+dash+cend1+".png";
  		img4 = imgDir+sat1+ymd+headers[sat1]+name4+yr1+mo1+dy1+dash+prod1+dash+sfc1+dash+cend1+".png";
  		img5 = imgDir+sat1+ymd+headers[sat1]+name5+yr1+mo1+dy1+dash+prod1+dash+sfc1+dash+cend1+".png";
		img6 = imgDir+sat1+ymd+headers[sat1]+name6+yr1+mo1+dy1+dash+prod1+dash+'ice'+dash+cend1+".png";
	}
	else if (prod1 == "wspd"){
		sfc1="sea";
  		img1 = imgDir+sat1+ymd+headers[sat1]+name1+yr1+mo1+dy1+dash+prod1+dash+sfc1+dash+cend1+".png";
  		img2 = imgDir+sat1+ymd+headers[sat1]+name2+yr1+mo1+dy1+dash+prod1+dash+sfc1+dash+cend1+".png"; 
                img3 = imgDir+sat1+ymd+headers[sat1]+name3+yr1+mo1+dy1+dash+prod1+dash+cond1+sfc1+dash+cend1+".png";
  		img4 = imgDir+sat1+ymd+headers[sat1]+name4+yr1+mo1+dy1+dash+prod1+dash+cond1+sfc1+dash+cend1+".png";
  		img5 = imgDir+sat1+ymd+headers[sat1]+name5+yr1+mo1+dy1+dash+prod1+dash+cond1+sfc1+dash+cend1+".png";
		img6 = "";
	}
	else {
  		img1 = imgDir+sat1+ymd+headers[sat1]+name1+yr1+mo1+dy1+dash+prod1+dash+sfc1+dash+cend1+".png";
  		img2 = imgDir+sat1+ymd+headers[sat1]+name2+yr1+mo1+dy1+dash+prod1+dash+sfc1+dash+cend1+".png"; 
  		img3 = imgDir+sat1+ymd+headers[sat1]+name3+yr1+mo1+dy1+dash+prod1+dash+cond1+sfc1+dash+cend1+".png";
  		img4 = imgDir+sat1+ymd+headers[sat1]+name4+yr1+mo1+dy1+dash+prod1+dash+cond1+sfc1+dash+cend1+".png";
  		img5 = imgDir+sat1+ymd+headers[sat1]+name5+yr1+mo1+dy1+dash+prod1+dash+cond1+sfc1+dash+cend1+".png";
		if ( sfc1 == 'lnd' ) {
		    img6 = imgDir+sat1+ymd+headers[sat1]+name6+yr1+mo1+dy1+dash+prod1+dash+cond1+'snw'+dash+cend1+".png";
		}
		else if ( sfc1 == 'sea' ) {
		    img6 = imgDir+sat1+ymd+headers[sat1]+name6+yr1+mo1+dy1+dash+prod1+dash+cond1+'ice'+dash+cend1+".png";
		}
	}

    }	

    else {
      
      if ( sat1 == 'mtsa' || sat1 == 'mtma' ) {
	if ( prod1 == "em" ) {
  		img1 = imgDir+sat1+ymd+headers[sat1]+name1+yr1+mo1+dy1+dash+prod1+dash+layer+dash+sfc1+dash+cend1+".png";
  		img2 = imgDir+sat1+ymd+headers[sat1]+name2+yr1+mo1+dy1+dash+prod1+dash+layer+dash+sfc1+dash+cend1+".png"; 
  		
                img3 = imgDir+sat1+ymd+headers[sat1]+name3+yr1+mo1+dy1+dash+prod1+dash+layer+dash+cond1+sfc1+dash+cend1+".png";
  		img4 = imgDir+sat1+ymd+headers[sat1]+name4+yr1+mo1+dy1+dash+prod1+dash+layer+dash+cond1+sfc1+dash+cend1+".png";
  		img5 = imgDir+sat1+ymd+headers[sat1]+name5+yr1+mo1+dy1+dash+prod1+dash+layer+dash+cond1+sfc1+dash+cend1+".png";
		if ( sfc1 == 'lnd' ) {
		    img6 = imgDir+sat1+ymd+headers[sat1]+name6+yr1+mo1+dy1+dash+prod1+dash+layer+dash+cond1+'snw'+dash+cend1+".png";
		}
		else if ( sfc1 == 'sea' ) {
		    img6 = imgDir+sat1+ymd+headers[sat1]+name6+yr1+mo1+dy1+dash+prod1+dash+layer+dash+cond1+'ice'+dash+cend1+".png";
		}
	}
	else if ( prod1 == "temp" || prod1 == "wv" ) {
  		img1 = imgDir+sat1+ymd+headers[sat1]+name1+yr1+mo1+dy1+dash+prod1+dash+layer+dash+sfc1+dash+cend1+".png";
  		img2 = imgDir+sat1+ymd+headers[sat1]+name2+yr1+mo1+dy1+dash+prod1+dash+layer+dash+sfc1+dash+cend1+".png"; 
  		
                img3 = imgDir+sat1+ymd+headers[sat1]+name3+yr1+mo1+dy1+dash+prod1+dash+layer+dash+cond1+sfc1+dash+cend1+".png";
  		img4 = imgDir+sat1+ymd+headers[sat1]+name4+yr1+mo1+dy1+dash+prod1+dash+layer+dash+cond1+sfc1+dash+cend1+".png";
		
  		img5 = imgDir+sat1+ymd+headers[sat1]+name5+yr1+mo1+dy1+dash+prod1+dash+layer+dash+cond1+sfc1+dash+cend1+".png";
		if ( sfc1 == 'lnd' ) {
		    img6 = imgDir+sat1+ymd+headers[sat1]+name6+yr1+mo1+dy1+dash+prod1+dash+layer+dash+cond1+'snw'+dash+cend1+".png";
		}
		else if ( sfc1 == 'sea' ) {
		    img6 = imgDir+sat1+ymd+headers[sat1]+name6+yr1+mo1+dy1+dash+prod1+dash+layer+dash+cond1+'ice'+dash+cend1+".png";
		}
		
		img7 = imgDir+sat1+ymd+headers[sat1]+name7+yr1+mo1+dy1+dash+prod1+"_mean_vert_"+cond1+cend1+".png";
		img8 = imgDir+sat1+ymd+headers[sat1]+name8+yr1+mo1+dy1+dash+prod1+"_stdv_vert_"+cond1+cend1+".png";
		img9 = imgDir+sat1+ymd+headers[sat1]+name9+yr1+mo1+dy1+dash+prod1+"_mean_vert_"+cond1+"ad.png";
		imga = imgDir+sat1+ymd+headers[sat1]+namea+yr1+mo1+dy1+dash+prod1+"_stdv_vert_"+cond1+"ad.png";
	}
	
	else if (prod1 == "clw"){
  		var cond2 = 'allcond_';
                img1 = imgDir+sat1+ymd+headers[sat1]+name1+yr1+mo1+dy1+dash+prod1+dash+sfc1+dash+cend1+".png";
  		img2 = imgDir+sat1+ymd+headers[sat1]+name2+yr1+mo1+dy1+dash+prod1+dash+sfc1+dash+cend1+".png";
  		img3 = imgDir+sat1+ymd+headers[sat1]+name3+yr1+mo1+dy1+dash+prod1+dash+cond2+sfc1+dash+cend1+".png";
  		img4 = imgDir+sat1+ymd+headers[sat1]+name4+yr1+mo1+dy1+dash+prod1+dash+cond2+sfc1+dash+cend1+".png";
  		img5 = imgDir+sat1+ymd+headers[sat1]+name5+yr1+mo1+dy1+dash+prod1+dash+cond2+sfc1+dash+cend1+".png";
		img6 = "";
	}
	else if (prod1 == "lwp"){
  		var cond2 = 'allcond_';
                img1 = imgDir+sat1+ymd+headers[sat1]+name1+yr1+mo1+dy1+dash+prod1+dash+'sea'+dash+cend1+".png";
  		img2 = imgDir+sat1+ymd+headers[sat1]+name2+yr1+mo1+dy1+dash+'clw'+dash+'sea'+dash+cend1+".png"; 
  		img3 = imgDir+sat1+ymd+headers[sat1]+name3+yr1+mo1+dy1+dash+prod1+dash+cond2+'sea'+dash+cend1+".png";
  		img4 = imgDir+sat1+ymd+headers[sat1]+name4+yr1+mo1+dy1+dash+prod1+dash+cond2+'sea'+dash+cend1+".png";
  		img5 = imgDir+sat1+ymd+headers[sat1]+name5+yr1+mo1+dy1+dash+prod1+dash+cond2+'sea'+dash+cend1+".png";
		img6 = ""
	}
	else if (prod1 == "iwp"){
  		var cond2 = 'allcond_';
  		img1 = imgDir+sat1+ymd+headers[sat1]+name1+yr1+mo1+dy1+dash+prod1+dash+sfc1+dash+cend1+".png";
  		img2 = imgDir+sat1+ymd+headers[sat1]+name2+yr1+mo1+dy1+dash+prod1+dash+sfc1+dash+cend1+".png"; 
  		img3 = imgDir+sat1+ymd+headers[sat1]+name3+yr1+mo1+dy1+dash+prod1+dash+cond2+sfc1+dash+cend1+".png";
  		img4 = imgDir+sat1+ymd+headers[sat1]+name4+yr1+mo1+dy1+dash+prod1+dash+cond2+sfc1+dash+cend1+".png";
  		img5 = imgDir+sat1+ymd+headers[sat1]+name5+yr1+mo1+dy1+dash+prod1+dash+cond2+sfc1+dash+cend1+".png";
		img6 = ""
	}
	else if (prod1 == "sice"){
		sfc1="sea";
  		img1 = imgDir+sat1+ymd+headers[sat1]+name1+yr1+mo1+dy1+dash+prod1+dash+sfc1+dash+cend1+".png";
  		img2 = imgDir+sat1+ymd+headers[sat1]+name2+yr1+mo1+dy1+dash+prod1+dash+sfc1+dash+cend1+".png"; 
  		img3 = imgDir+sat1+ymd+headers[sat1]+name3+yr1+mo1+dy1+dash+prod1+dash+sfc1+dash+cend1+".png";
  		img4 = imgDir+sat1+ymd+headers[sat1]+name4+yr1+mo1+dy1+dash+prod1+dash+sfc1+dash+cend1+".png";
  		img5 = imgDir+sat1+ymd+headers[sat1]+name5+yr1+mo1+dy1+dash+prod1+dash+sfc1+dash+cend1+".png";
		img6 = imgDir+sat1+ymd+headers[sat1]+name6+yr1+mo1+dy1+dash+prod1+dash+'ice'+dash+cend1+".png";
	}
	else if (prod1 == "wspd"){
		sfc1="sea";
  		img1 = imgDir+sat1+ymd+headers[sat1]+name1+yr1+mo1+dy1+dash+prod1+dash+sfc1+dash+cend1+".png";
  		img2 = imgDir+sat1+ymd+headers[sat1]+name2+yr1+mo1+dy1+dash+prod1+dash+sfc1+dash+cend1+".png"; 
                img3 = imgDir+sat1+ymd+headers[sat1]+name3+yr1+mo1+dy1+dash+prod1+dash+cond1+sfc1+dash+cend1+".png";
  		img4 = imgDir+sat1+ymd+headers[sat1]+name4+yr1+mo1+dy1+dash+prod1+dash+cond1+sfc1+dash+cend1+".png";
  		img5 = imgDir+sat1+ymd+headers[sat1]+name5+yr1+mo1+dy1+dash+prod1+dash+cond1+sfc1+dash+cend1+".png";
		img6 = "";
	}
	else {
  		img1 = imgDir+sat1+ymd+headers[sat1]+name1+yr1+mo1+dy1+dash+prod1+dash+sfc1+dash+cend1+".png";
  		img2 = imgDir+sat1+ymd+headers[sat1]+name2+yr1+mo1+dy1+dash+prod1+dash+sfc1+dash+cend1+".png"; 
  		img3 = imgDir+sat1+ymd+headers[sat1]+name3+yr1+mo1+dy1+dash+prod1+dash+cond1+sfc1+dash+cend1+".png";
  		img4 = imgDir+sat1+ymd+headers[sat1]+name4+yr1+mo1+dy1+dash+prod1+dash+cond1+sfc1+dash+cend1+".png";
  		img5 = imgDir+sat1+ymd+headers[sat1]+name5+yr1+mo1+dy1+dash+prod1+dash+cond1+sfc1+dash+cend1+".png";
		if ( sfc1 == 'lnd' ) {
		    img6 = imgDir+sat1+ymd+headers[sat1]+name6+yr1+mo1+dy1+dash+prod1+dash+cond1+'snw'+dash+cend1+".png";
		}
		else if ( sfc1 == 'sea' ) {
		    img6 = imgDir+sat1+ymd+headers[sat1]+name6+yr1+mo1+dy1+dash+prod1+dash+cond1+'ice'+dash+cend1+".png";
		}
	}
      
      }
      
      else {
	if ( prod1 == "em" ) {
  		img1 = imgDir+sat1+ymd+headers[sat1]+name1+yr1+mo1+dy1+dash+prod1+dash+layer+dash+sfc1+dash+cend1+".png";
  		img2 = imgDir+sat1+ymd+headers[sat1]+name2+yr1+mo1+dy1+dash+prod1+dash+layer+dash+sfc1+dash+cend1+".png"; 
  		
                img3 = imgDir+sat1+ymd+headers[sat1]+name3+yr1+mo1+dy1+dash+prod1+dash+layer+dash+sfc1+dash+cend1+".png";
  		img4 = imgDir+sat1+ymd+headers[sat1]+name4+yr1+mo1+dy1+dash+prod1+dash+layer+dash+sfc1+dash+cend1+".png";
  		img5 = imgDir+sat1+ymd+headers[sat1]+name5+yr1+mo1+dy1+dash+prod1+dash+layer+dash+sfc1+dash+cend1+".png";
		if ( sfc1 == 'lnd' ) {
		    img6 = imgDir+sat1+ymd+headers[sat1]+name6+yr1+mo1+dy1+dash+prod1+dash+layer+dash+'snw'+dash+cend1+".png";
		}
		else if ( sfc1 == 'sea' ) {
		    img6 = imgDir+sat1+ymd+headers[sat1]+name6+yr1+mo1+dy1+dash+prod1+dash+layer+dash+'ice'+dash+cend1+".png";
		}
	}
	else if ( prod1 == "temp" || prod1 == "wv" ) {
  		img1 = imgDir+sat1+ymd+headers[sat1]+name1+yr1+mo1+dy1+dash+prod1+dash+layer+dash+sfc1+dash+cend1+".png";
  		img2 = imgDir+sat1+ymd+headers[sat1]+name2+yr1+mo1+dy1+dash+prod1+dash+layer+dash+sfc1+dash+cend1+".png"; 
  		
                img3 = imgDir+sat1+ymd+headers[sat1]+name3+yr1+mo1+dy1+dash+prod1+dash+layer+dash+sfc1+dash+cend1+".png";
  		img4 = imgDir+sat1+ymd+headers[sat1]+name4+yr1+mo1+dy1+dash+prod1+dash+layer+dash+sfc1+dash+cend1+".png";
		
  		img5 = imgDir+sat1+ymd+headers[sat1]+name5+yr1+mo1+dy1+dash+prod1+dash+layer+dash+sfc1+dash+cend1+".png";
		if ( sfc1 == 'lnd' ) {
		    img6 = imgDir+sat1+ymd+headers[sat1]+name6+yr1+mo1+dy1+dash+prod1+dash+layer+dash+'snw'+dash+cend1+".png";
		}
		else if ( sfc1 == 'sea' ) {
		    img6 = imgDir+sat1+ymd+headers[sat1]+name6+yr1+mo1+dy1+dash+prod1+dash+layer+dash+'ice'+dash+cend1+".png";
		}
		
		img7 = imgDir+sat1+ymd+headers[sat1]+name7+yr1+mo1+dy1+dash+prod1+"_mean_vert_"+cend1+".png";
		img8 = imgDir+sat1+ymd+headers[sat1]+name8+yr1+mo1+dy1+dash+prod1+"_stdv_vert_"+cend1+".png";
		img9 = imgDir+sat1+ymd+headers[sat1]+name9+yr1+mo1+dy1+dash+prod1+"_mean_vert_"+"ad.png";
		imga = imgDir+sat1+ymd+headers[sat1]+namea+yr1+mo1+dy1+dash+prod1+"_stdv_vert_"+"ad.png";
	}
	
	else if (prod1 == "clw"){
                img1 = imgDir+sat1+ymd+headers[sat1]+name1+yr1+mo1+dy1+dash+prod1+dash+sfc1+dash+cend1+".png";
  		img2 = imgDir+sat1+ymd+headers[sat1]+name2+yr1+mo1+dy1+dash+prod1+dash+sfc1+dash+cend1+".png";
  		img3 = imgDir+sat1+ymd+headers[sat1]+name3+yr1+mo1+dy1+dash+prod1+dash+sfc1+dash+cend1+".png";
  		img4 = imgDir+sat1+ymd+headers[sat1]+name4+yr1+mo1+dy1+dash+prod1+dash+sfc1+dash+cend1+".png";
  		img5 = imgDir+sat1+ymd+headers[sat1]+name5+yr1+mo1+dy1+dash+prod1+dash+sfc1+dash+cend1+".png";
		img6 = "";
	}
	else if (prod1 == "lwp"){
                img1 = imgDir+sat1+ymd+headers[sat1]+name1+yr1+mo1+dy1+dash+prod1+dash+'sea'+dash+cend1+".png";
  		img2 = imgDir+sat1+ymd+headers[sat1]+name2+yr1+mo1+dy1+dash+'clw'+dash+'sea'+dash+cend1+".png"; 
  		img3 = imgDir+sat1+ymd+headers[sat1]+name3+yr1+mo1+dy1+dash+prod1+dash+'sea'+dash+cend1+".png";
  		img4 = imgDir+sat1+ymd+headers[sat1]+name4+yr1+mo1+dy1+dash+prod1+dash+'sea'+dash+cend1+".png";
  		img5 = imgDir+sat1+ymd+headers[sat1]+name5+yr1+mo1+dy1+dash+prod1+dash+'sea'+dash+cend1+".png";
		img6 = ""
	}
	else if (prod1 == "iwp"){
  		img1 = imgDir+sat1+ymd+headers[sat1]+name1+yr1+mo1+dy1+dash+prod1+dash+sfc1+dash+cend1+".png";
  		img2 = imgDir+sat1+ymd+headers[sat1]+name2+yr1+mo1+dy1+dash+prod1+dash+sfc1+dash+cend1+".png"; 
  		img3 = imgDir+sat1+ymd+headers[sat1]+name3+yr1+mo1+dy1+dash+prod1+dash+sfc1+dash+cend1+".png";
  		img4 = imgDir+sat1+ymd+headers[sat1]+name4+yr1+mo1+dy1+dash+prod1+dash+sfc1+dash+cend1+".png";
  		img5 = imgDir+sat1+ymd+headers[sat1]+name5+yr1+mo1+dy1+dash+prod1+dash+sfc1+dash+cend1+".png";
		img6 = ""
	}
	else if (prod1 == "sice"){
		sfc1="sea";
  		img1 = imgDir+sat1+ymd+headers[sat1]+name1+yr1+mo1+dy1+dash+prod1+dash+sfc1+dash+cend1+".png";
  		img2 = imgDir+sat1+ymd+headers[sat1]+name2+yr1+mo1+dy1+dash+prod1+dash+sfc1+dash+cend1+".png"; 
  		img3 = imgDir+sat1+ymd+headers[sat1]+name3+yr1+mo1+dy1+dash+prod1+dash+sfc1+dash+cend1+".png";
  		img4 = imgDir+sat1+ymd+headers[sat1]+name4+yr1+mo1+dy1+dash+prod1+dash+sfc1+dash+cend1+".png";
  		img5 = imgDir+sat1+ymd+headers[sat1]+name5+yr1+mo1+dy1+dash+prod1+dash+sfc1+dash+cend1+".png";
		img6 = imgDir+sat1+ymd+headers[sat1]+name6+yr1+mo1+dy1+dash+prod1+dash+'ice'+dash+cend1+".png";
	}
	else if (prod1 == "wspd"){
		sfc1="sea";
  		img1 = imgDir+sat1+ymd+headers[sat1]+name1+yr1+mo1+dy1+dash+prod1+dash+sfc1+dash+cend1+".png";
  		img2 = imgDir+sat1+ymd+headers[sat1]+name2+yr1+mo1+dy1+dash+prod1+dash+sfc1+dash+cend1+".png"; 
                img3 = imgDir+sat1+ymd+headers[sat1]+name3+yr1+mo1+dy1+dash+prod1+dash+sfc1+dash+cend1+".png";
  		img4 = imgDir+sat1+ymd+headers[sat1]+name4+yr1+mo1+dy1+dash+prod1+dash+sfc1+dash+cend1+".png";
  		img5 = imgDir+sat1+ymd+headers[sat1]+name5+yr1+mo1+dy1+dash+prod1+dash+sfc1+dash+cend1+".png";
		img6 = "";
	}
	else {
  		img1 = imgDir+sat1+ymd+headers[sat1]+name1+yr1+mo1+dy1+dash+prod1+dash+sfc1+dash+cend1+".png";
  		img2 = imgDir+sat1+ymd+headers[sat1]+name2+yr1+mo1+dy1+dash+prod1+dash+sfc1+dash+cend1+".png"; 
  		img3 = imgDir+sat1+ymd+headers[sat1]+name3+yr1+mo1+dy1+dash+prod1+dash+sfc1+dash+cend1+".png";
  		img4 = imgDir+sat1+ymd+headers[sat1]+name4+yr1+mo1+dy1+dash+prod1+dash+sfc1+dash+cend1+".png";
  		img5 = imgDir+sat1+ymd+headers[sat1]+name5+yr1+mo1+dy1+dash+prod1+dash+sfc1+dash+cend1+".png";
		if ( sfc1 == 'lnd' ) {
		    img6 = imgDir+sat1+ymd+headers[sat1]+name6+yr1+mo1+dy1+dash+prod1+dash+'snw'+dash+cend1+".png";
		}
		else if ( sfc1 == 'sea' ) {
		    img6 = imgDir+sat1+ymd+headers[sat1]+name6+yr1+mo1+dy1+dash+prod1+dash+'ice'+dash+cend1+".png";
		}
	}
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

	document.form.img1.title = img1;
	document.form.img2.title = img2;
	document.form.img3.title = img3;
	document.form.img4.title = img4;
	document.form.img5.title = img5;
	document.form.img6.title = img6;
	
	document.form.img1.alt = alt1;
	document.form.img2.alt = alt2;
	document.form.img3.alt = alt3;
	document.form.img4.alt = alt4;
	document.form.img5.alt = alt5;
	document.form.img6.alt = alt6;
	
	document.getElementById("href1").href = img1;
	document.getElementById("href2").href = img2;
	document.getElementById("href3").href = img3;
	document.getElementById("href4").href = img4;
	document.getElementById("href5").href = img5;
	document.getElementById("href6").href = img6;
	
	if  ( prod1 == "temp" || prod1 == "wv" ) {       
	    document.form.img7.src = img7;
            document.form.img8.src = img8;
	    document.form.img9.src = img9;
            document.form.imga.src = imga;
	
	    var index7 = img7.lastIndexOf("/");
	    var alt7   = img7.substring(index7+1,img7.length);
	
	    var index8 = img8.lastIndexOf("/");
	    var alt8   = img8.substring(index8+1,img8.length);
	   
	    var index9 = img9.lastIndexOf("/");
	    var alt9   = img9.substring(index9+1,img9.length);
	    
	    var indexa = imga.lastIndexOf("/");
	    var alta   = imga.substring(indexa+1,imga.length);
	
	    document.form.img7.title = img7;
	    document.form.img8.title = img8;
	    document.form.img9.title = img9;
	    document.form.imga.title = imga;
	
	    document.form.img7.alt = alt7;
	    document.form.img8.alt = alt8;
	    document.form.img9.alt = alt9;
	    document.form.imga.alt = alta;
	
	    document.getElementById("href7").href = img7;
	    document.getElementById("href8").href = img8;
	    document.getElementById("href9").href = img9;
	    document.getElementById("hrefa").href = imga;

	}
	else {	
	    document.form.img7.src = "";
            document.form.img8.src = "";
	    document.form.img9.src = "";
            document.form.imga.src = "";
	
	    document.form.img7.title = "";
	    document.form.img8.title = "";
	    document.form.img9.title = "";
	    document.form.imga.title = "";
	
	    document.form.img7.alt = "";
	    document.form.img8.alt = "";
	    document.form.img9.alt = "";
	    document.form.imga.alt = "";
	
	    document.getElementById("href7").href = "";
	    document.getElementById("href8").href = "";
	    document.getElementById("href9").href = "";
	    document.getElementById("hrefa").href = "";
	}
	
}



function loadInit( ref1, sat1, prod1, layer1, cond1 ) 
{ 
	var prod_old_exist = 0;
	var layer_old_exist = 0;
	var cond_old_exist = 0;
	
	////////////////////////////////////////////////////////////////////////////////////////////
	// product list update
	////////////////////////////////////////////////////////////////////////////////////////////
        
	document.form.prod.options.length = 0; 
	document.form.prod2.options.length = 0; 
	
	var ref_sat = ref1 + '_' + sat1;
	var prods = PRODS[ref_sat].split(" ");
	
	for ( i=0; i<prods.length; i++ )  {
	    var left = prods[i].indexOf("(");
	    var prod_cur = prods[i] ;
	    if ( left != -1 ) {
	        prod_cur = prods[i].substring(0,left);
	    }
	    if ( prod_cur == prod1 ) {
	        prod_old_exist = 1;
	    }
	    var prod_text = getTxt(prod_cur);
	    document.form.prod.options[i]       = new Option();
	    document.form.prod.options[i].value = prod_cur;
	    document.form.prod.options[i].text  = prod_text;
	    
	    document.form.prod2.options[i]       = new Option();
	    document.form.prod2.options[i].value = prod_cur;
	    document.form.prod2.options[i].text  = prod_text;
	   
	}
		
	// if populated product list has old product value, we use old one; otherwise, use the 1st one in the list
	
	if ( prod_old_exist == 1 ) {
	    document.form.prod.value = prod1;
	    document.form.prod2.value = prod1;
	}
	else {
	    prod = document.form.prod.options[0].value;
	}


	////////////////////////////////////////////////////////////////////////////////////////////
	// layer and cond list update
	////////////////////////////////////////////////////////////////////////////////////////////
         
	document.form.layer.options.length = 0; 
	document.form.layer2.options.length = 0; 

	document.form.cond.options.length = 0; 
	document.form.cond2.options.length = 0; 

	document.getElementById("layer").className ="optioninvisible";
	document.getElementById("layer2").className="optioninvisible";
	
	document.getElementById("cond").className ="optioninvisible";
	document.getElementById("cond2").className="optioninvisible";


	for ( i=0; i<prods.length; i++ )  {
	    var left = prods[i].indexOf("(");
	    var prod_cur = prods[i] ;
	    if ( left != -1 ) { 
	        prod_cur = prods[i].substring(0,left);
	    }

	    if ( prod_cur == prod1 && left > 0 ) {  // start more than one level ( channel/layer ) 
	        document.getElementById("layer").className ="optionvisible";  
	        document.getElementById("layer2").className="optionvisible";

	    	var right = prods[i].indexOf(")");
	    	var layer_str = prods[i].substring(left+1,right);
	    	var layers = layer_str.split(":");
	    	
	    	for ( j=0; j<layers.length; j++ ) {
		    document.form.layer.options[j] = new Option();
		    document.form.layer.options[j].value = layers[j];

		    document.form.layer2.options[j] = new Option();
		    document.form.layer2.options[j].value = layers[j];

	    	    if ( layer1 == layers[j] ) { 
		        layer_old_exist = 1;
		    }
	    	    if ( prod_cur == 'em' ) { 
	    	        document.form.layer.options[j].text  = 'ch'+String(j+1)+":"+String(layers[j]);
		        document.form.layer2.options[j].text = 'ch'+String(j+1)+":"+String(layers[j]);
			document.form.layer.title  = "select a channel";
			document.form.layer2.title = "select a channel";
		    }
	    	    else { 
	    	        document.form.layer.options[j].text  = String(layers[j]);
			document.form.layer2.options[j].text = String(layers[j]);
			document.form.layer.title  = "select a layer";
			document.form.layer2.title = "select a layer";
		    }
	    	}
		
		if( prod_cur == 'temp' || prod_cur == 'wv' || prod_cur == 'em' ) { 
		
		    document.getElementById("cond").className ="optionvisible";  
	            document.getElementById("cond2").className="optionvisible";
		    
	    	    for ( j=0; j<conds_val.length; j++ ) {
		        document.form.cond.options[j] = new Option();
		        document.form.cond.options[j].value = conds_val[j];
	    	        document.form.cond.options[j].text  = conds_txt[j];

		        document.form.cond2.options[j] = new Option();
		        document.form.cond2.options[j].value = conds_val[j];
			document.form.cond2.options[j].text  = conds_txt[j];

	    	        if ( cond1 == conds_val[j] ) { 
			    cond_old_exist = 1; 
			}
	    	    }
		}
                
	        if ( prod_cur == prod && left < 0 && prod_cur == 'wspd' ) {  // wind speed

		        document.getElementById("cond").className ="optionvisible";  
	                document.getElementById("cond2").className="optionvisible";

	    	        for ( j=0; j<cond2s_val.length; j++ ) {
		            document.form.cond.options[j] = new Option();
		            document.form.cond.options[j].value = cond2s_val[j];
	    	            document.form.cond.options[j].text  = cond2s_txt[j];

		            document.form.cond2.options[j] = new Option();
		            document.form.cond2.options[j].value = cond2s_val[j];
			    document.form.cond2.options[j].text  = cond2s_txt[j];

	    	            if ( cond == cond2s_val[j] ) { 
			        cond_old_exist = 1; 
			    }
	    	        }
	        }
	    
	        if ( prod_cur == prod && left < 0 && ( prod_cur == 'tpw' || prod_cur == 'tskin' || prod_cur == 'psfc' ) ) {  // wind speed

		        document.getElementById("cond").className ="optionvisible";  
	                document.getElementById("cond2").className="optionvisible";
	    	        
                        for ( j=0; j<conds_val.length; j++ ) {
		            document.form.cond.options[j] = new Option();
		            document.form.cond.options[j].value = conds_val[j];
	    	            document.form.cond.options[j].text  = conds_txt[j];

		           document.form.cond2.options[j] = new Option();
		           document.form.cond2.options[j].value = conds_val[j];
			   document.form.cond2.options[j].text  = conds_txt[j];

	    	           if ( cond1 == conds_val[j] ) { 
			       cond_old_exist = 1; 
			   }
	    	    }

	        }
	    
            }  // end more than one level ( channel/layer )

	}
	// if layer list has old layer, we use old one; otherwise, use the 1st one in the layer list
	if ( layer_old_exist == 1 ) { 
		document.form.layer.value  = layer1; 
		document.form.layer2.value = layer1;
	}
	else if( document.getElementById("layer").className == "optionvisible" ) {
		layer = document.form.layer.options[0].value;
	}
	
	// if cond list has old cond, we use old one; otherwise, use the 1st one in the cond list	
	if ( cond_old_exist == 1 ) { 
		document.form.cond.value  = cond1; 
		document.form.cond2.value = cond1;
	}
	else if( document.getElementById("cond").className == "optionvisible" ) {
		cond = document.form.cond.options[0].value;
	}

} 



function loadInitialImages(ref1,sat1,prod1,layer1,cond1,cend1,sfc1,yr1,mo1,dy1) {
	
	if ( ref1 == "" ) {
		var now = new Date();

		var hour = now.getHours();
		var minute = now.getMinutes()
		
		if( hour > 14 ) {
			now.setDate(now.getDate()-ndayback) ; // n days ago
		}
		else {
			now.setDate(now.getDate()-ndayback-1) ; // n days ago
		}

		var year  = now.getFullYear();
		var month = now.getMonth();  // month starting index is 0
		var day   = now.getDate();
		
		document.form.ref.selectedIndex = 0;
		document.form.yr.value = year;
		document.form.mo.selectedIndex = month ;
		document.form.dy.selectedIndex = day - 1;
		document.form.sat.selectedIndex = 0;
		document.form.prod.value = "tpw";
		document.form.layer.selectedIndex = 0;
		document.form.cond.selectedIndex = 0;
		document.form.cend.selectedIndex = 0;
		document.form.sfc.selectedIndex = 2;

		YEAR_DEFAULT  = document.form.yr.value
		MONTH_DEFAULT = document.form.mo.value;
		DAY_DEFAULT   = document.form.dy.value;
		
		document.form.ref2.selectedIndex = 0;
		document.form.yr2.value = year;
		document.form.mo2.selectedIndex = month ;
		document.form.dy2.selectedIndex = day - 1;
		document.form.sat2.selectedIndex = 0;
		document.form.prod2.value = "tpw";
		document.form.layer2.selectedIndex = 0;
		document.form.cond2.selectedIndex = 0;
		document.form.cend2.selectedIndex = 0;
		document.form.sfc2.selectedIndex = 2;
		
		document.getElementById("box7").style.display = "none";   
		document.getElementById("box8").style.display = "none";   
		document.getElementById("box9").style.display = "none";   
		document.getElementById("boxa").style.display = "none";
		
		loadImage();
	}
	else {
		
		ref = ref1;
		sat = sat1;
		prod = prod1;
		layer = layer1;
		cond = cond1;
		
		loadInit( ref1, sat1, prod1, layer1, cond1 );
		
		document.form.ref.value = ref1;
		document.form.yr.value = yr1;
		document.form.mo.value = mo1;
		document.form.dy.value = dy1;
		document.form.sat.value = sat1;
		document.form.prod.value = prod1;
		document.form.layer.value = layer1;
		document.form.cond.value = cond1;
		document.form.cend.value = cend1;
		document.form.sfc.value = sfc1;
		
		document.form.ref2.value = ref1;
		document.form.yr2.value = yr1;
		document.form.mo2.value = mo1;
		document.form.dy2.value = dy1;
		document.form.sat2.value = sat1;
		document.form.prod2.value = prod1;
		document.form.layer2.value = layer1;
		document.form.cond2.value = cond1;
		document.form.cend2.value = cend1;
		document.form.sfc2.value = sfc1;
		
		changeSize(prod1);
		loadImageHelp(ref1,sat1,prod1,layer1,cond1,cend1,sfc1,yr1,mo1,dy1);
	}
	
}



function loadInitialImagesV(ref1,sat1,prod1,layer1,cond1,cend1,sfc1,yr1,mo1,dy1) {

	if ( ref1 == "" ) {
		var now = new Date();
		now.setDate(now.getDate()-ndayback) ; // n days ago

		var year  = now.getFullYear();
		var month = now.getMonth();  // month starting index is 0
		var day   = now.getDate();

		document.form.yr.value = year;
		document.form.mo.selectedIndex = month ;
		document.form.dy.selectedIndex = day - 1;
		document.form.sfc.selectedIndex = 2;
		document.form.prod.value = "tpw";

		YEAR_DEFAULT  = document.form.yr.value
		MONTH_DEFAULT = document.form.mo.value;
		DAY_DEFAULT   = document.form.dy.value;
		
		document.form.yr2.value = year;
		document.form.mo2.selectedIndex = month ;
		document.form.dy2.selectedIndex = day - 1;
		document.form.sfc2.selectedIndex = 2;
		document.form.prod2.value = "tpw";

		document.getElementById("box7").style.display = "none";   
		document.getElementById("box8").style.display = "none";   
		document.getElementById("box9").style.display = "none";   
		document.getElementById("boxa").style.display = "none";

		loadImage();
	}
	else {
		
		ref = ref1;
		sat = sat1;
		prod = prod1;
		layer = layer1;
		cond = cond1;

		loadInit( ref1, sat1, prod1, layer1, cond1 ); 
		
		document.form.ref.value = ref1;
		document.form.yr.value = yr1;
		document.form.mo.value = mo1;
		document.form.dy.value = dy1;
		document.form.sat.value = sat1;
		document.form.prod.value = prod1;
		document.form.layer.value = layer1;
		document.form.cond.value = cond1;
		document.form.cend.value = cend1;
		document.form.sfc.value = sfc1;
		
		document.form.ref2.value = ref1;
		document.form.yr2.value = yr1;
		document.form.mo2.value = mo1;
		document.form.dy2.value = dy1;
		document.form.sat2.value = sat1;
		document.form.prod2.value = prod1;
		document.form.layer2.value = layer1;
		document.form.cond2.value = cond1;
		document.form.cend2.value = cend1;
		document.form.sfc2.value = sfc1;
		
		changeSizeV(prod1);
		loadImage();
		//loadImageHelp(ref1,sat1,prod1,layer1,cond1,cend1,sfc1,yr1,mo1,dy1);
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


// if nday > 0, foward, if nday < 0, backward
function shift( nday ) {

        var year  = parseInt(document.form.yr.value,10);
        var month = parseInt(document.form.mo.value,10);
        var day   = parseInt(document.form.dy.value,10);

        var now = new Date(year,month-1,day);
        now.setDate(now.getDate()+nday);

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
