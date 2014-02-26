var prodIds_poes = "angle clw em(23v:31v:50v:52v:53h:54h:54v:55h:57h1:57h2:57h3:57h4:57h5:57h6:89v1:89v2:157h:184h:186h:190v) gs iwp lwp psfc rr rwp scanday sice sicemy sicefy snow swe tbu(23v:31v:50v:52v:53h:54h:54v:55h:57h1:57h2:57h3:57h4:57h5:57h6:89v1:89v2:157h:184h:186h:190v) tbc(23v:31v:50v:52v:53h:54h:54v:55h:57h1:57h2:57h3:57h4:57h5:57h6:89v1:89v2:157h:184h:186h:190v) temp(100mb:200mb:300mb:400mb:500mb:600mb:700mb:800mb:850mb:900mb:950mb) tpw tskin wv(100mb:200mb:300mb:400mb:500mb:600mb:700mb:800mb:850mb:900mb:950mb)";
var prodIds_atms = "angle clw em(23v:31v:53h:54h:55h:57h1:57h2:57h3:57h4:57h5:57h6:184h:186h) gs iwp lwp psfc rr rwp scanday sice sicemy sicefy snow swe tbu(23v:31v:53h:54h:55h:57h1:57h2:57h3:57h4:57h5:57h6:184h:186h) tbc(23v:31v:53h:54h:55h:57h1:57h2:57h3:57h4:57h5:57h6:184h:186h) temp(100mb:200mb:300mb:400mb:500mb:600mb:700mb:800mb:850mb:900mb:950mb) tpw tskin wv(100mb:200mb:300mb:400mb:500mb:600mb:700mb:800mb:850mb:900mb:950mb)";
var prodIds_dmsp = "clw em(57rc:59rc:150h:190h:186h:184h:19h:19v:22v:37h:37v:91v:91h:63rc:60rc1:60rc2:60rc3:60rc4:60rc5) gs iwp lwp psfc rr rwp scanday scanpos sice sicemy sicefy snow swe tbu(57rc:59rc:150h:190h:186h:184h:19h:19v:22v:37h:37v:91v:91h:63rc:60rc1:60rc2:60rc3:60rc4:60rc5) tbc(57rc:59rc:150h:190h:186h:184h:19h:19v:22v:37h:37v:91v:91h:63rc:60rc1:60rc2:60rc3:60rc4:60rc5) temp(100mb:200mb:300mb:400mb:500mb:600mb:700mb:800mb:850mb:900mb:950mb) tpw tskin wv(100mb:200mb:300mb:400mb:500mb:600mb:700mb:800mb:850mb:900mb:950mb)";

var prodIds = new Array();

prodIds['n18_n19']       = prodIds_poes;
prodIds['n18_metopA']    = prodIds_poes;
prodIds['n18_metopB']    = prodIds_poes;
prodIds['n19_metopA']    = prodIds_poes;
prodIds['n19_metopB']    = prodIds_poes;
prodIds['metopA_metopB'] = prodIds_poes;;

prodIds['n18_npp']       = prodIds_atms;
prodIds['n19_npp']       = prodIds_atms;
prodIds['metopA_npp']    = prodIds_atms;
prodIds['metopB_npp']    = prodIds_atms;

prodIds['f16_f18']       = prodIds_dmsp;


// gridded data based set ids
var pair_grid_vals = new Array(
			"n18_n19",
			"n18_metopA",
			"n18_metopB",
			"n19_metopA",
			"n19_metopB", 
			"metopA_metopB",
			"n18_npp",
			"n19_npp",
			"metopA_npp",
			"metopB_npp", 
			"f16_f18");

// gridded data based set texts
var pair_grid_txts = new Array(
			"N18 vs N19",
			"N18 vs METOPA",
			"N18 vs METOPB",
			"N19 vs METOPA",
			"N19 vs METOPB",
			"METOPA vs METOPB",
			"N18 vs ATMS",
			"N19 vs ATMS",
			"METOPA vs ATMS",
			"METOPB vs ATMS",
			"F16 vs F18");

// p2p data set based set ids
var pair_p2p_vals  = new Array(
			"n18_n19",
			"n18_metopA",
			"n19_metopA",
			"f16_f18");

// p2p data set based set texts
var pair_p2p_txts  = new Array(
			"N18 vs N19",
			"N18 vs METOPA",
			"N19 vs METOPA",
			"F16 vs F18");


var dirImgs = new Array();
dirImgs['grid'] = "images/";
dirImgs['p2p']  = "img_p2p/";
dirImgs['hr']   = "images/";

var tags = new Array();
tags['grid'] = "";
tags['p2p']  = "p2p_";

// number of channels matched
var NCHANS = new Array();
NCHANS["n18_n19"]       = 20;
NCHANS["n18_metopA"]    = 20;
NCHANS["n18_metopB"]    = 20;
NCHANS["n19_metopA"]    = 20;
NCHANS["n19_metopB"]    = 20;
NCHANS["metopA_metopB"] = 20;
NCHANS["n18_npp"]       = 13;
NCHANS["n19_npp"]       = 13;
NCHANS["metopA_npp"]    = 13;
NCHANS["metopB_npp"]    = 13;
NCHANS["f16_f18"]       = 19;

var sfc_brothers = new Array();
sfc_brothers['sea'] = 'ice';
sfc_brothers['lnd'] = 'snw';
sfc_brothers['all'] = 'all';

var headers = new Array();
headers['n18']    = 'mirs_adv_poes_' ;
headers['n19']    = 'mirs_adv_poes_' ;
headers['metopA'] = 'mirs_adv_poes_' ;
headers['metopB'] = 'mirs_adv_poes_' ;
headers['npp']    = 'mirs_adv_npoess_' ;
headers['f16']    = 'mirs_adv_dmsp_' ;
headers['f18']    = 'mirs_adv_dmsp_' ;

var sensors = new Array();
sensors['n18']    = "_amsuamhs_";
sensors['n19']    = "_amsuamhs_";
sensors['metopA'] = "_amsuamhs_";
sensors['metopB'] = "_amsuamhs_";
sensors['npp']    = "_atms_";
sensors['f16']    = "_ssmis_";
sensors['f18']    = "_ssmis_";

var prodTxts = new Array();
prodTxts["at"]       = "Antenna Temperature";
prodTxts["angle"]    = "Scan Angle";                
prodTxts["chisq"]    = "Chi Square";                
prodTxts["clw"]      = "CLW";                
prodTxts["em"]       = "Emissivity";
prodTxts["gs"]       = "Snow Grain Size Radius";
prodTxts["iwp"]      = "Ice Water Path";
prodTxts["lwp"]      = "Liquid Water Path";
prodTxts["nattempt"] = "Attempt Number";
prodTxts["niter"]    = "Iteration Number";
prodTxts["psfc"]     = "Surface Pressure";
prodTxts["qc"]       = "QC Flag";        
prodTxts["rr"]       = "Rain Rate";
prodTxts["rrday"]    = "Precip Estimate";
prodTxts["rrlat"]    = "Precip Lat Distri.";
prodTxts["rwp"]      = "Rain Water Path";
prodTxts["scanday"]  = "Scan Time";
prodTxts["scanpos"]  = "Scan Position";
prodTxts["seaicvr"]  = "Sea Ice Cover";
prodTxts["sfcTyp"]   = "Pre-Classified Sfc Type";
prodTxts["sfcTyp2"]  = "Post-Processed Sfc Type";
prodTxts["sice"]     = "Sea Ice Concentration";
prodTxts["sicefy"]   = "First Year SIC";
prodTxts["sicemy"]   = "Multiple Year SIC";
prodTxts["snow"]     = "Snow Cover";
prodTxts["swe"]      = "Snow Water Equivalent";        
prodTxts["swp"]      = "Snow Water Path";
prodTxts["windsp"]   = "Surface Wind Speed";
prodTxts["temp"]     = "Temperature Profile";
prodTxts["tpw"]      = "TPW";
prodTxts["ts"]       = "Sfc Temperature";
prodTxts["tskin"]    = "Skin Temperature";        
prodTxts["wet"]      = "Wetness Index";
prodTxts["wv"]       = "Water Vapor Profile";
prodTxts["tbc"]      = "Corr. TB";
prodTxts["tbu"]      = "UnCorr. TB";

var titles = new Array();
titles['em']   = "Select a channel";
titles['tbc']  = "Select a channel";
titles['tbu']  = "Select a channel";
titles['temp'] = "Select a pressure layer";
titles['wv']   = "Select a pressure layer";




function loadHelp( satval, prodval, isProd ) {
        
        var prod1_obj = document.form.prod1;
        var prod2_obj = document.form.prod2;
        
        var prod_old_exist = 0;
        
        var lay_old_value = document.form.lay1.value; 
        var lay_old_exist = 0 ;
                
        lay1_obj = document.form.lay1; 
        lay2_obj = document.form.lay2;
        
        lay1_obj.options.length = 0; 
        lay2_obj.options.length = 0; 
        
        
        document.getElementById("lay1").className  = "optioninvisible";
        document.getElementById("lay2").className  = "optioninvisible";

        document.getElementById("prev1").className = "inputinvisible";  
        document.getElementById("prev2").className = "inputinvisible";

        document.getElementById("next1").className = "inputinvisible";  
        document.getElementById("next2").className = "inputinvisible";

        var products = prodIds[satval].split(" ");
        for ( var i=0; i<products.length; i++ )  {
            
            var left = products[i].indexOf("(");
            var prod_value = products[i] ;
        	
            if ( left != -1 ) {
        	    prod_value = products[i].substring(0,left);
        	}
           
            if ( prod_value == prodval ) {
        	    prod_old_exist = 1;
        	}
        	
            var prod_text = prodTxts[prod_value];
            prod1_obj.options[i]       = new Option();
            prod1_obj.options[i].value = prod_value;
            prod1_obj.options[i].text  = prod_text;
        
            prod2_obj.options[i]       = new Option();
            prod2_obj.options[i].value = prod_value;
            prod2_obj.options[i].text  = prod_text;
        
        	// product has more than 1 level, need make lay1 option visible
            if ( prod_value == prodval && left > 0 ) {
                
		document.getElementById("lay1").className = "optionvisible";  
        	document.getElementById("lay2").className = "optionvisible";

        	document.getElementById("prev1").className = "inputvisible";  
        	document.getElementById("prev2").className = "inputvisible";

                document.getElementById("next1").className = "inputvisible";  
        	document.getElementById("next2").className = "inputvisible";

        	var right = products[i].indexOf(")");
        	var lay_str = products[i].substring(left+1,right);
        	var lays = lay_str.split(":");
        	
        	for ( var j=0; j<lays.length; j++ ) {
        		
        	    lay1_obj.options[j]       = new Option();
        	    lay1_obj.options[j].value = lays[j];
        	    lay1_obj.options[j].text  = lays[j];
        	    lay1_obj.title	     = titles[prod_value];
        			
        	    lay2_obj.options[j]       = new Option();
        	    lay2_obj.options[j].value = lays[j];
        	    lay2_obj.options[j].text  = lays[j];
        	    lay2_obj.title	     = titles[prod_value];
        			
        	    if ( lay_old_value == lays[j] ) {
        	    	lay_old_exist = 1;
        	    }
        	}
            }
        }
        
        //if( isProd == 1 ) {
        
            // if prod list has old prod value, then use old one; otherwise, use the 1st one in the prod list (default)
            if ( prod_old_exist == 1 ) { 
                    prod1_obj.value = prodval; 
                    prod2_obj.value = prodval;
                    prod1_obj.text  = prodTxts[prodval];
                    prod2_obj.text  = prodTxts[prodval];
            }
        //}
        
        // if lay list has old lay value, then use old one; otherwise, use the 1st one in the lay list (default)
        if ( lay_old_exist == 1 ) { 
                lay1_obj.value = lay_old_value; 
                lay2_obj.value = lay_old_value;
        }
} 


// the following 4 functions just call loadHelp

function changeSat1( satval )
{
    loadHelp( satval, document.form.prod1.value, 1 );
}

function changeSat2( satval )
{
    loadHelp( satval, document.form.prod2.value, 1 );
}

function changeProd1( prodval )
{
    loadHelp( document.form.sat1.value, prodval, 0 );
}

function changeProd2( prodval )
{
    loadHelp( document.form.sat2.value, prodval, 0 );
}


function changeData1( dataval ) 
{ 
        var obj_sat1 = document.form.sat1;
        var obj_sat2 = document.form.sat2;
        
        obj_sat1.options.length = 0;
        obj_sat2.options.length = 0;
        
        if( dataval == "p2p" ) {
        
                for(var i=0; i<pair_p2p_vals.length; i++) {
                        obj_sat1.options[i] = new Option();
                        obj_sat1.options[i].value = pair_p2p_vals[i];
                        obj_sat1.options[i].text  = pair_p2p_txts[i];
                }
        
                for(var i=0; i<pair_p2p_vals.length; i++) {
                        obj_sat2.options[i] = new Option();
                        obj_sat2.options[i].value = pair_p2p_vals[i];
                        obj_sat2.options[i].text  = pair_p2p_txts[i];
                }
                
        }
        else {
        
                for(var i=0; i<pair_grid_vals.length; i++) {
                        obj_sat1.options[i] = new Option();
                        obj_sat1.options[i].value = pair_grid_vals[i];
                        obj_sat1.options[i].text  = pair_grid_txts[i];
                }
        
                for(var i=0; i<pair_grid_vals.length; i++) {
                        obj_sat2.options[i] = new Option();
                        obj_sat2.options[i].value = pair_grid_vals[i];
                        obj_sat2.options[i].text  = pair_grid_txts[i];
                }
        }
                
} 


function changeData2( dataval ) 
{ 
        var obj_sat1 = document.form.sat1;
        var obj_sat2 = document.form.sat2;
        
        obj_sat1.options.length = 0;
        obj_sat2.options.length = 0;
        
        if( dataval == "p2p" ) {
        
                for(var i=0; i<pair_p2p_vals.length; i++) {
                        obj_sat1.options[i] = new Option();
                        obj_sat1.options[i].value = pair_p2p_vals[i];
                        obj_sat1.options[i].text  = pair_p2p_txts[i];
                }
        
                for(var i=0; i<pair_p2p_vals.length; i++) {
                        obj_sat2.options[i] = new Option();
                        obj_sat2.options[i].value = pair_p2p_vals[i];
                        obj_sat2.options[i].text  = pair_p2p_txts[i];
                }
        }
        else 
	{
                
                for(var i=0; i<pair_grid_vals.length; i++) {
                        obj_sat1.options[i] = new Option();
                        obj_sat1.options[i].value = pair_grid_vals[i];
                        obj_sat1.options[i].text  = pair_grid_txts[i];
                }
        
                for(var i=0; i<pair_grid_vals.length; i++) {
                        obj_sat2.options[i] = new Option();
                        obj_sat2.options[i].value = pair_grid_vals[i];
                        obj_sat2.options[i].text  = pair_grid_txts[i];
                }
        }
                
} 


function changeSize( product ) {
        
        if ( product == "temp" ||  product == "wv" ) {
                document.getElementById("box9").style.display = "table-cell";   
                document.getElementById("boxa").style.display = "table-cell";
                document.getElementById("boxb").style.display = "table-cell";
                document.getElementById("boxc").style.display = "table-cell";
        }
        else {
                document.getElementById("box9").style.display = "none";   
                document.getElementById("boxa").style.display = "none";
                document.getElementById("boxb").style.display = "none";
                document.getElementById("boxc").style.display = "none";
        } 
}


function changeSizeV( product ) {
        
        if ( product == "temp" ||  product == "wv" ) {
                document.getElementById("box9").style.display = "table-row";   
                document.getElementById("boxa").style.display = "table-row";
                document.getElementById("boxb").style.display = "table-row";
                document.getElementById("boxc").style.display = "table-row";
        }
        else {
                document.getElementById("box9").style.display = "none";   
                document.getElementById("boxa").style.display = "none";
                document.getElementById("boxb").style.display = "none";
                document.getElementById("boxc").style.display = "none";
        } 
}



function loadImage1() {
 
        var yr   = document.form.yr1.value;
        var mo   = document.form.mo1.value;
        var dy   = document.form.dy1.value;
        var cend = document.form.cend1.value;
        var sfc  = document.form.sfc1.value;
        var data = document.form.data1.value;
        var sat  = document.form.sat1.value;
        var prod = document.form.prod1.value;
        var lay  = document.form.lay1.value;

        // update bottom panel
        document.form.yr2.value   = yr;
        document.form.mo2.value   = mo;
        document.form.dy2.value   = dy;
        document.form.cend2.value = cend;
        document.form.sfc2.value  = sfc;
        document.form.data2.value = data;
        document.form.sat2.value  = sat;
        document.form.prod2.value = prod; 
        document.form.lay2.value  = lay;

        loadImageHelp(data,sat,prod,lay,cend,sfc,yr,mo,dy);
}


function loadImage2() {
 
        var yr = document.form.yr2.value;
        var mo = document.form.mo2.value;
        var dy = document.form.dy2.value;
        
        var cend = document.form.cend2.value;
        var sfc  = document.form.sfc2.value;
        var sat  = document.form.sat2.value;
        var data = document.form.data2.value;
        var prod = document.form.prod2.value;
        var lay  = document.form.lay2.value;

        // update upper panel
        document.form.yr1.value   = yr;
        document.form.mo1.value   = mo;
        document.form.dy1.value   = dy;
        document.form.cend1.value = cend;
        document.form.sfc1.value  = sfc;
        document.form.data1.value = data;
        document.form.sat1.value  = sat;
        document.form.prod1.value = prod; 
        document.form.lay1.value  = lay;

        loadImageHelp(data,sat,prod,lay,cend,sfc,yr,mo,dy);
}


function loadImageHelp(data,sat,prod,lay,cend,sfc,yr,mo,dy) {

        var ymd = '/' + yr + '-' + mo + '-' + dy + '/';
        var yyyymmdd = yr+mo+dy;
        
        var sats = sat.split("_");
        var sat1 = sats[0];
        var sat2 = sats[1];
        
        var header1 = headers[sat1];
        var header2 = headers[sat2];
        
        var sensor1 = sensors[sat1];
        var sensor2 = sensors[sat2];
        
        var header = header1;
        var region = 'glb_';
        
        var imgDir = dirImgs[data];
        var tag = tags[data];
        

        var dash  = "_";
        var name1 = sensor1 + region + tag ;
        var name2 = sensor2 + region + tag ;
        var name3 = "_bias_" + region + tag ;
        var name4 = "_asym_" + region + tag ;
        var name5 = '_scat_' + region + tag ;
        var name6 = '_scat_' + region + tag ;
        var name7 = "_hist_" + region + tag ;
        var name8 = "_hist_" + region + tag ;
        var name9 = dash + region + tag ;
        var namea = dash + region + tag ;
        var nameb = dash + region + tag ;
        var namec = dash + region + tag ;
        
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
        var imgb = ""
        var imgc = ""
        
        imgDir1="";
        imgDir2="";
	
	if( data == "grid" ) {   
          imgDir1 = imgDir+sat1+ymd;
          if ( sat == 'n18_npp' ) {
                imgDir1 = dirImgs['grid']+sat1+ymd;
          }
          if ( sat == 'metopB_npp' ) {
                imgDir1 = dirImgs['hr']+sat1+ymd;
          }
	  
          imgDir2 = imgDir+sat2+ymd;
          if ( sat == 'metopA_metopB' || sat == 'n18_metopB' || sat == 'n19_metopB' ) {
                imgDir2 = dirImgs['hr']+sat2+ymd;
          }
	}
	else {
          imgDir1 = imgDir+sat+ymd;
          imgDir2 = imgDir+sat+ymd;
	}  
	  
	  
        if ( prod == "em" || prod == "tbc" || prod == "tbu" ) {
		img1 = imgDir1+header1+sat1+name1+yyyymmdd+dash+prod+dash+lay+dash+sfc+dash+cend+".png";
                img2 = imgDir2+header2+sat2+name2+yyyymmdd+dash+prod+dash+lay+dash+sfc+dash+cend+".png"; 
		img3 = imgDir+sat+ymd+header+sat+name3+yyyymmdd+dash+prod+dash+lay+dash+sfc+dash+cend+".png";
                img4 = imgDir+sat+ymd+header+sat+name4+yyyymmdd+dash+prod+dash+lay+dash+sfc+dash+cend+".png";
                img5 = imgDir+sat+ymd+header+sat+name5+yyyymmdd+dash+prod+dash+lay+dash+sfc+dash+cend+".png";
                img6 = imgDir+sat+ymd+header+sat+name6+yyyymmdd+dash+prod+dash+lay+dash+sfc_brothers[sfc]+dash+cend+".png";
                img7 = imgDir+sat+ymd+header+sat+name7+yyyymmdd+dash+prod+dash+lay+dash+sfc+dash+"ad.png";
                img8 = imgDir+sat+ymd+header+sat+name8+yyyymmdd+dash+prod+dash+lay+dash+sfc_brothers[sfc]+dash+"ad.png";
        }
        else if ( prod == "temp" || prod == "wv" ) {
                img1 = imgDir1+header1+sat1+name1+yyyymmdd+dash+prod+dash+lay+dash+sfc+dash+cend+".png";
                img2 = imgDir2+header2+sat2+name2+yyyymmdd+dash+prod+dash+lay+dash+sfc+dash+cend+".png"; 
                img3 = imgDir+sat+ymd+header+sat+name3+yyyymmdd+dash+prod+dash+lay+dash+sfc+dash+cend+".png";
                img4 = imgDir+sat+ymd+header+sat+name4+yyyymmdd+dash+prod+dash+lay+dash+sfc+dash+cend+".png";
                img5 = imgDir+sat+ymd+header+sat+name5+yyyymmdd+dash+prod+dash+lay+dash+sfc+dash+cend+".png";
                img6 = imgDir+sat+ymd+header+sat+name6+yyyymmdd+dash+prod+dash+lay+dash+sfc_brothers[sfc]+dash+cend+".png";
                img7 = imgDir+sat+ymd+header+sat+name7+yyyymmdd+dash+prod+dash+lay+dash+sfc+dash+"ad.png";
                img8 = imgDir+sat+ymd+header+sat+name7+yyyymmdd+dash+prod+dash+lay+dash+sfc_brothers[sfc]+dash+"ad.png";
                
                img9 = imgDir+sat+ymd+header+sat+name9+yyyymmdd+dash+prod+"_mean_vert_"+cend+".png";
                imga = imgDir+sat+ymd+header+sat+namea+yyyymmdd+dash+prod+"_stdv_vert_"+cend+".png";
                imgb = imgDir+sat+ymd+header+sat+nameb+yyyymmdd+dash+prod+"_mean_vert_ad.png";
                imgc = imgDir+sat+ymd+header+sat+namec+yyyymmdd+dash+prod+"_stdv_vert_ad.png";
        }
        else if ( prod == "swe" || prod == "snow" || prod == "gs" ){
                sfc = 'lnd';
                img1 = imgDir1+header1+sat1+name1+yyyymmdd+dash+prod+dash+'cyl'+dash+sfc+dash+cend+".png";
                img2 = imgDir2+header2+sat2+name2+yyyymmdd+dash+prod+dash+'cyl'+dash+sfc+dash+cend+".png"; 
                img3 = imgDir+sat+ymd+header+sat+name3+yyyymmdd+dash+prod+dash+sfc+dash+cend+".png";
                img4 = imgDir+sat+ymd+header+sat+name4+yyyymmdd+dash+prod+dash+sfc+dash+cend+".png";
                img5 = imgDir+sat+ymd+header+sat+name5+yyyymmdd+dash+prod+dash+sfc+dash+cend+".png";
                img6 = imgDir+sat+ymd+header+sat+name6+yyyymmdd+dash+prod+dash+sfc_brothers[sfc]+dash+cend+".png";
                img7 = imgDir+sat+ymd+header+sat+name7+yyyymmdd+dash+prod+dash+sfc+dash+"ad.png";
                img8 = imgDir+sat+ymd+header+sat+name8+yyyymmdd+dash+prod+dash+sfc_brothers[sfc]+dash+"ad.png";
        }
        else if ( prod == "sice" || prod == "sicefy" || prod == "sicemy" ){
                sfc = 'sea';
                img1 = imgDir1+header1+sat1+name1+yyyymmdd+dash+prod+dash+'cyl'+dash+sfc+dash+cend+".png";
                img2 = imgDir2+header2+sat2+name2+yyyymmdd+dash+prod+dash+'cyl'+dash+sfc+dash+cend+".png"; 
                img3 = imgDir+sat+ymd+header+sat+name3+yyyymmdd+dash+prod+dash+sfc+dash+cend+".png";
                img4 = imgDir+sat+ymd+header+sat+name4+yyyymmdd+dash+prod+dash+sfc+dash+cend+".png";
                img5 = imgDir+sat+ymd+header+sat+name5+yyyymmdd+dash+prod+dash+sfc+dash+cend+".png";
                img6 = imgDir+sat+ymd+header+sat+name6+yyyymmdd+dash+prod+dash+sfc_brothers[sfc]+dash+cend+".png";
                img7 = imgDir+sat+ymd+header+sat+name7+yyyymmdd+dash+prod+dash+sfc+dash+"ad.png";
                img8 = imgDir+sat+ymd+header+sat+name8+yyyymmdd+dash+prod+dash+sfc_brothers[sfc]+dash+"ad.png";
        }
        else if ( prod == "clw" || prod == "lwp" ){
                sfc = 'sea';
                img1 = imgDir1+header1+sat1+name1+yyyymmdd+dash+prod+dash+sfc+dash+cend+".png";
                img2 = imgDir2+header2+sat2+name2+yyyymmdd+dash+prod+dash+sfc+dash+cend+".png"; 
                img3 = imgDir+sat+ymd+header+sat+name3+yyyymmdd+dash+prod+dash+sfc+dash+cend+".png";
                img4 = imgDir+sat+ymd+header+sat+name4+yyyymmdd+dash+prod+dash+sfc+dash+cend+".png";
                img5 = imgDir+sat+ymd+header+sat+name5+yyyymmdd+dash+prod+dash+sfc+dash+cend+".png";
		img6 = imgDir+sat+ymd+header+sat+name6+yyyymmdd+dash+prod+dash+sfc_brothers[sfc]+dash+cend+".png";
		img7 = imgDir+sat+ymd+header+sat+name7+yyyymmdd+dash+prod+dash+sfc+dash+"ad.png";
                img8 = imgDir+sat+ymd+header+sat+name8+yyyymmdd+dash+prod+dash+sfc_brothers[sfc]+dash+"ad.png";
        }
        else if ( prod == "scanday" ){
                sfc = 'all';
                img1 = imgDir1+header1+sat1+name1+yyyymmdd+dash+prod+dash+sfc+dash+cend+".png";
                img2 = imgDir2+header2+sat2+name2+yyyymmdd+dash+prod+dash+sfc+dash+cend+".png"; 
                img3 = imgDir+sat+ymd+header+sat+name3+yyyymmdd+dash+prod+dash+sfc+dash+cend+".png";
                img4 = imgDir+sat+ymd+header+sat+name4+yyyymmdd+dash+prod+dash+sfc+dash+cend+".png";
                img5 = imgDir+sat+ymd+header+sat+name5+yyyymmdd+dash+prod+dash+sfc+dash+cend+".png";
                img6 = imgDir+sat+ymd+header+sat+name5+yyyymmdd+dash+prod+dash+sfc_brothers[sfc]+dash+cend+".png";
                img7 = imgDir+sat+ymd+header+sat+name7+yyyymmdd+dash+prod+dash+sfc+dash+"ad.png";
                img8 = imgDir+sat+ymd+header+sat+name8+yyyymmdd+dash+prod+dash+sfc_brothers[sfc]+dash+"ad.png";
        }
        else
        {
                img1 = imgDir1+header1+sat1+name1+yyyymmdd+dash+prod+dash+sfc+dash+cend+".png";
                img2 = imgDir2+header2+sat2+name2+yyyymmdd+dash+prod+dash+sfc+dash+cend+".png"; 
                img3 = imgDir+sat+ymd+header+sat+name3+yyyymmdd+dash+prod+dash+sfc+dash+cend+".png";
                img4 = imgDir+sat+ymd+header+sat+name4+yyyymmdd+dash+prod+dash+sfc+dash+cend+".png";
                img5 = imgDir+sat+ymd+header+sat+name5+yyyymmdd+dash+prod+dash+sfc+dash+cend+".png";
                img6 = imgDir+sat+ymd+header+sat+name6+yyyymmdd+dash+prod+dash+sfc_brothers[sfc]+dash+cend+".png";
                img7 = imgDir+sat+ymd+header+sat+name7+yyyymmdd+dash+prod+dash+sfc+dash+"ad.png";
                img8 = imgDir+sat+ymd+header+sat+name8+yyyymmdd+dash+prod+dash+sfc_brothers[sfc]+dash+"ad.png";

        }

        document.form.img1.src = img1;
        document.form.img2.src = img2;
        document.form.img3.src = img3;
        document.form.img4.src = img4;
        document.form.img5.src = img5;
        document.form.img6.src = img6;
        document.form.img7.src = img7;
        document.form.img8.src = img8;
        
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
           
        document.form.img1.title = img1;
        document.form.img2.title = img2;
        document.form.img3.title = img3;
        document.form.img4.title = img4;
        document.form.img5.title = img5;
        document.form.img6.title = img6;
        document.form.img7.title = img7;
        document.form.img8.title = img8;
        
        document.form.img1.alt = alt1;
        document.form.img2.alt = alt2;
        document.form.img3.alt = alt3;
        document.form.img4.alt = alt4;
        document.form.img5.alt = alt5;
        document.form.img6.alt = alt6;
        document.form.img7.alt = alt7;
        document.form.img8.alt = alt8;
        
        document.getElementById("href1").href = img1;
        document.getElementById("href2").href = img2;
        document.getElementById("href3").href = img3;
        document.getElementById("href4").href = img4;
        document.getElementById("href5").href = img5;
        document.getElementById("href6").href = img6;
        document.getElementById("href7").href = img7;
        document.getElementById("href8").href = img8;
        
        if  ( prod == "temp" || prod == "wv" ) {
        
                document.form.img9.src = img9;
                document.form.imga.src = imga;
                document.form.imgb.src = imgb;
                document.form.imgc.src = imgc;
        
                var index9 = img9.lastIndexOf("/");
                var alt9   = img9.substring(index9+1,img9.length);

                var indexa = imga.lastIndexOf("/");
                var alta   = imga.substring(indexa+1,imga.length);

                var indexb = imgb.lastIndexOf("/");
                var altb   = imgb.substring(indexb+1,imgb.length);

                var indexc = imgc.lastIndexOf("/");
                var altc   = imgc.substring(indexc+1,imgc.length);

                document.form.img9.title = img9;
                document.form.imga.title = imga;
                document.form.imgb.title = imgb;
                document.form.imgc.title = imgc;

                document.form.img9.alt = alt9;
                document.form.imga.alt = alta;
                document.form.imgb.alt = altb;
                document.form.imgc.alt = altc;

                document.getElementById("href9").href = img9;
                document.getElementById("hrefa").href = imga;
                document.getElementById("hrefb").href = imgb;
                document.getElementById("hrefc").href = imgc;
        }
        else {        
                document.form.img9.src  = "";
                document.form.imga.src = "";
                document.form.imgb.src = "";
                document.form.imgc.src = "";

                document.form.img9.title  = "";
                document.form.imga.title = "";
                document.form.imgb.title = "";
                document.form.imgc.title = "";

                document.form.img9.alt  = "";
                document.form.imga.alt = "";
                document.form.imgb.alt = "";
                document.form.imgc.alt = "";

                document.getElementById("href9").href = "";
                document.getElementById("hrefa").href = "";
                document.getElementById("hrefb").href = "";
                document.getElementById("hrefc").href = "";
        }
        
}


function inter_init(data,sat,prod,lay,cend,sfc,yr,mo,dy) {
        
        if ( sat == "" ) {
                var now = new Date();
                now.setDate(now.getDate()-2) ; // 1 days ago

                var year  = now.getFullYear();
                var month = now.getMonth();  // month starting index is 0
                var day   = now.getDate();

                document.form.yr1.value = year ;
                document.form.mo1.selectedIndex = month ;
                document.form.dy1.selectedIndex = day - 1;
                document.form.sat1.selectedIndex = 0;
                document.form.data1.selectedIndex = 0;
                document.form.prod1.selectedIndex = 0;
                document.form.lay1.selectedIndex = 0;
                document.form.cend1.selectedIndex = 0;
                document.form.sfc1.selectedIndex = 0;

                document.form.yr2.value = year ;
                document.form.mo2.selectedIndex = month ;
                document.form.dy2.selectedIndex = day - 1;
                document.form.data2.selectedIndex = 0;
                document.form.sat2.selectedIndex = 0;
                document.form.prod2.selectedIndex = 0;
                document.form.lay2.selectedIndex = 0;
                document.form.cend2.selectedIndex = 0;
                document.form.sfc2.selectedIndex = 0;

                document.getElementById("box9").style.display = "none";   
                document.getElementById("boxa").style.display = "none";
                document.getElementById("boxb").style.display = "none";
                document.getElementById("boxc").style.display = "none";

                loadImage1();
        }
        else {
                
                loadHelp(sat, prod);
                
                document.form.yr1.value = yr;
                document.form.mo1.value = mo ;
                document.form.dy1.value = dy;
                document.form.data1.value = data;
                document.form.sat1.value = sat;
                document.form.prod1.value = prod;
                document.form.lay1.value = lay;
                document.form.cend1.value = cend;
                document.form.sfc1.value = sfc;
                
                document.form.yr2.value = yr;
                document.form.mo2.value = mo ;
                document.form.dy2.value = dy;
                document.form.data2.value = data;
                document.form.sat2.value = sat;
                document.form.prod2.value = prod;
                document.form.lay2.value = lay;
                document.form.cend2.value = cend;
                document.form.sfc2.value = sfc;
                
                changeSize(prod);
                loadImageHelp(data,sat,prod,lay,cend,sfc,yr,mo,dy);
        }
        
}


function interv_init(data,sat,prod,lay,cend,sfc,yr,mo,dy) {
        
        if ( sat == "" ) {
                var now = new Date();
                now.setDate(now.getDate()-2) ; // 2 days ago

                var year  = now.getFullYear();
                var month = now.getMonth();  // month starting index is 0
                var day   = now.getDate();

                document.form.yr1.value = year;
                document.form.mo1.selectedIndex = month ;
                document.form.dy1.selectedIndex = day - 1;
                document.form.sat1.selectedIndex = 0;
                document.form.prod1.selectedIndex = 0;
                document.form.lay1.selectedIndex = 0;
                document.form.cend1.selectedIndex = 0;
                document.form.sfc1.selectedIndex = 0;

                document.form.yr2.value = year;
                document.form.mo2.selectedIndex = month ;
                document.form.dy2.selectedIndex = day - 1;
                document.form.sat2.selectedIndex = 0;
                document.form.prod2.selectedIndex = 0;
                document.form.lay2.selectedIndex = 0;
                document.form.cend2.selectedIndex = 0;
                document.form.sfc2.selectedIndex = 0;

                document.getElementById("box9").style.display = "none";   
                document.getElementById("boxa").style.display = "none";
                document.getElementById("boxb").style.display = "none";
                document.getElementById("boxc").style.display = "none";

                loadImage1();
        }
        else {
                
                loadHelp(sat, prod);
                
                document.form.yr1.value = yr;
                document.form.mo1.value = mo ;
                document.form.dy1.value = dy;
                document.form.sat1.value = sat;
                document.form.prod1.value = prod;
                document.form.lay1.value = lay;
                document.form.cend1.value = cend;
                document.form.sfc1.value = sfc;
                
                document.form.yr2.value = yr;
                document.form.mo2.value = mo ;
                document.form.dy2.value = dy;
                document.form.sat2.value = sat;
                document.form.prod2.value = prod;
                document.form.lay2.value = lay;
                document.form.cend2.value = cend;
                document.form.sfc2.value = sfc;
                
                changeSizeV(prod);
                loadImageHelp(data,sat,prod,lay,cend,sfc,yr,mo,dy);
        }
        
}


function rev() {

        var year  = parseInt(document.form.yr1.value,10);
        var month = parseInt(document.form.mo1.value,10);
        var day   = parseInt(document.form.dy1.value,10);

        var now = new Date(year,month-1,day);
        now.setDate(now.getDate()-1);

        year  = now.getFullYear();
        month = now.getMonth();
        day   = now.getDate();

        document.form.yr1.value = year;
        document.form.mo1.selectedIndex = month ;
        document.form.dy1.selectedIndex = day - 1;
        
        document.form.yr2.value = year;
        document.form.mo2.selectedIndex = month ;
        document.form.dy2.selectedIndex = day - 1;
        
        loadImage1(); 
}


function fwd() {

        var year  = parseInt(document.form.yr1.value,10);
        var month = parseInt(document.form.mo1.value,10);
        var day   = parseInt(document.form.dy1.value,10);

        var now = new Date(year,month-1,day);
        now.setDate(now.getDate()+1);

        year  = now.getFullYear();
        month = now.getMonth();
        day   = now.getDate();

        document.form.yr1.value = year;
        document.form.mo1.selectedIndex = month ;
        document.form.dy1.selectedIndex = day - 1;

        document.form.yr2.value = year;
        document.form.mo2.selectedIndex = month ;
        document.form.dy2.selectedIndex = day - 1;

        loadImage1();
}


function nextLay1() {
        
        var prod = document.form.prod1.value;
        
        if( prod == "temp" || prod == "wv" ) {
                
                var index_lay = document.form.lay1.selectedIndex + 1;
                if( index_lay > 10 ) 
                        document.form.lay1.selectedIndex = 0 ;
                else
                        document.form.lay1.selectedIndex = index_lay;
        }
        else if( prod == "em" || prod == "tbc" || prod == "tbu" ) {
        
                var pair = document.form.sat1.value;
                var nchan = NCHANS[pair];
                
                var index_lay = document.form.lay1.selectedIndex + 1;
                if( index_lay >= nchan ) 
                        document.form.lay1.selectedIndex = 0 ;
                else
                        document.form.lay1.selectedIndex = index_lay;
        }
        
        document.form.lay2.selectedIndex = document.form.lay1.selectedIndex;
        
        loadImage1();
}


function nextLay2() {
        
        var prod = document.form.prod2.value;
        
        if( prod == "temp" || prod == "wv" ) {
                
                var index_lay = document.form.lay2.selectedIndex + 1;
                if( index_lay > 10 ) 
                        document.form.lay2.selectedIndex = 0 ;
                else
                        document.form.lay2.selectedIndex = index_lay;
        }
        else if( prod == "em" || prod == "tbc" || prod == "tbu" ) {
        
                var pair = document.form.sat2.value;
                var nchan = NCHANS[pair];
                
                var index_lay = document.form.lay2.selectedIndex + 1;
                if( index_lay >= nchan ) 
                        document.form.lay2.selectedIndex = 0 ;
                else
                        document.form.lay2.selectedIndex = index_lay;
        }

        document.form.lay1.selectedIndex = document.form.lay2.selectedIndex;

        loadImage2();
}


function prevLay1() {
        
        var prod = document.form.prod1.value;
        
        if( prod == "temp" || prod == "wv" ) {
                
                var index_lay = document.form.lay1.selectedIndex - 1;
                if( index_lay == 0 ) 
                        document.form.lay1.selectedIndex = 10 ;
                else
                        document.form.lay1.selectedIndex = index_lay;
        }
        else if( prod == "em" || prod == "tbc" || prod == "tbu" ) {
        
                var pair = document.form.sat1.value;
                var nchan = NCHANS[pair];
                
                var index_lay = document.form.lay1.selectedIndex - 1;
                if( index_lay == 0 ) 
                        document.form.lay1.selectedIndex = nchan-1 ;
                else
                        document.form.lay1.selectedIndex = index_lay;
        }
        
        document.form.lay2.selectedIndex = document.form.lay1.selectedIndex;
        
        loadImage1();
}


function prevLay2() {
        
        var prod = document.form.prod2.value;
        
        if( prod == "temp" || prod == "wv" ) {
                
                var index_lay = document.form.lay2.selectedIndex - 1;
                if( index_lay == 0 ) 
                        document.form.lay2.selectedIndex = 10 ;
                else
                        document.form.lay2.selectedIndex = index_lay;
        }
        else if( prod == "em" || prod == "tbc" || prod == "tbu" ) {
        
                var pair = document.form.sat2.value;
                var nchan = NCHANS[pair];
                
                var index_lay = document.form.lay2.selectedIndex + 1;
                if( index_lay == 0 ) 
                        document.form.lay2.selectedIndex = nchan - 1 ;
                else
                        document.form.lay2.selectedIndex = index_lay;
        }

        document.form.lay1.selectedIndex = document.form.lay2.selectedIndex;
       
        loadImage2();
}
