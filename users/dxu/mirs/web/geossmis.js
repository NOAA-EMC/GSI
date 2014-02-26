var imgDir = "images/";

var imgDirs = new Array();

imgDirs["n18"] = "images/";
imgDirs["n19"] = "images/";
imgDirs["f16"] = "images/";
imgDirs["f18"] = "images/";
imgDirs["metopA"] = "images/";
imgDirs["metopB"] = "images/";
imgDirs["npp"]    = "images/";

var ndayback = 5;
var dash  = "_";

var values_ts = new Array(
			"bia",
			"cor",
			"dfr",
			"far",
			"fcn",
			"foh",
			"fom",
			"hss",
			"pfd",
			"pod",
			"pon",
			"std");

var texts_ts  = new Array(
			"Bias",
			"Correlation",
			"Detection Failure Ratio",
			"False Alarm Ratio",
			"Freq. of Corr. Null Fcst",
			"Frequency of Hits",
			"Frequency of Misses",
			"Heidke Skill Score",
			"Pr. of False Detection",
			"Pr. of Detection",
			"Pr. of a Null Event",
			"Standard Deviation");


function changeSat( satval ) {}
	

function changePlot( plotval ) 
{
	
	plot = plotval ;
	
	if( plotval == "ts" ) {
	
		document.getElementById("stat").className  = "optionvisible";
		document.getElementById("stat2").className = "optionvisible";
		
		document.getElementById("region").className  = "optionvisible";
		document.getElementById("region2").className = "optionvisible";
		
		document.getElementById("view").className  = "optioninvisible";
		document.getElementById("view2").className = "optioninvisible";
		
		document.getElementById("box6").style.display = "table-cell";
		document.getElementById("box7").style.display = "table-cell";
		document.getElementById("box8").style.display = "table-cell";
		document.getElementById("box9").style.display = "table-cell";
		document.getElementById("boxa").style.display = "table-cell";
		document.getElementById("boxb").style.display = "table-cell";
		document.getElementById("boxc").style.display = "table-cell";
		
		/*
		document.form.region.options[1] = new Option();
		document.form.region.options[1].value = "nh";
		document.form.region.options[1].text = "N. Hemi.";
		
		document.form.region.options[2] = new Option();
		document.form.region.options[2].value = "sh";
		document.form.region.options[2].text = "S. Hemi.";
		
		document.form.region2.options[1] = new Option();
		document.form.region2.options[1].value = "nh";
		document.form.region2.options[1].text = "N. Hemi.";
		
		document.form.region2.options[2] = new Option();
		document.form.region2.options[2].value = "sh";
		document.form.region2.options[2].text = "S. Hemi.";
		*/
		
		// add all sensor 
		document.form.sat.options[7] = new Option();
		document.form.sat.options[7].value = "allSensors";
		document.form.sat.options[7].text = "All Sensors";
		
		document.form.sat2.options[7] = new Option();
		document.form.sat2.options[7].value = "allSensors";
		document.form.sat2.options[7].text = "All Sensors";
		
		document.form.stat.options.length = 0;
		for(var i=0; i<values_ts.length; i++ ) {
			document.form.stat.options[i] = new Option();
			document.form.stat.options[i].value = values_ts[i];
			document.form.stat.options[i].text = texts_ts[i];
		}
		
		document.form.stat2.options.length = 0;
		for(var i=0; i<values_ts.length; i++ ) {
			document.form.stat2.options[i] = new Option();
			document.form.stat2.options[i].value = values_ts[i];
			document.form.stat2.options[i].text = texts_ts[i];
		}
	}
	
	else if ( plotval == "map" ) {
		
		document.form.sat.options.length = 7;
		document.form.sat2.options.length = 7;
		
		document.getElementById("stat").className  = "optioninvisible";
		document.getElementById("stat2").className = "optioninvisible";
		
		document.getElementById("view").className  = "optionvisible";
		document.getElementById("view2").className = "optionvisible";
		
		document.getElementById("region").className  = "optionvisible";
		document.getElementById("region2").className = "optionvisible";
		
		
		//document.form.region.options.length = 1;
		//document.form.region2.options.length = 1;
		
		document.getElementById("region").className  = "optioninvisible";
		document.getElementById("region2").className = "optioninvisible";
		
		
		//document.getElementById("box3").style.display = "table-cell";
		//document.getElementById("box4").style.display = "table-cell";
		//document.getElementById("box5").style.display = "table-cell";
		document.getElementById("box6").style.display = "none";
		document.getElementById("box7").style.display = "none";
		document.getElementById("box8").style.display = "none";
		document.getElementById("box9").style.display = "none";
		document.getElementById("boxa").style.display = "none";
		document.getElementById("boxb").style.display = "none";
		document.getElementById("boxc").style.display = "none";

	}
	
	else if ( plotval == "his" ) {

		document.form.sat.options.length = 7;
		document.form.sat2.options.length = 7;
	
		document.getElementById("stat").className  = "optioninvisible";
		document.getElementById("stat2").className = "optioninvisible";
		
		document.getElementById("view").className  = "optioninvisible";
		document.getElementById("view2").className = "optioninvisible";
		
		document.getElementById("region").className  = "optioninvisible";
		document.getElementById("region2").className = "optioninvisible";
		
		document.getElementById("box6").style.display = "table-cell";
		document.getElementById("box7").style.display = "none";
		document.getElementById("box8").style.display = "none";
		document.getElementById("box9").style.display = "none";
		document.getElementById("boxa").style.display = "none";
		document.getElementById("boxb").style.display = "none";
		document.getElementById("boxc").style.display = "none";

	}

}


function changeStat( statval ) {}


function loadImage() {
    
	sat    = document.form.sat.value;
 	plot   = document.form.plot.value;
 	stat   = document.form.stat.value;
 	prod   = "sice";
	
	region = document.form.region.value;
	view   = document.form.view.value;
	cend   = document.form.cend.value;
	yr     = document.form.yr.value;
	mo     = document.form.mo.value;
	dy     = document.form.dy.value;
	
	// update bottom
	document.form.sat2.value    =  sat   ; 
 	document.form.plot2.value   =  plot  ; 
 	document.form.stat2.value   =  stat  ; 
 	//document.form.prod2.value   =  prod  ; 
	
	document.form.region2.value = region ; 
	document.form.view2.value   =  view  ; 
	document.form.cend2.value   =  cend  ; 
	document.form.yr2.value     =    yr  ; 
	document.form.mo2.value     =    mo  ; 
	document.form.dy2.value     =    dy  ; 

	loadImageHelp(sat,plot,stat,prod,region,view,cend,yr,mo,dy);
	
}


function loadImage2() {
    
	sat    = document.form.sat2.value;
 	plot   = document.form.plot2.value;
 	stat   = document.form.stat2.value;
 	prod   = "sice";
	
	region = document.form.region2.value;
	view   = document.form.view2.value;
	cend   = document.form.cend2.value;
	yr     = document.form.yr2.value;
	mo     = document.form.mo2.value;
	dy     = document.form.dy2.value;
	
	// update top
	document.form.sat.value    =   sat  ; 
 	document.form.plot.value   =  plot  ; 
 	document.form.stat.value   =  stat  ; 
 	//document.form.prod.value   =  prod  ; 
	
	document.form.region.value = region ; 
	document.form.view.value   =  view  ; 
	document.form.cend.value   =  cend  ; 
	document.form.yr.value     =    yr  ; 
	document.form.mo.value     =    mo  ; 
	document.form.dy.value     =    dy  ; 

	loadImageHelp(sat,plot,stat,prod,region,view,cend,yr,mo,dy);
	
}

function loadImagev() {
    
	sat    = document.form.sat.value;
 	plot   = document.form.plot.value;
 	stat   = document.form.stat.value;
 	prod   = "sice";
	
	region = document.form.region.value;
	view   = document.form.view.value;
	cend   = document.form.cend.value;
	yr     = document.form.yr.value;
	mo     = document.form.mo.value;
	dy     = document.form.dy.value;
	
	// update bottom
	document.form.sat2.value    =   sat  ; 
 	document.form.plot2.value   =  plot  ; 
 	document.form.stat2.value   =  stat  ; 
 	//document.form.prod2.value   =  prod  ; 
	
	document.form.region2.value = region ; 
	document.form.view2.value   =  view  ; 
	document.form.cend2.value   =  cend  ; 
	document.form.yr2.value     =    yr  ; 
	document.form.mo2.value     =    mo  ; 
	document.form.dy2.value     =    dy  ; 

	loadImageHelpv(sat,plot,stat,prod,region,view,cend,yr,mo,dy);
	
}


function loadImage2v() {
    
	sat    = document.form.sat2.value;
 	plot   = document.form.plot2.value;
 	stat   = document.form.stat2.value;
 	prod   = "sice";
	
	region = document.form.region2.value;
	view   = document.form.view2.value;
	cend   = document.form.cend2.value;
	yr     = document.form.yr2.value;
	mo     = document.form.mo2.value;
	dy     = document.form.dy2.value;
	
	// update top
	document.form.sat.value    =  sat   ; 
 	document.form.plot.value   =  plot  ; 
 	document.form.stat.value   =  stat  ; 
 	//document.form.prod.value   =  prod  ; 
	
	document.form.region.value = region ; 
	document.form.view.value   =  view  ; 
	document.form.cend.value   =  cend  ; 
	document.form.yr.value     =    yr  ; 
	document.form.mo.value     =    mo  ; 
	document.form.dy.value     =    dy  ; 

	loadImageHelpv(sat,plot,stat,prod,region,view,cend,yr,mo,dy);
	
}


function loadImageHelp(sat1,plot1,stat1,prod1,region1,view1,cend1,yr1,mo1,dy1) {
    
	var ymd1 =  "/" + yr1 + "-" + mo1 + "-" + dy1 + "/";
	
	if ( sat1 == 'f16' || sat1 == 'f18' ) {
		header="mirs_adv_dmsp_";
		sensor="ssmis_";
	}
	else if( sat1 == "npp" ) {
		header="mirs_adv_npoess_";
		sensor="atms_";
	}
	else {
		header="mirs_adv_poes_";
		sensor="amsuamhs_";
	}
	
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
	var imgb = "";	
	var imgc = "";	
	
	var img1_big = "";
	var img2_big = "";
	var img3_big = "";
	var img4_big = "";	
	var img5_big = "";	
	var img6_big = "";	
	var img7_big = "";	
	var img8_big = "";	
	var img9_big = "";	
	var imga_big = "";	
	var imgb_big = "";	
	var imgc_big = "";	
	
  	var sfc1 = "sea";
	var region2 = "glb";
	
	imgDir1 = imgDirs[sat1];
	
	if ( plot1 == "map" ) {
		img1 = imgDir1+sat1+ymd1+header+sat1+dash+sensor+region2+dash+yr1+mo1+dy1+dash+prod1+dash+view1+dash+sfc1+dash+cend1+".png";
  		img2 = imgDir+sat1+ymd1+"valid_f17ssmis25_"+region2+dash+yr1+mo1+dy1+dash+prod1+"_nt2_"+view1+dash+sfc1+"_dy.png";
		img3 = imgDir+sat1+ymd1+"valid_"+sat1+"_f17ssmis25_bias_"+region2+dash+yr1+mo1+dy1+dash+prod1+"_nt2dy_"+view1+dash+sfc1+dash+cend1+".png";
  		img4 = imgDir+sat1+ymd1+"valid_"+sat1+"_f17ssmis25_"+region2+"_p2p_"+yr1+mo1+dy1+dash+prod1+"_nt2dy_"+view1+dash+"all"+dash+cend1+".png";
  		img5 = imgDir+sat1+ymd1+"valid_"+sat1+"_f17ssmis25_"+region2+"_p2p_"+yr1+mo1+dy1+dash+prod1+"_nt2dy_"+view1+dash+"ice"+dash+cend1+".png";
	}
	else if ( plot1 == "his" ) {
		img1 = imgDir+sat1+ymd1+"histo_"+sat1+dash+yr1+mo1+dy1+dash+prod1+"_glb_as.png";
  		img2 = imgDir+sat1+ymd1+"histo_"+sat1+dash+yr1+mo1+dy1+dash+prod1+"_glb_ds.png";
		img3 = imgDir+sat1+ymd1+"histo_"+sat1+dash+yr1+mo1+dy1+dash+prod1+"_nh_as.png";
  		img4 = imgDir+sat1+ymd1+"histo_"+sat1+dash+yr1+mo1+dy1+dash+prod1+"_nh_ds.png";
		img5 = imgDir+sat1+ymd1+"histo_"+sat1+dash+yr1+mo1+dy1+dash+prod1+"_sh_as.png";
  		img6 = imgDir+sat1+ymd1+"histo_"+sat1+dash+yr1+mo1+dy1+dash+prod1+"_sh_ds.png";
	}
	else if ( plot1 == "ts" ) {
		
		img1 = imgDir+sat1+'/timeseries/scores_'+sat1+"_f17ssmis25_sice_nt2dy_"+region1+dash+stat1+dash+cend1+".png";
		img1_big = imgDir+sat1+'/timeseries/scores_'+sat1+"_f17ssmis25_sice_nt2dy_"+region1+dash+stat1+dash+cend1+"_big.png";
  		
		var values_tmp = new Array();
		var j=0;
		for( var i=0; i<values_ts.length; i++ ) {
			if( stat1 != values_ts[i] ) {
				values_tmp[j] = values_ts[i];
				j++;	
			}
		}

		img2 = imgDir+sat1+'/timeseries/scores_'+sat1+"_f17ssmis25_sice_nt2dy_"+region1+dash+values_tmp[0]+dash+cend1+".png";
		img3 = imgDir+sat1+'/timeseries/scores_'+sat1+"_f17ssmis25_sice_nt2dy_"+region1+dash+values_tmp[1]+dash+cend1+".png";
  		img4 = imgDir+sat1+'/timeseries/scores_'+sat1+"_f17ssmis25_sice_nt2dy_"+region1+dash+values_tmp[2]+dash+cend1+".png";
		img5 = imgDir+sat1+'/timeseries/scores_'+sat1+"_f17ssmis25_sice_nt2dy_"+region1+dash+values_tmp[3]+dash+cend1+".png";
  		img6 = imgDir+sat1+'/timeseries/scores_'+sat1+"_f17ssmis25_sice_nt2dy_"+region1+dash+values_tmp[4]+dash+cend1+".png";
		img7 = imgDir+sat1+'/timeseries/scores_'+sat1+"_f17ssmis25_sice_nt2dy_"+region1+dash+values_tmp[5]+dash+cend1+".png";
  		img8 = imgDir+sat1+'/timeseries/scores_'+sat1+"_f17ssmis25_sice_nt2dy_"+region1+dash+values_tmp[6]+dash+cend1+".png";
		img9 = imgDir+sat1+'/timeseries/scores_'+sat1+"_f17ssmis25_sice_nt2dy_"+region1+dash+values_tmp[7]+dash+cend1+".png";
  		imga = imgDir+sat1+'/timeseries/scores_'+sat1+"_f17ssmis25_sice_nt2dy_"+region1+dash+values_tmp[8]+dash+cend1+".png";
		imgb = imgDir+sat1+'/timeseries/scores_'+sat1+"_f17ssmis25_sice_nt2dy_"+region1+dash+values_tmp[9]+dash+cend1+".png";
  		imgc = imgDir+sat1+'/timeseries/scores_'+sat1+"_f17ssmis25_sice_nt2dy_"+region1+dash+values_tmp[10]+dash+cend1+".png";
		
		img2_big = imgDir+sat1+'/timeseries/scores_'+sat1+"_f17ssmis25_sice_nt2dy_"+region1+dash+values_tmp[0]+dash+cend1+"_big.png";
		img3_big = imgDir+sat1+'/timeseries/scores_'+sat1+"_f17ssmis25_sice_nt2dy_"+region1+dash+values_tmp[1]+dash+cend1+"_big.png";
  		img4_big = imgDir+sat1+'/timeseries/scores_'+sat1+"_f17ssmis25_sice_nt2dy_"+region1+dash+values_tmp[2]+dash+cend1+"_big.png";
		img5_big = imgDir+sat1+'/timeseries/scores_'+sat1+"_f17ssmis25_sice_nt2dy_"+region1+dash+values_tmp[3]+dash+cend1+"_big.png";
  		img6_big = imgDir+sat1+'/timeseries/scores_'+sat1+"_f17ssmis25_sice_nt2dy_"+region1+dash+values_tmp[4]+dash+cend1+"_big.png";
		img7_big = imgDir+sat1+'/timeseries/scores_'+sat1+"_f17ssmis25_sice_nt2dy_"+region1+dash+values_tmp[5]+dash+cend1+"_big.png";
  		img8_big = imgDir+sat1+'/timeseries/scores_'+sat1+"_f17ssmis25_sice_nt2dy_"+region1+dash+values_tmp[6]+dash+cend1+"_big.png";
		img9_big = imgDir+sat1+'/timeseries/scores_'+sat1+"_f17ssmis25_sice_nt2dy_"+region1+dash+values_tmp[7]+dash+cend1+"_big.png";
  		imga_big = imgDir+sat1+'/timeseries/scores_'+sat1+"_f17ssmis25_sice_nt2dy_"+region1+dash+values_tmp[8]+dash+cend1+"_big.png";
		imgb_big = imgDir+sat1+'/timeseries/scores_'+sat1+"_f17ssmis25_sice_nt2dy_"+region1+dash+values_tmp[9]+dash+cend1+"_big.png";
  		imgc_big = imgDir+sat1+'/timeseries/scores_'+sat1+"_f17ssmis25_sice_nt2dy_"+region1+dash+values_tmp[10]+dash+cend1+"_big.png";
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
        document.form.imgb.src = imgb;
        document.form.imgc.src = imgc;
	

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
	
	var indexb = imgb.lastIndexOf("/");
	var altb   = imgb.substring(indexb+1,imgb.length);
	
	var indexc = imgc.lastIndexOf("/");
	var altc   = imgc.substring(indexc+1,imgc.length);
	
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
	document.form.imgb.alt = altb;
	document.form.imgc.alt = altc;
	
	if( plot1 == "ts" ) {
	
		document.getElementById("href1").href = img1_big;
		document.getElementById("href2").href = img2_big;
		document.getElementById("href3").href = img3_big;
		document.getElementById("href4").href = img4_big;
		document.getElementById("href5").href = img5_big;
		document.getElementById("href6").href = img6_big;
		document.getElementById("href7").href = img7_big;
		document.getElementById("href8").href = img8_big;
		document.getElementById("href9").href = img9_big;
		document.getElementById("hrefa").href = imga_big;
		document.getElementById("hrefb").href = imgb_big;
		document.getElementById("hrefc").href = imgc_big;
	}
	else {
		document.getElementById("href1").href = img1;
		document.getElementById("href2").href = img2;
		document.getElementById("href3").href = img3;
		document.getElementById("href4").href = img4;
		document.getElementById("href5").href = img5;
		document.getElementById("href6").href = img6;
		document.getElementById("href7").href = img7;
		document.getElementById("href8").href = img8;
		document.getElementById("href9").href = img9;
		document.getElementById("hrefa").href = imga;
		document.getElementById("hrefb").href = imgb;
		document.getElementById("hrefc").href = imgc;
	}
	
}


function loadImageHelpv(sat1,plot1,stat1,prod1,region1,view1,cend1,yr1,mo1,dy1) {
    
	var ymd1 =  "/" + yr1 + "-" + mo1 + "-" + dy1 + "/";
	
	if ( sat1 == 'f16' || sat1 == 'f18' ) {
		header="mirs_adv_dmsp_";
		sensor="ssmis_";
	}
	else if( sat1 == "npp" ) {
		header="mirs_adv_npoess_";
		sensor="atms_";
	}
	else {
		header="mirs_adv_poes_";
		sensor="amsuamhs_";
	}
	
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
	var imgb = "";	
	var imgc = "";	
	
  	var sfc1 = "sea";
	var region2 = "glb";
	
	imgDir1 = imgDirs[sat1];
	
	if ( plot1 == "map" ) {
		img1 = imgDir1+sat1+ymd1+header+sat1+dash+sensor+region2+dash+yr1+mo1+dy1+dash+prod1+dash+view1+dash+sfc1+dash+cend1+".png";
  		img2 = imgDir+sat1+ymd1+"valid_f17ssmis25_"+region2+dash+yr1+mo1+dy1+dash+prod1+"_nt2_"+view1+dash+sfc1+"_dy.png";
		img3 = imgDir+sat1+ymd1+"valid_"+sat1+"_f17ssmis25_bias_"+region2+dash+yr1+mo1+dy1+dash+prod1+"_nt2dy_"+view1+dash+sfc1+dash+cend1+".png";
  		img4 = imgDir+sat1+ymd1+"valid_"+sat1+"_f17ssmis25_"+region2+"_p2p_"+yr1+mo1+dy1+dash+prod1+"_nt2dy_"+view1+dash+"all"+dash+cend1+".png";
  		img5 = imgDir+sat1+ymd1+"valid_"+sat1+"_f17ssmis25_"+region2+"_p2p_"+yr1+mo1+dy1+dash+prod1+"_nt2dy_"+view1+dash+"ice"+dash+cend1+".png";
	}
	else if ( plot1 == "his" ) {
		img1 = imgDir+sat1+ymd1+"histo_"+sat1+dash+yr1+mo1+dy1+dash+prod1+"_glb_as.png";
  		img2 = imgDir+sat1+ymd1+"histo_"+sat1+dash+yr1+mo1+dy1+dash+prod1+"_glb_ds.png";
		img3 = imgDir+sat1+ymd1+"histo_"+sat1+dash+yr1+mo1+dy1+dash+prod1+"_nh_as.png";
  		img4 = imgDir+sat1+ymd1+"histo_"+sat1+dash+yr1+mo1+dy1+dash+prod1+"_nh_ds.png";
		img5 = imgDir+sat1+ymd1+"histo_"+sat1+dash+yr1+mo1+dy1+dash+prod1+"_sh_as.png";
  		img6 = imgDir+sat1+ymd1+"histo_"+sat1+dash+yr1+mo1+dy1+dash+prod1+"_sh_ds.png";
	}
	else if ( plot1 == "ts" ) {
		
		img1 = imgDir+sat1+'/timeseries/scores_'+sat1+"_f17ssmis25_sice_nt2dy_"+region1+dash+stat1+dash+cend1+"_big.png";
  		
		var values_tmp = new Array();
		var j=0;
		for( var i=0; i<values_ts.length; i++ ) {
			if( stat1 != values_ts[i] ) {
				values_tmp[j] = values_ts[i];
				j++;	
			}
		}

		img2 = imgDir+sat1+'/timeseries/scores_'+sat1+"_f17ssmis25_sice_nt2dy_"+region1+dash+values_tmp[0]+dash+cend1+"_big.png";
		img3 = imgDir+sat1+'/timeseries/scores_'+sat1+"_f17ssmis25_sice_nt2dy_"+region1+dash+values_tmp[1]+dash+cend1+"_big.png";
  		img4 = imgDir+sat1+'/timeseries/scores_'+sat1+"_f17ssmis25_sice_nt2dy_"+region1+dash+values_tmp[2]+dash+cend1+"_big.png";
		img5 = imgDir+sat1+'/timeseries/scores_'+sat1+"_f17ssmis25_sice_nt2dy_"+region1+dash+values_tmp[3]+dash+cend1+"_big.png";
  		img6 = imgDir+sat1+'/timeseries/scores_'+sat1+"_f17ssmis25_sice_nt2dy_"+region1+dash+values_tmp[4]+dash+cend1+"_big.png";
		img7 = imgDir+sat1+'/timeseries/scores_'+sat1+"_f17ssmis25_sice_nt2dy_"+region1+dash+values_tmp[5]+dash+cend1+"_big.png";
  		img8 = imgDir+sat1+'/timeseries/scores_'+sat1+"_f17ssmis25_sice_nt2dy_"+region1+dash+values_tmp[6]+dash+cend1+"_big.png";
		img9 = imgDir+sat1+'/timeseries/scores_'+sat1+"_f17ssmis25_sice_nt2dy_"+region1+dash+values_tmp[7]+dash+cend1+"_big.png";
  		imga = imgDir+sat1+'/timeseries/scores_'+sat1+"_f17ssmis25_sice_nt2dy_"+region1+dash+values_tmp[8]+dash+cend1+"_big.png";
		imgb = imgDir+sat1+'/timeseries/scores_'+sat1+"_f17ssmis25_sice_nt2dy_"+region1+dash+values_tmp[9]+dash+cend1+"_big.png";
  		imgc = imgDir+sat1+'/timeseries/scores_'+sat1+"_f17ssmis25_sice_nt2dy_"+region1+dash+values_tmp[10]+dash+cend1+"_big.png";
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
        document.form.imgb.src = imgb;
        document.form.imgc.src = imgc;
	

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
	
	var indexb = imgb.lastIndexOf("/");
	var altb   = imgb.substring(indexb+1,imgb.length);
	
	var indexc = imgc.lastIndexOf("/");
	var altc   = imgc.substring(indexc+1,imgc.length);
	
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
	document.form.imgb.alt = altb;
	document.form.imgc.alt = altc;

	document.getElementById("href1").href = img1;
	document.getElementById("href2").href = img2;
	document.getElementById("href3").href = img3;
	document.getElementById("href4").href = img4;
	document.getElementById("href5").href = img5;
	document.getElementById("href6").href = img6;
	document.getElementById("href7").href = img7;
	document.getElementById("href8").href = img8;
	document.getElementById("href9").href = img9;
	document.getElementById("hrefa").href = imga;
	document.getElementById("hrefb").href = imgb;
	document.getElementById("hrefc").href = imgc;
}


function loadInit( sat1, plot1, stat1, prod1, region1, view1, cend1, yr1, mo1, dy1 ) {
	
	if ( sat1 == "" ) {
		var now = new Date();
		now.setDate(now.getDate()-ndayback) ; // n day ago

		var year  = now.getFullYear();
		var month = now.getMonth();  // month starting index is 0
		var day   = now.getDate();

		document.form.sat.selectedIndex = 0;
		document.form.plot.selectedIndex = 0;
		//document.form.prod.selectedIndex = 0;
		document.form.stat.selectedIndex = 0;
		document.form.region.selectedIndex = 0;
		document.form.view.selectedIndex = 0;
		document.form.cend.selectedIndex = 0;
		document.form.yr.value = year;
		document.form.mo.selectedIndex = month;
		document.form.dy.selectedIndex = day - 1;

		document.form.sat2.selectedIndex = 0;
		document.form.plot2.selectedIndex = 0;
		//document.form.prod2.selectedIndex = 0;
		document.form.stat2.selectedIndex = 0;
		document.form.region2.selectedIndex = 0;
		document.form.view2.selectedIndex = 0;
		document.form.cend2.selectedIndex = 0;
		document.form.yr2.value = year;
		document.form.mo2.selectedIndex = month;
		document.form.dy2.selectedIndex = day -1;
		
		//document.getElementById("box5").style.display = "none";
		document.getElementById("box6").style.display = "none";
		document.getElementById("box7").style.display = "none";
		document.getElementById("box8").style.display = "none";
		document.getElementById("box9").style.display = "none";
		document.getElementById("boxa").style.display = "none";
		document.getElementById("boxb").style.display = "none";
		document.getElementById("boxc").style.display = "none";
		  
		loadImage();
	}
	else {
		
		document.form.sat.value = sat1;
		//document.form.prod.value = prod1;
		document.form.plot.value = plot1;
		document.form.stat.value = stat1;
		document.form.region.value = region1;
		document.form.view.value = view1;
		document.form.cend.value = cend1;
		document.form.yr.value = yr1;
		document.form.mo.value = mo1;
		document.form.dy.value = dy1;
		
		document.form.sat2.value = sat1;
		//document.form.prod2.value = prod1;
		document.form.plot2.value = plot1;
		document.form.stat2.value = stat1;
		document.form.region2.value = region1;
		document.form.view2.value = view1;
		document.form.cend2.value = cend1;
		document.form.yr2.value = yr1;
		document.form.mo2.value = mo1;
		document.form.dy2.value = dy1;
		

		changePlot(plot1);
		
		loadImageHelp(sat1,plot1,stat1,prod1,region1,view1,cend1,yr1,mo1,dy1);
	}
	
}


function loadInitv( sat1, plot1, stat1, prod1, region1, view1, cend1, yr1, mo1, dy1 ) {
	
	if ( sat1 == "" ) {
		var now = new Date();
		now.setDate(now.getDate()-ndayback) ; // n day ago

		var year  = now.getFullYear();
		var month = now.getMonth();  // month starting index is 0
		var day   = now.getDate();

		document.form.sat.selectedIndex = 0;
		document.form.plot.selectedIndex = 0;
		//document.form.prod.selectedIndex = 0;
		document.form.stat.selectedIndex = 0;
		document.form.region.selectedIndex = 0;
		document.form.view.selectedIndex = 0;
		document.form.cend.selectedIndex = 0;
		document.form.yr.value = year;
		document.form.mo.selectedIndex = month;
		document.form.dy.selectedIndex = day - 1;

		document.form.sat2.selectedIndex = 0;
		document.form.plot2.selectedIndex = 0;
		//document.form.prod2.selectedIndex = 0;
		document.form.stat2.selectedIndex = 0;
		document.form.region2.selectedIndex = 0;
		document.form.view2.selectedIndex = 0;
		document.form.cend2.selectedIndex = 0;
		document.form.yr2.value = year;
		document.form.mo2.selectedIndex = month;
		document.form.dy2.selectedIndex = day -1;
		
		//document.getElementById("box5").style.display = "none";
		document.getElementById("box6").style.display = "none";
		document.getElementById("box7").style.display = "none";
		document.getElementById("box8").style.display = "none";
		document.getElementById("box9").style.display = "none";
		document.getElementById("boxa").style.display = "none";
		document.getElementById("boxb").style.display = "none";
		document.getElementById("boxc").style.display = "none";
		  
		loadImagev();
	}
	else {
		
		document.form.sat.value = sat1;
		//document.form.prod.value = prod1;
		document.form.plot.value = plot1;
		document.form.stat.value = stat1;
		document.form.region.value = region1;
		document.form.view.value = view1;
		document.form.cend.value = cend1;
		document.form.yr.value = yr1;
		document.form.mo.value = mo1;
		document.form.dy.value = dy1;
		
		document.form.sat2.value = sat1;
		//document.form.prod2.value = prod1;
		document.form.plot2.value = plot1;
		document.form.stat2.value = stat1;
		document.form.region2.value = region1;
		document.form.view2.value = view1;
		document.form.cend2.value = cend1;
		document.form.yr2.value = yr1;
		document.form.mo2.value = mo1;
		document.form.dy2.value = dy1;
		

		changePlot(plot1);
		
		loadImageHelpv(sat1,plot1,stat1,prod1,region1,view1,cend1,yr1,mo1,dy1);
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
