var IMGDIR = "images/ipwg/";
var prefix = "ipwg_time_series_MW_Gauge_"

function loadImage() {

	var region = document.form.region.value;
	
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
	var imgd = "";
	var imge = "";
	
	if( region == "Australia" ) {
		img1 = IMGDIR+prefix+region+"_BIA.png"
		img2 = IMGDIR+prefix+region+"_COR.png"
		img3 = IMGDIR+prefix+region+"_ETS.png"
		img4 = IMGDIR+prefix+region+"_FAR.png"
		img5 = IMGDIR+prefix+region+"_HSS.png"
		img6 = IMGDIR+prefix+region+"_MAE.png"
		img7 = IMGDIR+prefix+region+"_POD.png"
		img8 = IMGDIR+prefix+region+"_POFD.png";
		img9 = IMGDIR+prefix+region+"_RMSE.png";
	}
	else if( region == "CONUS" ) {
		img1 = IMGDIR+prefix+region+"_BIA.png"
		img2 = IMGDIR+prefix+region+"_BIR.png"
		img3 = IMGDIR+prefix+region+"_COR.png"
		img4 = IMGDIR+prefix+region+"_ETS.png"
		img5 = IMGDIR+prefix+region+"_FAR.png"
		img6 = IMGDIR+prefix+region+"_HSS.png"
		img7 = IMGDIR+prefix+region+"_MAE.png"
		img8 = IMGDIR+prefix+region+"_MEANRRA.png";
		img9 = IMGDIR+prefix+region+"_POD.png"
		imga = IMGDIR+prefix+region+"_POFD.png";
		imgb = IMGDIR+prefix+region+"_RMSE.png";
	}
	else if( region == "SouthAme" ) {
		img1 = IMGDIR+prefix+region+"_BIA.png"
		img2 = IMGDIR+prefix+region+"_COR.png"
		img3 = IMGDIR+prefix+region+"_ETS.png"
		img4 = IMGDIR+prefix+region+"_FAR.png"
		img5 = IMGDIR+prefix+region+"_HSS.png"
		img6 = IMGDIR+prefix+region+"_MAE.png"
		img7 = IMGDIR+prefix+region+"_POD.png"
		img8 = IMGDIR+prefix+region+"_POFD.png";
		img9 = IMGDIR+prefix+region+"_RMSE.png";
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
	
	var index = img1.lastIndexOf("/");
	var alt = img1.substring(index+1,img1.length);
	document.form.img1.alt = alt;

	var index = img2.lastIndexOf("/");
	var alt = img2.substring(index+1,img2.length);
	document.form.img2.alt = alt;

	var index = img3.lastIndexOf("/");
	var alt = img3.substring(index+1,img3.length);
	document.form.img3.alt = alt;

	var index = img4.lastIndexOf("/");
	var alt = img4.substring(index+1,img4.length);
	document.form.img4.alt = alt;

	var index = img5.lastIndexOf("/");
	var alt = img5.substring(index+1,img5.length);
	document.form.img5.alt = alt;

	var index = img6.lastIndexOf("/");
	var alt = img6.substring(index+1,img6.length);
	document.form.img6.alt = alt;
	
	var index = img7.lastIndexOf("/");
	var alt = img7.substring(index+1,img7.length);
	document.form.img7.alt = alt;

	var index = img8.lastIndexOf("/");
	var alt = img8.substring(index+1,img8.length);
	document.form.img8.alt = alt;

	var index = img9.lastIndexOf("/");
	var alt = img9.substring(index+1,img9.length);
	document.form.img9.alt = alt;

	var index = imga.lastIndexOf("/");
	var alt = imga.substring(index+1,imga.length);
	document.form.imga.alt = alt;

	var index = imgb.lastIndexOf("/");
	var alt = imgb.substring(index+1,imgb.length);
	document.form.imgb.alt = alt;

	if( region == "Australia" || region == "SouthAme" ) {
	
		document.form.img1.title = "Bias";
		document.form.img2.title = "Spatial Correlation Coefficient";
		document.form.img3.title = "Equitable Threat Score";
		document.form.img4.title = "False Alarm Ratio";
		document.form.img5.title = "Heidke Skill Score";
		document.form.img6.title = "Mean Absolute Error";
		document.form.img7.title = "Probalility of Detection";
		document.form.img8.title = "False Alarm Rate";
		document.form.img9.title = "Root Mean Square Error";
		
		document.getElementById("boxa").style.display  = "none";
		document.getElementById("boxb").style.display  = "none";
	}
	else {
		document.form.img1.title = "Bias";
		document.form.img2.title = "Bias Ratio";
		document.form.img3.title = "Spatial Correlation Coefficient";
		document.form.img4.title = "Equitable Threat Score";
		document.form.img5.title = "False Alarm Ratio";
		document.form.img6.title = "Heidke Skill Score";
		document.form.img7.title = "Mean Absolute Error";
		document.form.img8.title = "Mean Rain Rate";
		document.form.img9.title = "Probalility of Detection";
		document.form.imga.title = "False Alarm Rate";
		document.form.imgb.title = "Root Mean Square Error";
		
		document.getElementById("boxa").style.display  = "table-cell";
		document.getElementById("boxb").style.display  = "table-cell";
	}


}
