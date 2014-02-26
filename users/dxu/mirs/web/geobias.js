var IMGPATH = "images/" ;
var IMGPRE  = "mirs_adv_poes_" ;

function loadImage() {
     	
	var sat1 = document.form.sat1.value;
	var sat2 = document.form.sat2.value;
	var sat3 = document.form.sat3.value;
	var sat4 = document.form.sat4.value;
	
	var prod1 = document.form.prod1.value;
	var prod2 = document.form.prod2.value;
	var prod3 = document.form.prod3.value;
	var prod4 = document.form.prod4.value;
	
	var param1 = document.form.param1.value;
	var param2 = document.form.param2.value;
	var param3 = document.form.param3.value;
	var param4 = document.form.param4.value;
	
	var sfc1 = document.form.sfc1.value;
	var sfc2 = document.form.sfc2.value;
	var sfc3 = document.form.sfc3.value;
	var sfc4 = document.form.sfc4.value;
	
	var nwp1 = document.form.nwp1.value;
	var nwp2 = document.form.nwp2.value;
	var nwp3 = document.form.nwp3.value;
	var nwp4 = document.form.nwp4.value;
	
	var imgDir1 = IMGPATH + sat1 + "/bias/";
	var imgDir2 = IMGPATH + sat2 + "/bias/";
	var imgDir3 = IMGPATH + sat3 + "/bias/";
	var imgDir4 = IMGPATH + sat4 + "/bias/";
	
 	var img1 = imgDir1+IMGPRE + sat1 + "_" + nwp1 + "_" + param1 + "_glb_" + prod1 + "_" + sfc1 + ".png" ;
 	var img2 = imgDir2+IMGPRE + sat2 + "_" + nwp2 + "_" + param2 + "_glb_" + prod2 + "_" + sfc2 + ".png" ;
 	var img3 = imgDir3+IMGPRE + sat3 + "_" + nwp3 + "_" + param3 + "_glb_" + prod3 + "_" + sfc3 + ".png" ;
 	var img4 = imgDir4+IMGPRE + sat4 + "_" + nwp4 + "_" + param4 + "_glb_" + prod4 + "_" + sfc4 + ".png" ;

 	var img1_big = imgDir1+IMGPRE + sat1 + "_" + nwp1 + "_" + param1 + "_glb_" + prod1 + "_"  + sfc1 + "_big.png" ;
 	var img2_big = imgDir2+IMGPRE + sat2 + "_" + nwp2 + "_" + param2 + "_glb_" + prod2 + "_"  + sfc2 + "_big.png" ;
 	var img3_big = imgDir3+IMGPRE + sat3 + "_" + nwp3 + "_" + param3 + "_glb_" + prod3 + "_"  + sfc3 + "_big.png" ;
 	var img4_big = imgDir4+IMGPRE + sat4 + "_" + nwp4 + "_" + param4 + "_glb_" + prod4 + "_"  + sfc4 + "_big.png" ;

	document.form.img1.src = img1;
	document.form.img2.src = img2;
	document.form.img3.src = img3;
	document.form.img4.src = img4;
	
	document.form.img1.alt = img1;
	document.form.img2.alt = img2;
	document.form.img3.alt = img3;
	document.form.img4.alt = img4;
	
	//document.getElementById("href1").href = img1_big;
	//document.getElementById("href2").href = img2_big;
	//document.getElementById("href3").href = img3_big;
	//document.getElementById("href4").href = img4_big;
	document.getElementById("href1").href = img1;
	document.getElementById("href2").href = img2;
	document.getElementById("href3").href = img3;
	document.getElementById("href4").href = img4;
	
}


function loadImageSmall() {
     	
	var sat1 = document.form.sat1.value;
	var sat2 = document.form.sat2.value;
	var sat3 = document.form.sat3.value;
	var sat4 = document.form.sat4.value;
	
	var prod1 = document.form.prod1.value;
	var prod2 = document.form.prod2.value;
	var prod3 = document.form.prod3.value;
	var prod4 = document.form.prod4.value;
	
	var param1 = document.form.param1.value;
	var param2 = document.form.param2.value;
	var param3 = document.form.param3.value;
	var param4 = document.form.param4.value;
	
	var sfc1 = document.form.sfc1.value;
	var sfc2 = document.form.sfc2.value;
	var sfc3 = document.form.sfc3.value;
	var sfc4 = document.form.sfc4.value;
	
	var nwp1 = document.form.nwp1.value;
	var nwp2 = document.form.nwp2.value;
	var nwp3 = document.form.nwp3.value;
	var nwp4 = document.form.nwp4.value;
	
	var imgDir1 = IMGPATH + sat1 + "/bias/";
	var imgDir2 = IMGPATH + sat2 + "/bias/";
	var imgDir3 = IMGPATH + sat3 + "/bias/";
	var imgDir4 = IMGPATH + sat4 + "/bias/";
	
 	var img1 = imgDir1+IMGPRE + sat1 + "_" + nwp1 + "_" + param1 + "_glb_" + prod1 + "_" + sfc1 + ".png" ;
 	var img2 = imgDir2+IMGPRE + sat2 + "_" + nwp2 + "_" + param2 + "_glb_" + prod2 + "_" + sfc2 + ".png" ;
 	var img3 = imgDir3+IMGPRE + sat3 + "_" + nwp3 + "_" + param3 + "_glb_" + prod3 + "_" + sfc3 + ".png" ;
 	var img4 = imgDir4+IMGPRE + sat4 + "_" + nwp4 + "_" + param4 + "_glb_" + prod4 + "_" + sfc4 + ".png" ;

 	var img1_small = imgDir1+IMGPRE + sat1 + "_" + nwp1 + "_" + param1 + "_glb_" + prod1 + "_"  + sfc1 + "_small.png" ;
 	var img2_small = imgDir2+IMGPRE + sat2 + "_" + nwp2 + "_" + param2 + "_glb_" + prod2 + "_"  + sfc2 + "_small.png" ;
 	var img3_small = imgDir3+IMGPRE + sat3 + "_" + nwp3 + "_" + param3 + "_glb_" + prod3 + "_"  + sfc3 + "_small.png" ;
 	var img4_small = imgDir4+IMGPRE + sat4 + "_" + nwp4 + "_" + param4 + "_glb_" + prod4 + "_"  + sfc4 + "_small.png" ;

	document.form.img1.src = img1_small;
	document.form.img2.src = img2_small;
	document.form.img3.src = img3_small;
	document.form.img4.src = img4_small;
	
	document.form.img1.alt = img1_small;
	document.form.img2.alt = img2_small;
	document.form.img3.alt = img3_small;
	document.form.img4.alt = img4_small;
	
	document.getElementById("href1").href = img1;
	document.getElementById("href2").href = img2;
	document.getElementById("href3").href = img3;
	document.getElementById("href4").href = img4;
	
}


function loadInitialImageEmpty() {
     	
	var sat = document.form.sat1.value;
	var nwp = document.form.nwp1.value;
	
	document.form.sfc1.selectedIndex=0;
	document.form.sfc2.selectedIndex=1;
	document.form.sfc3.selectedIndex=2;
	document.form.sfc4.selectedIndex=3;
	
	var sfc1 = document.form.sfc1.value;
	var sfc2 = document.form.sfc2.value;
	var sfc3 = document.form.sfc3.value;
	var sfc4 = document.form.sfc4.value;
	
	var imgDir = IMGPATH + sat + "/bias/";
	
 	var img1 = imgDir+IMGPRE + sat + "_gdas_biasmean_glb_temp_" + sfc1 + ".png" ;
 	var img2 = imgDir+IMGPRE + sat + "_gdas_biasmean_glb_temp_" + sfc2 + ".png" ;
 	var img3 = imgDir+IMGPRE + sat + "_gdas_biasmean_glb_temp_" + sfc3 + ".png" ;
 	var img4 = imgDir+IMGPRE + sat + "_gdas_biasmean_glb_temp_" + sfc4 + ".png" ;
 	
	var img1_big = imgDir+IMGPRE + sat + "_gdas_biasmean_glb_temp_" + sfc1 + "_big.png" ;
 	var img2_big = imgDir+IMGPRE + sat + "_gdas_biasmean_glb_temp_" + sfc2 + "_big.png" ;
 	var img3_big = imgDir+IMGPRE + sat + "_gdas_biasmean_glb_temp_" + sfc3 + "_big.png" ;
 	var img4_big = imgDir+IMGPRE + sat + "_gdas_biasmean_glb_temp_" + sfc4 + "_big.png" ;
	
	document.form.img1.src = img1;
	document.form.img2.src = img2;
	document.form.img3.src = img3;
	document.form.img4.src = img4;
	
	document.form.img1.alt = img1;
	document.form.img2.alt = img2;
	document.form.img3.alt = img3;
	document.form.img4.alt = img4;
	
	//document.getElementById("href1").href = img1_big;
	//document.getElementById("href2").href = img2_big;
	//document.getElementById("href3").href = img3_big;
	//document.getElementById("href4").href = img4_big;
	
	document.getElementById("href1").href = img1;
	document.getElementById("href2").href = img2;
	document.getElementById("href3").href = img3;
	document.getElementById("href4").href = img4;
	
}


function loadInitialImageEmptySmall() {
     	
	var sat = document.form.sat1.value;
	var nwp = document.form.nwp1.value;
	
	document.form.sfc1.selectedIndex=0;
	document.form.sfc2.selectedIndex=1;
	document.form.sfc3.selectedIndex=2;
	document.form.sfc4.selectedIndex=3;
	
	var sfc1 = document.form.sfc1.value;
	var sfc2 = document.form.sfc2.value;
	var sfc3 = document.form.sfc3.value;
	var sfc4 = document.form.sfc4.value;
	
	var imgDir = IMGPATH + sat + "/bias/";
	
 	var img1 = imgDir+IMGPRE + sat + "_gdas_biasmean_glb_temp_" + sfc1 + ".png" ;
 	var img2 = imgDir+IMGPRE + sat + "_gdas_biasmean_glb_temp_" + sfc2 + ".png" ;
 	var img3 = imgDir+IMGPRE + sat + "_gdas_biasmean_glb_temp_" + sfc3 + ".png" ;
 	var img4 = imgDir+IMGPRE + sat + "_gdas_biasmean_glb_temp_" + sfc4 + ".png" ;
 	
	var img1_small = imgDir+IMGPRE + sat + "_gdas_biasmean_glb_temp_" + sfc1 + "_small.png" ;
 	var img2_small = imgDir+IMGPRE + sat + "_gdas_biasmean_glb_temp_" + sfc2 + "_small.png" ;
 	var img3_small = imgDir+IMGPRE + sat + "_gdas_biasmean_glb_temp_" + sfc3 + "_small.png" ;
 	var img4_small = imgDir+IMGPRE + sat + "_gdas_biasmean_glb_temp_" + sfc4 + "_small.png" ;
	
	document.form.img1.src = img1_small;
	document.form.img2.src = img2_small;
	document.form.img3.src = img3_small;
	document.form.img4.src = img4_small;
	
	document.form.img1.alt = img1_small;
	document.form.img2.alt = img2_small;
	document.form.img3.alt = img3_small;
	document.form.img4.alt = img4_small;
	
	document.getElementById("href1").href = img1;
	document.getElementById("href2").href = img2;
	document.getElementById("href3").href = img3;
	document.getElementById("href4").href = img4;
	
}


function loadInitialImage(sat1,prod1,param1,sfc1,nwp1, 
			  sat2,prod2,param2,sfc2,nwp2,
			  sat3,prod3,param3,sfc3,nwp3,
			  sat4,prod4,param4,sfc4,nwp4) {
     	
	if ( sat1 != '' ) {
	
		document.form.sat1.value = sat1;
		document.form.sat2.value = sat2;
		document.form.sat3.value = sat3;
		document.form.sat4.value = sat4;
		
		document.form.prod1.value = prod1;
		document.form.prod2.value = prod2;
		document.form.prod3.value = prod3;
		document.form.prod4.value = prod4;
		
		document.form.param1.value = param1;
		document.form.param2.value = param2;
		document.form.param3.value = param3;
		document.form.param4.value = param4;

		document.form.sfc1.value = sfc1;
		document.form.sfc2.value = sfc2;
		document.form.sfc3.value = sfc3;
		document.form.sfc4.value = sfc4;
		
		document.form.nwp1.value = nwp1;
		document.form.nwp2.value = nwp2;
		document.form.nwp3.value = nwp3;
		document.form.nwp4.value = nwp4;
		
		var imgDir1 = IMGPATH + sat1 + "/bias/";
		var imgDir2 = IMGPATH + sat2 + "/bias/";
		var imgDir3 = IMGPATH + sat3 + "/bias/";
		var imgDir4 = IMGPATH + sat4 + "/bias/";

 		var img1 = imgDir1+IMGPRE + sat1 + "_" + nwp1 + "_" + param1 + "_glb_" + prod1 + "_" + sfc1 + ".png" ;
 		var img2 = imgDir2+IMGPRE + sat2 + "_" + nwp2 + "_" + param2 + "_glb_" + prod2 + "_" + sfc2 + ".png" ;
 		var img3 = imgDir3+IMGPRE + sat3 + "_" + nwp3 + "_" + param3 + "_glb_" + prod3 + "_" + sfc3 + ".png" ;
 		var img4 = imgDir4+IMGPRE + sat4 + "_" + nwp4 + "_" + param4 + "_glb_" + prod4 + "_" + sfc4 + ".png" ;

 		var img1_big = imgDir1+IMGPRE + sat1 + "_" + nwp1 + "_" + param1 + "_glb_" + prod1 + "_"  + sfc1 + "_big.png" ;
 		var img2_big = imgDir2+IMGPRE + sat2 + "_" + nwp2 + "_" + param2 + "_glb_" + prod2 + "_"  + sfc2 + "_big.png" ;
 		var img3_big = imgDir3+IMGPRE + sat3 + "_" + nwp3 + "_" + param3 + "_glb_" + prod3 + "_"  + sfc3 + "_big.png" ;
 		var img4_big = imgDir4+IMGPRE + sat4 + "_" + nwp4 + "_" + param4 + "_glb_" + prod4 + "_"  + sfc4 + "_big.png" ;

		document.form.img1.src = img1;
		document.form.img2.src = img2;
		document.form.img3.src = img3;
		document.form.img4.src = img4;

		document.form.img1.alt = img1;
		document.form.img2.alt = img2;
		document.form.img3.alt = img3;
		document.form.img4.alt = img4;
		
		//document.getElementById("href1").href = img1_big;
		//document.getElementById("href2").href = img2_big;
		//document.getElementById("href3").href = img3_big;
		//document.getElementById("href4").href = img4_big;
		document.getElementById("href1").href = img1;
		document.getElementById("href2").href = img2;
		document.getElementById("href3").href = img3;
		document.getElementById("href4").href = img4;
	}
	
	else {
	
		loadInitialImageEmpty() ;
	}
	
}


function loadInitialImageSmall(sat1,prod1,param1,sfc1,nwp1, 
			  sat2,prod2,param2,sfc2,nwp2,
			  sat3,prod3,param3,sfc3,nwp3,
			  sat4,prod4,param4,sfc4,nwp4) {
     	
	if ( sat1 != '' ) {
	
		document.form.sat1.value = sat1;
		document.form.sat2.value = sat2;
		document.form.sat3.value = sat3;
		document.form.sat4.value = sat4;
		
		document.form.prod1.value = prod1;
		document.form.prod2.value = prod2;
		document.form.prod3.value = prod3;
		document.form.prod4.value = prod4;
		
		document.form.param1.value = param1;
		document.form.param2.value = param2;
		document.form.param3.value = param3;
		document.form.param4.value = param4;

		document.form.sfc1.value = sfc1;
		document.form.sfc2.value = sfc2;
		document.form.sfc3.value = sfc3;
		document.form.sfc4.value = sfc4;
		
		document.form.nwp1.value = nwp1;
		document.form.nwp2.value = nwp2;
		document.form.nwp3.value = nwp3;
		document.form.nwp4.value = nwp4;
		
		var imgDir1 = IMGPATH + sat1 + "/bias/";
		var imgDir2 = IMGPATH + sat2 + "/bias/";
		var imgDir3 = IMGPATH + sat3 + "/bias/";
		var imgDir4 = IMGPATH + sat4 + "/bias/";

 		var img1 = imgDir1+IMGPRE + sat1 + "_" + nwp1 + "_" + param1 + "_glb_" + prod1 + "_" + sfc1 + ".png" ;
 		var img2 = imgDir2+IMGPRE + sat2 + "_" + nwp2 + "_" + param2 + "_glb_" + prod2 + "_" + sfc2 + ".png" ;
 		var img3 = imgDir3+IMGPRE + sat3 + "_" + nwp3 + "_" + param3 + "_glb_" + prod3 + "_" + sfc3 + ".png" ;
 		var img4 = imgDir4+IMGPRE + sat4 + "_" + nwp4 + "_" + param4 + "_glb_" + prod4 + "_" + sfc4 + ".png" ;

 		var img1_small = imgDir1+IMGPRE + sat1 + "_" + nwp1 + "_" + param1 + "_glb_" + prod1 + "_"  + sfc1 + "_small.png" ;
 		var img2_small = imgDir2+IMGPRE + sat2 + "_" + nwp2 + "_" + param2 + "_glb_" + prod2 + "_"  + sfc2 + "_small.png" ;
 		var img3_small = imgDir3+IMGPRE + sat3 + "_" + nwp3 + "_" + param3 + "_glb_" + prod3 + "_"  + sfc3 + "_small.png" ;
 		var img4_small = imgDir4+IMGPRE + sat4 + "_" + nwp4 + "_" + param4 + "_glb_" + prod4 + "_"  + sfc4 + "_small.png" ;

		document.form.img1.src = img1_small;
		document.form.img2.src = img2_small;
		document.form.img3.src = img3_small;
		document.form.img4.src = img4_small;

		document.form.img1.alt = img1_small;
		document.form.img2.alt = img2_small;
		document.form.img3.alt = img3_small;
		document.form.img4.alt = img4_small;
		
		document.getElementById("href1").href = img1;
		document.getElementById("href2").href = img2;
		document.getElementById("href3").href = img3;
		document.getElementById("href4").href = img4;
	}
	
	else {
	
		loadInitialImageEmptySmall() ;
	}
	
}


function fwd( sfc_obj ) {
	
	var sfc = sfc_obj.value ;
	if      ( sfc == "sea" ) { sfc_obj.value = "lnd"; }
	else if ( sfc == "lnd" ) { sfc_obj.value = "ice"; }
	else if ( sfc == "ice" ) { sfc_obj.value = "snw"; }
	else if ( sfc == "snw" ) { sfc_obj.value = "sea"; }
	
	//loadImage();
}

function rev( sfc_obj ) {

	var sfc = sfc_obj.value ;
	if      ( sfc == "sea" ) { sfc_obj.value = "snw"; }
	else if ( sfc == "snw" ) { sfc_obj.value = "ice"; }
	else if ( sfc == "ice" ) { sfc_obj.value = "lnd"; }
	else if ( sfc == "lnd" ) { sfc_obj.value = "sea"; }
	
	//loadImage();
}





