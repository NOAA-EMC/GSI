var IMGPATH = "images/" ;

var IMGPRE = "mirs_adv_poes_" ;
var CHAN_N18 = new Array("23v","31v","50v","52v","53h","54h","54v","55h","57h1","57h2","57h3","57h4","57h5","57h6","89v1","89v2","157h","184h","186h","190v");
var CHAN_F16 = new Array("50v","52v","53v","54v","55v","57rc","59rc","150h","190h","186h","184h","19h","19v","22v","37h","37v","91v","91h","63rc","60rc1","60rc2","60rc3","60rc4","60rc5");
var CHAN_F18 = new Array("50h","52h","53h","54h","55h","57rc","59rc","150h","190h","186h","184h","19h","19v","22v","37h","37v","91v","91h","63rc","60rc1","60rc2","60rc3","60rc4","60rc5");
var CHAN_NPP = new Array("23v","31v","50h","51h","52h","53h","54h1","54h2","55h","57h1","57h2","57h3","57h4","57h5","57h6","88v","165h","183h1","183h2","183h3","183h4","183h5");

function changeRef( ref ) {}


function changeSensor(sat, chan_obj) 
{
	chan_obj.options.length = 0; 
	
	if ( sat == "f16" ) {
		for ( var i=0; i<CHAN_F16.length; i++ ) {
			chan_obj.options[i] = new Option();
			chan_obj.options[i].value = String(i+1);
			chan_obj.options[i].text  = String(i+1)+":"+CHAN_F16[i];
		}
	
	}
	else if( sat == "f18" ) {
		for ( var i=0; i<CHAN_F18.length; i++ ) {
			chan_obj.options[i] = new Option();
			chan_obj.options[i].value = String(i+1);
			chan_obj.options[i].text  = String(i+1)+":"+CHAN_F18[i];
		}
	
	}
	else if( sat == "npp" ) {
		for ( var i=0; i<CHAN_NPP.length; i++ ) {
			chan_obj.options[i] = new Option();
			chan_obj.options[i].value = String(i+1);
			chan_obj.options[i].text  = String(i+1)+":"+CHAN_NPP[i];
		}
	
	}
	else {
		for ( var i=0; i<CHAN_N18.length; i++ ) {
			chan_obj.options[i] = new Option();
			chan_obj.options[i].value = String(i+1);
			chan_obj.options[i].text  = String(i+1)+":"+CHAN_N18[i];
		}
	}
}


function loadImage() 
{
	var ref1 = document.form.ref1.value;
	var ref2 = document.form.ref2.value;
	var ref3 = document.form.ref3.value;
	var ref4 = document.form.ref4.value;
	
	var sat1 = document.form.sat1.value;
	var sat2 = document.form.sat2.value;
	var sat3 = document.form.sat3.value;
	var sat4 = document.form.sat4.value;
	
	var param1 = document.form.param1.value;
	var param2 = document.form.param2.value;
	var param3 = document.form.param3.value;
	var param4 = document.form.param4.value;
	
	var chan1 = document.form.chan1.value;
	var chan2 = document.form.chan2.value;
	var chan3 = document.form.chan3.value;
	var chan4 = document.form.chan4.value;
	
	var imgDir1 = IMGPATH + sat1 + "/bias/";
	var imgDir2 = IMGPATH + sat2 + "/bias/";
	var imgDir3 = IMGPATH + sat3 + "/bias/";
	var imgDir4 = IMGPATH + sat4 + "/bias/";
	
 	var img1 = imgDir1+IMGPRE + sat1 + "_" + ref1 + "_" + param1 + "_glb_tb_ch" + chan1 + ".png" ;
 	var img2 = imgDir2+IMGPRE + sat2 + "_" + ref2 + "_" + param2 + "_glb_tb_ch" + chan2 + ".png" ;
 	var img3 = imgDir3+IMGPRE + sat3 + "_" + ref3 + "_" + param3 + "_glb_tb_ch" + chan3 + ".png" ;
 	var img4 = imgDir4+IMGPRE + sat4 + "_" + ref4 + "_" + param4 + "_glb_tb_ch" + chan4 + ".png" ;

 	var img1_big = imgDir1+IMGPRE + sat1 + "_" + ref1 + "_" + param1 + "_glb_tb_ch" + chan1 + "_big.png" ;
 	var img2_big = imgDir2+IMGPRE + sat2 + "_" + ref2 + "_" + param2 + "_glb_tb_ch" + chan2 + "_big.png" ;
 	var img3_big = imgDir3+IMGPRE + sat3 + "_" + ref3 + "_" + param3 + "_glb_tb_ch" + chan3 + "_big.png" ;
 	var img4_big = imgDir4+IMGPRE + sat4 + "_" + ref4 + "_" + param4 + "_glb_tb_ch" + chan4 + "_big.png" ;

	document.form.img1.src = img1;
	document.form.img2.src = img2;
	document.form.img3.src = img3;
	document.form.img4.src = img4;
	
	document.form.img1.alt = img1;
	document.form.img2.alt = img2;
	document.form.img3.alt = img3;
	document.form.img4.alt = img4;
	
	document.getElementById("href1").href = img1;
	document.getElementById("href2").href = img2;
	document.getElementById("href3").href = img3;
	document.getElementById("href4").href = img4;
}


function loadImageSmall()
{
	var ref1 = document.form.ref1.value;
	var ref2 = document.form.ref2.value;
	var ref3 = document.form.ref3.value;
	var ref4 = document.form.ref4.value;
	
	var sat1 = document.form.sat1.value;
	var sat2 = document.form.sat2.value;
	var sat3 = document.form.sat3.value;
	var sat4 = document.form.sat4.value;
	
	var param1 = document.form.param1.value;
	var param2 = document.form.param2.value;
	var param3 = document.form.param3.value;
	var param4 = document.form.param4.value;
	
	var chan1 = document.form.chan1.value;
	var chan2 = document.form.chan2.value;
	var chan3 = document.form.chan3.value;
	var chan4 = document.form.chan4.value;
	
	var imgDir1 = IMGPATH + sat1 + "/bias/";
	var imgDir2 = IMGPATH + sat2 + "/bias/";
	var imgDir3 = IMGPATH + sat3 + "/bias/";
	var imgDir4 = IMGPATH + sat4 + "/bias/";
	
 	var img1_small = imgDir1+IMGPRE + sat1 + "_" + ref1 + "_" + param1 + "_glb_tb_ch" + chan1 + "_small.png" ;
 	var img2_small = imgDir2+IMGPRE + sat2 + "_" + ref2 + "_" + param2 + "_glb_tb_ch" + chan2 + "_small.png" ;
 	var img3_small = imgDir3+IMGPRE + sat3 + "_" + ref3 + "_" + param3 + "_glb_tb_ch" + chan3 + "_small.png" ;
 	var img4_small = imgDir4+IMGPRE + sat4 + "_" + ref4 + "_" + param4 + "_glb_tb_ch" + chan4 + "_small.png" ;

	document.form.img1.src = img1_small;
	document.form.img2.src = img2_small;
	document.form.img3.src = img3_small;
	document.form.img4.src = img4_small;
	
	document.form.img1.alt = img1_small;
	document.form.img2.alt = img2_small;
	document.form.img3.alt = img3_small;
	document.form.img4.alt = img4_small;
	
 	var img1 = imgDir1+IMGPRE + sat1 + "_" + ref1 + "_" + param1 + "_glb_tb_ch" + chan1 + ".png" ;
 	var img2 = imgDir2+IMGPRE + sat2 + "_" + ref2 + "_" + param2 + "_glb_tb_ch" + chan2 + ".png" ;
 	var img3 = imgDir3+IMGPRE + sat3 + "_" + ref3 + "_" + param3 + "_glb_tb_ch" + chan3 + ".png" ;
 	var img4 = imgDir4+IMGPRE + sat4 + "_" + ref4 + "_" + param4 + "_glb_tb_ch" + chan4 + ".png" ;

	document.getElementById("href1").href = img1;
	document.getElementById("href2").href = img2;
	document.getElementById("href3").href = img3;
	document.getElementById("href4").href = img4;
	
}


function loadInitialImageEmpty() 
{
	var ref = document.form.ref1.value;
	var sat = document.form.sat1.value;
	
	document.form.chan1.selectedIndex=0;
	document.form.chan2.selectedIndex=1;
	document.form.chan3.selectedIndex=2;
	document.form.chan4.selectedIndex=3;
	
	var chan1 = document.form.chan1.value;
	var chan2 = document.form.chan2.value;
	var chan3 = document.form.chan3.value;
	var chan4 = document.form.chan4.value;
	
	var imgDir = IMGPATH + sat + "/bias/";
	
 	var img1 = imgDir+IMGPRE + sat + "_" + ref + "_biasmean_glb_tb_ch" + chan1 + ".png" ;
 	var img2 = imgDir+IMGPRE + sat + "_" + ref + "_biasmean_glb_tb_ch" + chan2 + ".png" ;
 	var img3 = imgDir+IMGPRE + sat + "_" + ref + "_biasmean_glb_tb_ch" + chan3 + ".png" ;
 	var img4 = imgDir+IMGPRE + sat + "_" + ref + "_biasmean_glb_tb_ch" + chan4 + ".png" ;
 	
	var img1_big = imgDir + IMGPRE + sat + "_" + ref + "_biasmean_glb_tb_ch" + chan1 + "_big.png" ;
 	var img2_big = imgDir + IMGPRE + sat + "_" + ref + "_biasmean_glb_tb_ch" + chan2 + "_big.png" ;
 	var img3_big = imgDir + IMGPRE + sat + "_" + ref + "_biasmean_glb_tb_ch" + chan3 + "_big.png" ;
 	var img4_big = imgDir + IMGPRE + sat + "_" + ref + "_biasmean_glb_tb_ch" + chan4 + "_big.png" ;
	
	document.form.img1.src = img1;
	document.form.img2.src = img2;
	document.form.img3.src = img3;
	document.form.img4.src = img4;
	
	document.form.img1.alt = img1;
	document.form.img2.alt = img2;
	document.form.img3.alt = img3;
	document.form.img4.alt = img4;
	
	document.getElementById("href1").href = img1;
	document.getElementById("href2").href = img2;
	document.getElementById("href3").href = img3;
	document.getElementById("href4").href = img4;
}


function loadInitialImageEmptySmall() 
{
	var ref = document.form.ref1.value;
	var sat = document.form.sat1.value;
	
	document.form.chan1.selectedIndex=0;
	document.form.chan2.selectedIndex=1;
	document.form.chan3.selectedIndex=2;
	document.form.chan4.selectedIndex=3;
	
	var chan1 = document.form.chan1.value;
	var chan2 = document.form.chan2.value;
	var chan3 = document.form.chan3.value;
	var chan4 = document.form.chan4.value;
	
	var imgDir = IMGPATH + sat + "/bias/";
	
 	var img1 = imgDir+IMGPRE + sat + "_" + ref + "_biasmean_glb_tb_ch" + chan1 + ".png" ;
 	var img2 = imgDir+IMGPRE + sat + "_" + ref + "_biasmean_glb_tb_ch" + chan2 + ".png" ;
 	var img3 = imgDir+IMGPRE + sat + "_" + ref + "_biasmean_glb_tb_ch" + chan3 + ".png" ;
 	var img4 = imgDir+IMGPRE + sat + "_" + ref + "_biasmean_glb_tb_ch" + chan4 + ".png" ;
 	
 	var img1_small = imgDir+IMGPRE + sat + "_" + ref + "_biasmean_glb_tb_ch" + chan1 + "_small.png" ;
 	var img2_small = imgDir+IMGPRE + sat + "_" + ref + "_biasmean_glb_tb_ch" + chan2 + "_small.png" ;
 	var img3_small = imgDir+IMGPRE + sat + "_" + ref + "_biasmean_glb_tb_ch" + chan3 + "_small.png" ;
 	var img4_small = imgDir+IMGPRE + sat + "_" + ref + "_biasmean_glb_tb_ch" + chan4 + "_small.png" ;
 	
	var img1_big = imgDir+IMGPRE + sat + "_" + ref + "_biasmean_glb_tb_ch" + chan1 + "_big.png" ;
 	var img2_big = imgDir+IMGPRE + sat + "_" + ref + "_biasmean_glb_tb_ch" + chan2 + "_big.png" ;
 	var img3_big = imgDir+IMGPRE + sat + "_" + ref + "_biasmean_glb_tb_ch" + chan3 + "_big.png" ;
 	var img4_big = imgDir+IMGPRE + sat + "_" + ref + "_biasmean_glb_tb_ch" + chan4 + "_big.png" ;
	
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


function loadInitialImage(ref1,sat1,param1,chan1, ref2,sat2,param2,chan2, ref3,sat3,param3,chan3, ref4,sat4,param4,chan4) 
{     	
	if ( sat1 != '' ) {
	
		document.form.ref1.value = ref1;
		document.form.ref2.value = ref2;
		document.form.ref3.value = ref3;
		document.form.ref4.value = ref4;
		
		document.form.sat1.value = sat1;
		document.form.sat2.value = sat2;
		document.form.sat3.value = sat3;
		document.form.sat4.value = sat4;
		
		document.form.param1.value = param1;
		document.form.param2.value = param2;
		document.form.param3.value = param3;
		document.form.param4.value = param4;

		document.form.chan1.value = chan1;
		document.form.chan2.value = chan2;
		document.form.chan3.value = chan3;
		document.form.chan4.value = chan4;
		
		var imgDir1 = IMGPATH + sat1 + "/bias/";
		var imgDir2 = IMGPATH + sat2 + "/bias/";
		var imgDir3 = IMGPATH + sat3 + "/bias/";
		var imgDir4 = IMGPATH + sat4 + "/bias/";

 		var img1 = imgDir1+IMGPRE + sat1 + "_" + ref1 + "_" + param1 + "_glb_tb_ch" + chan1 + ".png" ;
 		var img2 = imgDir2+IMGPRE + sat2 + "_" + ref2 + "_" + param2 + "_glb_tb_ch" + chan2 + ".png" ;
 		var img3 = imgDir3+IMGPRE + sat3 + "_" + ref3 + "_" + param3 + "_glb_tb_ch" + chan3 + ".png" ;
 		var img4 = imgDir4+IMGPRE + sat4 + "_" + ref4 + "_" + param4 + "_glb_tb_ch" + chan4 + ".png" ;
 		
		var img1_big = imgDir1+IMGPRE + sat1 + "_" + ref1 + "_" + param1 + "_glb_tb_ch" + chan1 + "_big.png" ;
 		var img2_big = imgDir2+IMGPRE + sat2 + "_" + ref2 + "_" + param2 + "_glb_tb_ch" + chan2 + "_big.png" ;
 		var img3_big = imgDir3+IMGPRE + sat3 + "_" + ref3 + "_" + param3 + "_glb_tb_ch" + chan3 + "_big.png" ;
 		var img4_big = imgDir4+IMGPRE + sat4 + "_" + ref4 + "_" + param4 + "_glb_tb_ch" + chan4 + "_big.png" ;

		document.form.img1.src = img1;
		document.form.img2.src = img2;
		document.form.img3.src = img3;
		document.form.img4.src = img4;

		document.form.img1.alt = img1;
		document.form.img2.alt = img2;
		document.form.img3.alt = img3;
		document.form.img4.alt = img4;
		
		document.getElementById("href1").href = img1;
		document.getElementById("href2").href = img2;
		document.getElementById("href3").href = img3;
		document.getElementById("href4").href = img4;
	}
	
	else {
	
		loadInitialImageEmpty() ;
	}
}


function loadInitialImageSmall(ref1,sat1,param1,chan1, ref2,sat2,param2,chan2, ref3,sat3,param3,chan3, ref4,sat4,param4,chan4)
{
	if ( sat1 != '' ) {
	
		document.form.ref1.value = ref1;
		document.form.ref2.value = ref2;
		document.form.ref3.value = ref3;
		document.form.ref4.value = ref4;
		
		document.form.sat1.value = sat1;
		document.form.sat2.value = sat2;
		document.form.sat3.value = sat3;
		document.form.sat4.value = sat4;
		
		document.form.param1.value = param1;
		document.form.param2.value = param2;
		document.form.param3.value = param3;
		document.form.param4.value = param4;

		document.form.chan1.value = chan1;
		document.form.chan2.value = chan2;
		document.form.chan3.value = chan3;
		document.form.chan4.value = chan4;
		
		var imgDir1 = IMGPATH + sat1 + "/bias/";
		var imgDir2 = IMGPATH + sat2 + "/bias/";
		var imgDir3 = IMGPATH + sat3 + "/bias/";
		var imgDir4 = IMGPATH + sat4 + "/bias/";

 		var img1 = imgDir1+IMGPRE + sat1 + "_" + ref1 + "_" + param1 + "_glb_tb_ch" + chan1 + ".png" ;
 		var img2 = imgDir2+IMGPRE + sat2 + "_" + ref2 + "_" + param2 + "_glb_tb_ch" + chan2 + ".png" ;
 		var img3 = imgDir3+IMGPRE + sat3 + "_" + ref3 + "_" + param3 + "_glb_tb_ch" + chan3 + ".png" ;
 		var img4 = imgDir4+IMGPRE + sat4 + "_" + ref4 + "_" + param4 + "_glb_tb_ch" + chan4 + ".png" ;
  		
		var img1_small = imgDir1+IMGPRE + sat1 + "_" + ref1 + "_" + param1 + "_glb_tb_ch" + chan1 + "_small.png" ;
 		var img2_small = imgDir2+IMGPRE + sat2 + "_" + ref2 + "_" + param2 + "_glb_tb_ch" + chan2 + "_small.png" ;
 		var img3_small = imgDir3+IMGPRE + sat3 + "_" + ref3 + "_" + param3 + "_glb_tb_ch" + chan3 + "_small.png" ;
 		var img4_small = imgDir4+IMGPRE + sat4 + "_" + ref4 + "_" + param4 + "_glb_tb_ch" + chan4 + "_small.png" ;
		
		var img1_big = imgDir1+IMGPRE + sat1 + "_" + ref1 + "_" + param1 + "_glb_tb_ch" + chan1 + "_big.png" ;
 		var img2_big = imgDir2+IMGPRE + sat2 + "_" + ref2 + "_" + param2 + "_glb_tb_ch" + chan2 + "_big.png" ;
 		var img3_big = imgDir3+IMGPRE + sat3 + "_" + ref3 + "_" + param3 + "_glb_tb_ch" + chan3 + "_big.png" ;
 		var img4_big = imgDir4+IMGPRE + sat4 + "_" + ref4 + "_" + param4 + "_glb_tb_ch" + chan4 + "_big.png" ;

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


function fwd( sat, chan_obj )
{
        var chan = parseInt(chan_obj.value,10);	
        chan = chan + 1 ;
	
	if      ( ( sat == "f16" || sat == "f18" ) && chan > 24 ) chan = 1;
	else if ( ( sat == "n18" || sat == "n19" || sat == "metopA" || sat == "metopB" ) && chan > 20 ) chan = 1;
	else if (  sat == "npp" && chan > 22 ) chan = 1;
	
        chan_obj.value = chan;
}


function rev( sat, chan_obj ) 
{
        var chan = parseInt(chan_obj.value,10);
        chan = chan - 1;
        
	if ( chan < 1 && ( sat == "n18" || sat == "n19" || sat == "metopA" || sat == "metopB" ) ) chan = 20;
	else if ( chan < 1 && ( sat == "f16" || sat == "f18" ) ) chan = 24;
	else if ( chan < 1 && ( sat == "npp" ) ) chan = 22;

        chan_obj.value = chan;
}
