var imgDir = "images/";



function changeProduct( prod_obj, layer_id ) 
{
	
	var prod = prod_obj.value ;
	
	if( prod == "swe" ) {
		document.getElementById(layer_id).className ="optioninvisible";
	}
	else if ( prod == "sic" ) {
		document.getElementById(layer_id).className ="optionvisible";
	}
	
}


function getImage(sat,parameter,cend,region,prod,layer) {
    
	var header = "scores_" ;
	var amsrsic  = "_amsrsic_"
	var amsrswe  = "_amsrswe_"
	var dash  = "_" ;
	
	var img = "";
	
	if( prod == "sic" )
		img = imgDir+sat+'/timeseries/'+header+sat+amsrsic+prod+dash+layer+dash+region+dash+parameter+dash+cend+".png";
	else if( prod == "swe" )
		img = imgDir+sat+'/timeseries/'+header+sat+amsrswe+prod+"_nasaswe_"+region+dash+parameter+dash+cend+".png";
	
	return img;

}


function loadImage(sat_obj,parameter_obj,cend_obj,region_obj,prod_obj,layer_obj,img_obj,href_id) {

	var sat 	= sat_obj.value;
	var parameter 	= parameter_obj.value;
	var cend 	= cend_obj.value;
	var region 	= region_obj.value;
	var prod 	= prod_obj.value;
	var layer 	= layer_obj.value;

	var img = getImage(sat,parameter,cend,region,prod,layer);
	var img_big = img.replace(/.png/, "_big.png");
	
	img_obj.src = img;
	
	var index = img.lastIndexOf("/");
	var alt   = img.substring(index+1,img.length);
	
	img_obj.title = img;
	img_obj.alt = alt;
	
	document.getElementById(href_id).href = img_big;

}



function loadImageV(sat_obj,parameter_obj,cend_obj,region_obj,prod_obj,layer_obj,img_obj,href_id) {

	var sat 	= sat_obj.value;
	var parameter 	= parameter_obj.value;
	var cend 	= cend_obj.value;
	var region 	= region_obj.value;
	var prod 	= prod_obj.value;
	var layer 	= layer_obj.value;

	var img = getImage(sat,parameter,cend,region,prod,layer);
	var img_big = img.replace(/.png/, "_big.png");
	
	img_obj.src = img_big;
	
	var index = img_big.lastIndexOf("/");
	var alt   = img_big.substring(index+1,img_big.length);
	
	img_obj.title = img_big;
	img_obj.alt = alt;
	
	document.getElementById(href_id).href = img_big;

}



function loadInitialImages(sat1,parameter1,cend1,region1,prod1,layer1,
			   sat2,parameter2,cend2,region2,prod2,layer2,
			   sat3,parameter3,cend3,region3,prod3,layer3,
			   sat4,parameter4,cend4,region4,prod4,layer4,
			   sat5,parameter5,cend5,region5,prod5,layer5,
			   sat6,parameter6,cend6,region6,prod6,layer6)
{
	var img1 = getImage(sat1,parameter1,cend1,region1,prod1,layer1);
	var img2 = getImage(sat2,parameter2,cend2,region2,prod2,layer2);
	var img3 = getImage(sat3,parameter3,cend3,region3,prod3,layer3);
	var img4 = getImage(sat4,parameter4,cend4,region4,prod4,layer4);
	var img5 = getImage(sat5,parameter5,cend5,region5,prod5,layer5);
	var img6 = getImage(sat6,parameter6,cend6,region6,prod6,layer6);
	
	document.form.sat1.value 	= sat1;
	document.form.parameter1.value 	= parameter1;
	document.form.cend1.value 	= cend1;
	document.form.region1.value 	= region1;
	document.form.prod1.value 	= prod1;
	document.form.layer1.value 	= layer1;
	document.form.img1.src 		= img1;
	
	document.form.sat2.value 	= sat2;
	document.form.parameter2.value 	= parameter2;
	document.form.cend2.value 	= cend2;
	document.form.region2.value 	= region2;
	document.form.prod2.value 	= prod2;
	document.form.layer2.value 	= layer2;
	document.form.img2.src 		= img2;
	
	document.form.sat3.value 	= sat3;
	document.form.parameter3.value 	= parameter3;
	document.form.cend3.value 	= cend3;
	document.form.region3.value 	= region3;
	document.form.prod3.value 	= prod3;
	document.form.layer3.value 	= layer3;
	document.form.img3.src 		= img3;
	
	document.form.sat4.value 	= sat4;
	document.form.parameter4.value 	= parameter4;
	document.form.cend4.value 	= cend4;
	document.form.region4.value 	= region4;
	document.form.prod4.value 	= prod4;
	document.form.layer4.value 	= layer4;
	document.form.img4.src 		= img4;
	
	document.form.sat5.value 	= sat5;
	document.form.parameter5.value 	= parameter5;
	document.form.cend5.value 	= cend5;
	document.form.region5.value 	= region5;
	document.form.prod5.value 	= prod5;
	document.form.layer5.value 	= layer5;
	document.form.img5.src 		= img5;
	
	document.form.sat6.value 	= sat6;
	document.form.parameter6.value 	= parameter6;
	document.form.cend6.value 	= cend6;
	document.form.region6.value 	= region6;
	document.form.prod6.value 	= prod6;
	document.form.layer6.value 	= layer6;
	document.form.img6.src 		= img6;
	
	if( prod1 == "swe" ) {
		document.getElementById('layer1').className ="optioninvisible";
	}
	else {
		document.getElementById('layer1').className ="optionvisible";
	}
	
	if( prod2 == "swe" ) {
		document.getElementById('layer2').className ="optioninvisible";
	}
	else {
		document.getElementById('layer2').className ="optionvisible";
	}
	
	if( prod3 == "swe" ) {
		document.getElementById('layer3').className ="optioninvisible";
	}
	else {
		document.getElementById('layer3').className ="optionvisible";
	}
	
	if( prod4 == "swe" ) {
		document.getElementById('layer4').className ="optioninvisible";
	}
	else {
		document.getElementById('layer4').className ="optionvisible";
	}
	
	if( prod5 == "swe" ) {
		document.getElementById('layer5').className ="optioninvisible";
	}
	else {
		document.getElementById('layer5').className ="optionvisible";
	}
	
	if( prod6 == "swe" ) {
		document.getElementById('layer6').className ="optioninvisible";
	}
	else {
		document.getElementById('layer6').className ="optionvisible";
	}
	
	
	var img1_big = img1.replace(/.png/, "_big.png");
	var img2_big = img2.replace(/.png/, "_big.png");
	var img3_big = img3.replace(/.png/, "_big.png");
	var img4_big = img4.replace(/.png/, "_big.png");
	var img5_big = img5.replace(/.png/, "_big.png");
	var img6_big = img6.replace(/.png/, "_big.png");
	
	document.getElementById('href1').href = img1_big;
	document.getElementById('href2').href = img2_big;
	document.getElementById('href3').href = img3_big;
	document.getElementById('href4').href = img4_big;
	document.getElementById('href5').href = img5_big;
	document.getElementById('href6').href = img6_big;
	
}
			 


function loadInitialImagesV(sat1,parameter1,cend1,region1,prod1,layer1,
			    sat2,parameter2,cend2,region2,prod2,layer2,
			    sat3,parameter3,cend3,region3,prod3,layer3,
			    sat4,parameter4,cend4,region4,prod4,layer4,
			    sat5,parameter5,cend5,region5,prod5,layer5,
			    sat6,parameter6,cend6,region6,prod6,layer6)
{
	var img1_tmp = getImage(sat1,parameter1,cend1,region1,prod1,layer1);
	var img2_tmp = getImage(sat2,parameter2,cend2,region2,prod2,layer2);
	var img3_tmp = getImage(sat3,parameter3,cend3,region3,prod3,layer3);
	var img4_tmp = getImage(sat4,parameter4,cend4,region4,prod4,layer4);
	var img5_tmp = getImage(sat5,parameter5,cend5,region5,prod5,layer5);
	var img6_tmp = getImage(sat6,parameter6,cend6,region6,prod6,layer6);
	
	var img1 = img1_tmp.replace(/.png/, "_big.png");
	var img2 = img2_tmp.replace(/.png/, "_big.png");
	var img3 = img3_tmp.replace(/.png/, "_big.png");
	var img4 = img4_tmp.replace(/.png/, "_big.png");
	var img5 = img5_tmp.replace(/.png/, "_big.png");
	var img6 = img6_tmp.replace(/.png/, "_big.png");
	
	document.form.sat1.value 	= sat1;
	document.form.parameter1.value 	= parameter1;
	document.form.cend1.value 	= cend1;
	document.form.region1.value 	= region1;
	document.form.prod1.value 	= prod1;
	document.form.layer1.value 	= layer1;
	document.form.img1.src 		= img1;
	
	document.form.sat2.value 	= sat2;
	document.form.parameter2.value 	= parameter2;
	document.form.cend2.value 	= cend2;
	document.form.region2.value 	= region2;
	document.form.prod2.value 	= prod2;
	document.form.layer2.value 	= layer2;
	document.form.img2.src 		= img2;
	
	document.form.sat3.value 	= sat3;
	document.form.parameter3.value 	= parameter3;
	document.form.cend3.value 	= cend3;
	document.form.region3.value 	= region3;
	document.form.prod3.value 	= prod3;
	document.form.layer3.value 	= layer3;
	document.form.img3.src 		= img3;
	
	document.form.sat4.value 	= sat4;
	document.form.parameter4.value 	= parameter4;
	document.form.cend4.value 	= cend4;
	document.form.region4.value 	= region4;
	document.form.prod4.value 	= prod4;
	document.form.layer4.value 	= layer4;
	document.form.img4.src 		= img4;
	
	document.form.sat5.value 	= sat5;
	document.form.parameter5.value 	= parameter5;
	document.form.cend5.value 	= cend5;
	document.form.region5.value 	= region5;
	document.form.prod5.value 	= prod5;
	document.form.layer5.value 	= layer5;
	document.form.img5.src 		= img5;
	
	document.form.sat6.value 	= sat6;
	document.form.parameter6.value 	= parameter6;
	document.form.cend6.value 	= cend6;
	document.form.region6.value 	= region6;
	document.form.prod6.value 	= prod6;
	document.form.layer6.value 	= layer6;
	document.form.img6.src 		= img6;
	
	if( prod1 == "swe" ) {
		document.getElementById('layer1').className ="optioninvisible";
	}
	else {
		document.getElementById('layer1').className ="optionvisible";
	}
	
	if( prod2 == "swe" ) {
		document.getElementById('layer2').className ="optioninvisible";
	}
	else {
		document.getElementById('layer2').className ="optionvisible";
	}
	
	if( prod3 == "swe" ) {
		document.getElementById('layer3').className ="optioninvisible";
	}
	else {
		document.getElementById('layer3').className ="optionvisible";
	}
	
	if( prod4 == "swe" ) {
		document.getElementById('layer4').className ="optioninvisible";
	}
	else {
		document.getElementById('layer4').className ="optionvisible";
	}
	
	if( prod5 == "swe" ) {
		document.getElementById('layer5').className ="optioninvisible";
	}
	else {
		document.getElementById('layer5').className ="optionvisible";
	}
	
	if( prod6 == "swe" ) {
		document.getElementById('layer6').className ="optioninvisible";
	}
	else {
		document.getElementById('layer6').className ="optionvisible";
	}
	
	
	var img1_big = img1.replace(/.png/, "_big.png");
	var img2_big = img2.replace(/.png/, "_big.png");
	var img3_big = img3.replace(/.png/, "_big.png");
	var img4_big = img4.replace(/.png/, "_big.png");
	var img5_big = img5.replace(/.png/, "_big.png");
	var img6_big = img6.replace(/.png/, "_big.png");
	
	document.getElementById('href1').href = img1_big;
	document.getElementById('href2').href = img2_big;
	document.getElementById('href3').href = img3_big;
	document.getElementById('href4').href = img4_big;
	document.getElementById('href5').href = img5_big;
	document.getElementById('href6').href = img6_big;
	
}
			 
