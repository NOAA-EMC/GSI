var IMGPATHS = new Array();
IMGPATHS["n18"] = "images/" ;
IMGPATHS["n19"] = "images/" ;
IMGPATHS["metopA"] = "images/" ;
IMGPATHS["metopB"] = "images/" ;

//var IMGPATH = "images/" ;

var IMGPRE  = "mirs_adv_poes_" ;


function loadImage1() {
    
    var sat1 = document.form.sat1.value;
    var cend1 = document.form.cend1.value;
    var param1 = document.form.param1.value;
    var yyyy1 = document.form.yr1.value;
    var mm1 = document.form.mo1.value;
    var dd1 = document.form.dy1.value;
    var ymd1 = yyyy1 + "-" + mm1 + "-" + dd1;
    
    var IMGPATH = IMGPATHS[sat1];
    var imgDir1 = IMGPATH + sat1 + "/" + ymd1 + "/";
    var img1 = imgDir1+IMGPRE + sat1 + "_fm_glb_"  + yyyy1 + mm1 + dd1 + "_" + param1 + "_" + cend1 + ".png" ;

    document.form.img1.src = img1;
    document.form.img1.alt = img1;
    document.getElementById("href1").href = img1;
}


function loadImage2() {
    
    var sat2 = document.form.sat2.value;
    var cend2 = document.form.cend2.value;
    var param2 = document.form.param2.value;
    var yyyy2 = document.form.yr2.value;
    var mm2 = document.form.mo2.value;
    var dd2 = document.form.dy2.value;
    var ymd2 = yyyy2 + "-" + mm2 + "-" + dd2;

    var IMGPATH = IMGPATHS[sat2];
    var imgDir2 = IMGPATH + sat2 + "/" + ymd2 + "/";
    var img2 = imgDir2+IMGPRE + sat2 + "_fm_glb_"  + yyyy2 + mm2 + dd2 + "_" + param2 + "_" + cend2 + ".png" ;

    document.form.img2.src = img2;
    document.form.img2.alt = img2;
    document.getElementById("href2").href = img2;
}


function loadImage3() {
    
    var sat3 = document.form.sat3.value;
    var cend3 = document.form.cend3.value;
    var param3 = document.form.param3.value;
    var yyyy3 = document.form.yr3.value;
    var mm3 = document.form.mo3.value;
    var dd3 = document.form.dy3.value;
    var ymd3 = yyyy3 + "-" + mm3 + "-" + dd3;

    var IMGPATH = IMGPATHS[sat3];
    var imgDir3 = IMGPATH + sat3 + "/" + ymd3 + "/";
    var img3 = imgDir3+IMGPRE + sat3 + "_fm_glb_"  + yyyy3 + mm3 + dd3 + "_" + param3 + "_" + cend3 + ".png" ;

    document.form.img3.src = img3;
    document.form.img3.alt = img3;
    document.getElementById("href3").href = img3;
}


function loadInitialImages() {
    
    var now = new Date();
    now.setDate(now.getDate()-1) ; // yesterday

    var year  = now.getFullYear();
    var month = now.getMonth();  // month starting index is 0
    var day   = now.getDate();

    document.form.yr1.value = year;
    document.form.mo1.selectedIndex = month ;
    document.form.dy1.selectedIndex = day - 1;
    document.form.cend1.selectedIndex=0;
    document.form.sat1.selectedIndex=0;
    document.form.param1.selectedIndex=0;

    document.form.yr2.value = year;
    document.form.mo2.selectedIndex = month ;
    document.form.dy2.selectedIndex = day - 1;
    document.form.cend2.selectedIndex=0;
    document.form.sat2.selectedIndex=0;
    document.form.param2.selectedIndex=1;

    document.form.yr3.value = year;
    document.form.mo3.selectedIndex = month ;
    document.form.dy3.selectedIndex = day - 1;
    document.form.cend3.selectedIndex=0;
    document.form.sat3.selectedIndex=0;
    document.form.param3.selectedIndex=2;

    loadImage1();
    loadImage2();
    loadImage3();

}


function rev(yr,mo,dy,i) {

    var year  = parseInt(yr.value,10);
    var month = parseInt(mo.value,10);
    var day   = parseInt(dy.value,10);

    var now = new Date(year,month-1,day);
    now.setDate(now.getDate()-1);

    year  = now.getFullYear();
    month = now.getMonth();
    day   = now.getDate();

    yr.value = year;
    mo.selectedIndex = month ;
    dy.selectedIndex = day - 1;

    if      ( i == 1 ) loadImage1();
    else if ( i == 2 ) loadImage2();  
    else if ( i == 3 ) loadImage3();  
}


function fwd(yr,mo,dy,i) {

    var year  = parseInt(yr.value,10);
    var month = parseInt(mo.value,10);
    var day   = parseInt(dy.value,10);

    var now = new Date(year,month-1,day);
    now.setDate(now.getDate()+1);

    year  = now.getFullYear();
    month = now.getMonth();
    day   = now.getDate();

    yr.value = year;
    mo.selectedIndex = month ;
    dy.selectedIndex = day - 1;

    if      ( i == 1 ) loadImage1();
    else if ( i == 2 ) loadImage2();  
    else if ( i == 3 ) loadImage3();  
}
