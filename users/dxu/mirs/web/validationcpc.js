var IMGPATH = 'images/cpc/' ;
var ndayback = 8;


function loadImages() {
    
    var comp = document.form.comp.value;
    var yyyy = document.form.yr.value;
    var mm = document.form.mo.value;
    var dd = document.form.dy.value;
    var yyyymmdd = yyyy+mm+dd;

    var img1 = IMGPATH+'cpc_prcp_cu_gauge_v1.0_us_'+yyyymmdd+'_rrday.png';
    
    var img2 = IMGPATH+'mirs_adv_poes_'+comp+'_us_'+yyyymmdd+'_rrday_ad.png';
    var img3 = IMGPATH+'mirs_adv_poes_cpc_'+comp+'_us_'+yyyymmdd+'_p2p_rrday_1212Z_ad.png';
    var img4 = IMGPATH+'mirs_adv_poes_cpc_'+comp+'_us_'+yyyymmdd+'_hist_p2p_rrday_1212Z_ad.png';

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



function rev(yr,mo,dy) {

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

    loadImages();
}



function fwd(yr,mo,dy) {

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

    loadImages();
}



function loadInitialImages() {
    
    var now = new Date();
    now.setDate(now.getDate()-ndayback) ; // n days ago

    var year  = now.getFullYear();
    var month = now.getMonth();  // month starting index is 0
    var day   = now.getDate();

    document.form.yr.value = year;
    document.form.mo.selectedIndex = month ;
    document.form.dy.selectedIndex = day - 1;
    
    document.form.comp.value = "n18_metopA_f16";
    
    loadImages();

}

// change panel formation
function changePanel( count ) {
	if     ( count == 1 ) document.form.action="validationcpctimeseriesv.php";
	else if( count == 2 ) document.form.action="validationcpc.php"; 
	else if( count == 3 ) document.form.action="validationcpcv.php"; 
}
