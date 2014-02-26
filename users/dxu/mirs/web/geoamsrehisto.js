var IMGDIR = 'images/' ;
var ndayback = 4;

function loadImages() {

	var sat = document.form.sat.value;
	var yr  = document.form.yr.value;
	var mo  = document.form.mo.value;
	var dy  = document.form.dy.value;
	
	var ymd = '/' + yr + '-' + mo + '-' + dy + '/' ;
	
	var img1 = IMGDIR + sat + ymd + 'histo_' + sat + '_' + yr + mo + dy + '_sice_glb_as.png';
	var img2 = IMGDIR + sat + ymd + 'histo_' + sat + '_' + yr + mo + dy + '_sice_glb_ds.png';
	var img3 = IMGDIR + sat + ymd + 'histo_' + sat + '_' + yr + mo + dy + '_sice_nh_as.png';
	var img4 = IMGDIR + sat + ymd + 'histo_' + sat + '_' + yr + mo + dy + '_sice_nh_ds.png';
	var img5 = IMGDIR + sat + ymd + 'histo_' + sat + '_' + yr + mo + dy + '_sice_sh_as.png';
	var img6 = IMGDIR + sat + ymd + 'histo_' + sat + '_' + yr + mo + dy + '_sice_sh_ds.png';
	var img7 = IMGDIR + sat + ymd + 'histo_' + sat + '_' + yr + mo + dy + '_swe_glb_as.png';
	var img8 = IMGDIR + sat + ymd + 'histo_' + sat + '_' + yr + mo + dy + '_swe_glb_ds.png';
	
	document.form.sat2.value = sat;
	document.form.yr2.value  = yr;
	document.form.mo2.value  = mo;
	document.form.dy2.value  = dy;

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
	
	
	document.form.img1.alt = alt1;
	document.form.img2.alt = alt2;
	document.form.img3.alt = alt3;
	document.form.img4.alt = alt4;
	document.form.img5.alt = alt5;
	document.form.img6.alt = alt6;
	document.form.img7.alt = alt7;
	document.form.img8.alt = alt8;

	document.getElementById("href1").href=img1;
	document.getElementById("href2").href=img2;
	document.getElementById("href3").href=img3;
	document.getElementById("href4").href=img4;
	document.getElementById("href5").href=img5;
	document.getElementById("href6").href=img6;
	document.getElementById("href7").href=img7;
	document.getElementById("href8").href=img8;
	
}


function loadImages2() {

	var sat = document.form.sat2.value;
	var yr  = document.form.yr2.value;
	var mo  = document.form.mo2.value;
	var dy  = document.form.dy2.value;
	
	var ymd = '/' + yr + '-' + mo + '-' + dy + '/' ;
	
	var img1 = IMGDIR + sat + ymd + 'histo_' + sat + '_' + yr + mo + dy + '_sic_glb_as.png';
	var img2 = IMGDIR + sat + ymd + 'histo_' + sat + '_' + yr + mo + dy + '_sic_glb_ds.png';
	var img3 = IMGDIR + sat + ymd + 'histo_' + sat + '_' + yr + mo + dy + '_sic_nh_as.png';
	var img4 = IMGDIR + sat + ymd + 'histo_' + sat + '_' + yr + mo + dy + '_sic_nh_ds.png';
	var img5 = IMGDIR + sat + ymd + 'histo_' + sat + '_' + yr + mo + dy + '_sic_sh_as.png';
	var img6 = IMGDIR + sat + ymd + 'histo_' + sat + '_' + yr + mo + dy + '_sic_sh_ds.png';
	var img7 = IMGDIR + sat + ymd + 'histo_' + sat + '_' + yr + mo + dy + '_swe_glb_as.png';
	var img8 = IMGDIR + sat + ymd + 'histo_' + sat + '_' + yr + mo + dy + '_swe_glb_ds.png';
	
	document.form.sat.value = sat;
	document.form.yr.value  = yr;
	document.form.mo.value  = mo;
	document.form.dy.value  = dy;

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
	
	document.form.img1.alt = alt1;
	document.form.img2.alt = alt2;
	document.form.img3.alt = alt3;
	document.form.img4.alt = alt4;
	document.form.img5.alt = alt5;
	document.form.img6.alt = alt6;
	document.form.img7.alt = alt7;
	document.form.img8.alt = alt8;
	
	document.getElementById("href1").href=img1;
	document.getElementById("href2").href=img2;
	document.getElementById("href3").href=img3;
	document.getElementById("href4").href=img4;
	document.getElementById("href5").href=img5;
	document.getElementById("href6").href=img6;
	document.getElementById("href7").href=img7;
	document.getElementById("href8").href=img8;
}



function loadInitialImages(sat,yr,mo,dy) {
	
	if ( sat == "" ) {
		
		var now = new Date();
		now.setDate(now.getDate()-ndayback) ; // n day ago

		var year  = now.getFullYear();
		var month = now.getMonth();  // month starting index is 0
		var day   = now.getDate();
		
		// these were: year=2011 month=9 and day = 3; there were no images for those dates. L. Brown 2013-09-12
		year=2013;
		month=5;
		day=4;
		
		document.form.yr.value = year;
		document.form.mo.selectedIndex = month ;
		document.form.dy.selectedIndex = day - 1;
		document.form.sat.selectedIndex=0;

		document.form.yr2.value = year;
		document.form.mo2.selectedIndex = month ;
		document.form.dy2.selectedIndex = day - 1;
		document.form.sat2.selectedIndex=0;
		
	}
	else {
	
		document.form.yr.value  = yr;
		document.form.mo.value  = mo ;
		document.form.dy.value  = dy;
		document.form.sat.value = sat;

		document.form.yr2.value  = yr;
		document.form.mo2.value  = mo ;
		document.form.dy2.value  = dy;
		document.form.sat2.value = sat;
	
	}
	
	
	loadImages();
	
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
	
	loadImages(); 
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

        loadImages();
}

