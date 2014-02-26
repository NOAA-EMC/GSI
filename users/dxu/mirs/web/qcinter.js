function clientSideInclude(id, url) {
	var req = false;
	
	// For Safari, Firefox, and other non-MS browsers
	if (window.XMLHttpRequest) 
	{
		try {
			req = new XMLHttpRequest();
		} catch (e) {
			req = false;
		}
	} 
	// For Internet Explorer on Windows
	else if (window.ActiveXObject) {
		try {
			req = new ActiveXObject("Msxml2.XMLHTTP");
		} catch (e) {
			try {
				req = new ActiveXObject("Microsoft.XMLHTTP");
			} catch (e) {
				req = false;
			}
		}
	}
	
	var element = document.getElementById(id);
	if (!element) 
	{
		alert("Bad id " + id + " passed to clientSideInclude." +
		"You need a div or span element with this id in your page.");
		return;
	}
	
	if (req) {
    	// Synchronous request, wait till we have it all
	//req.open('GET', url, false);
	req.open('POST', url, false);
	req.send(null);
	var stat= req.status;

	if( stat == 200 ) {
		element.innerHTML = req.responseText;
	}
	else {
		element.innerHTML = "<p><center><font color=red>No table exist for selected date.</font></center></p>";
	}
    
	} else {
		element.innerHTML = "Sorry, your browser does not support XMLHTTPRequest objects.";
	}
}


function loadTable() {

	var yr  = document.form.yr.value;
	var mo  = document.form.mo.value;
	var dy  = document.form.dy.value;
	var ref = document.form.ref.value;
	
	var url = "qc/inter/inter_" + ref + "_table_" + yr + mo + dy + ".inc";
	var id = "inter_table";
	
	clientSideInclude(id, url);

}


function loadInit() 
{
	var now = new Date();
	now.setDate(now.getDate()-3) ; // 3 days ago

	var year  = now.getFullYear();
	var month = now.getMonth();    // month starting index is 0
	var day   = now.getDate();
	
	document.form.yr.value = year;
	document.form.mo.selectedIndex = month ;
	document.form.dy.selectedIndex = day - 1;
	document.form.ref.value = 'gdas';
	
	loadTable();
	
	// windown scroll down 140 px, thus no need to click "Skip Top Navigation" in the page
	window.scrollBy(300,140);
}


function rev() 
{
        var year  = parseInt(document.form.yr.value,10);
        var month = parseInt(document.form.mo.value,10);
        var day   = parseInt(document.form.dy.value,10);

        var now = new Date(year,month-1,day);
        now.setDate(now.getDate()-1);

        year  = now.getFullYear();
        month = now.getMonth();
        day   = now.getDate();

	document.form.yr.value = year;
	document.form.mo.selectedIndex = month ;
	document.form.dy.selectedIndex = day - 1;
	
	loadTable();
}


function fwd()
{
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
	
	loadTable();
}
