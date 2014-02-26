<!--//
	if (document.getElementById) {
		var popupSpan = document.getElementById("jsDependent");

		// we want to show this span IF javascript is on;
		// it loads invisible, but switches to 'display' if javascript makes this happen.

		popupSpan.style.display = "block";
}


function popUp(pageName,wideness,tallness,canScroll,reSize) {
	window.open(pageName,"newWindow","location=yes,menubar=yes,toolbar=yes,resizable=" + reSize + ",scrollbars=" + canScroll + ",width=" + wideness + ",height="+ tallness); 
	}

//-->
