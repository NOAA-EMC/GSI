<!--//
		if (document.getElementById) {

	var AllDIVs = document.getElementsByTagName("div");
	// alert(AllDivs.length);
	var numDIVs = AllDIVs.length;
	for (i=0; i < numDIVs; i++) {
		if ((AllDIVs[i].className == "detailContainer") || (AllDIVs[i].className == "acroContainer")) {
		AllDIVs[i].style.display = "none";
		}
	}
}

function expandomaticWithParameters(targetID,linkText1,linkText2) {
if (document.getElementById) {
	var Stripper = new String(targetID);
	//alert(Stripper.length);
	var grabbedValue = targetID.substring(2,Stripper.length);
	//alert(grabbedValue);
	var anchorToChange = "show" + grabbedValue;
	//alert(anchorToChange);
	
var detailTarget = document.getElementById(targetID);
var anchorTarget = document.getElementById(anchorToChange);
	if (detailTarget.style.display == "none") { 
		detailTarget.style.display = "";
		anchorTarget.innerHTML = linkText1;
		// what it is AFTER expanded
		anchorTarget.style.color = "#ff0000";
		detailTarget.style.backgroundColor="#ffffff";
	}
	else if (detailTarget.style.display == "") {
		detailTarget.style.display = "none";
		anchorTarget.innerHTML = linkText2;
		// what it is when UNEXPANDED
		anchorTarget.style.color = "#325f99";
		detailTarget.style.backgroundColor="#ffffff";
	}
}
}

function expandomaticAP(targetID,anchorToChange,linkText1,linkText2) {
// parameters:
// targetID is the id for the container that opens and closes
// anchorToChange is the ID of the link that drives the expand / contract; 
// linkText1 & linkText2 are the text strings for that link
// when they are expanded (usually w/ '..Hide..')
// and when they are closed (usually w/ 'show' or 'expand', etc.)

	if (document.getElementById) {
	
	var detailTarget = document.getElementById(targetID);
	var anchorTarget = document.getElementById(anchorToChange);

		if (detailTarget.style.display == "none") { 
			detailTarget.style.display = "";
			anchorTarget.innerHTML = linkText1;
			// what it is AFTER expanded
			anchorTarget.style.color = "#ff0000";
			detailTarget.style.backgroundColor="#ffffff";
		}
		else if (detailTarget.style.display == "") {
			detailTarget.style.display = "none";
			anchorTarget.innerHTML = linkText2;
			// what it is when UNEXPANDED
			anchorTarget.style.color = "#325f99";
			detailTarget.style.backgroundColor="#ffffff";
		}
	}
}



function goFish(namedAnchor) {
NowPlace=document.location;
document.location = NowPlace + namedAnchor;
}	


//-->