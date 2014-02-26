<!-- //
// iterate through all the navigation ULs
// in order to hide the subitems ONLY if javascript is on
// if javascript is turned off, just show the expanded navigation tree all the time
// thus staying out of accessibility trouble
// and staying out of trouble with the 'i don't have javascript' whiners.

if (document.getElementById) {

	var AllULs = document.getElementsByTagName("ul");
	// alert(AllDivs.length);
	var numULs = AllULs.length;
	for (i=0; i < numULs; i++) {
		if (AllULs[i].className == "detailContainer") {
		AllULs[i].style.display = "none";
		}
	}

// this part figures out what the current page's file name is;
// then that filename is compared to the id of all the li's in the navigation system
// the li with an id that matches the current file name 
// is made red, gets a title that says it's the current page
// and gets '>>' at the end, all to indicate that 'you are here'.

	var currentLocation = document.location;
	var currentLocationText = new String(currentLocation);
	var currentLocationTextEnd = (currentLocationText.length - 4);
	var lastSlash = currentLocationText.lastIndexOf('/');
	var dropAnchors = currentLocationText.lastIndexOf('#');
	
	var startFileName = lastSlash + 1;

	if (dropAnchors != -1) {
	var EndOfFileName = dropAnchors - 4;
	var fileText = currentLocationText.slice(startFileName,EndOfFileName);
} else {
	var fileText = currentLocationText.slice(startFileName,currentLocationTextEnd);
}

	if (lastSlash == (currentLocationText.length-1)) {
		fileText = 'index';
	}

	var listItems = document.getElementsByTagName("li");
	var numListItems = listItems.length;
	//window.alert(numListItems);
	// window.alert(fileText);

	for (i=0; i < numListItems; i++) {
		if (listItems[i].id == fileText) {  
				// window.alert('recognized child state properly');
				listItems[i].parentNode.className = "showDetailContainer";
				listItems[i].parentNode.style.display = "block";
				listItems[i].parentNode.style.backgroundColor = "#6886d3";

				if ((listItems[i].parentNode.parentNode.parentNode.tagName == "UL") && (listItems[i].parentNode.parentNode.parentNode.className != "contentList")) {
					listItems[i].parentNode.parentNode.parentNode.className = "showDetailContainer";
					listItems[i].parentNode.parentNode.parentNode.style.display = "block";
					listItems[i].parentNode.parentNode.parentNode.style.backgroundColor = "#6886d3";
				}

				if((listItems[i].parentNode.parentNode.className == "leftNav") && (listItems[i].childNodes[2])) {
					listItems[i].childNodes[2].className = "showDetailContainer";
					listItems[i].childNodes[2].style.display = "block";
					listItems[i].childNodes[2].style.backgroundColor = "#6886d3";
				}

				var textNow = listItems[i].firstChild.innerHTML;
				// window.alert(textNow);
				listItems[i].firstChild.innerHTML = textNow + " >>";
				listItems[i].firstChild.title = "this is the current page";
				listItems[i].firstChild.id = "activeLink";
		}
	}
}

//-->
