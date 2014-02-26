<?php
// insert header that forbids caching and carries doctype
// and html tag;
require('includes/noCacheHeader.inc');
?>
<META name="verify-v1" content="CwbLBcFt9+GqRTgaLZsENmPnSWNB5MStHHdYsB7U2nI=">
<title>STAR - MIRS Project - Vertical Cross Section Monitoring</title>
<?php
// style links and the icon link
// pulls in .css files for regular and print display
require('includes/styleLinks.inc');
?>

<style type="text/css">

  select.productSelect {font-size: 85%}
  td.productTd {font-size: 85%}
  input.productInput {font-size: 85%; background-color: #eeeeee}

/* device,set_resolution=[650, 500]
 * position=[0.05, 0.15, 0.95, 0.9]
 * 
 * the real image width  then is 650*0.90 = 585
 * the real image height then is 500*0.75 = 375
 * We need shrink the image using the same ration in both X and Y direction.
 * 
 * 5 times smaller, then whole image is 130 X 100
 * 5 times smaller, then real  image is 117 X 75
 */
  
  div.mapDivTitle
  {
    position: absolute;
    left: 215px;
    top: 170px;
    width: 163px;
    height: 15px;
    border: 0px white ridge;
    text-align: center;
    padding: 0px; 
    color: #000000;
  }

  div.mapDiv
  {
    position: absolute;
    left: 215px;
    top: 185px;
    width: 163px;
    height: 125px;
    border: 1px green ridge; 
    padding: 1px; 
    color: #947683;
  }

  div.mapDivTitle2
  {
    position: absolute;
    left: 215px;
    top: 320px;
    width: 163px;
    height: 15px;
    border: 0px white ridge;
    text-align: center;
    padding: 0px; 
    color: #000000;
  }

  div.mapDiv2
  {
    position: absolute;
    left: 215px;
    top: 335px;
    width: 163px;
    height: 125px;
    border: 1px green ridge; 
    padding: 1px; 
    color: #947683;
  }

  div.mapDivTitle3
  {
    position: absolute;
    left: 215px;
    top: 470px;
    width: 163px;
    height: 15px;
    border: 0px white ridge;
    text-align: center;
    padding: 0px; 
    color: #000000;
  }

  div.mapDiv3
  {
    position: absolute;
    left: 215px;
    top: 485px;
    width: 163px;
    height: 125px;
    border: 1px green ridge; 
    padding: 1px; 
    color: #947683;
  }

  div.mapDivTitle4
  {
    position: absolute;
    left: 215px;
    top: 620px;
    width: 163px;
    height: 15px;
    border: 0px white ridge;
    text-align: center;
    padding: 0px; 
    color: #000000;
  }

  div.mapDiv4
  {
    position: absolute;
    left: 215px;
    top: 635px;
    width: 163px;
    height: 125px;
    border: 1px green ridge; 
    padding: 1px; 
    color: #947683;
  }


  div.mapDivTitle5
  {
    position: absolute;
    left: 215px;
    top: 770px;
    width: 163px;
    height: 15px;
    border: 0px white ridge;
    text-align: center;
    padding: 0px; 
    color: #000000;
  }

  div.mapDiv5
  {
    position: absolute;
    left: 215px;
    top: 785px;
    width: 163px;
    height: 125px;
    border: 1px green ridge; 
    padding: 1px; 
    color: #947683;
  }



  div.line
  {
    position: absolute;
    visibility:visible;
    left: 215px;
    top: 185px;
    width:1px;
    height:1px;
    z-index:1;
    color:red;
    font-weight:bold;
    cursor:default;
  }

  div.line2
  {
    position: absolute;
    visibility:visible;
    left: 215px;
    top: 325px;
    width:1px;
    height:1px;
    z-index:1;
    color:red;
    font-weight:bold;
    cursor:default;
  }

  div.line3
  {
    position: absolute;
    visibility:visible;
    left: 215px;
    top: 465px;
    width:1px;
    height:1px;
    z-index:1;
    color:red;
    font-weight:bold;
    cursor:default;
  }

  div.line4
  {
    position: absolute;
    visibility:visible;
    left: 215px;
    top: 605px;
    width:1px;
    height:1px;
    z-index:1;
    color:red;
    font-weight:bold;
    cursor:default;
  }

  div.line5
  {
    position: absolute;
    visibility:visible;
    left: 215px;
    top: 745px;
    width:1px;
    height:1px;
    z-index:1;
    color:red;
    font-weight:bold;
    cursor:default;
  }



  img.mapImg
  {
    position:absolute;
    left:0px;
    top:0px;
  }



  img.mapCross
  {
    position:absolute;
    left:395px;
    top:220px;
  }


  div.panel2
  {
    position: absolute;
    left: 395px;
    top: 545px;
    width: 550px;
    height: 330px;
    border: 0px white ridge; 
    padding: 1px; 
    color: #947683;
  }


  div.panel3
  {
    position: absolute;
    left: 395px;
    top: 905px;
    width: 550px;
    height: 155px;
    border: 0px white ridge; 
    padding: 1px; 
    color: #947683;
  }




</style>

<script language="javascript" type="text/javascript" src="vertical.js"></script>

<script language="JavaScript">
  
  document.write("<div id=\"vertical_line\"   class=\"line\" > |\n|\n|\n|\n|\n|\n|\n|\n|\n</div>" );
  document.write("<div id=\"horizontal_line\" class=\"line\" >________________________</div>");

  document.write("<div id=\"vertical_line2\"   class=\"line2\" > |\n|\n|\n|\n|\n|\n|\n|\n|\n</div>" );
  document.write("<div id=\"horizontal_line2\" class=\"line2\" >________________________</div>");

  document.write("<div id=\"vertical_line3\"   class=\"line3\" > |\n|\n|\n|\n|\n|\n|\n|\n|\n</div>" );
  document.write("<div id=\"horizontal_line3\" class=\"line3\" >________________________</div>");

  document.write("<div id=\"vertical_line4\"   class=\"line4\" > |\n|\n|\n|\n|\n|\n|\n|\n|\n</div>" );
  document.write("<div id=\"horizontal_line4\" class=\"line4\" >________________________</div>");

  document.write("<div id=\"vertical_line5\"   class=\"line5\" > |\n|\n|\n|\n|\n|\n|\n|\n|\n</div>" );
  document.write("<div id=\"horizontal_line5\" class=\"line5\" >________________________</div>");

</script>

</head>


<body onLoad="init()">

<form name=form0 action="" method="post">

<div id="myMapTitle" class="mapDivTitle">
Temperature
</div>

<div class="mapDiv">
  <a id="hrefa" href="" target="_blank">
  <img  id="myMap" name="myMap"   src="images/static/world_map.jpg" width=163 height=125 
  	title="Temperature" alt="map image not exist" style="cursor:default;">
  </a>
</div>


<div class="mapDivTitle2">
Cloud Liquid Water
</div>

<div class="mapDiv2">
  <a id="hrefb" href="" target="_blank">
  <img  id="myMap2" name="myMap2" src="images/static/world_map.jpg" width=163 height=125 
  	title="Cloud Liquid Water" alt="clw image not exist" style="cursor:default;">
  </a>
</div>


<div class="mapDivTitle3">
Rain Water Path
</div>

<div class="mapDiv3">
  <a id="hrefc" href="" target="_blank">
  <img  id="myMap3" name="myMap3" src="images/static/world_map.jpg" width=163 height=125 
  	title="Rain Water Path" alt="rwp image not exist" style="cursor:default;">
  </a>
</div>


<div class="mapDivTitle4">
Ice Water Path
</div>

<div class="mapDiv4">
  <a id="hrefd" href="" target="_blank">
  <img  id="myMap4" name="myMap4" src="images/static/world_map.jpg" width=163 height=125 
  	title="Ice Water Path" alt="iwp image not exist" style="cursor:default;">
  </a>
</div>



<div id="myMap5Title" class="mapDivTitle5">
TRMM Rain Rate
</div>

<div class="mapDiv5">
  <a id="hrefe" href="" target="_blank">
  <img  id="myMap5" name="myMap5" src="images/static/world_map.jpg" width=163 height=125 
  	title="TRMM Rain Rate" alt="TRMM img has 6 day delay and you need go back 6 day" style="cursor:default;">
  </a>
</div>







</form>

<?php
// insert banner rows
require('includes/banner.inc');

// insert gray link bar
require('includes/toolBar_withSearch.inc');
?>
  <tr>
    <td id="navCell">
			<?php
			// insert navigation div
			require('includes/Sample_NavDiv.inc');
			?>
		</td>
		<td class="mainPanel"><a name="skipTarget"></a>
		
		<noscript><h3 class="noscriptWarning">
		Javascript is currently disabled on this computer. 
		Product animations on this page are generated with javascript.
		If you need static access to these images, please contact MIRS webmaster.</h3>
		</noscript>
		
			<div class="padding" id="monitor">
				<!-- DO NOT DELETE OR ALTER CODE ABOVE THIS COMMENT -->
				<!-- EXCEPT for the contents of the <title></title> TAG!! -->
				<!-- You can start project specific content HERE -->

				<FORM NAME=form  action="" method="post">


				<table border="0" cellpadding="0" cellspacing="0" class="tableGrid" bgcolor="#eeeeee">

				<TR align=left>

				<TR align=center>
				<TD align=center nowrap class="productTd">
				<B><font size=4>MIRS Vertical Cross Section Monitoring</font></B>
				<I>(move mouse over 5 left small maps for location)</I>
				</TD>
				</TR>

				<TR>
				<TD class="productTd" align=right width=650 height=915 valign=top>
				&nbsp;Sat
				<select class="productSelect" id="sat1" name="sat1" 
					onChange="changeRef_1( this.value ); changeMap2();loadImage1();loadImage2();" title="select a satellite">
				<option value="n18" 		>NOAA-18</option>
				<option value="n19"     	>NOAA-19</option>
				<option value="metopA"  	>METOP-A</option>
				<option value="metopB"  	>METOP-B</option>
				<option value="f16"		>F16/SSMIS</option>
				<option value="f18"		>F18/SSMIS</option>
				<option value="npp"		>NPP/ATMS</option>
				<option value="gcomw1"		>GCOMW1/AMSR2</option>
				<option value="trmm"		>TRMM/TMI</option>
				</select>
				
				&nbsp;Product:
				<select class="productSelect" id="prod1" name="prod1" title="select a product"
					onChange="changeMap2();loadImage1();" title="select a product">
				<option value="temp" 	    >Temperature</option>
				<option value="wv" 	    >Water Vapor Content</option>
				<option value="clwp" 	    >CLW</option>
				<option value="rainp" 	    >Rain</option>
				<option value="graupelp"    >Graupel</option>
				<option value="rain_graupel">Rain & Graupel</option>
				</select>
				
				&nbsp;Orbit:
				<select class="productSelect" id="cend1" name="cend1" title="select a passing mode (ascending/descending)"
					onChange="changeMap2();loadImage1();loadImage2();">
				<option VALUE="as" >Asc</option>
				<option VALUE="ds" >Des</option>
				</select>

				&nbsp;Area:
				<select class="productSelect" id="region1" name="region1" title="select a region"
					onChange="changeMap();loadImage1();loadImage2();">
				<option VALUE="glb" >Global</option>
				<option VALUE="us"  >USA</option>
				</select>
				
				<br>
				&nbsp;<font color=blue>View</font>:
				<select class="productSelect" id="view1" name="view1" title="select cross section direction: latitude or longitude"
					onChange="populateLatLon(this, document.form.slice1);loadImage1();loadImage2();">
				<option value="lat" >Lat</option>
				<option value="lon" >Lon</option>
				</select>
				
				<select class="productSelect" id="slice1" name="slice1" 
					onChange="changeSlice();loadImage1();loadImage2();" title="select lat/lon location">
				<option value="90" >90</option>
				<option value="80" >80</option>
				<option value="70" >70</option>
				<option value="60" >60</option>
				<option value="50" >50</option>
				<option value="40" >40</option>
				<option value="30" >30</option>
				<option value="20" >20</option>
				<option value="10" >10</option>
				<option value="0"  >0</option>
				<option value="-10" >-10</option>
				<option value="-20" >-20</option>
				<option value="-30" >-30</option>
				<option value="-40" >-40</option>
				<option value="-50" >-50</option>
				<option value="-60" >-60</option>
				<option value="-70" >-70</option>
				<option value="-80" >-80</option>
				<option value="-90" >-90</option>
				</select>
				
				&nbsp;Year:
				<select class="productSelect" id="yr1" name="yr1" 
					onChange="changeMap2();loadImage1();loadImage2();" title="select a year">
				<option value="2011">2011</option> 
				<option value="2012">2012</option> 
				<option value="2013">2013</option>
				<!--option value="2014">2014</option> 
				<option value="2015">2015</option> 
				<option value="2016">2016</option> 
				<option value="2017">2017</option> 
				<option value="2018">2018</option> 
				<option value="2019">2019</option> 
				<option value="2020">2020</option-->  
				</select>
				
				Month:
				<select class="productSelect" id="mo1" name="mo1" 
					onChange="changeMap2();loadImage1();loadImage2();" title="select a month">
				<option value="01">Jan</option>
				<option value="02">Feb</option>
				<option value="03">Mar</option>
				<option value="04">Apr</option>
				<option value="05">May</option>
				<option value="06">Jun</option>
				<option value="07">Jul</option>
				<option value="08">Aug</option>
				<option value="09">Sep</option>
				<option value="10">Oct</option>
				<option value="11">Nov</option>
				<option value="12">Dec</option>
				</select>
				
				Day:
				<select class="productSelect" id="dy1" name="dy1" 
					onChange="changeMap2();loadImage1();loadImage2();" title="select a date">
				<option value="01">1 </option>   
				<option value="02">2 </option>   
				<option value="03">3 </option>   
				<option value="04">4 </option>   
				<option value="05">5 </option>   
				<option value="06">6 </option>   
				<option value="07">7 </option>   
				<option value="08">8 </option>   
				<option value="09">9 </option>   
				<option value="10">10</option>  	  
				<option value="11">11</option>  	  
				<option value="12">12</option>  	  
				<option value="13">13</option>  	  
				<option value="14">14</option>  	  
				<option value="15">15</option>  	  
				<option value="16">16</option>  	  
				<option value="17">17</option>  	  
				<option value="18">18</option>  	  
				<option value="19">19</option>  	  
				<option value="20">20</option>  	  
				<option value="21">21</option>  	  
				<option value="22">22</option>  	  
				<option value="23">23</option>  	  
				<option value="24">24</option>  	  
				<option value="25">25</option>  	  
				<option value="26">26</option>  	  
				<option value="27">27</option>  	  
				<option value="28">28</option>  	  
				<option value="29">29</option>  	 
				<option value="30">30</option>  	 
				<option value="31">31</option>  	 
				</select>
				
				&nbsp;Browse:
				<input 	class="productInput" type="button" 
					onClick="rev(document.form.yr1,document.form.mo1,document.form.dy1);" value="<-" title="previous day">
				<input  class="productInput" type="button" 
					onClick="fwd(document.form.yr1,document.form.mo1,document.form.dy1);" value="->" title="next day">
				<br><br>

				<a id="href1" href="" target="_blank" >
				<img name="img1" class="mapCross" src=""  alt=""  title="click it will open a new window with this image"
				width=550 height=325 style="display:block; clear:both;" /></a>

				<br>
				
				
				<div class="panel2">
				<hr>

				Reference:
				<select class="productSelect" id="layer1" name="layer1" 
					onChange="changeRef(this.value);loadImage2();" title="select a reference data set">
				<option value="" 	       >NOAA-18</option>
				<option value="trmm2a12"       >TRMM2A12</option>
				<option value="gdas_"	       >GDAS</option>
				<option value="ecmwf_"         >ECMWF</option>
				<option value="gdas_diff_"     >NOAA-18 - GDAS</option>
				<option value="ecmwf_diff_"    >NOAA-18 - ECMWF</option>
				<option value="trmm2a12_diff_" >NOAA-18 - TRMM2A12</option>
				</select>
				
				&nbsp; Product:
				<select class="productSelect" id="prod2" name="prod2" 
					onChange="loadImage2();" title="select a reference data product">
				<option value="temp" 	    >Temperature</option>
				<option value="wv" 	    >Water Vapor Content</option>
				<option value="clwp" 	    >CLW</option>
				<option value="rainp" 	    >Rain</option>
				<option value="graupelp"    >Graupel</option>
				<option value="rain_graupel">Rain & Graupel</option>
				</select>
				
				<br>
				<a id="href2" href="" target="_blank" >
				<img name="img2" src=""  alt=""  width=550 height=325 style="display:block; clear:both;" /></a>
				
				</div>
				
				<br>
				<div class="panel3">
				<hr>
				<a id="href3" href="" target="_blank" title="scan time difference between MIRS and TRMM2A12">
				<img name="img3" src=""  alt=""  width=550 height=150 style="display:block; clear:both;" /></a>
				</div>
				
				
				</TD>
				</TR>




				</TABLE>
				</FORM>


				<!-- END your project specific content HERE -->
				<!-- DO NOT DELETE OR ALTER BELOW THIS COMMENT! -->
			</div>
		</td>
	</tr>
<?php
// insert footer & javascript include for menuController
require('includes/footer.inc');
?>
</table>
</body>
</html>
