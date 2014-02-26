<?php
// insert header that forbids caching and carries doctype
// and html tag;
require('includes/noCacheHeader.inc');
?>
<META name="verify-v1" content="CwbLBcFt9+GqRTgaLZsENmPnSWNB5MStHHdYsB7U2nI=">
<title>STAR - MIRS Project - Climate Time Series Monitoring</title>
<?php
// style links and the icon link
// pulls in .css files for regular and print display
require('includes/styleLinks.inc');
?>

<style type="text/css">
  select.optionvisible          {font-size: 100%; visibility:visible}
  select.optioninvisible        {font-size: 100%; visibility:hidden}

  select.productSelect {font-size: 100%}
  td.productTd {font-size: 100%}
  input.productInput {font-size: 100%; background-color: #eeeeee}
  
</style>

<script language="javascript" type="text/javascript" src="climate_timeseries.js"></script>

</head>


<body onLoad="loadImage()">
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
				
				
				<FORM NAME=form action="" method="post">

				<table border="0" cellpadding="0" cellspacing="0" class="tableGrid" bgcolor="#eeeeee">

				<TR align=center>
				<TD align=center nowrap class="productTd" colspan=2>
				<B><font size=4>MIRS Climate Time Series</font></B>
				</TD>
				</TR>


				<TR>
				<TD class="productTd" id="panel1" align=center>
				Sensor:
				<select class="productSelect" id="sat1" name="sat1" 
					onChange="changeSensor(this.value)" title="select a sensor">
				<option value="aggr"   >Composite</option>
				<option value="n18"    >NOAA-18</option>
				<option value="n19"    >NOAA-19</option>
				<option value="metopA" >METOP-A</option>
				<option value="metopB" >METOP-B</option>
				<option value="f16"    >F16/SSMIS</option>
				<option value="f18"    >F18/SSMIS</option>
				<option value="npp"    >NPP/ATMS</option>
				<option value="trmm"   >TRMM/TMI</option>
				</select>
				
				&nbsp;
				Passing Mode:
				<select class="productSelect" id="cend1" name="cend1" 
					onChange="changeCend(this.value)" title="select a passing mode">
				<option value="as" >Asc</option>
				<option value="ds" >Des</option>
				<option value="ad" >Asc-Des</option>				
				</select>
				
				&nbsp;
				Surface Type:
				<select class="productSelect" id="sfc1" name="sfc1" 
					onChange="changeSfc(this.value)" title="select a surface type">
				<option value="GLOBE"  >Global</option>
				<option value="ocean"  >Open Ocean</option>
				<option value="amazon" >Amazon</option>
				<option value="desert" >Desert</option>
				<option value="snow"   >Snow</option>
				<option value="sicean" >Antarctic Sea Ice</option>
				<option value="sicear" >Arctic Sea Ice</option>			   
				<option value="sicefy" >First Year Sea Ice</option>
				<option value="land"   >Wet Land</option>			   
				<option value="OCEAN"  >ALL OCEAN</option>
				<option value="ICE"    >ALL ICE</option>
				<option value="LAND"   >ALL LAND</option>
				<option value="SNOW"   >ALL SNOW</option>
				</select>
				<br>
				
				Product:
				<select class="productSelect" id="prod1" name="prod1" 
					onChange="changeProduct(this.value)" title="select a product">
                                <option value="tskin">Skin Temperature</option>
				<!--option value="em">Emissivity</option-->
                                <option value="chisq">Chi Square</option>
                                <option value="clw">CLW</option>
                                <option value="gs">Snow Grain Size</option>
                                <option value="iwp">Ice Water Path</option>
                                <option value="lwp">Liquid Water Path</option>
                                <option value="psfc">Surface Pressure</option>
                                <option value="rr">Rain Rate</option>
                                <option value="rwp">Rain Water Path</option>
                                <option value="sice">Sea Ice Concentration</option>
                                <option value="sicefy">First Year SIC</option>
                                <option value="sicemy">Multiple Year SIC</option>
                                <option value="swe">Snow Water Equivalent</option>
                                <!--option value="ymCorr">TB</option-->
                                <!--option value="ym">Un-Corrected TB</option-->
                                <option value="temp">Temperature Profile</option>
                                <option value="tpw">TPW</option>
                                 <option value="wv">Water Vapor Profile</option>
				</select>
				
				&nbsp;
                                <select id="layer1" name="layer1" class="optioninvisible" 
					onChange="changeLayer(this.value)" title="select a channel">
				<!--option value="23v">23v</option>
				<option value="31v">31v</option>
				<option value="50v">50v</option>
				<option value="52v">52v</option>
				<option value="53h">53h</option>
				<option value="54h">54h</option>
				<option value="54v">54v</option>
				<option value="55h">55h</option>
				<option value="57h1">57h1</option>
				<option value="57h2">57h2</option>
				<option value="57h3">57h3</option>
				<option value="57h4">57h4</option>
				<option value="57h5">57h5</option>
				<option value="57h6">57h6</option>
				<option value="89v1">89v1</option>
				<option value="89v2">89v2</option>
				<option value="157h">157h</option>
				<option value="184h">184h</option>
				<option value="186h">186h</option>
				<option value="190h">190h</option-->
                                </select>
				
				&nbsp;
				<select id="angle1" name="angle1" class="optioninvisible" 
					onChange="changeAngle(this.value)" title="select local zenith angle range">
				<option value="0-20"  >LZA: 0&#176; ~ 20&#176; </option>
				<option value="20-40" >LZA: 20&#176; ~ 40&#176;</option>
				<option value="40-60" >LZA: 40&#176; ~ 60&#176;</option>				
				<option value="0-60"  >LZA: 0&#176; ~ 60&#176;</option>				
				</select>
				
				
				<br>
				<br>
				
				<a id="href1" href="" target="_blank" >
				<img name="img1" src="" alt=""  width=640 height=480 style="display:block; clear:both;" 
				     title="click this will pop up a new window with only the image inside">
				</a>
				
				<br>
				
				<a href="images/aggr/timeseries/sfcs.png" target="_blank">
				<img name="imgsfc" src="images/aggr/timeseries/sfcs.png" 
				     title="regions used to define different surface types">
				</a>
				
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
