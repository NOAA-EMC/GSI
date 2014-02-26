<?php
// insert header that forbids caching and carries doctype
// and html tag;
require('includes/noCacheHeader.inc');
?>
<META name="verify-v1" content="CwbLBcFt9+GqRTgaLZsENmPnSWNB5MStHHdYsB7U2nI=">
<title>STAR - MIRS Project - Climate</title>
<?php
// style links and the icon link
// pulls in .css files for regular and print display
require('includes/styleLinks.inc');
?>

<style type="text/css">

  select.optionvisible   	{visibility:visible}
  select.optioninvisible 	{visibility:hidden}

</style>

<script language="javascript" type="text/javascript" src="climate.js"></script>

</head>
<body onLoad="initialParameters();loadImages();loadImages2();">

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
				
				<FORM name=form1>
				
				<table border="0" cellpadding="0" cellspacing="0" class="tableGrid" bgcolor="#eeeeee">
				<TR>
				<TD align=center>
				
				<b><font size=4>MIRS Climate Monitoring </b></font>
				<br>

				<select id="sat" name="sat" onChange="changeSatellite( this.value, 'layer' )" title="select a sensor">
  					<option value="n18"	    >NOAA-18</option>
  					<option value="n19" 	    >NOAA-19</option>
  					<option value="metopA"      >METOP-A</option>
					<option value="metopB"      >METOP-B</option>
  					<option value="f16" 	    >F16/SSMIS</option>
  					<option value="f18" 	    >F18/SSMIS</option>
  					<option value="npp" 	    >NPP/ATMS</option>
  					<option value="trmm" 	    >TRMM/TMI</option>
				</select>

				<select id="prod" name="prod" onChange="changeProduct( this.value, 'layer' )" title="select a product">
                                	<option value="clw">CLW</option>
                                	<option value="em">Emissivity</option>
                                	<option value="gs">Snow Grain Size</option>
                                	<option value="iwp">Ice Water Path</option>
                                	<option value="lwp">Liquid Water Path</option>
                                	<option value="rr">Rain Rate</option>
                                	<option value="rwp">Rain Water Path</option>
                                	<option value="sfc">Surface Type</option>
                                	<option value="sice">Sea Ice</option>
                                	<option value="sicefy">First Year SIC</option>
                                	<option value="sicemy">Multiple Year SIC</option>
                                	<option value="snow">Snow Cover</option>
                                	<option value="swe">SWE</option>
                                	<option value="tbc">Corr. TB</option>
                                	<option value="tbu">UnCorr. TB</option>
                                	<option value="temp">Temp. Profile</option>
                                	<option value="tpw">TPW</option>
                                	<option value="ts">Surface Temp.</option>
                                	<option value="wv">Water Vapor Profile</option>
				</select>

				<select id="layer" name="layer" class="optioninvisible" onChange="changeLayer( this.value )" title="select a layer/channel/map projection type">
				</select>
				
				<select id="resolution" name="resolution" onChange="changeResolution( this.value )" title="select a resolution">
  					<option value="1.0deg" >1.0 deg</option>
  					<option value="2.5deg" >2.5 deg</option>
				</select>

				<select id="year" name="year" onChange="changeYear( this.value )" title="select a year">
				  <option value="2008">2008</option>
				  <option value="2009">2009</option>
				  <option value="2010">2010</option>
				  <option value="2011">2011</option>
				  <option value="2012">2012</option>
				  <option value="2013">2013</option>
				</select>

				<select id="climateType" name="climateType" onChange="changeClimateType( this.value )" title="select a climate type">
				  <option value="pentad"  >Pentad</option>
				  <!--option value="weekly"  >Weekly</option-->
				  <option value="monthly" >Monthly</option>
				</select>

				<select id="alg" name="alg" onChange="changeAlg(this.value)" title="select an algorithm">
				  <option value="mirs_"  >MIRS</option>
				  <option value="mspps_" >MSPPS</option>
				</select>

				<br>
			       <input type="button" value="Start Loop" onClick="start()" title="Start animation loop" style="background-color: lightblue"> 
			       <input type="button" value="Stop Loop"  onClick="stop()"  title="Stop animation loop"  style="background-color: lightblue">
			       		
			       <input type="button" value="Prev" onClick="decrementImage(--current_image)" title="Previous image">
			       <input type="button" value="Next" onClick="incrementImage(++current_image)" title="Next image">

			       <input type="button" value="Backward" onClick="rev()" title="backward direction">
			       <input type="button" value="Forward"  onClick="fwd()" title="forward direction">  

			       <input type="button" value="Slower" onClick="change_speed(delay_step)"  title="slower animation speed">
			       <input type="button" value="Faster" onClick="change_speed(-delay_step)" title="increase animation speed">

				<BR><BR>

				<a id="href1" href="" target="_blank" >
				<img name="animation" src="" width="650" height="500" alt="" style="display:block; clear:both;" />
				</a>
				
				<BR><HR>
				



				<select id="sat2" name="sat2" onChange="changeSatellite2( this.value, 'layer2' )" title="select a sensor">
  					<option value="n18"	    >NOAA-18</option>
  					<option value="n19"	    >NOAA-19</option>
  					<option value="metopA"      >METOP-A</option>
					<option value="metopB"      >METOP-B</option>
  					<option value="f16" 	    >F16/SSMIS</option>
  					<option value="f18" 	    >F18/SSMIS</option>
  					<option value="npp" 	    >NPP/ATMS</option>
  					<option value="trmm" 	    >TRMM/TMI</option>
				</select>

				<select id="prod2" name="prod2"  onChange="changeProduct2( this.value , 'layer2' )" title="select a product">
                                	<option value="clw">CLW</option>
                                	<option value="iwp">Ice Water Path</option>
                                	<option value="rr">Rain Rate</option>
                                	<option value="sice">Sea Ice</option>
                                	<option value="tpw">TPW</option>
                                 	<option value="ts">Surface Temp.</option>
	
					<!--option value="em">Emissivity</option>
                                	<option value="gs">Snow Grain Size</option>
                                	<option value="lwp">Liquid Water Path</option>
                                	<option value="rwp">Rain Water Path</option>
                                	<option value="sicemy">Multiple Year SIC</option>
                                	<option value="sicefy">First Year SIC</option>
                                	<option value="snow">Snow Cover</option>
                                	<option value="swe">SWE</option>
                                	<option value="tbc">Corr. TB</option>
                                	<option value="tbu">UnCorr. TB</option>
                                	<option value="temp">Temp. Profile</option>
                                	<option value="wv">Water Vapor Profile</option-->
				</select>
				<select id="layer2" name="layer2" class="optioninvisible" onChange="changeLayer2( this.value )" title="select a layer/channel">
				</select>
				

				<select id="resolution2" name="resolution2" onChange="changeResolution2( this.value )" title="select a resolution">
  					<option value="1.0deg" >1.0 deg</option>
  					<option value="2.5deg" >2.5 deg</option>
				</select>

				<select id="year2" name="year2" onChange="changeYear2( this.value )" title="select a year">
				  <option value="2008">2008</option>
				  <option value="2009">2009</option>
				  <option value="2010">2010</option>
				  <option value="2011">2011</option>
				  <option value="2012">2012</option>
				  <option value="2013">2013</option>
				</select>

				<select id="climateType2" name="climateType2" onChange="changeClimateType2( this.value )" title="select a climate type">
				  <option value="pentad"  >Pentad</option>
				  <!--option value="weekly"  >Weekly</option-->
				  <option value="monthly" >Monthly</option>
				</select>

				<select id="alg2" name="alg2" onChange="changeAlg2( this.value )" title="select an algorithm">
				  <option value="mspps_" >MSPPS</option>
				  <option value="mirs_"  >MIRS</option>
				</select>

				<br>
				<input type="button" value="Start Loop" onClick="start2()" style="background-color: lightblue" title="Start animation loop" > 
				<input type="button" value="Stop Loop"  onClick="stop2()"  style="background-color: lightblue" title="Stop animation loop" > 

				<input type="button" value="Prev" onClick="decrementImage2(--current_image2)" title="Previous image">
				<input type="button" value="Next" onClick="incrementImage2(++current_image2)" title="Next image">

				<input type="button" value="Backward" onClick="rev2()" title="backward direction">
				<input type="button" value="Forward"  onClick="fwd2()" title="forward direction">

				<input type="button" value="Slower" onClick="change_speed2(delay_step2)"  title="slower animation speed">
				<input type="button" value="Faster" onClick="change_speed2(-delay_step2)" title="increase animation speed">

				<BR><BR>
				
				<a id="href2" href="" target="_blank" >
				<img name="animation2"  src=""  width=650 height=500 alt="" style="display:block; clear:both;" />
				</a>
				
				<BR>
				
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
