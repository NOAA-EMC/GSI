<?php
// insert header that forbids caching and carries doctype
// and html tag;
require('includes/noCacheHeader.inc');
?>
<META name="verify-v1" content="CwbLBcFt9+GqRTgaLZsENmPnSWNB5MStHHdYsB7U2nI=">
<title>STAR - MIRS Project - Daily Map Time Series</title>
<?php
// style links and the icon link
// pulls in .css files for regular and print display
require('includes/styleLinks.inc');
?>


<style type="text/css">
  select.optionvisible   	{visibility:visible}
  select.optioninvisible 	{visibility:hidden}
</style>


<script language="javascript" type="text/javascript" src="daily.js"></script>



</head>
<body onLoad="initialParameters();launch();">

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
				<TD align=center height=500>
				
				<b><font size=4>30-Day Loop of MIRS Daily Product Images</b></font>
				<br>


				<!-- ===============  parameter section ================ -->
				

				<select id="sat" name="sat" title="select a satellite sensor"
				onChange="populateAlgorithm(this.value); changeAlgorithmSatellite( document.form1.algorithm, this, document.form1.prod, document.form1.layer )">
  					<option value="n18"	    >NOAA-18</option>
  					<option value="n19" 	    >NOAA-19</option>
  					<option value="metopA"      >METOP-A</option>
					<option value="metopB"      >METOP-B</option>
  					<option value="f16" 	    >F16/SSMIS</option>
 					<option value="f18" 	    >F18/SSMIS</option>
 					<option value="npp" 	    >NPP/ATMS</option>
 					<option value="trmm" 	    >TRMM/TMI</option>
				</select>

				<select id="prod" name="prod"  onChange="changeProduct( this.value );" title="select a product">
<?php
require('includes/productOptions.inc');
?>
				</select>

				<select id="layer" name="layer" class="optioninvisible" onChange="changeLayer( this.value )">
				</select>

				<select id="region" name="region" onChange="changeRegion( this.value )" title="select a region">
  					<option value="glb" selected>Globe</option>
  					<option value="us"    >USA</option>
  					<option value="eu"    >Europe</option>
  					<option value="gulf"  >Gulf</option>
  					<option value="china" >China</option>
				</select>

				<select id="cend" name="cend" onChange="changeCend( this.value )" title="select a passing mode">
				  <option value="as" selected> Asc</option>
				  <option value="ds" > Des</option>
				  <option value="ad" > Des/Asc</option>
				</select>


				<select id="sfc" name="sfc" onChange="changeSfc( this.value )" title="select a surface type">
				  <option value="all" >All</option>
				  <option value="sea" >Sea</option>
				  <option value="lnd" >Land</option>
				</select>

				<select id="algorithm" name="algorithm" title="select an algorithm"
				onChange="changeAlgorithmSatellite( this, document.form1.sat, document.form1.prod, document.form1.layer )">
				  <option value="adv" selected> 1DVAR</option>
				  <option value="her"> MSPPS</option>
				</select>

				<br>

				<input type=button value="Start Loop" onClick="start()" title="Start Animation" style="background-color: lightblue;">
				<input type=button value="Stop Loop"  onClick="stop()"  title="Stop Animation"  style="background-color: lightblue;">

				<input type=button value=Prev onClick="decrementImage(--current_image)" title="Previous image">
				<input type=button value=Next onClick="incrementImage(++current_image)" title="Next image">

				<input type=button value=Backward onClick="change_mode(1);rev()" title="Backward Direction">
				<input type=button value=Forward  onClick="change_mode(1);fwd()" title="Fowward Direction">

				<input type=button value=Slower onClick="change_speed(delay_step)"  title="Slower Animation Speed">
				<input type=button value=Faster onClick="change_speed(-delay_step)" title="Speed up Animation Speed">

				<BR><BR>


				<!-- ================ image section ================== -->
				
				<img name="animation" src=""  width=650 height=500 alt="" style="display:block; clear:both;" />
				
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
