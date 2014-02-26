<?php
// insert header that forbids caching and carries doctype
// and html tag;
require('includes/noCacheHeader.inc');
?>
<META name="verify-v1" content="CwbLBcFt9+GqRTgaLZsENmPnSWNB5MStHHdYsB7U2nI=">
<title>STAR - MIRS Project - Geophysical Performances Monitoring (NWP)</title>
<?php
// style links and the icon link
// pulls in .css files for regular and print display
require('includes/styleLinks.inc');
?>

<?php
$ref='';  
$sat='';  
$prod=''; 
$layer='';
$cond='';  
$cend=''; 
$sfc='';  
$yr='';   
$mo='';   
$dy='';   

if(isset($_POST['ref']))   { $ref   = $_POST['ref'];   }
if(isset($_POST['sat']))   { $sat   = $_POST['sat'];   }
if(isset($_POST['prod']))  { $prod  = $_POST['prod'];  }
if(isset($_POST['layer'])) { $layer = $_POST['layer']; }
if(isset($_POST['cond']))  { $cond  = $_POST['cond'];  }
if(isset($_POST['cend']))  { $cend  = $_POST['cend'];  }
if(isset($_POST['sfc']))   { $sfc   = $_POST['sfc'];   }
if(isset($_POST['yr']))    { $yr    = $_POST['yr'];    }
if(isset($_POST['mo']))    { $mo    = $_POST['mo'];    }
if(isset($_POST['dy']))    { $dy    = $_POST['dy'];    }

echo "<script>\n";
echo " var ref   = '$ref';   \n";
echo " var sat   = '$sat';   \n";
echo " var prod  = '$prod';  \n";
echo " var layer = '$layer'; \n";
echo " var cond  = '$cond';  \n";
echo " var cend  = '$cend';  \n";
echo " var sfc   = '$sfc';   \n";
echo " var yr    = '$yr';    \n";
echo " var mo    = '$mo';    \n";
echo " var dy    = '$dy';    \n";
echo "</script>\n";

$ref2='';  
$sat2='';  
$prod2=''; 
$layer2='';
$cond2='';  
$cend2=''; 
$sfc2='';  
$yr2='';   
$mo2='';   
$dy2='';   

if(isset($_POST['ref2']))   { $ref2   = $_POST['ref2'];   }
if(isset($_POST['sat2']))   { $sat2   = $_POST['sat2'];   }
if(isset($_POST['prod2']))  { $prod2  = $_POST['prod2'];  }
if(isset($_POST['layer2'])) { $layer2 = $_POST['layer2']; }
if(isset($_POST['cond2']))  { $cond2  = $_POST['cond2'];  }
if(isset($_POST['cend2']))  { $cend2  = $_POST['cend2'];  }
if(isset($_POST['sfc2']))   { $sfc2   = $_POST['sfc2'];   }
if(isset($_POST['yr2']))    { $yr2    = $_POST['yr2'];    }
if(isset($_POST['mo2']))    { $mo2    = $_POST['mo2'];    }
if(isset($_POST['dy2']))    { $dy2    = $_POST['dy2'];    }

echo "<script>\n";
echo " var ref2   = '$ref2';   \n";
echo " var sat2   = '$sat2';   \n";
echo " var prod2  = '$prod2';  \n";
echo " var cond2  = '$cond2';  \n";
echo " var layer2 = '$layer2'; \n";
echo " var cend2  = '$cend2';  \n";
echo " var sfc2   = '$sfc2';   \n";
echo " var yr2    = '$yr2';    \n";
echo " var mo2    = '$mo2';    \n";
echo " var dy2    = '$dy2';    \n";
echo "</script>\n";

?>


<style type="text/css">

  select.optionvisible   {visibility:visible}
  select.optioninvisible {visibility:hidden}

  td.arrowvisible 	{visibility:visible}
  td.arrowinvisible 	{visibility:hidden} 
  
</style>

<script language="javascript" type="text/javascript" src="geonwp.js"></script>


</head>
<body  onLoad="loadInitialImagesV(ref,sat,prod,layer,cond,cend,sfc,yr,mo,dy)">
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
			require('includes/Sample_NavDiv_geonwpv.inc');
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


				<FORM NAME=form action="geonwp.php" method="post">

				<table border="0" cellpadding="0" cellspacing="0" class="tableGrid" bgcolor="#eeeeee">
				
				<TR><TD align=center bgcolor="#eeeeee" height=18>
				<input  type=submit  value="2 Column Panel" style="background-color: lightblue" 
					title="2 columns / row page format with small images"> &nbsp;&nbsp;
				<B><font size=4>MIRS Geophysical Performance Monitoring (NWP) </font></B>
				</TD></TR>


				<TR>
				<TD align=center>

				Mode:
				<select name="cend" onChange="changeCend(this.value);loadImage();" 
					title="Select a passing mode (Asc: ascending or Des: descending)">
				<option value="as" >Asc</option>
				<option value="ds" >Des</option>
				</select>			  
				
				&nbsp;Surface:
				<select name="sfc" onChange="changeSfc(this.value);loadImage();" title="Select a surface type">
				<option value="sea" >Sea</option>
				<option value="lnd" >Land</option>
				<option value="all" >All</option>
				</select>
				
				<!--  			    
				&nbsp;Region:
				<select name="region">
				<option value="glb" >Globe</option>
				<option value="us"  >US</option>
				<option value="na"  >North America</option>
				<option value="eu"  >Europe</option>
				<option value="af"  >North Africa</option>
				<option value="as"  >Asia</option>
				</select-->
				
				&nbsp;Year:
				<select id="yr" name="yr" onChange="changeYear(this.value);loadImage();" title="Select a year">
				<option value="2011">2011</option> 
				<option value="2012">2012</option> 
				<option value="2013">2013</option>
				<option value="2014">2014</option> 
				<option value="2015">2015</option> 
				<option value="2016">2016</option> 
				<option value="2017">2017</option> 
				<option value="2018">2018</option> 
				<option value="2019">2019</option> 
				<option value="2020">2020</option>  
				</select>			  
				
				Month:
				<select id="mo" name="mo" onChange="changeMonth(this.value);loadImage();" title="Select a month">	  
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
				<select id="dy" name="dy" onChange="changeDay(this.value);loadImage();" title="Select a day">	  
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
				<input type="button" onclick="rev();" value="<=" title="previous day">
				<input type="button" onclick="fwd();" value="=>" title="next day">
				
				<br>
				Ref Data:
				<select id="ref" name="ref" onChange="changeRef(this.value); loadImage();" title="select a reference data set">
				 <option value="gdas"  >GDAS</option>
				 <option value="ecmwf" >ECMWF</option>
				</select>				
				  			    
				&nbsp;&nbsp;Sat:
				<select name="sat" onChange="changeSat( this.value ); loadImage();" title="select a satellite sensor">
				<option value="n18"    >NOAA-18</option>
				<option value="n19"    >NOAA-19</option>
				<option value="metopA" >METOP-A</option>
				<option value="metopB" >METOP-B</option>
				<option value="f16"    >F16/SSMIS</option>
				<option value="f18"    >F18/SSMIS</option>
				<option value="npp"    >NPP/ATMS</option>
				<option value="gcomw1" >GCOMW1/AMSR2</option>
                                <option value="trmm"  title="TRMM/TMI data has 3 more days delay relative to other sensors" >TRMM/TMI</option>
				<option value="mtma"  title="MT-MADRAS is based on simulated proxy data for testing purposes only" >MT/MADRAS</option>
				<option value="mtsa"  title="MT-SAPHIR is based on simulated proxy data for testing purposes only" >MT/SAPHIR</option>
				</select>

				&nbsp;Product:
				<select name="prod" title="select a product"  
					onChange="changeProd( this.value ); changeSizeV( this.value ); loadImage();">
				<option value="tpw"   >TPW</option> 
				<option value="tskin" >Skin Temp.</option>
				<option value="em"    >Emissivity</option>   
				<option value="temp"  >Temp. Profile</option>
				<option value="wv"    >Water Vapor Profile</option>
				</select>

				<select id="layer" name="layer" class="optioninvisible" onChange="changeLayer( this.value ); loadImage();"
					title="select a layer(temp/wv) or a channel(emissivity)">
				</select>

				<select id="cond" name="cond" class="optionvisible" title="select a weather condition"
					onChange="changeCond( this.value ); loadImage();" >
				<option value="allcond_" >All Cond</option>
				<option value="clear_"   >Clear</option>
				<option value="cloudy_"  >Cloudy</option>
				<option value="rainy_"   >Rainy</option>
				</select>


				</TD>
				</TR>


				<TR>
				  <TD align=center nowrap width=650 height=500>
				    <a id="href1" href="" target=_blank>
				    <img name="img1" src="" width=650 height=500 align=center alt="mirs retrieval" title="mirs retrieval"></a>
				  </TD>
				</TR>

				<TR>
				  <TD align=center nowrap width=650 height=500>
				    <a id="href2" href="" target=_blank>
				    <img name="img2" src="" width=650 height=500 align=center alt="ecmwf"></a>
				  </TD>
				</TR>

				<TR>
				  <TD align=center nowrap width=650 height=500>
				    <a id="href3" href="" target=_blank>
				    <img name="img3" src=""  width=650 height=500 align=center alt="difference of mirs and ecmwf"></a>
				  </TD>
				</TR>

				<TR>
				  <TD align=center nowrap width=650 height=500>
				    <a id="href4" href="" target=_blank>
				    <img name="img4" src=""  width=650 height=500 align=center alt="asymmetry of difference"></a>
				  </TD>
				</TR>

				<TR>
				  <TD align=center nowrap width=650 height=500>
				    <a id="href5" href="" target=_blank>
				    <img name="img5" src=""  width=650 height=500 align=center alt="scattered plot"></a>
				  </TD>
				</TR>

				<TR>
				  <TD align=center nowrap width=650 height=500>
				    <a id="href6" href="" target=_blank>
				    <img name="img6" src=""  width=650 height=500 align=center alt=""></a>
				  </TD>
				</TR>


				<TR id="box7">
				  <TD align=center nowrap width=650 height=500>
				    <a id="href7" href="" target=_blank>
				    <img name="img7" src=""  width=650 height=500 align=center alt=""></a>
				  </TD>
				</TR>


				<TR id="box8">
				  <TD align=center nowrap width=650 height=500>
				    <a id="href8" href="" target=_blank>
				    <img name="img8" src=""  width=650 height=500 align=center alt=""></a>
				  </TD>
				</TR>


				<TR id="box9">
				  <TD align=center nowrap width=650 height=500>
				    <a id="href9" href="" target=_blank>
				    <img name="img9" src=""  width=650 height=500 align=center alt=""></a>
				  </TD>
				</TR>


				<TR id="boxa">
				  <TD align=center nowrap width=650 height=500>
				    <a id="hrefa" href="" target=_blank>
				    <img name="imga" src=""  width=650 height=500 align=center alt=""></a>
				  </TD>
				</TR>


				<TR>
				<TD align=center colspan=2>

				Mode:
				<select name="cend2" onChange="changeCend(this.value);loadImage_bottom();" 
					title="Select a passing mode (Asc: ascending or Des: descending)">
				<option value="as" >Asc</option>
				<option value="ds" >Des</option>
				</select>			  
				
				&nbsp;Surface:
				<select name="sfc2" onChange="changeSfc(this.value);loadImage_bottom();" title="select a surface type">
				<option value="sea" >Sea</option>
				<option value="lnd" >Land</option>
				<option value="all" >All</option>
				</select>
				
				<!--  			    
				&nbsp;Region:
				<select name="region2">
				<option value="glb" >Globe</option>
				<option value="us"  >US</option>
				<option value="na"  >North America</option>
				<option value="eu"  >Europe</option>
				<option value="af"  >North Africa</option>
				<option value="as"  >Asia</option>
				</select-->
				
				&nbsp;Year:
				<select id="yr2" name="yr2" onChange="changeYear(this.value);loadImage_bottom();" title="select a year">
				<option value="2011">2011</option> 
				<option value="2012">2012</option> 
				<option value="2013">2013</option>
				<option value="2014">2014</option> 
				<option value="2015">2015</option> 
				<option value="2016">2016</option> 
				<option value="2017">2017</option> 
				<option value="2018">2018</option> 
				<option value="2019">2019</option> 
				<option value="2020">2020</option>  
				</select>			  
				
				Month:
				<select id="mo2" name="mo2" onChange="changeMonth(this.value);loadImage_bottom();" title="select a month">	  
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
				<select id="dy2" name="dy2" onChange="changeDay(this.value);loadImage_bottom();" title="select a day">	  
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
				<input type="button" onclick="rev();" value="<=" title="previous day">
				<input type="button" onclick="fwd();" value="=>" title="next day">
				
				<br>
				Ref Data:
				<select id="ref2" name="ref2" onChange="changeRef(this.value); loadImage_bottom();" title="select a reference data set">
				 <option value="gdas"  >GDAS</option>
				 <option value="ecmwf" >ECMWF</option>
				</select>				
				&nbsp;&nbsp;Sat:
				<select name="sat2" title="select a satellite sensor" 
					onChange="changeSat( this.value ); loadImage_bottom();" >
				<option value="n18"    >NOAA-18</option>
				<option value="n19"    >NOAA-19</option>
				<option value="metopA" >METOP-A</option>
				<option value="metopB" >METOP-B</option>
				<option value="f16"    >F16/SSMIS</option>
				<option value="f18"    >F18/SSMIS</option>
				<option value="npp"    >NPP/ATMS</option>
				<option value="gcomw1" >GCOMW1/AMSR2</option>
                                <option value="trmm"  title="TRMM/TMI data has 3 more days delay relative to other sensors" >TRMM/TMI</option>
				<option value="mtma"  title="MT-MADRAS is based on simulated proxy data for testing purposes only" >MT/MADRAS</option>
				<option value="mtsa"  title="MT-SAPHIR is based on simulated proxy data for testing purposes only" >MT/SAPHIR</option>
				</select>

				&nbsp;Product:
				<select name="prod2" title="select a product" 
					onChange="changeProd( this.value ); changeSizeV( this.value ); loadImage_bottom();">
				<option value="tpw"   >TPW</option> 
				<option value="tskin" >Skin Temp.</option>
				<option value="em"    >Emissivity</option>   
				<option value="temp"  >Temp. Profile</option>
				<option value="wv"    >Water Vapor Profile</option>
				</select>

				<select id="layer2" name="layer2" class="optioninvisible" 
					onChange="changeLayer( this.value ); loadImage_bottom();"
					title="select a layer(temp/wv) or a channel(emissivity)">
				</select>

				&nbsp
				<select id="cond2" name="cond2" class="optionvisible" title="select a weather condition" 
					onChange="changeCond( this.value ); loadImage_bottom();" >
				<option value="allcond_" >All Cond</option>
				<option value="clear_"   >Clear</option>
				<option value="cloudy_"  >Cloudy</option>
				<option value="rainy_"   >Rainy</option>
				</select>
				
				</TD>
				</TR>

				</table>

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
