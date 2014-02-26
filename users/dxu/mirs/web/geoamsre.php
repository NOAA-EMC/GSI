<?php include("password_protect.php"); ?>

<?php
// insert header that forbids caching and carries doctype
// and html tag;
require('includes/noCacheHeader.inc');
?>
<META name="verify-v1" content="CwbLBcFt9+GqRTgaLZsENmPnSWNB5MStHHdYsB7U2nI=">
<title>STAR - MIRS Project - Comparison to AMSR-E Snow and Sea Ice</title>


<?php
// style links and the icon link
// pulls in .css files for regular and print display
require('includes/styleLinks.inc');
?>


<?php

$sat = '';  
$prod = ''; 
$layer = '';
$cend = ''; 
$view = '';  
$yr = '';   
$mo = '';   
$dy = '';   

if(isset($_POST['sat']))   { $sat   = $_POST['sat'];   }
if(isset($_POST['prod']))  { $prod  = $_POST['prod'];  }
if(isset($_POST['layer'])) { $layer = $_POST['layer']; }
if(isset($_POST['cend']))  { $cend  = $_POST['cend'];  }
if(isset($_POST['view']))  { $view  = $_POST['view'];  }
if(isset($_POST['yr']))    { $yr    = $_POST['yr'];    }
if(isset($_POST['mo']))    { $mo    = $_POST['mo'];    }
if(isset($_POST['dy']))    { $dy    = $_POST['dy'];    }

echo "<script>\n";
echo " var sat   = '$sat';   \n";
echo " var prod  = '$prod';  \n";
echo " var layer = '$layer'; \n";
echo " var cend  = '$cend';  \n";
echo " var view  = '$view';  \n";
echo " var yr    = '$yr';    \n";
echo " var mo    = '$mo';    \n";
echo " var dy    = '$dy';    \n";
echo "</script>\n";

$sat2 = '';  
$prod2 = ''; 
$layer2 = '';
$cend2 = ''; 
$view2 = '';  
$yr2 = '';   
$mo2 = '';   
$dy2 = '';   

if(isset($_POST['sat2']))   { $sat2   = $_POST['sat2'];   }
if(isset($_POST['prod2']))  { $prod2  = $_POST['prod2'];  }
if(isset($_POST['layer2'])) { $layer2 = $_POST['layer2']; }
if(isset($_POST['cend2']))  { $cend2  = $_POST['cend2'];  }
if(isset($_POST['view2']))  { $view2  = $_POST['view2'];  }
if(isset($_POST['yr2']))    { $yr2    = $_POST['yr2'];    }
if(isset($_POST['mo2']))    { $mo2    = $_POST['mo2'];    }
if(isset($_POST['dy2']))    { $dy2    = $_POST['dy2'];    }

echo "<script>\n";
echo " var sat2   = '$sat2';   \n";
echo " var prod2  = '$prod2';  \n";
echo " var layer2 = '$layer2'; \n";
echo " var cend2  = '$cend2';  \n";
echo " var view2  = '$view2';  \n";
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


<script language="javascript" type="text/javascript" src="geoamsre.js"></script>

</head>
<body  onLoad="loadInitialImages(sat,prod,layer,cend,view,yr,mo,dy)">
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


				<FORM NAME=form action="geoamsrev.php" method="post">

				<table border="0" cellpadding="0" cellspacing="0" class="tableGrid" bgcolor="#eeeeee">

				<TR><TD align=center bgcolor="#eeeeee" height=18 colspan=2>
				<input  type=submit  value="Vertical Panel" style="background-color: lightblue" 
					title="vertical page format with large images"> &nbsp;&nbsp;
				<B><font size=4>MIRS Comparison to AMSR-E Snow and Sea Ice</font></B> (<em>Click image for large view</em>)
				</TD></TR>

				<TR>
				<TD align=center colspan=2>

				Mode:
				<select name="cend" onChange="loadImage();" title="select a passing mode (Asc: ascending or Des: descending)">
				<option value="as" >Asc</option>
				<option value="ds" >Des</option>
				</select>			  
				
				&nbsp;View:
				<select name="view" onChange="loadImage();" title="select a map projection type">
				<option value="cyl" >Global</option>
				<option value="pn"  >N. Hemisphere</option>
				<!--option value="ps"  >S. Hemisphere</option-->
				</select>
				
				&nbsp;Sensor:
				<select name="sat" onChange="loadImage();" title="select a satellite sensor">
				<option value="n18"    >NOAA-18</option>
				<option value="n19"    >NOAA-19</option>
				<option value="metopA" >METOP-A</option>
				<option value="f16"    >F16/SSMIS</option>
				<option value="f18"    >F18/SSMIS</option>
				</select>

				&nbsp;Product:
				<select name="prod" onChange="changeProduct( this.value ); changeSize( this.value ); loadImage();"
					title="select a product">
				<option value="swe"    >SWE</option>  
				<option value="sice"   >Sea Ice</option>   
				</select>

				<select id="layer" name="layer" class="optioninvisible" onChange="loadImage();" 
					title="select an algorithm">
				<option value="nt2"   >NASA Team 2 </option>   
				<option value="btp"   >Bootstrap </option>  
				</select>

				<br>
								
				&nbsp;Year:
				<select id="yr" name="yr" onChange="loadImage();" title="select a year">
				<option value="2000">2000</option>
				<option value="2001">2001</option> 
				<option value="2002">2002</option>
				<option value="2003">2003</option> 
				<option value="2004">2004</option> 
				<option value="2005">2005</option> 
				<option value="2006">2006</option> 
				<option value="2007">2007</option> 
				<option value="2008">2008</option> 
				<option value="2009">2009</option> 
				<option value="2010">2010</option> 
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
				<select id="mo" name="mo" onChange="loadImage();" title="select a month">	  
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
				<select id="dy" name="dy" onChange="loadImage();" title="select a day">	  
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
				
				</TD>
				</TR>


				<TR>
				  <TD align=center nowrap width=325 height=275>
				    <a id="href1" href="" target=_blank>
				    <img name="img1" src="" align=left  width=325 height=250 alt="mirs retrieval" title=""></a>
				  </TD>

				  <TD align=center nowrap width=325 height=275>
				    <a id="href2" href="" target=_blank>
				    <img name="img2" src="" align=right width=325 height=250 alt="nwp gdas"></a>
				  </TD>
				</TR>

				<TR>
				  <TD align=center nowrap width=325 height=275>
				    <a id="href3" href="" target=_blank>
				    <img name="img3" src="" align=left  width=325 height=250 alt=""></a>
				  </TD>

				  <TD align=center nowrap width=325 height=275>
				    <a id="href4" href="" target=_blank>
				    <img name="img4" src="" align=right width=325 height=250 alt=""></a>
				  </TD>
				</TR>


				<TR>
				  <TD id="box5"  align=center nowrap width=325 height=275>
				    <a id="href5" href="" target=_blank>
				    <img name="img5" src="" align=left  width=325 height=250 alt=""></a>
				  </TD>
				  
				  <TD id="box6"  align=center nowrap width=325 height=275>
				    <a id="href6" href="" target=_blank>
				    <img name="img6" src="" align=right  width=325 height=250 alt=""></a>
				  </TD>

				</TR>


				<TR>
				  <TD id="box7" align=center nowrap width=325 height=275>
				    <a id="href7" href="" target=_blank>
				    <img name="img7" src="" align=left width=325 height=250 alt=""></a>
				  </TD>
				  
				  <TD id="box8" align=center nowrap width=325 height=275>
				    <a id="href8" href="" target=_blank>
				    <img name="img8" src="" align=right width=325 height=250 alt=""></a>
				  </TD>

				</TR>

				<TR>
				<TD align=center colspan=2>

				Mode:
				<select name="cend2" onChange="loadImage_bottom();" 
					title="select a passing mode (Asc: ascending or Des: descending)">
				<option value="as" >Asc</option>
				<option value="ds" >Des</option>
				</select>			  
				
				&nbsp;View:
				<select name="view2" onChange="loadImage_bottom();" title="select a map projection type">
				<option value="cyl" >Global</option>
				<option value="pn"  >N. Hemisphere</option>
				<!--option value="ps"  >S. Hemisphere</option-->
				</select>
				
				&nbsp;Sensor:
				<select name="sat2" onChange="loadImage_bottom();" title="select a satellite sensor">
				<option value="n18"	>NOAA-18</option>
				<option value="n19"	>NOAA-19</option>
				<option value="metopA"  >METOP-A</option>
				<option value="f16"	>F16/SSMIS</option>
				<option value="f18"	>F18/SSMIS</option>
				</select>

				&nbsp;Product:
				<select name="prod2" onChange="changeProduct( this.value ); changeSize( this.value ); loadImage_bottom();" title="select a product">
				<option value="swe"    >SWE</option>  
				<option value="sice"   >Sea Ice</option> 
				</select>

				<select id="layer2" name="layer2" class="optioninvisible" onChange="loadImage_bottom();"
					title="select an algorithm">
				<option value="nt2"   >NASA Team 2</option>   
				<option value="btp"   >Bootstrap</option>  
				</select>
				
				<br>
				
				&nbsp;Year:
				<select id="yr2" name="yr2" onChange="loadImage_bottom();" title="select a year">
				<option value="2000">2000</option>
				<option value="2001">2001</option> 
				<option value="2002">2002</option>
				<option value="2003">2003</option> 
				<option value="2004">2004</option> 
				<option value="2005">2005</option> 
				<option value="2006">2006</option> 
				<option value="2007">2007</option> 
				<option value="2008">2008</option> 
				<option value="2009">2009</option> 
				<option value="2010">2010</option> 
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
				<select id="mo2" name="mo2" onChange="loadImage_bottom();" title="select a month">	  
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
				<select id="dy2" name="dy2" onChange="loadImage_bottom();" title="select a day">	  
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
