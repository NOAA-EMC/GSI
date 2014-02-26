<?php include("password_protect.php"); ?>

<?php
// insert header that forbids caching and carries doctype
// and html tag;
require('includes/noCacheHeader.inc');
?>
<META name="verify-v1" content="CwbLBcFt9+GqRTgaLZsENmPnSWNB5MStHHdYsB7U2nI=">
<title>STAR - MIRS Project - Comparison SSMIS Sea Ice</title>


<?php
// style links and the icon link
// pulls in .css files for regular and print display
require('includes/styleLinks.inc');
?>

<?php

$sat = '';  
$plot = '';  
$stat = ''; 
$prod = 'sice'; 
$view = '';  
$region = '';  
$cend = ''; 
$yr = '';   
$mo = '';   
$dy = '';   

if(isset($_POST['sat']))   { $sat   = $_POST['sat'];   }
if(isset($_POST['plot']))  { $plot  = $_POST['plot'];  }
if(isset($_POST['stat']))  { $stat  = $_POST['stat'];  }
//if(isset($_POST['prod']))  { $prod  = $_POST['prod'];  }
if(isset($_POST['region']))  { $region  = $_POST['region'];  }
if(isset($_POST['view']))  { $view  = $_POST['view'];  }
if(isset($_POST['cend']))  { $cend  = $_POST['cend'];  }
if(isset($_POST['yr']))    { $yr    = $_POST['yr'];    }
if(isset($_POST['mo']))    { $mo    = $_POST['mo'];    }
if(isset($_POST['dy']))    { $dy    = $_POST['dy'];    }

echo "<script>\n";
echo " var sat   = '$sat';   \n";
echo " var plot  = '$plot';  \n";
echo " var stat  = '$stat';  \n";
echo " var prod  = '$prod';  \n";
echo " var region = '$region';  \n";
echo " var view  = '$view';  \n";
echo " var cend  = '$cend';  \n";
echo " var yr    = '$yr';    \n";
echo " var mo    = '$mo';    \n";
echo " var dy    = '$dy';    \n";

$sat2 = '';  
$plot2 = '';  
$stat2 = ''; 
$prod2 = 'sice'; 
$region2 = '';  
$view2 = '';  
$cend2 = ''; 
$yr2 = '';   
$mo2 = '';   
$dy2 = '';   

if(isset($_POST['sat2']))   { $sat2   = $_POST['sat2'];   }
if(isset($_POST['plot2']))  { $plot2  = $_POST['plot2'];  }
if(isset($_POST['stat2']))  { $stat2  = $_POST['stat2'];  }
//if(isset($_POST['prod2']))  { $prod2  = $_POST['prod2'];  }
if(isset($_POST['region2']))  { $region2  = $_POST['region2'];  }
if(isset($_POST['view2']))  { $view2  = $_POST['view2'];  }
if(isset($_POST['cend2']))  { $cend2  = $_POST['cend2'];  }
if(isset($_POST['yr2']))    { $yr2    = $_POST['yr2'];    }
if(isset($_POST['mo2']))    { $mo2    = $_POST['mo2'];    }
if(isset($_POST['dy2']))    { $dy2    = $_POST['dy2'];    }

echo " var sat2   = '$sat2';   \n";
echo " var plot2  = '$plot2';  \n";
echo " var stat2  = '$stat2';  \n";
echo " var prod2  = '$prod2';  \n";
echo " var region2 = '$region2';  \n";
echo " var view2  = '$view2';  \n";
echo " var cend2  = '$cend2';  \n";
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


<script language="javascript" type="text/javascript" src="geossmis.js"></script>

</head>
<body  onLoad="loadInit(sat,plot,stat,prod,region,view,cend,yr,mo,dy);">
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


				<FORM NAME=form action="geossmisv.php" method="post">

				<table border="0" cellpadding="0" cellspacing="0" class="tableGrid" bgcolor="#eeeeee">

				<TR><TD align=center bgcolor="#eeeeee" height=18 colspan=2>
				<input  type=submit  value="Vertical Panel" style="background-color: lightblue" 
					title="vertical page format with large images"> &nbsp;&nbsp;
				<B><font size=4>MIRS Comparison to SSMIS Sea Ice</font></B> (<em>Click image for large view</em>)
				</TD></TR>

				<TR>
				<TD align=center colspan=2>

				<select name="sat" onChange="loadImage();" title="select a satellite sensor">
				<option value="n18"    >NOAA-18</option>
				<option value="n19"    >NOAA-19</option>
				<option value="metopA" >METOP-A</option>
				<option value="metopB" >METOP-B</option>
				<option value="npp"    >NPP/ATMS</option>
				<option value="f16"    >F16/SSMIS</option>
				<option value="f18"    >F18/SSMIS</option>
				</select>

				&nbsp;
				<select name="plot" onChange="changePlot( this.value ); loadImage();" title="select a plot type">
				<option value="map" >Map</option>  
				<option value="his" >Histogram</option>   
				<option value="ts"  >Time Series</option>   
				</select>

				&nbsp;
				<select id="stat" name="stat" class="optioninvisible" onChange="changeStat( this.value ); loadImage();" title="select a stat to be the first one">
				<option value="hss">Heidke Skill Score		</option>
				<option value="pod">Pr. of Detection		</option> 
				<option value="far">False Alarm Ratio		</option>
				<option value="fom">Frequency of Misses 	</option> 
				<option value="foh">Frequency of Hits		</option> 
				<option value="pon">Pr. of a Null Event 	</option> 
				<option value="dfr">Detection Failure Ratio	</option> 
				<option value="fcn">Freq. of Corr. Null Fcst  	</option> 
				<option value="pfd">Pr. of False Detection	</option> 
				<option value="bia">Bias			</option> 
				<option value="std">Standard Deviation		</option> 
				<option value="cor">Correlation 		</option>
				</select>

				<br>
								
				<select id="region" name="region" class="optioninvisible" onChange="loadImage();" title="select a region">
				<option value="glb" >Globe</option>
				<option value="nh"  >N. Hemi.</option>
				<option value="sh"  >S. Hemi.</option>
				</select>
				
				&nbsp;
				<select id="view" name="view" onChange="loadImage();" title="select a map projection type">
				<option value="cyl" >Cyl</option>
				<option value="pn"  >PN</option>
				<option value="ps"  >PS</option>
				</select>
				
				
				&nbsp;
				<select name="cend" onChange="loadImage();" title="select a passing mode (Asc: ascending or Des: descending)">
				<option value="as" >Asc</option>
				<option value="ds" >Des</option>
				</select>			  
				
				&nbsp;
				<select id="yr" name="yr" onChange="loadImage();" title="select a year">
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
				
				&nbsp;
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
				
				&nbsp;
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
				    <img name="img1" src="" align=center  width=325 height=250 alt="MiRS retrieval" title=""></a>
				  </TD>

				  <TD align=center nowrap width=325 height=275>
				    <a id="href2" href="" target=_blank>
				    <img name="img2" src="" align=center width=325 height=250 alt="SSMIS collocated"></a>
				  </TD>
				</TR>

				<TR>
				  <TD id="box3" align=center nowrap width=325 height=275>
				    <a id="href3" href="" target=_blank>
				    <img name="img3" src="" align=center  width=325 height=250 alt="Bias Map"></a>
				  </TD>

				  <TD id="box4" align=center nowrap width=325 height=275>
				    <a id="href4" href="" target=_blank>
				    <img name="img4" src="" align=center width=325 height=250 alt="p2p scattered plot"></a>
				  </TD>
				</TR>


				<TR>
				  <TD id="box5"  align=center nowrap width=325 height=275 style="display:table-cell">
				    <a id="href5" href="" target=_blank>
				    <img name="img5" src="" align=center  width=325 height=250 alt=""></a>
				  </TD>
				  
				  <TD id="box6"  align=center nowrap width=325 height=275 style="display:none">
				    <a id="href6" href="" target=_blank>
				    <img name="img6" src="" align=center  width=325 height=250 alt=""></a>
				  </TD>
				</TR>


				<TR>
				  <TD id="box7" align=center nowrap width=325 height=275 style="display:none">
				    <a id="href7" href="" target=_blank>
				    <img name="img7" src="" align=center width=325 height=250 alt=""></a>
				  </TD>
				  
				  <TD id="box8" align=center nowrap width=325 height=275 style="display:none">
				    <a id="href8" href="" target=_blank>
				    <img name="img8" src="" align=center width=325 height=250 alt=""></a>
				  </TD>
				</TR>


				<TR>
				  <TD id="box9" align=center nowrap width=325 height=275 style="display:none">
				    <a id="href9" href="" target=_blank>
				    <img name="img9" src="" align=center width=325 height=250 alt=""></a>
				  </TD>
				  
				  <TD id="boxa" align=center nowrap width=325 height=275 style="display:none">
				    <a id="hrefa" href="" target=_blank>
				    <img name="imga" src="" align=center width=325 height=250 alt=""></a>
				  </TD>
				</TR>


				<TR>
				  <TD id="boxb" align=center nowrap width=325 height=275 style="display:none">
				    <a id="hrefb" href="" target=_blank>
				    <img name="imgb" src="" align=center width=325 height=250 alt=""></a>
				  </TD>
				  
				  <TD id="boxc" align=center nowrap width=325 height=275 style="display:none">
				    <a id="hrefc" href="" target=_blank>
				    <img name="imgc" src="" align=center width=325 height=250 alt=""></a>
				  </TD>
				</TR>



				
				<TR>
				<TD align=center colspan=2>

				<select name="sat2" onChange="loadImage2();" title="select a satellite sensor">
				<option value="n18"    >NOAA-18</option>
				<option value="n19"    >NOAA-19</option>
				<option value="metopA" >METOP-A</option>
				<option value="metopB" >METOP-B</option>
				<option value="npp"    >NPP/ATMS</option>
				<option value="f16"    >F16/SSMIS</option>
				<option value="f18"    >F18/SSMIS</option>
				</select>

				&nbsp;
				<select name="plot2" onChange="changePlot( this.value ); loadImage2();" title="select a plot type">
				<option value="map"   >Map</option>  
				<option value="his"   >Histogram</option>   
				<option value="ts"    >Time Series</option>   
				</select>

				&nbsp;
				<select id="stat2" name="stat2" class="optioninvisible" onChange="changeStat( this.value ); loadImage();" title="select a statistics">
				<option value="hss">Heidke Skill Score			</option>
				<option value="pod">Probability of Detection		</option> 
				<option value="far">False Alarm Ratio			</option>
				<option value="fom">Frequency of Misses 		</option> 
				<option value="foh">Frequency of Hits			</option> 
				<option value="pon">Probability of a Null Event 	</option> 
				<option value="dfr">Detection Failure Ratio		</option> 
				<option value="fcn">Frequency of Correct Null Forecasts </option> 
				<option value="pfd">Probability of False Detection	</option> 
				<option value="bia">Bias				</option> 
				<option value="std">Standard Deviation			</option> 
				<option value="cor">Correlation 			</option>
				</select>

				<br>
								
				<select id="region2" name="region2" class="optioninvisible" onChange="loadImage2();" title="select a region">
				<option value="glb" >Globe</option>
				<option value="nh"  >N. Hemi.</option>
				<option value="sh"  >S. Hemi.</option>
				</select>
				
				&nbsp;
				<select id="view2" name="view2" onChange="loadImage2();" title="select a map projection type">
				<option value="cyl" >Cyl</option>
				<option value="pn"  >PN</option>
				<option value="ps"  >PS</option>
				</select>
				
				&nbsp;
				<select name="cend2" onChange="loadImage2();" title="select a passing mode (Asc: ascending or Des: descending)">
				<option value="as" >Asc</option>
				<option value="ds" >Des</option>
				</select>			  
				
				&nbsp;
				<select id="yr2" name="yr2" onChange="loadImage2();" title="select a year">
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
				
				&nbsp;
				<select id="mo2" name="mo2" onChange="loadImage2();" title="select a month">	  
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
				
				&nbsp;
				<select id="dy2" name="dy2" onChange="loadImage2();" title="select a day">	  
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
