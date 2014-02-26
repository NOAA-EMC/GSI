<?php
// insert header that forbids caching and carries doctype
// and html tag;
require('includes/noCacheHeader.inc');
?>
<META name="verify-v1" content="CwbLBcFt9+GqRTgaLZsENmPnSWNB5MStHHdYsB7U2nI=">
<title>STAR - MIRS Project - Comparison to HIRS</title>
<?php
// style links and the icon link
// pulls in .css files for regular and print display
require('includes/styleLinks.inc');
?>

<?php

$sat='';  
$prod=''; 
$layer='';
$sfc='';
$yr='';   
$mo='';   
$dy='';   

if(isset($_POST['sat']))   { $sat   = $_POST['sat'];   }
if(isset($_POST['prod']))  { $prod  = $_POST['prod'];  }
if(isset($_POST['sfc']))   { $sfc   = $_POST['sfc'];   }
if(isset($_POST['yr']))    { $yr    = $_POST['yr'];    }
if(isset($_POST['mo']))    { $mo    = $_POST['mo'];    }
if(isset($_POST['dy']))    { $dy    = $_POST['dy'];    }

echo "<script> var sat   = '$sat';   </script>";
echo "<script> var prod  = '$prod';  </script>";
echo "<script> var sfc   = '$sfc';   </script>";
echo "<script> var yr    = '$yr';    </script>";
echo "<script> var mo    = '$mo';    </script>";
echo "<script> var dy    = '$dy';    </script>";

$sat2='';  
$prod2=''; 
$sfc2='';
$yr2='';   
$mo2='';   
$dy2='';   

if(isset($_POST['sat2']))   { $sat2   = $_POST['sat2'];   }
if(isset($_POST['prod2']))  { $prod2  = $_POST['prod2'];  }
if(isset($_POST['sfc2']))   { $sfc2   = $_POST['sfc2'];   }
if(isset($_POST['yr2']))    { $yr2    = $_POST['yr2'];    }
if(isset($_POST['mo2']))    { $mo2    = $_POST['mo2'];    }
if(isset($_POST['dy2']))    { $dy2    = $_POST['dy2'];    }

echo "<script> var sat2   = '$sat2';   </script>";
echo "<script> var prod2  = '$prod2';  </script>";
echo "<script> var sfc2   = '$sfc2';   </script>";
echo "<script> var yr2    = '$yr2';    </script>";
echo "<script> var mo2    = '$mo2';    </script>";
echo "<script> var dy2    = '$dy2';    </script>";

?>


<style type="text/css">

  select.optionvisible   {visibility:visible}
  select.optioninvisible {visibility:hidden}

  td.arrowvisible 	{visibility:visible}
  td.arrowinvisible 	{visibility:hidden} 
  
</style>

<script language="javascript" type="text/javascript" src="validationhirs.js"></script>


</head>
<body  onLoad="loadInitialImagesv(sat,prod,sfc,yr,mo,dy)">
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
			require('includes/Sample_NavDiv_validationhirsv.inc');
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


				<FORM NAME=form action="validationhirs.php" method="post">

				<table border="0" cellpadding="0" cellspacing="0" class="tableGrid" bgcolor="#eeeeee">

				<TR><TD align=center bgcolor="#eeeeee" height=18>
				<input  type=submit  value="2 X 2 Panels" style="background-color: lightblue" > &nbsp;&nbsp;
				<B><font size=4>MIRS Comparison to HIRS</font></B> (<em>Click image for large view</em>)
				</TD></TR>

				<TR>
				<TD align=center>

				&nbsp;
				<select name="sat" onChange="loadImage_top();"  title="Select a sensor" >
				<option value="n18"	      	>NOAA-18</option>
				<option value="n19"	      	>NOAA-19</option>
				<option value="metopA"        	>METOP-A</option>
				<option value="metopB"        	>METOP-B</option>
				<option value="f16"	      	>F16/SSMIS</option>
				<option value="f18"	      	>F18/SSMIS</option>
				<option value="npp"		>NPP/ATMS</option>
				<option value="gcomw1"		>GCOMW1/AMSR2</option>
				</select>

				&nbsp;
				<select name="prod" onChange="loadImage_top();"  title="Select a product">
				<option value="tskin"   >Skin Temperature</option>
				<!--option value="temp"   >Temperature Profile</option>
				<option value="wv"     >Water Vapor Profile</option>  
				<option value="tpw"    >TPW</option-->  
				</select>
                                
				&nbsp;
				<!--select id="layer" name="layer" onChange="loadImage_top();"  title="Select a layer">
				<option value="100mb"   >100mb</option>
				<option value="200mb"   >200mb</option>
				<option value="300mb"   >300mb</option>
				<option value="400mb"   >400mb</option>
				<option value="500mb"   >500mb</option>
				<option value="600mb"   >600mb</option>
				<option value="700mb"   >700mb</option>
				<option value="800mb"   >800mb</option>
				<option value="850mb"   >850mb</option>
				<option value="900mb"   >900mb</option>
				<option value="950mb"   >950mb</option>
                                </select-->

				&nbsp;
				<select id="sfc" name="sfc" onChange="loadImage_top();"  title="Select a surface type">
				<option value="sea">Sea</option>
				<option value="lnd">Land</option>
				</select>			  

				&nbsp;
				<select id="yr" name="yr" onChange="loadImage_top();"  title="Select a year">
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
				<select id="mo" name="mo" onChange="loadImage_top();"  title="Select a month">	  
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
				<select id="dy" name="dy" onChange="loadImage_top();"  title="Select a day">	  
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
				<input type="button" onclick="rev();" value="<="  title="Previous day">
				<input type="button" onclick="fwd();" value="=>"  title="Next day">
				
				</TD>
				</TR>


				<TR>
				  <TD align=center nowrap width=650 height=500>
				    <a id="href1" href="" target=_blank>
				    <img name="img1" src="" align=center width=650 height=500 alt="" title="all"></a>
				  </TD>
				</TR>

				<TR>
				  <TD align=center nowrap width=650 height=500>
				    <a id="href2" href="" target=_blank>
				    <img name="img2" src="" align=center width=650 height=500 alt=""></a>
				  </TD>
				</TR>

				<TR>
				  <TD id="box3" align=center nowrap width=650 height=500>
				    <a id="href3" href="" target=_blank>
				    <img name="img3" src="" align=center width=650 height=500 alt=""></a>
				  </TD>
				</TR>

				<TR>
				  <TD id="box4" align=center nowrap width=0 height=0>
				    <a id="href4" href="" target=_blank>
				    <img name="img4" src="" align=center width=0 height=0 alt=""></a>
				  </TD>
				</TR>


				<TR>
				<TD align=center>
				&nbsp;
				<select name="sat2" onChange="loadImage_bottom();"  title="Select a sensor" >
				<option value="n18"	      	>NOAA-18</option>
				<option value="n19"	      	>NOAA-19</option>
				<option value="metopA"        	>METOP-A</option>
				<option value="metopB"        	>METOP-B</option>
				<option value="f16"	      	>F16/SSMIS</option>
				<option value="f18"	      	>F18/SSMIS</option>
				<option value="npp"		>NPP/ATMS</option>
				<option value="gcomw1"		>GCOMW1/AMSR2</option>
				</select>

				&nbsp;
				<select name="prod2" onChange="loadImage_bottom();"  title="Select a product">
				<option value="tskin"   >Skin Temperature</option>
				<!--option value="temp" >Temperature Profile</option>
				<option value="wv"     	>Water Vapor Profile</option>  
				<option value="tpw"    	>TPW</option-->  
				</select>

				&nbsp;
				<!--select id="layer2" name="layer2" onChange="loadImage_bottom();">
				<option value="100mb"   >100mb</option>
				<option value="200mb"   >200mb</option>
				<option value="300mb"   >300mb</option>
				<option value="400mb"   >400mb</option>
				<option value="500mb"   >500mb</option>
				<option value="600mb"   >600mb</option>
				<option value="700mb"   >700mb</option>
				<option value="800mb"   >800mb</option>
				<option value="850mb"   >850mb</option>
				<option value="900mb"   >900mb</option>
				<option value="950mb"   >950mb</option>
                                </select-->
				  			    
				&nbsp;
				<select id="sfc2" name="sfc2" onChange="loadImage_bottom();"  title="Select a surface type">
				<option value="sea">Sea</option>
				<option value="lnd">Land</option>
				</select>
				  			    
				&nbsp;
				<select id="yr2" name="yr2" onChange="loadImage_bottom();"  title="Select a year">
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
				<select id="mo2" name="mo2" onChange="loadImage_bottom();"  title="Select a month">	  
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
				<select id="dy2" name="dy2" onChange="loadImage_bottom();"  title="Select a day">	  
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
				<input type="button" onclick="rev();" value="<="  title="Previous day">
				<input type="button" onclick="fwd();" value="=>"  title="Next day">

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
