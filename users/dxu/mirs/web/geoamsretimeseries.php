<?php
// insert header that forbids caching and carries doctype
// and html tag;
require('includes/noCacheHeader.inc');
?>
<META name="verify-v1" content="CwbLBcFt9+GqRTgaLZsENmPnSWNB5MStHHdYsB7U2nI=">
<title>STAR - MIRS Project - Compare to AMSR-E Snow and Ice: Time Series</title>
<?php
// style links and the icon link
// pulls in .css files for regular and print display
require('includes/styleLinks.inc');
?>

<?php

$sat1='n18';  
$prod1='sic'; 
$layer1='nt2dy';
$cend1='as'; 
$region1='glb';  
$parameter1='std';  

if(isset($_POST['sat1']))       { $sat1	      = $_POST['sat1'];       }
if(isset($_POST['prod1']))      { $prod1      = $_POST['prod1'];      }
if(isset($_POST['layer1']))     { $layer1     = $_POST['layer1'];     }
if(isset($_POST['cend1']))      { $cend1      = $_POST['cend1'];      }
if(isset($_POST['region1']))    { $region1    = $_POST['region1'];    }
if(isset($_POST['parameter1'])) { $parameter1 = $_POST['parameter1']; }

echo "<script>\n";
echo " var sat1       = '$sat1';       \n";
echo " var prod1      = '$prod1';      \n";
echo " var layer1     = '$layer1';     \n";
echo " var cend1      = '$cend1';      \n";
echo " var region1    = '$region1';    \n";
echo " var parameter1 = '$parameter1'; \n";
echo "</script>\n";



$sat2='n18';  
$prod2='sic'; 
$layer2='nt2dy';
$cend2='as'; 
$region2='glb';  
$parameter2='hss';  

if(isset($_POST['sat2']))       { $sat2	      = $_POST['sat2'];       }
if(isset($_POST['prod2']))      { $prod2      = $_POST['prod2'];      }
if(isset($_POST['layer2']))     { $layer2     = $_POST['layer2'];     }
if(isset($_POST['cend2']))      { $cend2      = $_POST['cend2'];      }
if(isset($_POST['region2']))    { $region2    = $_POST['region2'];    }
if(isset($_POST['parameter2'])) { $parameter2 = $_POST['parameter2']; }

echo "<script>\n";
echo " var sat2       = '$sat2';       \n";
echo " var prod2      = '$prod2';      \n";
echo " var layer2     = '$layer2';     \n";
echo " var cend2      = '$cend2';      \n";
echo " var region2    = '$region2';    \n";
echo " var parameter2 = '$parameter2'; \n";
echo "</script>\n";



$sat3='n18';  
$prod3='sic'; 
$layer3='nt2dy';
$cend3='as'; 
$region3='glb';  
$parameter3='bia';  

if(isset($_POST['sat3']))       { $sat3	      = $_POST['sat3'];       }
if(isset($_POST['prod3']))      { $prod3      = $_POST['prod3'];      }
if(isset($_POST['layer3']))     { $layer3     = $_POST['layer3'];     }
if(isset($_POST['cend3']))      { $cend3      = $_POST['cend3'];      }
if(isset($_POST['region3']))    { $region3    = $_POST['region3'];    }
if(isset($_POST['parameter3'])) { $parameter3 = $_POST['parameter3']; }

echo "<script>\n";
echo " var sat3       = '$sat3';       \n";
echo " var prod3      = '$prod3';      \n";
echo " var layer3     = '$layer3';     \n";
echo " var cend3      = '$cend3';      \n";
echo " var region3    = '$region3';    \n";
echo " var parameter3 = '$parameter3'; \n";
echo "</script>\n";



$sat4='n18';  
$prod4='sic'; 
$layer4='nt2dy';
$cend4='as'; 
$region4='glb';  
$parameter4='far';  

if(isset($_POST['sat4']))       { $sat4	      = $_POST['sat4'];       }
if(isset($_POST['prod4']))      { $prod4      = $_POST['prod4'];      }
if(isset($_POST['layer4']))     { $layer4     = $_POST['layer4'];     }
if(isset($_POST['cend4']))      { $cend4      = $_POST['cend4'];      }
if(isset($_POST['region4']))    { $region4    = $_POST['region4'];    }
if(isset($_POST['parameter4'])) { $parameter4 = $_POST['parameter4']; }

echo "<script>\n";
echo " var sat4       = '$sat4';       \n";
echo " var prod4      = '$prod4';      \n";
echo " var layer4     = '$layer4';     \n";
echo " var cend4      = '$cend4';      \n";
echo " var region4    = '$region4';    \n";
echo " var parameter4 = '$parameter4'; \n";
echo "</script>\n";



$sat5='n18';  
$prod5='sic'; 
$layer5='nt2dy';
$cend5='as'; 
$region5='glb';  
$parameter5='cor';  

if(isset($_POST['sat5']))       { $sat5	      = $_POST['sat5'];       }
if(isset($_POST['prod5']))      { $prod5      = $_POST['prod5'];      }
if(isset($_POST['layer5']))     { $layer5     = $_POST['layer5'];     }
if(isset($_POST['cend5']))      { $cend5      = $_POST['cend5'];      }
if(isset($_POST['region5']))    { $region5    = $_POST['region5'];    }
if(isset($_POST['parameter5'])) { $parameter5 = $_POST['parameter5']; }

echo "<script>\n";
echo " var sat5       = '$sat5';       \n";
echo " var prod5      = '$prod5';      \n";
echo " var layer5     = '$layer5';     \n";
echo " var cend5      = '$cend5';      \n";
echo " var region5    = '$region5';    \n";
echo " var parameter5 = '$parameter5'; \n";
echo "</script>\n";



$sat6='n18';  
$prod6='sic'; 
$layer6='nt2dy';
$cend6='as'; 
$region6='glb';  
$parameter6='pod';  

if(isset($_POST['sat6']))       { $sat6	      = $_POST['sat6'];       }
if(isset($_POST['prod6']))      { $prod6      = $_POST['prod6'];      }
if(isset($_POST['layer6']))     { $layer6     = $_POST['layer6'];     }
if(isset($_POST['cend6']))      { $cend6      = $_POST['cend6'];      }
if(isset($_POST['region6']))    { $region6    = $_POST['region6'];    }
if(isset($_POST['parameter6'])) { $parameter6 = $_POST['parameter6']; }

echo "<script>\n";
echo " var sat6       = '$sat6';       \n";
echo " var prod6      = '$prod6';      \n";
echo " var layer6     = '$layer6';     \n";
echo " var cend6      = '$cend6';      \n";
echo " var region6    = '$region6';    \n";
echo " var parameter6 = '$parameter6'; \n";
echo "</script>\n";

?>


<style type="text/css">

  select.optionvisible   {visibility:visible; font-size: 85%}
  select.optioninvisible {visibility:hidden;  font-size: 85%}


  select.productSelect {font-size: 85%}
  td.productTd {font-size: 85%}

  input.productInput {font-size: 85%; background-color: #eeeeee}

  
</style>

<script language="javascript" type="text/javascript" src="geoamsretimeseries.js"></script>

</head>


<body onLoad="loadInitialImages(sat1,parameter1,cend1,region1,prod1,layer1,
			   	sat2,parameter2,cend2,region2,prod2,layer2,
			   	sat3,parameter3,cend3,region3,prod3,layer3,
			   	sat4,parameter4,cend4,region4,prod4,layer4,
			   	sat5,parameter5,cend5,region5,prod5,layer5,
			   	sat6,parameter6,cend6,region6,prod6,layer6)">
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
				
				
				<FORM name=form action="geoamsretimeseriesv.php" method="post">

				<table border="0" cellpadding="0" cellspacing="0" class="tableGrid" bgcolor="#eeeeee">

				<TR align=center>
				<TD align=center nowrap class="productTd" colspan=2>
				 <input class="productInput" type=submit  value="6 X 1 Panel" style="background-color: lightblue" >&nbsp;&nbsp;
				 <B><font size=4>MIRS Comparison to AMSR-E Snow and Ice: Time Series</font></B>&nbsp;(click image for large view)
				</TD>
				</TR>


				<TR>
				
				<TD class="productTd" id="panel1" align=center valign=top width=325 height=250 align=center>
				
				<select class="productSelect" id="sat1" name="sat1"
				onChange="loadImage(document.form.sat1,document.form.parameter1,document.form.cend1,document.form.region1,document.form.prod1,document.form.layer1,document.form.img1,'href1');">
				<option value="n18"    >NOAA-18</option>
				<option value="n19"    >NOAA-19</option>
				<option value="metopA" >METOP-A</option>
				<option value="f16"    >F16/SSMIS</option>
				<option value="f18"    >F18/SSMIS</option>
				<option value="allSensors"    >All Sensors</option>
				</select>
				
				&nbsp;
				<select class="productSelect" id="parameter1" name="parameter1"
				onChange="loadImage(document.form.sat1,document.form.parameter1,document.form.cend1,document.form.region1,document.form.prod1,document.form.layer1,document.form.img1,'href1');">
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
				
				<select class="productSelect" name="cend1"
				onChange="loadImage(document.form.sat1,document.form.parameter1,document.form.cend1,document.form.region1,document.form.prod1,document.form.layer1,document.form.img1,'href1');">
				<option value="as" >Asc</option>
				<option value="ds" >Des</option>
				</select>			  
				
				&nbsp;
				<select class="productSelect" name="region1"
				onChange="loadImage(document.form.sat1,document.form.parameter1,document.form.cend1,document.form.region1,document.form.prod1,document.form.layer1,document.form.img1,'href1');">
				<option value="glb" >Global</option>
				<option value="nh" >N. Hem.</option>
				<option value="sh" >S. Hem.</option>
				</select>

				&nbsp;
				<select class="productSelect" name="prod1" 
				onChange="changeProduct(this, 'layer1');loadImage(document.form.sat1,document.form.parameter1,document.form.cend1,document.form.region1,document.form.prod1,document.form.layer1,document.form.img1,'href1');">
 				<option value="sic"   >Sea Ice </option>   
				<option value="swe"   >SWE </option>  
				</select>
				
				&nbsp;
				<select class="productSelect" id="layer1" name="layer1" class="optioninvisible"
				onChange="loadImage(document.form.sat1,document.form.parameter1,document.form.cend1,document.form.region1,document.form.prod1,document.form.layer1,document.form.img1,'href1');">
				<option value="nt2dy"   >NASA Team 2</option>   
				<option value="btpdy"   >Bootstrap</option>  
				</select>

				<br>
				
				<a id="href1" href="" target="_blank" >
				<img name="img1" src=""  alt=""  width=325 height=250 ></a>
				
				</TD>

				<TD class="productTd" id="panel2" align=center valign=top width=325 height=250 align=center>
				
				<select class="productSelect" id="sat2" name="sat2"
				onChange="loadImage(document.form.sat2,document.form.parameter2,document.form.cend2,document.form.region2,document.form.prod2,document.form.layer2,document.form.img2,'href2');">
				<option value="n18"    >NOAA-18</option>
				<option value="n19"    >NOAA-19</option>
				<option value="metopA" >METOP-A</option>
				<option value="f16"    >F16/SSMIS</option>
				<option value="f18"    >F18/SSMIS</option>
				<option value="allSensors"    >All Sensors</option>
				</select>
				
				&nbsp;
				<select class="productSelect" id="parameter2" name="parameter2"
				onChange="loadImage(document.form.sat2,document.form.parameter2,document.form.cend2,document.form.region2,document.form.prod2,document.form.layer2,document.form.img2,'href2');">
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
				
				<select class="productSelect" name="cend2"
				onChange="loadImage(document.form.sat2,document.form.parameter2,document.form.cend2,document.form.region2,document.form.prod2,document.form.layer2,document.form.img2,'href2');">
				<option value="as" >Asc</option>
				<option value="ds" >Des</option>
				</select>			  
				
				&nbsp;
				<select class="productSelect" name="region2"
				onChange="loadImage(document.form.sat2,document.form.parameter2,document.form.cend2,document.form.region2,document.form.prod2,document.form.layer2,document.form.img2,'href2');">
				<option value="glb" >Global</option>
				<option value="nh" >N. Hem.</option>
				<option value="sh" >S. Hem.</option>
				</select>

				&nbsp;
				<select class="productSelect" name="prod2" 
				onChange="changeProduct(this, 'layer2');loadImage(document.form.sat2,document.form.parameter2,document.form.cend2,document.form.region2,document.form.prod2,document.form.layer2,document.form.img2,'href2');">
 				<option value="sic"   >Sea Ice </option>   
				<option value="swe"   >SWE </option>  
				</select>
				
				&nbsp;
				<select class="productSelect" id="layer2" name="layer2" class="optioninvisible"
				onChange="loadImage(document.form.sat2,document.form.parameter2,document.form.cend2,document.form.region2,document.form.prod2,document.form.layer2,document.form.img2,'href2');">
				<option value="nt2dy"   >NASA Team 2</option>   
				<option value="btpdy"   >Bootstrap</option>  
				</select>

				<br>
				
				<a id="href2" href="" target="_blank" >
				<img name="img2" src=""  alt=""  width=325 height=250 ></a>
				
				</TD>
				
				</TR>



				<TR>
				
				<TD class="productTd" id="panel3" align=center valign=top width=325 height=250 align=center>
				
				<select class="productSelect" id="sat3" name="sat3"
				onChange="loadImage(document.form.sat3,document.form.parameter3,document.form.cend3,document.form.region3,document.form.prod3,document.form.layer3,document.form.img3,'href3');">
				<option value="n18"    >NOAA-18</option>
				<option value="n19"    >NOAA-19</option>
				<option value="metopA" >METOP-A</option>
				<option value="f16"    >F16/SSMIS</option>
				<option value="f18"    >F18/SSMIS</option>
				<option value="allSensors"    >All Sensors</option>
				</select>
				
				&nbsp;
				<select class="productSelect" id="parameter3" name="parameter3"
				onChange="loadImage(document.form.sat3,document.form.parameter3,document.form.cend3,document.form.region3,document.form.prod3,document.form.layer3,document.form.img3,'href3');">
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
				
				<select class="productSelect" name="cend3"
				onChange="loadImage(document.form.sat3,document.form.parameter3,document.form.cend3,document.form.region3,document.form.prod3,document.form.layer3,document.form.img3,'href3');">
				<option value="as" >Asc</option>
				<option value="ds" >Des</option>
				</select>			  
				
				&nbsp;
				<select class="productSelect" name="region3"
				onChange="loadImage(document.form.sat3,document.form.parameter3,document.form.cend3,document.form.region3,document.form.prod3,document.form.layer3,document.form.img3,'href3');">
				<option value="glb" >Global</option>
				<option value="nh" >N. Hem.</option>
				<option value="sh" >S. Hem.</option>
				</select>

				&nbsp;
				<select class="productSelect" name="prod3" 
				onChange="changeProduct(this, 'layer3');loadImage(document.form.sat3,document.form.parameter3,document.form.cend3,document.form.region3,document.form.prod3,document.form.layer3,document.form.img3,'href3');">
 				<option value="sic"   >Sea Ice </option>   
				<option value="swe"   >SWE </option>  
				</select>
				
				&nbsp;
				<select class="productSelect" id="layer3" name="layer3" class="optioninvisible"
				onChange="loadImage(document.form.sat3,document.form.parameter3,document.form.cend3,document.form.region3,document.form.prod3,document.form.layer3,document.form.img3,'href3');">
				<option value="nt2dy"   >NASA Team 2</option>   
				<option value="btpdy"   >Bootstrap</option>  
				</select>

				<br>
				
				<a id="href3" href="" target="_blank" >
				<img name="img3" src=""  alt=""  width=325 height=250 ></a>
				
				</TD>

				<TD class="productTd" id="panel4" align=center valign=top width=325 height=250 align=center>
				
				<select class="productSelect" id="sat4" name="sat4"
				onChange="loadImage(document.form.sat4,document.form.parameter4,document.form.cend4,document.form.region4,document.form.prod4,document.form.layer4,document.form.img4,'href4');">
				<option value="n18"    >NOAA-18</option>
				<option value="n19"    >NOAA-19</option>
				<option value="metopA" >METOP-A</option>
				<option value="f16"    >F16/SSMIS</option>
				<option value="f18"    >F18/SSMIS</option>
				<option value="allSensors"    >All Sensors</option>
				</select>
				
				&nbsp;
				<select class="productSelect" id="parameter4" name="parameter4"
				onChange="loadImage(document.form.sat4,document.form.parameter4,document.form.cend4,document.form.region4,document.form.prod4,document.form.layer4,document.form.img4,'href4');">
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
				
				<select class="productSelect" name="cend4"
				onChange="loadImage(document.form.sat4,document.form.parameter4,document.form.cend4,document.form.region4,document.form.prod4,document.form.layer4,document.form.img4,'href4');">
				<option value="as" >Asc</option>
				<option value="ds" >Des</option>
				</select>			  
				
				&nbsp;
				<select class="productSelect" name="region4"
				onChange="loadImage(document.form.sat4,document.form.parameter4,document.form.cend4,document.form.region4,document.form.prod4,document.form.layer4,document.form.img4,'href4');">
				<option value="glb" >Global</option>
				<option value="nh" >N. Hem.</option>
				<option value="sh" >S. Hem.</option>
				</select>

				&nbsp;
				<select class="productSelect" name="prod4" 
				onChange="changeProduct(this, 'layer4');loadImage(document.form.sat4,document.form.parameter4,document.form.cend4,document.form.region4,document.form.prod4,document.form.layer4,document.form.img4,'href4');">
 				<option value="sic"   >Sea Ice </option>   
				<option value="swe"   >SWE </option>  
				</select>
				
				&nbsp;
				<select class="productSelect" id="layer4" name="layer4" class="optioninvisible"
				onChange="loadImage(document.form.sat4,document.form.parameter4,document.form.cend4,document.form.region4,document.form.prod4,document.form.layer4,document.form.img4,'href4');">
				<option value="nt2dy"   >NASA Team 2</option>   
				<option value="btpdy"   >Bootstrap</option>  
				</select>

				<br>
				
				<a id="href4" href="" target="_blank" >
				<img name="img4" src=""  alt=""  width=325 height=250 ></a>
				
				</TD>
				
				</TR>


				<TR>
				
				<TD class="productTd" id="panel5" align=center valign=top width=325 height=250 align=center>
				
				<select class="productSelect" id="sat5" name="sat5"
				onChange="loadImage(document.form.sat5,document.form.parameter5,document.form.cend5,document.form.region5,document.form.prod5,document.form.layer5,document.form.img5,'href5');">
				<option value="n18"    >NOAA-18</option>
				<option value="n19"    >NOAA-19</option>
				<option value="metopA" >METOP-A</option>
				<option value="f16"    >F16/SSMIS</option>
				<option value="f18"    >F18/SSMIS</option>
				<option value="allSensors"    >All Sensors</option>
				</select>
				
				&nbsp;
				<select class="productSelect" id="parameter5" name="parameter5"
				onChange="loadImage(document.form.sat5,document.form.parameter5,document.form.cend5,document.form.region5,document.form.prod5,document.form.layer5,document.form.img5,'href5');">
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
				
				<select class="productSelect" name="cend5"
				onChange="loadImage(document.form.sat5,document.form.parameter5,document.form.cend5,document.form.region5,document.form.prod5,document.form.layer5,document.form.img5,'href5');">
				<option value="as" >Asc</option>
				<option value="ds" >Des</option>
				</select>			  
				
				&nbsp;
				<select class="productSelect" name="region5"
				onChange="loadImage(document.form.sat5,document.form.parameter5,document.form.cend5,document.form.region5,document.form.prod5,document.form.layer5,document.form.img5,'href5');">
				<option value="glb" >Global</option>
				<option value="nh" >N. Hem.</option>
				<option value="sh" >S. Hem.</option>
				</select>

				&nbsp;
				<select class="productSelect" name="prod5" 
				onChange="changeProduct(this, 'layer5');loadImage(document.form.sat5,document.form.parameter5,document.form.cend5,document.form.region5,document.form.prod5,document.form.layer5,document.form.img5,'href5');">
 				<option value="sic"   >Sea Ice </option>   
				<option value="swe"   >SWE </option>  
				</select>
				
				&nbsp;
				<select class="productSelect" id="layer5" name="layer5" class="optioninvisible"
				onChange="loadImage(document.form.sat5,document.form.parameter5,document.form.cend5,document.form.region5,document.form.prod5,document.form.layer5,document.form.img5,'href5');">
				<option value="nt2dy"   >NASA Team 2</option>   
				<option value="btpdy"   >Bootstrap</option>  
				</select>

				<br>
				
				<a id="href5" href="" target="_blank" >
				<img name="img5" src=""  alt=""  width=325 height=250 ></a>
				
				</TD>

				<TD class="productTd" id="panel6" align=center valign=top width=325 height=250 align=center>
				
				<select class="productSelect" id="sat6" name="sat6"
				onChange="loadImage(document.form.sat6,document.form.parameter6,document.form.cend6,document.form.region6,document.form.prod6,document.form.layer6,document.form.img6,'href6');">
				<option value="n18"    >NOAA-18</option>
				<option value="n19"    >NOAA-19</option>
				<option value="metopA" >METOP-A</option>
				<option value="f16"    >F16/SSMIS</option>
				<option value="f18"    >F18/SSMIS</option>
				<option value="allSensors"    >All Sensors</option>
				</select>
				
				&nbsp;
				<select class="productSelect" id="parameter6" name="parameter6"
				onChange="loadImage(document.form.sat6,document.form.parameter6,document.form.cend6,document.form.region6,document.form.prod6,document.form.layer6,document.form.img6,'href6');">
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
				
				<select class="productSelect" name="cend6"
				onChange="loadImage(document.form.sat6,document.form.parameter6,document.form.cend6,document.form.region6,document.form.prod6,document.form.layer6,document.form.img6,'href6');">
				<option value="as" >Asc</option>
				<option value="ds" >Des</option>
				</select>			  
				
				&nbsp;
				<select class="productSelect" name="region6"
				onChange="loadImage(document.form.sat6,document.form.parameter6,document.form.cend6,document.form.region6,document.form.prod6,document.form.layer6,document.form.img6,'href6');">
				<option value="glb" >Global</option>
				<option value="nh" >N. Hem.</option>
				<option value="sh" >S. Hem.</option>
				</select>

				&nbsp;
				<select class="productSelect" name="prod6" 
				onChange="changeProduct(this, 'layer6');loadImage(document.form.sat6,document.form.parameter6,document.form.cend6,document.form.region6,document.form.prod6,document.form.layer6,document.form.img6,'href6');">
 				<option value="sic"   >Sea Ice </option>   
				<option value="swe"   >SWE </option>  
				</select>
				
				&nbsp;
				<select class="productSelect" id="layer6" name="layer6" class="optioninvisible"
				onChange="loadImage(document.form.sat6,document.form.parameter6,document.form.cend6,document.form.region6,document.form.prod6,document.form.layer6,document.form.img6,'href6');">
				<option value="nt2dy"   >NASA Team 2</option>   
				<option value="btpdy"   >Bootstrap</option>  
				</select>

				<br>
				
				<a id="href6" href="" target="_blank" >
				<img name="img6" src=""  alt=""  width=325 height=250 ></a>
				
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
