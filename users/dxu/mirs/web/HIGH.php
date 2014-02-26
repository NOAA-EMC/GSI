<html>
<head>
<title>STAR - MIRS Project - High Resolution Product</title>

<?php

$sat1 = '';  
$region1 = ''; 
$cend1 = ''; 
$sfc1 = '';  
$prod1 = ''; 
$layer1 = '';
$yr1 = '';   
$mo1 = '';   
$dy1 = '';   

if(isset($_POST['sat1']))    { $sat1    = $_POST['sat1'];    }
if(isset($_POST['region1'])) { $region1 = $_POST['region1']; }
if(isset($_POST['cend1']))   { $cend1   = $_POST['cend1'];   }
if(isset($_POST['sfc1']))    { $sfc1    = $_POST['sfc1'];    }
if(isset($_POST['prod1']))   { $prod1   = $_POST['prod1'];   }
if(isset($_POST['layer1']))  { $layer1  = $_POST['layer1'];  }
if(isset($_POST['yr1']))     { $yr1     = $_POST['yr1'];     }
if(isset($_POST['mo1']))     { $mo1     = $_POST['mo1'];     }
if(isset($_POST['dy1']))     { $dy1     = $_POST['dy1'];     }

echo "<script> var sat1    = '$sat1';    </script>";
echo "<script> var region1 = '$region1'; </script>";
echo "<script> var cend1   = '$cend1';   </script>";
echo "<script> var sfc1    = '$sfc1';    </script>";
echo "<script> var prod1   = '$prod1';   </script>";
echo "<script> var layer1  = '$layer1';  </script>";
echo "<script> var yr1     = '$yr1';     </script>";
echo "<script> var mo1     = '$mo1';     </script>";
echo "<script> var dy1     = '$dy1';     </script>";

$sat2 = '';  
$region2 = ''; 
$cend2 = ''; 
$sfc2 = '';  
$prod2 = ''; 
$layer2 = '';
$yr2 = '';   
$mo2 = '';   
$dy2 = '';   

if(isset($_POST['sat2']))    { $sat2    = $_POST['sat2'];    }
if(isset($_POST['region2'])) { $region2 = $_POST['region2']; }
if(isset($_POST['cend2']))   { $cend2   = $_POST['cend2'];   }
if(isset($_POST['sfc2']))    { $sfc2    = $_POST['sfc2'];    }
if(isset($_POST['prod2']))   { $prod2   = $_POST['prod2'];   }
if(isset($_POST['layer2']))  { $layer2  = $_POST['layer2'];  }
if(isset($_POST['yr2']))     { $yr2     = $_POST['yr2'];     }
if(isset($_POST['mo2']))     { $mo2     = $_POST['mo2'];     }
if(isset($_POST['dy2']))     { $dy2     = $_POST['dy2'];     }

echo "<script> var sat2    = '$sat2';    </script>";
echo "<script> var region2 = '$region2'; </script>";
echo "<script> var cend2   = '$cend2';   </script>";
echo "<script> var sfc2    = '$sfc2';    </script>";
echo "<script> var prod2   = '$prod2';   </script>";
echo "<script> var layer2  = '$layer2';  </script>";
echo "<script> var yr2     = '$yr2';     </script>";
echo "<script> var mo2     = '$mo2';     </script>";
echo "<script> var dy2     = '$dy2';     </script>";


$sat3 = '';  
$region3 = ''; 
$cend3 = ''; 
$sfc3 = '';  
$prod3 = ''; 
$layer3 = '';
$yr3 = '';   
$mo3 = '';   
$dy3 = '';   

if(isset($_POST['sat3']))    { $sat3    = $_POST['sat3'];    }
if(isset($_POST['region3'])) { $region3 = $_POST['region3']; }
if(isset($_POST['cend3']))   { $cend3   = $_POST['cend3'];   }
if(isset($_POST['sfc3']))    { $sfc3    = $_POST['sfc3'];    }
if(isset($_POST['prod3']))   { $prod3   = $_POST['prod3'];   }
if(isset($_POST['layer3']))  { $layer3  = $_POST['layer3'];  }
if(isset($_POST['yr3']))     { $yr3     = $_POST['yr3'];     }
if(isset($_POST['mo3']))     { $mo3     = $_POST['mo3'];     }
if(isset($_POST['dy3']))     { $dy3     = $_POST['dy3'];     }

echo "<script> var sat3    = '$sat3';    </script>";
echo "<script> var region3 = '$region3'; </script>";
echo "<script> var cend3   = '$cend3';   </script>";
echo "<script> var sfc3    = '$sfc3';    </script>";
echo "<script> var prod3   = '$prod3';   </script>";
echo "<script> var layer3  = '$layer3';  </script>";
echo "<script> var yr3     = '$yr3';     </script>";
echo "<script> var mo3     = '$mo3';     </script>";
echo "<script> var dy3     = '$dy3';     </script>";


$sat4 = '';  
$region4 = ''; 
$cend4 = ''; 
$sfc4 = '';  
$prod4 = ''; 
$layer4 = '';
$yr4 = '';   
$mo4 = '';   
$dy4 = '';   

if(isset($_POST['sat4']))    { $sat4    = $_POST['sat4'];    }
if(isset($_POST['region4'])) { $region4 = $_POST['region4']; }
if(isset($_POST['cend4']))   { $cend4   = $_POST['cend4'];   }
if(isset($_POST['sfc4']))    { $sfc4    = $_POST['sfc4'];    }
if(isset($_POST['prod4']))   { $prod4   = $_POST['prod4'];   }
if(isset($_POST['layer4']))  { $layer4  = $_POST['layer4'];  }
if(isset($_POST['yr4']))     { $yr4     = $_POST['yr4'];     }
if(isset($_POST['mo4']))     { $mo4     = $_POST['mo4'];     }
if(isset($_POST['dy4']))     { $dy4     = $_POST['dy4'];     }

echo "<script> var sat4    = '$sat4';    </script>";
echo "<script> var region4 = '$region4'; </script>";
echo "<script> var cend4   = '$cend4';   </script>";
echo "<script> var sfc4    = '$sfc4';    </script>";
echo "<script> var prod4   = '$prod4';   </script>";
echo "<script> var layer4  = '$layer4';  </script>";
echo "<script> var yr4     = '$yr4';     </script>";
echo "<script> var mo4     = '$mo4';     </script>";
echo "<script> var dy4     = '$dy4';     </script>";

?>


<style type="text/css">

select.optionvisible {
  	font-size: 90%; 
  	visibility: visible;
}

select.optioninvisible {
	font-size: 90%; 
	visibility: hidden;
}
  
select.productSelect {
	font-size: 85%; 
}

td.productTd {
  font-size: 85%;
  text-align: center;
  vertical-align: middle;
}

caption.productCaption {
	font-size: 85%;
}

input.productInput {
	font-size: 0.9em; 
	background-color: #eeeeee;
}
 
td.imageContainer {
	width: 325px;
	height: 325px;
  font-size: 0.9em;
  text-align: center;
} 

span.productTitle {
	font-weight: bold;
	font-size: 1.6em;	
} 
 
fieldset {
	padding: 0px;
	margin: 0px;
	border: 0px;
}
  
</style>

<script language="javascript" type="text/javascript" src="HIGH.js"></script>

</head>



<body onLoad="loadInit(sat1,region1,cend1,sfc1,prod1,layer1,yr1,mo1,dy1, sat2,region2,cend2,sfc2,prod2,layer2,yr2,mo2,dy2, sat3,region3,cend3,sfc3,prod3,layer3,yr3,mo3,dy3, sat4,region4,cend4,sfc4,prod4,layer4,yr4,mo4,dy4 );">
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

				<form name="form" action="" method="post">

				<table border="0" cellpadding="0" cellspacing="0" class="tableGrid" bgcolor="#eeeeee">
				<tr>
				<td class="productTd" colspan="2">
				  <input class="productInput" type="submit" value="1 Panel"      onClick="setCount(1);" style="background-color: lightblue" >
				  <input class="productInput" type="submit" value="4 X 1 Panel"  onClick="setCount(4);" style="background-color: lightblue" > 
				  <span class="productTitle">MIRS High Resolution Products</span> (<em>Click image for large view</em>)
				  <input class="productInput" type="button" style="background-color: lightblue" 
				  	onclick="launchAnimation();" value="Start Animation">
				  <input class="productInput" type="button" style="background-color: lightblue;" 
				  	onclick="stopAnimation();"  value="Stop Animation"><br />
				</td>
				</tr>
				<tr>
					<td class="imageContainer" id="panel1">
						<fieldset title="this fieldset groups form controls for the upper left image">
				<select 
					class="productSelect" 
					id="sat1" name="sat1" 
					title="choose a satellite sensor"
					onChange="populateRegions(this.value,document.form.region1);changeSatellite_template( this, document.form.prod1, document.form.layer1, 'layer1' );loadImage1();">
					<option value="n18"	>NOAA-18</option>
					<option value="metopB"	>METOP-B</option>
					<option value="npp"	>NPP/ATMS</option>
					<option value="metopA"	>METOP-A</option>
					<option value="f16"	>F16/SSMIS</option>
					<option value="f18"	>F18/SSMIS</option>
					<option value="aqua"	>AMSR-E</option>
					<option value="trmm"	>TRMM/TMI</option>
					<option value="gpm"	>GPM/GMI</option>
					<!--option value="n15"	>N15</option-->
					<option value="n16"	>NOAA-16</option>
					<!--option value="n17"	>N17</option-->
					<option value="n19"	>NOAA-19</option>
				</select>&nbsp;
				
				<select 
					class="productSelect" 
					id="region1" name="region1" 
					title="choose a region"
					onChange="loadImage1();">
					<option value="glb"	  >Globe</option>
					<option value="us"	  >USA</option>
					<option value="eu"	  >Europe</option>
					<option value="gulf"	  >Gulf</option>
					<option value="china"     >China</option>
				</select>&nbsp;
				
				<select class="productSelect" id="prod1" name="prod1" 
					title="choose a MIRS product"
					onChange="changeProduct_template( document.form.sat1, this, document.form.layer1, 'layer1' );loadImage1();">
<?php
// product options
require('includes/productOptions_highresolution.inc');
?>
				</select>

				<select id="layer1" name="layer1" class="optioninvisible" onChange="loadImage1();" title="select algorithm/channel/layer">
				</select>
				
				<br />
				<select class="productSelect" id="cend1" name="cend1" 
					title="choose a passing mode (ascending/descending)"
					onChange="loadImage1();">
					<option value="as">Asc</option>
					<option value="ds">Des</option>
				</select>&nbsp;

				<select class="productSelect" id="sfc1" name="sfc1" 
					title="choose terrain"
					onChange="loadImage1();">
					<option value="all">All</option>
					<option value="sea">Sea</option>
					<option value="lnd">Land</option>
				</select>&nbsp;
				
				<select class="productSelect" id="yr1" name="yr1" 
					title="select year"
					onChange="loadImage1();">
<?php
// year options
require('includes/yearOptions.inc');
?>
				</select>			  

				<select class="productSelect" id="mo1" name="mo1" 
					title="select month"
					onChange="loadImage1();">
<?php
// month options
require('includes/monthOptions.inc');
?>
				</select>			  

				<select class="productSelect" id="dy1" name="dy1" 
					title="select day"
					onChange="loadImage1();">	  
<?php
// day options
require('includes/dayOptions.inc');
?>
				</select>
							  
				&nbsp;
				<input class="productInput" type="button" onclick="rev(document.form.yr1,document.form.mo1,document.form.dy1,1);" 
				title="step backward through images" value="<=">  
				<input class="productInput" type="button" onclick="fwd(document.form.yr1,document.form.mo1,document.form.dy1,1);"
				title="step forward through images" value="=>">
				<br />
				<a id="href1" href="" target="_blank"><img 
					name="img1" src="" 
					alt="" 
					width="325" height="250"></a></fieldset></td>

				<td class="imageContainer" id="panel2">
					<fieldset title="this fieldset groups form controls for the upper right image">
				
				<select class="productSelect" id="sat2" name="sat2" 
					title="choose a satellite sensor"
					onChange="populateRegions(this.value,document.form.region2);changeSatellite_template( this, document.form.prod2, document.form.layer2, 'layer2' );loadImage2();">
					<option value="n18"	>NOAA-18</option>
					<option value="metopB"	>METOP-B</option>
					<option value="npp"	>NPP/ATMS</option>
					<option value="metopA"	>METOP-A</option>
					<option value="f16"	>F16/SSMIS</option>
					<option value="f18"	>F18/SSMIS</option>
					<option value="aqua"	>AMSR-E</option>
					<option value="trmm"	>TRMM/TMI</option>
					<option value="gpm"	>GPM/GMI</option>
					<!--option value="n15"	>N15</option-->
					<option value="n16"	>NOAA-16</option>
					<!--option value="n17"	>N17</option-->
					<option value="n19"	>NOAA-19</option>
				</select>&nbsp;

				<select 
					class="productSelect" 
					id="region2" name="region2" 
					title="choose a region"
					onChange="loadImage2();">
					<option value="glb"	  >Globe</option>
					<option value="us"	  >USA</option>
					<option value="eu"	  >Europe</option>
					<option value="gulf"	  >Gulf</option>
					<option value="china"     >China</option>
				</select>&nbsp;

				<select class="productSelect" id="prod2" name="prod2" 
					title="choose a MIRS product"
					onChange="changeProduct_template( document.form.sat2, this, document.form.layer2, 'layer2' );loadImage2();">
<?php
// product options
require('includes/productOptions_highresolution.inc');
?>
				</select>

				<select id="layer2"  name="layer2" class="optioninvisible" onChange="loadImage2();">
				</select><br />
				
				<select class="productSelect" id="cend2" name="cend2" 
					title="choose a passing mode (ascending/descending)"
					onChange="loadImage2();">
					<option value="as">Asc</option>
					<option value="ds">Des</option>
				</select>&nbsp;

				<select class="productSelect" id="sfc2" name="sfc2" 
					title="choose a surface type for images"
					onChange="loadImage2();">
					<option value="all">All</option>
					<option value="sea">Sea</option>
					<option value="lnd">Land</option>
				</select>&nbsp;
				
				<select class="productSelect" name="yr2"
					title="select year"
					onChange="loadImage2();">
<?php
// year options
require('includes/yearOptions.inc');
?>
				</select>			  

				<select class="productSelect" name="mo2"
					title="select month"
					onChange="loadImage2();">	  
<?php
// month options
require('includes/monthOptions.inc');
?>
				</select>			  

				<select class="productSelect" name="dy2"
					title="select day"
					onChange="loadImage2();">	  
<?php
// day options
require('includes/dayOptions.inc');
?>
				</select>
							  
				&nbsp;
				<input class="productInput" type="button" 
				onclick="rev(document.form.yr2,document.form.mo2,document.form.dy2,2);"
				title="step backward through images"
				value="<=">
				<input class="productInput" type="button" 
				onclick="fwd(document.form.yr2,document.form.mo2,document.form.dy2,2);"
				title="step forward through images"
				value="=>">
				
				<br />
				<a id="href2" href="" target="_blank">
				<img name="img2" src="" 
					alt="" 
					width="325" height="250" /></a></fieldset></td>
				</tr>


				<tr><td class="imageContainer" id="panel3"><fieldset title="this fieldset groups form controls for the lower left image">
				
				<select class="productSelect" id="sat3" name="sat3" 
					title="choose a satellite sensor"
					onChange="populateRegions(this.value,document.form.region3);changeSatellite_template( this, document.form.prod3, document.form.layer3, 'layer3' );loadImage3();">
					<option value="n18"	>NOAA-18</option>
					<option value="metopB"	>METOP-B</option>
					<option value="npp"	>NPP/ATMS</option>
					<option value="metopA"	>METOP-A</option>
					<option value="f16"	>F16/SSMIS</option>
					<option value="f18"	>F18/SSMIS</option>
					<option value="aqua"	>AMSR-E</option>
					<option value="trmm"	>TRMM/TMI</option>
					<option value="gpm"	>GPM/GMI</option>
					<!--option value="n15"	>N15</option-->
					<option value="n16"	>NOAA-16</option>
					<!--option value="n17"	>N17</option-->
					<option value="n19"	>NOAA-19</option>
				</select>&nbsp;	
				
				<select 
					class="productSelect" 
					id="region3" name="region3" 
					title="choose a region"
					onChange="loadImage3();">
					<option value="glb"	  >Globe</option>
					<option value="us"	  >USA</option>
					<option value="eu"	  >Europe</option>
					<option value="gulf"	  >Gulf</option>
					<option value="china"     >China</option>
				</select>&nbsp;
				
				<select class="productSelect" id="prod3" name="prod3" 
					title="choose a MIRS product"
					onChange="changeProduct_template( document.form.sat3, this, document.form.layer3, 'layer3' );loadImage3();">
<?php
// product options
require('includes/productOptions_highresolution.inc');
?>
				</select>
				<select id="layer3"  name="layer3" class="optioninvisible" onChange="loadImage3();">
				</select>
				<br />
				
				<select class="productSelect" id="cend3" name="cend3" 
					title="choose a passing mode (ascending/descending)"
					onChange="loadImage3();">
					<option value="as">Asc</option>
					<option value="ds">Des</option>
				</select>&nbsp;

				<select class="productSelect" id="sfc3" name="sfc3" 
					title="choose a surface type for images"
					onChange="loadImage3();">
					<option value="all">All</option>
					<option value="sea">Sea</option>
					<option value="lnd">Land</option>
				</select>&nbsp;
				
				<select class="productSelect" name="yr3"
					title="select year"
					onChange="loadImage3();">
<?php
// year options
require('includes/yearOptions.inc');
?>
				</select>			  

				<select class="productSelect" name="mo3"
					title="select month"
					onChange="loadImage3();">
<?php
// month options
require('includes/monthOptions.inc');
?>
				</select>			  

				<select class="productSelect" name="dy3"
					title="select day"
					onChange="loadImage3();">	  
<?php
// day options
require('includes/dayOptions.inc');
?>
				</select>			  
				
				&nbsp;
				<input class="productInput" type="button" 
				onclick="rev(document.form.yr3,document.form.mo3,document.form.dy3,3);"
				title="step backward through images" 
				value="<=">
				<input class="productInput" type="button" 
				onclick="fwd(document.form.yr3,document.form.mo3,document.form.dy3,3);"
				title="step forward through images"
				value="=>">
				
				<br />
				<a id="href3" href="" target="_blank">
				<img name="img3" src="" 
		     			alt="" 
		     			width="325" height="250" /></a></fieldset></td>

				<td class="imageContainer" id="panel4"><fieldset title="this fieldset groups form controls for the lower right image">
				
				<select class="productSelect" id="sat4" name="sat4" 
					title="choose a satellite sensor"
					onChange="populateRegions(this.value,document.form.region4);changeSatellite_template( this, document.form.prod4, document.form.layer4, 'layer4');loadImage4();">
					<option value="n18"	>NOAA-18</option>
					<option value="metopB"	>METOP-B</option>
					<option value="npp"	>NPP/ATMS</option>
					<option value="metopA"	>METOP-A</option>
					<option value="f16"	>F16/SSMIS</option>
					<option value="f18"	>F18/SSMIS</option>
					<option value="aqua"	>AMSR-E</option>
					<option value="trmm"	>TRMM/TMI</option>
					<option value="gpm"	>GPM/GMI</option>
					<!--option value="n15"	>N15</option-->
					<option value="n16"	>NOAA-16</option>
					<!--option value="n17"	>N17</option-->
					<option value="n19"	>NOAA-19</option>
				</select>&nbsp;

				<select 
					class="productSelect" 
					id="region4" name="region4" 
					title="choose a region"
					onChange="loadImage4();">
					<option value="glb"	  >Globe</option>
					<option value="us"	  >USA</option>
					<option value="eu"	  >Europe</option>
					<option value="gulf"	  >Gulf</option>
					<option value="china"     >China</option>
				</select>&nbsp;
				
				<select class="productSelect" id="prod4" name="prod4" 
					title="choose a MIRS product"
					onChange="changeProduct_template( document.form.sat4, this, document.form.layer4, 'layer4');loadImage4();">
<?php
// product options
require('includes/productOptions_highresolution.inc');
2?>
				</select>

				<select id="layer4"  name="layer4" class="optioninvisible" onChange="loadImage4();">
				</select>
				<br />
				
				<select class="productSelect" class="productSelect" id="cend4" name="cend4" 
					title="choose a passing mode (ascending/descending)"
					onChange="loadImage4();">
					<option value="as">Asc</option>
					<option value="ds">Des</option>
				</select>&nbsp;

				<select class="productSelect" id="sfc4" name="sfc4" 
					title="choose a terrain"				
					onChange="loadImage4();">
					<option value="all">All</option>
					<option value="sea">Sea</option>
					<option value="lnd">Land</option>
				</select>&nbsp;
				
				<select class="productSelect" name="yr4"
				title="select year"
				onChange="loadImage4();">
<?php
// year options
require('includes/yearOptions.inc');
?>
				</select>			  

				<select class="productSelect" name="mo4"
					title="select month"
					onChange="loadImage4();">	  
<?php
// month options
require('includes/monthOptions.inc');
?>
				</select>			  

				<select class="productSelect" name="dy4"
					title="select day"
					onChange="loadImage4();">	  
<?php
// day options
require('includes/dayOptions.inc');
?>
				</select>			  
				
				&nbsp;
				<input class="productInput" type="button" 
				onclick="rev(document.form.yr4,document.form.mo4,document.form.dy4,4);" 
				title="step backward through images"
				value="<=">
				<input class="productInput" type="button" 
				onclick="fwd(document.form.yr4,document.form.mo4,document.form.dy4,4);" 
				title="step forward through images"
				value="=>">
				
				<br />
				<a id="href4" href="" target="_blank">
				<img name="img4" 
					src="" 
		     			alt="" 
				 	width="325" height="250" /></a></td>
				</tr>
			</table>
			</form>


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
