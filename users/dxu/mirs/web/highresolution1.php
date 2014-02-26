<?php
// insert header that forbids caching and carries doctype
// and html tag;
require('includes/noCacheHeader.inc');
?>
<META name="verify-v1" content="CwbLBcFt9+GqRTgaLZsENmPnSWNB5MStHHdYsB7U2nI=">
<title>STAR - MIRS Project - High Resolution Product</title>
<?php
// style links and the icon link
// pulls in .css files for regular and print display
require('includes/styleLinks.inc');
?>

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

  select.optionvisible   	{font-size: 90%; visibility:visible}
  select.optioninvisible 	{font-size: 90%; visibility:hidden}
  
  select.productSelect {font-size: 90%}
  td.productTd {font-size: 90%}
  caption.productCaption {font-size: 90%}
  input.productInput {font-size: 90%; background-color: #eeeeee}
   
</style>

<script language="javascript" type="text/javascript" src="highresolution.js"></script>

</head>


<body onLoad="loadInitialImages1(sat1,region1,cend1,sfc1,prod1,layer1,yr1,mo1,dy1)">
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
			require('includes/Sample_NavDiv_highresolution1.inc');
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
<?php
// purpose here is to remember 3 other panel variables
echo "					<input type=hidden name=sat2 value=\"$sat2\"/>\n";
echo "					<input type=hidden name=region2 value=\"$region2\"/>\n";
echo "					<input type=hidden name=cend2 value=\"$cend2\"/>\n";
echo "					<input type=hidden name=sfc2 value=\"$sfc2\"/>\n";
echo "					<input type=hidden name=prod2 value=\"$prod2\"/>\n";
echo "					<input type=hidden name=layer2 value=\"$layer2\"/>\n";
echo "					<input type=hidden name=yr2 value=\"$yr2\"/>\n";
echo "					<input type=hidden name=mo2 value=\"$mo2\"/>\n";
echo "					<input type=hidden name=dy2 value=\"$dy2\"/>\n";

echo "					<input type=hidden name=sat3 value=\"$sat3\"/>\n";
echo "					<input type=hidden name=region3 value=\"$region3\"/>\n";
echo "					<input type=hidden name=cend3 value=\"$cend3\"/>\n";
echo "					<input type=hidden name=sfc3 value=\"$sfc3\"/>\n";
echo "					<input type=hidden name=prod3 value=\"$prod3\"/>\n";
echo "					<input type=hidden name=layer3 value=\"$layer3\"/>\n";
echo "					<input type=hidden name=yr3 value=\"$yr3\"/>\n";
echo "					<input type=hidden name=mo3 value=\"$mo3\"/>\n";
echo "					<input type=hidden name=dy3 value=\"$dy3\"/>\n";

echo "					<input type=hidden name=sat4 value=\"$sat4\"/>\n";
echo "					<input type=hidden name=region4 value=\"$region4\"/>\n";
echo "					<input type=hidden name=cend4 value=\"$cend4\"/>\n";
echo "					<input type=hidden name=sfc4 value=\"$sfc4\"/>\n";
echo "					<input type=hidden name=prod4 value=\"$prod4\"/>\n";
echo "					<input type=hidden name=layer4 value=\"$layer4\"/>\n";
echo "					<input type=hidden name=yr4 value=\"$yr4\"/>\n";
echo "					<input type=hidden name=mo4 value=\"$mo4\"/>\n";
echo "					<input type=hidden name=dy4 value=\"$dy4\"/>\n";
?>

				<table border="0" cellpadding="0" cellspacing="0" class="tableGrid" bgcolor="#eeeeee">

				<TR align=left>

				<TD align=center nowrap class="productTd">
				  <input class="productInput" type=submit value="2 X 2 Panel" onClick="setCount(2)" style="background-color: lightblue" > &nbsp;&nbsp;
				  <input class="productInput" type=submit value="4 X 1 Panel" onClick="setCount(4)" style="background-color: lightblue" > &nbsp;&nbsp;
				  
				 <B><font size=4>MIRS High Resolution Products</font></B>&nbsp;&nbsp;&nbsp;&nbsp;
				  <input class="productInput" type="button" style="background-color: lightblue" onclick="launchAnimation1()" value="Start Animation">&nbsp;&nbsp;
				  <input class="productInput" type="button" style="background-color: lightblue" onclick="stopAnimation()"    value="Stop Animation">
				</TD>
				</TR>


				<TR><TD class="productTd" id="panel1" align=center valign=top height=575>Sensor:
				<select class="productSelect" id="sat1" name="sat1" 
					onChange="populateRegions(this.value,document.form.region1);changeSatellite_template(this,document.form.prod1,document.form.layer1,'layer1');loadImage1();" >
				<option value="metopB"  >METOP-B</option>
				<option value="npp"	>NPP/ATMS</option>
				<option value="trmm" title="TRMM has 2 days delay" >TRMM/TMI</option>
				<option value="gcomw1">GCOMW1/AMSR2</option>
				</select>
				
				&nbsp;Region:
				<select 
					class="productSelect" 
					id="region1" name="region1" 
					title="choose a region"
					onChange="loadImage1();">
					<option value="glb"	  >Globe</option>
					<option value="us"	  >USA</option>
				</select>&nbsp;
				
				&nbsp;Product:
				<select class="productSelect" id="prod1" name="prod1" onChange="changeProduct_template(document.form.sat1,this,document.form.layer1,'layer1');loadImage1();">
<?php
require('includes/productOptions_highresolution.inc');
?>
				</select>
				
				<select id="layer1" name="layer1" class="optioninvisible" onChange="loadImage1()">
				</select>

				<br>
				Mode:
				<select class="productSelect" id="cend1" name="cend1" onChange="loadImage1()" title="Select a passing mode">
				<option VALUE="as" >Asc</option>
				<option VALUE="ds" >Des</option>
				</select>
				
				&nbsp;Surface:
				<select class="productSelect" id="sfc1" name="sfc1" onChange="loadImage1()" title="Select a surface type">
				<option VALUE="all" >All</option>
				<option VALUE="sea" >Sea</option>
				<option VALUE="lnd" >Land</option>
				</select>
				
				&nbsp;Year:
				<select class="productSelect" id="yr1" name="yr1" onChange="loadImage1()">
<?php
// year options
require('includes/yearOptions.inc');
?>
				</select>
				
				Month:
				<select class="productSelect" id="mo1" name="mo1" onChange="loadImage1()">
<?php
// month options
require('includes/monthOptions.inc');
?>
				</select>
				
				Day:
				<select class="productSelect" id="dy1" name="dy1" onChange="loadImage1()">
<?php
// day options
require('includes/dayOptions.inc');
?>
				</select>
				
				&nbsp;Browse:
				<input class="productInput" type="button" onclick="rev(document.form.yr1,document.form.mo1,document.form.dy1,1);" value="<=">
				<input class="productInput" type="button" onclick="fwd(document.form.yr1,document.form.mo1,document.form.dy1,1);" value="=>">
				<br><br>

				<a id="href1" href="" target="_blank" >
				<img name="img1" src=""  alt=""  width=650 height=500></a>

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
