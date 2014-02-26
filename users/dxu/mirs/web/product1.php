<?php include("password_protect.php"); ?>

<?php
// insert header that forbids caching and carries doctype
// and html tag;
require('includes/noCacheHeader.inc');
?>

<META name="verify-v1" content="CwbLBcFt9+GqRTgaLZsENmPnSWNB5MStHHdYsB7U2nI=">
<title>STAR - MIRS Project - Product Monitoring</title>
<?php
// style links and the icon link
// pulls in .css files for regular and print display
require('includes/styleLinks.inc');
?>

<?php
echo "<script>\n";

$IMGDIR_ADV="images/";
$IMGDIR_HER="/corp/scsb/mspps/GIFs/";

$SENSORTAG = array( "f16"    => "_dmsp_f16_ssmis_", 
		    "n18"    => "_poes_n18_amsuamhs_",
		    "metopA" => "_poes_metopA_amsuamhs_",
		    "n19"    => "_poes_n19_amsuamhs_",
		    "f18"    => "_dmsp_f18_ssmis_"       
		  );


$yesterday = mktime(0, 0, 0, date("m"), date("d")-1, date("Y"));

$yyyy = date("Y",$yesterday);
$mm   = date("m",$yesterday);
$dd   = date("d",$yesterday);

$sat1='n18';  
$alg1='adv';  
$cend1='as'; 
$sfc1='all';  
$region1='glb';  
$prod1='tpw'; 
$layer1='';
$proj1='';
$yr1=$yyyy;
$mo1=$mm;
$dy1=$dd;

if(isset($_POST['sat1']))   { $sat1    = $_POST['sat1'];   }
if(isset($_POST['alg1']))   { $alg1    = $_POST['alg1'];   }
if(isset($_POST['cend1']))  { $cend1   = $_POST['cend1'];  }
if(isset($_POST['sfc1']))   { $sfc1    = $_POST['sfc1'];   }
if(isset($_POST['region1'])){ $region1 = $_POST['region1'];}
if(isset($_POST['prod1']))  { $prod1   = $_POST['prod1'];  }
if(isset($_POST['layer1'])) { $layer1  = $_POST['layer1']; }
if(isset($_POST['proj1']))  { $proj1   = $_POST['proj1'];  }
if(isset($_POST['yr1']))    { $yr1     = $_POST['yr1'];    }
if(isset($_POST['mo1']))    { $mo1     = $_POST['mo1'];    }
if(isset($_POST['dy1']))    { $dy1     = $_POST['dy1'];    }


echo "var sat1    = '$sat1';   \n";
echo "var alg1    = '$alg1';   \n";
echo "var cend1   = '$cend1';  \n";
echo "var sfc1    = '$sfc1';   \n";
echo "var region1 = '$region1';\n";
echo "var prod1   = '$prod1';  \n";
echo "var layer1  = '$layer1'; \n";
echo "var proj1   = '$proj1';  \n";
echo "var yr1	  = '$yr1';    \n";
echo "var mo1	  = '$mo1';    \n";
echo "var dy1	  = '$dy1';    \n\n";

$img1=$IMGDIR_ADV.$sat1."/".$yyyy."-".$mm."-".$dd."/mirs_".$alg1.$SENSORTAG[$sat1].$region1."_".$yyyy.$mm.$dd."_".$prod1."_".$sfc1."_".$cend1.".png";


$sat2='n18';  
$alg2='adv';  
$cend2='as'; 
$sfc2='all';  
$region2='glb';  
$prod2='tskin'; 
$layer2='';
$proj2='';
$yr2=$yyyy;
$mo2=$mm;
$dy2=$dd;

if(isset($_POST['sat2']))   { $sat2   = $_POST['sat2'];   }
if(isset($_POST['alg2']))   { $alg2   = $_POST['alg2'];   }
if(isset($_POST['cend2']))  { $cend2  = $_POST['cend2'];  }
if(isset($_POST['sfc2']))   { $sfc2   = $_POST['sfc2'];   }
if(isset($_POST['region2'])){ $region2= $_POST['region2'];}
if(isset($_POST['prod2']))  { $prod2  = $_POST['prod2'];  }
if(isset($_POST['layer2'])) { $layer2 = $_POST['layer2']; }
if(isset($_POST['proj2']))  { $proj2  = $_POST['proj2'];  }
if(isset($_POST['yr2']))    { $yr2    = $_POST['yr2'];    }
if(isset($_POST['mo2']))    { $mo2    = $_POST['mo2'];    }
if(isset($_POST['dy2']))    { $dy2    = $_POST['dy2'];    }

echo "var sat2   = '$sat2';   \n";
echo "var alg2   = '$alg2';   \n";
echo "var cend2  = '$cend2';  \n";
echo "var sfc2   = '$sfc2';   \n";
echo "var region2= '$region2';\n";
echo "var prod2  = '$prod2';  \n";
echo "var layer2 = '$layer2'; \n";
echo "var proj2  = '$proj2';  \n";
echo "var yr2	 = '$yr2';    \n";
echo "var mo2	 = '$mo2';    \n";
echo "var dy2	 = '$dy2';    \n\n";

$img2=$IMGDIR_ADV.$sat2."/".$yyyy."-".$mm."-".$dd."/mirs_".$alg2.$SENSORTAG[$sat2].$region2."_".$yyyy.$mm.$dd."_".$prod2."_".$sfc2."_".$cend2.".png";

$sat3='n18';  
$alg3='adv';  
$cend3='as'; 
$sfc3='all';  
$region3='glb';  
$prod3='em'; 
$layer3='23v';
$proj3='';
$yr3=$yyyy;
$mo3=$mm;
$dy3=$dd;

if(isset($_POST['sat3']))   { $sat3   = $_POST['sat3'];   }
if(isset($_POST['alg3']))   { $alg3   = $_POST['alg3'];   }
if(isset($_POST['cend3']))  { $cend3  = $_POST['cend3'];  }
if(isset($_POST['sfc3']))   { $sfc3   = $_POST['sfc3'];   }
if(isset($_POST['region3'])){ $region3= $_POST['region3'];}
if(isset($_POST['prod3']))  { $prod3  = $_POST['prod3'];  }
if(isset($_POST['layer3'])) { $layer3 = $_POST['layer3']; }
if(isset($_POST['proj3']))  { $proj3  = $_POST['proj3'];  }
if(isset($_POST['yr3']))    { $yr3    = $_POST['yr3'];    }
if(isset($_POST['mo3']))    { $mo3    = $_POST['mo3'];    }
if(isset($_POST['dy3']))    { $dy3    = $_POST['dy3'];    }

echo "var sat3   = '$sat3';   \n";
echo "var alg3   = '$alg3';   \n";
echo "var cend3  = '$cend3';  \n";
echo "var sfc3   = '$sfc3';   \n";
echo "var region3= '$region3';\n";
echo "var prod3  = '$prod3';  \n";
echo "var layer3 = '$layer3'; \n";
echo "var proj3  = '$proj3';  \n";
echo "var yr3	 = '$yr3';    \n";
echo "var mo3	 = '$mo3';    \n";
echo "var dy3	 = '$dy3';    \n\n";

$img3=$IMGDIR_ADV.$sat3."/".$yyyy."-".$mm."-".$dd."/mirs_".$alg3.$SENSORTAG[$sat3].$region3."_".$yyyy.$mm.$dd."_".$prod3."_".$layer3."_".$sfc3."_".$cend3.".png";

$sat4='n18';  
$alg4='adv';  
$cend4='as'; 
$sfc4='all';  
$region4='glb';  
$prod4='temp'; 
$layer4='100mb';
$proj4='';
$yr4=$yyyy;
$mo4=$mm;
$dy4=$dd;

if(isset($_POST['sat4']))   { $sat4   = $_POST['sat4'];   }
if(isset($_POST['alg4']))   { $alg4   = $_POST['alg4'];   }
if(isset($_POST['cend4']))  { $cend4  = $_POST['cend4'];  }
if(isset($_POST['sfc4']))   { $sfc4   = $_POST['sfc4'];   }
if(isset($_POST['region4'])){ $region4= $_POST['region4'];}
if(isset($_POST['prod4']))  { $prod4  = $_POST['prod4'];  }
if(isset($_POST['layer4'])) { $layer4 = $_POST['layer4']; }
if(isset($_POST['proj4']))  { $proj4  = $_POST['proj4'];  }
if(isset($_POST['yr4']))    { $yr4    = $_POST['yr4'];    }
if(isset($_POST['mo4']))    { $mo4    = $_POST['mo4'];    }
if(isset($_POST['dy4']))    { $dy4    = $_POST['dy4'];    }

echo "var sat4   = '$sat4';   \n";
echo "var alg4   = '$alg4';   \n";
echo "var cend4  = '$cend4';  \n";
echo "var sfc4   = '$sfc4';   \n";
echo "var region4= '$region4';\n";
echo "var prod4  = '$prod4';  \n";
echo "var layer4 = '$layer4'; \n";
echo "var proj4  = '$proj4';  \n";
echo "var yr4	 = '$yr4';    \n";
echo "var mo4	 = '$mo4';    \n";
echo "var dy4	 = '$dy4';    \n";

$img4=$IMGDIR_ADV.$sat4."/".$yyyy."-".$mm."-".$dd."/mirs_".$alg4.$SENSORTAG[$sat4].$region4."_".$yyyy.$mm.$dd."_".$prod4."_".$layer4."_".$sfc4."_".$cend4.".png";

echo "</script>\n";
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

<script language="javascript" type="text/javascript" src="product.js"> </script>

</head>


<body onLoad="loadInitialImages1(alg1,sat1,cend1,sfc1,region1,prod1,layer1,proj1,yr1,mo1,dy1)">
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
			require('includes/Sample_NavDiv_product1.inc');
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
echo "					<input type=hidden name=alg2 value=\"$alg2\"/>\n";
echo "					<input type=hidden name=cend2 value=\"$cend2\"/>\n";
echo "					<input type=hidden name=sfc2 value=\"$sfc2\"/>\n";
echo "					<input type=hidden name=region2 value=\"$region2\"/>\n";
echo "					<input type=hidden name=prod2 value=\"$prod2\"/>\n";
echo "					<input type=hidden name=layer2 value=\"$layer2\"/>\n";
echo "					<input type=hidden name=proj2 value=\"$proj2\"/>\n";
echo "					<input type=hidden name=yr2 value=\"$yr2\"/>\n";
echo "					<input type=hidden name=mo2 value=\"$mo2\"/>\n";
echo "					<input type=hidden name=dy2 value=\"$dy2\"/>\n";

echo "					<input type=hidden name=sat3 value=\"$sat3\"/>\n";
echo "					<input type=hidden name=alg3 value=\"$alg3\"/>\n";
echo "					<input type=hidden name=cend3 value=\"$cend3\"/>\n";
echo "					<input type=hidden name=sfc3 value=\"$sfc3\"/>\n";
echo "					<input type=hidden name=region3 value=\"$region3\"/>\n";
echo "					<input type=hidden name=prod3 value=\"$prod3\"/>\n";
echo "					<input type=hidden name=layer3 value=\"$layer3\"/>\n";
echo "					<input type=hidden name=proj3 value=\"$proj3\"/>\n";
echo "					<input type=hidden name=yr3 value=\"$yr3\"/>\n";
echo "					<input type=hidden name=mo3 value=\"$mo3\"/>\n";
echo "					<input type=hidden name=dy3 value=\"$dy3\"/>\n";

echo "					<input type=hidden name=sat4 value=\"$sat4\"/>\n";
echo "					<input type=hidden name=alg4 value=\"$alg4\"/>\n";
echo "					<input type=hidden name=cend4 value=\"$cend4\"/>\n";
echo "					<input type=hidden name=sfc4 value=\"$sfc4\"/>\n";
echo "					<input type=hidden name=region4 value=\"$region4\"/>\n";
echo "					<input type=hidden name=prod4 value=\"$prod4\"/>\n";
echo "					<input type=hidden name=layer4 value=\"$layer4\"/>\n";
echo "					<input type=hidden name=proj4 value=\"$proj4\"/>\n";
echo "					<input type=hidden name=yr4 value=\"$yr4\"/>\n";
echo "					<input type=hidden name=mo4 value=\"$mo4\"/>\n";
echo "					<input type=hidden name=dy4 value=\"$dy4\"/>\n";
?>

				<table border="0" cellpadding="0" cellspacing="0" class="tableGrid" bgcolor="#eeeeee">

				<TR align=left>

				<TD align=center nowrap class="productTd">
				  <input class="productInput" type=submit value="2 X 2 Panel" style="background-color: lightblue" onClick="changePanel(2)" title="2 Row by 2 Column Panel Display"> &nbsp;&nbsp;
				  <input class="productInput" type=submit value="4 X 1 Panel" style="background-color: lightblue" onClick="changePanel(4)" title="4 Row by 1 Column Panel Display"> &nbsp;&nbsp;
				  
				 <B><font size=4>MIRS Low Resolution Products</font></B>&nbsp;&nbsp;
				  <input class="productInput" type="button" style="background-color: lightblue" 
				  	 onclick="launchAnimation1()" value="Start Animation" title="This will start one month images loop" /> &nbsp;&nbsp;
				  <input class="productInput" type="button" style="background-color: lightblue" 
				  	 onclick="stopAnimation()"    value="Stop Animation" title="This will stop images loop" />
				</TD>
				</TR>


				<TR><TD class="productTd" id="panel1" align=center valign=top height=575>Sensor:
				<select class="productSelect" id="sat1" name="sat1" title="Select a Satellite Sensor"
					onChange="changeSatellite_template( document.form.alg1, document.form.sat1, document.form.prod1, document.form.layer1, document.form.proj1, 'layer1', 'proj1', '1' ); loadImage1();">
					<option value="n18"    >NOAA-18</option>
					<option value="n19"    >NOAA-19</option>
					<option value="metopA" >METOP-A</option>
					<option value="f16"    >F16/SSMIS</option>
					<option value="f18"    >F18/SSMIS</option>
					<option value="mtma"  title="MT-MADRAS is based on simulated proxy data for testing purposes only">MT/MADRAS</option>
					<option value="mtsa"  title="MT-SAPHIR is based on simulated proxy data for testing purposes only">MT/SAPHIR</option>
				</select>
				
				&nbsp;Algorithm:
				<select class="productSelect" id="alg1" name="alg1" title="Select Algorithm(MIRS 1DVAR Advanced Algorithm or MSPPS Heritage Algorithm"
					onChange="changeSatellite_template( document.form.alg1, document.form.sat1, document.form.prod1, document.form.layer1, document.form.proj1, 'layer1', 'proj1', '1' ); loadImage1();">
					<option value="adv"  >1DVAR</option>
					<option value="her"  >Heritage</option>
				</select>
				
				&nbsp;Product:
				<select class="productSelect" id="prod1" name="prod1" title="Select a Product"
					onChange="changeProduct_template( document.form.alg1, document.form.sat1, document.form.prod1, document.form.layer1, 'layer1', 'proj1' );loadImage1();">
<?php
require('includes/productOptions.inc');
?>
				</select>
				
				<select id="layer1" name="layer1" class="optioninvisible" onChange="loadImage1()" title="Select a Channel/Layer/Map Projection">
				</select>

				<select id="proj1" name="proj1" class="optioninvisible" onChange="loadImage1();" 
					title="choose a map projection type (cyl:cylindrical; pn:Northern Polar Stereographic; ps:Southern Polar Stereographic)">
					<option value=""   >cyl</option>
					<option value="pn_">pn</option>
					<option value="ps_">ps</option>
				</select>
				
				<br />
				
				Orbit:
				<select class="productSelect" id="cend1" name="cend1" onChange="loadImage1()" title="Select an Orbit Passing Mode (Ascending or Descending)">
					<option value="as" >Asc</option>
					<option value="ds" >Des</option>
				</select>
				
				&nbsp;Surface:
				<select class="productSelect" id="sfc1" name="sfc1" onChange="loadImage1()" title="Select a Surface Type">
					<option value="all" >All</option>
					<option value="sea" >Sea</option>
					<option value="lnd" >Land</option>
				</select>
				
				&nbsp;Region:
				<select class="productSelect" id="region1" name="region1" onChange="loadImage1()" title="Select a Region">
					<option value="glb" >Globe</option>
					<option value="us"  >USA</option>
				</select>
				
				&nbsp;Year:
				<select class="productSelect" id="yr1" name="yr1" onChange="loadImage1()" title="Select a year">
<?php
// year options
require('includes/yearOptions.inc');
?>
				</select>
				
				Month:
				<select class="productSelect" id="mo1" name="mo1" onChange="loadImage1()" title="Select a month">
<?php
// month options
require('includes/monthOptions.inc');
?>
				</select>
				
				Day:
				<select class="productSelect" id="dy1" name="dy1" onChange="loadImage1()" title="Select a day">
<?php
// day options
require('includes/dayOptions.inc');
?>
				</select>
				
				&nbsp;Browse:
				<input class="productInput" type="button" title="Image of the Previous Day"
				       onclick="rev(document.form.yr1,document.form.mo1,document.form.dy1,1);" value="<=">
				<input class="productInput" type="button" title="Image of the Next Day"
				       onclick="fwd(document.form.yr1,document.form.mo1,document.form.dy1,1);" value="=>">
				<br><br>

				<a id="href1" href="" target="_blank" >
				<img name="img1" src=""  alt=""  width=650 height=500 title="mirs daily product image"  style="display:block; clear:both;" /></a>

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
