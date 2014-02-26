<htm>

<head>
<title>STAR - MIRS Project - Product Monitoring</title>

<?php
echo "<script>\n";

$IMGDIR_ADV="images/";
$IMGDIR_HER="/corp/scsb/mspps/GIFs/";

$SENSORTAG = array( "f16"    => "_dmsp_f16_ssmis_", 
		    "n18"    => "_poes_n18_amsuamhs_",
		    "metopA" => "_poes_metopA_amsuamhs_",
		    "n19"    => "_poes_n19_amsuamhs_",
		    "f18"    => "_dmsp_f18_ssmis_",
		    "mtma"   => "_mt_mtma_madras_",
		    "mtsa"   => "_mt_mtsa_saphir_",       
		    "gcomw1" => "_eos_gcomw1_amsr2_"
		  );


//$yesterday = mktime(0, 0, 0, date("m"), date("d")-1, date("Y"));

//$yyyy = date("Y",$yesterday);
//$mm   = date("m",$yesterday);
//$dd   = date("d",$yesterday);

$yyyy = "2012";
$mm   = "11";
$dd   = "01";

$sat1='gcomw1';  
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


$sat2='gcomw1';  
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

$sat3='gcomw1';  
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

$sat4='gcomw1';  
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
  	visibility: visible;
	width: 150px;
}

select.optioninvisible {
	visibility: hidden;
	width: 150px;
}
  

td.productTd {
	text-align: center;
	vertical-align: middle;
}


input.productInput {
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

input, select
{
   width: 150px;
}
 
</style>

<script language="javascript" type="text/javascript" src="AMSR2_old.js"> </script>

</head>


<body onLoad="loadInitialImages(alg1,sat1,cend1,sfc1,region1,prod1,layer1,proj1,yr1,mo1,dy1, alg2,sat2,cend2,sfc2,region2,prod2,layer2,proj2,yr2,mo2,dy2, alg3,sat3,cend3,sfc3,region3,prod3,layer3,proj3,yr3,mo3,dy3, alg4,sat4,cend4,sfc4,region4,prod4,layer4,proj4,yr4,mo4,dy4 );">


<form name="form" action="" method="post">



<table align=center border="1" cellpadding="0" cellspacing="0" bgcolor="#eeeeee" width=1600>


<tr>
	<td class="imageContainer" id="panel1">
<select 
	title="choose a satellite sensor"
	 
	id="sat1" name="sat1" 
	title="choose a satellite sensor"
	onChange="changeSatellite_template( document.form.alg1, document.form.sat1, document.form.prod1, document.form.layer1, document.form.proj1, 'layer1', 'proj1', '1' ); loadImage1();">
	<option value="n18">N18</option>
	<option value="n19">N19</option>
	<option value="metopA">MOA</option>
	<option value="f16">F16</option>
	<option value="f18">F18</option>
	<option value="trmm" title="TMI data has 4 days delay relative to other sensors">TMI</option>
	<option value="mtma" title="MT-MADRAS is based on simulated proxy data for testing purposes only">MADRAS</option>
	<option value="mtsa" title="MT-SAPHIR is based on simulated proxy data for testing purposes only">SAPHIR</option>
	<option value="gcomw1">GCOMW1/AMSR2</option>
</select>

<select id="alg1" name="alg1" 
	title="choose an algorithm" 
	onChange="changeAlgorithm_template( document.form.alg1, document.form.sat1, document.form.prod1, document.form.layer1, document.form.proj1, 'layer1', 'proj1', '1' ); loadImage1();">
	<option value="adv">MIRS</option>
	<option value="her">MSPPS</option>
</select>

<select id="cend1" name="cend1" 
	title="choose a passing mode for images ( ascending or descending )"
	onChange="loadImage1();">
	<option value="as">Asc</option>
	<option value="ds">Des</option>
</select>

<select id="sfc1" name="sfc1" 
	title="choose a surface type"
	onChange="loadImage1();">
	<option value="all">All</option>
	<option value="sea">Sea</option>
	<option value="lnd">Land</option>
</select>

<select id="region1" name="region1" 
	title="choose a region"
	onChange="loadImage1();">
	<option value="glb">Globe</option>
	<option value="us">US</option>
	<option value="eu">Europe</option>
	<option value="gulf">Gulf</option>
	<option value="china">China</option>
</select>

<select id="prod1" name="prod1" 
	title="choose a product"
	onChange="changeProduct_template( document.form.alg1, document.form.sat1, document.form.prod1, document.form.layer1, 'layer1', 'proj1' ); loadImage1();">
<?php
// product options
require('includes/PRODUCT_OPTS.inc');
?>
</select>

<select id="layer1" name="layer1" class="optioninvisible" onChange="loadImage1();" title="choose a layer/channel">
</select>


<select id="proj1" name="proj1" class="optioninvisible" onChange="loadImage1();" 
	title="choose a map projection type (cyl:cylindrical; pn:Northern Polar Stereographic; ps:Southern Polar Stereographic)">
	<option value=""   >cyl</option>
	<option value="pn_">pn</option>
	<option value="ps_">ps</option>
</select>

 	
<select id="yr1" name="yr1" 
	title="select year"
	onChange="loadImage1();">
<?php
// year options
require('includes/yearOptions.inc');
?>
</select>			  

<select id="mo1" name="mo1" 
	title="select month"
	onChange="loadImage1();">
<?php
// month options
require('includes/monthOptions.inc');
?>
</select>			  

<select id="dy1" name="dy1" 
	title="select day"
	onChange="loadImage1();">	  
<?php
// day options
require('includes/dayOptions.inc');
?>
</select>

 
<input class="productInput" type="button" onclick="rev(document.form.yr1,document.form.mo1,document.form.dy1,1);" 
title="previous day image" value="<==">  
<input class="productInput" type="button" onclick="fwd(document.form.yr1,document.form.mo1,document.form.dy1,1);"
title="next day image" value="==>">

</td>


<td>
<a id="href1" href="" target="_blank"><img 
	name="img1" src="<?php echo $img1 ?>" 
	alt="<?php echo $img1 ?>" 
	width=650 height=448></a>
</td>






<td>
<a id="href2" href="" target="_blank"><img 
	name="img2" src="<?php echo $img2 ?>" 
	alt="<?php echo $img2 ?>" 
	width=650 height=448></a>
</td>


<td class="imageContainer" id="panel2">

<select id="sat2" name="sat2" 
	title="choose a satellite sensor"
	onChange="changeSatellite_template( document.form.alg2, document.form.sat2, document.form.prod2, document.form.layer2, document.form.proj2, 'layer2', 'proj2', '2' ); loadImage2();">
	<option value="n18">N18</option>
	<option value="n19">N19</option>
	<option value="metopA">MOA</option>
	<option value="f16">F16</option>
	<option value="f18">F18</option>
	<!--option value="npp">NPP ATMS</option-->
	<option value="trmm" title="TMI data has 4 days delay relative to other sensors">TMI</option>
	<option value="mtma" title="MT-MADRAS is based on simulated proxy data for testing purposes only">MADRAS</option>
	<option value="mtsa" title="MT-SAPHIR is based on simulated proxy data for testing purposes only">SAPHIR</option>
	<option value="gcomw1">GCOMW1/AMSR2</option>
</select>

<select id="alg2" name="alg2" 
	title="choose an algorithm" 
	onChange="changeSatellite_template( document.form.alg2, document.form.sat2, document.form.prod2, document.form.layer2, document.form.proj2, 'layer2', 'proj2', '2' ); loadImage2();">
	<option value="adv">MIRS</option>
	<option value="her">MSPPS</option>
</select>

<select id="cend2" name="cend2" 
	title="choose a passing mode ( ascending or descending )"
	onChange="loadImage2();">
	<option value="as">Asc</option>
	<option value="ds">Des</option>
</select>

<select id="sfc2" name="sfc2" 
	title="choose a surface type"
	onChange="loadImage2();">
	<option value="all">All</option>
	<option value="sea">Sea</option>
	<option value="lnd">Land</option>
</select>

<select id="region2" name="region2" 
	title="choose a region"
	onChange="loadImage2();">
	<option value="glb">Globe</option>
	<option value="us">US</option>
	<option value="eu">Europe</option>
	<option value="gulf">Gulf</option>
	<option value="china">China</option>
</select>


<select id="prod2" name="prod2" 
	title="choose a product"
	onChange="changeProduct_template( document.form.alg2, document.form.sat2, document.form.prod2, document.form.layer2, 'layer2', 'proj2' );loadImage2();">
<?php
// product options
require('includes/PRODUCT_OPTS.inc');
?>
</select>

<select id="layer2"  name="layer2" class="optioninvisible" onChange="loadImage2();" title="choose a layer/channel">
</select>

<select id="proj2" name="proj2" class="optioninvisible" onChange="loadImage2();" 
	title="choose a map projection type (cyl:cylindrical; pn:Northern Polar Stereographic; ps:Southern Polar Stereographic)">
	<option value=""   >cyl</option>
	<option value="pn_">pn</option>
	<option value="ps_">ps</option>
</select>


<select id="yr2" name="yr2"
	title="select year"
	onChange="loadImage2();">
<?php
// year options
require('includes/yearOptions.inc');
?>
</select>			  

<select id="mo2" name="mo2"
	title="select month"
	onChange="loadImage2();">	  
<?php
// month options
require('includes/monthOptions.inc');
?>
</select>			  

<select id="dy2" name="dy2"
	title="select day"
	onChange="loadImage2();">	  
<?php
// day options
require('includes/dayOptions.inc');
?>
</select>


<input class="productInput" type="button" 
onclick="rev(document.form.yr2,document.form.mo2,document.form.dy2,2);"
title="previous day image"
value="<==">
<input class="productInput" type="button" 
onclick="fwd(document.form.yr2,document.form.mo2,document.form.dy2,2);"
title="next day image"
value="==>">

</td>

</tr>







<tr>

<td class="imageContainer" id="panel3">

<select id="sat3" name="sat3" 
	title="choose a satellite sensor"
	onChange="changeSatellite_template( document.form.alg3, document.form.sat3, document.form.prod3, document.form.layer3, document.form.proj3, 'layer3', 'proj3', '3' ); loadImage3();">
	<option value="n18">N18</option>
	<option value="n19">N19</option>
	<option value="metopA">MOA</option>
	<option value="f16">F16</option>
	<option value="f18">F18</option>
	<!--option value="npp">NPP ATMS</option-->
	<option value="trmm" title="TMI data has 4 days delay relative to other sensors">TMI</option>
	<option value="mtma" title="MT-MADRAS is based on simulated proxy data for testing purposes only">MADRAS</option>
	<option value="mtsa" title="MT-SAPHIR is based on simulated proxy data for testing purposes only">SAPHIR</option>
	<option value="gcomw1">GCOMW1/AMSR2</option>
</select>	

<select id="alg3" name="alg3"
	title="choose an algorithm"  
	onChange="changeSatellite_template( document.form.alg3, document.form.sat3, document.form.prod3, document.form.layer3, document.form.proj3, 'layer3', 'proj3', '3' ); loadImage3();">
	<option value="adv">MIRS</option>
	<option value="her">MSPPS</option>
</select>

<select id="cend3" name="cend3" 
	title="choose a passing mode ( ascending or descending )"
	onChange="loadImage3();">
	<option value="as">Asc</option>
	<option value="ds">Des</option>
</select>

<select id="sfc3" name="sfc3" 
	title="choose a surface type"
	onChange="loadImage3();">
	<option value="all">All</option>
	<option value="sea">Sea</option>
	<option value="lnd">Land</option>
</select>

<select id="region3" name="region3" 
	title="choose a region"
	onChange="loadImage3();">
	<option value="glb">Globe</option>
	<option value="us">US</option>
	<option value="eu">Europe</option>
	<option value="gulf">Gulf</option>
	<option value="china">China</option>
</select>


<select id="prod3" name="prod3" 
	title="choose a product"
	onChange="changeProduct_template( document.form.alg3, document.form.sat3, document.form.prod3, document.form.layer3, 'layer3', 'proj3' );loadImage3();">
<?php
// product options
require('includes/PRODUCT_OPTS.inc');
?>
</select>

<select id="layer3"  name="layer3" class="optioninvisible" onChange="loadImage3();" title="choose a channel">
</select>

<select id="proj3" name="proj3" class="optioninvisible" onChange="loadImage3();" 
	title="choose a map projection type (cyl:cylindrical; pn:Northern Polar Stereographic; ps:Southern Polar Stereographic)">
	<option value=""   >cyl</option>
	<option value="pn_">pn</option>
	<option value="ps_">ps</option>
</select>


<select id="yr3" name="yr3"
	title="select year"
	onChange="loadImage3();">
<?php
// year options
require('includes/yearOptions.inc');
?>
</select>			  

<select id="mo3" name="mo3"
	title="select month"
	onChange="loadImage3();">
<?php
// month options
require('includes/monthOptions.inc');
?>
</select>			  

<select id="dy3" name="dy3"
	title="select day"
	onChange="loadImage3();">	  
<?php
// day options
require('includes/dayOptions.inc');
?>
</select>			  

 
<input class="productInput" type="button" 
onclick="rev(document.form.yr3,document.form.mo3,document.form.dy3,3);"
title="previous day image" 
value="<==">
<input class="productInput" type="button" 
onclick="fwd(document.form.yr3,document.form.mo3,document.form.dy3,3);"
title="next day image"
value="==>">

</td>

<td>
<a id="href3" href="" target="_blank">
<img name="img3" src="<?php echo $img3 ?>" 
	alt="<?php echo $img3 ?>" 
	width=650 height=448 /></a>
</td>



<td>
<a id="href4" href="" target="_blank">
<img name="img4" src="<?php echo $img4 ?>" 
	alt="<?php echo $img4 ?>" 
	width=650 height=448 /></a>
</td>


<td class="imageContainer" id="panel4">

<select id="sat4" name="sat4" 
	title="choose a satellite sensor"
	onChange="changeSatellite_template( document.form.alg4, document.form.sat4, document.form.prod4, document.form.layer4, document.form.proj4, 'layer4', 'proj4', '4' ); loadImage4();">
	<option value="n18">N18</option>
	<option value="n19">N19</option>
	<option value="metopA">MOA</option>
	<option value="f16">F16</option>
	<option value="f18">F18</option>
	<!--option value="npp">NPP ATMS</option-->
	<option value="trmm" title="TMI data has 4 days delay relative to other sensors">TMI</option>
	<option value="mtma" title="MT-MADRAS is based on simulated proxy data for testing purposes only">MADRAS</option>
	<option value="mtsa" title="MT-SAPHIR is based on simulated proxy data for testing purposes only">SAPHIR</option>
	<option value="gcomw1">GCOMW1/AMSR2</option>
</select>

<select id="alg4" name="alg4" 
	title="choose an algorithm" 
	onChange="changeSatellite_template( document.form.alg4, document.form.sat4, document.form.prod4, document.form.layer4, document.form.proj4, 'layer4', 'proj4', '4' ); loadImage4();">
	<option value="adv">MIRS</option>
	<option value="her">MSPPS</option>
</select>

<select   id="cend4" name="cend4" 
	title="choose a passing mode ( ascending or descending )"
	onChange="loadImage4();">
	<option value="as">Asc</option>
	<option value="ds">Des</option>
</select>

<select id="sfc4" name="sfc4" 
	title="choose a surface type"				
	onChange="loadImage4();">
	<option value="all">All</option>
	<option value="sea">Sea</option>
	<option value="lnd">Land</option>
</select>

<select id="region4" name="region4" 
	title="choose a region"
	onChange="loadImage4();">
	<option value="glb">Globe</option>
	<option value="us">US</option>
	<option value="eu">Europe</option>
	<option value="gulf">Gulf</option>
	<option value="china">China</option>
</select>


<select id="prod4" name="prod4" 
	title="choose a product"
	onChange="changeProduct_template( document.form.alg4, document.form.sat4, document.form.prod4, document.form.layer4, 'layer4', 'proj4' );loadImage4();">
<?php
// product options
require('includes/PRODUCT_OPTS.inc');
?>
				</select>

				<select id="layer4"  name="layer4" class="optioninvisible" onChange="loadImage4();" title="choose a pressure layer">
				</select>

				<select id="proj4" name="proj4" class="optioninvisible" onChange="loadImage4();" 
					title="choose a map projection type (cyl:cylindrical; pn:Northern Polar Stereographic; ps:Southern Polar Stereographic)">
					<option value=""   >cyl</option>
					<option value="pn_">pn</option>
					<option value="ps_">ps</option>
				</select>

				
				<select id="yr4" name="yr4"
				title="select year"
				onChange="loadImage4();">
<?php
// year options
require('includes/yearOptions.inc');
?>
				</select>			  

				<select id="mo4" name="mo4"
					title="select month"
					onChange="loadImage4();">	  
<?php
// month options
require('includes/monthOptions.inc');
?>
				</select>			  

				<select id="dy4" name="dy4"
					title="select day"
					onChange="loadImage4();">	  
<?php
// day options
require('includes/dayOptions.inc');
?>
</select>			  


<input class="productInput" type="button" 
onclick="rev(document.form.yr4,document.form.mo4,document.form.dy4,4);" 
title="previous day image"
value="<==">
<input class="productInput" type="button" 
onclick="fwd(document.form.yr4,document.form.mo4,document.form.dy4,4);" 
title="next day image"
value="==>">


</td>

</tr>


</table>
</form>


</body>
</html>
