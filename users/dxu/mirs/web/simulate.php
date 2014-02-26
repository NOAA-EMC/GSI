<?php include("password_protect.php"); ?>

<?php
// insert header that forbids caching and carries doctype
// and html tag;
require('includes/noCacheHeader.inc');
?>
<META name="verify-v1" content="CwbLBcFt9+GqRTgaLZsENmPnSWNB5MStHHdYsB7U2nI=">
<title>STAR - MIRS Project - Simulated Performance Monitoring</title>


<?php
// style links and the icon link
// pulls in .css files for regular and print display
require('includes/styleLinks.inc');
?>


<?php
$prod1=''; 
$layer1='';
$cond1='';  
$yr1='';   
$mo1='';   
$dy1='';   

if(isset($_POST['prod1']))   { $prod1   = $_POST['prod1'];   }
if(isset($_POST['layer1']))  { $layer1  = $_POST['layer1'];  }
if(isset($_POST['cond1']))   { $cond1   = $_POST['cond1'];   }
if(isset($_POST['yr1']))     { $yr1     = $_POST['yr1'];     }
if(isset($_POST['mo1']))     { $mo1     = $_POST['mo1'];     }
if(isset($_POST['dy1']))     { $dy1     = $_POST['dy1'];     }

echo "<script>\n";
echo " var prod1   = '$prod1';   \n";
echo " var layer1  = '$layer1';  \n";
echo " var cond1   = '$cond1';   \n";
echo " var yr1     = '$yr1';     \n";
echo " var mo1     = '$mo1';     \n";
echo " var dy1     = '$dy1';     \n";
echo "</script>\n";



$prod2=''; 
$layer2='';
$cond2='';  
$yr2='';   
$mo2='';   
$dy2='';   

if(isset($_POST['prod2']))   { $prod2   = $_POST['prod2'];   }
if(isset($_POST['layer2']))  { $layer2  = $_POST['layer2'];  }
if(isset($_POST['cond2']))   { $cond2   = $_POST['cond2'];   }
if(isset($_POST['yr2']))     { $yr2     = $_POST['yr2'];     }
if(isset($_POST['mo2']))     { $mo2     = $_POST['mo2'];     }
if(isset($_POST['dy2']))     { $dy2     = $_POST['dy2'];     }

echo "<script>\n";
echo " var prod2   = '$prod2';   \n";
echo " var layer2  = '$layer2';  \n";
echo " var cond2   = '$cond2';   \n";
echo " var yr2     = '$yr2';     \n";
echo " var mo2     = '$mo2';     \n";
echo " var dy2     = '$dy2';     \n";
echo "</script>\n";



$prod3=''; 
$layer3='';
$cond3='';  
$yr3='';   
$mo3='';   
$dy3='';   

if(isset($_POST['prod3']))   { $prod3   = $_POST['prod3'];   }
if(isset($_POST['layer3']))  { $layer3  = $_POST['layer3'];  }
if(isset($_POST['cond3']))   { $cond3   = $_POST['cond3'];   }
if(isset($_POST['yr3']))     { $yr3     = $_POST['yr3'];     }
if(isset($_POST['mo3']))     { $mo3     = $_POST['mo3'];     }
if(isset($_POST['dy3']))     { $dy3     = $_POST['dy3'];     }

echo "<script>\n";
echo " var prod3   = '$prod3';   \n";
echo " var layer3  = '$layer3';  \n";
echo " var cond3   = '$cond3';   \n";
echo " var yr3     = '$yr3';     \n";
echo " var mo3     = '$mo3';     \n";
echo " var dy3     = '$dy3';     \n";
echo "</script>\n";



$prod4=''; 
$layer4='';
$cond4='';  
$yr4='';   
$mo4='';   
$dy4='';   

if(isset($_POST['prod4']))   { $prod4   = $_POST['prod4'];   }
if(isset($_POST['layer4']))  { $layer4  = $_POST['layer4'];  }
if(isset($_POST['cond4']))   { $cond4   = $_POST['cond4'];   }
if(isset($_POST['yr4']))     { $yr4     = $_POST['yr4'];     }
if(isset($_POST['mo4']))     { $mo4     = $_POST['mo4'];     }
if(isset($_POST['dy4']))     { $dy4     = $_POST['dy4'];     }

echo "<script>\n";
echo " var prod4   = '$prod4';   \n";
echo " var layer4  = '$layer4';  \n";
echo " var cond4   = '$cond4';   \n";
echo " var yr4     = '$yr4';     \n";
echo " var mo4     = '$mo4';     \n";
echo " var dy4     = '$dy4';     \n";
echo "</script>\n";



$prod5=''; 
$layer5='';
$cond5='';  
$yr5='';   
$mo5='';   
$dy5='';   

if(isset($_POST['prod5']))   { $prod5   = $_POST['prod5'];   }
if(isset($_POST['layer5']))  { $layer5  = $_POST['layer5'];  }
if(isset($_POST['cond5']))   { $cond5   = $_POST['cond5'];   }
if(isset($_POST['yr5']))     { $yr5     = $_POST['yr5'];     }
if(isset($_POST['mo5']))     { $mo5     = $_POST['mo5'];     }
if(isset($_POST['dy5']))     { $dy5     = $_POST['dy5'];     }

echo "<script>\n";
echo " var prod5   = '$prod5';   \n";
echo " var layer5  = '$layer5';  \n";
echo " var cond5   = '$cond5';   \n";
echo " var yr5     = '$yr5';     \n";
echo " var mo5     = '$mo5';     \n";
echo " var dy5     = '$dy5';     \n";
echo "</script>\n";



$prod6=''; 
$layer6='';
$cond6='';  
$yr6='';   
$mo6='';   
$dy6='';   

if(isset($_POST['prod6']))   { $prod6   = $_POST['prod6'];   }
if(isset($_POST['layer6']))  { $layer6  = $_POST['layer6'];  }
if(isset($_POST['cond6']))   { $cond6   = $_POST['cond6'];   }
if(isset($_POST['yr6']))     { $yr6     = $_POST['yr6'];     }
if(isset($_POST['mo6']))     { $mo6     = $_POST['mo6'];     }
if(isset($_POST['dy6']))     { $dy6     = $_POST['dy6'];     }

echo "<script>\n";
echo " var prod6   = '$prod6';   \n";
echo " var layer6  = '$layer6';  \n";
echo " var cond6   = '$cond6';   \n";
echo " var yr6     = '$yr6';     \n";
echo " var mo6     = '$mo6';     \n";
echo " var dy6     = '$dy6';     \n";
echo "</script>\n";

?>


<style type="text/css">

select.optionvisible   {visibility:visible}
select.optioninvisible {visibility:hidden}

td.arrowvisible        {visibility:visible}
td.arrowinvisible      {visibility:hidden} 
  

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


<script language="javascript" type="text/javascript" src="simulate.js"></script>

</head>
<body onLoad="loadInitialImages(prod1,layer1,cond1,yr1,mo1,dy1,
				prod2,layer2,cond2,yr2,mo2,dy2,
				prod3,layer3,cond3,yr3,mo3,dy3,
				prod4,layer4,cond4,yr4,mo4,dy4,
				prod5,layer5,cond5,yr5,mo5,dy5,
				prod6,layer6,cond6,yr6,mo6,dy6)" >

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


				<FORM NAME=form action="simulatev.php" method="post">

				<table border="0" cellpadding="0" cellspacing="0" class="tableGrid" bgcolor="#eeeeee">

				<TR><TD align=center bgcolor="#eeeeee" height=18 colspan=2>
				<input  type=submit  value="6 X 1 Panels" style="background-color: lightblue" > &nbsp;&nbsp;
				<B><font size=4>MIRS AMSUA/MHS Simulated Performance</font></B> (<em>Click image for large view</em>)
				</TD></TR>




				<tr>
				
				<td class="imageContainer" id="panel1">
						<fieldset title="this fieldset groups form controls for the top left image">

				<select class="productSelect" id="prod1" name="prod1" 
					title="choose a product"
					onChange="changeProduct( this, document.form.layer1, 'layer1'); loadImage1(); ">
				<option value="inttpw" >TPW(MIRS vs model)</option> 
				<option value="em_allch_biasstd">Emissivity(MIRS vs model)</option>   
				<option value="tskin"  >Skin Temperature(MIRS vs model)</option>
				<option value="tb"     >TB(current vs previous)</option>  
				<option value="temp"   >Temperature(current vs previous)</option>
				<option value="wv"     >Water Vapor(current vs previous)</option>  
 				<option value="ave_temp_mean_vert">Temperature Bias(MIRS vs model)</option>  
 				<option value="ave_temp_stdv_vert">Temperature StdDev(MIRS vs model)</option>
 				<option value="ave_wv_mean_vert">Water Vapor Bias(MIRS vs model)</option>)  
			  	<option value="ave_wv_stdv_vert">Water Vapor StdDev(MIRS vs model)</option>
			  	<option value="clw">CLW(MIRS vs model)</option>
				</select>&nbsp;

				<select id="layer1" name="layer1" class="optioninvisible" onChange="loadImage1();" title="choose a layer/channel">
				</select>
				

				<br />
				
				
				<select class="productSelect" id="cond1" name="cond1" 
					title="choose a atmospheric condition"
					onChange="loadImage1();">
				<option value="clr"    >Clear</option>
				<option value="cld"    >Cloudy</option>
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
				title="previous day image" value="<=">  
				<input class="productInput" type="button" onclick="fwd(document.form.yr1,document.form.mo1,document.form.dy1,1);"
				title="next day image" value="=>"></fieldset>
				<br />
				<a id="href1" href="" target="_blank">
				<img name="img1" src="" alt="" width="325" height="250" style="display:block; clear:both;position:relative;left:15px;" /></a>
				</td>

				
				
				
				<td class="imageContainer" id="panel2">
						<fieldset title="this fieldset groups form controls for the top right image">

				<select class="productSelect" id="prod2" name="prod2" 
					title="choose a product"
					onChange="changeProduct( this, document.form.layer2, 'layer2' );loadImage2();">
				<option value="inttpw" >TPW(MIRS vs model)</option> 
				<option value="em_allch_biasstd">Emissivity(MIRS vs model)</option>   
				<option value="tskin"  >Skin Temperature(MIRS vs model)</option>
				<option value="tb"     >TB(current vs previous)</option>  
				<option value="temp"   >Temperature(current vs previous)</option>
				<option value="wv"     >Water Vapor(current vs previous)</option>  
 				<option value="ave_temp_mean_vert">Temperature Bias(MIRS vs model)</option>  
 				<option value="ave_temp_stdv_vert">Temperature StdDev(MIRS vs model)</option>
 				<option value="ave_wv_mean_vert">Water Vapor Bias(MIRS vs model)</option>)  
			  	<option value="ave_wv_stdv_vert">Water Vapor StdDev(MIRS vs model)</option>
			  	<option value="clw">CLW(MIRS vs model)</option>
				</select>&nbsp;

				<select id="layer2" name="layer2" class="optioninvisible" onChange="loadImage2();" title="choose a layer/channel">
				</select>
				

				<br />
				
				<select class="productSelect" id="cond2" name="cond2" 
					title="choose a atmospheric condition"
					onChange="loadImage2();">
				<option value="clr"    >Clear</option>
				<option value="cld"    >Cloudy</option>
				</select>&nbsp;
					
				<select class="productSelect" id="yr2" name="yr2" 
					title="select year"
					onChange="loadImage2();">
<?php
// year options
require('includes/yearOptions.inc');
?>
				</select>			  

				<select class="productSelect" id="mo2" name="mo2" 
					title="select month"
					onChange="loadImage2();">
<?php
// month options
require('includes/monthOptions.inc');
?>
				</select>			  

				<select class="productSelect" id="dy2" name="dy2" 
					title="select day"
					onChange="loadImage2();">	  
<?php
// day options
require('includes/dayOptions.inc');
?>
				</select>
							  
				&nbsp;
				<input class="productInput" type="button" onclick="rev(document.form.yr2,document.form.mo2,document.form.dy2,2);" 
				title="previous day image" value="<=">  
				<input class="productInput" type="button" onclick="fwd(document.form.yr2,document.form.mo2,document.form.dy2,2);"
				title="next day image" value="=>"></fieldset>
				<br />
				<a id="href2" href="" target="_blank">
				<img name="img2" src="" alt="" width="325" height="250" style="display:block; clear:both;position:relative;left:15px;" /></a>
				</td>
				</tr>





				<tr>
				
				<td class="imageContainer" id="panel3">
						<fieldset title="this fieldset groups form controls for the middle left image">

				<select class="productSelect" id="prod3" name="prod3" 
					title="choose a product"
					onChange="changeProduct( this, document.form.layer3, 'layer3'); loadImage3(); ">
				<option value="inttpw" >TPW(MIRS vs model)</option> 
				<option value="em_allch_biasstd">Emissivity(MIRS vs model)</option>   
				<option value="tskin"  >Skin Temperature(MIRS vs model)</option>
				<option value="tb"     >TB(current vs previous)</option>  
				<option value="temp"   >Temperature(current vs previous)</option>
				<option value="wv"     >Water Vapor(current vs previous)</option>  
 				<option value="ave_temp_mean_vert">Temperature Bias(MIRS vs model)</option>  
 				<option value="ave_temp_stdv_vert">Temperature StdDev(MIRS vs model)</option>
 				<option value="ave_wv_mean_vert">Water Vapor Bias(MIRS vs model)</option>)  
			  	<option value="ave_wv_stdv_vert">Water Vapor StdDev(MIRS vs model)</option>
			  	<option value="clw">CLW(MIRS vs model)</option>
				</select>&nbsp;

				<select id="layer3" name="layer3" class="optioninvisible" onChange="loadImage3();" title="choose a layer/channel">
				</select>
				

				<br />
				
				
				<select class="productSelect" id="cond3" name="cond3" 
					title="choose a atmospheric condition"
					onChange="loadImage3();">
				<option value="clr"    >Clear</option>
				<option value="cld"    >Cloudy</option>
				</select>&nbsp;
					
				
				<select class="productSelect" id="yr3" name="yr3" 
					title="select year"
					onChange="loadImage3();">
<?php
// year options
require('includes/yearOptions.inc');
?>
				</select>			  

				<select class="productSelect" id="mo3" name="mo3" 
					title="select month"
					onChange="loadImage3();">
<?php
// month options
require('includes/monthOptions.inc');
?>
				</select>			  

				<select class="productSelect" id="dy3" name="dy3" 
					title="select day"
					onChange="loadImage3();">	  
<?php
// day options
require('includes/dayOptions.inc');
?>
				</select>
							  
				&nbsp;
				<input class="productInput" type="button" onclick="rev(document.form.yr3,document.form.mo3,document.form.dy3,3);" 
				title="previous day image" value="<=">  
				<input class="productInput" type="button" onclick="fwd(document.form.yr3,document.form.mo3,document.form.dy3,3);"
				title="next day image" value="=>"></fieldset>
				<br />
				<a id="href3" href="" target="_blank">
				<img name="img3" src="" alt="" width="325" height="250" style="display:block; clear:both;position:relative;left:15px;" /></a>
				</td>

				
				
				
				<td class="imageContainer" id="panel4">
						<fieldset title="this fieldset groups form controls for the middle right image">

				<select class="productSelect" id="prod4" name="prod4" 
					title="choose a product"
					onChange="changeProduct( this, document.form.layer4, 'layer4' );loadImage4();">
				<option value="inttpw" >TPW(MIRS vs model)</option> 
				<option value="em_allch_biasstd">Emissivity(MIRS vs model)</option>   
				<option value="tskin"  >Skin Temperature(MIRS vs model)</option>
				<option value="tb"     >TB(current vs previous)</option>  
				<option value="temp"   >Temperature(current vs previous)</option>
				<option value="wv"     >Water Vapor(current vs previous)</option>  
 				<option value="ave_temp_mean_vert">Temperature Bias(MIRS vs model)</option>  
 				<option value="ave_temp_stdv_vert">Temperature StdDev(MIRS vs model)</option>
 				<option value="ave_wv_mean_vert">Water Vapor Bias(MIRS vs model)</option>)  
			  	<option value="ave_wv_stdv_vert">Water Vapor StdDev(MIRS vs model)</option>
			  	<option value="clw">CLW(MIRS vs model)</option>
				</select>&nbsp;

				<select id="layer4" name="layer4" class="optioninvisible" onChange="loadImage4();" title="choose a layer/channel">
				</select>
				

				<br />
				
				<select class="productSelect" id="cond4" name="cond4" 
					title="choose a atmospheric condition"
					onChange="loadImage4();">
				<option value="clr"    >Clear</option>
				<option value="cld"    >Cloudy</option>
				</select>&nbsp;
					
				<select class="productSelect" id="yr4" name="yr4" 
					title="select year"
					onChange="loadImage4();">
<?php
// year options
require('includes/yearOptions.inc');
?>
				</select>			  

				<select class="productSelect" id="mo4" name="mo4" 
					title="select month"
					onChange="loadImage4();">
<?php
// month options
require('includes/monthOptions.inc');
?>
				</select>			  

				<select class="productSelect" id="dy4" name="dy4" 
					title="select day"
					onChange="loadImage4();">	  
<?php
// day options
require('includes/dayOptions.inc');
?>
				</select>
							  
				&nbsp;
				<input class="productInput" type="button" onclick="rev(document.form.yr4,document.form.mo4,document.form.dy4,4);" 
				title="previous day image" value="<=">  
				<input class="productInput" type="button" onclick="fwd(document.form.yr4,document.form.mo4,document.form.dy4,4);"
				title="next day image" value="=>"></fieldset>
				<br />
				<a id="href4" href="" target="_blank">
				<img name="img4" src="" alt="" width="325" height="250" style="display:block; clear:both;position:relative;left:15px;" /></a>
				</td>
				</tr>



				<tr>
				
				<td class="imageContainer" id="panel5">
						<fieldset title="this fieldset groups form controls for the bottom left image">

				<select class="productSelect" id="prod5" name="prod5" 
					title="choose a product"
					onChange="changeProduct( this, document.form.layer5, 'layer5'); loadImage5(); ">
				<option value="inttpw" >TPW(MIRS vs model)</option> 
				<option value="em_allch_biasstd">Emissivity(MIRS vs model)</option>   
				<option value="tskin"  >Skin Temperature(MIRS vs model)</option>
				<option value="tb"     >TB(current vs previous)</option>  
				<option value="temp"   >Temperature(current vs previous)</option>
				<option value="wv"     >Water Vapor(current vs previous)</option>  
 				<option value="ave_temp_mean_vert">Temperature Bias(MIRS vs model)</option>  
 				<option value="ave_temp_stdv_vert">Temperature StdDev(MIRS vs model)</option>
 				<option value="ave_wv_mean_vert">Water Vapor Bias(MIRS vs model)</option>)  
			  	<option value="ave_wv_stdv_vert">Water Vapor StdDev(MIRS vs model)</option>
			  	<option value="clw">CLW(MIRS vs model)</option>
				</select>&nbsp;

				<select id="layer5" name="layer5" class="optioninvisible" onChange="loadImage5();" title="choose a layer/channel">
				</select>
				

				<br />
				
				
				<select class="productSelect" id="cond5" name="cond5" 
					title="choose a atmospheric condition"
					onChange="loadImage5();">
				<option value="clr"    >Clear</option>
				<option value="cld"    >Cloudy</option>
				</select>&nbsp;
					
				
				<select class="productSelect" id="yr5" name="yr5" 
					title="select year"
					onChange="loadImage5();">
<?php
// year options
require('includes/yearOptions.inc');
?>
				</select>			  

				<select class="productSelect" id="mo5" name="mo5" 
					title="select month"
					onChange="loadImage5();">
<?php
// month options
require('includes/monthOptions.inc');
?>
				</select>			  

				<select class="productSelect" id="dy5" name="dy5" 
					title="select day"
					onChange="loadImage5();">	  
<?php
// day options
require('includes/dayOptions.inc');
?>
				</select>
							  
				&nbsp;
				<input class="productInput" type="button" onclick="rev(document.form.yr5,document.form.mo5,document.form.dy5,5);" 
				title="previous day image" value="<=">  
				<input class="productInput" type="button" onclick="fwd(document.form.yr5,document.form.mo5,document.form.dy5,5);"
				title="next day image" value="=>"></fieldset>
				<br />
				<a id="href5" href="" target="_blank">
				<img name="img5" src="" alt="" width="325" height="250" style="display:block; clear:both;position:relative;left:15px;" /></a>
				</td>

				
				
				
				<td class="imageContainer" id="panel6">
						<fieldset title="this fieldset groups form controls for the bottom right image">

				<select class="productSelect" id="prod6" name="prod6" 
					title="choose a product"
					onChange="changeProduct( this, document.form.layer6, 'layer6' );loadImage6();">
				<option value="inttpw" >TPW(MIRS vs model)</option> 
				<option value="em_allch_biasstd">Emissivity(MIRS vs model)</option>   
				<option value="tskin"  >Skin Temperature(MIRS vs model)</option>
				<option value="tb"     >TB(current vs previous)</option>  
				<option value="temp"   >Temperature(current vs previous)</option>
				<option value="wv"     >Water Vapor(current vs previous)</option>  
 				<option value="ave_temp_mean_vert">Temperature Bias(MIRS vs model)</option>  
 				<option value="ave_temp_stdv_vert">Temperature StdDev(MIRS vs model)</option>
 				<option value="ave_wv_mean_vert">Water Vapor Bias(MIRS vs model)</option>)  
			  	<option value="ave_wv_stdv_vert">Water Vapor StdDev(MIRS vs model)</option>
			  	<option value="clw">CLW(MIRS vs model)</option>
				</select>&nbsp;

				<select id="layer6" name="layer6" class="optioninvisible" onChange="loadImage6();" title="choose a layer/channel">
				</select>
				

				<br />
				
				<select class="productSelect" id="cond6" name="cond6" 
					title="choose a atmospheric condition"
					onChange="loadImage6();">
				<option value="clr"    >Clear</option>
				<option value="cld"    >Cloudy</option>
				</select>&nbsp;
					
				<select class="productSelect" id="yr6" name="yr6" 
					title="select year"
					onChange="loadImage6();">
<?php
// year options
require('includes/yearOptions.inc');
?>
				</select>			  

				<select class="productSelect" id="mo6" name="mo6" 
					title="select month"
					onChange="loadImage6();">
<?php
// month options
require('includes/monthOptions.inc');
?>
				</select>			  

				<select class="productSelect" id="dy6" name="dy6" 
					title="select day"
					onChange="loadImage6();">	  
<?php
// day options
require('includes/dayOptions.inc');
?>
				</select>
							  
				&nbsp;
				<input class="productInput" type="button" onclick="rev(document.form.yr6,document.form.mo6,document.form.dy6,6);" 
				title="previous day image" value="<=">  
				<input class="productInput" type="button" onclick="fwd(document.form.yr6,document.form.mo6,document.form.dy6,6);"
				title="next day image" value="=>"></fieldset>
				<br />
				<a id="href6" href="" target="_blank">
				<img name="img6" src="" alt="" width="325" height="250" style="display:block; clear:both;position:relative;left:15px;" /></a>
				</td>
				</tr>



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
