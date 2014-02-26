<?php include("password_protect.php"); ?>

<?php
// insert header that forbids caching and carries doctype
// and html tag;
require('includes/noCacheHeader.inc');
?>

<META name="verify-v1" content="CwbLBcFt9+GqRTgaLZsENmPnSWNB5MStHHdYsB7U2nI=">
<title>STAR - MIRS Project - Comparison to Stage IV</title>
<?php
// style links and the icon link
// pulls in .css files for regular and print display
require('includes/styleLinks.inc');
?>

<?php

$sat='';  
$prod=''; 
$sfc='';
$cend='';
$yr='';   
$mo='';   
$dy='';   

if(isset($_POST['sat']))   { $sat   = $_POST['sat'];   }
if(isset($_POST['prod']))  { $prod  = $_POST['prod'];  }
if(isset($_POST['cend']))  { $cend  = $_POST['cend'];  }
if(isset($_POST['sfc']))   { $sfc   = $_POST['sfc'];   }
if(isset($_POST['yr']))    { $yr    = $_POST['yr'];    }
if(isset($_POST['mo']))    { $mo    = $_POST['mo'];    }
if(isset($_POST['dy']))    { $dy    = $_POST['dy'];    }

echo "<script> var sat   = '$sat';   </script>";
echo "<script> var prod  = '$prod';  </script>";
echo "<script> var cend  = '$cend';  </script>";
echo "<script> var sfc   = '$sfc';   </script>";
echo "<script> var yr    = '$yr';    </script>";
echo "<script> var mo    = '$mo';    </script>";
echo "<script> var dy    = '$dy';    </script>";

$sat2='';  
$prod2=''; 
$cend2='';
$sfc2='';
$yr2='';   
$mo2='';   
$dy2='';   

if(isset($_POST['sat2']))   { $sat2   = $_POST['sat2'];   }
if(isset($_POST['prod2']))  { $prod2  = $_POST['prod2'];  }
if(isset($_POST['cend2']))  { $cend2  = $_POST['cend2'];  }
if(isset($_POST['sfc2']))   { $sfc2   = $_POST['sfc2'];   }
if(isset($_POST['yr2']))    { $yr2    = $_POST['yr2'];    }
if(isset($_POST['mo2']))    { $mo2    = $_POST['mo2'];    }
if(isset($_POST['dy2']))    { $dy2    = $_POST['dy2'];    }

echo "<script> var sat2   = '$sat2';   </script>";
echo "<script> var prod2  = '$prod2';  </script>";
echo "<script> var cend2  = '$cend2';  </script>";
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

<script language="javascript" type="text/javascript" src="validationstage4.js"></script>


</head>
<body  onLoad="loadInit(sat,prod,cend,sfc,yr,mo,dy)">
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
			require('includes/Sample_NavDiv_validationstage4v.inc');
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


				<FORM NAME=form action="" method="post">

				<table border="0" cellpadding="0" cellspacing="0" class="tableGrid" bgcolor="#eeeeee">

				<TR align=center><TD align=center bgcolor="#eeeeee" height=18 class="productTd">
				<input  type=submit  value="Time Series"   onClick="changePanel(1)" style="background-color: lightblue;"> &nbsp;&nbsp;
				<input  type=submit  value="4 X 2 Panels"  onClick="changePanel(2)" style="background-color: lightblue;"> &nbsp;&nbsp;
				
				<B><font size=4>MIRS Comparison to Stage IV</font></B> (<em>Click image for large view</em>)
				</TD></TR>

				<TR align=center>
				<TD align=center>

				&nbsp;Sensor:
				<select name="sat" onChange="loadImage_top();" >
				<option value="n18"	>NOAA-18</option>
				<option value="n19"	>NOAA-19</option>
				<option value="metopA"	>METOP-A</option>
				<option value="metopB"	>METOP-B</option>
				<option value="f16"	>F16/SSMIS</option>
				<option value="f18"	>F18/SSMIS</option>
				<option value="npp"	>NPP/ATMS</option>
				<option value="gcomw1"	>GCOMW1/AMSR2</option>
				<option value="trmm"	>TRMM/TMI</option>
				</select>

				&nbsp;Product:
				<select name="prod" onChange="changeProduct(this.value); loadImage_top();">
				<option value="rr"       >Rain Rate</option>
				<!--option value="pixel" >Collocated Pixels</option-->
				</select>

				<br>
				
				&nbsp;Mode:
				<select id="cend" name="cend" onChange="loadImage_top();">
				<option value="as">Asc</option>
				<option value="ds">Des</option>
				</select>			  

				&nbsp;Sfc:
				<select id="sfc" name="sfc" onChange="loadImage_top();">
				<option value="lnd">Land</option>
				</select>			  

				&nbsp;Year:
				<select id="yr" name="yr" onChange="loadImage_top();">
<?php
require('includes/yearOptions.inc');
?>
				</select>			  
				
				Month:
				<select id="mo" name="mo" onChange="loadImage_top();">	  
<?php
require('includes/monthOptions.inc');
?>
				</select>			  
				
				Day:
				<select id="dy" name="dy" onChange="loadImage_top();">	  
<?php
// day options
require('includes/dayOptions.inc');
?>
				</select>			  

				&nbsp;Browse:
				<input type="button" onclick="rev();" value="<=">
				<input type="button" onclick="fwd();" value="=>">
				
				</TD>
				</TR>


				<TR align=center>
				  <TD align=center nowrap width=650 height=500>
				    <a id="href1" href="" target=_blank>
				    <img name="img1" src="" width=650 height=500 ></a>
				  </TD>
				</TR>
				
				<TR align=center>
				  <TD align=center nowrap width=650 height=500>
				    <a id="href2" href="" target=_blank>
				    <img name="img2" src="" width=650 height=500 ></a>
				  </TD>
				</TR>

				<TR align=center>
				  <TD id="box3" align=center nowrap width=650 height=500>
				    <a id="href3" href="" target=_blank>
				    <img name="img3" src="" width=650 height=500 ></a>
				  </TD>
				</TR>
				
				<TR align=center>
				  <TD id="box4" align=center nowrap width=650 height=500>
				    <a id="href4" href="" target=_blank>
				    <img name="img4" src="" width=650 height=500 ></a>
				  </TD>
				</TR>

				<TR align=center>
				  <TD id="box5" align=center nowrap width=650 height=500>
				    <a id="href5" href="" target=_blank>
				    <img name="img5" src="" width=650 height=500 ></a>
				  </TD>
				</TR>
				
				<TR align=center>
				  <TD id="box6" align=center nowrap width=650 height=500>
				    <a id="href6" href="" target=_blank>
				    <img name="img6" src="" width=650 height=500 ></a>
				  </TD>
				</TR>

				<TR align=center>
				  <TD id="box7" align=center nowrap width=650 height=500>
				    <a id="href7" href="" target=_blank>
				    <img name="img7" src="" width=650 height=500 ></a>
				  </TD>
				</TR>
				
				<TR align=center>
				  <TD id="box8" align=center nowrap width=650 height=500>
				    <a id="href8" href="" target=_blank>
				    <img name="img8" src="" width=650 height=500 ></a>
				  </TD>
				</TR>

				
				<TR align=center>
				  <TD id="box9" align=center nowrap width=650 height=500>
				    <a id="href9" href="" target=_blank>
				    <img name="img9" src="" width=650 height=500 ></a>
				  </TD>
				</TR>

				<TR align=center>
				  <TD id="boxa" align=center nowrap width=650 height=500>
				    <a id="hrefa" href="" target=_blank>
				    <img name="imga" src="" width=650 height=500 ></a>
				  </TD>
				</TR>


				<TR align=center>
				<TD align=center>
				&nbsp;Sensor:
				<select name="sat2" onChange="loadImage_bottom();" >
				<option value="n18"	>NOAA-18</option>
				<option value="n19"	>NOAA-19</option>
				<option value="metopA"	>METOP-A</option>
				<option value="metopB"	>METOP-B</option>
				<option value="f16"	>F16/SSMIS</option>
				<option value="f18"	>F18/SSMIS</option>
				<option value="npp"	>NPP/ATMS</option>
				<option value="gcomw1"	>GCOMW1/AMSR2</option>
				<option value="trmm"	>TRMM/TMI</option>
				</select>

				&nbsp;Product:
				<select name="prod2" onChange="changeProduct(this.value); loadImage_bottom();">
				<option value="rr"      >Rain Rate</option>
				<!--option value="pixel"   >Collocated Pixels</option-->
				</select>

				<br>

				&nbsp;Mode:
				<select id="cend2" name="cend2" onChange="loadImage_bottom();">
				<option value="as">Asc</option>
				<option value="ds">Des</option>
				</select>			  

				&nbsp;Sfc:
				<select id="sfc2" name="sfc2" onChange="loadImage_bottom();">
				<option value="lnd">Land</option>
				</select>
							  
				&nbsp;Year:
				<select id="yr2" name="yr2" onChange="loadImage_bottom();">
<?php
require('includes/yearOptions.inc');
?>
				</select>			  
				
				Month:
				<select id="mo2" name="mo2" onChange="loadImage_bottom();">	  
<?php
require('includes/monthOptions.inc');
?>
				</select>			  
				
				Day:
				<select id="dy2" name="dy2" onChange="loadImage_bottom();">	  
<?php
require('includes/dayOptions.inc');
?>
				</select>			  
				
				&nbsp;Browse:
				<input type="button" onclick="rev();" value="<=">
				<input type="button" onclick="fwd();" value="=>">

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
