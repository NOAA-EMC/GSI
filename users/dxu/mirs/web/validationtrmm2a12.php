<?php include("password_protect.php"); ?>

<?php
// insert header that forbids caching and carries doctype
// and html tag;
require('includes/noCacheHeader.inc');
?>

<META name="verify-v1" content="CwbLBcFt9+GqRTgaLZsENmPnSWNB5MStHHdYsB7U2nI=">
<title>STAR - MIRS Project - Comparison to TRMM_2A12</title>
<?php
// style links and the icon link
// pulls in .css files for regular and print display
require('includes/styleLinks.inc');
?>

<?php

$sat='';  
$prod='';  
$sfc='';
$yr='';   
$mo='';   
$dy='';   
$cend='';   

if(isset($_POST['sat']))   { $sat   = $_POST['sat'];   }
if(isset($_POST['prod']))  { $prod  = $_POST['prod'];  }
if(isset($_POST['sfc']))   { $sfc   = $_POST['sfc'];   }
if(isset($_POST['yr']))    { $yr    = $_POST['yr'];    }
if(isset($_POST['mo']))    { $mo    = $_POST['mo'];    }
if(isset($_POST['dy']))    { $dy    = $_POST['dy'];    }
if(isset($_POST['cend']))  { $cend  = $_POST['cend'];  }

echo "<script> var sat   = '$sat';   </script>";
echo "<script> var prod  = '$prod';   </script>";
echo "<script> var sfc   = '$sfc';   </script>";
echo "<script> var yr    = '$yr';    </script>";
echo "<script> var mo    = '$mo';    </script>";
echo "<script> var dy    = '$dy';    </script>";
echo "<script> var cend  = '$cend';  </script>";

$sat2='';  
$prod2='';  
$sfc2='';
$yr2='';   
$mo2='';   
$dy2='';   
$cend2='';   

if(isset($_POST['sat2']))   { $sat2   = $_POST['sat2'];   }
if(isset($_POST['prod2']))  { $prod2  = $_POST['prod2'];  }
if(isset($_POST['sfc2']))   { $sfc2   = $_POST['sfc2'];   }
if(isset($_POST['yr2']))    { $yr2    = $_POST['yr2'];    }
if(isset($_POST['mo2']))    { $mo2    = $_POST['mo2'];    }
if(isset($_POST['dy2']))    { $dy2    = $_POST['dy2'];    }
if(isset($_POST['cend2']))  { $cend2  = $_POST['cend2'];  }

echo "<script> var sat2   = '$sat2';   </script>";
echo "<script> var prod2  = '$prod2';  </script>";
echo "<script> var sfc2   = '$sfc2';   </script>";
echo "<script> var yr2    = '$yr2';    </script>";
echo "<script> var mo2    = '$mo2';    </script>";
echo "<script> var dy2    = '$dy2';    </script>";
echo "<script> var cend2  = '$cend2';  </script>";

?>


<style type="text/css">

  select.optionvisible   {visibility:visible}
  select.optioninvisible {visibility:hidden}

  td.arrowvisible 	{visibility:visible}
  td.arrowinvisible 	{visibility:hidden} 
  
</style>

<script language="javascript" type="text/javascript" src="validationtrmm2a12.js"></script>


</head>
<body  onLoad="loadInitialImages(sat,prod,sfc,yr,mo,dy,cend)">
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


				<FORM NAME=form action="" method="post">

				<table border="0" cellpadding="0" cellspacing="0" class="tableGrid" bgcolor="#eeeeee">

				<TR><TD align=center bgcolor="#eeeeee" height=18 colspan=2>
				<input  type=submit  value="Profiles"        onClick="changePanel(1)" title="Vertical Profiles" style="background-color: lightblue;"> &nbsp;&nbsp;
				<input  type=submit  value="Vertical Panels" onClick="changePanel(3)" title="Vertical layout"   style="background-color: lightblue;"> &nbsp;&nbsp;
				<B><font size=4>MIRS Comparison to TRMM_2A12</font></B> (<em>Click image for large view</em>)
				</TD></TR>

				<TR>
				<TD align=center colspan=2>

				&nbsp;
				<select name="sat" onChange="changeSat(this.value); loadImage_top();" title="Select a sensor">
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
				
				&nbsp;
				<select id="prod" name="prod" onChange="loadImage_top();" title="Select a product">
				<option value="clw"	>Cloud Liquid Water</option>
				<option value="iwp"	>Ice Water Path</option>
				<option value="rwp"	>Rain Water Path</option>
				<option value="lwp"	>Liquid Water Path</option>
				<option value="rr"	>Rain Rate</option>
				<option value="pixel"	>Collocated Pixels</option>
				</select>			  
				
				&nbsp;
				<select id="sfc" name="sfc" onChange="loadImage_top();" title="Select a surface type">
				<option value="sea">Sea</option>
				<option value="lnd">Land</option>
				<option value="all">All</option>
				</select>			  

				&nbsp;
				<select id="cend" name="cend" onChange="loadImage_top();" title="Select a passing mode">
				<option value="as">Asc</option>
				<option value="ds">Des</option>
				</select>			  

				&nbsp;
				<select id="yr" name="yr" onChange="loadImage_top();" title="Select a year">
<?php
require('includes/yearOptions.inc');
?>
				</select>			  
				
				&nbsp;
				<select id="mo" name="mo" onChange="loadImage_top();" title="Select a month">	  
<?php
require('includes/monthOptions.inc');
?>
				</select>			  
				
				&nbsp;
				<select id="dy" name="dy" onChange="loadImage_top();" title="Select a day">	  
<?php
// day options
require('includes/dayOptions.inc');
?>
				</select>			  

				&nbsp;Browse:
				<input type="button" onclick="rev();" value="<=" title="previous day">
				<input type="button" onclick="fwd();" value="=>" title="next day">
				
				</TD>
				</TR>


				<TR>
				  <TD align=center nowrap width=325 height=275>
				    <a id="href1" href="" target=_blank>
				    <img name="img1" src="" align=left  width=325 height=250 alt="all region" title="all surfaces"></a>
				  </TD>

				  <TD align=center nowrap width=325 height=275>
				    <a id="href2" href="" target=_blank>
				    <img name="img2" src="" align=right width=325 height=250 alt=""></a>
				  </TD>
				</TR>

				<TR>
				  <TD id="box3" align=center nowrap width=325 height=275>
				    <a id="href3" href="" target=_blank>
				    <img name="img3" src="" align=left  width=325 height=250 alt=""></a>
				  </TD>
				  
				  <TD id="box4" align=center nowrap width=325 height=275>
				    <a id="href4" href="" target=_blank>
				    <img name="img4" src="" align=right  width=325 height=250 alt=""></a>
				  </TD>

				</TR>


				<TR>
				<TD align=center colspan=2>
				&nbsp;
				<select name="sat2" onChange="changeSat(this.value); loadImage_bottom();"  title="Select a sensor">
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

				&nbsp;
				<select id="prod2" name="prod2" onChange="loadImage_bottom();" title="Select a product">
				<option value="clw"	>Cloud Liquid Water</option>
				<option value="iwp"	>Ice Water Path</option>
				<option value="rwp"	>Rain Water Path</option>
				<option value="rr"	>Rain Rate</option>
				<option value="pixel"	>Collocated Pixels</option>
				</select>			  
				
				&nbsp;
				<select id="sfc2" name="sfc2" onChange="loadImage_bottom();" title="Select a surface type">
				<option value="sea">Sea</option>
				<option value="lnd">Land</option>
				<option value="all">All</option>
				</select>
							  
				&nbsp;
				<select id="cend2" name="cend2" onChange="loadImage_bottom();" title="Select a passing mode">
				<option value="as">Asc</option>
				<option value="ds">Des</option>
				</select>			  

				&nbsp;
				<select id="yr2" name="yr2" onChange="loadImage_bottom();" title="Select a year">
<?php
require('includes/yearOptions.inc');
?>
				</select>			  
				
				&nbsp;
				<select id="mo2" name="mo2" onChange="loadImage_bottom();" title="Select a month">	  
<?php
require('includes/monthOptions.inc');
?>
				</select>			  
				
				&nbsp;
				<select id="dy2" name="dy2" onChange="loadImage_bottom();" title="Select a day">	  
<?php
require('includes/dayOptions.inc');
?>
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
