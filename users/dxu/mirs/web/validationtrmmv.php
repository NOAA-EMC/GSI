<?php include("password_protect.php"); ?>

<?php
// insert header that forbids caching and carries doctype
// and html tag;
require('includes/noCacheHeader.inc');
?>

<META name="verify-v1" content="CwbLBcFt9+GqRTgaLZsENmPnSWNB5MStHHdYsB7U2nI=">
<title>STAR - MIRS Project - Comparison to TRMM</title>
<?php
// style links and the icon link
// pulls in .css files for regular and print display
require('includes/styleLinks.inc');
?>

<?php

$sat='';  
$sfc='';
$yr='';   
$mo='';   
$dy='';   

if(isset($_POST['sat']))   { $sat   = $_POST['sat'];   }
if(isset($_POST['sfc']))   { $sfc   = $_POST['sfc'];   }
if(isset($_POST['yr']))    { $yr    = $_POST['yr'];    }
if(isset($_POST['mo']))    { $mo    = $_POST['mo'];    }
if(isset($_POST['dy']))    { $dy    = $_POST['dy'];    }

echo "<script> var sat   = '$sat';   </script>";
echo "<script> var sfc   = '$sfc';   </script>";
echo "<script> var yr    = '$yr';    </script>";
echo "<script> var mo    = '$mo';    </script>";
echo "<script> var dy    = '$dy';    </script>";

$sat2='';  
$sfc2='';
$yr2='';   
$mo2='';   
$dy2='';   

if(isset($_POST['sat2']))   { $sat2   = $_POST['sat2'];   }
if(isset($_POST['sfc2']))   { $sfc2   = $_POST['sfc2'];   }
if(isset($_POST['yr2']))    { $yr2    = $_POST['yr2'];    }
if(isset($_POST['mo2']))    { $mo2    = $_POST['mo2'];    }
if(isset($_POST['dy2']))    { $dy2    = $_POST['dy2'];    }

echo "<script> var sat2   = '$sat2';   </script>";
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

<script language="javascript" type="text/javascript" src="validationtrmm.js"></script>


</head>
<body  onLoad="loadInitialImagesv(sat,sfc,yr,mo,dy)">
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
			require('includes/Sample_NavDiv_validationtrmmv.inc');
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


				<FORM NAME=form action="validationtrmm.php" method="post">

				<table border="0" cellpadding="0" cellspacing="0" class="tableGrid" bgcolor="#eeeeee">

				<TR><TD align=center bgcolor="#eeeeee" height=18>
				<input  type=submit  value="2 X 2 Panels" style="background-color: lightblue" > &nbsp;&nbsp;
				<B><font size=4>MIRS Comparison to TRMM</font></B> (<em>Click image for large view</em>)
				</TD></TR>

				<TR>
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
				</select>

				&nbsp;Area:
				<select id="sfc" name="sfc" onChange="loadImage_top();">
				<option value="sea">Sea</option>
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
				  <TD id="box4" align=center nowrap width=650 height=500>
				    <a id="href4" href="" target=_blank>
				    <img name="img4" src="" align=center width=650 height=500 alt=""></a>
				  </TD>
				</TR>


				<TR>
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
				</select>

				&nbsp;Area:
				<select id="sfc2" name="sfc2" onChange="loadImage_bottom();">
				<option value="sea">Sea</option>
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
// day options
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
