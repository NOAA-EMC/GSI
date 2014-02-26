<?php
// insert header that forbids caching and carries doctype
// and html tag;
require('includes/noCacheHeader.inc');
?>
<META name="verify-v1" content="CwbLBcFt9+GqRTgaLZsENmPnSWNB5MStHHdYsB7U2nI=">
<title>STAR - MIRS Project - Performancd Time Series</title>
<?php
// style links and the icon link
// pulls in .css files for regular and print display
require('includes/styleLinks.inc');
?>

<?php

$sat1='';
$prod1='';
$param1='';  
$sfc1=''; 
$nwp1=''; 

if(isset($_POST['sat1']))   { $sat1   = $_POST['sat1'];   }
if(isset($_POST['prod1']))  { $prod1  = $_POST['prod1'];  }
if(isset($_POST['param1'])) { $param1 = $_POST['param1']; }
if(isset($_POST['sfc1']))   { $sfc1   = $_POST['sfc1'];   }
if(isset($_POST['nwp1']))   { $nwp1   = $_POST['nwp1'];   }

echo "<script> var sat1   = '$sat1';   </script>";
echo "<script> var prod1  = '$prod1';  </script>";
echo "<script> var param1 = '$param1'; </script>";
echo "<script> var sfc1   = '$sfc1';   </script>";
echo "<script> var nwp1   = '$nwp1';   </script>";


$sat2='';  
$prod2='';
$param2='';  
$sfc2=''; 
$nwp2=''; 

if(isset($_POST['sat2']))   { $sat2   = $_POST['sat2'];   }
if(isset($_POST['prod2']))  { $prod2  = $_POST['prod2'];  }
if(isset($_POST['param2'])) { $param2 = $_POST['param2']; }
if(isset($_POST['sfc2']))   { $sfc2  = $_POST['sfc2'];    }
if(isset($_POST['nwp2']))   { $nwp2   = $_POST['nwp2'];   }

echo "<script> var sat2   = '$sat2';   </script>";
echo "<script> var prod2  = '$prod2';  </script>";
echo "<script> var param2 = '$param2'; </script>";
echo "<script> var sfc2   = '$sfc2';   </script>";
echo "<script> var nwp2   = '$nwp2';   </script>";


$sat3='';  
$prod3='';
$param3='';  
$sfc3=''; 
$nwp3=''; 

if(isset($_POST['sat3']))   { $sat3   = $_POST['sat3'];   }
if(isset($_POST['prod3']))  { $prod3  = $_POST['prod3'];  }
if(isset($_POST['param3'])) { $param3 = $_POST['param3']; }
if(isset($_POST['sfc3']))   { $sfc3   = $_POST['sfc3'];   }
if(isset($_POST['nwp3']))   { $nwp3   = $_POST['nwp3'];   }

echo "<script> var sat3   = '$sat3';   </script>";
echo "<script> var prod3  = '$prod3';  </script>";
echo "<script> var param3 = '$param3'; </script>";
echo "<script> var sfc3   = '$sfc3';   </script>";
echo "<script> var nwp3   = '$nwp3';   </script>";


$sat4='';  
$prod4='';
$param4='';  
$sfc4=''; 
$nwp4=''; 

if(isset($_POST['sat4']))   { $sat4   = $_POST['sat4'];   }
if(isset($_POST['prod4']))  { $prod4  = $_POST['prod4'];  }
if(isset($_POST['param4'])) { $param4 = $_POST['param4']; }
if(isset($_POST['sfc4']))   { $sfc4   = $_POST['sfc4'];   }
if(isset($_POST['nwp4']))   { $nwp4   = $_POST['nwp4'];   }

echo "<script> var sat4   = '$sat4';   </script>";
echo "<script> var prod4  = '$prod4';  </script>";
echo "<script> var param4 = '$param4'; </script>";
echo "<script> var sfc4   = '$sfc4';   </script>";
echo "<script> var nwp4   = '$nwp4';   </script>";

?>

<style type="text/css">

  select.productSelect {font-size: 85%}
  td.productTd {font-size: 85%}
  input.productInput {font-size: 85%; background-color: #eeeeee}
  
</style>

<script language="javascript" type="text/javascript" src="geobias.js"></script>

</head>


<body onLoad="loadInitialImageSmall(sat1,prod1,param1,sfc1,nwp1, sat2,prod2,param2,sfc2,nwp2, sat3,prod3,param3,sfc3,nwp3, sat4,prod4,param4,sfc4,nwp4)">
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
				
				
				<FORM NAME=form action="geobiasv.php" method="post">

				<table border="0" cellpadding="0" cellspacing="0" class="tableGrid" bgcolor="#eeeeee">

				<TR align=center>
				<TD align=center nowrap class="productTd" colspan=2>
				 <input class="productInput" type=submit  value="4 X 1 Panel" 
				 	title="click this button will switch to 4 row by 1 column page view with larger images"
				 	style="background-color: lightblue;">&nbsp;&nbsp;
				 <B><font size=4>MIRS Performance Time Series</font></B> (<em>Click image for large view</em>)
				</TD>
				</TR>


				<TR>
				
				<TD class="productTd" id="panel1" align=center valign=top width=325 height=250 align=center>Sat
				<select class="productSelect" id="sat1" name="sat1" onChange="loadImageSmall()" >
				<option value="n18"    >NOAA-18</option>
				<option value="n19"    >NOAA-19</option>
				<option value="metopA" >METOP-A</option>
				<option value="metopB" >METOP-B</option>
				<option value="f16"    >F16/SSMIS</option>
				<option value="f18"    >F18/SSMIS</option>
				<option value="npp"    >NPP/ATMS</option>
				</select>

				&nbsp;Prod
				<select class="productSelect"  id="prod1" name="prod1" onChange="loadImageSmall()">
				<option value="temp" >Temperature</option>
				<option value="wv"   >Water Vapor</option>
				</select>
				
				&nbsp;Param
				<select class="productSelect"  id="param1" name="param1" onChange="loadImageSmall()">
				<option value="biasmean" >Bias Mean</option>
				<option value="biasstdv" >Bias Stdv</option>
				</select>
				
				<br>Reference Data
				<select  class="productSelect" id="nwp1" name="nwp1" onChange="loadImageSmall()">
				<option value="gdas">GDAS</option>   
				<option value="ecmwf">ECMWF</option>   
				</select>
				
				&nbsp;Surface
				<select  class="productSelect" id="sfc1" name="sfc1" onChange="loadImageSmall()">
				<option value="sea">Sea</option>   
				<option value="lnd">Land</option>   
				<option value="ice">Ice</option>   
				<option value="snw">Snow</option>   
				</select>
				
				<input type="button" class="productInput" onclick="rev(document.form.sfc1);loadImageSmall()" value="<=" title="previous surface type">
				<input type="button" class="productInput" onclick="fwd(document.form.sfc1);loadImageSmall()" value="=>" title="next surface type">
				
				<br>
				
				<a id="href1" href="" target="_blank" >
				<img name="img1" src="" alt=""  width=325 height=250 ></a>
				
				</TD>
				
				
				<TD class="productTd" id="panel2" align=center valign=top width=325 height=250 align=center>Sat
				<select class="productSelect" id="sat2" name="sat2" onChange="loadImageSmall()" >
				<option value="n18"    >NOAA-18</option>
				<option value="n19"    >NOAA-19</option>
				<option value="metopA" >METOP-A</option>
				<option value="metopB" >METOP-B</option>
				<option value="f16"    >F16/SSMIS</option>
				<option value="f18"    >F18/SSMIS</option>
				<option value="npp"    >NPP/ATMS</option>
				</select>

				&nbsp;Prod
				<select class="productSelect"  id="prod2" name="prod2" onChange="loadImageSmall()">
				<option value="temp" >Temperature</option>
				<option value="wv"   >Water Vapor</option>
				</select>
				
				&nbsp;Param
				<select class="productSelect" id="param2" name="param2" onChange="loadImageSmall()">
				<option value="biasmean" >Bias Mean</option>
				<option value="biasstdv" >Bias Stdv</option>
				</select>
				
				<br>Reference Data
				<select  class="productSelect" id="nwp2" name="nwp2" onChange="loadImageSmall()">
				<option value="gdas">GDAS</option>   
				<option value="ecmwf">ECMWF</option>   
				</select>
				
				&nbsp;Surface
				<select class="productSelect" id="sfc2" name="sfc2" onChange="loadImageSmall()">
				<option value="sea">Sea</option>   
				<option value="lnd">Land</option>   
				<option value="ice">Ice</option>   
				<option value="snw">Snow</option>   
				</select>

				<input type="button" class="productInput" onclick="rev(document.form.sfc2);loadImageSmall()" value="<=" title="previous surface type">
				<input type="button" class="productInput" onclick="fwd(document.form.sfc2);loadImageSmall()" value="=>" title="next surface type">

				<br>
				
				<a id="href2" href="" target="_blank" >
				<img name="img2" src="" alt=""  width=325 height=250 ></a>
				
				</TD>
				
				</TR>



				<TR>
				
				<TD class="productTd" id="panel3" align=center valign=top width=325 height=250 align=center>Sat
				<select class="productSelect" id="sat3" name="sat3" onChange="loadImageSmall()" >
				<option value="n18"    >NOAA-18</option>
				<option value="n19"    >NOAA-19</option>
				<option value="metopA" >METOP-A</option>
				<option value="metopB" >METOP-B</option>
				<option value="f16"    >F16/SSMIS</option>
				<option value="f18"    >F18/SSMIS</option>
				<option value="npp"    >NPP/ATMS</option>
				</select>
				
				&nbsp;Prod
				<select class="productSelect"  id="prod3" name="prod3" onChange="loadImageSmall()">
				<option value="temp" >Temperature</option>
				<option value="wv"   >Water Vapor</option>
				</select>
				
				&nbsp;Param
				<select class="productSelect" id="param3" name="param3" onChange="loadImageSmall()">
				<option value="biasmean" >Bias Mean</option>
				<option value="biasstdv" >Bias Stdv</option>
				</select>

				<br>Reference Data
				<select  class="productSelect" id="nwp3" name="nwp3" onChange="loadImageSmall()">
				<option value="gdas">GDAS</option>   
				<option value="ecmwf">ECMWF</option>   
				</select>
				
				&nbsp;Surface
				<select class="productSelect" id="sfc3" name="sfc3" onChange="loadImageSmall()">
				<option value="sea">Sea</option>   
				<option value="lnd">Land</option>   
				<option value="ice">Ice</option>   
				<option value="snw">Snow</option>   
				</select>
				
				<input type="button" class="productInput" onclick="rev(document.form.sfc3);loadImageSmall()" value="<=" title="previous surface type">
				<input type="button" class="productInput" onclick="fwd(document.form.sfc3);loadImageSmall()" value="=>" title="next surface type">
				
				<br>
				
				<a id="href3" href="" target="_blank" >
				<img name="img3" src="" alt="" width=325 height=250 ></a>
				
				</TD>
				
				
				<TD class="productTd" id="panel4" align=center valign=top width=325 height=250 align=center>Sat
				<select class="productSelect" id="sat4" name="sat4" onChange="loadImageSmall()" >
				<option value="n18"    >NOAA-18</option>
				<option value="n19"    >NOAA-19</option>
				<option value="metopA" >METOP-A</option>
				<option value="metopB" >METOP-B</option>
				<option value="f16"    >F16/SSMIS</option>
				<option value="f18"    >F18/SSMIS</option>
				<option value="npp"    >NPP/ATMS</option>
				</select>

				&nbsp;Prod
				<select class="productSelect"  id="prod4" name="prod4" onChange="loadImageSmall()">
				<option value="temp" >Temperature</option>
				<option value="wv"   >Water Vapor</option>
				</select>
				
				&nbsp;Param
				<select class="productSelect" id="param4" name="param4" onChange="loadImageSmall()">
				<option value="biasmean" >Bias Mean</option>
				<option value="biasstdv" >Bias Stdv</option>
				</select>
				
				<br>Reference Data
				<select  class="productSelect" id="nwp4" name="nwp4" onChange="loadImageSmall()">
				<option value="gdas">GDAS</option>   
				<option value="ecmwf">ECMWF</option>   
				</select>
				
				&nbsp;Surface
				<select  class="productSelect" id="sfc4" name="sfc4" onChange="loadImageSmall()">
				<option value="sea">Sea</option>   
				<option value="lnd">Land</option>   
				<option value="ice">Ice</option>   
				<option value="snw">Snow</option>   
				</select>
				
				<input type="button" class="productInput" onclick="rev(document.form.sfc4);loadImageSmall()" value="<=" title="previous surface type">
				<input type="button" class="productInput" onclick="fwd(document.form.sfc4);loadImageSmall()" value="=>" title="next surface type">
				<br>
				
				<a id="href4" href="" target="_blank" >
				<img name="img4" src="" alt="" width=325 height=250 ></a>
				
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
