<?php include("pass.php"); ?>

<?php
// insert header that forbids caching and carries doctype
// and html tag;
require('includes/noCacheHeader.inc');
?>
<META name="verify-v1" content="CwbLBcFt9+GqRTgaLZsENmPnSWNB5MStHHdYsB7U2nI=">
<title>STAR - MIRS Project - NEDT Monitoring</title>
<?php
// style links and the icon link
// pulls in .css files for regular and print display
require('includes/styleLinks.inc');
?>

<?php

$sat1='';  
$scale1='';  
$chan1=''; 

if(isset($_POST['scale1'])) { $scale1 = $_POST['scale1'];   }
if(isset($_POST['sat1']))   { $sat1   = $_POST['sat1'];   }
if(isset($_POST['chan1']))  { $chan1  = $_POST['chan1'];  }

echo "<script> var scale1 = '$scale1'; </script>";
echo "<script> var sat1   = '$sat1';   </script>";
echo "<script> var chan1  = '$chan1';  </script>";


$sat2='';  
$scale2='';  
$chan2=''; 

if(isset($_POST['scale2'])) { $scale2 = $_POST['scale2'];   }
if(isset($_POST['sat2']))   { $sat2   = $_POST['sat2'];   }
if(isset($_POST['chan2']))  { $chan2  = $_POST['chan2'];  }

echo "<script> var scale2 = '$scale2'; </script>";
echo "<script> var sat2   = '$sat2';   </script>";
echo "<script> var chan2  = '$chan2';  </script>";


$sat3='';  
$scale3='';  
$chan3=''; 

if(isset($_POST['scale3'])) { $scale3 = $_POST['scale3'];   }
if(isset($_POST['sat3']))   { $sat3   = $_POST['sat3'];   }
if(isset($_POST['chan3']))  { $chan3  = $_POST['chan3'];  }

echo "<script> var scale3 = '$scale3'; </script>";
echo "<script> var sat3   = '$sat3';   </script>";
echo "<script> var chan3  = '$chan3';  </script>";


$sat4='';  
$scale4='';  
$chan4=''; 

if(isset($_POST['scale4'])) { $scale4 = $_POST['scale4'];   }
if(isset($_POST['sat4']))   { $sat4   = $_POST['sat4'];   }
if(isset($_POST['chan4']))  { $chan4  = $_POST['chan4'];  }

echo "<script> var scale4 = '$scale4'; </script>";
echo "<script> var sat4   = '$sat4';   </script>";
echo "<script> var chan4  = '$chan4';  </script>";

?>

<style type="text/css">

  select.productSelect {font-size: 85%}
  td.productTd {font-size: 85%}
  input.productInput {font-size: 85%; background-color: #eeeeee}
  
</style>

<script language="javascript" type="text/javascript" src="nedt.js"></script>

</head>


<body onLoad="loadInitialImage(scale1,sat1,chan1, scale2,sat2,chan2, scale3,sat3,chan3, scale4,sat4,chan4)">
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
				
				
				<FORM name=form action="" method="post">

				<table border="0" cellpadding="0" cellspacing="0" class="tableGrid" bgcolor="#eeeeee">

				<TR align=center>
				<TD align=center nowrap class="productTd" colspan=2>
				 <input class="productInput" type=submit value="Table Format" title="Table Format"
				 	onClick="tableFormat();" style="background-color: lightblue" >&nbsp;&nbsp;
				 <input class="productInput" type=submit  value="4 X 1 Panel" title="4 row by 1 column page view"
				 	onClick="verticalView();" style="background-color: lightblue" >&nbsp;&nbsp;
				 <B><font size=4>MIRS NEDT Monitoring</font></B>&nbsp;(click image for large view)
				</TD>
				</TR>


				<TR>
				
				<TD class="productTd" id="panel1" align=center valign=top width=325 height=250 align=center>
				
				<select class="productSelect" id="scale1" name="scale1" onChange="changeScale(this.value);loadImage()" title="select a time scale">
				<option value="" >Sensor Life</option>
				<option value="2">30-day</option>
				</select>
				&nbsp;
				
				<select class="productSelect" id="sat1" name="sat1" title="select a sensor" onChange="changeSensor(this.value,document.form.chan1);loadImage()" >
				<option value="n18"    >NOAA-18</option>
				<option value="metopA" >METOP-A</option>
				<option value="n19"    >NOAA-19</option>
				<option value="npp"    >NPP ATMS</option>
				</select>

				&nbsp;
				<select  class="productSelect" id="chan1" name="chan1" onChange="loadImage()" title="select a channel">
				<option value="1">1:23v</option>   
				<option value="2">2:31v</option>   
				<option value="3">3:50v</option>   
				<option value="4">4:52v</option>   
				<option value="5">5:53h</option>   
				<option value="6">6:54h</option>   
				<option value="7">7:54v</option>   
				<option value="8">8:55h</option>   
				<option value="9">9:57h1</option>   
				<option value="10">10:57h2</option>  	  
				<option value="11">11:57h3</option>  	  
				<option value="12">12:57h4</option>  	  
				<option value="13">13:57h5</option>  	  
				<option value="14">14:57h6</option>  	  
				<option value="15">15:89v1</option>  	  
				<option value="16">16:89v2</option>  	  
				<option value="17">17:157h</option>  	  
				<option value="18">18:184h</option>  	  
				<option value="19">19:186h</option>  	  
				<option value="20">20:190h</option>  	  
				</select>
				
			        <input type="button" class="productInput" onclick="rev(document.form.sat1.value,document.form.chan1);loadImage();" value="<=" title="previous channel">
                                <input type="button" class="productInput" onclick="fwd(document.form.sat1.value,document.form.chan1);loadImage();" value="=>" title="next channel">

				<br>
				
				<a id="href1" href="" target="_blank" >
				<img name="img1" src=""  alt=""  width=325 height=250 ></a>
				
				</TD>
				
				
				<TD class="productTd" id="panel2" align=center valign=top width=325 height=250 align=center>
				
				<select class="productSelect" id="scale2" name="scale2" onChange="changeScale(this.value);loadImage()" title="select a time scale">
				<option value="" >Sensor Life</option>
				<option value="2">30-day</option>
				</select>
				&nbsp;
				
				<select class="productSelect" id="sat2" name="sat2" title="select a sensor" onChange="changeSensor(this.value,document.form.chan2);loadImage()" >
				<option value="n18"    >NOAA-18</option>
				<option value="metopA" >METOP-A</option>
				<option value="n19"    >NOAA-19</option>
				<option value="npp"    >NPP ATMS</option>
				</select>

				&nbsp;
				<select class="productSelect" id="chan2" name="chan2" onChange="loadImage()" title="select a channel">
				<option value="1">1:23v</option>   
				<option value="2">2:31v</option>   
				<option value="3">3:50v</option>   
				<option value="4">4:52v</option>   
				<option value="5">5:53h</option>   
				<option value="6">6:54h</option>   
				<option value="7">7:54v</option>   
				<option value="8">8:55h</option>   
				<option value="9">9:57h1</option>   
				<option value="10">10:57h2</option>  	  
				<option value="11">11:57h3</option>  	  
				<option value="12">12:57h4</option>  	  
				<option value="13">13:57h5</option>  	  
				<option value="14">14:57h6</option>  	  
				<option value="15">15:89v1</option>  	  
				<option value="16">16:89v2</option>  	  
				<option value="17">17:157h</option>  	  
				<option value="18">18:184h</option>  	  
				<option value="19">19:186h</option>  	  
				<option value="20">20:190h</option>  	  
				</select>

			        <input type="button" class="productInput" onclick="rev(document.form.sat2.value,document.form.chan2);loadImage();" value="<=" title="previous channel">
                                <input type="button" class="productInput" onclick="fwd(document.form.sat2.value,document.form.chan2);loadImage();" value="=>" title="next channel">
				
				<br>
				
				<a id="href2" href="" target="_blank" >
				<img name="img2" src=""  alt=""  width=325 height=250 ></a>
				
				</TD>
				
				</TR>


				<TR>
				
				<TD class="productTd" id="panel3" align=center valign=top width=325 height=250 align=center>
				
				<select class="productSelect" id="scale3" name="scale3" onChange="changeScale(this.value);loadImage()" title="select a time scale">
				<option value="" >Sensor Life</option>
				<option value="2">30-day</option>
				</select>
				&nbsp;
				
				<select class="productSelect" id="sat3" name="sat3" title="select a sensor" onChange="changeSensor(this.value,document.form.chan3);loadImage()" >
				<option value="n18"    >NOAA-18</option>
				<option value="metopA" >METOP-A</option>
				<option value="n19"    >NOAA-19</option>
				<option value="npp"    >NPP ATMS</option>
				</select>

				&nbsp;
				<select class="productSelect" id="chan3" name="chan3" onChange="loadImage()" title="select a channel">
				<option value="1">1:23v</option>   
				<option value="2">2:31v</option>   
				<option value="3">3:50v</option>   
				<option value="4">4:52v</option>   
				<option value="5">5:53h</option>   
				<option value="6">6:54h</option>   
				<option value="7">7:54v</option>   
				<option value="8">8:55h</option>   
				<option value="9">9:57h1</option>   
				<option value="10">10:57h2</option>  	  
				<option value="11">11:57h3</option>  	  
				<option value="12">12:57h4</option>  	  
				<option value="13">13:57h5</option>  	  
				<option value="14">14:57h6</option>  	  
				<option value="15">15:89v1</option>  	  
				<option value="16">16:89v2</option>  	  
				<option value="17">17:157h</option>  	  
				<option value="18">18:184h</option>  	  
				<option value="19">19:186h</option>  	  
				<option value="20">20:190h</option>  	  
				</select>

			        <input type="button" class="productInput" onclick="rev(document.form.sat3.value,document.form.chan3);loadImage();" value="<=" title="previous channel">
                                <input type="button" class="productInput" onclick="fwd(document.form.sat3.value,document.form.chan3);loadImage();" value="=>" title="next channel">

				<br>
				
				<a id="href3" href="" target="_blank" >
				<img name="img3" src=""  alt="" width=325 height=250 ></a>
				
				</TD>
				
				
				<TD class="productTd" id="panel4" align=center valign=top width=325 height=250 align=center>
				
				<select class="productSelect" id="scale4" name="scale4" onChange="changeScale(this.value);loadImage()" title="select a time scale">
				<option value="" >Sensor Life</option>
				<option value="2">30-day</option>
				</select>
				&nbsp;
				
				<select class="productSelect" id="sat4" name="sat4" title="select a sensor" onChange="changeSensor(this.value,document.form.chan4);loadImage()" >
				<option value="n18" 	>NOAA-18</option>
				<option value="metopA"	>METOP-A</option>
				<option value="n19"     >NOAA-19</option>
				<option value="npp"     >NPP ATMS</option>
				</select>

				&nbsp;
				<select  class="productSelect" id="chan4" name="chan4" onChange="loadImage()" title="select a channel">
				<option value="1">1:23v</option>   
				<option value="2">2:31v</option>   
				<option value="3">3:50v</option>   
				<option value="4">4:52v</option>   
				<option value="5">5:53h</option>   
				<option value="6">6:54h</option>   
				<option value="7">7:54v</option>   
				<option value="8">8:55h</option>   
				<option value="9">9:57h1</option>   
				<option value="10">10:57h2</option>  	  
				<option value="11">11:57h3</option>  	  
				<option value="12">12:57h4</option>  	  
				<option value="13">13:57h5</option>  	  
				<option value="14">14:57h6</option>  	  
				<option value="15">15:89v1</option>  	  
				<option value="16">16:89v2</option>  	  
				<option value="17">17:157h</option>  	  
				<option value="18">18:184h</option>  	  
				<option value="19">19:186h</option>  	  
				<option value="20">20:190h</option>  	  
				</select>
                               
			        <input type="button" class="productInput" onclick="rev(document.form.sat4.value,document.form.chan4);loadImage();" value="<=" title="previous channel">
                                <input type="button" class="productInput" onclick="fwd(document.form.sat4.value,document.form.chan4);loadImage();" value="=>" title="next channel">
				
				<br>
				
				<a id="href4" href="" target="_blank" >
				<img name="img4" src=""  alt="" width=325 height=250 ></a>
				
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
