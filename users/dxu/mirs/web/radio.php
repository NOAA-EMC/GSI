<?php include("password_protect.php"); ?>

<?php
// insert header that forbids caching and carries doctype
// and html tag;
require('includes/noCacheHeader.inc');
?>

<META name="verify-v1" content="CwbLBcFt9+GqRTgaLZsENmPnSWNB5MStHHdYsB7U2nI=">
<title>STAR - MIRS Project - Radiometric Monitoring </title>
<?php
// style links and the icon link
// pulls in .css files for regular and print display
require('includes/styleLinks.inc');
?>

<?php

$ref='';  
$sat='';  
$prod=''; 
$layer='';
$cend=''; 
$sfc='';  
$yr='';   
$mo='';   
$dy='';   

if(isset($_POST['ref']))   { $ref   = $_POST['ref'];   }
if(isset($_POST['sat']))   { $sat   = $_POST['sat'];   }
if(isset($_POST['prod']))  { $prod  = $_POST['prod'];  }
if(isset($_POST['layer'])) { $layer = $_POST['layer']; }
if(isset($_POST['cend']))  { $cend  = $_POST['cend'];  }
if(isset($_POST['sfc']))   { $sfc   = $_POST['sfc'];   }
if(isset($_POST['yr']))    { $yr    = $_POST['yr'];    }
if(isset($_POST['mo']))    { $mo    = $_POST['mo'];    }
if(isset($_POST['dy']))    { $dy    = $_POST['dy'];    }

echo "<script> var ref   = '$ref';   </script>";
echo "<script> var sat   = '$sat';   </script>";
echo "<script> var prod  = '$prod';  </script>";
echo "<script> var layer = '$layer'; </script>";
echo "<script> var cend  = '$cend';  </script>";
echo "<script> var sfc   = '$sfc';   </script>";
echo "<script> var yr    = '$yr';    </script>";
echo "<script> var mo    = '$mo';    </script>";
echo "<script> var dy    = '$dy';    </script>";

?>



<script language="JavaScript" type="text/javascript" src="radio.js"> </script>


</head>
<body onLoad="loadInitialImages(ref,sat,prod,layer,cend,sfc,yr,mo,dy)">
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
				
				<FORM NAME=form action="radiov.php" method="post">

				<table border="0" cellpadding="0" cellspacing="0" class="tableGrid" bgcolor="#eeeeee">
				
				<TR>
				<TD align=center bgcolor="#eeeeee" height=18 colspan=2>
				<input  type=submit  value="8 X 1 Panel" style="background-color: lightblue" > &nbsp;&nbsp;
				<B><font size=4>MIRS Radiometric Performance Monitoring </font></B>
				</TD>
				</TR>


				<TR>
				<TD align=center nowrap colspan=2>

				Ref:
				<select name="ref" onChange="changeRef( this.value ); loadImage();" title="Select a reference NWP data set">
				<option value="gdas"  >GDAS</option>
				<option value="ecmwf" >ECMWF</option>
				</select>			  
				
				&nbsp;Sensor:
				<select name="sat" onChange="changeSensor( this.value ); loadImage();" title="Select a sensor (TRMM TMI has 5 day delay)">
				<option value="n18"    >NOAA-18</option>
				<option value="n19"    >NOAA-19</option>
				<option value="metopA" >METOP-A</option>
				<option value="metopB" >METOP-B</option>
				<option value="f16"    >F16/SSMIS</option>
				<option value="f18"    >F18/SSMIS</option>
				<option value="npp"    >NPP/ATMS</option>
				<option value="gcomw1" >GCOMW1/AMSR2</option>
                                <option value="trmm"  title="TRMM/TMI data has 2 more days delay relative to other sensors" >TRMM/TMI</option>
				<option value="mtma"  title="MT-MADRAS is based on simulated proxy data for testing purposes only" >MT/MADRAS</option>
				<option value="mtsa"  title="MT-SAPHIR is based on simulated proxy data for testing purposes only" >MT/SAPHIR</option>
				</select>

				&nbsp;Product:
				<select name="prod" onChange="changeProduct( this.value );loadImage();">
				<option value="tb"     >TB</option>  
				<!--option value="tpw" >TPW</option> 
				<option value="tskin"  >Skin Temperature</option>
				<option value="temp"   >Temperature Profile </option>
				<option value="wv"     >Water Vapor Profile</option>  
				<option value="em"     >Emissivity</option>   
				<option value="rwp"    >Rain Water Path</option> 
				<option value="clw"    >CLW</option>
				<option value="iwp"    >Ice Water Path</option> 
				<option value="clwp"   >Cloud Liquid Water Profile</option> 
				<option value="ip"     >Ice Profile</option>
				<option value="gp"     >Graupel Profile</option> 
				<option value="sp"     >Snow Profile</option> 
				<option value="rp"     >Rain Profile</option>  -->   
				</select>

				<select id="layer" name="layer" class="optionvisible" onChange="loadImage();" title="Select a channel">
				<option value="23v">ch1:23v</option>
				<option value="31v">ch2:31v</option>
				<option value="50v">ch3:50v</option>
				<option value="52v">ch4:52v</option>
				<option value="53h">ch5:53h</option>
				<option value="54h">ch6:54h</option>
				<option value="54v">ch7:54v</option>
				<option value="55h">ch8:55h</option>
				<option value="57h1">ch9:57h1</option>
				<option value="57h2">ch10:57h2</option>
				<option value="57h3">ch11:57h3</option>
				<option value="57h4">ch12:57h4</option>
				<option value="57h5">ch13:57h5</option>
				<option value="57h6">ch14:57h6</option>
				<option value="89v1">ch15:89v1</option>
				<option value="89v2">ch16:89v2</option>
				<option value="157h">ch17:157h</option>
				<option value="184h">ch18:184h</option>
				<option value="186h">ch19:186h</option>
				<option value="190h">ch20:190h</option>
				</select>

                                <input type="button" onclick="prev(document.form.layer);" value="<=" title="next channel">
                                <input type="button" onclick="next(document.form.layer);" value="=>" title="previous channel">
				
				<br />
				
				Mode:
				<select name="cend" onChange="loadImage();">
				<option value="as" >Asc</option>
				<option value="ds" >Des</option>
				</select>			  
				
				&nbsp;Sfc:
				<select name="sfc" onChange="loadImage();">
				<option value="sea" >Sea</option>
				<option value="lnd" >Land</option>
				<option value="all" >All</option>
				</select>			  
				
				<!--
				<select name="region">
				<option value="glb" >Globe</option>
				<option value="us"  >US</option>
				<option value="na"  >North America</option>
				<option value="eu"  >Europe</option>
				<option value="af"  >North Africa</option>
				<option value="as"  >Asia</option>
				</select-->			  

				&nbsp;Year:
				<select id="yr" name="yr" onChange="loadImage();">
				<option value="2011">2011</option> 
				<option value="2012">2012</option> 
				<option value="2013">2013</option>
				<option value="2014">2014</option> 
				<option value="2015">2015</option> 
				<option value="2016">2016</option> 
				<option value="2017">2017</option> 
				<option value="2018">2018</option> 
				<option value="2019">2019</option> 
				<option value="2020">2020</option>  
				</select>
							  
				Month:
				<select id="mo" name="mo" onChange="loadImage();">	  
				<option value="01">Jan</option>
				<option value="02">Feb</option>
				<option value="03">Mar</option>
				<option value="04">Apr</option>
				<option value="05">May</option>
				<option value="06">Jun</option>
				<option value="07">Jul</option>
				<option value="08">Aug</option>
				<option value="09">Sep</option>
				<option value="10">Oct</option>
				<option value="11">Nov</option>
				<option value="12">Dec</option>
				</select>			  
				
				Day:
				<select id="dy" name="dy" onChange="loadImage();">	  
				<option value="01">1 </option>   
				<option value="02">2 </option>   
				<option value="03">3 </option>   
				<option value="04">4 </option>   
				<option value="05">5 </option>   
				<option value="06">6 </option>   
				<option value="07">7 </option>   
				<option value="08">8 </option>   
				<option value="09">9 </option>   
				<option value="10">10</option>  	  
				<option value="11">11</option>  	  
				<option value="12">12</option>  	  
				<option value="13">13</option>  	  
				<option value="14">14</option>  	  
				<option value="15">15</option>  	  
				<option value="16">16</option>  	  
				<option value="17">17</option>  	  
				<option value="18">18</option>  	  
				<option value="19">19</option>  	  
				<option value="20">20</option>  	  
				<option value="21">21</option>  	  
				<option value="22">22</option>  	  
				<option value="23">23</option>  	  
				<option value="24">24</option>  	  
				<option value="25">25</option>  	  
				<option value="26">26</option>  	  
				<option value="27">27</option>  	  
				<option value="28">28</option>  	  
				<option value="29">29</option>  	 
				<option value="30">30</option>  	 
				<option value="31">31</option>  	 
				</select>			  

				&nbsp;Browse:
				<input type="button" onclick="rev();" value="<=" title="next day">
				<input type="button" onclick="fwd();" value="=>" title="previous day">
				</TD>
				</TR>


				<TR>
				  <TD align=center nowrap width=325 height=250 >
				    <a id="href1" href="" target=_blank>
				    <img name="img1" src="" width=325 height=250 alt=""></a>
				  </TD>
				  <TD align=center nowrap width=325 height=250 >
				    <a id="href2" href="" target=_blank>
				    <img name="img2" src="" width=325 height=250 alt=""></a>
				  </TD>
				</TR>

				<TR>
				  <TD align=center nowrap width=325 height=250 >
				    <a id="href3" href="" target=_blank>
				    <img name="img3" src="" width=325 height=250 alt=""></a>
				  </TD>
				  <TD align=center nowrap width=325 height=250 >
				    <a id="href4" href="" target=_blank>
				    <img name="img4" src="" width=325 height=250 alt=""></a>
				  </TD>
				</TR>

				<TR>
				  <TD align=center nowrap width=325 height=250 >
				    <a id="href5" href="" target=_blank>
				    <img name="img5" src="" width=325 height=250 alt=""></a>
				  </TD>
				  <TD align=center nowrap width=325 height=250 >
				    <a id="href6" href="" target=_blank>
				    <img name="img6" src="" width=325 height=250 alt=""></a>
				  </TD>
				</TR>

				<TR>
				  <TD align=center nowrap width=325 height=250 >
				    <a id="href7" href="" target=_blank>
				    <img name="img7" src="" width=325 height=250 alt="Chi Square"></a>
				  </TD>
				  <TD align=center nowrap width=325 height=250 >
				    <a id="href8" href="" target=_blank>
				    <img name="img8" src="" width=325 height=250 alt=""></a>
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
