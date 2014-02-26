<?php include("password_protect.php"); ?>

<?php
// insert header that forbids caching and carries doctype
// and html tag;
require('includes/noCacheHeader.inc');
?>
<META name="verify-v1" content="CwbLBcFt9+GqRTgaLZsENmPnSWNB5MStHHdYsB7U2nI=">
<title>STAR - MIRS Project - Inter-Comparison</title>


<?php
// style links and the icon link
// pulls in .css files for regular and print display
require('includes/styleLinks.inc');
?>


<?php

$data1 = '';  
$sat1 = '';  
$prod1 = ''; 
$lay1 = '';
$cend1 = ''; 
$sfc1 = '';  
$yr1 = '';   
$mo1 = '';   
$dy1 = '';   

if(isset($_POST['data1']))  { $data1  = $_POST['data1'];  }
if(isset($_POST['sat1']))   { $sat1   = $_POST['sat1'];   }
if(isset($_POST['prod1']))  { $prod1  = $_POST['prod1'];  }
if(isset($_POST['lay1']))   { $lay1   = $_POST['lay1'];   }
if(isset($_POST['cend1']))  { $cend1  = $_POST['cend1'];  }
if(isset($_POST['sfc1']))   { $sfc1   = $_POST['sfc1'];   }
if(isset($_POST['yr1']))    { $yr1    = $_POST['yr1'];    }
if(isset($_POST['mo1']))    { $mo1    = $_POST['mo1'];    }
if(isset($_POST['dy1']))    { $dy1    = $_POST['dy1'];    }

echo "<script>\n";
echo " var data1  = '$data1';  \n";
echo " var sat1   = '$sat1';   \n";
echo " var prod1  = '$prod1';  \n";
echo " var lay1   = '$lay1';   \n";
echo " var cend1  = '$cend1';  \n";
echo " var sfc1   = '$sfc1';   \n";
echo " var yr1    = '$yr1';    \n";
echo " var mo1    = '$mo1';    \n";
echo " var dy1    = '$dy1';    \n";
echo "</script>\n";

$data2 = '';  
$sat2 = '';  
$prod2 = ''; 
$layer2 = '';
$cend2 = ''; 
$sfc2 = '';  
$yr2 = '';   
$mo2 = '';   
$dy2 = '';   

if(isset($_POST['data2']))  { $data2  = $_POST['data2'];  }
if(isset($_POST['sat2']))   { $sat2   = $_POST['sat2'];   }
if(isset($_POST['prod2']))  { $prod2  = $_POST['prod2'];  }
if(isset($_POST['lay2']))   { $lay2   = $_POST['lay2'];   }
if(isset($_POST['cend2']))  { $cend2  = $_POST['cend2'];  }
if(isset($_POST['sfc2']))   { $sfc2   = $_POST['sfc2'];   }
if(isset($_POST['yr2']))    { $yr2    = $_POST['yr2'];    }
if(isset($_POST['mo2']))    { $mo2    = $_POST['mo2'];    }
if(isset($_POST['dy2']))    { $dy2    = $_POST['dy2'];    }

echo "<script>\n";
echo " var data2  = '$data2';  \n";
echo " var sat2   = '$sat2';   \n";
echo " var prod2  = '$prod2';  \n";
echo " var lay2   = '$lay2';   \n";
echo " var cend2  = '$cend2';  \n";
echo " var sfc2   = '$sfc2';   \n";
echo " var yr2    = '$yr2';    \n";
echo " var mo2    = '$mo2';    \n";
echo " var dy2    = '$dy2';    \n";
echo "</script>\n";

?>


<style type="text/css">

  select.optionvisible   {visibility:visible}
  select.optioninvisible {visibility:hidden}

  td.arrowvisible 	{font-size: 85%; visibility:visible}
  td.arrowinvisible 	{font-size: 85%; visibility:hidden} 
  
  input.inputvisible {
        font-size: 85%; 
        visibility: visible;
  }

  input.inputinvisible {
        font-size: 85%; 
        visibility: hidden;
  }
  
</style>


<script language="javascript" type="text/javascript" src="inter.js"></script>

</head>
<body  onLoad="inter_init(data1,sat1,prod1,lay1,cend1,sfc1,yr1,mo1,dy1)">
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


				<FORM NAME=form action="interv.php" method="post">

				<table border="0" cellpadding="0" cellspacing="0" class="tableGrid" bgcolor="#eeeeee">

				<TR><TD align=center bgcolor="#eeeeee" height=18 colspan=2>
				<input  type=submit  value="Vertical Panel" style="background-color: lightblue;"
					title="vertical page format with larger images view"> &nbsp;&nbsp;

				<B><font size=4>MIRS Inter-Comparison Monitoring</font></B> (<em>Click image for large view</em>)
				</TD></TR>

				<TR>
				<TD align=center colspan=2>

				Mode:
				<select name="cend1" onChange="loadImage1();" title="Select a passing mode (Asc: ascending or Des: descending">
				<option value="as" >Asc</option>
				<option value="ds" >Des</option>
				</select>			  
				
				&nbsp;Sfc:
				<select name="sfc1" onChange="loadImage1();" title="Select a surface type">
				<option value="sea" >Sea</option>
				<option value="lnd" >Land</option>
				<option value="all" >All</option>
				</select>

				
				&nbsp;Year:
				<select id="yr1" name="yr1" onChange="loadImage1();" title="Select a year">
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
				<select id="mo1" name="mo1" onChange="loadImage1();" title="Select a month">	  
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
				<select id="dy1" name="dy1" onChange="loadImage1();" title="Select a day">	  
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
				<input type="button" class="inputvisible" onclick="rev();" value="<=" title="previous day">
				<input type="button" class="inputvisible" onclick="fwd();" value="=>" title="next day">
				
				<br>
				
				&nbsp;Data:
				<select id="data1" name="data1" onChange="changeData1( this.value ); loadImage1();" 
					title="Select a base data format">
				<option value="grid" title="Stats are based on gridded data">GRID</option>
				<option value="p2p"  title="Stats are based on point-to-point collocated data">P2P</option>
				</select>

				&nbsp;Pair:
				<select name="sat1" onChange="changeSat1( this.value ); loadImage1();" 
					title="Select a satellite pair to compare">
				<option value="n18_n19"	   >N18 vs N19</option>
				<option value="n18_metopA" >N18 vs MetopA</option>
				<option value="n18_metopB" >N18 vs MetopB</option>
				<option value="n19_metopA" >N19 vs MetopA</option>
				<option value="n19_metopB" >N19 vs MetopB</option>
				<option value="metopA_metopB" >metopA vs MetopB</option>
				<option value="n18_npp"	   >N18 vs ATMS</option>
				<option value="n19_npp"	   >N19 vs ATMS</option>
				<option value="metopA_npp" >MetopA vs ATMS</option>
				<option value="metopB_npp" >MetopB vs ATMS</option>
				<option value="f16_f18"	   >F16 vs F18</option>
				</select>

				&nbsp;Product:
				<select name="prod1" title="Select a product"
					onChange="changeProd1( this.value ); changeSize( this.value ); loadImage1();">
                                <option value="tpw">TPW</option> 
                                <option value="clw">CLW</option>
                                <option value="em">Emissivity</option>
                                <option value="gs">Snow Effective Grain Size</option>
                                <option value="iwp">Ice Water Path</option>
                                <option value="lwp">Liquid Water Path</option>
                                <option value="psfc">Surface Pressure</option>
                                <option value="rr">Rain Rate</option>
                                <option value="rwp">Rain Water Path</option>
                                <option value="angle">Scan Angle</option>
                                <option value="scanday">Scan Time</option>
                                <option value="sice">Sea Ice Concentration</option>
                                <option value="sicefy">First Year SIC</option>
                                <option value="sicemy">Multiple Year SIC</option>
                                <option value="snow">Snow Cover</option>
                                <option value="swe">Snow Water Equivalent</option>
                                <option value="tbc">Corr. TB</option>
                                <option value="tbu">UnCorr. TB</option>
                                <option value="temp">Temperature Profile</option>
                                <option value="tskin">Skin Temperature</option>
                                <option value="wv">Water Vapor Profile</option>
				</select>

				<select id="lay1" name="lay1" class="optioninvisible" onChange="loadImage1();">
				</select>
				&nbsp;
				<input id="prev1" name="prev1" class="inputinvisible" type="button" onClick="prevLay1();" title="next channel/layer" value="<=">
				<input id="next1" name="next1" class="inputinvisible" type="button" onClick="nextLay1();" title="prev channel/layer" value="=>">

				</TD>
				</TR>


				<TR>
				  <TD align=center nowrap width=325 height=275>
				    <a id="href1" href="" target=_blank>
				    <img name="img1" src="" align=center  width=325 height=250 alt="Sat1" title="Sat1 map"></a>
				  </TD>

				  <TD align=center nowrap width=325 height=275>

				    <a id="href2" href="" target=_blank>
				    <img name="img2" src="" align=center width=325 height=250 alt="Sat2" title="Sat2 map" ></a>
				  </TD>
				</TR>

				<TR>
				  <TD align=center nowrap width=325 height=275>
				    <a id="href3" href="" target=_blank>
				    <img name="img3" src="" align=center  width=325 height=250 alt="Map of (Sat1 - Sat2)"></a>

				  </TD>

				  <TD align=center nowrap width=325 height=275>
				    <a id="href4" href="" target=_blank>
				    <img name="img4" src="" align=center width=325 height=250 alt="asymmetry of difference"></a>
				  </TD>
				</TR>


				<TR>

				  <TD align=center nowrap width=325 height=275>
				    <a id="href5" href="" target=_blank>
				    <img name="img5" src="" align=center  width=325 height=250 alt="sea/land scattered plot"></a>
				  </TD>
				  
				  <TD align=center nowrap width=325 height=275>
				    <a id="href6" href="" target=_blank>
				    <img name="img6" src="" align=center  width=325 height=250 alt="ice/snow scattered plot"></a>
				  </TD>
				</TR>


				<TR>
				  <TD id="box7" align=center nowrap width=325 height=275>
				    <a id="href7" href="" target=_blank>
				    <img name="img7" src="" align=center width=325 height=250 alt="sea/land histogram of difference"></a>
				  </TD>
				  
				  <TD id="box8" align=center nowrap width=325 height=275>
				    <a id="href8" href="" target=_blank>
				    <img name="img8" src="" align=center width=325 height=250 alt="ice/snow histogram of difference"></a>

				  </TD>
				</TR>

				<TR>
				  <TD id="box9" align=center nowrap width=325 height=275>
				    <a id="href9" href="" target=_blank>
				    <img name="img9" src="" align=center width=325 height=250 alt=""></a>
				  </TD>
				  
				  <TD id="boxa" align=center nowrap width=325 height=275>

				    <a id="hrefa" href="" target=_blank>
				    <img name="imga" src="" align=center width=325 height=250 alt=""></a>
				  </TD>
				</TR>

				<TR>
				  <TD id="boxb" align=center nowrap width=325 height=275>
				    <a id="hrefb" href="" target=_blank>
				    <img name="imgb" src="" align=center width=325 height=250 alt=""></a>

				  </TD>
				  
				  <TD id="boxc" align=center nowrap width=325 height=275>
				    <a id="hrefc" href="" target=_blank>
				    <img name="imgc" src="" align=center width=325 height=250 alt=""></a>
				  </TD>
				</TR>


				<TR>
				<TD align=center colspan=2>

				Mode:
				<select name="cend2" onChange="loadImage2();" title="Select a passing mode (Asc: ascending or Des: descending">
				<option value="as" >Asc</option>
				<option value="ds" >Des</option>
				</select>			  
				
				&nbsp;Sfc:
				<select name="sfc2" onChange="loadImage2();" title="Select a surface type">
				<option value="sea" >Sea</option>
				<option value="lnd" >Land</option>
				<option value="all" >All</option>
				</select>
				
				&nbsp;Year:
				<select id="yr2" name="yr2" onChange="loadImage2();" title="Select a year">
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
				<select id="mo2" name="mo2" onChange="loadImage2();" title="Select a month">	  
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
				<select id="dy2" name="dy2" onChange="loadImage2();" title="previous day">	  
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
				<input type="button" class="inputvisible" onclick="rev();" value="<=" title="previous day">
				<input type="button" class="inputvisible" onclick="fwd();" value="=>" title="next day">
				
				<br>
				
				&nbsp;Data:
				<select id="data2" name="data2" onChange="changeData2( this.value ); loadImage2();" 
					title="Select a base data format">
				<option value="grid" title="Stats based on gridded data set">GRID</option>
				<option value="p2p"  title="Stats based on point-to-point collocated data set">P2P</option>
				</select>

				&nbsp;Pair:
				<select name="sat2" onChange="changeSat2( this.value ); loadImage2();" 
					title="Select a satellite pair to compare" >
				<option value="n18_n19"	   >N18 vs N19</option>
				<option value="n18_metopA" >N18 vs MetopA</option>
				<option value="n18_metopB" >N18 vs MetopB</option>
				<option value="n19_metopA" >N19 vs MetopA</option>
				<option value="n19_metopB" >N19 vs MetopB</option>
				<option value="metopA_metopB" >metopA vs MetopB</option>
				<option value="n18_npp"	   >N18 vs ATMS</option>
				<option value="n19_npp"	   >N19 vs ATMS</option>
				<option value="metopA_npp" >MetopA vs ATMS</option>
				<option value="metopB_npp" >MetopB vs ATMS</option>
				<option value="f16_f18"	   >F16 vs F18</option>
				</select>

				&nbsp;Product:
				<select name="prod2" title="Select a product" 
					onChange="changeProd2( this.value ); changeSize( this.value ); loadImage2();">
                                <option value="tpw">TPW</option> 
                                <option value="clw">CLW</option>
                                <option value="em">Emissivity</option>
                                <option value="gs">Snow Effective Grain Size</option>
                                <option value="iwp">Ice Water Path</option>
                                <option value="lwp">Liquid Water Path</option>
                                <option value="psfc">Surface Pressure</option>
                                <option value="rr">Rain Rate</option>
                                <option value="rwp">Rain Water Path</option>
                                <option value="scanday">Scan Time</option>
                                <option value="angle">Scan Angle</option>
                                <option value="sice">Sea Ice Concentration</option>
                                <option value="sicefy">First Year SIC</option>
                                <option value="sicemy">Multiple Year SIC</option>
                                <option value="snow">Snow Cover</option>
                                <option value="swe">Snow Water Equivalent</option>
                                <option value="tbc">Corr. TB</option>
                                <option value="tbu">UnCorr. TB</option>
                                <option value="temp">Temperature Profile</option>
                                <option value="tskin">Skin Temperature</option>
                                <option value="wv">Water Vapor Profile</option>
				</select>

				<select id="lay2" name="lay2" class="optioninvisible" onChange="loadImage2();">
				</select>

				&nbsp;
				<input id="prev2" name="prev2" class="inputinvisible" type="button" onclick="prevLay2();loadImage2();" title="next chan/lay" value="<=">
				<input id="next2" name="next2" class="inputinvisible" type="button" onclick="nextLay2();loadImage2();" title="prev chan/lay" value="=>">

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
