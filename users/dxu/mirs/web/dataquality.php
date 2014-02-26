<?php
// insert header that forbids caching and carries doctype
// and html tag;
require('includes/noCacheHeader.inc');
?>
<META name="verify-v1" content="CwbLBcFt9+GqRTgaLZsENmPnSWNB5MStHHdYsB7U2nI=">
<title>STAR - MIRS Project - Data Quality Monitoring</title>
<?php
// style links and the icon link
// pulls in .css files for regular and print display
require('includes/styleLinks.inc');
?>

<?php

$sat1='';  
$scale1='';  
$chan1=''; 
$yr1=''; 
$mo1=''; 
$dy1=''; 

if(isset($_POST['sat1']))   { $sat1   = $_POST['sat1'];   }
if(isset($_POST['scale1'])) { $scale1 = $_POST['scale1']; }
if(isset($_POST['chan1']))  { $chan1  = $_POST['chan1'];  }
if(isset($_POST['yr1']))    { $yr1    = $_POST['yr1'];    }
if(isset($_POST['mo1']))    { $mo1    = $_POST['mo1'];    }
if(isset($_POST['dy1']))    { $dy1    = $_POST['dy1'];    }

echo "<script> var sat1   = '$sat1';   </script>\n";
echo "<script> var scale1 = '$scale1'; </script>\n";
echo "<script> var chan1  = '$chan1';  </script>\n";
echo "<script> var yr1    = '$yr1';    </script>\n";
echo "<script> var mo1    = '$mo1';    </script>\n";
echo "<script> var dy1    = '$dy1';    </script>\n";


$sat2='';  
$scale2='';  
$chan2=''; 
$yr2=''; 
$mo2=''; 
$dy2=''; 

if(isset($_POST['sat2']))   { $sat2   = $_POST['sat2'];   }
if(isset($_POST['scale2'])) { $scale2 = $_POST['scale2']; }
if(isset($_POST['chan2']))  { $chan2  = $_POST['chan2'];  }
if(isset($_POST['yr2']))    { $yr2    = $_POST['yr2'];    }
if(isset($_POST['mo2']))    { $mo2    = $_POST['mo2'];    }
if(isset($_POST['dy2']))    { $dy2    = $_POST['dy2'];    }

echo "<script> var sat2   = '$sat2';   </script>\n";
echo "<script> var scale2 = '$scale2'; </script>\n";
echo "<script> var chan2  = '$chan2';  </script>\n";
echo "<script> var yr2    = '$yr2';    </script>\n";
echo "<script> var mo2    = '$mo2';    </script>\n";
echo "<script> var dy2    = '$dy2';    </script>\n";


$sat3='';  
$scale3='';  
$chan3=''; 
$yr3=''; 
$mo3=''; 
$dy3=''; 

if(isset($_POST['sat3']))   { $sat3   = $_POST['sat3'];   }
if(isset($_POST['scale3'])) { $scale3 = $_POST['scale3']; }
if(isset($_POST['chan3']))  { $chan3  = $_POST['chan3'];  }
if(isset($_POST['yr3']))    { $yr3    = $_POST['yr3'];    }
if(isset($_POST['mo3']))    { $mo3    = $_POST['mo3'];    }
if(isset($_POST['dy3']))    { $dy3    = $_POST['dy3'];    }

echo "<script> var sat3   = '$sat3';   </script>\n";
echo "<script> var scale3 = '$scale3'; </script>\n";
echo "<script> var chan3  = '$chan3';  </script>\n";
echo "<script> var yr3    = '$yr3';    </script>\n";
echo "<script> var mo3    = '$mo3';    </script>\n";
echo "<script> var dy3    = '$dy3';    </script>\n";


$sat4='';  
$scale4='';  
$chan4=''; 
$yr4=''; 
$mo4=''; 
$dy4=''; 

if(isset($_POST['sat4']))   { $sat4   = $_POST['sat4'];   }
if(isset($_POST['scale4'])) { $scale4 = $_POST['scale4']; }
if(isset($_POST['chan4']))  { $chan4  = $_POST['chan4'];  }
if(isset($_POST['yr4']))    { $yr4    = $_POST['yr4'];    }
if(isset($_POST['mo4']))    { $mo4    = $_POST['mo4'];    }
if(isset($_POST['dy4']))    { $dy4    = $_POST['dy4'];    }

echo "<script> var sat4   = '$sat4';   </script>\n";
echo "<script> var scale4 = '$scale4'; </script>\n";
echo "<script> var chan4  = '$chan4';  </script>\n";
echo "<script> var yr4    = '$yr4';    </script>\n";
echo "<script> var mo4    = '$mo4';    </script>\n";
echo "<script> var dy4    = '$dy4';    </script>\n";

?>

<style type="text/css">

  select.productSelect {font-size: 85%}
  td.productTd {font-size: 85%}
  input.productInput {font-size: 85%; background-color: #eeeeee}
  
  select.optionvisible {
        font-size: 85%; 
        visibility: visible;
  }

  select.optioninvisible {
        font-size: 85%; 
        visibility: hidden;
  }

  input.inputvisible {
        font-size: 85%; 
        visibility: visible;
  }

  input.inputinvisible {
        font-size: 85%; 
        visibility: hidden;
  }
  
</style>

<script language="javascript" type="text/javascript" src="dataquality.js"></script>

</head>


<body onLoad="init(sat1,scale1,chan1,yr1,mo1,dy1, sat2,scale2,chan2,yr2,mo2,dy2, sat3,scale3,chan3,yr3,mo3,dy3, sat4,scale4,chan4,yr4,mo4,dy4);">
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
				 <B><font size=4>MIRS Data Quality Monitoring</font></B>&nbsp;(click image for large view)
				</TD>
				</TR>


				<TR>
				
				<TD class="productTd" id="panel1" align=center valign=top width=325 height=250 align=center>
				<select class="productSelect" id="sat1" name="sat1" onChange="changeSat(this.value,1);loadImage(1);"
					title="select a satellite sensor">
				<option value="n18"    >NOAA-18</option>
				<option value="n19"    >NOAA-19</option>
				<option value="metopA" >METOP-A</option>
				<option value="metopB" >METOP-B</option>
				<option value="f16"    >F16/SSMIS</option>
				<option value="f18"    >F18/SSMIS</option>
				<option value="npp"    >NPP/ATMS</option>
				</select>
				
				&nbsp;
				<select class="productSelect" id="scale1" name="scale1" onChange="changeScale(this.value,1);loadImage(1);"
					title="select a time scale ( only NPP ATMS has daily one )">
				<option value="Life" >Sensor Life</option>
				<!--option value="daily">Daily</option-->
				</select>
				
				&nbsp; 
				<select class="productSelect" id="chan1" name="chan1" onChange="loadImage(1);"
				        title="select a parameter" >
				<option value="convrate">Convergence Rate</option>   
				<option value="qc0">QC=0</option>	
				<option value="qc1">QC=1</option>	
				<option value="qc2">QC=2</option>	
				</select>

				<br>
				
				<select class="optioninvisible" id="yr1" name="yr1" title="select a year"  onChange="loadImage(1);">
                                <option value="2011">2011</option> 
                                <option value="2012">2012</option> 
                                <option value="2013">2013</option> 
                                <option value="2014">2014</option> 
                                <option value="2016">2015</option> 
                                <option value="2015">2016</option> 
                                <option value="2017">2017</option> 
                                <option value="2018">2018</option> 
                                <option value="2019">2019</option> 
                                <option value="2020">2020</option> 
				</select>

				<select class="optioninvisible" id="mo1" name="mo1" title="select a month" onChange="loadImage(1);">
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

				<select class="optioninvisible" id="dy1" name="dy1" title="select a day" onChange="loadImage(1);">
				<option value="01">1</option>
                                <option value="02">2</option>
                                <option value="03">3</option>
                                <option value="04">4</option>
                                <option value="05">5</option>
                                <option value="06">6</option>
                                <option value="07">7</option>
                                <option value="08">8</option>
                                <option value="09">9</option>
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
				
				<input id="prev1" class="inputinvisible" type="button" onclick="prev(1);loadImage(1);" title="previous day image" value="<=">
				<input id="next1" class="inputinvisible" type="button" onclick="next(1);loadImage(1);" title="next day image" value="=>">

				<br>
				
				<a id="href1" href="" target="_blank" >
				<img id="img1" name="img1" src=""  alt=""  width=325 height=250 ></a>
				
				</TD>
				
				
				<TD class="productTd" id="panel2" align=center valign=top width=325 height=250 align=center>
				<select class="productSelect" id="sat2" name="sat2" onChange="changeSat(this.value,2);loadImage(2);"
					title="select a satellite sensor">
				<option value="n18"    >NOAA-18</option>
				<option value="n19"    >NOAA-19</option>
				<option value="metopA" >METOP-A</option>
				<option value="metopB" >METOP-B</option>
				<option value="f16"    >F16/SSMIS</option>
				<option value="f18"    >F18/SSMIS</option>
				<option value="npp"    >NPP/ATMS</option>
				</select>

				&nbsp;
				<select class="productSelect" id="scale2" name="scale2" onChange="changeScale(this.value,2);loadImage(2);"
					title="select a time scale ( only NPP ATMS has daily one )">
				<option value="Life" >Sensor Life</option>
				<!--option value="daily">Daily</option-->
				</select>
				
				&nbsp; 
				<select class="productSelect" id="chan2" name="chan2" onChange="loadImage(2);" 
					title="select a parameter">
				<option value="convrate">Convergence Rate</option>   
				<option value="qc0">QC=0</option>	
				<option value="qc1">QC=1</option>	
				<option value="qc2">QC=2</option>	
				</select>

				<br>
				
				<select  class="optioninvisible" id="yr2" name="yr2" title="select a year"  onChange="loadImage(2);">
                                <option value="2011">2011</option> 
                                <option value="2012">2012</option> 
                                <option value="2013">2013</option> 
                                <option value="2014">2014</option> 
                                <option value="2016">2015</option> 
                                <option value="2015">2016</option> 
                                <option value="2017">2017</option> 
                                <option value="2018">2018</option> 
                                <option value="2019">2019</option> 
                                <option value="2020">2020</option> 
				</select>

				<select  class="optioninvisible" id="mo2" name="mo2" title="select a month" onChange="loadImage(2);">
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

				<select  class="optioninvisible" id="dy2" name="dy2" title="select a day" onChange="loadImage(2);">
				<option value="01">1</option>
                                <option value="02">2</option>
                                <option value="03">3</option>
                                <option value="04">4</option>
                                <option value="05">5</option>
                                <option value="06">6</option>
                                <option value="07">7</option>
                                <option value="08">8</option>
                                <option value="09">9</option>
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
				
				<input id="prev2" class="inputinvisible" type="button" onclick="prev(2);loadImage(2);" title="previous day image" value="<=">
				<input id="next2" class="inputinvisible" type="button" onclick="next(2);loadImage(2);" title="next day image" value="=>">

				<br>
				
				<a id="href2" href="" target="_blank" >
				<img id="img2" name="img2" src=""  alt=""  width=325 height=250 ></a>
				
				</TD>
				
				</TR>


				<TR>
				
				<TD class="productTd" id="panel3" align=center valign=top width=325 height=250 align=center>
				<select class="productSelect" id="sat3" name="sat3" onChange="changeSat(this.value,3);loadImage(3);" 
					title="select a satellite sensor">
				<option value="n18"    >NOAA-18</option>
				<option value="n19"    >NOAA-19</option>
				<option value="metopA" >METOP-A</option>
				<option value="metopB" >METOP-B</option>
				<option value="f16"    >F16/SSMIS</option>
				<option value="f18"    >F18/SSMIS</option>
				<option value="npp"    >NPP/ATMS</option>
				</select>
				
				&nbsp;
				<select class="productSelect" id="scale3" name="scale3" onChange="changeScale(this.value,3);loadImage(3);"
					title="select a time scale ( only NPP ATMS has daily one )">
				<option value="Life" >Sensor Life</option>
				<!--option value="daily">Daily</option-->
				</select>
				
				&nbsp;
				<select class="productSelect" id="chan3" name="chan3" onChange="loadImage(3);"
					title="select a parameter">
				<option value="convrate">Convergence Rate</option>   
				<option value="qc0">QC=0</option>	
				<option value="qc1">QC=1</option>	
				<option value="qc2">QC=2</option>	
				</select>

				<br>
				
				<select  class="optioninvisible" id="yr3" name="yr3" title="select a year"  onChange="loadImage(3);">
                                <option value="2011">2011</option> 
                                <option value="2012">2012</option> 
                                <option value="2013">2013</option> 
                                <option value="2014">2014</option> 
                                <option value="2016">2015</option> 
                                <option value="2015">2016</option> 
                                <option value="2017">2017</option> 
                                <option value="2018">2018</option> 
                                <option value="2019">2019</option> 
                                <option value="2020">2020</option> 
				</select>

				<select  class="optioninvisible" id="mo3" name="mo3" title="select a month" onChange="loadImage(3);">
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

				<select  class="optioninvisible" id="dy3" name="dy3" title="select a day" onChange="loadImage(3);">
				<option value="01">1</option>
                                <option value="02">2</option>
                                <option value="03">3</option>
                                <option value="04">4</option>
                                <option value="05">5</option>
                                <option value="06">6</option>
                                <option value="07">7</option>
                                <option value="08">8</option>
                                <option value="09">9</option>
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
			        
				<input id="prev3" class="inputinvisible" type="button" onclick="prev(3);loadImage(3);" title="previous day image" value="<=">
				<input id="next3" class="inputinvisible" type="button" onclick="next(3);loadImage(3);" title="next day image" value="=>">

				<br>
				
				<a id="href3" href="" target="_blank" >
				<img id="img3" name="img3" src=""  alt="" width=325 height=250 ></a>
				
				</TD>
				
				
				<TD class="productTd" id="panel4" align=center valign=top width=325 height=250 align=center>
				<select class="productSelect" id="sat4" name="sat4" onChange="changeSat(this.value,4);loadImage(4);" 
					title="select a satellite sensor">
				<option value="n18"    >NOAA-18</option>
				<option value="n19"    >NOAA-19</option>
				<option value="metopA" >METOP-A</option>
				<option value="metopB" >METOP-B</option>
				<option value="f16"    >F16/SSMIS</option>
				<option value="f18"    >F18/SSMIS</option>
				<option value="npp"    >NPP/ATMS</option>
				</select>

				&nbsp;
				<select class="productSelect" id="scale4" name="scale4" onChange="changeScale(this.value,4);loadImage(4);" 
					title="select a time scale ( only NPP ATMS has daily one )">
				<option value="Life" >Sensor Life</option>
				<!--option value="daily">Daily</option-->
				</select>
				
				&nbsp;
				<select class="productSelect" id="chan4" name="chan4" onChange="loadImage(4);"
					title="select a parameter">
				<option value="convrate">Convergence Rate</option>   
				<option value="qc0">QC=0</option>	
				<option value="qc1">QC=1</option>	
				<option value="qc2">QC=2</option>	
				</select>

				<br>
				
				<select  class="optioninvisible" id="yr4" name="yr4" title="select a year"  onChange="loadImage(4);">
                                <option value="2011">2011</option> 
                                <option value="2012">2012</option> 
                                <option value="2013">2013</option> 
                                <option value="2014">2014</option> 
                                <option value="2016">2015</option> 
                                <option value="2015">2016</option> 
                                <option value="2017">2017</option> 
                                <option value="2018">2018</option> 
                                <option value="2019">2019</option> 
                                <option value="2020">2020</option> 
				</select>

				<select  class="optioninvisible" id="mo4" name="mo4" title="select a month" onChange="loadImage(4);">
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

				<select  class="optioninvisible" id="dy4" name="dy4" title="select a day" onChange="loadImage(4);">
				<option value="01">1</option>
                                <option value="02">2</option>
                                <option value="03">3</option>
                                <option value="04">4</option>
                                <option value="05">5</option>
                                <option value="06">6</option>
                                <option value="07">7</option>
                                <option value="08">8</option>
                                <option value="09">9</option>
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
				
				<input id="prev4" class="inputinvisible" type="button" onclick="prev(4);loadImage(4);" title="previous day image" value="<=">
				<input id="next4" class="inputinvisible" type="button" onclick="next(4);loadImage(4);" title="next day image" value="=>">

				<br>
				
				<a id="href4" href="" target="_blank" >
				<img id="img4" name="img4" src=""  alt="" width=325 height=250 ></a>
				
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
