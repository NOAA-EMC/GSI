<?php
// insert header that forbids caching and carries doctype
// and html tag;
require('includes/noCacheHeader.inc');
?>
<META name="verify-v1" content="CwbLBcFt9+GqRTgaLZsENmPnSWNB5MStHHdYsB7U2nI=">
<title>STAR - MIRS Project - Footprint Matching Monitoring</title>
<?php
// style links and the icon link
// pulls in .css files for regular and print display
require('includes/styleLinks.inc');
?>


<style type="text/css">

  select.productSelect {font-size: 85%}
  td.productTd {font-size: 85%}
  input.productInput {font-size: 85%; background-color: #eeeeee}
  
</style>

<script language="javascript" type="text/javascript" src="fm.js"></script>

</head>


<body onLoad="loadInitialImages()">
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
				<TD align=center nowrap class="productTd">
				 <!--input class="productInput" type=submit  value="2 X 2 Panel"-->
				 <B><font size=4>MIRS Footprint Matching Monitoring</font></B>
				</TD>
				</TR>


				<TR>
				<TD class="productTd" id="panel1" align=center valign=top width=650 height=500 align=center>Sensor
				<select class="productSelect" id="sat1" name="sat1" onchange="loadImage1()" >
				<option value="n18" 	>NOAA-18</option>
				<option value="n19"     >NOAA-19</option>
				<option value="metopA"	>METOP-A</option>
				<option value="metopB"	>METOP-B</option>
				</select>

				&nbsp; Orbit
				<select  class="productSelect" id="cend1" name="cend1" onChange="loadImage1()">
				<option value="as" >Asc</option>
				<option value="ds" >Des</option>
				</select>
				
				&nbsp; Param
				<select class="productSelect"  id="param1" name="param1" onChange="loadImage1()">
				<option value="delta" >Delta</option>
				<option value="mean" >Mean</option>
				<option value="stdv" >Standard Deviation</option>				
				</select>
				
				<br>
				
				Year:
				<select class="productSelect" id="yr1" name="yr1" onChange="loadImage1()">
				<option value="2011" >2011</option>
				<option value="2012" >2012</option>
				<option value="2013" >2013</option>
				<option value="2014" >2014</option>
				<option value="2015" >2015</option>
				<option value="2016" >2016</option>
				<option value="2017" >2017</option>
				<option value="2018" >2018</option>
				<option value="2019" >2019</option>
				<option value="2020" >2020</option> 
				</select>		          
				
			       Month:
				<select class="productSelect" id="mo1" name="mo1" onChange="loadImage1()">	  
				<option value="01" >Jan</option>
				<option value="02" >Feb</option>
				<option value="03" >Mar</option>
				<option value="04" >Apr</option>
				<option value="05" >May</option>
				<option value="06" >Jun</option>
				<option value="07" >Jul</option>
				<option value="08" >Aug</option>
				<option value="09" >Sep</option>
				<option value="10" >Oct</option>
				<option value="11" >Nov</option>
				<option value="12" >Dec</option>
				</select>		          
				
				Day:
				<select class="productSelect" id="dy1" name="dy1" onChange="loadImage1()">	  
				<option value="01" >1 </option>   
				<option value="02" >2 </option>   
				<option value="03" >3 </option>   
				<option value="04" >4 </option>   
				<option value="05" >5 </option>   
				<option value="06" >6 </option>   
				<option value="07" >7 </option>   
				<option value="08" >8 </option>   
				<option value="09" >9 </option>   
				<option value="10" >10</option> 	   
				<option value="11" >11</option> 	   
				<option value="12" >12</option> 	   
				<option value="13" >13</option> 	   
				<option value="14" >14</option> 	   
				<option value="15" >15</option> 	   
				<option value="16" >16</option> 	   
				<option value="17" >17</option> 	   
				<option value="18" >18</option> 	   
				<option value="19" >19</option> 	   
				<option value="20" >20</option> 	   
				<option value="21" >21</option> 	   
				<option value="22" >22</option> 	   
				<option value="23" >23</option> 	   
				<option value="24" >24</option> 	   
				<option value="25" >25</option> 	   
				<option value="26" >26</option> 	   
				<option value="27" >27</option> 	   
				<option value="28" >28</option> 	   
				<option value="29" >29</option> 	   
				<option value="30" >30</option> 	   
				<option value="31" >31</option> 	   
				</select>

				&nbsp;Browse:
				<input class="productInput" type="button" onclick="rev(document.form.yr1,document.form.mo1,document.form.dy1,1);" value="<=">
				<input class="productInput" type="button" onclick="fwd(document.form.yr1,document.form.mo1,document.form.dy1,1);" value="=>">

				<br>
							  
				<a id="href1" href="" target=_blank >
				<img name="img1" src=""  alt=""  width=640 height=480 ></a>
				
				</TD>
				</TR>


				<TR>
				<TD class="productTd" id="panel2" align=center valign=top width=650 height=500 align=center>Sensor
				<select class="productSelect" id="sat2" name="sat2" onChange="loadImage2()" >
				<option value="n18" 	>NOAA-18</option>
				<option value="n19"     >NOAA-19</option>
				<option value="metopA"	>METOP-A</option>
				<option value="metopB"	>METOP-B</option>
				</select>

				&nbsp; Orbit
				<select  class="productSelect" id="cend2" name="cend2" onChange="loadImage2()">
				<option value="as" >Asc</option>
				<option value="ds" >Des</option>
				</select>
				
				&nbsp; Param
				<select class="productSelect"  id="param2" name="param2" onChange="loadImage2()">
				<option value="delta" >Delta</option>
				<option value="mean" >Mean</option>
				<option value="stdv" >Standard Deviation</option>				
				</select>
				
				<br>
				
				Year:
				<select class="productSelect" id="yr2" name="yr2" onChange="loadImage2()">
				<option value="2011" >2011</option>
				<option value="2012" >2012</option>
				<option value="2013" >2013</option>
				<option value="2014" >2014</option>
				<option value="2015" >2015</option>
				<option value="2016" >2016</option>
				<option value="2017" >2017</option>
				<option value="2018" >2018</option>
				<option value="2019" >2019</option>
				<option value="2020" >2020</option> 
				</select>		          
				
				Month:
				<select class="productSelect" id="mo2" name="mo2" onChange="loadImage2()">	  
				<option value="01" >Jan</option>
				<option value="02" >Feb</option>
				<option value="03" >Mar</option>
				<option value="04" >Apr</option>
				<option value="05" >May</option>
				<option value="06" >Jun</option>
				<option value="07" >Jul</option>
				<option value="08" >Aug</option>
				<option value="09" >Sep</option>
				<option value="10" >Oct</option>
				<option value="11" >Nov</option>
				<option value="12" >Dec</option>
				</select>		          
				
				Day:
				<select class="productSelect" id="dy2" name="dy2" onChange="loadImage2()">	  
				<option value="01" >1 </option>   
				<option value="02" >2 </option>   
				<option value="03" >3 </option>   
				<option value="04" >4 </option>   
				<option value="05" >5 </option>   
				<option value="06" >6 </option>   
				<option value="07" >7 </option>   
				<option value="08" >8 </option>   
				<option value="09" >9 </option>   
				<option value="10" >10</option> 	   
				<option value="11" >11</option> 	   
				<option value="12" >12</option> 	   
				<option value="13" >13</option> 	   
				<option value="14" >14</option> 	   
				<option value="15" >15</option> 	   
				<option value="16" >16</option> 	   
				<option value="17" >17</option> 	   
				<option value="18" >18</option> 	   
				<option value="19" >19</option> 	   
				<option value="20" >20</option> 	   
				<option value="21" >21</option> 	   
				<option value="22" >22</option> 	   
				<option value="23" >23</option> 	   
				<option value="24" >24</option> 	   
				<option value="25" >25</option> 	   
				<option value="26" >26</option> 	   
				<option value="27" >27</option> 	   
				<option value="28" >28</option> 	   
				<option value="29" >29</option> 	   
				<option value="30" >30</option> 	   
				<option value="31" >31</option> 	   
				</select>

				&nbsp;Browse:
				<input class="productInput" type="button" onclick="rev(document.form.yr2,document.form.mo2,document.form.dy2,2);" value="<=">
				<input class="productInput" type="button" onclick="fwd(document.form.yr2,document.form.mo2,document.form.dy2,2);" value="=>">

				<br>
							  
				<a id="href2" href="" target=_blank >
				<img name="img2" src=""  alt=""  width=640 height=480 ></a>
				
				</TD>
				</TR>
				

				<TR>
				<TD class="productTd" id="panel3" align=center valign=top width=650 height=500 align=center>Sensor
				<select class="productSelect" id="sat3" name="sat3" onChange="loadImage3()" >
				<option value="n18" 	>NOAA-18</option>
				<option value="n19"     >NOAA-19</option>
				<option value="metopA"	>METOP-A</option>
				<option value="metopB"	>METOP-B</option>
				</select>

				&nbsp; Orbit
				<select  class="productSelect" id="cend3" name="cend3" onChange="loadImage3()">
				<option value="as" >Asc</option></option>
				<option value="ds" >Des</option></option>
				</select>
				
				&nbsp; Param
				<select class="productSelect" id="param3" name="param3" onChange="loadImage3()">
				<option value="delta" >Delta</option>
				<option value="mean" >Mean</option>
				<option value="stdv" >Standard Deviation</option>				
				</select>
				
				<br>
				
				Year:
				<select class="productSelect" id="yr3" name="yr3" onChange="loadImage3()">
				<option value="2011" >2011</option>
				<option value="2012" >2012</option>
				<option value="2013" >2013</option>
				<option value="2014" >2014</option>
				<option value="2015" >2015</option>
				<option value="2016" >2016</option>
				<option value="2017" >2017</option>
				<option value="2018" >2018</option>
				<option value="2019" >2019</option>
				<option value="2020" >2020</option> 
				</select>			  

				Month:
				<select class="productSelect" id="mo3" name="mo3" onchange="loadImage3()">	  
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
				<select class="productSelect" id="dy3" name="dy3" onchange="loadImage3()">	  
				<option value="01" >1 </option>   
				<option value="02" >2 </option>   
				<option value="03" >3 </option>   
				<option value="04" >4 </option>   
				<option value="05" >5 </option>   
				<option value="06" >6 </option>   
				<option value="07" >7 </option>   
				<option value="08" >8 </option>   
				<option value="09" >9 </option>   
				<option value="10" >10</option> 	   
				<option value="11" >11</option> 	   
				<option value="12" >12</option> 	   
				<option value="13" >13</option> 	   
				<option value="14" >14</option> 	   
				<option value="15" >15</option> 	   
				<option value="16" >16</option> 	   
				<option value="17" >17</option> 	   
				<option value="18" >18</option> 	   
				<option value="19" >19</option> 	   
				<option value="20" >20</option> 	   
				<option value="21" >21</option> 	   
				<option value="22" >22</option> 	   
				<option value="23" >23</option> 	   
				<option value="24" >24</option> 	   
				<option value="25" >25</option> 	   
				<option value="26" >26</option> 	   
				<option value="27" >27</option> 	   
				<option value="28" >28</option> 	   
				<option value="29" >29</option> 	   
				<option value="30" >30</option> 	   
				<option value="31" >31</option> 	   
				</select>

				&nbsp;Browse:
				<input class="productInput" type="button" onclick="rev(document.form.yr3,document.form.mo3,document.form.dy3,3);" value="<=">
				<input class="productInput" type="button" onclick="fwd(document.form.yr3,document.form.mo3,document.form.dy3,3);" value="=>">

				<br>
							  
				<a id="href3" href="" target=_blank >
				<img name="img3" src=""  alt=""  width=640 height=480 ></a>
				
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
