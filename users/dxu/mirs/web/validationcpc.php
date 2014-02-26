<?php include("password_protect.php"); ?>

<?php
// insert header that forbids caching and carries doctype
// and html tag;
require('includes/noCacheHeader.inc');
?>

<META name="verify-v1" content="CwbLBcFt9+GqRTgaLZsENmPnSWNB5MStHHdYsB7U2nI=">
<title>STAR - MIRS Project - Comparison to CPC</title>
<?php
// style links and the icon link
// pulls in .css files for regular and print display
require('includes/styleLinks.inc');
?>

<style type="text/css">

  select.optionvisible   {visibility:visible}
  select.optioninvisible {visibility:hidden}

  td.arrowvisible 	{visibility:visible}
  td.arrowinvisible 	{visibility:hidden} 
  
</style>

<script language="javascript" type="text/javascript" src="validationcpc.js"></script>

</head>
<body  onLoad="loadInitialImages()">
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
			require('includes/Sample_NavDiv_validationcpc.inc');
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

				<TR><TD align=center bgcolor="#eeeeee" height=10 colspan=2>
				<input  type=submit  value="Time Series"    title="Go to Time Series Page" onClick="changePanel(1)" style="background-color: lightblue;"> &nbsp;&nbsp;
				<input  type=submit  value="Vertical Panel" title="4 Row large view panel" onClick="changePanel(3)" style="background-color: lightblue;"> &nbsp;&nbsp;
				<B><font size=4>MIRS Comparison to CPC</font></B> (<em>Click image for large view</em>)

				<br>
				Sat/Composite:
				<select class="productSelect" id="comp" name="comp" onChange="loadImages()">
				<option value="n18"     >NOAA-18</option>
				<option value="n19"     >NOAA-19</option>
				<option value="metopA"  >METOP-A</option>
				<option value="metopB"  >METOP-B</option>
				<option value="npp"     >NPP/ATMS</option>
				<option value="gcomw1"	>GCOMW1/AMSR2</option>
				<option value="f16"     >F16/SSMIS</option>
				<option value="f18"     >F18/SSMIS</option>
				<option value="n18_metopA_f16"     >N18_MetopA_F16</option>
				<option value="npp_metopA_f16"     >NPP_MetopA_F16</option>
				<option value="n18_metopA_f16_npp" >N18_MetopA_F16_NPP</option>
				</select>		          
				
				&nbsp;
				Year:
				<select class="productSelect" id="yr" name="yr" onChange="loadImages()">
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
				
			       	&nbsp;Month:
				<select class="productSelect" id="mo" name="mo" onChange="loadImages()">	  
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
				
				&nbsp;Day:
				<select class="productSelect" id="dy" name="dy" onChange="loadImages()">	  
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
				<input class="productInput" type="button" onclick="rev(document.form.yr,document.form.mo,document.form.dy);" value="<=">
				<input class="productInput" type="button" onclick="fwd(document.form.yr,document.form.mo,document.form.dy);" value="=>">

				</TD></TR>


				<TR>
				  <TD align=center nowrap width=325 height=275>
				    <a id="href1" href="" target=_blank>
				    <img name="img1" src="" 
				    align=left  width=325 height=250 alt="CPC Map" title="CPC Map"></a>
				  </TD>

				  <TD align=center nowrap width=325 height=275>
				    <a id="href2" href="" target=_blank>
				    <img name="img2" src=""
				    align=right width=325 height=250 alt=""></a>
				  </TD>
				</TR>

				<TR>
				  <TD align=center nowrap width=325 height=275>
				    <a id="href3" href="" target=_blank>
				    <img name="img3" src="" 
				    align=left  width=325 height=250 alt=""></a>
				  </TD>
				  
				  <TD align=center nowrap width=325 height=275>
				    <a id="href4" href="" target=_blank>
				    <img name="img4" src=""
				    align=right  width=325 height=250 alt=""></a>
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
