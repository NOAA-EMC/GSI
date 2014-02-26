<?php
// insert header that forbids caching and carries doctype
// and html tag;
require('includes/noCacheHeader.inc');
?>
<META name="verify-v1" content="CwbLBcFt9+GqRTgaLZsENmPnSWNB5MStHHdYsB7U2nI=">
<title>STAR - Microwave Integrated Retrieval System (MIRS) - QC Monitoring - Data Quality </title>
<?php
// style links and the icon link
// pulls in .css files for regular and print display
require('includes/styleLinks.inc');
?>


<style type="text/css">

.box {
width:150px;
text-align:right;/*move the text to the right*/
}

.box th {
border-left:10px solid #fff; /*same as page colour*/
border-bottom:10px solid #000; /*black, bottom, border*/
font-size:18px;
color:#000; /*black text*/}  
</style>

<script language="javascript" type="text/javascript" src="qcconvg.js"></script>

</head>
<body onLoad="loadInit()">
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
			require('includes/Sample_NavDiv_qcconvg.inc');
			?>
		</td>
		<td class="mainPanel"><a name="skipTarget"></a><?php require('includes/noScriptWarning.inc'); ?>
			<div class="padding">
				<!-- DO NOT DELETE OR ALTER CODE ABOVE THIS COMMENT -->
				<!-- EXCEPT for the contents of the <title></title> TAG!! -->
				<!-- You can start project specific content HERE -->

			<left>
			<h1>MIRS QC Monitoring - Data Quality Table</h1>
			
			<FORM NAME=form method=POST>
			
				 <input class="productInput" type=submit value="2 X 2 Plots" title="2 by 2 panel of images"
				 	onClick="plot2x2();" style="background-color: lightblue" >&nbsp;&nbsp;
				 
				 <input class="productInput" type=submit  value="4 X 1 Plots" title="4 by 1 panel of images"
				 	onClick="plot4x1();" style="background-color: lightblue" >&nbsp;&nbsp;
			
			
				<select id="yr" name="yr" title="Select a year" onChange="loadTable();">
                                <option value="2008">2008</option> 
                                <option value="2009">2009</option> 
                                <option value="2010">2010</option> 
                                <option value="2011">2011</option> 
                                <option value="2012">2012</option> 
                                <option value="2013">2012</option> 
                                <option value="2014">2012</option> 
				</select>			  
				
				<select id="mo" name="mo" title="Select a month" onChange="loadTable();">	  
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
				
				<select id="dy" name="dy" title="Select a day" onChange="loadTable();">	  
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
			
			<input type="button" onclick="rev();" value="<" title="previous day">
			<input type="button" onclick="fwd();" value=">" title="next day">
			
			</left>
			<br />
			
			<div id="convg_table">
			</div>

			</FORM>

				<!-- END your project-specific content HERE -->
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
