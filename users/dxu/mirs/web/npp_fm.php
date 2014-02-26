<?php
// insert header that forbids caching and carries doctype
// and html tag;
require('includes/noCacheHeader.inc');
?>
<META name="verify-v1" content="CwbLBcFt9+GqRTgaLZsENmPnSWNB5MStHHdYsB7U2nI=">
<title>STAR - MIRS Project - Climate Time Series Monitoring</title>
<?php
// style links and the icon link
// pulls in .css files for regular and print display
require('includes/styleLinks.inc');
?>

<style type="text/css">
  select.optionvisible          {font-size: 100%; visibility:visible}
  select.optioninvisible        {font-size: 100%; visibility:hidden}

  select.productSelect {font-size: 100%}
  td.productTd {font-size: 100%}
  input.productInput {font-size: 100%; background-color: #eeeeee}
  
</style>

<script language="javascript" type="text/javascript" src="npp_fm.js"></script>

</head>


<body onLoad="loadImage()">
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
				
				
				<FORM NAME=form action="climate_timeseriesv.php" method="post">

				<table border="0" cellpadding="0" cellspacing="0" class="tableGrid" bgcolor="#eeeeee">

				<TR align=center>
				<TD align=center nowrap class="productTd" colspan=2>
				 <!--input class="productInput" type=submit  value="4 X 1 Panel"-->&nbsp;&nbsp;
				 <B><font size=4> MIRS NPP ATMS Measured TB ( 2700 granules )<font color=red size=4></font></font></B>
				 
				</TD>
				</TR>


				<TR>
				
				<TD class="productTd" id="panel1" align=center valign=top width=675 height=550 align=center>

				Scan Mode:
				<select class="productSelect" id="cend1" name="cend1" onChange="changeCend(this.value)">
				<option value="as" >Asc</option>
				<option value="ds" >Des</option>
				</select>
				
				&nbsp;
				Channel:
                                <select id="chan1" name="chan1" class="optionvisible" onChange="changeChan(this.value)">
				<option value="23v">ch1: 23v</option>
				<option value="31v">ch2: 31v</option>
				<option value="50h">ch3: 50h</option>
				<option value="51h">ch4: 51h</option>
				<option value="52h">ch5: 52h</option>
				<option value="53h">ch6: 53h</option>
				<option value="54h1">ch7: 54h1</option>
				<option value="54h2">ch8: 54h2</option>
				<option value="55h">ch9: 55h</option>
				
				<option value="57h1">ch10: 57h1</option>
				<option value="57h2">ch11: 57h2</option>
				<option value="57h3">ch12: 57h3</option>
				<option value="57h4">ch13: 57h4</option>
				<option value="57h5">ch14: 57h5</option>
				<option value="57h6">ch15: 57h6</option>
				
				<option value="88v">ch16: 88v</option>
				<option value="165h">ch17: 165h</option>
				
				<option value="183h1">ch18: 183h1</option>
				<option value="183h2">ch19: 183h2</option>
				<option value="183h3">ch20: 183h3</option>
				<option value="183h4">ch21: 183h4</option>
				<option value="183h5">ch22: 183h5</option>
                                </select>
				
				<br>
				<br>
				
				<a id="href1" href="" target="_blank" >
				<img name="img1" src="" alt=""  width=650 height=500 style="display:block; clear:both;" ></a>
				
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
