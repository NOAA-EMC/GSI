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

			<h1><center>MIRS QC Monitoring - Data Quality Table</center></h1>



<table border="0" cellpadding="0" cellspacing="0" class="tableGrid">
<tr align="center"><th rowspan="2" colspan="2">Satellite</th> <th colspan="24">Orbit Number of 2012-01-01 ( Stats are percentage rate )</th></tr>

<tr align="center">
<th>1</th>
<th>2</th>
<th>3</th>
<th>4</th>
<th>5</th>
<th>6</th>
<th>7</th>
<th>8</th>
<th>9</th>
<th>10</th>
<th>11</th>
<th>12</th>
<th>13</th>
<th>14</th>
<th>15</th>
<th>16</th>
<th>17</th>
<th>18</th>
<th>19</th>
<th>20</th>
<th>21</th>
<th>22</th>
<th>23</th>
<th>24</th>
</tr>

<tr align=center><th rowspan="4">N18</th>
<th>QC=0(>30%)</th><td bgcolor="#77e7ae" title="percentage rate > 30">37.49</td><td bgcolor="#77e7ae" title="percentage rate > 30">34.20</td><td bgcolor="#77e7ae" title="percentage rate > 30">38.40</td><td bgcolor="#77e7ae" title="percentage rate > 30">31.42</td><td bgcolor="#fc9999" title="percentage rate <= 30">29.98</td><td bgcolor="#77e7ae" title="percentage rate > 30">32.68</td><td bgcolor="#fc9999" title="percentage rate <= 30">25.91</td><td bgcolor="#77e7ae" title="percentage rate > 30">38.69</td><td bgcolor="#77e7ae" title="percentage rate > 30">39.52</td><td bgcolor="#77e7ae" title="percentage rate > 30">36.57</td><td bgcolor="#77e7ae" title="percentage rate > 30">33.83</td><td bgcolor="#77e7ae" title="percentage rate > 30">36.68</td><td bgcolor="#77e7ae" title="percentage rate > 30">35.45</td><td bgcolor="#77e7ae" title="percentage rate > 30">32.96</td><td bgcolor="#77e7ae" title="percentage rate > 30">33.08</td><td>&nbsp;</td><td>&nbsp;</td><td>&nbsp;</td><td>&nbsp;</td><td>&nbsp;</td><td>&nbsp;</td><td>&nbsp;</td><td>&nbsp;</td><td>&nbsp;</td></tr>
<tr align=center><th>QC=1</th><td bgcolor=gray>62.51</td><td bgcolor=gray>65.78</td><td bgcolor=gray>61.12</td><td bgcolor=gray>68.22</td><td bgcolor=gray>70.02</td><td bgcolor=gray>67.07</td><td bgcolor=gray>73.85</td><td bgcolor=gray>60.88</td><td bgcolor=gray>60.12</td><td bgcolor=gray>63.29</td><td bgcolor=gray>66.05</td><td bgcolor=gray>63.32</td><td bgcolor=gray>64.55</td><td bgcolor=gray>66.91</td><td bgcolor=gray>66.92</td><td>&nbsp;</td><td>&nbsp;</td><td>&nbsp;</td><td>&nbsp;</td><td>&nbsp;</td><td>&nbsp;</td><td>&nbsp;</td><td>&nbsp;</td><td>&nbsp;</td></tr>

<tr align=center>
<th>QC=2(<10%)</th>

<td bgcolor="#77e7ae" title="percentage rate < 10">0.00</td>
<td bgcolor="#77e7ae" title="percentage rate < 10">0.02</td>
<td bgcolor="#77e7ae" title="percentage rate < 10">0.48</td>
<td bgcolor="#77e7ae" title="percentage rate < 10">0.36</td>
<td bgcolor="#77e7ae" title="percentage rate < 10">0.00</td>
<td bgcolor="#77e7ae" title="percentage rate < 10">0.25</td>
<td bgcolor="#77e7ae" title="percentage rate < 10">0.24</td>
<td bgcolor="#77e7ae" title="percentage rate < 10">0.43</td>
<td bgcolor="#77e7ae" title="percentage rate < 10">0.35</td>
<td bgcolor="#77e7ae" title="percentage rate < 10">0.14</td>
<td bgcolor="#77e7ae" title="percentage rate < 10">0.12</td>
<td bgcolor="#77e7ae" title="percentage rate < 10">0.00</td>
<td bgcolor="#77e7ae" title="percentage rate < 10">0.00</td>
<td bgcolor="#77e7ae" title="percentage rate < 10">0.13</td>
<td bgcolor="#77e7ae" title="percentage rate < 10">0.01</td>
<td>&nbsp;</td>
<td>&nbsp;</td>
<td>&nbsp;</td>
<td>&nbsp;</td>
<td>&nbsp;</td>
<td>&nbsp;</td>
<td>&nbsp;</td>
<td>&nbsp;</td>
<td>&nbsp;</td>
</tr>

<tr align=center>
<th>Conv(>70%)</th>

<td bgcolor="#77e7ae" title="percentage rate > 70">90.84</td>
<td bgcolor="#77e7ae" title="percentage rate > 70">79.85</td>
<td bgcolor="#77e7ae" title="percentage rate > 70">83.26</td>
<td bgcolor="#77e7ae" title="percentage rate > 70">78.49</td>
<td bgcolor="#77e7ae" title="percentage rate > 70">79.32</td>
<td bgcolor="#77e7ae" title="percentage rate > 70">78.23</td>
<td bgcolor="#77e7ae" title="percentage rate > 70">79.22</td>
<td bgcolor="#77e7ae" title="percentage rate > 70">84.31</td>
<td bgcolor="#77e7ae" title="percentage rate > 70">81.87</td>
<td bgcolor="#77e7ae" title="percentage rate > 70">87.10</td>
<td bgcolor="#77e7ae" title="percentage rate > 70">83.16</td>
<td bgcolor="#77e7ae" title="percentage rate > 70">83.43</td>
<td bgcolor="#77e7ae" title="percentage rate > 70">81.68</td>
<td bgcolor="#77e7ae" title="percentage rate > 70">80.16</td>
<td bgcolor="#77e7ae" title="percentage rate > 70">78.40</td>
<td>&nbsp;</td>
<td>&nbsp;</td>
<td>&nbsp;</td>
<td>&nbsp;</td>
<td>&nbsp;</td>
<td>&nbsp;</td>
<td>&nbsp;</td>
<td>&nbsp;</td>
<td>&nbsp;</td>
</tr>

<tr align=center><td colspan=26>&nbsp;</td></tr>


</table>

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
