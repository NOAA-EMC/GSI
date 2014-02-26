<?php
// insert header that forbids caching and carries doctype
// and html tag;
require('includes/noCacheHeader.inc');
?>
<META name="verify-v1" content="CwbLBcFt9+GqRTgaLZsENmPnSWNB5MStHHdYsB7U2nI=">
<title>STAR - Microwave Integrated Retrieval System (MIRS) - Sensors - AMSUB</title>
<?php
// style links and the icon link
// pulls in .css files for regular and print display
require('includes/styleLinks.inc');
?>

<!-- if you need to include your own .css files
     put the links HERE, before the closing head tag. -->

</head>
<body>
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
		<td class="mainPanel"><a name="skipTarget"></a><?php require('includes/noScriptWarning.inc'); ?>
			<div class="padding">
				<!-- DO NOT DELETE OR ALTER CODE ABOVE THIS COMMENT -->
				<!-- EXCEPT for the contents of the <title></title> TAG!! -->
				<!-- You can start project specific content HERE -->

			<h1>Sensors<br />
			AMSU-B Overview</h1>

			<p>AMSU-B is a 5 channel cross-track, continuous line scanning,total power 
			microwave radiometer. The instrument has an instantaneous field-of-view of 
			1.1&#176;. Spatial resolution at nadir is nominally 16 km (9.94 mi). The 
			antenna provides a cross-track scan, scanning &#177;48.95&#176; from nadir with a 
			total of 90 Earth fields-of-view per scan line. This instrument completes 
			one scan every 8/3 seconds. The two channels are centered nominally at 89 
			GHz and 150 GHz, and the other three centered around the 183.31 GHz water 
			vapor line with double-sideband centers located at 183.31&#177;1, &#177;3, and &#177;7 
			GHz, respectively. AMSU-B has a FOV of 1.1 degrees &#177;10%.</p>

			<table border="0" cellpadding="0" cellspacing="0" class="tableGrid">
			<caption>AMSU-B Channel Characteristics</caption>
				<tr>
					<th scope="col" bgcolor=yellow>Channel Number</th> 
					<th scope="col" bgcolor=yellow>Central<br />Frequency (GHz)</th>
					<th scope="col" bgcolor=yellow>Double-sided <br /> Maximum (MHz)</th>
					<th scope="col" bgcolor=yellow>Pass<br /> Band (MHz)</th>
					<th scope="col" bgcolor=yellow>IF<br /> Band (MHz)</th>
					<th scope="col" bgcolor=yellow>Stop<br /> Band (MHz)</th>
					<th scope="col" bgcolor=yellow>NE&#8710;T(K)</th>
				</tr>
				
				<tr>
					<th scope="row">16</th>
					<td align=center>89.0&#177;0.9</td>
					<td align=center>6000</td>
					<td align=center>3000</td>
					<td align=center>&#8805; 1000</td>
					<td align=center>&#177;400</td>
					<td align=center>0.37</td>
				</tr>
				  
				<tr>
					<th scope="row">17</th>
					<td align=center>150.0&#177;0.9</td>
					<td align=center>4000</td>
					<td align=center>2000</td>
					<td align=center>&#8805; 1000</td>
					<td align=center>&#177;400</td>
					<td align=center>0.84</td>
				</tr>

				<tr>
					<th scope="row">18</th>
					<td align=center>183.31&#177;1.00</td>
					<td align=center>1000</td>
					<td align=center>2 x 500</td>
					<td align=center>500</td>
					<td align=center>-</td>
					<td align=center>1.06</td>
				</tr>  

				<tr>
					<th scope="row">19</th>
					<td align=center>183.31&#177;3.00</td> 
					<td align=center>2000</td> <td align=center>2 x 1000</td> 
					<td align=center>1000</td>
					<td align=center>-</td>
					<td align=center>0.70</td>
				</tr>
				<tr>
					<th scope="row">20</th>
					<td align=center>183.31&#177;7.00</td>
					<td align=center>4000</td>
					<td align=center>2 x 2000</td>
					<td align=center>2000</td>
					<td align=center>-</td>
					<td align=center>0.60</td>
				</tr>  
			</table>
			
				<br />
				<hr>
				<br />
					<p>Source: <a href="http://www.ncdc.noaa.gov/oa/pod-guide/ncdc/docs/klm/html/c3/sec3-4.htm">NOAA KLM User's Guide</a></p>

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
