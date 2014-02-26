<?php
// insert header that forbids caching and carries doctype
// and html tag;
require('includes/noCacheHeader.inc');
?>
<META name="verify-v1" content="CwbLBcFt9+GqRTgaLZsENmPnSWNB5MStHHdYsB7U2nI=">
<title>STAR - Microwave Integrated Retrieval System (MIRS) - Sensors - AMSUA</title>
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
			AMSU-A Overview</h1>

			<p>The Advanced Microwave Sounding Unit-A (AMSU-A) is a 15-channel cross-
			track, stepped-line scanning, total power microwave radiometer. The 
			instrument has an instantaneous field-of-view of 3.3&#176; at the half-power 
			points providing a nominal spatial resolution at nadir of 48 km (29.8 mi). 
			The antenna provides a cross-track scan, scanning &#177;48.3&#176; from nadir with a 
			total of 30 Earth fields-of-view per scan line. This instrument completes 
			one scan every 8 seconds.</p>
			
			<table border="0" cellpadding="0" cellspacing="0" class="tableGrid">
			<caption>AMSU-A Channel Characteristics</caption>
				<tr>
					<th scope="col" >Channel Number</th>
					<th scope="col" >Central Freq.<br /> (MHz)</th>
					<th scope="col" ># Bands</th>
					<th scope="col" >Nominal<br />Bandwidth<br />(MHz)</th>
					<th scope="col" >Nominal<br />Beamwidth<br />(degree)</th>
					<th scope="col" >Central<br /> Frequency<br /> Stability (MHz)</th>
					<th scope="col" >Temperature<br /> Sensivity (K)<br /> NE&#8710;T</th>
					<th scope="col" >Calibration<br /> Accuracy (K)</th>
					<th scope="col" >Polarization<br /> at nadir</th>
				</tr>
				
				<tr>
					<th scope="row">1</th>
					<td align=center>23800</td> 
					<td align=center>1</td> 
					<td align=center>270</td> 
					<td align=center>3.3</td>
					<td align=center>10</td>
					<td align=center>0.3</td>
					<td align=center>2.0</td>
					<td align=center>V</td>
				</tr>
				
				<tr>
					<th scope="row">2</th>
					<td align=center>31400</td>
					<td align=center>1</td>
					<td align=center>180</td>
					<td align=center>3.3</td>
					<td align=center>10</td>
					<td align=center>0.3</td>
					<td align=center>2.0</td>
					<td align=center>V</td>
				</tr>
				
				<tr>
					<th scope="row">3</th>
					<td align=center>50300</td>
					<td align=center>1</td>
					<td align=center>180</td>
					<td align=center>3.3</td>
					<td align=center>10</td>
					<td align=center>0.4</td>
					<td align=center>1.5</td>
					<td align=center>V</td>
				</tr>
				
				<tr>
					<th scope="row">4</th>
					<td align=center>52800</td>
					<td align=center>1</td>
					<td align=center>400</td>
					<td align=center>3.3</td>
					<td align=center>5</td>
					<td align=center>0.25</td>
					<td align=center>1.5</td>
					<td align=center>V</td>
				</tr>
				
				<tr>
					<th scope="row">5</th>
					<td align=center>53596<br />&#177;115</td>
					<td align=center>2</td>
					<td align=center>170</td>
					<td align=center>3.3</td>
					<td align=center>5</td>
					<td align=center>0.25</td>
					<td align=center>1.5</td>
					<td align=center>H</td>
				</tr>
				
				<tr>
					<th scope="row">6</th>
					<td align=center>54400</td>
					<td align=center>1</td>
					<td align=center>400</td>
					<td align=center>3.3</td>
					<td align=center>5</td>
					<td align=center>0.25</td>
					<td align=center>1.5</td>
					<td align=center>H</td>
				</tr>
				
				<tr>
					<th scope="row">7</th>
					<td align=center>54940</td>
					<td align=center>1</td>
					<td align=center>400</td>
					<td align=center>3.3</td>
					<td align=center>5</td>
					<td align=center>0.25</td>
					<td align=center>1.5</td>
					<td align=center>V</td>
				</tr>
				
				<tr>
					<th scope="row">8</th>
					 <td align=center>55500</td>
					 <td align=center>1</td>
					 <td align=center>330</td>
					 <td align=center>3.3</td>
					 <td align=center>10</td>
					 <td align=center>0.25</td>
					 <td align=center>1.5</td>
					 <td align=center>H</td>
				</tr>
				
				<tr>
					<th scope="row">9</th>
					<td align=center>57290.344</td>
					<td align=center>1</td>
					<td align=center>330</td>
					<td align=center>3.3</td>
					<td align=center>0.5</td>
					<td align=center>0.25</td>
					<td align=center>1.5</td>
					<td align=center>H</td>
				</tr>
				
				<tr>
					<th scope="row">10</th>
					<td align=center>57290.344<br />&#177;217</td>
					<td align=center>2</td>
					<td align=center>78</td>
					<td align=center>3.3</td>
					<td align=center>0.5</td>
					<td align=center>0.4</td>
					<td align=center>1.5</td>
					<td align=center>H</td>
				</tr>
				
				<tr>
					<th scope="row">11</th>
					<td align=center>57290.344<br />&#177;322.2<br />&#177;48</td>
					<td align=center>4</td>
					<td align=center>36</td>
					<td align=center>3.3</td>
					<td align=center>1.2</td>
					<td align=center>0.4</td>
					<td align=center>1.5</td>
					<td align=center>H</td>
				</tr>
				
				<tr>
					<th scope="row">12</th>
					<td align=center>57290.344<br />&#177;322.2<br />&#177;22</td>
					<td align=center>4</td>
					<td align=center>16</td>
					<td align=center>3.3</td>
					<td align=center>1.2</td>
					<td align=center>0.6</td>
					<td align=center>1.5</td>
					<td align=center>H</td>
				</tr>
				
				<tr>
					<th scope="row">13</th>
					<td align=center>57290.344<br />&#177;322.2<br />&#177;10</td>
					<td align=center>4</td>
					<td align=center>8</td>
					<td align=center>3.3</td>
					<td align=center>0.5</td>
					<td align=center>0.80</td>
					<td align=center>1.5</td>
					<td align=center>H</td>
				</tr>
					
				<tr>
					<th scope="row">14</th>
					<td align=center>57290.344<br />&#177;322.2<br />&#177;4.5</td>
					<td align=center>4</td>
					<td align=center>3</td>
					<td align=center>3.3</td>
					<td align=center>0.5</td>
					<td align=center>1.20</td>
					<td align=center>1.5</td>
					<td align=center>H</td>
				</tr>
					
				<tr>
					<th scope="row">15</th>
					<td align=center>89000</td>
					<td align=center>1</td>
					<td align=center><6000</td>
					<td align=center>3.3</td>
					<td align=center>50</td>
					<td align=center>0.5</td>
					<td align=center>2.0</td>
					<td align=center>V</td>
					</tr>
					
				</table>
				
				<br />
				<ul>
					<li><strong>V:</strong> Polarization vector is parallel to scan plane at nadir</li>
					<li><strong>H:</strong> Polarization vector is perpendicular to scan plane at nadir</li>
				</ul>
				
				
				<br />
				<hr>
				<br />
					<p>Source: <a href="http://www.ncdc.noaa.gov/oa/pod-guide/ncdc/docs/klm/html/c3/sec3-3.htm">NOAA KLM User's Guide</a></p>

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
