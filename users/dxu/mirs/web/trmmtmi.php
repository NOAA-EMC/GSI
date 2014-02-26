<?php
// insert header that forbids caching and carries doctype
// and html tag;
require('includes/noCacheHeader.inc');
?>
<META name="verify-v1" content="CwbLBcFt9+GqRTgaLZsENmPnSWNB5MStHHdYsB7U2nI=">
<title>STAR - Microwave Integrated Retrieval System (MIRS) - Sensors - TRMM TMI</title>
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
			TRMM TMI Overview</h1>

			<table border="0" cellpadding="0" cellspacing="0" class="tableGrid">
			<caption>TMI Characteristics (From Kummerow et al. 1998)</caption>
				<tr>
					<th scope="col" >Channel Number</th>
					<th scope="col" >1</th>
					<th scope="col" >2</th>
					<th scope="col" >3</th>
					<th scope="col" >4</th>
					<th scope="col" >5</th>
					<th scope="col" >6</th>
					<th scope="col" >7</th>
					<th scope="col" >8</th>
					<th scope="col" >9</th>
				</tr>
				
				<tr>
					<th scope="row">Central freq (GHz)</th>
					<td align=center>10.65</td> 
					<td align=center>10.65</td> 
					<td align=center>19.35</td> 
					<td align=center>19.35</td> 
					<td align=center>21.3</td> 
					<td align=center>37.0</td> 
					<td align=center>37.0</td> 
					<td align=center>85.5</td> 
					<td align=center>85.5</td> 
				</tr>
				
				<tr>
					<th scope="row">Polarization</th>
					<td align=center>V</td>
					<td align=center>H</td>
					<td align=center>V</td>
					<td align=center>H</td>
					<td align=center>V</td>
					<td align=center>V</td>
					<td align=center>H</td>
					<td align=center>V</td>
					<td align=center>H</td>
				</tr>
				
				<tr>
					<th scope="row">Bandwidth (MHz)</th>
					<td align=center>100</td>
					<td align=center>100</td>
					<td align=center>500</td>
					<td align=center>500</td>
					<td align=center>200</td>
					<td align=center>2000</td>
					<td align=center>2000</td>
					<td align=center>3000</td>
					<td align=center>3000</td>
				</tr>
				
				<tr>
					<th scope="row">Stability (MHz)</th>
					<td align=center>10</td>
					<td align=center>10</td>
					<td align=center>20</td>
					<td align=center>20</td>
					<td align=center>20</td>
					<td align=center>50</td>
					<td align=center>50</td>
					<td align=center>100</td>
					<td align=center>100</td>
				</tr>

				<tr>
					<th scope="row">Beamwidth (deg)</th>
					<td align=center>3.68</td>
					<td align=center>3.75</td>
					<td align=center>1.90</td>
					<td align=center>1.88</td>
					<td align=center>1.70</td>
					<td align=center>1.0</td>
					<td align=center>1.0</td>
					<td align=center>0.42</td>
					<td align=center>0.23</td>
				</tr>
				
				<tr>
					<th scope="row" title="Instantaneous Field-Of-View Along Track">IFOV Along-Track (km)</th>
					<td align=center>59.0</td>
					<td align=center>60.1</td>
					<td align=center>30.5</td>
					<td align=center>30.1</td>
					<td align=center>27.2</td>
					<td align=center>16.0</td>
					<td align=center>16.0</td>
					<td align=center>6.7</td>
					<td align=center>6.9</td>
				</tr>

				<tr>
					<th scope="row" title="Instantaneous Field-Of-View Cross Track">IFOV Cross-Track (km)</th>
					<td align=center>35.7</td>
					<td align=center>36.4</td>
					<td align=center>18.4</td>
					<td align=center>18.2</td>
					<td align=center>16.5</td>
					<td align=center>9.7</td>
					<td align=center>9.7</td>
					<td align=center>4.1</td>
					<td align=center>4.2</td>
				</tr>
				
				<tr>
					<th scope="row">Integration time <br />per sample (ms)</th>
					<td align=center>6.6</td>
					<td align=center>6.6</td>
					<td align=center>6.6</td>
					<td align=center>6.6</td>
					<td align=center>6.6</td>
					<td align=center>6.6</td>
					<td align=center>6.6</td>
					<td align=center>3.3</td>
					<td align=center>3.3</td>
 				</tr>
				
				<tr>
					<th scope="row" title="Effective Field-Of-View Along Track">EFOV Along-Track (km)</th>
					<td align=center>63.2</td>
					<td align=center>63.2</td>
					<td align=center>30.4</td>
					<td align=center>30.4</td>
					<td align=center>22.6</td>
					<td align=center>16.0</td>
					<td align=center>16.0</td>
					<td align=center>7.2</td>
					<td align=center>7.2</td>
				</tr>
				
				<tr>
					<th scope="row" title="Effective Field-Of-View Cross Track">EFOV Cross-Track (km)</th>
					<td align=center>9.1</td>
					<td align=center>9.1</td>
					<td align=center>9.1</td>
					<td align=center>9.1</td>
					<td align=center>9.1</td>
					<td align=center>9.1</td>
					<td align=center>9.1</td>
					<td align=center>4.6</td>
					<td align=center>4.6</td>
				</tr>
				
				<tr>
					<th scope="row">EFOVs per scan</th>
					<td align=center>104</td>
					<td align=center>104</td>
					<td align=center>104</td>
					<td align=center>104</td>
					<td align=center>104</td>
					<td align=center>104</td>
					<td align=center>104</td>
					<td align=center>208</td>
					<td align=center>208</td>
				</tr>
				
				<tr>
					<th scope="row">Samples (N) <br />per beamwidth</th>
					<td align=center>4</td>
					<td align=center>4</td>
					<td align=center>2</td>
					<td align=center>2</td>
					<td align=center>2</td>
					<td align=center>1</td>
					<td align=center>1</td>
					<td align=center>1</td>
					<td align=center>1</td>
				</tr>
				
				<tr>
					<th scope="row">Beam EFOV ( km x km )</th>
					<td align=center>63 x 63</td>
					<td align=center>63 x 63</td>
					<td align=center>30 x 30</td>
					<td align=center>30 x 30</td>
					<td align=center>23 x 23</td>
					<td align=center>16 x 9</td>
					<td align=center>16 x 9</td>
					<td align=center>7 x 5</td>
					<td align=center>7 x 5</td>
				</tr>
				
				<tr>
					<th scope="row">Beam EFOVs per scan</th>
					<td align=center>26</td>
					<td align=center>26</td>
					<td align=center>52</td>
					<td align=center>52</td>
					<td align=center>52</td>
					<td align=center>104</td>
					<td align=center>104</td>
					<td align=center>208</td>
					<td align=center>208</td>
				</tr>
				
				<tr>
					<th scope="row">NEdT (K)</th>
					<td align=center>0.63</td>
					<td align=center>0.54</td>
					<td align=center>0.50</td>
					<td align=center>0.47</td>
					<td align=center>0.71</td>
					<td align=center>0.36</td>
					<td align=center>0.31</td>
					<td align=center>0.52</td>
					<td align=center>0.93</td>
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
					<p>Source: <a href="http://trmm.gsfc.nasa.gov/">TRMM official site</a></p>



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
