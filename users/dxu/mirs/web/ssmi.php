<?php
// insert header that forbids caching and carries doctype
// and html tag;
require('includes/noCacheHeader.inc');
?>
<META name="verify-v1" content="CwbLBcFt9+GqRTgaLZsENmPnSWNB5MStHHdYsB7U2nI=">
<title>STAR - MIRS Project - - Sensors - SSM/I</title>
<?php
// style links and the icon link
// pulls in .css files for regular and print display
require('includes/styleLinks.inc');
?>
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
				SSM/I Overview</h1>


				<table border="0" cellpadding="0" cellspacing="0" class="tableGrid">
				<caption>SSM/I Sensor Characteristics</caption>
				
				<tr>
				<th scope="col" rowspan=2>Channel<br>Frequency<br>(GHz)<br></th>
				<th scope="col" rowspan=2>Pol.<br>(V/H)<br></th>
				<th scope="col" rowspan=2>IF Pass-Band<br>(MHz)<br></th>
				<th scope="col" colspan=3 align=center>Beamwidth (Deg)</th>
				<th scope="col" colspan=2 align=center>EFOV on Earth Surface</th>
				</tr>

				<tr>
				<th scope="col" >E-Plane<br>IFOV<br></th>
				<th scope="col" >H-Plane<br>IFOV<br></th>
				<th scope="col" >H-Plane<br>EFOV<br></th>
				<th scope="col" >Along-<br>Track<br></th>
				<th scope="col" >Cross-<br>Track<br></th>
				</tr>

				<tr>
				<td align=center>19.35</td>
				<td align=center>V</td>
				<td align=center>10-250</td>
				<td align=center>1.86</td>
				<td align=center>1.87</td>
				<td align=center>1.93</td>
				<td align=center>69 km</td>
				<td align=center>43 km</td>
				</tr>

				<tr>
				<td align=center>19.35</td>
				<td align=center>H</td>
				<td align=center>10-250</td>
				<td align=center>1.88</td>
				<td align=center>1.87</td>
				<td align=center>1.93</td>
				<td align=center>69 km</td>
				<td align=center>43 km</td>
				</tr>

				<tr>
				<td align=center>22.235</td>
				<td align=center>V</td>
				<td align=center>10-250</td>
				<td align=center>1.60</td>
				<td align=center>1.65</td>
				<td align=center>1.83</td>
				<td align=center>60 km</td>
				<td align=center>40 km</td>
				</tr>

				<tr>
				<td align=center>37.0</td>
				<td align=center>V</td>
				<td align=center>100-1000</td>
				<td align=center>1.00</td>
				<td align=center>1.10</td>
				<td align=center>1.27</td>
				<td align=center>37 km</td>
				<td align=center>28 km</td>
				</tr>

				<tr>
				<td align=center>37.0</td>
				<td align=center>H</td>
				<td align=center>100-1000</td>
				<td align=center>1.00</td>
				<td align=center>1.10</td>
				<td align=center>1.31</td>
				<td align=center>37 km</td>
				<td align=center>29 km</td>
				</tr>

				<tr>
				<td align=center>85.5</td>
				<td align=center>V</td>
				<td align=center>100-1500</td>
				<td align=center>0.41</td>
				<td align=center>0.43</td>
				<td align=center>0.60</td>
				<td align=center>15 km</td>
				<td align=center>13 km</td>
				</tr>

				<tr>
				<td align=center>85.5</td>
				<td align=center>H</td>
				<td align=center>100-1500</td>
				<td align=center>0.42</td>
				<td align=center>0.45</td>
				<td align=center>0.60</td>
				<td align=center>15 km</td>
				<td align=center>13 km</td>
				</tr>

				</table>

				<br>

				<p>More detailed information can be found here 
				<a href="ftp://ftp.star.nesdis.noaa.gov/pub/corp/scsb/wchen/doc/ssmitdr.html">SSM/I TDR Documentation</a>
				</p>



				<br />
				<br />
				<hr>
				<br />
					<p>Source: <a href="http://www.ssmi.com/">Remote Sensing Systems SSMI</a></p>
					


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
