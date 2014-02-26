<?php
// insert header that forbids caching and carries doctype
// and html tag;
require('includes/noCacheHeader.inc');
?>
<META name="verify-v1" content="CwbLBcFt9+GqRTgaLZsENmPnSWNB5MStHHdYsB7U2nI=">
<title>STAR - MIRS Project - Climate</title>
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
		<td class="mainPanel"><a name="skipTarget"></a>
		
		<noscript><h3 class="noscriptWarning">
		Javascript is currently disabled on this computer. 
		Product animations on this page are generated with javascript.
		If you need static access to these images, please contact MIRS webmaster.</h3>
		</noscript>
		
			<div class="padding">
				<!-- DO NOT DELETE OR ALTER CODE ABOVE THIS COMMENT -->
				<!-- EXCEPT for the contents of the <title></title> TAG!! -->
				<!-- You can start project specific content HERE -->
				
				<h1>Microwave Integrated Retrieval System (MIRS) - Climate</h1>
				<p>
				This is to monitor climate performance of MIRS system. It contains
				daily product maps, pentad maps and monthly maps, plus time serise.
				</p>
				<br>
				
				<h2><a href="daily.php">Daily Maps</a></h2>
				<p>This is one month long of daily product loop.</p>
				<br>
				
				<h2><a href="comp.php">Composite Maps</a></h2>
				<p>This is composite product images from all MiRS currently running satellites 
                                ( N18,N19,Metop-A,F16 SSMIS,F18 SSMIS and NPP/ATMS ).</p>
				<br>
				
				<h2><a href="climate.php">Pentad&Monthly Maps</a></h2>
				<p>This is maps of pentad/monthly and animation of those average Maps.
				It has both 1 degree and 2.5 degree resolutions.
				</p>
				<br>

				<h2><a href="climate_timeseries.php">Climate Time Series</a></h2>
				<p>This includes time series of various products of each sensors plus GDAS,
				over different surface types. It is also a diurnal variation assessment 
				(same product, same sensor, but ascending versus descending), allowing 
				an animation of the fields of the products with different steps and 
				increments to allow the user to detect any change in features with time.
				</p>
				<br>

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
