<?php
// insert header that forbids caching and carries doctype
// and html tag;
require('includes/noCacheHeader.inc');
?>
<META name="verify-v1" content="CwbLBcFt9+GqRTgaLZsENmPnSWNB5MStHHdYsB7U2nI=">
<title>STAR - MIRS Project - Products</title>
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
				
				<h1>Microwave Integrated Retrieval System (MIRS) - Products Monitoring</h1>
				<br>

				<h2><a href="product.php">Daily Products - Low Resolution</a></h2>
				<p>This is the low resolution daily products monitoring.The MIRS runs routinely at 
				NOAA/NESDIS/STAR for a number of sensors (currently
				NOAA-18, NOAA-19, METOP-A, F16 SSMIS, F18 SSMIS, TRMM TMI).
				It is important to note that at any given day at around 1AM, 
				MIRS kicks off the processing of the previous day data. ( TRMM TMI has 4 days delay )
				</p>
				<br>

				<h2><a href="highresolution.php">Daily Products - High Resolution</a></h2>
				<p>MIRS is also running on daily basis for some selected regions using high resolution mode:
				For POES, it uses MHS foot print size; for DMSP, it uses image channel resoltion; for NPP ATMS, 
				it uses 96 NFOV ( roughly the same as MHS resolution ). Due to
				resource limitation, we can only afford to run N18 and NPP ATMS high resolution mode over global at this moment.
				</p>
				<br>


				<h2><a href="vertical.php">Vertical Cross Section</a></h2>
				<p>This is vertical cross section of hydrometrical products. In order to evaluate
				the performance of MIRS, we do comparison with GDAS,ECMWF and TRMM_2A12. The GDAS
				data has 2 days delay, ECMWF data is not very stable, and TRMM_2A12 has 6 days delay.
				</p>
				<br>

				<h2><a href="dataquality.php">Data Quality</a></h2>
				<p>This is to monitor how well MiRS is performing in the retrieval process. 
				The section includes time series plots of the convergence rate for
				each orbit, as well as the percentage of QC flags equal to 0 (Good), 
				1 (Some meteorological condition, e.g. precipitation, inversion,
				super-saturation), and 2 (Bad). </p><br>
				





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
