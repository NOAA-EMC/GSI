<?php
// insert header that forbids caching and carries doctype
// and html tag;
require('includes/noCacheHeader.inc');
?>
<META name="verify-v1" content="CwbLBcFt9+GqRTgaLZsENmPnSWNB5MStHHdYsB7U2nI=">
<title>STAR - MIRS Project - Inter-Comparison</title>
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
				
				<h1>Microwave Integrated Retrieval System (MIRS) - Inter-Comparison</h1>
				<p> The inter-Comparison is to compare the same products retrived from MiRS,but
				different sensors, such as NOAA18 vs NOAA-19, Metop-A vs NPP ATMS, etc.
				</p>
				<br>
				
				<h2><a href="inter.php">Inter-Comparison</h2> </a>
				<p>For gridded data set:
				POES and NPP/ATMS are gridded using resolution of 0.25 degree, while DMSP is girdded using
				resolution of 0.5 degree. No average is taken, and only the near-nadir values are recorded. 
				Filling is done for the grid box if it falls in FOV coverage. 
				</p>
				<br>

				<p>
				For P2P data set:. 
				We have increase time thresh hold value to 4 hours to observe any scan pattern shifted situation
				and the space thresh hold is max of the 2 FOV sizes. The FOV size is changing with scan 
				position for POES, so is the space thresh hold used. For each data point in data set 1,
				we find the closest data point in data set 2 if the time difference is less than 4 hours. Due to 
				resource constraints, we can not run P2P collocation for high resolution products.
				</p>
				<br>


				<h2><a href="qcinter.php">Performance Table</h2> </a>
				<p>This is table format of inter-comparison stats. Gray one is used as reference set.
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
