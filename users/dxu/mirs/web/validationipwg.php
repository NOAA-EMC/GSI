<?php
// insert header that forbids caching and carries doctype
// and html tag;
require('includes/noCacheHeader.inc');
?>
<META name="verify-v1" content="CwbLBcFt9+GqRTgaLZsENmPnSWNB5MStHHdYsB7U2nI=">
<title>STAR - MIRS Project - IPWG</title>
<?php
// style links and the icon link
// pulls in .css files for regular and print display
require('includes/styleLinks.inc');
?>
<style type="text/css">
	p.equation, span.equation {
	font-size: 1.6em !important; 
	font-style: italic; 
	font-family: serif;
	letter-spacing: 0.2em !important;
	}

</style>
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
			<div class="padding" id="algorithm">
				<!-- DO NOT DELETE OR ALTER CODE ABOVE THIS COMMENT -->
				<!-- EXCEPT for the contents of the <title></title> TAG!! -->
				<!-- You can start project specific content HERE -->

				<h1>MIRS at the International Precipitation Working Group (IPWG) Project</h1>

				<h2>Description of IPWG project</h2>

				<p>Please visit IPWG web site <a href="http://cawcr.gov.au/projects/SatRainVal/validation-intercomparison.html">IPWG website</a>.</p>

				<p>Please visit IPWG web site at Japan <a href="http://www-ipwg.kugi.kyoto-u.ac.jp/IPWG/dailyval.html">IPWG at Japan</a>.</p>

				<h2>Validation / Intercomparison of Daily Satellite Precipitation Estimates</h2>

				<p><a href="http://cics.umd.edu/~johnj/us_web.html">
				Verification of MIRS Precipitation Estimate Over the US</a></p>

				<p><a href="http://cics.umd.edu/~dvila/web/mirs_gif.dir/">
				Verification of MIRS Precipitation Estimate Over the South America</a></p>

				<p><a href="http://cawcr.gov.au/projects/SatRainVal/sat_val_aus.html">
				Verification of MIRS Precipitation Estimate Over Australia</a></p>



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
