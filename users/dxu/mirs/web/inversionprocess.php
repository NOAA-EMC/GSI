<?php
// insert header that forbids caching and carries doctype
// and html tag;
require('includes/noCacheHeader.inc');
?>
<META name="verify-v1" content="CwbLBcFt9+GqRTgaLZsENmPnSWNB5MStHHdYsB7U2nI=">
<title>STAR - MIRS Project - Testbed</title>
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
				
				<h1>Microwave Integrated Retrieval System (MIRS) - Testbed</h1>
				
				<h2>Inversion Process</h2>
				<p>
				<img src="images/static/testbed3.png"
				alt="MIRS Inversion Process"
				width=700 height=410
				border=0 usemap="#testbedmap3"/> 

				<map name="testbedmap3">
				  <!--area shape="rect" coords="30, 60, 190,172" href="../mspps/" alt="Heritage Algorithm" target="_blank" title="this link opens a new window"-->
				  <area shape="rect" coords="270,60, 440,172" href="advancedretrieval.php" alt="Advanced Retrieval(1DVAR)">
				  <area shape="rect" coords="495,60, 675,172" href="vipp.php" alt="Vertical Integrated & Post-Processing">
				  <area shape="rect" coords="290,270,420,345" href="product.php" alt="MIRS Products Monitoring">
  				</map>
				</p>


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


