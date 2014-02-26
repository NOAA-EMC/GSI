<?php
// insert header that forbids caching and carries doctype
// and html tag;
require('includes/noCacheHeader.inc');
?>
<META name="verify-v1" content="CwbLBcFt9+GqRTgaLZsENmPnSWNB5MStHHdYsB7U2nI=">
<title>STAR - MIRS Project - 1DVAR Algorithm Advantages</title>
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

				<h1>Microwave Integrated Retrieval System (MIRS) - 1DVAR vs Regression Technique</h1>

				<h2>Advantages of Physical Retrieval wrt Regression Technique</h2>

				<p>It is important to note that if (1) the same dataset used to generate 
				the covariance matrix and background mean vector, were also used to 
				generate the coefficients of an algorithm based on multivariate 
				regression technique, and if (2) the distributions of the geophysical 
				vector and the modeling/instrument errors are both Gaussian and 
				(3) the forward model is purely linear, then the application of the regression 
				algorithm or the first iteration of the physical algorithm would 
				mathematically result in the same optimal solution.</p>

				<p>If however the problem is moderately non-linear or if the distributions 
				non-Gaussian, the iterative-based numerical model is superior to the 
				multivariate regression technique because it allows the use of the 
				previous linear inversion locally and move, step by step, to the 
				minimum-penalty solution. It is the iterative nature of the numerical 
				method that is providing the added benefit with respect to the simple 
				regression technique, by accommodating non-linearities and non-Normal 
				distributions.</p>

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
