<?php
// insert header that forbids caching and carries doctype
// and html tag;
require('includes/noCacheHeader.inc');
?>
<META name="verify-v1" content="CwbLBcFt9+GqRTgaLZsENmPnSWNB5MStHHdYsB7U2nI=">
<title>STAR - MIRS Project - Validity Space</title>
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
				
				<h1>Microwave Integrated Retrieval System (MIRS) - Validity Space</h1>

				<p>Note that the same solutions (<a href="images/algorithm/image056.gif">eq. 19</a> 
				and <a href="images/algorithm/image058.gif">eq. 20</a>) could be obtained using 
				different techniques such as the optimal estimation theory or the 
				minimum variance solution (MVS). Both assume a local linearity of the 
				forward model. These methods are therefore all mathematically equivalent 
				and make the same assumptions that we summarize below:</p>

				<ul>
					<li>The probability density function of the geophysical vector X 
					is assumed Gaussian with a mean background and a representative 
					covariance matrix.</li>
					<li>The forward operator Y is able to simulate measurements-like 
					radiances</li>
					<li>The errors of the models and the instrumental noise combined 
					are assumed non-biased and normally distributed.</li>
					<li>The forward model is assumed to be locally linear at each 
					iteration.</li>
				</ul>

				<p>A legitimate question would be the following: What would happen if any 
				of the assumptions above is not satisfied or if the mean and covariance 
				information are not accurate enough? The solution that would be obtained 
				under those conditions would likely be non-optimal. The term non-optimal 
				here refers to the fact that the cost function would not necessarily be 
				the smallest possible. The formulation of the cost function above 
				(<a href="images/algorithm/image036.gif">eq. 9 </a>
				and the resulting solutions <a href="images/algorithm/image056.gif">19</a> 
				and <a href="images/algorithm/image058.gif">20</a>) were possible because of the 
				simplifications introduced with those assumptions. In theory it is 
				possible to derive another cost function based on non-Gaussian 
				assumptions although it will likely be complicated. The corresponding 
				solution could as well be determined by solving for the same equation 10 
				and resulting in a different formulation of the optimal solution.</p>


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
