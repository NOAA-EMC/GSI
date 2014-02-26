<?php
// insert header that forbids caching and carries doctype
// and html tag;
require('includes/noCacheHeader.inc');
?>
<META name="verify-v1" content="CwbLBcFt9+GqRTgaLZsENmPnSWNB5MStHHdYsB7U2nI=">
<title>STAR - Microwave Integrated Retrieval System (MIRS) - Project Algorithm Overview</title>
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

				<h1>Microwave Integrated Retrieval System (MIRS) - Algorithm Description </h1>

<p>
The Microwave Integrated Retrieval System (MIRS) is an iterative, physically-based retrieval algorithm (1DVAR), whose principle is to minimize a
two-source penalty function. This cost function is composed of; (1) the departure of the simulated radiances from the actual measurements and (2) the
departure of the retrieved parameters from their respective backgrounds. The retrieval process is performed in a reduced space and expects a number of
degrees of freedom for each EDR in order to strike a balance between the stability of the matrix inversion and the optimal information content
extraction. This mechanism allows MIRS to apply to both imaging and sounding sensors or any sensor that combines imaging and sounding capabilities. In
the retrieval scheme used by MIRS, the departure from the measured radiances is normalized by the noise level (NEDT) impacting the measurements and the
uncertainty in the forward modeling, making it possible to sometimes use the signal of a particular channel when the geophysical signature (through the
derivative) is stronger than the noise (leading to a useful signal-to-noise level), and some other times dismiss the same channel when the signal in
question is within the uncertainty/noise level.</p>
<p>
The departure from the background is also scaled by the uncertainty placed on the background. This allows the retrieval to make it harder to depart from
a background information if it is deemed accurate. The source of these backgrounds is currently a simple climatology (loose background errors).</p>

<p>The MIRS system was designed to be a flexible retrieval/assimilation tool and is therefore suited for applications in the microwave as well as other
spectral regions, although it has been applied only to millimeter frequencies instruments so far. It allows the user to select which channels to use for
a particular retrieval and which to-be-retrieved parameters to turn OFF or ON in a retrieval process.</p>

<p>These features (reduced space retrieval, integrated approach and flexibility), coupled with the advanced radiative transfer model (CRTM) used as the
forward operator, allow MIRS to be a cutting edge algorithm, readily applicable to current and future sensors, both sounders and imagers.</p>

<p>The use of CRTM allows MIRS to perform retrievals in clear,cloudy as well as precipitating conditions, as this forward operator produces the radiances
and the corresponding Jacobians in all these conditions.</p>

<h2>Design/Architecture</h2>

<p>Given a set of radiances and a geophysical covariance matrix and assuming the hypotheses for its mathematical basis are satisfied, MIRS produces a set of
consistent parameters to fit the measured radiances. As mentioned above, the MIRS is interfaced with the community radiative transfer model (CRTM) that
it uses as the forward operator. CRTM is valid in all-weather conditions and has been validated independently. The current applications of MIRS involve
the retrieval of the following parameters:</p>

<ul>
<li> Temperature and water vapor vertical profiles

<li> Cloud and precipitation parameters vertical profiles (non-precipitating cloud amount, rain, ice, snow, graupel)

<li> Skin temperature and emissivity spectrum

</ul>

<p>The idea of retrieving the hydrometeors profiles (cloud and precipitation parameters) solves a number of outstanding issues:<br>
<ul>
<li> The cloud top, thickness and base are all trivial to determine from the profile.

<li> The multi-layer nature of the cloud is also easily determined from the profile.

<li> The cloud/precip derivatives computation does not suffer from instabilities that usually come with derivatives of cloud top, thickness, etc (when cloud
boundaries cross layer boundaries).
</ul>

<p>It is recognized however that the accuracy of the vertical distribution of the cloud profiles is not established.</p>

<p>The retrieval of emissivity spectrum allows application of MIRS over all surfaces: ocean, land, snow, ice, etc. One should think of this emissivity
retrieval as a technique that is similar to the cloud/aerosol optical depth retrieval in the IR. The diagram below represents the iterative nature of the
MIRS system and highlights the different assumptions that must be satisfied for the retrievals to be optimal.</p><br>



				<img src="images/static/architecture.gif" alt="architecture of MIRS process" />
				<h4>Diagram representing the general approach adopted in the iterative-based MIRS physical retrieval</h4>
				

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
