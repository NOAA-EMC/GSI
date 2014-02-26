<?php
// insert header that forbids caching and carries doctype
// and html tag;
require('includes/noCacheHeader.inc');
?>
<META name="verify-v1" content="CwbLBcFt9+GqRTgaLZsENmPnSWNB5MStHHdYsB7U2nI=">
<title>STAR - MIRS Project - Simultaneous Retrieval</title>
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

				<h1>Microwave Integrated Retrieval System (MIRS) - Advantages/Drawbacks of Simultaneous Retrieval</h1>


				<p>As stated above, MIRS nominally retrieves the state vector in one single 
				vector. The resulting solution is consistent in its fitting of all 
				radiances used for the retrieval. Here simultaneous retrieval refer to 
				both geophysical (all parameters retrieved together) and radiometric 
				(all available channels used in the retrieval) simultaneous retrieval. 
				The following is a general discussion about the advantages/drawbacks of 
				following this approach. Note that the MIRS system is capable of 
				retrieving the whole geophysical vector X or a subset of it and is 
				capable of using the whole radiometric vector Y or just a subset of it.</p>

				<h2>1.  Channels Purity</h2>

				<p>Some algorithms prefer to make use of specific channels for the 
				retrieval of specific parameters. The temperature profile sounding for 
				instance is often done using channels that are sensitive only to the 
				temperature profile and nothing else. Channels that are ‘contaminated’ 
				by other parameters are disregarded. The drawback with this approach is 
				that we are missing significant information contained in other channels 
				that are sensitive to the lower part of the atmosphere (where most 
				atmospheric phenomenology occurs) but which are also sensitive to other 
				parameters such as water vapor, cloud and surface emissivity. Moreover, 
				it is unlikely to have a channel that is purely sensitive to one 
				parameter alone and is instead a varied mixture of signatures from 
				different parameters. The rate of this mixture varies with the channel 
				itself and with the geophysical situation. The tropospheric temperature 
				sounding channels are in some instances sensitive to high thick clouds 
				as well. The water vapor channels are in some dry cases sensitive to the 
				surface emissivity as well because of the low opacity due to atmospheric 
				dryness. This multi-signatures nature of the measurements is a 
				significant argument for taking the approach of the simultaneous 
				retrieval, both at the geophysical level (whole vector X retrieved), and 
				at the radiometric level (all channels used simultaneously to benefit 
				from all information content available).</p>

				<h2>2. Natural Correlation</h2>

				<p>An additional benefit in retrieving the geophysical vector together is 
				the possibility to account for the natural correlation that exists 
				between the different parameters. This correlation refers to cause-
				effect type of relationships between parameters (highly humid profiles 
				are likely over warm ocean surface temperatures for instance). The 
				correlation refers also to inter-correlation between different layers of 
				the same parameter (due to natural constraints on the temperature 
				atmospheric gradient, layers of the temperature profile are naturally 
				correlated). By performing a physical retrieval, these natural 
				correlations are accounted for and therefore introduce an additional 
				piece of information, especially useful in under-determined problems.</p>

				 
				<h2>3.  Unwanted Errors?</h2>
				
				<p>(if a channel that has no information is used)
				It is sometimes assumed that if a channel does not contain any 
				information about a specific parameter and if this channel is included 
				in the retrieval, then this will introduce unwanted errors. However, if 
				no signal is present in a measurement, then its Jacobian is null. 
				Therefore, the solution in <a href="images/algorithm/image058.gif">equation 20</a> 
				would result in a value that 
				would not move from the background value (worst case scenario). However, 
				if there is any signal under specific conditions (tskin signal in a 
				water vapor channel under dry conditions for instance), then the 
				Jacobian would not be null and therefore the retrieval would benefit 
				from including this seemingly non-useful channel. The physical approach 
				is therefore assessing dynamically, if a channel is used or not in a 
				particular retrieval. The mere fact that the channel is put in the list 
				of channels to be used does not mean that it is effectively used in the 
				estimation of the optimal solution (it is in fact not used if its 
				Jacobian is zero).</p>

				<p>In practice however, the Jacobian is not necessarily null when it 
				depends on the assumed state vector (like at the first guess stage).</p>

				<h2>4.  Added noisiness? (if a noisy channel is used in the retrieval)</h2>

				<p>It is generally assumed that a noisy channel is better left out of the 
				retrieval system, otherwise it will introduce unwanted noise in the 
				retrieved state vector. This is only true if we do not know the amount 
				of noise impacting the channel measurement. If we wrongly underestimate 
				the noise level, then the retrieval process will tend to overfit the 
				measurements producing therefore noisy retrievals (see section below 
				about the importance of the knowledge of the noise level). Note that if 
				we on the contrary, overestimate the noise level impacting the 
				radiances, then the restrieval process will tend to smooth out the 
				retrievals.</p>

				<p>If on the other hand, the noise level is known (even if it is high), 
				then the inclusion of the ‘noisy’ channel in the physical retrieval 
				should not introduce any error in theory. The reason is that the channel 
				is used effectively in the physical process only if its signal-to-noise 
				ratio is high enough. See first item of the right side of <a href="images/algorithm/image056.gif">equation (19)</a>. 
				The term could become zero (or close) if the noise is much higher than 
				the signal, resulting in no departure from the background, similar to 
				the case where a channel has no signal for the parameter to retrieve, 
				cited above. In other words, if the signal (Jacobian) of a parameter 
				present in the measurement of a channel is less than the noise level 
				known, then the retrieval won’t move that parameter. It is only when the 
				signal is higher than the noise level, that the parameter is modified in 
				the course of the retrieval. Of course, in practice, the guessed signal 
				would depend on the geophysical situation itself and on the parameter in 
				question. Examples include a channel that might be sensitive just a 
				little to water vapor but strongly to precipitation, or a channel like 
				the water vapor sounding 183 GHz that might not see any cloud because of 
				the water vapor screening but could become very sensitive to the same 
				cloud in a dry atmosphere situation. So the ratio between the signal and 
				the noise impacting the channel used in the physical retrieval, will 
				decide dynamically on the usefulness of including the noisy channel or 
				not in the retrieval. This tends to advocate using all channels in the 
				retrieval procedure but at the condition of knowing quite well the noise 
				level impacting the measurements.</p>


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
