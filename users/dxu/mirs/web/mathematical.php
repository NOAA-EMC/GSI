<?php
// insert header that forbids caching and carries doctype
// and html tag;
require('includes/noCacheHeader.inc');
?>
<META name="verify-v1" content="CwbLBcFt9+GqRTgaLZsENmPnSWNB5MStHHdYsB7U2nI=">
<title>STAR - MIRS Project - Mathematical Background</title>
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
				
				<h1>Microwave Integrated Retrieval System (MIRS) - Mathematical Background</h1>

				<p>The basis for the inversion problem is to find a vector X, in this 
				case, a set of geophysical parameters, given a vector of measurements 
				Y<sup>m</sup>, in this case a vector of radiometric data (radiances or brightness 
				temperatures). Different techniques are available in the literature. The 
				validity of which mainly depends on the nature of problem. These 
				techniques have different names but generally result in the same 
				mathematical expressions. Among the names found in the literature are: 
				maximum probability solution (MPS), variational retrieval (1D-VAR), 
				Bayesian algorithm, Optimal Estimation Theory, etc. For the purpose of 
				describing the mathematical basis of MIRS, we will follow the 
				probabilistic approach as it will highlight two important points in the 
				assumptions made for this type of retrievals; Namely, the local-
				linearity of the forward problem, as well as the Gaussian nature of both 
				the geophysical state vector and the simulated radiometric vector around 
				the measured vector.</p>
			
				<h2>General Inverse Solution</h2>
				
				<p>In what follows, the problem is posed in general terms. Each time a 
				hypothesis is made, it is highlighted in bold with a superscript 
				attached to it. The resulting mathematical formulation of the inverse 
				problem will therefore be always valid provided those assumptions 
				satisfied. Intuitively, the retrieval problem amounts to finding the 
				vector X which maximizes the probability of being able to simulate Y<sup>m</sup>
				using X as an input and Y as the forward operator. This translates 
				mathematically into maximizing:</p>

				<p class="equation">P(X|Y<sup>m</sup>)</p>
				<p>(1)</p>
					
					<!--<p><img src="images/algorithm/image002.gif" 
					alt="equation reference (1)" /><br />
					(1)</p>-->

				<p>The Bayes theorem states that the joint probability P(X,Y) could be written as:</p>

				<p class="equation">P(X,Y)=P(Y|X)xP(X)=P(X|Y)xP(Y)</p>
				<p>(2)</p>

				<!--<p><img src="images/algorithm/image004.gif" 
					alt="equation reference (2)" /><br />(2)</p>-->

				<p>Therefore, the retrieval problem amount to maximizing:</p>

				<p><img src="images/algorithm/image006.gif" 
					alt="equation reference (3)" /><br />(3)</p>

				<p>Because we are in a situation where the measurements vector Y<sup>m</sup> 
				exists, it is therefore evident that</p>

				<p class="equation">P(Y<sup>m</sup>)=1</p>
				<p>(4)</p>

				<!--<p><img src="images/algorithm/image008.gif" 
					alt="equation reference (4)" /><br />(4)</p>-->

				<p>It is generally assumed that X follows a <strong>Gaussian</strong><sup>1</sup> distribution. If 
				we assume for a moment, for the sake of simplicity, that X is a 
				monovariate vector, with a mean value X<sub>0</sub> and a standard deviation &#963;, then 
				the Probability Density Function (PDF) of its Gaussian distribution 
				could be written as:</p>

				<p><img src="images/algorithm/image014.gif" 
					alt="equation reference (5)" /><br />(5)</p>

				<p>or more generally, for a multivariate vector:</p>

				<p><img src="images/algorithm/image016.gif" 
					alt="equation reference (6)" /><br />(6)</p>

				<p>Where <em>X<sub>0</sub></em> and <em>B</em> are the mean vector and 
					covariance matrix of the vector X respectively.</p>

				<p>Ideally, the probability <span class="equation">P(X|Y<sup>m</sup>)</span>					
					is a Dirac function with a value of zero except 
				for X. Modeling errors and instrumental noises all influence this 
				probability. For simplicity, it is assumed that the PDF of <span class="equation">P(X|Y<sup>m</sup>)</span> is also a 
				<strong>Gaussian</strong><sup>2</sup> function with Y(X) as the mean value (i.e., the errors of 
				modeling and instrumental noise are non-biased<sup>3</sup>), that could be written 
				as:</p>

				<p><img src="images/algorithm/image026.gif" 
					alt="equation reference (7)" />(7)</p>

				<p>E is the measurement and/or modeling error covariance matrix. Y is a 
				forward operator<sup>4</sup> capable of simulating a measurements-like vector.

				<p>So, maximizing <span class="equation">P(X|Y<sup>m</sup>)</span> is equivalent to maximizing <span class="equation">P(X|Y<sup>m</sup>)xP(X)</span> or:

				<p><img src="images/algorithm/image030.gif" 
					alt="equation reference (8)" />(8)</p>

				<p>It is obvious that it is easier to maximize the logarithm <span class="equation">ln(P(X|Y<sup>m</sup>))</span>
					 instead, or 
				minimize <span class="equation">-ln(P(X|Y<sup>m</sup>))</span> which would amount to minimizing:

				<p><img src="images/algorithm/image036.gif" 
					alt="equation reference (9)" />(9)</p>

				<p>J(X) is generally called the cost function. The left term represents the 
				penalty in departing from the background value (a-priori information) 
				and the right term represents the penalty in departing from the 
				measurements. The solution that minimizes this two-terms cost function 
				is sometimes referred to as a constrained solution. The minimization of 
				this cost function is also the basis for the variational analysis 
				retrieval. The solution that minimizes this cost function is easily 
				found by solving for:</p>

				<p><img src="images/algorithm/image038.gif" 
					alt="equation reference (10)" />(10)</p>

				<p>For the sake of illustration, let us consider the monovariate case 
				again. The cost function is written in this case, as:</p>

				<p><img src="images/algorithm/image040.gif" 
					alt="equation reference (11)" />(11)</p>

				<p>Where e is the standard deviation of the modeling/measurement errors 
				distribution. So, minimizing this cost function amounts to:</p>

				<p><img src="images/algorithm/image042.gif" 
					alt="equation reference (12)" />(12)</p>

				<p>At this stage, we make another assumption about the forward operator Y; 
				We assume it is locally linear5 around X. For the simple case, this 
				translates into:</p>

				<p><img src="images/algorithm/image044.gif" 
					alt="equation reference (13)" />(13)</p>

				<p>Therefore:</p>

				<p><img src="images/algorithm/image046.gif" 
					alt="equation reference (14)" />(14)</p>

				<p>If we apply the above to the general case, we obtain:</p>

				<p><img src="images/algorithm/image048.gif" 
					alt="equation reference (15)" />(15)</p>

				<p>Or,</p>

				<p><img src="images/algorithm/image050.gif" 
					alt="equation reference (16)" />(16)</p>

				<p>Which results into the following:</p>

				<p><img src="images/algorithm/image052.gif" 
					alt="equation reference (17)" />(17)</p>

				<p>If we keep in mind that:</p>

				<p><img src="images/algorithm/image054.gif" 
					alt="equation reference (18)" />(18)</p>

				<p>And if we ingest the above equations into an iterative loop, each time 
				assuming that the forward operator is linear, we could end up with the 
				following iterative solution to the cost function minimization process:</p>

				<p><img src="images/algorithm/image056.gif" 
					alt="equation reference (19)" />(19)</p>

				<p>Where n is the iteration index. The previous solution could be rewritten 
				in another form after matrix manipulations:</p>

				<p><img src="images/algorithm/image058.gif" 
					alt="equation reference (20)" />(20)</p>

				<p>Although equations 19 and 20 are equivalent mathematically, we can 
				notice that equation 20 is more efficient because it requires the 
				inversion of only one matrix. Generally, the first formulation is used 
				in the infrared interferometry because the number of parameters is much 
				smaller than the number of channels while the second formulation is more 
				often used when the number of channels is smaller than the number of 
				parameter to retrieve which is the case of the microwave in general. 
				Equation 20 is currently implemented in MIRS. At each iteration n, we 
				compute the new optimal departure from the background given the current 
				geophysical and radiometric departures, the derivatives as well as the 
				covariance matrices (error/modeling covariance and the geophysical 
				covariance). This is an iterative-based numerical solution that 
				accommodates slightly non-linear problems or parameters with slightly 
				non-Gaussian distributions. This approach to the solution is generally 
				labeled under the general term of physical retrieval. The Microwave 
				Integrated Retrieval System (MIRS) performs the retrieval in one loop as 
				detailed in next section. The whole geophysical vector is retrieved as 
				one entity, ensuring a consistent solution that fits the radiances. The 
				constraints are in the form of the measurements as well as the assumed 
				a-priori information.</p>
				


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
