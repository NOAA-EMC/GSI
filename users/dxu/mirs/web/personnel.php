<?php
// insert header that forbids caching and carries doctype
// and html tag;
require('includes/noCacheHeader.inc');
?>
<META name="verify-v1" content="CwbLBcFt9+GqRTgaLZsENmPnSWNB5MStHHdYsB7U2nI=">
<title>STAR - Microwave Integrated Retrieval System (MIRS) - Personnel</title>
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

				<h1>Microwave Integrated Retrieval System (MIRS) - Team Members</h1>

				<table width="100%" border="0" cellspacing=4 cellpadding=4>

				<tr>

				<td>
				<h2>Scientific Development Team</h2>
				<ul>
					<li><a href="mailto:Sid.Boukabara@noaa.gov">Sid-Ahmed Boukabara (Lead)</a></li>
					<li><a href="mailto:Xiwu.Zhan@noaa.gov">Xiwu Zhan</a></li>
					<li><a href="mailto:Kevin.Garrett@noaa.gov">Kevin Garrett</a></li>
					<!--li><a href="mailto:Flavio.Iturbide@noaa.gov">Flavio Iturbide-Sanchez</a></li-->
					<li><a href="mailto:Christopher.Grassotti@noaa.gov">Chris Grassotti</a></li>
					<!--li><a href="mailto:Shepard.Clough@noaa.gov">Shepard A. Clough (Tony), Consultant </a></li-->
					<li><a href="mailto:Amanda.Mims@noaa.gov">Amanda Mims</a></li>
					<li><a href="mailto:tanvir.islam@noaa.gov">Tanvir Islam</a></li>
					<li><a href="mailto:viktor.zubko@noaa.gov">Viktor Zubko</a></li>
					<li><a href="mailto:Wanchun.Chen@noaa.gov">Wanchun Chen</a></li>
				</ul>
				</td>

				<td>
				<h2>Oversight Board</h2>
				<ul>
					<li><a href="mailto:Fuzhong.Weng@noaa.gov">Fuzhong Weng (Chair)</a></li>
					<li><a href="mailto:Ralph.R.Ferraro@noaa.gov">Ralph Ferraro</a></li>
					<li><a href="mailto:Limin.Zhao@noaa.gov">Limin Zhao</a></li>
					<li><a href="mailto:Tom.Schott@noaa.gov">Tom Schott</a></li>
				</ul>
				</td>

				<td>
				<h2>Webmaster</h2>
				<ul>
					<li><a href="mailto:Wanchun.Chen@noaa.gov">Wanchun Chen</a></li>
				</ul>
				</td>

				</tr>
				</table>
			
			
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
