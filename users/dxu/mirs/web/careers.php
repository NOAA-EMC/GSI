<?php
// insert header that forbids caching and carries doctype
// and html tag;
require('includes/noCacheHeader.inc');
?>
<META name="verify-v1" content="CwbLBcFt9+GqRTgaLZsENmPnSWNB5MStHHdYsB7U2nI=">
<title>STAR - Microwave Integrated Retrieval System (MIRS) - Careers </title>
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

				<br>

				<h1>MIRS Announcement of PhD Level Research Position for 2012</h1>

				<ul>
				<li>
				<a href="openings/MiRS_PhD_projects_v0.3.pdf" 
				target="_blank"
				title="this document opens a PDF file in a new window">
				MIRS PhD position
				</a> (PDF, 19 KB),<br>
				</li>
				</ul>

				<br>
				<h1>MIRS Announcement of Post-doctoral Research Position for 2012</h1>

				<ul>
				<li>
				<a href="openings/MiRS_Postdoc_projects_v0.3.pdf" 
				target="_blank"
				title="this document opens a PDF file in a new window">
				MIRS postdoc position
				</a> (PDF, 21 KB),<br>
				</li>
				</ul>



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
