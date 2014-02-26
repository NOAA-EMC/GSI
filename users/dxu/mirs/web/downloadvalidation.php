<?php
// insert header that forbids caching and carries doctype
// and html tag;
require('includes/noCacheHeader.inc');
?>
<META name="verify-v1" content="CwbLBcFt9+GqRTgaLZsENmPnSWNB5MStHHdYsB7U2nI=">
<title>STAR - MIRS Project - Download MIRS Performance Fliers</title>
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
				
				<h1>Download MIRS NOAA-18 Performance Fliers</h1>
				
				<ul>

				<li>
				<a href="download/Performances/MIRS_Perfs_Flyer_emissivity_n18.pdf">NOAA-18 Emissivity</a>
				</li>

				<li>
				<a href="download/Performances/MIRS_Perfs_Flyer_Qprofile_n18.pdf">NOAA-18 Water Vapor Profiles</a>
				</li>
				
				<li>
				<a href="download/Performances/MIRS_Perfs_Flyer_surfaceType_n18.pdf">NOAA-18 Surface Type</a>
				</li>
				
				<li>
				<a href="download/Performances/MIRS_Perfs_Flyer_Tprofile_n18.pdf">NOAA-18 Temperature Profiles</a>
				</li>

				<li>
				<a href="download/Performances/MIRS_Perfs_Flyer_TPW_n18.pdf">NOAA-18 TPW</a>
				</li>

				<li>
				<a href="download/Performances/MIRS_Perfs_Flyer_TSKIN_n18.pdf">NOAA-18 Skin Temperature</a>
				</li>
				
				</ul>
				
				<br>
				
				<h1>Download MIRS MetOp-A Performance Fliers</h1>
				
				<ul>

				<li>
				<a href="download/Performances/MIRS_Perfs_Flyer_emissivity_metopA.pdf">MetOp-A Emissivity</a>
				</li>

				<li>
				<a href="download/Performances/MIRS_Perfs_Flyer_Qprofile_metopA.pdf">MetOp-A Water Vapor Profiles</a>
				</li>
				
				<li>
				<a href="download/Performances/MIRS_Perfs_Flyer_Tprofile_metopA.pdf">MetOp-A Temperature Profiles</a>
				</li>

				<li>
				<a href="download/Performances/MIRS_Perfs_Flyer_TPW_metopA.pdf">MetOp-A TPW</a>
				</li>

				<li>
				<a href="download/Performances/MIRS_Perfs_Flyer_TSKIN_metopA.pdf">MetOp-A Skin Temperature</a>
				</li>
				
				</ul>

				

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
