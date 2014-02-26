<?php
// insert header that forbids caching and carries doctype
// and html tag;
require('includes/noCacheHeader.inc');
?>
<META name="verify-v1" content="CwbLBcFt9+GqRTgaLZsENmPnSWNB5MStHHdYsB7U2nI=">
<title>STAR - MIRS Project - Download MIRS Document</title>
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
				<h1>Download MIRS Documents Version 9.2:</h1>
				
				<ul>

				<li>
				<a href="./download/doc_v9/MIRS_Delivery_Memo_DAP9.2_04jun2013.pdf" target=_blank>MIRS Delivery Memo</a> (84 KB)
				</li>
				
				<li>
				<a href="./download/doc_v9/MIRS_direcTree.pdf" target=_blank>MIRS Directory Tree</a> (84 KB)
				</li>
				
				<li>
				<a href="./download/doc_v9/MIRS_Interface_Control_Document_DAP9.pdf" target=_blank>MIRS Interface Control Document</a>(1.44 MB)
				</li>

				<li>
				<a href="./download/doc_v9/MIRS_ListofKnownDefects.pdf" target=_blank>MIRS List Of Known Defects</a> (15 KB)
				</li>

				<li>
				<a href="./download/doc_v9/MIRS_ProcessControl_and_ProductionRules.pdf" target=_blank>MIRS Process Control and Production Rules</a> (206 KB)
				</li>
				
				<li>
				<a href="./download/doc_v9/MIRS_System_Description_Document_DAP9.pdf" target=_blank>MIRS System Description</a> (3.28 MB)
				</li>

				<li>
				<a href="./download/doc_v9/MIRS_User_Manual_DAP9.pdf" target=_blank>MIRS User Manual</a> (3.93 MB)
				</li>

				<li>
				<a href="./download/doc_v9/NOAA_Products_MSPPS2MIRS_Transition.pdf" target=_blank>MSPPS Products to MIRS Products Transition</a> (23 KB)
				</li>

				<li>
				<a href="./download/Fortran95_standard_rev14Jul2008.pdf" target=_blank>Fortran95 Standard</a> (85 KB)
				</li>

				<li>
				<a href="./download/svn_cheatsheet.pdf" target=_blank>simple svn manual</a> (128 KB)
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
