<?php
// insert header that forbids caching and carries doctype
// and html tag;
require('includes/noCacheHeader.inc');
?>
<title>Full Width Page Style -- Your Title Here!</title>
<?php
// insert style links and the icon link
require('includes/styleLinks.inc');
?>
</head>
<body>

<?php
// insert banner rows
require('includes/banner.inc');
?>
<?php
// insert gray link bar
require('includes/toolBar_withSearch.inc');
?>
  <tr>
    <td colspan="2" class="mainPanel" id="bestOf"><a name="skipTarget"></a><?php require('includes/noScriptWarning.inc'); ?>
			<div class="padding">
			<ul class="breadcrumbs">
				<li><a href="index.php">Project Home</a></li>
				<li>&nbsp;>&nbsp;Title of this page</li>
			</ul>
			<div class="floatBox" id="bestTitle">
				<div class="left">
					<h1>Title of this page</h1>
				</div>
				<div class="right">Your content or small image here</div>
				<div class="clearing"> </div>
			</div>
			<h2>Subheading for your page</h2>
			<p>Sample paragraph of text on your page.</p>

			<p>By observing the color and strength of light leaving the water, 
			satellite instruments can monitor the quality of the water, the presence 
			of marine organisms, especially plankton, and properties of water 
			associated with particular species.</p>

			<div style="text-indent: 0px;
				text-align: 0px; padding: 0px; float: right; display: table;"><img 
				alt="sample image placement on page"
				src="images/static/samplePic.jpg" /><br />
		  <em>The above image is style with 'class="bordered"'<br />
		  	which gives it a nice thin gray border.</em></div>

			<p style="float: left; width: 60%;">In the Chesapeake Bay, drainage of fresh water from rivers profoundly 
			affects the ecosystem of this normally brackish Bay. Sometimes plumes of 
			fresh water are associated with sediment washed from the land; the 
			sediment is rendered visible by a unique gray color which is observable 
			from space. At other times, river water exhibits a brown or orange tint 
			from dissolved material like decaying leaves or tannins. This tint can be 
			measured by an index known as "chromophoric dissolved organic matter 
			(CDOM)" – a measure of organic matter associated with the color of light 
			exiting the water.</p>


			<p style="float: left; width: 60%;">Between July 5 and 7, 2006, a team from the Marine Optical 
			Characterization Experiment (MOCE – organized by the Ocean Sensors Branch 
			of NESDIS-STAR) took samples from the upper portion of the Chesapeake Bay 
			near Baltimore and the Bay Bridge. The team documented a discharge of 
			fresh water having high amounts of sediment into the Bay, which then 
			advanced down the Bay following record-breaking rainfall in the Chesapeake 
			Bay watershed. This plume of fresh water was potentially devastating to 
			marine species. The team collected measurements of total suspended matter, 
			chlorophyll-a, CDOM, and the color of the light coming from the subsurface 
			water, to better enable the detection of fresh water and its associated 
			pollution in the future. The purpose of developing such satellite-based 
			techniques for observing the quality of the water is to maintain and 
			protect the health of the watershed ecosystem.</p>

			<div class="clearing"></div>
			
			<h3>Data Tables</h3>
			<p>If you are creating tables to display tabular data in your project
				pages, there are best practices for coding those tables. It is important
				to indicate which cells contain column headings and row labels, for example. You may
			copy the table code below and modify it for your purposes.</p>
			<p>When you create your own datatables, please always
				set the table attributes as follows:</p>

		<strong>&#60;table class="tableGrid" cellspacing="0" cellpadding="0" border="0"&#62;</strong>
		<br /><br />

		<p>Using 'class="tableGrid"' will give the table a thin gray outline, not the 
		ugly bevelled borders that we prefer not be used anywhere on STAR's web pages.</p>
			
			<table class="tableGrid" cellspacing="0" cellpadding="0" border="0">
				<tr>
					<td> </td>
					<th scope="col">Column Heading 1</th>
					<th scope="col">Column Heading 2</th>
					<th scope="col">Column Heading 3</th>
					<th scope="col">Column Heading 4</th>
				</tr>
				<tr>
					<th scope="row">Row Label A</th>
					<td>Data</td>
					<td>Data</td>
					<td>Data</td>
					<td>Data</td>
				</tr>
				<tr>
					<th scope="row">Row Label B</th>
					<td>Data</td>
					<td>Data</td>
					<td>Data</td>
					<td>Data</td>
				</tr>
				<tr>
					<th scope="row">Row Label C</th>
					<td>Data</td>
					<td>Data</td>
					<td>Data</td>
					<td>Data</td>
				</tr>
			</table>

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
