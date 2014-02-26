<?php
// insert header that forbids caching and carries doctype
// and html tag;
require('includes/noCacheHeader.inc');
?>
<META name="verify-v1" content="CwbLBcFt9+GqRTgaLZsENmPnSWNB5MStHHdYsB7U2nI=">
<title>STAR Website Development Kit - put your project specific page title here</title>
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
			
				<table border="0" cellpadding="8" cellspacing="0"
				style="width: 100%;"
				summary="this table is for page layout purposes only, not tabular data">
					<tr>
						<td width="50%">
				<h1>This is a plain H1 heading</h1>
				<h2>This is a plain H2 heading</h2>
				<h3>This is a plain H3 heading</h3>

						<p>Welcome to the STAR website development kit home page.
						The STAR site is coded in PHP and relies upon 'includes' to 
						reuse commonly called content items for simplicity of maintenance.</p>
						<p>Our hope is that this bundle of files can form the basis
							of your own project site. Then we save you the
							effort of designing and developing reliable and extensible
							content containers and give STAR's projects a clear, navigable
							look and feel that complies with government laws
							regarding the accessibility of web content to users who have
							disabilities.</p>
		
							<p>In this bundle of files you will find the following items:</p>
						<ul>
							<li><strong>index.php</strong> - this page is a template for most of the site
								pages you will be producing, as well as containing instructions
								about how to use the templates. Copy it to a new filename and 
							then use the filename index.php as your home page file name.</li>
							<li><strong><a href="FullWidthPageStyle.php">FullWidthPageStyle.php</a></strong> - This is a template
								for a page layout that omits the navigation at left to create
								a wider content area. It maintains navigational clarity to
								the rest of your site via a breadcrumb trail at the top of the page.
								If you are unfamiliar with how to build and maintain breadcrumb
								navigation, see me for assistance.</li>
						</ul>
						<p>All of your site's pages should reside in the top level directory
								of the site and be named with a .php extension. The STAR webserver
								is set up to use PHP4. If you are working from a different webserver
								than the orbit / orbit2 servers, please see me to investigate the 
							availability of PHP on your server.</p>

						<h3>Directories in the site template kit:</h3>
						<ul>
							<li><h3>'css' directory:</h3>
								<p>This directory contains the 
								stylesheets for the online display of your project 
								pages AND a stylesheet for printing that omits navigation 
								and	makes the page printer-friendly. The stylesheets establish
								page layouts, set resizable type sizes, and defines the style
								of all the basic tags -- headlines, paragraphs, lists, and tables.</p>
								<p>I encourage you to use the styles you see in the stylesheet, and 
								can instruct you in more detail about how to get the most out
								of the work that has already been done on these sheets. The CSS 
								files for the site also are designed to minimize problems caused by
								viewing files in different browsers.</p></li>
							<li><h3>'images' directory:</h3>
								<p>Subdirectory for images. Inside this folder is a folder called 'banner'
								that contains banner images, please don't change or delete any of these.</p>
								<p>Inside of the image directory, it is a good idea to create
									subdirectories to sort the images you  use on
									your site into topical directories, particularly if you're going 
									serve lots of images, or have images that will change frequently.</p></li>
							<li><h3>'documents' directory:</h3>
								<p>If you need to put academic papers, presentations, and other
									static non-HTML formatted content on your project's site, 
								put that content into this directory. </p>
							<li><h3>'js' directory:</h3>
								<p>This folder contains javascripts used on the site. The only one
								that is required on every page is the file called <strong>menuControllerPHP.js</strong>.
								The main page template includes this file at the bottom, and it should
							appear on every page that	contains the navigation panel. </p>
							<p>This script makes the navigation panel expand and contract to expose lower
								level menus and also generates the red 'arrows' that show what page in
								the navigation panel is currently active. If javascript is not enabled
								on a user's machine, the navigation menu is displayed fully expanded at 
								no loss of access or functionality to users.</p></li>
							<li><h3>'includes' directory:</h3>
								<p>The includes directory contains the following files:</p>
								<ul>
									<li><strong>banner.inc</strong> - The banner contains the site's banner and logo,
									both print and screen versions. All pages use this include.</li>
									<li><strong>footer.inc</strong> - footer.inc contains the site's footer --
									the gray bar at the bottom with links to other government sites, a mailto: link for 
									the STAR webmaster, privacy, accessibility, and product disclaimers. It also
									contains a snippet of php that renders the date that the page was last changed.
									All STAR pages use this include.</li>
									<li><strong>noCacheHeader.inc</strong> - this file renders a piece of PHP code
									that forces each page loaded to refresh itself fully from the server, and it also
									contains the page's HTML doctype statement. All STAR pages use this include.</li>
									<li><strong>noScriptWarning.inc</strong> - This include renders the following text on 
									the	screen when user's have javascript disabled: <em style="color: navy;">Javascript 
									is currently disabled on this computer. Without javascript, some display enhancements 
									do not work; however, all content is fully visible and accessible.</em> It is important
									to minimize the extent to which your project site is dependent on javascript for access
									to content. </li>
									<li><strong>ProductDisclaimer.inc</strong> - This include appears
									at the bottom of the navigation panel. <br />
									<img src="images/static/ProductDisclaimerText.gif" alt="product disclaimer block"
									style="padding: 10px; float: right;" 
									class="bordered" /><br />
									This language is important because we are required to make it clear to site visitors
									that we aren't set up for operational levels of support for the products
									under development.</li>
									<li><strong>Sample_NavDiv.inc</strong> - This include file contains the code for
									the left hand navigation panel. It is coded as an unordered list ( &lt;ul&gt; )
									with special style classes and IDs that, along with the javascript file 
									<strong>menuControllerPHP.js</strong>, create the expanding / contracting 
									behavior of the list. I encourage you to consult with me to
									develop a logical navigation tree structure and labeling, and we can then
									edit this file to reflect the organization and content of your project's site
									and the file names you use for each page.</li>
									<li><strong>styleLinks.inc</strong> - This include pulls in the cascading style
									sheet files for each page and links in the NOAA favicon file. This include must 
									be used on all site pages. If you need to create a .css file of your own to define
									locally used styles, call it AFTER the this file is called (lines 8-12 in this file).</li>
									<li><strong>toolbar_withSearch.inc</strong> - This include file contains the 
									gray bar under the banner with links to the STAR home page, search, and other links. 
									It appears on all pages, and it is best not to edit it if possible.</li>
								</ul>
							</li>
						</ul>
						</td>

						<td width="50%">
							<h2>Image randomizer</h2>
							<p>To use the image display randomizing script supplied with this site template, 
							just edit the file '<strong>js/imageRandomizer.js</strong>' to reference your images, 
							which should be copied to the directory <strong>images/random</strong>.
							Also edit the variable strings in the javascript to give your images a proper caption. 
							To make the page not jump around,	be sure that the different images you use are close to the same size.</p>
							<div style="text-align: center;"><script language="javascript" type="text/javascript" src="js/imageRandomizer.js"></script></div>
							<noscript>
								<h3>Random images</h3>
								<img src='images/random/RandomImage1.jpg' 
								alt='alt text for Random Image 1' />
							</noscript>
							<hr size="1" noshade="noshade" />
							
							<h2>Guidelines</h2>
							<ul>
								<li>Don't set arbitrary widths for your webpage content. The page
									structure this site gives you is a fixed 750px, next to the navigation
									panel at left. If you really really need it wider, there is a
									navigation-free version of this page that is 950px.
								<a href="FullWidthPageStyle.php">This is the wide version of the page</a></li>
								</li>
								<li>Every single web page you create needs to have a unique title inside the 
								<strong>&lt;title&gt;</strong> tags inside the page header.</li>

								<li>Don't use frames. If you think you need frames, ask me
									and we'll come up with a workaround, possibly an iframe.
									Frames are inaccessible, among other problems.</li>

								<li>Try not to make access to important parts of your website
									dependent on javascript. The navigation for the STAR site
									uses javascript to expand and contract the navigation menu, but
									if a user disables javascript in their browser, the menu
								doesn't break, it just is displayed in fully expanded form.</li>
	
								<li>Don't use font tags</li>

								<li>Avoid all caps in your text content. Headings should use initial capitalization.</li>

								<li>All documents that you want to provide on your page must be saved
									as PDF files. Don't put MS Word or PowerPoint files in native format
									out on the public server. It's not good web manners to assume
									that the general public has Microsoft software; Adobe Reader is
									free, and Acrobat files are generally smaller than Word or PowerPoint.</li>

								<li>Filenames should have NO SPACES in them. If you know this already, I apologize;
								if you don't, please don't do it!</li>

								<li>Please always enclose content in some kind of tags -- paragraphs, lists, whatever.
								Don't just plunk text on the page with no enclosing tags. 
								When in doubt, use <strong>&lt;p&gt;</strong> tags.</li>
								<li>All html code tags should be lower case.</li>
								<li>All text should have exactly one space after each period. Not two. Not a tab. Not
								three. But ONE space.</li>
								<li>Don't add full justification to pages on the site. Left justification 
								with a ragged right margin is the standard for STAR pages. </li>
								<li>Don't manually format your content using color, bold, etc. The 
								stylesheet for this website template has colors and type sizes built
								into it. If you want to do something that you don't see supplied here, 
								ask me and I'll help you.</li>
								<li>If you want to create vertical empty space in your pages, use inline styles to
								set margins or padding or &lt;br&gt; tags. Do NOT create empty paragraph 
								tags that look like this in your code: &lt;p&gt; &lt;/p&gt;. </li>
						</ul>

					   	<h2>Guidelines for coding images in your pages</h2>
				 			<p>When you code images, please always:</p>
				 			<ul class="contentList">
				 				<li>Include an alt attribute on every single image. Inside the image tag, type 'alt=' and 
				 				enclose a brief description of WHAT the image is in the quotes that follow the alt=.</li>
				 				<li>RESIZE them in something like photoshop, as opposed to using the browser to
				 				resize a too-large image.</li>
				 			<li>Put a meaningful caption above or below it, inside an &lt;h3&gt; or &lt;h4&gt; tag, as appropriate.
				 				What is it a picture of? What is the date of the image? Why is it worth looking at?</li>
				 			<li>Images should in most cases have the statement 'class="bordered"' inside of the &lt;img&gt; tag;
				 			this puts a nice thin gray border around each image. </li>
				 		</ul>

						<h2>Using and coding lists</h2>
						<ul>
							<li>Unordered lists are a great thing! Please use them. Their style is defined
								in the main stylesheet so that they don't indent crazy far and so that a
							nice blue list marker is used.</li>
						</ul>
							<h2>Recommendations for coding links</h2>
							<ul>
								<li>Please don't open a new window for links except for in
									the following cases:
									<ul>
										<li>The link points to a non-government website. In this case, you should
											also add a title attribute to your &lt;a&gt; tag that says
											<strong>'title="this link points to a non-government website."'</strong></li>
										<li>The link opens a file in a format other than html, such as a PDF or an image (JPG, PNG, GIF).
											In those cases, you SHOULD use the link to open a new window
											and after the link, provide an explanation of what format the file is and
											how big it is (Example: "<a href="http://nrc.noaa.gov/Docs/NOAA_5-Year_Research_Plan_010605.pdf"
											target="_blank">Five-Year Research Plan</a> (PDF, 304 KB)"</li>
									</li>
								<li>Don't use javascript to open new windows. Instead, use 'target="_blank"' inside
								the anchor tag.</li>
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
