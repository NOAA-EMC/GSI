<?php include("password_protect.php"); ?>

<?php
// insert header that forbids caching and carries doctype
// and html tag;
require('includes/noCacheHeader.inc');
?>

<META name="verify-v1" content="CwbLBcFt9+GqRTgaLZsENmPnSWNB5MStHHdYsB7U2nI=">
<title>STAR - MIRS Project - Comparison to Rain Radar</title>
<?php
// style links and the icon link
// pulls in .css files for regular and print display
require('includes/styleLinks.inc');
?>

<style type="text/css">

  select.optionvisible   {visibility:visible}
  select.optioninvisible {visibility:hidden}

  td.arrowvisible 	{visibility:visible}
  td.arrowinvisible 	{visibility:hidden} 
  
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
			require('includes/Sample_NavDiv_validationrainradarv.inc');
			?>
		</td>
		<td class="mainPanel"><a name="skipTarget"></a>
		
		<noscript><h3 class="noscriptWarning">
		Javascript is currently disabled on this computer. 
		Product animations on this page are generated with javascript.
		If you need static access to these images, please contact MIRS webmaster.</h3>
		</noscript>
		
			<div class="padding" id="monitor">
				<!-- DO NOT DELETE OR ALTER CODE ABOVE THIS COMMENT -->
				<!-- EXCEPT for the contents of the <title></title> TAG!! -->
				<!-- You can start project specific content HERE -->


				<FORM NAME=form action="validationrainradar.php" method="post">

				<table border="0" cellpadding="0" cellspacing="0" class="tableGrid" bgcolor="#eeeeee">

				<TR><TD align=center bgcolor="#eeeeee" height=18>
				<input  type=submit  value="2 Column Panel" style="background-color: lightblue" 
					title="2 Column Panel Layout with smaller Images"> &nbsp;&nbsp;
				<B><font size=4>MIRS Comparison to Rain Radar</font></B>
				</TD></TR>


				<TR>
				  <TD align=center nowrap width=650 height=500>
				    <a id="href1" href="images/ipwg/ipwg_time_series_MW_Radar_BIA.png" target=_blank>
				    <img name="img1" src="images/ipwg/ipwg_time_series_MW_Radar_BIA.png" 
				    width=650 height=500 alt="" title="Bias"></a>
				  </TD>
				</TR>
				
				<TR>
				  <TD align=center nowrap width=650 height=500>
				    <a id="href2" href="images/ipwg/ipwg_time_series_MW_Radar_BIR.png" target=_blank>
				    <img name="img2" src="images/ipwg/ipwg_time_series_MW_Radar_BIR.png"
				    width=650 height=500 alt="" title="Bias Ratio"></a>
				  </TD>
				</TR>

				<TR>
				  <TD align=center nowrap width=650 height=500>
				    <a id="href3" href="images/ipwg/ipwg_time_series_MW_Radar_COR.png" target=_blank>
				    <img name="img3" src="images/ipwg/ipwg_time_series_MW_Radar_COR.png" 
				    width=650 height=500 alt="" title="Correlation"></a>
				  </TD>
				</TR>
				
				<TR>
				  <TD align=center nowrap width=650 height=500>
				    <a id="href4" href="images/ipwg/ipwg_time_series_MW_Radar_ETS.png" target=_blank>
				    <img name="img4" src="images/ipwg/ipwg_time_series_MW_Radar_ETS.png"
				    width=650 height=500 alt="" title="Equitable Threat Score"></a>
				  </TD>
				</TR>

				<TR>
				  <TD id="box5" align=center nowrap width=650 height=500>
				    <a id="href5" href="images/ipwg/ipwg_time_series_MW_Radar_FAR.png" target=_blank>
				    <img name="img3" src="images/ipwg/ipwg_time_series_MW_Radar_FAR.png" 
				    width=650 height=500 alt="" title="False Alarm Rate"></a>
				  </TD>
				</TR>
				
				<TR>
				  <TD id="box6" align=center nowrap width=650 height=500>
				    <a id="href6" href="images/ipwg/ipwg_time_series_MW_Radar_HSS.png" target=_blank>
				    <img name="img6" src="images/ipwg/ipwg_time_series_MW_Radar_HSS.png"
				    width=650 height=500 alt="" title="Heidke Skill Score"></a>
				  </TD>
				</TR>

				<TR>
				  <TD id="box7" align=center nowrap width=650 height=500>
				    <a id="href7" href="images/ipwg/ipwg_time_series_MW_Radar_MAE.png" target=_blank>
				    <img name="img7" src="images/ipwg/ipwg_time_series_MW_Radar_MAE.png"
				    width=650 height=500 alt="" title="Mean Absolute Error"></a>
				  </TD>
				</TR>
				
				<TR>
				  <TD id="box8" align=center nowrap width=650 height=500>
				    <a id="href8" href="images/ipwg/ipwg_time_series_MW_Radar_MEANRRA.png" target=_blank>
				    <img name="img8" src="images/ipwg/ipwg_time_series_MW_Radar_MEANRRA.png"
				    width=650 height=500 alt="" title="Mean Rain Rate"></a>
				  </TD>
				</TR>

				<TR>
				  <TD id="box9" align=center nowrap width=650 height=500>
				    <a id="href9" href="images/ipwg/ipwg_time_series_MW_Radar_POD.png" target=_blank>
				    <img name="img9" src="images/ipwg/ipwg_time_series_MW_Radar_POD.png"
				    width=650 height=500 alt="" title="Probability of Detection"></a>
				  </TD>
				</TR>
				
				<TR>
				  <TD id="boxa" align=center nowrap width=650 height=500>
				    <a id="hrefa" href="images/ipwg/ipwg_time_series_MW_Radar_POFD.png" target=_blank>
				    <img name="imga" src="images/ipwg/ipwg_time_series_MW_Radar_POFD.png"
				    width=650 height=500 alt="" title="False Alarm Rate"></a>
				  </TD>
				</TR>

				<TR>
				  <TD id="boxb" align=center nowrap width=650 height=500>
				    <a id="hrefb" href="images/ipwg/ipwg_time_series_MW_Radar_RMSE.png" target=_blank>
				    <img name="imgb" src="images/ipwg/ipwg_time_series_MW_Radar_RMSE.png"
				    width=650 height=500 alt="" title="Root Mean Square Error"></a>
				  </TD>
				</TR>
				
				</table>

				</FORM>


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
