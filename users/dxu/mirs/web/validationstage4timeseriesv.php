<?php include("password_protect.php"); ?>

<?php
// insert header that forbids caching and carries doctype
// and html tag;
require('includes/noCacheHeader.inc');
?>

<META name="verify-v1" content="CwbLBcFt9+GqRTgaLZsENmPnSWNB5MStHHdYsB7U2nI=">
<title>STAR - MIRS Project - MIRS Comparison to Stage IV Time Series of Scores</title>
<?php
// style links and the icon link
// pulls in .css files for regular and print display
require('includes/styleLinks.inc');
?>


<script language="javascript" type="text/javascript">

var DIR_IMG = 'images/st4/ts/st4_';
var PREFIX  = '_time_series_conus_lnd_';

function loadImages() {
	
	var scale  = document.form.scale.value;
	var sensor = document.form.sensor.value;
	
	var img1 = DIR_IMG + sensor + PREFIX + scale + 'bia.png';
	var img2 = DIR_IMG + sensor + PREFIX + scale + 'cor.png';
	var img3 = DIR_IMG + sensor + PREFIX + scale + 'dfr.png';
	var img4 = DIR_IMG + sensor + PREFIX + scale + 'far.png';
	var img5 = DIR_IMG + sensor + PREFIX + scale + 'fcn.png';
	var img6 = DIR_IMG + sensor + PREFIX + scale + 'foh.png';
	var img7 = DIR_IMG + sensor + PREFIX + scale + 'fom.png';
	var img8 = DIR_IMG + sensor + PREFIX + scale + 'hss.png';
	var img9 = DIR_IMG + sensor + PREFIX + scale + 'mae.png';
	var imga = DIR_IMG + sensor + PREFIX + scale + 'pfd.png';
	var imgb = DIR_IMG + sensor + PREFIX + scale + 'pod.png';
	var imgc = DIR_IMG + sensor + PREFIX + scale + 'pon.png';
	var imgd = DIR_IMG + sensor + PREFIX + scale + 'rms.png';
	var imge = DIR_IMG + sensor + PREFIX + scale + 'corln.png';
	//var imgf = DIR_IMG + sensor + PREFIX + scale + 'corln_hr.png';

	document.form.img1.src = img1;
	document.form.img2.src = img2;
	document.form.img3.src = img3;
	document.form.img4.src = img4;
	document.form.img5.src = img5;
	document.form.img6.src = img6;
	document.form.img7.src = img7;
	document.form.img8.src = img8;
	document.form.img9.src = img9;
	document.form.imga.src = imga;
	document.form.imgb.src = imgb;
	document.form.imgc.src = imgc;
	document.form.imgd.src = imgd;
	document.form.imge.src = imge;
	//document.form.imgf.src = imgf;

	document.form.img1.alt = img1;
	document.form.img2.alt = img2;
	document.form.img3.alt = img3;
	document.form.img4.alt = img4;
	document.form.img5.alt = img5;
	document.form.img6.alt = img6;
	document.form.img7.alt = img7;
	document.form.img8.alt = img8;
	document.form.img9.alt = img9;
	document.form.imga.alt = imga;
	document.form.imgb.alt = imgb;
	document.form.imgc.alt = imgc;
	document.form.imgd.alt = imgd;
	document.form.imge.alt = imge;
	//document.form.imgf.alt = imgf;

	var hr_img1 = DIR_IMG + sensor + PREFIX + scale + 'bia_hr.png';
	var hr_img2 = DIR_IMG + sensor + PREFIX + scale + 'cor_hr.png';
	var hr_img3 = DIR_IMG + sensor + PREFIX + scale + 'dfr_hr.png';
	var hr_img4 = DIR_IMG + sensor + PREFIX + scale + 'far_hr.png';
	var hr_img5 = DIR_IMG + sensor + PREFIX + scale + 'fcn_hr.png';
	var hr_img6 = DIR_IMG + sensor + PREFIX + scale + 'foh_hr.png';
	var hr_img7 = DIR_IMG + sensor + PREFIX + scale + 'fom_hr.png';
	var hr_img8 = DIR_IMG + sensor + PREFIX + scale + 'hss_hr.png';
	var hr_img9 = DIR_IMG + sensor + PREFIX + scale + 'mae_hr.png';
	var hr_imga = DIR_IMG + sensor + PREFIX + scale + 'pfd_hr.png';
	var hr_imgb = DIR_IMG + sensor + PREFIX + scale + 'pod_hr.png';
	var hr_imgc = DIR_IMG + sensor + PREFIX + scale + 'pon_hr.png';
	var hr_imgd = DIR_IMG + sensor + PREFIX + scale + 'rms_hr.png';
	var hr_imge = DIR_IMG + sensor + PREFIX + scale + 'corln_hr.png';


	
	if( scale != '30d_' ) {
		document.getElementById("href1").href = hr_img1 ;
		document.getElementById("href2").href = hr_img2 ;
		document.getElementById("href3").href = hr_img3 ;
		document.getElementById("href4").href = hr_img4 ;
		document.getElementById("href5").href = hr_img5 ;
		document.getElementById("href6").href = hr_img6 ;
		document.getElementById("href7").href = hr_img7 ;
		document.getElementById("href8").href = hr_img8 ;
		document.getElementById("href9").href = hr_img9 ;
		document.getElementById("hrefa").href = hr_imga ;
		document.getElementById("hrefb").href = hr_imgb ;
		document.getElementById("hrefc").href = hr_imgc ;
		document.getElementById("hrefd").href = hr_imgd ;
		document.getElementById("hrefe").href = hr_imge ;
	}
	else {
		document.getElementById("href1").href = img1 ;
		document.getElementById("href2").href = img2 ;
		document.getElementById("href3").href = img3 ;
		document.getElementById("href4").href = img4 ;
		document.getElementById("href5").href = img5 ;
		document.getElementById("href6").href = img6 ;
		document.getElementById("href7").href = img7 ;
		document.getElementById("href8").href = img8 ;
		document.getElementById("href9").href = img9 ;
		document.getElementById("hrefa").href = imga ;
		document.getElementById("hrefb").href = imgb ;
		document.getElementById("hrefc").href = imgc ;
		document.getElementById("hrefd").href = imgd ;
		document.getElementById("hrefe").href = imge ;
	}
	
}

</script> 


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


				<FORM NAME=form action="validationstage4.php" method="post">

				<table border="0" cellpadding="0" cellspacing="0" class="tableGrid" bgcolor="#eeeeee">

				<TR>
				<TD align=center bgcolor="#eeeeee" height=18>
				
				<input type=submit  value="Histogram/Distribution" title="go to Histogram/Distribution Page" style="background-color: lightblue;">
				&nbsp; &nbsp; &nbsp; &nbsp; 
				<B><font size=4>MIRS Comparison to Stage IV Time Series of Scores</font></B> <br />
				
				<select id="scale" name="scale" title="choose a time scale" onChange="loadImages();">
					<option value="" >Sensor Life</option> 
					<option value="30d_"  >30-day</option> 
				</select>&nbsp; &nbsp;
				
				<select id="sensor" name="sensor" title="choose datesets to compare" onChange="loadImages();">
					<option value="mirs" 	  >MIRS vs Stage IV</option> 
					<option value="mirsmspps" >MIRS/MSPPS vs Stage IV</option> 
				</select>
				
				</TD>
				</TR>



				<TR>
				  <TD align=center nowrap width=650 height=500>
				    <a id="href1" href="images/st4/ts/st4_mirs_time_series_conus_lnd_bia_hr.png" target=_blank>
				    <img name="img1" src="images/st4/ts/st4_mirs_time_series_conus_lnd_bia.png" 
				    align=center  width=650 height=500 alt=""></a>
				  </TD>
				</TR>

				<TR>
				  <TD align=center nowrap width=650 height=500>
				    <a id="hrefe" href="images/st4/ts/st4_mirs_time_series_conus_lnd_corln.png" target=_blank>
				    <img name="imge" src="images/st4/ts/st4_mirs_time_series_conus_lnd_corln.png" 
				    align=center  width=650 height=500 alt=""></a>
				  </TD>
				</TR>

				
				<TR>
				  <TD align=center nowrap width=650 height=500>
				    <a id="href2" href="images/st4/ts/st4_mirs_time_series_conus_lnd_cor_hr.png" target=_blank>
				    <img name="img2" src="images/st4/ts/st4_mirs_time_series_conus_lnd_cor.png"
				    align=center width=650 height=500 alt=""></a>
				  </TD>
				</TR>

				<TR>
				  <TD align=center nowrap width=650 height=500>
				    <a id="href3" href="images/st4/ts/st4_mirs_time_series_conus_lnd_dfr_hr.png" target=_blank>
				    <img name="img3" src="images/st4/ts/st4_mirs_time_series_conus_lnd_dfr.png" 
				    align=center width=650 height=500 alt=""></a>
				  </TD>
				  
				</TR>
				
				<TR>
				  <TD align=center nowrap width=650 height=500>
				    <a id="href4" href="images/st4/ts/st4_mirs_time_series_conus_lnd_far_hr.png" target=_blank>
				    <img name="img4" src="images/st4/ts/st4_mirs_time_series_conus_lnd_far.png"
				    align=center width=650 height=500 alt=""></a>
				  </TD>

				</TR>

				<TR>
				  <TD align=center nowrap width=650 height=500>
				    <a id="href5" href="images/st4/ts/st4_mirs_time_series_conus_lnd_fcn_hr.png" target=_blank>
				    <img name="img5" src="images/st4/ts/st4_mirs_time_series_conus_lnd_fcn.png"
				    align=center width=650 height=500 alt=""></a>
				  </TD>
				</TR>
				
				<TR>
				  <TD align=center nowrap width=650 height=500>
				    <a id="href6" href="images/st4/ts/st4_mirs_time_series_conus_lnd_foh_hr.png" target=_blank>
				    <img name="img6" src="images/st4/ts/st4_mirs_time_series_conus_lnd_foh.png"
				    align=center width=650 height=500 alt=""></a>
				  </TD>
				</TR>
				
				<TR>
				  <TD align=center nowrap width=650 height=500>
				    <a id="href7" href="images/st4/ts/st4_mirs_time_series_conus_lnd_fom_hr.png" target=_blank>
				    <img name="img7" src="images/st4/ts/st4_mirs_time_series_conus_lnd_fom.png"
				    align=center width=650 height=500 alt=""></a>
				  </TD>
				</TR>
				
				<TR>
				  <TD align=center nowrap width=650 height=500>
				    <a id="href8" href="images/st4/ts/st4_mirs_time_series_conus_lnd_hss_hr.png" target=_blank>
				    <img name="img8" src="images/st4/ts/st4_mirs_time_series_conus_lnd_hss.png"
				    align=center width=650 height=500 alt=""></a>
				  </TD>
				</TR>
				
				<TR>
				  <TD align=center nowrap width=650 height=500>
				    <a id="href9" href="images/st4/ts/st4_mirs_time_series_conus_lnd_mae_hr.png" target=_blank>
				    <img name="img9" src="images/st4/ts/st4_mirs_time_series_conus_lnd_mae.png"
				    align=center width=650 height=500 alt=""></a>
				  </TD>
				</TR>
				
				<TR>
				  <TD align=center nowrap width=650 height=500>
				    <a id="hrefa" href="images/st4/ts/st4_mirs_time_series_conus_lnd_pfd_hr.png" target=_blank>
				    <img name="imga" src="images/st4/ts/st4_mirs_time_series_conus_lnd_pfd.png"
				    align=center width=650 height=500 alt=""></a>
				  </TD>
				</TR>
				

				<TR>
				  <TD align=center nowrap width=650 height=500>
				    <a id="hrefb" href="images/st4/ts/st4_mirs_time_series_conus_lnd_pod_hr.png" target=_blank>
				    <img name="imgb" src="images/st4/ts/st4_mirs_time_series_conus_lnd_pod.png"
				    align=center width=650 height=500 alt=""></a>
				  </TD>
				</TR>
				
				<TR>
				  <TD align=center nowrap width=650 height=500>
				    <a id="hrefc" href="images/st4/ts/st4_mirs_time_series_conus_lnd_pon_hr.png" target=_blank>
				    <img name="imgc" src="images/st4/ts/st4_mirs_time_series_conus_lnd_pon.png"
				    align=center width=650 height=500 alt=""></a>
				  </TD>
				</TR>
				
				<TR>
				  <TD align=center nowrap width=650 height=500>
				    <a id="hrefd" href="images/st4/ts/st4_mirs_time_series_conus_lnd_rms_hr.png" target=_blank>
				    <img name="imgd" src="images/st4/ts/st4_mirs_time_series_conus_lnd_rms.png"
				    align=center width=650 height=500 alt=""></a>
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
