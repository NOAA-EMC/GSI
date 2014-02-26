<?php
// insert header that forbids caching and carries doctype
// and html tag;
require('includes/noCacheHeader.inc');
?>
<META name="verify-v1" content="CwbLBcFt9+GqRTgaLZsENmPnSWNB5MStHHdYsB7U2nI=">
<title>STAR - MIRS Project - Bias Correction Monitoring</title>
<?php
// style links and the icon link
// pulls in .css files for regular and print display
require('includes/styleLinks.inc');
?>
<script language="javascript" type="text/javascript">

    function loadImage() {
     	
	YYYY='2007';
	
	var sat = document.form.sat.value;
	var chan = document.form.chan.value;
	
	var imgDir="/smcd/mirs/images/" + sat + "/";
	
 	var img1 = imgDir+"mirs_adv_poes_" + sat + "_biasmean_glb_"      + YYYY + '_tb_ch' + chan + ".png" ;
 	var img2 = imgDir+"mirs_adv_poes_" + sat + "_biasslope_glb_"     + YYYY + '_tb_ch' + chan + ".png" ;
 	var img3 = imgDir+"mirs_adv_poes_" + sat + "_biasintercept_glb_" + YYYY + '_tb_ch' + chan + ".png" ;
	
	document.form.img1.src = img1;
	document.form.img2.src = img2;
	document.form.img3.src = img3;
	
	document.form.img1.alt = img1;
	document.form.img2.alt = img2;
	document.form.img3.alt = img3;
	
    }


</script>

</head>

<body onLoad="loadImage()">
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
				
				
				<FORM NAME=form>

				<table border="0" cellpadding="0" cellspacing="0" class="tableGrid">

				<TR align=center>
				<TD align=center nowrap class="productTd">
				 <B><font size=4>MIRS Bias Monitoring</font></B>
				</TD>
				</TR>


				<TR><TD class="productTd" id="panel" align=center valign=top>Sensor:
				<select class="productSelect" ID="sat" NAME="sat" onChange="loadImage()" >
				<option VALUE="n18" 	>NOAA-18
				<option VALUE="metopA"	>METOP-A
				</select>


				&nbsp; &nbsp; Channel:
				<select id="chan" NAME="chan" onChange="loadImage()">
				<option VALUE=1 >1
				<option VALUE=2 >2
				<option VALUE=3 >3
				<option VALUE=4 >4
				<option VALUE=5 >5
				<option VALUE=6 >6
				<option VALUE=7 >7
				<option VALUE=8 >8
				<option VALUE=9 >9
				<option VALUE=10 >10
				<option VALUE=11 >11
				<option VALUE=12 >12
				<option VALUE=13 >13
				<option VALUE=14 >14
				<option VALUE=15 >15
				<option VALUE=16 >16
				<option VALUE=17 >17
				<option VALUE=18 >18
				<option VALUE=19 >19
				<option VALUE=20 >20
				</select>
				</TD>
				</TR>
				
				<TR><TD width=650 height=500 align=center>
				
				<a id="href1" href="" target=_blank >
				<img name="img1" src=""  alt="" ></a>

				</TD>
				</TR>


				<TR><TD width=650 height=500 align=center>
				
				<a id="href2" href="" target=_blank >
				<img name="img2" src=""  alt="" ></a>

				</TD>
				</TR>

				<TR><TD width=650 height=500 align=center>
				
				<a id="href3" href="" target=_blank >
				<img name="img3" src=""  alt="" ></a>

				</TD>
				</TR>



				</TABLE>
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
