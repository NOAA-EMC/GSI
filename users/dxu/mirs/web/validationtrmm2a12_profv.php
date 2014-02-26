<?php include("password_protect.php"); ?>

<?php
// insert header that forbids caching and carries doctype
// and html tag;
require('includes/noCacheHeader.inc');
?>

<META name="verify-v1" content="CwbLBcFt9+GqRTgaLZsENmPnSWNB5MStHHdYsB7U2nI=">
<title>STAR - MIRS Project - Comparison to TRMM_2A12</title>
<?php
// style links and the icon link
// pulls in .css files for regular and print display
require('includes/styleLinks.inc');
?>

<?php

//$ROOT = $_SERVER['DOCUMENT_ROOT'];  
$ROOT='http://www.star.nesdis.noaa.gov/smcd/mirs';

$PHP_SELF="validationtrmm2a12_profv.php";
//$PHP_SELF=$_SERVER['PHP_SELF'];


$DIR_IMG="images/";
echo "<script> var NUM_IMG = new Array();  </script>\n";
$arr_sat  = array("n18", "n19", "metopA", "metopB", "f16", "f18", "npp");
$arr_prd  = array("clw", "iwp", "rwp");
$arr_sfc  = array("sea", "lnd" ); 
$arr_satpref = array( "n18" => "mirs_adv_poes_",
		      "n19" => "mirs_adv_poes_",
		      "metopA" => "mirs_adv_poes_",
		      "metopB" => "mirs_adv_poes_", 
		      "f16" => "mirs_adv_dmsp_",
		      "f18" => "mirs_adv_dmsp_",
		      "npp" => "mirs_adv_npoess_");

$trmm = "_trmm_2A12_coll_prof_" ;


$iday = 7;

$yesterday = mktime(0, 0, 0, date("m"), date("d")-7, date("Y"));
$yyyy = date("Y",$yesterday);
$mm   = date("m",$yesterday);
$dd   = date("d",$yesterday);

$sat='n18';  
$prod='clw';  
$sfc='sea';
$yr=$yyyy;   
$mo=$mm;   
$dy=$dd;   

if(isset($_POST['sat']))   { $sat   = $_POST['sat'];   }
if(isset($_POST['prod']))  { $prod  = $_POST['prod'];  }
if(isset($_POST['sfc']))   { $sfc   = $_POST['sfc'];   }
if(isset($_POST['yr']))    { $yr    = $_POST['yr'];    }
if(isset($_POST['mo']))    { $mo    = $_POST['mo'];    }
if(isset($_POST['dy']))    { $dy    = $_POST['dy'];    }

$satpref = $arr_satpref[$sat] ;

$arr_img = array();

$nfile=1;
for ($i = 1; $i <= 40 ; $i++) {
  $filename=$DIR_IMG.$sat."/".$yr."-".$mo."-".$dy."/".$satpref.$sat.$trmm.$prod."_".$sfc."_".strval($i).".png";
  //echo "<script> var filename = '$filename';  </script>\n";
  if (file_exists($filename)) {
    $arr_img[$nfile] = $filename;
    $nfile++;
    echo "<script> var filename = '$filename';  </script>\n";
  } 
}


//$url_n18    = $ROOT.'/'.$PHP_SELF.'?sat=n18&prod='.$prod.'&sfc='.$sfc.'&yr='.$yr.'&mo='.$mo.'&dy='.$dy;
//$url_metopA = $ROOT.'/'.$PHP_SELF.'?sat=metopA&prod='.$prod.'&sfc='.$sfc.'&yr='.$yr.'&mo='.$mo.'&dy='.$dy;
//$url_f16    = $ROOT.'/'.$PHP_SELF.'?sat=f16&prod='.$prod.'&sfc='.$sfc.'&yr='.$yr.'&mo='.$mo.'&dy='.$dy;
//$url_n19    = $ROOT.'/'.$PHP_SELF.'?sat=n19&prod='.$prod.'&sfc='.$sfc.'&yr='.$yr.'&mo='.$mo.'&dy='.$dy;
//$url_npp    = $ROOT.'/'.$PHP_SELF.'?sat=npp&prod='.$prod.'&sfc='.$sfc.'&yr='.$yr.'&mo='.$mo.'&dy='.$dy;

$url_n18    = $PHP_SELF.'?sat=n18&prod='.$prod.'&sfc='.$sfc.'&yr='.$yr.'&mo='.$mo.'&dy='.$dy;
$url_n19    = $PHP_SELF.'?sat=n19&prod='.$prod.'&sfc='.$sfc.'&yr='.$yr.'&mo='.$mo.'&dy='.$dy;
$url_metopA = $PHP_SELF.'?sat=metopA&prod='.$prod.'&sfc='.$sfc.'&yr='.$yr.'&mo='.$mo.'&dy='.$dy;
$url_metopB = $PHP_SELF.'?sat=metopB&prod='.$prod.'&sfc='.$sfc.'&yr='.$yr.'&mo='.$mo.'&dy='.$dy;
$url_f16    = $PHP_SELF.'?sat=f16&prod='.$prod.'&sfc='.$sfc.'&yr='.$yr.'&mo='.$mo.'&dy='.$dy;
$url_f18    = $PHP_SELF.'?sat=f18&prod='.$prod.'&sfc='.$sfc.'&yr='.$yr.'&mo='.$mo.'&dy='.$dy;
$url_npp    = $PHP_SELF.'?sat=npp&prod='.$prod.'&sfc='.$sfc.'&yr='.$yr.'&mo='.$mo.'&dy='.$dy;


//$url_prev   = $PHP_SELF.'?sat='.$sat.'&prod='.$prod.'&sfc='.$sfc.'&yr='.$yr.'&mo='.$mo.'&dy='.$dy;

echo "<script> var sat   = '$sat';   </script>\n";
echo "<script> var prod  = '$prod';  </script>\n";
echo "<script> var sfc   = '$sfc';   </script>\n";
echo "<script> var yr    = '$yr';    </script>\n";
echo "<script> var mo    = '$mo';    </script>\n";
echo "<script> var dy    = '$dy';    </script>\n";


?>



<script language="javascript" type="text/javascript" src="validationtrmm2a12_profv.js"></script>


</head>
<body onLoad="loadInitialImages(sat,prod,sfc,yr,mo,dy)">
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
			require('includes/Sample_NavDiv_validationtrmm2a12_profv.inc');
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


				<FORM NAME=form action="<?php echo $PHP_SELF;?>"  method=POST>
				<table border="0" cellpadding="0" cellspacing="0" class="tableGrid" bgcolor="#eeeeee">

				<TR>
				<TD align=center bgcolor="#eeeeee" height=18 COLSPAN=1>
				<input  type=submit  value="Map/Histogram" 
					onClick="document.form.action='validationtrmm2a12.php'" style="background-color: lightblue;"> &nbsp;
				<B><font size=4>MIRS Comparison to TRMM_2A12 Vertical Profiles</font></B>
				<input type="submit" value="Submit Query"  style="background-color: lightblue;">
				</TD>
				</TR>

				<TR title="Click light blue &#34;Submit Query&#34; button after making selection">
				<TD align=center>

				&nbsp;
				<select name="sat" title="Select a satellite sensor">
				<option value="n18"    >NOAA-18</option> 
				<option value="n19"    >NOAA-19</option> 
				<option value="metopA" >METOP-A</option> 
				<option value="metopB" >METOP-B</option> 
				<option value="f16"    >F16/SSMIS</option>	 
				<option value="f18"    >F18/SSMIS</option>	 
				<option value="npp"    >NPP/ATMS</option>
				<option value="gcomw1"	>GCOMW1/AMSR2</option>
				</select>
				
				&nbsp;
				<select id="prod" name="prod" title="Select a product">
				<option value="clw"	>Cloud Liquid Water</option>
				<option value="iwp"	>Ice Water Path</option>
				<option value="rwp"	>Rain Water Path</option>
				</select>			  
				
				&nbsp;
				<select id="sfc" name="sfc" title="Select a surface type">
				<option value="sea">Sea</option>
				<!--option value="lnd">Land</option-->
				</select>			  

				&nbsp;
				<select id="yr" name="yr" title="Select a year">
                                <option value="2011">2011</option> 
                                <option value="2012">2012</option> 
                                <option value="2013">2013</option> 
                                <option value="2014">2014</option> 
                                <option value="2015">2015</option> 
				</select>			  
				&nbsp;
				<select id="mo" name="mo" title="Select a month">	  
                                <option value="01">Jan</option>
                                <option value="02">Feb</option>
                                <option value="03">Mar</option>
                                <option value="04">Apr</option>
                                <option value="05">May</option>
                                <option value="06">Jun</option>
                                <option value="07">Jul</option>
                                <option value="08">Aug</option>
                                <option value="09">Sep</option>
                                <option value="10">Oct</option>
                                <option value="11">Nov</option>
                                <option value="12">Dec</option>
				</select>			  
				
				&nbsp;
				<select id="dy" name="dy" title="Select a day">	  
                                <option value="01">1</option>
                                <option value="02">2</option>
                                <option value="03">3</option>
                                <option value="04">4</option>
                                <option value="05">5</option>
                                <option value="06">6</option>
                                <option value="07">7</option>
                                <option value="08">8</option>
                                <option value="09">9</option>
                                <option value="10">10</option>
                                <option value="11">11</option>
                                <option value="12">12</option>
                                <option value="13">13</option>
                                <option value="14">14</option>
                                <option value="15">15</option>
                                <option value="16">16</option>
                                <option value="17">17</option>
                                <option value="18">18</option>
                                <option value="19">19</option>
                                <option value="20">20</option>
                                <option value="21">21</option>
                                <option value="22">22</option>
                                <option value="23">23</option>
                                <option value="24">24</option>
                                <option value="25">25</option>
                                <option value="26">26</option>
                                <option value="27">27</option>
                                <option value="28">28</option>
                                <option value="29">29</option>
                                <option value="30">30</option>
                                <option value="31">31</option>
				</select>			  

				&nbsp;
				<input type="button" onclick="rev();" value="<=" title="previous day">
				<input type="button" onclick="fwd();" value="=>" title="next day">
				
				</TD>
				</TR>


<?php
for ( $i = 1; $i < $nfile ; $i++ ) {
	echo "<TR>\n";
	echo "<TD align=center>\n";
	$str = "<a id=href" . strval($i) . " target=_blank>\n";
	echo $str;
	$str = "<img id=img" . strval($i) . " src=\"" . $arr_img[$i] . "\""  . " align=center width=650 height=500></a>\n";
	echo $str;
	echo "</TD>\n\n";
	echo "</TR>\n\n";
}
?>

				</table>
<?php
if( $nfile == 1 ) {
 	echo "<center><p><font color=red>Sorry, no images exist for those criteria. Please use different selection criteria.</font></p></center>" ;

}
?>
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
