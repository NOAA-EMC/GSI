<?php
// insert header that forbids caching and carries doctype
// and html tag;
require('includes/noCacheHeader.inc');
?>
<META name="verify-v1" content="CwbLBcFt9+GqRTgaLZsENmPnSWNB5MStHHdYsB7U2nI=">
<title>STAR - MIRS Project - Daily Map Time Series</title>
<?php
// style links and the icon link
// pulls in .css files for regular and print display
require('includes/styleLinks.inc');
?>


<style type="text/css">
  select.optionvisible   	{visibility:visible}
  select.optioninvisible 	{visibility:hidden}
</style>


<script language="javascript" type="text/javascript" src="comp.js"></script>



</head>
<body onLoad="init();launch();">

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
				
				<FORM name=form1>
				
				<table border="0" cellpadding="0" cellspacing="0" class="tableGrid" bgcolor="#eeeeee">
				<TR>
				<TD align=center height=500 nowrap>
				
				<b><font size=4>MIRS Composite Products</b></font>
				<br>


				<!-- ===============  parameter section ================ -->
				
				<select id="prod" name="prod"  onChange="changeProduct( this.value );" title="select a product">
<?php
require('includes/productOptions_comp.inc');
?>
				</select>

				<select id="layer" name="layer" class="optioninvisible" onChange="changeLayer( this.value )">
				</select>


				<select id="yr" name="yr" title="select a year"  onChange="loadImage();">
                                <option value="2011">2011</option> 
                                <option value="2012">2012</option> 
                                <option value="2013">2013</option> 
                                <option value="2014">2014</option> 
                                <option value="2016">2015</option> 
                                <option value="2015">2016</option> 
                                <option value="2017">2017</option> 
                                <option value="2018">2018</option> 
                                <option value="2019">2019</option> 
                                <option value="2020">2020</option> 
				</select>

				<select id="mo" name="mo" title="select a month" onChange="loadImage();">
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

				<select id="dy" name="dy" title="select a day" onChange="loadImage();">
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
				

                                <br>

				<input type=button value="Start" onClick="start()" title="Start a 31-day loop Animation" style="background-color: #77e7ae" ;">
				<input type=button value="Stop"  onClick="stop()"  title="Stop Animation"  style="background-color: #fc9999;">

				<input type=button value="Prev" onClick="decrementImage(--current_image)" title="Previous image">
				<input type=button value="Next" onClick="incrementImage(++current_image)" title="Next image">

				<input type=button value="Rev"  onClick="change_mode(1);rev()" title="Backward Direction">
				<input type=button value="Fwd"  onClick="change_mode(1);fwd()" title="Fowward Direction">

				<input type=button value="Slower" onClick="change_speed(delay_step)"  title="Slower Animation Speed">
				<input type=button value="Faster" onClick="change_speed(-delay_step)" title="Speed up Animation Speed">

				<BR><BR>


				<!-- ================ image section ================== -->
				<a id="href" href="" target="_blank" >
				<img name="animation" src=""  width=650 height=500 alt="" style="display:block; clear:both;" /></a>
				
				</TD>
				
				</TR>

				</TABLE>

				</FORM>				
				
                                <p>Composite is based on MiRS currently daily running satellite sensors: N18,N19,Metop-A,F16 SSMIS,F18 SSMIS and NPP ATMS.</p>

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
