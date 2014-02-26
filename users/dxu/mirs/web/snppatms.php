<?php
// insert header that forbids caching and carries doctype
// and html tag;
require('includes/noCacheHeader.inc');
?>
<META name="verify-v1" content="CwbLBcFt9+GqRTgaLZsENmPnSWNB5MStHHdYsB7U2nI=">
<title>STAR - MIRS Project - - Sensors - NPOESS ATMS</title>
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



				<h1>Sensors<br />
				SNPP ATMS Overview</h1>


				<table border="0" cellpadding="0" cellspacing="0" class="tableGrid">
				<caption>ATMS Channel Characteristics</caption>

				<tr> <th scope="col" >Channel</th> 
				<th scope="col" >Center Freq.(GHz)</th>
				<th scope="col" >Polarization</th>
				<th scope="col" >Max Bandwidth(GHz)</th> 
				<th scope="col" >Temperature Sensitivity(K) NE&#8710;T</th> 
				</tr>


				<tr> <th scope="row">1</th> <td align=center>23.8</td> 		     		  <td align=center>V</td>	<td align=center>0.27</td>  <td align=center>0.9 </td>  </tr> 
				<tr> <th scope="row">2</th> <td align=center>31.4</td> 		     		  <td align=center>V</td>	<td align=center>0.18</td>  <td align=center>0.9 </td>  </tr> 
				<tr> <th scope="row">3</th> <td align=center>50.3</td> 		     		  <td align=center>H</td>	<td align=center>0.18</td>  <td align=center>1.20</td>  </tr> 
				<tr> <th scope="row">4</th> <td align=center>51.76</td> 			  <td align=center>H</td>	<td align=center>0.40</td>  <td align=center>0.75</td>  </tr> 
				<tr> <th scope="row">5</th> <td align=center>52.8</td>  			  <td align=center>H</td>	<td align=center>0.40</td>  <td align=center>0.75</td>  </tr> 
				<tr> <th scope="row">6</th> <td align=center>53.596&#177;0.115</td>  	  	  <td align=center>H</td>	<td align=center>0.17</td>  <td align=center>0.75</td>  </tr>  		      
				<tr> <th scope="row">7</th> <td align=center>54.40</td> 			  <td align=center>H</td>	<td align=center>0.40</td>  <td align=center>0.75</td>  </tr> 
				<tr> <th scope="row">8</th> <td align=center>54.94</td> 			  <td align=center>H</td>	<td align=center>0.40</td>  <td align=center>0.75</td>  </tr> 
				<tr> <th scope="row">9 </th> <td align=center>55.50</td> 		    	  <td align=center>H</td>	<td align=center>0.33</td>  <td align=center>0.75</td>  </tr>
				<tr> <th scope="row">10</th> <td align=center>57.2903</td> 		    	  <td align=center>H</td>	<td align=center>0.33</td>  <td align=center>0.75</td>  </tr>
				<tr> <th scope="row">11</th> <td align=center>57.2903&#177;0.115</td> 	    	  <td align=center>H</td>	<td align=center>0.078</td> <td align=center>1.20</td>  </tr>
				<tr> <th scope="row">12</th> <td align=center>57.2903</td> 		    	  <td align=center>H</td>	<td align=center>0.036</td> <td align=center>1.20</td>  </tr>
				<tr> <th scope="row">13</th> <td align=center>57.2903&#177;0.322</td> 	    	  <td align=center>H</td>	<td align=center>0.016</td> <td align=center>1.50</td>  </tr>
				<tr> <th scope="row">14</th> <td align=center>57.2903&#177;0.322&#177;0.010</td>  <td align=center>H</td>	<td align=center>0.008</td> <td align=center>2.40</td>  </tr>
				<tr> <th scope="row">15</th> <td align=center>57.2903&#177;0.322&#177;0.004</td>  <td align=center>H</td>	<td align=center>0.003</td> <td align=center>3.60</td>  </tr>
				<tr> <th scope="row">16</th> <td align=center>87-91(88.20)</td> 		  <td align=center>V</td>	<td align=center> 2.0 </td> <td align=center>0.5 </td>  </tr>
				<tr> <th scope="row">17</th> <td align=center>165.5</td> 		  	  <td align=center>H</td>	<td align=center>3.0</td>   <td align=center>0.6</td>   </tr>
				<tr> <th scope="row">18</th> <td align=center>183.31&#177;7</td> 		  <td align=center>H</td> 	<td align=center>2.0</td>   <td align=center>0.8</td>   </tr>
				<tr> <th scope="row">19</th> <td align=center>183.31&#177;4.5</td> 	  	  <td align=center>H</td>	<td align=center>2.0</td>   <td align=center>0.8</td>   </tr>
				<tr> <th scope="row">20</th> <td align=center>183.31&#177;3</td> 		  <td align=center>H</td> 	<td align=center>1.0</td>   <td align=center>0.8</td>   </tr>
				<tr> <th scope="row">21</th> <td align=center>183.31&#177;1.8</td> 	  	  <td align=center>H</td>	<td align=center>1.0</td>   <td align=center>0.8</td>   </tr>
				<tr> <th scope="row">22</th> <td align=center>183.31&#177;1.0</td> 	  	  <td align=center>H</td>	<td align=center>0.5</td>   <td align=center>0.9</td>   </tr>

        		       </table>

				<br>

				<hr>
				<br />
					<p>Source: <a href="http://www.nesdis.noaa.gov/jpss/">NOAA JPSS</a></p>

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
