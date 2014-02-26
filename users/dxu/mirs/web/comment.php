<?php
// insert header that forbids caching and carries doctype
// and html tag;
require('includes/noCacheHeader.inc');
?>
<META name="verify-v1" content="CwbLBcFt9+GqRTgaLZsENmPnSWNB5MStHHdYsB7U2nI=">
<title>STAR - Microwave Integrated Retrieval System (MIRS)</title>
<?php
// style links and the icon link
// pulls in .css files for regular and print display
require('includes/styleLinks.inc');
?>

<!-- if you need to include your own .css files
     put the links HERE, before the closing head tag. 
-->


<SCRIPT language="javascript" type="text/javascript">

function checkform() {
  if ( document.form.firstname.value == "" ) {
    alert ( "Please enter first name field.");
    return false;
  }
  else if ( document.form.lastname.value == "" ) {
    alert ( "Please enter last name field.");
    return false;
  }
  else if ( document.form.from.value == "" ) {
    alert ( "Please enter email.");
    return false;
  }
  else if ( !document.form.license[0].checked && !document.form.license[1].checked ) {
    alert ( "Please check license agreement.");
    return false;
  }

  if( document.form.license[1].checked ) {
    alert ( "Sorry, since you don't accept license agreement terms, we can not give you permission to use MIRS.");
    return false;
  }
  
  return true;
}

</SCRIPT>



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

                                <h1><center>MIRS DAP Password Request Form</center></h1>

                                <div style="margin: auto; text-align: left;">
				<HR>
				<FORM name="form" METHOD="POST" ACTION="comment_action.php" onsubmit="return checkform();">
				
				<PRE>
				<B>
			First Name: <INPUT name="firstname" TYPE=text size=25 /><p>	
			Last Name:  <INPUT name="lastname"  TYPE=text size=25 /><p>
			Your Email: <INPUT name="from"      TYPE=text size=25 /><p>	
			Your message to MIRS Team: <br>
			<TEXTAREA NAME="msg" COLS=80 ROWS=4 WRAP=soft></TEXTAREA><P>
				</B>
				</PRE>
				
				<center>
				<h3>License Agreement for the Use of MIRS</h3>
				<p>
				...
				blah, blah, blah, license Articles here one by one.
				...<br>
				...<br>
				</p>
				<a href="mirs_license.doc">MIRS License (word doc)</a><br>
				
				
			<INPUT TYPE="radio" NAME="license" VALUE="0" > I accept Terms in License Agreement<br>
			<INPUT TYPE="radio" NAME="license" VALUE="1" > I deny Terms in License Agreement &nbsp;&nbsp;<br><br>
				
			<INPUT TYPE=submit VALUE="Submit"> &nbsp; &nbsp;  <INPUT TYPE=reset VALUE="Clear">

				</center>
				</FORM>

				
				
                                </div>


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
