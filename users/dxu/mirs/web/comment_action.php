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


<?php

if(isset($_POST['firstname'])) { $firstname = $_POST['firstname']; }
if(isset($_POST['lastname']))  { $lastname  = $_POST['lastname'];  }
if(isset($_POST['from']))      { $from      = $_POST['from'];      }
if(isset($_POST['msg']))       { $msg       = $_POST['msg'];       }

$to = "Wanchun.Chen@noaa.gov,Sid.Boukabara@noaa.gov,Kevin.Garrett@noaa.gov";
//$to = "Wanchun.Chen@noaa.gov,wanchen@atmos.umd.edu";

$headers = "From: " . $firstname . " " . $lastname . "<" . $from . ">\r\n" . "X-Mailer: php";

$subject = "MIRS DAP password request";

//$body = "Hi,\n\nHow are you?\nI just downloaded MIRS package.\n" . "-" . $firstname . "\nRegards\n" ; 

$body = $msg;

$ip = $_SERVER['REMOTE_ADDR'];
$hostname = gethostbyaddr($ip);

$body = $body . "\n\nPS: The client IP is $ip, hostname is $hostname." ;

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

                                <h1><center>Submit MIRS DAP Password Request Form Successfully</center></h1>

                                <div style="margin: auto; text-align: left;">
				
<?php

if (mail($to, $subject, $body, $headers)) {


  echo("<p>Hi $firstname,<br></p>");
  echo("<p>Thanks for your interest in MIRS.<br></p>");
  echo("<p>Your request is successfully sent to MIRS team. And we will contact you soon.<br></p>");
  echo("<p>Best Regards,</p>");
  echo("<p>-MIRS Team<br></p><br>");
  
  //echo("<a href=\"http://mirs.nesdis.noaa.gov\">MIRS Homepage</a>");
  
 } else {
  echo("<p>Submit request failed.</p>");
 }
?>
				
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

