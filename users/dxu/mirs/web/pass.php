<?php

###############################################################
# Page Password Protect 2.13
###############################################################
# Visit http://www.zubrag.com/scripts/ for updates
############################################################### 
#
# Usage:
# Set usernames / passwords below between SETTINGS START and SETTINGS END.
# Open it in browser with "help" parameter to get the code
# to add to all files being protected. 
#    Example: password_protect.php?help
# Include protection string which it gave you into every file that needs to be protected
#
# Add following HTML code to your page where you want to have logout link
# <a href="http://www.example.com/path/to/protected/page.php?logout=1">Logout</a>
#
###############################################################

##################################################################
#  SETTINGS START
##################################################################

// Add login/password pairs below, like described above
// NOTE: all rows except last must have comma "," at the end of line
$LOGIN_INFORMATION = array(
  'test'    => 'testpass',
  'user'    => 'pass'
);

// request login? true - show login and password boxes, false - password box only
define('USE_USERNAME', true);

// User will be redirected to this page after logout
define('LOGOUT_URL', 'http://mirs.nesdis.noaa.gov');

// time out after NN minutes of inactivity. Set to 0 to not timeout
define('TIMEOUT_MINUTES', 0);

// This parameter is only useful when TIMEOUT_MINUTES is not zero
// true - timeout time from last activity, false - timeout time from login
define('TIMEOUT_CHECK_ACTIVITY', true);

##################################################################
#  SETTINGS END
##################################################################


///////////////////////////////////////////////////////
// do not change code below
///////////////////////////////////////////////////////

// show usage example
if(isset($_GET['help'])) {
  die('Include following code into every page you would like to protect, at the very beginning (first line):<br>&lt;?php include("' . str_replace('\\','\\\\',__FILE__) . '"); ?&gt;');
}

// timeout in seconds
$timeout = (TIMEOUT_MINUTES == 0 ? 0 : time() + TIMEOUT_MINUTES * 60);

// logout?
if(isset($_GET['logout'])) {
  setcookie("verify", '', $timeout, '/'); // clear password;
  header('Location: ' . LOGOUT_URL);
  exit();
}

if(!function_exists('showLoginPasswordProtect')) {

// show login form
function showLoginPasswordProtect($error_msg) {
?>

<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN" "http://www.w3.org/TR/html4/loose.dtd">
<html lang="en">
<head>
  <title>MIRS - Please enter username / password to access this page</title>
<?php
// insert style links and the icon link
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
    <td colspan="2" class="mainPanel" id="bestOf"><a name="skipTarget"></a><?php require('includes/noScriptWarning.inc'); ?>
	<div class="padding">
			<ul class="breadcrumbs">
				<li><a href="index.php">MIRS Home</a></li>
				<li>&nbsp;>&nbsp;User Log-In</li>
			</ul>
			<h1>MIRS Image &amp; Data Access</h1>
			<h2>Please enter your username / password to access this page</h2>
			<p>To obtain a valid username and password for this site, please contact: <a 
				href="mailto:Sid.Boukabara@noaa.gov">Sid.Boukabara@noaa.gov</a>.</p>

  		<form method="post" style="margin-left: 100px; margin-top: 16px;" action="">
			<p class="stylered"><?php echo $error_msg; ?></p>
			<table cellspacing="0" border="0" cellpadding="3" class="tableGrid" style="width: 40% !important;">

			<?php if (USE_USERNAME) echo '<tr><th>Username:</th><td><input type="text" name="access_login" style="width: 200px;" /></td></tr><tr><th>Password:</th>'; ?>
		  <td><input type="password" name="access_password"  style="width: 200px;"/></td></tr>
			</table>
			<p style="margin-left: 140px;"><input type="submit" name="Submit" value="Submit" /></p>
		</form>
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

<?php
  // stop at this point
  die();
}
}

// user provided password
if (isset($_POST['access_password'])) {

  $login = isset($_POST['access_login']) ? $_POST['access_login'] : '';
  $pass = $_POST['access_password'];
  if (!USE_USERNAME && !in_array($pass, $LOGIN_INFORMATION)
  || (USE_USERNAME && ( !array_key_exists($login, $LOGIN_INFORMATION) || $LOGIN_INFORMATION[$login] != $pass ) ) 
  ) {
    showLoginPasswordProtect("Incorrect username/password.");
  }
  else {
    // set cookie if password was validated
    setcookie("verify", md5($login.'%'.$pass), $timeout, '/');
    
    // Some programs (like Form1 Bilder) check $_POST array to see if parameters passed
    // So need to clear password protector variables
    unset($_POST['access_login']);
    unset($_POST['access_password']);
    unset($_POST['Submit']);
  }

}

else {

  // check if password cookie is set
  if (!isset($_COOKIE['verify'])) {
    showLoginPasswordProtect("");
  }

  // check if cookie is good
  $found = false;
  foreach($LOGIN_INFORMATION as $key=>$val) {
    $lp = (USE_USERNAME ? $key : '') .'%'.$val;
    if ($_COOKIE['verify'] == md5($lp)) {
      $found = true;
      // prolong timeout
      if (TIMEOUT_CHECK_ACTIVITY) {
        setcookie("verify", md5($lp), $timeout, '/');
      }
      break;
    }
  }
  if (!$found) {
    showLoginPasswordProtect("");
  }

}

?>
