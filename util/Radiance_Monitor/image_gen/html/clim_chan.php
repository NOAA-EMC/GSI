<head>
</head>
<body>
<?php

/*
 *  clim_chan.php
 *    - takes an input cycle time and sat/instrument and
 *      return the available channels.
 *
 *    Note:  This is only used by plot_2d.html.
 */

$cycle = $_GET['cyc'];
$sat   = $_GET['sat'];

if( (strlen( $cycle ) > 0) && (strlen( $sat ) > 0) ) {

   $pdir = "../../../../../GFDPT/site/clim/" . $cycle . "/" . $sat . "/";

   $files = glob( "$pdir*N*.png" );
   $nfiles = count( $files );

   echo ",";           /* add this so the calling script can
                          break on "," and remove all the
                          returned html preceeding the src list */


foreach( $files as $file ){
      $strs = explode( '_', $file );
      $chan = preg_replace( '/Ch/', '', $strs[4] );

      echo "$chan,";
   }


}
?>
</body>
</html>
