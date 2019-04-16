<html>
<head>
</head>
<body>
<?php

/*
 *  clim_src.php
 *    - takes an input cycle time and find the available sat/instrument
 *      plots availble in from the GFDPT group.
 *
 *    Note:  this is only used by plot_2d.html.
 */

$cycle=$_GET['cyc'];

if( strlen( $cycle ) > 0 ) {

    $pdir="../../../../../GFDPT/site/clim/" . $cycle . "/.";
    $subdirs = glob( $pdir . '/*' , GLOB_ONLYDIR );
    sort( $subdirs );
    $cnt = count( $subdirs );


    if( $cnt > 0 ) {
        echo ",";           /* add this so the calling script can
                               break on "," and remove all the
                               returned html preceeding the src list */

        for ($i = 0; $i <= count( $subdirs ); $i++) {
            $sdir = explode( '/', $subdirs[$i] );
            $src = $sdir[count($sdir)-1];
            if( strlen( $src ) > 2 ){
                echo "$src,";
            }
        }
    }
}

?>
</body>
</html>
