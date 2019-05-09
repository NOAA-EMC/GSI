<html>
<head>
    <title> clim date </title>
</head>
<body>
<?php

/*
 *  clim_date.php
 *    - find the lastest available cycle for the 2-D plots and
 *      and return that 10 digit string.
 *
 *    Note:  this is only used by plot_2d.html.
 */

if ($handle = opendir('../../../../../GFDPT/site/clim')) {

    $src_len = strlen( $source );
    $date_st = 0;

    $dates = array ();

    /* Loop over directory and load all 00z cycle images
     * into array $dates.
     */
    while (false !== ($entry = readdir($handle))) {
        if ($entry != "." && $entry != "..") {
           $date = substr($entry, $date_st, 10);

           $cyc = substr($date,8,2);
           $src = substr($entry, 0, $src_len);

           if ( is_numeric( $date ) && $src == $source ){
              array_push( $dates, $date );
           }
        }

    }

    /* Sort the array for good measure, pop and return
     * the last date.
     */
    asort($dates);
    $rdate = array_pop($dates);
    echo " $rdate ";

    closedir($handle);
}
?>
</body>
</html>
