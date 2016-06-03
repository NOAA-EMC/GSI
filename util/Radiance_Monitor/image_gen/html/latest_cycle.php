<html>
 <head>
  <title> cycle </title>
 </head>
 <body>
 <?php 

/*
 *  latest_cycle.php
 *    - takes an input source (or uses airs_aqua as the default)
 *      finds the latest cycle and returns that 10 digit string.
 */
$source=$_GET['src'];
if ($source == "" ) {
  $source = "airs_aqua"; 
}

if ($handle = opendir('./pngs/summary')) {

    $src_len = strlen( $source );
    $date_st = $src_len + 1;

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
    echo "$rdate";

    closedir($handle);
}
?> 
 </body>
</html>
