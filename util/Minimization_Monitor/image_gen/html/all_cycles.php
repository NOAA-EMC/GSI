<html>
 <head>
  <title>PHP Test</title>
 </head>
 <body>
 <?php 

$source=$_GET['src'];
if ($source == "" ) {
  $source = "GDAS";
}
$type=$_GET['type'];
//echo "$source, $type";

if ( empty($source) ){
//if ($source == "" ) {
  $type = "gnorm";
}

if( $type === "single_cycle_reduction" ){
   $typ = "reduction";
} elseif( $type === "single_cycle_costs" ){
   $typ = "costs";
} elseif( $type === "single_cycle_gnorm" ){
   $typ = "gnorms";
} else {
   $typ = "none";
}

//if( $type == "single_cycle_costs" ){
// $typ = "costs";
//}

//echo "oh jeeze:  $source, $type, $typ";

if ($handle = opendir('./pngs')) {

    $date_st = strlen( $source ) + 1;

    /* This is the correct way to loop over the directory. */
    while (false !== ($entry = readdir($handle))) {
        if ($entry != "." && $entry != "..") {

           $src_pos = strpos( $entry, $source );
           $typ_pos = strpos( $entry, $typ    );

           $date = substr($entry, $date_st, 10); 

           if ( is_numeric( $date )){
              if( $src_pos !== false && $typ_pos !== false ){
                 echo "$date ";   
              }
           }
        }

    }
    closedir($handle);
}
?> 
 </body>
</html>
