<?php

/*
 * ---------------------------------------------------------------------
 *  Return all unique storm names found in the hwrf.gnorm_data.txt file
 * ---------------------------------------------------------------------
 */

$handle = @fopen("./pngs/HWRF.gnorm_data.txt", "r");

if ($handle) {
   $arr = array();

   // ---------------------------------------------
   //  read file, push storm names into $arr array
   // ---------------------------------------------
   $buffer = fgets($handle);
   while ($buffer !== false) {
      $parts = explode(',', $buffer);
      array_push( $arr, $parts[0] );
      $buffer = fgets( $handle );
   }

   if (!feof($handle)) {
      echo "Error: unexpected fgets() fail\n";
   }

   fclose($handle);

   // --------------------------------------------------------------
   //  move unique storm names into $result and sort alphabetically
   // --------------------------------------------------------------
   $result = array_unique($arr);
   asort( $result );
   $result_str = "";

   // -------------------------------------------------
   //  Load all values in $result into a single string
   // -------------------------------------------------
   foreach($result as $val) {
      $result_str .= $val;
   }

   // -------------------------------------
   // return list of storms in $result_str
   // -------------------------------------
   //$clean_str = strip_tags( $result_str );
   echo "$result_str\n";

} else {
   echo "Error:  unable to open file\n";
}

?>
