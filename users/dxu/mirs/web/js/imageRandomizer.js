<!--

// ***********************************************
// AUTHOR: WWW.CGISCRIPT.NET, LLC
// URL: http://www.cgiscript.net
// Use the script, just leave this message intact.
// Download your FREE CGI/Perl Scripts today!
// ( http://www.cgiscript.net/scripts.htm )
// ***********************************************

function image() {
};

image = new image();
number = 0;

// imageArray
image[number++] = "<h3 style='text-align: center;'>Title for random image 1<br />on sample home page</h3><img src='images/random/RandomImage1.jpg' border='0' alt='Alt text for random image 1' /><p style='text-align: left;'>Descriptive text for random image 1. <a href=''>Learn more about random image 1's work</a>.</p>"
image[number++] = "<h3 style='text-align: center;'>Title for random image 2<br />on sample home page</h3><img src='images/random/RandomImage2.jpg' border='0' alt='Alt text for random image 2' /><p style='text-align: left;'>Description of random image 2.  <a href=''>Learn more about random image 2's work</a>.</p>"
image[number++] = "<h3 style='text-align: center;'>Title for random image 3<br />on sample home page</h3><img src='images/random/RandomImage3.jpg' border='0' alt='Alt text for random image 3' /><p style='text-align: left;'>Description of random image 3. <a href=''>Learn more about random image 3's work.</a>."

// keep adding items here...

increment = Math.floor(Math.random() * number);

document.write(image[increment]);

//-->