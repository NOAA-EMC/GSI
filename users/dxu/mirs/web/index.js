// to test web browser is IE or not
var IE = document.all?true:false;

// global variable to keep track if image already visible or not
var VISIBLE_ALREADY = 0;

// glocal image id
img_id="mspps_composite";

function init() {

    document.getElementById(img_id).style.display  = "none"; 
    
    if( IE ) {
        document.attachEvent( "onmousemove", getXY );
        //document.attachEvent( "click", getXY );
    }
    else  {
        document.addEventListener( "mousemove", getXY, false );
        //document.addEventListener( "click", getXY, false );
    }

}


function getXY(e) {

  if ( VISIBLE_ALREADY == 0 ) {

        var posx = 0;
        var posy = 0;
        if (!e) var e = window.event;
        if (e.pageX || e.pageY)         {
                posx = e.pageX;
                posy = e.pageY;
        }
        else if (e.clientX || e.clientY)        {
                posx = e.clientX + document.body.scrollLeft
                        + document.documentElement.scrollLeft;
                posy = e.clientY + document.body.scrollTop
                        + document.documentElement.scrollTop;
        }

        // posx and posy contain the mouse position relative to the document
        // Do something with this information

        //alert('posx=' + posx + ', posy=' + posy );

        if( posx > 0 && posy > 0 ) {
                document.getElementById(img_id).style.display  = "block";
        }

        VISIBLE_ALREADY = 1;

    }

}

