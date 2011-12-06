var lineGroup;  /* The <g> object we create and destroy to hold our dynamically generated lines. */
var k = 4;      /* How many neighbors to show. */

var svgNS = "http://www.w3.org/2000/svg";  /* Used when creating the <g> and <line> elements. */

    /* This is the function that is called by onmouseover events. */
function showNeighbors(evt, k, neighbors)
{
    var idx = 1 * evt.target.getAttribute('id');
    window.status = "Showing " + idx; 
    addLines(evt.target, neighbors[idx], k);
}

/*addLines function here */

/*hideLines function here */