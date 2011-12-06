function highlightEdges(evt, row, color)
{
  var labels = edgeTable[row];
  var reset = false;
  var el;

   /* If no color was specified, we reset the original values. */
  if(typeof color == 'undefined') {
    reset = true; 
    color=black;
  } else {
    currentStyle = evt.target.getAttribute('style');
  }

    /* Loop over the edges associated with this node */
  for(var i = 0; i < labels.length; i++) {
      el = document.getElementById(labels[i]);
      setEdgeStyle(el, "stroke: " + color);
  }
 
  labels = edgeDiff[row];
  var stroke = "lightgray";
  if(reset) stroke = "black";

    /* hide the other edges */
  for(var i = 0 ; i < labels.length; i++) {
    el = document.getElementById(labels[i]);
    setEdgeStyle(el, 'stroke: ' + stroke);
  }

   /* Restore or set the style for the target node. */
  if(reset) evt.target.setAttribute('style', currentStyle);
  else evt.target.setAttribute('style', 'fill: ' + color);
}
