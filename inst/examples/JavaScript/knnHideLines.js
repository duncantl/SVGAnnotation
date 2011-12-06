
function hideLines()
{
    if(typeof lineGroup != "undefined") {
      lineGroup.parentNode.removeChild(lineGroup);
      lineGroup = document.createElementNS(svgNS, "g");      
    }
}
