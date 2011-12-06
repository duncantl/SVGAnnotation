function addLines(obj, neighbors, numNeighbors)
{
    var x, y, x1, y1;

    var tmp = obj.getBBox();
    x = tmp.x + tmp.width/2;
    y = tmp.y + tmp.height/2;

    lineGroup = document.createElementNS(svgNS, "g");      
    obj.parentNode.appendChild(lineGroup);
    var ids = obj.getAttribute('id') + ": ";
    for(var i = 1; i <= numNeighbors ; i++) {
      var target;
      target = document.getElementById(neighbors[i]);
      ids = ids + " " + neighbors[i];

      tmp = target.getBBox();
      x1 = tmp.x + tmp.width/2;
      y1 = tmp.y + tmp.height/2;

      var line = document.createElementNS(svgNS, "line");      
      line.setAttribute('x1', x);
      line.setAttribute('y1', y);
      line.setAttribute('x2', x1);
      line.setAttribute('y2', y1);
      line.setAttribute('style', "fill: red; stroke: red;");

      line.setAttribute('class', "neighborLine");
      lineGroup.appendChild(line);
    }
    window.status = ids;
}
