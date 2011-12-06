function highlight(group, pointCts, status)
{
    var el;
    var i, numPanels = pointCts.length;

       /*  we want the group */
    for(panel = 1; panel <= numPanels; panel++) {
      for(i = 1; i <= pointCts[panel-1]; i++) {
        var id = panel + "-"+ group + "-" + i;
        el = doc.getElementById(id);
        if(el == null) {
          alert("can't find element " + id)
          return(1);
        }
        highlightPoint(el, status);
      }
    }
 }
