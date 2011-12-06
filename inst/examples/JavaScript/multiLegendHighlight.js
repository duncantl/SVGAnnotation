function highlight(group, pointCounts, status)
{
    var el;
    var i, numPanels = pointCounts.length;

       /*  we want the group */
     for(panel = 1; panel <= numPanels; panel++) {
       for(i = 1; i <= pointCounts[panel-1]; i++) {
         var id = panel + "-"+ group + "-" + i;
         el = document.getElementById(id);
         if(el == null) {
           alert("can't find element " + id)
           return(1);
         }
         highlightPoint(el, status);
       }
     }
 }
