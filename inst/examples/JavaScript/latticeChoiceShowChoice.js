var oldgroup = 0;
var group = 0;
var doc;

function showChoice(obj)
{
   doc = document.getElementById('latticePlot');
   doc = doc.getSVGDocument();
   group = Math.floor(obj.value);
   if (group > 0) {
    highlight(group, pointCounts[(group - 1)], true);
   }

   if (oldgroup > 0) {
    highlight(oldgroup, pointCounts[(oldgroup - 1)], false);
   }
  
   oldgroup = group;
}
