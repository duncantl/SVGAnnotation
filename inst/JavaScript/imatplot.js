/*
var factor = 2.0;
var color = null;
*/
function toggleSeries(which, on)
{
   var el = document.getElementById(which);
   if(!el) {
      return(false);
   }
   var cur = el.getAttribute('stroke-width');
   var val;
   if(on) {
      val = cur * factor;
   } else {
      val = cur / factor;
   }
   
   el.setAttribute('stroke-width', val);
   if(color)  {
       if(on) {
          if(!el.getAttribute('original-stroke'))
              el.setAttribute('original-stroke', el.getAttribute('stroke'));
           el.setAttribute('stroke', color);
       } else {
           el.setAttribute('stroke', el.getAttribute('original-stroke'));
       }
   }
   return(true);
}
