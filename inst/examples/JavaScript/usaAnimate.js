var maxRepeat = 3;
var numRepeats = 0;
var currentYear = 0;
var handle = null;


function init(evt)
{
  handle= setInterval("updateStates(stateColors, polyIds)", 300);
}


function updateStates(colors, ids)
{
     /* Move to the next year and if we are at the end, start all 
        over again.*/       
  currentYear++;
  if(currentYear == colors.length) {
    currentYear = 0;
    numRepeats++;
  }
  if(numRepeats == maxRepeat) {
      clearInterval(handle);
      alert("That's enough now!");
  }

     /* Loop over the ids and update the colors */
  for(var i = 0; i < ids.length; i++) {
     var el = document.getElementById(ids[i]);
     el.setAttribute('style', 'fill: ' + colors[currentYear][i]);
  }
}

function pause(evt)
{
    var title = doc.getElementById('title');
    var label;
    if(handle) {
        clearInterval(handle);
	label = "Start";
    } else {
	init(evt);
	label = "Pause";
    }
}



/* I thought that I needed to expose the updateStates function,
   but apparantly not.
window.updateStates = updateStates;
*/

