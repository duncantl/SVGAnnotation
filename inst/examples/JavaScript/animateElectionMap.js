
var animationHandle = null;
var currentYear = 0;
var Interval = 1000;
 
function animationStep(ids, colors, yearLabels, stateNames)
{
  currentYear++;

  if(currentYear >= yearLabels.length) {
     var el = document.getElementById("Start");
     setTextValue(el, "Start");
     clearInterval(animationHandle);
     animationHandle = null;
     for(var i = 0; i < ids.length; i++) {
        var el = document.getElementById(ids[i]);
        if(el)
          el.setAttribute('style', 'fill: ' + '#A8A8A8');
     }
     var title = document.getElementById('title');
     setTextValue(title, 'Presidential Election Results by State 1900-2008');
     return;
  }

    displayYear(currentYear, ids, colors, yearLabels, stateNames);
}
 
function displayYear(year, ids, colors, yearLabels, stateNames)
{
  for(var i = 0; i < ids.length; i++) {
     var el = document.getElementById(ids[i]);
        /* Lookup the year by name. Then within the year, look up
           the state, using the actual name not the polygon id. */
     var col = colors[yearLabels[year]][ stateNames[ i ] ] ;
     if(el && col)
        el.setAttribute('style', 'fill: ' +  col);
  }

  var title = document.getElementById('title');
  setTextValue(title, yearLabels[year]);
}
 
function toggleAnimation(evt)
{
    var label;
    if(animationHandle) {
        clearInterval(animationHandle);
	animationHandle = null;
	label = currentYear > 0 ? "Restart" : "Start";
    } else {
	animationHandle= setInterval("animationStep(polyIds, stateResultsByYear, yearLabels, polyStateNames)", Interval);
        if(currentYear >= yearLabels.length) currentYear = 0;
	displayYear(currentYear, polyIds, stateResultsByYear, yearLabels, polyStateNames);
	label = "Pause";
    }

    var start = document.getElementById('Start');
    setTextValue(start, label);
}

 
function setTextValue(node, val)
{
    node.firstChild.data = val;
    // In Opera, we can also use node.text = val;
}


