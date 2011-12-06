var checkBoxes = new Array();
var radioGroupBandwidth;
var numPanels = 0;

function createRadioBoxList(count, x, labels, toggleCB, prefix, npanel) {
	
		//labeltext styles
    var labeltextStyles = {"font-family":"Arial,Helvetica","fill":"dimgray","font-size":15};
		//variables for label placement
    var labelDistance = 12;
    var labelYOffset = 5.5;

    var y = 50; 
    var i;

    numPanels = npanel;

    for(i = 1; i <= count; i++) {
          checkBoxes[i] = new checkBox(prefix + i, "checkboxes", x, y + (i-1) * 18, "checkBoxRect", "checkBoxCross", true, 
                                       labels[i-1], labeltextStyles,labelDistance,labelYOffset,undefined, toggleCB);

    }
}

function toggle(id, status, label) {
    var onOrOff = "hidden";

    if (status) {
	onOrOff = "visible";
    }
    document.getElementById(id).setAttribute('visibility', onOrOff);
}


function togglePanel(id, status, label) {
    var onOrOff = "hidden";
    var p;

    if (status) {
	onOrOff = "visible";
    }

    for( p = 1; p <= numPanels; p++) {
       pid = "panel-" + p + "-" + id;
       document.getElementById(pid).setAttribute('visibility', onOrOff);
    }
}
