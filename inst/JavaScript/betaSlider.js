 var myMapApp = new mapApp(false,undefined);
 var slider1;
 var slider2;

 var alpha = 1;
 var beta = 1;
 var curve = "curve-1-1";

function init(evt, len_a, len_b) {
   var sliderStyles={"stroke":"dimgray", "stroke-width":3};
   var invisSliderWidth = 15;
   slider1 = new slider("alpha", "slider-alpha", 100, 510, 1, 475, 510, len_a, 1, 
                          sliderStyles, invisSliderWidth, "sliderSymbol", showVal, true);
   slider2 = new slider("beta", "slider-beta", 100, 560, 1, 475, 560, len_b, 1,
                          sliderStyles, invisSliderWidth, "sliderSymbol", showVal, true);
}

function toggleCurve(curve, onOff) {
    document.getElementById(curve).setAttribute('visibility', onOff);
}

function showVal(type, group, value) {
  value = Math.round(value);
  toggleCurve(curve, 'hidden');
  if(group == "alpha") {
      alpha = value;
  } else {
      beta = value;
  }

  curve = "curve" + "-" + alpha + "-" + beta;

  toggleCurve(curve, 'visible'); 

  var tmp = document.getElementById(curve);
  statusChange("\u03B1 = " + tmp.getAttribute('alpha') + ", \u03B2 = " + tmp.getAttribute('beta'));
}

function statusChange(val) {
   document.getElementById("statusText").firstChild.nodeValue = val;
}
