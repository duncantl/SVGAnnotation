var myMapApp = new mapApp(false,undefined);
var cur_lambda = 2;
var numPoints;

/*  Create the slider.  */
function init(evt, maxLambda, n) {
   var sliderStyles = {"stroke" : "blue", "stroke-width" : 3};
   var invisSliderWidth = 15;
   new slider("lambda", "slider-lambda", 100, 510, 2, 475, 510, 
              maxLambda, 2, sliderStyles, invisSliderWidth, 
              "sliderSymbol", setLambda, false);
   numPoints = n;
}
