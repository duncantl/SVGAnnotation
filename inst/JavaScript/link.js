function color_point(evt, which, numPlots, color) {
        for(i = 1; i <= numPlots ; i++) {
          path = document.getElementById("plot" + i + "-" + which);
          path.setAttribute("fill", color);
        }
}

function reset_color(evt, which, numPlots)
{
        for(i = 1; i <= numPlots ; i++) {
          path = document.getElementById("plot" + i + "-" + which);
          path.setAttribute("fill", path.getAttribute("originalFill"));
        }  
}