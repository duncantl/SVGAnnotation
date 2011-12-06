
function setLambda(evType, group, val)
{
        /* If it is the same value, don't do anything*/
    if(Math.floor(val) == cur_lambda) return(0);
    setVisibility(cur_lambda, 'hidden', numPoints);
    cur_lambda = Math.floor(val);
    setVisibility(cur_lambda, 'visible', numPoints);
}

function setVisibility(lambda, state, numPoints)
{
     var el;
     lambda = Math.floor(lambda);
     el = document.getElementById("curve-lambda-" + lambda);
     el.setAttribute('visibility', state);

     el = document.getElementById("residual-group-" + lambda);
     if(el) {
          el.setAttribute('visibility', state);
          return(0);
     }

     for(i = 0 ; i < numPoints; i++) {
         el = document.getElementById("residual-" + lambda + 
                                       "-" + (i+1));
         el.setAttribute('visibility', state);
     }
}
