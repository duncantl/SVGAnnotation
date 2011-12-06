/*
  
*/
function highlight(id, status)
{
    var el = document.getElementById(id);
    var w = 1;
    var old;

    if(status)
	w = 3;


    old = el.getAttribute('original-style');


    if(status && old == null) 
	el.setAttribute('original-style', el.getAttribute('style'));


    if(status) {
      /* Have to set the attribute within the style attribute, i.e. a sub-attribute
         which makes things more complex. */
	var cur = el.getAttribute('style');
	var re = new RegExp("stroke-width:[^;]+", "g");
	re.compile();
        var tmp = cur.replace(/stroke-width: [^;]+/, "stroke-width: " + w);
	el.setAttribute('style', tmp);
    }
    else 
	el.setAttribute('style', old);
}


function highlightText(id, status)
{
    id = id + "-labels";
    var obj = document.getElementById(id);
    var val;
    
    if(obj == null) {
	return(1);
    }

    if(status) val = 'visible' else val = 'hidden';

    obj.setAttribute('visibility', val);
}