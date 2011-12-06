function highlightPoint(el, status)
{
    var old = el.getAttribute('original-style');

    if(status && old == null) 
	el.setAttribute('original-style', el.getAttribute('style'));


    if(status) {
      /* Have to set the attribute within the style attribute, 
         i.e. a sub-attribute which makes things more complex. */
	var cur = el.getAttribute('style');
        var tmp = cur.replace(/fill:?[^;]+/, "fill: black");
        var tmp = tmp.replace(/stroke-width:?[^;]+/, "stroke-width: 2");
	el.setAttribute('style', tmp);
    }
    else
        el.setAttribute('style', old);
}
