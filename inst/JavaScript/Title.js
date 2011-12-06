// == Title.js -- Copyright (C) Stefan Goessner ========================
function Title(doc, sz)
{
   this.element = null;  // element to show title of ..
   Title.size = sz;      // text size ..
   Title.scl = doc.getDocumentElement().getCurrentScale();     // scaling modified by zooming ..
   Title.off = doc.getDocumentElement().getCurrentTranslate(); // offset modified by zooming ..
   this.Create(doc);
   AddTitleEvents(doc.getDocumentElement());
   doc.getDocumentElement().addEventListener("zoom", Title.Zoom, false);
   window.svgTitle = this;
}

Title.prototype.Create = function(doc)
{
   this.rec = doc.createElement("rect");
   this.rec.setAttribute("y", -0.9*Title.size);
   this.rec.setAttribute("x", -0.25*Title.size);
   this.rec.setAttribute("width", "1");
   this.rec.setAttribute("height", 1.25*Title.size);
   this.rec.setAttribute("style", "stroke:black;fill:#edefc2;stroke-width:1");

   this.str = doc.createTextNode("");

   this.txt = doc.createElement("text")
   this.txt.setAttribute("style", "font-family:Arial; font-size:" + Title.size + ";fill:black;");
   this.txt.appendChild(this.str);

   this.grp = doc.createElement("g"),
   this.grp.setAttribute("transform", "translate(0,0)");
   this.grp.setAttribute("visibility", "hidden");
   this.grp.appendChild(this.rec);
   this.grp.appendChild(this.txt);

   doc.getDocumentElement().appendChild(this.grp);
}

Title.Activate = function Title_Activate(evt)
{
   if (window.svgTitle.element == null)
   {
     var  x = (evt.getClientX() - Title.off.getX())/Title.scl +  0.25*Title.size,
          y = (evt.getClientY() - Title.off.getY())/Title.scl - Title.size;

      window.svgTitle.element = evt.getCurrentTarget();
      window.svgTitle.element.removeEventListener("mouseover", Title.Activate, false);
      window.svgTitle.element.addEventListener("mouseout", Title.Passivate, false);
      window.svgTitle.str.setNodeValue(TextOf(TitleElementOf(window.svgTitle.element)));
      window.svgTitle.rec.setAttribute("width", window.svgTitle.txt.getComputedTextLength() + 0.5*Title.size);
      window.svgTitle.grp.setAttribute("transform", "translate(" + x + "," + y + ")");
      window.svgTitle.grp.setAttribute("visibility", "visible");
   }
}

Title.Passivate = function Title_Passivate(evt)
{
   if (window.svgTitle.element != null)
   {
      window.svgTitle.grp.setAttribute("visibility", "hidden");
      window.svgTitle.element.removeEventListener("mouseout", Title.Passivate, false);
      window.svgTitle.element.addEventListener("mouseover", Title.Activate, false);
      window.svgTitle.element = null;
   }
}

Title.Zoom = function Title_Zoom(evt)
{
   var newscl = evt.getTarget().getOwnerDocument().getDocumentElement().getCurrentScale();

   Title.size *= Title.scl/newscl;
   Title.scl = newscl;
   Title.off = evt.getTarget().getOwnerDocument().getDocumentElement().getCurrentTranslate();

   window.svgTitle.rec.setAttribute("y", -0.9*Title.size);
   window.svgTitle.rec.setAttribute("x", -0.25*Title.size);
   window.svgTitle.rec.setAttribute("height", 1.25*Title.size);
   window.svgTitle.rec.setAttribute("style", "stroke:black;fill:#edefc2;stroke-width:" + 1/Title.scl);
   window.svgTitle.txt.setAttribute("style", "font-family:Arial; font-size:" + Title.size + ";fill:black;");
}

Title.Register = function Title_Register(elem)
{
   if (TitleElementOf(elem) != null)
      elem.addEventListener("mouseover", Title.Activate, false);
}

// --- local helper functions ------------------------

function TitleElementOf(elem)
{
   var childs = elem.getChildNodes();

   for (var i=0; i<childs.getLength(); i++)
      if (childs.item(i).getNodeType() == 1 && childs.item(i).getNodeName() == "title") // title element ..
         return childs.item(i);
   
   return null;
}

function TextOf(elem)
{
   var childs = elem ? elem.getChildNodes() : null;

   for (var i=0; childs && i<childs.getLength(); i++)
      if (childs.item(i).getNodeType() == 3) // text node ..
         return childs.item(i).getNodeValue();
   
   return "";
}

function AddTitleEvents(elem)
{
   var childs = elem.getChildNodes();

   for (var i=0; i<childs.getLength(); i++)
      if (childs.item(i).getNodeType() == 1) // element node ..
         AddTitleEvents(childs.item(i));

   if (TitleElementOf(elem) != null)
      elem.addEventListener("mouseover", Title.Activate, false);
}

// === end ======================================================
