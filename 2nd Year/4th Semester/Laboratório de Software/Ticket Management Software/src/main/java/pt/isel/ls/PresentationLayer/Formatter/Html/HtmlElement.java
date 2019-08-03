package pt.isel.ls.PresentationLayer.Formatter.Html;

import java.util.ArrayList;
import java.util.List;

public abstract class HtmlElement {

    // spaces are important, otherwise elements will be glued together in tags
    private static final String ID_TEMPLATE      = " id=%s";
    private static final String CLASS_IDENTIFIER = " class";

    private String _id;
    private List<String> _classes;
    private List<HtmlElement> _elements;
    private String _text;

    protected String stringCache;
    private HtmlElement parent;

    public void setId(String id) {
        _id = id;
    }
    public String getId() {
        if(_id == null)
            return "";
        return String.format(ID_TEMPLATE, _id);
    }

    public void setText(String text) {
        _text = text;
    }
    public String getText() {
        if(_text == null) return "";
        return _text;
    }

    public void addClass(String cssClass) {
        if (_classes == null) _classes = new ArrayList<>();
        _classes.add(cssClass);
    }
    public String getHtmlClassString() {
        if (_classes == null) return "";

        StringBuilder sb = new StringBuilder();
        sb.append(CLASS_IDENTIFIER);
        sb.append("=\"");

        if (_classes != null)
            for (String c : _classes) {
                sb.append(c);
                sb.append(" ");
            }

        sb.append("\"");
        return sb.toString();
    }

    /* Child Elements */
    public void addElement(HtmlElement element) {
        if (_elements == null) _elements = new ArrayList<>();
        _elements.add(element);
        element.setParent(this);
    }
    protected String getElementsString(){
        if (_elements != null) {
            StringBuilder sb = new StringBuilder();
            for (HtmlElement e : _elements)
                sb.append(e.toString());

            return sb.toString();
        }
        return "";
    }

    private void setParent(HtmlElement parent){
        this.parent = parent;
    }

}
