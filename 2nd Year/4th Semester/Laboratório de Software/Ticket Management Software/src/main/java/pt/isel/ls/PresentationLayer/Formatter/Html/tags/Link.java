package pt.isel.ls.PresentationLayer.Formatter.Html.tags;

import pt.isel.ls.PresentationLayer.Formatter.Html.HtmlElement;
import pt.isel.ls.PresentationLayer.Formatter.Html.IHtmlElement;

public class Link extends HtmlElement implements IHtmlElement {
    private static final String STRING_LINK_TEMPLATE = "<link %s%s%s%s></link>";

    private String _rel;
    private String _href;
    private String _integrity;
    private String _crossorigin;

    public Link (String rel, String href){
        setRel(rel);
        setHref(href);
    }

    public void setHref(String url) {
        _href = url;
    }
    private String getHref() {
        return String.format("href=\"%s\"", _href);
    }

    public void setRel(String rel) {
        _rel = rel;
    }
    private String getRel() {
        return String.format("rel=\"%s\"", _rel);
    }

    public void setIntegrity(String integrity) {
        _integrity = integrity;
    }
    private String getIntegrity() {
        if(_integrity != null)
            return String.format("integrity=\"%s\"", _integrity);
        return "";
    }

    public void setCrossorigin(String crossorigin) {
        _crossorigin = crossorigin;
    }
    private String getCrossorigin() {
        if(_integrity != null)
            return String.format("crossorigin=\"%s\"", _crossorigin);
        return "";
    }

    @Override
    public String toString() {
        return String.format(STRING_LINK_TEMPLATE, getRel(), getHref(), getIntegrity(), getCrossorigin() );
    }
}
