package pt.isel.ls.PresentationLayer.Formatter.Html.tags;

import pt.isel.ls.PresentationLayer.Formatter.Html.HtmlElement;
import pt.isel.ls.PresentationLayer.Formatter.Html.IHtmlElement;

public class A extends HtmlElement implements IHtmlElement {
    private static final String STRING_TEMPLATE = "<a%s>%s</a>";

    private String _href;

    public A(){}

    public A(String href){
        this._href = href;
    }

    public void setHref(String url) {
        _href = url;
    }
    private String getHref() {
        return _href == null ? "" : String.format(" href=%s", _href);
    }

    @Override
    public String toString() {
            return String.format(STRING_TEMPLATE,  getHref(),
                     getText());
    }
}
