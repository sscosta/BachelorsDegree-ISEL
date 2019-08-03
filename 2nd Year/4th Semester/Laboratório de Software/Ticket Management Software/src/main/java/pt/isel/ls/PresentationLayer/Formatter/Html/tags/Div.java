package pt.isel.ls.PresentationLayer.Formatter.Html.tags;

import pt.isel.ls.PresentationLayer.Formatter.Html.HtmlElement;
import pt.isel.ls.PresentationLayer.Formatter.Html.IHtmlElement;

public class Div extends HtmlElement implements IHtmlElement {
    private static final String STRING_TEMPLATE = "<div%s>%s%s</div>";

    public Div(){}
    public Div(String text){
        setText(text);
    }

    @Override
    public String toString() {
        stringCache = String.format(STRING_TEMPLATE, getId(), getText(), getElementsString());
        return stringCache;
    }
}
