package pt.isel.ls.PresentationLayer.Formatter.Html.tags;

import pt.isel.ls.PresentationLayer.Formatter.Html.HtmlElement;
import pt.isel.ls.PresentationLayer.Formatter.Html.IHtmlElement;

public class BR extends HtmlElement implements IHtmlElement {
    private final String STRING_TEMPLATE = "<br>";

    public BR(){
    }

    @Override
    public String toString() {
        return String.format(STRING_TEMPLATE);
    }
}
