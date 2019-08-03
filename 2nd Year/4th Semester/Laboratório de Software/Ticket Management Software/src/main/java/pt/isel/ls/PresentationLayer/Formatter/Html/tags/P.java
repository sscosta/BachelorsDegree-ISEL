package pt.isel.ls.PresentationLayer.Formatter.Html.tags;

import pt.isel.ls.PresentationLayer.Formatter.Html.HtmlElement;
import pt.isel.ls.PresentationLayer.Formatter.Html.IHtmlElement;

public class P extends HtmlElement implements IHtmlElement {
    private final String STRING_TEMPLATE = "<p>%s</p>";

    public P(){
    }

    public P(String text) {
        super.setText(text);
    }

    @Override
    public String toString() {
        return String.format(STRING_TEMPLATE, getText());
    }
}
