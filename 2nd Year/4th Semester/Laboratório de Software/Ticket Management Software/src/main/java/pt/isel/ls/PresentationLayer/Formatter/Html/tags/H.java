package pt.isel.ls.PresentationLayer.Formatter.Html.tags;

import pt.isel.ls.PresentationLayer.Formatter.Html.HtmlElement;
import pt.isel.ls.PresentationLayer.Formatter.Html.IHtmlElement;

public class H extends HtmlElement implements IHtmlElement {
    private final String STRING_TEMPLATE;
    private final int INDEX;

    public H(Integer index) {
        this.INDEX = index;
        STRING_TEMPLATE = String.format("<h%1$d%%s>%%s%%s</h%1$d>", INDEX);
    }

    @Override
    public String toString() {
        return String.format(STRING_TEMPLATE, getId(), getText(), getElementsString());
    }
}
