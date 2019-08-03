package pt.isel.ls.PresentationLayer.Formatter.Html.tags;

import pt.isel.ls.PresentationLayer.Formatter.Html.HtmlElement;


public class TableRow extends HtmlElement {
    private static final String STRING_TEMPLATE = "\t\t\t<tr%s>%s%s</tr>\n";

    @Override
    public String toString() {
            stringCache = String.format(STRING_TEMPLATE,
                    getId(),
                    getText(), getElementsString());
        return stringCache;
    }
}
