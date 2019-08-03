package pt.isel.ls.PresentationLayer.Formatter.Html.tags;

import pt.isel.ls.PresentationLayer.Formatter.Html.HtmlElement;

public class TableHeader extends HtmlElement {
    private static final String STRING_TEMPLATE = "<th%s>%s%s</th>";

    @Override
    public String toString() {
            stringCache = String.format(STRING_TEMPLATE,
                    getId(),
                    getText(), getElementsString());
        return stringCache;
    }
}
