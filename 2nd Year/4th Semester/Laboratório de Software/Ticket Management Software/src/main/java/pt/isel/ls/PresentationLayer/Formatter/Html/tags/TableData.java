package pt.isel.ls.PresentationLayer.Formatter.Html.tags;

import pt.isel.ls.PresentationLayer.Formatter.Html.HtmlElement;

public class TableData extends HtmlElement {
    private static final String STRING_TEMPLATE = "<td%s>%s%s</td>";

    @Override
    public String toString() {
            stringCache = String.format(STRING_TEMPLATE,
                    getId(),
                    getText(), getElementsString());
        return stringCache;
    }

    @Override
    public void addElement(HtmlElement element) {
        super.addElement(element);
    }
}
