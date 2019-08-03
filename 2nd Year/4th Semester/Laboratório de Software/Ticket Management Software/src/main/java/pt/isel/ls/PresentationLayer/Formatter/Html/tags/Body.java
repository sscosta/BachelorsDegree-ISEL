package pt.isel.ls.PresentationLayer.Formatter.Html.tags;

import pt.isel.ls.PresentationLayer.Formatter.Html.HtmlElement;

import java.util.ArrayList;
import java.util.List;

public class Body extends HtmlElement {
    private static final String STRING_BEGIN_DELIMITER = "\t<body>\n";
    private static final String STRING_END_DELIMITER = "\t</body>\n";

    private List<HtmlElement> Elements;

    public void addElement(HtmlElement element) {
        if (Elements == null) Elements = new ArrayList<>();

        Elements.add(element);
    }

    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append(STRING_BEGIN_DELIMITER);

        for (HtmlElement elem :
                Elements) {
            sb.append(elem.toString());
        }
        sb.append(STRING_END_DELIMITER);

        return sb.toString();
    }
}
