package pt.isel.ls.PresentationLayer.Formatter.Html;

import pt.isel.ls.PresentationLayer.Formatter.Html.tags.Head;

public class HtmlBuilder {
    private static final String EMPTY_RESULTS = "There are no results for this request";

    public static Html buildHtml(String pageTitle) {
        Html html = new Html();
        Head head = new Head(pageTitle);

        html.setHead(head);

        return html;
    }

}
