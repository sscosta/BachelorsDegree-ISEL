package pt.isel.ls.PresentationLayer.View.Html.ViewsForStatusCodes;

import pt.isel.ls.Controller.RequestContext;
import pt.isel.ls.PresentationLayer.Formatter.Html.Html;
import pt.isel.ls.PresentationLayer.Formatter.Html.HtmlBuilder;
import pt.isel.ls.PresentationLayer.Formatter.Html.tags.Body;
import pt.isel.ls.PresentationLayer.Formatter.Html.tags.H;
import pt.isel.ls.PresentationLayer.View.ViewHtml;
import pt.isel.ls.ResultType.Result;

public class Http500 extends ViewHtml {
    private static final String TITLE = "Http 500";
    private final String message;
    Html html;
    Body body;
    public Http500(Result result, RequestContext requestContext, String message) {
        super(result,requestContext);
        this.message= message;
    }

    @Override
    public String getHtml() {
        html = HtmlBuilder.buildHtml(TITLE);
        body = new Body();
        H h = new H(1);
        h.setText("Error 500");
        body.addElement(h);

        H h2 = new H(2);
        h2.setText(message);
        body.addElement(h2);

        html.setBody(body);
        return html.toString();
    }
}
