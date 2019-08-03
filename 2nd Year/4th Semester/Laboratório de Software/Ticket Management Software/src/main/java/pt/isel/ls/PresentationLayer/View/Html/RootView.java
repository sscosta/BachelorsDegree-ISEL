package pt.isel.ls.PresentationLayer.View.Html;

import pt.isel.ls.Controller.RequestContext;
import pt.isel.ls.PresentationLayer.Formatter.Html.Html;
import pt.isel.ls.PresentationLayer.Formatter.Html.HtmlBuilder;
import pt.isel.ls.PresentationLayer.Formatter.Html.tags.A;
import pt.isel.ls.PresentationLayer.Formatter.Html.tags.Body;
import pt.isel.ls.PresentationLayer.View.ViewHtml;
import pt.isel.ls.ResultType.Result;


public class RootView extends ViewHtml {
    private static final String TITLE = "Root page";
    Html html;

    public RootView(Result result, RequestContext requestContext) {
        super(result, requestContext);
    }

    @Override
    public String getHtml() {
        html    = HtmlBuilder.buildHtml(TITLE);

        Body body = new Body();
        html.setBody(body);

        A a = new A("/projects");
        a.setText("Go to Projects List");
        body.addElement(a);

        return html.toString();
    }
}
