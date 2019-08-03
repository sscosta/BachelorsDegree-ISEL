package pt.isel.ls.PresentationLayer.View;

import pt.isel.ls.Controller.RequestContext;
import pt.isel.ls.ResultType.Result;

public abstract class ViewHtml extends View {
    public ViewHtml(Result result, RequestContext requestContext) {
        super(result, requestContext);
    }

    public abstract String getHtml();
}
