package pt.isel.ls.PresentationLayer.View;

import pt.isel.ls.Controller.RequestContext;
import pt.isel.ls.ResultType.Result;

public abstract class View {
    protected Result result;
    protected RequestContext requestContext;

    public View(Result result, RequestContext requestContext) {
        this.result = result;
        this.requestContext = requestContext;
    }
}

