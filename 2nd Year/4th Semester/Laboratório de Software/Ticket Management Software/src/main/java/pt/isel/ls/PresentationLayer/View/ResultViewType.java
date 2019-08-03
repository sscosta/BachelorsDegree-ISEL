package pt.isel.ls.PresentationLayer.View;


import pt.isel.ls.Controller.RequestContext;
import pt.isel.ls.ResultType.Result;

public interface ResultViewType {

    void setResult(Result res);
    void setRequestOptions(RequestContext requestContext);
}
