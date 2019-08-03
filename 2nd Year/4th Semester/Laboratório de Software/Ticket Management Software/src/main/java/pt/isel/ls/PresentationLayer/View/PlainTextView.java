package pt.isel.ls.PresentationLayer.View;

import pt.isel.ls.Controller.RequestContext;
import pt.isel.ls.ResultType.Result;

import java.util.Collection;
import java.util.List;

public class PlainTextView extends View {

    public PlainTextView (Result result, RequestContext requestContext) {
        super(result, requestContext);    }

    public <T> String getText() {
        StringBuilder res = new StringBuilder();
        if (result.getValue() instanceof Collection<?>)
            res = parseResult((List<T>) result.getValue(), res);
        else
            res.append(result.getValue());

        return res.toString();
    }

    private <T> StringBuilder parseResult(List<T> list, StringBuilder res) {
        String prefix = "";

        for (T item : list){
            res.append(prefix);
            prefix = "\n";
            res.append(item.toString());
        }
        return res;
    }
}
