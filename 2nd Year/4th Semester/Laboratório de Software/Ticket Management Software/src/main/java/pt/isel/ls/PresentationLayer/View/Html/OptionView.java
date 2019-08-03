package pt.isel.ls.PresentationLayer.View.Html;

import pt.isel.ls.Controller.MethodPath;
import pt.isel.ls.Controller.RequestContext;
import pt.isel.ls.Model.Issue;
import pt.isel.ls.PresentationLayer.Formatter.Html.Html;
import pt.isel.ls.PresentationLayer.Formatter.Html.HtmlBuilder;
import pt.isel.ls.PresentationLayer.Formatter.Html.tags.*;
import pt.isel.ls.PresentationLayer.View.ViewHtml;
import pt.isel.ls.ResultType.Result;

import java.util.List;

public class OptionView extends ViewHtml {

    private static final String TITLE = "Options";
    List<MethodPath> methodPaths;
    Html html;
    Table tableOfProject;
    Body body;
    List<Issue> domainEntityList;

    public OptionView(Result result, RequestContext requestContext) {
        super(result, requestContext);
        methodPaths = (List<MethodPath>) result.getValue();
    }

    @Override
    public String getHtml() {

        html = HtmlBuilder.buildHtml(TITLE);
        body = new Body();

        tableOfProject  = new Table();

        for (MethodPath mp : methodPaths) {
            addOption(mp);
        }

        body.addElement(tableOfProject);

        A backToRoot = new A("/");
        backToRoot.setText("Back to root");
        body.addElement(backToRoot);

        html.setBody(body);
        return html.toString();

    }

    private void addOption(MethodPath mp) {
        TableData td = new TableData();
        td.setText(mp.toString());
        TableRow tr = new TableRow();
        tr.addElement(td);
        tableOfProject.addRow(tr);
    }
}
