package pt.isel.ls.PresentationLayer.View;

import pt.isel.ls.Controller.RequestContext;
import pt.isel.ls.Model.ProjectLabel;
import pt.isel.ls.PresentationLayer.Formatter.Html.Html;
import pt.isel.ls.PresentationLayer.Formatter.Html.HtmlBuilder;
import pt.isel.ls.PresentationLayer.Formatter.Html.tags.A;
import pt.isel.ls.PresentationLayer.Formatter.Html.tags.Body;
import pt.isel.ls.PresentationLayer.Formatter.Html.tags.P;
import pt.isel.ls.ResultType.CustomWrapper;
import pt.isel.ls.ResultType.Result;

public class ProjectLabelDetailView extends ViewHtml {

    ProjectLabel label;
    int pid;

    final String TITLE = "LS : G1 : Label Detail View";
    Html html;
    Body body;

    public ProjectLabelDetailView(Result result, RequestContext requestContext) {
        super(result, requestContext);
        CustomWrapper res = (CustomWrapper) result.getValue();
        label = (ProjectLabel) res.getValue();
        pid = res.getPid();

    }

    @Override
    public String getHtml() {
        html= HtmlBuilder.buildHtml(TITLE);
        body = new Body();
        A backToRoot = new A("/");
        backToRoot.setText("Back to root");
        body.addElement(backToRoot);

        addLabel();

        A backToProjectPid = new A("/projects/"+pid);
        backToProjectPid.setText("Back to Project " + pid);
        body.addElement(backToProjectPid);
        html.setBody(body);
        return html.toString();
    }
    private void addLabel() {
        if(label!=null){
            P pDesc = new P("Description: " + label.labelDesc);
            body.addElement(pDesc);
            P pColor = new P("Color : " + label.color);
            body.addElement(pColor);
        }
        else
        {
            P pNoLabels = new P("No such Label is associated with an issue in project " + pid);
            body.addElement(pNoLabels);
        }
    }

}
