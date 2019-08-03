package pt.isel.ls.PresentationLayer.View.Html;

import pt.isel.ls.Controller.RequestContext;
import pt.isel.ls.Model.ProjectLabel;
import pt.isel.ls.PresentationLayer.Formatter.Html.Html;
import pt.isel.ls.PresentationLayer.Formatter.Html.HtmlBuilder;
import pt.isel.ls.PresentationLayer.Formatter.Html.tags.*;
import pt.isel.ls.PresentationLayer.View.ViewHtml;
import pt.isel.ls.ResultType.Result;
import pt.isel.ls.ResultType.ResultGet.ResultGetLabelsInProject;
import pt.isel.ls.Urlbuilder.Urlbuilder;

import java.util.LinkedList;

public class ProjectLabelView extends ViewHtml {

    final String TITLE = "ProjectLabelView";
    private static final String DESCRIPTION_KEY = "labeldesc";
    private static final String PROJECT_NAME_KEY = "projectname";
    private static final String COLOR_KEY = "color";

    private LinkedList<ProjectLabel> listOfLabels;
    private int pid;
    private String labelName;

    Html html;
    Body body;
    Table tableOfLabels;

    public ProjectLabelView(Result result, RequestContext requestContext) {
        super(result, requestContext);
        ResultGetLabelsInProject res = (ResultGetLabelsInProject) result;
        listOfLabels = res.getValue();
        pid = res.pid;
        labelName = res.getLabelName();
    }

    @Override
    public String getHtml() {
        html= HtmlBuilder.buildHtml(TITLE);

        body = new Body();
        html.setBody(body);

        A backToRoot = new A("/");
        backToRoot.setText("Back to root");
        body.addElement(backToRoot);

        BR br = new BR();
        body.addElement(br);
        body.addElement(br);

        addListOfIssues();

        body.addElement(getFormToAddLabel());

        if(labelName!=null){
            A noLabel = new A();
            noLabel.setText("Unable to add -"+labelName+"-. Already exist");
            body.addElement(noLabel);
            body.addElement(br);
            body.addElement(br);
        }


        A backToProjectPid = new A("/projects/"+pid);
        backToProjectPid.setText("Back to Project " + pid);
        body.addElement(backToProjectPid);

        return html.toString();
    }

    private void addListOfIssues() {
        if (listOfLabels==null || listOfLabels.isEmpty()) {
            P pNoLabels = new P("No labels yet associated with project " + pid);
            body.addElement(pNoLabels);
        }
        else {
            tableOfLabels = new Table();
            createLabelTableHeader();
            for (ProjectLabel pl : listOfLabels) {
                addLabel(pl);
            }

            body.addElement(tableOfLabels);
        }
        P p = new P();
        body.addElement(p);
    }
    private void createLabelTableHeader() {
        TableRow    th   = new TableRow();
        tableOfLabels.addRow(th);

        TableHeader tdh1 = new TableHeader();
        tdh1.setText(DESCRIPTION_KEY);
        th.addElement(tdh1);

        TableHeader tdh2 = new TableHeader();
        tdh2.setText(PROJECT_NAME_KEY);
        th.addElement(tdh2);

        TableHeader tdh3 = new TableHeader();
        tdh3.setText(COLOR_KEY);
        th.addElement(tdh3);
    }
    private void addLabel(ProjectLabel domainEntity) {
        TableRow tr = new TableRow();
        tableOfLabels.addRow(tr);

        TableData tdr1 = new TableData();
        tdr1.setText(domainEntity.labelDesc);
        tr.addElement(tdr1);

        TableData tdr2 = new TableData();
        tdr2.setText(domainEntity.projectName);
        tr.addElement(tdr2);

        TableData tdr3 = new TableData();
        tdr3.setText(domainEntity.color);
        tr.addElement(tdr3);

    }
    private Form getFormToAddLabel() {
        Form form = new Form();
        form.setAction(Urlbuilder.postLabel(pid));
        form.setMethod("Post");
        form.setEnctype("multipart/form");

        Input labelDescInput = new Input();
        labelDescInput.setType("text");
        labelDescInput.setRequired();
        labelDescInput.setId("inputDesc");
        labelDescInput.setPlaceHolder("Enter the label description");
        labelDescInput.setName("name");

        form.addElement(labelDescInput);

        Input colorInput = new Input();
        colorInput.setType("text");
        colorInput.setRequired();
        colorInput.setId("inputColor");
        colorInput.setPlaceHolder("Enter the label color");
        colorInput.setName("color");

        form.addElement(colorInput);
        Button submitBtn = new Button("Submit");
        submitBtn.setType("submit");
        submitBtn.addClass("btn btn-primary");
        form.addElement(submitBtn);

        return form;
    }

}
