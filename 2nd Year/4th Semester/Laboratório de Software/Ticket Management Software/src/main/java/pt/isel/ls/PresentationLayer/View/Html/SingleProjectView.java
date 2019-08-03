package pt.isel.ls.PresentationLayer.View.Html;

import pt.isel.ls.Controller.RequestContext;
import pt.isel.ls.Model.Project;

import pt.isel.ls.Model.ProjectLabel;
import pt.isel.ls.PresentationLayer.Formatter.Html.Html;
import pt.isel.ls.PresentationLayer.Formatter.Html.HtmlBuilder;
import pt.isel.ls.PresentationLayer.Formatter.Html.tags.*;
import pt.isel.ls.PresentationLayer.View.ViewHtml;
import pt.isel.ls.ResultType.CustomWrapper;
import pt.isel.ls.ResultType.Result;
import pt.isel.ls.ResultType.ResultGet.ResultGetProjectWithId;

public class SingleProjectView extends ViewHtml {

    private static final String TITLE = "SingleProjectView";
    private static final String PROJECT_NAME_KEY = "Project Name";
    private static final String DESCRIPTION_KEY = "Description";
    private static final String CREATION_DATE_KEY = "Creation Date";
    private static final String PROJECT_ID = "ID";

    Html html;
    Table tableOfProject;
    Body body;
    Project domainEntity;

    public SingleProjectView(Result result, RequestContext requestContext) {
        super(result, requestContext);
        domainEntity = (Project) result.getValue();
    }

    @Override
    public String getHtml() {


        html = HtmlBuilder.buildHtml(TITLE);
        body = new Body();
        tableOfProject = new Table();

        A backToRoot = new A("/");
        backToRoot.setText("Back to root");
        body.addElement(backToRoot);

        H h = new H(1);
        h.setText(domainEntity.name);
        body.addElement(h);

        addProject(domainEntity);

        BR br = new BR();
        body.addElement(br);

        A goToLabels = new A("/projects/" + this.domainEntity.ID + "/labels");
        goToLabels.setText("Go to project labels");

        body.addElement(goToLabels);

        body.addElement(br);
        H h4 = new H(4);
        h4.setText("Project labels");
        body.addElement(h4);

        if (!this.domainEntity.getLabels().isEmpty())
            body.addElement(addTableOfLabels());
        else {
            P noLabels = new P("No labels associated with this project.");
            body.addElement(noLabels);
        }

        body.addElement(br);

        A search = new A(domainEntity.ID + "/search");
        search.setText("Search project issues");
        body.addElement(search);


        body.addElement(br);
        body.addElement(br);

        A back = new A("/projects");
        back.setText("Back to projects list");
        body.addElement(back);

        html.setBody(body);
        return html.toString();
    }

    private void addProject(Project project) {
        body.addElement(tableOfProject);

        TableRow trID = new TableRow();
        tableOfProject.addRow(trID);

        TableData tdIDLabel = new TableData();
        tdIDLabel.setText(PROJECT_ID);
        trID.addElement(tdIDLabel);

        TableData tdID = new TableData();
        tdID.setText(String.valueOf(project.ID));
        trID.addElement(tdID);

        TableRow trDesc = new TableRow();
        tableOfProject.addRow(trDesc);

        TableData tdDescLabel = new TableData();
        tdDescLabel.setText(DESCRIPTION_KEY);
        trDesc.addElement(tdDescLabel);

        TableData tdDesc = new TableData();
        tdDesc.setText(project.description);
        trDesc.addElement(tdDesc);

        TableRow trCreateDate = new TableRow();

        TableData tdCreateDateLabel = new TableData();
        tdCreateDateLabel.setText(CREATION_DATE_KEY);
        trCreateDate.addElement(tdCreateDateLabel);

        TableData tdCreateDate = new TableData();
        tdCreateDate.setText(project.creationDate.toString());
        trCreateDate.addElement(tdCreateDate);
        tableOfProject.addRow(trCreateDate);

    }

    private Table addTableOfLabels() {
        Table tableOfLabels = new Table();
        createLabelTableHeader(tableOfLabels);

        for (ProjectLabel l : domainEntity.getLabels()) {
            addLabel(l, tableOfLabels);
        }
        return tableOfLabels;
    }

    private void createLabelTableHeader(Table tableOfLabels) {
        TableRow th = new TableRow();
        TableHeader tdh1 = new TableHeader();
        tdh1.setText("Description");
        th.addElement(tdh1);
        tableOfLabels.addRow(th);
    }

    private void addLabel(ProjectLabel l, Table tableOfLabels) {
        TableRow tr = new TableRow();
        tableOfLabels.addRow(tr);

        TableData tdr1 = new TableData();
        A label = new A(domainEntity.ID + "/labels/" + l.labelDesc);
        label.setText(l.labelDesc);
        tdr1.addElement(label);
        tr.addElement(tdr1);
    }

}
