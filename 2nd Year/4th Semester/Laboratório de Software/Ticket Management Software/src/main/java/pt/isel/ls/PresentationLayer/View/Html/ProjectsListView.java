package pt.isel.ls.PresentationLayer.View.Html;

import pt.isel.ls.Controller.RequestContext;

import pt.isel.ls.Model.Project;
import pt.isel.ls.PresentationLayer.Formatter.Html.Html;
import pt.isel.ls.PresentationLayer.Formatter.Html.HtmlBuilder;
import pt.isel.ls.PresentationLayer.Formatter.Html.HtmlElement;
import pt.isel.ls.PresentationLayer.Formatter.Html.tags.*;
import pt.isel.ls.PresentationLayer.View.ViewHtml;
import pt.isel.ls.ResultType.Result;
import pt.isel.ls.ResultType.ResultGet.ResultGetProjects;
import pt.isel.ls.Urlbuilder.Urlbuilder;

import java.util.List;

public class ProjectsListView extends ViewHtml {

    final String TITLE = "LS : G1 - ProjectsView";
    final String PROJECT_NAME_KEY = "Project Name";
    final String DESCRIPTION_KEY = "Description";
    final String CREATION_DATE_KEY = "Creation Date";
    final String PROJECT_ID = "ID";
    private String projectName;
    private List<Project> listDomainEntity;
    Html  html;
    Table tableOfProject;
    Body  body;


    public ProjectsListView(Result result, RequestContext requestContext) {
        super(result, requestContext);
        ResultGetProjects res = (ResultGetProjects) result;
        listDomainEntity = res.getValue();
        projectName = res.getProjectName();
    }

    @Override
    public String getHtml() {

        html = HtmlBuilder.buildHtml(TITLE);

        body = new Body();
        html.setBody(body);

        tableOfProject = new Table();
        body.addElement(tableOfProject);

        if (!listDomainEntity.isEmpty())
            createTableHeader();

        if (listDomainEntity != null && !listDomainEntity.isEmpty()) {
            for (Project p :
                    listDomainEntity) {
                addProject(p);
            }
        } else {
            TableData tdr1 = new TableData();
            tdr1.setText("NO RESULTS");
            TableRow tr = new TableRow();
            tr.addElement(tdr1);
            tableOfProject.addRow(tr);
        }

        BR br = new BR();
        body.addElement(br);

        body.addElement(getNewProjectForm());

        if(projectName!=null){
            A noProject = new A();
            noProject.setText("Unable to add -"+projectName+"-. Already exist");
            body.addElement(noProject);
            body.addElement(br);
            body.addElement(br);
        }

        A backToRoot = new A("/");
        backToRoot.setText("Back to root");
        body.addElement(backToRoot);

        return html.toString();
    }

    private void createTableHeader() {
        TableRow th = new TableRow();
        tableOfProject.addRow(th);

        TableHeader tdh1 = new TableHeader();
        tdh1.setId("Column1");
        tdh1.setText(PROJECT_NAME_KEY);
        th.addElement(tdh1);

        TableHeader tdh2 = new TableHeader();
        tdh1.setId("Column2");
        tdh2.setText(DESCRIPTION_KEY);
        th.addElement(tdh2);

        TableHeader tdh3 = new TableHeader();
        tdh1.setId("Column3");
        tdh3.setText(CREATION_DATE_KEY);
        th.addElement(tdh3);

        TableHeader tdh4 = new TableHeader();
        tdh1.setId("Column4");
        tdh4.setText(PROJECT_ID);
        th.addElement(tdh4);
    }


    private void addProject(Project project) {
        TableRow tr = new TableRow();
        tableOfProject.addRow(tr);

        TableData tdr1 = new TableData();
        A a = new A("/projects/"+project.ID);
        a.setText(project.name);
        tdr1.addElement(a);
        tr.addElement(tdr1);

        TableData tdr2 = new TableData();
        tdr2.setText(project.description);
        tr.addElement(tdr2);

        TableData tdr3 = new TableData();
        tdr3.setText(project.creationDate.toString());
        tr.addElement(tdr3);

        TableData tdr4 = new TableData();
        tdr4.setText(String.valueOf(project.ID));
        tr.addElement(tdr4);
    }

    private HtmlElement getNewProjectForm() {
        Form form = new Form();
        form.setAction(Urlbuilder.postProject());
        form.setMethod("Post");
        form.setEnctype("multipart/form");

        Label nameLabel = new Label("Name:");
        nameLabel.setFor("inputName");

        Input nameInput = new Input();
        nameInput.setType("text");
        nameInput.setRequired();
        nameInput.setId("inputName");
        nameInput.setPlaceHolder("Enter the project name");
        nameInput.setName("name");

        form.addElement(nameInput);
        Label descriptionLabel = new Label("description:");
        descriptionLabel.setFor("inputDescription");

        Input descriptionInput = new Input();
        descriptionInput.setType("text");
        descriptionInput.setRequired();
        descriptionInput.setId("inputDescription");
        descriptionInput.setPlaceHolder("Enter the project description");
        descriptionInput.setName("description");

        form.addElement(descriptionInput);
        Button submitBtn = new Button("Submit");
        submitBtn.setType("submit");
        submitBtn.addClass("btn btn-primary");

        form.addElement(submitBtn);


        return form;

    }

}
