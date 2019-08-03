package pt.isel.ls.PresentationLayer.View.Html;

import pt.isel.ls.Controller.RequestContext;
import pt.isel.ls.Model.Issue;
import pt.isel.ls.Model.Label;
import pt.isel.ls.PresentationLayer.Formatter.Html.Html;
import pt.isel.ls.PresentationLayer.Formatter.Html.HtmlBuilder;
import pt.isel.ls.PresentationLayer.Formatter.Html.tags.*;
import pt.isel.ls.PresentationLayer.View.ViewHtml;
import pt.isel.ls.ResultType.Result;
import pt.isel.ls.ResultType.ResultGet.ResultGetIssuesWithLabel;
import pt.isel.ls.Urlbuilder.Urlbuilder;

import java.text.SimpleDateFormat;
import java.util.List;

public class LabelView extends ViewHtml {

    final String TITLE = "LabelView";
    private static final String ID_KEY = "id";
    private static final String NAME_KEY = "name";
    private static final String DESCRIPTION_KEY = "description";
    private static final String CREATED_KEY = "created";
    private static final String CLOSED_KEY = "date of Closure";
    private static final String PROJECT_NAME_KEY = "project name";
    private static final String STATUS_KEY = "status";
    private static final String UPDATED_KEY = "updated";
    private static final SimpleDateFormat dateFormat = new SimpleDateFormat("dd/MM/yyyy HH:mm:ss");

    List<Issue> listOfIssues;
    Label label;
    int pid;

    Html html;
    Body body;
    Table tableOfIssues;


    public LabelView(Result result, RequestContext requestContext) {
        super(result, requestContext);
        ResultGetIssuesWithLabel res = (ResultGetIssuesWithLabel) result;
        listOfIssues = res.getValue();
        label = res.label;
        pid = res.pid;
    }

    @Override
    public String getHtml() {

        html= HtmlBuilder.buildHtml(TITLE);

        body = new Body();
        html.setBody(body);

        A backToRoot = new A("/");
        backToRoot.setText("Back to root");
        body.addElement(backToRoot);

        addLabel();

        addListOfIssues();

        body.addElement(getNewLabelInProjectForm());

        A backToProjectPid = new A("/projects/"+pid);
        backToProjectPid.setText("Back to Project " + pid);
        body.addElement(backToProjectPid);

        return html.toString();
    }

    private void addLabel() {
        if(label!=null){
            P pDesc = new P("Description: " + label.description);
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

    private void addListOfIssues() {
        if (listOfIssues==null || listOfIssues.isEmpty()) {
            P pNoLabels = new P("No Issues with this label yet associated with project " + pid);
            body.addElement(pNoLabels);
        }
        else {
            tableOfIssues = new Table();
            createIssueTableHeader();
            for (Issue i : listOfIssues) {
                addIssue(i);
            }

            body.addElement(tableOfIssues);
        }
        P p = new P();
        body.addElement(p);
    }
    private void createIssueTableHeader() {
        TableRow    th   = new TableRow();
        tableOfIssues.addRow(th);

        TableHeader tdh1 = new TableHeader();
        tdh1.setId("Column1");
        tdh1.setText(ID_KEY);
        th.addElement(tdh1);
        TableHeader tdh2 = new TableHeader();
        tdh2.setId("Column2");
        tdh2.setText(NAME_KEY);
        th.addElement(tdh2);

        TableHeader tdh3 = new TableHeader();
        tdh3.setId("Column3");
        tdh3.setText(DESCRIPTION_KEY);
        th.addElement(tdh3);

        TableHeader tdh4 = new TableHeader();
        tdh4.setId("Column4");
        tdh4.setText(CREATED_KEY);
        th.addElement(tdh4);

        TableHeader tdh5 = new TableHeader();
        tdh5.setId("Column5");
        tdh5.setText(CLOSED_KEY);
        th.addElement(tdh5);

        TableHeader tdh6 = new TableHeader();
        tdh6.setId("Column6");
        tdh6.setText(PROJECT_NAME_KEY);
        th.addElement(tdh6);

        TableHeader tdh7 = new TableHeader();
        tdh7.setId("Column7");
        tdh7.setText(STATUS_KEY);
        th.addElement(tdh7);

        TableHeader tdh8 = new TableHeader();
        tdh8.setId("Column8");
        tdh8.setText(UPDATED_KEY);
        th.addElement(tdh8);

    }
    private void addIssue(Issue domainEntity) {
        TableRow tr = new TableRow();

        TableData tdr1 = new TableData();
        A a = new A("/projects/"+pid+"/issues/"+domainEntity.ID);
        a.setText(String.valueOf(domainEntity.ID));
        tdr1.addElement(a);
        tr.addElement(tdr1);

        TableData tdr2 = new TableData();
        tdr2.setText(domainEntity.name);
        tr.addElement(tdr2);

        TableData tdr3 = new TableData();
        tdr3.setText(domainEntity.description);
        tr.addElement(tdr3);

        TableData tdr4 = new TableData();
        tdr4.setText(dateFormat.format(domainEntity.creationDate));
        tr.addElement(tdr4);

        TableData tdr5 = new TableData();
        tdr5.setText(domainEntity.closedDate==null?"-":dateFormat.format(domainEntity.closedDate));
        tr.addElement(tdr5);

        TableData tdr6 = new TableData();
        tdr6.setText(domainEntity.projectName);
        tr.addElement(tdr6);

        TableData tdr7 = new TableData();
        tdr7.setText(domainEntity.statusDesc);
        tr.addElement(tdr7);

        TableData tdr8 = new TableData();
        tdr8.setText(domainEntity.updated==null?"-":dateFormat.format(domainEntity.updated));
        tr.addElement(tdr8);

        tableOfIssues.addRow(tr);
    }

    private Form getNewLabelInProjectForm() {
        Form form = new Form();
        form.setAction(Urlbuilder.postLabel(pid));
        form.setMethod("Post");
        form.setEnctype("multipart/form");

        Input labelDescriptionInput = new Input();
        labelDescriptionInput.setType("text");
        labelDescriptionInput.setId("inputName");
        labelDescriptionInput.setPlaceHolder("Enter the label description");
        labelDescriptionInput.setName("name");

        form.addElement(labelDescriptionInput);

        Input colorInput = new Input();
        colorInput.setType("text");
        colorInput.setId("inputDescription");
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
