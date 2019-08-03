package pt.isel.ls.PresentationLayer.View.Html;


import pt.isel.ls.Controller.RequestContext;
import pt.isel.ls.Model.Comment;
import pt.isel.ls.Model.Issue;


import pt.isel.ls.PresentationLayer.Formatter.Html.Html;
import pt.isel.ls.PresentationLayer.Formatter.Html.HtmlBuilder;
import pt.isel.ls.PresentationLayer.Formatter.Html.tags.*;
import pt.isel.ls.PresentationLayer.View.ViewHtml;
import pt.isel.ls.ResultType.Result;
import pt.isel.ls.ResultType.ResultGet.ResultGetIssues;
import pt.isel.ls.Urlbuilder.Urlbuilder;

import java.text.SimpleDateFormat;
import java.util.List;

public class IssueListView extends ViewHtml {

    private static final String TITLE = "LS : G1 : IssueListView";
    private static final String ID_KEY = "id";
    private static final String NAME_KEY = "name";
    private static final String DESCRIPTION_KEY = "description";
    private static final String CREATED_KEY = "created";
    private static final String CLOSED_KEY = "date of Closure";
    private static final String PROJECT_NAME_KEY = "project name";
    private static final String STATUS_KEY = "status";
    private static final String UPDATED_KEY = "updated";
    private static final SimpleDateFormat dateFormat = new SimpleDateFormat("dd/MM/yyyy HH:mm:ss");

    Html html;
    Table tableOfIssues;
    Body body;
    List<Issue> domainEntityList;
    int pid;
    String issueName;
    public IssueListView(Result result, RequestContext requestContext) {
        super(result, requestContext);
        ResultGetIssues res = (ResultGetIssues)result;
        domainEntityList = res.getValue();
        pid = res.pid;
        issueName = res.getIssueName();
    }

    @Override
    public String getHtml() {

        html = HtmlBuilder.buildHtml(TITLE);
        body = new Body();
        BR br = new BR();

        A backToRoot = new A("/");
        backToRoot.setText("Back to root");
        body.addElement(backToRoot);

        H h4 = new H(4);
        h4.setText("Project issues");
        body.addElement(h4);


        if(!domainEntityList.isEmpty()) {
            tableOfIssues = new Table();
            body.addElement(tableOfIssues);
            createIssueTableHeader();
        }
        else{
            P p = new P();
            p.setText("No results match your criteria");
            body.addElement(p);
        }
        for (Issue i : domainEntityList) {
            addIssue(i);
        }

        for(Issue i: domainEntityList){
            if(!i.getComments().isEmpty()) {
                body.addElement(addTableOfComments(i));
            }
        }
        body.addElement(br);

        insertForm();

        if(issueName!=null){
            A noIssue = new A();
            noIssue.setText("Unable to add -"+issueName+"-. Already exist");
            body.addElement(noIssue);
            body.addElement(br);
            body.addElement(br);
        }

        A backToSearch = new A("/projects/"+pid+"/search");
        backToSearch.setText("Back to search");
        body.addElement(backToSearch);

        html.setBody(body);
        return html.toString();
    }

    private void insertForm() {
        Form form = new Form();
        form.setAction(Urlbuilder.postIssue(pid));
        form.setMethod("Post");
        form.setEnctype("multipart/form");

        Label nameLabel = new Label("Name:");
        nameLabel.setFor("inputName");

        Input nameInput = new Input();
        nameInput.setType("text");
        nameInput.setRequired();
        nameInput.setId("inputName");
        nameInput.setPlaceHolder("Enter new issue name");
        nameInput.setName("name");

        form.addElement(nameInput);
        Label descriptionLabel = new Label("description:");
        descriptionLabel.setFor("inputDescription");

        Input descriptionInput = new Input();
        descriptionInput.setType("text");
        descriptionInput.setRequired();
        descriptionInput.setId("inputDescription");
        descriptionInput.setPlaceHolder("Enter the issue description");
        descriptionInput.setName("description");

        form.addElement(descriptionInput);
        Button submitBtn = new Button("Submit");
        submitBtn.setType("submit");
        submitBtn.addClass("btn btn-primary");

        form.addElement(submitBtn);
        body.addElement(form);

    }

    private Table addTableOfComments(Issue i) {
        H h2 = new H(2);
        h2.setText("Comments for issue" + i.ID + " of project " + i.projectName);
        body.addElement(h2);
        Table tableOfComments = new Table();
        createCommentTableHeader(tableOfComments);
        for(Comment c : i.getComments()){
            addComment(c,tableOfComments);

        }
        return tableOfComments;
    }

    private void addComment(Comment c, Table tableOfComments) {
        TableData tdr1 = new TableData();
        tdr1.setText(String.valueOf(c.id));
        TableData tdr2 = new TableData();
        tdr2.setText(String.valueOf(c.issueID));
        TableData tdr3 = new TableData();
        tdr3.setText(c.projectName);
        TableData tdr4 = new TableData();
        tdr4.setText(String.valueOf(c.date));
        TableData tdr5 = new TableData();
        tdr5.setText(c.text);

        TableRow tr = new TableRow();
        tr.addElement(tdr1);
        tr.addElement(tdr2);
        tr.addElement(tdr3);
        tr.addElement(tdr4);
        tr.addElement(tdr5);
        tableOfComments.addRow(tr);
    }

    private void createCommentTableHeader(Table t) {
        TableRow    th   = new TableRow();

        TableHeader tdh1 = new TableHeader();
        tdh1.setId("Column1");
        tdh1.setText("ID");
        th.addElement(tdh1);

        TableHeader tdh2 = new TableHeader();
        tdh2.setId("Column2");
        tdh2.setText("ISSUE ID");
        th.addElement(tdh2);

        TableHeader tdh3 = new TableHeader();
        tdh3.setId("Column3");
        tdh3.setText("PROJECT NAME");
        th.addElement(tdh3);

        TableHeader tdh4 = new TableHeader();
        tdh4.setId("Column4");
        tdh4.setText("DATE");
        th.addElement(tdh4);

        TableHeader tdh5 = new TableHeader();
        tdh5.setId("Column5");
        tdh5.setText("TEXT");
        th.addElement(tdh5);

        t.addRow(th);
    }

    private void createIssueTableHeader() {
        TableRow    th   = new TableRow();

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

        tableOfIssues.addRow(th);
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

}
