package pt.isel.ls.PresentationLayer.View.Html;

import pt.isel.ls.Controller.RequestContext;
import pt.isel.ls.Model.Comment;
import pt.isel.ls.Model.Issue;
import pt.isel.ls.Model.Label;
import pt.isel.ls.PresentationLayer.Formatter.Html.Html;
import pt.isel.ls.PresentationLayer.Formatter.Html.HtmlBuilder;
import pt.isel.ls.PresentationLayer.Formatter.Html.tags.*;
import pt.isel.ls.PresentationLayer.View.ViewHtml;
import pt.isel.ls.ResultType.CustomWrapper;
import pt.isel.ls.ResultType.Result;
import pt.isel.ls.ResultType.ResultGet.ResultGetIssueById;
import pt.isel.ls.Urlbuilder.Urlbuilder;

import java.text.SimpleDateFormat;

public class SingleIssueView extends ViewHtml {

    private static final String TITLE = "LS : G1 : SingleIssueView";
    private static final String ID_KEY = "id";
    private static final String NAME_KEY = "name";
    private static final String DESCRIPTION_KEY = "description";
    private static final String CREATED_KEY = "created";
    private static final String CLOSED_KEY = "date of Closure";
    private static final String PROJECT_NAME_KEY = "project name";
    private static final String STATUS_KEY = "status";
    private static final String UPDATED_KEY = "updated";
    private static final SimpleDateFormat dateFormat = new SimpleDateFormat("dd/MM/yyyy HH:mm:ss");

    Html  html;
    Table issueTable;
    Body  body;
    Issue issue;
    int pid;
    public SingleIssueView(Result result, RequestContext requestContext) {
        super(result, requestContext);
        CustomWrapper wrappedResult = (CustomWrapper) result.getValue();
        issue = (Issue) wrappedResult.getValue();
        pid = wrappedResult.getPid();
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
        h4.setText("issue #" + issue.ID + " of project " + issue.projectName);
        body.addElement(h4);

        issueTable = new Table();
        createTableHeader();
        addIssue(issue);
        body.addElement(issueTable);

        /*Button to Open/close*/
        insertForm();

        if(!issue.getComments().isEmpty())
            body.addElement(addTableOfComments(issue));

        else{
            P p = new P();
            p.setText("No Comments associated to this issue");
            body.addElement(p);
        }


        body.addElement(br);

        if(issue.statusDesc.equals("open"))
            body.addElement(getNewCommentForm());

        if(!issue.getLabels().isEmpty())
            body.addElement(addTableOfLabels(issue));
        else{
            P p = new P();
            p.setText("No Labels associated to this issue");
            body.addElement(p);
        }

        body.addElement(br);

        A goToIssueLabel = new A("/projects/"+ pid + "/issues/" + issue.ID + "/labels");
        goToIssueLabel.setText("Go to Issue's associated labels");
        body.addElement(goToIssueLabel);

        body.addElement(br);
        body.addElement(br);

        A backToIssuesList = new A("/projects/"+pid);
        backToIssuesList.setText("Back To Project");
        body.addElement(backToIssuesList);

        html.setBody(body);
        return html.toString();
    }

    private void insertForm() {
        BR br = new BR();
        body.addElement(br);
        String updateStatus = issue.statusDesc.equals("open") ? "close" : "open";

        Form form = new Form();
        form.setAction("/projects/"+pid+"/issues/"+ issue.ID+"/"+updateStatus);
        form.setMethod("Post");
        form.setEnctype("multipart/form");

        Button submitBtn = new Button(updateStatus);
        submitBtn.setType("submit");
        submitBtn.addClass("btn btn-primary");

        form.addElement(submitBtn);
        body.addElement(form);
    }

    private Table addTableOfLabels(Issue issue) {
        H h4 = new H(4);
        h4.setText("Labels for issue #" + this.issue.ID + " of project " + this.issue.projectName);
        body.addElement(h4);
        Table tableOfLabels = new Table();
        createLabelTableHeader(tableOfLabels);
        for(Label l : this.issue.getLabels()){
            addLabel(l,tableOfLabels);
        }
        return tableOfLabels;
    }

    private void addLabel(Label l, Table tableOfLabels) {
        TableData tdr1 = new TableData();
        A label = new A("/projects/" + pid + "/labels/" + l.description);
        label.setText(l.description);
        tdr1.addElement(label);

        TableRow tr = new TableRow();
        tr.addElement(tdr1);

        tableOfLabels.addRow(tr);
    }

    private void createLabelTableHeader(Table tableOfLabels) {
        TableRow th = new TableRow();
        TableHeader tdh1 = new TableHeader();
        tdh1.setId("Column1");
        tdh1.setText("Description");
        th.addElement(tdh1);
        tableOfLabels.addRow(th);
    }

    private Table addTableOfComments(Issue domainEntity) {
        H h4 = new H(4);
        h4.setText("Comments for issue #" + domainEntity.ID + " of project " + domainEntity.projectName);
        body.addElement(h4);
        Table tableOfComments = new Table();
        createCommentTableHeader(tableOfComments);
        for(Comment c : domainEntity.getComments()){
            addComment(c,tableOfComments);
        }
        return tableOfComments;
    }

    private void addComment(Comment c, Table tableOfComments) {
        TableRow tr = new TableRow();

        TableData tdr1 = new TableData();
        A a = new A("/projects/"+pid+"/issues/"+ issue.ID+
                "/comments/"+c.id);
        a.setText(String.valueOf(c.id));
        tdr1.addElement(a);
        tr.addElement(tdr1);

        TableData tableDataIssueID = new TableData();
        tableDataIssueID.setText(String.valueOf(c.issueID));
        tr.addElement(tableDataIssueID);

        TableData tableDataProjectName = new TableData();
        tr.addElement(tableDataProjectName);
        tableDataProjectName.setText(c.projectName);

        TableData tableDataDate = new TableData();
        tableDataDate.setText(dateFormat.format(c.date));
        tr.addElement(tableDataDate);

        TableData tableDataText = new TableData();
        tableDataText.setText(c.text);
        tr.addElement(tableDataText);
        tableOfComments.addRow(tr);
    }

    private void createCommentTableHeader(Table tableOfComments) {
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

        tableOfComments.addRow(th);
    }

    private void createTableHeader() {
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

        issueTable.addRow(th);
    }

    private void addIssue(Issue domainEntity) {
        TableRow tr = new TableRow();

        TableData tdr1 = new TableData();
        tdr1.setText(String.valueOf(domainEntity.ID));
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

        issueTable.addRow(tr);
    }

    private Form getNewCommentForm(){
        Form form = new Form();
        form.setAction(Urlbuilder.postComment(pid, issue.ID));
        form.setMethod("Post");
        form.setEnctype("multipart/form");
        form.setOnSubmit("setValidation()");



        Input textInput = new Input();
        textInput.setType("text");
        textInput.setRequired();
        textInput.setId("inputCommentText");
        textInput.setPlaceHolder("Enter the comment text");
        textInput.setName("text");
        form.addElement(textInput);


        Button submitBtn = new Button("Submit");
        submitBtn.setType("submit");
        submitBtn.addClass("btn btn-primary");

        form.addElement(submitBtn);

        return form;
    }


}
