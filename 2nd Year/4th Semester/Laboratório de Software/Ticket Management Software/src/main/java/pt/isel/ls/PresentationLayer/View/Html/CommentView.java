package pt.isel.ls.PresentationLayer.View.Html;

import pt.isel.ls.Controller.RequestContext;
import pt.isel.ls.Model.Comment;
import pt.isel.ls.PresentationLayer.Formatter.Html.Html;
import pt.isel.ls.PresentationLayer.Formatter.Html.HtmlBuilder;
import pt.isel.ls.PresentationLayer.Formatter.Html.tags.*;
import pt.isel.ls.PresentationLayer.View.ViewHtml;
import pt.isel.ls.ResultType.CustomWrapper;
import pt.isel.ls.ResultType.Result;
import pt.isel.ls.Urlbuilder.Urlbuilder;

import java.text.SimpleDateFormat;
import java.util.LinkedList;


public class CommentView extends ViewHtml {
    private static final String TITLE = "LS : G1 : Comment View";
    private static final SimpleDateFormat dateFormat = new SimpleDateFormat("dd/MM/yyyy HH:mm:ss");

    Html html;
    Table commentTable;
    Body  body;
    Comment domainEntity;
    LinkedList<Comment> commentList;

    public CommentView(Result result, RequestContext requestContext) {
        super(result, requestContext);
        CustomWrapper res = (CustomWrapper) result.getValue();
        domainEntity = (Comment) res.getValue();
        commentList = res.getCommentList();
    }

    @Override
    public String getHtml() {

        html = HtmlBuilder.buildHtml(TITLE);
        body = new Body();
        html.setBody(body);

        A backToRoot = new A("/");
        backToRoot.setText("Back to root");
        body.addElement(backToRoot);

        int pid = (int) requestContext.getParamMap().get("projectID");
        int iid = (int) requestContext.getParamMap().get("issueID");


        H h = new H(4);
        h.setText("Comments for issue #"+iid);
        body.addElement(h);
        addCommentToBody();

        commentTable = new Table();
        commentTable.setBorder(0);
        body.addElement(commentTable);

        int idx=-1;
        for(Comment comment : commentList){
            if(comment.id == domainEntity.id){
                idx = commentList.indexOf(comment);
            }
        }

        String prev = null;
        String next = null;
        if(idx-1>=0)
            prev = getLinkComment(pid,iid,commentList.get(idx-1).id);

        if(idx+1<commentList.size())
            next = getLinkComment(pid,iid,commentList.get(idx+1).id);

        addLinkToTable(prev, next);

        //body.addElement(getNewCommentForm());
        body.addElement(new BR());
        A backLink = new A("/projects/"+pid+"/issues/"+iid);
        backLink.setText("Back To issue "+iid);
        body.addElement(backLink);

        return html.toString();
    }

    private void addLinkToTable(String prev, String next) {
        TableData tdr1 = new TableData();
        if(prev!=null) {
            A prevLink = new A(prev);
            prevLink.setText("Previous Comment");
            tdr1.addElement(prevLink);
        }
        TableData tdr2 = new TableData();
        if(next!=null) {
            A nextLink = new A(next);
            nextLink.setText("Next Comment");
            tdr2.addElement(nextLink);
        }
        TableRow tr = new TableRow();
        tr.addElement(tdr1);
        tr.addElement(tdr2);

        commentTable.addRow(tr);
    }

    private String getLinkComment(int pid, int iid, int cid) {
        String ref ="/projects/"+pid+"/issues/"+iid+"/comments/"+cid;
        return ref;
    }

    private void addCommentToBody() {
        P p1 = new P();
        p1.setText("Comment Id = "+domainEntity.id);
        body.addElement(p1);

        P p2 = new P();
        p2.setText("Post Date = "+dateFormat.format(domainEntity.date));
        body.addElement(p2);

        P p3 = new P();
        p3.setText("Detail Text:<br>"+domainEntity.text);
        body.addElement(p3);
    }
    private Form getNewCommentForm(){

        Form form = new Form();
        form.setAction(Urlbuilder.postProject()); //todo?????
        form.setMethod("Post");
        form.setEnctype("multipart/form");

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
