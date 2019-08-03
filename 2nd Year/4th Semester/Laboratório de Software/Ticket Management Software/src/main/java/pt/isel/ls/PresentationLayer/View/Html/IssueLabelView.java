package pt.isel.ls.PresentationLayer.View.Html;

import pt.isel.ls.Controller.RequestContext;
import pt.isel.ls.Model.Label;
import pt.isel.ls.Model.ProjectLabel;
import pt.isel.ls.PresentationLayer.Formatter.Html.Html;
import pt.isel.ls.PresentationLayer.Formatter.Html.HtmlBuilder;
import pt.isel.ls.PresentationLayer.Formatter.Html.tags.*;
import pt.isel.ls.PresentationLayer.View.ViewHtml;
import pt.isel.ls.ResultType.CustomWrapper;
import pt.isel.ls.ResultType.Result;
import pt.isel.ls.ResultType.ResultGet.ResultGetLabelsInIssue;

import java.util.LinkedList;

public class IssueLabelView extends ViewHtml {
    private static final String TITLE = "LS : G1 : Issue Label View";
    private final LinkedList<Label> listOfIssueLabels;
    private final LinkedList<ProjectLabel> listOfProjectLabels;
    private final int pid;
    private final int iid;
    private final String label;

    Html html;
    Body body;

    public IssueLabelView(Result result, RequestContext requestContext) {
        super(result, requestContext);
        CustomWrapper res = (CustomWrapper) result.getValue();
        listOfIssueLabels = res.getIssueLabelList();
        listOfProjectLabels = res.getProjectLabelList();
        pid = res.getPid();
        iid = res.getIid();
        label = res.getLabel();
    }

    @Override
    public String getHtml() {
        html = HtmlBuilder.buildHtml(TITLE);
        body = new Body();
        A backToRoot = new A("/");
        backToRoot.setText("Back to root");
        body.addElement(backToRoot);

        if(!listOfIssueLabels.isEmpty())
            body.addElement(addTableOfLabels());
        else{
            P p = new P();
            p.setText("No labels associated to this issue yet.");
            body.addElement(p);
        }

        BR br = new BR();
        body.addElement(br);

        addLabelForm();

        A backToIssueIid = new A("/projects/" + pid + "/issues/" + iid);
        backToIssueIid.setText("Back to Issue " + iid);
        body.addElement(backToIssueIid);

        body.addElement(br);
        body.addElement(br);

        if(label!=null){
            A noProject = new A();
            noProject.setText("Unable to add -"+label+"-. Already exist");
            body.addElement(noProject);
            body.addElement(br);
            body.addElement(br);
        }

        A backToProjectPid = new A("/projects/" + pid);
        backToProjectPid.setText("Back to Project " + pid);
        body.addElement(backToProjectPid);

        html.setBody(body);
        return html.toString();
    }

    private void addLabelForm() {
        if (listOfProjectLabels == null || listOfProjectLabels.isEmpty()) {
            P pNoLabels = new P("No labels yet associated with project " + pid);
            body.addElement(pNoLabels);
        } else {

            Form addLabelForm = new Form();
            addLabelForm.setMethod("POST");

            Select labelSelect = new Select("label");
            int count = 0;
            for (ProjectLabel pl : listOfProjectLabels) {
                if (!containsLabel(listOfIssueLabels, pl.labelDesc)) {
                    labelSelect.addElement(new Option(pl.labelDesc, pl.labelDesc));
                    count++;
                }
            }

            if(count!=0){
                addLabelForm.addElement(labelSelect);
                Input addButton = new Input("Add label");
                addButton.setType("submit");
                addButton.setText("Add label");
                addLabelForm.addElement(addButton);

            body.addElement(addLabelForm);
            }
            else
                body.addElement(new P("No labels in project to submit"));
        }
    }

    private Table addTableOfLabels() {
        H h4 = new H(4);
        h4.setText("Labels for issue #" + iid + ":");
        body.addElement(h4);
        Table tableOfLabels = new Table();
        createLabelTableHeader(tableOfLabels);
        for(Label l : listOfIssueLabels){
            addLabel(l,tableOfLabels);
        }
        return tableOfLabels;
    }

    private void createLabelTableHeader(Table tableOfLabels) {
        TableRow th = new TableRow();
        TableHeader tdh1 = new TableHeader();
        tdh1.setId("Column1");
        tdh1.setText("Description");
        th.addElement(tdh1);
        tableOfLabels.addRow(th);
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


    public boolean containsLabel(final LinkedList<Label> IssueLabelList, final String label){
        return IssueLabelList.stream().map(Label::getDescription).filter(label::equals).findFirst().isPresent();
    }

}