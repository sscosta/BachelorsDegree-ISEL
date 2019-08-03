package pt.isel.ls.PresentationLayer.View.Html;

import pt.isel.ls.Controller.RequestContext;
import pt.isel.ls.Model.Label;
import pt.isel.ls.PresentationLayer.Formatter.Html.Html;
import pt.isel.ls.PresentationLayer.Formatter.Html.HtmlBuilder;
import pt.isel.ls.PresentationLayer.Formatter.Html.tags.*;
import pt.isel.ls.PresentationLayer.View.ViewHtml;
import pt.isel.ls.ResultType.Result;
import pt.isel.ls.ResultType.ResultSearchProjects;

import java.util.*;

public class SearchView extends ViewHtml {
    private static final String TITLE = "Search page";
    List<Label> labels;
    Html html;
    Body body;

    public SearchView(Result result, RequestContext requestContext) {
        super(result, requestContext);
        labels =((ResultSearchProjects) result).getLabelList();
    }

    @Override
    public String getHtml() {
        html = HtmlBuilder.buildHtml(TITLE);
        body = new Body();
        html.setBody(body);

        H h4 = new H(4);
        h4.setText("Search project issues");
        body.addElement(h4);

        addSearchForm();

        BR br = new BR();
        A back = new A("/projects/"+result.getValue());
        back.setText("Back to Project Detail");
        body.addElement(back);
        body.addElement(br);

        A backToRoot = new A("/");
        backToRoot.setText("Back to root");
        body.addElement(backToRoot);

        return html.toString();
    }

    private void addSearchForm() {
        Form searchForm = new Form();
        searchForm.setMethod("GET");
        searchForm.setAction("issues/");

        Select stateSelect = new Select("state");
        searchForm.addElement(stateSelect);

        Option stateOpt1 = new Option("All","all");
        stateSelect.addElement(stateOpt1);

        Option stateOpt2 = new Option("Open","open");
        stateSelect.addElement(stateOpt2);

        Option stateOpt3 = new Option("Closed","closed");
        stateSelect.addElement(stateOpt3);

        Select sortSelect = new Select("sort");
        searchForm.addElement(sortSelect);

        Option sortOpt1 = new Option("Comments","comments");
        sortSelect.addElement(sortOpt1);

        Option sortOpt2 = new Option("Created","created");
        sortSelect.addElement(sortOpt2);

        Option sortOpt3 = new Option("Updated","updated");
        sortSelect.addElement(sortOpt3);

        Select directionSelect = new Select("direction");
        searchForm.addElement(directionSelect);

        Option directionOpt1 = new Option("Asc", "asc");
        directionSelect.addElement(directionOpt1);

        Option directionOpt2 = new Option("Desc", "desc");
        directionSelect.addElement(directionOpt2);

        Input searchButton = new Input("Search");
        searchButton.setType("submit");
        searchForm.addElement(searchButton);

        body.addElement(searchForm);
    }
}
