package pt.isel.ls.ResultType.ResultPost;

import pt.isel.ls.Model.Label;
import pt.isel.ls.Model.ProjectLabel;
import pt.isel.ls.ResultType.CustomWrapper;
import pt.isel.ls.ResultType.Result;

import java.util.LinkedList;

public class ResultPostIssueLabel implements Result {
    private CustomWrapper wrapper;

    public ResultPostIssueLabel(int projectID, int issueID, LinkedList<ProjectLabel> projectLabelList, LinkedList<Label> labelList) {
        wrapper = new CustomWrapper(projectID, issueID, projectLabelList, labelList);
    }

    @Override
    public Object getValue() {
        return wrapper;
    }
}
