package pt.isel.ls.ResultType.ResultGet;

import pt.isel.ls.Model.Label;
import pt.isel.ls.Model.ProjectLabel;
import pt.isel.ls.ResultType.CustomWrapper;
import pt.isel.ls.ResultType.Result;

import java.util.LinkedList;

public class ResultGetLabelsInIssue implements Result {
    private String label;
    private CustomWrapper wrapper;

    public ResultGetLabelsInIssue(int projectID, int issueID, LinkedList<ProjectLabel> projectLabelList, LinkedList<Label> labelList) {
        wrapper = new CustomWrapper(projectID, issueID, projectLabelList, labelList);
    }

    public ResultGetLabelsInIssue(int projectID, int issueID, LinkedList<ProjectLabel> projectLabelList, LinkedList<Label> labelList, String label) {
        wrapper = new CustomWrapper(projectID,issueID,projectLabelList,labelList,label);
    }

    @Override
    public Object getValue() {
        return wrapper;
    }
}
