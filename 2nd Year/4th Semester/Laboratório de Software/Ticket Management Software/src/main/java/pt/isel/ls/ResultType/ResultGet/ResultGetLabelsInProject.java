package pt.isel.ls.ResultType.ResultGet;

import pt.isel.ls.Model.ProjectLabel;
import pt.isel.ls.ResultType.Result;

import java.util.LinkedList;
import java.util.List;

public class ResultGetLabelsInProject implements Result {
    public int pid;
    private LinkedList<ProjectLabel> labelLinkedList;
    private String labelName;

    public ResultGetLabelsInProject(LinkedList<ProjectLabel> labelList, int projectID) {
        labelLinkedList=labelList;
        this.pid=projectID;
    }

    public ResultGetLabelsInProject(LinkedList<ProjectLabel> labelLinkedList, int pid, String labelName) {
        this.pid = pid;
        this.labelLinkedList = labelLinkedList;
        this.labelName = labelName;
    }

    public LinkedList<ProjectLabel> getValue() {
        return labelLinkedList;
    }

    public String getLabelName() {
        return labelName;
    }
}
