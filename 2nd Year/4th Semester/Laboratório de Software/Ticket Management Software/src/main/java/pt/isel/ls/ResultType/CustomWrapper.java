package pt.isel.ls.ResultType;

import pt.isel.ls.Model.Comment;
import pt.isel.ls.Model.Label;
import pt.isel.ls.Model.Project;
import pt.isel.ls.Model.ProjectLabel;

import java.util.LinkedList;
import java.util.List;

public class CustomWrapper {
    private String label;
    private int pid;
    private Object value;
    private LinkedList<Comment> list;
    private int iid;
    private LinkedList<ProjectLabel> projectLabelList;
    private LinkedList<Label> issueLabelList;


    public CustomWrapper(Object value, int pid) {
        this.pid = pid;
        this.value = value;
    }

    public CustomWrapper(Comment comment, LinkedList commentList) {
        value = comment;
        list = commentList;
    }

    public CustomWrapper(int pid, int iid, LinkedList<ProjectLabel> projectLabelList, LinkedList<Label> issueLabelList) {
        this.pid = pid;
        this.iid = iid;
        this.projectLabelList = projectLabelList;
        this.issueLabelList = issueLabelList;
    }

    public CustomWrapper(int projectID, int issueID, LinkedList<ProjectLabel> projectLabelList, LinkedList<Label> labelList, String label) {
        this.pid = projectID;
        this.iid = issueID;
        this.projectLabelList = projectLabelList;
        this.issueLabelList = labelList;
        this.label = label;
    }

    public int getIid() {
        return iid;
    }

    public LinkedList<ProjectLabel> getProjectLabelList() {
        return projectLabelList;
    }

    public LinkedList<Label> getIssueLabelList() {
        return issueLabelList;
    }

    public int getPid() {
        return pid;
    }

    public Object getValue() {
        return value;
    }

    public LinkedList<Comment> getCommentList() {
        return list;
    }

    public String getLabel(){return label;}
}
