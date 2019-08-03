package pt.isel.ls.ResultType;

import pt.isel.ls.Model.Label;

import java.util.List;

public class ResultSearchProjects implements Result {

    private int projectID;
    private List<Label> labelList;

    public ResultSearchProjects(int projectID) {
        this.projectID=projectID;
    }

    @Override
    public Object getValue() {
        return this.projectID;
    }

    public List<Label> getLabelList(){return this.labelList;}

    public void setLabelList(List<Label> labelList) {
        this.labelList = labelList;
    }
}
