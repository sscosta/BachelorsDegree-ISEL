package pt.isel.ls.DAO.IssueLabel;

import pt.isel.ls.Exceptions.InvalidParamException;
import pt.isel.ls.Exceptions.LabelNotAllowedInProjectException;
import pt.isel.ls.Model.IssueLabel;
import pt.isel.ls.Model.Label;

import java.util.LinkedList;

public interface IssueLabelDAO {


    boolean addLabelToIssue(IssueLabel issueLabel) throws LabelNotAllowedInProjectException;

    boolean deleteLabelFromIssue(IssueLabel issueLabel) throws InvalidParamException;

    LinkedList<Label> getLabels(int id) throws InvalidParamException;
}
