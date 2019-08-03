package pt.isel.ls.DAO.Label;


import pt.isel.ls.Exceptions.InvalidParamException;
import pt.isel.ls.Model.Label;

import java.sql.SQLException;
import java.util.List;

public interface LabelDAO {

    Label getLabelWithName(String label, String IssueID) throws InvalidParamException;
    List<Label> getAllLabels() throws InvalidParamException;

    boolean insert(Label build) throws SQLException;
}
