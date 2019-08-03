package pt.isel.ls.DAO.Label;

import pt.isel.ls.Exceptions.InvalidParamException;
import pt.isel.ls.Model.Label;

import java.sql.*;
import java.util.LinkedList;
import java.util.List;


public class LabelDAOImpl implements LabelDAO {


    private final Connection connection;

    public LabelDAOImpl(Connection connection) {
        this.connection = connection;
    }

    final String GET_LABEL_FROM_DESCRIPTION_QUERY = "select ProjectLabel.labelDesc,ProjectLabel.color from IssueLabel inner join ProjectLabel on IssueLabel.labelDesc= ProjectLabel.labelDesc where IssueLabel.labelDesc = ? and IssueLabel.projectName = ? ";

    @Override
    public Label getLabelWithName(String label, String projectName) throws InvalidParamException {
        Label resultLabel = null;
        try {
            PreparedStatement ps = connection.prepareStatement(GET_LABEL_FROM_DESCRIPTION_QUERY);
            ps.setString(1, label);
            ps.setString(2, projectName);
            ResultSet rs = ps.executeQuery();
            while (rs.next()) {
                String description = rs.getString(1);
                String color = rs.getString(2);
                resultLabel = new Label.LabelBuilder()
                        .withDescription(description)
                        .withColor(color)
                        .build();
            }
            return resultLabel;
        } catch (SQLException e) {
            throw new InvalidParamException("Unable to get labels " + e.getMessage());
        }
    }

    private static final String GET_ALL_LABELS_QUERY = "select * from Label";

    @Override
    public List<Label> getAllLabels() throws InvalidParamException {
        List<Label> allLabels = new LinkedList<Label>();
        try {
            Statement statement = connection.createStatement();
            ResultSet rs = statement.executeQuery(GET_ALL_LABELS_QUERY);
            while (rs.next()) {
                String description = rs.getString(1);
                Label label = new Label.LabelBuilder().withDescription(description).build();
                allLabels.add(label);
            }
            return allLabels;
        } catch (SQLException e) {
            throw new InvalidParamException("Unable to get labels " + e.getMessage());
        }
    }

    final String QUERY_TO_INSERT_LABEL = "insert into Label (description) values (?)";
    @Override
    public boolean insert(Label label) throws SQLException {
        if(!existsInDB(label)) {
            PreparedStatement ps = connection.prepareStatement(QUERY_TO_INSERT_LABEL);
            ps.setString(1, label.description);
            return ps.execute();
        }
        else return false;
    }

    final String QUERY_TO_CHECK_IF_LABEL_EXISTS = "select * from Label where description=?";
    private boolean existsInDB(Label label) throws SQLException {
        PreparedStatement ps = connection.prepareStatement(QUERY_TO_CHECK_IF_LABEL_EXISTS);
        ps.setString(1,label.description);
        ResultSet rs = ps.executeQuery();
        return rs.next();
    }
}
