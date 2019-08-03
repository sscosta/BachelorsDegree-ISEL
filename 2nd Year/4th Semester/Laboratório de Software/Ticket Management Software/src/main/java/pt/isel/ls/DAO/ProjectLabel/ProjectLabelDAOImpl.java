package pt.isel.ls.DAO.ProjectLabel;

import pt.isel.ls.Exceptions.InvalidParamException;
import pt.isel.ls.Model.ProjectLabel;

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.LinkedList;

public class ProjectLabelDAOImpl implements ProjectLabelDAO {

    private final Connection connection;
    private final String TABLE_NAME = "ProjectLabel";
    private final String GET_LABELS_FOR_PROJECT_QUERY = "select * from ProjectLabel where projectName =?";

    public ProjectLabelDAOImpl(Connection connection) {
        this.connection = connection;
    }


    @Override
    public LinkedList<ProjectLabel> getLabelsForProject(String projectName) throws InvalidParamException {
        LinkedList<ProjectLabel> allProjecLabels = new LinkedList<>();
        try {
            PreparedStatement preparedStatement = connection.prepareStatement(GET_LABELS_FOR_PROJECT_QUERY);
            preparedStatement.setString(1, projectName);
            ResultSet resultSet = preparedStatement.executeQuery();
            while (resultSet.next()) {
                String pName = resultSet.getString(1);
                String labelDesc = resultSet.getString(2);
                String color = resultSet.getString(3);
                ProjectLabel projectLabel = new ProjectLabel.ProjectLabelBuilder()
                        .withProjectName(pName)
                        .withLabelDesc(labelDesc)
                        .withColor(color)
                        .build();

                allProjecLabels.add(projectLabel);
            }
            return allProjecLabels;
        } catch (SQLException e) {
            throw new InvalidParamException("Unable to get Labels for Project " + e.getMessage());
        }
    }

    private static final String GET_LABEL_DETAIL_QUERY = "select * from projectLabel where projectName = ? and labelDesc = ?;";
    public ProjectLabel getLabelDetail(String label, String projectName)throws InvalidParamException {
        ProjectLabel resultLabel = null;
        try {
            PreparedStatement ps = connection.prepareStatement(GET_LABEL_DETAIL_QUERY);
            ps.setString(1, projectName);
            ps.setString(2, label);
            ResultSet rs = ps.executeQuery();
            while (rs.next()) {
                String projName = rs.getString(1);
                String description = rs.getString(2);
                String color = rs.getString(3);

                resultLabel = new ProjectLabel.ProjectLabelBuilder()
                        .withProjectName(projName)
                        .withLabelDesc(description)
                        .withColor(color)
                        .withProjectName(projectName)
                        .build();
            }
            return resultLabel;
        } catch (SQLException e) {
            throw new InvalidParamException("Unable to get labels " + e.getMessage());
        }
    }
}
