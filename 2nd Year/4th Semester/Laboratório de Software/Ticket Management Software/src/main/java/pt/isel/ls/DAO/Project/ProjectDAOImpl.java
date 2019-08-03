package pt.isel.ls.DAO.Project;

import pt.isel.ls.DAO.ProjectLabel.ProjectLabelDAO;
import pt.isel.ls.DAO.ProjectLabel.ProjectLabelDAOImpl;
import pt.isel.ls.Exceptions.CommandException;
import pt.isel.ls.Exceptions.InvalidParamException;
import pt.isel.ls.Model.Project;
import pt.isel.ls.Model.ProjectLabel;

import java.sql.*;
import java.util.LinkedList;
import java.util.List;

public class ProjectDAOImpl implements ProjectDAO {
    private final Connection connection;
    private static final String TABLE_NAME = "Project";


    public ProjectDAOImpl(Connection connection) {
        this.connection = connection;
    }

    @Override
    public List<Project> getAll() throws InvalidParamException {
        List<Project> allProjects = new LinkedList<>();
        try {
            Statement statement = connection.createStatement();
            ResultSet resultSet = statement.executeQuery("Select * from " + TABLE_NAME);
            while (resultSet.next()) {
                String name = resultSet.getString(1);
                String description = resultSet.getString(2);
                java.sql.Date creationDate = resultSet.getDate(3);
                int id = resultSet.getInt(4);

                Project project = new Project.ProjectBuilder().withName(name).withDescription(description).withID(id).withCreationDate(creationDate).build();
                allProjects.add(project);
            }
            return allProjects;
        } catch (SQLException e) {
            throw new InvalidParamException("Unable to get Projects " + e.getMessage());

        }
    }

    @Override
    public long create(Project project) throws InvalidParamException {
        PreparedStatement preparedStatement;
        long createdProjectID = 0;
        try {
            preparedStatement = connection.prepareStatement(
                    "Insert into " + TABLE_NAME + "(name,description,creationdate) values (?,?,?)", Statement.RETURN_GENERATED_KEYS);
            preparedStatement.setString(1, project.name);
            preparedStatement.setString(2, project.description);
            preparedStatement.setDate(3, project.creationDate);
            int affectedRows = preparedStatement.executeUpdate();
            if (affectedRows == 1) {
                ResultSet resultSet = preparedStatement.getGeneratedKeys();
                if (resultSet.next())
                    createdProjectID = resultSet.getLong(4);
            }

            return createdProjectID;
        } catch (SQLException e) {
            throw new InvalidParamException("Unable to create project " + e.getMessage());
        }
    }

    @Override
    public void update(Project project, String[] params) {

    }

    @Override
    public void delete(Project project) {

    }

    public Project getSingleProject(int id) throws CommandException {
        Project project = null;
        PreparedStatement preparedStatement = null;
        try {
            preparedStatement = connection.prepareStatement(
                    "select * from " + TABLE_NAME + " where id = ?");

            preparedStatement.setInt(1, id);

            ResultSet resultSet = preparedStatement.executeQuery();
            if (resultSet.next()) {
                String name = resultSet.getString(1);
                String description = resultSet.getString(2);
                java.sql.Date creationDate = resultSet.getDate(3);
                int idRes = resultSet.getInt(4);
                project = new Project.ProjectBuilder()
                        .withName(name).withDescription(description).withID(idRes).withCreationDate(creationDate).build();
            }

            if (project != null){
                ProjectLabelDAO projectLabelDAO = new ProjectLabelDAOImpl(connection);
                LinkedList<ProjectLabel> labelsInProject = projectLabelDAO.getLabelsForProject(project.name);
                if(labelsInProject!=null)
                    project.setLabels(labelsInProject);
                return project;
            }

            else
                throw new CommandException("Invalid Project Id");
        } catch (SQLException e) {
            throw new InvalidParamException("Unable to get project " + e.getMessage());
        }
    }
}
