package pt.isel.ls.Commands.GetCommands;

import pt.isel.ls.Commands.Command;
import pt.isel.ls.Controller.ConnectionProvider;
import pt.isel.ls.Controller.RequestContext;
import pt.isel.ls.Exceptions.CommandException;
import pt.isel.ls.Exceptions.InvalidParamException;
import pt.isel.ls.Model.Project;
import pt.isel.ls.ResultType.Result;
import pt.isel.ls.ResultType.ResultGet.ResultGetProjectsOrderedByCreationDate;

import java.sql.Connection;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;


public class GetProjectsOrderedByCreationDate implements Command {
    private static final String ORDER_BY_CREATION_DATE_QUERY = "select * from Project order by creationDate";

    public GetProjectsOrderedByCreationDate() {
    }

    @Override
    public Result execute(RequestContext requestContext, ConnectionProvider con) throws CommandException {
        Map paramMap = requestContext.getParamMap();
        Connection connection = con.getConnection();
        List<Project> projectsOrderedByCreationDate = new LinkedList<>();
        try {
            Statement statement = connection.createStatement();
            ResultSet resultSet = statement.executeQuery(ORDER_BY_CREATION_DATE_QUERY + " " + (paramMap.get("direction") == null ? "" : paramMap.get("direction")));
            while (resultSet.next()) {
                String name = resultSet.getString(1);
                String description = resultSet.getString(2);
                java.sql.Date creationDate = resultSet.getDate(3);
                int id = resultSet.getInt(4);

                Project project = new Project.ProjectBuilder().withName(name).withDescription(description).withID(id).withCreationDate(creationDate).build();
                projectsOrderedByCreationDate.add(project);
            }
            return new ResultGetProjectsOrderedByCreationDate(projectsOrderedByCreationDate);
        } catch (SQLException e) {
            throw new InvalidParamException("Unable to get project ordered by creation date " +
                    e.getMessage());
        }
    }
}
