package pt.isel.ls.Commands.GetCommands;

import pt.isel.ls.Commands.Command;
import pt.isel.ls.Controller.ConnectionProvider;
import pt.isel.ls.Controller.RequestContext;
import pt.isel.ls.Exceptions.CommandException;
import pt.isel.ls.Exceptions.InvalidParamException;
import pt.isel.ls.Model.Project;
import pt.isel.ls.ResultType.Result;
import pt.isel.ls.ResultType.ResultGet.ResultGetProjectsOrderedByClosedIssueCount;

import java.sql.Connection;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

public class GetProjectsOrderedByClosedIssueCount implements Command {

    private static final String CLOSED_ISSUE_COUNT_QUERY = "select Project.name, Project.description, Project.creationDate, Project.id, count(*) as OpenIssueCount\n" +
            "\tfrom Project\n" +
            "\tinner join\n" +
            "\t(select *\n" +
            "\t\tfrom Issue\n" +
            "\t\twhere statusDesc='closed'\n" +
            "\t\t) as I\n" +
            "\t on Project.name=I.projectName group by Project.name,Project.description, Project.creationDate, Project.id\n" +
            "\t order by OpenIssueCount";

    @Override
    public Result execute(RequestContext requestContext, ConnectionProvider con) throws CommandException {
        Map paramMap = requestContext.getParamMap();
        Connection connection = con.getConnection();
        List<Project> projectsOrderedByOpenIssueCount = new LinkedList<>();
        try {
            Statement statement = connection.createStatement();
            ResultSet resultSet = statement.executeQuery(CLOSED_ISSUE_COUNT_QUERY + " " + (paramMap.get("direction") == null ? "" : paramMap.get("direction")));
            while (resultSet.next()) {
                String name = resultSet.getString(1);
                String description = resultSet.getString(2);
                java.sql.Date creationDate = resultSet.getDate(3);
                int id = resultSet.getInt(4);

                Project project = new Project.ProjectBuilder().withName(name).withDescription(description).withID(id).withCreationDate(creationDate).build();
                projectsOrderedByOpenIssueCount.add(project);
            }
            return new ResultGetProjectsOrderedByClosedIssueCount(projectsOrderedByOpenIssueCount);
        } catch (SQLException e) {
            throw new InvalidParamException("Unable to get project ordered by closed issue " +
                    e.getMessage());
        }
    }
}
