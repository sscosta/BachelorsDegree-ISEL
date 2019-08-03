package pt.isel.ls.Commands.GetCommands;

import pt.isel.ls.Commands.Command;
import pt.isel.ls.Controller.ConnectionProvider;
import pt.isel.ls.Controller.RequestContext;
import pt.isel.ls.Exceptions.CommandException;
import pt.isel.ls.Exceptions.InvalidParamException;
import pt.isel.ls.Model.Project;
import pt.isel.ls.ResultType.Result;
import pt.isel.ls.ResultType.ResultGet.ResultGetProjectsOrderedByIssueCount;

import java.sql.Connection;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

public class GetProjectsOrderedByIssueCount implements Command {

    private static final String ORDER_BY_ISSUE_COUNT_QUERY = "select A.name as ProjectName, P.description, P.creationDate,P.id,issueCount\n" +
            "\tfrom(\n" +
            "\t\tselect Project.name,count(*) as issueCount\n" +
            "\t\tfrom\n" +
            "\t\t\tProject inner join Issue\n" +
            "\t\t\ton Project.name=projectName \n" +
            "\t\t\tgroup by Project.name\n" +
            "\t\t) as A\n" +
            "\tinner join\n" +
            "\tProject as P\n" +
            "\ton A.name=P.name" +
            "\torder by issueCount";

    @Override
    public Result execute(RequestContext requestContext, ConnectionProvider con) throws CommandException {
        Map paramMap = requestContext.getParamMap();
        Connection connection = con.getConnection();

        List<Project> projectsOrderedByIssueCount = new LinkedList<>();
        try {
            Statement statement = connection.createStatement();
            ResultSet resultSet = statement.executeQuery(
                    ORDER_BY_ISSUE_COUNT_QUERY + " " + (paramMap.get("direction") == null ? "" : paramMap.get("direction")));
            while (resultSet.next()) {
                String name = resultSet.getString(1);
                String description = resultSet.getString(2);
                java.sql.Date creationDate = resultSet.getDate(3);
                int id = resultSet.getInt(4);

                Project project = new Project.ProjectBuilder()
                        .withName(name)
                        .withDescription(description)
                        .withID(id)
                        .withCreationDate(creationDate).build();
                projectsOrderedByIssueCount.add(project);
            }
            return new ResultGetProjectsOrderedByIssueCount(projectsOrderedByIssueCount);
        } catch (SQLException e) {
            throw new InvalidParamException("Unable to get project ordered by issue count " +
                    e.getMessage());
        }
    }
}
