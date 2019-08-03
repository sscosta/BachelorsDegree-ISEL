package pt.isel.ls.Commands.PostCommands;

import pt.isel.ls.Commands.Command;
import pt.isel.ls.Controller.ConnectionProvider;
import pt.isel.ls.Controller.RequestContext;
import pt.isel.ls.DAO.Project.ProjectDAO;
import pt.isel.ls.Exceptions.CommandException;
import pt.isel.ls.Exceptions.InvalidParamException;
import pt.isel.ls.Model.Project;
import pt.isel.ls.ResultType.Result;
import pt.isel.ls.DAO.Project.ProjectDAOImpl;
import pt.isel.ls.ResultType.ResultGet.ResultGetProjects;
import pt.isel.ls.ResultType.ResultPost.ResultPostProjects;

import java.sql.Connection;
import java.sql.Date;
import java.util.List;
import java.util.Map;

public class PostProjects implements Command {
    private final String NAME = "name";
    private final String DESCRIPTION = "description";

    @Override
    public Result execute(RequestContext requestContext, ConnectionProvider con) throws CommandException {
        Map paramMap = requestContext.getParamMap();
        Connection connection = con.getConnection();
        long id;
        if (isParamValid(paramMap)) {
            ProjectDAO projectDAO = new ProjectDAOImpl(connection);
            Date creationDate = new Date(System.currentTimeMillis());
            String name = (String) paramMap.get(NAME);

            List<Project> projects = projectDAO.getAll();
            for (Project project : projects){
                if(project.name.equals(name))
                    return new ResultGetProjects(projects,name);
            }

            id = projectDAO.create(new Project.ProjectBuilder().withName(
                    String.valueOf(name))
                    .withDescription(String.valueOf(paramMap.get(DESCRIPTION)))
                    .withCreationDate(creationDate).build());
            return new ResultPostProjects(projectDAO.getSingleProject((int) id));
        } else
            throw new InvalidParamException("Parameter Missing");
    }

    private boolean isParamValid(Map paramMap) throws InvalidParamException {
        if (paramMap.size() < 2) return false;
        if (paramMap.containsKey(NAME)) {
            if (paramMap.containsKey(DESCRIPTION))
                return true;
            else
                throw new InvalidParamException("Parameter invalid. Use \"" + DESCRIPTION + "\"");
        } else
            throw new InvalidParamException("Parameter invalid. Use \"" + NAME + "\"");
    }
}
