package pt.isel.ls.Commands.PostCommands;

import pt.isel.ls.Commands.Command;
import pt.isel.ls.Controller.ConnectionProvider;
import pt.isel.ls.Controller.RequestContext;
import pt.isel.ls.DAO.Label.LabelDAO;
import pt.isel.ls.DAO.Label.LabelDAOImpl;
import pt.isel.ls.DAO.ProjectLabel.ProjectLabelDAO;
import pt.isel.ls.DAO.ProjectLabel.ProjectLabelDAOImpl;
import pt.isel.ls.Exceptions.CommandException;
import pt.isel.ls.DAO.Project.ProjectDAOImpl;
import pt.isel.ls.Exceptions.InvalidParamException;
import pt.isel.ls.Model.Label;
import pt.isel.ls.Model.ProjectLabel;
import pt.isel.ls.ResultType.Result;
import pt.isel.ls.ResultType.ResultGet.ResultGetLabelsInProject;
import pt.isel.ls.ResultType.ResultPost.ResultPostLabel;

import java.io.UnsupportedEncodingException;
import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.SQLException;
import java.util.LinkedList;
import java.util.Map;

public class PostLabelInProject implements Command {
    @Override
    public Result execute(RequestContext requestContext, ConnectionProvider connectionProvider) throws CommandException {
        Map paramMap = requestContext.getParamMap();
        String[] labelType = {"name", "color"};

        validateLabels(paramMap, labelType);

        int projectID = (int) paramMap.get("projectID");
        String labelName = String.valueOf(paramMap.get(labelType[0]));
        String color = String.valueOf(paramMap.get(labelType[1]));

        Connection connection = connectionProvider.getConnection();

        ProjectDAOImpl projectDAOImpl = new ProjectDAOImpl(connection);
        String projectName = projectDAOImpl.getSingleProject(projectID).name;


        String query = "insert into ProjectLabel (projectName,labelDesc,color) " +
                "values(?,?,?)";
        try {

            ProjectLabelDAO projectLabelDAO = new ProjectLabelDAOImpl(connection);
            LinkedList<ProjectLabel> labelList = projectLabelDAO.getLabelsForProject(projectName);
            for(ProjectLabel pl : labelList){
                if(pl.labelDesc.equals(labelName))
                    return new ResultGetLabelsInProject(labelList,projectID, labelName);
            }
            LabelDAO labelDAO = new LabelDAOImpl(connection);

            labelDAO.insert(new Label.LabelBuilder().withDescription(labelName).build());

            PreparedStatement ps = connection.prepareStatement(
                    query);
            ps.setString(1, projectName);
            ps.setString(2, labelName);
            ps.setString(3, color);
            ps.execute();

            ProjectLabel label = new ProjectLabel.ProjectLabelBuilder().withProjectName(projectName)
                    .withLabelDesc(labelName).withColor(color).build();

            return new ResultPostLabel(label, projectID);

        } catch (SQLException e) {
            throw new InvalidParamException("Unable to post Label " +
                    e.getMessage());
        }
    }

    private void validateLabels(Map paramMap, String[] labelType) throws CommandException {
        for (int i = 0; i < labelType.length; i++) {
            if(!paramMap.containsKey(labelType[i]))
                throw new CommandException("Missing param -"+labelType[i]);
        }
    }

}
