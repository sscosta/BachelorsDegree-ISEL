package pt.isel.ls.Commands;

import pt.isel.ls.Controller.ConnectionProvider;
import pt.isel.ls.Controller.RequestContext;
import pt.isel.ls.DAO.Label.LabelDAO;
import pt.isel.ls.DAO.Label.LabelDAOImpl;
import pt.isel.ls.Exceptions.CommandException;
import pt.isel.ls.Model.Label;
import pt.isel.ls.ResultType.Result;
import pt.isel.ls.ResultType.ResultSearchProjects;

import java.util.List;

public class SearchProjects implements Command {
    @Override
    public Result execute(RequestContext requestContext, ConnectionProvider connectionPro) throws CommandException{

        LabelDAO labelDAO = new LabelDAOImpl(connectionPro.getConnection());
        List<Label> labels = labelDAO.getAllLabels();
        ResultSearchProjects res = new ResultSearchProjects((int) requestContext.getParamMap().get("projectID"));
        res.setLabelList(labels);
        return res;
    }
}
