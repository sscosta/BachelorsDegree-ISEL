package pt.isel.ls.Commands;

import pt.isel.ls.Controller.ConnectionProvider;
import pt.isel.ls.Controller.RequestContext;
import pt.isel.ls.Exceptions.CommandException;
import pt.isel.ls.ResultType.Result;
import pt.isel.ls.ResultType.ResultListOptions;

public class ListOptions implements Command {

    @Override
    public Result execute(RequestContext requestContext, ConnectionProvider connection) throws CommandException {
        return new ResultListOptions(requestContext.getMethodPathList());
    }
}
