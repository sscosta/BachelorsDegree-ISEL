package pt.isel.ls.Commands;

import pt.isel.ls.Controller.ConnectionProvider;
import pt.isel.ls.Controller.RequestContext;
import pt.isel.ls.Exceptions.CommandException;
import pt.isel.ls.ResultType.Result;

import java.util.Date;

public class GetTime implements Command {
    @Override
    public Result execute(RequestContext requestContext, ConnectionProvider connection) throws CommandException {
        Date now = new Date();
        System.out.println(now);
        return null;
    }
}
