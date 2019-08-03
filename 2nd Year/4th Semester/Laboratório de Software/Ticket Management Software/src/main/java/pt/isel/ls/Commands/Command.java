package pt.isel.ls.Commands;

import pt.isel.ls.Controller.ConnectionProvider;
import pt.isel.ls.Controller.RequestContext;
import pt.isel.ls.Exceptions.CommandException;
import pt.isel.ls.ResultType.Result;


public interface Command {
    Result execute(RequestContext requestContext, ConnectionProvider connection) throws CommandException;
}
