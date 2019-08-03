package pt.isel.ls;

import pt.isel.ls.Commands.*;
import pt.isel.ls.Controller.*;
import pt.isel.ls.Exceptions.CommandException;
import pt.isel.ls.Exceptions.SQLConnectionException;
import pt.isel.ls.PresentationLayer.Console.Print;
import pt.isel.ls.PresentationLayer.View.PlainTextView;
import pt.isel.ls.PresentationLayer.View.ViewFactory;
import pt.isel.ls.ResultType.Result;
import java.io.IOException;
import java.lang.reflect.InvocationTargetException;
import java.sql.SQLException;
import java.util.Scanner;

public class Main {
    static Router router;
    static Registrator registrator = new Registrator();

    public static void main(String[] args) throws CommandException {

        doRegistratorWork();
        ConnectionProvider connectionProvider = new ConnectionProvider();
        singleCmdExecution(new String [] {"LISTEN", "/"},connectionProvider);

//      breaks compatibility with phase 3
//        if(isInInteractiveMode(args)){
//            interactiveMode(connectionProvider);
//        } else {
//            singleCmdExecutionMode(args, connectionProvider);
//        }
    }

    private static void doRegistratorWork() {
        registrator.registerOperation();
        registrator.registerCriteria();
        registrator.registerCriteriaForIssues();
        registrator.registerCommands(router = new Router());

    }

    private static boolean isInInteractiveMode(String[] args) {
        return args.length==0;
    }

    private static void interactiveMode(ConnectionProvider connectionProvider) throws CommandException {
        while(true) {
            System.out.print("Insert Command\n>");
            Scanner scanner                 = new Scanner(System.in);
            String [] executionArguments = scanner.nextLine().split(" ");
            singleCmdExecution(executionArguments,connectionProvider);
        }
    }

    private static void singleCmdExecutionMode(String[] args,ConnectionProvider connectionProvider)throws  CommandException {
        singleCmdExecution(args,connectionProvider);
    }
    private static void singleCmdExecution(String[] args,ConnectionProvider connectionProvider) throws CommandException {
        try {
            if (isValid(args)) {
                Parser  parser = new Parser(args);
                RequestContext requestContext = new RequestContext(parser);

                Command cmd = router.getCommand(requestContext.getMethod(), requestContext.getPath(), requestContext.getParamMap());
                Result  res = cmd.execute(requestContext,connectionProvider);
                connectionProvider.commit();

                if (res!=null) {
                    String response = getResponseFromResult(requestContext, res);

                    OutputSelect outputSelect = new OutputSelect(response, requestContext);
                    outputSelect.writeResponse();
                }
            } else
                Print.message("Please enter a command in format: {method} {path} {parameters}");
        } catch (CommandException e) {
            Print.message(e.getMessage());
        } catch (SQLException e) {
            try {
                connectionProvider.rollback();
            } catch (SQLException e1) {
                throw new SQLConnectionException("Connection Failed");
            }
            Print.message(e.getMessage());
        } catch (NoSuchMethodException e) {
            e.printStackTrace();
        } catch (InstantiationException e) {
            e.printStackTrace();
        } catch (IllegalAccessException e) {
            e.printStackTrace();
        } catch (InvocationTargetException e) {
            e.printStackTrace();
        } catch (IOException e) {
            e.printStackTrace();
        } finally {
            try {
                connectionProvider.closeConnection();
            } catch (SQLException e) {
                throw new SQLConnectionException("Connection Failed");
            }
        }
    }

    public static String getResponseFromResult(RequestContext requestContext, Result res) throws InvocationTargetException, NoSuchMethodException, InstantiationException, IllegalAccessException {
        String visualizationMode = (String) requestContext.getParamMap().get("accept");
        if(visualizationMode!= null && visualizationMode.equals("text/plain"))
            return new PlainTextView(res, requestContext).getText();
        return ViewFactory.getHtmlView(res, requestContext);
    }

    private static boolean isValid(String[] args) throws CommandException {
        if(args.length < 2) {
            throw new CommandException("Invalid Number of arguments");
        }
        else return true;
    }
}
