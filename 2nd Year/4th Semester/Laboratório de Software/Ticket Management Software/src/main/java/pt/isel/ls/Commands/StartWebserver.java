package pt.isel.ls.Commands;


import org.eclipse.jetty.server.Server;
import org.eclipse.jetty.servlet.ServletHandler;
import org.eclipse.jetty.servlet.ServletHolder;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import pt.isel.ls.Commands.Servlets.WebServerServlet;
import pt.isel.ls.Controller.ConnectionProvider;
import pt.isel.ls.Controller.RequestContext;
import pt.isel.ls.Exceptions.CommandException;
import pt.isel.ls.ResultType.Result;

import java.util.Map;

public class StartWebserver implements Command {

    private static final int DEFAULT_LISTEN_PORT = 8080;

    @Override
    public Result execute(RequestContext requestContext, ConnectionProvider connectionProvider) throws CommandException {
        System.setProperty("org.slf4j.simpleLogger.levelInBrackets", "true");

        Logger logger = LoggerFactory.getLogger(StartWebserver.class);
        logger.info("Starting web server...");

        Map paramMap = requestContext.getParamMap();
        final String PORT_KEY = "port";
        //int portParam = paramMap.containsKey(PORT_KEY) ? Integer.parseInt((String) paramMap.get(PORT_KEY)) : 0;
        //int port = IsValidPort(portParam) ? portParam : DEFAULT_LISTEN_PORT;
        int port = Integer.parseInt(System.getenv("PORT"));
        logger.info("Listening on port {}", port);
        Server server = new Server(port);
        ServletHandler handler = new ServletHandler();
        server.setHandler(handler);
        handler.addServletWithMapping(new ServletHolder(new WebServerServlet(connectionProvider)), "/*");
        try {
            server.start();
            System.out.println("Server started");
        } catch (Exception e) {
            logger.error("Server did not start");
            throw new CommandException("Webserver not started");
        }

        return null;
    }

    // valid ports should be between 1-65535
    private boolean IsValidPort(int port) {
        return (port >= 1 && port <= 65535);
    }
}
