package pt.isel.ls.Commands.Servlets;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import pt.isel.ls.Controller.ConnectionProvider;
import pt.isel.ls.Controller.Parser;
import pt.isel.ls.Controller.RequestContext;
import pt.isel.ls.Controller.Router;
import pt.isel.ls.Exceptions.CommandException;
import pt.isel.ls.Exceptions.InvalidParamException;
import pt.isel.ls.Exceptions.SQLConnectionException;
import pt.isel.ls.Main;
import pt.isel.ls.PresentationLayer.View.Html.ViewsForStatusCodes.Http404;
import pt.isel.ls.PresentationLayer.View.Html.ViewsForStatusCodes.Http500;
import pt.isel.ls.PresentationLayer.View.Html.ViewsForStatusCodes.Http400;
import pt.isel.ls.ResultType.Result;

import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.IOException;
import java.io.OutputStream;
import java.lang.reflect.InvocationTargetException;
import java.nio.charset.Charset;
import java.sql.SQLException;

public class WebServerServlet extends HttpServlet {

    private static final Logger logger = LoggerFactory.getLogger(WebServerServlet.class);
    private static final String TAG = "ISSUE MANAGEMENT SYSTEM";
    private ConnectionProvider connectionProvider;
    private Router router;

    public WebServerServlet(ConnectionProvider connectionProvider) {
        this.connectionProvider = connectionProvider;
        this.router = new Router();
    }

    @Override
    public void doGet(HttpServletRequest req, HttpServletResponse resp) throws IOException {
        doMethod(req, resp);
    }

    @Override
    protected void doPost(HttpServletRequest req, HttpServletResponse resp) throws IOException {
        doMethod(req, resp);
//        try {
//            logger.info("{} on '{}' (path='{}') with accept:'{}', instance {}",
//                    req.getMethod(), req.getRequestURI(), req.getPathInfo(), req.getHeader("Accept"),
//                    this.hashCode());
//
//            Charset utf8 = Charset.forName("utf-8");
//            resp.setContentType(String.format("text/html; charset=%s", utf8.name()));
//            RequestContext context;
//            String respBody = null;
//            byte[] respBodyBytes;
//
//            context = new RequestContext(new Parser(req));
//
//            respBody = Main.getResponseFromResult(
//                    context,
//                    router.getCommand(
//                            context.getMethod(),
//                            context.getPath(),
//                            context.getParamMap()
//                    )
//                            .execute(context, connectionProvider));
//            respBodyBytes = respBody.getBytes(utf8);
//            resp.setStatus(200);
//            resp.setContentLength(respBodyBytes.length);
//            OutputStream os = resp.getOutputStream();
//            os.write(respBodyBytes);
//            os.close();
//            logger.info("Response sent");
//            connectionProvider.commit();
//            resp.setStatus(303);
//            logger.info("POST method executed");
//        } catch (InvalidParamException e) {
//            resp.setStatus(400);
//            logger.warn(TAG, "Exception while handling the request due to no such parameter", e);
//        } catch (CommandException e) {
//            resp.setStatus(404);
//            logger.info(TAG, "Exception while handling the post request due to no such command", e);
//        } catch (SQLException e) {
//            resp.setStatus(500);
//            logger.error(TAG, "Exception while trying to commit transaction.", e);
//        } catch (NoSuchMethodException e) {
//            e.printStackTrace();
//        } catch (InstantiationException e) {
//            e.printStackTrace();
//        } catch (IllegalAccessException e) {
//            e.printStackTrace();
//        } catch (InvocationTargetException e) {
//            e.printStackTrace();
//        } finally {
//            try {
//                connectionProvider.closeConnection();
//            } catch (SQLException e) {
//                resp.setStatus(500);
//                logger.error(TAG, "Unexpected exception while trying to close connection", e);
//            }
//        }
    }
    private void doMethod(HttpServletRequest req, HttpServletResponse resp) throws IOException {
        logger.info("{} on '{}' (path='{}') with accept:'{}', instance {}",
                req.getMethod(), req.getRequestURI(), req.getPathInfo(), req.getHeader("Accept"),
                this.hashCode());

        Charset utf8 = Charset.forName("utf-8");
        resp.setContentType(String.format("text/html; charset=%s", utf8.name()));
        RequestContext context;
        String respBody = null;
        byte[] respBodyBytes;
        try {
            context = new RequestContext(new Parser(req));

            respBody = Main.getResponseFromResult(
                    context,
                    router.getCommand(
                            context.getMethod(),
                            context.getPath(),
                            context.getParamMap()
                    )
                            .execute(context, connectionProvider));
            //respBodyBytes = respBody.getBytes(utf8);
            resp.setStatus(200);
            //resp.setContentLength(respBodyBytes.length);
            //OutputStream os = resp.getOutputStream();
            //os.write(respBodyBytes);
            //os.close();
            connectionProvider.commit();
            logger.info("Response sent");
        } catch (InvalidParamException e) {
            respBody = new Http404(null, null, e.getMessage()).getHtml();
            resp.setStatus(400);
            logger.warn("Exception while handling the request due to no such parameter", e);
        } catch (InvocationTargetException e) {
            respBody = new Http404(null, null, e.getMessage()).getHtml();
            resp.setStatus(404);
            logger.warn("Exception while handling the request due to no such view", e);
        } catch (NoSuchMethodException e) {
            respBody = new Http404(null, null, e.getMessage()).getHtml();
            resp.setStatus(400);
            logger.warn("Exception while handling the request due to no such method", e);
        } catch (InstantiationException e) {
            respBody = new Http400(null, null, e.getMessage()).getHtml();
            resp.setStatus(400);
            logger.warn("Exception while handling the request trying to instantiate", e);
        } catch (IllegalAccessException e) {
            respBody = new Http400(null, null, e.getMessage()).getHtml();
            logger.warn("Exception while handling the request due to Illegal Access", e);
        } catch (SQLConnectionException e) {
            respBody = new Http500(null, null, e.getMessage()).getHtml();
            resp.setStatus(500);
            logger.error("Unexpected exception while handling the request", e);
        } catch (CommandException e) {
            respBody = new Http404(null, null, e.getMessage()).getHtml();
            resp.setStatus(404);
            logger.warn("Exception while handling the request due to no such command", e);
        } catch (SQLException e) {
            resp.setStatus(500);
            logger.error(TAG, "Exception while trying to commit transaction.", e);
        } finally {
            try {
                connectionProvider.closeConnection();
            } catch (SQLException e) {
                respBody = new Http500(null, null, e.getMessage()).getHtml();
                resp.setStatus(500);
                logger.error("Unexpected exception while trying to close connection", e);
            }
            respBodyBytes = respBody.getBytes(utf8);
            resp.setContentLength(respBodyBytes.length);
            OutputStream os = resp.getOutputStream();
            os.write(respBodyBytes);
            os.close();

        }
    }


}
