package org.isel.jingle;

import io.vertx.core.Handler;
import io.vertx.core.Vertx;
import io.vertx.core.http.HttpServer;
import io.vertx.core.http.HttpServerResponse;
import io.vertx.ext.web.Router;
import io.vertx.ext.web.RoutingContext;
import org.isel.jingle.Controller.*;

public class WebApp {
    public static void main(String[] args) {
        Vertx vertx = Vertx.vertx();
        Router router = Router.router(vertx);
        HttpServer server = vertx.createHttpServer();


        new IndexHandler(router);
        new ArtistListController(router);
        new AlbumListController(router);
        new ArtistTracksListController(router);
        new AlbumTracksListController(router);

        server.requestHandler(router).listen(3000);
    }

    private static Handler<RoutingContext> start() {
        return routingContext -> {
            HttpServerResponse response = routingContext.response();
            response.putHeader("content-type", "text/html");
            response.end("ah ah ah Start");
        };
    }
}
