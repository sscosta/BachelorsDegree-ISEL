package org.isel.jingle.Controller;

import io.vertx.core.http.HttpServerResponse;
import io.vertx.ext.web.Router;
import io.vertx.ext.web.RoutingContext;
import org.isel.jingle.View.IndexView;

public class IndexHandler {
    private final IndexView view = new IndexView();

    public IndexHandler(Router router) {
        router.route("/").handler(this::index);
    }

    private void index(RoutingContext ctx) {
        HttpServerResponse response = ctx.response();

        view.write(response);
    }
}
