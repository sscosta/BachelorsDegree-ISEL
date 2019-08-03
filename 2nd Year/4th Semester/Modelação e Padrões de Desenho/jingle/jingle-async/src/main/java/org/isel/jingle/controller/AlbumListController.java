package org.isel.jingle.Controller;

import io.reactivex.Observable;
import io.vertx.ext.web.Router;
import io.vertx.ext.web.RoutingContext;
import org.isel.jingle.JingleService;
import org.isel.jingle.View.AlbumView;
import org.isel.jingle.model.Album;

public class AlbumListController implements AutoCloseable {
    
    final AlbumView view = new AlbumView();
    private Router router;

    public AlbumListController(Router router) {
        this.router = router;
        router.route("/artists/:id/albums").handler(this::albumListHandler);
    }


    private void albumListHandler(RoutingContext ctx) {
        JingleService jingleService = new JingleService();
        String mbId = ctx.request().getParam("id");
        Observable<Album> albums = jingleService.getAlbums(mbId).take(100);
        view.write(ctx.response(),albums);
        ctx.response().end();

    }

    @Override
    public void close() throws Exception {

    }
}
