package org.isel.jingle.Controller;

import io.reactivex.Observable;
import io.vertx.ext.web.Router;
import io.vertx.ext.web.RoutingContext;
import org.isel.jingle.JingleService;
import org.isel.jingle.View.TracksView;
import org.isel.jingle.model.Track;

public class AlbumTracksListController {
    private final TracksView view = new TracksView();
    private final Router router;
    public AlbumTracksListController(Router router) {
        this.router = router;
        router.route("/albums/:id/tracks").handler(this::trackListHandle);
    }

    private void trackListHandle(RoutingContext ctx) {
        JingleService jingleService = new JingleService();
        String id = ctx.request().getParam("id");
        Observable<Track> tracks = jingleService.getAlbumTracks(id).take(100);
        view.write(ctx.response(), tracks);
        ctx.response().end();
    }


}
