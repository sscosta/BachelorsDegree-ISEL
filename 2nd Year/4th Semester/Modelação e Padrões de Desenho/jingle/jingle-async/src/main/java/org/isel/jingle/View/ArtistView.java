package org.isel.jingle.View;

import htmlflow.HtmlView;
import htmlflow.StaticHtml;
import io.reactivex.Observable;
import io.reactivex.Observer;
import io.reactivex.disposables.Disposable;
import io.vertx.core.http.HttpServerResponse;
import org.apache.commons.lang3.StringUtils;
import org.isel.jingle.model.Artist;
import org.xmlet.htmlapifaster.*;

import java.io.IOException;
import java.io.OutputStream;
import java.io.PrintStream;

public class ArtistView implements View<Observable<Artist>> {

    @Override
    public void write(HttpServerResponse resp, Observable<Artist> model) {
        resp.setChunked(true);
        resp.putHeader("Content-Type", "text/html");
        model.subscribeWith(new Observer<Artist>() {
            Tbody<Table<Body<Html<HtmlView>>>> tbody;
            public void onSubscribe(Disposable d) {
                tbody = writeHeader(resp);
            }

            public void onNext(Artist artist) {
                writeTableRow(tbody, artist);
            }

            public void onError(Throwable e) { /* TO DO !!! */ }

            @Override
            public void onComplete() {
                tbody
                        .__() // tbody
                        .__() // table
                        .__() // body
                        .__();// html
                resp.end();
            }
        });
    }

    @Override
    public void write(HttpServerResponse resp) {
        throw new UnsupportedOperationException("This view requires a Model. You should invoke write(resp, model) instead!");
    }

    private static Tbody<Table<Body<Html<HtmlView>>>> writeHeader(HttpServerResponse resp) {
        return StaticHtml.view(new ResponsePrintStream(resp))
                .html()
                .head()
                .title()
                .text("Artist")
                .__()// title
                .__()// head
                .body()
                .h1()
                .text("Artists List")
                .__()// h1
                .table()
                .thead()
                .tr()
                .th().text("Name").__()
                .th().text("Picture").__()
                .th().text("URL").__()
                .th().text("Abums").__()
                .th().text("Tracks").__()
                .__()
                .__()
                .tbody();
    }

    private static void writeTableRow(Tbody<Table<Body<Html<HtmlView>>>> tbody, Artist artist) {
        tbody
                .tr()
                .td()
                    .style().text("tr:nth-child(even) {background-color: #f2f2f2;}").__()
                    .text(artist.getName())
                .__()
                .td()
                    .img().attrSrc(artist.getImage()).__()
                .__()
                .td()
                    .a()
                        .attrHref(artist.getUrl())
                        .text("Visit Artist")
                .td()
                    .a()
                        .attrHref("/artists/"+artist.getMbid()+"/albums")
                        .text(StringUtils.isEmpty(artist.getMbid()) ? "" : "Albums")
                .td()
                    .a()
                        .attrHref("/artists/"+artist.getMbid()+"/tracks")
                        .text(StringUtils.isEmpty(artist.getMbid())? "" : "Tracks")
                .__()
        ;
    }

    private static class ResponsePrintStream extends PrintStream {
        /**
         * We may improve this with a Buffer.
         * For now we just want to see the effect of sending
         * char by char to the browser !!!
         */
        public ResponsePrintStream(HttpServerResponse resp) {
            super(new OutputStream() {
                @Override
                public void write(int b) throws IOException {
                    char c = (char) b;
                    resp.write(String.valueOf(c));
                }
            });
        }
    }

}
