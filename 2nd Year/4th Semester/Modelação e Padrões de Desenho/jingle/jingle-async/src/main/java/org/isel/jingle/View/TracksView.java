package org.isel.jingle.View;

import htmlflow.HtmlView;
import htmlflow.StaticHtml;
import io.reactivex.Observable;
import io.reactivex.Observer;
import io.reactivex.disposables.Disposable;
import io.vertx.core.http.HttpServerResponse;
import org.isel.jingle.model.Album;
import org.isel.jingle.model.Track;
import org.xmlet.htmlapifaster.*;

import java.io.IOException;
import java.io.OutputStream;
import java.io.PrintStream;

public class TracksView implements View<Observable<Track>>{
    @Override
    public void write(HttpServerResponse resp, Observable<Track> model) {
        resp.setChunked(true);
        resp.putHeader("Content-Type", "text/html");
        model.subscribeWith(new Observer<Track>(){
            Tbody<Table<Body<Html<HtmlView>>>> tbody;
            @Override
            public void onSubscribe(Disposable d) {
                tbody = writeHeader(resp);
            }
    
            @Override
            public void onNext(Track track) {
                writeTableRow(tbody,track);
            }
    
            @Override
            public void onError(Throwable e) {
                //TODO
            }
    
            @Override
            public void onComplete() {
                tbody
                        .__()//tbody
                        .__()//table
                        .__()//body
                        .__();//html
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
                            .text("Tracks")
                        .__()//title
                    .__()//head
                    .body()
                        .h1()
                            .text("Track List")
                        .__()//h1
                    .table()
                        .thead()
                            .tr()
                                .th().text("Name").__()
                                .th().text("URL").__()
                                .th().text("Duration").__()
                            .__()
                        .__()
                        .tbody();
    }
    
    private static void writeTableRow(Tbody<Table<Body<Html<HtmlView>>>> tbody, Track track) {
        tbody
                .tr()
                    .td().style().text("tr:nth-child(even) {background-color: #f2f2f2;}").__()
                        .text(track.getName())
                    .__()
                    .td()
                        .text(track.getUrl())
                    .__()
                    .td()
                        .text(track.getDuration())
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
