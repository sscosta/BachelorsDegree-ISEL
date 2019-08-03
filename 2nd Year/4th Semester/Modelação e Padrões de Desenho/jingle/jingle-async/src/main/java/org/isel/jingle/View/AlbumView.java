package org.isel.jingle.View;

import htmlflow.HtmlView;
import htmlflow.StaticHtml;
import io.reactivex.Observable;
import io.reactivex.Observer;
import io.reactivex.disposables.Disposable;
import io.vertx.core.http.HttpServerResponse;
import org.isel.jingle.model.Album;
import org.xmlet.htmlapifaster.Body;
import org.xmlet.htmlapifaster.Html;
import org.xmlet.htmlapifaster.Table;
import org.xmlet.htmlapifaster.Tbody;

import java.io.IOException;
import java.io.OutputStream;
import java.io.PrintStream;
import java.util.List;

public class AlbumView implements View<Observable<Album>>{
    @Override
    public void write(HttpServerResponse resp, Observable<Album> model) {
        resp.setChunked(true);
        //List<Album> a = model.take(10).toList().blockingGet();
        resp.putHeader("Content-Type", "text/html");
        model.subscribeWith(new Observer<Album>(){
            Tbody<Table<Body<Html<HtmlView>>>> tbody;
            @Override
            public void onSubscribe(Disposable d) {
                tbody = writeHeader(resp);
            }
    
            @Override
            public void onNext(Album album) {
                writeTableRow(tbody,album);
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
                            .text("Album")
                        .__()//title
                    .__()//head
                    .body()
                        .h1()
                            .text("Album List")
                        .__()//h1
                    .table()
                        .thead()
                            .tr()
                                .th().text("Name").__()
                                .th().text("PlayCount").__()
                                .th().text("Mbid").__()
                                .th().text("URL").__()
                                .th().text("Image").__()
                                .th().text("Tracks").__()
                            .__()
                        .__()
                        .tbody();
    }
    
    private static void writeTableRow(Tbody<Table<Body<Html<HtmlView>>>> tbody, Album album) {
        tbody
                .tr()
                    .td().style().text("tr:nth-child(even) {background-color: #f2f2f2;}").__()
                        .text(album.getName())
                    .__()
                    .td()
                        .text(album.getPlaycount())
                    .__()
                    .td()
                        .text(album.getMbid()==null ? "" : album.getMbid())
                    .__()
                    .td()
                        .text(album.getUrl())
                    .__()
                    .td()
                        .img().attrSrc(album.getImage()).__()
                    .__()
                    .td()
                        .a()
                        .attrHref("/albums/"+album.getMbid()+"/tracks")
                        .text(album.getMbid()==null ? "" : "Tracks")
                        .__()
                .__();
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
