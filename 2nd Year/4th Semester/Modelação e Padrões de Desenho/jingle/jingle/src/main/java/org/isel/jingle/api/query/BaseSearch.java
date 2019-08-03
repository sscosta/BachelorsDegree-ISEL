package org.isel.jingle.api.query;

import com.google.gson.Gson;
import org.isel.jingle.req.Request;

import java.util.stream.Collectors;

public abstract class BaseSearch implements Query{
    protected static final String LASTFM_API_KEY = "0c4b6ffe1b7ba90d165be12be1d04b96";
    protected static final String LASTFM_HOST = "http://ws.audioscrobbler.com/2.0/";
    private final Request request;
    protected final Gson gson;
    protected String body;

    BaseSearch(Request request, Gson gson){
        this.request = request;
        this.gson = gson;
    }


    public Object executeQuery(){
        body = getBodyFromURL();
        return fromJson();
    }

    private String getBodyFromURL() {
        String url = getURL();
        return request.getLines(url).collect(Collectors.joining());
    }
    abstract String getURL();
    abstract Object fromJson();
}
