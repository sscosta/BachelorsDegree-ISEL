package org.isel.jingle.api.query;

import com.google.gson.Gson;
import org.isel.jingle.api.dto.TopTracksRankDto;
import org.isel.jingle.api.dto.TopTracksTrackDto;
import org.isel.jingle.api.dto.TrackRankDto;
import org.isel.jingle.api.dto.TracksDto;
import org.isel.jingle.req.Request;

public class SearchTopTracks extends BaseSearch {

    private final String LASTFM_GET_TOP_TRACKS = LASTFM_HOST +
            "?method=geo.gettoptracks&format=json&country=%s&page=%d&api_key=" +
            LASTFM_API_KEY;
    private final String country;
    private final int page;

    public SearchTopTracks(Request request, Gson gson, String country, int page) {
        super(request,gson);
        this.country = country;
        this.page = page;
    }

    @Override
    String getURL() {
        return String.format(LASTFM_GET_TOP_TRACKS, country, page);
    }

    @Override
    Object fromJson() {
        TopTracksTrackDto tracksDto = gson.fromJson(body, TopTracksRankDto.class).getTracks();
        if(tracksDto == null)
            return new TrackRankDto[0];
        return tracksDto.getTrack();
    }
}
