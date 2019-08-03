package org.isel.jingle.api.query;

import com.google.gson.Gson;
import org.isel.jingle.api.dto.ArtistSearchDto;
import org.isel.jingle.api.dto.ArtistSearchResultsDto;
import org.isel.jingle.service.model.Artist;
import org.isel.jingle.util.req.Request;

public class SearchArtist extends BaseSearch {

    private static final String LASTFM_SEARCH = LASTFM_HOST
            + "?method=artist.search&format=json&artist=%s&page=%d&api_key="
            + LASTFM_API_KEY;
    private final String artistName;
    private final int page;


    public SearchArtist(Request request, Gson gson, String name, int page) {
        super(request,gson);
        this.artistName = name;
        this.page = page;
    }

    @Override
    String getURL() {
        return String.format(LASTFM_SEARCH,artistName,page);
    }

    @Override
    Object fromJson() {
        ArtistSearchResultsDto asrDto = gson.fromJson(body,ArtistSearchDto.class).getResults();
        if(asrDto == null)
            return new Artist[0];
        return asrDto.getArtistMatches().getArtist();
    }
}
