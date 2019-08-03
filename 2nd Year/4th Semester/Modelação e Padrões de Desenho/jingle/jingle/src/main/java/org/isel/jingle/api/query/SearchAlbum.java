package org.isel.jingle.api.query;

import com.google.gson.Gson;
import org.isel.jingle.api.dto.AlbumDto;
import org.isel.jingle.api.dto.AlbumSearchDto;
import org.isel.jingle.api.dto.TopAlbumsDto;
import org.isel.jingle.req.Request;

public class SearchAlbum extends BaseSearch {
    private static final String LASTFM_GET_ALBUMS = LASTFM_HOST
            + "?method=artist.gettopalbums&format=json&mbid=%s&page=%d&api_key="
            + LASTFM_API_KEY;
    private final String artistMbid;
    private final int page;

    public SearchAlbum(Request request, Gson gson, String artistMbid, int page) {
        super(request,gson);
        this.artistMbid = artistMbid;
        this.page = page;
    }

    @Override
    String getURL() {
        return String.format(LASTFM_GET_ALBUMS, artistMbid, page);
    }

    @Override
    Object fromJson() {
        TopAlbumsDto taDto = gson.fromJson(body,AlbumSearchDto.class).getTopalbums();
        if(taDto == null)
            return new AlbumDto[0];
        return taDto.getAlbums();
    }
}
