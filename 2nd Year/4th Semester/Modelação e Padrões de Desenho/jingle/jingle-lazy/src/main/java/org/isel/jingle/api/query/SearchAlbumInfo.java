package org.isel.jingle.api.query;

import com.google.gson.Gson;
import org.isel.jingle.api.dto.AlbumDto;
import org.isel.jingle.api.dto.SearchAlbumInfoDto;
import org.isel.jingle.api.dto.TrackDto;
import org.isel.jingle.util.req.Request;

public class SearchAlbumInfo extends BaseSearch {
    private static final String LASTFM_GET_ALBUM_INFO = LASTFM_HOST
            + "?method=album.getinfo&format=json&mbid=%s&api_key="
            + LASTFM_API_KEY;
    private final String albumMbid;

    public SearchAlbumInfo(Request request, Gson gson, String albumMbid) {
        super(request,gson);
        this.albumMbid = albumMbid;
    }

    @Override
    String getURL() {
        return String.format(LASTFM_GET_ALBUM_INFO, albumMbid);
    }

    @Override
    Object fromJson() {
        AlbumDto adto = gson.fromJson(body,SearchAlbumInfoDto.class).getAlbum();
        if(adto == null)
            return new TrackDto[0];
        return adto.getTracks().getTrack();
    }
}
