/*
 * GNU General Public License v3.0
 *
 * Copyright (c) 2019, Miguel Gamboa (gamboa.pt)
 *
 *   All rights granted under this License are granted for the term of
 * copyright on the Program, and are irrevocable provided the stated
 * conditions are met.  This License explicitly affirms your unlimited
 * permission to run the unmodified Program.  The output from running a
 * covered work is covered by this License only if the output, given its
 * content, constitutes a covered work.  This License acknowledges your
 * rights of fair use or other equivalent, as provided by copyright law.
 *
 *   You may make, run and propagate covered works that you do not
 * convey, without conditions so long as your license otherwise remains
 * in force.  You may convey covered works to others for the sole purpose
 * of having them make modifications exclusively for you, or provide you
 * with facilities for running those works, provided that you comply with
 * the terms of this License in conveying all material for which you do
 * not control copyright.  Those thus making or running the covered works
 * for you must do so exclusively on your behalf, under your direction
 * and control, on terms that prohibit them from making any copies of
 * your copyrighted material outside their relationship with you.
 *
 *   Conveying under any other circumstances is permitted solely under
 * the conditions stated below.  Sublicensing is not allowed; section 10
 * makes it unnecessary.
 *
 */

package org.isel.jingle;

import com.google.gson.Gson;
import org.isel.jingle.dto.*;
import org.isel.jingle.util.AsyncRequest;

import java.util.concurrent.CompletableFuture;

public class LastfmWebApi {
    private static final String LASTFM_API_KEY = "55394a24c02f82f0b62712b219374964";
    private static final String LASTFM_HOST = "http://ws.audioscrobbler.com/2.0/";
    private static final String LASTFM_SEARCH = LASTFM_HOST
            + "?method=artist.search&format=json&artist=%s&page=%d&api_key="
            + LASTFM_API_KEY;

    private static final String LASTFM_GET_ALBUMS = LASTFM_HOST
            + "?method=artist.gettopalbums&format=json&mbid=%s&page=%d&api_key="
            + LASTFM_API_KEY;

    private static final String LASTFM_GET_ALBUM_INFO = LASTFM_HOST
            + "?method=album.getinfo&format=json&mbid=%s&api_key="
            + LASTFM_API_KEY;

    private static final String LASTfM_GET_TOP_TRACKS = LASTFM_HOST
            + "?method=geo.gettoptracks&format=json&country=%s&page=%s&api_key="
            +LASTFM_API_KEY;

    private final Gson gson;
    private final AsyncRequest request;

    public LastfmWebApi(AsyncRequest request) {
        this.request = request;
        this.gson = new Gson();
    }

    public CompletableFuture<ArtistDto[]> searchArtist(String name, int page) {
        String path = String.format(LASTFM_SEARCH, name, page);
        CompletableFuture<String> src = request.getLines(path);
        CompletableFuture<ArtistDto[]> cfs = src.thenApply(body -> {
            SearchDto dto = gson.fromJson(body, SearchDto.class);
            return dto.getResults().getArtistMatchDto().getArtist();
        });
        return cfs;
    }

    public CompletableFuture<AlbumDto[]> getAlbums(String artistMbid, int page) {
        String path = String.format(LASTFM_GET_ALBUMS, artistMbid, page);
        CompletableFuture<String> src = request.getLines(path);
        CompletableFuture<AlbumDto[]> cfs = src.thenApply(body -> {
            SearchDto dto = gson.fromJson(body, SearchDto.class);
            return dto.getTopalbums().getAlbum();
        });
        return cfs;
    }

    public CompletableFuture<TrackDto[]> getAlbumInfo(String albumMbid){
        String path = String.format(LASTFM_GET_ALBUM_INFO, albumMbid);
        CompletableFuture<String> src = request.getLines(path);
        CompletableFuture<TrackDto[]> cfs = src.thenApply(body -> {
            ResultAlbumDto dto = gson.fromJson(body, ResultAlbumDto.class);
            return dto.getAlbum().getTracks().getTrack();
        });
        return cfs;
    }

    public CompletableFuture<TrackRankDto[]> getTopTracks(String country, int page){
        String path = String.format(LASTfM_GET_TOP_TRACKS, country, page);
        CompletableFuture<String> src = request.getLines(path);
        return src.thenApply(body -> {
            ResultTopTracksDto dto = gson.fromJson(body, ResultTopTracksDto.class);
            return dto.getTracks().getTrack();
        });
    }
}
