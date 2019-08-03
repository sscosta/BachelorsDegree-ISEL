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

package org.isel.jingle.api;

import com.google.gson.Gson;
import org.isel.jingle.api.dto.*;
import org.isel.jingle.api.query.SearchAlbum;
import org.isel.jingle.api.query.SearchAlbumInfo;
import org.isel.jingle.api.query.Query;
import org.isel.jingle.api.query.SearchArtist;
import org.isel.jingle.util.req.Request;


public class LastfmWebApi {
    private final Request request;
    protected final Gson gson;

    public LastfmWebApi(Request request) {
        this(request, new Gson());
    }

    private LastfmWebApi(Request request, Gson gson) {
        this.request = request;
        this.gson = gson;
    }

    public ArtistDto[] searchArtist(String name, int page) {
        Query searchArtist = new SearchArtist(request,gson,name,page);
        return (ArtistDto[]) searchArtist.executeQuery();
    }

    public AlbumDto[] getAlbums(String artistMbid, int page) {
        Query getAlbum = new SearchAlbum(request,gson,artistMbid,page);
        return (AlbumDto[]) getAlbum.executeQuery();
    }

    public TrackDto[] getAlbumInfo(String albumMbid) {
        Query getAlbumInfo = new SearchAlbumInfo(request,gson,albumMbid);
        return (TrackDto[]) getAlbumInfo.executeQuery();
    }

}
