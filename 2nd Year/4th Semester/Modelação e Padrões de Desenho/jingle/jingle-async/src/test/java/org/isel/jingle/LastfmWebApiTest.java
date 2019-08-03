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

import org.isel.jingle.dto.AlbumDto;
import org.isel.jingle.dto.ArtistDto;
import org.isel.jingle.dto.TrackDto;
import org.isel.jingle.dto.TrackRankDto;
import org.isel.jingle.util.*;
import org.junit.Test;

import java.util.concurrent.CompletableFuture;

import static org.junit.Assert.assertEquals;


public class LastfmWebApiTest {
    @Test
    public void searchForArtistsNamedDavid(){
        AsyncRequest req = new BaseRequestAsync(HttpRequestAsync::openStreamAsync);
        LastfmWebApi api = new LastfmWebApi(req);
        CompletableFuture<ArtistDto[]> cf = api.searchArtist("david", 1);
        ArtistDto[] join = cf.join();
        String name = join[0].getName();
        assertEquals("David Bowie", name);
    }

    @Test
    public void getTopAlbumsFromMuse(){
        AsyncRequest req = new BaseRequestAsync(HttpRequestAsync::openStreamAsync);
        LastfmWebApi api = new LastfmWebApi(req);
        CompletableFuture<ArtistDto[]> artist = api.searchArtist("muse", 1);
        String mbid = artist.join()[0].getMbid();
        CompletableFuture<AlbumDto[]> albums = api.getAlbums(mbid, 1);
        assertEquals("Black Holes and Revelations", albums.join()[0].getName());
    }
    @Test
    public void getStarlightFromBlackHolesAlbumOfMuse(){
        AsyncRequest req = new BaseRequestAsync(HttpRequestAsync::openStreamAsync);
        LastfmWebApi api = new LastfmWebApi(req);
        CompletableFuture<ArtistDto[]> artists = api.searchArtist("muse", 1);
        String mbid = artists.join()[0].getMbid();
        AlbumDto album = api.getAlbums(mbid, 1).join()[0];
        TrackDto track = api.getAlbumInfo(album.getMbid()).join()[1];
        assertEquals("Starlight", track.getName());
    }

    @Test
    public void getTopTracksFromPortugal(){
        AsyncRequest req = new BaseRequestAsync(HttpRequestAsync::openStreamAsync);
        LastfmWebApi api = new LastfmWebApi(req);
        TrackRankDto[] topTracks = api.getTopTracks("Portugal", 1).join();
        assertEquals(50, topTracks.length);
    }
}
