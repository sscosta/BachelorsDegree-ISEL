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

import org.isel.jingle.api.LastfmWebApi;
import org.isel.jingle.misc.StreamUtils;
import org.isel.jingle.req.BaseRequest;
import org.isel.jingle.req.HttpRequest;
import org.isel.jingle.service.JingleService;
import org.isel.jingle.service.model.Album;
import org.isel.jingle.service.model.Artist;
import org.isel.jingle.service.model.Track;
import org.isel.jingle.service.model.TrackRank;
import org.junit.Test;

import java.io.InputStream;
import java.util.function.Function;
import java.util.function.Supplier;
import java.util.stream.Stream;

import static junit.framework.Assert.assertEquals;
import static junit.framework.Assert.assertTrue;
import static org.isel.jingle.misc.StreamUtils.cache;
import static org.isel.jingle.misc.StreamUtils.distinctByKey;

public class JingleServiceTest {
    static class HttpGet implements Function<String, InputStream> {
        int count = 0;
        @Override
        public InputStream apply(String path) {
            System.out.println("Requesting..." + ++count);
            return HttpRequest.openStream(path);
        }
    }

    @Test
    public void searchHiperAndCountAllResults() {
        HttpGet httpGet = new HttpGet();
        JingleService service = new JingleService(new LastfmWebApi(new BaseRequest(httpGet)));
        Supplier<Stream<Artist>> artists = () -> service.searchArtist("hiper");
        assertEquals(0, httpGet.count);
        assertTrue(artists.get().count() > 0);
        assertEquals(25, httpGet.count);
        Artist last = artists.get().reduce((p, c) -> c).get();//gets last element
        assertEquals("Coma - Hipertrofia.(2008)", last.getName());
        assertEquals(50, httpGet.count);
    }

    @Test
    public void searchHiperAndCountAllResultsWithCaching() {
        HttpGet httpGet = new HttpGet();
        JingleService service = new JingleService(new LastfmWebApi(new BaseRequest(httpGet)));
        Stream<Artist> artists =  service.searchArtist("hiper");
        Supplier<Stream<Artist>> artistsSup = cache(artists);
        assertEquals(0, httpGet.count);
        assertTrue(artistsSup.get().count() > 0);
        assertEquals(25, httpGet.count);
        Artist last = artistsSup.get().reduce((p, c) -> c).get();//gets last element
        assertEquals("Coma - Hipertrofia.(2008)", last.getName());
        assertEquals(25, httpGet.count);
    }

    @Test
    public void getFirstAlbumOfMuse() {
        HttpGet httpGet = new HttpGet();
        JingleService service = new JingleService(new LastfmWebApi(new BaseRequest(httpGet)));
        Stream<Artist> artists = service.searchArtist("muse");
        assertEquals(0, httpGet.count);
        Artist muse = artists.findFirst().get();
        assertEquals(1, httpGet.count);
        Stream<Album> albums = muse.getAlbums();
        assertEquals(1, httpGet.count);
        Album first = albums.findFirst().get();
        assertEquals(2, httpGet.count);
        assertEquals("Black Holes and Revelations", first.getName());
    }

    @Test
    public void get111AlbumsOfMuse() {
        HttpGet httpGet = new HttpGet();
        JingleService service = new JingleService(new LastfmWebApi(new BaseRequest(httpGet)));
        Artist muse = service.searchArtist("muse").findFirst().get();
        Stream<Album> albums = muse.getAlbums().limit(111);
        assertEquals(111, albums.count());
        assertEquals(4, httpGet.count); // 1 for artist + 3 pages of albums each with 50 albums
    }

    @Test
    public void getSecondSongFromBlackHolesAlbumOfMuse() {
        HttpGet httpGet = new HttpGet();
        JingleService service = new JingleService(new LastfmWebApi(new BaseRequest(httpGet)));
        Album blackHoles = service.searchArtist("muse").findFirst().get().getAlbums().findFirst().get();
        assertEquals(2, httpGet.count); // 1 for artist + 1 page of albums
        assertEquals("Black Holes and Revelations", blackHoles.getName());
        Track song = blackHoles.getTracks().skip(1).findFirst().get();
        assertEquals(3, httpGet.count); // + 1 to getTracks
        assertEquals("Starlight", song.getName());
    }

    @Test
    public void get42thTrackOfMuse() {
        HttpGet httpGet = new HttpGet();
        JingleService service = new JingleService(new LastfmWebApi(new BaseRequest(httpGet)));
        Stream<Track> tracks = service.searchArtist("muse").findFirst().get().getTracks();
        assertEquals(1, httpGet.count); // 1 for artist + 0 for tracks because it fetches lazily
        Track track = tracks.skip(42).findFirst().get();
        assertEquals("MK Ultra", track.getName());
        assertEquals(6, httpGet.count);
    }
    @Test
    public void getLastTrackOfMuseOf500() {
        HttpGet httpGet = new HttpGet();
        JingleService service = new JingleService(new LastfmWebApi(new BaseRequest(httpGet)));
        Stream<Track> tracks = service.searchArtist("muse").findFirst().get().getTracks().limit(500);
        assertEquals(1,httpGet.count);
        assertEquals(500, tracks.count());
        assertTrue(httpGet.count > 75);
    }

    @Test
    public void get500TopTracksFromPortugal(){
        HttpGet httpGet = new HttpGet();
        JingleService service = new JingleService(new LastfmWebApi(new BaseRequest(httpGet)));
        Supplier<Stream <TrackRank>> tracks = cache(service.getTopTracks("portugal").limit(500));
        assertEquals(0,httpGet.count);
        assertEquals(500, tracks.get().count());
        assertTrue(httpGet.count >= 10);
        TrackRank last = tracks.get().reduce((a, b) -> b).get();
        assertEquals(49, last.getRank());
    }
    @Test
    public void getTracksRank(){
        HttpGet httpGet = new HttpGet();
        JingleService service = new JingleService(new LastfmWebApi(new BaseRequest(httpGet)));
        Artist tameImpala = service.searchArtist("Tame Impala").findFirst().get();
        Supplier<Stream<TrackRank>> tracks = cache(tameImpala.getTracksRank("portugal"));
        assertEquals(1,httpGet.count);
        assertEquals(82, tracks.get().count());
        assertEquals(220, httpGet.count);
        assertEquals(60, tracks.get().filter(distinctByKey(TrackRank::getName)).count());
        assertEquals("Feels Like We Only Go Backwards", tracks.get().reduce(StreamUtils::highestRank).get().getName());
    }
}
