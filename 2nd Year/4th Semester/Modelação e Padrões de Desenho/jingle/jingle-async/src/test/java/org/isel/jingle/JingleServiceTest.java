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

import io.reactivex.Observable;
import org.asynchttpclient.BoundRequestBuilder;
import org.isel.jingle.model.Album;
import org.isel.jingle.model.Artist;
import org.isel.jingle.model.Track;
import org.isel.jingle.model.TrackRank;
import org.isel.jingle.util.BaseRequestAsync;
import org.isel.jingle.util.HttpRequestAsync;
import org.junit.Assert;
import org.junit.Test;

import java.util.List;
import java.util.function.Function;

import static org.junit.Assert.assertEquals;


public class JingleServiceTest {
    static class HttpGet implements Function<String, BoundRequestBuilder> {
        int count = 0;
        @Override
        public BoundRequestBuilder apply(String path) {
            System.out.println("Requesting..." + ++count);
            return HttpRequestAsync.openStreamAsync(path);
        }
    }

    @Test
    public void searchHiperAndCountAllResults() {
        HttpGet httpGet = new HttpGet();
        JingleService service = new JingleService(new LastfmWebApi(new BaseRequestAsync(httpGet)));
        Observable<Artist> artists = service.searchArtist("hiper").cache();
        assertEquals(0, httpGet.count);
        assertEquals(711, (long)artists.count().blockingGet());
        assertEquals(25, httpGet.count);
        Artist last = artists.reduce((first, second) -> second).blockingGet();
        assertEquals("Coma - Hipertrofia.(2008)", last.getName());
        assertEquals(25, httpGet.count);
    }

    @Test
    public void getFirstAlbumOfMuse() {
        HttpGet httpGet = new HttpGet();
        JingleService service = new JingleService(new LastfmWebApi(new BaseRequestAsync(httpGet)));
        Observable<Artist> artists = service.searchArtist("muse");
        assertEquals(0, httpGet.count);
        Artist muse = artists.firstElement().blockingGet();
        assertEquals(1, httpGet.count);
        Observable<Album> albums = muse.getAlbums();
        assertEquals(1, httpGet.count);
        Album first = albums.firstElement().blockingGet();
        assertEquals(3, httpGet.count);
        assertEquals("Black Holes and Revelations", first.getName());
    }

    @Test
    public void get111AlbumsOfMuse() {
        HttpGet httpGet = new HttpGet();
        JingleService service = new JingleService(new LastfmWebApi(new BaseRequestAsync(httpGet)));
        Artist muse = service.searchArtist("muse").firstElement().blockingGet();
        Observable<Album> albums = muse.getAlbums().take(111);
        assertEquals(111, (long)albums.count().blockingGet());
        assertEquals(115, httpGet.count); // 1 for artist + 3 pages of albums each with 50 albums
    }

    @Test
    public void getSecondSongFromBlackHolesAlbumOfMuse() {
        HttpGet httpGet = new HttpGet();
        JingleService service = new JingleService(new LastfmWebApi(new BaseRequestAsync(httpGet)));
        Album blackHoles = service.searchArtist("muse").firstElement().blockingGet().getAlbums().firstElement().blockingGet();
        assertEquals(3, httpGet.count); // 1 for artist + 1 page of albums
        assertEquals("Black Holes and Revelations", blackHoles.getName());
        Track song = blackHoles.getTracks().skip(1).firstElement().blockingGet();
        assertEquals(3, httpGet.count); // + 1 to getTracks
        assertEquals("Starlight", song.getName());
    }
    @Test
    public void get42thTrackOfMuse() {
        HttpGet httpGet = new HttpGet();
        JingleService service = new JingleService(new LastfmWebApi(new BaseRequestAsync(httpGet)));
        Observable<Track> tracks = service.searchArtist("muse").blockingFirst().getTracks();
        assertEquals(1, httpGet.count);
        Track track = tracks.skip(42).blockingFirst();// + 1 to getAlbums + 4 to get tracks of first 4 albums.
        assertEquals("MK Ultra", track.getName());
        assertEquals(10, httpGet.count);
    }

    @Test   //rebenta
    public void getLastTrackOfMuseOf500() {
        HttpGet httpGet = new HttpGet();
        JingleService service = new JingleService(new LastfmWebApi(new BaseRequestAsync(httpGet)));
        Observable<Track> tracks = service.searchArtist("muse").blockingFirst().getTracks().take(50);
        assertEquals(50, (long)tracks.count().blockingGet());
        assertEquals(12, httpGet.count); // Each page has 50 albums => 50 requests to get their tracks. Some albums have no tracks.
    }

    @Test
    public void get100TopTracksFromPortugal(){
        HttpGet httpGet = new HttpGet();
        JingleService service = new JingleService(new LastfmWebApi(new BaseRequestAsync(httpGet)));
        Observable<TrackRank> topTracks = service.getTopTracks("Portugal").take(100);
        List<TrackRank> topList = topTracks.toList().blockingGet();
        assertEquals(100, topList.size());
        assertEquals(2,httpGet.count);
    }

    @Test   //rebenta
    public void getTop100FromPortugal(){
        HttpGet httpGet = new HttpGet();
        JingleService service = new JingleService(new LastfmWebApi(new BaseRequestAsync(httpGet)));
        Artist artist = service.searchArtist("Stick Figure").firstElement().blockingGet();
        Observable<TrackRank> tracksRank = service.getTracksRank(artist.getMbid(), "Portugal");
        List<TrackRank> collect = tracksRank.toList().blockingGet();
        assertEquals(43,collect.size()); //43 tracks
    }
}
