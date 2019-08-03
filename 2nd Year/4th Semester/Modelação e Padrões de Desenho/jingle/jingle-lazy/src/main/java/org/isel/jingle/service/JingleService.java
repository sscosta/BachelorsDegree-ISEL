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

package org.isel.jingle.service;

import org.isel.jingle.api.LastfmWebApi;
import org.isel.jingle.api.dto.AlbumDto;
import org.isel.jingle.api.dto.ArtistDto;
import org.isel.jingle.api.dto.TrackDto;
import org.isel.jingle.service.model.Album;
import org.isel.jingle.service.model.Artist;
import org.isel.jingle.service.model.Track;
import org.isel.jingle.util.queries.LazyQueries;

import java.util.Objects;

import static org.isel.jingle.util.queries.LazyQueries.*;

public class JingleService {

    private final LastfmWebApi api;

    public JingleService(LastfmWebApi api) {
        this.api = api;
    }
    /* TODO - isolate code in common in searchArtist and getAlbums
    * in order to have an object that calls api.searchArtist or api.getAlbums we have to create a dependency between
    * that object and LastfmWebApi, which is not desirable
    */
    public Iterable<Artist> searchArtist(String name) {
        Iterable<Integer> pageNr = iterate(1, n -> n+1);
        final Iterable<ArtistDto[]> map = takeWhile(map(pageNr, nr -> api.searchArtist(name,nr)), this::isEmptyArray);
        return map(flatMap(map, LazyQueries::from) ,this::toArtist);
    }

    private Iterable<Album> getAlbums(String artistMbid) {
        Iterable<Integer> pageNr = iterate(1, n -> n+1);
        final Iterable<AlbumDto[]> map = takeWhile( map(pageNr, nr -> api.getAlbums(artistMbid,nr)), this::isEmptyArray);
        return map(flatMap(map, LazyQueries::from) , this::toAlbum);
    }

    private <T> boolean isEmptyArray(T[] arr) {
        return arr.length != 0;
    }

    private Iterable<Track> getAlbumTracks(String albumMbid) {
        return map(from(api.getAlbumInfo(albumMbid)), this::toTrack);
    }

    private Iterable<Track> getTracks(String artistMbid) {
        Iterable<String> id = map(getAlbums(artistMbid), Album::getMbid);
        id = filter(id, Objects::nonNull);
        Iterable<Iterable<Track>> tracks = map(id, this::getAlbumTracks);
        return flatMap(tracks, it -> it);
    }

    private Artist toArtist(ArtistDto artistDto) {
        Iterable<Album> as = () -> getAlbums(artistDto.getMbid()).iterator();
        Iterable<Track> ts = () -> getTracks(artistDto.getMbid()).iterator();
        return new Artist(
                artistDto.getName(),
                artistDto.getListeners(),
                artistDto.getMbid(),
                artistDto.getUrl(),
                artistDto.getImage()[0].getImgURL(),
                as,
                ts
        );
    }

    private Album toAlbum(AlbumDto albumDto) {
        Iterable<Track> ats = () -> getAlbumTracks(albumDto.getMbid()).iterator();
        return new Album(
                albumDto.getName(),
                albumDto.getPlaycount(),
                albumDto.getMbid(),
                albumDto.getUrl(),
                albumDto.getImage()[0].getImgURL(),
                ats
        );
    }

    private Track toTrack(TrackDto tracksDto) {
        return new Track(tracksDto.getName(),tracksDto.getUrl(),tracksDto.getDuration());
    }
}
