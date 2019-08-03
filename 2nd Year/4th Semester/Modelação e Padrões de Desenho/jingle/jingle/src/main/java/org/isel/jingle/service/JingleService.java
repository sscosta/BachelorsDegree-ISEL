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
import org.isel.jingle.api.dto.TrackRankDto;
import org.isel.jingle.misc.StreamUtils;
import org.isel.jingle.service.model.Album;
import org.isel.jingle.service.model.Artist;
import org.isel.jingle.service.model.Track;
import org.isel.jingle.service.model.TrackRank;

import java.util.Arrays;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.function.*;
import java.util.stream.*;

import java.util.Objects;

public class JingleService {

    private final LastfmWebApi api;

    public JingleService(LastfmWebApi api) {
        this.api = api;
    }

    public Stream<Artist> searchArtist(String name) {
        return search((nr)-> api.searchArtist(name,nr),this::toArtist);
    }

    private Stream<Album> getAlbums(String artistMbid) {
        return search((nr)-> api.getAlbums(artistMbid,nr),this::toAlbum);
    }

    private <T,R> Stream<R> search(Function<Integer,T[]> apiCall, Function<T,R> mapper){
        Stream<Integer> pageNr = Stream.iterate(1, n -> n + 1);
        final Stream<T[]> map = pageNr.map(apiCall::apply).takeWhile(this::isEmptyArray);
        return map.flatMap(Arrays::stream).map(mapper::apply);
    }

    private Stream<Track> getAlbumTracks(String albumMbid) {
        return Stream.of(api.getAlbumInfo(albumMbid)).map(this::toTrack);
    }

    private Stream<Track> getTracks(String artistMbid){
        final Stream<String> id = StreamSupport.stream(getAlbums(artistMbid).spliterator(),false).map(Album::getMbid);
        final Stream<String> validIds = id.filter(Objects::nonNull);
        return validIds.map(this::getAlbumTracks).flatMap(it -> it);
    }

    public Stream<TrackRank> getTopTracks(String country){
        AtomicInteger i = new AtomicInteger(1);
        return search((nr)-> api.getTopTracks(country,nr), this::toTrackRank).map(tr -> absoluteRank(tr, i.getAndIncrement()));
    }

    private Stream<TrackRank> getTracksRank(String artistMbId, String country){
        BiPredicate<Track,TrackRank> criteria = this::ComparingTracks;
        BiFunction<Track, TrackRank, TrackRank> transformation = (t, tr) -> new TrackRank(t.getName(), t.getUrl(), t.getDuration(), tr.getRank());
        return StreamUtils.merge(getTracks(artistMbId), getTopTracks(country).limit(100), criteria, transformation, new TrackRank("","",0,0));
    }

    private Artist toArtist(ArtistDto artistDto) {
        Supplier<Stream<Album>> as = () -> getAlbums(artistDto.getMbid());
        Supplier<Stream<Track>> ts = () -> getTracks(artistDto.getMbid());
        Function<String,Stream<TrackRank>> rs = (c) -> getTracksRank(artistDto.getMbid(),c);
        return new Artist(
                artistDto.getName(),
                artistDto.getListeners(),
                artistDto.getMbid(),
                artistDto.getUrl(),
                artistDto.getImage()[0].getImgURL(),
                as,
                ts,
                rs
        );
    }

    private Album toAlbum(AlbumDto albumDto) {
        Supplier<Stream<Track>> ats = () -> getAlbumTracks(albumDto.getMbid());
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

    private  TrackRank toTrackRank(TrackRankDto trDto) {
        return new TrackRank(trDto.getName(), trDto.getUrl(), trDto.getDuration(), trDto.getRank());
    }

    private <T> boolean isEmptyArray(T[] arr) {
        return arr.length != 0;
    }
    private boolean ComparingTracks(Track t, TrackRank tr) {
        return t.getName().equals(tr.getName()) && t.getDuration() == tr.getDuration() && t.getUrl().equals(tr.getUrl());
    }
    private TrackRank absoluteRank(TrackRank trackRank, int absoluteRank) {
        trackRank.setRank(absoluteRank);
        return trackRank;
    }
}
