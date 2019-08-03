
package org.isel.jingle;

import io.reactivex.Observable;
import org.isel.jingle.dto.AlbumDto;
import org.isel.jingle.dto.ArtistDto;
import org.isel.jingle.dto.TrackDto;
import org.isel.jingle.dto.TrackRankDto;
import org.isel.jingle.model.Album;
import org.isel.jingle.model.Artist;
import org.isel.jingle.model.Track;
import org.isel.jingle.model.TrackRank;
import org.isel.jingle.util.BaseRequestAsync;
import org.isel.jingle.util.HttpRequestAsync;

import java.util.concurrent.CompletableFuture;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.stream.Stream;

public class JingleService {

    final LastfmWebApi api;

    public JingleService(LastfmWebApi api) {
        this.api = api;
    }
    public JingleService() {
        this(new LastfmWebApi(new BaseRequestAsync(HttpRequestAsync::openStreamAsync)));
    }

    public Observable<Artist> searchArtist(String name) {
        Stream<CompletableFuture<ArtistDto[]>> cf = Stream
                .iterate(1, n -> n + 1)
                .map(nr -> api.searchArtist(name, nr));
        Observable<ArtistDto[]> dto = Observable
                .fromIterable(cf::iterator)
                .flatMap(Observable::fromFuture);
        return dto
                .takeWhile(arr -> arr.length != 0)
                .flatMap(Observable::fromArray)
                .map(this::createArtist);
    }

    public Observable<Album> getAlbums(String artistMbid) {
        Stream<CompletableFuture<AlbumDto[]>> cf = Stream
                .iterate(1, n -> n + 1)
                .map(nr -> api.getAlbums(artistMbid, nr));
        Observable<AlbumDto[]> dto = Observable
                .fromIterable(cf::iterator)
                .flatMap(Observable::fromFuture);
        return dto
                .takeWhile(arr -> arr.length != 0)
                .flatMap(Observable::fromArray)
                .map(this::createAlbums);
    }

    public Observable<Track> getAlbumTracks(String albumMbid) {
        CompletableFuture<TrackDto[]> cf = api.getAlbumInfo(albumMbid);
        Observable<TrackDto[]> dto = Observable.fromFuture(cf);
        return dto
                .takeWhile(arr -> arr.length != 0)
                .flatMap(Observable::fromArray)
                .map(this::createTrack);
    }

    public Observable<Track> getTracks(String artistMbid) {
        return getAlbums(artistMbid)
                .filter(s -> s.getMbid()!= null)
                .flatMap(s -> getAlbumTracks(s.getMbid()));
    }

    public Observable<TrackRank> getTopTracks(String country){
        Stream<CompletableFuture<TrackRankDto[]>> cf = Stream
                .iterate(1, n -> n + 1)
                .map(nr -> api.getTopTracks(country, nr));
        Observable<TrackRankDto[]> obs = Observable
                .fromIterable(cf::iterator)
                .flatMap(Observable::fromFuture);
        Observable<TrackRankDto> dto = obs
                .takeWhile(arr -> arr.length != 0)
                .flatMap(Observable::fromArray);
        return dto
                .map(e -> {
                    AtomicInteger rankNumber = new AtomicInteger(1);
                    return createTrackRank(e, rankNumber);
                });
    }

    public Observable<TrackRank> getTracksRank(String artistMbId, String country){
        Observable<Track> artistTracks = getTracks(artistMbId);
        Observable<TrackRank> countryTracks = getTopTracks(country).take(100);
        Observable<TrackRank> merge = artistTracks
            .zipWith(countryTracks, (a, c) -> {
                if (a.getName().equals(c.getName()))
                    return new TrackRank(a.getName(), a.getUrl(), a.getDuration(), c.getRank());
                else
                    return new TrackRank("", "", 0, 0);
            });
        return merge;
    }

    private TrackRank createTrackRank(TrackRankDto dto, AtomicInteger nr) {
        return new TrackRank(
                dto.getName(),
                dto.getUrl(),
                dto.getDuration(),
                nr.getAndIncrement()
        );
    }

    private Artist createArtist(ArtistDto dto) {
        return new Artist(
                dto.getName(),
                dto.getListeners(),
                dto.getMbid(),
                dto.getUrl(),
                dto.getImage()[0].getText(),
                getAlbums(dto.getMbid()),
                getTracks(dto.getMbid()),
                c -> getTracksRank(dto.getMbid(), c)
        );
    }

    private Album createAlbums(AlbumDto dto) {
        return new Album(
                dto.getName(),
                dto.getPlaycount(),
                dto.getMbid(),
                dto.getUrl(),
                dto.getImage()[0].getText(),
                getAlbumTracks(dto.getMbid())
        );
    }

    private Track createTrack(TrackDto dto) {
        return new Track(
                dto.getName(),
                dto.getUrl(),
                dto.getDuration());
    }
}
