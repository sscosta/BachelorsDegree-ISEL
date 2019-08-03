package org.isel.jingle.api.dto;

public class TopTracksTrackDto {

    private final TrackRankDto[] track;

    public TopTracksTrackDto(TrackRankDto[] track) {
        this.track = track;
    }

    public TrackRankDto[] getTrack() {
        return track;
    }
}
