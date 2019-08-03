package org.isel.jingle.dto;

public class TopTracksDto {
    private final TrackRankDto[] track;

    public TopTracksDto(TrackRankDto[] track) {
        this.track = track;
    }

    public TrackRankDto[] getTrack() {
        return track;
    }
}
