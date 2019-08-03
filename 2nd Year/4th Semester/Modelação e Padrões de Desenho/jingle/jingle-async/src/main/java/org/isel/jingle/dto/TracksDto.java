package org.isel.jingle.dto;

public class TracksDto {
    private final TrackDto[] track;

    public TracksDto(TrackDto[] track) {
        this.track = track;
    }

    public TrackDto[] getTrack() {
        return track;
    }
}

