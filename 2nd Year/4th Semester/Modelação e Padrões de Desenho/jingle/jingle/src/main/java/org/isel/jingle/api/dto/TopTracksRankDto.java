package org.isel.jingle.api.dto;

public class TopTracksRankDto {

    private final TopTracksTrackDto tracks;

    public TopTracksRankDto(TopTracksTrackDto tracks) {
        this.tracks = tracks;
    }

    public TopTracksTrackDto getTracks() {
        return tracks;
    }
}
