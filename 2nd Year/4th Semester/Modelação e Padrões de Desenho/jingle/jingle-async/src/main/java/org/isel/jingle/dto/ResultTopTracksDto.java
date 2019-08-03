package org.isel.jingle.dto;

public class ResultTopTracksDto {
    private final TopTracksDto tracks;

    public ResultTopTracksDto(TopTracksDto tracks) {
        this.tracks = tracks;
    }

    public TopTracksDto getTracks() {
        return tracks;
    }
}

