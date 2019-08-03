package org.isel.jingle.dto;

public class ResultArtistDto {
    private final ArtistMatchDto artistmatches;

    public ResultArtistDto(ArtistMatchDto artistmatches) {
        this.artistmatches = artistmatches;
    }

    public ArtistMatchDto getArtistMatchDto() {
        return artistmatches;
    }
}
