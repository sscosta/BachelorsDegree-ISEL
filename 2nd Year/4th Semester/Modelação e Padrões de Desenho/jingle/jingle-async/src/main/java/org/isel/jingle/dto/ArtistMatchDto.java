package org.isel.jingle.dto;

public class ArtistMatchDto {
    private final ArtistDto[] artist;

    public ArtistMatchDto(ArtistDto[] artist) {
        this.artist = artist;
    }

    public ArtistDto[] getArtist() {
        return artist;
    }
}
