package org.isel.jingle.api.dto;

public class ArtistSearchResultsArtistMatchesDto {
    final ArtistDto [] artist;

    public ArtistSearchResultsArtistMatchesDto(ArtistDto[] artist) {
        this.artist = artist;
    }

    public ArtistDto[] getArtist() {
        return artist;
    }
}
