package org.isel.jingle.api.dto;

public class ArtistSearchDto {
    final ArtistSearchResultsDto results;

    public ArtistSearchDto(ArtistSearchResultsDto results) {
        this.results = results;
    }

    public ArtistSearchResultsDto getResults() {
        return results;
    }

}
