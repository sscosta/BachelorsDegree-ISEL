package org.isel.jingle.dto;

public class SearchDto {
    private ResultArtistDto results;
    private ResultAlbumsDto topalbums;

    public SearchDto(ResultArtistDto results) {
        this.results = results;
    }

    public SearchDto(ResultAlbumsDto topalbuns) {
        this.topalbums = topalbuns;
    }

    public ResultArtistDto getResults() {
        return results;
    }

    public ResultAlbumsDto getTopalbums() {
        return topalbums;
    }
}
