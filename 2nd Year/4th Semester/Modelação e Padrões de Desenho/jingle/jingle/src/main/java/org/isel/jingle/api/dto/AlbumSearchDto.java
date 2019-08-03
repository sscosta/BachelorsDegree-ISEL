package org.isel.jingle.api.dto;

public class AlbumSearchDto {
    final TopAlbumsDto topalbums;

    public AlbumSearchDto(TopAlbumsDto topalbums) {
        this.topalbums = topalbums;
    }

    public TopAlbumsDto getTopalbums() {
        return topalbums;
    }
}
