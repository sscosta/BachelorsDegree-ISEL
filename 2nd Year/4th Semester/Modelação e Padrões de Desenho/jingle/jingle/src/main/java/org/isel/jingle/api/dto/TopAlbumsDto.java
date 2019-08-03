package org.isel.jingle.api.dto;

public class TopAlbumsDto {
    final AlbumDto [] album;

    public TopAlbumsDto(AlbumDto[] album) {
        this.album = album;
    }

    public AlbumDto[] getAlbums() {
        return album;
    }
}
