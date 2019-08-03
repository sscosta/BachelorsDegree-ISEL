package org.isel.jingle.dto;

public class ResultAlbumsDto {
    private final AlbumDto[] album;

    public ResultAlbumsDto(AlbumDto[] album) {
        this.album = album;
    }

    public AlbumDto[] getAlbum() {
        return album;
    }
}
