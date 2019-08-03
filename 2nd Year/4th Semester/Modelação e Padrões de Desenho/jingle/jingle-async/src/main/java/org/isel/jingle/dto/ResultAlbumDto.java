package org.isel.jingle.dto;

public class ResultAlbumDto {
    private final AlbumDto album;

    public ResultAlbumDto(AlbumDto album) {
        this.album = album;
    }

    public AlbumDto getAlbum() {
        return album;
    }
}
