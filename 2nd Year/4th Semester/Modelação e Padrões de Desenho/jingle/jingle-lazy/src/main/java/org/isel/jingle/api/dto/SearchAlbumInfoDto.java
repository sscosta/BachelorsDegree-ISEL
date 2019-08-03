package org.isel.jingle.api.dto;

public class SearchAlbumInfoDto {
    final AlbumDto album;

    public SearchAlbumInfoDto(AlbumDto album) {
        this.album = album;
    }

    public AlbumDto getAlbum() {
        return album;
    }
}
