package org.isel.jingle.api.dto;

import com.google.gson.annotations.SerializedName;

public class ArtistSearchResultsDto {

    @SerializedName("opensearch:totalResults")
    final long totalResults;
    @SerializedName("opensearch:itemsPerPage")
    final int itemsPerpage;
    final ArtistSearchResultsArtistMatchesDto artistmatches;

    public ArtistSearchResultsDto(long totalResults, int itemsPerpage, ArtistSearchResultsArtistMatchesDto artistmatches) {
        this.totalResults = totalResults;
        this.itemsPerpage = itemsPerpage;
        this.artistmatches = artistmatches;
    }

    public long getTotalResults() {
        return totalResults;
    }

    public int getItemsPerpage() {
        return itemsPerpage;
    }

    public ArtistSearchResultsArtistMatchesDto getArtistMatches() {
        return artistmatches;
    }
}
