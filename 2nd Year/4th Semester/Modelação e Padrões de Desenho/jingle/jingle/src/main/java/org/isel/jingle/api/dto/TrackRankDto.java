package org.isel.jingle.api.dto;

import com.google.gson.annotations.SerializedName;

public class TrackRankDto {

    private final String name;
    private final String url;
    private final int duration;
    @SerializedName("@attr")
    private final RankDto attr;
    private int rank;

    public TrackRankDto(String name, String url, int duration, RankDto attr) {
        this.name = name;
        this.url = url;
        this.duration = duration;
        this.attr = attr;
    }

    public String getName() {
        return name;
    }

    public String getUrl() {
        return url;
    }

    public int getDuration() {
        return duration;
    }

    public int getRank() {
        if(rank == 0)
            rank = attr.getRank();
        return rank;
    }
}
