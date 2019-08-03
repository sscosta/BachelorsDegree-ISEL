package org.isel.jingle.dto;

import com.google.gson.annotations.SerializedName;

public class TrackRankDto {
    private final String name;
    private final String url;
    private final int duration;
    @SerializedName("@attr")
    private final Attr attr;

    public TrackRankDto(String name, String url, int duration, Attr attr) {
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

    public Attr getAttr() {
        return attr;
    }
}
