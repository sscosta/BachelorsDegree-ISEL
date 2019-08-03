package org.isel.jingle.req;

import java.io.IOException;
import java.io.InputStream;
import java.net.URL;

public class HttpRequest{

    public static InputStream openStream(String path) {
        try {
            return new URL(path).openStream();
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }
}