package org.isel.jingle.util;

import java.util.concurrent.CompletableFuture;

public interface AsyncRequest {
    CompletableFuture<String> getLines(String path);
}
