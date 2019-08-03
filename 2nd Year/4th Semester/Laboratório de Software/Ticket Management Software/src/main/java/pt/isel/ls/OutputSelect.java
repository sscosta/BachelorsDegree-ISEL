package pt.isel.ls;

import pt.isel.ls.Controller.RequestContext;

import java.io.BufferedWriter;
import java.io.FileWriter;
import java.io.IOException;

public class OutputSelect {
    final String FILENAME_KEY = "file-name";
    String response;
    RequestContext options;

    public OutputSelect(String response, RequestContext options) {
        this.response = response;
        this.options = options;
    }

    void writeResponse() throws IOException {
        if(options.getParamMap().containsKey(FILENAME_KEY))
            writeToFile();
        else
            System.out.println(response);
    }

    private void writeToFile() throws IOException {
        BufferedWriter writer = new BufferedWriter(
                new FileWriter(
                        (String) options.getParamMap().get(FILENAME_KEY)
                )
        );
        writer.write(response);

        writer.close();
    }
}
