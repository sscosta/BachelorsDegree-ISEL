package pt.isel.ls.Controller;

public class MethodPath {
    public final String method;
    public final String path;

    public MethodPath(String method, String path) {
        this.method = method;
        this.path = path;
    }

    @Override
    public String toString() {
        return "" +
                "" + method +
                " " + path
                ;
    }
}
