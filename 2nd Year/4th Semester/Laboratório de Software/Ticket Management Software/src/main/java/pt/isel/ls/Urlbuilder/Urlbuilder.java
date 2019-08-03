package pt.isel.ls.Urlbuilder;

public class Urlbuilder {
    public static String postLabel(int pid) {
        return "/projects/"+ pid +"/labels";
    }

    public static String postProject() {
        return "/projects";
    }

    public static String postIssue(int pid) {
        return "/projects/"+pid+"/issues";
    }

    public static String postComment(int pid, int iid) {
        return "/projects/" + pid + "/issues/" + iid + "/comments";
    }
}
