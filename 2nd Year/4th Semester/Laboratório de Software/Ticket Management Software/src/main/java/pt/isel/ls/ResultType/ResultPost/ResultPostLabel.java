package pt.isel.ls.ResultType.ResultPost;

import pt.isel.ls.Model.Label;
import pt.isel.ls.Model.ProjectLabel;
import pt.isel.ls.ResultType.CustomWrapper;
import pt.isel.ls.ResultType.Result;

import java.io.UnsupportedEncodingException;
import java.net.URLEncoder;
import java.sql.Wrapper;

public class ResultPostLabel implements Result {
    CustomWrapper wrapper;

    public ResultPostLabel(ProjectLabel label, int pid){
            wrapper = new CustomWrapper(label,pid);
    }

    @Override
    public Object getValue() {
        return wrapper;
    }
}
