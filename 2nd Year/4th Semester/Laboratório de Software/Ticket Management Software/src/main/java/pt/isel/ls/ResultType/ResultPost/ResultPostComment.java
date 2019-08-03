package pt.isel.ls.ResultType.ResultPost;

import pt.isel.ls.Model.Comment;
import pt.isel.ls.ResultType.CustomWrapper;
import pt.isel.ls.ResultType.Result;

import java.util.LinkedList;

public class ResultPostComment implements Result {

    private CustomWrapper wrapper;

    public ResultPostComment(Comment comment, LinkedList<Comment>commentList) {
        wrapper = new CustomWrapper(comment, commentList);
    }

    @Override
    public Object getValue() {
        return wrapper;
    }

}
