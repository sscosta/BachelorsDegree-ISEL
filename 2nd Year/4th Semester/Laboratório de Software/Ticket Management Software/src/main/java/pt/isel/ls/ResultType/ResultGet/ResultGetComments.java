package pt.isel.ls.ResultType.ResultGet;

import pt.isel.ls.Model.Comment;
import pt.isel.ls.ResultType.CustomWrapper;
import pt.isel.ls.ResultType.Result;

import java.util.LinkedList;
import java.util.List;

public class ResultGetComments implements Result {

    private CustomWrapper wrapper;

    public ResultGetComments(Comment comment, LinkedList<Comment> commentList) {
        wrapper = new CustomWrapper(comment,commentList);
    }

    @Override
    public Object getValue() {
        return wrapper;
    }

}
