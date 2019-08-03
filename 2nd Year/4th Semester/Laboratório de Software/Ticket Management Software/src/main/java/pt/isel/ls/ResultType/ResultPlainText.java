package pt.isel.ls.ResultType;

public class ResultPlainText implements Result {


    private Object result;

    public ResultPlainText(Object o) {
        this.result = o;
    }

    public Object getValue() {
        return result;
    }
}
