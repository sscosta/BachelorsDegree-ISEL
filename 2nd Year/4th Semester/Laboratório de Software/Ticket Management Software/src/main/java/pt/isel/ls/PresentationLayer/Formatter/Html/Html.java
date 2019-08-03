package pt.isel.ls.PresentationLayer.Formatter.Html;

import pt.isel.ls.PresentationLayer.Formatter.Html.tags.Body;
import pt.isel.ls.PresentationLayer.Formatter.Html.tags.Head;

public class Html {
    private static final String STRING_TEMPLATE = "<html>\n%s%s</html>";

    private Head _head = new Head("");
    private Body _body = new Body();

    public void setHead(Head head) {
        _head = head;
    }

    public void setBody(Body body) {
        _body = body;
    }

    public Body getBody() {
        return _body;
    }

    public Head getHead() { return _head; }

    @Override
    public String toString() {
        return String.format(STRING_TEMPLATE, _head.toString(), _body.toString());
    }
}
