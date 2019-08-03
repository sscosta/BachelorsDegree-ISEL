package pt.isel.ls.PresentationLayer.Formatter.Html.tags;

import pt.isel.ls.PresentationLayer.Formatter.Html.HtmlElement;
import pt.isel.ls.PresentationLayer.Formatter.Html.IHtmlElement;

public class Label extends HtmlElement implements IHtmlElement {

    private static final String STRING_TEMPLATE = "<label%s%s>%s%s</label>";

    private String _for;

    public Label(){}

    public Label(String text){
        setText(text);
    }

    public void setFor(String _for) {
        this._for = _for;
    }
    private String getFor() {
        return _for == null ? "" : String.format(" for=\"%s\"",_for);
    }

    @Override
    public String toString() {
        return String.format(STRING_TEMPLATE, getId(), getFor(), getText(), getElementsString());
    }
}
