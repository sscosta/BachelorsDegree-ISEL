package pt.isel.ls.PresentationLayer.Formatter.Html.tags;

import pt.isel.ls.PresentationLayer.Formatter.Html.HtmlElement;
import pt.isel.ls.PresentationLayer.Formatter.Html.IHtmlElement;

public class Option extends HtmlElement implements IHtmlElement {
    private static final String STRING_TEMPLATE = "<option%s>%s</option>";

    private String _value;

    public Option(){}

    public Option(String text, String value) {
        setText(text);
        this._value = value;
    }

    public void setValue(String value) {
        _value = value;
    }
    private String getValue() {
        return _value == null ? "" : String.format(" value=\"%s\"", _value);
    }

    @Override
    public String toString() {
        return String.format(STRING_TEMPLATE,  getValue(),
                getText());
    }
}
