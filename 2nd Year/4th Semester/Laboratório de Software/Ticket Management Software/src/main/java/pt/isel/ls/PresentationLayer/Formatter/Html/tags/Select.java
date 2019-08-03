package pt.isel.ls.PresentationLayer.Formatter.Html.tags;

import pt.isel.ls.PresentationLayer.Formatter.Html.HtmlElement;

public class Select extends HtmlElement {
    private static final String STRING_TEMPLATE = "<select%s>%s%s</select>";

    private String _name;

    public Select() {
    }

    public Select(String _name) {
        this._name = _name;
    }

    public void setName(String name) {
        _name = name;
    }
    private String getName() {
        return _name == null ? "" : String.format(" name=\"%s\"", _name);
    }

    @Override
    public String toString() {
        return String.format(STRING_TEMPLATE, getName(), getText(), getElementsString());
    }

    @Override
    public void addElement(HtmlElement element) {
        super.addElement(element);
    }
}
