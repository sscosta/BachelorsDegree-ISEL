package pt.isel.ls.PresentationLayer.Formatter.Html.tags;


import pt.isel.ls.PresentationLayer.Formatter.Html.HtmlElement;
import pt.isel.ls.PresentationLayer.Formatter.Html.IHtmlElement;

public class Button extends HtmlElement implements IHtmlElement {

    private static final String STRING_TEMPLATE = "<button%s%s%s>%s%s</button>";

    private String _type = "button";

    public Button(){}

    public Button(String text){
        setText(text);
    }

    public void setType(String type) {
        this._type = type;
            }
    private String getType() {
        return String.format(" type=\"%s\"", _type);
    }

    @Override
    public String toString() {
        return String.format(STRING_TEMPLATE, getId(), getHtmlClassString(),
                getType(), getText(), getElementsString());
    }
}
