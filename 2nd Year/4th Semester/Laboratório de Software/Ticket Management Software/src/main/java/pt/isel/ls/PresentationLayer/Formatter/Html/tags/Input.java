package pt.isel.ls.PresentationLayer.Formatter.Html.tags;


import pt.isel.ls.PresentationLayer.Formatter.Html.HtmlElement;
import pt.isel.ls.PresentationLayer.Formatter.Html.IHtmlElement;

public class Input extends HtmlElement implements IHtmlElement {

    private static final String STRING_TEMPLATE = "<input%s%s%s%s%s>";

    private String _type;
    private String _placeholder;
    private String _name;
    private String _required;

    public Input(){}

    public Input(String text){
        setText(text);
    }

    public void setType(String type) {
        this._type = type;
    }
    private String getType() {
        return _type == null ? "" : String.format(" type=\"%s\"", _type);
    }

    public void setPlaceHolder(String placeHolder) {
        this._placeholder = placeHolder;
    }
    private String getPlaceHolder() {
        return _placeholder == null ? "" : String.format(" placeholder=\"%s\"", _placeholder);
    }

    public void setRequired(){
        this._required = "required";
    }
    private String  getRequired(){
        return _required == null ? "" : String.format(" required=\"%s\"", _required);
    }

    public void setName(String name) {
        this._name = name;
    }
    private String getName() {
        return _name == null ? "" : String.format(" name=\"%s\"", _name);
    }

    @Override
    public String toString() {
        return String.format(STRING_TEMPLATE, getId(), getType(),getRequired(), getPlaceHolder(), getName());
    }
}
