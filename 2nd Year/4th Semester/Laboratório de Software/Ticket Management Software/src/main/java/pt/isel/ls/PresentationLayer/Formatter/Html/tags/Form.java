package pt.isel.ls.PresentationLayer.Formatter.Html.tags;

import pt.isel.ls.PresentationLayer.Formatter.Html.HtmlElement;
import pt.isel.ls.PresentationLayer.Formatter.Html.IHtmlElement;

public class Form extends HtmlElement implements IHtmlElement {

    private static final String STRING_TEMPLATE = "<form%s%s%s%s%s%s%s%s%s%s>%s%s</form>";

    // Specific attributes
    private String _acceptCharset;
    private String _action;
    private String _autocomplete;
    private String _enctype;
    private String _method;
    private String _name;
    private String _novalidate;
    private String _target;
    private String _onSubmit;

    public Form(){}

    public Form(String text){
        setText(text);
    }

    @Override
    public String toString() {
            return String.format(STRING_TEMPLATE, getId(),
                getAcceptCharset(), getAction(), getAutoComplete(), getEnctype(), getMethod(), getName(), getNoValidate(),
                getTarget(), getOnSubmit(),
                getText(), getElementsString());


    }

    public void setAcceptCharset(String _acceptCharset) {
        this._acceptCharset = _acceptCharset;
    }
    private String getAcceptCharset() {
        return _acceptCharset == null ? "" : String.format(" acceptCharset=\"%s\"", _acceptCharset);
    }

    public void setAction(String _action) {
        this._action = _action;
    }
    private String getAction() {
        return _action == null ? "" : String.format(" action=\"%s\"", _action);
    }

    public void setAutoComplete(boolean state) {
        this._autocomplete = state ? "on" : "off";
    }
    private String getAutoComplete() {
        return _autocomplete == null ? "" : String.format(" autocomplete=\"%s\"", _autocomplete) ;
    }

    public void setEnctype(String _enctype) {
        this._enctype = _enctype;
    }
    private String getEnctype() {
        return _enctype == null ?  "" : String.format(" enctype=\"%s\"", _enctype) ;
    }

    public void setMethod(String _method) {
        this._method = _method;
    }
    private String getMethod() {
        return _method == null ? "" : String.format(" method=\"%s\"", _method);
    }

    public void setName(String name) {
        this._name = name;
    }
    private String getName() {
        return _name == null ? "" : String.format(" name=\"%s\"",_name);
    }

    public void setNoValidate() {
        this._novalidate = " novalidate";
    }
    private String getNoValidate() {
        return _novalidate == null ? "" : _novalidate;
    }

    public void setTarget(String target) {
        this._target = target;
    }
    private String getTarget() {
        return _target == null ? "" : String.format(" target=\"%s\"", _target);
    }

    public void setOnSubmit(String onSubmit) {
        this._onSubmit = onSubmit;
    }
    private String getOnSubmit() {
        return _onSubmit == null ? "" : String.format(" onsubmit=\"%s\"", _onSubmit);
    }

}


/*

accept-charset — Character encodings to use for form submission
action — URL to use for form submission
autocomplete — Default setting for autofill feature for controls in the form
enctype — Form data set encoding type to use for form submission
method — Variant to use for form submission
name — Name of form to use in the document.forms API
novalidate — Bypass form control validation for form submission
target — Browsing context for form submission

 */