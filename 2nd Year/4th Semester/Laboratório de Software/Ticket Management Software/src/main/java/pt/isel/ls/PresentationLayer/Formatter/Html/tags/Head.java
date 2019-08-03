package pt.isel.ls.PresentationLayer.Formatter.Html.tags;


import pt.isel.ls.PresentationLayer.Formatter.Html.HtmlElement;

public class Head extends HtmlElement {
    private static final String STRING_HEAD_TEMPLATE = "\t<head>%s%s</head>\n";
    private static final String STRING_TITLE_TEMPLATE = "<title>%s</title>";

    private String _title;

    public Head (){}
    public Head(String title) {
        _title = title;
    }

    public void setTitle(String title) {
        _title = title;
    }

    private String getTitle(){
        if(_title == null)
            return "LS G1";
        return _title;
    }

    @Override
    public String toString() {
        String title = String.format(STRING_TITLE_TEMPLATE, getTitle() );

        return String.format(STRING_HEAD_TEMPLATE, title, getElementsString());
    }
}
