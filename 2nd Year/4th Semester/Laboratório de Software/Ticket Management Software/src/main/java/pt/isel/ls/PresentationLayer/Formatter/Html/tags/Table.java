package pt.isel.ls.PresentationLayer.Formatter.Html.tags;

import pt.isel.ls.PresentationLayer.Formatter.Html.HtmlElement;
import pt.isel.ls.PresentationLayer.Formatter.Html.IHtmlElement;

import java.util.ArrayList;
import java.util.List;

public class Table extends HtmlElement implements IHtmlElement {
    private static final String STRING_TEMPLATE = "\t\t<table border=%s>\n%s%s%s\t\t</table>\n";

    private List<TableRow> _rows;
    private int border = 1;

    public void addRow(TableRow row) {
        if (_rows == null) _rows = new ArrayList<>();

        _rows.add(row);
    }

    private String getRows(){
        if (_rows == null) return "";

        StringBuilder sb = new StringBuilder();
        sb.append(" ");
        for (TableRow r : _rows)
            sb.append(r.toString());
        return sb.toString();
    }
    private String getBorder(){
        return String.valueOf(border);
    }
    public void setBorder(int border){
        this.border = border;
    }

    @Override
    public String toString() {
            stringCache = String.format(STRING_TEMPLATE,getBorder(),
                    getId(),
                    getText(), getRows());
        return stringCache;
    }
}
