import javax.swing.*;
import javax.swing.text.*;
import java.awt.*;
import java.awt.event.*;
import se.sics.jasper.*;

"NOTE: This class has not been updated to work with SICStus 3.9"

/**
 * A panel with a combobox at the top and a textpane below. The
 * combobox is used to enter Prolog queries, and the textpane
 * displayes the results of the queries.
 */
public class PrologPanel extends JPanel {
    SICStus sp;
//    SPPredicate callstring_pred = null;
    JComboBox comboBox = new JComboBox();
    JTextPane textPane = new JTextPane();
    JScrollPane scrollPane =
	new JScrollPane(textPane,
			JScrollPane.VERTICAL_SCROLLBAR_ALWAYS,
			JScrollPane.HORIZONTAL_SCROLLBAR_NEVER);

    private void setupStyles(JTextPane textPane) {
	Style def = StyleContext.getDefaultStyleContext().
	    getStyle(StyleContext.DEFAULT_STYLE);
	Style regular = textPane.addStyle("regular", def);
	StyleConstants.setFontFamily(def, "SansSerif");
	Style bold = textPane.addStyle("bold", regular);
	StyleConstants.setBold(bold, true);
    }

    private void appendText(JTextPane textPane, String style, String text) {
	Document doc = textPane.getDocument();
	try {
	    doc.insertString(doc.getLength(), text, textPane.getStyle(style));
	} catch (BadLocationException ble) {
	    System.err.println("Couldn't insert text.");
	}
    }

    /**
     * Send a query to Prolog and append the answer at the end of the
     * textpane.
     * @param goal A prolog goal (including a full stop at the end).
     */
    public void doQuery(String goal) {
	SPTerm A, X;
	appendText(textPane, "regular", "?- " + goal + "\n");
	try {
	    A = new SPTerm(sp).putListChars(goal);
	    X = new SPTerm(sp).putVariable();
	    sp.query("user", "callstring", new SPTerm[] { A, X });
	} catch (Exception excp) {
	    excp.printStackTrace();
	    appendText(textPane, "bold", excp.toString() + "\n");
	    return;
	}
	appendText(textPane, "bold", X.toString() + "\n");
    }

    public PrologPanel() {
	super(false);
	try {
	    if (null == (sp = SICStus.getInitializedSICStus())) {
		sp = new SICStus();
	    }
	    sp.load("callstring.pl");
	} catch (Exception excp) {
	    excp.printStackTrace();
	}
	textPane.setEditable(false);
	setupStyles(textPane);
	GridBagLayout gridBag = new GridBagLayout();
	setLayout(gridBag);
	GridBagConstraints c = new GridBagConstraints();
	c.gridwidth = GridBagConstraints.REMAINDER;
	c.fill = GridBagConstraints.HORIZONTAL;
	gridBag.setConstraints(comboBox, c);
	comboBox.addActionListener(new ActionListener() {
		public void actionPerformed(ActionEvent event) {
		    Object item = comboBox.getSelectedItem();
		    comboBox.insertItemAt(item, 0);
		    comboBox.getEditor().selectAll();
		    doQuery((String)item);
		}
	    });
	comboBox.setEditable(true);
	add(comboBox);
	c.fill = GridBagConstraints.BOTH;
	c.weightx = 1.0;
	c.weighty = 1.0;
	gridBag.setConstraints(scrollPane, c);
	add(scrollPane);
    }
} // PrologPanel
