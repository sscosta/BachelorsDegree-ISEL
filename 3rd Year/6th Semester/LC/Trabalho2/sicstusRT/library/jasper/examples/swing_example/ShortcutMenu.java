import javax.swing.*;
import java.awt.event.*;
// import java.util.*;

/**
 * The ShortcutMenu class handles user defined shortcuts selectable
 * from a menu.
 * An object of the class ShortcutMenu can be used whenever an object
 * of the class JMenu can be used.
 * Initially the menu is empty, except for an 'Add' menu item.
 * Selecting 'Add' will cause a popup window to appear where the user
 * can specify a shortcut.
 */
public class ShortcutMenu extends JMenu {
    JMenuItem add_shortcut;
    ShortcutReceiver shortcutReceiver = null;
    String namePrompt, dataPrompt;

    private class ShortcutMenuItem extends JMenuItem implements ActionListener {
	String shortcutName, shortcutData;

	public void actionPerformed(ActionEvent event) {
	    if (shortcutReceiver != null) {
		shortcutReceiver.doShortcut(shortcutName, shortcutData);
	    }
	}

	public ShortcutMenuItem(String name, String data) {
	    super(name);
	    shortcutName = name;
	    shortcutData = data;
	}
    }

    private class AddListener implements ActionListener {
	public void actionPerformed(ActionEvent event) {
	    String[] defaultData = shortcutReceiver.getDefaultShortcut();
	    JTextField nameField = new JTextField(defaultData[0]);
// ?***? selectAll() on nameField and dataField doesn't seem to select anything
	    nameField.selectAll();
	    JTextField dataField = new JTextField(defaultData[1]);
	    dataField.selectAll();
	    Object[] message = {namePrompt, nameField, dataPrompt, dataField};
	    String[] options = {"Save", "Cancel"};
	    // popup dialog to enter the shortcut data and the shortcut name
	    int n = JOptionPane.showOptionDialog(getParent(),
						 message,
						 "Shortcut",
						 JOptionPane.DEFAULT_OPTION,
						 JOptionPane.PLAIN_MESSAGE,
						 null,
						 options,
						 options[1]);
	    if (n == JOptionPane.YES_OPTION) {
		String nameString = nameField.getText();
		String dataString = dataField.getText();
		addShortcut(nameString, dataString);
	    }
	}
    }

    /**
     * Adds a shortcut to the menu.
     * @param name The menuitem string.
     * @param data An arbitrary string.
     */
    public void addShortcut(String name, String data) {
	ShortcutMenuItem item = new ShortcutMenuItem(name, data);
	insert(item, 0);
	item.addActionListener(item);
    }

    /**
     * Registers a ShortcutReceiver with this ShortcutMenu. The
     * doShortcut method of the ShortcutReceiver will be called when
     * the user selects a shortcut from the menu. The
     * getDefaultShortcut method of the receiver will be called when a
     * new menuitem is made.
     * @param receiver An object implementing the ShortcutReceiver interface.
     * @see ShortcutReceiver
     */
    public void addReceiver(ShortcutReceiver receiver) {
	shortcutReceiver = receiver;
    }

    /**
     * @param name The text for the menu label.
     * @param namePrompt The prompt for the menu item name in the
     * popup window where the user specifies a shortcut.
     * @param dataPrompt Similary used to prompt for the data.
     */
    public ShortcutMenu(String name, String namePrompt, String dataPrompt) {
	super(name);
	this.namePrompt = namePrompt;
	this.dataPrompt = dataPrompt;
	addSeparator();
	add_shortcut = new JMenuItem("Add");
	add_shortcut.addActionListener(new AddListener());
	add(add_shortcut);
    }
   
}
