import javax.swing.*;
import javax.swing.filechooser.FileFilter;
import java.awt.*;
import java.awt.event.*;
import java.io.*;
import java.util.HashMap;
import se.sics.jasper.*;

"NOTE: This class has not been updated to work with SICStus 3.9"

/**
 * The Demo class is a demonstration of how to use jasper.
 * Jasper is used in three ways:
 * <li>
 *   From the 'File' menu a prolog file can be loaded.
 * <li>
 *   From the 'Edit' menu a preference dialog can be selected, and the
 *   button pressed will be asserted in the prolog database.
 * <li>
 *   One of the tabs in the tab panel selects a panel which is a kind
 *   of Prolog toplevel.
 */
// ******************************************************************
// There should be code to demonstrate how to use the new method
//  'public void delete' of the class SPTerm. That method invalidates
// the SPTerm and makes the Prolog side term-ref available for reuse.
// Not yet done, though.
// ******************************************************************

public class Demo implements ShortcutReceiver {

    JTabbedPane tabbedPane;
    PrologPanel prologPanel;

    private class ExitButton extends JButton implements ActionListener {
	public void actionPerformed(ActionEvent event) {
	    System.exit(0);	    
	}

	public ExitButton(String label) {
	    super(label);
	    this.setToolTipText("Get outta here!");
	    this.addActionListener(this);
	}
    } // ExitButton


    private class PrologFileFilter extends FileFilter {
    
	String getExtension(File f) {
	    String ext = null;
	    String s = f.getName();
	    int i = s.lastIndexOf('.');

	    if (i > 0 &&  i < s.length() - 1) {
		ext = s.substring(i+1).toLowerCase();
	    }
	    return ext;
	}

	// Accept all directories and all pl, po, or ql files.
	public boolean accept(File f) {
	    if (f.isDirectory()) { return true; }

	    String extension = getExtension(f);
	    if (extension != null) {
		if (extension.equals("pl") ||
		    extension.equals("po") ||
		    extension.equals("ql")) {
                    return true;
		} else {
		    return false;
		}
	    }
	    return false;
	}
    
	// The description of this filter
	public String getDescription() {
	    return "Prolog files (*.pl, *.po, *.ql)";
	}
    }

    private class FileLoader extends JFileChooser implements ActionListener {

	public void actionPerformed(ActionEvent e) {
	    SICStus sp;
	    int returnVal = showDialog(getParent(), "Load");
	    if (returnVal == JFileChooser.APPROVE_OPTION) {
		File file = getSelectedFile();
		try {
		    if (null == (sp = SICStus.getInitializedSICStus())) {
			sp = new SICStus();
		    }
		    sp.load(file.toString());
		} catch (Exception excp) {
		    excp.printStackTrace();
		    return;
		}
//  	    } else {
//  		System.out.println("Load command cancelled by user.");
	    }
	}

	public FileLoader() {
	    super();
	    setApproveButtonMnemonic('L');
	}
    }

    private abstract class DialogAction implements ActionListener {
	JFrame owner;

	public DialogAction(JFrame frame) {
	    owner = frame;
	}
    } // DialogAction

    private class HelpDialog extends DialogAction {
	public void actionPerformed(ActionEvent e) {
	    JOptionPane.showMessageDialog(owner, "HELP!");
	}

	public HelpDialog(JFrame frame) {super(frame);}
    } // HelpDialog

    /**
     * The class PreferencesDialog demonstrates how a set of option
     * buttons can interact with Jasper.
     */
    private class PreferencesDialog extends DialogAction {
	SICStus sp;

	public void actionPerformed(ActionEvent e) {
	    Object[] options = {"Tea, please.",
				"Coffe, please.",
				"Nothing, thank you.",
				"Cancel"};
	    String[] option_strings = {"Tea", "Coffe"};
	    int n = JOptionPane.showOptionDialog(
			owner,
			"Do you prefer coffe or tea?",
			"Preferences",
			JOptionPane.DEFAULT_OPTION,
			JOptionPane.QUESTION_MESSAGE,
			null,
			options,
			options[3]);
	    if (n >= 0 && n <= 2) {
		try {
		    if (null == (sp = SICStus.getInitializedSICStus())) {
			sp = new SICStus();
		    }
		    sp.queryCutFail("retractall(preference(_)).", null);
		    if (n < 2) {
			HashMap varMap = new HashMap();
			varMap.put("Pref", new SPTerm(sp, option_strings[n]));
			sp.queryCutFail("assert(preference(Pref)).", varMap);
		    }
		    
		} catch (Exception excp) {
		    excp.printStackTrace();
		    return;
		}
	    }
	}

	public PreferencesDialog(JFrame frame) {super(frame);}
    } // PreferencesDialog

    /**
     * This method is called from an object of the class ShortcutMenu.
     * It will be called from the ShortcutMenu object when the user
     * selects a shortcut from the menu.
     */
    public void doShortcut(String dummy, String goal) {
	prologPanel.doQuery(goal);
	tabbedPane.setSelectedComponent(prologPanel); // expose the prolog panel
    }

    /**
     * This method is called from an object of the class ShortcutMenu
     * when the user selects the 'Add' menuitem from the menu.
     */
    public String[] getDefaultShortcut() {
	String def = (String)(prologPanel.comboBox.getSelectedItem());
	return new String[] {def, def};
    }

    private JComponent makeTextPanel(String text) {
        JPanel panel = new JPanel(false);
        JLabel filler = new JLabel(text);
        filler.setHorizontalAlignment(JLabel.CENTER);
        panel.setLayout(new GridLayout(1, 1));
        panel.add(filler);
        return panel;
    }

    private JMenu makeMenu(String name, JMenuItem[] menuItems) {
	JMenu menu = new JMenu(name);
	for (int i = 0; i < menuItems.length; ++i) {
	    menu.add(menuItems[i]);
	}
	return menu;
    }

    private JMenuItem makeMenuItem(String name, ActionListener al) {
	JMenuItem menuItem = new JMenuItem(name);
	menuItem.addActionListener(al);
	return menuItem;
    }

    public Demo (JFrame frame) {
	JPanel tabpanel = new JPanel();
	JPanel menupanel = new JPanel();
	tabbedPane = new JTabbedPane();

	ExitButton exitbutton = new ExitButton("Exit");

	JMenuBar menubar = new JMenuBar();
	// Create and set up the File menu
	FileLoader fl = new FileLoader();
	fl.addChoosableFileFilter(new PrologFileFilter());
	JMenuItem file_load = makeMenuItem("Load", fl);
	JMenuItem file_new_prolog =
	    makeMenuItem("New Prolog frame",
			 new ActionListener() {
				 public void actionPerformed(ActionEvent e) {
				     PrologFrame pf = new PrologFrame();
				     new Thread(pf).start();
				 }
			     } 
			 );
  	JMenuItem file_exit = makeMenuItem("Exit", exitbutton);
	JMenu filemenu = makeMenu("File", new JMenuItem[] {file_load,
							   file_new_prolog,
							   file_exit});
	menubar.add(filemenu);
	// Create and set up the Edit menu
	JMenuItem edit_prefs = makeMenuItem("Preferences",
					    new PreferencesDialog(frame));

	JMenu editmenu = makeMenu("Edit", new JMenuItem[] {edit_prefs});
	menubar.add(editmenu);
	// Create and set up the Shortcut menu
	ShortcutMenu shortcut_menu = new ShortcutMenu("Goals", "Name:","Goal:");
	shortcut_menu.addReceiver(this);
	menubar.add(shortcut_menu);
	// Create and set up the Help menu
	JMenuItem help_help = makeMenuItem("Help", new HelpDialog(frame));
	JMenu helpmenu = makeMenu("Help", new JMenuItem[] {help_help});
	menubar.add(Box.createHorizontalGlue());
	menubar.add(helpmenu);
	menupanel.setLayout(new GridLayout(1,1));
	menupanel.add(menubar);

	ImageIcon icon = null;

	JComponent panel1 = makeTextPanel("Demo of Swing/Jasper interaction.");
	tabbedPane.addTab("Splash panel", icon, panel1, "Does nothing");
	tabbedPane.setSelectedIndex(0);

	prologPanel = new PrologPanel();
	tabbedPane.addTab("Prolog top level", icon, prologPanel,
			  "Prolog top level");
	tabpanel.setLayout(new GridLayout(1,1));
	tabpanel.add(tabbedPane);

        frame.getContentPane().add(menupanel, BorderLayout.NORTH);
	frame.getContentPane().add(tabpanel, BorderLayout.CENTER);
//	frame.getContentPane().add(exitbutton, BorderLayout.SOUTH);
    } // Demo
	

    public static void main(String[] args) {
        try {
            UIManager.setLookAndFeel(
		  UIManager.getCrossPlatformLookAndFeelClassName());
//		  UIManager.getSystemLookAndFeelClassName());
        } catch (Exception e) {
	    System.out.println("Can't set look and feel: " + e);
	}

        JFrame frame = new JFrame("Demo");
	Demo demo = new Demo(frame);

        frame.addWindowListener(new WindowAdapter() {
            public void windowClosing(WindowEvent e) {
                System.exit(0);
            }
        });
        frame.pack();
        frame.setSize(400, 600);
        frame.setVisible(true);
    }
}
