import javax.swing.*;
import java.awt.event.*;

/**
 * This class creates a frame with a PrologPanel and displays it.
 * @see PrologPanel
 */
public class PrologFrame extends PrologPanel implements Runnable {

    public PrologFrame() {
	super();
    }

    public void run() {
	final JFrame frame = new JFrame("Demo");
	frame.getContentPane().add(this);
        frame.addWindowListener(new WindowAdapter() {
            public void windowClosing(WindowEvent e) {
		frame.dispose();
            }
        });
        frame.pack();
        frame.setSize(400, 400);
        frame.setVisible(true);
    }
}
