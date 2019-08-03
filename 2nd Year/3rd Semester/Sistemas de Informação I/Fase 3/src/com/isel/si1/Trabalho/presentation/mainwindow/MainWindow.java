package com.isel.si1.Trabalho.presentation.mainwindow;

import java.awt.Color;
import java.awt.Cursor;
import java.awt.EventQueue;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.io.IOException;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.imageio.ImageIO;
import javax.swing.JButton;
import javax.swing.JFrame;
import javax.swing.JOptionPane;
import javax.swing.JProgressBar;
import javax.swing.JTextPane;

import com.isel.si1.Trabalho.i18n.Resources;
import com.isel.si1.Trabalho.model.Product;
import com.isel.si1.Trabalho.presentation.ProductActions.ProductActions;
import com.isel.si1.Trabalho.presentation.workers.MySimpleWorker;

public class MainWindow {

	private static Logger logger = Logger.getLogger(MainWindow.class.getName());

	private JFrame frame;
	JProgressBar progressBar;

	/**
	 * Launch the application.
	 */
	public static void show() {
		EventQueue.invokeLater(new Runnable() {
			public void run() {
				try {
					MainWindow window = new MainWindow();
					window.frame.setVisible(true);
				} catch (Exception theCause) {
					logger.log(Level.SEVERE, "an exception was thrown", theCause);
				}
			}
		});
	}

	/**
	 * Create the application.
	 */
	public MainWindow() {
		initialize();
	}
	
	/**
	 * Initialize the contents of the frame.
	 */
	private void initialize() {
		frame = new JFrame();
		frame.setTitle(Resources.getMessage("main.window.title"));
		setDBAppIcon();
		frame.setBounds(100, 100, 450, 300);
		frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
		GridBagLayout gridBagLayout = new GridBagLayout();
		gridBagLayout.columnWidths = new int[] { 0, 0, 0, 0 };
		gridBagLayout.rowHeights = new int[] { 0, 0, 0, 0, 0, 0, 0 };
		gridBagLayout.columnWeights = new double[] { 0.0, 0.0, 1.0, Double.MIN_VALUE };
		gridBagLayout.rowWeights = new double[] { 0.0, 1.0, 0.0, 1.0, 0.0, 1.0, Double.MIN_VALUE };
		frame.getContentPane().setLayout(gridBagLayout);
		final Color textPaneBackgroundColor = Color.PINK;
		frame.getContentPane().setBackground(textPaneBackgroundColor);

		JButton btnNewButton = new JButton(Resources.getMessage("button.product.list.all"));
		btnNewButton.addActionListener(e -> callListAllProducts(e));
		GridBagConstraints gbc_btnNewButton = new GridBagConstraints();
		gbc_btnNewButton.insets = new Insets(0, 0, 5, 5);
		gbc_btnNewButton.gridx = 0;
		gbc_btnNewButton.gridy = 1;
		frame.getContentPane().add(btnNewButton, gbc_btnNewButton);

		JTextPane txtpnOptionADescription = new JTextPane();
		txtpnOptionADescription.setBackground(textPaneBackgroundColor);
		txtpnOptionADescription.setEditable(false);
		txtpnOptionADescription.setText(Resources.getMessage("label.product.list.all"));
		GridBagConstraints gbc_txtpnOptionADescription = new GridBagConstraints();
		gbc_txtpnOptionADescription.insets = new Insets(0, 0, 5, 0);
		gbc_txtpnOptionADescription.fill = GridBagConstraints.CENTER;
		gbc_txtpnOptionADescription.gridx = 2;
		gbc_txtpnOptionADescription.gridy = 1;
		frame.getContentPane().add(txtpnOptionADescription, gbc_txtpnOptionADescription);

		JButton btnNewButton_1 = new JButton("Option B");
		btnNewButton_1.addActionListener(e -> callOptionB(e));
		GridBagConstraints gbc_btnNewButton_1 = new GridBagConstraints();
		gbc_btnNewButton_1.insets = new Insets(0, 0, 5, 5);
		gbc_btnNewButton_1.gridx = 0;
		gbc_btnNewButton_1.gridy = 3;
		frame.getContentPane().add(btnNewButton_1, gbc_btnNewButton_1);

		JTextPane txtpnOptionBDescription = new JTextPane();
		txtpnOptionBDescription.setBackground(textPaneBackgroundColor);
		txtpnOptionBDescription.setText("Option B description...");
		txtpnOptionBDescription.setEditable(false);
		GridBagConstraints gbc_txtpnOptionBDescription = new GridBagConstraints();
		gbc_txtpnOptionBDescription.insets = new Insets(0, 0, 5, 0);
		gbc_txtpnOptionBDescription.fill = GridBagConstraints.CENTER;
		gbc_txtpnOptionBDescription.gridx = 2;
		gbc_txtpnOptionBDescription.gridy = 3;
		frame.getContentPane().add(txtpnOptionBDescription, gbc_txtpnOptionBDescription);

		JButton btnNewButton_2 = new JButton("Option C");
		btnNewButton_2.addActionListener(e -> callOptioC(e));
		GridBagConstraints gbc_btnNewButton_2 = new GridBagConstraints();
		gbc_btnNewButton_2.insets = new Insets(0, 0, 0, 5);
		gbc_btnNewButton_2.gridx = 0;
		gbc_btnNewButton_2.gridy = 5;
		frame.getContentPane().add(btnNewButton_2, gbc_btnNewButton_2);

		JTextPane txtpnOptionCDescription = new JTextPane();
		txtpnOptionCDescription.setBackground(textPaneBackgroundColor);
		txtpnOptionCDescription.setText("Option C description...");
		txtpnOptionCDescription.setEditable(false);
		GridBagConstraints gbc_txtpnOptionCDescription = new GridBagConstraints();
		gbc_txtpnOptionCDescription.fill = GridBagConstraints.CENTER;
		gbc_txtpnOptionCDescription.gridx = 2;
		gbc_txtpnOptionCDescription.gridy = 5;
		frame.getContentPane().add(txtpnOptionCDescription, gbc_txtpnOptionCDescription);

	}

	public void setDBAppIcon() {
		try {
			frame.setIconImage(
					ImageIO.read(MainWindow.class.getClassLoader().getResourceAsStream("images/16px-DB.png")));
		} catch (IOException theCause) {
			logger.log(Level.SEVERE, "an exception was thrown", theCause);
		}
	}

	public void setCursorWaiting(boolean theIsWaitCursor) {
		if (theIsWaitCursor) {
			frame.setCursor(Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR));
		} else {
			frame.setCursor(Cursor.getPredefinedCursor(Cursor.DEFAULT_CURSOR));
		}
	}

	private void callListAllProducts(ActionEvent e) {
		logger.info("callListAllProducts");

		setCursorWaiting(true);

		new MySimpleWorker<List<Product>>(() -> ProductActions.callListAllProductsInJob(),
				() -> setCursorWaiting(false), // done -> disable waiting and
												// activate fields
				(theResult) -> ProductActions.callListAllProductsInJobDone(theResult, frame)).execute();
	}

	private void callOptionB(ActionEvent e) {
		JOptionPane.showMessageDialog(frame, "Option B Choosen...");
	}

	private void callOptioC(ActionEvent e) {
		JOptionPane.showMessageDialog(frame, "Option C Choosen...");
	}
}
