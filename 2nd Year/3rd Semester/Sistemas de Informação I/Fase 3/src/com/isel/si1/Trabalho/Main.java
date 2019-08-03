/**
 * 
 */
package com.isel.si1.Trabalho;

import java.io.IOException;
import java.util.logging.LogManager;

import com.isel.si1.Trabalho.presentation.mainwindow.MainWindow;
/**
 * @author sscosta adapted from Demo App by aremedios
 *
 * Application entry point.
 *
 */
public class Main {

	/**
	 * @param args
	 * @throws IOException 
	 * @throws SecurityException 
	 */
	public static void main(String[] args) throws SecurityException, IOException {
		LogManager.getLogManager().readConfiguration(Main.class.getClassLoader().getResourceAsStream("logging.properties"));
		MainWindow.show();
	}

}
