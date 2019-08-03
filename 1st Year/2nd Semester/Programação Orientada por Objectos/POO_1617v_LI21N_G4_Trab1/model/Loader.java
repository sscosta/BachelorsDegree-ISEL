package model;

import model.cell.Cell;
import java.util.NoSuchElementException;
import java.util.Scanner;

/**
 * Loads a game level from the file read with the scanner indicated in the constructor.<br/>
 * The file contains several levels.<br/>
 * Each level has a number from 1 to N.<br/><br/>
 * The first line of description for a level must conform to the format:<br/>
 * <code>#NNN LINES x COLUMNS</code><br/>
 * Where <code>NNN</code> is the level number, <code>LINES</code> is the number of rows of the row, and <code>COLUMNS</code> is the number of rows in each row.<br/>
 * The following <code>LINES</code> lines describe the cells of the level.<br/><br/>
 *
 */
public class Loader {
    private final Scanner in;   // Scanner used to read the file
    private int lineNumber;     // Current line number
    private String line;        // Text of current line

    private int height, width;  // Number of lines and columns of level
    private Circuit model;      // The loaded model

    /**
     * Build the loader to read it from the file through the scanner
     * @param in The scanner to use
     */
    public Loader(Scanner in) {
        this.in = in;
    }

    /**
     * Reads the level identified by the number.<br/>
     * This is the only public method of the class.<br/>
     * @param level The level number
     * @return The model for the loaded level
     * @throws LevelFormatException If an error is found in the file
     */
    public Circuit load(int level) throws LevelFormatException {
        findHeader(level);                  // Find the header line
        model = new Circuit(height,width);  // Build the model
        loadGrid();                         // Load cells information
        return model;
    }

    /**
     * Read the cell grid and instantiate each cell according to its description.<br/>
     * The cell descriptions of each row are separated by one or more separators.
     * @throws LevelFormatException If an error is found in cell descriptions
     */
    private void loadGrid() throws LevelFormatException {
        for(int l=0; l<height ; ++lineNumber,++l) {
            line = in.nextLine();                       // Read a line of cells
            String[] cells = line.split("\\s+");        // Split by separators
            if (cells.length!=width)                    // Verify number of cells in line
                error("Wrong number of cells in line");
            int c = 0;
            for(String word : cells) {                  // For each description
                char type = word.charAt(0);
                Cell cell = Cell.newInstance(type);     // Create a cell identified by first char
                if (cell==null)
                    error("Unknown cell type ("+type+")");
                if (!cell.fromString(word))             // Loads information specific to cell type
                    error("Invalid cell ("+word+")");
                model.putCell(l,c++,cell);              // Add cell to the model
            }
        }
    }

    /**
     * Find the header line for the level<br/>
     * Stores the dimensions of the level in <code>height</code> and <code>width</code> fields.
     * @param level The level number
     * @throws LevelFormatException If an errors is found in the file or level not found.
     */
    private void findHeader(int level) throws LevelFormatException {
        int idxSpace;
        try {
            for (lineNumber = 1; ; ++lineNumber) {
                line = in.nextLine();
                if (line.length() == 0 || line.charAt(0) != '#') continue;
                if ((idxSpace = line.indexOf(' ')) <= 1)
                    error("Invalid header line");
                if (Integer.parseInt(line.substring(1, idxSpace)) == level) break;
            }
            int idxX = line.indexOf('x', idxSpace);
            if (idxX <= 0)
                error("Missing dimensions in header line");
            height = Integer.parseInt(line.substring(idxSpace + 1, idxX).trim());
            width = Integer.parseInt(line.substring(idxX + 1).trim());
            if (height <= 0 || height > 99 || width <= 0 || width > 99)
                error("Invalid grid dimensions");
        } catch (NumberFormatException e) {
            error("Invalid number");
        } catch (NoSuchElementException e) {
            error("Level " + level + " not found");
        }
    }

    /**
     * Helper method to launch a LevelFormatException in internal methods.
     * @param msg The exception message
     * @throws LevelFormatException if LEVELS_FILE is not appropriately formatted.
     */
    private void error(String msg) throws LevelFormatException {
        throw new LevelFormatException(msg);
    }

    /**
     * Launched when a level loading error is detected.
     * The message describes the type of error.
     * Has the line number and the line where the error was detected.
     */
    public class LevelFormatException extends Exception {
        public LevelFormatException(String msg) {
            super(msg);
        }
        public int getLineNumber() { return lineNumber; }
        public String getLine() { return line; }
    }
}
