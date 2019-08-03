package pt.isel.leic.poo.app.model;

import pt.isel.leic.poo.app.model.cell.Cell;
import android.content.res.AssetManager;
import java.io.IOException;
import java.io.InputStream;
import java.util.InputMismatchException;
import java.util.Scanner;

public class Game {

    final private static String LEVELS_FILE = "LEVELS_FILE.txt";

    private Circuit modelCircuit;
    private int level;
    private int elapsedTime;
    private final AssetManager mng;

    public Game(AssetManager mng) {
        this.mng = mng;
    }

    /**
     * start a new game
     * @param level to start
     */
    public void startGame(int level) {
        this.loadLevel(level);
    }

    /**
     * start a game from data
     * @param level to start
     * @param elapsedTime
     * @param game data of filled tracks
     */
    public void startGame(int level,int elapsedTime,String game){
        this.loadLevel(level);
        this.elapsedTime=elapsedTime;
        if (game.length()>0) {
            String[] lines = game.split(";");        // Split by separators
            for (int l = 0; l < lines.length; l++) {
                if (lines[l].length() > 0) {
                    for (int c = 0; c < lines[l].length(); c++) {
                        this.modelCircuit.getCellFromPosition(l, c).fromSaved(lines[l].charAt(c));
                    }
                }
            }
        }
    }

    /**
     * save filled tracks to string
     * @return
     */
    public String saveToString() {
        StringBuilder b = new StringBuilder();
        for (int l = 0; l<this.modelCircuit.getHeight(); l++ ){
            for (int c = 0; c<this.modelCircuit.getWidth(); c++){
                b.append(this.modelCircuit.getCellFromPosition(l,c).toSave());
            }
            b.append(";");
        }
        return b.toString();
    }

    /**
     * load level
     * @param level
     */
    private void loadLevel(int level) {
        this.level = level;
        InputStream is;
        Scanner in = null;
        try {
            is =mng.open(LEVELS_FILE);
            in = new Scanner(is); // Scanner to read the file
            modelCircuit = new Loader(in).load(level);                     // Load level from scanner
            //return true;
        } catch (IOException | InputMismatchException e) {
            System.out.println("Error loading file \""+LEVELS_FILE+"\":\n"+e.getMessage());
            //return false;
        } catch (Loader.LevelFormatException e) {
            System.out.println(e.getMessage()+" in file \""+LEVELS_FILE+"\"");
            System.out.println(" "+e.getLineNumber()+": "+e.getLine());
            //return false;
        } finally {
            if (in!=null) in.close();   // Close the file
        }
    }

    public int getHeight() {
        return this.modelCircuit.getHeight();
    }

    public int getWidth() {
        return this.modelCircuit.getWidth();
    }

    public int getLevel(){
        return this.level;
    }

     public Cell getCellFromPosition(int line, int col) {
        return this.modelCircuit.getCellFromPosition(line,col);
    }

    /**
     * verify if game is over
     * @return game over
     */
    public boolean isOver(){
        return this.modelCircuit.isOver();
    }

    /**
     * try unlink tile
     * @param col of tile
     * @param line of tile
     * @return if unlink tile
     */
    public boolean unlink(int col,int line){
        return !this.modelCircuit.isOver() &&
                this.modelCircuit.unlink(new Position(col,line));
    }

    /**
     * try connect tiles
     * @param colFrom of origin tile
     * @param lineFrom of origin tile
     * @param colTo of destination tile
     * @param lineTo of destination tile
     * @return if connect tiles
     */
    public boolean drag(int colFrom, int lineFrom, int colTo, int lineTo) {
        return !this.modelCircuit.isOver() &&
                this.modelCircuit.drag(new Position (lineFrom,colFrom),new Position(lineTo,colTo));

    }

    public int getElapsedTime(){
        return this.elapsedTime;
    }

    public void incrementElapsedTime(){
        this.elapsedTime++;
    }

    /**
     * helper to format elapsedtime
     * @return format elapsedtime
     */
    public String elapsedTimeToString() {
        return String.format("%1$02d:%2$02d",
                this.elapsedTime/60,
                this.elapsedTime%60);
    }
}
