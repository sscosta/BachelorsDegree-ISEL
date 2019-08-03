package app.Sketch.model;

import java.io.BufferedWriter;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.Scanner;

public class Sketch implements Iterable<Figure> {
    private LinkedList<Figure> figures ;

    public Sketch(){
       this.figures= new LinkedList<>();
    }

    public void add(Figure f){
        this.figures.add(f);
    }

    public Iterator<Figure> iterator() {
        return figures.iterator();
    }

    public void save(OutputStream out) throws IOException {
        BufferedWriter writer =
                new BufferedWriter(
                        new OutputStreamWriter(out)
                );
        writer.write(""+this.figures.size());
        writer.newLine();
        for (Figure f : this.figures) {
            writer.write(f.toString());
            writer.newLine();
        }
        writer.flush();
    }

    public void load(FileInputStream in) {
        Scanner scan = new Scanner(in);
        String line = scan.nextLine();
        int nPoints = Integer.parseInt(line);
        this.figures.clear();
        while (nPoints > 0) {
            line = scan.nextLine();
            Figure f =Figure.GetFigureFromString(line);
            this.figures.add(f);
            nPoints--;
        }
    }

    /*
    public int size(){
        return figures.size();
    }
    */

    public void removeAll() {
        figures.clear();
    }
    public Figure getLastCreated() {
        return figures.getLast();
    }

}
