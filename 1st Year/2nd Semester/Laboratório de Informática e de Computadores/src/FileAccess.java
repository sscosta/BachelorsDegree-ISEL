import java.io.*;
import java.util.ArrayList;
import java.util.InputMismatchException;
import java.util.Scanner;

class FileAccess {

    public static ArrayList<String> load(String fileName,int initialCapacity){
        if (initialCapacity<=2) initialCapacity=2;
        ArrayList<String> SL=new ArrayList<>(initialCapacity);

        Scanner in = null;

        try {
            in = new Scanner(new FileInputStream(fileName));
            while (in.hasNextLine()) {
                SL.add(in.nextLine());
            }

        } catch (FileNotFoundException | InputMismatchException e) {
            System.out.println("Error loading file \""+fileName+"\":\n"+e.getMessage());
        } finally {
            if (in!=null) in.close();   // Close the file
        }
        return SL;
    }


    public static void save(String fileName,ArrayList<String> SL){
        BufferedWriter out =null;

        try {
            out = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(fileName)));
            if (SL !=null)
                for (String s: SL) {
                    out.write(s);
                    out.newLine();
                }
        } catch (IOException e) {
            System.out.println("Error saving file \""+fileName+"\":\n"+e.getMessage());
        }

        try {
            if (out!=null) {
                out.flush();
                out.close();  // Close the file
            }
        } catch ( IOException e) {
            System.out.println("Error saving file \""+fileName+"\":\n"+e.getMessage());
        }
    }
}
