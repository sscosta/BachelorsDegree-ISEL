package series.serie3.planningsubwaytrip.console.presentationlayer;

import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.Scanner;

public class Menu {
    public class MenuItem
    {
        public String description;
        public Method method;

        public MenuItem(String description, Method method)
        {
            this.description = description;
            this.method = method;
        }
    }

    public ArrayList<MenuItem> menuItems = null;

    @SuppressWarnings("rawtypes")
    public Menu() throws NoSuchMethodException
    {
        menuItems = new ArrayList<MenuItem>();
        Class params[] = {Scanner.class, Console.class};

        /*Items do menu*/
        menuItems.add(new MenuItem("List All Paths", Actions.class.getDeclaredMethod("allPaths", params)));
        menuItems.add(new MenuItem("Select Fastest Path", Actions.class.getDeclaredMethod("fastestPath", params)));
        menuItems.add(new MenuItem("Select Path with Less Changes",  Actions.class.getDeclaredMethod("pathWithLessChanges", params)));
        menuItems.add(new MenuItem("Quit", Actions.class.getDeclaredMethod("Quit", params)));
    }
}
