package series.serie3.planningsubwaytrip.console.presentationlayer;

import javax.naming.ConfigurationException;
import java.io.IOException;
import java.lang.reflect.InvocationTargetException;
import java.util.InvalidPropertiesFormatException;
import java.util.Scanner;

public class Console {

    private Menu menu;
    private Actions actions;
    private Boolean running;

    public Console() throws NoSuchMethodException, ConfigurationException, InvalidPropertiesFormatException, IOException
    {
        menu = new Menu();
        actions = new Actions();
        running = true;
    }

    public void run() throws IOException, IllegalArgumentException, IllegalAccessException, InvocationTargetException
    {
        Scanner input = new Scanner(System.in);
        int     opcao = -1;

        do{
            ShowMenu();
            System.out.print("Please select an option:");

            if (input.hasNextInt())
            {
                opcao = input.nextInt();
                input.nextLine();

                if(opcao >= menu.menuItems.size())
                {
                    System.out.println("Unkown option - > " + opcao);
                    continue;
                }
            }
            else
            {
                String str = input.nextLine();
                System.out.println("Unkown option - > " + str);
                continue;
            }

            try
            {
                menu.menuItems.get(opcao).method.invoke(actions, new Object[]{input, this});
            }
            catch(InvocationTargetException exception)
            {
                System.out.println("Error. Unable to perform the option " + opcao);
                System.out.println("Reason: " + exception.getCause().getMessage());
                System.out.println();
            }

        }
        while(running);
    }

    public void Terminate()
    {
        running = false;
    }

    private void ShowMenu()
    {
        System.out.println();
        System.out.println("---------------------------------------------------------------");
        System.out.println("-------------------------Menu----------------------------------");

        int numberOfOptions = menu.menuItems.size();
        for(int option = 0; option < numberOfOptions; ++option)
        {
            Menu.MenuItem item = menu.menuItems.get(option);
            System.out.println(option + " - " + item.description);
        }
        System.out.println("---------------------------------------------------------------");
        System.out.println();
    }
}
