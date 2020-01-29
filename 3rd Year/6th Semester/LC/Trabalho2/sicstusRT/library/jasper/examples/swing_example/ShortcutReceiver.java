/**
 * This interface should be implemented by any object that is using a
 * ShortcutMenu.
 * @see ShortcutMenu
 */
public interface ShortcutReceiver {
    /**
     * This method is called by a ShortcutMenu when the user selects a
     * shortcut item from the menu.
     * @param name The name of the shortcut menu item.
     * @param data The data string of the shortcut menu item.
     */
    void doShortcut(String name, String data);
    /**
     * This method is called to present default values when the user
     * adds a shortcut to the ShortcutMenu.
     * @return A String array with two elements. The first is the
     * default name for the new menu item. The second is the the
     * default data string for the new menu item.
     */
    String[] getDefaultShortcut();
}
