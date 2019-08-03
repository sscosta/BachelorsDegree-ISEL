package pt.isel.ls.Controller;

import pt.isel.ls.Exceptions.CommandException;
import pt.isel.ls.Exceptions.CommandNotFoundException;
import pt.isel.ls.Commands.Command;

import java.util.*;

public class Router {
    private static ArrayList<Pair> cmdList = new ArrayList<>();
    private static Map registratedCriteria;

    public Router() {
    }

    public static List<MethodPath> getMethodPaths() {
        LinkedList<MethodPath> methodPaths = new LinkedList<>();
        for (Pair p :
                cmdList) {
            methodPaths.add((MethodPath)p.k);
        }
        return methodPaths;
    }


    public void addCommand(String method, String path, Command command){
        cmdList.add(new Pair<>(new MethodPath(method,path),command));
    }

    public Command getCommand(String method, String path,Map paramMap) throws CommandException {
        if(!hasSortingCriteria(paramMap)){
            return getCommandWithNoSortingCriteria(method,path);
        }

        return getCommandWithSortingCriteria(method,path, paramMap);
    }

    private boolean hasSortingCriteria(Map sortingCriteria) throws CommandException {
        if(!sortingCriteria.containsKey("sort") && sortingCriteria.containsKey("state") && sortingCriteria.containsKey("direction"))
            throw new CommandException("State is not a sorting criteria. Therefore the result cannot be ordered in a direction.");
        return sortingCriteria.containsKey("sort");
    }

    private Command getCommandWithNoSortingCriteria (String method, String path) throws CommandNotFoundException {
        for (Pair p : cmdList) {
            MethodPath mp = (MethodPath) p.k;
            if(method.equals(mp.method)&& path.equals(mp.path))
                return (Command) p.v;
        }
        throw new CommandNotFoundException("No command found in "+method+" "+path);
    }

    private Command getCommandWithSortingCriteria(String method, String path, Map paramMap) throws CommandNotFoundException{
        Map sortingCriteria = getSortingCriteriaMap(method,path);
        if(paramMap.containsKey("sort")) {
            String sortValue = String.valueOf(paramMap.get("sort"));
            if(sortingCriteria.containsKey(sortValue))
                return (Command) sortingCriteria.get(sortValue);
        }
        throw new CommandNotFoundException("Not found Command with such sorting criteria or filter");
    }

    private Map getSortingCriteriaMap(String method, String path) throws CommandNotFoundException {
        if(method.equals("GET") && (path.equals("/projects"))) {
            registratedCriteria = Registrator.getRegistratedSortingCriteria();
        }
        else if(path.equals("/projects/{pid}/issues"))
            registratedCriteria = Registrator.getRegistratedSortingCriteriaForIssue();
        else
            throw new CommandNotFoundException("No command found.");
        return registratedCriteria;
    }




}
