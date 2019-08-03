package pt.isel.ls.Controller;

import pt.isel.ls.Commands.*;
import pt.isel.ls.Commands.GetCommands.*;
import pt.isel.ls.Commands.PostCommands.*;
import pt.isel.ls.PresentationLayer.View.*;

import java.util.*;

public class Registrator {
    private static ArrayList<String> registratedOperations;
    private static Map<String, Command> registratedSortingCriteria;
    private static Map<String, Command> registratedSortingCriteriaForIssue;
    private static Map <String,Map<String, View>> registratedViewsForResult;

    public Registrator() {
        registratedOperations = new ArrayList<>();
        registratedSortingCriteria = new HashMap<>();
        registratedSortingCriteriaForIssue = new HashMap<>();
        registratedViewsForResult = new HashMap<>();
    }


    public void addOperation(String operation){
        registratedOperations.add(operation);
    }

    public void addCriteria(String criteria, Command cmd){
        registratedSortingCriteria.put(criteria,cmd);
    }
    public void addCriteriaForIssues(String criteria, Command cmd){
        registratedSortingCriteriaForIssue.put(criteria,cmd);
    }

    public static ArrayList<String> getRegistratedOperations() {
        return registratedOperations;
    }

    public static Map getRegistratedSortingCriteria() {
        return registratedSortingCriteria;
    }
    public static Map getRegistratedSortingCriteriaForIssue() {
        return registratedSortingCriteriaForIssue;
    }



    public void registerCommands(Router router){
        router.addCommand("GET","/",new GetRoot());
        router.addCommand("GET","/projects", new GetProjects());
        router.addCommand("POST", "/projects", new PostProjects());
        router.addCommand("GET","/projects/{pid}", new GetProjectWithId());
        router.addCommand("GET","/projects/{pid}/issues", new GetIssuesFromProjectWithPid());
        router.addCommand("POST","/projects/{pid}/issues", new PostIssue());
        router.addCommand("GET","/projects/{pid}/issues/{iid}", new GetIssuesById());
        router.addCommand("POST","/projects/{pid}/issues/{iid}/comments", new PostComment());
        router.addCommand("POST","/projects/{pid}/issues/{iid}/close", new CloseIssue());
        router.addCommand("POST","/projects/{pid}/issues/{iid}/open",new OpenIssue());
        router.addCommand("GET", "/projects/{pid}/issues/{iid}/comments/{cid}", new GetCommentsByCID());
        router.addCommand("POST","/projects/{pid}/labels",new PostLabelInProject());
        router.addCommand("GET", "/projects/{pid}/labels", new GetLabelsInProject());
        router.addCommand("POST", "/projects/{pid}/issues/{iid}/labels", new PostLabelInIssue());
        router.addCommand("DELETE", "/projects/{pid}/issues/{iid}/labels/{label-name}", new DeleteLabelFromIssue());
        router.addCommand("GET", "/projects/{pid}/labels/{label-name}",new GetLabelInProject());

        router.addCommand("OPTION", "/", new ListOptions());
        router.addCommand("GET", "/time", new GetTime());
        router.addCommand("EXIT","/", new Exit());

        router.addCommand("LISTEN","/", new StartWebserver());
        router.addCommand("GET","/projects/{pid}/search", new SearchProjects());
        router.addCommand("GET","/projects/{pid}/issues/{iid}/comments", new GetComments());
        router.addCommand("POST","/projects/{pid}/issues/{iid}/comments/{cid}",new PostComment());
        router.addCommand("GET" ,"/projects/{pid}/issues/{iid}/labels",new GetLabelsInIssue());
    }

    public void registerOperation() {
        this.addOperation("GET");
        this.addOperation("POST");
        this.addOperation("OPTION");
        this.addOperation("EXIT");
        this.addOperation("DELETE");
        this.addOperation("LISTEN");
    }

    public void registerCriteria() {
        this.addCriteria("creation-date",new GetProjectsOrderedByCreationDate());
        this.addCriteria("issue-count", new GetProjectsOrderedByIssueCount());
        this.addCriteria("open-issue-count",new GetProjectsOrderedByOpenIssueCount());
        this.addCriteria("closed-issue-count",new GetProjectsOrderedByClosedIssueCount());
    }
    public void registerCriteriaForIssues() {
        this.addCriteriaForIssues("created", new GetIssuesOrderedByDate());
        this.addCriteriaForIssues("updated", new GetIssuesOrderedByDate());
        this.addCriteriaForIssues("comments", new GetIssuesOrderedByCommentCount());
    }

    public Map getViewMap() {
        return registratedViewsForResult;
    }

    public static Set<String> getSearchKeyWords(){
        return registratedSortingCriteriaForIssue.keySet();
    }
}
