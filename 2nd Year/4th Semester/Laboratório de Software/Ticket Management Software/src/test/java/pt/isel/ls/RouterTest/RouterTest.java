package pt.isel.ls.RouterTest;

import org.junit.BeforeClass;
import org.junit.Test;
import pt.isel.ls.Commands.*;
import pt.isel.ls.Commands.GetCommands.*;
import pt.isel.ls.Commands.PostCommands.*;
import pt.isel.ls.Controller.Registrator;
import pt.isel.ls.Controller.Router;
import pt.isel.ls.Exceptions.CommandException;
import pt.isel.ls.Exceptions.CommandNotFoundException;

import java.util.HashMap;
import java.util.Map;

import static org.junit.Assert.assertTrue;

public class RouterTest {
    private static Router router = new Router();
    private static Registrator registrator = new Registrator();

    @BeforeClass
    public static void init_router_and_add_commands(){
        registrator.registerCommands(router = new Router());
        registrator.registerCriteria();
        registrator.registerCriteriaForIssues();
    }

    @Test
    public void get_command_test_valid_post_projects() throws CommandException {
        Command expected = new PostProjects();
        String method="POST";
        String path="/projects";
        Command actual = router.getCommand(method,path,new HashMap());
        assertTrue(expected.getClass() == actual.getClass());
    }

    @Test(expected = CommandNotFoundException.class)
    public void get_command_test_invalid_post_projects() throws CommandException {
        String method="POST";
        String path="/projects/{pid}";
        router.getCommand(method,path,new HashMap());
    }

    @Test
    public void get_command_test_valid_get_projects() throws CommandException {
        Command expected = new GetProjects();
        String method="GET";
        String path="/projects";
        Command actual = router.getCommand(method,path,new HashMap());
        assertTrue(expected.getClass() == actual.getClass());
    }

    @Test(expected = CommandNotFoundException.class)
    public void get_command_test_invalid_get_projects() throws CommandException {
        String method="GET";
        String path="/projects/";
        router.getCommand(method,path,new HashMap());
    }

    @Test
    public void get_command_test_valid_get_projects_with_pid() throws CommandException {
        Command expected = new GetProjectWithId();
        String method = "GET";
        String path = "/projects/{pid}";
        Command actual = router.getCommand(method,path, new HashMap());
        assertTrue(expected.getClass() == actual.getClass());
    }

    @Test(expected = CommandNotFoundException.class)
    public void get_command_test_invalid_get_projects_with_pid() throws CommandException {
        String method="GET";
        String path="/projects//";
        router.getCommand(method,path,new HashMap());
    }

    @Test
    public void get_command_test_valid_get_issues_from_project_with_pid() throws CommandException {
        Command expected = new GetIssuesFromProjectWithPid();
        String method="GET";
        String path="/projects/{pid}/issues";
        Command actual = router.getCommand(method,path,new HashMap());
        assertTrue(expected.getClass() == actual.getClass());
    }

    @Test
    public void get_command_test_valid_post_issue_in_project_with_pid() throws CommandException {
        Command expected = new PostIssue();
        String method="POST";
        String path="/projects/{pid}/issues";
        Command actual = router.getCommand(method,path,new HashMap());
        assertTrue(expected.getClass() == actual.getClass());
    }

    @Test
    public void get_command_test_valid_get_issue_with_id_from_project_with_pid() throws CommandException {
        Command expected = new GetIssuesById();
        String method="GET";
        String path="/projects/{pid}/issues/{iid}";
        Command actual = router.getCommand(method,path,new HashMap());
        assertTrue(expected.getClass() == actual.getClass());
    }

    @Test
    public void get_command_test_valid_post_comment_in_issue_with_issue_id_from_project_with_pid() throws CommandException {
        Command expected = new PostComment();
        String method="POST";
        String path="/projects/{pid}/issues/{iid}/comments";
        Command actual = router.getCommand(method,path,new HashMap());
        assertTrue(expected.getClass() == actual.getClass());
    }

    @Test
    public void get_command_test_valid_close_issue_with_i_id_from_project_with_pid() throws CommandException {
        Command expected = new CloseIssue();
        String method="POST";
        String path="/projects/{pid}/issues/{iid}/close";
        Command actual = router.getCommand(method,path,new HashMap());
        assertTrue(expected.getClass() == actual.getClass());
    }

    @Test
    public void get_command_test_valid_open_issue_with_i_id_from_project_with_pid() throws CommandException {
        Command expected = new OpenIssue();
        String method="POST";
        String path="/projects/{pid}/issues/{iid}/open";
        Command actual = router.getCommand(method,path,new HashMap());
        assertTrue(expected.getClass() == actual.getClass());
    }

    @Test
    public void has_sorting_criteria_get_projects_orderedby_closedissuecount() throws CommandException {
        Command expected = new GetProjectsOrderedByClosedIssueCount();
        Map<String, String> sortCriteria = new HashMap<>();
        String method="GET";
        String path="/projects";
        sortCriteria.put("sort", "closed-issue-count");
        Command actual = router.getCommand(method,path,sortCriteria);
        assertTrue(expected.getClass() == actual.getClass());
    }

    @Test
    public void has_sorting_criteria_get_projects_orderedby_creationdate() throws CommandException {
        Command expected = new GetProjectsOrderedByCreationDate();
        Map<String, String> sortCriteria = new HashMap<>();
        String method="GET";
        String path="/projects";
        sortCriteria.put("sort", "creation-date");
        Command actual = router.getCommand(method,path,sortCriteria);
        assertTrue(expected.getClass() == actual.getClass());
    }

    @Test
    public void has_sorting_criteria_get_projects_orderedby_issuecount() throws CommandException {
        Command expected = new GetProjectsOrderedByIssueCount();
        Map<String, String> sortCriteria = new HashMap<>();
        String method="GET";
        String path="/projects";
        sortCriteria.put("sort", "issue-count");
        Command actual = router.getCommand(method,path,sortCriteria);
        assertTrue(expected.getClass() == actual.getClass());
    }

    @Test
    public void has_sorting_criteria_get_projects_orderedby_openissuecount() throws CommandException {
        Command expected = new GetProjectsOrderedByOpenIssueCount();
        Map<String, String> sortCriteria = new HashMap<>();
        String method="GET";
        String path="/projects";
        sortCriteria.put("sort", "open-issue-count");
        Command actual = router.getCommand(method,path,sortCriteria);
        assertTrue(expected.getClass() == actual.getClass());
    }

    @Test
    public void has_sorting_criteria_get_issues_orderedby_created() throws CommandException {
        Command expected = new GetIssuesOrderedByDate();
        Map<String, String> sortCriteria = new HashMap<>();
        String method="GET";
        String path="/projects/{pid}/issues";

        //no state filter
        sortCriteria.put("sort", "created");
        Command actual = router.getCommand(method,path,sortCriteria);
        assertTrue(expected.getClass() == actual.getClass());

        //filtered by open issues
        sortCriteria.put("state", "open");
        actual = router.getCommand(method,path,sortCriteria);
        assertTrue(expected.getClass() == actual.getClass());

        //filtered by closed issues
        sortCriteria.put("state", "closed");
        actual = router.getCommand(method,path,sortCriteria);
        assertTrue(expected.getClass() == actual.getClass());

        //filtered by all issues
        sortCriteria.put("state", "all");
        actual = router.getCommand(method,path,sortCriteria);
        assertTrue(expected.getClass() == actual.getClass());
    }

    @Test
    public void has_sorting_criteria_get_issues_orderedby_updated() throws CommandException {
        Command expected = new GetIssuesOrderedByDate();
        Map<String, String> sortCriteria = new HashMap<>();
        String method="GET";
        String path="/projects/{pid}/issues";

        //no state filter
        sortCriteria.put("sort", "updated");
        Command actual = router.getCommand(method,path,sortCriteria);
        assertTrue(expected.getClass() == actual.getClass());

        //filtered by open issues
        sortCriteria.put("state", "open");
        actual = router.getCommand(method,path,sortCriteria);
        assertTrue(expected.getClass() == actual.getClass());

        //filtered by closed issues
        sortCriteria.put("state", "closed");
        actual = router.getCommand(method,path,sortCriteria);
        assertTrue(expected.getClass() == actual.getClass());

        //filtered by all issues
        sortCriteria.put("state", "all");
        actual = router.getCommand(method,path,sortCriteria);
        assertTrue(expected.getClass() == actual.getClass());
    }

    @Test
    public void has_sorting_criteria_get_issues_orderedby_comments() throws CommandException {
        Command expected = new GetIssuesOrderedByCommentCount();
        Map<String, String> sortCriteria = new HashMap<>();
        String method="GET";
        String path="/projects/{pid}/issues";

        //no state filter
        sortCriteria.put("sort", "comments");
        Command actual = router.getCommand(method,path,sortCriteria);
        assertTrue(expected.getClass() == actual.getClass());

        //filtered by open issues
        sortCriteria.put("state", "open");
        actual = router.getCommand(method,path,sortCriteria);
        assertTrue(expected.getClass() == actual.getClass());

        //filtered by closed issues
        sortCriteria.put("state", "closed");
        actual = router.getCommand(method,path,sortCriteria);
        assertTrue(expected.getClass() == actual.getClass());

        //filtered by all issues
        sortCriteria.put("state", "all");
        actual = router.getCommand(method,path,sortCriteria);
        assertTrue(expected.getClass() == actual.getClass());
    }
}