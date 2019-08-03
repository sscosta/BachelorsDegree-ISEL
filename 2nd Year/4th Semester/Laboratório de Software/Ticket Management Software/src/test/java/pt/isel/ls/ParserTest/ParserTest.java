package pt.isel.ls.ParserTest;

import org.junit.*;
import static org.junit.Assert.*;
import pt.isel.ls.Controller.Parser;
import pt.isel.ls.Controller.Registrator;
import pt.isel.ls.Exceptions.CommandException;
import pt.isel.ls.Exceptions.CommandNotFoundException;
import pt.isel.ls.Exceptions.InvalidParamException;

public class ParserTest {
    private static Registrator registrator;
    private static Parser parser;

    @BeforeClass
    public static void init_registrator_and_add_operations(){
        registrator = new Registrator();
        registrator.registerOperation();
    }

    @Test(expected = CommandException.class)
    public void parser_test_empty_args() throws CommandException {
        String [] args = {};
        parser = new Parser(args);
    }

    @Test(expected = CommandNotFoundException.class)
    public void parser_test_unknown_operation() throws CommandException {
        String [] args = {"OTHEROP"};
        parser = new Parser(args);
    }

    @Test(expected = CommandNotFoundException.class)
    public void parser_test_get() throws CommandException {
        //lowercase test
        String [] args = {"get"};
        parser = new Parser(args);

        //uppercase test
        String [] upperCaseArgs = {"GET"};
        parser = new Parser(upperCaseArgs);
    }

    @Test(expected = CommandNotFoundException.class)
    public void parser_test_post() throws CommandException {
        //lowercase test
        String [] args = {"post"};
        parser = new Parser(args);

        //uppercase test
        String [] upperCaseArgs = {"POST"};
        parser = new Parser(upperCaseArgs);

    }

    @Test
    public void parser_test_get_projects() throws CommandException {
        String [] args = {"GET", "/projects"};
        parser = new Parser(args);
        assertEquals("GET", parser.getMethod());
        assertEquals("/projects", parser.getPath());
    }


    @Test
    public void parser_test_get_projects_withid() throws CommandException {
        String [] args = {"GET", "/projects/123"};
        parser = new Parser(args);
        assertEquals("GET", parser.getMethod());
        assertEquals("/projects/{pid}", parser.getPath());
    }


    @Test
    public void parser_test_get_issues() throws CommandException {
        String [] args = {"GET", "/projects/123/issues"};
        parser = new Parser(args);
        assertEquals("GET", parser.getMethod());
        assertEquals("/projects/{pid}/issues", parser.getPath());
    }


    @Test
    public void parser_test_get_issues_withid() throws CommandException {
        String [] args = {"GET", "/projects/123/issues/123"};
        parser = new Parser(args);
        assertEquals("GET", parser.getMethod());
        assertEquals("/projects/{pid}/issues/{iid}", parser.getPath());
    }


    @Test
    public void parser_test_get_issues_filtered_open() throws CommandException {
        String [] args = {"GET", "/projects/123/issues", "state=open"};
        parser = new Parser(args);
        assertEquals("GET", parser.getMethod());
        assertEquals("/projects/{pid}/issues", parser.getPath());
        assertTrue(parser.getParamMap().get("state").equals("open"));
    }

    @Test
    public void parser_test_get_issues_filtered_closed() throws CommandException {
        String [] args = {"GET", "/projects/123/issues", "state=closed"};
        parser = new Parser(args);
        assertEquals("GET", parser.getMethod());
        assertEquals("/projects/{pid}/issues", parser.getPath());
        assertTrue(parser.getParamMap().get("state").equals("closed"));
    }

    @Test
    public void parser_test_get_issues_filtered_all() throws CommandException {
        String [] args = {"GET", "/projects/123/issues", "state=all"};
        parser = new Parser(args);
        assertEquals("GET", parser.getMethod());
        assertEquals("/projects/{pid}/issues", parser.getPath());
        assertTrue(parser.getParamMap().get("state").equals("all"));
    }

    @Test
    public void parser_test_get_issues_sorted_created() throws CommandException {
        String [] args = {"GET", "/projects/123/issues", "sort=created"};
        parser = new Parser(args);
        assertEquals("GET", parser.getMethod());
        assertEquals("/projects/{pid}/issues", parser.getPath());
        assertTrue(parser.getParamMap().get("sort").equals("created"));
    }

    @Test
    public void parser_test_get_issues_sorted_updated() throws CommandException {
        String [] args = {"GET", "/projects/123/issues", "sort=updated"};
        parser = new Parser(args);
        assertEquals("GET", parser.getMethod());
        assertEquals("/projects/{pid}/issues", parser.getPath());
        assertTrue(parser.getParamMap().get("sort").equals("updated"));
    }

    @Test
    public void parser_test_get_issues_sorted_comments() throws CommandException {
        String [] args = {"GET", "/projects/123/issues", "sort=comments"};
        parser = new Parser(args);
        assertEquals("GET", parser.getMethod());
        assertEquals("/projects/{pid}/issues", parser.getPath());
        assertTrue(parser.getParamMap().get("sort").equals("comments"));
    }

    @Test
    public void parser_test_get_issues_direction_asc() throws CommandException {
        String [] args = {"GET", "/projects/123/issues", "direction=asc"};
        parser = new Parser(args);
        assertEquals("GET", parser.getMethod());
        assertEquals("/projects/{pid}/issues", parser.getPath());
        assertTrue(parser.getParamMap().get("direction").equals("asc"));
    }

    @Test
    public void parser_test_get_issues_direction_desc() throws CommandException {
        String [] args = {"GET", "/projects/123/issues", "direction=desc"};
        parser = new Parser(args);
        assertEquals("GET", parser.getMethod());
        assertEquals("/projects/{pid}/issues", parser.getPath());
        assertTrue(parser.getParamMap().get("direction").equals("desc"));
    }

    @Test
    public void parser_test_post_projects() throws CommandException {
        String [] args = {"POST", "/projects"};
        parser = new Parser(args);
        assertEquals("POST", parser.getMethod());
        assertEquals("/projects", parser.getPath());
    }

    @Test
    public void parser_test_post_issues() throws CommandException {
        String [] args = {"POST", "/projects/123/issues", "name=issue+name&description=test+description"};
        parser = new Parser(args);
        assertEquals("POST", parser.getMethod());
        assertEquals("/projects/{pid}/issues", parser.getPath());
        assertTrue(parser.getParamMap().get("name").equals("issue name"));
        assertTrue(parser.getParamMap().get("description").equals("test description"));
    }

    @Test
    public void parser_test_post_issues_comments() throws CommandException {
        String [] args = {"POST", "/projects/123/issues/123/comments", "text=comment+on+issue"};
        parser = new Parser(args);
        assertEquals("POST", parser.getMethod());
        assertEquals("/projects/{pid}/issues/{iid}/comments", parser.getPath());
        assertTrue(parser.getParamMap().get("text").equals("comment on issue"));
    }

    @Test
    public void parser_test_post_issues_open() throws CommandException {
        String [] args = {"POST", "/projects/123/issues/123/open"};
        parser = new Parser(args);
        assertEquals("POST", parser.getMethod());
        assertEquals("/projects/{pid}/issues/{iid}/open", parser.getPath());
    }

    @Test
    public void parser_test_post_issues_close() throws CommandException {
        String [] args = {"POST", "/projects/123/issues/123/close"};
        parser = new Parser(args);
        assertEquals("POST", parser.getMethod());
        assertEquals("/projects/{pid}/issues/{iid}/close", parser.getPath());
    }


}
