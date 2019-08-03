package pt.isel.ls.DataAccessTest;
import pt.isel.ls.Model.Comment;
import pt.isel.ls.Model.Issue;
import pt.isel.ls.Model.Project;

import java.sql.*;
import java.text.SimpleDateFormat;

public class SampleData {

    // Insert dummy project for testing
    public static Project createProject(Connection conn) throws SQLException {
        final String TABLE_NAME = "Project";

        final String dName = "Mock Project";
        final String dDesc = "A projectName created for the purpose of testing";
        final java.sql.Date dDate = java.sql.Date.valueOf("2018-10-05");

        PreparedStatement ps = conn.prepareStatement("INSERT INTO " + TABLE_NAME + "(name,description,creationDate) VALUES( ?, ?, ?)", Statement.RETURN_GENERATED_KEYS);
        ps.setString(1, dName);
        ps.setString(2, dDesc);
        ps.setDate(3, dDate);
        ps.executeUpdate();

        ResultSet dResultSet = ps.getGeneratedKeys();
        dResultSet.next();
        int dID = dResultSet.getInt(4);

        return new Project.ProjectBuilder().withID(dID).withName(dName).withDescription(dDesc).withCreationDate(dDate).build();
    }

    // Insert dummy project and issue for testing
    public static Issue createIssue(Connection conn) throws SQLException {
        final String TABLE_NAME = "Issue";

        final String dName = "Mock Funcionality";
        final String dDesc = "a functionality to test data access";
        final java.sql.Timestamp dCreationDate = java.sql.Timestamp.valueOf("2018-10-04 01:02:03");
        final java.sql.Timestamp dClosedDate = java.sql.Timestamp.valueOf("2018-10-05 01:02:03");
        final String dStatus = "open";
        //final java.sql.Timestamp dUpdated = new Timestamp(System.currentTimeMillis());
        Project dProject = createProject(conn);

        PreparedStatement ps = conn.prepareStatement("INSERT INTO " + TABLE_NAME +
                " (name, description, created, dateofclosure, projectname, statusdesc) VALUES(?, ?, ?, ?, ?, ?)",
                Statement.RETURN_GENERATED_KEYS);
        ps.setString(1, dName);
        ps.setString(2, dDesc);
        ps.setTimestamp(3, dCreationDate);
        ps.setTimestamp(4, dClosedDate);
        ps.setString(5, dProject.name);
        ps.setString(6, dStatus);
        //ps.setTimestamp(7,dUpdated);
        ps.executeUpdate();

        ResultSet dResultSet = ps.getGeneratedKeys();
        dResultSet.next();
        int dID = dResultSet.getInt(1);

        return new Issue.
                IssueBuilder()
                .withID(dID)
                .withName(dName)
                .withDescription(dDesc)
                .withCreationDate(dCreationDate)
                .withClosedDate(dClosedDate)
                .withProjectName(dProject.name)
                .withStatusDesc(dStatus)
                //.withUpdated(dUpdated)
                .build();
    }

    // Insert dummy project, issue and comment for testing
    public static Comment createComment(Connection conn) throws SQLException {
        final String TABLE_NAME = "Comment";

        final String dText = "Mock comment";
        final java.sql.Timestamp dDate = java.sql.Timestamp.valueOf("2018-10-04 01:02:03");

        Issue dIssue = createIssue(conn);

        PreparedStatement ps = conn.prepareStatement("INSERT INTO " + TABLE_NAME + " (issueid, projectname, date, text) VALUES(?, ?, ?, ?)", Statement.RETURN_GENERATED_KEYS);
        ps.setInt(1, dIssue.ID);
        ps.setString(2, dIssue.projectName);
        ps.setTimestamp(3, dDate);
        ps.setString(4, dText);
        ps.executeUpdate();

        ResultSet dResultSet = ps.getGeneratedKeys();
        dResultSet.next();
        int dID = dResultSet.getInt(1);

        return new Comment.CommentBuilder().withId(dID).withIssueID(dIssue.ID).withProjectName(dIssue.projectName).withDate(dDate).withText(dText).build();
    }
}
