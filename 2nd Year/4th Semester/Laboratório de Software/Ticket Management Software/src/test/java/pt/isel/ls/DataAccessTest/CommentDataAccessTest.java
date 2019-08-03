package pt.isel.ls.DataAccessTest;

import org.junit.After;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;
import org.postgresql.ds.PGSimpleDataSource;
import pt.isel.ls.Model.Comment;
import pt.isel.ls.Model.Issue;

import java.sql.*;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;

public class CommentDataAccessTest {
    private static PGSimpleDataSource dataSource;
    private Connection connection;
    public static final String              TABLE_NAME = "Comment";

    @BeforeClass
    public static void init_and_configure_data_source(){
        dataSource = new PGSimpleDataSource();
        dataSource.setUrl(System.getenv("JDBC_DATABASE_URL"));
    }

    @Before
    public void init() throws SQLException {
        connection = dataSource.getConnection();
        connection.setAutoCommit(false);
    }

    @Test
    public void insert_comment_test() throws SQLException{
        //generate data for testing
        Issue dIssue = SampleData.createIssue(connection);

        // Query the database
        PreparedStatement ps = connection.prepareStatement("INSERT INTO " + TABLE_NAME + " (issueID, projectName, date, text) VALUES(?, ?, ?, ?)", Statement.RETURN_GENERATED_KEYS);
        ps.setInt(1, dIssue.ID);
        ps.setString(2, dIssue.projectName);
        ps.setDate(3, java.sql.Date.valueOf("2018-10-04"));
        ps.setString(4, "Mock comment");
        int nRowsAffected = ps.executeUpdate();

        assertEquals(1, nRowsAffected);
    }

    @Test
    public void select_comment_test() throws SQLException{
        //generate data for testing
        Comment dComment = SampleData.createComment(connection);

        // Query the database
        PreparedStatement ps = connection.prepareStatement("SELECT * FROM "+ TABLE_NAME + " WHERE id = ? AND issueid = ? AND projectname = ?;");
        ps.setInt(1, dComment.id);
        ps.setInt(2, dComment.issueID);
        ps.setString(3, dComment.projectName);
        ResultSet rs = ps.executeQuery();

        // Obtain the result
        rs.next();
        int cid = rs.getInt(1);
        int iid = rs.getInt(2);
        String projectName = rs.getString(3);
        java.sql.Timestamp date = rs.getTimestamp(4);
        String text = rs.getString(5);

        // Create actual object
        Comment actual = new Comment.CommentBuilder().withId(cid).withIssueID(iid).withProjectName(projectName).withDate(date).withText(text).build();

        // Validate fields
        assertEquals(dComment.id, actual.id);
        assertEquals(dComment.issueID, actual.issueID);
        assertEquals(dComment.projectName, actual.projectName);
        assertEquals(dComment.date, actual.date);
        assertEquals(dComment.text, actual.text);
    }

    @Test
    public void delete_comment_test() throws SQLException{
        //generate data for testing
        Comment dComment = SampleData.createComment(connection);

        // Execute delete row
        PreparedStatement ps = connection.prepareStatement("DELETE FROM " + TABLE_NAME + " WHERE id = ? AND issueID = ? AND projectName = ?;");
        ps.setInt(1, dComment.id);
        ps.setInt(2, dComment.issueID);
        ps.setString(3, dComment.projectName);
        final int nRowsDeleted = ps.executeUpdate();

        // Verify one row deleted
        assertEquals(1, nRowsDeleted);

        // Try to obtain same comment information
        ps = connection.prepareStatement("SELECT * FROM "+ TABLE_NAME + " WHERE id = ? AND issueID = ? AND projectName = ?;");
        ps.setInt(1, dComment.id);
        ps.setInt(2, dComment.issueID);
        ps.setString(3, dComment.projectName);
        ResultSet rs = ps.executeQuery();

        // Verify resultSet is empty
        assertFalse(rs.isBeforeFirst());
    }

    @Test
    public void update_comment_test() throws SQLException{
        final java.sql.Timestamp NEW_DATE = java.sql.Timestamp.valueOf("2020-01-01 05:06:07");
        final String NEW_TEXT = "Mock comment updated for testing";

        //generate data for testing
        Comment dComment = SampleData.createComment(connection);

        // Execute update
        PreparedStatement ps = connection.prepareStatement("UPDATE " + TABLE_NAME + " set date = ?, text = ? WHERE id = ? AND issueID = ? AND projectName = ?;");
        ps.setTimestamp(1, NEW_DATE);
        ps.setString(2, NEW_TEXT);
        ps.setInt(3, dComment.id);
        ps.setInt(4, dComment.issueID);
        ps.setString(5, dComment.projectName);
        final int nRowsUpdated = ps.executeUpdate();

        // Verify one row updated
        assertEquals(1, nRowsUpdated);

        // Try to obtain same issue information
        ps = connection.prepareStatement("SELECT * FROM "+ TABLE_NAME + " WHERE id = ? AND issueID = ? AND projectName = ?;");
        ps.setInt(1, dComment.id);
        ps.setInt(2, dComment.issueID);
        ps.setString(3, dComment.projectName);
        ResultSet rs = ps.executeQuery();

        // Obtain the result
        rs.next();
        java.sql.Timestamp date = rs.getTimestamp(4);
        String text = rs.getString(5);

        // Verify fields were updated
        assertEquals(NEW_DATE, date);
        assertEquals(NEW_TEXT, text);
    }

    @After
    public void rollback_transaction() throws SQLException {
        try {
            connection.rollback();
        }finally {

            if(connection!=null)
                connection.close();
        }
    }
}
