package pt.isel.ls.DataAccessTest;


import org.junit.After;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;
import org.postgresql.ds.PGSimpleDataSource;
import pt.isel.ls.Model.Issue;
import pt.isel.ls.Model.Project;

import java.sql.*;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;

public class IssueDataAccessTest {
    private static PGSimpleDataSource theDataSource;
    private Connection theConnection;
    public static final String              TABLE_NAME = "Issue";

    @BeforeClass
    public static void init_and_configure_data_source(){
        theDataSource = new PGSimpleDataSource();
        theDataSource.setUrl(System.getenv("JDBC_DATABASE_URL"));
//        theDataSource = new SQLServerDataSource();
//        theDataSource.setServerName("localhost");
//        theDataSource.setInstanceName(System.getenv("LS1819_DBinstance"));
//        theDataSource.setUser(System.getenv("LS1819_DBuser"));
//        theDataSource.setPassword(System.getenv("LS1819_DBpass"));
//        theDataSource.setDatabaseName("Issue_ManagementDB_tests");
    }

    @Before
    public void init() throws SQLException {
        theConnection = theDataSource.getConnection();
        theConnection.setAutoCommit(false);
    }

    @Test
    public void insert_issue_test() throws SQLException{
        //generate data for testing
        Project dProject = SampleData.createProject(theConnection);

        // Query the database
        PreparedStatement ps = theConnection.prepareStatement("INSERT INTO " + TABLE_NAME + " (name, description, created, dateOfClosure, projectName, statusDesc) VALUES(?, ?, ?, ?, ?, ?)", Statement.RETURN_GENERATED_KEYS);
        ps.setString(1, "Mock Funcionality");
        ps.setString(2,"a functionality to test data access");
        ps.setTimestamp(3, java.sql.Timestamp.valueOf("2018-10-04 12:01:01.0"));
        ps.setTimestamp(4, java.sql.Timestamp.valueOf("2018-10-05 12:01:01.0"));
        ps.setString(5, dProject.name);
        ps.setString(6, "closed");
        int nRowsAffected = ps.executeUpdate();

        assertEquals(1, nRowsAffected);
    }

    @Test
    public void select_issue_test() throws SQLException{
        //generate data for testing
        Issue dIssue = SampleData.createIssue(theConnection);

        // Query the database
        PreparedStatement ps = theConnection.prepareStatement("SELECT * FROM "+ TABLE_NAME + " WHERE id = ? AND projectName = ?;");
        ps.setInt(1, dIssue.ID);
        ps.setString(2, dIssue.projectName);
        ResultSet rs = ps.executeQuery();

        // Obtain the result
        rs.next();
        int iid = rs.getInt(1);
        String issueName = rs.getString(2);
        String issueDesc = rs.getString(3);
        java.sql.Timestamp created =  rs.getTimestamp(4);
        java.sql.Timestamp dateOfClosure =  rs.getTimestamp(5);
        String projectName = rs.getString(6);
        String statusDesc = rs.getString(7);
        //java.sql.Timestamp updated = rs.getTimestamp(8);


        // Create actual object
        Issue actual = new Issue.IssueBuilder()
                .withID(iid)
                .withName(issueName)
                .withDescription(issueDesc)
                .withCreationDate(created)
                .withClosedDate(dateOfClosure)
                .withProjectName(projectName)
                .withStatusDesc(statusDesc)
                //.withUpdated(updated)
                .build();
        assertEquals(dIssue, actual);
    }

    @Test
    public void delete_issue_test() throws SQLException{
        //generate data for testing
        Issue dIssue = SampleData.createIssue(theConnection);

        // Execute delete row
        PreparedStatement ps = theConnection.prepareStatement("DELETE FROM " + TABLE_NAME + " WHERE id = ? AND projectName = ?;");
        ps.setInt(1, dIssue.ID);
        ps.setString(2, dIssue.projectName);
        final int nRowsDeleted = ps.executeUpdate();

        // Verify one row deleted
        assertEquals(1, nRowsDeleted);

        // Try to obtain same issue information
        ps = theConnection.prepareStatement("SELECT * FROM "+ TABLE_NAME + " WHERE id = ? AND projectName = ?;");
        ps.setInt(1, dIssue.ID);
        ps.setString(2, dIssue.projectName);
        ResultSet rs = ps.executeQuery();

        // Verify resultSet is empty
        assertFalse(rs.isBeforeFirst());
    }

    @Test
    public void update_issue_test() throws SQLException{
        final String NEW_NAME = "A new dummy issue name";
        final String NEW_DESCRIPTION = "A issue created to test update table";
        final java.sql.Date NEW_DATECREATED = java.sql.Date.valueOf("2020-01-01");
        final java.sql.Date NEW_DATEOFCLOSURE = java.sql.Date.valueOf("2022-02-02");
        final String NEW_STATUS = "open";

        //generate data for testing
        Issue dIssue = SampleData.createIssue(theConnection);

        // Execute update
        PreparedStatement ps = theConnection.prepareStatement("UPDATE " + TABLE_NAME + " set name = ?, description = ?, created = ?, dateOfClosure = ?, statusDesc = ? WHERE id = ? AND projectName = ?;");
        ps.setString(1, NEW_NAME);
        ps.setString(2, NEW_DESCRIPTION);
        ps.setDate(3, NEW_DATECREATED);
        ps.setDate(4, NEW_DATEOFCLOSURE);
        ps.setString(5, NEW_STATUS);
        ps.setInt(6, dIssue.ID);
        ps.setString(7, dIssue.projectName);
        final int nRowsUpdated = ps.executeUpdate();

        // Verify one row updated
        assertEquals(1, nRowsUpdated);

        // Try to obtain same issue information
        ps = theConnection.prepareStatement("SELECT * FROM "+ TABLE_NAME + " WHERE id = ? AND projectName = ?;");
        ps.setInt(1, dIssue.ID);
        ps.setString(2, dIssue.projectName);
        ResultSet rs = ps.executeQuery();

        // Obtain the result
        rs.next();
        String issueName = rs.getString(2);
        String issueDesc = rs.getString(3);
        java.sql.Date created = rs.getDate(4);
        java.sql.Date dateOfClosure = rs.getDate(5);
        String statusDesc = rs.getString(7);

        // Verify fields were updated
        assertEquals(NEW_NAME, issueName);
        assertEquals(NEW_DESCRIPTION, issueDesc);
        assertEquals(NEW_DATECREATED, created);
        assertEquals(NEW_DATEOFCLOSURE, dateOfClosure);
        assertEquals(NEW_STATUS, statusDesc);
    }

    @After
    public void rollback_transaction() throws SQLException {
        try {
            theConnection.rollback();
        }finally {
            if(theConnection!=null)
                theConnection.close();
        }
    }
}
