package pt.isel.ls.DataAccessTest;


import org.junit.After;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;
import org.postgresql.ds.PGSimpleDataSource;
import pt.isel.ls.Model.Project;

import java.sql.*;

import static org.junit.Assert.*;

public class ProjectDataAccessTest {
    private static PGSimpleDataSource theDataSource;
    private Connection theConnection;
    public static final String              TABLE_NAME = "Project";


    @BeforeClass
    public static void init_and_configure_data_source(){
        theDataSource = new PGSimpleDataSource();
        theDataSource.setUrl(System.getenv("JDBC_DATABASE_URL"));
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
    public void insert_project_test()throws SQLException{
        PreparedStatement ps = theConnection.prepareStatement("INSERT INTO " + TABLE_NAME + " VALUES( ?, ?, ?)", Statement.RETURN_GENERATED_KEYS);
        ps.setString(1, "Mock Project for unit tests");
        ps.setString(2, "A projectName created for the purpose of testing");
        ps.setDate(3, java.sql.Date.valueOf("2018-10-05"));
        int nRowsAffected = ps.executeUpdate();

        assertEquals(1, nRowsAffected);
    }

    @Test
    public void select_project_test() throws SQLException{
        //generate data for testing
        Project dProject = SampleData.createProject(theConnection);

        // Query the database
        PreparedStatement ps = theConnection.prepareStatement("select * from "+ TABLE_NAME + " where name = ?;");
        ps.setString(1, dProject.name);
        ResultSet rs = ps.executeQuery();

        // Obtain the result
        rs.next();
        String name = rs.getString(1);
        String description = rs.getString(2);
        java.sql.Date creationDate = rs.getDate(3);
        int pid = rs.getInt(4);

        // Create actual object
        Project actual = new Project.ProjectBuilder().withID(pid).withName(name).withDescription(description).withCreationDate(creationDate).build();

        assertEquals(dProject, actual);
    }

    @Test
    public void delete_project_test() throws SQLException{
        //generate data for testing
        Project dProject = SampleData.createProject(theConnection);

        // Execute delete row
        PreparedStatement ps = theConnection.prepareStatement(" delete from " + TABLE_NAME + " where name = ?; ");
        ps.setString(1, dProject.name);
        final int nRowsDeleted = ps.executeUpdate();

        // Verify one row deleted
        assertEquals(1, nRowsDeleted);

        // Try to obtain same project information
        ps = theConnection.prepareStatement("select * from "+ TABLE_NAME + " where name = ?;");
        ps.setString(1, dProject.name);
        ResultSet rs = ps.executeQuery();

        // Verify resultSet is empty
        assertFalse(rs.isBeforeFirst());
    }

    @Test
    public void update_project_test() throws SQLException{
        final String NEW_NAME = "A new dummy project name";
        final String NEW_DESCRIPTION = " A projectName created to test update table";
        final java.sql.Date NEW_DATE = java.sql.Date.valueOf("2020-01-01");

        //generate data for testing
        Project dProject = SampleData.createProject(theConnection);

        // Execute update
        PreparedStatement ps = theConnection.prepareStatement("update " + TABLE_NAME + " set name = ?, description = ?, creationDate = ? where name = ?");
        ps.setString(1, NEW_NAME);
        ps.setString(2, NEW_DESCRIPTION);
        ps.setDate(3, NEW_DATE);
        ps.setString(4, dProject.name);
        final int nRowsUpdated = ps.executeUpdate();

        // Verify one row updated
        assertEquals(1, nRowsUpdated);

        // Obtain project information again
        ps = theConnection.prepareStatement("select * from "+ TABLE_NAME + " where id = ?;");
        ps.setInt(1, dProject.ID);
        ResultSet rs = ps.executeQuery();

        // Obtain the result
        rs.next();
        String name = rs.getString(1);
        String description = rs.getString(2);
        java.sql.Date creationDate = rs.getDate(3);

        // Verify fields were updated
        assertEquals(name, NEW_NAME);
        assertEquals(description, NEW_DESCRIPTION);
        assertEquals(creationDate, NEW_DATE);
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
