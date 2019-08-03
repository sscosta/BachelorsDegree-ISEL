package pt.isel.ls.Controller;

import org.postgresql.ds.PGSimpleDataSource;
import pt.isel.ls.Exceptions.SQLConnectionException;
import pt.isel.ls.PresentationLayer.Console.Print;

import java.sql.Connection;
import java.sql.SQLException;

public class ConnectionProvider {
    private static PGSimpleDataSource dataSource;
    private Connection connection;

    public ConnectionProvider() {
        dataSource = new PGSimpleDataSource();
        dataSource.setUrl(System.getenv("JDBC_DATABASE_URL"));
    }

    public void commit() throws SQLException {
        if(connection!=null && !connection.isClosed()) {
            connection.commit();
            Print.message("Command Done");
        }

    }

    public Connection getConnection() throws SQLConnectionException {
        try {
            connection = dataSource.getConnection();
            connection.setAutoCommit(false);
        } catch (SQLException e) {
            throw new SQLConnectionException(e.getMessage());
        }

        return connection;
    }

    public void closeConnection() throws SQLException {
        if(connection!=null && !connection.isClosed())
            connection.close();
    }

    public void rollback() throws SQLException {
        connection.rollback();
    }
}
