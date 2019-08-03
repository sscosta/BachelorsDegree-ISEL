package com.isel.si1.Trabalho.data;

import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.SQLException;

public class SQLServerConnectionFactory extends ConnectionFactory {

	private static SQLServerConnectionFactory connFactory=null;
	
	public static ConnectionFactory getInstance(){
		if( connFactory==null){
			connFactory = new SQLServerConnectionFactory();
		}
		return connFactory;
	}
	
	@Override
	public Connection getConnection() throws ClassNotFoundException, SQLException {
		/* Private methods */

		Connection conn = null;

		Class.forName("com.microsoft.sqlserver.jdbc.SQLServerDriver");

		// get a connection to DB
		conn = (Connection) DriverManager.getConnection(GetConnectionString());
		conn.setAutoCommit(false);
		
		return conn;
	}

}
