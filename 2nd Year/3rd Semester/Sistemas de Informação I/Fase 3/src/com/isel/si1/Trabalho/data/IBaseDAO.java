package com.isel.si1.Trabalho.data;

import java.sql.Connection;

public abstract class IBaseDAO {
	
	private Connection connection;
	
	public IBaseDAO(Connection theConnection){
		connection = theConnection;
	}
	 
	final public Connection getConnection(){
		return connection;
	}
}
