package com.isel.si1.Trabalho.data;

import java.sql.Connection;
import java.sql.SQLException;

import com.isel.si1.Trabalho.Configuration;
import com.isel.si1.Trabalho.Configuration.DataBaseConfiguration;

public abstract class ConnectionFactory {
	public abstract Connection getConnection() throws ClassNotFoundException,
			SQLException;

	public String GetConnectionString() {

		DataBaseConfiguration database = Configuration.getInstance().database;

		String url = "jdbc:sqlserver://" + database.Server;

		if (database.Port != null && database.Port.length() > 0) {
			url += ":" + database.Port + ";";
		} else {
			url += ";";
		}

		if (database.InstanceName != null && database.InstanceName.length() > 0) {
			url += "instanceName=" + database.InstanceName + ";";
		}

		if (database.Database != null && database.Database.length() > 0) {
			url += "databaseName=" + database.Database + ";";
		}
		if (database.IntegratedSecurity) {
			url += "integratedSecurity=true;";
		} else {
			url += "user=" + database.Username + ";password="
					+ database.Password + ";";
		}

		return url;
	}

}
