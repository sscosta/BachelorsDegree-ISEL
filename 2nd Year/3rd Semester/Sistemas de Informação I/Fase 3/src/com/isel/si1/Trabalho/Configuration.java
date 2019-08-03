package com.isel.si1.Trabalho;

import java.util.Properties;

public class Configuration {

	public static final String dbConfigurationFileName = "database.config";
	public static final String appConfigurationFileName = "application.config";


	public boolean ConfigurationLoadSucess = true;
	public String ConfigurationLoadDescription = "OK";

	public DataBaseConfiguration database;
	public ApplicationConfiguration application;

	public class DataBaseConfiguration {
		public static final String DatabaseType_Key = "databaseType";
		public static final String DatabaseType_SQLSERVER = "sqlserver";
		public static final String DatabaseType_MYSQL = "mysql";
		public static final String DatabaseType_ORACLE = "oracle";

		public DataBaseConfiguration(Properties properties) {
			Server = properties.getProperty("server");
			Port = properties.getProperty("port");
			InstanceName = properties.getProperty("instanceName");
			Database = properties.getProperty("database");
			Username = properties.getProperty("username");
			Password = properties.getProperty("password");
			IntegratedSecurity = Boolean.parseBoolean(properties
					.getProperty("integratedSecurity"));
			databaseType = properties.getProperty(DatabaseType_Key);

		}

		public String Server;
		public String Port;
		public String InstanceName;
		public String Database;
		public String Username;
		public String Password;
		public boolean IntegratedSecurity;
		public String databaseType;

	}
	
	public class ApplicationConfiguration {
		public ApplicationConfiguration(Properties properties) {
			language = properties.getProperty("language");
			country = properties.getProperty("country");

		}

		public String language;
		public String country;

	}

	private static Configuration Instance = new Configuration();

	public static Configuration getInstance() {
		return Instance;
	}

	private Configuration() {
		try {
			Properties dbProperties = new Properties();
			dbProperties.loadFromXML(getClass().getClassLoader().getResourceAsStream(dbConfigurationFileName));
			database = new DataBaseConfiguration(dbProperties);

			Properties appProperties = new Properties();
			appProperties.loadFromXML(getClass().getClassLoader().getResourceAsStream(appConfigurationFileName));
			application = new ApplicationConfiguration(appProperties);
		} catch (Exception exception) {
			ConfigurationLoadDescription = exception.getMessage();
			ConfigurationLoadSucess = false;
		}
	}
}
