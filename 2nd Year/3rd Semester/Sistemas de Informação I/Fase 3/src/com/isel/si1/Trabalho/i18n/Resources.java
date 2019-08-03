package com.isel.si1.Trabalho.i18n;

import java.util.Locale;
import java.util.ResourceBundle;

import com.isel.si1.Trabalho.Configuration;

public class Resources {

	private static ResourceBundle messages;
	
	static{
		String language = Configuration.getInstance().application.language;
		String country = Configuration.getInstance().application.country;
		Locale currentLocale = new Locale(language, country);
		messages = ResourceBundle.getBundle("MessagesBundle", currentLocale);
	}
	
	public static String getMessage(String theMsgKey){
		return messages.getString(theMsgKey);
	}
}
