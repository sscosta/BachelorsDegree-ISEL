package com.isel.si1.Trabalho.data.product;

import java.sql.Connection;

public class ProductDAOFactory {

	public static IProductDAO getNewInstance(Connection theConnection){
		return new ProductDAO(theConnection);
	}
}
