package com.isel.si1.Trabalho.business.product;

public class ProductFactory {

	public static ProductService getNewInstance(){
		return new ProductService();
	}
}
