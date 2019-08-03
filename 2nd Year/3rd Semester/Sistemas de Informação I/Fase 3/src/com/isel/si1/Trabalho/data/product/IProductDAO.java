package com.isel.si1.Trabalho.data.product;

import java.util.List;

import com.isel.si1.Trabalho.model.Product;

public interface IProductDAO {

	/**
	 * Get all products
	 * 
	 * @return List<Product> with all products 
	 */
	List<Product> getAllProducts();
}
