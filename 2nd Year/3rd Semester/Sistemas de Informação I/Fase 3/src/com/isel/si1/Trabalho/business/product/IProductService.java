package com.isel.si1.Trabalho.business.product;

import java.util.List;

import com.isel.si1.Trabalho.model.Product;

public interface IProductService {

	/**
	 * Get all products
	 * 
	 * @return List<Product> with all products 
	 */
	List<Product> getAllProducts();

}
