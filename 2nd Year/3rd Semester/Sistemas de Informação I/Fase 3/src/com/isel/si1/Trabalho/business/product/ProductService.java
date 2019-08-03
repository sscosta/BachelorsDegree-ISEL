package com.isel.si1.Trabalho.business.product;

import java.sql.Connection;
import java.sql.SQLException;
import java.util.Collections;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;

import com.isel.si1.Trabalho.data.SQLServerConnectionFactory;
import com.isel.si1.Trabalho.data.product.IProductDAO;
import com.isel.si1.Trabalho.data.product.ProductDAOFactory;
import com.isel.si1.Trabalho.model.Product;

public class ProductService implements IProductService{

	private static Logger logger = Logger.getLogger(ProductService.class.getName());

	@Override
	public List<Product> getAllProducts() {
		
		try {
			Connection aConnection = SQLServerConnectionFactory.getInstance().getConnection();
			IProductDAO aProductDAO = ProductDAOFactory.getNewInstance(aConnection);
			
			return aProductDAO.getAllProducts();
			
//		System.out.println("Sleeping...");
//		Thread.sleep(10000);
	} catch (ClassNotFoundException | SQLException theCause) {
		logger.log(Level.SEVERE, "an exception was thrown", theCause);
	}

		return Collections.emptyList();
	}
	
}
