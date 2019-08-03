package com.isel.si1.Trabalho.data.product;

import java.sql.Connection;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.ArrayList;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;

import com.isel.si1.Trabalho.data.IBaseDAO;
import com.isel.si1.Trabalho.model.Product;

public class ProductDAO extends IBaseDAO implements IProductDAO {

	private static Logger logger = Logger.getLogger(ProductDAO.class.getName());

	private final String GET_ALL_PRODUCTS = "select ref, name, description, price, volume from Product";


	ProductDAO(Connection theConnection) {
		super(theConnection);
	}

	@Override
	public List<Product> getAllProducts() {
		List<Product> allProducts = new ArrayList<>();
		Connection aCon = getConnection();

		// using try with resources, check this cool feature
		try (Statement aStatement = aCon.createStatement();
				ResultSet aResultSet = aStatement.executeQuery(GET_ALL_PRODUCTS);) {
			processResult(allProducts, aResultSet);

		} catch (SQLException theCause) {
			logger.log(Level.SEVERE, "an exception was thrown", theCause);
		}

		return allProducts;
	}

	public void processResult(List<Product> allProducts, ResultSet aResultSet) throws SQLException {
		while (aResultSet.next()) {
			Product aProduct = new Product();
			allProducts.add(aProduct);
			aProduct.setRef(aResultSet.getString("ref"));
			aProduct.setName(aResultSet.getString("name"));
			aProduct.setDescription(aResultSet.getString("description"));
			aProduct.setPrice(aResultSet.getBigDecimal("price"));
			aProduct.setVolume(aResultSet.getBigDecimal("volume"));
		}
	}

}
