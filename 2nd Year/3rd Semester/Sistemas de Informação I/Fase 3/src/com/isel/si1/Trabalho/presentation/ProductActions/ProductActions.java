package com.isel.si1.Trabalho.presentation.ProductActions;

import java.util.Collections;
import java.util.List;
import java.util.logging.Logger;

import javax.swing.JFrame;
import javax.swing.JOptionPane;

import com.isel.si1.Trabalho.business.product.IProductService;
import com.isel.si1.Trabalho.business.product.ProductFactory;
import com.isel.si1.Trabalho.model.Product;

public class ProductActions {

	private static Logger logger = Logger.getLogger(ProductActions.class.getName());

	/**
	 * Called by MySimpleWorker outside EventDispatchThread in a Worker Thread.
	 * 
	 * Do heavy duty work here.
	 */
	public static List<Product> callListAllProductsInJob() {
		List<Product> allProducts = null;
		logger.severe("callListAllProductsInJobDone");
		IProductService aProductService = ProductFactory.getNewInstance();
		allProducts = aProductService.getAllProducts();
		return allProducts != null ? allProducts : Collections.emptyList();
	}

	/**
	 * Called by MySimpleWorker in EventDispatchThread to update stuff or do
	 * follow up stuff
	 * 
	 * Update UI here (Do not do heavy duty work here. If necessary start
	 * another SwingWorker).
	 * 
	 * @param theResult
	 * @param theFrame
	 */
	public static void callListAllProductsInJobDone(List<Product> theResult, JFrame theFrame) {
		logger.info("callListAllProductsInJobDone");
		
		JOptionPane.showMessageDialog(theFrame, "Result:"+theResult.toString());
		logger.severe(theResult.toString());
	}

}
